module Main where

import Control.Arrow
import Control.Concurrent
import Control.Monad.State
import Data.Function
import Data.List
import Data.Maybe
import Language.Lojban.Jbovlaste
import Language.Lojban.Lujvo
import Language.Lojban.Util
import Network
import Network.IRC hiding (command)
import Prelude hiding (log)
import Safe
import System
import System.IO
import System.Posix
import Text.Regex

------------------------------------------------------------------------------
-- Main start-up actions

-- Main for executable
main :: IO ()
main = do
  args <- getArgs
  case args of
    [conf] -> do conf <- readFile conf
                 case read conf of
                   Just conf -> start conf
                   _ -> error "unable to parse config file"
    _ -> start defConfig

-- Start up for inside ghci
start :: Config -> IO ()
start config = do
  installHandler sigPIPE Ignore Nothing
  evalStateT runBot (defState { lojbotConfig = config })

-- Main Lojbot action
runBot :: Lojbot ()
runBot = do
  openLog
  openJbovlaste
  connectToIRC
  readIRCLines

------------------------------------------------------------------------------
-- Initialisation

-- Open the log file or use stdout
openLog :: Lojbot ()
openLog = do
  logFile <- config confLogFile
  handle <- case logFile of
              LogStdout -> return stdout
              LogF file -> liftIO $ openFile file AppendMode
  liftIO $ hSetBuffering handle NoBuffering
  modify $ \state -> state { lojbotLog = handle }

-- Open the jbovlaste database
openJbovlaste :: Lojbot ()
openJbovlaste = do
  path <- config confJbov
  log $ "Reading jbovlaste database from " ++ path ++ "... "
  db <- liftIO $ readDB path
  modify $ \state -> state { lojbotJboDB = db }
  logLn "done."

-- Attempt to open a connection to the IRC server
connectToIRC :: Lojbot ()
connectToIRC = do
  host <- config confServer
  (port,portNo) <- config ((id &&& PortNumber . fromInteger) . confPort)
  log $ "Connecting to " ++ host ++ ":" ++ show port ++ "... "
  handle <- liftIO $ connectTo host portNo
  liftIO $ hSetBuffering handle LineBuffering
  modify $ \state -> state { lojbotIRC = handle }
  logLn "connected on socket."

------------------------------------------------------------------------------
-- Message handling
 
-- Extract lines and pass them to the handler
readIRCLines :: Lojbot ()
readIRCLines = do
  handle <- gets lojbotIRC
  lines <- lines `fmap` liftIO (hGetContents handle)
  mapM_ lineHandler lines

-- Attempt to decode a valid IRC message, logging unhandled ones
lineHandler :: String -> Lojbot ()
lineHandler msg =
    case decode (msg++"\n") of
      Just msg -> do logLn $ "<- " ++ showMessage msg
                     handleMsg msg
      Nothing  -> logLn $ "unhandled message: <- " ++ msg

-- Handle an IRC commands or nickserv messages, to be acted upon
handleMsg :: Message -> Lojbot ()
handleMsg msg =
    case msg of
      Message _ "PING" ps -> irc $ Message Nothing "PONG" ps
      _ -> maybe (handleCommands msg) handleNickserv (nickservMsg msg)

-- Nickserv actions:
-- 1) Reply to nickserv with identify command
-- 2) Join channels when bot is identified
handleNickserv :: String -> Lojbot ()
handleNickserv msg
    | "You are now identified" `isPrefixOf` msg = joinChans
    | "This nickname is registered" `isPrefixOf` msg =
        do password <- config confNickPass
           irc $ privmsg "nickserv" ("identify " ++ password)
    | otherwise = return ()

-- Maybe a nickserv message
nickservMsg :: Message -> Maybe String
nickservMsg msg =
    case msg of
      Message (Just (NickName "NickServ" _ _)) "NOTICE" [nick',msg]
          -> Just msg
      _   -> Nothing

-- Handle possible commands according to channel rules or privmsg
handleCommands :: Message -> Lojbot ()
handleCommands msg =
    case msg of
      Message (Just (NickName from _ _)) "PRIVMSG" (to:cmd)
              -> setupCmd from to (concat cmd)
      _ -> return ()

-- Handle a command
setupCmd :: String -> String -> String -> Lojbot ()
setupCmd from to msg = do
  chans <- config confChans
  mapM_ (runCmd from to msg . chanPrefix) $ check chans
      where check | head to /= '#' = id
                  | otherwise      = filter ((==to) . chanName) 

-- Run a command
runCmd :: String -> String -> String -> [String] -> Lojbot ()
runCmd from to msg p
    | any (flip isPrefixOf msg) p = maybe (return ()) try (match 1)
--    | pm = maybe (return ()) try (match 0)
    | otherwise = return ()
    where
    match n = fmap head . matchRegex cmdRegex $ drop n msg
    pm = head to /= '#'
    cmdRegex = mkRegex "^([a-zA-Z'_]+.*)"
    try cmd = let (name,args) = id *** drop 1 $ break (==' ') cmd
              in mapM_ (run args) $ cmds name
    run args cmd = evalStateT (cmdProc cmd args) (from,to)

------------------------------------------------------------------------------
-- Bot commands

-- Main command list
commands = [cmdValsi,cmdSelma'o,cmdCoi,cmdMore,cmdHelp]

cmdCoi = Cmd { cmdName = ["coi"]
             , cmdDesc = "list cmavo of a selma'o"
             , cmdProc = const $ reply "coi" }

-- valsi lookup
cmdValsi :: Cmd
cmdValsi = Cmd
  { cmdName = ["valsi","v","w"]
  , cmdDesc = "lookup a gismu/cmavo/lujvo/fu'ivla"
  , cmdProc = proc } where
    proc valsi' = do
      db <- lift $ gets lojbotJboDB
      case valsi db valsi' of
        [] -> case rafsis valsi' of
                [] -> reply $ "\"" ++ valsi' ++ "\" not found, or invalid"
                ws -> lookupLujvo valsi' ws
        ws -> replies $ map showValsi ws

-- Lookup the parts of a lujvo and display it.
lookupLujvo :: String -> [String] -> LojbotCmd ()
lookupLujvo w rs = do
  db <- lift $ gets lojbotJboDB
  Right (_,good) <- liftIO $ translate w
  let selrafsi = map (findSelrafsi db) rs
  reply $ "lujvo {" ++ w ++ "}" ++ rafsis rs ++ selrafs selrafsi
            ++ selgloss selrafsi ++ ": " ++ trans good
  where rafsis = (", with rafsis "++) . braces . commas
        selrafs = (", selrafsi "++) . braces . commas . catMaybes . map (fmap valsiWord)
        selgloss = (' ':) . parens . commas . catMaybes . map (fmap (slashes . valsiGloss))
        trans = fromMaybe "" . fmap head . matchRegex (mkRegex "/([^/]+)/")

-- gismu lookup via selma'o
cmdSelma'o = Cmd { cmdName = ["selma'o","s"]
                 , cmdDesc = "list cmavo of a selma'o"
                 , cmdProc = proc } where
    proc selma'o = do
      db <- lift $ gets lojbotJboDB
      case filterSelma'o db selma'o of
        [] -> return ()
        xs -> replies $ split' (commas list) ++ (map showValsi xs)
            where list = map showCmavo xs
                  showCmavo w = valsiWord w ++ " " ++ parens (slashes $ valsiGloss w)

-- Command to display help
cmdHelp :: Cmd
cmdHelp = Cmd
  { cmdName = ["help","h"]
  , cmdDesc = "help: <command>\nshows help"
  , cmdProc = proc } where
    proc cmd = do
      if (isJust $ matchRegex (mkRegex "^[a-zA-Z'_]+$") cmd)
         then reply $ maybe "" showCmd $ find (any (==cmd) . cmdName) commands
         else reply $ "commands: " ++ (commas $ map (alias . cmdName) commands)
    showCmd cmd = alias (cmdName cmd) ++ ": " ++ cmdDesc cmd

-- Display more results
cmdMore = Cmd { cmdName = ["more"]
              , cmdDesc = "list cmavo of a selma'o"
              , cmdProc = proc } where
    proc _ = do
      to <- gets replyTo
      replies <- lift $ gets lojbotMore
      case lookup to replies of
        Nothing -> return ()
        Just [] -> return ()
        Just xs -> let (now,later) = splitAt 3 xs
                       few = init now
                       end = last now ++ list "" more later
                   in do mapM_ reply (few++[end])
                         setMore later

------------------------------------------------------------------------------

more :: [String] -> String
more later = " .. " ++ show (length later) 
             ++ " more results: " ++ preview where
    preview = (take 7 $ head later) ++ " .."

replies :: [String] -> LojbotCmd ()
replies (x:xs) | null xs   = reply x
               | otherwise = do reply $ x ++ more xs
                                setMore xs

setMore :: [String] -> LojbotCmd ()
setMore xs = do
  to <- gets replyTo
  mores <- lift $ gets lojbotMore
  lift $ modify $ \state -> state { lojbotMore = update (to,xs) mores }

reply :: String -> LojbotCmd ()
reply msg = do
  to <- gets replyTo
  lift $ mapM_ (irc . privmsg to) $ split' msg

-- Filter all commands matching a name
cmds :: String -> [Cmd]
cmds name = filter (any (==name) . cmdName) commands

split' = split 390

-- Partition a string into separate ones that will fit into IRC messages
split :: Int -> String -> [String]
split n = join . longest . splits . normalise where
    join = uncurry (:) . (unwords *** list [] (split n . unwords))
    longest = last . takeWhile ((<=n) . length . unwords . fst)
    splits = liftM2 zip inits tails
    normalise = concatMap (\a -> filter (not . null) [take n a, drop n a]) . words

-- Who do we send the message to?
replyTo :: (String,String) -> String
replyTo (from,to) | "#" `isPrefixOf` to = to
                  | otherwise           = from

-- A command data type
data Cmd = Cmd
    { cmdName :: [String]               -- Name and aliases
    , cmdDesc :: String                 -- A description for the help file
    , cmdProc :: String -> LojbotCmd () -- The process to be run
    }

type LojbotCmd = StateT CmdSt Lojbot
type CmdSt = (String  -- Who is using the command?
             ,String) -- Who was it sent to?

------------------------------------------------------------------------------
-- Generic IRC actions

-- Join all channels in configuration
joinChans :: Lojbot ()
joinChans = do
  chans <- config confChans
  mapM_ (irc . joinChan . chanName) chans

-- Send an IRC message to the server
irc :: Message -> Lojbot ()
irc msg = do
  handle <- gets lojbotIRC
  liftIO $ hPutStrLn handle (showMessage msg)
  logLn $ "-> " ++ (showMessage msg)
  liftIO $ threadDelay 500000

------------------------------------------------------------------------------
-- Logging actions

-- Log a string
log :: String -> Lojbot ()
log str = do
  handle <- gets lojbotLog
  liftIO $ hPutStr handle str

-- Log a line
logLn :: String -> Lojbot ()
logLn str = do 
  handle <- gets lojbotLog
  liftIO $ hPutStrLn handle str

------------------------------------------------------------------------------
-- Bot state types

type Lojbot = StateT LojbotSt IO
         
data LojbotSt = LojbotSt 
    { lojbotConfig :: Config    -- The bot's configuration
    , lojbotIRC    :: Handle    -- IRC connection
    , lojbotJboDB  :: JboDB     -- Jbovlaste database
    , lojbotLog    :: Handle    -- Logging handle
    , lojbotMore   :: [Mores] } -- More messages assoc list

type Mores = (String    -- Nick/channel
             ,[String]) -- The reply

-- Default state
defState :: LojbotSt
defState = LojbotSt
   { lojbotConfig = undefined
   , lojbotIRC    = undefined
   , lojbotJboDB  = undefined 
   , lojbotLog    = stdout 
   , lojbotMore   = [] }

------------------------------------------------------------------------------
-- Configuration types

data Config = Config
    { confNickName :: String        -- Main nickname to use
    , confAltNicks :: [String]      -- Alternate nicks to use
    , confNickPass :: String        -- Nickserv password
    , confServer   :: String        -- Server host
    , confPort     :: Integer       -- Server port
    , confChans    :: [ChanAssign]  -- Channels to join, and options
    , confAdmins   :: [String]      -- Admin usernames
    , confJbov     :: FilePath      -- Jbovlaste database path
    , confLogFile  :: LogH }        -- How to log
  deriving (Read,Show)

-- Default configuration
defConfig :: Config
defConfig = Config
    { confNickName = "lojbot"
    , confNickPass = "3281h9k"
    , confAltNicks = ["lojbot_","lojbot__"]
    , confServer   = "127.0.0.1"
    , confPort     = 6667
    , confChans    = [ChanAssign "#lojbot" True ["@"]]
    , confAdmins   = ["chrisdone"] 
    , confJbov     = "jbovlaste.db"
    , confLogFile  = LogStdout }

-- Simple utilities to access config entries
config :: (Config -> a) -> Lojbot a
config = gets . (. lojbotConfig)

-- A channel assignment
data ChanAssign = ChanAssign
    { chanName     :: String     -- Channel IRC name
    , chanCanReply :: Bool       -- Can the bot reply in this channel?
    , chanPrefix   :: [String] } -- What prefix should be looked for?
  deriving (Read,Show)

data LogH = LogStdout      -- Just use stdout to log
          | LogF FilePath  -- Use this file path to log
  deriving (Read,Show)

-- General utilties
braces s = "{" ++ s ++ "}"
parens s = "(" ++ s ++ ")"
commas = intercalate ", "
slashes = intercalate "/"
list nil c [] = nil; list _ c v = c v
alias (x:xs) = x ++ list "" ((" "++) . parens . commas) xs
update a = unionBy ((==) `on` fst) [a]
bool a b p = if p then a else b