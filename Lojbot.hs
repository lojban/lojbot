module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Arrow
import Control.Concurrent
import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Language.Lojban.Jbovlaste
import Language.Lojban.Lujvo
import Language.Lojban.Util
import Language.Lojban.CLL
import Language.Lojban.Jbobau
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
  openJbobau
  connectToIRC
  startMsgBuffer
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

-- Open jbobau generator
openJbobau :: Lojbot ()
openJbobau = do
  path <- config confTrainDat
  log $ "Reading jbobau training data from " ++ path ++ "... "
  Right jbo <- liftIO $ newJbobau path
  modify $ \state -> state { lojbotJbobau = jbo }
  logLn "done."

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
  logLn "connected on socket."
  modify $ \state -> state { lojbotIRC = handle }

startMsgBuffer :: Lojbot ()
startMsgBuffer = do
  chan <- liftIO newChan
  modify $ \state -> state { lojbotBuffer = chan }
  state <- get
  liftIO $ do forkIO $ evalStateT msgBuffer state; return ()

msgBuffer :: Lojbot ()
msgBuffer = do
  handle <- gets lojbotIRC
  buffer <- gets lojbotBuffer
  lines <- liftIO $ getChanContents buffer
  forM_ lines $ \msg -> do
    liftIO $ hPutStrLn handle (showMessage msg)
    logLn $ "-> " ++ (showMessage msg)
    liftIO $ threadDelay 500000

------------------------------------------------------------------------------
-- Message handling
 
-- Extract lines and pass them to the handler
readIRCLines :: Lojbot ()
readIRCLines = do
  handle <- gets lojbotIRC
  lines <- (lines) `fmap` liftIO (hGetContents handle)
  mapM_ (lineHandler) lines

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
    | pm = maybe (return ()) try (match 0)
    | otherwise = return ()
    where
    match n = fmap head . matchRegex cmdRegex $ drop n msg
    pm = head to /= '#'
    cmdRegex = mkRegex "^([a-zA-Z'_]+.*)"
    try cmd = let (name,args) = id *** drop 1 $ break (==' ') cmd
              in mapM_ (run args) $ cmds (lower name)
    run args cmd = evalStateT (cmdProc cmd (trim args)) (from,to)

------------------------------------------------------------------------------
-- Bot commands

-- Main command list
commands = [cmdValsi,cmdDef,cmdTrans,cmdGrammar
           ,cmdSelma'o,cmdRef,cmdCLL,cmdLujvo,cmdSelrafsi
           ,cmdJbobau,cmdJbobau',cmdCoi,cmdMore,cmdHelp]

-- Reply with some random grammatical lojbanic text and provide a translation
cmdJbobau' :: Cmd
cmdJbobau' = Cmd { cmdName = ["speak"]
                 , cmdDesc = "reply with some random grammatical lojbanic text (and translate it--never works)"
                 , cmdProc = proc } where
    proc rel = do
      jbo <- lift $ gets lojbotJbobau
      line <- liftIO $ jbobauLine jbo
      reply line
      (cmdProc cmdTrans) line

-- Reply with some random grammatical lojbanic text
cmdJbobau :: Cmd
cmdJbobau = Cmd { cmdName = ["jbobau","tavla"]
                , cmdDesc = "reply with some random grammatical lojbanic text \
                            \(uses #lojban IRC logs for training data)"
                , cmdProc = proc } where
    proc rel = do
      jbo <- lift $ gets lojbotJbobau
      line <- liftIO $ jbobauLine jbo
      reply line

-- Find all lujvo with a selrafsi
cmdSelrafsi :: Cmd
cmdSelrafsi = Cmd { cmdName = ["selrafsi","sr"]
                  , cmdDesc = "find all lujvo with given selrafsi"
                  , cmdProc = proc } where
    proc sr = do
      db <- lift $ gets lojbotJboDB
      case lujvosSelrafsis db (words sr) of
        [] -> reply "no lujvo found with the given selrafsi"
        xs -> replies $ commas (map showLujvo xs) : map showValsi xs
    showLujvo v = valsiWord v ++ list "" ((" "++) . parens . head) (valsiGloss v)

-- Create a lujvo with vlatai
cmdLujvo :: Cmd
cmdLujvo = Cmd { cmdName = ["lujvo"]
               , cmdDesc = "construct lujvos from selrafsis and rate them"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ lujvoAndRate (words text)
      case res of
        Left e -> reply e
        Right xs -> reply $ commas $ map showLujvo xs
      where showLujvo (r,w) = w ++ " (" ++ show r ++ ")"

-- Lookup from the CLL
cmdCLL :: Cmd
cmdCLL = Cmd { cmdName = ["cll"]
             , cmdDesc = "lookup something in the lojban reference grammar"
             , cmdProc = proc } where
    proc text = do
      res <- if null text' then return Nothing else liftIO $ cll text'
      case res of
        Just res | res /= [] -> replies $ map showRes res
        _ -> reply $ "no results for \"" ++ text ++ "\""
      where showRes (url,desc) = url ++ " : " ++ desc
            text' = UTF8.encodeString $ filter ok $ UTF8.decodeString text
            ok c = isLetter c || isSpace c || c == '\'' || c == '"'
 
-- Check some lojban grammar
cmdGrammar :: Cmd
cmdGrammar = Cmd { cmdName = ["grammar","jg","g"]
                 , cmdDesc = "check/show grammar with jbofihe -ie"
                 , cmdProc = proc } where
    proc text = do
      res <- liftIO $ grammar text
      case res of
        Right (err,out) | out /= "" -> reply out
                        | otherwise -> reply "parse error"
        Left e -> reply e

-- Translate some lojban
cmdTrans :: Cmd
cmdTrans = Cmd { cmdName = ["translate","jt","t"]
               , cmdDesc = "translate some lojban with jbofihe -x"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ translate text
      case res of
        Right (err,out) | out /= "" -> reply out
                        | otherwise -> reply "parse error"
        Left e -> reply e

-- Search for valsi(s) by definition
cmdDef :: Cmd
cmdDef = Cmd { cmdName = ["definition","d"]
             , cmdDesc = "search for valsi(s) by definition"
             , cmdProc = proc } where
    proc string = do
      db <- lift $ gets lojbotJboDB
      case defSub db string ++ defWildCard db string of
        []     -> reply $ show string ++ " not found in any definitions"
        valsis -> replies $ map showValsi valsis

-- simple bot ping
cmdCoi :: Cmd
cmdCoi = Cmd { cmdName = ["coi"]
             , cmdDesc = "list cmavo of a selma'o"
             , cmdProc = const $ reply "coi" }

-- valsi lookup
cmdValsi :: Cmd
cmdValsi = Cmd
  { cmdName = ["valsi","v","w"]
  , cmdDesc = "lookup a gismu/cmavo/lujvo/fu'ivla"
  , cmdProc = proc } where
    proc terms = do
      let valsi = map lower $ words terms
      valsi' <- join `fmap` mapM lookupValsi valsi
      let valsiUniq = valsi \\ (map fst valsi')
      lujvo <- catMaybes `fmap` mapM lookupLujvo valsiUniq
      let results  = lujvo ++ valsi'
          results' = map (\v -> filter ((==v) . fst) results) valsi
      replies $ map snd $ join $ results'
-- TODO handle no results

-- Lookup a lookup a gismu/cmavo/extant-lujvo/fu'ivla
lookupValsi :: String -> LojbotCmd [(String,String)]
lookupValsi w = do
  db <- lift $ gets lojbotJboDB
  return $ map (((,) w) . showValsi) $ valsi db w

-- Lookup the parts of a lujvo and display it.
lookupLujvo :: String -> LojbotCmd (Maybe (String,String))
lookupLujvo w =
    case rafsis (fixClusters w) of
      [] -> return Nothing
      rs -> do db <- lift $ gets lojbotJboDB
               Right (_,good) <- liftIO $ translate w
               let selrafsi = map (findSelrafsi db) rs
               return $ Just $ (w,showLujvo w rs selrafsi good)

-- Show a nonce lujvo.
showLujvo :: String -> [String] -> [Maybe JboValsi] -> String -> String
showLujvo w rs selrafsi good = 
    "lujvo {" ++ w ++ "}" ++ rafsis rs ++ selrafs selrafsi
    ++ selgloss selrafsi ++ ": " ++ trans good
  where rafsis = (", with rafsis "++) . braces . commas
        selrafs = (", selrafsi "++) . braces . commas . catMaybes . map (fmap valsiWord)
        selgloss = (' ':) . parens . commas . catMaybes . map (fmap (slashes . valsiGloss))
        trans = fromMaybe "" . fmap head . matchRegex (mkRegex "/([^/]+)/")

-- Describe a selma'o and link to the reference grammar
cmdRef :: Cmd
cmdRef = Cmd { cmdName = ["ref"]
             , cmdDesc = "describe a selma'o and link to the reference grammar"
             , cmdProc = proc } where
    proc selma'o = do
      info <- liftIO $ selma'oInfo selma'o
      db <- lift $ gets lojbotJboDB
      case info of
        Right inf -> reply (unwords $ lines inf)
        Left e    -> case valsi db selma'o of 
                       []    -> reply e
                       (x:_) -> proc (fromMaybe "" $ valsiSelma'o x)
 
-- gismu lookup via selma'o
cmdSelma'o :: Cmd
cmdSelma'o = Cmd { cmdName = ["selma'o","s"]
                 , cmdDesc = "list cmavo of a selma'o"
                 , cmdProc = proc } where
    proc selma'o = do
      db <- lift $ gets lojbotJboDB
      case filterSelma'o db (lower selma'o) of
        [] -> return ()
        xs -> replies $ split' (commas list) ++ (map showValsi xs)
            where list = map showCmavo xs
                  showCmavo w = "\2" ++ valsiWord w ++ "\2: " ++ (slashes $ valsiGloss w)

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
             ++ " more result" ++ s where
    s = if length later > 1 then "s" else ""

replies :: [String] -> LojbotCmd ()
replies (x:xs) | null xs   = reply x
               | otherwise = do reply $ x ++ more xs
                                setMore xs
replies [] = return ()

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

split' = split 300

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
  buffer <- gets lojbotBuffer
  liftIO $ do forkIO $ writeChan buffer msg; return ()

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
    { lojbotConfig :: Config       -- The bot's configuration
    , lojbotIRC    :: Handle       -- IRC connection
    , lojbotBuffer :: Chan Message -- Buffer for sending messages
    , lojbotJboDB  :: JboDB        -- Jbovlaste database
    , lojbotLog    :: Handle       -- Logging handle
    , lojbotMore   :: [Mores]      -- More messages assoc list
    , lojbotJbobau :: Jbobau       -- Random lojbanic text generator
    }

type Mores = (String    -- Nick/channel
             ,[String]) -- The reply

-- Default state
defState :: LojbotSt
defState = LojbotSt
   { lojbotConfig = undefined
   , lojbotIRC    = undefined
   , lojbotBuffer = undefined
   , lojbotJboDB  = undefined 
   , lojbotLog    = stdout 
   , lojbotMore   = [] 
   , lojbotJbobau = undefined
   }

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
    , confLogFile  :: LogH          -- How to log
    , confTrainDat :: FilePath      -- Training data path
    } deriving (Read,Show)

-- Default configuration
defConfig :: Config
defConfig = Config
    { confNickName = "lojbot"
    , confNickPass = "3281h9k"
    , confAltNicks = ["lojbot_","lojbot__"]
    , confServer   = "127.0.0.1"
    , confPort     = 6667
    , confChans    = [ChanAssign "#lojbot" True ["@","?"]]
    , confAdmins   = ["chrisdone"] 
    , confJbov     = "jbovlaste.db"
    , confLogFile  = LogStdout 
    , confTrainDat = "lojban.log"
    }

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
lower = map toLower
trim = unwords . words
