module Main where

import Control.Arrow
import Control.Monad.State
import Data.List
import Language.Lojban.Jbovlaste
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
    _ -> error "expected <config file>"

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
      Just msg -> do logLn $ "<- " ++ show msg
                     handleMsg msg
      Nothing  -> logLn $ "unhandled message: <- " ++ show msg

-- Handle an IRC commands or nickserv messages, to be acted upon.
handleMsg :: Message -> Lojbot ()
handleMsg msg =
    case nickservMsg msg of
      Just msg -> handleNickserv msg
      Nothing  -> handleCommands msg

-- Nickserv actions:
-- 1) Reply to nickserv with identify command
-- 2) Join channels when bot is identified
handleNickserv :: String -> Lojbot ()
handleNickserv msg 
    | "You are now identified" `isPrefixOf` msg = joinChans
    | "This nickname is registered" `isPrefixOf` msg =
        do password <- config confNickPass
           irc $ privmsg "nickserv" ("identify " ++ password)

-- Maybe a nickserv message.
nickservMsg :: Message -> Maybe String
nickservMsg msg =
    case msg of
      Message (Just (NickName "NickServ" _ _)) "NOTICE" [nick',msg]
          -> Just msg
      _   -> Nothing

-- Handle possible commands according to channel rules or privmsg.
handleCommands :: Message -> Lojbot ()
handleCommands msg =
    case msg of
      Message (Just (NickName from _ _)) "PRIVMSG" (to:cmd)
          | "#" `isPrefixOf` to -> channelCmd from to (concat cmd)
          | otherwise           -> pmCmd from to (concat cmd)
      _ -> return ()

-- Handle a channel command.
channelCmd :: String -> String -> String -> Lojbot ()
channelCmd from to cmd = do
  chans <- config confChans
  mapM_ (command from to cmd . chanPrefix) $ filter ((==to) . chanName) chans

-- Run a channel command.
command from to cmd p 
    | p `isPrefixOf` cmd = do
  case matchRegex cmdRegex cmd of
    Just [r] -> let (cmd',args) = break (==' ' ) r
                in mapM_ (run (tail args)) $ cmds cmd'
    _        -> return ()
    | otherwise = return ()
  where cmdRegex = mkRegex "^.([a-zA-Z'_]+ .*)"
        run args cmd = evalStateT (cmdProc cmd args) (from,to)

pmCmd :: String -> String -> String -> Lojbot ()
pmCmd from to cmd = do
  return ()

------------------------------------------------------------------------------
-- Bot commands

-- Main command list
commands = [echo]

-- Command to echo what a user says.
echo :: Cmd
echo = Cmd
  { cmdName = ["echo"]
  , cmdDesc = "echo: <text>\nechos what you say"
  , cmdProc = reply }

-- Filter all commands matching a name.
cmds :: String -> [Cmd]
cmds name = filter (any (==name) . cmdName) commands

-- Replies to a command according to whether it's a channel or user.
reply :: String -> LojbotCmd ()
reply msg = do
  (from,to) <- get
  case to of
    ('#':_) -> lift $ irc $ privmsg to msg
    _       -> lift $ irc $ privmsg from msg

-- A command data type.
data Cmd = Cmd
    { cmdName :: [String]               -- Name and aliases.
    , cmdDesc :: String                 -- A description for the help file.
    , cmdProc :: String -> LojbotCmd () -- The process to be run.
    }

type LojbotCmd = StateT CmdSt Lojbot
type CmdSt = (String  -- Who is using the command?
             ,String) -- Who was it sent to?

------------------------------------------------------------------------------
-- Generic IRC actions

-- Join all channels in configuration.
joinChans :: Lojbot ()
joinChans = do
  chans <- config confChans
  mapM_ (irc . joinChan . chanName) chans
-- Send an IRC message to the server.
irc :: Message -> Lojbot ()
irc msg = do
  handle <- gets lojbotIRC
  liftIO $ hPutStrLn handle (showMessage msg)

------------------------------------------------------------------------------
-- Logging actions

-- Log a string.
log :: String -> Lojbot ()
log str = do
  handle <- gets lojbotLog
  liftIO $ hPutStr handle str

-- Log a line.
logLn :: String -> Lojbot ()
logLn str = do 
  handle <- gets lojbotLog
  liftIO $ hPutStrLn handle str

------------------------------------------------------------------------------
-- Bot state types

type Lojbot = StateT LojbotSt IO
         
data LojbotSt = LojbotSt 
    { lojbotConfig :: Config   -- The bot's configuration.
    , lojbotIRC    :: Handle   -- IRC connection.
    , lojbotJboDB  :: JboDB    -- Jbovlaste database.
    , lojbotLog    :: Handle } -- Logging handle.

-- Default state
defState :: LojbotSt
defState = LojbotSt
   { lojbotConfig = undefined
   , lojbotIRC    = undefined
   , lojbotJboDB  = undefined 
   , lojbotLog    = stdout }

------------------------------------------------------------------------------
-- Configuration types

data Config = Config
    { confNickName :: String        -- Main nickname to use.
    , confAltNicks :: [String]      -- Alternate nicks to use.
    , confNickPass :: String        -- Nickserv password.
    , confServer   :: String        -- Server host.
    , confPort     :: Integer       -- Server port.
    , confChans    :: [ChanAssign]  -- Channels to join, and options.
    , confAdmins   :: [String]      -- Admin usernames.
    , confJbov     :: FilePath      -- Jbovlaste database path.
    , confLogFile  :: LogH }        -- How to log.
  deriving (Read,Show)

-- Default configuration
defConfig :: Config
defConfig = Config
    { confNickName = "lojbot"
    , confNickPass = "3281h9k"
    , confAltNicks = ["lojbot_","lojbot__"]
    , confServer   = "127.0.0.1"
    , confPort     = 6667
    , confChans    = [ChanAssign "#lojbot" False "@"]
    , confAdmins   = ["chrisdone"] 
    , confJbov     = "jbovlaste.db"
    , confLogFile  = LogStdout }

-- Simple utilities to access config entries.
config :: (Config -> a) -> Lojbot a
config = gets . (. lojbotConfig)

-- A channel assignment
data ChanAssign = ChanAssign
    { chanName     :: String   -- Channel IRC name
    , chanCanReply :: Bool     -- Can the bot reply in this channel?
    , chanPrefix   :: String } -- What prefix should be looked for?
  deriving (Read,Show)

data LogH = LogStdout      -- Just use stdout to log
          | LogF FilePath  -- Use this file path to log
  deriving (Read,Show)
