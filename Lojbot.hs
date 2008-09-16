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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [conf] -> do conf <- readFile conf
                 case read conf of
                   Just conf -> start conf
                   _ -> error "unable to parse config file"
    _ -> error "expected <config file>"

start :: Config -> IO ()
start config = do
  installHandler sigPIPE Ignore Nothing
  evalStateT runBot (defState { lojbotConfig = config })
  
runBot :: Lojbot ()
runBot = do
  openLog
  openJbovlaste
  connectToIRC
  readIRCLines

openLog :: Lojbot ()
openLog = do
  logFile <- config confLogFile
  handle <- case logFile of
              LogStdout -> return stdout
              LogF file -> liftIO $ openFile file AppendMode
  liftIO $ hSetBuffering handle NoBuffering
  modify $ \state -> state { lojbotLog = handle }

openJbovlaste :: Lojbot ()
openJbovlaste = do
  path <- config confJbov
  log $ "Reading jbovlaste database from " ++ path ++ "... "
  db <- liftIO $ readDB path
  modify $ \state -> state { lojbotJboDB = db }
  logLn "done."

connectToIRC :: Lojbot ()
connectToIRC = do
  host <- config confServer
  (port,portNo) <- config ((id &&& PortNumber . fromInteger) . confPort)
  log $ "Connecting to " ++ host ++ ":" ++ show port ++ "... "
  handle <- liftIO $ connectTo host portNo
  liftIO $ hSetBuffering handle LineBuffering
  modify $ \state -> state { lojbotIRC = handle }
  logLn "connected on socket."

readIRCLines :: Lojbot ()
readIRCLines = do
  handle <- gets lojbotIRC
  lines <- lines `fmap` liftIO (hGetContents handle)
  mapM_ lineHandler lines

lineHandler :: String -> Lojbot ()
lineHandler msg =
    case decode (msg++"\n") of
      Just msg -> do logLn $ "<- " ++ show msg
                     handleMsg msg
      Nothing  -> logLn $ "unhandled message: <- " ++ show msg

handleMsg :: Message -> Lojbot ()
handleMsg msg =
    case nickservMsg msg of
      Just msg -> handleNickserv msg
      Nothing  -> handleCommands msg

handleNickserv :: String -> Lojbot ()
handleNickserv msg 
    | "You are now identified" `isPrefixOf` msg = joinChans
    | "This nickname is registered" `isPrefixOf` msg =
        do password <- config confNickPass
           irc $ privmsg "nickserv" ("identify " ++ password)

handleCommands :: Message -> Lojbot ()
handleCommands msg =
    case msg of
      Message (Just (NickName from _ _)) "PRIVMSG" (to:cmd)
          | "#" `isPrefixOf` to -> channelCmd from to (concat cmd)
          | otherwise           -> pmCmd from to (concat cmd)
      _ -> return ()

channelCmd :: String -> String -> String -> Lojbot ()
channelCmd from to cmd = do
  chans <- config confChans
  mapM_ (command from to cmd . chanPrefix) $ filter ((==to) . chanName) chans

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

cmds :: String -> [Cmd]
cmds name = filter (any (==name) . cmdName) commands

commands = [echo]

echo :: Cmd
echo = Cmd
  { cmdName = ["echo"]
  , cmdDesc = "echo: <text>\nechos what you say"
  , cmdProc = reply }

joinChans :: Lojbot ()
joinChans = do
  chans <- config confChans
  mapM_ (irc . joinChan . chanName) chans

log :: String -> Lojbot ()
log str = do
  handle <- gets lojbotLog
  liftIO $ hPutStr handle str

logLn :: String -> Lojbot ()
logLn str = do
  handle <- gets lojbotLog
  liftIO $ hPutStrLn handle str

irc :: Message -> Lojbot ()
irc msg = do
  handle <- gets lojbotIRC
  liftIO $ hPutStrLn handle (showMessage msg)

nickservMsg :: Message -> Maybe String
nickservMsg msg =
    case msg of
      Message (Just (NickName "NickServ" _ _)) "NOTICE" [nick',msg]
          -> Just msg
      _   -> Nothing

reply :: String -> LojbotCmd ()
reply msg = do
  (from,to) <- get
  case to of
    ('#':_) -> lift $ irc $ privmsg to msg
    _       -> lift $ irc $ privmsg from msg

data Cmd = Cmd
    { cmdName :: [String]
    , cmdDesc :: String
    , cmdProc :: String -> LojbotCmd () }

type LojbotCmd = StateT CmdSt Lojbot

type CmdSt = (String,String)

type Lojbot = StateT LojbotSt IO
         
data LojbotSt = LojbotSt 
    { lojbotConfig :: Config
    , lojbotIRC    :: Handle
    , lojbotJboDB  :: JboDB 
    , lojbotLog    :: Handle }

defState :: LojbotSt
defState = LojbotSt
   { lojbotConfig = undefined
   , lojbotIRC    = undefined
   , lojbotJboDB  = undefined 
   , lojbotLog    = stdout }

data Config = Config
    { confNickName :: String
    , confAltNicks :: [String]
    , confNickPass :: String
    , confServer   :: String
    , confPort     :: Integer
    , confChans    :: [ChanAssign]
    , confAdmins   :: [String] 
    , confJbov     :: FilePath
    , confLogFile  :: LogH }
              deriving (Read,Show)

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

config :: (Config -> a) -> Lojbot a
config = gets . (. lojbotConfig)

data ChanAssign = ChanAssign
    { chanName     :: String
    , chanCanReply :: Bool
    , chanPrefix   :: String }
                  deriving (Read,Show)

data LogH = LogStdout | LogF FilePath
          deriving (Read,Show)
