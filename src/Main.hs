{-# LANGUAGE FlexibleInstances #-}
module Main where

import Codec.Binary.UTF8.String (encodeString,decodeString)
import Control.Applicative
import Control.Arrow
import Control.Exception (evaluate)
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Error
import Data.Char
import qualified Data.ConfigFile as C
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Foreign.C.Types
import Language.Lojban.Jbovlaste
import Language.Lojban.Lujvo
import Language.Lojban.Util hiding (isValidLojban)
import Language.Lojban.CLL
import Language.Lojban.Jbobau
import Language.Lojban.Mlismu
import Network
import Network.IRC hiding (command)
import Prelude hiding (log)
import System
import System.Directory
import System.Console.Readline
import System.FilePath
import System.IO
import System.Posix
import System.Posix.Time
import System.Process
import Text.Regex
import WildCard
import Paths_lojbot

------------------------------------------------------------------------------
-- Bot state types

-- | Lojbot monad.
type Lojbot = StateT LojbotSt IO

-- | Lojbot's state.         
data LojbotSt = LojbotSt 
    { lojbotConfig :: Config       -- ^ The bot's configuration.
    , lojbotIRC    :: Handle       -- ^  IRC connection.
    , lojbotBuffer :: Chan Message -- ^  Buffer for sending messages.
    , lojbotJboDB  :: JboDB        -- ^  Jbovlaste database.
    , lojbotLog    :: Handle       -- ^  Logging handle.
    , lojbotMore   :: [Mores]      -- ^  More messages assoc list.
    , lojbotJbobau :: Jbobau       -- ^  Random lojbanic text generator.
    , lojbotMlismu :: Mlismu       -- ^  Mlismu handle.
    , lojbotCamxes :: Maybe Camxes -- ^  Camxes handle.
    }

-- | Camxes handles.
type Camxes = MVar (Handle,Handle)

-- | More messages.
type Mores = (String,[String])

-- | Default state.
defState :: LojbotSt
defState = LojbotSt
   { lojbotConfig = undefined
   , lojbotIRC    = undefined
   , lojbotBuffer = undefined
   , lojbotJboDB  = undefined 
   , lojbotLog    = stdout 
   , lojbotMore   = [] 
   , lojbotJbobau = undefined
   , lojbotMlismu = undefined
   , lojbotCamxes = undefined
   }

------------------------------------------------------------------------------
-- Configuration types

-- | Lojbot configuration.
data Config = Config
    { confNickName :: String        -- ^  Main nickname to use.
    , confNickPass :: String        -- ^  Nickserv password.
    , confServer   :: String        -- ^  Server host.
    , confPort     :: Integer       -- ^  Server port.
    , confChans    :: [ChanAssign]  -- ^  Channels to join, and options.
    , confLogFile  :: LogH          -- ^  How to log.
    , confJbov     :: FilePath      -- ^  Jbovlaste database path.
    , confMlismu   :: FilePath      -- ^  Mlismu fatci file.
    , confMode     :: Mode          -- ^  Running mode.
    } deriving (Read,Show)

data Mode = IRCBot | CmdLine deriving (Read,Show,Eq)

------------------------------------------------------------------------------
-- Main start-up actions

-- | Main for executable.
main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [conf] -> readConfig conf >>= either (error . show) start
    []     -> do dir <- getAppUserDataDirectory prog
                 let config = dir </> "config.ini"
                 exists <- doesFileExist config
                 if exists
                    then readConfig config >>= either (error . show) start
                    else getDataFileName "sample-config.ini" >>= readConfig >>= either (error . show) start
    _      -> error $ "expected: " ++ prog ++ "<config.ini>"

instance Monad m => Applicative (ErrorT C.CPError m) where
    pure = return; (<*>) = ap
instance Monad m => Alternative (ErrorT C.CPError m) where
    empty = mzero; (<|>) = mplus

readConfig :: String -> IO (Either (C.CPErrorData,String) Config)
readConfig filePath = runErrorT $ do
  config <- join $ liftIO $ C.readfile C.emptyCP filePath
  let irc = C.get config "IRCBOT"
      port = C.get config "IRCBOT" "port"
      misc = C.get config "MISC"
      res k f = misc k <|> liftIO (getDataFileName f)
  Config <$> irc "nick" <*>  irc "nickservpass" <*> irc "server" <*> port
         <*> (irc "chans" >>= tryGet "invalid channel list")
         <*> (irc "log" >>= tryGet "invalid log specification")
         <*> res "jbov" "jbovlaste.db"
         <*> res "mlismu" "fatci.txt"
         <*> (misc "mode" >>= tryGet "invalid mode")
      where tryGet msg = list (fail msg) (return . fst . head) . reads

-- | Start up for inside ghci.
start :: Config -> IO ()
start config = do
  installHandler sigPIPE Ignore Nothing
  evalStateT runBot (defState { lojbotConfig = config })

-- | Main Lojbot action.
runBot :: Lojbot ()
runBot = do
  openLog
  openJbovlaste
  openMlismu
  startCamxes
  mode <- config confMode
  case mode of
    IRCBot  -> do connectToIRC; startMsgBuffer; readIRCLines
    CmdLine -> readCmdLines

------------------------------------------------------------------------------
-- Initialisation

-- | Open the log file or use stdout.
openLog :: Lojbot ()
openLog = do
  logFile <- config confLogFile
  handle <- case logFile of
              LogStdout -> return stdout
              LogF file -> liftIO $ openFile file AppendMode
  liftIO $ hSetBuffering handle NoBuffering
  modify $ \state -> state { lojbotLog = handle }

-- | Open mlismu generator.
openMlismu :: Lojbot ()
openMlismu = do
  path <- config confMlismu
  doing $ "Opening mlismu fatci from " ++ path
  Right mli <- liftIO $ newMlismu path
  modify $ \state -> state { lojbotMlismu = mli }
  logLn "done."

-- | Open the jbovlaste database.
openJbovlaste :: Lojbot ()
openJbovlaste = do
  path <- config confJbov
  doing $ "Reading jbovlaste database from " ++ path
  db <- liftIO $ readDB path
  modify $ \state -> state { lojbotJboDB = db }
  logLn "done."

-- | Attempt to open a connection to the IRC server.
connectToIRC :: Lojbot ()
connectToIRC = do
  host <- config confServer
  (port,portNo) <- config ((id &&& PortNumber . fromInteger) . confPort)
  doing $ "Connecting to " ++ host ++ ":" ++ show port 
  handle <- liftIO $ connectTo host portNo
  liftIO $ hSetBuffering handle LineBuffering
  logLn "connected on socket."
  modify $ \state -> state { lojbotIRC = handle }

-- | Start the message buffer with the proper state.
startMsgBuffer :: Lojbot ()
startMsgBuffer = do
  chan <- liftIO newChan
  modify $ \state -> state { lojbotBuffer = chan }
  forkLojbot msgBuffer

forkLojbot :: Lojbot () -> Lojbot ()
forkLojbot m = do
  state <- get
  liftIO $ do forkIO $ evalStateT m state; return ()

forkLojbotAction :: LojbotAction () -> LojbotAction ()
forkLojbotAction m = do
  state <- get
  lift $ forkLojbot $ evalStateT m state; return ()

-- | The message buffer reads messages and delays 
--   every three messages to stop flooding.
{-

New method. Unstable/not yet stood test of time.
Only allow 512 bytes per five seconds. 
Sleep one second every four seconds.

-}
msgBuffer :: Lojbot ()
msgBuffer = do
  handle <- gets lojbotIRC
  buffer <- gets lojbotBuffer
  startTime <- liftIO epochTime
  let go lastTime charsSoFar (n:xs) = do
        msg <- liftIO $ readChan buffer
        let msg' = showMessage msg
            msgChars = length msg' + 1
            totalChars = charsSoFar + msgChars
            sendMsg t cs = do liftIO $ do threadDelay $ 1000 * 1000 * n
                                          hPutStrLn handle msg'
                              logLn $ "-> " ++ msg'
                              go t cs xs
        nowTime <- liftIO epochTime
        case nowTime - lastTime of
          d | d > 5             -> sendMsg nowTime msgChars
            | totalChars <= 512 -> sendMsg lastTime totalChars
            | otherwise         -> do liftIO $ do unGetChan buffer msg
                                                  threadDelay $ 1000 * 1000
                                      go lastTime charsSoFar xs
  go startTime 0 $ cycle [0,0,1]
  
{-

Old method. Stable/stood test of time.
It's simple and stupid, but works, but is also annoying.

msgBuffer :: Lojbot ()
msgBuffer = do
  handle <- gets lojbotIRC
  buffer <- gets lojbotBuffer
  lines <- liftIO $ getChanContents buffer
  let msgs = zip (cycle [1..3]) lines
  forM_ msgs $ \(n,msg) -> do
    let msg' = showMessage msg
        wait = if n == 3 then 5 else 1
    liftIO $ hPutStrLn handle msg'
    logLn $ "-> " ++ msg'
    liftIO $ threadDelay $ 1000 * 1000 * wait
-}

-- | Starts the camxes Java process.
startCamxes :: Lojbot ()
startCamxes = do
  doing "Starting camxes"
  camxes <- liftIO $ connectCamxes
  modify $ \state -> state { lojbotCamxes = camxes }
  logLn (maybe "failed." (const "done.") camxes)

-- | Opens the java process.
connectCamxes :: IO (Maybe Camxes)
connectCamxes = do
  pipe <- catch (Right <$> runInteractiveCommand "camxes -t")
                (const $ return $ Left "Broken pipe.")
  case pipe of
    Left e -> return Nothing
    Right (inp,out,err,_) -> do
      hSetBuffering inp LineBuffering
      hGetLine out
      var <- newMVar (inp,out)
      return $ Just var

------------------------------------------------------------------------------
-- Message handling

-- | Extract lines from command line and handle them as commands.
readCmdLines :: Lojbot ()
readCmdLines = do
  maybeLine <- liftIO $ readline "> "
  case maybeLine of 
    Nothing               -> return () 
    Just line | end       -> return ()
              | otherwise -> do liftIO $ addHistory line
                                handleCmdLine line
                                readCmdLines
              where end = any (==line) ["quit","exit"]

handleCmdLine :: String -> Lojbot ()
handleCmdLine line = do
  case words line of
    (cmd:args) -> case find (any (==cmd) . cmdName) commands of
                    Just command -> let arg = unwords args
                                    in evalStateT (cmdProc command arg) (Msg "PRIVMSG" "" ["",""])
                    Nothing      -> return ()
    _          -> return ()

-- | Extract lines and pass them to the handler.
readIRCLines :: Lojbot ()
readIRCLines = do
  handle <- gets lojbotIRC
  lines <- lines <$> liftIO (hGetContents handle)
  mapM_ lineHandler lines

-- | Attempt to decode a valid IRC message, logging unhandled ones.
lineHandler :: String -> Lojbot ()
lineHandler msg =
    case decode (msg++"\n") of
      Just msg -> do logLn $ "<- " ++ showMessage msg
                     handleMsg msg
      Nothing  -> logLn $ "unhandled message: <- " ++ msg

-- | Handle an IRC commands or nickserv messages, to be acted upon.
handleMsg :: Message -> Lojbot ()
handleMsg msg = do
  case msg of
    Message _ "PING" ps -> irc $ Message Nothing "PONG" ps
    Message (Just (NickName "lojbot" _ _)) "JOIN" ["#jbosnu"] -> irc $ privmsg "chanserv" "op #jbosnu lojbot"
    _ -> maybe (handlePMs msg) handleNickserv (nickservMsg msg)

-- | Nickserv actions:
--   1) Reply to nickserv with identify command.
--   2) Join channels when bot is identified.
handleNickserv :: String -> Lojbot ()
handleNickserv msg
    | "You are now identified" `isPrefixOf` msg = joinChans
    | "This nickname is registered" `isPrefixOf` msg =
        do password <- config confNickPass
           irc $ privmsg "nickserv" ("identify " ++ password)
    | otherwise = return ()

-- | Maybe a nickserv message.
nickservMsg :: Message -> Maybe String
nickservMsg msg =
    case msg of
      Message (Just (NickName "NickServ" _ _)) "NOTICE" [_,msg]
          -> Just msg
      _   -> Nothing

-- | Handle private messages according to channel rules or privmsg.
handlePMs :: Message -> Lojbot ()
handlePMs msg =
    case msg of
      Message (Just (NickName from _ _)) "PRIVMSG" (to:cmd)
              -> do setupAction msg
                    setupCmd from to (concat cmd)
      msg -> setupAction msg

-- | Handle a command.
setupCmd :: String -> String -> String -> Lojbot ()
setupCmd from to msg = do
  chans <- config confChans
  case filter ((==to) . chanName) chans of
    (chan:_) -> runCmd from to msg (chanPrefix chan)
    []       -> runCmd from to msg (join $ map chanPrefix chans)

-- | Run a command.
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
    run args cmd = evalStateT (cmdProc cmd (trim args)) (Msg "PRIVMSG" from [to,msg])

-- | Setup the state for an action.
setupAction :: Message -> Lojbot ()
setupAction msg = mapM_ (flip evalStateT (mkMsg msg)) actions

-- | An action's messge.
data Msg = Msg { msgCmd :: String, msgFrom :: String, msgPs :: [String] }
         deriving Show

-- | Make a Msg from a Message.
mkMsg :: Message -> Msg
mkMsg (Message (Just (NickName from _ _)) cmd ps) = Msg cmd from ps
mkMsg (Message (Just _) cmd ps) = Msg cmd "" ps
mkMsg (Message Nothing cmd ps) = Msg cmd "" ps

-- | Get the text from a message.
msgText :: Msg -> String
msgText = join . drop 1 . msgPs

-- | Get the person a message is addressed to.
msgTo :: Msg -> String
msgTo = list "" head . msgPs

------------------------------------------------------------------------------
-- Bot hooks

-- Actions to perform on private messages

-- | Actions to be peformed on private messages.
actions :: [LojbotAction ()]
actions = [actDoi,actRecord]

-- | Record valid mlismu lines into the mlismu fatci file. 
actRecord :: LojbotAction ()
actRecord = do
  fatci <- gets msgText
  (to:_) <- gets msgPs
  nick <- lift $ config confNickName
  if any (==to) ["#lojban",nick]
     then do mli <- lift $ gets lojbotMlismu
             valid <- lift $ isValidLojban fatci
             when valid $ liftIO $ readFatci mli fatci
     else return ()

-- | Respond to DOI/COI vocatives with mlismu utterances.
actDoi :: LojbotAction ()
actDoi = do
  text <- gets msgText
  nick <- lift $ config confNickName
  valid <- lift $ isValidLojban text
  let text' = text
      selbri = unwords $ filter ((>=3) . length) $ words $ text'
  if valid && isDoiCoi nick text' && (length (words text') > 2) 
      then mlismu selbri
      else return ()

-- Reply with mlismu output.
mlismu :: String -> LojbotAction ()
mlismu rel = do
  mli <- lift $ gets lojbotMlismu
  out <- liftIO $ randomBridiRel mli $ words rel
  case out of
    Just bridi -> reply bridi
    Nothing    -> reply "ii mi spofu i e'u ko tavla la kiris"

-- | Is a word COI or DOI?
isDoiCoi :: String -> String -> Bool
isDoiCoi prenu = isJust . find match . tails . words where
    match (coi:to:_) = any (==coi) coiDoi && lower (filter isLetter to) == prenu
    match _          = False

-- | COI and DOI cmavo.
coiDoi :: [String]
coiDoi = ["be'e","co'o","coi","fe'o","fi'i","je'e","ju'i","ke'o","ki'e"
         ,"mi'e","mu'o","nu'e","pe'u","re'i","ta'a","vi'o","doi"]

------------------------------------------------------------------------------
-- Bot commands

-- | Main command list.
commands :: [Cmd]
commands = [cmdSearch,cmdValsi,cmdRafsi,cmdGloss,cmdDef,cmdSelma'o
           ,cmdTrans,cmdSelrafsi,cmdCLL,cmdVlatai,cmdLujvo,cmdGrammar,cmdWords,cmdCamxes
           ,cmdMore,cmdHelp,cmdInfo]

-----------------------------------------
-- Lookup utilities

cmdLujvo :: Cmd
cmdLujvo = Cmd { cmdName = ["lujvo","l"]
               , cmdDesc = "deconstruct a lujvo"
               , cmdProc = proc } where
    proc string = do
      lines <- lujvoLookup string
      case lines of
        [] -> reply $ "invalid lujvo: " ++ string
        xs -> replies $ map showValsi xs

lujvoLookup :: String -> LojbotAction [JboValsi]
lujvoLookup string = do
  let word = list "" head $ words string
  case rafsis word of 
    [] -> return []
    rs -> do selrafs <- map head . filter (/=[]) <$> mapM rafsiLookup rs
             valsi <- makeLujvo' word rs selrafs
             return $ valsi : selrafs

makeLujvo' :: String -> [String] -> [JboValsi] -> LojbotAction JboValsi
makeLujvo' word rafsis selrafsis = do
  def <- clean . either (const "") snd <$> (liftIO $ translate word)
  return $ makeLujvo word rafsis selrafsis def
      where clean = flip (subRegex r) "\\1"
            r = mkRegex "^\\[<<[^ ]+ /(.*)/>>\\]$"

cmdInfo :: Cmd
cmdInfo = Cmd { cmdName = ["info","about"]
              , cmdDesc = "shows information about the bot, links, etc."
              , cmdProc = proc } where
    proc _ = do
      reply $ "Lojbot is written by Chris Done, in Haskell. \
              \Source code here: http://github.com/chrisdone/lojbot/tree/master and \
              \Support page here: http://chrisdone.lighthouseapp.com/projects/22016-lojbot/overview"

-- | Really generic search using the existing searches, prioritised.
cmdSearch :: Cmd
cmdSearch = Cmd { cmdName = ["search","query","q"]
                , cmdDesc = "really generic search using the others, prioritised: v -> r -> g -> d"
                , cmdProc = proc } where
    proc string = do
      valsi <- valsiLookup string
      rafsi <- rafsiLookup string
      gloss <- glossLookup string
      def   <- defLookup string
      lujvo <- lujvoLookup string
      selma'o <- if (any isUpper string) 
                 then selma'oLookup string 
                 else return []
      let results = nub $ valsi ++ rafsi ++ gloss ++ def ++ selma'o ++ lujvo
      case results of
        [] -> reply $ "no results for: " ++ string
        xs -> replies (map showValsi xs)

-- | Lookup a word from its gloss.
cmdGloss :: Cmd
cmdGloss = Cmd { cmdName = ["gloss","g"]
               , cmdDesc = "find valsi with the given gloss"
               , cmdProc = proc } where
    proc gloss = do 
      res <- glossLookup gloss
      case res of
        [] -> reply $ "no results for: " ++ gloss
        xs -> replies (map showValsi xs)

glossLookup :: String -> LojbotAction [JboValsi]
glossLookup gloss = do
      db <- lift $ gets lojbotJboDB
      let find f = filterValsi db $ any f . valsiGloss
          tries g = [wild g,wild g . lower             -- wildcard
                    ,(==g),(==g) . lower               -- full
                    ,isPrefixOf g,isPrefixOf g . lower -- prefix
                    ,isInfixOf g,isInfixOf g . lower]  -- infix
          search = nub . (find =<<) . (tries =<<) . argsInc 
      return $ search gloss

-- | Lookup a gismu/cmavo with the given rafsi.
cmdRafsi :: Cmd
cmdRafsi = Cmd { cmdName = ["rafsi","r"]
               , cmdDesc = "find gismu/cmavo with the given rafsi"
               , cmdProc = proc } where
    proc rafsi = do
      res <- rafsiLookup rafsi
      case res of
        [] -> reply $ "no entries found with the given rafsi: " ++ rafsi
        xs -> instReplies (length $ words rafsi) $ map showValsi xs

rafsiLookup :: String -> LojbotAction [JboValsi]
rafsiLookup rafsi = do
  db <- lift $ gets lojbotJboDB
  let lookup r = filterValsi db (liftM2 (&&) match filter) where
                    match = any (liftM2 (||) (==r) (wild r)) . valsiRafsis
                    filter = (/=LujvoType) . valsiType
  return $ nub $ join $ map lookup (words rafsi)

-- | Find all lujvo with a selrafsi.
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

-- | Create a lujvo with jvocuhadju.
cmdVlatai :: Cmd
cmdVlatai = Cmd { cmdName = ["jvocuhadju","jv"]
               , cmdDesc = "construct lujvos from selrafsis and rate them"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ lujvoAndRate (words text)
      case res of
        Left e -> reply (list "" head $ lines e)
        Right xs -> reply $ commas $ map showLujvo xs
      where showLujvo (r,w) = w ++ " (" ++ show r ++ ")"

-- | Search for valsi(s) by definition.
cmdDef :: Cmd
cmdDef = Cmd { cmdName = ["definition","d"]
             , cmdDesc = "search for valsi(s) by definition"
             , cmdProc = proc } where
    proc string = do 
      res <- defLookup string
      case res of
        []     -> reply $ "no results for: " ++ string
        valsis -> replies $ map showValsi $ sort valsis


defLookup :: String -> LojbotAction [JboValsi]
defLookup string = do
  db <- lift $ gets lojbotJboDB
  let terms = filter (not . badWild) $ string : words string
      basicWords = join $ map (defSub db) terms
      wildWords = join $ map (defWildCard db) terms
  return $ nub $ basicWords ++ wildWords

-- | valsi lookup.
cmdValsi :: Cmd
cmdValsi = Cmd
  { cmdName = ["valsi","v"]
  , cmdDesc = "lookup a gismu/cmavo/lujvo/fu'ivla"
  , cmdProc = proc } where
    proc string = do 
      res <- valsiLookup string
      case res of
        [] -> reply $ "no results for: " ++ string
        xs -> instReplies (length (words string)) $ map showValsi xs

valsiLookup :: String -> LojbotAction [JboValsi]
valsiLookup string = do
  db <- lift $ gets lojbotJboDB
  let terms = args string
      basicWords = join $ map (valsi db) terms
      wildWords = join $ map (valsiWildCard db) terms
  return $ nub $ basicWords ++ wildWords 

-- | gismu lookup via selma'o.
cmdSelma'o :: Cmd
cmdSelma'o = Cmd { cmdName = ["selma'o","s"]
                 , cmdDesc = "list cmavo of a selma'o"
                 , cmdProc = proc } where
    proc selma'o = do
      res <- selma'oLookup selma'o
      case res of
        [] -> reply "no such selma'o"
        xs -> replies $ split' (commas list) ++ (map showValsi xs)
            where list = map showCmavo xs
                  showCmavo w = "{" ++ valsiWord w ++ "}: " ++ (slashes $ valsiGloss w)

selma'oLookup :: String -> LojbotAction [JboValsi]        
selma'oLookup selma'o = do
  db <- lift $ gets lojbotJboDB
  return $ filterSelma'o db (lower selma'o) 

-- | Command to display help.
cmdHelp :: Cmd
cmdHelp = Cmd
  { cmdName = ["help","h","commands"]
  , cmdDesc = desc
  , cmdProc = proc } where
    proc cmd = do
      if isJust $ match "^[a-zA-Z'_]+$" cmd
         then reply $ maybe "" showCmd $ find (any (==cmd) . cmdName) commands
         else reply $ "commands: " ++ (commas $ map (alias . cmdName) commands)
    showCmd cmd = alias (cmdName cmd) ++ ": " ++ cmdDesc cmd
    desc = "show command list, or description of given command"

-- | Lookup from the CLL.
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
            text' = encodeString $ filter ok $ decodeString text
            ok c = isLetter c || isSpace c || c == '\'' || c == '"' || c == '-'

-----------------------------------------
-- Grammar

-- | camxes test for validity.
cmdCamxes :: Cmd
cmdCamxes = Cmd { cmdName = ["correct","c"]
                , cmdDesc = "camxes test for validity"
                , cmdProc = proc } where
    proc text = do
      valid <- lift $ isValidLojban text
      types <- liftIO $ valsiTypes text
      reply $ bool ("not grammatical: " ++ types) ("grammatical: " ++ text) valid
 
-- | Check some lojban grammar.
cmdGrammar :: Cmd
cmdGrammar = Cmd { cmdName = ["grammar","gr"]
                 , cmdDesc = "check/show grammar with jbofihe -ie"
                 , cmdProc = proc } where
    proc text = do
      res <- liftIO $ grammar $ newCmavo text
      case res of
        Right (err,out) | out /= "" -> reply out
                        | otherwise -> do
           types <- liftIO $ valsiTypes text
	   list (reply "parse error") perror types
        Left e -> reply $ list "" head $ lines e

-- | Check some lojban grammar.
cmdWords :: Cmd
cmdWords = Cmd { cmdName = ["selvla"]
               , cmdDesc = "show word types"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ valsiTypes $ newCmavo text
      mapM_ reply $ lines res

valsiTypes :: String -> IO String
valsiTypes str = do
  res <- run "cmafihe" str
  case res of
    Left error -> return error
    Right (err,out) -> return $ intercalate "\n" [err,out]

-----------------------------------------
-- Translation

-- | Translate some lojban.
cmdTrans :: Cmd
cmdTrans = Cmd { cmdName = ["translate","t"]
               , cmdDesc = "translate some lojban with jbofihe -x"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ translate (newCmavo text)
      case res of
        Right (err,out) | out /= "" -> reply out
                        | otherwise -> do
           types <- liftIO $ valsiTypes text
	   list (reply "parse error") perror types
        Left e -> reply (list "" head $ lines e)

perror s = reply $ "not grammatical: " ++ s

-- | Display more results.
cmdMore = Cmd { cmdName = ["more","m"]
              , cmdDesc = "show more results"
              , cmdProc = proc } where
    proc _ = do
      to <- gets replyTo
      replies <- lift $ gets lojbotMore
      case lookup to replies of
        Just msgs@(now:later) -> do 
               mode <- lift $ config confMode
               case mode of
                 IRCBot  -> let end = list "" more later
                            in do setMore later; doReplies [now++end]
                 CmdLine -> let later = drop 10 msgs
                                now = take 10 msgs
                                end = list "" more later
                            in do setMore later; doReplies $ now ++ [end]
        _ -> return ()

------------------------------------------------------------------------------ 
-- Lojbot action utilities

delayedReply l = do liftIO $ threadDelay $ 1000 * 200
                    reply l

-- | Append " .. x more results" if there are more results in a list.
more :: [String] -> String
more []    = ""
more later = " .. " ++ show (length later) 
             ++ " more result" ++ s where
    s = if length later > 1 then "s" else ""

-- | Reply immediately, and if over n stop there and add " .. x more results".
instReplies :: Int -> [String] -> LojbotAction ()
instReplies n xs = do
  let first = take n xs
      later = drop n xs
      now = init first
      end = last first ++ more later
  setMore (drop n xs)
  doReplies $ now ++ [end]

doReplies :: [String]-> LojbotAction ()
doReplies msgs = do
  mode <- lift $ config confMode
  case mode of
    IRCBot  -> forkLojbotAction $ mapM_ delayedReply msgs
    CmdLine -> mapM_ reply msgs

-- | Reply immediately or reply with " .. x more results".
replies :: [String] -> LojbotAction ()
replies msgs@(x:xs) = do
  mode <- lift $ config confMode
  case mode of
    IRCBot  -> do reply $ x ++ more xs; setMore xs
    CmdLine -> let now = take 10 msgs
                   later = drop 10 msgs
               in do mapM_ reply now; setMore later
replies []     = return ()

-- | Set the list of more results for that channel/person.
setMore :: [String] -> LojbotAction ()
setMore xs = do
  to <- gets replyTo
  mores <- lift $ gets lojbotMore
  lift $ modify $ \state -> state { lojbotMore = update (to,xs) mores }

-- | Split a message and send all the lines.
reply :: String -> LojbotAction ()
reply msg = do
  to <- gets replyTo
  mode <- lift $ config confMode
  case mode of
    IRCBot  -> lift $ mapM_ (irc . privmsg to) $ split' msg
    CmdLine -> liftIO $ do putStrLn (encodeString $ filter (/='\n') msg)
                           hFlush stdout

-- | Filter all commands matching a name.
cmds :: String -> [Cmd]
cmds name = filter (any (==name) . cmdName) commands

-- | Default splitter; 300 characters.
split' :: String -> [String]
split' = split 300

-- | Partition a string into separate ones that will fit into IRC messages
split :: Int -> String -> [String]
split n = join . longest . splits . normalise where
    join = uncurry (:) . (unwords *** list [] (split n . unwords))
    longest = last . takeWhile ((<=n) . length . unwords . fst)
    splits = liftM2 zip inits tails
    normalise = concatMap (\a -> filter (not . null) [take n a, drop n a]) . words

-- | Who do we send the message to?
replyTo :: Msg -> String
replyTo (Msg _ from (to:_)) | "#" `isPrefixOf` to = to
                            | otherwise           = from

-- | A command data type.
data Cmd = Cmd
    { cmdName :: [String]                  -- ^ Name and aliases
    , cmdDesc :: String                    -- ^ A description for the help file
    , cmdProc :: String -> LojbotAction () -- ^ The process to be run
    }

-- | The LojbotAction monad.
type LojbotAction = StateT Msg Lojbot

------------------------------------------------------------------------------
-- Generic IRC actions

-- | Join all channels in configuration.
joinChans :: Lojbot ()
joinChans = do
  chans <- config confChans
  mapM_ (irc . joinChan . chanName) chans

-- | Send an IRC message to the server.
irc :: Message -> Lojbot ()
irc msg = do
  buffer <- gets lojbotBuffer
  liftIO $ do forkIO $ writeChan buffer msg; return ()

------------------------------------------------------------------------------
-- Logging actions

-- | Log a line for doing something, e.g. "Doing this ... ".
doing :: String -> Lojbot ()
doing = log . (++ " ... ")

-- | Log a string.
log :: String -> Lojbot ()
log str = do
  handle <- gets lojbotLog
  liftIO $ hPutStr handle str

-- | Log a line.
logLn :: String -> Lojbot ()
logLn str = do 
  handle <- gets lojbotLog
  liftIO $ hPutStrLn handle str

-- | Simple utilities to access config entries.
config :: (Config -> a) -> Lojbot a
config = gets . (. lojbotConfig)

-- | A channel assignment.
data ChanAssign = ChanAssign
    { chanName     :: String     -- ^ Channel IRC name
    , chanCanReply :: Bool       -- ^ Can the bot reply in this channel?
    , chanPrefix   :: [String] } -- ^ What prefix should be looked for?
  deriving (Read,Show)

-- | Log to a file or stdout.
data LogH = LogStdout      -- ^ Just use stdout to log
          | LogF FilePath  -- ^ Use this file path to log
  deriving (Read,Show)

------------------------------------------------------------------------------
-- General utilties

-- | Is some text valid according to camxes?
isValidLojban :: String -> Lojbot Bool
isValidLojban line = do
  let text = newCmavo $ filter (/=')') line
  var <- gets lojbotCamxes
  case var of
    Nothing  -> return False
    Just var -> liftIO $ do c@(in',out) <- takeMVar var
                            hPutStrLn in' text
                            line <- hGetLine out
                            putMVar var c
                            return $ line == text 

-- | Replace new cmavo zo'oi, la'oi and me'oi with zo'e.
newCmavo :: String -> String
newCmavo = me'oi . la'oi . zo'oi . filter (/=')') where
    zo'oi = flip (subRegex (mkRegex zo'oiR)) " zo'e"
    zo'oiR = "(^zo'oi ([^ ]+)| zo'oi ([^ ]+))"
    la'oi = flip (subRegex (mkRegex la'oiR)) " zo'e"
    la'oiR = "(^la'oi ([^ ]+)| la'oi ([^ ]+))"
    me'oi = flip (subRegex (mkRegex me'oiR)) " zo'e"
    me'oiR = "(^me'oi ([^ ]+)| me'oi ([^ ]+))"

-- | Match a regular expression against a string.
match :: String -> String -> Maybe [String]
match = matchRegex . mkRegex

-- | braces \"foo\" = \"{foo}\"
braces :: String -> String
braces s = "{" ++ s ++ "}"

-- | parens \"foo\" = \"(foo)\"
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- | commas [\"foo\",\"bar\"] = "foo, bar"
commas :: [String] -> String
commas = intercalate ", " . filter (/="")

-- | slashes [\"foo\",\"bar\"] = \"foo/bar\".
slashes :: [String] -> String
slashes = intercalate "/" . filter (/="")

-- | A list equivalent for `maybe`.
list :: a -> ([b] -> a) -> [b] -> a
list nil _    [] = nil
list _   cons v  = cons v

-- | alias [\"Jonathan\",\"Jon\",\"J\"] = \"Jonathan (Jon, J)\".
alias :: [String] -> String
alias (x:xs) = x ++ list "" ((" "++) . parens . commas) xs
alias []     = ""

-- | Update an association list's value with a new value (or insert).
update :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
update a = unionBy ((==) `on` fst) [a]

-- | bool b a p =~ \"if p then a else b\".
bool :: t -> t -> Bool -> t
bool b a p = if p then a else b

-- | Lowercase a string.
lower :: String -> String
lower = map toLower

-- | Trim a string of whitespace on the edges or double spaces.
trim :: String -> String
trim = unwords . words

-- | Make a string into simple lojban [a-z']+.
lojban :: String -> String
lojban = filter (isJust . match "[a-z']" . return) . lower

-- | Wildcard match?
wild :: String -> String -> Bool
wild = (isNothing .) . wildcard

-- | Get arguments for a command.
args :: String -> [String]
args = filter (not . badWild) . words

-- | Get arguments for a command.
argsInc :: String -> [String]
argsInc x = filter (not . badWild) $ x : words x

badWild :: String -> Bool
badWild w = all (=='*') w || (any (=='*') w && length (filter (/='*') w) < 3)

-- | Proper process launching.
run :: String -> String -> IO (Either String (String,String))
run cmd input = do
  pipe <- catch (Right `fmap` runInteractiveCommand ("ulimit -t 1 && " ++ cmd))
                (const $ return $ Left "Broken pipe")
  case pipe of
    Right (inp,out,err,pid) -> do
                  catch (do hSetBuffering inp NoBuffering
                            hPutStr inp input 
                            hClose inp
                            errv <- newEmptyMVar
                            outv <- newEmptyMVar
                            output <- hGetContents out
                            errput <- hGetContents err
                            forkIO $ evaluate (length output) >> putMVar outv ()
                            forkIO $ evaluate (length errput) >> putMVar errv ()
                            takeMVar errv
                            takeMVar outv
                            e <- catch (waitForProcess pid)
                                       (const $ return ExitSuccess)
                            return $ Right (errput,output))
                        (const $ return $ Left "Broken pipe")
    _ -> return $ Left "Unable to launch process"
