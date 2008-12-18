module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Arrow
import Control.Concurrent
import Control.Monad.State
import Data.Char
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
import System.IO
import System.Posix
import System.Process
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
  openMlismu
  startCamxes
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

-- Open mlismu generator
openMlismu :: Lojbot ()
openMlismu = do
  path <- config confMlismu
  log $ "Opening mlismu fatci from " ++ path ++ " ... "
  Right mli <- liftIO $ newMlismu path
  modify $ \state -> state { lojbotMlismu = mli }
  logLn "done."

-- Open jbobau generator
openJbobau :: Lojbot ()
openJbobau = do
  path <- config confTrainDat
  log $ "Reading jbobau training data from " ++ path ++ " ... "
  Right jbo <- liftIO $ newJbobau path
  modify $ \state -> state { lojbotJbobau = jbo }
  logLn "done."

-- Open the jbovlaste database
openJbovlaste :: Lojbot ()
openJbovlaste = do
  path <- config confJbov
  log $ "Reading jbovlaste database from " ++ path ++ " ... "
  db <- liftIO $ readDB path
  modify $ \state -> state { lojbotJboDB = db }
  logLn "done."

-- Attempt to open a connection to the IRC server
connectToIRC :: Lojbot ()
connectToIRC = do
  host <- config confServer
  (port,portNo) <- config ((id &&& PortNumber . fromInteger) . confPort)
  log $ "Connecting to " ++ host ++ ":" ++ show port ++ " ... "
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
  let msgs = zip (cycle [1..3]) lines
  forM_ msgs $ \(n,msg) -> do
    let msg' = showMessage msg
        wait = if n == 3 then 5 else 1
    liftIO $ hPutStrLn handle msg'
    logLn $ "-> " ++ msg'
    liftIO $ threadDelay $ 1000 * 1000 * wait

startCamxes :: Lojbot ()
startCamxes = do
  log "Starting camxes ... "
  camxes <- liftIO $ connectCamxes
  modify $ \state -> state { lojbotCamxes = camxes }
  logLn (maybe "failed." (const "done.") camxes)

connectCamxes :: IO (Maybe Camxes)
connectCamxes = do
  pipe <- catch (Right `fmap` runInteractiveCommand "camxes -t")
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
handleMsg msg = do
  case msg of
    Message _ "PING" ps -> irc $ Message Nothing "PONG" ps
    Message (Just (NickName "lojbot" _ _)) "JOIN" ["#jbosnu"] -> irc $ privmsg "chanserv" "op #jbosnu lojbot"
    _ -> maybe (handlePMs msg) handleNickserv (nickservMsg msg)

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
      Message (Just (NickName "NickServ" _ _)) "NOTICE" [_,msg]
          -> Just msg
      _   -> Nothing

-- Handle private messages according to channel rules or privmsg
handlePMs :: Message -> Lojbot ()
handlePMs msg =
    case msg of
      Message (Just (NickName from _ _)) "PRIVMSG" (to:cmd)
              -> do setupAction msg
                    setupCmd from to (concat cmd)
      msg -> setupAction msg

-- Handle a command
setupCmd :: String -> String -> String -> Lojbot ()
setupCmd from to msg = do
  chans <- config confChans
  case filter ((==to) . chanName) chans of
    (chan:_) -> runCmd from to msg (chanPrefix chan)
    []       -> runCmd from to msg (join $ map chanPrefix chans)

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
    run args cmd = evalStateT (cmdProc cmd (trim args)) (Msg "PRIVMSG" from [to,msg])

setupAction :: Message -> Lojbot ()
setupAction msg = mapM_ (flip evalStateT (mkMsg msg)) actions

data Msg = Msg { msgCmd :: String, msgFrom :: String, msgPs :: [String] }
         deriving Show

mkMsg :: Message -> Msg
mkMsg (Message (Just (NickName from _ _)) cmd ps) = Msg cmd from ps
mkMsg (Message (Just _) cmd ps) = Msg cmd "" ps
mkMsg (Message Nothing cmd ps) = Msg cmd "" ps

msgText :: Msg -> String
msgText = join . drop 1 . msgPs

msgTo :: Msg -> String
msgTo = list "" head . msgPs

------------------------------------------------------------------------------
-- Bot hooks

-- Actions to perform on private messages

actions = [actDoi,actRecord]

actRecord :: LojbotAction ()
actRecord = do
  fatci <- gets msgText
  (to:_) <- gets msgPs
  nick <- lift $ config confNickName
  if any (==to) ["#lojban","#lojbot","#jbosnu",nick]
     then do mli <- lift $ gets lojbotMlismu
             valid <- lift $ isValidLojban fatci
             when valid $ liftIO $ readFatci mli fatci
     else return ()

actDoi :: LojbotAction ()
actDoi = do
  text <- gets msgText
  nick <- lift $ config confNickName
  valid <- lift $ isValidLojban text
  let text' = text
      selbri = unwords $ filter ((>=3) . length) $ words $ text'
  if valid && isCOI nick text' && (length (words text') > 2) 
      then mlismu selbri
      else return ()

-- Reply with mlismu output
mlismu rel = do
  mli <- lift $ gets lojbotMlismu
  out <- liftIO $ randomBridiRel mli $ words rel
  case out of
    Just bridi -> reply bridi
    Nothing    -> reply "ii mi spofu i e'u ko tavla la kiris"

isCOI :: String -> String -> Bool
isCOI prenu = isJust . find match . tails . words where
    match (coi:to:_) = any (==coi) coiDoi && lower (filter isLetter to) == prenu
    match _          = False

coiDoi = ["be'e","co'o","coi","fe'o","fi'i","je'e","ju'i","ke'o","ki'e"
         ,"mi'e","mu'o","nu'e","pe'u","re'i","ta'a","vi'o","doi"]

------------------------------------------------------------------------------
-- Bot commands

-- Main command list
commands = [cmdValsi,cmdDef,cmdTrans,cmdGrammar
           ,cmdSelma'o,cmdRef,cmdCLL,cmdLujvo,cmdSelrafsi
           ,cmdMore,cmdHelp,cmdCamxes]

cmdCamxes :: Cmd
cmdCamxes = Cmd { cmdName = ["valid"]
                , cmdDesc = "camxes test for validity"
                , cmdProc = proc } where
    proc text = do
      valid <- lift $ isValidLojban text
      reply $ bool ("not valid: " ++ text) ("valid: " ++ text) valid
      nick <- gets msgFrom
      reply $ "valid fatci (if any) were added, " ++ nick

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
        Left e -> reply (list "" head $ lines e)
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
            ok c = isLetter c || isSpace c || c == '\'' || c == '"' || c == '-'
 
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
        Left e -> reply (list "" head $ lines e)

-- Translate some lojban
cmdTrans :: Cmd
cmdTrans = Cmd { cmdName = ["translate","jt","t"]
               , cmdDesc = "translate some lojban with jbofihe -x"
               , cmdProc = proc } where
    proc text = do
      res <- liftIO $ translate (newCmavo text)
      case res of
        Right (err,out) | out /= "" -> reply out
                        | otherwise -> reply "parse error"
        Left e -> reply (list "" head $ lines e)

-- Search for valsi(s) by definition
cmdDef :: Cmd
cmdDef = Cmd { cmdName = ["definition","d"]
             , cmdDesc = "search for valsi(s) by definition"
             , cmdProc = proc } where
    proc string = do
      db <- lift $ gets lojbotJboDB
      case defSub db string ++ defWildCard db string of
        []     -> reply $ show string ++ " not found"
        valsis -> replies $ map showValsi $ sort valsis

-- valsi lookup
cmdValsi :: Cmd
cmdValsi = Cmd
  { cmdName = ["valsi","v","w"]
  , cmdDesc = "lookup a gismu/cmavo/lujvo/fu'ivla"
  , cmdProc = proc } where
    proc terms = do
      let valsi = terms : (map lojban (words terms) ++ words terms)
          word  (_,x,_) = x
          desc  (_,_,x) = x
          vtype (x,_,_) = x
      valsi' <- join `fmap` mapM lookupValsi valsi
      let valsiUniq = valsi \\ (map word valsi')
      lujvo <- catMaybes `fmap` mapM lookupLujvo valsiUniq
      dvalsi <- defLookup terms
      let results  = list dvalsi id $ lujvo ++ valsi'
          results' = map (\v -> filter ((==v) . word) results) valsi
          results'' = sortBy (comparing vtype) $ nub $ join $ results'
          out = map desc $ results''
      list (reply $ show terms ++ " not found") replies out

-- Lookup a definition
defLookup :: String -> LojbotAction [(JboValsiType,String,String)]
defLookup w = do
  db <- lift $ gets lojbotJboDB
  let phrase = map (\v -> (valsiType v,w,showValsi v)) $ defSub db w
      terms  = join $ map (map (\v -> (valsiType v,w,showValsi v)) . defSub db) $ words w
  return $ phrase ++ if null phrase then terms else []

-- Lookup a lookup a gismu/cmavo/extant-lujvo/fu'ivla
lookupValsi :: String -> LojbotAction [(JboValsiType,String,String)]
lookupValsi w = do
  db <- lift $ gets lojbotJboDB
  let simple = map (\v -> (valsiType v,w,showValsi v)) $ valsi db w
      byGloss = map (\v -> (valsiType v,w,showValsi v)) $ nub $ join $ map (valsiByGloss db) $ w : words w
  return $ simple ++ byGloss

-- Lookup the parts of a lujvo and display it.
lookupLujvo :: String -> LojbotAction (Maybe (JboValsiType,String,String))
lookupLujvo w =
    case rafsis w' of
      [] -> return Nothing
      rs -> do db <- lift $ gets lojbotJboDB
               Right (_,good) <- liftIO $ translate w'
               let selrafsi = map (findSelrafsi db) rs
               return $ Just $ (LujvoType,w,showLujvo (w/=w') w' rs selrafsi good)
    where w' = fixClusters w

-- Show a nonce lujvo.
showLujvo :: Bool -> String -> [String] -> [Maybe JboValsi] -> String -> String
showLujvo fixed w rs selrafsi good = 
    "lujvo {" ++ w ++ "}" ++ bool "" " (fixed)" fixed ++ rafsis rs ++ selrafs selrafsi
    ++ selgloss selrafsi ++ ": " ++ trans good
  where rafsis = (", with rafsis "++) . braces . commas
        selrafs = (", selrafsi "++) . braces . commas . catMaybes . map (fmap valsiWord)
        selgloss = (' ':) . parens . commas . catMaybes . map (fmap (slashes . valsiGloss))
        trans = fromMaybe "" . fmap head . match "/([^/]+)/"

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
        [] -> reply "no such selma'o"
        xs -> replies $ split' (commas list) ++ (map showValsi xs)
            where list = map showCmavo xs
                  showCmavo w = "{" ++ valsiWord w ++ "}: " ++ (slashes $ valsiGloss w)

-- Command to display help
cmdHelp :: Cmd
cmdHelp = Cmd
  { cmdName = ["help","h"]
  , cmdDesc = "help: <command>\nshows help"
  , cmdProc = proc } where
    proc cmd = do
      if isJust $ match "^[a-zA-Z'_]+$" cmd
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

replies :: [String] -> LojbotAction ()
replies (x:xs) | null xs   = reply x
               | otherwise = do reply $ x ++ more xs
                                setMore xs
replies [] = return ()

setMore :: [String] -> LojbotAction ()
setMore xs = do
  to <- gets replyTo
  mores <- lift $ gets lojbotMore
  lift $ modify $ \state -> state { lojbotMore = update (to,xs) mores }

reply :: String -> LojbotAction ()
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
replyTo :: Msg -> String
replyTo (Msg _ from (to:_)) | "#" `isPrefixOf` to = to
                            | otherwise           = from

-- A command data type
data Cmd = Cmd
    { cmdName :: [String]               -- Name and aliases
    , cmdDesc :: String                 -- A description for the help file
    , cmdProc :: String -> LojbotAction () -- The process to be run
    }

type LojbotAction = StateT Msg Lojbot

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
    , lojbotMlismu :: Mlismu       -- Mlismu handle
    , lojbotCamxes :: Maybe Camxes
    }
type Camxes = MVar (Handle,Handle)
type Jbocre = (String,AccessTable)
type AccessTable = Map String NickAccess
type NickAccess = (Bool,DelayNumber, DevoiceTime)
type DevoiceTime = CTime
type DelayNumber = CTime
startN = 10
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

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
   , lojbotMlismu = undefined
   , lojbotCamxes = undefined
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
    , confMlismu   :: FilePath      -- Mlismu fatci file
    } deriving (Read,Show)

-- Default configuration
defConfig :: Config
defConfig = Config
    { confNickName = "lojbot"
    , confNickPass = "3281h9k"
    , confAltNicks = ["lojbot_","lojbot__"]
    , confServer   = "127.0.0.1"
    , confPort     = 6667
    , confChans    = [ChanAssign "#lojbot" True ["@","?"]
                     ,ChanAssign "#lojban" True []]
    , confAdmins   = ["chrisdone"] 
    , confJbov     = "jbovlaste.db"
    , confLogFile  = LogF "lojbot.log"
    , confTrainDat = "lojban.log"
    , confMlismu = "fatci.txt"
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

isValidLojban :: String -> Lojbot Bool
isValidLojban line = do
  let text = newCmavo $ filter (/=')') line
  var <- gets lojbotCamxes
  case var of
    Nothing  -> undefined
    Just var -> liftIO $ do c@(in',out) <- takeMVar var
                            hPutStrLn in' text
                            line <- hGetLine out
                            putMVar var c
                            return $ line == text 

newCmavo :: String -> String
newCmavo = me'oi . la'oi . zo'oi where
    zo'oi = flip (subRegex (mkRegex zo'oiR)) " zo'e"
    zo'oiR = "(^zo'oi ([^ ]+)| zo'oi ([^ ]+))"
    la'oi = flip (subRegex (mkRegex la'oiR)) " zo'e"
    la'oiR = "(^la'oi ([^ ]+)| la'oi ([^ ]+))"
    me'oi = flip (subRegex (mkRegex me'oiR)) " zo'e"
    me'oiR = "(^me'oi ([^ ]+)| me'oi ([^ ]+))"

-- | Match a regular expression against a string.
match :: String -> String -> Maybe [String]
match = matchRegex . mkRegex

-- | braces "foo" = "{foo}"
braces :: String -> String
braces s = "{" ++ s ++ "}"

-- | parens "foo" = "(foo)"
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- | commas ["foo","bar"] = "foo, bar"
commas :: [String] -> String
commas = intercalate ", " . filter (/="")

-- | slashes ["foo","bar"] = "foo/bar".
slashes :: [String] -> String
slashes = intercalate "/" . filter (/="")

-- | [a] equivalent for "maybe".
list :: a -> ([b] -> a) -> [b] -> a
list nil _    [] = nil
list _   cons v  = cons v

-- | alias ["Jonathan","Jon","J"] = "Jonathan (Jon, J)".
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

