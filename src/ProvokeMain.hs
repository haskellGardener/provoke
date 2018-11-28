{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-| Time-stamp: <2018-11-28 12:08:00 CST>

Module      : ProvokeMain 
Copyright   : (c) Robert Lee, 2015-2018
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provoke's main operational functions.

-}

{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘

⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ Omega Symbol Key ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
                   early or abnormal termination ⋅⋅⋅ Ω
                            termination (normal) ⋅⋅⋅ ω
                                    a new thread ⋅⋅⋅ ⋔
          code that can throw an error exception ⋅⋅⋅ ⏈
                                  loop-like code ⋅⋅⋅ ➿
                              a loop-like repeat ⋅⋅⋅ ↺
                           end of loop-like code ⋅⋅⋅ 🔚
               an uninterruptible exception mask ⋅⋅⋅ ☔
                code that can emit IO exceptions ⋅⋅⋅ ☢
                a warning about troublesome code ⋅⋅⋅ ⚠
  an imperative concerning imprudent code change ⋅⋅⋅ ⚡
                  a forbidden/nonsense condition ⋅⋅⋅ ⛞
                          a timed race condition ⋅⋅⋅ 🏁
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

{-
  TODO: add quit with final program: qq
        add remove command: rm num meaning remove charge num
        add shell variable change: vc variablename -- dangerous for XMPP w/o end to end encryption

        add --auto-reload boolean which automatically reloads the provoke config file on change to config file.
        add --auto-alert boolean which alerts the user that the provoke config file has been updated.
        Add watchdir capability.
        Add cabal read and watch capability
        Add detect new files capability
        Add multiple read only jid recipients for monitoring.
-}

module ProvokeMain where

-- Local Imports

import Lading
import XMPP   ( chanOutXMPP, disconnect, getChat, rawTran, startXMPP, styleMsg, transmit, warnDisco )

-- Explicit Imports

import Control.Concurrent       ( readMVar                                                                        )
import Control.Concurrent.Async ( Async, cancel, cancelWith, wait                                                 )
import Control.Concurrent.STM   ( TMVar, TVar
                                , atomically, dupTChan, newEmptyTMVar, newEmptyTMVarIO, newTVarIO, putTMVar
                                , readTVar, tryPutTMVar, tryReadTMVar, tryTakeTMVar, writeTChan, writeTVar        )
import Control.Exception.Base   ( AsyncException(UserInterrupt)                                                   )
import Control.Monad            ( forM_, forever, join, void                                                      )
import Data.Char                ( isDigit, isLetter                                                               )
import Data.Maybe               ( catMaybes, fromJust, fromMaybe, isJust, isNothing, maybe                        )
import Foreign.C.Types          ( CClock(..)                                                                      )
import Network.HostName         ( getHostName                                                                     )
import Network.Xmpp             ( Jid, jidFromText                                                                )
import Network.Xmpp.IM          ( bodyContent, getIM, imBody                                                      )
import Shelly                   ( FilePath, Sh
                                , asyncSh, canonicalize, chdir, echo, echo_n, errorExit, escaping, exit, fromText
                                , get_env_all, get_env_text , handleany_sh, inspect, lastExitCode, liftIO
                                , log_stderr_with, log_stdout_with, ls, print_commands, pwd, readfile, run
                                , setCallback, setCreateProcessGroup, setenv, shelly, test_d, test_e, test_f, toTextIgnore
                                , unless, verbosely, when                                                         )
import System.Console.GetOpt    ( ArgDescr(NoArg, ReqArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo )
import System.Environment       ( getArgs                                                                         )
import System.Exit              ( exitFailure                                                                     )
import System.FilePath          ( takeFileName                                                                    )
import System.IO                ( Handle, stderr, stdout                                                          )
import System.IO.Unsafe         ( unsafePerformIO                                                                 )
import System.Posix.Process     ( ProcessTimes, elapsedTime, getProcessGroupIDOf, getProcessTimes                 )
import System.Posix.Signals     ( sigTERM, signalProcessGroup                                                     )
import System.Posix.Types       ( CPid                                                                            )
import System.Process.Internals ( ProcessHandle(..), ProcessHandle__(..)                                          )

-- Qualified Imports

import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Prelude              as PRLD
import qualified System.INotify       as IN
import qualified Data.Attoparsec.Text as X
import qualified System.FilePath.Glob as G

-- Undisciplined Imports

import Prelude hiding (FilePath)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

{-# NOINLINE __eventBarrierTMVar #-}
__eventBarrierTMVar :: TMVar PRLD.FilePath
__eventBarrierTMVar = unsafePerformIO (newEmptyTMVarIO) -- Create a barrier to pause on events (default closed)

{-# NOINLINE __transactionTVar #-}
__transactionTVar :: TVar Bool
__transactionTVar = unsafePerformIO (newTVarIO False)   -- Transaction predicate, when True new events are ignored until set to False.

{-# NOINLINE __runningTVar #-}
__runningTVar :: TVar (Maybe Running)
__runningTVar = unsafePerformIO (newTVarIO Nothing)     -- Charge running/halted state variable

{-# NOINLINE __chargesTVar #-}
__chargesTVar :: TVar [T.Text]
__chargesTVar = unsafePerformIO (newTVarIO [])          -- Charge sequence (Shell commands)

{-# NOINLINE __curChargesTVar #-}
__curChargesTVar :: TVar [Int]
__curChargesTVar = unsafePerformIO (newTVarIO [])       -- Charge Indice for altering/reordering charge sequence. Empty indicates no reorder.

{-# NOINLINE __oncePTVar #-}
__oncePTVar :: TVar Bool
__oncePTVar = unsafePerformIO (newTVarIO False)         -- Last dispatch predicate, set by cli option or interactive command

-- End of Module global tvars
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

verbosity :: Bool -> Sh a -> Sh a
verbosity p = verbosely . print_commands p

toPreludeFilePath :: Shelly.FilePath -> PRLD.FilePath
toPreludeFilePath = T.unpack . toTextIgnore

data Running = Running { startTimeRun :: ProcessTimes
                       , asyncRun :: Async ()
                       , tvarRun :: TMVar ProcessHandle
                       }

data WatchDir = WatchDir { directory :: FilePath
                         , files :: [FilePath]
                         , globs :: [] String
                         } deriving (Show)

watchDirToFPs :: WatchDir -> Sh [FilePath]
watchDirToFPs WatchDir{..} = do
  existsP <- test_e directory
  isDirectoryP <- test_d directory
  if existsP && isDirectoryP                 -- Ignore non-existant or non-directory filesystem objects.
  then chdir directory (ls "." >>= mapM canonicalize . filter (\fp -> isMatch (toPreludeFilePath fp) || isFile fp))
  else pure []

  where patterns = map (G.simplify . G.compile) globs
        matchers = map G.match patterns
        isMatch fp = any ($ fp) matchers

        fileMatchers = map (==) files
        isFile fp = any ($ fp) fileMatchers

data Flag = Verbose
          | Version
          | Once
          | Immediate
          | Terminate
          | Charge String
          | ConfigFile T.Text
          | JID (Either () Jid)
            deriving Show

data Config = Config { verboseP    :: Bool
                     , onceP       :: Bool
                     , immediateP  :: Bool
                     , terminateP  :: Bool
                     , flagCharge  :: T.Text
                     , configFile  :: T.Text
                     , configJid   :: Maybe Jid
                     , flagMap     :: FlagMap
                     } deriving Show

type FlagMap = M.Map String Flag

options :: [ OptDescr                           Flag                       ]
options  = [ Option ['v'] ["verbose"  ] (NoArg  Verbose                    ) "chatty output"
           , Option ['V'] ["version"  ] (NoArg  Version                    ) "show version number"
           , Option ['c'] ["command"  ] (ReqArg Charge "command"           ) "command to run"

           , Option ['o'] ["once"     ] (NoArg  Once                       ) "run command once"
           , Option ['i'] ["immediate"] (NoArg  Immediate                  ) "run command immediately"
           , Option ['t'] ["terminate"] (NoArg  Terminate                  ) "terminate on command error"
           , Option ['f'] ["config"   ] (ReqArg configFileF "provoke.conf" ) "provoke config file"
           , Option ['j'] ["jid"      ] (ReqArg stringToJid "xyz@zyx.zzzz" ) "XMPP jid"
           ]
  where
    configFileF :: String -> Flag
    configFileF = ConfigFile . T.pack

    stringToJid :: String -> Flag
    stringToJid = JID . maybe (Left ()) Right . jidFromText . T.pack

clOpts :: [] String -> IO (FlagMap, [] String)
clOpts argv = case getOpt Permute options argv of
                (flags, directories,[]) -> pure (mkFlagMap flags, directories)
                (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: provoke --command='foo' [OPTION...]" -- TODO: improve this.                                                                     -- ⚠

    mkFlagMap :: [] Flag -> FlagMap
    mkFlagMap = M.fromList . map flagPair

    flagPair :: Flag -> (String, Flag)
    flagPair flag = (head . words $ show flag, flag)

updateCharges :: Config -> Sh ()
updateCharges Config {..} = do
  unless (T.null configFile) $ do
    cliConfigP <- test_f $ fromText configFile
    unless cliConfigP . errorExit $ T.concat ["config file: ", configFile, " not found"]                                                             -- Ω
  configFileP <- T.null configFile ? test_f "provoke.config" $ pure True
  let configFilePath = fromText (T.null configFile && configFileP ? "provoke.config" $ configFile)
  charges <- (configFileP ? readfile configFilePath $ pure flagCharge) >>= pure . T.lines
  when verboseP $ echo_n "charges: " >> inspect charges
  atomSh $ do
    writeTVar __chargesTVar charges
    writeTVar __curChargesTVar [0..(length charges - 1)]
  when (T.null flagCharge && null charges) $ errorExit "-c command REQUIRED"                                                                         -- Ω

-- | provokeMain is called by main (provides a nice break from main)
provokeMain :: IO ()
provokeMain = do
  (flagMap', _) <- getArgs >>= clOpts
  provoke Config { verboseP   = M.member "Verbose"                                                         flagMap'
                 , onceP      = M.member "Once"                                                            flagMap'
                 , immediateP = M.member "Immediate"                                                       flagMap'
                 , terminateP = M.member "Terminate"                                                       flagMap'
                 , flagCharge = maybe T.empty (\(Charge s) -> T.pack s) $ M.lookup "Charge"                flagMap'
                 , configFile = maybe T.empty (\(ConfigFile t) -> t) $ M.lookup "ConfigFile"               flagMap'
                 , configJid  = maybe Nothing (\(JID e) -> either (const Nothing) Just e) $ M.lookup "JID" flagMap'
                 , flagMap    =                                                                            flagMap'
                 }

-- | provoke is the entry point for this application, called once by provokeMain
provoke :: Config -> IO ()
provoke config@Config {..} = do
  iNot <- IN.initINotify -- perfunctory: required by the inotify module

  shelly . escaping False . verbosity verboseP $ do -- start up provoke system
    mSession <- case M.lookup "JID" flagMap of
      Just (JID (Left ()))   -> errorExit "Jid provided is not well formed"                                                                          -- Ω
      Just (JID (Right toJ)) -> do cwd <- pwd >>= pure . T.pack . takeFileName . toPreludeFilePath
                                   hnm <- liftIO $ getHostName >>= pure . T.pack
                                   let commandR = T.intercalate "▱" [hnm, cwd, "command"]
                                       chargeR = T.intercalate "▱" [hnm, cwd, "chargeLog"]
                                   eSess <- liftIO $ startXMPP toJ chargeR
                                   case eSess of
                                     Left _ -> errorExit "XMPP session Failure"                                                                      -- Ω
                                     Right Nothing -> errorExit "Recipient Jid not available"                                                        -- Ω
                                     Right (Just xmppPK) -> do
                                       ecomSess <- liftIO $ startXMPP toJ commandR
                                       case ecomSess of
                                         Left _ -> errorExit "XMPP session Failure"                                                                  -- Ω
                                         Right Nothing -> errorExit "Recipient Jid not available"                                                    -- Ω
                                         Right (Just cXmppPK) -> writeTVarSh xmppPKTVarCom $ Just cXmppPK
                                       pure $ Just xmppPK
      _ -> pure Nothing
    writeTVarSh xmppPKTVar mSession
    whenJust mSession $ echo "XMPP active"
    when verboseP $ inspect flagMap
    updateCharges config
    writeTVarSh __oncePTVar onceP

    fps <- mapM watchDirToFPs watchDirs >>= pure . join -- establish what triggers events on the filesystem
    when verboseP $ inspect fps
    liftIO . mapM_ (\fp -> IN.addWatch iNot [IN.Modify, IN.CloseWrite] fp (callbackF fp)) $ map toPreludeFilePath fps -- establish event watchers
    prevProcTime <- liftIO getProcessTimes

    unless (immediateP && onceP) . void $        -- launch the manual command loop if applicable
      do mXmppPK <- readTVarSh xmppPKTVarCom     -- maybe there's xmpp joy, or sadly Nothing
         tchan <- atomSh $ dupTChan commandTChan -- get a local copy of the broadcast chan
         case mXmppPK of                         -- got XMPP command session/resource?
           Nothing -> asyncSh (chanOutStdout tchan) -- do it via terminal only
                   >> asyncSh commandLoop           -- commandLoop thread for receiving commands typed at the terminal
           Just xmppPK -> asyncSh (liftIO $ chanOutXMPP xmppPK tchan) -- thread to receive chan broadcast and send to XMMP partner
                       >> asyncSh (imXMPP config xmppPK)              -- thread to receive XMPP IM and send to commandResponse
                       >> asyncSh commandLoop                         -- commandLoop thread for receiving commands typed at the terminal


    if immediateP                                               -- immediateP from command line option
    then do
      atomSh $ putTMVar __eventBarrierTMVar "Immediate"         -- wedge the event barrier
      respondToEvents (prevProcTime { elapsedTime = 0 }) config -- launch dispatch
    else do
      statusMsg
      respondToEvents prevProcTime config                       -- dispatch watches/waits till event barrier is wedged open
  where
    -- | commandLoop repeatedly reads a line at a time and passes each line to commandResponse for processing
    commandLoop :: Sh ()
    commandLoop = forever $ do
      ln <- liftIO $ TIO.getLine                 -- read a line from the terminal or whatever stdin is
      when verboseP $ inspect (ln, T.length ln)
      commandResponse config ln                  -- parse the line (act on it as well)

    watchDirs = [ WatchDir { directory = "src"   -- TODO: watchDirs needs to be settable on the command line, this would be default                  -- ⚠
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "src-exe"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "src-test"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "src-doctest"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "src-benchmark"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "."
                           , files = []
                           , globs = ["*.cabal", "*.hs", "*.yaml"]
                           }
                , WatchDir { directory = "lib"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/V1_1"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/Base"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/PL"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/PL/Base"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/Source"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "lib/Source/CallCredit"
                           , files = []
                           , globs = ["*.hs"]
                           }
                , WatchDir { directory = "app"
                           , files = []
                           , globs = ["*.hs"]
                           }
                ]

    -- | callbackF is the inotify callback function
    callbackF :: String -> IN.Event -> IO ()
    callbackF fp _ = putTMVarIO __eventBarrierTMVar fp -- __eventBarrierTMVar is watched by respondToEvents

-- | respondToEvents is a long running thread that conditionally cycles over an inner event loop dispatching charges
--   Inotify events and events generated by interactive commandResponse can trigger charge dispatches, etc.
respondToEvents :: ProcessTimes -> Config -> Sh ()
respondToEvents prevProcTime Config {..} = eventLoop prevProcTime
  where
    deltaThreshold = 100 -- unit: 1/100 second

    onError err = do                     -- exception/error callback for charges that blow up
      writeTVarSh __runningTVar Nothing  -- state set to halted
      when verboseP $ inspect err
      when terminateP $ do               -- exit on terminate flag set by command line option or interactive command
        verboseP ? errorExit "provoke terminating: 'terminate' flag set" $ exit 1                                                                    -- Ω

    eventLoop prevProcTime' = do
      (fp, procTime) <- liftIO $ do
        fp <- takeTMVarIO __eventBarrierTMVar -- pause for event, once barrier is wedged open collect event from barrier (barrier snaps shut)
        procTime <- getProcessTimes           -- note event time
        pure (fp, procTime)
      when verboseP $ inspect (fp, elapsedTime procTime)
      exitOnQuit $ Just fp                         -- quit events are established by interactive command                                             -- Ω
      transactionP <- readTVarSh __transactionTVar -- retrieve transaction predicate
      if transactionP
      then eventLoop prevProcTime'                 -- during transaction ignore charge dispatch events
      else do
        let delta = elapsedTime procTime - elapsedTime prevProcTime'
        when (delta > deltaThreshold) $ do         -- only respond to deltas larger than deltaThreshold
          setenv "LASTEXITCODE" "0"                -- set/initialize LASTEXITCODE environment variable
          xcho "waiting for charge to complete..."
          charges <- atomSh $ do
            charges_t <- readTVar __chargesTVar                                   -- get sequence of charges
            curCharges_indice <- readTVar __curChargesTVar                        -- get curCharges indice, can be empty
            let inbounds c = c < length charges_t && c >= 0                       -- index validation predicate
                curCharges = null curCharges_indice ? [0..(length charges_t - 1)] -- generate charge indice
                                                    $ curCharges_indice           -- prefer current charge indice if not empty

            pure . map (charges_t !!) $ filter inbounds curCharges                -- accepted charge list

          mXmppPK <- readTVarSh xmppPKTVar
          just mXmppPK $ {- (xcho "XMPP charge logging" <*) . -} (\xmppPK -> liftIO $ do transmit xmppPK "--- Charge Dispatch Start ---")

          forM_ charges $ \charge -> do -- dispatch charges sequentially, stop if charge exits non-zero
            xcho $ T.append "Running: " charge
            priorExitCode <- get_env_text "LASTEXITCODE"
            if priorExitCode == "0"
            then do       -- proceed if prior exit code was 0 (shell parlance for 'good' exit)
              emptyTMvar <- atomSh newEmptyTMVar
              dispatch <- asyncSh . handleany_sh onError . void      -- dispatch charge asynchronously
                                  . log_stderr_with (logE mXmppPK)   -- monitor charge stderr
                                  . log_stdout_with (logO mXmppPK)   -- monitor charge stdout
                                  $ do setCallback (Just (void . atomically . tryPutTMVar emptyTMvar))
                                       setCreateProcessGroup True
                                       run (fromText charge) []      -- dispatch charge
              writeTVarSh __runningTVar (Just Running { startTimeRun = procTime -- set running flag
                                                      , asyncRun = dispatch     -- record dispatch async for potential kill/cancel
                                                      , tvarRun = emptyTMvar
                                                      }
                                        )
              liftIO $ wait dispatch                                        -- wait for dispatch to complete
              mFp <- liftIO $ atomically $ tryReadTMVar __eventBarrierTMVar -- check for events after charge is finished
              exitOnQuit mFp                                                -- exitOnQuit set by interactive command                                 -- Ω
              chargeExitCode <- lastExitCode                                -- get charge exit code
              mRunning <- readTVarSh __runningTVar                          -- Nothing indicates the process was killed by interactive command
              setenv "LASTEXITCODE" $ (isNothing mRunning ? "1" $ tshow chargeExitCode)
            else ignore -- skip charge because a prior charge in the sequence had non-zero exit code

          afterTime <- liftIO getProcessTimes
          finalCode <- get_env_text "LASTEXITCODE"

          -- report to user

          let fcMsg = isJust mXmppPK ? (finalCode == "0" ? "" $ T.concat ["Error: exit (", finalCode, "). "]) $ ""
              msg = T.concat ["Dispatch duration: ", tshow . centToFrac $ elapsedTime afterTime - elapsedTime procTime, " seconds"]
          just mXmppPK $ (\xmppPK -> liftIO $ do transmit xmppPK $ T.concat [ finalCode == "0" ? "--- " $ "--- ⃠ "
                                                                            , "Charge Dispatch "
                                                                            , finalCode == "0" ? "Success" $ "Failure ⃠"
                                                                            , " ---\n"
                                                                            ]
                                                 transmit xmppPK $ T.concat [msg, "\n"])
          xcho $ T.concat ["Charge Completed. ", fcMsg, msg]
          (liftIO $ atomically $ tryReadTMVar __eventBarrierTMVar) >>= exitOnQuit                                                                    -- Ω

          oncePT <- readTVarSh __oncePTVar -- final dispatch ?
          when oncePT $ do
            when verboseP . xcho $ onceP ? "Provoke terminating: 'once' flag set"
                                         $ "Provoke terminating: Manual rq command issued."
            exit 0                                                                                                                                   -- Ω

          writeTVarSh __runningTVar Nothing        -- charges halted, set running to Nothing
          changeWaitMsg

        latestProcTime <- liftIO getProcessTimes
        eventLoop latestProcTime                   -- cycle again
      where
        logX :: Maybe XmppPK -> Handle -> (T.Text -> IO ()) -- logs dispatch stdout/stderr to xmpp session if exists, else to terminal
        logX Nothing h | h == stdout = out_chans [ TIO.hPutStrLn stderr . T.append "    " ]
                       | h == stderr = out_chans [ TIO.hPutStrLn stderr . T.append "e   " ]
                       | otherwise = error "Illegal Handle"

        logX (Just xmppPK) h | h == stdout = out_chans [ void . rawTran xmppPK . styleMsg "color:#00461e;font-size: 12pt;" . T.append circle ]
                             | h == stderr = out_chans [ void . rawTran xmppPK . styleMsg "color:#770000;font-size: 12pt;" . T.append circle ]
                             | otherwise = error "Illegal Handle"
        logE mXmppPK = logX mXmppPK stderr
        logO mXmppPK = logX mXmppPK stdout
        circle = "❍　" -- Unicode lurks here, pretty.

quitString :: [] Char
quitString = "Manual quit: q"

quitText :: T.Text
quitText = T.pack quitString

exitOnQuit :: Maybe ([] Char) -> Sh ()
exitOnQuit (Just t) | t == quitString = exit 0                                                                                                       -- Ω
                    | otherwise = ignore
exitOnQuit _ = ignore

xcho_g :: ReportMsg -> T.Text -> Sh ()
xcho_g rMsg_pp msg = atomSh $ writeTChan commandTChan rMsg_pp { rmMsg = msg }

xcho, xcho_n :: T.Text -> Sh ()
xcho = xcho_g reportMsg
xcho_n = xcho_g reportMsg { rmNl = False }

data Style = StdoutSty
           | StderrSty
           | CommandSty

styleXcho :: Style -> T.Text -> Sh ()
styleXcho style msg = atomSh $ writeTChan commandTChan reportMsg { rmMessage = Just $ styleMsg (styler style) msg
                                                                 , rmMsg = msg
                                                                 }
  where styler StdoutSty = "color:#00461e;font-size: 12pt;"
        styler StderrSty = "color:#F00;font-size: 14pt;"
        styler CommandSty = "color:#770077;font-size: 14pt;"



-- xchoc, xchoc_n :: T.Text -> T.Text -> Sh ()
-- xchoc cmd = xcho_g reportMsg { rmCmd = cmd }
-- xchoc_n cmd = xcho_g reportMsg { rmNl = False, rmCmd = cmd }


changeIgnoreMsg :: Sh ()
changeIgnoreMsg = xcho "Ignoring filesystem change(s)" >> promptMsg

changeWaitMsg :: Sh ()
changeWaitMsg = xcho "Awaiting filesystem change(s)" >> promptMsg

promptMsg :: Sh ()
promptMsg = do
  (mRunning, mCommand) <- atomSh $ do mR <- readTVar __runningTVar
                                      mC <- readTVar xmppPKTVarCom
                                      pure (mR,mC)
  let msg = isNothing mRunning ? "Halted" $ "Running"
  isJust mCommand ? xcho msg $ (echo_n $ T.concat ["\n", msg, " $ "])

statusMsg :: Sh ()
statusMsg = do tP <- readTVarSh __transactionTVar
               tP ? changeIgnoreMsg $ changeWaitMsg

centToFrac :: CClock -> Double
centToFrac (CClock i) = fromIntegral i / 100.0


data Orders = RunSetCurCharges [Int]
            | RunQuitSetCurCharges [Int]
            | ListCharges [Int]
            | UnknownOrder

-- | parse interactive commands containing integer arguments
--   e.g. r 1,2,4
ordersParse :: X.Parser Orders
ordersParse = do
  X.skipSpace
  w <- word
  ints <- X.many' rep

  pure $ case w of
           "r"  -> RunSetCurCharges     ints
           "rq" -> RunQuitSetCurCharges ints
           "l"  -> ListCharges          ints
           _    -> UnknownOrder

  where
    word = X.takeWhile1 isLetter
    rep = do
      X.skipWhile (not . isDigit) -- any non-digit separator will do
      X.decimal

-- | chargeStatusReport retrieves running state and formats a (very small) report
chargeStatusReport :: Sh T.Text
chargeStatusReport = do
  mRunning <- readTVarSh __runningTVar
  case mRunning of
    Nothing -> pure "    No Process"
    Just Running{..} -> do afterTime <- liftIO getProcessTimes
                           pure $ T.concat [ "    Charge Dispatch in progress, elapsed time: "
                                           , tshow . centToFrac $ elapsedTime afterTime - elapsedTime startTimeRun
                                           , " seconds"
                                           ]
imXMPP :: Config -> XmppPK -> Sh ()
imXMPP config xmppPK = forever $ do
  mess <- liftIO $ getChat xmppPK
  case getIM mess of
    Just im -> commandResponse config . T.intercalate "\n" . map bodyContent $ imBody im
    Nothing -> pure ()


-- | commandResponse parses manual input from stdin and responds accordingly
commandResponse :: Config -> T.Text -> Sh ()
commandResponse config@Config{..} txt_pp = lilParse txt_pp
  where

    lilParse :: T.Text -> Sh ()

    lilParse ""   = statusMsg

    lilParse "e"  = do styleXcho CommandSty "end transaction"
                       writeTVarSh __transactionTVar False
                       changeWaitMsg

    lilParse "gs" = do mXmppPK <- readTVarSh xmppPKTVarCom --  *> pure Nothing
                       void . log_stderr_with (logE mXmppPK) . log_stdout_with (logO mXmppPK) $ run "git" ["status"]
                       statusMsg

    lilParse "?"  = lilParse "h"
    lilParse "h"  = do xcho . T.append "Help:\n" . T.intercalate "\n" $ map (T.append "    ")
                              [ "e   End transaction"
                              , "env Show shell environment variables"
                              , "gs  Run 'git status'"
                              , "h   Help (this message)"
                              , "k   Kill running command"
                              , "l   List all commands"
                              , "ld  Reload command(s) / config file"
                              , "p   Process status"
                              , "q   Quit"
                              , "r   Run immediate, remove last numbered r command, and end transaction if any"
                              , "rq  Run immediate and quit (similar to provoke --immediate --once)"
                              , "rr  Repeat last numbered r command if any"
                              , "s   Current Status"
                              , "t   Start transaction"
                              ]
                       statusMsg

    lilParse "k"  = do mRunning <- readTVarSh __runningTVar
                       case mRunning of
                         Nothing -> styleXcho CommandSty "no process, nothing to do" >> statusMsg
                         Just Running{..} -> do styleXcho CommandSty "kill running command"
                                                mProcessHandle <- atomSh $ tryReadTMVar tvarRun
                                                mPID <- case mProcessHandle of
                                                  Just (ProcessHandle phandle _) -> do
                                                    ph__ <- liftIO $ readMVar phandle
                                                    pure $ case ph__ of
                                                      OpenHandle ph -> Just ph
                                                      _ -> Nothing
                                                  Nothing -> pure Nothing
                                                case mPID of
                                                  Just pid -> do xcho $ T.append "SIGNALING " (tShow pid)
                                                                 liftIO $ do
                                                                   gid <- getProcessGroupIDOf pid
                                                                   signalProcessGroup sigTERM gid
                                                  Nothing -> pure ()
                                                liftIO $ cancelWith asyncRun UserInterrupt

    lilParse "l"  = do charges_t <- readTVarSh __chargesTVar
                       forM_ (zip [1::Int ..] charges_t) $ \(n,c) -> styleXcho CommandSty $ T.concat ["    ", tshow n, ": ", c]
                       statusMsg

    lilParse "env" = do get_env_all >>= styleXcho CommandSty . T.append "env\n"
                                                             . T.intercalate "\n"
                                                             . map (\(k,v) -> T.append "❍    " $ T.concat [T.pack k, "=", T.pack v])
                        statusMsg

    lilParse "ld" = do updateCharges config
                       styleXcho CommandSty "commands reloaded"
                       statusMsg

    lilParse "p"  = chargeStatusReport >>= styleXcho CommandSty >> statusMsg

    lilParse "q"  = do xcho quitText                              -- "q" is a force quit
                       (mXmppPK, mXmppPKCom) <- atomSh $ do       -- retrieve xmpp sessions if any
                          mCharge <- readTVar xmppPKTVar
                          mCommand <- readTVar xmppPKTVarCom
                          pure (mCharge, mCommand)
                       just mXmppPK    (\xmppPK -> liftIO . void . warnDisco xmppPK $ T.append "Provoke exiting, " quitText)
                       just mXmppPKCom (\xmppPK -> liftIO . void . warnDisco xmppPK $ T.append "Provoke exiting, " quitText)

                       mRunning <- atomSh $ do                    -- prevent race conditions. On quit every thread is forfeit.
                         void $ tryTakeTMVar __eventBarrierTMVar  -- clear the barrier regardless of state
                         putTMVar __eventBarrierTMVar quitString  -- wedge the barrier with quitString
                         writeTVar __oncePTVar True               -- set for final dispatch
                         readTVar __runningTVar                   -- get running status
                       just mRunning $ \Running{..} -> do
                         styleXcho CommandSty "terminating current charge dispatch"
                         mProcessHandle <- atomSh $ tryReadTMVar tvarRun
                         mPID <- case mProcessHandle of
                           Just (ProcessHandle phandle _) -> do
                             ph__ <- liftIO $ readMVar phandle
                             pure $ case ph__ of
                               OpenHandle ph -> Just ph
                               _ -> Nothing
                           Nothing -> pure Nothing
                         case mPID of
                           Just pid -> do xcho $ T.append "SIGNALING " (tShow pid)
                                          liftIO $ do
                                            gid <- getProcessGroupIDOf pid
                                            signalProcessGroup sigTERM gid
                           Nothing -> pure ()
                         liftIO $ cancel asyncRun -- kill current charge

                       just mXmppPK    $ liftIO . disconnect  -- disconnect Charge XMPP session, if exists
                       just mXmppPKCom $ liftIO . disconnect  -- disconnect Command XMPP session, if exists

                       exit 0                                                                                                                        -- Ω

    lilParse "r" =  atomSh $ do                                -- run dispatch immediately
                      writeTVar __transactionTVar False        -- end transaction if any
                      putTMVar __eventBarrierTMVar "Manual: r" -- wedge event barrier open with manual event (see respondToEvents)
                      writeTVar __curChargesTVar []            -- empty current charge index

    lilParse "rr" = atomSh $ do                                -- run dispatch immediately with current charge index intact
                      writeTVar __transactionTVar False        -- end transaction if any
                      putTMVar __eventBarrierTMVar "Manual: r" -- wedge event barrier open with manual event (see respondToEvents)

    lilParse "rq" = do mRunning <- atomSh $ do                       -- run dispatch and quit
                         mRunning <- readTVar __runningTVar          -- get running status
                         when (isNothing mRunning) $ do              -- when halted
                           writeTVar __transactionTVar False         --    end transaction if any
                           putTMVar __eventBarrierTMVar "Manual: r"  --    set for charge dispatch (wedge the barrier with "Manual: r")
                           writeTVar __oncePTVar True                --    Set for final run
                         pure mRunning
                       whenJust mRunning $ do
                         styleXcho CommandSty "Process running: kill running process first, or wait till it finishes before rq"
                         statusMsg

    lilParse "s"  = do when verboseP $ styleXcho CommandSty "❍    verbose mode set"
                       styleXcho CommandSty $ onceP ? "❍    run once" $ "❍    run repeatedly"
                       when immediateP $ styleXcho CommandSty "❍    run immediate"
                       styleXcho CommandSty $ terminateP ? "❍    terminate on run command error" $ "❍    ignore run command errors"
                       styleXcho CommandSty $ T.concat ["❍    run command '", flagCharge, "' "]
                       chargeStatusReport >>= styleXcho CommandSty . T.append "❍    "
                       statusMsg

    lilParse "t"  = do styleXcho CommandSty "start transaction (enter 'e' to end transaction, or 'r' for immediate run)"
                       writeTVarSh __transactionTVar True
                       changeIgnoreMsg

    lilParse x    = case X.parseOnly ordersParse x of     -- Send unmatched interactive command entries to ordersParse
                      Left _             -> scold x       -- borked command, scold user
                      Right UnknownOrder -> scold x       -- borked command, scold user
                      Right order        -> runSub order  -- handle order

      where scold m = styleXcho StderrSty (T.append "Unknown command: " m) >> statusMsg

            runSub (RunSetCurCharges []) = lilParse "r"
            runSub (RunSetCurCharges ns) = do p <- atomSh $ do
                                                charges_t <- readTVar __chargesTVar
                                                let inbounds c = c < length charges_t && c >= 0
                                                    charges = filter inbounds nsminus
                                                if length charges /= length nsminus
                                                then pure False
                                                else do
                                                  writeTVar __curChargesTVar charges
                                                  writeTVar __transactionTVar False
                                                  putTMVar __eventBarrierTMVar "Manual: r"
                                                  pure True
                                              unless p $ styleXcho StderrSty "Unknown command number for r" *> statusMsg
              where nsminus = map pred ns -- pred equivalent to (subtract 1), (-) cannot be sectioned (Haskell Gods)

            runSub (RunQuitSetCurCharges []) = lilParse "r"
            runSub (RunQuitSetCurCharges ns) = do mErrMsg <- atomSh $ do
                                                    charges_t <- readTVar __chargesTVar
                                                    let inbounds c = c < length charges_t && c >= 0
                                                        charges = filter inbounds nsminus
                                                    if length charges /= length nsminus
                                                    then pure $ Just "Unknown command number for rq"
                                                    else do
                                                      mRunning <- readTVar __runningTVar -- Get running status
                                                      whenJust mRunning $ do
                                                        writeTVar __curChargesTVar charges
                                                        writeTVar __transactionTVar False
                                                        putTMVar __eventBarrierTMVar "Manual: r"
                                                        writeTVar __oncePTVar True       -- Set for final run
                                                      pure $ isJust mRunning ? Just "Process running: kill running process first before rq" $ Nothing
                                                  just mErrMsg ((statusMsg <*) . styleXcho StderrSty)

                                                  where nsminus = map pred ns -- pred equivalent to (subtract 1), (-) cannot be sectioned (Haskell Gods)

            runSub (ListCharges []) = lilParse "l"
            runSub (ListCharges ns) = do charges_t <- readTVarSh __chargesTVar
                                         let inbounds c = c < length charges_t && c >= 0
                                             charges_i  = filter inbounds nsminus
                                             commands   = zip ns $ map (charges_t !!) charges_i
                                         if length charges_i /= length nsminus
                                         then styleXcho StderrSty "Unknown command number for l"
                                         else forM_ commands $ \(n,c) -> xcho $ T.concat ["    ", tshow n, ": ", c]
                                         statusMsg
                                         where nsminus = map pred ns -- pred equivalent to (subtract 1), (-) cannot be sectioned (Haskell Gods)

            runSub _ = ignore
              
    logX :: Maybe XmppPK -> Handle -> (T.Text -> IO ())
    logX Nothing h | h == stdout = out_chans [ TIO.hPutStrLn stderr . T.append "    " ]
                   | h == stderr = out_chans [ TIO.hPutStrLn stderr . T.append "e   " ]
                   | otherwise = error "Illegal Handle"

    logX (Just xmppPK) h | h == stdout = out_chans [ void . rawTran xmppPK . styleMsg "color:#00461e;font-size: 12pt;" . T.append circle ]
                         | h == stderr = out_chans [ void . rawTran xmppPK . styleMsg "color:#770000;font-size: 12pt;" . T.append circle ]
                         | otherwise = error "Illegal Handle"
    logE mXmppPK = logX mXmppPK stderr
    logO mXmppPK = logX mXmppPK stdout

    circle = "❍　" -- Unicode lurks here.


{- The self cleaning shell:
#!/bin/bash
...
trap 'KILL -TERM $PID' TERM INT
$JAVA_EXECUTABLE $JAVA_ARGS &
PID=$!
wait $PID
wait $PID
EXIT_STATUS=$?
-}
