{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, UndecidableInstances, TypeFamilies  #-}

{-|

Module      : Lading
Copyright   : (c) Robert Lee 2015
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Lading module for provoke

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

module Lading
where

import Data.Maybe (fromMaybe)  
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Text as T
import Control.Monad (void, forever)
import Control.Concurrent.STM ( STM, TChan, TMVar, TVar, readTVar, writeTVar
                              , atomically, putTMVar, takeTMVar, readTChan, newTVarIO, newBroadcastTChanIO
                              )
import Shelly (Sh, echo, echo_n, liftIO)
import Network.Xmpp (Session, Jid, Message)
import System.IO (Handle, stdin, stdout)

class TShow t where
  tshow :: (->) t T.Text

instance {-# OVERLAPPABLE #-} (Show a) => TShow a where
  tshow = T.pack . show

instance {-# OVERLAPPING #-} TShow T.Text where
  tshow = id

instance {-# OVERLAPPING #-} TShow [Char] where
  tshow = T.pack

type TableRow = [] T.Text
type TableBody = [] TableRow

data Table = Table { headTbl  :: Maybe TableRow
                   , bodyTbl  :: TableBody
                   , footTbl  :: Maybe TableRow
                   , colsTbl  :: Int
                   , styleTbl :: T.Text
                   } deriving (Show)

emptyTable :: Table
emptyTable = Table { headTbl = Nothing
                   , bodyTbl = []
                   , footTbl = Nothing
                   , colsTbl = 0
                   , styleTbl = T.empty
                   }

class TRow t where
  trow :: (->) t TableRow
  trow = const []

instance (TShow x) => TRow [x] where
  trow xs = map tshow xs

instance (TShow a,TShow b) => TRow (a,b) where
  trow (a,b) = [tshow a,tshow b]

instance (TShow a,TShow b,TShow c) => TRow (a,b,c) where
  trow (a,b,c) = [tshow a,tshow b,tshow c]

instance (TShow a,TShow b,TShow c,TShow d) => TRow (a,b,c,d) where
  trow (a,b,c,d) = [tshow a,tshow b,tshow c,tshow d]

instance (TShow a,TShow b,TShow c,TShow d,TShow e) => TRow (a,b,c,d,e) where
  trow (a,b,c,d,e) = [tshow a,tshow b,tshow c,tshow d,tshow e]

instance (TShow a,TShow b,TShow c,TShow d,TShow e,TShow f) => TRow (a,b,c,d,e,f) where
  trow (a,b,c,d,e,f) = [tshow a,tshow b,tshow c,tshow d,tshow e,tshow f]

instance (TShow a,TShow b,TShow c,TShow d,TShow e,TShow f,TShow g) => TRow (a,b,c,d,e,f,g) where
  trow (a,b,c,d,e,f,g) = [tshow a,tshow b,tshow c,tshow d,tshow e,tshow f,tshow g]

instance (TShow a,TShow b,TShow c,TShow d,TShow e,TShow f,TShow g,TShow h) => TRow (a,b,c,d,e,f,g,h) where
  trow (a,b,c,d,e,f,g,h) = [tshow a,tshow b,tshow c,tshow d,tshow e,tshow f,tshow g,tshow h]

instance (TShow a,TShow b,TShow c,TShow d,TShow e,TShow f,TShow g,TShow h,TShow i) => TRow (a,b,c,d,e,f,g,h,i) where
  trow (a,b,c,d,e,f,g,h,i) = [tshow a,tshow b,tshow c,tshow d,tshow e,tshow f,tshow g,tshow h,tshow i]

instance (TShow a,TShow b,TShow c,TShow d,TShow e,TShow f,TShow g,TShow h,TShow i,TShow j) => TRow (a,b,c,d,e,f,g,h,i,j) where
  trow (a,b,c,d,e,f,g,h,i,j) = [tshow a,tshow b,tshow c,tshow d,tshow e,tshow f,tshow g,tshow h,tshow i,tshow j]

atomSh :: STM a -> Sh a
atomSh = liftIO . atomically

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO var = atomically . putTMVar var

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar

readTVarSh :: TVar a -> Sh a
readTVarSh = atomSh . readTVar

writeTVarSh :: TVar a -> a -> Sh ()
writeTVarSh t = atomSh . writeTVar t

ignore :: Applicative f => f ()
ignore = pure ()

tShow :: Show a => a -> T.Text
tShow a = T.pack $ show a

just :: (Applicative m) => Maybe a -> (a -> m b) -> m ()
just (Just a) action = action a *> ignore
just Nothing _ = ignore

maybeM :: (Applicative m) => m b -> (a -> m b) -> Maybe a -> m b
maybeM _             action (Just a) = action a
maybeM defaultAction _       Nothing = defaultAction

whenNothing :: (Applicative m) => Maybe a -> m b -> m ()
whenNothing Nothing action = void action
whenNothing _ _ = ignore

whenJust :: (Applicative m) => Maybe a -> m b -> m ()
whenJust (Just _) action = action *> ignore
whenJust Nothing _ = ignore

mMempty :: Monoid m => Maybe m -> m
mMempty = fromMaybe mempty

mMemptyM :: (Monoid n, Monad m) => m (Maybe n) -> m n
mMemptyM = (>>= pure . mMempty)

(?) :: Bool -> a -> a -> a
(?) True t _ = t
(?) False _ f = f

infixl 1 ?

data XmppPK = XmppPK { sessX         :: Session
                     , toX           :: Jid
                     , fromResourceX :: T.Text
                     }

data ReportMsg = ReportMsg { rmHandle    :: Handle
                           , rmMsg       :: T.Text
                           , rmStyle     :: T.Text
                           , rmPrefix    :: T.Text
                           , rmAltPrefix :: T.Text
                           , rmNl        :: Bool
                           , rmCmd       :: T.Text
                           , rmMessage   :: Maybe Message
                           } deriving (Show)

reportMsg = ReportMsg { rmHandle    = stdout
                      , rmMsg       = T.empty
                      , rmStyle     = T.empty
                      , rmPrefix    = T.empty
                      , rmAltPrefix = T.empty
                      , rmNl        = True
                      , rmCmd       = T.empty
                      , rmMessage   = Nothing
                      }

{-# NOINLINE xmppPKTVar #-}
xmppPKTVar :: TVar (Maybe XmppPK)
xmppPKTVar = unsafePerformIO (newTVarIO Nothing)

{-# NOINLINE xmppPKTVarCom #-}
xmppPKTVarCom :: TVar (Maybe XmppPK)
xmppPKTVarCom = unsafePerformIO (newTVarIO Nothing)

{-# NOINLINE commandTChan #-}
commandTChan :: TChan ReportMsg
commandTChan = unsafePerformIO (newBroadcastTChanIO)

{-# NOINLINE chargeTChan #-}
chargeTChan :: TChan ReportMsg
chargeTChan = unsafePerformIO (newBroadcastTChanIO)

out_chans :: [T.Text -> IO ()] -> T.Text -> IO ()
out_chans [] _ = ignore
out_chans fs txt = mapM_ ($ txt) fs

chanOutStdout :: TChan ReportMsg -> Sh ()
chanOutStdout tchan = forever $ do
  ReportMsg {..} <- atomSh $ readTChan tchan
  (rmNl ? echo $ echo_n) $ T.append rmPrefix rmMsg
