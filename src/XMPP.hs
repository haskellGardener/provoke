 {-# LANGUAGE OverloadedStrings
            , RecordWildCards
            , DoAndIfThenElse
            , TemplateHaskell
            , QuasiQuotes
            , UndecidableInstances
  #-}

{-|

Module      : XMPP
Copyright   : (c) Robert Lee 2015
License     : All Rights Reserved

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

XMPP module for provoke

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

module XMPP
where

-- Local Imports

import Lading

-- Explicit Imports

import           Control.Concurrent.STM (atomically, readTChan, TChan)
import           Control.Monad          (void, join, forever, forM_, unless)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Version           (showVersion)
import           Data.Word              (Word8)
import           Data.XML.Types         (Element)
import           Network.HostName       (getHostName)
import           Network.Xmpp
import           Network.Xmpp.IM        (simpleIM)
import           Network.Xmpp.Lens      (set)
import           Paths_provoke          (version)
import           System.IO              (stderr)
import           System.Info            (os, arch, compilerName, compilerVersion)
import           Text.Shakespeare.Text  (lt)

-- Qualified Imports

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import qualified Text.XML as TX

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------


{- TODO
   Add a version of transmit that can break long lines into multiple transmits with greater indentation, or some such.
-}

chanOutXMPP :: XmppPK -> TChan ReportMsg -> IO (Either XmppFailure ())
chanOutXMPP xmppPK tchan = forever $ do
  ReportMsg {..} <- atomically $ readTChan tchan
  case rmMessage of
    Nothing -> transmit xmppPK $ T.null rmCmd ? rmMsg $ T.concat [rmCmd, "\n", rmMsg]
    Just mess -> rawTran xmppPK mess

rawTran :: XmppPK -> Message -> IO (Either XmppFailure ())
rawTran XmppPK {..} mess_pp = sendMessage (mess_pp { messageTo = Just toX }) sessX

transmit :: XmppPK -> T.Text -> IO (Either XmppFailure ())
transmit XmppPK {..} txt = sendMessage (set messageTypeL Chat $ simpleIM toX txt) sessX

warnDisco :: XmppPK -> T.Text -> IO (Either XmppFailure ())
warnDisco xmppPK@XmppPK {..} txt = transmit xmppPK (T.concat ["▲ ▲ ▲ ", txt, " ▲ ▲ ▲"])

disconnect :: XmppPK -> IO ()
disconnect XmppPK {..} = sendPresence presenceOffline sessX *> endSession sessX

getChat :: XmppPK -> IO Message
getChat XmppPK {..} = waitForMessage mF sessX
  where mF Message {..} = messageType == Chat && case messageFrom of
                                                   Just fromJid -> fromJid == toX -- Is fromJid the Jid we've been chatting with?
                                                   Nothing -> False               -- This is not the Jid you're looking for.

startXMPP :: Jid -> T.Text -> IO (Either XmppFailure (Maybe XmppPK))
startXMPP toJ fromResource = do
  -- TODO: report any existing resources in the header comments sent to the user.
  eSess <- session realm (simpleAuthRes user pass fromResource) def
  case eSess of
    Left xmppfail -> pure $ Left xmppfail
    Right sess -> do
      let xmppPK = XmppPK { toX = toJ
                          , sessX = sess
                          , fromResourceX = fromResource
                          }
      ret <- sendPresence presenceOnline sess
      found <- waitForPresence presenceP sess

      if (presenceType found /= Available)
      then do
        TIO.hPutStrLn stderr $ T.append (jidToText toJ) " is NOT avalable\n\n"
        disconnect xmppPK
        pure $ Right Nothing
      else do
        hnm <- getHostName
        jidMsg <- getJid sess >>= pure . T.append "✧ JID: " . maybe T.empty jidToText
        let hostMsg = T.intercalate " " $ map T.pack ["✧ Hostname:", hnm]

        void $ transmit xmppPK "▼ ▼ ▼ provoke XMPP -- session start ▼ ▼ ▼"      -- Send header comments
            >> transmit xmppPK provokeMsg
            >> transmit xmppPK infoMsg
            >> transmit xmppPK hostMsg
            >> transmit xmppPK (T.append jidMsg "\n")
        pure . Right $ Just xmppPK

  where
    presenceP :: Presence -> Bool
    presenceP Presence{..} = case presenceFrom of
                               Just toJ -> True
                               _ -> False

    provokeMsg = T.intercalate " " $ map T.pack ["✧ provoke version:", showVersion version]
    infoMsg = T.intercalate " " $ map T.pack ["✧ System.Info:", os, arch, compilerName, showVersion compilerVersion]
    simpleAuthRes uname pwd resource = Just (\cstate -> [ scramSha1 uname Nothing pwd
                                                        , digestMd5 uname Nothing pwd
                                                        ] ++ if (cstate == Secured)
                                                             then [plain uname Nothing pwd]
                                                             else []
                                            , Just resource
                                            )
    realm = "chicago.vc" -- TODO: move credentials to config file or ask on command line                                                             -- ⚠
    user = "provoke"
    pass = "qBYt0Q5o"

styleMsg :: T.Text -> T.Text -> Message -- TODO: This should come with a real alt prefix for XHTML poor clients
styleMsg style alt@txt = message { messageType    = Chat
                                 , messagePayload = [ xhtml, plain ]
                                 }
  where
    plain, xhtml :: Element
    plain = ltToElement [lt|<body>#{escaper alt}</body>|]
    xhtml = ltToElement [lt|<html xmlns="http://jabber.org/protocol/xhtml-im">
                              <body xmlns="http://www.w3.org/1999/xhtml">
                                <span style="#{style}">#{lineBr txt}</span>
                              </body>
                            </html>
                        |]

    lineBr = T.intercalate "<br/>" . T.lines . escaper

tableMsg :: T.Text -> T.Text -> Table -> Message -- TODO: This should come with a real alt prefix for XHTML poor clients
tableMsg style plain table = message { messageType    = Chat
                                     , messagePayload = [ xhtml, plainE ]
                                     }
  where
    plainE, xhtml :: Element
    plainE = ltToElement [lt|<body>#{escaper plain}</body>|]
    xhtml = ltToElement [lt|<html xmlns="http://jabber.org/protocol/xhtml-im">
                              <body xmlns="http://www.w3.org/1999/xhtml">
                                <table>
                                  <thead/>
                                  <tbody/>
                                  <tfooter/>
                                </table>
                              </body>
                            </html>
                        |]

escaper :: T.Text -> T.Text
escaper txt_pp = T.concatMap f txt_pp
  where f '<' = "&lt;"
        f '>' = "&gt;"
        f '/' = "&#x2F;"
        f '&' = "&amp;"
        f x = T.singleton x

ltToElement :: LT.Text -> Element
ltToElement = TX.toXMLElement . TX.documentRoot . TX.parseText_ psett
  where psett = def { TX.psDecodeEntities = TX.decodeHtmlEntities
                    , TX.psRetainNamespaces = True
                    }

{-
data Node
  = NodeElement Element
  | NodeInstruction Instruction
  | NodeContent Content
  | NodeComment Text
    deriving (Data, Eq, Ord, Show, Typeable)

data Element = Element
  { elementName :: Name
  , elementAttributes :: [(Name, [Content])]
  , elementNodes :: [Node]
  } deriving (Data, Eq, Ord, Show, Typeable)

data Content
  = ContentText Text
  | ContentEntity Text -- ^ For pass-through parsing
    deriving (Data, Eq, Ord, Show, Typeable)

data Message = Message { messageID      :: !(Maybe Text)
                       , messageFrom    :: !(Maybe Jid)
                       , messageTo      :: !(Maybe Jid)
                       , messageLangTag :: !(Maybe LangTag)
                       , messageType    :: !MessageType
                       , messagePayload :: ![Element]
                       , messageAttributes :: ![ExtendedAttribute]
                       } deriving (Eq, Show)

message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  , messageAttributes = []
                  }
sendMessage :: Message -> Session -> IO (Either XmppFailure ())
-}
