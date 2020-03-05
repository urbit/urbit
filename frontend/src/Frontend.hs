{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Prelude
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Urbit.Atom
import Reflex.Dom.Widget.Input

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import System.IO.Unsafe    (unsafePerformIO)
import Urbit.Moon.Repl     (evalTextFast, evalText)

--------------------------------------------------------------------------------

slow
  :: (Monad m, MonadSample s m, Reflex s, DomBuilder s m, PostBuild s m) => m ()
slow = do
  el "h1" (text "Slow")

  val <-
    fmap _inputElement_value
    $  inputElement
    $  (def & inputElementConfig_initialValue .~ "(K K K)")

  el "pre" $ dynText $ unsafePerformIO . evalText <$> val


fast
  :: (Monad m, MonadSample s m, Reflex s, DomBuilder s m, PostBuild s m) => m ()
fast = do
  el "h1" (text "Fast")

  val <-
    fmap _inputElement_value
    $  inputElement
    $  (def & inputElementConfig_initialValue .~ "(K K K)")

  el "pre" $ dynText $ unsafePerformIO . evalTextFast <$> val


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
                       el "title" $ text "Obelisk Minimal Example"
                       elAttr
                         "link"
                         (  "href"
                         =: static @"main.css"
                         <> "type"
                         =: "text/css"
                         <> "rel"
                         =: "stylesheet"
                         )
                         blank
  , _frontend_body = do

    fast

    slow

    return ()
  }
