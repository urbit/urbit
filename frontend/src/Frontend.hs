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

import qualified Urbit.Uruk.Refr.Raw as Ur

import System.IO.Unsafe (unsafePerformIO)
import Urbit.Moon.Repl  (evalTextFast)

--------------------------------------------------------------------------------

{-
  Holy shit this is ugly!
-}

evalMoon :: T.Text -> T.Text
evalMoon = unsafePerformIO . evalTextFast

replThing :: (Monad m, MonadSample s m, Reflex s, DomBuilder s m, PostBuild s m) => m ()
replThing = do
    let k = Ur.N Ur.K

    el "h3" $ text (T.pack $ show $ Ur.eval (k Ur.:@ k Ur.:@ k))

    inp <- inputElement (def & inputElementConfig_initialValue .~ "(K K K)" )

    let val = inp & _inputElement_value

    el "p" (el "b" (dynText val))

    el "pre" $ dynText (evalMoon <$> val)


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

    el "h1" $ text "Welcome to Obelisk!"

    el "h3" $ text (T.pack $ show $ utf8Atom "Welcome to Obelisk!")

    el "p" $ text $ T.pack commonStuff

    replThing

    -- `prerender` and `prerender_` let you choose a widget to run on the server
    -- during prerendering and a different widget to run on the client with
    -- JavaScript. The following will generate a `blank` widget on the server and
    -- print "Holla, World!" on the client.
    prerender_ blank $ liftJSM $ void $ eval
      ("console.log('Holla, World! Get fucked nigga.')" :: T.Text)

    elAttr "img" ("src" =: static @"obelisk.jpg") blank

    el "div" $ do
      exampleConfig <- getConfig "common/example"
      case exampleConfig of
        Nothing -> text "No config file found in config/common/example"
        Just s  -> text $ T.decodeUtf8 s

    return ()
  }
