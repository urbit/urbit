{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Frontend where

import ClassyPrelude            hiding (exp)
import Common.Api
import Common.Route
import Control.Monad
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Dom.Widget.Input
import Reflex.Host.Class
import Urbit.Atom

import Control.Monad.Fix           (MonadFix)
import Language.Javascript.JSaddle (eval, liftJSM)
import Prelude                     ()
import System.IO.Unsafe            (unsafePerformIO)
import Urbit.Moon.Repl             (evalText, evalText')
import Urbit.Uruk.UrukDemo         (Env, EvalResult(..), Exp, Inp(..),
                                    InpResult(..))
import Urbit.Uruk.UrukDemo         (execInp, execText, parseInps,
                                    prettyInpResult)
import Urbit.UrukRTS               (dumpEventsFile, toJSON, vProfDone)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Urbit.Moon.MoonToUruk as MU
import qualified Urbit.UrukRTS.Types   as RTS

--------------------------------------------------------------------------------

thinking :: Text
thinking = "Thinking..."

evalTextFast = evalText' (pure . MU.strictOlegFile' (id :: RTS.Val -> RTS.Val))


fastResult
  :: ( TriggerEvent t m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Dynamic t Text
  -> m (Dynamic t Text)
fastResult txt = do
  (e, f) <- newTriggerEvent
  performEvent_ $ fmap (compute fastBrain thinking f evalTextFast) $ updated txt
  res <- holdDyn thinking e
  pure res

slowResult
  :: ( TriggerEvent t m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Dynamic t Text
  -> m (Dynamic t Text)
slowResult txt = do
  (e, f) <- newTriggerEvent
  performEvent_ $ fmap (compute slowBrain thinking f evalText) $ updated txt
  res <- holdDyn thinking e
  pure res

slowResultWContext
  :: ( TriggerEvent t m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => [Text]
  -> Dynamic t Text
  -> m (Dynamic t Text)
slowResultWContext context txt = do
  (e, f) <- newTriggerEvent
  performEvent_ $
    fmap ((compute slowBrain thinking f evalText) . unlines . (snoc context)) $
    updated txt
  res <- holdDyn thinking e
  pure res

urukResult
  :: ( TriggerEvent t m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Dynamic t (Env, Text)
  -> m (Dynamic t (Either Text (Env, [InpResult])))
urukResult envTxt = do
  (e, f) <- newTriggerEvent
  let think = Left thinking

  let goInp :: (Env, Text) -> Either Text (Env, [InpResult])
      goInp (env, txt) = parseInps txt >>= inpSeq env

  performEvent_ (compute urukBrain think f (pure . goInp) <$> updated envTxt)

  holdDyn think e


urukResultTrace
  :: ( TriggerEvent t m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Dynamic t (Text)
  -> m (Dynamic t (Either Text (Env, [InpResult])))
urukResultTrace envTxt = do
  (e, f) <- newTriggerEvent
  let think = Left thinking

  -- This part of the demo can't use the fast RTS: the point of these demo
  -- parts are that we show the the individual reduction steps to the user.
  let goInp :: Text -> Either Text (Env, [InpResult])
      goInp txt = parseInps txt >>= inpSeq (mempty :: Env)

  performEvent_ (compute urukBrain think f (pure . goInp) <$> updated envTxt)

  holdDyn think e


inpSeq :: Env -> [Inp] -> Either Text (Env, [InpResult])
inpSeq initEnv = go initEnv []
 where
  go env acc []     = pure (env, reverse acc)
  go env acc (x:xs) = do
    (env', r) <- execInp env x
    go env' (r:acc) xs

fastBrain :: MVar (Maybe (Async ()))
fastBrain = unsafePerformIO (newMVar Nothing)

slowBrain :: MVar (Maybe (Async ()))
slowBrain = unsafePerformIO (newMVar Nothing)

urukBrain :: MVar (Maybe (Async ()))
urukBrain = unsafePerformIO (newMVar Nothing)

compute
  :: MonadIO m
  => MVar (Maybe (Async ())) --  thread to kill
  -> b                       --  Value while executing
  -> (b -> IO ())            --  Callback
  -> (a -> IO b)             --  Action
  -> a                       --  Argument
  -> m ()
compute ref thinkVal cb exec txt = do
  takeMVar ref >>= maybe (pure ()) cancel
  liftIO (cb thinkVal)
  tid <- liftIO $ async $ (exec txt >>= liftIO . cb)
  putMVar ref (Just tid)

slow
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     )
  => m ()
slow = do
  el "h3" (text "Slow")

  valu <-
    fmap _inputElement_value
    $  inputElement
    $  (def & inputElementConfig_initialValue .~ "(K K K)")

  resu <- slowResult valu

  el "pre" (dynText resu)


showWipeW :: (Reflex s, DomBuilder s m) => Text -> m ()
showWipeW = el "pre" . text . showWipe

showWipe :: Text -> Text
showWipe = ("!" <>)

inpInputW :: (Reflex s, DomBuilder s m, Monad m) => InpResult -> m ()
inpInputW res = do
  el "h4" (text "Input")
  case res of
    InpWipe v     -> el "pre" (text ("!" <> v))
    InpExpr e _   -> el "pre" (text $ tshow e)
    InpDecl v e _ -> el "pre" (text $ "=" <> v <> " " <> tshow e)

showExpW :: (Monad m, Reflex t, DomBuilder t m) => Exp -> m ()
showExpW = el "pre" . text . tshow

showDeclW :: (Monad m, Reflex t, DomBuilder t m) => Text -> Exp -> m ()
showDeclW nm exp = el "pre" $ text $ showDecl nm exp

showDecl :: Text -> Exp -> Text
showDecl nm exp = "=" <> nm <> " " <> tshow exp

inpResultEvalResult :: InpResult -> Maybe EvalResult
inpResultEvalResult = \case
  InpWipe _     -> Nothing
  InpExpr _ r   -> Just r
  InpDecl _ _ r -> Just r

inpResultW :: (Reflex s, DomBuilder s m, Monad m) => InpResult -> m ()
inpResultW = \case
  InpWipe _                    -> pure ()
  InpExpr _ (EvalResult x _)   -> hdr >> showExpW x
  InpDecl v _ (EvalResult x _) -> hdr >> showDeclW v x
 where
  hdr = el "h4" (text "Result")

showTrace :: (Monad m, Reflex t, DomBuilder t m) => [Exp] -> m ()
showTrace = el "pre" . text . intercalate "\n" . fmap tshow

showDeclTrace :: (Monad m, Reflex t, DomBuilder t m) => Text -> [Exp] -> m ()
showDeclTrace v = el "pre" . text . intercalate "\n" . fmap declShow
 where
  declShow exp = "=" <> v <> " " <> tshow exp

inpTraceW :: (Reflex s, DomBuilder s m, Monad m) => InpResult -> m ()
inpTraceW = \case
  InpWipe _                    -> pure ()
  InpExpr _ (EvalResult _ t)   -> hdr >> showTrace (tal $ reverse t)
  InpDecl v _ (EvalResult _ t) -> hdr >> showDeclTrace v (tal $ reverse t)
 where
  hdr = pure () -- el "h4"  (text "Reductions")

  tal []       = []
  tal (_ : xs) = xs

inpTraceDownwards :: (Reflex s, DomBuilder s m, Monad m) => InpResult -> m ()
inpTraceDownwards = \case
  InpWipe _                    -> pure ()
  InpExpr _ (EvalResult _ t)   -> hdr >> showTrace (tal t)
  InpDecl v _ (EvalResult _ t) -> hdr >> showDeclTrace v (tal t)
 where
  hdr = pure () -- el "h4"  (text "Reductions")

  tal []       = []
  tal (_ : xs) = xs


resultPreview
  :: (Monad m, Reflex t, DomBuilder t m) => Either Text [InpResult] -> m ()
resultPreview eRes = do
  case eRes of
    Left err -> do
      elAttr "p" ("class" =: "noresult") $ do
        el "pre" (text "ERROR")
      elAttr "p" ("class" =: "error") $ do
        el "pre" (text err)
    Right results -> do
      elAttr "p" ("class" =: "result") $ do
        for_ results $ \case
          InpWipe v                    -> showWipeW v
          InpExpr _ (EvalResult x _)   -> showExpW x
          InpDecl v _ (EvalResult x _) -> showDeclW v x
      elAttr "p" ("class" =: "trace") $ do
        traverse_ inpTraceW results

fullReductionTrace
  :: (Monad m, Reflex t, DomBuilder t m) => Either Text [InpResult] -> m ()
fullReductionTrace eRes = do
  case eRes of
    Left err -> do
      elAttr "p" ("class" =: "noresult") $ do
        el "pre" (text "ERROR")
      elAttr "p" ("class" =: "error") $ do
        el "pre" (text err)
    Right results -> do
      elAttr "p" ("class" =: "trace") $ do
        -- TODO: Insert the original uruk statement here.
        traverse_ inpTraceDownwards results


showHistory :: [InpResult] -> Text
showHistory = unlines . fmap doit
 where
  doit :: InpResult -> Text
  doit = \case
    InpWipe v                    -> showWipe v
    InpExpr _ (EvalResult x _)   -> tshow x
    InpDecl v _ (EvalResult x _) -> showDecl v x

inpEnvW :: (Monad m, Reflex t, DomBuilder t m) => Env -> m ()
inpEnvW env = do
  unless (null env) $ do
    el "h3" (text "Environment")
    el "pre" $ text $ intercalate "\n" $ mapToList env <&> \(k,v) ->
      "=" <> k <> " " <> tshow v

prettyInpResultW
  :: ( Monad m
     , Reflex t
     , DomBuilder t m
     , MonadSample t m
     , MonadHold t m
     , PostBuild t m
     )
  => (Env, [InpResult])
  -> m ()
prettyInpResultW (env, results) = do
  el "h3" (text "Execution Results")
  for_ results $ \res -> do
    inpInputW res
    inpResultW res
    inpTraceW res
  inpEnvW env

prettyInpWaiting :: (Monad m, Reflex t, DomBuilder t m) => m ()
prettyInpWaiting = do
  el "h3" (text "Execution Results")
  el "pre" (text "Waiting for input")


urdocSection :: (Monad m, Reflex t, DomBuilder t m)
             => Text -> Text -> m () -> m ()
urdocSection sectionName id content = do
  elAttr "div" ("class" =: "wrap-collapsible") $ do
    elAttr "input" ("id" =: id <>
                    "class" =: "toggle" <>
                    "type" =: "checkbox") (pure ())
    elAttr "label" ("for" =: id <>
                    "class" =: "label-toggle") (text sectionName)
    elAttr "div" ("class" =: "collapsible-content") $ do
      content

urukW
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     , MonadFix m
     )
  => m ()
urukW = do
  elAttr "div" ("class" =: "demo") $ mdo
    void $ widgetHold (pure ()) (prettyHistoryW <$> updated history)

    val <-
      fmap _inputElement_value
      $  inputElement
      $  (def & inputElementConfig_initialValue .~ "(K K K)")

    envD  <- holdDyn (mempty :: Env) evEnvUpdate
    resD  <- urukResult (zipDyn envD val)
    press <- button "Execute"

    void $ widgetHold (pure ()) (resultPreview <$> updated (fmap snd <$> resD))

    let execRes     = onlyRights (current resD <@ press)
        evEnvUpdate = fst <$> execRes

    history <- foldDyn (flip snoc) [] (snd <$> execRes)

    when False $ do
      void $ widgetHold prettyInpWaiting (prettyInpResultW <$> execRes)

 where
  onlyRights :: Reflex t => Event t (Either a b) -> Event t b
  onlyRights = fmapMaybe $ \case
    Left  _ -> Nothing
    Right x -> Just x

prettyHistoryW :: (Reflex t, DomBuilder t m) => [[InpResult]] -> m ()
prettyHistoryW (mconcat -> results) = do
  el "pre" $ do
    text (showHistory results)
  -- for_ results $ \case
    -- InpWipe v                    -> showWipeW v
    -- InpExpr _ (EvalResult x _)   -> showExpW x
    -- InpDecl v _ (EvalResult x _) -> showDeclW v x

-- showHistory :: [InpResult] -> Text
-- showHistory = unlines . fmap doit
 -- where
  -- doit :: InpResult -> Text
  -- doit = \case
    -- InpWipe v                    -> showWipe v
    -- InpExpr _ (EvalResult x _)   -> tshow x
    -- InpDecl v _ (EvalResult x _) -> showDecl v x

fast
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     )
  => m ()
fast = do
  el "h3" (text "Fast")
  valu <-
    fmap _inputElement_value
    $  inputElement
    $  (def & inputElementConfig_initialValue .~ "(K K K)")
  res <- fastResult valu
  el "pre" $ dynText res


-- The reduce demo takes an initial input and then builds the reduction trace
reduceDemo
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     , MonadFix m
     )
  => Text -> m ()
reduceDemo input = do
  elAttr "div" ("class" =: "demo") $ mdo
    el "h5" (text $ "TODO: " ++ input)
    val <-
      fmap _inputElement_value
      $  inputElement
      $  (def & inputElementConfig_initialValue .~ input)

    -- TODO: I tried to build a constant Dyn, but the next line seems to reduce
    -- to just mempty? The type is just Env? Copy urukResult for now.
    --
    -- Given what I want to do, this is also just not what we really want: we
    -- want a dead simple input a value, click a button which replaces another
    -- value.

--    envD  <- constDyn (mempty :: Env)
    resD  <- urukResultTrace val --(zipDyn envD val)

    -- TODO: If we stuff the initial input into the input element and live
    -- update it, we don't really want an Execute button, but a Reset one.
    press <- button "Execute"

    void $ widgetHold (pure ())
      (fullReductionTrace <$> updated (fmap snd <$> resD))


compileDemo
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     , MonadFix m
     )
  => [Text] -> [Text] -> m ()
compileDemo context inputLines = do
  elAttr "div" ("class" =: "demo") $ mdo
    el "pre" $ for_ context $ \t -> do
      el "div" (text t)

    for_ inputLines $ \l -> do
      el "h5" $ el "pre" $ (text $ "TODO: " ++ l)

    -- TODO: Figure out how to get inputLines set as the initial value.
    val <-
      fmap _textAreaElement_value
      $  textAreaElement
      $  (def & textAreaElementConfig_initialValue .~ (unlines inputLines))

    -- TODO: move to fastResult when jet matching works on the fast
    -- interpreter.
    res <- slowResultWContext context val
    el "pre" $ dynText res

storageDemo
  :: ( Monad m
     , MonadSample s m
     , Reflex s
     , DomBuilder s m
     , PostBuild s m
     , PerformEvent s m
     , MonadHold s m
     , TriggerEvent s m
     , MonadIO (Performable m)
     , MonadFix m
     )
  => [Text] -> m ()
storageDemo inputLines = do
  let input = unlines inputLines

  elAttr "div" ("class" =: "demo") $ mdo
    el "h5" (text $ "TODO: " ++ input)

    val <-
      fmap _textAreaElement_value
      $  textAreaElement
      $  (def & textAreaElementConfig_initialValue .~ input)

    el "pre" $ text "TODO: Table of hash to serialized SKEW value"




paragraph :: (DomBuilder t m) => [Text] -> m ()
paragraph x = el "p" (text $ unlines x)

--ul :: (DomBuilder t m) => m () -> m ()
ul x = el "ul" x

li :: (DomBuilder t m) => [Text] -> m ()
li x = el "li" (text $ unlines x)



-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
                       el "title" $ text "Uruk Demo"
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

    el "h1" $ do
      text "SKEW"

    paragraph [
      "SKEW is an extension of the SK Calculus with the ideas developed for ",
      "Nock. This demo is designed to teach you what SKEW is, why it should ",
      "become the successor to Nock 4K+, why it's faster, and to allow you ",
      "to play with it in your browser. In each section, the code in text ",
      "boxes is live and you can edit it to play with it."
      ]

    el "p" $ do
      text "The "

      elAttr
        "a"
        ("href" =: "https://en.wikipedia.org/wiki/SKI_combinator_calculus")
        (text "SK Calculus")

      text $ unlines [
        " is an encoding of the Lambda Calculus introduced by Schönfinkel ",
        "and Curry in the 1920s. It is Turing complete, simple, and well ",
        "understood. We believe it is the best foundation to build on due ",
        "to how well researched it is."
        ]


    urdocSection "The Basic Reduction Rules" "basic-reduction" $ do
      paragraph [
        "What's the smallest practical combinator? Let's first list our ",
        "functional requirements:"
        ]

      el "ul" $ do
        el "li" $ do
          text "We need to be able to perform any computable computation."
        el "li" $ do
          text $ unlines [
            "We need to be able to recognize functions and values and ",
            "replace them with optimized versions to make runtime practical."
            ]
        el "li" $ do
          text "We need to be able to virtualize computations."

      paragraph [
        "Let's go over proposed reduction rules one by one, and relate them ",
        "to our requirements."
        ]

      elAttr "pre" ("class" =: "docs") $ do
        text $ unlines
          [ "Reduction Rules:"
          , "    *(K x y)             -> x"
          , "    *(x y)               -> (*x y)"
          , "    *(x y)               -> (x *y)"
          , "    *(S x y z)           -> (x z (y z))"
          , "    *(E^n t f x1 … xn)   -> (f x1 … xn)"
          , "    *(W a s k e w (x y)) -> (a x y)"
          , "    *(W a s k e w S)     -> s"
          , "    *(W a s k e w K)     -> k"
          , "    *(W a s k e w E)     -> e"
          , "    *(W a s k e w W)     -> w"
          ]

      paragraph [
        "The first four rules give us the SK calculus with a strict, well ",
        "defined reduction ordering. The first reduction is the ",
        "K combinator. The K combinator returns its first argument and ",
        "discards its second. Mnemonic: Konstant."
        ]

      reduceDemo "(K K K)"

      paragraph [
        "Simple enough. The next two reduction rules specify order of ",
        "operations: we reduce the left hand side before the right hand side. ",
        "We must do this for short circuiting reasons. SKEW is strict instead ",
        "of lazy and if we did not do this, both arguments to K would be ",
        "evaluated before the first is returned."
        ]

      reduceDemo "(K K (S K K))"

      paragraph [
        "After that, we have the S combinator. The S combinator substitutes ",
        "its arguments, returning its first, third, and then calling its ",
        "second with its third. Mnemonic: Substitution."
        ]

      reduceDemo "(S K (S K) (S K K))"

      paragraph [
        "That's it for the core of the SK system we have in SKEW. With S and ",
        "K, we can encode the entire unenriched lambda calculus."
        ]

      paragraph [
        "Our next requirement is making functions in code legible to the ",
        "interpreter. Nock's big idea ",
        "is the Jet: formally specify every function or piece of code in ",
        "your pure-functional system without any side-effects or FFI and ",
        "signal to the interpreter that a piece of set of raw combinator ",
        "reductions can be replaced with a native, optimized implementation."
        ]

      paragraph [
        "The E combinator specifies a jet and specifies order of execution. ",
        "The jet is a number of E letters ",
        "which specify the arity of the function, a tag which is only used ",
        "for matching, the function being jetted, and then the number of ",
        "arguments according to the stated arity. The arguments are ",
        "evaluated first. Mnemonic: Enhance"
        ]

      paragraph [
        "Look at the order in which the arguments are reduced in this trace:"
        ]

      reduceDemo "(E E K (S K) (S K K K) (S K K K))"

      paragraph [
        "Since the raw function specified in the jet is supposed to be ",
        "replacable with an optimized implementation, we want to prevent ",
        "the partial evaluation of that function with only some of its ",
        "arguments. This means we don't have to deal with keeping track of ",
        "whether a partially evaluated closure refers to a jet or with ",
        "allocating closures in the common case."
        ]

      paragraph [
        "In Nock 4K, the hint tag annotates the return value of an expression,",
        " so if an expression returns a function, the interpreter must ",
        "remember an extra bit of matching information. In SKEW, there's no ",
        "need to do that because the jet matching happens at reduction time, ",
        "allowing for both faster jet matching, and ahead of time ",
        "optimizations."
        ]

      el "p" $ do
        text $ unlines [
          "Our final requirement is being able to virtualize computations: we ",
          "must have a way of running a function which we don't know if it ",
          "will error by infinite looping in predictable ways. We thus need ",
          "to be able to declare a +mock function with the signature "
          ]

        el "code" (text "a -> Either err a")

        text $ unlines [
          ". SKEW supports reflection with the W combinator, which is the ",
          "final, five reduction rules. For (W a s k e w x), the W combinator ",
          "will switch on x. Mnemonic: sWitch."
          ]

      reduceDemo "(W 0 1 2 3 4 K)"

      paragraph [
        "sWitch gives us generalized introspection on SKEW code, but we ",
        "expect that the only practical use will be in the specification ",
        "of virtualizing computation."
        ]

    ---------------------------------------------------------------------------

    urdocSection "Jet Matching Data Types" "jet-data" $ do
      paragraph [
        "In SKEW, everything is made up of the unenriched lambda calculus, ",
        "including ",
        "numbers which are Church numerals, which are functions. ",
        "Traditionally, this has not been ",
        "seen as practical: most system enrich the lambda calculus with cons ",
        "cells, strings, numbers, etc. Nock 4K baked in natural numbers and ",
        "cons cells."
        ]

      paragraph [
        "But jets give us a way to recognize any function, including classes ",
        "of functions such as the functions for Church numerals, and this ",
        "allows a SKEW interpreter to store a natural number in memory ",
        "instead of the raw series of S and Ks which represent a number."
        ]

      paragraph [
        "A Church numeral takes two functions as arguments, what to do in ",
        "the zero case and what to do in the non-zero case so you can see ",
        "that numbers have to start with (E E ...).",
        "For example, if (S K) is the encoding of 0 as a Church numeral, then ",
        "0 as a jet recognized natural number is:"
        ]

      reduceDemo "(E E K (S K))"

      paragraph [
        "And in turn, 1 is:"
        ]

      reduceDemo "(E E K (S K K))"

      paragraph [
        "And so forth. But because they are jet recognized, the underlying ",
        "in-memory representation is the natural number 1, and since there ",
        "is a bijection between the natural numbers and the church numerals, ",
        "we can recover the raw SKEW whenever we need it."
        ]

      paragraph [
        "And when we jet functions, such as +add, we can match on the ",
        "these internal representations, so that the jet implementation of ",
        "add is given two jetted church numerals, the jet implementation just ",
        "adds the natural representations together. And if there's a problem ",
        "for any reason, we can just fall back to executing the raw SK code."
        ]

      paragraph [
        "Let's put this together into a sandbox where we can play with ",
        "addition:"
        ]

      compileDemo [
        "=/  skzero  (S K)",
        "=/  sksucc  (S (S (K S) K))",
        "=/  zero    (E E K skzero)",
        "=/  one     (E E K (sksucc skzero))",
        "=/  two     (E E K (sksucc (sksucc skzero)))"
        ]
        [ "(add one (add one two))"
        ]

    ---------------------------------------------------------------------------

    urdocSection "Storage and Snapshot Strategy" "storage" $ do
      paragraph [
        "So we've gone over how SKEW's jetting strategy makes things ",
        "performant and how we can store jetted data instead of raw ",
        "representations in S and K. But what about larger scale data?"
        ]

      paragraph [
        "SKEW is intended to be a substrate for Urbit, which is a complete ",
        "system as a single value. Your Urbit is one closure which takes a ",
        "command and returns a pair of effects and a new closure. Everything ",
        "lives in this one closure: all system code, all user code, all data."
        ]

      paragraph [
        "How do you handle all of a user's data as one value?"
        ]

      paragraph [
        "In the current Urbit system, Vere maps a 2 gigabyte memory image ",
        "directly to disk, which is fast but limits the size of the image. ",
        "The alternative Jaque interpreter serializes the entire system state ",
        "on each save, but this is slow and costly. Neither of these ",
        "solutions scale."
        ]

      paragraph [
        "We want to be able to page parts of your system value from disk on ",
        "use, but we don't want to hash-cons each SKEW letter. We only want ",
        "to break the value up into parts at specific boundaries. We want to ",
        "specify from inside an SKEW program that a value should be ",
        "serialized separately. So we define two functions which put a value ",
        "in a box and take a value out of a box:"
        ]

      el "pre" $ el "code" $
        paragraph [
          "++  (box x)    (J %box (K x))",
          "++  (unbox x)  (x uni)"
          ]

      paragraph [
        "All we are doing is declaring a function which returns a jetted ",
        "function with a tag of %box which returns the value when called with ",
        "any argument. The ",
        "corresponding unbox function just calls the function with an unused ",
        "argument. Using the same data jet matching infrastructure from the ",
        "last section, we can data jet the value stored in this operation ",
        "separately."
        ]

      paragraph [
        "To illustrate how this works, let's make something analogous to our ",
        "current Arvo operating system: a function which takes a command, and ",
        "returns a pair of effects and a new function. To keep things simple, ",
        "let's implement a persistent, stateful Fibonacci sequence generator. ",
        "A function with type:"
        ]

      el "pre" $ el "code" $
        paragraph [
          "type Fun = Int -> (Int, Fun)"
          ]

      -- TODO: Continue here, and I would have preferred to work on this
      -- instead of what I eventually did work on.
      paragraph [
        "For this exercise, we're going to write a small program in \"moon\", ",
        "a language superficially like hoon, except that it is untyped and ",
        "compiles to SKEW. (We aren't proposing this language as a replacement ",
        "for hoon; there's a separate effort working on repairing the lack of ",
        "rigor in hoon %141.)"
        ]

      let fibLines = [
            "=/  fib",
            "  ~/  2  fib",
            "  ..  $",
            "  |=  (cache n)",
            "  ?:  (zer n)    [1 cache]",
            "  ?:  (eql n 1)  [1 cache]",
            "  ::",
            "  ?-    (find-assoc eql cache n)",
            "      val",
            "    [(unbox val) cache]",
            "  ::",
            "      nothing",
            "    =/  n-fec-fec  ($ cache (fec (fec n)))",
            "    =/  n-fec      ($ (cdr n-fec-fec) (fec n))",
            "    =/  total      (add (car n-fec-fec) (car n-fec))",
            "    ::",
            "    [total (add-assoc lth eql (cdr n-fec) n (box total))]",
            "  ==",
            "",
            "=/  arvo",
            "  ~/  2  arvo",
            "  ..  $",
            "  |=  (cache n)",
            "  =/  val  (fib cache n)",
            "  [(car val) ($ (cdr val))]",
            "",
            "(arvo lnil)"
            ]

      el "pre" $ el "code" $
        paragraph fibLines

      paragraph [
        "This defines a function named fib that has a fixed, and a . "
        ]






      paragraph [
        "Nock 4K+ has unifying equality on all nouns, meaning there wasn't a ",
        "principled place to break the noun tree up into units. The way SKEW ",
        "does jets gives us an easy way to break up the memory state: we ",
        "treat jet boundaries as points where we break the state up. We can ",
        "securely hash these values and form an entire Merkle tree."
        ]

      -- TODO: Unlike the other demos, what this should output is a set of
      -- decomposed hash references: it should contain the
      --
      storageDemo  [
        "=/  id  (J K S K K)",
        "=/  rawzer  (S K)",
        "=/  rawsuc  (S (S (K S) K))",
        "",
        "=/  pak  (J K (S (K (J J K)) (S (S id (K rawsuc)) (K rawzer))))",
        "=/  inc  (J K (S (K pak) (S (S (K S) K))))",
        "=/  add  (J J K (S (K (S (K pak))) (S (K S) (S (K (S (K S) K))))))",
        "=/  zer  (pak rawzer)",
        "=/  one  (inc zer)",
        "=/  two  (inc one)",
        "(add two)"
        ]

      paragraph [
        "[TODO] You can see above that the state has been chopped up into a ",
        "set of pieces which are refereed to by hashref. Writing a snapshot ",
        "of this value means writing the hashref/value pairs to a database ",
        "and keeping track of the hash of the toplevel."
        ]

      paragraph [
        "This also gives us a path towards dealing with piers which are larger",
        " than the physical memory on the current machine: since all pieces ",
        "are stored hash referenced in a key-value store, we can lazily page ",
        "values into memory when they are referenced and unload values we ",
        "know are on disk during memory pressure events."
        ]

    el "hr" $ pure ()

    ---------------------------------------------------------------------------

    -- TODO: Rework this section into something like, "What was Nock 4K+ trying
    -- to accomplish?"
    urdocSection "Why not Nock 4K+?" "why-not" $ do
      paragraph [
        "Nock was designed to be the virtual machine for a user-controlled ",
        "computer. Aspirationally:"
        ]

      ul $ do
        li [
          "Nock was supposed to be so small as to be trivial for anyone to ",
          "implement their own interpreter because of its simple nature and ",
          "short specification, meaning that it would be easy for there to ",
          "be a diversity of interpreters available."
          ]

        el "li" $ do
          text "Nock was supposed to be as small as possible and perfect."
          el "i" (text "\"Tiny and diamond perfect.\"")

      paragraph [
        "In practice, Nock 4K is none of those things."
        ]

      paragraph [
        "Vere is the only semi-performant Nock interpreter available. While ",
        "writing one is an interesting exercise, making it performant is ",
        "seriously difficult because of Nock's semantics. Being able to ",
        "edit any part of your state makes optimization difficult, and the ",
        "semantics where jet hints attach to the returned value of a ",
        "computation make jet matching significantly more difficult than it ",
        "has to be."
        ]

      paragraph [
        "And yet here we all are. Nock 4K is not good enough, but has an ",
        "interesting set of features and innovations which are good and which ",
        "we should keep. Mainly:"
        ]

      ul $ do
        li [
          "You should design your low level runtime so that everything is ",
          "formally specified in your core language without any FFI or other ",
          "cheating."
          ]

        li [
          "You should design your low level runtime so you can recognize ",
          "formal specifications and use optimized implementations at runtime.",
          "Jets are a legitimate innovation of Nock and have been copied by ",
          "other systems."
          ]

        li [
          "Your system is event based. Your system state is a function which ",
          "takes input and returns side effects and an updated function."
          ]

        li [
          "Your entire system state is a value and this value is portable ",
          "across interpreters."
          ]


    ---------------------------------------------------------------------------

    urdocSection "Compiling Hoonish Expressions" "hoon-expressions" $ do
      paragraph [
        "Building a new high performance functional VM is nice, but we also ",
        "want it to be a compile target for something like Hoon. So, as a ",
        "feasibility proof, lets produce a non-typed variant of Hoon and make ",
        "sure we can compile it to SKEW."
        ]

      paragraph [
        "For now, we'll skip other jetted data types we can skip and just use ",
        "jetted natural numbers for now. Let's declare a gate which always ",
        "returns the same number:"
        ]

      compileDemo [] [
        "|=(a 43)"
        ]

      paragraph [
        "For any given input, \"K 43\" will return 43, since (K x y) is x. We ",
        "can verify this by calling the gate with a different number:"
        ]

      compileDemo [] [
        "=/  gate  |=(a 43)",
        "(gate 81)"
        ]

      -- TODO: This could be a lot better of a tutorial if I wasn't a talentless
      -- hack.
      el "h4" $ (text "<dog>I have no idea what I'm doing.</dog>")


    urdocSection "The Old Demo" "the-old-demo" $ do
      el "h3" $ do
        text "Quick Reference"

      elAttr "pre" ("class" =: "docs") $ do
        text $ unlines
          [ "Command Syntax:"
          , "    EXPR     ::  Evaluate EXPR"
          , "    =x EXPR  ::  Bind `x` to result of evaluating EXPR"
          , "    !x       ::  Unbind `x`"
          , ""
          , "Syntax:"
          , "    /[SKJD]/           ->  primtive combinators S, K, J, D"
          , "    /[$a-z]+/          ->  identifier (reference to bound variable)"
          , "    (x:EXPR y:EXPR)    ->  Call `x` with argument `y`."
          , "    (x y z)            ->  ((x y) z)"
          , "    (x y z ...)        ->  ((x y) z ...)"
          , ""
          , "Reduction Rules:"
          , "    *(K x y)           -> x"
          , "    *(x y)             -> (*x y)"
          , "    *(x y)             -> (x *y)"
          , "    *(S x y z)         -> (x z (y z))"
          , "    *(D x)             -> JAM(x)"
          , "    *(J^n t f x1 … xn) -> (f x1 … xn)"
          , ""
          , "Examples:"
          , "    =id (J K S K K)"
          , ""
          , "    =rawzer (S K)"
          , "    =rawsuc (S (S (K S) K))"
          , "    =rawone (rawsuc rawzer)"
          , "    =rawtwo (rawsuc rawone)"
          , "    =rawthr (rawsuc rawtwo)"
          , "    =rawfor (rawsuc rawthr)"
          , ""
          , "    =pak (J K (S (K (J J K)) (S (S id (K rawsuc)) (K rawzer))))"
          , "    =inc (J K (S (K pak) (S (S (K S) K))))"
          , "    =add (J J K (S (K (S (K pak))) (S (K S) (S (K (S (K S) K))))))"
          , "    =zer (pak rawzer)"
          , "    =one (inc zer)"
          , ""
          , "    =cons (J J J K (S (K (S (K (S (K (S (K (S S (K K))) K)) S)) (S id))) K))"
          , "    =car (J K (S id (K K)))"
          , "    =cdr (J K (S id (K (S K))))"
          , ""
          , "    (car (cons zer (id (inc (inc (inc (zer)))))))"
          ]

      el "h2" (text "Demo")
      urukW

    when False $ do
      el "hr" $ pure ()
      el "h2" $ text "Old Stuff"
      fast
      slow

    return ()
  }
