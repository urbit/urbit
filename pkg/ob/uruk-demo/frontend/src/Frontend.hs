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
  => [Text] -> m ()
compileDemo inputLines = do
  let input = unlines inputLines

  elAttr "div" ("class" =: "demo") $ mdo
    el "h5" (text $ "TODO: " ++ input)

    val <-
      fmap _textAreaElement_value
      $  textAreaElement
      $  (def & textAreaElementConfig_initialValue .~ input)

    res <- fastResult val
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

    el "pre" $ text "TODO: Table of hash to serialized Uruk value"




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
      text "Uruk"

    paragraph [
      "Uruk is an extension of the SK Calculus with the ideas developed for ",
      "Nock. This demo is designed to teach you what Uruk is, why it should ",
      "become the next Nock, why it's faster, and to allow you to play with ",
      "it in your browser. In each section, the code in text boxes is live ",
      "and you can edit it to play with it."
      ]

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


    urdocSection "The Basic Reduction Rules" "basic-reduction" $ do
      paragraph [
        "What's the smallest practical combinator? Let's first list the ",
        "features we need: we need to be able to perform any computation, ",
        "we need to be able to virtualize computations, and we need to be ",
        "able to recognize functions and values and replace them with jetted ",
        "versions to make runtime practical."
        ]

      elAttr "pre" ("class" =: "docs") $ do
        text $ unlines
          [ "Reduction Rules:"
          , "    *(K x y)           -> x"
          , "    *(x y)             -> (*x y)"
          , "    *(x y)             -> (x *y)"
          , "    *(S x y z)         -> (x z (y z))"
          , "    *(D x)             -> JAM(x)"
          , "    *(J^n t f x1 … xn) -> (f x1 … xn)"
          ]

      paragraph [
        "The first four rules give us the strict SK calculus with a well ",
        "defined reduction ordering. ",
        "Let's go over the rules one by one. The first reduction is the K ",
        "combinator. The K combinator returns its first argument and discards ",
        "its second:"
        ]

      reduceDemo "(K K K)"

      paragraph [
        "Simple enough. The next two reduction rules specify order of ",
        "operations: we reduce the left hand side before the right hand side. ",
        "We must do this for short circuiting reasons. Uruk is strict instead ",
        "of lazy and if we did not do this, both arguments to K would be ",
        "evaluated before the first is returned."
        ]

      reduceDemo "(K K (S K K))"

      -- TODO: This is technically true, but doesn't get at the heart of the why
      -- of S.
      paragraph [
        "After that, we have the S combinator. The S combinator substitutes ",
        "its arguments, returning its first, third, and then calling its ",
        "second with its third."
        ]

      reduceDemo "(S K (S K) (S K K))"

      paragraph [
        "That's it for the core of the SK system we have in Uruk."
        ]

      el "p" $ do
        text $ unlines [
          "Our next requirement is being able to virtualize computations: we ",
          "must have a way of running a function which we don't know if it ",
          "will error by infinite looping. We thus need to be able to declare ",
          "a +mock function with the signature "
          ]

        el "code" (text "a -> Maybe a")

        text $ unlines [
          ". Nock 4K supports virtualization by being homiconic, but since ",
          "Uruk isn't homoiconic, we support this by having a dedicated Dump ",
          "combinator that dumps any Uruk expression as a Church-encoded ",
          "natural number."
          ]

      reduceDemo "(D (S K K))"

      paragraph [
        "The D combinator is never run in practice though. Nock's big idea ",
        "is the Jet: formally specify every function or piece of code in ",
        "your pure-functional system without any side-effects or FFI and ",
        "signal to the interpreter that a piece of code can be replaced with ",
        "an optimized implementation. In practice, D is only used inside of ",
        "virtualization functions which need to read out an Uruk value."
        ]

      paragraph [
        "Our final requirement is making functions in code legible to the ",
        "interpreter, so that they can be replaced with optimized native ",
        "code, if available. ",
        "The J combinator specifies a jet. The jet is a number of J letters ",
        "which specify the arity of the function, a tag which is only used ",
        "for matching, the function being jetted, and then the number of ",
        "arguments according to the stated arity. The arguments are ",
        "evaluated first. Look at the order in which the arguments are ",
        "reduced in this trace:"
        ]

      reduceDemo "(J J K (S K) (S K K K) (S K K K))"

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
        "remember an extra bit of matching information. In Uruk, there's no ",
        "need to do that because the jet matching happens at reduction time, ",
        "allowing for both faster jet matching, and ahead of time ",
        "optimizations."
        ]

      paragraph [
        "Unlike Nock 4K, we don't bake in natural numbers or cons cells. ",
        "Everything is made up of the unenriched lambda calculus, including ",
        "numbers which are Church numerals. We can make this fast by doing ",
        "data jetting, where we use jets to match the Church numeral ",
        "functions and replace them in the interpreter with integers."
        ]

    ---------------------------------------------------------------------------

    urdocSection "Compiling Hoonish Expressions" "hoon-expressions" $ do
      paragraph [
        "Building a new high performance functional VM is nice, but we also ",
        "want it to be a compile target for something like Hoon. So, as a ",
        "feasibility proof, lets produce a non-typed variant of Hoon and make ",
        "sure we can compile it to Uruk."
        ]

      -- TODO: This description could maybe actually explain how Church
      -- encoding works in SK?
      paragraph [
        "Uruk, unlike Nock 4K, does not have natural numbers as a primitive, ",
        "which means we must build them out of Church numerals. Raw Church ",
        "numerals are pretty inefficient though, so we wrap numbers with data ",
        "jets. We have raw Church numerals (+rawzer, +rawsuc), an operation ",
        "which takes a raw numeral and returns a jetted numeral (+pak), and ",
        "finally add jetted increment and addition that operate on jetted ",
        "numerals:"
        ]

      --  TODO: Text in paragraphs ignores spaces. This demo doesn't really
      --  work until it gets put in the text box correctly.
      compileDemo [
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
        "(add two two)"
        ]
        -- TODO: Preferably outputs 4.

      paragraph [
        "For now, we'll skip other jetted data types we can skip and just use ",
        "jetted natural numbers for now. Let's declare a gate which always ",
        "returns the same number:"
        ]

      compileDemo [
        "|=(a 43)"
        ]

      paragraph [
        "For any given input, \"K 43\" will return 43, since (K x y) is x. We ",
        "can verify this by calling the gate with a different number:"
        ]

      compileDemo [
        "=/  gate  |=(a 43)",
        "(gate 81)"
        ]

      -- TODO: This could be a lot better of a tutorial if I wasn't a talentless
      -- hack.
      el "h4" $ (text "<dog>I have no idea what I'm doing.</dog>")

    ---------------------------------------------------------------------------

    urdocSection "Storage and Snapshot Strategy" "storage" $ do
      paragraph [
        "So we've gone over the low level details of the reduction rules, and ",
        "shown how we can compile a hoon-like language to Uruk. But Urbit as ",
        "a complete system implies you have one giant function which contains ",
        "everything in your system. The move to Uruk doesn't change the fact ",
        "that the value of your Urbit is an arvo-like function which takes an ",
        "event and returns a pair of effects and a new arvo function."
        ]

      paragraph [
        "How do you make that fast?"
        ]

      paragraph [
        "Vere maps a 2 gigabyte loom directly to disk, which is fast but ",
        "limits the size of the image. Jaque serializes the entire arvo state ",
        "on each save, but this is slow and costly. Can we do better?"
        ]

      paragraph [
        "Nock has unifying equality on all nouns, meaning there wasn't a ",
        "principled place to break the noun tree up into units. The way Uruk ",
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
