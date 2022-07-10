module Practice.TopLevelDH4 where

import ClassyPrelude hiding (find)

import Data.Void

import Practice.DependentHoon4 hiding (typ, cod)
import Practice.Hoon2DependentHoon4
import Practice.HoonCommon
import Practice.HoonSyntax
import Practice.Nock
import Practice.Render
import Practice.RenderDH4Orphans()

data Err
  = ErrRead Text
  | ErrOpen Text
  | ErrType ([Act], Fail)

instance Rolling Err where
  roll = \case
    ErrRead t -> roll t
    ErrOpen t -> roll t
    ErrType t -> roll t

data Result extra a
  = ResType
    { cst :: Hoon
    , sof :: Soft
    , cod :: Code Void
    , typ :: Type a
    , fis :: Set Fish  -- XX should this not be checked at top level?
    , ken :: Semi a
    , nok :: Nock
    , mor :: extra
    }
  | ResOpen
    { cst :: Hoon
    , sof :: Soft
    , ert :: ([Act], Fail)
    , mor :: extra
    }
  | ResRead
    { cst :: Hoon
    , ero :: Text
    }
  | ResNone
    { err :: Text
    }

deriving instance (Show a, Show extra) => Show (Result extra a)

-- | The "empty" subject context.
scam :: Con Void
scam = Con 0 (Sing' (Atom' 0) Noun')

-- | Perform all passes of the compiler on the given text
ride :: Var a => FilePath -> Con a -> Text -> Result () a
ride filename con txt =
  case parse vest filename txt of
    Left err -> ResNone{..}
    Right cst ->
      case open cst of
        Left ero -> ResRead{..}
        Right sof ->
          let mor = () in
          case runCheck (play con sof) of
            Left ert -> ResOpen{..}
            Right (cod, typ, fis) ->
              let ken = evil con cod
                  nok = mint cod
              in ResType{..}

-- | Perform all passes of the compiler on the given text, tracing execution.
road :: Var a => FilePath -> Con a -> Text -> Result ActTree a
road filename con txt =
  case parse vest filename txt of
    Left err -> ResNone{..}
    Right cst ->
      case open cst of
        Left ero -> ResRead{..}
        Right sof ->
          case runTrace (play con sof) of
            (mor, Left er) ->
              let ert = (traceToStack mor, er)
              in ResOpen{..}
            (mor, Right (cod, typ, fis)) ->
              let ken = evil con cod
                  nok = mint cod
              in ResType{..}
