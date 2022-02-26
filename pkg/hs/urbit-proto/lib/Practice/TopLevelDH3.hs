module Practice.TopLevelDH3 where

import ClassyPrelude

import Data.Void

import Practice.DependentHoon3
import Practice.Hoon2DependentHoon3
import Practice.HoonSyntax
import Practice.Render

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
    , cod :: Soft
    , cld :: Code Void
    , typ :: Type a
    , bas :: Base a
    , mor :: extra
    }
  | ResOpen
    { cst :: Hoon
    , cod :: Soft
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
scam = Con 0 Noun' (Atom' 0)

-- | Perform all passes of the compiler on the given text
ride :: Var a => FilePath -> Con a -> Text -> Result () a
ride filename con txt =
  case parse vest filename txt of
    Left err -> ResNone{..}
    Right cst ->
      case open cst of
        Left ero -> ResRead{..}
        Right cod ->
          let mor = () in
          case runCheck (play con cod) of
            Left ert -> ResOpen{..}
            Right (cld, typ) ->
              let bas = eval (ken con) (vacuous cld)
              in ResType{..}

-- | Perform all passes of the compiler on the given text, tracing execution.
road :: Var a => FilePath -> Con a -> Text -> Result ActTree a
road filename con txt =
  case parse vest filename txt of
    Left err -> ResNone{..}
    Right cst ->
      case open cst of
        Left ero -> ResRead{..}
        Right cod ->
          case runTrace (play con cod) of
            (mor, Left er) ->
              let ert = (traceToStack mor, er)
              in ResOpen{..}
            (mor, Right (cld, typ)) ->
              let bas = eval (ken con) (vacuous cld)
              in ResType{..}
