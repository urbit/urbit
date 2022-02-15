module Practice.TopLevelDH3 where

import ClassyPrelude

import Data.Void

import Practice.DependentHoon3
import Practice.Hoon2DependentHoon3
import Practice.HoonCommon
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

data Result a
  = ResType
    { cst :: Hoon
    , cod :: Code Wing
    , cld :: Code Stub
    , typ :: Type a
    , bas :: Base a
    }
  | ResOpen
    { cst :: Hoon
    , cod :: Code Wing
    , ert :: ([Act], Fail)
    }
  | ResRead
    { cst :: Hoon
    , ero :: Text
    }
  | ResNone
    { err :: Text
    }

-- | The "empty" subject context.
scam :: Con Void
scam = Con 0 Noun' (Atom' 0 Sand "")

-- | Perform all passes of the compiler on the given text
road :: Var a => FilePath -> Con a -> Text -> Result a
road filename con txt =
  case parse vest filename txt of
    Left err -> ResNone{..}
    Right cst ->
      case open cst of
        Left ero -> ResRead{..}
        Right cod ->
          case runReaderT (play con cod) [] of
            Left ert -> ResOpen{..}
            Right (cld, typ) ->
              let bas = eval (ken con) (fmap New cld)
              in ResType{..}
