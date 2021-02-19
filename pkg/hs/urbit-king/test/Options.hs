module Options
  ( Brass
  , Pill
  , getPillPath
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy             (Proxy (Proxy))
import Data.String            (IsString)
import GHC.TypeLits           (KnownSymbol, Symbol)
import Prelude

import qualified GHC.TypeLits       as TypeLits
import qualified RIO.Directory      as Directory
import qualified Test.Tasty         as Tasty
import qualified Test.Tasty.Options as Options

type Brass = Pill "brass"

-- | A file-system path tagged by the pill name.
newtype Pill (name :: Symbol) = Pill FilePath
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance KnownSymbol name => Options.IsOption (Pill name) where
  optionName =
    pure ( TypeLits.symbolVal (Proxy @name)
        ++ "-pill"
         )
  
  optionHelp =
    pure ( "The file path to the "
        ++ TypeLits.symbolVal (Proxy @name)
        ++ " pill"
         )
  
  defaultValue =
    Pill ( "../../../bin/"
        ++ TypeLits.symbolVal (Proxy @name)
        ++ ".pill"
         )
    
  parseValue = \case
    ""   -> Nothing
    path -> Just (Pill path)

getPillPath :: MonadIO m => Pill name -> m FilePath
getPillPath (Pill path) = Directory.canonicalizePath path
