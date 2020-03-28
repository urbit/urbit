{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Uruk.JetSpec (jetSpec) where

import Data.FileEmbed (embedStringFile)
import Data.Text      (Text)


-- Numbers ---------------------------------------------------------------------

jetSpec :: Text
jetSpec = $(embedStringFile "urbit-uruk/jets.dash")
