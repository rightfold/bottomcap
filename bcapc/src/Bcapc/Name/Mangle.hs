-- |
-- bcapc.name.mangle.
module Bcapc.Name.Mangle
  ( -- * Mangling
    mangleGlobal
  , mangleLocal
  , mangleField
  ) where

import Control.Category ((>>>))
import Control.Lens ((&))
import Data.ByteString.Builder (Builder)
import Data.Char (chr, ord)
import Data.Hashable (unhashed)
import Data.Text (Text)
import Text.Printf (printf)

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text

import Bcapc.Name (Field (..), Global (..), Identifier (..), Local (..))

--------------------------------------------------------------------------------
-- Mangling

-- |
-- bcapc.name.mangle.global.
mangleGlobal :: Global -> Builder
mangleGlobal (Global package name) =
  text $ globalPrefix <> foldMap normalize (package' <> [name'])
  where
  package' :: [Text]
  package' = [ p | Identifier (unhashed -> p) <- package ]

  name' :: Text
  name' = let Identifier (unhashed -> n) = name in n

-- |
-- bcapc.name.mangle.local.
mangleLocal :: Local -> Builder

mangleLocal (Local (Identifier (unhashed -> name))) =
  text $ localPrefix <> normalize name

mangleLocal (Synthesized identity) =
  text $ synthesizedPrefix <> normalize (show identity & Text.pack)

-- |
-- bcapc.name.mangle.field.
mangleField :: Field -> Builder
mangleField (Field (Identifier (unhashed -> name))) =
  text $ fieldPrefix <> normalize name

--------------------------------------------------------------------------------
-- Normalizing

-- |
-- bcapc.name.mangle.normalize.
normalize :: Text -> Text
normalize = id
  >>> Text.concatMap step1
  >>> Text.concatMap step2
  >>> Text.concatMap step3
  >>> step4
  where
  step1 :: Char -> Text
  step1 'z' = "zz"
  step1 c   = Text.singleton c

  step2 :: Char -> Text
  step2 c | isLowerLetter c = Text.singleton c
          | isUpperLetter c = Text.singleton c
          | isDigit c       = Text.singleton c
          | otherwise = printf "zs%08x" (ord c) & Text.pack

  step3 :: Char -> Text
  step3 c | isUpperLetter c = ['z', 'u', toLower c] & Text.pack
          | otherwise = Text.singleton c

  step4 :: Text -> Text
  step4 t = Text.pack (show (Text.length t)) <> "z" <> t

  isLowerLetter, isUpperLetter, isDigit :: Char -> Bool
  isLowerLetter c = c >= 'a' && c <= 'z'
  isUpperLetter c = c >= 'A' && c <= 'Z'
  isDigit c = c >= '0' && c <= '9'

  toLower :: Char -> Char
  toLower c = chr (ord c + 32)

--------------------------------------------------------------------------------
-- Miscellaneous

text :: Text -> Builder
text = Builder.stringUtf8 . Text.unpack

globalPrefix :: Text
globalPrefix = "bcapg"

localPrefix :: Text
localPrefix = "bcapl"

synthesizedPrefix :: Text
synthesizedPrefix = "bcaps"

fieldPrefix :: Text
fieldPrefix = "bcapf"
