module Ur.Moon.Lexer where

import ClassyPrelude hiding (head, many, some, try)
import GHC.Natural

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy
import Data.List.NonEmpty (NonEmpty(..))

import Data.Void        (Void)
import Prelude          (read)
import Prelude          (head)
import Text.Format.Para (formatParas)

import qualified Data.MultiMap     as MM
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Prelude


--------------------------------------------------------------------------------

type Nat = Natural

data Lexeme
    = PAL | PAR | LOB | ROB | GAL | GAR | FAS
    | GAP | ACE
    | LET | LAM | END
    | AP2 | AP3 | AP4 | APN
    | PI2 | PI3 | PI4 | PIN
    | SYM !Text
    | NAT !Nat
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

pal,par,lob,rob,lak,rak,gal,gar,fas :: (MonadParsec e s m, s~Text) => m ()
pal = void $ char '('
par = void $ char ')'
lob = void $ char '{'
rob = void $ char '}'
lak = void $ char '['
rak = void $ char ']'
gal = void $ char '<'
gar = void $ char '>'
fas = void $ char '/'

gap,fat,lin,ace :: (MonadParsec e s m, s~Text) => m ()
gap = lin <|> fat
fat = void $ (:) <$> char ' ' <*> some (char ' ')
lin = void $ char '\n'
ace = void $ char ' '

lEt,lam,end :: (MonadParsec e s m, s~Text) => m ()
lEt = void $ string "=/"
lam = void $ string "|="
end = void $ string "##"

ap2,ap3,ap4,apn,pi2,pi3,pi4,pin :: (MonadParsec e s m, s~Text) => m ()
ap2 = void $ string "%-"
ap3 = void $ string "%+"
ap4 = void $ string "%@"
apn = void $ string "%*"
pi2 = void $ string "$-"
pi3 = void $ string "$+"
pi4 = void $ string "$@"
pin = void $ string "$*"

symChr, fstChr, resChr, digChr :: (MonadParsec e s m, s~Text) => m Char
symChr = oneOf ("*@~`!@#$%^&*-_+=|;:,.?]" :: [Char])
fstChr = oneOf ['a' .. 'z']
resChr = oneOf (['a' .. 'z'] <> "-" <> ['0' .. '9'])
digChr = oneOf ['0' .. '9']

num :: (MonadParsec e s m, s~Text) => m Nat
num = read . unpack <$> some digChr

sym, nam, idn :: (MonadParsec e s m, s~Text) => m Text
sym = singleton <$> symChr
nam = pack <$> ((:) <$> fstChr <*> many resChr)
idn = sym <|> nam

lexeme :: (MonadParsec e s m, s~Text) => m Lexeme
lexeme = choice
    [ pal      $> PAL
    , par      $> PAR
    , lob      $> LOB
    , rob      $> ROB
    , gal      $> GAL
    , gar      $> GAR
    , fas      $> FAS
    , try gap  $> GAP
    , ace      $> ACE
    , lEt      $> LET
    , lam      $> LAM
    , ap2      $> AP2
    , ap3      $> AP3
    , ap4      $> AP4
    , apn      $> APN
    , pi2      $> PI2
    , pi3      $> PI3
    , pi4      $> PI4
    , pin      $> PIN
    , end      $> END
    , idn     <&> SYM
    , num     <&> NAT
    ]


-- Testing ---------------------------------------------------------------------

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (many @(Parsec Void Text) lexeme <* eof)

main ∷ IO ()
main = forever $ do
    ln <- getLine
    parseHoonTest ln
