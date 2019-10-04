module Parser.Lexer where

import System.IO
import Control.Monad
import Data.Functor.Identity
import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))
import Text.Parsec.String (Parser(..))
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import qualified Numeric as N
import Data.Char(toUpper)

registerName :: Parser String
registerName = do
    P.char '$'
    (thirtyAndThirtyOne <|> twoDigits <|> oneDigit)
    where
        thirtyAndThirtyOne = P.try $ do
            P.char '3'
            u <- P.oneOf "01"
            return ['3', u]
        twoDigits = P.try $ do
            t <- P.oneOf "12"
            u <- P.digit
            return [t, u]
        oneDigit = P.count 1 P.digit


decImmediate :: Parser Int
decImmediate = do
    h <- P.digit
    P.notFollowedBy $ P.oneOf "bx"
    num <- P.many $ P.digit
    return (read (h:num)     :: Int)

hexImmediate :: Parser Int
hexImmediate = do
    P.string "0x"
    num <- P.many1 P.hexDigit
    return $ (fst . head) $ (N.readHex num)

binImmediate :: Parser Int
binImmediate = do
    P.string "0b"
    num <- P.many1 (P.oneOf "01")
    return $ (fst . head) $ (readBin num) where
        readBin = N.readInt 2 (\c -> (c == '0') || (c == '1')) (\c -> if c == '0' then 0 else 1)

label :: Parser String
label = (:) <$> P.letter <*> P.many P.alphaNum <?> "label"

keyword :: String -> Parser String
keyword str = P.try (P.string str <* spaceOrTab) >>= \p -> return p

colon :: Parser ()
colon = P.between spacesOrTabs spacesOrTabs (void $ P.char ':')

comma :: Parser ()
comma = P.between spacesOrTabs spacesOrTabs (void $ P.char ',')

comment :: Parser ()
comment = P.skipMany1 (P.char ';' >> P.manyTill P.anyChar P.endOfLine) <?> "comment"

spaceOrTab :: Parser ()
spaceOrTab = void $ P.oneOf " \t"

spacesOrTabs :: Parser ()
spacesOrTabs = P.skipMany spaceOrTab

spacesOrTabs1 :: Parser ()
spacesOrTabs1 = P.skipMany1 spaceOrTab

eol :: Parser ()
eol = void P.endOfLine

whitespaceOrComment :: Parser ()
whitespaceOrComment = spacesOrTabs >> void (P.sepEndBy1 eolOrComment spacesOrTabs1) where
    eolOrComment = P.many1 (eol <|> comment)