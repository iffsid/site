module Extras.ReadKeywords (
    Keywords(..)
  , KeywordElement(..)
  , readKeywords
) where

import           Control.Monad      (void)
import           Text.Parsec
import           Text.Parsec.String

newtype Keywords = Keywords
    { unKeyword :: [KeywordElement]
    } deriving (Show, Eq)

data KeywordElement
    = Chunk String
    | Escaped
    | Youtube String
    -- | Vimeo String
    -- | SlideShare String
    -- | Tikz (Maybe String) String
    deriving (Show, Eq)

readKeywords :: String -> Keywords
readKeywords input = case parse keywords "" input of
    Left err -> error $ "Cannot parse keywords: " ++ show err
    Right t -> t

keywords :: Parser Keywords
keywords = Keywords <$> many1 (chunk <|> escaped <|> youtube)

chunk :: Parser KeywordElement
chunk = Chunk <$> many1 (noneOf "§")

escaped :: Parser KeywordElement
escaped = Escaped <$ try (string "§§")

simpleIdParserGen :: String -> (String -> KeywordElement) -> Parser KeywordElement
simpleIdParserGen identifier constructor = try $ do
    void $ string ("§"++ identifier ++ "(")
    embedId <- many1 $ noneOf ")"
    void $ string ")§"
    return $ constructor embedId

youtube :: Parser KeywordElement
youtube = simpleIdParserGen "youtube" Youtube
