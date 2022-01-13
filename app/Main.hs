module Main where

--import qualified Scraping as S
--import qualified Assessment as A
--
import Text.HTML.Scalpel

type Author = String

data Comment
  = TextComment Author String
  | ImageComment Author URL
  deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
allComments = scrapeURL "http://example.com/article.html" comments
  where
    comments :: Scraper String [Comment]
    comments = chroots ("div" @: [hasClass "container"]) comment

    comment :: Scraper String Comment
    comment = textComment <|> imageComment

    textComment :: Scraper String Comment
    textComment = do
      author <- text $ "span" @: [hasClass "author"]
      commentText <- text $ "div" @: [hasClass "text"]
      return $ TextComment author commentText

    imageComment :: Scraper String Comment
    imageComment = do
      author <- text $ "span" @: [hasClass "author"]
      imageURL <- attr "src" $ "img" @: [hasClass "image"]
      return $ ImageComment author imageURL

main :: IO ()
main = do
  return allComments
