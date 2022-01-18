{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraping where

import Control.Applicative ((<$>))
import Data.Default (def)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.HTML.Scalpel as Scalpel

data ScrapingData = ScrapingData
  { attr :: String,
    tag :: String
  }

-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings =
  HTTP.tlsManagerSettings
    { HTTP.managerModifyRequest = \req -> do
        req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
        return $
          req'
            { HTTP.requestHeaders =
                (HTTP.hUserAgent, "My Custom UA") :
                HTTP.requestHeaders req'
            }
    }

handleArgs :: [String] -> ScrapingData -> IO ()
handleArgs [url] sd = listUrlsForSite url sd
handleArgs _ _ = putStrLn "usage: custom-user-agent URL"

listUrlsForSite :: Scalpel.URL -> ScrapingData -> IO ()
listUrlsForSite url (ScrapingData {attr = a, tag = tagName}) = do
  manager <- Just <$> HTTP.newManager managerSettings
  images <- Scalpel.scrapeURLWithConfig (def {Scalpel.manager}) url $ Scalpel.attrs a tagName
  maybe printError printImages images
  where
    printError = putStrLn "ERROR: Could not scrape the URL!"
    printImages = mapM_ putStrLn
