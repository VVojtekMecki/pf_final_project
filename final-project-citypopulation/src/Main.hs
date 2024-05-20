{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (eitherDecode, withObject, (.:))
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception (catch)
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- Define a data type for the API response
data CityInfo = CityInfo
  { population :: Int
  } deriving (Show)

instance FromJSON CityInfo where
  parseJSON = withObject "CityInfo" $ \v -> CityInfo
    <$> v .: "population"

-- Function to fetch city information
fetchCityInfo :: String -> IO (Either String CityInfo)
fetchCityInfo city = do
  let url = "https://api.example.com/population?city=" ++ city
  response <- catch (simpleHttp url) handleHttpException
  return $ eitherDecode response

  where
    handleHttpException :: HttpException -> IO L8.ByteString
    handleHttpException _ = return "{\"error\": \"Request failed\"}"

-- Function to interact with the user and display results
main :: IO ()
main = do
  putStr "Enter city name: "
  hFlush stdout
  city <- getLine
  result <- fetchCityInfo city

  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right cityInfo -> do
      when (population cityInfo == 0) $
        putStrLn "City not found or no population data available."
      putStrLn $ "Population of " ++ city ++ ": " ++ show (population cityInfo)