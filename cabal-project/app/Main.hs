{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
newtype City = City
    { population :: Int
    } deriving (Show)

instance FromJSON City where
    parseJSON = withObject "City" $ \v -> City
        <$> v .: "population"

getCityPopulation :: String -> IO (Maybe Int)
getCityPopulation cityName = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest $ "https://api.api-ninjas.com/v1/city?name=" ++ cityName
    let requestWithHeaders = request { requestHeaders = [("X-Api-Key", "nrRActwLfCgxft2u3GmZKw==fc5AxcwtjDdsyNWL")] }
    response <- httpLbs requestWithHeaders manager
    let status = statusCode (responseStatus response)
    if status == 200
        then do
            let body = responseBody response
            case decode body :: Maybe [City] of
                Just (city:_) -> return $ Just (population city)
                _             -> do
                    putStrLn "Error: Unable to parse JSON response."
                    return Nothing
        else do
            putStrLn $ "Error: " ++ show status ++ " " ++ L8.unpack (responseBody response)
            return Nothing

main :: IO ()
main = do
    putStrLn "Enter the name of the city: "
    cityName <- getLine
    population <- getCityPopulation cityName
    case population of
        Just pop -> putStrLn $ "The population of " ++ cityName ++ " is " ++ show pop ++ "."
        Nothing  -> putStrLn $ "Could not find population data for " ++ cityName ++ "."
