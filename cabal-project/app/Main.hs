{-# LANGUAGE OverloadedStrings #-}

import CityPopulation (getCityPopulation)
import Control.Monad (unless)

main :: IO ()
main = do
    putStrLn "Enter the name of the city: "
    cityName <- getLine
    unless (null cityName) $ do
        population <- getCityPopulation cityName
        case population of
            Just pop -> putStrLn $ "The population of " ++ cityName ++ " is " ++ show pop ++ "."
            Nothing  -> putStrLn $ "Could not find population data for " ++ cityName ++ "."
