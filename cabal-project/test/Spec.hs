{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import CityPopulation (getCityPopulation)

main :: IO ()
main = hspec $ do
  describe "getCityPopulation" $ do
    it "returns the population for a known city" $ do
      population <- getCityPopulation "New York"
      population `shouldBe` Just 18713220
