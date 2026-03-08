{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V

data Waste = Waste
  { country :: String
  , measure :: String
  , year :: Int
  , value :: Double
  } deriving (Show, Generic)

instance FromNamedRecord Waste where
  parseNamedRecord r =
    Waste <$> r .: "Country"
          <*> r .: "Measure"
          <*> r .: "Year"
          <*> r .: "WasteValue"

loadWasteData :: IO [Waste]
loadWasteData = do
  csvData <- BL.readFile "data/waste26.csv"
  case decodeByName csvData of
    Left err -> do
      putStrLn err
      return []
    Right (_, v) -> return (V.toList v)

generateTable :: [Waste] -> String
generateTable rows =
  "<table>" ++
  "<tr><th>Country</th><th>Measure</th><th>Year</th><th>Waste Value</th></tr>" ++
  concatMap row rows ++
  "</table>"
  where
    row w =
      "<tr><td>" ++ country w ++
      "</td><td>" ++ measure w ++
      "</td><td>" ++ show (year w) ++
      "</td><td>" ++ show (value w) ++
      "</td></tr>"

main :: IO ()
main = do

  wasteData <- loadWasteData

  let efficient = filter (\x -> value x > 5000) wasteData
  let total = foldr (\x acc -> value x + acc) 0 wasteData
  let avg =
        if null wasteData
        then 0
        else total / fromIntegral (length wasteData)

  scotty 3000 $ do

    get "/" $ do
      html (pack (
        "<html>" ++
        "<head>" ++
        "<title>Municipal Recycling Efficiency</title>" ++
        "<style>" ++
        "body{font-family:Segoe UI;background:#eef2f7;margin:0;padding:0;text-align:center;}" ++
        "header{background:#1e3a5f;color:white;padding:20px;font-size:28px;font-weight:bold;}" ++
        "section{padding:30px;}" ++
        "h2{color:#2c3e50;margin-top:40px;}" ++
        "table{border-collapse:collapse;width:85%;margin:auto;background:white;border-radius:8px;overflow:hidden;box-shadow:0 4px 12px rgba(0,0,0,0.1);}" ++
        "th{background:#2980b9;color:white;padding:12px;font-size:16px;}" ++
        "td{padding:10px;border-bottom:1px solid #ddd;font-size:14px;}" ++
        "tr:hover{background:#f1f7ff;}" ++
        ".card{background:white;width:300px;margin:20px auto;padding:20px;border-radius:8px;box-shadow:0 4px 10px rgba(0,0,0,0.1);}" ++
        ".value{font-size:24px;color:#27ae60;font-weight:bold;}" ++
        "</style>" ++
        "</head>" ++

        "<body>" ++

        "<header>Municipal Recycling Efficiency Analysis</header>" ++

        "<section>" ++

        "<p>OECD Waste Dataset Analysis using Haskell Functional Programming</p>" ++

        "<div class='card'>" ++
        "<p>Total Waste Value</p>" ++
        "<div class='value'>" ++ show total ++ "</div>" ++
        "</div>" ++

        "<div class='card'>" ++
        "<p>Average Waste Value</p>" ++
        "<div class='value'>" ++ show avg ++ "</div>" ++
        "</div>" ++

        "<h2>All Waste Data</h2>" ++
        generateTable wasteData ++

        "<h2>High Recycling Efficiency (Value > 5000)</h2>" ++
        generateTable efficient ++

        "</section>" ++
        "</body>" ++
        "</html>"
        ))