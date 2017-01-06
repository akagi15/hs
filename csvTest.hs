{-# LANGUAGE OverloadedStrings #-}


import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V


data Animal = Animal
            {   num :: !Int,
                name :: !String,
                what :: !String,
                kg :: !Int
            }

instance FromNamedRecord Animal where 
        parseNamedRecord r = Animal <$>
                             r.: "num" <*> 
                             r.: "name" <*> 
                             r .: "what" <*> 
                             r .: "kg" 



main = do
        csvData <- BL.readFile "testCsv.csv"
        case decodeByName csvData of
            Left err -> putStr err
            Right (_, v) -> V.forM_ v $ \ p -> putStrLn $ show (num p) ++ name p ++ " is " ++ show (kg p) ++ " kg " ++ what p

