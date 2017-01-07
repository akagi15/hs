{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T


data Values = Values
            {   num :: !Int,
                name :: !String,
                what :: !String,
                kg :: !Int
            }

instance FromNamedRecord Values where 
        parseNamedRecord r = Values <$>
                             r.: "num" <*> 
                             r.: "name" <*> 
                             r .: "what" <*> 
                             r .: "kg" 


main = do
        csvData <- BL.readFile "testCsv.csv"
        case decodeByName csvData of
            Left err -> putStr err
            Right (_, v) -> mapM_ putStrLn $ V.toList $  V.map name v

