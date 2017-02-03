{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Codec.Binary.UTF8.String
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T


getCsvRow csvData rowName =  case decodeByName csvData of
                                Left err ->  err
                                Right (_, v) ->  tempList
                                where
                                    tempList = V.toList $  V.map rowName v



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
    mapM_ putStr tempList 
    
    where
        tempList = getCsvRow csvData name

{--
intLst = map show [1..(length trdLst)]
indLst = ["x1","x2","x3"]
goodsLst = ["a1","a2"]
trdLst = [["x1","x2","a1","1"],["x2","x3","a2","2"],["x1","x2","a1","3"],["x2","x3","a1","4"]]

ex a = [[q,a,i]|  i <- intLst,
                  p  <- indLst,
                  q <- indLst,
                (elem [q,p,a,i]  trdLst) ||( elem [p,q,a,i] trdLst)]


count a = [(n,q,a) | q <- indLst,
                    let ex' b p  = [[p,b]|i <- intLst, elem [p,b,i] (ex b) ],
                    let n = (length (ex' a q))] 

countedNum a = [x| (x,q,a) <- (count a)]

fSNum a   
    |  (length list) > 1    = ((head list), (list !! 1))
    | otherwise             = ((head list), 0)
        where 
            list = reverse (sort (countedNum a))

selection a = [((x::Int,q,a),(y::Int,p,a))| (x,q,a) <- (count a),
                                            (y,p,a) <- (count a),
                                            (x,y) == (fSNum a)]

main = do
    print (ex "a1")
    print (count "a1")
    print (countedNum "a1")
    print (fSNum "a1")
    print (selection "a1")
--}
