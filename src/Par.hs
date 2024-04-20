module Par(
    someFunc,
    brute,
    hex,
    bruteforce
) where

import Hashes
import Data.Maybe
import Data.Time
import Data.List
import Text.Printf
import Prelude
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA
import qualified Crypto.Hash.SHA256 as SHA2
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Control.Parallel.Strategies

hex :: B.ByteString -> String
hex = concatMap (printf "%02x") . B.unpack

someFunc :: IO ()
someFunc = putStrLn "someFunc"

alphabet :: Int -> [Char]
alphabet 0 = ['a'..'z']
alphabet 1 = ['A'..'Z'] ++ (alphabet 0)
alphabet 2 = ['0'..'9'] ++ (alphabet 1)
alphabet 3 = ['!'..'/'] ++ (alphabet 2)
alphabet 4 = [':'..'@'] ++ (alphabet 3)
alphabet mode = ['!'..'~']

bruteforce :: Int -> Int -> Bool -> String -> String -> Maybe String         --now -usebase -algo
bruteforce _ _ _ _ "" = Nothing
bruteforce (-1) alphs useBase algo pass = bruteWitnUncnownLength 1 alphs useBase algo pass
bruteforce len alphs useBase algo pass  = if isJust resOfcmpWithBase then resOfcmpWithBase else brute 0 len algo "" pass alphs
    where resOfcmpWithBase = if useBase && len >= 4 then findKey pass (codes len algo) else Nothing

bruteWitnUncnownLength ::  Int -> Int -> Bool -> String -> String -> Maybe String         --now -usebase -algo
bruteWitnUncnownLength 7 alphs useBase algo pass = Nothing
bruteWitnUncnownLength curLen alphs useBase algo pass   | res == Nothing = bruteWitnUncnownLength (curLen + 1) alphs useBase algo pass
                                                        | otherwise = res
    where res = bruteforce curLen alphs useBase algo pass

findKey :: String -> [(String,String)] -> Maybe String
findKey key [] = Nothing
findKey key ((k,v):xs)  | key == k = Just v
                        | otherwise = findKey key xs

-- brute :: Int -> Int -> String -> String -> Maybe String
-- brute curLen len str pass   | curLen == len = if str == pass then Just str else Nothing
--                             | otherwise = result
--     where   result = doWhile' (alphabet 0)
--             doWhile' :: [Char] -> Maybe String
--             doWhile' [] = Nothing
--             doWhile' (x:xs) | isNothing res = doWhile' xs
--                             | otherwise = res
--                 where res = brute (curLen + 1) len (x : str) pass

brute :: Int -> Int -> String -> String -> String -> Int -> Maybe String
brute curLen len algo str pass mode     | curLen == len = if (customHash str algo) == pass then Just str else Nothing
                            | otherwise = result
    where   result = runEval (do
                q <- rpar (doWhile' (alphabet 0))
                return q)
            doWhile' :: [Char] -> Maybe String
            doWhile' [] = Nothing
            doWhile' (x:xs) | isNothing res = runEval (do 
                                qq <- rpar (doWhile' xs)
                                return qq)
                            | otherwise = res
                where res = runEval (do
                                        qqq <- rpar (brute (curLen + 1) len algo (x : str) pass mode)
                                        return qqq)

-- resE curLen len str pass = runEval $ do
--     q <- rpar (doWhile' (alphabet 0))
--     return q

-- doWhile' :: [Char] -> Maybe String
-- doWhile' [] = Nothing
-- doWhile' (x:xs) | isNothing res = doWhile' xs
--                 | otherwise = res
--     where res = brute (curLen + 1) len (x : str) pass


customHash :: String -> String -> String
customHash str "md5" = hex $ MD5.hash $ BC.pack str
customHash str "pass" = hex $ MD5.hash $ BC.pack str
customHash str "sha1" = hex $ SHA.hash $ BC.pack str
customHash str "sha256" = hex $ SHA2.hash $ BC.pack str