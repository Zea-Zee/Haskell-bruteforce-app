import Data.Maybe
import Data.Time
import Text.Printf
import Prelude
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA
import qualified Crypto.Hash.SHA256 as SHA2
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.HexString as HEXstr

infixr  1 ??
(??) :: a -> a -> Bool -> a
(??) t f p = if' p t f 

if' :: Bool -> a -> a -> a
if' p t f = if p then t else f

tstByte :: B.ByteString
tstByte = BC.pack "12345"

m = hex $ MD5.hash tstByte
s1 = hex $ SHA.hash tstByte
s2 = SHA2.hash tstByte

hex :: B.ByteString -> String
hex = concatMap (printf "%02x") . B.unpack














alphabet :: Int -> [Char]
alphabet 0 = ['a'..'z']
alphabet 1 = ['A'..'Z'] ++ (alphabet 0)
alphabet 2 = ['0'..'9'] ++ (alphabet 1)
alphabet 3 = ['!'..'/'] ++ (alphabet 2)
alphabet 4 = [':'..'@'] ++ (alphabet 3)
alphabet mode = ['!'..'~']

brute :: Int -> Int -> String -> String -> Int -> Maybe String
brute curLen len str pass mode  | curLen == len = if str == pass then Just str else Nothing
                                | otherwise = result
    where   result' = [b | x <- (alphabet mode), let b = (brute (curLen + 1) len (x : str) pass mode), isJust b]
            result'' = if length result' > 0 then head result' else Nothing
            result = doWhile' (alphabet mode)
            doWhile' :: [Char] -> Maybe String
            doWhile' [] = Nothing
            doWhile' (x:xs) | isNothing res = doWhile' xs
                            | otherwise = res
                where res = brute (curLen + 1) len (x : str) pass mode

-- doWhile' :: [Char] -> Maybe String
-- doWhile' [] = Nothing
-- doWhile' (x:xs) | isNothing res = xdoWhile' xs
--                 | otherwise = res
--     where res = brute (curLen + 1) len (x : str) pass mode

main = do
    start1 <- getCurrentTime
    let a = brute 0 5 "" "bapes" 3
    end1 <- getCurrentTime
    putStr $ show a
    print (diffUTCTime end1 start1)
    -- start2 <- getCurrentTime
    -- let b = brute 0 3 "" "abc" 5
    -- end2 <- getCurrentTime
    -- putStr $ show b
    -- print (diffUTCTime end2 start2)
    -- start3 <- getCurrentTime
    -- let c = brute 0 4 "" "abcd" 3
    -- end3 <- getCurrentTime
    -- putStr $ show c
    -- print (diffUTCTime end3 start3)
    -- start4 <- getCurrentTime
    -- let d = brute 0 4 "" "aB0!" 4
    -- end4 <- getCurrentTime
    -- putStr $ show d
    -- print (diffUTCTime end4 start4)
    -- start5 <- getCurrentTime
    -- let e = brute 0 5 "" "aB0!:" 4
    -- putStr $ show e
    -- end5 <- getCurrentTime
    -- print (diffUTCTime end5 start5)

    -- start6 <- getCurrentTime
    -- let f = brute 0 6 "" "abcdef" 0
    -- end6 <- getCurrentTime
    -- putStr $ show f
    -- print (diffUTCTime end6 start6)

    -- start6 <- getCurrentTime
    -- let f = brute 0 5 "" "fO4an" 2
    -- end6 <- getCurrentTime
    -- putStr $ show f
    -- print (diffUTCTime end6 start6)

    -- start6 <- getCurrentTime
    -- let f = brute 0 6 "" "fO4an?" 3
    -- end6 <- getCurrentTime
    -- putStr $ show f
    -- print (diffUTCTime end6 start6)

    -- start6 <- getCurrentTime
    -- let f = brute 0 6 "" "aB0!:~" 5
    -- end6 <- getCurrentTime
    -- putStr $ show f
    -- print (diffUTCTime end6 start6)
    putStrLn "THAT's ALL"
    stp <- getLine
    print stp