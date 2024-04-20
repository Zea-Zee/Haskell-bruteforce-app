module Main (main) where
import qualified Seq
import qualified Par
import qualified Par2
import Hashes

import Data.Maybe
import Data.Time
import Data.List
import Text.Printf
import Data.Char
import Prelude
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA
import qualified Crypto.Hash.SHA256 as SHA2
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- main :: IO ()
algo' :: String -> String -> String
algo' "md5" pass = pass--hex $ MD5.hash $ BC.pack pass
algo' "sha1" pass = pass--hex $ SHA.hash $ BC.pack pass
algo' "sha256" pass = pass--hex $ SHA2.hash $ BC.pack pass
algo' "pass" pass = Seq.hex $ MD5.hash $ BC.pack pass--hex $ SHA2.hash $ BC.pack pass
algo' m pass = Seq.hex $ MD5.hash $ BC.pack pass

main = do
    putStrLn "Enter the algo (md5/sha1/sha256/pass (if it is uncoded)) or press 'Enter' if it is original pass 'abc' for ex."
    algo <- getLine
    putStrLn "Enter the length of pass (max 6) or Enter if you dont know len"
    len <- getLine
    putStrLn "Do you want to use base of most popular passwords?(y/n(Enter))"
    useBase <- getLine
    --putStrLn "Enter pass mode (a-z is 0/A-z is 1/ 0-z is 2/+symbols - 3: +more symbols - 4/use all symbols - 5); For example: pass qwerty 2"
    putStrLn "Enter mode for symbols: 0: a-z(26); 1: a-Z(52); 2: a-0(62); 3: prev + '!\"\\#$%&'()*+,-./'(77); 4: prev + ':;<=>?@'(84); 5: all useful ASCII symbols(94); Enter = 0"
    alph <- getLine
    putStrLn "Enter pass/hash"
    pass <- getLine

    start <- getCurrentTime
    print $ show (if length len > 0 then digitToInt (head len) else -1) ++ "<-len|||mode->" ++ show (if length alph > 0 then digitToInt (head alph) else 0) ++ show (if useBase == "y" then True else False) ++ show (if length algo > 0 then algo else "md5") ++ show (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    let answ = Par2.bruteforce (if length len > 0 then digitToInt (head len) else -1) (if length alph > 0 then digitToInt (head alph) else 0) (if useBase == "y" then True else False) (if length algo > 0 then algo else "md5") (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    print answ
    -- print start
    end <- getCurrentTime
    -- print end
    print $ "Parallel bruteforce took: " ++ show (diffUTCTime end start)

    start <- getCurrentTime
    print $ show (if length len > 0 then digitToInt (head len) else -1) ++ "<-len|||mode->" ++ show (if length alph > 0 then digitToInt (head alph) else 0) ++ show (if useBase == "y" then True else False) ++ show (if length algo > 0 then algo else "md5") ++ show (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    let answ = Seq.bruteforce (if length len > 0 then digitToInt (head len) else -1) (if length alph > 0 then digitToInt (head alph) else 0) (if useBase == "y" then True else False) (if length algo > 0 then algo else "md5") (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    print answ
    -- print start
    end <- getCurrentTime
    -- print end
    print $ "Sequence bruteforce took: " ++ show (diffUTCTime end start)

    start <- getCurrentTime
    print $ show (if length len > 0 then digitToInt (head len) else -1) ++ "<-len|||mode->" ++ show (if length alph > 0 then digitToInt (head alph) else 0) ++ show (if useBase == "y" then True else False) ++ show (if length algo > 0 then algo else "md5") ++ show (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    let answ = Par.bruteforce (if length len > 0 then digitToInt (head len) else -1) (if length alph > 0 then digitToInt (head alph) else 0) (if useBase == "y" then True else False) (if length algo > 0 then algo else "md5") (algo' algo pass)                            --len used_symbols use_defold_passes algorithm code
    print answ
    -- print start
    end <- getCurrentTime
    -- print end
    print $ "Parallel 1_ver took: " ++ show (diffUTCTime end start)