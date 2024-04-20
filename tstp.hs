import Control.Parallel.Strategies

-- op :: Double
op = {-(foldl1 (\x y -> x + y)-} map (\x -> x ** (5.3) / 11.9 ** (7.2) / (-56.5 ** 3.1) * (0.19 **(25.1))) [1..1000000]-- `using`(evalList rpar))



main = do
    -- print $ sum op
    print $ sum $ parMap rpar (\x -> ($ tan $ sin x) ** (5.3) / 11.9 ** (7.2) / (-56.5 ** 3.1) * (0.19 **(55.1))) [1..1000000]