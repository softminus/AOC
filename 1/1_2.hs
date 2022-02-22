
windowsummer' sums remaining = windowsummer (sums ++ [foldr (+) 0 (take 3 remaining)]) (drop 1 remaining) 
windowsummer sums remaining = case (3 <= length remaining) of
                                False -> sums
                                True -> windowsummer' sums remaining
-- we could have done this with zipWith3 (and three lists appropriately delayed)!
main = do
    text <- readFile "input.txt"
    let num = lines text
    let k = map read num :: [Integer]


    
    let sums = windowsummer [] k
    print sums
    let indicator new acc = case (new < fst acc) of
                            True -> (new, 1 + snd acc)
                            False -> (new, snd acc)

    let res = foldr indicator (0,0) sums

    print res