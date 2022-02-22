main = do
    text <- readFile "input.txt"
    let num = lines text
    let k = map read num :: [Integer]

    let indicator new acc = case (new < fst acc) of  -- reverse sense because of foldr
                            True -> (new, 1 + snd acc)
                            False -> (new, snd acc)

    let res = foldr indicator (0,0) k
    print res

    -- we could also have done this by zipping the list with a delayed version of the list and mapping (<) over it