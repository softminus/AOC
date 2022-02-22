import Control.Monad.State

main = do
    text <- readFile "input.txt"
    let split_lines = lines text

    let arsed = map (break (' '==)) split_lines
    let flines = filter (\x -> fst x == "forward") arsed
    let forwards = map (\x -> read $ snd x) flines :: [Integer]
    let sum_forward = foldr (+) 0 forwards 

    let dlines = filter (\x -> or [fst x == "up", fst x == "down"]) arsed

    let depth_parsed = map (\x -> (fst x, read $ snd x)) dlines :: [(String, Integer)]

    let depth_outcome = foldl (\acc ele -> acc + snd ele * case (fst ele) of
                                                            "up" -> 1
                                                            "down" -> (-1)) 0 depth_parsed
    putStrLn $ "forward sum: " ++ show sum_forward
    putStrLn $ "depth sum: " ++ show depth_outcome

    print (sum_forward * depth_outcome)