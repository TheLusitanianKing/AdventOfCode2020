main :: IO ()
main = do
    content <- getContents
    let inp = map (read :: String -> Int) . words $ content
    putStrLn . show . head $ [(i, j, i*j) | i <- inp, j <- inp, i + j == 2020]
    putStrLn . show . head $ [(a, b, c, a*b*c) | a <- inp, b <- inp, c <- inp, a + b + c == 2020]