module Click (click, longClick) where
    import System.Process

    click :: Int -> Int -> IO ()
    click x y = do
        r <- readProcess "python" ["Click.py", "click", "x=" ++ show x, "y=" ++ show y] ""
        putStrLn r

    longClick :: Int -> Int -> IO ()
    longClick x y = do
        r <- readProcess "python" ["Click.py", "long_click", "x=" ++ show x, "y=" ++ show y] ""
        putStrLn r
