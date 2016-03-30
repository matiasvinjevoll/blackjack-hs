module PrintUtils ( printText ) where 

printText :: [String] -> IO ()
printText xs = mapM_ putStrLn xs
