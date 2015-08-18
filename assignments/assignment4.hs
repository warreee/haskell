import Control.Monad

main1 = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    mapM_ print $ replicate x y
    putStrLn "Alternative: "
    replicateM_ x (print y)
    
main2 = do
    ln <- getLine
    if ln == "" then return () else do 
        print $ reverse ln 
        main2        
