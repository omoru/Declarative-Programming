 
hw = putStrLn "Hello"
hw :: IO ()

--Not a function, is a IO  accion
greet :: IO ()
greet = do
    putStrLn "Hwats ur name"
    name <- getLine -- getLine is a IO action
    putStrLn ("hELLO" ++ name)


--ERROR
F :: IO a -> a
--unsafePerformIO :: IO a -> a  no se debe usar, puede trarer comportamientos qu eno queremos

main :: IO ()
main = do
    i <- getLine
    if i /= "quit" then do
        putStrLn ("INPUT:" ++ i)
        main
    else
        return () --the value encapsulated in the IO action

count :: Int -> Int -> IO ()
count n m = do
    putStrLn (show n)
    if n < m then
        count (n+1) m
    else
        return ()