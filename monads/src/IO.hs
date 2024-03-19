module IO where 

-- Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String 
myGetLine = getChar >>= \c -> if c == '\n' then return [] else myGetLine >>= \ne -> return (c : ne) 

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO () 
helloUser = putStr "Enter your name: " >> myGetLine >>= \name -> putStrLn $ "Hello, " ++ name


-- Use interact in helloUser.
helloUser' :: IO () 
helloUser' = putStr "Enter your name: " >> interact ("Hello, " ++)