module IO where 

-- Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String 
myGetLine = do
    s <- getChar
    if (s == '\n') then
        return []
    else
        x <- myGetLine
        return s : x 

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO () 
helloUser = do
    putStrLn "Hello, what is your name?\n"
    name <- myGetLine
    putStrLn "Hello, " ++ name 

-- Use interact in helloUser.
helloUser' :: IO () 
helloUser' = do
    putStrLn "Hello, what is your name?\n"
    interact ("Hello, " ++) 