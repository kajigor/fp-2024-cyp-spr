module IO where

-- Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        s <- myGetLine
        return (x : s)

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO ()
helloUser = do
    putStrLn "Enter your name:"
    name <- myGetLine
    putStrLn ("Hello, " ++ name)

-- Use interact in helloUser.
helloUser' :: IO ()
helloUser' = interact ("Hello, " ++)