module IO where

-- Implement reading line from standard input.
-- Use getChar to read a single character.
myGetLine :: IO String
myGetLine = do
    c <- getChar
    if c == '\n' then
        return ""
    else do
        cs <- myGetLine
        return (c:cs)

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO ()
helloUser = do
        putStrLn "What's your name?"
        name <- myGetLine
        putStrLn $ "Hello, " ++ name

-- Use interact in helloUser.
helloUser' :: IO ()
helloUser' = do
        putStrLn "What's your name?"
        interact ("Hello, " ++)