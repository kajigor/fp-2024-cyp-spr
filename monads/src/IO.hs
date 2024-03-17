module IO where

getLineInner :: String -> IO String
getLineInner str = do
    chr <- getChar
    case chr of
        '\n' -> return str
        x -> getLineInner (str ++ [x])


myGetLine :: IO String
myGetLine = getLineInner []


-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO ()
helloUser = do
    putStrLn "What is your name?"
    name <- myGetLine
    putStrLn ("Hello, " ++ name)

-- Use interact in helloUser.
helloUser' :: IO ()
helloUser' = do
    putStrLn "What is your name?"
    interact ("Hello, " ++)