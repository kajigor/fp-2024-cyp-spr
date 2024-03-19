module IO where 

-- Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String 
myGetLine = getLineHelper
    where getLineHelper :: IO String
          getLineHelper = do
                c <- getChar
                if c == '\n'
                    then return []
                    else (c:) <$> getLineHelper -- what have I created
                        


-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO () 
helloUser = do
    putStrLn "What is your name?"
    name <- myGetLine
    putStrLn ("Hello, " ++ name ++ ".")


-- Use interact in helloUser.
helloUser' :: IO () 
helloUser' = do 
    putStrLn "What is your name?"
    interact (\name -> "Hello" ++ name ++ ".")