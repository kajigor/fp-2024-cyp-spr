module IO where 
import Text.Printf (printf)

-- Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String 
myGetLine = getChar >>= (\c -> if c == '\n' then return "" else myGetLine >>= (\rest -> return (c:rest)))

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO () 
helloUser = putStrLn "What is your name?" >>= (\_ -> myGetLine >>= printf "Hello, %s\n")

-- Use interact in helloUser.
helloUser' :: IO () 
helloUser' = putStrLn "What is your name?" >>= (\ _ -> interact (printf "Hello, %s\n")) 