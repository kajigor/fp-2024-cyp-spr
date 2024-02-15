module Main where

import Control.Monad (unless)
import Text.Printf (printf)

ageOn :: String -> Float -> Float
ageOn planet ageInSeconds
  | planet == "Mercury" = ageInSeconds / 31557600 / 0.2408467
  | planet == "Venus" = ageInSeconds / 31557600 / 0.61519726
  | planet == "Earth" = ageInSeconds / 31557600
  | planet == "Mars" = ageInSeconds / 31557600 / 1.8808158
  | planet == "Jupiter" = ageInSeconds / 31557600 / 11.862615
  | planet == "Saturn" = ageInSeconds / 31557600 / 29.447498
  | planet == "Uranus" = ageInSeconds / 31557600 / 84.016846
  | planet == "Neptune" = ageInSeconds / 31557600 / 164.79132
  | planet == "Pluto" = error ("Pluto is not planet anymore ;(")
  | otherwise = error (planet ++ "is not a correct state of planet")


isLeapYear :: Int -> Bool
isLeapYear year =
  if year < 0 then error "Year could not be less than 0." else
    if (year `mod` 400 == 0) then True else
      if (year `mod` 100 == 0) then False else
        if (year `mod` 4 == 0) then True else False


main = do 
  runTests
  putStrLn "Done"

runTests = do
    runAgeOnTests
    runIsLeapYearTests
  where
    describeFailure functionName errorMsg input exp actual = 
      printf "Test for a function %s has failed:\n  %s\n  Input: %s\n  Expected: %s\n  But got: %s\n" 
             functionName 
             errorMsg 
             (show input) 
             (show exp) 
             (show actual)

    runAgeOnTests =
        mapM_ test cases
      where
        test (planet, seconds, exp) = 
            let actual = ageOn planet seconds in 
            unless (actual `isEqual` exp) $ describeFailure "ageOn" (printf "Wrong age on planet %s" planet :: String) seconds exp actual
          where 
            isEqual x y = roundTo 2 x == roundTo 2 y
            roundTo n = (/ 10 ^ n) . fromIntegral . round . (* 10 ^ n) 
        cases = [ ( "Earth"
                  , 1000000000
                  , 31.69
                  )
                , ( "Mercury"
                  , 2134835688
                  , 280.88
                  )
                , ( "Venus"
                  , 189839836
                  , 9.78
                  )
                , ( "Mars"
                  , 2129871239
                  , 35.88
                  )
                , ( "Jupiter"
                  , 901876382
                  , 2.41
                  )
                , ( "Saturn"
                  , 2000000000
                  , 2.15
                  )
                , ( "Uranus"
                  , 1210123456
                  , 0.46
                  )
                , ( "Neptune"
                  , 1821023456
                  , 0.35
                  )
                ]

    runIsLeapYearTests =
        mapM_ test cases
      where
        test (errorMsg, input, exp) =
          let actual = isLeapYear input in 
          unless (actual == exp) $ describeFailure "isLeapYear" errorMsg input exp actual
         
        cases = [ ( "year not divisible by 4 in common year"
                  , 2015
                  , False
                  )
                , ( "year divisible by 2, not divisible by 4 in common year"
                  , 1970
                  , False
                  )
                , ( "year divisible by 4, not divisible by 100 in leap year"
                  , 1996
                  , True
                  )
                , ( "year divisible by 4 and 5 is still a leap year"
                  , 1960
                  , True
                  )
                , ( "year divisible by 100, not divisible by 400 in common year"
                  , 2100
                  , False
                  )
                , ( "year divisible by 100 but not by 3 is still not a leap year"
                  , 1900
                  , False
                  )
                , ( "year divisible by 400 in leap year"
                  , 2000
                  , True
                  )
                , ( "year divisible by 400 but not by 125 is still a leap year"
                  , 2400
                  , True
                  )
                , ( "year divisible by 200, not divisible by 400 in common year"
                  , 1800
                  , False
                  )
                ]
