module Main where

import Control.Monad (unless)
import Text.Printf (printf)


countAgeOn planetOn timeInSeconds [] = error ("Planet " ++ planetOn ++ " does not exists ;(")

countAgeOn planetOn timeInSeconds ((planetName, planetModifier, isPlanet):t) =
  if (planetOn == planetName) then
    if (isPlanet) then timeInSeconds / 31557600 / planetModifier
    else error (planetOn ++ " is not a planet anymore ;(")
  else countAgeOn planetOn timeInSeconds t


ageOn :: String -> Float -> Float
ageOn planet ageInSeconds =
  let mercuryInfo = ("Mercury", 0.2408467, True)
      venusInfo = ("Venus", 0.61519726, True)
      earthInfo = ("Earth", 1, True)
      marsInfo = ("Mars", 1.8808158, True)
      jupiterInfo = ("Jupiter", 11.862615, True)
      saturnInfo = ("Saturn", 29.447498, True)
      uranusInfo = ("Uranus", 84.016846, True)
      neptuneInfo = ("Neptune", 164.79132, True)
      plutoInfo = ("Pluto", 1, False)
  in countAgeOn planet ageInSeconds [mercuryInfo, venusInfo, earthInfo, marsInfo, jupiterInfo, saturnInfo, uranusInfo, neptuneInfo, plutoInfo]


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
