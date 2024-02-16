# HW01
## Deadline: beginning of the class on Friday

1. Given an age in seconds, calculate how old (in years) someone would be on a different planet. Implement a function `ageOn planet ageInSeconds` which returns the age in years represented as a floating point number of type `Float`. Planet name is a `String`, case sensitive. An error should be reported if an unrecognised planet is passed into. If someone tried to find their age on Pluto, explain to them that it's not a planet. 

   * Mercury: orbital period 0.2408467 Earth years
   * Venus: orbital period 0.61519726 Earth years
   * Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
   * Mars: orbital period 1.8808158 Earth years
   * Jupiter: orbital period 11.862615 Earth years
   * Saturn: orbital period 29.447498 Earth years
   * Uranus: orbital period 84.016846 Earth years
   * Neptune: orbital period 164.79132 Earth years

2. Given a year, report if it is a leap year (function `isLeapYear year`). A leap year in the Gregorian calendar occurs:

   * on every year that is evenly divisible by 4
   * except every year that is evenly divisible by 100
   * unless the year is also evenly divisible by 400

   For example, 1997 is not a leap year, but 1996 is. 1900 is not a leap year, but 2000 is. Report an error, if the year is negative. 

## Notes 

* Make a fork of this repository and checkout the branch `HW01`.
* Write your code in the `Main.hs` file: replace `undefined` with your definitions.
* Make sure that your tests pass, i.e. running `main` (or `./Main`) only outputs `Done`.
* If you need more tests, add them, but don't delete the ones which are already in the file. 
* When finished, open a pull request into the main repo. Make sure to put your name in the title of the PR.  