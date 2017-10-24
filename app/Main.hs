{-
   Copyright (c) 2016 Michael Dippery <michael@monkey-robot.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import Data.Char (isDigit)
import Data.List (isInfixOf, isSuffixOf)
import Data.List.Split (splitOn)
import System.Console.GetOpt (ArgOrder(..), getOpt)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import Text.Printf (printf)

data BMI = SeverelyUnderweight
         | Underweight
         | Normal
         | Overweight
         | Obese1
         | Obese2
         | Obese3

instance Show BMI where
  show SeverelyUnderweight = "Severely underweight"
  show Underweight         = "Underweight"
  show Normal              = "Normal"
  show Overweight          = "Overweight"
  show Obese1              = "Obese (Class I)"
  show Obese2              = "Obese (Class II)"
  show Obese3              = "Obese (Class III)"

data HeightUnit = Inches Double | Centimeters Double | Meters Double | Mixed (Double,Double)
  deriving Show

data WeightUnit = Pounds Double | Kilograms Double | Stone Double
  deriving Show

class SILength a where
  toMeters :: a -> Double

class SIMass a where
  toKilograms :: a -> Double

instance SILength HeightUnit where
  toMeters (Inches n)         = n * 0.0254
  toMeters (Centimeters n)    = n / 100.0
  toMeters (Meters n)         = n
  toMeters (Mixed (hft, hin)) = toMeters $ Inches (hft * 12 + hin)

instance SIMass WeightUnit where
  toKilograms (Pounds n)    = n / 2.2
  toKilograms (Kilograms n) = n
  toKilograms (Stone n)     = n * 6.35029

strToDouble s = read s :: Double

formsDigit c = isDigit c || c == '.'

chopUnits = takeWhile formsDigit

splitMixed s = (f,i)
  where f = chopUnits s
        i = chopUnits $ last $ splitOn "ft" s

strToMeters s
  | "ft" `isInfixOf`  s = Meters $ toMeters $ toMetersMixed $ splitMixed s
  | "in" `isSuffixOf` s = Meters $ toMeters $ Inches s'
  | "cm" `isSuffixOf` s = Meters $ toMeters $ Centimeters s'
  | otherwise           = Meters $ toMeters $ Inches $ strToDouble s
  where s' = strToDouble $ chopUnits s
        toMetersMixed (fStr, iStr) = Mixed (strToDouble fStr, strToDouble iStr)

strToKilograms s
  | "lbs" `isSuffixOf` s = Kilograms $ toKilograms $ Pounds s'
  | "st" `isSuffixOf`  s = Kilograms $ toKilograms $ Stone s'
  | "kg" `isSuffixOf`  s = Kilograms s'
  | otherwise            = Kilograms $ toKilograms $ Pounds $ strToDouble s
  where s' = strToDouble $ chopUnits s

bmiValue (Kilograms weight) (Meters height) = weight / (height ^ 2)

bmi n
  | n <= 16.5 = SeverelyUnderweight
  | n <= 18.5 = Underweight
  | n <= 25.0 = Normal
  | n <= 30.1 = Overweight
  | n <= 35.0 = Obese1
  | n <= 40.0 = Obese2
  | otherwise = Obese3

usage = do
  progName <- getProgName
  putStrLn $ printf "Usage: %s height weight" progName
  putStrLn "  height may be in:"
  putStrLn "    inches      - 62 or 62in"
  putStrLn "    mixed       - 5ft2in"
  putStrLn "    centimeters - 157.48cm"
  putStrLn "  weight may be in:"
  putStrLn "    pounds    - 120 or 120lbs"
  putStrLn "    kilograms - 54.43kg"

die code = do
  usage
  exitWith (ExitFailure code)

parseArgs = getOpt RequireOrder []

doBmi height weight = do
  let ht = strToMeters height
      wt = strToKilograms weight
      bmiNum = bmiValue wt ht
      bmiType = bmi bmiNum
  putStr $ printf "%.2f - " bmiNum
  print bmiType

main = do
  argv <- getArgs
  case parseArgs argv of
    ([], (h:w:_), []) -> doBmi h w
    ([], _, [])       -> die 2
    (opts, _, [])     -> die 1
    (_, _, errs)      -> die 255
