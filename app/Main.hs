{-
  N.B.: This is just a simple example of a Haskell program. Don't
  take BMI seriously. You're fine just the way you are!
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

data UnitSystem = Imperial | SI
  deriving Show

data HeightUnit = Inches Double | Centimeters Double | Meters Double | Mixed (Double,Double)
  deriving Show

data WeightUnit = Pounds Double | Kilograms Double | Stone Double
  deriving Show

class SILength a where
  getMeters :: a -> Double

class SIMass a where
  getKilograms :: a -> Double

instance SILength HeightUnit where
  getMeters (Inches n)         = n * 0.0254
  getMeters (Centimeters n)    = n / 100.0
  getMeters (Meters n)         = n
  getMeters (Mixed (hft, hin)) = getMeters $ Inches (hft * 12 + hin)

instance SIMass WeightUnit where
  getKilograms (Pounds n)    = n / 2.2
  getKilograms (Kilograms n) = n
  getKilograms (Stone n)     = n * 6.35029

strToDouble s = read s :: Double

formsDigit c = isDigit c || c == '.'

chopUnits = takeWhile formsDigit

splitMixed s = (f,i)
  where f = chopUnits s
        i = chopUnits $ last $ splitOn "ft" s

strToMeters s
  | "ft" `isInfixOf`  s = Meters $ getMeters $ getMetersMixed $ splitMixed s
  | "in" `isSuffixOf` s = Meters $ getMeters $ Inches s'
  | "cm" `isSuffixOf` s = Meters $ getMeters $ Centimeters s'
  | otherwise           = Meters $ getMeters $ Inches $ strToDouble s
  where s' = strToDouble $ chopUnits s
        getMetersMixed (fStr, iStr) = Mixed (strToDouble fStr, strToDouble iStr)

strToKilograms s
  | "lbs" `isSuffixOf` s = Kilograms $ getKilograms $ Pounds s'
  | "st" `isSuffixOf`  s = Kilograms $ getKilograms $ Stone s'
  | "kg" `isSuffixOf`  s = Kilograms s'
  | otherwise            = Kilograms $ getKilograms $ Pounds $ strToDouble s
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

doBmi units = do
  let (htStr:wtStr:_) = units
      height = strToMeters htStr
      weight = strToKilograms wtStr
      bmiNum = bmiValue weight height
      bmiType = bmi bmiNum
  putStr $ printf "%.2f - " bmiNum
  print bmiType

main = do
  argv <- getArgs
  case parseArgs argv of
    ([], units, []) -> if (length units) == 2 then doBmi units else die 2
    (opts, _, [])   -> die 1
    (_, _, errs)    -> die 255
