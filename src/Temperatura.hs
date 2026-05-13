module Temperatura (main, converteTemperatura) where

import Data.Maybe (fromJust, isNothing)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

converteTemperatura :: Double -> Maybe (Double, Double)
converteTemperatura celsius
  | celsius < -273.15 = Nothing
  | otherwise         = Just (fahrenheit, kelvin)
  where
    fahrenheit = celsius * 1.8 + 32
    kelvin = celsius + 273.15

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Digite a temperatura em Celsius: "
  entrada <- getLine
  let maybeCelsius = readMaybe entrada :: Maybe Double
  if isNothing maybeCelsius
    then 
      putStrLn "Entrada inválida."
    else do
      let celsius = fromJust maybeCelsius
      let maybeTemps = converteTemperatura celsius
      if isNothing maybeTemps
        then 
          putStrLn "Entrada Inválida."
        else do
          let (f, k) = fromJust maybeTemps
          printf "Temperatura em Fahrenheit: %.2f\n" f
          printf "Temperatura em Kelvin: %.2f\n" k
          