module Main where

import Data.List ( elemIndex )
import System.Environment

brightnessValues :: Integral a => [ a ]
brightnessValues =
  0 : takeWhile (<100) (lvls 1) ++ [100]
  where
    lvls n = n : lvls (floor.(max 2).exp.(+0.5).log.fromIntegral $ n)

(|>) = flip (.)

takeWhileFrom = flip takeWhile
capAt low high = (max low) . (min high)

main = do
  [ arg1 ] <- getArgs
  let incBy = read arg1
  interact
    $  read
    |> round
    |> ((>) |> takeWhileFrom brightnessValues |> length)
    |> (+ incBy)
    |> (capAt 0 $ length brightnessValues - 1)
    |> (brightnessValues !!)
    |> show

