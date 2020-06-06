{-# OPTIONS -Wno-orphans #-}

module Main
  ( main
  ) where

import qualified Data.Text.Lazy.IO as TL

import qualified Lorentz as L

import Lorentz.Contracts.Math.LogAvg (logAvg)
import Lorentz.Contracts.Math.Log (log)
import Lorentz.Contracts.Math.Pow (pow)


main :: IO ()
main =
  do
    TL.writeFile "contracts/logavg.tz" $
      L.printLorentzContract False logAvg
    TL.writeFile "contracts/log2fix.tz" $
      L.printLorentzContract False log
    TL.writeFile "contracts/pow2fix.tz" $
      L.printLorentzContract False pow
