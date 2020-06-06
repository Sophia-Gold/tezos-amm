{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Math.Pow where

import Prelude hiding ((>>), drop, swap, get, abs)
import qualified Prelude (drop)

import Lorentz

import Lorentz.Contracts.Math.Util

import Numeric.Approximation.Chebyshev (cheby_approx)


invert :: (Natural & s) :-> (Natural & s)
invert =
  do
    push oneFix
    edivNeq0
    dip $ drop

powInt :: (Natural & s) :-> (Natural & s)
powInt =
  do
    push @Natural 256
    swap
    edivNeq0
    int
    dip $ push oneFix
    push True
    loop $ do
      dup
      ifEq0 (push False) $ do
        dip $ push @Natural 1
        sub
        dip $ do
          dip $ push @Natural 256
          lsl
        push True
    drop
    lsl

powFrac :: (Natural & s) :-> (Natural & s)
powFrac =
  let -- coeffs = (\x -> round (x*2^(p*2)) :: Natural) <$> reverse (Prelude.drop 1 (cheby_approx (\x -> 2.0**x) 0.0 1.0 8))
      -- coeffs = [ 5676, 65510, 661577, 5726720, 41309550, 238388332, 1031764991, 2977044472 ] :: List Natural
      coeffs = (\x -> round (x*2^(p*2)) :: Natural) <$> reverse [ 0.69314718055994530941723212145817656807, 0.24022650695910071233355126316333248586, 0.055504108664821579953142263768621757359, 0.0096181291076284771619790715736588654799, 0.0013333558146428443423412221987996174731, 0.00015403530393381609954437097332742347961, 0.000015252733804059840280025439012009638170, 0.0000013215486790144309488403758228288360756 ]
      mulFixP2 = mulFix (p*2)
      oneFixP2 = 2^(p*2) :: Natural
  in do
    dip $ push pNat
    lsl
    push @Natural 0
    push coeffs
    iter $ do
      add
      dip dup
      mulFixP2
    dip drop
    push oneFixP2
    add
    dip $ push pNat
    lsr

powAux :: (Natural & s) :-> (Natural & s)
powAux =
  do
    dip $ push oneFix
    edivNeq0
    powInt
    dip powFrac
    dup
    int
    ifEq0 drop $ mulFix p

pow' :: (Integer & s) :-> (Natural & s) 
pow' =
  do
    dup
    ifEq0
      (do
          drop
          push oneFix
      ) $ do
      dup
      ifGt0
        (do
            abs
            powAux
        ) $ do
        abs
        powAux
        invert

pow :: ContractCode Integer Natural
pow =
  do
    unpair
    dip drop
    pow'
    nil; pair
