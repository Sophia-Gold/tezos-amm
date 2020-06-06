{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Math.Log where

import Prelude hiding ((>>), drop, swap, get, abs)

import Lorentz
import Michelson.Text (mkMTextUnsafe)

import Lorentz.Contracts.Math.Util


normalizeGe1 :: (Natural & (Integer & s)) :-> (Natural & (Integer & s))
normalizeGe1 =
  do
    push True
    loop $ do
      dup
      dip $ push oneFix
      ifGe (push False) $ do
        dip $ push @Natural 1
        lsl
        dip $ do
          dip $ push oneFix
          sub
        push True

normalizeLt2 :: (Natural & (Integer & s)) :-> (Natural & (Integer & s))
normalizeLt2 =
  do
    push True
    loop $ do
      dup
      dip $ push twoFix
      ifLt (push False) $ do
        dip $ push @Natural 1
        lsr
        dip $ do
          dip $ push oneFix
          add
        push True

normalizeLt2' :: (Natural & (Integer & (Natural & s))) :->
                 (Natural & (Integer & (Natural & s)))
normalizeLt2' =
  do
    dup
    dip $ push twoFix
    ifGt
      (do
          dip $ push @Natural 1
          lsr
          dip $ do
            dip dup
            add
      )
      (do
          dup
          drop
      )

logAux :: (Natural & (Integer & (Natural & s))) :->
          (Natural & (Natural & (Integer & (Natural & s))))
logAux =
  do
    push @Natural 0
    push True
    loop $ do
      dup
      dip $ push pNat
      ifGe (push False) $ do
        push @Natural 1
        add
        dip $ do
          dup
          mulFix p
          normalizeLt2'
          dip $ dip $ do
            dip $ push @Natural 1
            lsr
        push True

log' :: (Natural & s) :-> (Integer & s)
log' =
  let oneHalfFix = fromIntegral 2^p `div` 2 :: Natural
  in do
    dup
    int
    assertNeq0 $ mkMTextUnsafe "Log zero undefined"
    dip $ do
      push oneHalfFix
      push @Integer 0
    normalizeGe1
    normalizeLt2
    logAux
    drop
    drop
    dip drop

log :: ContractCode Natural Integer
log =
  do
    unpair
    dip drop
    log'
    nil; pair
