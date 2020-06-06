{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Math.Util where

import Prelude hiding ((>>), drop, swap, get, abs)

import Lorentz
import Michelson.Typed.Haskell


p :: Int
p = 16

pNat :: Natural
pNat = fromIntegral p

oneFix :: Natural
oneFix = 2^p

twoFix :: Natural
twoFix = 2^p*2

mulFix :: Int -> (Natural & (Natural & s)) :-> (Natural & s)
mulFix p =
  let pNat = fromIntegral p :: Natural
  in do
    mul
    dip $ push pNat
    lsr

data DivideByZero = DivideByZero
  deriving stock Generic
  deriving anyclass IsoValue

instance IsError DivideByZero where
  errorToVal = isoErrorToVal
  errorFromVal = isoErrorFromVal

instance ErrorHasDoc DivideByZero where
  errorDocName = "Divide by zero"
  errorDocMdCause = "Divide by zero"
  errorDocHaskellRep = typeDocMdReference (Proxy @()) (WithinParens False) <> "."
  errorDocDependencies = typeDocDependencies' (Proxy @())

edivNeq0 :: EDivOpHs a b =>
            (a & (b & s)) :-> (EDivOpResHs a b & (EModOpResHs a b & s))
edivNeq0 =
  do
    ediv
    assertSome $ DivideByZero
    unpair
