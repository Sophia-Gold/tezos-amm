{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Math.LogAvg where

import Prelude hiding ((>>), drop, swap, get, abs, map)

import Lorentz

import Lorentz.Contracts.Math.Util
import Lorentz.Contracts.Math.Log (log')
import Lorentz.Contracts.Math.Pow (pow')

logAvg :: ContractCode [(Natural, Natural)] Natural
logAvg =
  do
    unpair
    dip drop
    dup
    map $ do
      unpair
      log' -- compute log2
      mul -- mul by weight
    dip $ push @Integer 0
    iter add
    dip $ do
      map $ do
        unpair
        drop
      dip $ push @Natural 0
      iter add -- sum weights
    edivNeq0 -- normalize weight
    dip $ drop -- round down
    pow' -- raise to 2^x
    nil; pair
