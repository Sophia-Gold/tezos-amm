{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Factory where

import Lorentz hiding (concat)

import Prelude hiding ((>>), and, show, unwords, swap, drop, some, get)


createContractFromStorage :: forall cp st s. (NiceParameterFull cp, NiceStorage st)
  => ContractCode cp st
  -> st & s :-> Operation & Address & s
createContractFromStorage targetContract = do
  push $ toEnum @Mutez 0
  none
  createContract targetContract

newtype OriginationInfo = OriginationInfo
  { initialStorageHash :: ByteString }
  deriving Show
  deriving Generic
  deriving anyclass IsoValue

unOriginationInfo :: forall s. OriginationInfo & s :-> ByteString & s
unOriginationInfo = forcedCoerce_

toOriginationInfo :: forall s. ByteString & s :-> OriginationInfo & s
toOriginationInfo = forcedCoerce_

storageToOriginationInfo :: forall st s. NicePackedValue st => st & s :-> OriginationInfo & s
storageToOriginationInfo = do
  pack
  blake2B
  toOriginationInfo

data Parameter st
  = ProduceContract !(View st Address)
  | GetOriginationInfo !(View Address (Maybe OriginationInfo))
  deriving  (Generic)

deriving instance Show st => Show (Parameter st)

deriving instance IsoValue st => IsoValue (Parameter st)

type Storage = BigMap Address OriginationInfo

unView :: forall a b s. View a b & s :-> (a, ContractRef b) & s
unView = forcedCoerce_

factoryContract :: forall cp st. (NiceParameterFull cp, NicePackedValue st, NiceStorage st)
  => ContractCode cp st
  -> ContractCode (Parameter st) Storage
factoryContract targetContract = do
  unpair
  caseT @(Parameter st)
    ( #cProduceContract /-> produceContract targetContract
    , #cGetOriginationInfo /-> getOriginationInfo
    )

sendAddress :: forall s'. ContractRef Address & Address & [Operation] & s' :-> Address & [Operation] & s'
sendAddress = do
  swap
  dip $ push $ toEnum @Mutez 0
  dup
  dip $ do
    transferTokens
    cons

produceContract :: forall cp st s. (NiceParameterFull cp, NicePackedValue st, NiceStorage st)
  => ContractCode cp st
  -> View st Address & Storage & s :-> ([Operation], Storage) & s
produceContract targetContract = do
  unView
  unpair
  dup
  storageToOriginationInfo
  dip $ do
    swap
    dip $ do
      dip nil
      createContractFromStorage targetContract
      swap
      dip cons
    sendAddress
    swap
  swap
  dip $ do
    some
    swap
    update
  pair

getOriginationInfo :: forall s. ()
  => View Address (Maybe OriginationInfo) & Storage & s :-> ([Operation], Storage) & s
getOriginationInfo = do
  view_ $ do
    unpair
    get
