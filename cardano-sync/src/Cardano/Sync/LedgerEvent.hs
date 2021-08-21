{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Sync.LedgerEvent
  ( LedgerEvent (..)
  , convertAuxLedgerEvent
  ) where

import           Cardano.Db
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Slotting.Slot (EpochNo (..))
import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Types

import           Control.State.Transition (Event)

import           Data.Map.Strict (Map)
import           Data.SOP.Strict (All, K (..), Proxy (..), hcollapse, hcmap)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import           Ouroboros.Consensus.Ledger.Monad (AuxLedgerEvent)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraLedgerEvent)

import           Shelley.Spec.Ledger.STS.NewEpoch (NewEpochEvent (..))
import           Shelley.Spec.Ledger.STS.Tick (TickEvent (..))

data LedgerEvent
  = LedgerNewEpoch !EpochNo !SyncState
  | LedgerStartAtEpoch !EpochNo
  | LedgerRewards !SlotDetails !Generic.Rewards
  | LedgerStakeDist !Generic.StakeDist

  | LedgerRewardDist !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) Coin)
  deriving Eq

convertAuxLedgerEvent :: OneEraLedgerEvent (CardanoEras StandardCrypto) -> Maybe LedgerEvent
convertAuxLedgerEvent = toLedgerEvent . wrappedAuxLedgerEvent

wrappedAuxLedgerEvent
    :: OneEraLedgerEvent (CardanoEras StandardCrypto)
    -> WrapLedgerEvent (HardForkBlock (CardanoEras StandardCrypto))
wrappedAuxLedgerEvent =
  WrapLedgerEvent @(HardForkBlock (CardanoEras StandardCrypto))

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance
  ( Crypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
  ) =>
  ConvertLedgerEvent (ShelleyBlock ledgerera)
  where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LESumRewards e m -> Just $ LedgerRewardDist e m
      ShelleyLedgerEventBBODY {} -> Nothing
      ShelleyLedgerEventTICK {} -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------
-- Patterns for event access
--------------------------------------------------------------------------------

pattern LESumRewards
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) Coin
    -> AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LESumRewards e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (SumRewards e m))
