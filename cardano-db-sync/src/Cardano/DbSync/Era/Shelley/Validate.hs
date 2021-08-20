{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Validate
  ( validateEpochRewardsBefore
  ) where

import           Cardano.Prelude hiding (from, on)

import           Cardano.BM.Trace (Trace, logError)

<<<<<<< HEAD
=======
import           Cardano.Db (DbLovelace, RewardSource)
>>>>>>> 52fa1f81 (db: Add PersistField RewardSource)
import qualified Cardano.Db as Db

import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

<<<<<<< HEAD
import           Database.Esqueleto.Legacy (Value (..), from, select, sum_, val, where_, (==.),
                   (^.))
=======
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), from, on, select, sum_, val,
                   where_, (==.), (^.))
>>>>>>> 52fa1f81 (db: Add PersistField RewardSource)

import           Database.Persist.Sql (SqlBackend)


validateEpochRewardsBefore
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo
    -> ReaderT SqlBackend m ()
validateEpochRewardsBefore tracer epochNo = do
  actual <- queryEpochRewardTotal epochNo
  unless (actual == 0) $ do
    mExpected <- queryEpochRewardTotalReceived epochNo
    case mExpected of
      Nothing ->
        liftIO . logError tracer $ mconcat
                    [ "validateEpochRewardsBefore: no expected total for rewards earned in epoch "
                    , textShow (unEpochNo epochNo)
                    ]
      Just expected ->
        when (actual /= expected) .
          liftIO .
            logError tracer $ mconcat
                [ "validateEpochRewardsBefore: rewards earned in epoch "
                , textShow (unEpochNo epochNo), " expected total of ", textShow expected
                , " ADA but got " , textShow actual, " ADA"
                ]

-- -------------------------------------------------------------------------------------------------

queryEpochRewardTotal
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m Db.Ada
queryEpochRewardTotal (EpochNo epochNo) = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardEarnedEpoch ==. val epochNo)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumAda (listToMaybe res)

queryEpochRewardTotalReceived
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Maybe Db.Ada)
queryEpochRewardTotalReceived (EpochNo epochNo) = do
  res <- select . from $ \ ertr -> do
            where_ (ertr ^. Db.EpochRewardTotalReceivedEarnedEpoch==. val epochNo)
            pure (ertr ^. Db.EpochRewardTotalReceivedAmount)
  pure $ Db.word64ToAda . Db.unDbLovelace . unValue <$> listToMaybe res
-- -------------------------------------------------------------------------------------------------

convertRewardMap :: Network -> Map (Ledger.StakeCredential c) Coin -> Map Generic.StakeCred Coin
convertRewardMap nw = Map.mapKeys (Generic.toStakeCred nw)


logFullRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Map Generic.StakeCred Coin -> ReaderT SqlBackend m ()
logFullRewardMap epochNo ledgerMap = do
    dbMap <- queryRewardMap epochNo
    liftIO $ diffRewardMap dbMap ledgerMap


queryRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Map Generic.StakeCred (RewardSource, DbLovelace))
queryRewardMap (EpochNo epochNo) = do
    res <- select . from $ \ (rwd `InnerJoin` saddr) -> do
              on (rwd ^. Db.RewardAddrId ==. saddr ^. Db.StakeAddressId)
              where_ (rwd ^. Db.RewardEarnedEpoch ==. val epochNo)
              pure (saddr ^. Db.StakeAddressHashRaw, rwd ^. Db.RewardType, rwd ^.Db.RewardAmount)
    pure $ Map.fromList (map convert res)
  where
    convert :: (Value ByteString, Value RewardSource, Value DbLovelace) -> (Generic.StakeCred, (RewardSource, DbLovelace))
    convert (Value cred, Value source, Value amount) = (Generic.StakeCred cred, (source, amount))


diffRewardMap :: Map Generic.StakeCred (RewardSource, DbLovelace) -> Map Generic.StakeCred Coin -> IO a
diffRewardMap dbMap ledgerMap = do
    putStrLn $ "dbMap: " ++ show (length $ Map.keys dbMap)
    putStrLn $ "ledgerMap: " + show (length $ Map.keys ledgerMap)

    panic "diffRewardMap"
  where
    keys :: [Generic.StakeCred]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map Generic.StakeCred (RewardSource, DbLovelace, Coin)
    diffMap = List.foldl' check mempty keys

