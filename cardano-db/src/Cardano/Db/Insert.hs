{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertAdaPots
  , insertAdminUser
  , insertBlock
  , insertCollateralTxIn
  , insertDelegation
  , insertEpoch
  , insertEpochParam
  , insertEpochRewardTotalReceived
  , insertEpochSyncTime
  , insertManyEpochStakes
  , insertManyRewards
  , insertMaTxMint
  , insertMaTxOut
  , insertMeta
  , insertParamProposal
  , insertPotTransfer
  , insertPoolHash
  , insertPoolMetadataRef
  , insertPoolOfflineData
  , insertPoolOfflineFetchError
  , insertPoolOwner
  , insertPoolRelay
  , insertPoolRetire
  , insertPoolUpdate
  , insertReserve
  , insertReservedPoolTicker
  , insertScript
  , insertSlotLeader
  , insertStakeAddress
  , insertStakeDeregistration
  , insertStakeRegistration
  , insertTreasury
  , insertTx
  , insertTxIn
  , insertTxMetadata
  , insertTxOut
  , insertWithdrawal
  , insertRedeemer
  , insertCostModels

  -- Export mainly for testing.
  , insertBlockChecked
  , insertCheckUnique
  , insertManyUncheckedUnique
  , insertUnchecked
  ) where


import           Control.Exception.Lifted (Exception, handle, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Class (AtLeastOneUniqueKey, PersistEntityBackend, insert,
                   insertBy, replaceUnique)
import           Database.Persist.EntityDef.Internal (entityDB, entityUniques)
import           Database.Persist.Sql (OnlyOneUniqueKey, PersistRecordBackend, SqlBackend,
                   UniqueDef, entityDef, rawExecute, rawSql, toPersistFields, toPersistValue,
                   uniqueDBName)
import qualified Database.Persist.Sql.Util as Util
import           Database.Persist.Types (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..),
                   PersistValue, entityKey)
import           Database.PostgreSQL.Simple (SqlError)

import           Cardano.Db.Schema


-- The original naive way of inserting rows into Postgres was:
--
--     insertByReturnKey :: record -> ReaderT SqlBackend m recordId
--        res <- getByValue value
--        case res of
--          Nothing -> insertBy value
--          Just ident -> pure ident
--
-- Unfortunately this is relatively slow if the row is not already found in the database.
--
-- One alternative is to just use `insert` but that fails on some uniquness constraints on some
-- tables (about 6 out of a total of 25+).
--
-- Instead we use `insertUnchecked` for tables where uniqueness constraints are unlikley to be hit
-- and `insertChecked` for tables where the uniqueness constraint might can be hit.

insertAdaPots :: (MonadBaseControl IO m, MonadIO m) => AdaPots -> ReaderT SqlBackend m AdaPotsId
insertAdaPots = insertCheckUnique "AdaPots"

insertAdminUser :: (MonadBaseControl IO m, MonadIO m) => AdminUser -> ReaderT SqlBackend m AdminUserId
insertAdminUser = insertUnchecked "AdminUser"

insertBlock :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlock = insertUnchecked "Block"

insertBlockChecked :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlockChecked = insertCheckUnique "Block"

insertCollateralTxIn :: (MonadBaseControl IO m, MonadIO m) => CollateralTxIn -> ReaderT SqlBackend m CollateralTxInId
insertCollateralTxIn = insertUnchecked "CollateralTxIn"

insertDelegation :: (MonadBaseControl IO m, MonadIO m) => Delegation -> ReaderT SqlBackend m DelegationId
insertDelegation = insertCheckUnique "Delegation"

insertEpoch :: (MonadBaseControl IO m, MonadIO m) => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertUnchecked "Epoch"

insertEpochParam :: (MonadBaseControl IO m, MonadIO m) => EpochParam -> ReaderT SqlBackend m EpochParamId
insertEpochParam = insertUnchecked "EpochParam"

insertEpochRewardTotalReceived :: (MonadBaseControl IO m, MonadIO m) => EpochRewardTotalReceived -> ReaderT SqlBackend m EpochRewardTotalReceivedId
insertEpochRewardTotalReceived = insertCheckUnique "EpochRewardTotalReceived"

insertEpochSyncTime :: (MonadBaseControl IO m, MonadIO m) => EpochSyncTime -> ReaderT SqlBackend m EpochSyncTimeId
insertEpochSyncTime = insertReplace "EpochSyncTime"

insertManyEpochStakes :: (MonadBaseControl IO m, MonadIO m) => [EpochStake] -> ReaderT SqlBackend m ()
insertManyEpochStakes = insertManyUncheckedUnique "Many EpochStake"

insertManyRewards :: (MonadBaseControl IO m, MonadIO m) => [Reward] -> ReaderT SqlBackend m ()
insertManyRewards = insertManyUncheckedUnique "Many Rewards"

insertMaTxMint :: (MonadBaseControl IO m, MonadIO m) => MaTxMint -> ReaderT SqlBackend m MaTxMintId
insertMaTxMint = insertCheckUnique "insertMaTxMint"

insertMaTxOut :: (MonadBaseControl IO m, MonadIO m) => MaTxOut -> ReaderT SqlBackend m MaTxOutId
insertMaTxOut = insertCheckUnique "insertMaTxOut"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertCheckUnique "Meta"

insertParamProposal :: (MonadBaseControl IO m, MonadIO m) => ParamProposal -> ReaderT SqlBackend m ParamProposalId
insertParamProposal = insertUnchecked "ParamProposal"

insertPotTransfer :: (MonadBaseControl IO m, MonadIO m) => PotTransfer -> ReaderT SqlBackend m PotTransferId
insertPotTransfer = insertUnchecked "PotTransfer"

insertPoolHash :: (MonadBaseControl IO m, MonadIO m) => PoolHash -> ReaderT SqlBackend m PoolHashId
insertPoolHash = insertCheckUnique "PoolHash"

insertPoolMetadataRef :: (MonadBaseControl IO m, MonadIO m) => PoolMetadataRef -> ReaderT SqlBackend m PoolMetadataRefId
insertPoolMetadataRef = insertCheckUnique "PoolMetadataRef"

insertPoolOfflineData :: (MonadBaseControl IO m, MonadIO m) => PoolOfflineData -> ReaderT SqlBackend m PoolOfflineDataId
insertPoolOfflineData = insertCheckUnique "PoolOfflineData"

insertPoolOfflineFetchError :: (MonadBaseControl IO m, MonadIO m) => PoolOfflineFetchError -> ReaderT SqlBackend m PoolOfflineFetchErrorId
insertPoolOfflineFetchError = insertCheckUnique "PoolOfflineFetchError"

insertPoolOwner :: (MonadBaseControl IO m, MonadIO m) => PoolOwner -> ReaderT SqlBackend m PoolOwnerId
insertPoolOwner = insertCheckUnique "PoolOwner"

insertPoolRelay :: (MonadBaseControl IO m, MonadIO m) => PoolRelay -> ReaderT SqlBackend m PoolRelayId
insertPoolRelay = insertUnchecked "PoolRelay"

insertPoolRetire :: (MonadBaseControl IO m, MonadIO m) => PoolRetire -> ReaderT SqlBackend m PoolRetireId
insertPoolRetire = insertUnchecked "PoolRetire"

insertPoolUpdate :: (MonadBaseControl IO m, MonadIO m) => PoolUpdate -> ReaderT SqlBackend m PoolUpdateId
insertPoolUpdate = insertCheckUnique "PoolUpdate"

insertReserve :: (MonadBaseControl IO m, MonadIO m) => Reserve -> ReaderT SqlBackend m ReserveId
insertReserve = insertUnchecked "Reserve"

insertReservedPoolTicker :: (MonadBaseControl IO m, MonadIO m) => ReservedPoolTicker -> ReaderT SqlBackend m ReservedPoolTickerId
insertReservedPoolTicker = insertUnchecked "ReservedPoolTicker"

insertScript :: (MonadBaseControl IO m, MonadIO m) => Script -> ReaderT SqlBackend m ScriptId
insertScript = insertCheckUnique "insertScript"

insertSlotLeader :: (MonadBaseControl IO m, MonadIO m) => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertCheckUnique "SlotLeader"

insertStakeAddress :: (MonadBaseControl IO m, MonadIO m) => StakeAddress -> ReaderT SqlBackend m StakeAddressId
insertStakeAddress = insertCheckUnique "StakeAddress"

insertStakeDeregistration :: (MonadBaseControl IO m, MonadIO m) => StakeDeregistration -> ReaderT SqlBackend m StakeDeregistrationId
insertStakeDeregistration = insertUnchecked "StakeDeregistration"

insertStakeRegistration :: (MonadBaseControl IO m, MonadIO m) => StakeRegistration -> ReaderT SqlBackend m StakeRegistrationId
insertStakeRegistration = insertUnchecked "StakeRegistration"

insertTreasury :: (MonadBaseControl IO m, MonadIO m) => Treasury -> ReaderT SqlBackend m TreasuryId
insertTreasury = insertUnchecked "Treasury"

insertTx :: (MonadBaseControl IO m, MonadIO m) => Tx -> ReaderT SqlBackend m TxId
insertTx tx = insertUnchecked ("Tx: " ++ show (BS.length (txHash tx))) tx

insertTxIn :: (MonadBaseControl IO m, MonadIO m) => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertUnchecked "TxIn"

insertTxMetadata :: (MonadBaseControl IO m, MonadIO m) => TxMetadata -> ReaderT SqlBackend m TxMetadataId
insertTxMetadata = insertCheckUnique "TxMetadata"

insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertUnchecked "TxOut"

insertWithdrawal :: (MonadBaseControl IO m, MonadIO m) => Withdrawal  -> ReaderT SqlBackend m WithdrawalId
insertWithdrawal = insertUnchecked "Withdrawal"

insertRedeemer :: (MonadBaseControl IO m, MonadIO m) => Redeemer -> ReaderT SqlBackend m RedeemerId
insertRedeemer = insertCheckUnique "Redeemer"

insertCostModels :: (MonadBaseControl IO m, MonadIO m) => CostModels -> ReaderT SqlBackend m CostModelsId
insertCostModels = insertCheckUnique "CostModels"

-- -----------------------------------------------------------------------------

data DbInsertException
  = DbInsertException String SqlError
  deriving Show

instance Exception DbInsertException

insertManyUncheckedUnique
    :: forall m record.
        ( MonadBaseControl IO m
        , MonadIO m
        , OnlyOneUniqueKey record
        )
    => String -> [record] -> ReaderT SqlBackend m ()
insertManyUncheckedUnique vtype records = do
    handle exceptHandler (rawExecute query values)
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ records)
        , " (", Util.commaSeparated fieldNames
        , ") VALUES "
        ,  Util.commaSeparated . replicate (length records)
             . Util.parenWrapped . Util.commaSeparated $ placeholders
        , " ON CONFLICT ON CONSTRAINT "
        , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
        , " DO NOTHING"
        ]

    values :: [PersistValue]
    values = concatMap (map toPersistValue . toPersistFields) records

    fieldNames, placeholders :: [Text]
    (fieldNames, placeholders) =
      unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

-- Insert, getting PostgreSQL to check the uniqueness constaint, and if it is violated, rewrite
-- the first field with the same value to force PostgresSQL to return the row identifier.
insertCheckUnique
    :: forall m record.
        ( MonadBaseControl IO m
        , MonadIO m
        , OnlyOneUniqueKey record
        , PersistRecordBackend record SqlBackend
        )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertCheckUnique vtype record = do
    res <- handle exceptHandler $ rawSql query values
    case res of
      [ident] -> pure ident
      _other -> error $ mconcat [ "insertCheckUnique: Inserting ", vtype, " failed with ", show res ]
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ Just record)
        , " (", Util.commaSeparated fieldNames
        , ") VALUES (", Util.commaSeparated placeholders
        , ") ON CONFLICT ON CONSTRAINT "
        , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
        -- Head is applied to these two lists, but these two lists should never be empty.
        -- If either list is empty, it is due to a table definition with zero columns.
        , " DO UPDATE SET ", head fieldNames, " = ", head placeholders
        , " RETURNING id ;"
        ]

    values :: [PersistValue]
    values = pvalues ++ case pvalues of
                          [] -> []
                          (x:_) -> [x]

    pvalues :: [PersistValue]
    pvalues = map toPersistValue (toPersistFields record)

    fieldNames, placeholders :: [Text]
    (fieldNames, placeholders) =
      unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

insertReplace
    :: forall m record.
        ( AtLeastOneUniqueKey record
        , Eq (Unique record)
        , MonadBaseControl IO m
        , MonadIO m
        , PersistRecordBackend record SqlBackend
        )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertReplace vtype record =
    handle exceptHandler $ do
      eres <- insertBy record
      case eres of
        Right rid -> pure rid
        Left rec -> do
          mres <- replaceUnique (entityKey rec) record
          maybe (pure $ entityKey rec) (const . pure $ entityKey rec) mres
  where
    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

-- Insert without checking uniqueness constraints. This should be safe for most tables
-- even tables with uniqueness constraints, especially block, tx and many others, where
-- uniqueness is enforced by the ledger.
insertUnchecked
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , MonadBaseControl IO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertUnchecked vtype =
    handle exceptHandler . insert
  where
    exceptHandler :: MonadIO m => SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)


-- This is cargo culted from Persistent because it is not exported.
escapeFieldName :: FieldNameDB -> Text
escapeFieldName (FieldNameDB s) =
    Text.pack $ '"' : go (Text.unpack s) ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

-- This is cargo culted from Persistent because it is not exported.
-- https://github.com/yesodweb/persistent/issues/1194
onlyOneUniqueDef :: OnlyOneUniqueKey record => proxy record -> UniqueDef
onlyOneUniqueDef prxy =
    case entityUniques (entityDef prxy) of
        [uniq] -> uniq
        _ -> error "impossible due to OnlyOneUniqueKey constraint"
