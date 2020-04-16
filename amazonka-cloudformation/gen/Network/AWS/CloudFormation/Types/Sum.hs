{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Sum where

import Network.AWS.Prelude

data AccountGateStatus
  = AGSFailed
  | AGSSkipped
  | AGSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountGateStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure AGSFailed
        "skipped" -> pure AGSSkipped
        "succeeded" -> pure AGSSucceeded
        e -> fromTextError $ "Failure parsing AccountGateStatus from value: '" <> e
           <> "'. Accepted values: failed, skipped, succeeded"

instance ToText AccountGateStatus where
    toText = \case
        AGSFailed -> "FAILED"
        AGSSkipped -> "SKIPPED"
        AGSSucceeded -> "SUCCEEDED"

instance Hashable     AccountGateStatus
instance NFData       AccountGateStatus
instance ToByteString AccountGateStatus
instance ToQuery      AccountGateStatus
instance ToHeader     AccountGateStatus

instance FromXML AccountGateStatus where
    parseXML = parseXMLText "AccountGateStatus"

data Capability
  = CapabilityAutoExpand
  | CapabilityIAM
  | CapabilityNamedIAM
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Capability where
    parser = takeLowerText >>= \case
        "capability_auto_expand" -> pure CapabilityAutoExpand
        "capability_iam" -> pure CapabilityIAM
        "capability_named_iam" -> pure CapabilityNamedIAM
        e -> fromTextError $ "Failure parsing Capability from value: '" <> e
           <> "'. Accepted values: capability_auto_expand, capability_iam, capability_named_iam"

instance ToText Capability where
    toText = \case
        CapabilityAutoExpand -> "CAPABILITY_AUTO_EXPAND"
        CapabilityIAM -> "CAPABILITY_IAM"
        CapabilityNamedIAM -> "CAPABILITY_NAMED_IAM"

instance Hashable     Capability
instance NFData       Capability
instance ToByteString Capability
instance ToQuery      Capability
instance ToHeader     Capability

instance FromXML Capability where
    parseXML = parseXMLText "Capability"

data ChangeAction
  = Add
  | Import
  | Modify
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "import" -> pure Import
        "modify" -> pure Modify
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: add, import, modify, remove"

instance ToText ChangeAction where
    toText = \case
        Add -> "Add"
        Import -> "Import"
        Modify -> "Modify"
        Remove -> "Remove"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance FromXML ChangeAction where
    parseXML = parseXMLText "ChangeAction"

data ChangeSetStatus
  = CSSCreateComplete
  | CSSCreateInProgress
  | CSSCreatePending
  | CSSDeleteComplete
  | CSSFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSetStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure CSSCreateComplete
        "create_in_progress" -> pure CSSCreateInProgress
        "create_pending" -> pure CSSCreatePending
        "delete_complete" -> pure CSSDeleteComplete
        "failed" -> pure CSSFailed
        e -> fromTextError $ "Failure parsing ChangeSetStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_in_progress, create_pending, delete_complete, failed"

instance ToText ChangeSetStatus where
    toText = \case
        CSSCreateComplete -> "CREATE_COMPLETE"
        CSSCreateInProgress -> "CREATE_IN_PROGRESS"
        CSSCreatePending -> "CREATE_PENDING"
        CSSDeleteComplete -> "DELETE_COMPLETE"
        CSSFailed -> "FAILED"

instance Hashable     ChangeSetStatus
instance NFData       ChangeSetStatus
instance ToByteString ChangeSetStatus
instance ToQuery      ChangeSetStatus
instance ToHeader     ChangeSetStatus

instance FromXML ChangeSetStatus where
    parseXML = parseXMLText "ChangeSetStatus"

data ChangeSetType
  = CSTCreate
  | CSTImport
  | CSTUpdate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSetType where
    parser = takeLowerText >>= \case
        "create" -> pure CSTCreate
        "import" -> pure CSTImport
        "update" -> pure CSTUpdate
        e -> fromTextError $ "Failure parsing ChangeSetType from value: '" <> e
           <> "'. Accepted values: create, import, update"

instance ToText ChangeSetType where
    toText = \case
        CSTCreate -> "CREATE"
        CSTImport -> "IMPORT"
        CSTUpdate -> "UPDATE"

instance Hashable     ChangeSetType
instance NFData       ChangeSetType
instance ToByteString ChangeSetType
instance ToQuery      ChangeSetType
instance ToHeader     ChangeSetType

data ChangeSource
  = Automatic
  | DirectModification
  | ParameterReference
  | ResourceAttribute
  | ResourceReference
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSource where
    parser = takeLowerText >>= \case
        "automatic" -> pure Automatic
        "directmodification" -> pure DirectModification
        "parameterreference" -> pure ParameterReference
        "resourceattribute" -> pure ResourceAttribute
        "resourcereference" -> pure ResourceReference
        e -> fromTextError $ "Failure parsing ChangeSource from value: '" <> e
           <> "'. Accepted values: automatic, directmodification, parameterreference, resourceattribute, resourcereference"

instance ToText ChangeSource where
    toText = \case
        Automatic -> "Automatic"
        DirectModification -> "DirectModification"
        ParameterReference -> "ParameterReference"
        ResourceAttribute -> "ResourceAttribute"
        ResourceReference -> "ResourceReference"

instance Hashable     ChangeSource
instance NFData       ChangeSource
instance ToByteString ChangeSource
instance ToQuery      ChangeSource
instance ToHeader     ChangeSource

instance FromXML ChangeSource where
    parseXML = parseXMLText "ChangeSource"

data ChangeType =
  CTResource
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeType where
    parser = takeLowerText >>= \case
        "resource" -> pure CTResource
        e -> fromTextError $ "Failure parsing ChangeType from value: '" <> e
           <> "'. Accepted values: resource"

instance ToText ChangeType where
    toText = \case
        CTResource -> "Resource"

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromXML ChangeType where
    parseXML = parseXMLText "ChangeType"

data DeprecatedStatus
  = Deprecated
  | Live
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeprecatedStatus where
    parser = takeLowerText >>= \case
        "deprecated" -> pure Deprecated
        "live" -> pure Live
        e -> fromTextError $ "Failure parsing DeprecatedStatus from value: '" <> e
           <> "'. Accepted values: deprecated, live"

instance ToText DeprecatedStatus where
    toText = \case
        Deprecated -> "DEPRECATED"
        Live -> "LIVE"

instance Hashable     DeprecatedStatus
instance NFData       DeprecatedStatus
instance ToByteString DeprecatedStatus
instance ToQuery      DeprecatedStatus
instance ToHeader     DeprecatedStatus

instance FromXML DeprecatedStatus where
    parseXML = parseXMLText "DeprecatedStatus"

data DifferenceType
  = DTAdd
  | DTNotEqual
  | DTRemove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DifferenceType where
    parser = takeLowerText >>= \case
        "add" -> pure DTAdd
        "not_equal" -> pure DTNotEqual
        "remove" -> pure DTRemove
        e -> fromTextError $ "Failure parsing DifferenceType from value: '" <> e
           <> "'. Accepted values: add, not_equal, remove"

instance ToText DifferenceType where
    toText = \case
        DTAdd -> "ADD"
        DTNotEqual -> "NOT_EQUAL"
        DTRemove -> "REMOVE"

instance Hashable     DifferenceType
instance NFData       DifferenceType
instance ToByteString DifferenceType
instance ToQuery      DifferenceType
instance ToHeader     DifferenceType

instance FromXML DifferenceType where
    parseXML = parseXMLText "DifferenceType"

data EvaluationType
  = Dynamic
  | Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EvaluationType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing EvaluationType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText EvaluationType where
    toText = \case
        Dynamic -> "Dynamic"
        Static -> "Static"

instance Hashable     EvaluationType
instance NFData       EvaluationType
instance ToByteString EvaluationType
instance ToQuery      EvaluationType
instance ToHeader     EvaluationType

instance FromXML EvaluationType where
    parseXML = parseXMLText "EvaluationType"

data ExecutionStatus
  = Available
  | ExecuteComplete
  | ExecuteFailed
  | ExecuteInProgress
  | Obsolete
  | Unavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "execute_complete" -> pure ExecuteComplete
        "execute_failed" -> pure ExecuteFailed
        "execute_in_progress" -> pure ExecuteInProgress
        "obsolete" -> pure Obsolete
        "unavailable" -> pure Unavailable
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: available, execute_complete, execute_failed, execute_in_progress, obsolete, unavailable"

instance ToText ExecutionStatus where
    toText = \case
        Available -> "AVAILABLE"
        ExecuteComplete -> "EXECUTE_COMPLETE"
        ExecuteFailed -> "EXECUTE_FAILED"
        ExecuteInProgress -> "EXECUTE_IN_PROGRESS"
        Obsolete -> "OBSOLETE"
        Unavailable -> "UNAVAILABLE"

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromXML ExecutionStatus where
    parseXML = parseXMLText "ExecutionStatus"

data HandlerErrorCode
  = AccessDenied
  | AlreadyExists
  | GeneralServiceException
  | InternalFailure
  | InvalidCredentials
  | InvalidRequest
  | NetworkFailure
  | NotFound
  | NotStabilized
  | NotUpdatable
  | ResourceConflict
  | ServiceInternalError
  | ServiceLimitExceeded
  | Throttling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HandlerErrorCode where
    parser = takeLowerText >>= \case
        "accessdenied" -> pure AccessDenied
        "alreadyexists" -> pure AlreadyExists
        "generalserviceexception" -> pure GeneralServiceException
        "internalfailure" -> pure InternalFailure
        "invalidcredentials" -> pure InvalidCredentials
        "invalidrequest" -> pure InvalidRequest
        "networkfailure" -> pure NetworkFailure
        "notfound" -> pure NotFound
        "notstabilized" -> pure NotStabilized
        "notupdatable" -> pure NotUpdatable
        "resourceconflict" -> pure ResourceConflict
        "serviceinternalerror" -> pure ServiceInternalError
        "servicelimitexceeded" -> pure ServiceLimitExceeded
        "throttling" -> pure Throttling
        e -> fromTextError $ "Failure parsing HandlerErrorCode from value: '" <> e
           <> "'. Accepted values: accessdenied, alreadyexists, generalserviceexception, internalfailure, invalidcredentials, invalidrequest, networkfailure, notfound, notstabilized, notupdatable, resourceconflict, serviceinternalerror, servicelimitexceeded, throttling"

instance ToText HandlerErrorCode where
    toText = \case
        AccessDenied -> "AccessDenied"
        AlreadyExists -> "AlreadyExists"
        GeneralServiceException -> "GeneralServiceException"
        InternalFailure -> "InternalFailure"
        InvalidCredentials -> "InvalidCredentials"
        InvalidRequest -> "InvalidRequest"
        NetworkFailure -> "NetworkFailure"
        NotFound -> "NotFound"
        NotStabilized -> "NotStabilized"
        NotUpdatable -> "NotUpdatable"
        ResourceConflict -> "ResourceConflict"
        ServiceInternalError -> "ServiceInternalError"
        ServiceLimitExceeded -> "ServiceLimitExceeded"
        Throttling -> "Throttling"

instance Hashable     HandlerErrorCode
instance NFData       HandlerErrorCode
instance ToByteString HandlerErrorCode
instance ToQuery      HandlerErrorCode
instance ToHeader     HandlerErrorCode

data OnFailure
  = OFDelete
  | OFDoNothing
  | OFRollback
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OnFailure where
    parser = takeLowerText >>= \case
        "delete" -> pure OFDelete
        "do_nothing" -> pure OFDoNothing
        "rollback" -> pure OFRollback
        e -> fromTextError $ "Failure parsing OnFailure from value: '" <> e
           <> "'. Accepted values: delete, do_nothing, rollback"

instance ToText OnFailure where
    toText = \case
        OFDelete -> "DELETE"
        OFDoNothing -> "DO_NOTHING"
        OFRollback -> "ROLLBACK"

instance Hashable     OnFailure
instance NFData       OnFailure
instance ToByteString OnFailure
instance ToQuery      OnFailure
instance ToHeader     OnFailure

data OperationStatus
  = OSFailed
  | OSInProgress
  | OSPending
  | OSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure OSFailed
        "in_progress" -> pure OSInProgress
        "pending" -> pure OSPending
        "success" -> pure OSSuccess
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, pending, success"

instance ToText OperationStatus where
    toText = \case
        OSFailed -> "FAILED"
        OSInProgress -> "IN_PROGRESS"
        OSPending -> "PENDING"
        OSSuccess -> "SUCCESS"

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

data PermissionModels
  = SelfManaged
  | ServiceManaged
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PermissionModels where
    parser = takeLowerText >>= \case
        "self_managed" -> pure SelfManaged
        "service_managed" -> pure ServiceManaged
        e -> fromTextError $ "Failure parsing PermissionModels from value: '" <> e
           <> "'. Accepted values: self_managed, service_managed"

instance ToText PermissionModels where
    toText = \case
        SelfManaged -> "SELF_MANAGED"
        ServiceManaged -> "SERVICE_MANAGED"

instance Hashable     PermissionModels
instance NFData       PermissionModels
instance ToByteString PermissionModels
instance ToQuery      PermissionModels
instance ToHeader     PermissionModels

instance FromXML PermissionModels where
    parseXML = parseXMLText "PermissionModels"

data ProvisioningType
  = FullyMutable
  | Immutable
  | NonProvisionable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisioningType where
    parser = takeLowerText >>= \case
        "fully_mutable" -> pure FullyMutable
        "immutable" -> pure Immutable
        "non_provisionable" -> pure NonProvisionable
        e -> fromTextError $ "Failure parsing ProvisioningType from value: '" <> e
           <> "'. Accepted values: fully_mutable, immutable, non_provisionable"

instance ToText ProvisioningType where
    toText = \case
        FullyMutable -> "FULLY_MUTABLE"
        Immutable -> "IMMUTABLE"
        NonProvisionable -> "NON_PROVISIONABLE"

instance Hashable     ProvisioningType
instance NFData       ProvisioningType
instance ToByteString ProvisioningType
instance ToQuery      ProvisioningType
instance ToHeader     ProvisioningType

instance FromXML ProvisioningType where
    parseXML = parseXMLText "ProvisioningType"

data RegistrationStatus
  = RSComplete
  | RSFailed
  | RSInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "complete" -> pure RSComplete
        "failed" -> pure RSFailed
        "in_progress" -> pure RSInProgress
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: complete, failed, in_progress"

instance ToText RegistrationStatus where
    toText = \case
        RSComplete -> "COMPLETE"
        RSFailed -> "FAILED"
        RSInProgress -> "IN_PROGRESS"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance FromXML RegistrationStatus where
    parseXML = parseXMLText "RegistrationStatus"

data RegistryType =
  Resource
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegistryType where
    parser = takeLowerText >>= \case
        "resource" -> pure Resource
        e -> fromTextError $ "Failure parsing RegistryType from value: '" <> e
           <> "'. Accepted values: resource"

instance ToText RegistryType where
    toText = \case
        Resource -> "RESOURCE"

instance Hashable     RegistryType
instance NFData       RegistryType
instance ToByteString RegistryType
instance ToQuery      RegistryType
instance ToHeader     RegistryType

instance FromXML RegistryType where
    parseXML = parseXMLText "RegistryType"

data Replacement
  = Conditional
  | False'
  | True'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Replacement where
    parser = takeLowerText >>= \case
        "conditional" -> pure Conditional
        "false" -> pure False'
        "true" -> pure True'
        e -> fromTextError $ "Failure parsing Replacement from value: '" <> e
           <> "'. Accepted values: conditional, false, true"

instance ToText Replacement where
    toText = \case
        Conditional -> "Conditional"
        False' -> "False"
        True' -> "True"

instance Hashable     Replacement
instance NFData       Replacement
instance ToByteString Replacement
instance ToQuery      Replacement
instance ToHeader     Replacement

instance FromXML Replacement where
    parseXML = parseXMLText "Replacement"

data RequiresRecreation
  = Always
  | Conditionally
  | Never
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequiresRecreation where
    parser = takeLowerText >>= \case
        "always" -> pure Always
        "conditionally" -> pure Conditionally
        "never" -> pure Never
        e -> fromTextError $ "Failure parsing RequiresRecreation from value: '" <> e
           <> "'. Accepted values: always, conditionally, never"

instance ToText RequiresRecreation where
    toText = \case
        Always -> "Always"
        Conditionally -> "Conditionally"
        Never -> "Never"

instance Hashable     RequiresRecreation
instance NFData       RequiresRecreation
instance ToByteString RequiresRecreation
instance ToQuery      RequiresRecreation
instance ToHeader     RequiresRecreation

instance FromXML RequiresRecreation where
    parseXML = parseXMLText "RequiresRecreation"

data ResourceAttribute
  = CreationPolicy
  | DeletionPolicy
  | Metadata
  | Properties
  | Tags
  | UpdatePolicy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceAttribute where
    parser = takeLowerText >>= \case
        "creationpolicy" -> pure CreationPolicy
        "deletionpolicy" -> pure DeletionPolicy
        "metadata" -> pure Metadata
        "properties" -> pure Properties
        "tags" -> pure Tags
        "updatepolicy" -> pure UpdatePolicy
        e -> fromTextError $ "Failure parsing ResourceAttribute from value: '" <> e
           <> "'. Accepted values: creationpolicy, deletionpolicy, metadata, properties, tags, updatepolicy"

instance ToText ResourceAttribute where
    toText = \case
        CreationPolicy -> "CreationPolicy"
        DeletionPolicy -> "DeletionPolicy"
        Metadata -> "Metadata"
        Properties -> "Properties"
        Tags -> "Tags"
        UpdatePolicy -> "UpdatePolicy"

instance Hashable     ResourceAttribute
instance NFData       ResourceAttribute
instance ToByteString ResourceAttribute
instance ToQuery      ResourceAttribute
instance ToHeader     ResourceAttribute

instance FromXML ResourceAttribute where
    parseXML = parseXMLText "ResourceAttribute"

data ResourceSignalStatus
  = Failure
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceSignalStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ResourceSignalStatus from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

instance Hashable     ResourceSignalStatus
instance NFData       ResourceSignalStatus
instance ToByteString ResourceSignalStatus
instance ToQuery      ResourceSignalStatus
instance ToHeader     ResourceSignalStatus

data ResourceStatus
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | DeleteSkipped
  | ImportComplete
  | ImportFailed
  | ImportInProgress
  | ImportRollbackComplete
  | ImportRollbackFailed
  | ImportRollbackInProgress
  | RollbackComplete
  | UpdateComplete
  | UpdateFailed
  | UpdateInProgress
  | UpdateRollbackComplete
  | UpdateRollbackCompleteCleanupInProgress
  | UpdateRollbackInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure CreateComplete
        "create_failed" -> pure CreateFailed
        "create_in_progress" -> pure CreateInProgress
        "delete_complete" -> pure DeleteComplete
        "delete_failed" -> pure DeleteFailed
        "delete_in_progress" -> pure DeleteInProgress
        "delete_skipped" -> pure DeleteSkipped
        "import_complete" -> pure ImportComplete
        "import_failed" -> pure ImportFailed
        "import_in_progress" -> pure ImportInProgress
        "import_rollback_complete" -> pure ImportRollbackComplete
        "import_rollback_failed" -> pure ImportRollbackFailed
        "import_rollback_in_progress" -> pure ImportRollbackInProgress
        "rollback_complete" -> pure RollbackComplete
        "update_complete" -> pure UpdateComplete
        "update_failed" -> pure UpdateFailed
        "update_in_progress" -> pure UpdateInProgress
        "update_rollback_complete" -> pure UpdateRollbackComplete
        "update_rollback_complete_cleanup_in_progress" -> pure UpdateRollbackCompleteCleanupInProgress
        "update_rollback_in_progress" -> pure UpdateRollbackInProgress
        e -> fromTextError $ "Failure parsing ResourceStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, delete_skipped, import_complete, import_failed, import_in_progress, import_rollback_complete, import_rollback_failed, import_rollback_in_progress, update_complete, update_failed, update_in_progress, update_rollback_complete, update_rollback_complete_cleanup_in_progress, update_rollback_in_progress"

instance ToText ResourceStatus where
    toText = \case
        CreateComplete -> "CREATE_COMPLETE"
        CreateFailed -> "CREATE_FAILED"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        DeleteComplete -> "DELETE_COMPLETE"
        DeleteFailed -> "DELETE_FAILED"
        DeleteInProgress -> "DELETE_IN_PROGRESS"
        DeleteSkipped -> "DELETE_SKIPPED"
        ImportComplete -> "IMPORT_COMPLETE"
        ImportFailed -> "IMPORT_FAILED"
        ImportInProgress -> "IMPORT_IN_PROGRESS"
        ImportRollbackComplete -> "IMPORT_ROLLBACK_COMPLETE"
        ImportRollbackFailed -> "IMPORT_ROLLBACK_FAILED"
        ImportRollbackInProgress -> "IMPORT_ROLLBACK_IN_PROGRESS"
        RollbackComplete -> "ROLLBACK_COMPLETE"
        UpdateComplete -> "UPDATE_COMPLETE"
        UpdateFailed -> "UPDATE_FAILED"
        UpdateInProgress -> "UPDATE_IN_PROGRESS"
        UpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
        UpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
        UpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Hashable     ResourceStatus
instance NFData       ResourceStatus
instance ToByteString ResourceStatus
instance ToQuery      ResourceStatus
instance ToHeader     ResourceStatus

instance FromXML ResourceStatus where
    parseXML = parseXMLText "ResourceStatus"

data StackDriftDetectionStatus
  = DetectionComplete
  | DetectionFailed
  | DetectionInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackDriftDetectionStatus where
    parser = takeLowerText >>= \case
        "detection_complete" -> pure DetectionComplete
        "detection_failed" -> pure DetectionFailed
        "detection_in_progress" -> pure DetectionInProgress
        e -> fromTextError $ "Failure parsing StackDriftDetectionStatus from value: '" <> e
           <> "'. Accepted values: detection_complete, detection_failed, detection_in_progress"

instance ToText StackDriftDetectionStatus where
    toText = \case
        DetectionComplete -> "DETECTION_COMPLETE"
        DetectionFailed -> "DETECTION_FAILED"
        DetectionInProgress -> "DETECTION_IN_PROGRESS"

instance Hashable     StackDriftDetectionStatus
instance NFData       StackDriftDetectionStatus
instance ToByteString StackDriftDetectionStatus
instance ToQuery      StackDriftDetectionStatus
instance ToHeader     StackDriftDetectionStatus

instance FromXML StackDriftDetectionStatus where
    parseXML = parseXMLText "StackDriftDetectionStatus"

data StackDriftStatus
  = SDSDrifted
  | SDSInSync
  | SDSNotChecked
  | SDSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackDriftStatus where
    parser = takeLowerText >>= \case
        "drifted" -> pure SDSDrifted
        "in_sync" -> pure SDSInSync
        "not_checked" -> pure SDSNotChecked
        "unknown" -> pure SDSUnknown
        e -> fromTextError $ "Failure parsing StackDriftStatus from value: '" <> e
           <> "'. Accepted values: drifted, in_sync, not_checked, unknown"

instance ToText StackDriftStatus where
    toText = \case
        SDSDrifted -> "DRIFTED"
        SDSInSync -> "IN_SYNC"
        SDSNotChecked -> "NOT_CHECKED"
        SDSUnknown -> "UNKNOWN"

instance Hashable     StackDriftStatus
instance NFData       StackDriftStatus
instance ToByteString StackDriftStatus
instance ToQuery      StackDriftStatus
instance ToHeader     StackDriftStatus

instance FromXML StackDriftStatus where
    parseXML = parseXMLText "StackDriftStatus"

data StackInstanceStatus
  = Current
  | Inoperable
  | Outdated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackInstanceStatus where
    parser = takeLowerText >>= \case
        "current" -> pure Current
        "inoperable" -> pure Inoperable
        "outdated" -> pure Outdated
        e -> fromTextError $ "Failure parsing StackInstanceStatus from value: '" <> e
           <> "'. Accepted values: current, inoperable, outdated"

instance ToText StackInstanceStatus where
    toText = \case
        Current -> "CURRENT"
        Inoperable -> "INOPERABLE"
        Outdated -> "OUTDATED"

instance Hashable     StackInstanceStatus
instance NFData       StackInstanceStatus
instance ToByteString StackInstanceStatus
instance ToQuery      StackInstanceStatus
instance ToHeader     StackInstanceStatus

instance FromXML StackInstanceStatus where
    parseXML = parseXMLText "StackInstanceStatus"

data StackResourceDriftStatus
  = SRDSDeleted
  | SRDSInSync
  | SRDSModified
  | SRDSNotChecked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackResourceDriftStatus where
    parser = takeLowerText >>= \case
        "deleted" -> pure SRDSDeleted
        "in_sync" -> pure SRDSInSync
        "modified" -> pure SRDSModified
        "not_checked" -> pure SRDSNotChecked
        e -> fromTextError $ "Failure parsing StackResourceDriftStatus from value: '" <> e
           <> "'. Accepted values: deleted, in_sync, modified, not_checked"

instance ToText StackResourceDriftStatus where
    toText = \case
        SRDSDeleted -> "DELETED"
        SRDSInSync -> "IN_SYNC"
        SRDSModified -> "MODIFIED"
        SRDSNotChecked -> "NOT_CHECKED"

instance Hashable     StackResourceDriftStatus
instance NFData       StackResourceDriftStatus
instance ToByteString StackResourceDriftStatus
instance ToQuery      StackResourceDriftStatus
instance ToHeader     StackResourceDriftStatus

instance FromXML StackResourceDriftStatus where
    parseXML = parseXMLText "StackResourceDriftStatus"

data StackSetDriftDetectionStatus
  = SSDDSCompleted
  | SSDDSFailed
  | SSDDSInProgress
  | SSDDSPartialSuccess
  | SSDDSStopped
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetDriftDetectionStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure SSDDSCompleted
        "failed" -> pure SSDDSFailed
        "in_progress" -> pure SSDDSInProgress
        "partial_success" -> pure SSDDSPartialSuccess
        "stopped" -> pure SSDDSStopped
        e -> fromTextError $ "Failure parsing StackSetDriftDetectionStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, in_progress, partial_success, stopped"

instance ToText StackSetDriftDetectionStatus where
    toText = \case
        SSDDSCompleted -> "COMPLETED"
        SSDDSFailed -> "FAILED"
        SSDDSInProgress -> "IN_PROGRESS"
        SSDDSPartialSuccess -> "PARTIAL_SUCCESS"
        SSDDSStopped -> "STOPPED"

instance Hashable     StackSetDriftDetectionStatus
instance NFData       StackSetDriftDetectionStatus
instance ToByteString StackSetDriftDetectionStatus
instance ToQuery      StackSetDriftDetectionStatus
instance ToHeader     StackSetDriftDetectionStatus

instance FromXML StackSetDriftDetectionStatus where
    parseXML = parseXMLText "StackSetDriftDetectionStatus"

data StackSetDriftStatus
  = Drifted
  | InSync
  | NotChecked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetDriftStatus where
    parser = takeLowerText >>= \case
        "drifted" -> pure Drifted
        "in_sync" -> pure InSync
        "not_checked" -> pure NotChecked
        e -> fromTextError $ "Failure parsing StackSetDriftStatus from value: '" <> e
           <> "'. Accepted values: drifted, in_sync, not_checked"

instance ToText StackSetDriftStatus where
    toText = \case
        Drifted -> "DRIFTED"
        InSync -> "IN_SYNC"
        NotChecked -> "NOT_CHECKED"

instance Hashable     StackSetDriftStatus
instance NFData       StackSetDriftStatus
instance ToByteString StackSetDriftStatus
instance ToQuery      StackSetDriftStatus
instance ToHeader     StackSetDriftStatus

instance FromXML StackSetDriftStatus where
    parseXML = parseXMLText "StackSetDriftStatus"

data StackSetOperationAction
  = Create
  | Delete
  | DetectDrift
  | Update
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationAction where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "detect_drift" -> pure DetectDrift
        "update" -> pure Update
        e -> fromTextError $ "Failure parsing StackSetOperationAction from value: '" <> e
           <> "'. Accepted values: create, delete, detect_drift, update"

instance ToText StackSetOperationAction where
    toText = \case
        Create -> "CREATE"
        Delete -> "DELETE"
        DetectDrift -> "DETECT_DRIFT"
        Update -> "UPDATE"

instance Hashable     StackSetOperationAction
instance NFData       StackSetOperationAction
instance ToByteString StackSetOperationAction
instance ToQuery      StackSetOperationAction
instance ToHeader     StackSetOperationAction

instance FromXML StackSetOperationAction where
    parseXML = parseXMLText "StackSetOperationAction"

data StackSetOperationResultStatus
  = Cancelled
  | Failed
  | Pending
  | Running
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationResultStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "pending" -> pure Pending
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing StackSetOperationResultStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, pending, running, succeeded"

instance ToText StackSetOperationResultStatus where
    toText = \case
        Cancelled -> "CANCELLED"
        Failed -> "FAILED"
        Pending -> "PENDING"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"

instance Hashable     StackSetOperationResultStatus
instance NFData       StackSetOperationResultStatus
instance ToByteString StackSetOperationResultStatus
instance ToQuery      StackSetOperationResultStatus
instance ToHeader     StackSetOperationResultStatus

instance FromXML StackSetOperationResultStatus where
    parseXML = parseXMLText "StackSetOperationResultStatus"

data StackSetOperationStatus
  = SSOSFailed
  | SSOSQueued
  | SSOSRunning
  | SSOSStopped
  | SSOSStopping
  | SSOSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure SSOSFailed
        "queued" -> pure SSOSQueued
        "running" -> pure SSOSRunning
        "stopped" -> pure SSOSStopped
        "stopping" -> pure SSOSStopping
        "succeeded" -> pure SSOSSucceeded
        e -> fromTextError $ "Failure parsing StackSetOperationStatus from value: '" <> e
           <> "'. Accepted values: failed, queued, running, stopped, stopping, succeeded"

instance ToText StackSetOperationStatus where
    toText = \case
        SSOSFailed -> "FAILED"
        SSOSQueued -> "QUEUED"
        SSOSRunning -> "RUNNING"
        SSOSStopped -> "STOPPED"
        SSOSStopping -> "STOPPING"
        SSOSSucceeded -> "SUCCEEDED"

instance Hashable     StackSetOperationStatus
instance NFData       StackSetOperationStatus
instance ToByteString StackSetOperationStatus
instance ToQuery      StackSetOperationStatus
instance ToHeader     StackSetOperationStatus

instance FromXML StackSetOperationStatus where
    parseXML = parseXMLText "StackSetOperationStatus"

data StackSetStatus
  = Active
  | Deleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleted" -> pure Deleted
        e -> fromTextError $ "Failure parsing StackSetStatus from value: '" <> e
           <> "'. Accepted values: active, deleted"

instance ToText StackSetStatus where
    toText = \case
        Active -> "ACTIVE"
        Deleted -> "DELETED"

instance Hashable     StackSetStatus
instance NFData       StackSetStatus
instance ToByteString StackSetStatus
instance ToQuery      StackSetStatus
instance ToHeader     StackSetStatus

instance FromXML StackSetStatus where
    parseXML = parseXMLText "StackSetStatus"

data StackStatus
  = SSCreateComplete
  | SSCreateFailed
  | SSCreateInProgress
  | SSDeleteComplete
  | SSDeleteFailed
  | SSDeleteInProgress
  | SSImportComplete
  | SSImportInProgress
  | SSImportRollbackComplete
  | SSImportRollbackFailed
  | SSImportRollbackInProgress
  | SSReviewInProgress
  | SSRollbackComplete
  | SSRollbackFailed
  | SSRollbackInProgress
  | SSUpdateComplete
  | SSUpdateCompleteCleanupInProgress
  | SSUpdateInProgress
  | SSUpdateRollbackComplete
  | SSUpdateRollbackCompleteCleanupInProgress
  | SSUpdateRollbackFailed
  | SSUpdateRollbackInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure SSCreateComplete
        "create_failed" -> pure SSCreateFailed
        "create_in_progress" -> pure SSCreateInProgress
        "delete_complete" -> pure SSDeleteComplete
        "delete_failed" -> pure SSDeleteFailed
        "delete_in_progress" -> pure SSDeleteInProgress
        "import_complete" -> pure SSImportComplete
        "import_in_progress" -> pure SSImportInProgress
        "import_rollback_complete" -> pure SSImportRollbackComplete
        "import_rollback_failed" -> pure SSImportRollbackFailed
        "import_rollback_in_progress" -> pure SSImportRollbackInProgress
        "review_in_progress" -> pure SSReviewInProgress
        "rollback_complete" -> pure SSRollbackComplete
        "rollback_failed" -> pure SSRollbackFailed
        "rollback_in_progress" -> pure SSRollbackInProgress
        "update_complete" -> pure SSUpdateComplete
        "update_complete_cleanup_in_progress" -> pure SSUpdateCompleteCleanupInProgress
        "update_in_progress" -> pure SSUpdateInProgress
        "update_rollback_complete" -> pure SSUpdateRollbackComplete
        "update_rollback_complete_cleanup_in_progress" -> pure SSUpdateRollbackCompleteCleanupInProgress
        "update_rollback_failed" -> pure SSUpdateRollbackFailed
        "update_rollback_in_progress" -> pure SSUpdateRollbackInProgress
        e -> fromTextError $ "Failure parsing StackStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, import_complete, import_in_progress, import_rollback_complete, import_rollback_failed, import_rollback_in_progress, review_in_progress, rollback_complete, rollback_failed, rollback_in_progress, update_complete, update_complete_cleanup_in_progress, update_in_progress, update_rollback_complete, update_rollback_complete_cleanup_in_progress, update_rollback_failed, update_rollback_in_progress"

instance ToText StackStatus where
    toText = \case
        SSCreateComplete -> "CREATE_COMPLETE"
        SSCreateFailed -> "CREATE_FAILED"
        SSCreateInProgress -> "CREATE_IN_PROGRESS"
        SSDeleteComplete -> "DELETE_COMPLETE"
        SSDeleteFailed -> "DELETE_FAILED"
        SSDeleteInProgress -> "DELETE_IN_PROGRESS"
        SSImportComplete -> "IMPORT_COMPLETE"
        SSImportInProgress -> "IMPORT_IN_PROGRESS"
        SSImportRollbackComplete -> "IMPORT_ROLLBACK_COMPLETE"
        SSImportRollbackFailed -> "IMPORT_ROLLBACK_FAILED"
        SSImportRollbackInProgress -> "IMPORT_ROLLBACK_IN_PROGRESS"
        SSReviewInProgress -> "REVIEW_IN_PROGRESS"
        SSRollbackComplete -> "ROLLBACK_COMPLETE"
        SSRollbackFailed -> "ROLLBACK_FAILED"
        SSRollbackInProgress -> "ROLLBACK_IN_PROGRESS"
        SSUpdateComplete -> "UPDATE_COMPLETE"
        SSUpdateCompleteCleanupInProgress -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateInProgress -> "UPDATE_IN_PROGRESS"
        SSUpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
        SSUpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateRollbackFailed -> "UPDATE_ROLLBACK_FAILED"
        SSUpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Hashable     StackStatus
instance NFData       StackStatus
instance ToByteString StackStatus
instance ToQuery      StackStatus
instance ToHeader     StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"

data TemplateStage
  = Original
  | Processed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TemplateStage where
    parser = takeLowerText >>= \case
        "original" -> pure Original
        "processed" -> pure Processed
        e -> fromTextError $ "Failure parsing TemplateStage from value: '" <> e
           <> "'. Accepted values: original, processed"

instance ToText TemplateStage where
    toText = \case
        Original -> "Original"
        Processed -> "Processed"

instance Hashable     TemplateStage
instance NFData       TemplateStage
instance ToByteString TemplateStage
instance ToQuery      TemplateStage
instance ToHeader     TemplateStage

instance FromXML TemplateStage where
    parseXML = parseXMLText "TemplateStage"

data Visibility
  = Private
  | Public
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Visibility where
    parser = takeLowerText >>= \case
        "private" -> pure Private
        "public" -> pure Public
        e -> fromTextError $ "Failure parsing Visibility from value: '" <> e
           <> "'. Accepted values: private, public"

instance ToText Visibility where
    toText = \case
        Private -> "PRIVATE"
        Public -> "PUBLIC"

instance Hashable     Visibility
instance NFData       Visibility
instance ToByteString Visibility
instance ToQuery      Visibility
instance ToHeader     Visibility

instance FromXML Visibility where
    parseXML = parseXMLText "Visibility"
