{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types
    (
    -- * Service Configuration
      cloudFormation

    -- * Errors
    , _TypeNotFoundException
    , _CreatedButModifiedException
    , _ChangeSetNotFoundException
    , _OperationInProgressException
    , _InvalidChangeSetStatusException
    , _OperationNotFoundException
    , _OperationIdAlreadyExistsException
    , _InsufficientCapabilitiesException
    , _TokenAlreadyExistsException
    , _StackSetNotFoundException
    , _StackInstanceNotFoundException
    , _OperationStatusCheckFailedException
    , _StackSetNotEmptyException
    , _InvalidOperationException
    , _InvalidStateTransitionException
    , _NameAlreadyExistsException
    , _CFNRegistryException
    , _StaleRequestException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * AccountGateStatus
    , AccountGateStatus (..)

    -- * Capability
    , Capability (..)

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeSetStatus
    , ChangeSetStatus (..)

    -- * ChangeSetType
    , ChangeSetType (..)

    -- * ChangeSource
    , ChangeSource (..)

    -- * ChangeType
    , ChangeType (..)

    -- * DeprecatedStatus
    , DeprecatedStatus (..)

    -- * DifferenceType
    , DifferenceType (..)

    -- * EvaluationType
    , EvaluationType (..)

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * HandlerErrorCode
    , HandlerErrorCode (..)

    -- * OnFailure
    , OnFailure (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * PermissionModels
    , PermissionModels (..)

    -- * ProvisioningType
    , ProvisioningType (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * RegistryType
    , RegistryType (..)

    -- * Replacement
    , Replacement (..)

    -- * RequiresRecreation
    , RequiresRecreation (..)

    -- * ResourceAttribute
    , ResourceAttribute (..)

    -- * ResourceSignalStatus
    , ResourceSignalStatus (..)

    -- * ResourceStatus
    , ResourceStatus (..)

    -- * StackDriftDetectionStatus
    , StackDriftDetectionStatus (..)

    -- * StackDriftStatus
    , StackDriftStatus (..)

    -- * StackInstanceStatus
    , StackInstanceStatus (..)

    -- * StackResourceDriftStatus
    , StackResourceDriftStatus (..)

    -- * StackSetDriftDetectionStatus
    , StackSetDriftDetectionStatus (..)

    -- * StackSetDriftStatus
    , StackSetDriftStatus (..)

    -- * StackSetOperationAction
    , StackSetOperationAction (..)

    -- * StackSetOperationResultStatus
    , StackSetOperationResultStatus (..)

    -- * StackSetOperationStatus
    , StackSetOperationStatus (..)

    -- * StackSetStatus
    , StackSetStatus (..)

    -- * StackStatus
    , StackStatus (..)

    -- * TemplateStage
    , TemplateStage (..)

    -- * Visibility
    , Visibility (..)

    -- * AccountGateResult
    , AccountGateResult
    , accountGateResult
    , agrStatus
    , agrStatusReason

    -- * AccountLimit
    , AccountLimit
    , accountLimit
    , alValue
    , alName

    -- * AutoDeployment
    , AutoDeployment
    , autoDeployment
    , adEnabled
    , adRetainStacksOnAccountRemoval

    -- * Change
    , Change
    , change
    , cResourceChange
    , cType

    -- * ChangeSetSummary
    , ChangeSetSummary
    , changeSetSummary
    , cCreationTime
    , cStatus
    , cChangeSetName
    , cExecutionStatus
    , cChangeSetId
    , cStatusReason
    , cStackId
    , cDescription
    , cStackName

    -- * DeploymentTargets
    , DeploymentTargets
    , deploymentTargets
    , dtAccounts
    , dtOrganizationalUnitIds

    -- * Export
    , Export
    , export'
    , eValue
    , eExportingStackId
    , eName

    -- * LoggingConfig
    , LoggingConfig
    , loggingConfig
    , lcLogRoleARN
    , lcLogGroupName

    -- * Output
    , Output
    , output
    , oOutputValue
    , oOutputKey
    , oExportName
    , oDescription

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pResolvedValue
    , pParameterKey
    , pUsePreviousValue

    -- * ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcAllowedValues

    -- * ParameterDeclaration
    , ParameterDeclaration
    , parameterDeclaration
    , pdParameterKey
    , pdParameterType
    , pdParameterConstraints
    , pdDefaultValue
    , pdNoEcho
    , pdDescription

    -- * PhysicalResourceIdContextKeyValuePair
    , PhysicalResourceIdContextKeyValuePair
    , physicalResourceIdContextKeyValuePair
    , prickvpKey
    , prickvpValue

    -- * PropertyDifference
    , PropertyDifference
    , propertyDifference
    , pdPropertyPath
    , pdExpectedValue
    , pdActualValue
    , pdDifferenceType

    -- * ResourceChange
    , ResourceChange
    , resourceChange
    , rcLogicalResourceId
    , rcPhysicalResourceId
    , rcResourceType
    , rcAction
    , rcScope
    , rcDetails
    , rcReplacement

    -- * ResourceChangeDetail
    , ResourceChangeDetail
    , resourceChangeDetail
    , rcdCausingEntity
    , rcdChangeSource
    , rcdEvaluation
    , rcdTarget

    -- * ResourceIdentifierSummary
    , ResourceIdentifierSummary
    , resourceIdentifierSummary
    , risResourceType
    , risLogicalResourceIds
    , risResourceIdentifiers

    -- * ResourceTargetDefinition
    , ResourceTargetDefinition
    , resourceTargetDefinition
    , rtdAttribute
    , rtdRequiresRecreation
    , rtdName

    -- * ResourceToImport
    , ResourceToImport
    , resourceToImport
    , rtiResourceType
    , rtiLogicalResourceId
    , rtiResourceIdentifier

    -- * RollbackConfiguration
    , RollbackConfiguration
    , rollbackConfiguration
    , rcRollbackTriggers
    , rcMonitoringTimeInMinutes

    -- * RollbackTrigger
    , RollbackTrigger
    , rollbackTrigger
    , rtARN
    , rtType

    -- * Stack
    , Stack
    , stack
    , staDisableRollback
    , staLastUpdatedTime
    , staRootId
    , staNotificationARNs
    , staStackStatusReason
    , staEnableTerminationProtection
    , staDriftInformation
    , staChangeSetId
    , staDeletionTime
    , staOutputs
    , staParameters
    , staStackId
    , staDescription
    , staCapabilities
    , staRollbackConfiguration
    , staTags
    , staTimeoutInMinutes
    , staParentId
    , staRoleARN
    , staStackName
    , staCreationTime
    , staStackStatus

    -- * StackDriftInformation
    , StackDriftInformation
    , stackDriftInformation
    , sdiLastCheckTimestamp
    , sdiStackDriftStatus

    -- * StackDriftInformationSummary
    , StackDriftInformationSummary
    , stackDriftInformationSummary
    , sdisLastCheckTimestamp
    , sdisStackDriftStatus

    -- * StackEvent
    , StackEvent
    , stackEvent
    , seLogicalResourceId
    , sePhysicalResourceId
    , seResourceType
    , seResourceStatusReason
    , seResourceProperties
    , seResourceStatus
    , seClientRequestToken
    , seStackId
    , seEventId
    , seStackName
    , seTimestamp

    -- * StackInstance
    , StackInstance
    , stackInstance
    , siStatus
    , siLastDriftCheckTimestamp
    , siAccount
    , siDriftStatus
    , siOrganizationalUnitId
    , siRegion
    , siStatusReason
    , siStackId
    , siParameterOverrides
    , siStackSetId

    -- * StackInstanceSummary
    , StackInstanceSummary
    , stackInstanceSummary
    , sisStatus
    , sisLastDriftCheckTimestamp
    , sisAccount
    , sisDriftStatus
    , sisOrganizationalUnitId
    , sisRegion
    , sisStatusReason
    , sisStackId
    , sisStackSetId

    -- * StackResource
    , StackResource
    , stackResource
    , srPhysicalResourceId
    , srResourceStatusReason
    , srDriftInformation
    , srStackId
    , srDescription
    , srStackName
    , srLogicalResourceId
    , srResourceType
    , srTimestamp
    , srResourceStatus

    -- * StackResourceDetail
    , StackResourceDetail
    , stackResourceDetail
    , sPhysicalResourceId
    , sResourceStatusReason
    , sDriftInformation
    , sMetadata
    , sStackId
    , sDescription
    , sStackName
    , sLogicalResourceId
    , sResourceType
    , sLastUpdatedTimestamp
    , sResourceStatus

    -- * StackResourceDrift
    , StackResourceDrift
    , stackResourceDrift
    , srdActualProperties
    , srdPhysicalResourceId
    , srdPhysicalResourceIdContext
    , srdPropertyDifferences
    , srdExpectedProperties
    , srdStackId
    , srdLogicalResourceId
    , srdResourceType
    , srdStackResourceDriftStatus
    , srdTimestamp

    -- * StackResourceDriftInformation
    , StackResourceDriftInformation
    , stackResourceDriftInformation
    , srdiLastCheckTimestamp
    , srdiStackResourceDriftStatus

    -- * StackResourceDriftInformationSummary
    , StackResourceDriftInformationSummary
    , stackResourceDriftInformationSummary
    , srdisLastCheckTimestamp
    , srdisStackResourceDriftStatus

    -- * StackResourceSummary
    , StackResourceSummary
    , stackResourceSummary
    , srsPhysicalResourceId
    , srsResourceStatusReason
    , srsDriftInformation
    , srsLogicalResourceId
    , srsResourceType
    , srsLastUpdatedTimestamp
    , srsResourceStatus

    -- * StackSet
    , StackSet
    , stackSet
    , ssStackSetDriftDetectionDetails
    , ssStatus
    , ssAdministrationRoleARN
    , ssAutoDeployment
    , ssOrganizationalUnitIds
    , ssStackSetARN
    , ssPermissionModel
    , ssParameters
    , ssTemplateBody
    , ssStackSetName
    , ssDescription
    , ssCapabilities
    , ssTags
    , ssStackSetId
    , ssExecutionRoleName

    -- * StackSetDriftDetectionDetails
    , StackSetDriftDetectionDetails
    , stackSetDriftDetectionDetails
    , ssdddLastDriftCheckTimestamp
    , ssdddTotalStackInstancesCount
    , ssdddInProgressStackInstancesCount
    , ssdddDriftedStackInstancesCount
    , ssdddDriftDetectionStatus
    , ssdddDriftStatus
    , ssdddFailedStackInstancesCount
    , ssdddInSyncStackInstancesCount

    -- * StackSetOperation
    , StackSetOperation
    , stackSetOperation
    , ssoStackSetDriftDetectionDetails
    , ssoStatus
    , ssoAdministrationRoleARN
    , ssoAction
    , ssoEndTimestamp
    , ssoCreationTimestamp
    , ssoOperationPreferences
    , ssoOperationId
    , ssoRetainStacks
    , ssoDeploymentTargets
    , ssoStackSetId
    , ssoExecutionRoleName

    -- * StackSetOperationPreferences
    , StackSetOperationPreferences
    , stackSetOperationPreferences
    , ssopRegionOrder
    , ssopMaxConcurrentCount
    , ssopMaxConcurrentPercentage
    , ssopFailureToleranceCount
    , ssopFailureTolerancePercentage

    -- * StackSetOperationResultSummary
    , StackSetOperationResultSummary
    , stackSetOperationResultSummary
    , ssorsStatus
    , ssorsAccount
    , ssorsAccountGateResult
    , ssorsOrganizationalUnitId
    , ssorsRegion
    , ssorsStatusReason

    -- * StackSetOperationSummary
    , StackSetOperationSummary
    , stackSetOperationSummary
    , ssosStatus
    , ssosAction
    , ssosEndTimestamp
    , ssosCreationTimestamp
    , ssosOperationId

    -- * StackSetSummary
    , StackSetSummary
    , stackSetSummary
    , sssStatus
    , sssLastDriftCheckTimestamp
    , sssAutoDeployment
    , sssDriftStatus
    , sssPermissionModel
    , sssStackSetName
    , sssDescription
    , sssStackSetId

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssLastUpdatedTime
    , ssRootId
    , ssStackStatusReason
    , ssTemplateDescription
    , ssDriftInformation
    , ssDeletionTime
    , ssStackId
    , ssParentId
    , ssStackName
    , ssCreationTime
    , ssStackStatus

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TemplateParameter
    , TemplateParameter
    , templateParameter
    , tpParameterKey
    , tpDefaultValue
    , tpNoEcho
    , tpDescription

    -- * TypeSummary
    , TypeSummary
    , typeSummary
    , tsLastUpdated
    , tsTypeName
    , tsDefaultVersionId
    , tsTypeARN
    , tsType
    , tsDescription

    -- * TypeVersionSummary
    , TypeVersionSummary
    , typeVersionSummary
    , tvsVersionId
    , tvsTypeName
    , tvsARN
    , tvsTimeCreated
    , tvsType
    , tvsDescription
    ) where

import Network.AWS.CloudFormation.Types.Product
import Network.AWS.CloudFormation.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
cloudFormation :: Service
cloudFormation =
  Service
    { _svcAbbrev = "CloudFormation"
    , _svcSigner = v4
    , _svcPrefix = "cloudformation"
    , _svcVersion = "2010-05-15"
    , _svcEndpoint = defaultEndpoint cloudFormation
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "CloudFormation"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasCode "ProvisionedThroughputExceededException" . hasStatus 400) e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The specified type does not exist in the CloudFormation registry.
--
--
_TypeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TypeNotFoundException =
  _MatchServiceError cloudFormation "TypeNotFoundException" . hasStatus 404


-- | The specified resource exists, but has been changed.
--
--
_CreatedButModifiedException :: AsError a => Getting (First ServiceError) a ServiceError
_CreatedButModifiedException =
  _MatchServiceError cloudFormation "CreatedButModifiedException" .
  hasStatus 409


-- | The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the @ListChangeSets@ action.
--
--
_ChangeSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ChangeSetNotFoundException =
  _MatchServiceError cloudFormation "ChangeSetNotFound" . hasStatus 404


-- | Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
  _MatchServiceError cloudFormation "OperationInProgressException" .
  hasStatus 409


-- | The specified change set can't be used to update the stack. For example, the change set status might be @CREATE_IN_PROGRESS@ , or the stack status might be @UPDATE_IN_PROGRESS@ .
--
--
_InvalidChangeSetStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeSetStatusException =
  _MatchServiceError cloudFormation "InvalidChangeSetStatus" . hasStatus 400


-- | The specified ID refers to an operation that doesn't exist.
--
--
_OperationNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotFoundException =
  _MatchServiceError cloudFormation "OperationNotFoundException" . hasStatus 404


-- | The specified operation ID already exists.
--
--
_OperationIdAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationIdAlreadyExistsException =
  _MatchServiceError cloudFormation "OperationIdAlreadyExistsException" .
  hasStatus 409


-- | The template contains resources with capabilities that weren't specified in the Capabilities parameter.
--
--
_InsufficientCapabilitiesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCapabilitiesException =
  _MatchServiceError cloudFormation "InsufficientCapabilitiesException" .
  hasStatus 400


-- | A client request token already exists.
--
--
_TokenAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_TokenAlreadyExistsException =
  _MatchServiceError cloudFormation "TokenAlreadyExistsException" .
  hasStatus 400


-- | The specified stack set doesn't exist.
--
--
_StackSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StackSetNotFoundException =
  _MatchServiceError cloudFormation "StackSetNotFoundException" . hasStatus 404


-- | The specified stack instance doesn't exist.
--
--
_StackInstanceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StackInstanceNotFoundException =
  _MatchServiceError cloudFormation "StackInstanceNotFoundException" .
  hasStatus 404


-- | Error reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> . CloudFormation does not return this error to users.
--
--
_OperationStatusCheckFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationStatusCheckFailedException =
  _MatchServiceError cloudFormation "ConditionalCheckFailed" . hasStatus 400


-- | You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.
--
--
_StackSetNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_StackSetNotEmptyException =
  _MatchServiceError cloudFormation "StackSetNotEmptyException" . hasStatus 409


-- | The specified operation isn't valid.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
  _MatchServiceError cloudFormation "InvalidOperationException" . hasStatus 400


-- | Error reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> . CloudFormation does not return this error to users.
--
--
_InvalidStateTransitionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateTransitionException =
  _MatchServiceError cloudFormation "InvalidStateTransition" . hasStatus 400


-- | The specified name is already in use.
--
--
_NameAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_NameAlreadyExistsException =
  _MatchServiceError cloudFormation "NameAlreadyExistsException" . hasStatus 409


-- | An error occurred during a CloudFormation registry operation.
--
--
_CFNRegistryException :: AsError a => Getting (First ServiceError) a ServiceError
_CFNRegistryException =
  _MatchServiceError cloudFormation "CFNRegistryException" . hasStatus 400


-- | Another operation has been performed on this stack set since the specified operation was performed.
--
--
_StaleRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_StaleRequestException =
  _MatchServiceError cloudFormation "StaleRequestException" . hasStatus 409


-- | The resource with the name requested already exists.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
  _MatchServiceError cloudFormation "AlreadyExistsException" . hasStatus 400


-- | The quota for the resource has already been reached.
--
--
-- For information on resource and stack limitations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html Limits> in the /AWS CloudFormation User Guide/ .
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudFormation "LimitExceededException" . hasStatus 400

