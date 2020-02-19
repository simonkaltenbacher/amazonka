{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Product where

import Network.AWS.CloudFormation.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and region.
--
--
-- For each account and region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and region. CloudFormation invokes the function each time a stack set operation is requested for that account and region; if the function returns @FAILED@ , CloudFormation cancels the operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html Configuring a target account gate> .
--
--
-- /See:/ 'accountGateResult' smart constructor.
data AccountGateResult =
  AccountGateResult'
    { _agrStatus       :: !(Maybe AccountGateStatus)
    , _agrStatusReason :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountGateResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agrStatus' - The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and region.      * @FAILED@ : The account gate function has determined that the account and region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and region, for one of the following reasons:     * An account gate function has not been specified for the account and region. AWS CloudFormation proceeds with the stack set operation in this account and region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and region.
--
-- * 'agrStatusReason' - The reason for the account gate status assigned to this account and region for the stack set operation.
accountGateResult
    :: AccountGateResult
accountGateResult =
  AccountGateResult' {_agrStatus = Nothing, _agrStatusReason = Nothing}


-- | The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and region.      * @FAILED@ : The account gate function has determined that the account and region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and region, for one of the following reasons:     * An account gate function has not been specified for the account and region. AWS CloudFormation proceeds with the stack set operation in this account and region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and region.
agrStatus :: Lens' AccountGateResult (Maybe AccountGateStatus)
agrStatus = lens _agrStatus (\ s a -> s{_agrStatus = a})

-- | The reason for the account gate status assigned to this account and region for the stack set operation.
agrStatusReason :: Lens' AccountGateResult (Maybe Text)
agrStatusReason = lens _agrStatusReason (\ s a -> s{_agrStatusReason = a})

instance FromXML AccountGateResult where
        parseXML x
          = AccountGateResult' <$>
              (x .@? "Status") <*> (x .@? "StatusReason")

instance Hashable AccountGateResult where

instance NFData AccountGateResult where

-- | The AccountLimit data type.
--
--
-- CloudFormation has the following limits per account:
--
--     * Number of concurrent resources
--
--     * Number of stacks
--
--     * Number of stack outputs
--
--
--
-- For more information about these account limits, and other CloudFormation limits, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits> in the /AWS CloudFormation User Guide/ .
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit =
  AccountLimit'
    { _alValue :: !(Maybe Int)
    , _alName  :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alValue' - The value that is associated with the account limit name.
--
-- * 'alName' - The name of the account limit. Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
accountLimit
    :: AccountLimit
accountLimit = AccountLimit' {_alValue = Nothing, _alName = Nothing}


-- | The value that is associated with the account limit name.
alValue :: Lens' AccountLimit (Maybe Int)
alValue = lens _alValue (\ s a -> s{_alValue = a})

-- | The name of the account limit. Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
alName :: Lens' AccountLimit (Maybe Text)
alName = lens _alName (\ s a -> s{_alName = a})

instance FromXML AccountLimit where
        parseXML x
          = AccountLimit' <$>
              (x .@? "Value") <*> (x .@? "Name")

instance Hashable AccountLimit where

instance NFData AccountLimit where

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
--
--
-- /See:/ 'autoDeployment' smart constructor.
data AutoDeployment =
  AutoDeployment'
    { _adEnabled                      :: !(Maybe Bool)
    , _adRetainStacksOnAccountRemoval :: !(Maybe Bool)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adEnabled' - If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
--
-- * 'adRetainStacksOnAccountRemoval' - If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
autoDeployment
    :: AutoDeployment
autoDeployment =
  AutoDeployment'
    {_adEnabled = Nothing, _adRetainStacksOnAccountRemoval = Nothing}


-- | If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
adEnabled :: Lens' AutoDeployment (Maybe Bool)
adEnabled = lens _adEnabled (\ s a -> s{_adEnabled = a})

-- | If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
adRetainStacksOnAccountRemoval :: Lens' AutoDeployment (Maybe Bool)
adRetainStacksOnAccountRemoval = lens _adRetainStacksOnAccountRemoval (\ s a -> s{_adRetainStacksOnAccountRemoval = a})

instance FromXML AutoDeployment where
        parseXML x
          = AutoDeployment' <$>
              (x .@? "Enabled") <*>
                (x .@? "RetainStacksOnAccountRemoval")

instance Hashable AutoDeployment where

instance NFData AutoDeployment where

instance ToQuery AutoDeployment where
        toQuery AutoDeployment'{..}
          = mconcat
              ["Enabled" =: _adEnabled,
               "RetainStacksOnAccountRemoval" =:
                 _adRetainStacksOnAccountRemoval]

-- | The @Change@ structure describes the changes AWS CloudFormation will perform if you execute the change set.
--
--
--
-- /See:/ 'change' smart constructor.
data Change =
  Change'
    { _cResourceChange :: !(Maybe ResourceChange)
    , _cType           :: !(Maybe ChangeType)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResourceChange' - A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
--
-- * 'cType' - The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
change
    :: Change
change = Change' {_cResourceChange = Nothing, _cType = Nothing}


-- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
cResourceChange :: Lens' Change (Maybe ResourceChange)
cResourceChange = lens _cResourceChange (\ s a -> s{_cResourceChange = a})

-- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
cType :: Lens' Change (Maybe ChangeType)
cType = lens _cType (\ s a -> s{_cType = a})

instance FromXML Change where
        parseXML x
          = Change' <$>
              (x .@? "ResourceChange") <*> (x .@? "Type")

instance Hashable Change where

instance NFData Change where

-- | The @ChangeSetSummary@ structure describes a change set, its status, and the stack with which it's associated.
--
--
--
-- /See:/ 'changeSetSummary' smart constructor.
data ChangeSetSummary =
  ChangeSetSummary'
    { _cCreationTime    :: !(Maybe ISO8601)
    , _cStatus          :: !(Maybe ChangeSetStatus)
    , _cChangeSetName   :: !(Maybe Text)
    , _cExecutionStatus :: !(Maybe ExecutionStatus)
    , _cChangeSetId     :: !(Maybe Text)
    , _cStatusReason    :: !(Maybe Text)
    , _cStackId         :: !(Maybe Text)
    , _cDescription     :: !(Maybe Text)
    , _cStackName       :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'cStatus' - The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- * 'cChangeSetName' - The name of the change set.
--
-- * 'cExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- * 'cChangeSetId' - The ID of the change set.
--
-- * 'cStatusReason' - A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
--
-- * 'cStackId' - The ID of the stack with which the change set is associated.
--
-- * 'cDescription' - Descriptive information about the change set.
--
-- * 'cStackName' - The name of the stack with which the change set is associated.
changeSetSummary
    :: ChangeSetSummary
changeSetSummary =
  ChangeSetSummary'
    { _cCreationTime = Nothing
    , _cStatus = Nothing
    , _cChangeSetName = Nothing
    , _cExecutionStatus = Nothing
    , _cChangeSetId = Nothing
    , _cStatusReason = Nothing
    , _cStackId = Nothing
    , _cDescription = Nothing
    , _cStackName = Nothing
    }


-- | The start time when the change set was created, in UTC.
cCreationTime :: Lens' ChangeSetSummary (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\ s a -> s{_cCreationTime = a}) . mapping _Time

-- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
cStatus :: Lens' ChangeSetSummary (Maybe ChangeSetStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The name of the change set.
cChangeSetName :: Lens' ChangeSetSummary (Maybe Text)
cChangeSetName = lens _cChangeSetName (\ s a -> s{_cChangeSetName = a})

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
cExecutionStatus :: Lens' ChangeSetSummary (Maybe ExecutionStatus)
cExecutionStatus = lens _cExecutionStatus (\ s a -> s{_cExecutionStatus = a})

-- | The ID of the change set.
cChangeSetId :: Lens' ChangeSetSummary (Maybe Text)
cChangeSetId = lens _cChangeSetId (\ s a -> s{_cChangeSetId = a})

-- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
cStatusReason :: Lens' ChangeSetSummary (Maybe Text)
cStatusReason = lens _cStatusReason (\ s a -> s{_cStatusReason = a})

-- | The ID of the stack with which the change set is associated.
cStackId :: Lens' ChangeSetSummary (Maybe Text)
cStackId = lens _cStackId (\ s a -> s{_cStackId = a})

-- | Descriptive information about the change set.
cDescription :: Lens' ChangeSetSummary (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | The name of the stack with which the change set is associated.
cStackName :: Lens' ChangeSetSummary (Maybe Text)
cStackName = lens _cStackName (\ s a -> s{_cStackName = a})

instance FromXML ChangeSetSummary where
        parseXML x
          = ChangeSetSummary' <$>
              (x .@? "CreationTime") <*> (x .@? "Status") <*>
                (x .@? "ChangeSetName")
                <*> (x .@? "ExecutionStatus")
                <*> (x .@? "ChangeSetId")
                <*> (x .@? "StatusReason")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")

instance Hashable ChangeSetSummary where

instance NFData ChangeSetSummary where

-- | [@Service-managed@ permissions] The AWS Organizations accounts to which StackSets deploys.
--
--
-- For update operations, you can specify either @Accounts@ or @OrganizationalUnitIds@ . For create and delete operations, specify @OrganizationalUnitIds@ .
--
--
-- /See:/ 'deploymentTargets' smart constructor.
data DeploymentTargets =
  DeploymentTargets'
    { _dtAccounts              :: !(Maybe [Text])
    , _dtOrganizationalUnitIds :: !(Maybe [Text])
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtAccounts' - The names of one or more AWS accounts for which you want to deploy stack set updates.
--
-- * 'dtOrganizationalUnitIds' - The organization root ID or organizational unit (OUs) IDs to which StackSets deploys.
deploymentTargets
    :: DeploymentTargets
deploymentTargets =
  DeploymentTargets' {_dtAccounts = Nothing, _dtOrganizationalUnitIds = Nothing}


-- | The names of one or more AWS accounts for which you want to deploy stack set updates.
dtAccounts :: Lens' DeploymentTargets [Text]
dtAccounts = lens _dtAccounts (\ s a -> s{_dtAccounts = a}) . _Default . _Coerce

-- | The organization root ID or organizational unit (OUs) IDs to which StackSets deploys.
dtOrganizationalUnitIds :: Lens' DeploymentTargets [Text]
dtOrganizationalUnitIds = lens _dtOrganizationalUnitIds (\ s a -> s{_dtOrganizationalUnitIds = a}) . _Default . _Coerce

instance FromXML DeploymentTargets where
        parseXML x
          = DeploymentTargets' <$>
              (x .@? "Accounts" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "OrganizationalUnitIds" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable DeploymentTargets where

instance NFData DeploymentTargets where

instance ToQuery DeploymentTargets where
        toQuery DeploymentTargets'{..}
          = mconcat
              ["Accounts" =:
                 toQuery (toQueryList "member" <$> _dtAccounts),
               "OrganizationalUnitIds" =:
                 toQuery
                   (toQueryList "member" <$> _dtOrganizationalUnitIds)]

-- | The @Export@ structure describes the exported output values for a stack.
--
--
--
-- /See:/ 'export'' smart constructor.
data Export =
  Export'
    { _eValue            :: !(Maybe Text)
    , _eExportingStackId :: !(Maybe Text)
    , _eName             :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Export' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eValue' - The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- * 'eExportingStackId' - The stack that contains the exported output name and value.
--
-- * 'eName' - The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
export'
    :: Export
export' =
  Export' {_eValue = Nothing, _eExportingStackId = Nothing, _eName = Nothing}


-- | The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
eValue :: Lens' Export (Maybe Text)
eValue = lens _eValue (\ s a -> s{_eValue = a})

-- | The stack that contains the exported output name and value.
eExportingStackId :: Lens' Export (Maybe Text)
eExportingStackId = lens _eExportingStackId (\ s a -> s{_eExportingStackId = a})

-- | The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
eName :: Lens' Export (Maybe Text)
eName = lens _eName (\ s a -> s{_eName = a})

instance FromXML Export where
        parseXML x
          = Export' <$>
              (x .@? "Value") <*> (x .@? "ExportingStackId") <*>
                (x .@? "Name")

instance Hashable Export where

instance NFData Export where

-- | Contains logging configuration information for a type.
--
--
--
-- /See:/ 'loggingConfig' smart constructor.
data LoggingConfig =
  LoggingConfig'
    { _lcLogRoleARN   :: !Text
    , _lcLogGroupName :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcLogRoleARN' - The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
--
-- * 'lcLogGroupName' - The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
loggingConfig
    :: Text -- ^ 'lcLogRoleARN'
    -> Text -- ^ 'lcLogGroupName'
    -> LoggingConfig
loggingConfig pLogRoleARN_ pLogGroupName_ =
  LoggingConfig'
    {_lcLogRoleARN = pLogRoleARN_, _lcLogGroupName = pLogGroupName_}


-- | The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
lcLogRoleARN :: Lens' LoggingConfig Text
lcLogRoleARN = lens _lcLogRoleARN (\ s a -> s{_lcLogRoleARN = a})

-- | The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
lcLogGroupName :: Lens' LoggingConfig Text
lcLogGroupName = lens _lcLogGroupName (\ s a -> s{_lcLogGroupName = a})

instance FromXML LoggingConfig where
        parseXML x
          = LoggingConfig' <$>
              (x .@ "LogRoleArn") <*> (x .@ "LogGroupName")

instance Hashable LoggingConfig where

instance NFData LoggingConfig where

instance ToQuery LoggingConfig where
        toQuery LoggingConfig'{..}
          = mconcat
              ["LogRoleArn" =: _lcLogRoleARN,
               "LogGroupName" =: _lcLogGroupName]

-- | The Output data type.
--
--
--
-- /See:/ 'output' smart constructor.
data Output =
  Output'
    { _oOutputValue :: !(Maybe Text)
    , _oOutputKey   :: !(Maybe Text)
    , _oExportName  :: !(Maybe Text)
    , _oDescription :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOutputValue' - The value associated with the output.
--
-- * 'oOutputKey' - The key associated with the output.
--
-- * 'oExportName' - The name of the export associated with the output.
--
-- * 'oDescription' - User defined description associated with the output.
output
    :: Output
output =
  Output'
    { _oOutputValue = Nothing
    , _oOutputKey = Nothing
    , _oExportName = Nothing
    , _oDescription = Nothing
    }


-- | The value associated with the output.
oOutputValue :: Lens' Output (Maybe Text)
oOutputValue = lens _oOutputValue (\ s a -> s{_oOutputValue = a})

-- | The key associated with the output.
oOutputKey :: Lens' Output (Maybe Text)
oOutputKey = lens _oOutputKey (\ s a -> s{_oOutputKey = a})

-- | The name of the export associated with the output.
oExportName :: Lens' Output (Maybe Text)
oExportName = lens _oExportName (\ s a -> s{_oExportName = a})

-- | User defined description associated with the output.
oDescription :: Lens' Output (Maybe Text)
oDescription = lens _oDescription (\ s a -> s{_oDescription = a})

instance FromXML Output where
        parseXML x
          = Output' <$>
              (x .@? "OutputValue") <*> (x .@? "OutputKey") <*>
                (x .@? "ExportName")
                <*> (x .@? "Description")

instance Hashable Output where

instance NFData Output where

-- | The Parameter data type.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter =
  Parameter'
    { _pParameterValue   :: !(Maybe Text)
    , _pResolvedValue    :: !(Maybe Text)
    , _pParameterKey     :: !(Maybe Text)
    , _pUsePreviousValue :: !(Maybe Bool)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue' - The input value associated with the parameter.
--
-- * 'pResolvedValue' - Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
--
-- * 'pParameterKey' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
--
-- * 'pUsePreviousValue' - During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
parameter
    :: Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing
    , _pResolvedValue = Nothing
    , _pParameterKey = Nothing
    , _pUsePreviousValue = Nothing
    }


-- | The input value associated with the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
pResolvedValue :: Lens' Parameter (Maybe Text)
pResolvedValue = lens _pResolvedValue (\ s a -> s{_pResolvedValue = a})

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
pParameterKey :: Lens' Parameter (Maybe Text)
pParameterKey = lens _pParameterKey (\ s a -> s{_pParameterKey = a})

-- | During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
pUsePreviousValue :: Lens' Parameter (Maybe Bool)
pUsePreviousValue = lens _pUsePreviousValue (\ s a -> s{_pUsePreviousValue = a})

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ParameterValue") <*> (x .@? "ResolvedValue")
                <*> (x .@? "ParameterKey")
                <*> (x .@? "UsePreviousValue")

instance Hashable Parameter where

instance NFData Parameter where

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ParameterValue" =: _pParameterValue,
               "ResolvedValue" =: _pResolvedValue,
               "ParameterKey" =: _pParameterKey,
               "UsePreviousValue" =: _pUsePreviousValue]

-- | A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the @AllowedValues@ property.
--
--
--
-- /See:/ 'parameterConstraints' smart constructor.
newtype ParameterConstraints =
  ParameterConstraints'
    { _pcAllowedValues :: Maybe [Text]
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAllowedValues' - A list of values that are permitted for a parameter.
parameterConstraints
    :: ParameterConstraints
parameterConstraints = ParameterConstraints' {_pcAllowedValues = Nothing}


-- | A list of values that are permitted for a parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\ s a -> s{_pcAllowedValues = a}) . _Default . _Coerce

instance FromXML ParameterConstraints where
        parseXML x
          = ParameterConstraints' <$>
              (x .@? "AllowedValues" .!@ mempty >>=
                 may (parseXMLList "member"))

instance Hashable ParameterConstraints where

instance NFData ParameterConstraints where

-- | The ParameterDeclaration data type.
--
--
--
-- /See:/ 'parameterDeclaration' smart constructor.
data ParameterDeclaration =
  ParameterDeclaration'
    { _pdParameterKey         :: !(Maybe Text)
    , _pdParameterType        :: !(Maybe Text)
    , _pdParameterConstraints :: !(Maybe ParameterConstraints)
    , _pdDefaultValue         :: !(Maybe Text)
    , _pdNoEcho               :: !(Maybe Bool)
    , _pdDescription          :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdParameterKey' - The name that is associated with the parameter.
--
-- * 'pdParameterType' - The type of parameter.
--
-- * 'pdParameterConstraints' - The criteria that AWS CloudFormation uses to validate parameter values.
--
-- * 'pdDefaultValue' - The default value of the parameter.
--
-- * 'pdNoEcho' - Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
--
-- * 'pdDescription' - The description that is associate with the parameter.
parameterDeclaration
    :: ParameterDeclaration
parameterDeclaration =
  ParameterDeclaration'
    { _pdParameterKey = Nothing
    , _pdParameterType = Nothing
    , _pdParameterConstraints = Nothing
    , _pdDefaultValue = Nothing
    , _pdNoEcho = Nothing
    , _pdDescription = Nothing
    }


-- | The name that is associated with the parameter.
pdParameterKey :: Lens' ParameterDeclaration (Maybe Text)
pdParameterKey = lens _pdParameterKey (\ s a -> s{_pdParameterKey = a})

-- | The type of parameter.
pdParameterType :: Lens' ParameterDeclaration (Maybe Text)
pdParameterType = lens _pdParameterType (\ s a -> s{_pdParameterType = a})

-- | The criteria that AWS CloudFormation uses to validate parameter values.
pdParameterConstraints :: Lens' ParameterDeclaration (Maybe ParameterConstraints)
pdParameterConstraints = lens _pdParameterConstraints (\ s a -> s{_pdParameterConstraints = a})

-- | The default value of the parameter.
pdDefaultValue :: Lens' ParameterDeclaration (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\ s a -> s{_pdDefaultValue = a})

-- | Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
pdNoEcho :: Lens' ParameterDeclaration (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\ s a -> s{_pdNoEcho = a})

-- | The description that is associate with the parameter.
pdDescription :: Lens' ParameterDeclaration (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

instance FromXML ParameterDeclaration where
        parseXML x
          = ParameterDeclaration' <$>
              (x .@? "ParameterKey") <*> (x .@? "ParameterType")
                <*> (x .@? "ParameterConstraints")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "NoEcho")
                <*> (x .@? "Description")

instance Hashable ParameterDeclaration where

instance NFData ParameterDeclaration where

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a resource that contains the targeted resource.
--
--
--
-- /See:/ 'physicalResourceIdContextKeyValuePair' smart constructor.
data PhysicalResourceIdContextKeyValuePair =
  PhysicalResourceIdContextKeyValuePair'
    { _prickvpKey   :: !Text
    , _prickvpValue :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PhysicalResourceIdContextKeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prickvpKey' - The resource context key.
--
-- * 'prickvpValue' - The resource context value.
physicalResourceIdContextKeyValuePair
    :: Text -- ^ 'prickvpKey'
    -> Text -- ^ 'prickvpValue'
    -> PhysicalResourceIdContextKeyValuePair
physicalResourceIdContextKeyValuePair pKey_ pValue_ =
  PhysicalResourceIdContextKeyValuePair'
    {_prickvpKey = pKey_, _prickvpValue = pValue_}


-- | The resource context key.
prickvpKey :: Lens' PhysicalResourceIdContextKeyValuePair Text
prickvpKey = lens _prickvpKey (\ s a -> s{_prickvpKey = a})

-- | The resource context value.
prickvpValue :: Lens' PhysicalResourceIdContextKeyValuePair Text
prickvpValue = lens _prickvpValue (\ s a -> s{_prickvpValue = a})

instance FromXML
           PhysicalResourceIdContextKeyValuePair
         where
        parseXML x
          = PhysicalResourceIdContextKeyValuePair' <$>
              (x .@ "Key") <*> (x .@ "Value")

instance Hashable
           PhysicalResourceIdContextKeyValuePair
         where

instance NFData PhysicalResourceIdContextKeyValuePair
         where

-- | Information about a resource property whose actual value differs from its expected value, as defined in the stack template and any values specified as template parameters. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
--
-- /See:/ 'propertyDifference' smart constructor.
data PropertyDifference =
  PropertyDifference'
    { _pdPropertyPath   :: !Text
    , _pdExpectedValue  :: !Text
    , _pdActualValue    :: !Text
    , _pdDifferenceType :: !DifferenceType
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertyDifference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPropertyPath' - The fully-qualified path to the resource property.
--
-- * 'pdExpectedValue' - The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
--
-- * 'pdActualValue' - The actual property value of the resource property.
--
-- * 'pdDifferenceType' - The type of property difference.     * @ADD@ : A value has been added to a resource property that is an array or list data type.     * @REMOVE@ : The property has been removed from the current resource configuration.     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
propertyDifference
    :: Text -- ^ 'pdPropertyPath'
    -> Text -- ^ 'pdExpectedValue'
    -> Text -- ^ 'pdActualValue'
    -> DifferenceType -- ^ 'pdDifferenceType'
    -> PropertyDifference
propertyDifference pPropertyPath_ pExpectedValue_ pActualValue_ pDifferenceType_ =
  PropertyDifference'
    { _pdPropertyPath = pPropertyPath_
    , _pdExpectedValue = pExpectedValue_
    , _pdActualValue = pActualValue_
    , _pdDifferenceType = pDifferenceType_
    }


-- | The fully-qualified path to the resource property.
pdPropertyPath :: Lens' PropertyDifference Text
pdPropertyPath = lens _pdPropertyPath (\ s a -> s{_pdPropertyPath = a})

-- | The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
pdExpectedValue :: Lens' PropertyDifference Text
pdExpectedValue = lens _pdExpectedValue (\ s a -> s{_pdExpectedValue = a})

-- | The actual property value of the resource property.
pdActualValue :: Lens' PropertyDifference Text
pdActualValue = lens _pdActualValue (\ s a -> s{_pdActualValue = a})

-- | The type of property difference.     * @ADD@ : A value has been added to a resource property that is an array or list data type.     * @REMOVE@ : The property has been removed from the current resource configuration.     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
pdDifferenceType :: Lens' PropertyDifference DifferenceType
pdDifferenceType = lens _pdDifferenceType (\ s a -> s{_pdDifferenceType = a})

instance FromXML PropertyDifference where
        parseXML x
          = PropertyDifference' <$>
              (x .@ "PropertyPath") <*> (x .@ "ExpectedValue") <*>
                (x .@ "ActualValue")
                <*> (x .@ "DifferenceType")

instance Hashable PropertyDifference where

instance NFData PropertyDifference where

-- | The @ResourceChange@ structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.
--
--
--
-- /See:/ 'resourceChange' smart constructor.
data ResourceChange =
  ResourceChange'
    { _rcLogicalResourceId  :: !(Maybe Text)
    , _rcPhysicalResourceId :: !(Maybe Text)
    , _rcResourceType       :: !(Maybe Text)
    , _rcAction             :: !(Maybe ChangeAction)
    , _rcScope              :: !(Maybe [ResourceAttribute])
    , _rcDetails            :: !(Maybe [ResourceChangeDetail])
    , _rcReplacement        :: !(Maybe Replacement)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcLogicalResourceId' - The resource's logical ID, which is defined in the stack's template.
--
-- * 'rcPhysicalResourceId' - The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
--
-- * 'rcResourceType' - The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
--
-- * 'rcAction' - The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), or @Remove@ (deletes a resource).
--
-- * 'rcScope' - For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- * 'rcDetails' - For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
--
-- * 'rcReplacement' - For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ . If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
resourceChange
    :: ResourceChange
resourceChange =
  ResourceChange'
    { _rcLogicalResourceId = Nothing
    , _rcPhysicalResourceId = Nothing
    , _rcResourceType = Nothing
    , _rcAction = Nothing
    , _rcScope = Nothing
    , _rcDetails = Nothing
    , _rcReplacement = Nothing
    }


-- | The resource's logical ID, which is defined in the stack's template.
rcLogicalResourceId :: Lens' ResourceChange (Maybe Text)
rcLogicalResourceId = lens _rcLogicalResourceId (\ s a -> s{_rcLogicalResourceId = a})

-- | The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
rcPhysicalResourceId :: Lens' ResourceChange (Maybe Text)
rcPhysicalResourceId = lens _rcPhysicalResourceId (\ s a -> s{_rcPhysicalResourceId = a})

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
rcResourceType :: Lens' ResourceChange (Maybe Text)
rcResourceType = lens _rcResourceType (\ s a -> s{_rcResourceType = a})

-- | The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), or @Remove@ (deletes a resource).
rcAction :: Lens' ResourceChange (Maybe ChangeAction)
rcAction = lens _rcAction (\ s a -> s{_rcAction = a})

-- | For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
rcScope :: Lens' ResourceChange [ResourceAttribute]
rcScope = lens _rcScope (\ s a -> s{_rcScope = a}) . _Default . _Coerce

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
rcDetails :: Lens' ResourceChange [ResourceChangeDetail]
rcDetails = lens _rcDetails (\ s a -> s{_rcDetails = a}) . _Default . _Coerce

-- | For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ . If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
rcReplacement :: Lens' ResourceChange (Maybe Replacement)
rcReplacement = lens _rcReplacement (\ s a -> s{_rcReplacement = a})

instance FromXML ResourceChange where
        parseXML x
          = ResourceChange' <$>
              (x .@? "LogicalResourceId") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@? "ResourceType")
                <*> (x .@? "Action")
                <*>
                (x .@? "Scope" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Details" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Replacement")

instance Hashable ResourceChange where

instance NFData ResourceChange where

-- | For a resource with @Modify@ as the action, the @ResourceChange@ structure describes the changes AWS CloudFormation will make to that resource.
--
--
--
-- /See:/ 'resourceChangeDetail' smart constructor.
data ResourceChangeDetail =
  ResourceChangeDetail'
    { _rcdCausingEntity :: !(Maybe Text)
    , _rcdChangeSource  :: !(Maybe ChangeSource)
    , _rcdEvaluation    :: !(Maybe EvaluationType)
    , _rcdTarget        :: !(Maybe ResourceTargetDefinition)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceChangeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcdCausingEntity' - The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ). If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
--
-- * 'rcdChangeSource' - The group to which the @CausingEntity@ value belongs. There are five entity groups:     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .     * @DirectModification@ entities are changes that are made directly to the template.     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
--
-- * 'rcdEvaluation' - Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set. For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation. For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
--
-- * 'rcdTarget' - A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
resourceChangeDetail
    :: ResourceChangeDetail
resourceChangeDetail =
  ResourceChangeDetail'
    { _rcdCausingEntity = Nothing
    , _rcdChangeSource = Nothing
    , _rcdEvaluation = Nothing
    , _rcdTarget = Nothing
    }


-- | The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ). If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
rcdCausingEntity :: Lens' ResourceChangeDetail (Maybe Text)
rcdCausingEntity = lens _rcdCausingEntity (\ s a -> s{_rcdCausingEntity = a})

-- | The group to which the @CausingEntity@ value belongs. There are five entity groups:     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .     * @DirectModification@ entities are changes that are made directly to the template.     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
rcdChangeSource :: Lens' ResourceChangeDetail (Maybe ChangeSource)
rcdChangeSource = lens _rcdChangeSource (\ s a -> s{_rcdChangeSource = a})

-- | Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set. For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation. For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
rcdEvaluation :: Lens' ResourceChangeDetail (Maybe EvaluationType)
rcdEvaluation = lens _rcdEvaluation (\ s a -> s{_rcdEvaluation = a})

-- | A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
rcdTarget :: Lens' ResourceChangeDetail (Maybe ResourceTargetDefinition)
rcdTarget = lens _rcdTarget (\ s a -> s{_rcdTarget = a})

instance FromXML ResourceChangeDetail where
        parseXML x
          = ResourceChangeDetail' <$>
              (x .@? "CausingEntity") <*> (x .@? "ChangeSource")
                <*> (x .@? "Evaluation")
                <*> (x .@? "Target")

instance Hashable ResourceChangeDetail where

instance NFData ResourceChangeDetail where

-- | Describes the target resources of a specific type in your import template (for example, all @AWS::S3::Bucket@ resources) and the properties you can provide during the import to identify resources of that type.
--
--
--
-- /See:/ 'resourceIdentifierSummary' smart constructor.
data ResourceIdentifierSummary =
  ResourceIdentifierSummary'
    { _risResourceType        :: !(Maybe Text)
    , _risLogicalResourceIds  :: !(Maybe (List1 Text))
    , _risResourceIdentifiers :: !(Maybe [Text])
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceIdentifierSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risResourceType' - The template resource type of the target resources, such as @AWS::S3::Bucket@ .
--
-- * 'risLogicalResourceIds' - The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
--
-- * 'risResourceIdentifiers' - The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
resourceIdentifierSummary
    :: ResourceIdentifierSummary
resourceIdentifierSummary =
  ResourceIdentifierSummary'
    { _risResourceType = Nothing
    , _risLogicalResourceIds = Nothing
    , _risResourceIdentifiers = Nothing
    }


-- | The template resource type of the target resources, such as @AWS::S3::Bucket@ .
risResourceType :: Lens' ResourceIdentifierSummary (Maybe Text)
risResourceType = lens _risResourceType (\ s a -> s{_risResourceType = a})

-- | The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
risLogicalResourceIds :: Lens' ResourceIdentifierSummary (Maybe (NonEmpty Text))
risLogicalResourceIds = lens _risLogicalResourceIds (\ s a -> s{_risLogicalResourceIds = a}) . mapping _List1

-- | The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
risResourceIdentifiers :: Lens' ResourceIdentifierSummary [Text]
risResourceIdentifiers = lens _risResourceIdentifiers (\ s a -> s{_risResourceIdentifiers = a}) . _Default . _Coerce

instance FromXML ResourceIdentifierSummary where
        parseXML x
          = ResourceIdentifierSummary' <$>
              (x .@? "ResourceType") <*>
                (x .@? "LogicalResourceIds" .!@ mempty >>=
                   may (parseXMLList1 "member"))
                <*>
                (x .@? "ResourceIdentifiers" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable ResourceIdentifierSummary where

instance NFData ResourceIdentifierSummary where

-- | The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.
--
--
--
-- /See:/ 'resourceTargetDefinition' smart constructor.
data ResourceTargetDefinition =
  ResourceTargetDefinition'
    { _rtdAttribute          :: !(Maybe ResourceAttribute)
    , _rtdRequiresRecreation :: !(Maybe RequiresRecreation)
    , _rtdName               :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdAttribute' - Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- * 'rtdRequiresRecreation' - If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
--
-- * 'rtdName' - If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
resourceTargetDefinition
    :: ResourceTargetDefinition
resourceTargetDefinition =
  ResourceTargetDefinition'
    { _rtdAttribute = Nothing
    , _rtdRequiresRecreation = Nothing
    , _rtdName = Nothing
    }


-- | Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
rtdAttribute :: Lens' ResourceTargetDefinition (Maybe ResourceAttribute)
rtdAttribute = lens _rtdAttribute (\ s a -> s{_rtdAttribute = a})

-- | If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
rtdRequiresRecreation :: Lens' ResourceTargetDefinition (Maybe RequiresRecreation)
rtdRequiresRecreation = lens _rtdRequiresRecreation (\ s a -> s{_rtdRequiresRecreation = a})

-- | If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
rtdName :: Lens' ResourceTargetDefinition (Maybe Text)
rtdName = lens _rtdName (\ s a -> s{_rtdName = a})

instance FromXML ResourceTargetDefinition where
        parseXML x
          = ResourceTargetDefinition' <$>
              (x .@? "Attribute") <*> (x .@? "RequiresRecreation")
                <*> (x .@? "Name")

instance Hashable ResourceTargetDefinition where

instance NFData ResourceTargetDefinition where

-- | Describes the target resource of an import operation.
--
--
--
-- /See:/ 'resourceToImport' smart constructor.
data ResourceToImport =
  ResourceToImport'
    { _rtiResourceType       :: !Text
    , _rtiLogicalResourceId  :: !Text
    , _rtiResourceIdentifier :: !(Map Text Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceToImport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtiResourceType' - The type of resource to import into your stack, such as @AWS::S3::Bucket@ .
--
-- * 'rtiLogicalResourceId' - The logical ID of the target resource as specified in the template.
--
-- * 'rtiResourceIdentifier' - A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
resourceToImport
    :: Text -- ^ 'rtiResourceType'
    -> Text -- ^ 'rtiLogicalResourceId'
    -> ResourceToImport
resourceToImport pResourceType_ pLogicalResourceId_ =
  ResourceToImport'
    { _rtiResourceType = pResourceType_
    , _rtiLogicalResourceId = pLogicalResourceId_
    , _rtiResourceIdentifier = mempty
    }


-- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ .
rtiResourceType :: Lens' ResourceToImport Text
rtiResourceType = lens _rtiResourceType (\ s a -> s{_rtiResourceType = a})

-- | The logical ID of the target resource as specified in the template.
rtiLogicalResourceId :: Lens' ResourceToImport Text
rtiLogicalResourceId = lens _rtiLogicalResourceId (\ s a -> s{_rtiLogicalResourceId = a})

-- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
rtiResourceIdentifier :: Lens' ResourceToImport (HashMap Text Text)
rtiResourceIdentifier = lens _rtiResourceIdentifier (\ s a -> s{_rtiResourceIdentifier = a}) . _Map

instance Hashable ResourceToImport where

instance NFData ResourceToImport where

instance ToQuery ResourceToImport where
        toQuery ResourceToImport'{..}
          = mconcat
              ["ResourceType" =: _rtiResourceType,
               "LogicalResourceId" =: _rtiLogicalResourceId,
               "ResourceIdentifier" =:
                 toQueryMap "entry" "key" "value"
                   _rtiResourceIdentifier]

-- | Structure containing the rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
--
-- Rollback triggers enable you to have AWS CloudFormation monitor the state of your application during stack creation and updating, and to roll back that operation if the application breaches the threshold of any of the alarms you've specified. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-rollback-triggers.html Monitor and Roll Back Stack Operations> .
--
--
-- /See:/ 'rollbackConfiguration' smart constructor.
data RollbackConfiguration =
  RollbackConfiguration'
    { _rcRollbackTriggers        :: !(Maybe [RollbackTrigger])
    , _rcMonitoringTimeInMinutes :: !(Maybe Nat)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RollbackConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRollbackTriggers' - The triggers to monitor during stack creation or update actions.  By default, AWS CloudFormation saves the rollback triggers specified for a stack and applies them to any subsequent update operations for the stack, unless you specify otherwise. If you do specify rollback triggers for this parameter, those triggers replace any list of triggers previously specified for the stack. This means:     * To use the rollback triggers previously specified for this stack, if any, don't specify this parameter.     * To specify new or updated rollback triggers, you must specify /all/ the triggers that you want used for this stack, even triggers you've specifed before (for example, when creating the stack or during a previous stack update). Any triggers that you don't include in the updated list of triggers are no longer applied to the stack.     * To remove all currently specified triggers, specify an empty list for this parameter. If a specified trigger is missing, the entire stack operation fails and is rolled back.
--
-- * 'rcMonitoringTimeInMinutes' - The amount of time, in minutes, during which CloudFormation should monitor all the rollback triggers after the stack creation or update operation deploys all necessary resources. The default is 0 minutes. If you specify a monitoring period but do not specify any rollback triggers, CloudFormation still waits the specified period of time before cleaning up old resources after update operations. You can use this monitoring period to perform any manual stack validation desired, and manually cancel the stack creation or update (using <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack> , for example) as necessary. If you specify 0 for this parameter, CloudFormation still monitors the specified rollback triggers during stack creation and update operations. Then, for update operations, it begins disposing of old resources immediately once the operation completes.
rollbackConfiguration
    :: RollbackConfiguration
rollbackConfiguration =
  RollbackConfiguration'
    {_rcRollbackTriggers = Nothing, _rcMonitoringTimeInMinutes = Nothing}


-- | The triggers to monitor during stack creation or update actions.  By default, AWS CloudFormation saves the rollback triggers specified for a stack and applies them to any subsequent update operations for the stack, unless you specify otherwise. If you do specify rollback triggers for this parameter, those triggers replace any list of triggers previously specified for the stack. This means:     * To use the rollback triggers previously specified for this stack, if any, don't specify this parameter.     * To specify new or updated rollback triggers, you must specify /all/ the triggers that you want used for this stack, even triggers you've specifed before (for example, when creating the stack or during a previous stack update). Any triggers that you don't include in the updated list of triggers are no longer applied to the stack.     * To remove all currently specified triggers, specify an empty list for this parameter. If a specified trigger is missing, the entire stack operation fails and is rolled back.
rcRollbackTriggers :: Lens' RollbackConfiguration [RollbackTrigger]
rcRollbackTriggers = lens _rcRollbackTriggers (\ s a -> s{_rcRollbackTriggers = a}) . _Default . _Coerce

-- | The amount of time, in minutes, during which CloudFormation should monitor all the rollback triggers after the stack creation or update operation deploys all necessary resources. The default is 0 minutes. If you specify a monitoring period but do not specify any rollback triggers, CloudFormation still waits the specified period of time before cleaning up old resources after update operations. You can use this monitoring period to perform any manual stack validation desired, and manually cancel the stack creation or update (using <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack> , for example) as necessary. If you specify 0 for this parameter, CloudFormation still monitors the specified rollback triggers during stack creation and update operations. Then, for update operations, it begins disposing of old resources immediately once the operation completes.
rcMonitoringTimeInMinutes :: Lens' RollbackConfiguration (Maybe Natural)
rcMonitoringTimeInMinutes = lens _rcMonitoringTimeInMinutes (\ s a -> s{_rcMonitoringTimeInMinutes = a}) . mapping _Nat

instance FromXML RollbackConfiguration where
        parseXML x
          = RollbackConfiguration' <$>
              (x .@? "RollbackTriggers" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "MonitoringTimeInMinutes")

instance Hashable RollbackConfiguration where

instance NFData RollbackConfiguration where

instance ToQuery RollbackConfiguration where
        toQuery RollbackConfiguration'{..}
          = mconcat
              ["RollbackTriggers" =:
                 toQuery
                   (toQueryList "member" <$> _rcRollbackTriggers),
               "MonitoringTimeInMinutes" =:
                 _rcMonitoringTimeInMinutes]

-- | A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALARM state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation.
--
--
--
-- /See:/ 'rollbackTrigger' smart constructor.
data RollbackTrigger =
  RollbackTrigger'
    { _rtARN  :: !Text
    , _rtType :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtARN' - The Amazon Resource Name (ARN) of the rollback trigger. If a specified trigger is missing, the entire stack operation fails and is rolled back.
--
-- * 'rtType' - The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
rollbackTrigger
    :: Text -- ^ 'rtARN'
    -> Text -- ^ 'rtType'
    -> RollbackTrigger
rollbackTrigger pARN_ pType_ =
  RollbackTrigger' {_rtARN = pARN_, _rtType = pType_}


-- | The Amazon Resource Name (ARN) of the rollback trigger. If a specified trigger is missing, the entire stack operation fails and is rolled back.
rtARN :: Lens' RollbackTrigger Text
rtARN = lens _rtARN (\ s a -> s{_rtARN = a})

-- | The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
rtType :: Lens' RollbackTrigger Text
rtType = lens _rtType (\ s a -> s{_rtType = a})

instance FromXML RollbackTrigger where
        parseXML x
          = RollbackTrigger' <$> (x .@ "Arn") <*> (x .@ "Type")

instance Hashable RollbackTrigger where

instance NFData RollbackTrigger where

instance ToQuery RollbackTrigger where
        toQuery RollbackTrigger'{..}
          = mconcat ["Arn" =: _rtARN, "Type" =: _rtType]

-- | The Stack data type.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack =
  Stack'
    { _staDisableRollback             :: !(Maybe Bool)
    , _staLastUpdatedTime             :: !(Maybe ISO8601)
    , _staRootId                      :: !(Maybe Text)
    , _staNotificationARNs            :: !(Maybe [Text])
    , _staStackStatusReason           :: !(Maybe Text)
    , _staEnableTerminationProtection :: !(Maybe Bool)
    , _staDriftInformation            :: !(Maybe StackDriftInformation)
    , _staChangeSetId                 :: !(Maybe Text)
    , _staDeletionTime                :: !(Maybe ISO8601)
    , _staOutputs                     :: !(Maybe [Output])
    , _staParameters                  :: !(Maybe [Parameter])
    , _staStackId                     :: !(Maybe Text)
    , _staDescription                 :: !(Maybe Text)
    , _staCapabilities                :: !(Maybe [Capability])
    , _staRollbackConfiguration       :: !(Maybe RollbackConfiguration)
    , _staTags                        :: !(Maybe [Tag])
    , _staTimeoutInMinutes            :: !(Maybe Nat)
    , _staParentId                    :: !(Maybe Text)
    , _staRoleARN                     :: !(Maybe Text)
    , _staStackName                   :: !Text
    , _staCreationTime                :: !ISO8601
    , _staStackStatus                 :: !StackStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staDisableRollback' - Boolean to enable or disable rollback on stack creation failures:     * @true@ : disable rollback     * @false@ : enable rollback
--
-- * 'staLastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- * 'staRootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'staNotificationARNs' - SNS topic ARNs to which stack related events are published.
--
-- * 'staStackStatusReason' - Success/failure message associated with the stack status.
--
-- * 'staEnableTerminationProtection' - Whether termination protection is enabled for the stack. For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- * 'staDriftInformation' - Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'staChangeSetId' - The unique ID of the change set.
--
-- * 'staDeletionTime' - The time the stack was deleted.
--
-- * 'staOutputs' - A list of output structures.
--
-- * 'staParameters' - A list of @Parameter@ structures.
--
-- * 'staStackId' - Unique identifier of the stack.
--
-- * 'staDescription' - A user-defined description associated with the stack.
--
-- * 'staCapabilities' - The capabilities allowed in the stack.
--
-- * 'staRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'staTags' - A list of @Tag@ s that specify information about the stack.
--
-- * 'staTimeoutInMinutes' - The amount of time within which stack creation should complete.
--
-- * 'staParentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'staRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
--
-- * 'staStackName' - The name associated with the stack.
--
-- * 'staCreationTime' - The time at which the stack was created.
--
-- * 'staStackStatus' - Current status of the stack.
stack
    :: Text -- ^ 'staStackName'
    -> UTCTime -- ^ 'staCreationTime'
    -> StackStatus -- ^ 'staStackStatus'
    -> Stack
stack pStackName_ pCreationTime_ pStackStatus_ =
  Stack'
    { _staDisableRollback = Nothing
    , _staLastUpdatedTime = Nothing
    , _staRootId = Nothing
    , _staNotificationARNs = Nothing
    , _staStackStatusReason = Nothing
    , _staEnableTerminationProtection = Nothing
    , _staDriftInformation = Nothing
    , _staChangeSetId = Nothing
    , _staDeletionTime = Nothing
    , _staOutputs = Nothing
    , _staParameters = Nothing
    , _staStackId = Nothing
    , _staDescription = Nothing
    , _staCapabilities = Nothing
    , _staRollbackConfiguration = Nothing
    , _staTags = Nothing
    , _staTimeoutInMinutes = Nothing
    , _staParentId = Nothing
    , _staRoleARN = Nothing
    , _staStackName = pStackName_
    , _staCreationTime = _Time # pCreationTime_
    , _staStackStatus = pStackStatus_
    }


-- | Boolean to enable or disable rollback on stack creation failures:     * @true@ : disable rollback     * @false@ : enable rollback
staDisableRollback :: Lens' Stack (Maybe Bool)
staDisableRollback = lens _staDisableRollback (\ s a -> s{_staDisableRollback = a})

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
staLastUpdatedTime :: Lens' Stack (Maybe UTCTime)
staLastUpdatedTime = lens _staLastUpdatedTime (\ s a -> s{_staLastUpdatedTime = a}) . mapping _Time

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
staRootId :: Lens' Stack (Maybe Text)
staRootId = lens _staRootId (\ s a -> s{_staRootId = a})

-- | SNS topic ARNs to which stack related events are published.
staNotificationARNs :: Lens' Stack [Text]
staNotificationARNs = lens _staNotificationARNs (\ s a -> s{_staNotificationARNs = a}) . _Default . _Coerce

-- | Success/failure message associated with the stack status.
staStackStatusReason :: Lens' Stack (Maybe Text)
staStackStatusReason = lens _staStackStatusReason (\ s a -> s{_staStackStatusReason = a})

-- | Whether termination protection is enabled for the stack. For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
staEnableTerminationProtection :: Lens' Stack (Maybe Bool)
staEnableTerminationProtection = lens _staEnableTerminationProtection (\ s a -> s{_staEnableTerminationProtection = a})

-- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
staDriftInformation :: Lens' Stack (Maybe StackDriftInformation)
staDriftInformation = lens _staDriftInformation (\ s a -> s{_staDriftInformation = a})

-- | The unique ID of the change set.
staChangeSetId :: Lens' Stack (Maybe Text)
staChangeSetId = lens _staChangeSetId (\ s a -> s{_staChangeSetId = a})

-- | The time the stack was deleted.
staDeletionTime :: Lens' Stack (Maybe UTCTime)
staDeletionTime = lens _staDeletionTime (\ s a -> s{_staDeletionTime = a}) . mapping _Time

-- | A list of output structures.
staOutputs :: Lens' Stack [Output]
staOutputs = lens _staOutputs (\ s a -> s{_staOutputs = a}) . _Default . _Coerce

-- | A list of @Parameter@ structures.
staParameters :: Lens' Stack [Parameter]
staParameters = lens _staParameters (\ s a -> s{_staParameters = a}) . _Default . _Coerce

-- | Unique identifier of the stack.
staStackId :: Lens' Stack (Maybe Text)
staStackId = lens _staStackId (\ s a -> s{_staStackId = a})

-- | A user-defined description associated with the stack.
staDescription :: Lens' Stack (Maybe Text)
staDescription = lens _staDescription (\ s a -> s{_staDescription = a})

-- | The capabilities allowed in the stack.
staCapabilities :: Lens' Stack [Capability]
staCapabilities = lens _staCapabilities (\ s a -> s{_staCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
staRollbackConfiguration :: Lens' Stack (Maybe RollbackConfiguration)
staRollbackConfiguration = lens _staRollbackConfiguration (\ s a -> s{_staRollbackConfiguration = a})

-- | A list of @Tag@ s that specify information about the stack.
staTags :: Lens' Stack [Tag]
staTags = lens _staTags (\ s a -> s{_staTags = a}) . _Default . _Coerce

-- | The amount of time within which stack creation should complete.
staTimeoutInMinutes :: Lens' Stack (Maybe Natural)
staTimeoutInMinutes = lens _staTimeoutInMinutes (\ s a -> s{_staTimeoutInMinutes = a}) . mapping _Nat

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
staParentId :: Lens' Stack (Maybe Text)
staParentId = lens _staParentId (\ s a -> s{_staParentId = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
staRoleARN :: Lens' Stack (Maybe Text)
staRoleARN = lens _staRoleARN (\ s a -> s{_staRoleARN = a})

-- | The name associated with the stack.
staStackName :: Lens' Stack Text
staStackName = lens _staStackName (\ s a -> s{_staStackName = a})

-- | The time at which the stack was created.
staCreationTime :: Lens' Stack UTCTime
staCreationTime = lens _staCreationTime (\ s a -> s{_staCreationTime = a}) . _Time

-- | Current status of the stack.
staStackStatus :: Lens' Stack StackStatus
staStackStatus = lens _staStackStatus (\ s a -> s{_staStackStatus = a})

instance FromXML Stack where
        parseXML x
          = Stack' <$>
              (x .@? "DisableRollback") <*>
                (x .@? "LastUpdatedTime")
                <*> (x .@? "RootId")
                <*>
                (x .@? "NotificationARNs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackStatusReason")
                <*> (x .@? "EnableTerminationProtection")
                <*> (x .@? "DriftInformation")
                <*> (x .@? "ChangeSetId")
                <*> (x .@? "DeletionTime")
                <*>
                (x .@? "Outputs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*>
                (x .@? "Capabilities" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "RollbackConfiguration")
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TimeoutInMinutes")
                <*> (x .@? "ParentId")
                <*> (x .@? "RoleARN")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

instance Hashable Stack where

instance NFData Stack where

-- | Contains information about whether the stack's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--
-- /See:/ 'stackDriftInformation' smart constructor.
data StackDriftInformation =
  StackDriftInformation'
    { _sdiLastCheckTimestamp :: !(Maybe ISO8601)
    , _sdiStackDriftStatus   :: !StackDriftStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackDriftInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiLastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
--
-- * 'sdiStackDriftStatus' - Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
stackDriftInformation
    :: StackDriftStatus -- ^ 'sdiStackDriftStatus'
    -> StackDriftInformation
stackDriftInformation pStackDriftStatus_ =
  StackDriftInformation'
    { _sdiLastCheckTimestamp = Nothing
    , _sdiStackDriftStatus = pStackDriftStatus_
    }


-- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
sdiLastCheckTimestamp :: Lens' StackDriftInformation (Maybe UTCTime)
sdiLastCheckTimestamp = lens _sdiLastCheckTimestamp (\ s a -> s{_sdiLastCheckTimestamp = a}) . mapping _Time

-- | Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
sdiStackDriftStatus :: Lens' StackDriftInformation StackDriftStatus
sdiStackDriftStatus = lens _sdiStackDriftStatus (\ s a -> s{_sdiStackDriftStatus = a})

instance FromXML StackDriftInformation where
        parseXML x
          = StackDriftInformation' <$>
              (x .@? "LastCheckTimestamp") <*>
                (x .@ "StackDriftStatus")

instance Hashable StackDriftInformation where

instance NFData StackDriftInformation where

-- | Contains information about whether the stack's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--
-- /See:/ 'stackDriftInformationSummary' smart constructor.
data StackDriftInformationSummary =
  StackDriftInformationSummary'
    { _sdisLastCheckTimestamp :: !(Maybe ISO8601)
    , _sdisStackDriftStatus   :: !StackDriftStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackDriftInformationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdisLastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
--
-- * 'sdisStackDriftStatus' - Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
stackDriftInformationSummary
    :: StackDriftStatus -- ^ 'sdisStackDriftStatus'
    -> StackDriftInformationSummary
stackDriftInformationSummary pStackDriftStatus_ =
  StackDriftInformationSummary'
    { _sdisLastCheckTimestamp = Nothing
    , _sdisStackDriftStatus = pStackDriftStatus_
    }


-- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
sdisLastCheckTimestamp :: Lens' StackDriftInformationSummary (Maybe UTCTime)
sdisLastCheckTimestamp = lens _sdisLastCheckTimestamp (\ s a -> s{_sdisLastCheckTimestamp = a}) . mapping _Time

-- | Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
sdisStackDriftStatus :: Lens' StackDriftInformationSummary StackDriftStatus
sdisStackDriftStatus = lens _sdisStackDriftStatus (\ s a -> s{_sdisStackDriftStatus = a})

instance FromXML StackDriftInformationSummary where
        parseXML x
          = StackDriftInformationSummary' <$>
              (x .@? "LastCheckTimestamp") <*>
                (x .@ "StackDriftStatus")

instance Hashable StackDriftInformationSummary where

instance NFData StackDriftInformationSummary where

-- | The StackEvent data type.
--
--
--
-- /See:/ 'stackEvent' smart constructor.
data StackEvent =
  StackEvent'
    { _seLogicalResourceId    :: !(Maybe Text)
    , _sePhysicalResourceId   :: !(Maybe Text)
    , _seResourceType         :: !(Maybe Text)
    , _seResourceStatusReason :: !(Maybe Text)
    , _seResourceProperties   :: !(Maybe Text)
    , _seResourceStatus       :: !(Maybe ResourceStatus)
    , _seClientRequestToken   :: !(Maybe Text)
    , _seStackId              :: !Text
    , _seEventId              :: !Text
    , _seStackName            :: !Text
    , _seTimestamp            :: !ISO8601
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'sePhysicalResourceId' - The name or unique identifier associated with the physical instance of the resource.
--
-- * 'seResourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'seResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'seResourceProperties' - BLOB of the properties used to create the resource.
--
-- * 'seResourceStatus' - Current status of the resource.
--
-- * 'seClientRequestToken' - The token passed to the operation that generated this event. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- * 'seStackId' - The unique ID name of the instance of the stack.
--
-- * 'seEventId' - The unique ID of this event.
--
-- * 'seStackName' - The name associated with a stack.
--
-- * 'seTimestamp' - Time the status was updated.
stackEvent
    :: Text -- ^ 'seStackId'
    -> Text -- ^ 'seEventId'
    -> Text -- ^ 'seStackName'
    -> UTCTime -- ^ 'seTimestamp'
    -> StackEvent
stackEvent pStackId_ pEventId_ pStackName_ pTimestamp_ =
  StackEvent'
    { _seLogicalResourceId = Nothing
    , _sePhysicalResourceId = Nothing
    , _seResourceType = Nothing
    , _seResourceStatusReason = Nothing
    , _seResourceProperties = Nothing
    , _seResourceStatus = Nothing
    , _seClientRequestToken = Nothing
    , _seStackId = pStackId_
    , _seEventId = pEventId_
    , _seStackName = pStackName_
    , _seTimestamp = _Time # pTimestamp_
    }


-- | The logical name of the resource specified in the template.
seLogicalResourceId :: Lens' StackEvent (Maybe Text)
seLogicalResourceId = lens _seLogicalResourceId (\ s a -> s{_seLogicalResourceId = a})

-- | The name or unique identifier associated with the physical instance of the resource.
sePhysicalResourceId :: Lens' StackEvent (Maybe Text)
sePhysicalResourceId = lens _sePhysicalResourceId (\ s a -> s{_sePhysicalResourceId = a})

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
seResourceType :: Lens' StackEvent (Maybe Text)
seResourceType = lens _seResourceType (\ s a -> s{_seResourceType = a})

-- | Success/failure message associated with the resource.
seResourceStatusReason :: Lens' StackEvent (Maybe Text)
seResourceStatusReason = lens _seResourceStatusReason (\ s a -> s{_seResourceStatusReason = a})

-- | BLOB of the properties used to create the resource.
seResourceProperties :: Lens' StackEvent (Maybe Text)
seResourceProperties = lens _seResourceProperties (\ s a -> s{_seResourceProperties = a})

-- | Current status of the resource.
seResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
seResourceStatus = lens _seResourceStatus (\ s a -> s{_seResourceStatus = a})

-- | The token passed to the operation that generated this event. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
seClientRequestToken :: Lens' StackEvent (Maybe Text)
seClientRequestToken = lens _seClientRequestToken (\ s a -> s{_seClientRequestToken = a})

-- | The unique ID name of the instance of the stack.
seStackId :: Lens' StackEvent Text
seStackId = lens _seStackId (\ s a -> s{_seStackId = a})

-- | The unique ID of this event.
seEventId :: Lens' StackEvent Text
seEventId = lens _seEventId (\ s a -> s{_seEventId = a})

-- | The name associated with a stack.
seStackName :: Lens' StackEvent Text
seStackName = lens _seStackName (\ s a -> s{_seStackName = a})

-- | Time the status was updated.
seTimestamp :: Lens' StackEvent UTCTime
seTimestamp = lens _seTimestamp (\ s a -> s{_seTimestamp = a}) . _Time

instance FromXML StackEvent where
        parseXML x
          = StackEvent' <$>
              (x .@? "LogicalResourceId") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@? "ResourceType")
                <*> (x .@? "ResourceStatusReason")
                <*> (x .@? "ResourceProperties")
                <*> (x .@? "ResourceStatus")
                <*> (x .@? "ClientRequestToken")
                <*> (x .@ "StackId")
                <*> (x .@ "EventId")
                <*> (x .@ "StackName")
                <*> (x .@ "Timestamp")

instance Hashable StackEvent where

instance NFData StackEvent where

-- | An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
--
--
-- /See:/ 'stackInstance' smart constructor.
data StackInstance =
  StackInstance'
    { _siStatus                  :: !(Maybe StackInstanceStatus)
    , _siLastDriftCheckTimestamp :: !(Maybe ISO8601)
    , _siAccount                 :: !(Maybe Text)
    , _siDriftStatus             :: !(Maybe StackDriftStatus)
    , _siOrganizationalUnitId    :: !(Maybe Text)
    , _siRegion                  :: !(Maybe Text)
    , _siStatusReason            :: !(Maybe Text)
    , _siStackId                 :: !(Maybe Text)
    , _siParameterOverrides      :: !(Maybe [Parameter])
    , _siStackSetId              :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
--
-- * 'siLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- * 'siAccount' - [Self-managed permissions] The name of the AWS account that the stack instance is associated with.
--
-- * 'siDriftStatus' - Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'siOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID that the stack instance is associated with.
--
-- * 'siRegion' - The name of the AWS region that the stack instance is associated with.
--
-- * 'siStatusReason' - The explanation for the specific status code that is assigned to this stack instance.
--
-- * 'siStackId' - The ID of the stack instance.
--
-- * 'siParameterOverrides' - A list of parameters from the stack set template whose values have been overridden in this stack instance.
--
-- * 'siStackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
stackInstance
    :: StackInstance
stackInstance =
  StackInstance'
    { _siStatus = Nothing
    , _siLastDriftCheckTimestamp = Nothing
    , _siAccount = Nothing
    , _siDriftStatus = Nothing
    , _siOrganizationalUnitId = Nothing
    , _siRegion = Nothing
    , _siStatusReason = Nothing
    , _siStackId = Nothing
    , _siParameterOverrides = Nothing
    , _siStackSetId = Nothing
    }


-- | The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
siStatus :: Lens' StackInstance (Maybe StackInstanceStatus)
siStatus = lens _siStatus (\ s a -> s{_siStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
siLastDriftCheckTimestamp :: Lens' StackInstance (Maybe UTCTime)
siLastDriftCheckTimestamp = lens _siLastDriftCheckTimestamp (\ s a -> s{_siLastDriftCheckTimestamp = a}) . mapping _Time

-- | [Self-managed permissions] The name of the AWS account that the stack instance is associated with.
siAccount :: Lens' StackInstance (Maybe Text)
siAccount = lens _siAccount (\ s a -> s{_siAccount = a})

-- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
siDriftStatus :: Lens' StackInstance (Maybe StackDriftStatus)
siDriftStatus = lens _siDriftStatus (\ s a -> s{_siDriftStatus = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID that the stack instance is associated with.
siOrganizationalUnitId :: Lens' StackInstance (Maybe Text)
siOrganizationalUnitId = lens _siOrganizationalUnitId (\ s a -> s{_siOrganizationalUnitId = a})

-- | The name of the AWS region that the stack instance is associated with.
siRegion :: Lens' StackInstance (Maybe Text)
siRegion = lens _siRegion (\ s a -> s{_siRegion = a})

-- | The explanation for the specific status code that is assigned to this stack instance.
siStatusReason :: Lens' StackInstance (Maybe Text)
siStatusReason = lens _siStatusReason (\ s a -> s{_siStatusReason = a})

-- | The ID of the stack instance.
siStackId :: Lens' StackInstance (Maybe Text)
siStackId = lens _siStackId (\ s a -> s{_siStackId = a})

-- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
siParameterOverrides :: Lens' StackInstance [Parameter]
siParameterOverrides = lens _siParameterOverrides (\ s a -> s{_siParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set that the stack instance is associated with.
siStackSetId :: Lens' StackInstance (Maybe Text)
siStackSetId = lens _siStackSetId (\ s a -> s{_siStackSetId = a})

instance FromXML StackInstance where
        parseXML x
          = StackInstance' <$>
              (x .@? "Status") <*>
                (x .@? "LastDriftCheckTimestamp")
                <*> (x .@? "Account")
                <*> (x .@? "DriftStatus")
                <*> (x .@? "OrganizationalUnitId")
                <*> (x .@? "Region")
                <*> (x .@? "StatusReason")
                <*> (x .@? "StackId")
                <*>
                (x .@? "ParameterOverrides" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackSetId")

instance Hashable StackInstance where

instance NFData StackInstance where

-- | The structure that contains summary information about a stack instance.
--
--
--
-- /See:/ 'stackInstanceSummary' smart constructor.
data StackInstanceSummary =
  StackInstanceSummary'
    { _sisStatus                  :: !(Maybe StackInstanceStatus)
    , _sisLastDriftCheckTimestamp :: !(Maybe ISO8601)
    , _sisAccount                 :: !(Maybe Text)
    , _sisDriftStatus             :: !(Maybe StackDriftStatus)
    , _sisOrganizationalUnitId    :: !(Maybe Text)
    , _sisRegion                  :: !(Maybe Text)
    , _sisStatusReason            :: !(Maybe Text)
    , _sisStackId                 :: !(Maybe Text)
    , _sisStackSetId              :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackInstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
--
-- * 'sisLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- * 'sisAccount' - [Self-managed permissions] The name of the AWS account that the stack instance is associated with.
--
-- * 'sisDriftStatus' - Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'sisOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID that the stack instance is associated with.
--
-- * 'sisRegion' - The name of the AWS region that the stack instance is associated with.
--
-- * 'sisStatusReason' - The explanation for the specific status code assigned to this stack instance.
--
-- * 'sisStackId' - The ID of the stack instance.
--
-- * 'sisStackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
stackInstanceSummary
    :: StackInstanceSummary
stackInstanceSummary =
  StackInstanceSummary'
    { _sisStatus = Nothing
    , _sisLastDriftCheckTimestamp = Nothing
    , _sisAccount = Nothing
    , _sisDriftStatus = Nothing
    , _sisOrganizationalUnitId = Nothing
    , _sisRegion = Nothing
    , _sisStatusReason = Nothing
    , _sisStackId = Nothing
    , _sisStackSetId = Nothing
    }


-- | The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
sisStatus :: Lens' StackInstanceSummary (Maybe StackInstanceStatus)
sisStatus = lens _sisStatus (\ s a -> s{_sisStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
sisLastDriftCheckTimestamp :: Lens' StackInstanceSummary (Maybe UTCTime)
sisLastDriftCheckTimestamp = lens _sisLastDriftCheckTimestamp (\ s a -> s{_sisLastDriftCheckTimestamp = a}) . mapping _Time

-- | [Self-managed permissions] The name of the AWS account that the stack instance is associated with.
sisAccount :: Lens' StackInstanceSummary (Maybe Text)
sisAccount = lens _sisAccount (\ s a -> s{_sisAccount = a})

-- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
sisDriftStatus :: Lens' StackInstanceSummary (Maybe StackDriftStatus)
sisDriftStatus = lens _sisDriftStatus (\ s a -> s{_sisDriftStatus = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID that the stack instance is associated with.
sisOrganizationalUnitId :: Lens' StackInstanceSummary (Maybe Text)
sisOrganizationalUnitId = lens _sisOrganizationalUnitId (\ s a -> s{_sisOrganizationalUnitId = a})

-- | The name of the AWS region that the stack instance is associated with.
sisRegion :: Lens' StackInstanceSummary (Maybe Text)
sisRegion = lens _sisRegion (\ s a -> s{_sisRegion = a})

-- | The explanation for the specific status code assigned to this stack instance.
sisStatusReason :: Lens' StackInstanceSummary (Maybe Text)
sisStatusReason = lens _sisStatusReason (\ s a -> s{_sisStatusReason = a})

-- | The ID of the stack instance.
sisStackId :: Lens' StackInstanceSummary (Maybe Text)
sisStackId = lens _sisStackId (\ s a -> s{_sisStackId = a})

-- | The name or unique ID of the stack set that the stack instance is associated with.
sisStackSetId :: Lens' StackInstanceSummary (Maybe Text)
sisStackSetId = lens _sisStackSetId (\ s a -> s{_sisStackSetId = a})

instance FromXML StackInstanceSummary where
        parseXML x
          = StackInstanceSummary' <$>
              (x .@? "Status") <*>
                (x .@? "LastDriftCheckTimestamp")
                <*> (x .@? "Account")
                <*> (x .@? "DriftStatus")
                <*> (x .@? "OrganizationalUnitId")
                <*> (x .@? "Region")
                <*> (x .@? "StatusReason")
                <*> (x .@? "StackId")
                <*> (x .@? "StackSetId")

instance Hashable StackInstanceSummary where

instance NFData StackInstanceSummary where

-- | The StackResource data type.
--
--
--
-- /See:/ 'stackResource' smart constructor.
data StackResource =
  StackResource'
    { _srPhysicalResourceId   :: !(Maybe Text)
    , _srResourceStatusReason :: !(Maybe Text)
    , _srDriftInformation     :: !(Maybe StackResourceDriftInformation)
    , _srStackId              :: !(Maybe Text)
    , _srDescription          :: !(Maybe Text)
    , _srStackName            :: !(Maybe Text)
    , _srLogicalResourceId    :: !Text
    , _srResourceType         :: !Text
    , _srTimestamp            :: !ISO8601
    , _srResourceStatus       :: !ResourceStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'srResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'srDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'srStackId' - Unique identifier of the stack.
--
-- * 'srDescription' - User defined description associated with the resource.
--
-- * 'srStackName' - The name associated with the stack.
--
-- * 'srLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srResourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'srTimestamp' - Time the status was updated.
--
-- * 'srResourceStatus' - Current status of the resource.
stackResource
    :: Text -- ^ 'srLogicalResourceId'
    -> Text -- ^ 'srResourceType'
    -> UTCTime -- ^ 'srTimestamp'
    -> ResourceStatus -- ^ 'srResourceStatus'
    -> StackResource
stackResource pLogicalResourceId_ pResourceType_ pTimestamp_ pResourceStatus_ =
  StackResource'
    { _srPhysicalResourceId = Nothing
    , _srResourceStatusReason = Nothing
    , _srDriftInformation = Nothing
    , _srStackId = Nothing
    , _srDescription = Nothing
    , _srStackName = Nothing
    , _srLogicalResourceId = pLogicalResourceId_
    , _srResourceType = pResourceType_
    , _srTimestamp = _Time # pTimestamp_
    , _srResourceStatus = pResourceStatus_
    }


-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
srPhysicalResourceId :: Lens' StackResource (Maybe Text)
srPhysicalResourceId = lens _srPhysicalResourceId (\ s a -> s{_srPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
srResourceStatusReason :: Lens' StackResource (Maybe Text)
srResourceStatusReason = lens _srResourceStatusReason (\ s a -> s{_srResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
srDriftInformation :: Lens' StackResource (Maybe StackResourceDriftInformation)
srDriftInformation = lens _srDriftInformation (\ s a -> s{_srDriftInformation = a})

-- | Unique identifier of the stack.
srStackId :: Lens' StackResource (Maybe Text)
srStackId = lens _srStackId (\ s a -> s{_srStackId = a})

-- | User defined description associated with the resource.
srDescription :: Lens' StackResource (Maybe Text)
srDescription = lens _srDescription (\ s a -> s{_srDescription = a})

-- | The name associated with the stack.
srStackName :: Lens' StackResource (Maybe Text)
srStackName = lens _srStackName (\ s a -> s{_srStackName = a})

-- | The logical name of the resource specified in the template.
srLogicalResourceId :: Lens' StackResource Text
srLogicalResourceId = lens _srLogicalResourceId (\ s a -> s{_srLogicalResourceId = a})

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
srResourceType :: Lens' StackResource Text
srResourceType = lens _srResourceType (\ s a -> s{_srResourceType = a})

-- | Time the status was updated.
srTimestamp :: Lens' StackResource UTCTime
srTimestamp = lens _srTimestamp (\ s a -> s{_srTimestamp = a}) . _Time

-- | Current status of the resource.
srResourceStatus :: Lens' StackResource ResourceStatus
srResourceStatus = lens _srResourceStatus (\ s a -> s{_srResourceStatus = a})

instance FromXML StackResource where
        parseXML x
          = StackResource' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "DriftInformation")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "Timestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResource where

instance NFData StackResource where

-- | Contains detailed information about the specified stack resource.
--
--
--
-- /See:/ 'stackResourceDetail' smart constructor.
data StackResourceDetail =
  StackResourceDetail'
    { _sPhysicalResourceId   :: !(Maybe Text)
    , _sResourceStatusReason :: !(Maybe Text)
    , _sDriftInformation     :: !(Maybe StackResourceDriftInformation)
    , _sMetadata             :: !(Maybe Text)
    , _sStackId              :: !(Maybe Text)
    , _sDescription          :: !(Maybe Text)
    , _sStackName            :: !(Maybe Text)
    , _sLogicalResourceId    :: !Text
    , _sResourceType         :: !Text
    , _sLastUpdatedTimestamp :: !ISO8601
    , _sResourceStatus       :: !ResourceStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'sResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'sDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'sMetadata' - The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
--
-- * 'sStackId' - Unique identifier of the stack.
--
-- * 'sDescription' - User defined description associated with the resource.
--
-- * 'sStackName' - The name associated with the stack.
--
-- * 'sLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'sResourceType' - Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'sLastUpdatedTimestamp' - Time the status was updated.
--
-- * 'sResourceStatus' - Current status of the resource.
stackResourceDetail
    :: Text -- ^ 'sLogicalResourceId'
    -> Text -- ^ 'sResourceType'
    -> UTCTime -- ^ 'sLastUpdatedTimestamp'
    -> ResourceStatus -- ^ 'sResourceStatus'
    -> StackResourceDetail
stackResourceDetail pLogicalResourceId_ pResourceType_ pLastUpdatedTimestamp_ pResourceStatus_ =
  StackResourceDetail'
    { _sPhysicalResourceId = Nothing
    , _sResourceStatusReason = Nothing
    , _sDriftInformation = Nothing
    , _sMetadata = Nothing
    , _sStackId = Nothing
    , _sDescription = Nothing
    , _sStackName = Nothing
    , _sLogicalResourceId = pLogicalResourceId_
    , _sResourceType = pResourceType_
    , _sLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_
    , _sResourceStatus = pResourceStatus_
    }


-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
sPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
sPhysicalResourceId = lens _sPhysicalResourceId (\ s a -> s{_sPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
sResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
sResourceStatusReason = lens _sResourceStatusReason (\ s a -> s{_sResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
sDriftInformation :: Lens' StackResourceDetail (Maybe StackResourceDriftInformation)
sDriftInformation = lens _sDriftInformation (\ s a -> s{_sDriftInformation = a})

-- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
sMetadata :: Lens' StackResourceDetail (Maybe Text)
sMetadata = lens _sMetadata (\ s a -> s{_sMetadata = a})

-- | Unique identifier of the stack.
sStackId :: Lens' StackResourceDetail (Maybe Text)
sStackId = lens _sStackId (\ s a -> s{_sStackId = a})

-- | User defined description associated with the resource.
sDescription :: Lens' StackResourceDetail (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The name associated with the stack.
sStackName :: Lens' StackResourceDetail (Maybe Text)
sStackName = lens _sStackName (\ s a -> s{_sStackName = a})

-- | The logical name of the resource specified in the template.
sLogicalResourceId :: Lens' StackResourceDetail Text
sLogicalResourceId = lens _sLogicalResourceId (\ s a -> s{_sLogicalResourceId = a})

-- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
sResourceType :: Lens' StackResourceDetail Text
sResourceType = lens _sResourceType (\ s a -> s{_sResourceType = a})

-- | Time the status was updated.
sLastUpdatedTimestamp :: Lens' StackResourceDetail UTCTime
sLastUpdatedTimestamp = lens _sLastUpdatedTimestamp (\ s a -> s{_sLastUpdatedTimestamp = a}) . _Time

-- | Current status of the resource.
sResourceStatus :: Lens' StackResourceDetail ResourceStatus
sResourceStatus = lens _sResourceStatus (\ s a -> s{_sResourceStatus = a})

instance FromXML StackResourceDetail where
        parseXML x
          = StackResourceDetail' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "DriftInformation")
                <*> (x .@? "Metadata")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResourceDetail where

instance NFData StackResourceDetail where

-- | Contains the drift information for a resource that has been checked for drift. This includes actual and expected property values for resources in which AWS CloudFormation has detected drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
--
--
-- /See:/ 'stackResourceDrift' smart constructor.
data StackResourceDrift =
  StackResourceDrift'
    { _srdActualProperties :: !(Maybe Text)
    , _srdPhysicalResourceId :: !(Maybe Text)
    , _srdPhysicalResourceIdContext :: !(Maybe [PhysicalResourceIdContextKeyValuePair])
    , _srdPropertyDifferences :: !(Maybe [PropertyDifference])
    , _srdExpectedProperties :: !(Maybe Text)
    , _srdStackId :: !Text
    , _srdLogicalResourceId :: !Text
    , _srdResourceType :: !Text
    , _srdStackResourceDriftStatus :: !StackResourceDriftStatus
    , _srdTimestamp :: !ISO8601
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResourceDrift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdActualProperties' - A JSON structure containing the actual property values of the stack resource. For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- * 'srdPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'srdPhysicalResourceIdContext' - Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
--
-- * 'srdPropertyDifferences' - A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
--
-- * 'srdExpectedProperties' - A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.  For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- * 'srdStackId' - The ID of the stack.
--
-- * 'srdLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srdResourceType' - The type of the resource.
--
-- * 'srdStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
-- * 'srdTimestamp' - Time at which AWS CloudFormation performed drift detection on the stack resource.
stackResourceDrift
    :: Text -- ^ 'srdStackId'
    -> Text -- ^ 'srdLogicalResourceId'
    -> Text -- ^ 'srdResourceType'
    -> StackResourceDriftStatus -- ^ 'srdStackResourceDriftStatus'
    -> UTCTime -- ^ 'srdTimestamp'
    -> StackResourceDrift
stackResourceDrift pStackId_ pLogicalResourceId_ pResourceType_ pStackResourceDriftStatus_ pTimestamp_ =
  StackResourceDrift'
    { _srdActualProperties = Nothing
    , _srdPhysicalResourceId = Nothing
    , _srdPhysicalResourceIdContext = Nothing
    , _srdPropertyDifferences = Nothing
    , _srdExpectedProperties = Nothing
    , _srdStackId = pStackId_
    , _srdLogicalResourceId = pLogicalResourceId_
    , _srdResourceType = pResourceType_
    , _srdStackResourceDriftStatus = pStackResourceDriftStatus_
    , _srdTimestamp = _Time # pTimestamp_
    }


-- | A JSON structure containing the actual property values of the stack resource. For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
srdActualProperties :: Lens' StackResourceDrift (Maybe Text)
srdActualProperties = lens _srdActualProperties (\ s a -> s{_srdActualProperties = a})

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDrift (Maybe Text)
srdPhysicalResourceId = lens _srdPhysicalResourceId (\ s a -> s{_srdPhysicalResourceId = a})

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
srdPhysicalResourceIdContext :: Lens' StackResourceDrift [PhysicalResourceIdContextKeyValuePair]
srdPhysicalResourceIdContext = lens _srdPhysicalResourceIdContext (\ s a -> s{_srdPhysicalResourceIdContext = a}) . _Default . _Coerce

-- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
srdPropertyDifferences :: Lens' StackResourceDrift [PropertyDifference]
srdPropertyDifferences = lens _srdPropertyDifferences (\ s a -> s{_srdPropertyDifferences = a}) . _Default . _Coerce

-- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.  For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
srdExpectedProperties :: Lens' StackResourceDrift (Maybe Text)
srdExpectedProperties = lens _srdExpectedProperties (\ s a -> s{_srdExpectedProperties = a})

-- | The ID of the stack.
srdStackId :: Lens' StackResourceDrift Text
srdStackId = lens _srdStackId (\ s a -> s{_srdStackId = a})

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDrift Text
srdLogicalResourceId = lens _srdLogicalResourceId (\ s a -> s{_srdLogicalResourceId = a})

-- | The type of the resource.
srdResourceType :: Lens' StackResourceDrift Text
srdResourceType = lens _srdResourceType (\ s a -> s{_srdResourceType = a})

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
srdStackResourceDriftStatus :: Lens' StackResourceDrift StackResourceDriftStatus
srdStackResourceDriftStatus = lens _srdStackResourceDriftStatus (\ s a -> s{_srdStackResourceDriftStatus = a})

-- | Time at which AWS CloudFormation performed drift detection on the stack resource.
srdTimestamp :: Lens' StackResourceDrift UTCTime
srdTimestamp = lens _srdTimestamp (\ s a -> s{_srdTimestamp = a}) . _Time

instance FromXML StackResourceDrift where
        parseXML x
          = StackResourceDrift' <$>
              (x .@? "ActualProperties") <*>
                (x .@? "PhysicalResourceId")
                <*>
                (x .@? "PhysicalResourceIdContext" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "PropertyDifferences" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ExpectedProperties")
                <*> (x .@ "StackId")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "StackResourceDriftStatus")
                <*> (x .@ "Timestamp")

instance Hashable StackResourceDrift where

instance NFData StackResourceDrift where

-- | Contains information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
--
--
-- /See:/ 'stackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation =
  StackResourceDriftInformation'
    { _srdiLastCheckTimestamp       :: !(Maybe ISO8601)
    , _srdiStackResourceDriftStatus :: !StackResourceDriftStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResourceDriftInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdiLastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- * 'srdiStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .      * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
stackResourceDriftInformation
    :: StackResourceDriftStatus -- ^ 'srdiStackResourceDriftStatus'
    -> StackResourceDriftInformation
stackResourceDriftInformation pStackResourceDriftStatus_ =
  StackResourceDriftInformation'
    { _srdiLastCheckTimestamp = Nothing
    , _srdiStackResourceDriftStatus = pStackResourceDriftStatus_
    }


-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
srdiLastCheckTimestamp :: Lens' StackResourceDriftInformation (Maybe UTCTime)
srdiLastCheckTimestamp = lens _srdiLastCheckTimestamp (\ s a -> s{_srdiLastCheckTimestamp = a}) . mapping _Time

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .      * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
srdiStackResourceDriftStatus :: Lens' StackResourceDriftInformation StackResourceDriftStatus
srdiStackResourceDriftStatus = lens _srdiStackResourceDriftStatus (\ s a -> s{_srdiStackResourceDriftStatus = a})

instance FromXML StackResourceDriftInformation where
        parseXML x
          = StackResourceDriftInformation' <$>
              (x .@? "LastCheckTimestamp") <*>
                (x .@ "StackResourceDriftStatus")

instance Hashable StackResourceDriftInformation where

instance NFData StackResourceDriftInformation where

-- | Summarizes information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
--
--
-- /See:/ 'stackResourceDriftInformationSummary' smart constructor.
data StackResourceDriftInformationSummary =
  StackResourceDriftInformationSummary'
    { _srdisLastCheckTimestamp       :: !(Maybe ISO8601)
    , _srdisStackResourceDriftStatus :: !StackResourceDriftStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResourceDriftInformationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdisLastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- * 'srdisStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
stackResourceDriftInformationSummary
    :: StackResourceDriftStatus -- ^ 'srdisStackResourceDriftStatus'
    -> StackResourceDriftInformationSummary
stackResourceDriftInformationSummary pStackResourceDriftStatus_ =
  StackResourceDriftInformationSummary'
    { _srdisLastCheckTimestamp = Nothing
    , _srdisStackResourceDriftStatus = pStackResourceDriftStatus_
    }


-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
srdisLastCheckTimestamp :: Lens' StackResourceDriftInformationSummary (Maybe UTCTime)
srdisLastCheckTimestamp = lens _srdisLastCheckTimestamp (\ s a -> s{_srdisLastCheckTimestamp = a}) . mapping _Time

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
srdisStackResourceDriftStatus :: Lens' StackResourceDriftInformationSummary StackResourceDriftStatus
srdisStackResourceDriftStatus = lens _srdisStackResourceDriftStatus (\ s a -> s{_srdisStackResourceDriftStatus = a})

instance FromXML StackResourceDriftInformationSummary
         where
        parseXML x
          = StackResourceDriftInformationSummary' <$>
              (x .@? "LastCheckTimestamp") <*>
                (x .@ "StackResourceDriftStatus")

instance Hashable
           StackResourceDriftInformationSummary
         where

instance NFData StackResourceDriftInformationSummary
         where

-- | Contains high-level information about the specified stack resource.
--
--
--
-- /See:/ 'stackResourceSummary' smart constructor.
data StackResourceSummary =
  StackResourceSummary'
    { _srsPhysicalResourceId   :: !(Maybe Text)
    , _srsResourceStatusReason :: !(Maybe Text)
    , _srsDriftInformation     :: !(Maybe StackResourceDriftInformationSummary)
    , _srsLogicalResourceId    :: !Text
    , _srsResourceType         :: !Text
    , _srsLastUpdatedTimestamp :: !ISO8601
    , _srsResourceStatus       :: !ResourceStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackResourceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of the resource.
--
-- * 'srsResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'srsDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'srsLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srsResourceType' - Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'srsLastUpdatedTimestamp' - Time the status was updated.
--
-- * 'srsResourceStatus' - Current status of the resource.
stackResourceSummary
    :: Text -- ^ 'srsLogicalResourceId'
    -> Text -- ^ 'srsResourceType'
    -> UTCTime -- ^ 'srsLastUpdatedTimestamp'
    -> ResourceStatus -- ^ 'srsResourceStatus'
    -> StackResourceSummary
stackResourceSummary pLogicalResourceId_ pResourceType_ pLastUpdatedTimestamp_ pResourceStatus_ =
  StackResourceSummary'
    { _srsPhysicalResourceId = Nothing
    , _srsResourceStatusReason = Nothing
    , _srsDriftInformation = Nothing
    , _srsLogicalResourceId = pLogicalResourceId_
    , _srsResourceType = pResourceType_
    , _srsLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_
    , _srsResourceStatus = pResourceStatus_
    }


-- | The name or unique identifier that corresponds to a physical instance ID of the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId = lens _srsPhysicalResourceId (\ s a -> s{_srsPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason = lens _srsResourceStatusReason (\ s a -> s{_srsResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
srsDriftInformation :: Lens' StackResourceSummary (Maybe StackResourceDriftInformationSummary)
srsDriftInformation = lens _srsDriftInformation (\ s a -> s{_srsDriftInformation = a})

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId = lens _srsLogicalResourceId (\ s a -> s{_srsLogicalResourceId = a})

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\ s a -> s{_srsResourceType = a})

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary UTCTime
srsLastUpdatedTimestamp = lens _srsLastUpdatedTimestamp (\ s a -> s{_srsLastUpdatedTimestamp = a}) . _Time

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus = lens _srsResourceStatus (\ s a -> s{_srsResourceStatus = a})

instance FromXML StackResourceSummary where
        parseXML x
          = StackResourceSummary' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "DriftInformation")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResourceSummary where

instance NFData StackResourceSummary where

-- | A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires.
--
--
--
-- /See:/ 'stackSet' smart constructor.
data StackSet =
  StackSet'
    { _ssStackSetDriftDetectionDetails :: !(Maybe StackSetDriftDetectionDetails)
    , _ssStatus                        :: !(Maybe StackSetStatus)
    , _ssAdministrationRoleARN         :: !(Maybe Text)
    , _ssAutoDeployment                :: !(Maybe AutoDeployment)
    , _ssOrganizationalUnitIds         :: !(Maybe [Text])
    , _ssStackSetARN                   :: !(Maybe Text)
    , _ssPermissionModel               :: !(Maybe PermissionModels)
    , _ssParameters                    :: !(Maybe [Parameter])
    , _ssTemplateBody                  :: !(Maybe Text)
    , _ssStackSetName                  :: !(Maybe Text)
    , _ssDescription                   :: !(Maybe Text)
    , _ssCapabilities                  :: !(Maybe [Capability])
    , _ssTags                          :: !(Maybe [Tag])
    , _ssStackSetId                    :: !(Maybe Text)
    , _ssExecutionRoleName             :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
--
-- * 'ssStatus' - The status of the stack set.
--
-- * 'ssAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set. Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssAutoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- * 'ssOrganizationalUnitIds' - [@Service-managed@ permissions] The organization root ID or organizational unit (OUs) IDs to which stacks in your stack set have been deployed.
--
-- * 'ssStackSetARN' - The Amazon Resource Number (ARN) of the stack set.
--
-- * 'ssPermissionModel' - Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
-- * 'ssParameters' - A list of input parameters for a stack set.
--
-- * 'ssTemplateBody' - The structure that contains the body of the template that was used to create or update the stack set.
--
-- * 'ssStackSetName' - The name that's associated with the stack set.
--
-- * 'ssDescription' - A description of the stack set that you specify when the stack set is created or updated.
--
-- * 'ssCapabilities' - The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- * 'ssTags' - A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
--
-- * 'ssStackSetId' - The ID of the stack set.
--
-- * 'ssExecutionRoleName' - The name of the IAM execution role used to create or update the stack set.  Use customized execution roles to control which stack resources users and groups can include in their stack sets.
stackSet
    :: StackSet
stackSet =
  StackSet'
    { _ssStackSetDriftDetectionDetails = Nothing
    , _ssStatus = Nothing
    , _ssAdministrationRoleARN = Nothing
    , _ssAutoDeployment = Nothing
    , _ssOrganizationalUnitIds = Nothing
    , _ssStackSetARN = Nothing
    , _ssPermissionModel = Nothing
    , _ssParameters = Nothing
    , _ssTemplateBody = Nothing
    , _ssStackSetName = Nothing
    , _ssDescription = Nothing
    , _ssCapabilities = Nothing
    , _ssTags = Nothing
    , _ssStackSetId = Nothing
    , _ssExecutionRoleName = Nothing
    }


-- | Detailed information about the drift status of the stack set. For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
ssStackSetDriftDetectionDetails :: Lens' StackSet (Maybe StackSetDriftDetectionDetails)
ssStackSetDriftDetectionDetails = lens _ssStackSetDriftDetectionDetails (\ s a -> s{_ssStackSetDriftDetectionDetails = a})

-- | The status of the stack set.
ssStatus :: Lens' StackSet (Maybe StackSetStatus)
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a})

-- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set. Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
ssAdministrationRoleARN :: Lens' StackSet (Maybe Text)
ssAdministrationRoleARN = lens _ssAdministrationRoleARN (\ s a -> s{_ssAdministrationRoleARN = a})

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
ssAutoDeployment :: Lens' StackSet (Maybe AutoDeployment)
ssAutoDeployment = lens _ssAutoDeployment (\ s a -> s{_ssAutoDeployment = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OUs) IDs to which stacks in your stack set have been deployed.
ssOrganizationalUnitIds :: Lens' StackSet [Text]
ssOrganizationalUnitIds = lens _ssOrganizationalUnitIds (\ s a -> s{_ssOrganizationalUnitIds = a}) . _Default . _Coerce

-- | The Amazon Resource Number (ARN) of the stack set.
ssStackSetARN :: Lens' StackSet (Maybe Text)
ssStackSetARN = lens _ssStackSetARN (\ s a -> s{_ssStackSetARN = a})

-- | Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
ssPermissionModel :: Lens' StackSet (Maybe PermissionModels)
ssPermissionModel = lens _ssPermissionModel (\ s a -> s{_ssPermissionModel = a})

-- | A list of input parameters for a stack set.
ssParameters :: Lens' StackSet [Parameter]
ssParameters = lens _ssParameters (\ s a -> s{_ssParameters = a}) . _Default . _Coerce

-- | The structure that contains the body of the template that was used to create or update the stack set.
ssTemplateBody :: Lens' StackSet (Maybe Text)
ssTemplateBody = lens _ssTemplateBody (\ s a -> s{_ssTemplateBody = a})

-- | The name that's associated with the stack set.
ssStackSetName :: Lens' StackSet (Maybe Text)
ssStackSetName = lens _ssStackSetName (\ s a -> s{_ssStackSetName = a})

-- | A description of the stack set that you specify when the stack set is created or updated.
ssDescription :: Lens' StackSet (Maybe Text)
ssDescription = lens _ssDescription (\ s a -> s{_ssDescription = a})

-- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
ssCapabilities :: Lens' StackSet [Capability]
ssCapabilities = lens _ssCapabilities (\ s a -> s{_ssCapabilities = a}) . _Default . _Coerce

-- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
ssTags :: Lens' StackSet [Tag]
ssTags = lens _ssTags (\ s a -> s{_ssTags = a}) . _Default . _Coerce

-- | The ID of the stack set.
ssStackSetId :: Lens' StackSet (Maybe Text)
ssStackSetId = lens _ssStackSetId (\ s a -> s{_ssStackSetId = a})

-- | The name of the IAM execution role used to create or update the stack set.  Use customized execution roles to control which stack resources users and groups can include in their stack sets.
ssExecutionRoleName :: Lens' StackSet (Maybe Text)
ssExecutionRoleName = lens _ssExecutionRoleName (\ s a -> s{_ssExecutionRoleName = a})

instance FromXML StackSet where
        parseXML x
          = StackSet' <$>
              (x .@? "StackSetDriftDetectionDetails") <*>
                (x .@? "Status")
                <*> (x .@? "AdministrationRoleARN")
                <*> (x .@? "AutoDeployment")
                <*>
                (x .@? "OrganizationalUnitIds" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackSetARN")
                <*> (x .@? "PermissionModel")
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TemplateBody")
                <*> (x .@? "StackSetName")
                <*> (x .@? "Description")
                <*>
                (x .@? "Capabilities" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackSetId")
                <*> (x .@? "ExecutionRoleName")

instance Hashable StackSet where

instance NFData StackSet where

-- | Detailed information about the drift status of the stack set.
--
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations in-progress is not included.
--
-- For stack set operations, includes information about drift operations currently being performed on the stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the /AWS CloudFormation User Guide/ .
--
--
-- /See:/ 'stackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { _ssdddLastDriftCheckTimestamp :: !(Maybe ISO8601)
    , _ssdddTotalStackInstancesCount :: !(Maybe Nat)
    , _ssdddInProgressStackInstancesCount :: !(Maybe Nat)
    , _ssdddDriftedStackInstancesCount :: !(Maybe Nat)
    , _ssdddDriftDetectionStatus :: !(Maybe StackSetDriftDetectionStatus)
    , _ssdddDriftStatus :: !(Maybe StackSetDriftStatus)
    , _ssdddFailedStackInstancesCount :: !(Maybe Nat)
    , _ssdddInSyncStackInstancesCount :: !(Maybe Nat)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetDriftDetectionDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdddLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- * 'ssdddTotalStackInstancesCount' - The total number of stack instances belonging to this stack set.  The total number of stack instances is equal to the total of:     * Stack instances that match the stack set configuration.      * Stack instances that have drifted from the stack set configuration.      * Stack instances where the drift detection operation has failed.     * Stack instances currently being checked for drift.
--
-- * 'ssdddInProgressStackInstancesCount' - The number of stack instances that are currently being checked for drift.
--
-- * 'ssdddDriftedStackInstancesCount' - The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
--
-- * 'ssdddDriftDetectionStatus' - The status of the stack set drift detection operation.     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.      * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.     * @IN_PROGRESS@ : The drift detection operation is currently being performed.     * @STOPPED@ : The user has cancelled the drift detection operation.
--
-- * 'ssdddDriftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
--
-- * 'ssdddFailedStackInstancesCount' - The number of stack instances for which the drift detection operation failed.
--
-- * 'ssdddInSyncStackInstancesCount' - The number of stack instances which match the expected template and parameter configuration of the stack set.
stackSetDriftDetectionDetails
    :: StackSetDriftDetectionDetails
stackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { _ssdddLastDriftCheckTimestamp = Nothing
    , _ssdddTotalStackInstancesCount = Nothing
    , _ssdddInProgressStackInstancesCount = Nothing
    , _ssdddDriftedStackInstancesCount = Nothing
    , _ssdddDriftDetectionStatus = Nothing
    , _ssdddDriftStatus = Nothing
    , _ssdddFailedStackInstancesCount = Nothing
    , _ssdddInSyncStackInstancesCount = Nothing
    }


-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
ssdddLastDriftCheckTimestamp :: Lens' StackSetDriftDetectionDetails (Maybe UTCTime)
ssdddLastDriftCheckTimestamp = lens _ssdddLastDriftCheckTimestamp (\ s a -> s{_ssdddLastDriftCheckTimestamp = a}) . mapping _Time

-- | The total number of stack instances belonging to this stack set.  The total number of stack instances is equal to the total of:     * Stack instances that match the stack set configuration.      * Stack instances that have drifted from the stack set configuration.      * Stack instances where the drift detection operation has failed.     * Stack instances currently being checked for drift.
ssdddTotalStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddTotalStackInstancesCount = lens _ssdddTotalStackInstancesCount (\ s a -> s{_ssdddTotalStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances that are currently being checked for drift.
ssdddInProgressStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddInProgressStackInstancesCount = lens _ssdddInProgressStackInstancesCount (\ s a -> s{_ssdddInProgressStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
ssdddDriftedStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddDriftedStackInstancesCount = lens _ssdddDriftedStackInstancesCount (\ s a -> s{_ssdddDriftedStackInstancesCount = a}) . mapping _Nat

-- | The status of the stack set drift detection operation.     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.      * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.     * @IN_PROGRESS@ : The drift detection operation is currently being performed.     * @STOPPED@ : The user has cancelled the drift detection operation.
ssdddDriftDetectionStatus :: Lens' StackSetDriftDetectionDetails (Maybe StackSetDriftDetectionStatus)
ssdddDriftDetectionStatus = lens _ssdddDriftDetectionStatus (\ s a -> s{_ssdddDriftDetectionStatus = a})

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
ssdddDriftStatus :: Lens' StackSetDriftDetectionDetails (Maybe StackSetDriftStatus)
ssdddDriftStatus = lens _ssdddDriftStatus (\ s a -> s{_ssdddDriftStatus = a})

-- | The number of stack instances for which the drift detection operation failed.
ssdddFailedStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddFailedStackInstancesCount = lens _ssdddFailedStackInstancesCount (\ s a -> s{_ssdddFailedStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances which match the expected template and parameter configuration of the stack set.
ssdddInSyncStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddInSyncStackInstancesCount = lens _ssdddInSyncStackInstancesCount (\ s a -> s{_ssdddInSyncStackInstancesCount = a}) . mapping _Nat

instance FromXML StackSetDriftDetectionDetails where
        parseXML x
          = StackSetDriftDetectionDetails' <$>
              (x .@? "LastDriftCheckTimestamp") <*>
                (x .@? "TotalStackInstancesCount")
                <*> (x .@? "InProgressStackInstancesCount")
                <*> (x .@? "DriftedStackInstancesCount")
                <*> (x .@? "DriftDetectionStatus")
                <*> (x .@? "DriftStatus")
                <*> (x .@? "FailedStackInstancesCount")
                <*> (x .@? "InSyncStackInstancesCount")

instance Hashable StackSetDriftDetectionDetails where

instance NFData StackSetDriftDetectionDetails where

-- | The structure that contains information about a stack set operation.
--
--
--
-- /See:/ 'stackSetOperation' smart constructor.
data StackSetOperation =
  StackSetOperation'
    { _ssoStackSetDriftDetectionDetails :: !(Maybe StackSetDriftDetectionDetails)
    , _ssoStatus :: !(Maybe StackSetOperationStatus)
    , _ssoAdministrationRoleARN :: !(Maybe Text)
    , _ssoAction :: !(Maybe StackSetOperationAction)
    , _ssoEndTimestamp :: !(Maybe ISO8601)
    , _ssoCreationTimestamp :: !(Maybe ISO8601)
    , _ssoOperationPreferences :: !(Maybe StackSetOperationPreferences)
    , _ssoOperationId :: !(Maybe Text)
    , _ssoRetainStacks :: !(Maybe Bool)
    , _ssoDeploymentTargets :: !(Maybe DeploymentTargets)
    , _ssoStackSetId :: !(Maybe Text)
    , _ssoExecutionRoleName :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssoStackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set. this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
--
-- * 'ssoStatus' - The status of the operation.      * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each region during stack create and update operations. If the number of failed stacks within a region exceeds the failure tolerance, the status of the operation in the region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining regions.     * @QUEUED@ : [Service-managed permissions] For automatic deployments that require a sequence of operations. The operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
-- * 'ssoAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.  Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssoAction' - The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
--
-- * 'ssoEndTimestamp' - The time at which the stack set operation ended, across all accounts and regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or region.
--
-- * 'ssoCreationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested regions, before actually creating the first stacks.
--
-- * 'ssoOperationPreferences' - The preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'ssoOperationId' - The unique ID of a stack set operation.
--
-- * 'ssoRetainStacks' - For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
--
-- * 'ssoDeploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
--
-- * 'ssoStackSetId' - The ID of the stack set.
--
-- * 'ssoExecutionRoleName' - The name of the IAM execution role used to create or update the stack set. Use customized execution roles to control which stack resources users and groups can include in their stack sets.
stackSetOperation
    :: StackSetOperation
stackSetOperation =
  StackSetOperation'
    { _ssoStackSetDriftDetectionDetails = Nothing
    , _ssoStatus = Nothing
    , _ssoAdministrationRoleARN = Nothing
    , _ssoAction = Nothing
    , _ssoEndTimestamp = Nothing
    , _ssoCreationTimestamp = Nothing
    , _ssoOperationPreferences = Nothing
    , _ssoOperationId = Nothing
    , _ssoRetainStacks = Nothing
    , _ssoDeploymentTargets = Nothing
    , _ssoStackSetId = Nothing
    , _ssoExecutionRoleName = Nothing
    }


-- | Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set. this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
ssoStackSetDriftDetectionDetails :: Lens' StackSetOperation (Maybe StackSetDriftDetectionDetails)
ssoStackSetDriftDetectionDetails = lens _ssoStackSetDriftDetectionDetails (\ s a -> s{_ssoStackSetDriftDetectionDetails = a})

-- | The status of the operation.      * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each region during stack create and update operations. If the number of failed stacks within a region exceeds the failure tolerance, the status of the operation in the region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining regions.     * @QUEUED@ : [Service-managed permissions] For automatic deployments that require a sequence of operations. The operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
ssoStatus :: Lens' StackSetOperation (Maybe StackSetOperationStatus)
ssoStatus = lens _ssoStatus (\ s a -> s{_ssoStatus = a})

-- | The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.  Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
ssoAdministrationRoleARN :: Lens' StackSetOperation (Maybe Text)
ssoAdministrationRoleARN = lens _ssoAdministrationRoleARN (\ s a -> s{_ssoAdministrationRoleARN = a})

-- | The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
ssoAction :: Lens' StackSetOperation (Maybe StackSetOperationAction)
ssoAction = lens _ssoAction (\ s a -> s{_ssoAction = a})

-- | The time at which the stack set operation ended, across all accounts and regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or region.
ssoEndTimestamp :: Lens' StackSetOperation (Maybe UTCTime)
ssoEndTimestamp = lens _ssoEndTimestamp (\ s a -> s{_ssoEndTimestamp = a}) . mapping _Time

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested regions, before actually creating the first stacks.
ssoCreationTimestamp :: Lens' StackSetOperation (Maybe UTCTime)
ssoCreationTimestamp = lens _ssoCreationTimestamp (\ s a -> s{_ssoCreationTimestamp = a}) . mapping _Time

-- | The preferences for how AWS CloudFormation performs this stack set operation.
ssoOperationPreferences :: Lens' StackSetOperation (Maybe StackSetOperationPreferences)
ssoOperationPreferences = lens _ssoOperationPreferences (\ s a -> s{_ssoOperationPreferences = a})

-- | The unique ID of a stack set operation.
ssoOperationId :: Lens' StackSetOperation (Maybe Text)
ssoOperationId = lens _ssoOperationId (\ s a -> s{_ssoOperationId = a})

-- | For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
ssoRetainStacks :: Lens' StackSetOperation (Maybe Bool)
ssoRetainStacks = lens _ssoRetainStacks (\ s a -> s{_ssoRetainStacks = a})

-- | [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
ssoDeploymentTargets :: Lens' StackSetOperation (Maybe DeploymentTargets)
ssoDeploymentTargets = lens _ssoDeploymentTargets (\ s a -> s{_ssoDeploymentTargets = a})

-- | The ID of the stack set.
ssoStackSetId :: Lens' StackSetOperation (Maybe Text)
ssoStackSetId = lens _ssoStackSetId (\ s a -> s{_ssoStackSetId = a})

-- | The name of the IAM execution role used to create or update the stack set. Use customized execution roles to control which stack resources users and groups can include in their stack sets.
ssoExecutionRoleName :: Lens' StackSetOperation (Maybe Text)
ssoExecutionRoleName = lens _ssoExecutionRoleName (\ s a -> s{_ssoExecutionRoleName = a})

instance FromXML StackSetOperation where
        parseXML x
          = StackSetOperation' <$>
              (x .@? "StackSetDriftDetectionDetails") <*>
                (x .@? "Status")
                <*> (x .@? "AdministrationRoleARN")
                <*> (x .@? "Action")
                <*> (x .@? "EndTimestamp")
                <*> (x .@? "CreationTimestamp")
                <*> (x .@? "OperationPreferences")
                <*> (x .@? "OperationId")
                <*> (x .@? "RetainStacks")
                <*> (x .@? "DeploymentTargets")
                <*> (x .@? "StackSetId")
                <*> (x .@? "ExecutionRoleName")

instance Hashable StackSetOperation where

instance NFData StackSetOperation where

-- | The user-specified preferences for how AWS CloudFormation performs a stack set operation.
--
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
--
-- /See:/ 'stackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences =
  StackSetOperationPreferences'
    { _ssopRegionOrder                :: !(Maybe [Text])
    , _ssopMaxConcurrentCount         :: !(Maybe Nat)
    , _ssopMaxConcurrentPercentage    :: !(Maybe Nat)
    , _ssopFailureToleranceCount      :: !(Maybe Nat)
    , _ssopFailureTolerancePercentage :: !(Maybe Nat)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetOperationPreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssopRegionOrder' - The order of the regions in where you want to perform the stack operation.
--
-- * 'ssopMaxConcurrentCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ —@MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ . Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- * 'ssopMaxConcurrentPercentage' - The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead. Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- * 'ssopFailureToleranceCount' - The number of accounts, per region, for which this operation can fail before AWS CloudFormation stops the operation in that region. If the operation is stopped in a region, AWS CloudFormation doesn't attempt the operation in any subsequent regions. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
--
-- * 'ssopFailureTolerancePercentage' - The percentage of accounts, per region, for which this stack operation can fail before AWS CloudFormation stops the operation in that region. If the operation is stopped in a region, AWS CloudFormation doesn't attempt the operation in any subsequent regions. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
stackSetOperationPreferences
    :: StackSetOperationPreferences
stackSetOperationPreferences =
  StackSetOperationPreferences'
    { _ssopRegionOrder = Nothing
    , _ssopMaxConcurrentCount = Nothing
    , _ssopMaxConcurrentPercentage = Nothing
    , _ssopFailureToleranceCount = Nothing
    , _ssopFailureTolerancePercentage = Nothing
    }


-- | The order of the regions in where you want to perform the stack operation.
ssopRegionOrder :: Lens' StackSetOperationPreferences [Text]
ssopRegionOrder = lens _ssopRegionOrder (\ s a -> s{_ssopRegionOrder = a}) . _Default . _Coerce

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ —@MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ . Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
ssopMaxConcurrentCount :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopMaxConcurrentCount = lens _ssopMaxConcurrentCount (\ s a -> s{_ssopMaxConcurrentCount = a}) . mapping _Nat

-- | The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead. Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
ssopMaxConcurrentPercentage :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopMaxConcurrentPercentage = lens _ssopMaxConcurrentPercentage (\ s a -> s{_ssopMaxConcurrentPercentage = a}) . mapping _Nat

-- | The number of accounts, per region, for which this operation can fail before AWS CloudFormation stops the operation in that region. If the operation is stopped in a region, AWS CloudFormation doesn't attempt the operation in any subsequent regions. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
ssopFailureToleranceCount :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopFailureToleranceCount = lens _ssopFailureToleranceCount (\ s a -> s{_ssopFailureToleranceCount = a}) . mapping _Nat

-- | The percentage of accounts, per region, for which this stack operation can fail before AWS CloudFormation stops the operation in that region. If the operation is stopped in a region, AWS CloudFormation doesn't attempt the operation in any subsequent regions. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
ssopFailureTolerancePercentage :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopFailureTolerancePercentage = lens _ssopFailureTolerancePercentage (\ s a -> s{_ssopFailureTolerancePercentage = a}) . mapping _Nat

instance FromXML StackSetOperationPreferences where
        parseXML x
          = StackSetOperationPreferences' <$>
              (x .@? "RegionOrder" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "MaxConcurrentCount")
                <*> (x .@? "MaxConcurrentPercentage")
                <*> (x .@? "FailureToleranceCount")
                <*> (x .@? "FailureTolerancePercentage")

instance Hashable StackSetOperationPreferences where

instance NFData StackSetOperationPreferences where

instance ToQuery StackSetOperationPreferences where
        toQuery StackSetOperationPreferences'{..}
          = mconcat
              ["RegionOrder" =:
                 toQuery (toQueryList "member" <$> _ssopRegionOrder),
               "MaxConcurrentCount" =: _ssopMaxConcurrentCount,
               "MaxConcurrentPercentage" =:
                 _ssopMaxConcurrentPercentage,
               "FailureToleranceCount" =:
                 _ssopFailureToleranceCount,
               "FailureTolerancePercentage" =:
                 _ssopFailureTolerancePercentage]

-- | The structure that contains information about a specified operation's results for a given account in a given region.
--
--
--
-- /See:/ 'stackSetOperationResultSummary' smart constructor.
data StackSetOperationResultSummary =
  StackSetOperationResultSummary'
    { _ssorsStatus               :: !(Maybe StackSetOperationResultStatus)
    , _ssorsAccount              :: !(Maybe Text)
    , _ssorsAccountGateResult    :: !(Maybe AccountGateResult)
    , _ssorsOrganizationalUnitId :: !(Maybe Text)
    , _ssorsRegion               :: !(Maybe Text)
    , _ssorsStatusReason         :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetOperationResultSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssorsStatus' - The result status of the stack set operation for the given account in the given region.     * @CANCELLED@ : The operation in the specified account and region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and region failed.  If the stack set operation fails in enough accounts within a region, the failure tolerance for the stack set operation as a whole might be exceeded.      * @RUNNING@ : The operation in the specified account and region is currently in progress.     * @PENDING@ : The operation in the specified account and region has yet to start.      * @SUCCEEDED@ : The operation in the specified account and region completed successfully.
--
-- * 'ssorsAccount' - [Self-managed permissions] The name of the AWS account for this operation result.
--
-- * 'ssorsAccountGateResult' - The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
--
-- * 'ssorsOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID for this operation result.
--
-- * 'ssorsRegion' - The name of the AWS region for this operation result.
--
-- * 'ssorsStatusReason' - The reason for the assigned result status.
stackSetOperationResultSummary
    :: StackSetOperationResultSummary
stackSetOperationResultSummary =
  StackSetOperationResultSummary'
    { _ssorsStatus = Nothing
    , _ssorsAccount = Nothing
    , _ssorsAccountGateResult = Nothing
    , _ssorsOrganizationalUnitId = Nothing
    , _ssorsRegion = Nothing
    , _ssorsStatusReason = Nothing
    }


-- | The result status of the stack set operation for the given account in the given region.     * @CANCELLED@ : The operation in the specified account and region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and region failed.  If the stack set operation fails in enough accounts within a region, the failure tolerance for the stack set operation as a whole might be exceeded.      * @RUNNING@ : The operation in the specified account and region is currently in progress.     * @PENDING@ : The operation in the specified account and region has yet to start.      * @SUCCEEDED@ : The operation in the specified account and region completed successfully.
ssorsStatus :: Lens' StackSetOperationResultSummary (Maybe StackSetOperationResultStatus)
ssorsStatus = lens _ssorsStatus (\ s a -> s{_ssorsStatus = a})

-- | [Self-managed permissions] The name of the AWS account for this operation result.
ssorsAccount :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsAccount = lens _ssorsAccount (\ s a -> s{_ssorsAccount = a})

-- | The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
ssorsAccountGateResult :: Lens' StackSetOperationResultSummary (Maybe AccountGateResult)
ssorsAccountGateResult = lens _ssorsAccountGateResult (\ s a -> s{_ssorsAccountGateResult = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) ID for this operation result.
ssorsOrganizationalUnitId :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsOrganizationalUnitId = lens _ssorsOrganizationalUnitId (\ s a -> s{_ssorsOrganizationalUnitId = a})

-- | The name of the AWS region for this operation result.
ssorsRegion :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsRegion = lens _ssorsRegion (\ s a -> s{_ssorsRegion = a})

-- | The reason for the assigned result status.
ssorsStatusReason :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsStatusReason = lens _ssorsStatusReason (\ s a -> s{_ssorsStatusReason = a})

instance FromXML StackSetOperationResultSummary where
        parseXML x
          = StackSetOperationResultSummary' <$>
              (x .@? "Status") <*> (x .@? "Account") <*>
                (x .@? "AccountGateResult")
                <*> (x .@? "OrganizationalUnitId")
                <*> (x .@? "Region")
                <*> (x .@? "StatusReason")

instance Hashable StackSetOperationResultSummary
         where

instance NFData StackSetOperationResultSummary where

-- | The structures that contain summary information about the specified operation.
--
--
--
-- /See:/ 'stackSetOperationSummary' smart constructor.
data StackSetOperationSummary =
  StackSetOperationSummary'
    { _ssosStatus            :: !(Maybe StackSetOperationStatus)
    , _ssosAction            :: !(Maybe StackSetOperationAction)
    , _ssosEndTimestamp      :: !(Maybe ISO8601)
    , _ssosCreationTimestamp :: !(Maybe ISO8601)
    , _ssosOperationId       :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetOperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssosStatus' - The overall status of the operation.     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each region during stack create and update operations. If the number of failed stacks within a region exceeds the failure tolerance, the status of the operation in the region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining regions.     * @QUEUED@ : [Service-managed permissions] For automatic deployments that require a sequence of operations. The operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
-- * 'ssosAction' - The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
--
-- * 'ssosEndTimestamp' - The time at which the stack set operation ended, across all accounts and regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or region.
--
-- * 'ssosCreationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested regions, before actually creating the first stacks.
--
-- * 'ssosOperationId' - The unique ID of the stack set operation.
stackSetOperationSummary
    :: StackSetOperationSummary
stackSetOperationSummary =
  StackSetOperationSummary'
    { _ssosStatus = Nothing
    , _ssosAction = Nothing
    , _ssosEndTimestamp = Nothing
    , _ssosCreationTimestamp = Nothing
    , _ssosOperationId = Nothing
    }


-- | The overall status of the operation.     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each region during stack create and update operations. If the number of failed stacks within a region exceeds the failure tolerance, the status of the operation in the region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining regions.     * @QUEUED@ : [Service-managed permissions] For automatic deployments that require a sequence of operations. The operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
ssosStatus :: Lens' StackSetOperationSummary (Maybe StackSetOperationStatus)
ssosStatus = lens _ssosStatus (\ s a -> s{_ssosStatus = a})

-- | The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
ssosAction :: Lens' StackSetOperationSummary (Maybe StackSetOperationAction)
ssosAction = lens _ssosAction (\ s a -> s{_ssosAction = a})

-- | The time at which the stack set operation ended, across all accounts and regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or region.
ssosEndTimestamp :: Lens' StackSetOperationSummary (Maybe UTCTime)
ssosEndTimestamp = lens _ssosEndTimestamp (\ s a -> s{_ssosEndTimestamp = a}) . mapping _Time

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested regions, before actually creating the first stacks.
ssosCreationTimestamp :: Lens' StackSetOperationSummary (Maybe UTCTime)
ssosCreationTimestamp = lens _ssosCreationTimestamp (\ s a -> s{_ssosCreationTimestamp = a}) . mapping _Time

-- | The unique ID of the stack set operation.
ssosOperationId :: Lens' StackSetOperationSummary (Maybe Text)
ssosOperationId = lens _ssosOperationId (\ s a -> s{_ssosOperationId = a})

instance FromXML StackSetOperationSummary where
        parseXML x
          = StackSetOperationSummary' <$>
              (x .@? "Status") <*> (x .@? "Action") <*>
                (x .@? "EndTimestamp")
                <*> (x .@? "CreationTimestamp")
                <*> (x .@? "OperationId")

instance Hashable StackSetOperationSummary where

instance NFData StackSetOperationSummary where

-- | The structures that contain summary information about the specified stack set.
--
--
--
-- /See:/ 'stackSetSummary' smart constructor.
data StackSetSummary =
  StackSetSummary'
    { _sssStatus                  :: !(Maybe StackSetStatus)
    , _sssLastDriftCheckTimestamp :: !(Maybe ISO8601)
    , _sssAutoDeployment          :: !(Maybe AutoDeployment)
    , _sssDriftStatus             :: !(Maybe StackDriftStatus)
    , _sssPermissionModel         :: !(Maybe PermissionModels)
    , _sssStackSetName            :: !(Maybe Text)
    , _sssDescription             :: !(Maybe Text)
    , _sssStackSetId              :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssStatus' - The status of the stack set.
--
-- * 'sssLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- * 'sssAutoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
--
-- * 'sssDriftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'sssPermissionModel' - Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
-- * 'sssStackSetName' - The name of the stack set.
--
-- * 'sssDescription' - A description of the stack set that you specify when the stack set is created or updated.
--
-- * 'sssStackSetId' - The ID of the stack set.
stackSetSummary
    :: StackSetSummary
stackSetSummary =
  StackSetSummary'
    { _sssStatus = Nothing
    , _sssLastDriftCheckTimestamp = Nothing
    , _sssAutoDeployment = Nothing
    , _sssDriftStatus = Nothing
    , _sssPermissionModel = Nothing
    , _sssStackSetName = Nothing
    , _sssDescription = Nothing
    , _sssStackSetId = Nothing
    }


-- | The status of the stack set.
sssStatus :: Lens' StackSetSummary (Maybe StackSetStatus)
sssStatus = lens _sssStatus (\ s a -> s{_sssStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
sssLastDriftCheckTimestamp :: Lens' StackSetSummary (Maybe UTCTime)
sssLastDriftCheckTimestamp = lens _sssLastDriftCheckTimestamp (\ s a -> s{_sssLastDriftCheckTimestamp = a}) . mapping _Time

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
sssAutoDeployment :: Lens' StackSetSummary (Maybe AutoDeployment)
sssAutoDeployment = lens _sssAutoDeployment (\ s a -> s{_sssAutoDeployment = a})

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.     * @UNKNOWN@ : This value is reserved for future use.
sssDriftStatus :: Lens' StackSetSummary (Maybe StackDriftStatus)
sssDriftStatus = lens _sssDriftStatus (\ s a -> s{_sssDriftStatus = a})

-- | Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
sssPermissionModel :: Lens' StackSetSummary (Maybe PermissionModels)
sssPermissionModel = lens _sssPermissionModel (\ s a -> s{_sssPermissionModel = a})

-- | The name of the stack set.
sssStackSetName :: Lens' StackSetSummary (Maybe Text)
sssStackSetName = lens _sssStackSetName (\ s a -> s{_sssStackSetName = a})

-- | A description of the stack set that you specify when the stack set is created or updated.
sssDescription :: Lens' StackSetSummary (Maybe Text)
sssDescription = lens _sssDescription (\ s a -> s{_sssDescription = a})

-- | The ID of the stack set.
sssStackSetId :: Lens' StackSetSummary (Maybe Text)
sssStackSetId = lens _sssStackSetId (\ s a -> s{_sssStackSetId = a})

instance FromXML StackSetSummary where
        parseXML x
          = StackSetSummary' <$>
              (x .@? "Status") <*>
                (x .@? "LastDriftCheckTimestamp")
                <*> (x .@? "AutoDeployment")
                <*> (x .@? "DriftStatus")
                <*> (x .@? "PermissionModel")
                <*> (x .@? "StackSetName")
                <*> (x .@? "Description")
                <*> (x .@? "StackSetId")

instance Hashable StackSetSummary where

instance NFData StackSetSummary where

-- | The StackSummary Data Type
--
--
--
-- /See:/ 'stackSummary' smart constructor.
data StackSummary =
  StackSummary'
    { _ssLastUpdatedTime     :: !(Maybe ISO8601)
    , _ssRootId              :: !(Maybe Text)
    , _ssStackStatusReason   :: !(Maybe Text)
    , _ssTemplateDescription :: !(Maybe Text)
    , _ssDriftInformation    :: !(Maybe StackDriftInformationSummary)
    , _ssDeletionTime        :: !(Maybe ISO8601)
    , _ssStackId             :: !(Maybe Text)
    , _ssParentId            :: !(Maybe Text)
    , _ssStackName           :: !Text
    , _ssCreationTime        :: !ISO8601
    , _ssStackStatus         :: !StackStatus
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- * 'ssRootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssStackStatusReason' - Success/Failure message associated with the stack status.
--
-- * 'ssTemplateDescription' - The template description of the template used to create the stack.
--
-- * 'ssDriftInformation' - Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'ssDeletionTime' - The time the stack was deleted.
--
-- * 'ssStackId' - Unique stack identifier.
--
-- * 'ssParentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssStackName' - The name associated with the stack.
--
-- * 'ssCreationTime' - The time the stack was created.
--
-- * 'ssStackStatus' - The current status of the stack.
stackSummary
    :: Text -- ^ 'ssStackName'
    -> UTCTime -- ^ 'ssCreationTime'
    -> StackStatus -- ^ 'ssStackStatus'
    -> StackSummary
stackSummary pStackName_ pCreationTime_ pStackStatus_ =
  StackSummary'
    { _ssLastUpdatedTime = Nothing
    , _ssRootId = Nothing
    , _ssStackStatusReason = Nothing
    , _ssTemplateDescription = Nothing
    , _ssDriftInformation = Nothing
    , _ssDeletionTime = Nothing
    , _ssStackId = Nothing
    , _ssParentId = Nothing
    , _ssStackName = pStackName_
    , _ssCreationTime = _Time # pCreationTime_
    , _ssStackStatus = pStackStatus_
    }


-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe UTCTime)
ssLastUpdatedTime = lens _ssLastUpdatedTime (\ s a -> s{_ssLastUpdatedTime = a}) . mapping _Time

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
ssRootId :: Lens' StackSummary (Maybe Text)
ssRootId = lens _ssRootId (\ s a -> s{_ssRootId = a})

-- | Success/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason = lens _ssStackStatusReason (\ s a -> s{_ssStackStatusReason = a})

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription = lens _ssTemplateDescription (\ s a -> s{_ssTemplateDescription = a})

-- | Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
ssDriftInformation :: Lens' StackSummary (Maybe StackDriftInformationSummary)
ssDriftInformation = lens _ssDriftInformation (\ s a -> s{_ssDriftInformation = a})

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe UTCTime)
ssDeletionTime = lens _ssDeletionTime (\ s a -> s{_ssDeletionTime = a}) . mapping _Time

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a})

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
ssParentId :: Lens' StackSummary (Maybe Text)
ssParentId = lens _ssParentId (\ s a -> s{_ssParentId = a})

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\ s a -> s{_ssStackName = a})

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary UTCTime
ssCreationTime = lens _ssCreationTime (\ s a -> s{_ssCreationTime = a}) . _Time

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\ s a -> s{_ssStackStatus = a})

instance FromXML StackSummary where
        parseXML x
          = StackSummary' <$>
              (x .@? "LastUpdatedTime") <*> (x .@? "RootId") <*>
                (x .@? "StackStatusReason")
                <*> (x .@? "TemplateDescription")
                <*> (x .@? "DriftInformation")
                <*> (x .@? "DeletionTime")
                <*> (x .@? "StackId")
                <*> (x .@? "ParentId")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

instance Hashable StackSummary where

instance NFData StackSummary where

-- | The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag =
  Tag'
    { _tagKey   :: !Text
    , _tagValue :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
--
-- * 'tagValue' - /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]

-- | The TemplateParameter data type.
--
--
--
-- /See:/ 'templateParameter' smart constructor.
data TemplateParameter =
  TemplateParameter'
    { _tpParameterKey :: !(Maybe Text)
    , _tpDefaultValue :: !(Maybe Text)
    , _tpNoEcho       :: !(Maybe Bool)
    , _tpDescription  :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TemplateParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpParameterKey' - The name associated with the parameter.
--
-- * 'tpDefaultValue' - The default value associated with the parameter.
--
-- * 'tpNoEcho' - Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
--
-- * 'tpDescription' - User defined description associated with the parameter.
templateParameter
    :: TemplateParameter
templateParameter =
  TemplateParameter'
    { _tpParameterKey = Nothing
    , _tpDefaultValue = Nothing
    , _tpNoEcho = Nothing
    , _tpDescription = Nothing
    }


-- | The name associated with the parameter.
tpParameterKey :: Lens' TemplateParameter (Maybe Text)
tpParameterKey = lens _tpParameterKey (\ s a -> s{_tpParameterKey = a})

-- | The default value associated with the parameter.
tpDefaultValue :: Lens' TemplateParameter (Maybe Text)
tpDefaultValue = lens _tpDefaultValue (\ s a -> s{_tpDefaultValue = a})

-- | Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
tpNoEcho :: Lens' TemplateParameter (Maybe Bool)
tpNoEcho = lens _tpNoEcho (\ s a -> s{_tpNoEcho = a})

-- | User defined description associated with the parameter.
tpDescription :: Lens' TemplateParameter (Maybe Text)
tpDescription = lens _tpDescription (\ s a -> s{_tpDescription = a})

instance FromXML TemplateParameter where
        parseXML x
          = TemplateParameter' <$>
              (x .@? "ParameterKey") <*> (x .@? "DefaultValue") <*>
                (x .@? "NoEcho")
                <*> (x .@? "Description")

instance Hashable TemplateParameter where

instance NFData TemplateParameter where

-- | Contains summary information about the specified CloudFormation type.
--
--
--
-- /See:/ 'typeSummary' smart constructor.
data TypeSummary =
  TypeSummary'
    { _tsLastUpdated      :: !(Maybe ISO8601)
    , _tsTypeName         :: !(Maybe Text)
    , _tsDefaultVersionId :: !(Maybe Text)
    , _tsTypeARN          :: !(Maybe Text)
    , _tsType             :: !(Maybe RegistryType)
    , _tsDescription      :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsLastUpdated' - When the current default version of the type was registered.
--
-- * 'tsTypeName' - The name of the type.
--
-- * 'tsDefaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- * 'tsTypeARN' - The Amazon Resource Name (ARN) of the type.
--
-- * 'tsType' - The kind of type.
--
-- * 'tsDescription' - The description of the type.
typeSummary
    :: TypeSummary
typeSummary =
  TypeSummary'
    { _tsLastUpdated = Nothing
    , _tsTypeName = Nothing
    , _tsDefaultVersionId = Nothing
    , _tsTypeARN = Nothing
    , _tsType = Nothing
    , _tsDescription = Nothing
    }


-- | When the current default version of the type was registered.
tsLastUpdated :: Lens' TypeSummary (Maybe UTCTime)
tsLastUpdated = lens _tsLastUpdated (\ s a -> s{_tsLastUpdated = a}) . mapping _Time

-- | The name of the type.
tsTypeName :: Lens' TypeSummary (Maybe Text)
tsTypeName = lens _tsTypeName (\ s a -> s{_tsTypeName = a})

-- | The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
tsDefaultVersionId :: Lens' TypeSummary (Maybe Text)
tsDefaultVersionId = lens _tsDefaultVersionId (\ s a -> s{_tsDefaultVersionId = a})

-- | The Amazon Resource Name (ARN) of the type.
tsTypeARN :: Lens' TypeSummary (Maybe Text)
tsTypeARN = lens _tsTypeARN (\ s a -> s{_tsTypeARN = a})

-- | The kind of type.
tsType :: Lens' TypeSummary (Maybe RegistryType)
tsType = lens _tsType (\ s a -> s{_tsType = a})

-- | The description of the type.
tsDescription :: Lens' TypeSummary (Maybe Text)
tsDescription = lens _tsDescription (\ s a -> s{_tsDescription = a})

instance FromXML TypeSummary where
        parseXML x
          = TypeSummary' <$>
              (x .@? "LastUpdated") <*> (x .@? "TypeName") <*>
                (x .@? "DefaultVersionId")
                <*> (x .@? "TypeArn")
                <*> (x .@? "Type")
                <*> (x .@? "Description")

instance Hashable TypeSummary where

instance NFData TypeSummary where

-- | Contains summary information about a specific version of a CloudFormation type.
--
--
--
-- /See:/ 'typeVersionSummary' smart constructor.
data TypeVersionSummary =
  TypeVersionSummary'
    { _tvsVersionId   :: !(Maybe Text)
    , _tvsTypeName    :: !(Maybe Text)
    , _tvsARN         :: !(Maybe Text)
    , _tvsTimeCreated :: !(Maybe ISO8601)
    , _tvsType        :: !(Maybe RegistryType)
    , _tvsDescription :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypeVersionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvsVersionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- * 'tvsTypeName' - The name of the type.
--
-- * 'tvsARN' - The Amazon Resource Name (ARN) of the type version.
--
-- * 'tvsTimeCreated' - When the version was registered.
--
-- * 'tvsType' - The kind of type.
--
-- * 'tvsDescription' - The description of the type version.
typeVersionSummary
    :: TypeVersionSummary
typeVersionSummary =
  TypeVersionSummary'
    { _tvsVersionId = Nothing
    , _tvsTypeName = Nothing
    , _tvsARN = Nothing
    , _tvsTimeCreated = Nothing
    , _tvsType = Nothing
    , _tvsDescription = Nothing
    }


-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
tvsVersionId :: Lens' TypeVersionSummary (Maybe Text)
tvsVersionId = lens _tvsVersionId (\ s a -> s{_tvsVersionId = a})

-- | The name of the type.
tvsTypeName :: Lens' TypeVersionSummary (Maybe Text)
tvsTypeName = lens _tvsTypeName (\ s a -> s{_tvsTypeName = a})

-- | The Amazon Resource Name (ARN) of the type version.
tvsARN :: Lens' TypeVersionSummary (Maybe Text)
tvsARN = lens _tvsARN (\ s a -> s{_tvsARN = a})

-- | When the version was registered.
tvsTimeCreated :: Lens' TypeVersionSummary (Maybe UTCTime)
tvsTimeCreated = lens _tvsTimeCreated (\ s a -> s{_tvsTimeCreated = a}) . mapping _Time

-- | The kind of type.
tvsType :: Lens' TypeVersionSummary (Maybe RegistryType)
tvsType = lens _tvsType (\ s a -> s{_tvsType = a})

-- | The description of the type version.
tvsDescription :: Lens' TypeVersionSummary (Maybe Text)
tvsDescription = lens _tvsDescription (\ s a -> s{_tvsDescription = a})

instance FromXML TypeVersionSummary where
        parseXML x
          = TypeVersionSummary' <$>
              (x .@? "VersionId") <*> (x .@? "TypeName") <*>
                (x .@? "Arn")
                <*> (x .@? "TimeCreated")
                <*> (x .@? "Type")
                <*> (x .@? "Description")

instance Hashable TypeVersionSummary where

instance NFData TypeVersionSummary where
