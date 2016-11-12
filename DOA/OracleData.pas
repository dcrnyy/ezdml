// Direct Oracle Access - Dataset for Data-aware components
// Copyright 1997 - 2006 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleData;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion4} DBCommon, {$ENDIF}
  {$IFDEF CompilerVersion6} Variants, {$ENDIF}
  {$IFNDEF NONVISUAL} Dialogs, Controls, Graphics, {$ENDIF}
  DB, {$IFDEF CompilerVersion2006} widestrutils, {$ENDIF} DBConsts, Classes, OracleVisual, TypInfo,
  Oracle, OracleTypes, OracleFilter;
{$ELSE}
uses
  DBCommon, DB, DBConsts, Classes, OracleVisual, Variants,
  {$IFNDEF NONVISUAL} QDialogs, QControls, QGraphics, {$ENDIF}
  TypInfo, Oracle, OracleTypes, OracleFilter;
{$ENDIF}

const // Allow translation of dataset messages
  dmRecordLocked:  string = 'Record is locked by another user';
  dmRecordChanged: string = 'Record has been changed by another user';
  dmRecordDeleted: string = 'Record has been deleted by another user';

type
  TIntArray = array[0..1000000] of Integer;
  PIntArray = ^TIntArray;
  TByteArray = array[0..1000000000] of Byte;
  PByteArray = ^TByteArray;

type
  TBlobList = class(TCollection)
    function  BlobByField(Field: TField): TCollectionItem;
    procedure Copy(Source: TBlobList);
  end;
  TRowId = array[0..18] of char;
  TRecordData = packed record
    Bookmark: Integer;
    MasterBookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
    RowId: TRowId;
    PRowid: PChar;
    BlobList: TBlobList;
    ArraySize: Integer;
    Bytes: TByteArray;
  end;
  PRecordData = ^TRecordData;
  TRecordBlock = packed record
    MaxRecords: Integer;
    RecordCount: Integer;
    Size: Integer;
    Bytes: TByteArray;
  end;
  PRecordBlock = ^TRecordBlock;

const
  RecInfoSize = SizeOf(TRecordData) - SizeOf(TByteArray);
  RowIdAllocations: Integer = 0;

type
  // Some forward declarations
  TOracleDataSet = class;
  TQBEDefinition = class;

  // List type for buffering Oracle Records
  TRecordDataList = class(TObject)
  public
    constructor Create(ADataSet: TOracleDataSet);
    destructor Destroy; override;
    function  FirstFreeRecord: PRecordData;
    function  NewRecord(Index: Integer; AddToFiltered: Boolean): PRecordData;
    procedure DeleteRecord(Index: Integer; DeleteFromFiltered: Boolean);
    procedure FreeRecords;
    function  RecordAt(Index: Integer): PRecordData;
    function  DataAt(Index: Integer): PRecordData;
    procedure CopyData(Source, Dest: PRecordData);
    function  DataIndex(FilteredIndex: Integer): Integer;
    function  FilteredIndex(DataIndex: Integer): Integer;
    procedure FreeBlobs(ARecord: PRecordData);
    function  RecordCount: Integer;
    procedure AddToFilteredList(Data: PRecordData; AtEnd: Boolean);
  private
    DataSet: TOracleDataSet;
    Blocks: TList;
    RecordList: TList;
    FilteredList: TList;
    Unidirectional: Boolean;
    BlockOffset: Integer;
    function  AllocateBlock: PRecordBlock;
    procedure FreeBlock(ABlock: Integer);
    procedure FreeBlocks;
    function  BlockRecord(ABlock: PRecordBlock; Index: Integer): PRecordData;
    function  Count: Integer;
    function  Add(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    procedure Delete(Index: Integer);
    function  GetItem(Index: Integer): Pointer;
    property  Items[Index: Integer]: Pointer read GetItem; default;
    function  IndexOf(Item: Pointer): Integer;
  end;

  // Translate message event
  TTranslateMessageEvent = procedure(Sender: TOracleDataSet; ErrorCode: Integer;
    const ConstraintName: string; Action: char; var Msg: string) of object;

  // Apply record event
  TApplyRecordEvent = procedure(Sender: TOracleDataSet; Action: Char;
    var Applied: Boolean; var NewRowId: string) of object;

  // Cancel QBE event
  TCancelQBEEvent = procedure(Sender: TOracleDataSet; var Requery: Boolean) of object;

  // General OracleDataSet event
  TOracleDataSetEvent = procedure(Sender: TOracleDataSet) of object;

  // AfterFetchRecord event
  TAfterFetchRecordAction = (afContinue, afPause, afStop, afCancel);
  TAfterFetchRecordEvent = procedure(Sender: TOracleDataSet; FilterAccept: Boolean;
    var Action: TAfterFetchRecordAction) of Object;

  // Compression event
  TCompressionEvent = procedure(Sender: TOracleDataSet; Field: TBLOBField;
    InStream, OutStream: TMemoryStream) of Object;

  // LockingMode options
  TLockingModeOptions = (lmLockImmediate, lmCheckImmediate, lmLockDelayed, lmNone);

  // Refresh options
  TRefreshOption = (roBeforeEdit, roAfterInsert, roAfterUpdate, roAllFields);
  TRefreshOptions = set of TRefreshOption;

  // ApplyUpdate options
  TApplyAction = (aaInsert, aaUpdate, aaDelete);
  TApplyActions = set of TApplyAction;

  // TOracleTableInfo
  TOracleColumnInfo = class;
  TOracleTableInfo = class(TCollectionItem)
  public
    TableExpression: string;
    TableOwner: string;
    TableName: string;
    OracleConstraintInfoList: TCollection;
    OracleColumnInfoList: TCollection;
    ColumnDefaultExpressionsRead: Boolean;
    ColumnDefaultValuesRead: Boolean;
    NoColumnDefaults: Boolean;
    Query: TOracleQuery;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure GetConstraints(DataSet: TOracleDataSet);
    procedure GetColumns;
    procedure GetColumnDefaultExpressions(DataSet: TOracleDataSet);
    procedure GetColumnDefaultValues(DataSet: TOracleDataSet);
    function  OracleColumnInfo(const AName: string): TOracleColumnInfo;
  end;

  // TOracleColumnInfo
  TOracleColumnInfo = class(TCollectionItem)
  public
    OracleTableInfo: TOracleTableInfo;
    ColumnName: string;
    ColumnType: Integer;
    ColumnPrecision: Integer;
    ColumnScale: Integer;
    DefaultExpression: string;
    DefaultValue: Variant;
  end;

  // TOracleConstraintInfo
  TConstraintType = (ctUnknown, ctPrimaryKey, ctUniqueKey, ctForeignKey, ctCheck);
  TOracleConstraintInfo = class(TCollectionItem)
  public
    OracleTableInfo: TOracleTableInfo;
    ColumnNames: TStringList;
    ConstraintName: string;
    ConstraintType: TConstraintType;
    RefOwner: string;
    RefTableName: string;
    RefColumnNames: TStringList;
    SearchCondition: string;
    Enabled: Boolean;
    HasBeenRead: Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure ReadFromDictionary(DataSet: TOracleDataSet);
    function  SingleColumn(DataSet: TOracleDataSet): string;
    procedure Check(DataSet: TOracleDataSet);
  end;

  TSequenceApplyMoment = (amOnNewRecord, amOnPost, amOnServer);

  // TSequenceField
  TSequenceField = class(TPersistent)
  private
    FField: string;
    FSequence: string;
    FApplyMoment: TSequenceApplyMoment;
    function  UsesField(const AField: string): Boolean;
    function  IsComplete: Boolean;
    function  ApplyFieldOnServer(const AField: string): Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    DataSet: TOracleDataSet;
    constructor Create(ADataSet: TOracleDataSet);
    function  ValueName: string;
    procedure Apply;
  published
    property Field: string read FField write FField;
    property Sequence: string read FSequence write FSequence;
    property ApplyMoment: TSequenceApplyMoment read FApplyMoment write FApplyMoment default amOnPost;
  end;

  // TDictionary
  TDisabledConstraints = class(TStringList)
  end;
  TOracleDictionary = Class(TPersistent)
  private
    FEnforceConstraints: Boolean;
    FDisabledConstraints: TDisabledConstraints;
    FUseMessageTable: Boolean;
    FDefaultValues: Boolean;
    FDynamicDefaults: Boolean;
    FFieldKinds: Boolean;
    FRangeValues: Boolean;
    FDisplayFormats: Boolean;
    FRequiredFields: Boolean;
    FCharacterSize: Boolean;
    procedure SetDisabledConstraints(Value: TDisabledConstraints);
    procedure SetDisplayFormats(Value: Boolean);
    procedure SetRequiredFields(Value: Boolean);
    procedure SetCharacterSize(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    DataSet: TOracleDataSet;
    constructor Create(ADataSet: TOracleDataSet);
    destructor  Destroy; override;
  published
    property EnforceConstraints: Boolean read FEnforceConstraints write FEnforceConstraints default False;
    property DisabledConstraints: TDisabledConstraints read FDisabledConstraints write SetDisabledConstraints;
    property UseMessageTable: Boolean read FUseMEssageTable write FUseMessageTable default False;
    property DefaultValues: Boolean read FDefaultValues write FDefaultValues default False;
    property DynamicDefaults: Boolean read FDynamicDefaults write FDynamicDefaults default False;
    property FieldKinds: Boolean read FFieldKinds write FFieldKinds default False;
    property DisplayFormats: Boolean read FDisplayFormats write SetDisplayFormats default False;
    property RangeValues: Boolean read FRangeValues write FRangeValues default False;
    property RequiredFields: Boolean read FRequiredFields write SetRequiredFields default True;
    property CharacterSize: Boolean read FCharacterSize write SetCharacterSize default False;
  end;

  // QBE Field definition
  TQBEField = class(TCollectionItem)
  private
    Valid: Boolean;
    OldGetText: TFieldGetTextEvent;
    OldSetText: TFieldSetTextEvent;
    OldChange: TFieldNotifyEvent;
    OldValidChars: TFieldChars;
    OldSize, OldWidth: Integer;
    QBEDefinition: TQBEDefinition;
    FFieldName: string;
    FQueryable: Boolean;
    FAutoPartialMatch: Boolean;
    FAutoContains: Boolean;
    FAutoSoundex: Boolean;
    FCaseInsensitive: Boolean;
    FIgnoreTime: Boolean;
    FLastValue: Variant;
    FValue: widestring;
    function  GetVarName: string;
    function  GetQueryable: Boolean;
    procedure SetQueryable(Value: Boolean);
    function  GetValue: widestring;
    procedure SetValue(Value: widestring);
    procedure GetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure SetText(Sender: TField; const Text: string);
    procedure Change(Sender: TField);
  public
    function IsNull: Boolean;
    property FieldName: string read FFieldName;
    property Queryable: Boolean read GetQueryable write SetQueryable;
    property AutoPartialMatch: Boolean read FAutoPartialMatch write FAutoPartialMatch;
    property AutoContains: Boolean read FAutoContains write FAutoContains;
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive;
    property IgnoreTime: Boolean read FIgnoreTime write FIgnoreTime;
    property VarName: string read GetVarName;
    property LastValue: Variant read FLastValue write FLastValue;
    property Value: widestring read GetValue write SetValue;
    property AutoSoundex: Boolean read FAutoSoundex write FAutoSoundex;
    constructor Create(Collection: TCollection); override;
  end;

  // QBE definition
  TQBEDefinition = class(TPersistent)
  private
    FQBEFontColor: TColor;
    FQBEBackgroundColor: TColor;
    FSaveQBEValues: Boolean;
    FAllowFileWildCards: Boolean;
    FAllowOperators: Boolean;
    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);
    function  GetVersion: Integer;
    function  GetField(Index: Integer): TQBEField;
    function  GetFieldCount: Integer;
    function  GetDataSet: TOracleDataSet;
  protected
    Silent: Boolean;
    QBEFields: TCollection;
    Owner: TComponent;
    constructor Create(AOwner: TComponent);
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    function  Update: Boolean;
    function  FieldByName(const AName: string): TQBEField;
    property  Fields[Index: Integer]: TQBEField read GetField; default;
    property  FieldCount: Integer read GetFieldCount;
    property  DataSet: TOracleDataSet read GetDataSet;
  published
    property SaveQBEValues: Boolean read FSaveQBEValues write FSaveQBEValues default True;
    property AllowFileWildCards: Boolean read FAllowFileWildCards write FAllowFileWildCards default True;
    property QBEFontColor: TColor read FQBEFontColor write FQBEFontColor default clNone;
    property QBEBackgroundColor: TColor read FQBEBackgroundColor write FQBEBackgroundColor default clNone;
    property AllowOperators: Boolean read FAllowOperators write FAllowOperators default False;
  end;

  // SearchRecord types
  TSearchRecordOption = (srForward, srBackward, srFromCurrent, srFromBeginning,
    srFromEnd, srIgnoreCase, srIgnoreTime, srWildcards, srPartialMatch, srDontNavigate);
  TSearchRecordOptions = set of TSearchRecordOption;

  // Field info collection item
  TOracleFieldInfoType = (itField, itUnusedField, itAttr, itOther);
  TOracleFieldInfo = class(TCollectionItem)
  private
    FChildList: TList;
    Field: TField;
    QBEField: TQBEField;
    FieldOffset: Integer;
    ReadOnly: Boolean;
    Required: Boolean;
    QBEReadOnly: Boolean;
    QBERequired: Boolean;
    InfoType: TOracleFieldInfoType;
    FFullName: string;
    ColumnName: string;
    AttrName: string;
    DataType: Integer;
    TypeName: string;
    Size: Integer;
    Scale: Integer;
    Precision: Integer;
    FieldIndex: Integer;
    AttrIndex: Integer;
    IsCollection: Boolean;
    Parent: TOracleFieldInfo;
    IsXMLCLOB: Boolean;
    CharSetID: Integer;
    CharSetForm: Integer;
    IsUTF16: Boolean;
    IsUTF8: Boolean;
    IsWide: Boolean;
    procedure SetFullname(const Value: string);
    function GetChild(Index: Integer): TOracleFieldInfo;
    function GetChildCount: Integer;
    function DataSet: TOracleDataSet;
    function FieldIsNull: Boolean;
    function FieldWasNull: Boolean;
    function FieldChanged: Boolean;
    function FieldHasDefault: Boolean;
    function FieldDefaultExpression: string;
    function FieldNullExpression(WithDefaults: Boolean): string;
    function QueryValue(Q: TOracleQuery): Variant;
    function QueryValueIndex(Q: TOracleQuery): Integer;
    function IsPartOf(Owner: TOracleFieldInfo): Boolean;
    property Children[Index: Integer]: TOracleFieldInfo read GetChild;
    property ChildCount: Integer read GetChildCount;
    property FullName: string read FFullName write SetFullName;
    function RealDataSize: Integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;
  end;

  // Field info collection
  TOracleFieldInfoList = class(TCollection)
  private
    DataSet: TOracleDataSet;
    function GetFieldInfo(Index: Integer): TOracleFieldInfo;
    function FieldInfoByName(const AFieldName: string): TOracleFieldInfo;
  public
    function ForField(AField: TField): TOracleFieldInfo;
    property FieldInfo[Index: Integer]: TOracleFieldInfo read GetFieldInfo; default;
  end;

  {$IFDEF CompilerVersion5}
  TOracleProviderOption = (opNoKeyFields, opNoIndexDefs, opNoDefaultOrder, opNoCommit);
  TOracleProviderOptions = set of TOracleProviderOption;
  {$ENDIF}

  // TOracleDataset
  TOracleDataset = class(TDataSet)
  private
    Records: TRecordDataList;
    DMLQuery: TOracleQuery;
    FExternalCursor: TOracleQuery;
    CursorQuery: TOracleQuery;
    CursorValid: Boolean;
    CursorCreated: Boolean;
    FirstSync: Boolean;
    NoDataEvents: Boolean;
    Posting: Boolean;
    ActiveOnLoading: Boolean;
    MasterBookmark: Integer;
    FieldInfoList: TOracleFieldInfoList;
    RowIdIndex: Integer;
    FRecBufSize: Integer;
    FRecordSize: Integer;
    FCurRec: Integer;
    FLastBookmark: Integer;
    FQuery: TOracleQuery;
    FTableName: string;
    FHasRowId: Boolean;
    FFilterBuffer: PRecordData;
    FilterExpr: TExpression;
    FMaster: TOracleDataSet;
    FMasterFields: string;
    FDetailFields: string;
    FDetails: TList;
    FReadOnly: Boolean;
    FSequenceField: TSequenceField;
    FOracleDictionary: TOracleDictionary;
    FOnTranslateMessage: TTranslateMessageEvent;
    FBeforeQuery: TOracleDataSetEvent;
    FAfterQuery: TOracleDataSetEvent;
    FOnApplyRecord: TApplyRecordEvent;
    FAfterRefreshRecord: TOracleDataSetEvent;
    FAfterFetchRecord: TAfterFetchRecordEvent;
    FLockingMode: TLockingModeOptions;
    FQueryAllRecords: Boolean;
    FUnidirectional: Boolean;
    FCountAllRecords: Boolean;
    FRefreshOptions: TRefreshOptions;
    FUpdatingTable: string;
    FCollectionTypes: TStringList;
    FCollectionNames: TStringList;
    FCommitOnPost: Boolean;
    FDesignActivation: Boolean;
    FDetachable: Boolean;
    FirstFetch: Boolean;
    IdColumns: TStringList;
    Query: TOracleQuery;
    WorkBlobs: TBlobList;
    ChangeLog: TCollection;
    FLogChanges: Boolean;
    FCachedUpdates: Boolean;
    FLogRec: Integer;
    Applying: Boolean;
    JustData: Boolean;
    OldCursor: TCursor;
    QBEFontColorList: TList;
    QBEBkgColorList: TList;
    QBEMaxLengthControlList: TList;
    QBEMaxLengthValueList: TList;
    DontEnterQBE: Boolean;
    FQBEMode: Boolean;
    FQBEDefinition: TQBEDefinition;
    FQBEModified: Boolean;
    FBeforeQBE: TOracleDataSetEvent;
    FAfterQBE: TOracleDataSetEvent;
    FOnCancelQBE: TCancelQBEEvent;
    QBEOldSQL: string;
    QBEError: Boolean;
    NoFieldValidation: Boolean;
    FUniqueFields: string;
    Counted: Boolean;
    RowsAdded: Integer;
    RowCount: Integer;
    // Nested dataset MIDAS support
    {$IFDEF CompilerVersion4}
    FMasterSource: TDataSource;
    FMasterLink: TMasterDataLink;
    // Added by Quest
    FCreateAllFields: Boolean; // Added by Bruce C., Quest Software
    FSyncFieldSizes: Boolean;  // Added by Bruce C., Quest Software
    {$ENDIF}
    FMonId: Integer;
    SentToMonitor: Boolean;
    MonActivityStartTime: LongInt;
    FForceMasterSynchronization: Boolean;
    {$IFDEF CompilerVersion5}
    FProviderOptions: TOracleProviderOptions;
    {$ENDIF}
    Activating: Boolean;
    CompressedBytes, DecompressedBytes: Double;
    FCompressBLOBs: Boolean;
    FOnCompressBLOB: TCompressionEvent;
    FOnDecompressBLOB: TCompressionEvent;
    function  GetRecBufSize: Integer;
    procedure SetOracleFieldInfo;
    procedure SetFieldProperties;
    function  IsReadOnlyField(Field: TField): Boolean;
    procedure FieldToRecord(Field: TField; FieldIsNull: Boolean;
                            FieldData: Pointer; RecordData: PRecordData);
    procedure SetCursorShape;
    procedure RestoreCursorShape;
    function  DoBLOBCompress(Field: TBLOBField; var Buffer: Pointer; var Size: Integer): Boolean;
    function  DoBLOBDecompress(Field: TBLOBField; var Buffer: Pointer; var Size: Integer; FreeCompressed: Boolean): Boolean;
    function  QueryRecord: Boolean;
    procedure QueryRecords;
    function  GetActiveRecBuf(var RecBuf: PRecordData): Boolean;
    function  FieldChanged(Field: TField): Boolean;
    procedure AddRecordIdWhereClause(AQuery: TOracleQuery; OldValues: Boolean);
    procedure OpenDMLQuery(NoDebug: Boolean);
    procedure CloseDMLQuery;
    procedure SetSequenceField(Value: TSequenceField);
    procedure SetOracleDictionary(Value: TOracleDictionary);
    procedure SetVariables(Value: TVariables);
    function  GetVariables: TVariables;
    procedure SetStringFieldsOnly(Value: Boolean);
    function  GetStringFieldsOnly: Boolean;
    procedure SetSQL(Value: TStrings);
    function  GetSQL: TStrings;
    function  TrimStringS(const S: string): string;
    function  TrimStringW(const S: widestring): widestring;
    function  TrimStringV(const S: Variant): Variant;
    procedure InternalSetActive(Value: Boolean);
    function  GetActive: Boolean;
    function  GetSession: TOracleSession;
    procedure SetReadBuffer(Value: Integer);
    function  GetReadBuffer: Integer;
    procedure SetOptimize(Value: Boolean);
    function  GetOptimize: Boolean;
    procedure SetDebug(Value: Boolean);
    function  GetDebug: Boolean;
    procedure SetUpdatingTable(const Value: string);
    procedure SetCursor(Value: TCursor);
    function  GetCursor: TCursor;
    procedure SetCachedUpdates(Value: Boolean);
    function  GetLogChanges: Boolean;
    function  GetPKFields(Q: TOracleQuery; const TableOwner, TableName: string; CheckExists: Boolean): TStringList;
    procedure InitMasterCollection;
    procedure GetCollectionInfo;
    function  GetCollectionMasterFields(Q: TOracleQuery; const MasterOwner, MasterTableName: string): Boolean;
    procedure GetMasterDetailFields;
    procedure SetMaster(Value: TOracleDataSet);
    procedure SetMasterFields(Value: string);
    procedure SetDetailFields(Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetQBEMode(Value: Boolean);
    procedure CheckQBEMode;
    procedure EnterQBEMode(Sync: Boolean);
    procedure CancelQBEMode(DoOnCancelQBE: Boolean);
    procedure SaveQBEValues;
    procedure RestoreQBEValues;
(*    function  CaseInsensitiveExpression(const Value, Column: string): string; *)
    procedure AddQBEToQuery;
    procedure RemoveQBEFromQuery;
    procedure SetQBEColors;
    procedure AddDetail(ADetail: TOracleDataSet);
    procedure RemoveDetail(ADetail: TOracleDataSet);
    procedure SyncDetails(Clear: Boolean);
    procedure SyncDetailVars;
    procedure CopyMasterFieldValues;
    procedure CheckMasterFields;
    procedure EnableMasterFields(NoDetails: Boolean);
    procedure CheckMasterFieldsNull;
    function  CompareMasterFieldValues(Buffer: PRecordData): Boolean;
    procedure CopyMasterVarValues(Buffer: PRecordData);
    function  MasterLess: Boolean;
    function  GetMasterBookmark: Integer;
    function  HasLooseCachedUpdates: Boolean;
    function  IsLooseDetail: Boolean;
    procedure PostDetails;
    function  TranslatedMessage(Action: char; ErrorCode: Integer; const Msg: string): string;
    procedure ResolveName(const ObjectName: string; var TableOwner, TableName: string);
    function  FieldIsGenerated(const AField: string): Boolean;
    procedure CheckConstraints(Field: TField);
    function  FetchRowId(UseOldValues: Boolean): string;
    function  HasDefaults: Boolean;
    function  HasBlobs: Boolean;
    procedure GetQuery;
    function  IsCursorDataSet: Boolean;
    procedure Commit(ForceCommit: Boolean);
    procedure Rollback;
    procedure Lockpoint;
    procedure RollbackToLockpoint;
    function  CalcFieldInList(const List: string): Boolean;
    function  FindChangeLogByBookmark(Bookmark: Integer): TObject;
    function  FindChangeLogByRowId(const ARowId: string): TObject;
    function  FindChangeLogByIndex(Index: Integer): TObject;
    procedure RecordToChangeLog(BeforeData, AfterData: PRecordData; Action: Char; Bookmark, RecordIndex: Integer);
    function  ChangeLogRecordCount: Integer;
    procedure SetQBEDefinition(Value: TQBEDefinition);
    procedure RefreshFilter;
    procedure ActivateFilter;
    // Added by Quest
    {$IFDEF CompilerVersion4}
    procedure CreateUndefinedFields;    // Added by Bruce C., Quest Software
    procedure SyncStringFieldSizes;     // Added by Bruce C., Quest Software
    {$ENDIF}
    function  UseTemporaryLOBs: Boolean;
    procedure CreateFieldDef(FieldInfo: TOracleFIeldInfo);
    procedure InitParentChildAttributes;
    procedure InitObjectFieldDefs(FieldInfo: TOracleFieldInfo);
    procedure SetUnidirectional(Value: Boolean);
    procedure FreeRowId(Buffer: PRecordData);
    procedure SetRowId(Buffer: PRecordData; ARowId: PChar);
    function  GetRowId(Buffer: PRecordData): string;
    procedure SetExternalCursor(Value: TOracleQuery);
    function  GetDetached: Boolean;
    function  GetCompressionFactor: Double;
    {$IFDEF CompilerVersion5}
    function  GetCommitProviderTransactions: Boolean;
    procedure SetCommitProviderTransactions(Value: Boolean);
    {$ENDIF}
    function  RealFieldDataSize(AField: TField): Integer;
  protected
    procedure SetSession(Value: TOracleSession); virtual;
    {$IFDEF CompilerVersion5}
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    procedure SetBlockReadSize(Value: Integer); override;
    {$ENDIF}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function  FieldOracleType(AField: TField): Integer;
    function  GetCanModify: Boolean; override;
    procedure CheckAttached;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  FieldOffset(Field: TField): Integer;
    function  FieldIndex(Field: TField): Integer;
    function  GetRecordSize: Word; override;
    procedure CheckRowId(const ARowId: string; const Action: string);
    function  FetchRecord(Lockit, CheckIt, RefreshIt, AllFields: Boolean;
                          RefreshData: PRecordData): Boolean;
    function  InternalRefreshRecord(AQuery: TOracleQuery; ARecord: PRecordData): Boolean;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure SaveRecordData(Buffer: PRecordData; Index: Integer);
    function  FieldColumnName(AField: TField): string;
    procedure WriteLOBData(ALOB: TLOBLocator; AOracleBlob: TCollectionItem; ACol: TOracleFieldInfo);
    function  IsObjectQuery: Boolean;
    procedure ColSQL(Col: TOracleFieldInfo; Inserting: Boolean;
                     var SQL1, SQL2, ReturningFrom, ReturningInto: string;
                     LOBList: TList; Blobs: TBlobList);
    procedure CreateUpdate(LOBList: TList; Blobs: TBLOBList);
    procedure CreateInsert(LOBList: TList; Blobs: TBLOBList);
    procedure InternalInsertUpdate;
    procedure InternalUpdate;
    procedure InternalDelete; override;
    function  CompareFieldToVariant(AField: TField; const AValue: Variant; CompareOld: Boolean): Boolean;
    function  RecordChanged(Q: TOracleQuery): Boolean;
    procedure LockRecord(CheckOnly: Boolean);
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalFirst; override;
    function  FindBookmark(Bookmark: Integer): Integer;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalRefresh; override;
    procedure Loaded; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure CreateBlobList(var ABlobList: TBlobList);
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function  IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure DoOnNewRecord; override;
    procedure DoBeforeScroll; override;
    procedure CloseBlob(Field: TField); override;
    procedure AddToList;
    procedure RemoveFromList;
    function  InternalQuery: TOracleQuery;
    procedure Reactivate;
    procedure DoAfterInsert; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterCancel; override;
    procedure DoBeforeCancel; override;
    function  GetOracleTableInfo: TOracleTableInfo;
    function  ApplyFilter: Boolean; virtual;
    function  ApplyFilterTo(Data: PRecordData): Boolean;
    procedure SetFiltered(Value: Boolean); override;
    function  UseFiltered: Boolean;
    procedure SetFilterText(const Value: string); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure CalculateFields(Buffer: PChar); override;
    function  GetColumnExpr(const Identifier: String; ParameterList: TParameterList): TExpression;
    function  CreateInternalQuery: TOracleQuery; virtual;
    function  ApplyRecord(Action: Char; var NewRowId: string): Boolean; virtual;
  {$IFDEF CompilerVersion5}
  protected
    { IProviderSupport }
    IParams: TParams;
    ProviderDataSet: Boolean;
    PSLobs: TList;
    procedure PSLobsInit;
    procedure PSLobsWrite;
    procedure PSLobsFree;
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
//    procedure PSGetAttributes(List: TList); virtual;
    function PSGetDefaultOrder: TIndexDef; override;
    function GetKeyFields: string;
    function PSGetKeyFields: string; override;
    function PSGetParams: TParams; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
//    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; virtual;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSStartTransaction; override;
//    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; virtual;
  {$ENDIF CompielrVersion5}
    function  GetMonID: Integer;
    procedure SendStartActivity(const ADescription: string; Parameters: TStrings);
    procedure SendEndActivity(const ADescription, AResultMessage: string; Parameters: TStrings);
    procedure CopyUpdatingTableVariables(AQuery: TOracleQuery);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Post; override;
    function SearchRecord(const FieldNames: string; const FieldValues: Variant;
      Options: TSearchRecordOptions): Boolean;
    function  Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function  BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure ClearVariables;
    procedure DeleteVariable(AName: string);
    procedure DeleteVariables;
    procedure DeclareVariable(AName: string; AType: Integer);
    procedure DeclareVariableCharSet(AName: string; ACharSetForm, ACharSetID: Integer);
    procedure DeclareAndSet(AName: string; AType: Integer; const AValue: Variant);
    procedure DimPLSQLTable(AName: string; TableSize, StringSize: Integer);
    {$IFDEF Overloading}
    procedure SetVariable(Index: Integer; const AValue: Variant); overload;
    function  GetVariable(Index: Integer): Variant; overload;
    {$ENDIF}
    procedure SetVariable(AName: string; const AValue: Variant); {$IFDEF Overloading} overload; {$ENDIF}
    function  GetVariable(AName: string): Variant; {$IFDEF Overloading} overload; {$ENDIF}
    procedure SetComplexVariable(AName: string; const AValue: TObject);
    procedure SetLongVariable(AName: string; Buffer: Pointer; Length: Integer);
    function  GetComplexVariable(AName: string): TObject;
    function  VariableCount: Integer;
    function  VariableName(Index: Integer): string;
    function  VariableType(Index: Integer): Integer;
    function  VariableIndex(AName: string): Integer;
    function  RowId: string;
    function  ErrorLine: Integer;
    function  ErrorPosition: Integer;
    procedure RefreshRecord;
    procedure GetDetailList(var AList: TList);
    procedure StartUpdates;
    procedure ApplyUpdates(Actions: TApplyActions);
    procedure CancelUpdates;
    procedure CommitUpdates;
    function  UpdatesPending: Boolean;
    function  UpdateStatus: TUpdateStatus; {$IFDEF CompilerVersion4} override; {$ENDIF}
    property  LogChanges: Boolean read GetLogChanges;
    procedure GetIdColumns(List: TStrings);
    function  GetUpdatingAlias: string; virtual;
    function  IsCollectionDataSet: Boolean;
    function  GetUpdatingTable: string; virtual;
    procedure ExecuteQBE;
    procedure DescribeQBE;
    procedure ToggleQBEValues;
    procedure ClearQBE;
    function  CanQBE(RaiseException: Boolean): Boolean;
    procedure CloseAll;
    function  CountQueryHits: Integer;
    procedure DeclareQueryVariables(AQuery: TOracleQuery);
    procedure SetQueryVariables(AQuery: TOracleQuery);
    procedure GetQueryVariables(AQuery: TOracleQuery);
    function  CompareQueryVariables(AQuery: TOracleQuery): Boolean;
    function  SubstitutedSQL: string;
    procedure ExecSQL;
    property  MonId: Integer read GetMonID;
    procedure SendToMonitor(Creation, Signal: Boolean);
    property  ForceMasterSynchronization: Boolean read FForceMasterSynchronization write FForceMasterSynchronization;
    property  QBEModified: Boolean read FQBEModified;
    function  CreateObject(const FieldName: string): TOracleObject;
    property  ExternalCursor: TOracleQuery read FExternalCursor write SetExternalCursor;
    procedure DataSetChange;
    property  CompressionFactor: Double read GetCompressionFactor;
    {$IFDEF CompilerVersion5}
    property  CommitProviderTransactions: Boolean read GetCommitProviderTransactions write SetCommitProviderTransactions;
    {$ENDIF}
  published
    property SQL: TStrings read GetSQL write SetSQL;
    property ReadBuffer: Integer read GetReadBuffer write SetReadBuffer default 25;
    property Optimize: Boolean read GetOptimize write SetOptimize default True;
    property Debug: Boolean read GetDebug write SetDebug default False;
    property Variables: TVariables read GetVariables write SetVariables;
    property StringFieldsOnly: Boolean read GetStringFieldsOnly write SetStringFieldsOnly default False;
    property SequenceField: TSequenceField read FSequenceField write SetSequenceField;
    property OracleDictionary: TOracleDictionary read FOracleDictionary write SetOracleDictionary;
    property QBEDefinition: TQBEDefinition read FQBEDefinition write SetQBEDefinition;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property AutoCalcFields;
    property UniqueFields: string read FUniqueFields write FUniqueFields;
    {$IFDEF CompilerVersion4}
    // Added by Quest
//    property SyncFieldSizes: Boolean read FSyncFieldSizes write FSyncFieldSizes;
//    property CreateAllFields: Boolean read FCreateAllFields write FCreateAllFields;
    {$ENDIF}
    property Master: TOracleDataSet read FMaster write SetMaster;
    property MasterFields: string read FMasterFields write SetMasterFields;
    property DetailFields: string read FDetailFields write SetDetailFields;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property LockingMode: TLockingModeOptions read FLockingMode write FLockingMode default lmCheckImmediate;
    property QueryAllRecords: Boolean read FQueryAllRecords write FQueryAllRecords default True;
    property Unidirectional: Boolean read FUnidirectional write SetUnidirectional default False;
    property CountAllRecords: Boolean read FCountAllRecords write FCountAllRecords default False;
    property RefreshOptions: TRefreshOptions read FRefreshOptions write FRefreshOptions default [];
    property OnTranslateMessage: TTranslateMessageEvent read FOnTranslateMessage write FOnTranslateMessage;
    property BeforeQuery: TOracleDataSetEvent read FBeforeQuery write FBeforeQuery;
    property AfterQuery: TOracleDataSetEvent read FAfterQuery write FAfterQuery;
    property BeforeQBE: TOracleDataSetEvent read FBeforeQBE write FBeforeQBE;
    property AfterQBE: TOracleDataSetEvent read FAfterQBE write FAfterQBE;
    property OnCancelQBE: TCancelQBEEvent read FOnCancelQBE write FOnCancelQBE;
    property OnApplyRecord: TApplyRecordEvent read FOnApplyRecord write FOnApplyRecord;
    property AfterFetchRecord: TAfterFetchRecordEvent read FAfterFetchRecord write FAfterFetchRecord;
    property AfterRefreshRecord: TOracleDataSetEvent read FAfterRefreshRecord write FAfterRefreshRecord;
    property UpdatingTable: string read FUpdatingTable write SetUpdatingTable;
    property CommitOnPost: Boolean read FCommitOnPost write FCommitOnPost default True;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default False;
    property QBEMode: Boolean read FQBEMode write SetQBEMode default False;
    property Session: TOracleSession read GetSession write SetSession;
    property DesignActivation: Boolean read FDesignActivation write FDesignActivation default False;
    property Detachable: Boolean read FDetachable write FDetachable default False;
    property Detached: Boolean read GetDetached;
    property CompressBLOBs: Boolean read FCompressBLOBs write FCompressBLOBS default False;
    property OnCompressBLOB: TCompressionEvent read FOnCompressBLOB write FOnCompressBLOB;
    property OnDecompressBLOB: TCompressionEvent read FOnDecompressBLOB write FOnDecompressBLOB;
    property Active: Boolean read GetActive write InternalSetActive default False;
    property Filter;
    property Filtered;
    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    {$IFDEF CompilerVersion5}
    property BeforeRefresh;
    property AfterRefresh;
    property ProviderOptions: TOracleProviderOptions read FProviderOptions write FProviderOptions default [];
    {$ENDIF}
  end;

const
  dtNoQBEForCursor: string      = 'QBE mode not supported for cursors';
  dtNoQBEUpdatesPending: string = 'QBE mode not allowed with pending updates';
  dtNoQBEOperation: string      = 'Operation not allowed in QBE mode';
  dtQBEOperation: string        = 'Operation only allowed in QBE mode';
  dtNoQBEMasterLess: string     = 'QBE mode not allowed without a master record';
  dtNoQBEDetached: string       = 'QBE mode not allowed for a detached dataset';
  dtNoMasterRefresh: string     = 'Refresh not allowed: detail records have cached updates.'#13#10'You have to apply the updates first';

const
  AllowSetFieldInBrowseState: Boolean = False;

implementation

uses
  OracleCI, OracleCompress,
{$IFNDEF NONVISUAL}
  OracleSelForm,
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Math;
{$ELSE}
  Windows, SysUtils, Math;
{$ENDIF}

const
  Identifiers = ['a'..'z', 'A'..'Z', '0'..'9', '_', '#', '$', '.', '"', '@', #128..#255];
  Blanks      = [' ', #9, #13, #10];

const
  IgnoreState = [csAncestor {$IFDEF CompilerVersion6} , csFreeNotification {$ENDIF}];

{$IFNDEF CompilerVersion5}
const
  ftWideString = ftUnknown;
{$ENDIF}

const // Commit modes
  cmAlways = True;
  cmNormal = False;

const
  QBEComment = '/* Modified for QBE */';
  EndRefreshComment = '/* END_REFRESH */';

const
  CompressionMagicValue1: Integer = $38A60091;
  CompressionMagicValue2: Integer = $1BCCB788;
  CompressionMagicValueSize = SizeOf(CompressionMagicValue1) + SizeOf(CompressionMagicValue2);

type
  // TOracleBlob
  TOracleBlob = class(TCollectionItem)
  public
    BlobPointer: PByteArray;
    BlobSize: Integer;
    Compressed: Boolean;
    CompressedPointer: PByteArray;
    CompressedSize: Integer;
    Field: TBlobField;
    destructor Destroy; override;
    procedure  FreeBlob;
    procedure  Decompress;
    procedure  Compress;
  end;

  // TOracleBlobStream
  TOracleBlobStream = class(TStream)
  private
    FField: TBlobField;
    FOracleBlob: TOracleBlob;
    FDataSet: TOracleDataSet;
    FBuffer: PRecordData;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    function GetBlobSize: Longint;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

  // TChangeLogItem
  TChangeLogItem = class(TCollectionItem)
  public
    DataSet: TOracleDataSet;
    Action: Char;            // 'I' = Insert, 'U' = Update, 'D' = Delete
    RecordIndex: Integer;    // Index in Records collection
    BeforeData: PRecordData; // Record before update or delete (nil if insert)
    AfterData: PRecordData;  // Record after update or insert (nil if delete)
    Applied: Boolean;        // Has change been applied to the database?
    Visible: Boolean;        // Is record of change log visible in the dataset?
    constructor CreateForDataSet(ADataSet: TOracleDataSet);
    destructor  Destroy; override;
    procedure FreeBeforeData;
    procedure SetBeforeData(ARecordData: PRecordData);
    procedure FreeAfterData;
    procedure SetAfterData(ARecordData: PRecordData);
    procedure GetAfterData(ARecordData: PRecordData);
    procedure Cancel;
    function  InDataSet: Boolean;
    function  GetBookmark: Integer;
    procedure SetBookmark(Value: Integer);
    property  Bookmark: Integer read GetBookmark write SetBookmark;
  end;

  // This type is used by the filter expression object to return the value of a column
  TColumnExpression = class(TExpression)
  private
    fDataset: TDataset;
    fField: string;
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetIsNull: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ADataset : TDataset; AField : string);
  end;

  {$IFNDEF NONVISUAL}
  // Inherited Control (to access some properties for QBE colors)
  TMyControl = class(TControl)
  end;
  {$ENDIF}

// Add source string to destination string with comma
procedure AddWithComma(const Source: string; var Destination: string; WithSpace: Boolean);
begin
  if Source = '' then Exit;
  if Destination = '' then
    Destination := Source
  else begin
    if WithSpace then
      Destination := Destination + ', ' + Source
    else
      Destination := Destination + ',' + Source;
  end;
end;

function FirstWord(const s: string): string;
var i: Integer;
    ws: WideString;
begin
  ws := s;
  for i := 1 to Length(ws) do
  begin
    if WideCharIn(ws[i], Blanks) then
    begin
      SetLength(ws, i - 1);
      Break
    end;
  end;
  Result := ws;
end;

// TOracleTableInfo
constructor TOracleTableInfo.Create(Collection: TCollection);
begin
  inherited;
  TableExpression := '';
  TableOwner := '';
  TableName := '';
  OracleConstraintInfoList := nil;
  OracleColumnInfoList := nil;
  ColumnDefaultExpressionsRead := False;
  ColumnDefaultValuesRead := False;
  Query := TOracleQuery.Create(nil);
  Query.Name := 'TableInfoQuery';
end;

destructor TOracleTableInfo.Destroy;
begin
  if OracleConstraintInfoList <> nil then OracleConstraintInfoList.Free;
  if OracleColumnInfoList <> nil then OracleColumnInfoList.Free;
  Query.Free;
  inherited;
end;

// Get all constraints from the Oracle Dictionary
procedure TOracleTableInfo.GetConstraints(DataSet: TOracleDataSet);
var ThisConstraint: string;
    C: TOracleConstraintInfo;
begin
  // Already read?
  if OracleConstraintInfoList <> nil then Exit;
  OracleConstraintInfoList := TCollection.Create(TOracleConstraintInfo);
  // Open a query to select all constraint columns for this table
  with Query do
  try
    Debug := False;
    Clear;
    SQL.Add('select constraint_name, column_name from sys.all_cons_columns');
    SQL.Add('where owner = :owner and table_name = :table_name');
    if not DataSet.Session.POLite then
      SQL.Add('and constraint_name not like ''SYS_C%''');
    SQL.Add('order by constraint_name, position');
    DeclareVariable('owner', otString);
    DeclareVariable('table_name', otString);
    SetVariable('owner', TableOwner);
    SetVariable('table_name', TableName);
    Execute;
    // Loop through all constraint columns to find constraints and their columns
    C := nil;
    while not EOF do
    begin
      ThisConstraint := Field('constraint_name');
      if (C = nil) or (C.ConstraintName <> ThisConstraint) then
      begin
        C := TOracleConstraintInfo.Create(OracleConstraintInfoList);
        C.ConstraintName := ThisConstraint;
        C.ColumnNames := TStringList.Create;
        C.OracleTableInfo := Self;
      end;
      C.ColumnNames.Add(Field('column_name'));
      Next;
    end;
  except
    OracleConstraintInfoList.Clear;
    raise;
  end;
end;

// Determine the columns of the table
procedure TOracleTableInfo.GetColumns;
var C: TOracleColumnInfo;
    f: Integer;
begin
  // Already read?
  if OracleColumnInfoList <> nil then Exit;
  OracleColumnInfoList := TCollection.Create(TOracleColumnInfo);
  // Open a query to describe for the table
  with Query do
  try
    Debug := False;
    Clear;
    SQL.Text := 'select * from ' + TableExpression;
    Describe;
    for f := 0 to FieldCount - 1 do
    begin
      C := TOracleColumnInfo.Create(OracleColumnInfoList);
      C.ColumnName := FieldName(f);
      C.ColumnType := FieldType(f);
      C.ColumnPrecision := FieldPrecision(f);
      C.ColumnScale := FieldScale(f);
      C.DefaultExpression := '';
      C.DefaultValue := Null;
    end;
  except
    on E: Exception do
    begin
      OracleColumnInfoList.Clear;
      ShowMessage(E.Message);
    end;
  end;
end;

// Determine the default expressions for all columns of the table
procedure TOracleTableInfo.GetColumnDefaultExpressions(DataSet: TOracleDataSet);
var C: TOracleColumnInfo;
begin
  // Already read?
  if ColumnDefaultExpressionsRead then Exit;
  ColumnDefaultExpressionsRead := True;
  // Read the columns
  GetColumns;
  // Open a query to select the default expressions
  with Query do
  begin
    Debug := False;
    Clear;
    SQL.Add('select column_name, data_default from sys.all_tab_columns');
    SQL.Add('where owner = :owner and table_name = :table_name');
    SQL.Add('and data_default is not null');
    DeclareVariable('owner', otString);
    DeclareVariable('table_name', otString);
    SetVariable('owner', TableOwner);
    SetVariable('table_name', TableName);
    Execute;
    NoColumnDefaults := True;
    while not Eof do
    begin
      C := OracleColumnInfo(Field('column_name'));
      if C <> nil then C.DefaultExpression := string(Field('data_default'));
      NoColumnDefaults := False;
      Next;
    end;
  end;
end;

// Determine the default values for all columns of the table
procedure TOracleTableInfo.GetColumnDefaultValues(DataSet: TOracleDataSet);
var C: TOracleColumnInfo;
    F: TField;
    i: Integer;
begin
  // Get the deefault expressions
  GetColumnDefaultExpressions(DataSet);
  // Are there any?
  if NoColumnDefaults then Exit;
  // Is it necessary to determine the default values now?
  if DataSet.OracleDictionary.DynamicDefaults or not ColumnDefaultValuesRead then
  begin
    ColumnDefaultValuesRead := True;
    // Execute a PL/SQL block to get the default values
    with Query do
    begin
      Debug := DataSet.Debug;
      Clear;
      SQL.Add('begin');
      for i := 0 to OracleColumnInfoList.Count - 1 do
      begin
        C := TOracleColumnInfo(OracleColumnInfoList.Items[i]);
        if C.DefaultExpression <> '' then
        begin
          SQL.Add('  :' + C.ColumnName + ' := ' + C.DefaultExpression + ';');
          DeclareVariable(C.ColumnName, C.ColumnType);
        end;
      end;
      SQL.Add('end;');
      Execute;
      // Retrieve the defaults and store them with the column info
      for i := 0 to OracleColumnInfoList.Count - 1 do
      begin
        C := TOracleColumnInfo(OracleColumnInfoList.Items[i]);
        if C.DefaultExpression <> '' then
          C.DefaultValue := GetVariable(C.ColumnName);
      end;
    end;
  end;
  // Update the record in the dataset
  for i := 0 to OracleColumnInfoList.Count - 1 do
  begin
    C := TOracleColumnInfo(OracleColumnInfoList.Items[i]);
    if C.DefaultExpression <> '' then
    begin
      F := DataSet.FindField(C.ColumnName);
      if F <> nil then F.Value := C.DefaultValue;
    end;
  end;
end;

// Return the column info
function TOracleTableInfo.OracleColumnInfo(const AName: string): TOracleColumnInfo;
var C: TOracleColumnInfo;
    i: Integer;
begin
  // Read the columns
  GetColumns;
  // Search it
  for i := 0 to OracleColumnInfoList.Count - 1 do
  begin
    C := TOracleColumnInfo(OracleColumnInfoList.Items[i]);
    if AName = C.ColumnName then
    begin
      Result := C;
      Exit;
    end;
  end;
  Result := nil;
end;

// TOracleConstraintInfo
constructor TOracleConstraintInfo.Create(Collection: TCollection);
begin
  inherited;
  OracleTableInfo := nil;
  ColumnNames := nil;
  ConstraintName := '';
  ConstraintType := ctUnknown;
  RefOwner := '';
  RefTableName := '';
  RefColumnNames := nil;
  SearchCondition := '';
  Enabled := False;
  HasBeenRead := False;
end;

destructor TOracleConstraintInfo.Destroy;
begin
  if ColumnNames <> nil then ColumnNames.Free;
  if RefColumnNames <> nil then RefColumnNames.Free;
  inherited;
end;

procedure TOracleConstraintInfo.ReadFromDictionary(DataSet: TOracleDataSet);
var ct, s: string;
    c, p1, p2: Integer;
    RefConstraintName: string;
begin
  // Was it already read?
  if HasBeenRead then Exit;
  HasBeenRead := True;
  // Open query to determine constraint properties
  DataSet.OpenDMLQuery(True);
  with DataSet.DMLQuery do
  begin
    Clear;
    SQL.Add('select constraint_type, r_constraint_name, r_owner, search_condition');
    SQL.Add('  from sys.all_constraints');
    SQL.Add(' where constraint_name = :constraint_name and owner = :owner');
    // For POLITE, all constraints seem disabled, but are enabled
    // Check constraints cannot be evaluated due to missing PL/SQL
    if not Session.POLite then
      SQL.Add('   and status = ''ENABLED''')
    else
      SQL.Add('   and constraint_type != ''C''');
    DeclareVariable('constraint_name', otString);
    DeclareVariable('owner', otString);
    SetVariable('constraint_name', ConstraintName);
    SetVariable('owner', OracleTableInfo.TableOwner);
    Execute;
    if not EOF then
    begin
      Enabled := True;
      ct := Field('constraint_type');
      case ct[1] of
        'P' : ConstraintType := ctPrimaryKey;
        'U' : ConstraintType := ctUniqueKey;
        'R' : ConstraintType := ctForeignKey;
        'C' : ConstraintType := ctCheck;
      end;
      // In case of a check constraint, copy the search condition and place
      // a ':' before each occurrence of the column names to make it a variable
      if ConstraintType = ctCheck then
      begin
        SearchCondition := Field('search_condition');
        for c := 0 to ColumnNames.Count - 1 do
        begin
          s := AnsiUpperCase(SearchCondition);
          repeat
            p1 := Pos(ColumnNames[c], s);
            if p1 > 0 then
            begin
              p2 := p1 + Length(ColumnNames[c]);
              s[p1] := ' '; // We won't find this one next time we call Pos
              // Check for column and "column", replace both by :column
              if ((p1 = 1) or not (s[p1 - 1] in Identifiers)) and
                 ((p2 > Length(s)) or not (s[p2] in Identifiers)) then
              begin
                Insert(':', SearchCondition, p1);
                Insert(':', s, p1);
              end else begin
                if (p1 > 1) and (s[p1 - 1] = '"') and
                   (p2 <= Length(s)) and (s[p2] = '"') then
                begin
                  Delete(SearchCondition, p2, 1);
                  SearchCondition[p1 - 1] := ':';
                  Delete(s, p2, 1);
                  s[p1 - 1] := ':';
                end;
              end;
            end;
          until p1 = 0;
        end;
      end;
      // In case of a foreign key constraint, determine the referenced table
      // and column names
      if ConstraintType = ctForeignKey then
      begin
        RefOwner := Field('r_owner');
        RefConstraintName := Field('r_constraint_name');
        RefColumnNames := TStringList.Create;
        Clear;
        SQL.Add('select table_name, column_name from sys.all_cons_columns');
        SQL.Add(' where owner = :owner and constraint_name = :constraint_name');
        SQL.Add(' order by position');
        DeclareVariable('owner', otString);
        DeclareVariable('constraint_name', otString);
        SetVariable('owner', RefOwner);
        SetVariable('constraint_name', RefConstraintName);
        Execute;
        while not EOF do
        begin
          RefTableName := Field('table_name');
          RefColumnNames.Add(Field('column_name'));
          Next;
        end;
      end;
    end;
  end;
  DataSet.CloseDMLQuery;
end;

// Determine if this constraint is considered single column for the given dataset
function TOracleConstraintInfo.SingleColumn(DataSet: TOracleDataSet): string;
var Col: Integer;
    F: TField;
begin
  Result := '';
  for Col := 0 to ColumnNames.Count - 1 do
  begin
    F := DataSet.FindField(ColumnNames[Col]);
    if (F <> nil) and (F.FieldKind = fkData) and (not F.ReadOnly) then
    begin
      if Result = '' then
        Result := ColumnNames[Col]
      else begin
        Result := '*';
        Exit;
      end;
    end;
  end;
end;

// Check if a field is null (according to Oracle)
function NullField(AField: TField): Boolean;
begin
  Result := AField.IsNull or ((AField.DataType in [ftString, ftMemo, ftWideString]) and (AField.Value = ''));
end;

// Check the constraint for the current record in the dataset
procedure TOracleConstraintInfo.Check(DataSet: TOracleDataSet);
var Action: char;
    Err, i: Integer;
    Msg, s: string;
    p1, p2: Integer;
    CField: TField;
    CName, RName: string;
    FromString, IntoString: string;
    IncludeRowid: Boolean;
    AllNulls: Boolean;
    T: TOracleTableInfo;
    C: TOracleColumnInfo;

 function FieldVariable(AField: TField; WithNulls: Boolean): Boolean;
 begin
   Result := False;
   if AField <> nil then
   begin
     if WithNulls or not NullField(AField) then
     begin
       DataSet.DMLQuery.DeclareVariable(AField.FieldName, DataSet.FieldOracleType(AField));
       DataSet.DMLQuery.SetVariable(AField.FieldName, AField.Value);
       Result := True;
     end;
   end;
 end;

begin
  // Read constraint properties from the Oracle Dictionary
  ReadFromDictionary(DataSet);
  // If the constraint is not enabled, don't check it
  if not Enabled then Exit;
  Err := 0;
  // Primary and Unique Key Constraint are handled the same
  if ConstraintType in [ctPrimaryKey, ctUniqueKey] then
  begin
    // Open a query to determine if a record with this value for the
    // primary/unique constraint column already exists
    DataSet.OpenDMLQuery(False);
    with DataSet.DMLQuery do
    begin
      Clear;
      SQL.Add('select ''x'' from ' + DataSet.GetUpdatingTable);
      // Ignore this record for an update
      if DataSet.UpdateStatus = usInserted then
      begin
        IncludeRowid := False;
        s := 'where ';
      end else begin
        IncludeRowid := True;
        s := 'where rowid <> :doa__rowid' + #13#10 + 'and ';
      end;
      AllNulls := True;
      // Include all constraint columns in the where-clause
      for i := 0 to ColumnNames.Count - 1 do
      begin
        CName := ColumnNames[i];
        if i > 0 then s := 'and ';
        // Find the field
        CField := DataSet.FindField(CName);
        if CField <> nil then
        begin
          // If found, add it to the where-clause, "is null" for nulls
          if not FieldVariable(CField, False) then
            s := s + CName + ' is null'
          else begin
            s := s + CName + ' = :' + CName;
            AllNulls := False;
          end;
        end else begin
          // The Field is not included for an insert: don't check
          if DataSet.UpdateStatus = usInserted then Exit;
          // The field is not included for an update: select the value
          // DOESN'T WORK FOR NULL VALUES!!
          s := s + CName + ' = (select ' + CName + ' from ' + DataSet.GetUpdatingTable +
               ' where rowid = :doa__rowid)';
          IncludeRowId := True;
        end;
        SQL.Add(s);
      end;
      if AllNulls then Exit;
      if IncludeRowid then
      begin
        DeclareVariable('doa__rowid', otString);
        SetVariable('doa__rowid', DataSet.RowId);
      end;
      Execute;
      // ORA-00001: Unique constraint (OWNER.CONSTRAINT) violated
      if not EOF then Err := 1;
    end;
  end;
  // Foreign Key Constraint
  if ConstraintType = ctForeignKey then
  begin
    // Open a query to determine if a record with this value for the
    // foreign key column exists
    DataSet.OpenDMLQuery(False);
    with DataSet.DMLQuery do
    begin
      Clear;
      SQL.Add('select ''x'' from ' + RefOwner + '.' + RefTableName);
      IncludeRowId := False;
      AllNulls := True;
      // Include all constraint columns in the where-clause
      for i := 0 to RefColumnNames.Count - 1 do
      begin
        CName := ColumnNames[i];
        RName := RefColumnNames[i];
        if i > 0 then s := 'and ' else s := 'where ';
        // Find the field
        CField := DataSet.FindField(CName);
        if CField <> nil then
        begin
          // If found, add it to the where-clause, "is null" for nulls
          if not FieldVariable(CField, False) then
            s := s + RName + ' is null'
          else begin
            s := s + RName + ' = :' + CName;
            AllNulls := False;
          end;
        end else begin
          // The Field is not included for an insert: don't check
          if DataSet.UpdateStatus = usInserted then Exit;
          // The field is not included for an update: select the value
          // DOESN'T WORK FOR NULL VALUES!!
          s := s + RName + ' = (select ' + CName + ' from ' + DataSet.GetUpdatingTable +
               ' where rowid = :doa__rowid)';
          IncludeRowId := True;
        end;
        SQL.Add(s);
      end;
      If AllNulls then Exit;
      if IncludeRowid then
      begin
        DeclareVariable('doa__rowid', otString);
        SetVariable('doa__rowid', DataSet.RowId);
      end;
      Execute;
      if EOF then Err := 2291;
      // ORA-02291: Integrity constraint (O.C) violated - parent key not found
    end;
  end;
  // Check Constraint
  if ConstraintType = ctCheck then
  begin
    // Open a query to test the search condition of the check constraint
    DataSet.OpenDMLQuery(False);
    with DataSet.DMLQuery do
    begin
      Clear;
      FromString := '';
      IntoString := '';
      SQL.Add('begin');
      // Declare a variable for each constraint column
      for i := 0 to ColumnNames.Count - 1 do
      begin
        CName := ColumnNames[i];
        // Find the field
        CField := DataSet.FindField(CName);
        if CField <> nil then
          FieldVariable(CField, True)
        else begin
          // The Field is not included for an insert: don't check
          if DataSet.UpdateStatus = usInserted then Exit;
          // Not found, determine the column data type
          T := DataSet.GetOracleTableInfo;
          C := T.OracleColumnInfo(CName);
          // Not found, can't check it
          if C = nil then Exit;
          DeclareVariable(CName, C.ColumnType);
          if FromString <> '' then
          begin
            FromString := FromString + ', ';
            IntoString := IntoString + ', ';
          end;
          FromString := FromString + CName;
          IntoString := IntoString + ':' + CName;
        end;
      end;
      if FromString <> '' then
      begin
        SQL.Add('  select ' + FromString + ' into ' + IntoString + ' from ' +
                 DataSet.GetUpdatingTable + ' where rowid = :doa__rowid;');
        DeclareVariable('doa__rowid', otString);
        SetVariable('doa__rowid', DataSet.RowId);
      end;
      SQL.Add('  if not (' + SearchCondition + ') then');
      SQL.Add('    :error := 2290;');
      SQL.Add('  else');
      SQL.Add('    :error := 0;');
      SQL.Add('  end if;');
      if FromString <> '' then
      begin
        SQL.Add('exception');
        SQL.Add('  when others then :error := 0;');
      end;
      SQL.Add('end;');
      DeclareVariable('error', otInteger);
      Execute;
      Err := GetVariable('error');
      // ORA-02290: Check constraint (OWNER.CONSTRAINT) violated
   end;
  end;
  DataSet.CloseDMLQuery;
  // If a violation was detected, translate the message and raise an error
  if Err > 0 then
  begin
    // Determine the action
    if DataSet.UpdateStatus = usInserted then Action := 'I' else Action := 'U';
    if not DataSet.Session.POLite then
    begin
      // Determine the error message and insert OWNER.CONSTRAINT
      Msg := DataSet.Session.ErrorMessage(Err);
      p1 := Pos('(', Msg);
      p2 := Pos(')', Msg);
      if (p1 > 0) and (p2 > p1) then
      begin
        Delete(Msg, p1 + 1, p2 - p1 - 1);
        Insert(OracleTableInfo.TableOwner + '.' + ConstraintName, Msg, p1 + 1);
      end;
    end else begin
      // For POLITE, all messages are OCA-30021
      Msg := DataSet.Session.ErrorMessage(30021);
      if Err = 1 then
        Msg := Msg + '[POL-3220] duplicate keys in unique/primary index';
      if Err = 2291 then
        Msg := Msg + '[POL-5143] integrity constraint violation';
      Err := 30021;
    end;
    // Translate the message and raise the error
    DatabaseError(DataSet.TranslatedMessage(Action, Err, Msg));
  end;
end;

// Miscellaneous functions

// Compare if two Doubles are as good as equal
// This function is necessary because SQL*Net converts floating point data
// a little bit different than Delphi
function AlmostEqual(D1, D2: Double): Boolean;
begin
  if D2 <> 0 then D1 := (D1 - D2) / D2;
  Result := Abs(D1) < 0.0000000001;
end;

function WideUpperCase(const S: WideString): WideString;
var Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
end;

// Find a whole word in a string
function WordPos(const AWord, AString: widestring): Integer;
var s: widestring;
    i, p: Integer;
begin
  s := ' ' + WideUpperCase(AString) + ' ';
  for i := 1 to Length(s) do if not WideCharIn(s[i], Identifiers) then s[i] := ' ';
  p := Pos(' ' + WideUpperCase(AWord) + ' ', s);
  Result := p;
end;

// QBE Field Object
constructor TQBEField.Create(Collection: TCollection);
begin
  inherited;
  FFieldName := '';
  FQueryable := True;
  FAutoPartialMatch := False;
  FAutoContains := False;
  FAutoSoundex := False;
  FCaseInsensitive := False;
  FIgnoreTime := False;
  FLastValue := Null;
  FValue := '';
  QBEDefinition := nil;
  Valid := True;
end;

function TQBEField.GetVarName: string;
var FieldDef: TFieldDef;
    FieldIndex: Integer;
begin
  FieldDef := nil;
  FieldIndex := QBEDefinition.DataSet.FieldDefs.IndexOf(FieldName);
  if FieldIndex >= 0 then
    FieldDef := QBEDefinition.DataSet.FieldDefs[FieldIndex];
  if FieldDef = nil then Result := '' else Result := 'doa__qbe' + IntToStr(FieldDef.FieldNo);
end;

function TQBEField.GetQueryable: Boolean;
var FieldDef: TFieldDef;
    FieldIndex: Integer;
begin
  Result := FQueryable;
  if Result then
  begin
    if (QBEDefinition <> nil) and
       (QBEDefinition.DataSet <> nil) and
       (QBEDefinition.DataSet.FieldDefs <> nil) then
    begin
      FieldDef := nil;
      FieldIndex := QBEDefinition.DataSet.FieldDefs.IndexOf(FieldName);
      if FieldIndex >= 0 then
        FieldDef := QBEDefinition.DataSet.FieldDefs[FieldIndex];
      if (FieldDef <> nil) and
         (FieldDef.DataType in [ftBlob, ftGraphic, ftMemo
                               {$IFDEF CompilerVersion5}, ftOraClob, ftOraBlob {$ENDIF}]) then
        Result := False;
    end;
  end;
end;

procedure TQBEField.SetQueryable(Value: Boolean);
begin
  FQueryable := Value;
  FQueryable := GetQueryable;
end;

function TQBEField.GetValue: widestring;
var F: TField;
begin
  F := QBEDefinition.DataSet.FindField(FFieldName);
  if QBEDefinition.AllowOperators and (not QBEDefinition.DataSet.FieldInfoList.ForField(F).IsWide) then
    Result := FValue
  else begin
    if (F = nil) or VarIsNull(F.Value) then
      Result := ''
    else
      Result := F.Value;
  end;
end;

procedure TQBEField.SetValue(Value: widestring);
var F: TField;
begin
  F := QBEDefinition.DataSet.FieldByName(FFieldName);
  if QBEDefinition.AllowOperators and (not QBEDefinition.DataSet.FieldInfoList.ForField(F).IsWide) then
  begin
    FValue := Value;
//  if QBEDefinition.DataSet.Active then QBEDefinition.DataSet.DataEvent(deFieldChange, Longint(F));
  end else begin
    if F <> nil then
    begin
     if Value = '' then
       F.AsString := Value
     else
       F.Value := Value;
    end;
  end;
end;

procedure TQBEField.GetText(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  if QBEDefinition.AllowOperators and (not QBEDefinition.DataSet.FieldInfoList.ForField(Sender).IsWide) then
  begin
    // MemoFields don't have OnSetText!
    if (Sender.AsString <> '') then
    begin
      FValue := Sender.AsString;
    end;
    Text := Value;
  end else
    Text := Sender.AsString;
end;

var SetTextField: TField;

procedure TQBEField.SetText(Sender: TField; const Text: string);
begin
  SetTextField := Sender;
  if QBEDefinition.AllowOperators and (not QBEDefinition.DataSet.FieldInfoList.ForField(Sender).IsWide) then
    Value := Text
  else
    Sender.AsString := Text;
end;

procedure TQBEField.Change(Sender: TField);
begin
  if QBEDefinition.Silent then Exit;
  if (SetTextField = nil) or (SetTextField <> Sender) then FValue := Sender.AsString;
  SetTextField := nil;
end;

function TQBEField.IsNull: Boolean;
var F: TField;
begin
  F := QBEDefinition.DataSet.FieldByName(FFieldName);
  if QBEDefinition.AllowOperators and (not QBEDefinition.DataSet.FieldInfoList.ForField(F).IsWide) then
    Result := (Value = '')
  else begin
    if F <> nil then Result := F.IsNull else Result := False;
  end;
end;

// QBEDefinition Object

constructor TQBEDefinition.Create(AOwner: TComponent);
begin
  inherited Create;
  QBEFields := TCollection.Create(TQBEField);
  Owner := AOwner;
end;

destructor TQBEDefinition.Destroy;
begin
  QBEFields.Free;
end;

procedure TQBEDefinition.AssignTo(Dest: TPersistent);
var i: Integer;
    Field: TQBEField;
begin
  if Dest is TQBEDefinition then
  begin
    TQBEDefinition(Dest).SaveQBEValues      := SaveQBEValues;
    TQBEDefinition(Dest).AllowFileWildCards := AllowFileWildCards;
    TQBEDefinition(Dest).AllowOperators     := AllowOperators;
    TQBEDefinition(Dest).QBEFontColor       := QBEFontColor;
    TQBEDefinition(Dest).QBEBackgroundColor := QBEBackgroundColor;
    TQBEDefinition(Dest).QBEFields.Clear;
    for i := 0 to FieldCount - 1 do
    begin
      Field := TQBEField.Create(TQBEDefinition(Dest).QBEFields);
      Field.QBEDefinition := TQBEDefinition(Dest);
      Field.FFieldName := Fields[i].FieldName;
      Field.FQueryable := Fields[i].Queryable;
      Field.FAutoPartialMatch := Fields[i].AutoPartialMatch;
      Field.FCaseInsensitive := Fields[i].CaseInsensitive;
      Field.FIgnoreTime := Fields[i].IgnoreTime;
      Field.FAutoContains := Fields[i].AutoContains;
      Field.FAutoSoundex := Fields[i].AutoSoundex;
      Field.FValue := Fields[i].Value;
    end;
    Exit;
  end;
  inherited;
end;

function TQBEDefinition.GetDataSet: TOracleDataSet;
begin
  Result := Owner as TOracleDataSet;
end;

function TQBEDefinition.Update: Boolean;
var i: Integer;
    FieldDef: TFieldDef;
    QBEField: TQBEField;
begin
  Result := False;
  // Mark fields
  for i := 0 to FieldCount - 1 do Fields[i].Valid := False;
  // Add new fields
  for i := 0 to DataSet.FieldDefs.Count - 1 do
  begin
    FieldDef := DataSet.FieldDefs[i];
    QBEField := FieldByName(FieldDef.Name);
    if QBEField = nil then
    begin
      QBEField := TQBEField.Create(QBEFields);
      QBEField.QBEDefinition := Self;
      QBEField.FFieldName := FieldDef.Name;
      QBEField.Queryable := True;
      Result := True;
    end;
    QBEField.Valid := True;
  end;
  // Remove unused fields
  for i := FieldCount - 1 downto 0 do
  begin
    QBEField := Fields[i];
    if not QBEField.Valid then QBEField.Free;
  end;
end;

function TQBEDefinition.GetField(Index: Integer): TQBEField;
begin
  Result := QBEFields.Items[Index] as TQBEField;
end;

function TQBEDefinition.GetFieldCount: Integer;
begin
  Result := QBEFields.Count;
end;

function TQBEDefinition.FieldByName(const AName: string): TQBEField;
var i: Integer;
    QBEField: TQBEField;
begin
  for i := 0 to FieldCount - 1 do
  begin
    QBEField := Fields[i];
    if AnsiStrIComp(PChar(AName), PChar(QBEField.FieldName)) = 0 then
    begin
      Result := QBEField;
      Exit;
    end;
  end;
  Result := nil;
end;

function TQBEDefinition.GetVersion: Integer;
begin
  Result := 4;
end;

procedure TQBEDefinition.ReadBinaryData(Stream: TStream);
var i, Temp, NumItems, Ver: Integer;
begin
  QBEFields.Clear;
  with Stream do
  begin
    ReadBuffer(Ver, SizeOf(Temp));
    if Ver > GetVersion then
      raise Exception.Create('Invalid version of QBEDefinition');
    ReadBuffer(NumItems, SizeOf(NumItems));
    for i := 0 to NumItems - 1 do
    begin
      with TQBEField.Create(QBEFields) do
      begin
        QBEDefinition := Self;
        // Length of name
        ReadBuffer(Temp, SizeOf(Temp));
        // Name
        SetLength(FFieldName, Temp);
        ReadBuffer(PChar(FieldName)^, Temp);
        // properties
        ReadBuffer(FQueryable, SizeOf(Queryable));
        ReadBuffer(FAutoPartialMatch, SizeOf(AutoPartialMatch));
        ReadBuffer(FCaseInsensitive, SizeOf(CaseInsensitive));
        if Ver > 1 then ReadBuffer(FIgnoreTime, SizeOf(IgnoreTime));
        if Ver > 2 then ReadBuffer(FAutoContains, SizeOf(AutoContains));
        if Ver > 3 then ReadBuffer(FAutoSoundex, SizeOf(AutoSoundex));
      end;
    end;
  end;
end;

procedure TQBEDefinition.WriteBinaryData(Stream: TStream);
var i, Temp: Integer;
begin
  // Write Version, Count, {Length of name, name, Queryable, }
  with Stream do
  begin
    Temp := GetVersion;
    WriteBuffer(Temp, SizeOf(Temp));
    Temp := FieldCount;
    WriteBuffer(Temp, SizeOf(Temp));
    for i := 0 to FieldCount - 1 do with Fields[i] do
    begin
      Temp := Length(FieldName);
      WriteBuffer(Temp, SizeOf(Temp));
      WriteBuffer(PChar(FieldName)^, Temp);
      WriteBuffer(FQueryable, SizeOf(Queryable));
      WriteBuffer(AutoPartialMatch, SizeOf(AutoPartialMatch));
      WriteBuffer(CaseInsensitive, SizeOf(CaseInsensitive));
      WriteBuffer(IgnoreTime, SizeOf(IgnoreTime));
      WriteBuffer(AutoContains, SizeOf(AutoContains));
      WriteBuffer(AutoSoundex, SizeOf(AutoSoundex));
    end;
  end;
end;

procedure TQBEDefinition.DefineProperties(Filer: TFiler);
var AQ: TQBEDefinition;
    f: Integer;
    TheSame, DoWrite: Boolean;
    F1, F2: TQBEField;
begin
  inherited DefineProperties(Filer);
  // Is there an ancestor? If so, we must compare the properties
  AQ := Filer.Ancestor as TQBEDefinition;
  if AQ = nil then
    DoWrite := (FieldCount > 0)
  else begin
    // Compare everything: Number of fields, all field properties
    TheSame := (AQ.FieldCount = FieldCount);
    if TheSame then for f := 0 to FieldCount - 1 do
    begin
      F1 := Fields[f];
      F2 := AQ.Fields[f];
      if (F1.FFieldName <> F2.FFieldName) or
         (F1.FQueryable <> F2.FQueryable) or
         (F1.FAutoPartialMatch <> F2.FAutoPartialMatch) or
         (F1.FAutoContains <> F2.FAutoContains) or
         (F1.FAutoSoundex <> F2.FAutoSoundex) or
         (F1.FCaseInsensitive <> F2.FCaseInsensitive) or
         (F1.FIgnoreTime <> F2.FIgnoreTime) then TheSame := False;
    end;
    DoWrite := not TheSame;
  end;
  Filer.DefineBinaryProperty('QBEFieldDefs', ReadBinaryData, WriteBinaryData, DoWrite);
end;

// TRecordDataList object

constructor TRecordDataList.Create(ADataSet: TOracleDataSet);
begin
  inherited Create;
  Blocks := TList.Create;
  DataSet := ADataSet;
  RecordList := TList.Create;
  FilteredList := TList.Create;
  Unidirectional := True;
end;

destructor TRecordDataList.Destroy;
begin
  FreeRecords;
  RecordList.Free;
  Blocks.Free;
  FilteredList.Free;
  inherited;
end;

// TList emulation functions

function TRecordDataList.Count: Integer;
begin
  Result := RecordList.Count + BlockOffset;
end;

function TRecordDataList.Add(Item: Pointer): Integer;
begin
  Result := RecordList.Add(Item) + BlockOffset;
end;

procedure TRecordDataList.Insert(Index: Integer; Item: Pointer);
begin
  RecordList.Insert(Index - BlockOffset, Item);
end;

procedure TRecordDataList.Delete(Index: Integer);
begin
  RecordList.Delete(Index - BlockOffset);
end;

function TRecordDataList.GetItem(Index: Integer): Pointer;
begin
  if Index - BlockOffset < 0 then
    raise Exception.Create(DataSet.Name + ': Record ' + IntToStr(Index) + ' not accessible in Unidirectional mode');
  Result := RecordList[Index - BlockOffset];
end;

function TRecordDataList.IndexOf(Item: Pointer): Integer;
begin
  Result := RecordList.IndexOf(Item);
  if Result >= 0 then Inc(Result, BlockOffset);
end;

// Allocate a new block of records
function TRecordDataList.AllocateBlock: PRecordBlock;
var MaxRecords, Size: Integer;
begin
  if Unidirectional and (Blocks.Count > 0) then
  begin
    MaxRecords := PRecordBlock(Blocks[0]).MaxRecords;
    FreeRecords;
    Inc(BlockOffset, MaxRecords);
  end else begin
    BlockOffset := 0;
    // Determine the size of the new block
    MaxRecords := Count div 2;
    if MaxRecords < DataSet.ReadBuffer + 1 then
      MaxRecords := DataSet.ReadBuffer + 1;
  end;
  // Calculate the actual number of bytes for the block
  Size := (MaxRecords * DataSet.FRecBufSize) + (SizeOf(TRecordBlock) - SizeOf(TByteArray));
  // Allocate it
  GetMem(Result, Size);
  // Initialize fields
  FillChar(Result^, Size, #0);
  Result.MaxRecords := MaxRecords;
  Result.RecordCount := 0;
  Result.Size := Size;
  // Add it to the list
  Blocks.Add(Result);
end;

// Deallocate a block of records
procedure TRecordDataList.FreeBlock(ABlock: Integer);
var Block: PRecordBlock;
begin
  Block := Blocks[ABlock];
  FreeMem(Block, Block.Size);
end;

// Deallocate all record blocks
procedure TRecordDataList.FreeBlocks;
var i: Integer;
begin
  for i := Blocks.Count - 1 downto 0 do FreeBlock(i);
  Blocks.Clear;
end;

// Return a record by index in a block
function TRecordDataList.BlockRecord(ABlock: PRecordBlock; Index: Integer): PRecordData;
begin
  Result := @ABlock.Bytes[Index * DataSet.FRecBufSize];
end;

// Get a pointer to the first free record
function TRecordDataList.FirstFreeRecord: PRecordData;
var Block: PRecordBlock;
begin
  Result := nil;
  // Find a place for this new record in the last block
  if (Blocks.Count > 0) then
  begin
    Block := Blocks[Blocks.Count - 1];
    if Block.RecordCount < Block.MaxRecords then
    begin
      Result := BlockRecord(Block, Block.RecordCount);
    end;
  end;
  // No place, allocate a new block and use the first record
  if Result = nil then
  begin
    Block := AllocateBlock;
    Result := BlockRecord(Block, 0);
    Block.RecordCount := 0;
  end;
end;

// Get a pointer to a new record
function TRecordDataList.NewRecord(Index: Integer; AddToFiltered: Boolean): PRecordData;
var RealIndex: Integer;
begin
  // Get the first free record
  Result := FirstFreeRecord;
  // Reserve it
  Inc(PRecordBlock(Blocks[Blocks.Count - 1]).RecordCount);
  // Set the master bookmark
  Result^.MasterBookmark := DataSet.GetMasterBookmark;
  // Add the record index to filtered list
  if not AddToFiltered then
    RealIndex := Index
  else begin
    RealIndex := DataIndex(Index);
    if (Index < 0) or (Index >= Count) then
      FilteredList.Add(Result)
    else
      FilteredList.Insert(Index, Result)
  end;
  // Add the result to the list at the specified index
  if (RealIndex < 0) or (RealIndex >= Count) then
    Add(Result)
  else
    Insert(RealIndex, Result)
end;

// Delete a record
procedure TRecordDataList.DeleteRecord(Index: Integer; DeleteFromFiltered: Boolean);
var DIndex, FIndex: Integer;
begin
  if not DeleteFromFiltered then
    DIndex := Index
  else begin
    if not DataSet.UseFiltered then
    begin
      DIndex := Index;
      FIndex := FilteredIndex(DIndex);
    end else begin
      DIndex := DataIndex(Index);
      FIndex := Index;
    end;
    if FIndex >= 0 then FilteredList.Delete(FIndex);
  end;
  FreeBlobs(DataAt(DIndex));
  DataSet.FreeRowId(DataAt(DIndex));
  Delete(DIndex);
end;

// Free all records
procedure TRecordDataList.FreeRecords;
var i: Integer;
begin
  if DataSet.HasBlobs then
    for i := Count - 1 downto BlockOffset do FreeBlobs(DataAt(i));
  for i := Count - 1 downto BlockOffset do DataSet.FreeRowId(DataAt(i));
  RecordList.Clear;
  FreeBlocks;
  FilteredList.Clear;
end;

// Return the record at a given index
function TRecordDataList.RecordAt(Index: Integer): PRecordData;
begin
  try
    if DataSet.UseFiltered then
      Result := PRecordData(FilteredList[Index])
    else
      Result := PRecordData(Self[Index]);
  except
    on e:Exception do
    begin
      ShowMessage(E.Message);
      raise;
    end;
  end;
end;

// Return the record at a given index, regardless of Filter
function TRecordDataList.DataAt(Index: Integer): PRecordData;
begin
  Result := PRecordData(Self[Index]);
end;

// Return the Data Index of the filtered record
function TRecordDataList.DataIndex(FilteredIndex: Integer): Integer;
begin
  if (not DataSet.UseFiltered) or (FilteredIndex < 0) then
    Result := FilteredIndex
  else begin
    if FilteredIndex >= FilteredList.Count then
      Result := Count
    else
      Result := IndexOf(FilteredList[FilteredIndex]);
  end;
end;

// Return the Filtered Index of the data record
function TRecordDataList.FilteredIndex(DataIndex: Integer): Integer;
begin
  Result := FilteredList.IndexOf(Items[DataIndex]);
end;

// Add a record-pointer to the filtered list
procedure TRecordDataList.AddToFilteredList(Data: PRecordData; AtEnd: Boolean);
var DI, FI: Integer;
begin
  if FilteredList.IndexOf(Data) > 0 then
  begin
    raise Exception('BUG: Duplicate record in filtered list (' +
      IntToStr(FilteredList.IndexOf(Data)) + ')');
  end;
  if not AtEnd then
  begin
    DI := IndexOf(Data);
    while DI < Count do
    begin
      FI := FilteredList.IndexOf(Items[DI]);
      if FI >= 0 then
      begin
        FilteredList.Insert(FI, Data);
        Exit;
      end;
      Inc(DI);
    end;
  end;
  FilteredList.Add(Data)
end;

function TRecordDataList.RecordCount: Integer;
begin
  if DataSet.UseFiltered then
    Result := FilteredList.Count
  else
    Result := Count;
end;

// Copy the data of a source record to a destination record
procedure TRecordDataList.CopyData(Source, Dest: PRecordData);
begin
  System.Move(Source^, Dest^, DataSet.FRecBufSize);
end;

// Free the Blob data of a record
procedure TRecordDataList.FreeBlobs(ARecord: PRecordData);
begin
  if ARecord.BlobList <> nil then
  begin
    ARecord.BlobList.Free;
    ARecord.BlobList := nil;
  end;
end;

// SequenceField Object

constructor TSequenceField.Create(ADataSet: TOracleDataSet);
begin
  inherited Create;
  FField := '';
  FSequence := '';
  DataSet := ADataSet;
end;

procedure TSequenceField.AssignTo(Dest: TPersistent);
begin
  if Dest is TSequenceField then
  begin
    TSequenceField(Dest).Field       := Field;
    TSequenceField(Dest).Sequence    := Sequence;
    TSequenceField(Dest).ApplyMoment := ApplyMoment;
    Exit;
  end;
  inherited;
end;

function TSequenceField.IsComplete: Boolean;
begin
  Result := (Sequence <> '') and (Field <> '');
end;

function TSequenceField.ValueName: string;
begin
  if IsComplete then
    Result := Sequence + ' > ' + Field
  else
    Result := '';
end;

procedure TSequenceField.Apply;
begin
  if IsComplete and VarIsNull(DataSet.FieldByName(Field).Value) then
  begin
    DataSet.OpenDMLQuery(False);
    with DataSet.DMLQuery do
    begin
      Clear;
      SQL.Text := 'select ' + Sequence + '.nextval from dual';
      Execute;
      DataSet.FieldByName(Self.Field).Value := Field(0);
    end;
    DataSet.CloseDMLQuery;
  end;
end;

function TSequenceField.UsesField(const AField: string): Boolean;
begin
  Result := (IsComplete and (AnsiUpperCase(AField) = AnsiUpperCase(Field)));
end;

function TSequenceField.ApplyFieldOnServer(const AField: string): Boolean;
begin
  Result := (ApplyMoment = amOnServer) and UsesField(AField);
end;

// TOracleDictionary object

constructor TOracleDictionary.Create(ADataSet: TOracleDataSet);
begin
  inherited Create;
  DataSet := ADataSet;
  FEnforceConstraints  := False;
  FDisabledConstraints := TDisabledConstraints.Create;
  FUseMessageTable := False;
  FDefaultValues  := False;
  FDynamicDefaults := False;
  FFieldKinds := False;
  FRangeValues := False;
  FDisplayFormats := False;
  FRequiredFields := True;
  FCharacterSize := False;
end;

destructor TOracleDictionary.Destroy;
begin
  FDisabledConstraints.Free;
  inherited;
end;

procedure TOracleDictionary.AssignTo(Dest: TPersistent);
begin
  if Dest is TOracleDictionary then
  begin
    TOracleDictionary(Dest).DefaultValues       := DefaultValues;
    TOracleDictionary(Dest).DisabledConstraints := DisabledConstraints;
    TOracleDictionary(Dest).DisplayFormats      := DisplayFormats;
    TOracleDictionary(Dest).DynamicDefaults     := DynamicDefaults;
    TOracleDictionary(Dest).EnforceConstraints  := EnforceConstraints;
    TOracleDictionary(Dest).FieldKinds          := FieldKinds;
    TOracleDictionary(Dest).RangeValues         := RangeValues;
    TOracleDictionary(Dest).UseMessageTable     := UseMessageTable;
    TOracleDictionary(Dest).RequiredFields      := RequiredFields;
    TOracleDictionary(Dest).CharacterSize       := CharacterSize;
    Exit;
  end;
  inherited;
end;

procedure TOracleDictionary.SetDisabledConstraints(Value: TDisabledConstraints);
begin
  FDisabledConstraints.BeginUpdate;
  try
    FDisabledConstraints.Assign(Value);
  finally
    FDisabledConstraints.EndUpdate;
  end;
end;

procedure TOracleDictionary.SetDisplayFormats(Value: Boolean);
begin
  if Value = FDisplayFormats then Exit;
  FDisplayFormats := Value;
  DataSet.Reactivate;
end;

procedure TOracleDictionary.SetRequiredFields(Value: Boolean);
begin
  if Value = FRequiredFields then Exit;
  FRequiredFields := Value;
  DataSet.DataEvent(dePropertyChange, 0);
end;

procedure TOracleDictionary.SetCharacterSize(Value: Boolean);
begin
  if Value = FCharacterSize then Exit;
  FCharacterSize := Value;
  DataSet.DataEvent(dePropertyChange, 0);
end;

// TBlobList
function TBlobList.BlobByField(Field: TField): TCollectionItem;
var i: Integer;
    OracleBlob: TOracleBlob;
begin
  for i := 0 to Count - 1 do
  begin
    OracleBlob := TOracleBlob(Items[i]);
    if OracleBlob.Field = Field then
    begin
      Result := OracleBlob;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TBlobList.Copy(Source: TBlobList);
var i: Integer;
    SourceBlob: TOracleBlob;
    DestBlob: TOracleBlob;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    SourceBlob := Source.Items[i] as TOracleBlob;
    DestBlob   := TOracleBlob.Create(Self);
    if SourceBlob.BlobSize = 0 then
      DestBlob.BlobPointer := nil
    else begin
      GetMem(DestBlob.BlobPointer, SourceBlob.BlobSize);
      Move(SourceBlob.BlobPointer^, DestBlob.BlobPointer^, SourceBlob.BlobSize);
    end;
    DestBlob.BlobSize := SourceBlob.BlobSize;
    DestBlob.Field := SourceBlob.Field;
  end;
end;

// TOracleBlob

destructor TOracleBlob.Destroy;
begin
  FreeBlob;
  inherited;
end;

procedure TOracleBlob.FreeBlob;
begin
  if (BlobPointer <> nil) and (BlobSize > 0) then
    FreeMem(BlobPointer, BlobSize);
  Decompress;
  BlobPointer := nil;
  BlobSize := 0;
end;

procedure TOracleBlob.Decompress;
begin
  if Compressed and (CompressedPointer <> nil) and (CompressedSize > 0) then
    FreeMem(CompressedPointer, CompressedSize);
  Compressed := False;
  CompressedSize := 0;
  CompressedPointer := nil;
end;

procedure TOracleBlob.Compress;
var ds: TOracleDataSet;
begin
  Decompress;
  ds := Field.DataSet as TOracleDataSet;
  CompressedSize := BlobSize;
  CompressedPointer := BlobPointer;
  if ds.CompressBLOBs then
  begin
    Compressed := ds.DoBLOBCompress(Field, Pointer(CompressedPointer), CompressedSize);
  end;
end;

// TOracleBlobStream
// This stream is used to communicate between the Blob fields and the
// underlying record collection
constructor TOracleBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TOracleDataSet;
  if not FDataSet.GetActiveRecBuf(FBuffer) then Exit;
  if (FField.Modified or (Mode <> bmRead)) and not (FDataSet.State in [dsOldValue, dsNewValue]) then
  begin
    FOracleBlob := FDataSet.WorkBlobs.BlobByField(FField) as TOracleBlob;
  end else begin
    if FBuffer^.BlobList = nil then
      FOracleBlob := nil
    else
      FOracleBlob := FBuffer^.BlobList.BlobByField(FField) as TOracleBlob;
  end;
  if not FField.Modified then
  begin
    if Mode <> bmRead then
      if not (FDataSet.State in [dsEdit, dsInsert, dsNewValue]) then DatabaseError(SNotEditing);
  end;
  FOpened := True;
  if Mode = bmWrite then Truncate;
end;

destructor TOracleBlobStream.Destroy;
begin
  if FOpened then
  begin
    if FModified then FField.Modified := True;
  end;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    ApplicationHandleException(Self);
  end;
end;

function TOracleBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then
    begin
      Move(FOracleBlob.BlobPointer^[FPosition], Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

function TOracleBlobStream.Write(const Buffer; Count: Longint): Longint;
var Temp: Pointer;
    NewSize: Integer;
begin
  Result := 0;
  if FOpened then
  begin
    NewSize := FPosition + Count;
    if NewSize < FOracleBlob.BlobSize then NewSize := FOracleBlob.BlobSize;
    if NewSize > FOracleBlob.BlobSize then
    begin
      GetMem(Temp, NewSize);
      if (FOracleBlob.BlobPointer <> nil) and (FOracleBlob.BlobSize > 0) then
      begin
        Move(FOracleBlob.BlobPointer^, Temp^, FOracleBlob.BlobSize);
        FreeMem(FOracleBlob.BlobPointer, FOracleBlob.BlobSize);
      end;
      FOracleBlob.BlobPointer := Temp;
    end;
    if Count > 0 then Move(Buffer, FOracleBlob.BlobPointer^[FPosition], Count);
    Inc(FPosition, Count);
    if FPosition > FOracleBlob.BlobSize then FOracleBlob.BlobSize := FPosition;
    Result := Count;
    if FDataSet.State in [dsEdit, dsInsert] then FModified := True;
  end;
end;

function TOracleBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TOracleBlobStream.Truncate;
begin
  if FOpened then
  begin
    FPosition := 0;
    FOracleBlob.FreeBlob;
    if FDataSet.State in [dsEdit, dsInsert] then FModified := True;
  end;
end;

function TOracleBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened and (FOracleBlob <> nil) then Result := FOracleBlob.BlobSize;
end;

// TChangeLogItem

constructor TChangeLogItem.CreateForDataSet(ADataSet: TOracleDataSet);
begin
  inherited Create(ADataSet.ChangeLog);
  DataSet     := ADataSet;
  Action      := ' ';
  Bookmark    := -1;
  RecordIndex := -1;
  Applied     := False;
  BeforeData  := nil;
  Visible     := True;
end;

destructor TChangeLogItem.Destroy;
begin
  FreeBeforeData;
  FreeAfterData;
  inherited;
end;

function TChangeLogItem.GetBookmark: Integer;
begin
  Result := -1;
  if BeforeData <> nil then
    Result := BeforeData.Bookmark
  else if AfterData <> nil then
    Result := AfterData.Bookmark;
end;

procedure TChangeLogItem.SetBookmark(Value: Integer);
begin
  if BeforeData <> nil then BeforeData.Bookmark := Value;
  if AfterData  <> nil then AfterData.Bookmark  := Value;
end;

procedure TChangeLogItem.FreeBeforeData;
begin
  if BeforeData <> nil then
  begin
    if BeforeData^.BlobList <> nil then BeforeData^.BlobList.Free;
    DataSet.FreeRowId(BeforeData);
    FreeMem(BeforeData, BeforeData^.ArraySize + RecInfoSize);
    BeforeData := nil;
  end;
end;

procedure TChangeLogItem.SetBeforeData(ARecordData: PRecordData);
begin
  FreeBeforeData;
  if ARecordData = nil then Exit;
  BeforeData := PRecordData(DataSet.AllocRecordBuffer);
  Move(ARecordData^, BeforeData^, BeforeData^.ArraySize + RecInfoSize);
  BeforeData.PRowid := nil;
  DataSet.SetRowId(BeforeData, PChar(DataSet.GetRowId(ARecordData)));
  Bookmark := ARecordData.Bookmark;
  if ARecordData^.BlobList <> nil then
  begin
    BeforeData^.BlobList := TBlobList.Create(TOracleBlob);
    BeforeData^.BlobList.Copy(ARecordData^.BlobList);
  end;
end;

procedure TChangeLogItem.FreeAfterData;
begin
  if AfterData <> nil then
  begin
    if AfterData^.BlobList <> nil then AfterData^.BlobList.Free;
    DataSet.FreeRowId(AfterData);
    FreeMem(AfterData, AfterData^.ArraySize + RecInfoSize);
    AfterData := nil;
  end;
end;

procedure TChangeLogItem.SetAfterData(ARecordData: PRecordData);
var i: Integer;
    AfterBlob, WorkBlob: TOracleBlob;
begin
  FreeAfterData;
  if ARecordData = nil then Exit;
  AfterData := PRecordData(DataSet.AllocRecordBuffer);
  Move(ARecordData^, AfterData^, AfterData.ArraySize + RecInfoSize);
  AfterData.PRowid := nil;
  DataSet.SetRowId(AfterData, PChar(DataSet.GetRowId(ARecordData)));
  AfterData.BlobList := nil;
  Bookmark := ARecordData.Bookmark;
  if DataSet.WorkBlobs <> nil then
  begin
    DataSet.CreateBlobList(AfterData.BlobList);
    if AfterData.BlobList <> nil then
    begin
      for i := 0 to AfterData.BlobList.Count - 1 do
      begin
        AfterBlob := AfterData.BlobList.Items[i] as TOracleBlob;
        WorkBlob  := DataSet.WorkBlobs.Items[i] as TOracleBlob;
        if not WorkBlob.Field.Modified then
        begin
          if ARecordData.BlobList <> nil then
            WorkBlob := ARecordData.BlobList.Items[i] as TOracleBlob
          else
            WorkBlob := nil;
        end;
        if (WorkBlob <> nil) and (WorkBlob.BlobPointer <> nil) and (WorkBlob.BlobSize > 0) then
        begin
          GetMem(AfterBlob.BlobPointer, WorkBlob.BlobSize);
          Move(WorkBlob.BlobPointer^, AfterBlob.BlobPointer^, WorkBlob.BlobSize);
          AfterBlob.BlobSize := WorkBlob.BlobSize;
        end;
      end;
    end;
  end;
end;

procedure TChangeLogItem.GetAfterData(ARecordData: PRecordData);
var OldBookmark: Integer;
begin
  OldBookmark := ARecordData.Bookmark;
  Move(AfterData^, ARecordData^, AFterData^.ArraySize + RecInfoSize);
  ARecordData.PRowid := nil;
  DataSet.SetRowId(ARecordData, PChar(DataSet.GetRowId(AfterData)));
  ARecordData.Bookmark := OldBookmark;
  if AFterData^.BlobList <> nil then
  begin
    ARecordData^.BlobList := TBlobList.Create(TOracleBlob);
    ARecordData^.BlobList.Copy(AfterData^.BlobList);
  end;
end;

procedure TChangeLogItem.Cancel;
var R: PRecordData;
    DI: Integer;
begin
  DataSet.JustData := True;
  try
    if not Visible then Exit;
    case Action of
      'U': begin
             DI := DataSet.FindBookmark(Bookmark);
             if DI >= 0 then
             begin
               R := DataSet.Records.RecordAt(DI);
               DataSet.Records.FreeBlobs(R);
               DataSet.FreeRowId(R);
               DataSet.Records.CopyData(BeforeData, R);
               BeforeData.BlobList := nil;
               BeforeData.PRowid := nil;
               FreeBeforeData;
             end;
           end;
      'I': begin
             DI := DataSet.FindBookmark(Bookmark);
             if DI >= 0 then DataSet.Records.DeleteRecord(DI, True);
           end;
      'D': begin
             if RecordIndex < 0 then
               R := DataSet.Records.NewRecord(0, False)
             else
               R := DataSet.Records.NewRecord(RecordIndex, False);
             DataSet.Records.CopyData(BeforeData, R);
             BeforeData.BlobList := nil;
             BeforeData.PRowid := nil;
             FreeBeforeData;
             if DataSet.Filtered and DataSet.ApplyFilterTo(R) then
             begin
               DataSet.Records.AddToFilteredList(R, False);
             end;
           end;
    end;
  finally
    DataSet.JustData := False;
  end;
end;

// Does this change log belong to the current data set
function TChangeLogItem.InDataSet: Boolean;
var TheData: PRecordData;
begin
  if Action = 'I' then TheData := AfterData else TheData := BeforeData;
  if not DataSet.IsLooseDetail then
    Result := DataSet.CompareMasterFieldValues(TheData)
  else
    Result := (TheData.MasterBookmark = DataSet.GetMasterBookmark);
end;

// TOracleFieldInfo

constructor TOracleFieldInfo.Create(ACollection: TCollection);
begin
  inherited;
  FChildList := TList.Create;
end;

destructor TOracleFieldInfo.Destroy;
begin
  FChildList.Free;
  inherited;
end;

// Set the full name and avoid duplicates
procedure TOracleFieldInfo.SetFullname(const Value: string);
var Suffix: Integer;
    NewName: string;
begin
  Suffix := 0;
  FFullName := '';
  repeat
    NewName := Value;
    if Suffix > 0 then NewName := NewName + '_' + IntToStr(Suffix);
    Inc(Suffix);
  until TOracleFieldInfoList(Collection).FieldInfoByname(NewName) = nil;
  FFullName := NewName;
end;

function TOracleFieldInfo.RealDataSize: Integer;
begin
  Result := Field.DataSize;
  if Field.DataType = ftWideString then
  begin
    if IsUTF16 then
      Result := Field.Size * 2 + 2
    else
      Result := Field.Size + 1;
  end;
end;

function TOracleFieldInfo.GetChild(Index: Integer): TOracleFieldInfo;
begin
  Result := TOracleFieldInfo(FChildList[Index]);
end;

function TOracleFieldInfo.GetChildCount: Integer;
begin
  Result := FChildList.Count;
end;

function TOracleFieldInfo.DataSet: TOracleDataSet;
begin
  Result := TOracleFieldInfoList(Collection).DataSet;
end;

function TOracleFieldInfo.FieldWasNull: Boolean;
var c: Integer;
begin
  if DataType = otObject then
  begin
    Result := True;
    for c := 0 to ChildCount - 1 do
      if not Children[c].FieldWasNull then Result := False;
  end else begin
    Result := (Field = nil) or
              VarIsNull(Field.OldValue) or
              ((Field.DataType in [ftString, ftMemo, ftWideString]) and (DataSet.TrimStringV(Field.OldValue) = ''));
  end;
end;

function TOracleFieldInfo.FieldIsNull: Boolean;
var c: Integer;
begin
  if DataType = otObject then
  begin
    Result := True;
    for c := 0 to ChildCount - 1 do
      if not Children[c].FieldIsNull then Result := False;
  end else begin
    Result := (Field = nil) or
              VarIsNull(Field.Value) or
              ((Field.DataType in [ftString, ftMemo {$IFDEF CompilerVersion5}, ftOraClob {$ENDIF}, ftWideString]) and
               (DataSet.TrimStringV(Field.Value) = ''));
  end;
end;

function TOracleFieldInfo.FieldChanged: Boolean;
var c: Integer;
begin
  if DataType = otObject then
  begin
    Result := False;
    for c := 0 to ChildCount - 1 do
      if Children[c].FieldChanged then Result := True;
  end else begin
    Result := (Field <> nil) and DataSet.FieldChanged(Field);
  end;
end;

function TOracleFieldInfo.FieldHasDefault: Boolean;
begin
  Result := (FieldDefaultExpression <> '');
end;

function TOracleFieldInfo.FieldDefaultExpression: string;
var c: Integer;
    HasDefaults: Boolean;
    s: string;
begin
  Result := '';
  if DataType = otObject then
  begin
    HasDefaults := False;
    Result := TypeName + '(';
    for c := 0 to ChildCount - 1 do
    begin
      s := Children[c].FieldDefaultExpression;
      if s <> '' then HasDefaults := True else s := Children[c].FieldNullExpression(False);
      if c > 0 then Result := Result + ', ';
      Result := Result + s;
    end;
    if HasDefaults then Result := Result + ')' else Result := '';
  end;
  if Field <> nil then
  begin
    Result := Field.DefaultExpression;
    if (Result = '') and DataSet.SequenceField.ApplyFieldOnServer(Field.FieldName) then
      Result := DataSet.SequenceField.Sequence + '.nextval';
  end;
  if (Result = '') and
     ((DataSet.Session.Preferences.NullLOBIsEmpty    and (DataType in [otCLOB, otBLOB])) or
      (DataSet.Session.Preferences.NullObjectIsEmpty and (DataType = otObject))) then
  begin
    Result := FieldNullExpression(False);
  end;
end;

function TOracleFieldInfo.FieldNullExpression(WithDefaults: Boolean): string;
var c: Integer;
begin
  Result := '';
  if WithDefaults then Result := FieldDefaultExpression;
  if DataSet.Session.Preferences.NullLOBIsEmpty and
    (DataType in [otCLOB, otBLOB]) and
    (Result = '') and
    (not IsXMLCLOB) then
  begin
    if DataType = otCLOB then Result := 'empty_clob()';
    if DataType = otBLOB then Result := 'empty_blob()';
  end;
  if DataSet.Session.Preferences.NullObjectIsEmpty and
    (DataType = otObject) and
    (Result = '') and
    (not IsXMLCLOB) then
  begin
    Result := TypeName + '(';
    for c := 0 to ChildCount - 1 do
    begin
      if c > 0 then Result := Result + ', ';
      Result := Result + Children[c].FieldNullExpression(False);
    end;
    Result := Result + ')';
  end;
  if Result = '' then Result := 'null';
end;

// Return the value for this field from the current record of a query
function TOracleFieldInfo.QueryValue(Q: TOracleQuery): Variant;
var i: Integer;
begin
  Result := Unassigned;
  i := QueryValueIndex(Q);
  if i >= 0 then
  begin
    if InfoType = itField then
      Result := Q.Field(i)
    else if InfoType = itAttr then
      Result := Q.ObjField(i).Attrs[AttrIndex].Value;
    if (not IsWide) and (VarType(Result) = varOleStr) then
      Result := Oracle.UTF8Encode(Result);
  end;
end;

function TOracleFieldInfo.QueryValueIndex(Q: TOracleQuery): Integer;
var Attr: TObjectAttribute;
begin
  Result := Q.FieldIndex(ColumnName);
  if Result >= 0 then
  begin
    if (InfoType = itField) and (Q.FieldType(Result) <> DataType) then Result := -1;
    if (InfoType = itAttr) then
    begin
      if (Q.FieldType(Result) <> otObject) then
        Result := -1
      else begin
        Attr := Q.ObjField(Result).AttrByName(AttrName);
        if (Attr = nil) or (Attr.DataType <> DataType) then Result := -1;
      end;
    end;
  end;
end;

function TOracleFieldInfo.IsPartOf(Owner: TOracleFieldInfo): Boolean;
var P: TOracleFieldInfo;
begin
  P := Parent;
  while P <> nil do
  begin
    if Owner = P then
    begin
      Result := True;
      Exit;
    end;
    P := P.Parent;
  end;
  Result := False;
end;

// TOracleFieldInfoList

function TOracleFieldInfoList.GetFieldInfo(Index: Integer): TOracleFieldInfo;
begin
  Result := Items[Index] as TOracleFieldInfo;
end;

function TOracleFieldInfoList.ForField(AField: TField): TOracleFieldInfo;
var f: Integer;
begin
  Result := nil;
  for f := 0 to Count - 1 do
  begin
    if AField = Self[f].Field then
    begin
      Result := Self[f];
      Break;
    end;
  end;
  if Result = nil then Result := FieldInfoByName(AField.FieldName);
end;

function TOracleFieldInfoList.FieldInfoByName(const AFieldName: string): TOracleFieldInfo;
var f: Integer;
begin
  Result := nil;
  for f := 0 to Count - 1 do
  begin
    if AnsiCompareText(AFieldName, Self[f].FullName) = 0 then
    begin
      Result := Self[f];
      Break;
    end;
  end;
end;

// TOracleDataset

constructor TOracleDataset.Create(AOwner: TComponent);
begin
  inherited;
  DMLQuery := nil;
  CursorQuery := nil;
  ActiveOnLoading := False;
  FQuery := CreateInternalQuery;
  FQuery.Name := 'MainQuery';
  FQuery.ParentMonID := MonID;
  FDetails := TList.Create;
  FSequenceField := TSequenceField.Create(Self);
  FSequenceField.Sequence := '';
  FSequenceField.Field := '';
  FSequenceField.ApplyMoment := amOnPost;
  FOracleDictionary := TOracleDictionary.Create(Self);
  FQBEDefinition := TQBEDefinition.Create(Self);
  FQBEDefinition.SaveQBEValues := True;
  FQBEDefinition.AllowFileWildCards := True;
  FQBEDefinition.AllowOperators := False;
  FQBEDefinition.FQBEFontColor := clNone;
  FQBEDefinition.FQBEBackgroundColor := clNone;
  FQBEDefinition.AllowOperators := False;
  QBEFontColorList := TList.Create;
  QBEBkgColorList := TList.Create;
  QBEMaxLengthControlList := TList.Create;
  QBEMaxLengthValueList := TList.Create;
  DontEnterQBE := False;
  FieldInfoList := TOracleFieldInfoList.Create(TOracleFieldInfo);
  FieldInfoList.DataSet := Self;
  FLockingMode := lmCheckImmediate;
  FQueryAllRecords := True;
  FCountAllRecords := False;
  FRefreshOptions := [];
  FUpdatingTable := '';
  FCommitOnPost := True;
  IdColumns := TStringList.Create;
  ChangeLog := TCollection.Create(TChangeLogItem);
  Applying := False;
  JustData := False;
  FQBEMode := False;
  NoFieldValidation := False;
  FilterExpr := nil;
  Counted := False;
  RowsAdded := 0;
  RowCount := 0;
  ForceMasterSynchronization := False;
  // Nested dataset MIDAS support
  {$IFDEF CompilerVersion4}
  FMasterSource := TDataSource.Create(Self);
  FMasterSource.DataSet := Self;
  FMasterLink := TMasterDataLink.Create(Self);
  {$ENDIF}
  if Assigned(SetDefaults) then SetDefaults(Self);
end;

destructor TOracleDataset.Destroy;
var d: Integer;
begin
  try
    Destroying;
    try
      Close;
    except
    end;
    RemoveFromList;
    if FMaster <> nil then FMaster.RemoveDetail(Self);
    for d := FDetails.Count - 1 downto 0 do
      TOracleDataSet(FDetails[d]).Master := nil;
    FQuery.Free;
    FQuery := nil;
    FSequenceField.Free;
    FOracleDictionary.Free;
    FQBEDefinition.Free;
    FDetails.Free;
    IdColumns.Free;
    ChangeLog.Free;
    FieldInfoList.Free;
    QBEFontColorList.Free;
    QBEBkgColorList.Free;
    QBEMaxLengthControlList.Free;
    QBEMaxLengthValueList.Free;
    FCollectionTypes.Free;
    FCollectionNames.Free;
    if FilterExpr <> nil then FilterExpr.Free;
    // Nested dataset MIDAS support
    {$IFDEF CompilerVersion4}
    FMasterSource.Free;
    FMasterLink.Free;
    {$ENDIF}
    {$IFDEF CompilerVersion5}
    if IParams <> nil then FreeAndNil(IParams);
    {$ENDIF}
    SendToMonitor(False, True);
  except
  end;
  inherited;
end;

function TOracleDataSet.CreateInternalQuery: TOracleQuery;
begin
  Result := TOracleQuery.Create(Self);
end;

// Methods & Properties derived from TOracleQuery

procedure TOracleDataSet.ClearVariables;
begin
  FQuery.ClearVariables;
end;

procedure TOracleDataSet.DeleteVariable(AName: string);
begin
  FQuery.DeleteVariable(AName);
  {$IFDEF CompilerVersion5}
  FreeAndNil(IParams);
  {$ENDIF}
end;

procedure TOracleDataSet.DeleteVariables;
begin
  FQuery.DeleteVariables;
  {$IFDEF CompilerVersion5}
  FreeAndNil(IParams);
  {$ENDIF}
end;

procedure TOracleDataSet.DeclareVariable(AName: string; AType: Integer);
begin
  FQuery.DeclareVariable(AName, AType);
  {$IFDEF CompilerVersion5}
  FreeAndNil(IParams);
  {$ENDIF}
end;

procedure TOracleDataSet.DeclareVariableCharSet(AName: string; ACharSetForm, ACharSetID: Integer);
begin
  FQuery.DeclareVariableCharSet(AName, ACharSetForm, ACharSetID);
end;

procedure TOracleDataSet.DeclareAndSet(AName: string; AType: Integer; const AValue: Variant);
begin
  FQuery.DeclareAndSet(AName, AType, AValue);
end;

procedure TOracleDataSet.DimPLSQLTable(AName: string; TableSize, StringSize: Integer);
begin
  FQuery.DimPLSQLTable(AName, TableSize, StringSize);
end;

{$IFDEF Overloading}
procedure TOracleDataSet.SetVariable(Index: Integer; const AValue: Variant);
begin
  FQuery.SetVariable(Index, AValue);
end;

function TOracleDataSet.GetVariable(Index: Integer): Variant;
begin
  Result := FQuery.GetVariable(Index);
end;
{$ENDIF}

procedure TOracleDataSet.SetVariable(AName: string; const AValue: Variant);
begin
  FQuery.SetVariable(AName, AValue);
end;

procedure TOracleDataSet.SetComplexVariable(AName: string; const AValue: TObject);
begin
  FQuery.SetComplexVariable(AName, AValue);
end;

procedure TOracleDataSet.SetLongVariable(AName: string; Buffer: Pointer; Length: Integer);
begin
  FQuery.SetLongVariable(AName, Buffer, Length);
end;

function TOracleDataSet.GetVariable(AName: string): Variant;
begin
  Result := FQuery.GetVariable(AName);
end;

function TOracleDataSet.GetComplexVariable(AName: string): TObject;
begin
  Result := FQuery.GetComplexVariable(AName);
end;

function TOracleDataSet.VariableCount: Integer;
begin
  Result := FQuery.VariableCount;;
end;

function TOracleDataSet.VariableName(Index: Integer): string;
begin
  Result := FQuery.VariableName(Index);
end;

function TOracleDataSet.VariableType(Index: Integer): Integer;
begin
  Result := FQuery.VariableType(Index);
end;

function TOracleDataSet.VariableIndex(AName: string): Integer;
begin
  Result := FQuery.VariableIndex(AName);
end;

// Add a DataSet to the DataSetlist of the session
procedure TOracleDataSet.AddToList;
begin
  if Session <> nil then Session.DataSets.Add(Self);
end;

// Remove a DataSet from the DataSetlist of the session
procedure TOracleDataSet.RemoveFromList;
begin
  if Session <> nil then Session.DataSets.Remove(Self);
end;

function TOracleDataSet.RowId: string;
var b: PRecordData;
    OldState: TDataSetState;
    StateSet: Boolean;
begin
  if ControlsDisabled then
  begin
    OldState := dsInactive;
    StateSet := False;
  end else begin
    OldState := SetTempState(dsOldValue);
    StateSet := True;
  end;
  if not GetActiveRecBuf(b) then
    Result := ''
  else
    Result := GetRowId(b);
  if StateSet then RestoreState(OldState);
end;

function TOracleDataSet.ErrorLine: Integer;
begin
  Result := FQuery.ErrorLine;
end;

function TOracleDataSet.ErrorPosition: Integer;
begin
  Result := FQuery.ErrorPosition;
end;

procedure TOracleDataSet.SetSequenceField(Value: TSequenceField);
begin
  FSequenceField.Assign(Value);
end;

procedure TOracleDataSet.SetOracleDictionary(Value: TOracleDictionary);
begin
  FOracleDictionary.Assign(Value);
end;

procedure TOracleDataSet.SetQBEDefinition(Value: TQBEDefinition);
begin
  FQBEDefinition.Assign(Value);
end;

procedure TOracleDataSet.SetVariables(Value: TVariables);
begin
  FQuery.Variables := Value;
end;

function TOracleDataSet.GetVariables: TVariables;
begin
  Result := FQuery.Variables;
end;

procedure TOracleDataSet.SetStringFieldsOnly(Value: Boolean);
begin
  FQuery.StringFieldsOnly := Value;
  // The FieldDefs may be updated
  DataEvent(dePropertyChange, 0);
  // When the StringFieldsOnly property changes and the dataset is active, it
  // needs to be reactivated to completely reinitialize
  Reactivate;
end;

function TOracleDataSet.GetStringFieldsOnly: Boolean;
begin
  Result := FQuery.StringFieldsOnly;
end;

procedure TOracleDataSet.SetSQL(Value: TStrings);
begin
  FQuery.SQL := Value;
  // Clear the updating table
  FTableName := '';
  // Clear the Collection types and names
  FCollectionTypes.Free;
  FCollectionTypes := nil;
  FCollectionNames.Free;
  FCollectionNames := nil;
  // The FieldDefs may be updated
  DataEvent(dePropertyChange, 0);
  // When the SQL statement changes and the dataset is active, it needs to be
  // reactivated to completely reinitialize
  Reactivate;
end;

function TOracleDataSet.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

// Trim string if session preference TrimStringFields is True
function TOracleDataSet.TrimStringS(const S: string): string;
begin
  if Session.Preferences.TrimStringFields then
  begin
    Result := TrimRight(s)
  end else
    Result := s;
end;

function TOracleDataSet.TrimStringW(const S: widestring): widestring;
begin
  if Session.Preferences.TrimStringFields then
  begin
   Result := TrimRight(s)
  end else
    Result := s;
end;

function TOracleDataSet.TrimStringV(const S: Variant): Variant;
begin
  if Session.Preferences.TrimStringFields then
  begin
    if VarType(s) = varOleStr then
      Result := TrimRight(WideString(s))
    else
      Result := TrimRight(S);
  end else
    Result := s;
end;

procedure TOracleDataSet.InternalSetActive(Value: Boolean);
var aol: Boolean;
begin
  if FDesignActivation and (not (csDesigning in ComponentState)) and
     ((csReading in ComponentState)) then Exit;
  if ((csReading in ComponentState) or (csLoading in ComponentState)) or
     ((Session = nil) and (csDesigning in ComponentState)) then
  begin
    if Value then ActiveOnLoading := True;
  end else begin
    aol := ActiveOnLoading;
    ActiveOnLoading := False;
    if Value <> Active then
    begin
      if Session <> nil then Session.ConnectOnLoading;
      if aol and Value and (Session <> nil) and (not Session.Connected) and
         (not (csDesigning in ComponentState)) then Exit;
      DontEnterQBE := True;
      try
        inherited Active := Value;
      finally
        DontEnterQBE := False;
      end;
      if Value and QBEMode then EnterQBEMode(True);
    end;
  end;
end;

function TOracleDataSet.GetActive: Boolean;
begin
  Result := inherited Active;
end;

function TOracleDataSet.GetDetached: Boolean;
begin
  Result := ((Session = nil) or (not Session.Connected)) and Active;
end;

function TOracleDataSet.GetCompressionFactor: Double;
begin
  if DecompressedBytes = 0 then
    result := 0
  else
    Result := DecompressedBytes / CompressedBytes;
end;

procedure TOracleDataSet.DataSetChange;
begin
  DataEvent(deDataSetChange, 0);
end;

{$IFDEF CompilerVersion5}
{$IFDEF CompilerVersion2006}
procedure TOracleDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
var l: Integer;
    ws: WideString;
    bs: PByteArray;
    s: string;
    FI: TOracleFieldInfo;
    LengthOK: Boolean;
begin
  inherited;
  case Field.DataType of
    ftWideString:
      begin
        FI := FieldInfoList.ForField(Field);
        if (FI <> nil) and not FI.IsUTF16 then
        begin
          if not ToNative then
          begin
            if FI.IsUTF8 then
              ws := Oracle.UTF8Decode(PChar(Source)) + #0
            else
              ws := PCHAR(Source) + #0;
            Move(ws[1], Dest^, Length(ws) * 2);
          end else begin
            SetLength(ws, wstrlen(PWideChar(Source)));
            Move(Source^, ws[1], Length(ws) * 2);
            bs := Dest;
            LengthOK := False;
            repeat
              if FI.IsUTF8 then
                s := Oracle.UTF8Encode(ws)
              else
                s := ws;
              l := Length(s);
              if l > Field.Size then
                SetLength(ws, Length(ws) - 1)
              else
                LengthOK := True;
            until LengthOK or (s = '');
            Move(s[1], Dest^, l);
            bs[l] := 0;
          end;
        end else begin
          l := (wstrlen(PWideChar(Source)) + 1) * 2;
          Move(Source^, Dest^, l)
        end;
      end;
  end;
end;
{$ELSE}
procedure TOracleDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
var l: Integer;
    ws: WideString;
    pws: PWideString;
    bs: PByteArray;
    s: string;
    FI: TOracleFieldInfo;
    LengthOK: Boolean;
begin
  inherited;
  case Field.DataType of
    ftWideString:
      begin
        FI := FieldInfoList.ForField(Field);
        if (FI <> nil) and not FI.IsUTF16 then
        begin
          if not ToNative then
          begin
            if FI.IsUTF8 then
              PWideString(Dest)^ := Oracle.UTF8Decode(PChar(Source))
            else
              PWideString(Dest)^ := PChar(Source);
          end else begin
            pws := PWideString(Source);
            ws := pws^;
            bs := Dest;
            LengthOK := False;
            repeat
              if FI.IsUTF8 then
                s := Oracle.UTF8Encode(ws)
              else
                s := ws;
              l  := Length(s);
              if l > Field.Size then
                SetLength(ws, Length(ws) - 1)
              else
                LengthOK := True;
            until LengthOK or (s = '');
            Move(s[1], Dest^, l);
            bs[l] := 0;
          end;
        end else begin
          if not ToNative then
            PWideString(Dest)^ := PWideChar(Source)
          else begin
            pws := PWideString(Source);
            bs := Dest;
            l  := Length(pws^) * 2;
            Move(pws^[1], Dest^, l);
            bs[l] := 0;
            bs[l + 1] := 0;
          end;
        end;
      end;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TOracleDataSet.SetSession(Value: TOracleSession);
begin
  if Value <> FQuery.Session then
  begin
    if (Value <> nil) or (not Detachable) then Active := False;
    RemoveFromList;
    SendToMonitor(False, True);
    if FQuery.Session <> nil then FMonID := 0;
    FQuery.Session := Value;
    AddToList;
    if Detached then DataSetChange;
  end;
  if ActiveOnLoading then Active := True;
end;

function TOracleDataSet.GetSession: TOracleSession;
begin
  Result := FQuery.Session;
end;

procedure TOracleDataSet.SetReadBuffer(Value: Integer);
begin
  FQuery.ReadBuffer := Value;
end;

function TOracleDataSet.GetReadBuffer: Integer;
begin
  Result := FQuery.ReadBuffer;
end;

procedure TOracleDataSet.SetOptimize(Value: Boolean);
begin
  FQuery.Optimize := Value;
end;

function TOracleDataSet.GetOptimize: Boolean;
begin
  Result := FQuery.Optimize;
end;

procedure TOracleDataSet.SetDebug(Value: Boolean);
begin
  FQuery.Debug := Value;
end;

function TOracleDataSet.GetDebug: Boolean;
begin
  Result := FQuery.Debug;
end;

procedure TOracleDataSet.SetCursor(Value: TCursor);
begin
  FQuery.Cursor := Value;
end;

function TOracleDataSet.GetCursor: TCursor;
begin
  Result := FQuery.Cursor;
end;

procedure TOracleDataSet.SetUpdatingTable(const Value: string);
begin
  FTableName := '';
  FUpdatingTable := Value;
end;

// Blob functions

procedure TOracleDataSet.CloseBlob(Field: TField);
var OracleBlob: TOracleBlob;
begin
  OracleBlob := WorkBlobs.BlobByField(Field) as TOracleBlob;
  if OracleBlob <> nil then OracleBlob.FreeBlob;
end;

function TOracleDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TOracleBlobStream.Create(Field as TBlobField, Mode);
end;

// Master/Detail functions

function TOracleDataSet.GetPKFields(Q: TOracleQuery; const TableOwner, TableName: string; CheckExists: Boolean): TStringList;
var ConstraintName: string;
    Okay: Boolean;
begin
  Result := TStringList.Create;
  Okay := True;
  with Q do
  try
    Clear;
    // Find a primary/unique key
    SQL.Add('select constraint_name from all_constraints');
    SQL.Add('where owner = :owner and table_name = :table_name');
    SQL.Add('and constraint_type in (''P'', ''U'')');
    SQL.Add('order by constraint_type');
    DeclareVariable('owner', otString);
    DeclareVariable('table_name', otString);
    SetVariable('owner', TableOwner);
    SetVariable('table_name', TableName);
    Execute;
    if not EOF then
    begin
      // Now find the columns
      ConstraintName := FieldAsString(0);
      SQL.Clear;
      SQL.Add('select column_name from all_cons_columns');
      SQL.Add('where owner = :owner and table_name = :table_name');
      SQL.Add('and constraint_name = :constraint_name');
      SQL.Add('order by position');
      DeclareVariable('constraint_name', otString);
      SetVariable('constraint_name', ConstraintName);
      Execute;
      // Loop through all constraint columns to add them to the list
      // Each column must be included in the field list
      while not EOF do
      begin
        Result.Add(FieldAsString(0));
        if CheckExists and (FindField(FieldAsString(0)) = nil) then Okay := False;
        Next;
      end;
    end;
  except
    Okay := False;
  end;
  if (not Okay) or (Result.Count = 0) then
  begin
    Result.Free;
    Result := nil;
  end;
end;

// Initialize the master collection by updating an empty collection if it is null
procedure TOracleDataSet.InitMasterCollection;
var MasterTable, WhereClause: string;
    s, ss: string;
    p2, p3, c: Integer;
    Vars: TStringList;
    VarIndex, i: Integer;
    VarName: string;
begin
  // Is this a collection dataset? If not, we're done
  if not IsCollectionDataSet then Exit;
  // Determine the collection object names and types
  GetCollectionInfo;
  if FCollectionTypes = nil then Exit;
  // Determine the Master table (including alias)
  ss := Trim(GetUpdatingTable);
  s := AnsiUpperCase(ss);
  if (s = '') or (s[Length(s)] <> ')') then Exit;
  p2 := Pos('FROM', s);
  p3 := Pos('WHERE', s);
  if (p2 <= 0) or (p3 <= p2) then Exit;
  Inc(p2, 4);
  MasterTable := Trim(Copy(ss, p2, p3 - p2));
  // Determine the Where clause
  Inc(p3, 5);
  WhereClause := Trim(Copy(ss, p3, Length(ss) - p3));
  // Open a query
  OpenDMLQuery(False);
  DMLQuery.Clear;
  // Set the SQL
  DMLQuery.SQL.Add('begin');
  for c := 0 to FCollectionNames.Count - 1 do
  begin
    DMLQuery.SQL.Add('  update ' + MasterTable);
    DMLQuery.SQL.Add('  set ' + FCollectionNames[c] + ' = ' + FCollectionTypes[c]);
    DMLQuery.SQL.Add('  where ' + FCollectionNames[c] + ' is null');
    DMLQuery.SQL.Add('  and (' + WhereClause + ');');
  end;
  DMLQuery.SQL.Add('end;');
  // Copy the variables
  Vars := FindVariables(DMLQuery.SQL.Text, False);
  for i := 0 to Vars.Count - 1 do
  begin
    VarName  := Vars[i];
    VarIndex := VariableIndex(VarName);
    if VarIndex >= 0 then
    begin
      DMLQuery.DeclareVariable(VarName, VariableType(VarIndex));
      DMLQuery.SetVariable(VarName, GetVariable(VarName));
    end;
  end;
  Vars.Free;
  // Execute it!
  DMLQuery.Execute;
end;

// Determine Master fields for a collection (varray or nested table)
function TOracleDataSet.GetCollectionMasterFields(Q: TOracleQuery; const MasterOwner, MasterTableName: string): Boolean;
var Collections, Selections: TStringList;
    {$IFNDEF NONVISUAL}
    i, SelectedCollection: Integer;
    ReadOnlyDataSet: Boolean;
    s, WherePrefix: string;
    {$ENDIF}
    KeyFields: TStringList;
    ObjType: string;
    Obj: TOracleObject;
    Attr: TObjectAttribute;
    a: Integer;
begin
  Result := False;
  // Get the key fields, whithout them we can do nothing
  KeyFields := Master.GetPKFields(Q, MasterOwner, MasterTableName, False);
  if KeyFields = nil then Exit;
  // Get the collection columns from the master table
  Collections := TStringList.Create;
  Selections := TStringList.Create;
  try
    Q.Clear;
    Q.SQL.Add('select c.data_type_owner, c.data_type, c.column_name, t.typecode');
    Q.SQL.Add('from sys.all_tab_columns c,');
    Q.SQL.Add('     sys.all_types t');
    Q.SQL.Add('where c.owner = :owner');
    Q.SQL.Add('and c.table_name = :table_name');
    Q.SQL.Add('and c.data_type is not null');
    Q.SQL.Add('and c.data_type_owner is not null');
    Q.SQL.Add('and c.data_type_mod is null');
    Q.SQL.Add('and t.owner = c.data_type_owner');
    Q.SQL.Add('and t.type_name = c.data_type');
    Q.SQL.Add('and t.typecode in (''COLLECTION'', ''OBJECT'')');
    Q.SQL.Add('order by c.column_id');
    Q.DeclareVariable('owner', otString);
    Q.DeclareVariable('table_name', otString);
    Q.SetVariable('owner', MasterOwner);
    Q.SetVariable('table_name', MasterTableName);
    Q.Execute;
    while not Q.Eof do
    begin
      ObjType := Q.Field('data_type_owner') + '.' + Q.Field('data_type');
      if Q.Field('typecode') = 'COLLECTION' then
      begin
        Collections.Add(Q.Field('column_name'));
        Selections.Add(Q.Field('column_name') + ' (' + ObjType + ')');
      end else begin
        Obj := nil;
        try
          Obj := TOracleObject.Create(Q.Session, ObjType, '');
          for a := 0 to Obj.AttrCount - 1 do
          begin
            Attr := Obj.Attrs[a];
            if Attr.IsCollection then
            begin
              Collections.Add(Q.Field('column_name') + '.' + Attr.Name);
              Selections.Add(Q.Field('column_name') + '.' + Attr.Name + ' (' + Attr.TypeName + ')');
            end;
          end;
          Obj.Free;
        except
        end;
        try
          Obj.Free;
        except
        end;
      end;
      Q.Next;
    end;
  except
  end;
  // If there are any collections, provide a selection list and apply them to
  // the SQL and variables
  if Collections.Count > 0 then
  begin
    Result := True;
    {$IFNDEF NONVISUAL}
    if SelectCollectionForDataSet(Selections, SelectedCollection, ReadOnlyDataSet) then
    begin
      SQL.Clear;
      DeleteVariables;
      MasterFields := '';
      DetailFields := '';
      SQL.Add('select t.*');
      if not ReadOnlyDataSet then SQL[0] := SQL[0] + ', rowid';
      SQL.Add('from table(select m.' + AnsiLowerCase(Collections[SelectedCollection]));
      SQL.Add('           from   ' + AnsiLowerCase(Master.GetUpdatingTable) + ' m');
      // Get the data types of the key fields
      Q.Clear;
      Q.SQL.Text := 'select ' + KeyFields.CommaText + ' from ' + Master.GetUpdatingTable;
      Q.Describe;
      WherePrefix := '           where  ';
      for i := 0 to Q.FieldCount - 1 do
      begin
        // Get the column name
        s := AnsiLowerCase(Q.FieldName(i));
        // Declare a corresponding variable
        DeclareVariable(s, Q.FieldType(i));
        // Add it to the master fields
        if MasterFields <> '' then MasterFields := MasterFields + ';';
        MasterFields := MasterFields + s;
        // Add it to the where clause
        s := WherePrefix + 'm.' + s + ' = :' + s;
        if i = Q.FieldCount - 1 then s := s + ') t';
        SQL.Add(s);
        WherePrefix := '           and    ';
      end;
    end;
    {$ENDIF}
  end;
  Collections.Free;
  KeyFields.Free;
end;

// Determine Master & DetailFields based on Foreign Key Constraint
procedure TOracleDataSet.GetMasterDetailFields;
var MasterOwner, MasterTableName, DetailOwner, DetailTableName: string;
    PrimaryKey, ForeignKey: string;
    ModifyQuery: Boolean;
    ColumnList: string;
    WhereClause, NewSQL: WideString;
    p1, p2, c: Integer;
    s: string;
    Q: TOracleQuery;
    DataSet: TOracleDataSet;
    VarType: Integer;
begin
  if (Master = nil) then Exit;
  // Use the DMLQuery of the detail or master, whichever is usable
  if (Session <> nil) and Session.Connected then
  begin
    OpenDMLQuery(True);
    Q := DMLQuery;
    DataSet := Self;
  end else begin
    if (Master.Session <> nil) and Master.Session.Connected then
    begin
      Master.OpenDMLQuery(True);
      Q := Master.DMLQuery;
      DataSet := Master;
    end else begin
      ShowMessage('Automatic Master/Detail configuration is only' + #13#10 +
                  'possible if the Session is connected.');
      Exit;
    end;
  end;
  MasterFields := '';
  DataSet.ResolveName(Master.GetUpdatingTable, MasterOwner, MasterTableName);
  if MasterTableName = '' then
  begin
    ShowMessage('Automatic Master/Detail configuration is only' + #13#10 +
                'possible if the Master SQL text is set correctly.');
    Exit;
  end;
  // Maybe the master contains one or more collection fields?
  if GetCollectionMasterFields(Q, MasterOwner, MasterTableName) then Exit;
  // No, continue with the detail
  DetailFields := '';
  DataSet.ResolveName(GetUpdatingTable, DetailOwner, DetailTableName);
  if DetailTableName = '' then
  begin
    ShowMessage('Automatic Master/Detail configuraion is only' + #13#10 +
                'possible if the Detail SQL text is set correctly,' + #13#10 +
                'or if the Master dataset contains one or more collection fields.');
    Exit;
  end;
  // Determine related foreign key & primary/unique key
  with Q do
  try
    Clear;
    ReadBuffer := 5;
    SQL.Add('select fk.constraint_name fkname, pk.constraint_name pkname');
    SQL.Add('  from sys.all_constraints fk, sys.all_constraints pk');
    SQL.Add(' where fk.constraint_type = ''R''');
    SQL.Add('   and fk.owner = :fk_owner and fk.table_name = :fk_table_name');
    SQL.Add('   and pk.constraint_name = fk.r_constraint_name');
    SQL.Add('   and pk.owner = :pk_owner and pk.table_name = :pk_table_name');
    DeclareVariable('fk_owner', otString);
    DeclareVariable('fk_table_name', otString);
    DeclareVariable('pk_owner', otString);
    DeclareVariable('pk_table_name', otString);
    SetVariable('fk_owner', DetailOwner);
    SetVariable('fk_table_name', DetailTableName);
    SetVariable('pk_owner', MasterOwner);
    SetVariable('pk_table_name', MasterTableName);
    Execute;
    // If no foreign key exists between the two tables, exit
    if EOF then
    begin
      ShowMessage('Automatic Master/Detail configuration is only' + #13#10 +
                  'possible if a foreign key constraint exists' + #13#10 +
                  'between the Master and Detail table.');
      Exit;
    end;
    ForeignKey := Field('fkname');
    PrimaryKey := Field('pkname');
    // If more than one foreign key exists between the two tables, exit
    Next;
    if not EOF then
    begin
      ShowMessage('Automatic Master/Detail configuration is only' + #13#10 +
                  'possible if just one foreign key constraint' + #13#10 +
                  'exists between the Master and Detail table.');
      Exit;
    end;
    // Ask to modify query

    ModifyQuery := Confirm(
      'Modify query to add variables to WHERE clause?' + #13#10 +
      'NOTE: This will remove any existing WHERE clause and variables!',
      'Confirm', 'YN') = idYes;

    // Determine columns
    Clear;
    SQL.Add('select column_name from sys.all_cons_columns');
    SQL.Add(' where owner = :owner and constraint_name = :constraint_name');
    SQL.Add(' order by position');
    DeclareVariable('owner', otString);
    DeclareVariable('constraint_name', otString);
    SetVariable('owner', DetailOwner);
    SetVariable('constraint_name', ForeignKey);
    Execute;
    ColumnList := '';
    while not EOF do
    begin
      if DetailFields <> '' then
      begin
        DetailFields := DetailFields + ';';
        ColumnList := ColumnList + ', ';
      end;
      DetailFields := DetailFields + Field('column_name');
      ColumnList := ColumnList + Field('column_name');
      Next;
    end;
    SetVariable('owner', MasterOwner);
    SetVariable('constraint_name', PrimaryKey);
    Execute;
    while not EOF do
    begin
      if MasterFields <> '' then MasterFields := MasterFields + ';';
      MasterFields := MasterFields + Field('column_name');
      Next;
    end;
    // Create variables and where clause, describe SQL statement to get datatypes
    if ModifyQuery then
    begin
      // Delete existing variables
      FQuery.DeleteVariables;
      // Get a list of new variables and create a WHERE clause from it
      Clear;
      SQL.Add('select ' + ColumnList + ' from ' + GetUpdatingTable);
      Describe;
      WhereClause := 'where ';
      for c := 0 to FieldCount - 1 do
      begin
        if c > 0 then WhereClause := WhereClause + ' and ';
        s := AnsiLowerCase(FieldName(c));
        WhereClause := WhereClause + s + ' = :' + s;
        VarType := FieldType(c);
        if FieldDBType(c) = otDBChar then VarType := otChar;
        FQuery.DeclareVariable(s, VarType);
      end;
      WhereClause := WhereClause + #13#10;
      // Replace the current WHERE clause
      NewSQL := FQuery.SQL.Text;
      p1 := WordPos('WHERE', NewSQL);
      p2 := WordPos('ORDER', NewSQL);
      if p2 = 0 then p2 := Length(NewSQL) + 1;
      if p1 > 0 then
        System.Delete(NewSQL, p1, p2 - p1)
      else
        WhereClause := #13#10 + WhereClause;
      if p1 <= 0 then p1 := p2;
      System.Insert(WhereClause, NewSQL, p1);
      FQuery.SQL.Text := NewSQL;
    end;
  except
    raise; // ??????
  end;
end;

// Set the master of this detail
procedure TOracleDataSet.SetMaster(Value: TOracleDataSet);
var AMaster: TOracleDataSet;
begin
  if Value <> FMaster then
  begin
    // Check circular master/detail dependencies
    AMaster := Value;
    while AMaster <> nil do
    begin
      if AMaster = Self then
        raise Exception.Create('Circular master/detail dependency not allowed');
      AMaster := AMaster.Master;
    end;
    Active := False;
    // Remove this dataset from the details of the current master
    if FMaster <> nil then FMaster.RemoveDetail(Self);
    FMaster := Value;
    // Add this dataset to the details of the new master
    if FMaster <> nil then
    begin
      FMaster.AddDetail(Self);
      // Nested dataset MIDAS support
      {$IFDEF CompilerVersion4}
      FMasterLink.DataSource := FMaster.FMasterSource;
      {$ENDIF}
    end else begin
      {$IFDEF CompilerVersion4}
      FMasterLink.DataSource := nil;
      {$ENDIF}
    end;
    if (ComponentState - IgnoreState = [csDesigning]) and
       ((FMaster = nil) or (FMaster.ComponentState - IgnoreState = [csDesigning])) then
      GetMasterDetailFields;
  end;
end;

procedure TOracleDataSet.SetMasterFields(Value: string);
begin
  Active := False;
  FMasterFields := Value;
end;

procedure TOracleDataSet.SetDetailFields(Value: string);
begin
  Active := False;
  FDetailFields := Value;
end;

// Add a detail to this master
procedure TOracleDataSet.AddDetail(ADetail: TOracleDataSet);
begin
  // Check if detail is already on the list, else add it
  if FDetails.IndexOf(ADetail) < 0 then FDetails.Add(ADetail);
end;

// Remove a detail from this master
procedure TOracleDataSet.RemoveDetail(ADetail: TOracleDataSet);
var i: Integer;
begin
  i := FDetails.IndexOf(ADetail);
  if i >= 0 then FDetails.Delete(i);
end;

// Return a list of details
procedure TOracleDataSet.GetDetailList(var AList: TList);
var d, i: Integer;
    Detail: TOracleDataSet;
begin
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    i := AList.IndexOf(Detail);
    if i >= 0 then AList.Delete(i);
    AList.Add(Detail);
    Detail.GetDetailList(AList);
  end;
end;

// Synchronize details to this master
procedure TOracleDataSet.SyncDetails(Clear: Boolean);
var d: Integer;
    Detail: TOracleDataSet;
    Skipped, Queried: Boolean;
begin
  // If controls are disabled, no detail synchronization
  if ControlsDisabled and not ForceMasterSynchronization then Exit;
  // Refresh all details
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    if Clear then
      Detail.MasterBookmark := -1
    else begin
      // Only if active and not synchronized previously or masterless detail
      Skipped := False;
      Queried := False;
      if Detail.Active and (not Detail.QBEMode) then
      begin
        if not Detail.MasterLess then
          Skipped := (Detail.MasterBookmark = Detail.GetMasterBookmark);
        if not Skipped then
        begin
          Detail.Refresh;
          Queried := True;
        end;
      end;
      if (Detail.Query <> nil) and (not Queried) and (not Skipped) then
        Detail.Query.Cancel;
    end;
  end;
end;

// Synchronize detail variables
procedure TOracleDataSet.SyncDetailVars;
var d: Integer;
    Detail: TOracleDataSet;
begin
  // If controls are disabled, no detail synchronization
  if ControlsDisabled and not ForceMasterSynchronization then Exit;
  // Set variables of all details
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    if Detail.Active then Detail.CopyMasterVarValues(nil);
  end;
end;

// Copy the values of the MasterFields to the DetailFields
procedure TOracleDataSet.CopyMasterFieldValues;
var f, fc: Integer;
    mf, df: TList;
begin
  if (FMaster = nil) or not FMaster.Active then Exit;
  mf := TList.Create;
  df := TList.Create;
  // Get the fieldlists
  FMaster.GetFieldList(mf, MasterFields);
  GetFieldList(df, DetailFields);
  // Determine minimum number of fields
  if mf.Count < df.Count then fc := mf.Count else fc := df.Count;
  // Set the fields in this detail to the values of the master fields
  for f := 0 to fc - 1 do
    TField(df[f]).Value := TField(mf[f]).Value;
  mf.Free;
  df.Free;
end;

// Check the master fields for this detail
procedure TOracleDataSet.CheckMasterFields;
begin
  if (FMaster <> nil) and (FMaster.Active) then FMaster.EnableMasterFields(False);
end;

// Enable the the master fields by setting the read-only property
procedure TOracleDataSet.EnableMasterFields(NoDetails: Boolean);
var d, f: Integer;
    Detail: TOracleDataSet;
    mf: TList;
    ReadOnlyValue: Boolean;
begin
  if QBEMode then Exit;
  // Some basic checks, not if there are no details or if we're at design time
  if (FDetails.Count = 0) or (csDesigning in ComponentState) then Exit;
  // Not if we're in a remote data module
  if (Owner <> nil) and
     (Owner.ClassParent <> nil) and
     (Pos('REMOTEDATAMODULE', UpperCase(Owner.ClassParent.ClassName)) > 0) then Exit;
  {$IFDEF CompilerVersion5}
  if ProviderDataSet then Exit;
  {$ENDIF}
  mf := TList.Create;
  NoDataEvents := True;
  // Set all read-only properties to their original value
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    // Determine the master fields for this detail
    mf.Clear;
    GetFieldList(mf, Detail.MasterFields);
    // Set the ReadOnly property of the master fields to False
    for f := 0 to mf.Count - 1 do
      if not IsReadOnlyField(mf[f]) then TField(mf[f]).ReadOnly := False;
  end;
  // Disable those master fields that have details
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    // Determine if master fields must be read-only
    ReadOnlyValue := True;
    if ((Detail.Records = nil) or (Detail.Records.RecordCount = 0)) and
       (Detail.State <> dsInsert) then ReadOnlyValue := False;
    if NoDetails then ReadOnlyValue := False;
    if ReadOnlyValue then
    begin
      // Determine the master fields for this detail
      mf.Clear;
      GetFieldList(mf, Detail.MasterFields);
      // Set the ReadOnly property of the master fields to True
      for f := 0 to mf.Count - 1 do
        if not IsReadOnlyField(mf[f]) then TField(mf[f]).ReadOnly := True;
    end;
  end;
  NoDataEvents := False;
  mf.Free;
end;

// Check if the master fields are not null
procedure TOracleDataSet.CheckMasterFieldsNull;
var mf: TList;
    f: Integer;
    Field: TField;
begin
  if (FMaster = nil) or not FMaster.Active then Exit;
  mf := TList.Create;
  // Get the fieldlists
  FMaster.GetFieldList(mf, MasterFields);
  // Check each master field
  for f := 0 to mf.Count - 1 do
  begin
    Field := mf[f];
    if Field.IsNull or ((Field.DataType in [ftString, ftMemo, ftWideString]) and (Trim(Field.Value) = '')) then
    begin
      mf.Free;
      Field.FocusControl;
      DatabaseErrorFmt(SFieldRequired, [Field.DisplayName]);
    end;
  end;
  mf.Free;
end;

// Compare the detail fields in a recordbuffer to the master fields
function TOracleDataSet.CompareMasterFieldValues(Buffer: PRecordData): Boolean;
var OldState: TDataSetState;
    OldFilterBuffer: PRecordData;
    f, fc: Integer;
    mf, df: TList;
    v1, v2: Variant;
begin
  Result := True;
  if (FMaster = nil) or not FMaster.Active then Exit;
  mf := TList.Create;
  df := TList.Create;
  OldState := SetTempState(dsFilter);
  OldFilterBuffer := FFilterBuffer;
  try
    FFilterBuffer := Buffer;
    // Get the fieldlists
    FMaster.GetFieldList(mf, MasterFields);
    GetFieldList(df, DetailFields);
    // Determine minimum number of fields
    if mf.Count < df.Count then fc := mf.Count else fc := df.Count;
    // Compare the fields in this detail to the values of the master fields
    for f := 0 to fc - 1 do
    begin
      v1 := TField(df[f]).Value;
      v2 := TField(mf[f]).Value;
      if not (VarIsNull(v1) and VarIsNull(v2)) then
      begin
        if VarIsNull(v1) xor VarIsNull(v2) then Result := False;
        if Result and (v1 <> v2) then Result := False;
      end;
    end;
  finally
    RestoreState(OldState);
    FFilterBuffer := OldFilterBuffer;
    mf.Free;
    df.Free;
  end;
end;

function SplitItems(S: string): TStringList;
var i: Integer;
    Item: string;
begin
  Result := TStringList.Create;
  S := S + ';';
  Item := '';
  for i := 1 to Length(S) do
  begin
    if S[i] <> ';' then
      Item := Item + S[i]
    else begin
      Item := Trim(Item);
      if Item <> '' then Result.Add(Trim(Item));
      Item := '';
    end;
  end;
end;

// Copy the values of the MasterFields to the Detail variables
procedure TOracleDataSet.CopyMasterVarValues(Buffer: PRecordData);
var f, fc, fi: Integer;
    mf: TList;
    df: TStringList;
    Field: TField;
    s: string;
    VarName: string;
    OldState: TDataSetState;
    OldFilterBuffer: PRecordData;
begin
  if (FMaster = nil) or not FMaster.Active then Exit;
  if Buffer <> nil then
  begin
    OldState := Master.SetTempState(dsFilter);
    OldFilterBuffer := Master.FFilterBuffer;
    Master.FFilterBuffer := Buffer;
  end else begin
    OldState := Master.State;
    OldFilterBuffer := nil;
  end;
  try
    // Get the fieldlists
    mf := TList.Create;
    FMaster.GetFieldList(mf, MasterFields);
    df := SplitItems(DetailFields);
    // Determine minimum number of fields
    if (mf.Count < df.Count) or (df.Count = 0) then fc := mf.Count else fc := df.Count;
    // Set the variables in this detail to the values of the master fields
    for f := 0 to fc - 1 do
    begin
      Field := mf[f];
      fi := FMaster.FieldIndex(Field);
      if df.Count > 0 then VarName := df[f] else VarName := Field.FieldName;
      if (fi >= 0) and (FMaster.Query.FieldDBType(fi) = otDBChar) and (VariableType(VariableIndex(VarName)) <> otChar) then
      begin
        // For char columns, pad the string with spaces
        s := Field.AsString;
        s := s + StringOfChar(' ', FMaster.Query.FieldSize(fi) - Length(s));
        SetVariable(VarName, s);
      end else begin
        // For all other data types, just set the value
        SetVariable(VarName, Field.Value);
      end;
    end;
    mf.Free;
    df.Free;
  finally
    if Buffer <> nil then
    begin
      Master.RestoreState(OldState);
      Master.FFilterBuffer := OldFilterBuffer;
    end;
  end;
end;

// Determine if a detail is MasterLess (has no master record)
function TOracleDataSet.MasterLess: Boolean;
begin
  Result := (FMaster <> nil) and
  ((not FMaster.Active) or (FMaster.Records = nil) or (FMaster.IsEmpty) or
   (FMaster.UpdateStatus = usInserted) or (FMaster.QBEMode));
end;

// Get the bookmark of the master
function TOracleDataSet.GetMasterBookmark: Integer;
var b:PRecordData;
begin
  Result := -1;
  if (FMaster <> nil) and FMaster.GetActiveRecBuf(b) then Result := b.Bookmark;
end;

// Does this master have any loose details with cached updates?
function TOracleDataSet.HasLooseCachedUpdates: Boolean;
var d: Integer;
    Detail: TOracleDataSet;
begin
  Result := False;
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    if Detail.CachedUpdates and Detail.IsLooseDetail and (Detail.ChangeLog.Count > 0) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// Is this a loose detail dataset?
// A loose detail dataset does not have any master fields, and is matched to
// the master by the MasterBookmark
function TOracleDataSet.IsLooseDetail: Boolean;
begin
  Result := (Master <> nil) and (Trim(DetailFields) = '');
end;

(*
// Is this field a MasterField?
function TOracleDataSet.IsMasterField(const AFieldName: string): Boolean;
var f: Integer;
    mf: TList;
begin
  Result := False;
  if FMaster = nil then Exit;
  // Get the fieldlist
  mf := TList.Create;
  FMaster.FieldList(mf, MasterFields);
  // Set the variables in this detail to the values of the master fields
  for f := 0 to mf.Count - 1 do
  begin
    if stricomp(PChar(AFieldName), PChar(TField(mf[f]).FieldName)) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
  mf.Free;
end;
*)

// When a new record is created:
// - The MasterFields may not be Null
// - The MasterFields must be copied to the DetailFields
// - The SequenceField may be applied
procedure TOracleDataSet.DoOnNewRecord;
var T: TOracleTableInfo;
begin
  if Records.RecordCount > 0 then CheckQBEMode;
  if QBEMode then Exit;
  CheckMasterFieldsNull;
  NoFieldValidation := True;
  try
    CopyMasterFieldValues;
    if SequenceField.ApplyMoment = amOnNewRecord then SequenceField.Apply;
    if OracleDictionary.DefaultValues then
    begin
      T := GetOracleTableInfo;
      T.GetColumnDefaultValues(Self);
    end;
  finally
    NoFieldValidation := False;
  end;
  inherited;
end;

// Post all details of this master
procedure TOracleDataSet.PostDetails;
var d: Integer;
    Detail: TOracleDataSet;
begin
  // Post all details
  for d := 0 to FDetails.Count - 1 do
  begin
    Detail := TOracleDataSet(FDetails[d]);
    if Detail.Active then
    begin
      if (Detail.State in [dsEdit, dsInsert]) and (not Detail.QBEMode) then Detail.Post;
      Detail.PostDetails;
    end;
  end;
end;

function TOracleDataSet.InternalQuery: TOracleQuery;
begin
  Result := FQuery;
end;

procedure TOracleDataSet.SetReadOnly(Value: Boolean);
begin
  if Value = FReadOnly then Exit;
  FReadOnly := Value;
  DataEvent(deDataSetChange, 0);
end;

// QBE Methods

// The property
procedure TOracleDataSet.SetQBEMode(Value: Boolean);
begin
  if Value = FQBEMode then Exit;
  if not Active then
    FQBEMode := Value
  else
    if Value then EnterQBEMode(True) else CancelQBEMode(True);
end;

// A 'hack' to set Field.Size
// Necessary to remove the edit limition in QBE mode
procedure SetFieldSize(Field: TField; NewSize: Integer);
begin
  {$IFDEF COMPILERVERSION6}
  PIntArray(Field)^[18] := NewSize;
  {$ELSE} {$IFDEF COMPILERVERSION5}
  PIntArray(Field)^[15] := NewSize;
  {$ELSE} {$IFDEF COMPILERVERSION4}
  PIntArray(Field)^[14] := NewSize;
  {$ELSE} {$IFDEF COMPILERVERSION3}
  PIntArray(Field)^[13] := NewSize;
  {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF}
end;

// Enter QBE Mode
procedure TOracleDataSet.EnterQBEMode(Sync: Boolean);
var f, w: Integer;
    FieldInfo: TOracleFieldInfo;
begin
  // Can we?
  CanQBE(True);
  if Sync then
  begin
    // Post any updates and go to the first record
    CheckBrowseMode;
    // Now we're in QBE Mode
    FQBEMode := True;
    // Clear the dataset
    Records.FreeRecords;
    try
      First;
      Resync([]);
    except
    end;
  end;
  // Set the QBE properties of the fields
  if not (csDesigning in ComponentState) then
  begin
    for f := 1 to FieldInfoList.Count - 1 do
    begin
      FieldInfo := FieldInfoList[f];
      if FieldInfo.Field <> nil then
      begin

        FieldInfo.QBEReadOnly := FieldInfo.Field.ReadOnly;
        FieldInfo.Field.ReadOnly := not FieldInfo.QBEField.Queryable;
        FieldInfo.QBERequired := FieldInfo.Field.Required;
        FieldInfo.Field.Required := False;


        FieldInfo.QBEField.OldValidChars := FieldInfo.Field.ValidChars;
        FieldInfo.QBEField.OldSize := FieldInfo.Field.Size;
        FieldInfo.QBEField.OldWidth := FieldInfo.Field.DisplayWidth;

        if QBEDefinition.AllowOperators then
        begin
          if not FieldInfo.IsWide then
          begin
            FieldInfo.QBEField.OldGetText := FieldInfo.Field.OnGetText;
            FieldInfo.QBEField.OldSetText := FieldInfo.Field.OnSetText;
            FieldInfo.QBEField.OldChange  := FieldInfo.Field.OnChange;
            FieldInfo.Field.OnGetText := FieldInfo.QBEField.GetText;
            FieldInfo.Field.OnSetText := FieldInfo.QBEField.SetText;
            FieldInfo.Field.OnChange  := FieldInfo.QBEField.Change;
            if FieldInfo.Field.Size > 0 then
            begin
              w := FieldInfo.Field.DisplayWidth;
              if w <= 0 then w := FieldInfo.Field.Size;
              FieldInfo.Field.DisplayWidth := w;
              SetFieldSize(FieldInfo.Field, 999);
            end;
            FieldInfo.Field.ValidChars := [#32..#255];
          end;
        end;

      end;
    end;
  end;
  // Insert QBE record and restore previous QBE values
  try
    if Sync then Insert;
  except
  end;
  QBEDefinition.Silent := True;
  try
    if QBEDefinition.AllowOperators and not QBEDefinition.SaveQBEValues then
    begin
      for f := 0 to QBEDefinition.FieldCount - 1 do
        QBEDefinition.Fields[f].Value := '';
    end;
    if QBEDefinition.SaveQBEValues then RestoreQBEValues;
    // Trigger BeforeQBE event
    if Assigned(FBeforeQBE) then FBeforeQBE(Self);
    SetQBEColors
  finally
    QBEDefinition.Silent := False;
  end;
end;

// Cancel QBE Mode
procedure TOracleDataSet.CancelQBEMode(DoOnCancelQBE: Boolean);
var f: Integer;
    FieldInfo: TOracleFieldInfo;
    Requery: Boolean;
begin
  Requery := True;
  if DoOnCancelQBE and Assigned(FOnCancelQBE) then FOnCancelQBE(Self, Requery);
  if not (csDesigning in ComponentState) then
  begin
    for f := 1 to FieldInfoList.Count - 1 do
    begin
      FieldInfo := FieldInfoList[f];
      if FieldInfo.Field <> nil then
      begin
        FieldInfo.Field.ReadOnly := FieldInfo.QBEReadOnly;
        FieldInfo.Field.Required := FieldInfo.QBERequired;

        if QBEDefinition.AllowOperators then
        begin
          if not FieldInfo.IsWide then
          begin
            FieldInfo.Field.OnGetText := FieldInfo.QBEField.OldGetText;
            FieldInfo.Field.OnSetText := FieldInfo.QBEField.OldSetText;
            FieldInfo.Field.OnChange  := FieldInfo.QBEField.OldChange;
            FieldInfo.Field.ValidChars := FieldInfo.QBEField.OldValidChars;
            SetFieldSize(FieldInfo.Field, FieldInfo.QBEField.OldSize);
            if FieldInfo.Field.DisplayWidth <> FieldInfo.QBEField.OldWidth then
              FieldInfo.Field.DisplayWidth := FieldInfo.QBEField.OldWidth;
          end;
        end;
      end;
    end;
  end;
  Cancel;
  FQBEMode := False;
  if QBEError then ClearQBE;
  if DoOnCancelQBE then SetQBEColors;
  DataEvent(deDataSetChange, 0);
  if Requery then Refresh;
end;

// Execute Query By Example
procedure TOracleDataSet.ExecuteQBE;
begin
  CheckActive;
  if not QBEMode then raise Exception.Create(dtQBEOperation);
  if IsCursorDataSet then raise Exception.Create(dtNoQBEForCursor);
  if State = dsInsert then UpdateRecord;
  if Assigned(FAfterQBE) then FAfterQBE(Self);
  SaveQBEValues;
  AddQBEToQuery;
  QBEError := False;
  try
    CancelQBEMode(False);
    SetQBEColors;
  except
    QBEError := True;
    SetQBEColors;
    EnterQBEMode(True);
    raise;
  end;
end;

// Describe Query By Example
procedure TOracleDataSet.DescribeQBE;
begin
  if not QBEMode then raise Exception.Create(dtQBEOperation);
  if IsCursorDataSet then raise Exception.Create(dtNoQBEForCursor);
  if State = dsInsert then UpdateRecord;
  SaveQBEValues;
  AddQBEToQuery;
  FQuery.Describe;
end;

// Check Query By Example
procedure TOracleDataSet.CheckQBEMode;
begin
  if QBEMode then raise Exception.Create(dtNoQBEOperation);
end;

// Can the dataset go to QBE Mode?
function TOracleDataSet.CanQBE(RaiseException: Boolean): Boolean;
begin
  Result := True;
  // If it's already in QBE mode, then yes
  if QBEMode then Exit;
  // Cursors cannot be queried, where to put the where clause?
  if IsCursorDataSet then
  begin
    Result := False;
    if RaiseException then raise Exception.Create(dtNoQBEForCursor);
  end;
  // Don't mix pending updates with queries
  if (CachedUpdates) and (UpdatesPending or (State in [dsInsert, dsEdit])) then
  begin
    Result := False;
    if RaiseException then raise Exception.Create(dtNoQBEUpdatesPending);
  end;
  // You can't query a detail without its master
  if MasterLess then
  begin
    Result := False;
    if RaiseException then raise Exception.Create(dtNoQBEMasterLess);
  end;
  // You can't query if you're not connected
  if Detached then
  begin
    Result := False;
    if RaiseException then raise Exception.Create(dtNoQBEDetached);
  end;
end;

// Save the QBE values from the dataset to the field info
procedure TOracleDataSet.SaveQBEValues;
var f: Integer;
    FieldInfo: TOracleFieldInfo;
begin
  for f := 1 to FieldInfoList.Count - 1 do
  begin
    FieldInfo := FieldInfoList[f];
    if (FieldInfo.Field <> nil) and (not FieldInfo.Field.IsBlob) then
    begin
      if FieldInfo.QBEField.Value = '' then
        FieldInfo.QBEField.LastValue := Null
      else begin
        FieldInfo.QBEField.LastValue := FieldInfo.QBEField.Value;
      end;
    end;
  end;
end;

// Restore the QBE values to the dataset
procedure TOracleDataSet.RestoreQBEValues;
var f: Integer;
    FieldInfo: TOracleFieldInfo;
begin
  for f := 1 to FieldInfoList.Count - 1 do
  begin
    FieldInfo := FieldInfoList[f];
    if (FieldInfo.Field <> nil) and FieldInfo.QBEField.Queryable and
       (not VarIsNull(FieldInfo.QBEField.LastValue)) then
    try
      FieldInfo.QBEField.Value := FieldInfo.QBEField.LastValue;
    except
    end;
  end;
end;

// Toggle between QBE Values and empty record
procedure TOracleDataSet.ToggleQBEValues;
var f: Integer;
    FieldInfo: TOracleFieldInfo;
    Empty: Boolean;
begin
  // Check if the current record is empty
  Empty := True;
  for f := 1 to FieldInfoList.Count - 1 do
  begin
    FieldInfo := FieldInfoList[f];
    if (FieldInfo.Field <> nil) and FieldInfo.QBEField.Queryable and
       (not FieldInfo.QBEField.IsNull) then Empty := False;
  end;
  // If it's empty restore previous QBE values, else clear it
  if Empty then
    RestoreQBEValues
  else begin
    for f := 1 to FieldInfoList.Count - 1 do
    begin
      FieldInfo := FieldInfoList[f];
      if (FieldInfo.Field <> nil) and FieldInfo.QBEField.Queryable then
      begin
        FieldInfo.QBEField.Value := '';
        FieldInfo.Field.Value := Null;
      end;
    end;
  end;
end;

procedure TOracleDataSet.ClearQBE;
var f: Integer;
    FieldInfo: TOracleFieldInfo;
begin
  if not QBEModified then Exit;
  for f := 1 to FieldInfoList.Count - 1 do
  begin
    FieldInfo := FieldInfoList[f];
    if FieldInfo.QBEField <> nil then
    begin
      FieldInfo.QBEField.LastValue := Null;
      FieldInfo.QBEField.Value := '';
    end;
  end;
  RemoveQBEFromQuery;
  if Active then Refresh;
end;

// Does this string contain wildcards?
function HasSQLWildCards(const S: string): Boolean;
var p: Integer;
    cs: string;
begin
  cs := s;
  repeat
    p := Pos('to_', cs);
    if p > 0 then System.Delete(cs, p, 3);
  until p <= 0;
  Result := (Pos('%', cs) > 0) or (Pos('_', cs) > 0);
end;

// Convert file wildcards (*, ?) to SQL wildcards (%, _)
procedure ConvertFileWildcards(var s: WideString);
var i: Integer;
    w: WideString;
begin
  w := s;
  for i := 1 to Length(w) do
  begin
    if w[i] = '*' then w[i] := '%';
    if w[i] = '?' then w[i] := '_';
  end;
  s := w;
end;

function RemoveParenthesisAndQuotes(const ASQL: WideString): WideString;
var i, pLevel: Integer;
    qMode: Boolean;
begin
  Result := ASQL;
  // Discard text between parnthesis and quotes
  pLevel := 0;
  qMode := False;
  for i := 1 to Length(Result) do
  begin
    if Result[i] = '(' then Inc(pLevel);
    if Result[i] = ')' then Dec(pLevel);
    if Result[i] = '''' then qMode := not qMode;
    if ((pLevel > 0) or qMode) and not WideCharIn(Result[i], ['(', ')', '''']) then
      Result[i] := '_';
  end;
end;

// Split the select statement into 3 parts: select_part1 where_part2 order_by_part3
procedure SplitSelect(Select: widestring; KeepHints: Boolean;
                      var BeforeWhere, WhereClause, AfterWhere, WhereWord: widestring);
var p0, p1, p2: Integer;
    ncSelect, ucSelect: widestring;
begin
  // Remove comments
  ncSelect := RemoveSQLComment(Select, KeepHints);
  // Convert to uppercase
  ucSelect := AnsiUpperCase(ncSelect);
  // Discard text between parnthesis and quotes
  ucSelect := RemoveParenthesisAndQuotes(ucSelect);
  p0 := WordPos('GROUP', ucSelect);
  if p0 > 0 then WhereWord := 'having' else WhereWord := 'where';
  p1 := WordPos(AnsiUpperCase(WhereWord), ucSelect);
  p2 := WordPos('ORDER', ucSelect);
  if p2 <= 0 then p2 := WordPos('FOR', ucSelect);
  if p2 > 0 then
    AfterWhere := Copy(ncSelect, p2, Length(ncSelect))
  else begin
    AfterWhere := '';
    p2 := Length(ncSelect) + 1;
  end;
  if p1 > 0 then
  begin
    BeforeWhere := Copy(ncSelect, 1, p1 - 1);
    WhereClause := Copy(ncSelect, p1 + Length(WhereWord), p2 - p1 - Length(WhereWord));
  end else begin
    BeforeWhere := Copy(ncSelect, 1, p2 - 1);
    WhereClause := '';
  end;
  BeforeWhere := Trim(BeforeWhere);
  WhereClause := Trim(WhereClause);
  AfterWhere  := Trim(AfterWhere);
end;

function AnalyzeExpression(S: WideString; var FieldName: widestring; var DoNot: Boolean; FieldType: TFieldType; QBE: TQBEField; DataSet: TOracleDataSet): WideString;
var i, ix: Integer;
    Operator: WideString;
    Parameters: array[0..99] of widestring;
    ParameterCount: Integer;
    W, X, V: WideString;
    AndOr, SingleParam, SpaceField, AllowOperatorAdd, OldAutoSoundex: Boolean;
    Separator: TCSet;
    FieldInfo: TOracleFieldInfo;
    MVV: WideString;

 function RemoveQuotes(S: WideString): WideString;
 var Q: WideChar;
     i: Integer;
 begin
   Result := S;
   Q := ' ';
   if (Length(S) > 2) and (S[1] = '''') and (S[Length(S)] = '''') then
   begin
     Result := Copy(S, 2, Length(S) - 2);
     Q := '''';
   end;
   if (Length(S) > 2) and (S[1] = '"') and (S[Length(S)] = '"') then
   begin
     Result := Copy(S, 2, Length(S) - 2);
     Q := '"';
   end;
   // Remove double quotes
   if Q <> ' ' then
   begin
     for i := Length(Result) downto 2 do
     begin
       if (Result[i] = Q) and (Result[i - 1] = Q) then Delete(Result, i, 1);
     end;
   end;
 end;
 function AddQuotes(S: WideString): WideString;
 var i: Integer;
 begin
   Result := '''';
   for i := 1 to Length(S) do
   begin
     Result := Result + WideString(S[i]);
     if S[i] = '''' then Result := Result + '''';
   end;
   Result := Result + '''';
 end;
 function VarOrVal(const Expression: widestring; VarIndex: Integer): widestring;
 var vv: WideString;
     DeclareVar: Boolean;
 begin
   if not (FieldInfo.IsUTF16 or FieldInfo.IsUTF8) then
     Result := Expression
   else begin
     Result := ':doa__qbe' + IntToStr(FieldInfo.Index + 1);
     if VarIndex > 0 then Result := Result + '_' + IntToStr(VarIndex);
     DeclareVar := True;
     if DataSet.Query.VariableIndex(Result) >= 0 then
     begin
       vv := DataSet.Query.GetVariable(Result);
       if Pos(Result, Expression) > 0 then DeclareVar := False;
     end;
     if DeclareVar then
     begin
       DataSet.Query.DeclareVariable(Result, otString);
       DataSet.Query.DeclareVariableCharSet(Result, FieldInfo.CharSetForm, FieldInfo.CharSetID);
       DataSet.Query.SetVariable(Result, RemoveQuotes(Expression));
     end;
   end;
 end;
 function VarQuotes(const Expression: widestring; VarIndex: Integer): widestring;
 begin
   Result := VarOrVal(Expression, VarIndex);
   if Result = Expression then Result := AddQuotes(Expression);
 end;
 procedure AddParameter(const s: widestring);
 begin
   Parameters[ParameterCount] := s;
   Inc(ParameterCount);
 end;
 procedure InsertParameter(Index: Integer; const s: widestring);
 var p: Integer;
 begin
   AddParameter('');
   for p := ParameterCount - 2 downto Index do
     Parameters[p + 1] := Parameters[p];
   Parameters[Index] := s;
 end;
 function NextWord: WideString;
 var Q1, Q2: Boolean;
     W, P: WideString;
     Finished: Boolean;
 begin
   Result := '';
   Q1 := False;
   Q2 := False;
   repeat
     Finished := True;
     while (ix <= Length(S)) and WideCharIn(S[ix], Separator) do
     begin
       X := X + WideString(S[ix]);
       inc(ix);
     end;
     W := '';
     P := Result;
     while (ix <= Length(S)) and ((Q1 or Q2) or not WideCharIn(S[ix], Separator)) do
     begin
       X := X + WideString(S[ix]);
       if (not Q2) and (S[ix] = '''') then Q1 := not Q1;
       if (not Q1) and (S[ix] = '"')  then Q2 := not Q2;
       Result := Result + WideString(S[ix]);
       W := W + Uppercase(WideString(S[ix]));
       inc(ix);
     end;
     if SpaceField and (WideCharIn(S[ix], [' ', ','])) then
     begin
       if (W <> 'OR') and (W <> 'AND') then
       begin
         Result := Result + WideString(S[ix]);
         Finished := False;
       end else begin
         Result := P;
         AndOr := True;
         Finished := (Result <> '');
       end;
     end;
   until (ix >= Length(S)) or Finished;
 end;
 function Remainder: WideString;
 begin
   Result := TrimLeft(Copy(S, ix, Length(S)));
 end;
 function Value(S: WideString; UnModified: Boolean; VarIndex: Integer; out VarValue: WideString): WideString;
 var i, V, Error: Integer;
     B {, CheckWildCards}: Boolean;
     D: TDateTime;
 begin
   Result := S;
   VarValue := Result;
//   CheckWildCards := True;
   // String
   if FieldType in [ftString, ftMemo, ftWideString] then
   begin
     Result := RemoveQuotes(S);
     if QBE.AutoSoundex then
     begin
       if Pos('soundex(', FieldName) = 0 then FieldName := 'soundex(' + FieldName + ')';
       Result := 'soundex(' + VarQuotes(Result, VarIndex) + ')';
//       UnModified := True;
     end else begin
       if not (HasSQLWildCards(Result) or UnModified) then
       begin
         if (QBE.AutoPartialMatch or QBE.AutoContains) and
            ((Result = '') or (Result[Length(Result)] <> '%')) then Result := Result + widestring('%');
         if QBE.AutoContains and (Result[1] <> '%') then Result := widestring('%') + Result;
       end;
       VarValue := Result;
       Result := VarQuotes(Result, VarIndex);
       if QBE.CaseInsensitive then
       begin
         if Pos('upper(', FieldName) = 0 then FieldName := 'upper(' + FieldName + ')';
         Result := 'upper(' + Result + ')';
       end;
     end;
   end;
   // Date
   if FieldType in [ftDateTime, ftDate] then
   begin
     Result := '';
     if HasSQLWildcards(S) then
       Result := S
     else begin
//       CheckWildCards := False;
       D := StrToDateTime(RemoveQuotes(S));
       if Frac(D) <> 0 then
         Result := 'to_date(''' + FormatDateTime('dd-mm-yyyy hh:mm:ss', D) + ''', ''DD-MM-YYYY HH24:MI:SS'')'
       else begin
         if QBE.IgnoreTime and SingleParam and ((Operator = '') or (Operator = '=')) then
         begin
           Operator := '>=';
           Result := 'to_date(''' + FormatDateTime('dd-mm-yyyy', D) + ''', ''DD-MM-YYYY'') and ' +
                     FieldName + ' < to_date(''' + FormatDateTime('dd-mm-yyyy', D + 1) + ''', ''DD-MM-YYYY'')';
         end else
           Result := 'to_date(''' + FormatDateTime('dd-mm-yyyy', D) + ''', ''DD-MM-YYYY'')';
       end;
     end;
   end;
   // Time
   if FieldType in [ftTime] then
   begin
     Result := '';
     D := StrToTime(RemoveQuotes(S));
     Result := 'to_date(''' + FormatDateTime('hh:mm:ss', D) + ''', ''HH24:MI:SS'')';
   end;
   // Integer
   if FieldType in [ftSmallInt, ftInteger, ftWord] then
   begin
     try
       Val(S, V, Error);
       if Error = 0 then
       begin
         Result := IntToStr(V);
         Exit;
       end;
     except
     end;
     Result := S;
     for i := Length(Result) downto 1 do
     begin
       if not WideCharIn(Result[i], ['-', '+', '%', '_', '0' .. '9']) then Delete(Result, i, 1)
     end;
     if Result = '' then Result := S;
   end;
   // Float
   if FieldType in [ftFloat, ftCurrency] then
   begin
     Result := S;
     B := False;
     for i := Length(Result) downto 1 do
     begin
       if WideCharIn(Result[i], ['.', ',']) then
       begin
         if B then
           Delete(Result, i, 1)
         else begin
           Result[i] := '.';
           B := True;
         end;
       end;
     end;
     for i := Length(Result) downto 1 do
     begin
       if not WideCharIn(Result[i], ['-', '+', '%', '_', '0' .. '9', 'e', '.']) then Delete(Result, i, 1)
     end;
     if Result = '' then Result := S;
   end;
(*
   if CheckWildCards and HasSQLWildcards(VarValue) and (not UnModified) then
   begin
     if not (FieldInfo.IsUTF16 or FieldInfo.IsUTF8) then
       Result := AddQuotes(RemoveQuotes(Result));
   end;
*)
 end;
 function Like(Param: WideString): WideString;
 begin
   if Uppercase(Param) = 'NULL' then
     Result := 'is'
   else begin
     if DataSet.Query.VariableIndex(Param) >= 0 then Param := DataSet.Query.GetVariable(Param);
     if HasSQLWildcards(Param) and not (QBE.AutoSoundex) then Result := 'like' else Result := '=';
   end;
 end;
 // Handle 'simple' operators
 function SimpleOperator(S: WideString): Boolean;
 var R, VV: WideString;
 begin
   Result := False;
   if (Length(S) >= 1) and WideCharIn(S[1], ['=', '>', '<', '!']) then
   begin
     SingleParam := True;
     Operator := S[1];
     if (Length(S) >= 2) and WideCharIn(S[2], ['=', '>', '<', '!']) then
       Operator := Operator + WideString(S[2]);
     S := Trim(Copy(S, Length(Operator) + 1, Length(S)));
     R := Value(S, True, 0, VV);
     if Operator = '=' then Operator := like(VV);
     R := Operator + ' ' + R;
     AnalyzeExpression := R;
     Result := True;
   end;
 end;
begin
  Result := '';
  FieldInfo := DataSet.FieldInfoList.FieldInfoByName(QBE.FieldName);
  DoNot := False;
  SingleParam := False;
  SpaceField := False;
  ParameterCount := 0;
  X := '';
  if not QBE.QBEDefinition.AllowOperators then S := AddQuotes(RemoveQuotes(S));
  try

    if SimpleOperator(S) then Exit;

    if Lowercase(Trim(S)) = 'is not null' then S := 'not null';

    // Analyze
    ix := 1;
    Separator := [' ', #9, #10, #13, '('];

    Operator := NextWord;
    if Operator = S then
    begin
      Operator := '';
      AddParameter(S);
    end;

    SpaceField := FieldType in [ftString, ftWideString, ftMemo, ftDate, ftDateTime];
    if UpperCase(Operator) = 'NOT' then
    begin
      DoNot := True;
      if SimpleOperator(Remainder) then Exit;
      Operator := NextWord;
    end;
    AllowOperatorAdd := False;
    X := '';

    repeat
      if UpperCase(Operator) = 'IN' then
      begin
        Separator := Separator + [')', ','];
       SpaceField := False;
      end;
      V := X;
      W := NextWord;
      if V = '' then AllowOperatorAdd := not AndOr;
      AndOr := (UpperCase(W) = 'AND') or (Uppercase(W) = 'OR');
      if (UpperCase(Operator) = 'BETWEEN') then
      begin
        if AndOr then
        begin
          ParameterCount := 0;
          AddParameter(V);
          AddParameter(Remainder);
          Break;
        end;
      end;
      if (W <> '') and (not AndOr) then AddParameter(W);
    until W = '';

    // is null
    if (UpperCase(Operator) = 'NULL') or
       ((Operator = '') and ((ParameterCount = 1) and (UpperCase(Parameters[0]) = 'NULL')) ) or
       (UpperCase(Operator) = 'IS') and
       ((ParameterCount > 0) and (UpperCase(Parameters[0]) = 'NULL')) then
    begin
      Result := 'is null';
      Exit;
    end;

    // sounds like (soundex)
    if UpperCase(Operator) = 'SOUNDS' then
    begin
      OldAutoSoundex := QBE.AutoSoundex;
      QBE.AutoSoundex := True;
      for i := 0 to ParameterCount - 1 do
      begin
        V := Trim(Parameters[i]);
        if (i = 0) and (Pos('LIKE ', UpperCase(V)) = 1) then
        begin
          Delete(V, 1, 5);
          V := Trim(V);
        end;
        if V <> '' then
        begin
          V := Value(V, False, 0, MVV);
          if i > 0 then Result := Result + ' or ' + FieldName;
          Result := Result + ' ' + Like(MVV) + ' ' + V;
        end;
      end;
      QBE.AutoSoundex := OldAutoSoundex;
      Exit;
    end;

    // in
    if UpperCase(Operator) = 'IN' then
    begin
      if ParameterCount > 0 then
      begin
        Result := 'in (';
        for i := 0 to ParameterCount - 1 do
        begin
          if i > 0 then Result := Result + ',';
          Result := Result + Value(Trim(Parameters[i]), True, i + 1, MVV);
        end;
        Result := Result + ')';
      end else begin
        Result := 'is null';
      end;
      Exit;
    end;

    // between
    if UpperCase(Operator) = 'BETWEEN' then
    begin
      if ParameterCount >= 2 then
      begin
        Result := 'between ' + Value(Trim(Parameters[0]), True, 1, MVV) + ' and ' + Value(Trim(Parameters[1]), True, 2, MVV);
      end else begin
        if ParameterCount <> 1 then
          Result := ''
        else begin
          Result := Value(Trim(Parameters[0]), True, 0, MVV);
          Result := Like(MVV) + ' ' + Result;
        end;
      end;
      Exit;
    end;

    // like
    if UpperCase(Operator) = 'LIKE' then
    begin
      for i := 0 to ParameterCount - 1 do
      begin
        V := Trim(Parameters[i]);
        if QBE.QBEDefinition.AllowFileWildcards then ConvertFileWildcards(V);
        V := Value(V, True, 0, MVV);
        if not HasSQLWildCards(V) then V := V + '%';
        if i > 0 then Result := Result + ' or ' + FieldName;
        Result := Result + ' ' + Like(MVV) + ' ' + V;
      end;
      Exit;
    end;

    // unknown
    if Operator <> '' then
    begin
      if SpaceField and AllowOperatorAdd and (ParameterCount > 0) then
        Parameters[0] := Operator + ' ' + Parameters[0]
      else
        InsertParameter(0, Operator);
      Operator := '';
    end;
    if ParameterCount = 1 then
    begin
      SingleParam := True;
      Result := Trim(Parameters[0]);
      if QBE.QBEDefinition.AllowFileWildcards then ConvertFileWildcards(Result);
      Result := Value(Result, False, 0, MVV);
      if Operator <> '' then
        Result := Operator + ' ' + Result
      else
        Result := Like(MVV) + ' ' + Result;
    end else begin
      if (ParameterCount = 3) and ((Parameters[1] = '..') or (Parameters[1] = '...') or (Parameters[1] = '-'))then
      begin
        Result := 'between ' + Value(Trim(Parameters[0]), True, 1, MVV) + ' and ' + Value(Trim(Parameters[2]), True, 2, MVV);
      end else begin
        for i := 0 to ParameterCount - 1 do
        begin
          V := Trim(Parameters[i]);
          if QBE.QBEDefinition.AllowFileWildcards then ConvertFileWildcards(V);
          V := Value(V, False, 0, MVV);
          if i > 0 then Result := Result + ' or ' + FieldName;
          Result := Result + ' ' + Like(MVV) + ' ' + V;
        end;
      end;
    end;
  finally
  end;
end;

// Add the QBE variables to the where-clause
procedure TOracleDataSet.AddQBEToQuery;
var f: Integer;
    FieldInfo: TOracleFieldInfo;
    WhereWord: widestring;
    WhereClause: widestring;
    AfterWhere: widestring;
    BeforeWhere: widestring;
    QBEWhere: widestring;
    cs, vs: widestring;
    DoNot: Boolean;
begin
  // Restore the old query..
  RemoveQBEFromQuery;
  // ..and save it
  QBEOldSQL := SQL.Text;
  // If the fields are gone, we need to re-describe the query
  if Query.FieldCount = 0 then Query.Describe;
  // Replace the where clause and add the variables
  SplitSelect(SubstitutedSQL, False, BeforeWhere, WhereClause, AfterWhere, WhereWord);
  QBEWhere := '';
  for f := 1 to FieldInfoList.Count - 1 do if FieldInfoList[f].QBEField <> nil then
  begin
    FieldInfo := FieldInfoList[f];
    if QBEDefinition.AllowOperators then
      vs := FieldInfo.QBEField.Value
    else begin
      if VarIsNull(FieldInfo.QBEField.LastValue) then
        vs := ''
      else
        vs := FieldInfo.QBEField.LastValue;
    end;

    if (vs <> '') then
    begin

      if QBEWhere <> '' then QBEWhere := QBEWhere + #13#10 + 'and ';
      cs := TrimRight(FieldInfo.Field.Origin);
      if cs = '' then
      begin
        cs := Trim(FieldInfo.Field.FieldName);
        if FieldInfo.InfoType = itAttr then cs := GetUpdatingAlias + '.' + cs;
      end;

      vs := AnalyzeExpression(vs, cs, DoNot, FieldInfo.Field.DataType, FieldInfo.QBEField, Self);

      if vs <> '' then
      begin
        if DoNot then
        begin
          QBEWhere := QBEWhere + '(not (' + cs + ' ' + vs + ')';
          if vs = 'is null' then
            QBEWhere := QBEWhere + ')'
          else
            if vs <> 'is null)' then QBEWhere := QBEWhere + ' or ' + cs + ' is null)'
        end else
          QBEWhere := QBEWhere + '(' + cs + ' ' + vs + ')';
      end;
    end;
  end;
  // Add the where clause
  if QBEWhere <> '' then
  begin
    FQBEModified := True;
    SQL.Clear;
    SQL.Add(BeforeWhere);
    if WhereClause <> '' then
      SQL.Add(WhereWord + ' (' + WhereClause + ')' + #13#10 + 'and' + #13#10 + '(' + QBEWhere + ')')
    else
      SQL.Add(WhereWord + ' ' + QBEWhere);
    SQL.Add(AfterWhere);
    SQL.Add(QBEComment);
  end;
end;

// Remove the QBE information from the query
procedure TOracleDataSet.RemoveQBEFromQuery;
var i: Integer;
    VarName: string;
begin
  FQBEModified := False;
  if FQuery = nil then Exit;
  for i := VariableCount - 1 downto 0 do
  begin
    VarName := VariableName(i);
    if copy(VarName, 1, 9) = ':DOA__QBE' then FQuery.DeleteVariable(VarName);
  end;
  i := FQuery.SQL.Count;
  if i <= 0 then Exit;
  if FQuery.SQL[i - 1] = QBEComment then FQuery.SQL.Text := QBEOldSQL;
end;

{$IFDEF NONVISUAL}

procedure TOracleDataSet.SetQBEColors;
begin
end;

{$ELSE}

procedure TOracleDataSet.SetQBEColors;
var i: Integer;
 procedure ResetControlLength(Index: Integer);
 var PropInfo: PPropInfo;
 begin
   PropInfo := TypInfo.GetPropInfo(TControl(QBEMaxLengthControlList[Index]).ClassInfo, 'MaxLength');
   if PropInfo <> nil then
   begin
     SetOrdProp(TControl(QBEMaxLengthControlList[Index]), PropInfo, Integer(QBEMaxLengthValueList[Index]));
   end;
 end;
 procedure SetControlLengths(C: TComponent);
 var i, D: Integer;
     PropInfo: PPropInfo;
 begin
   if C is TControl then
   begin
     PropInfo := TypInfo.GetPropInfo(C.ClassInfo, 'DataSource');
     if (PropInfo <> nil) then
     begin
       D := GetOrdProp(C, PropInfo);
       if (D <> 0) and (TDataSource(D).DataSet = Self) then
       begin
         PropInfo := TypInfo.GetPropInfo(C.ClassInfo, 'MaxLength');
         if PropInfo <> nil then
         begin
           i := GetOrdProp(C, PropInfo);
           SetOrdProp(C, PropInfo, 0);
           QBEMaxLengthControlList.Add(C);
           QBEMaxLengthValueList.Add(Pointer(i));
         end;
       end;
     end;
   end;
   for i := 0 to C.ComponentCount - 1 do SetControlLengths(C.Components[i]);
 end;
 procedure NotifyControls(C: TComponent);
 var i, D: Integer;
     PropInfo: PPropInfo;
     FN: string;
     QF: TQBEField;
     NoColor: Boolean;
 begin
   if C is TControl then
   begin
     // Only set font for controls that don't have a Caption
     PropInfo := TypInfo.GetPropInfo(C.ClassInfo, 'Caption');
     if PropInfo = nil then
     begin
       // Don't set the color if the field is not queryable
       NoColor := False;
       PropInfo := TypInfo.GetPropInfo(C.ClassInfo, 'DataField');
       if PropInfo <> nil then
       begin
         FN := GetStrProp(C, PropInfo);
         QF := QBEDefinition.FieldByName(FN);
         NoColor := (QF <> nil) and not QF.Queryable;
       end;
       PropInfo := TypInfo.GetPropInfo(C.ClassInfo, 'DataSource');
       if (PropInfo <> nil) and not NoColor then
       begin
         D := GetOrdProp(C, PropInfo);
         if (D <> 0) and (TDataSource(D).DataSet = Self) then
         begin
           if (QBEDefinition.QBEBackgroundColor <> clNone) and
              (TMyControl(C).Color = clWindow) then
           begin
             TMyControl(C).Color := QBEDefinition.QBEBackgroundColor;
             QBEBkgColorList.Add(C);
           end;
           if (QBEDefinition.QBEFontColor <> clNone) and
              (TMyControl(C).Font.Color = clWindowText) then
           begin
             TMyControl(C).Font.Color := QBEDefinition.QBEFontColor;
             QBEFontColorList.Add(C);
           end;
         end;
       end;
     end;
   end;
   for i := 0 to C.ComponentCount - 1 do NotifyControls(C.Components[i]);
 end;
begin
  if csDesigning in ComponentState then Exit;
  // Reset MaxLength of DBEdits
  if QBEDefinition.AllowOperators then
  begin
    if QBEMode then
    begin
      QBEMaxLengthControlList.Clear;
      QBEMaxLengthValueList.Clear;
      for i := 0 to ApplicationComponentCount - 1 do SetControlLengths(ApplicationComponents(i))
    end else begin
      for i := 0 to QBEMaxLengthControlList.Count - 1 do
      begin
        ResetControlLength(i);
      end;
    end;
  end;
  //
  if (QBEDefinition.QBEBackgroundColor = clNone) and
     (QBEDefinition.QBEFontColor = clNone) then Exit;

  if QBEMode then
  begin
    if (QBEFontColorList.Count = 0) and (QBEBkgColorList.Count = 0) then
    begin
      for i := 0 to ApplicationComponentCount - 1 do
        NotifyControls(ApplicationComponents(i));
    end;
  end else begin
    if QBEDefinition.QBEFontColor <> clNone then
      for i := 0 to QBEFontColorList.Count - 1 do
        TMyControl(QBEFontColorList[i]).Font.Color := clWindowText;
    if QBEDefinition.QBEBackgroundColor <> clNone then
      for i := 0 to QBEBkgColorList.Count - 1 do
        TMyControl(QBEBkgColorList[i]).Color := clWindow;
    QBEFontColorList.Clear;
    QBEBkgColorList.Clear;
  end;
end;

{$ENDIF}

procedure TOracleDataSet.DoAfterInsert;
begin
  if not QBEMode then
  begin
    inherited;
    CheckMasterFields;
  end;
end;

procedure TOracleDataSet.DoBeforeInsert;
begin
  if not QBEMode then
  begin
    inherited;
    EnableMasterFields(True);
  end;
end;

procedure TOracleDataSet.DoAfterCancel;
begin
  if not QBEMode then inherited;
end;

procedure TOracleDataSet.DoBeforeCancel;
begin
  if not QBEMode then inherited;
end;

// Reactivate a dataset if it is already active
procedure TOracleDataSet.Reactivate;
begin
  if Active then
  begin
    Active := False;
    Active := True;
  end;
end;

// Post all details before scolling. This disables a scroll on error.
procedure TOracleDataSet.DoBeforeScroll;
begin
  PostDetails;
  inherited;
end;

// On certain data events, we need to:
// - Set field properties according to dictionary information
// - Synchronize the details
procedure TOracleDataSet.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if (Event = deFieldListChange) and (ComponentState - IgnoreState = [csDesigning]) then
  begin
    SetFieldProperties;
  end;
  if NoDataEvents then Exit;
  // There's a terrible piece of code here, where we refresh once on a
  // deUpdateState event. This is needed to get an initial synchronisation of
  // the details.
  if FDetails.Count > 0 then
  begin
    if (Event = deUpdateState) and FirstSync then
      SyncDetails(False);
    if Event in [deDataSetChange, deDataSetScroll] then
    begin
      SyncDetails(False);
      FirstSync := False;
    end;
  end;
  inherited;
end;

// Return an error message translated by the OnTranslateMessage event
function TOracleDataSet.TranslatedMessage(Action: char; ErrorCode: Integer; const Msg: string): string;
var p1, p2, p3: Integer;
    MsgCopy: string;
    ConstraintName: string;
    TableMessage: string;
    ParentChild: string;
    MessageActions, MessageParentChild: string;
    Q: TOracleQuery;
begin
  Result := Msg;
  // Strip error stack from user defined error messages
  if (ErrorCode >= 20000) and (ErrorCode <= 20999) then
  begin
    p1 := Pos('ORA-06512', Msg);
    if p1 > 0 then Result := Copy(Msg, 1, p1 - 2);
  end;
  // If primary, unique, foreign or check constraint violation, extract
  // constraint name
  if (ErrorCode = 1) or (ErrorCode = 2290) or (ErrorCode = 2291) or (ErrorCode = 2292) then
  begin
    MsgCopy := Msg;
    ConstraintName := '';
    repeat
      p1 := Pos('(', MsgCopy);
      p2 := Pos(')', MsgCopy);
      if p1 > 0 then
      begin
        if p2 < p1 then p1 := p2 else ConstraintName := Copy(MsgCopy, p1 + 1, p2 - p1 - 1);
        p3 := Pos('.', ConstraintName);
        if p3 > 0 then
          ConstraintName := Copy(ConstraintName, p3 + 1, Length(ConstraintName))
        else begin
          ConstraintName := '';
          System.Delete(MsgCopy, 1, p2);
        end;
      end;
    until (ConstraintName <> '') or (p1 <= 0);
  end else
    ConstraintName := '';
  // Check if there this message must be translated through a message table
  if OracleDictionary.UseMessageTable and (Session.MessageTable <> '') and
    (ConstraintName <> '') then
  begin
    ParentChild := '*';
    if ErrorCode = 2291 then ParentChild := 'C';
    if ErrorCode = 2292 then ParentChild := 'P';
    Q := TOracleQuery.Create(nil);
    Q.Name := 'MessageQuery';
    Q.ParentMonID := MonID;
    Q.Session := Session;
    Q.SQL.Add('select actions, parent_child, error_message from ' + Session.MessageTable);
    Q.SQL.Add('where constraint_name = ''' + ConstraintName + '''');
    Q.SQL.Add('order by actions');
    try
      Q.Execute;
      TableMessage := '';
      while not Q.Eof do
      begin
        MessageActions := UpperCase(Q.Field('actions'));
        MessageParentChild := UpperCase(Q.Field('parent_child'));
        if ((MessageParentChild = '*') or (MessageParentChild = ParentChild)) then
          if (Pos(Action, MessageActions) > 0) or
             ((MessageActions = '*') and (TableMessage = '')) then
            TableMessage := Q.Field('error_message');
        Q.Next;
      end;
      if TableMessage <> '' then Result := TableMessage;
    except
      on E: Exception do Result := Result + '(' + E.Message + ')';
    end;
    Q.Free;
  end;
  // Call the event handler, if assigned
  if Assigned(FOnTranslateMessage) then
    OnTranslateMessage(Self, ErrorCode, ConstraintName, Action, Result);
end;

// Apply record by using the OnApplyRecord event
function TOracleDataSet.ApplyRecord(Action: Char; var NewRowId: string): Boolean;
begin
  Result := False;
  if Assigned(FOnApplyRecord) then FOnApplyRecord(Self, Action, Result, NewRowId);
end;

// Get the OracleType of a field
function TOracleDataSet.FieldOracleType(AField: TField): Integer;
begin
  result := Query.FieldType(FieldIndex(AField))
end;

// Check if the dataset is not detached
procedure TOracleDataSet.CheckAttached;
begin
  if Detached then DatabaseError('Operation not allowed for a detached dataset');
end;

// Can this dataset be modified?
function TOracleDataSet.GetCanModify: Boolean;
begin
  if QBEMode then
    Result := True
  else begin
    Result := Active and (not Detached) and
              ((FHasRowId or Assigned(FOnApplyRecord)) and not FReadOnly);
    if Result then
      Result := not ((FMaster <> nil) and
                    ((not FMaster.Active) or (FMaster.Records = nil) or
                     (FMaster.IsEmpty) or (FMaster.QBEMode)));
  end;
end;

// Create and open the Query used to perform Data Manipulation
procedure TOracleDataset.OpenDMLQuery(NoDebug: Boolean);
begin
  if DMLQuery = nil then
  begin
    SendToMonitor(True, True);
    DMLQuery := TOracleQuery.Create(Self);
    DMLQuery.Name := 'DMLQuery';
    DMLQuery.ParentMonID := MonID;
  end;
  DMLQuery.Debug := Debug and not NoDebug;
  DMLQuery.Optimize := Optimize;
  if FQuery <> nil then
  begin
    DMLQuery.Session := FQuery.Session;
    DMLQuery.Cursor  := FQuery.Cursor;
  end;
  DMLQuery.ReadBuffer := 2;
  DMLQuery.StringFieldsOnly := False;
end;

procedure TOracleDataSet.CloseDMLQuery;
begin
  if (not Optimize) or (Session = nil) or (Session.AreStatementsCached) then DMLQuery.Close;
end;

// Determine the alias of the updating table
function TOracleDataSet.GetUpdatingAlias: string;
var s, so, ss: string;
    i, FromPos, TablePos: Integer;
begin
  Result := GetUpdatingTable;
  // Find the from clause
  ss := RemoveSQLComment(SubstitutedSQL, False);
  so := RemoveParenthesisAndQuotes(ss);
  s := AnsiUpperCase(so);
  for i := 1 to Length(s) do if not (s[i] in Identifiers + ['(', ')', '''']) then s[i] := ' ';
  FromPos := Pos(' FROM ', s);
  if FromPos <= 0 then Exit;
  // Check for inline subquery
  i := FromPos + 6;
  while (i <= Length(so)) and (so[i] in Blanks) do Inc(i);
  if so[i] = '(' then
  begin
    while (i <= Length(so)) and (so[i] <> ')') do Inc(i);
    TablePos := i + 1;
  end else begin;
    // Find the updating table after the from clause
    System.Delete(s, 1, FromPos);
    TablePos := Pos(' ' + AnsiUpperCase(GetUpdatingTable) + ' ', s);
    if TablePos <= 0 then
      TablePos := Pos(AnsiUpperCase(GetUpdatingTable), AnsiUpperCase(ss)) - 1;
    if TablePos <= 0 then Exit;
    Inc(TablePos, FromPos + 1);
  end;
  // Check if there is an alias
  i := TablePos;
  while (i <= Length(so)) and not (so[i] in Blanks + [',']) do Inc(i);
  while (i <= Length(so)) and (so[i] in Blanks) do Inc(i);
  s := '';
  while (i <= Length(so)) and (so[i] in Identifiers) do
  begin
    s := s + so[i];
    Inc(i);
  end;
  if (s <> '') and (Pos('"' + UpperCase(s) + '"', '"WHERE", "ORDER", "GROUP", "FOR", "START", "CONNECT"') <= 0) then
    Result := s;
end;

function GetParenthesis(const s: WideString; StartPos: Integer): WideString;
var i, Level: Integer;
    QChar, c: WideChar;
begin
  Result := '';
  i := StartPos;
  while (i <= Length(s)) and WideCharIn(s[i], Blanks) do Inc(i);
  if (i <= Length(s)) and (s[i] = '(') then
  begin
    Level := 1;
    Inc(i);
    QChar := #0;
    while (i <= Length(s)) and (Level > 0) do
    begin
      c := s[i];
      if c = QChar then
        QChar := #0
      else begin
        if (c = '''') or (c = '"') then
          QChar := c;
      end;
      if QChar = #0 then
      begin
        if c = '(' then
          Inc(Level);
        if c = ')' then
          Dec(Level);
      end;
      Inc(i);
    end;
    if Level <= 0 then Result := Copy(s, StartPos, i - StartPos);
  end;
end;

// Copy the variables of the updating table expression from the dataset to the query
procedure TOracleDataSet.CopyUpdatingTableVariables(AQuery: TOracleQuery);
var Vars: TStringList;
    VarIndex, i: Integer;
    VarName: string;
begin
  Vars := FindVariables(GetUpdatingTable, False);
  for i := 0 to Vars.Count - 1 do
  begin
    VarName  := Vars[i];
    VarIndex := VariableIndex(VarName);
    if VarIndex >= 0 then
    begin
      AQuery.DeclareVariable(VarName, VariableType(VarIndex));
      AQuery.SetVariable(VarName, GetVariable(VarName));
    end;
  end;
  Vars.Free;
end;

// Is this a collection dataset?
function TOracleDataSet.IsCollectionDataSet: Boolean;
var s: string;
begin
  Result := False;
  if Master <> nil then
  begin
    s := Trim(AnsiUpperCase(GetUpdatingTable));
    Result := (s <> '') and (s[Length(s)] = ')') and
              ((Copy(s, 1, 5) = 'TABLE') or (Copy(s, 1, 3) = 'THE'));
  end;
end;

// Determine the Name & Type of the collection and embedding objects of a dataset
procedure TOracleDataSet.GetCollectionInfo;
var CollectionColumn, MasterTable: string;
    s, ss, ObjType: string;
    i, p1, p2, p3, a: Integer;
    FirstDot: Boolean;
    Obj: TOracleObject;
    Attr: TObjectAttribute;
begin
  if (FCollectionTypes = nil) and IsCollectionDataSet then
  begin
    FCollectionTypes := TStringList.Create;
    FCollectionNames := TStringList.Create;
    // Determine the collection column (remove alias)
    ss := Trim(GetUpdatingTable);
    s := AnsiUpperCase(ss);
    if (s = '') or (s[Length(s)] <> ')') then Exit;
    p1 := Pos('SELECT', s);
    p2 := Pos('FROM', s);
    p3 := Pos('WHERE', s);
    if (p1 <= 0) or (p2 <= p1) or (p3 <= p2) then Exit;
    Inc(p1, 6);
    CollectionColumn := FirstWord(Trim(Copy(ss, p1, p2 - p1)));
    // Determine the Master table (including alias)
    Inc(p2, 4);
    MasterTable := Trim(Copy(ss, p2, p3 - p2));
    // Create a select statement for the collection and all embedding objects
    FirstDot := True;
    s := '';
    for i := 1 to Length(CollectionColumn) + 1 do
    begin
      if (i = Length(CollectionColumn) + 1) or (CollectionColumn[i] = '.') then
      begin
        // The first dot marks the alias
        if FirstDot then
          FirstDot := False
        else begin
          FCollectionNames.Add(Copy(CollectionColumn, 1, i - 1));
          AddWithComma(FCollectionNames[FCollectionNames.Count -1], s, True);
        end;
      end;
    end;
    OpenDMLQuery(True);
    DMLQuery.Clear;
    DMLQuery.SQL.Text := 'select ' + s + ' from ' + MasterTable;
    // Describe the query to obtain all object types
    try
      DMLQuery.Describe;
      for i := 0 to DMLQuery.FieldCount - 1 do
      begin
        // Determine the default constructor
        ObjType := DMLQuery.FieldTypeName(i);
        s := '';
        if i < DMLQuery.FieldCount - 1 then
        begin
          Obj := nil;
          try
            Obj := TOracleObject._Create(DMLQuery.Session, ObjType, '', Self.MonID, DMLQuery.FieldName(i));
            for a := 0 to Obj.AttrCount - 1 do
            begin
              Attr := Obj.Attrs[a];
              if Pos('.', Attr.Name) <= 0 then AddWithComma('null', s, True);
            end;
          except
          end;
          try
            Obj.Free;
          except;
          end;
        end;
        ObjType := ObjType + '(' + s + ')';
        FCollectionTypes.Add(ObjType);
      end;
    except
    end;
    CloseDMLQuery;
  end;
end;

// Determine the name of the updating table, used by DML statements
function TOracleDataSet.GetUpdatingTable: string;
var s, s2, ssql: widestring;
    i, From: Integer;
    InlineQuery: Boolean;
    NestedTable: Boolean;
begin
  // Was it already determined?
  if FTableName <> '' then
  begin
    Result := FTableName;
    Exit;
  end;
  NestedTable := False;
  // UpdatingTable property overrules SQL Text
  if Trim(FUpdatingTable) <> '' then
    s := FUpdatingTable
  else begin
    Result := '';
    if FQuery = nil then Exit;
    // Can we determine the table name from the SQL text?
    if IsCursorDataSet then
      raise Exception.Create('UpdatingTable property required for cursor dataset');
    // The tablename is the first identifier after FROM
    ssql := SubstitutedSQL;
    s := AnsiUpperCase(RemoveSQLComment(SubstitutedSQL, False));
    for i := 1 to Length(s) do if not (WideCharIn(s[i], Identifiers + ['('])) then s[i] := ' ';
    repeat
      From := Pos(' FROM ', s);
      if From = 0 then From := Pos(' FROM(', s);
      if From = 0 then Exit;
      Inc(From, 5);
      while (From <= Length(s)) and (s[From] = ' ') do Inc(From);
      i := From;
      InlineQuery := (i <= Length(s)) and (s[i] = '(');
      if InlineQuery then
        System.Delete(s, 1, i)
      else begin
        while (i <= Length(s)) and WideCharIn(s[i], Identifiers) do Inc(i);
        s2 := Copy(s, From, i - From);
        if (AnsiCompareText(s2, 'TABLE') = 0) or (AnsiCompareText(s2, 'THE') = 0) then
        begin
          s2 := s2 + GetParenthesis(ssql, i);
          NestedTable := True;
        end;
        s := s2;
      end;
    until not InlineQuery;
  end;
  if NestedTable then
    Result := s
  else begin
    // Remove quotes and convert to uppercase
    Result := '';
    s := AnsiUpperCase(s);
    for i := 1 to Length(s) do if s[i] <> '"' then Result := Result + s[i];
  end;
  FTableName := Result;
end;

// Is one or more fields a calculated field?
function TOracleDataSet.CalcFieldInList(const List: string): Boolean;
var i: Integer;
    Fields: TList;
begin
  if Pos(';', List) <> 0 then
  begin
    Result := False;
    Fields := TList.Create;
    try
      GetFieldList(Fields, List);
      for i := 0 to Fields.Count - 1 do
        if TField(Fields[I]).FieldKind in [fkCalculated, fkLookup] then Result := True;
    finally
      Fields.Free;
    end;
  end else
    Result := (FieldByName(List).FieldKind in [fkCalculated, fkLookup]);
end;

// Search a record in a dataset
function TOracleDataSet.SearchRecord(const FieldNames: string; const FieldValues: Variant;
      Options: TSearchRecordOptions): Boolean;
type TLIntArray = array[0..9999] of Integer;
     TLFloatArray = array[0..9999] of Double;
     TLDateArray = array[0..9999] of TDateTime;
     TLNullArray = array[0..9999] of Boolean;
     TLTypeArray = array[0..9999] of TFieldType;
var IntArray: ^TLIntArray;
    FloatArray: ^TLFloatArray;
    DateArray: ^TLDateArray;
    StringArray: TStringList;
    NullArray: ^TLNullArray;
    TypeArray: ^TLTypeArray;
    FieldList: TList;
    Field: TField;
    i, fc, f, ValueLo, ValueHi, ValueCount: Integer;
    Value: Variant;
    MustCalc, Ready, Match: Boolean;
    RecNo, Increment: Integer;
    OldState: TDataSetState;
    SearchRowId: Boolean;
    UsedType: TFieldType;
    s: string;
begin
  // Only on an active dataset
  CheckActive;
  CheckBrowseMode;
  // First check for some invalid combinations of options
  if ((srFromBeginning in Options) and (srFromEnd in Options)) or
     ((srFromBeginning in Options) and (srFromCurrent in Options)) or
     ((srFromEnd in Options) and (srFromCurrent in Options)) or
     ((srForward in Options) and (srBackward in Options)) or
     ((srForward in Options) and (srFromEnd in Options)) or
     ((srBackward in Options) and (srFromBeginning in Options)) then
    raise Exception.Create(Name + '.SearchRecord options invalid');
  Result := False;
  // Determine the Fields
  SearchRowId := (UpperCase(FieldNames) = 'ROWID');
  if SearchRowId and not FHasRowId then
    raise Exception.Create('You cannot search a rowid if it is not included in the select list');
  FieldList := TList.Create;
  try
    if SearchRowid then
      FieldList.Add(nil)
    else
      GetFieldList(FieldList, FieldNames);
  except
    FieldList.Free;
    raise;
  end;
  fc := FieldList.Count;
  Match := False;
  GetMem(IntArray, SizeOf(Integer) * fc);
  GetMem(FloatArray, Sizeof(Double) * fc);
  GetMem(DateArray, SizeOf(TDateTime) * fc);
  StringArray := TStringList.Create;
  for i := 0 to fc - 1 do StringArray.Add('');
  GetMem(NullArray, SizeOf(Boolean) * fc);
  GetMem(TypeArray, SizeOf(TFieldType) * fc);
  OldState := SetTempState(dsFilter);
  SetCursorShape;
  try
    // Check the values
    if not VarIsArray(FieldValues) then
      ValueCount := 1
    else begin
      ValueLo := VarArrayLowBound(FieldValues, 1);
      ValueHi := VarArrayHighBound(FieldValues, 1);
      ValueCount := ValueHi - ValueLo + 1;
    end;
    if ValueCount <> FieldList.Count then
      raise Exception.Create(Name + '.SearchRecord: number of fields and values do not match');
    MustCalc := False;
    // Determine the field values as non-variants
    for f := 0 to FieldList.Count - 1 do
    begin
      if VarIsArray(FieldValues) then Value := FieldValues[f] else Value := FieldValues;
      if SearchRowId then
      begin
        TypeArray[0] := ftString;
        StringArray[0] := Value;
      end else begin
        Field := FieldList[f];
        // If one or more fields are calculated, GetCalcFields must be called for each record
        if Field.FieldKind in [fkCalculated, fkLookup] then MustCalc := True;
        NullArray[f] := VarIsNull(Value) or VarIsEmpty(Value);
        if not NullArray[f] then
        begin
          if (srWildCards in Options) or (srPartialMatch in Options) then
            UsedType := ftString
          else
            UsedType := Field.DataType;
          case UsedType of
            ftInteger, ftSmallInt, ftWord:
            begin
              TypeArray[f] := ftInteger;
              IntArray[f] := Value;
            end;
            ftFloat, ftCurrency, ftBCD:
            begin
              TypeArray[f] := ftFloat;
              FloatArray[f] := Value;
            end;
            ftDate, ftTime, ftDateTime:
            begin
              TypeArray[f] := ftDateTime;
              if (VarType(Value) = varString) or (VarType(Value) = varOleStr) then
                DateArray[f] := StrToDateTime(Value)
              else
                DateArray[f] := Value;
              if srIgnoreTime in Options then DateArray[f] := Trunc(DateArray[f]);
            end;
          else
            begin
              TypeArray[f] := ftString;
              if srIgnoreCase in Options then
                StringArray[f] := TrimStringS(AnsiUpperCase(Value))
              else
                StringArray[f] := TrimStringS(Value);
              if StringArray[f] = '' then NullArray[f] := True;
            end;
          end;
        end;
      end;
    end;
    // Determine where to start and in which direction to go
    RecNo := FCurRec;
    Increment := 1;
    if srFromBeginning in Options then RecNo := -1;
    if srFromEnd in Options then
    begin
      while QueryRecord do ; // Last
      RecNo := Records.RecordCount;
      Increment := -1;
    end;
    if srForward in Options then Increment := 1;
    if srBackward in Options then Increment := -1;
    // Search records
    Ready := False;
    Match := False;
    while not Ready do
    begin
      Inc(RecNo, Increment);
      if (Increment = -1) and (RecNo < 0) then Ready := True;
      if (Increment = 1) and (RecNo >= Records.RecordCount) then Ready := not QueryRecord;
      if not Ready then
      begin
        FFilterBuffer := Records.RecordAt(RecNo);
        if SearchRowId then
          Match := StringArray[0] = GetRowId(FFilterBuffer)
        else begin
          if MustCalc then GetCalcFields(PChar(FFilterBuffer));
          Match := True;
          f := 0;
          while Match and (f <= FieldList.Count - 1) do
          begin
            Field := FieldList[f];
            if NullArray[f] then
              Match := NullField(Field)
            else begin
              case TypeArray[f] of
                ftInteger: Match := (IntArray[f] = Field.AsInteger);
                ftFloat: Match := (FloatArray[f] = Field.AsFloat);
                ftDateTime: begin
                              if srIgnoreTime in Options then
                                Match := (DateArray[f] = Trunc(Field.AsDateTime))
                              else
                                Match := (DateArray[f] = Field.AsDateTime);
                            end;
              else
                begin
                  if srIgnoreCase in Options then
                    s := AnsiUpperCase(TrimStringS(Field.AsString))
                  else
                    s := TrimStringS(Field.AsString);
                  if srPartialMatch in Options then
                    s := Copy(s, 1, Length(StringArray[f]));
                  if srWildcards in Options then
                    Match := WildCardCompare(StringArray[f], s)
                  else
                    Match := (stringArray[f] = s);
                end;
              end;
            end;
            Inc(f);
          end;
        end;
        if Match then Ready := True;
      end;
    end;
  finally
    FreeMem(IntArray);
    FreeMem(FloatArray);
    FreeMem(DateArray);
    StringArray.Free;
    FreeMem(NullArray);
    FreeMem(TypeArray);
    RestoreState(OldState);
    FieldList.Free;
    RestoreCursorShape;
  end;
  if Match then
  begin
    if not (srDontNavigate in Options) then GotoBookMark(@FFilterBuffer.Bookmark);
    Result := True;
  end;
end;

function TOracleDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var OldState: TDataSetState;
begin
  Result := Null;
  if SearchRecord(KeyFields, KeyValues, [srFromBeginning, srDontNavigate]) then
  begin
    if CalcFieldInList(ResultFields) then GetCalcFields(PChar(FFilterBuffer));
    OldState := SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(OldState);
    end;
  end;
end;

function TOracleDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var SearchOptions: TSearchRecordOptions;
begin
  SearchOptions := [srFromBeginning];
  if loCaseInsensitive in Options then SearchOptions := SearchOptions + [srIgnoreCase];
  if loPartialKey in Options then SearchOptions := SearchOptions + [srPartialMatch];
  Result := SearchRecord(KeyFields, KeyValues, SearchOptions);
end;

// Resolve the name of a table, translating synonyms if necessary
procedure TOracleDataSet.ResolveName(const ObjectName: string; var TableOwner, TableName: string);
var p: Integer;
begin
  p := pos('.', ObjectName);
  if p > 0 then
  begin
    TableOwner := AnsiUpperCase(Copy(ObjectName, 1, p - 1));
    TableName  := AnsiUpperCase(Copy(ObjectName, p + 1, Length(ObjectName) - p));
    Exit;
  end;
  TableName  := '';
  TableOwner := '';
  if ObjectName = '' then Exit;
  OpenDMLQuery(True);
  with DMLQuery do
  try
    // For POLite compatibility use all_tables and all_synonyms and
    // 3 queries instead of 1 PL/SQL block
    Clear;
    DeclareVariable('name', otString);
    SetVariable('name', AnsiUpperCase(ObjectName));
    if not Session.POLite then
    begin
      DeclareVariable('table_owner', otString);
      DeclareVariable('table_name', otString);
      SQL.Add('begin');
      SQL.Add('  begin');
      SQL.Add('    select user, table_name into :table_owner, :table_name');
      SQL.Add('      from sys.user_tables where table_name = :name;');
      SQL.Add('  exception when no_data_found then');
      SQL.Add('    begin');
      SQL.Add('      select user, view_name into :table_owner, :table_name');
      SQL.Add('        from sys.user_views where view_name = :name;');
      SQL.Add('    exception when no_data_found then');
      SQL.Add('      begin');
      SQL.Add('        select table_owner, table_name into :table_owner, :table_name');
      SQL.Add('          from sys.user_synonyms where synonym_name = :name;');
      SQL.Add('      exception when no_data_found then');
      SQL.Add('        begin');
      SQL.Add('          select table_owner, table_name into :table_owner, :table_name');
      SQL.Add('            from sys.all_synonyms where owner = ''PUBLIC'' and synonym_name = :name;');
      SQL.Add('        exception when no_data_found then');
      SQL.Add('          :table_owner := null; :table_name := null;');
      SQL.Add('        end;');
      SQL.Add('      end;');
      SQL.Add('    end;');
      SQL.Add('  end;');
      SQL.Add('end;');
      Execute;
      TableName  := GetVariable('table_name');
      TableOwner := GetVariable('table_owner');
    end else begin
      // A table owned by the user?
      SQL.Add('select owner, table_name from all_tables');
      SQL.Add('where owner = user and table_name = :name');
      Execute;
      if EOF then
      begin
        // A private synonym for the user?
        SQL.Clear;
        SQL.Add('select table_owner, table_name from all_synonyms');
        SQL.Add('where owner = user and synonym_name = :name;');
        Execute;
        if EOF then
        begin
          // A public synonym?
          SQL.Clear;
          SQL.Add('select table_owner, table_name from all_synonyms');
          SQL.Add('where owner = ''PUBLIC'' and synonym_name = :name;');
          Execute;
        end;
      end;
      if not EOF then
      begin
        TableOwner := FieldAsString(0);
        TableName  := FieldAsString(1);
      end;
    end;
  except
    raise; // ??????????????
  end;
  CloseDMLQuery;
end;

// Determine if the value for a field is generated
function TOracleDataSet.FieldIsGenerated(const AField: string): Boolean;
var f: Integer;
    df: TList;
begin
  Result := True;
  // Is it a detail field?
  if FMaster <> nil then
  begin
    df := TList.Create;
    GetFieldList(df, DetailFields);
    for f := 0 to df.Count - 1 do
    begin
      if AnsiStrIComp(PChar(string(AField)), PChar(string(TField(df[f]).FieldName))) = 0 then
      begin
        df.Free;
        Exit;
      end;
    end;
    df.Free;
   end;
  // Is it a sequence field?
  if SequenceField.UsesField(AField) then Exit;
  // None of the above
  Result := False;
end;

// Check constraints for a field or record (Field = nil)
procedure TOracleDataSet.CheckConstraints(Field: TField);
var T: TOracleTableInfo;
    C: TOracleConstraintInfo;
    F, FocusField: TField;
    CanCheck, NeedToCheck, FieldChanged: Boolean;
    i, ci: Integer;
begin
  // Do constraints need to be enforced?
  if (not FOracleDictionary.FEnforceConstraints) or (not FHasRowId) then Exit;
  // Is it for a single field?
  if Field <> nil then
  begin
    // Only data fields need to be checked
    if Field.FieldKind <> fkData then Exit;
    // A generated field is never checked
    if FieldIsGenerated(Field.FieldName) then Exit;
    // A null value is always okay
    if NullField(Field) then Exit;
  end;
  // Loop through all constraints, check if necessary and possible
  T := GetOracleTableInfo;
  T.GetConstraints(Self);
  for i := 0 to T.OracleConstraintInfoList.Count - 1 do
  begin
    C := T.OracleConstraintInfoList.Items[i] as TOracleConstraintInfo;
    C.ReadFromDictionary(Self);
    // Is this constraint disabled for the dataset?
    if OracleDictionary.DisabledConstraints.IndexOf(C.ConstraintName) < 0 then
    begin
      if Field <> nil then
      begin
        if Field.FieldName = C.SingleColumn(Self) then C.Check(Self);
      end else begin
        NeedToCheck := False;
        CanCheck := True;
        FocusField := nil;
        for ci := 0 to C.ColumnNames.Count - 1 do
        begin
          F := FindField(C.ColumnNames[ci]);
          if F <> nil then
          begin
            // If this field is going to be generated by a sequence on the server,
            // we can't check the constraint
            if SequenceField.UsesField(F.FieldName) and
              (SequenceField.ApplyMoment = amOnServer) and
              NullField(F) then CanCheck := False;
            // If this is a non-data field, it can't be checked
            if F.FieldKind <> fkData then CanCheck := False;
            // If the constraint is for just this field, it's already validated
            if F.FieldName = C.SingleColumn(Self) then CanCheck := False;
            // We only need to check if any field of the constraint is changed by the user
            FieldChanged := ((State = dsInsert) and not (NullField(F)) or
                            ((State <> dsInsert) and (F.NewValue <> F.Value)));
            if FieldChanged and not FieldIsGenerated(F.FieldName) then
            begin
              if not NeedToCheck then FocusField := F;
              NeedToCheck := True;
            end;
            // If one or more columns are null for a FK constraint, we cannot check
            if (C.ConstraintType = ctForeignKey) and NullField(F) then CanCheck := False;
          end;
        end;
        if CanCheck and NeedToCheck then
        try
          C.Check(Self);
        except
          if FocusField <> nil then FocusField.FocusControl;
          raise;
        end;
      end;
    end;
  end;
end;

// Determine the primary or unique key columns for POLite compatibilty
procedure TOracleDataSet.GetIdColumns(List: TStrings);
var TableOwner, TableName: string;
    Okay: Boolean;
    uf: TList;
    i: Integer;
    KeyFields: TStringList;
begin
  // Only if DataSet can be updated
  if FHasRowid then
  begin
    Okay := True;
    List.Clear;
    // If there unique fields are defined, use them
    if TrimRight(FUniqueFields) <> '' then
    begin
      uf := TList.Create;
      try
        GetFieldList(uf, FUniqueFields);
        for i := 0 to uf.Count - 1 do List.Add(TField(uf[i]).FieldName);
      finally
        uf.Free;
      end;
      Exit;
    end;
    // Determine the owner and name of the updating table
    ResolveName(GetUpdatingTable, TableOwner, TableName);
    // If the tablename cannot be determined, forget it
    if TableName = '' then
      Okay := False
    else begin
      // Open a query to select primary/unique constraint columns for this table
      OpenDMLQuery(True);
      KeyFields := GetPKFields(DMLQuery, TableOwner, TableName, True);
      if KeyFields = nil then
        Okay := False
      else begin
        List.Assign(KeyFields);
        KeyFields.Free;
      end;
      CloseDMLQuery;
    end;
    if not Okay then
      DatabaseError('Primary or Unique key columns must be included');
  end;
end;

// Fetch the rowid by primary/unique key
function TOracleDataSet.FetchRowId(UseOldValues: Boolean): string;
var i: Integer;
    fn, s: string;
    f: TField;
    v: Variant;
begin
  Result := '';
  if (IdColumns <> nil) and (IdColumns.Count > 0) then
  begin
    OpenDMLQuery(True);
    with DMLQuery do
    try
      Clear;
      SQL.Add('select rowid from ' + GetUpdatingTable + ' where');
      for i := 0 to IdColumns.Count - 1 do
      begin
        fn := IdColumns[i];
        f := FindField(fn);
        if f = nil then Exit;
//        if Self.State = dsInsert then v := f.Value else v := f.OldValue;
        if UseOldValues then v := f.OldValue else v := f.Value;
        if i > 0 then s := 'and ' + fn else s := fn;
        if VarIsNull(v) or ((f.DataType in [ftString, ftMemo, ftWideString]) and (v = '')) then
          s := s + ' is null'
        else begin
          DeclareVariable(fn, FieldOracleType(f));
          SetVariable(fn, v);
          s := s + ' = :' + fn;
        end;
        SQL.Add(s);
      end;
      Execute;
      if not EOF then
        Result := FieldAsString(0)
      else
        raise Exception.Create('Error determining ROWID: no record found');
      Next;
      if not EOF then
        raise Exception.Create('Error determining ROWID: more than one record found');
    except
      raise;
    end;
    CloseDMLQuery;
  end;
end;

// Does the dataset have any fields with a default value?
function TOracleDataSet.HasDefaults: Boolean;
var f: Integer;
begin
  Result := True;
  for f := 0 to FieldCount - 1 do
    if Trim(Fields[f].DefaultExpression) <> '' then Exit;
  Result := False;
end;

// Does the dataset have any blob fields?
function TOracleDataSet.HasBlobs: Boolean;
var f: Integer;
begin
  Result := True;
  for f := 0 to FieldCount - 1 do if Fields[f].IsBlob then Exit;
  Result := False;
end;

procedure TOracleDataSet.SetExternalCursor(Value: TOracleQuery);
begin
  if Value = FExternalCursor then Exit;
  FExternalCursor := Value;
  CursorCreated := False; 
end;

// Determine the query to fetch the records
procedure TOracleDataSet.GetQuery;
var i: Integer;
begin
  SendToMonitor(True, True);
  Query := FQuery;
  Query.ParentMonID := MonID;
  if ExternalCursor <> nil then
  begin
    CursorQuery := ExternalCursor;
    Query := CursorQuery;
    CursorValid := True;
  end else begin
    for i := 0 to FQuery.Variables.Count - 1 do with FQuery.Variables.Data(i) do
    begin
      if buftype = otCursor then
      begin
        if CursorQuery = nil then
        begin
          CursorQuery := TOracleQuery.Create(nil);
          CursorCreated := True;
          CursorQuery.Name := 'CursorQuery';
          CursorQuery.ParentMonID := MonID;
        end;
        CursorQuery.ReadBuffer       := FQuery.ReadBuffer;
        CursorQuery.Session          := FQuery.Session;
        CursorQuery.Optimize         := FQuery.Optimize;
        CursorQuery.Cursor           := FQuery.Cursor;
        CursorQuery.StringFieldsOnly := FQuery.StringFieldsOnly;
        FQuery.SetComplexVariable(Name, CursorQuery);
        Query := CursorQuery;
        FQuery.Execute;
        CursorValid := True;
        Break;
      end;
    end;
  end;
end;

// Determine if the dataset is based on a cursor
function TOracleDataSet.IsCursorDataSet: Boolean;
var v: Integer;
begin
  for v := 0 to VariableCount - 1 do
  begin
    if VariableType(v) = otCursor then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TOracleDataSet.Commit(ForceCommit: Boolean);
begin
  if LogChanges then Exit;
  if not CommitOnPost then Exit;
  if Session.MTSEnlisted then Exit;
  if ForceCommit or (not Session.AutoCommit) then
  begin
    if MonitorListening then SendStartActivity('Session.Commit', nil);
    try
      Session.InternalCommit;
    except
      on E: Exception do
      begin
        if MonitorPresent then SendEndActivity('', E.Message, nil);
        raise;
      end;
    end;
    if MonitorPresent then SendEndActivity('', '', nil);
  end;
end;

procedure TOracleDataSet.Rollback;
begin
  if LogChanges then Exit;
  if Session.MTSEnlisted then Exit;
  if CommitOnPost then
  begin
    if MonitorListening then SendStartActivity('Session.Rollback', nil);
    try
      Session.InternalRollback;
    except
      on E: Exception do
      begin
        if MonitorPresent then SendEndActivity('', E.Message, nil);
        raise;
      end;
    end;
    if MonitorPresent then SendEndActivity('', '', nil);
  end;
end;

procedure TOracleDataSet.Lockpoint;
begin
  if LogChanges then Exit;
  if not ((Session.POLite and not Session.POLite35) or Session.AutoCommit) then
  begin
    OpenDMLQuery(False);
    DMLQuery.Clear;
    DMLQuery.SQL.Text := 'savepoint DOA__LOCKPOINT';
    DMLQuery.Execute;
  end;
//  Session.Savepoint('DOA__LOCKPOINT');
end;

procedure TOracleDataSet.RollbackToLockpoint;
begin
  if LogChanges then Exit;
  if not ((Session.POLite and not Session.POLite35) or Session.AutoCommit) then
  begin
    OpenDMLQuery(False);
    DMLQuery.Clear;
    DMLQuery.SQL.Text := 'rollback to savepoint DOA__LOCKPOINT';
    DMLQuery.Execute;
//    Session.RollbackToSavepoint('DOA__LOCKPOINT');
  end else begin
    if CommitOnPost then Session.Rollback;
  end;
end;

procedure TOracleDataSet.CreateBlobList(var ABlobList: TBlobList);
var f: Integer;
    OracleBlob: TOracleBlob;
begin
  for f := 0 to FieldCount - 1 do if Fields[f].FieldKind in [fkData, fkInternalCalc] then
  begin
    if Fields[f].IsBlob then
    begin
      if ABlobList = nil then ABlobList := TBlobList.Create(TOracleBlob);
      OracleBlob := TOracleBlob.Create(ABlobList);
      OracleBlob.Field := Fields[f] as TBlobField;
    end;
  end;
end;

function FindRowIdIndex(Query: TOracleQuery): Integer;
var f: Integer;
begin
  Result := Query.FieldIndex('ROWID');
  // If there is no 'ROWID', check for a rowid type
  if Result < 0 then
  begin
    for f := 0 to Query.FieldCount - 1 do
    begin
      if Query.FieldDBType(f) in [otRowId, otRowidDesc] then
      begin
        Result := f;
        Break;
      end;
    end;
  end;
end;

// This method is called by TDataSet.Open and also when FieldDefs need to
// be updated (usually by the DataSet designer).  Everything which is
// allocated or initialized in this method should also be freed or
// uninitialized in the InternalClose method.
procedure TOracleDataset.InternalOpen;
begin
  Activating := True;
  try
    {$IFDEF CompilerVersion5}
    if (FMaster <> nil) and (FMaster.ProviderDataSet) then ProviderDataSet := True;
    {$ENDIF}
    SendToMonitor(True, True);
    // Initialize our internal position.
    // We use -1 to indicate the "crack" before the first record.
    FCurRec := -1;
    // Tell TDataSet how big our Bookmarks are
    BookmarkSize := SizeOf(Integer);
    // Reset compression info
    CompressedBytes := 0;
    DecompressedBytes := 0;
    // Create field definitions
    InternalInitFieldDefs;
    // Create TField components when no persistent fields have been created
    if DefaultFields then CreateFields;

    // Added by Quest
    {$IFDEF CompilerVersion4}
    // Create fields that are retrieved, but not defined by the user.  Added by Bruce C. Quest Software
    if FCreateAllFields then  CreateUndefinedFields;
    // Syncronize the String TFields sizes to the physical fields.  Added by Bruce C. Quest Software
    if FSyncFieldSizes then SyncStringFieldSizes;
    {$ENDIF}

    // Bind the TField components to the physical fields
    BindFields(True);
    // Determine the extra field info
    SetOracleFieldInfo;
    // Create the work blob list
    CreateBlobList(WorkBlobs);
    // Create collection for records
    Records := TRecordDataList.Create(Self);
    Records.Unidirectional := Unidirectional;
    // Check if cached updates are possible
    if CachedUpdates and (not (csDesigning in ComponentState)) and (not FHasRowId) then
      raise Exception.Create('CachedUpdates requires a rowid!');
    // Reset the MasterBookmark
    MasterBookmark := -1;
    FirstSync := True;
    // Calculate the size of the record buffers.
    // Note: This is NOT the same as the RecordSize property which
    // only gets the size of the data in the record buffer
    FRecBufSize := GetRecBufSize;
    // Get primary/unique key columns
    if (Session.POLite) or (TrimRight(FUniqueFields) <> '') then GetIdColumns(IdColumns);
    // If necessary, create the filter
    if Filter <> '' then
    begin
      if FilterExpr <> nil then
      begin
        FilterExpr.Free;
        FilterExpr := nil;
      end;
      FilterExpr := CreateExpression(Filter, GetColumnExpr, FilterOptions);
    end;
    // Go to QBEMode or query the records
    if not QBEMode then
      QueryRecords
    else begin
      if not DontEnterQBE then EnterQBEMode(False); //SetQBEColors;
    end;
    // Indicate that we need to count again
    Counted := False;
    RowsAdded := 0;
  finally
    Activating := False;
  end;
end;

function TOracleDataSet.DoBLOBCompress(Field: TBLOBField; var Buffer: Pointer; var Size: Integer): Boolean;
var InStream, OutStream: TMemoryStream;
begin
  Result := False;
  if (not CompressBLOBs) or (Size = 0) then Exit;
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    // Set the input stream data
    InStream.Write(Buffer^, Size);
    InStream.Seek(0, soFrombeginning);
    if not Assigned(FOnCompressBLOB) then
    begin
      // Write the magic values
      OutStream.Write(CompressionMAgicValue1, SizeOf(CompressionMAgicValue1));
      OutStream.Write(CompressionMAgicValue2, SizeOf(CompressionMAgicValue2));
      // Compress it
      LZHStreamPack(InStream, OutStream);
    end else begin
      FOnCompressBLOB(Self, Field, InStream, OutStream);
    end;
    if (OutStream.Size <> 0) then
    begin
      // Replace the buffer with the compressed data
      DecompressedBytes := DecompressedBytes + Size;
      GetMem(Buffer, OutStream.Size);
      Move(OutStream.Memory^, Buffer^, OutStream.Size);
      Size := OutStream.Size;
      CompressedBytes := CompressedBytes + Size;
      Result := True;
    end;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

function TOracleDataSet.DoBLOBDecompress(Field: TBLOBField; var Buffer: Pointer; var Size: Integer; FreeCompressed: Boolean): Boolean;
var InStream, OutStream: TMemoryStream;
    DOACompressed: Boolean;
begin
  Result := False;
  if Size = 0 then Exit;
  DOACompressed := (Size >= CompressionMagicValueSize) and
                   (PIntArray(Buffer)[0] = CompressionMagicValue1) and
                   (PIntArray(Buffer)[1] = CompressionMagicValue2);
  if DOACompressed or (CompressBLOBs and Assigned(FOnDecompressBLOB)) then
  begin
    InStream := TMemoryStream.Create;
    OutStream := TMemoryStream.Create;
    try
      // Set the input stream data
      InStream.Write(Buffer^, Size);
      InStream.Seek(0, soFromBeginning);
      // Decompress it
      if DOACompressed then
      begin
        InStream.Seek(CompressionMagicValueSize, soFromBeginning);
        LZHStreamUnpack(InStream, OutStream);
      end else begin
        if Assigned(FOnDecompressBLOB) then OnDecompressBLOB(Self, Field, InStream, OutStream);
      end;
      if OutStream.Size > 0 then
      begin
        // Replace the buffer with the decompressed data
        if FreeCompressed then FreeMem(Buffer, Size);
        GetMem(Buffer, OutStream.Size);
        Move(OutStream.Memory^, Buffer^, OutStream.Size);
        Size := OutStream.Size;
        Result := True;
      end;
    finally
      InStream.Free;
      OutStream.Free;
    end;
  end;
end;

// Query current record into the collection
function TOracleDataSet.QueryRecord: Boolean;
var i, f, fi: Integer;
    Buffer: Pointer;
    ps: array[0..AbsMaxVarchar * 2] of char;
    bd: TDateTime;
    bi: Integer;
    bf: Double;
    TimeStamp: TTimeStamp;
    FieldIsNull, FilterAccept: Boolean;
    BlobVariant: Variant;
    OracleBlob: TOracleBlob;
    s: string;
    ws: widestring;
    Log: TChangeLogItem;
    Cached: Boolean;
    Deleted: Boolean;
    Data: PRecordData;
    NoMoreLogs: Boolean;
    afAction: TAfterFetchRecordAction;
    OldFCurRec: Integer;
    FieldInfo: TOracleFieldInfo;
    Obj: TOracleObject;
    Attr: TObjectAttribute;
    IsField, IsAttr: Boolean;
    XML: TXMLType;
begin
  FilterAccept := False;
  if QBEMode then
  begin
    Result := False;
    Exit;
  end;
  // Retrieve current record and place it in the dataset
  repeat
    repeat
      NoMoreLogs := True;
      FFilterBuffer := Records.FirstFreeRecord;
      FFilterBuffer.ArraySize := FRecBufSize - RecInfoSize;
      FFilterBuffer.BlobList := nil;
      FreeRowId(FFilterBuffer);
      Log := nil;
      // Try to find a cached insert for this position
      if CachedUpdates then
      begin
        if not Query.EOF then
        begin
          // Find the cached insert for this index
          Log := FindChangeLogByIndex(Records.Count) as TChangeLogItem;
          if (Log <> nil) and ((Log.Action <> 'I') or Log.Visible) then Log := nil;
        end else begin
          // Find any cached insert at the end of the dataset
          for i := 0 to ChangeLog.Count - 1 do
          begin
            Log := ChangeLog.Items[i] as TChangeLogItem;
            if (Log.Action = 'I') and (not Log.Visible) and (Log.InDataSet) then Break;
            Log := nil;
          end;
          if Log = nil then
          begin
            Result := False;
            Exit;
          end;
        end;
        if Log <> nil then
        begin
          NoMoreLogs := False;
          Data := Records.NewRecord(-1, False);
          Log.GetAfterData(Data);
          Data.BookmarkFlag := bfCurrent;
          Inc(FLastBookMark);
          Data.Bookmark := FLastBookmark;
          Log.Bookmark := Data.Bookmark;
          Log.RecordIndex := Records.Count - 1;
          Log.Visible := True;
          FilterAccept := True;
          if Filtered then
          begin
            FilterAccept := ApplyFilterTo(Data);
            if FilterAccept then Records.AddToFilteredList(Data, True);
          end;
          if FilterAccept then
          begin
            if (not Filtered) and (Records.Count = 1) then CheckMasterFields;
            if (Filtered) and (Records.FilteredList.Count = 1) then CheckMasterFields;
            Result := True;
            Exit;
          end;
        end;
      end;
    until NoMoreLogs;
    if MasterLess then
    begin
      Result := False;
      Exit;
    end;
    if not FirstFetch then Query.Next;
    FirstFetch := False;
    if Query.Eof and not CachedUpdates then
    begin
      Result := False;
      Exit;
    end;
    if not Query.Eof then
    begin
      // Check if this record has a cached update
      Cached := False;
      Deleted := False;
      if CachedUpdates and (RowIdIndex >= 0) then
      begin
        Log := FindChangeLogByRowId(Query.FieldAsString(RowIdIndex)) as TChangeLogItem;
        if Log <> nil then
        begin
          // A cached update exists, in case of a delete: skip it, in case of
          // an update: use the updated version
          Cached := True;
          Log.Visible := True;
          if Log.Action = 'D' then
          begin
            Deleted := True;
            Log.Visible := True;
            Inc(FLastBookmark);
            Log.Bookmark := FLastBookmark;
          end else begin
            Log.GetAfterData(FFilterBuffer);
          end;
        end;
      end;
      // If the record is not cached, build a record buffer from the queried record
      if not Cached then
      begin
        for f := 0 to FieldCount - 1 do if Fields[f].FieldKind in [fkData, fkInternalCalc] then
        begin
          FieldInfo := FieldInfoList.ForField(Fields[f]);
          OracleBlob := nil;
          if Fields[f].IsBlob then
          begin
            if FFilterBuffer^.BlobList = nil then
            begin
              FFilterBuffer^.BlobList := TBlobList.Create(TOracleBlob);
            end;
            OracleBlob := TOracleBlob.Create(FFilterBuffer^.BlobList);
            OracleBlob.Field := Fields[f] as TBlobField;
          end;
          fi := FieldIndex(Fields[f]);
          IsField := (FieldInfo.InfoType = itField);
          IsAttr  := (FieldInfo.InfoType = itAttr);
          Buffer := nil;
          Attr := nil;
          Obj := nil;
          if IsAttr then
          begin
            if not Query.FieldIsNull(fi) then
            begin
              Obj := Query.ObjField(fi);
              if (Obj <> nil) then Attr := Obj.Attrs[FieldInfo.AttrIndex];
            end;
          end;
          FieldIsNull := Query.FieldIsNull(fi) or
                         (IsAttr and ((Attr = nil) or Attr.IsNull)) or
                         ((not IsField) and (not IsAttr));
          // To optimize performance, fields are accessed as binary data
          if (not FieldIsNull) then case FieldInfo.DataType of
            otString,
         otReference: begin
                        if FieldInfo.IsUTF16 and FieldInfo.IsWide then
                        begin
                          if IsAttr then
                            ws := TrimStringW(Attr.AsWideString)
                          else
                            ws := TrimStringW(Query.FieldAsWideString(fi));
                          if Length(ws) > 0 then Move(ws[1], ps[0], Length(ws) * 2);
                          ps[Length(ws) * 2] := #0;
                          ps[Length(ws) * 2 + 1] := #0;
                          Buffer := @ps;
                        end else begin
                          if IsAttr then
                          begin
                            if FieldInfo.IsWide and FieldInfo.IsUTF8 then
                              s := UTF8Encode(Attr.AsWideString)
                            else
                              s := Attr.AsString;
                          end else begin
                            if FieldInfo.IsWide and FieldInfo.IsUTF8 then
                              s := UTF8Encode(Query.FieldAsWideString(fi))
                            else
                              s := Query.FieldAsString(fi);
                          end;
                          s := TrimStringS(s);
                          if OracleBlob = nil then
                          begin
                            if Length(s) > 0 then Move(s[1], ps[0], Length(s));
                            ps[Length(s)] := #0;
                            Buffer := @ps;
                          end else begin
                            OracleBlob.BlobSize := Length(s);
                            if OracleBlob.BlobSize > 0 then
                            begin
                              GetMem(Buffer, OracleBlob.BlobSize);
                              Move(s[1], Buffer^, OracleBlob.BlobSize);
                              OracleBlob.BlobPointer := Buffer;
                            end else begin
                              FieldIsNull := True;
                            end;
                          end;
                        end;
                      end;
           otInteger: begin
                        if IsAttr then
                          bi := Attr.Value
                        else
                          bi := Query.FieldAsInteger(fi);
                        Buffer := @bi;
                      end;
             otFloat: begin
                        if IsAttr then
                          bf := Attr.Value
                        else
                          bf := Query.FieldAsFloat(fi);
                        Buffer := @bf;
                      end;
              otDate,
         otTimestamp,
       otTimestampTZ,
      otTimestampLTZ: begin
                        if IsAttr then
                          bd := Attr.Value
                        else
                          bd := Query.FieldAsDate(fi);
                        TimeStamp := DateTimeToTimeStamp(bd);
                        bd := TimeStampToMSecs(TimeStamp);
                        Buffer := @bd;
                      end;
           otLongRaw,
              otBLOB,
             otBFile: begin
                        if IsAttr then
                          BlobVariant := Attr.Value
                        else
                          BlobVariant := Query.Field(fi);
                        if VarIsEmpty(BlobVariant) or VarIsNull(BlobVariant) then
                          FieldIsNull := True
                        else begin
                          OracleBlob.BlobSize := VarArrayHighBound(BlobVariant, 1) + 1;
                          GetMem(Buffer, OracleBlob.BlobSize);
                          try
                            Move(VarArrayLock(BlobVariant)^, Buffer^, OracleBlob.BlobSize);
                          finally
                            VarArrayUnlock(BlobVariant);
                          end;
                          DoBLOBDecompress(Fields[f] as TBLOBField, Buffer, OracleBlob.BlobSize, True);
                          OracleBlob.BlobPointer := Buffer;
                        end;
                      end;
              otLong,
              otCLOB: begin
                        if IsAttr then
                        begin
                          if FieldInfo.IsXMLCLOB then
                          begin
                            XML := Obj.XMLAttr(FieldInfo.AttrName);
                            if XML.IsNull then
                              FieldIsNull := True
                            else
                              BlobVariant := XML.XML
                          end else
                            BlobVariant := Attr.Value
                        end else begin
                          if FieldInfo.IsXMLCLOB then
                            BlobVariant := Query.XMLField(fi).XML
                          else
                            BlobVariant := Query.Field(fi);
                        end;
                        if VarIsEmpty(BlobVariant) or VarIsNull(BlobVariant) then
                          FieldIsNull := True
                        else begin
                          {$IFDEF CompilerVersion2006}
                          if FieldInfo.IsWide then
                          begin
                            ws := BlobVariant;
                            OracleBlob.BlobSize := Length(ws) * 2;
                            GetMem(Buffer, OracleBlob.BlobSize);
                            Move(ws[1], Buffer^, OracleBlob.BlobSize);
                          end else
                          {$ENDIF}
                          begin
                            if VarType(BlobVariant) = varOleStr then
                              s := Oracle.UTF8Encode(BlobVariant)
                            else
                              s := BlobVariant;
                            OracleBlob.BlobSize := Length(s);
                            GetMem(Buffer, OracleBlob.BlobSize);
                            Move(PChar(s)^, Buffer^, OracleBlob.BlobSize);
                          end;
                          OracleBlob.BlobPointer := Buffer;
                        end;
                      end;
          end;
          if (Buffer <> nil) or FieldIsNull then
            FieldToRecord(Fields[f], FieldIsNull, Buffer, FFilterBuffer)
          else
            DatabaseError('Unexpected datatype: ' + Fields[f].FieldName);
        end;
      end;
      if Deleted then
        FilterAccept := False
      else begin
        FilterAccept := True;
        Data := Records.NewRecord(-1, False);
        if RowIdIndex >= 0 then SetRowId(Data, PChar(Query.FieldAsString(RowIdIndex)));
        Inc(FLastBookMark);
        Data.Bookmark := FLastBookMark;
        Data.BookmarkFlag := bfCurrent;
        if Cached then
        begin
          Log.Visible     := True;
          Log.Bookmark    := Data.Bookmark;
          Log.RecordIndex := Records.Count - 1;
        end;
        if Filtered then
        begin
          FilterAccept := ApplyFilterTo(FFilterBuffer);
          if FilterAccept then Records.AddToFilteredList(Data, True);
        end;
        if Records.RecordCount = 1 then CheckMasterFields;
        if Assigned(AfterFetchRecord) then
        begin
          afAction := afContinue;
          OldFCurRec := FCurRec;
          try
            FCurRec := Records.RecordCount - 1;
            AfterFetchRecord(Self, FilterAccept, afAction);
          except
            on E:Exception do
            begin
              ShowMessage(E.Message);
              afAction := afCancel;
            end;
          end;
          FCurRec := OldFCurRec;
          case afAction of
            afContinue: ;
               afPause: begin
                          Result := False;
                          Exit;
                        end;
                afStop: Query.Cancel;
              afCancel: begin
                          Query.Cancel;
                          Records.FreeRecords;
                          FilterAccept := False;
                        end;
          end;
        end;
      end;
    end;
  until FilterAccept;
  Result := FilterAccept;
end;

procedure TOracleDataSet.SetCursorShape;
begin
  if Cursor <> crDefault then
  begin
    OldCursor := GetScreenCursor;
    SetScreenCursor(Cursor);
  end;
end;

procedure TOracleDataSet.RestoreCursorShape;
begin
  if Cursor <> crDefault then SetScreenCursor(OldCursor);
end;

// Query all records into the collection
procedure TOracleDataSet.QueryRecords;
var i, d: Integer;
    Log: TChangeLogItem;
begin
  // Clear the records
  Records.FreeRecords;
  CheckMasterFields;
  // Clear some change log attributes
  for i := 0 to ChangeLog.Count - 1 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    Log.Visible := False;
  end;
  // Reset the bookmark counter
  FLastBookmark := 0;
  // For Detached datasets we cannot query anythin
  if Detached then Exit; 
  // If this is a masterless detail, exit without querying
  if MasterLess then
  begin
    if (FMaster <> nil) and (FMaster.State in [dsEdit, dsBrowse]) then
      MasterBookmark := GetMasterBookmark
    else begin
      MasterBookmark := -1;
    end;
    CopyMasterVarValues(nil);
    Query.Cancel;
    if (not Unidirectional) and QueryAllRecords then while QueryRecord do ;
    Exit;
  end;
  // Clear the master bookmarks of the details
  for d := 0 to FDetails.Count - 1 do
    TOracleDataSet(FDetails[d]).MasterBookmark := -1;
  // Copy the fields of the master to the variables
  CopyMasterVarValues(nil);
  if (not Activating) and Assigned(FBeforeQuery) then FBeforeQuery(Self);
  try
    // If cursor dataset then execute PL/SQL block to reopen cursor
    if IsCursorDataSet and not CursorValid then FQuery.Execute;
    CursorValid := False;
    // Execute the query and fetch all rows
    if not Activating then Query.Execute;
    FirstFetch := True;
    // Set the masterbookmark
    MasterBookmark := GetMasterBookmark;
    // Retrieve all records and place them in the dataset, if QueryAllRecords
    SetCursorShape;
    try
      if (not Unidirectional) and QueryAllRecords then while QueryRecord do ;
    finally
      RestoreCursorShape;
    end;
  finally
    if Assigned(FAfterQuery) then FAfterQuery(Self);
  end;
end;

// Calculate Buffer Size. Can only be called after BindFields
function TOracleDataset.GetRecBufSize: Integer;
var i: Integer;
    ds: Integer;
    FI: TOracleFieldInfo;
begin
  Result := 0;
  FRecordSize := 0;
  for i := 0 to FieldCount - 1 do with Fields[i] do
  begin
    FI := FieldInfoList.ForField(Fields[i]);
    if FieldNo >= 0 then
    begin
      FI.FieldOffset := FRecordSize;
      ds := FI.RealDataSize;
    end else begin
      ds := DataSize;
      if DataType = ftWideString then ds := Size * 2;
    end;
    Inc(Result, ds + 1);
    if FieldKind in [fkData, fkInternalCalc] then Inc(FRecordSize, ds + 1);
  end;
  Inc(Result, RecInfoSize);
end;

// Set extra field info
procedure TOracleDataSet.SetOracleFieldInfo;
var fi: Integer;
    FieldInfo: TOracleFieldInfo;
    F: TField;
begin
  if DefaultFields then SetFieldProperties;
  QBEDefinition.Update;
  for fi := 0 to FieldCount - 1 do if Fields[fi].FieldNo > 0 then
  begin
    F := Fields[fi];
    FieldInfo := FieldInfoList.ForField(F);
    if FieldInfo <> nil then
    begin
      FieldInfo.Field    := F;
      FieldInfo.QBEField := QBEDefinition.FieldByName(F.FieldName);
      FieldInfo.ReadOnly := F.ReadOnly;
      FieldInfo.Required := F.Required;
    end;
  end;
  for fi := 0 to FieldInfoList.Count - 1 do
    if (FieldInfoList[fi].InfoType = itField) and (FieldInfoList[fi].Field = nil) then
      FieldInfoList[fi].InfoType := itUnusedField;
end;

// Set field properties according to the Oracle dictionary
procedure TOracleDataSet.SetFieldProperties;
var fi, i: Integer;
    F: TField;
    FieldInfo: TOracleFieldInfo;
    T: TOracleTableInfo;
    C: TOracleColumnInfo;
    CPrecision, CScale: Integer;
    Max: Double;
    EdtFrm, DspFrm: string;
    DisplayWidth: Integer;
begin
  if (Session = nil) or (not Session.Connected) then Exit;
  T := nil;
  for fi := 0 to FieldCount - 1 do if Fields[fi].FieldKind in [fkData, fkInternalCalc] then
  begin
    F := Fields[fi];
    C := nil;
    // Check the field kind, fkInternalCalc fields will be read only
    if OracleDictionary.FieldKinds then
    begin
      if T = nil then T := GetOracleTableInfo;
      if C = nil then C := T.OracleColumnInfo(F.FieldName);
      if (C = nil) and (F.FieldKind = fkData) then
      begin
        F.FieldKind := fkInternalCalc;
        F.ReadOnly := True;
      end;
    end;
    // Determine the DisplayWidth for string fields
    if OracleDictionary.CharacterSize and (F is TStringField) then
    begin
      if Query = nil then i := -1 else i := Query.FieldIndex(F.FieldName);
      if i >= 0 then
      begin
        DisplayWidth := Query.FieldCharSize(i);
        if DisplayWidth > 0 then TStringField(F).DisplayWidth := DisplayWidth;
        TStringField(F).Size := DisplayWidth;
      end;
    end;
    // Determine the MinValue and MaxValue for numeric fields
    if OracleDictionary.RangeValues and (F is TNumericField) then
    begin
      if Query = nil then i := -1 else i := Query.FieldIndex(F.FieldName);
      if i >= 0 then
      begin
        Max := (IntPower(10, Query.FieldPrecision(i)) - 1);
        Max := Max / IntPower(10, Query.FieldScale(i));
        if F is TIntegerField then with F as TIntegerField do
        begin
          if (MinValue = 0) and (MaxValue = 0) then
          begin
            MinValue := -Round(Max);
            MaxValue := Round(Max);
          end;
        end;
        if F is TFloatField then with F as TFloatField do
        begin
          if (MinValue = 0) and (MaxValue = 0) then
          begin
            MinValue := -Max;
            MaxValue := Max;
          end;
        end;
      end;
    end;
    // Determine the display format for float fields
    if OracleDictionary.DisplayFormats and (F is TNumericField) then
    begin
      if Query = nil then i := -1 else i := Query.FieldIndex(F.FieldName);
      if i >= 0 then
      begin
        CPrecision := Query.FieldPrecision(i);
        CScale := Query.FieldScale(i);
        if (CPrecision <> 0) and (CScale > 0) then
        begin
          if CPrecision > CScale then
            DspFrm := StringOfChar('#', CPrecision - CScale - 1) + '0'
          else
            DspFrm := '0';
          EdtFrm := DspFrm;
          i := Length(DspFrm) - 2;
          while i > 1 do
          begin
            System.Insert(',', DspFrm, i);
            Dec(i, 3);
          end;
          if CScale > 0 then
          begin
            DspFrm := DspFrm + '.' + StringOfChar('0', CScale);
            EdtFrm := EdtFrm + '.' + StringOfChar('0', CScale);
          end;
          with F as TNumericField do
          begin
            if (DisplayFormat = '') and (EditFormat = '') then
            begin
              DisplayFormat := DspFrm;
              EditFormat := EdtFrm;
              DisplayWidth := Length(DspFrm) + 1;
            end;
          end;
        end else begin
          with F as TNumericField do
            if (CPrecision > 0) and (DisplayFormat = '') and (EditFormat = '') then
              DisplayWidth := CPrecision + 1;
        end;
      end;
    end;
    // Make references and collections invisible
    FieldInfo := FieldInfoList.ForField(F);
    if (FieldInfo <> nil) and (FieldInfo.DataType in [otReference, otObject]) then F.Visible := False;
  end;
end;

// Determine if a field is a read-only field
function TOracleDataSet.IsReadOnlyField(Field: TField): Boolean;
begin
  if Field.FieldNo > 0 then
    Result := FieldInfoList.ForField(Field).ReadOnly
  else
    Result := Field.ReadOnly;
end;

procedure TOracleDataset.InternalClose;
begin
  // Restore master fields
  EnableMasterFields(True);
  // Restore SQL
  RemoveQBEFromQuery;
  // Tablename must be determined again
  FTableName := '';
  // Free the record collection
  Records.Free;
  Records := nil;
  // Destroy the TField components if no persistent fields
  if DefaultFields then DestroyFields;
  // Reset these internal flags
  FLastBookmark := 0;
  FCurRec := -1;
  // Synchronize Details
  SyncDetails(False);
  // Enable master
  CheckMasterFields;
  // Free the DML Query
  if DMLQuery <> nil then
  begin
    try
      DMLQuery.Free;
    except
    end;
    DMLQuery := nil;
  end;
  // Free the Cursor Query
  if CursorQuery <> nil then
  begin
    try
      if CursorCreated then CursorQuery.Free;
    except
    end;
    CursorQuery := nil;
    CursorCreated := False;
    Query := FQuery;
  end;
  // Free the work blob list
  if WorkBlobs <> nil then
  begin
    WorkBlobs.Free;
    WorkBlobs := nil;
  end;
  // Clear the change log
  if ChangeLog <> nil then ChangeLog.Clear;
  {$IFDEF CompilerVersion5} PSLobsFree; {$ENDIF}
end;

// Close all queries associated with the dataset
procedure TOracleDataSet.CloseAll;
begin
  Active := False;
  if FQuery <> nil then FQuery.Close;
  DataEvent(dePropertyChange, 0);
end;

// Count the number of query hits for the dataset
function TOracleDataSet.CountQueryHits: Integer;
var v, vi: Integer;
    BeforeWhere, WhereClause, AfterWhere, WhereWord: widestring;
    VarList: TStringList;
begin
  // Cannot be performed for cursor datasets
  if IsCursorDataSet then
    raise Exception.Create('Cannot count query hits for cursor dataset');
  // If QBE Mode, modify the query for QBE
  if QBEMode then
  begin
    if State = dsInsert then UpdateRecord;
    SaveQBEValues;
    AddQBEToQuery;
  end;
  // Copy SQL and add count aggregation, remove order by clause
  OpenDMLQuery(False);
  DMLQuery.Clear;
  SplitSelect(SubstitutedSQL, True, BeforeWhere, WhereClause, AfterWhere, WhereWord);
  DMLQuery.SQL.Add('select count(*) from (');
  DMLQuery.SQL.Add(BeforeWhere);
  if WhereClause <> '' then
    DMLQuery.SQL.Add(WhereWord + ' ' + WhereClause + ')')
  else
    DMLQuery.SQL.Add(')');
  // Copy all variables that remains in the new SQL
  VarList := FindVariables(DMLQuery.SQL.Text, False);
  for v := 0 to VarList.Count - 1 do
  begin
    vi := VariableIndex(VarList[v]);
    if vi >= 0 then
    begin
      DMLQuery.DeclareVariable(VarList[v], VariableType(vi));
      DMLQuery.SetVariable(VarList[v], GetVariable(VarList[v]));
    end;
  end;
  VarList.Free;
  // Execute the count and return the result
  DMLQuery.Execute;
  Result := DMLQuery.FieldAsInteger(0);
  CloseDMLQuery;
end;

// Declare the variables in a query. All variables are
// assumed to have the same name and type as the fields.
procedure TOracleDataSet.DeclareQueryVariables(AQuery: TOracleQuery);
var v: Integer;
    F: TField;
    VarList: TStringList;
    VarName: string;
    VarType: Integer;
begin
  // Create a list of variable names, no duplicates
  VarList := FindVariables(AQuery.SQL.Text, False);
  // Declare all variables for which there is a corresponding field in the dataset
  try
    for v := 0 to VarList.Count - 1 do
    begin
      VarName := VarList[v];
      VarType := -1;
      if VarName = 'DOA__ROWID' then
        // The RowId is a special case
        VarType := otString
      else begin
        // For all other variables, find the field
        F := FindField(VarName);
        // Not found, maybe it's old_fieldname?
        if (F = nil) and (Copy(VarName, 1, 4) = 'OLD_') then
          F := FindField(Copy(VarName, 5, 255));
        if F <> nil then VarType := FieldOracleType(F);
      end;
      // Declare the variable
      if VarType >= 0 then AQuery.DeclareVariable(VarName, VarType);
    end;
  finally
    VarList.Free;
  end;
end;

// Set the values of the variables in a query. All variables are
// assumed to have the same name and type as the fields.
procedure TOracleDataSet.SetQueryVariables(AQuery: TOracleQuery);
var v: Integer;
    F: TField;
    VarName: string;
    OldField: Boolean;
    OldState: TDataSetState;
begin
  // Set all variables for which there is a corresponding field in the dataset
  for v := 0 to AQuery.VariableCount - 1 do
  begin
    VarName := AQuery.VariableName(v);
    if VarName[1] = ':' then System.Delete(VarName, 1, 1);
    if VarName = 'DOA__ROWID' then
      // The RowId is a special case
      AQuery.SetVariable(VarName, RowId)
    else begin
      // For all other variables, find the field
      F := FindField(VarName);
      OldField := False;
      // Not found, maybe it's old_fieldname?
      if (F = nil) and (Copy(VarName, 1, 4) = 'OLD_') then
      begin
        F := FindField(Copy(VarName, 5, 255));
        OldField := True;
      end;
      if F <> nil then
      begin
        // Assign the value of the field
        if OldField then OldState := SetTempState(dsOldValue) else OldState := dsOldValue;
        try
          if not NullField(F) then AQuery.SetVariable(VarName, F.Value);
        finally
          if OldField then RestoreState(OldState);
        end;
      end;
    end;
  end;
end;

// Get the values of the variables in a query. All variables are
// assumed to have the same name and type as the fields.
procedure TOracleDataSet.GetQueryVariables(AQuery: TOracleQuery);
var v: Integer;
    F: TField;
    VarName: string;
    VarValue: Variant;
    OldState: TDataSetState;
begin
  // Get all variables for which there is a corresponding field in the dataset
  for v := 0 to AQuery.VariableCount - 1 do
  begin
    VarName := AQuery.VariableName(v);
    if VarName[1] = ':' then System.Delete(VarName, 1, 1);
    // Find the field
    F := FindField(VarName);
    // If found and not a BLOB
    if (F <> nil) and not F.IsBlob then
    begin
      // Get the variable value
      VarValue := AQuery.GetVariable(VarName);
      // If different from the field value, set the field value
      if not CompareFieldToVariant(F, VarValue, True) then
      begin
        OldState := SetTempState(dsNewValue);
        try
          if VarIsNull(VarValue) or VarIsEmpty(VarValue) then
            F.Clear
          else begin
            if F.DataType in [ftString, ftMemo, ftWideString] then
              F.Value := TrimStringV(VarValue)
            else
              F.Value := VarValue;
          end;
        finally
          RestoreState(OldState);
        end;
      end;
    end;
  end;
end;

// Compare the values of the variables in a query. All variables are
// assumed to have the same name and type as the fields.
function TOracleDataSet.CompareQueryVariables(AQuery: TOracleQuery): Boolean;
var v: Integer;
    F: TField;
    VarName: string;
    VarValue: Variant;
begin
  Result := True;
  // Compare all variables for which there is a corresponding field in the dataset
  for v := 0 to AQuery.VariableCount - 1 do
  begin
    VarName := AQuery.VariableName(v);
    if VarName[1] = ':' then System.Delete(VarName, 1, 1);
    // Find the field
    F := FindField(VarName);
    // If found and not a BLOB
    if (F <> nil) and not F.IsBlob then
    begin
      // Get the variable value
      VarValue := AQuery.GetVariable(VarName);
      // If different from the field value, set the field value
      if not CompareFieldToVariant(F, VarValue, True) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TOracleDataSet.SubstitutedSQL: string;
begin
  Result := FQuery.SubstitutedSQL;
end;

procedure TOracleDataSet.ExecSQL;
begin
  CheckInactive;
  SendToMonitor(True, True);
  FQuery.ParentMonID := MonID;
  FQuery.Execute;
end;

function TOracleDataSet.GetMonID: Integer;
begin
  if FMonID = 0 then FMonID := GetNewMonID;
  Result := FMonID;
end;

procedure TOracleDataSet.SendToMonitor(Creation, Signal: Boolean);
var S: TOracleSession;
begin
  if ((Creation xor SentToMonitor) or not Signal) and MonitorListening then
  begin
    SentToMonitor := Creation;
    if Creation then S := Session else S := nil;
    AddMonObject(Creation, ckDataSet, MonID, 0, Self, S, nil, Signal);
  end;
end;

procedure TOracleDataSet.SendStartActivity(const ADescription: string; Parameters: TStrings);
begin
  SendToMonitor(True, True);
  AddMonActivity(ckDataSet, Session.MonID, MonID, 0, Self, afStart, 0, ADescription,
                 '', '', nil, Parameters, 0);
  MonActivityStartTime := GetTickCount;
end;

procedure TOracleDataSet.SendEndActivity(const ADescription, AResultMessage: string; Parameters: TStrings);
var t: DWord;
begin
  if MonActivityStartTime <> 0 then
  begin
    t := MonActivityStartTime;
    MonActivityStartTime := 0;
    SendToMonitor(True, True);
    AddMonActivity(ckDataSet, Session.MonID, MonID, 0, Self, afEnd, 0, ADescription,
                   '', AResultMessage, nil, Parameters, t);
  end;
end;

// This property is used while opening the dataset. It indicates if data is
// available even though the current state is still dsInActive.
function TOracleDataset.IsCursorOpen: Boolean;
begin
  Result := Assigned(Records);
end;

procedure TOracleDataSet.CreateFieldDef(FieldInfo: TOracleFieldInfo);
var FieldName: string;
    FieldRequired: boolean;
    FieldSize: Integer;
    FieldType: TFieldType;
    FieldNo: Integer;
    SmiPrec: Integer;
    msfSize: Integer;
    {$IFNDEF Delphi}
    FD: TFieldDef;
    {$ENDIF}
begin
  FieldType := ftUnknown;
  FieldRequired := OracleDictionary.RequiredFields and FieldInfo.Required;
  FieldSize := 0;
  FieldNo := FieldInfo.Index;
  FieldName := FieldInfo.FullName;
  case FieldInfo.DataType of
         otString: begin
                     FieldType := ftString;
                     if FieldInfo.IsWide then FieldType := ftWideString;
                     FieldSize := FieldInfo.Size;
                     if FieldInfo.IsUTF16 and not FieldInfo.IsWide then FieldSize := FieldSize * 2;
                     if FieldSize <= 0 then FieldSize := 1;
                     msfSize := Session.Preferences.MaxStringFieldSize;
                     if (msfSize <> 0) and (FieldSize > msfSize) and (FieldType <> ftWideString) then
                       FieldType := ftMemo;
                   end;
           otDate,
      otTimestamp,
    otTimestampTZ,
   otTimestampLTZ: FieldType := ftDateTime;
          otFloat: FieldType := ftFloat;
        otInteger: begin
                     SmiPrec := Session.Preferences.SmallIntPrecision;
                     if SmiPrec = 0 then SmiPrec := 4;
                     if FieldInfo.Precision <= SmiPrec then
                       FieldType := ftSmallInt
                     else
                       FieldType := ftInteger;
                   end;
        otLongRaw: FieldType := ftBlob;
           otBLOB,
          otBFile: {$IFDEF CompilerVersion5} FieldType := ftOraBlob; {$ELSE} FieldType := ftBlob; {$ENDIF}
           otLong: {$IFDEF CompilerVersion2006}
                   if not FieldInfo.IsWide then FieldType := ftMemo else FieldType := ftWideMemo;
                   {$ELSE}
                   FieldType := ftMemo;
                   {$ENDIF}
           otCLOB: {$IFDEF CompilerVersion5}
                     {$IFDEF CompilerVersion2006}
                     if not FieldInfo.IsWide then FieldType := ftOraClob else FieldType := ftWideMemo;
                     {$ELSE}
                     FieldType := ftOraClob;
                     {$ENDIF}
                   {$ELSE}
                     FieldType := ftMemo;
                   {$ENDIF}
      otReference: begin
                     FieldType := ftString;
                     FieldSize := 256;
                   end;
  end;
  if FieldType <> ftUnknown then
  begin
    // A difference between Delphi 3 and C++Builder 3
    {$IFDEF Delphi}
    TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize, FieldRequired, FieldNo);
    {$ELSE}
    FD := TFieldDef.Create(FieldDefs);
    FD.Name := FieldName;
    FD.DataType := FieldType;
    FD.Size := FieldSize;
    FD.Required := FieldRequired;
    FD.FieldNo := FieldNo;
    {$ENDIF}
  end;
end;

// Determine parent/child object attributes
procedure TOracleDataSet.InitParentChildAttributes;
var f, a: Integer;
    ParentName, ChildName: string;
    ParentNameLength: Integer;
    NoChild: Boolean;
begin
  for f := 0 to FieldInfoList.Count - 1 do
  begin
    if FieldInfoList[f].DataType = otObject then
    begin
      a := f + 1;
      ParentName := FieldInfoList[f].FullName + '.';
      ParentNameLength := Length(ParentName);
      while a < FieldInfoList.Count do
      begin
        ChildName := FieldInfoList[a].FullName;
        NoChild := Copy(ChildName, 1, ParentNameLength) <> ParentName;
        System.Delete(ChildName, 1, ParentNameLength);
        if (ChildName = '') or (Pos('.', ChildName) > 0) then NoChild := True;
        if not NoChild then
        begin
          FieldInfoList[f].FChildList.Add(FieldInfoList[a]);
          FieldInfoList[a].Parent := FieldInfoList[f];
//        ShowMessage(FieldInfoList[f].FullName + ' is the parent of ' + FieldInfoList[a].FullName);
        end;
        Inc(a);
      end;
    end;
  end;
end;

// Initialize field definitions for an object
procedure TOracleDataset.InitObjectFieldDefs(FieldInfo: TOracleFieldInfo);
var Obj: TOracleObject;
    a: Integer;
    FI: TOracleFieldInfo;
begin
  Obj := TOracleObject._Create(Session, Query.FieldTypeName(FieldInfo.FieldIndex), '', Self.MonID, FieldInfo.FullName);
  try
    FieldInfo.IsCollection := Obj.IsCollection;
    if not FieldInfo.IsCollection then for a := 0 to Obj.AttrCount - 1 do
    begin
      FI := TOracleFieldInfo.Create(FieldInfoList);
      FI.ColumnName   := Query.FieldName(FieldInfo.FieldIndex);
      FI.AttrName     := Obj.Attrs[a].Name;
      FI.FullName     := FI.ColumnName + '.' + FI.AttrName;
      FI.InfoType     := itAttr;
      FI.DataType     := Obj.Attrs[a].DataType;
      FI.Size         := Obj.Attrs[a].Size;
      FI.Scale        := Obj.Attrs[a].Scale;
      FI.Precision    := Obj.Attrs[a].Precision;
      FI.TypeName     := Obj.Attrs[a].TypeName;
      FI.CharSetID    := Obj.Attrs[a].CharSetID;
      FI.CharSetForm  := Obj.Attrs[a].CharSetForm;
      FI.IsUTF16      := Obj.Attrs[a].IsUTF16;
      FI.IsUTF8       := Obj.Attrs[a].IsUTF8;
      if FI.TypeName = 'SYS.XMLTYPE' then
      begin
        FI.IsXMLCLOB := True;
        FI.DataType  := otCLOB;
      end;
      {$IFDEF CompilerVersion5}
      FI.IsWide       := (FI.IsUTF16 and not (Session.Preferences.ConvertUTF = cuUTF16toUTF8)) or
                         (FI.IsUTF8 and (Session.Preferences.ConvertUTF = cuUTF8toUTF16)) or
                         ((FI.DataType = otString) and Session.Preferences.ForceWideStringFields);
      {$ENDIF}
      {$IFNDEF CompilerVersion2006}
      if FI.DataType in [otCLOB, otLong] then FI.IsWide := False;
      {$ENDIF}
      FI.FieldIndex   := FieldInfo.FieldIndex;
      FI.AttrIndex    := a;
      FI.IsCollection := Obj.Attrs[a].IsCollection;
      if FI.DataType <> otObject then CreateFieldDef(FI);
    end;
  finally
    Obj.Free;
  end;
end;

// Initialize field definitions
procedure TOracleDataset.InternalInitFieldDefs;
var i: Integer;
    FI: TOracleFieldInfo;
begin
  SendToMonitor(True, True);
  FieldDefs.Clear;
  // Update the variables for a detail dataset
  CopyMasterVarValues(nil);
  // Fire the BeforeQuery event
  if Activating and (not QBEMode) and (not Masterless) and Assigned(FBeforeQuery) then
    FBeforeQuery(Self);
  // Determine if the query contains a cursor variable
  GetQuery;
  // Describe or execute the query to determine the fields
  if Activating and (not QBEMode) and (not Masterless) then Query.Execute else Query.Describe;
  // Determine if query has a ROWID
  RowIdIndex := FindRowIdIndex(Query);
  FHasRowId := (RowIdIndex >= 0);
  // Clear the extra field info list and add one dummy field (FielNo's start at 1)
  FieldInfoList.Clear;
  TOracleFieldInfo.Create(FieldInfoList).InfoType := itOther;
  // Create a field in the dataset for each field in the query
  if Query <> nil then for i := 0 to Query.FieldCount - 1 do
  begin
    if i <> RowidIndex then // RowId's are never displayed
    begin
      FI := TOracleFieldInfo.Create(FieldInfoList);
      FI.ColumnName    := Query.FieldName(i);
      FI.FullName      := FI.ColumnName;
      FI.InfoType      := itField;
      FI.DataType      := Query.FieldType(i);
      FI.Size          := Query.FieldSize(i);
      FI.Scale         := Query.FieldScale(i);
      FI.Precision     := Query.FieldPrecision(i);
      FI.TypeName      := Query.FieldTypeName(i);
      FI.Required      := not Query.FieldOptional(i);
      FI.CharSetID     := Query.FieldCharSetId(i);
      FI.CharSetForm   := QUery.FieldCharSetForm(i);
      FI.IsUTF16       := IsUTF16CharSet(FI.CharSetID);
      FI.IsUTF8        := IsUTF8CharSet(FI.CharSetID);
      if FI.TypeName = 'SYS.XMLTYPE' then
      begin
        FI.IsXMLCLOB := True;
        FI.DataType  := otCLOB;
      end;
      {$IFDEF CompilerVersion5}
      FI.IsWide        := (FI.IsUTF16 and not (Session.Preferences.ConvertUTF = cuUTF16toUTF8)) or
                          (FI.IsUTF8 and (Session.Preferences.ConvertUTF = cuUTF8toUTF16)) or
                          ((FI.DataType = otString) and Session.Preferences.ForceWideStringFields);
      {$ENDIF}
      {$IFNDEF CompilerVersion2006}
      if FI.DataType in [otCLOB, otLong] then FI.IsWide := False;
      {$ENDIF}
      FI.FieldIndex    := i;
      if FI.DataType = otObject then
        InitObjectFieldDefs(FI)
      else
        CreateFieldDef(FI);
    end;
  end;
  InitParentChildAttributes;
  if Active then
  begin
    SetOracleFieldInfo;
    FRecBufSize := GetRecBufSize;
  end else begin
    QBEDefinition.Update;
  end;
end;

procedure TOracleDataSet.SetUnidirectional(Value: Boolean);
begin
  if Value = FUnidirectional then Exit;
  CheckInActive;
  Funidirectional := Value;
//  {$IFDEF CompilerVersion6} inherited SetUnidirectional(Value); {$ENDIF}
end;

{$IFDEF CompilerVersion4} // The Quest enhancements only work with Delphi 4 or higher

// Create a TField for any field that is returned by the query, but is
// not currently defined.
// Added by Bruce C. Quest Software
procedure TOracleDataSet.CreateUndefinedFields;
var
  I: integer;
begin
  if ObjectView then
  begin
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
      begin
        if FindField(Name) = nil then
          if (DataType <> ftUnknown) and not (faHiddenCol in Attributes) then
            CreateField(Self);
      end;
  end
  else
  begin
    for I := 0 to FieldDefList.Count - 1 do
      with FieldDefList[I] do
      begin
        if FindField(FieldDefList.Strings[i]) = nil then
          if (DataType <> ftUnknown) and not (faHiddenCol in Attributes)
             and not (DataType in ObjectFieldTypes) then
            CreateField(Self, nil, FieldDefList.Strings[I]);
      end;
  end;
end;

// Automatically sets the field size to the size of the data selected.
// Added by Bruce C. Quest Software
procedure TOracleDataset.SyncStringFieldSizes;
var
  I: integer;
  FieldDef: TFieldDef;
  FieldIndex: integer;
begin
  for I := 0 to Fields.Count - 1 do
  with Fields[I] do
  begin
    if DataType = ftString then
    begin
      FieldIndex := FieldDefList.IndexOf(FullName);
      if FieldIndex <> -1 then begin
        FieldDef := FieldDefList[FieldIndex];
        Size := FieldDef.Size;
      end
      else Continue;
    end;
  end;
end;

{$ENDIF}

function TOracleDataset.UseTemporaryLOBs: Boolean;
begin
  Result := (Session <> nil) and OCI81 and (Session.Preferences.TemporaryLOB <> tlNone);
end;

// This is the exception handler which is called if an exception is raised
// while the component is being streamed in or streamed out
procedure TOracleDataset.InternalHandleException;
begin
  ApplicationHandleException(Self);
end;

// Refresh the dataset by requerying the database
procedure TOracleDataSet.InternalRefresh;
begin
  if HasLooseCachedUpdates then raise Exception.Create(dtNoMasterRefresh);
  Counted := False;
  RowsAdded := 0;
  // Not allowed in QBE mode
  CheckQBEMode;
  // Post all details before refreshing
  PostDetails;
  // Force cursor re-opening
  CursorValid := False;
  // Query and go to first record
  QueryRecords;
  SyncDetails(True);
  First;
end;

procedure TOracleDataSet.Loaded;
begin
  inherited;
  try
    if ActiveOnLoading then Active := True;
  except
    if csDesigning in ComponentState then InternalHandleException else raise;
  end;
  (*
  if ComponentState = [] then ShowMessage('ComponentState = empty');
  if csAncestor in ComponentState then ShowMessage('csAncestor');
  if csDesigning in ComponentState then ShowMessage('csDesigning');
  if csDestroying in ComponentState then ShowMessage('csDestroying');
  if csFixups in ComponentState then ShowMessage('csFixups');
  if csLoading in ComponentState then ShowMessage('csLoading');
  if csReading in ComponentState then ShowMessage('csReading');
  if csUpdating in ComponentState then ShowMessage('csUpdating');
  if csWriting in ComponentState then ShowMessage('csWriting');
  *)
end;

// Bookmark functions

// Find a bookmark and return the record index
function TOracleDataSet.FindBookmark(Bookmark: Integer): Integer;
var i, b: Integer;
begin
  Result := -1;
  if Records = nil then Exit;
  b := Bookmark;
  if (b - 1 > 0) and (b - 1 < Records.RecordCount) then
  begin
    if b = Records.RecordAt(b - 1).Bookmark then
    begin
      Result := b - 1;
      Exit;
    end;
  end;
  for i := 0 to Records.RecordCount - 1 do
  begin
    if Bookmark = Records.RecordAt(i).Bookmark then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Bookmarks are stored as integers in the record data. To go to a bookmark,
// simply search for it in the Record collection
procedure TOracleDataset.InternalGotoBookmark(Bookmark: Pointer);
var i: Integer;
begin
  i := FindBookmark(PInteger(Bookmark)^);
  if i >= 0 then
    FCurRec := i
  else
    DatabaseError('Bookmark not found (' + IntToStr(PInteger(Bookmark)^) + ')');
end;

procedure TOracleDataset.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PRecordData(Buffer).Bookmark);
end;

{$IFDEF CompilerVersion5}
procedure TOracleDataSet.SetBlockReadSize(Value: Integer);
begin
  if Value <> BlockReadSize then
  begin
    if Value > 0 then
    begin
      BlockReadSize := -1;
    end else
      inherited
  end;
end;
{$ENDIF}


{$IFDEF CompilerVersion5}
function TOracleDataSet.GetCommitProviderTransactions: Boolean;
begin
  Result := not (opNoCommit in ProviderOptions);
end;

procedure TOracleDataSet.SetCommitProviderTransactions(Value: Boolean);
begin
  if Value = CommitProviderTransactions then Exit;
  if Value then
    ProviderOptions := ProviderOptions - [opNoCommit]
  else
    ProviderOptions := ProviderOptions + [opNoCommit];
end;
{$ENDIF}

// Bookmark flags are used to indicate if a particular record is the first
// or last record in the dataset.  This is necessary for "crack" handling.
// If the bookmark flag is bfBOF or bfEOF then the bookmark is not actually
// used; InternalFirst, or InternalLast are called instead by TDataSet.
function TOracleDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecordData(Buffer).BookmarkFlag;
end;

procedure TOracleDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordData(Buffer).BookmarkFlag := Value;
end;

// These methods provide a way to read and write bookmark data into the
// record buffer without actually repositioning the current record
procedure TOracleDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PRecordData(Buffer).Bookmark;
end;

procedure TOracleDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecordData(Buffer).Bookmark := PInteger(Data)^;
end;

// Is bookmark valid?
function TOracleDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Bookmark <> nil) and (FindBookmark(PInteger(Bookmark)^) >= 0);
end;

// Compare two bookmarks
function TOracleDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if (PInteger(Bookmark1)^ = PInteger(Bookmark2)^) then Result := 0;
    if (PInteger(Bookmark1)^ > PInteger(Bookmark2)^) then Result := 1;
    if (PInteger(Bookmark1)^ < PInteger(Bookmark2)^) then Result := -1;
  end;
end;

// Record / Field Access

// Get field offset
function TOracleDataset.FieldOffset(Field: TField): Integer;
begin
  if Field.FieldNo < 0 then
    Result := FRecordSize + Field.Offset
  else
    Result := FieldInfoList.ForField(Field).FieldOffset;
end;

// Get field index
function TOracleDataset.FieldIndex(Field: TField): Integer;
begin
  if Field.FieldNo < 0 then
    Result := -1
  else
    Result := FieldInfoList.ForField(Field).FieldIndex;
end;

// This method returns the size of just the data in the record buffer.
// Do not confuse this with RecBufSize which also includes any additonal
// structures stored in the record buffer (such as TRecInfo).
function TOracleDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

// TDataSet calls this method to allocate the record buffer.  Here we use
// FRecBufSize which is equal to the size of the data plus the size of the
// Record info.
function TOracleDataset.AllocRecordBuffer: PChar;
var b: PRecordData;
begin
  GetMem(Result, FRecBufSize);
  b := PRecordData(Result);
  b^.ArraySize := FRecBufSize - RecInfoSize;
  b^.BlobList := nil;
  b^.RowId := '';
  b^.PRowId := nil;
  b^.MasterBookmark := GetMasterBookmark;
end;

// Free an allocated rowid;
procedure TOracleDataSet.FreeRowId(Buffer: PRecordData);
var l: Integer;
begin
  if Buffer = nil then Exit;
  if Buffer.PRowId <> nil then
  begin
    l := StrLen(Buffer.PRowId) + 1;
    FreeMem(Buffer.PRowId, l);
    Dec(RowIdAllocations);
    Buffer.PRowId := nil;
  end;
  Buffer.RowId := '';
end;

// Set the rowid
procedure TOracleDataSet.SetRowId(Buffer: PRecordData; ARowId: PChar);
var l: Integer;
begin
  if Buffer = nil then Exit;
  FreeRowId(Buffer);
  l := StrLen(ARowId) + 1;
  if l < SizeOf(Buffer.RowId) then
    StrCopy(Buffer.RowId, ARowId)
  else begin
    GetMem(Buffer.PRowId, l);
    Inc(RowIdAllocations);
    StrCopy(Buffer.PRowId, ARowId);
  end;
end;

// Get the rowid
function TOracleDataSet.GetRowId(Buffer: PRecordData): string;
begin
  if Buffer = nil then
  begin
    Result := '';
    Exit;
  end;
  if Buffer.PRowId = nil then
    Result := Buffer.RowId
  else
    Result := Buffer.PRowId;
end;

// Again, TDataSet calls this method to free the record buffer.
procedure TOracleDataset.FreeRecordBuffer(var Buffer: PChar);
var b: PRecordData;
begin
  b := PRecordData(Buffer);
  FreeMem(b, FRecBufSize);
end;

// This procedure is called to clear all calculated fields,
// by filling the buffer with 1, all fields will be null
procedure TOracleDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(PRecordData(Buffer)^.Bytes[RecordSize], CalcFieldsSize, 1);
end;

// This multi-purpose function does 3 jobs.  It retrieves data for either
// the current, the prior, or the next record.  It must return the status
// (TGetResult), and raise an exception if DoCheck is True.
function TOracleDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;
  case GetMode of
    gmNext: if FCurRec < Records.RecordCount - 1 then
              Inc(FCurRec)
            else begin
              if not QueryRecord then
                Result := grEOF
              else begin
                Inc(FCurRec);
              end;
            end;
   gmPrior: if FCurRec <= 0 then Result := grBOF  else Dec(FCurRec);
 gmCurrent: if (FCurRec < 0) or (FCurRec >= Records.RecordCount) then
            begin
              if not QueryRecord then Result := grError;
            end;
  end;
  if Result = grOK then
  begin
    if FCurRec < 0 then
      Result := grError
    else begin
      Move(Records.RecordAt(FCurRec)^, Buffer^, FRecBufSize);
      with PRecordData(Buffer)^ do BookmarkFlag := bfCurrent;
      GetCalcFields(Buffer);
    end;
  end else begin
    if (Result = grError) and DoCheck then DatabaseError('No Records');
  end;
end;

// This routine is called to initialize a record buffer. By filling it
// with 1, all fields will be null
procedure TOracleDataset.InternalInitRecord(Buffer: PChar);
var b: PRecordData;
begin
  b := PRecordData(Buffer);
  FillChar(b^.Bytes[0], FRecordSize + CalcFieldsSize, 1);
  b^.BlobList := nil;
  b^.RowId := '';
  b^.PRowid := nil;
  b^.MasterBookmark := GetMasterBookmark;
end;

// Get the active record buffer. This depends on the State of the dataset.
function TOracleDataSet.GetActiveRecBuf(var RecBuf: PRecordData): Boolean;
var i: Integer;
    Log: TChangeLogItem;
    Cur: PRecordData;
begin
  // Determine the Log record if we are applying
  if Applying then Log := ChangeLog.Items[FLogRec] as TChangeLogItem else Log := nil;
  // Determine the Current record
  i := FCurRec;
  if i < 0 then i := 0;
  if (Records = nil) or (i >= Records.RecordCount) then
    Cur := nil
  else
    Cur := Records.RecordAt(i);
  // Determine the buffer based on the dataset state
  case State of
      dsBrowse,
    dsCurValue: if Applying then
                begin
                  if Log.Action = 'D' then
                    RecBuf := Log.BeforeData else RecBuf := Log.AfterData;
                end else begin
                  if IsEmpty then RecBuf := nil else RecBuf := PRecordData(ActiveBuffer);
                end;
        dsEdit,
      dsInsert: RecBuf := PRecordData(ActiveBuffer);
  dsCalcFields: RecBuf := PRecordData(CalcBuffer);
      dsFilter: RecBuf := FFilterBuffer;
    dsNewValue: if Applying then
                begin
                  if Log.Visible then RecBuf := Cur else RecBuf := Log.AfterData
                end else
                  RecBuf := Cur;
    dsOldValue: if Applying then
                  RecBuf := Log.BeforeData
                else begin
                  RecBuf := Cur;
                  if CachedUpdates and (RecBuf <> nil) then
                  begin
                    Log := FindChangeLogByBookmark(RecBuf.Bookmark) as TChangeLogItem;
                    if (Log <> nil) and (Log.Action <> 'I') then
                      RecBuf := Log.BeforeData;
                  end
                end;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

// Here we copy the data from the record buffer into a field's buffer.
function TOracleDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var b: PRecordData;
    RecordBlob: TOracleBlob;
begin
  Result := False;
  if not GetActiveRecBuf(b) then Exit;
  if Field.IsBlob then
  begin
    if b.BlobList <> nil then
    begin
      RecordBlob := b.BlobList.BlobByField(Field) as TOracleBlob;
      Result := (RecordBlob = nil) or (RecordBlob.BlobSize <> 0);
    end;
    Exit;
  end;
  if b^.Bytes[FieldOffset(Field)] = 0 then
  begin
    if Buffer <> nil then
    begin
      if Field.DataType = ftWideString then
        Move(b^.Bytes[FieldOffset(Field) + 1], Buffer^, RealFieldDataSize(Field))
      else
        Move(b^.Bytes[FieldOffset(Field) + 1], Buffer^, Field.DataSize);
    end;
    Result := True;
  end;
end;

function TOracleDataSet.RealFieldDataSize(AField: TField): Integer;
var FI: TOracleFieldInfo;
begin
  FI := FieldInfoList.ForField(AField);
  if FI <> nil then
    Result := FI.RealDataSize
  else
    if AField.DataType = ftWideString then
      Result := AField.Size * 2
    else
      Result := AField.DataSize;
end;

// Here we copy the data from the field's buffer into a record buffer.
procedure TOracleDataset.SetFieldData(Field: TField; Buffer: Pointer);
var b:PRecordData;
begin
  if not GetActiveRecBuf(b) then Exit;
//  if (not AllowSetFieldInBrowseState) and (not (State in dsWriteModes)) then DatabaseError(SNotEditing);
  if (State in [dsEdit, dsInsert]) and (not QBEMode) and (not NoFieldValidation) then
    Field.Validate(Buffer);
  if (Buffer = nil) or ((Field is TStringField) and (PChar(Buffer)^ = #0)) then
    b^.Bytes[FieldOffset(Field)] := 1
  else begin
    b^.Bytes[FieldOffset(Field)] := 0;
    if (Field.DataType = ftWideString) then
      Move(Buffer^, b^.Bytes[FieldOffset(Field) + 1], RealFieldDataSize(Field))
    else
      Move(Buffer^, b^.Bytes[FieldOffset(Field) + 1], Field.DataSize);
  end;
  if (State in [dsEdit, dsInsert]) and (not QBEMode) and (not NoFieldValidation) then
    CheckConstraints(Field);
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
  if not Detached then SyncDetailVars;
end;

// Put Field data into a PRecordData structure
procedure TOracleDataSet.FieldToRecord(Field: TField; FieldIsNull: Boolean;
  FieldData: Pointer; RecordData: PRecordData);
var OffSet, DataSize: Integer;
begin
  OffSet := FieldOffset(Field);
  DataSize := FieldInfoList.ForField(Field).RealDataSize;
  if FieldIsNull then
    RecordData^.Bytes[Offset] := 1
  else begin
    RecordData^.Bytes[Offset] := 0;
{$IFNDEF LINUX}
    if RecordData.ArraySize < Offset + 1 + DataSize then MessageBeep($FFFF);
{$ENDIF}
    Move(FieldData^, RecordData^.Bytes[OffSet + 1], DataSize);
    if Field is TStringField then
      RecordData^.Bytes[OffSet + DataSize] := 0;
  end;
end;

// Record Navigation / Editing

procedure TOracleDataset.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TOracleDataset.InternalLast;
begin
  SetCursorShape;
  try
    while QueryRecord do ;
  finally
    RestoreCursorShape;
  end;
  FCurRec := Records.RecordCount;
end;

// Determine if a field has been changed
function TOracleDataSet.FieldChanged(Field: TField): Boolean;
var OldValue, NewValue: Variant;
    OldNull, NewNull: Boolean;
    OldBlob, NewBlob: TOracleBlob;
    Log: TChangeLogItem;
    Rec: PRecordData;
begin
  Result := True;
  if Field.IsBlob then
  begin
    if not Applying then
      Result := (Field as TBlobField).Modified
    else begin
      Log := ChangeLog.Items[FLogRec] as TChangeLogItem;
      Rec := Records.RecordAt(FCurRec);
      OldBlob := Log.BeforeData.BlobList.BlobByField(Field) as TOracleBlob;
      NewBlob := Rec.BlobList.BlobByField(Field) as TOracleBlob;
      if OldBlob.BlobSize = NewBlob.BlobSize then
      begin
        if OldBlob.BlobSize > 0 then
          Result := not CompareMem(OldBlob.BlobPointer, NewBlob.BlobPointer, OldBlob.BlobSize)
        else
          Result := False;  
      end;
    end;
    Exit;
  end;
  OldValue := Field.OldValue;
  NewValue := Field.Value;
  OldNull  := VarIsNull(OldValue) or
              ((Field.DataType in [ftString, ftMemo, ftWideString]) and (TrimStringV(OldValue) = ''));
  NewNull  := VarIsNull(NewValue) or
              ((Field.DataType in [ftString, ftMemo, ftWideString]) and (TrimStringV(NewValue) = ''));
  if (OldNull xor NewNull) then Exit;
  if (OldNull and NewNull) then
  begin
    Result := False;
    Exit;
  end;
  if Field.DataType in [ftString, ftMemo, ftWideString] then
    Result := TrimStringV(OldValue) <> TrimStringV(NewValue)
  else
    Result := OldValue <> NewValue;
end;

// Add a where clause to the query to find the current record
procedure TOracleDataSet.AddRecordIdWhereClause(AQuery: TOracleQuery; OldValues: Boolean);
var KeyFields: TList;
    f: Integer;
    v: Variant;
    s, VarName: string;
    Field: TField;
begin
  if Trim(FUniqueFields) = '' then
  begin
    AQuery.SQL.Add('where rowid = :doa__rowid');
    AQuery.DeclareVariable('doa__rowid', otString);
    AQuery.SetVariable('doa__rowid', RowId);
  end else begin
    KeyFields := TList.Create;
    try
      GetFieldList(KeyFields, FUniqueFields);
      if KeyFields.Count = 0 then
        DatabaseError('No UniqueFields are present in dataset');
      s := 'where ';
      for f := 0 to KeyFields.Count - 1 do
      begin
        if f <> 0 then s := s + #13#10 + 'and ';
        Field := KeyFields[f];
        if OldValues then v := Field.OldValue else v := Field.Value;
        if VarIsNull(v) or ((Field.DataType in [ftString, ftWideString]) and (v = '')) then
        begin
          s := s + Field.FieldName + ' is null'
        end else begin
          VarName := 'doa__key' + IntToStr(f + 1);
          s := s + Field.FieldName + ' = :' + VarName;
          AQuery.DeclareVariable(VarName, FieldOracleType(Field));
          AQuery.SetVariable(VarName, v);
        end;
      end;
      AQuery.SQL.Add(s);
    finally
      KeyFields.Free;
    end;
  end;
end;

procedure TOracleDataset.Post;
begin
  if (Master <> nil) and Master.Posting and (Master.State = dsInsert) then Exit;
  CheckQBEMode;
  Posting := True;
  try
    inherited;
  finally
    Posting := False;
  end;
end;

// This method is called by TDataSet.Post.
procedure TOracleDataset.InternalPost;
var b: PRecordData;
    d: Integer;
    Detail: TOracleDataSet;
    OldModified: Boolean;
begin
  {$IFDEF CompilerVersion6} inherited; {$ENDIF}
  CheckQBEMode;
  OldModified := Modified;
  try
    case State of
      dsEdit: begin
                if CachedUpdates then CheckConstraints(nil);
                InternalUpdate;
              end;
    dsInsert: begin
                if SequenceField.ApplyMoment = amOnPost then SequenceField.Apply;
                if CachedUpdates then CheckConstraints(nil);
                GetActiveRecBuf(b);
                InternalAddRecord(b, False);
                for d := 0 to FDetails.Count - 1 do
                begin
                  Detail := FDetails[d];
                  Detail.MasterBookmark := Detail.GetMasterBookmark;
                end;
              end;
    end;
  finally
    SetModified(OldModified);
  end;
end;

// Save record data from a buffer to the record collection
// Each modified OracleBlob of the WorkBlobs must be copied
procedure TOracleDataSet.SaveRecordData(Buffer: PRecordData; Index: Integer);
var b: PRecordData;
    i: Integer;
    RecordBlob, WorkBlob: TOracleBlob;
begin
  b := Records.DataAt(Records.DataIndex(Index));
  Move(Buffer^, b^, FRecBufSize);
  if b.BlobList = nil then CreateBlobList(b.BlobList);
  if b.BlobList <> nil then
  begin
    for i := 0 to b.BlobList.Count - 1 do
    begin
      RecordBlob := b.BlobList.Items[i] as TOracleBlob;
      WorkBlob   := WorkBlobs.Items[i] as TOracleBlob;
      if WorkBlob.Field.Modified then
      begin
        RecordBlob.FreeBlob;
        if (WorkBlob.BlobPointer <> nil) and (WorkBlob.BlobSize > 0) then
        begin
          GetMem(RecordBlob.BlobPointer, WorkBlob.BlobSize);
          Move(WorkBlob.BlobPointer^, RecordBlob.BlobPointer^, WorkBlob.BlobSize);
          RecordBlob.BlobSize := WorkBlob.BlobSize;
        end;
      end;
    end;
  end;
end;

// Check if the RowId is valid for this action
procedure TOracleDataSet.CheckRowId(const ARowId: string; const Action: string);
begin
  if Trim(ARowId) = '' then
    raise Exception.Create(Action + ' cannot be performed without a rowid.' + #13#10 +
      'Provide a rowid in the query or handle this action in the OnApplyRecord event');
end;

// Refresh this record by fetching it from the database
procedure TOracleDataSet.RefreshRecord;
begin
  if (FCurRec < 0) or (FCurRec > Records.RecordCount - 1) then Exit;
  if State = dsInsert then Exit;
  FetchRecord(False, False, True, roAllFields in RefreshOptions, 
              Records.RecordAt(FCurRec));
  if State = dsEdit then Cancel else ReSync([]);
end;

// Fetch a record, either just the data fields or all fields,
// locking it if requested
// Refreshing it if requested
// Checking it if requested (mutually exclusive with refreshing)
function TOracleDataSet.FetchRecord(Lockit, CheckIt, RefreshIt, AllFields: Boolean;
                                    RefreshData: PRecordData): Boolean;
var v, vi, pn, f: Integer;
    ARowId: string;
    DataField: TField;
    OldSQL, BeforeWhere, WhereClause, AfterWhere, WhereWord, NewWhere: widestring;
    VarName: string;
    VarList: TStringList;
    NewRowId: string;
    SaveState: TDataSetState;
    LockpointSet: Boolean;
    s: string;
begin
  Result := False;
  if RefreshData <> nil then ARowId := GetRowId(RefreshData) else ARowId := Self.RowId;
  LockpointSet := False;
  try
    // Set a savepoint in case of a lock
    if LockIt then
    begin
      Lockpoint;
      LockpointSet := True;
    end;
    NewRowId := '';
    // First try all actions through the OnApplyRecord event
    if RefreshIt then
    begin
      // No need to check after refreshing
      CheckIt := False;
      SaveState := SetTempState(dsNewValue);
      try
        if ApplyRecord('R', NewRowId) then RefreshIt := False;
      finally
        RestoreState(SaveState);
      end
    end;
    if LockIt then
    begin
      LockIt := not ApplyRecord('L', NewRowId);
      // If the lock is handled, this handled the check as well
      if not LockIt then CheckIt := False;
    end;
    if CheckIt then CheckIt := not ApplyRecord('C', NewRowId);
    // If no action remains, exit
    if not (LockIt or RefreshIt or CheckIt) then Exit;
    // One or more actions remain, check if there is a rowid
    CheckRowId(ARowId, 'A lock, refresh or check');
    // OK, continue
    OpenDMLQuery(False);
    DMLQuery.StringFieldsOnly := Self.StringFieldsOnly;
    with DMLQuery do
    begin
      Clear;
      // Declare the RowId variable
      DeclareVariable('doa__rowid', otString);
      SetVariable('doa__rowid', ARowId);
      if not AllFields then
      begin
        // If just data fields, execute 'select * from UpdatingTable'
        SQL.Text := 'select * from ' + GetUpdatingTable + ' where rowid = :doa__rowid';
        // Lock it if requested
        if LockIt then SQL.Add('for update nowait');
        CopyUpdatingTableVariables(DMLQuery);
      end else begin
        // If all fields, re-execute the original SQL with rowid in where clause
        OldSQL := Self.SubstitutedSQL;
        // Check if the SQL is changed for QBE, if so, take the original
        if Pos(QBEComment, OldSQL) > 0 then
        begin
          s := Self.SQL.Text;
          Self.SQL.Text := QBEOldSQL;
          OldSQL := Self.SubstitutedSQL;
          Self.SQL.Text := s;
        end;
        // Trim after the EndRefreshComment in the where clause
        pn := Pos(EndRefreshComment, AnsiUpperCase(OldSQL));
        if pn > 0 then OldSQL := TrimRight(Copy(OldSQL, 1, pn - 1));

        // Split the SQL so that we can modify it
        SplitSelect(OldSQL, False, BeforeWhere, WhereClause, AfterWhere, WhereWord);
        // Take the part before the where-clause
        SQL.Add(BeforeWhere);
        // Add the new where clause and the rowid bit
        NewWhere := GetUpdatingAlias + '.rowid = :doa__rowid';
        if WhereClause <> '' then
          SQL.Add(WhereWord + ' (' + WhereClause + ')' + #13#10 + 'and' + #13#10 + '(' + NewWhere + ')')
        else
          SQL.Add(WhereWord + ' ' + NewWhere);
        // Lock the record if requested: THIS MUST CHANGE: JUST ONE TABLE
        if LockIt then
        begin
          // Find a data field for the 'for update of table.column nowait'clause
          DataField := nil;
          for f := 0 to Self.FieldCount - 1 do
          if Self.Fields[f].FieldKind = fkData then
          begin
            DataField := Self.Fields[f];
            Break;
          end;
          if DataField = nil then Exception.Create(Self.Name + ': No data fields');
          SQL.Add('for update of ' + GetUpdatingAlias + '.' + DataField.FieldName + ' nowait');
        end;
        // Copy the variables that remained in the new SQL
        VarList := FindVariables(SQL.Text, False);
        for v := 0 to Self.VariableCount - 1 do
        begin
          VarName := AnsiUpperCase(Self.VariableName(v));
          if VarName[1] = ':' then System.Delete(VarName, 1, 1);
          vi := VarList.IndexOf(VarName);
          if vi >= 0 then
          begin
            DeclareVariable(VarName, Self.VariableType(v));
            SetVariable(VarName, Self.GetVariable(VarName));
          end;
        end;
        VarList.Free;
      end;
      // Execute the statement to fetch and maybe lock a record
      Execute;
      // Does the record still exist?
      if LockIt or CheckIt then
      begin
        if Eof then
          DatabaseError(TranslatedMessage('L', 0, dmRecordDeleted));
      end;
      // Refresh the record
      if RefreshIt and not Eof then
      begin
        if RefreshData = nil then GetActiveRecBuf(RefreshData);
        if RefreshData <> nil then
          Result := InternalRefreshRecord(DMLQuery, RefreshData);
      end;
      // Check the record
      if CheckIt then
      begin
        // Is the record still the same?
        if RecordChanged(DMLQuery) then
        begin
          // No, unlock it and raise an error
          if LockIt then RollbackToLockpoint;
          DatabaseError(TranslatedMessage('L', 0, dmRecordChanged))
        end;
      end;
    end;
    CloseDMLQuery;
  except
    // Translate 'ORA-00054: resource busy and acquire with NOWAIT specified'
    // into something a user might understand.
    on E:Exception do
    begin
      if LockpointSet then
      try
        RollbackToLockpoint;
      except
      end;
      if not (E is EOracleError) then raise;
      s := E.Message;
      if EOracleError(E).ErrorCode = 54 then s := dmRecordLocked;
      DatabaseError(TranslatedMessage('L', EOracleError(E).ErrorCode, s));
    end;
  end;
end;

// Refresh a record with query data
function TOracleDataSet.InternalRefreshRecord(AQuery: TOracleQuery; ARecord: PRecordData): Boolean;
var f: Integer;
    SaveState: TDataSetState;
    FI: TOracleFieldInfo;
    v: Variant;
begin
  Result := False;
  // Set all fields to the newly fetched values
  if not AQuery.Eof then for f := 1 to FieldInfoList.Count - 1 do
  begin
    FI := FieldInfoList[f];
    if FI.Field <> nil then with FI.Field do
    begin
      if FieldKind in [fkData, fkInternalCalc] then
      begin
        if FI.QueryValueIndex(AQuery) >= 0 then
        begin
          v := FI.QueryValue(AQuery);
          if not CompareFieldToVariant(FI.Field, v, False) then
          begin
            Result := True;
            SaveState := SetTempState(dsNewValue);
            try
              if VarIsNull(v) or VarIsEmpty(v) then
                Clear
              else begin
                if DataType in [ftString, ftWideString] then
                  Value := TrimStringV(v)
                else if VarIsArray(v) then
                  Value := ByteArrayToString(v)
                else
                  Value := v;
              end;
            finally
              RestoreState(SaveState);
            end;
          end;
        end;
      end;
    end;
  end;
  if Assigned(FAfterRefreshRecord) then AfterRefreshRecord(Self);
end;

// Cached updates & ChangeLog methods

procedure TOracleDataSet.SetCachedUpdates(Value: Boolean);
begin
  FCachedUpdates := Value;
end;

// Determine if changes need to be logged
function TOracleDataSet.GetLogChanges: Boolean;
begin
  Result := FCachedUpdates or FLogChanges;
end;

// Start logging changes in the change log
procedure TOracleDataSet.StartUpdates;
begin
  ChangeLog.Clear;
  FLogChanges := True;
end;

// Apply all pending updates
procedure TOracleDataSet.ApplyUpdates(Actions: TApplyActions);
var i, a, bm, OldCurRec, mbm, mix: Integer;
    Log: TChangeLogItem;
    MasterBuffer: PRecordData;
const ActionArray: array[0..2] of TApplyAction = (aaDelete, aaUpdate, aaInsert);
begin
  if not Active then Exit;
  if State in [dsEdit, dsInsert] then Post;
  Log := nil;
  Applying := True;
  JustData := True;
  OldCurRec := FCurRec;
  try
    for a := 0 to 2 do
    begin
      for i := 0 to ChangeLog.Count - 1 do
      begin
        FLogRec := i;
        Log := ChangeLog.Items[FLogRec] as TChangeLogItem;
        if Log.Visible and (Log.Action in ['I', 'U']) then
        begin
          bm := Log.Bookmark;
          {if FindBookmark(bm) >= 0 then} InternalGotoBookmark(@bm);
        end;
        if IsLooseDetail then
        begin
          if Log.Action = 'D' then
            mbm := Log.BeforeData.MasterBookmark
          else
            mbm := Log.AfterData.MasterBookmark;
          mix := Master.FindBookmark(mbm);
          if mix >= 0 then
          begin
            MasterBuffer := Master.Records.RecordAt(mix);
            CopyMasterVarValues(MasterBuffer);
          end;
        end;
        case Log.Action of
          'I': if (ActionArray[a] = aaInsert) and (aaInsert in Actions) then
                 InternalInsertUpdate;
          'U': if (ActionArray[a] = aaUpdate) and (aaUpdate in Actions) then
                 InternalInsertUpdate;
          'D': if (ActionArray[a] = aaDelete) and (aaDelete in Actions) then
                 InternalDelete;
        end;
      end;
    end;
    FCurRec := OldCurRec;
    Applying := False;
    JustData := False;
    Resync([]);
  except
    FCurRec := OldCurRec;
    Applying := False;
    JustData := False;
    if Log.Visible and (Log.Action in ['I', 'U']) then
    begin
      bm := Log.Bookmark;
      if FindBookmark(bm) >= 0 then InternalGotoBookmark(@bm);
    end;
    Resync([]);
    raise;
  end;
end;

// Cancel pending updates and clear the change log
procedure TOracleDataSet.CancelUpdates;
var i: Integer;
    Log: TChangeLogItem;
begin
  if not Active then
  begin
    ChangeLog.Clear;
    FLogChanges := False;
    Exit;
  end;
  if State in [dsEdit, dsInsert] then Cancel;
  for i := ChangeLog.Count - 1 downto 0 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    Log.Cancel;
  end;
  ChangeLog.Clear;
  FLogChanges := False;
  if FCurRec > Records.RecordCount - 1 then FCurRec := Records.RecordCount - 1;
  CheckMasterFields;
  Resync([]);
  DataEvent(deDataSetChange, 0);
end;

// Remove all changes from the change log
procedure TOracleDataSet.CommitUpdates;
begin
  RowsAdded := ChangeLogRecordCount;
  ChangeLog.Clear;
  FLogChanges := False;
  DataEvent(deDataSetChange, 0);
end;

// Determine if there are any pending updates
function TOracleDataSet.UpdatesPending: Boolean;
begin
  Result := CachedUpdates and (ChangeLog.Count > 0);
end;

// Determine the status of the current record
function TOracleDataSet.UpdateStatus: TUpdateStatus;
var bm: TBookmark;
    Log: TChangeLogItem;
begin
  Result := usUnmodified;
  if IsEmpty then Exit;
  if State = dsInsert then
  begin
    Result := usInserted;
    Exit;
  end;
  Log := nil;
  if CachedUpdates then
  begin
    bm := GetBookmark;
    Log := FindChangeLogByBookmark(PInteger(bm)^) as TChangeLogItem;
    FreeBookmark(bm);
    if Log <> nil then
    begin
      case Log.Action of
        'I': Result := usInserted;
        'U': Result := usModified;
        'D': Result := usDeleted;
      end;
    end;
  end;
  if (Log = nil) and (State = dsEdit) then Result := usModified;
end;

function TOracleDataSet.FindChangeLogByBookmark(Bookmark: Integer): TObject;
var i: Integer;
    Log: TChangeLogItem;
begin
  for i := ChangeLog.Count - 1 downto 0 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    if (Log.Bookmark = Bookmark) and Log.InDataSet then
    begin
      Result := Log;
      Exit;
    end;
  end;
  Result := nil;
end;

function TOracleDataSet.FindChangeLogByRowId(const ARowId: string): TObject;
var i: Integer;
    Log: TChangeLogItem;
begin
  for i := ChangeLog.Count - 1 downto 0 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    if (Log.BeforeData <> nil) and (GetRowId(Log.BeforeData) = ARowId) and Log.InDataSet then
    begin
      Result := Log;
      Exit;
    end;
  end;
  Result := nil;
end;

function TOracleDataSet.FindChangeLogByIndex(Index: Integer): TObject;
var i: Integer;
    Log: TChangeLogItem;
begin
  for i := ChangeLog.Count - 1 downto 0 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    if (Log.RecordIndex = Index) and (Log.Action = 'I') and Log.InDataSet then
    begin
      Result := Log;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TOracleDataSet.RecordToChangeLog(BeforeData, AfterData: PRecordData; Action: Char; Bookmark, RecordIndex: Integer);
var i, bm, Offset, RealIndex: Integer;
    Log: TChangeLogItem;
begin
  if not LogChanges then Exit;
  if UseFiltered then RealIndex := Records.DataIndex(RecordIndex) else RealIndex := RecordIndex;
  if BeforeData <> nil then bm := BeforeData^.Bookmark else bm := Bookmark;
  Log := FindChangeLogByBookmark(bm) as TChangeLogItem;
  if Log = nil then
  begin
    // Rearrange ChangeLog in case of delete or insert
    Offset := 0;
    if Action = 'D' then Offset := -1 else if Action = 'I' then Offset := 1;
    if Offset <> 0 then
    begin
      for i := 0 to ChangeLog.Count - 1 do
      begin
        Log := ChangeLog.Items[i] as TChangeLogItem;
        if (Log.RecordIndex >= RealIndex) and (Log.InDataSet) and (Log.Action = 'I') then
          Inc(Log.RecordIndex, Offset);
      end;
    end;
    Log := TChangeLogItem.CreateForDataSet(Self);
    Log.SetBeforeData(BeforeData);
    Log.SetAfterData(AfterData);
    Log.Action := Action;
    Log.Bookmark := bm;
    Log.RecordIndex := RealIndex;
    Log.Applied := not CachedUpdates;
  end else begin
    if (Log.Action = 'I') and (Action = 'U') then
    begin
      // Update on Insert => Insert
      Log.SetAfterData(AfterData);
      Exit;
    end;
    if (Log.Action = 'I') and (Action = 'D') then
    begin
      // Delete on Insert => Remove
      Log.Free;
      Exit;
    end;
    if (Log.Action = 'U') and (Action = 'U') then
    begin
      // Update on Update => Update
      Log.SetAfterData(AfterData);
      Exit;
    end;
    if (Log.Action = 'U') and (Action = 'D') then
    begin
      // Delete on Update => Delete
      Log.FreeAfterData;
      Log.Action := Action;
      Exit;
    end;
  end;
end;

// Determine how many records were added/deleted for this dataset
function TOracleDataSet.ChangeLogRecordCount: Integer;
var i: Integer;
    Log: TChangeLogItem;
begin
  Result := 0;
  for i := 0 to ChangeLog.Count - 1 do
  begin
    Log := ChangeLog.Items[i] as TChangeLogItem;
    if Log.InDataSet then
    begin
      if (Log.Action = 'I') then Inc(Result);
      if (Log.Action = 'D') then Dec(Result);
    end;
  end;
end;

// Determine the column name of a field
function TOracleDataSet.FieldColumnName(AField: TField): string;
begin
  Result := Trim(AField.Origin);
  if (Result = '') or (Pos('.', Result) > 0) then Result := AField.FieldName;
end;

// Write LOB Data
procedure TOracleDataSet.WriteLOBData(ALOB: TLOBLocator; AOracleBlob: TCollectionItem; ACol: TOracleFieldInfo);
var LongString: string;
    ws: WideString;
    OracleBlob: TOracleBlob;
begin
  OracleBlob := AOracleBlob as TOracleBlob;
  if (OracleBlob.BlobPointer <> nil) and (OracleBlob.BlobSize > 0) then
  begin
    if ALOB.LOBType in [otBLOB, otBFile] then
    begin
      OracleBlob.Compress;
      ALOB.Write(OracleBlob.CompressedPointer^, OracleBlob.CompressedSize);
      OracleBlob.Decompress;
    end else begin
      if ACol.IsWide then
      begin
        SetLength(ws, OracleBlob.BlobSize div 2);
        if OracleBlob.BlobSize > 0 then
          Move(OracleBlob.BlobPointer^, ws[1], OracleBlob.BlobSize);
        ALOB.AsWideString := ws;
      end else begin
        SetLength(LongString, OracleBlob.BlobSize);
        if OracleBlob.BlobSize > 0 then
          Move(OracleBlob.BlobPointer^, LongString[1], OracleBlob.BlobSize);
        ALOB.AsString := LongString;
      end;
    end;
  end;
end;

// Does the dataset contain object fields?
function TOracleDataSet.IsObjectQuery: Boolean;
var f: Integer;
begin
  Result := True;
  for f := 0 to FieldInfoList.Count - 1 do
    if FieldInfoList[f].DataType = otObject then Exit;
  Result := False;
end;

// Create the SQL, returning clause and variables for a column
procedure TOracleDataSet.ColSQL(Col: TOracleFieldInfo; Inserting: Boolean;
  var SQL1, SQL2, ReturningFrom, ReturningInto: string;
      LOBList: TList; Blobs: TBlobList);
var ASQL1, ASQL2, ASubSQL, VarName, Dummy: string;
    LOB: TLOBLocator;
    IsNull, WasNull, ParentWasNull: Boolean;
    c: Integer;
    ColPrefix: string;
    OracleBlob: TOracleBlob;
    v: Variant;
    p: Pointer;
    LongString: string;
    LongStringW: WideString;
    LOBType: Integer;
begin
  ASQL1 := '';
  ASQL2 := '';
  if IsObjectQuery then ColPrefix := 't.' else ColPrefix := '';
  IsNull := Col.FieldIsNull;
  WasNull := Inserting or ((Col.DataType = otObject) and Col.FieldWasNull);
  ParentWasNull := ((Col.InfoType = itAttr) and
                    (Inserting or Col.Parent.FieldWasNull));
  // Fields always need to be included in the field list
  if Col.InfoType in [itField, itUnusedField] then
  begin
    // Prefix it with the standard table alias
    if Inserting then
      ASQL1 := ColPrefix + Col.ColumnName
    else begin
      // Add an ' = ' for updates
      ASQL2 := ColPrefix + Col.ColumnName + ' = ';
      // Not for objects that were not null before
    end;
  end else begin
    // If we are updating an attribute of a non-null object, just update that
    // attribute and do not use a complete default constructor
    if (not ParentWasNull) and (not Inserting) then
      ASQL2 := ColPrefix + Col.FullName + ' = ';
  end;
  if (Col.DataType = otObject) and (not WasNull) and (not IsNull) then ASQL2 := '';
  // Is it an object?
  if Col.DataType <> otObject then
  begin
    // No, check if the column is null
    if IsNull then
    begin
      // Simply use the null expression
      ASQL2 := ASQL2 + Col.FieldNullExpression(Inserting);
    end else begin
      // It is not null, create a variable for the field
      VarName := 'v' + IntToStr(Col.Index);
      // LOB values are initially empty, all others are values
      if (Col.DataType = otCLOB) and Col.IsXMLCLOB then
        ASQL2 := ASQL2 + 'sys.xmltype.createxml(' + ':' + VarName +')'
      else if (Col.DataType = otCLOB) and not UseTemporaryLOBs then
        ASQL2 := ASQL2 + 'empty_clob()'
      else if (Col.DataType = otBLOB)  and not UseTemporaryLOBs then
        ASQL2 := ASQL2 + 'empty_blob()'
      else begin
        ASQL2 := ASQL2 + ':' + VarName;
      end;
      DMLQuery.DeclareVariable(VarName, Col.DataType);
      if (Col.IsUTF16 or Col.IsUTF8) and (Col.DataType in [otString, otLong]) then
        DMLQuery.DeclareVariableCharSet(VarName, Col.CharSetForm, Col.CharSetID);
      case Col.DataType of
        otLongRaw:
        begin
          OracleBlob := Blobs.BlobByField(Col.Field) as TOracleBlob;
          OracleBlob.Compress;
          v := VarArrayCreate([0, OracleBlob.CompressedSize - 1], varByte);
          p := VarArrayLock(v);
          try
            if OracleBlob.CompressedSize > 0 then
              Move(OracleBlob.CompressedPointer^, p^, OracleBlob.CompressedSize);
            OracleBlob.Decompress;
          finally
            VarArrayUnlock(v);
          end;
          DMLQuery.SetVariable(VarName, v);
        end;
        otLong:
        begin
          OracleBlob := Blobs.BlobByField(Col.Field) as TOracleBlob;
          if Col.IsWide then
          begin
            SetLength(LongStringW, OracleBlob.BlobSize div 2);
            if OracleBlob.BlobSize > 0 then
              Move(OracleBlob.BlobPointer^, PWideChar(LongStringW)^, OracleBlob.BlobSize);
            LongString := UTF8Encode(LongStringW);
          end else begin
            SetLength(LongString, OracleBlob.BlobSize);
            if OracleBlob.BlobSize > 0 then
              Move(OracleBlob.BlobPointer^, PChar(LongString)^, OracleBlob.BlobSize);
          end;
          DMLQuery.SetVariable(VarName, LongString);
        end;
        otCLOB, otBLOB, otBFile:
        begin
          if Col.IsXMLCLOB or UseTemporaryLOBs then
          begin
            LOBType := Col.DataType;
            if (LOBType = otCLOB) and (Col.CharSetForm = ocfNational) then LOBType := otNCLOB;
            LOB := TLOBLocator.CreateTemporary(Session, LOBType, Session.Preferences.TemporaryLOB = tlCache);
            LOB.ParentMonID := Self.MonID;
            LOB.Name := VarName;
            if (Col.IsUTF16 or Col.IsUTF8) and (Col.DataType = otCLOB) then
            begin
              LOB.CharSetForm := Col.CharSetForm;
              LOB.CharSetID := ocsAL16UTF16;
            end;
            OracleBlob := Blobs.BlobByField(Col.Field) as TOracleBlob;
            WriteLOBData(LOB, OracleBlob, Col);
          end else begin
            AddWithComma(ColPrefix + Col.FullName, ReturningFrom, True);
            AddWithComma(':' + VarName, ReturningInto, True);
            LOB := TLOBLocator.Create(Session, Col.DataType);
            LOB.Tag := Col.Field.Index;
            LOB.ParentMonID := Self.MonID;
            LOB.Name := VarName;
            if (Col.IsUTF16 or Col.IsUTF8) and (Col.DataType = otCLOB) then
            begin
              LOB.CharSetForm := Col.CharSetForm;
              LOB.CharSetID := ocsAL16UTF16;
            end;
          end;
          DMLQuery.SetComplexVariable(VarName, LOB);
          LOBList.Add(LOB);
        end;
      else
        DMLQuery.SetVariable(VarName, Col.Field.Value);
      end;
    end;
  end else begin
    // It is an object, is it null?
    if IsNull or Col.IsCollection then
      ASQL2 := ASQL2 + Col.FieldNullExpression(Inserting)
    else begin
      Dummy := '';
      ASubSQL := '';
      for c := 0 to Col.ChildCount - 1 do
      begin
        if Inserting or WasNull or Col.Children[c].FieldChanged then
          ColSQL(Col.Children[c], Inserting, Dummy, ASubSQL,
                 ReturningFrom, ReturningInto, LOBList, Blobs);
      end;
      // It is not null, create a constructor if it was null before
      // Otherwise individual attributes are updated
      if Inserting or WasNull then
        ASQL2 := ASQL2 + Col.TypeName + '(' + ASubSQL + ')'
      else
        ASQL2 := ASQL2 + ASubSQL;
    end;
  end;
  AddWithComma(ASQL1, SQL1, True);
  AddWithComma(ASQL2, SQL2, True);
end;

procedure TOracleDataSet.CreateUpdate(LOBList: TList; Blobs: TBLOBList);
var AllSame: Boolean;
    f: Integer;
    SQL1, SQL2, ReturningFrom, ReturningInto: string;
    Col: TOracleFieldInfo;
begin
  DMLQuery.Clear;
  SQL1 := '';
  SQL2 := '';
  ReturningFrom := '';
  ReturningInto := '';
  AllSame := True;
  for f := 0 to FieldInfoList.Count - 1 do
  begin
    Col := FieldInfoList[f];
    if ((Col.InfoType = itField) and (Col.Field.FieldKind = fkData)) or
       ((Col.InfoType = itUnusedField) and (Col.DataType = otObject) and (not Col.IsCollection)) then
    begin
      if Col.FieldChanged then
      begin
        AllSame := False;
        ColSQL(Col, False, SQL1, SQL2, ReturningFrom, ReturningInto, LOBList, Blobs);
      end;
    end;
  end;
  if not AllSame then
  begin
    if IsObjectQuery then
    begin
      DMLQuery.SQL.Add('update ' + GetUpdatingTable + ' t');
      DMLQuery.SQL.Add('set ' + SQL2);
    end else begin
      DMLQuery.SQL.Add('update ' + GetUpdatingTable);
      DMLQuery.SQL.Add('set ' + SQL2);
    end;
    CopyUpdatingTableVariables(DMLQuery);
    AddRecordIdWhereClause(DMLQuery, True);
    // For OCI80 (implying Oracle8) we use the returning clause to obtain the rowid
    if Session.UseOCI80 and (Trim(UniqueFields) = '') then
    begin
      AddWithComma('rowid', ReturningFrom, True);
      AddWithComma(':doa__rowid', ReturningInto, True);
      if DMLQuery.VariableIndex('doa__rowid') < 0 then DMLQuery.DeclareVariable('doa__rowid', otString);
    end;
    if ReturningFrom <> '' then DMLQuery.SQL.Add('returning ' + ReturningFrom + ' into ' + ReturningInto);
  end;
end;

procedure TOracleDataSet.CreateInsert(LOBList: TList; Blobs: TBLOBList);
var SQL1, SQL2, ReturningFrom, ReturningInto: string;
    f: Integer;
    AllNull: Boolean;
    Col: TOracleFieldInfo;
begin
  DMLQuery.Clear;
  SQL1 := '';
  SQL2 := '';
  ReturningFrom := '';
  ReturningInto := '';
  AllNull := True;
  for f := 0 to FieldInfoList.Count - 1 do
  begin
    Col := FieldInfoList[f];
    if ((Col.InfoType = itField) and (Col.Field.FieldKind = fkData)) or
       ((Col.InfoType = itUnusedField) and (Col.DataType = otObject) and (not Col.IsCollection)) then
    begin
      if (not Col.FieldIsNull) or Col.FieldHasDefault then
      begin
        AllNull := False;
        ColSQL(Col, True, SQL1, SQL2, ReturningFrom, ReturningInto, LOBList, Blobs);
      end;
    end;
  end;
  if not AllNull then
  begin
    if IsObjectQuery then
      DMLQuery.SQL.Add('insert into ' + GetUpdatingTable + ' t')
    else
      DMLQuery.SQL.Add('insert into ' + GetUpdatingTable);
    CopyUpdatingTableVariables(DMLQuery);
    DMLQuery.SQL.Add('  (' + SQL1 + ')');
    DMLQuery.SQL.Add('values');
    DMLQuery.SQL.Add('  (' + SQL2 + ')');
    // For OCI80 (implying Oracle8) we use the returning clause to obtain the rowid
    if Session.UseOCI80 and (Trim(UniqueFields) = '') then
    begin
      AddWithComma('rowid', ReturningFrom, True);
      AddWithComma(':doa__rowid', ReturningInto, True);
      DMLQuery.DeclareVariable('doa__rowid', otString);
    end;
    if ReturningFrom <> '' then
      DMLQuery.SQL.Add('returning ' + ReturningFrom + ' into ' + ReturningInto);
  end;
end;

// Insert or update a record
procedure TOracleDataSet.InternalInsertUpdate;
var Updating, Inserting, NoChanges: Boolean;
    LOBList: TList;
    f, i, rr: Integer;
    FieldValue: Variant;
    FieldIsNull: Boolean;
    LOB: TLOBLocator;
    c: Char;
    OracleBlob: TOracleBlob;
    Log: TChangeLogItem;
    DontApply: Boolean;
    r, b: PRecordData;
    Blobs: TBlobList;
    NewRowId: string;
    LockpointSet: Boolean;
    Col: TOracleFieldInfo;
begin
  if not GetActiveRecBuf(b) then Exit;
  // Set inserting/updating booleans
  if Applying then
  begin
    Log := ChangeLog.Items[FLogRec] as TChangeLogItem;
    Updating := Log.Action = 'U';
    Blobs := Log.AfterData.BlobList;
  end else begin
    Log := nil;
    Updating := (State = dsEdit);
    Blobs := WorkBlobs;
  end;
  Inserting := not Updating;
  // Determine if anything has changed
  NoChanges := True;
  for f := 0 to FieldCount - 1 do with Fields[f] do if FieldKind = fkData then
  begin
    if Updating then
    begin
      if FieldChanged(Fields[f]) then
      begin
        NoChanges := False;
        Break;
      end;
    end else begin
      if IsBlob then
        FieldIsNull := (TOracleBlob(Blobs.BlobByField(Fields[f])).BlobSize = 0)
      else begin
        FieldValue  := Value;
        FieldIsNull := VarIsNull(FieldValue) or
                       ((DataType in [ftString, ftWideString]) and (TrimStringV(FieldValue) = ''))
      end;
      if not FieldIsNull then
      begin
        NoChanges := False;
        Break;
      end;
    end;
  end;
  // Determine if changes should be applied to the database
  DontApply := (CachedUpdates and not Applying) or NoChanges;
  // Create a list of LOBLocators to write after the insert/update
  LOBList := TList.Create;
  LockpointSet := False;
  try
    if not DontApply then
    begin
      // Lock the record and/or set a saveopint to rollback to later
      if Updating and (LockingMode in [lmCheckImmediate, lmLockDelayed]) then
        LockRecord(False) // Also sets lockpoint
      else
        Lockpoint;
      LockpointSet := True;
      if Inserting then c := 'I' else c := 'U';
      NewRowId := '';
      if ApplyRecord(c, NewRowId) then
      begin
        if Inserting then SetRowId(b, PChar(NewRowId));
      end else begin
        // Initialize DML query
        OpenDMLQuery(False);
        // Determine the SQL and variables
        if Updating then
          CreateUpdate(LOBList, Blobs)
        else begin
          InitMasterCollection;
          CreateInsert(LOBList, Blobs);
        end;
        // Exeute it!
        DMLQuery.Execute;
        // If updating and no rows were affected, the record has been deleted
        if Updating and (DMLQuery.RowsProcessed = 0) then
          DatabaseError(TranslatedMessage('U', 0, dmRecordDeleted));
        // Save the new RowId
        if (not Session.POLite) and (TrimRight(FUniqueFields) = '') then
        begin
          if Session.UseOCI80 then
            SetRowId(b, PChar(string(DMLQuery.GetVariable('doa__rowid'))))
          else begin
            if Inserting then SetRowId(b, PChar(DMLQuery.RowId));
          end;
        end else
          if Inserting then SetRowId(b, PChar(FetchRowId(False)));
        // Write all non-temporary LOB data, they are all empty right now
        for i := 0 to LOBList.Count - 1 do
        begin
          LOB := LOBList.Items[i];
          if not LOB.Temporary then
          begin
            OracleBlob := Blobs.BlobByField(Fields[LOB.Tag]) as TOracleBlob;
            Col := FieldInfoList.ForField(Fields[LOB.Tag]);
            WriteLOBData(LOB, OracleBlob, Col);
(*
            if (OracleBlob.BlobPointer <> nil) and (OracleBlob.BlobSize > 0) then
            begin
              if LOB.LOBType in [otBLOB, otBFile] then
              begin
                OracleBlob.Compress;
                LOB.Write(OracleBlob.CompressedPointer^, OracleBlob.CompressedSize);
                OracleBlob.Decompress;
              end else begin
                SetLength(LongString, OracleBlob.BlobSize);
                if OracleBlob.BlobSize > 0 then
                  Move(OracleBlob.BlobPointer^, LongString[1], OracleBlob.BlobSize);
                LongString := RemoveCR(LongString, Session);
                LOB.Write(LongString[1], Length(LongString));
              end;
            end;
*)
          end;
        end;
      end;
      // Free the LOB List
      for i := 0 to LOBList.Count - 1 do
      begin
        TLOBLocator(LOBList.Items[i]).Free;
        LOBList[i] := nil;
      end;
      // Commit all modifications
      Commit(cmNormal);
    end else begin
      // Commit the lock if nothing has changed
      if (LockingMode = lmLockImmediate) and (not CachedUpdates) and (not Applying) then Commit(cmNormal);
    end;
    // Update or insert the record in the collection
    if not (CachedUpdates and Applying) then
    begin
      if Updating then
      begin
        // Log the change
        RecordToChangeLog(Records.RecordAt(FCurRec), b, 'U', 0, FCurRec);
        // Save the change to the Record collection
        SaveRecordData(b, FCurRec);
      end else begin
        // Create a new item in the Record collection
        r := Records.NewRecord(FCurRec, Filtered);
        if (FCurRec < 0) or (FCurRec >= Records.RecordCount) then
          FCurRec := Records.RecordCount - 1;
        SaveRecordData(b, FCurRec);
        Inc(FLastBookmark);
        if not CachedUpdates then Inc(RowsAdded);
        r.Bookmark := FLastBookmark;
        r.BookmarkFlag := bfCurrent;
        b.Bookmark := r.Bookmark;
        // Log the change
        RecordToChangeLog(nil, b, 'I', r.Bookmark, FCurRec);
      end;
    end;
    if CachedUpdates and Applying and Log.Visible then
    begin
      r := Records.RecordAt(FCurRec);
      SetRowId(r, PChar(GetRowId(b)));
    end;
    // Refresh the record if necessary
    if (not DontApply) and
       ((Updating and (roAfterUpdate in RefreshOptions)) or
        (Inserting and (HasDefaults or (SequenceField.ApplyMoment = amOnServer) or
         (roAfterInsert in RefreshOptions)))) then
    begin
      if (not Applying) or (Applying and (Log.Visible)) then
      begin
        rr := FCurRec;
        if rr < 0 then rr := 0;
        if rr <= Records.RecordCount - 1 then
          FetchRecord(False, False, True, roAllFields in RefreshOptions,
                      Records.RecordAt(rr));
      end;
    end;
    LOBList.Free;
  except
    on E:Exception do
    begin
      // In case of an error, rollback to undo any changes and unlock the record
      try
        if LockpointSet then RollbackToLockpoint;
      except
      end;
      // Free the LOB List
      for i := 0 to LOBList.Count - 1 do TLOBLocator(LOBList.Items[i]).Free;
      LOBList.Free;
      // For POLite, in case of an update, refetch the record by primary/unique
      // key to work around bug causing "[POL-4000} null object not allowed"
      if Session.POLite and Updating then FetchRowId(False);
      // Raise the appropriate exception
      if not (E is EOracleError) then raise;
      if Inserting then C := 'I' else C := 'U';
      DatabaseError(TranslatedMessage(C, EOracleError(E).ErrorCode, E.Message));
    end;
  end;
end;

// Update active record
procedure TOracleDataset.InternalUpdate;
begin
  InternalInsertUpdate;
end;

// Insert or append a record
procedure TOracleDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var b: PRecordData;
begin
  if not GetActiveRecBuf(b) then Exit;
  if b <> Buffer then raise Exception.Create('InternalAddRecord: b <> buffer');
  if Append then InternalLast;
  InternalInsertUpdate;
end;

// Delete the current record
procedure TOracleDataset.InternalDelete;
var b: PRecordData;
    NewRowId: string;
    LockpointSet: Boolean;
begin
  CheckQBEMode;
  if not GetActiveRecBuf(b) then Exit;
  LockpointSet := False;
  try
    if Applying or not CachedUpdates then
    begin
      // Lock the record.
      // If the delete goes wrong, we have to rollback to undo this lock
      if (FLockingMode in [lmCheckImmediate, lmLockDelayed]) or
         ((FLockingMode = lmLockImmediate) and not CachedUpdates) then
      begin
        LockRecord(False);
        LockpointSet := True;
      end;
      // Create and execute a delete statement based on the rowid
      OpenDMLQuery(False);
      with DMLQuery do
      begin
        NewRowId := '';
        if not ApplyRecord('D', NewRowId) then
        begin
          Clear;
          CheckRowId(Self.RowId, 'A delete');
(*        SQL.Text := 'delete from ' + GetUpdatingTable + ' where rowid = :doa__rowid';
          DeclareVariable('doa__rowid', otString);
          SetVariable('doa__rowid', Self.RowId);  *)
          SQL.Text := 'delete from ' + GetUpdatingTable + #13#10;
          CopyUpdatingTableVariables(DMLQuery);
          AddRecordIdWhereClause(DMLQuery, True);
          Execute;
          // If no rows were affected, the record has been deleted
          if RowsProcessed = 0 then
            DatabaseError(TranslatedMessage('D', 0, dmRecordDeleted));
          // Commit the delete if the session didn't already do that
        end;
        Commit(cmNormal);
      end;
    end else begin
      if FLockingMode in [lmLockImmediate, lmCheckImmediate] then
        if (FindChangeLogByBookmark(b.Bookmark) = nil) then
          LockRecord(FLockingMode = lmCheckImmediate);
    end;
  except
    on E:Exception do
    begin
      // In case of an error, rollback to unlock the record
      if FLockingMode <> lmNone then
      try
        if LockpointSet then RollbackToLockpoint;
      except
      end;
      if not (E is EOracleError) then raise;
      DatabaseError(TranslatedMessage('D', EOracleError(E).ErrorCode, E.Message));
    end;
  end;
  if not Applying then
  begin
    // Log the change
    RecordToChangeLog(Records.RecordAt(FCurRec), nil, 'D', 0, FCurRec);
    // Delete the record from the list
    Records.DeleteRecord(FCurRec, True);
    CheckMasterFields;
    if not CachedUpdates then Dec(RowsAdded);
  end;
end;

// Compare a dataset field to a variant value
function TOracleDataSet.CompareFieldToVariant(AField: TField; const AValue: Variant; CompareOld: Boolean): Boolean;
var vd: Variant;
    nd, nq: Boolean;
    s: string;
    ws: widestring;
    p: Pointer;
    l: Integer;
    FreeIt, Equal: Boolean;
begin
  Result := False;
  {$IFDEF CompilerVersion2006}
  // Cannot compare old value of TWideMemoFields (OldValue = String instead of WideString)
  if CompareOld and (AField.DataType = ftWideMemo) then
  begin
    Result := True;
    Exit;
  end;
  {$ENDIF}
  if CompareOld then vd := AField.OldValue else vd := AField.Value;
  nd := VarIsNull(vd) or
        ((AField.DataType in [ftString, ftMemo, ftWideString
                             {$IFDEF CompilerVersion5}, ftOraClob {$ENDIF}]) and (TrimRight(vd) = '')) or
        ((AField.DataType in [ftBlob, ftGraphic
                             {$IFDEF CompilerVersion5}, ftOraBlob {$ENDIF}]) and (string(vd) = ''));
  nq := VarIsNull(AValue) or
        VarIsEmpty(AValue) or
        (((VarType(AValue) and VarTypeMask) = varString) and (TrimRight(AValue) = ''));
  // Avoid compares with null/unassigned!!
  // If one field is null, and the other is not, they are unequal
  if nd xor nq then Exit;
  // Check if both are null, or both are equal
  if not (nd and nq) then
  begin
    case FieldOracleType(AField) of
       // In case of a string, trim trailing spaces and disregard overflow
       otString: begin
                   if (VarType(AValue) = varOleStr) and (VarType(vd) = VarOleStr) then
                   begin
                     ws := AValue;
                     if TrimRight(WideString(vd)) <> TrimRight(ws) then Exit;
                   end else begin
                     s := string(AValue);
                     if (Length(s) > AField.DataSize - 1) and (AField.DataSize > 0) and (AField.DataType <> ftWideString) then
                       s := Copy(s, 1, AField.DataSize - 1);
                     if (AField.DataType = ftWideString) and FieldInfoList.ForField(AField).IsUTF8 then
                       vd := UTF8Encode(vd);
                     if TrimRight(vd) <> TrimRight(s) then Exit;
                   end;
                 end;
        // In case of a float, discard accuracy difference
        otFloat: if not AlmostEqual(vd, AValue) then Exit;
        // In case of a date, discard 10ths of a second
         otDate: if Abs(vd - AValue) > 1 / (24 * 60 * 60) then Exit;
        // In case of a Timestamp, discard 10,000ths of a second
    otTimestamp,
  otTimestampTZ,
 otTimestampLTZ: if Abs(vd - AValue) > 1 / (24 * 60 * 60 * 1000) then Exit;
         otBLOB,
      otLongRaw: begin
                   // For BLOBs we need to compare the binary, uncompressed data
                   p := VarArrayLock(AValue);
                   try
                     l := VarArrayHighBound(AValue, 1) - VarArrayLowBound(AValue, 1) + 1;
                     FreeIt := DoBLOBDecompress(AField as TBLOBField, p, l, False);
                   finally
                     VarArrayUnlock(AValue);
                   end;
                   s := vd;
                   Equal := (Length(s) = l) and CompareMem(p, @s[1], l);
                   if FreeIt then FreeMem(p, l);
                   if not Equal then Exit;
                 end;
         otCLOB: begin
                   if (varType(AValue) = varOleStr) and (varType(vd) = varString) then
                   begin
                     if vd <> Oracle.UTF8Encode(AValue) then Exit
                   end else begin
                     if vd <> AValue then Exit;
                   end;
                 end;
    else
      if vd <> AValue then Exit;
    end;
  end;
  Result := True;
end;

// Check if the current record is different from the query
function TOracleDataSet.RecordChanged(Q: TOracleQuery): Boolean;
var fd: Integer;
    FI: TOracleFieldInfo;
begin
  Result := True;
  // Compare each dataset field to the corresponding query field
  for fd := 0 to FieldCount - 1 do
  if (Fields[fd].FieldKind = fkData) {and (not Fields[fd].IsBlob)} then
  begin
    // Find the field in the query
    FI := FieldInfoList.ForField(Fields[fd]);
    // Compare it
    if (FI <> nil) and (FI.QueryValueIndex(Q) >= 0) and not CompareFieldToVariant(Fields[fd], FI.QueryValue(Q), True) then Exit;
(*
    // Find the field in the query
    fq := Q.FieldIndex(Fields[fd].FieldName);
    // Compare it
    if (fq >= 0) and not CompareFieldToVariant(Fields[fd], Q.Field(fq), True) then Exit;
*)
  end;
  Result := False;
end;

// Lock the current record and/or check if it has been changed
procedure TOracleDataSet.LockRecord(CheckOnly: Boolean);
begin
  FetchRecord(not CheckOnly, True, False, False, nil);
end;

// When a user starts editing, the record will be locked or checked depending
// on the LockingMode
procedure TOracleDataSet.InternalEdit;
var b: PRecordData;
    AllFields, RefreshIt, CheckIt, LockIt: Boolean;
    f: Integer;
begin
  if not GetActiveRecBuf(b) then Exit;
  if (FLockingMode in [lmLockImmediate, lmCheckImmediate]) or
     (roBeforeEdit in RefreshOptions) then
  begin
    if (FindChangeLogByBookmark(b.Bookmark) = nil) then
    begin
      CheckIt   := (LockingMode in [lmLockImmediate, lmCheckImmediate]);
      LockIt    := (LockingMode = lmLockImmediate);
      if roBeforeEdit in RefreshOptions then
      begin
        AllFields := roAllFields in RefreshOptions;
        RefreshIt := True;
        CheckIt   := False;
      end else begin
        AllFields := False;
        RefreshIt := False;
      end;
      FetchRecord(LockIt, CheckIt, RefreshIt, AllFields, Records.RecordAt(FCurRec));
      if RefreshIt then
        for f := 0 to FieldCount - 1 do with Fields[f] do
          if (not IsBlob) and (OldValue <> Value) then Value := OldValue;
    end;
  end;
end;

// When a user cancels an edit, the record will be unlocked
// This is achieved by rolling back the current transaction
procedure TOracleDataSet.InternalCancel;
begin
  if (FLockingMode = lmLockImmediate) and (not CachedUpdates)  and (State <> dsInsert) then
  try
    Rollback;
  except
    on E:Exception do DatabaseError(E.Message);
  end;
end;

// Optional Methods

function TOracleDataset.GetRecordCount: Longint;
begin
  if Records = nil then CheckActive;
  if FCountAllRecords and (not Filtered) then
  begin
    if not Counted then
    begin
      RowCount := CountQueryHits;
      Counted := True;
    end;
    Result := RowCount + RowsAdded + ChangeLogRecordCount;
  end else
    Result := Records.RecordCount;
end;

function TOracleDataset.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
end;

procedure TOracleDataset.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= Records.RecordCount) then
  begin
    if (FCurRec <> Value - 1) then
    begin
      CheckBrowseMode;
      FCurRec := Value - 1;
      Resync([]);
      DoAfterScroll;
    end;
  end;
  if FCountAllRecords and (Value <> RecNo) then
  begin
    if (Value > 0) and (Value < RecordCount) then MoveBy(Value - RecNo);
  end;
end;

// Get the table information for the updating table
function TOracleDataSet.GetOracleTableInfo: TOracleTableInfo;
var c, i: Integer;
    TableName: string;
begin
  if Session = nil then raise Exception.Create('Session undefined');
  // If the table info list doesn't exist yet, create it
  if Session.OracleTableInfoList = nil then
    Session.OracleTableInfoList := TCollection.Create(TOracleTableInfo);
  // Find this table in the list
  c := Session.OracleTableInfoList.Count - 1;
  TableName := GetUpdatingTable;
  for i := 0 to c do
  begin
    Result := Session.OracleTableInfoList.Items[i] as TOracleTableInfo;
    if stricomp(PChar(Result.TableExpression), PChar(TableName)) = 0 then Exit;
  end;
  // Not found, create a new one and resolve its owner and name
  Result := TOracleTableInfo.Create(Session.OracleTableInfoList);
  Result.Query.Session := Session;
  Result.TableExpression := TableName;
  ResolveName(Result.TableExpression, Result.TableOwner, Result.TableName);
end;

// Filtering functions

function TOracleDataset.ApplyFilter: Boolean;
begin
  if FilterExpr = nil then
    Result := True
  else
    Result := FilterExpr.AsBoolean;
end;

function TOracleDataSet.ApplyFilterTo(Data: PRecordData): Boolean;
var OldFFilterBuffer: Pointer;
    SaveState: TDataSetState;
begin
  OldFFilterBuffer := FFilterBuffer;
  SaveState := SetTempState(dsFilter);
  try
    Result := True;
    FFilterBuffer := Data;
    if Assigned(OnFilterRecord) then OnFilterRecord(Self, Result);
    if Result then Result := ApplyFilter;
  finally
    RestoreState(SaveState);
    FFilterBuffer := OldFFilterBuffer;
  end;
end;

procedure TOracleDataset.CalculateFields(Buffer: PChar);
begin
  if (State <> dsFilter) {and not fDontCalc} then
    inherited CalculateFields(Buffer);
end;

// This routine is called by the expression parser to create a column identifier
function TOracleDataset.GetColumnExpr(const Identifier: String;
  ParameterList: TParameterList): TExpression;
begin
  Result := TColumnExpression.Create(Self, Identifier)
end;

// Refresh display after filter property change
procedure TOracleDataset.RefreshFilter;
var i: Integer;
    Data: PRecordData;
begin
  if Active then
  begin
    if Filtered then
    begin
      Records.FilteredList.Clear;
      for i := 0 to Records.Count - 1 do
      begin
        Data := Records.DataAt(i);
        if ApplyFilterTo(Data) then Records.AddToFilteredList(Data, True);
      end;
    end;
    First;
//    Resync([]);
  end;
end;

// Activate filter after property change
procedure TOracleDataset.ActivateFilter;
begin
  if FilterExpr <> nil then
  begin
    FilterExpr.Free;
    FilterExpr := nil;
  end;
  if Active then
  begin
    if Filter <> '' then FilterExpr := CreateExpression(Filter, GetColumnExpr, FilterOptions);
    if Filtered then RefreshFilter;
  end;
end;

// Set Filtered property
procedure TOracleDataset.SetFiltered(Value: Boolean);
begin
  if Active then CheckBrowseMode;
  inherited SetFiltered(Value);
  RefreshFilter;
end;

function TOracleDataSet.UseFiltered: Boolean;
begin
  Result := Filtered and not JustData;
end;

// Set Filter property
procedure TOracleDataset.SetFilterText(const Value: string);
begin
  inherited SetFilterText(Value);
  ActivateFilter;
end;

// Set FilterOptions property
procedure TOracleDataset.SetFilterOptions(Value: TFilterOptions);
begin
  inherited SetFilterOptions(Value);
  ActivateFilter;
end;

// TColumnExpression

constructor TColumnExpression.Create(ADataset: TDataset; AField: string);
begin
  inherited Create;
  fDataset := ADataset;
  fField   := AField;
end;

function TColumnExpression.GetAsDate: TDateTime;
begin
  Result := fDataSet.FieldByName(fField).AsDateTime
end;

function TColumnExpression.GetAsBoolean: Boolean;
begin
  Result := fDataSet.FieldByName(fField).AsBoolean;
end;

function TColumnExpression.GetAsFloat: Double;
begin
  case fDataSet.FieldByName(fField).Datatype of
    ftDate, ftTime, ftDateTime : Result := fDataSet.FieldByName(fField).AsDateTime;
  else
    Result := fDataSet.FieldByName(fField).AsFloat;
  end;
end;

function TColumnExpression.GetAsInteger: Integer;
begin
  Result := fDataSet.FieldByName(fField).AsInteger;
end;

function TColumnExpression.GetAsString: String;
begin
  Result := fDataSet.FieldByName(fField).AsString;
end;

function TColumnExpression.GetIsNull: Boolean;
begin
  Result := (fField = 'NULL') or fDataSet.FieldByName(fField).IsNull;
end;

// Returns the type of data stored in this row to the expression evaluator
function TColumnExpression.GetExprType: TExprType;
begin
  if fField = 'NULL' then
  begin
    Result := ttString;
    Exit;
  end;
  case fDataSet.FieldByName(fField).DataType of
{$IFDEF CompilerVersion4}
    ftFixedChar,
   ftWideString: Result := ttString;
     ftLargeint: Result := ttInteger;
{$ENDIF}
       ftString,
         ftMemo,
{$IFDEF CompilerVersion5}
      ftOraClob,
{$ENDIF}
      ftFmtMemo: Result := ttString;
     ftSmallint,
      ftInteger,
         ftWord: Result := ttInteger;
      ftBoolean: Result := ttBoolean;
        ftFloat,
     ftCurrency,
          ftBCD: Result := ttFloat;
         ftDate,
         ftTime,
     ftDateTime: Result := ttDate;
  else
    Result := ttString;
  end;
end;

{$IFDEF CompilerVersion5}

{ IProviderSupport implementation }

procedure TOracleDataSet.PSEndTransaction(Commit: Boolean);
begin
  if not (opNoCommit in ProviderOptions) then
  begin
    if Commit then
    begin
      if not Session.MTSEnlisted then Session.Commit;
      CommitUpdates;
    end else begin
      if not Session.MTSEnlisted then Session.Rollback;
      CancelUpdates;
    end;
  end;
end;

procedure TOracleDataSet.PSExecute;
begin
  Active := False;
  Active := True;
  PSLobsWrite;
  PSLobsFree;
end;

function TOracleDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: Pointer = nil): Integer;
var p, VarType, qp, l: Integer;
    s, VarName: string;
    LOB: TLOBLocator;
    LOBList: TList;
    ParamType: TFieldType;
    ResultDataSet: TOracleDataSet;
begin
  OpenDMLQuery(False);
  DMLQuery.Clear;
  s := ASQL;
  Result := 0;
  LOBList := TList.Create;
  try
    for p := 0 to AParams.Count - 1 do
    begin
      // The Provider creates a SQL statement with a '?' for each variable
      // Replace '?' by ':Vn'
      qp := Pos('?', s);
      if qp <= 0 then raise
        Exception.Create('No variable for parameter ' + IntToStr(p));
      VarName := 'V' + IntToStr(p);
      System.Delete(s, qp, 1);
      System.Insert(':' + VarName, s, qp);
      // Convert FieldType to VariableType
      VarType := otString;
      ParamType := AParams[p].DataType;
      case ParamType of
         ftInteger,
        ftSmallInt,
            ftWord: VarType := otInteger;
           ftFloat,
        ftCurrency,
             ftBCD: VarType := otFloat;
            ftDate,
            ftTime,
        ftDateTime: VarType := otDate;
            ftBlob: VarType := otLongRaw;
         ftOraBlob: VarType := otBLOB;
         ftOraClob: VarType := otCLOB;
      end;
      // Strings longer than the maximum varchar length are longs
      if (VarType = otString) and (Length(string(AParams[p].Value)) > Session.MaxVarchar) then
        VarType := otLong;
      // Declare the variable and assign the value
      DMLQuery.DeclareVariable(VarName, VarType);
      if not (VarType in [otBLOB, otCLOB]) then
        DMLQuery.SetVariable(VarName, AParams[p].Value)
      else begin
        LOB := TLOBLocator.Create(DMLQuery.Session, VarType);
        LOB.ParentMonID := Self.MonID;
        LOB.Name := VarName;
        LOB.Tag := p;
        DMLQuery.SetComplexVariable(VarName, LOB);
        LOBList.Add(LOB);
      end;
    end;
    // Execute the query or open the dataset
    if ResultSet = nil then
    begin
      // Set the modified SQL and execute it
      DMLQuery.SQL.Text := s;
      DMLQuery.Execute;
      // Return the number of rows processed by the query
      Result := DMLQuery.RowsProcessed;
    end else begin
      ResultDataSet := TOracleDataSet.Create(nil);
      ResultDataSet.Session := Session;
      ResultDataSet.SQL.Text := s;
      ResultDataSet.Variables := DMLQuery.Variables;
      ResultDataSet.Active := True;
      Result := ResultDataSet.RecordCount;
      TDataSet(ResultSet^) := ResultDataSet;
    end;
    // Write the LOB's
    if Result > 0 then for l := 0 to LOBList.Count - 1 do
    begin
      LOB := LOBList[l];
      s := AParams[LOB.Tag].AsBlob;
      if Length(s) > 0 then
      begin
        if LOB.LOBType = otCLOB then s := RemoveCR(s, Session);
        LOB.Write(s[1], Length(s));
      end;
    end;
    try
      for l := 0 to LOBList.Count - 1 do TLOBLocator(LOBList[l]).Free;
      LOBList.Free;
    except
    end;
  except
    on E:Exception do
    begin
      try
        for l := 0 to LOBList.Count - 1 do TLOBLocator(LOBList[l]).Free;
        LOBList.Free;
      except
      end;
      if not (E is EOracleError) then raise;
      DatabaseError(TranslatedMessage(UpCase(ASQL[1]), EOracleError(E).ErrorCode, E.Message))
    end;
  end;
  CloseDMLQuery;
end;

function TOracleDataSet.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
var i, f: Integer;
    T: TOracleTableInfo;
    C: TOracleConstraintInfo;
    Fields, s: string;
    Options: TIndexOptions;
    Okay: Boolean;
begin
  Result := TIndexDefs.Create(Self);
  if not (opNoIndexDefs in ProviderOptions) then
  begin
    if (Trim(FUpdatingTable) = '') and IsCursorDataSet then Exit;
    T := GetOracleTableInfo;
    T.GetConstraints(Self);
    for i := 0 to T.OracleConstraintInfoList.Count - 1 do
    begin
      C := T.OracleConstraintInfoList.Items[i] as TOracleConstraintInfo;
      C.ReadFromDictionary(Self);
      if (C.ConstraintType in [ctPrimaryKey, ctUniqueKey]) then
      begin
        Okay := (ixUnique in IndexTypes) or
                ((ixPrimary in IndexTypes) and (C.ConstraintType = ctPrimaryKey));
        if Okay then
        begin
          Fields := '';
          for f := 0 to C.ColumnNames.Count - 1 do
          begin
            s := C.ColumnNames[f];
            if Query.FieldIndex(s) < 0 then
            begin
              Fields := '';
              Break;
            end;
            if f <> 0 then Fields := Fields + ';';
            Fields := Fields + s;
          end;
          if Fields <> '' then
          begin
            Options := [ixUnique];
            if C.ConstraintType = ctPrimaryKey then Options := Options + [ixPrimary];
            TIndexDef.Create(Result, C.ConstraintName, Fields, Options);
          end;
        end;
      end;
    end;
  end;
end;

function TOracleDataSet.PSGetDefaultOrder: TIndexDef;
var BeforeWhere, WhereClause, s, WhereWord, FieldName, Fields, DescFields, LastField, CaseInsFields: widestring;
    op, i, p1, p2, DotPos: Integer;
    FieldPos, ValCode: Integer;
    Done, CaseIns: Boolean;
const LocalBlanks = [' ', #9, #13, #10, '"', ','];
begin
  // Presume no order by
  Result := nil;
  if not (opNoDefaultOrder in ProviderOptions) then
  begin
    // Split the SQL to find the order by clause
    SplitSelect(SQL.Text, False, BeforeWhere, WhereClause, s, WhereWord);
    s := AnsiUpperCase(RemoveSQLComment(s, False));
    op := WordPos('ORDER', s);
    // Found ORDER
    if op > 0 then
    begin
      System.Delete(s, 1, op + 4);
      op := WordPos('BY', s);
      // Found BY
      if op > 0 then
      begin
        Fields := '';
        DescFields := '';
        LastField := '';
        CaseInsFields := '';
        i := op + 2;
        Done := False;
        // Find all order_by fields
        while (not Done) and (i <= Length(s)) do
        begin
          p1 := i;
          while (p1 <= Length(s)) and WideCharIn(s[p1], LocalBlanks) do Inc(p1);
          p2 := p1;
          while (p2 <= Length(s)) and not WideCharIn(s[p2], LocalBlanks) do Inc(p2);
          FieldName := System.Copy(s, p1, p2 - p1);
          // Check for upper(field), and create a case insensitive field if so
          CaseIns := False;
          if (Length(FieldName) > 0) and
             (Copy(FieldName, 1, 6) = 'UPPER(') and
             (FieldName[Length(FieldName)] = ')') then
          begin
            CaseIns := True;
            FieldName := Trim(Copy(FieldName, 7, Length(FieldName) - 7));
          end;
          // Skip ASC keywords
          if FieldName <> 'ASC' then
          begin
            // Interpret DESC keywords
            if FieldName = 'DESC' then
            begin
              // Add the last field to the descending fields
              if LastField <> '' then
              begin
                if DescFields <> '' then DescFields := DescFields + ';';
                DescFields := DescFields + LastField;
              end;
            end else begin
              // After FOR we're done
              if FieldName = 'FOR' then
                Done := True
              else begin
                // Check if it's a field position rather than the name
                Val(FieldName, FieldPos, ValCode);
                if ValCode = 0 then
                begin
                  if (FieldPos > 0) and (FieldPos <= Query.FieldCount) then
                    FieldName := Query.FieldName(FieldPos - 1);
                end;
                // Remove any table/alias prefix from the column name
                repeat
                  DotPos := AnsiPos('.', FieldName);
                  if DotPos > 0 then System.Delete(FieldName, 1, DotPos);
                until DotPos <= 0;
                // Check if the field name is valid
                if Query.FieldIndex(FieldName) < 0 then
                begin
                  Done := True;
                  Fields := '';
                end else begin
                  if Fields <> '' then Fields := Fields + ';';
                  Fields := Fields + FieldName;
                  if CaseIns then
                  begin
                    if CaseInsFields <> '' then CaseInsFields := CaseInsFields + ';';
                    CaseInsFields := CaseInsFields + FieldName;
                  end;
                  LastField := FieldName;
                end;
              end;
            end;
          end;
          i := p2;
        end;
        // If everything is okay, create the TIndexDef
        if Fields <> '' then
        begin
          Result := TIndexDef.Create(nil, '', Fields, []);
          Result.DescFields := DescFields;
          Result.CaseInsFields := CaseInsFields;
        end;
      end;
    end;
  end;
end;

function TOracleDataSet.GetKeyFields: string;
var i: Integer;
    T: TOracleTableInfo;
    C1, C2: TOracleConstraintInfo;
    s: string;
begin
  // First use the UniqueFields of the DataSet
  Result := Trim(FUniqueFields);
  // If these are not defined either, use a primary or unique key
  if Result = '' then
  begin
    // We need the query here
    if Query = nil then GetQuery;
    if Query.FieldCount = 0 then Query.Describe;
    T := GetOracleTableInfo;
    T.GetConstraints(Self);
    C2 := nil;
    for i := 0 to T.OracleConstraintInfoList.Count - 1 do
    begin
      C1 := T.OracleConstraintInfoList.Items[i] as TOracleConstraintInfo;
      C1.ReadFromDictionary(Self);
      if (C1.ConstraintType = ctPrimaryKey) then
      begin
        C2 := C1;
        Break;
      end;
      if (C1.ConstraintType = ctUniqueKey) and (C2 = nil) then C2 := C1;
    end;
    if C2 <> nil then
    begin
      for i := 0 to C2.ColumnNames.Count - 1 do
      begin
        s := C2.ColumnNames[i];
        if Query.FieldIndex(s) < 0 then
        begin
          Result := '';
          Break;
        end;
        if i <> 0 then Result := Result + ';';
        Result := Result + s;
      end;
    end;
  end;
end;

function TOracleDataSet.PSGetKeyFields: string;
begin
  if opNoKeyFields in ProviderOptions then
    Result := ''
  else begin
    // First try to obtain the unique fields through the ProviderFlags of the Fields
    Result := inherited PSGetKeyFields;
    // If these are not defined, use the UniqueFields or constraints of the DataSet
    if Result = '' then
    begin
      if (Trim(FUniqueFields) <> '') or (Trim(FUpdatingTable) <> '') or (not IsCursorDataSet) then
        if not Activating then Result := GetKeyFields;
    end;
    // If no KeyFields can be found, the provider will raise an exception
  end;
end;

function TOracleDataSet.PSGetParams: TParams;
var v, p: Integer;
    Param: TParam;
    ParamName: string;
    DataType: TFieldType;
begin
  // Create IParams if necessary
  if IParams = nil then IParams := TParams.Create(Self);
  // Delete unused parameters
  for p := IParams.Count - 1 downto 0 do
  begin
    v := VariableIndex(IParams[p].Name);
    if (v < 0) then IParams[p].Free;
  end;
  // Create & Update parameters based on variables
  for v := 0 to VariableCount - 1 do
  begin
    case VariableType(v) of
       otString: DataType := ftString;
         otDate,
    otTimestamp,
  otTimestampTZ,
 otTimestampLTZ: DataType := ftDateTime;
        otFloat: DataType := ftFloat;
      otInteger: DataType := ftInteger;
      otLongRaw: DataType := ftBlob;
         otBLOB,
        otBFile: DataType := ftOraBlob;
         otLong: DataType := ftMemo;
         otCLOB: DataType := ftOraClob;
       otCursor: DataType := ftCursor;
    else
      DataType := ftUnknown;
    end;
    ParamName := Copy(VariableName(v), 2, 30);
    Param := IParams.FindParam(ParamName);
    if (Param <> nil) and (Param.DataType <> DataType) then FreeAndNil(Param);
    if Param = nil then
      Param := IParams.CreateParam(DataType, ParamName, ptInput);
    if not (VariableType(v) in [otBLOB, otCLOB, otCursor]) then
      Param.Value := GetVariable(VariableName(v));
  end;
  Result := IParams;
end;

function TOracleDataSet.PSGetQuoteChar: string;
begin
  Result := '"';
end;

function TOracleDataSet.PSGetTableName: string;
begin
  Result := GetUpdatingTable;
end;

(*
function TDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode else
    PrevErr := 0;
  Result := EUpdateError.Create(E.Message, '', 1, PrevErr, E);
end;
*)

function TOracleDataSet.PSInTransaction: Boolean;
begin
  Result := FLogChanges;
end;

function TOracleDataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TOracleDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TOracleDataSet.PSReset;
begin
  Close;
end;

procedure TOracleDataSet.PSSetCommandText(const CommandText: string);
begin
  SQL.Text := CommandText;
  Active := False;
end;

procedure TOracleDataSet.PSLobsInit;
begin
  PSLobsFree;
  PSLobs := TList.Create;
end;

procedure TOracleDataSet.PSLobsWrite;
var l: Integer;
    LOB: TLOBLocator;
    s: string;
begin
  if PSLobs <> nil then
  begin
    for l := 0 to PSLobs.Count - 1 do
    begin
      LOB := PSLobs[l];
      s := IParams[LOB.Tag].AsBlob;
      if Length(s) > 0 then
      begin
        if LOB.LOBType = otCLOB then s := RemoveCR(s, Session);
        LOB.Write(s[1], Length(s));
      end;
    end;
  end;
end;

procedure TOracleDataSet.PSLobsFree;
var i: Integer;
begin
  if PSLobs <> nil then
  begin
    for i := 0 to PSLobs.Count - 1 do TLOBLocator(PSLobs[i]).Free;
    FreeAndNil(PSLobs);
  end;
end;

procedure TOracleDataSet.PSSetParams(AParams: TParams);
var p, DataType: Integer;
    LOB: TLOBLocator;
begin
  ProviderDataSet := True;
  PSLobsInit;
  DeleteVariables;
  for p := 0 to AParams.Count - 1 do
  begin
    case AParams[p].DataType of
       ftString,
   ftWideString: DataType := otString;
         ftDate,
         ftTime,
     ftDateTime: DataType := otDate;
        ftFloat,
     ftCurrency,
          ftBCD: DataType := otFloat;
      ftInteger,
     ftSmallInt,
         ftWord: DataType := otInteger;
         ftBlob: DataType := otLongRaw;
      ftOraBLOB: DataType := otBLOB;
         ftMemo: DataType := otLong;
      ftOraCLOB: DataType := otCLOB;
       ftCursor: DataType := otCursor;
    else
      DataType := -1;
    end;
    if DataType >= 0 then
    begin
      DeclareVariable(AParams[p].Name, DataType);
      if not (DataType in [otBLOB, otCLOB, otCursor]) then
        SetVariable(AParams[p].Name, AParams[p].Value);
      if DataType in [otBLOB, otCLOB] then
      begin
        LOB := TLOBLocator.Create(Session, DataType);
        LOB.ParentMonID := MonID;
        LOB.Name := AParams[p].Name;
        LOB.Tag := p;
        SetComplexVariable(AParams[p].Name, LOB);
        PSLobs.Add(LOB);
      end;
    end;
  end;
  if IParams = nil then IParams := TParams.Create(Self);
  IParams.Assign(AParams);
end;

procedure TOracleDataSet.PSStartTransaction;
begin
  StartUpdates;
  CommitOnPost := False;
end;

(*
function TDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
begin
  Result := False;
end;
*)

{$ENDIF CompilerVersion5}

function TOracleDataSet.CreateObject(const FieldName: string): TOracleObject;
var OFI, AFI: TOracleFieldInfo;
    f: Integer;
    AttrName: string;
begin
  CheckActive;
  OFI := FieldInfoList.FieldInfoByName(FieldName);
  if OFI = nil then
    DatabaseErrorFmt(SFieldNotFound, [Name, FieldName]);
  if OFI.DataType <> otObject then
    DatabaseErrorFmt(SFieldTypeMismatch, [FieldName]);
  Result := TOracleObject._Create(Session, OFI.TypeName, '', 0, FieldName);
  try
    for f := 0 to FieldInfoList.Count - 1 do
    begin
      AFI := FieldInfoList[f];
      if (AFI.InfoType = itAttr) and
         (AFI.Field <> nil) and
         (AFI.IsPartOf(OFI)) then
      begin
        AttrName := AFI.AttrName;
        if OFI.InfoType = itAttr then
          System.Delete(AttrName, 1, Length(OFI.AttrName) + 1);
        Result.SetAttr(AttrName, AFI.Field.Value);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

initialization

finalization
//  if RowIdAllocations <> 0 then
//    raise Exception.Create('Assertion failed: RowIdAllocations = 0');

end.
