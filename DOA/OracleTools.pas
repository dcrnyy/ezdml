// Direct Oracle Access - OracleTools (Wizard & Menu) unit
// Copyright 2000, 2005 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

// todo
// - Start PL/SQL Dev with userid= ?
// - Lots of stuff still disabled for Kylix

unit OracleTools;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,  StdCtrls, ComCtrls, Menus, FileCtrl, ShellAPI, ToolIntf,
  {$IFDEF CompilerVersion2005} ToolsAPI, {$ENDIF}
  ExptIntf, Oracle, OracleExplorer, OraclePreferences,
  OracleDefaults, Buttons, OracleVisual;

{$ELSE}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QExtCtrls,  QStdCtrls, QComCtrls, QMenus, ShellAPI, QImgList, Qt,
  ToolIntf, ExptIntf, Oracle, OracleExplorer, OraclePreferences,
  OracleDefaults, QButtons, OracleVisual;
{$ENDIF}

const
  WizardVersion = '1.0.3';

type
  TPackageDefinition = class;
  TPackageObject  = class;
  TPackageParameter = class;

  TPackageDefinition = class(TObject)
  private
    FOwner: string;
    FName: string;
    FObjects: TCollection;
    FSession: TOracleSession;
    FDelphiName: string;
    FUseVariants: Boolean;
    FUseSchemaName: Boolean;
    FUseOverload: Boolean;
    FIncludeSpec: Boolean;
    FIsComponent: Boolean;
    FUseDefaults: Boolean;
    FThreadSafe: Boolean;
    FPackageSpecification: string;
    FPureSpec: string;
    function GetPlSqlName: string;
    function GetObjectCount: Integer;
    function GetObject(Index: Integer): TPackageObject;
    function GetDelphiName: string;
    function GetInterfaceSource: string;
    function GetImplementationSource: string;
    function GetPackageSpecification: string;
  public
    constructor Create(const AOwner, AName: string; ASession: TOracleSession);
    destructor  Destroy; override;
    procedure ReadFromDictionary;
    function  NextSpecWord(var FromPos: Integer): string;
    property  UseVariants: Boolean read FUseVariants write FUseVariants;
    property  UseSchemaName: Boolean read FUseSchemaName write FUseSchemaName;
    property  UseOverload: Boolean read FUseOverload write FUseOverload;
    property  IncludeSpec: Boolean read FIncludeSpec write FIncludeSpec;
    property  IsComponent: Boolean read FIsComponent write FIsComponent;
    property  UseDefaults: Boolean read FUseDefaults write FUseDefaults;
    property  ThreadSafe: Boolean read FThreadSafe write FThreadSafe;
    property  Owner: string read FOwner;
    property  Name: string read FName;
    property  PlSqlName: string read GetPlSqlName;
    property  DelphiName: string read GetDelphiName write FDelphiName;
    property  ObjectCount: Integer read GetObjectCount;
    property  Objects[Index: Integer]: TPackageObject read GetObject; default;
    property  Session: TOracleSession read FSession;
    property  InterfaceSource: string read GetInterfaceSource;
    property  ImplementationSource: string read GetImplementationSource;
    property  PackageSpecification: string read GetPackageSpecification;
  end;

  TPackageObject = class(TCollectionItem)
  private
    FPackageDefinition: TPackageDefinition;
    FName: string;
    FOverloadId: string;
    FParameters: TCollection;
    FFlatParameters: TList;
    FIsFunction: Boolean;
    FSelected: Boolean;
    FDelphiName: string;
    FOverloads: TList;
    function  GetParameterCount: Integer;
    function  GetParameter(Index: Integer): TPackageParameter;
    function  GetResultParameter: TPackageParameter;
    function  GetAllParameterCount: Integer;
    function  GetAllParameter(Index: Integer): TPackageParameter;
    function  GetFlatParameterCount: Integer;
    function  GetFlatParameter(Index: Integer): TPackageParameter;
    function  GetIsSupported: Boolean;
    function  GetIsFunction: Boolean;
    function  GetIsOverloaded: Boolean;
    procedure AddOverload(AObject: TPackageObject);
    function  GetOverloadCount: Integer;
    function  GetOverload(Index: Integer): TPackageObject;
    function  GetOverloadIndex: Integer;
    function  GetObjectType: string;
    function  GetUseVariants: Boolean;
    function  GetDelphiName: string;
    function  GetPLSQLName: string;
    function  GetPLSQLBlock: string;
    function  GetInterfaceSource: string;
    function  GetImplementationSource: string;
  public
    constructor Make(APackageDefinition: TPackageDefinition; const AName, AOverloadId: string);
    destructor  Destroy; override;
    property  PackageDefinition: TPackageDefinition read FPackageDefinition;
    property  Name: string read FName;
    property  IsSupported: Boolean read GetIsSupported;
    property  IsFunction: Boolean read GetIsFunction;
    property  OverloadId: string read FOverloadId;
    property  IsOverloaded: Boolean read GetIsOverloaded;
    property  Overloads[Index: Integer]: TPackageObject read GetOverload;
    property  OverloadCount: Integer read GetOverloadCount;
    property  OverloadIndex: Integer read GetOverloadIndex;
    property  ObjectType: string read GetObjectType;
    property  Selected: Boolean read FSelected write FSelected;
    property  UseVariants: Boolean read GetUseVariants;
    property  DelphiName: string read GetDelphiName write FDelphiName;
    property  ParameterCount: Integer read GetParameterCount;
    property  Parameters[Index: Integer]: TPackageParameter read GetParameter; default;
    property  ResultParameter: TPackageParameter read GetResultParameter;
    property  AllParameterCount: Integer read GetAllParameterCount;
    property  AllParameters[Index: Integer]: TPackageParameter read GetAllParameter;
    property  FlatParameterCount: Integer read GetFlatParameterCount;
    property  FlatParameters[Index: Integer]: TPackageParameter read GetFlatParameter;
    property  PLSQLName: string read GetPLSQLName;
    property  PLSQLBlock: string read GetPLSQLBlock;
    property  InterfaceSource: string read GetInterfaceSource;
    property  ImplementationSource: string read GetImplementationSource;
  end;

  TPackageParameter = class(TCollectionItem)
  private
    FPackageObject: TPackageObject;
    FParent: TPackageParameter;
    FName: string;
    FMode: TParameterMode;
    FDataType: string;
    FDataLevel: Integer;
    FSequence: Integer;
    FDefaultValue: string;
    FDataLength: Integer;
    FDataPrecision: Integer;
    FDataScale: Integer;
    FTypeOwner: string;
    FTypeName: string;
    FTypeSubName: string;
    FParameters: TCollection;
    FDelphiName: string;
    FDelphiType: string;
    FPLSQLRecordType: string;
    function GetFullTypeName: string;
    function GetParameterCount: Integer;
    function GetParameter(Index: Integer): TPackageParameter;
    function GetPackageDefinition: TPackageDefinition;
    function GetPackageObject: TPackageObject;
    function GetUseVariants: Boolean;
    function GetIsBoolean: Boolean;
    function GetIsRecord: Boolean;
    function GetDelphiName: string;
    function GetDelphiNameQualified: string;
    function GetDelphiType: string;
    function GetDelphiExpression: string;
    function GetDelphiVarType: string;
    function GetIsComplex: Boolean;
    function GetIsLong: Boolean;
    function GetHasComplexParameters: Boolean;
    function GetHasSubTypes: Boolean;
    function GetIsPLSQLTable: Boolean;
    function GetDelphiConstructor(const ASessionName: string; Qualified: Boolean): string;
    function GetDelphiDestructor(Qualified: Boolean): string;
    function GetDelphiPreCall: string;
    function GetDelphiPostCall: string;
    function GetDelphiTypeInterface: string;
    function GetDelphiTypeImplementation: string;
    function GetPLSQLVarName: string;
    function GetBindVarName: string;
    function GetPLSQLDeclaration: string;
    function GetPLSQLPreCall: string;
    function GetPLSQLInCall: string;
    function GetPLSQLPostCall: string;
    function GetPLSQLRecordType: string;
  public
    constructor Make(APackageObject: TPackageObject; const AName: string);
    constructor MakeSub(AParent: TPackageParameter; const AName: string);
    destructor  Destroy; override;
    property  PackageDefinition: TPackageDefinition read GetPackageDefinition;
    property  PackageObject: TPackageObject read GetPackageObject;
    property  Parent: TPackageParameter read FParent;
    property  Name: string read FName;
    property  Mode: TParameterMode read FMode;
    property  DataType: string read FDataType;
    property  IsBoolean: Boolean read GetIsBoolean;
    property  IsRecord: Boolean read GetIsRecord;
    property  DataLevel: Integer read FDataLevel;
    property  Sequence: Integer read FSequence;
    property  DefaultValue: string read FDefaultValue;
    property  DataLength: Integer read FDataLength;
    property  DataPrecision: Integer read FDataPrecision;
    property  DataScale: Integer read FDataScale;
    property  TypeOwner: string read FTypeOwner;
    property  TypeName: string read FTypeName;
    property  TypeSubName: string read FTypeSubName;
    property  FullTypeName: string read GetFullTypeName;
    property  ParameterCount: Integer read GetParameterCount;
    property  Parameters[Index: Integer]: TPackageParameter read GetParameter; default;
    property  UseVariants: Boolean read GetUseVariants;
    property  DelphiName: string read GetDelphiName write FDelphiName;
    property  DelphiNameQualified: string read GetDelphiNameQualified;
    property  DelphiType: string read GetDelphiType;
    property  DelphiExpression: string read GetDelphiExpression;
    property  DelphiVarType: string read GetDelphiVarType;
    property  IsComplex: Boolean read GetIsComplex;
    property  IsLong: Boolean read GetIsLong;
    property  HasComplexParameters: Boolean read GetHasComplexParameters;
    property  HasSubTypes: Boolean read GetHasSubTypes;
    property  IsPLSQLTable: Boolean read GetIsPLSQLTable;
    property  DelphiPreCall: string read GetDelphiPreCall;
    property  DelphiPostCall: string read GetDelphiPostCall;
    property  DelphiTypeInterface: string read GetDelphiTypeInterface;
    property  DelphiTypeImplementation: string read GetDelphiTypeImplementation;
    property  PLSQLVarName: string read GetPLSQLVarName;
    property  BindVarName: string read GetBindVarName;
    property  PLSQLDeclaration: string read GetPLSQLDeclaration;
    property  PLSQLPreCall: string read GetPLSQLPreCall;
    property  PLSQLInCall: string read GetPLSQLInCall;
    property  PLSQLPostCall: string read GetPLSQLPostCall;
    property  PLSQLRecordType: string read GetPLSQLRecordType;
  end;

  TOracleToolItem = record
    Name: string;             // Menu item name in Oracle menu
    Page: string;             // The Page for the Wizard
    WizardName: string;       // The name for the Wizard (in the page)
    Comment: string;          // and the comment (in the page)
{$IFDEF CompilerVersion2005}
    MenuItem: TMenuItem;      // The actual menu
{$ELSE}
    MenuItem: TIMenuItemIntf; // The actual menu
{$ENDIF}
  end;

  TWizardForm = class(TForm)
    LeftPanel: TPanel;
    ImagePanel: TPanel;
    WizardPanel: TPanel;
    ButtonPanel: TPanel;
    PackageWizardControl: TPageControl;
    PackagePage1: TTabSheet;
    PackagePage3: TTabSheet;
    HelpBtn: TButton;
    BackBtn: TButton;
    NextBtn: TButton;
    CancelBtn: TButton;
    PackagePage4: TTabSheet;
    AddToProjectCheck: TCheckBox;
    OpenInIDECheck: TCheckBox;
    WizardSession: TOracleSession;
    PackageQuery: TOracleQuery;
    LogonPanel: TPanel;
    PackageListBox: TListBox;
    LogonBtn: TButton;
    WizardLogon: TOracleLogon;
    AllPackagesCheck: TCheckBox;
    PackageFunctionsTree: TTreeView;
    TreeImageList: TImageList;
    PathEdit: TEdit;
    Label6: TLabel;
    FileEdit: TEdit;
    Label7: TLabel;
    PathBrowsBtn: TSpeedButton;
    PackagePage2: TTabSheet;
    TPrefixCheck: TCheckBox;
    RemoveUSCheck: TCheckBox;
    UseVariantsCheck: TCheckBox;
    PackageCommentCheck: TCheckBox;
    ExamplePanel: TPanel;
    Label10: TLabel;
    CaseGroup: TRadioGroup;
    ProgressPanel1: TPanel;
    PackageBevel2: TBevel;
    GenerateOverloadCheck: TCheckBox;
    PrefixSchemaCheck: TCheckBox;
    ComponentGroup: TGroupBox;
    CreateComponentCheck: TCheckBox;
    CreateResourceCheck: TCheckBox;
    PaletteEdit: TComboBox;
    PaletteLabel: TLabel;
    PackageBevel4: TBevel;
    TitlePanel2: TPanel;
    HandImage2: TImage;
    Label8: TLabel;
    Label9: TLabel;
    TitlePanel1: TPanel;
    HandImage1: TImage;
    Label12: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    TitlePanel3: TPanel;
    HandImage3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    TitlePanel4: TPanel;
    HandImage4: TImage;
    Label4: TLabel;
    Label5: TLabel;
    UseDefaultsCheck: TCheckBox;
    TestAllArgumentsQuery: TOracleQuery;
    ProgressBar1: TProgressBar;
    ProgressPanel2: TPanel;
    ProgressBar2: TProgressBar;
    ThreadSafeCheck: TCheckBox;
    APrefixCheck: TCheckBox;
    PackagePage0: TTabSheet;
    DontShowAgain: TCheckBox;
    InfoPanel: TPanel;
    InfoMemo: TMemo;
    InfoLabel: TLabel;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure LogonBtnClick(Sender: TObject);
    procedure WizardSessionChange(Sender: TOracleSession);
    procedure PackageListBoxClick(Sender: TObject);
    procedure AllPackagesCheckClick(Sender: TObject);
    procedure PackageFunctionsTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SourceStyleChange(Sender: TObject);
    procedure NameStyleChange(Sender: TObject);
    procedure PackageFunctionsTreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure PathBrowsBtnClick(Sender: TObject);
    procedure FileEditChange(Sender: TObject);
    procedure PackageListBoxDblClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure UseVariantsCheckClick(Sender: TObject);
    procedure PackageFunctionsTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    SelectedPackages: TStringList;
    NameStyleChanged: Boolean;
    AllArgumentsOkay: Boolean;
    function  FinishWizard: Boolean;
    procedure ClearPackageFunctions;
    procedure FetchPackages;
    procedure AdjustObjects;
    procedure SetButtons;
    function  ConvertName(Name: string; IsObject, IsParameter: Boolean): string;
    procedure ConvertNames;
    procedure TestAllArguments;
  end;

  // Execute a specific menu
  TOracleToolExecutor = class(TObject)
  public
    procedure ClickPackageWizard(ASender: TObject);
    procedure ClickExplorer(ASender: TObject);
    procedure ClickMonitor(ASender: TObject);
    procedure ClickPSD(ASender: TObject);
    procedure ClickHelp(ASender: TObject);
    procedure ClickDoc(ASender: TObject);
    procedure ClickAddendum(ASender: TObject);
    procedure ClickPreferences(ASender: TObject);
    procedure ClickPropertyDefaults(ASender: TObject);
    procedure ClickInfo(ASender: TObject);
    function GetOnClick(MenuIndex: Integer): TNotifyEvent;
    procedure Execute(MenuIndex: Integer);
  end;

const // This array defines all menu items and wizards
  OracleToolCount = 13;
  OracleToolItems: array[1..OracleToolCount] of TOracleToolItem =
  (
   (Name: '&Package Wizard';   Page: 'New'; WizardName: 'Oracle Package'; Comment: 'Creates encapsulation classes for Oracle Packages'),
   (Name: '-';                 Page: ''),
   (Name: '&Explorer...';      Page: ''),
   (Name: '&Monitor...';       Page: ''),
   (Name: '&PL/SQL Developer...'; Page: ''),
   (Name: '-';                 Page: ''),
   (Name: '&Help...';          Page: ''),
   (Name: '&User''s Guide...'; Page: ''),
   (Name: '4.1 &Addendum...';  Page: ''),
   (Name: '-';                 Page: ''),
   (Name: 'P&references...';   Page: ''),
   (Name: 'Property Defaults...'; Page: ''),
   (Name: '&Info...';          Page: '')
  );

function  DoPackageWizard: Boolean;
procedure RegisterOracleTools;
function  SaveResourceFile(Filename: string; ImageNames: TStringList): Boolean;

implementation

const
  // Delphi reserved words and words that would cause a name clash
  ReservedWordCount = 80;
  ReservedWords: array[0 .. ReservedWordCount - 1] of string =
    ('and', 'array', 'as', 'asm', 'at', 'automated', 'begin', 'case', 'class',
     'const', 'constructor', 'destructor', 'dispinterface', 'div', 'do',
     'downto', 'else', 'end', 'except', 'exports', 'file', 'finalization',
     'finally', 'for', 'function', 'goto', 'if', 'implementation', 'in',
     'inherited', 'initialization', 'inline', 'interface', 'is', 'label',
     'library', 'mod', 'nil', 'not', 'object', 'of', 'on', 'or', 'out',
     'packed', 'private', 'procedure', 'program', 'property', 'protected',
     'public', 'published', 'raise', 'record', 'repeat', 'resourcestring',
     'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type',
     'unit', 'until', 'uses', 'var', 'while', 'with', 'xor',
     'result', 'ocpquery', 'getquery', 'threadrelease', 'threadacquire',
     'create', 'destroy', 'free');

var
  // The sequence number for system generated type names within this unit
  UnitTypeSequence: Integer;
  // The main form, we need it in various low level object types
  MainForm: TWizardForm;
  OracleExecutor: TOracleToolExecutor;

{$R *.dfm}
{$R OracleTools.res}

function GetStateIndex(Node: TTreeNode): Integer;
begin
  {$IFDEF LINUX}
  Result := 0;
  {$ELSE}
  Result := Node.StateIndex;
  {$ENDIF}
end;

procedure SetStateIndex(Node: TTreeNode; Index: Integer);
begin
  {$IFDEF LINUX}
  {$ELSE}
  Node.StateIndex := Index;
  {$ENDIF}
end;

// Execute the Package Wizard
function DoPackageWizard: Boolean;
begin
  Result := False;
  MainForm := TWizardForm.Create(nil);
  with MainForm do
  try
    SetButtons;
    if ShowModal = mrOK then
    begin
      Result := True;
    end;
  finally
    Free;
  end;
end;

function Execute(Exe, Params: string): Integer;
begin
  Exe    := QuoteTrim(Exe);
  Params := QuoteTrim(Params);
  {$IFDEF LINUX}
  Result := 0;
  {$ELSE}
  Result := ShellExecute(Application.Handle, nil, PChar(Exe),  PChar(Params), nil, sw_ShowNormal);
  {$ENDIF}
end;

procedure ShowExecuteError(App, Exe, Msg: string);
var S: string;
begin
  S := 'Error opening ' + App + #13#10;
  if Exe <> '' then S := S + '(' + Exe + ')' + #13#10;
  S := S + #13#10 + Msg;
  {$IFNDEF LINUX}
  MessageBeep(MB_ICONEXCLAMATION);
  {$ENDIF}
  MessageDlg(PChar(S), mtError, [mbOK], 0);
end;

// Execute the Oracle Monitor
function ExecuteOracleMonitor: Boolean;
{$IFNDEF LINUX}
var Exe: string;
{$ENDIF}
begin
  Result := False;
  {$IFNDEF LINUX}
  Exe := ReadRegString(HKEY_CURRENT_USER, 'Software\Allround Automations\OracleMonitor', 'Executable');
  if Exe <> '' then Result := Execute(Exe, '') > 32;
  if not Result then
    ShowExecuteError('OracleMonitor', Exe, 'You need to have a properly installed version available.');
  {$ENDIF}
end;

// Execute PL/SQL Developer
function ExecutePLSQLDeveloper: Boolean;
{$IFNDEF LINUX}
var Exe: string;
    WHandle: THandle;
{$ENDIF}
begin
  Result := False;
  {$IFNDEF LINUX}
  WHandle := FindWindow('TPLSQLDevForm', nil);
  // If active, bring it to front
  if WHandle <> 0 then
  begin
    if IsIconic(WHandle) then ShowWindow(WHandle, SW_RESTORE);
    BringWindowToTop(WHandle);
    Result := True;
  end else begin
    Exe := ReadRegString(HKEY_CLASSES_ROOT, 'PL/SQL Developer\Shell\Open\Command', '');
    if Exe <> '' then Result := Execute(Exe, '') > 31;
  end;
  if not Result then
    ShowExecuteError('PL/SQL Developer', Exe, 'You need to have a properly installed version available.');
  {$ENDIF}
end;

// Open documentation
function ExecuteOracleDoc(Doc: string): Boolean;
{$IFNDEF LINUX}
var S: string;
{$ENDIF}
begin
  Result := False;
  {$IFNDEF LINUX}
  S := GetCompilerName + IntToStr(GetCompilerVersion);
  S := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\Allround Automations\DOA\Install\' + S, 'InstalledDocPath');
  {$IFDEF CompilerVersion2006}
  if not FileExists(S + '\' + Doc) then
  begin
    S := GetCompilerName + IntToStr(2007);
    S := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\Allround Automations\DOA\Install\' + S, 'InstalledDocPath');
  end;
  {$ENDIF}
  if S <> '' then Result := Execute(S + '\' + Doc, '') > 31;
  if not Result then ShowExecuteError('Documentation', S + '\' + Doc, 'Please make sure you have installed Direct Oracle Access correctly');
  {$ENDIF}
end;

// A 'Quick & Dirty' way to create a dcr file (don't even try to understand this)
function SaveResourceFile(Filename: string; ImageNames: TStringList): Boolean;
type ByteArray = array[0..999] of byte;
var BMP: TBitMap;
    F: File;
    i: Integer;
 procedure WriteResourceHeader;
 const H: Array[1..8] of Integer = (0, $20, $FFFF, $FFFF, 0, 0, 0, 0);
 begin
   BlockWrite(F, H[1], SizeOf(H));
 end;
 procedure WriteImageHeader(Name: string);
 const H1: Array[1..3] of Integer = ($188, 0, $2FFFF);
 const H2: Array[1..4] of Integer = ($04131010, 0, 0, $28);
 const H3: Array[1..9] of Integer = ($18, $18, $40001, 0, 0, 0, 0, 0, 0);
 var Size, i: Integer;
     T: array[0..255] of Byte;
 begin
   Size := Length(Name) * 2 + 6;
   while (Size and $03) <> 0 do inc(Size);
   H1[2] := Size + 24;
   BlockWrite(F, H1[1], SizeOf(H1));
   FillChar(T, SizeOf(T), #0);
   for i := 1 to Length(Name) do T[i * 2 - 2] := Ord(Name[i]);
   BlockWrite(F, T[0], Size);
   BlockWrite(F, H2[1], SizeOf(H2));
   BlockWrite(F, H3[1], SizeOf(H3));
 end;
 procedure WriteImageData;
 const P: Array[1..16] of Integer = (
       $000000, $800000, $008000, $808000, $000080, $800080, $008080, $C0C0C0,
       $808080, $FF0000, $00FF00, $FFFF00, $0000FF, $FF00FF, $00FFFF, $FFFFFF);
 var D: Pointer;
     y: Integer;
 begin
   BlockWrite(F, P[1], SizeOf(P));
   for y := 23 downto 0 do
   begin
     D := BMP.Scanline[y];
     BlockWrite(F, D^, 12);
   end;
 end;
begin
  Result := False;
  BMP := TBitMap.Create;
  try
    BMP.LoadFromResourceName(HInstance, 'DEFAULTPACKAGE');
    if BMP.Width > 23 then
    try
      AssignFile(F, Filename);
      Rewrite(F, 1);
      for i := 0 to ImageNames.Count - 1 do
      begin
        if i = 0 then WriteResourceHeader;
        WriteImageHeader(UpperCase(ImageNames[i]));
        WriteImageData;
      end;
      CloseFile(F);
      Result := True;
    except
    end;
  finally
    BMP.Free;
  end;
end;

// Return the compiler Name & Version
function CompilerVersion: string;
begin
  Result := GetCompilerName + IntToStr(GetCompilerVersion);
end;

// Get the OS user
function OSUser: string;
{$IFNDEF LINUX}
var Username: array[0..256] of char;
    Len: dword;
{$ENDIF}
begin
{$IFNDEF LINUX}
  Len := 255;
  GetUsername(Username, Len);
  Result := Username;
{$ELSE}
  Result := 'Kylix';
{$ENDIF}
end;

procedure TOracleToolExecutor.ClickPropertyDefaults(ASender: TObject);
begin
  ExecuteDefaultsEditor(nil);
end;

procedure TOracleToolExecutor.ClickPackageWizard(ASender: TObject);
begin
  DoPackageWizard;
end;

procedure TOracleToolExecutor.ClickPreferences(ASender: TObject);
begin
  DoOraclePreferences;
end;

procedure TOracleToolExecutor.ClickHelp(ASender: TObject);
begin
{$IFNDEF LINUX}
  WinHelp(Application.Handle, PChar(DOAHelpFile), Help_Index, 0);
{$ENDIF}
end;

procedure TOracleToolExecutor.ClickInfo(ASender: TObject);
begin
  ShowDOAInfo(nil);
end;

procedure TOracleToolExecutor.ClickDoc(ASender: TObject);
begin
  ExecuteOracleDoc('manual.pdf');
end;

procedure TOracleToolExecutor.ClickAddendum(ASender: TObject);
begin
  ExecuteOracleDoc('Addendum_4.1.pdf');
end;

procedure TOracleToolExecutor.ClickExplorer(ASender: TObject);
begin
  ExecuteExplorer(nil, nil);
end;

procedure TOracleToolExecutor.ClickPSD(ASender: TObject);
begin
  ExecutePLSQLDeveloper;
end;

procedure TOracleToolExecutor.ClickMonitor(ASender: TObject);
begin
  ExecuteOracleMonitor;
end;

function TOracleToolExecutor.GetOnClick(MenuIndex: Integer): TNotifyEvent;
begin
  case MenuIndex of
    1 : Result := ClickPackageWizard;
    3 : Result := ClickExplorer;
    4 : Result := ClickMonitor;
    5 : Result := ClickPSD;
    7 : Result := ClickHelp;
    8 : Result := ClickDoc;
    9 : Result := ClickAddendum;
   11 : Result := ClickPreferences;
   12 : Result := ClickPropertyDefaults;
   13 : Result := ClickInfo;
   else Result := nil;
  end;
end;

procedure TOracleToolExecutor.Execute(MenuIndex: Integer);
var
  evtClick: TNotifyEvent;
begin
  evtClick := GetOnClick(MenuIndex);
  if Assigned(evtClick) then
    evtClick(nil);
end;

// Pad a string with spaces
function StrPad(const s: string; Size: Integer): string;
begin
  if Length(s) >= Size then
    Result := s
  else
    Result := s + StringOfChar(' ', Size - Length(s));
end;

// Indent a string with spaces
function StrIndent(const s: string; Indent: Integer): string;
var i: Integer;
    Pad: string;
begin
  Result := '';
  Pad := StringOfChar(' ', Indent);
  for i := 1 to Length(s) do
  begin
    if (i = 1) or (s[i - 1] = #10) then Result := Result + Pad;
    Result := Result + s[i];
  end;
end;

// Return a string as a quoted expression
function StrQuoted(const s: string): string;
var i: Integer;
begin
  Result := '''';
  for i := 1 to Length(s) do
  begin
    if s[i] = '''' then Result := Result + '''';
    Result := Result + s[i];
  end;
  Result := Result + '''';
end;

// Remove double quotes from a string
function StrNoDoubleQuotes(const s: string): string;
var i: Integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do if Result[i] = '"' then Delete(Result, i, 1);
end;

// Is a string a reserved word?
function StrReserved(const s: string): Boolean;
var ls: string;
    i: Integer;
begin
  ls := AnsiLowerCase(s);
  Result := False;
  for i := 0 to ReservedWordCount - 1 do
    if ls = ReservedWords[i] then Result := True;
end;

// TPackageDefinition

constructor TPackageDefinition.Create(const AOwner, AName: string; ASession: TOracleSession);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FSession := ASession;
  FObjects := TCollection.Create(TPackageObject);
end;

destructor TPackageDefinition.Destroy;
begin
  FObjects.Free;
  inherited;
end;

// Get the PL/SQL name for this package
function TPackageDefinition.GetPlSqlName: string;
begin
  if UseSchemaName then Result := '"' + Owner + '".' else Result := '';
  Result := Result + '"' + Name + '"';
end;

// The objects (functions and procedures) of the package
function TPackageDefinition.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TPackageDefinition.GetObject(Index: Integer): TPackageObject;
begin
  Result := FObjects.Items[Index] as TPackageObject;
end;

// Get the Delphi name for this package
function TPackageDefinition.GetDelphiName: string;
begin
  if FDelphiName <> '' then Result := FDelphiName else Result := Name;
end;

// Read the package definition from the database dictionary
procedure TPackageDefinition.ReadFromDictionary;
var Query: TOracleQuery;
    Obj: TPackageObject;
    LastObject, LastOverload: string;
    ThisObject, ThisOverload: string;
    ThisDataType: string;
    Par, ParentParam: TPackageParameter;
    ThisDataLevel: Integer;
    ParentIndex: Integer;
begin
  FObjects.Clear;
  Query := TOracleQuery.Create(nil);
  try
    Query.Session := Session;
    Query.SQL.Add('select * from sys.all_arguments');
    Query.SQL.Add('where package_name = :package_name');
    Query.SQL.Add('and owner = :owner');
    Query.SQL.Add('order by object_name, overload, sequence');
    Query.DeclareVariable('package_name', otString);
    Query.DeclareVariable('owner', otString);
    Query.SetVariable('package_name', Name);
    Query.SetVariable('owner', Owner);
    Query.Execute;
    while not Query.Eof do
    begin
      ThisObject   := Query.Field('object_name');
      ThisOverload := Query.Field('overload');
      LastObject := ThisObject;
      LastOverload := ThisOverload;
      Obj := TPackageObject.Make(Self, ThisObject, ThisOverload);
      repeat
        ThisDataLevel := Query.Field('data_level');
        ThisDataType  := Query.Field('data_type');
        Par := nil;
        if ThisDataType <> '' then
        begin
          if ThisDataLevel = 0 then
          begin
            Par := TPackageParameter.Make(Obj, Query.Field('argument_name'));
          end else begin
            ParentIndex := Obj.FlatParameterCount;
            repeat
              Dec(ParentIndex);
              ParentParam := Obj.FlatParameters[ParentIndex];
            until (ParentParam.DataLevel = ThisDataLevel - 1) or (ParentIndex = 0);
            Par := TPackageParameter.MakeSub(ParentParam, Query.Field('argument_name'))
          end;
        end;
        if Par <> nil then
        begin
          Par.FDataLevel := ThisDataLevel;
          Par.FSequence := Query.Field('sequence');
          Par.FMode := pmIn;
          if Query.Field('in_out') = 'OUT' then Par.FMode := pmOut;
          if Query.Field('in_out') = 'IN/OUT' then Par.FMode := pmInOut;
          Par.FDataType := Query.Field('data_type');
          Par.FDefaultValue := Query.Field('default_value');
          Par.FDataLength := Query.Field('data_length');
          Par.FDataPrecision := Query.Field('data_precision');
          Par.FDataScale := Query.Field('data_scale');
          if Query.FieldIndex('type_owner') >= 0 then
            Par.FTypeOwner := Query.Field('type_owner')
          else
            Par.FTypeOwner := '';
          if Query.FieldIndex('type_name') >= 0 then
            Par.FTypeName := Query.Field('type_name')
          else
            Par.FTypeName := '';
          if Query.FieldIndex('type_subname') >= 0 then
            Par.FTypeSubName := Query.Field('type_subname')
          else
            Par.FTypeSubName := '';
        end;
        Query.Next;
        if not Query.Eof then
        begin
          ThisObject   := Query.Field('object_name');
          ThisOverload := Query.Field('overload');
        end;
      until Query.Eof or (ThisObject <> LastObject) or (ThisOverload <> LastOverload);
      Obj.Selected := Obj.IsSupported;
    end;
  except
    FObjects.Clear;
    Query.Free;
    raise;
  end;
  Query.Free;
end;

// Get the next word in the package specification after the given position
function TPackageDefinition.NextSpecWord(var FromPos: Integer): string;
const Separators = [' ', #9, #10, #12, #13, '(', ')', ',', ':', ';'];
begin
  GetPackageSpecification;
  while (FromPos <= Length(FPureSpec)) and
        ((FromPos < 1) or (FPureSpec[FromPos] in Separators)) do Inc(FromPos);
  Result := '';
  while (FromPos <= Length(FPureSpec)) and not (FPureSpec[FromPos] in Separators) do
  begin
    Result := Result + FPureSpec[FromPos];
    Inc(FromPos);
  end;
end;

// Fetch the package specification source from the database
function TPackageDefinition.GetPackageSpecification: string;
var Query: TOracleQuery;
    S: TStringList;
    CPos: Integer;
begin
  if FPackageSpecification = '' then
  begin
    Query := TOracleQuery.Create(nil);
    S := TStringList.Create;
    try
      Query.Session := Session;
      Query.SQL.Add('select text from sys.all_source');
      Query.SQL.Add('where owner = :owner');
      Query.SQL.Add('and name = :name');
      Query.SQL.Add('and type = ''PACKAGE''');
      Query.SQL.Add('order by line');
      Query.DeclareVariable('name', otString);
      Query.SetVariable('name', Name);
      Query.DeclareVariable('owner', otString);
      Query.SetVariable('owner', Owner);
      Query.Execute;
      while not Query.Eof do
      begin
        S.Add(TrimRight(Query.FieldAsString(0)));
        Query.Next;
      end;
      FPackageSpecification := TrimRight(S.Text);
      repeat
        CPos := Pos('(*', FPackageSpecification);
        if CPos <= 0 then CPos := Pos('*)', FPackageSpecification);
        if CPos > 0 then Insert(' ', FPackageSpecification, CPos + 1);
      until CPos <= 0;
      if Trim(FPackageSpecification) = '' then
        FPackageSpecification := 'Package specification source not available';
      if Pos(' WRAPPED', AnsiUpperCase(FPackageSpecification)) > 0 then
        FPackageSpecification := 'Package specification source is wrapped';
    except
      on E:Exception do FPackageSpecification := Trim(E.Message);
    end;
    Query.Free;
    S.Free;
    FPureSpec := RemoveSQLComment(FPackageSpecification, False);
  end;
  Result := FPackageSpecification;
end;

// Get the Delphi source for the interface of the package
function TPackageDefinition.GetInterfaceSource: string;
var S: TStringList;
    i: Integer;
begin
  S := TStringList.Create;
  if IncludeSpec and not IsComponent then
  begin
    S.Add('(*');
    S.Add(PackageSpecification);
    S.Add('*)');
  end;
  S.Add(Format('  %s = class(TOracleCustomPackage)', [DelphiName]));
  if IncludeSpec and IsComponent then
  begin
    S.Add('  protected');
    S.Add('    function  GetPackageSpecification: TStrings; override;');
  end;
  S.Add('  public');
  for i := 0 to ObjectCount - 1 do
    if Objects[i].Selected then S.Add(StrIndent(Objects[i].InterfaceSource, 4));
  S.Add('  published');
  S.Add('    property Name;');
  S.Add('    property Session;');
  S.Add('    property Cursor;');
  if IncludeSpec and IsComponent then S.Add('    property PackageSpecification;');
  S.Add('  end;');
  Result := TrimRight(S.Text);
  S.Free;
end;

// Get the Delphi source for the implementation of the package
function TPackageDefinition.GetImplementationSource: string;
var S, PS: TStringList;
    i: Integer;
begin
  S := TStringList.Create;
  for i := 0 to ObjectCount - 1 do
  begin
    if Objects[i].Selected then
    begin
      S.Add(Objects[i].ImplementationSource);
      S.Add('');
    end;
  end;
  if IncludeSpec and IsComponent then
  begin
    S.Add('function ' + DelphiName + '.GetPackageSpecification: TStrings;');
    S.Add('begin');
    S.Add('  Result := inherited GetPackageSpecification;');
    PS := TStringList.Create;
    PS.Text := PackageSpecification;
    for i := 0 to PS.Count - 1 do
      S.Add('  Result.Add(' + StrQuoted(PS[i]) + ');');
    PS.Free;
    S.Add('end;');
    S.Add('');
  end;
  Result := Trim(S.Text);
  S.Free;
end;

// TPackageObject

constructor TPackageObject.Make(APackageDefinition: TPackageDefinition; const AName, AOverloadId: string);
var i: Integer;
begin
  inherited Create(APackageDefinition.FObjects);
  FPackageDefinition := APackageDefinition;
  FName := AName;
  FOverloadId := AOverloadId;
  FParameters := TCollection.Create(TPackageParameter);
  FFlatParameters := TList.Create;
  FIsFunction := False;
  FOverloads := TList.Create;
  FSelected := True;
  for i := 0 to PackageDefinition.ObjectCount - 2 do
  begin
    if PackageDefinition[i].Name = Name then
    begin
      PackageDefinition[i].AddOverload(Self);
      AddOverload(PackageDefinition[i]);
    end;
  end;
  AddOverload(Self);
end;

destructor TPackageObject.Destroy;
begin
  FParameters.Free;
  FFlatParameters.Free;
  FOverloads.Free;
  inherited;
end;

// Is this object a function?
function TPackageObject.GetIsFunction: Boolean;
begin
  Result := (ResultParameter <> nil);
end;

// Is this object supported by the Package Wizard?
function TPackageObject.GetIsSupported: Boolean;
var i: Integer;
begin
  Result := True;
  for i := 0 to FlatParameterCount - 1 do
    if FlatParameters[i].IsPLSQLTable and FlatParameters[i].Parameters[0].IsRecord then
      Result := False;
end;

// Is this object overloaded?
function TPackageObject.GetIsOverloaded: Boolean;
begin
  Result := (OverloadCount > 0);
end;

// Add an overload
procedure TPackageObject.AddOverload(AObject: TPackageObject);
begin
  FOverloads.Add(AObject);
end;

// The number of overloads for this object
function TPackageObject.GetOverloadCount: Integer;
begin
  Result := FOverloads.Count;
  if Result = 1 then Result := 0;
end;

// Give overload at given index for this object
function TPackageObject.GetOverload(Index: Integer): TPackageObject;
begin
  Result := TPackageObject(FOverloads.Items[Index]);
end;

// The overload index of this object
function TPackageObject.GetOverloadIndex: Integer;
begin
  Result := FOverloads.IndexOf(Self);
end;

// Will we use variants for parameters?
function TPackageObject.GetUseVariants: Boolean;
begin
  Result := PackageDefinition.UseVariants;
end;

// The name of the object in the Delphi source
function TPackageObject.GetDelphiName: string;
begin
  if FDelphiName <> '' then Result := FDelphiName else Result := Name;
end;

// Return the type (function or procedure) of the object
function TPackageObject.GetObjectType: string;
begin
  if IsFunction then Result := 'Function' else Result := 'Procedure';
end;

// Parameters, not including sub-parameters and the result parameter
function TPackageObject.GetParameterCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to FParameters.Count - 1 do
    if (FParameters.Items[i] as TPackageParameter).Name <> '' then Inc(Result);
end;

function TPackageObject.GetParameter(Index: Integer): TPackageParameter;
var i: Integer;
begin
  i := 0;
  repeat
    Result := FParameters.Items[i] as TPackageParameter;
    if Result.Name <> '' then Dec(Index);
    Inc(i);
  until Index < 0;
end;

// Flat parameters are all parameters and the result parameters
function TPackageObject.GetAllParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

function TPackageObject.GetAllParameter(Index: Integer): TPackageParameter;
begin
  Result := FParameters.Items[Index] as TPackageParameter;
end;

// Flat parameters are all parameters and sub parameters, without any hierarchy
function TPackageObject.GetFlatParameterCount: Integer;
begin
  Result := FFlatParameters.Count;
end;

function TPackageObject.GetFlatParameter(Index: Integer): TPackageParameter;
begin
  Result := TPackageParameter(FFlatParameters.Items[Index]);
end;

// The result parameter, if the object is a function
function TPackageObject.GetResultParameter: TPackageParameter;
var i: Integer;
begin
  for i := 0 to FParameters.Count - 1 do
  begin
    Result := FParameters.Items[i] as TPackageParameter;
    if Result.Name = '' then Exit;
  end;
  Result := nil;
end;

// The full PL/SQL name for the object
function TPackageObject.GetPLSQLName: string;
begin
  Result := PackageDefinition.PlSqlName + '."' + Name + '"';
end;

// The PL/SQL Block that should be executed for this object
function TPackageObject.GetPLSQLBlock: string;
var p1, p2, p3, p4: string;
    i: Integer;
    S: TStringList;
begin
  p1 := '';
  p2 := '';
  p4 := '';
  for i := 0 to AllParameterCount - 1 do
  begin
    p1 := Trim(p1 + #13#10 + AllParameters[i].PLSQLDeclaration);
    p2 := Trim(p2 + #13#10 + AllParameters[i].PLSQLPreCall);
    p4 := Trim(p4 + #13#10 + AllParameters[i].PLSQLPostCall);
  end;
  p3 := '';
  if IsFunction then
  begin
    if not (ResultParameter.IsBoolean or
            ResultParameter.IsRecord) then p3 := ':';
    p3 := p3 + Format('%s := ', [ResultParameter.PLSQLVarName]);
  end;
  p3 := p3 + PLSQLName;
  if ParameterCount > 0 then p3 := p3 + '(';
//  ParIndent := Length(p3);
  for i := 0 to ParameterCount - 1 do
  begin
    if i > 0 then p3 := p3 + ',';
    if ParameterCount > 1 then p3 := p3 + #13#10 + '  ';
//    if i > 0 then p3 := p3 + ','#13#10 + StringOfChar(' ', ParIndent);
    p3 := p3 + Parameters[i].PLSQLInCall;
  end;
  if ParameterCount > 0 then p3 := p3 + ')';
  p3 := p3 + ';';
  S := TStringList.Create;
  if p1 <> '' then
  begin
    S.Add('declare');
    S.Add(StrIndent(p1, 2));
  end;
  S.Add('begin');
  if p2 <> '' then S.Add(StrIndent(p2, 2));
  S.Add(StrIndent(p3, 2));
  if p4 <> '' then S.Add(StrIndent(p4, 2));
  S.Add('end;');
  Result := '';
  S.Text := S.Text;
  for i := 0 to S.Count - 1 do
  begin
    if i > 0 then Result := Result + #13#10;
    Result := Result + 'OCPQuery.SQL.Add(''' + S[i] + ''');';
  end;
  S.Free;
end;

// The interface source for the object
function TPackageObject.GetInterfaceSource: string;
var i, l: Integer;
    s, ps: string;
begin
  s := StrPad(LowerCase(ObjectType), 9) + ' ' + DelphiName;
  if ParameterCount > 0 then
  begin
    s := s + '(';
    l := Length(s);
    for i := 0 to ParameterCount - 1 do
    begin
      if i > 0 then s := s + '; ';
      ps := Parameters[i].DelphiExpression;
      Inc(l, Length(ps) + 2);
      if l > 72 then
      begin
        s := s + #13#10 + '  ';
        l := Length(ps) + 2;
      end;
      s := s + ps;
    end;
    s := s + ')';
  end;
  if IsFunction then s := s + ': ' + ResultParameter.DelphiType;
  if PackageDefinition.UseOverload and IsOverloaded then
    s := s + '; overload';
  Result := s + ';';
end;

// The implementation source for the object
function TPackageObject.GetImplementationSource: string;
var S: TStringList;
    Header: string;
    i: Integer;
    cs, ps: string;
    Constructed: Boolean;
    Destruction: string;
    Indent: Integer;
begin
  S := TStringList.Create;
  cs := '// ' + PackageDefinition.Name + '.' + Name;
  if IsOverloaded then cs := cs + ', overload ' + OverloadId;
  S.Add(cs);
  Header := InterfaceSource;
  i := Pos('function', Header);
  if i = 1 then Delete(Header, 9, 1);
  i := Pos(' ', Header);
  if i > 0 then Insert(PackageDefinition.DelphiName + '.', Header, i + 1);
  i := Pos('; overload;', Header);
  if i > 0 then Delete(Header, i, 10);
  S.Add(Header);
  S.Add('begin');
  Constructed := False;
  for i := 0 to AllParameterCount - 1 do
  begin
    if AllParameters[i].Mode = pmOut then
    begin
      cs := AllParameters[i].GetDelphiConstructor('Session', True);
      if cs <> '' then
      begin
        Constructed := True;
        S.Add(StrIndent(cs, 2));
        if Destruction <> '' then Destruction := Destruction + #13#10;
        Destruction := Destruction + AllParameters[i].DelphiNameQualified + '.Free;';
      end;
    end;
  end;
  (*
  if IsFunction and (ResultParameter.IsComplex or
                     ResultParameter.IsPLSQLTable or
                     ResultParameter.IsRecord) then
  begin
    cs := ResultParameter.GetDelphiConstructor('Session', True);
    Constructed := (cs <> '');
    if Constructed then S.Add(StrIndent(cs, 2));
  end;
  *)
  if not (Constructed or PackageDefinition.ThreadSafe) then
    Indent := 2
  else begin
    Indent := 4;
    if PackageDefinition.ThreadSafe then S.Add('  ThreadAcquire;');
    S.Add('  try');
  end;
  S.Add(StrIndent('GetQuery;', Indent));
  for i := 0 to AllParameterCount - 1 do
  begin
    ps := AllParameters[i].DelphiPreCall;
    if ps <> '' then S.Add(StrIndent(ps, Indent));
  end;
  S.Add(StrIndent(PLSQLBlock, Indent));
  S.Add(StrIndent('OCPQuery.Execute;', Indent));
  for i := 0 to AllParameterCount - 1 do
  begin
    ps := AllParameters[i].DelphiPostCall;
    if ps <> '' then S.Add(StrIndent(ps, Indent));
  end;
  if Constructed then
  begin
    S.Add('  except');
    if PackageDefinition.ThreadSafe then S.Add('    ThreadRelease;');
//    S.Add('    ' + ResultParameter.DelphiNameQualified + '.Free;');
    S.Add(StrIndent(Destruction, 4));
    S.Add('    raise;');
    S.Add('  end;');
    if PackageDefinition.ThreadSafe then S.Add('  ThreadRelease;');
  end;
  if PackageDefinition.ThreadSafe and not Constructed then
  begin
    S.Add('  finally');
    if PackageDefinition.ThreadSafe then S.Add('    ThreadRelease;');
    S.Add('  end;');
  end;
  S.Add('end;');
  Result := Trim(S.Text);
  S.Free;
end;

// TPackageParameter

// Make a level 0 parameter
constructor TPackageParameter.Make(APackageObject: TPackageObject; const AName: string);
begin
  inherited Create(APackageObject.FParameters);
  FPackageObject := APackageObject;
  FParameters := TCollection.Create(TPackageParameter);
  FName := AName;
  PackageObject.FFlatParameters.Add(self);
end;

// Make a level 1 or greater parameter (subparameter)
constructor TPackageParameter.MakeSub(AParent: TPackageParameter; const AName: string);
begin
  inherited Create(AParent.FParameters);
  FParent := AParent;
  FParameters := TCollection.Create(TPackageParameter);
  FName := AName;
  PackageObject.FFlatParameters.Add(self);
end;

destructor TPackageParameter.Destroy;
begin
  FParameters.Free;
  inherited;
end;

// The number of sub parameters
function TPackageParameter.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

// Return a sub parameter
function TPackageParameter.GetParameter(Index: Integer): TPackageParameter;
begin
  Result := FParameters.Items[Index] as TPackageParameter;
end;

// Return the package definition
function TPackageParameter.GetPackageDefinition: TPackageDefinition;
begin
  Result := PackageObject.PackageDefinition
end;

// Return the package object that this (sub)parameter belongs to
function TPackageParameter.GetPackageObject: TPackageObject;
begin
  if FPackageObject <> nil then
    Result := FPackageObject
  else
    Result := Parent.PackageObject;
end;

// Are we using variants for parameter types?
function TPackageParameter.GetUseVariants: Boolean;
begin
  if PackageObject <> nil then
    Result := PackageObject.UseVariants
  else
    Result := Parent.UseVariants;
end;

// Is this parameter a boolean?
function TPackageParameter.GetIsBoolean: Boolean;
begin
  Result := (DataType = 'PL/SQL BOOLEAN');
end;

// Is this parameter a record type?
function TPackageParameter.GetIsRecord: Boolean;
begin
  Result := (DataType = 'PL/SQL RECORD');
end;

// Return the Delphi variable name for the parameter
function TPackageParameter.GetDelphiName: string;
begin
  if FDelphiName <> '' then Result := FDelphiName else Result := Name;
  if Parent <> nil then Result := MainForm.ConvertName(Result, False, False);
end;

// Return the fully qualified Delphi variable name for the parameter
// E.g. myrecord.mysubrecord.myfield
function TPackageParameter.GetDelphiNameQualified: string;
begin
  Result := DelphiName;
  if (Name = '') and (Parent = nil) then Result := 'Result';
  if Parent <> nil then Result := Parent.DelphiNameQualified + '.' + Result;
end;

// Return the name of the Delphi equivalent type of the parameter
function TPackageParameter.GetDelphiType: string;
var s: string;
    i: Integer;
begin
  if UseVariants then
    s := 'Variant'
  else begin
    s := 'string';
    if (DataType = 'DATE') then s := 'TDateTime';
    if (DataType = 'NUMBER') or (DataType = 'FLOAT') then
    begin
      if (DataPrecision <= 0) or (DataPrecision >= 9) or (DataScale <> 0) then
        s := 'Double'
      else
        s := 'Integer';
    end;
    if (DataType = 'BINARY_INTEGER') then s := 'Integer';
    if (DataType = 'PL/SQL BOOLEAN') then s := 'Boolean';
  end;
  if (DataType = 'PL/SQL RECORD') then
  begin
    s := StrNoDoubleQuotes(PLSQLRecordType);
    for i := 1 to Length(s) do
      if s[i] in ['$', '#', ' ', '.', '%'] then s[i] := '_';
    if s <> '' then
      s := MainForm.ConvertName(s, False, False)
    else begin
      if FDelphiType <> '' then
        s := FDelphiType
      else begin
        Inc(UnitTypeSequence);
        s := 'TRowType' + IntToStr(UnitTypeSequence);
        FDelphiType := s;
      end;
    end;
  end;
  if (DataType = 'PL/SQL TABLE') then s := 'TPLSQLTable';
  if (DataType = 'REF CURSOR') then s := 'TOracleQuery';
  if (DataType = 'CLOB') or
     (DataType = 'BLOB') or
     (DataType = 'BFILE') or
     (DataType = 'NCLOB') then s := 'TLOBLocator';
  if (DataType = 'REF') then s := 'TOracleReference';
  if (DataType = 'OBJECT') or
     (DataType = 'TABLE') or
     (DataType = 'VARRAY') then s := 'TOracleObject';
  Result := s;
end;

// Get the Delphi expression for the parameter
// E.g. var p_empno Integer
function TPackageParameter.GetDelphiExpression: string;
begin
  Result := '';
  case Mode of
    pmIn: if (DelphiType = 'string') or (DelphiType = 'Variant') then Result := 'const ';
    pmOut: Result := 'out ';
    pmInOut: Result := 'var ';
  end;
  Result := Result + DelphiName + ': ' + DelphiType;
//  This is obviously too simple. We first need a all_arguments view patch to test this.
//  if PackageDefinition.UseDefaults and (DefaultValue <> '') then
//    Result := Result + ' = ' + DefaultValue;
end;

// Get the name of the local (!) PL/SQL variable of the parameter
function TPackageParameter.GetPLSQLVarName: string;
begin
  if Name = '' then
    Result := 'function_result'
  else
    Result := Name;
  if Parent <> nil then Result := Parent.PLSQLVarName + '.' + Result;
end;

// Get the name of the bind (!) variable of the parameter
function TPackageParameter.GetBindVarName: string;
begin
  if Parent = nil then
    Result := PLSQLVarName
  else
    Result := 'record_var' + IntToStr(Sequence);
end;

// Get the variable type for TOracleQuery.DeclareVariable
function TPackageParameter.GetDelphiVarType: string;
var s: string;
begin
  Result := 'otString';
  s := DataType;
  if (s = 'NUMBER') or (s = 'FLOAT') then
  begin
    if (DataPrecision <= 0) or (DataPrecision >= 9) or (DataScale <> 0) then
      Result := 'otFloat'
    else
      Result := 'otInteger';
  end;
  if s = 'BINARY_INTEGER' then Result := 'otInteger';
  if s = 'DATE'           then Result := 'otDate';
  if s = 'PL/SQL BOOLEAN' then Result := 'otInteger';
  if s = 'CLOB'           then Result := 'otCLOB';
  if s = 'BLOB'           then Result := 'otBLOB';
  if s = 'BFILE'          then Result := 'otBFILE';
  if s = 'NCLOB'          then Result := 'otCLOB';
  if s = 'LONG'           then Result := 'otLong';
  if s = 'LONG RAW'       then Result := 'otLongRaw';
  if s = 'REF'            then Result := 'otReference';
  if s = 'OBJECT'         then Result := 'otObject';
  if s = 'TABLE'          then Result := 'otObject';
  if s = 'VARRAY'         then Result := 'otObject';
  if s = 'REF CURSOR'     then Result := 'otCursor';
  if s = 'PL/SQL TABLE'   then Result := Parameters[0].DelphiVarType;
end;

// Is this a complex parameter?
function TPackageParameter.GetIsComplex: Boolean;
begin
  Result := (DelphiType = 'TOracleQuery') or
            (DelphiType = 'TLOBLocator') or
            (DelphiType = 'TOracleObject') or
            (DelphiType = 'TOracleReference');
end;

// Is this an Oracle7 LONG or LONG RAW parameter?
function TPackageParameter.GetIsLong: Boolean;
begin
  Result := (DelphiVarType = 'otLong') or (DelphiVarType = 'otLongRaw');
end;

// Does this record parameter have complex parameters?
function TPackageParameter.GetHasComplexParameters: Boolean;
var i: Integer;
begin
  Result := False;
  if IsRecord then
    for i := 0 to ParameterCount - 1 do
      Result := Result or Parameters[i].IsComplex or Parameters[i].HasComplexParameters;
end;

// Does this record parameter have sub types (records or PL/SQL Tables)?
function TPackageParameter.GetHasSubTypes: Boolean;
var i: Integer;
begin
  Result := False;
  if IsRecord then
    for i := 0 to ParameterCount - 1 do
      Result := Result or Parameters[i].IsRecord or Parameters[i].IsPLSQLTable;
end;

// Is this a PL/SQL Table parameter?
function TPackageParameter.GetIsPLSQLTable: Boolean;
begin
  Result := (DelphiType = 'TPLSQLTable');
end;

// Return the full SQL type name of the parameter
function TPackageParameter.GetFullTypeName: string;
begin
  Result := '';
  if TypeName <> '' then
  begin
    if PackageDefinition.UseSchemaName then Result := '"' + TypeOwner + '".';
    Result := Result + '"' + TypeName + '"';
    if TypeSubName <> '' then Result := Result + '."' + TypeSubName + '"';
  end;
end;

// Return the constructor for a complex, Record, or PL/SQL Table parameter
function TPackageParameter.GetDelphiConstructor(const ASessionName: string; Qualified: Boolean): string;
var DT, Params: string;
    VarName: string;
begin
  Result := '';
  if IsComplex or IsPLSQLTable or IsRecord then
  begin
    if Qualified then VarName := DelphiNameQualified else VarName := DelphiName;
    DT := DelphiType;
    Params := '';
    if DT = 'TOracleQuery' then
    begin
      Params := 'nil';
      Result := Format(#13#10'%s.Session := %s;', [VarName, ASessionName]);
    end;
    if DT = 'TLOBLocator'      then Params := ASessionName + ', ' + DelphiVarType;
    if DT = 'TOracleObject'    then Params := ASessionName + ', ' + StrQuoted(FullTypeName) + ', ''''';
    if DT = 'TOracleReference' then Params := ASessionName + ', ' + StrQuoted(FullTypeName);
    if IsPLSQLTable then
    begin
      Params := 'DefaultPLSQLTableSize, ';
      if Parameters[0].DelphiType = 'string' then
        Params := Params + IntToStr(Parameters[0].DataLength)
      else
        Params := Params + '0';
    end;
    if IsRecord then Params := ASessionName;
    if Params <> '' then Params := '(' + Params + ')';
    Result := Format('%s := %s.Create%s;', [VarName, DT, Params]) + Result;
  end;
end;

// Return the destructor for a complex, Record, or PL/SQL Table parameter
function TPackageParameter.GetDelphiDestructor(Qualified: Boolean): string;
begin
  Result := '';
  if IsComplex or IsPLSQLTable or IsRecord then
  begin
    if Qualified then Result := DelphiNameQualified else Result := DelphiName;
    Result := Result + '.Free;';
  end;
end;

// Return the actions that should be performed before executing the call
function TPackageParameter.GetDelphiPreCall: string;
var s, sc, sb: string;
    i: Integer;
begin
  if IsRecord then
  begin
    Result := '';
    for i := 0 to ParameterCount - 1 do
    begin
      s := Parameters[i].DelphiPreCall;
      if s <> '' then Result := Result + s + #13#10;
    end;
    Result := Trim(Result);
  end else begin
    Result := Format('OCPQuery.DeclareVariable(''%s'', %s);', [BindVarName, DelphiVarType]);
    if IsPLSQLTable then
      Result := Result + #13#10 + Format('OCPQuery.DimPLSQLTable(''%s'', %s.TableSize, %s.StringSize);',
       [BindVarName, DelphiNameQualified, DelphiNameQualified]);
    if IsComplex or IsLong or (Mode in [pmIn, pmInOut]) then
    begin
      if IsComplex then sc := 'Complex' else sc := '';
      if IsLong and (Mode = pmOut) then
        sb := 'StringOfChar(#0, 32512)'
      else
        sb := DelphiNameQualified;
      if IsPLSQLTable then sb := sb + '.ValueArray';
      if IsBoolean then sb := 'BoolToInt(' + sb + ')';
      Result := Result + #13#10 + Format('OCPQuery.Set%sVariable(''%s'', %s);', [sc, BindVarName, sb]);
    end;
  end;
end;

// Return the actions that should be performed after executing the call
function TPackageParameter.GetDelphiPostCall: string;
var s, vs: string;
    i: Integer;
begin
  Result := '';
  if IsRecord then
  begin
    for i := 0 to ParameterCount - 1 do
    begin
      s := Parameters[i].DelphiPostCall;
      if s <> '' then Result := Result + s + #13#10;
    end;
    Result := Trim(Result);
  end else begin
    if (not IsComplex) and (Mode in [pmInOut, pmOut]) then
    begin
      Result := Format('OCPQuery.GetVariable(''%s'')', [BindVarName]);
      if IsBoolean then Result := Format('IntToBool(%s)', [Result]);
      if (not UseVariants) and (not IsPLSQLTable) then Result := Format('ConvertVariant(%s)', [Result]);
      if not IsPLSQLTable then vs := '' else vs := '.ValueArray';
      Result := Format('%s%s := %s;', [DelphiNameQualified, vs, Result]);
    end;
  end;
end;

// Return the Delphi type interface for a record type parameter
function TPackageParameter.GetDelphiTypeInterface: string;
var SL: TStringList;
    i: Integer;
begin
  Result := '';
  if IsRecord then
  begin
    SL := TStringList.Create;
    SL.Add(DelphiType + ' = class(TPLSQLRecord)');
    SL.Add('public');
    for i := 0 to ParameterCount - 1 do
    begin
      SL.Add(Format('  %s: %s;', [Parameters[i].DelphiName, Parameters[i].DelphiType]));
    end;
    if HasSubTypes or HasComplexParameters then
    begin
      SL.Add('  constructor Create(ASession: TOracleSession); override;');
      SL.Add('  destructor  Destroy; override;');
    end;
    SL.Add(Format('  procedure Assign(Source: TPLSQLRecord); override;', [DelphiType]));
    SL.Add('end;');
    Result := StrIndent(Trim(SL.Text), 2);
    SL.Free;
  end;
end;

// Return the Delphi type interface for a record type parameter
function TPackageParameter.GetDelphiTypeImplementation: string;
var SL: TStringList;
    s: string;
    i: Integer;
    Par: TPackageParameter;
begin
  Result := '';
  if IsRecord then
  begin
    SL := TStringList.Create;
    if HasSubTypes or HasComplexParameters then
    begin
      if Parent = nil then
        s := PLSQLRecordType
      else begin
        if TypeName <> ''    then s := TypeName;
        if TypeSubName <> '' then s := TypeName + '.' + s;
      end;
      if s <> '' then s := ' (' + s + ')';
      SL.Add(Format('// %s object%s', [DelphiType, s]));
      SL.Add(Format('constructor %s.Create(ASession: TOracleSession);', [DelphiType]));
      SL.Add('begin');
      SL.Add('  inherited;');
      for i := 0 to ParameterCount - 1 do
      begin
        Par := Parameters[i];
        s := Par.GetDelphiConstructor('ASession', False);
        if s <> '' then SL.Add(StrIndent(s, 2));
      end;
      SL.Add('end;');
      SL.Add('');
      SL.Add(Format('destructor %s.Destroy;', [DelphiType]));
      SL.Add('begin');
      for i := 0 to ParameterCount - 1 do
      begin
        Par := Parameters[i];
        s := Par.GetDelphiDestructor(False);
        if s <> '' then SL.Add(StrIndent(s, 2));
      end;
      SL.Add('  inherited;');
      SL.Add('end;');
    end;
    SL.Add('');
    SL.Add(Format('procedure %s.Assign(Source: TPLSQLRecord);', [DelphiType]));
    SL.Add('begin');
    SL.Add('  inherited;');
    SL.Add(Format('  with Source as %s do', [DelphiType]));
    SL.Add('  begin');
    for i := 0 to ParameterCount - 1 do
    begin
      Par := Parameters[i];
      if Par.IsRecord or Par.IsPLSQLTable or Par.IsComplex then
        s := 'Self.%s.Assign(%s);'
      else
        s := 'Self.%s := %s;';
      SL.Add('    ' + Format(s, [Par.DelphiName, Par.DelphiName]));
    end;
    SL.Add('  end;');
    SL.Add('end;');
    Result := Trim(SL.Text);
    SL.Free;
  end;
end;

// Return the local PL/SQL variable declaration for this parameter, if any
function TPackageParameter.GetPLSQLDeclaration: string;
begin
  Result := '';
  if IsBoolean then Result := PLSQLVarName + ' boolean;';
  if IsRecord then Result := PLSQLVarName + ' ' + PLSQLRecordType + ';';
end;

// Return the actions that should be performed before executing the call
function TPackageParameter.GetPLSQLPreCall: string;
var i: Integer;
    s: string;
begin
  Result := '';
  if Mode in [pmIn, pmInOut] then
  begin
    if IsBoolean then
      Result := Format('%s := sys.diutil.int_to_bool(:%s);', [PLSQLVarName, BindVarName]);
    if (Parent <> nil) and (not IsRecord) and (not IsBoolean) then
      Result := Format('%s := :%s;', [PLSQLVarName, BindVarName]);
    if IsRecord then
    begin
      for i := 0 to ParameterCount - 1 do
      begin
        s := Trim(Parameters[i].PLSQLPreCall);
        if s <> '' then
        begin
          if Result <> '' then Result := Result + #13#10;
          Result := Result + s;
        end;
      end;
    end;
  end;
end;

// Return parameter expression
function TPackageParameter.GetPLSQLInCall: string;
begin
  Result := Name + ' => ';
  if not (IsBoolean or IsRecord) then
    Result := Result + ':' + BindVarName
  else
    Result := Result + PLSQLVarName;
end;

// Return the actions that should be performed after executing the call
function TPackageParameter.GetPLSQLPostCall: string;
var i: Integer;
    s: string;
begin
  Result := '';
  if Mode in [pmOut, pmInOut] then
  begin
    if IsBoolean then
      Result := Format(':%s := sys.diutil.bool_to_int(%s);', [BindVarName, PLSQLVarName]);
    if (Parent <> nil) and (not IsRecord) and (not IsBoolean) then
      Result := Format(':%s := %s;', [BindVarName, PLSQLVarName]);
    if IsRecord then
    begin
      for i := 0 to ParameterCount - 1 do
      begin
        s := Trim(Parameters[i].PLSQLPostCall);
        if s <> '' then
        begin
          if Result <> '' then Result := Result + #13#10;
          Result := Result + s;
        end;
      end;
    end;
  end;
end;

// Return the PL/SQL record type, declared locally in the PL/SQL Block
function TPackageParameter.GetPLSQLRecordType: string;
var WordPos, MarkPos, OverloadCount: Integer;
    Word, ProcName: string;
    ArgName: string;
begin
  Result := '';
  if not IsRecord then Exit;
  if FPLSQLRecordType = '' then
  begin
    if Parent <> nil then
    begin
      Result := TypeName;
      if TypeSubName <> '' then Result := TypeName + '_' + TypeSubName;
    end else begin
      WordPos := 0;
      OverloadCount := PackageObject.OverloadIndex;
      if Self = PackageObject.ResultParameter then
        ArgName := 'RETURN'
      else
        ArgName := AnsiUpperCase(Name);
      repeat
        Word := AnsiUpperCase(PackageDefinition.NextSpecWord(WordPos));
        if (Word = 'FUNCTION') or (Word = 'PROCEDURE') then
        begin
          MarkPos := WordPos;
          ProcName := StrNoDoubleQuotes(UpperCase(PackageDefinition.NextSpecWord(WordPos)));
          if ProcName = AnsiUpperCase(PackageObject.Name) then
          begin
            Dec(OverloadCount);
            if OverloadCount < 0 then
            begin
              repeat
                Word := StrNoDoubleQuotes(UpperCase(PackageDefinition.NextSpecWord(WordPos)));
                if Word = ArgName then
                begin
                  repeat
                    Word := PackageDefinition.NextSpecWord(WordPos);
                    if (AnsiUpperCase(Word) <> 'IN') and (AnsiUpperCase(Word) <> 'OUT') then
                      Result := Word;
                  until (Word = '') or (Result <> '');
                end;
              until (Word = '') or (Word = 'FUNCTION') or (Word = 'PROCEDURE');
            end;
          end;
          WordPos := MarkPos;
        end;
      until (Word = '') or (Result <> '');
    end;
    if Result <> '' then
    begin
      // If the parameter is a type declared in this package, add the package prefix
      if (Pos('%', Result) <= 0) and (Pos('.', Result) <= 0) then
      begin
        Result := PackageDefinition.PLSQLName + '.' + Result;
      end;
    end;
    FPLSQLRecordType := Result;
  end;
  Result := FPLSQLRecordType;
end;

// Default Wizard and menu handling & registration

{$IFNDEF CompilerVersion2005}
type
  TOracleProjectExpert = class (TIExpert)
  private
    MenuIndex: Integer;
  public
    constructor Create(Index: Integer);
    function  GetStyle: TExpertStyle; override;
    function  GetName: string; override;
    function  GetAuthor: string; override;
    function  GetComment: string; override;
    function  GetPage: string; override;
    {$IFNDEF LINUX}
    function  GetGlyph: HICON; override;
    {$ENDIF}
    function  GetState: TExpertState; override;
    function  GetIDString: string; override;
    function  GetMenuText: string; override;
    procedure Execute; override;
  end;

type
  TOracleAddInExpert = class(TIExpert)
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function  GetStyle: TExpertStyle; override;
    function  GetIDString: String; override;
    function  GetName: String; override;
    function  GetAuthor: String; override;
    function  GetMenuText: string; override;
    function  GetState: TExpertState; override;
    {$IFNDEF LINUX}
    function  GetGlyph: HICON; override;
    {$ENDIF}
    function  GetComment: string; override;
    function  GetPage: string; override;
    procedure Execute; override;
  protected
    procedure OnClick(Sender: TIMenuItemIntf); virtual;
    procedure OnDummyClick(Sender: TIMenuItemIntf); virtual;
  private
    MenuItem: TIMenuItemIntf;
  end;

(*
function RemoveQuotes(S: string): string;
begin
  Result := S;
  if Length(S) > 2 then
  begin
    if (S[1] = '"') and (S[Length(S)] = '"') then Result := Copy(S, 2, Length(S) - 2);
  end;
end;

procedure CheckOracleMonitor;
var Exe: string;
begin
  Exe := ReadRegString(HKEY_CURRENT_USER, 'Software\Allround Automations\OracleMonitor', 'Executable');
  Exe := RemoveQuotes(Exe);
  if (Exe = '') or (not FileExists(Exe)) then OracleToolItems[3].MenuItem.SetFlags([mfEnabled], [mfInvalid]);
end;

procedure CheckPLSQLDeveloper;
var Exe: string;
begin
  Exe := ReadRegString(HKEY_CLASSES_ROOT, 'PL/SQL Developer\Shell\Open\Command', '');
  Exe := RemoveQuotes(Exe);
  if (Exe = '') or (not FileExists(Exe)) then OracleToolItems[8].MenuItem.SetFlags([mfEnabled], [mfInvalid]);
end;
*)

function TOracleAddInExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn;
end;

function TOracleAddInExpert.GetName: String;
begin
  Result := 'OracleAddIn';
end;

function TOracleAddInExpert.GetAuthor: string;
begin
  Result := 'Allround Automations';
end;

function TOracleAddInExpert.GetComment: String;
begin
  Result := 'Oracle AddIn';
end;

function TOracleAddInExpert.GetPage: string;
begin
  Result := '';
end;

{$IFNDEF LINUX}
function TOracleAddInExpert.GetGlyph: HICON;
begin
  Result := 0;
end;
{$ENDIF}

function TOracleAddInExpert.GetState: TExpertState;
begin
  Result := [esEnabled];
end;

function TOracleAddInExpert.GetIDString: String;
begin
  Result := 'DOA.OracleAddIn';
end;

function TOracleAddInExpert.GetMenuText: String;
begin
  Result := 'Oracle AddIn';
end;

procedure TOracleAddInExpert.Execute;
begin
end;

constructor TOracleAddInExpert.Create;
var i: Integer;
    Main: TIMainMenuIntf;
    MainMenu: TIMenuItemIntf;
begin
  inherited Create;
  for i := 1 to OracleToolCount do OracleToolItems[i].MenuItem := nil;
  MenuItem := nil;
  MainMenu := nil;
  if ToolServices <> nil then
  begin
    Main := ToolServices.GetMainMenu;
    if Main <> nil then MainMenu := Main.GetMenuItems;
    if MainMenu <> nil then
    try
      MenuItem := MainMenu.InsertItem(MainMenu.GetItemCount - 2,
                                    'Oracle', 'DOA0','', 0,0,0,
                                     [mfEnabled, mfVisible], OnDummyClick);
      for i := 1 to OracleToolCount do
      begin
        OracleToolItems[i].MenuItem := MenuItem.InsertItem(i - 1,
                    OracleToolItems[i].Name, 'DOA' + IntToStr(i),'',
                    0,0,0, [mfEnabled, mfVisible], OnClick);
      end;
    finally
      if MainMenu <> nil then MainMenu.DestroyMenuItem;
      Main.Free;
    end;
  end;
end;

destructor TOracleAddInExpert.Destroy;
var i: Integer;
begin
  for i := 1 to OracleToolCount do
    if OracleToolItems[i].MenuItem <> nil then OracleToolItems[i].MenuItem.DestroyMenuItem;
  if MenuItem <> nil then MenuItem.DestroyMenuItem;
  inherited Destroy;
end;

procedure TOracleAddInExpert.OnClick(Sender: TIMenuItemIntf);
begin
  OracleExecutor.Execute(Sender.GetIndex + 1);
end;

procedure TOracleAddInExpert.OnDummyClick(Sender: TIMenuItemIntf);
begin
end;

// New... Wizard

constructor TOracleProjectExpert.Create(Index: Integer);
begin
  MenuIndex := Index;
  inherited Create;
end;

function TOracleProjectExpert.GetStyle: TExpertStyle;
begin
  Result := esProject;
end;

function TOracleProjectExpert.GetName: String;
begin
  Result := OracleToolItems[MenuIndex].WizardName;
end;

function TOracleProjectExpert.GetAuthor: string;
begin
  Result := 'Allround Automations';
end;

function TOracleProjectExpert.GetComment: String;
begin
  Result := OracleToolItems[MenuIndex].Comment;
end;

function TOracleProjectExpert.GetPage: string;
begin
  Result := OracleToolItems[MenuIndex].Page;
end;

{$IFNDEF LINUX}
function TOracleProjectExpert.GetGlyph: HICON;
var i: Integer;
    S: string;
begin
  S := '';
  // Delete spaces to make a valid resource name
  for i := 1 to Length(OracleToolItems[MenuIndex].WizardName) do
    if OracleToolItems[MenuIndex].WizardName[i] > ' ' then
      S := S + UpperCase(OracleToolItems[MenuIndex].WizardName[i]);
  Result := LoadIcon(HInstance, MakeIntResource(S));
end;
{$ENDIF}

function TOracleProjectExpert.GetState: TExpertState;
begin
  Result := [esEnabled];
end;

function TOracleProjectExpert.GetIDString: String;
begin
  Result := 'DOA.Expert' + IntToStr(MenuIndex);
end;

function TOracleProjectExpert.GetMenuText: String;
begin
  Result := ''
end;

procedure TOracleProjectExpert.Execute;
begin
  OracleExecutor.Execute(MenuIndex);
end;
{$ENDIF}

{$IFDEF CompilerVersion2005}
type
  TOracleProjectExpert = class (TNotifierObject, IOTAWIzard, IOTARepositoryWizard,
    IOTAFormWizard)
  private
    MenuIndex: Integer;
  public
    constructor Create(Index: Integer);
    // IOTAWIzard
    function  GetIDString: String;
    function  GetName: String;
    function  GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function  GetAuthor: string;
    function  GetComment: string;
    function  GetPage: string;
    {$IFNDEF LINUX}
    function  GetGlyph: Cardinal;
    {$ENDIF}
  end;

type
  TOracleAddInExpert = class(TNotifierObject, IOTAWIzard)
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    // IOTAWIzard
    function  GetIDString: String;
    function  GetName: String;
    function  GetState: TWizardState;
    procedure Execute;
  private
    MenuItem: TMenuItem;
  end;

function TOracleAddInExpert.GetIDString: String;
begin
  Result := 'DOA.OracleAddIn';
end;

function TOracleAddInExpert.GetName: String;
begin
  Result := 'OracleAddIn';
end;

function TOracleAddInExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TOracleAddInExpert.Execute;
begin
end;

constructor TOracleAddInExpert.Create;
var i: Integer;
    NTAServ: INTAServices;
    MainMenu: TMainMenu;
begin
  inherited Create;
  for i := 1 to OracleToolCount do OracleToolItems[i].MenuItem := nil;

  NTAServ := BorlandIDEServices as INTAServices;
  MainMenu := NTAServ.MainMenu;

  MenuItem := NewItem('Oracle', 0, False, True, nil, -1, 'DOA0');
  MainMenu.Items.Insert(MainMenu.Items.Count - 2, MenuItem);
  for i := 1 to OracleToolCount do
  begin
    OracleToolItems[i].MenuItem := NewItem(OracleToolItems[i].Name,
      0, False, True, OracleExecutor.GetOnClick(i), -1, 'DOA' + IntToStr(i));
    MenuItem.Add(OracleToolItems[i].MenuItem);
  end;
end;

destructor TOracleAddInExpert.Destroy;
var i: Integer;
begin
  for i := 1 to OracleToolCount do
    if OracleToolItems[i].MenuItem <> nil then FreeAndNil(OracleToolItems[i].MenuItem);
  if MenuItem <> nil then FreeAndNil(MenuItem);
  inherited Destroy;
end;

// New... Wizard

function TOracleProjectExpert.GetIDString: String;
begin
  Result := 'DOA.Expert' + IntToStr(MenuIndex);
end;

function TOracleProjectExpert.GetName: String;
begin
  Result := OracleToolItems[MenuIndex].WizardName;
end;

function TOracleProjectExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TOracleProjectExpert.GetAuthor: string;
begin
  Result := 'Allround Automations';
end;

function TOracleProjectExpert.GetComment: String;
begin
  Result := OracleToolItems[MenuIndex].Comment;
end;

function TOracleProjectExpert.GetPage: string;
begin
  Result := OracleToolItems[MenuIndex].Page;
end;

{$IFNDEF LINUX}
function TOracleProjectExpert.GetGlyph: Cardinal;
var i: Integer;
    S: string;
begin
  S := '';
  // Delete spaces to make a valid resource name
  for i := 1 to Length(OracleToolItems[MenuIndex].WizardName) do
    if OracleToolItems[MenuIndex].WizardName[i] > ' ' then
      S := S + UpperCase(OracleToolItems[MenuIndex].WizardName[i]);
  Result := LoadIcon(HInstance, MakeIntResource(S));
end;
{$ENDIF}

constructor TOracleProjectExpert.Create(Index: Integer);
begin
  MenuIndex := Index;
  inherited Create;
end;

procedure TOracleProjectExpert.Execute;
begin
  OracleExecutor.Execute(MenuIndex);
end;
{$ENDIF}

procedure RegisterOracleTools;
{$IFNDEF LINUX}
var i: Integer;
{$ENDIF}
begin
  {$IFNDEF LINUX}
  OpenRegistry('Preferences');
  if ReadBool('OracleMenu', True) then
  begin
{$IFDEF CompilerVersion2005}
    RegisterPackageWizard(TOracleAddInExpert.Create as IOTAWizard);
{$ELSE}
    RegisterLibraryExpert(TOracleAddInExpert.Create);
{$ENDIF}
  end;
  if ReadBool('OracleWizards', True) then
  begin
    for i := 1 to OracleToolCount do
    begin
      if OracleToolItems[i].Page <> '' then
{$IFDEF CompilerVersion2005}
        RegisterPackageWizard(TOracleProjectExpert.Create(i) as IOTAWizard);
{$ELSE}
        RegisterLibraryExpert(TOracleProjectExpert.Create(i));
{$ENDIF}
    end;
  end;
  CloseRegistry;
//  CheckOracleMonitor;
//  CheckPLSQLDeveloper;
  {$ENDIF}
end;

// Package Wizard

procedure TWizardForm.FormCreate(Sender: TObject);
var PaletteList: TStringList;
    i: Integer;
 procedure AddInfo(S: string);
 begin
   if InfoLabel.Caption <> '' then S := #13#10 + S;
   InfoLabel.Caption := InfoLabel.Caption + S;
 end;
begin

  // Temporarily disabled until Oracle bug ###### is fixed
  UseDefaultsCheck.Enabled := False;
  UseDefaultsCheck.Checked := False;

  SelectedPackages := TStringList.Create;

  OpenDelphiRegistry('Palette');
  PaletteList := ReadSection;
  for i := 0 to PaletteList.Count - 1 do
  begin
    if (Length(PaletteList[i]) > 1) and (PaletteList[i][1] <> '.') then
    begin
      if Pos('.Hidden', PaletteList[i]) <= 0 then PaletteEdit.Items.Add(PaletteList[i]);
    end;
  end;
  PaletteList.Free;
  CloseRegistry;

  OpenRegistry('PackageWizard');
  DontShowAgain.Checked := ReadBool('DontShowInfo', False);
  AllPackagesCheck.Checked := ReadBool('AllPackages', False);
  UseVariantsCheck.Checked := ReadBool('UseVariants', False);
  {$IFDEF CompilerVersion4}
  GenerateOverloadCheck.Checked := ReadBool('OverloadedMethods', True);
  UseDefaultsCheck.Checked := ReadBool('UseDefaults', False);
  {$ENDIF}
  CaseGroup.ItemIndex := ReadInteger('Case', 1);
  TPrefixCheck.Checked := ReadBool('TPrefix', True);
  APrefixCheck.Checked := ReadBool('APrefix', False);
  RemoveUSCheck.Checked := ReadBool('RemoveUnderscore', True);
  PrefixSchemaCheck.Checked := ReadBool('PrefixSchema', False);
  ThreadSafeCheck.Checked := ReadBool('ThreadSafe', False);
  PackageCommentCheck.Checked := ReadBool('PackageComment', True);
  CreateComponentCheck.Checked := ReadBool('CreateComponent', False);
  CreateResourceCheck.Checked := ReadBool('CreateResource', True);
  AddToProjectCheck.Checked := ReadBool('AddToProject', True);
  OpenInIDECheck.Checked := ReadBool('OpenInIDE', True);
  OpenRegistry('PackageWizard\' + CompilerVersion);
  PaletteEdit.Text := ReadString('Palette', '');
  PathEdit.Text := ReadString('LastPath', '');
  FileEdit.Text := ReadString('LastFile', '');
  CloseRegistry;
  {$IFNDEF CompilerVersion4}
  GenerateOverloadCheck.Enabled := False;
  GenerateOverloadCheck.Checked := False;
  UseDefaultsCheck.Enabled := False;
  UseDefaultsCheck.Checked := False;
  {$ENDIF}
  if ToolServices <> nil then
  begin
    PathEdit.Text := ExtractFilePath(ToolServices.GetProjectname);
    FileEdit.Text := '';
  end;
  SourceStyleChange(nil);
  NameStyleChange(nil);

  i := 0;
  if DontShowAgain.Checked then inc(i);
  PackageWizardControl.ActivePage := PackageWizardControl.Pages[i];

 { AddInfo('Welcome...');
  AddInfo('');
  AddInfo('This is a wizard');}
end;

procedure TWizardForm.FormActivate(Sender: TObject);
begin
  InitForm(Self);
end;

procedure TWizardForm.FormDestroy(Sender: TObject);
begin
  OpenRegistry('PackageWizard');
  WriteBool('DontShowInfo', DontShowAgain.Checked);
  if ModalResult = mrOK then
  begin
    WriteBool('AllPackages', AllPackagesCheck.Checked);
    WriteBool('UseVariants', UseVariantsCheck.Checked);
    {$IFDEF CompilerVersion4}
    WriteBool('OverloadedMethods', GenerateOverloadCheck.Checked);
    WriteBool('UseDefaults', UseDefaultsCheck.Checked);
    {$ENDIF}
    WriteInteger('Case', CaseGroup.ItemIndex);
    WriteBool('TPrefix', TPrefixCheck.Checked);
    WriteBool('APrefix', APrefixCheck.Checked);
    WriteBool('RemoveUnderscore', RemoveUSCheck.Checked);
    WriteBool('PrefixSchema', PrefixSchemaCheck.Checked);
    WriteBool('ThreadSafe', ThreadSafeCheck.Checked);
    WriteBool('PackageComment', PackageCommentCheck.Checked);
    WriteBool('CreateComponent', CreateComponentCheck.Checked);
    WriteBool('CreateResource', CreateResourceCheck.Checked);
    WriteBool('AddToProject', AddToProjectCheck.Checked);
    WriteBool('OpenInIDE', OpenInIDECheck.Checked);
    OpenRegistry('PackageWizard\' + CompilerVersion);
    WriteString('Palette', PaletteEdit.Text);
    WriteString('LastPath', PathEdit.Text);
    WriteString('LastFile', FileEdit.Text);
  end;
  CloseRegistry;
  ClearPackageFunctions;
  SelectedPackages.Free;
end;

function TWizardForm.FinishWizard: Boolean;
var s, Filename, UnitName, Palette: string;
    Options: TCreateModuleFlags;
    F: TextFile;
    i, oi, pi: Integer;
    Pkg: TPackageDefinition;
    Obj: TPackageObject;
    Par: TPackageParameter;
    ComponentList: TStringList;
    TypeWritten: Boolean;
    UnitTypeNames: TStringList;
    ThisTypeName: string;
begin
  Result := False;
  // Compose the full filename
  Filename := Trim(PathEdit.Text);
  if (Filename <> '') and (Filename[Length(Filename)] = '\') then
    Filename := Copy(Filename, 1, Length(Filename) - 1);
  // Check if Path exists
  if not DirectoryExists(Filename) then
  begin
    if Confirm('"' + Filename + '" does not exist,' + #13#10 +
               'Create this directory?', 'Confirm', 'YN') = idNo then Exit;
    ForceDirectories(Filename);
  end;
  Filename := Filename + '\' + FileEdit.Text;
  UnitName := ExtractFilename(Filename);
  if Pos('.', UnitName) > 0 then SetLength(UnitName, Pos('.', UnitName) - 1);
  if ExtractFileExt(Filename) = '' then Filename := Filename + '.pas';
  // Does it exist?
  if FileExists(Filename) then
  begin
    if Confirm('"' + Filename + '" already exists,' + #13#10 +
         'Do you want to overwrite this file?', 'Confirm', 'YN') = idNo then Exit;
  end;
  // List to memorize all types created in this unit
  UnitTypeNames := TStringList.Create;
  // List to memorize the components
  ComponentList := TStringList.Create;
  // Reset the sequence number for unnamed subtypes
  UnitTypeSequence := 0;
  // Open the file and write the source
  AssignFile(F, Filename);
  Screen.Cursor := crHourGlass;
  ProgressBar2.Position := 0;
  ProgressBar2.Max := SelectedPackages.Count * 3;
  ProgressPanel2.Align := alClient;
  ProgressPanel2.Visible := True;
  ProgressPanel2.Refresh;
  try
    Rewrite(F);
    WriteLn(F, 'unit ', UnitName, ';');
    WriteLn(F, '');
    WriteLn(F, '// Oracle Package Wizard ' + WizardVersion);
    WriteLn(F, '// File ' + ExtractFilename(Filename) + ' generated by ' + OSUser + ' on ' + DateTimeToStr(Now));
    if SelectedPackages.Count = 1 then
    begin
      Pkg := SelectedPackages.Objects[0] as TPackageDefinition;
      WriteLn(F, '// This unit contains interface objects for oracle package ' + Pkg.Owner + '.' + Pkg.Name);
    end else begin
      WriteLn(F, '// This unit contains interface objects for the following oracle packages:');
      for i := 0 to SelectedPackages.Count - 1 do
      begin
        Pkg := SelectedPackages.Objects[i] as TPackageDefinition;
        WriteLn(F, '// - ' + Pkg.Owner + '.' + Pkg.Name);
      end;
    end;
    WriteLn(F, '// WARNING: Modifications made to this file will be lost after regeneration!');
    WriteLn(F, '');
    WriteLn(F, 'interface');
    WriteLn(F, '');
    WriteLn(F, 'uses Classes, SysUtils, Oracle;');
    // We haven't written 'type' yet
    TypeWritten := False;
    // Create all record types
    for i := 0 to SelectedPackages.Count - 1 do
    begin
      if SelectedPackages.Objects[i] <> nil then
      begin
        Pkg := SelectedPackages.Objects[i] as TPackageDefinition;
        Pkg.UseVariants   := UseVariantsCheck.Checked;
        Pkg.UseOverload   := GenerateOverloadCheck.Checked;
        Pkg.UseDefaults   := UseDefaultsCheck.Checked;
        Pkg.UseSchemaName := PrefixSchemaCheck.Checked;
        Pkg.ThreadSafe    := ThreadSafeCheck.Checked;
        Pkg.IncludeSpec   := PackageCommentCheck.Checked;
        Pkg.IsComponent   := CreateComponentCheck.Checked;
        for oi := 0 to Pkg.ObjectCount - 1 do
        begin
          Obj := Pkg[oi];
          if Obj.Selected then
          begin
            for pi := Obj.FlatParameterCount - 1 downto 0 do
            begin
              Par := Pkg[oi].FlatParameters[pi];
              if Par.IsRecord then
              begin
                ThisTypeName := AnsiUpperCase(Par.DelphiType);
                if UnitTypeNames.IndexOf(ThisTypeName) < 0 then
                begin
                  WriteLn(F, '');
                  if not TypeWritten then WriteLn(F, 'type');
                  TypeWritten := True;
                  WriteLn(F, Par.DelphiTypeInterface);
                  UnitTypeNames.AddObject(ThisTypeName, Par);
                end;
              end;
            end;
          end;
        end;
      end;
      ProgressBar2.Position := ProgressBar2.Position + 1;
    end;
    // Create the package objects
    for i := 0 to SelectedPackages.Count - 1 do
    begin
      if SelectedPackages.Objects[i] <> nil then
      begin
        Pkg := SelectedPackages.Objects[i] as TPackageDefinition;
        WriteLn(F, '');
        if not TypeWritten then WriteLn(F, 'type');
        TypeWritten := True;
        WriteLn(F, Pkg.InterfaceSource);
        ComponentList.Add(Pkg.DelphiName);
      end;
      ProgressBar2.Position := ProgressBar2.Position + 1;
    end;
    // The default PL/SQL Table size
    WriteLn(F, '');
    WriteLn(F, 'var');
    WriteLn(F, '  DefaultPLSQLTableSize: Integer = 100; // Default size of a PL/SQL Table');
    WriteLn(F, '');
    // The Register procedure if the user selected to create components
    if CreateComponentCheck.Checked then
    begin
      WriteLn(F, 'procedure Register;');
      WriteLn(F, '');
    end;
    // Interface done, now the implementation
    WriteLn(F, 'implementation');
    // First the record types
    for i := 0 to UnitTypeNames.Count - 1 do
    begin
      Par := UnitTypeNames.Objects[i] as TPackageParameter;
      s := Par.DelphiTypeImplementation;
      if s <> '' then
      begin
        WriteLn(F, '');
        WriteLn(F, s);
      end;
    end;
    // The object packages
    for i := 0 to SelectedPackages.Count - 1 do
    begin
      if SelectedPackages.Objects[i] <> nil then
      begin
        Pkg := SelectedPackages.Objects[i] as TPackageDefinition;
        WriteLn(F, '');
        WriteLn(F, Pkg.ImplementationSource);
      end;
      ProgressBar2.Position := ProgressBar2.Position + 1;
    end;
    // The Register procedure
    if CreateComponentCheck.Checked then
    begin
      Palette := Trim(PaletteEdit.Text);
      if Palette = '' then Palette := 'Data Access';
      WriteLn(F, '');
      WriteLn(F, 'procedure Register;');
      WriteLn(F, 'begin');
      for i := 0 to ComponentList.Count - 1 do
        WriteLn(F, '  RegisterComponents(''' + Palette + ''', [' + ComponentList[i] + ']);');
      WriteLn(F, 'end;');
    end;
    // Done!
    WriteLn(F, '');
    WriteLn(F, 'end.');
    CloseFile(F);
    // Create the .DCR file, if the user indicated so
    if CreateComponentCheck.Checked and CreateResourceCheck.Checked then
    begin
      SaveResourceFile(ChangeFileExt(Filename, '.dcr'), ComponentList);
    end;
    // All actions successfully completed, clear the form and show a message
    ClearPackageFunctions;
    Result := True;
  except
    on E:Exception do
    begin
      ShowMessage('Error writing file' + #13#10 + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
  // Free some stuff
  ComponentList.Free;
  UnitTypeNames.Free;
  // Perform actions in case generation was successful
  if Result then
  begin
    // Add the file to the project and open it in the IDE, if requested
    if ToolServices <> nil then
    begin
      ToolServices.ReloadFile(Filename);
      if ToolServices.CloseFile(Filename) then
      begin
        Options := [cmExisting];
        if OpenInIDECheck.Checked then Options := Options + [cmShowSource];
        if AddToProjectCheck.Checked then Options := Options + [cmAddToProject];
        ToolServices.CreateModuleEx(Filename, '', '', '', nil, nil, Options);
      end;
    end;
    // Ask to generate another package
    s := Filename + ' generated successfully.' + #13#10;
    s := s + 'Do you want to generate another package?';
    if Confirm(s, 'Confirm', 'YN') = idYES then
    begin
      PackageWizardControl.ActivePage := PackagePage1;
      SetButtons;
      Result := False;
    end;
  end;
  ProgressPanel2.Visible := False;
end;

procedure TWizardForm.SetButtons;
begin
  with PackageWizardControl do
  begin
    BackBtn.Enabled := ActivePage.PageIndex > 0;
    if ActivePage.PageIndex = (PageCount - 1) then
      NextBtn.Caption := '&Finish'
    else
      NextBtn.Caption := '&Next >';
    case ActivePage.PageIndex of
      0 : NextBtn.Enabled := True;
      1 : NextBtn.Enabled := (PackageListBox.SelCount > 0) and AllArgumentsOkay;
      2 : NextBtn.Enabled := (PackageFunctionsTree.Items.Count >= 0);
      3 : NextBtn.Enabled := True;
      4 : NextBtn.Enabled := (FileEdit.Text <> '');
    end;
  end;
end;

procedure TWizardForm.ClearPackageFunctions;
var i: Integer;
begin
  for i := 0 to SelectedPackages.Count - 1 do
  begin
    if SelectedPackages.Objects[i] <> nil then
      TPackageDefinition(SelectedPackages.Objects[i]).Free;
  end;
  SelectedPackages.Clear;
end;

procedure TWizardForm.FetchPackages;
var s: string;
    i, p: integer;
    Pkg: TPackageDefinition;
begin
  if PackageListBox.ItemIndex < 0 then
    ClearPackageFunctions
  else begin
    s := '';
    for i := 0 to PackageListBox.Items.Count - 1 do
    begin
      if PackageListBox.Selected[i] then s := s + PackageListBox.Items[i] + #13#10;
    end;
    // If nothing changed, don't bother querying again
    if SelectedPackages.Text = s then Exit;
    NameStyleChanged := True;
    ClearPackageFunctions;
    Refresh;
    SelectedPackages.Text := s;
    try
      ProgressBar1.Position := 0;
      ProgressBar1.Max := SelectedPackages.Count;
      ProgressPanel1.Align := alClient;
      ProgressPanel1.Visible := True;
      ProgressPanel1.Refresh;
      Screen.Cursor := crSQLWait;
      for i := 0 to SelectedPackages.Count - 1 do
      begin
        s := SelectedPackages.Strings[i];
        p := Pos('.', s);
        if p > 0 then
        begin
          Pkg := TPackageDefinition.Create(Copy(s, 1, p - 1), Copy(s, p + 1, Length(S)), WizardSession);
          SelectedPackages.Objects[i] := Pkg;
          Pkg.ReadFromDictionary;
          ProgressBar1.Position := i + 1;
        end;
      end;
    finally
      Screen.Cursor := crDefault;
      ProgressPanel1.Visible := False;
    end;
  end
end;

// Copy changes in Tree back to Packages and Objects
procedure TWizardForm.AdjustObjects;
var Node: TTreeNode;
    S: string;
    p: Integer;
    Pkg: TPackageDefinition;
begin
  with PackageFunctionsTree do
  begin
    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      if Node.Data <> nil then
      begin
        if TObject(Node.Data) is TPackageDefinition then
        begin
          TPackageDefinition(Node.Data).DelphiName := Node.Text;
        end;
        if TObject(Node.Data) is TPackageObject then
        begin
          TPackageObject(Node.Data).DelphiName := Node.Text;
          TPackageObject(Node.Data).Selected   := GetStateIndex(Node) = 2;
        end;
        if TObject(Node.Data) is TPackageParameter then
        begin
          S := Node.Text;
          p := Pos(' = ', S);
          if p > 0 then SetLength(S, p - 1);
          TPackageParameter(Node.Data).DelphiName := S;
        end;
      end;
      Node := Node.GetNext;
    end;
  end;
  if SelectedPackages.Count = 1 then
  begin
    Pkg := TPackageDefinition(SelectedPackages.Objects[0]);
    if Pkg.DelphiName <> '' then
    begin
      p := 1;
      if TPrefixCheck.Checked and (Pkg.DelphiName[1] = 'T') then p := 2;
      s := Copy(Pkg.DelphiName, p, 255);
      if p = 1 then s := s + 'Unit';
      FileEdit.Text := s + '.pas';
    end;
  end;
end;

procedure TWizardForm.HelpBtnClick(Sender: TObject);
begin
  HelpByIndex(Self, 1000 + PackageWizardControl.ActivePage.PageIndex);
end;

procedure TWizardForm.BackBtnClick(Sender: TObject);
begin
  with PackageWizardControl do ActivePage := Pages[ActivePage.PageIndex - 1];
  SetButtons;
end;

procedure TWizardForm.NextBtnClick(Sender: TObject);
begin
  if PackageFunctionsTree.IsEditing then
  begin
    {$IFNDEF LINUX}
    PackageFunctionsTree.Selected.EndEdit(False);
    {$ENDIF}
    Exit;
  end;
  with PackageWizardControl do
  begin
    if ActivePage.PageIndex = (PageCount - 1) then
    begin
      if FinishWizard then ModalResult := mrOK;
      Exit;
    end else begin
      ActivePage := Pages[ActivePage.PageIndex + 1];
      SetButtons;
    end;
    Refresh;
    case ActivePage.PageIndex of
      0 : ;
      1 : ;
      2 : FetchPackages;
      3 : ConvertNames;
      4 : AdjustObjects;
    end;
  end;
end;

procedure TWizardForm.LogonBtnClick(Sender: TObject);
begin
  InitOracleLogon(WizardLogon);
  try
    Screen.Cursor := crHourGlass;
    WizardLogon.Execute;
  finally
    Screen.Cursor := crDefault;
  end;
end;

// Test if sys.all_arguments is accessible
procedure TWizardForm.TestAllArguments;
begin
  AllArgumentsOkay := False;
  try
    TestAllArgumentsQuery.Describe;
    AllArgumentsOkay := True
  except
    on E: Exception do
    begin
      ShowMessage('Dictionary view sys.all_arguments is not accessible, Package Wizard'#13#10 +
                  'cannot continue. Make the view available before retrying.'#13#10 +
                  'Note: On early Oracle 7.2 and 7.3 versions this view does not exist.'#13#10 +
                  'Contact your Oracle Representative for a patch.'#13#10#13#10 +
                  '(Error = ' + Trim(E.Message) + ')');
    end;
  end;
end;

procedure TWizardForm.WizardSessionChange(Sender: TOracleSession);
begin
  PackageListBox.Clear;
  if not WizardSession.Connected then
    LogonPanel.Caption := 'not connected'
  else begin
    Logonpanel.Caption := 'connected as ' + WizardSession.LogonUsername;
    if WizardSession.LogonDatabase <> '' then
      Logonpanel.Caption := Logonpanel.Caption + '@' + WizardSession.LogonDatabase;
    LogonPanel.Refresh;
    try
      if AllPackagesCheck.Checked then
        PackageQuery.SetVariable('owner', '')
      else
        PackageQuery.SetVariable('owner', 'and owner = user');
      PackageQuery.Execute;
      while not PackageQuery.Eof do
      begin
        if PackageQuery.Field('status') <> 'INVALID' then
        begin
          PackageListBox.Items.Add(PackageQuery.Field('owner') + '.' + PackageQuery.Field('object_name'));
        end;
        PackageQuery.Next;
      end;
      TestAllArguments;
    except
      on E:Exception do ShowMessage('Error reading packages from dictionary:'#13#10 + E.Message);
    end;
  end;
  SetButtons;
end;

procedure TWizardForm.PackageListBoxClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TWizardForm.AllPackagesCheckClick(Sender: TObject);
begin
  WizardSessionChange(nil);
end;

procedure TWizardForm.PackageFunctionsTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Node: TTreeNode;
begin
  if Button = mbLeft then
  begin
    Node := PackageFunctionsTree.GetNodeAt(X, Y);
    if (Node <> nil) and (GetStateIndex(Node) < 3) then
    begin
      {$IFNDEF LINUX}
      if htOnStateIcon in PackageFunctionsTree.GetHitTestInfoAt(X, Y) then
      begin
        SetStateIndex(Node, GetStateIndex(Node) xor 3);
      end;
      {$ENDIF}
    end;
  end;
end;

procedure TWizardForm.PackageFunctionsTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (Key = vk_F2) then
  begin
    if PackageFunctionsTree.Selected <> nil then PackageFunctionsTree.Selected.EditText;
    Key := 0;
  end;
end;

procedure TWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (SelectedPackages.Text = '') or
              (Confirm('Are you sure you want to close the Oracle Package Wizard', 'Confirm', 'YN') = idYES);
end;

procedure TWizardForm.SourceStyleChange(Sender: TObject);
begin
  CreateResourceCheck.Enabled := CreateComponentCheck.Checked;
  PaletteEdit.Enabled := CreateComponentCheck.Checked;
  PaletteLabel.Enabled := CreateComponentCheck.Checked;
  AddToProjectCheck.Enabled := not CreateComponentCheck.Checked;
  AddToProjectCheck.Checked := False;
end;

procedure TWizardForm.NameStyleChange(Sender: TObject);
var S: string;
begin
  S := 'DBMS_OUTPUT';
  S := S + '  ->  ' + ConvertName(S, True, False);
  ExamplePanel.Caption := S;
  NameStyleChanged := True;
end;

procedure TWizardForm.FileEditChange(Sender: TObject);
begin
  SetButtons;
end;

function TWizardForm.ConvertName(Name: string; IsObject, IsParameter: Boolean): string;
var i: Integer;
    Capital, ToUpper, ToLower: Boolean;
    C, PrevChar: Char;
begin
  Result := '';
  PrevChar := ' ';
  for i := 1 to Length(Name) do
  begin
    Capital := (Result = '') or (PrevChar = '_');
    ToLower := False;
    ToUpper := False;
    case CaseGroup.ItemIndex of
      1 : begin
            ToUpper := Capital;
            ToLower := not ToUpper;
          end;
      2 : ToUpper := True;
      3 : ToLower := True
    end;
    C := Name[i];
    if (C = '$') or (C = '#') or (C = ' ') or (C = '.') or (C = '%') then C := '_';
    if ToUpper then C := AnsiUpperCase(C)[1];
    if ToLower then C := AnsiLowerCase(C)[1];
    if not (RemoveUSCheck.Checked and (C = '_')) then Result := Result + C;
    PrevChar := C;
  end;
  if IsObject and TPrefixCheck.Checked then Result := 'T' + Result;
  if IsParameter and APrefixCheck.Checked then Result := 'A' + Result;
  if StrReserved(Result) then Result := Result + '1';
end;

// Convert names and copy them to list
procedure TWizardForm.ConvertNames;
var i, oi, pi: integer;
    Node, PkgNode, ObjNode: TTreeNode;
    Pkg: TPackageDefinition;
    Obj: TPackageObject;
    Par: TPackageParameter;
    S: string;
begin
  if not NameStyleChanged then Exit;
  PackageFunctionsTree.Items.Clear;
  PackageFunctionsTree.Refresh;
  PackageFunctionsTree.Items.BeginUpdate;
  try
    for i := 0 to SelectedPackages.Count - 1 do
    begin
      Pkg := TPackageDefinition(SelectedPackages.Objects[i]);
      if Pkg <> nil then
      begin
        PkgNode := PackageFunctionsTree.Items.AddObject(nil, ConvertName(Pkg.Name, True, False), Pkg);
        SetStateIndex(PkgNode, 9);
        for oi := 0 to Pkg.ObjectCount - 1 do
        begin
          Obj := Pkg[oi];
          S := Obj.Name;
          if Pos('"' + UpperCase(S) + '"', '"NAME", "SESSION", "CURSOR", "PACKAGESPECIFICATION"') > 0 then
            S := S + '1';
          if (not GenerateOverloadCheck.Checked) and Obj.IsOverloaded then
            S := S + '_' + Obj.OverloadId;
          ObjNode := PackageFunctionsTree.Items.AddChildObject(PkgNode, ConvertName(S, False, False), Obj);
          if not Obj.IsSupported then
            SetStateIndex(ObjNode, 3)
          else
            if Obj.Selected then SetStateIndex(ObjNode, 2) else SetStateIndex(ObjNode, 1);
          for pi := 0 to Obj.ParameterCount - 1 do
          begin
            Par := Obj[pi];
            Node := PackageFunctionsTree.Items.AddChildObject(ObjNode, ConvertName(Par.Name, False, True) + ' = ' + Par.DataType, Par);
            SetStateIndex(Node, 4);
          end;
        end;
        PkgNode.Expand(False);
      end;
    end;
    PackageFunctionsTree.TopItem := PackageFunctionsTree.Items[0];
  finally
    PackageFunctionsTree.Items.EndUpdate;
  end;
  NameStyleChanged := False;
end;

procedure TWizardForm.PackageFunctionsTreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
var p: Integer;
begin
  S := Trim(S);
  p := Pos('=', S);
  if p > 0 then S := Trim(Copy(S, 1, p - 1));
  if (Node.Data <> nil) and (TObject(Node.Data) is TPackageParameter) then
  begin
    S := S + ' = ' + TPackageParameter(Node.Data).DataType;
  end;
end;

procedure TWizardForm.PathBrowsBtnClick(Sender: TObject);
var Dir: string;
begin
  Dir := PathEdit.Text;
  {$IFNDEF LINUX}
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    PathEdit.Text := Dir;
  end;
  {$ENDIF}
end;

procedure TWizardForm.PackageListBoxDblClick(Sender: TObject);
begin
  NextBtnClick(nil);
end;

procedure TWizardForm.CancelBtnClick(Sender: TObject);
begin
  if PackageFunctionsTree.IsEditing then
    {$IFNDEF LINUX}
    PackageFunctionsTree.Selected.EndEdit(True)
    {$ENDIF}
  else
    ModalResult := mrCancel;
end;

procedure TWizardForm.UseVariantsCheckClick(Sender: TObject);
begin
  {$IFDEF CompilerVersion4}
  if not UseVariantsCheck.Checked then
    GenerateOverloadCheck.Enabled := True
  else begin
    GenerateOverloadCheck.Checked := False;
    GenerateOverloadCheck.Enabled := False;
  end;
  {$ENDIF}
  NameStyleChange(nil);
end;

initialization
  OracleExecutor := TOracleToolExecutor.Create;

finalization
  OracleExecutor.Free;
  OracleExecutor := nil;

end.



