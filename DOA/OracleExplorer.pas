// Direct Oracle Access - Explorer unit
// Copyright 1998 - 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleExplorer;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, Printers, Menus,
  Oracle, OraclePreferences, OracleQB, OracleVisual;
{$ELSE}
uses
  SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, QButtons, QImgList, Printers,
  QMenus, QTypes, Oracle, OraclePreferences, OracleVisual;
{$ENDIF}

type
  TExplorerObject = class(TCollectionItem)
  public
    ObjectType: string;
    ObjectOwner: string;
    ObjectName: string;
    ParentObject: TExplorerObject;
    Node: TTreeNode;
    constructor CreateOnNode(ACollection: TCollection; ANode: TTreeNode);
    function IsProgramUnit: Boolean;
    function IsConstraint: Boolean;
    function IsColumn: Boolean;
    function HasData: Boolean;
    function HasSource: Boolean;
  end;
  TExplorerForm = class(TForm)
    TopPanel: TPanel;
    BodyPanel: TPanel;
    StatusBar: TStatusBar;
    Browser: TTreeView;
    ImageList: TImageList;
    ObjectQuery: TOracleQuery;
    Session: TOracleSession;
    Logon: TOracleLogon;
    ConstraintQuery: TOracleQuery;
    ForeignKeyQuery: TOracleQuery;
    ConsColumnsQuery: TOracleQuery;
    TriggerQuery: TOracleQuery;
    ReferencesQuery: TOracleQuery;
    ReferencedByQuery: TOracleQuery;
    IndexQuery: TOracleQuery;
    IndColumnsQuery: TOracleQuery;
    FKReferenceQuery: TOracleQuery;
    SplitterPanel: TPanel;
    LogonBtn: TSpeedButton;
    PBox: TPaintBox;
    RightPanel: TPanel;
    ExitBtn: TSpeedButton;
    OkayBtn: TSpeedButton;
    ColumnQuery: TOracleQuery;
    SequencePropertiesQuery: TOracleQuery;
    ProgSourceQuery: TOracleQuery;
    ConstraintSourceQuery: TOracleQuery;
    TriggerSourceQuery: TOracleQuery;
    ConstraintPropertiesQuery: TOracleQuery;
    ColumnPropertiesQuery: TOracleQuery;
    IndexPropertiesQuery: TOracleQuery;
    BasicPropertiesQuery: TOracleQuery;
    PrintBtn: TSpeedButton;
    SetupBtn: TSpeedButton;
    FontDialog: TFontDialog;
    Bevel1: TBevel;
    SetupPopup: TPopupMenu;
    BrowserFont: TMenuItem;
    TextFont: TMenuItem;
    ListFont: TMenuItem;
    N1: TMenuItem;
    PrinterSetup: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    RichEdit: TRichEdit;
    PageControl: TPageControl;
    ListPage: TTabSheet;
    TextPage: TTabSheet;
    EmptyPage: TTabSheet;
    EmptyPanel: TPanel;
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure BrowserExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure BrowserChange(Sender: TObject; Node: TTreeNode);
    procedure OkayBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure DropIt(Sender, Source: TObject; X, Y: Integer);
    procedure DragIt(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SessionChange(Sender: TOracleSession);
    procedure LogonBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure BrowserFontClick(Sender: TObject);
    procedure TextFontClick(Sender: TObject);
    procedure ListFontClick(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    Start: Integer;
    Drag: Boolean;
    OraclePackage: TOraclePackage;
    ExplorerObjects: TCollection;
    SelectedItem: String;
    procedure StatusMessage(const Msg: string);
    procedure AddFolders(Node: TTreeNode; const Folders: Array of String);
    procedure ClearBrowser;
    procedure GetObjects(Node: TTreeNode; const ObjectType: string; CanOpen: Boolean);
    procedure GetConstraints(Node: TTreeNode; ConstraintType: string);
    procedure GetConsColumns(Node: TTreeNode);
    procedure GetTriggers(Node: TTreeNode);
    procedure GetReferences(Node: TTreeNode; const ObjectType: string);
    procedure GetReferencedBy(Node: TTreeNode; const ObjectType: string);
    function  OwnerPrefix(const Owner: string): string;
    procedure GetIndexes(Node: TTreeNode);
    procedure GetIndColumns(Node: TTreeNode);
    procedure GetFKReferences(Node: TTreeNode);
    procedure QueryColumns(const TableOwner, TableName: string);
    procedure QueryProperties(Query: TOracleQuery; Obj: TExplorerObject);
    procedure QuerySource(const ObjectType, ObjectOwner, ObjectName: string);
    procedure ShowProperties(ExplorerObject: TExplorerObject);
    procedure SetFonts;
    procedure PrintListView(const PrintTitle, PrintCaption: string);
    function  SelectFont(Font: TFont; Section: string): Boolean;
  end;

function ExecuteExplorer(S: TOracleSession; P: TOraclePackage): Boolean;

implementation

{$R *.dfm}

var
  LeftMargin: Integer = 1;
  TopMargin:  Integer = 1;

// Start the explorer
function ExecuteExplorer(S: TOracleSession; P: TOraclePackage): Boolean;
begin
  Result := False;
  with TExplorerForm.Create(nil) do
  begin
    SetFonts;
    OraclePackage := P;
    if S <> nil then
    begin
      Session.LogonUsername := S.LogonUsername;
      Session.LogonPassword := S.LogonPassword;
      Session.LogonDatabase := S.LogonDatabase;
      Caption := S.Owner.Name + '.' + S.Name + ' Explorer';
    end else begin
      if OpenRegistry('Logon') then
      begin
        // Read logon settings from registry
        Session.LogonUsername := ReadString('Username', '');
        Session.LogonPassword := ReadString('Password', '');
        Session.LogonDatabase := ReadString('Database', '');
        CloseRegistry;
      end;
    end;
    if P <> nil then
      Caption := P.Owner.Name + '.' + P.Name + ' Explorer';
    if (ShowModal = mrOK) then
    begin
      // Modify the OraclePackage
      OraclePackage.PackageName := SelectedItem;
      Result := True;
    end;
    Free;
  end;
end;

// TExplorerObject Object

constructor TExplorerObject.CreateOnNode(ACollection: TCollection; ANode: TTreeNode);
begin
  inherited Create(ACollection);
  Node := ANode;
  Node.Data := Self;
end;

function TExplorerObject.IsProgramUnit: Boolean;
begin
  Result := (ObjectType = 'FUNCTION') or (ObjectType = 'PROCEDURE') or
            (ObjectType = 'PACKAGE') or (ObjectType = 'PACKAGE BODY') or
            (ObjectType = 'TYPE') or (ObjectType = 'TYPE BODY') or
            (ObjectType = 'TRIGGER');
end;

function TExplorerObject.IsConstraint: Boolean;
begin
  Result := (ObjectType = 'CHECK CONSTRAINT') or (ObjectType = 'FOREIGN KEY') or
            (ObjectType = 'PRIMARY KEY') or (ObjectType = 'UNIQUE KEY');
end;

function TExplorerObject.IsColumn: Boolean;
begin
  Result := (ObjectType = 'COLUMN') or (ObjectType = 'CONSTRAINT COLUMN') or
            (ObjectType = 'INDEX COLUMN');
end;

function TExplorerObject.HasData: Boolean;
begin
  Result := (ObjectType = 'TABLE') or (ObjectType = 'VIEW');
end;

function TExplorerObject.HasSource: Boolean;
begin
  Result := (ObjectType = 'CHECK CONSTRAINT') or IsProgramUnit;
end;

// Explorer Mainform

// Show a message in the statusbar
procedure TExplorerForm.StatusMessage(const Msg: string);
begin
  StatusBar.SimpleText := Msg;
end;

// Add a number of items to the TreeView
procedure TExplorerForm.AddFolders(Node: TTreeNode; const Folders: Array of String);
var i: Integer;
    NewNode: TTreeNode;
begin
  for i := 0 to High(Folders) do
  begin
    NewNode := Browser.Items.AddChild(Node, Folders[i]);
    NewNode.ImageIndex    := 1;
    NewNode.SelectedIndex := 1;
    Browser.items.AddChild(NewNode, '');
  end;
end;

// Clear and initialize the TreeView
procedure TExplorerForm.ClearBrowser;
begin
  LockWindowUpdate(Integer(Handle));
  with Browser.Items do
  begin
    Clear;
    AddFolders(Browser.Selected, ['Tables', 'Views', 'Sequences', 'Functions',
                                  'Procedures', 'Packages', 'Package bodies',
                                  'Types', 'Type bodies']);
  end;
  ExplorerObjects.Clear;
  LockWindowUpdate(0);
end;

// The session has connected or disconnected
procedure TExplorerForm.SessionChange(Sender: TOracleSession);
var S: string;
begin
  ClearBrowser;
  LogonBtn.Down := Session.Connected;
  if Session.Connected then
  begin
    S := Session.LogonUsername;
    if Session.LogonDatabase <> '' then S := S + '@' + Session.LogonDatabase;
    StatusMessage('Connected as ' + S);
  end else
    StatusMessage('Not connected');
end;

// Manually log on or off
procedure TExplorerForm.LogonBtnClick(Sender: TObject);
begin
  if Sender = nil then
  begin
    StatusMessage('Logging on...');
    Refresh;
  end;
  InitOracleLogon(Logon);
  Logon.Execute;
  StoreLogonParams(Session);
end;

procedure TExplorerForm.FormCreate(Sender: TObject);
begin
  if OpenRegistry('Explorer') then
  begin
    Left := ReadInteger('Left', Left);
    Top := ReadInteger('Top', Top);
    Width := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    Browser.Width := ReadInteger('Splitter', Browser.Width);
    WindowState := TWindowState(ReadInteger('State', Ord(WindowState)));
    CloseRegistry;
  end;
  Start := -1;
  Drag := False;
  ExplorerObjects := TCollection.Create(TExplorerObject);
  ClearBrowser;
  {$IFDEF LINUX}
  PrintBtn.Enabled := False;
  SetupBtn.Enabled := False;
  {$ELSE}
  ChangeGlyphInit(Self);
  ChangeGlyphs(TopPanel);
  ChangeGlyphs(RightPanel);
  ChangeGlyphClose;
  {$ENDIF}
end;

// When the form activates, try to logon and select a package
procedure TExplorerForm.FormActivate(Sender: TObject);
var Node: TTreeNode;
    S: string;
begin
  LogonBtnClick(nil);
  if Session.Connected and (OraclePackage <> nil) then
  begin
    // Search for the Package and select it
    S := 'Packages';
    LockWindowUpdate(Integer(Handle));
    Node := Browser.TopItem;
    repeat
      if Node.Text = S then
      begin
        Node.Selected:=True;
        if Node.Parent <> nil then
          Break
        else begin
          Node.Expand(False);
          S := OraclePackage.PackageName;
        end;
      end;
      Node := Node.GetNextVisible;
    until Node = nil;
    LockWindowUpdate(0);
  end;
  InitForm(Self);
end;

// Save settings in HKEY_CURRENT_USER/Software/DOA/Explorer
procedure TExplorerForm.FormDestroy(Sender: TObject);
begin
  if OpenRegistry('Explorer') then
  begin
    if WindowState = wsNormal then
    begin
      WriteInteger('Left', Left);
      WriteInteger('Top', Top);
      WriteInteger('Width', Width);
      WriteInteger('Height', Height);
    end;
    WriteInteger('State', Ord(WindowState));
    WriteInteger('Splitter', Browser.Width);
    CloseRegistry;
  end;
end;

// Select objects from the database
procedure TExplorerForm.GetObjects(Node: TTreeNode; const ObjectType: string; CanOpen: Boolean);
var NewNode: TTreeNode;
    ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with ObjectQuery do
  try
    SetVariable('object_type', ObjectType);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('synonym_name'));
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType  := ObjectType;
      ExplorerObject.ObjectOwner := Field('object_owner');
      ExplorerObject.ObjectName  := Field('object_name');
      if Field('status') <> 'VALID' then
      begin
        NewNode.ImageIndex    := 3;
        NewNode.SelectedIndex := 3;
      end else begin
        NewNode.ImageIndex    := 2;
        NewNode.SelectedIndex := 2;
      end;
      if CanOpen then Browser.Items.AddChild(NewNode, '');
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for constraints
procedure TExplorerForm.GetConstraints(Node: TTreeNode; ConstraintType: string);
var NewNode: TTreeNode;
    ParentObject, ExplorerObject: TExplorerObject;
    s: string;
    Q: TOracleQuery;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  if ConstraintType = 'R' then Q := ForeignKeyQuery else Q := ConstraintQuery;
  with Q do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('table_name', ParentObject.ObjectName);
    SetVariable('owner', ParentObject.ObjectOwner);
    SetVariable('constraint_type', ConstraintType);
    Execute;
    while not EOF do
    begin
      s := Field('constraint_name');
      if ConstraintType = 'R' then s := Field('r_table_name') + '  (' + s + ')';
      NewNode := Browser.Items.AddChild(Node, s);
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      case ConstraintType[1] of
       'P': ExplorerObject.ObjectType   := 'PRIMARY KEY';
       'U': ExplorerObject.ObjectType   := 'UNIQUE KEY';
       'R': ExplorerObject.ObjectType   := 'FOREIGN KEY';
       'C': ExplorerObject.ObjectType   := 'CHECK CONSTRAINT';
      end;
      ExplorerObject.ObjectOwner  := ParentObject.ObjectOwner;
      ExplorerObject.ObjectName   := Field('constraint_name');
      ExplorerObject.ParentObject := ParentObject;
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Browser.Items.AddChild(NewNode, '');
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for constraint columns
procedure TExplorerForm.GetConsColumns(Node: TTreeNode);
var NewNode: TTreeNode;
    ParentObject, GrandParentObject, ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with ConsColumnsQuery do
  try
    ParentObject := Node.Parent.Data;
    GrandParentObject := ParentObject.ParentObject;
    SetVariable('table_name', GrandParentObject.ObjectName);
    SetVariable('owner', GrandParentObject.ObjectOwner);
    SetVariable('constraint_name', ParentObject.ObjectName);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('column_name'));
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType   := 'CONSTRAINT COLUMN';
      ExplorerObject.ObjectOwner  := GrandParentObject.ObjectOwner;
      ExplorerObject.ObjectName   := Field('column_name');
      ExplorerObject.ParentObject := GrandParentObject;
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for triggers
procedure TExplorerForm.GetTriggers(Node: TTreeNode);
var NewNode: TTreeNode;
    ParentObject, ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with TriggerQuery do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('table_name', ParentObject.ObjectName);
    SetVariable('table_owner', ParentObject.ObjectOwner);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('trigger_name'));
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType   := 'TRIGGER';
      ExplorerObject.ObjectOwner  := Field('trigger_owner');
      ExplorerObject.ObjectName   := Field('trigger_name');
      ExplorerObject.ParentObject := ParentObject;
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Browser.Items.AddChild(NewNode, '');
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Insert an owner
function TExplorerForm.OwnerPrefix(const Owner: string): string;
begin
  if CompareText(Owner, Session.LogonUsername) = 0 then
    Result := ''
  else
    Result := Owner + '.';
end;

// Search for references
procedure TExplorerForm.GetReferences(Node: TTreeNode; const ObjectType: string);
var NewNode: TTreeNode;
    s: string;
    ParentObject, ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with ReferencesQuery do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('name', ParentObject.ObjectName);
    SetVariable('owner', ParentObject.ObjectOwner);
    SetVariable('type', ParentObject.ObjectType);
    Execute;
    while not EOF do
    begin
      s := Field('referenced_type') + ' ' + OwnerPrefix(Field('referenced_owner')) +
           Field('referenced_name');
      NewNode := Browser.Items.AddChild(Node, s);
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType  := Field('referenced_type');
      ExplorerObject.ObjectOwner := Field('referenced_owner');
      ExplorerObject.ObjectName  := Field('referenced_name');
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for referenced by
procedure TExplorerForm.GetReferencedBy(Node: TTreeNode; const ObjectType: string);
var NewNode: TTreeNode;
    ParentObject, ExplorerObject: TExplorerObject;
    s: string;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with ReferencedByQuery do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('referenced_name', ParentObject.ObjectName);
    SetVariable('referenced_owner', ParentObject.ObjectOwner);
    SetVariable('referenced_type', ParentObject.ObjectType);
    Execute;
    while not EOF do
    begin
      s := Field('type') + ' ' + OwnerPrefix(Field('owner')) + Field('name');
      NewNode := Browser.Items.AddChild(Node, s);
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType  := Field('type');
      ExplorerObject.ObjectOwner := Field('owner');
      ExplorerObject.ObjectName  := Field('name');
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for indexes
procedure TExplorerForm.GetIndexes(Node: TTreeNode);
var NewNode: TTreeNode;
    ParentObject, ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with IndexQuery do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('table_name', ParentObject.ObjectName);
    SetVariable('table_owner', ParentObject.ObjectOwner);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('index_name'));
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType   := 'INDEX';
      ExplorerObject.ObjectOwner  := Field('owner');
      ExplorerObject.ObjectName   := Field('index_name');
      ExplorerObject.ParentObject := ParentObject;
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Browser.Items.AddChild(NewNode, '');
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for index columns
procedure TExplorerForm.GetIndColumns(Node: TTreeNode);
var NewNode: TTreeNode;
    ParentObject, GrandParentObject, ExplorerObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with IndColumnsQuery do
  try
    ParentObject := Node.Parent.Data;
    GrandParentObject := ParentObject.ParentObject;
    SetVariable('table_name', GrandParentObject.ObjectName);
    SetVariable('table_owner', GrandParentObject.ObjectOwner);
    SetVariable('index_name', ParentObject.ObjectName);
    SetVariable('index_owner', ParentObject.ObjectOwner);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('column_name'));
      ExplorerObject := TExplorerObject.CreateOnNode(ExplorerObjects, NewNode);
      ExplorerObject.ObjectType   := 'INDEX COLUMN';
      ExplorerObject.ObjectOwner  := ParentObject.ObjectOwner;
      ExplorerObject.ObjectName   := Field('column_name');
      ExplorerObject.ParentObject := GrandParentObject;
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Search for foreign key references
procedure TExplorerForm.GetFKReferences(Node: TTreeNode);
var NewNode: TTreeNode;
    ParentObject: TExplorerObject;
begin
  Node.ImageIndex    := 1;
  Node.SelectedIndex := 1;
  with FKReferenceQuery do
  try
    ParentObject := Node.Parent.Data;
    SetVariable('r_table_name', ParentObject.ObjectName);
    SetVariable('r_owner', ParentObject.ObjectOwner);
    Execute;
    while not EOF do
    begin
      NewNode := Browser.Items.AddChild(Node, Field('table_name') + '  (' + Field('constraint_name') + ')');
      NewNode.ImageIndex    := 2;
      NewNode.SelectedIndex := 2;
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Determine wat has to be shown
procedure TExplorerForm.BrowserExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var FirstChild: TTreeNode;
    Nodes:array[0..10] of string;
    ParentNode: TTreeNode;
    i: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FirstChild := Node.GetFirstChild;
    if (FirstChild <> nil) and (FirstChild.Text = '') then
    begin
      Node.DeleteChildren;
      ParentNode := Node;
      while ParentNode <> nil do
      begin
        for i := 9 downto 0 do Nodes[i + 1] := Nodes[i];
        Nodes[0] := ParentNode.Text;
        ParentNode := ParentNode.Parent;
      end;
      if Nodes[0] = 'Tables' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'TABLE', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['Primary key',
          'Unique keys', 'Foreign keys', 'Check constraints', 'Triggers',
          'Indexes', 'Foreign key references', 'Referenced by']);
          if Nodes[2] = 'Primary key' then
          begin
            if Nodes[3] = '' then GetConstraints(Node, 'P');
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['Columns']);
              if Nodes[4] = 'Columns' then GetConsColumns(Node);
            end;
          end;
          if Nodes[2] = 'Unique keys' then
          begin
            if Nodes[3] = '' then GetConstraints(Node, 'U');
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['Columns']);
              if Nodes[4] = 'Columns' then GetConsColumns(Node);
            end;
          end;
          if Nodes[2] = 'Foreign keys' then
          begin
            if Nodes[3] = '' then GetConstraints(Node, 'R');
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['Columns']);
              if Nodes[4] = 'Columns' then GetConsColumns(Node);
            end;
          end;
          if Nodes[2] = 'Check constraints' then
          begin
            if Nodes[3] = '' then GetConstraints(Node, 'C');
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['Columns']);
              if Nodes[4] = 'Columns' then GetConsColumns(Node);
            end;
          end;
          if Nodes[2] = 'Triggers' then
          begin
            if Nodes[3] = '' then GetTriggers(Node);
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['References']);
              if Nodes[4] = 'References' then GetReferences(Node, 'TRIGGER');
            end;
          end;
          if Nodes[2] = 'Indexes' then
          begin
            if Nodes[3] = '' then GetIndexes(Node);
            if Nodes[3] <> '' then
            begin
              if Nodes[4] = '' then AddFolders(Node, ['Columns']);
              if Nodes[4] = 'Columns' then GetIndColumns(Node);
            end;
          end;
          if Nodes[2] = 'Foreign key references' then GetFKReferences(Node);
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'TABLE');
        end;
      end;
      if Nodes[0] = 'Views' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'VIEW', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References', 'Referenced by']);
          if Nodes[2] = 'References' then GetReferences(Node, 'VIEW');
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'VIEW');
        end;
      end;
      if Nodes[0] = 'Sequences' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'SEQUENCE', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['Referenced by']);
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'SEQUENCE');
        end;
      end;
      if Nodes[0] = 'Functions' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'FUNCTION', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References', 'Referenced by']);
          if Nodes[2] = 'References' then GetReferences(Node, 'FUNCTION');
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'FUNCTION');
        end;
      end;
      if Nodes[0] = 'Procedures' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'PROCEDURE', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References', 'Referenced by']);
          if Nodes[2] = 'References' then GetReferences(Node, 'PROCEDURE');
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'PROCEDURE');
        end;
      end;
      if Nodes[0] = 'Packages' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'PACKAGE', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References', 'Referenced by']);
          if Nodes[2] = 'References' then GetReferences(Node, 'PACKAGE');
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'PACKAGE');
        end;
      end;
      if Nodes[0] = 'Package bodies' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'PACKAGE BODY', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References']);
          if Nodes[2] = 'References' then GetReferences(Node, 'PACKAGE BODY');
        end;
      end;
      if Nodes[0] = 'Types' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'TYPE', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References', 'Referenced by']);
          if Nodes[2] = 'References' then GetReferences(Node, 'TYPE');
          if Nodes[2] = 'Referenced by' then GetReferencedBy(Node, 'TYPE');
        end;
      end;
      if Nodes[0] = 'Type bodies' then
      begin
        if Nodes[1] = '' then GetObjects(Node, 'TYPE BODY', True);
        if Nodes[1] <> '' then
        begin
          if Nodes[2] = '' then AddFolders(Node, ['References']);
          if Nodes[2] = 'References' then GetReferences(Node, 'TYPE BODY');
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

// Query and display the properties of an object
procedure TExplorerForm.QueryProperties(Query: TOracleQuery; Obj: TExplorerObject);
var i, cw, tw: Integer;
    L: TListItem;
 procedure AddCaption(S: string);
 var C: TListColumn;
 begin
   C := ListView.Columns.Add;
   C.Caption := S;
   C.Width := PBox.Canvas.TextWidth(S+'    ');
 end;
 procedure AddItem(Index: Integer; S: string);
 begin
   if Index=0 then
   begin
     L := ListView.Items.Add;
     L.Caption := S
   end else
     L.SubItems.Add(S);
   cw := ListView.Columns[Index].Width;
   tw := PBox.Canvas.TextWidth(S+'    ');
   if tw > 300 then tw := 300;
   if tw > cw then ListView.Columns[Index].Width := tw;
 end;
begin
  PrintBtn.Enabled := True;
  PageControl.ActivePage := ListPage;
  try
    Query.SetVariable('object_type',  Obj.ObjectType);
    Query.SetVariable('object_owner', Obj.ObjectOwner);
    Query.SetVariable('object_name',  Obj.ObjectName);
    Query.Execute;
    if Query.EOF then  Exit;
  except
    on E: Exception do
    begin
//    StatusMessage(E.Message);
      Exit;
    end;
  end;
  AddCaption('Name');
  AddCaption('Value');
  for i:=0 to Query.FieldCount - 1 do
  begin
    AddItem(0, Query.FieldName(i));
    AddItem(1, Query.FieldAsString(i));
  end;
end;

// Query and display the columns of a table
procedure TExplorerForm.QueryColumns(const TableOwner, TableName: string);
var cw, tw, dl, dp, ds: Integer;
    s1, s2, tmod, towner: string;
    L: TListItem;
 procedure AddCaption(S: string);
 var C: TListColumn;
 begin
   C := ListView.Columns.Add;
   C.Caption := S;
   C.Width := PBox.Canvas.TextWidth(S+'    ');
 end;
 procedure AddItem(Index: Integer; S: string);
 begin
   if Index=0 then
   begin
     L := ListView.Items.Add;
     L.Caption := S
   end else
     L.SubItems.Add(S);
   cw := ListView.Columns[Index].Width;
   tw := PBox.Canvas.TextWidth(S+'    ');
   if tw > 300 then tw := 300;
   if tw > cw then ListView.Columns[Index].Width := tw;
 end;
begin
  PrintBtn.Enabled := True;
  PageControl.ActivePage := ListPage;
  with ColumnQuery do
  try
    SetVariable('owner', TableOwner);
    SetVariable('table_name', TableName);
    Execute;
    AddCaption('Name');
    AddCaption('Type');
    AddCaption('Nullable');
    AddCaption('Default');
    while not EOF do
    begin
      s1 := Field('data_type');
      dl := Field('data_length');
      dp := Field('data_precision');
      ds := Field('data_scale');
      if FieldIndex('data_type_owner') >= 0 then
      begin
        towner := string(Field('data_type_owner'));
        tmod   := string(Field('data_type_mod'));
      end else begin
        towner := '';
        tmod   := '';
      end;
      s2 := '';
      AddItem(0, string(Field('column_name')));
      if (dl > 0) and (towner = '') and (s1 <> 'DATE') and (s1 <> 'MLSLABEL') and
         (s1 <> 'LONG') and (s1 <> 'LONG RAW') and (s1 <> 'CLOB') and (s1 <> 'BLOB') and
         (s1 <> 'BFILE') and (s1 <> 'CFILE') then
        s2 := IntToStr(dl);
      if s1 = 'NUMBER' then
      begin
        s2 := '';
        if dp <> 0 then s2 := IntToStr(dp);
        if ds <> 0 then s2 := s2 + ',' + IntToStr(ds);
      end;
      if s2 <> '' then s1 := s1 + '(' + s2 + ')';
      if tmod <> '' then s1 := tmod + ' ' + s1;
      AddItem(1, s1);
      if Field('nullable') = 'Y' then AddItem(2, 'Y');
      AddItem(3, trim(string(Field('data_default'))));
      Next;
    end;
  except
//  on E: Exception do StatusMessage(E.Message);
  end;
end;

// Query and display source
procedure TExplorerForm.QuerySource(const ObjectType, ObjectOwner, ObjectName: string);
var s: string;
    Buf: TStringList;
begin
  PrintBtn.Enabled := True;
  PageControl.ActivePage := TextPage;
  Buf := TStringList.Create;
  if (ObjectType = 'FUNCTION') or (ObjectType = 'PROCEDURE') or
     (ObjectType = 'PACKAGE') or (ObjectType = 'PACKAGE BODY') or
     (ObjectType = 'TYPE') or (ObjectType = 'TYPE BODY') or
     (ObjectType = 'TRIGGER') then
  with ProgSourceQuery do
  try
    SetVariable('type', ObjectType);
    SetVariable('owner', ObjectOwner);
    SetVariable('name', ObjectName);
    Execute;
    while not EOF do
    begin
      s := Field('text');
//      if s[Length(s)] = #10 then SetLength(s, Length(s) - 2);
      s := TrimRight(s);  
      Buf.Add(s);
      Next;
    end;
  except
//  on E:Exception do StatusMessage(E.Message);
  end;
  if (ObjectType = 'TRIGGER') and (Buf.Count = 0) then
  with TriggerSourceQuery do
  try
    SetVariable('owner', ObjectOwner);
    SetVariable('trigger_name', ObjectName);
    Execute;
    if not EOF then
    begin
      Buf.Text := 'TRIGGER ' + Field('description');
      s := Field('when_clause');
      if s <> '' then Buf.Add('WHEN (' + s + ')');
      Buf.Add(Field('trigger_body'));
    end;
  except
//  on E:Exception do StatusMessage(E.Message);
  end;
  if (ObjectType = 'CHECK CONSTRAINT') then
  begin
    with ConstraintSourceQuery do
    try
      SetVariable('owner', ObjectOwner);
      SetVariable('constraint_name', ObjectName);
      Execute;
      if not EOF then Buf.Text := Field('search_condition');
    except
//    on E:Exception do StatusMessage(E.Message);
    end;
  end;
  if Buf.Text = '' then Buf.Text := '(Source not available)';
  RichEdit.Text := Buf.Text;
  Buf.Free;
end;

// When an object gets selected, show some usefull info
procedure TExplorerForm.ShowProperties(ExplorerObject: TExplorerObject);
begin
  with ExplorerObject do
  begin
//    caption := objecttype;
    SelectedItem := ObjectName;
    // If an OraclePackage started the explorer, enable the "OK" Button
    OkayBtn.Enabled := (ObjectType = 'PACKAGE') and (OraclePackage <> nil);
    if ObjectType = 'TABLE' then
      QueryColumns(ObjectOwner, ObjectName)
    else if ObjectType = 'VIEW' then
      QueryColumns(ObjectOwner, ObjectName)
    else if ObjectType = 'SEQUENCE' then
      QueryProperties(SequencePropertiesQuery, ExplorerObject)
    else if ObjectType = 'INDEX' then
      QueryProperties(IndexPropertiesQuery, ExplorerObject)
    else if IsColumn then
    begin
      ColumnPropertiesQuery.SetVariable('table_name', ParentObject.ObjectName);
      QueryProperties(ColumnPropertiesQuery, ExplorerObject);
    end else if HasSource then
      QuerySource(ObjectType, ObjectOwner, ObjectName)
    else if IsConstraint then
      QueryProperties(ConstraintPropertiesQuery, ExplorerObject)
    else
      QueryProperties(BasicPropertiesQuery, ExplorerObject);
  end;
end;

// Something got selected in the TreeView, refresh
procedure TExplorerForm.BrowserChange(Sender: TObject; Node: TTreeNode);
var Obj: TExplorerObject;
begin
  Screen.Cursor:=crHourglass;
  LockWindowUpdate(Integer(Handle));
  PrintBtn.Enabled := False;
  PageControl.ActivePage := EmptyPage;
  OkayBtn.Enabled := False;
  RichEdit.Clear;
  ListView.Columns.Clear;
  ListView.Items.Clear;
  if Session.Connected then
  begin
    Obj := Node.Data;
    if Obj <> nil then ShowProperties(Obj);
  end;
  LockWindowUpdate(0);
  OkayBtn.Refresh;
  Screen.Cursor:=crDefault;
end;

procedure TExplorerForm.DropIt(Sender, Source: TObject; X, Y: Integer);
begin
  Start := -1;
  Drag := False;
end;

procedure TExplorerForm.DragIt(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var Position: Integer;
begin
  Accept := (Source = SplitterPanel);
  if Accept then
  begin
    Drag := True;
    Position := ScreenToClient(TControl(Sender).ClientToScreen(Point(X, Y))).X;
    if Start < 0 then Start := Position;
    if (Start - Position) <> 0 then
    begin
      Browser.Width := Browser.Width - (Start - Position);
      Refresh;
    end;
    Start := Position;
  end;
end;

procedure TExplorerForm.OkayBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TExplorerForm.ExitBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TExplorerForm.PrintListView(const PrintTitle, PrintCaption: string);
{$IFDEF LINUX}
begin
end;
{$ELSE}
var y, r, rh, space: Integer;
    XF: Double;
 procedure PrintStat(Row: Integer);
 var x, c, f: Integer;
     Rect: TRect;
     ListItem: TListItem;
 begin
   ListItem := ListView.Items[Row];
   Rect.Top := y;
   Rect.Bottom := y + rh;
   Rect.Left := LeftMargin + space;
   Rect.Right := Round(LeftMargin + XF*ListView.Columns[0].Width) + space;
   DrawText(Printer.Canvas.Handle, PChar(ListItem.Caption), -1, Rect, dt_NoPrefix);
   x := Rect.Right;
   for c := 0 to ListItem.SubItems.Count - 1 do
   begin
     Rect.Left := x + space;
     Rect.Right := x + Round(XF*ListView.Columns[c + 1].Width) + space;
     f := dt_NoPrefix;
     if ListView.Columns[c + 1].Alignment = taRightJustify then f := f + dt_Right;
     if ListView.Columns[c + 1].Alignment = taCenter then f := f + dt_Center;
     DrawText(Printer.Canvas.Handle, PChar(ListItem.SubItems[c]), -1, Rect, f);
     x := Rect.Right;
   end;
   y := Rect.Bottom;
 end;
 procedure PrintStatHeader;
 var x, c, f: Integer;
     Rect: TRect;
 begin
   Rect.Top := y;
   Rect.Bottom := y + rh;
   Rect.Left := LeftMargin;
   x := Rect.Left;
   for c := 0 to ListView.Columns.Count - 1 do
   begin
     Rect.Left := x + space;
     Rect.Right := x + Round(XF * ListView.Columns[c].Width) + space;
     f := dt_NoPrefix;
     if ListView.Columns[c].Alignment = taRightJustify then f := f + dt_Right;
     if ListView.Columns[c].Alignment = taCenter then f := f + dt_Center;
     DrawText(Printer.Canvas.Handle, PChar(ListView.Columns[c].Caption), -1, Rect, f);
     x := Rect.Right;
   end;
   y := Rect.Bottom;
 end;
begin
  with Printer do
  begin
    Title := PrintTitle;
    BeginDoc;
    XF := GetDeviceCaps(Printer.Handle, LogPixelsX) / Screen.PixelsPerInch;
    y  := -1;
    Canvas.Font := ListView.Font;
    rh := Canvas.TextHeight('Hj');
    space := Canvas.TextWidth(' ');
    for r := 0 to ListView.Items.Count - 1 do
    begin
      if ((y < 0) or ((y + rh) > PageHeight)) then // Header
      begin
        if y < 0 then
          y := Abs(y)
        else begin
          NewPage;
          y := TopMargin;
        end;
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
        Canvas.Font.Size  := Canvas.Font.Size + 2;
        Printer.Canvas.TextOut(LeftMargin, y, ' ' + PrintCaption);
        inc(y, 2 * rh);
        Canvas.Font.Size  := Canvas.Font.Size - 2;
        Canvas.Font.Style := Canvas.Font.Style - [fsBold];
        Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        PrintStatHeader;
      end;
      Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
      PrintStat(r);
    end;
    EndDoc;
  end;
end;
{$ENDIF}

procedure TExplorerForm.PrintBtnClick(Sender: TObject);
{$IFNDEF LINUX}
var Node: TTreeNode;
    Obj: TExplorerObject;
{$ENDIF}
begin
  {$IFNDEF LINUX}
  Node := Browser.Selected;
  Obj := Node.Data;
  if PageControl.ActivePage = ListPage then PrintListView('Explorer', Obj.ObjectName);
  if PageControl.ActivePage = TextPage then RichEdit.Print('Explorer');
  {$ENDIF}
end;

function StyleToInt(Style: TFontStyles): Integer;
begin
  Result := 0;
  if fsBold in Style then Inc(Result, 1);
  if fsItalic in Style then Inc(Result, 2);
  if fsUnderline in Style then Inc(Result, 4);
  if fsStrikeOut in Style then Inc(Result, 8);
end;

function IntToStyle(i: Integer): TFontStyles;
begin
  Result := [];
  if i and 1 <> 0 then Result := Result + [fsBold];
  if i and 2 <> 0 then Result := Result + [fsItalic];
  if i and 4 <> 0 then Result := Result + [fsUnderline];
  if i and 8 <> 0 then Result := Result + [fsStrikeOut];
end;

procedure TExplorerForm.SetFonts;
begin
  if OpenRegistry('Explorer\BrowserFont') then
  begin
    Browser.Font.Name  := ReadString('Name', 'MS Sans Serif');
    Browser.Font.Color := ReadInteger('Color', clWindowText);
    Browser.Font.Size  := ReadInteger('Size', 8);
    Browser.Font.Style := IntToStyle(ReadInteger('Style', 0));
    CloseRegistry;
  end;
  if OpenRegistry('Explorer\TextFont') then
  begin
    RichEdit.Font.Name  := ReadString('Name', 'Courier New');
    RichEdit.Font.Color := ReadInteger('Color', clWindowText);
    RichEdit.Font.Size  := ReadInteger('Size', 8);
    RichEdit.Font.Style := IntToStyle(ReadInteger('Style', 0));
    CloseRegistry;
  end;
  if OpenRegistry('Explorer\ListFont') then
  begin
    ListView.Font.Name  := ReadString('Name', 'MS Sans Serif');
    ListView.Font.Color := ReadInteger('Color', clWindowText);
    ListView.Font.Size  := ReadInteger('Size', 8);
    ListView.Font.Style := IntToStyle(ReadInteger('Style', 0));
    CloseRegistry;
    PBox.Font := ListView.Font;
  end;
end;

function TExplorerForm.SelectFont(Font: TFont; Section: string): Boolean;
begin
  Result := False;
  FontDialog.Font := Font;
  if FontDialog.Execute then
  begin
    if OpenRegistry(Section) then
    begin
      WriteString('Name', FontDialog.Font.Name);
      WriteInteger('Color', FontDialog.Font.Color);
      WriteInteger('Size', FontDialog.Font.Size);
      WriteInteger('Style', StyleToInt(FontDialog.Font.Style));
      CloseRegistry;
    end;
    Result := True;
  end;
end;

procedure TExplorerForm.SetupBtnClick(Sender: TObject);
var P: TPoint;
begin
  P := ClientToScreen(Point(SetupBtn.Left, SetupBtn.Top + SetupBtn.Height));
  SetupPopup.Popup(P.X, P.Y);
end;

procedure TExplorerForm.BrowserFontClick(Sender: TObject);
begin
  if SelectFont(Browser.Font, 'Explorer\BrowserFont') then SetFonts;
end;

procedure TExplorerForm.TextFontClick(Sender: TObject);
begin
  if SelectFont(RichEdit.Font, 'Explorer\TextFont') then SetFonts;
end;

procedure TExplorerForm.ListFontClick(Sender: TObject);
begin
  if SelectFont(ListView.Font, 'Explorer\ListFont') then SetFonts;
end;

procedure TExplorerForm.PrinterSetupClick(Sender: TObject);
begin
  {$IFNDEF LINUX}
  PrinterSetupDialog.Execute;
  {$ENDIF}
end;

end.
