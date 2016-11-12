// Direct Oracle Access - QBE Navigator
// Based on Borland's DBNavigator
// Copyright 1998 - 2003 Allround Automations
// support@allround.automations.com
// http://www.allroundautomations.com

{$I Oracle.inc}
{$R OracleNavigator.res}

unit OracleNavigator;

interface

{$IFNDEF LINUX}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, WinTypes, WinProcs, ExtCtrls, Buttons, OracleData;
{$ELSE}
uses
  Variants, SysUtils, Types, Classes, Qt, QTypes, QControls, QComCtrls,
  QForms, QGraphics, QButtons, QMenus, QStdCtrls, QExtCtrls, QMask, DB,
  QDialogs, OracleData, OracleVisual;
{$ENDIF}

const
  InitRepeatPause = 400;  // pause before repeat timer (ms)
  RepeatPause     = 100;  // pause before hint window displays (ms)
  SpaceSize       = 5;    // size of space between special buttons

ResourceString
  SEnterQBE   = 'Enter Query';
  SExecuteQBE = 'Execute Query';
  SRefreshSingleRecord = 'Refresh record';

type
  TOracleNavButton = class;
  TOracleNavDataLink = class;

  TOracleNavGlyph = (ngEnabled, ngDisabled);
  TOracleNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
                nbEdit, nbPost, nbCancel, nbEnterQBE, nbExecuteQBE, nbRefresh, nbRefreshRecord);
  TOracleButtonSet = set of TOracleNavigateBtn;
  TOracleNavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  EOracleNavClick = procedure (Sender: TObject; Button: TOracleNavigateBtn) of object;

  {$IFDEF LINUX}
  TOracleNavigator = class(TCustomPanel)
  private
    FDataLink: TOracleNavDataLink;
    FVisibleButtons: TOracleButtonSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: EOracleNavClick;
    FBeforeAction: EOracleNavClick;
    FocusedButton: TOracleNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    Silent: Boolean;
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    procedure CMKeyDown(var Msg: TCMKeyDown); message CM_KEYDOWN;
    function  GetHints: TStrings;
    function  GetDataSource: TDataSource;
    procedure HintsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFlat(Value: Boolean);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TOracleButtonSet);
  protected
    Buttons: array[TOracleNavigateBtn] of TOracleNavButton;
    procedure BoundsChanged; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DataChanged;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure EditingChanged;
    procedure EnabledChanged; override;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure CalcMinSize(var W, H: Integer);
    function  GetOracleDataSet: TOracleDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TOracleNavigateBtn); virtual;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TOracleButtonSet read FVisibleButtons write SetVisible;
    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Hints: TStrings read GetHints write SetHints;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: EOracleNavClick read FBeforeAction write FBeforeAction;
    property OnClick: EOracleNavClick read FOnNavClick write FOnNavClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

  {$ELSE}

  TOracleNavigator = class (TCustomPanel)
  private
    FDataLink: TOracleNavDataLink;
    FVisibleButtons: TOracleButtonSet;
    FHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: EOracleNavClick;
    FBeforeAction: EOracleNavClick;
    FocusedButton: TOracleNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    Silent: Boolean;
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure InitButtons;
    procedure InitHints;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetVisible(Value: TOracleButtonSet);
    procedure SetSize (var W: Integer; var H: Integer);
    procedure HintsChanged(Sender: TObject);
    procedure SetHints(Value: TStrings);
    procedure SetFlat(Value: Boolean);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    Buttons: array[TOracleNavigateBtn] of TOracleNavButton;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  GetOracleDataSet: TOracleDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TOracleNavigateBtn);
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TOracleButtonSet read FVisibleButtons write SetVisible;
    property Align;
    {$IFDEF CompilerVersion4}
    property Anchors;
    property Constraints;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Ctl3D;
    property Hints: TStrings read FHints write SetHints;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: EOracleNavClick read FBeforeAction write FBeforeAction;
    property OnClick: EOracleNavClick read FOnNavClick write FOnNavClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

  {$ENDIF}

  TOracleNavButton = class(TSpeedButton)
  private
    FIndex: TOracleNavigateBtn;
    FNavStyle: TOracleNavButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    destructor Destroy; override;
    property NavStyle: TOracleNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TOracleNavigateBtn read FIndex write FIndex;
  end;

  TOracleNavDataLink = class(TDataLink)
  private
    FNavigator: TOracleNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TOracleNavigator);
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF LINUX}
    DBConsts,
  {$ELSE}
    {$IFDEF CompilerVersion6} VDBConsts, {$ELSE} DBConsts, {$ENDIF}
  {$ENDIF}
  Math, Oracle;

// TOracleNavigator

var
  BtnTypeName: array[TOracleNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT', 'LAST',
    'INSERT', 'DELETE', 'EDIT', 'POST', 'CANCEL', 'ENTERQBE', 'EXECUTEQBE', 'REFRESH', 'REFRESHRECORD');

constructor TOracleNavigator.Create(AOwner: TComponent);
var I: TOracleNavigateBtn;
    w: Integer;
begin
  inherited Create(AOwner);
  {$IFDEF LINUX}
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque, csFramed];
  {$ELSE}
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FullRepaint := False;
  {$ENDIF}
  FDataLink := TOracleNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbEnterQBE, nbExecuteQBE, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  InitButtons;
  {$IFDEF LINUX}
  InitHints;
  {$ENDIF}
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
  Silent := False;
  if Assigned(SetDefaults) then SetDefaults(Self);
  w := 1;
  for I := Low(Buttons) to High(Buttons) do
    if I in FVisibleButtons then inc(w, 24);
  Width := w;
  Height := 25;
end;

destructor TOracleNavigator.Destroy;
begin
  {$IFDEF LINUX}
  FDefHints.Free;
  {$ENDIF}
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TOracleNavigator.InitButtons;
var I: TOracleNavigateBtn;
    Btn: TOracleNavButton;
    X: Integer;
    ResName: string;
begin
  MinBtnSize := Point(20, 18);
  X := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TOracleNavButton.Create(Self);
    Btn.Flat := Flat;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds (X, 0, MinBtnSize.X, MinBtnSize.Y);
    FmtStr(ResName, 'DOANAV_%s', [BtnTypeName[I]]);
    Btn.Glyph.LoadFromResourceName(HInstance, ResName);
    Btn.NumGlyphs := 2;
    if BtnTypeName[i] = 'ENTERQBE' then
    begin
      Btn.AllowAllUp := True;
      Btn.Groupindex := 1;
    end;
    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  {$IFNDEF LINUX}
  InitHints;
  {$ENDIF}
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TOracleNavigator.InitHints;
var i: Integer;
    j: TOracleNavigateBtn;
begin
  {$IFDEF LINUX}
  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;
    for J := Low(Buttons) to High(Buttons) do FDefHints.Add(BtnTypeName[J]);
  end;
  for J := Low(Buttons) to High(Buttons) do Buttons[J].Hint := FDefHints[Ord(J)];
  {$ELSE}
  Buttons[nbFirst].Hint      := SFirstRecord;
  Buttons[nbPrior].Hint      := SPriorRecord;
  Buttons[nbNext].Hint       := SNextRecord;
  Buttons[nbLast].Hint       := SLastRecord;
  Buttons[nbInsert].Hint     := SInsertRecord;
  Buttons[nbDelete].Hint     := SDeleteRecord;
  Buttons[nbEdit].Hint       := SEditRecord;
  Buttons[nbPost].Hint       := SPostEdit;
  Buttons[nbCancel].Hint     := SCancelEdit;
  Buttons[nbEnterQBE].Hint   := SEnterQBE;
  Buttons[nbExecuteQBE].Hint := SExecuteQBE;
  Buttons[nbRefresh].Hint    := SRefreshRecord;
  Buttons[nbRefreshRecord].Hint := SRefreshSingleRecord;
  {$ENDIF}
  j := Low(Buttons);
  for i := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[i] <> '' then Buttons[j].Hint := FHints.Strings[i];
    if j = High(Buttons) then Exit;
    Inc(j);
  end;
end;

procedure TOracleNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TOracleNavigator.SetFlat(Value: Boolean);
var I: TOracleNavigateBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(Buttons) to High(Buttons) do Buttons[I].Flat := Value;
  end;
end;

procedure TOracleNavigator.SetHints(Value: TStrings);
begin
  {$IFDEF LINUX}
  if Value.Text = FDefHints.Text then FHints.Clear else FHints.Assign(Value);
  {$ELSE}
  FHints.Assign(Value);
  {$ENDIF}
end;

procedure TOracleNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TOracleNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TOracleNavigator.SetVisible(Value: TOracleButtonSet);
var I: TOracleNavigateBtn;
    W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do Buttons[I].Visible := I in FVisibleButtons;
  SetSize (W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds(Left, Top, W, H);
  Invalidate;
end;

procedure TOracleNavigator.SetSize (var W: Integer; var H: Integer);
var Count: Integer;
    {$IFNDEF LINUX}
    MinW: Integer;
    {$ENDIF}
    I: TOracleNavigateBtn;
    Space, Temp, Remain: Integer;
    X: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;
  {$IFDEF LINUX}
  CalcMinSize(W, H);
  {$ENDIF}
  Count := 0;
  for I := Low(Buttons) to High(Buttons) do if Buttons[I].Visible then Inc(Count);
  if Count = 0 then Inc(Count);
  ButtonWidth := W div Count;
  Temp := Count * ButtonWidth;
  if Align = alNone then W := Temp;
  {$IFNDEF LINUX}
  MinW := Count * MinBtnSize.X;
  if W < MinW then W := MinW;
  if H < MinBtnSize.Y then H := MinBtnSize.Y;
  {$ENDIF}
  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      Buttons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
      Inc(X, ButtonWidth + Space);
    end else
      Buttons[I].SetBounds(Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TOracleNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  {$IFDEF LINUX}
  SetSize(W, H);
  {$ELSE}
  if not HandleAllocated then SetSize (W, H);
  {$ENDIF}
  inherited SetBounds(ALeft, ATop, W, H);
end;

{$IFNDEF LINUX}
procedure TOracleNavigator.WMSize(var Message: TWMSize);
var W, H: Integer;
begin
  inherited;
  // check for minimum size
  W := Width;
  H := Height;
  SetSize (W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;
{$ENDIF}

procedure TOracleNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TOracleNavButton(Sender).Index);
end;

procedure TOracleNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var OldFocus: TOracleNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TOracleNavButton(Sender).Index;
  {$IFDEF LINUX}
  if TabStop and not Focused and CanFocus then
  {$ELSE}
  if TabStop and (GetFocus <> Handle) and CanFocus then
  {$ENDIF}
  begin
    SetFocus;
    {$IFDEF LINUX}
    if not Focused then Exit;
    {$ELSE}
    if (GetFocus <> Handle) then  Exit;
    {$ENDIF}
  end else begin
    {$IFDEF LINUX}
    if TabStop and Focused and (OldFocus <> FocusedButton) then
    {$ELSE}
    if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
    {$ENDIF}
    begin
      Buttons[OldFocus].Invalidate;
      Buttons[FocusedButton].Invalidate;
    end;
  end;
  TOracleNavButton(Sender).MouseUp(Button, Shift, X, Y);
end;

function TOracleNavigator.GetOracleDataSet: TOracleDataSet;
begin
  Result := nil;
  if (DataSource <> nil) and
     (DataSource.DataSet is TOracleDataSet) then Result := TOracleDataSet(DataSource.DataSet);
end;

procedure TOracleNavigator.BtnClick(Index: TOracleNavigateBtn);
var OracleDataSet: TOracleDataSet;
begin
  if Silent then Exit;
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    OracleDataSet := GetOracleDataSet;
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
         nbPrior: Prior;
          nbNext: Next;
         nbFirst: First;
          nbLast: Last;
        nbInsert: Insert;
          nbEdit: Edit;
        nbCancel: Cancel;
          nbPost: Post;
       nbRefresh: if (OracleDataSet <> nil) and (OracleDataSet.QBEMode) then
                    OracleDataSet.ToggleQBEValues
                  else
                    Refresh;
 nbRefreshRecord: if (OracleDataSet <> nil) then
                  begin
                    if (OracleDataSet.QBEMode) then
                      OracleDataSet.ToggleQBEValues
                    else
                      OracleDataSet.RefreshRecord;
                  end;
        nbDelete: if not FConfirmDelete or
                    (MessageDlg(SDeleteRecordQuestion, mtConfirmation,
                     mbOKCancel, 0) <> idCancel) then Delete;
      nbEnterQBE: try
                    OracleDataSet.QBEMode := not OracleDataSet.QBEMode; //Buttons[nbEnterQBE].Down;
                  finally
                    {$IFNDEF LINUX}
                    Buttons[nbEnterQBE].Down := OracleDataSet.QBEMode;
                    {$ENDIF}
                  end;
    nbExecuteQBE: OracleDataSet.ExecuteQBE;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

{$IFNDEF LINUX}
procedure TOracleNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TOracleNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;
{$ENDIF}

procedure TOracleNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var NewFocus: TOracleNavigateBtn;
    OldFocus: TOracleNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus < High(Buttons) then
            NewFocus := Succ(NewFocus);
        until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
          Buttons[FocusedButton].Click;
      end;
  end;
end;

{$IFNDEF LINUX}
procedure TOracleNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;
{$ENDIF}

procedure TOracleNavigator.DataChanged;
var UpEnable, DnEnable: Boolean;
    OracleDataSet: TOracleDataSet;
begin
  OracleDataSet := GetOracleDataSet;
  if (OracleDataSet <> nil) and OracleDataSet.QBEMode then
  begin
    UpEnable := False;
    DnEnable := False;
  end else begin
    UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
    DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  end;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  if OracleDataSet <> nil then
    EditingChanged
  else begin
    Buttons[nbEnterQBE].Enabled := False;
    Buttons[nbExecuteQBE].Enabled := False;
    Buttons[nbRefreshRecord].Enabled := False;
  end;
end;

procedure TOracleNavigator.EditingChanged;
var CanModify: Boolean;
    OracleDataSet: TOracleDataSet;
begin
  Silent := True;
  OracleDataSet := GetOracleDataSet;
  Buttons[nbEnterQBE].Enabled := (OracleDataSet <> nil) and
                                   OracleDataSet.Active and
                                   OracleDataSet.CanQBE(False);
  if (OracleDataSet <> nil) and OracleDataSet.QBEMode then
  begin
    Buttons[nbInsert].Enabled := False;
    Buttons[nbEdit].Enabled := False;
    Buttons[nbPost].Enabled := False;
    Buttons[nbCancel].Enabled := False;
    Buttons[nbRefresh].Enabled := True;
    Buttons[nbRefreshRecord].Enabled := False;
    {$IFDEF LINUX}
    Buttons[nbEnterQBE].FState := bsDown;
    Buttons[nbEnterQBE].Invalidate;
    {$ELSE}
    Buttons[nbEnterQBE].Down := True;
    {$ENDIF}
  end else begin
    CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
    Buttons[nbInsert].Enabled := CanModify;
    Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
    Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
    Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
    Buttons[nbRefresh].Enabled := not OracleDataSet.Detached;
    Buttons[nbRefreshRecord].Enabled := (OracleDataSet <> nil) and
                                        (not OracleDataSet.Detached) and
                                        (OracleDataSet.State <> dsInsert);
    {$IFDEF LINUX}
    Buttons[nbEnterQBE].FState := bsUp;
    Buttons[nbEnterQBE].Invalidate;
    {$ELSE}
    Buttons[nbEnterQBE].Down := False;
    {$ENDIF}
  end;
  Buttons[nbExecuteQBE].Enabled := (OracleDataSet <> nil) and
                                    OracleDataSet.QBEMode and
                                    OracleDataSet.Active;
  Silent := False;
end;

procedure TOracleNavigator.ActiveChanged;
var I: TOracleNavigateBtn;
begin
  if not Assigned(FDataLink) then Exit;
  if not (Enabled and FDataLink.Active) then
    for I := Low(Buttons) to High(Buttons) do Buttons[I].Enabled := False
  else begin
    DataChanged;
    EditingChanged;
  end;
end;

{$IFNDEF LINUX}
procedure TOracleNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then ActiveChanged;
end;
{$ENDIF}

procedure TOracleNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TOracleNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TOracleNavigator.Loaded;
var W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds(Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;

// TOracleNavButton

destructor TOracleNavButton.Destroy;
begin
  if FRepeatTimer <> nil then FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TOracleNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TOracleNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then FRepeatTimer.Enabled  := False;
end;

procedure TOracleNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TOracleNavButton.Paint;
var R: TRect;
begin
  inherited Paint;
  {$IFDEF LINUX}
  if Parent.TabStop and Parent.Focused and
    (Findex = TOracleNavigator(Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then OffsetRect(R, 1, 1);
    Canvas.Brush.Style := bsSolid;
    Font.Color := clBtnShadow;
    Canvas.DrawFocusRect(R);
  end;
  {$ELSE}
  if (GetFocus = Parent.Handle) and
     (FIndex = TOracleNavigator (Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
  {$ENDIF}
end;

// TOracleNavDataLink

constructor TOracleNavDataLink.Create(ANav: TOracleNavigator);
begin
  inherited Create;
  FNavigator := ANav;
end;

destructor TOracleNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TOracleNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TOracleNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TOracleNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

{$IFDEF LINUX}

procedure TOracleNavigator.CMKeyDown(var Msg: TCMKeyDown);
begin
  with Msg do
  begin
    case Key of
      Key_Up, Key_Down, Key_Left, Key_Right: Exit;
    else
      inherited;
    end;
  end;
end;

function TOracleNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints
  else
    Result := FHints;
end;

procedure TOracleNavigator.BoundsChanged;
var W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TOracleNavigator.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  CalcMinSize(AWidth, AHeight);
end;

procedure TOracleNavigator.DoEnter;
begin
  inherited;
  Buttons[FocusedButton].Invalidate;
end;

procedure TOracleNavigator.DoExit;
begin
  inherited;
  Buttons[FocusedButton].Invalidate;
end;

procedure TOracleNavigator.EnabledChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then ActiveChanged;
end;

procedure TOracleNavigator.CalcMinSize(var W, H: Integer);
var Count: Integer;
    I: TOracleNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;
  Count := 0;
  for I := Low(Buttons) to High(Buttons) do if Buttons[I].Visible then Inc(Count);
  if Count = 0 then Inc(Count);
  W := Max(W, Count * MinBtnSize.X);
  H := Max(H, MinBtnSize.Y);
  if Align = alNone then W := (W div Count) * Count;
end;

{$ENDIF}

end.
