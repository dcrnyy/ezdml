// Direct Oracle Access - Property Defaults
// Copyright 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleDefaults;

interface

{$IFNDEF LINUX}
uses
  {$IFDEF CompilerVersion6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ComCtrls, ExtCtrls, Buttons, TypInfo, Menus,
  Oracle,
  {$IFNDEF NODATASET}
  Oracledata, OracleNavigator,
  {$ENDIF}
   OraclePreferences, OracleSQLEdit, OracleVisual;
{$ELSE}
uses
  DesignIntf, DesignEditors, TypInfo,
  SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, QButtons, QImgList, QMenus, QTypes,
  Oracle, OracleData, OracleNavigator, OraclePreferences, OracleSQLEdit, OracleVisual;
{$ENDIF}

type
  TOracleDefaultsForm = class(TForm)
    MainPanel: TPanel;
    ButtonPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    ScrollBox: TScrollBox;
    TopPanel: TPanel;
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    Button10: TSpeedButton;
    Button11: TSpeedButton;
    InfoPanel: TPanel;
    HiddenEdit: TEdit;
    PropEditButton: TSpeedButton;
    ChangedLabel: TLabel;
    HelpBtn: TButton;
    DefaultBtn: TButton;
    EnabledCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComponentButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EditFocusChange(Sender: TObject);
    procedure PropEditButtonClick(Sender: TObject);
    procedure PopupClick(Sender: TObject);
    procedure EditDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure HelpBtnClick(Sender: TObject);
    procedure ButtonPanelResize(Sender: TObject);
    procedure DefaultBtnClick(Sender: TObject);
  private
    procedure StoreDefaults;
    procedure ClearProp;
    procedure BuildPropertyEdits;
    procedure EnablePropButton(Edit: Tedit);
    procedure GetColorProc(const S: string);
  public
    Changed: Boolean;
    procedure BuildPropertyForm(Component: TClass);
    function  SetCurrentClass(Name: string): Boolean;
    function  CurrentClass: TClass;
  end;

function ExecuteDefaultsEditor(Component: TComponent): Boolean;
procedure SetComponentDefaults(Component: TComponent);

implementation

{$R *.dfm}

type
  TDefaultItem = class(TCollectionItem)
  public
    // Property data
    ppi: PPropInfo;
    Instance: TObject;
    PropName: string;
    PropType: string;
    Options: string;
    OptionValues: string;
    Value: string;
    Level: Integer;
    FullName: string;
    // Editor data
    SubItem: Boolean;
    Panel: TPanel;
    Desc: TLabel;
    Edit: TEdit;
    destructor Destroy; override;
  end;

const
  Defaults: TCollection = nil;
  DefaultsEnabled: Boolean = True;
  ColorList: string = '';

const
  IgnoreState = [csAncestor {$IFDEF CompilerVersion5} ,csFreeNotification, csDesignInstance {$ENDIF}];

function ExecuteDefaultsEditor(Component: TComponent): Boolean;
begin
  with TOracleDefaultsForm.Create(Application.MainForm) do
  begin
    if Component = nil then
      SetCurrentClass('TOracleSession')
    else
      SetCurrentClass(Component.ClassName);
    ComponentButtonClick(nil);
    Result := ShowModal = mrOK;
    Free;
  end;
end;

function HexValue(S: string; var Value: Longint): Boolean;
var Code: Integer;
begin
  Val(S, Value, Code);
  Result := Code = 0;
end;

//

procedure SortDefaults;
var i: Integer;
    Done: Boolean;
 function Compare(Item1, Item2: TCollectionItem): Integer;
 begin
   Result := CompareStr(TDefaultItem(Item2).FullName, TDefaultItem(Item1).FullName);
 end;
begin
  if Defaults.Count < 2 then Exit;
  repeat
    Done := True;
    for i := 0 to Defaults.count - 2 do
    begin
      if Compare(Defaults.Items[i], Defaults.Items[i + 1]) < 0 then
      begin
        Defaults.Items[i].Index := Defaults.Items[i + 1].Index;
        Done := False;
      end;
    end;
  until Done;
end;

function FindDefaultItem(Reference: TComponent): TDefaultItem;
var i : Integer;
begin
  Result := nil;
  if (Reference = nil) or (Defaults = nil) then Exit;
  for i := 0 to Defaults.Count - 1 do with TDefaultItem(Defaults.Items[i]) do
  begin
    if (Reference = Panel) or
       (Reference = Desc) or
       (Reference = Edit) then
    begin
      Result := TDefaultItem(Defaults.Items[i]);
      Break;
    end;
  end;
end;

// Get RTTI information for ordinal types
function GetPropertyInfo(pti: PTypeInfo): string;
var ptd: PTypeData;
 procedure ListEnum(pti: PTypeInfo);
 var i: Integer;
 begin
   with GetTypeData(pti)^ do
   begin
     for i := MinValue to MaxValue do
     begin
       if Result <> '' then Result := Result + ',';
       Result := Result + GetEnumName (pti, i);
     end;
   end;
 end;
begin
  Result := '';
  if not (pti^.Kind in [tkClass, tkLString, tkInteger, tkChar, tkEnumeration, tkSet, tkWChar]) then Exit;
  // get a pointer to the TTypeData structure
  ptd := GetTypeData(pti);
  // access the TTypeInfo structure
  if pti^.Kind = tkEnumeration then ListEnum(pti);
  // show RRTI info about set base type
  if pti^.Kind = tkSet then Result := GetPropertyInfo(ptd^.CompType^);
end;

// show RTTI information for class type
procedure GetClassInfo(Instance: TObject; pti: PTypeInfo; Level: Integer; Sort: Boolean);
var ptd: PTypeData;
    ppi: PPropInfo;
    pProps: PPropList;
    nProps, i, c: Integer;
    S, PropType, Options, ClassName: string;
    L: TstringList;
    SubClass: TObject;
 procedure AddProp(p: PPropInfo; PropName, PropType, Options: string);
 var D: TDefaultItem;
     i, x: Integer;
     S: string;
 begin
   D := TDefaultItem.Create(Defaults);
   D.Instance := Instance;
   D.ppi := p;
   D.PropName := PropName;
   D.PropType := PropType;
   D.Options := Options;
   D.Level := Level;
   if p = nil then inc(D.Level);
   D.FullName := PropName;
   x := D.Level - 1;
   for i := Defaults.Count - 1 downto 0 do
   begin
     if TDefaultItem(Defaults.Items[i]).Level = x then
     begin
       D.FullName :=  TDefaultItem(Defaults.Items[i]).Propname + '.' + D.FullName;
       dec(x);
       if x < 0 then Break;
     end;
   end;
   S := ReadString(D.FullName, #0);
   if S <> #0 then
   begin
     D.Value := S;
   end else begin
     D.Value := '';
   end;
 end;
 function HideProp(Name: string): Boolean;
 begin
   Result := False;
   if Level > 0 then Exit;
   Name := UpperCase(Name);
   HideProp := (Name = 'NAME') or (Name = 'LEFT') or (Name = 'TOP') or
               (Name = 'WIDTH') or (Name = 'HEIGHT');
 end;
begin
  // protect against misuse
  if pti.Kind <> tkClass then Exit;
  if Level = 0 then OpenRegistry('Defaults\' + pti.Name);
  // get a pointer to the TTypeData structure
  ptd := GetTypeData(pti);
  // add the list of properties (if any)
  nProps := ptd.PropCount;
  if nProps > 0 then
  begin
    // allocate the required memory
    GetMem(pProps, sizeof (PPropInfo) * nProps);
    // protect the memory allocation
    try
      // fill the TPropList structure
      // pointed to by pProps
      GetPropInfos(pti, pProps);
      // get each property
      for i := 0 to nProps - 1 do
      begin
        PropType := '*';
        ppi := pProps[i];
        Options := '';
        if (ppi.PropType^.Kind <> tkMethod) and (not HideProp(ppi.Name)) then
        begin
          case ppi.Proptype^.Kind of
            tkLString: PropType := 'S';
            tkInteger: PropType := 'I';
        tkEnumeration: begin
                         PropType := 'L';
                         Options := GetPropertyInfo(ppi.Proptype^);
                       end;
                tkSet: begin
                         PropType := '';
                         L := TStringList.Create;
                         AddProp(ppi, ppi.Name, 'X', '');
                         L.CommaText := GetPropertyInfo(ppi.Proptype^);
                         for c := 0 to L.Count - 1 do AddProp(nil, L[c], 'L', 'True,False');
                         L.Free;
                       end;
              tkClass: begin
                         PropType := '';
                         ClassName := UpperCase(ppi.PropType^.Name);
                         if (ClassName = 'TORACLESESSION') or
                            (ClassName = 'TORACLESESSIONPOOL') or
                            (ClassName = 'TORACLEDATASET') or
                            (ClassName = 'TDATASOURCE') or
                            (ClassName = 'TDATASET') then
                         begin
                           PropType := 'L';
                           Options := 'Unassigned,Autodetect';
                         end else begin
                           SubClass := nil;
                           if (Instance = nil) or (Instance is TOracleSession) then
                           begin
                             if ClassName = 'TSESSIONPREFERENCES' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleSession(Instance).Preferences;
                             end;
                           end;
                           {$IFNDEF NODATASET}
                           if (Instance = nil) or (Instance is TOracleDataSet) then
                           begin
                             if ClassName = 'TSEQUENCEFIELD' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleDataSet(Instance).SequenceField;
                             end;
                             if ClassName = 'TQBEDEFINITION' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleDataSet(Instance).QBEDefinition;
                             end;
                             if ClassName = 'TORACLEDICTIONARY' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleDataSet(Instance).OracleDictionary;
                             end;
                           end;
                           {$ENDIF}
                           if (Instance = nil) or (Instance is TOracleQueue) then
                           begin
                             if ClassName = 'TAQDEQUEUEOPTIONS' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleQueue(Instance).DequeueOptions;
                             end;
                             if ClassName = 'TAQENQUEUEOPTIONS' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleQueue(Instance).EnqueueOptions;
                             end;
                             if ClassName = 'TAQMESSAGEPROPERTIES' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TOracleQueue(Instance).MessageProperties;
                             end;
                           end;
                           if (Instance = nil) or (Instance is TAQMessageProperties) then
                           begin
                             if ClassName = 'TAQAGENT' then
                             begin
                               if Instance = nil then
                                 SubClass := TObject(1)
                               else
                                 SubClass := TAQMessageProperties(Instance).SenderId;
                             end;
                           end;
                           if SubClass <> nil then
                           begin
                             if SubClass = TObject(1) then SubClass := nil;
                             AddProp(ppi, ppi.Name, 'C', '');
                             GetClassInfo(SubClass, ppi.PropType^, Level + 1, Sort);
                           end;
                           if (ClassName = 'TSTRINGS') then PropType := 'M';
                         end;
                       end;
          end;
          GetPropertyInfo(ppi.Proptype^);
          if ppi.PropType^.Name = 'TCursor' then
          begin
            PropType := 'L';
            Options := '';
            for c := 0 downto -100 do
            begin
              if CursorToIdent(c, S) then
              begin
                if Options <> '' then Options := Options + ',';
                Options := Options + S;
              end;
            end;
          end;
          if ppi.PropType^.Name = 'TColor' then
          begin
            PropType := 'L';
            Options := ColorList;
          end;
          if PropType <> '' then AddProp(ppi, ppi.Name, PropType, Options);
        end;
      end;
    finally
      // free the allocated memory
      FreeMem(pProps, sizeof(PPropInfo) * nProps);
    end;
  end;
  if Level = 0 then
  begin
    if Sort then SortDefaults;
    CloseRegistry;
  end;
end;

//

destructor TDefaultItem.Destroy;
begin
  if Desc <> nil then Desc.Free;
  if Panel <> nil then Panel.Free;
  if Edit <> nil then Edit.Free;
  inherited;
end;

//

procedure TOracleDefaultsForm.ClearProp;
begin
  if Defaults <> nil then Defaults.Clear;
  PropEditButton.Tag := 0;
end;

procedure TOracleDefaultsForm.StoreDefaults;
var i: Integer;
    P: TDefaultItem;
begin
  // Save the value in the current active edit
  P := FindDefaultItem(Screen.ActiveControl);
  if (P <> nil) and (P.Edit <> nil) then P.Value := P.Edit.Text;
  // Store values
  OpenRegistry('Defaults\' + InfoPanel.Caption);
  ClearSection;
  for i := 0 to Defaults.Count - 1 do with TDefaultItem(Defaults.Items[i]) do
  begin
    if Value <> '' then WriteString(FullName, Value);
  end;
  CloseRegistry;
  Changed := False;
  ChangedLabel.Caption := '';
end;

function GetDescription(Desc, Value: string): string;
begin
  if Value = '' then Result := '' else Result := Desc;
end;

procedure TOracleDefaultsForm.BuildPropertyEdits;
var W, H, i: Integer;
begin
  H := HiddenEdit.Height;
  W := H * 4;

  ScrollBox.OnResize := nil;
  for i := 0 to Defaults.Count - 1 do
  begin

    with TDefaultItem(Defaults.Items[i]) do
    begin
      Edit := nil;
      Panel := nil;
      Desc := nil;
      SubItem := (ppi = nil);
      Panel := TPanel.Create(nil);
      with Panel do
      begin
        Left := 0;
        Width := W;
        Top := i * H;
        Height := H;
        Parent := ScrollBox;
      end;

      Desc := TLabel.Create(nil);
      with Desc do
      begin
        Left := 4 + (Level * Height);
        Caption := PropName;
        Top := (H - Height) div 2;
        Parent := Panel;
        {$IFDEF LINUX}
        Width := 150;
        {$ELSE}
        Width := Desc.Canvas.TextWidth(Caption) + 2;
        {$ENDIF}
      end;
      if Desc.Left + Desc.Width > W then W := Desc.Left + Desc.Width + 4;
      if (PropType <> '*') and (PropType <> 'X')  and (PropType <> 'C') then
      begin
        // Default edit
        Edit := TEdit.Create(nil);
        with Edit do
        begin
          Color := clBtnFace;
          Left := Panel.Width;
          Top := i * H;
          Parent := ScrollBox;
          if PropType = 'B' then Options := 'True,False';
          if PropType <> 'M' then
            Text := Value
          else begin
            Text := GetDescription('(TStrings)', Value);
            ReadOnly := True;
          end;
        end;
      end;
      if Edit <> nil then
      begin
        Edit.OnEnter := EditFocusChange;
        Edit.OnExit := EditFocusChange;
        Edit.OnDblClick := EditDblClick;
        Edit.OnKeyPress := EditKeyPress;
      end;
    end;

  end;

  for i := 0 to Defaults.Count - 1 do with TDefaultItem(Defaults.Items[i]) do
  begin
    if Panel <> nil then Panel.Width := W;
    if Edit <> nil then Edit.Left := W
  end;

  ScrollBox.OnResize := ScrollBoxResize;
end;

procedure TOracleDefaultsForm.BuildPropertyForm(Component: TClass);
var pti: PTypeInfo;
begin
  EnablePropButton(nil);
  InfoPanel.Caption := Component.ClassName;
  pti := PTypeInfo(PTypeInfo(Component.ClassInfo));
  GetClassInfo(nil, pti, 0, True);
  BuildPropertyEdits;
  Changed := False;
  ChangedLabel.Caption := '';
end;

procedure TOracleDefaultsForm.GetColorProc(const S: string);
begin
  if ColorList <> '' then ColorList := ColorList + ',';
  ColorList := ColorList + S;
end;

procedure TOracleDefaultsForm.FormCreate(Sender: TObject);
var ButtonLeft: Integer;
 procedure SetGlyph(Button: TSpeedButton);
 begin
   Button.Glyph.LoadFromResourceName(HInstance, 'T' + UpperCase(Button.Hint));
   Button.Top := 2;
   Button.Left := ButtonLeft;
   Button.Width := 28;
   Button.Height := 28;
   inc(ButtonLeft, 28);
   {$IFDEF NODATASET}
   if UpperCase(Button.Hint) = 'ORACLEDATASET' then Button.Enabled := False;
   if UpperCase(Button.Hint) = 'ORACLENAVIGATOR' then Button.Enabled := False;
   {$ENDIF}
 end;
begin
  ScrollBox.OnResize := nil;
  if OpenRegistry('Defaults Editor') then
  begin
    Left := ReadInteger('Left', Left);
    Top := ReadInteger('Top', Top);
    Width := ReadInteger('Width', Width);
    Height := ReadInteger('Height', Height);
    WindowState := TWindowState(ReadInteger('State', Ord(WindowState)));
    DefaultsEnabled := ReadBool('Enabled', DefaultsEnabled);
    CloseRegistry;
  end;
  EnabledCheckBox.Checked := DefaultsEnabled;
  TopPanel.Height := 32; // 24 pixels for the glyph and some space
  ButtonLeft := 4;
  SetGlyph(Button1);
  SetGlyph(Button2);
  SetGlyph(Button3);
  SetGlyph(Button4);
  SetGlyph(Button5);
  SetGlyph(Button6);
  SetGlyph(Button7);
  SetGlyph(Button8);
  SetGlyph(Button9);
  SetGlyph(Button10);
  SetGlyph(Button11);
  Defaults := TCollection.Create(TDefaultItem);
  ClearProp;
  ScrollBox.OnResize := ScrollBoxResize;
  ColorList := '';
  GetColorValues(GetColorProc);
  {$IFDEF CompilerVersion4}
  Constraints.MinWidth := 26 + (OKButton.Width + 8) * 4;
  {$ENDIF}
  {$IFDEF LINUX}
  DefaultBtn.Visible := False;
  {$ENDIF}
end;

procedure TOracleDefaultsForm.FormDestroy(Sender: TObject);
begin
  if OpenRegistry('Defaults Editor') then
  begin
    if WindowState = wsNormal then
    begin
      WriteInteger('Left', Left);
      WriteInteger('Top', Top);
      WriteInteger('Width', Width);
      WriteInteger('Height', Height);
    end;
    WriteInteger('State', Ord(WindowState));
    CloseRegistry;
  end;
  ScrollBox.OnResize := nil;
  ClearProp;
  Defaults.Free;
  Defaults := nil;
end;

procedure TOracleDefaultsForm.ScrollBoxResize(Sender: TObject);
var W, i: Integer;
    X: TEdit;
begin
  for i := 0 to Defaults.Count - 1 do with TDefaultItem(Defaults.Items[i]) do
  begin
    if Edit = nil then
      Panel.Width := ScrollBox.ClientWidth - 1
    else begin
      W := ScrollBox.ClientWidth - Panel.Width;
      if PropEditButton.Visible then
      begin
        X := TEdit(PropEditButton.Tag);
        if X = Edit then
        begin
          W := W - PropEditButton.Width;
          PropEditButton.Left := Edit.Left + W;
        end;
      end;
      Edit.Width := W;
    end;
  end;
end;

// Some assembler (copied from TypeInfo.pas) to get and set properties
procedure SetStringsProp(Instance: TObject; PropInfo: PPropInfo; Value: TStrings); assembler;
asm
  { ->    EAX Pointer to instance         }
  {       EDX Pointer to property info    }
  {       ECX Pointer to string value     }
  PUSH    ESI
  PUSH    EDI
  MOV     ESI,EDX
  MOV     EDX,[ESI].TPropInfo.Index
  CMP     EDX,$80000000
  JNE     @@hasIndex
  MOV     EDX,ECX
@@hasIndex:
  MOV     EDI,[ESI].TPropInfo.SetProc
  CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
  JB      @@isStaticMethod
@@isVirtualMethod:
  MOVSX   EDI,DI
  ADD     EDI,[EAX]
  CALL    DWORD PTR [EDI]
  JMP     @@exit
@@isStaticMethod:
  CALL    EDI
  JMP     @@exit
@@exit:
  POP     EDI
  POP     ESI
end;

procedure SetSetProp(Instance: TObject; PropInfo: PPropInfo; Value: integer); assembler;
asm
  { ->    EAX Pointer to instance         }
  {       EDX Pointer to property info    }
  {       ECX Pointer to string value     }
  PUSH    EBX
  PUSH    ESI
  PUSH    EDI
  MOV     EDI,EDX
  MOV     ESI,[EDI].TPropInfo.PropType
  MOV     ESI,[ESI]
  MOV     BL,otSLong
  CMP     [ESI].TTypeInfo.Kind,tkClass
  JE      @@isClass
  XOR     EBX,EBX
  MOV     BL,[ESI].TTypeInfo.Name.Byte[0]
  MOV     BL,[ESI].TTypeInfo.Name[EBX+1].TTypeData.OrdType
@@isClass:
  MOV     EDX,[EDI].TPropInfo.Index
  CMP     EDX,$80000000
  JNE     @@hasIndex
  MOV     EDX,ECX
@@hasIndex:
  MOV     ESI,[EDI].TPropInfo.SetProc
  CMP     [EDI].TPropInfo.SetProc.Byte[3],$FE
  JA      @@isField
  JB      @@isStaticMethod
  MOVSX   ESI,SI
  ADD     ESI,[EAX]
  CALL    dword ptr [ESI]
  JMP     @@exit
@@isStaticMethod:
  CALL    ESI
  JMP     @@exit
@@isField:
  AND     ESI,$00FFFFFF
  ADD     EAX,ESI
  MOV     [EAX],CL
  CMP     BL,otSWord
  JB      @@exit
  MOV     [EAX],CX
  CMP     BL,otSLong
  JB      @@exit
  MOV     [EAX],ECX
@@exit:
  POP     EDI
  POP     ESI
  POP     EBX
end;

function GetSetProp(Instance: TObject; PropInfo: PPropInfo): Integer; assembler;
asm
  { ->    EAX Pointer to instance         }
  {       EDX Pointer to property info    }
  { <-    EAX Longint result              }
  PUSH    EBX
  PUSH    EDI
  MOV     EDI,[EDX].TPropInfo.PropType
  MOV     EDI,[EDI]
  MOV     BL,otSLong
  CMP     [EDI].TTypeInfo.Kind,tkClass
  JE      @@isClass
  XOR     ECX,ECX
  MOV     CL,[EDI].TTypeInfo.Name.Byte[0]
  MOV     BL,[EDI].TTypeInfo.Name[ECX+1].TTypeData.OrdType
@@isClass:
  MOV     ECX,[EDX].TPropInfo.GetProc
  CMP     [EDX].TPropInfo.GetProc.Byte[3],$FE
  MOV     EDX,[EDX].TPropInfo.Index
  JB      @@isStaticMethod
  JA      @@isField
  MOVSX   ECX,CX
  ADD     ECX,[EAX]
  CALL    dword ptr [ECX]
  JMP     @@final
@@isStaticMethod:
  CALL    ECX
  JMP     @@final
@@isField:
  AND     ECX,$00FFFFFF
  ADD     ECX,EAX
  MOV     AL,[ECX]
  CMP     BL,otSWord
  JB      @@final
  MOV     AX,[ECX]
  CMP     BL,otSLong
  JB      @@final
  MOV     EAX,[ECX]
@@final:
  CMP     BL,otSLong
  JAE     @@exit
  CMP     BL,otSWord
  JAE     @@word
  CMP     BL,otSByte
  MOVSX   EAX,AL
  JE      @@exit
  AND     EAX,$FF
  JMP     @@exit
@@word:
  MOVSX   EAX,AX
  JE      @@exit
  AND     EAX,$FFFF
@@exit:
  POP     EDI
  POP     EBX
end;

procedure SetComponentDefaults(Component: TComponent);
var pti: PTypeInfo;
    pi: PPropInfo;
    i, x, SetBit: Integer;
    L: TStringList;
    TheClass: string;
 function ComponentName(Name: string): string;
 begin
   Result := Name + IntToStr(Random(255));
 end;
 function FindComponent(ComponentClass: string): Integer;
 var i, x, y, d, BestD: Integer;
     P: TPoint;
 begin
   Result := 0;
   if Component.Owner = nil then Exit;
   {$IFNDEF NODATASET}
   if Component is TOracleNavigator then
   begin
     x := TOracleNavigator(Component).Left;
     y := TOracleNavigator(Component).Top;
   end else
   {$ENDIF}
   begin
     x := LongRec(Component.DesignInfo).Lo;
     y := LongRec(Component.DesignInfo).Hi;
   end;
   if (x + y = 0) and (Component.Owner is TControl) then
   begin
     GetCursorPos(P);
     P := TControl(Component.Owner).ScreenToClient(P);
     x := P.x;
     y := P.y;
   end;
   BestD := 10000;
   for i := 0 to Component.Owner.ComponentCount - 1 do
   begin
     if Uppercase(Component.Owner.Components[i].ClassName) = ComponentClass then
     begin
       d := Round(Sqrt(Sqr(x - LongRec(Component.Owner.Components[i].DesignInfo).Lo) +
                       Sqr(y - LongRec(Component.Owner.Components[i].DesignInfo).Hi)));
       if d < BestD then
       begin
         Result := Integer(Component.Owner.Components[i]);
         BestD := d;
       end;
     end;
   end;
 end;
begin
  if (Component.Owner <> nil) and
     (Component.Owner.ComponentState - IgnoreState <> [csDesigning]) then Exit;
  if (Component.ComponentState - IgnoreState <> [csDesigning]) then Exit;
  if OpenRegistry('Defaults Editor') then
  begin
    DefaultsEnabled := ReadBool('Enabled', DefaultsEnabled);
    CloseRegistry;
  end;
  if not DefaultsEnabled then Exit;
  try
    Defaults := TCollection.Create(TDefaultItem);
    pti := PTypeInfo(Component.ClassInfo);
    GetClassInfo(Component, pti, 0, False);
    SetBit := 1;
    pi := nil;
    for i := 0 to Defaults.Count - 1 do with TDefaultItem(Defaults.Items[i]) do
    begin
      try
        if ppi <> nil then
        begin
          pi := ppi;
          SetBit := 1;
        end;
        if Value <> '' then
        begin
          if ppi = nil then TheClass := '' else TheClass := UpperCase(ppi.PropType^.Name);
          if PropType <> 'L' then
          begin
            if TheClass = 'INTEGER' then SetOrdProp(Instance, ppi, StrToInt(Value));
            if TheClass = 'TCOMPONENTNAME' then SetStrProp(Instance, ppi, ComponentName(Value));
            if TheClass = 'STRING' then SetStrProp(Instance, ppi, Value);
            if TheClass = 'TSTRINGS' then
            begin
              L := TStringList.Create;
              L.Text := Value;
              SetStringsProp(Instance, ppi, L);
              L.Free;
            end;
          end else begin
            if (TheClass = 'TCURSOR') or (TheClass = 'TCOLOR') then
            begin
              if IdentToCursor(Value, x) then SetOrdProp(Instance, ppi, x);
              if HexValue(Value, x) then SetOrdProp(Instance, ppi, x);
            end else begin
              if ppi <> nil then
              begin
                if (TheClass = 'TORACLESESSION') or
                   (TheClass = 'TORACLESESSIONPOOL') or
                   (TheClass = 'TORACLEDATASET') or
                   (TheClass = 'TDATASOURCE') or
                   (TheClass = 'TDATASET') then
                begin
                  if UpperCase(Value) = 'AUTODETECT' then
                  begin
                    x := FindComponent(TheClass);
                    if x > 0 then SetOrdProp(Instance, ppi, x);
                  end;
                end;
                // Enumeration
                if ppi.PropType^.Kind = tkEnumeration then
                begin
                  L := TStringList.Create;
                  L.CommaText := Uppercase(Options);
                  x := L.IndexOf(Uppercase(Value));
                  if x >= 0 then SetOrdProp(Instance, ppi, x);
                  L.Free;
                end;
              end else begin
                // Set
                x := GetSetProp(Instance, pi);
                if UpperCase(Value) = 'TRUE' then x := x or SetBit;
                if UpperCase(Value) = 'FALSE' then x := x and (not SetBit);
                SetSetprop(Instance, pi, x);
              end;
            end;
          end;
        end;
        if ppi = nil then SetBit := SetBit * 2
      except
        // Don't bother users with errors
      end;
    end;
    Defaults.Free;
    Defaults := nil;
  except
  end;
end;

function TOracleDefaultsForm.CurrentClass: TClass;
begin
  Result := TOracleSession;
  if Button2.Down then Result := TOracleSessionPool;
  if Button3.Down then Result := TOracleLogon;
  if Button4.Down then Result := TOracleQuery;
  {$IFNDEF NODATASET}
  if Button5.Down then Result := TOracleDataSet;
  {$ENDIF}
  if Button6.Down then Result := TOraclePackage;
  if Button7.Down then Result := TOracleEvent;
  if Button8.Down then Result := TOracleQueue;
  if Button9.Down then Result := TOracleScript;
  if Button10.Down then Result := TOracleDirectPathLoader;
  {$IFNDEF NODATASET}
  if Button11.Down then Result := TOracleNavigator;
  {$ENDIF}
end;

function TOracleDefaultsForm.SetCurrentClass(Name: string): Boolean;
var i: Integer;
begin
  Result := False;
  Name := UpperCase(Name);
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TSpeedButton then
    begin
      if UpperCase('T' + TSpeedButton(Components[i]).Hint) = Name then
      begin
        TSpeedButton(Components[i]).Down := True;
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TOracleDefaultsForm.ComponentButtonClick(Sender: TObject);
begin
  if Sender <> nil then StoreDefaults;
  {$IFDEF LINUX}
  ScrollBox.Visible := False;
  if Sender <> nil then TSpeedButton(Sender).Down := True;
  {$ELSE}
  LockWindowUpdate(Integer(Handle));
  {$ENDIF}
  ClearProp;
  BuildPropertyForm(CurrentClass);
  if Assigned(ScrollBox.OnResize) then ScrollBoxResize(nil);
  {$IFDEF LINUX}
  ScrollBox.Visible := True;
  {$ELSE}
  LockWindowUpdate(0);
  {$ENDIF}
end;

procedure TOracleDefaultsForm.OKButtonClick(Sender: TObject);
begin
  DefaultsEnabled := EnabledCheckBox.Checked;
  if OpenRegistry('Defaults Editor') then
  begin
    WriteBool('Enabled', DefaultsEnabled);
    CloseRegistry;
  end;
  StoreDefaults;
  ModalResult := mrOK;
end;

procedure TOracleDefaultsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOracleDefaultsForm.EnablePropButton(Edit: Tedit);
var X: TWinControl;
begin
  if PropEditButton.Visible then
  begin
    X := TWinControl(PropEditButton.Tag);
    if X <> nil then X.Width := X.Width + PropEditButton.Width;
    PropEditButton.Tag := 0;
  end;

  if Edit = nil then
    PropEditButton.Visible := False
  else begin
    PropEditButton.Top := Edit.Top;
    PropEditButton.Width := Edit.Height;
    PropEditButton.Height := Edit.Height;
    Edit.Width := Edit.Width - PropEditButton.Width;
    PropEditButton.Left := Edit.Left + Edit.Width;
    PropEditButton.Tag := Integer(Edit);
    PropEditButton.Visible := True;
  end;
end;

procedure TOracleDefaultsForm.EditFocusChange(Sender: TObject);
var Edit: TEdit;
    P: TDefaultItem;
begin
  Edit := TEdit(Sender);
  P := FindDefaultItem(Edit);
  if Edit.Focused then
  begin
    Edit.Color := clWindow;
    if (P <> nil) and ((P.PropType = 'L') or (P.PropType = 'M')) then
      EnablePropButton(Edit)
    else
      EnablePropButton(nil);
  end else begin
    if (Edit <> nil) and (not Edit.ReadOnly) and (P <> nil) then
    begin
      if P.Value <> Edit.Text then Changed := True;
      P.Value := Edit.Text;
    end;
    EnablePropButton(nil);
    Edit.Color := clBtnFace;
  end;
  if Changed then ChangedLabel.Caption := #$B0;
end;

procedure TOracleDefaultsForm.EditDblClick(Sender: TObject);
var P: TDefaultItem;
    L: TStringList;
    i: Integer;
begin
  P := FindDefaultItem(TComponent(Sender));
  if P <> nil then
  begin
    if P.PropType = 'L' then
    begin
      L := TStringList.Create;
      L.CommaText := P.Options;
      i := L.IndexOf(P.Edit.Text);
      inc(i);
      if i > (L.Count - 1) then i := 0;
      P.Edit.Text := L[i];
      L.Free;
    end;
  end;
end;

procedure TOracleDefaultsForm.EditKeyPress(Sender: TObject; var Key: Char);
var P: TDefaultItem;
begin
  P := FindDefaultItem(TComponent(Sender));
  if P <> nil then
  begin
    if P.PropType = 'I' then
    begin
      if (Key > #31) and not (Key in ['0' .. '9']) then Key := #0;
    end;
  end;
end;

procedure TOracleDefaultsForm.PopupClick(Sender: TObject);
var P: TDefaultItem;
    C: Longint;
begin
  P := TDefaultItem(TMenuItem(Sender).Tag);
  if (P <> nil) and (P.Edit <> nil) then
  begin
    if (P.ppi <> nil) and (P.ppi.PropType^.Name = 'TColor') then
    begin
      if IdentToColor(TMenuItem(Sender).Caption, C) then
      begin
        P.Edit.Text := '$' + IntTohex(C, 8);
      end;
    end else begin
      P.Edit.Text := TMenuItem(Sender).Caption;
    end;
    Changed := True;
  end;
end;

procedure TOracleDefaultsForm.PropEditButtonClick(Sender: TObject);
var P: TDefaultItem;
    T: TPoint;
    L: TStringList;
    S: string;
    i: Integer;
    Popup: TPopupMenu;
 procedure PopupAdd(S: string);
 var M: TMenuItem;
 begin
   M := TMenuItem.Create(Popup);
   M.Tag := Integer(P);
   M.Caption := S;
   M.OnClick := PopupClick;
   Popup.Items.Add(M);
 end;
begin
  P := FindDefaultItem(TComponent(TComponent(Sender).Tag));
  if P <> nil then
  begin
    if P.PropType = 'L' then
    begin
      L := TStringList.Create;
      L.CommaText := P.Options;
      Popup := TPopupMenu.Create(Self);
      {$IFDEF CompilerVersion5}
      Popup.AutoHotkeys := maManual;
      {$ENDIF}
      for i := 0 to L.Count - 1 do PopupAdd(L[i]);
      T.X := PropEditButton.Left;
      T.Y := PropEditButton.Top + PropEditButton.Height;
      T := ScrollBox.ClientToScreen(T);
      Popup.Popup(T.X, T.Y);
      {$IFNDEF LINUX}
      Application.ProcessMessages;
      Popup.Free;
      {$ENDIF}
      L.Free;
    end;
    if P.PropType = 'M' then
    begin
      S := P.Value;
      if ExecuteEditor(S) then
      begin
        Changed := True;
        P.Value := S;
        P.Edit.Text := GetDescription('(TStrings)', S);
      end;
    end;
  end;
  if Changed then ChangedLabel.Caption := #$B0;
end;

procedure TOracleDefaultsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if Changed then
  begin
    case Confirm('Save Changes?', 'Confirm', 'YNC') of
     IDYES: StoreDefaults;
      IDNO: CanClose := True;
  IDCANCEL: CanClose := False;
    end;
  end;
end;

procedure TOracleDefaultsForm.HelpBtnClick(Sender: TObject);
begin
  Help(Self, 'Direct Oracle Access Designtime Property Defaults');
end;

procedure TOracleDefaultsForm.DefaultBtnClick(Sender: TObject);
 function SetClass(Name: string): Boolean;
 var i: Integer;
 begin
   Result := SetCurrentClass(Name);
   if not Result then Exit;
   ComponentButtonClick(nil);
   for i := 0 to Defaults.Count - 1 do
   begin
     with TDefaultItem(Defaults.Items[i]) do
     begin
       Edit.Text := '';
       Value := '';
     end;
   end;
 end;
 procedure SetProperty(Name, Value: string);
 var i: Integer;
 begin
   for i := 0 to Defaults.Count - 1 do
   begin
     if TDefaultItem(Defaults.Items[i]).FullName = Name then
     begin
       TDefaultItem(Defaults.Items[i]).Value := Value;
       TDefaultItem(Defaults.Items[i]).Edit.Text := Value;
     end;
   end;
 end;
begin
  if Confirm('Set properties of all components to optimal defaults?', 'Confirm', 'YN') = IDYES then
  begin
    if SetClass('TOracleSessionPool') then
    begin
      StoreDefaults;
    end;
    if SetClass('TOracleLogon') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      SetProperty('Options.ldDatabaseList', 'True');
      SetProperty('Options.ldConnectAs', 'True');
      StoreDefaults;
    end;
    if SetClass('TOracleQuery') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleDataSet') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      SetProperty('QueryAllRecords', 'False');
      StoreDefaults;
    end;
    if SetClass('TOraclePackage') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleEvent') then
    begin
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleQueue') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleScript') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleDirectPathLoader') then
    begin
      SetProperty('Session', 'Autodetect');
      StoreDefaults;
    end;
    if SetClass('TOracleNavigator') then
    begin
      SetProperty('DataSource', 'Autodetect');
      SetProperty('VisibleButtons.nbRefreshRecord', 'True');
      StoreDefaults;
    end;
    // and last but not least, the OracleDataSet
    if SetClass('TOracleSession') then
    begin
      SetProperty('Cursor', 'crSQLWait');
      SetProperty('BytesPerCharacter', 'bcAutoDetect');
      StoreDefaults;
    end;
    ScrollBoxResize(nil);
  end;
end;

procedure TOracleDefaultsForm.ButtonPanelResize(Sender: TObject);
begin
  HelpBtn.Left := ButtonPanel.Width - HelpBtn.Width - 8;
end;

initialization
  Oracle.SetDefaults := SetComponentDefaults;

finalization
  Oracle.SetDefaults := nil;

end.
