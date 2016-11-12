// Direct Oracle Access - Nonvisual stuff, used in NONVISUAL mode
// Copyright 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleVisual;

interface

{$IFNDEF LINUX}
uses
  {$IFNDEF NONVISUAL} Dialogs, Forms, Controls, Graphics, {$ENDIF}
  Classes, SysUtils, Windows;
{$ELSE}
uses
  {$IFNDEF NONVISUAL} QDialogs, QForms, QControls, Qt, QGraphics, {$ENDIF}
  Classes, SysUtils;
{$ENDIF}

{$IFDEF NONVISUAL}

 {$IFDEF LINUX}

type
  TCursor = Integer;
  TForm = TObject;
  TPicture = TObject;
  TColor = -$7FFFFFFF..$7FFFFFFF;
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  crHourGlass = -11;
  crDefault = 0;
  clNone = TColor($1FFFFFFF);

const
  mrNone     = 0;
  mrOk       = 1;
  mrCancel   = 2;
  mrAbort    = 3;
  mrRetry    = 4;
  mrIgnore   = 5;
  mrYes      = 6;
  mrNo       = 7;
  mrAll      = 8;
  mrNoToAll  = 9;
  mrYesToAll = 10;

const
  idYes     = Integer(mbYes);
  idNo      = Integer(mbNo);
  idCancel  = Integer(mbCancel);

const
  vk_F1     = 0;
  vk_F2     = 0;
  vk_F8     = 0;
  vk_Escape = 27;
  vk_Left   = 0;
  vk_Right  = 0;
  vk_Space  = 32;
  vk_Enter  = 13;
  vk_Return = 13;

 {$ELSE}

type
  TCursor = Integer;
  TForm = TObject;
  TPicture = TObject;
  TColor = $80000000..$7FFFFFFF;
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  crHourGlass = -11;
  crDefault = 0;
  clNone = TColor($1FFFFFFF);

const
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;
  mrAll      = mrNo + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;

 {$ENDIF}

{$ELSE}

 {$IFDEF LINUX}

const
  idYes     = Integer(smbYes);
  idNo      = Integer(smbNo);
  idCancel  = Integer(smbCancel);

const
  vk_F1     = Key_F1;
  vk_F2     = Key_F2;
  vk_F8     = Key_F8;
  vk_Escape = Key_Escape;
  vk_Left   = Key_Left;
  vk_Right  = Key_Right;
  vk_Space  = Key_Space;
  vk_Enter  = Key_Enter;
  vk_Return = Key_Return;

 {$ENDIF}

{$ENDIF}

procedure Help(Form: TForm; Topic: string);
procedure HelpbyIndex(Form: TForm; Topic: Integer);
function  Confirm(Text, Caption, Style: string): Integer;
procedure InitForm(Form: TForm);

//
function  GetScreenCursor: TCursor;
procedure SetScreenCursor(Cursor: TCursor);
procedure ProcessMessages;
function  ApplicationTerminated: Boolean;
procedure ApplicationHandleException(Sender: TObject);
function  ApplicationComponentCount: Integer;
function  ApplicationComponents(Index: Integer): TComponent;

//
{$IFDEF NONVISUAL}
function  MessageDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
procedure ShowMessage(const Msg: string);
{$ENDIF}

implementation

uses Oracle;

procedure Help(Form: TForm; Topic: string);
begin
  {$IFDEF LINUX}
  {$ELSE}
  {$IFNDEF NONVISUAL}
  WinHelp(Form.Handle, PChar(DOAHelpFile), Help_Key, LongInt(PChar(Topic)));
  {$ENDIF}
  {$ENDIF}
end;

procedure HelpbyIndex(Form: TForm; Topic: Integer);
begin
  {$IFDEF LINUX}
  {$ELSE}
  {$IFNDEF NONVISUAL}
  WinHelp(Form.Handle, PChar(DOAHelpFile), Help_Key, Topic);
  {$ENDIF}
  {$ENDIF}
end;

function Confirm(Text, Caption, Style: string): Integer;
var K: TMsgDlgButtons;
begin
  K := [];
  if Pos('Y', Style) > 0 then K := K + [mbYes];
  if Pos('N', Style) > 0 then K := K + [mbNo];
  if Pos('C', Style) > 0 then K := K + [mbCancel];
  if Pos('O', Style) > 0 then K := K + [mbOK];
  {$IFNDEF LINUX}
  if Pos('I', Style) > 0 then K := K + [mbIgnore];
  if Pos('A', Style) > 0 then K := K + [mbAll];
  {$ENDIF}
  Result := MessageDlg(Text, mtConfirmation, K, 0);
end;

procedure InitForm(Form: TForm);
begin
  {$IFDEF LINUX}
  {$IFNDEF NONVISUAL}
  if Form.BorderStyle = fbsDialog then with Form.Constraints do
  begin
    if MaxHeight = 0 then MaxHeight := Form.Height;
    if MinHeight = 0 then MinHeight := Form.Height;
    if MaxWidth = 0  then MaxWidth  := Form.Width;
    if MinWidth = 0  then MinWidth  := Form.Width;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function GetScreenCursor: TCursor;
begin
  {$IFNDEF NONVISUAL}
  Result := Screen.Cursor;
  {$ELSE}
  Result := crDefault;
  {$ENDIF}
end;

procedure SetScreenCursor(Cursor: TCursor);
begin
  {$IFNDEF NONVISUAL}
  Screen.Cursor := Cursor;
  {$ENDIF}
end;

procedure ProcessMessages;
begin
  {$IFNDEF NONVISUAL}
  Application.ProcessMessages;
  {$ENDIF}
end;

function ApplicationTerminated: Boolean;
begin
  {$IFDEF NONVISUAL}
  Result := False;
  {$ELSE}
  Result := Application.Terminated;
  {$ENDIF}
end;

procedure ApplicationHandleException(Sender: TObject);
begin
  {$IFDEF NONVISUAL}
  SysUtils.ShowException(ExceptObject, ExceptAddr);
  {$ELSE}
  Application.HandleException(Sender);
  {$ENDIF}
end;

function ApplicationComponentCount: Integer;
begin
  {$IFDEF NONVISUAL}
  Result := 0;
  {$ELSE}
  Result := Application.ComponentCount;
  {$ENDIF}
end;

function ApplicationComponents(Index: Integer): TComponent;
begin
  {$IFDEF NONVISUAL}
  Result := nil;
  {$ELSE}
  Result := Application.Components[Index];
  {$ENDIF}
end;

{$IFDEF NONVISUAL}
function MessageDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
var R, S: string;
begin
  Result := mrNone;
  WriteLn(Msg);
  S := '';
  if mbYes in AButtons then S := S + ' Yes';
  if mbNo in AButtons then S := S + ' No';
  if mbOK in AButtons then S := S + ' OK';
  if mbCancel in AButtons then S := S + ' Cancel';
  if mbAbort in AButtons then S := S + ' Abort';
  if mbRetry in AButtons then S := S + ' Retry';
  if mbIgnore in AButtons then S := S + ' Ignore';
  repeat
    Write(S + ': ');
    ReadLn(R);
    R := UpperCase(Copy(R, 1, 1));
    if (mbYes in AButtons) and (R = 'Y') then Result := mrYes;
    if (mbNo in AButtons) and (R = 'N') then Result := mrNo;
    if (mbOK in AButtons) and (R = 'O') then Result := mrOK;
    if (mbCancel in AButtons) and (R = 'C') then Result := mrCancel;
    if (mbAbort in AButtons) and (R = 'A') then Result := mrAbort;
    if (mbRetry in AButtons) and (R = 'R') then Result := mrRetry;
    if (mbIgnore in AButtons) and (R = 'I') then Result := mrIgnore;
  until Result <> mrNone;
end;

procedure ShowMessage(const Msg: string);
begin
  WriteLn(Msg);
  Write('Press enter to continue');
  ReadLn;
end;
{$ENDIF}

end.
