// Direct Oracle Access - QueryBuilder (DLL) interface
// Copyright 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleQB;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs, Oracle, OracleCI,
  Controls, Graphics, Buttons, ExtCtrls;

var
  QBInit: procedure(Handle: THandle);
  QBClose: procedure;
  RunQueryBuilder: function(Username, Password, Database: PChar; var SQL: PChar): Boolean;
  AdjustBitmap: procedure(var BMP: HBitmap; NumGlyphs, FadeColor: Integer);

procedure QBCreate(Handle: THandle);
procedure QBFree;
function  CanQB: Boolean;
function  DoQueryBuilder(Session: TOracleSession; var SQL: string): Boolean;

procedure ChangeGlyphInit(Form: TForm);
procedure ChangeGlyphClose;
procedure ChangeGlyphs(Control: TWinControl);

implementation

var HDLL: THandle = hInstance_Error - 1;

function DLLLoaded:Boolean;
begin
  Result := HDLL >= hInstance_Error;
end;

procedure GetProc(Handle: THandle; var OK: Boolean; var Ad: pointer; const Name: string);
begin
  if not OK then
    Ad := nil
  else begin
    Ad := GetProcAddress(Handle, PChar(Name));
    if Ad = nil then OK := False;
  end;
end;

procedure QBCreate(Handle: THandle);
var OK: Boolean;
    Path: string;
begin
  Path := ReadRegString(HKEY_CURRENT_USER, 'Software\Allround Automations\QueryBuilder', 'dll');
  if Path = '' then Path := 'qb.dll';
  HDLL := LoadLibrary(PChar(Path));
  if DLLLoaded then
  begin
    OK := True;
    GetProc(HDLL, OK, @RunQueryBuilder, 'RunQueryBuilder');
    GetProc(HDLL, OK, @QBInit, 'QBInit');
    GetProc(HDLL, OK, @QBClose, 'QBClose');
    if not OK then QBFree;
    GetProc(HDLL, OK, @AdjustBitmap, 'AdjustBitmap');
    if (Handle > 0) and DLLLoaded then QBInit(Handle);
  end;
end;

procedure QBFree;
begin
  if DLLLoaded then
  begin
    QBClose;
    FreeLibrary(HDLL);
  end;
  HDLL := hInstance_Error - 1;
end;

function CanQB: Boolean;
begin
  Result := DLLLoaded;
end;

function DoQueryBuilder(Session: TOracleSession; var SQL: string): Boolean;
var P: PChar;
    Username, Password, Database: string;
begin
  Result := False;
  if not DLLLoaded then
  begin
    ShowMessage('Query Builder not found.' + #13#10#13#10 +
                'Please make sure you have downloaded and' + #13#10 +
                'installed the Query Builder.');
    Exit;
  end;
  P := PChar(SQL);
  if Session = nil then
  begin
    Username := '';
    Password := '';
    Database := '';
  end else begin
    Username := Session.LogonUsername;
    Password := Session.LogonPassword;
    Database := Session.LogonDatabase;
  end;
  if RunQueryBuilder(PChar(Username), PChar(Password), PChar(Database), P) then
  begin
    SQL := P;
    Result := True;
  end;
end;

procedure ChangeGlyphInit(Form: TForm);
begin
  if GetDeviceCaps(Form.Canvas.Handle, BITSPIXEL) > 7 then QBCreate(0);
end;

procedure ChangeGlyphClose;
begin
  if DLLLoaded then FreeLibrary(HDLL);
  HDLL := hInstance_Error - 1;
end;

procedure ChangeGlyphs(Control: TWinControl);
var i: Integer;
    FadeColor: TColor;
    BMP: HBitMap;
begin
  if not Assigned(AdjustBitmap) then Exit;
  if Control is TPanel then
    FadeColor := TPanel(Control).Color
  else
    FadeColor := clBtnFace;
//  ShowMessage('ChangeGlyphs count: ' + IntToStr(Control.ControlCount));
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TSpeedButton then with TSpeedButton(Control.Controls[i]) do
    begin
//      ShowMessage('Adjust: ' + IntToStr(Glyph.Width));
      if NumGlyphs = 1 then
      begin
        NumGlyphs   := 2;
        Glyph.Width := Glyph.Width * 2;
      end;
      BMP := Glyph.Handle;
      AdjustBitmap(BMP, NumGlyphs, FadeColor);
      Glyph.Handle := BMP;
    end;
  end;
end;

end.
