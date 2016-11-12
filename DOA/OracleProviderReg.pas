// Direct Oracle Access - Component registration unit
// Copyright 1997, 1998 Allround Automations
// support@allroundautomations.nl
// http://www.allroundautomations.nl

{$WARNINGS OFF}

unit OracleProviderReg;

interface

uses
  Classes, SysUtils, dsgnintf, DB, Provider, OracleProvider;

procedure Register;

implementation

procedure Register;
var TmpProvider: TProvider;
    TmpProviderEditor: TComponentEditor;
begin
  try
    TmpProvider := nil;
    TmpProviderEditor := nil;
    RegisterComponents('Data Access', [TOracleProvider]);
    // Some tricks to add the standard component editor
    try
      TmpProvider := TProvider.Create(nil);
      TmpProviderEditor := GetComponentEditor(TmpProvider, nil);
      {$IFNDEF VER130}
      OracleProviderEditorClass := TComponentEditorClass(TmpProviderEditor.ClassType);
      RegisterComponentEditor(TOracleProvider, TOracleProviderEditor);
      {$ENDIF}
    finally
      TmpProviderEditor.Free;
      TmpProvider.Free;
    end;
  except;
    // If it fails, it obviously wasn't the C/S version
  end;
end;

end.

