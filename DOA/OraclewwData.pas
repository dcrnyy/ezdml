// Direct Oracle Access - Dataset for Woll2Woll Infopower components
// Copyright 1997, 1998 Allround Automations
// support@allround.automations.nl
// http://www.allroundautomations.nl

{$D-}

unit OraclewwData;

interface

uses Classes, DB, wwTypes, OracleData;

type
  TOraclewwDataSet = class(TOracleDataSet)
  private
    FControlType: TStrings;
    FPictureMasks: TStrings;
    FUsePictureMask: Boolean;
    FOnInvalidValue: TwwInvalidValueEvent;
    function  GetControlType: TStrings;
    procedure SetControlType(sel: TStrings);
    function  GetPictureMasks: TStrings;
    procedure SetPictureMasks(sel: TStrings);
    function  GetMasterSource: TDataSource;
    procedure SetMasterSource(Value: TDataSource);
  protected
    procedure DoBeforePost; override; { For picture support }
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
  published
{   property IndexDefs;  //!!!Only Publish this if your component defines IndexDefs}
    property ControlType: TStrings read GetControlType write SetControltype;
    property PictureMasks: TStrings read GetPictureMasks write SetPictureMasks;
    property ValidateWithMask: Boolean read FUsePictureMask write FUsePictureMask;
    property OnInvalidValue: TwwInvalidValueEvent read FOnInvalidValue write FOnInvalidValue;
    // Property added for fcDBTreeView
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource stored False;
  end;

procedure Register;

implementation

uses wwCommon, Dialogs;

constructor TOraclewwDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlType := TStringList.Create;
  FPictureMasks := TStringList.Create;
  FUsePictureMask := True;
end;

destructor TOraclewwDataSet.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  FPictureMasks := nil;
  inherited Destroy;
end;

function TOraclewwDataSet.GetControltype: TStrings;
begin
  Result := FControlType;
end;

procedure TOraclewwDataSet.SetControlType(sel: TStrings);
begin
  FControlType.Assign(sel);
end;

function TOraclewwDataSet.GetPictureMasks: TStrings;
begin
  Result := FPictureMasks
end;

procedure TOraclewwDataSet.SetPictureMasks(sel: TStrings);
begin
  FPictureMasks.Assign(sel);
end;

procedure TOraclewwDataSet.DoBeforePost;
begin
  inherited DoBeforePost;
  if FUsePictureMask then wwValidatePictureFields(self, FOnInvalidValue);
end;

// The MasterSource property is added for fcDBTreeView compatibility
// It is used to find the DataSources between FirstDataSource and LastDataSource
// By deriving it from the Master property, a developer does not need to do
// anything to make it work
function TOraclewwDataSet.GetMasterSource: TDataSource;
var O, DS: TComponent;
    i: Integer;
begin
  Result := nil;
  if Master <> nil then
  begin
    // Search the Owner for a DataSource connected to the Master
    O := Master.Owner;
    for i := 0 to O.ComponentCount - 1 do
    begin
      DS := O.Components[i];
      if (DS is TDataSource) and (TDataSource(DS).DataSet = Master) then
      begin
        Result := TDataSource(DS);
        Break;
      end;
    end;
  end;
end;

// The property is read-only, but must be published or fcDBTreeView won't find it
procedure TOraclewwDataSet.SetMasterSource(Value: TDataSource);
begin
  ShowMessage('The MasterSource property cannot be set. It is derived'#13 +
              'from the Master property, and is only present for'#13 +
              'compatibility with the 1stClass TfcDBTreeView component.');
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TOraclewwDataSet]);
end;

end.

