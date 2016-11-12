// Direct Oracle Access - OracleMonitor
// Based on Borland's Inter-Process Communication (IPC)
// Copyright 1999 - 2003 Allround Automations
// support@allround.automations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleMonitor;

interface

uses
  SysUtils, Classes, Windows, Forms, StdCtrls, Dialogs,
  {$IFNDEF NODATASET} OracleData, {$ENDIF}
  OracleMonitorIntf, Oracle, OracleTypes;

type
  TMonitorClient = class(TMonitorThread)
  private
  protected
    Sequence: Integer;
    procedure Execute; override;
  public
    constructor Create(AID: Integer; const AName: string);
    destructor  Destroy; override;
    procedure Activate; override;
    procedure DeActivate; override;
    function  ClientCount: Integer;
    procedure SignalMonitor(ek: Integer);
    procedure OnSignal(Sender: TMonitorThread; Data: TEventData);     virtual;
    procedure OnComponents(Sender: TMonitorThread; Data: TEventData); virtual;
    procedure CheckMemSize;
    procedure SendComponents;
  end;

var
  DOAClient: TMonitorClient = nil;

implementation

var
  AppStartTime: DWord;
  ClientFinished: Boolean;

(*
function DOAComponentType(C: TObject): Integer;
begin
  Result := ckNone;
  if C is TOracleSession then Result := ckSession;
  if C is TOracleQuery then Result :=ckQuery;
  if C is TOracleDataSet then Result :=ckDataSet;
  if C is TOraclePackage then Result :=ckPackage;
  if C is TOracleEvent then Result :=ckEvent;
  if C is TOracleObject then Result :=ckObject;
  if C is TOracleReference then Result :=ckReference;
  if C is TLOBLocator then Result := ckLOBLocator;
end;
*)

procedure AddMonParameters(Parameters: TStrings; Signal: Boolean);
var i: Integer;
begin
  if Parameters = nil then
    DOAClient.AddEventInteger(0)
  else begin
    if odd(Parameters.Count) then
      DOAClient.AddEventInteger(0)
    else begin
      DOAClient.AddEventInteger(Parameters.Count div 2);
      for i := 0 to Parameters.Count - 1 do DOAClient.AddEventString(Parameters[i]);
    end;
  end;
  if Signal then
  begin
    DOAClient.AddEventString('');
    DOAClient.SignalMonitor(ekComponentList);
  end;
end;

procedure AddMonObject(ComponentType, SessionID, ID, ParentID: Integer; AObject: TObject; Parameters: TStrings; Signal: Boolean);
var Name: string;
 function OwnerName(C: TComponent): string;
 begin
   Result := '';
   C := C.Owner;
   while (C <> nil) and (C.Owner <> nil) {$IFNDEF NODATASET} and (not (C is TOracleDataSet)) {$ENDIF} do
   begin
     if C.Name = '' then Result := C.ClassName + '.' + Result else Result := C.Name + '.' + Result;
     C := C.Owner;
   end;
 end;
begin
  if AObject = nil then Exit;
  if (not Signal) or (Signal and DOAClient.CreateNewEvent) then
  begin
    if AObject is TComponent then
    begin
      if TComponent(AObject).Name = '' then Name := AObject.ClassName else Name := TComponent(AObject).Name;
      if ParentID <> 0 then
        Name := TComponent(AObject).Name
      else
        Name := OwnerName(TComponent(AObject)) + Name;
    end else begin
      Name := '';
      if AObject is TLOBLocator      then Name := TLOBLocator(AObject).Name;
      if AObject is TOracleObject    then Name := TOracleObject(AObject).Name;
      if AObject is TOracleReference then Name := TOracleReference(AObject).Name;
    end;
    if Name = '' then Name := AObject.ClassName;

    DOAClient.AddEventString(Name);

    DOAClient.AddEventInteger(ComponentType);
    DOAClient.AddEventInteger(SessionID);
    DOAClient.AddEventInteger(ID);
    DOAClient.AddEventInteger(ParentID);
    if ID > 0 then AddMonParameters(Parameters, False);
    if Signal then
    begin
      DOAClient.AddEventString('');
      DOAClient.SignalMonitor(ekComponentList);
    end;
  end;
end;

procedure AddMonActivity(ComponentType, SessionID, ID, ParentID: Integer; AObject: TObject;
  Flag, Info: Integer; const Description, SQL: string; const ResultMessage: string; Variables: TVariables;
  Parameters: TStrings; StartTime: DWord);
var i, VarCount: Integer;
    ElapsedTime: DWord;
begin
  if DOAClient.CreateNewEvent then
  begin
    AddMonObject(ComponentType, SessionID, ID, ParentID, AObject, nil, False);

    DOAClient.AddEventInteger(Flag);
    DOAClient.AddEventInteger(Info);
    if (Flag = afEnd) and (StartTime <> 0) then
    begin
      ElapsedTime := GetTickCount - StartTime;
      if ElapsedTime = 0 then ElapsedTime := 1;
    end else
      ElapsedTime := 0;
    if StartTime = 0 then StartTime := GetTickCount;
    DOAClient.AddEventInteger(StartTime - AppStartTime);
    DOAClient.AddEventInteger(ElapsedTime);
    DOAClient.AddEventString(Description);
    DOAClient.AddEventClippedString(SQL);
    DOAClient.AddEventString(ResultMessage);
    // The number of variables
    if Variables = nil then
      DOAClient.AddEventInteger(0)
    else begin
      VarCount := 0;
      for i := 0 to Variables.Count - 1 do
        if Variables.Data(i).BufType <> otSubst then Inc(VarCount);
      DOAClient.AddEventInteger(VarCount);
      // The variable names and values
      for i := 0 to Variables.Count - 1 do with Variables.Data(i) do
      begin
        if BufType <> otSubst then
        begin
          DOAClient.AddEventString(Name);
          DOAClient.AddEventClippedString(DisplayString(Variables.Owner));
        end;
      end;
    end;
    // Add the Parameters
    AddMonParameters(Parameters, False);
    DOAClient.AddEventString('');
    DOAClient.SignalMonitor(ekActivity);
    if (Flag <> afSingle) and DOAClient.StatisticsEnabled then DOAClient.WaitForMonitor;
  end;
end;

function MonitorPresent: Boolean;
begin
  Result := DOAClient.IsMonitorRunning;
end;

function MonitorListening: Boolean;
begin
  Result := MonitorPresent and DOAClient.MonitorEnabled;
end;

// TMonitorClient

constructor TMonitorClient.Create(AID: Integer; const AName: string);
begin
  inherited Create(AID, AName);
  FreeOnTerminate := True;
  CreateClientEvent(AID);
  SenderEvent   := FMonitorEvent;
  ReceiverEvent := FClientEvent;
  ReceiverEvent.Clear;
end;

destructor TMonitorClient.Destroy;
begin
  FMonitorEvent.EventData.Status := msAvailable;
  inherited Destroy;
  ClientFinished := True;
end;

procedure TMonitorClient.Activate;
begin
  Sequence := FClientDirectory.AddClient(FID, FName);
  FMonitorEvent.EventData.Status := msAvailable;
  Resume;
  FMainEvent.Signal;
end;

procedure TMonitorClient.DeActivate;
begin
  CreateNewEvent;
  Deactivating := True;
  FMainEvent.Signal;
  if Assigned(FClientDirectory) then FClientDirectory.RemoveClient(FID);
//  Terminate;
end;

procedure TMonitorClient.Execute;
var WaitResult: Integer;
begin
  while True do
  try
    WaitResult := WaitForSingleObject(FClientEvent.Handle, 100);
    if Terminated then Break;
    if WaitResult = WAIT_OBJECT_0 then
    try
      FClientEvent.EventData.Status := msInUse;
      case FClientEvent.EventData.EventKind of
 ekMonitorStarting: ;
 ekMonitorStopping: ;
          ekSignal: OnSignal(Self, FClientEvent.Data);
   ekComponentList: OnComponents(Self, FClientEvent.Data);
   ekGetProperties: ;
      end;
    finally
      FClientEvent.EventData.Status := msAvailable;
    end;
  except
    on E:Exception do
      DOALog(Format('Exception raised in Thread Handler: %s', [E.Message]));
  end;
end;

procedure TMonitorClient.SignalMonitor(ek: Integer);
begin
  FMonitorEvent.EventData.ClientID  := FID;
  FMonitorEvent.EventData.EventKind := ek;
  FMonitorEvent.EventData.DataIndex := 0;
  FMonitorEvent.EventData.Status    := msFilled;
  FMonitorEvent.Signal;
end;

function TMonitorClient.ClientCount: Integer;
begin
  Result := FClientDirectory.Count;
end;

procedure TMonitorClient.OnSignal(Sender: TMonitorThread; Data: TEventData);
begin
//  if Sender.GetEventInteger <> OMHeader then Exit;
  Sender.GetEventInteger;
  Sender.GetEventString;
end;

procedure TMonitorClient.OnComponents(Sender: TMonitorThread; Data: TEventData);
begin
//  if Sender.GetEventInteger <> OMHeader then Exit;
  SendComponents;
end;

procedure TMonitorClient.CheckMemSize;
begin
  if HighWaterMark then
  begin
    AddEventString('');
    SignalMonitor(ekComponentList);
    CreateNewEvent;
  end;
end;

procedure TMonitorClient.SendComponents;
var i, q: Integer;
begin
  with AllSessions.LockList do
  try
    for i := 0 to Count - 1 do TOracleSession(Items[i]).AUDSID;
  finally
    AllSessions.UnlockList;
  end;
  if CreateNewEvent then
  begin
    with AllSessions.LockList do
    try
      for i := 0 to Count - 1 do
      begin
        CheckMemSize;
        TOracleSession(Items[i]).SendToMonitor(True, False);
        with TOracleSession(Items[i]) do
        begin
          // Queries
          with Queries.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleQuery(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Queries.UnlockList;
          end;
          // DataSets
          {$IFNDEF NODATASET}
          with DataSets.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleDataSet(Items[q]).SendToMonitor(True, False);
            end;
          finally
            DataSets.UnlockList;
          end;
          {$ENDIF}
          // Packages
          with Packages.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOraclePackage(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Packages.UnlockList;
          end;
          // Events
          with Events.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleEvent(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Events.UnlockList;
          end;
          // LOBLocators
          with LOBLocators.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TLOBLocator(Items[q]).SendToMonitor(True, False);
            end;
          finally
            LOBLocators.UnlockList;
          end;
          // Objects
          with Objects.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleObject(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Objects.UnlockList;
          end;
          // References
          with References.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleReference(Items[q]).SendToMonitor(True, False);
            end;
          finally
            References.UnlockList;
          end;
          // CustomPackages
          with CustomPackages.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleCustomPackage(Items[q]).SendToMonitor(True, False);
            end;
          finally
            CustomPackages.UnlockList;
          end;
          // OracleScripts
          with Scripts.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleScript(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Scripts.UnlockList;
          end;
          // Queues
          with Queues.LockList do
          try
            for q := 0 to Count - 1 do
            begin
              CheckMemSize;
              TOracleQueue(Items[q]).SendToMonitor(True, False);
            end;
          finally
            Queues.UnlockList;
          end;
        end;
      end;
    finally
      AllSessions.UnlockList;
    end;
    AddEventString('');
    SignalMonitor(ekComponentList);
  end;
end;

procedure SendMonitorComponents;
begin
  if DOAClient <> nil then DOAClient.SendComponents;
end;

procedure MonitorFinalize;
begin
  InternalAddMonObject     := nil;
  InternalAddMonActivity   := nil;
  InternalMonitorPresent   := nil;
  InternalMonitorListening := nil;
  InternalMonitorFinalize  := nil;
  InternalSendMonitorComponents := nil;
  if DOAClient <> nil then
  begin
    ClientFinished := False;
    DOAClient.DeActivate;
    DOACLient.Terminate;
    repeat Sleep(0) until ClientFinished;
//  DOAClient.Free;
    DOAClient := nil;
  end;
end;

initialization
begin
  AppStartTime := GetTickCount;
  DOAClient := TMonitorClient.Create(GetCurrentProcessID, Application.Title);
  DOAClient.Activate;
  InternalAddMonObject     := @AddMonObject;
  InternalAddMonActivity   := @AddMonActivity;
  InternalMonitorPresent   := @MonitorPresent;
  InternalMonitorListening := @MonitorListening;
  InternalMonitorFinalize  := @MonitorFinalize;
  InternalSendMonitorComponents := @SendMonitorComponents;
end;

end.
