// Direct Oracle Access - OracleMonitor Interface
// Based on Borland's Inter-Process Communication (IPC)
// Copyright 1999 - 2003 Allround Automations
// support@allround.automations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

unit OracleMonitorIntf;

interface

uses
  SysUtils, Classes, Windows, Forms, OracleCI;

const
  MAX_CLIENTS        = 100;
  TIMEOUT            = 5000;
  MONITOR_EVENT_NAME = 'DOAMonitorEvent';
  CLIENT_EVENT_NAME  = 'DOAClientEvent';
  CLIENT_DIR_NAME    = 'DOADirectory';
  CLIENT_DIR_MUTEX   = 'DOADirectoryMutex';
  EventDataSize      = $00FFFF;

const // Interface version and 'magic' value for identification of valid events
  OMVersion = 1;
  {$IFNDEF EVALUATION}
  OMHeader  = 256 * 256 * (256 * Ord('O') + Ord('X')) + OMVersion;
  {$ELSE}
  {$I MonitorEvaluation.inc}
  {$ENDIF}

const // EventKind constants
  ekNone            = 0;
  ekMonitorStarting = 1;   // Monitor is started
  ekMonitorStopping = 2;   // Monitor is stopped
  ekClientStarting  = 3;   // a Client is started
  ekClientStopping  = 4;   // a Client is stopped
  ekSignal          = 10;  // Signal to monitor or client
  ekComponentList   = 11;  // Monitor requests a list of components or Client sends them
  ekActivity        = 12;  // Client sends object activity information
  ekGetProperties   = 20;  // Monitor requests a list of properties of a component
  ekSetProperties   = 21;  // Monitor sends (modified) properties back to Client

const // ActionKind constants
  akNone            = 0;
  akDebugMessage    = 1;
  akExecute         = 10;
  akParse           = 11;
  akDescribe        = 12;

const // MemoryStatus constants
  msAvailable       = 0;
  msReserved        = 1;
  msFilled          = 2;
  msInUse           = 3;

const // MonitorMode bitmask
  mmEnabled         = $0100;
  mmStatistics      = $0200;

type
  TMonitorThread = class;

// TMutex
// This class encapsulates the concept of a Win32 mutex.  See "CreateMutex"
// in the Win32 reference for more information
  TMutex = class(TObject)
  public
    Handle: THandle;
    constructor Create(const Name: string);
    destructor  Destroy; override;
    function Get(TimeOut: Integer): Boolean;
    function Release: Boolean;
  end;

// TEvent
// This class encapsulates the concept of a Win32 event (not to be
// confused with Delphi events), see "CreateEvent" in the Win32
// reference for more information
  TEvent = class(TObject)
  public
    Handle: THandle;
    constructor Create(const Name: string; Manual: Boolean);
    destructor  Destroy; override;
    procedure Signal;
    procedure Reset;
    function  Wait(TimeOut: Integer): Boolean;
  end;

// TSharedMem
// This class simplifies the process of creating a region of shared memory.
// In Win32, this is accomplished by using the CreateFileMapping and
// MapViewOfFile functions.
  TSharedMem = class(TObject)
  private
    FName: string;
    FSize: Integer;
    FCreated: Boolean;
    FFileView: Pointer;
  public
    Handle: THandle;
    constructor Create(const Name: string; Size: Integer);
    destructor  Destroy; override;
    property Name: string read FName;
    property Size: Integer read FSize;
    property Buffer: Pointer read FFileView;
    property Created: Boolean read FCreated;
  end;

// TClientDirectory
// The client directory is a block of shared memory where the list of all
// active clients is maintained
  TClientDirEntry = packed record
    ID: Integer;
    SequenceID: Integer;
    ApplicationHandle: HWnd;
    Name: Array[0..58] of Char;
  end;
  TClientDirRecords = array[1..MAX_CLIENTS] of TClientDirEntry;
  PClientDirRecords = ^TClientDirRecords;
  TClientDirectory = class
  private
    FClientCount: PInteger;
    FMonitorID: PInteger;
    FSequence: PInteger;
    FMonitorMode: PInteger;
    FMaxClients: Integer;
    FMutex: TMutex;
    FSharedMem: TSharedMem;
    FDirBuffer: PClientDirRecords;
    function  GetCount: Integer;
    function  GetClientName(ClientID: Integer): string;
    function  GetClientRec(Index: Integer): TClientDirEntry;
    function  IndexOf(ClientID: Integer): Integer;
    function  SequenceIndexOf(SequenceID: Integer): Integer;
    function  GetMonitorID: Integer;
    procedure SetMonitorID(MonitorID: Integer);
    function  GetMonitorMode: Integer;
    procedure SetMonitorMode(AMonitorMode: Integer);
    function  Sequence: Integer;
  public
    constructor Create(MaxClients: Integer);
    destructor  Destroy; override;
    function AddClient(ClientID: Integer; const AName: string): Integer;
    function Last: Integer;
    function RemoveClient(ClientID: Integer): Boolean;
    function FindClient(SequenceID: Integer): TClientDirEntry;
    property Count: Integer read GetCount;
    property ClientRec[Index: Integer]: TClientDirEntry read GetClientRec;
    property MonitorID: Integer read GetMonitorID write SetMonitorID;
    property MonitorMode: Integer read GetMonitorMode write SetMonitorMode;
    property Name[ClientID: Integer]: string read GetClientName;
  end;

// TMonitorEvent
// Win32 events are very basic.  They are either signaled or non-signaled.
// This Event class creates a "typed" TEvent, by using a block of shared
// memory to hold an "EventKind" property.  The shared memory is also used
// to hold an ID, which is important when running multiple clients, and
// a Data area for communicating data along with the event

  PEventData = ^TEventData;
  TEventData = packed record
    EventKind: Integer;       // ek.. constant
    ClientID:  Integer;       // DOA Application ID
    DataIndex: Integer;       // Used to read/write data
    DataSize:  Integer;       // Used data
    Status:    Integer;       // Memory status, ms.. constant
    Data: Array[0..EventDataSize] of Byte;
  end;

  TMonitorEvent = class(TEvent)
  private
    FOwner: TMonitorThread;
    FSharedMem: TSharedMem;
    FEventData: PEventData;
    function  GetData: TEventData;
    procedure SetData(Value: TEventData);
  public
    constructor Create(AOwner: TMonitorThread; const Name: string; Manual: Boolean);
    destructor  Destroy; override;
    procedure Clear;
    function  WaitFor(TimeOut: Integer): Boolean;
    property  Data: TEventData read GetData write SetData;
    property  EventData: PEventData read FEventData;
  end;

// TIPCThread
// The TIPCThread class implements the functionality which is common between
// the monitor and client thread classes.
  TMonitorThread = class(TThread)
  private
    function GetStatisticsEnabled: Boolean;
    function GetMonitorEnabled: Boolean;
    procedure SetStatisticsEnabled(Value: Boolean);
    procedure SetMonitorEnabled(Value: Boolean);
  protected
    CreateEventSection: TOracleCriticalSection;
    FID: Integer;
    FName: string;
    FClientEvent: TMonitorEvent;
    FMonitorEvent: TMonitorEvent;
    FMainEvent: TEvent;
    FClientDirectory: TClientDirectory;
    SenderEvent: TMonitorEvent;
    ReceiverEvent: TMonitorEvent;
    FMonitorMutex: TMutex;
    FDeactivating: Boolean;
  public
    StringTrimmed: Boolean;
    constructor Create(AID: Integer; const AName: string);
    destructor  Destroy; override;
    procedure CreateClientEvent(AID: Integer);
    procedure Activate; virtual; abstract;
    procedure DeActivate; virtual; abstract;
    function  IsMonitorRunning: Boolean;
    function  HighWaterMark: Boolean;
    function  WaitForMonitor: Boolean;
    function  CreateNewEvent: Boolean;
    procedure AddEventClippedString(const S: string);
    procedure AddEventString(const S: string);
    procedure AddEventInteger(I: Integer);
    function  GetEventString: string;
    function  GetEventInteger: Integer;
    property  Name: string read FName;
    property  StatisticsEnabled: Boolean read GetStatisticsEnabled write SetStatisticsEnabled;
    property  MonitorEnabled: Boolean read GetMonitorEnabled write SetMonitorEnabled;
    property  Deactivating: Boolean read FDeactivating write FDeactivating;
  end;

const
  NoDirEntry: TClientDirEntry = (ID: 0; SequenceID: 0);

procedure Error(const Msg: string);

implementation

uses TypInfo;

var SecurityAttributes: TSecurityAttributes; // SECURITY_ATTRIBUTES
    SecurityDescriptor: TSecurityDescriptor; // SECURITY_DESCRIPTOR
    PSA: PSecurityAttributes;
    PSD: PSecurityDescriptor;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

// TMutex

constructor TMutex.Create(const Name: string);
begin
  Handle := CreateMutex(PSA, False, PChar(Name));
  if Handle = 0 then Error('Error creating mutex');
end;

destructor TMutex.Destroy;
begin
  if Handle <> 0 then CloseHandle(Handle);
  inherited Destroy;
end;

function TMutex.Get(TimeOut: Integer): Boolean;
begin
  Result := WaitForSingleObject(Handle, TimeOut) = WAIT_OBJECT_0;
end;

function TMutex.Release: Boolean;
begin
  Result := ReleaseMutex(Handle);
end;

// TEvent

constructor TEvent.Create(const Name: string; Manual: Boolean);
begin
  Handle := CreateEvent(PSA, Manual, False, PChar(Name));
  if Handle = 0 then Error('Error creating event');
end;

destructor TEvent.Destroy;
begin
  if Handle <> 0 then CloseHandle(Handle);
  inherited Destroy;
end;

procedure TEvent.Reset;
begin
  ResetEvent(Handle);
end;

procedure TEvent.Signal;
begin
  SetEvent(Handle);
end;

function TEvent.Wait(TimeOut: Integer): Boolean;
begin
  Result := WaitForSingleObject(Handle, TimeOut) = WAIT_OBJECT_0;
end;

// TSharedMem

constructor TSharedMem.Create(const Name: string; Size: Integer);
begin
  try
    FName := Name;
    FSize := Size;
    Handle := CreateFileMapping($FFFFFFFF, PSA, PAGE_READWRITE, 0, Size, PChar(Name));
    if Handle = 0 then Error('CreateFileMapping');
    FCreated := GetLastError = 0;
    FFileView := MapViewOfFile(Handle, FILE_MAP_WRITE, 0, 0, Size);
    if FFileView = nil then Error('MapViewOfFile');
  except on E:Exception do
    Error(Format('Error creating shared memory %s (%d): %s', [Name, GetLastError, E.Message]));
  end;
end;

destructor TSharedMem.Destroy;
begin
  if FFileView <> nil then UnmapViewOfFile(FFileView);
  if Handle <> 0 then CloseHandle(Handle);
  inherited Destroy;
end;

// TClientDirectory

constructor TClientDirectory.Create(MaxClients: Integer);
begin
  FMaxClients := MaxClients;
  FMutex := TMutex.Create(CLIENT_DIR_MUTEX);
  FSharedMem := TSharedMem.Create(CLIENT_DIR_NAME, FMaxClients * SizeOf(TClientDirEntry) + 16);
  FMonitorID := FSharedMem.Buffer;
  Integer(FClientCount) := Integer(FMonitorID) + SizeOf(FMonitorID);
  Integer(FSequence)    := Integer(FClientCount) + SizeOf(FCLientCount);
  Integer(FMonitorMode) := Integer(FSequence) + SizeOf(FSequence);
  Integer(FDirBuffer)   := Integer(FMonitorMode) + SizeOf(FMonitorMode);
end;

destructor TClientDirectory.Destroy;
begin
  FMutex.Free;
  FSharedMem.Free;
end;

function TClientDirectory.AddClient(ClientID: Integer; const AName: string): Integer;
begin
  Result := 0;
  if Count = FMaxClients then Error(Format('Maximum of %d clients allowed', [FMaxClients]));
  // If there is an 'old' process with the same ID, remove it
  if IndexOf(ClientID) > -1 then RemoveClient(ClientID);
  if IndexOf(ClientID) > -1 then Error('Duplicate client ID');
  if FMutex.Get(TIMEOUT) then
  try
    with FDirBuffer[Count + 1] do
    begin
      SequenceID := Sequence;
      ID := ClientID;
      ApplicationHandle := Application.Handle;
      StrPLCopy(Name, PChar(AName), SizeOf(Name) - 1);
      Inc(FClientCount^);
      Result := SequenceID;
    end;
  finally
    FMutex.Release;
  end;
end;

function TClientDirectory.GetCount: Integer;
begin
  Result := FClientCount^;
end;

function TClientDirectory.GetClientRec(Index: Integer): TClientDirEntry;
begin
  if (Index > 0) and (Index <= Count) then
    Result := FDirBuffer[Index]
  else
    Error('Invalid client list index');
end;

function TClientDirectory.GetClientName(ClientID: Integer): string;
var Index: Integer;
begin
  Index := IndexOf(ClientID);
  if Index > 0 then Result := FDirBuffer[Index].Name else Result := '';
end;

function TClientDirectory.IndexOf(ClientID: Integer): Integer;
var i: Integer;
begin
  for i := 1 to Count do
  begin
    if FDirBuffer[i].ID = ClientID then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TClientDirectory.SequenceIndexOf(SequenceID: Integer): Integer;
var i: Integer;
begin
  for i := 1 to Count do
  begin
    if FDirBuffer[i].SequenceID = SequenceID then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TClientDirectory.Last: Integer;
begin
  if Count > 0 then Result := FDirBuffer[Count].ID else Result := 0;
end;

function TClientDirectory.RemoveClient(ClientID: Integer): Boolean;
var Index: Integer;
begin
  Index := IndexOf(ClientID);
  if (Index > -1) and FMutex.Get(TIMEOUT) then
  try
    if (Index > 0) and (Index < Count) then
      Move(FDirBuffer[Index+1], FDirBuffer[Index], (Count - Index) * SizeOf(TClientDirEntry));
    Dec(FClientCount^);
    Result := True;
  finally
    FMutex.Release;
  end else
    Result := False;
end;

function TClientDirectory.FindClient(SequenceID: Integer): TClientDirEntry;
var Index: Integer;
begin
  Index := SequenceIndexOf(SequenceID);
  if Index > 0 then Result := FDirBuffer[Index] else Result := NoDirEntry;
end;

function TClientDirectory.GetMonitorID: Integer;
begin
  Result := FMonitorID^;
end;

procedure TClientDirectory.SetMonitorID(MonitorID: Integer);
begin
  FMonitorID^ := MonitorID;
end;

function TClientDirectory.GetMonitorMode: Integer;
begin
  Result := FMonitorMode^;
end;

procedure TClientDirectory.SetMonitorMode(AMonitorMode: Integer);
begin
  FMonitorMode^ := AMonitorMode;
end;

function TClientDirectory.Sequence: Integer;
begin
  Result := FSequence^;
  inc(Result);
  FSequence^ := Result;
end;

// TMonitorThread

constructor TMonitorThread.Create(AID: Integer; const AName: string);
begin
  inherited Create(True);
  FID := AID;
  FName := AName;
  FMainEvent    := TEvent.Create(MONITOR_EVENT_NAME, False);
  FMonitorEvent := TMonitorEvent.Create(Self, MONITOR_EVENT_NAME + '_' + IntToHex(AID, 8), False);
  FClientEvent  := nil;
  FClientDirectory := TClientDirectory.Create(MAX_CLIENTS);
  CreateEventSection := TOracleCriticalSection.Create;
end;

destructor TMonitorThread.Destroy;
begin
  DeActivate;
  inherited Destroy;
  FClientDirectory.Free;
  FClientEvent.Free;
  FMainEvent.Free;
  FMonitorEvent.Free;
  CreateEventSection.Free;
end;

// Needs to be called before the Monitor can signal a specific client
procedure TMonitorThread.CreateClientEvent(AID: Integer);
begin
  if FClientEvent <> nil then FClientEvent.Free;
  FClientEvent := TMonitorEvent.Create(Self, CLIENT_EVENT_NAME + '_' + IntToHex(AID, 8), False);
  SenderEvent  := FClientEvent;
end;

function TMonitorThread.WaitForMonitor: Boolean;
var Delay, Delayed: Integer;
    DelayMode: Integer;
begin
  Result := False;
  if not IsMonitorRunning then Exit;        // Monitor is not running
  Delay := 1;
  DelayMode := 0;
  Delayed := 0;
  while (not Deactivating) and (SenderEvent.FEventData.Status <> msAvailable) do
  begin
    if Deactivating then Exit;
    if DelayMode < 10 then
      Sleep(0)                                 // Give the monitor some time
    else begin
      Sleep(Delay);
      inc(Delayed, Delay);
      if Delay < 100 then Delay := 2 * Delay;
      if Delayed > 2000 then
      begin
        if not IsMonitorRunning then Exit;  // Monitor has stopped, no use to continue
        Delayed := 0;
      end;
    end;
    inc(DelayMode);
    if DelayMode > 600 then Exit;              // Monitor time-out ~ 60 seconds
  end;
  Result := not Deactivating;
end;

function TMonitorThread.CreateNewEvent: Boolean;
begin
  CreateEventSection.Enter;
  try
    Result := WaitForMonitor;
    if Result then
    begin
      SenderEvent.FEventData.Status    := msReserved;
      SenderEvent.FEventData.DataIndex := 0;
      SenderEvent.FEventData.DataSize  := 0;
      AddEventInteger(OMHeader);
    end;
  finally
    CreateEventSection.Leave;
  end;
end;

procedure TMonitorThread.AddEventClippedString(const S: string);
var Ix, m: Integer;
begin
  m := EventDataSize div 2;                              // max 32K ...
  If SenderEvent.FEventData.DataIndex > m then m := 100; // ... or 100 bytes
  if Length(S) > m then m := -m else m := Length(S);
  AddEventInteger(m);
  Ix := SenderEvent.FEventData.DataIndex;
  Move(S[1], SenderEvent.FEventData.Data[Ix], Abs(m));
  Inc(Ix, Abs(m));
  SenderEvent.FEventData.DataIndex := Ix;
end;

procedure TMonitorThread.AddEventString(const S: string);
var Ix, m: Integer;
begin
  m := Length(S);
  AddEventInteger(Length(S));
  Ix := SenderEvent.FEventData.DataIndex;
  Move(S[1], SenderEvent.FEventData.Data[Ix], m);
  Inc(Ix, m);
  SenderEvent.FEventData.DataIndex := Ix;
end;

procedure TMonitorThread.AddEventInteger(I: Integer);
var Ix: Integer;
begin
  Ix := SenderEvent.FEventData.DataIndex;
  Move(I, SenderEvent.FEventData.Data[Ix], 4);
  Inc(Ix, 4);
  SenderEvent.FEventData.DataIndex := Ix;
end;

function TMonitorThread.GetEventString: string;
var Ix: Integer;
    Size: Integer;
begin
  Size := GetEventInteger;
  StringTrimmed := (Size < 0);
  Size := Abs(Size);
  Ix := ReceiverEvent.FEventData.DataIndex;
  SetLength(Result, Size);
  Move(ReceiverEvent.FEventData.Data[Ix], Result[1], Size);
  Inc(Ix, Size);
  ReceiverEvent.FEventData.DataIndex := Ix;
  if StringTrimmed then Result := Result + #13#10 + '...';
end;

function TMonitorThread.GetEventInteger: Integer;
var Ix: Integer;
begin
  Ix := ReceiverEvent.FEventData.DataIndex;
  Move(ReceiverEvent.FEventData.Data[Ix], Result, 4);
  Inc(Ix, 4);
  ReceiverEvent.FEventData.DataIndex := Ix;
end;

function TMonitorThread.GetStatisticsEnabled: Boolean;
begin
  Result := (FClientDirectory.MonitorMode and mmStatistics <> 0)
end;

function TMonitorThread.GetMonitorEnabled: Boolean;
begin
  Result := (FClientDirectory.MonitorMode and mmEnabled <> 0)
end;

procedure TMonitorThread.SetStatisticsEnabled(Value: Boolean);
begin
  if Value then
    FClientDirectory.MonitorMode := FClientDirectory.MonitorMode or mmStatistics
  else
    FClientDirectory.MonitorMode := FClientDirectory.MonitorMode and not mmStatistics;
end;

procedure TMonitorThread.SetMonitorEnabled(Value: Boolean);
begin
  if Value then
    FClientDirectory.MonitorMode := FClientDirectory.MonitorMode or mmEnabled
  else
    FClientDirectory.MonitorMode := FClientDirectory.MonitorMode and not mmEnabled;
end;

// Check if the monitor application is running
function TMonitorThread.IsMonitorRunning: Boolean;
begin
  Result := (FClientDirectory.MonitorID <> 0);
end;

// Check if the shared memory is almost full
function TMonitorThread.HighWaterMark: Boolean;
begin
  Result := (EventDataSize - SenderEvent.EventData.DataIndex < 1024);
end;

// TMonitorEvent

constructor TMonitorEvent.Create(AOwner: TMonitorThread; const Name: string; Manual: Boolean);
begin
  inherited Create(Name, Manual);
  FOwner := AOwner;
  FSharedMem := TSharedMem.Create(Format('%s.Data', [Name]), SizeOf(TEventData));
  FEventData := FSharedMem.Buffer;
  if FSharedMem.Created then
  begin
    FEventData.Status   := msAvailable;
    FEventData.ClientID := 0;
  end;
end;

destructor TMonitorEvent.Destroy;
begin
  FSharedMem.Free;
  inherited Destroy;
end;

function TMonitorEvent.GetData: TEventData;
begin
  Result := FEventData^;
end;

procedure TMonitorEvent.SetData(Value: TEventData);
begin
  FEventData^ := Value;
end;

function TMonitorEvent.WaitFor(TimeOut: Integer): Boolean;
begin
  Result := Wait(TimeOut);
end;

procedure TMonitorEvent.Clear;
begin
  FillChar(FEventData^, SizeOf(TEventData), #0);
end;

initialization
begin
  PSA := @SecurityAttributes;
  PSD := @SecurityDescriptor;
  with PSA^ do
  begin
    nLength := SizeOf(TSecurityAttributes);
    lpSecurityDescriptor := PSD;
    bInheritHandle := True;
  end;
  InitializeSecurityDescriptor(PSD, 1 {SECURITY_DESCRIPTOR_REVISION});
  SetSecurityDescriptorDacl(PSD, True, nil, False);
end;

end.
