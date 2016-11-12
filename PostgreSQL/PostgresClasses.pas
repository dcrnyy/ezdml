{ Vladimir Klimov 1.0
  classes for Postgres data access 
  wintarif@narod.ru
}
   
unit PostgresClasses;

interface

uses Windows, SysUtils, libpq_fe, postgres_ext, SyncObjs;

const
  DEFAULT_PORT = '5432';
  DEFAULT_HOST = 'localhost';
type
  TNotifyEvent = procedure(Sender: TObject) of object;

  IPostgresQuery = interface;
  IPostgresStmt = interface;

  PParams = ^TParam;
  TParam = record
    nParams:longint;
    paramTypes: POid;
    paramValues: PPChars;
    paramLengths: PIntegers;
    paramFormats: PIntegers;
    resultFormat:longint;
  end;

  IPostgres = interface(IInterface)
    procedure Lock;
    procedure Unlock;
    function IsConnected: Boolean;
    function GetConnection: PPGconn;
    function GetDatabase: string;
    function GetError: string;     
    function GetErrorUtf8: string;

    function Connect: Boolean;
    procedure Disconnect;
    function ExecQuery(const SQL: String): IPostgresQuery;
    function ExecQueryUtf8(const SQL: String): IPostgresQuery;
    function ExecQueryParams(const SQL: String; params: PParams): IPostgresQuery;
    function SendQuery(const SQL: String): boolean;
    function Prepare(const SQL, StmtName: string; params: PParams): IPostgresStmt;
    function ExecQueryPrepared(const StmtName: string; params: PParams): IPostgresQuery;
    function GetLastErrorString: string;

    property Connection: PPGconn read GetConnection;
    property LastErrorString: string read GetLastErrorString;
  end;

  TPostgres = class(TInterfacedObject, IPostgres)
  private
    FConnected: boolean;
    FConnection: PPGconn;
    FPort: string;
    FPassword: string;
    FDatabase: string;
    FHost: string;
    FUser: string;
    FCS: TCriticalSection;
    FLastErrorString: string;

    { IPostgres }
    function IsConnected: Boolean;
    function GetConnection: PPGconn;
    function GetDatabase: string;
    function GetError: string;
    function GetLastErrorString: string;     
    function GetErrorUtf8: string;
  public
    constructor Create(const host, port, database, user, password: string); virtual;
    destructor Destroy; override;

    { IPostgres }
    procedure Lock;
    procedure Unlock;
    function Connect: Boolean; virtual;
    procedure Disconnect; virtual;
    function ExecQuery(const SQL: String): IPostgresQuery; virtual;
    function ExecQueryUtf8(const SQL: String): IPostgresQuery; virtual;
    function ExecQueryParams(const SQL: String; params: PParams): IPostgresQuery; virtual;
    function SendQuery(const SQL: String): boolean; virtual;
    function Prepare(const SQL, StmtName: string; params: PParams): IPostgresStmt; virtual;
    function ExecQueryPrepared(const StmtName: string; params: PParams): IPostgresQuery; virtual;
    property Connection: PPGconn read GetConnection;
    property Database: string read FDatabase;
    property LastErrorString: string read GetLastErrorString;
  end;

  IPostgresQuery = interface(IInterface)
    function GetQueryStatus: ExecStatusType;
    function GetQueryStatusStr: string;
    function GetRecordCount: Integer;
    function GetFieldCount: Integer;
    function GetValue(row, field: integer): PChar;
    function GetValueLen(row, field: integer): integer;
    function GetFieldIndex(const fname: string): integer;
    function GetValueIsNull(row, field: integer): boolean;
    function GetFieldName(field: Integer): PChar;
    function FieldAsStringUtf8(row, field: integer): String;
    function FieldAsInteger(row, field: integer): Integer;

    property Status: ExecStatusType read GetQueryStatus;
    property StatusStr: string read GetQueryStatusStr;
    property FieldCount: Integer read GetFieldCount;
    property RecordCount: Integer read GetRecordCount;
    property Value[row, field: Integer]: PChar read GetValue;
    property ValueLen[row, field: Integer]: integer read GetValueLen;
    property FieldIndex[const fname: string]: integer read GetFieldIndex;
    property ValueIsNull[row, field: Integer]: boolean read GetValueIsNull;
  end;

  TPostgresQuery = class(TInterfacedObject, IPostgresQuery)
  private
    FPPGresult: PPGresult;
    FParams: PParams;
    function GetQueryStatus: ExecStatusType;
    function GetQueryStatusStr: string;
    function GetRecordCount: Integer;
    function GetFieldCount: Integer;
    function GetValue(row, field: integer): PChar;
    function GetValueLen(row, field: integer): integer;
    function GetFieldIndex(const fname: string): integer;
    function GetValueIsNull(row, field: integer): boolean;  
    function GetFieldName(field: Integer): PChar;
    function FieldAsStringUtf8(row, field: integer): String;
    function FieldAsInteger(row, field: integer): Integer;
  public
    constructor Create(APostgres: IPostgres; res: PPGresult; params: PParams);
    destructor Destroy; override;

    property Status: ExecStatusType read GetQueryStatus;
    property StatusStr: string read GetQueryStatusStr;
    property FieldCount: Integer read GetFieldCount;
    property RecordCount: Integer read GetRecordCount;
    property Value[row, field: Integer]: PChar read GetValue;
    property ValueLen[row, field: Integer]: integer read GetValueLen;
    property FieldIndex[const fname: string]: integer read GetFieldIndex;
    property ValueIsNull[row, field: Integer]: boolean read GetValueIsNull;
    //property FieldName[Index: Integer]: string read GetFieldName; todo
    //property ValueByName[const FieldName: string]: string read GetValueByName; todo
  end;

  IPostgresStmt = interface(IInterface)
    function GetQueryStatus: ExecStatusType;
    function GetQueryStatusStr: string;
    function GetStmtName: string;

    property Status: ExecStatusType read GetQueryStatus;
    property StatusStr: string read GetQueryStatusStr;
    property StmtName: string read GetStmtName;
  end;

  TPostgresStmt = class(TInterfacedObject, IPostgresStmt)
  private
    FPPGresult: PPGresult;
    FParams: PParams;
    FStmtName: string;
    function GetQueryStatus: ExecStatusType;
    function GetQueryStatusStr: string;
    function GetStmtName: string;
  public
    constructor Create(APostgres: IPostgres; const StmtName: string; res: PPGresult; params: PParams);
    destructor Destroy; override;

    property Status: ExecStatusType read GetQueryStatus;
    property StatusStr: string read GetQueryStatusStr;
    property StmtName: string read GetStmtName;
  end;

implementation

{ TPostgres }

constructor TPostgres.Create(const host, port, database, user, password: string);
begin
  inherited Create;
  FConnected := false;
  if length(host) = 0 then FHost := DEFAULT_HOST;
  if length(port) = 0 then FPort := DEFAULT_PORT;
  FDatabase:= database;
  FUser:= user;
  FPassword:= password;
  FCS := TCriticalSection.Create;
end;

destructor TPostgres.Destroy;
begin
  Disconnect;
  FCS.Free;
  inherited;
end;

procedure TPostgres.Lock;
begin
    FCS.Enter;
end;

procedure TPostgres.Unlock;
begin
    FCS.Leave;
end;

function TPostgres.Connect: Boolean;
begin
    FConnection := PQsetdbLogin(PChar(FHost), PChar(FPort), nil, nil, PChar(FDatabase), PChar(FUser), PChar(FPassword));
    FConnected := PQstatus(FConnection) = CONNECTION_OK;
    result:= FConnected;
    if not(FConnected) then begin
      FLastErrorString:= GetError;
      Disconnect;
    end;
end;

procedure TPostgres.Disconnect;
begin
    if FConnection = nil then exit;
    FConnected:= false;
    PQfinish(FConnection);
    FConnection:= nil;
end;

function TPostgres.IsConnected: Boolean;
begin
    Result := FConnected;
end;

function TPostgres.GetDatabase: string;
begin
    Result := FDatabase;
end;

function TPostgres.GetError: string;
begin
    result:= PQerrorMessage(FConnection);
    result:= Copy(result, 1, length(result) - 1);
end;

function TPostgres.GetErrorUtf8: string;
begin
  Result := GetError;
  Result := Utf8Decode(Result);
end;

function TPostgres.GetLastErrorString: string;
begin
    result:= FLastErrorString;
end;

function TPostgres.GetConnection: PPGconn;
begin
    Result := FConnection;
end;

function TPostgres.SendQuery(const SQL: String): boolean;
begin
    result:= PQsendQuery(FConnection, PChar(SQL)) <> 0;
end;

function TPostgres.ExecQuery(const SQL: String): IPostgresQuery;
var
  pr: PPGresult;
begin
    result := nil;
    if not(FConnected) then exit;

    pr := PQexec(FConnection, PChar(SQL));
    result:= TPostgresQuery.Create(self, pr, nil);
end;

function TPostgres.ExecQueryParams(const SQL: String; params: PParams): IPostgresQuery;
var
  pr: PPGresult;
begin
    result := nil;
    if not(FConnected) then exit;

    pr := PQexecParams(FConnection, PChar(SQL), params.nParams, params.paramTypes, params.paramValues, params.paramLengths, params.paramFormats, params.resultFormat);
    result:= TPostgresQuery.Create(self, pr, params);
end;

function TPostgres.Prepare(const SQL, StmtName: string; params: PParams): IPostgresStmt;
var
  pr: PPGresult;
begin
    result := nil;
    if not(FConnected) then exit;

    pr := PQprepare(FConnection, PChar(StmtName), PChar(SQL), params.nParams, params.paramTypes);
    result:= TPostgresStmt.Create(self, StmtName, pr, params);
end;

function TPostgres.ExecQueryPrepared(const StmtName: string; params: PParams): IPostgresQuery;
var
  pr: PPGresult;
begin
    result := nil;
    if not(FConnected) then exit;

    pr := PQexecPrepared(FConnection, PChar(StmtName), params.nParams, params.paramValues, params.paramLengths, params.paramFormats, params.resultFormat);
    result:= TPostgresQuery.Create(self, pr, params);
end;

function TPostgres.ExecQueryUtf8(const SQL: String): IPostgresQuery;
begin
  Result := ExecQuery(Utf8Encode(SQL));
end;

{ TPostgresQuery }

constructor TPostgresQuery.Create(APostgres: IPostgres; res: PPGresult; params: PParams);
begin
    FPPGresult := res;
    FParams:= params;
end;

destructor TPostgresQuery.Destroy;
begin
    PQclear(FPPGresult);
    inherited;
end;

function TPostgresQuery.FieldAsInteger(row, field: integer): Integer;
begin
  Result := StrToIntDef(Value[row, field],0);
end;

function TPostgresQuery.FieldAsStringUtf8(row, field: integer): String;
begin
  Result := Value[row, field];
  Result := Utf8Decode(Result);
end;

function TPostgresQuery.GetQueryStatus: ExecStatusType;
begin
    result := PQresultStatus(FPPGresult);
end;

function TPostgresQuery.GetQueryStatusStr: string;
begin
    result := PQresStatus(PQresultStatus(FPPGresult));
end;

function TPostgresQuery.GetRecordCount: Integer;
begin
    result := PQntuples(FPPGresult);
end;

function TPostgresQuery.GetFieldCount: Integer;
begin
    result := PQnfields(FPPGresult);
end;

function TPostgresQuery.GetValue(row, field: integer): PChar;
begin
    result:= PQgetvalue(FPPGresult, row, field);
end;

function TPostgresQuery.GetValueLen(row, field: integer): integer;
begin
    result:= PQgetlength(FPPGresult, row, field);
end;

function TPostgresQuery.GetFieldIndex(const fname: string): integer;
begin
    result:= PQfnumber(FPPGresult, PAnsiChar(fname));
end;

function TPostgresQuery.GetFieldName(field: Integer): PChar;
begin
    result:= PQfname(FPPGresult, field);
end;

function TPostgresQuery.GetValueIsNull(row, field: integer): boolean;
begin
    result:= PQgetisnull(FPPGresult, row, field) <> 0;
end;

{ TPostgresStmt }

constructor TPostgresStmt.Create(APostgres: IPostgres; const StmtName: string; res: PPGresult; params: PParams);
begin
    FPPGresult := res;
    FParams:= params;
    FStmtName:= StmtName;
end;

destructor TPostgresStmt.Destroy;
begin
    PQclear(FPPGresult);
    inherited;
end;

function TPostgresStmt.GetQueryStatus: ExecStatusType;
begin
    result := PQresultStatus(FPPGresult);
end;

function TPostgresStmt.GetQueryStatusStr: string;
begin
    result := PQresStatus(PQresultStatus(FPPGresult));
end;

function TPostgresStmt.GetStmtName: string;
begin
    result:= FStmtName;
end;

end{$WARNINGS OFF}.

