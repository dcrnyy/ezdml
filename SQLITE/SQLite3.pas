unit SQLite3;

{
  Simplified interface for SQLite.
  Updated for Sqlite 3 by Tim Anderson (tim@itwriting.com)
  Note: NOT COMPLETE for version 3, just minimal functionality
  Adapted from file created by Pablo Pissanetzky (pablo@myhtpc.net)
  which was based on SQLite.pas by Ben Hochstrasser (bhoc@surfeu.ch)
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}            (* use AnsiString *)
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  Windows;

const
{$IF Defined(MSWINDOWS)}
  SQLiteDLL = 'sqlite3.dll';
{$ELSEIF Defined(DARWIN)}
  SQLiteDLL = 'libsqlite3.dylib';
  {$linklib libsqlite3}
{$ELSEIF Defined(UNIX)}
  SQLiteDLL = 'sqlite3.so';
{$IFEND}

// Return values for sqlite3_exec() and sqlite3_step()

const
  SQLITE_OK          =  0; // Successful result
  (* beginning-of-error-codes *)
  SQLITE_ERROR       =  1; // SQL error or missing database
  SQLITE_INTERNAL    =  2; // An internal logic error in SQLite
  SQLITE_PERM        =  3; // Access permission denied
  SQLITE_ABORT       =  4; // Callback routine requested an abort
  SQLITE_BUSY        =  5; // The database file is locked
  SQLITE_LOCKED      =  6; // A table in the database is locked
  SQLITE_NOMEM       =  7; // A malloc() failed
  SQLITE_READONLY    =  8; // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11; // The database disk image is malformed
  SQLITE_NOTFOUND    = 12; // (Internal Only) Table or record not found
  SQLITE_FULL        = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14; // Unable to open the database file
  SQLITE_PROTOCOL    = 15; // Database lock protocol error
  SQLITE_EMPTY       = 16; // Database is empty
  SQLITE_SCHEMA      = 17; // The database schema changed
  SQLITE_TOOBIG      = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT  = 19; // Abort due to contraint violation
  SQLITE_MISMATCH    = 20; // Data type mismatch
  SQLITE_MISUSE      = 21; // Library used incorrectly
  SQLITE_NOLFS       = 22; // Uses OS features not supported on host
  SQLITE_AUTH        = 23; // Authorization denied
  SQLITE_FORMAT      = 24; // Auxiliary database format error
  SQLITE_RANGE       = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26; // File opened that is not a database file
  SQLITE_ROW         = 100; // sqlite3_step() has another row ready
  SQLITE_DONE        = 101; // sqlite3_step() has finished executing

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  SQLITE_UTF8     = 1;
  SQLITE_UTF16    = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16LE  = 4;
  SQLITE_ANY      = 5;

  SQLITE_STATIC    {: TSQLite3Destructor} = Pointer(0);
  SQLITE_TRANSIENT {: TSQLite3Destructor} = Pointer(-1);

type
  TSQLiteDB = Pointer;
  TSQLiteResult = ^PAnsiChar;
  TSQLiteStmt = Pointer;
  TSQLiteBackup = pointer;

type
  PPAnsiCharArray = ^TPAnsiCharArray; 
  TPAnsiCharArray = array[0 .. (MaxInt div SizeOf(PAnsiChar))-1] of PAnsiChar;

type
  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues:
    PPAnsiCharArray; ColNames: PPAnsiCharArray): integer; cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  //function prototype for define own collate
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;

var
SQLite3_Open: function(filename: PAnsiChar; var db: TSQLiteDB): integer; cdecl;
SQLite3_Close: function(db: TSQLiteDB): integer; cdecl;
SQLite3_Exec: function(db: TSQLiteDB; SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; var ErrMsg: PAnsiChar): integer; cdecl;
SQLite3_Version: function(): PAnsiChar; cdecl;
SQLite3_ErrMsg: function(db: TSQLiteDB): PAnsiChar; cdecl;
SQLite3_ErrCode: function(db: TSQLiteDB): integer; cdecl;
SQlite3_Free: procedure(P: PAnsiChar); cdecl;
SQLite3_GetTable: function(db: TSQLiteDB; SQLStatement: PAnsiChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PAnsiChar): integer; cdecl;
SQLite3_FreeTable: procedure(Table: TSQLiteResult); cdecl;
SQLite3_Complete: function(P: PAnsiChar): boolean; cdecl;
SQLite3_LastInsertRowID: function(db: TSQLiteDB): int64; cdecl;
SQLite3_Interrupt: procedure(db: TSQLiteDB); cdecl;
SQLite3_BusyHandler: procedure(db: TSQLiteDB; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl;
SQLite3_BusyTimeout: procedure(db: TSQLiteDB; TimeOut: integer); cdecl;
SQLite3_Changes: function(db: TSQLiteDB): integer; cdecl;
SQLite3_TotalChanges: function(db: TSQLiteDB): integer; cdecl;
SQLite3_Prepare: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl;
SQLite3_Prepare_v2: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl;
SQLite3_ColumnCount: function(hStmt: TSqliteStmt): integer; cdecl;
SQLite3_ColumnName: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
SQLite3_ColumnDeclType: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
SQLite3_Step: function(hStmt: TSqliteStmt): integer; cdecl;
SQLite3_DataCount: function(hStmt: TSqliteStmt): integer; cdecl;

SQLite3_ColumnBlob: function(hStmt: TSqliteStmt; ColNum: integer): pointer; cdecl;
SQLite3_ColumnBytes: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
SQLite3_ColumnDouble: function(hStmt: TSqliteStmt; ColNum: integer): double; cdecl;
SQLite3_ColumnInt: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
SQLite3_ColumnText: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
SQLite3_ColumnType: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
SQLite3_ColumnInt64: function(hStmt: TSqliteStmt; ColNum: integer): Int64; cdecl;
SQLite3_Finalize: function(hStmt: TSqliteStmt): integer; cdecl;
SQLite3_Reset: function(hStmt: TSqliteStmt): integer; cdecl;

SQLite3_Backup_Init: function(DestDb: TSQLiteDB; DestDbName: PAnsiChar; SourceDb: TSQLiteDB; SourceDbName: PAnsiChar): TSqliteBackup; cdecl;
SQLite3_Backup_Step: function(hBackup: TSQLiteBackup; nPage: integer): integer; cdecl;
SQLite3_Backup_Finish: function(hBackup: TSQLiteBackup): integer; cdecl;
SQLite3_Backup_Remaining: function(hBackup: TSQLiteBackup): integer; cdecl;
SQLite3_Backup_Pagecount: function(hBackup: TSQLiteBackup): integer; cdecl;

// 
// In the SQL strings input to sqlite3_prepare() and sqlite3_prepare16(),
// one or more literals can be replace by a wildcard "?" or ":N:" where
// N is an integer.  These value of these wildcard literals can be set
// using the routines listed below.
// 
// In every case, the first parameter is a pointer to the sqlite3_stmt
// structure returned from sqlite3_prepare().  The second parameter is the
// index of the wildcard.  The first "?" has an index of 1.  ":N:" wildcards
// use the index N.
// 
// The fifth parameter to sqlite3_bind_blob(), sqlite3_bind_text(), and
//sqlite3_bind_text16() is a destructor used to dispose of the BLOB or
//text after SQLite has finished with it.  If the fifth argument is the
// special value SQLITE_STATIC, then the library assumes that the information
// is in static, unmanaged space and does not need to be freed.  If the
// fifth argument has the value SQLITE_TRANSIENT, then SQLite makes its
// own private copy of the data.
// 
// The sqlite3_bind_* routine must be called before sqlite3_step() after
// an sqlite3_prepare() or sqlite3_reset().  Unbound wildcards are interpreted
// as NULL.
// 

type
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;

var
sqlite3_bind_blob: function(hStmt: TSqliteStmt; ParamNum: integer; ptrData: pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
sqlite3_bind_text: function(hStmt: TSqliteStmt; ParamNum: integer; Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
sqlite3_bind_double: function(hStmt: TSqliteStmt; ParamNum: integer; Data: Double): integer; cdecl;
sqlite3_bind_int: function(hStmt: TSqLiteStmt; ParamNum: integer; Data: integer): integer; cdecl;
sqlite3_bind_int64: function(hStmt: TSqliteStmt; ParamNum: integer; Data: int64): integer; cdecl;
sqlite3_bind_null: function(hStmt: TSqliteStmt; ParamNum: integer): integer; cdecl;
sqlite3_bind_parameter_index: function(hStmt: TSqliteStmt; zName: PAnsiChar): integer; cdecl;
sqlite3_enable_shared_cache: function(Value: integer): integer; cdecl;
//user collate definiton
SQLite3_create_collation: function(db: TSQLiteDB; Name: PAnsiChar; eTextRep: integer; UserData: pointer; xCompare: TCollateXCompare): integer; cdecl;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): AnsiString;
function SQLiteErrorStr(SQLiteErrorCode: Integer): AnsiString;

procedure checkSqliteDllLoad;

var
  libsqlite_handle: HMODULE = 0;

implementation

uses
  SysUtils;

procedure checkSqliteDllLoad;
  procedure GetProc(var proc: pointer; name: PAnsiChar);
  begin
    proc := GetProcAddress(libsqlite_handle, name);
    if not Assigned(proc) then
      RaiseLastOsError;
  end;
var
  LastError: DWORD;
begin
  if libsqlite_handle<>0 then
    Exit;
  libsqlite_handle := LoadLibrary(SQLiteDLL);
  if libsqlite_handle = 0 then
  begin
    LastError := GetLastError;
    raise Exception.CreateFmt('Load library %s error: %d %s', [SQLiteDLL, LastError, SysErrorMessage(LastError)]);
  end;
    //raise Exception.Create('Failed to load sqlite dll');
  GetProc(@SQLite3_Open, 'sqlite3_open');
  GetProc(@SQLite3_Close, 'sqlite3_close');
  GetProc(@SQLite3_Exec, 'sqlite3_exec');
  GetProc(@SQLite3_Version, 'sqlite3_libversion');
  GetProc(@SQLite3_ErrMsg, 'sqlite3_errmsg');
  GetProc(@SQLite3_ErrCode, 'sqlite3_errcode');
  GetProc(@SQlite3_Free, 'sqlite3_free');
  GetProc(@SQLite3_GetTable, 'sqlite3_get_table');
  GetProc(@SQLite3_FreeTable, 'sqlite3_free_table');
  GetProc(@SQLite3_Complete, 'sqlite3_complete');
  GetProc(@SQLite3_LastInsertRowID, 'sqlite3_last_insert_rowid');
  GetProc(@SQLite3_Interrupt, 'sqlite3_interrupt');
  GetProc(@SQLite3_BusyHandler, 'sqlite3_busy_handler');
  GetProc(@SQLite3_BusyTimeout, 'sqlite3_busy_timeout');
  GetProc(@SQLite3_Changes, 'sqlite3_changes');
  GetProc(@SQLite3_TotalChanges, 'sqlite3_total_changes');
  GetProc(@SQLite3_Prepare, 'sqlite3_prepare');
  GetProc(@SQLite3_Prepare_v2, 'sqlite3_prepare_v2');
  GetProc(@SQLite3_ColumnCount, 'sqlite3_column_count');
  GetProc(@SQLite3_ColumnName, 'sqlite3_column_name');
  GetProc(@SQLite3_ColumnDeclType, 'sqlite3_column_decltype');
  GetProc(@SQLite3_Step, 'sqlite3_step');
  GetProc(@SQLite3_DataCount, 'sqlite3_data_count');
  GetProc(@SQLite3_ColumnBlob, 'sqlite3_column_blob');
  GetProc(@SQLite3_ColumnBytes, 'sqlite3_column_bytes');
  GetProc(@SQLite3_ColumnDouble, 'sqlite3_column_double');
  GetProc(@SQLite3_ColumnInt, 'sqlite3_column_int');
  GetProc(@SQLite3_ColumnText, 'sqlite3_column_text');
  GetProc(@SQLite3_ColumnType, 'sqlite3_column_type');
  GetProc(@SQLite3_ColumnInt64, 'sqlite3_column_int64');
  GetProc(@SQLite3_Finalize, 'sqlite3_finalize');
  GetProc(@SQLite3_Reset, 'sqlite3_reset');
  GetProc(@SQLite3_Backup_Init, 'sqlite3_backup_init');
  GetProc(@SQLite3_Backup_Step, 'sqlite3_backup_step');
  GetProc(@SQLite3_Backup_Finish, 'sqlite3_backup_finish');
  GetProc(@SQLite3_Backup_Remaining, 'sqlite3_backup_remaining');
  GetProc(@SQLite3_Backup_Pagecount, 'sqlite3_backup_pagecount');
  GetProc(@sqlite3_bind_blob, 'sqlite3_bind_blob');
  GetProc(@sqlite3_bind_text, 'sqlite3_bind_text');
  GetProc(@sqlite3_bind_double, 'sqlite3_bind_double');
  GetProc(@sqlite3_bind_int, 'sqlite3_bind_int');
  GetProc(@sqlite3_bind_int64, 'sqlite3_bind_int64');
  GetProc(@sqlite3_bind_null, 'sqlite3_bind_null');
  GetProc(@sqlite3_bind_parameter_index, 'sqlite3_bind_parameter_index');
  GetProc(@sqlite3_enable_shared_cache, 'sqlite3_enable_shared_cache');
  GetProc(@SQLite3_create_collation, 'sqlite3_create_collation');


end;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): AnsiString;
begin
  case SQLiteFieldTypeCode of
    SQLITE_INTEGER: Result := 'Integer';
    SQLITE_FLOAT: Result := 'Float';
    SQLITE_TEXT: Result := 'Text';
    SQLITE_BLOB: Result := 'Blob';
    SQLITE_NULL: Result := 'Null';
  else
    Result := 'Unknown SQLite Field Type Code "' + IntToStr(SQLiteFieldTypeCode) + '"';
  end;
end;

function SQLiteErrorStr(SQLiteErrorCode: Integer): AnsiString;
begin
  case SQLiteErrorCode of
    SQLITE_OK: Result := 'Successful result';
    SQLITE_ERROR: Result := 'SQL error or missing database';
    SQLITE_INTERNAL: Result := 'An internal logic error in SQLite';
    SQLITE_PERM: Result := 'Access permission denied';
    SQLITE_ABORT: Result := 'Callback routine requested an abort';
    SQLITE_BUSY: Result := 'The database file is locked';
    SQLITE_LOCKED: Result := 'A table in the database is locked';
    SQLITE_NOMEM: Result := 'A malloc() failed';
    SQLITE_READONLY: Result := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT: Result := 'Operation terminated by sqlite3_interrupt()';
    SQLITE_IOERR: Result := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT: Result := 'The database disk image is malformed';
    SQLITE_NOTFOUND: Result := '(Internal Only) Table or record not found';
    SQLITE_FULL: Result := 'Insertion failed because database is full';
    SQLITE_CANTOPEN: Result := 'Unable to open the database file';
    SQLITE_PROTOCOL: Result := 'Database lock protocol error';
    SQLITE_EMPTY: Result := 'Database is empty';
    SQLITE_SCHEMA: Result := 'The database schema changed';
    SQLITE_TOOBIG: Result := 'Too much data for one row of a table';
    SQLITE_CONSTRAINT: Result := 'Abort due to contraint violation';
    SQLITE_MISMATCH: Result := 'Data type mismatch';
    SQLITE_MISUSE: Result := 'Library used incorrectly';
    SQLITE_NOLFS: Result := 'Uses OS features not supported on host';
    SQLITE_AUTH: Result := 'Authorization denied';
    SQLITE_FORMAT: Result := 'Auxiliary database format error';
    SQLITE_RANGE: Result := '2nd parameter to sqlite3_bind out of range';
    SQLITE_NOTADB: Result := 'File opened that is not a database file';
    SQLITE_ROW: Result := 'sqlite3_step() has another row ready';
    SQLITE_DONE: Result := 'sqlite3_step() has finished executing';
  else
    Result := 'Unknown SQLite Error Code "' + IntToStr(SQLiteErrorCode) + '"';
  end;
end;

function ColValueToStr(Value: PAnsiChar): AnsiString;
begin
  if (Value = nil) then
    Result := 'NULL'
  else
    Result := Value;
end;


end.


