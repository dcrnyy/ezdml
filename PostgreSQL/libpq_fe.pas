
unit libpq_fe;
interface

uses Windows, SysUtils, postgres_ext;

{
  Automatically converted by H2Pas 1.0.0 from libpq-fe.h
  The following command line parameters were used:
    -c
    -p
    libpq-fe.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
{$ifndef PLongint}
  PLongint  = ^Longint;
{$endif}
{$ifndef PSmallInt}
  PSmallInt = ^SmallInt;
{$endif}
{$ifndef PByte}
  PByte     = ^Byte;
{$endif}
{$ifndef PWord}
  PWord     = ^Word;
{$endif}
{$ifndef PDWord}
  PDWord    = ^DWord;
{$endif}
{$ifndef PDouble}
  PDouble   = ^Double;
{$endif}
  PFILE  = ^FILE;
  size_t = dword;
  Psize_t  = ^size_t;
  PIntegers = array of integer;
  PPChars = array of PChar;

{Type
P_PQconninfoOption  = ^_PQconninfoOption;
P_PQprintOpt  = ^_PQprintOpt;
PConnStatusType  = ^ConnStatusType;
PExecStatusType  = ^ExecStatusType;
PFILE  = ^FILE;
POid  = ^Oid;
PPGcancel  = ^PGcancel;
PPGconn  = ^PGconn;
PpgNotify  = ^pgNotify;
PpgresAttDesc  = ^pgresAttDesc;
PPGresult  = ^PGresult;
PPGTransactionStatusType  = ^PGTransactionStatusType;
PPGVerbosity  = ^PGVerbosity;
PPostgresPollingStatusType  = ^PostgresPollingStatusType;
PPQArgBlock  = ^PQArgBlock;
Ppqbool  = ^pqbool;
PPQconninfoOption  = ^PQconninfoOption;
PPQprintOpt  = ^PQprintOpt;
Psize_t  = ^size_t;}


{-------------------------------------------------------------------------
 *
 * libpq-fe.h
 *	  This file contains definitions for structures and
 *	  externs for functions used by frontend postgres applications.
 *
 * Portions Copyright (c) 1996-2009, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 *
 * $PostgreSQL: pgsql/src/interfaces/libpq/libpq-fe.h,v 1.147 2009/06/11 14:49:14 momjian Exp $
 *
 *-------------------------------------------------------------------------
  }
{
 * Option flags for PQcopyResult
  }

const
   libpq                      = 'libpq.dll';

   PG_COPYRES_ATTRS = $01;
{ Implies PG_COPYRES_ATTRS  }
   PG_COPYRES_TUPLES = $02;   
   PG_COPYRES_EVENTS = $04;   
   PG_COPYRES_NOTICEHOOKS = $08;   
{ Application-visible enum types  }
{
	 * Although it is okay to add to this list, values which become unused
	 * should never be removed, nor should constants be redefined - that would
	 * break compatibility with existing code.
	  }
{ Non-blocking mode only below here  }
{
	 * The existence of these should never be relied upon - they should only
	 * be used for user feedback or similar purposes.
	  }
{ Waiting for connection to be made.   }
{ Connection OK; waiting to send.	    }
{ Waiting for a response from the
										 * postmaster.		   }
{ Received authentication; waiting for
								 * backend startup.  }
{ Negotiating environment.  }
{ Negotiating SSL.  }
{ Internal state: connect() needed  }

{ Error when no password was given.  }
{ Note: depending on this is deprecated; use PQconnectionNeedsPassword().  }

   PQnoPasswordSupplied = 'fe_sendauth: no password supplied\n';
type

   PConnStatusType = ^ConnStatusType;
   ConnStatusType = (CONNECTION_OK,CONNECTION_BAD,CONNECTION_STARTED,
     CONNECTION_MADE,CONNECTION_AWAITING_RESPONSE,
     CONNECTION_AUTH_OK,CONNECTION_SETENV,CONNECTION_SSL_STARTUP,
     CONNECTION_NEEDED);
{ These two indicate that one may	   }
{ use select before polling again.    }
{ unused; keep for awhile for backwards
								 * compatibility  }

   PPostgresPollingStatusType = ^PostgresPollingStatusType;
   PostgresPollingStatusType = (PGRES_POLLING_FAILED = 0,PGRES_POLLING_READING,
     PGRES_POLLING_WRITING,PGRES_POLLING_OK,
     PGRES_POLLING_ACTIVE);
{ empty query string was executed  }
{ a query command that doesn't return
								 * anything was executed properly by the
								 * backend  }
{ a query command that returns tuples was
								 * executed properly by the backend, PGresult
								 * contains the result tuples  }
{ Copy Out data transfer in progress  }
{ Copy In data transfer in progress  }
{ an unexpected response was recv'd from the
								 * backend  }
{ notice or warning message  }
{ query failed  }

   PExecStatusType = ^ExecStatusType;
   ExecStatusType = (PGRES_EMPTY_QUERY = 0,PGRES_COMMAND_OK,
     PGRES_TUPLES_OK,PGRES_COPY_OUT,PGRES_COPY_IN,
     PGRES_BAD_RESPONSE,PGRES_NONFATAL_ERROR,
     PGRES_FATAL_ERROR);
{ connection idle  }
{ command in progress  }
{ idle, within transaction block  }
{ idle, within failed transaction  }
{ cannot determine status  }

   PPGTransactionStatusType = ^PGTransactionStatusType;
   PGTransactionStatusType = (PQTRANS_IDLE,PQTRANS_ACTIVE,PQTRANS_INTRANS,
     PQTRANS_INERROR,PQTRANS_UNKNOWN);
{ single-line error messages  }
{ recommended style  }
{ all the facts, ma'am  }

   PPGVerbosity = ^PGVerbosity;
   PGVerbosity = (PQERRORS_TERSE,PQERRORS_DEFAULT,PQERRORS_VERBOSE
     );
{ PGconn encapsulates a connection to the backend.
 * The contents of this struct are not supposed to be known to applications.
  }
   //pg_conn = PGconn;
   PPGconn = pointer;
{ PGresult encapsulates the result of a query (or more precisely, of a single
 * SQL command --- a query string given to PQsendQuery can contain multiple
 * commands and thus return multiple PGresult objects).
 * The contents of this struct are not supposed to be known to applications.
  }
   //pg_result = PGresult;
   PPGresult = pointer;
{ PGcancel encapsulates the information needed to cancel a running
 * query on an existing connection.
 * The contents of this struct are not supposed to be known to applications.
  }
   //pg_cancel = PGcancel;
   PPGcancel = pointer;
{ PGnotify represents the occurrence of a NOTIFY message.
 * Ideally this would be an opaque typedef, but it's so simple that it's
 * unlikely to change.
 * NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
 * whereas in earlier versions it was always your own backend's PID.
  }
{ notification condition name  }
{ process ID of notifying server process  }
{ notification parameter  }
{ Fields below here are private to libpq; apps should not use 'em  }
{ list link  }

   PpgNotify = ^pgNotify;
   pgNotify = record
        relname : PChar;
        be_pid : longint;
        extra : PChar;
        next : PpgNotify;
     end;
{ Function types for notice-handling callbacks  }


   PQnoticeReceiver = procedure (arg:pointer; res:PPGresult); cdecl;


   PQnoticeProcessor = procedure (arg:pointer; message:PChar); cdecl;
{ Print options for PQprint()  }

   Ppqbool = ^pqbool;
   pqbool = char;
{ print output field headings and row count  }
{ fill align the fields  }
{ old brain dead format  }
{ output html tables  }
{ expand tables  }
{ use pager for output if needed  }
{ field separator  }
{ insert to HTML <table ...>  }
{ HTML <caption>  }
{ null terminated array of replacement field
								 * names  }

   P_PQprintOpt = ^_PQprintOpt;
   _PQprintOpt = record
        header : pqbool;
        align : pqbool;
        standard : pqbool;
        html3 : pqbool;
        expanded : pqbool;
        pager : pqbool;
        fieldSep : PChar;
        tableOpt : PChar;
        caption : PChar;
        fieldName : ^PChar;
     end;
   PQprintOpt = _PQprintOpt;
   PPQprintOpt = ^PQprintOpt;
{ ----------------
 * Structure for the conninfo parameter definitions returned by PQconndefaults
 * or PQconninfoParse.
 *
 * All fields except "val" point at static strings which must not be altered.
 * "val" is either NULL or a malloc'd current-value string.  PQconninfoFree()
 * will release both the val strings and the PQconninfoOption array itself.
 * ----------------
  }
{ The keyword of the option			 }
{ Fallback environment variable name	 }
{ Fallback compiled in default value	 }
{ Option's current value, or NULL		  }
{ Label for field in connect dialog	 }
{ Indicates how to display this field in a
								 * connect dialog. Values are: "" Display
								 * entered value as is "*" Password field -
								 * hide value "D"  Debug option - don't show
								 * by default  }
{ Field size in characters for dialog	 }

   P_PQconninfoOption = ^_PQconninfoOption;
   _PQconninfoOption = record
        keyword : PChar;
        envvar : PChar;
        compiled : PChar;
        val : PChar;
        _label : PChar;
        dispchar : PChar;
        dispsize : longint;
     end;
   PQconninfoOption = _PQconninfoOption;
   PPQconninfoOption = ^PQconninfoOption;
{ ----------------
 * PQArgBlock -- structure for PQfn() arguments
 * ----------------
  }
{ can't use void (dec compiler barfs)	  }

   PPQArgBlock = ^PQArgBlock;
   PQArgBlock = record
        len : longint;
        isint : longint;
        u : record
            case longint of
               0 : ( ptr : Plongint );
               1 : ( integer : longint );
            end;
     end;
{ ----------------
 * PGresAttDesc -- Data about a single attribute (column) of a query result
 * ----------------
  }
{ column name  }
{ source table, if known  }
{ source column, if known  }
{ format code for value (text/binary)  }
{ type id  }
{ type size  }
{ type-specific modifier info  }

   PpgresAttDesc = ^pgresAttDesc;
   pgresAttDesc = record
        name : PChar;
        tableid : Oid;
        columnid : longint;
        format : longint;
        typid : Oid;
        typlen : longint;
        atttypmod : longint;
     end;

   pgthreadlock_t = procedure (acquire:longint); cdecl;

   { Exported functions of libpq }
{ ===	in fe-connect.c ===  }
{ make a new client connection to the backend  }
{ Asynchronous (non-blocking)  }


function PQsetdb(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME : PAnsiChar) : PPGconn;

procedure PQfreeNotify(ptr : pointer);


var
PQconnectStart: function(conninfo:PChar):PPGconn; cdecl;
PQconnectPoll: function(conn:PPGconn):PostgresPollingStatusType; cdecl;
PQconnectdb: function(conninfo:PChar):PPGconn; cdecl;
PQsetdbLogin: function(pghost:PChar; pgport:PChar; pgoptions:PChar; pgtty:PChar; dbName:PChar; login:PChar; pwd:PChar):PPGconn; cdecl;
PQfinish: procedure(conn:PPGconn); cdecl;
PQconndefaults: function:PPQconninfoOption; cdecl;
PQconninfoParse: function(conninfo:PChar; errmsg:PPchar):PPQconninfoOption; cdecl;
PQconninfoFree: procedure(connOptions:PPQconninfoOption); cdecl;
PQresetStart: function(conn:PPGconn):longint; cdecl;
PQresetPoll: function(conn:PPGconn):PostgresPollingStatusType; cdecl;
PQreset: procedure(conn:PPGconn); cdecl;
PQgetCancel: function(conn:PPGconn):PPGcancel; cdecl;
PQfreeCancel: procedure(cancel:PPGcancel); cdecl;
PQcancel: function(cancel:PPGcancel; errbuf:PChar; errbufsize:longint):longint; cdecl;
PQrequestCancel: function(conn:PPGconn):longint; cdecl;
PQdb: function(conn:PPGconn):PChar; cdecl;
PQuser: function(conn:PPGconn):PChar; cdecl;
PQpass: function(conn:PPGconn):PChar; cdecl;
PQhost: function(conn:PPGconn):PChar; cdecl;
PQport: function(conn:PPGconn):PChar; cdecl;
PQtty: function(conn:PPGconn):PChar; cdecl;
PQoptions: function(conn:PPGconn):PChar; cdecl;
PQstatus: function(conn:PPGconn):ConnStatusType; cdecl;
PQtransactionStatus: function(conn:PPGconn):PGTransactionStatusType; cdecl;
PQparameterStatus: function(conn:PPGconn; paramName:PChar):PChar; cdecl;
PQprotocolVersion: function(conn:PPGconn):longint; cdecl;
PQserverVersion: function(conn:PPGconn):longint; cdecl;
PQerrorMessage: function(conn:PPGconn):PChar; cdecl;
PQsocket: function(conn:PPGconn):longint; cdecl;
PQbackendPID: function(conn:PPGconn):longint; cdecl;
PQconnectionNeedsPassword: function(conn:PPGconn):longint; cdecl;
PQconnectionUsedPassword: function(conn:PPGconn):longint; cdecl;
PQclientEncoding: function(conn:PPGconn):longint; cdecl;
PQsetClientEncoding: function(conn:PPGconn; encoding:PChar):longint; cdecl;
PQgetssl: function(conn:PPGconn):pointer; cdecl;
PQinitSSL: procedure(do_init:longint); cdecl;
PQinitOpenSSL: procedure(do_ssl:longint; do_crypto:longint); cdecl;
PQsetErrorVerbosity: function(conn:PPGconn; verbosity:PGVerbosity):PGVerbosity; cdecl;
PQtrace: procedure(conn:PPGconn; debug_port:PFILE); cdecl;
PQuntrace: procedure(conn:PPGconn); cdecl;
PQsetNoticeReceiver: function(conn:PPGconn; proc:PQnoticeReceiver; arg:pointer):PQnoticeReceiver; cdecl;
PQsetNoticeProcessor: function(conn:PPGconn; proc:PQnoticeProcessor; arg:pointer):PQnoticeProcessor; cdecl;
PQregisterThreadLock: function(newhandler:pgthreadlock_t):pgthreadlock_t; cdecl;
PQexec: function(conn:PPGconn; query:PChar):PPGresult; cdecl;
PQexecParams: function(conn:PPGconn; command:PChar; nParams:longint; paramTypes:POid; paramValues: PPChars; paramLengths: PIntegers; paramFormats: PIntegers; resultFormat:longint):PPGresult; cdecl;
PQprepare: function(conn:PPGconn; stmtName:PChar; query:PChar; nParams:longint; paramTypes:POid):PPGresult; cdecl;
PQexecPrepared: function(conn:PPGconn; stmtName:PChar; nParams:longint; paramValues: PPChars; paramLengths:PIntegers; paramFormats:PIntegers; resultFormat:longint):PPGresult; cdecl;
PQsendQuery: function(conn:PPGconn; query:PChar):longint; cdecl;
PQsendQueryParams: function(conn:PPGconn; command:PChar; nParams:longint; paramTypes:POid; paramValues: PPChars; paramLengths:Plongint; paramFormats:Plongint; resultFormat:longint):longint; cdecl;
PQsendPrepare: function(conn:PPGconn; stmtName:PChar; query:PChar; nParams:longint; paramTypes:POid):longint; cdecl;
PQsendQueryPrepared: function(conn:PPGconn; stmtName:PChar; nParams:longint; paramValues: PPChars; paramLengths:Plongint; paramFormats:Plongint; resultFormat:longint):longint; cdecl;
PQgetResult: function(conn:PPGconn):PPGresult; cdecl;
PQisBusy: function(conn:PPGconn):longint; cdecl;
PQconsumeInput: function(conn:PPGconn):longint; cdecl;
PQnotifies: function(conn:PPGconn):PPGnotify; cdecl;
PQputCopyData: function(conn:PPGconn; buffer:PChar; nbytes:longint):longint; cdecl;
PQputCopyEnd: function(conn:PPGconn; errormsg:PChar):longint; cdecl;
PQgetCopyData: function(conn:PPGconn; buffer:PPchar; async:longint):longint; cdecl;
PQgetline: function(conn:PPGconn; _string:PChar; length:longint):longint; cdecl;
PQputline: function(conn:PPGconn; _string:PChar):longint; cdecl;
PQgetlineAsync: function(conn:PPGconn; buffer:PChar; bufsize:longint):longint; cdecl;
PQputnbytes: function(conn:PPGconn; buffer:PChar; nbytes:longint):longint; cdecl;
PQendcopy: function(conn:PPGconn):longint; cdecl;
PQsetnonblocking: function(conn:PPGconn; arg:longint):longint; cdecl;
PQisnonblocking: function(conn:PPGconn):longint; cdecl;
PQisthreadsafe: function:longint; cdecl;
PQflush: function(conn:PPGconn):longint; cdecl;
PQfn: function(conn:PPGconn; fnid:longint; result_buf:Plongint; result_len:Plongint; result_is_int:longint; args:PPQArgBlock; nargs:longint):PPGresult; cdecl;
PQresultStatus: function(res:PPGresult):ExecStatusType; cdecl;
PQresStatus: function(status:ExecStatusType):PChar; cdecl;
PQresultErrorMessage: function(res:PPGresult):PChar; cdecl;
PQresultErrorField: function(res:PPGresult; fieldcode:longint):PChar; cdecl;
PQntuples: function(res:PPGresult):longint; cdecl;
PQnfields: function(res:PPGresult):longint; cdecl;
PQbinaryTuples: function(res:PPGresult):longint; cdecl;
PQfname: function(res:PPGresult; field_num:longint):PChar; cdecl;
PQfnumber: function(res:PPGresult; field_name:PChar):longint; cdecl;
PQftable: function(res:PPGresult; field_num:longint):Oid; cdecl;
PQftablecol: function(res:PPGresult; field_num:longint):longint; cdecl;
PQfformat: function(res:PPGresult; field_num:longint):longint; cdecl;
PQftype: function(res:PPGresult; field_num:longint):Oid; cdecl;
PQfsize: function(res:PPGresult; field_num:longint):longint; cdecl;
PQfmod: function(res:PPGresult; field_num:longint):longint; cdecl;
PQcmdStatus: function(res:PPGresult):PChar; cdecl;
PQoidStatus: function(res:PPGresult):PChar; cdecl;
PQoidValue: function(res:PPGresult):Oid; cdecl;
PQcmdTuples: function(res:PPGresult):PChar; cdecl;
PQgetvalue: function(res:PPGresult; tup_num:longint; field_num:longint):PChar; cdecl;
PQgetlength: function(res:PPGresult; tup_num:longint; field_num:longint):longint; cdecl;
PQgetisnull: function(res:PPGresult; tup_num:longint; field_num:longint):longint; cdecl;
PQnparams: function(res:PPGresult):longint; cdecl;
PQparamtype: function(res:PPGresult; param_num:longint):Oid; cdecl;
PQdescribePrepared: function(conn:PPGconn; stmt:PChar):PPGresult; cdecl;
PQdescribePortal: function(conn:PPGconn; portal:PChar):PPGresult; cdecl;
PQsendDescribePrepared: function(conn:PPGconn; stmt:PChar):longint; cdecl;
PQsendDescribePortal: function(conn:PPGconn; portal:PChar):longint; cdecl;
PQclear: procedure(res:PPGresult); cdecl;
PQfreemem: procedure(ptr:pointer); cdecl;
PQmakeEmptyPGresult: function(conn:PPGconn; status:ExecStatusType):PPGresult; cdecl;
PQcopyResult: function(src:PPGresult; flags:longint):PPGresult; cdecl;
PQsetResultAttrs: function(res:PPGresult; numAttributes:longint; attDescs:PPGresAttDesc):longint; cdecl;
PQresultAlloc: function(res:PPGresult; nBytes:size_t):pointer; cdecl;
PQsetvalue: function(res:PPGresult; tup_num:longint; field_num:longint; value:PChar; len:longint):longint; cdecl;
PQescapeStringConn: function(conn:PPGconn; c_to:PChar; from:PChar; length:size_t; error:Plongint):size_t; cdecl;
PQescapeByteaConn: function(conn:PPGconn; from:PByte; from_length:size_t; to_length:Psize_t):PByte; cdecl;
PQunescapeBytea: function(strtext:PByte; retbuflen:Psize_t):PByte; cdecl;
PQescapeString: function(c_to:PChar; from:PChar; length:size_t):size_t; cdecl;
PQescapeBytea: function(from:PByte; from_length:size_t; to_length:Psize_t):PByte; cdecl;
PQprint: procedure(fout:PFILE; res:PPGresult; ps:PPQprintOpt); cdecl;
PQdisplayTuples: procedure(res:PPGresult; fp:PFILE; fillAlign:longint; fieldSep:PChar; printHeader:longint; quiet:longint); cdecl;
PQprintTuples: procedure(res:PPGresult; fout:PFILE; printAttName:longint; terseOutput:longint; width:longint); cdecl;
lo_open: function(conn:PPGconn; lobjId:Oid; mode:longint):longint; cdecl;
lo_close: function(conn:PPGconn; fd:longint):longint; cdecl;
lo_read: function(conn:PPGconn; fd:longint; buf:PChar; len:size_t):longint; cdecl;
lo_write: function(conn:PPGconn; fd:longint; buf:PChar; len:size_t):longint; cdecl;
lo_lseek: function(conn:PPGconn; fd:longint; offset:longint; whence:longint):longint; cdecl;
lo_creat: function(conn:PPGconn; mode:longint):Oid; cdecl;
lo_create: function(conn:PPGconn; lobjId:Oid):Oid; cdecl;
lo_tell: function(conn:PPGconn; fd:longint):longint; cdecl;
lo_truncate: function(conn:PPGconn; fd:longint; len:size_t):longint; cdecl;
lo_unlink: function(conn:PPGconn; lobjId:Oid):longint; cdecl;
lo_import: function(conn:PPGconn; filename:PChar):Oid; cdecl;
lo_import_with_oid: function(conn:PPGconn; filename:PChar; lobjId:Oid):Oid; cdecl;
lo_export: function(conn:PPGconn; lobjId:Oid; filename:PChar):longint; cdecl;
PQmblen: function(s:PChar; encoding:longint):longint; cdecl;
PQdsplen: function(s:PChar; encoding:longint):longint; cdecl;
PQenv2encoding: function():longint; cdecl;
PQencryptPassword: function(passwd:PChar; user:PChar):PChar; cdecl;
pg_char_to_encoding: function(name:PChar):longint; cdecl;
pg_encoding_to_char: function(encoding:longint):PChar; cdecl;
pg_valid_server_encoding_id: function(encoding:longint):longint; cdecl;


procedure CheckPostgreSqlDllLoad;

var
  libpostgresql_handle: HMODULE = 0;


implementation




function PQsetdb(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME : PAnsiChar) : PPGconn;
begin
   result:=PQsetdbLogin(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME,nil,nil);
end;

procedure PQfreeNotify(ptr : pointer);
begin
   PQfreemem(ptr);
end;


procedure CheckPostgreSqlDllLoad;
  procedure GetProc(var proc: pointer; name: PAnsiChar);
  begin
    proc := GetProcAddress(libpostgresql_handle, name);
    if not Assigned(proc) then
    //if name='sdfsf' then
      RaiseLastOsError;
  end;
var
  LastError: DWORD;
begin
  if libpostgresql_handle<>0 then
    Exit;
  libpostgresql_handle := LoadLibrary(libpq);
  if libpostgresql_handle = 0 then
  begin
    LastError := GetLastError;
    raise Exception.CreateFmt('Load library %s error: %d %s', [libpq, LastError, SysErrorMessage(LastError)]);
  end;
    //raise Exception.Create('Failed to load sqlite dll');
  GetProc(@PQconnectStart, 'PQconnectStart');
  GetProc(@PQconnectPoll, 'PQconnectPoll');
  GetProc(@PQconnectdb, 'PQconnectdb');
  GetProc(@PQsetdbLogin, 'PQsetdbLogin');
  GetProc(@PQfinish, 'PQfinish');
  GetProc(@PQconndefaults, 'PQconndefaults');
  GetProc(@PQconninfoParse, 'PQconninfoParse');
  GetProc(@PQconninfoFree, 'PQconninfoFree');
  GetProc(@PQresetStart, 'PQresetStart');
  GetProc(@PQresetPoll, 'PQresetPoll');
  GetProc(@PQreset, 'PQreset');
  GetProc(@PQgetCancel, 'PQgetCancel');
  GetProc(@PQfreeCancel, 'PQfreeCancel');
  GetProc(@PQcancel, 'PQcancel');
  GetProc(@PQrequestCancel, 'PQrequestCancel');
  GetProc(@PQdb, 'PQdb');
  GetProc(@PQuser, 'PQuser');
  GetProc(@PQpass, 'PQpass');
  GetProc(@PQhost, 'PQhost');
  GetProc(@PQport, 'PQport');
  GetProc(@PQtty, 'PQtty');
  GetProc(@PQoptions, 'PQoptions');
  GetProc(@PQstatus, 'PQstatus');
  GetProc(@PQtransactionStatus, 'PQtransactionStatus');
  GetProc(@PQparameterStatus, 'PQparameterStatus');
  GetProc(@PQprotocolVersion, 'PQprotocolVersion');
  GetProc(@PQserverVersion, 'PQserverVersion');
  GetProc(@PQerrorMessage, 'PQerrorMessage');
  GetProc(@PQsocket, 'PQsocket');
  GetProc(@PQbackendPID, 'PQbackendPID');
  GetProc(@PQconnectionNeedsPassword, 'PQconnectionNeedsPassword');
  GetProc(@PQconnectionUsedPassword, 'PQconnectionUsedPassword');
  GetProc(@PQclientEncoding, 'PQclientEncoding');
  GetProc(@PQsetClientEncoding, 'PQsetClientEncoding');
  GetProc(@PQgetssl, 'PQgetssl');
  GetProc(@PQinitSSL, 'PQinitSSL');
  GetProc(@PQinitOpenSSL, 'PQinitOpenSSL');
  GetProc(@PQsetErrorVerbosity, 'PQsetErrorVerbosity');
  GetProc(@PQtrace, 'PQtrace');
  GetProc(@PQuntrace, 'PQuntrace');
  GetProc(@PQsetNoticeReceiver, 'PQsetNoticeReceiver');
  GetProc(@PQsetNoticeProcessor, 'PQsetNoticeProcessor');
  GetProc(@PQregisterThreadLock, 'PQregisterThreadLock');
  GetProc(@PQexec, 'PQexec');
  GetProc(@PQexecParams, 'PQexecParams');
  GetProc(@PQprepare, 'PQprepare');
  GetProc(@PQexecPrepared, 'PQexecPrepared');
  GetProc(@PQsendQuery, 'PQsendQuery');
  GetProc(@PQsendQueryParams, 'PQsendQueryParams');
  GetProc(@PQsendPrepare, 'PQsendPrepare');
  GetProc(@PQsendQueryPrepared, 'PQsendQueryPrepared');
  GetProc(@PQgetResult, 'PQgetResult');
  GetProc(@PQisBusy, 'PQisBusy');
  GetProc(@PQconsumeInput, 'PQconsumeInput');
  GetProc(@PQnotifies, 'PQnotifies');
  GetProc(@PQputCopyData, 'PQputCopyData');
  GetProc(@PQputCopyEnd, 'PQputCopyEnd');
  GetProc(@PQgetCopyData, 'PQgetCopyData');
  GetProc(@PQgetline, 'PQgetline');
  GetProc(@PQputline, 'PQputline');
  GetProc(@PQgetlineAsync, 'PQgetlineAsync');
  GetProc(@PQputnbytes, 'PQputnbytes');
  GetProc(@PQendcopy, 'PQendcopy');
  GetProc(@PQsetnonblocking, 'PQsetnonblocking');
  GetProc(@PQisnonblocking, 'PQisnonblocking');
  GetProc(@PQisthreadsafe, 'PQisthreadsafe');
  GetProc(@PQflush, 'PQflush');
  GetProc(@PQfn, 'PQfn');
  GetProc(@PQresultStatus, 'PQresultStatus');
  GetProc(@PQresStatus, 'PQresStatus');
  GetProc(@PQresultErrorMessage, 'PQresultErrorMessage');
  GetProc(@PQresultErrorField, 'PQresultErrorField');
  GetProc(@PQntuples, 'PQntuples');
  GetProc(@PQnfields, 'PQnfields');
  GetProc(@PQbinaryTuples, 'PQbinaryTuples');
  GetProc(@PQfname, 'PQfname');
  GetProc(@PQfnumber, 'PQfnumber');
  GetProc(@PQftable, 'PQftable');
  GetProc(@PQftablecol, 'PQftablecol');
  GetProc(@PQfformat, 'PQfformat');
  GetProc(@PQftype, 'PQftype');
  GetProc(@PQfsize, 'PQfsize');
  GetProc(@PQfmod, 'PQfmod');
  GetProc(@PQcmdStatus, 'PQcmdStatus');
  GetProc(@PQoidStatus, 'PQoidStatus');
  GetProc(@PQoidValue, 'PQoidValue');
  GetProc(@PQcmdTuples, 'PQcmdTuples');
  GetProc(@PQgetvalue, 'PQgetvalue');
  GetProc(@PQgetlength, 'PQgetlength');
  GetProc(@PQgetisnull, 'PQgetisnull');
  GetProc(@PQnparams, 'PQnparams');
  GetProc(@PQparamtype, 'PQparamtype');
  GetProc(@PQdescribePrepared, 'PQdescribePrepared');
  GetProc(@PQdescribePortal, 'PQdescribePortal');
  GetProc(@PQsendDescribePrepared, 'PQsendDescribePrepared');
  GetProc(@PQsendDescribePortal, 'PQsendDescribePortal');
  GetProc(@PQclear, 'PQclear');
  GetProc(@PQfreemem, 'PQfreemem');
  GetProc(@PQmakeEmptyPGresult, 'PQmakeEmptyPGresult');
  GetProc(@PQcopyResult, 'PQcopyResult');
  GetProc(@PQsetResultAttrs, 'PQsetResultAttrs');
  GetProc(@PQresultAlloc, 'PQresultAlloc');
  GetProc(@PQsetvalue, 'PQsetvalue');
  GetProc(@PQescapeStringConn, 'PQescapeStringConn');
  GetProc(@PQescapeByteaConn, 'PQescapeByteaConn');
  GetProc(@PQunescapeBytea, 'PQunescapeBytea');
  GetProc(@PQescapeString, 'PQescapeString');
  GetProc(@PQescapeBytea, 'PQescapeBytea');
  GetProc(@PQprint, 'PQprint');
  GetProc(@PQdisplayTuples, 'PQdisplayTuples');
  GetProc(@PQprintTuples, 'PQprintTuples');
  GetProc(@lo_open, 'lo_open');
  GetProc(@lo_close, 'lo_close');
  GetProc(@lo_read, 'lo_read');
  GetProc(@lo_write, 'lo_write');
  GetProc(@lo_lseek, 'lo_lseek');
  GetProc(@lo_creat, 'lo_creat');
  GetProc(@lo_create, 'lo_create');
  GetProc(@lo_tell, 'lo_tell');
  GetProc(@lo_truncate, 'lo_truncate');
  GetProc(@lo_unlink, 'lo_unlink');
  GetProc(@lo_import, 'lo_import');
  GetProc(@lo_import_with_oid, 'lo_import_with_oid');
  GetProc(@lo_export, 'lo_export');
  GetProc(@PQmblen, 'PQmblen');
  GetProc(@PQdsplen, 'PQdsplen');
  GetProc(@PQenv2encoding, 'PQenv2encoding');
  GetProc(@PQencryptPassword, 'PQencryptPassword');
  GetProc(@pg_char_to_encoding, 'pg_char_to_encoding');
  GetProc(@pg_encoding_to_char, 'pg_encoding_to_char');
  GetProc(@pg_valid_server_encoding_id, 'pg_valid_server_encoding_id');

end;


end.
