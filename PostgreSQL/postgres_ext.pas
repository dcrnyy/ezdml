
unit postgres_ext;
interface

uses Windows;

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
{$ifndef DWord}
  DWord     = Longint;
{$endif}
{$ifndef PDWord}
  PDWord    = ^DWord;
{$endif}
{$ifndef PDouble}
  PDouble   = ^Double;
{$endif}

   POid = ^Oid;
   Oid = dword;
{ was #define dname def_expr }
function InvalidOid : Oid;  


const
   OID_MAX = MAXDWORD;
{ you will need to include <limits.h> to use the above #define  }
{
 * Identifiers of error message fields.  Kept here to keep common
 * between frontend and backend, and also to export them to libpq
 * applications.
  }
   PG_DIAG_SEVERITY = 'S';   
   PG_DIAG_SQLSTATE = 'C';   
   PG_DIAG_MESSAGE_PRIMARY = 'M';   
   PG_DIAG_MESSAGE_DETAIL = 'D';   
   PG_DIAG_MESSAGE_HINT = 'H';   
   PG_DIAG_STATEMENT_POSITION = 'P';   
   PG_DIAG_INTERNAL_POSITION = 'p';   
   PG_DIAG_INTERNAL_QUERY = 'q';   
   PG_DIAG_CONTEXT = 'W';   
   PG_DIAG_SOURCE_FILE = 'F';   
   PG_DIAG_SOURCE_LINE = 'L';   
   PG_DIAG_SOURCE_FUNCTION = 'R';   

implementation

{ was #define dname def_expr }
function InvalidOid : Oid;
  begin
     InvalidOid:=Oid(0);
  end;


end.
