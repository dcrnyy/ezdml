EZDML V2.13 (Freeware)

EZDML is a small tool which helps you to create data models quickly. It's simple and fast, with EZDML you can create a table within few seconds.

Features:
1.Table model design: create table, fields, primary key, foreign key and comments
2.Text script to create models quickly
3.Diagram layout with logical and physical model views
4.Import exists tables from database (oralce, sqlserver and ODBC)
5.Generate database DDL sql, compare with exists object to genarate proper SQL
6.Generate simple codes: C++, Pascal, c#, java


Usage:
1. The programe will automatically load the last model file on start
2. In the diagram, you may following function keys: + zoom in, - zoom out, R restore, F fit selection (or all), Left/Right/Up/Down for direction
3. In the text script editor, you can use simple letter to present data types: S, I, F, D, BO, E, BL, O (String, Integer, Float, DateTime, Boolean, Enum, Blob, Object), default is String.

For example:

TestTable
--------
ID  PK
RID FK
Title S(200)
ItemCount I
ItemPrice F(10,2)
OrderDate D
Desc Describ
TpNa TypeName
Memo

4. Foreign keys can be added and displayed in the diagram
5. Importing and exporting to ORACLE, SQLSERVER and ODBC supported
6. Without database connection, you can also generate DDL SQL for creation only; only when connected will the generation make compared SQL
7. To avoid data loss, the programe will still generate SQL to Drop objects, but these SQLs will be marked as comments
8. This soft is free for any use. The author assumes no liability for damages, direct or consequential, which may result from the use of the Software. The user must assume the entire risk of using the Software.


Modify INI file for customized configurations:
[DefaultFieldTypes]
1=String:VARSTR(2000)
2=Integer:DECIMAL
[CustFieldTypes]
1=BigInt
2=Decimal
3=TestUnk
[CustDataTypeReplaces]
1=VARCHAR2:NVARCHAR2
2=NUMBER(10):DECIMAL
3=%TEXT%:NCLOB
[Options]
AutoSaveMinutes=5
FieldNameMaxDrawSize=64
FieldTypeMaxDrawSize=48
CreateSeqForOracle=0
OCIDLL=D:\oracle\ora112\instantclient\oci.dll

huzzz@163.com
http://www.ezdml.com
QQ group: 344282607
Forum: http://www.ezdml.com/bbs

Version history:
2009-2-18 V1.21
This tool project started from Apr 2006, so version number is 1.2 now

2009-2-22 V1.22
Some precision bug fixed

2009-6-5 V1.23
Improvements of importing, exporting, foreign link. Support new data types: objects, functions and events.

2009-7-15 V1.24
Index creating bug fixed. Auto deal with long index name. Check hint for invalid field names.

2009-8-29 V1.30
Support SQLSERVER and ODBC driver. Support for multi diagram. C++ code generation.

2009-9-12 V1.32
Generate DML and DQL SQL. Import and export table comments. Add foreign key in diagram. Remember database connection info. Bugs fixed. Support for english

2009-9-25 V1.34
Model diagram color style settings, copy image to clipboard, bugs fixed.

2009-10-11 V1.35
Big application icon, use English by default.

2009-10-21 V1.36
Bugs of importing database object fixed.

2009-11-8 V1.40
Support for MYSQL, some bugs fixed.

2009-11-25 V1.42, 2009-11-29 V1.50
Export to excel, some improvements and bugs fixed.

2009-12-19 V1.52
Bugs fixed.

2010-4-20 V1.55
Support Pascal-Script-Templates, bugs fixed.

2010-9-6 V1.59
Pascal-Script with Setting-Panel, bugs fixed.

2010-12-1 V1.62
Search and import fields from exists table. bugs fixed.

2010-12-20 V1.63
Support auto-increment fields for MySql and SQLServer.

2011-3-15 V1.64
Drag and drop to open a file, recent file list.

2011-4-7 V1.71
Support field comments for SQLServer, fully support for SQLServer2005, bugs fixed.

2011-4-23 V1.77
Batch add or import fields to more than one table, generate indexes SQL for FK fields, bugs of importing ORACLE indexes fixed.

2011-5-1 V1.79
Color settings for PK and Lines, and scroll bars for model graph.

2011-5-15 V1.80
Backup and restore data rows between ORACLE and SQLServer.

2011-6-8 V1.81
Support batch add/remove fields in popup-menu of the table tree. Bugs fixed.

2011-7-17 V1.82
Text script support new format like "CusNam NameOfCustomer S(100)". Bugs fixed.

2011-7-25 V1.85
Support ORACLE connections with NSDOA (BA-Platform-Data-Service). Bugs fixed.

2011-9-16 V1.88
Support text or sql entities. Bugs fixed.

2011-10-16 V1.91
Support overview mode.

2011-11-02 V1.92
Add a SQL tool.

2012-10-20 V1.95
Add html form generator, bugs fixed.

2012-10-28 V1.96
Support multi primary keys, generate with selection, bugs fixed.

2014-04-27 V1.97
Auto save & load on exit & start, some other improvements and bugs fixed.

2014-09-29 V1.98
Specail copy, multi entity color, pascal script with template support (like JSP and ASP).

2014-12-07 V1.99
Auto-save, Custom field type list, Custom field type replacements, bugs fixed.

2015-02-09 V2.01
Default field type customize, Sync same name tables, Both logic and physics name mode, bugs fixed.

2015-04-15 V2.02
Support oralce instant-client, generate sql without db-connection, check file date before fast-loading of tmp file, bugs fixed.

2015-09-07 V2.03
Bugs fixed.

2015-10-11 V2.04
Support Sqlite, bugs fixed.

2015-10-19 V2.06
Support PostgreSQL, support Global-Script-Events to generate customized SQL, save db password, bugs fixed.

2015-12-26 V2.11
Export doc and mht with graph UI and code, support UTF-8 charset for ORACLE, bugs fixed.

2016-05-15 V2.13
Prompt if files opened twice, link line auto align, INI switch for ORACLE sequence, New-table template file, paste field prop from excel, bugs fixed.

