// Direct Oracle Access - Directives and defines include file
// Copyright 1998 - 2007 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{ DEFINE NODATASET}           // Lite Edition
{ DEFINE EVALUATION}

{$DEFINE Delphi}              // Define for Delphi
{$DEFINE CompilerVersion3}    // Define for Delphi 3 and C++Builder 3
{$DEFINE CompilerVersion4}    // Define for Delphi 4 and C++Builder 4
{$DEFINE CompilerVersion5}    // Define for Delphi 5 and C++Builder 5
{$DEFINE CompilerVersion6}    // Define for Delphi 6 and C++Builder 6
{$DEFINE CompilerVersion7}    // Define for Delphi 7 and C++Builder 7
{$DEFINE CompilerVersion2005} // Define for Delphi and C++Builder 2005
{$DEFINE CompilerVersion2006} // Define for Delphi and C++Builder 2006/2007

{.$D- Debug Information}
{$R- Range checking}
{$A+ Aligned record fields}
{$B- Complete Boolean Eval}
{$H+ Huge Strings}
{$J+ Assignable Typed Constants}
{$X+ Extended Syntax}

{$IFDEF EVALUATION}
  {$L- Local symbols}
  {$Y- Symbol Info}
{$ENDIF}


{$IFNDEF Delphi} {$DEFINE CBuilder} {$ENDIF}

{$IFDEF CompilerVersion4}
  {$IFDEF Delphi}
    {$DEFINE Overloading}
  {$ENDIF}
{$ENDIF}

{$IFDEF CBuilder} {$IFDEF CompilerVersion3}
{$ObjExportAll On} // C++Builder 3 setting
{$ENDIF} {$ENDIF}

