// Direct Oracle Access - Filter Unit, used by OracleData
// Copyright 1999 - 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

// Based upon Martin Lafferty's Expression component
// Copyright: 1997 Production Robots Engineering Ltd, all rights reserved.
// Version:   1.02 20/1/98
// Status:    Free for private or commercial use subject to following restrictions:
//            * Use entirely at your own risk
//            * Do not resdistribute without this note
//            * Any redistribution to be free of charges
// any questions to Martin Lafferty   robots@enterprise.net

{$I Oracle.inc}

unit OracleFilter;

interface

{$IFNDEF LINUX}
uses
  DB, Classes, SysUtils, OracleVisual;
{$ELSE}
uses
  DB, Classes, SysUtils, OracleVisual;
{$ENDIF}


type
  TExprType = (ttDate, ttString, ttFloat, ttInteger, ttBoolean);

  TExpression = class
  private
    FCaseInsensitive: Boolean;
    FPartialMatch: Boolean;
  protected
    function GetAsDate: TDateTime; virtual;
    function GetAsString: String; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetIsNull: Boolean; virtual;
    function GetExprType: TExprType; virtual; abstract;
  public
    property AsDate: TDateTime read GetAsDate;
    property AsString: String read GetAsString;
    property AsFloat: Double read GetAsFloat;
    property AsInteger: Integer read GetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean;
    property ExprType: TExprType read GetExprType;
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive;
    property PartialMatch: Boolean read FPartialMatch write FPartialMatch;
    property IsNull: Boolean read GetIsNull;
    function CanReadAs(aExprType: TExprType): Boolean;
    constructor Create;
    destructor  Destroy; override;
  end;

  TStringLiteral = class(TExpression)
  private
    FAsString: String;
  protected
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsString: String);
  end;

  TFloatLiteral = class(TExpression)
  private
    FAsFloat: Double;
  protected
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsFloat: Double);
  end;

  TIntegerLiteral = class(TExpression)
  private
    FAsInteger: Integer;
  protected
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsInteger: Integer);
  end;

  TBooleanLiteral = class(TExpression)
  private
    FAsBoolean: Boolean;
  protected
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsBoolean: Boolean);
  end;

  TParameterList = class(TList)
  private
    function GetAsString(i: Integer): String;
    function GetAsFloat(i: Integer): Double;
    function GetAsInteger(i: Integer): Integer;
    function GetAsBoolean(i: Integer): Boolean;
    function GetExprType(i: Integer): TExprType;
    function GetParam(i: Integer): TExpression;
  public
    destructor Destroy; override;
    property Param[i: Integer]: TExpression read GetParam;
    property ExprType[i: Integer]: TExprType read GetExprType;
    property AsString[i: Integer]: String read GetAsString;
    property AsFloat[i: Integer]: Double read GetAsFloat;
    property AsInteger[i: Integer]: Integer read GetAsInteger;
    property AsBoolean[i: Integer]: Boolean read GetAsBoolean;
  end;

  TFunction = class(TExpression)
  private
    FParameterList: TParameterList;
    function GetParam(n: Integer): TExpression;
  public
    constructor Create(aParameterList: TParameterList);
    destructor Destroy; override;
    function ParameterCount: Integer;
    property Param[n: Integer]: TExpression read GetParam;
  end;

  EExpression = class(Exception);

  TIdentifierFunction = function(const Identifier: String;
                                  ParameterList: TParameterList): TExpression of Object;

function CreateExpression(const S: String;
                IdentifierFunction: TIdentifierFunction; Options: TFilterOptions): TExpression;

const
  // Max length for fieldnames and characterstrings
  MaxStringLength = 255;
  // to get a string representation of TExprType use NExprType[ExprType]
  NExprType: array[TExprType] of String = ('Date', 'String', 'Float', 'Integer', 'Boolean');
  // Options, used in CreateExpression
  MasterCaseInsensitive: Boolean = False;
  MasterPartialMatch:    Boolean = False;

  function WildCardCompare(Mask, s: string): Boolean;

implementation

type
  TOperator = (opNot,
               opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr,
               opPlus, opMinus, opOr, opXor,
               opEq, opNEQ, opLT, opGT, opLTE, opGTE);
  TUnaryOp = class(TExpression)
  private
    Operand: TExpression;
    OperandType: TExprType;
    Operator: TOperator;
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand: TExpression);
    destructor Destroy; override;
  end;

  TBinaryOp = class(TExpression)
  private
    Operand1, Operand2: TExpression;
    Operator: TOperator;
    OperandType: TExprType;
  protected
    function GetAsDate: TDateTime; override;
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
    destructor Destroy; override;
  end;

  TRelationalOp = class(TExpression)
  private
    Operand1, Operand2: TExpression;
    Operator: TOperator;
  protected
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
    destructor Destroy; override;
  end;

const
  NOperator: array[TOperator] of String =
              ('opNot',
               'opMult', 'opDivide', 'opDiv', 'opMod', 'opAnd', 'opShl', 'opShr',
               'opPlus', 'opMinus', 'opOr', 'opXor',
               'opEq', 'opNEQ', 'opLT', 'opGT', 'opLTE', 'opGTE');

  UnaryOperators = [opNot];
  MultiplyingOperators = [opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr];
  AddingOperators = [opPlus, opMinus, opOr, opXor];
  RelationalOperators = [opEQ, opNEQ, opLT, opGT, opLTE, opGTE];

  NBoolean: array[Boolean] of String[5] = ('FALSE', 'TRUE');

function ResultType(Operator: TOperator; OperandType: TExprType): TExprType;
procedure NotAppropriate;
begin
  Result := ttString;
  raise EExpression.CreateFmt( 'Operator %s incompatible with %s',
                               [NOperator[Operator], NExprType[OperandType]])
end;

begin
  case OperandType of
    ttDate:
    case Operator of
      opPlus, opMinus: Result := ttDate;
      opEq..opGTE: Result := ttBoolean;
    else
      NotAppropriate;
    end;
    ttString:
    case Operator of
      opPlus: Result := ttString;
      opEq..opGTE: Result := ttBoolean;
    else
      NotAppropriate;
    end;
    ttFloat:
    case Operator of
      opMult, opDivide, opPlus, opMinus: Result := ttFloat;
      opEq..opGTE: Result := ttBoolean;
    else
      NotAppropriate;
    end;
    ttInteger:
    case Operator of
      opNot, opMult, opDiv, opMod, opAnd, opShl, opShr, opPlus, opMinus,
      opOr, opXor: Result := ttInteger;
      opDivide: Result := ttFloat;
      opEq..opGTE: Result := ttBoolean;
    else
      NotAppropriate;
    end;
    ttBoolean:
    case Operator of
      opNot, opAnd, opOr, opXor, opEq, opNEQ: Result := ttBoolean;
    else
      NotAppropriate;
    end;
  end
end;

function CommonType(Op1Type, Op2Type: TExprType): TExprType;
begin
  if Op1Type < Op2Type then Result := Op1Type else Result := Op2Type
end;

procedure Internal(Code: Integer);
begin
  raise EExpression.CreateFmt('Internal parser error. Code %d', [Code])
end;

constructor TExpression.Create;
begin
  inherited Create;
  FCaseInsensitive := MasterCaseInsensitive;
  FPartialMatch    := MasterPartialMatch;
end;

destructor TExpression.Destroy;
begin
  inherited Destroy
end;

function TExpression.GetAsDate: TDateTime;
begin
  Result := 0;
  case ExprType of
      ttDate,
   ttBoolean: raise EExpression.CreateFmt('Cannot read %s as Date', [NExprType[ExprType]]);
    ttString: Result := StrToDateTime(AsString);
     ttFloat: Result := AsFloat;
   ttInteger: Result := AsInteger;
  end
end;

function DateString(bd: TDateTime): string;
begin
  if Frac(bd) = 0 then
    Result := FormatDateTime('ddddd', bd)
  else
    Result := FormatDateTime('ddddd tt', bd);
end;

function TExpression.GetAsString: String;
begin
  Result := '';
  case ExprType of
      ttDate: Result := DateString(AsDate);
    ttString: raise EExpression.CreateFmt('Cannot read %s as String', [NExprType[ExprType]]);
     ttFloat: Result := FloatToStr(AsFloat);
   ttInteger: Result := IntToStr(AsInteger);
   ttBoolean: Result := NBoolean[AsBoolean];
  end
end;

function TExpression.GetAsFloat: Double;
begin
  Result := 0;
  case ExprType of
    ttString, ttFloat:
      raise EExpression.CreateFmt('Cannot read %s as Float', [NExprType[ExprType]]);
    ttDate: Result := AsDate;
    ttInteger, ttBoolean: Result:= AsInteger;
  end
end;

function TExpression.GetAsInteger: Integer;
begin
  Result:= 0;
  case ExprType of
    ttString, ttFloat, ttInteger:
       raise EExpression.CreateFmt('Cannot read %s as integer', [NExprType[ExprType]]);
    ttDate: Result := Trunc(AsDate);
    ttBoolean: Result := Integer(AsBoolean);
  end;
end;

function TExpression.GetAsBoolean: Boolean;
begin
  raise EExpression.CreateFmt('Cannot read %s as boolean', [NExprType[ExprType]])
end;

function TExpression.CanReadAs(aExprType: TExprType): Boolean;
begin
  Result := Ord(ExprType) >= Ord(aExprType)
end;

function TExpression.GetIsNull: Boolean;
begin
  Result := False;
end;

// StringLiteral

function TStringLiteral.GetAsDate: TDateTime;
begin
  Result := StrToDateTime(FAsString);
end;

function TStringLiteral.GetAsString: String;
begin
  Result := FAsString;
end;

function TStringLiteral.GetExprType: TExprType;
begin
  Result := ttString;
end;

constructor TStringLiteral.Create(aAsString: String);
begin
  inherited Create;
  FAsString := aAsString;
end;

// FloatLiteral

function TFloatLiteral.GetAsDate: TDateTime;
begin
  Result := FAsFloat;
end;

function TFloatLiteral.GetAsString: String;
begin
  Result := FloatToStr(FAsFloat);
end;

function TFloatLiteral.GetAsFloat: Double;
begin
  Result := FAsFloat;
end;

function TFloatLiteral.GetExprType: TExprType;
begin
  Result := ttFloat;
end;

constructor TFloatLiteral.Create(aAsFloat: Double);
begin
  inherited Create;
  FAsFloat := aAsFloat;
end;

// IntegerLiteral

function TIntegerLiteral.GetAsDate: TDateTime;
begin
  Result := FAsInteger;
end;

function TIntegerLiteral.GetAsString: String;
begin
  Result := FloatToStr(FAsInteger)
end;

function TIntegerLiteral.GetAsFloat: Double;
begin
  Result := FAsInteger
end;

function TIntegerLiteral.GetAsInteger: Integer;
begin
  Result := FAsInteger
end;

function TIntegerLiteral.GetExprType: TExprType;
begin
  Result := ttInteger
end;

constructor TIntegerLiteral.Create(aAsInteger: Integer);
begin
  inherited Create;
  FAsInteger := aAsInteger
end;

// BooleanLiteral

function TBooleanLiteral.GetAsDate: TDateTime;
begin
  Result := GetAsFloat;
end;

function TBooleanLiteral.GetAsString: String;
begin
  Result := NBoolean[FAsBoolean]
end;

function TBooleanLiteral.GetAsFloat: Double;
begin
  Result := GetAsInteger
end;

function TBooleanLiteral.GetAsInteger: Integer;
begin
  Result := Integer(FAsBoolean)
end;

function TBooleanLiteral.GetAsBoolean: Boolean;
begin
  Result := FAsBoolean
end;

function TBooleanLiteral.GetExprType: TExprType;
begin
  Result := ttBoolean
end;

constructor TBooleanLiteral.Create(aAsBoolean: Boolean);
begin
  inherited Create;
  FAsBoolean := aAsBoolean
end;

function TUnaryOp.GetAsFloat: Double;
begin
  case Operator of
   opMinus: Result := -Operand.AsFloat;
    opPlus: Result := Operand.AsFloat;
  else
    Result := inherited GetAsFloat;
  end
end;

function TUnaryOp.GetAsInteger: Integer;
begin
  Result := 0;
  case Operator of
   opMinus: Result := -Operand.AsInteger;
    opPlus: Result := Operand.AsInteger;
     opNot:
    case OperandType of
      ttInteger: Result := not Operand.AsInteger;
      ttBoolean: Result := Integer(AsBoolean);
    else
      Internal(6);
    end;
  else
    Result := inherited GetAsInteger;
  end
end;

function TUnaryOp.GetAsBoolean: Boolean;
begin
  case Operator of
    opNot: Result := not(Operand.AsBoolean)
  else
    Result := inherited GetAsBoolean;
  end
end;

function TUnaryOp.GetExprType: TExprType;
begin
  Result := ResultType(Operator, OperandType)
end;

constructor TUnaryOp.Create(aOperator: TOperator; aOperand: TExpression);
begin
  inherited Create;
  Operand := aOperand;
  Operator := aOperator;
  OperandType := Operand.ExprType;
  if not (Operator in [opNot, opPlus, opMinus]) then
    raise EExpression.CreateFmt('%s is not simple unary operator', [NOperator[Operator]])
end;

destructor TUnaryOp.Destroy;
begin
  Operand.Free;
  inherited Destroy
end;

function TBinaryOp.GetAsDate: TDateTime;
begin
  Result:= 0;
  case ExprType of
     ttDate: case Operator of
               opPlus: Result := Operand1.AsDate + Operand2.AsDate;
              opMinus: Result := Operand1.AsDate - Operand2.AsDate;
             else
               Internal(14);
             end;
   ttString: Result := StrToDateTime(AsString);
    ttFloat: Result := AsFloat;
  ttInteger: Result := AsInteger;
  ttBoolean: Result := Integer(AsBoolean);
  end
end;

function TBinaryOp.GetAsString: String;
begin
  Result:= '';
  case ExprType of
     ttDate: Result := DateString(Asdate);
   ttString:
      case Operator of
        opPlus: Result := Operand1.AsString + Operand2.AsString;
      else
        Internal(10);
      end;
    ttFloat: Result := FloatToStr(AsFloat);
  ttInteger: Result := IntToStr(AsInteger);
  ttBoolean: Result := NBoolean[AsBoolean];
  end
end;

function TBinaryOp.GetAsFloat: Double;
begin
  Result:= 0;
  case ExprType of
    ttFloat:
      case Operator of
          opPlus: Result := Operand1.AsFloat + Operand2.AsFloat;
         opMinus: Result := Operand1.AsFloat - Operand2.AsFloat;
          opMult: Result := Operand1.AsFloat * Operand2.AsFloat;
        opDivide: Result := Operand1.AsFloat / Operand2.AsFloat;
      else
        Internal(11);
      end;
     ttDate: Result := AsDate;
  ttInteger: Result := AsInteger;
  ttBoolean: Result := Integer(AsBoolean);
  end
end;

function TBinaryOp.GetAsInteger: Integer;
begin
  Result := 0;
  case ExprType of
    ttInteger:
    case Operator of
       opPlus: Result := Operand1.AsInteger +   Operand2.AsInteger;
      opMinus: Result := Operand1.AsInteger -   Operand2.AsInteger;
       opMult: Result := Operand1.AsInteger *   Operand2.AsInteger;
        opDiv: Result := Operand1.AsInteger div Operand2.AsInteger;
        opMod: Result := Operand1.AsInteger mod Operand2.AsInteger;
        opShl: Result := Operand1.AsInteger shl Operand2.AsInteger;
        opShr: Result := Operand1.AsInteger shr Operand2.AsInteger;
        opAnd: Result := Operand1.AsInteger and Operand2.AsInteger;
         opOr: Result := Operand1.AsInteger or  Operand2.AsInteger;
        opXor: Result := Operand1.AsInteger xor Operand2.AsInteger;
    else
      Internal(12);
    end;
    ttBoolean: Result := Integer(GetAsBoolean);
  end
end;

function TBinaryOp.GetAsBoolean: Boolean;
begin
  Result := false;
  case Operator of
    opAnd: Result := Operand1.AsBoolean and Operand2.AsBoolean;
     opOr: Result := Operand1.AsBoolean or Operand2.AsBoolean;
    opXor: Result := Operand1.AsBoolean xor Operand2.AsBoolean;
  else
    Internal(13);
  end
end;

function TBinaryOp.GetExprType: TExprType;
begin
  GetExprType := ResultType(Operator, OperandType)
end;

constructor TBinaryOp.Create( aOperator: TOperator; aOperand1, aOperand2: TExpression);
begin
  inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  OperandType := CommonType(Operand1.ExprType, Operand2.ExprType);
  if not (Operator in [opMult..opXor]) then
    raise EExpression.CreateFmt('%s is not a simple binary operator', [NOperator[Operator]])
end;

destructor TBinaryOp.Destroy;
begin
  Operand1.Free;
  Operand2.Free;
  Operand1 := nil;
  Operand2 := nil;
  inherited Destroy
end;

function TRelationalOp.GetAsString: String;
begin
  Result := NBoolean[AsBoolean]
end;

function TRelationalOp.GetAsFloat: Double;
begin
  Result := Integer(AsBoolean)
end;

function TRelationalOp.GetAsInteger: Integer;
begin
  Result := Integer(AsBoolean)
end;

function WildCardCompare(Mask, s: string): Boolean;
var mlen, slen: Integer;
 function Fit(mpos, spos: integer): Boolean;
 var DoesntFit, FitExist: Boolean;
  function AllTest(mpos, spos: integer): Boolean;
  var Exists: Boolean;
  begin
    Exists := (mpos > mlen);
    while (spos <= slen) and not Exists do
    begin
      if Fit(mpos, spos) then Exists := True else inc(spos);
    end;
    Result := Exists;
  end;
 begin
   DoesntFit := False;
   FitExist  := False;
   while (mpos <= mlen) and (not FitExist) and not DoesntFit do
   begin
     if Mask[mpos] = '*' then
     begin
       FitExist := AllTest(mpos + 1, spos);
       if not FitExist then DoesntFit := true;
     end else
       if spos > slen then
         DoesntFit := True
       else
         if Mask[mpos] <> '?' then
           if Mask[mpos] <> s[spos] then DoesntFit := True;
     inc(mpos);
     inc(spos);
   end;
   if FitExist then
     Result := True
   else
     if DoesntFit then Result := False else Result := (mpos > mlen) and (spos > slen);
 end;
begin
  mlen := Length(Mask);
  slen := Length(s);
  WildCardCompare := Fit(1,1);
end;

function TRelationalOp.GetAsBoolean: Boolean;
var S1, S2: string;
    Matched, Match: Boolean;
begin
  Result := False;

  if Operand1.IsNull or Operand2.IsNull then
  begin
    case Operator of
      opEQ: Result := Operand1.IsNull and Operand2.IsNull;
     opNEQ: Result := (not Operand1.IsNull) or (not Operand2.IsNull);
    end;
    Exit;
  end;

  case CommonType(Operand1.ExprType, Operand2.ExprType) of

    ttBoolean:
    case Operator of
       opEQ: Result := Operand1.AsBoolean =  Operand2.AsBoolean;
      opNEQ: Result := Operand1.AsBoolean <> Operand2.AsBoolean;
    else
      raise EExpression.CreateFmt('cannot apply %s to boolean operands', [NOperator[Operator]]);
    end;

    ttInteger:
    case Operator of
       opLT: Result := Operand1.AsInteger <  Operand2.AsInteger;
      opLTE: Result := Operand1.AsInteger <= Operand2.AsInteger;
       opGT: Result := Operand1.AsInteger >  Operand2.AsInteger;
      opGTE: Result := Operand1.AsInteger >= Operand2.AsInteger;
       opEQ: Result := Operand1.AsInteger =  Operand2.AsInteger;
      opNEQ: Result := Operand1.AsInteger <> Operand2.AsInteger;
    end;

    ttDate:
    case Operator of
       opLT: Result := Operand1.AsDate <  Operand2.AsDate;
      opLTE: Result := Operand1.AsDate <= Operand2.AsDate;
       opGT: Result := Operand1.AsDate >  Operand2.AsDate;
      opGTE: Result := Operand1.AsDate >= Operand2.AsDate;
       opEQ: Result := Operand1.AsDate =  Operand2.AsDate;
      opNEQ: Result := Operand1.AsDate <> Operand2.AsDate;
    end;

    ttFloat:
    case Operator of
       opLT: Result := Operand1.AsFloat <  Operand2.AsFloat;
      opLTE: Result := Operand1.AsFloat <= Operand2.AsFloat;
       opGT: Result := Operand1.AsFloat >  Operand2.AsFloat;
      opGTE: Result := Operand1.AsFloat >= Operand2.AsFloat;
       opEQ: Result := Operand1.AsFloat =  Operand2.AsFloat;
      opNEQ: Result := Operand1.AsFloat <> Operand2.AsFloat;
    end;

    ttString:
    begin
      S1 := Operand1.AsString;
      S2 := Operand2.AsString;
      if fCaseInsensitive then
      begin
        S1 := AnsiUpperCase(S1);
        S2 := AnsiUpperCase(S2);
      end;
      if fPartialMatch then
      begin
        Matched := True;
        Match   := WildCardCompare(S2, S1);
      end else begin
        Matched := False;
        Match   := False;
      end;
      case Operator of
       opLT: Result := S1 <  S2;
      opLTE: Result := S1 <= S2;
       opGT: Result := S1 >  S2;
      opGTE: Result := S1 >= S2;
       opEQ: if Matched then Result := Match else Result := S1 = S2;
      opNEQ: if Matched then Result := not Match else Result := S1 <> S2;
      end;
    end;

  end
end;

function TRelationalOp.GetExprType: TExprType;
begin
  Result := ttBoolean
end;

constructor TRelationalOp.Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
begin
  inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  if not (Operator in RelationalOperators) then
    raise EExpression.CreateFmt('%s is not relational operator', [NOperator[Operator]])
end;

destructor TRelationalOp.Destroy;
begin
  Operand1.Free;
  Operand2.Free;
  inherited Destroy
end;

function TParameterList.GetAsString(i: Integer): String;
begin
  Result := Param[i].AsString
end;

function TParameterList.GetAsFloat(i: Integer): Double;
begin
  Result := Param[i].AsFloat
end;

function TParameterList.GetAsInteger(i: Integer): Integer;
begin
  Result := Param[i].AsInteger
end;

function TParameterList.GetAsBoolean(i: Integer): Boolean;
begin
  Result := Param[i].AsBoolean
end;

function TParameterList.GetExprType(i: Integer): TExprType;
begin
  Result := Param[i].ExprType
end;

function TParameterList.GetParam(i: Integer): TExpression;
begin
  Result := TExpression(Items[i])
end;

destructor TParameterList.Destroy;
var i: Integer;
begin
  for i := 0 to (Count - 1) do TObject(Items[i]).Free;
  inherited Destroy
end;

function TFunction.GetParam(n: Integer): TExpression;
begin
  Result := FParameterList.Param[n]
end;

function TFunction.ParameterCount: Integer;
begin
  if Assigned(FParameterList) then
    ParameterCount := FParameterList.Count
  else
    ParameterCount := 0
end;

constructor TFunction.Create(aParameterList: TParameterList);
begin
  inherited Create;
  FParameterList := aParameterList
end;

destructor TFunction.Destroy;
begin
  FParameterList.Free;
  inherited Destroy
end;

type
  TTypeCast = class(TFunction)
  private
    Operator: TExprType;
  protected
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create( aParameterList: TParameterList; aOperator: TExprType);
  end;

  TMF = (mfTrunc, mfRound, mfAbs, mfArcTan, mfCos, mfExp, mfFrac, mfInt,
         mfLn, mfPi, mfSin, mfSqr, mfSqrt, mfPower);

  TMathExpression = class(TFunction)
  private
    Operator: TMF;
    procedure CheckParameters;
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create( aParameterList: TParameterList; aOperator: TMF);
  end;

  TSF = (sfUpper, sfLower, sfCopy, sfPos, sfLength);

  TStringExpression = class(TFunction)
  private
    Operator: TSF;
    procedure CheckParameters;
  protected
    function GetAsString: String; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aParameterList: TParameterList; aOperator: TSF);
  end;

  TConditional = class(TFunction)
  private
    procedure CheckParameters;
    function Rex: TExpression;
  protected
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
  end;

const
  NTypeCast: array[TExprType] of PChar =
    ('DATE', 'STRING', 'FLOAT', 'INTEGER', 'BOOLEAN');
  NMF: array[TMF] of PChar =
    ('TRUNC', 'ROUND', 'ABS', 'ARCTAN', 'COS', 'EXP', 'FRAC', 'INT',
     'LN', 'PI', 'SIN', 'SQR', 'SQRT', 'POWER');
  NSF: array[TSF] of PChar = ('UPPER', 'LOWER', 'COPY', 'POS', 'LENGTH');

function TStringExpression.GetAsString: String;
begin
  CheckParameters;
  case Operator of
    sfUpper: Result := UpperCase(Param[0].AsString);
    sfLower: Result := LowerCase(Param[0].AsString);
     sfCopy: Result :=  Copy(Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger);
  else
    Result := inherited GetAsString;
  end
end;

function TStringExpression.GetAsInteger: Integer;
begin
  CheckParameters;
  case Operator of
       sfPos: Result := Pos(Param[0].AsString, Param[1].AsString);
    sfLength: Result := Length(Param[0].AsString);
  else
    Result := inherited GetAsInteger
  end
end;

function TStringExpression.GetExprType: TExprType;
begin
  case Operator of
    sfUpper, sfLower, sfCopy: Result := ttString;
  else
    Result := ttInteger;
  end
end;

procedure TStringExpression.CheckParameters;
var OK: Boolean;
begin
  OK:= false;
  case Operator of
    sfUpper, sfLower, sfLength:
      OK := (ParameterCount = 1) and
            (Param[0].ExprType >= ttString);
    sfCopy:
      OK := (ParameterCount = 3) and
            (Param[0].ExprType >= ttString) and
            (Param[1].ExprType >= ttInteger) and
            (Param[2].ExprType >= ttInteger);
    sfPos:
      OK := (ParameterCount = 2) and
            (Param[0].ExprType >= ttString) and
            (Param[1].ExprType >= ttString);
  end;
  if not OK then
    raise EExpression.CreateFmt('Invalid parameter to %s', [NSF[Operator]])
end;

constructor TStringExpression.Create(aParameterList: TParameterList; aOperator: TSF);
begin
  inherited Create(aParameterList);
  Operator:= aOperator
end;

function TMathExpression.GetAsFloat: Double;
begin
  CheckParameters;
  case Operator of
       mfAbs: Result := Abs(Param[0].AsFloat);
    mfArcTan: Result := ArcTan(Param[0].AsFloat);
       mfCos: Result := Cos(Param[0].AsFloat);
       mfExp: Result := Exp(Param[0].AsFloat);
      mfFrac: Result := Frac(Param[0].AsFloat);
       mfInt: Result := Int(Param[0].AsFloat);
        mfLn: Result := Ln(Param[0].AsFloat);
        mfPi: Result := Pi;
       mfSin: Result := Sin(Param[0].AsFloat);
       mfSqr: Result := Sqr(Param[0].AsFloat);
      mfSqrt: Result := Sqrt(Param[0].AsFloat);
     mfPower: Result := Exp(Param[1].AsFloat * Ln(Param[0].AsFloat))
  else
    Result := inherited GetAsFloat;
  end
end;

function TMathExpression.GetAsInteger: Integer;
begin
  CheckParameters;
  case Operator of
    mfTrunc: Result := Trunc(Param[0].AsFloat);
    mfRound: Result := Round(Param[0].AsFloat);
      mfAbs: Result := Abs(Param[0].AsInteger);
  else
    Result := inherited GetAsInteger;
  end
end;

procedure TMathExpression.CheckParameters;
var OK: Boolean;
begin
  OK:= True;
  case Operator of
    mfTrunc, mfRound, mfArcTan, mfCos, mfExp, mfFrac, mfInt,
    mfLn, mfSin, mfSqr, mfSqrt, mfAbs:
    begin
      OK := (ParameterCount = 1) and (Param[0].ExprType >= ttFloat);
    end;
    mfPower:
    begin
      OK := (ParameterCount = 2) and
            (Param[0].ExprType >= ttFloat) and
            (Param[1].ExprType >= ttFloat);
    end;
  end;
  if not OK then
    raise EExpression.CreateFmt('Invalid parameter to %s', [NMF[Operator]])
end;

function TMathExpression.GetExprType: TExprType;
begin
  case Operator of
    mfTrunc, mfRound: Result := ttInteger;
  else
    Result := ttFloat;
  end
end;

constructor TMathExpression.Create(aParameterList: TParameterList; aOperator: TMF);
begin
  inherited Create(aParameterList);
  Operator := aOperator
end;

function TTypeCast.GetAsString: String;
begin
  Result := Param[0].AsString
end;

function TTypeCast.GetAsFloat: Double;
begin
  Result := Param[0].AsFloat
end;

function TTypeCast.GetAsInteger: Integer;
begin
  Result := Param[0].AsInteger
end;

function TTypeCast.GetAsBoolean: Boolean;
begin
  Result := Param[0].AsBoolean
end;

function TTypeCast.GetExprType: TExprType;
begin
  Result := Operator
end;

constructor TTypeCast.Create(aParameterList: TParameterList; aOperator: TExprType);
begin
  inherited Create(aParameterList);
  Operator := aOperator
end;

function TConditional.Rex: TExpression;
begin
  CheckParameters;
  if Param[0].AsBoolean then Result:= Param[1] else Result:= Param[2]
end;

procedure TConditional.CheckParameters;
begin
  if not ((ParameterCount = 3) and
          (Param[0].ExprType = ttBoolean)) then
    raise EExpression.Create('Invalid parameters to If')
end;

function TConditional.GetAsString: String;
begin
  Result := Rex.AsString
end;

function TConditional.GetAsFloat: Double;
begin
  Result := Rex.AsFloat
end;

function TConditional.GetAsInteger: Integer;
begin
  Result := Rex.AsInteger
end;

function TConditional.GetAsBoolean: Boolean;
begin
  Result := Rex.AsBoolean
end;

function TConditional.GetExprType: TExprType;
begin
  Result := Rex.ExprType
end;

function StandardFunctions (const Ident: String; PL: TParameterList): TExpression;
var i: TExprType;
    j: TMF;
    k: TSF;
    Found: Boolean;
begin
  Found := false;
  if Ident = 'IF' then
  begin
    Result := TConditional.Create(PL)
  end else begin
    for i := Low(TExprType) to High(TExprType) do
    begin
      if Ident = NTypeCast[i] then
      begin
        Found := true;
        Break
      end;
    end;
    if Found then
    begin
      Result := TTypeCast.Create(PL, i)
    end else begin
      for j := Low(TMF) to High(TMF) do
      begin
        if Ident = NMF[j] then
        begin
          Found := true;
          break;
        end;
      end;
      if Found then
      begin
        Result := TMathExpression.Create(PL, j)
      end else
      begin
        for k := Low(TSF) to High(TSF) do
        begin
          if Ident = NSF[k] then
          begin
            Found := true;
            break
          end
        end;
        if Found then
        begin
          Result := TStringExpression.Create(PL, k)
        end else begin
          Result := nil;
        end;
      end;
    end;
  end;
end;

const
  OpTokens: array[TOperator] of PChar =
              ( 'NOT',
                '*', '/', 'DIV', 'MOD', 'AND', 'SHL', 'SHR',
                '+', '-', 'OR', 'XOR',
                '=', '<>', '<', '>', '<=', '>=');

const
  Whitespace = [#$1..#$20];
  Digits = ['0'..'9'];
  SignChars = ['+', '-'];
  RelationalChars = ['<', '>', '='];
  OpChars = SignChars + ['/', '*'] + RelationalChars;

  OpenSub = '(';
  CloseSub = ')';
  SQuote = '''';
  PrimaryIdentChars = ['a'..'z', 'A'..'Z', '_'];
  IdentChars = PrimaryIdentChars + Digits;

function CreateExpression(const S: String; IdentifierFunction: TIdentifierFunction; Options: TFilterOptions): TExpression;
var P: PChar;

function Expression: TExpression;

procedure SwallowWhitespace;
begin
  while P^ in Whitespace do inc(P)
end;

function EoE: Boolean;
begin
  Result:= (P^ = #0) or (P^ = CloseSub) or (P^ = ',')
end;

function UnsignedFloat: TExpression;
type
  TNScan = (nsMantissa, nsDPFound, nsExpFound, nsFound);
var S: String[30];
    State: TNScan;
    IsInt: Boolean;
    v: Double;

procedure Bomb;
begin
  raise EExpression.Create('Bad numeric format')
end;

begin // Expression
  S := '';
  IsInt := false;
  State := nsMantissa;
  repeat
    if P^ in Digits then
    begin
      S := S + P^;
      inc(P)
    end else
    if P^ = SysUtils.DecimalSeparator then
    begin
      if State = nsMantissa then
      begin
        S := S + P^;
        inc(P);
        State:= nsDPFound
      end else begin
        Bomb
      end;
    end else
    if (P^ = 'e') or (P^ = 'E') then
    begin
      if (State = nsMantissa) or (State = nsDPFound) then
      begin
        S := S + 'E';
        inc(P);
        if P^ = '-' then
        begin
          S:= S + P^;
          inc(P);
        end;
        State := nsExpFound;
        if not (P^ in Digits) then Bomb
      end else begin
        Bomb
      end;
    end else begin
      IsInt:= (State = nsMantissa);
      State:= nsFound
    end;
    if Length(S) > 28 then Bomb
  until State = nsFound;
  if IsInt then
  begin
    v := StrToFloat(S);
    if v > MaxInt then IsInt := False;
  end;
  if IsInt then
    Result := TIntegerLiteral.Create(StrToInt(S))
  else
    Result := TFloatLiteral.Create(StrToFloat(S))
end; // Expression

function CharacterString: TExpression;
var SR: String;
begin
  SR := '';
  repeat
    inc(P);
    if P^ = SQuote then
    begin
     inc(P);
     if P^ <> SQuote then Break;
    end;
    if P^ = #0 then raise EExpression.Create('Unterminated string');
    if Length(SR) > MaxStringLength then raise EExpression.Create('String too long');
    SR := SR + P^;
  until False;
  Result := TStringLiteral.Create(SR)
end;

function FieldnameString: TExpression;
var SR: String;
begin
  SR := '';
  repeat
    inc(P);
    if P^ = ']' then
    begin
      inc(P);
      Break;
    end;
    if P^ = #0 then raise EExpression.Create('Unterminated fieldname');
    if Length(SR) > MaxStringLength then raise EExpression.Create('Fieldname too long');
    SR := SR + P^;
  until False;
  Result := IdentifierFunction(SR, nil);
end;

type TTokType = (ttIdentifier, ttOperator, ttBooleanLiteral);

function GetTok( var Ident: String;
                 var Operator: TOperator;
                 var BoolLit: Boolean): TTokType;
var Found: Boolean;
    LocalOp: TOperator;
begin
  Found := false;
  Ident := UpCase(P^);
  Result := ttIdentifier;
  repeat
    inc(P);
    if P^ in IdentChars then
      Ident := Ident + UpCase(P^)
    else
      Found := true
  until Found;
  for LocalOp:= Low(TOperator) to High(TOperator) do
  begin
    if OpTokens[LocalOp] = Ident then
    begin
      Result := ttOperator;
      Operator := LocalOp;
      break
    end;
  end;
  if Result = ttIdentifier then
  begin
    if Ident = 'TRUE' then
    begin
      Result := ttBooleanLiteral;
      BoolLit := true
    end else
    if Ident = 'FALSE' then
    begin
      Result := ttBooleanLiteral;
      BoolLit := false
    end
  end
end;

function Factor: TExpression;
var Identifier: String;
    Operator: TOperator;
    BoolLit: Boolean;
    PList: TParameterList;
    MoreParameters: Boolean;
begin
  Result := nil;
  try
    SwallowWhitespace;
    if P^ in SignChars then
    begin
      case P^ of
        '+':
        begin
          Inc(P);
          Result := TUnaryOp.Create(opPlus, Factor);
        end;
        '-':
        begin
          Inc(P);
          Result := TUnaryOp.Create(opMinus, Factor);
        end;
      end
    end else
    if P^ = '[' then
    begin
      Result := FieldnameString;
    end else
    if P^ = SQuote then
    begin
      Result := CharacterString;
    end else
    if P^ in Digits then
    begin
      Result := UnsignedFloat;
    end else
    if P^ = OpenSub then
    begin
      Inc(P);
      Result := Expression;
      if Result = nil then
        raise EExpression.Create('invalid sub-expression');
      if P^ = CloseSub then
        inc(P)
      else
        raise EExpression.Create(' ) expected')
    end else
    if P^ in PrimaryIdentChars then
    begin
      case GetTok(Identifier, Operator, BoolLit) of
        ttOperator:
        if Operator = opNot then
        begin
          inc(P);
          Result:= TUnaryOp.Create(opNot, Factor)
        end else
        begin
          raise EExpression.CreateFmt('%s not allowed here', [NOperator[Operator]]);
        end;
        ttIdentifier:
        begin
          PList := nil;
          try
            SwallowWhitespace;
            MoreParameters:= P^ = OpenSub;
            if MoreParameters then
            begin
              PList := TParameterList.Create;
              while MoreParameters do
              begin
                inc(P);
                PList.Add(Expression);
                MoreParameters := P^ = ','
              end;
              if P^ = CloseSub then
                Inc(P)
              else
                raise EExpression.Create('Incorrectly formed parameters')
            end;
            Result := StandardFunctions(Identifier, PList);
            if (Result = nil) and Assigned(IdentifierFunction) then
              Result := IdentifierFunction(Identifier, PList);
            if Result = nil then
              raise EExpression.CreateFmt('Unknown Identifier %s', [Identifier]);
          finally
            if Result = nil then PList.Free;
          end;
        end;
        ttBooleanLiteral:
        begin
          Result := TBooleanLiteral.Create(BoolLit);
        end;
      end;
    end else
    if EoE then
      raise EExpression.Create('Unexpected end of factor')
    else
      raise EExpression.Create('Syntax error'); // leak here ?
  except
    Result.Free;
    raise;
  end;
end;

function Term: TExpression;
var Identifier: String;
    Operator: TOperator;
    BoolLit: Boolean;
    SavedP: PChar;
    Stop: Boolean;
begin
  Result := Factor;
  Stop := false;
  try
    repeat
      SwallowWhitespace;
      if EoE then
      begin
        Stop := true
      end else
      if (P^ = '*') then
      begin
        inc(P);
        Result := TBinaryOp.Create(opMult, Result, Factor) // xy
      end else
      if (P^ = '/') then
      begin
        inc(P);
        Result := TBinaryOp.Create(opDivide, Result, Factor) // xy
      end else
      if P^ in OpChars then  // only checks for single char operators
      begin
        Stop := True
      end else
      if P^ in PrimaryIdentChars then
      begin
        SavedP := P;
        case GetTok(Identifier, Operator, BoolLit) of
          ttIdentifier:
          begin
            raise EExpression.CreateFmt('Identifier %s not allowed here', [Identifier]);
          end;
          ttOperator:
          if Operator in [opAnd, opDiv, opMod, opShl, opShr] then
          begin
            Result := TBinaryOp.Create(Operator, Result, Factor);
          end else
          begin
            P := SavedP; // push token back - probably or|xor
            Stop := true
          end;
          ttBooleanLiteral:
          begin
            raise EExpression.Create('Boolean literal not allowed here')
          end
        end
      end else begin
        raise EExpression.CreateFmt('char %s in input stream', [P^]);
      end
    until Stop;
  except
    Result.Free;
    raise
  end
end;

function Simple: TExpression;
var Identifier: String;
    Operator: TOperator;
    BoolLit: Boolean;
    SavedP: PChar;
    Stop: Boolean;
    FreeResult: Boolean;
begin
  Result := Term;
  FreeResult := True;
  try
    Stop := false;
    repeat
      SwallowWhitespace;
      if EoE then
      begin
        Stop := true
      end else
      if (P^ = '+') then
      begin
        inc(P);
        FreeResult := False;
        Result := TBinaryOp.Create(opPlus, Result, Term);
        FreeResult := True;
      end else
      if (P^ = '-') then
      begin
        inc(P);
        FreeResult := False;
        Result := TBinaryOp.Create(opMinus, Result, Term);
        FreeResult := True;
      end else
      if P^ in OpChars then  // only checks for single char operators
      begin
        Stop := true;
      end else begin
        SavedP := P;
        case GetTok(Identifier, Operator, BoolLit) of
          ttIdentifier:
          begin
            raise EExpression.CreateFmt('Identifier %s not allowed here', [Identifier]);
          end;
          ttOperator:
          if (Operator = opOr) or (Operator = opXor) then
          begin
            FreeResult := False;
            Result := TBinaryOp.Create(Operator, Result, Term);
            FreeResult := True;
          end else begin
            P:= SavedP;  // push token back - not ours
            Stop := true;
          end;
          ttBooleanLiteral:
          begin
            raise EExpression.Create('Boolean literal not allowed here');
          end;
        end;
      end;
    until Stop;
  except
    if FreeResult then Result.Free;
    raise;
  end;
end;

var OpString: String;
    Op: TOperator;
    OpFound: Boolean;
    Finished: Boolean;
begin // CreateExpression
  Result := nil;
  try
    Finished := false;
    repeat
      SwallowWhitespace;
      if not EoE then
      begin
        Result := Simple;
        if P^ in RelationalChars then
        begin
          OpString := P^;
          inc(P);
          if P^ in RelationalChars then
          begin
            OpString := OpString + P^;
            inc(P);
          end;
          OpFound := false;
          for Op := opEQ to opGTE do
          if OpTokens[Op] = OpString then
          begin
            OpFound := true;
            Break;
          end;
          if not OpFound then
            raise EExpression.CreateFmt('%s not a valid operator', [OpString])
          else begin
            Result := TRelationalOp.Create(Op, Result, Simple);
          end;
        end;
      end else begin
        Finished := true;
      end;
    until Finished;
  except
    Result.Free;
    raise;
  end;
end;

begin // CreateExpression
  MasterCaseInsensitive := foCaseInsensitive in Options;
  MasterPartialMatch    := not (foNoPartialCompare in Options);
  P := PChar(S);
  Result := Expression;
  if P^ <> #0 then
  begin
    Result.Free;
    raise EExpression.CreateFmt('%s not appropriate', [P^]);
  end;
end;

end.

