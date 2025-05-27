










(*<summary>Class and type definitions for the expression parser.</summary>
  
<remarks>Addapted from TExpressionParser. Written by Egbert van Nes (Egbert.vanNes@Aqec.WKAO.WAU.NL).
</remarks>
*)
unit MtxParseClass;


interface

{$I BdsppDefs.inc}

uses
  Math387, MtxVec, MtxVecInt, MtxBaseComp, MtxVecBase, AbstractMtxVec
  
  

  
    
    ,Contnrs
    
    ,Classes, SysUtils
  
  ;

const
  HEX_PREFIX = '$';
  ARGUMENT_SEPARATOR = ',';
  EXPRESSION_SEPARATOR: string = sLineBreak;
  LEFT_BRACKET = '(';
  RIGHT_CLOSED_BRACKET = ']';
  LEFT_CLOSED_BRACKET = '[';
  RIGHT_BRACKET = ')';
  DECIMAL_SEPARATOR = '.';
  SEMICOLON = ';';
  COLON = ':';

const
  (*<summary>Maximum number of function arguments.</summary>*)
  MaxArg = 18; 
  
  UnknownExprWord: string = 'Unrecognized function or value select called on undefined variable!';
  

type

  (*<summary>Defines <see cref="TValueRec"/> value type.</summary>*)
  TExprValueType = (
    (*<summary>Undefined value.</summary>*)
  vtUndefined = 0,
    (*<summary>Double value.</summary>*)
  vtDoubleValue = 1,
    (*<summary>Range</summary>*)
  vtRangeValue = 2,
    (*<summary>String.</summary>*)
  vtString = 3,
    (*<summary>Complex value.</summary>*)
  vtComplexValue = 4,
    (*<summary>Vector.</summary>*)
  vtVectorValue = 5,
    (*<summary>Integer</summary>*)
  vtIntegerValue = 6,
    (*<summary>Int vector</summary>*)
  vtIntVectorValue = 7,
    (*<summary>Int Matrix</summary>*)
  vtIntMatrixValue = 8,
      (*<summary>Bool.</summary>*)
  vtBoolValue = 9,
    (*<summary>Bool vector.</summary>*)
  vtBoolVectorValue  = 10,
      (*<summary>Bool matrix.</summary>*)
  vtBoolMatrixValue = 11,
    (*<summary>Matrix.</summary>*)
  vtMatrixValue = 12,
    (*<summary>List of strings.</summary>*)
  vtStringListValue = 13,
    (*<summary>Custom object.</summary>*)
  vtCustomValue = 14
  );


type
  TSimpleType = (
    stUndefined,
    stInt8,
    stInt16,
    stInt32,
    stInt64,
    stUInt8,
    stUInt16,
    stUInt32,
    stUInt64,
    stFloat,
    stDouble,
    stFloatComplex,
    stDoubleComplex,
    stBoolean,
    stString
  );

const
  
  ValTypeAsStr: array [TExprValueType] of string = (
  
    'undefined', 'double', 'range', 'string', 'complex', 'vector',
                'integer', 'integer vector', 'integer matrix', 'bool', 'bool vector', 'bool matrix', 'matrix','string list','custom object');

  (*<summary> Types of lexems. </summary>
              
<remarks>Does not include operators.
</remarks>
*)
type TLexemeType = Cardinal; 

const ltConstant = 1;
const ltVariable = 2;
const ltFunction = 4;
const ltVaryingFunction = 8;
const ltOperator = 16;
const ltLeftBracket = 32;
const ltRightBracket = 64;
const ltLeftSquareBracket = 128;
const ltRightSquareBracket = 256;
const ltSemicolon = 512;
const ltComma = 1024;

type

  TLexemeTypes = Cardinal;  

  (*<summary>Parser exception error class.</summary>*)
  EMtxParseError = class(EMtxVecException)
  
  end;

  
  TExprRec = class;
  TExprWord = class;
  

  (*<summary>Function type for custom function definition.</summary>
    
<remarks>Any custome defined function must be of this type to be used in the expression parser.
</remarks>


    

    <example>
    <code>
    void _log10x(TExprRec Param);
    {
    }

    void Example()
    {
      TMtxExpression ep = new TMtxExpression();

      ep.DefineFunction("Log10x",_log10x,1);
      ep.Add("Log10x(1.2)+3");
    </code></example>*)
  TExprFunc = procedure(const Expr: TExprRec);

  
  TOperVector = array [vtUndefined..vtCustomValue] of TExprFunc;
  TOperMatrix = array [vtUndefined..vtCustomValue, vtUndefined..vtCustomValue] of TExprFunc;
  

  (*<summary></summary>*)
  TValueRec = class(TPersistent)
  strict private
    FRangeIntPrecision: TIntPrecision;
    FTag: NativeInt;
    procedure SetRangeIntPrecision(const Value: TIntPrecision);
    procedure SetTag(const Value: NativeInt);
    
    
    procedure ReadBinData(Stream: TStream); overload;
    procedure WriteBinData(Stream: TStream); overload;
    
  strict protected
    fOwned: boolean;
    fExtRange: Boolean;
    fBoolValue: boolean;
    fIntegerValue: Int64;
    fValueType: TExprValueType;
    fDoubleValue: double;
    fComplexValue: TCplx;
    fIntVectorValue: TVecInt;
    fIntMatrixValue: TMtxInt;
    fVectorValue: TVec;
    fMatrixValue: TMtx;
    FStringValue: string;
    fCustomValue: TObject;
    FStringListValue: TStringList;

    FOnUndefine: TNotifyEvent;
    FValueTypeLock: boolean;
    procedure SetOnUndefine(const Value: TNotifyEvent);
    procedure SetValueTypeLock(const Value: boolean);
    procedure SetStringListValue(const Value: TStringList);
    procedure SetCustomValue(const Value: TObject);
    procedure SetIntegerValue(const Value: Int64); 
    procedure SetValueType(vtype: TExprValueType);
    procedure SetMatrixValue(const Value: TMtx);
    procedure SetVectorValue(const Value: TVec);
    procedure SetBoolValue(const Value: boolean); 
    procedure SetStringValue(const Value: string);
    procedure SetIntVectorValue(const Value: TVecInt);
    procedure SetIntMatrixValue(const Value: TMtxInt);
    function getVectorValue: TVec;
    function GetIntVectorValue: TVecInt;
    procedure InternalUndefine(vtype: TExprValueType);
  strict protected
    
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo (Dest: TPersistent); override;
    
    function GetAsString: string; virtual;
  public
    fRange: TIntegerArray;

    property IsOwned: boolean read fOwned;
    constructor Create;
    destructor Destroy; override;

    

    procedure AssignOper (const vrec: TValueRec);
    procedure Copy (const vrec: TValueRec);

    (*<summary> Returns object in which the data is stored.</summary>
                
<remarks>Returns TVec, TVecInt, TMtx, TMtxInt or Custom object depending on ValueType property.
</remarks>
*)
    function ValueObject: TObject;

    (*<summary>Returns True, if valueType is vtDoubleValue or vtIntegerValue.</summary>*)
    function IsRealValue: boolean; 
    (*<summary>Define value as custom object value with initial value of nil.</summary>*)
    procedure DefineCustomValue; overload;
    (*<summary>Define value as range.</summary>*)
    procedure DefineInteger; overload;
    (*<summary>Define value as integer vector.</summary>*)
    procedure DefineIntVector; overload;
    (*<summary>Define value as integer vector.</summary>*)
    procedure DefineIntVector(const Vec: TVecInt; aIsOwned: boolean = false); overload;
    (*<summary>Define value as integer matrix.</summary>*)
    procedure DefineIntMatrix; overload;
    (*<summary>Define value as integer matrix.</summary>*)
    procedure DefineIntMatrix(const mtx: TMtxInt; aIsOwned: boolean = false); overload;
    (*<summary>Define value as string.</summary>*)
    procedure DefineString; overload;
    (*<summary>Define value as a boolean.</summary>*)
    procedure DefineBool; overload;
    (*<summary>Define value as a vector of boolean values.</summary>*)
    procedure DefineBoolVector; overload;
    (*<summary>Define value as a vector of boolean values. If IsOwned is true, the var will be owned by TValueRec and released when TValueRec is freed.</summary>*)
    procedure DefineBoolVector(const Vec: TVecInt; aIsOwned: boolean = false); overload;
    (*<summary>Define value as a matrix of boolean values.</summary>*)
    procedure DefineBoolMatrix; overload;
    (*<summary>Define value as a matrix boolean values. If IsOwned is true, the var will be owned by TValueRec and released when TValueRec is freed.</summary>*)
    procedure DefineBoolMatrix(const mtx: TMtxInt; aIsOwned: boolean = false); overload;
    (*<summary>Define value as range.</summary>*)
    procedure DefineRange; overload;
    (*<summary>Define value as string list.</summary>*)
    procedure DefineStringList; overload;
    (*<summary>Define value as string list.</summary>*)
    procedure DefineStringList(const aList: TStringList; aIsOwned: boolean = false); overload;
    (*<summary>Define value as range.</summary>*)
    procedure DefineExtRange; overload;
    (*<summary>Define value as double.</summary>*)
    procedure DefineDouble; overload;
    (*<summary>Define value as complex.</summary>*)
    procedure DefineComplex; overload;
    (*<summary>Define value as vector.</summary>*)
    procedure DefineVector; overload;
    (*<summary>Define value as custom object.</summary>*)
    procedure DefineCustomValue(const obj: TObject; aIsOwned: boolean = false); overload;
    (*<summary>Define value as vector using existing vector.</summary>*)
    procedure DefineVector(const Vec: TVec; aIsOwned: boolean = false); overload;
    (*<summary>Define range value as vector using existing vector.</summary>*)
    procedure DefineExtRange(const Vec: TVec; aIsOwned: boolean = false); overload;
    (*<summary>Define value as matrix.</summary>*)
    procedure DefineMatrix; overload;
    (*<summary>Define value as matrix using existing matrix.</summary>*)
    procedure DefineMatrix (const mtx: TMtx; aIsOwned: boolean = false); overload;

    procedure Undefine;
    procedure InternalDefineOwned(const srcRec: TValueRec);

    property CustomValue: TObject read fCustomValue write SetCustomValue;
    (*<summary>Returns value as string.</summary>*)
    property AsString: string read GetAsString;
    (*<summary>Gets or sets value type.</summary>*)
    property ValueType: TExprValueType read fValueType write SetValueType;
    (*<summary>Returns value as double.</summary>*)
    property DoubleValue: double read fDoubleValue write fDoubleValue;
    (*<summary>Returns value as complex.</summary>*)
    property ComplexValue: TCplx read fComplexValue write fComplexValue;
    (*<summary>Returns value as vector.</summary>*)
    property VectorValue: TVec read GetVectorValue write SetVectorValue;
    (*<summary>Returns value as matrix.</summary>*)
    property MatrixValue: TMtx read fMatrixValue write SetMatrixValue;
    (*<summary>Returns value as range.</summary>*)
    property ExtRangeValue: TVec read fVectorValue write SetVectorValue;
    (*<summary>Returns value as string value.</summary>*)
    property StringValue: string read FStringValue write fStringValue;
    (*<summary>Returns value as Bool vector.</summary>*)
    property BoolValue: boolean read fBoolValue write fBoolValue;
    (*<summary>Returns value as Bool vector.</summary>*)
    property BoolVectorValue: TVecInt read FIntVectorValue write SetIntVectorValue; 
    (*<summary>Returns value as Bool vector.</summary>*)
    property BoolMatrixValue: TMtxInt read FIntMatrixValue write SetIntMatrixValue; 
    (*<summary>Returns value as integer.</summary>*)
    property IntegerValue: Int64 read fIntegerValue write SetIntegerValue;
    (*<summary>Returns value as Bool vector.</summary>*)
    property IntVectorValue: TVecInt read GetIntVectorValue write SetIntVectorValue;
    (*<summary>Returns value as Bool vector.</summary>*)
    property IntMatrixValue: TMtxInt read fIntMatrixValue write SetIntMatrixValue;

    property StringListValue: TStringList read FStringListValue write SetStringListValue;
    (*<summary>For internal use.</summary>*)
    property Tag: NativeInt read FTag write SetTag;
    (*<summary>Causes an exception, if undefine is called.</summary>
               
<remarks>Prevents ValueType to be reset. Usefull for externally defined types.
</remarks>
*)
    property ValueTypeLock: boolean read FValueTypeLock write SetValueTypeLock;

    property RangeIntPrecision: TIntPrecision read FRangeIntPrecision write SetRangeIntPrecision;
    property ExtRange: boolean read fExtRange write fExtRange;
    property OnUndefine: TNotifyEvent read FOnUndefine write SetOnUndefine;

    procedure PromoteRangeToVector;
  end;

  TExprGridVariable = class;

  
  TExprGridSetProc = procedure(const Sender: TExprGridVariable; const Src: string; const row, col: integer) of object;

  
  TExprGridGetProc = procedure(const Sender: TExprGridVariable; var Dst: string; const row, col: integer) of object;


  (*<summary>Custom variable object type.</summary>
             
<remarks>Define TExprGridVariable object with TValueRec.DefineCustomValue. The object provides callbacks used when
             reading or writing to the variable. These callbacks can be used for example to read and write from a string
             grid or to some external large storage.
</remarks>
*)
  TExprGridVariable = class(TObject)
  strict private
     FOnGetValue: TExprGridGetProc;
     FOnSetValue: TExprGridSetProc;
     FReFormat: string;
     FImFormat: string;
     FScientific: boolean;
     FCols: integer;
     FRows: integer;

     procedure SetOnGetValue(const Value: TExprGridGetProc);
     procedure SetOnSetValue(const Value: TExprGridSetProc);
     procedure SetImFormat(const Value: string);
     procedure SetReFormat(const Value: string);
     procedure SetScientific(const Value: boolean);
     procedure SetCols(const Value: integer);
     procedure SetRows(const Value: integer);
  strict protected
     procedure SetValue(const Src: TValueRec; const Index: integer); overload; virtual;
     procedure SetValue(const Src: TValueRec; const IndexStart, Step, IndexStop: integer); overload; virtual;

     procedure GetValue(const Dst: TValueRec; const Index: integer); overload; virtual;
     procedure GetValue(const Dst: TValueRec; const IndexStart, Step, IndexStop: integer); overload; virtual;

     procedure SetValue(const Src: TValueRec; const row, col: integer); overload; virtual;
     procedure SetRowValue(const Src: TValueRec; const row, colStart, colStep, colStop: integer); overload; virtual;
     procedure SetColValue(const Src: TValueRec; const rowStart, rowStep, RowStop, col: integer); overload; virtual;
     procedure SetValue(const Src: TValueRec; const rowStart, rowStep, RowStop, colStart, colStep, colStop: integer); overload; virtual;

     procedure GetValue(const Dst: TValueRec; const row, col: integer); overload; virtual;
     procedure GetRowValue(const Dst: TValueRec; const row, colStart, colStep, colStop: integer); overload; virtual;
     procedure GetColValue(const Dst: TValueRec; const rowStart, rowStep, RowStop, col: integer); overload; virtual;
     procedure GetValue(const Dst: TValueRec; const rowStart, rowStep, RowStop, colStart, colStep, colStop: integer); overload; virtual;
  public
     function getLength: integer;
  public
    (*<summary>Specifies formating of real and complex numbers to use full scientific notation (1.2313E+20 for example).</summary>*)
     property Scientific: boolean read FScientific write SetScientific;
    (*<summary>Specifies formating for real numbers and real part of complex numbers.</summary>*)
     property ReFormat: string read FReFormat write SetReFormat;
    (*<summary>Specifies formating for imaginary part f complex numbers.</summary>*)
     property ImFormat: string read FImFormat write SetImFormat;
    (*<summary>Event triggered when value of an external structure is to be set.</summary>*)
     property OnSetValue: TExprGridSetProc read FOnSetValue write SetOnSetValue;
    (*<summary>Event triggered when value of an external structure is to be fetched from.</summary>*)
     property OnGetValue: TExprGridGetProc read FOnGetValue write SetOnGetValue;
    (*<summary>Specifies row count of the grid object.</summary>
               
<remarks>Set this value to the limit to which the user can set this value, its usable range, or leave at 0.
</remarks>
*)
     property Rows: integer read FRows write SetRows;
    (*<summary>Specifies column count of the grid object.</summary>
               
<remarks>Set this value to the limit to which the user can set this value, its usable range, or leave at 0.
</remarks>
*)
     property Cols: integer read FCols write SetCols;
    (*<summary>Returns total number of values.</summary>
        
<remarks>Returns total number of values computed with Rows*Cols.
</remarks>
*)
     property Length: integer read getLength;
  end;

  
  TStringValue = class (TValueRec)
  public
    constructor Create;
    property Value: string read fStringValue write SetStringValue;
  end;

  TIntegerValue = class (TValueRec)
  public
    constructor Create;
    property Value: Int64 read fIntegerValue write fIntegerValue;
  end;

  TRangeValue = class (TValueRec)
  public
    constructor Create;
    property Value: TIntegerArray read fRange write fRange;
  end;

  TDoubleValue = class (TValueRec)
  public
    constructor Create;
    property Value: double read fDoubleValue write fDoubleValue;
  end;

  TBoolValue = class (TValueRec)
  public
    constructor Create;
    property Value: boolean read fBoolValue write fBoolValue;
  end;

  TComplexValue = class (TValueRec)
  public
    constructor Create;
    property Value: TCplx read fComplexValue write fComplexValue;
  end;

  TIntVectorValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const Vec: TVecInt); overload;
    property Value: TVecInt read fIntVectorValue write SetIntVectorValue;
  end;

  TStringListValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const strList: TStringList); overload;
    property Value: TStringList read fStringListValue write SetStringListValue;
  end;

  TIntMatrixValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const mtx: TMtxInt); overload;
    property Value: TMtxInt read fIntMatrixValue write SetIntMatrixValue;
  end;

  TBoolVectorValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const Vec: TVecInt); overload;
    property Value: TVecInt read fIntVectorValue write SetIntVectorValue;
  end;

  TBoolMatrixValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const mtx: TMtxInt); overload;
    property Value: TMtxInt read fIntMatrixValue write SetIntMatrixValue;
  end;

  TVectorValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const Vec: TVec); overload;
    property Value: TVec read fVectorValue write SetVectorValue;
  end;

  TCustomValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const obj: TObject); overload;
    property Value: TObject read fCustomValue write SetCustomValue;
  end;

  TMatrixValue = class (TValueRec)
  public
    constructor Create; overload;
    constructor Create(const mtx: TMtx); overload;
    property Value: TMtx read fMatrixValue write SetMatrixValue;
  end;

  TExprFuncObj = procedure (const Param: TExprRec) of object;

  TExprRec = class 
  public
    
    Oper: TExprFunc;
    OperObj: TExprFuncObj;
    Next: TExprRec;
    Res: TValueRec;   
    tmpRes: TValueRec; 
    ExprWord: TExprWord;
    ArgCount: integer;
    Args: array of TValueRec; 
    ArgList: array of TExprRec; 

    procedure ResizeArgs(NewArgs: integer);
    constructor Create;
    destructor Destroy; override;
  end;

  TExprWord = class 
  strict protected
    fName: string;
    fLexemeType: TLexemeType;
    FMaxArgCount: integer;
    FVariableArgCount: boolean;
    function GetFunc: TExprFunc;
    function GetHelp: string;
    function GetFuncObj: TExprFuncObj;
    procedure SetMaxArgCount(const Value: integer);
    procedure SetVariableArgCount(const Value: boolean);
    procedure SetNResultArg(const Value: integer);
    procedure SetVRecOwned(const Value: boolean);
  strict protected
    fFunc: array [0..MaxArg - 1] of TExprFunc; 
    fFuncObj: array [0..MaxArg - 1] of TExprFuncObj; 
    fHelp: array [0..MaxArg - 1] of string;
    fNFuncArg: integer;
    fNResultArg: integer;
    fOperPrec: integer;
    fVRec: TValueRec;
    fVRecIsOwned: boolean;
    procedure SetNFuncArg(ANFuncArg: integer); virtual;
  public
    constructor Create (const AName: string; ALexemeType: TLexemeType); overload;
    constructor Create (const AName: char; ALexemeType: TLexemeType); overload;
    destructor Destroy; override;

    function HasOverload(ArgCount: integer): boolean;
    function CanHaveZeroArguments: boolean;
    procedure DefineExternalVariable(const Src: TValueRec; const IsSrcOwned: boolean = false);

    property Name: string read fName;
    property Func: TExprFunc read GetFunc;
    property LexemeType: TLexemeType read fLexemeType;
    property OperPrec: Integer read fOperPrec;
    property VRec: TValueRec read fVRec;
    property VRecOwned: boolean read FVRecIsOwned;
    property Help: string read GetHelp;
    property FuncObj: TExprFuncObj read GetFuncObj;
    property MaxArgCount: integer read FMaxArgCount write SetMaxArgCount;
    
    property NFuncArg: Integer read fNFuncArg write SetNFuncArg;
    property NResultArg: integer read FNResultArg write SetNResultArg;
    
    property VariableArgCount: boolean read FVariableArgCount write SetVariableArgCount;
  end;

  TExprSemicolon = class (TExprWord)
  public
    constructor Create;
  end;

  TExprLeftBracket = class (TExprWord)
  public
    constructor Create;
  end;

  TExprRightBracket = class (TExprWord)
  public
    constructor Create;
  end;

  TExprLeftSquareBracket = class (TExprWord)
  public
    constructor Create;
  end;

  TExprRightSquareBracket = class (TExprWord)
  public
    constructor Create;
  end;

  TExprComma = class (TExprWord)
  public
    constructor Create;
  end;

  TExprConstant = class (TExprWord)
  public
    constructor Create (const AName: string; const AValue: string; const ConstantsAlwaysReal: boolean);
  end;

  TExprStringConstant = class (TExprWord)
  public
    constructor Create(const AValue: string); overload;
    constructor Create(const AName: string; const AValue: string; const Help: string); overload;
  end;

  TExprDoubleConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: double; const Help: string);
  end;

  TExprComplexConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TCplx; const Help: string);
  end;

  TExprBoolConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: boolean; const Help: string);
  end;

  TExprIntegerConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: Int64; const Help: string);
  end;

  TExprRangeConstant = class (TExprWord)
  public
    constructor Create (const AName: string; const AValue: TIntegerArray; const Help: string);
  end;

  TExprVectorConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TVec; const Help: string);
  end;

  TExprStringListConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TStringList; const Help: string);
  end;

  TExprExtRangeConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TVec; const Help: string);
  end;

  TExprBoolVectorConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TVecInt; const Help: string);
  end;

  TExprIntVectorConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TVecInt; const Help: string);
  end;

  TExprMatrixConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TMtx; const Help: string);
  end;

  TExprIntMatrixConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TMtxInt; const Help: string);
  end;

  TExprBoolMatrixConstant = class (TExprWord)
  public
    constructor Create (const AName: string; AValue: TMtxInt; const Help: string);
  end;

  TExprVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprDoubleVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprIntegerVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprBoolVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprRangeVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprStringVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprComplexVariable = class (TExprWord)
  public
    constructor Create (const AName: string);
  end;

  TExprVectorVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const Vec: TVec); overload;
  end;

  TExprCustomValueVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const obj: TObject); overload;
  end;

  TExprIntVectorVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const Vec: TVecInt); overload;
  end;

  TExprBoolVectorVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const Vec: TVecInt); overload;
  end;

  TExprMatrixVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const mtx: TMtx); overload;
  end;

  TExprIntMatrixVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const mtx: TMtxInt); overload;
  end;

  TExprBoolMatrixVariable = class (TExprWord)
  public
    constructor Create (const AName: string); overload;
    constructor Create (const AName: string; const mtx: TMtxInt); overload;
  end;

  TExprFunction = class (TExprWord)
  strict protected
    procedure SetNFuncArg(ANFuncArg: integer); override;
  public
    constructor Create (const AName: string; AFunc: TExprFunc;  ANFuncArg, ANResultArg: Integer; const Help: string); overload;
    constructor Create (const AName: string; ANFuncArg, ANResultArg: Integer; const AFunc: TExprFuncObj; const Help: string); overload;
    procedure AddOverload(const Src: TExprWord);
    procedure RemoveOverload(ANFuncArg: integer);
  end;

  TExprVaryingFunction = class (TExprWord)
  strict protected
    procedure SetNFuncArg(ANFuncArg: integer); override;
  public
    constructor Create(const AName: string; AFunc: TExprFunc;ANFuncArg, ANResultArg: Integer; const Help: string); overload;
    constructor Create(const AName: string; const ANFuncArg, ANResultArg: Integer; const AFunc: TExprFuncObj; const Help: string); overload;
    procedure AddOverload(const Src: TExprWord);
    procedure RemoveOverload(ANFuncArg: integer);    
  end;

  TExprOperator = class (TExprWord)
  strict protected
    procedure SetNFuncArg(ANFuncArg: integer); override;  
  public
    constructor Create (const AName: string; AFunc: TExprFunc; ANFuncArg: Integer; AOperPrec: Integer; const Help: string); overload;
  end;
  

  (*<summary>Named variables collection item.</summary>*)
  TNamedVariable = class (TMtxCollectionItem)
  private
    fVarName: string;
  strict private
    fValue: TValueRec;
    procedure SetValue(const Value: TValueRec);
    procedure SetVarName(const Value: string);
  strict protected
   
    procedure AssignTo (Dest: TPersistent); override;
   
  public
    
    constructor Create(Collection: TCollection); override;
    
    destructor Destroy; override;
  published
    (*<summary>Variable name.</summary>*)
    property VarName: string read fVarName write SetVarName;
    (*<summary>Variable properties.</summary>
      <seealso cref="TValueRec"/>*)
    property Value: TValueRec read fValue write SetValue;
  end;

  (*<summary>Collection of <see cref="TNamedVariable"/>s.</summary>*)
  TVariableCollection = class (TMtxCollection)
  strict private
    function GetItems(Idx: integer): TNamedVariable;
    procedure SetItems(Idx: integer; const Value: TNamedVariable);
  public
    constructor Create;
    (*<summary>Adds new named variable.</summary>*)
    function Add (const var_name: string): TNamedVariable; overload;
    function Insert (Idx: integer): TNamedVariable; overload;
    (*<summary>Find named variable by it's <see cref="TNamedVariable.VarName"/> property.</summary>*)
    function FindByName (const var_name: string): TNamedVariable;
    (*<summary>Access named variable by it's index in collection.</summary>*)
    property Items[Idx: integer]: TNamedVariable read GetItems write SetItems; default; 
  end;

  

  (*<summary> Expression context, holding expression list and variables with type and the data they hold. </summary>*)
  TExprContext = class(TPersistent)
  strict private
    fExpression: string;
    fVariables: TVariableCollection;
    procedure SetVariables(const Value: TVariableCollection);
  protected
    procedure AssignTo (dest: TPersistent);  override; 
  public
    
    constructor Create;
    destructor Destroy; override;

    

    
    procedure SaveToStream(const Dst: TStream);
    procedure LoadFromStream(const Src: TStream);
    

    procedure SaveToFile(const Dst: string; const Append: boolean = false);
    procedure LoadFromFile(const Src: string);
  published
    (*<summary>Returns expression as string.</summary>*)
    property Expression: string read fExpression write fExpression;
    (*<summary>Variables collection.</summary>
      <seealso cref="TNamedVariable"/>*)
    property Variables: TVariableCollection read fVariables write SetVariables;
  end;

  (*<summary> Stores workspace for expression parser. </summary>
               
<remarks>The workspace includes command line history, names and values of named variables, script names
               and corresponding scripts (list of expressions). All data properties need to be assigned by the programmer.
               Use the object to load and save the workspace to/from files.
</remarks>
*)

  TExprWorkspace = class(TPersistent)
  strict private
      FName: string;
      FHistory: TStringList;
      FScripts: TStringList;
      FVariables: TExprContext;
      FScriptNames: TStringList;

      procedure SetHistory(const Value: TStringList);
      procedure SetName(const Value: string);
      procedure SetScripts(const Value: TStringList);
      procedure SetVariables(const Value: TExprContext);
      procedure SetScriptNames(const Value: TStringList);
   public
      procedure Assign(Src: TPersistent);  override; 
      constructor Create;
      destructor Destroy; override;

      

      
      procedure SaveToStream(const Dst: TStream);
      procedure LoadFromStream(const Src: TStream);
      

      procedure SaveToFile(const Dst: string; const Append: boolean = false);
      procedure LoadFromFile(const Src: string);

      (*<summary> The name of the workspace. </summary>*)
      property Name: string read FName write SetName;
      (*<summary> List of strings to store command line history. </summary>*)
      property History: TStringList read FHistory write SetHistory;
      (*<summary> Stores lists of scripts. </summary>
                   
<remarks>Each item in the list holds delimited list of one script. To retreive the script assign the Scripts[i]
                   to TStringList.Text. Equally, assign TStrignList.Text to Scripts[i] to store a specific script.
                   It is programmers responsability to maintain coherence between both lists: Scripts and ScriptNames.
</remarks>
*)
      property Scripts: TStringList read FScripts write SetScripts;
      (*<summary> Stores the names of scripts in the Script property. </summary>
                   
<remarks>It is programmers responsability to maintain coherence between both lists: Scripts and ScriptNames.
</remarks>
*)
      property ScriptNames: TStringList read FScriptNames write SetScriptNames;
      (*<summary> Stores names and data of named variables. </summary>
                   
<remarks>TExprContext can be filled with a call to TMtxExpression.SaveContext/LoadContext.
</remarks>
*)
      property Variables: TExprContext read FVariables write SetVariables;
     end;

  

  TExprWordList = class(TObjectsList)
  strict private
      procedure SetItems(i: integer; const Value: TExprWord);
      function GetItems(i: integer): TExprWord;
  public
      property Item[i: integer]: TExprWord read GetItems write SetItems; default; 
      function Find(const Name: string; var Index: integer): boolean;
      
      procedure Sort; overload;
      
      procedure CheckForDuplicates; virtual;
      constructor Create(AOwnsObjects: Boolean); overload;
      constructor Create; overload;
  end;

  TWordList = class;

  TWordPopulate = procedure(const Sender: TWordList);
  TExprTest = procedure(Sender: TObject);

  TWordList = class(TExprWordList)
  strict private
    FOnDelete: TNotifyEvent;
    procedure SetOnDelete(const Value: TNotifyEvent);
  strict private
    class var FOnProbabilities: TWordPopulate;
    class var FOnStatistics: TWordPopulate;
    class var FOnSignal: TWordPopulate;
    class var FOnCustom: TWordPopulate;


    class var FOnTestProbabilities: TExprTest;
    class var FOnTestCustom: TExprTest;
    class var FOnTestStatistics: TExprTest;
    class var FOnTestSignal: TExprTest;

    class procedure SetOnSignal(const Value: TWordPopulate); static;
    class procedure SetOnStatistics(const Value: TWordPopulate); static;
    class procedure SetOnCustom(const Value: TWordPopulate); static;
    class procedure SetOnProbabilities(const Value: TWordPopulate); static;

    class procedure SetOnTestCustom(const Value: TExprTest); static;
    class procedure SetOnTestProbabilities(const Value: TExprTest); static;
    class procedure SetOnTestSignal(const Value: TExprTest); static;
    class procedure SetOnTestStatistics(const Value: TExprTest); static;
  strict protected





  public
    function DefineFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments, NResults: Integer; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; OverloadIdx: integer = -1): integer; overload;
    function DefineFunctionUnsorted(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj; OverloadIdx: integer = -1): integer; overload;
    function DefineFunctionUnsorted(const AFunctName: string; NArguments, NResults: Integer; const AFuncAddress: TExprFuncObj; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; NArguments, NResults: Integer; const AFuncAddress: TExprFuncObj;  const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments, NResults: Integer; const Help: string; OverloadIdx: integer = -1): integer; overload;
    function DefineVaryingFunctionUnsorted(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; OverloadIdx: integer = -1): integer; overload;
    function DefineOperatorUnsorted(const AOperatorSign: string; AFuncAddress: TExprFunc; NArguments: Integer; Precedence: Integer; const Help: string): integer; overload;
    function DefineOperatorUnsorted(const AOperatorSign: char; AFuncAddress: TExprFunc; NArguments: Integer; Precedence: Integer; const Help: string): integer; overload;
    function DefineOperatorUnsorted(const AOperatorSign: string; AFuncAddress: TExprFunc; NArguments: Integer; Precedence: Integer): integer; overload;
    procedure DefineDoubleConstantUnsorted(const AConstName: string; Value: double; const Help: string); overload;
    procedure DefineDoubleConstantUnsorted(const AConstName: string; Value: double); overload;
    procedure DefineComplexConstantUnsorted(const AConstName: string; const Value: TCplx; const Help: string); overload;
    procedure DefineComplexConstantUnsorted(const AConstName: string; const Value: TCplx); overload;

    function DefineBoolConstantUnsorted(const AConstName: string; Value: boolean): integer; overload;
    function DefineBoolConstantUnsorted (const AConstName: string; Value: boolean; const Help: string): integer; overload;

    function DefineIntegerConstantUnsorted(const AConstName: string; Value: Integer): integer; overload;
    function DefineIntegerConstantUnsorted (const AConstName: string; Value: Integer; const Help: string): integer; overload;

    (*<summary> Adds and item maintains the sorting order </summary>
                
<remarks>Finds the position where to insert and potentially also removes existing item and replaces.
                The bisection based search is faster than linear, but when adding large amounts of Item use the AddUnsorted overload and sort explicitely after the adding.
</remarks>
*)
    function Add(const Item: TExprWord): integer;
    (*<summary> Adds, but requires sorting. </summary>
                
<remarks>Usefull for adding large amounts of Items in batch mode.
</remarks>
*)
    function AddUnsorted(const Item: TExprWord; OverloadParentIdx: integer = -1): integer;
    property OnDelete: TNotifyEvent read FOnDelete write SetOnDelete;

    procedure DoOnProbabilities;
    procedure DoOnStatistics;
    procedure DoOnSignal;
    procedure DoOnCustom;

    constructor Create(AOwnsObjects: Boolean); overload;
    constructor Create; overload;
  public
    class property OnProbabilities: TWordPopulate read FOnProbabilities write SetOnProbabilities;
    class property OnStatistics: TWordPopulate read FOnStatistics write SetOnStatistics;
    class Property OnSignal: TWordPopulate read FOnSignal write SetOnSignal;
    class property OnCustom: TWordPopulate read FOnCustom write SetOnCustom;

    class property OnTestProbabilities: TExprTest read FOnTestProbabilities write SetOnTestProbabilities;
    class property OnTestStatistics: TExprTest read FOnTestStatistics write SetOnTestStatistics;
    class Property OnTestSignal: TExprTest read FOnTestSignal write SetOnTestSignal;
    class property OnTestCustom: TExprTest read FOnTestCustom write SetOnTestCustom;
  end;

  TExprFlowControl = (
    efcNone,
    efcConditionalJump,
    efcUnconditionalJump
  );

  TExprFlowKeyword = (
    eccNone,
    eccIf,
    eccElse,
    eccEnd,
    eccWhile,
    eccFor,
    eccEndFor,
    eccBreak,
    eccContinue
  );

  TSingleExpression = class
  public
    Str: string;
    Rec: TExprRec;
    Res: TExprRec;
    Words: TExprWordList; 
    ResultName: string;
    FlowControl: TExprFlowControl; 
    NextLineIndex: integer; 
    JumpLineIndex: integer; 
    NestingDepth: integer;
    FlowKeyword: TExprFlowKeyword;
    destructor Destroy; override;
  end;

  TExprList = class(TObjectsList)
  strict private
    procedure SetItems(i: integer; const Value: TSingleExpression); 
    function GetItems(i: integer): TSingleExpression; 
  public
    property Item[i: integer]: TSingleExpression read GetItems write SetItems; default; 
  end;

  procedure UnsupportedValueTypesError(const Param: TExprRec; const vt: TExprValueType); overload;
  procedure UnsupportedValueTypesError(const Param: TExprRec; const vt1, vt2: TExprValueType); overload;
  procedure UnsupportedValueTypesError(const Param: TExprRec; const vt1, vt2, vt3: TExprValueType); overload;
  procedure UnsupportedValueTypesError(const Param: TExprRec; const vt1, vt2, vt3, vt4: TExprValueType); overload;
  procedure UnsupportedValueTypesError(const Param: TExprRec; const vt1, vt2, vt3, vt4, vt5: TExprValueType); overload;

  function StrToSimpleType(const Src: string): TSimpleType;
  procedure DisposeList(var ARec: TExprRec);

