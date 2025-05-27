











(*<summary>Expression parser and function evaluator.</summary>
  
<remarks>Holds classes and components for math expression parser, evaluator
  and scripter.
</remarks>
*)
unit MtxParseExpr;


interface

{$I BdsppDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}

uses
       
     Classes,
     
     Contnrs,
     
     
     

     Math387, MtxVec, MtxParseClass, MtxBaseComp, MtxVecInt, Probabilities
     ,Polynoms, MtxVecBase, AbstractMtxVec, AbstractMtxVecInt

         

         

         
         ,Types
         

     
         
         ,Windows
         
     
     
     ;

    
    


type

  TMtxExpression = class;

  TVarByNameIndexer = class
  strict private
    fOwner: TMtxExpression;
    function GetVarByName(const varname: string): TValueRec;
  public
    property Value[const varname: string]: TValueRec read GetVarByName; default;
    constructor Create(aOwner: TMtxExpression);
    destructor Destroy; override;
  end;

  TExprResultIndexer = class
  strict private
    fOwner: TMtxExpression;
    function GetExprResult(const i: integer): TValueRec;
  public
    property Value[const i: integer]: TValueRec read GetExprResult; default;
    constructor Create(aOwner: TMtxExpression);
    destructor Destroy; override;
  end;

  (*<summary>Event type for the preprocessor.</summary>

    *)
  TOnPreprocessor = procedure (Sender: TObject; Expr: TExprWordList) of object;

  (*<summary>Expression evaluator.</summary>
    
<remarks>The object is a compiler for mathematical expressions. It holds
    the lists of all compiled expression and all recognized symbols.

  The user can define any number of user defined variables and functions. The
  functions can be overloaded by specifying different parameter count. Parameters
  to functions and result of functions can also be strings. User defined
  functions can be functions or methods (of object).

  The math expressions can mix scalar, vector or matrix type.
  The following standard operators are supported:

  * ', the transpose/adjungate operator for matrices
  * power operator ^
  * standard operators working on vector, matrix and scalar *, /, + , -
  * The per element operators working on matrices and vectors *. , /.
  If you dont specify per element operation, linear algebra logic will be
  used for * and / when operands are matrices
  * factorial operator !
  * percentage operator %
  * back divison operator for matrices \ , A\B = A^(-1)*B, which comes from A*X = B
    and gives a solution to the system of linear equations. If matrices are not square
    LQR (minimum norm) solution is computed.
  * div and mod for integer division
  * comparison operators &lt; , &lt;= , &gt; , &gt;= , ==
  * binary operators: and, not, or, xor
  * assign operator, =
  * colon operator (see below)

  The "i" defines a complex number:

  <c>a = 2+3i;</c>

  All operations support complex numbers. When using expressions which can result in a complex
  number like: (-1)^(1/3), the result will be complex, only if arguments are complex.
  The expression will also not auto reduce to a real number, even if imaginary part of the
  complex number is 0. To reduce the result to the real number part only, you have to call
  function real(x). To explicitely form a complex number use the cplx(re, im) function.   The
  iscomplex(x) function returns 1 if the variable is complex and 0 otherwise.

  The type of variable is determined at compile time and may not change at run time.
  Vectors and matrices can be accessed by individual elements:

  <c>a(i)</c><para/>
  <c>m(r,c)</c>

  or in ranges by using the colon operator:

  <c>a(0:9)</c>, returns elements from index 0 to including 9.<para/>
  <c>m(1:2,2:3)</c>, returns sub matrix.

  First element is referenced with index 0.
  The colon can also be used on the left side during assignment:

  <c>a(0:9) = b(1:10);</c>

  Optionally the colon operator also allows a step definition:

  <c>a = 20:-1:-20;</c>

  To return the matrix as a vector:

  <c>v = m(:);</c>

  Expressions with the colon operator are tightly optimized.
  The colon expression is only expanded to vector if needed.
  The variable returning result of colon expression uses
  <see cref="TVec.SetSubRange"/>. Consequently:

  <c>b = a(1:10);</c>

  Translates to one single TVec.Copy(..) method call.
  In expressions there is also no "copy" overhead. The following
  expression requires one call to TVec.Mul(..) and one TVec.Copy(..):

  <c>c = a(1:10)*b(11:20);</c>

  Vectors and matrices can also return elements from conditions:

  <c>a = m(m &gt; 4);</c>

  Despite the advanced vector/matrix syntax the single scalar syntax like:

  <c>c = Log(b) + d;</c>

  will still execute at top speed because type resolution is done at compile time.
  The constant expressions like 4*4 and 2^3 or even:

  <c> sin(3.2) - sqrt(cos(1.5)) </c>

  are also evaluated at compile time.

  It is of course possible to evaluate a list of expressions:

  <c>a = 1;</c><para/>
  <c>b = 2;</c><para/>
  <c>c = a + b;</c>

  where the variables will remain in the memory until cleared.  Currently there
  is no support for "for" and "while" loops or in-script function definitions.

  Other operands included:
  * comparison: <c> &gt; &lt; &lt;&gt; = &lt;= &gt;= </c>
  * <c>*., /., +., -</c>. to indicate per element operation on vector matrix pairs
  * factorial: <c>!</c>
  * percentage: <c>%</c>

 Assignment operator is always required to be a simple " = " char, but in general
 the user can customize everything else.

 The precedence of the operands is little different from Pascal (Delphi), giving
 a lower precedence to logical operands, as these only act on booleans
 (and not on integers like in Pascal)
  1. (highest): <c>! -x +x %</c>
  2. <c>^</c>
  3. <c>* / div mod</c>
  4. <c>+ -</c>
  5. <c> &gt; &gt;= &lt; &lt;= &lt;&gt; =</c>
  6. <c>not</c>
  7. <c>or and xor</c>
  8. (lowest) <c>=</c>

 This precedence order is easily customizable by overriding/changing InitSymbols

 Optionally the operators can be redefined, but the assignment (equal) operator can not
 be overriden.

 The following class properties:

 MtxParseClass.TWordList.OnProbabilities
 MtxParseClass.TWordList.OnStatistics
 MtxParseClass.TWordList.OnSignal
 MtxParseClass.TWordList.OnCustom

 hold the callback functions to declare functions from the corresponding software packages.
 If you dont want for example the Probabilities functions to be avaialble when creating
 new TMtxExpression, set that property to nil.

 The callbacks are of type MtxParseClass.TWordPopulate. You can assign your own callback to OnCustom event
 for custom defined function domains. The following units need to be included in your project for the first
 three callbacks to be populated: MtxParseProbabilities, MtxParseSignal, MtxParseStatistics.

 A large set of functions and constants is already predefined.
 
 Note:
 Originally adapted from TExpressionParser written by Egbert van Nes
 (<see href="mailto:Egbert.vanNes@Aqec.WKAO.WAU.NL">Egbert.vanNes@Aqec.WKAO.WAU.NL</see>) with permission.
</remarks>


  

      <example>
        Use parser to evaluate the "Sqrt(X*X + Y*Y)" formula where x=3 and y=4.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TDoubleValue x = MyParser.DefineDouble("x");
          TDoubleValue y = MyParser.DefineDouble("y");

          // 2. Set values
          x.DoubleValue = 3.0;
          y.DoubleValue = 4.0;

          // 3. Add formula and evaluate,
          MyParser.AddExpr("Sqrt(x*x+y*y)");
          double res = MyParser.EvaluateDouble();
          // res = 5
        }
      }
      </code></example>*)
  TMtxExpression = class(TPersistent)
  strict private
    fExprList: TExprList;
    fWordList: TWordList;
    fConstList: TExprWordList;
    FOnPreprocessor: TOnPreprocessor;
    ReturnLine: TIntegerArray;
    BreakContinueLine: T2DIntegerArray; 
    ewWhile, ewEnd, ewIf, ewElse, ewFor, ewEndFor, ewBreak, ewContinue, ewTrue, ewMtxVecEOA: TExprWord;
    ewComma, ewLeftBracket, ewRightBracket, ewRowConcat, ewColConcat, ewAssign, ewAssignOp, ewSelect, ewColonOp: TExprWord;
    FTestIntPrecision: TIntPrecision;
    fIsCompiled: boolean;
    FConstantsAlwaysReal: boolean;
    FConcatsAlwaysReal: boolean;
    fvarByNameIndexer: TVarByNameIndexer;
    fExprResultIndexer: TExprResultIndexer;
    FEvaluatedLine: integer;
    function ParseString(const AnExpression: string): TExprWordList;
    function MakeTree(const Expr: TExprWordList): TExprRec;
    function MakeLinkedList(ExprRec: TExprRec): TExprRec;
    procedure Check(const Expr: TExprWordList);
    function  CheckArguments(const ExprRec: TExprRec): Boolean;
    procedure DisposeTree(var ExprRec: TExprRec);
    function EvaluateList(ARec: TExprRec): TExprRec; 
    function RemoveConstants(ExprRec: TExprRec): TExprRec;
    function ResultCanVary(const ExprRec: TExprRec): Boolean;
    procedure GetSymbolList (const names,help: TStrings; lexeme_type: TLexemeTypes; arg_list: boolean);
    function GetExpression(Index: integer): string;
    function GetExprCount: integer;
    function GetExpressions: string;
    procedure SetExpression(Index: integer; const Value: string);
    procedure SetExpressions(const Value: string);
    procedure CreateExprRec(const se: TSingleExpression);
    procedure InitConsts;
    procedure InitSymbols;
    procedure InitControlWords;
    procedure InitFileManagement;
    procedure Preprocessor(const Expr: TExprWordList);
    procedure SetOnPreprocessor(const Value: TOnPreprocessor);
    function EvaluateResultName(const a: TExprRec): string;
    procedure SetTestIntPrecision(const Value: TIntPrecision);
    function EvaluateFlowKeyword(const aList: TExprWordList): TExprFlowKeyword;

    procedure CreateExprWords(const se: TSingleExpression);
    procedure CreateExprRec2(const se: TSingleExpression);
    procedure Run(const anExpr: string);
    procedure SetConstantsAlwaysReal(const Value: boolean);
    procedure SetConcatsAlwaysReal(const Value: boolean);
    function GetLastResult: TValueRec;
    procedure SetEvaluatedLine(const Value: integer);
  protected
    procedure TestFileFunctions;
    procedure TestSelectAssign;
    procedure DoDeleteWord(Sender: TObject);
    procedure CompileInternal;
    property TestIntPrecision: TIntPrecision read FTestIntPrecision write SetTestIntPrecision;
  public
    function GetExprResult(Index: integer): TValueRec;
    function GetVarByName(const aVarName: string): TValueRec;
  public
    (*<summary>Returns True, if the expressions were already compiled.</summary>
               
<remarks>Compile pass determines variables and checks program flow, but variable type is not set until the first run.
</remarks>
*)
    property IsCompiled: boolean read fIsCompiled;

    (*<summary>Prints the value "a" of variable with "aName"  to Dst.</summary>
               
<remarks>Adds lines to Dst to display the contents of variable in a on a command line like user interface.
               LenLimit defines the maximum number of values that will be printed, if the structure contains more than LenLimit items.
</remarks>
*)
    procedure VarPrint(a: TValueRec; const aName: string; const Dst: TStrings; const LenLimit: integer = 2147483647);

    (*<summary>Returns TExprWord, if a symbol with aName already exists.</summary>
               
<remarks>Usefull for testing unique var names, which dont overlapp with other elements like constants, functions etc...
               Returns nil, if not found.
</remarks>
*)
    function FindSymbol(const AName: string): TExprWord;
    (*<summary>Returns a word from Src string close to charIdx.</summary>
               
<remarks>The function expects one code line in Src parameter and will return the word or the variable which
               is close to char at charIdx.
</remarks>
*)

    function ExpandCharToWord(const Src: string; charIdx: integer): string;
    (*<summary>Returns Tool-Tip or the Hint string to be displayed for the variable with aName.</summary>
     
<remarks>The value of the variable needs to be passed as param "a" and the name of the variable as "aName".
     Typically one would write:

     <code>

     aVal := expr.VarByName[aName];
     if Assigned(aVal) then   //var was found
     begin
         ShowHint := True;
         Hint := expr.VarToolTip(aVal, aName);
     end ShowHint := False;

     </code>

     The returned string will contain the name of the variable, its type and the value(s).
</remarks>
*)
    function VarToolTip(const aName: string; const a: TValueRec): string;
    (*<summary>Performs compile of all the expressions in the list.</summary>
               
<remarks>Most of the time not needed, because functions compile automatically, if the compile was not yet performed.
</remarks>
*)
    procedure Compile;
    (*<summary>Runs the compiled script.</summary>*)
    function  EvaluateCompiled: TValueRec;
    (*<summary>Evaluates one line of compiled script.</summary>
               
<remarks>Evaluates one line of compiled script and returns the number of next line to be evaluated.
               If the value returned contains an index greater or equal to the line count, then the end of the script has been reached.
</remarks>
*)
    function EvaluateStep(aLine: integer): integer;
    (*<summary>Evaluates all lines of compiled script from specified Line forward.</summary>
               
<remarks>The function returns the result of the last line.
</remarks>
*)
    function EvaluateRun(aLine: integer): TValueRec;
    (*<summary>Specifies an expression, which must fail, underwise an exception is raised.</summary>*)
    procedure NegativeTest(const anExpr: string);

    (*<summary>Specifies a single expression, which must match ResultValue, underwise an exception is raised.</summary>

      <example>Test "+" operator

      <code>
      procedure TestAddOperator(aParser: TMtxExpression);
      begin
          aParser.Test('1+2',3); //no exception will be raised, if result is a match
      end;
      </code>
      </example>*)
    procedure Test(const anExpr: string; ResultValue: double; AllowNanInf: boolean = false); overload;
    (*<summary>Specifies a single expression, which must match ResultValue, underwise an exception is raised.</summary>*)
    procedure Test(const anExpr: string; ResultValue: TCplx; AllowNanInf: boolean = false); overload;
    procedure Test(const anExpr: string; ResultValue: boolean); overload;
    (*<summary>Specifies a single expression, which must match ResultValue, underwise an exception is raised.</summary>*)
    procedure TestVec(const anExpr: string; const ResultValue: array of double; AllowNanInf: boolean); overload;
    procedure TestVec(const anExpr: string; const ResultValue: array of double); overload;
    procedure TestVec(const anExpr: string; const ResultValue: TVec; AllowNanInf: boolean = false); overload;
    (*<summary>Specifies a single expression, which must match ResultValue, underwise an exception is raised.</summary>*)
    procedure TestMtx(const anExpr: string; const ResultValue: array of double; AllowNanInf: boolean); overload;
    procedure TestMtx(const anExpr: string; const ResultValue: array of double); overload;
    procedure TestMtx(const anExpr: string; const ResultValue: TMtx; AllowNanInf: boolean = false); overload;
    procedure TestMtxInt(const anExpr: string; const ResultValue: array of integer); overload;
    procedure TestVecInt(const anExpr: string; const ResultValue: array of integer); overload;

    (*<summary>Runs all registered tests and raises an exception, if one fails.</summary>*)
    procedure SelfTest;
    (*<summary>Returns the name of the result variable for expression at Index.</summary>
     
<remarks>Use this method to retreive the name of result variable of the expression
     stored at position Index. In case of a = b + c, the name of the result variable
     is "a". In case of b + c, the result variable has no name. The object stores
     a list of expressions and therefore requires an index to specify the name of the result
     of which expression is desired.
</remarks>


     <seealso cref="ClearAll"/>
     <seealso cref="ClearExpressions"/>*)

    function EvaluatedVarName(Index: integer): string;
    constructor Create;
    destructor Destroy; override;

    (*<summary>Clears all expressions and all defined variables.</summary>

      

      <example>
        Clear everything.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression myParser)
        {
          // 1. Example
          myParser.DefineDouble("omega");
          myParser.DefineDoubleConstant("hbar");
          myParser.AddExpr("hbar*omega");

          // 2. Clear everything
          myParser.ClearAll();

          // 3. What happened ?
          int eCount = myParser.ExprCount() // returns 0
          // Also, all defined the variable, constant varying function, operator,
          // etc.. lists are cleared.
        }
      }
      </code></example>

     <seealso cref="ClearExpressions"/>
     <seealso cref="ClearVariables"/>*)
    procedure ClearAll;

    (*<summary>Clears all defined variables only.</summary>
     
<remarks>Use this method if you do not want to delete parser expressions
     and only want to delete defined variables.
</remarks>


     <seealso cref="ClearAll"/>
     <seealso cref="ClearExpressions"/>*)
    procedure ClearVariables;

    (*<summary>Clears all expressions only.</summary>
     
<remarks>Use this method if you do not want to delete parser defined variables
     and only want to delete parser expressions.
     Manipulation of vars of compiled expressions can lead to memory leaks.
     Call ClearExpressions first, if the variables are to be kept and/or added, but the expressions will change.
</remarks>


     <seealso cref="ClearAll"/>
     <seealso cref="ClearVariables"/>*)
    procedure ClearExpressions;

    (*<summary>Removes symbol from defined symbols.</summary>
      <param name="AName">Defines the name of removed symbol.</param>
      <returns>True, if removing symbol AName was successful.</returns>

      
<remarks>Note:
        Any variables, constanst, functions and operators may be undefined. This is different from the "undefine" function called from
        the script, which will simply undefine the type of the variable thus allowing it to have its type defined again.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression myParser)
        {
          bool success = myParser.Undefine("x");
        }
      }
      </code></example>

     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function Undefine (const AName: string): boolean;

    (*<summary>Defines new variable.</summary>
      <returns></returns>
      <param name="AVarName">Defines variable name. Type of variable
        (double/complex/vector/matrix) must be defined <b>before</b> any
        Evaluate method will be called. If variable with name AVarName
        was already defined, it will be redefined and all expressions
        which contain the symbol AVarName will be recompiled before
        the next evaluation.
      </param>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineVariable (const AVarName: string): TValueRec; overload;

    (*<summary>Defines new double variable.</summary>
      
<remarks>If variable with name AVarName already was definded, it will be redefined
      and all expressions which contain the symbol AVarName will be recompiled
      before the next evaluation.
</remarks>


      

      <example>
        Use parser to evaluate the "Sqrt(X*X + Y*Y)" formula where x=3 and y=4.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TDoubleValue x = MyParser.DefineDouble("x");
          TDoubleValue y = MyParser.DefineDouble("y");

          // 2. Set values
          x.DoubleValue = 3.0;
          y.DoubleValue = 4.0;

          // 3. Add formula and evaluate,
          MyParser.AddExpr("Sqrt(x*x+y*y)");
          double res = MyParser.EvaluateDouble();
          // res = 5
        }
      }
      </code></example>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineDouble (const AVarName: string): TDoubleValue;

    (*<summary>Defines new boolean variable.</summary>*)
    function DefineBool (const AVarName: string): TBoolValue;
    (*<summary>Defines new integer variable.</summary>*)
    function DefineInteger (const AVarName: string): TIntegerValue;
    (*<summary>Defines new string variable.</summary>*)
    function DefineString(const AVarName: string): TStringValue;
    (*<summary>Defines new range variable.</summary>*)
    function DefineRange(const AVarName: string): TRangeValue;

    (*<summary>Defines new variable of arbitrary type.</summary>
     
<remarks>Use this method to define Global variables, which are shared between multiple
     expression objects. aVar can be constructed externally and then passed to multiple
     TMtxExpression objects. The name of the variable can be equal or different between
     TMtxExpression objects, but the value will be modified concurrently and be the same in all.
     This value is stored in the aVar object. The type does not need to be defined upfront, but once
     defined an attempt for the script to change the type will be blocked. The type can be reset to undefined
     with a call to aVar.Undefine.
</remarks>
*)
    function DefineVariable(const AVarName: string; const aVar: TValueRec): TValueRec; overload;

    (*<summary>Defines new complex variable.</summary>
      
<remarks>If variable with name AVarName already was definded, it will
      be redefined and all expressions which contain the symbol AVarName will be recompiled before the next evaluation.
</remarks>


      

      <example>
        Use parser to evaluate the "(2*i*z)" formula where <c>z=-2+3*i</c>.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TComplexValue z = MyParser.DefineComplex("z");

          // 2. Set values
          z.ComplexValue = Math387.Cplx(-2,3);

          // 3. Add formula and evaluate,
          MyParser.AddExpr("2*i*z");
          TCplx res = MyParser.EvaluateComplex();
          // res = -6-4*i
        }
      }
      </code></example>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineComplex (const AVarName: string): TComplexValue;

    (*<summary>Defines new vector variable.</summary>
      
<remarks>If variable with name AVarName already was definded, it is redefined and all expressions
      which contain the symbol AVarName will be recompiled before the next evaluation.
      External vector may be assigned as values holder. If external vector is not assigned,
      internal vector will be used.
</remarks>


      

      <example>
        Use parser to evaluate the "-s*x" formula where
        x(1,-1,1) is vector and s=0.5 scalar. To evaluate vector, use
        automatically generated vector.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TVectorValue = MyParser.DefineVector("x");
          TDoubleValue s = MyParser.DefineDouble('s');

          // 2. Set values
          x.Value.SetIt(false,new double[] {1,-1,1});
          s.DoubleValue = 0.5;

          // 3. Add formula and evaluate,
          MyParser.AddExpr("-s*x");
          Vector res = new Vector(0);

          res = MyParser.EvaluateVector();
          // res = (-0.5,0.5, -0.5)
        }
      }
      </code></example>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineVector (const AVarName: string; const Vec: TVec): TVectorValue; overload;
    (*<summary>Use internal vector.</summary>*)
    function DefineVector (const AVarName: string): TVectorValue; overload;

    (*<summary>Define new custom object variable.</summary>
      
<remarks>The function receiving this variable needs to check for correct type of object and report an error if not as expected.
</remarks>
*)
    function DefineCustomValue(const AVarName: string; const obj: TObject): TCustomValue; overload;
    (*<summary>Define type and initialize to nil.</summary>*)
    function DefineCustomValue(const AVarName: string): TCustomValue; overload;

    function DefineIntVector (const AVarName: string; const Vec: TVecInt): TIntVectorValue; overload;
    (*<summary>Use internal vector.</summary>*)
    function DefineIntVector (const AVarName: string): TIntVectorValue; overload;

    function DefineBoolVector (const AVarName: string; const Vec: TVecInt): TBoolVectorValue; overload;
    (*<summary>Use internal vector.</summary>*)
    function DefineBoolVector (const AVarName: string): TBoolVectorValue; overload;

    (*<summary>DefineMatrix defines new matrix variable.</summary>
      
<remarks>If variable with name AVarName already was definded, it will be redefined and
      all expressions which contain the symbol AVarName will be recompiled before the
      next evaluation. External matrix may be assigned as values holder. If external matrix is not assigned,
      internal matrix will be used.
</remarks>


      

      <example>
        Use parser to evaluate the "A+2*B" formula where
        A=(2,1; 3, 0)  B=(1,0; 1,1) are 2x2 matrices.

      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression MyParser)
        {
          // 1. Define variables
          TMatrixValue A = MyParser.DefineMatrix("A");
          TMatrixValue B = MyParser.DefineDouble("B");

          // 2. Set values
          A.Value.SetIt(2,2, false,new double[] {2,1,3,0});
          B.Value.SetIt(2,2, false,new double[] {1,0,1,1});

          // 3. Add formula and evaluate,
          MyParser.AddExpr("A+2*B");
          Matrix res = new Matrix(false,0,0)
          res = MyParser.EvaluateMatrix;
          // res = (4,1; 5,2)
        }
      }
      </code></example>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineInteger"/>
     <seealso cref="DefineBool"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineIntVector"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineMatrix (const AVarName: string; const mtx: TMtx): TMatrixValue; overload;
    (*<summary>Use internal matrix.</summary>*)
    function DefineMatrix (const AVarName: string): TMatrixValue; overload;

    function DefineIntMatrix (const AVarName: string; const mtx: TMtxInt): TIntMatrixValue; overload;
    (*<summary>Use internal matrix.</summary>*)
    function DefineIntMatrix (const AVarName: string): TIntMatrixValue; overload;

    function DefineBoolMatrix (const AVarName: string; const mtx: TMtxInt): TBoolMatrixValue; overload;
    (*<summary>Use internal matrix.</summary>*)
    function DefineBoolMatrix (const AVarName: string): TBoolMatrixValue; overload;

    (*<summary>Defines new function.</summary>
      <param name="AFunctName">Defines function name. If a function with the same name
        was already definded, it is redefined and all expressions which contain the symbol AFuncName
        will be recompiled before the next evaluation.</param>
      <param name="AFuncAddress"></param>
      <param name="NArguments">Defines number of arguments for function.</param>
      <param name="NResults">Defines number of arguments, which are treated as results. NResults &lt;= NArguments</param>
      <param name="Help">Function help text.</param>

      

      <example>
        Add factorial function to the function list.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          void _Factorial_D (TExprRec Param)
          {
              Param.Res.DefineDouble;
              Param.Res.DoubleValue = Fact(System.Math.Round(Param.Args[0].DoubleValue));
          }

          void Example(TMtxExpression MyParser);
          {
              myParser.DefineFunction("fact_double", _Factorial_D, 1, "fact_double(x): factorial function");
          }
        }
      }
      </code></example>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineFunction(const AFunctName: string; AFuncAddress: TExprFunc; NArguments, NResults: Integer; const Help: string): integer; overload;
    function DefineFunction(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; const Help: string): integer; overload;
    function DefineFunction(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer): integer; overload;

    function DefineFunction(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj): integer; overload;
    function DefineFunction(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj; const Help: string): integer; overload;
    function DefineFunction(const AFunctName: string; NArguments, NResults: Integer; const AFuncAddress: TExprFuncObj; const Help: string): integer; overload;

    (*<summary>Defines new varying function.</summary>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    function DefineVaryingFunction(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer; const Help: string): integer; overload;
    function DefineVaryingFunction(const AFunctName: string; AFuncAddress: TExprFunc; NArguments: Integer): integer; overload;
    function DefineVaryingFunction(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj; const Help: string): integer; overload;
    function DefineVaryingFunction(const AFunctName: string; NArguments: Integer; const AFuncAddress: TExprFuncObj): integer; overload;

    (*<summary>Defines new operator.</summary>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineComplexConstant"/>*)
    procedure DefineOperator(const AOperatorSign: string; AFuncAddress: TExprFunc; NArguments: Integer; Precedence: Integer; const Help: string); overload;
    procedure DefineOperator(const AOperatorSign: string; AFuncAddress: TExprFunc; NArguments: Integer; Precedence: Integer); overload;

    (*<summary>Defines new double constant.</summary>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineVector"/>
     <seealso cref="DefineComplexConstant"/>*)
    procedure DefineDoubleConstant (const AConstName: string; Value: double; const Help: string); overload;
    procedure DefineDoubleConstant (const AConstName: string; Value: double); overload;

    (*<summary>Defines new boolean constant.</summary>*)
    procedure DefineBoolConstant (const AConstName: string; Value: boolean; const Help: string); overload;
    procedure DefineBoolConstant (const AConstName: string; Value: boolean); overload;

    (*<summary>Defines new complex constant.</summary>

     <seealso cref="Undefine"/>
     <seealso cref="VarByName"/>
     <seealso cref="GetVarList"/>
     <seealso cref="DefineVariable"/>
     <seealso cref="DefineDouble"/>
     <seealso cref="DefineComplex"/>
     <seealso cref="DefineMatrix"/>
     <seealso cref="DefineFunction"/>
     <seealso cref="DefineVaryingFunction"/>
     <seealso cref="DefineOperator"/>
     <seealso cref="DefineDoubleConstant"/>
     <seealso cref="DefineVector"/>*)
    procedure DefineComplexConstant (const AConstName: string; const Value: TCplx; const Help: string); overload;
    procedure DefineComplexConstant (const AConstName: string; const Value: TCplx); overload;

    (*<summary>Retrieves list of functions.</summary>

     <seealso cref="GetOperList"/>
     <seealso cref="GetConstList"/>
     <seealso cref="GetVarList"/>*)
    procedure GetFuncList (const names, help: TStrings; arg_list: boolean);

    (*<summary>Retrieves list of operators.</summary>

     <seealso cref="GetFuncList"/>
     <seealso cref="GetConstList"/>
     <seealso cref="GetVarList"/>*)
    procedure GetOperList (const names, help: TStrings);

    (*<summary>Retrieves list of constants.</summary>

     <seealso cref="GetFuncList"/>
     <seealso cref="GetOperList"/>
     <seealso cref="GetVarList"/>*)
    procedure GetConstList (const names, help: TStrings);

    (*<summary>Retrieves list of variables.</summary>
               
<remarks>The TValueRec values, holding the type of the variable etc.. , are stored in names.Objects[i] after the function returns.
</remarks>


     <seealso cref="GetFuncList"/>
     <seealso cref="GetOperList"/>
     <seealso cref="GetConstList"/>
     <seealso cref="VarByName"/>*)
    procedure GetVarList (const names: TStrings);

    (*<summary>Returns variable by name or nil, if variable does not exist.</summary>

      

      <example>
        Use parser to evaluate the "(2*i*z)" formula where <c>z=-2+3*i</c>.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Add formula and evaluate,
          MyParser.AddExpr("2*i*z");

          // 2. Get defined variable
          z = MyParser.VarByName["z"];

          // 3. Specify the type
          z.DefineComplex();

          // 4. Set values
          z.ComplexValue = Math387.Cplx(-2,3);

          // 5. Evaluate
          TCplx res = MyParser.EvaluateComplex();
          // res = -6-4*i
        }
      }
      </code></example>

     <seealso cref="GetVarList"/>*)
    property VarByName: TVarByNameIndexer read fVarByNameIndexer;


    (*<summary>Gets/sets several expressions separated by semicolon at once.</summary>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression myParser)
        {
          myParser.Expressions = "x+y;sqrt(x);x/y";
          myParsers.ExprCount(); // 3
        }
      }
      </code></example>

     <seealso cref="ExprCount"/>*)
    property Expressions: string read GetExpressions write SetExpressions;

    (*<summary>Number of expressions.</summary>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression myParser)
        {
          myParser.Expressions = "x+y;sqrt(x);x/y";
          myParsers.ExprCount(); // 3
        }
      }
      </code></example>

     <seealso cref="Expressions"/>*)
    property ExprCount: integer read GetExprCount;

    (*<summary>If True, all constants will be double type even if number is integer.</summary>
               
<remarks>This setting is usefull, when the parser is to be used for a calculator style application.
               When used for Scripting type application, it makes more sense to have this setting set to false.
               Default value is false.
</remarks>
*)
    property ConstantsAlwaysReal: boolean read FConstantsAlwaysReal write SetConstantsAlwaysReal;

    (*<summary>If True, all [ ..] type of expression will be double type even, if all items are integer.</summary>
               
<remarks>Default value is true.
</remarks>
*)
    property ConcatsAlwaysReal: boolean read FConcatsAlwaysReal write SetConcatsAlwaysReal default True;

    (*<summary>Returns expression by index.</summary>

     <seealso cref="ExprCount"/>
     <seealso cref="Expressions"/>
     <seealso cref="ExprResult"/>*)
    property Expression[Index: integer]: string read GetExpression write SetExpression; default;

    (*<summary>Appends new expression at the end of list.</summary>
      <returns>index of added expression.</returns>

     <seealso cref="ExprCount"/>
     <seealso cref="InsertExpr"/>
     <seealso cref="DeleteExpr"/>*)
    function AddExpr (const expr: string): integer; overload;

    (*<summary>Appends new expressions at the end of list.</summary>*)
    function AddExpr(const exprList: TStrings): integer; overload;

    (*<summary>Inserts new expression at specified index.</summary>

     <seealso cref="ExprCount"/>
     <seealso cref="AddExpr"/>
     <seealso cref="DeleteExpr"/>*)
    procedure InsertExpr (Index: integer; const expr: string);

    (*<summary>Deletes expression at specified index.</summary>

     <seealso cref="ExprCount"/>
     <seealso cref="AddExpr"/>
     <seealso cref="InsertExpr"/>*)
    procedure DeleteExpr (Index: integer);

    (*<summary>Returns expression result at specified index.</summary>

     <seealso cref="ExprCount"/>
     <seealso cref="LastResult"/>*)
    property ExprResult: TExprResultIndexer read fExprResultIndexer;


    (*<summary>Returns result of the latest expression from the list.</summary>

     <seealso cref="ExprCount"/>
     <seealso cref="ExprResult"/>*)
    property LastResult: TValueRec read GetLastResult;

    (*<summary>Sets new context (expressions and variable values).</summary>*)
    procedure LoadContext (const contextSrc: TExprContext);

    (*<summary>Retrives current context (expressions and variable values).</summary>

     <seealso cref="LoadContext"/>*)
    procedure SaveContext (const contextDst: TExprContext);

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>result of the latest expression.</returns>

      <seealso cref="EvaluateDouble"/>
      <seealso cref="EvaluateComplex"/>
      <seealso cref="EvaluateVector"/>
      <seealso cref="EvaluateMatrix"/>*)
    function Evaluate: TValueRec; overload;

    (*<summary>Evaluates expression at the specified index.</summary>
      <returns>result of the expression, evaluated at specific index.</returns>

      
<remarks>If the Index parameter is specified to be -1, the method will evaluate
      all expressions in the list starting with the first and return the result
      of the last expression in the list.

      The function returns TValueRec. This object contains information about
      the type of the result and the result itself. To determine the
      type read the TValueRec.ValueType property.

      The types in the expression are resolved at compile time and may
      not change after the first call to the Evalute function. The result
      of a given expression will therefore always have the same type.

      This allows for omission of certain checks and further increase
      of code speed. Instead of calling EvaluateDouble method, it would
      be faster to call just:

      <c>
      myDouble := myParser.Evaluate.DoubleValue;
      </c>

      ,if you know that the result of the expression is a variable of
      type double.
</remarks>
 

      <example>
      <code>

      uses MtxParseExpr, MtxParseClass;

      function TMtxExpression.EvaluateDouble(Index :integer): Double;
      var vr :TValueRec;
      begin
        vr:= Evaluate(Index);
        case vr.ValueType of
          vtDoubleValue:
            begin
              result:= vr.DoubleValue;
              Exit;
            end;
          vtComplexValue:
            if vr.ComplexValue.Im = 0 then begin
              result:= vr.ComplexValue.Re;
              Exit;
            end;
        end;
        raise EMtxParseError.Create ('TMtxExpression.EvaluateDouble: double value expected');
      end;

      </code>
      </example>

     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function Evaluate(Index: integer): TValueRec; overload;

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>the result of the latest expression as double value.</returns>

      

      <example>
        Use parser to evaluate the "Sqrt(X*X + Y*Y)" formula where x=3 and y=4.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TDoubleValue x = MyParser.DefineDouble("x");
          TDoubleValue y = MyParser.DefineDouble("y");

          // 2. Set values
          x.DoubleValue = 3.0;
          y.DoubleValue = 4.0;

          // 3. Add formula and evaluate,
          MyParser.AddExpr("Sqrt(x*x+y*y)");
          double res = MyParser.EvaluateDouble();
          // res = 5
        }
      }
      </code></example>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateDouble: Double; overload;

    (*<summary>Evaluates expression at the specified index.</summary>
      <returns>the result as double value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateDouble(Index: integer): Double; overload;

    (*<summary>Evaluates expression at the specified index.</summary>
      <returns>the result as a boolean value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateBool(Index: integer): Boolean; overload;

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>the result of the last expression as a single boolean value.</returns>*)
    function EvaluateBool: boolean; overload;

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>the result of the latest expression as complex value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateComplex: TCplx; overload;

    (*<summary>Evaluates expression at the specified index.</summary>
      <returns>the result as complex value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateVector"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateComplex(Index: integer): TCplx; overload;

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>result of the latest expression as vector value.</returns>

      

      <example>
        Use parser to evaluate the "-s*x" formula where
        x(1,-1,1) is vector and s=0.5 scalar. To evaluate vector, use
        automatically generated vector.
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          // 1. Define variables
          TVectorValue = MyParser.DefineVector("x");
          TDoubleValue s = MyParser.DefineDouble('s');

          // 2. Set values
          x.Value.SetIt(false,new double[] {1,-1,1});
          s.DoubleValue = 0.5;

          // 3. Add formula and evaluate,
          MyParser.AddExpr("-s*x");
          Vector res = new Vector(0);

          res = MyParser.EvaluateVector();
          // res = (-0.5,0.5, -0.5)
        }
      }
      </code></example>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateVector: TVec; overload;

    (*<summary>Evaluates vector expression at the specified index.</summary>
      <returns>result as vector value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateMatrix"/>*)
    function EvaluateVector(Index: integer): TVec; overload;

    function EvaluateVectorInt: TVecInt; overload;
    function EvaluateVectorInt(Index: integer): TVecInt; overload;

    function EvaluateMatrixInt: TMtxInt; overload;
    function EvaluateMatrixInt(Index: integer): TMtxInt; overload;

    (*<summary>Evaluates all expressions from the list.</summary>
      <returns>result of the latest expression as matrix value.</returns>

      

      <example>
        Use parser to evaluate the "A+2*B" formula where
        A=(2,1; 3, 0)  B=(1,0; 1,1) are 2x2 matrices.

      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example(TMtxExpression MyParser)
        {
          // 1. Define variables
          TMatrixValue A = MyParser.DefineMatrix("A");
          TMatrixValue B = MyParser.DefineDouble("B");

          // 2. Set values
          A.Value.SetIt(2,2, false,new double[] {2,1,3,0});
          B.Value.SetIt(2,2, false,new double[] {1,0,1,1});

          // 3. Add formula and evaluate,
          MyParser.AddExpr("A+2*B");
          Matrix res = new Matrix(false,0,0)
          res = MyParser.EvaluateMatrix;
          // res = (4,1; 5,2)
        }
      }
      </code></example>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>*)
    function EvaluateMatrix: TMtx; overload;

    (*<summary>Evalutes expression at the specified index.</summary>
      <returns>result as matrix value.</returns>

     <seealso cref="Evaluate"/>
     <seealso cref="EvaluateDouble"/>
     <seealso cref="EvaluateComplex"/>
     <seealso cref="EvaluateVector"/>*)
    function EvaluateMatrix(Index: integer): TMtx; overload;
    (*<summary>The preprocessor event is called after the preprocessor has already applied
      any substitions and patched the parsed expression.</summary>
       
<remarks>All modifications must be made to the Expr list parameter. This
       is the list of lexems forming the expression. For example abc(2 + 3)
       is formed with <c>'abc', '(',  '2', '+', '3', ')'</c>. The parser already assigns
       the type of lexeme to each entry. May that be a variable, a constant or
       a function.

       This preprocessor allows you to easily parse individual expression
       elements further in to form understandable to the compiler. An example
       of the preprocessor purpose is the ability to pass spreadsheet cells or ranges
       to the expression parser. This expressions could be of type: <c>A2:B3, A3,
       A$2:$B3</c> and so on. You can replace these expression with custom
       function specifications like:

       <c>spreadsheet('A',2,'B',3);</c>

       which could be hard for the user to write. The "spradsheet" function itself
       however still has to be declared and implemented.
</remarks>
*)
    property OnPreprocessor: TOnPreprocessor read FOnPreprocessor write SetOnPreprocessor;
    (*<summary>Returns the line (expression number) of the last evaluation, where a runtime error occured. </summary>
               
<remarks>The property will return -1, if evaluation completed without errors.
</remarks>
*)
    property EvaluatedLine: integer read FEvaluatedLine write SetEvaluatedLine;
  end;

  

  
  
  (*<summary>Function evaluator.</summary>
    
<remarks>The component offers a simple interface to evaluate expressions.

    The expressions are assigned to the <see cref="Context"/> property and are compiled, prepared for
    execution and stored in the <see cref="Expressions"/> property. The expression parser
    can handle real and complex numbers, logical variables, variable assignments and more. The
    expression engine is fully customizable supporting user defined variables and functions.

    To evalute the expression(s) in the <see cref="Context"/> property again, set the <see cref="Context"/> property
    again or call the <see cref="Recalculate"/> method. In either case the expression will not be <b>parsed</b> again,
    just <b>evaluated</b> once more.
</remarks>
*)
  TMtxFunctionEvaluator = class(TMtxComponent)
  strict private
    fExpr: TMtxExpression;
    fContext: TExprContext;
    function GetResultAsString: string;
    procedure SetContext(const Value: TExprContext);
  public
   

    constructor Create(aOwner: TComponent); overload; override;
    destructor Destroy; overload; override;

    (*<summary>Re-evaluates expression in Context.</summary>
      <returns>the result of expression stored in <see cref="Context"/>
       as <see cref="TValueRec"/>.</returns>

       
<remarks>The procedure loads context from <see cref="Context"/> property, uses internal
       parser to evaluate expressions (now stored in <see cref="Expressions"/> property
       and returns result of expression evaluation.
</remarks>
*)
    function Recalculate: TValueRec;
    (*<summary>Returns the compiled expressions from the <see cref="Context"/> class.</summary>*)
    property Expressions: TMtxExpression read fExpr;
    (*<summary>Returns the result of evaluation as string.</summary>*)
    property ResultAsString: string read GetResultAsString;
  published
   (*<summary>Expression context.</summary>

    <seealso cref="TExprContext"/>*)
   
    property Context: TExprContext read fContext write SetContext;
  end;

  function TerminatedWithSemicolon(const aStr: string): boolean; overload;

  var AssignOperator: string = '=';
      EqualOperator: string = '==';

