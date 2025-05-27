










(*<summary>Interfaces several MtxVec routines and intruduces new components.</summary>*)

unit MtxVecTools;


interface

{$I BdsppDefs.inc}

Uses MtxVec, Optimization, Math387, MtxBaseComp, MtxIntDiff, MtxVecInt

     

     

     
     ,Classes
     
     ;

Type

  
  TMtxOptimization = class;
  

  



  (*<summary>Interfaces the optimization routines.</summary>
    
<remarks>The component can be used to find the minimum of function of several variables.<para/>

    <b>How to use TMtxOptimization component?</b>
    * Drop a TMtxOptimization component on the form.
    * Define the number of variables and their initial values by accessing the <see cref="VariableParameters"/> vector.
    * Define any additional constant parameters by accessing <see cref="ConstantParameters"/> vector.
    * Define any additional constant pointer parameter by using the <see cref="SetObjects"/> method.
    * Define real function (must be of <see cref="TRealFunction"/> type).
    * Define optimization method by accessing the <see cref="OptimizationMethod"/> property.
    * Depending on optimization method you'll have to (optionally) define the gradient calculation
    procedure (<see cref="GradProcedure"/> method) or gradient/Hessian matrix calculation procedure (<see cref="GradHessProcedure"/>
    method). If you don't specify the GradProcedure or GradHessProcedure then the numeric approximation will be used
    to calculate the gradient vector and Hessian matrix. In this case you must also specify which gradient
    aproximation method you will use - access the <see cref="NumericGradMethod"/> property.
    * Call the <see cref="Recalculate"/> method to find the minimum of function of several variables.

    <u>Results:</u>
    * <see cref="MinValue"/> : RealFunction, evaluated at minimum.
    * <see cref="VariableParameters"/> : minimum position.
    * <see cref="Iterations"/>:  Number of iterations needed to reach specified (<see cref="Tolerance"/> property) minimum precision.
    * <see cref="InverseHess"/> : Inverse Hessian matrix (returned only by BFGS, ConjGrad and Marquardt methods)
    * <see cref="StopReason"/>: Why did the optimization algorithm stopped ?
</remarks>


    

    <example>
      How to solve the optimization problem using TMtxOptimization component?
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      // define the real function to be minimized
	  private double Banana(TVec pars, TVec consts, params object[] objConsts)
	  {
		  return 100*Math.Pow(pars[1]-Math.Pow(pars[0],2),2)+Math.Pow(1-pars[0],2);
	  }

    private void Example(TMtxOptimization MtxOptim);
    {
      if (MtxOptim != null)
      {
        // define two variables and their initial values
        MtxOptim.VariableParameters.SetIt(false, new double[] {2,-1});
        // use BFGS optimization method
        MtxOptim.OptimizationMethod = TOptMethod.optBFGS;
        // tolerance for MinValue and gradient calculation
        // additional note : since we did not define the GradProc,
	      // the internal numerical gradient approximation will be used
        MtxOptim.Tolerance = 2.0e-6;
        MtxOptim.GradTolerance = 2.0e-6;
        // function to be minimized
        MtxOptim.RealFunction = Banana;
        // finally, calculate minimum
        MtxOptim.Recalculate();
      }
    }
  }
  </code></example>*)
  TMtxOptimization = class(TMtxComponent)
  strict private
    FOptimizationMethod: TOptMethod;
    FDirty: boolean;
    FMaxIterations: Integer;
    FVerbose: TStrings;
    IObjArray: TObjectArray;
    FConstantParameters: TVec;
    FVariableParameters: TVec;
    FTolerance: double;
    FLambda0: double;
    FGradTolerance: double;
    FGradProcedure: TGrad;
    FGradHessProcedure: TGradHess;
    FRealFunction: TRealFunction;
    FSoftSearch: boolean;
    FStopReason: TOptStopReason;
    FNumericGradMethod: TNumericGradMethod;
    FAutoUpdate: boolean;
    procedure SetOptimizationMethod(const Value: TOptMethod);
    procedure SetDirty(const Value: boolean);
    procedure SetMaxIterations(const Value: Integer);
    procedure SetVerbose(const Value: TStrings);
    procedure SetConstantParameters(const Value: TVec);
    procedure SetVariableParameters(const Value: TVec);
    procedure SetTolerance(const Value: double);
    procedure SetLambda0(const Value: double);
    procedure SetGradTolerance(const Value: double);
    procedure SetGradHessProcedure(const Value: TGradHess);
    procedure SetGradProcedure(const Value: TGrad);
    procedure SetRealFunction(const Value: TRealFunction);
    procedure SetSoftSearch(const Value: boolean);
    procedure SetNumericGradMethod(const Value: TNumericGradMethod);
    procedure SetAutoUpdate(const Value: boolean);
  public
    (*<summary>Returns the inverse of Hessian matrix.</summary>*)
    InverseHess: TMtx;
    (*<summary>Returns number of optimization algorithm iterations.</summary>*)
    Iterations : Integer;
    (*<summary>Stores value of the objective function, evaluated at minimum.</summary>*)
    MinValue: double;

    (*<summary>Becomes true after any of the properties have changed.</summary>      
	  
<remarks>When any of the TMtxOptimization properties changes, this property is automatically
      set to true. If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalculate"/>
      method will be called automatically.
</remarks>


      <seealso cref="AutoUpdate"/>
      <seealso cref="Recalculate"/>*)
    property Dirty: boolean read FDirty write SetDirty;
    (*<summary>Stop reason for optimization algorithm.</summary>

      
<remarks>Returns the stop reason for optimization algorithm. See <see cref="TOptStopReason"/>
      type to learn more about different stop reasons.
</remarks>


      <seealso cref="TOptStopReason"/>*)
    property StopReason: TOptStopReason read FStopReason;
    (*<summary>Define any additional Object constant parameters in the RealFunction.</summary>

      
<remarks>Use SetObjects method to define any additional Object constant parameters in the RealFunction.
      By default, the PConsts has 0 elements.
</remarks>


      

    <example>
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      // define the real function to be minimized
	  private double vecfun(TVec pars, TVec consts, params object[] objConsts)
	  {
	      TVec tmpVec = object0] as TVec;
	      return pars[0]*tmpVec.Sum() -tmpVec.SumOfSquares()*pars[1];
	  }

    private void Example(TMtxOptimization opt1);
    {
        // ...
        opt1.RealFunction = VecFun;
	      opt1.VariableParameters.SetIt(false,new double[] {-1,0)});
        Vector xVec = new Vector(0);
        opt1.SetObjects(new object[] {xVec});
    }
    }
    </code></example>

    <seealso cref="RealFunction"/>
    <seealso cref="VariableParameters"/>
    <seealso cref="ConstantParameters"/>*)
    procedure SetObjects(Const ObjConst: Array of TObject);
    (*<summary>Triggers TMtxOptimization recalculation.</summary>
      
<remarks>Triggers TMtxOptimization recalculation. After the Recalculate call
      the <see cref="Dirty"/> property is set to false.

      The results are stored to:
      * <see cref="MinValue"/> : RealFunction, evaluated at minimum.
      * <see cref="VariableParameters"/> : minimum position.
      * <see cref="Iterations"/>:  Number of iterations needed to reach specified (<see cref="Tolerance"/> property) minimum precision.
      * <see cref="InverseHess"/> : Inverse Hessian matrix (returned only by BFGS, ConjGrad and Marquardt methods).
      * <see cref="StopReason"/>: Why did the optimization algorithm stop?
</remarks>
*)
    procedure Recalculate;
    (*<summary>If not nil then the optimization method uses it for logging each optimization step.</summary>
      
<remarks>If not nil then the optimization method uses it for logging each optimization step.
      By default the Verbose property is nil meaning no logging is done.
</remarks>


      

      <example>
        Log directly to TStringList:
      <code>
        TStringList log = new TStringList();
        MtxOptimizaton.Verbose = log;
        MtxOptimization.Recalculate();
      </code></example>*)
    property Verbose: TStrings read FVerbose write SetVerbose;

    (*<summary>Defines the gradient vector calculation routine.</summary>
     
<remarks>Defines the gradient vector calculation routine. Set <see cref="GradProcedure"/>
     to nil if you want to use internal numeric gradient vector calculation.
</remarks>


     

    <example>
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      // Objective function
      double Banana(TVec pars, TVec consts, params object[] obj)
      {
          return 100.0*Math387.IntPower(pars[1]- Math387.IntPower(pars[0],2),2)
            + Math387.IntPower(1.0-pars[0],2);
      }

      // Analytical gradient of the objective function
      void GradBanana(TRealFunction Fun, TVec pars, TVec consts, object[] obj, TVec grad)
      {
        grad.Values[0] = -400*(pars[1]-Math387.IntPower(pars[0],2))*pars[0] - 2*(1-pars[0]);
        grad.Values[1] = 200*(pars[1]-Math387.IntPower(pars[0],2));
      }

      private void Example(TMtxOptimization opt)
      {
        opt.VariableParameters.SetIt(new double[] {2,-1});
        opt.RealFunction = Banana;
        opt.OptimizationMethod = TOptimizationMethod.optBFGS;
        // use exact gradient vector calculation
        // NOTE : set opt.GradProcedure to null if you
	      //        want to use internal numeric gradient calculation
        opt.GradProcedure = GradBanana;
        opt.Recalculate();
      }
    }
    </code></example>

    <seealso cref="GradHessProcedure"/>
    <seealso cref="GradTolerance"/>*)
    property GradProcedure: TGrad read FGradProcedure write SetGradProcedure;
    (*<summary>Defines the gradient vector and Hessian matrix calculation routine.</summary>
     
<remarks>Defines the gradient vector and Hessian matrix calculation routine. Set <see cref="GradHessProcedure"/> to
     nil if you want to use internal numeric gradient and Hessian matrix calculation.
</remarks>


     

    <example>

    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      // Objective function
      double Banana(TVec pars, TVec consts, params object[] obj)
      {
          return 100.0*Math387.IntPower(pars[1]- Math387.IntPower(pars[0],2),2)
            + Math387.IntPower(1.0-pars[0],2);
      }

      // Analytical gradient of the objective function
      void GradHessBanana(TRealFunction Fun, TVec pars, TVec consts, object[] obj, TVec grad, TMtx Hess)
      {
        grad.Values[0] = -400*(pars[1]-Math387.IntPower(pars[0],2))*pars[0] - 2*(1-pars[0]);
        grad.Values[1] = 200*(pars[1]-Math387.IntPower(pars[0],2));
        Hess.Values1D[0] = -400*Pars[1]+1200*Math387.IntPower(Pars[0],2)+2;
        Hess.Values1D[1] = -400*Pars[0];
        Hess.Values1D[2] = -400*Pars[0];
        Hess.Values1D[3] = 200;
      }

      private void Example(TMtxOptimization opt)
      {
        opt.VariableParameters.SetIt(new double[] {2,-1});
        opt.RealFunction = Banana;
        opt.OptimizationMethod = TOptimizationMethod.optBFGS;
        // use exact gradient and Hessian calculation
        // NOTE : set opt.GradHessProcedure to null if you
	      //        want to use internal numeric gradient calculation
        opt.GradHessProcedure = GradHessBanana;
        opt.Recalculate();
      }
    }
    </code></example>

    <seealso cref="GradProcedure"/>
    <seealso cref="GradTolerance"/>*)
    property GradHessProcedure: TGradHess read FGradHessProcedure write SetGradHessProcedure;

    (*<summary>Defines the function to be minimized.</summary>
      
<remarks>Defines the function to be minimized. The <see cref="RealFunction"/> must be of
      the <see cref="TRealFunction"/> type. Before you write actual function,
      set the Parameters, Constants and PConstants array size by using the:
      <list>
      <item> <see cref="VariableParameters"/> vector Length, Size or SetIt method, </item>
      <item> <see cref="ConstantParameters"/> vector Length, Size or SetIt method, </item>
      <item> <see cref="SetObjects"/> method </item>
      </list>
</remarks>


      <Example>Define a function of two variables and one additional constant parameter.

      1) Set the VariableParameters, ConstantParameters size and (optionally) initial value:

      <code>
        MtxOptimization.VariableParameters.SetIt(false,[1,1]);
    	  MtxOptimization.ConstantParameters.SetIt(false,[2.5]);
      </code>
      <code lang="C++">
        MtxOptimization->VariableParameters->SetIt(false,OPENARRAY(double,(1,1)));
    	  MtxOptimization->ConstantParameters->SetIt(false,OPENARRAY(double,(2.5)));
      </code>

      Note that all values are real.

      2) Define the actual function:

      <code>
      function TestFunction(const Pars: TVec; const Consts: TVec;const ObjConsts: Array of TObject): double;
	    begin
        TestFunction := 2*Pars[0]+Sqrt(Pars[1]-Consts[0]);
	    end;
      </code>
      <code lang="C++">
      double __fastcall BTestFunction(TVec* const Parameters, TVec* const Constants, System::TObject* const * ObjConst, const int ObjConst_Size)
      {
        double* Pars = Parameters->PValues1D(0);

        return 2*Pars[0] + Sqrt(Pars[1]-Consts[0]);
      }
      </code>

      Here Pars = VariableParameters.Values and Consts = ConstantParameters.Values.

      3) "Connect" the function with TMtxOptimization component:

      <code>
      MtxOptimization.RealFunction := TestFunction;
      </code>
      <code lang="C++">
      MtxOptimization->RealFunction = TestFunction;
      </code>

      Please note that the internal algorithm does not check if Parameters, Constants, PConstants array size
      is the same as VariableParameters.Values, ConstantParameters.Values and SetPointers array size.
      </Example>

      <seealso cref="GradProcedure"/>
      <seealso cref="GradHessProcedure"/>
      <seealso cref="VariableParameters"/>
      <seealso cref="ConstantParameters"/>
      <seealso cref="SetObjects"/>*)
    property RealFunction: TRealFunction read FRealFunction write SetRealFunction;

    constructor Create(aOwner: TComponent); override;

    
    destructor Destroy; override;
    
    classfunction  EditorClass: string; override;
    
  published
    
    (*<summary>Automatic recalculation.</summary>
      
<remarks>If true then changing any of the TMtxOptimization properties will trigger the
      <see cref="Recalculate"/> method.
</remarks>


      <seealso cref="Dirty"/>*)
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate default false;
    
    (*<summary>Optimization algorithm used for minimum search.</summary>
      
<remarks>Defines which optimization method will be used to find the minimum of function of several variables.
</remarks>
*)
    property OptimizationMethod: TOptMethod read FOptimizationMethod write SetOptimizationMethod default optSimplex;
    
    (*<summary>maximum number of iterations allowed for minimum search.</summary>
      
<remarks>Defines the maximum number of iterations allowed for minimum search.
</remarks>


      <seealso cref="StopReason"/>*)
    property MaxIterations: Integer read FMaxIterations write SetMaxIterations default 500;
    
    (*<summary>Precision.</summary>
      
<remarks>Defines VariableParameters precision.
</remarks>


      <seealso cref="GradTolerance"/>*)
    property Tolerance: double read FTolerance write SetTolerance;
    (*<summary>Set/read the variables in minimized function.</summary>
      
<remarks>Access the VariableParameters vector to set/read the variables in minimized function. To define number of variables and
      their initial values, set the VariableParameters vector Length and Values properties. Read the VariableParameters Values
      property to get the function minimum position.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Editors;

      double Banana(TVec Pars, TVec Consts, object[] objs)
      {
        return 100*Math387.IntPower(Pars[1]-Math387.IntPower(Pars[0],2),2) + Math387.IntPower(1-Pars[0],1);
      }

      void Example(TMtxOptim opt1)
      {
        // function with two variables
        // initial values for variables => Pars[0] := 2, Pars[1] := -1;
        opt1.VariableParameters.SetIt(false,new double[] {2,-1});
        opt1.RealFunction = Banana;
        opt1.OptimizationMethod = optMarquardt;
        // numeric approximation
        opt1.GradProcedure = null;
        opt1.GradHessProcedure = null;
        opt1.Recalculate();
        // now VariableParameters holds minimum position
        MtxVecEdit.ViewValues(opt1.VariableParameters,"Minimum position");
      end;
      </code></example>

      <seealso cref="ConstantParameters"/>
      <seealso cref="SetObjects"/>
      <seealso cref="RealFunction"/>*)
    
    property VariableParameters: TVec read FVariableParameters write SetVariableParameters stored AllowStreaming;
    (*<summary>set/read the additional constants used in minimized function.</summary>
      
<remarks>Access the ConstantParameters vector to set/read the additional constants used in minimized function. By default
      the ConstantParameters vector has 0 elements.
</remarks>


      <seealso cref="VariableParameters"/>*)
    
    property ConstantParameters: TVec read FConstantParameters write SetConstantParameters stored AllowStreaming;
    (*<summary>Initial lambda step used in Marquardt optimization algorithm.</summary>*)
    
    property Lambda0: double read FLambda0 write SetLambda0;
    (*<summary>The precision for numeric gradient and/or Hessian matrix calculation.</summary>
      
<remarks>Sets the precision for numeric gradient and/or Hessian matrix calculation. This value is used only if
      <see cref="GradProcedure"/> or <see cref="GradHessProcedure"/> is nil (i.e. if
      numerical approximation is being used).
</remarks>
*)
    
    property GradTolerance: double read FGradTolerance write SetGradTolerance;
    (*<summary>Internal line search algorithm.</summary>
      
<remarks>Defines the internal line search algorithm. If true then Quasi-Newton and Conjugate gradient method
      internal line search algoritm will use soft line search  method. Set SoftSearch to true if you're using
      numerical approximation for gradient.

      If SoftSearch if false, then the internal line search algorithm will use exact line search method.
      Set SoftSearch to false if you're using *exact* gradient.
</remarks>
*)
    
    property SoftSearch: boolean read FSoftSearch write SetSoftSearch default true;
    (*<summary>Defines which gradient numerical approximation method  will be used to to evaluate gradient.</summary>
      
<remarks>Defines which gradient numerical approximation method  will be used to evaluate gradient.
      The NumericGradMethod is used only if <see cref="GradProcedure"/> is nil.
</remarks>
*)
    
    property NumericGradMethod: TNumericGradMethod read FNumericGradMethod write SetNumericGradMethod default numRichardson;
  end;

  
  TMtxLP = class;
  

  

  (*<summary>Interfaces Linear Programming algorithms.</summary>
    
<remarks>The component can be used to solve several linear programming problems.<para/>

    <b>How to use TMtxLP component?</b>
    * Drop a TMtxLP component on the form.
    * Set the <see cref="Minimize"/> property to minimize or maximize objective function f(x).
    * By setting <see cref="Algorithm"/> select which algorithm will be used to
      solve LP problem.
    * Define <see cref="A"/>, <see cref="b"/>, <see cref="c"/> and optionaly <see cref="Relations"/>
      , keeping in mind correct size of matrix, vectors and string.
    * Call the <see cref="Recalculate"/> method to solve LP problem.

    The results of solving LP are returned as:
    * Final tableaux <see cref="AFinal"/>,
    * Legitimate variables, stored in vector <see cref="x"/>,
    * Indexes of legitimate variables, stored in vector <see cref="Indexes"/> IValues,
    * Solution type, stored in  <see cref="SolutionType"/>,
    * Objective function <see cref="z"/>, evaluated at minimum or maximum.
</remarks>
*)
  
  TMtxLP = class(TMtxComponent)
  strict private
    fDirty: boolean;
    fAutoUpdate : boolean;
    fA,fAFinal: TMtx;
    fc,fx,fb: TVec;
    fIndexes: TVecInt;
    FMinimize: boolean;
    fAlgorithm: TLPAlgorithm;
    FSolutionType: TLPSolution;
    fVerbose: TStrings;
    fRelations: String;
    fz: double;
    procedure Setfc(const Value: TVec);
    procedure SetfA(const Value: TMtx);
    procedure Setfb(const Value: TVec);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetMinimize(const Value: boolean);
    procedure SetAlgorithm(const Value: TLPAlgorithm);
    procedure SetVerbose(const Value: TStrings);
    procedure SetRelations(const Value: String);
  public
    (*<summary>Checks A,b,c and relations dimensions.</summary>
      <returns>True, if system has correct dimensions.</returns>

      
<remarks>Checks if A.Cols equals to b.Length and A.Rows equals to c.Length.
      Additionally, if Two-Phase Simplex algorithm is used, checks if
      Relations string length is equal to b.Length.
</remarks>
*)
    function CheckDimensions: Boolean;

    (*<summary>Triggers TMtxLP recalculation.</summary>
      
<remarks>Triggers TMtxLP recalculation. After the Recalculate call <see cref="Dirty"/> property is set to false.

      The results are stored to:
      * <see cref="z"/> : Objective function, evaluated at minimum or maximum.
      * <see cref="x"/> : Values of the legitimate variables at minimum or maximum.
      * <see cref="Indexes"/> : Indices of legitimate variables in final tableaux.
      * <see cref="SolutionType"/> : Type of solution.
      * <see cref="AFinal"/> : Final tableaux.
</remarks>
*)
    procedure Recalculate;

    (*<summary>Becomes true, after any property has changed.</summary>
      
<remarks>When any of the TMtxLP properties changes, this property is automatically
      set to true. If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalculate"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalculate"/>*)
    property Dirty: boolean read fDirty write SetDirty;

    (*<summary>LP solution type.</summary>
      
<remarks>Returns LP solution type. Check <see cref="TLPSolution"/>
      type to learn more about different solution types.
</remarks>
*)
    property SolutionType: TLPSolution read fSolutionType;
    (*<summary>Values of the legitimate variables.</summary>
      
<remarks>Returns values of the legitimate variables (optimal solution).
</remarks>


      <seealso cref="z"/>
      <seealso cref="Indexes"/>*)
    property x: TVec read fx;
    (*<summary>Final tableaux.</summary>
      
<remarks>Returns the final tableaux.
</remarks>


      <seealso cref="A"/>*)
    property AFinal: TMtx read fAFinal;
    (*<summary>Returns indices (stored in IValues) of basic variables in final tableaux.</summary>

      <seealso cref="x"/>*)
    property Indexes: TVecInt read fIndexes;
    (*<summary>Log optimization algorithm calculation.</summary>
      
<remarks>If not nil then the optimization method uses it for logging each optimization step.
      By default the Verbose property is nil meaning no logging is done.
</remarks>


      

      <example>
        Log to TStringList log and copy contence to richTextBox.
      <code>
      TStringList log = new TStringList();
      MtxLP.Verbose = log;
      MtxLP.Recalculate();
	    richTextBox1.Text = log.Text; // copy text to RichTextBox
      </code></example>*)
    property Verbose: TStrings read fVerbose write SetVerbose;
    (*<summary>Value of the objective function.</summary>
      
<remarks>Returns objective function, evaluated at minimum or maximum.
</remarks>


      <seealso cref="x"/>*)
    property z: double read fz;
    constructor Create(aOwner: TComponent); override;
    
    destructor Destroy; override;
    
    classfunction  EditorClass: string; override;
    
  published
    
    property AutoUpdate: boolean read fAutoUpdate write SetAutoUpdate default false;
    
    (*<summary>Defines c in f=c(T)*x equation.</summary>
      
<remarks>Defines c in the <c>f=c(T)*x</c> equation.
</remarks>
*)
    property c: TVec read fc write Setfc;
    
    (*<summary>Defines A in A*x &lt;= b equation.</summary>
      
<remarks>Defines A in the <c>A*x &lt;= b</c> equation.
</remarks>
*)
    property A: TMtx read fA write SetfA;
    
    (*<summary>Defines b in A*x &lt;= b equation.</summary>
      
<remarks>Defines constraints b in the <c>A*x &lt;= b</c> equation.
</remarks>


      <seealso cref="Relations"/>*)
    property b: TVec read fb write Setfb;
    
    (*<summary>Defines relations for two phase algorithm.</summary>
      
<remarks>Defines relations for two-phase Simplex algorith. Length of relation string
      must be equal to length of vector b (number of constraints).
</remarks>


      <seealso cref="b"/>*)
    property Relations: String read fRelations write SetRelations;
    
    (*<summary>Find minimum or maximum of objective function.</summary>
      
<remarks>Find minimum or maximum of objective function:

      <c>min(max) f=c(T)*x</c><para/>
</remarks>


      <seealso cref="b"/>
      <seealso cref="A"/>*)
    property Minimize: boolean read FMinimize write SetMinimize default true;
    
    (*<summary>Defines LP solving algorithm.</summary>
      
<remarks>Defines which algorithm will be used to solve LP problem.
</remarks>
*)
    property Algorithm: TLPAlgorithm read fAlgorithm write SetAlgorithm;
  end;

