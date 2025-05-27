










(*<summary>Function minimization routines.</summary>
  
<remarks>Introduces the following algorithms for finding the minimum of scalar function of several variables:
  <list>
  <item> Nelder-Mead (<see cref="Simplex"/>); unconstrained and bounded minimization.</item>
  <item> Quasi-Newton (<see cref="BFGS"/> or DFP update scheme); unconstrained minimization.</item>
  <item> Levenberg-Marquardt (<see cref="Marquardt"/>); unconstrained minimization.</item>
  <item> Conjugate Gradient (Fletcher - Reeves or Polak - Ribiere uate scheme); unconstrained minimization.</item>
  </list>

  Introduces the following algorithms for finding the minimum of vector function of several variables:
  <list>
  <item> Trust Region (TR); unconstrained and bounded minimization.</item>
  </list>

  Introduces the following algorithms for finding the minimum of function of single variable:
  <list>
  <item> Brent method; unconstrained minimization.</item>
  </list>

  In addition, several routines for linear programming (LP) are also provided. The following algorithms are supported:
  <list>
  <item> Two phase Simplex method.</item>
  <item> Dual Simplex method.</item>
  <item> Simplex method.</item>
  <item> Gomory's cutting plane algorithm for solving integral problems.</item>
  </list>
</remarks>
*)
unit Optimization;


interface

{$I BdsppDefs.inc}

uses Math387, MtxVec, MtxVecInt, MtxIntDiff, AbstractMtxVec, MtxVecBase, AbstractMtxVecInt
  
  ,Nmkl
  

  
  ,NmklSingle
  

  

  
  ,SysUtils, Classes
  

  
     ,Types 
  
  ;

type

  (*<summary>Optimization methods.</summary>*)
  TOptMethod = (
    (*<summary>Nelder-Mead optimization method.</summary>*)optSimplex,
    (*<summary>Marquardt optimization method.</summary>*)optMarquardt,
    (*<summary>Quasi-Newton optimization method (Broyden-Fletcher-Goldfarb-Shanno
    Hessian update).</summary>*)
    optBFGS,
    (*<summary>Quasi-Newton optimization method (Davidson-Fletcher-Power Hessian
    update).</summary>*)
    optDFP,
    (*<summary>Conjugate Gradient optimization method (Fletcher-Reeves).</summary>*)optConjGradFR,
    (*<summary>Conjugate Gradient optimization method (Polak-Ribiere).</summary>*)optConjGradPR,
    (*<summary>Trust Region (TR) optimization method.</summary>*)optTR
    );


  (*<summary>Stop reason for the main loop in optimization.</summary>*)
  TOptStopReason = (
    (*<summary>The algorithm did not find minimum (within given tolerance).</summary>*)OptResNotFound,
    (*<summary>The gradient C-Norm is bellow given gradient tolerance.</summary>*)OptResSmallGrad,
    (*<summary>The inverse Hessian matrix is near singular.</summary>*)OptResSingular,
    (*<summary>Iteration x step is less than EPS.</summary>*)OptResSmallStep,
    (*<summary>Iteration x step is zero.</summary>*)OptResZeroStep,
    (*<summary>(Marquardt) Lambda is more than 1000.</summary>*)OptResBigLambda,
    (*<summary>(Marquardt) Lambda is less than 1E-300 (singularity).</summary>*) OptResSmallLambda,
    (*<summary>Number of iterations has reached the MaxIter.</summary>*) OptResMaxIter,
    (*<summary>Number of iterations is less than MaxIter <b>and</b> parameters are within given tolerance.</summary>*)OptResConverged,
    (*<summary>Jacobian matrix elements are bekiw given tolerance.</summary>*) optResSmallJacobian,
    (*<summary>Target function returned NAN value.</summary>*) optNANValue,
    (*<summary>Stop signal detected.</summary>*) optStopped
    );

  (*<summary>LP system solution.</summary>*)
  TLPSolution = (
    (*<summary>System has empty feasable region.</summary>*)LPEmptyFeasableRegion,
    (*<summary>System has a finite (bounded) solution.</summary>*)LPFiniteSolution,
    (*<summary>System has unbounded (plus or minus infinity) objective function f(x).
      Otherwise function objectiva value is bounded. In this case there are three possibilities for a linear
      programming problem. It may be bounded feasible, it may be unbounded feasible, and it may be infeasible.</summary>*)
      LPUnboundedObjectiveFunction
    );

  (*<summary>Linear programming algorithm.</summary>*)
  TLPAlgorithm = (
    (*<summary>Dual Simplex algorithm.</summary>*)LPAlgoDualSimplex,
    (*<summary>Two Phase Simplex algorithm.</summary>*)LPAlgoTwoPhaseSimplex,
    (*<summary>Simplex algorithm.</summary>*)LPAlgoSimplex,
    (*<summary>Gomory's cutting plane algorithm.</summary>*)LPAlgoCPA
    );

  (*<summary>Defines vector function of several variables.</summary>
    
<remarks>Defines vector function of several variables.
</remarks>

    <param name="X">Function variables.</param>
    <param name="Y">Function results.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="PConsts">Array of additional constants (pointers) which can be used in math formula.</param>

    

    <example>
      Define the vector function f(x,y) with three components:
    <code>
    private void VecFun(TVec x, TVec f, double[] c, object[] o)
    {
      f[0] = x[0]*Math.Sqrt(x[0]*x[0]+x[1]*x[1]);
      f[1] = x[1]*Math.Sqrt(x[0]*x[0]+x[1]*x[1]);
      f[2] = x[0]*(x[1]-x[0])*(x[1]-x[0]);
    }
	  </code></example>

    
    <seealso cref="TJacobianFunction"/>
    <seealso cref="TRealFunction"/>*)
  TVectorFunction = procedure(const X,Y: TVec; const Consts: array of double; const PConsts: array of TObject);

 (*<summary>Optional object class type to be passed to optimization routines.</summary>
            
<remarks>Create and pass this object as the "Verbose" parameter to the optimization routines. This object
            allows different type of optimization control, if the optimization is executed inside of a thread.
</remarks>
*)
  TOptControl = class(TStringList)
  public
    (*<summary> Returns True, if functiona evaluation is in progress (not for the purpose of gradient or jacoby, used only by TR for now)  </summary>*)
    FunEval: boolean;
    (*<summary> If True, log messages will be appended to the inherited TStringList. </summary>*)
    Verbose: Boolean;
    (*<summary> Initialized to false by the algorithm. The user can set this property to True (from another thread), to stop the optimization process.  </summary>*)
    Stop: Boolean;
    (*<summary> Set to True by the optimization algorithm just before the function will exit. </summary>*)
    Stopped: Boolean;
    (*<summary> Contains the current iteration count, which can be read from another thread, while the optimization algorithm is running. </summary>*)
    Iter: integer;
    (*<summary> Returns True, if before ms amount of miliseconds has elapsed, the Stopped field was set to True </summary>*)
    function WaitFor(ms: integer): boolean; virtual;
    constructor Create; 
  end;

  (*<summary>Defines procedure for calculating the Jacobian matrix.</summary>
    
<remarks>Jacobian matrix element (i,j) is defined as <c>partial(Y(j))/partial(C(i))</c>.
</remarks>


    

    <example>
      Define the Jacobian for the banana function.
    <code>
    // Split banana function in two parts
    private void BananaFun(TVec x, TVec f, double[] c, object[] o);
    {
      f[0] = 100*Math387.IntPower(x[1]-Math387.IntPower(x[0],2),2);
      f[1] = Math387.IntPower(1-x[0],2);
    }
    // Jacobian is 2x2 matrix
    private void BananaJac(TVec x, TVec f, TMtx Jac);
    {
      Jac[0,0] = -400*(x[1]-Math387.IntPower(x[0],2))*x[0]; // partial(f0)/ partial(x0)
      Jac[0,1] = -2*(1-x[0]); // partial(f1)/ partial(x0)
      Jac[1,0] = 200*(x[1]-Math387.IntPower(x[0],2)); // partial(f0)/ partial(x1)
      Jac[1,1] = 0.0; // partial(f1)/ partial(x1)
    }
    </code></example>

    
    <seealso cref="TVectorFunction"/>*)
  TJacobianFunction = procedure(const X,Y: TVec; const Jac: TMtx);

  (*<summary>Defines the procedure for calculating the gradient of a real function.</summary>
    <param name="Fun">Real function of several variables.</param>
    <param name="Pars">Function variables.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="PConsts">Array of additional constants (objects) which can be used in math formula.</param>
    <param name="Grad">Returns calculated gradient.</param>

	
<remarks>Defines the procedure for calculating the gradient of a real function.
</remarks>


    *)
  TGrad = procedure (Fun : TRealFunction; const Pars: TVec; Const Consts: TVec; Const PConsts: Array of TObject; const Grad: TVec);

  (*<summary>Defines the procedure for calculating the gradient and Hessian matrix of a real function.</summary>
    <param name="Fun">Real function of several variables.</param>
    <param name="Pars">variables.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="ObjConst">Array of additional constants (objects) which can be used in math formula.</param>
    <param name="Grad">Returns calculated gradient.</param>
    <param name="Hess">Returns calculated Hessian matrix.</param>

    
<remarks>Defines the procedure for calculating the gradient and Hessian matrix of a real function.
</remarks>


    *)
  TGradHess = procedure (Fun: TRealFunction; const Pars: TVec; Const Consts: TVec; Const ObjConst: Array of TObject; const Grad: TVec; const Hess: TMtx);

  (*<summary>Stores the stopping tests for TR optimization.</summary>
    
<remarks>The eps array contains the stopping tests:
    <list type="bullet">
    <item> eps[0]: <c>Grad &lt; eps[0]</c> </item>
    <item> eps[1]: <c>||F(x)|| &lt; eps[1] </c> </item>
    <item> eps[2]: <c>||A(x)ij|| &lt; eps[2]</c> </item>
    <item> eps[3]: <c>||s|| &lt; eps[3]</c> </item>
    <item> eps[4]: <c>||F(x)||- ||F(x) - A(x)s|| &lt; eps[4]</c> </item>
    <item> eps[5]: trial step precision. If eps[5] = 0, then eps[5] = <c>1E-10</c>, </item>
    </list>

    where
    <list type="bullet">
    <item>  <c>A(x) = the jacobi matrix</c> </item>
    <item>  <c>F(x) = ||y - f(x)||</c> </item>
    </list>
</remarks>


    *)
  TEPSArray = array[0..5] of double;


  TRCustomData = class
     Consts: TDoubleArray;
     OConsts: TObjectArray;
     Fun: TVectorFunction;
     Fun1: TMKLTRFunction;
     FloatPrecision: TMtxFloatPrecision;
     constructor Create;
     destructor Destroy; override;
  end;



  (*<summary>Minimizes single variable function.</summary>
    <param name="ax">Defines initial lower limit for minimum search.</param>
    <param name="bx">Defines initial uššer limit for minimum search.</param>
    <param name="Func">Real functin of single variable (must be of <see cref="TRealFunction"/> type) to be minimized.</param>
    <param name="Consts">Additional Fun constant parameters (can be/is usually nil). </param>
    <param name="ObjConst">Additional Fun constant parameters (can be/is usually nil).</param>
    <param name="MinX">Returns the position of function minimum.</param>
    <param name="MaxIter">Maximum allowed numer of minimum search iterations.</param>
    <param name="Accuracy">Desired minimum position tolerance.</param>
    <param name="Verbose">If assigned, stores Func, evaluated at each iteration step.
        Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
        to be interrupted from another thread and optionally also allows logging and iteration count monitoring.</param>
    <param name="FloatPrecision">Specifies the floating point precision to be used by the routine.</param>
    <returns>the number of iterations required to reach the solution(minimum) within given tolerance.</returns>

    
<remarks>Minimizes the function of one variable. This routine uses slightly modified version of the algol 60 procedure localmin, introduced by Richard Brent.
</remarks>


    

    <example>
      <u>Problem:</u> Find the minimum of the function of single variable by using the Brent method.

    <u>Solution:</u>The function is defined by the following equation:

    <IMG name="optimization006"/><para/>
    <code>
    private double Fun(TVec pars, TVec c, params object[] o)
    {
      return Math.Sin(Pars[0])+Math387.IntPower(Pars[0]+2,2);
	    // note that Pars holds only one variable !
    }
    private void Example()
    {
      // initial estimates for x1 and x2
      double x;
      double res = Optimization.MinBrent(-10,10,Fun,TMtxFloatPrecsion.mvDouble,null,null,out x, 500,1e-8,null);
      // stop if Iters >500 or Tolerance &lt; 1e-8
	    // Returns res = -1.8582461797
    }
    </code></example>

    <seealso cref="TRealFunction"/>*)
  
  function MinBrent(ax,bx: double; Func: TRealFunction; const FloatPrecision: TMtxFloatPrecision;
                 Const Consts: array of double; Const ObjConst: Array of TObject;
                 out MinX: double; MaxIter: integer; Accuracy: double = 1.0E-8; const Verbose: TStrings = nil): Integer; overload;

  (*<summary>Minimizes single variable function by using default settings and no log.</summary>
    
<remarks>Use this version if algorithm step logs are not needed.
</remarks>
*)
  
  function MinBrent(ax,bx: double; Func: TRealFunction; const FloatPrecision: TMtxFloatPrecision;
                 Const Consts: array of double; Const ObjConst: Array of TObject;
                 out MinX: double): Integer; overload;


  (*<summary>Minimizes the function of several variables by using the Nelder-Mead (Simplex) optimization method.</summary>
    <param name="Func"> Real function (must be of <see cref="TRealFunction"/> type) to be minimized.</param>
    <param name="Pars"> Stores the initial estimates for parameters (minimum estimate). After the call to routine returns
      adjusted calculated values (minimum position).</param>
    <param name="Consts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="ObjConst">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="FMin"> Returns function value at minimum.</param>
    <param name="StopReason"> Returns reason why minimum search stopped (see <see cref="TOptStopReason"/>).</param>
    <param name="MaxIter"> Maximum allowed numer of minimum search iterations.</param>
    <param name="Tolerance"> Desired Pars - minimum position tolerance.</param>
    <param name="FloatPrecision">Specifies the floating point precision to be used by the routine.</param>
    <param name="Verbose"> If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring.  </param>
    <returns>the number of iterations required to reach the solution(minimum) within given tolerance.</returns>

    
<remarks>Minimizes the function of several variables by using the Nelder-Mead (Simplex) optimization method. The advantage of Simplex
    method is it does not require gradient or Hessian.
</remarks>


    

    <example>
      <u>Problem:</u> Find the minimum of the "Banana" function by using the Nelder-Mead (Simplex) method.

    <u>Solution:</u>The Banana function is defined by the following equation:

    <IMG name="optimization001"/><para/>
    <code>
    private double Banana(TVec x, TVec c, params object[] o)
    {
      return 100*Math387.IntPower(x[1] - Math387.IntPower(x[0],2),2) + Math387.IntPower(1 - x[0],2);
    }
    private void Example()
    {
      double[2] x;
      double fmin;
      TOptStopReason StopReason;
      // initial estimates for x1 and x2
      x[0] = 0;
      x[1] = 0;
      int iters = Simplex(Banana,x,null,null,out fmin,out StopReason, TMtxFloatPrecision.mvDouble, 1000,1.0E-8,null);
      // stop if Iters >1000 or Tolerance &lt; 1e-8
      // Returns x = [1,1] and FMin = 0, meaning x1=1, x2=1 and minimum value is 0
    }
    </code></example>

    <seealso cref="TRealFunction"/>*)
  
  function Simplex(Func: TRealFunction; var Pars: Array of double; Const Consts: array of double;
               Const ObjConst: Array of TObject; out FMin: double;
               out StopReason: TOptStopReason; const FloatPrecision: TMtxFloatPrecision = mvDouble; MaxIter: Integer = 500;
               Tolerance : double = 1.0E-8; const Verbose: TStrings = nil): Integer; overload;

  (*<summary>Minimizes function of several variables by using Simplex
    optimization method with no algorithm step log.</summary>*)
  
  function Simplex(Func: TRealFunction; var Pars: double; Const Consts: array of double;
               Const ObjConst: Array of TObject; out FMin: double;
               out StopReason: TOptStopReason; const FloatPrecision: TMtxFloatPrecision; MaxIter: Integer; Tolerance: double): Integer; overload;

  (*<summary>Minimize function of several variables by using Simplex method with lower and/or upper bounds for parameters.</summary>

    
<remarks>This version supports lower and upper bound(s) for function parameters Pars. Lower and upper bounds are defined in LB and UP
    arrays respectively. Depending on lower and/or upper bound for parameter, there are several possibilities for LB and UB:
    <list>
    <item> Lower and upper bound: For each parameter both LB and UB should be set to specific value. </item>
    <item> Upper bound only: In this case, LB is set to -INF, UB to specific value. </item>
    <item> Lower bound only: In this case, LB should be set to specific value, UB to +INF. </item>
    <item> No bounds: In this case, use non-bounded Simplex version or set LB and UB to -INF and +INF respecively. </item>
    </list>
</remarks>


  

  <example>
    Minimize the "Banana" function, but use lower and upper limit for first parameter and only upper limit for second parameter.
  <code>
  private double Banana(TVec x, TVec c, object[] o)
  {
     return 100*Math387.IntPower(x[1] - Math387.IntPower(x[0],2),2) + Math387.IntPower(1 - x[0],2);
  }

  private void Example()
  {
    double[2] x;
    double fmin;
    TOptStopReason StopReason;
    // initial estimates for x1 and x2
    x[0] = 0;
    x[1] = 0;
    int iters = Simplex(Banana,x,null,null,
                  new double[] {0,-INF}, new double[] {0.5, 0.7},
                  out fmin,out StopReason, TMtxFloatPrecision.mvDouble,1000,1.0E-8,null);
    // stop if Iters >1000 or Tolerance &lt; 1e-8
  }
  </code></example>*)
  
  function Simplex(Func: TRealFunction; var Pars: Array of double; Const Consts: array of double;
               Const ObjConst: Array of TObject; Const LB, UB: Array of double; out FMin: double;
               out StopReason: TOptStopReason; const FloatPrecision: TMtxFloatPrecision; MaxIter: Integer = 500;
               Tolerance : double = 1.0E-8; const Verbose: TStrings = nil): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Quasi-Newton optimization algorithm.</summary>
    <param name="Fun">Real function (must be of <see cref="TRealFunction"/> type) to be minimized.</param>
    <param name="Grad">The gradient and Hessian procedure (must be of <see cref="TGrad"/> type), used for calculating
      the gradient.</param>
    <param name="Pars">Stores the initial estimates for parameters (minimum estimate). After the call to routine returns
                      adjusted calculated values (minimum position).</param>
    <param name="Consts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="ObjConst">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="FMin">Returns function value at minimum.</param>
    <param name="IHess">Returns inverse Hessian matrix.</param>
    <param name="StopReason">Returns reason why minimum search stopped (see <see cref="TOptStopReason"/>).</param>
    <param name="DFPAlgo">If True, BFGS procedure will use Davidon-Fletcher-Powell Hessian update scheme.
      If False, BFGS procedure will use Broyden-Fletcher-Goldberg-Shamo Hessian update scheme.</param>
    <param name="SoftLineSearch">If True, BFGS internal line search algoritm will use soft line search method. Set SoftLineSearch
      to true if you're using numerical approximation for gradient. If SoftLineSearch if false, BFGS internal line search.
      algorithm will use exact line search method. Set SoftLineSearch to false if you're using *exact* gradient.</param>
    <param name="MaxIter">Maximum allowed numer of minimum search iterations.</param>
    <param name="Tol">Desired Pars - minimum position tolerance.</param>
    <param name="GradTol">Minimum allowed gradient C-Norm.</param>
    <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
    <param name="FloatPrecision">Specifies the floating point precision to be used by the routine.</param>
    <returns>the number of iterations required to reach the solution(minimum) within given tolerance.</returns>

    

    <example>
      <u>Problem:</u> Find the minimum of the "Banana" function by using the BFGS-DFP method.<para/>
    <u>Solution:</u>The Banana function is defined by the following equation:

    <IMG name="optimization001"/><para/>
    Also, BFGS method requires the gradient of the function. The gradient of the
    Banana function is:

    <IMG name="optimization002"/><para/>
    <code>
    // Objective function
    private double Banana(TVec x, TVec c, params object[] o)
    {
       return 100*Math387.IntPower(x[1] - Math387.IntPower(x[0],2),2) + Math387.IntPower(1-x[0],2);
    }

    // Analytical gradient of the objective function
    private void BananaGrad(TRealFunction Fun, TVec Pars, TVec Consts, object[] PConsts, TVec Grad)
		{
			Grad[0] = -400*(Pars[1]-Pars[0]*Pars[0])*Pars[0]-2*(1-Pars[0]);
			Grad[1] = 200*(Pars[1]-Pars[0]*Pars[0]);
		}

    private void Example()
    {
      double[2] Pars;
      double fmin;
      TOptStopReason StopReason;
      // initial estimates for x1 and x2
      Pars[0] = 0;
      Pars[1] = 0;
      int iters = Optimization.BFGS(Banana,GradBanana,Pars,null,null,out fmin, iHess,
                  out StopReason, TMtxFloatPrecision.mvDouble, false,true,1000,1.0e-8,1.0e-8,null);
      // stop if Iters >1000 or Tolerance &lt; 1e-8
    }
    </code></example>

    <seealso cref="TGrad"/>
    <seealso cref="TRealFunction"/>
    <seealso cref="MtxIntDiff.NumericGradDifference"/>
    <seealso cref="MtxIntDiff.NumericGradRichardson"/>*)
  
  function BFGS(Fun: TRealFunction; Grad: TGrad;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double; const IHess: TMtx;
             out StopReason: TOptStopReason; const FloatPrecision: TMtxFloatPrecision;
             DFPAlgo: Boolean = false; SoftLineSearch: boolean = true; MaxIter: Integer = 500;
             Tol: double = 1.0E-8;
             GradTol: double =1.0E-8; const Verbose: TStrings = nil): Integer;overload;

  (*<summary>Minimizes the function of several variables by using the Quasi-Newton
    optimization method with no log.</summary>*)
  
  function BFGS(Fun: TRealFunction; Grad: TGrad;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;
             const FloatPrecision: TMtxFloatPrecision; DFPAlgo: Boolean): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Quasi-Newton
    optimization method with no log.</summary>*)
  
  function BFGS(Fun: TRealFunction; Grad: TGrad;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double; const IHess: TMtx;
             out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             DFPAlgo: Boolean; SoftLineSearch: boolean): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Quasi-Newton
    optimization method with no log.</summary>*)
  
  function BFGS(Fun: TRealFunction; Grad: TGrad;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double; const IHess: TMtx;
             out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             DFPAlgo: Boolean; MaxIter: Integer; Tol: double; GradTol: double): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Conjugate gradient optimization algorithm.</summary>
    <returns>the number of iterations required to reach the solution(minimum) within given tolerance.</returns>
    <param name="Fun">Real function (must be of <see cref="TRealFunction"/> type) to be minimized.</param>
    <param name="Grad">The gradient and Hessian procedure (must be of <see cref="TGrad"/> type), used for calculating
      the gradient.</param>
    <param name="Pars">Stores the initial estimates for parameters (minimum estimate). After the call to routine returns
      adjusted calculated values (minimum position).</param>
    <param name="Consts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="FloatPrecision">Specifies the floating point precision to be used by the routine.</param>
    <param name="ObjConst">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="FMin">Returns function value at minimum.</param>
    <param name="StopReason">Returns reason why minimum search stopped (see <see cref="TOptStopReason"/>).</param>
    <param name="FletcherAlgo">If True, ConjGrad procedure will use Fletcher-Reeves method. If false, ConjGrad procedure
      will use Polak-Ribiere method.</param>
    <param name="SoftLineSearch">If True, ConjGrad internal line search algoritm will use soft line search method. Set SoftLineSearch
      to true if you're using numerical approximation for gradient. If SoftLineSearch if false, ConjGrad internal line search
      algorithm will use exact line search method. Set SoftLineSearch to false if you're using *exact* gradient.</param>
    <param name="MaxIter">Maximum allowed numer of minimum search iterations.</param>
    <param name="Tol">Desired Pars - minimum position tolerance.</param>
    <param name="GradTol">Minimum allowed gradient C-Norm.</param>
    <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
            Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
          to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>

    

    <example>
      <u>Problem:</u> Find the minimum of the "Banana" function by using the Conjugate gradient method.<para/>
      <u>Solution:</u>The Banana function is defined by the following equation:

    <IMG name="optimization001"/><para/>
      Normally ConjGrad method would also require gradient procedure. But in this example we'll use the numerical
      approximation, more precisely the <see cref="MtxIntDiff.NumericGradRichardson"/> routine. This is done by specifying
      NumericGradRichardson routine as Grad parameter in ConjGrad routine call (see below)

    <code>
    // Objective function
    private double Banana(TVec x, TVec c, params object[] o)
    {
       return 100*Math387.IntPower(x[1] - Math387.IntPower(x[0],2),2) + Math387.IntPower(1-x[0],2);
    }

    private void Example()
    {
      double[2] Pars;
      double fmin;
      TOptStopReason StopReason;
      // initial estimates for x1 and x2
      Pars[0] = 0;
      Pars[1] = 0;
      int iters = Optimization.ConjGrad(Banana, MtxIntDiff.NumericGradRichardson, Pars, null, null, out fmin, out StopReason,
                                        mvDouble, true, false, 1000, 1.0e-8, 1.0e-8, null);
      // stop if Iters >1000 or Tolerance &lt; 1e-8
    }
    </code></example>

    <seealso cref="TGrad"/>
    <seealso cref="TRealFunction"/>
    <seealso cref="MtxIntDiff.NumericGradDifference"/>
    <seealso cref="MtxIntDiff.NumericGradRichardson"/>*)
  
  function ConjGrad(Fun: TRealFunction; Grad: TGrad;  var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double; out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             FletcherAlgo: Boolean = true; SoftLineSearch: boolean = true; MaxIter: Integer = 500;
             Tol: double = 1.0E-8;
             GradTol: double =1.0E-8; const Verbose: TStrings = nil): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Conjugate gradient
    optimization method with no log.</summary>*)
  
  function ConjGrad(Fun: TRealFunction; Grad: TGrad;  var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;
             const FloatPrecision: TMtxFloatPrecision; FletcherAlgo: Boolean): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Conjugate gradient
    optimization method with no log.</summary>*)
  
  function ConjGrad(Fun: TRealFunction; Grad: TGrad;  var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;  out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             FletcherAlgo: Boolean; SoftLineSearch: boolean ): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Conjugate gradient
    optimization method with no log.</summary>*)
  
  function ConjGrad(Fun: TRealFunction; Grad: TGrad;  var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;  out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             FletcherAlgo: Boolean; SoftLineSearch: boolean; MaxIter: Integer; Tol: double; GradTol: double): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Marquardt optimization algorithm.</summary>
    <param name="Fun">Real function (must be of <see cref="TRealFunction"/> type) to be minimized.</param>
    <param name="GradHess">The gradient and Hessian procedure (must be of <see cref="TGradHess"/> type), used for calculating
    the gradient and Hessian matrix.</param>
    <param name="Pars">Stores the initial estimates for parameters (minimum estimate). After the call to routine returns
      adjusted calculated values (minimum position).</param>
    <param name="Consts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="ObjConst">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="FloatPrecision">Specifies the floating point precision to be used by the routine.</param>
    <param name="FMin">Returns function value at minimum.</param>
    <param name="IHess">Returns inverse Hessian matrix.</param>
    <param name="StopReason">Returns reason why minimum search stopped (see <see cref="TOptStopReason"/>).</param>
    <param name="MaxIter">Maximum allowed numer of minimum search iterations.</param>
    <param name="Tol">Desired Pars - minimum position tolerance.</param>
    <param name="GradTol">Minimum allowed gradient C-Norm.</param>
    <param name="Lambda0">Initial lambda step, used in Marquardt algorithm.</param>
    <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
    <returns>the number of iterations required to reach the solution(minimum) within given tolerance.</returns>

    

    <example>
      <u>Problem:</u> Find the minimum of the "Banana" function by using the Marquardt method.

      <u>Solution:</u>The Banana function is defined by the following equation:<para/>
      <IMG name="optimization001"/><para/>
      Also, Marquardt method requires the gradient and Hessian matrix of the function. The gradient of the
      Banana function is:

      <IMG name="optimization002"/><para/>
      and the Hessian matrix is :

      <IMG name="optimization003"/><para/>
      <code>
      // Objective function
      private double Banana(TVec x, TVec c, params object[] o)
      {
        return 100*Math387.IntPower(x[1] - Math387.IntPower(x[0],2),2) + Math387.IntPower(1-x[0],2);
      }

      // Analytical gradient and Hessian matrix of the objective function
      private void BananaGradHess(TRealFunction Fun, TVec Pars, TVec Consts, object[] obj, TVec Grad, TMtx Hess)
      {
        double[] Pars = Parameters.PValues1D(0);

        Grad[0] = -400.0*(Pars[1]-Math387.IntPower(Pars[0],2))*Pars[0] - 2*(1-Pars[0]);
        Grad[1] = 200.0*(Pars[1]-Math387.IntPower(Pars[0],2));
        Hess.Values1D[0] = -400.0*Pars[1]+1200*Math387.IntPower(Pars[0],2)+2;
        Hess.Values1D[1] = -400.0*Pars[0];
        Hess.Values1D[2] = -400.0*Pars[0];
        Hess.Values1D[3] = 200.0;
      }

      private void Example()
      {
        double[2] Pars;
        double fmin;
        Matrix iHess = new Matrix(0,0);
        TOptStopReason StopReason;
      // initial estimates for x1 and x2
      Pars[0] = 0;
      Pars[1] = 0;
      int iters = Optimization.Marquardt(Banana,BananaGradHess,Pars, null, null, out fmin, iHess, out StopReason,
                                         TMtxFloatPrecision.mvDouble, 1000, 1.0e-8, 1.0e-8, null);
      // stop if Iters >1000 or Tolerance &lt; 1e-8
    }
    </code></example>

    <seealso cref="TGradHess"/>
    <seealso cref="TRealFunction"/>
    <seealso cref="MtxIntDiff.NumericGradHess"/>*)
  
  function Marquardt(Fun: TRealFunction; GradHess: TGradHess;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;
             IHess: TMtx; out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision;
             MaxIter: Integer = 500;
             Tol: double = 1.0E-8;
             GradTol: double =1.0E-8; Lambda0: double = 1e-2; Verbose: TStrings = nil): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Marquardt optimization
    method with no log.</summary>*)
  
  function Marquardt(Fun: TRealFunction; GradHess: TGradHess;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;
             const FloatPrecision: TMtxFloatPrecision): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Marquardt optimization
    method with no log.</summary>*)
  
  function Marquardt(Fun: TRealFunction; GradHess: TGradHess;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double; out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision): Integer; overload;

  (*<summary>Minimizes the function of several variables by using the Marquardt optimization
    method with no log.</summary>*)
  
  function Marquardt(Fun: TRealFunction; GradHess: TGradHess;
             var Pars: Array of double; Const Consts: array of double;
             Const ObjConst: Array of TObject; out FMin: double;
             IHess: TMtx; out StopReason: TOptStopReason;
             const FloatPrecision: TMtxFloatPrecision; MaxIter: Integer; Tol: double; GradTol: double;
             Lambda0: double): Integer; overload;

  (*<summary>Trust region algorithm for finding minimum of vector function.</summary>
    <param name="Fun">The objective function to be minimized.</param>
    <param name="JacProc">The Jacobian matrix calculation procedure. If it is nil, then the numerical approximation
      will be used to evaluate Jacobi matrix elements.</param>
    <param name="X">Stores the initial estimates for X. On completion returns estimates, evaluated at function minimum.</param>
    <param name="Y">Returns objective function value, evaluated at minimum.</param>
    <param name="Consts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="OConsts">Additional Fun constant parameteres (can be/is usually nil).</param>
    <param name="MaxIter">Defines the maximum number of main TR algorithm loops.</param>
    <param name="MaxTrialIter">Defines the maximum number of iterations of trial-step calculation.</param>
    <param name="Control">Optional object, which allows the interruption of the search.</param>
    <param name="EPSArray">Array of size 6. Contains stopping tests.
    <list>
      <item> eps[0]: <c>Grad &lt; eps[0]</c> </item>
      <item> eps[1]: <c>||F(x)|| &lt; eps[1] </c> </item>
      <item> eps[2]: <c>||A(x)ij|| &lt; eps[2]</c> </item>
      <item> eps[3]: <c>||s|| &lt; eps[3]</c> </item>
      <item> eps[4]: <c>||F(x)||- ||F(x) - A(x)s|| &lt; eps[4]</c> </item>
      <item> eps[5]: trial step precision. If eps[5] = 0, then eps[5] = <c>1E-10</c>, </item>
    </list>
    <para/>
      where
    <list>
      <item> <c>A(x) = the jacobi matrix</c> </item>
      <item> <c>F(x) = ||y - f(x)||</c> </item>
    </list>
    </param>
    <param name="Rs">Positive input variable used in determining the initial step bound, the initial size of the trust region.
                     In most cases the factor should lie within the interval (0.1, 100.0).
                     The generally recommended value is 100, which is used by default, if this param is set to 0.</param>
    <param name="StopReason">Returns the TR algorithm stop reason.</param>
    <returns>the number of iterations needed for results.</returns>

    
<remarks>The Trust Region (TR) algorithms are relatively new iterative algorithms for solving nonlinear optimization
    problems. They are widely used in power engineering, finance, applied mathematics, physics, computer science,
    economics, sociology, biology, medicine, mechanical engineering, chemistry, and other areas. TR methods have
    global convergence and local super convergence, which differenciates them from line search methods and Newton
    methods. TR methods have better convergence when compared with widely-used Newton-type methods.

    The main idea behind TR algorithm is calculating a trial step and checking if the next values of x belong
    to the trust region. Calculation of the trial step is strongly associated with the approximation model.

    For more on TR algorithm, check the following links:
    * <exref target="http://iridia.ulb.ac.be/~fvandenb/optimization/optimizationIntro.html">http://iridia.ulb.ac.be/~fvandenb/optimization/optimizationIntro.html</exref>
    * <exref target="http://en.wikipedia.org/wiki/Trust_region">http://en.wikipedia.org/wiki/Trust_region</exref>

    Note:
      This function is currently not supported under .NET.
      The objective function calls are threaded. The objective function should not be
      allocating any memory on the heap because calling threads are created outside of Delphi/BCB code.
</remarks>


    <Example>
    <code>
    Uses MtxExpr, Optimization, MtxVec, Math387;
    // Objective function, 4 variables, 4 f components
    procedure TestVFun(const x,f: TVec; const c: Array of double; const o: Array of TObject);
    begin
      f[0] := x[0] + 10.0*x[1];
      f[1] := 2.2360679774997896964091736687313*(x[2] - x[3]);
      f[2] := (x[1] - 2.0*x[2])*(x[1] - 2.0*x[2]);
      f[3] := 3.1622776601683793319988935444327*(x[0] - x[3])*(x[0] - x[3]);
    end;
    procedure Example;
    var x,f: Vector;
      epsa: TEPSArray;
      sr: TOptStopReason;
    begin
      // Initial estimates for variabless
      x.SetIt(false,[3,-1,0,1]);
      // 4 components, size must match the TestVFun implementation above
      f.Size(4);
      // setup stopping criteria, use default values
      epsa[0] := 1.0E-5;
      epsa[1] := 1.0E-5;
      epsa[2] := 1.0E-5;
      epsa[3] := 1.0E-5;
      epsa[4] := 1.0E-5;
      epsa[5] := 1.0E-10;

      // Minimize
      TrustRegion(TestVFun,x,f,[],[],1000,100,epsa,0.0,sr);
      // x stores minimum position (variables)
      // f stores function value at minimum
      // sr stores stop reason
    end;
    </code>
    <code lang="C++">
    #include "MtxExpr.hpp"
    #include "Math387.hpp"
    #include "Optimization.hpp"

    void __fastcall TestVFun(TVec* const x, TVec* const f, const double * c, const int c_Size, System::TObject* const * o, const int o_Size)
    {
      double* X = x->PValues1D(0);

      f->Values[0] = X[0] + 10.0*X[1];
      f->Values[1] = 2.2360679774997896964091736687313*(X[2] - X[3]);
      f->Values[2] = (X[1] - 2.0*X[2])*(X[1] - 2.0*X[2]);
      f->Values[3] = 3.1622776601683793319988935444327*(X[0] - X[3])*(X[0] - X[3]);
    }

    void __fastcall Example();
    {
      Vector x,f;
      TEPSArray epsarr;
      TOptStopReason sr;
      // Initial estimates for variabless
      x->SetIt(false,OPENARRAY(double,(3,-1,0,1)));
      // 4 components, size must match the TestVFun implementation above
      f->Size(4,false);
      // setup stopping criteria, use default values
      epsarr[0] = 1.0E-5;
      epsarr[1] = 1.0E-5;
      epsarr[2] = 1.0E-5;
      epsarr[3] = 1.0E-5;
      epsarr[4] = 1.0E-5;
      epsarr[5] = 1.0E-10;

      // Minimize
      TrustRegion(TestVFun,NULL,x,f,NULL,-1,NULL,-1,1000,100,epsarr,0.0,sr);
      // x stores minimum position (variables)
      // f stores function value at minimum
      // sr stores stop reason
    }
    </code>
    </Example>*)
  
  function TrustRegion(Fun: TVectorFunction; JacProc: TJacobianFunction; const X,Y: TVec; Const Consts: Array of double;
    Const OConsts: Array of TObject; MaxIter, MaxTrialIter: Integer; Const EPSArray: TEPSArray;
    Rs: double; out StopReason: TOptStopReason; const Control: TOptControl = nil): Integer; overload;

  
  (*<summary></summary>*)
  function TrustRegion(Fun: TMKLTRFunction; JacProc: TJacobianFunction; const X,Y: TVec; const CustomData: TObject; MaxIter, MaxTrialIter: Integer;
     Const EPSArray: TEPSArray; Rs: double; out StopReason: TOptStopReason; const Control: TOptControl = nil): Integer; overload;

  (*<summary>Trust region algorithm to find bounded minimum of vector function.</summary>
    
<remarks>Additional parameters LB and UB define x lower and/or upper bounds.
</remarks>
*)
  
  function TrustRegion(Fun: TVectorFunction; JacProc: TJacobianFunction; const X,Y: TVec; const LB,UB: TVec;
    Const Consts: Array of double; Const OConsts: Array of TObject; MaxIter, MaxTrialIter: Integer; Const EPSArray: TEPSArray;
    Rs: double; out StopReason: TOptStopReason; const Control: TOptControl = nil): Integer; overload;

  
  (*<summary></summary>*)
  function TrustRegion(Fun: TMKLTRFunction; JacProc: TJacobianFunction; const X,Y: TVec; const CustomData: TObject; const LB,UB: TVec;
    MaxIter, MaxTrialIter: Integer; Const EPSArray: TEPSArray; Rs: double;
    out StopReason: TOptStopReason; const Control: TOptControl = nil): Integer; overload;


  (*<summary>Linear optimization by Dual Simplex algorithm.</summary>
    <returns>Value of the objective function, evaluated at minimum or maximum.</returns>
    <param name="A">Defines initial values for <c>A*x &gt;= b</c> relation.</param>
    <param name="c">Defines values in <c>f=c(T)*x</c> equation.</param>
    <param name="b">Defines initial values for <c>A*x &gt;= b</c> relation.</param>
    <param name="AFinal">Returns the final tableaux.</param>
    <param name="X">Returns values of the legitimate variables (optimal solution).</param>
    <param name="Indexes">Returns indices (in IValues) of the basic variables in AFinal tableu.</param>
    <param name="Minimize">If false, find minimum of the objective function f. If false, find maximum of the objective function f.</param>
    <param name="SolutionType">Returns type of LP solution.</param>
    <param name="Verbose">If assigned, each tableu and row/column pivoting is logged to Verbose.
                  Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
                  to be interrupted from another thread and optionally also allows logging and iteration count monitoring.</param>

    
<remarks>Solves the following optimization problem:

    <IMG name="optimization008"/><para/>
    Components of the vector b are not required to satisfy the nonnegativity constraints. The Dual
    Simplex Algorithm has numerous applications to other problems of linear programming. It is used,
    for instance, in some implementations of the Gomory's cutting plane algorithm for solving the
    integer programming problems.
</remarks>


    

    <example>
      Solve linear programming problem with 3 equations and 4 positive constraints.
    <code>
    using Dew.Math.Units;
    using Dew.Math;

    namespace Dew.Examples
    {
      private void Example()
      {
        Matrix A=new Matrix(0,0);
        Matrix Af=new Matrix(0,0);
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector x = new Vector(0);
        Vector iondexes = new Vector(0);
        TLPSolution sol;
        A.SetIt(3,4,false, new double[] {2,1,5,0,1,2,3,0,1,1,1,1});
        b.SetIt(new double[] {20,25,10});
        c.SetIt(new double[] {1,2,3,-1});
        // Find maximum using above system
        double f = Optimization.SimplexDual(A,b,c,Af,x,indexes, out sol, false, null);
      }
    }
    </code></example>

    <SeeAlso cref="SimplexTwoPhase"/>*)
  
  function SimplexDual(const A: TMtx; const b,c: TVec; const AFinal: TMtx; const X: TVec; const Indexes: TVecInt; out SolutionType: TLPSolution;
    Minimize: boolean = true; const Verbose: TStrings = nil): double;

  (*<summary>Linear optimization by Two-Phase Simplex algorithm.</summary>
    
<remarks>Solves the following optimization problem:

    <img name="optimization009"/><para/>
    where rel holds the relation signs. For example, relation string
    <c>' &lt; = &gt; '</c> means the constraints system consists of one inequality <c>' &lt;= '</c>, one
    equality <c>' = '</c> and one more inequality <c>' &gt;= '</c>.

    Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
                  to be interrupted from another thread and optionally also allows logging and iteration count monitoring.
</remarks>


    

    <example>
    Solve the following LP problem:

    <c>f(x) = x1+x2+2*x3-2*x4</c><para/>
    subject to:<para/>
    <c>f(x) = x1+x2+2*x3-2*x4</c><para/>
    subject to:<para/>
    <c>x1+2x      &lt;= 700</c><para/>
    <c>2x2-8x4    &lt;= 0</c><para/>
    <c>x2-2x3+x4   &gt; 1</c><para/>
    <c>x1+x2+x3+x4 = 1</c><para/>
    which translates to using two-phase simplex method:
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      private void Example()
      {
        Matrix A = new Matrix(0,0);
        Matrix AFinal = new Matrix(0,0);
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector x = new Vector(0);
        Vector indexes = new Vector(0);
        TLPSolution sol;
        A.SetIt(4,3,false,new double[] {1,1,0,0
                         0,2,0,-8,
                         0,1,-2,1,
                         1,1,1,1});
        b.SetIt(new double[] {700,0,1,1});
        c.SetIt(new double[] {1,1,2,-2});
        string relations = " &lt; &lt; &gt; = ";  //remove spaces from the string:
        relations = relations.Replace(" ", ""); //for XMLDoc only
        double f = Optimization.SimplexTwoPhase(A,b,c,relations,AFinal,x,indexes, out sol, true,null);
      }
    }
    </code></example>

    <seealso cref="SimplexDual"/>
    <seeAlso cref="SimplexLP"/>*)
  
  function SimplexTwoPhase(const A: TMtx; const b,c: TVec; const relations: String; const AFinal: TMtx; const X: TVec;
    const Indexes: TVecInt; out SolutionType: TLPSolution; Minimize: boolean = true; const Verbose: TStrings = nil): double;

  (*<summary>Linear optimization by using Simplex method.</summary>
    
<remarks>Solves the following optimization problem:

    <IMG name="optimization007"/>

    where vector b must be a positive vector. Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring.
</remarks>


    <seealso cref="SimplexDual"/>
    <seealso cref="SimplexTwoPhase"/>
    <seealso cref="CPA"/>*)
  
  function SimplexLP(const A: TMtx; const b,c: TVec; const AFinal: TMtx; const X: TVec; const Indexes: TVecInt; out SolutionType: TLPSolution;
    Minimize: boolean = true; const Verbose: TStrings = nil): double;

  (*<summary>Gomory's cutting plane algorithm for solving the integer programming problem.</summary>
    
<remarks>Implements the Gomory's Cutting Plane Algorithm (CPA) for solwing the following linear programming problem

    <img name="optimization010"/>

    with decision variables being restricted to integer values.

    Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring.
</remarks>


    

    <example>
    Minimize the following linear programming problem:

    <c>f(x)=x1-3x2 (x1,x2 integers)</c><para/>

    <c>x1-x2=2</c><para/>
    <c>2x1+4x2=15</c><para/>
    which translates to:
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples
    {
      private void Example()
      {
        Matrix A = new Matrix(0,0);
        Matrix AFinal = new Matrix(0,0);
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector x = new Vector(0);
        Vector indexes = new Vector(0);
        TLPSolution sol;
        A.SetIt(2,2,false,new double[] {1,-1,2,4});
        b.SetIt(new double[] {2,15});
        c.SetIt(new double[] {1,-3});
        double f = Optimization.CPA(A,b,c,AFinal,x,indexes, out sol, true,null);
        // f = -9.0
        // x1=0, x2=3
      }
    }
    </code></example>

    <SeeAlso cref="SimplexLP"/>*)
  
  function CPA(const A: TMtx; const b,c: TVec; const AFinal: TMtx; const X: TVec; const Indexes: TVecInt; out SolutionType: TLPSolution;
    Minimize: boolean = true; const Verbose: TStrings = nil): double;

  
  function StopReasonToStr(const stopReason: TOptStopReason): string;



