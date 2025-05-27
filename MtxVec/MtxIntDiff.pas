











(*<summary>Adds support for numerical integration and differentiation.</summary>*)
unit MtxIntDiff;


interface

{$I BdsppDefs.inc}

uses MtxVec, MtxVecInt, Math387
    
     ,Types 
    
    ;

type

    (*<summary>Defines numerical integration algorithm exit reason.</summary>*)
    TIntStopReason = (
    (*<summary>The result converged within given tolerance.</summary>*)intConverged,
    (*<summary>The result did not converge within given tolerance.</summary>*)intNotConverged);

    (*<summary>Defines weight type for Gauss quadrature integration algorithm.</summary>
      <seealso cref="QuadGauss"/>*)
    TQuadMethod = (
    (*<summary>Use the Gauss quadrature formula for calculating weights and base points.</summary>*)qmGauss,
    (*<summary>Use the Chebyshew-Gauss formula to calculate base points and weight.</summary>*)qmChebyshevGauss,
    (*<summary>Use thd Newron-Cotes formula to calculate base points and weights.</summary>*)qmNewtonCotes);

    (*<summary>Defines integration algorithm.</summary>*)
    TIntegrateAlgo = (
    (*<summary>Traapez rule method.</summary>*)intAlgoTrapez,
    (*<summary>Simpson algorithm.</summary>*)intAlgoSimpson,
    (*<summary>Boole integration method.</summary>*)intAlgoSimpson38,
    (*<summary>Gauss integration algorithm.</summary>*)intAlgoGauss,
    (*<summary>Romberg method.</summary>*)ntAlgoRomberg,
    (*<summary>Monte-Carlo method.</summary>*)intAlgoMonteCarlo
    );

  (*<summary>Numerical gradient approximation methods.</summary>
    
<remarks>Defines different numerical gradient approximation methods.
</remarks>


    <seealso cref="NumericGradRichardson"/>
    <seealso cref="NumericGradDifference"/>*)
  TNumericGradMethod = (
  (*<summary>High precision numerical differentiation. The algorithm uses
    Richardson extrapolation of three values of the symmetric difference quotient.</summary>*)numRichardson,
  (*<summary>Numerical differentiation using the forward difference quotient.</summary>*)numDifference);


  (*<summary>Numerical gradient by high precision numerical differentiation.</summary>
    <param name="Fun">Real function of several variables.</param>
    <param name="Pars">Function variables.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="ObjConst">Array of additional constants (pointers) which can be used in math formula.</param>
    <param name="Grad">Returns calculated gradient. If needed, Grad Length and Complex properties are adjusted automatically.</param>

    
<remarks>Calculates the numerical gradient by high precision numerical differentiation.
    The algorithm uses Richardson extrapolation of three values of the symmetric difference quotient. The gradient step size is defined by <see cref="GradStepSize"/>
    global variable. Normally the optimal stepsize depends on seventh partial derivatives of the function. Since they are not available, the initial value for
    GradientStepSize is <c>Exp(Ln(EPS)/7)*0.25</c>, as suggested by Spellucci.
</remarks>


    <seealso cref="NumericGradDifference"/>
    <seealso cref="Optimization.BFGS"/>
    <seealso cref="Optimization.ConjGrad"/>
    <seealso cref="TGrad"/>*)
  procedure NumericGradRichardson(Fun : TRealFunction; const Pars: TVec; const Consts: TVec; const ObjConst: Array of TObject; const Grad: TVec);

  (*<summary>Numerical gradient by numerical differentiation using the forward difference quotient.</summary>
    <param name="Fun">Real function of several variables.</param>
    <param name="Pars">Function variables.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="ObjConst">Array of additional constants (pointers) which can be used in math formula.</param>
    <param name="Grad">Returns calculated gradient. If needed, Grad Length and Complex properties are adjusted automatically.</param>

    
<remarks>Calculates the numerical gradient by numerical differentiation using the forward difference quotient. The NumericGradDifference procedure can be used with a
    very moderate <c>( &gt;= 1.0e-4)</c> precision requirement.
</remarks>


    <seealso cref="NumericGradRichardson"/>*)
  procedure NumericGradDifference(Fun : TRealFunction; const Pars: TVec;const Consts: TVec; Const ObjConst: Array of TObject; const Grad: TVec);
  (*<summary>Numerical gradient and Hessian matrix.</summary>
    <param name="Fun">Real function of several variables.</param>
    <param name="Pars">Function variables.</param>
    <param name="Consts">Array of additional constants which can be used in math formula.</param>
    <param name="ObjConst">Array of additional constants (pointers) which can be used in math formula.</param>
    <param name="Grad">Returns calculated gradient. If needed, Grad Length and Complex properties are adjusted automatically.</param>
    <param name="Hess">Returns calculated Hessian matrix. If needed, Hess matrix size and complex properties are adjusted automatically.</param>

    
<remarks>Calculates the numerical gradient and Hessian matrix.
</remarks>


    <seealso cref="Optimization.Marquardt"/>
    <seealso cref="TGradHess"/>*)
  procedure NumericGradHess(Fun: TRealFunction; const Pars: TVec;const Consts: TVec; Const ObjConst: Array of TObject; const Grad: TVec; const Hess: TMtx);

  (*<summary>Gauss base points and weights.</summary>
    
<remarks>Calculates base points and weight factors by using the so called Gauss algorithm given by Davis and Rabinowitz in 'Methods
    of Numerical Integration', page 365, Academic Press, 1975.
</remarks>


    

    <example>
      Use a ten point Gauss formula to evaluate six(x) on interval <c>[0,PI]</c>.
      For start use only one subsection.
    <code>
    private double IntFun(TVec x, TVec c, params object[] o)
    {
      double x = x[0];
      return System.Math.Sin(x)*System.Math.Exp(-x*x);
    }
    private void Example()
    {
      Vector bpoints = new Vector(0);
      Vector weights = new Vector(0);
      MtxIntDiff.WeightsGauss(10,bpoints,weights);
      double area = MtxIntDif.QuadGauss(IntFun,-0.5*System.Math.PI,System.Math.PI,bpoints,weights,64);
    }
    </code></example>

    <seealso cref="WeightsChebyshevGauss"/>
    <seealso cref="WeightsNewtonCotes"/>*)
  
  procedure WeightsGauss(NumPoints: Integer; const Points, Weights: TVec; const FloatPrecision: TMtxFloatPrecision =  mvDouble );

  (*<summary>Chebyshev-Gauss base points and weights.</summary>
    
<remarks>Calculates base points and weights for numerical integration by using the
    Chebyshev-Gauss scheme.
    Check the following <see href="http://mathworld.wolfram.com/Chebyshev-GaussQuadrature.html">link</see>
    to learn more about this algorithm
</remarks>


    <seealso cref="WeightsNewtonCotes"/>
    <seealso cref="WeightsGauss"/>*)
  procedure WeightsChebyshevGauss(NumPoints: Integer; const Points, Weights: TVec; const FloatPrecision: TMtxFloatPrecision =  mvDouble );

  (*<summary>Newton-Cotes base points and weights.</summary>
    <param name="PolyOrder">Defines the interpolating polynomial order.</param>
    <param name="Points">Returns base points coordinate.</param>
    <param name="Weights">Returns weights.</param>
    <param name="FloatPrecision">Specifies the precision of the weights to be returned.</param>

    
<remarks>Uses the Newton-Cotes formulas to calculate base points and weights for numerical integration.
    Check the following <see href="http://mathworld.wolfram.com/Newton-CotesFormulas.html">link</see>
    to learn more about Newton-Cotes formulas.
</remarks>


    

    <example>
      Use Simpson rule to evaluate sin(x) on interval [0,PI].

    <code>
    // Integrating function
    private double IntFunc(TVec Parameters, TVec c, params object[] o)
    {
      double x = Parameters[0];
      return System.Math.Sin(x);
    }
    // Integrate
    private void DoIntegrate()
    {
      Vector bpoints = new Vector(0);
      Vector weights = new Vector(0);
      MtxIntDiff.WeightsNewtonCotes(2,bpoints,weights); // 2 means Simpson
      double area = QuadGauss(IntFunc,0,System.Math.PI,bpoints,weights,1);
    }
    </code></example>

    <seealso cref="WeightsChebyshevGauss"/>
    <seealso cref="WeightsGauss"/>*)
  procedure WeightsNewtonCotes(PolyOrder: Integer; const Points, Weights: TVec; const FloatPrecision: TMtxFloatPrecision =  mvDouble );

  (*<summary>Numerical integration by using regular Gaussian quadrature scheme.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>

    
<remarks>Check the following <exref target="http://mathworld.wolfram.com/GaussianQuadrature.html">link</exref>
    to learn more about this algorithm
</remarks>


    

    <example>
      Evaluate fuction <c>Sin(x)*Exp(-x^2)</c> on interval <c>-PI/2</c>, PI.

    <code>
    private double IntFun(TVec x, TVec c, params object[] o)
    {
      double x = x[0];
      return System.Math.Sin(x)*System.Math.Exp(-x*x);
    }
    private void Example()
    {
      Vector bpoints = new Vector(0);
      Vector weights = new Vector(0);
      MtxIntDiff.WeightsGauss(10,bpoints,weights);
      double area = MtxIntDif.QuadGauss(IntFun,-0.5*System.Math.PI,System.Math.PI,bpoints,weights,64);
    }
    </code></example>

    <seealso cref="Romberg"/>
    <seealso cref="WeightsGauss"/>
    <seealso cref="WeightsNewtonCotes"/>
    <seealso cref="WeightsChebyshevGauss"/>*)
  function QuadGauss(Fun: TRealFunction; lb, ub: double; const BasePoints, Weights: TVec; Parts: Integer):double; overload;

  (*<summary>Numerical integration by using regular Gaussian quadrature scheme (Fun defined only with double(s)).</summary>*)
  function QuadGauss(Fun: TRealFunction; lb, ub: double; const Constants: TVec; Const ObjConst: Array of TObject; const BasePoints, Weights: TVec; Parts: Integer):double; overload;

  (*<summary>Evaluate the numerical integral between lower and upper bound using Gauss quadrature algorithm.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>

    
<remarks>This version calculates base points and weights on the fly.
</remarks>


    <seealso cref="Romberg"/>*)
  function QuadGauss(Fun: TRealFunction; lb, ub: double; const Constants: TVec; Const ObjConst: Array of TObject; out StopReason: TIntStopReason; const FloatPrecision: TMtxFloatPrecision; QMethod: TQuadMethod = qmGauss; Tolerance: double = 1.0E-4; MaxIter:Integer = 8):double; overload;

  (*<summary>Integration by using Gauss quadrature algorithm with no additional parameters for Fun.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>

    
<remarks>This version calculates base points and weights on the fly.

    Note
      Use this overload if integrating function is defined only by double parameter(s).
</remarks>


    

    <example>
        Evaluate fuction <c>Sin(x)*Exp(-x^2)</c> on interval <c>[-PI/2, PI]</c>.
        Use default Gauss base points and weights.

    <code>
    private double IntFun(TVec x, TVec c, params object[] o)
    {
        double x = x[0];
        return System.Math.Sin(x)*System.Math.Exp(-x*x);
    }
    private void Example()
    {
        TIntStopReason sr;
        double area = MtxIntDif.QuadGauss(IntFun,-0.5*System.Math.PI, System.Math.PI, out sr, 1.0e-4, 8);
    }
    </code></example>*)
   function QuadGauss(Fun: TRealFunction; lb, ub: double; out StopReason: TIntStopReason; const FloatPrecision: TMtxFloatPrecision; QMethod: TQuadMethod = qmGauss;  Tolerance: double = 1.0E-4; MaxIter:Integer = 8):double; overload;

  (*<summary>Numerical integration by using recursive Romberg algorithm.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>
    <param name="Fun">Integrating function.</param>
    <param name="Constants">Additional constants defining Fun function, usually nil/null.</param>
    <param name="ObjConst">Additional objects defining Fun function, usually nil/null.</param>
    <param name="lb">Defines lower bound.</param>
    <param name="ub">Defines upper bound.</param>
    <param name="StopReason">Returns algorithm stop reason.</param>
    <param name="Tolerance">Defines integration tolerance.</param>
    <param name="MaxIter">Defines maximum number of alorithm iterations.</param>
    <param name="FloatPrecision">Defines the computational precision to be used by the routine.</param>

    

    <example>
      Evaluate fuction <c>Sin(x)*Exp(-x^2)</c> on interval <c>[0,PI/2]</c> by using Romberg algorithm.

    <code>
    private double IntFun(TVec x, TVec c, params object[] o)
    {
      double x = x[0];
      return System.Math.Sin(x)*System.Math.Exp(-x*x);
    }
    private void Example()
    {
      TIntStopReason sr;
      double area = MtxIntDif.Romberg(IntFun,0.0,5*System.Math.PI,out sr, 1.0e-4, 100);
    }
    </code></example>

    <seealso cref="QuadGauss"/>*)
  function Romberg(Fun: TRealFunction; lb, ub: double; const FloatPrecision: TMtxFloatPrecision; Const Constants: TVec; Const ObjConst: Array of TObject; out StopReason: TIntStopReason; Tolerance: double = 1.0e-4; MaxIter:Integer = 100):double;

  (*<summary>Numerical integration by using optimized quadrature formula.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>

    <seealso cref="QuadGauss"/>*)
  function QuadRegular(Fun: TRealFunction; lb, ub: double; const FloatPrecision: TMtxFloatPrecision; Const Constants: TVec; Const ObjConst: Array of TObject;
    X0, F0: TVec; out StopReason: TIntStopReason; Tolerance: double = 1.0E-4; MaxIter:Integer = 8; MaxNInsert: Integer = 7): double;

  (*<summary>Numerical integration by Monte Carlo method.</summary>
    <returns>the numerical approximate on integral of function Fun between limits lb and ub.</returns>
    <param name="Fun">Integrating function.</param>
    <param name="Constants">Additional constants defining Fun function, usually nil/null.</param>
    <param name="ObjConst">Additional objects defining Fun function, usually nil/null.</param>
    <param name="lb">Defines lower bound.</param>
    <param name="ub">Defines pper bound.</param>
    <param name="N">Number of random points in [lb,ub] interval (see comments above).</param>
    <param name="FloatPrecision">Defines the computational precision to be used by the routine.</param>

    
<remarks>Performs a numerical integration of function of single variable by using Monte Carlo method.
</remarks>


    

    <example>
      Evaluate fuction <c>Sin(x)</c> on interval <c>[0,PI]</c> by using Monte Carlo algorithm.

    <code>
    private double IntFun(TVec x, TVec c, params object[] o)
    {
      double x = x[0];
      return System.Math.Sin(x);
    }
    private void Example()
    {
      TIntStopReason sr;
      double area = MtxIntDif.MonteCarlo(IntFun,0.0,System.Math.PI,null,null,65536);
    }
    </code></example>

    <seealso cref="QuadGauss"/>*)
  function MonteCarlo(Fun: TRealFunction; lb, ub: double; const FloatPrecision: TMtxFloatPrecision; Const Constants: TVec; Const ObjConst: Array of TObject; N: Integer = 65536): double;




