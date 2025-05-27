







                     



(*<summary>Introduces routines for several linearizable regression models.</summary>
  
<remarks>Introduces several ready-to-be-used linearizable regression models:
  <list type="bullet">
  <item> Power model : <IMG name="regmodels001"/>
  <item> simple linear model : <IMG name="regmodels002"/>
  <item> simple exponential model : <IMG name="regmodels003"/>
  <item> multiple linear model : <IMG name="regmodels004"/>
  <item> Rational function : <IMG name="regmodels005"/>
  <item> Generalized logistic function : <IMG name="regmodels006"/>
  <item> Natural logarithm model: <IMG name="regmodels007"/>
  </list>

  If your data cannot be described with linearizable equation, you should use the
  <see cref="TMtxNonLinReg"/> component or <see cref="NLinRegress"/> routine to
  retrieve regression coeeficients from fitted equation.
</remarks>
*)
unit RegModels;


{$I bdsppdefs.inc}

interface

uses MtxVec, Math387, Regress
  
  ,SysUtils
  ,Types
  
  ;

(*<summary>Defines different linear models.</summary>*)
Type TRegressionModel = (
  (*<summary>Linear function <c>Y = B[0]+B[1]*X</c>.</summary>*)rmLine,
  (*<summary>Power function <c>Y = (B[0]*X)^B[1]</c>.</summary>*)rmPower,
  (*<summary>Polynomial function.</summary>*)rmPoly,
  (*<summary>Simple exponential function <c>Y = B[0]*Exp(B[1]*X)</c>.</summary>*)rmExp,
  (*<summary>Multiple linear function <c>Y = B[0] + B[1]*X1 + B[2]*X2 + ...</c></summary>*)rmMulLi,
  (*<summary>Logistic function : <c>Y = B[0] + (B[1]-B[0])/(1+Exp(-B[2]*x + B[3]))</c></summary>*)rmLogistic,
  (*<summary>Rational function : <c>Y = (B[0] + B[1].x + ... + B[n]x^n)/(1 + B[n+1].x + B[n+2].x^2 + ... B[n+d]*x^d)</c></summary>*)rmFrac,
  (*<summary>Natural logarithm function : <c>y = B[0] + B[1]*Ln(X).</c></summary>*)rmLn
  );



(*<summary>Evaluates power function.</summary>
  <returns>b[0]*X^b[1] for given value X and parameters in B array.</returns>

  

  <example>
    Evaluate power function for single x.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      y = RegModels.PowerEval(new double[] {-5,3.5},2.0);
    }
  }
  </code></example>*)
function PowerEval(Const B: Array of double; X: double): double; overload;
(*<summary>Evaluates <c>b[0]*X^b[1]</c> for given values in X vector, parameters in B array,
  and returns the results in XHat vector.</summary>
  
<remarks>Size and Complex properties of XHat vector are adjusted automatically.

  Note
    Use this version if you want to evaluate power function for multiple values
    at the same time. This is a lot faster than calling single value version for each x value.
</remarks>


  

  <example>
    Evaluate power function for multiple values at the same time.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector X = new Vector(100,false);
      Vector Y = new Vector(0);
      X.Ramp(-5.0, 0.1);
      RegModels.PowerEval(new double[] {1.0, 3.0}, X, Y);
    }
  }
  </code></example>

  <SeeAlso cref="PowerDeriv"/>
  <SeeAlso cref="PowerFit"/>*)
procedure PowerEval(Const B: Array of double; const X, YHat: TVec); overload;
(*<summary>Evaluate power function model (vectorized version).</summary>*)
procedure PowerEval(const B, X, YHat: TVec); overload;
(*<summary>Derivatives of power function.</summary>
  
<remarks>Calculates the derivatives of the power function at point (X,Y)
  with respect to the regression parameters Pars. The results are returned in
  Grad vector. Grad.Values[I] contains the derivative with respect to the I-th
  parameter. The Length and Complex properties of the Grad vector are adjusted
  automatically.
</remarks>


  <SeeAlso cref="PowerEval"/>
  <SeeAlso cref="PowerFit"/>*)
procedure PowerDeriv(RegressFun: TRegressFun; X,Y : double; Const Pars: Array of double; const Grad: TVec);
(*<summary>Fits simple exponential equation to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for power function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels001"/><para/>
</remarks>

  

  <example>
    In the following example we generate some data. Then we fit
    power function to this data and retreive it's regression coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Point series1,
      Steema.TeeChart.Styles.Point series 2)
    {
      Vector X = new Vector(100,false);
      Vector Y = new Vector(0);
      Vector YHat = new Vector(0);
      Vector B = new Vector(0);

      X.Ramp(-5.0, 0.1); // x= -5.0, -4.9, ..., +4.9
      Y.Size(X);
      Y.RandGauss(3.5,0.1); // sample data
      RegModels.PowerFit(B,X,Y,null); // calculate coefficients
      // evaluate y
      RegModels.PowerEval(B, X, YHat);
      MtxVecTee.DrawValues(X,Y,series1,false); // original data
      MtxVecTee.DrawValueS(X,YHat,series2,false); // fitted data
    }
  }
  </code></example>

  <SeeAlso cref="PowerDeriv"/>
  <SeeAlso cref="PowerEval"/>*)
procedure PowerFit(const B: TVec; const X,Y: TVec; const Weights: TVec = nil);

(*<summary>Evaluates linear function.</summary>
  <returns><c>b[0] + b[1]*X</c> for given value X and parameters in B array.</returns>

  

  <example>
    Evaluate linear function for single x.
  <code>
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      RegModels.LineEval(new double[] {1.0, 3.0}, 2.5);
    }
  }
  </code></example>*)
function LineEval(Const B: Array of double; X: double): double; overload;
(*<summary>Evaluates <c>b[0] + b[1]*X</c> for given values in X vector, parameters in B array,
  and returns the results in XHat vector.</summary>
  
<remarks>Size and Complex properties of XHat vector are adjusted automatically.

  Note
    Use this version if you want to evaluate linear function for multiple values
    at the same time. This is a lot faster than calling single value version for each x value.
</remarks>


  

  <example>
    Evaluate linear function for multiple values at the same time.<para/>
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector X = new Vector(100,false);
      Vector Y = new Vector(0);
      X.Ramp(-5.0, 0.1);
      RegModels.LineEval(new double[] {1.0, 3.0}, X, Y);
    }
  }
  </code></example>

  <SeeAlso cref="LineDeriv"/>
  <SeeAlso cref="LineFit"/>*)
procedure LineEval(Const B: Array of double; const X, YHat: TVec); overload;
(*<summary>Evaluates linear function (fully vectorized version).</summary>*)
procedure LineEval(const B, X, YHat: TVec); overload;
(*<summary>Derivatives of linear function.</summary>
  
<remarks>Calculates the derivatives of linear function at point (X,Y)
  with respect to the regression parameters Pars. The results are returned in
  Grad vector. Grad.Values[I] contains the derivative with respect to the I-th
  parameter. The Length and Complex properties of the Grad vector are adjusted
  automatically.
</remarks>


  <SeeAlso cref="LineEval"/>
  <SeeAlso cref="LineFit"/>*)
procedure LineDeriv(RegressFun: TRegressFun; X,Y : double; Const Pars: Array of double; const Grad: TVec; const fp: TMtxFloatPrecision);
(*<summary>Fits linear equation to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for linear function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels002"/><para/>
</remarks>

  

  <example>
    In the following example we generate some data. Then we fit
    simple linear function to this data and retreive it's regression
    coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line line1,
      Steema.TeeChart.Styles.Line line2)
    {
      Vector X = new Vector(100,false);
      Vector Y = new Vector(100,false);
      Vector YHat = new Vector(0);
      Vector B = new Vector(0);

      X.Ramp(-5.0, 0.1); // x= -5, -4.95, ...-0.05
      Y.RandGauss(3.5, 0.12); // sample data
      RegModels.LineFit(B,X,Y,null); // calculate coefficients
      // evaluate y by using calculated coefficients
      LineEval(B,X, YHat);
      MtxVecTee.DrawValues(X,Y,line1,false); // draw original data
      MtxVecTee.DrawValues(X,YHat,line2,false); // draw fitted data
    }
  }
  </code></example>

  <SeeAlso cref="LineDeriv"/>
  <SeeAlso cref="LineEval"/>*)
procedure LineFit(const B: TVec; const X,Y: TVec; const Weights: TVec = nil);

(*<summary>Evaluates exponential function.</summary>
  <returns><c>b[0]*Exp(b[1]*X)</c> for given value X and parameters in B array.</returns>

  

  <example>
    Evaluate exponential function for single x.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      double y = RegModels.ExpEval(new double[] {3.0, -0.5}, 1.5);
      // y = 3*Exp(-0.5*1.5) = 1.4170996582
    }
  }
  </code></example>*)
function ExpEval(Const B: Array of double; X: double): double; overload;
(*<summary>Evaluates <c>b[0]*Exp(b[1]*X)</c> for given values in X vector, parameters in B array,
  and returns the results in XHat vector.</summary>
  
<remarks>Size and Complex properties of XHat vector are adjusted automatically.

  Note
    Use this version if you want to evaluate exponential function for multiple values
    at the same time. This is a lot faster than calling single value version for each x value.
</remarks>


  

  <example>
    Evaluate exponential function for multiple values at the same time.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line line1)
    {
      Vector X = new Vector(100);
      Vector Y = new Vector(0);
      X.Ramp(-5.0, 0.05); // x= -5.0, -4.95, ... -0.05
      // Y = b[0] + b[1]*X
      RegModels.ExpEval(new double[] {1,3},X,Y);
      MtxVecTee.DrawValues(X,Y,line1,false);
    }
  }
  </code></example>

  <SeeAlso cref="ExpDeriv"/>
  <SeeAlso cref="ExpFit"/>*)
procedure ExpEval(Const B: Array of double; const X, YHat: TVec) ; overload;
(*<summary>Evaluates exponential function (fully vecctorized version).</summary>*)
procedure ExpEval(const B, X, YHat: TVec); overload;

(*<summary>Derivatives of simple exponential function.</summary>
  
<remarks>Calculates the derivatives of the simple exponential function at point (X,Y)
  with respect to the regression parameters Pars. The results are returned in
  Grad vector. Grad.Values[I] contains the derivative with respect to the I-th
  parameter. The Length and Complex properties of the Grad vector are adjusted
  automatically.
</remarks>


  <SeeAlso cref="ExpEval"/>
  <SeeAlso cref="ExpFit"/>*)
procedure ExpDeriv(RegressFun: TRegressFun; X,Y : double; Const Pars: Array of double; const Grad: TVec; const fp: TMtxFloatPrecision);

(*<summary>Fits simple exponential equation to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for simple exponent function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels003"/><para/>
</remarks>

  

  <example>
    In the following example we generate some data. Then we fit
    simple exponential function to this data and retreive it's regression coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line line1,
      Steema.TeeChart.Styles.Line line2)
    {
      Vector X = new Vector(100);
      Vector Y = new Vector(100);
      Vector B = new Vector(0);
      Vector YHat = new Vector(0);

      X.Ramp(-5.0, 0.05); // x= -5.0, -4.95, ... -0.05
      Y.RandGauss(3.5, 0.12); // sample data
      // calculate coefficients
      RegModels.ExpFit(B,X,Y,null);
      // evaluate y by using calculated coefficients
      RegModels.ExpEval(B,X, YHat);
      MtxVecTee.DrawValues(X,Y,line1,false);
      MtxVecTee.DrawValues(X,YHat,line2,false);
    }
  }
  </code></example>

  <SeeAlso cref="ExpDeriv"/>
  <SeeAlso cref="ExpEval"/>*)
procedure ExpFit(const B: TVec; const X,Y: TVec; const Weights: TVec = nil);

(*<summary>Evaluates multiple linear function.</summary>
  <returns><c>b[0] + b[1]*x[0] + b[2]*x[1] + ...</c> for given value X and parameters in B array.</returns>*)
function MulLinEval(Const B: Array of double; const X: TVec; Constant: boolean = false): double; overload;
(*<summary>Evaluates <c>b[0] + b[1]*x[0] + b[2]*x[1] + ...</c> for given values in X vector, parameters in B array,
  and returns the results in XHat vector.</summary>
  
<remarks>Size and Complex properties of XHat vector are adjusted automatically.

  Note
    Use this version if you want to evaluate multiple linear function for multiple values
    at the same time. This is a lot faster than calling single value version for each x value.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix X = new Matrix(0,0);
      Vector YHat = new Vector(0);
      X.SetIt(3,3,false,new double[]{2, -3, 5,
                       1, 6, -4,
                       8, 7, 9});
      // Evaluate, no constant term!
      RegModels.MulLinEval(new double[] {1.0, 0.5, -2.0},X,YHat, false);
      // YHat = (-9.5, 12, -6.5)
    }
  }
  </code></example>

  <SeeAlso cref="MulLinFit"/>*)
procedure MulLinEval(Const B: Array of double; const X: TMtx; const YHat: TVec; Constant: boolean = false); overload;
(*<summary>Fits multiple linear equations to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Constant">If true then intercept term b(0) will be included in calculations. If false, set intercept term b(0) to 0.0.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for multiple linear function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels004"/><para/>
  where X is matrix, Y, B are vectors.
</remarks>


  

  <example>
    In the following example we generate some data. Then we fit
    multiple linear function to this data and retreive it's regression coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix X = new Matrix(0,0);
      Vector Y = new Vector(0);
      Vector B = new Vector(0);
      X.SetIt(3,2,false,new double[]{1.0, 2.0,
                      -3.2, 2.5,
                      8.0, -0.5});
      Y.SetIt(3,new double[] {-3.0, 0.25, 9.0});
      RegModels.MulLinFit(B,X,Y,true,null);
      // B = (18.646428571, -1.9464285714, -9.85 )
    }
  }
  </code></example>

  <SeeAlso cref="MulLinEval"/>*)
procedure MulLinFit(const B: TVec; const X: TMtx; const Y: TVec; Constant: boolean = false; const Weights: TVec = nil);

(*<summary>Evaluates rational fraction model.</summary>*)
function FracEval(Const B: array of double; X: double; DegNom: Integer; Constant: boolean = false): double; overload;
function FracEval(Const B: TVec; X: double; DegNom: Integer; Constant: boolean): double; overload;
(*<summary>Evaluates raational fraction model (parameters are not vectorized).</summary>
  
<remarks>In this case only the independent and dependent variables are vectorized.
</remarks>
*)
procedure FracEval(Const B: Array of double; const X: TVec; const YHat: TVec; DegNom: Integer; Constant: boolean = false); overload;
(*<summary>Evaluate rational fraction (fully vectorized version).</summary>
  
<remarks>In this case all parameter are vectors.
</remarks>

  <SeeAlso cref="FracFit"/>*)
procedure FracEval(const B, X, YHat: TVec; DegNom: Integer; Constant: boolean = false); overload;
(*<summary>Fits rational fraction equation to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="DegNom">Nominator degree.</param>
  <param name="DegDenom">Denominator degree.</param>
  <param name="Constant">If false, B[0] i.e. constant term in nominator is set to 0.0.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for rational function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels005"/><para/>
  where n and d are nominator and denominator polynomial degrees.
</remarks>


  

  <example>
    In the following example we generate some data. Then we fit
    power function to this data and retreive it's regression coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line line1, Steema.TeeChart.Styles.Line line2)
    {
      Vector X = new Vector(100);
      Vector Y = new Vector(100);
      Vector B = new Vector(0);
      Vector YHat = new Vector(0);

      X.Ramp(-5.0, 0.05); // x= -5.0, -4.95, ... -0.05
      Y.RandGauss(3.5, 0.12); // sample data
      // calculate coefficients
      RegModels.FracFit(B,X,Y,2,4,false,null);
      // evaluate y by using calculated coefficients
      RegModels.FracEval(B,X, YHat,2,false);
      MtxVecTee.DrawValues(X,Y,line1,false);
      MtxVecTee.DrawValues(X,YHat,line2,false);
    }
  }
  </code></example>

  <SeeAlso cref="FracEval"/>*)
procedure FracFit(const B: TVec; const X,Y: TVec; DegNom,DegDenom: Integer; Constant: boolean = false; const Weights: TVec = nil);

(*<summary>Evaluate logistic model.</summary>
  
<remarks>The observed values obey the following equation:

  <IMG name="regmodels006"/><para/>
</remarks>
*)
function LogisticEval(Const B: Array of double; X: double; Constant: boolean): double; overload;
(*<summary>Evaluate logistic model (partially vectorized).</summary>*)
procedure LogisticEval(Const B: Array of double; const X,YHat: TVec; Constant: boolean); overload;
(*<summary>Evaluate logistic function (fully vectorized).</summary>
  <SeeAlso cref="LogisticFit"/>*)
procedure LogisticEval(const B,X,YHat: TVec; Constant: boolean); overload;
(*<summary>Fits logistic equation to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Constant">If false, B[0] i.e. constant term in nominator is set to 0.0.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for logistic function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels006"/><para/>
</remarks>


  <SeeAlso cref="LogisticEval"/>*)
procedure LogisticFit(const B: TVec; const X,Y: TVec; Constant: boolean = false; const Weights: TVec = nil);

(*<summary>Evaluate the logarithm <c>y(x)=b[0] + b[1]*ln(x)</c> model.</summary>
  
<remarks>The observed values obey the following equation:

  <IMG name="regmodels007"/><para/>
</remarks>
*)
function LnEval(Const B: Array of double; X: double): double; overload;
(*<summary>Evaluate the logarithm model (partially vectorized).</summary>*)
procedure LnEval(Const B: Array of double; const X, YHat: TVec); overload;
(*<summary>Evaluate logarithm function (fully vectorized).</summary>
  <SeeAlso cref="LnFit"/>*)
procedure LnEval(const B, X,YHat: TVec); overload;
(*<summary>Fits simple logarithm equation <c>y(x)=b[0] + b[1]*ln(x)</c> to data.</summary>
  <param name="X">Vector of independent variable.</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="Weights">Weights (optional). Weights are used only if they are set.</param>
  <param name="B">Returns regression coefficients for natural logarithm function.</param>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals.
  The observed values obey the following equation:

  <IMG name="regmodels007"/><para/>
</remarks>

  

  <example>
    In the following example we generate some data. Then we fit
    natural logarithm function to this data and retreive it's regression coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line line1,
      Steema.TeeChart.Styles.Line line2)
    {
      Vector X = new Vector(100);
      Vector Y = new Vector(100);
      Vector B = new Vector(0);
      Vector YHat = new Vector(0);

      X.Ramp(0.5, 0.05); // x= 0.5, 0.55, ...
      Y.RandGauss(3.5, 0.12); // sample data
      // calculate coefficients
      RegModels.LnFit(B,X,Y,null);
      // evaluate y by using calculated coefficients
      RegModels.LnEval(B,X, YHat);
      MtxVecTee.DrawValues(X,Y,line1,false);
      MtxVecTee.DrawValues(X,YHat,line2,false);
    }
  }
  </code></example>

  <SeeAlso cref="LnEval"/>*)
procedure LnFit(const B: TVec; const X,Y: TVec; const Weights: TVec = nil);



