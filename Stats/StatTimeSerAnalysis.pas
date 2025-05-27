











(*<summary>Time series analysis.</summary>
  
<remarks>Introduces several routines for handling/analyzing univariante time series. Includes ARMA, ARIMA and
  exponential smoothing routines.<para/>

  As stated at NIST pages, time series is an ordered sequence of values of a variable at
  equally spaced time intervals. The usage of time series models is twofold:
  <list type="bullet">
  <item> Obtain an understanding of the underlying forces and structure that produced the observed data.</item>
  <item> Fit a model and proceed to forecasting, monitoring or even feedback and feedforward control.</item>
  </list>
  <para/>
  The fitting of time series models can be an ambitious undertaking. This unit utilizes the following:
  <list type="bullet">
  <item> average smoothing, </item>
  <item> Holt-Winters single, double and triple exponential smoothing,</item>
  <item> ARMA and ARIMA models,</item>
  <item> ARAR model.</item>
  </list>
  <para/>
  <u>Literature used</u>
  <list type="number">
  <item> Brockwell, P.J. and Davis, R.A. : <i>Introduction to Time Series and Forecasting - second edition</i>, Springer Verlag, New York, 2002.</item>
  <item> Brockwell, P.J. and Davis, R.A. : <i>Time Series: Theory and Methods - second edition</i>, Springer Verlag, New York, 1991.</item>
  <item> Shumway, R.H. and Stoffer, D.S. : <i>Time Series Analysis and Its Applications</i>, Springer Verlag, New York, 2000.</item>
  <item> <see href="http://www.stat.unc.edu/faculty/hurd/progs">www.stat.unc.edu/faculty/hurd/progs</see></item>
  <item> <see href="http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc43.htm">eww.itl.nist.gov/div898/handbook/pmc/section4/pmc43.htm</see>.</item>
  <item> <see href="http://www.it.iitb.ac.in/~praj/acads/seminar/04329008_ExponentialSmoothing.pdf">www.it.iitb.ac.in/~praj/acads/seminar/04329008_ExponentialSmoothing.pdf</see>.</item>
  </list>
</remarks>
*)
unit StatTimeSerAnalysis;


{$I BdsppDefs.inc}

interface

uses Probabilities, MtxVec, AbstractMtxVec, Math387, Statistics, Optimization
     
     ,Types  
     
     ;

(*<summary>ARMA/ARIMA coefficients initial estimate method.</summary>*)
type TcfInitMethod=(
 (*<summary>User suplied values for phi and theta.</summary>*)cfInitFixed,
 (*<summary>Yule-Walker estimates for pure AR(p) process.</summary>*)cfInitYW,
 (*<summary>Burg estimates for pure AR(p) process.</summary>*)cfInitBurg,
 (*<summary>Innovations estimates for ARMA(p,q) process.</summary>*)cfInitInno,
 (*<summary>Hannah-Rissanen estimates for ARMA(p,q) process.</summary>*)cfInitHannah
 );




(*<summary>Autocovariance function.</summary>
  <param name="Src">Sample data set.</param>
  <param name="Dst">Stores sample autocorrelation (ACF) function. Size of Dst is adjusted automatically.</param>
  <param name="Lags">Number of lags to calculate. If Lags is -1 the number of lags is computed as<para/>
  <c>Lags := Ceil(10*Log10(Src.Length))</c></param>

  <SeeAlso cref="ACF"/>*)
procedure AutoCov(const Src, Dst: TVec; Lags: integer = -1);

(*<summary>Autocorrelation/autocovariance function.</summary>
  <param name="Src">Sample data set.</param>
  <param name="Dst">Stores sample autocorrelation (ACF) function. Size of Dst is adjusted automatically.</param>
  <param name="Lags">Number of lags to calculate. If Lags is -1 the number of lags is computed as<para/>
  <c>Lags := Ceil(10*Log10(Src.Length))</c></param>

  
<remarks>Calculates sample autocorrelation function at lags Lags.
</remarks>


  <SeeAlso cref="AutoCov"/>*)
procedure ACF(const Src,Dst: TVec; Lags: integer = -1);

(*<summary>Partial autocorrelation function.</summary>
  <param name="Acf">Autocorrelation function.</param>
  <param name="Arp">autoregressive model parameters.</param>
  <param name="ParCorr">partial autocorrelation (-reflection coefficients)</param>
  <param name="Variance">Error variance.</param>

  
<remarks>Calculates sample partial autocorrelation function (PACF).
</remarks>
*)
procedure PACF(const Acf, Arp, ParCorr, Variance: TVec); overload;
(*<summary>Partial autocorelation function.</summary>
  <param name="Src">Stores the sample autocorrelation function (ACF).</param>
  <param name="Dst">Returns the sample partial autocorrelation function (PACF) on output.</param>

  <SeeAlso cref="ACF"/>*)
procedure PACF(const Src, Dst: TVec); overload;

(*<summary>Setup initial values for integrating ARMA series.</summary>
  
<remarks>Setup initial values for integrating ARMA series.
</remarks>


  <SeeAlso cref="ARMAForecast"/>
  <SeeAlso cref="TVec.Integrate"/>*)
procedure TimeSeriesIntInit(const Data: TVec; const Init: TVec; const d: Integer; const Future: boolean = true);

(*<summary>Estimates autocorrelation/autocovariance function for the ARMA model.</summary>
  <param name="Phi">Stores Phi values for ARMA process, without the initial 1.</param>
  <param name="Theta">Stores Theta values for ARMA process, without the initial 1.</param>
  <param name="n">Number of lags to calculate.</param>
  <param name="Normalize">If true, ACF values are normalized by ACF[0]. If false, no normalization is performed and the ResultACF
    stores ACVF <c>(gamma[0], gamma[1], ...gamma[n])</c> values.</param>
  <param name="ResultACF">Returns autocovarialce <c>(gamma[0], gamma[1], ...)</c> or
    autocorrelation <c>(rho[0], rho[1], ...)</c> function for the ARMA model..</param>

  
<remarks>Estimates autocorrelation/autocovariance function for the ARMA model.
</remarks>


  <SeeAlso cref="ARMAKappa"/>*)
procedure ARMAAcf(const Phi,Theta: TVec; const n: integer; const ResultACF: TVec; const Normalize: boolean = True);

(*<summary>ARMA process covariances.</summary>
  <param name="gamma">Time series ACVF.</param>
  <param name="maacvf">The ACVF of a MA part of the model.</param>
  <param name="Phi">Stores Phi values for ARMA process.</param>
  <param name="Theta">Stores Theta values for ARMA process.</param>
  <param name="i"></param>
  <param name="j"></param>

  
<remarks>Calculates ARMA (p,q) process covariances. For ARMA process, covariances are defined as:

  <IMG name="timeser017"/><para/>
  where gamma is time series autocovariance function, <c>sigma^2</c> is estimated white noise, m=max(p,q) and phi, theta are
  AR and MA coefficients.
</remarks>


  <SeeAlso cref="ARMAAcf"/>*)
function ARMAKappa(const gamma, maacvf: TVec; const i,j: Integer; const Phi, Theta: TVec): double; overload;
(*<summary>Calculate necessary covariances for ARMA(p,q) process up to kappa(KappaSize,KappaSize)</summary>*)
procedure ARMAKappa(const Data: TVec; const Phi, Theta: TVec; const Cov: TMtx; const KappaSize: Integer); overload;

(*<summary>Simulate the ARMA (p,q) process.</summary>
  <param name="p">stores the AR coefficients. Length of the p vector defines AR(p) order.</param>
  <param name="t">stores the MA coefficients. Length of the t vector defines MA(q) order.</param>
  <param name="n">defines number of points to simulate.</param>
  <param name="aResult"> returns ARMA (p,q) time series. Size of Result vector is adjusted automatiacally.</param>

  <Example>Simulate ARMA(1,1) process with Phi=[1.0], Theta=[-0.25].
  <code lang="Delphi">
  Uses MtxExpr, StatTimeSerAnalysis;
  procedure Example;
    var phi,theta,ts: Vector;
  begin
    phi.SetIt(false,[1.0]);
    theta.SetIt(false,[-0.25]);
    ARMASimulate(phi,theta,100,ts);
    // ts now stores 100 points from ARMA(1,1) process.
  end;
  </code>
  <code lang="C++">
  #include "MtxExpr.hpp"
  #include "Math387.hpp"
  #include "StatTimeSerAnalysis.hpp"
  void __fastcall Example();
  {
    sVector phi,theta,ts;
    phi.SetIt(false,OPENARRAY(double,(1.0)));
    theta.SetIt(false,OPENARRAY(double,(-0.25)));
    ARMASimulate(phi,theta,100,ts);
    // ts now stores 100 points from ARMA(1,1) process.
  </code>
  </Example>

  
<remarks>C# Example
    Simulate ARMA(1,1) process with Phi=[1.0], Theta=[-0.25].
  <code lang="C#">
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector phi = new Vector(0);
      Vector theta = new Vector(0);
      Vector ts = new Vector(0);
      phi.SetIt(false, new double[] {1.0});
      theta.SetIt(false,new double[] {-0.25});
      StatTimeSerAnalysis.ARMASimulate(phi,theta,100,ts);
      // ts now stores 100 points from ARMA(1,1) process.
    }
  }
  </code>
</remarks>


  <SeeAlso cref="ARIMASimulate"/>*)
procedure ARMASimulate(const p,t: TVec; const n: integer; const aResult: TVec); overload;

(*<summary>Simulate the ARIMA process.</summary>
  <param name="p">stores the AR coefficients. Length of the p vector defines AR(p) order.</param>
  <param name="t">stores the MA coefficients. Length of the t vector defines MA(q) order.</param>
  <param name="d">defines how many times time series is differentiated (d parameter in ARIMA).</param>
  <param name="ResInit">defines initial values for integration: r[-d+1],Dr[-d+2],...,D^(d-1)r[0]. The length of ResInit must be
    equal to d, otherwise an exception will be raised.</param>
  <param name="n">defines number of points to simulate.</param>
  <param name="aResult">returns ARIMA (p,d,q) time series. Size of Result vector is adjusted automatiacally.</param>

  
<remarks>Simulate the ARIMA (p,d,q) process.
</remarks>


  

  <example>
    Simulate ARIMA(1,2,1) process with Phi=[1.0], Theta=[-0.25], d=2.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector phi = new Vector(0);
      Vector theta = new Vector(0);
      Vector init = new Vector(0);
      Vector ts = new Vector(0);
      phi.SetIt(false, new double[] {1.0});
      theta.SetIt(false,new double[] {-0.25});
      theta.SetIt(false,new double[] {0.0});
      StatTimeSerAnalysis.ARIMASimulate(phi,theta,2,init,100,ts);
      // ts now stores 100 points from ARIMA(1,1,2) process.
    }
  }
  </code></example>

  <SeeAlso cref="ARMASimulate"/>*)
procedure ARIMASimulate(const p,t: TVec; const d: Integer; const ResInit: TVec; const n: integer; const aResult: TVec);














(*<summary>ARMA model one-step ahead predictors.</summary>
  
<remarks>Calculate the ARMA (p,q) model one-step ahead predictors.
</remarks>


  <SeeAlso cref="ARMAForecast"/>*)
procedure ARMAPredictors(const Data: TVec; const Phi,Theta: TVec; const Predictors: TVec; const r: TVec);

(*<summary>Forecast time series by using ARMA(p,q) model.</summary>
  <param name="Data">The original time series data set.</param>
  <param name="P">ARIMA Phi (AR) coefficients. Assumes P (AR model) to be without the leading 1.0.</param>
  <param name="T">ARIMA Theta (MA) coefficients. </param>
  <param name="Residuals">Residuals as returned by ARMAMLE.</param>
  <param name="n">Number of samples to forecast.</param>
  <param name="mu">This modified average value is included in to the optimization process of ARMAMLE, which returns optimal value for it.  </param>
  <param name="Forecast">Results of the forecasting (beyond the last index value of Data starting at Data.Length).</param>
  <param name="fStdDev">Returns standard deviation of residuals.</param>

  <SeeAlso cref="ARMAForecast"/>
  <SeeAlso cref="ARMAPredictors"/>*)

procedure ARMAForecast(const Data, P, T, Residuals: TVec; const n: integer; const mu: double; const Forecast, fStdDev: TVec); overload;

(*<summary>Estimate ARMA process AR and MA coefficients.</summary>
  <param name="Data">Time series data set.</param>
  <param name="P">ARIMA Before call stores initial estimates for ARIMA Phi coefficients. After call returns MLE estimates for Phi coefficients without leading 1.0.</param>
  <param name="T">ARIMA Before call stores initial estimates for ARIMA Theta coefficients. After call returns MLE estimates for Theta coefficients.</param>
  <param name="Residuals">Returns residuals between predicted (MLE) and actual time series values.</param>
  <param name="MLE">Returns -2 log likelihood of ARMA model.</param>
  <param name="mu">Returns the estimated modified series average value (constant). </param>
  <returns>Number of evaluations needed to converge to MLE solution.</returns>

  
<remarks>Estimate ARMA(p,t) process coefficients by using MLE.
</remarks>


  <SeeAlso cref="ARMAHannahFit"/>
  <SeeAlso cref="ARMAInnovationsFit"/>*)
function ARMAMLE(const Data: TVec; const P, T: TVec; const Residuals: TVec; out MLE: double; out mu: double): Integer; overload;



(*<summary>Check AR(MA) coeefficients.</summary>
  <param name="Pars">Phi (AR) or Theta (MA) coefficients.</param>
  <param name="Causality">If true, Params are checked for causality. If false, Params are checked for invertibility.</param>
  <returns>true, if coefficients are causal/invertible.</returns>

  
<remarks>Check if AR coefficients are causal or MA coefficients are invertible.
</remarks>
*)
function CheckARMACoeffs(const Pars: Array of double; const Causality: boolean = true): boolean; overload;
function CheckARMACoeffs(const Coeffs: TVec; const Causality: boolean = true): boolean; overload;

(*<summary>-2log likelihood.</summary>
  <param name="Data">Input date.</param>
  <param name="Trend">Optional trend line. Can be nil, if constant (average value) is assumed. </param>
  <param name="Phi">stores phi[0]..phi[p-1] coefficients. The order of AR(p) is defined by Phi vector length.</param>
  <param name="Theta">stores theta[0]..theta[q-1] coefficients. The order of AR(p) is defined by Phi vector length.</param>
  <param name="Residuals">stores the "errors" left after the fitting process.</param>
  <returns>-2log likelihood for ARIMA(p,q,d) process.</returns>

  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector phi = new Vector(0);
      Vector theta = new Vector(0);
      Vector ts = new Vector(0);
      phi.SetIt(false, new double[] {0.33,-0.24});
      theta.SetIt(false,new double[] {0.9});
      // ARMA(2,1,2) process -> evaluate -2log likelihood
      double l = StatTimeSerAnalysis.ARMALogLike(ts,phi,theta);
    }
  }
  </code></example>*)
function ARMALogLike(const Data, Trend, Phi, Theta, Residuals: TVec): double;

(*<summary>The Durbin-Levinson algorithm.</summary>
  <param name="gamma">Defines covariances for Durbin-Levinson algorithm.</param>
  <param name="Phi">Returns phi[n,1]...phi[n,n] coefficients.</param>
  <param name="NumEvals">Defines number of iterations of the Durbin-Levinson algorithm.</param>
  <param name="Sigma2">Returns estimated variance.</param>
  <param name="PhiVar">If not nil, it returns phi coefficients variances.</param>

  
<remarks>Uses the Durbin-Levinson algorithm to calculate <c>phi[n,1]...phi[n,n]</c> coefficients. Coefficients are
  calculated resursively from the following relations:

  <IMG name="timeser016"/><para/>
  where <c>phi(1,1) = gamma(1)/gamma(0) and v(0)=gamma(0)</c>.
</remarks>


  <SeeAlso cref="Innovations"/>*)
procedure DurbinLevinson(const gamma: TVec; const Phi: TVec; out Sigma2: double; const NumEvals: Integer; const PhiVar: TVec=nil);

(*<summary>Yule-Walker AR estimation.</summary>
  <param name="Data">Time series.</param>
  <param name="Phi">Returns estimates for Phi coefficients. AR(p) order is determined by Phi length.</param>
  <param name="Sigma2">Returns estimate for Sigma^2 i.e. (AR) model variance.</param>
  <param name="StdErrs">If not nil, it returns estimated phi coefficients standard errors.</param>

  
<remarks>Performs Yule-Walker estimation for pure (AR) model.
</remarks>


  

  <example>
    Calculate initial estimates for AR(3) process by using Yule-Walker algorithm.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector ts = new Vector(0);
      Vector phi = new Vector(0);
      double s2;
      ts.LoadFromFile("timeser.vec");
      phi.Length = 3; // for AR(3) process
      StatTimeSerAnalysis.ARYuleWalkerFit(ts,phi,out s2,null);
    }
  }
  </code></example>

  <SeeAlso cref="ARBurgFit"/>*)
procedure ARYuleWalkerFit(const Data: TVec; const Phi: TVec; out Sigma2: double; const StdErrs: TVec = nil);

(*<summary>Burg AR estimation.</summary>
  <param name="Data">Zero-mean time series. If this is not the case, subtract the mean from data.</param>
  <param name="Phi">Returns estimates for Phi coefficients. AR(p) order is determined by Phi length.</param>
  <param name="Sigma2">Returns Burg estimated variance for AR process.</param>
  <param name="StdErrs">Returns estimated phi coefficients standard errors.</param>

  
<remarks>Performs Burg estimation for pure (AR) model.
</remarks>


  

  <example>
    Calculate initial estimates for AR(3) process by using Burg's algorithm.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector ts = new Vector(0);
      Vector phi = new Vector(0);
      Vector stdErr = new Vector(0);
      double s2;
      ts.LoadFromFile("timeser.vec");
      phi.Length = 3; // for AR(3) process
      StatTimeSerAnalysis.ARBurgFit(ts,phi,out s2,stdErr);
    }
  }
  </code></example>

  <SeeAlso cref="ARYuleWalkerFit"/>*)
procedure ARBurgFit(const Data: TVec; const Phi: TVec; out Sigma2: double; const StdErrs: TVec);

(*<summary>The innovations algorithm.</summary>
  <param name="kappa">Defines covariances for innovations algorithm.</param>
  <param name="Theta">Returns <c>Theta[n,1]...Theta[n,n]</c> coefficients.</param>
  <param name="Sigma2">Returns variance.</param>
  <param name="NumEvals">Defines number of iterations of the innovation algorithm.</param>
  <param name="ThetaVar">If not nil, returns theta[n,1]..Theta[n,n] variances.</param>
  <param name="SumSqr">If not nil, returns sum of squares for each theta[n,i] element. </param>

  
<remarks>Uses the Innnovations algorithm to recursively calculate Theta[n,1]...Theta[n,n] coefficients.
</remarks>


  <SeeAlso cref="DurbinLevinson"/>*)
procedure Innovations(const kappa: TDenseMtxVec; const Theta: TVec; out Sigma2: double; const NumEvals: Integer; const ThetaVar: TVec = nil; const SumSqr: TVec = nil);overload;
(*<summary>Uses the Innnovations algorithm to recursively calculate Theta[1,1]...Theta[n,n] coefficients (all coefficients).</summary>
  
<remarks>The recursion relations are defined by the following equations:

  <IMG name="timeser015"/><para/>
  where <c>kappa(i,j)</c> are covariances.
  Use this overloaded variant only when you need all <c>theta[1,1]..theta[n,n]</c> values, otherwise use vector version.
</remarks>
*)
procedure Innovations(const kappa: TDenseMtxVec; const q: Integer; const ThetaMtx: TMtx; const Variances: TVec; const NumEvals: Integer);overload;

(*<summary>Hannah-Rissanen ARMA estimation.</summary>
  <param name="Data">Time series.</param>
  <param name="Phi">Returns estimates for Phi coefficients. AR(p) order is determined by Phi length.</param>
  <param name="Theta">Returns estimates for Theta coefficients. MA(q) order is determined by Theta length.</param>
  <param name="Sigma2">Returns estimate for Sigma^2 i.e. ARMA model variance.</param>

  
<remarks>Performs Hannah-Rissanen estimation for ARMA(p,q) model.
</remarks>


  <SeeAlso cref="ARMAInnovationsFit"/>*)
procedure ARMAHannahFit(const Data: TVec; const Phi, Theta: TVec; out Sigma2: double);

(*<summary>Innovations ARMA estimation.</summary>
  <param name="Data">Zero-mean time series. If this is not the case, subtract the mean from data.</param>
  <param name="Phi">Returns estimates for phi coefficients phi[1]..phi[p]. AR(p) order is determined by Phi length.</param>
  <param name="Theta">Returns estimates for theta coefficients theta[1]..theta[q]. MA(q) order is determined by Theta length.</param>
  <param name="PhiSE">If not nil, returns estimated phi coefficients standard errors.</param>
  <param name="ThetaSE">If not nil, returns estimated phi coefficients standard errors.</param>
  <param name="Sigma2">Returns estimate for Sigma^2 i.e. MA model variance.</param>
  <param name="MaxLags">Defines maximum lag used in calculation of ACVF. If MaxLags is -1 then the following formula
    will be used to automatically set lag number:<para/><c>Ceil(10*Log10(Data.Length))</c>.</param>

  
<remarks>Uses innovations algorithm to predict ARMA(p,q) process coefficients.
</remarks>


  <SeeAlso cref="ARMAHannahFit"/>*)
procedure ARMAInnovationsFit(const Data: TVec; const Phi,Theta: TVec; out Sigma2: double; const PhiSE :TVec = nil; const ThetaSE: TVec = nil; MaxLags: Integer = -1); overload;
(*<summary>Innovations ARMA estimation.</summary>*)
procedure ARMAInnovationsFit(const Data: TVec; const Theta: TVec; out Sigma2: double; const StdErrs: TVec = nil; MaxLags: Integer = -1); overload;

(*<summary>Single exponential smoothing.</summary>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="InitMethod">Defines how the initial values for S[0] are calculated.</param>
  <returns>MSE, evaluated at minimum.</returns>

  
<remarks>Performs single exponential smoothing using the following equation:

  <IMG name="timeser001"/><para/>
  This is the basic equation of exponential smoothing and the variable Alpha is called the smoothing constant.
  This smoothing scheme begins by setting S[0] to Y[0], where S[i] stands for smoothed observation and Y stands for
  the original observation. The subscripts refer to the time periods, 0, 1, ..., n.  Note that there is no S[0];
  the smoothed series starts with the smoothed version of the second observation. Also note that the internal algorithm
  automatically accounts for this by resizing S vector to Y.Length-1.

  Setting S[0] to Y[0] is not mandatory. There are numerous ways to initialize S[0]. Some of the choices are:

  <IMG name="timeser002"/><para/>
  Different initialization methods are controlled by the InitMethod parameter. Default value (0) uses first equation,
  setting it to (1) means the second equation will be used to initialize S[0].

  The smoothing constant alpha determines how fast the weights of the series decays. The value may be chosen either subjectively or
  objectively. Values near one put almost all weight on the most recent observations. Values of the smoothing constant near zero
  allow the distant past observations to have a large influence. When selecting the smoothing constant subjectively, you use your
  own experience with this, and similar, series. Also, specifying the smoothing constant yourself lets you tune the forecast to
  your own beliefs about the future of the series. If you believe that the mechanism generating the series has recently gone through
  some fundamental changes, use a smoothing constant value of 0.9 which will cause distant observations to be ignored. If, however,
  you think the series is fairly stable and only going through random fluctuations, use a value of 0.1.

  Note
    To select the value of the smoothing constant objectively, internal algorithm searches for an Alpha that minimizes the mean squared
    error (MSE)of the combined forecast errors of the currently available series.
</remarks>


  

  <example>
    Load data, perform smoothing and read Alpha + MSE.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector S = new Vector(0);
      Data.LoadFromFile("aerosol_particles.vec");
      // smooth data, initial alpha = 0.1
      double alpha = 0.1;
      double MSE = StatTimeSerAnalysis.SingleExpSmooth(Data,S,ref alpha,0);
      // results: MSE and MLE estimate for Alpha
    }
  }
  </code></example>

  <SeeAlso cref="SingleExpForecast"/>*)
function SingleExpSmooth(const Y, S: TVec; var Alpha: double; const InitMethod: Integer = 0): double; overload;
(*<summary>In this case a fixed smoothing constant Alpha is used in smoothing equations (no minimization is performed).</summary>
  <param name="MSE">Returns MSE, evaluated for constant Alpha.</param>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="InitMethod">Defines how the initial values for S[0] are calculated.</param>

  <Example>Load data, perform smoothing with Alpha = 0.33.
  <code lang="Delphi">
  Uses MtxExpr, StatTimeSerAnalysis, Math387;
  procedure Example;
  var Data,S: Vector;
      MSE: double;
  begin
    Data.LoadFromFile('aerosol_particles.vec');
    // smooth data with Alpha=0.33
    SingleExpSmooth(Data,S,0.33,MSE,0);
    // results: MSE
  end;
  </code>
  <code lang="C++">
  #include "MtxExpr.hpp"
  #include "Math387.hpp"
  #include "StatTimeSerAnalysis.hpp"
  void __fastcall Example();
  {
    sVector Data,S;
    Data.LoadFromFile("aerosol_particles.vec");
    // smooth data with Alpha=0.33
    double MSE;
    SingleExpSmooth(Data,S,0.33,MSE,0);
    // results: MSE
  }
  </code>
  </Example>

  
<remarks>C# Example
    Load data, perform smoothing with Alpha = 0.33.
  <code lang="C#">
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector S = new Vector(0);
      Data.LoadFromFile("aerosol_particles.vec");
      // smooth data with Alpha=0.33
      double MSE;
      StatTimeSerAnalysis.SingleExpSmooth(Data,S,0.33,out MSE,0);
      // results: MSE
    }
  }
  </code>
</remarks>


  <SeeAlso cref="SingleExpForecast"/>*)
procedure SingleExpSmooth(const Y, S: TVec; const Alpha: double; out MSE: double; const InitMethod: Integer = 0); overload;
(*<summary>Single exponential forecast.</summary>
  <param name="Y">Time series data set.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="InitMethod">Defines how the initial values for S[0] are calculated.</param>

  
<remarks>Forecasts time series values by using single exponential smoothing equations. For single exponential smoothing,
  the h period ahead forecast is given by:

  <IMG name="timeser007"/><para/>
</remarks>


  

    <example>
      Load data, assume Alpha is 0.33, forecast 20 points past the last value.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector Data = new Vector(0);
        Vector YHat = new Vector(0);
        Vector Residuals = new Vector(0);
        int NumPoints = 20;
        Data.LoadFromFile("aerosol_particles.vec");
        // last point period = Data.Length-1 + NumPoints
        int T = Data.Length - 1 + NumPoints;
        StatTimeSerAnalysis.SingleExpForecast(Data, YHat, 0.33, T, 0);
        // YHat now stores estimates for YHat[1,...Length-1]
        // so, if we need residuals, we have to subtract
        // these values from y[1,...,Length-1)
        Residuals.Size(YHat);
        Residuals.Sub(Data, YHat, 1, 0, 0, YHat.Length);
    }
  }
  </code></example>

  <SeeAlso cref="SingleExpSmooth"/>*)
procedure SingleExpForecast(const Y: TVec; const YHat: TVec; const Alpha: double; const T: Integer; const InitMethod: Integer = 0); overload;
(*<summary>rst estimate Alpha parameters by single smoothing and then use returned value to forecast up to T periods.</summary>
  <param name="MSE">MSE, evaluated at minimum.</param>
  <param name="Y">Time series data set.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="InitMethod">Defines how the initial values for S[0] are calculated.</param>

  
<remarks>Use this routine if you don't know the best estimates for Alpha.
</remarks>


  

  <example>
    Do estimation and forecasting in single routine call.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector YHat = new Vector(0);
      int NumPoints = 20;
      Data.LoadFromFile("aerosol_particles.vec");
      // last point period = Data.Length-1 + NumPoints
      int T = Data.Length-1+NumPoints;
      // initial alpha estimate = 0.6
      double alpha = 0.6;
      double mse;
      StatTimeSerAnalysis.SingleExpForecast(Data,YHat,ref alpha,T,out mse,0);
      // returs mse and estimated Alpha (from MLE)
    }
  }
  </code></example>*)
procedure SingleExpForecast(const Y: TVec; const YHat: TVec; var Alpha: double; const T: Integer; out MSE: double; const InitMethod: Integer = 0); overload;

(*<summary>Double exponential smoothing.</summary>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="B">Trend values (see above equation). Size and complex properties of b are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="Gamma">Defines initial estimate for Gamma, returns Gamma which minimizes MSE.</param>
  <param name="InitMethod">Defines how the initial values for b[0] are calculated.</param>
  <returns>MSE,evaluated at minimum.</returns>

  
<remarks>Performs double exponential smoothing using the following equations:

  <IMG name="timeser003"/><para/>
  Smoothing scheme begins by setting S[0] to Y[0] and b[0] to pne of the following choices:

  <IMG name="timeser004"/><para/>
  Different initialization methods are controlled by the InitMethod parameter. Default value (0) uses first equation,
  setting it to (1) means the second equation will be used and setting it to (2) means the third equation will be used to initialize b[0].

  The first smoothing equation adjusts S[i] directly for the trend of the previous period, b[i-1], by adding it to the last smoothed value, S[i-1].
  This helps to eliminate the lag and brings S[i] to the appropriate base of the current value. The second smoothing equation then updates the trend,
  which is expressed as the difference between the last two values. The equation is similar to the basic form of single smoothing, but here applied
  to the updating of the trend.
</remarks>


  

  <example>
    Load data, perform smoothing and read Alpha,Gamma + MSE.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector S = new Vector(0);
      Vector b = new Vector(0);
      Data.LoadFromFile("aerosol_particles.vec");
      // smooth data, initial alpha = 0.1, gamma = 0.3
      double alpha = 0.1;
      double gamma = 0.3;
      double MSE = StatTimeSerAnalysis.DoubleExpSmooth(Data,S,b,ref alpha,ref gamma,1);
      // results: MSE and MLE estimate for alpha,gamma
    }
  }
  </code></example>

  <SeeAlso cref="DoubleExpForecast"/>*)
function DoubleExpSmooth(const Y: TVec; const S, B: TVec; var Alpha, Gamma: double; const InitMethod: Integer = 0): double; overload;
(*<summary>In this case a fixed smoothing constants Alpha, Gamma are used in smoothing equations (no minimization is performed).</summary>
  <param name="MSE">Returns MSE, evaluated for constant Alpha and Gamma.</param>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="B">Trend values (see above equation). Size and complex properties of b are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="Gamma">Defines initial estimate for Gamma, returns Gamma which minimizes MSE.</param>
  <param name="InitMethod">Defines how the initial values for b[0] are calculated.</param>

  

  <example>
    Load data, perform smoothing with Alpha = 0.2, Gamma = 0.18
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector S = new Vector(0);
      Vector b = new Vector(0);
      Data.LoadFromFile("aerosol_particles.vec");

      // smooth data with Alpha=0.2, Gamma=0.18
      double MSE;
      StatTimeSerAnalysis.DoubleExpSmooth(Data,S,b,0.2,0.18,out MSE,1);
    }
  }
  </code></example>*)
procedure DoubleExpSmooth(const Y: TVec; const S,B: TVec; const Alpha, Gamma: double; out MSE: double; const InitMethod: Integer = 0); overload;

(*<summary>Double exponential forecast.</summary>
  <param name="Y">Time series data set.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="Gamma">Trend smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="InitMethod">Defines how the initial values for b[0] are calculated.</param>

  
<remarks>Forecasts time series values by using double exponential smoothing equations. For double exponential smoothing,
  the h period ahead forecast is given by:

  <IMG name="timeser008"/><para/>
</remarks>


  <SeeAlso cref="DoubleExpSmooth"/>*)
procedure DoubleExpForecast(const Y: TVec; const YHat: TVec; const Alpha, Gamma: double; const T: Integer; const InitMethod: Integer = 0); overload;
(*<summary>First estimate Alpha and Gamma parameters by double smoothing and then use returned values to forecast up to T periods.</summary>
  <param name="Y">Time series data set.</param>
  <param name="MSE">MSE, evaluated at minimum.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="Gamma">Trend smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="InitMethod">Defines how the initial values for b[0] are calculated.</param>

  
<remarks>Use this routine if you don't know the best estimates for Alpha and Gamma.
</remarks>


  

  <example>
    Do estimation and forecasting in single routine call.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector YHat = new Vector(0);
      int NumPoints = 20;
      Data.LoadFromFile("aerosol_particles.vec");
      // last point period = Data.Length-1 + NumPoints
      int T = Data.Length-1+NumPoints;
      // initial estimates for alpha, gamma
      double alpha = 0.1;
      double gamma = 0.1;
      double MSE;
      StatTimeSerAnalysis.DoubleExpForecast(Data,YHat,ref alpha,ref gamma,T,out MSE,1);
      // returs MSE and estimated Alpha (from MLE)
    }
  }
  </code></example>*)
procedure DoubleExpForecast(const Y: TVec; const YHat: TVec; var Alpha,Gamma: double; const T: Integer; out MSE: double; const InitMethod: Integer = 0); overload;
(*<summary>Triple exponential smoothing.</summary>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="B">Trend values (see above equation). Size and complex properties of b are set automatically.</param>
  <param name="L">Seasonal indices (see above equation). Size and complex properties of L are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="Beta">Defines initial estimate for Beta, returns Beta which minimizes MSE.</param>
  <param name="Gamma">Defines initial estimate for Gamma, returns Gamma which minimizes MSE.</param>
  <param name="Period">Period length. An exception is raised if Y.Length mod Period is not 0.</param>
  <returns>MSE, evaluated at minimum.</returns>

  
<remarks>Performs triple exponential smoothing (also known as Holt-Winters smoothing) using the following equations:

  <IMG name="timeser005"/><para/>
  where Y are the observations, S are the smoothed observations, b trend factors, L the seasonal indices and P is the period length.
  To initialize triple exponential smoothing method we need at least one complete season's data to determine initial estimates of the seasonal indices
  L[0]..L[P-1]. Again, there are several ways to initialize L values. The algorithm uses approach, described at
  <see href="http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm">www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm</see> page.
  For initial estimate for S and b, the following equations are being used:

  <IMG name="timeser006"/><para/>
  Note
    There are <b>no</b> <c>S[0]..S[P-2]</c> values; the smoothed series starts with the smoothed version of the Y[P] observation. Also note that the internal
    algorithm  automatically accounts for this by resizing S,b vector to Y.Length-Period.
</remarks>


  

  <example>
    Generate 24 random values representing 4 quarters x 6 years = 24, perform smoothing and read
    Alpha,Beta,Gamma + MSE.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector S = new Vector(0);
      Vector b = new Vector(0);
      Vector L = new Vector(0);
      Data.Size(24,false);
      Data.RandGauss();
      // smooth data, initial alpha = 0.1, beta=0.1, gamma = 0.3
      double alpha = 0.1;
      double beta = 0.1;
      double gamma = 0.3;
      // Period = 4
      double MSE = StatTimeSerAnalysis.TripleExpSmooth(Data,S, b, L, ref alpha,ref beta, ref gamma,4);
      // results: MSE and MLE estimate for Alpha,Beta,Gamma
    }
  }
  </code></example>

  <SeeAlso cref="TripleExpForecast"/>*)
function TripleExpSmooth(const Y: TVec; const S,B,L: TVec; var Alpha,Beta,Gamma: double; const Period: Integer): double; overload;
(*<summary>In this case a fixed smoothing constants Alpha, Beta and Gamma are used in smoothing equations (no minimization is performed).</summary>
  <param name="MSE">Returns MSE, evaluated for constant Alpha, Beta and Gamma.</param>
  <param name="Y">Time series data set.</param>
  <param name="S">Smoothed values (see above equation). Size and complex properties of S are set automatically.</param>
  <param name="B">Trend values (see above equation). Size and complex properties of b are set automatically.</param>
  <param name="L">Seasonal indices (see above equation). Size and complex properties of L are set automatically.</param>
  <param name="Alpha">Defines initial estimate for Alpha, returns Alpha which minimizes MSE.</param>
  <param name="Beta">Defines initial estimate for Beta, returns Beta which minimizes MSE.</param>
  <param name="Gamma">Defines initial estimate for Gamma, returns Gamma which minimizes MSE.</param>
  <param name="Period">Period length. An exception is raised if Y.Length mod Period is not 0.</param>*)
procedure TripleExpSmooth(const Y: TVec; const S,B,L: TVec; const Alpha,Beta,Gamma: double; out MSE: double; const Period: Integer); overload;

(*<summary>Triple exponential forecast.</summary>
  <param name="Y">Time series data set.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="Beta">Trend smoothing parameter used for forecast.</param>
  <param name="Gamma">Seasonal smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="Period">Period length. An exception is raised if Y.Length mod Period is not 0.</param>

  
<remarks>The h period ahead forecast is given by:

  <IMG name="timeser009"/><para/>
  where P is period length.
</remarks>


  <SeeAlso cref="TripleExpSmooth"/>*)
procedure TripleExpForecast(const Y: TVec; const YHat: TVec; const Alpha,Beta,Gamma: double; T: Integer; const Period: Integer); overload;
(*<summary>First estimate Alpha, Beta and Gamma parameters by triple exponential smoothing and then use returned values to forecast up to T periods.</summary>
  <param name="MSE"> MSE, evaluated at minimum.</param>
  <param name="Y">Time series data set.</param>
  <param name="YHat">Time series forecasts. Size of the YHat vector are adjusted automatically.</param>
  <param name="Alpha">Overal smoothing parameter used for forecast.</param>
  <param name="Beta">Trend smoothing parameter used for forecast.</param>
  <param name="Gamma">Seasonal smoothing parameter used for forecast.</param>
  <param name="T">Forecast values up to T period.</param>
  <param name="Period">Period length. An exception is raised if Y.Length mod Period is not 0.</param>

  
<remarks>Use this routine if you don't know the best estimates for Alpha, Beta and Gamma.
</remarks>
*)
procedure TripleExpForecast(const Y: TVec; const YHat: TVec; var Alpha,Beta,Gamma: double; const T: Integer; out MSE: double; const Period: Integer); overload;

(*<summary>Single moving average.</summary>
  <param name="Y">Sample data.</param>
  <param name="N">Number of elements in period.</param>
  <param name="M">Smoothed data.</param>
  <param name="Index">Index of first value in smoothed data.</param>
  <param name="Centered">If true, a centered moving average is perfomed.</param>
  <returns>MSE.</returns>

  
<remarks>Performs single moving average smoothing on data Y. General equation for moving average smoothing is:

  <IMG name="timeser010"/><para/>
  where N indicates number of points in period and X.Length data sample size. When using single moving average smoothing,
  bear in mind that when used as forecasts for the next period, single moving average is not able to cope with a significant
  trend.
</remarks>


  

  <example>
    Load sample time series, perform centered moving average smoothing with period 12 (yearly average) and finally plot
    the results using TChart.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  using Steema.TeeChart;
  namespace Dew.Examples
  {
    private void Example(Styles.Line line1, Styles.Line line2)
    {
      Vector ts = new Vector(0);
      Vector Mv = new Vector(0);
      int FirstIndex;
      ts.LoadFromFile("testdata.vec");
      MtxVecTee.DrawValues(ts,line1,0.0,1.0); // draw original time series
      StatTimeSerAnalysis.MovingAverage(ts,12,Mv,out FirstIndex,true);
      MtxVecTee.DrawValues(Mv,line2,FirstIndex,1.0); // draw MA over original time series
    }
  }
  </code></example>*)
function MovingAverage(const Y: TVec; const N: Integer; const M: TVec; out Index: Integer; const Centered: boolean = true): double;

(*<summary>Memory-shortening filter.</summary>
  <param name="Data">Original time series.</param>
  <param name="S">Returns transformed time series.</param>
  <param name="Tau">Returns optimal Tau shortening index.</param>
  <param name="Filter">If set, it returns coefficients of memory shortening filter. Size of Phi is adjusted automatically.</param>
  <param name="MaxTau">Maximum number of iterations in ERR minimization (default value 15).</param>

  
<remarks>Decide if time series is "long-memory", and if so, apply a memory-shortening transformation, as defined in
  Brockwell, page 318.
</remarks>


  

  <example>
    Shorten time series and retrieve optimal tau lag and shortening filter polynomial coefficients.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector ts = new Vector(0);
      Vector s = new Vector(0);
      Vector phi = new Vector(0);
      int tau;
      ts.LoadFromFile("Deaths.vec");
      StatTimeSerAnalysis.ShortenFilter(ts,s,out tau,phi,15);
    }
  }
  </code></example>

  <SeeAlso cref="ARARFit"/>
  <SeeAlso cref="ARARForecast"/>*)
procedure ShortenFilter(const Data: TVec; const S: TVec; out Tau: Integer; const Filter: TVec = nil; const MaxTau: Integer = 15);

(*<summary>Fit ARAR algorithm.</summary>
  <param name="S">Memory-shortened time series. If no memory-shortening was performed, then S defines the original unshortened time series.</param>
  <param name="Phi">Returns ARAR model phi coefficients (phi[1],phi[l1],phi[l2],phi[l3]). Size of Phi vector is adjusted automatically (4).</param>
  <param name="l1">Returns ARAR model phil1 lag.</param>
  <param name="l2">Returns ARAR model phil2 lag.</param>
  <param name="l3">Returns ARAR model phil3 lag.</param>
  <param name="Sigma2">Returns ARAR model estimated WN variance.</param>
  <param name="MaxLag">Defines upper limit for l3, where 1 &lt; l1 &lt; l2 &lt; l3 &lt;= MaxLag.</param>

  
<remarks>Fit ARAR algorithm to (optionaly) memory-shortened series. Let S[t] denote memory-shortened series, derived from Y[t] and let
  avg(S) denote sample mean of S[t]. The ARAR algorithm tries to fit an autoregressive (AR) process to the mean-corrected series:

  <IMG name="timeser011"/><para/>
  The fitted model then has the form:

  <IMG name="timeser012"/><para/>
  where Z[t] is WN(0,sigma2).
</remarks>


  

  <example>
    Fit and then forecast time series values by using ARAR algorithm. Before applying the ARAR algorithm, use the
    shortening filter on original series.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector timeseries = new Vector(0);
        Vector s = new Vector(0);
        Vector filter = new Vector(0);
        Vector phi = new Vector(0);
        Vector forecasts = new Vector(0);
        Vector stderrs = new Vector(0);
        int l1, l2, l3, tau, s22;
        double s2, rmse;
        timeseries.LoadFromFile("deaths.vec");
        // #1: shorten series
        StatTimeSerAnalysis.ShortenFilter(timeseries, s, out tau, filter, 15);
        // #2 : fit ARAR model on shortened series
        StatTimeSerAnalysis.ARARFit(s, phi, out l1, out l2, out l3, out s2, 13);
        // #3: forecast 100 values by using ARAR fit parameters
        StatTimeSerAnalysis.ARARForecast(timeseries, phi, filter, tau, l1, l2, l3, s.Mean(), 100, forecasts, stderrs, out rmse);
    }
  }
  </code></example>

  <SeeAlso cref="ARARForecast"/>
  <SeeAlso cref="ShortenFilter"/>*)
procedure ARARFit(const S: TVec; const Phi: TVec; out l1,l2,l3: Integer; out Sigma2: double; const MaxLag: Integer);

(*<summary>Forecast time series by ARAR.</summary>
  <param name="Data">Defines original time series.</param>
  <param name="Phi">Defines ARAR model Phi coefficients (phi[0],phi[1],phi[2],phi[3]).</param>
  <param name="Filter">Defines memory shortening filter, obtained from memory-shortening operation. In case no memory-shortening is performed, set filter to 1.0 by using Filter.SetIt([1.0]).</param>
  <param name="tau">Defines memory-shortening optimal lag, obtained from memory-shortening operation. In case no memory-shortening is performed, set it to 1.</param>
  <param name="l1">Defines optimal lag for phi[l1] (see equation above).</param>
  <param name="l2">Defines optimal lag for phi[l2] (see equation above).</param>
  <param name="l3">Defines optimal lag for phi[l3] (see equation above).</param>
  <param name="SMean">Defines memory-shortened series mean.</param>
  <param name="N">Defines number of forecasts.</param>
  <param name="aResult">Returns forecasts. Size and complex properties of Result are adjusted automatically.</param>
  <param name="StdErrs">Returns forecasts standard errors. Size and complex properties of StdErrs are adjusted automatically.</param>
  <param name="RMSE">Returns fit root mean square error (RMSE).</param>

  
<remarks>Forecast time series values by using ARAR model, defined by the following relation:

  <IMG name="timeser012"/><para/>
</remarks>


  

  <example>
    Fit and then forecast time series values by using ARAR algorithm. Before applying the ARAR algorithm, use the
    shortening filter on original series.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector timeseries = new Vector(0);
        Vector s = new Vector(0);
        Vector filter = new Vector(0);
        Vector phi = new Vector(0);
        Vector forecasts = new Vector(0);
        Vector stderrs = new Vector(0);
        int l1, l2, l3, tau;
        double s2, rmse;
        timeseries.LoadFromFile("deaths.vec");
        // #1: shorten series
        StatTimeSerAnalysis.ShortenFilter(timeseries, s, out tau, filter, 15);
        // #2 : fit ARAR model on shortened series
        StatTimeSerAnalysis.ARARFit(s, phi, out l1, out l2, out l3, out s2, 13);
        // #3: forecast 100 values by using ARAR fit parameters
        StatTimeSerAnalysis.ARARForecast(timeseries, phi, filter, tau, l1, l2, l3, s.Mean(), 100, forecasts, stderrs, out rmse);
    }
  }
  </code></example>

  <SeeAlso cref="ARARFit"/>
  <SeeAlso cref="ShortenFilter"/>*)
procedure ARARForecast(const Data: TVec; const Phi, Filter: TVec; const tau,l1,l2,l3: Integer; const SMean: double; const N: Integer; const aResult, StdErrs: TVec; out RMSE: double);

(*<summary>Box-Cox transformation.</summary>
  <param name="U">Time series.</param>
  <param name="Y">Transformed time series. Size of Y is adjusted automatically.</param>
  <param name="Lambda">Box-Cox transformation lambda value. An exception is raised if Lambda &lt; 0 .</param>

  
<remarks>Performs Box-Cox transformation on time series. The Box-Cox transformation is defined by the following equation:

  <IMG name="timeser013"/><para/>
</remarks>


  <SeeAlso cref="BoxCoxInv"/>*)
procedure BoxCox(const U: TVec; const Y: TVec; const Lambda: double);
(*<summary>Inverse Box-Cox transformation.</summary>
  <param name="Y">Transformed time series.</param>
  <param name="U">Time series. Size of Data is adjusted automatically.</param>
  <param name="Lambda">Box-Cox transformation lambda value. An exception is raised if Lambda &lt; 0 .</param>

  
<remarks>Performs the inverse Box-Cox transformation on time series. The inverse Box-Cox transformation is defined by the following
  equation:

  <IMG name="timeser014"/><para/>
</remarks>


  <SeeAlso cref="BoxCox"/>*)
procedure BoxCoxInv(const Y: TVec; const U: TVec; const Lambda: double);

(*<summary>The box-Ljung statistics.</summary>
  <param name="X">Defines the residuals of predicted values.</param>
  <param name="h">Defines the number of lags used in statistics.</param>
  <returns>the Box-Ljung statistics.</returns>

  
<remarks>The Ljung-Box test is based on the autocorrelation plot. However, instead of testing randomness
  at each distinct lag, it tests the "overall" randomness based on a number of lags. For this reason, it is often referred to
  as a "portmanteau" test. The Ljung-Box test statistics can be defined as follows:

  <IMG name="timeser018"/><para/>
  where n is the sample size, rho(j) is the autocorrelation at lag j, and h is the number of lags being tested.
  Actually we are testing the hypothesis:
  <list type="bullet">
  <item> H: The data are random.</item>
  <item> Ha: The data are not random.</item>
  </list>

  The Ljung-Box test is commonly used in ARIMA modeling. Note that it is applied to the residuals of a fitted ARIMA model, not the original series.
</remarks>
*)
function BoxLjung(const X: TVec; const h: Integer): double;

procedure InvTransformParams(const Src, Dst: TVec; k, p, q: integer);
procedure TransformParams(const Src, Dst: TVec; k, p, q: integer);

(*<summary>Calculates the Durbin-Watson statistic</summary>

<remarks>The Durbin-Watson tests for presence of correlation between consecutive residuals. We are testing hypothesis:
<list type="bullet">
<item> H0: The residuals are not correlated.</item>
<item> HA: Residuals are autocorrelated.</item>
</list>

The test statistics ranges from 0 to 4 where d value of means:
<list type="bullet">
<item> 0 : autocorrelation not preset.</item>
<item> &lt; 2: positive serial correlation.</item>
<item> &gt; 2: negative serial correlation.</item>
</list>

A perfect result is a value equal to two. Value less than 1.5 or bigger than 2.5 indicates an autocorrelation problem.
</remarks>
*)
function DurbinWatson(const Residuals: TVec): double;




