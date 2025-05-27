











(*<summary>Basic statistical routines.</summary>
  
<remarks>Introduces basic statistical routines, including hypothesis testing, distribution
  parameter estimation, descriptive statistics, ...
</remarks>
*)
unit Statistics;


{$I bdsppdefs.inc}

interface

uses AbstractMtxVec, MtxVec, Math387, Probabilities, Optimization, MtxVecInt

    ;

type
    (*<summary>Defines the array of two double elements.</summary>

      *)
    TTwoElmReal = array [0..1] of double;

    (*<summary>Defines one or two sided hypothesis testing.</summary>
      
<remarks>Defines if hypothesis will be tested on closed interval (two sided) or
      open interval (one sided).
</remarks>


    <SeeAlso cref="THypothesisResult"/>*)
    THypothesisType=(
    (*<summary>Perform two sided test. Acceptance region is <c>[p<sub>0</sub>-p<sub>0</sub>(signif),p<sub>0</sub>+p<sub>0</sub>(signif))</c>.</summary>*)
    htTwoTailed,
    (*<summary>Perform one sided right tailed test. Acceptance region is <c>[p<sub>0</sub>+p<sub>0</sub>(signif),INF)</c>.</summary>*)
    htRightTailed,
    (*<summary>Perform one sided left tailed test. Acceptance region is <c>[-INF,p<sub>0</sub>+p<sub>0</sub>(signif)]</c>.</summary>*)
    htLeftTailed
    );

    (*<summary>Defines result of the hypothesis test.</summary>
      
<remarks>After test, the result is one of three possibilities.
</remarks>


    <SeeAlso cref="THypothesisType"/>*)
    THypothesisResult=(
      (*<summary>Do not reject the null hypothesis <c>H<sub>0</sub></c>.</summary>*)hrNotReject,
      (*<summary>Reject the null hypothesis <c>H<sub>0</sub></c>.</summary>*)hrReject,
      (*<summary>Could not calculate the null hypothesis result. This usually means testi is NOT
        suitable for your data.</summary>*)hrNan
    );

     (*<summary>Defines type of Principal Component Analysis (PCA).</summary>
      
<remarks>Defines whether the PCA analysis is to be run on a correlation or covariance matrix.
      Normally, the analysis is run on the scale-invariant correlation matrix since the
      scale of the variables changes the analysis when the covariance matrix is used.
</remarks>


      <SeeAlso cref="PCA"/>*)
    TPCAMode = (
    (*<summary>CA is run on correlation matrix (supported by PCA).</summary>*)PCACorrMat,
    (*<summary>CA is run on covariance matrix (supported by PCA and FA).</summary>*)PCACovMat,
    (*<summary>CA is run on raw data matrix (supported by PCA and FA).</summary>*)PCARawData
    );

    (*<summary>Defines methods for calculatating percentile.</summary>*)
    TPercentileMethod = (
    (*<summary>The 100pth percentile is computed as <c>Zp = (1-d)X[k] + dX[k+1]</c>
        where k+1 equals the integer part of P(n+1), d is the fractional part of p(n+1), and X[k-1] is the kth observation when the data are sorted from lowest to highest.</summary>*)
    pctMethodNPlus=0,
    (*<summary>The 100pth percentile is computed as <c>Zp = (1-d)X[k] + dX[k+1]</c>
        where k+1 equals the integer part of P(n-1)+1, d is the fractional part of p(n+1), and X[k-1] is the kth observation when the data are sorted from lowest to highest.</summary>*)
    pctMethodNMinus=1,
    (*<summary>The 100pth percentile is computed as <c>Zp = X[k]</c>
        where k+1 equals the integer that is closest to np and X[k-1] is the kth observation when the data are sorted from lowest to highest.</summary>*)
    pctMethodClosestN=2,
    (*<summary>The 100pth percentile is computed as <c>Zp = X[k]</c>
        where k+1 equals the integer part of np if np is exactly an integer or the integer part of np+1 if np is not exactly an integer.
        X[k-1] is the kth observation when the data are sorted from lowest to highest. Note that EDF stands for empirical distribution function.</summary>*)
    pctMethodEDF=3,
    (*<summary>The 100pth percentile is computed as  <c>Zp = 0.5*(X[k1] + X[k2])y</c>
        where k1 and k2 are defined as follows: If np is an integer, k1=k2=np. If np is not exactly an integer, k1 equals the integer part of np and k2 = k1+1.
        X[k-1] is the kth observation when the data are sorted from lowest to highest. Note that EDF stands for empirical distribution function.</summary>*)
    pctMethodEDFAve=4
    );

    (*<summary>Defines Hotelling T2 test type.</summary>*)
    THotellingT2Type = (
    (*<summary>One sample Hotelling T2 test.</summary>*)htT2OneSample,
    (*<summary>Two sample paired Hotelling T2 test.</summary>*)htT2Paired,
    (*<summary>Two sample unpaired Hotelling T2 test (test homoskedasticity).</summary>*)htT2Homoskedasticity,
    (*<summary>Two sample unpaired Hotelling T2 test (test heteroskedasticity).</summary>*)htT2Heteroskedasticity
    );

    (*<summary>Defines method for pairwise distance calculation.</summary>*)
    TPWDistMethod = (
    (*<summary>Euclidian distance (L2 norm).</summary>*)pwdistEuclidian,
    (*<summary>The "City block" distance (L1 norm).</summary>*)pwdistCityBlock,
    (*<summary>The Chebychev distance.</summary>*)pwdistChebychev,
    (*<summary>Custom pairwise distance calculation.</summary>*)pwdistCustom
    );

    (*<summary>Defines the criterion for measuring the improvement of Latih Hypercube DOE.</summary>*)
    TLHCImprove = (
    (*<summary>No improvement.</summary>*)lhcImproveNone,
    (*<summary>Base criteria on maximized minimum distance between points.</summary>*)lhcImproveMinDist
    );

    (*<summary>Defines the rotation method.</summary>*)
    TMatrixRotation = (
    (*<summary>No rotation.</summary>*)rotNone,
    (*<summary>Varimax rotation.</summary>*)rotVarimax,
    (*<summary>Ortomax rotation.</summary>*)rotOrtomax,
    (*<summary>Quartimax rotation.</summary>*)rotQuartimax,
    (*<summary>Equamax rotation.</summary>*)rotEquamax
    );







(*<summary>Calculate parameters for Beta distributed values using MLE.</summary>*)
procedure BetaFit(const X: TVec; out A, B: double; MaxIter: Integer = 500; Tolerance: double = 1e-8); overload;
(*<summary>Calculate parameters for Beta distributed values.</summary>
  <param name="X">Stores data which is assumed to be Beta distributed.</param>
  <param name="A">Return Beta distribution parameter estimator a.</param>
  <param name="B">Return Beta distribution parameter estimator b.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a and b.</param>
  <param name="Tolerance">Defines the acceptable tolerance for calculating a and b.</param>
  <param name="PCIA">a (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCIB">b (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random Beta distributed
    values and then uses BetaFit routine to extract used a and b parameters:
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
        Vector v = new Vector(1000, false);
        // first, generate 1000 randomly beta distributed
        // numbers with parameters a = 3 and b = 2
        Random rnd = new Random();
        StatRandom.RandomBeta(3.0, 2.0, v, rnd.Next());

        double esta, estb;
        double[] cia = new double[2];
        double[] cib = new double[2];
        // Now extract the a,b and their 95% confidence intervals.
        //Use at max 300 iterations and tolerance 0.001
        Statistics.BetaFit(v, out esta, out estb, ref cia, ref cib, 300, 1.0e-3, 0.05);
  </code></example>

  <SeeAlso cref="RandomBeta"/>
  <SeeAlso cref="BetaStat"/>*)
procedure BetaFit(const X: TVec; out A, B: double; var PCIA, PCIB: TTwoElmReal; MaxIter: Integer = 500; Tolerance: double = 1e-8; Alpha: double = 0.05 ); overload;

(*<summary>Calculate parameters for Cauchy distributed values.</summary>*)
procedure CauchyFit(const X: TVec; out m, b: double;MaxIter: Integer = 500; Tolerance: double=1e-8); overload;
(*<summary>Calculate the parameters and their (1-Alpha) confidence invervals for Cauchy distributed values.</summary>*)
procedure CauchyFit(const X: TVec; out m, b: double; var mCI, bCI: TTwoElmReal; MaxIter: Integer = 500;
  Tolerance: double=1e-8; Alpha: double = 0.05 ); overload;

(*<summary>Calculate parameters for Chi-Squared distributed values.</summary>*)
procedure ChiSquareFit(const X: TVec; out nu: Integer);

(*<summary>Calculate parameters for Erlang distributed values.</summary>*)
procedure ErlangFit(const X: TVec; out k: Integer; out lambda: double);
(*<summary>Calculate parameters for exponentialy distributed values.</summary>*)
procedure ExponentFit(const X: TVec; out mu: double);overload;
(*<summary>Calculate parameters for exponentially distributed values.</summary>
  <param name="X">Stores data which is assumed to be exponentialy distributed.</param>
  <param name="mu">Returns exponential distribution parameter estimator.</param>
  <param name="PCIMu">Mu (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random standard exponentially distributed
    values and then uses ExponentFit routine to extract used Mu parameter:
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(1000,false);
      // first, generate 1000 randomly beta distributed
      // numbers with parameter mu=4.13, and default seed
      StatRandom.RandomExponent(4.13,vec1,-1);
      double estmu;
      double[] cimu = new double[2];
      // Now extract the mu and its 95% confidence intervals.
      Statistics.ExponentFit(vec1,out estmu,out cimu, 0.05);
  </code></example>


  <SeeAlso cref="RandomExponent"/>
  <SeeAlso cref="ExponentStat"/>*)
procedure ExponentFit(const X: TVec; out mu: double; var PCIMu: TTwoElmReal; Alpha: double = 0.05);overload;

(*<summary>Calculate parameter for Fisher-F distributed values.</summary>
  <param name="X">Stores data which is assumed to follow Fisher-F distribution.</param>
  <param name="Nu1">Return Fisher-F distribution parameter estimator Nu1.</param>
  <param name="Nu2">Return Fisher-F distribution parameter estimator Nu2.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a, b and d.</param>
  <param name="Tolerance">Defines the acceptable tolerance for calculating a and b.</param>

  <SeeAlso cref="RandomF"/>*)
procedure FFit(const X: TVec; out Nu1, Nu2: Integer; MaxIter: Integer = 500; Tolerance: double=1e-8);

(*<summary>Calculate parameters for Gamma distributed values using MLE.</summary>*)
procedure GammaFit(const X: TVec; out A, B: double; MaxIter: Integer = 500; Tolerance: double=1e-8); overload;
(*<summary>Calculate parameters for Gamma distributed values.</summary>
  <param name="X">Stores data which is assumed to be Gamma distributed.</param>
  <param name="A">Return Gamma distribution parameter estimator A.</param>
  <param name="B">Return Gamma distribution parameter estimator B.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a and b.</param>
  <param name="Tolerance"> Defines the acceptable tolerance for calculating a and b.</param>
  <param name="PCIA">A (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCIB">B (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random Gamma distributed
    values and then uses GammaFit routine to extract used a and b parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
        Vector vec1 = new Vector(0);
        // first, generate 1000 randomly gamma distributed
        // numbers with parameters a=0.5 and b =1.2
        vec1.Size(1000, false);
        StatRandom.RandomGamma(0.5, 1.2, vec1, -1);
        // Now extract the a,b and their 95% confidence intervals.
        // Use at max 400 iterations and tolerance 0.0001
        double resA, resB;
        double[] CIA = new double[2];
        double[] CIB = new double[2];
        Statistics.GammaFit(vec1, out resA, out resB, out CIA, out CIB, 400, 1e-4, 0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomGamma"/>
  <SeeAlso cref="GammaStat"/>*)
procedure GammaFit(const X: TVec; out A, B: double; var PCIA, PCIB: TTwoElmReal; MaxIter: Integer = 500; Tolerance: double=1e-8; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for inverse Gaussian distributed values.</summary>*)
procedure InverseGaussianFit(const X: TVec; out mu, lambda: double); overload;
(*<summary>Mu and lambda for inverse Gaussian distributed values.</summary>*)
procedure InverseGaussianFit(const X: TVec; out mu, lambda: double; var muConfInt, lambdaConfInt: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for Laplace distributed values.</summary>
  <param name="X"> Stores data which is assumed to be Laplace distributed.</param>
  <param name="mu">Return Laplace distribution parameter estimator M.</param>
  <param name="b">Return Laplace distribution parameter estimator b.</param>

  

  <example>
    The following example generates 100 random Laplace distributed
    values and then uses LaplaceFit routine to extract used Mu and b parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
      private void Example()
      {
          Vector Data = new Vector(100);
          StatRandom.RandomLaplace(3, 0.2, Data,-1);
          double mu, b;
          Statistics.LaplaceFit(Data, out mu, out b);
          // mu approx 3.0
          // b approx 0.2
      }
  }
  </code></example>

  <SeeAlso cref="RandomLaplace"/>*)
procedure LaplaceFit(const X: TVec; out mu, b: double);overload;

(*<summary>Calculate parameters for Logistic distributed values.</summary>
  <param name="X"> Stores data which is assumed to follow logistic distribution.</param>
  <param name="m">Return Logistic distribution parameter estimator M.</param>
  <param name="b">Return Logistic distribution parameter estimator b.</param>

  

  <example>
    The following example generates 100 random logistic distributed
    values and then uses LogisticFit routine to extract used m and b parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector Data = new Vector(100);
      StatRandom.RandomLogistic(3,0.2,Data, -1);
      double m, b;
      Statistics.LogisticFit(Data, out m, out b);
      // m approx 3.0
      // b approx 0.2
    }
  }
  </code></example>

  <SeeAlso cref="RandomLogistic"/>*)
procedure LogisticFit(const X: TVec; out m, b: double);

(*<summary>Calculate parameters for log-normally distributed values.</summary>*)
procedure LogNormalFit(const X: TVec; out mu, sigma: double);overload;
(*<summary>Calculate parameters for log-normally distributed values.</summary>
  <param name="X"> Stores data which is assumed to be log-normaly distributed.</param>
  <param name="mu">Return log-normal distribution parameter estimator M u.</param>
  <param name="sigma">Return log-normal distribution parameter estimator Sigma.</param>
  <param name="PCIMu">Mu (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCISigma">Sigma (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random log-normally distributed
    values and then uses NormalFit routine to extract used Mu and Sigma parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Data.Size(100, false);
      StatRandom.RandomLogNormal(3, 0.2, Data, -1);
      double mu, sigma;
      double[] MuCI = new double[2];
      double[] SigmaCI = new double[2];
      Statistics.LogNormalFit(Data, out mu, out sigma, out MuCI, out SigmaCI, 0.05);
      // mu approx 3.0
      // sigma approx 0.2
    }
  }
  </code></example>

  <SeeAlso cref="RandomLogNormal"/>*)
procedure LogNormalFit(const X: TVec; out mu, sigma:double; var PCIMu, PCISigma: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for Maxwell distributed values.</summary>
  <param name="X">Stores data which is assumed to to follow Maxwell distribution.</param>
  <param name="a">Return Maxwell distribution parameter estimator a.</param>

  <SeeAlso cref="RandomMaxwell"/>*)
procedure MaxwellFit(const X: TVec; out a: double); overload;

(*<summary>Calculate parameters for normally distributed values.</summary>*)
procedure NormalFit(const X: TVec; out mu, sigma: double);overload;
(*<summary>Calculate parameters for normally distributed values.</summary>
  <param name="X">Stores data which is assumed to be normaly distributed.</param>
  <param name="mu">Return normal distribution parameter estimator Mu.</param>
  <param name="sigma">Return normal distribution parameter estimator Sigma.</param>
  <param name="PCIMu">Mu (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCISigma">Sigma (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random standard normally distributed
    values and then uses NormalFit routine to extract used Mu and Sigma parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
        Vector vec1 = new Vector(0);
        // first, generate 1000 normaly distributed
        // numbers with Mu a=0.0 and Sigma =1.0
        vec1.Size(1000, false);
        StatRandom.RandomNormal(0.0, 1.0, vec1, -1);
        double resMu, resSigma;
        double[] CIMu = new double[2];
        double[] CISigma = new double[2];
        // Now extract the Mu,Sigma and their 95% confidence intervals.
        // Use at max 400 iterations and tolerance 0.0001
        Statistics.NormalFit(vec1, out resMu, out resSigma, out CIMu, out CISigma, 0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomNormal"/>
  <SeeAlso cref="NormalStat"/>*)
procedure NormalFit(const X: TVec; out mu, sigma:double; var PCIMu, PCISigma: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameter for Student-T distributed values.</summary>
  <param name="X">Stores data which is assumed to follow Student-T distribution.</param>
  <param name="Nu">Return Student-T distribution parameter estimator Nu.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a, b and d.</param>
  <param name="Tolerance">Defines the acceptable tolerance for calculating a and b.</param>*)
procedure StudentFit(const X: TVec; out Nu: Integer; MaxIter: Integer = 500; Tolerance: double=1e-8);

(*<summary>Calculate parameters for Rayleigh distributed values.</summary>*)
procedure RayleighFit(const X: TVec; out b: double); overload;
(*<summary>Calculate parameters for Rayleigh distributed values.</summary>
  <param name="X">Stores data which is assumed to be Rayleigh distributed.</param>
  <param name="b">Returns Rayleigh distribution parameter estimator.</param>
  <param name="PCIB">b (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random Rayleigh distributed
    values and then uses RayleighFit routine to extract used b parameter
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(0);
  	  // first, generate 1000 randomly Rayleigh distributed
  	  // numbers with b=1.3
  	  vec1.Size(1000,false);
	    StatRandom.RandomRayleigh(1.3,vec1,-1);
      // Now extract the r and it's 95% confidence intervals.
      double resb;
      double[] CIb = new double[2];
      Statistics.RayleighFit(vec1,out resb,out CIb,0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomRayleigh"/>
  <SeeAlso cref="RayleighStat"/>*)
procedure RayleighFit(const X: TVec; out b: double; var PCIb: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for Triangular distributed values.</summary>
  <param name="X">Stores data which is assumed to follow triangular distribution.</param>
  <param name="a">Return Triangular distribution parameter estimator a.</param>
  <param name="b">Return Triangular distribution parameter estimator b.</param>
  <param name="c">Return Triangular distribution parameter estimator c.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a, b and d.</param>
  <param name="Tolerance">Defines the acceptable tolerance for calculating a and b.</param>*)
procedure TriangularFit(const X: TVec; out a, b,c: double;MaxIter: Integer = 500; Tolerance: double=1e-8);
(*<summary>Calculate parameters for uniformly distributed values.</summary>*)
procedure UniformFit(const X: TVec; out low, high: double);overload;
(*<summary>Calculate parameters for uniformly distributed values.</summary>
  <param name="X">Stores data which is assumed to be uniformly distributed.</param>
  <param name="low">Return uniform distribution parameter estimator Low.</param>
  <param name="high">Return uniform distribution parameter estimator High.</param>
  <param name="PCILow">Low (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCIHigh">High (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random uniformly distributed
    values and then uses UniformFit routine to extract used Low and High parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(1000,false);
  	  // first, generate 1000 randomly Weibull distributed
  	  // numbers with parameters Low=1.5 and High =2.2
	    StatRandom.RandomUniform(1.5,2.2,vec1,-1);
      // Now extract the Low,High and their 95% confidence intervals.
      double resLow,resHigh;
      double[] CILow = new double[2];
      double[] CIHigh = new double[2];
      Statistics.UniformFit(vec1,out resLow,out resHigh,out CILow,out CIHigh, 0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomUniform"/>
  <SeeAlso cref="UniformStat"/>*)
procedure UniformFit(const X: TVec; out low, high : double; var PCILow, PCIHigh: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for Weibull distributed values using MLE.</summary>*)
procedure WeibullFit(const X: TVec; out A, B: double; MaxIter: Integer = 500; Tolerance: double=1e-8); overload;
(*<summary>Calculate parameters for Weibull distributed values.</summary>
  <param name="X">Stores data which is assumed to be Weibull distributed.</param>
  <param name="A">Return Weibull distribution parameter estimator A.</param>
  <param name="B">Return Weibull distribution parameter estimator B.</param>
  <param name="MaxIter">Maximum number of iterations needed for deriving a and b.</param>
  <param name="Tolerance">Defines the acceptable tolerance for calculating a and b.</param>
  <param name="PCIA">A (1-Alpha)*100 percent confidence interval.</param>
  <param name="PCIB">B (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 1000 random Weibull distributed
    values and then uses WeibullFit routine to extract used a and b parameters
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(1000,false);
  	  // first, generate 1000 randomly gamma distributed
  	  // numbers with parameters a=0.5 and b =1.2
  	  StatRandom.RandomWeibull(0.5,1.2,vec1,-1);
      // Now extract the a,b and their 95% confidence intervals.
  	  // Use at max 400 iterations and tolerance 0.0001
      double resA, resB;
      double[] CIA = new double[2];
      double[] CIB = new double[2];
      Statistics.WeibullFit(vec1,out resA,out resB,out CIA,out CIB,400,1e-4,0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomWeibull"/>
  <SeeAlso cref="WeibullStat"/>*)
procedure WeibullFit(const X: TVec; out A, B: double; var PCIA, PCIB: TTwoElmReal; MaxIter: Integer = 500; Tolerance: double=1e-8; Alpha: double = 0.05 ); overload;



(*<summary>Calculate parameters for binomial distributed values.</summary>*)
procedure BinomFit(const X: TVec; N: double; const p: TVec);overload;

procedure BinomFit(const X: TVec; const N: TVec; const p: TVec);overload;

(*<summary>Calculate parameters for binomial distributed values.</summary>
  <param name="X">Stores data which is assumed to be binomial distributed.</param>
  <param name="N">Defines binomial distribution n parameter.</param>
  <param name="p">Returns binomial distribution p parameter, estimated at each X value.</param>
  <param name="Low"></param>
  <param name="High">p (1-Alpha)*100 percent confidence intervals. Each pair of Low
    and High values defines lower and upper CI for corresponding p value.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 100 random Weibull distributed
    values and then uses WeibullFit routine to extract used a and b parameters:
    The following example generates 100 binomial distributed values and then
    uses BinomFit routine to extract p parameter
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(1000, false);
      Vector LowInt = new Vector(0);
      Vector HighInt = new Vector(0);
      Vector p = new Vector(0);
      StatRandom.RandomBinom(35, 0.25, vec1, -1);

      // Now extract p-s and its 100*(1-Alpha)
      // confidence intervals.

      Statistics.BinomFit(vec1, 35, p, LowInt, HighInt, 0.05);

      // vector p holds the p for each element in vec1,
      // vectors LowInt and HighInt hold the lower and upper conf. int. limit
      // for each element in p
    }
  }
  </code></example>

  <SeeAlso cref="RandomBinom"/>
  <SeeAlso cref="BinomStat"/>*)
procedure BinomFit(const X: TVec; N: double; const p, Low, High: TVec; Alpha: double = 0.05 );overload;

procedure BinomFit(const X: TVec; out N: Integer; out p: double); overload;


(*<summary>Calculate parameters for geometrically distributed values.</summary>*)
procedure GeometricFit(const X: TVec; out P: double); overload;
(*<summary>Calculate parameters for geometrically distributed values.</summary>
  <param name="X">Stores data which is assumed to be geometricaly distributed.</param>
  <param name="P">Returns geometric distribution parameter estimator.</param>
  <param name="PCI">P (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 1000 random Geometric distributed
    values and then uses GeometricFit routine to extract used P parameter
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
        Vector vec1 = new Vector(1000, false);
        // first, generate 1000 randomly geometr. distributed
        // numbers with parameter p=0.713
        StatRandom.RandomGeometric(0.713, vec1, -1);
        // Now, extract the p and its 100*(1-0.05) = 95%
        // confidence interval
        double resp;
        double[] CIp = new double[2];
        Statistics.GeometricFit(vec1, out resp, out CIp, 0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomGeometric"/>
  <SeeAlso cref="GeometricStat"/>*)
procedure GeometricFit(const X: TVec; out P: double; var PCI: TTwoElmReal; Alpha: double = 0.05); overload;

(*<summary>Calculate parameters for negative binomial distributed values.</summary>*)
procedure NegBinomFit(const X: TVec; out r, p: double);

(*<summary>Calculate parameters for Poisson distributed values.</summary>*)
procedure PoissonFit(const X: TVec; out lambda: double);overload;
(*<summary>Calculate parameters for Poisson distributed values.</summary>
  <param name="X">Stores data which is assumed to be Poisson distributed.</param>
  <param name="lambda">Returns Poisson distribution parameter estimator.</param>
  <param name="lambdaConfInt">Lambda (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 500 random Poisson distributed
    values and then uses PoissonFit routine to extract used Lambda parameter
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
    private void Example()
    {
      Vector vec1 = new Vector(500,false);
      // first, generate 500 randomly Poiss. distributed
      // numbers with parameter lambda=1.17
      StatRandom.RandomPoisson(1.17,vec1,-1);
      // Now, extract the lambda and its 95%
      // confidence interval
      double resLambda;
      double[] CILambda = new double[2];
      Statistics.PoissonFit(vec1,out resLambda,out CILambda,0.05);
    }
  }
  </code></example>

  <SeeAlso cref="RandomPoisson"/>
  <SeeAlso cref="PoissonStat"/>*)
procedure PoissonFit(const X: TVec; out lambda: double; var lambdaConfInt: TTwoElmReal; Alpha: double = 0.05 );overload;

(*<summary>Calculate parameters for discrete uniformly distributed values.</summary>*)
procedure UniformDFit(const X: TVec; out N: Integer); overload;
(*<summary>Calculate parameters for discrete uniformly distributed values.</summary>
  <param name="X">Stores data which is assumed to be discrete uniformly distributed.</param>
  <param name="N">Returns discrete uniform distribution parameter estimator.</param>
  <param name="NConfInt">N (1-Alpha)*100 percent confidence interval.</param>
  <param name="Alpha">Confidence interval percentage.</param>

  

  <example>
    The following example generates 1000 random discrete uniform distributed
    values and then uses UniformDFit routine to extract used N parameter:
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples;
  {
      private void Example()
      {
          Vector vec1 = new Vector(1000, false);
          // first, generate 1000 randomly disc.uniformly distributed
          // numbers with parameter N = 12
          StatRandom.RandomUniformD(12, vec1, -1);
          // Now, extract N and its 95%
          // confidence interval
          int resN;
          double[] CIN = new double[2];
          Statistics.UniformDFit(vec1, out resN, out CIN, 0.05);
      }
  }
  </code></example>

  <SeeAlso cref="RandomUniformD"/>
  <SeeAlso cref="UniformDStat"/>*)
procedure UniformDFit(const X: TVec; out N: Integer; var NConfInt: TTwoElmReal; Alpha: double = 0.05);overload;





(*<summary>Covariance/variance.</summary>
  <param name="X">Defines sample (variable) values (observables). In this
    case X is treated as row and not (as normally) column vector.</param>
  <param name="aResult">Returns the covariance (in this case equal to variance) for X vector elements.
    Because in this case X is represented as row vectro, the the result is simply scalar value
    <c>E(X(T)*X)-E(X(T))E(X) = Var(X)</c>.</param>
  <param name="NormN">If true (default value), the result will be normalized with number of observations
    (N), otherwise it will be normalized with <c>N-1</c>.</param>

  <remarks>
    The covariance between two real-valued random variables x and y,with expected
    values <c>E(x)=mu</c> and <c>E(y)=nu</c> is defined as: <para/>

    <img name="statistics005"/><para/>
    where <c>E(x), E(y)</c> are x and y expected values.

    For more info about covariance definition and properties check thd following links:<para/>
      1. <see href="http://mathworld.wolfram.com/Covariance.html">http://mathworld.wolfram.com/Covariance.html</see><para/>
      2. <see href="http://en.wikipedia.org/wiki/Covariance">http://en.wikipedia.org/wiki/Covariance</see>
  </remarks>*)
procedure Covariance(const X: TVec; out aResult: double; NormN: boolean = true ); overload;
(*<summary>Calculate the variance-covariance matrix (Result), assuming vectors X and Y are two variable and their elements are the observations.</summary>

  X and Y can be two vectors or matrices of equal size. In first case two vectors are treated as two variables, X values as first
  variable observables, Y vector values as second variable observabled. In second case two matrices are treated as two
  variables X and Y, <b>all</b> X values as X variable observables and
  <b>all</b> Y values as Y variable observables.

  <remarks>
    For column-vector valued random variables X and Y with respective expected values mu and nu,
    and respective scalar components m and n, the covariance is defined to be the m×n matrix called the
    covariance matrix:

    <img name="statistics006"/><para/>
  </remarks>

  

  <example>
    Calculate the covariance matrix from two vectors representing two variables.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector data1 = new Vector(0);
      Vector data2 = new Vector(0);
      Matrix cov = new Matrix(0,0);
      data1.SetIt(false,new double[] {1.2,3});
      data2.SetIt(false,new double[] {5,5.5});
      Statistics.Covariance(data1,data2,cov,false);
      // cov = [1.62, 0.45,
      //        0.45, 0.125]
    }
  }
  </code></example>*)
procedure Covariance(const X,Y: TDenseMtxVec; const aResult: TMtx; NormN: boolean = true); overload;
(*<summary>Calculate the covariance matrix (Result), assuming matrix X columns are variables and its rows are observations.</summary>
   
<remarks>By definition the covariance matrix is a matrix of covariances between elements of a vector. It is the natural
   generalization to higher dimensions of the concept of the variance of a scalar-valued random variable.

   If X columns represent observation samples (variables), it's rows sample(s) values (observables), <c>mu<sub>j</sub></c>
   <c>X<sub>j</sub></c> j-th column average value, then the covariance matrix is defined as:

   <img name="statistics007"/><para/>
   or in matrix form:

   <img name="statistics008"/><para/>
   where <c>E</c> is the expected value. The inverse of this matrix, is called the inverse covariance matrix or the
   precision matrix.

  Note
    This version does all necessary calculations to calculate covariance matrix.
</remarks>
*)
procedure Covariance(const X: TMtx; const aResult: TMtx; NormN: boolean = true); overload;

(*<summary>Pearson correlation coefficients.</summary>
  <param name="X">Defines first sample (variable) values (observables).</param>
  <param name="Y">Defines second sample (variable) values (observables).</param>
  <param name="aResult">Returns Pearson correlation coefficients bewteen samples
    X and Y. Size of Result is adjusted automatically.</param>

  Calculates Person correlation coefficients between X and Y representing two samples/variables
  with X and Y Values being treated as observables.

  If X and Y are two matrices, the rouitine calculates Pearson correlation coefficients r<sub>x,y</sub> between X and Y matrix.
  The algorithm assumes X and Y are two samples (variables) and <b>all</b> X are are treated as it's observables (same goes for Y).

  <remarks>
  <b>Correlation coefficient</b><para/>
  The correlation coefficient rho between two random variables is defined by the following equation:

  <img name="Statistics004"/><para/>
  where x,y are two variables, Cov covariance betwen x and y, sigma(s) their expected standard
  deviations and E-s their expected values. If the variables are independent then the correlation is 0, but the converse is not true
  because the correlation coefficient detects only linear dependencies between two variables.

  <b>Sample correlation coefficients.</b><para/>
  If we have a series of <c>n</c> measurements of X  and Y, then the Pearson product-moment
  correlation coefficient can be used to estimate the correlation of X and Y. The Pearson coefficient is also
  known as the "sample correlation coefficient". The Pearson correlation coefficient is then the best estimate
  of the correlation of X and Y .
  </remarks>

  Note
    The correlation is defined only if both of the standard deviations are finite and both of them are nonzero.

  

  <example>
    Calculate correlation coefficients from Data1 and Data2 representing two variables.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector data1 = new Vector(0);
      Vector data2 = new Vector(0);
      Matrix corr = new Matrix(0,0);
      data1.SetIt(false, new double[] {1,2,3});
      data2.SetIt(false, new double[] {5, 5.5, 1.0} );
      Statistics.CorrCoef(data1,data2,corr);
      // corr = [1.00000000, -0.81088485,
      //            -0.81088485,  1.00000000]
    }
  }
  </code></example>*)
procedure CorrCoef(const X,Y: TDenseMtxVec; const aResult: TMtx); overload;

(*<summary>Pearson correlation coefficients.</summary>
           
<remarks>Additionally returns also the Students t-distribution value as an
           indicator of statistical significance.
</remarks>
*)

procedure CorrCoef(const X,Y: TDenseMtxVec; const aResult: TMtx; var tValue: double); overload;
(*<summary>Pearson correlation coefficients between matrix rows and cols.</summary>
  
<remarks>This version calculates Pearson correlation coefficients r<sub>x,y</sub> between X matrix
  rows and cols. X colums are treated as samples (variables) and rows as values (observables).
</remarks>
*)

procedure CorrCoef(const X: TMtx; const aResult: TMtx); overload;

(*<summary>Histogram.</summary>
  
<remarks>Divide the Data vector elements into intervals, specified by the Bins vector. The Bins
  elements define the center points for the  intervals. The Bins elements must be sorted
  in ascending order. The number of elements falling in each interval is counted and the
  result for each interval (frequency distribution) is written to the Results vector.
  The Length and Complex properties of the Results vector are adjusted automatically.

  Note
    Use this version if you need non-equidistant histogram.
</remarks>


  

  <example>
    This example constructs 4 unequal bins and counts the frequencies.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector data = new Vector(0);
      Vector bins = new Vector(0);
      Vector freq = new Vector(0);
      data.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9,10});
      // Centerpoints, note that values are sorted!
      bins.SetIt(false, new double[] {1.5, 2.0, 6.0, 9.0});
      Statistics.Histogram(data,bins,freq);
      // Freq holds the count of elements in each bin
    }
  }
  </code></example>*)
procedure Histogram(const Data, Bins: TVec; const Results: TVec);overload;
(*<summary>Divide the Data vector elements into NumBins <u>equal</u> intervals.</summary>
  
<remarks>The number of elements falling in each interval is counted and the result for each interval
  (frequency distribution) is written to the Results vector. if CenterBins is true then
  Bins will store bins center points. If CenterBins is false, Bins will store bins
  edges.The Length and Complex properties of the Results and Bins vectors are
  adjusted automatically.

  Note
    Use this version if you need equidistant histogram.
</remarks>


  

  <example>
    Equal bins -> faster algorithm.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(1000);
      Vector Bins = new Vector(0);
      Vector Freq = new Vector(0);
      StatRandom.RandomNormal(-3,0.2,Data,-1); // generate some normaly distributed data
      // Now, divide the data into 15 equal intervals
      Statistics.Histogram(Data,15,Freq,Bins,true);
      // Bins holds the 15 intervals centerpoints
      // and Freq holds the count of elements in each bin
    }
  }
  </code></example>*)
procedure Histogram(const Data: TVec; const NumBins: Integer; const Results: TVec; const Bins: TVec; CenterBins: boolean = false); overload;
(*<summary>Same as above, but here only values between Min and Max parameters will be counted.</summary>

  <SeeAlso cref="CumulativeHist"/>*)
procedure Histogram(const Data: TVec; const NumBins: Integer; const Results: TVec; const Bins: TVec; CenterBins: boolean; Max,Min: double); overload;
(*<summary>Divide the Data vector elements into NumBins <u>equal</u> intervals.</summary>
      
<remarks>The number of elements falling in each interval is counted and the result for each interval
      (frequency distribution) is written to the Results vector. if CenterBins is true then
      Bins will store bins center points. If CenterBins is false, Bins will store bins
      edges.The Length and Complex properties of the Results and Bins vectors are
      adjusted automatically.
</remarks>


  <SeeAlso cref="CumulativeHist"/>*)
procedure Histogram(const Data: TVec; const NumBins: Integer; const Results: TVecInt; const Bins: TVec; CenterBins: boolean = false); overload;
(*<summary>Same as above, but here only values between Min and Max parameters will be counted.</summary>

  <SeeAlso cref="CumulativeHist"/>*)
procedure Histogram(const Data: TVec; const NumBins: Integer; const Results: TVecInt; const Bins: TVec; CenterBins: boolean; Max,Min: double); overload;
(*<summary>InterQuartile range.</summary>
  <returns>the interquartile range of all X vector elements. The interquartile range is defined as
    a difference between 75th percentile and 25th percentile of given values.</returns>

  <param name="X">Data. An exception is raised if X is complex.</param>
  <param name="Method"> Defines one of the available methods for calculating percentile. Software packages use
    different methods to calculate percentiles. For example, Excel uses pctMethodNMinus, while on the other hand,
    NCSS's default method is pctMethodNPlus.</param>

  

  <example>
    Calculate the IQR for given values.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector a = new Vector(0);
        a.SetIt(false, new double[] {2,0.1,3,4});
        double res = Statistics.InterQuartile(a,TPercentileMethod.pctMethodNMinus); // Res = 1.725  //Excel convention
    }
  }
  </code></example>

  <SeeAlso cref="Percentile"/>*)
function InterQuartile(const X: TVec; Method: TPercentileMethod= pctMethodNPlus): double;
(*<summary>Geometric mean.</summary>
  <returns>the geometric mean of given values.</returns>
  <param name="Data">Data. An exception is raised if Data is complex.</param>

  

  <example>
    Calculate the geometric mean for given values.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      a.SetIt(false, new double[] {2,0.1,3,4});
      double MG = Statistics.MeanG(a);
      // MG = 1.24466595457696
    }
  }
  </code></example>

  <SeeAlso cref="MeanH"/>
  <SeeAlso cref="MeanT"/>*)
function MeanG(const Data: TVec): double;
(*<summary>Harmonic mean.</summary>
  <returns>the harmonic mean of given values.</returns>
  <param name="Data">Data. An exception is raised if Data is complex.</param>

  

  <example>
    Calculate the harmonic mean for given values.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      a.SetIt(false, new double[] {2,3,5,4});
      double MH = Statistics.MeanH(a);
      // MH = 3.11688311688312
    }
  }
  </code></example>

  <SeeAlso cref="MeanG"/>
  <SeeAlso cref="MeanT"/>*)
function MeanH(const Data: TVec): double;
(*<summary>Trimmed mean.</summary>
  <returns>the mean of all Data vector elements, excluding highest and lowest 100*Alpha/2 percents of value.</returns>
  <param name="Data">Data. An exception is raised if Data vector is complex.</param>
  <param name="Alpha">Defines the percent of value excluded from mean calculation. If Alpha parameter is omitted,
    the default value 0.05 is used (meaning highest 2.5% and lowest 2.5% of data will
    be excluded). Alpha paramater must lie on the interval [0,1) otherwise an exception is raised.</param>

  

  <SeeAlso cref="MeanH"/>
  <SeeAlso cref="MeanG"/>*)
function MeanT(const Data: TVec; Alpha: double = 0.05): double;

(*<summary>Mode of all Src vector elements.</summary>
 
<remarks>The mode of a data sample is the element that occurs most often in the collection.
 For example, the mode of the sample <c>[1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]</c> is 6.
 Given the list of data <c>[1, 1, 2, 4, 4]</c> the mode is not <i>unique</i>, unlike the
 arithmetic mean.
</remarks>


  

  <example>
    Calculate the mode of sample data [1,5,2,11,5,3].
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      a.SetIt(false, new double[] {1,5,2,11,5,3});
      double md = Statistics.Mode(a); // md = 5
    }
  }
  </code></example>*)
function Mode(const Src: TVec): double; overload;
(*<summary>Mode of Src vector elements [SrcIndex..SrcIndex+Len].</summary>*)
function Mode(const Src: TVec; SrcIndex: Integer; Len: Integer): double; overload;

(*<summary>Moments.</summary>
  <returns>the AOrder-th central moment of all X vector elements.</returns>
  <param name="X">Data. An exception is raised if X vector is complex.</param>
  <param name="AMean">A mean value of all X vector elements.</param>
  <param name="AOrder">Defines the order of central moment.</param>

  

  <example>
    Calculate the fourth moment (related to Kurtosis)
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      a.SetIt(false, new double[] {1, 2, 7, 8});
      double amean = a.Mean();
      double m4 = Statistics.Moment(a,amean,4);
      // m4 = 94.5625
    }
  }
  </code></example>*)
function Moment(const X: TVec; AMean : double; const AOrder: Integer): double;
(*<summary>Cumulative histogram.</summary>
  
<remarks>Divide the Data vector elements into intervals, specified by the Bins vector. The Bins elements
  define the center points for the individual intervals. The Bins elements must be sorted in
  ascending order. The number of elements falling in each interval is counted and the relative
  cumulative frequency for each interval is written to the Results vector. The Length and
  Complex properties of the Results vector are adjusted automatically.

  Note
    Use this version if you need non-equidistant histogram.
</remarks>


  

  <example>
    Unequal bins -> slower that equidistant bins algorithm.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data  = new Vector(0);
      Vector Bins = new Vector(0);
      Vector CumRelFreq = new Vector(0);
      Data.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9,10});
      // define centerpoints, note that values are sorted!
      Bins.SetIt(false, new double[] {1.5, 2, 6, 9});
      Statistics.CumulativeHist(Data,Bins, CumRelFreq);
      // CumRelFreq holds the relative cumulative count of
      // elements in each bin.
    }
  }
  </code></example>*)
procedure CumulativeHist(const Data, Bins: TVec; const Results: TVec);overload;
(*<summary>Divide the Data vector elements into NumBins equal intervals.</summary>
  
<remarks>The number of elements falling in each interval is counted and the relative cumulative frequency for each interval
  is written to the Results vector. if CenterBins is true then Bins will store bins center points.
  If CenterBins is false, Bins will store bins edges. The Length and Complex properties of the
  Results and Bins vectors are adjusted automatically.

  Note
    Use this version if you need equidistant cumulative histogram.
</remarks>


  

  <example>
    Equal bins -> faster algorithm.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector Data = new Vector(1000, false);
        Vector Bins = new Vector(0);
        Vector CumRelFreq = new Vector(0);
        // generate some normally distributed data
        StatRandom.RandomNormal(-3.0, 0.2, Data, -1);
        Statistics.CumulativeHist(Data, 15, CumRelFreq, Bins, false);
        // Bins holds the 15 intervals centerpoints
        // and CumRelFreq holds the relative cumulative count of
        // elements in each bin.
    }
  }
  </code></example>

  <SeeAlso cref="Histogram"/>*)
procedure CumulativeHist(const Data: TVec; const NumBins: Integer; const Results: TVec; const Bins: TVec; CenterBins: boolean = false);overload;
(*<summary>Percentile.</summary>
  <returns>the 100*P th percentile of all X vector elements. The 100*P -th percentile is
  defined as the value that is greater than 100*P percent  of the values in X.</returns>
  <param name="X">Data. An exception is raised if X is complex.</param>
  <param name="P">Defines the 100*p percentile. The parameter P must lie in the interval (0,1), otherwise an exception is raised.</param>
  <param name="Method">Defines one of the available methods for calculating percentile. Software packages use
    different methods to calculate percentiles. For example, Excel uses pctMethodNMinus, while on the other hand,
    NCSS's default method is pctMethodNPlus.</param>

  

  <example>
    Calculate the 2nd quantile location = median position.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      double pctile;
      a.SetIt(false, new double[] {1,2,3,10,15,21});
      pctile = Statistics.Percentile(a,0.75,TPercentileMethod.pctMethodNPlus); //  = 16.5
      pctile = Statistics.Percentile(a,0.75, TPercentileMethod.pctMethodNMinus); //  = 13.75
      pctile = Statistics.Percentile(a,0.75,TPercentileMethod.pctMethodClosestN); //  = 10
    }
  }
  </code></example>

  <SeeAlso cref="InterQuartile"/>*)
function Percentile(const X: TVec; P : double; Method: TPercentileMethod = pctMethodNPlus): double; overload;
(*<summary>Percentile (several percentiles in one go).</summary>
  
<remarks>This overload calculates percentiles for all P elements in one pass. The advantage of this overload is X sample values only
  have to be sorted once.
</remarks>

  <Example>
  <code>
  procedure Example;
  var Data: Vector;
    P, Pct: Array of double;
  begin
    Data.LoadFromFile('c:\temp\Examples\Data\Percentile.vec');
    SetLength(P,4);
    SetLength(Pct,4);
    Percentile(Data,P,Pct);
  end;
  </code>
  </Example>*)
procedure Percentile(const X: TVec; const P: Array of double; var Percentiles: Array of double; Method: TPercentileMethod = pctMethodNPlus); overload;

(*<summary>Range for matrix rows.</summary>
  
<remarks>Calculates the range for each Src matrix row and stores the results in Result vector.
  Length and Complex properties of Result vector are adjusted automatically.
</remarks>


  <SeeAlso cref="RangeCols"/>
  <SeeAlso cref="Range"/>*)
procedure RangeRows(const Src: TMtx; const aResult: TVec);
(*<summary>Range for matrix columns.</summary>
  
<remarks>Calculates the range for each Src matrix column and stores the results in Result vector.
  Length and Complex properties of Result vector are adjusted automatically.
</remarks>


  <SeeAlso cref="RangeRows"/>
  <SeeAlso cref="Range"/>*)
procedure RangeCols(const Src: TMtx; const aResult: TVec);

(*<summary>Tied ranks for vector elements.</summary>
  <param name="Data">Defines data for rank calculation.</param>
  <param name="rks">Returns data ranks for all elements. If some data elements are tied, the average ranks will be returned.</param>
  <param name="Adjustment">The adjustment for ties.</param>

  
<remarks>Calculates the ranks of elements in Data vector elements. If some data elements are
  tied, routine calculates their average rank.
</remarks>


  <SeeAlso cref="Ranks"/>*)
procedure TiedRanks(const Data: TVec; const rks: TVec; out Adjustment: double);

(*<summary>Rank vector elements (no tie adjustment).</summary>
  <param name="Data">Defines data for rank calculation.</param>
  <param name="rks">Returns data ranks for all elements.</param>

  
<remarks>Ranks all Data vector elements with no adjustment for ties.
</remarks>


  <SeeAlso cref="TiedRanks"/>*)
procedure Ranks(const Data: TVec; const rks: TVec);

(*<summary>Finds the unique elements in vector/matrix.</summary>
  <param name="Src">Defines data unique elements search.</param>
  <param name="Dst">Returns unique elements. Size and <see cref="TMtxVec.Complex"/> properties
    of Dst are adjusted automatically.</param>
  <param name="Counts">If specifed, stores the count of each unique element in dataset.</param>

  
<remarks>Finds and sorts the unique elements in Src vector/matrix array. Sorted values are returned
  in Dst vector. An exception is raised if Src is Complex. Length and Complex properties of
  Dst vector are adjusted automatically.
</remarks>


  


  <example>
    Collect unique elements from matrix.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix m1 = new Matrix(0,0);
      Vector uvec = new Vector(0);
      m1.SetIt(4,3,false, new double[]
                              { 1,2,3,
                                4,2,1,
                                3,2,3,
                                3,3,1});
      Statistics.Unique(m1,uvec,null);
      // uvec elements are [1,2,3,4]
    }
  }
  </code></example>

  <SeeAlso cref="Ranks"/>
  <SeeAlso cref="TiedRanks"/>*)
procedure Unique(const Src: TMtxVec; const Dst: TVec; const Counts: TVecInt = nil);





(*<summary>Two sample Mann-Whitney test.</summary>
  <param name="Data1">First sample dataset.</param>
  <param name="Data2">Second sample dataset.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that there is no difference between samples).</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for Mann-Whitney test. Valid only if normal approximation
    is used to calculate significance level.</param>
  <param name="hType"> Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <param name="NormalApprox">If either of the sample size is greater than 8, a z-value can be used to approximate the significance level
    for the test. In this case, the calculated z is compared to the standard normal significance levels. Set NormalApprox to true
    if you want normal approximation for significance level.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result</param>
  <returns>The Mann-Whitney statistics (U, the smallest value of U1 and U2).</returns>

  <remarks>
  Performs two sample Mann-Whitney test. This is a method for the comparison of two independent random samples (Data1 and Data2).
  The Mann Whitney U statistic is defined as:

  <c>U1 = n1*n2 + 0.5*(n2+1)*n2 - sum (i=n1+1, n2) Ri</c><para/>
  where samples of size n1 and n2 are pooled and Ri are the ranks. Actually, there are two versions of the U statistic calculated,
  where <c>U2 = n1n2 - U1</c>, where n1 and n2 are the sample sizes of the two groups. The smallest of U1 or U2 is compared to the critical
  value for the purpose of the test.<para/>

  U can be resolved as the number of times observations in one sample precede observations in the other sample in the ranking.
  Wilcoxon rank sum, Kendall's S and the Mann-Whitney U test are exactly equivalent tests. In the presence of ties the Mann-Whitney test
  is also equivalent to a chi-square test for trend.

  In most circumstances a two sided test is required; here the alternative hypothesis is that Data1 values tend to be distributed
  differently to Data2 values. For a lower side test the alternative hypothesis is that Data1 values tend to be smaller than Data2 values.
  For an upper side test the alternative hypothesis is that Data1 values tend to be larger than Data2 values.<para/>

  Assumptions of the Mann-Whitney test:
  <list type="bullet">
  <item> random samples from populations </item>
  <item> independence within samples and mutual independence between samples </item>
  <item> measurement scale is at least ordinal </item>
  </list>

  A confidence interval for the difference between two measures of location is provided with the sample medians. The assumptions of this
  method are slightly different from the assumptions of the Mann-Whitney test:
  <list type="bullet">
  <item> random samples from populations </item>
  <item> independence within samples and mutual independence between samples </item>
  <item> two population distribution functions are identical apart from a possible difference in location parameters </item>
  </list>
  </remarks>

  

  <example>
    At the end of the experimental treatment period, the subjects are individually placed in a series of claustrophobia test
    situations, knowing that their reactions to these situations are being recorded on videotape. Subsequently three clinical experts,
    uninvolved in the experimental treatment and not knowing which subject received which treatment, independently view the videotapes
    and rate each subject according to the degree of claustrophobic tendency shown in the test situations. Each judge's rating takes place
    along a 10-point scale, with 1="very low" and 10="very high"; and the final measure for each subject is the simple average of the
    ratings of the three judges for that subject. In this example data for first and second group is as follows:
  <c>Group 1: 4.6, 4.7, 4.9, 5.1, 5.2, 5.5, 5.8, 6.1, 6.5, 6.5, 7.2</c><para/>
  <c>Group 2:  5.2, 5.3, 5.4, 5.6, 6.2, 6.3, 6.8, 7.7, 8.0, 8.1</c><para/>
  The investigators expected first group to prove the more effective, and sure enough it is first group that appears to show
  the lower mean level of claustrophobic tendency in the test situations. You might suppose that all they need to do now is plug their
  data into an independent-samples t-test to see whether the observed mean difference is significant. We will use Mann-Whitney U test
  to check, if this hypothesis can be rejected.

  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector v1 = new Vector(0);
      Vector v2 = new Vector(0);
      v1.SetIt(false, new double[] {4.6, 4.7, 4.9, 5.1, 5.2, 5.5, 5.8, 6.1, 6.5, 6.5, 7.2});
      v2.SetIt(false, new double[] {5.2, 5.3, 5.4, 5.6, 6.2, 6.3, 6.8, 7.7, 8.0, 8.1});
      double signif, UStat;
      double[] ci = new double[2];
      THypothesisResult hres;
      // don't use normal approximation
      UStat = Statistics.MannWhitneyTest(v1,v2,out hres,out signif,out ci,THypothesisType.htTwoTailed,0.05, false);
    }
  }
  </code></example>*)
function MannWhitneyTest(const Data1, Data2 : TVec; out hRes: THypothesisResult; out Signif: double;
                  var ConfInt: TTwoElmReal; const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05;
                  const NormalApprox: boolean = true): double;

(*<summary>One or two-sample Sign test.</summary>
  
<remarks>Performs one-sample Sign test by comparing Data median value with given Median.
  The null hypothesis is that Data median is equal to Median. Additional assumption
  is that underlying population is <i>continuous</i>.
</remarks>
*)
function SignTest(const Data: TVec; Median: double; out hRes: THypothesisResult; out Signif: double; var ConfInt: TTwoElmReal;
                  const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05):double; overload;
(*<summary>Performs paired Sign test. The routine tests the null hypothesis that Data1 and Data2 have equal median value.</summary>
  <param name="Data1">First set of data from which to compute the median.</param>
  <param name="Data2">Second set of data from which to compute the median.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>The Sign test statistics.</returns>

  

  <example>
    In this example we'll use paired sign test to verify if both sample medians are equal.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector d1 = new Vector(0);
      Vector d2 = new Vector(0);
      d1.SetIt(false, new double[] {1,4,5,6,7,12});
      d2.SetIt(false, new double[] {3,3.5,2,3.1,10,9});
      double signif, SStat;
      double[] ci = new double[2];
      THypothesisResult hres;
      // don't use normal approximation
      SStat = Statistics.SignTest(d1,d2,out hres,out signif,out ci,THypothesisType.htTwoTailed,0.05);
    }
  }
  </code></example>

  <SeeAlso cref="WilcoxonSign"/>*)
function SignTest(const Data1, Data2 : TVec; out hRes: THypothesisResult; out Signif: double; var ConfInt: TTwoElmReal;
                  const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05):double; overload;

(*<summary>One or two-sample Wilcoxon signed rank test.</summary>
  
<remarks>Performs one-sample Wilcoxon Signed-Rank test by comparing Data median value with given Median.
  The null hypothesis is that Data median is equal to Median.
</remarks>
*)
function WilcoxonSign(const Data: TVec; const Median: double; out hRes: THypothesisResult;
                      out Signif : double; var ConfInt: TTwoElmReal; const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05):double; overload;
(*<summary>Performs two-sample Wilcoxon Signed-Rank test by comparing Data1 and Data2 medians.</summary>
  <param name="Data1">First set of data from which to compute the median.</param>
  <param name="Data2">Second set of data from which to compute the median.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>The Wilcoxon W statistics.</returns>

  
<remarks>The routine tests the null hypothesis that Data1 and Data2 have equal median value.
</remarks>


  <SeeAlso cref="SignTest"/>*)
function WilcoxonSign(const Data1, Data2: TVec; out hRes: THypothesisResult;
                      out Signif : double; var ConfInt: TTwoElmReal;
                      const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05):double; overload;

(*<summary>One or two sample paired or pooled T-test.</summary>
  
<remarks>Performs the one sample T-test. It compares Data mean value with the Mean.
  The null hypothesis is that Data mean value is equal to the Mean.
</remarks>
*)
function tTest(const Data: TVec; Mean: double; out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal ;
                const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05 ):double; overload;
(*<summary>Performs the two sample pooled or paired t-test.</summary>
  <param name="Data1">First set of data from which to compute the mean.</param>
  <param name="Data2">Second set of data from which to compute the mean.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Paired">If true, tTest routine will perform Two-Sample Paired t-Test. If false, tTest will perform
    Two-Sample Pooled (unpaired) t-Test.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>The T-test statistics.</returns>

  
<remarks>It compares Data1 mean value with Data2 mean value. The assumption is Data1
  and Data2 variances are equal, but unknown. The null hypothesis is that Data1 mean is equal to Data2 mean.
</remarks>


  

  <example>
    In this example we'll try to confirm hypothesis that first sample
    mean is smaller from second sample mean.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector d1 = new Vector(0);
      Vector d2 = new Vector(0);
      d1.SetIt(false, new double[] {2, 3, 5, 2.5}); // mean = 3.125
      d2.SetIt(false, new double[] {5, 7, 8}); // mean = 6.6666666667
      double signif, TStat;
      double[] ci = new double[2];
      THypothesisResult hres;
      // don't use normal approximation
      TStat = Statistics.tTest(d1,d2,out hres,out signif,out ci,false,THypothesisType.htLeftTailed,0.05);
      // signif = 0.01070092300; hres = hrReject;
      // ci=[ -5.702239060933, +INF  ]
      // Comment : Since the signif is smaller than Alpha (0.05), the
      // null hypothesis (H0) that data means are equal is rejected and
      // the alternative (Ha) that d1 mean is smaller than d2 mean
      // CANNOT be rejected
    }
  }
  </code></example>

  <SeeAlso cref="ZTest"/>*)
function tTest(const Data1, Data2: TVec; out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal;
                Paired: boolean = false; hType: THypothesisType = htTwoTailed; Alpha: double = 0.05):double; overload;

(*<summary>Z Test.</summary>
  <param name="Data">The data samples to be analyzed.</param>
  <param name="Sigma">The standard deviation to compare the data to.</param>
  <param name="Mean">The mean to value to compare the data to.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>Z statistics.</returns>

  
<remarks>Compares the normally distributed Data elements mean value with known standard deviation Sigma, to a mean value Mean.
</remarks>


  

  <example>
    We'll use Z-Test to determine if data mean and variance are equal
    to specific values.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector d1 = new Vector(0);
        d1.SetIt(false, new double[] { 3, 3.5, 2, 3.1, 10, 9 });
        double signif, ZStat;
        double[] ci = new double[2];
        THypothesisResult hres;
        // don't use normal approximation
        ZStat = Statistics.ZTest(d1, 5.0, 1.29, out hres, out signif, out ci, THypothesisType.htTwoTailed, 0.05);
        // signif = 0.84940087229; hres = hrNotReject;
        // ci=[4.06780398958, 6.13219601041]
        // Comment : Since the SignRes is greater than Alpha (0.05), the
        // null hypothesis (H0) that data mean is equal to 5.1 cannot be rejected
        // the alternative (Ha) that data mean are NOT equal can therefore be
        // rejected
    }
  }
  </code></example>

  <SeeAlso cref="tTest"/>*)
function ZTest(const Data: TVec; const Mean, Sigma: double; out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal;
                const hType: THypothesisType = htTwoTailed; const Alpha: double = 0.05): double;

(*<summary>Chi-Squared Test.</summary>
  <param name="Data">Data to be tested.</param>
  <param name="Sigma">Standard deviation which is compared to data standard deviation.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>

  
<remarks>Performs Chi-Squared test by comparing sample standard deviation with Sigma.
</remarks>
*)
procedure ChiSquareTest(const Data: TVec; Sigma: double; out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal;
                hType: THypothesisType = htTwoTailed; Alpha: double = 0.05);
(*<summary>F Test.</summary>
  <param name="Data1">First data set from which to compute standard deviation.</param>
  <param name="Data2">Second data set from which to compute standard deviation.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that the means are equal).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the mean.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>The F-test statistics.</returns>

  
<remarks>Performs F-Test by comparing Data1 and Data2 standard deviations.
</remarks>
*)
function FTest(const Data1, Data2: TVec; out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal;
                hType: THypothesisType = htTwoTailed; Alpha: double = 0.05):double;

(*<summary>One sample Kolmogorov-Smirnov GOF test.</summary>
  <param name="Data">Samples to be tested.</param>
  <param name="CDFx">Defines set of possible x values.</param>
  <param name="CDFy">Defines set of hypothesized CDF values, evaluated at CDFx.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from specific distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>K-S statistics.</returns>

  
<remarks>Performs one-sample Kolmogorov-Smirnov (KS) goodnes of fit test.
  The KS test is used to decide if a sample comes from a population with a specific distribution.
  Test is based on the empirical distribution function (ECDF). An attractive feature of this test is that the
  distribution of the K-S test statistic itself does not depend on the underlying cumulative distribution
  function being tested. Another advantage is that it is an exact test (the chi-square goodness-of-fit test
  depends on an adequate sample size for the approximations to be valid). Despite these advantages, the K-S test
  has several important limitations:
  <list type="bullet">
  <item> It only applies to continuous distributions. </item>
  <item> It tends to be more sensitive near the center of the distribution than at the tails. </item>
  <item> Perhaps the most serious limitation is that the distribution must be fully specified. That is,
    if location, scale, and shape parameters are estimated from the data, the critical region of the K-S
    test is no longer valid. It typically must be determined by simulation. </item>
  </list>

  If CDFx and CDFy vectors are not defined, Data values are compared with standard normal distribution.
  If defined, CDFx and CDfy vectors represent hypothesized distribution x and CDF(x) values. In this case all
  Data values must lie within the [Min(CDFx),Max(CDFx)] interval. The KS test assumes CDFx and CDFy are predefined -
  KS test is not very accurate if CDFx and CDFy values are calculated from Data values.
</remarks>


  

  <example>
    In this example sample is generated using Normal (mu=2,sigma=1) distribution.
    Then a KS test is used to determine if sample comes from normal distribution.

  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector d = new Vector(300);
        StatRandom.RandomNormal(2, 1, d, -1);
        THypothesisResult hRes;
        double Signif;

        double KS = Statistics.GOFKolmogorov(d, out hRes, out Signif, null, null, THypothesisType.htTwoTailed, 0.05);

      // Result should be significance below 0.05 meaning d1 and d2 value
      // do not come from same distribution  =>  H0 is therefore rejected!
    }
  }
  </code></example>

  <SeeAlso cref="GOFLillieTest"/>
  <SeeAlso cref="GOFBeraJarqueTest"/>*)
function GOFKolmogorov(const Data: TVec; out hRes: THypothesisResult; out Signif : double;
                const CDFx: TVec = nil; const CDFy: TVec = nil;
                hType: THypothesisType = htTwoTailed; Alpha: double = 0.05):double; overload;
(*<summary>Two sample Kolmogorov-Smirnov GOF test.</summary>
  <param name="Data1">First dataset.</param>
  <param name="Data2">Second dataset.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from specific distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is below the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>K-S statistics.</returns>

  
<remarks>Performs two-sample Kolmogorov-Smirnov goodnes of fit test on indepentent random samples Data1 and Data2.
  Test determines if Data1 and Data2 samples are drawn from the same continuous population.
</remarks>


  

  <example>
  In this example two differently sized samples are generated using diferent Weibull(a=2,b=3) and Normal(mu=2,sigma=1)
  distributions. Then a KS test is used to determine if both samples come from the same distribution.

  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example1()
    {
        Vector d1 = new Vector(30);
        Vector d2 = new Vector(22);
        StatRandom.RandomWeibull(2, 3, d1, -1);
        StatRandom.RandomNormal(2, 1, d2, -1);
        THypothesisResult hRes;
        double Signif;

        double KS = Statistics.GOFKolmogorov(d1, d2, out hRes, out Signif, THypothesisType.htTwoTailed, 0.05);

      // Result should be significance below 0.05 meaning d1 and d2 value
      // do not come from same distribution  =>  H0 is therefore rejected!
    }
  }
  </code></example>*)
function GOFKolmogorov(const Data1, Data2: TVec; out hRes: THypothesisResult; out Signif : double;
                hType: THypothesisType = htTwoTailed; Alpha: double = 0.05): double; overload;

(*<summary>The Chi-Squared goodness of fit test.</summary>
  <param name="O">Observed frequencies.</param>
  <param name="E">Estimated (theoretical) frequencies.</param>
  <param name="NumSamples">Number of samples in original data vector.</param>
  <param name="DegF">Degrees of freedom for Chi2 statistics.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from specific distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>

  
<remarks>Performs the Chi-squared goodness of fit test to test if data is coming from specific distribution.
</remarks>
*)
procedure GOFChi2Test(const O,E: TVec; NumSamples, DegF: Integer; out hRes: THypothesisResult; out Signif : double;
  hType: THypothesisType = htTwoTailed; Alpha: double = 0.05); overload;
(*<summary>The Chi-Squared goodness of fit test.</summary>
  <param name="Data">Samples to be tested.</param>
  <param name="NumBins">Number of frequency histogram bins.</param>
  <param name="Distribution">Distribution name (string). At the moment the following string values are supported : 'beta', 'exponential',
    'gamma', 'normal', 'rayleigh' and 'weibull'.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from specific distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>

  
<remarks>Performs the Chi-squared goodness of fit test to test if data is coming from specific distribution.
  This overload version does not require expected and actual frequencies, but only data vector.
</remarks>
*)
procedure GOFChi2Test(const Data: TVec; Distribution: TDistribution; NumBins: Integer; out hRes: THypothesisResult; out Signif : double;
  hType: THypothesisType = htTwoTailed; Alpha: double = 0.05); overload;

(*<summary>The Bera-Jarque GOF test to a normal distribution.</summary>
  <param name="Data">Samples to be tested.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data composite normality is true).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>Bera-Jarque Statistics.</returns>

  
<remarks>Performs the Bera-Jarque test to a normal distribution.
</remarks>


  <SeeAlso cref="GOFLillieTest"/>
  <SeeAlso cref="GOFKolmogorov"/>*)
function GOFBeraJarqueTest(const Data:TVec; out hRes: THypothesisResult; out Signif : double; Alpha: double = 0.05): double;

(*<summary>The Lilliefors GOF test to a normal distribution.</summary>
  <returns>KS test statistics value.</returns>
  <param name="Data">Vector, storing sample values to-be-tested.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from normal distribution).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <param name="MCTol2">Defines default tolerance for Monte Carlo algorithm (0,0001).</param>

  
<remarks>Performs the Lilliefors goodnes of fit test to a normal distribution with unknown parameters mu and sigma.<para/>

  The test proceeds as follows:
  <list type="number">
  <item> First estimate the population mean and population variance based on the data. </item>
  <item> Then find the maximum discrepancy between the empirical distribution function and the cumulative distribution function (CDF) of
      the normal distribution with the estimated mean and estimated variance. Just as in the Kolmogorov-Smirnov test, this will
      be the test statistic. </item>
  <item> Finally, we confront the question of whether the maximum discrepancy is large enough to be statistically significant, thus requiring
      rejection of the null hypothesis. This is where this test becomes more complicated than the Kolmogorov-Smirnov test. Since the
      hypothesized CDF has been moved closer to the data by estimation based on those data, the maximum discrepancy has been
      made smaller than it would have been if the null hypothesis had singled out just one normal distribution. Thus we need the "null distribution" of the test statistic, i.e. its probability distribution assuming the null hypothesis is true. This is the Lilliefors distribution. To date, tables for
      this distribution have been computed only by Monte Carlo methods. </item>
  </list>

  Note: <para/>
    The test is relatively weak and a large amount of data is typically required to reject the normality hypothesis. A more sensitive test is the Jarque-Bera test which is based on a combination of the estimates of skewness and kurtosis. The Jarque-Bera test is therefore highly attentive to
    outliers, which the Lilliefors is not.
</remarks>


  <SeeAlso cref="GOFKolmogorov"/>
  <SeeAlso cref="GOFBeraJarqueTest"/>*)
function GOFLillieTest(const Data:TVec; out hRes: THypothesisResult; out Signif : double;
  Alpha: double = 0.05; MCTol2: double = 1E-4): double;

(*<summary>The Shapiro-Wilks test for normality of data.</summary>
  <param name="Data">Data vector, containing <b>ordered</b> and <b>unique</b> deviates from unknown distribution. Data Length must be betweeen 3 and 5000, otherwise
    an execption is raised.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from normal distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>the Shapiro-Wilks statistics.</returns>

  
<remarks>Performs the The Shapiro-Wilks test for normality of data.

  Note
    Basic assumption is Data values i.e. deviates from unknown distribution are <b>ordered</b> and <b>unique</b>.ž
</remarks>


  

  <example>
    In this example we'll use ShapiroWilks test to determine if data is coming from normal distribution.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector a = new Vector(0);
        Vector b = new Vector(0);
        a.Size(1000, false);
        StatRandom.RandomWeibull(2.5, 1.0, a, -1);
        // filter original data, retain only sorted unique values
        Statistics.Unique(a, b, null);
        THypothesisResult hres;
        double sign;
        Statistics.ShapiroWilks(b, out hres, out sign, THypothesisType.htTwoTailed, 0.05);
        // Significance approx. 0
        // hres = hrReject -> data is not coming from normal distribution
    }
  }
  </code></example>

  <SeeAlso cref="Unique"/>
  <SeeAlso cref="ShapiroFrancia"/>*)
function ShapiroWilks(const Data: TVec; out hRes: THypothesisResult; out Signif: double;
  hType: THypothesisType = htTwoTailed; Alpha: double = 0.05): double;

(*<summary>The Shapiro-Francia test for normality of data.</summary>
  <param name="Data">Data vector, containing <b>ordered</b> and <b>unique</b> deviates from unknown distribution.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is that data comes from normal distribution).</param>
  <param name="hType">Defines the type of the null hypothesis (left, right and two - tailed).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>the Shapiro-Francia statistics.</returns>

  
<remarks>Performs the The Shapiro-Francia test for normality of data.

  Note
    Basic assumption is Data values i.e. deviates from unknown distribution are <b>ordered</b> and <b>unique</b>.ž
</remarks>


  

  <example>
    In this example we'll use Shapiro-Francia test to determine if data is coming from normal distribution.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector a = new Vector(0);
      Vector b = new Vector(0);
      a.Size(1000,false);
      StatRandom.RandomWeibull(2.5,1.0,a,-1);
      // filter original data, retain only sorted unique values
      Statistics.Unique(a,b, null);
      THypothesisResult hres;
      double sign;
      ShapiroFrancia(b,out hres, out sign,THypothesisType.htTwoTailed,0.05);
      // Significance approx. 0
      // hres = hrReject -> data is not coming from normal distribution
    }
  }
  </code></example>

  <SeeAlso cref="Unique"/>
  <SeeAlso cref="ShapiroWilks"/>*)
function ShapiroFrancia(const Data: TVec; out hRes: THypothesisResult; out Signif: double;
  hType: THypothesisType = htTwoTailed; Alpha: double = 0.05): double;

(*<summary>Anderson-Darling GOF test.</summary>
  <param name="Data">Stores <b>ordered</b> data.</param>
  <param name="Distribution">Perform test for this distribution. Supported distributions : exponential, log-normal, normal and weibull.</param>
  <param name="hRes">Returns the result of the null hypothesis.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>Anderson-Darling test statistics, adjusted with small sample size factor.</returns>

  
<remarks>The Anderson-Darling test (Stephens, 1974) is used to test if a sample of data came from a population with
  a specific distribution. It is a modification of the Kolmogorov-Smirnov (K-S) test and gives more weight to
  the tails than does the K-S test. The K-S test is distribution free in the sense that the critical values
  do not depend on the specific distribution being tested. The Anderson-Darling test makes use of the specific
  distribution in calculating critical values. This has the advantage of allowing a more sensitive test and
  the disadvantage that critical values must be calculated for each distribution.<para/>

  The Anderson-Darling test is defined as:
  <list type="bullet">
  <item> H0: The data follow a specified distribution.</item>
  <item> Ha: The data do not follow the specified distribution.</item>
  </list>

  The test statistics is defined as:

  <IMG name="statistics003"/><para/>
  where F is the cumulative distribution function of distribution being tested.

  To learn more about A-D test, check the following links:
  <list type="bullet">
  <item> <see href="http://www.itl.nist.gov/div898/handbook/eda/section3/eda35e.htm">http://www.itl.nist.gov/div898/handbook/eda/section3/eda35e.htm</see>   </item>
  <item> <see href="http://Src.alionscience.com/pdf/A_DTest.pdf">http://Src.alionscience.com/pdf/A_DTest.pdf</see></item>
  </list>

  Note<para/>
    The basic assumption is that the data values are <b>sorted in ascending order</b>.
</remarks>


  <SeeAlso cref="GOFKolmogorov"/>*)
function AndersonDarling(const Data: TVec; Distribution: TDistribution; out hRes: THypothesisResult;
  out Signif : double; Alpha: double = 0.05): double;

(*<summary>Spearman rank correlation test.</summary>
  <param name="X">X dataset.</param>
  <param name="Y">Y dataset.</param>
  <param name="Rs">Returns Spearman rank correlation coefficient.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is there is no monotonic relation between the variables => Rs=0).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the 100*(1-Alpha) percent confidence interval for the Rs coefficient.</param>
  <param name="hType">Defines the type of the null hypothesis (one or two - tailed, default value two-tailed).</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>

  
<remarks>Performs the Spearman rank correlation test. Spearman rank correlation is a distribution-free
  analog of correlation analysis mentioned. Like regression, it can be applied to compare
  two independent random variables, each at several levels (which may be discrete or continuous).
  Unlike regression, Spearman's rank correlation works on ranked (relative) data, rather than
  directly on the data itself. Like the R2 value produced by regression, the Spearman's Rs
  coefficient indicates agreement. A value of rs near one indicates good agreement; a value
  near zero, poor agreement. Of course, as a distribution-free method, the Spearman rank
  correlation does not make any assumptions about the distribution of the underlying data.

  Spearman test is a distribution free test that determines whether there is a monotonic relation
  between two variables (X , Y). A monotonic relation exists when any increase in one variable
  is invariably associated with either an increase or a decrease in the other variable.
</remarks>
*)
procedure SpearmanRankCorr(const X, Y: TVec; out Rs: double;  out hRes: THypothesisResult;
                out Signif : double; var ConfInt: TTwoElmReal; hType: THypothesisType = htTwoTailed; Alpha: double = 0.05);

(*<summary>Empirical CDF.</summary>
  <param name="Data">Data, sorted in ascending order, with NANs and INFs removed.</param>
  <param name="xCDF">Values at which CDF increases.</param>
  <param name="yCDF">Calculated CDF.</param>

  
<remarks>Calculates empirical cumulative distribution function. Given N ordered data points <c>Y1, Y2, ..., YN,</c>
  the empirical CDF (ECDF) is defined as:

  <c>E(n)=n(i)/N , </c><para/>
  where n(i) is the number of points less than Yi and the Yi are ordered from smallest to largest value.
  This is a step function that increases by 1/N at the value of each ordered data point.
</remarks>
*)
procedure EmpiricalCDF(const Data: TVec; const xCDF,yCDF: TVec);

(*<summary>Grubb's test for outliers.</summary>
  <param name="Data">dataset.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is there are no outliers).</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="ConfInt">Returns the confidence interval to determine the outliers.</param>
  <param name="hType">Defines the type of the null hypothesis (one or two - tailed, default value two-tailed).</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>Grubb's (G) statistic.</returns>

  Performs the Grubbs test for outliers. Test is used to detect outliers in a univariate data set. It is based on the assumption
  of normality. That is, you should first verify that your data can be reasonably approximated by a normal distribution before
  applying the Grubbs' test.

  <remarks>
    Grubbs' test detects one outlier at a time. This outlier is expunged from the dataset and the test is iterated until no outliers
    are detected. However, multiple iterations change the probabilities of detection, and the test should NOT be used for sample
    sizes of six or less since it frequently tags most of the points as outliers. Grubbs' test is also known as the maximum normed
    residual test.

    Grubbs' test is defined for the hypothesis:
    * H0:  There are no outliers in the data set.
    * Ha:  There is at least one outlier in the data set.
  </remarks>

  More about the test can be found at <see href="http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h.htm">here</see>.*)
function GrubbsTest(const Data: TVec; out hRes: THypothesisResult; out Signif: double; var ConfInt: TTwoElmReal;
  hType: THypothesisType = htTwoTailed; Alpha: double = 0.05): double;






(*<summary>Performs a principal component analysis (PCA).</summary>
  
<remarks>Perform a PCA by using the original data covariance matrix CovMat. Return the principal
  components in PC matrix, eigenvalues of the covariance matrix (variances) in vector
  EigenVec and (optional) the percentage of total variance in vector VarPct. The PC,
  EigenVec and VarPct dimensions are adjusted automatically.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example
    {
        Matrix data = new Matrix(0, 0);
        Matrix PC = new Matrix(0, 0);
        Vector Z = new Vector(0);
        Vector variances = new Vector(0);

        data.SetIt(2,4, false, new double[] { 1,3,5,2,2,5,7,9});
        Statistics.Covariance(data,covMat,false);
        Statistics.PCA(covMat, PC, Z, variances); //requires cov matrix

         //      Z = [29 ,  0, 0 ,0 ]
        //variance = [100,  0, 0 ,0 ]

    }
  }
  </code></example>*)
procedure PCA(const CovMat: TMtx; const PC: TMtx; const EigenVec: TVec; const VarPct: TVec = nil);overload;
(*<summary>Perform a PCA on Data matrix, where Data columns are variables and rows are the observables.</summary>
  
<remarks>The (optional) PCAMode parameter defines whether the analysis should be run on
  correlation or covariance matrix. PCA procedure returns the principal components in matrix
  PC, the Z-scores (data, transformed in the PC space) in ZScores, the eigenvalues of the
  covariance matrix (variances) in the EigenVec vector and (optional) the percentage of
  total variance in VarPct vector. The PC, ZScores, EigenVec and VarPct dimensions are
  adjusted automatically.
</remarks>


  

  <example>
    In this example we derive the covariance matrix from original data
    and get the same results as in first example.
  <code>
  using Dew.Math;
  using Dew.Stat.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Matrix data = new Matrix(0, 0);
        Matrix PC = new Matrix(0, 0);
        Matrix Z = new Matrix(0, 0);
        Vector variances = new Vector(0);
        Vector varPercent = new Vector(0);
        data.SetIt(2, 4, false, new double[]
                                    {1,3,5,2,
                                    2,5,7,9});

        Statistics.PCA(data, PC, Z, variances, varPercent, TPCAMode.PCACovMat);  //works on raw data
        // ... variances  = [29,0,0,0]
        // varPercent = [100,0,0,0]
    }
  }
  </code></example>

  <SeeAlso cref="PCAResiduals"/>
  <SeeAlso cref="BartlettTest"/>*)
procedure PCA(const Data: TMtx; const PC, ZScores:TMtx; const EigenVec: TVec; const VarPct: TVec = nil; const PCAMode: TPCAMode = PCACorrMat);overload;

(*<summary>PC residuals.</summary>
  
<remarks>Constructs the residuals, copied from NumPC principal components of Data, where Data columns are variables
  and rows are the observables. An exception is raised if NumPC is more or equal than the number of Data
  columns (number of variables). The Rows, Cols and Complex properties of the Residuals matrix are
  adjusted automatically.
</remarks>


  <SeeAlso cref="PCA"/>*)
procedure PCAResiduals(const Data: TMtx; const NumPC: Integer; const Residuals: TMtx);

(*<summary>Performs the Bartlett test for dimensionality of data.</summary>
  <param name="Data">Data to be analyzed.</param>
  <param name="NumDim">Returns the number of dimensions needed to explain the non random
    variation in data. If NumDim is less than zero, the test cannot be applied to this data. </param>
  <param name="SignifVec">Returns calculated significance probabilities for one,
    two ... variables (dimensions).</param>
  <param name="Alpha">Desired significance.</param>

  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stat.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      int numdim;
      Matrix data = new Matrix(0,0);
      Vector signifVec = new Vector(0);
      data.LoadFromFile("c:\\temp\\data.mtx"); // load test data
      Statistics.BartlettTest(data, out numdim, signifVec, 0.05); // Alpha = 5%
      // returns numdim and signifVec
    }
  }
  </code></example>

  <SeeAlso cref="PCA"/>*)
procedure BartlettTest(const Data:TMtx; out NumDim: Integer; const SignifVec: TVec; const Alpha: double = 0.05);





(*<summary>Creates the factors for full factorial design.</summary>
  
<remarks>Performs Two-Level Full Factorial design. The results (factors) are stored in Result matrix. Parameter
  n defines number of Result matrix columns. Optional parameter MulVec (default value nil) is used only
  by the Wilcoxon Signed Rank test procedure. Size and Complex properties of Result matrix are adjusted
  automatically.
</remarks>


  

  <example>
  <code>
    Matrix m = new Matrix(0,0);
    Statistics.FullFactDesign(2,m,null);
  </code></example>*)
procedure FullFactDesign(const n: Integer; const aResult: TMtx; const fp: TMtxFloatPrecision; const MulVec: TVec = nil);overload;
(*<summary>Performs Mixed-Level Full Factorial Design.</summary>
  
<remarks>The UniqueLevels vector stores the number of unique settings for each column.
  UniqueLevels values must be integers, greater than 1. The results (factors)
  are stored in Result matrix. Size and Complex properties of Result matrix are adjusted automatically.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stat.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector tmpVec = new Vector(0);
      Matrix m = new Matrix(0,0);
      tmpVec.SetIt(false, new double[] {2,3,2});
      Statistics.FullFactDesign(tmpVec,m);
    }
  }
  </code></example>*)
procedure FullFactDesign(const UniqueLevels: TVec; const aResult: TMtx);overload;

(*<summary>One-sample Hotelling T2 test.</summary>
  <param name="X">Stores test values, each row representing different case and each column representing different response variable.
    The assumption is data is approximately multivariate normal.</param>
  <param name="Means">Stores estimated mean for each variable. An exception is raised if Means Length is not equal to Data columns. If
    Means is nil, the assumption is means are equal.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is variable means are equal to Means vector).</param>
  <param name="Alpha">Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <returns>Hotelling T2 Statistics for one-sample test.</returns>

  
<remarks>Performs one-sample Hotelling T2 test. The one-sample T2 is used to test hypotheses about a set of means simultaneously.
  The null hypothesis is that sample means are equal to Means vector values. The following assumptions are made when using T2:
  * The population follows the multivariate normal distribution.
  * The members of the sample are independent.

  The one-sample T2 test may also be applied to the situation in which two samples are to be compared that had a natural pairing
  between two observation vectors. In this case the differences between the first and second measurements are formed and then used
  as data in unpaired Hotelling T2 test.
</remarks>
*)
function HotellingT2One(const X: TMtx; const Means: TVec; out Signif: double; out hRes: THypothesisResult; const Alpha: double = 0.05): double; overload;
(*<summary>Two-sample Hotelling T2 test.</summary>
  <param name="X1">First test data.</param>
  <param name="X2">Second test data.</param>
  <param name="Means">Stores estimated mean for each variable. An exception is raised if Means Length is not equal to X1 or X2 columns.</param>
  <param name="hRes">Returns the result of the null hypothesis (default assumption is both tests variable means are equal to Means vector).</param>
  <param name="v">Returns n1+n2-2 for equal covariances, v for unequal covariance matrices.</param>
  <param name="Alpha"> Defines the desired significance level. If the significance probability (Signif)
    is bellow the desired significance (Alpha), the null hypothesis is rejected.</param>
  <param name="EqualCovariances">Assume both test have equal variances. If this is not the case, set EqualCovariances to false.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result by chance
    given that the null hypothesis is true.</param>
  <returns>Hotelling T2 Statistics for two-sample test.</returns>

  
<remarks>Performs two-sample Hotelling T2 test on samples X1 and X2. For this test the assumptions of equal variances and normally distributed residuals are used.
  Use the <see cref="MBoxTest"/> routine to check if both tests have equal variances.
</remarks>


  <SeeAlso cref="MBoxTest"/>*)
function HotellingT2Two(const X1, X2: TMtx; out Signif: double; out hRes: THypothesisResult; out v: double;
  const Means: TVec = nil; const EqualCovariances: boolean = true; const Alpha: double = 0.05): double; overload;

(*<summary>M-Box test for equal covariances.</summary>
  <param name="X1">First matrix. The number of columns for X1 and X2 must be equal, otherwise an exception is raised.</param>
  <param name="X2">Second matrix. The number of columns for X1 and X2 must be equal, otherwise an exception is raised.</param>
  <param name="df1">Nominator degrees of freedom.</param>
  <param name="df2">Denominator degrees of freedom.</param>
  <param name="Alpha"> Defines the desired significance level.</param>
  <param name="hRes">Returns the result of the null hypothesis.</param>
  <param name="Signif">(Significance level) returns the probability of observing the given result</param>

  
<remarks>Performs M-Box test for equal covariances. In this case the null hypothesis is that X1 and X2 covariances are equal and the alternative hypothesis is
  that X1 and X2 covariances are not equal.
</remarks>


  

  <example>
  Suppose we have two matrices, representing two tests with 5 samples x 3 variables. We want to test if
    two test matrices have the same covariances. Performing M-Box test with default significance level 5% will give us an answer.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
       int df1, df2;
       Matrix X1 = new Matrix(0, 0);
       Matrix X2 = new Matrix(0, 0);
       X1.SetIt(5, 3, false, new double[] { 23, 45, 15, 40, 85, 18, 215, 307, 60, 110, 110, 50, 65, 105, 24 });
       X2.SetIt(5, 3, false, new double[] { 277, 230, 63, 153, 80, 29, 306, 440, 105, 252, 350, 175, 143, 205, 42 });
       THypothesisResult hres;
       double sign;
       double MB = Statistics.MBoxTest(X1, X2, out sign, out hres, out df1, out df2, 0.05);

      // MB : 27,16221062
      // Sign : 0,01619810
      // Sign &lt; Alpha meaning hres = hrReject i.e. covariance matrices are significantly different.
    }
  }
  </code></example>

  <SeeAlso cref="HotellingT2One"/>*)
function MBoxTest(const X1,X2: TMtx; out Signif: double; out hRes: THypothesisResult;
                  out df1, df2: Integer; const Alpha: double = 0.05): double;

(*<summary>The stress factor for multidimensional scaling.</summary>
  <param name="D">Data distance matrix.</param>
  <param name="DHat">Estimated distance matrix.</param>
  <returns>The stress factor.</returns>

  
<remarks>Calculates the GOF statistics for multidimensional scaling. The stress factor is defined as:<para/>

  <IMG name="statistics002"/><para/>
  where hat(d(i,j)) is the predicted  distance based on the MDS model. In his original paper on multi dimensional scaling, Kruskal (1964)
  gave following advise about stress values based on his experience:

  <list type="table">
  <listheader>
  <term>Stress</term> <term>Goodness-of-fit</term>
  </listheader>
  <item> <term> 0.2 </term>	<term> poor </term> </item>
  <item> <term> 0.1	</term>	<term>  fair </term> </item>
  <item> <term> 0.05 </term>	<term>  good </term> </item>
  <item> <term> 0.025	</term>	<term> excellent </term> </item>
  <item> <term> 0.000	</term>	<term> perfect </term> </item>
   </list>

  More recent articles caution against using a table like this since acceptable values of stress depends
  on the quality of the distance matrix and the number of objects in that matrix.
</remarks>


  

  <example>
    D stores data distance matrix, DHat stores reduced dimension estimated distance matrix.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Matrix D = new Matrix(2, 2);
        Matrix DHat = new Matrix(2, 2);

        D.SetIt(2, 2, false, new double[4] { 0.3, 3, 2, 0.1 });
        DHat.SetIt(2, 2, false, new double[4] { 0.1, 3.1, 2.2, 0.3 });

        // Calculate stress value - measure for GOF
        // Smaller stress value means better GOF.
        double stress = Statistics.MDScaleStress(D, DHat);
    }
  }
  </code></example>

  <SeeAlso cref="MDScaleMetric"/>*)
function MDScaleStress(const D, DHat: TMtx): double;

(*<summary>Classical multidimensional scaling.</summary>
  <param name="D">Distance matrix.</param>
  <param name="Y">Returns the coordinates of object in reduced space.></param>
  <param name="EigenValues"></param>
  <param name="NumDim">Defines number of dimensions/variables to use in classical
    MD scaling algorithm.</param>

  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix X = new Matrix(0,0);
      Matrix Y = new Matrix(0,0);
      Matrix D = new Matrix(0,0);
      Matrix DHat = new Matrix(0,0);
      Vector eigen = new Vector(0);
      X.SetIt(5,2,false,new double[] {1,2,3,4,3,11,7,8,9,4});
      // Use all dimensions i.e. 2
      Statistics.PairwiseDistance(X,D,2,TPWDistMethod.pwdistEuclidian);
      // Reduce to just one variable (1d subspace)
      Statistics.MDScaleMetric(D,Y,eigen,1);
      // Calculate estimated distance matrix
      Statistics.PairwiseDistance(Y,DHat,1,TPWDistMethod.pwdistEuclidian);
      // Calculate stress - measure of GOF
      double stress = Statistics.MDScaleStress(D,DHat);
      // if stress &gt; 0.2, the GOF is poor.
    }
  }
  </code></example>

  <SeeAlso cref="MDScaleStress"/>*)
procedure MDScaleMetric(const D: TMtx; const Y: TMtx; const EigenValues: TVec; const NumDim: Integer);

(*<summary>Pairwise distance.</summary>
  
<remarks>Calculates the pairwise distance for variables, stored in X.
</remarks>


  

  <example>
    Suppose we have collected 5 data points with 3 variables. The data matrix will look like:
    <code>
    X=[2,1,3,
      2,3,1,
      4,5,6,
      6,6,6
      7,5,3]
    </code>
    Generated distance matrix has zero diagonal and is always symmetric.
    To calculate Euclidian distance between each pair of points, we use the following code:
    <code>
      Statistics.PairWiseDistance(X,D,3); // 3 dimensions for 3 variables
    </code></example>

  <SeeAlso cref="MDScaleStress"/>
  <SeeAlso cref="MDScaleMetric"/>*)
procedure PairwiseDistance(const X: TMtx; const D: TMtx; p: Integer; const Method: TPWDistMethod=pwdistEuclidian); overload;

(*<summary>The Cronbach Alpha coefficient.</summary>
  <returns>the Cronbach Alpha coefficient, defined by the following relation:

  <IMG name="statistics001"/><para/>
  where K defines number of items (Data columns) and sigma(i,j) estimated covariance between item i and j.</returns>

  
<remarks>Note
    Cronbach's Alpha measures how well a set of items (or variables) measures a single unidimensional latent construct.
    When data have a multidimensional structure, Cronbach's Alpha will usually be low. Technically speaking, Cronbach's
    Alpha is not a statistical test - it is a coefficient of reliability (or consistency).
</remarks>
*)
function CronbachAlpha(const Data: TMtx): double;

(*<summary>Remove matrix row.</summary>
  
<remarks>Removes index-th row from Src matrix and stores the result in Dst matrix. Size and complex properties of Dst matrix are
  adjusted automatically.
</remarks>


  <SeeAlso cref="RemoveColumn"/>*)
procedure RemoveRow(const Src,Dst: TMtx; const Index: Integer);

(*<summary>Remove matrix column.</summary>
  
<remarks>Removes index-th column from Src matrix and stores the result in Dst matrix. Size and complex properties of Dst matrix are
  adjusted automatically.
</remarks>


  <SeeAlso cref="RemoveRow"/>*)
procedure RemoveColumn(const Src,Dst: TMtx; const Index: Integer);

(*<summary>Orthogonal rotation of matrix.</summary>
  <param name="X">Defines original matrix (to be rotated).</param>
  <param name="Y">Stores rotated matrix (X*R, where R is calculated rotation matrix).</param>
  <param name="Gamma"> Defines different types of Varimax rotation (see above).</param>
  <param name="Normalize">If true, X matrix is normalized (by rows) prior to rotation. After the rotation the result is
    then renormalized.</param>
  <param name="Tolerance">Convergence tolerance in iteration algorithm.</param>
  <param name="MaxIter">Maximum number of rotations. Together with Tolerance parameter it defines
    convergence criteria.</param>
  <param name="R">Returns the rotation matrix, used for calculating Y=X*R.</param>
  <returns>true, if number of rotation did not exceed maximum number of iterations for rotation.</returns>

  
<remarks>Performs orthogonal rotation of X matrix. Based on the Gamma parameter value, the following rotations can be performed:
  <list type="bullet">
  <item> Gamma = 1 => Varimax rotation   </item>
  <item> Gamma &lt;&gt; 1 => Orthomax rotation </item>
  <item> Gamma = X.Cols div 2 => Equimax rotation </item>
  <item> Gamma = 0 => Quartimax rotation </item>
  </list>
</remarks>


  <SeeAlso cref="PCA"/>*)
function OrthogonalRotation(const X,Y: TMtx; const R: TMtx; const Gamma: double = 1.0; const Normalize: boolean = true; const Tolerance: double = SQRTEPS;
  const MaxIter: Integer = 200): boolean;

(*<summary>Latin Hyper-Cube design.</summary>
  <param name="X">Stores experimental design values (rows representing values, columns variables).</param>
  <param name="n">Defines the number of values for specific variable.</param>
  <param name="p">Defines the number of variables.</param>
  <param name="smooth">If true, generate random values for n random (permuted) values are distributed
    per one point in intervals <c>(0,1/n),(1/n,2/n)... ((n-1)/n,n)</c>. If false, produce the points in the
    middle of define intervals i.e. <c>0.5/n, 1.5/n, ...</c> .</param>
  <param name="IterCriteria">The criteria for design improvement.</param>
  <param name="MaxIter">Maximum number of iterations used for improving the design.</param>
  <param name="fp">Floating point precision to be used for the design.</param>

  
<remarks>Generates n x p samples for Latin Hypercube experimental design.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix e = new Matrix(0,0);
      Statistics.LatinHyperCubeDesign(e,50,3,mvDouble,true,TLHCImprove.lhcImproveMinDist, 4);
    }
  }
  </code></example>*)
procedure LatinHyperCubeDesign(const X: TMtx; const n,p: Integer; const fp: TMtxFloatPrecision; const smooth: boolean = true; const IterCriteria: TLHCImprove = lhcImproveMinDist; const MaxIter: Integer = 4);

function BetaLike(const Pars: TVec; Const Con: TVec; Const PConst: Array of TObject; var Sigma: TTwoElmReal): double; overload;
function BetaLike(const Pars: TVec; Const Con: TVec; Const OConst: Array of TObject): double; overload;

function MannWCDF1(size1, size2: Integer; x: double): double; overload;











const hhStatVersion = 'Stats Master v6.2.3';
         StatAuthor = '(c) 1999-2024 by Dew Research';


