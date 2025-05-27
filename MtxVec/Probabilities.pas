










(*<summary>
   Probabilities distribution functions.
   </summary>
   <remarks>
   This unit introduces over 40 commonly used probability
   distributions.

   <b>Discrete and Continuous distributions</b>

   A random variable is discrete if it can assume at most a
   finite or countably infinite number of possible values. A
   random variable is continuous if it can assume any value in
   some interval or intervals of real numbers and the
   probability that it assumes any specific value is zero.

   <b>PDF, CDF and inverse CDF</b>

   When dealing with the random variable, it is not enough just
   to determine what values are possible. There is also a need
   to determine what is probable. We must be able to predict in
   some sense the values that the variable is likely to assume
   at any time. Since the behavior of a random variable is
   governed by chance, these predictions must be made in the
   face of a great deal of uncertainty. The best that can be
   done is to describe the behavior of the random variable in
   terms of probabilities. Two functions are used to accomplish
   this, <i>probability density functions</i> and <i>cumulative
   distribution functions</i>.

   <b>Probability density function (PDF)</b>

   <u>Discrete distributions:</u> There are several facts to
   \note concerning the PDF in the discrete case. First, the PDF
   is defined on the entire real line, and for any given real
   number x, f(x) is the probability that the random variable X
   assumes the value x. The necessary and sufficient conditions
   for a function f to be a PDF are:

   <img name="probabilities044" />

   Even though a discrete density is defined on the entire real
   line, it is only necessary to specify the density for those
   values x for which <i>f(x)</i> is not zero.

   <u>Continuous distributions:</u> The PDF is defined on an
   entire real line and is nonnegative. In the continuous case
   the probability that the random variable X assumes the value
   x is 0. Therefore, we are interested in finding the
   probability that random variable X assumes values in some
   interval [a,b]. The necessary conditions for a function f to
   be a PDF are:

   <img name="probabilities046" />

   In Probabilities.pas all probability density functions have a
   PDF at the end of their name.

   <b>Cumulative distribution function (CDF)</b>

   <u>Discrete distributions:</u> The word &quot;cumulative&quot;
   suggests the role of the CDF function. It sums or accumulates
   the probabilities found by means of PDF. The cumulative
   distribution function F(x) is defined as follows:

   <img name="probabilities045" />

   We must sum the PDF over all values of X that occur with
   nonzero probability that are less than or equal to x.

   <u>Continuous distributions:</u> In continuous case is the
   cumulative distribution function - CDF defined exactly as in
   the discrete case although found by using integration rather
   than summation. The cumulative distribution function F(X) is
   defined as follows:

   <img name="probabilities047" />

   In this case, to find a specific F(x), we must integrate the
   density over all real numbers that are less than or equal to
   x. In Probabilities.pas all cumulative distribution functions
   have a CDF at the end of their name.

   <b>Getting the &quot;original values&quot; from given
   probability (inverse CDF)</b>

   Generally, the &quot;inverse&quot; cumulative distributions
   are used to &quot;get&quot; the original values that were
   used for calculating the CDF. If the distribution is
   continuous, the result of the inverse CDF is the original
   value. If the distribution is discrete, the result is
   generally not the exact original value.

   <u>Discrete distributions:</u> As stated above, the
   relationship between a discrete CDF and its inverse function
   is more complicated. It is likely that there is no such value
   x that the CDF of x gives the desired probability. In these
   cases the inverse function returns the first value x such
   that the CDF of that value equals or exceeds the desired
   probability.

   <u>Continuous distributions:</u> Calculating the CDF of
   values gives the probabilities in the interval [0,1]. If we
   want the original values that were used for the CDF (x), we
   must apply the inverse cumulative distribution. The result is
   the original value used to calculate CDF. In
   Probabilities.pas all inverse cumulative distribution
   functions have a CDFInv at the end of their name.
   </remarks>*)
unit Probabilities;


{$I BdsppDefs.inc}

interface


uses Math387
    
    , MtxVec, AbstractMtxVec, AbstractMtxVecInt, MtxVecInt, MtxVecBase
    
    
    ,SysUtils
    
    ;


type
  (*<summary>All supported probability distributions.</summary>*)
  TDistribution = (
  (*<summary>Beta distribution</summary>*)distBETA,
  (*<summary>Bernoulli distribution</summary>*)distBERNOULLI,
  (*<summary>Binomial distribution</summary>*)distBINOM,
  (*<summary>Bose-Einstein distribution</summary>*)distBOSEEINSTEIN,
  (*<summary>Cauchy distribution</summary>*)distCAUCHY,
  (*<summary>Chi-Squared distribution</summary>*)distCHISQUARE,
  (*<summary>Erlang distribution</summary>*)distERLANG,
  (*<summary>Exponential distribution</summary>*)distEXP,
  (*<summary>Fisher(F) distribution</summary>*)distF,
  (*<summary>Fatigue-Life distribution</summary>*)distFATIGUE,
  (*<summary>Fermi-Dirac distribution</summary>*)distFERMIDIRAC,
  (*<summary>Gamma distribution</summary>*)distGAMMA,
  (*<summary>Generalized Extreme value distribution</summary>*)distGenExtValue,
  (*<summary>Generalized Pareto distribution</summary>*)distGenPareto,
  (*<summary>Geometric distribution</summary>*)distGEOMETRIC,
  (*<summary>Gumbel maximum distribution</summary>*)distGUMBELMAX,
  (*<summary>Gumbel minimum distribution</summary>*)distGUMBELMIN,
  (*<summary>Hypergeometric distribution</summary>*)distHYPGEOMETRIC,
  (*<summary>Inverse Chi-Squared distribution</summary>*)distINVChi2,
  (*<summary>Inverse Gaussian distribution</summary>*)distINVGAUSS,
  (*<summary>Johnson bounded distribution</summary>*)distJOHNSONSB,
  (*<summary>Johnson unbounded distribution</summary>*)distJOHNSONUB,
  (*<summary>Laplace distribution</summary>*)distLAPLACE,
  (*<summary>Logarithmic distribution</summary>*)distLOGARITHMIC,
  (*<summary>Logistic distribution</summary>*)distLOGISTIC,
  (*<summary>Log-Normal distribution</summary>*)distLOGNORMAL,
  (*<summary>Log-Weibull distribution</summary>*)distLOGWEIBULL,
  (*<summary>Maxwell distribution</summary>*)distMAXWELL,
  (*<summary>Negative binomial distribution</summary>*)distNEGBINOM,
  (*<summary>Normal distribution</summary>*)distNORMAL,
  (*<summary>Pareto distribution</summary>*)distPARETO,
  (*<summary>Poisson distribution</summary>*)distPOISSON,
  (*<summary>Power distribution</summary>*)distPOWER,
  (*<summary>Rayleigh distribution</summary>*)distRAYLEIGH,
  (*<summary>Student(T) distribution</summary>*)distSTUDENT,
  (*<summary>Triangular distribution</summary>*)distTRIANGULAR,
  (*<summary>Uniform distribution</summary>*)distUNIFORM,
  (*<summary>Discrete uniform distribution</summary>*)distUNIFORMD,
  (*<summary>Weibull distribution</summary>*)distWEIBULL,
  (*<summary>Zipf distribution</summary>*)distZIPF);



(*<summary>Binomial coefficient C(n,k).</summary>
  <returns>the binomial coefficient <sub>n</sub>C<sub>k</sub>.</returns>
  <param name="N">n in <sub>n</sub>C<sub>k</sub>.</param>
  <param name="K">k in <sub>n</sub>C<sub>k</sub>.</param>
  
<remarks>Binomial coefficient <sub>n</sub>C<sub>k</sub> is the number of ways of picking <c>k</c> unordered
  outcomes from <c>n</c> possibilities, also known as a combination or combinatorial number.
  The binomial coefficient is defined by the following equation:

  <img name="probabilities127"/>
</remarks>
*)

function Binomial(N, K: integer): double; overload;


(*<summary>Beta function B.</summary>
  <returns>beta function B for given parameters x and y.</returns>
  <param name="x">x in Beta function definition.</param>
  <param name="y">y in Beta function definition.</param>
  
<remarks>The beta function is defined by the following equation:

  <img name="probabilities001"/>
</remarks>


  <seealso cref="IGamma"/>*)

function Beta(x, y: double): double; overload;



(*<summary>Incomplete beta function.</summary>
  <returns>incomplete beta function for given parameters x, a and b.</returns>

  
<remarks>The incomplete beta function is defined by the following equation:

  <IMG name="probabilities005"/>,

  where B(a,b) is <see cref="Beta"/> function.
</remarks>

  <SeeAlso cref="Beta"/>*)

function BetaIncomplete(x,a,b: double): double; overload;


(*<summary>Dirichlet Beta function.</summary>
  <returns>the Dirichlet Beta function.</returns>

  
<remarks>The Dirichlet beta function, defined for real values can be written as:

  <IMG name="probabilities125"/>,

  It can be extended over a whole complex plane by the following relation:

  <IMG name="probabilities126"/>,

  where Gamma denotes the <see cref="Math387.Gamma"/> function.
</remarks>


  <SeeAlso cref="DirichletLambda"/>
  <SeeAlso cref="DirichletEta"/>
  <SeeAlso cref="RiemannZeta"/>*)

function DirichletBeta(const Z: TCplx): TCplx; overload;

(*<summary>Dirichlet Lambda function.</summary>
  <returns>the Dirichlet Lambda function.</returns>
  <param name="Z">Defines complex value for which the Lambda function is to be calculated.</param>
  <param name="n">Defines used number of terms used in complex series approximation of Lambda function.</param>
  
<remarks>Dirichlet Lambda function is defined by the following equation:

  <IMG name="probabilities131"/>,
</remarks>


  <SeeAlso cref="DirichletEta"/>
  <SeeAlso cref="RiemannZeta"/>*)

function DirichletLambda(const Z: TCplx; n: Integer= 64): TCplx; overload;

(*<summary>Dirichlet Eta function.</summary>
  <returns>the Dirichlet Eta function.</returns>
  <param name="Z">Defines complex value for which the Eta function is to be calculated.</param>
  <param name="n">Defines used number of terms used in complex series approximation of Eta function.</param>

  
<remarks>Dirichlet eta function is defined by the following equation:

  <IMG name="probabilities130"/>,
</remarks>


  <SeeAlso cref="DirichletLambda"/>
  <SeeAlso cref="RiemannZeta"/>*)

function DirichletEta(const Z: TCplx; n: Integer = 64 ): TCplx; overload;


(*<summary>Riemann Zeta function.</summary>
  <returns>the Riemman zeta function.</returns>
  <param name="Z">Defines complex value for which the Riemann zetao function is to be calculated.</param>
  <param name="n">Defines used number of terms used in complex series approximation of Riemann zeta function.</param>

  
<remarks>The Riemann zeta-function is the function of a complex variable z initially defined by
  the following infinite series:

  <IMG name="probabilities128"/>,

  for values of z with real part greater than one, and then analytically continued to
  all complex z &lt;&gt; 1. The zeta-function satisfies the following functional equation:

  <IMG name="probabilities129"/>,

  valid for all complex numbers z. Zeta function is related to Dirichlet Lambda and Eta functions by:

  <IMG name="probabilities132"/>
</remarks>


  <SeeAlso cref="DirichletLambda"/>
  <SeeAlso cref="DirichletEta"/>
  <SeeAlso cref="DirichletBeta"/>*)

function RiemannZeta(const Z: TCplx; n: Integer = 64 ): TCplx; overload;


(*<summary>Lerch transcendent function.</summary>
  <returns>the an approximation to Lerch transcendent function.</returns>
  
<remarks>Estimates Lerch transcendent function. The Lerch transcendent function is defined as:

  <IMG name="probabilities077"/>
</remarks>
*)

function Lerch(const z: double; s: double; a: double): double; overload;












(*<summary>Generalized harmonic number H(n,m).</summary>
  <returns>the Generalized harmonic number (n).</returns>
  
<remarks>The Generalized harmonic number is defined by the following equation:

  <IMG name="probabilities118"/>
</remarks>


  <SeeAlso cref="Harmonic"/>*)

function GenHarmonic(n: Integer; m: double): double; overload;

(*<summary>Harmonic number H(n).</summary>
  <returns>the Harmonic number H(n).</returns>
  <SeeAlso cref="GenHarmonic"/>*)

function Harmonic(n: Integer): double; overload;




(*<summary>Beta probability density function (PDF).</summary>
  <returns>the beta distribution probability density function (PDF).</returns>
  <param name="x">Function domain, real positive value on closed interval [0,1].</param>
  <param name="a">Shape parameter, real positive value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>Calculates the beta probability density function. The beta probability density function is defined by the following equation:

  <IMG name="probabilities004"/>

  where B(a,b) is <see cref="Beta"/> function and it defines the interval on which the beta PDF is not zero.
  The beta distribution describes a family of curves that are nonzero only on the interval [0,1].
  The parameters a and b must both be greater than zero and x must lie on the interval (0,1) otherwise the result is NAN.
</remarks>


  

  <example>
    Calculate Beta distribution (a=3 and b= 2.1) PDF and CDF for x =0.55<code>
  using Dew.Probability;
  using Dew.Probability.Units;

  namespace Dew.Examples
  {
    void Example()
    {
      double pdf = BetaPDF(0.55, 3.0, 2.1);
      double cdf = BetaCDF(0.55, 3.0, 2.1);
    }
  }
  </code></example>

  <SeeAlso cref="BetaCDF"/>
  <SeeAlso cref="BetaCDFInv"/>*)

function BetaPDF(x, a, b: double): double; overload;

(*<summary>Beta cumulative distribution function (CDF).</summary>
  <returns>the  beta cumulative distribution function (CDF), which the probability that a single observation from a beta
  distribution with parameters a and b will fall in the interval [0,x].</returns>
  <param name="x">Function domain, real positive value on closed interval [0,1].</param>
  <param name="a">Shape parameter, real positive value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>The beta cumulative distribution function is defined by the following equation:

  <IMG name="probabilities002"/>

  where B(a,b) is <see cref="Beta"/> function.
</remarks>


  <seealso cref="BetaPDF"/>
  <seealso cref="BetaCDFInv"/>*)

function BetaCDF(x, a, b: double): double; overload;

(*<summary>Beta distribution point percent function (PPF).</summary>
  <returns>the Beta distribution point percent function (PPF) for given parameters a and b.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Shape parameter, real positive value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>The inverse beta cumulative distribution function is defined by the following equation:

  <IMG name="probabilities003"/>

  where B(a,b) is <see cref="Beta"/> function.
</remarks>

  <SeeAlso cref="BetaPDF"/>
  <SeeAlso cref="BetaCDF"/>*)

function BetaCDFInv(p, a, b: double): double; overload;


(*<summary>Bernoulli probability density function (PDF).</summary>
  <returns>the Bernoulli distribution probability density function (PDF).</returns>
  <param name="x">Function domain, integer, valid values 0 or 1.</param>
  <param name="p">parameter, real positive value on closed intervali [0,1].</param>

  
<remarks>Calculates the Bernoulli probability distribution function. The Bernoulli probability density function is defined by the following
  equation:

  <IMG name="probabilities115"/>
</remarks>


  <SeeAlso cref="BernoulliCDF"/>
  <SeeAlso cref="BernoulliCDFInv"/>
  <SeeAlso cref="BinomPDF"/>*)

function BernoulliPDF(x: integer; p: double): double; overload;

(*<summary>Bernoulli cumulative distribution function (CDF).</summary>
  <returns>Bernoulli cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, integer, valid values 0 or 1.</param>
  <param name="p">parameter, real positive value on closed intervali [0,1].</param>

  
<remarks>Calculates the Bernoulli czumulative density function. The Bernoulli probability density function is defined by the following equation:

  <IMG name="probabilities114"/>
</remarks>


  <SeeAlso cref="BernoulliPDF"/>
  <SeeAlso cref="BernoulliCDFInv"/>
  <SeeAlso cref="BinomCDF"/>*)

function BernoulliCDF(x: integer; p: double): double; overload;


(*<summary>Bernoulli distribution point percent function (PPF).</summary>
  <returns>the Bernoulli distribution point percent function (PPF)</returns>
  <param name="y">Probability, real positive value on closed interval [0,1].</param>
  <param name="p">parameter, real positive value on closed intervali [0,1].</param>

  <SeeAlso cref="BernoulliCDF"/>
  <SeeAlso cref="BinomCDFInv"/>*)

function BernoulliCDFInv(y: double; p: double): double; overload;


(*<summary>Binomial probability density function (PDF).</summary>
  <returns>the binomial probability density function (PDF).</returns>
  <param name="x">Function domain, integer, valid on closed interval [0,n].</param>
  <param name="N">Defines number of trials. n must be a positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  <remarks>
  Calculates the binomial probability density function. The binomial probability density function is defined by the following equation:

  <IMG name="probabilities007"/>

  where <c>q=1-p</c> and I is the discrete interval [0,1,...n] on which the binomial CDF is not zero. The result of BinomPDF is the
  probability of observing x successes in n independent trials and  where the probability of success in any given trial is p.
  
  To recognize a situation that involves a binomial random variable, following assumptions must be met:
  * The experiment consists of a fixed number, n, of Bernoulli trials that result in either success or failure.
  * The trials are identical and independent and therefore the probability of success p, remains the same from trial to trial.
  * The random variable x denotes the number of successes obtained in the n trials.
  </remarks>
  
  <seealso cref="BinomCDF"/>
  <seealso cref="BinomCDFInv"/>
  <seealso cref="BernoulliPDF"/>*)

function BinomPDF(x: Integer; N: integer; p: double): double; overload;


(*<summary>Binomial cumulative distribution function (CDF).</summary>
  <returns>the binomial cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, integer, valid on closed interval [0,n].</param>
  <param name="N">Defines number of trials. n must be a positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>The binomial cumulative distribution function is defined by the following equation:

  <IMG name="probabilities006"/>

  where <c>q=1-p</c> and I is the discrete interval [0,1,...n] on which the binomial CDF is
  not zero. The result of BinomCDF is the probability of observing up to x successes in n
  independent trials and  where the probability of success in any given trial is p.
</remarks>


  <seealso cref="BinomPDF"/>
  <seealso cref="BinomCDFInv"/>
  <seealso cref="BernoulliCDF"/>*)

function BinomCDF(x: Integer; N: integer; p: double): double; overload;


(*<summary>Binomial distribution point percent function (PPF).</summary>
  <returns>the binomial distribution point percent function (PPF).</returns>
  <param name="y">Probability, real positive value on closed interval [0,1].</param>
  <param name="N">Defines number of trials. n must be a positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>The result of BinomCDFInv is the smallest integer such that the binomial cdf evaluated at the result is equal
  to or exceeds y. The y is the probability of observing result successes in n independent trials where p is the
  probability of success in each trial.
</remarks>


  <seealso cref="BinomPDF"/>
  <seealso cref="BinomCDF"/>
  <seealso cref="BernoulliCDFInv"/>*)

function BinomCDFInv(y: double; N: integer; p: double): double; overload;


(*<summary>Bose-Einstein probability density function (PDF).</summary>
  <returns>Bose-Einstein probability density function (PDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>

  
<remarks>Calculates the Bose-Einstein PDF for x using the parameters a and s. The distribution arises in the study of integer spin particles in physics.
  Integer spin particles are described by Bose-Einstein statistics and are called BOSONS. Bose-Einstein PDF is defined by the following equation:

  <IMG name="probabilities066"/>
</remarks>


  <seealso cref="FermiDiracPDF"/>*)

function BoseEinsteinPDF(x: double; Mu, s: double): double; overload;


(*<summary>Bose-Einstein cumulative distribution function (CDF).</summary>
  <returns>Bose-Einstein cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>
  <remarks>The distribution arises in the study of integer spin particles in physics.</remarks>*)

function BoseEinsteinCDF(x: double; Mu,s: double): double; overload;


(*<summary>Cauchy probability density function (PDF).</summary>
  <returns>the Cauchy probability density function (PDF) for given parameters b and m. Parameter b must be greater than zero,
  otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>Calculates the Cauchy probability density function. The Cauchy probability density function is defined by the following equation:

  <IMG name="probabilities050"/>

  where m is the location parameter, specifying the location of the peak of the distribution, and b is the scale parameter which
  specifies the half-width at half-maximum (HWHM). As a probability distribution, it is known as the Cauchy distribution
  while among physicists it is known as the Lorentz distribution or the Breit-Wigner distribution.
</remarks>


  <seealso cref="CauchyCDF"/>
  <seealso cref="CauchyCDFInv"/>*)

function CauchyPDF(x, m, b: double): double; overload;


(*<summary>Cauchy cumulative distribution function (CDF).</summary>
  <returns>the Cauchy cumulative distribution function (CDF)</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>The Cauchy cumulative distribution function is defined by the following equation:

  <IMG name="probabilities048"/>
</remarks>


  <seealso cref="CauchyPDF"/>
  <seealso cref="CauchyCDFInv"/>*)

function CauchyCDF(x,m,b: double): double; overload;


(*<summary>Cauchy distribution point percent function (PPF).</summary>
  <returns>the Cauchy distribution point percent function (PPF) for given parameters b and m.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>

  
<remarks>The inverse Cauchy cumulative distribution function is defined by the following equation:

  <IMG name="probabilities049"/>

  The result of CauchyCDFInv is the solution of the integral equation of the Cauchy CDF with parameters
  b and m and with the desired probability p.
</remarks>


  <seealso cref="CauchyPDF"/>
  <seealso cref="CauchyCDF"/>*)

function CauchyCDFInv(p,m,b: double): double; overload;


(*<summary>Chi-squared probability density Function (PDF).</summary>
  <returns>the Chi-squared probability density Function (PDF).</returns>
  <param name="x">Function domain, real positive value.</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>

  
<remarks>Calculates the Chi-Squared probability density function, defined by the following equation:

  <IMG name="probabilities010"/>

  where n (Nu) is the degrees of freedom and G is <see cref="Math387.Gamma"/> function. One of the most important
  classes of quadratic functions in sampling theory is the class of functions
  reducible to sums of squares of Nu independent standard normals. This function is called a chi-squared function
  with Nu degrees of freedom. A chi-squared random variable is completely specified by stating its degrees of
  freedom Nu. The chi-squared distribution is just a special case of <see cref="GammaPDF"/> distribution
  (for gamma distribution parameter b=2).
</remarks>


  <seealso cref="ChiSquareCDF"/>
  <seealso cref="ChiSquareCDFInv"/>*)

function ChiSquarePDF(x: double; Nu: Integer): double; overload;


(*<summary>Chi-squared cumulative distribution Function (CDF).</summary>
  <returns>the chi-squared cumulative distribution Function (CDF). The parameter Nu (degrees of freedom)
  must be a positive integer, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real positive value.</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>

  
<remarks>The chi-squared cumulative distribution function is defined by the following equation:

  <IMG name="probabilities008"/>

  where n (Nu) is the degrees of freedom and G is gamma function.
  The result of ChiSquareCDF is the probability that a single observation from the chi-squared distribution
  with Nu degrees of freedom will fall in the interval [0,x].
</remarks>


  <seealso cref="ChiSquarePDF"/>
  <seealso cref="ChiSquareCDFInv"/>*)

function ChiSquareCDF(x: double; Nu: Integer): double; overload;


(*<summary>Chi-squared distribution point percent function (PPF).</summary>
  <returns>the Chi-squared distribution point percent function (PPF). The parameter Nu (degrees of freedom)
  must be a positive integer and the probability y must lie on the interval [0,1], otherwise the result is NAN.</returns>
  
  
<remarks>The Chi-squared PPF is defined by the following equation:

  <IMG name="probabilities009"/>
</remarks>


  <seealso cref="ChiSquarePDF"/>
  <seealso cref="ChiSquareCDF"/>*)

function ChiSquareCDFInv(p: double; Nu: Integer): double; overload;


(*<summary>Erlang probability density function (PDF).</summary>
  <returns>the Erlang probability density function (PDF) for integer parameter k and double parameter Lambda at the value x. Parameters k and
  Lambda must both be greater than zero.</returns>
  <param name="x">Function domain, real positive value or zero.</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>

  
<remarks>The Erlang probability distribution function (PDF) is defined by the following relation:

  <IMG name="probabilities081"/>

  The Erlang distribution is a continuous distribution, which has a positive value for all real numbers greater than
  zero, and is given by two parameters: the shape k, which is an integer, and the rate Lambda, which is a real.
  The distribution is sometimes defined using the inverse of the rate parameter, the scale phi.
  When the shape parameter k equals 1, the distribution simplifies to the exponential distribution.

  The Erlang distribution is a special case of the Gamma distribution where the shape parameter k is an integer.
  In the Gamma distribution, this parameter is a real.
</remarks>


  <seealso cref="ErlangCDF"/>
  <seealso cref="ErlangCDFInv"/>*)

function ErlangPDF(x:double; k: Integer; Lambda: double): double;  overload;


(*<summary>Erlang distribution cumulative distribution function (CDF).</summary>
  <returns>the Erlang cumulative distribution function (CDF) for parameters k and Lambda, evaluated at x.</returns>
  <param name="x">Function domain, real positive value or zero.</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>

  
<remarks>Calculates the Erlang cumulative distribution function, defined by the following relation:

  <IMG name="probabilities082"/>
</remarks>


  <seealso cref="ErlangCDF"/>
  <seealso cref="ErlangCDFInv"/>*)

function ErlangCDF(x:double; k: Integer; Lambda: double): double;  overload;


(*<summary>Erlang distribution point percent function (PPF).</summary>
  <returns>the Erlang distribution point percent function (PPF). Both parameters k and Lambda must be positive,
  otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>

  
<remarks>The inverse of the Erlang cumulative distribution function is defined by the following equation:

  <IMG name="probabilities082"/>
</remarks>


  <seealso cref="ErlangPDF"/>
  <seealso cref="ErlangCDF"/>*)

function ErlangCDFInv(p:double; k: Integer; Lambda: double): double;  overload;


(*<summary>Exponential probability density function (PDF).</summary>
  <returns>the exponent probability density function (PDF) for parameter Mu at the value x. Parameter Mu
  must be greater than zero, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real positive value or zero.</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>

  
<remarks>Calculates the exponential probability density function, defined by the equation:

  <IMG name="probabilities015"/>

  The exponential distribution arises often in practice with conjunction with the study of Poisson processes.
  In a Poisson process discrete events are being observed over a continuous time interval. If we let W denote
  the time of the occurrence of the first event, then W has and exponential distribution.
</remarks>


  <seealso cref="ExpCDF"/>
  <seealso cref="ExpCDFInv"/>*)

function ExpPDF(x, Mu: double): double; overload;


(*<summary>Exponential cumulative distribution function (CDF).</summary>
  <returns>the exponential cumulative distribution function (CDF) for parameter Mu at the value X. Parameter
  Mu must be greater than zero, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real positive value or zero.</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>

  
<remarks>The exponential cumulative distribution function s defined by the following equation:

  <IMG name="probabilities013"/>

  The result of ExpCDF is the probability that a single observation from an exponential distribution will
  fall in the interval [0,x].
</remarks>


  <seealso cref="ExpPDF"/>
  <seealso cref="ExpCDFInv"/>*)

function ExpCDF(x, Mu: double): double; overload;


(*<summary>Exponential distribution point percent function (PPF).</summary>
  <returns>the exponential distribution point percent function (PPF). The parameter Mu must be a positive
  real value and the probability y must lie on the interval [0,1], otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive real value.</param>

  
<remarks>The inverse exponential cumulative distribution function is defined by the following equation:

  <IMG name="probabilities014"/>

  The result of ExpCDFInv is the value such that there is a probability p that an observation from an
  exponential distribution with parameter Mu will fall in the range [0,result].
</remarks>


  <seealso cref="ExpCDF"/>
  <seealso cref="ExpPDF"/>*)

function ExpCDFInv(p, Mu: double): double; overload;


(*<summary>Fatigue life probability density function (PDF).</summary>
  <returns>the Fatigue life probability density function (PDF)evaluated at x using the parameters gamma, Mu and beta.
  Gamma and beta must be positive real numners and x>Mu, otherwise an exception is raised.</returns>
  <param name="x">Distribution domain, real value on open interval (Mu,INF).</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>

  
<remarks>Calculates the "Fatigue life" probability density function, defined by the equation:

  <IMG name="probabilities090"/>

  The fatigue life distribution is also commonly known as the Birnbaum-Saunders distribution. Please note that there are
  several alternative formulations of the fatigue life distribution in the literature.
</remarks>


  <seealso cref="FatigueLifeCDF"/>
  <seealso cref="FatigueLifeCDFInv"/>*)

function FatigueLifePDF(x: double; Mu, gamma, beta: double): double; overload;


(*<summary>Fatigue life cumulative distribution function (CDF).</summary>
  <returns>the Fatigue life CDF, evaluated at x using positive scalar parameters
  Mu, gamma and beta.</returns>
  <param name="x">Distribution domain, real value on open interval (Mu,INF).</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>

  
<remarks>The Fatigue CDF is defined by the following equation:

  <IMG name="probabilities091"/>

  where gamma>0, beta>0.
</remarks>


  <seealso cref="FatigueLifePDF"/>
  <seealso cref="FatigueLifeCDFInv"/>*)

function FatigueLifeCDF(x: double; Mu, gamma, beta: double): double; overload;


(*<summary>Fatigue life distribution point percent function (PPF).</summary>
  <returns>the "fatigue life" distribution point percent function (PPF). Parameters gamma and beta must be positive and
  the probability p must lie on the interval [0,1], otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>

  
<remarks>The inverse Fatigue life cumulative distribution function is defined by the following equation:

  <IMG name="probabilities092"/>

  The result is the value such that there is a probability p that an observation from an
  Fatigue life distribution with parameters Mu, gamma and beta will fall in the range [0 result].
</remarks>


  <seealso cref="FatigueLifePDF"/>
  <seealso cref="FatigueLifeCDF"/>*)

function FatigueLifeCDFInv(p: double; Mu, gamma, beta: double): double; overload;


(*<summary>F probability density function (PDF).</summary>
  <returns>the F probability density function (PDF) for integer parameters Nu1 and Nu2 (degrees of
  freedom) at the value x. Both Nu1 and Nu2 must be positive integer numbers, otherwise the result is NAN.</returns>
  <param name="x">Distribution domain, real positive value or zero.</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>

  
<remarks>Calculates the Fisher (F) probability density function, defined by the equation:

  <IMG name="probabilities018"/>

  where n1=Nu1 and n2=Nu2 are degrees of freedom, and G is the gamma function. The most widely applied of all
  statistical techniques, the analysis of variance, depends heavily on the comparison of mutually independent
  "sum of squares" which are proportional to the chi-squared variables. The F distribution can  defined in terms
  of a chi-squared distribution.

  In particular, any F random variable can be written as the ration of two independent chi-squared
  random variables, each divided by their respective degrees of freedom.
</remarks>


  <seealso cref="FCDF"/>
  <seealso cref="FCDFInv"/>*)

function FPDF(x: double; Nu1, Nu2: Integer): double; overload;


(*<summary>F cumulative distribution function (CDF).</summary>
  <returns>the F cumulative distribution function (CDF) for integer parameters Nu1 and Nu2 at the value
  X. Both Nu1 and Nu2 must be positive integer numbers, otherwise the result is NAN.</returns>
  <param name="x">Distribution domain, real positive value or zero.</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>

  
<remarks>The F cumulative distribution function is defined by the following equation:

  <IMG name="probabilities016"/>

  where n1=Nu1 and n2=Nu2 are degrees of freedom, and Gamma is the <see cref="Math387.Gamma"/>
  function. The result of FCDF is the probability that a single observation from an F distribution with parameters Nu1 and
  Nu2 will fall in the interval [0,x].
</remarks>


  <seealso cref="FPDF"/>
  <seealso cref="FCDFInv"/>*)

function FCDF(x: double; Nu1, Nu2: Integer): double; overload;


(*<summary>F distribution point percent function (PPF).</summary>
  <returns>the Fisher(F) distribution point percent function (PPF) for integer parameters Nu1 and Nu2
  (degrees of freedom) at the probability y. Both Nu1 and Nu2 must be positive integer numbers and the
  probability p must lie on the interval [0,1], otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>

  
<remarks>The inverse F cumulative distribution function is defined by the following equation:

  <IMG name="probabilities017"/>
</remarks>


  <seealso cref="FPDF"/>
  <seealso cref="FCDF"/>*)

function FCDFInv(p: double; Nu1, Nu2 : Integer): double; overload;


(*<summary>Fermi-Dirac probability density function (PDF).</summary>
  <returns>Fermi-Dirac probability density function (PDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>

  
<remarks>Calculates the Fermi-Dirac PDF for x using the parameters Mu and s. The distribution arises in the study of half-integer spin particles in physics.
  Half-integer spin particles are described by Fermi-Dirac statistics and are called FERMIONS. Fermi-Dirac PDF is defined by the following equation:

  <IMG name="probabilities067"/>
</remarks>



  <seealso cref="BoseEinsteinPDF"/>*)

function FermiDiracPDF(x: double; Mu, s: double): double; overload;


(*<summary>Fermi-Dirac cumulative distribution function (CDF).</summary>
  <returns>Fermi-Dirac cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>

  <remarks>The distribution arises in the study of integer spin particles in physics.</remarks>*)

function FermiDiracCDF(x: double; Mu, s: double): double; overload;


(*<summary>Gamma probability density function (PDF).</summary>
  <returns>the Gamma probability density function (PDF) for value x using the parameters a and b.</returns>
  <param name="x">Distribution domain, real positive value.</param>
  <param name="a">Defines distribution scale parameter. a must be a positive scalar.</param>
  <param name="b">Defines distribution shape parameter. b must be a positive scalar.</param>

  
<remarks>Calculates the Gamma distribution probability density function, defined by the equation:

  <IMG name="probabilities021"/>

  where Gamma denotes the <see cref="Math387.Gamma"/> function. The Gamma distribution is especially important in
  that it allows us to define two families of random variables, the exponential and  chi-squared, that
  are used extensively in applied statistics. Gamma probability density function can be useful in reliability
  models of lifetimes.The gamma distribution is more flexible than the exponential in that the probability
  of surviving an additional period may depend on age.
</remarks>


  <seealso cref="GammaCDF"/>
  <seealso cref="GammaCDFInv"/>*)

function GammaPDF(x, a, b: double): double;  overload;


(*<summary>Gamma cumulative distribution function (CDF).</summary>
  <returns>the Gamma cumulative distribution function (CDF) for value x using the parameters a and b.
  Both parameters a and b must be positive, otherwise the result is NAN.</returns>
  <param name="x">Distribution domain, real positive value.</param>
  <param name="a">Defines distribution scale parameter. a must be a positive scalar.</param>
  <param name="b">Defines distribution shape parameter. b must be a positive scalar.</param>

  
<remarks>The gamma cumulative distribution is defined by the following equation:

  <IMG name="probabilities019"/>

  where Gamma denotes the gamma function. The result is the probability that a single observation from a gamma distribution with
  parameters a and b will fall in the interval [0,x].
</remarks>


  <seealso cref="GammaPDF"/>
  <seealso cref="GammaCDFInv"/>*)

function GammaCDF(x, a, b: double): double; overload;


(*<summary>Gamma distribution point percent function (PPF).</summary>
  <returns>the Gamma distribution point percent function (PPF) for probability y using
  the parameters a and b. Both parameters a and b must be positive, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Defines distribution scale parameter. a must be a positive scalar.</param>
  <param name="b">Defines distribution shape parameter. b must be a positive scalar.</param>

  
<remarks>The inverse gamma cumulative distribution function is defined by the following equation:

  <IMG name="probabilities020"/>

  where Gamma denotes the <see cref="Math387.Gamma"/> function.
</remarks>


  <seealso cref="GammaPDF"/>
  <seealso cref="GammaCDF"/>*)

function GammaCDFInv(p, a, b: double): double; overload;


(*<summary>Generalized extreme value probability density function (PDF).</summary>
  <returns>the Generalized extreme value probability density function (PDF) for value x using the parameters k,
  Mu and sigma.</returns>
  <param name="x">Distribution domain, real value, valid if the relation <c>k*(x-Mu)/sigma >-1</c></param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  
<remarks>Calculates the Generalized extreme value distribution probability density function, defined by the equation:

  <IMG name="probabilities122"/>
</remarks>


  <seealso cref="GenExtValueCDF"/>
  <seealso cref="GenExtValueCDFInv"/>*)

function GenExtValuePDF(x, k, Mu, sigma: double): double; overload;


(*<summary>Generalized extreme value cumulative distribution function (CDF).</summary>
  <returns>the Generalized extreme value cumulative distribution function (CDF) for value x using the parameters k, Mu and sigma.</returns>
  <param name="x">Distribution domain, real value, valid if the relation <c>k*(x-Mu)/sigma >-1</c></param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  
<remarks>The Generalized Extreme Value distribution CDF is defined by the following equation:

  <IMG name="probabilities121"/>
</remarks>


  <seealso cref="GenExtValuePDF"/>
  <seealso cref="GenExtValueCDFInv"/>*)

function GenExtValueCDF(x, k, Mu, sigma: double): double; overload;


(*<summary>Generalized extreme value distribution point percent function (PPF).</summary>
  <returns>the Generalized extreme value distribution point percent function (PPF) for probability y using
  the parameters k,  Mu and sigma.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  <seealso cref="GenExtValuePDF"/>
  <seealso cref="GenExtValueCDF"/>*)

function GenExtValueCDFInv(p, k, Mu, sigma: double): double; overload;


(*<summary>Generalized Pareto probability density function (PDF).</summary>
  <returns>the Generalized Pareto probability density function (PDF) for value x using the parameters a and b. Both
  parameters k, Mu and sigma.</returns>
  <param name="x">Distribution domain, real value, if <c>k &gt;= 0</c> valid on open interval (Mu,Inf),
  if <c>k &lt; 0</c>, valid if <c>0 &lt;= (x-Mu)/sigma &lt;= -1/k</c>.</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  
<remarks>Calculates the Generalized Pareto distribution probability density function, defined by the equation:

  <IMG name="probabilities124"/>
</remarks>


  <seealso cref="GenParetoCDF"/>
  <seealso cref="GenParetoCDFInv"/>*)

function GenParetoPDF(x, k, Mu, sigma: double): double; overload;


(*<summary>Generalized Pareto cumulative distribution function (CDF).</summary>
  <returns>the Generalized Pareto cumulative distribution function (CDF) for value x using the parameters k, Mu and sigma.</returns>
  <param name="x">Distribution domain, real value, if <c>k &gt;= 0</c> valid on open interval (Mu,Inf),
  if <c>k &lt; 0</c>, valid if <c>0 &lt;= (x-Mu)/sigma &lt;= -1/k</c>.</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  
<remarks>Calculates the Generalized Pareto distribution CDF, defined by the equation:

  <IMG name="probabilities123"/>
</remarks>


  <seealso cref="GenParetoPDF"/>
  <seealso cref="GenParetoCDFInv"/>*)

function GenParetoCDF(x, k, Mu, sigma: double): double; overload;


(*<summary>Generalized Pareto distribution point percent function (PPF).</summary>
  <returns>the Generalized extreme value distribution point percent function (PPF) for probability y using
  the parameters k, Mu and sigma.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  <seealso cref="GenParetoPDF"/>
  <seealso cref="GenParetoCDF"/>*)

function GenParetoCDFInv(p, k, Mu, sigma: double): double; overload;


(*<summary>Geometric probability density function (PDF).</summary>
  <returns>the geometric probability density function (PDF) for value x by using parameter p
  (probability).</returns>
  <param name="x">Function domain, positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>Calculates the geometric distribution probability density function, defined by the following equation:

  <IMG name="probabilities023"/>

  where I is the discrete interval on which the geometric PDF is not zero. To recognize a situation that involves
  a geometrical random variable, following assumptions must be met:

  * The experiment consist of series of trials. The outcome of each trial can be classed either as success or
    a failure. A trial with this property is called a Bernoulli trial.
  * The trials are identical and independent in the sense that the outcome of one trial has no effect on
    the outcome of any other. The probability of success, p, remains the same from trial to trial.
  * The random variable x denotes the number of trials needed to obtain the first success.
</remarks>


  <seealso cref="GeometricCDF"/>
  <seealso cref="GeometricCDFInv"/>*)

function GeometricPDF(x: Integer; p: double): double; overload;


(*<summary>Geometric cumulative distribution function (CDF).</summary>
  <returns>The geometric cumulative distribution function (CDF) for value x by using parameter p (probability).
  Probability p must lie on the interval [0,1] and x must be greater or equal than zero, otherwise the result
  is NAN.</returns>
  <param name="x">Function domain, positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>The geometric cumulative distribution function is defined by the following equation:

  <IMG name="probabilities022"/>

  The result of GeometricCDF is the probability of observing up to x trials before a success, when the
  probability of success in any given trial is p.
</remarks>


  <seealso cref="GeometricPDF"/>
  <seealso cref="GeometricCDFInv"/>*)

function GeometricCDF(x: Integer; p: double): double; overload;


(*<summary>Geometric distribution point percent function (PPF).</summary>
  <returns>the geometric distribution point percent function (PPF) for probability y by using parameter p
  (probability). Probability y and parameter p must lie on the interval [0,1], otherwise the result is NAN.</returns>
  <param name="y">Probability, real positive value on closed interval [0,1].</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>The result of GeometricCDFInv is observing result successes in a row, each with independent success
  probability p, with probability y.
</remarks>


  <seealso cref="GeometricCDF"/>
  <seealso cref="GeometricPDF"/>*)

function GeometricCDFInv(y: double; p: double): double; overload;


(*<summary>Gumbel probability density function (PDF).</summary>
  <returns>the Gumbel probability density function (PDF) for value x by using parameters Mu and beta.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, th routine calculates minimum Gumbel PDF
  If false, the routine calculates maximum Gumbel PDF.</param>

  
<remarks>Calculates Gumbel (minimum or maximum) distribution probabilty density function, defined by the following equation:

  <IMG name="probabilities080"/>

  The Gumbel distribution is a special case of the Generalized Extreme Value distribution.
  It used in industry in QA/QC applications and in the environmental sciences to model extreme values associated
  with flooding and rainfall. The Gumbel distribution takes its name from Emil J. Gumbel.
</remarks>


  <seealso cref="GumbelCDF"/>
  <seealso cref="GumbelCDFInv"/>*)

function GumbelPDF(x: double; Mu, beta: double; minimum: boolean): double; overload;


(*<summary>Gumbel cumulative distribution function (CDF).</summary>
  <returns>the Gumbel cumulative distribution function (CDF) for value x by using parameters Mu and beta.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, th routine calculates minimum Gumbel PDF
  If false, the routine calculates maximum Gumbel PDF.</param>

  
<remarks>Gumbel cumulative distribution function is defined by the following equation:

  <IMG name="probabilities078"/>
</remarks>


  <seealso cref="GumbelPDF"/>
  <seealso cref="GumbelCDFInv"/>*)

function GumbelCDF(x: double; Mu, beta: double; minimum: boolean): double; overload;


(*<summary>Gumbel distribution point percent function (PPF).</summary>
  <returns>the Gumbel distribution point percent function (PPF) for probability p using
  the parameters Mu and beta. Parameter beta must be positive, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, th routine calculates minimum Gumbel PDF
  If false, the routine calculates maximum Gumbel PDF.</param>

  
<remarks>The inverse Gumbel cumulative distribution function is defined by the following equation:

  <IMG name="probabilities079"/>
</remarks>


  <seealso cref="GumbelPDF"/>
  <seealso cref="GumbelCDF"/>*)

function GumbelCDFInv(p: double; Mu, beta: double; minimum: boolean): double; overload;


(*<summary>Hypergeometric probability density function (PDF).</summary>
  <returns>the hypergeometric probability density function (PDF) for value x using the integer
  parameters M (total number of elements), K (number of elements with a certain trait) and N
  (number of samples).</returns>
  <param name="x">Distribution domain, integer on closed interval [0,N].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>

  
<remarks>Calculates the hypergeometric distribution probability density function. The hypergeometric probability density function is defined by the following equation:

  <IMG name="probabilities025"/>

  where M is the total size of elements, K is the number of elements with desired trait and N
  is the number of samples drawn.The function returns the probability of drawing exactly x elements of a possible
  K in N drawings without replacement from group of M elements.

  Sampling from a finite population can be done in one of two ways. An item can be selected,
  examined, and returned to the population for possible reselection; or it can be selected,
  examined, and kept, thus preventing its reselection in subsequent draws. The former is
  called sampling with replacement, whereas the later is called sampling without replacement.

  Sampling with replacement guarantees that the draws are independent. In sampling without
  the replacement the draws are not independent. Thus if we sample without replacement, the
  random variable x,  the number of successes in N draws, is no longer binomial. Rather, it follows
  a hypergeometric distribution.

  If the number of items sampled (N) is small relative to the number of objects from which the
  sample is drawn (M), then the <see cref="BinomPDF"/> distribution can be used to
  approximate hypergeometric probability.
</remarks>


  <SeeAlso cref="HypGeometricCDF"/>
  <SeeAlso cref="HypGeometricCDFInv"/>*)

function HypGeometricPDF(x: Integer; M, K, N: Integer): double; overload;

(*<summary>Hypergeometric cumulative distribution function (CDF).</summary>
  <returns>the hypergeometric cumulative distribution function (CDF) for value x using the integer
  parameters M (total number of elements), K (number of elements with a certain trait) and N (number
  of samples drawn). Value x must be greater of equal than zero, all the parameters M, K N must be greater or equal
  than zero,  M &gt;= N, M &gt;= K, N &gt;= X, K &gt;= x. If not all of these conditions are met, the result is NAN.</returns>
  <param name="x">Distribution domain, integer on closed interval [0,N].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>

  
<remarks>The hypergeometric cumulative distribution function is defined by the following equation:

  <IMG name="probabilities024"/>

  The result of HypGeometricCDF is the probability of drawing up to x elements of a possible K in N drawings without
  replacement from a group of M elements.
</remarks>


  <seealso cref="HypGeometricPDF"/>
  <seealso cref="HypGeometricCDFInv"/>*)

function HypGeometricCDF(x: Integer; M, K, N: Integer): double; overload;

(*<summary>Hypergeometric distribution point percent function (PPF).</summary>
  <returns>the hypergeometric distribution point percent function (PPF) for probability p using the integer
  parameters M (total number of elements), K (number of elements with a certain trait) and N (number of samples drawn).</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>

  
<remarks>The result is the least integer such that the hypergeometric CDF evaluated at result equals or exceeds p.
</remarks>


  <seealso cref="HypGeometricPDF"/>
  <seealso cref="HypGeometricCDF"/>*)

function HypGeometricCDFInv(p: double; M, K, N: Integer): double; overload;


(*<summary>Inverse Chi-Square probability distribution function (PDF).</summary>
  <returns>the inverse Chi-Squared distribution probability density function (PDF), evaluated at x for integer parameter Nu.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  
<remarks>The inverse-chi-square distribution is the probability distribution of a random variable whose multiplicative
  inverse (reciprocal) has a chi-square distribution. It is also often defined as the distribution of a random
  variable whose reciprocal divided by its degrees of freedom is a chi-square distribution. That is, if x has the
  chi-square distribution with Nu degrees of freedom, then according to the first definition, 1/x has the
  inverse-chi-square distribution with Nzu degrees of freedom; while according to the second definition, Nu/x has
  the inverse-chi-square distribution with Nu degrees of freedom. This package uses first definition which yields
  the following relation:
</remarks>


  <seealso cref="InverseChiSquareCDF"/>
  <seealso cref="InverseChiSquareCDFInv"/>*)

function InverseChiSquarePDF(x: double; Nu: Integer): double; overload;


(*<summary>Inverse Chi-Square cumulative density function (CDF).</summary>
  <returns>the inverse Chi-Squared distribution cumulative distribution function (CDF), evaluated at x for integer parameter Nu.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  <seealso cref="InverseChiSquarePDF"/>
  <seealso cref="InverseChiSquareCDFInv"/>*)

function InverseChiSquareCDF(x: double; Nu: Integer): double; overload;


(*<summary>Inverse Chi-Square Percent Point Function (PPF).</summary>
  <returns>the inverse Chi-Squared percent point function (PPF), evaluated for integer parameter Nu.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  <seealso cref="InverseChiSquareCDF"/>
  <seealso cref="InverseChiSquarePDF"/>*)

function InverseChiSquareCDFInv(p: double; Nu: Integer): double; overload;


(*<summary>Inverse Gaussian probability density function (PDF).</summary>
  <returns>the inverse Gaussian PDF for value x using parameters Mu and Lambda, where both Mu and Lambda
  are positive scalars.</returns>
  <param name="x">Function domain, positive real value.</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>

  
<remarks>Calculates the inverse Gaussian distribution probability density function, defined by the following equation:

  <img name="probabilities084"/>

  where Mu>0 and Lambda>0.
</remarks>


  <seealso cref="InverseGaussianCDF"/>
  <seealso cref="InverseGaussianCDFInv"/>*)

function InverseGaussianPDF(x: double; Mu, Lambda: double): double; overload;


(*<summary>Inverse Gaussian cumulative distribution function (CDF).</summary>
  <returns>the inverse Gaussian CDF for value x using parameters Mu and Lambda, where both Mu and Lambda
  are positive scalars.</returns>
  <param name="x">Function domain, positive real value.</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>

  
<remarks>Inverse Gaussian CDFis defined by the following equation:

  <img name="probabilities085"/>

  where Mu and Lambda are both positive real numbers.
</remarks>

  
  <seealso cref="InverseGaussianCDF"/>
  <seealso cref="InverseGaussianCDFInv"/>*)

function InverseGaussianCDF(x: double; Mu, Lambda: double): double; overload;


(*<summary>Inverse Gaussian distribution point percent function (PPF).</summary>
  <returns>the inverse Gaussian distribution point percent function (PPF) for probability p using the parameters Mu and Lambda.
  Probability p must lie on the interval [0,1] and Mu, Lambda must both be positive, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>

  
<remarks>The inverse CDF is defined by the following equation:

  <IMG name="probabilities086"/>
</remarks>


  <seealso cref="InverseGaussianCDF"/>
  <seealso cref="InverseGaussianPDF"/>*)

function InverseGaussianCDFInv(p: double; Mu, Lambda: double): double; overload;


(*<summary>Johnson bounded probability density function (PDF).</summary>
  <returns>Johnson bounded (SB) probability density function (PDF) for parameters
  gamma, delta, Lambda and xi.</returns>
  <param name="x">Function domain, real value on closed interval [xi,xi+Lambda].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates the Johnson bounded (SB) probability density function, defined by the following equation:

  <IMG name="probabilities093"/>
</remarks>


  <seealso cref="JohnsonSBCDF"/>
  <seealso cref="JohnsonSBCDFInv"/>*)

function JohnsonSBPDF(x: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Johnson bounded cumulative distribution function (CDF).</summary>
  <returns>Johnson bounded (SB) cumulative distribution function (CDF) for parameters
  gamma, delta, Lambda and xi.</returns>
  <param name="x">Function domain, real value on closed interval [xi,xi+Lambda].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates the Johnson bounded (SB) cumulative distribution function, defined by the following equation:

  <IMG name="probabilities094"/>
</remarks>


  <seealso cref="JohnsonSBPDF"/>
  <seealso cref="JohnsonSBCDFInv"/>*)

function JohnsonSBCDF(x: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Johnson bounded point percent function (PPF).</summary>
  <returns>Johnson distribution bounded (SB) point percent function (PPF).</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates the Johnson bounded (SB) point percent function, defined by the following equation:

  <IMG name="probabilities095"/>
</remarks>


  <seealso cref="JohnsonSBCDF"/>
  <seealso cref="JohnsonSBPDF"/>*)

function JohnsonSBCDFInv(p: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Johnson unbounded probability density function (PDF).</summary>
  <returns>Johnson unbounded (SU) probability density function (PDF) for parameters
  gamma, delta, Lambda and xi.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates the Johnson unbounded (SU) probability density function, defined by the following equation:

  <IMG name="probabilities096"/>
</remarks>


  <seealso cref="JohnsonSBCDF"/>
  <seealso cref="JohnsonSBCDFInv"/>*)

function JohnsonUBPDF(x: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Johnson unbounded cumulative distribution function (CDF).</summary>
  <returns>Johnson unbounded (SU) cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Johnson unbounded (SU) cumulative distribution function, defined by the following equation:

  <img name="probabilities097"/>
</remarks>


  <seealso cref="JohnsonSBPDF"/>
  <seealso cref="JohnsonSBCDFInv"/>*)

function JohnsonUBCDF(x: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Johnson unbounded point percent function (PPF).</summary>
  <returns>Johnson distribution unbounded (SU) point percent function (PPF).</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates the Johnson unbounded (SU) point percent function, defined by the following equation:

  <IMG name="probabilities098"/>
</remarks>


  <seealso cref="JohnsonSBCDF"/>
  <seealso cref="JohnsonSBPDF"/>*)

function JohnsonUBCDFInv(p: double; gamma,delta,Lambda,xi: double): double; overload;


(*<summary>Laplace probability densify function (PDF).</summary>
  <returns>the Laplace probability density function (PDF) for value x using the parameters m and b.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Laplace PDF is defined by the following equation:

  <img name="probabilities070"/>
</remarks>


  <seealso cref="LaplaceCDF"/>
  <seealso cref="LaplaceCDFInv"/>*)

function LaplacePDF(x : double; m, b: double) : double; overload;


(*<summary>Laplace cumulative distribution function (CDF).</summary>
  <returns>the Laplace cumulative distribution function (CDF) for value x using the parameters m and b.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>The result of LaplaceCDF is the probability that a single observation from a Laplace
  distribution with parameters m and b will fall in the interval [-infinity,x]. Laplace CDF is defined by the following equation:

  <img name="probabilities068"/>
</remarks>


  <seealso cref="LaplacePDF"/>
  <seealso cref="LaplaceCDFInv"/>*)

function LaplaceCDF(x : double; m, b: double) : double; overload;


(*<summary>Laplace distribution point percent function (PPF).</summary>
  <returns> the Laplace distribution point percent function (PPF) for probability p using the parameters m, and b.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Inverse CDF is defined by the following equation:

  <img name="probabilities069"/>
</remarks>


  <seealso cref="LaplacePDF"/>
  <seealso cref="LaplaceCDF"/>*)

function LaplaceCDFInv(p : double; m, b: double) : double; overload;


(*<summary>Logarithmic probability densify function (PDF).</summary>
  <returns>the logarithmic probability density function (PDF) for value x using the parameter p.</returns>
  <param name="x">Function domain, positive integer.</param>
  <param name="p">Distribution parameter, real value on open interval (0,1).</param>

  
<remarks>Calculates the logarithmic distribution probability density function (PDF), defined by
  the following equation:

  <IMG name="probabilities101"/>
</remarks>


  <seealso cref="LogarithmicCDF"/>
  <seealso cref="LogarithmicCDFInv"/>*)

function LogarithmicPDF(x: Integer; p: double): double; overload;


(*<summary>Logarithmic cumulative distribution function (CDF).</summary>
  <returns>the logarithmic probability density function (PDF) for value x using the parameter p.</returns>
  <param name="x">Function domain, positive integer.</param>
  <param name="p">Distribution parameter, real value on open interval (0,1).</param>

  
<remarks>Calculates the logarithmic distribution cumulative distribution function (CDF), defined by the following equation:

  <IMG name="probabilities102"/>
</remarks>


  <seealso cref="LogarithmicPDF"/>
  <seealso cref="LogarithmicCDFInv"/>*)

function LogarithmicCDF(x: Integer; p: double): double; overload;

(*<summary>Logarithmic point percent function (PPF).</summary>
  <returns>the logarithmic point percent function (PPF) for probability y using the parameter p.</returns>
  <param name="y">Probability, real positive value on closed interval [0,1].</param>
  <param name="p">Distribution parameter, real value on open interval (0,1).</param>

  <seealso cref="LogarithmicCDF"/>
  <seealso cref="LogarithmicPDF"/>*)

function LogarithmicCDFInv(y: double; p: double): double; overload;


(*<summary>Logistic probability densify function (PDF).</summary>
  <returns>the logistic probability density function(PDF) for value x using the parameters m and b.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Distribution location parameter.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>The logistic probability density function is defined by the following equation:

  <IMG name="probabilities065"/>

  m and b must both be positive, otherwise the result is NAN.
</remarks>


  <seealso cref="LogisticCDF"/>
  <seealso cref="LogisticCDFInv"/>*)

function LogisticPDF(x : double; m, b: double) : double; overload;

(*<summary>Logistic cumulative distribution function (CDF).</summary>
  <returns>the logistic cumulative distribution function (CDF) for value x using the parameters m and b.
  Both parameters must be positive, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>The logistic cumulative distribution function is defined by the following equation:

  <IMG name="probabilities063"/>

  The result of LogisticCDF is the probability that a single observation from a logistic
  distribution with parameters m and b will fall in the interval [-infinity,x].
</remarks>


  <seealso cref="LogisticPDF"/>
  <seealso cref="LogisticCDFInv"/>*)

function  LogisticCDF(x, m, b: double): double; overload;

(*<summary>Logistic distribution point percent function (PPF).</summary>
  <returns>the Logistic distribution point percent function (PPF) for probability p using the parameters m,
  and b. Probability p must lie on the interval [0,1] and m, b must both be positive, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>The inverse logistic cumulative distribution is defined by the following equation:

  <IMG name="probabilities064"/>
</remarks>


  <seealso cref="LogisticPDF"/>
  <seealso cref="LogisticCDF"/>*)

function  LogisticCDFInv(p, m, b: double): double; overload;


(*<summary>Log-normal probability densify function (PDF).</summary>
  <returns>the log-normal probability density function (PDF) for value x using the parameters Mu (mean value) and sigma
  (standard deviation). Sigma must be positive value, otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive real value or zero.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>Log-normal probability density function is defined by the following equation:

  <img name="probabilities073"/>
</remarks>


  <seealso cref="LogNormalCDF"/>
  <seealso cref="LogNormalCDFInv"/>*)

function LogNormalPDF(x : double; Mu, sigma: double) : double; overload;

(*<summary>Log-normal cumulative distribution function (CDF).</summary>
  <returns> the log-normal cumulative distribution function (CDF) for value x using the parameters Mu (mean value) and sigma
  (standard deviation). Sigma must be positive value, otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive real value or zero.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>Log-normal cumulative distribution function is defined by the following equation:

  <img name="probabilities071"/>
</remarks>


  <seealso cref="LogNormalPDF"/>
  <seealso cref="LogNormalCDFInv"/>*)

function  LogNormalCDF(x, Mu, sigma: double): double; overload;

(*<summary>Log-normal distribution point percent function (PPF).</summary>
  <returns>the Log-normal distribution point percent function (PPF) for probability p using the parameters Mu
  (mean value) and sigma (standard deviation). Probability p must lie on the interval [0,1] and sigma
  must be positive value, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>Inverse Log-normal CDF is defined by the following equation:

  <img name="probabilities072"/>
</remarks>


  <seealso cref="LogNormalPDF"/>
  <seealso cref="LogNormalCDF"/>*)

function  LogNormalCDFInv(p, Mu, sigma: double): double; overload;


(*<summary>Log-Weibull probability densify function (PDF).</summary>
  <returns>the log-Weibull (aka Extreme value or Fisher-Tippett) probability density function (PDF) for value x
  using the parameters a and b.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Log-Weibull PDF is defined by the following equation:

  <img name="probabilities076"/>
</remarks>


  <seealso cref="LogWeibullCDF"/>
  <seealso cref="LogWeibullCDFInv"/>
  <seealso cref="GenExtValuePDF"/>*)

function LogWeibullPDF(x : double; a, b: double) : double; overload;


(*<summary>Log-Weibull cumulative distribution function (CDF).</summary>
  <returns>the log-Weibull cumulative distribution function (CDF) for value x using the parameters a and and b.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Log-Weibull cumulative distribution function (CDF) is defined by the following equation:

  <img name="probabilities074"/>
</remarks>


  <seealso cref="LogWeibullPDF"/>
  <seealso cref="LogWeibullCDFInv"/>
  <seealso cref="GenExtValueCDF"/>*)

function LogWeibullCDF(x : double; a, b: double) : double; overload;


(*<summary>Log-Weibull distribution point percent function (PPF).</summary>
  <returns>the Log-Weibull distribution point percent function (PPF) for probability p using the parameters a
  and b. Probability p must lie on the interval [0,1], otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Inverse Log-Weibull CDF is defined by the following equation:

  <img name="probabilities075"/>
</remarks>


  <seealso cref="LogWeibullPDF"/>
  <seealso cref="LogWeibullCDF"/>
  <seealso cref="GenExtValueCDFInv"/>*)

function LogWeibullCDFInv(p : double; a, b: double) : double; overload;


(*<summary>Maxwell probability density function (PDF).</summary>
  <returns>the Maxwell probability density function (PDF) for value x using the parameter a. Parameter
  a must be positive and value x &gt;= 0 , otherwise the result is NAN.</returns>
  <param name="x">Function domain, zero or positive real value.</param>
  <param name="a">Distribution parameter, positive real value.</param>

  
<remarks>The Maxwell probability density function is defined by the following equation:

  <IMG name="probabilities053"/>
</remarks>


  <seealso cref="MaxwellCDF"/>
  <seealso cref="MaxwellCDFInv"/>*)

function MaxwellPDF(x, a: double): double; overload;


(*<summary>Maxwell cumulative distribution function (CDF).</summary>
  <returns>the Maxwell cumulative distribution function (CDF) for value x using the parameter a.
  Parameter a must be positive, value x &gt;= 0 , otherwise the result is NAN.</returns>
  <param name="x">Function domain, zero or positive real value.</param>
  <param name="a">Distribution parameter, positive real value.</param>

  
<remarks>Maxwell cumulative distribution is defined by the following equation:

  <IMG name="probabilities051"/>

  where <see cref="IGamma"/> is incomplete Gamma function. The result of MaxwellCDF is
  the probability that a single observation from a Maxwell distribution with parameter a will
  fall in the interval [0,x].
</remarks>


  <seealso cref="MaxwellPDF"/>
  <seealso cref="MaxwellCDFInv"/>*)

function MaxwellCDF(x, a: double): double; overload;


(*<summary>Maxwell point percent function (PPF).</summary>
  <returns>the Maxwell distribution point percent function (PPF) for probabilty p using
  the parameter a. Parameter a must be positive and probabilty p must lie on the interval [0,1],
  otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution parameter, positive real value.</param>

  
<remarks>Inverse Maxwell cumulative distribution is defined by the following equation:

  <IMG name="probabilities052"/>

  The result of MaxwellCDFInv is the solution of the integral equation of the <see cref="MaxwellCDF"/>
  with parameter a and with the desired probability p.
</remarks>


  <seealso cref="MaxwellPDF"/>
  <seealso cref="MaxwellCDF"/>*)

function MaxwellCDFInv(p, a: double): double; overload;


(*<summary>Negative binomial probability density function (PDF).</summary>
  <returns>the negative binomial probability density function (PDF) for value x using the parameters R
  (number of success) and p (probability of success). Probability p must lie on the interval [0,1] and
  R must be greater or equal than one, otherwise the result is NAN.</returns>
  <param name="x">Function domain, integer, number of trials needed to obtain R successes.</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="p">Defines probability of each trial, real value on closed interval [0,1].</param>

  
<remarks>The negative binomial probability density function is defined by the following equation:

  <IMG name="probabilities029"/>

  where I is the discrete interval on which the negative binomial PDF is not zero.
  The negative binomial distribution can be thought of as a "reversal" of the binomial distribution.
  In the binomial setting the random variable represents the number of successes obtained in a series
  on n independent and identical Bernoulli trials: the number of trials is fixed and the number of
  successes will vary from experiment to experiment. The negative binomial random variable represents
  the number of trials needed to obtain exactly R successes; here, the number of successes is fixed
  and the number of trials will vary from experiment to experiment. In particular, the negative
  binomial random variable arises in situations characterized by these properties:
  * The experiment consists of a series of independent and identical Bernoulli trials, each with probability p of success.
  * The trials are observed until exactly R successes are obtained, where R is fixed by the experimentator.
  * The random variable x is the number of trials needed to obtain the R successes.
</remarks>


  <seealso cref="NegBinomCDF"/>
  <seealso cref="NegBinomCDFInv"/>*)

function NegBinomPDF(x: Integer; R: double; p: double): double; overload ;


(*<summary>Negative binomial cumulative distribution function (CDF).</summary>
  <returns>the negative binomial cumulative distribution function (CDF) for value x using the parameters R
  (number of success) and p (probability of success). Probability p must lie on the interval [0,1], x must be greater
  or equal than zero and R must be greater or equal than one, otherwise the result is NAN.</returns>
  <param name="x">Function domain, integer, nzmber of trials needed to obtain R successes.</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="p">Defines probability of each trial, real value on closed interval [0,1].</param>

  
<remarks>The negative binomial cumulative distribution function is defined by the following equation:

  <IMG name="probabilities028"/>

  where I is the interval on which the negative binomial cumulative distribution function is not zero.
</remarks>


  <seealso cref="NegBinomPDF"/>
  <seealso cref="NegBinomCDFInv"/>*)

function NegBinomCDF(x: Integer; R: double; p: double): double; overload;


(*<summary>Negative binomial distribution point percent function (PPF).</summary>
  <returns>the Negative binomial distribution point percent function (PPF) for probability y using the parameters R
  (number of success) and p (probability of success). Probabilities p and y must lie on the interval [0,1], and R must
  be greater or equal than one, otherwise the result is NAN.</returns>
  <param name="y">Probability, real positive value on closed interval [0,1].</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="p">Defines probability of each trial, real value on closed interval [0,1].</param>

  
<remarks>The result of NegBinomCDFinv is the smallest integer such that the negative binomial CDF evaluated at result equals or exceeds y.
  If the function returns NAN, either one of the inputs was NAN, or total loss of precision was detected.
</remarks>


  <seealso cref="NegBinomPDF"/>
  <seealso cref="NegBinomCDF"/>*)

function NegBinomCDFInv(y: double; R: double; p: double): double; overload;


(*<summary>Normal probability density function (PDF).</summary>
  <returns>the normal probability density function (PDF) for value x using the parameters Mu (mean value) and sigma
  (standard deviation). Sigma must be positive value, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>The normal probability density function is defined by the following equation:

  <IMG name="probabilities032"/>

  where Mu is mean value and sigma is standard deviation. Special case of normal distribution (mean = 0, sigma =1) is
  called <b>standard normal distribution</b>.

  The normal distribution is a distribution that underlies many of the statistical methods used in data analysis.
  This distribution is often referred to as "Gaussian" distribution. Normal distribution can be used for modeling
  when the sample size is large. The theoretical justification can be found in Central Limit Theorem which states
  (roughly) that the sum of independent samples from any distribution with finite mean and variance converges
  to the normal distribution as the sample size goes to infinity.
</remarks>


  <seealso cref="NormalCDF"/>
  <seealso cref="NormalCDFInv"/>*)

function NormalPDF(x: double; Mu, sigma: double): double; overload;


(*<summary>Normal cumulative distribution function (CDF).</summary>
  <returns>the normal cumulative distribution function (CDF) for value x using the parameters Mu (mean value)
  and sigma (standard deviation). Sigma must be positive value, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>The normal cumulative distribution function is defined by the following equation:

  <IMG name="probabilities030"/>

  where Mu is mean value and sigma is standard deviation. The result of NormalCDF  is the probability that a single
  observation from a normal distribution with parameters  Mu and sigma will fall in the interval (-infinity,x].
</remarks>


  <seealso cref="NormalPDF"/>
  <seealso cref="NormalCDFInv"/>*)

function  NormalCDF(x, Mu, sigma: double): double; overload;

(*<summary>Normal distribution point percent function (PPF).</summary>
  <returns>the normal  distribution point percent function (PPF) for probability p using the parameters Mu
  (mean value) and sigma (standard deviation). Probability p must lie on the interval [0,1] and sigma
  must be positive value, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>The normal distribution point percent function is defined by the following equation:

  <IMG name="probabilities031"/>

  where Mu is mean value and sigmsa is standard deviation. The result of NormalCDFInv
  is the solution of the integral equation above with the parameters Mu and sigma where you supply the probability p.
</remarks>

  <seealso cref="NormalPDF"/>
  <seealso cref="NormalCDF"/>*)

function  NormalCDFInv(p, Mu, sigma: double): double; overload;


(*<summary>Pareto probability density function (PDF).</summary>
  <returns>the Pareto probability density function (PDF) for value x using the parameters a and b.
  Parameter b must be smaller than value x , otherwise the result is NAN.</returns>
  <param name="x">Function domain.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>

  
<remarks>The Pareto probability density function is defined by the following equation:

  <IMG name="probabilities056"/>
</remarks>


  <seealso cref="ParetoCDF"/>
  <seealso cref="ParetoCDFInv"/>*)

function ParetoPDF(x, a, b: double): double; overload;

(*<summary>Pareto cumulative distribution function (CDF).</summary>
  <returns>the Pareto cumulative distribution function (CDF) for value x using the parameters a and b.
  Parameter b must be smaller than value x , otherwise the result is NAN.</returns>
  <param name="x">Function domain</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>

  
<remarks>The Pareto cumulative distribution function is defined by the following equation:

  <IMG name="probabilities054"/>
</remarks>


  <seealso cref="ParetoPDF"/>
  <seealso cref="ParetoCDFInv"/>*)

function ParetoCDF(x, a, b: double): double; overload;

(*<summary>Pareto distribution point percent function (PPF).</summary>
  <returns>the Pareto distribution point percent function (PPF) for probability p using
  the parameters a and b. Probability p must lie on the interval [0,1],  otherwise the result is
  NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>

  
<remarks>The inverse Pareto cumulative distribution function is defined by the following equation:

  <IMG name="probabilities055"/>
</remarks>


  <seealso cref="ParetoPDF"/>
  <seealso cref="ParetoCDF"/>*)

function ParetoCDFInv(p, a, b: double): double; overload;


(*<summary>Poisson probability density function (PDF).</summary>
  <returns>the Poisson probability density function (PDF) for value x using the parameter Lambda.
  Lambda must be positive, otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive integer or zero.</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>

  
<remarks>The Poisson probability density function is defined by the following equation:

  <IMG name="probabilities034"/>

  Poisson random variable usually arise in connection with what are called Poisson processes. Poisson
  process involve observing discrete events in a continuous interval of time, length or space. The variable
  of interest in a Poisson process is the number of occurrences of the event in an interval of length.
  Basically, the result is physically, the parameter Lambda of a Poisson process represents the average
  number of occurrences of the event in question per measurement unit.

  One other important application of the Poisson distribution is approximating the <see cref="BinomPDF"/>
  distribution with the Poisson distribution. This approximation is usually good if  <c>n &gt;= 20</c> and
  <c>p &lt;= 0.05</c> (binomial distribution parameters) and very good if <c>n &gt;= 100</c> and <c>np &lt;= 10</c>. Since the approximation is
  used when number of events (n) is large and p (probability of success) is small, the Poisson distribution
  is often called the distribution of "rare" events.
</remarks>


  <seealso cref="PoissonCDF"/>
  <seealso cref="PoissonCDFInv"/>*)

function PoissonPDF(x: Integer; Lambda: double): double; overload;

(*<summary>Poisson cumulative distribution function (CDF).</summary>
  <returns>the Poisson cumulative distribution function (CDF) for value x using the parameter Lambda.
  Lambda must be positive, otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive integer or zero.</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>

  
<remarks>The Poisson cumulative distribution function is defined by the following equation:

  <IMG name="probabilities033"/>
</remarks>


  <seealso cref="PoissonPDF"/>
  <seealso cref="PoissonCDFInv"/>*)

function PoissonCDF(x: integer; Lambda: double): double; overload;

(*<summary>Poisson distribution point percent function (PPF).</summary>
  <returns>the Poisson distribution point percent function (PPF) for probability p using
  the parameter Lambda. Lambda must be positive and probability y must lie on the interval [0,1],
  otherwise the result is NAN.</returns>
  <param name="p">Probability, real value on closed interval [0,1].</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>

  <seealso cref="PoissonPDF"/>
  <seealso cref="PoissonCDF"/>*)

function PoissonCDFInv(p: double; Lambda: double): double; overload;


(*<summary>Power probability distribution function (PDF).</summary>
  <returns>Power distribution PDF, evaluated at x using parameters alpha and beta. Alpha, beta parameters
  must be positive and 0 &lt;= x &lt;= 1/beta, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real on closed interval [0,1/beta]</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>

  
<remarks>Power probability distribution function is defined by the following equation:

  <IMG name="probabilities087"/>

  where alpha, beta are both positive scalars and <c>0 &lt;= x &lt;= 1/beta</c>.
</remarks>


  <seealso cref="PowerCDF"/>
  <seealso cref="PowerCDFInv"/>*)

function PowerPDF(x: double; alpha, beta: double): double; overload;


(*<summary>ower cumulative density function (CDF).</summary>
  <returns>Power distribution CDF, evaluated at x using parameters alpha and beta. Alpha, beta parameters
  must be positive and 0 &lt;= x &lt;= 1/beta, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real on closed interval [0,1/beta]</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>

  
<remarks>Power probability distribution function is defined by the following equation:

  <IMG name="probabilities088"/>

  where alpha, beta are both positive scalars and <c>0 &lt;= x &lt;= 1/beta</c>.
</remarks>

  
  <seealso cref="PowerPDF"/>
  <seealso cref="PowerCDFInv"/>*)

function PowerCDF(x: double; alpha, beta: double): double; overload;


(*<summary>Power distribution point percent function (PPF).</summary>
  <returns>the Power distribution point percent function (PPF) for probability p using
  the parameters alpha and beta. Probability p must lie on the interval [0,1],  and alpha, beta
  must be positive scalars, otherwise the result is  NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>

  
<remarks>The inverse Pareto cumulative distribution function is defined by the following equation:

  <IMG name="probabilities089"/>
</remarks>


  <seealso cref="PowerPDF"/>
  <seealso cref="PowerCDF"/>*)

function PowerCDFInv(p: double; alpha, beta: double): double; overload;


(*<summary>Rayleigh probability density function (PDF).</summary>
  <returns>the Rayleigh probability density function (PDF) for value x using the parameter b. Parameter
  b must be positive and value x must be &gt;= 0,  otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive real value or zero.</param>
  <param name="b">Distribution parameter, positive real value.</param>

  
<remarks>The Rayleigh probabilty density function is defined by the following equation:

  <IMG name="probabilities059"/>
</remarks>


  <seealso cref="RayleighCDF"/>
  <seealso cref="RayleighCDFInv"/>*)

function RayleighPDF(x, b: double): double; overload;


(*<summary>Rayleigh cumulative distribution function (CDF).</summary>
  <returns>the Rayleigh cumulative distribution function (CDF) for value x using the parameter b.
  x and b must both be positive real numbers, otherwise the result is NAN.</returns>
  <param name="x">Function domain, positive real value or zero.</param>
  <param name="b">Distribution parameter, positive real value.</param>

  
<remarks>The Rayleigh cumulative distribution function is defined by the following equation:

  <IMG name="probabilities057"/>
</remarks>


  <seealso cref="RayleighPDF"/>
  <seealso cref="RayleighCDFInv"/>*)

function RayleighCDF(x, b: double): double; overload;


(*<summary>Rayleigh distribution point percent function (PPF).</summary>
  <returns>the Rayleigh distribution point percent function (PPF) for probabilty p
  using the parameter b. Parameter b must be positive and probabilty p must lie on the interval [0,1],
  otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="b">Distribution parameter, positive real value.</param>

  
<remarks>The inverse Rayleigh cumulative distribution function is defined by the following equation:

  <IMG name="probabilities058"/>

  The result of RayleighCDFInv is the solution of the integral equation of the <see cref="RayleighCDF"/>
  with parameter b and with the desired probability p.
</remarks>


  <seealso cref="RayleighPDF"/>
  <seealso cref="RayleighCDF"/>*)

function RayleighCDFInv(p, b: double): double; overload;


(*<summary>Student probability density function (PDF).</summary>
  <returns>the Student probability density function for value x using the integer parameter Nu (degrees
  of freedom). Parameter Nu must be positive integer number, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  
<remarks>The Student probability density function is defined by the following equation:

  <IMG name="probabilities037"/>

  The Student (or T) distribution describes family of curves that depend on a  single parameter Nu
  (degrees of freedom). If a random variable follows a Student distribution, then it must be written as
  a ratio of a standard normal random variable to the square root of an independent chi-squared random variable,
  divided by its degrees of freedom. As Nu goes towards infinity, the Student distribution approaches the
  <b>standard normal distribution</b>.
</remarks>


  <seealso cref="StudentCDF"/>
  <seealso cref="StudentCDFInv"/>*)

function StudentPDF(x :double; Nu: Integer): double; overload;

(*<summary>Student cumulative distribution function (CDF).</summary>
  <returns>the Student cumulative distribution function for value x using the integer parameter Nu (degrees of
  freedom). Parameter Nu must be positive integer number, otherwise the result is NAN.</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  
<remarks>The Student cumulative distribution function is defined by the following equation:

  <IMG name="probabilities035"/>

  The result of StudentCDF is the probability that a single observation from the Student distribution with Nu
  degrees of freedom will fall in the interval (-infinity,x].
</remarks>

  <seealso cref="StudentPDF"/>
  <seealso cref="StudentCDFInv"/>*)

function StudentCDF(x :double; Nu: Integer): double; overload;

(*<summary>Student distribution point percent function (PPF).</summary>
  <returns>the Student distribution point percent function (PPF) for probability p using the integer parameter Nu
  (degrees of freedom). Parameter Nu must be positive integer number and probability p must lie on the interval
  [0,1], otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>

  
<remarks>The inverse Student cumulative distribution function is defined by the following equation:

  <IMG name="probabilities036"/>

  The result of StudentCDFInv is the solution of the integral equation of the StudentCDF with parameter Nu where you
  must specify the probability p.
</remarks>


  <seealso cref="StudentPDF"/>
  <seealso cref="StudentCDF"/>*)

function StudentCDFInv(p :double; Nu: Integer): double;overload;


(*<summary>Triangular probability density function (PDF).</summary>
  <returns>the Triangular probability density function for parameters a, b and c.</returns>
  <param name="x">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>

  
<remarks>The triangular probability density function is defined by the following equation:

  <IMG name="probabilities062"/>

  where a is location, b scale and c shape parameter. The triangular distribution is a simple
  distribution that has no real source in nature. It is most, if not exclusively, useful for
  stochastic modeling rather than statistical analysis because of its artificial nature.
  In stochastic modeling, the triangular distribution is best used to model variables with
  both upper and lower bounds near the range of interest. The distribution does not need to
  be, and frequently is not, symmetric.
</remarks>


  <seealso cref="TriangularCDF"/>
  <seealso cref="TriangularCDFInv"/>*)

function TriangularPDF(x: double; a,b,c: double): double; overload;

(*<summary>Triangular cumulative distribution function (CDF).</summary>
  <returns>the triangular cumulative distribution function for value x using the parameters a,b,c.</returns>
  <param name="x">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>

  
<remarks>The triangular cumulative distribution function is defined by the following equation:

  <IMG name="probabilities060"/>

  The result of TriangularCDF is the probability that a single observation from the triangular distribution with location parameter a,
  scale parameter b and shape parameter c fall in the interval (a,x].
</remarks>


  <seealso cref="TriangularPDF"/>
  <seealso cref="TriangularCDFInv"/>*)

function TriangularCDF(x: double; a,b,c: double): double; overload;

(*<summary>Triangular distribution point percent function (PPF).</summary>
  <returns> the Triangular distribution point percent function (PPF) for probability y using distribution parameters a,b,c.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>

   
<remarks>The inverse triangular cumulative distribution function is defined by the following equation:

  <IMG name="probabilities061"/>

  The result of TriangularCDFInv is the solution of the integral equation of the TriangularCDF with parameters a,b,c where
  you must specify the probability p.
</remarks>


  <seealso cref="TriangularPDF"/>
  <seealso cref="TriangularCDF"/>*)

function TriangularCDFInv(p: double; a,b,c: double): double; overload;


(*<summary>Uniform probability density function (PDF).</summary>
  <returns>the uniform probability density function (PDF) for value x using the parameters a and b. The parameter a must
  be smaller than parameter b, otherwise the result is NAN.</returns>
  <param name="x">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="b">Distribution parameter, real value, defining distribution upper bound.</param>

  
<remarks>The uniform probability density function is defined by the following equation:

  <IMG name="probabilities040"/>

  where a and b define the interval on which uniform CDF is defined. Uniform distribution is perhaps the simplest distribution
  with which to work. It can be used do describe processes in which the events occur with equal or uniform probability.
</remarks>


  <seealso cref="UniformCDF"/>
  <seealso cref="UniformCDFInv"/>*)

function UniformPDF(x: double; a, b: double): double; overload;

(*<summary>Uniform cumulative distribution function (CDF).</summary>
  <returns>the uniform cumulative distribution function (CDF) for value x using the parameters a and b.
  The parameter a must be smaller than parameter b, otherwise the result is NAN.</returns>
  <param name="x">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="b">Distribution parameter, real value, defining distribution upper bound.</param>

  
<remarks>The uniform cumulative distribution function is defined by the following equation:

  <IMG name="probabilities038"/>

  where a and b define the interval on which uniform CDF is defined.
</remarks>


  <seealso cref="UniformPDF"/>
  <seealso cref="UniformCDFInv"/>*)

function UniformCDF(x: double; a, b: double): double; overload;

(*<summary>Uniform distribution point percent function (PPF).</summary>
  <returns>the Uniform distribution point percent function (PPF) for probability p using the parameters a and b.
  The parameter a must be smaller than parameter b and p must lie on the interval [0,1], otherwise the result
  is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="b">Distribution parameter, real value, defining distribution upper bound.</param>

  
<remarks>The inverse uniform cumulative distribution function is defined by the following equation:

  <IMG name="probabilities039"/>

  where a and b define the interval on which uniform CDF is defined.
</remarks>


  <seealso cref="UniformPDF"/>
  <seealso cref="UniformCDF"/>*)

function UniformCDFInv(p: double; a, b: double): double; overload;


(*<summary>Discrete uniform probability density function (PDF).</summary>
  <returns>the discrete uniform probability density function (PDF) for value x using the parameter N.
  The parameter N must be greater than 0, otherwise the result is NAN.</returns>

  
<remarks>The discrete uniform distribution is a simple distribution that puts equal weights 1/N on the integers from 1 to N.
</remarks>


  <seealso cref="UniformDCDF"/>
  <seealso cref="UniformDCDFInv"/>*)

function UniformDPDF(x: Integer; N: Integer): double; overload;

(*<summary>Discrete uniform cumulative distribution function (CDF).</summary>
  <returns>the discrete uniform cumulative distribution function (CDF) for value x using the parameter N. The parameter
  N must be greater or equal to 1, otherwise the result is NAN.</returns>

  <seealso cref="UniformDPDF"/>
  <seealso cref="UniformDCDFInv"/>*)

function UniformDCDF(x: Integer; N: Integer): double; overload;

(*<summary>Discrete uniform distribution point percent function (PPF).</summary>
  <returns> the Discrete uniform distribution point percent function (PPF) for probability p using the parameter
  N. The probability p must lie on the interval (0,1] and parameter N must be greater than 1, otherwise the result
  is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="N">Distribution parameter, real positive integer.</param>

  <seealso cref="UniformDPDF"/>
  <seealso cref="UniformDCDF"/>*)

function UniformDCDFInv(p: double; N: Integer): double; overload;


(*<summary>Weibull probability density function (PDF).</summary>
  <returns>the Weibull probability density function for value x using the parameters a and b. Parameters a and b must be positive numbers, otherwise
  the result is NAN.</returns>
  <param name="x">Function domain, zero or real positive value.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value.</param>

  
<remarks>Calculates the Weibull probability density function, defined by the following equation:

  <IMG name="probabilities043"/>

  where I is the interval on which the inverse Weibull CDF is not zero. The Weibull distribution has been found
  to be useful in a variety of physical applications. It arises in the study of reliability. Reliability
  studies are concentrated with accessing whether or not a system functions adequately under the conditions for which it was designed.
</remarks>


  <seealso cref="WeibullCDF"/>
  <seealso cref="WeibullCDFInv"/>*)

function WeibullPDF(x: double; a,b :double): double; overload;

(*<summary>Weibull cumulative distribution function (CDF).</summary>
  <returns>the Weibull cumulative distribution function for value x using the parameters a and b. Value x must be greater or equal
  than zero and parameters a and b must be positive numbers, otherwise the result is NAN.</returns>
  <param name="x">Function domain, zero or real positive value.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value.</param>

  
<remarks>The Weibull cumulative distribution  function is defined by the following equation:

  <IMG name="probabilities041"/>

  where I is the interval on which the Weibull CDF is not zero.
</remarks>


  <seealso cref="WeibullPDF"/>
  <seealso cref="WeibullCDFInv"/>*)

function WeibullCDF(x: double; a,b :double): double; overload;

(*<summary>Weibull distribution point percent function (PPF).</summary>
  <returns>the Weibull distribution point percent function (PPF) for probability p using the parameters a and b. Probability
  p must lie on the interval [0,1] and parameters a and b must be positive numbers, otherwise the result is NAN.</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value.</param>

  
<remarks>The inverse Weibull cumulative distribution function is defined by the following equation:

  <IMG name="probabilities042"/>

  where I is the interval on which the inverse Weibull CDF is not zero.
</remarks>


  <seealso cref="WeibullCDF"/>
  <seealso cref="WeibullPDF"/>*)

function WeibullCDFInv(p: double; a,b :double): double; overload;


(*<summary>Zipf distribution probability density function (PDF).</summary>
  <returns>the Zipf distribution probability density function (PDF).</returns>
  <param name="x">Function domain, positive integer or zero [0,N].</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>

  
<remarks>Calculates the Zipf probability distribution function. The Zipf probability density function is defined by the following equation:

  <IMG name="probabilities116"/>

  where H(N,s) is a generalized Harmonic function, evaluated at parameters N,s and x,s respectively.
</remarks>


  <seealso cref="ZipfCDF"/>
  <seealso cref="ZipfCDFInv"/>*)

function ZipfPDF(x: Integer; s: double; N: Integer): double; overload;

(*<summary>Zipf distribution cumulative distribution function (CDF).</summary>
  <returns> the Zipf distribution cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, positive integer or zero [0,N].</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>

  
<remarks>Calculates the Zipf cumulative density function. The Zipf probability density function is defined by the following equation:

  <IMG name="probabilities117"/>

  where H(N,s) is a generalized Harmonic function, evaluated at parameters N,s and N,s-1 respectively.
</remarks>


  <seealso cref="ZipfPDF"/>
  <seealso cref="ZipfCDFInv"/>*)

function ZipfCDF(x: Integer; s: double; N: Integer): double; overload;

(*<summary>Zipf distribution point percent function (PPF).</summary>
  <returns>Zipf distribution point percent function (PPF).</returns>
  <param name="p">Probability, real positive value on closed interval [0,1].</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>

  <seealso cref="ZipfPDF"/>
  <seealso cref="ZipfCDF"/>*)

function ZipfCDFInv(p: double; s: double; N: Integer): double; overload;



(*<summary>Inverse cumulative probability distribution for the Studentized Range distribution </summary>

  <param name="p"> The cumulative probability or percentile range.</param>
  <param name="r"> Number of samples.</param>
  <param name="v"> Degrees of freedom. To obtain quantiles from the maximum normal range distribution (v = Inf),
                   specify this param value bigger than 25000.</param>
  <param name="ci"> If ci = 1, then the inverse cumulative probability function of the studentized range is returned.
                    If ci &gt; 1 (2), then the inverse distribution of the studentized maximum modulus is obtained.</param>

  
<remarks>Note: The maximum allowed p value when v = 1, is 0.98. In general, the accuracy of the algorithm decreases as r increases,
  as v decreases and as p approaches 1. It is recommended to verify the result with the StudentRangeCDF function to ensure,
  that the value of the p parameter can again be recovered.

  <b>References: </b> <para/>
  [1] Based on Fortran program from statlib, http://lib.stat.cmu.edu, Algorithm AS 190  Appl. Statist. (1983) Vol.32, No. 2
      Incorporates corrections from Appl. Statist. (1985) Vol.34 (1)
  [2] Ferreira, D., et al. "Quantiles from the Maximum Studentized Range Distribution." Biometric Brazilian Journal 25.1 (2007): 117-135.
      http://jaguar.fcav.unesp.br/RME/fasciculos/v25/v25_n1/A8_Daniel.pdf (See prange function)
</remarks>
*)



function StudentRangeCDFInv(const p: double; const r, v: integer; const ci: integer = 1): double;    

(*<summary>Cumulative probability distribution for the Studentized Range statistics. </summary>

  <param name="q"> The probability at which to evaluate the function.</param>
  <param name="r"> Number of samples.</param>
  <param name="v"> Degrees of freedom. To obtain quantiles from the maximum normal range distribution (v = Inf),
                   specify this param value bigger than 25000.</param>
  <param name="ci"> If ci = 1, then the cumulative probability function of the studentized range is returned.
                    If ci &gt; 1 (2), then the distribution of the studentized maximum modulus is obtained.</param>

  
<remarks>Note: The maximum allowed q value when v = 1, is 200.

  In general, the accuracy of the algorithm decreases as r increases, as v decreases and as p approaches 1.

  <b>References: </b> <para/>
  [1] Based on Fortran program from statlib, http://lib.stat.cmu.edu, Algorithm AS 190  Appl. Statist. (1983) Vol.32, No. 2
      Incorporates corrections from Appl. Statist. (1985) Vol.34 (1)
  [2] Ferreira, D., et al. "Quantiles from the Maximum Studentized Range Distribution." Biometric Brazilian Journal 25.1 (2007): 117-135.
      http://jaguar.fcav.unesp.br/RME/fasciculos/v25/v25_n1/A8_Daniel.pdf (See qrange function)
</remarks>
*)


Function StudentRangeCDF(const q: double; const r, v: integer; const ci: integer = 1): double;  






(*<summary>Beta distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Shape parameter, real positive value.</param>
  <param name="b">Shape parameter, real positive value.</param>
  
  
<remarks>Calculates beta distribution mean, variance, skewness and kurtosis values using parameters a and b.
</remarks>
*)

procedure BetaStat(a, b: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Bernoulli distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="p">parameter, real positive value on closed intervali [0,1].</param>

  
<remarks>Calculates Bernoulli distribution mean, variance, skewness and kurtosis values using parameter p.
</remarks>
*)

procedure BernoulliStat(p: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Binomial distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="n">Defines number of trials. n must be a positive integer.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>

  
<remarks>Calculates binomial distribution mean, variance, skewness and kurtosis values using parameters n and p.
</remarks>
*)

procedure BinomStat(n: Integer; p: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Cauchy distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="b">Shape parameter, real positive value.</param>
  <param name="m">Location parameter, real value.</param>

  
<remarks>Calculates Cauchy distribution mean, variance, skewness and kurtosis values using parameters b and m.
</remarks>
*)

procedure CauchyStat(b, m: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Chi-squared distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>

  
<remarks>Calculates Chi-Squared distribution mean, variance, skewness and kurtosis values using parameter Nu.
</remarks>
*)

procedure ChiSquareStat(Nu: Integer; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Erlang distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>

  
<remarks>Calculates Erlang distribution mean, variance, skewness and kurtosis values using parameters k and Lambda.
</remarks>
*)

procedure ErlangStat(k: Integer; Lambda: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Exponential distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>
  
  
<remarks>Calculates exponential distribution mean, variance, skewness and kurtosis values using parameter Mu.
</remarks>
*)

procedure ExponentStat(Mu: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Fatigue life distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>
  
  
<remarks>Calculates Fatigue life distribution mean, variance, skewness nd kurtosis values using parameters Mu, gamma and beta.
</remarks>
*)

procedure FatigueLifeStat(Mu, gamma, beta: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>F(Fisher) distribution statistic parameters.</summary>	
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>
  
  
<remarks>Calculates Fisher-F distribution mean, variance, skewness and kurtosis values using parameters Nu1 and Nu2.
</remarks>
*)

procedure FStat(Nu1, Nu2: Integer; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Gamma distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="A">Defines distribution scale parameter. A must be a positive scalar.</param>
  <param name="B">Defines distribution shape parameter. B must be a positive scalar.</param>

  
<remarks>Calculates Gamma distribution mean, variance, skewness and kurtosis values using parameters a and b.
</remarks>
*)

procedure GammaStat(A, B: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Generalized Extreme value distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>

  
<remarks>Calculates Generalized Extreme value distribution mean, variance, skewness and kurtosis values using parameters k, Mu and sigma.
</remarks>
*)

procedure GenExtValueStat(k, Mu, sigma: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Generalized Pareto value distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="k">Defines distribution shape parameter, real value.</param>
  <param name="Mu">Defines distribution location parameter, real value.</param>
  <param name="sigma">Defines distribution scale parameter, positive real value.</param>
  
  
<remarks>Calculates Generalized Pareto distribution mean, variance, skewness and kurtosis values using parameters k, Mu and sigma.
</remarks>
*)

procedure GenParetoStat(k, Mu, sigma: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Geometric distribution statistic parameters.</summary>  
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>
  
  
<remarks>Calculates geometric distribution mean, variance, skewness and kurtosis values using parameter p.
</remarks>
*)

procedure GeometricStat(p: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Gumbel distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>

  
<remarks>Calculates Gumbel distribution mean, variance, skewness and kurtosis values using parameters Mu and beta.
</remarks>
*)

procedure GumbelStat(Mu, beta: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;

(*<summary>Hypergeometric distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>

  
<remarks>Calculates hypergeometric distribution mean, variance, skewness and kurtosis values using parameters M,K and N.
</remarks>
*)

procedure HypGeometricStat(M,K,N: Integer; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Inverse Chi-squared distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>
  
  
<remarks>Calculates Inverse Chi-squared distribution mean, variance, skewness and kurtosis values using parameters Mu and Lambda.
</remarks>
*)

procedure InverseChiSquareStat(Nu: Integer; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Inverse Gaussian distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>
  
  
<remarks>Calculates Inverse Gaussian distribution mean, variance, skewness and kurtosis values using parameters Mu and Lambda.
</remarks>
*)

procedure InverseGaussianStat(Mu, Lambda: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Johnson bounded (SB) distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  
  
<remarks>Calculates Johnson SB distribution mean, variance, skewness and kurtosis values using parameters gamma, delta, Lambda and xi.
</remarks>
*)

procedure JohnsonSBStat(gamma,delta,Lambda,xi: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Johnson unbounded (SU) distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>

  
<remarks>Calculates Johnson SU distribution mean, variance, skewness and kurtosis values using parameters gamma, delta, Lambda and xi.
</remarks>
*)

procedure JohnsonUBStat(gamma,delta,Lambda,xi: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Laplace distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>

  
<remarks>Calculates Laplace distribution mean, variance, skewness and kurtosis values using parameters m and b.
</remarks>
*)

procedure LaplaceStat(m,b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Logarithmic distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="p">Distribution parameter, real value on open interval (0,1).</param>

  
<remarks>Calculates logarithmic distribution mean, variance, skewness and kurtosis values using parameter p.
</remarks>
*)

procedure LogarithmicStat(p: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Logistic distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  
  
<remarks>Calculates Logistic distribution mean, variance, skewness and kurtosis values using parameters m and b.
</remarks>
*)

procedure LogisticStat(m,b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Log-Normal distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  
  
<remarks>Calculates log-normal distribution mean, variance, skewness and kurtosis values using parameters Mu and sigma.
</remarks>
*)

procedure LogNormalStat(Mu,sigma: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Log-Weibull distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  
  
<remarks>Calculates log-Weibull distribution mean, variance, skewness and kurtosis values using parameters a and b.
</remarks>
*)

procedure LogWeibullStat(a,b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Maxwell distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution parameter, positive real value.</param>
  
  
<remarks>Calculates negative binomial distribution mean, variance, skewness and kurtosis values using parameter a.
</remarks>
*)

procedure MaxwellStat(a: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;


(*<summary>Negaive binomial distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="p">Defines probability of each trial, real value on closed interval [0,1].</param>

  
<remarks>Calculates negative binomial distribution mean, variance, skewness and kurtosis values using parameters R and p.
</remarks>
*)

procedure NegBinomStat(R: double; p: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Normal distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>

  
<remarks>Calculates normal distribution mean, variance, skewness and kurtosis values using parameters Mu and sigma.
</remarks>
*)

procedure NormalStat(Mu, sigma: double; out AMean, AVariance, ASkewness, AKurtosis : double); overload;


(*<summary>Pareto distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>
  
  
<remarks>Calculates Pareto distribution mean, variance, skewness and kurtosis values using parameters a and b.
</remarks>
*)

procedure ParetoStat(a, b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Poisson distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>
    
  
<remarks>Calculates Poisson distribution mean, variance, skewness and kurtosis values using parameter Lambda.
</remarks>
*)

procedure PoissonStat(Lambda: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Power distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="alpha">Distribution shape parameter, real positive value.</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>
  
  
<remarks>Calculates Power distribution mean, variance, skewness and kurtosis values using parameters alpha and beta.
</remarks>
*)

procedure PowerStat(alpha, beta: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;


(*<summary>Rayleigh distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="b">Distribution parameter, positive real value.</param>
  
  
<remarks>Calculates Rayleigh distribution mean, variance, skewness and kurtosis values using parameter b.
</remarks>
*)

procedure RayleighStat(b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Student(T) distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>
  
  
<remarks>Calculates Student-t distribution mean, variance, skewness and kurtosis values using parameter Nu.
</remarks>
*)

procedure StudentStat(Nu: Integer; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Triangular distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>

  
  
<remarks>Calculates triangular distribution mean, variance, skewness and kurtosis values using parameters a,b and c.
</remarks>
*)

procedure TriangularStat(a,b,c: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Unniform distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="b">Distribution parameter, real value, defining distribution upper bound.</param>

  
<remarks>Calculates uniform distribution mean, variance, skewness and kurtosis values using parameters a and b.
</remarks>
*)

procedure UniformStat(a, b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Discrete uniform distribution statistic parameters.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="N">Distribution parameter, positive integer.</param>
  
  
<remarks>Calculates discrete uniform distribution mean, variance, skewness and kurtosis values using parameter N.
</remarks>
*)

procedure UniformDStat(N: Integer; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Weibull distribution moment estimates.</summary>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value.</param>*)

procedure WeibullStat(a, b: double; out AMean, AVariance, ASkewness, AKurtosis: double); overload;

(*<summary>Zipf distribution moment estimates.</summary>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name="AMean">Returns distribution mean estimate.</param>
  <param name="AVariance">Returns distribution variance estimate.</param>
  <param name="ASkewness">Returns distribution skewness estimate.</param>
  <param name="AKurtosis">Returns distribution kurtosis estimate.</param>*)

procedure ZipfStat(s: double; N: Integer; out AMean, AVariance, ASkewness, AKurtosis: double); overload;




(*<summary>Beta PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on open interval (0,1).</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BetaPDF(const X: TDenseMtxVec; A,B: double; const Res: TDenseMtxVec); overload;
(*<summary>Beta CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on open interval (0,1).</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BetaCDF(const X: TDenseMtxVec; A, B: double; const Res: TDenseMtxVec); overload;
(*<summary>Beta PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BetaCDFInv(const P: TDenseMtxVec; A, B: double; const Res: TDenseMtxVec); overload;
(*<summary>Binomial PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on closec interval [0,n].</param>
  <param name="n">Defines number of trials. n must be a positive integer.</param>
  <param name="P">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the PDF calculated using  X, n and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BinomPDF(const X: TMtxVecInt; n: Integer; P: double; const Res: TDenseMtxVec ); overload;
(*<summary>Binomial CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on closec interval [0,n].</param>
  <param name="n">Defines number of trials. n must be a positive integer.</param>
  <param name="P">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the CDF calculated using  X, n and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BinomCDF(const X: TMtxVecInt; n: Integer; P: double; const Res: TDenseMtxVec ); overload;
(*<summary>Binomial PPF (vectorized).</summary>
  <param name="Y">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="n">Defines number of trials. n must be a positive integer.</param>
  <param name="P">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the PPF calculated from Y, n and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure BinomCDFInv(const Y: TDenseMtxVec; n: integer; P: double; const Res: TDenseMtxVec); overload;
(*<summary>Bose-Einstein PDF (vectorized).</summary>*)

procedure BoseEinsteinPDF(const X: TDenseMtxVec; Mu, S: double; const Res: TDenseMtxVec); overload;
(*<summary>Cauchy PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure CauchyPDF(const X: TDenseMtxVec; m, b: double; const Res: TDenseMtxVec); overload;
(*<summary>Cauchy CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure CauchyCDF(const X: TDenseMtxVec; m, b: double; const Res: TDenseMtxVec); overload;
(*<summary>Cauchy PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="m">Location parameter, real value.</param>
  <param name="b">Shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure CauchyCDFInv(const P: TDenseMtxVec; m, b: double; const Res: TDenseMtxVec); overload;
(*<summary>Chi-Squared PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values.</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ChiSquarePDF(const X: TDenseMtxVec; Nu: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Chi-Squared CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values.</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ChiSquareCDF(const X: TDenseMtxVec; Nu: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Chi-Squared PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Nu">Defines distribution degrees of freedom. Nu must be a positive integer value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and  Nu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ChiSquareCDFInv(const P: TDenseMtxVec; Nu: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Erlang PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values or zero.</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, k and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ErlangPDF(const X: TDenseMtxVec; k: Integer; Lambda: double; const Res: TDenseMtxVec); overload;
(*<summary>Erlang CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values or zero.</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, k and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ErlangCDF(const X: TDenseMtxVec; k: Integer; Lambda: double; const Res: TDenseMtxVec); overload;
(*<summary>Erlang PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="k">Defines distribution shape parameter. k must be a positive integer.</param>
  <param name="Lambda">Defines distribution rate parameter. Lambda must be a positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, k and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure ErlangCDFInv(const P: TDenseMtxVec; k: Integer; Lambda: double; const Res: TDenseMtxVec); overload;
(*<summary>Exponential PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values or zero.</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and Mu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ExpPDF(const X: TDenseMtxVec; Mu: double; const Res: TDenseMtxVec); overload;
(*<summary>Exponential CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive values or zero.</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>
  <param name ="Res">After calculation stores the CDF calculated from X and Mu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ExpCDF(const X: TDenseMtxVec; Mu: double; const Res: TDenseMtxVec); overload;
(*<summary>Exponential PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Defines distribution rate parameter. Mu must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and Mu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure ExpCDFInv(const P: TDenseMtxVec; Mu: double; const Res: TDenseMtxVec); overload;
(*<summary>Fatigue Life PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real values on open interval (Mu,INF).</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu, gamma and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure FatigueLifePDF(const X: TDenseMtxVec; Mu, gamma, beta: double; const Res: TDenseMtxVec); overload;
(*<summary>Fatigue Life CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real values on open interval (Mu,INF).</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, Mu, gamma and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure FatigueLifeCDF(const X: TDenseMtxVec; Mu, gamma, beta: double; const Res: TDenseMtxVec); overload;
(*<summary>Fatigue Life PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Defines distribution location parameter.</param>
  <param name="gamma">Defines distribution shape parameter. gamma must be a positive scalar.</param>
  <param name="beta">Defines distribution scale parameter. beta must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, m, gamma and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure FatigueLifeCDFInv(const P: TDenseMtxVec; Mu, gamma, beta: double; const Res: TDenseMtxVec); overload;
(*<summary>Fisher(F) PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Nu1 and Nu2.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure FPDF(const X: TDenseMtxVec; Nu1, Nu2: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Fisher(F) CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, Nu1 and Nu2.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure FCDF(const X: TDenseMtxVec; Nu1, Nu2: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Fisher(F) PPF (vectorized).</summary>
  <param name="p">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Nu1">Defines distribution degrees of freedom. Nu1 must be a positive integer.</param>
  <param name="Nu2">Defines distribution degrees of freedom. Nu2 must be a positive integer.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, Nu1 and Nu2.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure FCDFInv(p: double; const Nu1, Nu2 : TMtxVecInt; const Res: TDenseMtxVec); overload;


procedure  FCDFInv(const p: TDenseMtxVec; Nu1, Nu2: Integer; const Res: TDenseMtxVec); overload;

(*<summary>Fermi-Dirac probability density function (PDF).</summary>
  <returns>Fermi-Dirac probability density function (PDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>
  <param name="y">Contains result.</param>

  
<remarks>Calculates the Fermi-Dirac PDF for x using the parameters Mu and s. The distribution arises in the study of half-integer spin particles in physics.
  Half-integer spin particles are described by Fermi-Dirac statistics and are called FERMIONS. Fermi-Dirac PDF is defined by the following equation:

  <IMG name="probabilities067"/>
</remarks>



  <seealso cref="BoseEinsteinPDF"/>*)

procedure FermiDiracPDF(const x: TDenseMtxVec; Mu, s: double; const y: TDenseMtxVec); overload;
(*<summary>Fermi-Dirac cumulative distribution function (CDF).</summary>
  <returns>Fermi-Dirac cumulative distribution function (CDF).</returns>
  <param name="x">Function domain, real value.</param>
  <param name="Mu">Location parameter, real value.</param>
  <param name="s">Shape parameter, real positive value.</param>
  <param name="y">Contains result.</param>

  <remarks>The distribution arises in the study of integer spin particles in physics.</remarks>*)

procedure FermiDiracCDF(const x: TDenseMtxVec; Mu, s: double; const y: TDenseMtxVec); overload;













(*<summary>Gamma distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real positive real values.</param>
  <param name="A">Defines distribution scale parameter. A must be a positive scalar.</param>
  <param name="B">Defines distribution shape parameter. B must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GammaPDF(const X: TDenseMtxVec; A, B : double; const Res: TDenseMtxVec); overload;
(*<summary>Gamma distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real positive real values.</param>
  <param name="A">Defines distribution scale parameter. A must be a positive scalar.</param>
  <param name="B">Defines distribution shape parameter. B must be a positive scalar.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GammaCDF(const X: TDenseMtxVec; A, B: double; const Res: TDenseMtxVec); overload;
(*<summary>Gamma distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="A">Defines distribution scale parameter. A must be a positive scalar.</param>
  <param name="B">Defines distribution shape parameter. B must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure GammaCDFInv(const P: TDenseMtxVec; A, B : double; const Res: TDenseMtxVec); overload;
(*<summary>Geometric distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive integers.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GeometricPDF(const X: TMtxVecInt; p: double; const Res: TDenseMtxVec); overload;
(*<summary>Geometric distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive integers.</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the CDF calculated from X and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GeometricCDF(const X: TMtxVecInt; p: double; const Res: TDenseMtxVec); overload;
(*<summary>Geometric distribution PPF (vectorized).</summary>
  <param name="Y">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="p">Defines success probability. p must lie on the [0,1] closed interval.</param>
  <param name ="Res">After calculation stores the PPF calculated from Y and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of Y.</param>*)

procedure GeometricCDFInv(const Y: TDenseMtxVec; p: double; const Res: TDenseMtxVec); overload;
(*<summary>Gumbel distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, th routine calculates minimum Gumbel PDF
  If false, the routine calculates maximum Gumbel PDF.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu, and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GumbelPDF(const X: TDenseMtxVec; Mu, beta : double; minimum: boolean; const Res: TDenseMtxVec); overload;
(*<summary>Gumbel distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, the routine calculates minimum Gumbel PDF. </param>
  <param name ="Res">After calculation stores the CDF calculated from X, Mu, and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure GumbelCDF(const X: TDenseMtxVec; Mu, beta: double; minimum: boolean; const Res: TDenseMtxVec); overload;
(*<summary>Gumbel distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Defines the location parameter.</param>
  <param name="beta">Defines the scale parameter, positive real value.</param>
  <param name="minimum">Defines maximum or minimum Gumbel distrubution. If true, the routine calculates minimum Gumbel PDF. </param>
  <param name ="Res">After calculation stores the PPF calculated from P, Mu and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure GumbelCDFInv(const P: TDenseMtxVec; Mu, beta : double; minimum: boolean; const Res: TDenseMtxVec); overload;
(*<summary>Hypergeometric distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, with integer values on closed interval [0,N].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>
  <param name ="Res">After calculation stores the PDF calculated from X, K, M and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure HypGeometricPDF(const X: TMtxVecInt; M, K, N: Integer; const Res: TDenseMtxVec); overload;
(*<summary>Hypergeometric distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, with integer values on closed interval [0,N].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>
  <param name ="Res">After calculation stores the CDF calculated from X, K, M and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure HypGeometricCDF(const X: TMtxVecInt; M, K, N: Integer; const Res: TDenseMtxVec); overload;
(*<summary>HyperGeometric distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="M">Defines total number of elements, valid values ae integers on closed interval [0,X].</param>
  <param name="K">Defines number of elements with certain traits, valid values are integers on closed interval [X,M].</param>
  <param name="N">Defines number of samples, valid values are integers on closed interval [X,M].</param>
  <param name ="Res">After calculation stores the PPF calculated from P, K, M and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure HypGeometricCDFInv(const P: TDenseMtxVec; M, K, N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Inverse Gaussian distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with positive real values.</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu, and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure InverseGaussianPDF(const X: TDenseMtxVec; Mu, Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Inverse Gaussian distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with positive real values.</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, Mu, and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure InverseGaussianCDF(const X: TDenseMtxVec; Mu, Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Inverse Gaussian distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Defines distribution Mu parameter. Mu must be a positive scalar.</param>
  <param name="Lambda">Defines distribution Lambda parameter. Lambda must be a positive scalar.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, Mu and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure InverseGaussianCDFInv(const P: TDenseMtxVec; Mu, Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson bounded (SB) distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values on closed interval <c>[xi,xi+Lambda]</c>.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, agmma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure JohnsonSBPDF(const X: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson bounded (SB) distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values on closed interval <c>[xi,xi+Lambda]</c>.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, agmma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure JohnsonSBCDF(const X: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson bounded (SB) distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, gamma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure JohnsonSBCDFInv(const P: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson unbounded (SU) distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, agmma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure JohnsonUBPDF(const X: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson unbounded (SU) distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values.</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, agmma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure JohnsonUBCDF(const X: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Johnson unbounded (SU) distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="gamma">Distribution shape parameter, real value.</param>
  <param name="delta">Distribution shape parameter, real positive value.</param>
  <param name="Lambda">Distribution scale parameter, real positive value.</param>
  <param name="xi">Distribution location parameter, real value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, gamma, delta, Lambda and xi.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure JohnsonUBCDFInv(const P: TDenseMtxVec; gamma,delta,Lambda,xi: double; const  Res: TDenseMtxVec); overload;
(*<summary>Laplace distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LaplacePDF(const X: TDenseMtxVec; m, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Laplace distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with real values.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LaplaceCDF(const X: TDenseMtxVec; m, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Laplace distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure LaplaceCDFInv(const P: TDenseMtxVec; m, b : double; const  Res: TDenseMtxVec); overload;
(*<summary>Logistic PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LogisticPDF(const X: TDenseMtxVec; m, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Logistic CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure  LogisticCDF(const X: TDenseMtxVec; m, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Logistic distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="m">Distribution location parameter, real value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, m and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure  LogisticCDFInv(const P: TDenseMtxVec; m, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Normal PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LogNormalPDF(const X: TDenseMtxVec; Mu, sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Normal CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LogNormalCDF(const X: TDenseMtxVec; Mu,sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Normql distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure LogNormalCDFInv(const P: TDenseMtxVec; Mu, sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Weibull PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real values.</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LogWeibullPDF(const X: TDenseMtxVec; a,b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Weibull CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with real values.</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure LogWeibullCDF(const X: TDenseMtxVec; a,b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Log-Weibull distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="a">Distribution location parameter, real positive value.</param>
  <param name="b">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure LogWeibullCDFInv(const P: TDenseMtxVec; a,b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Maxwell PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="a">Distribution parameter, positive real value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and a.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure MaxwellPDF(const X: TDenseMtxVec; a: double; const  Res: TDenseMtxVec); overload;
(*<summary>Maxwell CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values or zeros.</param>
  <param name="a">Distribution parameter, positive real value.</param>
 <param name ="Res">After calculation stores the CDF calculated from X and a.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure MaxwellCDF(const X: TDenseMtxVec; a: double; const  Res: TDenseMtxVec); overload;
(*<summary>Negative binomial distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="a">Distribution parameter, positive real value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and a.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure MaxwellCDFInv(const P: TDenseMtxVec; a: double; const  Res: TDenseMtxVec); overload;
(*<summary>Negative binomial distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with integer values.</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="P">Defines probability of each trial, real value on closed interval [0,1].</param>
  <param name ="Res">After calculation stores the PDF calculated from X, R and P
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NegBinomPDF(const X: TMtxVecInt; R: double; P: double; const  Res: TDenseMtxVec); overload;
(*<summary>Negative binomial distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with integer values.</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="P">Defines probability of each trial, real value on closed interval [0,1].</param>
  <param name ="Res">After calculation stores the CDF calculated from X, R and P
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NegBinomCDF(const X: TMtxVecInt; R: double; P: double; const  Res: TDenseMtxVec); overload;
(*<summary>Negative binomial distribution PPF (vectorized).</summary>
  <param name="Y">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="R">Defines number of successes, positive integer.</param>
  <param name="P">Defines probability of each trial, real value on closed interval [0,1].</param>
  <param name ="Res">After calculation stores the PPF calculated from Y, R and p.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure NegBinomCDFInv(const Y: TDenseMtxVec; R: double; P: double; const  Res: TDenseMtxVec); overload;
(*<summary>Normal distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalPDF(const X: TDenseMtxVec; Mu, sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Normal distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real vector or matrix.</param>
  <param name="sigma">Distribution scale parameter, real positive vector or matrix.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalPDF(const X, Mu, sigma: TDenseMtxVec; const Res: TDenseMtxVec); overload;
(*<summary>Normal distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, mi and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalCDF(const X: TDenseMtxVec; Mu,sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Normal distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real vector or matrix.</param>
  <param name="sigma">Distribution scale parameter, real positive vector or matrix.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, mi and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalCDF(const X, Mu, sigma, Res: TDenseMtxVec); overload;

(*<summary>Normal distribution CDF (vectorized) two-tailed.</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real vector or matrix.</param>
  <param name="sigma">Distribution scale parameter, real positive vector or matrix.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, mi and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalCDFTwoTail(const X, Mu, sigma: TDenseMtxVec; const Res: TDenseMtxVec); overload;
(*<summary>Normal distribution CDF (vectorized) two-tailed.</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, mi and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure NormalCDFTwoTail(const X: TDenseMtxVec; const Mu, sigma: double; const Res: TDenseMtxVec); overload;
(*<summary>Normal distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Mu">Distribution location parameter, real value.</param>
  <param name="sigma">Distribution scale parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, Mu and sigma.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure NormalCDFInv(const P: TDenseMtxVec; Mu, sigma: double; const  Res: TDenseMtxVec); overload;
(*<summary>Pareto distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ParetoPDF(const X: TDenseMtxVec; a, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Pareto distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix.</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ParetoCDF(const X: TDenseMtxVec; a, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Pareto distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="a">Distribution scale parameter, real positive value.</param>
  <param name="b">Distribution shape parameter, real positive value, smaller than x.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure ParetoCDFInv(const P: TDenseMtxVec; a, b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Poisson distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, positive integer values or zeros.</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, alpha and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure PoissonPDF(const X: TMtxVecInt; Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Poisson distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, positive integer values or zeros.</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, alpha and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure PoissonCDF(const X: TMtxVecInt; Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Poisson distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Lambda">Distribution parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and Lambda.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure PoissonCDFInv(const P: TDenseMtxVec; Lambda: double; const  Res: TDenseMtxVec); overload;
(*<summary>Power distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, positive real values on closed interval [0,1/beta].</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, alpha and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure PowerPDF(const X: TDenseMtxVec; alpha, beta: double; const  Res: TDenseMtxVec); overload;
(*<summary>Power distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, positive real values on closed interval [0,1/beta].</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, alpha and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure PowerCDF(const X: TDenseMtxVec; alpha, beta: double; const  Res: TDenseMtxVec); overload;
(*<summary>Power distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="alpha">Distribution shape parameter, real positive value..</param>
  <param name="beta">Distribution scale parameter, positive real number.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, alpha and beta.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure PowerCDFInv(const P: TDenseMtxVec; alpha, beta: double; const  Res: TDenseMtxVec); overload;
(*<summary>Rayleigh distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, positive real values or zeros.</param>
  <param name="b">Distribution parameter, positive real value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure RayleighPDF(const X: TDenseMtxVec; b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Rayleigh distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, positive real values or zeros.</param>
  <param name="b">Distribution parameter, positive real value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure RayleighCDF(const X: TDenseMtxVec; b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Rayleigh distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="b">Distribution parameter, positive real value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure RayleighCDFInv(const P: TDenseMtxVec; b: double; const  Res: TDenseMtxVec); overload;
(*<summary>Student(T) distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, real values.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and Nu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure StudentPDF(const X: TDenseMtxVec; Nu: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Student(T) distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, real values.</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>
  <param name ="Res">After calculation stores the CDF calculated from X and Nu.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure StudentCDF(const X: TDenseMtxVec; Nu: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Student(T) distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="Nu">Defines distribution degrees of freedom, positive integer.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and Nz.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure StudentCDFInv(const P: TDenseMtxVec; Nu: Integer; const  Res: TDenseMtxVec); overload;

(*<summary>Triangular distribution PDF (vectorized).</summary>
  <param name="X">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>
  <param name ="Res">After calculation stores the PDF calculated from X, a, b and c.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure TriangularPDF(const X: TDenseMtxVec; a,b,c: double; const  Res: TDenseMtxVec); overload;
(*<summary>Triangular distribution CDF (vectorized).</summary>
  <param name="X">Distribution domain, real value on closed interval [a,b].</param>
  <param name="a">Distribution location parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>
  <param name ="Res">After calculation stores the CDF calculated from X, a, b and c.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure TriangularCDF(const X: TDenseMtxVec; a,b,c: double; const  Res: TDenseMtxVec); overload;
(*<summary>Triangular distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="a">Distribution locagtion parameter, real value, smaller than parameter b.</param>
  <param name="b">Distribution scale parameter, real value, greater than parameter a.</param>
  <param name="c">Distribution shape parameter, real value on closed interval [a,b].</param>
  <param name ="Res">After calculation stores the PPF calculated from P, a,b and c.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure TriangularCDFInv(const P: TDenseMtxVec; a,b,c: double; const  Res: TDenseMtxVec); overload;
(*<summary>Uniform distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with values on closed interval [A,B].</param>
  <param name="A">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="B">Distribution parameter, real value, defining distribution upper bound.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure UniformPDF(const X: TDenseMtxVec; A, B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Discrete uniform distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with values on closed interval [A,B].</param>
  <param name="A">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="B">Distribution parameter, real value, defining distribution upper bound.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure UniformCDF(const X: TDenseMtxVec; A, B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Uniform distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="A">Distribution parameter, real value, defining distribution lower bound.</param>
  <param name="B">Distribution parameter, real value, defining distribution upper bound.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, A and B.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure UniformCDFInv(const P: TDenseMtxVec; A, B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Discrete uniform distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with integer values on closed interval [1,N].</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the PDF calculated from X and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure UniformDPDF(const X: TMtxVecInt; N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Discrete uniform distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, vector or matrix with integer values on closed interval [1,N].</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the CDF calculated from X and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure UniformDCDF(const X: TMtxVecInt; N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Discrete uniform distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the PPF calculated from P and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure UniformDCDFInv(const P: TDenseMtxVec; N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Weibull distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values.</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure WeibullPDF(const X: TDenseMtxVec; A,B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Weibull distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with positive real values.</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the CDF calculated from X, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure WeibullCDF(const X: TDenseMtxVec; A,B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Weibull distribution PPF (vectorized).</summary>
  <param name="P">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="A">Distribution scale parameter, real positive value.</param>
  <param name="B">Distribution shape parameter, real positive value.</param>
  <param name ="Res">After calculation stores the PPF calculated from P, a and b.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure WeibullCDFInv(const P: TDenseMtxVec; A,B: double; const  Res: TDenseMtxVec); overload;
(*<summary>Zipf distribution PDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on interval [1,Inf).</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the PDF calculated from X,s and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ZipfPDF(const X: TMtxVecInt; s: double; N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Zipf distribution CDF (vectorized).</summary>
  <param name="X">Defines distribution domain, real vector or matrix with integer values on interval [1,Inf).</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the CDF calculated from X,s and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of X.</param>*)

procedure ZipfCDF(const X: TMtxVecInt; s: double; N: Integer; const  Res: TDenseMtxVec); overload;
(*<summary>Zipf distribution PPF (vectorized).</summary>
  <param name="p">Defines distribution probabilities, real vector or matrix with values within closed interval [0,1].</param>
  <param name="s">Distribution parameter, real positive value.</param>
  <param name="N">Distribution parameter, positive integer.</param>
  <param name ="Res">After calculation stores the PPF calculated from P,s and N.
    Length and Complex properties of Res are adjusted automatically to match Length and Complex properties of P.</param>*)

procedure ZipfCDFInv(const p: TDenseMtxVec; s: double; N: Integer; const  Res: TDenseMtxVec); overload;


(*<summary> Returns digamma function or psi(0,x) </summary>
            
<remarks>Computes the first derivate of the logarithm of the gamma function.
            The function returns NAN, if the x is less than zero.

            The algorithm was adapted from:

            The algorithm "AS 103" from "Applied Statistics" (1976) VOL.25, NO.3
            Available in Fortran at: http://lib.stat.cmu.edu/apstat/103
</remarks>
*)

function digamma(x: double): double;


(*<summary> Returns trigamma function or psi(1,x) </summary>
            
<remarks>Computes the second derivate of the logarithm of the gamma function.
            The function returns NAN, if the x is less than or equal to zero.

            The algorithm was adapted from:

            The algorithm "AS 121" from "Applied Statistics" (1978) vol 27, no. 1
            Available in Fortran at: http://lib.stat.cmu.edu/apstat/121
</remarks>
*)

function trigamma(x: double): double;


(*<summary> Returns psi function of x.  </summary>
            
<remarks>Computes the second or third derivate of the logarithm of the gamma function.
            The function returns NAN, if the x is less than or equal to zero.
            If derivate parameter is 0, the function returns digamma(x).
            If the derivate parameter is 1 then the function returns trigamma(x).
            Other values for derivate parameter are not supported.
</remarks>
*)

function psi(derivate: integer; x: double): double;





