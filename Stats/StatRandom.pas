











(*<summary>Random number generators.</summary>
  
<remarks>Introduces several pseudo random number generators.
</remarks>
*)
unit StatRandom;


{$I bdsppdefs.inc}

interface

uses MtxVec, Math387, AbstractMtxVec, AbstractMtxVecInt
    
    ,RndGenerators 
    
    
    ,SysUtils
    
    ;


(*<summary>Beta distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the beta
  distribution with parameters a and b. The random numbers are stored in the Results vector.
  An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the beta
	  distribution with parameters a=2.5 and b=3.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomBeta(2.5, 3,r,-1);
     }
  }
  </code></example>*)
procedure RandomBeta(const A, B: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Binomial distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the binomial distribution
  with parameter p. The random numbers are stored in the Results vector. An exception is raised
  if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the binomial
	  distribution with parameters N=4 and p=0.2.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       VectorInt r = new VectorInt(2000);
       StatRandom.RandomBinom(4, 0.2, r,-1);
     }
  }
  </code></example>*)
procedure RandomBinom(const AN: Integer; const P: double; const Results: TMtxVecInt; Seed: Integer = -1);
(*<summary>ChiSquared distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the chi-squared
  distribution with parameter ANu. The random numbers are stored in the Results vector. An
  exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the chi-squared
	  distribution with parameter Nu=7.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomChiSquare(7, r,-1);
     }
  }
  </code></example>*)
procedure RandomChiSquare(const ANu: Integer; const Results: TDenseMtxVec; const Seed: Integer = -1);
(*<summary>Cauchy distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Cauchy
  distribution with parameters b and m. The random numbers are stored in the Results vector. An
  exception is raised if Result vector is complex.
</remarks>


    

  <example>
    Generate 2000 random numbers by using the Generalized Pareto distribution with
    parameters <c>m=1.2</c>, <c>b=0.5</c>.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomCauchy(1.2, 0.5, r, -1);
     }
  }
  </code></example>*)
procedure RandomCauchy(const m,b : double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Erlang distribution random generator.</summary>
  
<remarks>Generates Result.Length pseudo random numbers, using the Erlang distribution
  with parameters k and lambda. The random numbers are stored in the Results vector. An exception is raised
  if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Erlang distribution with
	  parameters k = 2, lambda=1.23.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomErlang(2,1.23, r,-1);
     }
  }
  </code></example>*)
procedure RandomErlang(const k: Integer; const lambda: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Exponent distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the exponential distribution
  with parameter mu. The random numbers are stored in the Results vector. An exception is raised
  if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the exponential
	  distribution with parameter mu=1.23.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomExponent(1.23, r,-1);
     }
  }
  </code></example>*)
procedure RandomExponent(const mu: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>F distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the F
  distribution with parameters ANu1 and ANu2. The random numbers are stored
  in the Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the F distribution with parameters Nu1=2 and Nu2=5.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
        Vector r = new Vector(2000,false);
        StatRandom.RandomF(2, 5, r, -1);
     }
  }
  </code></example>*)
procedure RandomF(const ANu1, ANu2: Integer; const Results: TDenseMtxVec; const Seed: Integer = -1);
(*<summary>Gamma distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the gamma distribution
  with parameters a and b. The random numbers are stored in the Results vector. An
  exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the gamma
	  distribution with parameters Nu1=4 and Nu2=0.25.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomGamma(4, 0.25, r, -1);
     }
  }
  </code></example>*)
procedure RandomGamma(const A, B: double; const Results: TDenseMtxVec; Seed: Integer = -1);

(*<summary>Generalized Pareto distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Generalized Pareto
  distribution with parameters k, mu and sigma. The random numbers are stored in the Results vector.
  An  exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Generalized Pareto distribution with
    parameters k=1.2, mu=0 and sigma=0.5.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomGenPareto(1.2, 0, 0.5, r, -1);
     }
  }
  </code></example>*)
procedure RandomGenPareto(const k, mu, sigma: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Geometric distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the geometric
  distribution with parameter P. The random numbers are stored in the Results vector.
  An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the geometric distribution with parameters P=0.6.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomGeometric(0.6, r, -1);
     }
  }
  </code></example>*)
procedure RandomGeometric(const P: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Gumbel distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Gumbel
  distribution with parameters mu and beta. The random numbers are stored in the Results vector.
  An exception is raised if Result vector is complex or beta is less or equal to zero.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Gumbel
	  distribution with parameters mu=1, beta = 0.55.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomGumbel(1.0, 0.55, r, -1);
     }
  }
  </code></example>*)
procedure RandomGumbel(const mu, beta: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Hypergeometric distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the hypergeometric distribution
  with parameters M (lot size), K (size of sampling) and N (marked elements in the lot).
  The random numbers are stored in the Results vector. An exception
  is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the hypergeometric
	  distribution with parameters M=100, K=50 and N=30.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       VectorInt r = new VectorInt(2000);
       StatRandom.RandomHypGeometric(100,50,30,r, -1);
     }
  }
  </code></example>*)
procedure RandomHypGeometric(const M, K, N: Integer; const Results: TMtxVecInt; Seed: Integer = -1);
(*<summary>Laplace distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Laplace
  distribution with parameters m and b. The random numbers are stored in the Results vector. An
  exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Laplace
	  distribution with parameters m=4.2, b=2.4.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomLaplace(4.2, 2.4,r, -1);
     }
  }
  </code></example>*)
procedure RandomLaplace(const m,b : double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Logistic distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the lognormal
  distribution with parameters mu and sigma. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the logistic
	  distribution with parameters m=3.0 and b=2.4.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomLogistic(3.0, 2.4, r, -1);
     }
  }
  </code></example>*)
procedure RandomLogistic(const m, b: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Lognormal distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the lognormal
  distribution with parameters mu and sigma. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the lognormal
	  distribution with parameters Mu=3.0 and Sigma=2.4.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomLogNormal(3.0, 2.4, r, -1);
     }
  }
  </code></example>*)
procedure RandomLogNormal(const mu, sigma: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Maxwell distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Maxwell
  distribution with parameter a. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Maxwell distribution with parameter a=3.0.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandoMaxwell(3.0, r, -1);
     }
  }
  </code></example>*)
procedure RandomMaxwell(const a: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Negative binomial distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the hypergeometric
  distribution with parameters M, K and N. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the hypergeometric
	  distribution with parameters M=100, K=50 and N=30.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       VectorInt r = new VectorInt(2000);
       StatRandom.RandomNegBinom(10,0.2,r, -1);
     }
  }
  </code></example>*)
procedure RandomNegBinom(const R,p: double; const Results: TMtxVecInt; Seed: Integer = -1);
(*<summary>Normal distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the normal
  distribution with parameters mu and sigma. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the normal
	  distribution with parameters mu=3.5 and sigma=0.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomNormal(0.0, 1.0 ,r, -1);
     }
  }
  </code></example>*)
procedure RandomNormal(const mu, sigma: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Poisson distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Poisson
  distribution with parameter lambda. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Poisson
	  distribution with parameter lambda=2.31.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       VectorInt r = new VectorInt(2000);
       StatRandom.RandomPoisson(2.31,r,-1);
     }
  }
  </code></example>*)
procedure RandomPoisson(const lambda: double; const Results: TMtxVecInt; Seed: Integer = -1);
(*<summary>Rayleigh distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Rayleigh
  distribution with parameter b. The random numbers are stored in the Results
  vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using Rayleigh distribution
    with parameters P=0.6.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomRayleigh(0.6,r,-1);
     }
  }
  </code></example>*)

procedure RandomRayleigh(const B: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Student(T) distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Student (t)
  distribution with parameter V. The random numbers are stored in the Results
  vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Student (t)
	  distribution with parameter V = 5.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomStudent(5,r,-1);
     }
  }
  </code></example>*)

procedure RandomStudent(const V: Integer; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Triangular distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the triangular
  distribution with parameters a,b and c. The random numbers are stored in
  the Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the triangular distribution witg parameters
    <c>a=2.2</c>, <c>b=8</c> and <c>c=5.5</c>.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomTriangular(2.2, 8.0, 5.5, r, -1);
     }
  }
  </code></example>*)

procedure RandomTriangular(const a, b,c : double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Uniform distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the continuous
  uniform distribution with parameters a and b. The random numbers are stored in
  the Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the continuous
	  uniform distribution with parameters a=-1 and b=1.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomUniform(-1,1,r,-1);
     }
  }
  </code></example>*)

procedure RandomUniform(const a, b: double; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Discrete uniform distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the discrete
  uniform distribution with parameter N. The random numbers are stored in the
  Results vector. An exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the discrete
	  uniform distribution with parameter N=30.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomUniformD(30, r,-1);
     }
  }
  </code></example>*)
procedure RandomUniformD(const N: Integer; const Results: TDenseMtxVec; Seed: Integer = -1);
(*<summary>Weibull distribution random generator.</summary>
  
<remarks>This routine generates Result.Length pseudo random numbers, using the Weibull distribution
  with parameters a and b. The random numbers are stored in the Results vector. An
  exception is raised if Result vector is complex.
</remarks>


  

  <example>
    Generate 2000 random numbers by using the Weibull
	  distribution with parameters a=3 and b=1.2.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
     private void Example()
     {
       Vector r = new Vector(2000,false);
       StatRandom.RandomWeibull(3, 1.2, r,-1);
     }
  }
  </code></example>*)
procedure RandomWeibull(const a, b: double; const Results: TDenseMtxVec; Seed: Integer = -1);




