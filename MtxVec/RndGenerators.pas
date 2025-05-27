











(*<summary>Nmkl random generators interface.</summary>*)
unit RndGenerators;


interface

{$I BdsppDefs.inc }
{$Z4}

uses Math387, AbstractMtxVec, MtxVec, MtxVecInt, AbstractMtxVecInt
    ,RndImport  
    
    
    ,Classes
    
    ;



type

  (*<summary>Defines available random generator algorithms.</summary>*)
  TBasicRandomGenerator = (
    (*<summary>MCG31m1</summary>
    
<remarks>32-bit linear congruential generators, which also include MCG31m1 [14], are still used as default
    RNGs in various systems mostly due to simplicity of implementation, speed of operation, and
    compatibility with earlier versions of the systems. However, their period lengths do not meet the
    requirements for modern basic random number generators.

    Nevertheless, MCG31m1 possesses good statistical properties and may be used to advantage in
    generating random numbers of various distribution types for relatively small samplings.
</remarks>
*)
    rgMCG31,
    (*<summary>R250</summary>
    
<remarks>R250 is a generalized feedback shift register generator. Feedback shift register generators possess
    extensive theoretical footing and were first considered as RNGs for cryptographic and
    communications applications. Generator R250 proposed in [10] is fast and simple in
    implementation.

    It is common in the field of physics. However, the generator fails a number of
    tests, a 2D self-avoiding random walk [21] being an example.
</remarks>
*)
    rgR250,
    (*<summary>MRG32K3A</summary>
    
<remarks>A combined generator MRG32k3a [13] meets the requirements for modern RNGs: good
    multidimensional uniformity, fairly large period, etc. Besides, being optimized for various Intel®
    architectures, this generator rivals the other VSL BRNGs in speed.
</remarks>
*)
    rgMRG32K3A,
    (*<summary>MCG59</summary>
    
<remarks>A multiplicative congruential generator MCG59 is one of the two basic generators implemented in
    NAG Numerical Libraries [18] (see www.nag.co.uk). Since the module of this generator is not
    prime, its period length is not 259, but just 257, if the seed is an odd number.

    A drawback of such generators is well-known (for example, see [11], [12]): the lower bits of the output sequence are
    not random, therefore breaking numbers down into their bit patterns and using individual bits may
    cause trouble. Besides, block-splitting of the sequence over the entire period into 2d similar blocks
    results in full coincidence of such blocks in d lower bits (see, for instance, [11], [12]).
</remarks>
*)
    rgMCG59,
    (*<summary>WH</summary>
    
<remarks>WH is a set of 273 different basic generators. It is the second basic generator in NAG libraries.
    The constants ai,j are in the range 112 to 127 and the constants mi,j are prime numbers in the range
    16718909 to 16776971, which are close to 224. These constants have been chosen so that they give
    good results with the spectral test, see [11] and [15]. The period of each Wichmann–Hill generator
    would be at least 292, if it were not for common factors between (m1,j–1), (m2,j–1), (m3,j–1), and
    (m4,j–1). However, each generator should still have a period of at least 280.

    Further discussion of the properties of these generators is given in [15], which shows that the generated
    pseudo-random sequences are essentially independent of one another according to the spectral test.
</remarks>
*)
    rgWH,
    (*<summary>SOBOL</summary>
    
<remarks>Bratley and Fox [4] provide an implementation of the Sobol quasi-random number generator. VSL
    implementation allows generating Sobol’s low-discrepancy sequences of length up to 232. The
    dimension of quasi-random vectors can vary from 1 to 40 inclusive.
</remarks>
*)
    rgSOBOL,
    (*<summary>NIEDERR</summary>
    
<remarks>According to the results of Bratley, Fox, and Niederreiter [5] Niederreiter’s sequences have the
    best known theoretical asymptotic properties. VSL implementation allows generating
    Niederreiter’s low-discrepancy sequences of length up to 232. The dimension of quasi-random
    vectors can vary from 1 to 318 inclusive.

    VSL provides an option of registering one or more new basic generators that you see as preferable
    or more reliable. Use them in the same way as the BRNGs available with VSL. The registration
    procedure makes it easy to include a variety of user-designed generators.
</remarks>
*)
    rgNIEDERR,
    (*<summary>MT19937</summary>
    
<remarks>Mersenne Twister pseudorandom number generator.
    Properties of the algorithm (the period length equal to 219937-1 and
    623-dimensional equidistribution up to 32-bit accuracy) make this generator
    applicable for simulations in various fields of science and engineering.
</remarks>
*)
    rgMT19937,
    (*<summary>MT2203</summary>
    
<remarks>The set of 1024 MT2203 pseudorandom number generators is an addition to MT19937
    generator intended for application in large scale Monte Carlo simulations performed on
    distributed multi-processor systems. Every MT2203
    generator has a period length equal to 22203-1 and possesses 68-dimensional
    equidistribution up to 32-bit accuracy.
</remarks>
*)
    rgMT2203,
    rgIAbstract,
    rgDAbstract,
    rgSAbstract,
    rgSFMT19937,
    rgNONDETERM,
    rgARS5,
    rgPHILOX4X32X10
    );


    (*<summary>Gaussian (normal) distribution random generator options.</summary>*)
    TGaussRandMethod = (
      (*<summary>Box Muller</summary>
      
<remarks>Generates a normal distributed random number x via a pair of uniforms u1, u2
      according to the formula <c>x=sqrt(-ln(u1))*sin(2*Pi*u2)</c>
</remarks>
*)	  
      grmBoxMuller = 0 ,
	  
      (*<summary>Box Muller 2</summary>
      
<remarks>Generates a pair of normal distributed random numbers x1, x2
      via a pair of uniforms u1, u2 according to the formulas<para/>

      <c>x1=sqrt(-ln(u1))*sin(2*Pi*u2)</c><para/>
      <c>x2=sqrt(-ln(u1))*cos(2*Pi*u2)</c><para/>.

      Method implemented so that it correctly processes odd vector
      length, i.e. if a call ends with generating x1, the next call starts
      from generating x2.
</remarks>
*)	  
      grmBoxMuller2 = 1,
	  
	  (*<summary>ICDF</summary>*)
      grmICDF = 2
      );

    (*<summary>Possion distribution random generator options.</summary>*)
     TPoissonRandMethod = (
      (*<summary>PTPE</summary>
        
<remarks>If lambda &gt;= 27, acceptance/rejection method is used with decomposition onto 4 regions:
       * 2 combined parallelograms, triangle,
       * left exponential tail and
       * right exponential tail.
      Othewise table lookup method is used.
</remarks>
*)
      prmPTPE = 0,
      (*<summary>Normal</summary>
      
<remarks>For lambda &gt;= 1 method is based on Poisson inverse CDF approximation via
      Gaussian (normal) inverse CDF; for lambda &lt; 1 table lookup method is used.
</remarks>
*)
      prmNormal = 1);

     (*<summary>Defines the method used to get random number series independence.</summary>
       
<remarks>Defines the method used to obtain several independent random streams
       using the same random generator with the same seed. If the method
       is not supported by the specified random basic random generator, an error will be raised.
</remarks>
*)
     TRngStreamIndependence = (
      (*<summary>None</summary>
      		
<remarks>All random streams will return the series of numbers.
</remarks>
*)
      rsiNone,
      (*<summary>Leap-frog</summary>
        
<remarks>Random streams will be sample interleaved. If there are three random streams defined,
        they will be filled from one continues random number stream like this:<para/>

        <c>Original : 0 1 2 3 4 5 6 7 8 9 10 11</c><para/>
        <c>Leapfrog 1: 0 3 6 9</c><para/>
        <c>Leapfrog 2: 1 4 7 10</c><para/>
        <c>Leapfrog 3: 2 5 8 11</c><para/>

        Leapfroging is not very suitable, if there is a need for a large number
        of independent streams.
</remarks>
*)
      rsiLeapfrog,
      (*<summary>Block-interleaved</summary>
        
<remarks>Random streams will be block interleaved. The following example is for block size 2 and
        random stream count 3. The block size is independent from the number of samples that are
        generated on each call from each of the streams.<para/>

        <c>Original :  0 1 2 3 4 5 6 7 8 9 10 11</c><para/>
        <c>Leapfrog 1: 0  1  6  7</c><para/>
        <c>Leapfrog 2: 2  3  8  9</c><para/>
        <c>Leapfrog 3: 4  5 10 11</c><para/>
</remarks>
*)
      rsiBlockSplitting);

     (*<summary>Random stream class.</summary>*)
     TRngStream = class(TPersistent)
     strict private
        fRngStream: PAPointer;
     strict protected
        procedure StatusCheck(const aStatus: integer);
        procedure CheckNewStream(const rStatus: integer);
     public
        property RngStream: PAPointer read fRngstream;
        procedure Delete;
        procedure NewStream(rngType: integer; Seed: cardinal);
        
        (*<summary>Saves random generator stream state to Stream.</summary>*)
        procedure SaveToStream(const Dst: TStream); virtual;
        (*<summary>Loads random generator stream state from Stream.</summary>*)
        procedure LoadFromStream(const Src: TStream); virtual;
        
        (*<summary>Copies the state from another stream.</summary>*)
        procedure CopyState(const Src: TRngStream);
        (*<summary>Copies entire descriptor from another random stream.</summary>*)
        procedure Copy(const Src: TRngStream);

        (*<summary>Generates an array of samples distributed by Cauchy distribution.</summary>
        
<remarks>Returns random numbers distributed according to to the Cauchy probability density function (PDF)
        as defined here <see cref="Probabilities.CauchyPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomCauchy(const Dst: TMtxVec; const mu,b: double);

        (*<summary>Generates an array of samples distributed by Uniform distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Uniform probability density function (PDF)
        as defined here <see cref="Probabilities.UniformPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomUniform(const Dst: TMtxVec; const a,b: double);

       (*<summary>Generates an array of samples distributed by Gaussian distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Gaussian probability density function (PDF)
        as defined here <see cref="Probabilities.NormalPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomGaussian(const Dst: TMtxVec; const mu ,sigma: double; const Method: TGaussRandMethod = grmBoxMuller);

        (*<summary>Generates random numbers from multivariate normal distribution.</summary>
          
<remarks>The function generates random numbers with d-variate normal (Gaussian) distribution with
          Mean value and variance-covariance matrix Sigma, where Sigma is a dxd symmetric positive-definite matrix in full storage.
          The number of d-dimensional vectors to be generated is defined with Dst.Rows, the dimension d with Dst.Cols. Mean must
          be equal to Dst.Cols and Sigma.Rows and Sigma.Cols also equal to Dst.Cols.
</remarks>
*)
        procedure RandomGaussianMV(const Dst: TMtx; const Mean: TVec; const Sigma: TMtx; const Method: TGaussRandMethod = grmBoxMuller);

        (*<summary>Generates an array of samples distributed by Exponential distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Exponential probability density function (PDF)
        similar to as defined here <see cref="Probabilities.ExpPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
        The probability density function is defined as:

        <code>
                  1           (x-a)
        f(x) = ------  exp( - ---- )  , x &gt;= a
                mu            mu

        f(x) =  0,  x &lt; a
        </code>
</remarks>
*)
        procedure RandomExponential(const Dst: TMtxVec; const a,mu: double);

       (*<summary>Generates an array of samples distributed by Laplace distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Laplace probability density function (PDF).
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
        The probability density function is defined as:

        <code>
                    1                  |x-a|
        f(x) = ---------------  exp( - ------ )
                sqrt(2*beta)            beta
        </code>
</remarks>
*)
        procedure RandomLaplace(const Dst: TMtxVec; const a, beta: double);

        (*<summary>Generates an array of samples distributed by Weibull distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Weibull probability density function (PDF)
        similar to as defined here <see cref="Probabilities.WeibullPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
        The probability density function is defined as:

        <code>
               alfa*(x-a)^(alfa-1)            x-a
        f(x) = -------------------  exp( -  (----)^alfa )  , x &gt;= a
                (beta)^alfa                  beta

        f(x) =  0,  x &lt; a
        </code>
</remarks>
*)
        procedure RandomWeibull(const Dst: TMtxVec; const alpha, a, beta: double);

      (*<summary>Generates an array of samples distributed by Rayleigh distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Rayleigh probability density function (PDF)
        similar to as defined here <see cref="Probabilities.RayleighPDF"/>.
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
        The Rayleigh distribution is a special case of the Weibull distribution
        where the alfa parameter is equal to 2. The probability density function is defined as:

        <code>
               2*(x-a)           sqr(x-a)
        f(x) = --------  exp( ------------ )  , x &gt;= a
                sqr(b)           sqr(beta)

        f(x) =  0,  x &lt; a

        </code>
</remarks>
*)
        procedure RandomRayleigh(const Dst: TMtxVec; const a, beta: double);

        (*<summary>Generates an array of samples distributed Lognormally.</summary>
        
<remarks>Returns random numbers distributed according to the Lognormal probability density function (PDF)
        similar to as defined here <see cref="Probabilities.LogNormalPDF"/>. Additional parameters are
        "b" is displacement and beta is scale factor:

        <code>
                        1                       sqr( ln((x - b)/beta) - mu)
        f(x)  = ---------------------*exp( -  ----------------------------)   , x &gt; b
                sigma*(x-b)*sqrt(2Pi)                2*sqr(sigma)

        f(x) = 0, x &lt;= b
        </code>

        The number of samples generated is defined with the Dst.Length parameter. The result can be accessed
        via Dst.Values property.
</remarks>
*)
        procedure RandomLogNormal(const Dst: TMtxVec; const mu, sigma, b, beta: double);

        (*<summary>Generates an array of samples distributed by Beta distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Beta probability density function (PDF).
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomBeta(const Dst: TMtxVec; const p, q, a, beta: double);


        (*<summary>Generates an array of samples distributed by Gamma distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Gamma probability density function (PDF).
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomGamma(const Dst: TMtxVec; const alpha, a, beta: double);

        (*<summary>Generates an array of samples distributed by Gumbel distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Gumbel probability density function (PDF).
        The number of samples generated is  defined with the Dst.Length parameter.
        The result is an array which can be accessed via Dst.Values property.
</remarks>
*)
        procedure RandomGumbel(const Dst: TMtxVec; const a, beta: double);

       (*<summary>Generates an array of integer samples distributed by Bernoulli distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Bernoulli distribution.
        The number of samples generated is defined with the Dst.Length parameter.
        Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomBernoulli(const Dst: TMtxVecInt; const p: double);

       (*<summary>Generates an array of integer samples distributed by Uniform distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Uniform probability density function (PDF)
        as defined here <see cref="Probabilities.UniformPDF"/>. The number of samples generated is
        defined with the Dst.Length parameter. Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomUniformI(const Dst: TMtxVecInt; const a,b: integer);

        (*<summary>Generates integer random values with Uniform bit distribution.</summary>
          
<remarks>Lower bits can be less random than higher bits depending
          on the random generator used. The number of samples generated is
          defined with the Dst.Length parameter. Dst is automatically resized to store 32bit integers, if needed.
          The result is an array of integers.
</remarks>
*)
        procedure RandomUniformBitsI(const Dst: TMtxVecInt);

        (*<summary>Generates an array of integer samples distributed Geometrically.</summary>
        
<remarks>Returns random numbers distributed according to the Geometric probability density function (PDF)
        as defined here <see cref="Probabilities.GeometricPDF"/>. The number of samples generated is
        defined with the Dst.Length parameter. Dst is automatically resized to store 32bit integers,
        if needed. The result is an array of integers.
</remarks>
*)
        procedure RandomGeometric(const Dst: TMtxVecInt; const p: double);

        (*<summary>Generates an array of integer samples distributed Binomially.</summary>
          
<remarks>Returns random numbers distributed according to the Binomial probability density function (PDF)
          as defined here <see cref="Probabilities.BinomPDF"/>. The number of samples generated is
          defined with the Dst.Length parameter. The result is an array of integers
          which can be accessed via Dst.IValues property.
          Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomBinomial(const Dst: TMtxVecInt; const n: integer; const p: double);
        (*<summary>Generates an array of integer samples distributed Hypergeometrically.</summary>
          
<remarks>Hypergeometric probability density function (PDF).
          Returns random numbers distributed according to the Hypergeometric probability density function (PDF)
          as defined here <see cref="Probabilities.HypGeometricPDF"/>. The number of samples generated is
          defined with the Dst.Length parameter. The result is an array of integers
          which can be accessed via Dst.IValues property.
          Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomHypergeometric(const Dst: TMtxVecInt; const m, k, n: integer);
        (*<summary>Generates an array of integer amples distributed according to the Negative binomial probability distribution.</summary>
          
<remarks>Returns random numbers distributed according to the Negative binomial probability density function (PDF)
          as defined here <see cref="Probabilities.NegBinomPDF"/>. The number of samples generated is
          defined with the Dst.Length parameter. The result is an array of integers
          which can be accessed via Dst.IValues property.
          Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomNegbinomial(const Dst: TMtxVecInt; const r, p: double);
        (*<summary>Generates an array of integer samples distributed according to the Poisson probability distribution.</summary>
          
<remarks>The lambda parameter is the same for all random numbers generated.
          Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomPoisson(const Dst: TMtxVecInt; const lambda: double; const Method: TPoissonRandMethod = prmPTPE); overload;
        (*<summary>Generates an array of integer samples distributed according to the Poisson probability distribution.</summary>
        
<remarks>Returns random numbers distributed according to the Poisson probability density function (PDF)
        as defined here <see cref="Probabilities.PoissonPDF"/>. The lambda parameter can be different for
        each random number generated, The number of samples generated is
        defined with the Lambda.Length parameter. The result is an array of integers
        which can be accessed via Dst.IValues property.
        Dst is automatically resized to store 32bit integers, if needed.
</remarks>
*)
        procedure RandomPoisson(const Dst: TMtxVecInt; const lambda: TDoubleArray; const Method: TPoissonRandMethod = prmPTPE); overload;
        (*<summary> Generates uniformly distributed bits in 32-bit chunks </summary>
          
<remarks>Unlike RandomUniformBits, which provides the output of underlying integer recurrence and does not guarantee
          uniform distribution across bits, RandomUniformBits32 is designed to ensure each bit in the 32-bit chunk is
          uniformly distributed.
</remarks>
*)
        procedure RandomUniformBits32(const Dst: TCardinalArray); overload;
        (*<summary> Generates uniformly distributed bits in 64-bit chunks </summary>
          
<remarks>Unlike RandomUniformBits, which provides the output of underlying integer recurrence and does not guarantee
          uniform distribution across bits, RandomUniformBits64 is designed to ensure each bit in the 64-bit chunk is
          uniformly distributed.
</remarks>
*)
        procedure RandomUniformBits64(const Dst: TUInt64Array); overload;
        (*<summary>Generates multinomially distributed random numbers.</summary>
          
<remarks>The function generates multinomially distributed random numbers with m independent
          trials and k possible mutually exclusive outcomes, with corresponding probabilities in p.
</remarks>
*)
        procedure RandomMultinomial(const Dst: TVecInt; const nTrials, k: integer; const p: TDoubleArray); overload;

        destructor Destroy; override;
     end;

     TRngStreamArray = array of TRngStream;



     (*<summary>Random generator.</summary>
       
<remarks>Use this class to generate streams of random numbers. To achieve independence between two series of random
       numbers there are three options:
       1. create several instances of this class and initialize each with a different seed. (and maybe a different random generator).
       2. create a single instance of this class and use leapfroging.
       3. create a single instance of this class and use block splitting.
</remarks>


       <example>
       <code>

    var rnd    : TRandomGenerator;
        IntVec : TVecInt;
    begin
        if Seed = -1 then Seed := aRound(Random*High(Cardinal));

        rnd := TRandomGenerator.Create;
        IntVec := TVecInt.Create;
        try
          IntVec.Size(Results.Length, prInt32);

          rnd.StreamCount := 1;
          rnd.Init(rgMRG32k3a,Seed);
          rnd.Stream[0].RandomBinomial(IntVec,an,p);
          Results.Copy(IntVec);
        finally
          rnd.Free;
          IntVec.Free;
        end
    end;

      </code>
      </example>*)

     TRandomGenerator = class(TPersistent)
     strict private
        StreamList: TRngStreamArray;
        FStreamCount: integer;
        FStreamIndependence: TRngStreamIndependence;
        FSeed: integer;
        FBasicRngType: TBasicRandomGenerator;
        FBlockStep: integer;
        function GetStream(Index1: integer): TRngStream;
        procedure SetStream(Index1: integer; const Value: TRngStream);
        procedure SetStreamCount(const Value: integer);
        procedure SetStreamIndependence(const Value: TRngStreamIndependence);
        procedure SetBlockStep(const Value: integer);
      
      public
        (*<summary>The basic random generator used.</summary>
          
<remarks>Returns the basic random generator used that was passed to the Init routine.
</remarks>
*)
        property BasicRngType: TBasicRandomGenerator read FBasicRngType;
        (*<summary>The random generator seed.</summary>
          
<remarks>Returns the random generator seed that was passed to the Init routine.
</remarks>
*)
        property Seed: integer read FSeed;
        (*<summary>A list of independent random streams.</summary>
          
<remarks>The number of independent random streams is defined with the
          <see cref="StreamCount"/> property. The method used to achieve
          random stream independence is defined with <see cref="StreamIndependence"/>.
</remarks>
*)
        property Stream[Index1: integer]: TRngStream read GetStream write SetStream; default;
        (*<summary>Initializes random generator.</summary>
          
<remarks>Initializes random generator by specifying the basic random generator
          and a seed for the generator. Typical value for aSeed is 1.
</remarks>
*)
        procedure Init(const aBasicRngType: TBasicRandomGenerator; const aSeed: integer);

        
        (*<summary>Saves random generator state to Dst.</summary>*)
        procedure SaveToStream(const Dst: TStream); virtual;
        (*<summary>Loads random generator state from Src.</summary>*)
        procedure LoadFromStream(const Src: TStream); virtual;
        

        (*<summary>Saves random generator state to File.</summary>

        <example>
        <code>

        var rnd: TRandomGenerator;
            iVec: VectorInt;
        begin
            rnd := TRandomGenerator.Create;
            rnd.StreamIndependence := TRngStreamIndependence.rsiLeapfrog;
            try
                iVec.Size(1000);
                rnd.StreamCount := 2;
                rnd.Stream[0].RandomUniformbitsI(iVec);
                rnd.Stream[1].RandomUniformbitsI(iVec);

                rnd.SaveToFile('D:\Test.rnd');
            finally
                rnd.Free;
            end;

            rnd := TRandomGenerator.Create;

            //load stream count, and individual stream state at the time when they were saved
            rnd.LoadFromFile('D:\Test.rnd');
            try
                rnd.Stream[0].RandomUniformBitsI(iVec);
                rnd.Stream[1].RandomUniformBitsI(iVec);
            finally
                rnd.Free;
            end;
        end;

        </code>
        </example>*)
        procedure SaveToFile(const FileName: string); virtual;
        (*<summary>Loads random generator state from File.</summary>

        <example>
        <code>

        var rnd: TRandomGenerator;
            iVec: VectorInt;
        begin
            rnd := TRandomGenerator.Create;
            rnd.StreamIndependence := TRngStreamIndependence.rsiLeapfrog;
            try
                iVec.Size(1000);
                rnd.StreamCount := 2;
                rnd.Stream[0].RandomUniformbitsI(iVec);
                rnd.Stream[1].RandomUniformbitsI(iVec);

                rnd.SaveToFile('D:\Test.rnd');
            finally
                rnd.Free;
            end;

            rnd := TRandomGenerator.Create;

            //load stream count, and individual stream state at the time when they were saved
            rnd.LoadFromFile('D:\Test.rnd');
            try  //continue where we left of:...
                rnd.Stream[0].RandomUniformbitsI(iVec);
                rnd.Stream[1].RandomUniformbitsI(iVec);
            finally
                rnd.Free;
            end;
        end;

        </code>
        </example>*)
        procedure LoadFromFile(const FileName: string); virtual;

        constructor Create; virtual;
        destructor Destroy; override;

      published
       (*<summary>Number of independent random streams.</summary>
         
<remarks>Defines the number of independent random streams to be obtained
         from the same stream of random numbers.
</remarks>
*)
        property StreamCount: integer read FStreamCount write SetStreamCount;
        (*<summary>Specifies the method to achieve random stream independence.</summary>*)
        property StreamIndependence: TRngStreamIndependence read FStreamIndependence write SetStreamIndependence;
        (*<summary>A parameter to the block splitting method to achieve stream independence.</summary>*)
        property BlockStep: integer read FBlockStep write SetBlockStep;
     end;

