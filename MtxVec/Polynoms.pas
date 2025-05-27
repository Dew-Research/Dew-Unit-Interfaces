











(*<summary>Introduces several polynomial handling routines.</summary>
  
<remarks>Unit introduces routines for:
    * poly roots,coefficients,
    * evaluation, fitting,
    * splines,
    * piecewise polynomials,
    * polynomial division and multiplication.
</remarks>
*)
unit Polynoms;


interface

{$I BdsppDefs.inc}

uses MtxVec, Math387,  IppsplTypes, AbstractMtxVec
     
     ,SysUtils
     ,Classes
     

     
       ,Types 
     

     ;

(*<summary>Defines the type of 1D interpolation.</summary>*)
type TInterpolationType = (
  (*<summary>Perform linear interpolation.</summary>*)IntLinear,
  (*<summary>Perform cubic interpolation.</summary>*)IntCubic
  );

(*<summary>Manages piece-wise polynomials.</summary>
  
<remarks>Use this object to construct and evaluate piece-wise polynomials.
  Key properties of the object can initialized by interpolation
  routines: <see cref="Polynoms.Spline1D"/> and <see cref="Polynoms.Linear1D"/>
  Additional properties like <see cref="BreaksOffset"/>,<see cref="BreaksStep"/>,
  <see cref="BreaksEquidistant"/> and <see cref="ValuesSorted"/> must be set manually.
  Setting these properties can significantly improve the performance.

  Note
    All vector and matrices, used in this class must be real.
</remarks>


  <seealso cref="Polynoms.Spline1D"/>
  <seealso cref="Polynoms.Linear1D"/>*)
type TPiecePoly=class(TObject)
  strict private
    FCoefficients: TMtx;
    FBreaks: TVec;
    StartPos  : Integer;
    FDummyInt : Integer;
    FBreaksEquidistant: boolean;
    FBreaksStep: double;
    FValuesSorted: boolean;
    FBreaksOffset: double;
    function GetPieces: Integer;
    function GetOrder: Integer;
    procedure SefCoefficients(const Value: TMtx);
    procedure SetBreaks(const Value: TVec);
    procedure IOnSetSize(Sender: TObject);
    procedure SetBreaksStep(const Value: double);
    procedure SetBreaksEquidistant(const Value: boolean);
    procedure SetValuesSorted(const Value: boolean);
    procedure SetBreaksOffset(const Value: double);
  public
    (*<summary>Evaluate piece-wise polynomial.</summary>
      
<remarks>Evaluate piece-wise polynomial at all X values and
      store the results in Y. The Length and Complex properies of vector Y are adjusted automatically.

      Note
        There are four additional parameters that affect how fast will the evaluation occur:
        <see cref="BreaksEquidistant"/>, <see cref="ValuesSorted"/>, <see cref="BreaksStep"/> and <see cref="BreaksOffset"/>.
</remarks>


      <seeAlso cref="BreaksStep"/>
      <seeAlso cref="BreaksOffset"/>
      <seeAlso cref="ValuesSorted"/>
      <seeAlso cref="BreaksEquidistant"/>*)
    procedure Evaluate(X: TDenseMtxVec; Y: TDenseMtxVec); overload;
    (*<summary>Evaluate piece-wise polynomial.</summary>
     
<remarks>Evaluates pice-wise polynomial for single x,y point.
</remarks>
*)
    procedure Evaluate(X: double;var Y: double); overload;
    (*<summary>Evaluate piece-wise polynomial.</summary>
     
<remarks>Evaluates pice-wise polynomial for single x,y point.
</remarks>
*)
    procedure Evaluate(X: single;var Y: single); overload;

    constructor Create;
    destructor Destroy; override;
  public
  (*<summary>If True all breaks will be assumed to be spaced at equal intervals.</summary>
    
<remarks><value>If True all breaks will be assumed to be spaced at equal intervals. This allows
     the routine to compute the interval within which the interpolation point falls. If the breaks are not
     equidistant the interval defined by two breaks <c>Low_Break &lt; X_Value &lt; High_Break</c>,
     must be searched for.
    </value>
</remarks>


    <SeeAlso cref="BreaksStep"/>
    <SeeAlso cref="BreaksOffset"/>
    <SeeAlso cref="ValuesSorted"/>
    <SeeAlso cref="Evaluate"/>*)
    property BreaksEquidistant: boolean read FBreaksEquidistant write SetBreaksEquidistant;
    (*<summary>Defines the step between the Breaks.</summary>
      
<remarks><value>Defines the step between the <see cref="Breaks"/>. This step must
      be specified, if <see cref="BreaksEquidistant"/> is True.
      </value>
</remarks>

      <SeeAlso cref="ValuesSorted"/>
      <SeeAlso cref="Evaluate"/>*)
    property BreaksStep: double read FBreaksStep write SetBreaksStep;
    (*<summary>Defines the offset from zero of the first break.</summary>
      
<remarks><value>Defines the offset from zero of the first break. This offest
      must be specified, if <see cref="BreaksEquidistant"/> is True.
      </value>
</remarks>


      <SeeAlso cref="ValuesSorted"/>
      <SeeAlso cref="Evaluate"/>*)
    property BreaksOffset: double read FBreaksOffset write SetBreaksOffset;
    (*<summary>Defines if X Values are sorted.</summary>
    
<remarks><value>If X Values defining the interpolation points are sorted, but the
    Breaks are not equidistant (BreaksEquidistant is false), the
    Evaluate routine will use a consecutive linear search algorithm to find the
    right breaks interval. The linear search for each X-Value will continue
    from the point where it left off for the previous one. If X Values are
    not sorted, the ValuesSorted must be set to false. A bisection search
    algorithm will be employed in that case.</value>
</remarks>


    <SeeAlso cref="Evaluate"/>*)
    property ValuesSorted: boolean read FValuesSorted write SetValuesSorted;
    (*<summary>Returns the number of piece-wise polynomial pieces.</summary>
      
<remarks><value>The number of pieces is one less than the number of
      breaks (Breaks.Length).
      </value>
</remarks>
*)
    property Pieces: Integer read GetPieces write FDummyInt;
    (*<summary>Returns the order of the piece-wise interpolation.</summary>
      
<remarks><value>The order of the polynomial is equal to the number of Coefficients columns -1
      and is smaller by one then the value of this property.
      For linear interpolation the order is 2. For cubic interpolation, the order
      is 4.
      </value>
</remarks>
*)
    property Order: Integer read GetOrder write FDummyInt;
    (*<summary>Stores the piece-wise polynomial breaks.</summary>
    
<remarks><value>Stores the piece-wise polynomial breaks. Property determines the
    intervals for piece-wise polynomial. It can not be complex.
    </value>
</remarks>
*)
    property Breaks: TVec read FBreaks write SetBreaks;
    (*<summary>Stores the coefficients of the piece-wise polynomial.</summary>
      
<remarks><value>Stores the coefficients of the piece-wise polynomial.
      Each section is stored in its own row. When setting the Coefficients
      Rows and Cols properties, the number of rows must be equal to the number
      of sections - pieces. The number of columns must be equal to the
      <see cref="Order"/> property.
      </value>
</remarks>
*)
    property Coefficients: TMtx read FCoefficients write SefCoefficients;
  end;



    (*<summary>Fits a polynomial to data.</summary>
      <param name="XValues">Defines x values in <c>p=p(x)</c>.</param>
      <param name="YValues">Defines polynomial, evaluated at x i.e <c>p(x)</c>.</param>
      <param name="Degree">Defines polynomial degree.</param>
      <param name="Weights">If not nil, defines fitting weights.</param>
      <param name="Coeff">Returns the coeficients of fitted polynomial.</param>
      <param name="R">Returns the Cholesky factor of the Vandermonde matrix.</param>
      <param name="DegFreedom">Returns the degree of freedom.</param>
      <param name="L2R">Returns the L2 norm of the residuals.</param>

      
<remarks>The PolyFit procedure uses the least-squares method to find the fitted polynomial coefficients.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Tee;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector X = new Vector(0);
          Vector Y = new Vector(0);
          Vector YCalc = new Vector(0);
          Vector Delta = new Vector(0);
          Vector Coeff = new Vector(0);
          Matrix R = new Matrix(0,0);
          int degF;
          double L2R;

          X.Ramp(100);
          y.Size(X);
          Y.RandomGauss(2.0,3.0);
          Polynoms.PolyFit(X,Y,3,Coeff,R, out degF, out L2R, null); //Fit
          Polynoms.PolyEval(X,Coeff,degF,L2R,YCalc,Delta); // Evaluate
          MtxVecTee.DrawIt(new Vector[] {Y,YCalc}, new string[] {"Original","Fit"},"Fit results",false);
        }
      }
      </code></example>*)
    
    procedure PolyFit(XValues,YValues: TVec; Degree: Integer; Coeff: TVec; R: TMtx; out DegFreedom: Integer; out L2R: double; Weights: TVec = nil); overload;

    (*<summary>Fits a polynomial to data.</summary>
      
<remarks>This overload does not return additional statistical parameters like Cholesky matrix
      or degrees of freedom.
</remarks>
*)
    
    procedure PolyFit(XValues,YValues: TVec; Degree: Integer; Coeff: TVec; Weights: TVec = nil); overload;

    (*<summary>Converts polynomial roots to coefficients.</summary>
      
<remarks>The PolyCoeff procedure converts polynomial roots to coefficients.
      The following scheme is used to construct a coefficients from the roots:

      <code>
      P(x) = coeff[0]*x^n + coeff[1]*x^(n-1) + .. + coeff[n]
           = (x - roots[0])* .. *(x - roots[n-1])

      n .. order of the polynomial
      </code>

      The length and complex property of the Coeff object are set automatically.
      The inverse to this routine is <see cref="PolyRoots"/>
</remarks>
*)
    
    procedure PolyCoeff(Roots, Coeff: TVec); overload;

    (*<summary>Returns coefficients of the characteristic polynomial.</summary>
      
<remarks>Returns coefficients of the characteristic polynomial: <c>det( Mtx - Lambda * I )</c>.
</remarks>


      

    <example>
      Find coefficients of a characteristic polynomial:
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Editors;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector Coeff = new Vector(0);
          Matrix A = new Matrix(0,0);
          A.SetIt(3,3,false, new double[] {1,  4, -1,
                                           2,  1,  5,
                                           3, -2,  0});
          Polynoms.PolyCoeff(A,Coeff);
          MtxVecEdit.ViewValues(Coeff,"Coefficients");
        }
      }
    </code></example>

    <seealso cref="PolyRoots"/>*)
    
    procedure PolyCoeff(Mtx: TMtx; Coeff: TVec); overload;

    (*<summary>Finds the roots of the polynomial from the coefficents.</summary>
      
<remarks>Finds the roots of the polynomial from the coefficents. The Coeff
      holds the coefficients and the result is placed in the Roots.

      <code>
      P(x) = coeff[0]*x^n + coeff[1]*x^(n-1) + .. + coeff[n+1]
           = (x - roots[0])* .. *(x - roots[n-1])

      n ... order of the polynomial
      P(x).. value of the polynomial evaluated at x.
      </code>

      The Length and Complex property of the Roots parameter are set
      automatically. The inverse to this routine is <see cref="PolyCoeff"/>
      Roots are computed by finding the eigenvalues of the companion matrix.
</remarks>


    

    <example>
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Editors;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector Coeff = new Vector(0);
          Vector Roots = new Vector(0);
          // coefficients of the polynomial 1*x^2 - 2*x + 1:

          Coeff.SetIt(false, new double[] {1, -2, 1});
          Polynoms.PolyRoots(Roots,Coeff);

          // Roots now hold the roots of the polynomial  = [ 1, 1 ]
          // x^2 - 2x + 1 = (x - 1) * (x - 1)
          MtxVecEdit.ViewValues(Roots,"Roots");
        }
      }
    </code></example>

    <SeeAlso cref="PolyCoeff"/>*)
    
    procedure PolyRoots(Roots, Coeff:TVec);

    (*<summary>Evaluate a polynomial.</summary>
      <param name="Val">Stores values doe which the polynomial will be evaluated (x values).</param>
      <param name="Coeff">Stores polynomial coefficients for which the polynomial will be evaaluted.</param>
      <param name="R">Returns the Cholesky factor of the Vandermonde matrix.</param>
      <param name="DegFreedom">Returns the residuals degree of freedom.</param>
      <param name="L2R">Returns the L2 norm of the residuals.</param>
      <param name="EvalResult">Returns the results P(x), evaluated at Val.</param>
      <param name="Delta">Returns the error estimates for Results.</param>

      
<remarks>Evaluate a polynomial  with coefficients Coeff at value Val.
      The order of polynomial (N) is determined as: <c>N := Coeff.Length - 1</c>.
      The algorithm employed uses Horners rule. The following formula is used to evaluate the polynomial:

      <c>P(x) = coeff[0]*x^n + coeff[1]*x^(n-1) + .. + coeff[n]</c><para/>
      To estimate the error of polynomial interpolation after a call to <see cref="PolyFit"/>
      additional parameters have to be passed to the routine.
</remarks>
*)
    procedure PolyEval(Val, Coeff: TVec; R: TMtx; DegFreedom: Integer; L2R: double; EvalResult, Delta: TVec); overload;

    (*<summary>Evaluates a polynomial with coefficients Coeff at value Val and returns the results in EvalResult.</summary>
      
<remarks>Evaluates a polynomial with coefficients Coeff at value Val and returns the results in EvalResult.
</remarks>
*)
    
    procedure PolyEval(Val, Coeff, EvalResult: TVec); overload;

    (*<summary>Evaluates a polynomial  with coefficients Coeff at real value Val and returns real result.</summary>
      
<remarks>Evaluates a polynomial  with coefficients Coeff at real value Val and returns real result.
</remarks>
*)
    
    function  PolyEval(Val: double; Coeff: TVec): double; overload;

    (*<summary>Evaluates a polynomial with coefficients Coeff at complex value Val and returns complex results.</summary>
      
<remarks>Evaluates a polynomial with coefficients Coeff at complex value
      Val and returns complex results.
</remarks>


    

    <example>
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Tee;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector X = new Vector(0);
          Vector Y = new Vector(0);
          Vector Coeff = new Vector(0);

          X.Size(10);
          X.Ramp(-5,1); // X = [-5,-4,-3,-2,-1, 0, 1, 2, 3, 4]
          // construct a parabola
          Coeff.SetIt(false,new double[] {2, -1, 3});
          Polynoms.PolyEval(X,Coeff,Y);

          // Y now holds the values of polynomial, evaluated at all X.Values
          MtxVecTee.DrawIt(X,Y,"Parabola",false);
        }
      }
    </code></example>

    <SeeAlso cref="PolyFit"/>
    <SeeAlso cref="TPiecePoly"/>*)
    
    function  PolyEval(Val:TCplx; Coeff:TVec): TCplx; overload;

    
    procedure StripZeros(Src,Dst:TVec; var Lead, Trail: integer); overload;

    (*<summary>Removes leading zeros.</summary>
      
<remarks>Removes leading zeros. In case of polynomials the most left coefficents
      in the array are for the highest powers. If those coefficients are zero
      they can be removed. The routine will copy the Src to Dst
      and remove any leading zeros from Dst.
</remarks>
*)
    
    procedure StripLeadZeros(Src,Dst: TVec); overload;

    (*<summary>Cubic spline interpolation.</summary>
      
<remarks>The procedure interpolates cubic splines between consequtive (X,Y) pairs.
      The rotine does not return interpolated points. It constructs
      piece-wise polynomial, which in term can be used for evaluating
      (by using the TPiecePoly.Evaluate method) cubic splines.
      To directly obtain interpolated values, use the <see cref="Interpolate"/> routine.

      Note
        If  <c>X.Length=Y.Length</c>, then the "not-a-knot" end conditions are used.
        If <c>Y.Length = X.Length +2</c> then the "knot" end conditions are used.
        X values must be <b>monotonic</b> or the result will not be valid.
</remarks>
*)
    
    procedure Spline1D(X, Y: TVec; PiecePoly: TPiecePoly);overload;

    (*<summary>Interpolates cubic splines between consecutive Y points.</summary>
      
<remarks>Interpolates cubic splines between consecutive Y points. The assumption is
      that Y is evaluated at <c>[0,1,2,...]</c> -> X values are <c>[0,1,2,...]</c>.
      If the Knot parameter is true then the first and the last Y value will
      be used for the end conditions. In this case <c>Y.Length = X.Length + 2</c>.
</remarks>


      

    <example>
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Tee;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector X = new Vector(0);
          Vector Y = new Vector(0);
          Vector Y2 = new Vector(0);
          TPiecePoly PP = new TPiecePoly();
          int i;
          double YVal;

          // generate function - note that X values are monotonical
          X.Size(100);
          Y.Size(100);
          Y2.Size(100);
          X.Ramp(0,1);
          Y.RandUniform(0,50);
          Y2.Ramp(100,0.25);
          Y += Y2;

          //   construct cubic splines, but do not evaluate them
          Polynoms.Spline1D(X,Y,PP);
          X.Size(800);
          X.Ramp(0,0.125); //get interpolation points
          PP.Evaluate(X,Y2); // evaluate

          MtxVecTee.DrawIt(Y,"Original",false);
          MtxVecTee.DrawIt(Y2,"Interpolated",false);
        }
      }
    </code></example>

    <SeeAlso cref="Interpolate"/>
    <SeeAlso cref="Linear1D"/>
    <SeeAlso cref="PolyFit"/>
    <SeeAlso cref="TPiecePoly"/>*)
    
    procedure Spline1D(Y: TVec; PiecePoly: TPiecePoly; Knots: boolean = false);overload;

    (*<summary>Linear interpolation.</summary>
      
<remarks>The Linear1D procedure interpolates lines between consecutive (X,Y) pairs.
      Linear1D does not return interpolated points. It constructs piece-wise polynomial,
      which can be used to evaluate ( by using the PiecePoly.Evaluate method)
      the linear functions. To directly obtain interpolated values, use the
      <see cref="Interpolate"/> routine.

      Note:
        X values must be monotonic or the result will not be valid.
</remarks>
*)
    
    procedure Linear1D(X, Y: TVec; PiecePoly: TPiecePoly);overload;

    (*<summary>Perform interpolation assuming that Y is evaluated at <c>[0,1,2,...]</c>.</summary>

    

    <example>
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Tee;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector X = new Vector(0);
          Vector Y = new Vector(0);
          Vector Y2 = new Vector(0);
          TPiecePoly PP = new TPiecePoly();
          int i;
          double YVal;

          // generate function - note that X values are monotonical
          X.Size(100);
          Y.Size(100);
          Y2.Size(100);

          X.Ramp(0,1);
          Y.RandUniform(0,50);
          Y2-Ramp(100,0.25);
          Y += Y2;

         //   construct cubic splines, but do not evaluate them

         Polynoms.Linear1D(X,Y,PP);

         X.Size(800);
         X.Ramp(0,0.125); //get interpolation points
         PP.Evaluate(X,Y2); // evaluate

         MtxVecTee.DrawIt(Y,"Original",false);
         MtxVecTee.DrawIt(Y2,"Interpolated",false);
        }
      }
    </code></example>

    <SeeAlso cref="Interpolate"/>
    <SeeAlso cref="Spline1D"/>
    <SeeAlso cref="PolyFit"/>
    <SeeAlso cref="TPiecePoly"/>*)
    
    procedure Linear1D(Y: TVec; PiecePoly: TPiecePoly);overload;

    procedure InterpolateEqd(X,Y: TVec; intX, IntY: TVec; IntType: TInterpolationType = IntCubic);

    (*<summary>Perform linear or cubic interpolation.</summary>
     
<remarks>The routine interpolates IntY at IntX to the underlying function
     values, stored in Y. X.Length and Y.length properties must match or an exception
     will be raised. The IntType parameter defines the type of interpolation (linear, cubic).
     The intXSorted parameter defines whether intX values are sorted. If intX values are
     sorted the interpolation is much faster.

     Vector X stores the positions, where the function is evaluated.
     X values must be monotonic. The Length and Complex properties
     of the IntY vector are adjusted automatically.
</remarks>
*)
    
    procedure Interpolate(X,Y: TVec; intX, IntY: TVec; IntType: TInterpolationType = IntCubic; IntXSorted: boolean = true);overload;

    (*<summary>Performs interpolation assuming <c>X=[0,1,..]</c>.</summary>
      
<remarks>Performs interpolation assuming <c>X=[0,1,..]</c> and assumes that Y is evaluated
      at <c>[0,1,2,...]</c> -> X values are <c>[0,1,2,...]</c>.
</remarks>


    

    <example>
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Tee;

      namespace Dew.Examples
      {
        private void Example()
        {
          Vector X = new Vector(0);
          Vector Y = new Vector(0);
          Vector PX = new Vector(0);
          Vector PY = new Vector(0);

          // generate function - note that X values are monotonical
          Random r = new Random();
          X.Ramp();
          Y.Values[0] = 100.0;
          for (int i=1; i &lt; X.Length; i++)
            Y.Values[i] = Y.Values[i-1] + 250 - r.Next(500);

          // now setup the points at which you want to interpolate
          PX.Size(1000);
          PX.Ramp();
          PX.Scale(0.1);

          // calculate piecewise poly for the range of points -
          //   note that PX values are sorted
          Polynoms.Interpolate(X,Y,PX,PY,TInterpolationType.IntCubic,true);
          // PY returns the interpolated points, calculated at PX

          MtxVecTee.DrawIt(Y,"Original",false);
          MtxVecTee.DrawIt(PY,"Interpolated",false);
        }
      }
    </code></example>

    <SeeAlso cref="Spline1D"/>
    <SeeAlso cref="Linear1D"/>
    <SeeAlso cref="PolyFit"/>
    <SeeAlso cref="TPiecePoly"/>*)
    
    procedure Interpolate(Y: TVec; intX, IntY: TVec; IntType: TInterpolationType = IntCubic; intXSorted: boolean = true); overload;

    (*<summary>Performs infinite impulse response (IIR) filtering.</summary>
      
<remarks>Performs infinite impulse response (IIR) filtering. The IIRFilter procedure filters X through an
      IIR filter ([1]) and stores the results in vector Y. The filter is described with vectors A and B.

      The X values are filtered by using the following equation:

       <code>
         y[i] = b[0]*x[i] + b[1]*x[i-1] + ... + b[N]*x[i-N]
                          + a[1]*y[i-1] + ... + a[M]*y[i-M]

         x[i] ... input sample
         y[i] ... output (filtered) sample
       </code>

       where <c>M = A.Length-1</c> and <c>N = B.Length-1</c>. The filter coefficients are normalized
       by A.Values[0]. The routine is not capable of continuous filtering (streaming).
</remarks>


       <SeeAlso href="[1] Understanding digital signal processing, Richard G. Lyons, Prentice Hall, 2001"/>
       <SeeAlso cref="DeConv"/>*)
    
    procedure IIRFilter(const B, A, X, Y: TVec; const DelayLine: TVec = nil); overload;

    (*<summary>Divide polynomials, deconvolution.</summary>
      
<remarks>The procedure deconvolves vector A out of vector B.
      If vectors A and B are treated as polynomial coefficients,
      the the deconvolution is equal to polynomial division. In this case the
      DeConv divides polynomial B with polynomial A and returns the quotient
      in Quot and remainder in Remain. An exception is raised if A and B complex
      properties do not match. An exception is raised if <c>|A.Values[0]| = 0</c>.

      NOTE
        To perform polynomial multiplication, use the TVec.Convolve method.
</remarks>


      

    <example>
      Below are two cases. In the first case the following division is performed.

      <code>
       (x  -  1) * (x  -  2) * (x  - 2)
       -------------------------------- =
            (x  -  1) * (x  - 2)


       1*x^3 - 5*x^2 +  8x  - 4
       -------------------------- = x - 2
            x^2 - 3x  + 2
      </code>

      For the first example the ratio is x-2, and the reminder is zero.
      The second case demonstrates that division can be done also
      for complex polynomials giving complex result and reminder.
    <code>
      using Dew.Math;
      using Dew.Math.Units;
      using Dew.Math.Editors;

      namespace Dew.Examples
      {
        private void Example()
        {
         Vector A = new Vector(0);
         Vector B = new Vector(0);
         Vector Ar = new Vector(0);
         Vector Br = new Vector(0);
         Vector Q = new Vector(0);
         Vector R = new Vector(0);

         // Case 1
         Ar.SetIt(false,new double[] {1,2});    // Define roots of the polynomial A:
         Polynoms.PolyCoeff(Ar,A);           // get coefficients  = (X - 1)* (X - 2)

         Br.SetIt(false,new double[] {1,2,2});     // Define roots of the polynomial B:
         Polynoms.PolyCoeff(Br,B);           //get coefficents    = (X - 1)* (X - 2) * (X - 2)

         Polynoms.DeConv(B,A,Q,R);

         // Q = [ 1, -2 ]
         // R = [ 0 ]
         MtxVecEdit.ViewValues(Q,"Q",true);
         MtxVecEdit.ViewValues(R,"R",true);

         // Case 2


         B.SetIt(true,new double[] {2,-1, 2,3, 0,5});
         A.SetIt(true,new double[] {0,2, 1,-3, 2,2});

         Polynoms.DeConv(B,A,Q,R);

         // Q = [ -0.5 - i ]
         // R = [  0       , 5.5 + 2.5i, -1 + 8i]
         MtxVecEdit.ViewValues(Q,"Q",true);
         MtxVecEdit.ViewValues(R,"R",true);
        }
      }
    </code></example>

    <SeeAlso cref="IIRFilter"/>
    <SeeAlso cref="PolyCoeff"/>
    <SeeAlso cref="PolyRoots"/>*)
    
    procedure DeConv(B,A:TVec; Quot,Remain: TVec);























