










(*<summary>Implements TVec and TMtx objects.</summary>
  
<remarks>This unit implements two key objects in linear algebra, namely the vector
  <see cref="TVec"/> and matrix <see cref="TMtx"/>.
</remarks>
*)
unit MtxVec;


interface

{$I BdsppDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}

uses Math387, AbstractMtxVec, MtxVecBase, MtxVecInt, AbstractMtxVecInt

  
      ,Nmkl
  

  
      ,NmklSingle
  
      ,IppsplTypes

  

  

  

  
      
      
      ,Dialogs
      ,Forms
      
      

      
         
         ,Windows
         
      

      

         
         ,Types
         
      

      ,Classes, SysUtils, SyncObjs
  

  

  

  

  
      ;

{$Z4}




    {$HPPEMIT END '#include "MtxVec.h"'}





type


     TVec = class;
     TMtx = class;
     TSmallMatrixMultiply = class;

  (*<summary>Defines real scalar function of several variables.</summary>
    <param name="Parameters">Function variables.</param>
    <param name="Constants">Array of additional constants which can be used in math formula.</param>
    <param name="ObjConst">Array of additional constants (pointers) which can be used in math formula.</param>

    
<remarks>Defines real scalar function of several variables.

    <b>What's the PConsts parameter for?</b>

    Constants can also be stored in a vector (or even matrix). In this case you could use the
    following code:

    <code>
    function YMul(const Pars: TVec; const Consts: TVec; const OConsts: Array of TObject): double;
    begin
      YMul := Consts[0]*Sin(Consts[1] + 2.0*Pars[0]) + Sqr(Pars[1]);
    end;
      ...

    var constVec, parsVec: Vector;
    begin
      constVec := [2,3];
      parsVec := [1,1];
      ...
      // calculate YMul=YMul(1,1) and set a,b constants to 2 and 3 respectively.
      result := yMul(parsVec, constVec, []);
    </code><para/>
    or even:<para/>
    <code>
    function YMul(const Pars: TVec; const Consts: TVec; const ObjConsts:  Array of TObject): double;
      var CVec0 : TVec;
    begin
      CVec0 := ObjConsts[0] as TVec;
      YMul := CVec0.Values[0]*Sin(CVec0.Values[1]+2.0*Pars[0])+ Sqr(Pars[1]);
    end;

    var constVec, parsVec: Vector;
    begin
      ...
      constVec := [2,3];
      parsVec := [1,1];
      // calculate YMul=YMul(1,1) and set a,b constants to 2 and 3 respectively.
      yMul(parsVec,nil,[ConstVec]);
    </code>
</remarks>


    <Example><b>Function of one variable</b>.<para/>Suppose we have a function of one variable:

    <c>y = a*Sin(b+2*x),</c><para/>
    where x is a variable and a,b are two additional constants. Here is how we would define <c>function y: TRealFunction</c>:

    <code>
    function y(const Pars: TVec; const Consts: TVec; Const Objects:   Array of TObject): double;
    begin
      y := Consts[0]*Sin(Consts[1]+2.0*Pars[0]);
    end;
    ...
    // calculate y=y(2.5) and set a,b, constants to 3 and 2 respectively
    var constVec, parsVec: Vector;

    constVec := [3,2];
    parsVec := [2.5];
    y(parsVec,constsVec,[]);

    </code>
    <code lang="C++">
    double __fastcall y(const TVec * Parameters, const TVec * Constants, System::TObject* const * ObjConst, const int ObjConst_Size)
    {
      return Consts[0]*Sin(Consts[1]+2.0*Parameters[0]);
    }
    ...
    // calculate y=y(2.5) and set a,b, constants to 3 and 2 respectively
    sVector parsVec, constVec;

    parsVec.SetIt(1, false, OPENARRAY(double,(2.5)));
    constVec.SetIt(2, false, OPENARRAY(double,(3,2)));
    y(parsVec, constVec, NULL,-1);
    </code>
    </Example>

    <Example><b>Function of several variables</b><para/>Suppose we have a function of several
    variables:<para/>
    <c>YMul = a*Sin(b+2*x1)+Sqr(x2),</c><para/>
    where x1,x2  are two variables and a,b are two additional constants. Here is how we would
    define function <c>yMul: TRealFunction;</c>

    <code>
    function y(const Pars: TVec; const Consts: TVec; const OConsts:
      Array of TObject): double;
    begin
      y := Consts[0]*Sin(Consts[1]+2.0*Pars[0])+ Sqr(Pars[1]);
    end;
    ...
    // calculate YMul=YMul(1,1) and set a,b constants to 2 and 3 respectively.
    var constVec, parsVec: Vector;
    begin
      constVec := [3,2];
      parsVec := [1,1];
      result := yMul(parsVec,constVec,[]);
    </code>
    </Example>

    
    <SeeAlso cref="TGrad"/>
    <SeeAlso cref="TGradHess"/>
    <SeeAlso cref="TVectorFunction"/>*)
    TRealFunction = function(const Parameters: TVec; const Constants: TVec; Const ObjConst: Array of TObject): double;


     (*<summary>Defines type of correlation norm used for Cross-Correlation and Auto-Correlation. </summary>*)
    TCorrNorm = (
    (*<summary>Without normalization.</summary>*)
    cnDefault,
    (*<summary>Biased normalized will be applied.</summary>*)
    cnBiased,
    (*<summary>Unbiased normalized will be applied.</summary>*)
    cnUnbiased);

     (*<summary>Defines type of size reduction used in PixelDownSample method.</summary>*)
     TPixelDownSample = (
     (*<summary>Use 1 for all areas with at least one nonzero value regardless of its amplitude.</summary>*)
     pdsPattern,
     (*<summary>Samples are averaged together.</summary>*)
     pdsAverage,
     (*<summary>Use average of all absolute values within an area (pixel).</summary>*)
     pdsAverageMagnitude,
     (*<summary>Use only the maximum absolute value within an area (region will become one pixel).</summary>*)
     pdsPeak,
     (*<summary>First magnitude is computed and maximum value is searched for.</summary>*)
     pdsPeakMagnitude
     );

     (*<summary>Defines type of matrix.</summary>
     <SeeAlso cref="TMtx.DetectMtxType"/>*)
     TMtxType = (
     (*<summary>symmetric positive definite matrix</summary>*)
     mtSymmPosDef,
     (*<summary>symmetric matrix</summary>*)
     mtSymmetric,
     (*<summary>Hermitian positive definite matrix</summary>*)
     mtHermPosDef,
     (*<summary>Hermitian matrix</summary>*)
     mtHermitian,
     (*<summary>triangular matrix, with unit or non unit main diagonal</summary>*)
     mtTriangle,
     (*<summary>general matrix (none of the above)</summary>*)
     mtGeneral,
     (*<summary>symmetric positive definite matrix in banded storage forma</summary>*)
     mtBandSymmPosDef,
     (*<summary>Hermitian positive definite matrix in banded storage format</summary>*)
     mtBandHermPosDef,
     (*<summary>general matrix in banded storage format</summary>*)
     mtBandGeneral
     );

     (*<summary>Defines the type of operation on matrix.</summary>*)
     TMtxOperation = (
     (*<summary>no operation</summary>*)opNone,
     (*<summary>transpose operation</summary>*)opTran,
     (*<summary>transpose and conjugate</summary>*)opHerm
     );

     (*<summary>Defines the type of operation on matrix.</summary>*)
     TJITedMul =  (
     (*<summary>JIT-ed matrix multiply is disabled</summary>*) jtmDisabled,
     (*<summary>JIT-ed matrix multiply is enabled</summary>*) jtmEnabled
     );

     (*<summary>Defines the type of pixel downsampling.</summary>
       
<remarks>If the X axis is equidistantly sampled, the pixel downsample
       routine can be much faster. To downsample an already
       downsampled data specify eqsXDownsampled.
</remarks>
*)
     TEquidistantSample = (
     (*<summary>Data is not sampled equidistantly on X axis.</summary>*)eqsXNotEquidistant,
     (*<summary>Data is sampled equidistantly on X axis.</summary>*)eqsXEquidistant,
     (*<summary>Data is already downsampled.</summary>*)eqsXDownsampled
     );

     (*<summary>Specifies if and which condition numbers for eigenvalues are to be computed.</summary>*)
     TEigCondition = (
     (*<summary>Do not compute condition numbers.</summary>*)
     eicNone,
     (*<summary>Computed for average of selected eigenvalues only.</summary>*)
     eicSelectedEigs,
     (*<summary>Computed for selected right invariant subspace only.</summary>*)
     eicSelectedRight,
     (*<summary>Computed for both.</summary>*)
     eicBoth);

     (*<summary>Fast Hilbert Transform computation method.</summary>*)
    THilbertMethod = (
    (*<summary>Compute forward real to complex FFT, FlipConj, sets DC to zero and Inverse complex to complex.</summary>*)
    hmMethod1,
    (*<summary>Compute forward real to complex FFT with symmetric conjugate part, set second half to zero, first half scaled by 2 and Inverse complex to complex.

    Refeference: S. Lawrence Marple, Jr., Computing the discrete-time "analytic" signal via FFT,
                 IEEE Transactions on Signal Processing ( Volume: 47 , Issue: 9 , Sep 1999 ), pp.2600-2603</summary>*)
    hmMethod2
    );

     (*<summary>Class used to store matrix balancing information and results returned by TMtx.Eig.. functions.</summary>*)
     TEigBalancing = class
     public
        ilo: integer;
        ihi: Integer;
        (*<summary>Details of the permutations and scaling factors applied when balancing the matrix.</summary>
        
<remarks>If P(j) is the index of the row and column
        interchanged with row and column j, and D(j) is the scaling factor applied to row and column j, then
        <code>
              SCALE(J) = P(J),    for J = 1,...,ILO-1
                       = D(J),    for J = ILO,...,IHI
                       = P(J)     for J = IHI+1,...,N.
        </code>
        The order in which the interchanges are made is N to IHI+1, then 1 to ILO-1.
        In case of generalized Eigenvalue calcuation this is the scaling from left.
</remarks>
*)
        lscale: TVec;
        (*<summary>Details of the permutations and scaling factors applied when balancing the matrix.</summary>
        
<remarks>If P(j) is the index of the row and column
        interchanged with row and column j, and D(j) is the scaling factor applied to row and column j, then
        <code>
              SCALE(J) = P(J),    for J = 1,...,ILO-1
                       = D(J),    for J = ILO,...,IHI
                       = P(J)     for J = IHI+1,...,N.
        </code>
        The order in which the interchanges are made is N to IHI+1, then 1 to ILO-1.
        In case of generalized Eigenvalue calcuation this is the scaling from right.
</remarks>
*)
        rscale: TVec;
        (*<summary> The one-norm of the balanced matrix </summary>
                  
<remarks>The maximum of the sum of absolute values of elements of any column of the balanced matrix.
                  In case of generalized Eigenvalue calculation, this is the norm of A.
</remarks>
*)
        abnrm: double;
        (*<summary> The one-norm of the balanced matrix </summary>
                  
<remarks>The maximum of the sum of absolute values of elements of any column of the balanced matrix.
                  In case of generalized Eigenvalue calculation, this is the norm of B.
</remarks>
*)
        bbnrm: double;
        constructor Create;
        destructor Destroy; override;
     end;

     (*<summary> Injects additional methods in to TMtxVecInt. </summary>
                 
<remarks>The helper class is declared for the abstract class of TMtxVecInt to allow additional helper classes
                 to be defined by the user for TVecInt and TMtxInt.
</remarks>
*)

     TMtxVecIntHelper = class helper for TMtxVecInt
     public
      (*<summary>Finds the masks for a vector and splits it.</summary>
        
<remarks>The method splits the a vector in two vectors. The MaskVec will hold the those
        elements where the Op comparison between a and b is True.
        Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or  &lt;&gt; </c>.
        NotMaksVec will hold all those elements where the Op comparison between a and b is false.

        The calling vector will store the mask, 1 at those index locations
        where the Op comparison was True and 0 at those index locations
        where the Op comparison was false.
</remarks>


        <SeeAlso cref="TVec.GatherSplit"/>
        <SeeAlso cref="TVec.FindAndGather"/>
        <SeeAlso cref="FindIndexes"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="TVec.Gather"/>
        <SeeAlso cref="TMtxVec.Scatter"/>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: TMtxVec; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
      (*<summary>The b parameter is of <see cref="TCplx"/> type.</summary>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: TCplx; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: double; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;

      (*<summary>Fills the calling vector with indexes, where the logical expression is true.</summary>
        
<remarks>Fills the calling vector with indexes, where the Op comparison between a and b is True. Op string parameter can be <c>'&lt;', '&gt;',
        '&gt;=','&lt;=','=' or '&lt;&gt;'</c>. The calling vector will be sized to match a.Length. On return it will be subranged
        to reflect actual number of matching elements. The method will not raise an exception, if the calling vector (Self)
        is already subranged. Check the TVec.SetSubRange routine to learn more about subranges.
</remarks>


        <Example>Ensure that the power function will return 0, if the exponent is 0.
        <code>
        CreateIt(a,b);
        With TVec(Self) do
        try
          Ln(Base);  //First do it the normal way then fix the anomalies
          Mul(Exponent);
          Exp;
          a.FindIndexes(Exponent,'=',0);
          b.Size(a.Length,Complex);
          b.Setval(1);
          Scatter(b,a);
        finally
          FreeIt(a,b);
        end;
        </code>
        </Example>

        <SeeAlso cref="TVec.GatherSplit"/>
        <SeeAlso cref="TVec.FindAndGather"/>
        <SeeAlso cref="FindMask"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="TVec.Gather"/>
        <SeeAlso cref="TMtxVec.Scatter"/>*)
      function FindIndexes(const a: TMtxVec; const op: string; const b: TCplx): TMtxVecInt; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindIndexes(const a: TMtxVec; const op: string; const b: double): TMtxVecInt; overload;
      (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
      function FindIndexes(const a: TMtxVec; const op: string; const b: TMtxVec): TMtxVecInt; overload;

      function FindIndexesAndLength(const a: TMtxVec; const op: string; const b: TCplx): integer; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindIndexesAndLength(const a: TMtxVec; const op: string; const b: double): integer; overload;
      (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
      function FindIndexesAndLength(const a: TMtxVec; const op: string; const b: TMtxVec): integer; overload;

      (*<summary>Finds a vector mask.</summary>
        
<remarks>The calling vector will hold the those
        elements where the Op comparison between a and b is True.
        Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or  &lt;&gt; </c>

        The calling vector will store the mask, 1 at those index locations
        where the Op comparison was True and 0 at those index locations
        where the Op comparison was false.
</remarks>


        <SeeAlso cref="TVec.GatherSplit"/>
        <SeeAlso cref="TVec.FindAndGather"/>
        <SeeAlso cref="FindIndexes"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="TVec.Gather"/>
        <SeeAlso cref="TMtxVec.Scatter"/>*)
      function FindMask(const a: TMtxVec; const op: string; const b: TCplx): TMtxVecInt; overload;
      function FindMask(const a: TMtxVec; const op: char; const b: TCplx): TMtxVecInt; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindMask(const a: TMtxVec; const op: string; const b: double): TMtxVecInt; overload;
      function FindMask(const a: TMtxVec; const op: char; const b: double): TMtxVecInt; overload;
      (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
      function FindMask(const a: TMtxVec; const op: string; const b: TMtxVec): TMtxVecInt; overload;
      function FindMask(const a: TMtxVec; const op: char; const b: TMtxVec): TMtxVecInt; overload;
     end;




    (*<summary>One dimensional array - vector.</summary>
       
<remarks>Encapsulates routines for vector specific operations. This include
       1D FFt's, autocorrelation, cross-correlation, convolution, sorting,
       discrete cosine and Hilbert transform's and many other utility funcitons.
</remarks>
*)
   
   
   
   TVec = class(TDenseMtxVec    )
   strict private

      procedure TensorProdInt(const Vec: TVec; const Mtx: TMtx; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone);
      procedure PixelDownSample1(Width:  integer;  const Y: TVec; Index,Len: integer; const X,NX: TVec);

      procedure PixelDownSampleInternal(Width:  integer;  const Y: TVec; Index, Len: integer; const X: TVec = nil; const NX: TVec = nil; Equidistant: TEquidistantSample = eqsXEquidistant);
      procedure PixelDownSampleHighInternal(Width:  integer;  const Y: TVec; Index, Len: integer; const X: TVec = nil; const NX: TVec = nil);
      procedure PixelDownSampleLowInternal(Width:  integer;  const Y: TVec; Index, Len: integer; const X: TVec = nil; const NX: TVec = nil);

    

   strict protected
      
      function GetRealValues: string; override;
      function GetComplexValues: string; override;

      function DoubleCachePointer: TDoubleArray; override;
      function DoubleCacheOffset: integer; override;
      function CplxDoubleCachePointer: TCplxDoubleArray; override;
      function CplxDoubleCacheOffset: integer; override;
      function SingleCachePointer: TSingleArray; override;
      function SingleCacheOffset: integer; override;
      function CplxSingleCachePointer: TCplxSingleArray; override;
      function CplxSingleCacheOffset: integer; override;

      procedure SubRangeSaveState; override;
      procedure SetSubRangeDirect(Index, Len: integer); override;
      procedure SetSubRangeDirect(const Src: TMtxVecBase; Index, Len: integer); override;

      function HilbertInternal1(const Vec: TVec): TVec; overload;
      function HilbertInternal2(const Vec: TVec): TVec; overload;

      procedure setActualSize(const byteCount: Int64); override;
      procedure DoSetComplex; override;
      function CacheLength: integer; override;
      function CacheLengthInBytes: Int64; override;
      function StringsToValuesInternal(const srcList: TStrings; const aDelimiter: string): TMtxVecBase; overload; override;
      function ValuesToStringsInternal(const dstList: TStrings; const aDelimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload; override;
      

    




  strict protected

      procedure SetLength(const Value: integer); override;
  public
      function ToolTip: string;
      

      

      constructor Create; overload; override;
      destructor Destroy; override;

      (*<summary>Obtains a pointer to a precreated object from cache.</summary>
        
<remarks>This class method does the same as the CreateIt function.
</remarks>


        <SeeAlso cref="CreateIt"/>*)
       class  function CreateFromCache: TVec; 
      (*<summary>Releases this object back to cache.</summary>
       
<remarks>This method does the same as the FreeIt function.
</remarks>


        <SeeAlso cref="FreeIt"/>*)
      procedure FreeToCache; override;
















      

      (*<summary>Adopts a pointer to one dimensional array.</summary>
      
<remarks>Adopts a pointer to AArray array. The method sets the calling vector <see cref="TMtxVec.FloatPrecision" text="FloatPrecision"/> property to aFloatPrecision,
      and Values1D and CValues1D to Pointer(AArray).

      Notes:
        * Required to call the <see cref="Disown"/> method, before freeing the object
        * do not resize the vector
        * do not call any routines relying on Lapack when "Core edition" is used (not linking dlls), because
          Lapack will assume that adopted memory is a dynamic array and will modify array index -2.
</remarks>


      <SeeAlso cref="Disown"/>*)

      procedure Adopt(AArray: PAPointer; ALength: integer; aFloatPrecision: TMtxFloatPrecision); overload;
      procedure Adopt(AArray: PAPointer; ALength: integer; aIsComplex, aIsDouble: boolean); overload;
      
      procedure Assign(Src: TPersistent); override;


      (*<summary>Biased auto-correlation.</summary>
        
<remarks>Calculates the biased auto-correlation of the vector Vec. The result of Length = Lags is stored in the calling vector (V).
        The Lags parameter must be equal or smaller than the calling vector length. The biased auto-correlation
        is defined by the following equation:

        <IMG name="TVec03"/><para/>

        The Buffer parameter allows the same work memory to be used between consecutive calls to this and other functions.
        The Buffer object will not be resized, if sufficient memory is present.
</remarks>


        

        <example>
        <code>
        using Dew.Math;
        using Dew.Math.Units;

        namespace Dew.Examples
        {
          void Example()
          {
            TVec a,b;
            MtxVec.CreateIt(out a, aut b);
            try
            {
              a.SetIt(false, new double[] {1,2,3,4});
              b.AutoCorrBiased(a,2);
            }
            finally
            {
              MtxVec.FreeIt(ref a, ref b);
            }
          }
        }
        </code></example>

        <SeeAlso cref="AutoCorrNormal"/>
        <SeeAlso cref="AutoCorrUnBiased"/>*)
      function AutoCorrBiased(const Vec: TVec; Lags: integer; const Buffer: TVecInt = nil): TVec;

      (*<summary>Normal auto-corellation.</summary>
        
<remarks>Calculates the normal auto-correlation of the vector Vec. The result of Length = Lags is stored in the calling vector (V).
        The Lags parameter must be equal or smaller than the calling vector (V) length. The normal auto-correlation is defined
        by the following equation:

        <IMG name="TVec04"/><para/>

        The Buffer parameter allows the same work memory to be used between consecutive calls to this and other functions.
        The Buffer object will not be resized, if sufficient memory is present.
</remarks>


        

        <example>
        <code>
        using Dew.Math;
        using Dew.Math.Units;

        namespace Dew.Examples
        {
          void Example()
          {
            TVec a,b;
            MtxVec.CreateIt(out a, aut b);
            try
            {
              a.SetIt(false, new double[] {1,2,3,4});
              b.AutoCorrNormal(a,2);
            }
            finally
            {
              MtxVec.FreeIt(ref a, ref b);
            }
          }
        }
        </code></example>

        <SeeAlso cref="AutoCorrBiased"/>
        <SeeAlso cref="AutoCorrUnBiased"/>*)
      function AutoCorrNormal(const Vec: TVec; Lags: integer; const Buffer: TVecInt = nil): TVec;

      (*<summary>Unbiased auto-correlation.</summary>
        
<remarks>Calculate the unbiased auto-correlation of the vector Vec. The result of Length = Lags is stored in the calling
        vector (V). The Lags parameter must be equal or smaller than the calling vector (V) length. The unbiased
        auto-correlation is defined by the following equation:

        <IMG name="TVec05"/><para/>

        The Buffer parameter allows the same work memory to be used between consecutive calls to this and other functions.
        The Buffer object will not be resized, if sufficient memory is present.
</remarks>


        

        <example>
        <code>
        using Dew.Math;
        using Dew.Math.Units;

        namespace Dew.Examples
        {
          void Example()
          {
            TVec a,b;
            MtxVec.CreateIt(out a, aut b);
            try
            {
              a.SetIt(false, new double[] {1,2,3,4});
              b.AutoCorrUnBiased(a,2);
            }
            finally
            {
              MtxVec.FreeIt(ref a, ref b);
            }
          }
        }
        </code></example>

        <SeeAlso cref="AutoCorrBiased"/>
        <SeeAlso cref="AutoCorrNormal"/>*)
      function AutoCorrUnBiased(const Vec: TVec; Lags: integer; const Buffer: TVecInt = nil): TVec;

      
      procedure Clear; override;
      

      (*<summary>Concatenates an array of TVec objects.</summary>
        
<remarks>Concatenates an array of TVec objects. The method copies the contents of all TVec objects from the Src
        array to the calling object. The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly. An exception is raised, if Complex properties
        of TVec objects do not match.
</remarks>


        

        <example>
        <code>
        using Dew.Math;
        using Dew.Math.Units;

        namespace Dew.Examples
        {
          void Example()
          {
            TVec a,b,c,d;
            MtxVec.CreateIt(out a, out b, out c, out d);
            try
            {
              a.SetIt(false, new double[] {1,2,3});
              b.Copy(a);
              c.Concat([a,b]); // c = [1,2,3,1,2,3]
              d.Size(10,true);
              d.SetZero(0,4);
              d.Concat(4,[c]); // d = [0,0,0,0,1,2,3,1,2,3]
            }
            finally
            {
              MtxVec.FreeIt(ref a, ref b, ref c, ref d);
            }
          }
        }
        </code></example>


        <SeeAlso cref="Split"/>
        <SeeAlso cref="Copy"/>
        <SeeAlso cref="TMtxVec.Copy"/>*)
      function Concat(const Src: array of TVec): TVec; overload;
      (*<summary>Copies the contents of all TVec objects from the Src array to the calling vector elements, starting
        with at Index.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex"/>
        properties of the calling vector must be set explicitly. An exception is raised, if Complex
        properties of TVec objects do not match or if the sum of
        Length's exceeds the Length property of the calling object.
</remarks>
*)
      function Concat(Index: integer; const Src: array of TVec): TVec; overload;

      (*<summary>Single-rate finite, linear convolution of two sequences.</summary>
        
<remarks>Calculate the single-rate finite, linear convolution of two sequences. The results are stored in the calling vector.
        The argument names X and H ar chosen to suggest FIR filtering. The result of the convolution is defined as follows:

        <IMG name="TVec14"/><para/>

        This finite-length convolution is related to infinite-length by:

        <IMG name="TVec15"/><para/>

        In the above equations, x'[n] and h'[n] are the zero-padded (infinite-length) versions of x[n] and h[n]; y'[n] is the
        infinite-length output version of y[n]. Then y'[n] is zero everywhere except over:

        <IMG name="TVec16"/><para/>

        The optional Buffer parameter can be specified to prevent repeated allocation and freeing of memory by the function
        when both X and H are not complex. If the Buffer parameter is left to be nil, the function will allocate and free
        required memory internally. If the parameter is not null, the object will be sized and can be reused on consecutive calls without
        triggering memory reallocation.

        Avoding memory reallocations is crucial for multithreaded algorithms.
</remarks>


        <SeeAlso cref="FFT"/>
        <SeeAlso cref="AutoCorrNormal"/>
        <SeeAlso cref="AutoCorrBiased"/>
        <SeeAlso cref="AutoCorrUnBiased"/>*)
      function Convolve(const X,H: TVec; const Buffer: TVecInt = nil): TVec;

      (*<summary>Copies values from Vec1 and Vec2 (concatenate).</summary>
        
<remarks>Copy each of Vec1 and Vec2 elements to the calling vector (concatenate).
        The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties
        of the calling vector are set implicitly to match Vec1 and Vec2 vector.
</remarks>


        <Example>
        <code>
        var a,b,c: TVec;
        begin
          CreateIt(a,b,c);
          try
            a.SetIt(True,[1,2,3,4]);
            b.Copy(a);
            c.Copy(a,b);  //concatenate a and b and store in c
            // c = [1,2,3,4,1,2,3,4]
          finally
            FreeIt(a,b,c);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Assign"/>
        <SeeAlso cref="CopyMtx"/>*)
      function Copy(const Vec1,Vec2: TMtxVec): TVec; overload;
      function Copy(const Src: TMtxVecInt; const dstFloatPrecision: TMtxFloatPrecision):TMtxVec; override;

      (*<summary>Copy values from matrix.</summary>
        
<remarks>Copy each of Mtx <see cref="TMtx"/> elements to the calling vector.
        The <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly to match Mtx matrix.
        The same can be achieved by calling the <see cref="Copy"/> method.
</remarks>


        <Example>
        <code>
        var b,c: TVec;
              A: TMtx;
        begin
          CreateIt(b,c);
          CreateIt(A);
          try
            a.SetIt(2,2,False,[1,2,3,4]);
            b.CopyMtx(a);   // b = [1,2,3,4]
          finally
            FreeIt(A);
            FreeIt(b,c);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Copy"/>
        <SeeAlso cref="TMtxVec.Copy"/>*)
      procedure CopyMtx(const Mtx: TMtx); overload; deprecated;
      (*<summary>Desc Copy Mtx elements Values1D[Row*fCols+Col]..Values1D[Row*fCols+Col+Len] in the calling vector elements
        [Index]..[Index+Len-1].</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties must be set explicitly.
        An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is True and array borders are overrun or underrun.
        The same can be achieved by calling the Copy method.
</remarks>
*)
      function CopyMtx(const Mtx: TMtx; Index, Row, Col, Len: Integer): TVec; overload; deprecated;

      (*<summary>The cross-correlation of two vectors.</summary>
        
<remarks>Calculate the cross-correlation of two vectors Vec1 and Vec2. The parameter HiLag indicates the top of the range
        of lags at which the correlation estimates should be computed. The parameter LoLag indicates the bottom of the
        range of lags at which the correlation estimates should be computed. The results are stored in calling vector.
        The resulting elements are defined by the equation:

        <IMG name="TVec17"/><para/>

        The Buffer parameter allows the same work memory to be used between consecutive calls to this and other functions.
        The Buffer object will not be resized, if sufficient memory is present.
</remarks>


        <Example>
        <code>
        var a,b,c: TVec;
        begin
          CreateIt(a,b,c);
          a.SetIt(False,[1,-2,3,4]);
          b.SetIt(False,[2,-2,3,4]);
          c.CrossCorr(a,b,2,2);
          FreeIt(a,b,c);
        end;
        </code>
        </Example>

        <SeeAlso cref="AutoCorrBiased"/>
        <SeeAlso cref="AutoCorrNormal"/>
        <SeeAlso cref="AutoCorrUnBiased"/>*)
      function CrossCorr(const Vec1, Vec2: TVec; HiLag, LoLag: integer; CorrelationNorm: TCorrNorm = cnDefault; const Buffer: TVecInt = nil): TVec;

      (*<summary>Cumulative sum.</summary>
      
<remarks>Calculate the cumulative sum for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: TVec;
      begin
        CreateIt(a);
        try
          a.SetIt(false,[1,2,3,4];
          a.CumSum; // a = [1,3,6,10]
        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

        <SeeAlso cref="Sum"/>*)
      function CumSum: TVec; overload;

      (*<summary>Calculate the cumulative sum for all Vec elements.</summary>
        
<remarks>Store the results in calling object. Size and  <see cref="TMtxVec.Complex">Complex</see> properties of
        the calling object are set implicitly.
</remarks>
*)
      function CumSum(const Vec: TVec): TVec; overload;

      (*<summary>Calculate the cumulative product for all Vec elements.</summary>
                
<remarks>The size must be set by the user. The function will store all powers of Value in
                Values[i] := IntPower(Value, i) at corresponding array index.
</remarks>
*)
      function CumProduct(Value: Double): TMtxVec; overload;

      (*<summary>Calculate the cumulative product for all Vec elements.</summary>
                
<remarks>The size must be set by the user. The function will store all powers of Value in
                CValues[i] := IntPower(Value, i) at corresponding array index.
</remarks>
*)
      function CumProduct(const Value: TCplx): TMtxVec; overload;

      (*<summary>The forward discrete cosine transform (DCT).</summary>
        
<remarks>Calculates the forward discrete cosine transform (DCT) of all calling vector elements in-place.
        If vector <see cref="TMtxVecBase.Length" text="Length"/> is a power of 2, the function uses an efficient algorithm that
        is significantly faster than the direct computation of DCT. For different lengths this function uses the
        direct formulas given below; however, the symmetry of cosine function is taken into account, which allows
        to perform about half of the multiplication operations in the formulas. In the following definition of DCT,
        <c>N=Vec.Length</c> and V is the calling vector:

        <IMG name="TVec18"/><para/>
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          a.SetIt(False,[1,-2,3,4]);
          b.DCT(a);
          FreeIt(a,b);
        end;
        </code>
        </Example>

        <SeeAlso cref="IDCT"/>
        <SeeAlso cref="FFT"/>
        <SeeAlso cref="TDenseMtxVec.IDCT"/>
        <SeeAlso cref="TDenseMtxVec.IDCT"/>
        <SeeAlso cref="TDenseMtxVec.FFT"/>*)
      function DCT: TVec; overload;
      (*<summary>Calculates the forward discrete cosine transform (DCT) of the Vec.</summary>
        
<remarks>Writes the result in the calling vector.
        If Vec.Length is a power of 2, the function uses an efficient algorithm that is significantly faster than
        the direct computation of DCT. For other values of Vec Length, this function uses the direct formulas.
</remarks>
*)
      function DCT(const Vec: TVec): TVec; overload;

      (*<summary>Copies the k-th diagonal from the TMtx object.</summary>
        
<remarks>Copies the k-th diagonal from the TMtx object. If <c>k = 0</c> then the main diagonal is copied,
        if <c>k &lt; 0</c> then the subdiagonal is copied and if <c>k &gt; 0</c> then the k-th super diagonal is copied to the calling vector.
</remarks>


        <Example>In the following example we setup a matrix, populate it with values and then extract it's main diagonal to a vector.
        <code>
        var a: TVec;
            d: TMtx;
        begin
          CreateIt(a);
          CreateIt(d);
          try
            // setup matrix
            d.SetIt(2,2,False,[1,-2,
                              3, 4]);
            // get main diagonal from matrix
            a.Diag(d,0); // a now containes [1,4];
          finally
            FreeIt(a);
            FreeIt(d);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="TMtx.Diag"/>*)
      function Diag(const Mtx: TMtx; k: integer): TVec;

      (*<summary>The difference between two succesive vector elements.</summary>
        
<remarks>Calculate the difference for all calling vector elements. The following formula is used to
        calculate the difference:

        <IMG namr="tvec19"/><para/>

        The Length of calling vector is automatically decremented by one.
</remarks>
*)
      function Difference(Lag: Integer = 1): TVec; overload;
      (*<summary>Calculate the difference for all Vec elements.</summary>
        
<remarks>Store the results in the calling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
        of the calling vector is set to one less the length of Vec and <see cref="TMtxVec.Complex"/>
        property is set to Vec.Complex.
</remarks>
*)
      function Difference(const Vec: TMtxVec; Lag: Integer = 1): TVec; overload;


      (*<summary>Disowns a values pointer.</summary>
        
<remarks>Disowns a values pointer. The Disown method is the opposite of the <see cref="Adopt"/> method. It will set the AArrays to
        Pointer(Values), ALength to vector's <see cref="TMtxVecBase.Length"/> and IsComplex to <see cref="TMtxVec.Complex">Complex</see> property.
        Use the Disown method to "disconnect" AArray from the TVec.Values. Disown sets Values and CValues array pointers to nil and Length property to
        zero, but without freeing the allocated memory. The allocated memory can be disowned only, if it was adopted with a call to
        the <see cref="Adopt"/> method.
</remarks>


        <SeeAlso cref="Adopt"/>*)
      
      procedure Disown(out AArray: PAPointer; out ALength: integer; out aFloatPrecision: TMtxFloatPrecision); overload;
      procedure Disown(out AArray: PAPointer; out ALength: integer; out aIsComplex, aIsDouble: boolean); overload;
      

      (*<summary>Disowns a calling vector values pointer by setting Values, CValues to nil and <see cref="TMtxVecBase.Length" text="Length"/> to 0.</summary>*)
      procedure Disown; overload;


      (*<summary>Downsamples vector values.</summary>
        
<remarks>The methods copies only every Factor sample from the Src vector to the calling vector.
        The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties
        of the calling vector are set implicitly. The phase parameter determines the initial sample offset.
        Phase must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            b.SetIt(False,[0,0,0,0,1,2,3,4,5,6]);
            a.DownSample(b,2); // a = [0,0,1,3,5]
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="UpSample"/>
        <SeeAlso cref="TDenseMtxVec.UpSample"/>
        <SeeAlso cref="PixelDownSample"/>*)
      function DownSample(const Src: TMtxVec; Factor: integer; Phase: integer = 0): TVec; overload;

      (*<summary>Compares vector elements and returns true if vectors are equal.</summary>
        
<remarks>Compares vector elements and returns true if vectors are equal, that is if all elements match in position
        and value. The Tolerance parameter defines the comparison tolerance. The maximum difference between elements
        may not exceed: <c>+/-Tolerance</c>.
</remarks>


        <Example>
        <code>
        var A,B: TMtx;
            c: boolean;
        begin
          CreateIt(A,B);
          A.SetIt(2,2,false,[1,2,
                            2,4]);  // 2x2 real matrix
          B.SetIt(2,2,false,[1,2,
                            2,4]);  // 2x2 real matrix

          c := A.Equal(B,1e-8); // Check for differences bigger than 0.00000001
          c := A.Equal(B); // Check for exact match
          FreeIt(A,B);
        end;
        </code>
        </Example>

        <SeeAlso cref="Find"/>*)
      function Equal(const Vec: TMtxVec; Tolerance: double = 0): boolean; overload;
      (*<summary>Optionally it is also possible to specify the Compare method.</summary>*)
      function Equal(const Vec: TMtxVec; Tolerance: double; Compare: TCompare): boolean; overload;



      (*<summary>Fast Furier Transformation (FFT) from complex to complex or from real to complex.</summary>
        
<remarks>Fast Furier Transformation (FFT) from complex to complex or from real to complex.
        Calculate the FFT from all calling vector elements in-place. If the calling vector is
        complex then complex to complex forward FFT is performed. If the calling vector is real
        then real to complex FFT is  performed.

        The <see cref="TMtxVecBase.Length" text="Length"/> of the transforming vector can be any number but highest performance will
        be achieved if it is a power of two.

        Note
          When performing FFT from real to complex the conjugated symmetric part
          is also generated by conjugating and mirroring the first half of the spectrum.
          If Complex is false, the operation will not be performed in-place, if the Length
          of the destination will allocate more than amount of the memory preallocated
          for the TVec object. Instead, the data will be copied first to allocate enough space
          to store the result.

        There are two important parameters to consider that affect how will the FFT
        be computed: <see cref="TDenseMtxVec.FFTStorageFormat"/> and <see cref="TDenseMtxVec.FFTScrambled"/>.
        The default storage format is fsfCCS.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(False,[1,2,3,4]);
            b.FFT(a);
            a.FFT;     // a = [(10, 0),( -2,2), (-2, 0), (-2,-2)]
            if not a.Equal(b) then ERaise('Not equal');
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="FFTFromReal"/>
        <SeeAlso cref="IFFT"/>
        <SeeAlso cref="TDenseMtxVec.FFTFromReal"/>
        <SeeAlso cref="TDenseMtxVec.IFFT"/>
        <SeeAlso cref="TDenseMtxVec.FFTStorageFormat"/>
        <SeeAlso cref="TDenseMtxVec.FFTScrambled"/>*)
      function FFT(ConjugateExtend: boolean = false): TVec; overload;

      function CopyToComplex(const Src: TVec): TVec; overload;
      function CopyToComplex(const Src: TVec; SrcIndex, Index, Len: integer): TVec; overload;
      (*<summary>Calculate the FFT from all Vec elements.</summary>
          
<remarks>Store the results in the calling vector. Vec <see cref="TMtxVecBase.Length" text="Length"/> must not be a power of two,
          but highest performance will be achieved if it is a power of two. Length and <see cref="TMtxVec.Complex">Complex</see>
          properties of the calling vector are set implicitly to match Vec vector. If Vec is complex then complex
          to complex forward FFT is performed. If Vec is real then real to complex
          FFT is performed and the conjugate symmetric part is appended.
</remarks>
*)
      function FFT(const Vec: TVec; ConjugateExtend: boolean = false): TVec; overload;

      (*<summary>The forward Fast Fourier Transformation (FFT) from real to complex.</summary>
        
<remarks>Calculates the forward Fast Fourier Transformation (FFT) from real to complex
        for the calling vector in-place. The transform should be used, when
        the conjugate symmetric part of the frequency spectrum is not desired.
        If the calling vector is complex an exception will be raised.
        The operation implicitly sets the calling vector Complex property to True.
        The highest performance will be achieved if the transform length will be a power of two.
        The transform length is equal to <see cref="TMtxVecBase.Length" text="Length"/>,
        when the <see cref="TDenseMtxVec.FFTStorageFormat"/> is fsfPack
        or fsfPerm. The default storage format is fsfCCS.

        <b>In-place fsfCCS complication</b>

        The transform length in case of fsfCCS will be equal to Length-2,
        because the result is bigger than the source data by 2 real samples, if the source
        data is even. If the source data length is odd, then <see cref="TDenseMtxVec.FFTOddLength"/> must
        be set to True and only Length-1 samples will be used, but Length must
        of course in that case be even, or Length-1 will not be odd.
        The last two (one) samples in the vector will be ignored and
        will be overwritten with the result.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try  //Even
            a.SetIt(false,[1,2,3,4]);
            b.FFTFromReal(a); // b = [(10, 0),( -2,2), (-2, 0)]
            a.SetIt(false,[1,2,3,4, 0, 0]); //result requires 3 complex (or 6 real values)
            a.FFTFromReal; // b = [(10, 0),( -2,2), (-2, 0)]

            //Odd length
            a.SetIt(false,[1,2,3]);

            b.FFTOddLength := True; //use only Length-1 samples
            b.FFTFromReal(a); // b = [(6,0),( -1.5,0.8660)] //result requires 2 complex (= 4 real numbers)

            a.SetIt(false,[1,2,3,0]);

            a.FFTOddLength := True; //use only Length-1 samples
            a.FFTFromReal; // b = [(6,0),( -1.5,0.8660)] //result requires 2 complex (= 4 real numbers)
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="FFT"/>
        <SeeAlso cref="TDenseMtxVec.FFT"/>
        <SeeAlso cref="IFFT"/>
        <SeeAlso cref="TDenseMtxVec.IFFT"/>*)
      function FFTFromReal: TVec; overload;
      (*<summary>Calculate the FFT from all Vec elements.</summary>
        
<remarks>Store the result in the calling vector. <see cref="TMtxVecBase.Length" text="Length"/> of
        the calling vector is set to match the length of the result.
</remarks>
*)
      function FFTFromReal(const Vec: TVec): TVec; overload;

      (*<summary>Finds and gathers vector elements.</summary>
        
<remarks>Fills the Indexes vector with indexes, of those elements where the Op
        comparison between a and b is True.  Op string parameter can be '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;'.
        The method also copies or "gathers" the matched elements to the calling vector.
        Both the calling vector and Indexes will be sized to match a.Length. On return, both will be subranged
        to reflect actual number of matching elements. The method will not raise an exception, if the calling vector (Self)
        or Indexes parameter are already subranged.
</remarks>
*)
      function FindAndGather(const a: TMtxVec; const op: string; const b: TCplx; const Indexes: TVecInt = nil): TVec; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindAndGather(const a: TMtxVec; const op: string; const b: double; const Indexes: TVecInt = nil): TVec; overload;
      (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
      function FindAndGather(const a: TMtxVec; const op: string; const b: TMtxVec; const Indexes: TVecInt = nil): TVec; overload;


      (*<summary>Gather vector elements.</summary>
        
<remarks>Gather the elements of vector X and store them to the calling vector
        according to the IndexType, Increment and Offset parameters.

        The Indexes vector is used only if IndexType is either indVector
        or indMask. If IndexType is indVector, the values from the Indexes
        vector denote the index positions in the X vector from which
        the values should be copied to the calling vector.
        The Indexes vector must have the indexes stored in the IValues
        array. The IValues integer arrays points the same memory as Values
        array.

        The Increment and Offset parameters are used only if TIndexType
        is indIncrement. They define the initial offset and a fixed
        step (increment) between elements to be gathered. If IndexType is indMaks the Indexes vector must have the same
        size as the X vector. The routine will copy only those elements
        from the X vector to the calling vector, for which there is a 1 at the coresponding
        index in the Indexes vector.

        The elements copied from the X vector will be stored consecutively in the calling vector.

        See the <see cref="TMtxVec.Scatter"/> method to see how to undo the gathering.

        The performance of the CPU heavily depends on the assumption that elements are stored at consecutive memory locations.
        If it is neccessary to apply a set of operations only to elements at specific indexes, performance-wise it can
        proof to be very helpfull, if the elements are gathered first.
</remarks>


        <SeeAlso cref="TVec.GatherSplit"/>
        <SeeAlso cref="TVec.FindAndGather"/>
        <SeeAlso cref="FindIndexes"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="TVec.Gather"/>
        <SeeAlso cref="TMtxVec.Scatter"/>*)
      function Gather(const X: TMtxVec; const Indexes: TMtxVecInt =nil; IndexType: TIndexType = indVector; Increment: integer = 1; Offset: integer = 0): TVec; overload;

      function GatherByIncr (const X: TMtxVec; Increment: integer = 1; Offset: integer = 0): TVec; overload;
      function GatherByIndex(const X: TMtxVec; const Indexes: TVecInt): TVec; overload;
      function GatherByMask (const X: TMtxVec; const Mask: TMtxVecInt; const MaskNonZeroCount: integer = -1): TVec; overload;

      (*<summary>Gather a vector, split to two vectors.</summary>
        
<remarks>If the elements of a vector have been split with a Mask and
        the "positive" elements have been stored in the MaskVec and
        "negative" elements have been stored in the NotMaskVec, this
        routine will restore the original vector.

        The Mask vector stores the mask as Integers in to the memory
        location occupied by Values array. The memory is typecasted
        to an array of integers via IValues pointer. Other TVec methods
        can not be used to perform operations on an array of integers
        unless explicitly specified.

        The length of the MaskVec vector must be equal to the number of ones in the Mask vector.
        The length of the NotMaskVec vector must be equal to the number of zeroes in the Mask vector.
</remarks>


        <SeeAlso cref="FindAndSplit"/>*)
      function GatherSplit(const MaskVec, NotMaskVec: TMtxVec; const Mask: TVecInt): TVec; overload;

      (*<summary>Copies a column from matrix.</summary>
        
<remarks>Copies the Col-th column from Mtx matrix to calling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
        and <see cref="TMtxVec.Complex"/> properties of calling vector are adjusted automatically.
        An exception is raised if condition checking is enabled and Col is greater than <c>Mtx.Cols-1.</c>
</remarks>


        <Example>
        <code>
        var a: TVec;
            b: TMtx;
        begin
          CreateIt(a);
          CreateIt(b);
          try
            b.SetIt(2,2,False,[1,2,
                              3,4]);
            a.GetCol(b,0); // a now contains [1,3]
          finally
            FreeIt(a);
            FreeIt(b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="GetRow"/>
        <SeeAlso cref="TMtx.SetCol"/>*)
      function GetCol(const Mtx: TMtx; Col: integer): TVec; overload;
      (*<summary>Copy the Col-th column elements [Row]..[Row+Len-1] to calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>The <see cref="TMtxVec.Complex">Complex</see> property of calling vector must be set explicitly. An exception is raised if condition checking is
        enabled and column array borders are overrun.
</remarks>
*)
      function GetCol(const Mtx: TMtx; Row, Col, Index, Len: integer): TVec; overload;
      (*<summary>Copy the Col-th column elements [Row]..[Row+Len-1] to calling vector.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of calling vector are adjusted automatically.
        An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
      function GetCol(const Mtx: TMtx; Row, Col, Len: integer): TVec; overload;

      (*<summary>Copies a row from matrix.</summary>
        
<remarks>Copies the Row-th row from Mtx matrix to calling vector. The <see cref="TMtxVecBase.Length"/> and
        <see cref="TMtxVec.Complex">Complex</see> properties of calling vector are adjusted automatically.
        An exception is raised if condition checking is enabled and Row is greater than
        <c>Mtx.Rows-1.</c>
</remarks>


        <Example>
        <code>
        var a: TVec;
            b: TMtx;
        begin
          CreateIt(a);
          CreateIt(b);
          try
            b.SetIt(2,2,False,[1,2,
                              3,4]);
            a.GetRow(b,0); // a now contains [1,2]
          finally
            FreeIt(a);
            FreeIt(b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="GetCol"/>
        <SeeAlso cref="TMtx.SetRow"/>*)
      function GetRow(const Mtx: TMtx; Row: integer): TVec; overload;
      (*<summary>Copy the Row-th column elements [Col]..[Col+Len-1] to calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>The <see cref="TMtxVec.Complex">Complex</see> property of calling vector must be set
        explicitly. An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
      function GetRow(const Mtx: TMtx; Row, Col, Index, Len: integer): TVec; overload;
      (*<summary>Copy the Row-th column elements [Col]..[Col+Len-1] to calling vector.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of calling
        vector are adjusted automatically. An exception is raised if condition checking is
        enabled and column array borders are overrun.
</remarks>
*)
      function GetRow(const Mtx: TMtx; Row, Col, Len: integer): TVec; overload;

      (*<summary>The fast hilbert transform (FFT based).</summary>
        
<remarks>The method forms the imaginary orthogonal part (90 degrees phase shifted version of the original)
        from the real series by using the fast hilbert transform (FFT based) and saves the complex result in the
        calling vector. No windowing is performed. Vec must be a real vector or an exception is raised.
        The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling vector are set
        implicitly to match Vec vector.
</remarks>


        <Example>
        <code>
        var a,b,Re,Im: TVec;
            azero: double;
        begin
          CreateIt(a,b,Re,Im);
          try
            a.SetIt(false,[1,2,3,4,5,6,7,8]);
            b.Hilbert(a);
            Re.RealPart(b);
            Im.ImagPart(b);
            azero := Re.DotProd(Im);
            // if Re is orthogonal to Im, azero becomes 0
          finally
            FreeIt(a,b,Re,Im);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="FFT"/>
        <SeeAlso cref="DCT"/>*)
      function Hilbert(const Vec: TVec; const method: THilbertMethod = hmMethod1): TVec; overload;
      (*<summary> Sums items from Data with matching corresponding indexes from Bins and places result in self.</summary>
                 
<remarks>The size of self is set to BinsMax+1 provided that sufficient memory is available.
                 BinsMax with default value of -1 indicates that Bins.Max call is to be made internally.
                 The routine does not perform additional range checking, if BinsMax is provided
                 explicitely (memory overwrite is possible).
</remarks>
*)
      function GroupSum(const Bins: TMtxVecInt; const Data: TMtxVec; const BinsMax: integer = -1): TVec; overload;
      (*<summary> Sums items from Data with matching corresponding indexes from Bins and places result in self.</summary>
                 
<remarks>The size of self is checked to be BinsMax+1. The routine does not initialize the calling vector to 0.
                 The routine does not perform additional range checking, because BinsMax is provided
                 explicitely (memory overwrite is possible).
</remarks>
*)
      function GroupSumIteration(const Bins: TMtxVecInt; const Data: TMtxVec; const BinsMax: integer): TVec; overload;

      (*<summary>The inverse discrete cosine transform (DCT).</summary>
        
<remarks>Calculates the inverse discrete cosine transform (DCT) of a Vec and writes the results in the calling vector.
        If Vec <see cref="TMtxVecBase.Length" text="Length"/> is a power of 2, the function uses an efficient algorithm that is significantly
        faster than the direct computation of DCT. For other values of Vec length, this function uses the direct
        formulas given below; however, the symmetry of cosine function is taken into account, which allows to
        perform about half of the multiplication operations in the formulas. In the following definition of inverse
        DCT, N=Vec.Length and V is the calling vector:

        <IMG name="TVec20"/><para/>
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(False,[1,-2,3,4]);
            b.IDCT(a);
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="DCT"/>*)
      function IDCT(const Vec: TVec): TVec; overload;
      (*<summary>Calculate the inverse DCT in-pllace.</summary>
      
<remarks>The length of the calling vector is adjusted automatically.
</remarks>
*)
      function IDCT: TVec; overload;

      (*<summary>The inverse backward FFT from complex to complex.</summary>
       
<remarks>Calculates the inverse (backward) Fast Fourier Transformation (FFT) from complex to complex for all
       calling vector elements in-place. If the NoScale parameter is true, then no scaling is performed for
       the calling vector. The results can be erroneous if overflow or underflow occurs.
</remarks>


       <Example>
       <code>
       var a,b: TVec;
       begin
        CreateIt(a,b);
        try
          a.SetIt(True,[1,2,3,4]);
          b.IFFT(a,True);
        finally
          FreeIt(a,b);
        end;
       end;
       </code>
       </Example>

        <SeeAlso cref="FFT"/>
        <SeeAlso cref="TDenseMtxVec.FFT"/>*)
      function IFFT(NoScale: boolean = False): TVec; overload;
      (*<summary>Calculate the inverse FFT from all Vec elements.</summary>
        
<remarks>Store the results in the calling vector. If the NoScale parameter is true then no scaling is
        performed for the calling vector.
        Vec <see cref="TMtxVecBase.Length" text="Length"/> must not be the power of two. <see cref="TMtxVecBase.Length" text="Length"/>
        and <see cref="TMtxVec.Complex">Complex</see> properties of the calling vector are set
        implicitly to match Vec vector.
</remarks>
*)
      function IFFT(const Vec: TVec; NoScale: boolean = False): TVec; overload;

      (*<summary>The inverse FFT from complex to real.</summary>
        
<remarks>Calculates the inverse Fast Fourier Transformation (FFT) from
        complex to real from all calling vector elements.
        The result is a real type vector.
        Input vector must be a complex vector or an exception is raised.
        If the NoScale parameter is true then no scaling is performed for
        the calling vector. The results can be erroneous if overflow or
        underflow occurs.

        <b>In-place fsfCCS complication</b> <para/>

        When <see cref="TDenseMtxVec.FFTStorageFormat"/> is equal to fsfCCS the
        Length of the result will be equal to Length-2, because the source
        data is bigger than the result data by 2 real samples,
        if <see cref="TDenseMtxVec.FFTOddLength"/> is false.
        If <see cref="TDenseMtxVec.FFTOddLength"/> is True the result will fill up
        <c>Length-1</c> samples.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try  //Even
            a.SetIt(True,[1,2,3,4]);
            b.FFTFromReal(a); // b = [(10, 0),( -2,2), (-2, 0)]
            b.IFFTToReal;     // b = [1, 2, 3, 4, -2, 0)]

            //Odd length
            a.SetIt(True,[1,2,3,0]);

            b.FFTOddLength := True; //use only Length-1 samples
            b.FFTFromReal(a); // b = [(6,0),( -1.5,0.8660)] //result requires 2 complex (= 4 real numbers)
            b.IFFTToReal;      //b = [1, 2, 3, 0.8660]

            //Event length
            a.SetIt(True,[1,2,3,4, 0,0]); //allocate two more elements
            a.FFTOddLength := false; //use only Length-2 samples
            a.FFTFromReal; // a = [(10, 0),( -2,2), (-2, 0)] //result requires 3 complex (= 6 real numbers)
            a.IFFToReal;   // a = [1, 2, 3, 4, -2, 0)]
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="FFTFromReal"/>
        <SeeAlso cref="TDenseMtxVec.FFTFromReal"/>
        <SeeAlso cref="IFFT"/>
        <SeeAlso cref="TDenseMtxVec.FFT"/>*)
      function IFFTToReal(NoScale: boolean = False): TVec; overload;
      (*<summary>Calculate the inverse FFT from all Vec elements.</summary>
        
<remarks>Store the results in the calling vector. Vec <see cref="TMtxVecBase.Length" text="Length"/> must
        not be the power of two. <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly the size and type of the result.
</remarks>
*)
      function IFFTToReal(const Vec: TVec; NoScale: boolean = False): TVec; overload;

      (*<summary>Integrate calling vector values.</summary>
        
<remarks>Perform d-times integration of calling vector values, where d is equal to Init length.
</remarks>

        <param name="aInit">Definies initial values for integration. Size of Init vector determines how many times calling vector
        values will be integrated.</param>*)
      function Integrate(const aInit: TVec): TVec; overload;
      (*<summary>Integrate all Vec elements using initial values in Init.</summary>
        
<remarks>Store the results in calling vector. <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly the size and type of the result.
</remarks>
*)
      function Integrate(const Vec: TVec; aInit: TVec):TVec; overload;

      (*<summary>The Kronecker product between two vectors.</summary>
        
<remarks>Calculates the Kronecker product between vectors Vec1 and Vec2 and stores the results in calling vector.
</remarks>


        <Example>
        <code>
        var a,b,c: TVec;
        begin
          CreateIt(a,b,c);
          try
            a.SetIt(False,[1,2,3,4]);
            b.SetIt(False,[1,2,3,4]);
            c.Kron(a,b);  // c = [1,2,3,4, 2,4,6,8, 3,6,9,12, 4,8,12,16]
          finally
            FreeIt(a,b,c);
          end;
        end;
        </code>
        </Example>*)
      function Kron(const Vec1, Vec2: TVec): TVec; overload;

      (*<summary>The norm of a vector.</summary>
        
<remarks>Calculates the norm of a Vec vector and stores the results in calling vector.
        This functions works the same as <see cref="TMtxVec.PowerSpectrum"/>.
</remarks>
*)
      function Norm(const Vec: TMtxVec): TMtxVec;











      (*<summary>Downsamples (reduces) the number of vector elements.</summary>
        
<remarks>This method speeds up drawing of huge amounts of data
        (>> 2000 samples). You should pass your data to the
        PixelDownSample method before you pass the values to the Charting
        routine. The X and Y vectors contain the values for X and Y axis,
        that you would normally pass to the charting procedure. The
        downsampled Y is stored in the calling vector and downsampled X is stored
        in the NX vector. If your data is equidistant on X axis,
        you can omit the X vector or, if you are in doubt, allow the routine to
        downsample the X vector also. If your data is not equidistant on the
        X axis, you must indicate that by specifying the Equidistant parameter as
        False and provide X and NX vectors, where X containes the step on the X axis
        and NX will contain the downsampled result for X axis.

        With Width parameter you specify the width of the charting
        region in pixels. (Example: Chart.Width). The routine will reduce
        the number of samples in vectors Y and X in such a way that there
        will be no visual difference between the original and downsampled data.
        That however will no longer be true, if you will zoom-in
        on the data. The performance gain can be as big as 100x depending on
        the charting tool that you use. You can easily draw data series from
        vectors with length of over 1.000.000 samples in real time.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(False,[0,0,0,1,2,3,1,2,3]);
            b.PixelDownSample(3,a);
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="DownSample"/>*)
      procedure PixelDownSample(Width:  integer;  Y: TVec; Index,Len: integer; X: TVec = nil; NX: TVec = nil; Equidistant: TEquidistantSample = eqsXEquidistant); overload;
      (*<summary>Downsample the vectors Y and (optionaly) X starting at position Index
        and for Len samples.</summary>
        
<remarks>Store result for Y in the calling vector and the result for  X in the NX vector.
        If X axis is equidistant, you can omit X and NX parameters.
        Set Width to the number of target pixels. An exception is raised,
        if any of the vectors is complex or if array borders are
        overrun/underrun.
</remarks>
*)
      procedure PixelDownSample(Width:  integer;  Y: TVec; X: TVec = nil; NX: TVec = nil; Equidistant: TEquidistantSample = eqsXEquidistant); overload;

      (*<summary>Downsample the vectors Y and (optionaly) X starting at position Index
        and for Len samples.</summary>
        
<remarks>The method downsamples only the maximum values.
        This can be usefull, if the input is already a downsampled signal which
        has to be further downsampled.
</remarks>


        <Example>
        <code>

        var a,b,c,d,High,Low: TVec;
        begin
          CreateIt(a,b,c,d);
          CreateIt(High,Low);
          try
            a.SetIt(False,[0,0,0,1,2,3,1,2,3]);
            b.PixelDownSample(4,a);
            High.Demultiplex(b,2,0);
            Low.Demultiplex(b,2,1);

            c.PixelDownSampleHigh(2,High); //further downsample
            d.PixelDownSampleLow(2,Low); //further downsample

            b.Size(2*c.Length);
            b.Multiplex(c,2,0);
            b.Multiplex(d,2,1);

            //b now contains the same result, as if we would have called
            //on the start:

            b.PixelDownSample(2,a);

          finally
            FreeIt(a,b,c,d);
            FreeIt(High,Low);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="DownSample"/>*)
      procedure PixelDownSampleHigh(Width:  integer;  Y: TVec; Index, Len: integer; X: TVec = nil; NX: TVec = nil); overload;

      procedure PixelDownSampleHigh(Width:  integer;  Y: TVec; X: TVec = nil; NX: TVec = nil); overload;

      (*<summary>Downsample the vectors Y and (optionaly) X starting at position Index
        and for Len samples.</summary>
        
<remarks>The method downsamples only the minimum values.
        This can be usefull, if the input is already a downsampled signal which
        has to be further downsampled.
</remarks>
*)
      procedure PixelDownSampleLow(Width:  integer;  Y: TVec; Index, Len: integer; X: TVec = nil; NX: TVec = nil); overload;

      procedure PixelDownSampleLow(Width:  integer;  Y: TVec; X: TVec = nil; NX: TVec = nil); overload;

      (*<summary>Fills vector with prime numbers, starting from 2 up to the value of n.</summary>
        
<remarks>Fills calling vector with prime numbers, starting from 2 up to the value of n.
        The memory for at least n elements is allocated. The maximum value of n
        is 1000. The prime numbers are fetched from a precreated table.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.PrimeNumbers(10); // a = [2,3,5,7]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>*)
      function PrimeNumbers(n: integer): TVec;

      (*<summary>Fills the calling vector with a series following linear rule.</summary>
        
<remarks>Fills the calling vector with a series following the rule:

        <code>
        Values[k] :=  k
        </code><para/>

        (Offset is zero and Step is one). If the calling vector is complex,
        only the real part is set.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.Size(5,True);
            a.Ramp(0,Pi);
            a.Sin;
          finally
            FreeIt(a);
          end;
        end;
        </code><para/>
        which is identical to:<para/>
        <code>
        CreateIt(a);
        try
          a.Size(5,True);
          for i:= 0 to a.Length-1 do a[i] := sin(i*Pi);
        finally
          FreeIt(a);
        end;
        </code>
        </Example>

        <SeeAlso cref="TMtxVec.SetVal"/>*)
      function Ramp: TVec; overload;
      (*<summary>Fills the calling vector with a series.</summary>
        
<remarks>Method follow the rule:

        <code>
        CValues[k] := Offset + k*Step.
        </code><para/>
</remarks>
*)
      function Ramp(const Offset, Step: TCplx): TVec; overload;
      (*<summary>Fills the calling vector.</summary>
        
<remarks>Method uses the following rule:

        <code>
        Values[k] := Offset + k*Step.
        </code><para/>
        If the calling vector is complex, only the real part is set.
</remarks>
*)
      function Ramp(const Offset, Step: double): TVec; overload;
      (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>Method uses the following rule:

        <code>Values[k] := Offset + k*Step.
        </code><para/>
        If the calling vector is complex, only the real part is set.
        An exception is raised if calling vector
        array borders are overrun.
</remarks>
*)
      function Ramp(const Offset,Step: double; Index,Len: integer): TVec; overload;
      (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>Following the rule:

        <code>
        Values[k] := Offset + k*Step.
        </code><para/>
        If the calling vector is complex, only the real part is set.
        An exception is raised if array borders of the calling vector
        are overrun.
</remarks>
*)
      function Ramp(const Offset, Step: TCplx; Index,Len: integer): TVec; overload;
      (*<summary>The Offset is complex, but the step is real.</summary>*)
      function Ramp(const Offset: TCplx; const Step: double): TVec; overload;
      (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>Following the rule:
        <code>
        Values[k] := Offset + k*Step.
        </code><para/>
        The Offset is complex, but the step is real.
</remarks>
*)
      function Ramp(const Offset: TCplx; const Step: double; Index,Len: integer): TVec; overload;

      
      
      function ReadValues(const SrcStream: TStream; Precision: TPrecision; Endian: TEndianness = boLittleEndian): Int64; override;
      
      

      
      procedure Reset; override;
      

      (*<summary>Resizes vector size while preserving values.</summary>
        
<remarks>Resizes calling vector <see cref="TMtxVecBase.Length" text="Length"/> to Len and fills it with Src vector
        first Len values. If Src length is less than Len and ZeroIt parameter is true, the remaining
        calling vector values are set to zero.
</remarks>


        <Example>
        <code>
        var a, b: Tvec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(false,[1,2,3]);
            b.SetIt(false,[9]);
            b.Resize(a,7,True); // b=(9,1,2,3,4,0,0,0)
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>*)
      function Resize(const Src: TMtxVec; Len: integer; ZeroIt: boolean = False): TVec; overload;

      (*<summary>Resizes calling vector Length to Len.</summary>
         
<remarks>If <see cref="TMtxVecBase.Length" text="Length"/> is less than Len and ZeroIt parameter is true, the remaining
        calling vector values are set to zero.
</remarks>


        <Example>
        <code>
        var a: Tvec;
        begin
          CreateIt(a);
          try
            a.SetIt(false,[1,2,3]);
            a.Resize(7,True); // a= [1,2,3,0,0,0,0]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>*)
      function Resize(Len: integer; ZeroIt: boolean = False): TMtxVec; overload;

      (*<summary>Reverse vector elements.</summary>
        
<remarks>The method reverses vector elements by using the following equation:

        <IMG name="TVec24"/><para/>

        This overload reverses all calling vector elements in-place.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.SetIt(False,[1,2,3,4]);
            a.Reverse;   // a = [4,3,2,1]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Rotate"/>
        <SeeAlso cref="Shift"/>*)
      function Reverse: TVec; overload;
      (*<summary>Reverse all Vec elements.</summary>
        
<remarks>Xtore the result in the calling vector elements. The <see cref="TMtxVecBase.Length" text="Length"/>
        and <see cref="TMtxVec.Complex">Complex</see> roperties of the calling vector are
        set implicitly to match Vec vector.
</remarks>
*)
      function Reverse(const Vec: TMtxVec): TVec; overload;

      (*<summary>A cyclic shift on vector elements.</summary>
        
<remarks>Performs cyclic shift on vector elements. The number of elements to shift is specified in the Offset parameter. Offset can be
        any integer number, positive or negative.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.SetIt(False,[1,2,3,4]);
            a.Rotate(2);   // a = [3,4,1,2]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Reverse"/>
        <SeeAlso cref="Shift"/>*)
      function Rotate(Offset: integer): TVec; overload;
      function Rotate(const Src: TMtxVec; Offset: integer): TVec; overload;

      (*<summary>Sets vector values (double).</summary>
        
<remarks>Sets the <see cref="TMtxVecBase.Length" text="Length"/> property of the calling vector to (High(A)+1) and
        the <see cref="TMtxVec.Complex">Complex</see> property to AComplex. The double elements of A array are copied to the calling vector.
        If AComplex is True then real parts of complex numbers are on even (0,2,4..)and imaginary parts on odd indexes.(1,3,5,..)

        Note
          Use this method for setting a double array only.
</remarks>


        <SeeAlso cref="SetIt"/>
        <SeeAlso cref="SetInteger"/>
        <SeeAlso cref="SetCplx"/>
        <SeeAlso cref="SetSingle"/>*)
      function SetDouble(AComplex: boolean; const A: array of double): TVec; overload;
      (*<summary>Sets vector values (integer).</summary>
        
<remarks>Sets the <see cref="TMtxVecBase.Length" text="Length"/> property of the calling vector to (High(A)+1) and
        the <see cref="TMtxVec.Complex">Complex</see> property to AComplex. The integer elements of A array are copied to the calling
        vector. If AComplex is True then real parts of complex numbers are on even (0,2,4..)and imaginary parts on
        odd indexes.(1,3,5,..).

        Note
          Use this method for integer array only.
</remarks>


        <SeeAlso cref="SetIt"/>
        <SeeAlso cref="SetDouble"/>
        <SeeAlso cref="SetCplx"/>
        <SeeAlso cref="SetSingle"/>*)
      function SetInteger(AComplex: boolean;const A: array of Integer): TVec; overload;
      (*<summary>Sets vector values (single).</summary>
        
<remarks>Sets the <see cref="TMtxVecBase.Length" text="Length"/> property of the calling vector to (High(A)+1) and
        the <see cref="TMtxVec.Complex">Complex</see> property to AComplex. The single elements of A array
        are copied to the calling vector. If AComplex is True then real parts of complex numbers
        are on even (0,2,4..)and imaginary parts on odd indexes.(1,3,5,..)

        Note
          Use this method for single array only.
</remarks>


        <SeeAlso cref="SetIt"/>
        <SeeAlso cref="SetInteger"/>
        <SeeAlso cref="SetCplx"/>
        <SeeAlso cref="SetDouble"/>*)
      function SetSingle(AComplex: boolean; const A: array of single): TVec; overload;

      (*<summary>Sets vector values.</summary>
        
<remarks>SetIt makes the use of open array construction, to set the values of the calling vector. It gives you the
        possibility to pass large arrays of elements without having to declare constant arrays.

        Sets the <see cref="TMtxVecBase.Length" text="Length"/> property of the calling vector to
        (High(A)+1) and the <see cref="TMtxVec.Complex"/> property to AComplex. The elements of A array are
        copied to the calling vector. If AComplex is True then real parts of complex numbers are on
        even (0,2,4..) and imaginary parts on odd indexes (1,3,5,..).
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.SetIt(False,[1,2,3,4]); // a = [1,2,3,4]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="SetDouble"/>
        <SeeAlso cref="SetInteger"/>
        <SeeAlso cref="SetCplx"/>
        <SeeAlso cref="SetSingle"/>*)
      function SetIt(AComplex: boolean;const A: array of double): TVec; overload;
      (*<summary>Sets vector values (double).</summary>
        
<remarks>Does not change the <see cref="TMtxVecBase.Length" text="Length"/> or <see cref="TMtxVec.Complex">Complex</see> properties of the calling vector,
        but it does check for array overrun. The elements of A array are copied to the calling vector, starting at index 0.
        If the calling vector is complex then real parts of complex numbers are on even (0,2,4..)and imaginary parts on
        odd indexes.(1,3,5,..)
</remarks>
*)
      function SetIt(const A: array of double): TVec; overload;

      (*<summary>Sets vector values (complex).</summary>
        
<remarks>Sets the <see cref="TMtxVecBase.Length" text="Length"/> property of the calling vector to (High(A)+1) and the <see cref="TMtxVec.Complex">Complex</see>
        property to true. The complex elements of A array are copied to the calling vector CValues.

        Note
          Use this method for complex array only.
</remarks>


        <SeeAlso cref="SetDouble"/>
        <SeeAlso cref="SetInteger"/>
        <SeeAlso cref="SetIt"/>
        <SeeAlso cref="SetSingle"/>*)
      function SetCplx(const A: array of TCplx): TVec;

      (*<summary>Defines a sub-vector.</summary>
        
<remarks>Define a subvector of the Src vector starting at BeginIndex and ending at EndIndex (inclusive).
</remarks>


        <SeeAlso cref="TVec.SetSubRange"/>
        <SeeAlso cref="TMtxVecBase.SetFullRange"/>*)
      procedure SetSubIndex(const Src: TMtxVec; BeginIndex, EndIndex: integer); overload;

    


    
      (*<summary>Defines the calling vector to have the view of the same memory as Src.</summary>
         
<remarks>Src vector's Length property may not change while any other object has it's own view of it.

         <code>
         a.SetSubRange(b,..);
         </code>

         This SetSubRange method must be handled with great care. Namely:
         * b can be freed before "a" and accessing "a" gives AV.
         * a can be further subranged with c and same problem occurs when b is freed before c.
         * If you resize b, all objects which have subranged b
          are no longer pointing to valid memory.
         * b can Subrange other objects. Similar problem as
          when changing the size of b.
          Again all objects which have subranged b are not
          longer pointing to valid memory.

         All this can lead to hard to find bugs.
</remarks>


        <SeeAlso cref="TVec.SetSubRange"/>
        <SeeAlso cref="TMtxVecBase.SetFullRange"/>*)
      procedure SetSubRange(const Src: TMtxVec); overload;

      (*<summary>Shift vector elements left or right in the array.</summary>
        
<remarks>Shifts calling vector elements. The number of elements to shift is specified in the Offset parameter.
        Offset can be any integer number, positive or negative.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
            a.SetIt(False,[1,-2,3,0]);
            a.Shift(1); // a = 1,1,-2,3
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Rotate"/>*)
      function Shift(Offset: integer): TVec; overload;

      (*<summary>Shift vector elements left or right in the array.</summary>
        
<remarks>Shifts Src vector elements and stores the result in the calling object.
        The number of elements to shift is specified in the Offset parameter.
        Offset can be any integer number, positive or negative.
</remarks>


        <SeeAlso cref="Rotate"/>*)
      function Shift(const Src: TMtxVec; Offset: integer): TVec; overload;

      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to True.
        FloatPrecision property is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TCplxArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to True.
        FloatPrecision property is preserved.
</remarks>
*)

      procedure SizeFromArray(const Src: TSCplxArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to false.
</remarks>
*)
      procedure SizeFromArray(const Src: TDoubleArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to false.
</remarks>
*)
      procedure SizeFromArray(const Src: TSingleArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to false.
</remarks>
*)
      procedure SizeFromArray(const Src: TIntegerArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to false.
</remarks>
*)
      procedure SizeFromArray(const Src: TSmallIntArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the size (Length) of the caling vector to match the length of the array.
        The <see cref="TMtxVec.Complex">Complex</see> is set to false.
</remarks>
*)
      procedure SizeFromArray(const Src: Math387.TByteArray); overload;

      (*<summary>Sets the size of the vector.</summary>
        
<remarks>Same as the following code:

        <pre>

        if aIsDouble then
        begin
            if aComplex then Size(aLength, mvDoubleComplex)
                        else Size(aLength, mvDouble);
        end else
        begin
            if aComplex then Size(aLength, mvSingleComplex)
                        else Size(aLength, mvSingle);
        end;
        </pre>

        Calling the Size method does not preserve the contents of the vector.
        Use the Resize method, if you want to preserve existing values.
</remarks>
*)
      function Size(ALength: integer; AComplex: boolean; aIsDouble: boolean): TVec ; overload;

      
      (*<summary>Sets the size of the vector.</summary>
        
<remarks>Same as the following code:

        <pre>
        Vec.Length := ALength;
        Vec.Complex := AComplex;
        </pre>

        Calling the Size method does not preserve the contents of the vector.
        Use the Resize method, if you want to preserve existing values.

        Existing floating point precision is preserved.
</remarks>
*)
      function Size(ALength: integer; AComplex: boolean): TVec ; overload;
      (*<summary>Sizes the vector.</summary>
        
<remarks>Sets the following properties:

        <pre>
        Length := ALength;
        Complex := false;
        </pre>

        Existing floating point precision is preserved.

        Calling the Size method does not preserve the contents of the vector.
        Use the Resize method, if you want to preserve existing values.
</remarks>
*)
      function Size(ALength: integer): TVec ; overload;
      

      (*<summary>Sets the size of the vector.</summary>
        
<remarks>Same as the following code:

        <pre>
        Vec.FloatPrecision := aFloatPrecision;
        Vec.Length := ALength;
        </pre>

        Calling the Size method does not preserve the contents of the vector.
        Use the Resize method, if you want to preserve existing values.
</remarks>
*)

      function Size(ALength: integer; const aFloatPrecision: TMtxFloatPrecision): TVec; overload;

      (*<summary>Sets the size of the vector.</summary>
        
<remarks>Same as the following code:

        <pre>
        Vec.FloatPrecision := FloatRef.FloatPrecision;
        Vec.Length := ALength;
        </pre>

        Calling the Size method does not preserve the contents of the vector.
        Use the Resize method, if you want to preserve existing values.
</remarks>
*)

      function Size(ALength: integer; const FloatRef: TMtxVec): TVec; overload;

      (*<summary>Sets vector size.</summary>
                 
<remarks>Length is set to Src.Length and Complex property to AComplex.
                 Floating point precision is obtained from Src. If Src is integer type (TVecInt, TMtxInt), the existing
                 floating point precision is preserved.
</remarks>
*)
      function Size(const Src: TMtxVecBase; AComplex: boolean): TMtxVec ; override;


      (*<summary>Sets vector size.</summary>
                 
<remarks>Length is set to Src.Length and precision is set to aFloatPrecision.
</remarks>
*)
      function Size(const Src: TMtxVecBase; const aFloatPrecision: TMtxFloatPrecision): TMtxVec; override;

      (*<summary>Sorts vector elements in ascending order.</summary>
        
<remarks>Sort all calling vector elements.

        Note
          If the vector is complex, the complex values are first compared by the absolute value and
          then (if absolute value is equal) by the argument.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          a.SetIt(True,[2,1,3,4]);
          a.SortAscend; // a = [1,2,3,4]
          FreeIt(a);
        end;
        </code>
        </Example>

        <SeeAlso cref="SortDescend"/>*)
      function SortAscend: TVec; overload;
      (*<summary>Sort calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is True and array
        borders are overrun.
</remarks>
*)
      function SortAscend(Index,Len: integer): TVec; overload;
      (*<summary>Sort calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is True and array
        borders are overrun.

        IndexVec contains the new order of elements. The order is stored in an array of integers
        which are accessible via the IValues property.
</remarks>
*)
      function SortAscend(Index,Len: integer; const IndexVec: TVecInt): TVec; overload;
      (*<summary>Sort all calling vector elements.</summary>
        
<remarks>IndexVec contains the new (zero based) order of elements. The order is stored in
        an array of integers which are accessible via the IValues property.
</remarks>
*)
      function SortAscend(const IndexVec: TVecInt): TVec; overload;

      (*<summary>Sorts vector elements in descending order.</summary>
        
<remarks>Sort all calling vector elements in descending order.

        Note
          If the vector is complex, the complex values are first compared by the absolute value and
          then by the argument.
</remarks>


        <Example>
        <code>
        var a: TVec;
        begin
          CreateIt(a);
          try
              a.SetIt(True,[2,1,3,4]);
              a.SortDescend; // a = [4,3,2,1]
          finally
              FreeIt(a);
          end;
        </code>
        </Example>

        <SeeAlso cref="SortAscend"/>*)
      function SortDescend: TVec; overload;
      (*<summary>Sort calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is True and array
        borders are overrun.
</remarks>
*)
      function SortDescend(Index,Len: integer): TVec; overload;
      (*<summary>Sort calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is True and array
        borders are overrun.

        IndexVec contains the new order of elements.
</remarks>
*)
      function SortDescend(Index,Len: integer; const IndexVec: TVecInt): TVec; overload;
      (*<summary>Sort all calling vector elements. IndexVec contains contains the new order of elements.</summary>*)
      function SortDescend(const IndexVec: TVecInt): TVec; overload;

      (*<summary>Splits the calling vector to an array of TVec objects.</summary>
        
<remarks>Copy the contents of the calling TVec object to the TVec objects in Dst array.
        The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of vectors in
        Dst array must be set explicitly. An exception is raised, if <see cref="TMtxVec.Complex">Complex</see> properties
        of TVec objects do not match or the sum of TVec lengths exceeds the
        <see cref="TMtxVecBase.Length" text="Length"/> of the calling vector.
</remarks>


        <Example>
        <code>
        var a,b,c,d: TVec;
        begin
          CreateIt(a,b,c,d);
          try
            a.SetIt(False,[0,0,0,0,1,2,3,1,2,3]);
            a.Split([b,c,d],[4,3,3]);
            // b = [0,0,0,0], c = [1,2,3], d = [1,2,3]
          finally
            FreeIt(a,b,c,d);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Concat"/>
        <SeeAlso cref="Copy"/>
        <SeeAlso cref="Resize"/>*)
      function Split(const Dst: array of TVec): TVec; overload;
      (*<summary>Copy the contents of the calling TVec object to the TVec objects in Dst array.</summary>
        
<remarks>The Length of objects in Dst array is defined in the DstSize array. The <see cref="TMtxVecBase.Length" text="Length"/> and
        <see cref="TMtxVec.Complex"/>  properties of vectors in Dst array are set implicitly. An exception is raised
        if the sum of DstSize lengths exceeds the <see cref="TMtxVecBase.Length" text="Length"/> of the calling vector.
</remarks>
*)
      procedure Split(const Dst: array of TVec;const DstSize: array of integer); overload;
      (*<summary>Copy the contents of the calling TVec object to Vec1 and Vec2 objects.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of are set implicitly. An exception is
        raised, if Offset parameter exceeds the length of the calling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
        property of Vec1 object is set to
        Offset and the Length of Vec2 is set to the rest.
</remarks>
*)
      procedure Split(const Vec1: TVec; Offset: integer; const Vec2: TVec); overload;


      (*<summary>Convert strings in aList to double or TCplx.</summary>
        
<remarks>Convert strings in aList to double.
        (<see cref="Cplx"/>) and store them in the Values array of the calling vector. If AComplex is True, then the
        <see cref="TMtxVec.Complex">Complex</see> property is set to True and strings are converted by using the
        <see cref="Math387.StrToCplx"/> function. The <see cref="TMtxVecBase.Length" text="Length"/> of the calling vector is set to
        aList.Count and the complex property is auto-detected by looking at the first string.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(False,[1,2,3,4]);
            a.cos;
            b.Size(a);
            b.SetVal(1);
            a.Add(b);
            Richedit1.Clear;
            Memo1.Clear;
            a.ValuesToStrings(Richedit1.Lines);
            b.ValuesToStrings(Richedit1.Lines);
            a.ValuesToStrings(Memo1.Lines);
            b.ValuesToStrings(Memo1.Lines);
            Memo1.Lines.SaveToFile('C:\Test.txt');
            Memo1.Lines.LoadFromFile('C:\Test.txt');
            a.StringsToValues(Memo1.Lines);
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="ValuesToStrings"/>*)
      procedure StringsToValues(const srcList: TStrings); overload;
      (*<summary>Convert strings in aList to double (TCplx) starting at ListIndex.</summary>
        
<remarks>Store them in the Values array of the calling vector from Index to Index+Len-1. If strings represent complex numbers,
        then the complex property must be True or an exception is raised. The Length property of the calling vector is not changed.
        If array bounds are overrun and exception is raised.
</remarks>
*)
      function StringsToValues(const srcList: TStrings; ListIndex: integer; Index: integer = 0; Len: integer = MtxVecEOA): TVec; overload;

      (*<summary>Copies only those values from Src, which are not NAN or INF.</summary>*)
      function StripNanAndInf(const Src: TMtxVec): TMtxVec; overload;

      (*<summary>Copies only those values from Src[SrcIndex.. SrcIndex+Len-1], which are not NAN or INF.</summary>
        
<remarks>The length of the Self is set to match the count of matching values.
</remarks>
*)
      function StripNanAndInf(const Src: TMtxVec; SrcIndex, Len: integer): TMtxVec; overload;
      (*<summary>Copies only those values from Src[SrcIndex.. SrcIndex+Len-1], which are not NAN or INF.</summary>
        
<remarks>The length of the Self is not modified. The funtion returns "Index" increased by Number of elements found.
        The calling vector must have enough space to hold at least Index+Len elements, or an exception will be raised.
</remarks>
*)
      function StripNanAndInf(const Src: TMtxVec; SrcIndex, Index, Len: integer): integer; overload;

      (*<summary>Tensor product between vector and matrix.</summary>
        
<remarks>Calculates the right tensor product between matrix and vector. The result is placed in the calling vector. Depending on the
        <see cref="TMtxType"/> the following operations are available:

        * mtSymmetric:	<c>y = alfa*a*x + beta*y </c>
        * mtSymmPosDef:	<c>y = alfa*a*x + beta*y </c>
        * mtHermitian:       	<c>y = alfa*a*x + beta*y</c>
        * mtHermPosDef:	<c>y = alfa*a*x + beta*y </c>
        * mtTriangular:       	<c>y = op(a)*x </c>
        * mtGeneral:         	<c>y = alfa*op(a)*x + beta*y</c>

        The <see cref="TMtxVec.Alfa"/> and <see cref="TMtxVec.Beta"/> are TVec complex public variables. Their default values are:

        * Alfa = Cplx(1,0)
        * Beta = Cplx(0,0).

        Note
          Each time you call TensorProd the values of Alfa and Beta are reset to default. If the matrix is not
          complex, only the real part of Alfa and Beta is used. If matrix complex and symmetric the general type is used.
</remarks>


      <Example>
      <code>
      var a,b,c,t: TMtx;
            d,e,f: TVec;
      begin
        CreateIt(a,b,c,t);
        CreateIt(d,e,f);
        try
          // Test non quadratic general matrix
          a.SetIt(2,3,False,[4,3,3,
                             3,4,2]);
          e.SetIt(false,[1,2]);
          d.TensorProd(e,a);

          f.SetIt(False,[10,11,7]);
          if not f.Equal(d) then raise Exception.Create('Not same');

          // Test on triangular matrices, left
          a.TriangleForm := tfUpper;
          a.TriangleUnit := False;

          a.SetIt(2,2,False,[4,3,
                             0,4]);
          e.SetIt(false,[1,2]);
          d.TensorProd(e,a,mtTriangle);

          f.SetIt(False,[4,11]);
          if not f.Equal(d) then raise Exception.Create('Not same');

          // Test on triangular matrices, right

          a.TriangleForm := tfUpper; // data to be referenced is in upper triangle
          a.TriangleUnit := False;  // non unit diagonal
          a.SetIt(2,2,False,[4,3,
                            0,4]);
          e.SetIt(false,[1,2]);
          d.TensorProd(a,e,mtTriangle);

          f.SetIt(False,[10,8]);
          if not f.Equal(d) then raise Exception.Create('Not same');

          // Test on symmetric matrices, right

          a.TriangleForm := tfUpper;
          a.SetIt(2,2,False,[4,3,
                            3,4]);
          e.SetIt(false,[1,2]);
          d.TensorProd(e,a,mtSymmetric);

          f.SetIt(False,[10,11]);
          if not f.Equal(d) then raise Exception.Create('Not same');

          // Test on symmetric matrices, Left

          a.TriangleForm := tfUpper;
          a.SetIt(2,2,False,[4,3,
                             3,4]);
          e.SetIt(false,[1,2]);
          d.TensorProd(a,e,mtSymmetric);

          f.SetIt(False,[10,11]);
          if not f.Equal(d) then raise Exception.Create('Not same');
        finally
          FreeIt(a,b,c,t);
          FreeIt(d,e,f);
        end;
      end;
      </code>
      </Example>

        <SeeAlso cref="TMtx.TensorProd"/>*)
      function TensorProd(const Mtx: TMtx; const Vec: TVec; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone): TVec; overload;
      (*<summary>Calculates the left tensor product between vector and matrix.</summary>*)
      function TensorProd(const Vec: TVec; const Mtx: TMtx; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone): TVec; overload;

      (*<summary>Calculates the right tensor product between vector and matrix and adds the result to the calling vector.</summary>*)
      function AddTensorProd(const Mtx: TMtx; const Vec: TVec; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone): TVec; overload;

      (*<summary>Inserts zeroes between consecutive vector values.</summary>
        
<remarks>Inserts zeroes between consecutive vector values. The method copies the values from Src to the calling vector and
        places Factor-1 zeros between consecutive values. The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly. Phase parameter defines the initial sample offset and must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            b.SetIt(False,[0,0,1,3,2]);
            a.UpSample(b,2); // a = [0,0,0,0,1,0,3,0,2,0]
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="PixelDownSample"/>
        <SeeAlso cref="DownSample"/>
        <SeeAlso cref="TDenseMtxVec.DownSample"/>
        <SeeAlso cref="TDenseMtxVec.UpSample"/>*)
      function UpSample(const Src: TMtxVec; Factor: integer; Phase: integer = 0): TVec; overload;

      (*<summary>Converts the content of the Values array of the calling vector to a list of strings.</summary>
        
<remarks>Converts all elements of the calling vector to strings with formating ReFormat for the real part and ImFormat for the imaginary part
        and stores them in dstList, by using the Add method of TStringsobject. If vector is not complex only the ReFormat is used.

        Note
          Complex numbers are formated by default as: a+bi. You can have any formating by specifying the ReFormat and ImFormat
          parameters, but in order for the <see cref="StringsToValues"/> to work, the basic a+bi formating must be preserved -
          (chars '+' (' - ') and 'i' ).

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


        <Example>
        <code>
        procedure TForm1.Button1Click(Sender: TObject);
        var a,b: TVec;
        begin
          CreateIt(a,b);
          try
            a.SetIt(False,[1,2,3,4]);
            a.Cos;
            b.Size(a);
            b.SetVal(1);
            a.Add(b);
            Richedit1.Clear;
            Memo1.Clear;
            a.ValuesToStrings(Richedit1.Lines);
            b.ValuesToStrings(Richedit1.Lines);
            a.ValuesToStrings(Memo1.Lines);
            b.ValuesToStrings(Memo1.Lines);
            Memo1.Lines.SaveToFile('C:\Test.txt');
            Memo1.Lines.LoadFromFile('C:\Test.txt');
            a.StringsToValues(Memo1.Lines);
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="StringsToValues"/>*)
      function ValuesToStrings(const dstList: TStrings; const Align: TFixedTextAlign = ftaNone;
                                const ReFormat: string = ' 0.#####;-0.#####';
                                const ImFormat: string = '+0.#####i;-0.#####i';
                                const Headers: boolean = false): integer; overload;
      (*<summary>Convert elements from Index to Index+Len-1 of the calling vector to strings.</summary>
        
<remarks>Use ReFormat for the real part and ImFormat for the imaginary part and store them in dstList starting at ListIndex. If dstList is not large enough,
        the method will use the add method of dstList object. If vector is not complex, only the ReFormat is used.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
      function ValuesToStrings(const dstList: TStrings; ListIndex, Index,  Len: integer;
                                const Align: TFixedTextAlign = ftaNone;
                                const ReFormat: string = ' 0.#####;-0.#####';
                                const ImFormat: string = '+0.#####i;-0.#####i';
                                const Headers: boolean = false): integer; overload;

      (*<summary>Convert all vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;
      (*<summary>Convert Index..Index+Len-1 vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String; Index,  Len: integer; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;

      (*<summary>Transforms vector into vector of standardized data.</summary>*)
      function ZScore: TVec; overload;
      (*<summary>Transforms Src vector into vector of standardized data.</summary>
        
<remarks>Store the results in calling vector.
</remarks>
*)
      function ZScore(const Src: TVec): TVec; overload;

      (*<summary>Returns content as a string.</summary>
                 
<remarks>Values will be separated by line breaks.
</remarks>
*)
      function ToString: string; override;
      (*<summary>Parses string content as vector.</summary>
                 
<remarks>Values need to be separated by line breaks.
                 For other parsing options call StringToValues.
                 The Parse method works in pair with the ToString method both meant to be a reverse of the other,
                 but do not preserve any other property of the vector except only values.
                 Use SaveToStream/LoadFromStream methods for binary storage option, which uses up to 10x less memory and is a lot faster to save and load.
</remarks>
*)
      function Parse(const Src: string): TVec;

      
    end;

(*<summary>Defines the norm, used in calculating the condition number parameter.</summary>
  
<remarks>Defines the norm, used in calculating the <see cref="TMtx.ConditionNr"/>.
</remarks>
*)
TConditionNumber = (
  (*<summary>the condition number will not be calculated</summary>*)cnNone,
  (*<summary>the norm-1 will be used to calculate the condition number</summary>*)cnNorm1,
  (*<summary>the infinite norm will be used to calculate the condition number</summary>*)cnNormInf
  );

(*<summary>Defines the matrix triangular type.</summary>*)
TTriangleForm = (
  (*<summary>lower triangular matrix</summary>*)tfLower,
  (*<summary>upper triangular matrix</summary>*)tfUpper
  );

(*<summary>Defines the method, used to balance the matrix.</summary>*)
TBalanceType = (
(*<summary>matrix is neither scaled or permuted</summary>*)btNone,
(*<summary>matrix is scaled, but not permuted</summary>*)btScale,
(*<summary>matrix is permuted, but not scaled</summary>*)btPerm,
(*<summary>matrix is permuted <b>AND</b> scaled</summary>*)btFull
);

(*<summary>Defines the type of the symmetric-definite generalized eigenvalue problem.</summary>*)
TEigGenType = (
(*<summary>The problem is A*x = lambda*B*x, and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)</summary>*)
etAzBz,
(*<summary>The problem is A*B*x = lambda*x and A is overwritten by U*A*U**T or L**T*A*L</summary>*)
etABz,
(*<summary>The problem is B*A*x = lambda*x and A is overwritten by U*A*U**T or L**T*A*L</summary>*)
etBAz
);

(*<summary>Defines the shape of the Sylvester equation.</summary>*)
TSign = (
(*<summary>the + sign in the Sylvester equation</summary>*)siPlus,
(*<summary>the - sign in the Sylvester equation</summary>*)siMinus
);

(*<summary>Defines the vector form in eigenvector calculation.</summary>*)
TVectorForm = (
(*<summary></summary>*)vfEig,
(*<summary></summary>*)vfSchur);

(*<summary>Defines the matrix function.</summary>
 
<remarks>Defines the matrix function. This function prototype is used
 to define a arbitrary "matrix" function. The parameters are passed
 as Params and the function is to be applied to the elements of the
 D vector.
</remarks>

 *)
TMtxFunction = procedure(D: TVec; const Params: array of TCplx);




  (*<summary>Two dimensional array - matrix.</summary>
   
<remarks>Use the matrix class to perform matix operations like multiplication
   addition, subtraction, solve a system of linear equations, compute
   eigenvalues, singular value decomposition, least squares solution,
   2D FFT's, parallel 1D FFT's and more.
</remarks>
*)
 
 
 
  TMtx = class(TDenseMtxVec   )
  strict private
    FMtxError: string;
    FRefineSolution: boolean;
    FBackError: double;
    FForwError: double;
    FConditionNumber: TConditionNumber;
    FConditionNr: double;
    FTriangleForm: TTriangleForm;
    FTriangleUnit: boolean;
    FBalance: TBalanceType;
    FAutoMtxType: boolean;
    FSuperDiag: integer;
    FSubDiag: integer;
    SRows,SCols: integer;
    SLeadingCols: integer;
    procedure SetRows(const Value: integer);
    procedure SetRefineSolution(const Value: boolean);
    procedure SetConditionNumber(const Value: TConditionNumber);
    procedure SetTriangleForm(const Value: TTriangleForm);
    procedure SetTriangleUnit(const Value: boolean);
    procedure SetBalance(const Value: TBalanceType);
    procedure SetAutoMtxType(const Value: boolean);
    procedure SetCols(const Value: integer);
    procedure SVDSolveInt(var B: TDoubleArray; BIndex: integer;
                          var BC: TCplxDoubleArray; BCIndex, nrhs: integer;
                          const S: TVec; Threshold: Double; var Rank: integer); overload;
    procedure SVDSolveInt(var B: TSingleArray; BIndex: integer;
                          var BC: TCplxSingleArray; BCIndex, nrhs: integer;
                          const S: TVec; Threshold: single; var Rank: integer); overload;
    procedure SetSubDiag(const Value: integer);
    procedure SetSuperDiag(const Value: integer);
    procedure SetLeadingCols(const Value: integer);
    function GetLeadingCols: integer; 
    procedure ValidateRange (Row, Col, ToRow, ToCol: integer);
  

  strict protected
    
    fQuadratic: boolean;
    finfo: integer;
    FRows: integer;
    FCols: integer;
    FLeadingCols: integer;
    jitter: TSmallMatrixMultiply;
    function GetComplexValues: string; override;
    function GetRealValues: string; override;

    procedure SubRangeSaveState; override;
    procedure SubRangeLoadState; override;

    procedure SetSubRangeDirect(Index: Integer; Len: Integer); override;
    procedure SetSubRangeDirect(const Src: TMtxVecBase; Index: Integer; Len: Integer); override;
    procedure SetSubRangeDirect2(const Src: TMtxVecBase; Index: Integer; aRows, aCols: Integer);

    procedure LQRSolveInt(var B: TDoubleArray; BIndex: integer;
                          var BC: TCplxDoubleArray; BCIndex: integer;
                          BCols: integer; const R: TMtx; Op: TMtxOperation = opNone); overload;
    procedure LQRSolveInt(var B: TDoubleArray; BIndex: integer;
                          var BC: TCplxDoubleArray; BCIndex: integer;
                          BCols: integer; rcond: double; var rank: integer; const R: TMtx; Op: TMtxOperation = opNone); overload;

    procedure LQRSolveInt(var B: TSingleArray; BIndex: integer;
                          var BC: TCplxSingleArray; BCIndex: integer;
                          BCols: integer; const R: TMtx; Op: TMtxOperation = opNone); overload;
    procedure LQRSolveInt(var B: TSingleArray; BIndex: integer;
                          var BC: TCplxSingleArray; BCIndex: integer;
                          BCols: integer; rcond: single; var rank: integer; const R: TMtx; Op: TMtxOperation = opNone); overload;

    procedure LUSolveInt(var B: TDoubleArray; BIndex: integer;
                          var BC: TCplxDoubleArray; BCIndex: integer;
                          var X: TDoubleArray; XIndex: integer;
                          var XC: TCplxDoubleArray; XCIndex: integer; nrhs: integer;
                          MtxType: TMtxType; Operation: TMtxOperation;
                          const Mtx, OrigMtx: TMtx; const Pipiv: TVecInt); overload;

    procedure LUSolveInt(var B: TSingleArray; BIndex: integer;
                          var BC: TCplxSingleArray; BCIndex: integer;
                          var X: TSingleArray; XIndex: integer;
                          var XC: TCplxSingleArray; XCIndex: integer; nrhs: integer;
                          MtxType: TMtxType; Operation: TMtxOperation;
                          const Mtx, OrigMtx: TMtx; const Pipiv: TVecInt); overload;

    function EigDoubleInt(const VL: TMtx; const D: TVec; const VR: TMTx = nil; MtxType: TMTxType = mtGeneral; VectorForm: TVectorForm = vfEig; aExpand: boolean = True): TMtx; overload;
    function EigSingleInt(const VL: TMtx; const D: TVec; const VR: TMTx = nil; MtxType: TMTxType = mtGeneral; VectorForm: TVectorForm = vfEig; aExpand: boolean = True): TMtx; overload;

    procedure DeterminantDoubleInt(out result: TCplx; MtxType: TMtxType = mtGeneral); overload;
    procedure DeterminantSingleInt(out result: TCplx; MtxType: TMtxType = mtGeneral); overload;

    function LowerTriangleInternal(const Mtx: TMtx; ZeroUpper, Diagonal: boolean): TMtx; overload;
    function UpperTriangleInternal(const Mtx: TMtx; ZeroLower, Diagonal: boolean): TMtx; overload;
    procedure DiagonalsCount(out Upper, Lower: integer);
    procedure DoSetComplex; override;

    function DoubleCachePointer: TDoubleArray; override;
    function DoubleCacheOffset: integer; override;
    function CplxDoubleCachePointer: TCplxDoubleArray; override;
    function CplxDoubleCacheOffset: integer; override;
    function SingleCachePointer: TSingleArray; override;
    function SingleCacheOffset: integer; override;
    function CplxSingleCachePointer: TCplxSingleArray; override;
    function CplxSingleCacheOffset: integer; override;
    function CacheLength: integer; override;
    function CacheLengthInBytes: Int64; override;
    procedure HookPointers; override;
    procedure SetLength(const Value: integer); override;

    function ValuesToStringsInternal(const dstList: TStrings; const aDelimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload; override;
    function StringsToValuesInternal(const srcList: TStrings; const aDelimiter: string): TMtxVecBase; overload; override;
    

  
  strict protected
  
    

    
    function get_CValues(const RowIdx, ColIdx: integer): TCplx; 
    function get_Values(const RowIdx, ColIdx: integer): double; 
    procedure set_CValues(const RowIdx, ColIdx: integer; const Value: TCplx); 
    procedure set_Values(const RowIdx, ColIdx: integer; const Value: double); 

    function get_SCValues(const RowIdx, ColIdx: integer): TSCplx; 
    function get_SValues(const RowIdx, ColIdx: integer): single; 
    procedure set_SCValues(const RowIdx, ColIdx: integer; const Value: TSCplx); 
    procedure set_SValues(const RowIdx, ColIdx: integer; const Value: single); 
    

    
    
    function  get_DefaultArray(const Indx1, Indx2: integer): double; 
    procedure set_DefaultArray(const Indx1, Indx2: integer; const value: double); 
    
 public
       function ToolTip: string;
      

    (*<summary>Returns the 1D index in to the matrix.</summary>
               
<remarks>Returns: Row*Mtx.Cols + Col
</remarks>
*)
      function IndexOf(const Row, Col: integer): integer; overload; 

      (*<summary>Returns content as a string.</summary>
                 
<remarks>Values will be separated by line breaks.
</remarks>
*)
      function ToString: string; override;
      (*<summary>Parses string content as vector.</summary>
                 
<remarks>Values need to be separated by the tab charachter.
                 Rows of values need to be separated by line breaks.
                 For other parsing options call StringToValues.
                 The Parse method works in pair with the ToString method both meant to be a reverse of the other,
                 but do not preserve any other property of the matrix except only values.
                 Use SaveToStream/LoadFromStream methods for binary storage option, which uses up to 10x less memory and is a lot faster to save and load.
</remarks>
*)
      function Parse(const Src: string): TMtx;

    (*<summary>Stores the condition number reciprocial value after the call to LUSolve.</summary>
      
<remarks>The property is set by the <see cref="LUSolve"/> method. It contains the calling matrix condition number reciprocial value after
      the call to <see cref="LUSolve"/> method and if <see cref="ConditionNumber"/> property was not cnNone. If the ConditionNr
      (inverse of condition number) is a very small number (thus the condition number is very large), then the calling matrix is
      ill-conditioned and the error in the solution will also be large. The condition number is used for analyzing the errors in the
      solution of a system of linear equations.

      In practice, most computations are performed with rounding errors. Besides, you often need to solve a system <c>Ax = b</c> where the data
      (the elements of A and b) are not known exactly. Therefore, it's important to understand how the data errors and rounding errors
      can affect the solution x. If x is the exact solution of Ax = b, and x + dx is the exact solution of a perturbed problem
      <c>(A + dA)x = (b + db)</c>, then

      <IMG name="mtx003"/><para/>

      In other words, relative errors in A or b may be amplified in the solution vector x by a factor <c>k(A) = ||A|| ||A -1 ||</c> called
      the condition number of A. The norm used to calculate the condition number is set by the <see cref="ConditionNumber"/> property.

      Rounding errors have the same effect as relative perturbations c(n)e in the original data. Here e is the machine precision,
      and c(n) is a modest function of the matrix order n. The corresponding solution error is <c>||dx||/||x|| &lt;= c(n)k(A)e.</c> (The value of
      c(n) is seldom greater than 10n), Thus, if your matrix A is ill-conditioned (that is, its condition number k(A) is very large),
      then the error in the solution x is also large; you may even encounter a complete loss of precision. This loss can be greatly reduced
      by enabling the <see cref="RefineSolution"/> property.
</remarks>


      <Example>
      <code>
      var X,B: TVec;
        A: TMtx;
      begin
        CreateIt(X,B);
        CreateIt(A);
        try
          B.SetIt(False,[0,2]);
          A.SetIt(2,2,false,[1,2,
                             3,4]);   // 2x2, not complex matrix
          A.RefineSolution := True;
          A.ConditionNumber := cnNormInf;
          A.LUSolve(B,X);
        finally
          FreeIt(B,X);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

        <SeeAlso cref="LUSolve"/>
        <SeeAlso cref="ConditionNumber"/>
        <SeeAlso cref="RefineSolution"/>*)
    property  ConditionNr: double read FConditionNr;
    (*<summary>The Info parameter as returned by the last called LAPACK function.</summary>*)
    property  Info: integer read FInfo;
    (*<summary>The type of the error condition signaled by LAPACK.</summary>
      
<remarks>The string is set by the various methods, to indicate the type of the error condition signaled by
      LAPACK if <see cref="Info"/> property is not zero.
</remarks>


      <SeeAlso cref="Info"/>*)
    property  MtxError: string read FMtxError;
    (*<summary>True, if matrix is quadratic (rows=cols).</summary>*)
    property  Quadratic: boolean read FQuadratic;
    (*<summary>The component-wise backward error.</summary>
      
<remarks>Set by the <see cref="LUSolve"/> method, if the <see cref="RefineSolution"/> property was True.
      The BackError property returns the component-wise backward error b. The backward error is the smallest
      relative perturbation in elements of A (where A is the calling matrix) and b such that x is the exact
      solution of the perturbed system:

      <IMG name="mtx004"/><para/>

      The BackError property is calculated only if the <see cref="RefineSolution"/> property is set to true.
</remarks>


      <SeeAlso cref="ForwError"/>
      <SeeAlso cref="RefineSolution"/>
      <SeeAlso cref="LUSolve"/>*)
    property BackError: double read FBackError;
    (*<summary>The component-wise forward error.</summary>
      
<remarks>Set by the <see cref="LUSolve"/> method, if the <see cref="RefineSolution"/> property was True.
      The ForwError property returns the component-wise forward error in the computed solution:

      <IMG name="mtx005"/><para/>
</remarks>


      <SeeAlso cref="BackError"/>
      <SeeAlso cref="RefineSolution"/>
      <SeeAlso cref="LUSolve"/>*)
    property ForwError: double read FForwError;
    (*<summary>Access elements of the matrix without specifying the name of the property.</summary>
      
<remarks>This property allows you to access elements of the matrix without specifying the name of the property.
      For example:

      <code>
      amtx[i,j] := 1; // where amtx is a TMtx object
      </code><para/>.

      Note
        Default array property is slower than array
        pointers. The default array property supports only real values.
</remarks>
*)
    property DefaultArray2D[const Indx1, Indx2: integer]: double read get_DefaultArray write set_DefaultArray; default; 

    

    {$WARNINGS OFF}
    (*<summary>Access elements of the matrix.</summary>
      
<remarks>The RowIdx indicates the row index and the ColIdx parameter indiciates the column index.
</remarks>
*)
    property Values[const RowIdx, ColIdx: integer]: double read get_Values write set_Values;
    property SValues[const RowIdx, ColIdx: integer]: single read get_SValues write set_SValues;
    (*<summary>Access elements of the matrix.</summary>
      
<remarks>The RowIdx indicates the row index and the ColIdx parameter indiciates the column index.
</remarks>
*)
    {$WARNINGS OFF}
    property CValues[const RowIdx, ColIdx: integer]: TCplx read get_CValues write set_CValues;
    property SCValues[const RowIdx, ColIdx: integer]: TSCplx read get_SCValues write set_SCValues;
    {$WARNINGS ON}




    


    (*<summary>Create a new TMtx object.</summary>
      
<remarks>Creates a new TMtx object. You should use the <see cref="MtxVec.CreateIt"/> where possible and avoid using the
      Create constructor, but only in cases when the object is created and destroy within the same routine.
</remarks>


      <Example>
      <code>
      var A: TMtx;
      begin
        A := TMtx.Create;
        try
          A.Size(20,20,false);
        finally
          A.Free;  // takes about 500 CPU cycles
        end;

        CreateIt(A);
        try
          A.Size(20,20,false);
        finally
          FreeIt(A); // takes only about 40 CPU cycles
        end;
      end;
      </code>
      </Example>*)
    constructor Create; overload; override;
    destructor Destroy; override;

    (*<summary>Obtain a pointer to a matrix from object cache.</summary>
      
<remarks>Returns a pointer to a matrix from object cache. Same as calling
      the CreateIt routine.
</remarks>
*)
     class  function CreateFromCache: TMtx; 
    
    procedure FreeToCache; override;
    

    (*<summary>Returns a pointer to the double value stored at Row and Col.</summary>*)
    function PValues(const Row, Col: integer): PPDouble; overload; 
    (*<summary>Returns a pointer to the single value stored at Row and Col.</summary>*)
    function PSValues(const Row, Col: integer): PPSingle; overload;     
    function SDataIndex(const Row,Col: integer): integer; overload; 
    (*<summary>Returns a pointer to the double precision complex value stored at Row and Col.</summary>*)
    function PCValues(const Row, Col: integer): PPCplx; overload; 
    (*<summary>Returns a pointer to the single precision complex value stored at Row and Col.</summary>*)
    function PSCValues(const Row, Col: integer): PPSCplx; overload; 
    function CDataIndex(const Row,Col: integer): integer; overload; 

     (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index + (aRows*aCols)-1. No copying will occur, only
      pointers will be shifted or indices adjusted. The subranged matrix will have the size aRows and aCols.
      Length will be equal to aRows*aCols.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised if an attempt is made to change the size of calling object.

      A sub-vector/matrix is vector/matrix, which does not have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      To again obtain a view of the full vector/matrix, see <see cref="TMtxVecBase.SetFullRange"/>

      The routine only makes sense to be used where aCols matches Src.Cols. MtxVec has only
      limited support for matrices where the matrix rows are not stored strictly consecutively.
      This overload is to be used with great caution.

      The routine should not be mixed with other SetSubRange routines or subrange stack.
</remarks>
*)

    procedure SetSubRange(const Src: TMtxVec; Index: integer; aRows, aCols: integer); overload;


    
    (*<summary>Adopts a pointer to one dimensional array.</summary>
      
<remarks>Addopts a pointer to AArray array. The method sets the calling matrix <see cref="TMtxVec.Complex" text="FlaotPrecision"/> property to aFloatPrecision,
      <see cref="Rows"/> to ARows, <see cref="Cols"/> to ACols and Values1D and CValues1D to Pointer(AArray).

      Notes:
        * Required to call the <see cref="Disown"/> method, before freeing the object
        * do not resize the vector
        * do not call any routines relying on Lapack when "Core edition" is used (not linking dlls), because
          Lapack will assume that adopted memory is a dynamic array and will modify array index -2.
</remarks>


      <SeeAlso cref="Disown"/>*)
    procedure Adopt(AArray: PAPointer; ARows, ACols: integer; aIsComplex: boolean; aIsDouble: boolean); overload;
    procedure Adopt(AArray: PAPointer; ARows, ACols: integer; aFloatPrecision: TMtxFloatPrecision); overload;
    

    (*<summary></summary>*)
    procedure Assign(Src: TPersistent); override;

    (*<summary>Calculates the tensor product of two vectors and adds the result to calling matrix.</summary>
      
<remarks>Calculates the tensor product of Vec1 and Vec2 vectors and adds the result  to the calling matrix by
      using the following formula:

      <code>
      Y=Vec1 x Vec2 + Y.
      </code><para/>
      If the ConjVec2 parameter is true, then the result will be tensor product of Vec1 and conjugated Vec2.
      The <see cref="Rows"/> property is set to Vec1. <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="Cols"/>
      property is set to Vec2. <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> property of
      the calling matrix is adjusted automatically.
</remarks>


      <Example>
      <code>
      var a,b: TMtx;
          v1,v2: TVec;
      begin
        CreateIt(a,b);
        CreateIt(v1,v2);
        try
          // Test non quadratic general matrix

          a.SetIt(2,2,False,[0,2,2,0]);
          v1.SetIt(false,[1,0]);
          v2.setIt(false,[0,1]);
          a.AddTensorProd(v1,v2,False);
          b.SetIt(False,[1,2,2,1]);
          if not a.Equal(b) then raise Exception.Create('Not same');
        finally
          FreeIt(a,b);
          FreeIt(v1,v2);
        end;
      end;
      </code>
      </Example>
      <SeeAlso cref="TensorProd"/>*)
    function AddTensorProd(const Vec1, Vec2: TVec; ConjVec2: boolean = False): TMtx; overload;

    (*<summary>Adjungate matrix.</summary>
      
<remarks>Calculate the adjungate matrix in-place. Adjungation is equal to transpose and conjugate.
</remarks>
*)
    function Adjung: TMtx; overload;
    (*<summary>Calculate the adjungate to X matrix and store the results in calling matrix.</summary>
      
<remarks>The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are
      set implicitly to match those of X matrix.
</remarks>
*)
    function Adjung(const X: TMtx): TMtx; overload;

    (*<summary>Convert banded matrix to dense.</summary>
      <SeeAlso cref="DenseToBanded"/>*)
    procedure BandedToDense(const Src: TMtx); overload;

    
    procedure Clear; override;
    

    (*<summary>Test if the matrix is semi positive definite.</summary>
      
<remarks>Returns True, if the matrix semi positive definite and at the same time performs
      the Cholesky decomposition of the calling matrix. If the TriangleForm is set to tfLower then
      the upper part of the matrix is overwritten with transpose of L, where L is the result of factorization.
      If the TriangleForm is set to ftUpper then the lower triangle of the matrix is overwritten with
      the transpose of U, where U the result of factorization.
</remarks>
*)
    function Cholesky: boolean; overload;

    (*<summary>Exchange matrix columns.</summary>
      
<remarks>Exchange the i-tj and j-th columns of the calling matrix in-place. An exception is raised if matrix bounds
      are overrun.
</remarks>

      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="ColPermute"/>
      <SeeAlso cref="RowPermute"/>*)
    function ColExchange(i, j: integer): TMtx; overload;

    (*<summary>Permute the columns of the Src matrix.</summary>
      
<remarks>The parameter PermuteIdx contains indexes of columns P[i] to which column at index "i" is to be moved.
      The result of the permutation is stored in the calling matrix. Only entire matrix can be copied.
</remarks>


      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="RowPermute"/>*)

    function ColPermute(const Src: TMtx; const PermuteIdx: TVecInt): TMtx; overload;

    (*<summary>Permute the rows of the Src matrix.</summary>
      
<remarks>The parameter PermuteIdx contains indexes of columns P[i] to which column at index "i" is to be moved.
      The result of the permutation is stored in the calling matrix. Only entire matrix can be copied.
</remarks>


      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="ColPermute"/>*)
    function RowPermute(const Src: TMtx; const PermuteIdx: TVecInt): TMtx; overload;

    (*<summary>Concatenate an array of matrices to single matrix.</summary>
      
<remarks>Concatenate an array of matrices to form one big matrix and store the result in the calling matrix. The dimensions
      of the block matrices in the Src array must match, to form the new matrix. The block matrices must be all real or
      all complex, otherwise an exception will be raised. You must specify Arows*ACols block matrices in the Src array.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix
      are adjusted automatically.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtx;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(2,2);
          B.Size(A);
          C.Size(A);
          D.Size(A);
          E.Concat(2,2,[A,B
                        C,D]);  // form one 4x4 matrix
        finally
          FreeIt(E);
          FreeIt(A,B,C,D);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ConcatHorz"/>
      <SeeAlso cref="ConcatVert"/>*)
    function Concat(ARows, ACols: integer;const Src: array of TMtx): TMtx;  overload;

    (*<summary>Concenates an array of matrices horizontally.</summary>
      
<remarks>Concenate the Src matrices horizontally and store the results in the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted
      automatically. An exception is raised if any of the Src matrices Complex or Rows property does not match.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtx;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(2,2);
          B.Size(A);
          E.Size(4,4);
          // overwrite the lower part of the E matrix
          //    with values from A and B
          E.ConcatHorz(2,2,[A,B]);
          //E becomes:
          //[E11 E12 E13 E14]
          //[E21 E22 E23 E24]
          //[A11 A12 B11 B12]
          //[A21 A22 B21 B22]
        finally
          FreeIt(E);
          FreeIt(A,B,C,D);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Concat"/>
      <SeeAlso cref="ConcatVert"/>*)
    function ConcatHorz(const Src: array of TMtx): TMtx;  overload;
    (*<summary>Concenate the Src matrices horizontally and store the results in the calling matrix.</summary>
      
<remarks>The DestRow and DestCol parameters indicate the starting position (in the calling matrix) for concenating. An exception is raised,
      if the calling matrix array bounds are overrun. An exception is raised, if any of the Src matrices Complex or Rows properties
      do not match.
</remarks>
*)
    function ConcatHorz(DestRow, DestCol: integer;const Src: array of TMtx): TMtx; overload;

    (*<summary>Concenates an array of matrices vertically.</summary>
      
<remarks>Concenate the Src matrices vertically and store the results in calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted automatically.
      An exception is raised, if any of the Src matrices Complex or Cols properties do not match.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtx;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(1,2);
          B.Size(A);
          E.Size(4,4);
          // overwrite the lower part of the E matrix
          // with values from A and B
          E.ConcatVert(2,2,[A,B]);
          // E becomes:
          //[E11 E12 E13 E14]
          //[E21 E22 E23 E24]
          //[A11 A12 E33 E34]
          //[B11 B12 E43 E44]
        finally
          FreeIt(E);
          FreeIt(A,B,C,D);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Concat"/>
      <SeeAlso cref="ConcatHorz"/>*)
    function ConcatVert(const Src: array of TMtx): TMtx;  overload;
    (*<summary>Concenate the Src matrices vertically and store the results in calling matrix.</summary>
      
<remarks>The DestRow and DestCol parameters indicate the starting position (in the calling matrix) for concatenating.
      An exception is raised, if the calling matrix array bounds are overrun. An exception is raised, if any of the
      Src matrices Complex or Cols properties do not match.
</remarks>
*)
    function ConcatVert(DestRow, DestCol: integer;const Src: array of TMtx): TMtx; overload;

    (*<summary>Performs finite, linear convolution of two two-dimensional signals.</summary>
      
<remarks>Performs finite, linear convolution of two two-dimensional signals. The argument names X and H are chosen to suggest
      FIR filtering. The result of the convolution is defined as follows:

      <IMG name="mtx014"/><para/>

      In the above expressions, X[n, m] is a shorthand for <c>X[n+ m* X.Cols]</c>, <c>H[n, m]</c> is a shorthand for <c>H[n+ m* H.Cols]</c>,
      and y[n, m] is a shorthand for:

      <code>y[n+ m* (X.Cols+ H.Cols-1)].
      </code><para/>

      If NoResize parameter is True, the size of the calling matrix is set to the size of the X matrix and filtering edge
      effect is equally distributed. If the NoResize parameter is False, the size of the calling matrix is set to:

      <code>
      Rows = X.Rows + H.Rows - 1
      Cols  = X.Cols +  H.Cols   - 1
      </code><para/>

      In image processing X is the mitxure and H is the convolution kernel.
</remarks>

      <SeeAlso cref="FFT"/>*)


    (*<summary>Copy the Mtx elements [MtxRow,MtxCol]..[MtxRow+NrRows-1,MtxCol+NrCols-1] to the calling matrix elements
      [Row,Col],,[Row+NrRows-1,Col+NrCols-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is true and bounds are
      overrun. If Transpose is true, the matrix is transposed as well. An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/>
      is true and Complex properties of the calling matrix and Mtx do not match.
</remarks>
*)
    function Copy(const Mtx: TMtx; MtxRow, MtxCol, Row, Col, NrRows, NrCols: integer; Transpose: boolean = false): TMtx;   overload;

    (*<summary>Copies matrix values to a 2D array.</summary>
      
<remarks>Copies all matrix values to a 2D array.
      The rows of the array will have two times as many columns, if the calling matrix object
       is Complex.
</remarks>
*)
    procedure CopyToArray(var Dst: T2DDoubleArray); overload;
    procedure CopyToArray(var Dst: T2DSingleArray); overload;    

    (*<summary>Copies all matrix values to a 2D array.</summary>*)
    procedure CopyToArray(var Dst: T2DCplxArray); overload;
    procedure CopyToArray(var Dst: T2DSCplxArray); overload;    

    (*<summary>Copies the matrix from a 2D array.</summary>
      
<remarks>Sizes the matrix to match the size of the 2D array and
      copies all the values.
</remarks>
*)
    procedure CopyFromArray(const Src: T2DDoubleArray); overload;
    procedure CopyFromArray(const Src: T2DSingleArray); overload;

    (*<summary>Sizes the matrix to match the size of the 2D array and
      copies all the values.</summary>*)
    procedure CopyFromArray(const Src: T2DCplxArray); overload;
    procedure CopyFromArray(const Src: T2DSCplxArray); overload;

    (*<summary>Sizes the Dst array to match the matrix.</summary>
      
<remarks>Sizes the Dst array to match the size of the matrix.
</remarks>
*)
    procedure SizeToArray(var Dst: T2DCplxArray); overload;
    procedure SizeToArray(var Dst: T2DSCplxArray); overload;

    (*<summary>Sizes the Dst array to match the size of the matrix.</summary>
      
<remarks>The rows will have two times as many columns, if the calling matrix object is Complex.
</remarks>
*)
    procedure SizeToArray(var Dst: T2DDoubleArray); overload;
    procedure SizeToArray(var Dst: T2DSingleArray); overload;    

    (*<summary>Sizes the calling matrix to match the size of the array.</summary>
               
<remarks>Sizes the calling matrix to match the size of the array.
               Complex property is set to true. IsDouble property value is preserved.
</remarks>
*)
    procedure SizeFromArray(const Src: T2DCplxArray); overload;
    procedure SizeFromArray(const Src: T2DSCplxArray); overload;
    (*<summary>Sizes the calling matrix to match the size of the array.</summary>
               
<remarks>Complex property is set to false. IsDouble property value is preserved.
</remarks>
*)
    procedure SizeFromArray(const Src: T2DDoubleArray); overload;
    procedure SizeFromArray(const Src: T2DSingleArray); overload;    


    (*<summary>Copies values from vector to a matrix.</summary>
      
<remarks>Copy all Vec elements to the calling matrix. Set the calling matrix <see cref="Rows"/> property to NrRows. Set the calling
      matrix <see cref="Cols"/> property to Vec.Length div NrRow (Length = Rows*Cols). The calling matrix <see cref="TMtxVec.Complex">Complex</see>
      property is adjusted automatically. An exception is raised if Vec.Length mod NrRows &lt;&gt; 0.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          v: TVec;
      begin
        CreateIt(A);
        Create(v);
        try
          v.SetIt(true,[1, 0, 2, -1, 5, 1.2]) ;
          A.CopyVec(v);
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Copy"/>
      <SeeAlso cref="TVec.CopyMtx"/>*)
    function CopyVec(const Vec: TVec; NrRows: Integer): TMtx; overload;
    function Copy(const Src: TMtxVecInt; const dstFloatPrecision: TMtxFloatPrecision): TMtxVec; override;
    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements starting with [Row,Col].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is true and bounds are overrun or if <see cref="TMtxVec.Complex">Complex</see> properties of the calling
      matrix and Vec do not match.
</remarks>
*)
    function CopyVec(const Vec: TVec; VecIndex, Len, Row, Col: Integer): TMtx;   overload;

    (*<summary>Cumulative sum for each of the matrix columns.</summary>
      
<remarks>Calculate the cumulative sum for each of the calling matrix columns in-place.
</remarks>


      <Example>
      <code>
      var Mtx: TMtx;
      begin
        Mtx := TMtx.Create;
        try
          Mtx.Size(3,2,false,[1,2,
                              2,5,
                              3,1]);
          Mtx.CumSum;
          // Mtx becomes:
          // 1, 2
          // 3, 7
          // 6, 8
        finally
          Mtx.Free;
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SumCols"/>*)
    function CumSum: TMtx; overload;
    (*<summary>Calculate the cumulative sum for each of the X matrix columns.</summary>
      
<remarks>Store the results in calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see>
      properties of the callig matrix are adjusted implicitly to match those of Mtx matrix.
</remarks>
*)
    function CumSum(const Mtx: TMtx): TMtx; overload;

    (*<summary>Determines type of the matrix.</summary>
      
<remarks>Tests the calling matrix and returns the <see cref="TMtxType"/> of the calling of matrix.
      The following types are supported:
      <list>
      <item> <see cref="TMtxType.mtSymmPosDef"/>	= symmetric positive definite matrix </item>
      <item> <see cref="TMtxType.mtSymmetric"/> = symmetric matrix </item>
      <item> <see cref="TMtxType.mtHermPosDef"/> = Hermitian positve definite matrix </item>
      <item> <see cref="TMtxType.mtHermitian"/>	= Hermitian matrix </item>
      <item> <see cref="TMtxType.mtTriangle"/>	= triangular matrix, with unit or non unit main diagonal </item>
      <item> <see cref="TMtxType.mtGeneral"/>	= general matrix (none of the above) </item>
      </list>

      Checking for positive definite matrix can be very expensive (O(n) = 1/3*n^3). Therefore the ChekPosDef
      parameter is False by default, unless the user specifies otherwise. The method will not detect banded matrix storage.
</remarks>


      <Example>
      <code>
      var X,B: TVec;
          A: TMtx;
          at: TMtxType;
      begin
        CreateIt(X,B);
        CreateIt(A);
        try
          B.SetIt(false,[0,2]);
          A.SetIt(2,2,false,[1,2,
                  2,4]);  // 2x2 real matrix

          at := A.DetectMtxType;
          A.LUSolve(B,X,at);
          // This is the same for this example (except slower)
          A.LUSolve(B,X,mtSymmetric);
          // You could also specify
          A.AutoMtxType := True
          A.LUSolve(B,X);  // the type is autodetected
        finally
          FreeIt(B,X);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>*)
    function DetectMtxType(CheckPosDef: boolean = False): TMtxType; overload;

    (*<summary>Convert dense matrix to banded.</summary>
      
<remarks>Convert the calling matrix stored in dense format to banded format and store the result in Dst.
      MaxElemCount defines the maximum number of elements that the Banded matrix may have before an
      exception will be raised.
</remarks>


      <SeeAlso cref="BandedToDense"/>*)
    procedure DenseToBanded(const Dst: TMtx; MaxElemCount: integer = 10000000); overload;

    (*<summary>Determinant of squared matrix.</summary>
      
<remarks>Calculate the determinant of squared real matrix. An exception is raised if the calling
      matrix <see cref="TMtxVec.Complex">Complex</see> property is true.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          b: double;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,false,[1,2,
                            2,4]);  // 2x2, not complex matrix
          b := A.Determinant; // 4 + 4 = 9
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>*)
    function Determinant(MtxType: TMtxType = mtGeneral): double; overload;
    (*<summary>Calculates the determinant of squared complex matrix.</summary>
      <returns>the result in result complex variable.</returns>
      
<remarks>An exception is raised if the calling matrix <see cref="TMtxVec.Complex">Complex</see> property is false.
</remarks>
*)
    procedure Determinant(out result: TCplx; MtxType: TMtxType = mtGeneral); overload;

    (*<summary>Disowns a pointer to an array from matrix Values1D.</summary>
      
<remarks>The method does the opposite of the <see cref="Adopt"/> method. It will set the AArrays to Pointer(Values),
      ARows and ACols to matrix's <see cref="Rows"/> and <see cref="Cols"/> and IsComplex to matrix
      <see cref="TMtxVec.Complex">Complex</see> property. Use the Disown method to "disconnect" AArray from the TMtx.Values1D.
      Disown sets Values1D and CValues1D array pointers to nil and Rows, Cols properties to ACols, but without freeing
      the allocated memory. The allocated memory can be disowned only, if it was adopted with a call to the
      <see cref="Adopt"/> method.
</remarks>


      <SeeAlso cref="Adopt"/>*)
    
    procedure Disown(out AArray: PAPointer; out ARows, ACols: integer; out aIsComplex: boolean; out aIsDouble: boolean); overload;
    procedure Disown(out AArray: PAPointer; out ARows, ACols: integer; out aFloatPrecision: TMtxFloatPrecision); overload;
    
    (*<summary>Disowns a pointer to an array from calling matrix Values1D.</summary>
      
<remarks>It dismisses the Values, Values1D, CValues1D array and
      sets the <see cref="Rows"/> and <see cref="Cols"/> to zero.
</remarks>
*)
    procedure Disown;  overload;

    (*<summary>Sets the matrix diagonal to values from the  vector.</summary>
      
<remarks>Sets the  k-th diagonal of the calling matrix to values from from the Vec object. If k = 0 then the main
      diagonal matrix is set, if k &lt; 0 then the k-th subdiagonal matrix is set and if k &gt; 0 the then the k-th
      super diagonal is set. The matrix must be large enough to hold diagonal or an exception is raised.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          D: TVec;
      begin
        CreateIt(A);
        CreateIt(D);
        try
          A.SetIt(2,2,True,[1,1, 2,1,
                           1,2, 2,2]);
          D.SetIt(2,  True,[2,2, 1,1]);
          A.Diag(D,0);
          // A becomes:
          // [2,2, 2,1]
          // [1,2, 1,1]
        finally
          FreeIt(D);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="TVec.Diag"/>*)
    function Diag(const Vec: TVec; k: integer): TMtx; overload;

    (*<summary>Calculates matrix eigenvalues and eigenvectors.</summary>
      
<remarks>Calculates the eigenvalues (stored in vector D elements) and optionally the eigenvectors (stored in matrix the columns of
      matrices VL and VR) of the calling matrix. The MtxType parameter indicates the calling matrix <see cref="TMtxType" text="type"/>.
      Depending the type of the calling matrix the Eig method will use specific optimized algorithm. If you don't know the
      type of the calling matrix, you can not omit the MtxType parameter. To determine the type of matrix use the
      <see cref="DetectMtxType"/> method, or set <see cref="AutoMtxType"/> property to True.<para/>
      Depending on the MtxType parameter, the following algorithms will be used :


      <b>1)</b> for symmetric positive definite matrix (mtSymPosDef, mtHermPosDef):

      1. reduce the calling matrix to tridiagonal form <c>A=Q*T*Q'</c>.
      2. generate matrix Q.
      3. find eigenvalues and eigenvectors of the tridiagonal matrix T.


      <b>2)</b> for symmetric or hermitian matrix (mtSymetric, mtHermitian):

      1. reduce the calling matrix to tridiagonal form <c>A=Q*T*Q'</c>.
      2. generate matrix Q.
      3. find eigenvalues and eigenvectors of the tridiagonal matrix T.


      <b>3)</b> for general matrix (mtGeneral):

      * VectorForm = vfEig:
        1. perform full balance of the calling matrix,
        2. reduce the calling matrix to Hessenberg form <c>A=Q*H*Q'</c>,
        3. generate matrix Q,
        4. find eigenvalues and left and/or right eigenvectors,
        5. transform eigenvectors of the balanced matrix back to those of the original matrix.
      * VectorForm = vfSchur:
        1. perform user defined balance of the calling matrix,
        2. reduce the calling matrix to Hessenberg form <c>A=Q*H*Q'</c>,
        3. generate matrix Q,
        4. Find the real Schur form and optionally the eigenvectors <c>Z*T*Z'</c>. Matrix T is the real Schur
          form and Z are the eigenvectors.
        5. transform the eigenvectors of the balanced matrix to those of the original matrix.
           Matrix is in real Schur form, if it is upper quasi-triangular with  1-by-1 and 2-by-2 blocks. 2-by-2 blocks
           are standardized in the form
           <c>[ a  b  ]</c><para/>
           <c>[ c  a  ]</c><para/>
           where <c>b*c &lt; 0</c>. The eigenvalues of such a block are: <c>a +/- sqrt(bc).</c>

      Note
        The calling matrix <see cref="Quadratic"/> property must be true, otherwise an
        exception is raised.

      This overloaded version calculates the calling matrix eigenvalues and stores them in the vector D. The
      <see cref="TMtxVec.Complex">Complex</see> and <see cref="TMtxVecBase.Length" text="Length"/> properties of
      the vector D are adjusted automatically. The MtxType parameter specifies the calling <see cref="TMtxType" Text ="matrix type"/>.
      If MtxType parameter is omitted, the default value mtGeneral (general matrix) will be used, but for symmetric matrices the
      matrix type has to be specified explicitly, because the algorithm for general matrices will fail on the symmetric matrix.
      The matrix Q is not explicitly generated.
</remarks>


      <Example>
      <code>
      var  D: TVec;
          A,V: TMtx;
      begin
        CreateIt(D);
        CreateIt(A,V);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);  // 2x2, real matrix
          A.Eig(D,mtSymmetric);
          // or if eigenvectors are also required
          A.Eig(V,D,nil,mtSymmetric);  // get left eigenvectors
        finally
          FreeIt(A,V);
          FreeIt(D);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="EigGen"/>*)
    function Eig(const D: TVec; MtxType: TMtxType = mtGeneral): TMtx; overload;
    (*<summary>Calculate the calling matrix eigenvalues and eigenvectors.</summary>
      
<remarks>Store the eigenvalues in the vector D. The Complex and Length
      properties of the vector D are adjusted automatically. If VectorForm is vfEig, store the left eigenvectors in matrix VL and
      the right eigenvectors in the matrix VR. Either of the eigenvector matrices VL and VR can be nil to indicate that they are not
      required. If the are both nil, an exception is raised. In case of symmetric matrices only VL is checked. The computed
      eigenvectors are normalized to have Euclidean norm equal to 1 and largest component real and are stored in the columns of the VL and VR matrices.
      If VectorForm is vfSchur, it will store the real T form in the matrix VL and the eigenvectors Z in the matrix VR. VR can be nil,
      to indicate that eigenvectors are not required. If VL is nil an exception is raised. The Rows, Cols and Complex properties of
      matrices VR and VL are adjusted automatically. The MtxType parameter specifies the calling <see cref="TMtxType" text="matrix type"/>. If MtxType
      parameter is omitted, the default value mtGeneral (general matrix) will be used, but for symmetric matrices the matrix type has to be
      specified explicitly, because the algorithm for general matrices will fail on the symmetric matrix.
</remarks>
*)
    function Eig(const VL: TMtx; const D: TVec; const VR: TMTx = nil; MtxType: TMTxType = mtGeneral; VectorForm: TVectorForm = vfEig; aExpand: boolean = True): TMtx; overload;

    (*<summary> Computes the eigenvalues and left and right eigenvectors of A general matrix. </summary>
                
<remarks>If VL and VR are not nil, corresponding left and right eigenvectors are computed.
                Does not work for symmetric matrices. Use EigSym for symmetric matrices.
</remarks>
*)

    procedure Eig(const D: TVec; const VL, VR: TMtx); overload;

    (*<summary> Computes the eigenvalues and left and right eigenvectors of A general matrix. </summary>
                
<remarks>If VL and VR are not nil, corresponding left and right eigenvectors are computed.
                Additionally balancing can be speicifed with Balance. The info about balancing performed is stored in
                TEigBalancing object. This object can be nil, if no balancing was requested or additional information
                about balancing is not needed. If assigned the rconde  parameter contains reciprocal condition number of the j-th eigenvalue.
                These reciprocal condition numbers always lie between zero (very badly conditioned) and one (very well
                conditioned). If assigned the rcondv contains reciprocal condition number of the j-th right eigenvector.
                Does not work for symmetric matrices. Use EigSym for symmetric matrices.
</remarks>
*)

    procedure Eig(const D: TVec; const Balance: TBalanceType; BInfo: TEigBalancing = nil;
                                 const  rconde: TVec = nil; const rcondv: TVec = nil; const VL: TMtx = nil; const VR: TMTx = nil); overload;

    (*<summary>Computes eigenvalues and optionally Schure matrix</summary>
           
<remarks>Computes for an N-by-N nonsymmetric matrix A, the eigenvalues, the Schur form T, and,
           optionally, the matrix of Schur vectors VS.  This gives the Schur factorization A = VS*T*(VS**H).

           Optionally the routine also orders the eigenvalues on the diagonal of the
           Schur form so that selected eigenvalues are at the top left.
           The Select function parameters accepts a function with two parameters. First is the real and the second is the
           imaginary part of the eigenvalue. The function is to return true, if the specified eigenvalue is to be
           included in to sorting at the top left of the Schur form. If sorting is not required pass nil (NULL) for this
           parameter.
</remarks>
*)

    procedure EigSchure(const D: TVec; VS: TMTx = nil; const T: TMtx = nil; dSelect: TSelectTwoFunction = nil; zSelect: TSelectOneFunctionC = nil;
                                                                            sSelect: TSelectTwoFunctionSingle = nil; cSelect: TSelectOneFunctionSingleC = nil);  overload;

    (*<summary>Computes eigenvalues and optionally Schure matrix</summary>
           
<remarks>Computes for an N-by-N nonsymmetric matrix A, the eigenvalues, the Schur form T, and,
           optionally, the matrix of Schur vectors VS.  This gives the Schur factorization A = VS*T*(VS**H).

           Optionally, it also orders the eigenvalues on the diagonal of the
           Schur form so that selected eigenvalues are at the top left. If this is needed specify the Select function parameter
           and return true for each eigenvalue desired.
           Optionally computes a reciprocal condition number for the average of the
           selected eigenvalues (rconde[0]) and computes a reciprocal condition
           number for the right invariant subspace corresponding to the
           selected eigenvalues (rcondv[0]).  The leading columns of VS form an
           orthonormal basis for this invariant subspace.
</remarks>
*)

    procedure EigSchure(const D: TVec; VS: TMTx; const T: TMtx; const rconde, rcondv: TVec; dSelect: TSelectTwoFunction = nil; zSelect: TSelectOneFunctionC = nil;
                                                                                            sSelect: TSelectTwoFunctionSingle = nil; cSelect: TSelectOneFunctionSingleC = nil); overload;

    (*<summary>Computes all the eigenvalues, and optionally, the eigenvectors of a generalized symmetric-definite eigenproblem.</summary>
                
<remarks>It can find solution to either of the following problems:
                <code>
                A*x = lambda*B*x, A*B*x = lambda*x, or B*A*x = lambda*x
                </code>
                Here A and B are assumed to be symmetric (Hermitian) and B is also positive definite.
                Eigenvector are stored within V in columns.
</remarks>


              <Example>
              <code>
              var  D: TVec;
                A,B,C: TMtx;
              begin
                CreateIt(D);
                CreateIt(A,B,C);
                try
                  D.SetIt(2  ,False,[0,2]); // vector, length 2, real
                  A.SetIt(2,2,False,[1,2,
                                     2,4]);  // 2x2, real matrix
                  // A must be symmetric
                  B.SetIt(2,2,False,[1,1,
                                     1,2]);  // 2x2, real matrix
                  // B must be symmetric and positive definite
                  A.EigSymGen(B,D);	// Use A and B to find eigenvalues
                finally
                  FreeIt(A,B,C);
                  FreeIt(D);
                end;
              end;
              </code>
              </Example>

              <SeeAlso cref="Eig"/>*)

    procedure EigSymGen(const B: TMtx; const D: TVec; const V: TMtx; const EigGenType: TEigGenType = etAzBz); overload;
    procedure EigSymGen(const B: TMtx; const D: TVec; const EigGenType: TEigGenType = etAzBz); overload;

    (*<summary>Computes generalized eigenvalues with reduction of the symmetric-definite generalized eigenvalues/eigenvectors problem to the normal eigenvalue case.</summary>
              
<remarks>The routine computes selected eigenvalues and optionally also eigenvectors. The problem is of type:
              <code>
               A*x = lambda*B*x, A*B*x = lambda*x, or B*A*x = lambda*x
              </code>
              A and B are symmetric (Hermitian) and B is also positive definite. Eigenvalues and eigenvectors can be selected
              by specifying a range of values. Eigenvectors are not computed, if V is passed as nil. (NULL).
              <para/>
              Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
              An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
              of width less than or equal to
              <code>
              Tolerance + EPS / max( |a|,|b| ) ,
              </code>
              where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
              where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              2*UnderflowThreshold.
              <para/>
              If V is assinged, VInfo contains values equal to 0 at indices for which eigenvector calculation converged.
              Eigenvector are stored within V in columns. The returned column count may vary between calls depending on the number
              of eigenvectors that converged. The eigenvectors are normalized as follows:
              <code>
              etAzBz, etBAz, Z**T*B*Z := I;
              etABz        , Z**T*inv(B)*Z := I.
              </code>

              <see href="Lapack Users Guide"/>.
</remarks>
*)

    procedure EigSymGen(const B: TMtx; const D: TVec; Minimum, Maximum: double; V: TMtx; var VInfo: TIntegerArray; Tolerance: double = 0; const EigGenType: TEigGenType = etAzBz); overload;
    procedure EigSymGen(const B: TMtx; const D: TVec; const Minimum, Maximum: double; const Tolerance: double = 0; const EigGenType: TEigGenType = etAzBz); overload;

    (*<summary>Computes generalized eigenvalues with reduction of the symmetric-definite generalized eigenvalues/eigenvectors problem to the normal eigenvalue case.</summary>
              
<remarks>The routine computes selected eigenvalues and optionally also eigenvectors. The problem is of type:
              <code>
               A*x = lambda*B*x, A*B*x = lambda*x, or B*A*x = lambda*x
              </code>

              A and B are symmetric (Hermitian) and B is also positive definite. Eigenvalues and eigenvectors can be selected
              by specifying a range of indexes of values. Eigenvectors are not computed, if V is passed as nil (NULL).
              <para/>
              Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
              An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
              of width less than or equal to
              <code>
              Tolerance + EPS / max( |a|,|b| ) ,
              </code>
              where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
              where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              2*UnderflowThreshold.

              The first eigenvalue index is 1 and the last index is equal to row/column count. LowerRange and UpperRange need to
              be specified within this interval: 1 &lt;= LowerRange &lt;= UpperRange &lt;= Rows
              <para/>
              If V is assinged, VInfo contains values equal to 0 at indices for which eigenvector calculation converged.
              Eigenvector are stored within V in columns. The returned column count may vary between calls depending on the number
              of eigenvectors that converged. The eigenvectors are normalized as follows:
              <code>
              etAzBz, etBAz, Z**T*B*Z := I;
              etABz        , Z**T*inv(B)*Z := I.
              </code>
</remarks>
*)

    procedure EigSymGen(const B: TMtx; const D: TVec; LowerRange, UpperRange: integer; V: TMtx; var VInfo: TIntegerArray; Tolerance: double = 0; const EigGenType: TEigGenType = etAzBz); overload;
    procedure EigSymGen(const B: TMtx; const D: TVec; const LowerRange, UpperRange: integer; const Tolerance: double = 0; const EigGenType: TEigGenType = etAzBz); overload;

    (*<summary>Computes eigenvalues of a symmetric (Hermitian) matrix between minimum and maximum.</summary>
               
<remarks>The computation is based on Relatively Robust Representations.

               <para/>
               Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
               An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
               of width less than or equal to
               <code>
               Tolerance + EPS / max( |a|,|b| ) ,
               </code>
               where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
               where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              UnderflowThreshold.
</remarks>
*)
    function EigSym(const D: TVec; const Minimum, Maximum: double; const Tolerance: double = 0): TMtx; overload;
    (*<summary>Computes eigenvalues and eigenvectors of a symmetric (Hermitian) matrix between minimum and maximum.</summary>
               
<remarks>The computation is based on Relatively Robust Representations.

              Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
              An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
              of width less than or equal to
              <code>
              Tolerance + EPS / max( |a|,|b| ) ,
              </code>
              where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
              where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              UnderflowThreshold.
</remarks>
*)
    function EigSym(const D: TVec; V: TMtx; Minimum, Maximum: double; Tolerance: double = 0): TMtx; overload;
    (*<summary>Computes eigenvalues and eigenvectors of a symmetric (Hermitian) matrix between min and max index.</summary>
               
<remarks>The computation is based on Relatively Robust Representations.

              Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
              An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
              of width less than or equal to
              <code>
              Tolerance + EPS / max( |a|,|b| ) ,
              </code>
              where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
              where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              UnderflowThreshold.

              The first eigenvalue index is 1 and the last index is equal to row/column count. LowerRange and UpperRange need to
              be specified within this interval: 1 &lt;= LowerRange &lt;= UpperRange &lt;= Rows
</remarks>
*)
    function EigSymRange(const D: TVec; V: TMtx; LowRange, HighRange: integer; Tolerance: double = 0): TMtx; overload;
    (*<summary>Computes eigenvalues of a symmetric (Hermitian) matrix between min and max index.</summary>
               
<remarks>The computation is based on Relatively Robust Representations.

              Tolerance parameter specifies the absolute error tolerance for the eigenvalues.
              An approximate eigenvalue is accepted as converged when it is determined to lie in an interval [a,b]
              of width less than or equal to
              <code>
              Tolerance + EPS / max( |a|,|b| ) ,
              </code>
              where EPS is the machine precision. If Tolerance is less than or equal to zero, then  EPS*|T|  will be used in its place,
              where |T| is the 1-norm of the tridiagonal matrix obtained by reducing A to tridiagonal form.

              Eigenvalues will be computed most accurately when Tolerance is set to twice the underflow threshold, not zero.
              If this routine returns fails , indicating that some eigenvectors did not converge, try setting Tolerance to
              UnderflowThreshold.

              The first eigenvalue index is 1 and the last index is equal to row/column count. LowerRange and UpperRange need to
              be specified within this interval: 1 &lt;= LowerRange &lt;= UpperRange &lt;= Rows
</remarks>
*)
    function EigSymRange(const D: TVec; const LowRange, HighRange: integer; const Tolerance: double = 0): TMtx; overload;

    (*<summary>Computes all eigenvalues of a symmetric (Hermitian) matrix between min and max index.</summary>
               
<remarks>The computation is based on Relatively Robust Representations.
</remarks>
*)
    function EigSym(const D: TVec; const V: TMtx): TMtx; overload;

    (*<summary>Computes generalized eigenvalues and eigenvectors of a non-symmetric matrix.</summary>

            
<remarks>A generalized eigenvalue for a pair of matrices (A = Self,B) is a scalar
            lambda or a ratio alpha/beta := lambda, such that A - lambda*B is
            singular. It is usually represented as the pair (alpha,beta), as
            there is a reasonable interpretation for beta = 0, and even for both
            being zero.

            The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
            of (A,B) satisfies:

            <code>
             A * v(j) = lambda(j) * B * v(j).
            </code>

            The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
            of (A,B) satisfies:

            <code>
            u(j)**H * A  = lambda(j) * u(j)**H * B .
            </code>

            where u(j)**H is the conjugate-transpose of u(j). The individual
            eigevalues can be computed as:

            <code>
            lambda(j) = dAlpha(j)/dBeta(j);
            </code>
</remarks>
*)
    procedure EigGen(const B: TMtx; const DAlpha, DBeta: TVec; const VL, VR: TMtx); overload;
    procedure EigGen(const B: TMtx; const DAlpha, DBeta: TVec); overload;

    (*<summary>Computes generalized eigenvalues and eigenvectors of a non-symmetric matrix.</summary>

            
<remarks>Computes for a pair of N-by-N real nonsymmetric matrices (A = Self,B)
            the generalized eigenvalues, and optionally, the left and/or right
            generalized eigenvectors (VL and/or VR).

            A generalized eigenvalue for a pair of matrices (A,B) is a scalar
            lambda or a ratio alpha/beta := lambda, such that A - lambda*B is
            singular. It is usually represented as the pair (alpha,beta), as
            there is a reasonable interpretation for beta = 0, and even for both
            being zero.

            The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
            of (A,B) satisfies:

            <code>
            A * v(j) = lambda[j] * B * v(j)
            </code>

            The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
            of (A,B) satisfies:

            <code>
            u(j)**H * A  = lambda[j] * u(j)**H * B
            </code>

            where u(j)**H is the conjugate-transpose of u(j). The individual
            eigevalues can be computed as:

            <code>
            lambda[j] := dAlpha[j]/dBeta[j];
            </code>

            Optionally also computes a balancing transformation to improve the conditioning of the eigenvalues and
            eigenvectors , reciprocal condition numbers for the eigenvalues
            (rconde), and reciprocal condition numbers for the right eigenvectors (rcondv).
</remarks>
*)

    procedure EigGen(const B: TMtx; const DAlpha, DBeta: TVec; const Balance: TBalanceType; BInfo: TEigBalancing;
                     const rconde: TVec = nil; const rcondv: TVec = nil; const VL: TMtx = nil; const VR: TMTx = nil); overload;

    (*<summary>Computes generalized eigenvalues and Schur vectors of a non-symmetric matrix.</summary>
            
<remarks>It gives Schur factorization (A = Self) :

            <code>
            (A,B) = ( VL*S*VR^T, VL*T*VR^T )
            </code>

            If only the generalized eigenvalues are needded then EigGen is faster. The individual
            eigevalues can be computed as:

            <code>
            lambda(j) = dAlpha(j)/dBeta(j);
            </code>

             If any of the VL, VR, S, T, rconde, rcondv are not required, pass nil value for the pointer.

             Optionally the routine also orders the eigenvalues on the diagonal of the
             Schur form so that selected eigenvalues are at the top left.
             The Select function parameters accepts a function with four parameters. In case of real data, the first is the real and the second is the
             imaginary part of the eigenvalue (alpha), the third is the real beta. In case of complex data, first two are complex dAlpha
             and the second two complex dBeta. The function is to return true, if the specified eigenvalue is to be
             included in to sorting at the top left of the Schur form. If sorting is not required pass nil (NULL) for this
             parameter.


             Optionally computes a reciprocal condition number for the average of the
             selected eigenvalues (rconde[0], rconde[1]) and computes a reciprocal condition
             number for the selected deflating subspaces (rcondv[0], rcondv[1]).
             If condition numbers are not required, pass nil (NULL) for this parameter.
</remarks>
*)

    procedure EigSchureGen(const B: TMtx; const DAlpha, DBeta: TVec; const VL, VR, S, T: TMtx; rconde, rcondv: TVec;
                          dSelect: TSelectThreeFunction = nil; zSelect: TSelectTwoFunctionC = nil;
                          sSelect: TSelectThreeFunctionSingle = nil; cSelect: TSelectTwoFunctionSingleC = nil); overload;
    procedure EigSchureGen(const B: TMtx; const DAlpha, DBeta: TVec; dSelect: TSelectThreeFunction = nil; zSelect: TSelectTwoFunctionC = nil;
                                                                     sSelect: TSelectThreeFunctionSingle = nil; cSelect: TSelectTwoFunctionSingleC = nil); overload;

    (*<summary> Computes generalized singular value decomposition. </summary>
                  
<remarks>Computes the generalized singular value decomposition (GSVD)
                  of an M-by-N real matrix A and P-by-N real matrix B:

                  <code>
                  U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
                  </code>

                  where U, V and Q are orthogonal matrices, and Z' is the transpose of Z.
                  The routine computes optionally the orthogonal transformation matrices U, V and Q.
                  D1 and D2 are diagonal matrices, which on their diagonals contain C and S.

                  <code>
                                K  L
                   D1 =     K [ I  0 ]
                            L [ 0  C ]
                        M-K-L [ 0  0 ]

                                K  L
                   D2 =     L [ 0  S ]
                          P-L [ 0  0 ]

                  </code>

                  The generalized singular values, stored in C and S on exit, have the
                  property:

                  <code>
                  sqr(C) + sqr(S) = I
                  </code>

                  The generalized singular value pairs of A and B in case of rank
                  deficiency are stored like this:

                  <code>
                  c(0:k-1) = 1,
                  s(0:k-1) = 0,

                  and if m-k-l = 0,

                  c(k:k+l-1) = C,
                  s(k:k+l-1) = S,

                  or if m-k-l &lt; 0,

                  c(k:m-1)= C, c(m:k+l-1)=0
                  s(k:m-1) = S, s(m:k+l-1) = 1

                  and

                  c(k+l:n-1) = 0
                  s(k+l:n-1) = 0
                  </code>

                  Effective rank is k + l.

                  If B is an N-by-N nonsingular matrix, then the GSVD of
                  A and B implicitly gives the SVD of A*inv(B):

                  <code>
                  A*inv(B] := U*(D1*inv(D2))*V'
                  </code>

                  If ( A',B')' has orthonormal columns, then the GSVD of A and B is
                  also equal to the CS decomposition of A and B. Furthermore, the GSVD
                  can be used to derive the solution of the eigenvalue problem:

                  <code>
                  A'*A x := lambda* B'*B x
                  </code>

                  In some literature, the GSVD of A and B is presented in the form
                  <code>
                  U'*A*X := ( 0 D1 ),   V'*B*X := ( 0 D2 )
                  </code>
                  where U and V are orthogonal and X is nonsingular, D1 and D2 are "diagonal".
                  The former GSVD form can be converted to the latter form by taking the nonsingular matrix X as:

                  <code>
                  X = Q*( I   0    )
                        ( 0 inv(R) )
                  </code>

                  The function returns the effective numerical rank of (A', B') = K + L.

                  Reference: Lapack v3.4 source code
</remarks>
*)

    function SVDGen(const B: TMtx; const C, S: TVec; U: TMtx; V: TMtx; Q: TMtx): integer; overload;
    function SVDGen(const B: TMtx; const C, S: TVec): integer; overload;


    (*<summary> Solves the linear equality-constrained least squares (LSE). </summary>
                  
<remarks>Solves the linear equality-constrained least squares (LSE) problem:

                  <code>
                  minimize || c - A*x ||_2   subject to   B*x = d
                  </code>

                  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
                  vector of length M, and d is a given vector of length P. The sign "_2", denotes Norm L2.
                  It is assumed that  P &lt;= N &lt;= M+P, and

                  <code>
                  rank(B) = P and  rank( (A) ) = N
                                       ( (B) )
                  </code>

                  These conditions ensure that the LSE problem has a unique solution,
                  which is obtained using a generalized RQ factorization of the
                  matrices (B, A) given by

                  <code>
                  B = (0 R)*Q,   A = Z*T*Q
                  </code>

                 The function returns the residual sum of squares for the solution

                 References: <para/>
                 1.) Lapack v3.4 source code <para/>
                 2.) http://isites.harvard.edu/fs/docs/icb.topic774900.files/lec16.09.pdf <para/>
                 3.) http://www.cs.ucdavis.edu/~bai/publications/andersonbaidongarra92.pdf <para/>
</remarks>
*)

    function LSESolve(const B: TMtx; const C, D, X: TVec): double; overload;

    (*<summary>  Solves a general Gauss-Markov linear model (GLM) problem. </summary>

            
<remarks>The routine solves a general Gauss-Markov linear model (GLM) problem:
            <code>
                    minimize || y ||_2   subject to   d = A*x + B*y
                            x
            </code>
            where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
            given N-vector. It is assumed that M &lt;= N &lt;= M+P, and

            <code>
                       rank(A) = M    and    rank( A B ) = N.
            </code>

            Under these assumptions, the constrained equation is always
            consistent, and there is a unique solution x and a minimal 2-norm
            solution y, which is obtained using a generalized QR factorization
            of the matrices (A, B) given by

            <code>
               A = Q*(R),   B = Q*T*Z
                     (0)
            </code>

            In particular, if matrix B is square nonsingular, then the problem
            GLM is equivalent to the following weighted linear least squares
            problem
            <code>
                                 minimize || inv(B)*(d-A*x) ||_2
                                         x
            </code>

           where inv(B) denotes the inverse of B. The sign _2, denotes Norm L2.

           References: <para/>
           1.) Lapack v3.4 source code
</remarks>
*)


    procedure GLMSolve(const B: TMtx; const D, X, Y: TVec); overload;

    (*<summary>Compares two matrices.</summary>
      
<remarks>Compares Mtx with the calling matrix and returns true if the matrices are equal (if all elements match in position
      and value). Tolerance defines the comparison tolerance. The maximum difference between elements may not exceed: +/-Tolerance.
      If Tolerance is omitted, a direct comparison algorithm is used.
</remarks>


      <Example>
      <code>
      var A,B: TMtx;
        c: boolean;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,false,[1,2,
                             2,4]);  // 2x2, real matrix
          B.SetIt(2,2,false,[1,2,
                             2,4]);  // 2x2, real matrix
          c := A.Equal(B,1e-8); // Check for differences bigger than 0.00000001
          c := A.Equal(B); // Check for an exact match
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>*)
    function Equal(const Mtx: TMtx; Tolerance: double = 0): boolean; overload;
    (*<summary>Allows to specify the comparison method via the Compare parameter.</summary>*)
    function Equal(const Mtx: TMtx; Tolerance: double; Compare: TCompare):boolean; overload;

    (*<summary>Constructs an eye matrix.</summary>
      
<remarks>Construct an eye matrix. The number of rows and columns of an eye matrix is set by ARows and ACols
      parameters. The <see cref="TMtxVec.Complex">Complex</see> property of an eye matrix is set by AComplex parameter.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A);
        try
          A.Eye(3,3);
          // ...
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>*)
    function Eye(ARows,ACols: integer; AComplex: boolean; aIsDouble: boolean): TMtx; overload;
    function Eye(ARows,ACols: integer; aFloatPrecision: TMtxFloatPrecision): TMtx; overload;
    function Eye(ARows,ACols: integer; aFloatPrecisionRef: TMtxVec): TMtx; overload;

    (*<summary>Forward parallel in-place 1D FFT.</summary>
      
<remarks>Performs forward 1D FFT on each row of the matrix. See <see cref="TVec.FFT"/> for
      more info on the forward FFT.
</remarks>
*)
    function FFT1D: TMtx; overload;
    (*<summary>Forward parallel not in-place 1D FFT.</summary>
     
<remarks>The size of the destination is set automatically
</remarks>
*)
    function FFT1D(const Mtx: TMtx): TMtx; overload;
    (*<summary>Forward parallel in-place 1D FFT from real to complex.</summary>
      
<remarks>Performs forward 1D FFT from real to complex on each row of the matrix. See
      <see cref="TVec.FFTFromReal"/> for more info on the forward real FFT.
</remarks>
*)
    function FFT1DFromReal: TMtx; overload;
    (*<summary>Forward parallel not in-place 1D real to complex FFT.</summary>
     
<remarks>The size of the destination is set automatically.
</remarks>
*)
    function FFT1DFromReal(const Mtx: TMtx): TMtx; overload;

    (*<summary>Forward two-dimensional Fast Fourier Transformation from real/complex to complex.</summary>
      
<remarks>The transformation is applied in-place. The source matrix may be real or complex.
      The result will be always complex. The size of the calling matrix is not changed.
      (rows and cols will not chage).
</remarks>


      <Example>
      <code>
      var a: TMtx;
      begin
        CreateIt(a);
        try
          a.SetIt(2,4,False,
                  [1, 2, 3, 4,
                  -5, 6,-7, 8]);
          a.FFT2D;
          // result will be
          [(12,0), ( 0,4),  (-28,0), (0,-4),
           ( 8,0), (-4,0),  ( 24,0), (-4,0)]

        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2D"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function FFT2D: TMtx; overload;

    (*<summary>Forward two-dimentional Fast Fourier Transformation from real/complex to complex.</summary>
      
<remarks>The transformation is applied on data in Mtx matrix and results are saved in the calling matrix.
      The source matrix may be real or complex. The source matrix is not changed.
      The calling matrix will be complex and will be of the same size as the source matrix.
      (rows and cols will not chage).
</remarks>


      <Example>
      <code>
      var a,b: TMtx;
      begin
        CreateIt(a,b);
        try
          a.SetIt(2,4,False,
                  [1, 2, 3, 4,
                  -5, 6,-7, 8]);

          b.FFT2D(a);
          // result will be
          [(12,0), ( 0,4),  (-28,0), (0,-4),
           ( 8,0), (-4,0),  ( 24,0), (-4,0)]

        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2D"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function FFT2D(const Mtx: TMtx): TMtx; overload;

    (*<summary>Forward two-dimentional Fast Fourier Transformation for real numbers.</summary>
      
<remarks>The transformation is applied in-place. The source matrix must be real. An exception will be raised, if the source matrix is complex.
      The calling matrix stays real and does not change it's size, if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfPack or to fsfPerm.
      If the <see cref="TDenseMtxVec.FFTStorageFormat"/> property was set to fsfCCS then the calling matrix
      becomes complex and is resized up to [Rows, Cols div 2],
      i.e. matrix changes <see cref="TMtxVec.Complex">Complex</see> property, but does not reallocate storage for elements.
      The number of element which are involved in the transformation depends on the property <see cref="TDenseMtxVec.FFTStorageFormat"/>.
      If that property is set to fsfPerm or to fsfPack, then all elements are involved into the trasformation.
      If that property is set to fsfCCS (which is default), then number of element which are involved into the transformation depends
      on the property <see cref="TDenseMtxVec.FFTOddLength"/> and
      these rules will be followed:

      * Number of source columns must be even and more then two. Number of source rows must be more than two. Exception will be raised otherwise;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to False, elements from range [0..Cols-2] x [0..Rows-2] will be involved into transformation;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to True, elements from range [0..Cols-1] x [0..Rows-2] will be involved into transformation;

      Note
        Not all source elements are involved into transformation but all elements in the result matrix are needed for the inverse transformation.
</remarks>


      <Example>
      <code>
      var a: TMtx;
      begin
        CreateIt(a);
        try
          a.SetIt (4,4,False,
                   [1,  2,  3,  4,
                   -5,  6, -7,  8,
                    9, 12,-11, 10,
                   16,-15,-14,  0]);

          a.FFTStorageFormat := fsfCCS;
          a.FFTOddLength := False;

          // the next elements are involved into transformation
              [1, 2,
              -5, 6]

          a.FFT2DFromReal;
          // result will be
          // [(4,0), (-12,0),
          //  (0,0), (0,0),
          //  (2,12), (10,10),
          //  (0,-15), (0,0)]

          // after inverse transformation (a.IFFT2DToReal) only elements in range
          // [0..Col-2] x [0..Rows-2] will be reconstructed, other elements will be undefined
          //  [1, 2, undef, undef,
          //  -5, 6, undef, undef,
          //  undef, undef, undef, undef,
          //  undef, undef, undef, undef]
          //
          // if a.FFTOddLength is set to true,
          // then the range of reconstruction would be [0..Col-1] x [0..Rows-2].

        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="IFFT2D"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function FFT2DFromReal: TMtx; overload;

    (*<summary>Forward two-dimentional Fast Fourier Transformation from real to complex.</summary>
      
<remarks>The transformation is applied on the Mtx matrix and results are saved in the calling matrix.
      The Mtx matrix must be real. And exception will be raised if the source matrix is complex.
      The source matrix will not be changed. The calling matrix will be real and will be resized up to [Mtx.Rows,Mtx.Cols], if <see cref="TDenseMtxVec.FFTStorageFormat"/> is set to fsfPack or to fsfPack.

      If <see cref="TDenseMtxVec.FFTStorageFormat"/> is set to fsfCCS, calling matrix will be complex and will be resized up to <c>[Mtx.Rows, Mtx.Cols div 2]</c>.
      The number of element which are involved in transformation depends on the property <see cref="TDenseMtxVec.FFTStorageFormat"/>.
      If that property is set to fsfPerm or to fsfPack, then all the source elements are involved into the trasformation.
      If that property is set to fsfCCS (which is default), then the number of elements which are involved into the transformation depends on the property <see cref="TDenseMtxVec.FFTOddLength"/> and
      these rules will be followed:
      * Number of source columns must be even and more then two. Number of source rows must be more than two. Exception will be raised otherwise;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to False, elements from range [0..Mtx.Cols-2] x [0..Mtx.Rows-2] will be involved into transformation;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to True, elements from range [0..Mtx.Cols-1] x [0..Mtx.Rows-2] will be involved into transformation;

      NOTE
        Not all source elements are involved into transformation but all elements in the result matrix are needed for the inverse transformation.
</remarks>


      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="IFFT2D"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function FFT2DFromReal(const Mtx: TMtx): TMtx; overload;

    (*<summary>Flips the matrix elements horizontally.</summary>
      
<remarks>Flip calling matrix elements horizontally - element [row, j] = element [row, Cols-j]. This operation
      is performed on all calling matrix rows.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);  // 2x2, not complex matrix
          B.FlipHor(A);
          // B becomes:
          // [2,1]
          // [4,2]
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FlipVer"/>*)
    function FlipHor: TMtx; overload;
    (*<summary>Flip all SrcMtx matrix elements horizontally.</summary>
      
<remarks>Store the results in the calling matrix. The flip operation is performed on
       all SrcMtx matrix rows. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVec.Complex">Complex</see> properties of callign matrix are adjusted automatically.
</remarks>
*)
    function FlipHor(const SrcMtx: TMtx): TMtx; overload;

    (*<summary>Flips the matrix elements vertically</summary>
      
<remarks>Flip calling matrix elements vertically - element [j, col] = element [Rows-j, col]. This operation
      is performed on all calling matrix columns.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);  // 2x2, not complex matrix
          B.FlipVert(A);
          // B becomes:
          // [2,4]
          // [1,2]
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FlipHor"/>*)
    function FlipVer: TMtx; overload;
    (*<summary>Flip all SrcMtx matrix elements vertically.</summary>
      
<remarks>Store the results in the calling matrix. The flip operation is performed
      on all SrcMtx matrix columns. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVec.Complex">Complex</see> properties of callign matrix are adjusted automatically.
</remarks>
*)
    function FlipVer(const SrcMtx: TMtx): TMtx; overload;

    (*<summary>Constructs a Hankel matrix.</summary>
      
<remarks>Constructs a Hankel matrix whose first column is FirstColumn and whose elements are zero below the first anti-diagonal.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the
      calling matrix are adjusted automatically.
</remarks>


      <Example>
      <code>
      var v: TVec;
          H: TMtx;
      begin
        CreateIt(v);
        CreateIt(H);
        try
          v.SetIt(False,[1,2,3,4]);
          H.Hankel(v);
          // H becomes:
          //[1  2  3  4]
          //[2  3  4  0]
          //[3  4  0  0]
          //[4  0  0  0]
        finally
          FreeIt(H);
          FreeIt(v);
        end;
      end;
      </code>
      </Example>*)
    function Hankel(const FirstColumn: TVec): TMtx; overload;

    (*<summary> Applies 2D filtering </summary>
       
<remarks>Computes the dot-product between the kernel matrix and underlaying source matrix elements for each possible position of the kernel
       and stores the result in to the calling object. Typically the kernel is much smaller than the source matrix. The operation is equivalent to the
       2D Convolution and is used also for image resampling, image blurring etc..

       SrcRect must meet the following condtitions:

       SrcRect.Width &lt;= (Src.Cols - Kernel.Cols)
       SrcRect.Height &lt;= (Src.Rows - Kernel.Rows)

       KernelAnchor defines the value to be written in the destination matrix relative to the corresponding position
       of the Kernel in the Src matrix. This can be for example center element (Kernel.Rows div 2, Kernel.Cols div 2), top left (0,0), bottom right (Kernel.Rows-1, Kernel.Cols-1)
       element etc... The X and Y coordinates may not exceed Kernel row or column count.

       The kernel can be square or not. Symmetric or not. The only requirement is that it is rectangular.

       It is the users responsability to select such combination of SrcRect, Kernel size and KernelAnchor to get desired border value processing.
       If the Kernel is 3x3 in size, then it would make sense that SrcRect leaves out 2 rows above and below and also 2 columns left and right in the Src matrix.
       These regions then need to be initialized to some value for example 0 for black color or use the value of the closest border element.

       The DstRect defines the area to be overwritten in the destination matrix. The width and height parameters of DstRect and SrcRect
       need to match. The Size of the destination matrix needs to be big enough to allow DstRect position and dimensions.
</remarks>


      <Example>
      <code>
      var H, A, B: TMtx;
          i: integer;
          SrcRect, DstRect: TRect;
          KernelAnchor: TPoint;
      begin
          CreateIt(H,A,B);

          H.SetIt(3,3,false,[1, 1, 1,
                             1, 1, 1,
                             1, 1, 1]; // simple average kernel

          A.SetIt(9,9,false, [0,	   0,   0,    0,   0,	  0,	 0,	  0,   0,
                            0.5,	 0.5,	0.5,	0.5, 0.5, 0.5, 0.5,	0.5, 0.5,
                              0,	   0,	  0,   	1,   1,  	1,   1,	  1,   0,
                              0,	   0,	  0,    2,   2,	  2, 	 2,	  2,   0,
                              0,	   0,	  0,    3,	 3,	  3, 	 3,	  3,   0,
                              0,	   0,	  0,    4,	 4, 	4, 	 4,	  4,   0,
                              0,	   0,	  0,    5,	 5, 	5,   5,	  5,   0,
                              0,	   0,	  0,    6,	 6, 	6,   6,	  6,   0,
                            0.5,	 0.5,	0.5, 	0.5, 0.5,	0.5, 0.5, 0.5, 0.5];

          SrcRect := TRect.Create(3,2, A.Cols-1, A.Rows-1 );
          DstRect := TRect.Create(0,0, SrcRect.Width, SrcRect.Height);
          KernelAnchor := TPoint.Create(H.Rows div 2, H.Cols div 2); //center element

          B.Size(DstRect.Height, DstRect.Width); //could be bigger also

          B.Filter2D( A, H, SrcRect, DstRect, KernelAnchor);

    //      B = [[  7.5,	10.5,	 10.5,	10.5,	  7.5],  //top left element at index A[2,3]
    //           [ 12	 ,  18	,  18	 ,  18	,  12  ],
    //           [ 18	 ,  27	,  27	 ,  27	,  18  ],
    //           [ 24	 ,  36	,  36	 ,  36	,  24  ],
    //           [ 30	 ,  45	,  45	 ,  45	,  30  ],
    //           [ 23.5,	34.5,	 34.5,	34.5,	 23.5]]

          FreeIt(H,A,B);
      end;
      </code>
      </Example>*)

    function Filter2D(const Src, Kernel: TMtx; const SrcRect, DstRect: TMtxRect; const KernelAnchor: TMtxPoint; const Buffer: TVecInt = nil): TMtx; overload;

    (*<summary>Applies 2D filtering.</summary>
        
<remarks>The result is stored in to the calling object. The size of the calling object is set to match SrcRect dimensions.

       Important: The allocated size of the destination will never be overwritten, but the value of samples on the edge of the Dst matrix
       could be computed from elements outside of the Src thus making them appear to be random, because uninitialized memory was being
       referenced, if the parameters are not set carefully.
</remarks>
*)

    function Filter2D(const Src, Kernel: TMtx; const SrcRect: TMtxRect; const KernelAnchor: TMtxPoint; const Buffer: TVecInt = nil): TMtx; overload;

    (*<summary>Applies 2D filtering.</summary>
        
<remarks>The function will assume that Src has a border which is (Kernel.Cols div 2+1) wide on left and right and
        border which is (Kernel.Rows div 2 + 1) high on top and bottom. Kernel will be applied with its center element
        as the anchor. The filtered result will be stored in to the calling object without the extra borders. The size
        of the calling object is set automatically.

        It is the users responsability to initialize the border elements in the Src.
</remarks>
*)

    function Filter2D(const Src, Kernel: TMtx; const Buffer: TVecInt = nil): TMtx; overload;

    (*<summary>Inverse parallel in-place 1D FFT.</summary>
      
<remarks>Performs inverse 1D FFT on each row of the matrix. See <see cref="TVec.IFFT"/> for
      more info on the inverse FFT. NoScale parameter allows the scaling
      to be turned off. The scaling scales the result by 1/(Length_Of_FFT).
</remarks>
*)
    function IFFT1D(NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse parallel not in-place 1D FFT.</summary>
       
<remarks>The size of the destination is set automatically.
</remarks>
*)
    function IFFT1D(const Mtx: TMtx; NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse parallel in-place 1D FFT from complex to real.</summary>
      
<remarks>Performs inverse 1D FFT from complex to real on each row of the matrix. See <see cref="TVec.IFFTToReal"/> for
      more info on the inverse complex to real 1D FFT. NoScale parameter allows the scaling
      to be turned off. The scaling scales the result by 1/(Length_Of_FFT)
</remarks>
*)
    function IFFT1DToReal(NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse parallel not in-place 1D complex to real FFT.</summary>
      
<remarks>The size of the destination is set automatically.
</remarks>
*)
    function IFFT1DToReal(const Mtx: TMtx; NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse two-dimensional Fast Fourier Transformation from complex to complex.</summary>
      
<remarks>Transformation is applied in-place. Source matrix must be complex. If source matrix is real,
      an exception will be raised.
      result matrix will be complex. Size of the calling matrix is not changed.
      NoScale parameter allows the scaling to be turned off.
</remarks>


      <Example>
      <code>
      var a: TMtx;
      begin
        CreateIt(a);
        try
          a.SetIt (2,4,False,
                   [1, 2, 3, 4,
                   -5, 6,-7, 8] );
          a.ExtendToComplex;

          a.IFFT2D;
          // result will be
          [(1.5, 0), (0, -0.5), (-3.5, 0),  (0, 0.5),
             (1, 0), (-0.5, 0),    (3, 0), (-0.5, 0) ]

        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function IFFT2D(NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse two-dimentional Fast Fourier Transformation from complex to complex.</summary>
      
<remarks>Transformation is applied on Mtx matrix and results are saved in the calling matrix.
      The source matrix must be complex. If source matrix is real, an exception will be raised.
      The source matrix is not changed. Calling matrix will be complex and will have the same size as
      the source matrix. NoScale parameter allows the scaling to be turned off.
</remarks>


      <Example>
      <code>
      var a,b: TMtx;
      begin
        CreateIt(a,b);
        try
          a.SetIt (2,4,False,
                   [1, 2, 3, 4,
                   -5, 6,-7, 8] );
          a.ExtendToComplex;

          b.IFFT2D(a);
          // result will be
          [(1.5, 0), (0, -0.5), (-3.5, 0),  (0, 0.5),
             (1, 0), (-0.5, 0),    (3, 0), (-0.5, 0) ]

        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2DToReal"/>*)
    function IFFT2D(const Mtx: TMtx; NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse two-dimentional Fast Fourier Transformation from complex to real.</summary>
      
<remarks>Transformation is applied in-place. Source matrix Complex property must be false, if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfPack or to fsfPack.
      Source matrix Complex property must be True, if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfCCS.
      The calling matrix becomes real. Size of the calling matrix is not changed if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfPack or to fsfPack.
      Size of calling matrix will be set to <c>[Rows, 2*Cols]</c>, if <see cref="TDenseMtxVec.FFTStorageFormat"/> is set to fsfCCS,
      i.e. matrix changes <see cref="TMtxVec.Complex">Complex</see> property, but does not reallocate storage for elements.
      All elements are involved into inverse transformation, but number of reconstructed elements depends on the property <see cref="TDenseMtxVec.FFTStorageFormat"/>.
      If that property is set to fsfPerm or to fsfPack, then all elements will be reconstructed after inverse transformation.
      If that property is set to fsfCCS (which is default), then the number of element's which will be reconstructed depends on the
      property <see cref="TDenseMtxVec.FFTOddLength"/> and these rules will be followed:
      * Number of source columns and rows must be more than one. Exception will be raised otherwise;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to False, elements in range [0..Cols-2] x [0..Rows-2] will be reconstructed;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to True, elements in range [0..Cols-1] x [0..Rows-2] will be reconstructed;
      * Here Cols and Rows are the number of columns and rows of the calling matrix after the inverse transformation.

      Note
        Both properties <see cref="TDenseMtxVec.FFTStorageFormat"/> and <see cref="TDenseMtxVec.FFTOddLength"/> must be set to the same values for forward and inverse
        transformation to get reversible results.
        NoScale parameter allows the scaling to be turned off.
</remarks>


      <Example>
      <code>
      var a: TMtx;
      begin
        CreateIt(a);
        try
          a.SetIt (4,4,False,
                   [1,  2,  3,  4,
                   -5,  6, -7,  8,
                    9, 12,-11, 10,
                   16,-15,-14,  0]);

          a.FFTStorageFormat := fsfCCS;
          a.FFTOddLength := TRUE;

          // the next elements are involved into transformation
              [1, 2, 3,
              -5, 6, -7]

          a.FFT2DFromReal;
          // result will be
          // [(0,0), (-6, -10.39),
          //  (0,0), (3, 12.12),
          //  (12,12), (-11,10),
          //  (0,-15), (-14,0)]

          a.IFFT2DToReal;
          // [ 1,  2,  3, 0
          //  -5,  6, -7, 0
          //  12, 12,-11, 10
          //   0,-15,-14, 0]

          // the next elements are reconstructed
          // [ 1, 2, 3,  undef,
          //  -5, 6, -7, undef,
          //  undef, undef, undef, undef,
          //  undef, undef, undef, undef]
          //
          // if a.FFTOddLength is set to False,
          // then range of reconstruction would be one column less, i.e.
          // [ 1, 2, undef, undef,
          //  -5, 6, undef, undef,
          //  undef, undef, undef, undef,
          //  undef, undef, undef, undef]

        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2D"/>*)
    function IFFT2DToReal(NoScale: boolean = False): TMtx; overload;

    (*<summary>Inverse two-dimensional Fast Fourier Transformation from complex to real.</summary>
      
<remarks>Transformation is applied on source Mtx matrix and results are saved in the calling matrix.
      Source matrix must be real, if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfPack or to fsfPack.
      Check the storage format requirements to see how the complex numbers are to be stored in a real matrix.
      Source matrix must be complex, if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfCCS.
      The source matrix will not be changed. The calling matrix becomes real.
      Size of the calling matrix will be set to [Mtx.Rows,Mtx.Cols], if <see cref="TDenseMtxVec.FFTStorageFormat"/> was set to fsfPack or to fsfPack.
      Size of calling matrix will be set to [Mtx.Rows, 2*Mtx.Cols], if <see cref="TDenseMtxVec.FFTStorageFormat"/> is set to fsfCCS.
      All element are involved into the inverse transformation, but the number of reconstructed elements depends on the property <see cref="TDenseMtxVec.FFTStorageFormat"/>.
      If that property is set to fsfPerm or to fsfPack, then all elements will be reconstructed after inverse transformation.
      If that property is set to fsfCCS (which is default), then the number of elements which will be reconstructed depends on the property
      <see cref="TDenseMtxVec.FFTOddLength"/> and these rules will be followed:
      * Number of source columns and rows must be more than one. An exception will be raised otherwise;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to False, elements in range [0..Cols-2] x [0..Rows-2] will be reconstructed;
      * if <see cref="TDenseMtxVec.FFTOddLength"/> is set to True, elements in range [0..Cols-1] x [0..Rows-2] will be reconstructed;
        Here Cols and Rows are the number of columns and rows of the calling matrix after the inverse transformation.

      Note
        Both properties <see cref="TDenseMtxVec.FFTStorageFormat"/> and <see cref="TDenseMtxVec.FFTOddLength"/> must be set to the same values for forward and inverse transformation to get reversible results.
        NoScale parameter allows the scaling to be turned off.
</remarks>


      <SeeAlso cref="FFT2D"/>
      <SeeAlso cref="FFT2DFromReal"/>
      <SeeAlso cref="IFFT2D"/>*)
    function IFFT2DToReal(const Mtx: TMtx; NoScale: boolean = False): TMtx; overload;

    (*<summary>Calculates matrix inverse (Mtx^-1).</summary>
      
<remarks>Calculate the inverse (Mtx^-1) of the calling matrix. If the calling matrix is not <see cref="Quadratic"/>,
      an exception is raised. Parameter <see cref="TMtxType"/> determines which optimized method will
      be used for calculating the inverse matrix. If MtxType is omitted, the default value mtGeneral
      (general quadratic matrix) is used.

      Note
        The MtxType parameter is not verified. To determine if the calling matrix type is actually MtxType,
        use the <see cref="DetectMtxType"/> method.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);  // 2x2, real matrix
          A.Inv;
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="DetectMtxType"/>
      <SeeAlso cref="MtxError"/>*)
    function Inv(): TMtx; reintroduce; overload;

    (*<summary>Compute inverse of the matrix.</summary>
      
<remarks>Specify the matrix type to select the most performance efficient algorithm.
</remarks>
*)
    function Inv(MtxType: TMtxType): TMtx; overload;

    (*<summary>The inverse of matrix elements.</summary>
      
<remarks>Calculates the inverse of all matrix elements in place. The computation occurs after first limiting the
      magnitude of each elements by the lower bound of Treshhold. The limiting operation is performed to avoid
      division by zero. Since Treshold represents a magnitude, it is always real and must always be positive.
      For complex versions, the magnitude of the input is limited, but the phase remains unchanged. Zero-valued
      input is assumed to have zero phase. To bypass the limiting operation set the Threshold to zero.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);  // 2x2, not complex matrix
          A.InvElem(1.0e-7);
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="MulElem"/>*)
    function InvElem(): TMtx; overload;
    (*<summary>Invert all Mtx elements and store them in the calling matrix.</summary>*)
    function InvElem(Threshold: double): TMtx; overload;
    (*<summary>Convert all Mtx elements</summary>
      
<remarks>Xtore them in the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are set implicitly
      to match Mtx matrix. If Treshold is omitted then the default value 1/MAXNUM is used.
</remarks>
*)
    function InvElem(const SrcMtx: TMtx; const Threshold: double = 1/MAXNUM): TMtx; overload;

    (*<summary>Construct the Kac (Clement) matrix.</summary>
      
<remarks>Constructs the so called Kac (Clement) (n+1)x(n+1) tridiagonal matrix, defined by:

      <c>| 0, n, 0, ...    ,  0 |</c><para/>
      <c>| 1, 0, n-1, 0, ..., 0 |</c><para/>
      <c>| 0, 2, 0, n-2, 0, ... |</c><para/>
      <c>| ...........n-1, 0, 1 |</c><para/>
      <c>| 0, ... ........, n, 0|</c><para/>
      The eigenvalues of so defined matrix are 2k-n for k=0,1,2,...,n.
</remarks>
*)
    function Kac(n: Integer): TMtx;

    (*<summary>The Kronecker product between two vectors.</summary>
      
<remarks>Calculates the Kronecker product between Vec1 and Vec2 and stores the result in the
      calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see>
      properties of calling matrix are set automatically.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          V1,V2: TVec;
      begin
        CreateIt(A);
        CreateIt(V1,V2);
        try
          V1.SetIt(False,[1,2,3]);
          V2.SetIt(False,[4,5,6]);
          A.Kron(V1,V2);
          // A becomes:
          // [4   5  6]
          // [8  10 12]
          // [12 15 18]
        finally
          FreeIt(A);
          FreeIt(V1,V2);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="TVec.Kron"/>*)
    function Kron(const Vec1, Vec2: TVec): TMtx; overload;
    (*<summary>The Kronecker product between two matrices.</summary>
      
<remarks>Calculates the Kronecker product between SrcMtx1 and SrcMtx2 and stores the result in the
      calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see>
      properties of calling matrix are set automatically.
</remarks>
*)
    function Kron(const SrcMtx1, SrcMtx2: TMtx): TMtx; overload;

    
    function LMinQR(const L: TMtx; Q: TMtx; const R: TMtx): TMtx; overload;
    

    (*<summary>Constructs lower triangular matrix.</summary>
      
<remarks>The method uses Mtx matrix to construct a lower triangular matrix. The results are stored in the calling matrix.
      If the ZeroLower parameter is true then the calling matrix superdiagonal elements will be set to zero - otherwise
      the superdiagonal elements will not be initialized. If the Diagonal boolean parameter is true then the Mtx matrix
      main diagonal elements will be copied to the calling matrix main diagonal elements. If the Diagonal parameter is
      false, the calling matrix main diagonal elements will be set to zero.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        try
            A.SetIt(2,1,True,[1,2,
                              2,4]);  // 2x2, not complex matrix
            B.LowerTriangle(A,True,True);
            // B becomes:
            //  [1,0,
            //  [2,4]
        finally
            FreeIt(A,B);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="UpperTriangle"/>*)
    function LowerTriangle(const Mtx: TMtx; ZeroUpper, Diagonal: boolean): TMtx; overload;

    (*<summary>QR or LQ factorization.</summary>
      
<remarks>Performs QR or LQ factorization. If <see cref="Rows"/> is bigger or equal than <see cref="Cols"/>
      for the calling matrix, the linear system of  equations is overdetermined and the method performs QR factorization.
      The  matrix L is not used and can be nil. If Rows is smaller than Cols, the linear system equations is
      underdetermined and the method performs LQ factorization. The matrix R is not used and can be nil. If the
      Matrix Q is nil, then it's not explicitly formed. If the pointer is not nil, the full size Q is computed.
      If you do not want economy size L, Q and R, set MinSize to false. The calling matrix must have full rank.
      If the rank is not full, use the <see cref="SVD"/> method. If the Minimized parameter is false, the following
      holds true:

      <code>
      A = Q*R

      A = L*Q
      </code>
</remarks>


      <Example>
      <code>
      var L,Q,R,A: TMtx;
      begin
        CreateIt(Q,R,A);
        try
          A.SetIt(3,2,False,[1,2,
                              3,4,
                             3,3]);
          A.LQR(nil,Q,R,false);
        finally
          FreeIt(Q,R,A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SVD"/>
      <SeeAlso cref="LQRSolve"/>*)
    function LQR(const L, Q, R: TMtx; Minimized: boolean = true): TMtx; overload;
    (*<summary>QR or LQ factorization.</summary>
      
<remarks>Performs QR factorization with optional pivoting or LQ factorization. The pivoting of columns
      in the calling matrix is performed only when P is not nil and Rows &gt; Cols.

      The P[i] holds the permutation of columns in A, so that:
      <code>

      P(A) = Q*R

      </code>
</remarks>
*)
    function LQR(const L: TMtx; Q: TMtx; const R: TMtx; const P: TVecInt; Minimized: boolean = true): TMtx; overload;

    (*<summary>Solve overdetermined or underdetermined system of real linear equations.</summary>
      
<remarks>Solve overdetermined or underdetermined real linear systems involving a Rows-by-Cols matrix or its transpose,
      using a QR or LQ factorization of the calling matrix. It is assumed that the calling matrix has full rank.
      The following options are provided:

      <b>1.</b>If Op = opNone and m &gt;= n:  find the least squares solution of an overdetermined system, i.e., solve
      the least squares problem<para/>
      <c>minimize || B - A*X ||.</c><para/>
      <b>2.</b>If Op = opNone and m &lt; n:  find the minimum norm solution of an underdetermined system<para/>
      <c>A * X = B.</c><para/>
      <b>3.</b>If Op = opTrans and m &gt;= n:  find the minimum norm solution of an undetermined system<para/>
      <c>A**T * X = B.</c><para/>
      <b>4.</b>If Op = opTran and m &lt; n:  find the least squares solution of an overdetermined system, i.e., solve
      the least squares problem<para/>
      <c>minimize || B - A**T * X ||</c><para/>
      <b>5.</b>If Op = opHerm and m &gt;= n:  find the minimum norm solution of an undetermined system<para/>
      <c>A**H * X = B.</c><para/>
      <b>6.</b>If Op = opHerm and m &lt; n:  find the least squares solution of an overdetermined system, i.e., solve
      the least squares problem<para/>
      <c>minimize || B - A**H * X ||.</c><para/>

      Note
        If the parameters are of TVec type (vectors), the routine requires less pre and post processing.
</remarks>


      <SeeAlso cref="LQR"/>
      <SeeAlso cref="SVDSolve"/>
      <SeeAlso cref="MtxError"/>*)
    function LQRSolve(const B, X: TMtx; const R: TMtx = nil; Op: TMtxOperation = opNone): TMtx; overload;
    (*<summary>Matrix version of LQRSolve. Perfroms a LQRSolve for each B and X matrices columns in single pass.</summary>*)
    function LQRSolve(const B, X: TVec; const R: TMtx = nil;Op: TMtxOperation = opNone): TMtx; overload;

    (*<summary>Rank deficient version of LQRSolve. Perfroms LQRSolve for B vector.  </summary>
      
<remarks>Computes the minimum-norm solution to a linear least squares problem:
      <code>
      minimize || A * X - B ||
      </code>
      using a complete orthogonal factorization of A. A is an M-by-N  matrix which may be rank-deficient.

      The function returns the effective rank of the matrix A.  The effective rank is determined with the rcond parameter.
      A is factorized in such a way that condition number of leading submatrix of A will be less then 1/rcond. Set value
      of rcond to 1E-6 for less strict and to 1E-3 for more strict stability conditition.
      If R is assigned, the function returns the factorization of matrix A.
      Op parameter specified the operation to be applied to A before the computation.
</remarks>
*)
    function LQRSolve(const B, X: TVec; rcond: double; const R: TMtx = nil;  Op: TMtxOperation = opNone): integer; overload;

    (*<summary>Rank deficient version of LQRSolve. Perfroms LQRSolve for each B column in one pass.  </summary>
      
<remarks>Computes the minimum-norm solution to a linear least squares problem:
      <code>
      minimize || A * X - B ||
      </code>
      using a complete orthogonal factorization of A. A is an M-by-N  matrix which may be rank-deficient.

      The function returns the effective rank of the matrix A.  The effective rank is determined with the rcond parameter.
      A is factorized in such a way that condition number of leading submatrix of A will be less then 1/rcond. Set value
      of rcond to 1E-6 for less strict and to 1E-3 for more strict stability conditition.
      If R is assigned, the function returns the factorization of matrix A.
      Op parameter specified the operation to be applied to A before the computation.
</remarks>
*)
    function LQRSolve(const B, X: TMtx; rcond: double; const R: TMtx = nil;  Op: TMtxOperation = opNone): integer; overload;

    (*<summary>LU, Cholesky or Bunch-Kaufmann factorization.</summary>
      
<remarks>Performs a general LU, Cholesky or Bunch-Kaufmann factorization on the calling matrix and stores the results in lower
      triangular matrix L and upper triangular matrix U. The MtxType parameter defines which optimized method will be used to
      calculate the LU factorization. Depending on the type of the calling matrix the LU method will use specific optimized
      algorithm to perform the factorization. If you don't know the <see cref="TMtxType"/> of the calling matrix, you
      can omit the MtxType parameter (the default value mtGeneral will be used) or determine the type of matrix with the
      <see cref="DetectMtxType"/> method. The following methods can be used to calculate the LU factorization:
      * Matrix type is mtSymmPosDef, mtHermPosDef, mtBandSymmPosDef, mtBandHermPosDef : Cholesky factorization.
      * mtSymmetric : Bunch-Kaufmann factorization.
      * mtHermitian : Bunch-Kaufmann factorization (only for complex calling matrix).
      * mtTriangle : <See xref="LowerTriangle"/> or <see cref="UpperTriangle"/> method.
      * mtGeneral, mtBandGeneral : general m x n matrix LU factorization.

      Note
        An exception will be raised if the calling matrix <see cref="Quadratic"/> property is not true and
        matrix storage format is not banded.
</remarks>


      <Example>
      <code>
      var LU,A: TMtx;
          P: TVecInt;
      begin
        CreateIt(LU,A);
        CreateIt(P);
        try
          A.SetIt(3,2,False,[1,2,
                             3,4]);
          A.LU(LU,P);
        finally
          FreeIt(LU,A);
          FreeIt(P);
        end;
      end;
      </code>
      </Example>
      <SeeAlso cref="LUSolve"/>*)
    function LU(const Dst: TMtx; const P: TVecInt; MtxType: TMtxType = mtGeneral): TMtx;

    (*<summary>Solves system of linear equations by using LU factorization.</summary>
      
<remarks>Uses the LU factorization to solve the system of linear equations. <c>A X = B</c>. The matrix must be full rank. If there are more
      rows than columns use the least square solver <see cref="LQRSolve"/> and if the matrix is also rank deficient use the
      <see cref="SVDSolve"/> method. MtxType allows the selection of an optimized algorithm and Op defines the operation to be
      performed on the calling matrix prior to solve.

      LUSolve also supports banded matrices. The banded matrix storage is defined with the help of two additional properties:
      <see cref="SubDiag"/> and <see cref="SuperDiag"/>. SubDiag defines the number of non-zero subdiagonals and the
      SuperDiag the number of non-zero super diagonals. An example of the storage format for the first sub and super diagonal:

      <code>
      A.SubDiag := 1;
      A.SuperDiag := 1;
      A.Size(3,6);
      // ...
      [0 , ud2, ud3, ud4 ,ud5 ,ud6]   first upper diagonal
      [md1, md2, md3, md4, md5, md6]      main diagonal
      [ld1, ld2, ld3, ld4, ld5,  0] first lower diagonal
      </code>

      The columns must be aligned. All the diagonals between the SubDiag and SuperDiag diagonals including the main diagonal must
      always be included. Similarly you can define two sub/super diagonal storage format:

      <code>
      aXY =   (X Row index, Y Column index)

      [a11,  a12,  a13,      0,     0,     0]
      [a21,  a22,  a23,  a24,     0,     0]
      [a31,  a32,  a33,  a34,  a35,     0]
      [0,      a42,  a43,  a44,  a45, a46]
      [0,          0,  a53,  a54,  a55, a56]
      [0,          0,      0,  a64,  a65, a66]

      A.SubDiag := 2;
      A.SuperDiag := 2;
      A.Size(5,6);   // 5 here is the number of rows for the banded, not for the dense matrix storage format

      [0   ,  0,  a13, a24 ,a35, a46]    second upper diagonal
      [0   , a12, a23, a34, a45, a56]     first upper diagonal
      [a11 , a22, a33, a44, a55, a66]      main diagonal
      [a21 , a32, a43, a54, a65,   0]     first lower diagonal
      [a31 , a42, a53, a64,   0,   0]     second lower diagonal

      </code><para/>

      If you would like to solve X for several different B vectors (from the formula AX= B), you can pass TMtx
      objects to LUSolve method. With one call you solve the system for several different B vectors and save
      time.
</remarks>


      <SeeAlso cref="LU"/>
      <SeeAlso cref="MtxError"/>
      <SeeAlso cref="RefineSolution"/>
      <SeeAlso cref="ForwError"/>
      <SeeAlso cref="BackError"/>
      <SeeAlso cref="ConditionNr"/>
      <SeeAlso cref="ConditionNumber"/>*)
    function LUSolve(const B, X: TVec; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone): TMtx; overload;

    (*<summary>Matrix version of LUSolve. Perfroms a LUSolve for each B and X matrices columns in single pass.</summary>*)
    function LUSolve(const B,X: TMtx; MtxType: TMtxType = mtGeneral; Operation: TMtxOperation = opNone): TMtx; overload;  
    (*<summary>Performs factorization for LUSolve. </summary>
               
<remarks>Mtx, origMtx and ipiv contain result of factorization on exit.
               This result is again to be passed to the LUSolve together with B to obtain solution for X.
</remarks>


      <Example>
      <code>
      var LU,A, W1, W2: TMtx;
          P: TVecInt;
          B,X: TVec;
      begin
        CreateIt(LU, A, W1, W2);
        CreateIt(P);
        CreateIt(B,X);
        try
          A.RefineSolution := True; //it is False by default
          A.SetIt(2,2,False,[1,2,
                             3,4]);

          B.SetIt(2,false, [1,
                            0 ]);

          //Perform factorization:
          A.LUSolve(mtGeneral, W1, W2, P);  //OrigMtx param can be nil, if A.RefineSolution = false

          //Perform solution with given factorization:
          A.LUSolve(B, X, mtGeneral, opNone, W1, W2, P);  //X now holds solution
        finally
          FreeIt(LU,A, W1, W2);
          FreeIt(B,X);
          FreeIt(P);
        end;
      end;
      </code>
      </Example>*)
    function LUSolve(MtxType: TMtxType; const Mtx, OrigMtx: TMtx; const pipiv: TVecInt): TMtx; overload;
    (*<summary>Finds solution with an already precomputed factorization. </summary>
               
<remarks>Mtx, origMtx and ipiv contain result of factorization on exit. The factorization was obtained with a previous call to LUSolve, which did not require B and X params.
</remarks>
*)
    function LUSolve(const B, X: TVec; MtxType: TMtxType;  Operation: TMtxOperation; const Mtx, OrigMtx: TMtx; const pipiv: TVecInt): TMtx; overload;
    (*<summary>Finds solution with an already precomputed factorzation. </summary>
               
<remarks>Mtx, origMtx and ipiv contain result of factorization on exit. The factorization was obtained with a previous call to LUSolve, which did not require B and X params.
</remarks>
*)
    function LUSolve(const B,X: TMtx; MtxType: TMtxType;  Operation: TMtxOperation; const Mtx, OrigMtx: TMtx; const pipiv: TVecInt): TMtx; overload;

    (*<summary>Calculates the mean value of each of the matrix columns.</summary>
      
<remarks>Calculate the mean vale of each of the calling matrix columns and store the results in result vector.
      The <See  ref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the
      result vector are adjusted automatically.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          res: TVec;
      begin
        A := TMtx.Create;
        res := TVec.Create;
        try
          A.SetIt(2,2,false,[1,5,
                            -3,6]);
          A.MeanCols(res); // res = (-1.0, 5.5)
        finally
          A.Free;
          res.Free;
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="MeanRows"/>*)
    procedure MeanCols(const result: TVec); overload;

    (*<summary>Calculates the mean value of each of the matrix rows.</summary>
      
<remarks>Calculate the mean vale of each of the calling matrix rows and store the results in result vector.
      The <See  ref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the
      result vector are adjusted automatically.
</remarks>


      <Example>
      <code>
      var A: TMtx;
          res: TVec;
      begin
        A := TMtx.Create;
        res := TVec.Create;
        try
          A.SetIt(2,2,false,[1,5,
                            -3,6]);
          A.MeanRows(res); // res = (3, 1.5)
        finally
          A.Free;
          res.Free;
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="MeanCols"/>*)
    procedure MeanRows(const result: TVec); overload;

    (*<summary>Compute matrix function as a function of another matrix.</summary>
      
<remarks>The method allows you to compute any function as a function of the Src matrix. For example, a square of root of the matrix would not
      compute the square root of the elements of the matrix, but instead, the result would be such, that the product of two resulting
      matrices would return the original matrix. The Params parameters allow you to specify aditional parameter for the  Func function.

      Because the method is based on the eigenvalue decomposition and the eigenvalue, the SourceType for symmetric and symmetric positive
      definite matrices has to be defined explicitly.
</remarks>


      <Example>The MtxPower function declaration:
      <code>
      function TMtx.MtxPower(const Src: TMtx; Exponent: TCplx; SourceType: TMtxType = mtGeneral): TMtx;
      begin
        result := MtxFunction(Src,[Exponent],@DoToPower,SourceType);
      end;
      </code>
      </Example>

      <SeeAlso cref="TMtxFunction"/>*)
    function MtxFunction(const Src: TMtx; const Params: array of TCplx; Func: TMtxFunction = nil;
                                          SourceType: TMtxType = mtGeneral): TMtx; overload;

    (*<summary>Calculates the matrix to integer power.</summary>
      
<remarks>Calculate the matrix to any integer power. Because the <see cref="MtxFunction"/> method is based on the eigenvalue
      decomposition and the eigenvalue, the SourceType for symmetric and symmetric positive definite matrices has to be defined explicitly.

      Note
        The algorithm for general matrices will fail on a symmetric matrix.
</remarks>

        <SeeAlso cref="MtxPower"/>*)
    function MtxIntPower(const Src: TMtx; Exponent: Integer): TMtx; overload;

    (*<summary>Calculates the matrix to any power, real or integer.</summary>
      
<remarks>Calculate the matrix to any power, real or integer. It can also be used to compute the square root of the matrix. Because the <see cref="MtxFunction"/>
      method is based on the eigenvalue decomposition and the eigenvalue, the SourceType for symmetric and symmetric positive definite matrices has to be
      defined explicitly.

      Note
        The algorithm for general matrices will fail on a symmetric matrix.
</remarks>

      <SeeAlso cref="MtxSqrt"/>
      <SeeAlso cref="MtxFunction"/>*)
    function MtxPower(const Src: TMtx; Exponent: double; SourceType: TMtxType = mtGeneral): TMtx; overload;
    (*<summary>Calculate the matrix to complex power Exponent.</summary>*)
    function MtxPower(const Src: TMtx; Exponent: TCplx; SourceType: TMtxType = mtGeneral): TMtx; overload;

    (*<summary>Calculates the square root of the matrix.</summary>
      
<remarks>Calculates the square root of the Src matrix and stores the results to calling matrix. The product of the result with itself will
      give the original matrix. Because the <see cref="MtxFunction"/> method is based on the eigenvalue decomposition and the eigenvalue,
      the SourceType for symmetric and symmetric positive definite matrices has to be defined explicitly.

      Note
        The algorithm for general matrices will fail on a symmetric matrix.
</remarks>


      <SeeAlso cref="MtxPower"/>
      <SeeAlso cref="MtxFunction"/>*)
    function MtxSqrt(const Src: TMtx; SourceType: TMtxType = mtGeneral): TMtx; overload;

    (*<summary>Matrix multiplication.</summary>
      
<remarks>Performs matrix multiplication. In most general case the matrix multiplication is defined by the following equation (result = calling matrix):

      <IMG name="mtx002"/><para/>

      where a and b are <see cref="TMtxVec.Alfa"/> and <see cref="TMtxVec.Beta"/> variables. The default values for a and b are Cplx(1,0)
      and Cplx(0,0), so the above
      equation is reduced to:

      <IMG name="mtx006"/><para/>

      The Operation1 and Operation2 indicate additional <see cref="TMtxOperation"/>, performed on Mtx1 and Mtx2 respectively.
      Default value for operation is opNone (no additional operation). The Mtx1Type and Mtx2Type parameters indicate the <see cref="TMtxType"/>
      of Mtx1 and Mtx2 matrices. Depending what type of matrices you are multiplying, the Mul method will choose most optimized multiplication method.
      So, choosing the correct values for Mtx1Type and Mtx2Type can significantly speed up the multiplication.<para/>
      This overloaded function performs a left side multiply operation B with A matrix and stores the results in the calling matrix. If Operation1
      and/or Operation2 parameters are specified, perform additional operation on A or B. If Operation1 and/or Operation2 parameters are omitted,
      the default values (no operation) will be used. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted automatically. An exception is raised is A Cols property does not match
      the B Rows property (matrix multiplication is not possible). An exception is raised, if <see cref="TMtxVec.Complex">Complex</see> property of A and B
      does not match.

      Note
        * If you are not sure, which <see cref="TMtxType"/> of matrix are you multiplying, you can use the mtGeneral type (general case).
        * You can also use the <see cref="DetectMtxType"/> method to determine the type of matrix.
        * The routine is multithreaded, if specified by the environment variable.
</remarks>



      <Example>
      <code>
      var  A,B,C: TMtx;
      begin
        CreateIt(A,B,C);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);
          B.SetIt(2,2,False,[1,2,
                             2,4]);
          C.Mul(A,B);
        finally
          FreeIt(A,B,C);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="MulElem"/>*)
    function Mul(const A, B: TMtx): TMtx; overload;

    (*<summary>Multiplies multiple matrices.</summary>
               
<remarks>Multiplies all matrices in mtxArray from left to right using matrix multiplication.
</remarks>
*)
    function MulArray(const mtxArray: array of TMtx): TMtx; overload;

    (*<summary>Multiplies multiple matrices.</summary>
               
<remarks>Multiplies all matrices in mtxArray from left to right using matrix multiplication.
               Specify "1" to request matrix transpose for the corresponding matrix in the second array.
               For complex matrices, specify "2" if conjugated transpose is needed.
</remarks>
*)
    function MulArray(const mtxArray: array of TMtx; const transpArray: array of integer): TMtx; overload;

    (*<summary>Initializes small matrix multiply according to lapacks dgemm api standard.</summary>

               
<remarks>Initializes compute of: C = alpha*opA(A)*opB(B)+ beta*C

               The routine generates code by using JIT (just in time compilation) to create a kernel, which is optimized for the specified parameters.
               To release existing kernel and create a new one call this routine again. The process of creating a kernel is about 1000x slower than multiplying
               two matrices with size 2x2. The JIT targets small matrices and will fallback to standard dgemm for large matrices.

               The routine will set the size the calling matrix to match the size of the result of the multiplication.

               The actual matrix multiplication is performed by calling TMtx.MulSmall

               The function returns true, if custom code kernel was created. It returns false, if the code was routed to the default dgemm/zgemm routine, because
               the two matrices are too big to expect an improvement.

               It is safe to call this method with the same parameters more than once. The internal kernel will not be recreated, if the parameters other than A and B
               have not changed.
</remarks>
*)
    function MulSmallInit(const A, B: TMtx; opA: TMtxOperation = opNone;  opB: TMtxOperation = opNone; const alpha: double = 1; const beta: double = 0): boolean; overload;
    (*<summary>Initializes small matrix multiply according to lapacks dgemm api standard.</summary>

               
<remarks>Initializes compute of: C = alpha*opA(A)*opB(B)+ beta*C

               The routine generates code by using JIT (just in time compilation) to create a kernel, which is optimized for the specified parameters.
               To release existing kernel and create a new one call this routine again. The process of creating a kernel is about 1000x slower than multiplying
               two matrices with size 2x2. The JIT targets small matrices and will fallback to standard dgemm for large matrices.

               The routine will set the size the calling matrix to match the size of the result of the multiplication.

               The actual matrix multiplication is performed by calling TMtx.MulSmall method.

               The function returns true, if custom code kernel was created. It returns false, if the code was routed to the default dgemm/zgemm routine, because
               the two matrices are too big to expect an improvement.

               It is safe to call this method with the same parameters more than once. The internal kernel will not be recreated, if the parameters other than A and B
               have not changed.
</remarks>
*)
    function MulSmallInit(const A, B: TMtx; opA: TMtxOperation;  opB: TMtxOperation; const alpha: TCplx; const beta: TCplx): boolean; overload;

    (*<summary>Small matrix multiply. Performs arbitrary small matrix multiplication.</summary>
               
<remarks>The routine does no error checking and assumes that both A and B matrix sizes, FloatPrecision and layout do not change between calls.
               The size of the calling matrix is set automatically.
               The kernel for the particular precision is initialized with a call to MulSmallInit and released either when a new one is created or when the calling matrix is freed.
</remarks>
*)
    function MulSmall(const A, B: TMtx): TMtx; overload; 
    (*<summary>Small matrix multiply. Performs arbitrary small matrix multiplication.</summary>
               
<remarks>The routine does no error checking and assumes that both A and B matrix and for the calling matrix, the sizes, FloatPrecision, the layout and the memory location (!) do not change between calls.
               The matrices multiplied are those passed to the MulSmallInit routine. You can change the values in the memory allocated by those matrices, but may not
               resize them or resize the calling matrix.

               The kernel for the particular configuration is initialized with a call to MulSmallInit and released either when a new configuration is created or when the calling matrix is freed.
</remarks>
*)
    function MulSmall: TMtx; overload;

    (*<summary>Left side multiply Mtx2 with Mtx1 matrix and store the results in the calling matrix.</summary>
      
<remarks>If Operation1 and/or Operation2 parameters are specified, perform additional operation on Mtx1 or Mtx2. If Operation1 and/or Operation2 parameters
      are omitted, the default values (no operation) will be used. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties
      of the calling matrix are adjusted automatically. An exception is raised is Mtx1 Cols property does not match the Mtx2 Rows property (matrix
      multiplication is not possible). An exception is raised, if Complex property of Mtx1 and Mtx2 does not match.

      When JITedMul is set to jtmEnabled, the JIT-ed matrix kernel will be created. This creation takes some time and should only be used, if the function will
      be called many times with the same parameters. The parameters that may not change include the size of the matrices, but matrix objects and memory locations can vary.
      As a rule of thumb, the change of the parameters can be 2-3x slower than using tmDisabled, but when the parameters do not change for a small matrix of 2x2 the speed-up can be 50x.
</remarks>
*)
    function Mul(const Mtx1, Mtx2: TMtx; Operation1: TMtxOperation;  Operation2: TMtxOperation = opNone; JITedMul: TJITedMul = jtmDisabled): TMtx; overload;
    (*<summary>Left side multiply Mtx2 with Mtx1 matrix and store the results in the calling matrix.</summary>
      
<remarks>If parameters Mtx1Type and/or Mtx2Type are specified, the optimized multiplication method will be used. If parameters Mtx1Type and/or Mtx2Type are omitted,
      default values (general matrix) will be used. If Operation1 and/or Operation2 parameters are specified, perform additional operation on Mtx1 or Mtx2. If Operation1 and/or
      Operation2 parameters are omitted, the default values (no operation) will be used. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted automatically. An exception is raised is Mtx1 Cols property does not match the Mtx2
      Rows property (matrix multiplication is not possible). An exception is raised, if Complex property of Mtx1 and Mtx2 does not match.

      When JITedMul is set to jtmEnabled, the JIT-ed matrix kernel will be created. This creation takes some time and should only be used, if the function will
      be called many times with the same parameters. The parameters that may not change include the size of the matrices, but matrix objects and memory locations can vary.
      As a rule of thumb, the change of the parameters can be 2-3x slower than using tmDisabled, but when the parameters do not change for a small matrix of 2x2 the speed-up can be 50x.
</remarks>
*)
    function Mul(const Mtx1, Mtx2: TMtx; Mtx1Type: TMtxType; Mtx2Type: TMtxType = mtGeneral; Operation1: TMtxOperation = opNone; Operation2: TMtxOperation = opNone; JITedMul: TJITedMul = jtmDisabled): TMtx; overload;

    (*<summary>Returns the matrix product of three matrices. </summary>
               
<remarks>Internally calls <see cref="TMtx.Mul"/> and does JIT-ed (faster) multiply only for small square sized matrices.
</remarks>
*)
    function Mul(const Mtx1, Mtx2, Mtx3: TMtx): TMtx; overload;

    (*<summary>Returns the matrix product of four matrices. </summary>
               
<remarks>Internally calls <see cref="TMtx.Mul"/> and does JIT-ed (faster) multiply only for small square sized matrices.
</remarks>
*)
    function Mul(const Mtx1, Mtx2, Mtx3, Mtx4: TMtx): TMtx; overload;

    (*<summary>Matrix element-wise multiplication.</summary>
      
<remarks>Multiplies elements in Mtx matrix with the elements in the calling matrix (array multiplication) and stores the results in calling matrix.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of both matrices must match, otherwise an exception is
      raised.
</remarks>


      <Example>
      <code>
      var  A,B,C: TMtx;
      begin
        CreateIt(A,B,C);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);
          B.SetIt(2,2,False,[1,2,
                             2,4]);
          C.MulElem(A,B);
          // C becomes:
          // [1, 4,
          //  4,16]
        finally
          FreeIt(A,B,C);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="InvElem"/>*)
    function MulElem(const Mtx: TMtx): TMtx; overload;
    (*<summary>Multiplies elements in Mtx1 matrix with the elements in Mtx2 matrix (array multiplication) and stores the results in calling matrix.</summary>
      
<remarks>The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of calling matrix are set implicitly to match those
      of Mtx1 and Mtx2 matrices. Mtx1 and Mtx2 Rows, Cols, and Complex properties must be the same, otherwise an excetion is raised.
      raised.
</remarks>
*)
    function MulElem(const Mtx1, Mtx2: TMtx): TMtx; overload;

    (*<summary> Compute X*Y*Z </summary>
       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*Y
</remarks>
*)
    function MulElem(const X, Y,Z: TMtxVec): TMtx; overload;
    (*<summary> Compute X*Y*zScalar </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*zScalar
</remarks>
*)
    function MulElem(const X,Y: TMtxVec; const Z: Double): TMtx; overload;
    (*<summary> Compute X*Y*zScalar </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*zScalar
</remarks>
*)
    function MulElem(const X,Y: TMtxVec; const Z: TCplx): TMtx; overload;

    (*<summary>Calculates the matrix norm-1.</summary>
      
<remarks>Calculates calling matrix norm-1. The "norm-1" is defined as the largest column sum.
</remarks>


      <Example>
      <code>
      var A: TMtx;
        b: double;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,false,[1,2,
                  2,4]);  // 2x2, not complex matrix
          b := A.Norm1;
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="NormInf"/>
      <SeeAlso cref="NormFro"/>*)
    function Norm1: double; overload;

    (*<summary>Calculates the matrix Frobenius norm.</summary>
      
<remarks>Calculate the calling matrix Frobenius norm. The Frobenius norm is defined as
      the square root of the sum of sums of squared elements of rows. An exception is raised
      if calling matrix <see cref="TMtxVec.Complex">Complex</see> property is true.
</remarks>


      <Example>
      <code>
      var A: TMtx;
        b: double;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,false,[1,2,
                  2,4]);  // 2x2, not complex matrix
          b := A.NormFro;
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="NormInf"/>
      <SeeAlso cref="Norm1"/>*)
    function NormFro: double; overload;

    (*<summary>Calculates the matrix infinity norm.</summary>
      
<remarks>Calculate the calling matrix infinity norm. The "infinity norm" is defined as the largest row sum.
</remarks>


      <Example>
      <code>
      var A: TMtx;
        b: double;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,false,[1,2,
                  2,4]);  // 2x2, not complex matrix
          b := A.NormInf; // b= 6
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="NormFro"/>
      <SeeAlso cref="Norm1"/>*)
    function NormInf: double; overload;

    (*<summary>Constructs a Pascal matrix.</summary>
      
<remarks>Construct a Pascal matrix of order Order with integer elements, taken from the Pascal's triangle. The Pascal matrix
      is by definition symmetric positive definite. The Pascal matrix is written to the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted automatically.
</remarks>
*)
    function Pascl(Order: integer): TMtx; overload;

    (*<summary>Raises matrix elements to any power.</summary>
      
<remarks>Raises Base matrix elements to the Exponent matrix elements power and stores the results in the calling matrix. The
      <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are
      adjusted automatically. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties
      of Base and Exponent matrices must match otherwise an exception is raised.

      Note
        Only positive exponents can be handled if Exponent matrix is not complex.
</remarks>


      <Example>
      <code>
      var  A,B,C: TMtx;
      begin
        CreateIt(A,B,C);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);
          B.SetIt(2,2,False,[1,2,
                             2,4]);
          C.Power(A,B);
        finally
          FreeIt(A,B,C);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function PowerMtx(const Base, Exponent: TMtx): TMtx; overload;

    (*<summary>Creates a reduced size dense matrix for screen display (bitmap) to show the matrix pattern.</summary>
      
<remarks>Creates a reduced size dense matrix for screen display (bitmap) to show the matrix pattern.
      Pixels parameter defines the target number of pixels to reduce <see cref="Rows"/> and <see cref="Cols"/> to.
      The size reduction method depends on what you want to see:
      * pdsPattern , Returns 1 for all areas with at least one nonzero value regardless of its amplitude.
      * pdsAverage, Average all values within an area (pixel).
      * pdsPeak, Return only the maximum absolute value within an area (region will become one pixel).
</remarks>


      <Example>
      <code>
      Uses ..., MtxVecTee;
      var Mtx, ReducedMtx: TMtx;
      begin
        CreateIt(Mtx,ReducedMtx);
        try
          Mtx.Size(1000,1000,false);
          Mtx.RandGauss;
          // we're interested in averages, not pattern
          Mtx.PixelDownSample(ReducedMtx,200,pdsAverage);
          DrawIt(ReducedMtx);
        finally
          FreeIt(Mtx,ReducedMtx);
        end;
      end;
      </code>
      7</Example>*)
    procedure PixelDownSample(const Dst: TMtx; Pixels: integer  = 500; Mode: TPixelDownSample = pdsPattern); overload;

    (*<summary>Allows to exactly specify the size of the destination matrix with PixelsHeight
       and PixelsWidth parameters.</summary>
       
<remarks>The source matrix can be bigger or smaller than the destination.
</remarks>
*)
    procedure PixelResample(const Dst: TMtx; const ZoomRect: TMtxRect; Mode: TPixelDownSample = pdsPattern); overload;

    
    
    function ReadHeader(const SrcStream: TStream; Endian: TEndianness = boLittleEndian): TPrecision; override;
    
    

    

    
    procedure Reset; override;
    

    (*<summary>Resizes the matrix, while preserving the values in already allocated memory.</summary>
      
<remarks>Sets the <see cref="Rows"/> property to NewRows,
      <see cref="Cols"/> property to NewCols,
      while preserving the values in already allocated memory.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,False,[1,3,
                             2,4]);
          B.Resize(A,3,2,True);
          // B becomes:
          // [1,2,0,
          //  3,4,0]);
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Size"/>*)
    function Resize(NewRows , NewCols: integer): TMtx; overload;

    (*<summary>Performs resize of the calling matrix to NewRows and NewCols.</summary>
      
<remarks>Copies the Src matrix values to the calling matrix.
      The elements outside the newly created matrix size are not referenced.
</remarks>
*)
    function Resize(const Src: TMtx; NewRows, NewCols: integer): TMtx; overload;

    (*<summary>Resize and transpose.</summary>
      
<remarks>Resizes the calling matrix to NewRows and NewCols and then copies
      the Src matrix values to the calling matrix. The elements outside
      the newly created matrix size are not referenced. At the end
      the method also transposes the Src matrix. All this operations
      are performed in one single pass.
</remarks>
*)
    function ResizeAndTranspose(const Src: TMtx; NewRows, NewCols: integer): TMtx;

    (*<summary>Rotates matrix rows 90 degrees clockwise.</summary>
      
<remarks>Rotates all calling matrix rows 90 degrees clockwise in-place (check the scheme bellow).

      <IMG name="mtx013"/><para/>

      Note
        This operation differs from the <see cref="Transp"/> operation.<para/>
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        A.SetIt(2,2,False,[1,3,
                           2,4]);
        B.Rotate90(A);
        FreeIt(A,B);
      end;
      </code>
      </Example>

      <SeeAlso cref="Transp"/>*)
    function Rotate90: TMtx; overload;
    (*<summary>Rotate all Mtx matrix rows 90deg clockwise.</summary>
      
<remarks>Store the results in calling matrix. The
      <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling
      matrix are adjusted automatically.
</remarks>
*)
    function Rotate90(const Mtx: TMtx): TMtx; overload;

    (*<summary>Exchanges two matrix rows.</summary>
      
<remarks>Exchange the i-th and j-th rows of the calling matrix in-place. An exception is raised if matrix bounds are
      overrun. The indexes i and j are zero based. (the first row has index 0).
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,False,[1,3,
                             2,4]);
          B.RowExchange(0,1);
          // B becomes:
          // [2,4,
          //  1,3]);
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ColExchange"/>*)
    function RowExchange(i, j: integer): TMtx; overload;

    (*<summary>Scale all matrix columns with values from Src.</summary>
              
<remarks>Src.Length must be equal to Self.Cols.
</remarks>
*)
    function ScaleCols(const Src: TVec): TMtx; overload;

    (*<summary>Scale all matrix rows with values from Src.</summary>
              
<remarks>Src.Length must be equal to Self.Rows.
</remarks>
*)
    function ScaleRows(const Src: TVec): TMtx; overload;

    (*<summary>Scale all Mtx matrix columns with values from Src.</summary>
              
<remarks>Src.Length must be equal to Mtx.Cols. The result is stored in to the calling matrix.
</remarks>
*)
    function ScaleCols(const Mtx: TMtx; const Src: TVec): TMtx; overload;

    (*<summary>Scale all Mtx matrix rows with values from Src.</summary>
              
<remarks>Src.Length must be equal to Mtx.Rows. The result is stored in to the calling matrix.
</remarks>
*)
    function ScaleRows(const Mtx: TMtx; const Src: TVec): TMtx; overload;

    (*<summary>Multiply matrix with diagonal matrix from the left.</summary>
               
<remarks>DiagMtx contains the values of the main diagional of the diagonal matrix.
               Other values of the DiagMtx matrix are assumed to be zero.
               DiagMtx.Length must be equal to Self.Cols. This operation is the same as calling TMtx.ScaleRows.
</remarks>
*)
    function MulDiagLeft(const DiagMtx: TVec): TMtx; overload;

    (*<summary>Multiply matrix with diagonal matrix from the right.</summary>
               
<remarks>DiagMtx contains the values of the main diagional of the diagonal matrix.
               Other values of the DiagMtx matrix are assumed to be zero.
               DiagMtx.Length must be equal to Self.Rows. This operation is the same as calling TMtx.ScaleCols.
</remarks>
*)
    function MulDiagRight(const DiagMtx: TVec): TMtx;  overload;

    (*<summary>Multiply matrix with diagonal matrix from the left.</summary>
               
<remarks>DiagMtx contains the values of the main diagional of the diagonal matrix.
               Other values of the DiagMtx matrix are assumed to be zero.
               DiagMtx.Length must be equal to Self.Cols. This operation is the same as calling TMtx.ScaleRows.
</remarks>
*)
    function MulDiagLeft(const DiagMtx: TVec; const Mtx: TMtx): TMtx;  overload;

    (*<summary>Multiply matrix with diagonal matrix from the right.</summary>
               
<remarks>DiagMtx contains the values of the main diagional of the diagonal matrix.
               Other values of the DiagMtx matrix are assumed to be zero.
               DiagMtx.Length must be equal to Self.Rows. This operation is the same as calling TMtx.ScaleCols.
</remarks>
*)
    function MulDiagRight(const Mtx: TMtx; const DiagMtx: TVec): TMtx; overload;

    (*<summary>Compute maximum of each row.</summary>
               
<remarks>Compute maximum of each row and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MaxRows(const Dst: TVec); overload;
    (*<summary>Compute minimum of each row.</summary>
               
<remarks>Compute mniimum of each row and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MinRows(const Dst: TVec); overload;
    (*<summary>Compute maximum of each column.</summary>
               
<remarks>Compute maximum of each column and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MaxCols(const Dst: TVec); overload;
    (*<summary>Compute minimum of each column.</summary>
               
<remarks>Compute mniimum of each column and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MinCols(const Dst: TVec); overload;

    (*<summary>Compute maximum and minimum of each row.</summary>
               
<remarks>Compute maximum and minimum of each row and store the result in to Dst vectors. The Dst's are sized automatically.
</remarks>
*)
    procedure MaxMinRows(const DstMaxRows, DstMinRows: TVec); overload;
    (*<summary>Compute maximum and minimum of each column.</summary>
               
<remarks>Compute maximum and minimum of each column and store the result in to Dst vectors. The Dst's are sized automatically.
</remarks>
*)
    procedure MaxMinCols(const DstMaxCols, DstMinCols: TVec);

    (*<summary>Copies values from vector to matrix column.</summary>
      
<remarks>Copy all Vec elements to the calling matrix Col column. The <see cref="TMtxVec.Complex">Complex</see> property of the calling
      matrix is adjusted automatically. An exception is raised, if array bounds are overrun.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
            V: TVec;
      begin
        CreateIt(A);
        CreateIt(V);
        try
          A.Size(2,1,True);
          V.SetIt(True,[1,2, 2,4]);  // complex vector
          A.SetCol(V,0);
        finally
          FreeIt(V);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetRow"/>*)
    function SetCol(const Vec: TDenseMtxVec; Col: integer): TMtx; overload;
    (*<summary>Copy vector Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements [Row,Col]..[Row+Len-1,Col].</summary>
      
<remarks>An exception is raised if condition checking is enabled and array bounds are overrun. An exception is raised, if Vec
      and the calling matrix complex properties do not match.
</remarks>
*)
    function SetCol(const Vec: TDenseMtxVec; VecIndex, Row, Col, Len: integer): TMtx; overload;

    (*<summary>Sets matrix values.</summary>
      
<remarks>Sets matrix values. This method gives you the possibility to pass large arrays of elements without having to declare constant arrays.

      Pass all elements in A array to the calling matrix. The <see cref="Rows"/> and <see cref="Cols"/> properties of the calling
      matrix are set to ARows and ACols and the <see cref="TMtxVec.Complex">Complex</see> property is set to AComplex. An exception is raised if the matrix size
      (ARows*ACols) does not match the number of complex numbers in A.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4]);
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetVal"/>*)
    function SetIt(ARows, ACols: integer; AComplex: Boolean;const A: array of double): TMtx; overload;

    (*<summary>Copies values from vector to matrix row.</summary>
      
<remarks>Copy all Vec elements to the calling matrix Row row. The <see cref="TMtxVec.Complex">Complex</see> property of the calling
      matrix is adjusted automatically. An exception is raised, if array bounds are overrun.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
            V: TVec;
      begin
        CreateIt(A);
        CreateIt(V);
        try
          A.Size(1,2,True);
          V.SetIt(True,[1,2, 2,4]);  // complex vector
          A.SetCol(V,0);
        finally
          FreeIt(V);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetCol"/>*)
    function SetRow(const Vec: TDenseMtxVec; Row: integer): TMtx; overload;
    (*<summary>Copy vector Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements [Row,Col]..[Row,Col+Len-1].</summary>
      
<remarks>An exception is raised if condition checking is enabled and array bounds are overrun. An exception is raised, if Vec
      and the calling matrix complex properties do not match.
</remarks>
*)
    function SetRow(const Vec: TDenseMtxVec; VecIndex, Row, Col, Len: integer): TMtx; overload;

    (*<summary>Initializes matrix values to zero.</summary>
      
<remarks>Sets the calling matrix elements [Row,Col]..[ToRow,ToCol] to zero. An exception is raised if
      <see cref="TMtxVecBase.ConditionCheck"/> is true and array bounds are overrun.
</remarks>
*)
    function SetZero(Row, Col, ToRow, ToCol: integer): TMtx; overload;

    (*<summary>Initializes matrix values to Value.</summary>
      
<remarks>Sets the calling matrix elements [Row,Col]..[ToRow,ToCol] to Value. An exception is raised if
      <see cref="TMtxVecBase.ConditionCheck"/> is true and array bounds are overrun.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: double; Row, Col, ToRow, ToCol: integer): TMtx; overload;
    (*<summary>Sets the calling matrix elements [Row,Col]..[ToRow,ToCol] to complex value Value.</summary>
      
<remarks><see cref="TMtxVec.Complex">Complex</see> properties of the calling object are set to true even before the call it was false.
      An exception is raised if array bounds are overrun.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: TCplx; Row, Col, ToRow, ToCol: integer): TMtx; overload;

    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols
      * and <see cref="TMtxVec.Complex">Complex</see> = AComplex
      * <see cref="TMtxVecBase.IsDouble"/> = aIsDouble

      Calling the Size method does not preserve the contents of the matrix.
      Use the Resize method, if you want to preserve existing values.

      Note
        The Size method performs an out-of-memory safe resize, if the matrix already has memory allocated. This prevents out
        of memory message for example when redefining the size of the matrix from single
        column to single row:

      <code>
      A.Rows := 10000; // matrix size = 0
      A.Cols := 1;     // matrix size = 10000
      // ...
      A.Cols := 10000; // matrix size = 100 000 000 (problems here)
      A.Rows := 1;     // matrix size = 10 000
      </code>
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.Size(2,1,True, mvDouble); // 2x1 complex matrix
          A.SetZero;
          // A becomes:
          // [0,0]
          // [0,0]
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Rows"/>
      <SeeAlso cref="Cols"/>
      <SeeAlso cref="TMtxVec.Complex"/>*)
    function Size(const ARows, ACols: integer; const aComplex: boolean; const aIsDouble: boolean): TMtx ; overload;

    
    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols
      * and <see cref="TMtxVec.Complex">Complex</see> = AComplex

      Calling the Size method does not preserve the contents of the matrix.
      Use the Resize method, if you want to preserve existing values.

      The existing floating point precision is preserved.
</remarks>


      <SeeAlso cref="Rows"/>
      <SeeAlso cref="Cols"/>
      <SeeAlso cref="TMtxVec.Complex"/>*)

    function Size(const ARows, ACols: integer; const aComplex: boolean): TMtx ; overload;

    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols
      * and <see cref="TMtxVec.Complex">Complex</see> = false

      Calling the Size method does not preserve the contents of the matrix.
      Use the Resize method, if you want to preserve existing values.

      The existing floating point precision is preserved.
</remarks>


      <SeeAlso cref="Rows"/>
      <SeeAlso cref="Cols"/>
      <SeeAlso cref="TMtxVec.Complex"/>*)
    function Size(const ARows, ACols: integer): TMtx ; overload;
    

    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols
      * and <see cref="TMtxVec.FloatPrecision" text="FloatPrecision"/> = aFloatPrecision.

      If the aFloatPrecision parameter is omitted, the default value mvDouble is used.
      Calling the Size method does not preserve the contents of the matrix.
      Use the Resize method, if you want to preserve existing values.

      Note
        The Size method performs an out-of-memory safe resize, if the matrix already has memory allocated. This prevents out
        of memory message for example when redefining the size of the matrix from single column to single row.
</remarks>
*)
    function Size(ARows, ACols: integer; aFloatPrecision: TMtxFloatPrecision): TMtx; overload;
    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols
      * and <see cref="TMtxVec.FloatPrecision" text="FloatPrecision"/> = FloatPrecisionRef.FloatPrecision
</remarks>
*)

    function Size(ARows, ACols: integer; const FloatPrecisionRef: TMtxVec): TMtx; overload;



    
    function Size(const Src: TMtxVecBase; AComplex: boolean): TMtxVec ; override;
    function Size(const Src: TMtxVecBase; const aFloatPrecision: TMtxFloatPrecision): TMtxVec; override;
    

    (*<summary>Sorts the elements in a matrix row(s) in ascending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in ascending order in-place. If the calling matrix is complex,
      then complex values are first compared by the absolute value and then by the argument.

      Note
        Each row is sorted independently from the other.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             4,2]);
          A.SortAscend;
          // A becomes:
          // [1,2]
          // [2,4]
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SortDescend"/>*)
    function SortAscend: TMtx; overload;
    (*<summary>Performs row based sort in ascending order. The Col parameter defines the column based on which the rows are compared with each other.</summary>*)
    function SortAscend(Col: integer): TMtx; overload; 

    (*<summary>Sorts the elements in a matrix row(s) in descending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in descending order in-place. If the calling matrix is complex,
      then complex values are first compared by the absolute value and then by the argument.<para/>

      Note
        Each row is sorted independently from the other.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             4,2]);
          A.SortDescend;
          // A becomes:
          // [2,1]
          // [4,2]
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SortAscend"/>*)
    function SortDescend: TMtx; overload;
    (*<summary>Performs row based sort in descending order. The Col parameter defines the column based on which the rows are compared with each other.</summary>*)
    function SortDescend(Col: integer): TMtx; overload; 

    (*<summary> Depreciated. </summary>
        
<remarks>Use <see cref="TMtxVec.Sqr"/> instead.
</remarks>
*)
    function SqrElem(const Mtx: TMtx): TMtx; overload; deprecated;
    (*<summary> Depreciated. </summary>
        
<remarks>Use <see cref="TMtxVec.Sqr"/> instead.
</remarks>
*)
    function SqrElem: TMtx; overload; deprecated;
    (*<summary> Depreciated. </summary>
        
<remarks>Use <see cref="TMtxVec.Sqrt"/> instead.
</remarks>
*)
    function SqrtElem(const Mtx: TMtx): TMtx; overload; deprecated;
    (*<summary> Depreciated. </summary>
        
<remarks>Use <see cref="TMtxVec.Sqrt"/> instead.
</remarks>
*)
    function SqrtElem: TMtx; overload; deprecated;

      (*<summary>Standard deviation for matrix columns.</summary>
        
<remarks>Calculates the standard deviation for each of the calling matrix columns and stores the
        results in vector result. An exception is raised if calling matrix is complex.
        The Length and Complex properties of the result vector are adjusted automatically.
</remarks>


        <SeeAlso cref="StdDevRows"/>
        <SeeAlso cref="MeanCols"/>*)
      procedure StdDevCols(result: TVec);
      (*<summary>Standard deviation for matrix rows.</summary>
        
<remarks>Calculates the standard deviation for each of the calling matrix rows and stores the
        results in vector result. An exception is raised if calling matrix is complex.
        The Length  and Complex properties of the result vector are adjusted automatically.
</remarks>


        <SeeAlso cref="StdDevCols"/>
        <SeeAlso cref="MeanRows"/>*)
      procedure StdDevRows(result: TVec);


    (*<summary>Convert strings to double (TCplx) and store them in the Values array.</summary>
      
<remarks>Convert strings in aList to double (real number) and store them in the Values array of the calling matrix.
      <see cref="Rows"/> property is set to aList.Count. <see cref="TMtxVec.Complex">Complex</see> and <see cref="Cols"/> propertes
      are auto-detected. Complex numbers must follow the format: a+bi. All strings must have the same number of columns
      (numbers). Columns must be separated with a aDelimiter.
</remarks>


      <Example>
      <code>
      var a,b: TMtx;
      begin
        CreateIt(a,b);
        try
          a.SetIt(2,2,False,[1,2,3,4]);
          a.Cos;
          b.Size(a);
          b.SetVal(1);
          a.Add(b);
          Richedit1.Clear;
          Memo1.Clear;
          a.ValuesToStrings(Richedit1.Lines);
          b.ValuesToStrings(Richedit1.Lines);
          a.ValuesToStrings(Memo1.Lines);
          b.ValuesToStrings(Memo1.Lines);
          Memo1.Lines.SaveToFile('C:\Test.txt');
          Memo1.Lines.LoadFromFile('C:\Test.txt');
          a.StringsToValues(Memo1.Lines);
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ValuesToStrings"/>*)
    function StringsToValues(const srcList: TStrings; const aDelimiter: string): TMtx; overload;

    (*<summary>Convert strings to double (TCplx) and store them in the Values array.</summary>
               
<remarks>Uses Tab (#9) as the default column separator.
</remarks>
*)
    function StringsToValues(const srcList: TStrings): TMtx; overload; virtual;

    (*<summary>Convert strings from [ListIndex]..[ListIndex+ListLen-1] in aList to double (TCplx).</summary>
      
<remarks>Store them in the Values array of the calling matrix, starting from element [Row,Cols]. The size of the calling matrix is not changed.
      If array bounds are overrun an exception is raised. Complex numbers must follow the format: a+bi.
</remarks>
*)
    function StringsToValues(const srcList: TStrings; ListIndex,ListLen, Row,Col: Integer; Delimiter: string = kTab): TMtx; overload;

    (*<summary>Singular value decomposition.</summary>
      
<remarks>Performs a singular value decomposition on a calling matrix (A). U or V can be nil, if not desired. If you do not want economy
      size U and V set MinSize to False. The singular value decomposition (SVD) of the calling matrix (A, with m rows and n cols) is
      defined by the following equation:

      <IMG name="mtx010"/><para/>

      where U and V are unitary (for complex A) or orthogonal (for real A). S is a diagonal matrix with real diagonal elements is:

      <IMG name="mtx011"/><para/>

      The diagonal elements are singular values of A (calling matrix). The first min(m, n) columns of the matrices U and V are,
      respectively, left and right singular vectors of calling matrix (A). The singular values and singular vectors satisfy:

      <IMG name="mtx012"/><para/>
      where ui and vi are the ith columns of U and V, respectively.

      One interpretation of the singular values is as follows. If you take a unit sphere in n-dimensional space, and multiply each
      vector in it by an m x n  matrix , you will get an ellipsoid in m-dimensional space. The singular values give the lengths of
      the principal axes of the ellipsoid. If the matrix is singular in some way, this will be reflected in the shape of the
      ellipsoid. In fact, the ratio of the largest singular value of a matrix to the smallest one gives a
      <see cref="ConditionNr"/> of the matrix, which determines, for example, the accuracy of numerical
      matrix inverses. Very small singular values are usually numerically meaningless.
</remarks>


      <Example>
      <code>
      var A,U,V: TMtx;
        S: TVec;
      begin
        CreateIt(U,V,A);
        CreateIt(S);
        try
          A.SetIt(3,3,True, [2, 5, 12, 1, 5, 2.5,
                            -4, 2,  8, 0, 0,   3,
                             2, 0,  4, 8, 12,  8]);

          A.SVD(U,S,V);
          // S =  [21.3406,10.9726,3.7989]

        finally
          FreeIt(U,V,A);
          FreeIt(S);
        end;
    end;
    </code>
    </Example>

    <SeeAlso cref="SVDSolve"/>
    <SeeAlso cref="ConditionNr"/>*)
    function SVD(const S: TVec): TMtx; overload;
    (*<summary>Matrix version of SVD. Performs a SVD for each U and V matrices columns in single pass.  </summary>
               
<remarks>U and V will be calculated only, if both are assigned. The following holds true:
               <code>
               A = U*S*Transp(V)
               </code>
               V is returned already transposed.
</remarks>
*)

    function SVD(U: TMtx; const S: TVec; V: TMtx; MinSize: boolean = True): TMtx; overload;

    (*<summary>Calculates the minimum norm solution to a real linear least squares problem.</summary>
      
<remarks>Calculates the minimum norm solution to a real linear least squares problem.

      <c>Minimize 2-norm(| b - A*x |).</c><para/>
      using the singular value decomposition (SVD) of the calling matrix A. A is an
      Rows-by-Cols matrix which may be rank-deficient. Several right hand side vectors b and solution vectors
      x can be handled in a single call. The effective rank of A is determined by treating as zero those singular
      values which are less than Threshold times the largest singular value and is returned by the function. The
      S vector holds the singular values on the output.
</remarks>


      <Example>
      <code>
      var X,B,D: TVec;
        V: TMtx;
      begin
        CreateIt(X,B,D);
        CreateIt(V);
        try
          B.SetIt(false,[0,2,3]);
          V.SetIt(3,3,false,[1,2,3,
                             3,4,5,
                             6,7,7]);

          V.SVDSolve(B,X,D); // matrix V can be non-quadratic
        finally
          FreeIt(X,B,D);
          FreeIt(V);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SVD"/>*)
    function SVDSolve(const B, X, S: TVec; Threshold: double = 1E-9): integer; overload;
    (*<summary>Matrix version of SVDSolve. Performs a SVDSolve for each U and V matrices columns in single pass.</summary>*)
    function SVDSolve(const B, X: TMtx; const S: TVec; Threshold: double = 1E-9): integer; overload;

    (*<summary>Solves the Sylvester equation.</summary>
      
<remarks>Solves the Sylvester's equation:

      <IMG name="mtx001"/><para/>

      for real quasi-triangular or complex triangular matrices A and B. The solution (X) overwrites the C matrix.
      The AOp and BOp parameters indicate the <see cref="TMtxOperation" text="operation(op)"/> performed on A and
      B respectively. The <see cref="TSign"/> of the Sylvester equation is determined with the ASign parameter
      (siPlus for +, siMinus for -). The result of a Sylvester function is a scale factor a. The A and B matrices
      <see cref="Quadratic"/> property must be true, otherwise an exception is raised.
</remarks>
*)
    function Sylvester(const A, B, C: TMtx; ASign: TSign = siPlus; AOp: TMtxOperation = opNone; BOp: TMtxOperation = opNone): double; overload;

    (*<summary>Saves matrix data to MatrixMarket ASCII file format.</summary>
      
<remarks>Saves matrix data to MatrixMarket ASCII file format.
</remarks>
*)
    procedure SaveToMatrixMarketFile(FileName: string);

    (*<summary>Splits matrix in blocks.</summary>
      
<remarks>Splits he calling matrix in blocks. Example:

      <code>
      [ A B ]
      [ C D ]
      </code><para/>
      where A,B,C,D are matrices of size 2x2 and the calling matrix is of size 4x4. ARows and ACols define
      the number of rows and columns of the block based matrix. The matrices passed in the dst array to be
      filled with values from the calling matrix, must already have matching size and complex properties to
      cover the calling matrix or an exception will be raised.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtx;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(2,2);
          B.Size(A);
          C.Size(A);
          D.Size(A);
          E.SetIt(4,4,False,[1,2,3,4
                             5,6,7,8
                             9,1,2,3
                             4,5,6,7]);
          E.Split(2,2,[A,B,
                       C,D]);
          // A becomes:
          // [1,2,
          //  5,6]

          // B becomes:
          // [3,4,
          //  7,8]

          // C becomes:
          // [9,1,
          //  4,5]

          // D becomes:
          // [2,3,
          //  6,7]
        finally
          FreeIt(A,B,C,D);
          FreeIt(E);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Concat"/>
      <SeeAlso cref="ConcatVert"/>
      <SeeAlso cref="ConcatHorz"/>*)
    procedure Split(ARows, ACols: integer;const Dst: array of TMtx);

    (*<summary>Calculates the sum of each of the calling matrix columns.</summary>
      
<remarks>Calculates the sum of each of the calling matrix columns and stores the results in Dst vector.
      The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the Dst vector are adjusted automatically.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
          V: TVec;
      begin
        CreateIt(A);
        CreateIt(V);
        A.Size(2,2,False,[[1,5,
                           2,3]);

        A.SumCols(V); // V = [3,8]
        FreeIt(V);
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="SumRows"/>*)
    procedure SumCols(const Dst: TVec); overload;

    (*<summary>Calculates the sum of each of the calling matrix rows.</summary>
      
<remarks>Calculates the sum of each of the calling matrix rows and stores the results in Dst vector.
      The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the Dst vector
      are adjusted automatically.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
          V: TVec;
      begin
        CreateIt(A);
        CreateIt(V);
        A.Size(2,2,False,[[1,2,
                           2,4]);

        A.SumRows(V); // V = [3,6]
        FreeIt(V);
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="SumCols"/>*)
    procedure SumRows(const Dst: TVec); overload;

    (*<summary>Calculates the tensor product of two vectors.</summary>
      
<remarks>Calculates the tensor product of Vec1 and Vec2 vectors and stores the results in the calling matrix. The <see cref="Rows"/>
      property is set to Vec1.<see cref="TMtxVecBase.Length" text="Length"/> and <see cref="Cols"/> property is set to Vec2.<see cref="TMtxVecBase.Length" text="Length"/>.
      The <see cref="TMtxVec.Complex">Complex</see> property of the calling matrix is adjusted automatically.
</remarks>



        <Example>
        <code>
        var Vec1,Vec2: TVec;
          V: TMtx;
        begin
          CreateIt(Vec1,Vec2);
          CreateIt(V);
          try
            Vec1.Size(3);
            Vec1.SetIt(false,[0,2,3]
            Vec2.Copy(Vec1);
            V.TensorProd(Vec1,Vec2);
            // V becomes:
            // [0,0,0]
            // [0,4,6]
            // [0,6,9]
          finally
            FreeIt(Vec1,Vec2);
            FreeIt(V);
          end;
        end;
        </code>
        </Example>

      <SeeAlso cref="AddTensorProd"/>*)
    function TensorProd(const Vec1, Vec2: TVec): TMtx; overload;

    (*<summary>Constructs a Toeplitz matrix.</summary>
      
<remarks>Constructs a Toeplitz matrix  with first row equal to FirstRow vector and first column equal to FirstCol vector,
      but without the first element. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see>
      properties of the calling matrix are adjusted automatically. An exception is raised if FirstRow and FirstCol
      Complex properties do not match.
</remarks>


      <Example>
      <code>
      var V1,V2: TVec;
          T: TMtx;
      begin
        CreateIt(V1,V2);
        CreateIt(T);
        try
          V1.SetIt(False,[1,2,3,4]);
          V2.SetIt(False,[2,3,4]);
          T.Toeplitz(V1,V2);
          // T becomes:
          //[1  2  3  4]
          //[2  1  2  3]
          //[3  2  1  2]
          //[4  3  2  1]
        finally
          FreeIt(T);
          FreeIt(V1,V2);
        end;
      end;
      </code>
      </Example>*)
    function Toeplitz(const FirstRow,FirstCol: TVec): TMtx; overload;
    (*<summary>Constructs a Toeplitz matrix.</summary>
      
<remarks>Constructs a Toeplitz matrix with first row equal to FirstRow vector
      and first column equal to FirstRow vector, but without the first element.
</remarks>
*)
    function Toeplitz(const FirstRow: TVec): TMtx; overload;

    (*<summary>Matrix trace.</summary>
      <returns>the trace of the calling matrix.</returns>
      
<remarks>If the calling matrix <see cref="TMtxVec.Complex">Complex</see> property is false, the imaginary part is set to zero.
</remarks>
*)
    function Trace: TCplx; overload;

    (*<summary>Transposes matrix.</summary>
      
<remarks>Transposes calling matrix in-place. Instead of using transpose directly, try using the <see cref="TMtxOperation"/>
      parameter of many TMtx methods. If this operation can not be avoided try using the not-in-place version  (see bellow)
      or see the <see cref="Rotate90"/> method.
</remarks>


      <Example>
      <code>
      var  A: TMtx;
      begin
        CreateIt(A);
        A.Size(2,1,True); // 2x1 complex matrix
        A.SetVal(Cplx(3,4));
        A.Transp;
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Rotate90"/>*)
    function Transp: TMtx; overload;
    (*<summary>Transpose the Mtx matrix.</summary>
      
<remarks>Write the results to the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVec.Complex">Complex</see> properties of the calling matrix are adjusted automatically.
</remarks>
*)
    function Transp(const Mtx: TMtx): TMtx; overload;

    (*<summary>Constructs upper triangular matrix.</summary>
      
<remarks>The method uses Mtx matrix to construct an upper triangular matrix. The results are stored in the calling matrix.
      If the ZeroLower parameter is true then the calling matrix subdiagonal elements will be set to zero - otherwise
      the subdiagonal elements will not be initialized. If the Diagonal boolean parameter is true then the Mtx matrix
      main diagonal elements will be copied to the calling matrix main diagonal elements. If the Diagonal parameter is
      false, the calling matrix main diagonal elements will be set to zero.
</remarks>


      <Example>
      <code>
      var  A,B: TMtx;
      begin
        CreateIt(A,B);
        A.SetIt(2,1,True,[1,2,
                          2,4]);  // 2x2, not complex matrix
        B.UpperTriangle(A,True,True);
        // B becomes:
        //  [1,2,
        //  [0,4]
        FreeIt(A,B);
      end;
      </code>
      </Example>

      <SeeAlso cref="LowerTriangle"/>*)
    function UpperTriangle(const Mtx: TMtx; ZeroLower, Diagonal: boolean): TMtx; overload;

    (*<summary>Construct VanderMonde matrix.</summary>
      
<remarks>Constructs a VanderMonde matrix with Vec.<see cref="TMtxVecBase.Length" text="Length"/> rows and ACols columns. The second to last column of the VanderMonde matrix
      is Vec:

      <IMG name="mtx007"/><para/>
</remarks>
*)
    function VanderMonde(ACols: Integer; const Vec: TVec): TMtx; overload;

    (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>
      
<remarks>Convert all elements of the calling matrix to strings with  formating real parts with ReFormat, imaginary parts with ImFormat,
      using the text delimiter Delimiter and store them in dstList, by using the Add method of TStrings object.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


      <Example>
      <code>
      procedure TForm1.Button1Click(Sender: TObject);
      var a,b: TMtx;
      begin
        CreateIt(a,b);
        try
          a.SetIt(2,2,False,[1,2,3,4]);
          a.Cos;
          b.Size(a);
          b.SetVal(1);
          a.Add(b);
          Richedit1.Clear;
          Memo1.Clear;
          a.ValuesToStrings(Richedit1.Lines);
          b.ValuesToStrings(Richedit1.Lines);
          a.ValuesToStrings(Memo1.Lines);
          b.ValuesToStrings(Memo1.Lines);
          Memo1.Lines.SaveToFile('C:\Test.txt');
          Memo1.Lines.LoadFromFile('C:\Test.txt');
          a.StringsToValues(Memo1.Lines);
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="StringsToValues"/>*)
    function ValuesToStrings(const dstList: TStrings; const Delimiter: string = kTab; const Align: TFixedTextAlign = ftaNone;
                                                     const ReFormat: string = ' 0.######;-0.#######';
                                                     const ImFormat: string = '+0.######i;-0.######i';
                                                     const Headers: boolean = false): integer; overload;

    (*<summary>Convert calling matrix elements, starting with [Row,Col] until [row + RowCount, col + ColCount] converting Len elements to strings.</summary>
      
<remarks>Formatting real parts with ReFormat, imaginary parts with ImFormat, using the text delimiter Delimiter and store them in dstList starting at ListIndex.
      If dstList is not large enough, the method will use the Add method of dstList object.

      Returns column width, when alignment is other than none.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
    function ValuesToStrings(const dstList: TStrings; ListIndex,Row,Col,RowCount,ColCount: integer;
                                           const Delimiter: string = kTab; const Align: TFixedTextAlign = ftaNone;
                                           const ReFormat: string = ' 0.######;-0.######';
                                           const ImFormat: string = '+0.######i;-0.######i';
                                           const Headers: boolean = false): integer; overload;



    (*<summary>Converts all calling matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Delimiter: string = kTab;
        const ReFormat: string = ' 0.######;-0.#######';
        const ImFormat: string = '+0.######i;-0.######i');overload;

    (*<summary>Converts [Row, Col] to [Row+RowCount, Col+ColCount] matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab;
                                     const ReFormat: string = ' 0.######;-0.######';
                                     const ImFormat: string = '+0.######i;-0.######i'); overload;

    function TextToValues(Text: String; Delimiter: String = kTab): TMtx;

    (*<summary>Transforms matrix into matrix of standardized data.</summary>
      
<remarks>The routine uses Data matrix and transforms it into ZScore matrix of standardized data
      (the so called Z Scores). The standardization of each column (variable) is made by subtracting
      its mean and dividing by its standard deviation:

      <code>
                    Column - Mean(Column)
      Z(Column) = -----------------------
                    StdDev(Column)
      </code>
</remarks>


    <example>
    <code>
    var Data: TMtx;
      Res TMtx;
    begin
      Data := TMtx.Create;
      Res := TMtx.Create;
      try
        Data.SetIt(3,3,false,[1,2,3,
                            4,5,6,
                            7,100,12]);
        Res.ZScore(Data);
        // result = (-1	-0,604	-0,873,
        //            0	-0,55	-0,218
        //            1	 1,154	 1,091)
      finally
        Data.Free;
        Res.Free;
      end;
    end;
    </code>
    </example>*)
    function ZScore(const Src: TMtx): TMtx; overload;

    
    
    procedure WriteHeader(const Dst: TStream; Precision: TPrecision; Rounding: TRounding = rnTrunc; Endian: TEndianness = boLittleEndian); override;
    procedure WriteHeader(const Dst: TStream); override;
    
    

    

    

published
    (*<summary>Defines if matrix type will be checked automatically.</summary>
      
<remarks>If True, some routines requesting <see cref="TMtxType"/> information will check for the matrix type first by calling the
      <see cref="DetectMtxType"/> method, in order to select the most efficient algorithm. The following types will be detected:
      <list>
      <item> mtSymmetric = symmetric matrix </item>
      <item> mtHermitian	= Hermitian matrix </item>
      <item> mtTriangle	= triangular matrix, with unit or non unit main diagonal </item>
      <item> mtGeneral	= general matrix (none of the above) </item>
      </list>

      In worst case the matrix type detection will require O(n^2) compare operations. In case of a general matrix, it only takes a few cycles
      for the algorithm to detect that the matrix is not Symmetric, triangular or Hermitian. And in case of a symmetric or hermitian or
      triangular matrix, the cost of O(n^2) compare operations is still much lower than the loss of performance, when using the general purpose
      algorithm. For some computations, like eigenvalues, the type of the matrix has to specified explicitly, especially when it comes to symmetric
      matrices, because the algorithm for the general matrix will fail on a symmetric matrix.<para/>
      If you specify the type of matrix explicitly to the procedures and AutoMtxType is set to True, then AutoMtxType will override the user
      specified type.
</remarks>
*)
    property  AutoMtxType: boolean read FAutoMtxType write SetAutoMtxType default false;
    (*<summary>Defines matrix balancing method.</summary>
      
<remarks>Defines which <see cref="TBalanceType"/> will be used to balance the calling matrix. Balancing the calling matrix can be
      very useful when calculating the eigenvalues and eigenvector. Balancing operation can perform one or both of the following similarity
      transformations:

      <b>1.</b>Permute the calling matrix (A) to block of upper triangular form:

      <IMG name="mtx008"/><para/>

      where P is a permutation matrix, and A'11 and A' 33 are upper triangular. The diagonal elements of A'11 and A' 33 are eigenvalues of the
      calling matrix (A).

      <b>2.</b>Apply a diagonal similarity transformation to A', to make the rows and columns of A'22  as close in norm as possible:

      <IMG name="mtx009"/><para/>

      This scaling can reduce the norm of the matrix (that is, ||A''22|| &lt; ||A'22||), and hence reduce the effect of rounding errors on the
      accuracy of computed eigenvalues and eigenvectors.
</remarks>


      <Example>
      <code>
      var A: TMtx;
        d: TVec;
      begin
        CreateIt(A);
        // ...
        A.Balance := btFull; // applies both methods
        A.Eig(d);
        // ...
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Eig"/>
      <SeeAlso cref="EigGen"/>*)
    property  Balance: TBalanceType read FBalance write SetBalance default btFull;
    (*<summary>Defines the number of matrix columns.</summary>
      
<remarks>Use this property to set or get the number of the calling matrix columns. Setting the Cols does not affect the actual amount of
      memory allocated unless Rows*Cols is bigger than <see cref="TMtxVecBase.ActualSize"/>.

      Note
        Always set the <see cref="TMtxVec.Complex">Complex</see> property before setting Cols. Complex is initially false and Cols is
        initially 0. You can also set the Cols property by calling the <see cref="Size"/> method.
</remarks>


      <SeeAlso cref="Rows"/>
      <SeeAlso cref="Size"/>*)
    property Cols: integer read FCols write SetCols default 0;
    (*<summary>Defines the norm used to compute the condition number.</summary>
      
<remarks>Defines the norm used to compute the <see cref="ConditionNr"/>.
      The default value is cnNone meaning that no condition number will be estimated.
</remarks>


      <SeeAlso cref="ConditionNr"/>*)
    property  ConditionNumber: TConditionNumber read FConditionNumber write SetConditionNumber default cnNone;

    (*<summary>Enables/disables the refining of the solution of the system of linear equations.</summary>
      
<remarks>The method enables/disables the refining of the solution of the system of linear equations,
      computed by <see cref="LUSolve"/> method. When RefineSolution is enabled, an iterative refinement of the solution to
      a system of linear equations is executed. For each computed solution the component-wise <see cref="BackError"/>
      and the the component-wise <see cref="ForwError"/> in the computed solution are calculated as well.
</remarks>


      <Example>
      <code>
      var X,B: TVec;
        A: TMtx;
      begin
        CreateIt(X,B);
        CreateIt(A);
        try
          B.SetIt(False,[0,2]);
          A.SetIt(2,2,false,[1,2,
                            3,4]);  // 2x2, not complex matrix
          A.RefineSolution := True;
          A.ConditionNumber := cnNormInf;
          A.LUSolve(B,X);
        finally
          FreeIt(B,X);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="BackError"/>
      <SeeAlso cref="ForwError"/>
      <SeeAlso cref="LUSolve"/>*)
    property  RefineSolution: boolean read FRefineSolution write SetRefineSolution default false;
    (*<summary>Defines the number of matrix rows.</summary>
      
<remarks>Use this property to set or get the number of the calling matrix rows. Setting the Rows does not affect the actual amount of
      memory allocated unless Rows*Cols is bigger than <see cref="TMtxVecBase.ActualSize"/>.

      Note
        Always set the <see cref="TMtxVec.Complex">Complex</see> property before setting Rows. Complex is initially false and Rows is
        initially 0. You can also set the Rows property by calling the <see cref="Size"/> method.
</remarks>


      <SeeAlso cref="Cols"/>
      <SeeAlso cref="Size"/>*)
    property  Rows: integer read FRows write SetRows default 0;
    (*<summary>Defines the number of leading columns.</summary>
      
<remarks>This value defines the spacing in number of samples between rows.
      It is fixed to be the same to the value of <see cref="Cols"/> (for now).
</remarks>
*)
    property  LeadingCols: integer read GetLeadingCols write SetLeadingCols;
    (*<summary>Sets/reads the number of superdiagonals in banded matrices.</summary>

      <SeeAlso cref="SubDiag"/>
      <SeeAlso cref="TMtxType"/>*)
    property  SuperDiag: integer read FSuperDiag write SetSuperDiag default 0;
    (*<summary>Sets/reads the number of subdiagonals in banded matrices.</summary>

      <SeeAlso cref="SuperDiag"/>
      <SeeAlso cref="TMtxType"/>*)
    property  SubDiag: integer read FSubDiag write SetSubDiag default 0;
    (*<summary>Determine which part (upper or lower) of the symmetric, hermitian or triangular matrix is set.</summary>
      
<remarks>Determine which part (upper or lower) of the symmetric, hermitian or triangular matrix is set.
</remarks>


      <SeeAlso cref="TriangleUnit"/>*)
    property  TriangleForm: TTriangleForm read FTriangleForm write SetTriangleForm default tfUpper;
    (*<summary>Determine, if the matrix is unit triangular.</summary>
      
<remarks>Determine, if the matrix is unit triangular.
</remarks>


      <SeeAlso cref="TriangleForm"/>*)
    property  TriangleUnit: boolean read FTriangleUnit write SetTriangleUnit default false;
  end;

  TMtxArray = array of TMtx;
  TVecArray = array of TVec;

   (*<summary>List of TMtx objects.</summary>
     
<remarks>List of <see cref="TMtx"/> objects. The list manages
     the object list. The TMtx objects are created and destroyed
     by the list. List size can be controled with the Count property.
</remarks>
*)
   TMtxList = class(TObjectsList)
   private
      function GetItems(i: integer): TMtx;
      procedure SetItems(i: integer; const Value: TMtx);
      procedure SetCount(const Value: integer); reintroduce;
      function GetCount: integer; reintroduce;
   public
      (*<summary>Creates and adds a new TMtx object to the list.</summary>
        
<remarks>The function returns the Index at which the newly created object resides in
         the list.
</remarks>
*)
      function Add: integer; virtual;
      (*<summary>Assigns the source to the calling object.</summary>
        
<remarks>All the data is copied.
</remarks>
*)
      procedure Assign(Source: TMtxList); virtual;

      destructor Destroy; override;

      
      procedure SaveToStream(Stream: TStream); overload; virtual;
      procedure LoadFromStream(Stream: TStream); overload; virtual;
      
      

      
      property Item[i: integer]: TMtx read GetItems write SetItems; default; 
      property Count: integer read GetCount write SetCount; 
      
   end;

   (*<summary>List of TVec objects.</summary>
      
<remarks>List of <see cref="TVec"/> objects.
      The list manages the object list. The TVec objects are created
      and destroyed by the list. List size can be controled
      with the Count property. Items can be added which already exist with Add(src), by calling Add() method
      or by setting Count property.
</remarks>
*)
   TVecList = class(TObjectsList)
   strict private
      function GetItems(i: integer): TVec;
      procedure SetItems(i: integer; const Value: TVec);
      procedure SetCount(const Value: integer); reintroduce;
      function GetCount: integer; reintroduce;
   public
      (*<summary>Uses all list elements to form columns of the Dst Matrix.</summary>
         
<remarks>All TVec list elements must have the same number
         of elements.
         The number of columns will match the number of vectors.
</remarks>
*)
      procedure ComposeColumnMatrix(const Dst: TMtx);
      (*<summary>Uses all list elements to form rows of the Dst Matrix.</summary>
         
<remarks>All TVec list elements must have the same number
         of elements.
         The number of rows will match the number of vectors.
</remarks>
*)
      procedure ComposeRowMatrix(const Dst: TMtx);
      (*<summary>Copies all columns to the list of vectors.</summary>
         
<remarks>The number of columns will match the number of vectors.
</remarks>
*)
      procedure DecomposeColumnMatrix(const Src: TMtx);
      (*<summary>Copies all rows to the list of vectors.</summary>
         
<remarks>The number of rows will match the number of vectors.
</remarks>
*)
      procedure DecomposeRowMatrix(const Src: TMtx);
      (*<summary>Subranges all rows from the Src Matrix with vectors
         from the list.</summary>*)
      procedure SetSubRange(const Src: TMtx);
      (*<summary>Calls SetFullRange on each of the list elements.</summary>*)
      procedure SetFullRange;
      (*<summary>Creates and adds a new TVec object to the list.</summary>
        
<remarks>The function returns the Index at which the newly created object resides in
         the list.
</remarks>
*)
      function Add: integer; overload; virtual;
      (*<summary>Creates and adds a new TVec object to the list.</summary>
        
<remarks>The function returns the Index at which the object resides in the list.
</remarks>
*)
      function Add(const Src: TVec): integer; overload; virtual;
      (*<summary>Assigns the source to the calling object.</summary>
        
<remarks>All the data is copied.
</remarks>
*)
      procedure Assign(Source: TVecList); virtual;

      destructor Destroy; override;


      
      (*<summary>Save the list to a stream.</summary>*)
      procedure SaveToStream(const Stream: TStream); overload; virtual;
      (*<summary>Load the list from a stream.</summary>*)
      procedure LoadFromStream(const Stream: TStream); overload;  virtual;
      
      

      
      property Item[i: integer]: TVec read GetItems write SetItems; default; 
      property Count: integer read GetCount write SetCount; 
      
   end;

   (*<summary>The report printer.</summary>
     
<remarks>The most common way to debug an algorithm in the old days was to print
     the values of the variables to the screen or to the file. Today it
     is more common to use watches and tooltips to examine the values of
     variables. In some cases this two approaches do not work because of
     multithreading. Sometimes it is also desirable to have an algorithm
     return report on its convergence, like it is the case with optimization
     algorithms. For cases like this there is a global variable called
     Report declared in MtxVec.pas unit. Report is of a TMtxVecReport
     type and is derived from TStringStream. It has been extended with several
     new methods to support:
     * Saving the stream to TStream or a file.
     * Write the contents of TVec and TMtx objects to the stream (as text).
     * Specify the text formatting of the floating point values.
</remarks>
*)
   TMtxVecReport = class( TStringStream )
   strict private
      FImFormat: string;
      FReFormat: string;
      
      StrList: TStringList;
      
      FDelimiter: string;
      procedure SetImFormat(const Value: string);
      procedure SetReFormat(const Value: string);
      procedure SetDelimiter(const Value: string);
      procedure StrListClear;
      
   public
      
      (*<summary>Save the strings to a stream.</summary>*)
      procedure SaveToStream(Dst: TStream); overload;  virtual; 
      procedure WriteString(const aStrings: TStringList); overload;
      
      (*<summary>Save the strings to a text file.</summary>*)
      procedure SaveToFile(const FileName: string; Append: boolean = false);
      (*<summary>Convert the values of TVec and TMtx objects to text.</summary>
        
<remarks>Adds them to stream. Names parameter is an open array that can hold
        the names of variables in the MtxVecs list.
</remarks>
*)
      procedure Print(const MtxVecs: array of TMtxVec; const Names: array of string); overload;
      (*<summary>Convert the values of TVec objects to text.</summary>
        
<remarks>Adds them to stream.  Names parameter is an open array that can hold
        the names of variables in the vecs list.
</remarks>
*)
      procedure PrintVec(const vecs: array of TVec; const Names: array of string); virtual;
      (*<summary>Convert the values of TMtx objects to text.</summary>
        
<remarks>Adds them to stream. Names parameter is an open array that can hold
        the names of variables in the mtxs list.
</remarks>
*)
      procedure PrintMtx(const mtxs: array of TMtx; const Names: array of string); virtual;
      (*<summary>Convert TCplx values to text.</summary>
        
<remarks>Adds them to stream. Names parameter is an open array that can hold
       the names of variables in the mtxs list.
</remarks>
*)
      procedure PrintCplx(const cplxs: array of TCplx; const Names: array of string); virtual;
      procedure PrintSCplx(const cplxs: array of TSCplx; const Names: array of string); virtual;
      (*<summary>Convert double values to text.</summary>
        
<remarks>Adds them to stream. Names parameter is an open array that can hold
        the names of variables in the mtxs list.
</remarks>
*)
      procedure PrintSample(const val: array of double; const Names: array of string); virtual;
      procedure PrintDouble(const val: array of double; const Names: array of string); virtual;
      procedure PrintSingle(const val: array of single; const Names: array of string); virtual;
      (*<summary>Convert an array of double values to text.</summary>
        
<remarks>Adds them to stream. Names parameter can hold the name of the array variable.
</remarks>
*)
      procedure PrintSampleArray(const val: array of double; const aName: string); virtual;
      (*<summary>Convert an array of TCplx values to text.</summary>
        
<remarks>Adds them to stream. Names parameter can hold the name of the array variable.
</remarks>
*)
      procedure PrintCplxArray(const val: array of TCplx; const aName: string); virtual;
      (*<summary>Inserts a new line.</summary>*)
      procedure NewLine;
      (*<summary>Inserts a new line.</summary>*)
      procedure WriteLine;
      (*<summary>Clears the stream.</summary>*)
      procedure Clear;

      constructor Create(Astring: string); virtual;
      destructor Destroy; override;

      (*<summary>The format specifier for the real part of the complex number.</summary>*)
      property ReFormat: string read FReFormat write SetReFormat;
      (*<summary>The format specifier for the imaginary part of the complex number.</summary>*)
      property ImFormat: string read FImFormat write SetImFormat;
      (*<summary>Delimiter used when converting matrices to text.</summary>*)
      property Delimiter: string read FDelimiter write SetDelimiter;
      
   end;

   

    TVecRecord = record
     Vec: TVec;
     Used: boolean;
   end;

    TMtxRecord = record
     Mtx: TMtx;
     Used: boolean;
   end;




       TCacheVec = array of TVecRecord;
       TCacheMtx = array of TMtxRecord;

     (*<summary>Sets threading mode used by MtxVec.</summary>*)
       TThreadingMode = (
      (*<summary>Threading is disabled.</summary>*)
       ttSerial,
      (*<summary>Threading is agressive. To reduce thread switching
      time, threads never enter sleep state and continue
      running empty until new job is given. This consumes
      all CPU resources and gives best performance. This is
      not suitable, if there are any other applications to
      be run on the same computer, because CPU will be completely
      consumed by one application.</summary>*)
        ttTurnAround,
      (*<summary>Threading is balanced. To reduce thread switching
      time, but also reduce CPU load, threads enter sleep
      state after specified time if no new job is given.
      This can consume a lot CPU resources, if the wait
      time before entering the sleep is larger than 0.
      While threads wait they consume all available CPU resources.
      By default, the time before sleeping is 0.</summary>*)
        ttThroughput);


       TVecCache = class(TAbstractMtxVecCache)
       public
          DoubleCacheOffset: TIntegerArray;
          DoubleCachePointers: T2DDoubleArray;  

          CplxDoubleCacheOffset: TIntegerArray;
          CplxDoubleCachePointers: T2DCplxDoubleArray;  

          SingleCacheOffset: TIntegerArray;
          SingleCachePointers: T2DSingleArray;  

          CplxSingleCacheOffset: TIntegerArray;
          CplxSingleCachePointers: T2DCplxSingleArray;  
       strict protected
          SingleCache: TSingleArray;
          DoubleCache: TDoubleArray;

          function NewItem: TMtxVecBase; override;
          procedure CacheMemFree; override;
          function GetCacheInBytes: Int64; override;
          procedure CacheMemAllocate(ItemCount, Elements: integer); override;
       end;


       TMtxCache = class(TAbstractMtxVecCache)
       public
          DoubleCacheOffset: TIntegerArray;
          DoubleCachePointers: T2DDoubleArray;  

          CplxDoubleCacheOffset: TIntegerArray;
          CplxDoubleCachePointers: T2DCplxDoubleArray;  

          SingleCacheOffset: TIntegerArray;
          SingleCachePointers: T2DSingleArray;  

          CplxSingleCacheOffset: TIntegerArray;
          CplxSingleCachePointers: T2DCplxSingleArray;  

       strict protected
          SingleCache: TSingleArray;
          DoubleCache: TDoubleArray;

          function NewItem: TMtxVecBase; override;
          procedure CacheMemFree; override;
          function GetCacheInBytes: Int64; override;
          procedure CacheMemAllocate(ItemCount, Elements: integer); override;
       end;

       TMtxVecPoolItem = class(TPersistent)
       strict protected
          fIsUsed: integer;
          fThreadID: TThreadID;
          fPoolIndex: integer;
          
          cs: TCriticalSection;
          
          procedure SetThreadID(Value: TThreadID);
          procedure SetPoolIndex(Value: integer);
          procedure SetIsUsed(Value: boolean);
          function GetIsUsed: boolean;
       public
          Vec: TVecCache;
          VecInt: TVecIntCache;
          Mtx: TMtxCache;
          MtxInt: TMtxIntCache;
          procedure Enter; overload;
          procedure Leave; overload;
          function TryEnter: boolean;
          function CacheUsed: boolean;
          property ThreadID: TThreadID read fThreadID write SetThreadID;
          property PoolIndex: integer read fPoolIndex write SetPoolIndex;
          property IsUsed: boolean read GetIsUsed write SetIsUsed;
          constructor Create;
          destructor Destroy; override;
       end;

       TMtxVecPoolItemArray = array of TMtxVecPoolItem;
       TSmallMatrixMultiplyArray = array of TSmallMatrixMultiply;

       TMtxMulJitCache = class(TPersistent)
       private
          
          cs: TCriticalSection;
          
          sKernel: TSmallMatrixMultiplyArray;
          dKernel: TSmallMatrixMultiplyArray;
          cKernel: TSmallMatrixMultiplyArray;
          zKernel: TSmallMatrixMultiplyArray;

          sKernelTransp: TSmallMatrixMultiplyArray;
          dKernelTransp: TSmallMatrixMultiplyArray;
          cKernelTransp: TSmallMatrixMultiplyArray;
          zKernelTransp: TSmallMatrixMultiplyArray;
          fSize: integer;
          procedure SetSize(const Value: integer);
       public
           function GetSKernel(mSize: integer): TSmallMatrixMultiply;
           function GetDKernel(mSize: integer): TSmallMatrixMultiply;
           function GetCKernel(mSize: integer): TSmallMatrixMultiply;
           function GetZKernel(mSize: integer): TSmallMatrixMultiply;

           function GetSKernelTransp(mSize: integer): TSmallMatrixMultiply;
           function GetDKernelTransp(mSize: integer): TSmallMatrixMultiply;
           function GetCKernelTransp(mSize: integer): TSmallMatrixMultiply;
           function GetZKernelTransp(mSize: integer): TSmallMatrixMultiply;

           property Size: integer read fSize write SetSize;

           procedure Enter;
           procedure Leave;
           constructor Create;
           destructor Destroy; override;
       end;


     (*<summary>Controlls the object cache and threading logic.</summary>*)
       TMtxVecController = class(TPersistent)
       strict private
          fThreadDimension: integer;






          FDenormalsAreZero: boolean;
          FThreadingMode: TThreadingMode;
          FThreadWaitBeforeSleep: integer;
          MainThreadID: TThreadID;
          fSuperConductive: boolean;

          fCacheLevel1: integer;
          fCacheLevel2: integer;
          fCacheLevel3: integer;
          fCPUCoresPhysical: integer;
          fCPUCoresLogical: integer;
          fCPUCoresPhysicalPerNuma: integer;
          fCPUCacheSizeInBytes: integer;
          fNumaNodes: integer;
          fCPUVendor: string;



          
          
          csLock: Byte;
          
          

          function GetFFTDescriptorCacheSize: integer;
          procedure SetFFTDescriptorCacheSize(const value: integer);

          function GetThreadCount: integer;
          procedure SetThreadCount(const Value: integer);

          function GetLapackThreadCount: integer;
          procedure SetLapackThreadCount(const Value: integer);

          function GetBlasThreadCount: integer;
          procedure SetBlasThreadCount(const Value: integer);

          function GetIppThreadCount: integer;
          procedure SetIppThreadCount(const Value: integer);

          function GetVmlThreadCount: integer;
          procedure SetVmlThreadCount(const Value: integer);

          function GetFFTThreadCount: integer;
          procedure SetFFTThreadCount(const Value: integer);

          function GetCpuFrequencyInMhz: integer;

          function GetCpuType: integer;
          procedure SetDenormalsAreZero(const Value: boolean);
          procedure SetThreadingMode(const Value: TThreadingMode);
          procedure SetThreadWaitBeforeSleep(const Value: integer);
          procedure SetThreadDimension(const Value: integer);
          procedure SetSuperConductive(const Value: boolean);

       strict protected
          procedure UnMarkThreads;
          procedure Enter;
          procedure MarkMainThread;
          procedure Leave; 

          procedure FreeMKLMemory;


       public
          procedure FreeItx(var a: TMtx); overload;
          procedure FreeItx(var a: TVec); overload;
          procedure FreeItx(var a: TVecInt); overload;
          procedure FreeItx(var a: TMtxInt); overload;

          procedure CreateItx(var a: TMtx); overload;
          procedure CreateItx(var a: TVec); overload;
          procedure CreateItx(var a: TVecInt); overload;
          procedure CreateItx(var a: TMtxInt); overload;

          function GetPoolIndex: integer;
       public
          Pool: TMtxVecPoolItemArray;
          JitCache: TMtxMulJitCache;

         (*<summary> Specifies the maximum number of threads that object cache can serve. </summary>
          
<remarks>Specifies maximum number of threads that object cache can serve concurrently.
          Each thread has its own memory pool.

          UnMarkThread must be called before a thread ends. Creating more threads
          than ThreadDimension will route additoinal threads in to the main threads pool.

          Before changing ThreadDimension the object cache may not be used:  There
          may not be any TVec/TVecInt/TMtx/TMtxInt objects for which FreeIt was not called.
</remarks>
*)

          property ThreadDimension: integer read FThreadDimension write SetThreadDimension;
         (*<summary> If true, the denormals will be rounded to 0. </summary>
          
<remarks>Denomormals are values close the maximum floating point range.
</remarks>
*)
          property DenormalsAreZero: boolean read FDenormalsAreZero write SetDenormalsAreZero;
         (*<summary> Specifies the threading for MtxVec. </summary>*)
          property ThreadingMode: TThreadingMode read FThreadingMode write SetThreadingMode;
         (*<summary> Specifies the thread waiting time before entering sleep in ms. </summary>
                     
<remarks>Used only when ThreadingMode is equal to ttThroughput.
                     A value larger than zero can lead to excessive CPU usage, with only minor improvements in processing speed.
</remarks>
*)
          property ThreadWaitBeforeSleep: integer read FThreadWaitBeforeSleep write SetThreadWaitBeforeSleep;
         (*<summary> Returns the CPU type. </summary>*)
          property CpuType: integer read GetCpuType;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Returns and allows setting thread count only for the Intel IPP algorithms related code.
                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed
</remarks>
*)
          property IppThreadCount: integer read GetIppThreadCount write SetIppThreadCount;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Returns and allows setting thread count only for the FFT algorithm related code.
                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed.
</remarks>
*)
          property FftThreadCount: integer read GetFftThreadCount write SetFftThreadCount;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Returns and allows setting thread count only for the Intel MKL Lapack algorithms related code.
                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed.
</remarks>
*)
          property BlasThreadCount: integer read GetBlasThreadCount write SetBlasThreadCount;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Returns and allows setting thread count only for the Intel MKL Lapack algorithms related code.
                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed.
</remarks>
*)
          property LapackThreadCount: integer read GetLapackThreadCount write SetLapackThreadCount;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Returns and allows setting thread count only for the Intel VML algorithms related code.
                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed.
</remarks>
*)
          property VmlThreadCount: integer read GetVmlThreadCount write SetVmlThreadCount;
         (*<summary> Specifies the number of CPU cores to be used by the threaded algorithms internally. </summary>
                     
<remarks>Setting this property will modify IppThreadCount, FftThreadCount, LapackThreadCount and VmlThreadCount.

                     If you want to do multi-threading in your own code, set this parameter to 1.
                     The default value is 1 and user side multi-threading is assumed.
</remarks>
*)
          property ThreadCount: integer read GetThreadCount write SetThreadCount;
         (*<summary> Returns CPU frequency in Mhz, but takes about 2s to execute. </summary>*)
          property CpuFrequencyInMhz: integer read GetCpuFrequencyInMhz;
         (*<summary> Returns the size of the CPU L3 cache in bytes. </summary>*)
          property CpuCacheSizeInBytes: integer read fCPUCacheSizeInBytes;
         (*<summary> Returns the size of the CPU L1 cache in bytes per CPU Core. </summary>*)
          property CpuCacheL1SizeInBytes: integer read fCacheLevel1;
         (*<summary> Returns the size of the CPU L2 cache in bytes  per CPU Core. </summary>*)
          property CpuCacheL2SizeInBytes: integer read fCacheLevel2;
         (*<summary> Returns the size of the CPU L3 cache in bytes across all CPU cores. </summary>*)
          property CpuCacheL3SizeInBytes: integer read fCacheLevel3;

         (*<summary> Returns the number of physical CPU cores per one Numa Node. </summary>
                     
<remarks>This value defines the default MtxVec pool thread size and the ThreadDimension property.

                     The OS scheduler will schedule threads on the same socket on which the memory is allocated.
                     Embarcadero RAD Studio currently does not support memory allocation on a different NUMA node (December 2024).
                     While it is possible to specify ThreadCount and ThreadDimension properties to be more than CPUCores on multi-socket
                     systems, the performance benefits could be modest.

                     Each time a thread enters sleep, it could wake up on a different CPU core (invalidating CPU cache) and
                     also a different NUMA node (invalidating main memory), if thread affinity is not specified explicitely.
</remarks>
*)
          property CpuCores: integer read fCPUCoresPhysicalPerNuma;
         (*<summary> Returns the number of physical CPU cores across all NUMA nodes. </summary>*)
          property CpuCoresPhysical: integer read fCPUCoresPhysical;
         (*<summary> Returns the number of (hyper-threaded) CPU cores across all NUMA nodes. </summary>*)
          property CpuCoresLogical: integer read fCPUCoresLogical;
         (*<summary> Returns the number of NUMA nodes (CPU sockets) in the system. </summary>*)
          property CpuNumaNodes: integer read fNumaNodes;

          (*<summary> Specifies the numbe of FFT algorithm setups (descriptors) to keep stored in memory. </summary>
                       
<remarks>When a descriptor or algorithm setup for a particular FFT size and type already
                       exists, the computation of the FFT can be much (3-5x) faster. However, if the number
                       of different FFT sizes and consequently descriptors is too big, the system can
                       run out of memory. Notice also that FFT's of lengths other than power of two,
                       can be 3-5x slower. As a guideline, allocating 100 descriptors for different (1D) FFT sizes in
                       range from 39600 to 40000 will require around 300MBytes of memory. The bigger the FFT's
                       that you compute, the smaller must be the descriptor cache size and fewer
                       different FFT sizes should be computed to keep the performance level high.

            Note: This property should not be changed while there are FFT's being executed!
</remarks>
*)
          property FFTDescriptorCacheSize: integer read GetFFTDescriptorCacheSize write SetFFTDescriptorCacheSize;

   


   

          (*<summary> Enables thread congestion free CreateIt/FreeIt. </summary>
                       
<remarks>Enabled by default.
</remarks>
*)
          property SuperConductive: boolean read FSuperCOnductive write SetSuperConductive;

          (*<summary>CPU Vendor name. Intel or AMD. </summary>*)
          property CPUVendor: string read FCPUVendor;

          (*<summary>Setting condition check to false, will disable all range checks.</summary>*)
          procedure SetMtxConditionCheck(Value: boolean); overload;
          (*<summary>Setting condition check to false, will disable all range checks.</summary>*)
          procedure SetVecConditionCheck(Value: boolean); overload;
          (*<summary>Defines the object cache size for vectors.</summary>
                     
<remarks>Elements defines the number of elements per vector to preallocate.
</remarks>
*)
          procedure SetVecCacheSize(Vectors, Elements: integer);
          (*<summary>Defines the object cache size for vectors.</summary>
                     
<remarks>Elements defines the number of elements per matrix to preallocate.
</remarks>
*)
          procedure SetMtxCacheSize(Matrices, Elements: integer);

          (*<summary>Defines the object cache size for integervectors.</summary>
                     
<remarks>Elements defines the number of elements per matrix to preallocate.
</remarks>
*)
          procedure SetMtxIntCacheSize(Matrices, Elements: integer);

          (*<summary>Defines the object cache size for integervectors.</summary>
                     
<remarks>Elements defines the number of elements per matrix to preallocate.
</remarks>
*)
          procedure SetVecIntCacheSize(Vectors, Elements: integer);

          (*<summary> When thread has finished using object cache, release it. </summary>
                      
<remarks>When threads are using object cache each thread must call UnMarkThread after all objects
                      have been released back to object cache and before the thread finished executing.
</remarks>
*)
          procedure UnMarkThread; overload;
          procedure UnMarkThread(const PoolIdx: integer); overload;
          (*<summary> Associates calling thread with object cache super conducting logic. </summary>
                      
<remarks>Call this before using object cache within the thread. Once all objects
                      have been released back to object cache and before the thread finished executing
                      call UnMarkThread.
</remarks>
*)
          function MarkThread: boolean;
          (*<summary> Returns the number of unused thread dimensions. </summary>
                      
<remarks>Thread dimensions are slots used one per thread. The function
                      returns the number of threads that can still be associated with the
                      object cache to make use of super conducting memory allocation logic.
</remarks>
*)
          function UnusedThreadDimension: integer;

   
          constructor Create;
          destructor Destroy; override;
   
       end;

         (*<summary> Provides interface for multiplying small matrices by using the lapack's dgemm api standard.) </summary>
    
<remarks>The matrix multiplication code is generated on the fly to achieve maximum possible performance. Additionally all the
    error checking on each consecutive call is also absent. This is most usefull, when the matrices to be multiplied are very small:
    2x2 or 3x3, (100x) but there are still considerable benefits up to size 50x50 (1.3x) and more.

    Typically this object would be created, then Init method is to be called and finally, one of the six Mul methods can be called
    multiple times.The class provides four variants of Multiply methods:
    <list type="bullet">
    <item>  Mul(a,b,c: TMtx);  </item>
    <item>  Mul;  </item>
    <item>  MulKernel </item>
    <item>  MulKernelFloat, MulKernelDouble, MulKernelComplexFloat, MulKernelComplexDouble  </item>
    </list>

    It is most safe to use the first variant. Performance will increase with the parameterless Mul method (2x) and best performance can be
    obtained with the third variant (3x). Only switch to using the third variant when you are confident, that your algorithm works correctly.

    Please note that

    TMtx.Mul(A,B);

    will use object cache to store JIT-ed kernels up to user specified matrix size for square matrices. The default limit is set at size 32x32.
    The kernel will be created on first call.
</remarks>


    <Example> Three different options to setup matrix multiplication of small matrices
    <code>
        uses MtxVec, MtxExpr, Math387;

        procedure TForm84.Button2Click(Sender: TObject);
        var am, bm, cm, cmRef: Matrix;
            jit: TSmallMatrixMultiply;
            a,b,c: TDoubleArray;
        begin
            jit := TSmallMatrixMultiply.Create;
            try
                am := [ [1  ,   2], [2  ,   3] ];
                bm := [ [0.5, 1.5], [2.5, 3.5] ];
                // Compute with TMtx
                cmRef.Mul(am, bm);  //standard method optimized for large matrices

                jit.Init(am, bm, cm, opNone, opNone, 1, 0);

                // Option 1:
                jit.Mul; //reuse memory allocation by am, cm and bm
                if not cmRef.Equal(cm) then ERaise('Problem');

                // Option 2:
                jit.Mul(am, bm, cm);  // am, cm, bm can use different memory on each call, but a bit slower
                if not cmRef.Equal(cm) then ERaise('Problem');

                // Option 3:
                a := [1  ,   2, 2  ,   3];
                b := [ 0.5, 1.5, 2.5, 3.5 ];
                jit.Init(2, 2, 2, 2, mvDouble, opNone, opNone, 1, 0);

                SetLength(c, 4); //allocate storage to hold the result
                jit.MulKernelDouble(jit.Jitter, a, b, c);

                if not cmRef.Equal(cm) then ERaise('Problem');

                //alternative
            finally
                jit.Free;
            end;
        end;
    </code>
    </Example>*)

  TSmallMatrixMultiply = class
  private
      FloatPrecision: TMtxFloatPrecision;
      ap, bp, cp: PAPointer;
      IsInit: boolean;
      JitKernel: PAPointer;
      calpha, cbeta: TSCplx;
      zalpha, zbeta: TCplx;
      fcRows: integer;
      fcCols: integer;
      faRows: integer;
      faCols: integer;
      fbRows: integer;
      fbCols: integer;
      faOp: TMtxOperation;
      fbOp: TMtxOperation;
  public
      (*<summary> Jitted code pointer. </summary>
                   
<remarks>Do not overwrite. To be passed to MulFloat, MulDouble, MulFloatComplex, MulDoubleComplex
</remarks>
*)
      Jitter: PAPointer;
      (*<summary> Function pointer to the jitted multiplication kernel. </summary>
        
<remarks>The first parameter must be the Jitter field of this object. The A, B and C parameters are pointers to the first
        element of the dense matrix storage (not jagged or other) and can vary between calls.

         It is at least 3x faster as the Delphi unrolled loop matrix multiply even for 2x2 matrices.
</remarks>
*)
      MulKernel: Tgemm_jit_kernel_tp;

      (*<summary> Function pointer to the single precision jitted multiplication kernel. </summary>
        
<remarks>This pointer is not nil, if the Init routine was called with matrix A,B and C.FloatPrecision set to mvSingle.
        The first parameter must be the Jitter field of this object. The A, B and C parameters are pointers to the first
        element of the dense matrix storage (not jagged or other) and can vary between calls.

         It is at least 3x faster as the Delphi unrolled loop matrix multiply.
</remarks>
*)
      
      MulKernelFloat: Tsgemm_jit_kernel_t;
      (*<summary> Function pointer to the complex single precision jitted multiplication kernel. </summary>
              
<remarks>This pointer is not nil, if the Init routine was called with matrix A, B and C.FloatPrecision set to mvSingleComplex.
              The first parameter must be the Jitter field of this object. The A, B and C parameters are pointers to the first
              element of the dense matrix storage (not jagged or other)  and can vary between calls.

         It is at least 3x faster as the Delphi unrolled loop matrix multiply even for 2x2 matrices.
</remarks>
*)

      MulKernelComplexFloat: Tcgemm_jit_kernel_t;
      

      
      (*<summary> Function pointer to the double precision jitted multiplication kernel. </summary>
        
<remarks>This pointer is not nil, if the Init routine was called with matrix A, B and C.FloatPrecision set to mvDouble.
        The first parameter must be the Jitter field of this object. The A, B and C parameters are pointers to the first
        element of the dense matrix storage (not jagged or other) and can vary between calls.

         It is at least 3x faster as the Delphi unrolled loop matrix multiply even for 2x2 matrices.
</remarks>
*)
      MulKernelDouble: Tdgemm_jit_kernel_t;

      (*<summary> Function pointer to the complex double precision jitted multiplication kernel. </summary>
              
<remarks>This pointer is not nil, if the Init routine was called with matrix A, B and C.FloatPrecision set to mvDoubleComplex.
              The first parameter must be the Jitter field of this object. The A, B and C parameters are pointers to the first
              element of the dense matrix storage (not jagged or other) and can vary between calls.

         It is at least 4x faster as the Delphi unrolled loop matrix multiply even for 2x2 matrices.
</remarks>
*)
      MulKernelComplexDouble: Tzgemm_jit_kernel_t;
      

      (*<summary> Frees the internally jitted kernel </summary>*)
      procedure FreeKernel;
      (*<summary> Initialize matrix multiplication </summary>
                   
<remarks>Initializes computation of:

                   C = alpha*opA(A)*opB(B) + beta*C

                   The procedure will initialize matrix multiplication and create in-memory code specifically designed to
                   multiply exactly these matrices with the specified operators. (just-in-time compilation).

                   This design makes the performance efficient multiplication of "small" matrices possible.

                   To perform actual multiplication call one of the Mul methods of this object. The matrices passed must
                   unconditionally match the parameters (size and layout) as they had when they were passed to this (Init) routine.

                   The parameterless Mul routine assumes also the same memory locations at which matrices A, B and C store their data
                   and is slightly faster than the alternative Mul method. This means, that A,B and C may not be resized after calling Init.

                   Init can be called multiple times. Each call will overwrite the previous settings.
                   The alpha and beta parameters are specified as complex type, but only the real part will be used when
                   A, B and C are not complex.

                   Returns true if JIT will be used and false otherwise. If the JIT will not be used standard dgemm routine will
                   be called instead. The procedure of JIT compilation is about 1000x slower, than the actual multiply operation.
                   The kernel(s) therefore must be prepared upfront.

                   The Init routine is protected against initializing with same parameters again and will simply exit and reuse the existing kernel.
</remarks>


                  <param name="A">Matrix A.</param>
                  <param name="B">Matrix B.</param>
                  <param name="C">Matrix C.</param>
                  <param name="opA">Request matrix A transpose.</param>
                  <param name="opB">Request matrix B transpose.</param>
                  <param name="Alpha">Scaling factor.</param>
                  <param name="Beta">Scaling factor.</param>*)
      function Init(const A, B, C: TMtx;  opA: TMtxOperation; opB: TMtxOperation; const Alpha, Beta: TCplx): boolean; overload;
      (*<summary> Initialize matrix multiplication </summary>
                   
<remarks>Initializes computation of:

                   C = alpha*opA(A)*opB(B) + beta*C

                   Returns true if JIT will be used and false otherwise. If the JIT will not be used standard dgemm routine will
                   be called instead. The procedure of JIT compilation is about 1000x slower, than the actual multiply operation.
                   The kernel(s) therefore must be prepared upfront.

                   The Init routine is protected against initializing with same parameters again and will simply exit and reuse the existing kernel.
</remarks>


                  <param name="ARows">Matrix A row count.</param>
                  <param name="ACols">Matrix A column count.</param>
                  <param name="BRows">Matrix B row count.</param>
                  <param name="BCols">Matrix B column count.</param>
                  <param name="opA">Request matrix A transpose.</param>
                  <param name="opB">Request matrix B transpose.</param>
                  <param name="Alpha">Scaling factor.</param>
                  <param name="Beta">Scaling factor.</param>
                  <param name="aFloatPrecision">The storage precision of all three matrices, A, B and C.</param>*)
      function Init(const ARows, ACols, BRows, BCols: integer; aFloatPrecision: TMtxFloatPrecision; opA, opB: TMtxOperation; const Alpha, Beta: double): boolean; overload;
      (*<summary> Initialize matrix multiplication </summary>
                   
<remarks>Initializes computation of:

                   C = alpha*opA(A)*opB(B) + beta*C

                   Returns true if JIT will be used and false otherwise. If the JIT will not be used, the standard dgemm routine will
                   be called instead. The procedure of JIT compilation is about 1000x slower, than the actual multiply operation.
                   The kernel(s) therefore must be prepared upfront.

                   The Init routine is protected against initializing with same parameters again and will simply exit and reuse the existing kernel.
</remarks>


                  <param name="ARows">Matrix A row count.</param>
                  <param name="ACols">Matrix A column count.</param>
                  <param name="BRows">Matrix B row count.</param>
                  <param name="BCols">Matrix B column count.</param>
                  <param name="opA">Request matrix A transpose.</param>
                  <param name="opB">Request matrix B transpose.</param>
                  <param name="Alpha">Scaling factor.</param>
                  <param name="Beta">Scaling factor.</param>
                  <param name="aFloatPrecision">The storage precision of all three matrices, A, B and C.</param>*)
      function Init(const ARows, ACols, BRows, BCols: integer; aFloatPrecision: TMtxFloatPrecision; opA, opB: TMtxOperation; const Alpha, Beta: TCplx): boolean; overload;
      (*<summary> Initialize matrix multiplication </summary>
                   
<remarks>Initializes computation of:

                   C = alpha*opA(A)*opB(B) + beta*C

                   The procedure will initialize matrix multiplication and create in-memory code specifically designed to
                   multiply exactly these matrices with the specified operators. (just-in-time compilation).

                   This design makes the performance efficient multiplication of "small" matrices possible.

                   To perform actual multiplication call one of the Mul methods of this object. The matrices passed must
                   unconditionally match the parameters (size and layout) as they had when they were passed to this (Init) routine.

                   The parameterless Mul routine assumes also the same memory locations at which matrices A, B and C store their data
                   and is slightly faster than the alternative Mul method. This means, that A,B and C may not be resized after calling Init.

                   Init can be called multiple times. Each call will overwrite the previous settings.
                   The alpha and beta parameters are specified as complex type, but only the real part will be used when
                   A, B and C are not complex.

                   The Init routine is protected against initializing with same parameters again and will simply exit and reuse the existing kernel.

                   Returns true if JIT will be used and false otherwise. If the JIT will not be used standard dgemm routine will
                   be called instead. The procedure of JIT compilation is about 1000x slower, than the actual multiply operation.
</remarks>


                  <param name="A">Matrix A.</param>
                  <param name="B">Matrix B.</param>
                  <param name="C">Matrix C.</param>
                  <param name="opA">Request matrix A transpose.</param>
                  <param name="opB">Request matrix B transpose.</param>
                  <param name="Alpha">Scaling factor.</param>
                  <param name="Beta">Scaling factor.</param>*)
      function Init(const A, B, C: TMtx;  opA: TMtxOperation; opB: TMtxOperation; const Alpha, Beta: double): boolean; overload;
      (*<summary> Performs matrix multiplication optimized for small matrices </summary>

         
<remarks>Computes: C = alpha*opA(A)*opB(B) + beta*C

         The operation must first be initialized with a call to the Init method.
         This means, that A,B and C may not be have a different size than when calling Init. Their size however can change in between the calls.

         This routine performs no error checking! Any error checking is performed during the call to the Init routine.

         The method is about 3x slower than the jit.Mul; overload;
         It is about 4x slower than calling  jit.MulKernelDouble(jit.jitter, a,b,c); function
         It is equally fast as the Delphi unrolled loop matrix multiply.
</remarks>
*)
      procedure Mul(const A,B,C: TMtx); overload;
      (*<summary> Performs matrix multiplication optimized for small matrices </summary>

         
<remarks>Computes: C = alpha*opA(A)*opB(B) + beta*C

         The operation must first be initialized with a call to the Init method. The method will use the memory locations
         of parameters A,B and C passed to the Init routine. This means, that A,B and C may not be resized after calling Init even
         if they restore the size and layout before calling this routine. There is no other dependence and all objects
         can be free independently.

         Calling TMtx.Size multiple times will not trigger a resize or memory allocation unless the size has changed.

         This routine performs no error checking! Any error checking is performed during the call to the Init routine.

         The method is about 2x faster than the jit.Mul(a,b,c) overload;
         It is about 1.3% slower than calling  jit.MulKernelDouble(jit.jitter, a,b,c); function
         It is 3x faster than the Delphi's unrolled-loop matrix multiply.
</remarks>
*)
      procedure Mul; overload; inline;
      (*<summary> The object is required to be freed manually. </summary>*)
      destructor Destroy; override;
      (*<summary> Field computed by Init method. Holds the row count of the matrix to hold the result. </summary>*)
      property cRows: integer read fcRows;
      (*<summary> Field computed by Init method. Holds the row count of the matrix to hold the result. </summary>*)
      property cCols: integer read fcCols;
  end;


       

       (*<summary> Generic thread object which can work with object cache.</summary>
                   
<remarks>Use this thread type when objects obtained from
                   object cache via Createit/FreeIt are used by the thread. Using
                   this thread type will result in super conductive memory manager avoiding
                   all thread contention allowing perfect parallelism, but only if default
                   memory manager is not used at all or very sparingly within the thread.
                   This thread object simply calls TMtxVecController.UnMarkThread before the thread
                   terminates.
</remarks>
*)


       TMtxThreadInternal = class(TThread)
       strict private
          fOnExecute: TNotifyEvent;
       protected
          procedure Execute; override;
          procedure SuperExecute; virtual;
          procedure SetOnExecute(const Value: TNotifyEvent);
       public
          property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
       end;

       TMtxThread = class(TMtxThreadInternal)
       protected
          procedure Execute; reintroduce; 
       end;

       

    
    TVecDouble = TVec;
    TMtxDouble = TMtx;
    



        (*<summary>Releases objects back to the object cache.</summary>
         
<remarks>Releases objects back to the object cache and sets the
         parameters to nil. If the parameters were not from
         the object cache, they are destroyed.
</remarks>


         <SeeAlso cref="CreateIt"/>*)
        
        procedure FreeIt(var a,b,c,d: TMtx); overload;
        (*<summary>Release three matrices.</summary>*)
        
        procedure FreeIt(var a,b,c: TMtx); overload;
        (*<summary>Release two matrices.</summary>*)
        
        procedure FreeIt(var a,b: TMtx); overload;
        (*<summary>Release one matrix.</summary>*)
        
        procedure FreeIt(var a: TMtx); overload;

       (*<summary>Releases objects back to the object cache.</summary>
         
<remarks>Releases objects back to the object cache and sets the
         parameters to nil. If the parameters were not from
         the object cache, they are destroyed.
</remarks>
*)
        
        procedure FreeIt(var a,b,c,d: TVec); overload;
        (*<summary>Release three vectors.</summary>*)
        
        procedure FreeIt(var a,b,c: TVec); overload;
        (*<summary>Release two vectors.</summary>*)
        
        procedure FreeIt(var a,b: TVec); overload;
        (*<summary>Release one vector.</summary>*)
        
        procedure FreeIt(var a: TVec); overload;

        (*<summary>Obtains an object from the object cache.</summary>
          
<remarks>Obtains an object from the object cache.
</remarks>
*)
        
        function CreateIt: TVec; overload;
        (*<summary>Obtains a TVec object from the object cache.</summary>
          
<remarks>Obtains a TVec object from the object cache.
          If the cache is used, it creates a new object.
          To release an object from the cache, use <See Routine="FreeIt"/>
</remarks>


          <SeeAlso cref="FreeIt"/>*)
        
        procedure CreateIt(out a: TVec); overload;
        (*<summary>Obtains two TVec objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b: TVec); overload;
        (*<summary>Obtains three TVec objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b,c: TVec); overload;
        (*<summary>Obtains four TVec objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b,c,d: TVec); overload;

        (*<summary>Obtains a TMtx object from the object cache.</summary>
          
<remarks>Obtains a TMtx object from the object cache.
          If the cache is used, it creates a new object.
          To release an object from the cache, use <See Routine="FreeIt"/>
</remarks>
*)
        
        procedure CreateIt(out a: TMtx); overload;
        (*<summary>Obtains two TMtx objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b: TMtx); overload;
        (*<summary>Obtains three TMtx objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b,c: TMtx); overload;
        (*<summary>Obtains four TMtx objects from the object cache.</summary>*)
        
        procedure CreateIt(out a,b,c,d: TMtx); overload;


        (*<summary>Raises exception if PAPointer is nil.</summary>
          
<remarks>Raises exception any of the PAPointer variables is nil.
</remarks>


          <Example>
          <code>
          var a,b: TVec;
          begin
            a := TVec.Create;
            b := TVec.Create;
            try
              // ...
            finally
              a.Free;
            end;
            TestNil(a,b);
          </code>
          </Example>*)
        
        procedure TestNil(a,b,c,d: PAPointer); overload;
        (*<summary>Test three pointers, if they are nil.</summary>*)
        
        procedure TestNil(a,b,c: PAPointer); overload;
        (*<summary>Test two pointers, if they are nil.</summary>*)
        
        procedure TestNil(a,b: PAPointer); overload;
        (*<summary>Test one pointer, if it equals nil.</summary>*)
        
        procedure TestNil(a: PAPointer); overload;

        (*<summary>Dereferences a pointer to TMtxVec memory.</summary>
          
<remarks>Dereferences pointer's to memory of four TMtx objects. It constructs a
          2D jagged floating point array. The range checks must be off, when working
          on this array. The array's must be "cleaned up" by passing them to DismissIt
          before the routine ends. Special care must be given not to destroy the objects
          that were dereferenced, before the arrays are freed.

          Note
            Under .NET this command simply copies data from objects to the arrays.
</remarks>
*)
        
        procedure Enlist(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtx objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtx; var ap,bp,cp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtx objects.</summary>*)
        
        procedure Enlist(a,b: TMtx; var ap,bp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) a pointer to memory of a TMtx object.</summary>*)
        
        procedure Enlist(a: TMtx; var ap: T2DDoubleArray); overload;

        
        procedure Enlist(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DSingleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtx objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtx; var ap,bp,cp: T2DSingleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtx objects.</summary>*)
        
        procedure Enlist(a,b: TMtx; var ap,bp: T2DSingleArray); overload;
        (*<summary>Dereferences (.NET copies) a pointer to memory of a TMtx object.</summary>*)
        
        procedure Enlist(a: TMtx; var ap: T2DSingleArray); overload;


        (*<summary>Dereferences a pointer to TMtxVec memory.</summary>
          
<remarks>Dereferences pointer's to memory of four TMtxVec objects.
          The range checking will work fine. The array's must be "cleaned up" by
          passing them to DismissIt before the routine ends.Special care must be given not
          to destroy the objects that were dereferenced, before the arrays are freed.

          Note
            Under .NET this command simply copies data from objects to the arrays.
</remarks>
*)
        
        procedure Enlist(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtxVec; var ap,bp,cp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b: TMtxVec; var ap,bp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer to memory of one TMtxVec object.</summary>*)
        
        procedure Enlist(a: TMtxVec; var ap: TDoubleArray); overload;

        
        procedure Enlist(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TSingleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtxVec; var ap,bp,cp: TSingleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b: TMtxVec; var ap,bp: TSingleArray); overload;
        (*<summary>Dereferences (.NET copies) pointer to memory of one TMtxVec object.</summary>*)
        
        procedure Enlist(a: TMtxVec; var ap: TSingleArray); overload;

        (*<summary>Dereferences (.NET copies) pointer's to memory of four TMtxVec objects
           as a jagged 2D array of complex numbers.</summary>*)
        
        procedure Enlist(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtx; var ap,bp,cp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of Two TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b: TMtx; var ap,bp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure Enlist(a: TMtx; Var ap: T2DCplxArray); overload;

        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects
          as an array of complex numbers. </summary>*)
        
        procedure Enlist(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtxVec; var ap,bp,cp: TCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b: TMtxVec; var ap,bp: TCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure Enlist(a: TMtxVec; var ap: TCplxArray); overload;


        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects
          as an array of complex numbers. </summary>*)
        
        procedure Enlist(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b,c: TMtxVec; var ap,bp,cp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure Enlist(a,b: TMtxVec; var ap,bp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET copies) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure Enlist(a: TMtxVec; var ap: TSCplxArray); overload;

        (*<summary>Free arrays allocated with a call to Enlist.</summary>
          
<remarks>Free arrays allocated with a call to Enlist. The version taking TMtxVec
          as a parameter under .NET also copies the values from the array back to the object
          before freeing the array. This is usefull for writing code that can execute
          both under W32 and .NET.
</remarks>
*)
        
        procedure DismissIt(var ap,bp,cp,dp: T2DDoubleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: T2DDoubleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: T2DDoubleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: T2DDoubleArray); overload;


        
        procedure DismissIt(var ap,bp,cp,dp: T2DSingleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: T2DSingleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: T2DSingleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: T2DSingleArray); overload;


        (*<summary>Free four arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp,dp: TDoubleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: TDoubleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: TDoubleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: TDoubleArray); overload;


        (*<summary>Free four arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp,dp: TSingleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: TSingleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: TSingleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: TSingleArray); overload;

        (*<summary>Free four arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp,dp: T2DCplxArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: T2DCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: T2DCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: T2DCplxArray); overload;

        (*<summary>Free four arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp,dp: TCplxArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: TCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: TCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: TCplxArray); overload;


        (*<summary>Free four arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp,dp: TSCplxArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp,cp: TSCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap,bp: TSCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(var ap: TSCplxArray); overload;


        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DDoubleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtx; var ap,bp,cp: T2DDoubleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtx; var ap,bp: T2DDoubleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtx; var ap: T2DDoubleArray); overload;


        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DSingleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtx; var ap,bp,cp: T2DSingleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtx; var ap,bp: T2DSingleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtx; var ap: T2DSingleArray); overload;



        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TDoubleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtxVec; var ap,bp,cp: TDoubleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtxVec; var ap,bp: TDoubleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtxVec; var ap: TDoubleArray); overload;


        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TSingleArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtxVec; var ap,bp,cp: TSingleArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtxVec; var ap,bp: TSingleArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtxVec; var ap: TSingleArray); overload;


        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtx; var ap,bp,cp,dp: T2DCplxArray); overload;
        (*<summary>Free three arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtx; var ap,bp,cp: T2DCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtx; var ap,bp: T2DCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtx; var ap: T2DCplxArray); overload;

        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtxVec; var ap,bp,cp: TCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtxVec; var ap,bp: TCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtxVec; var ap: TCplxArray); overload;



        (*<summary>Free arrays allocated with a call to Enlist and under .NET also copy
           the contents of arrays back to the coresponding objects.</summary>*)
        
        procedure DismissIt(a,b,c,d: TMtxVec; var ap,bp,cp,dp: TSCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b,c: TMtxVec; var ap,bp,cp: TSCplxArray); overload;
        (*<summary>Free two arrays allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a,b: TMtxVec; var ap,bp: TSCplxArray); overload;
        (*<summary>Free one array allocated with a call to Enlist.</summary>*)
        
        procedure DismissIt(a: TMtxVec; var ap: TSCplxArray); overload;

        (*<summary>Dereferences a pointer to TMtxVec memory.</summary>
          
<remarks>Dereferences pointer's to memory of four TMtx objects. It constructs a
          2D jagged floating point array. The range checks must be off, when working
          on this array. The array's must be "cleaned up" by passing them to DismissIt
          before the routine ends. Special care must be given not to destroy the objects
          that were dereferenced, before the arrays are freed.<para/>

          Note
            Under .NET this command only resizes the array to match the size of the objects.
</remarks>
*)
        
        procedure EnlistIt(a,b,c,d: TMtx; out ap,bp,cp,dp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtx; out ap,bp,cp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtx; out ap,bp: T2DCplxArray); overload;
        (*<summary>Dereferences (.NET sizes the array only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtx; out ap: T2DCplxArray); overload;

        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of four TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c,d: TMtx; out ap,bp,cp,dp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtx; out ap,bp,cp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtx; out ap,bp: T2DDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes the array only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtx; out ap: T2DDoubleArray); overload;

        (*<summary>Dereferences a pointer to TMtxVec memory.</summary>
          
<remarks>Dereferences pointer's to memory of four TMtxVec objects.
          The range checking will work fine. The array's must be "cleaned up" by
          passing them to DismissIt before the routine ends.Special care must be given not
          to destroy the objects that were dereferenced, before the arrays are freed.

          Note
            Under .NET this command only resizes the array to match the size of the objects.
</remarks>
*)
        
        procedure EnlistIt(a,b,c,d: TMtxVec; out ap,bp,cp,dp: TCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtxVec; out ap,bp,cp: TCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtxVec; out ap,bp: TCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtxVec; out ap: TCplxArray); overload;


        
        procedure EnlistIt(a,b,c,d: TMtxVec; out ap,bp,cp,dp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtxVec; out ap,bp,cp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtxVec; out ap,bp: TSCplxArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtxVec; out ap: TSCplxArray); overload;

        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of four TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c,d: TMtxVec; out ap,bp,cp,dp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtxVec; out ap,bp,cp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtxVec; out ap,bp: TDoubleArray); overload;
        (*<summary>Dereferences (.NET sizes the array only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtxVec; out ap: TDoubleArray); overload;


        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of four TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c,d: TMtxVec; out ap,bp,cp,dp: TSingleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of three TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b,c: TMtxVec; out ap,bp,cp: TSingleArray); overload;
        (*<summary>Dereferences (.NET sizes arrays only) pointer's to memory of two TMtxVec objects.</summary>*)
        
        procedure EnlistIt(a,b: TMtxVec; out ap,bp: TSingleArray); overload;
        (*<summary>Dereferences (.NET sizes the array only) pointer's to memory of TMtxVec object.</summary>*)
        
        procedure EnlistIt(a: TMtxVec; out ap: TSingleArray); overload;


        (*<summary>Computes the Median from the source data. </summary>*)
        
        function Median(const X: TDenseMtxVec): double; overload; 


        (*<summary>Returns the maximum of the source data. </summary>*)
        
        function Maxc(const X: TDenseMtxVec): TCplx; overload; 

        (*<summary>Returns the minimum of the source data. </summary>*)
        
        function Minc(const X: TDenseMtxVec): TCplx; overload; 

        (*<summary>Returns the average value of the source data. </summary>*)
        
        function Meanc(const X: TDenseMtxVec): TCplx; overload; 

        (*<summary>Returns the product of all elements in the source data. </summary>*)
        
        function Product(const X: TMtxVec): double; overload; 

        (*<summary>Returns the product of all elements in the source data. </summary>*)
        
        function Productc(const X: TMtxVec): TCplx; overload; 

        (*<summary>Compute the standard deviation from the source data. </summary>*)
        
        function StdDev(const X: TMtxVec): double; overload; 

        (*<summary>Returns the sum of elements in the source data. </summary>*)
        
        function Sum(const X: TMtxVec): double; overload; 

        (*<summary>Returns the sum of elements in the source data. </summary>*)
        
        function Sumc(const X: TMtxVec): TCplx; overload; 

        (*<summary>Returns the sum of squares of the source data. </summary>*)
        
        function SumOfSquares(const X: TDenseMtxVec): double; overload; 

        (*<summary>Vector Root Mean square.</summary>
          <returns>Vector RMS.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function RMS(const Vec: TDenseMtxVec): double; overload;
        (*<summary>RMS for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function RMS(const Vec: TDenseMtxVec; Index,Len: integer): double; overload;
        (*<summary>Vector standard deviation.</summary>
          <returns>Vector standard deviation.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>
          <param name="AMean">The average value computed from the Vec parameter.</param>*)
        
        function StdDev(const Vec: TDenseMtxVec; AMean: double): double; overload;
        (*<summary>Standard deviation for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function StdDev(const Vec: TDenseMtxVec; AMean: TCplx): TCplx; overload;
        (*<summary>Vector Norm-L1.</summary>
          <returns>Vector Norm-L1.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function NormL1(const Vec: TDenseMtxVec): double; overload;
        (*<summary>|L1| norm for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function NormL1(const Vec1,Vec2: TDenseMtxVec; RelativeError: boolean = False): double; overload;
        (*<summary>Vector L2-Norm.</summary>
          <returns>Vector L2-Norm</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function NormL2(const Vec: TDenseMtxVec): double; overload;
        (*<summary>|L2| norm for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function NormL2(const Vec1,Vec2: TDenseMtxVec; RelativeError: boolean = False): double; overload;
        (*<summary>Vector C-Norm.</summary>
          <returns>Vector C-Norm</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated."/></param>*)
        
        function NormC(const Vec: TDenseMtxVec): double; overload;
        (*<summary>|C| norm for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function NormC(const Vec1,Vec2: TDenseMtxVec; RelativeError: boolean = False): double; overload;
        (*<summary>Is number a prime number?</summary>
          <returns>True, if X is prime number.</returns>
          <param name="X">Evaluated nunmber.</param>*)
        
        function Prime(const X: integer): boolean;
       (*<summary>Vector kurtosis.</summary>
          <returns>Vector kurtosis value.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>
          <param name="AMean">The average value computed from the Vec parameter.</param>
          <param name="AStdDev">The standard deviation computed from the Vec parameter.</param>*)
        
        function Kurtosis(const Vec: TDenseMtxVec; AMean, AStdDev: double): double; overload;
       (*<summary>Vector skewness</summary>
          <returns>Vector skewness.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>
          <param name="AMean">The average value computed from the Vec parameter.</param>
          <param name="AStdDev">The standard deviation computed from the Vec parameter.</param>*)
        
        function Skewness(const Vec: TDenseMtxVec; AMean, AStdDev: double): double; overload;
        
        function Equal(const Vec1,Vec2: TVec; Tolerance: double = 0): boolean; overload;
         (*<summary>Vector median.</summary>
          <returns>Vector median value.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function Median(const Vec: TVec): double; overload;
         (*<summary>Vector mean.</summary>
          <returns>Vector mean value.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function Mean(const Vec: TDenseMtxVec):double; overload;
        (*<summary>Mean valur for Vector values <c>[Index,Index+Len]</c>, starting at <c>Index</c>.</summary>*)
        
        function Mean(const Vec: TDenseMtxVec; Index, Len: integer): double; overload;
         (*<summary>Vector sum.</summary>
          <returns>Sum of Vector values.</returns>
          <param name="Vec">Calling vector or matrix object for which function will be evaluated.</param>*)
        
        function Min(const Vec: TDenseMtxVec): double; overload;
        
        function Max(const Vec: TDenseMtxVec): double; overload;
        (*<summary>Matrix trace.</summary>
          <returns>Matrix trace.</returns>
          <param name="Mtx">Calling matrix object for which function will be evaluated.</param>*)
        
        function Trace(const Mtx: TMtx): TCplx; overload;
        
        function Equal(Mtx1,Mtx2: TMtx; Tolerance: double = 0): boolean; overload;
        
        function Find(const Vec: TDenseMtxVec; const X: TCplx): integer; overload;
        
        function Find(const Vec: TDenseMtxVec; const X: double): integer; overload;
        (*<summary>Matrix Norm-1.</summary>
          <returns>Matrix Norm-1.</returns>
          <param name="Mtx">Calling matrix object for which function will be evaluated.</param>*)
        
        function Norm1(const Mtx: TMtx): double; overload;
        (*<summary>Matrix infinite norm.</summary>
          <returns>Matrix infinite norm.</returns>
          <param name="Mtx">Calling matrix object for which function will be evaluated.</param>*)
        
        function NormInf(const Mtx: TMtx): double; overload;
        
        function ColumnCount(aList: TStrings; Row: integer; const Delimiter: string = kTab): integer; overload;
       
        
        function SizeOfPrecision(Precision: TPrecision; aIsComplex: Boolean): integer;
        
        procedure SetVecSize(Length: integer; FloatPrecision: TMtxFloatPrecision;  const Vec: array of TVec); overload;
        
        procedure SetMtxSize(Rows,Cols: integer; FloatPrecision: TMtxFloatPrecision; const Mtx: array of TMtx); overload;
        
        function PrecisionToStr(Precision: TPrecision): string;
        
        procedure MtxVecPrint(const vecs: array of TVec; const Names: array of string); overload;
        
        procedure MtxVecPrint(const mtxs: array of TMtx; const Names: array of string); overload;
        
        procedure MtxVecPrint(const cplxs: array of TCplx; const Names: array of string); overload;
        
        procedure MtxVecPrint(const vals: array of double; const Names: array of string); overload;
        
        procedure MtxVecPrint(const cplxs: array of TSCplx; const Names: array of string); overload;
        
        procedure MtxVecPrint(const vals: array of single; const Names: array of string); overload;
        
        function MtxTypeToString(matrixType: TMtxType): string; overload;
        
        procedure ShowMessageBox(const Msg: string); overload;
        
        procedure EigVecExpand(VR, Work: TMtx; WI: TVec); overload;
        
        procedure FindIndexes(const Dst: TVecInt; const a: TMtxVec; const op: string; const b: TMtxVec); overload;
        
        procedure FindIndexes(const Dst: TVecInt; const a: TMtxVec;const  op: string; const b: double); overload;
        
        procedure FindIndexes(const Dst: TVecInt; const a: TMtxVec; const op: string; const b: TCplx); overload;
        
        function aGetLength(const src: array of TMtx): integer; overload;
    
    
    

    
    
    

      
      var
          (*<summary>Controls how the TVec/TMtx assign operation works.</summary>
          
<remarks>If True, the assign operation, will also copy the data and not only
          object's published properties.
</remarks>
*)
         MtxVecAssignCopyData: boolean = True;

         (*<summary>MtxVec reporting object.</summary>*)
         Report: TMtxVecReport;

         (*<summary>MtxVec object cache controller.</summary>*)
         Controller: TMtxVecController;



