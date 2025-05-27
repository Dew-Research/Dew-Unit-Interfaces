










(*<summary>Implements base classes for vector, matrix and sparse matrix.</summary>
  
<remarks>Implements base classes for vector, matrix and sparse matrix. All end-user objects are created from this base classes.
</remarks>
*)
unit AbstractMtxVec;
                           

{$I BdsppDefs.inc}

interface

uses  Math387, IppsplTypes, MtxVecBase, MtxVecInt, AbstractMtxVecInt

            
    ,Lapack_dfti, DFTIDescriptors
    

       
    ,Nmkl
    

        
    ,NmklSingle
    

        
    ,Ippspl
    

    
    ,ippsplSingle
    

    ,MtxVecUtils


      ,Classes
      ,SysUtils






    ;


{$WARN SYMBOL_DEPRECATED ON}

{$Z4}





type

   
   TMtxVec = class;
   TDenseMtxVec = class;
   TDoubleFunc = function (const v: double): double;
   TSingleFunc = function (const v: single): single;
   TComplexFunc = function (const v: TCplx): TCplx;
   TSComplexFunc = function (const v: TSCplx): TSCplx;
   





  (*<summary> Abstract class for TVec and TMtx. </summary>*)
  TMtxVec = class(TMtxVecSample)
  strict private
    FDirty: boolean;
    fZeroTolerance: double;
    procedure setDirty(const value: boolean);
    procedure setComplex(const value: boolean);
    procedure setPrecision(const value: TPrecision);
    procedure setRounding(const value: TRounding);
    procedure setZeroTolerance(const value: double);
    procedure SetFloatPrecision(const value: TMtxFloatPrecision);

    function GetFirst: double;
    function GetFirstc: TCplx;
    function GetLast: double;
    function GetLastc: TCplx;

    procedure SetFirst(const value: double);
    procedure SetFirstc(const value: TCplx);
    procedure SetLast(const value: double);
    procedure SetLastc(const value: TCplx);
  public

   

   
   
    function LoadFromStream(const Src: TStream): Int64; override;
    procedure SaveToStream(const Dst: TStream); override;
   
   

  strict protected
    
    RISeed: cardinal;
    GISeed: cardinal;

    COffset: integer;

    fPrecision: TPrecision;
    fRounding: TRounding;
    function GetRealValues: string; virtual;
    function GetComplexValues: string; virtual;
    procedure ReleaseMemory; override;
    function ElemSize: Integer; override;
    function InternalResize(Len: integer; ZeroIt: boolean = False): TMtxVec; overload; virtual;
    procedure HookPointers; override;
    procedure DoSetComplex; virtual;

    function DoubleCachePointer: TDoubleArray; virtual;
    function DoubleCacheOffset: integer; virtual;
    function CplxDoubleCachePointer: TCplxDoubleArray; virtual;
    function CplxDoubleCacheOffset: integer; virtual;
    function SingleCachePointer: TSingleArray; virtual;
    function SingleCacheOffset: integer; virtual;
    function CplxSingleCachePointer: TCplxSingleArray; virtual;
    function CplxSingleCacheOffset: integer; virtual;

    procedure SubRangeLoadState; override;

    procedure AllocMemory; override;
    procedure AllocCacheMemory; override;

    procedure SetCapacityInElements(const value: Int64); override;
    function GetCapacityInElements: Int64; override;



    procedure ValidateRealIndexLen (const Index, RealSrcLen: integer);  

    
    procedure Sin (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer); overload;  
    procedure Cos (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Tan (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Cot (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Sec (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Csc (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcSin (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCos (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcTan (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCot (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcSec (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCsc (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Sinh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Cosh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Tanh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Coth (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Sech (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Csch(const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcSinh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCosh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcTanh (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCoth (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcSech (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure ArcCsch (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Sqr  (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Sqrt (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Cbrt (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Ln   (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Log10(const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Log2 (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Exp  (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Exp2 (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Exp10(const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Inv  (const Src,Dst: TMtxVec; Threshold: double; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Inv  (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure InvSqrt (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure InvCbrt (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure Abs  (const Src,Dst: TMtxVec; const SrcIdx,DstIdx, Len: integer ); overload; 
    procedure MulI (const Src,Dst: TMtxVec; SrcIndex, DstIndex, Len: integer); overload; 
    procedure Flip (const Src,Dst: TMtxVec; const SrcIdx,DstIdx,Len: integer); overload; 
    procedure FlipConj(const Src,Dst: TMtxVec; const SrcIdx,DstIdx,Len: integer); overload; 

    procedure Copy (const Src,Dst: TMtxVec; const SrcIdx,DstIdx,Len: integer); overload; 
    procedure CopyTo(const Src,Dst: TMtxVec;const SrcIdx,DstIdx,Len: integer); overload; 
    function StripNanAndInf(const Src, Dst: TMtxVec; SrcIdx, DstIdx, Len: integer): integer; overload; 

    procedure SinhCosh(const Src, SinhX, CoshX: TMtxVec;const SrcIndex, SinhIndex, CoshIndex, Len: integer); overload; 

    procedure CopyFromArray(const Src: TDoubleArray; const Dst: TMtxVec; const SrcIdx, DstIdx, Len: integer); overload; 
    procedure CopyFromArray(const Src: TSingleArray; const Dst: TMtxVec; const SrcIdx, DstIdx, Len: integer); overload;  
    procedure CopyFromArray(const Src: TCplxArray; const Dst: TMtxVec; const SrcIdx, DstIdx, Len: integer); overload;  
    procedure CopyFromArray(const Src: TSCplxArray; const Dst: TMtxVec; const SrcIdx, DstIdx, Len: integer); overload;  

    procedure CopyFromArray(const Src: TIntegerArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override; 
    procedure CopyFromArray(const Src: TSmallIntArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override; 
    procedure CopyFromArray(const Src: Math387.TByteArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override; 
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TDoubleArray; const Index,DstIdx,Len: integer); overload; override; 
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TSingleArray; const Index,DstIdx,Len: integer); overload; override; 

    procedure CopyToArrayInternal(const Src: TMtxVec; const Dst: TSCplxArray; const Index,DstIdx,Len: integer); overload;  
    procedure CopyToArrayInternal(const Src: TMtxVec; const Dst: TCplxArray; const Index,DstIdx,Len: integer); overload;  
    procedure CopyToArrayInternal(const Src: TMtxVec; const Dst: TIntegerArray; Rounding: TRounding; const Index,DstIdx,Len: integer); overload; 
    procedure CopyToArrayInternal(const Src: TMtxVec; const Dst: TSmallIntArray; Rounding: TRounding; const Index,DstIdx,Len: integer); overload; 
    procedure CopyToArrayInternal(const Src: TMtxVec; const Dst: Math387.TByteArray; Rounding: TRounding; const Index,DstIdx,Len: integer); overload; 


    function DotProdc(const Src1,Src2: TMtxVec; const ConjVec: boolean; const VecIndex, Index, Len: integer): TCplx; overload; 

    procedure RoundTrunc(const Src,Dst: TMtxVec; const Rounding: TRounding; const SrcIndex,DstIndex,Len: integer); overload; 

    function Skewness(const Src: TMtxVec;const AMean, AStdDev: double; const Index,Len: integer): double; overload; 
    function Kurtosis(const Src: TMtxVec;const AMean, AStdDev: double; const  Index,Len: integer): double; overload; 



    procedure setIsDouble(const value: boolean); override;

    


   public

    (*<summary>Allows setting/getting the real value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="CValues"/> properties.
</remarks>
*)
     Values:  TDoubleArray;
    (*<summary>Allows setting/getting the complex value at position Indx.</summary>
      
<remarks>Allows setting/getting the complex value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="CValues"/> properties.
</remarks>
*)
     CValues: TCplxArray;

     (*<summary>Allows setting/getting the real value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
     This property reads/writes to the same memory as <see cref="Values"/> and <see cref="Values"/> properties.
</remarks>
*)
     Values1D: TDoubleArray;
     (*<summary>Allows setting/getting the complex value at position Indx.</summary>
      
<remarks>Allows setting/getting the complex value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="Values"/> properties.
</remarks>
*)
     CValues1D: TCplxArray;


    (*<summary>Allows setting/getting the real value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="SCValues"/> properties.
</remarks>
*)
     SValues:  TSingleArray;
    (*<summary>Allows setting/getting the complex value at position Indx.</summary>
      
<remarks>Allows setting/getting the complex value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="SValues"/> properties.
</remarks>
*)
     SCValues: TCplxSingleArray;

     (*<summary>Allows setting/getting the real value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
     This property reads/writes to the same memory as <see cref="Values"/> and <see cref="SValues"/> properties.
</remarks>
*)
     SValues1D: TSingleArray;
     (*<summary>Allows setting/getting the complex value at position Indx.</summary>
      
<remarks>Allows setting/getting the complex value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> and <see cref="SValues"/> properties.
</remarks>
*)
     SCValues1D: TCplxSingleArray;


      
  strict protected
      function get_DefaultArray(const Indx: integer): double; 
      procedure set_DefaultArray(const Indx: integer; const value: double); 
      


  public
    (*<summary>Additional parameter for certain Lapack methods.</summary>
      
<remarks>Additional parameter for certain Lapack methods. When this value
      is used, it is mentioned in the method description.

      This parameter has been made a field of the object, to reduce
      the number of parameters required by a method, because it is not
      used very often.
</remarks>
*)
    Beta: TCplx;
    (*<summary>Additional parameter for certain Lapack methods.</summary>
      
<remarks>Additional parameter for certain Lapack methods. When this value
      is used, it is mentioned in the method description.

      This parameter has been made a field of the object, to reduce
      the number of parameters required by a method, because it is not
      used very often.
</remarks>
*)
    Alfa: TCplx;

    (*<summary>Defines the precision (single, float) and type (real, complex) of the floating point operations. </summary>*)
    property FloatPrecision: TMtxFloatPrecision read fFloatPrecision write SetFloatPrecision;

    
    property RealValues: string read GetRealValues;
    property ComplexValues: string read GetComplexValues;
    

    (*<summary>Defines the rounding used by streaming routines.</summary>

      <SeeAlso cref="TMtxVecBase.SaveToStream"/>
      <SeeAlso cref="TMtxVecBase.SaveToFile"/>*)
    property Rounding: TRounding read FRounding write setRounding;
    (*<summary>Defines the precision used by streaming routines.</summary>

      <SeeAlso cref="TMtxVecBase.SaveToStream"/>
      <SeeAlso cref="TMtxVecBase.SaveToFile"/>*)
    property Precision: TPrecision read FPrecision write setPrecision;
    (*<summary>True after object property changes.</summary>
      
<remarks>Set this to true after <see cref="TMtxVec"/> property has changed. Initially when the object is created this property is set to false. But when a
      change to object or it's elements is made, Dirty can be changed to true. Use this property to determine if
      the calling object and it's properties have changed. This property never changes
      automatically.
</remarks>
*)
    property Dirty: boolean read FDirty write setDirty;
    (*<summary>The tolerance used by <see cref="IsEqual"/>.</summary>
      
<remarks>The tolerance used by <see cref="IsEqual"/> when the comparison type is relative.
</remarks>
*)
    property ZeroTolerance: double read fZeroTolerance write setZeroTolerance;

    

    (*<summary>Allows setting/getting the real value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
     This property reads/writes to the same memory as <see cref="Values"/> and <see cref="IValues"/> properties.
</remarks>
*)
    property DefaultArray[const Indx: integer]: Double read get_DefaultArray write set_DefaultArray; default;

    

    
    procedure Assign(Src: TPersistent);  override;
    procedure ValidateParams2 (const X: TMtxVec; const XIndex: integer;const Index: integer; var Len: integer); overload; 
    procedure ValidateParamsPrecision(const X1: TMtxVec; const xIndex1, Index: integer; var Len: integer); overload; 
    procedure ValidateParamsPrecision(const X1, X2: TMtxVec; const xIndex1,xIndex2,Index: integer; var Len: integer); overload;
    procedure ValidateParamsPrecision(const X1, X2, X3: TMtxVec; const xIndex1, xIndex2, xIndex3, Index: integer; var Len: integer); overload;


    

    function IsComplex: boolean; virtual;

    (*<summary>Finds a match for X in sorted object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
</remarks>

      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>*)

    function BinarySearch(const x: double): Integer; overload;
    function BinarySearch(const x: double; const Index: integer; Len: integer): Integer; overload;

    function BinarySearch(const x: TCplx): Integer; overload;
    function BinarySearch(const x: TCplx; const Index: integer; Len: integer): Integer; overload;

    (*<summary>Finds exact or closest index match for X in sorted object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
               The closest match is the index of first bigger or smaller value in the array.

              To ensure bigger value write:

              <code>
              Data := [0,2,3];
              Data.BinarySearch(Value, XIndex);
              if Data[XIndex] &gt; Value then Dec(XIndex);
              </code>

              To ensure smaller value write:

              <code>
              Data := [0,2,3];
              Data.BinarySearch(1, XIndex);
              if Data[XIndex] &lt; Value then Inc(XIndex);
              </code>
</remarks>


      <returns>True and exact index in XIndex, if found and False and the index of the next bigger or smaller value in XIndex, if not found. </returns>*)

    function BinarySearch(const x: double; var XIndex: integer): boolean; overload;
    function BinarySearch(const x: double; var XIndex: integer; const Index: integer; Len: integer): boolean; overload;

    function BinarySearch(const x: TCplx; var XIndex: integer): boolean; overload;
    function BinarySearch(const x: TCplx; var XIndex: integer; const Index: integer; Len: integer): boolean; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Sin(); // Computes complex sine
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)
    function Sin: TMtxVec; overload; 
    (*<summary>Calculate the sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(Index: integer; Len: integer): TMtxVec; overload;
    (*<summary>Calculate the sine of all X object elements and store the results in calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sin(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(const X: TMtxVec; XIndex: integer; Index: integer; Len: integer): TMtxVec; overload; 

    (*<summary>Cosine.</summary>
      
<remarks>Calculate the cosine of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Cos(); // Computes complex sine
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCos"/>
      <SeeAlso cref="SinCos"/>*)
    function Cos: TMtxVec; overload; 
    (*<summary>Calculate the cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(Index: integer; Len: integer): TMtxVec; overload;
    (*<summary>Calculate the cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object.vSize and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Cos(const X: TMtxVec): TMtxVec; overload;
    (*<summary>Calculate the cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object and the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(const X: TMtxVec; XIndex: integer; Index: integer; Len: integer): TMtxVec; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Tan(); // Computes complex tangens
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)
    function Tan: TMtxVec; overload; 
    (*<summary>Calculate the tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan (Index: integer; Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tan(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan(const X: TMtxVec; XIndex: integer; Index: integer; Len: integer): TMtxVec; overload; 

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Cot(); // Computes complex cotangens
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)
    function Cot: TMtxVec; overload; 
    (*<summary>Calculate the cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cot(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Sec(); // Computes complex secant
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)
    function Sec: TMtxVec; overload; 
    (*<summary>Calculate the secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the secant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sec(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Csc(); // Computes complex cosecant
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)
    function Csc: TMtxVec; overload; 
    (*<summary>Calculate the cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csc(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the cosecant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-0.5,0.11,0.9});
            a.ArcSin(); // Computes complex inverse sine
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>*)
    function ArcSin: TMtxVec; overload; 
    (*<summary>Calculate the inverse sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSin(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-0.5,0.11,0.9});
            a.ArcCos(); // Computes complex inverse cosine
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos: TMtxVec; overload; 
    (*<summary>Calculate the inverse cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCos(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object elements. Size and <see cref="Complex"/> properties of calling object are adjusted automatically
      to match those of X and Y objects. An exception is raised if X and Y size and <see cref="Complex"/> properties do not match.

      Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(const Y, X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse tangens of Y/X.</summary>
      
<remarks>Calculation uses Y elements [YIndex]..[YIndex+Len-1], X elements [XIndex]..[XIndex+Len-1]
      and stores the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan2(const Y, X: TMtxVec; YIndex, XIndex, Index: integer; Len: integer): TMtxVec; overload; 

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TMtx A,B;
          MtxVec.CreateIt(out A, out B);
          try
          {
            A.SetIt(2,2,true, new double[]
              {1,0, 2,0,
               2,0  4,1}]);  // 2x2, complex matrix
            B.ArcTan(A);
          }
          finally
          {
            MtxVec.FreeIt(ref A, ref B);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan: TMtxVec; overload; 
    (*<summary>Calculate the inverse tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTan(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TMtx A,B;
          MtxVec.CreateIt(out A, out B);
          try
          {
            A.SetIt(2,2,true, new double[]
              {1,0, 2,0,
               2,0  4,1}]);  // 2x2, complex matrix
            B.ArcCot(A);
          }
          finally
          {
            MtxVec.FreeIt(ref A, ref B);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot: TMtxVec; overload; 
    (*<summary>Calculate the inverse cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCot(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse secant.</summary>
      
<remarks>Calculate the inverse secant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Sec"/>
      <SeeAlso cref="ArcCsc"/>*)
    function ArcSec: TMtxVec; overload; 
    (*<summary>Calculate the inverse secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(Index, Len: integer): TMtxVec; overload; 

    (*<summary>Calculate the inverse secant of all X object elements and store the results in the calling object. </summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse cosecant.</summary>
      
<remarks>Calculate the inverse cosecant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Csc"/>
      <SeeAlso cref="ArcSec"/>*)
    function ArcCsc: TMtxVec; overload; 
    (*<summary>Calculate the inverse cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsc(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse cosecant of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 1.5, 2, 0.3});
            a.Sinh();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sinh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 1.5, 2, 0.3});
            a.Cosh();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cosh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 1.5, 2, 0.3});
            a.Tanh();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Tanh: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tanh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 1.5, 2, 0.3});
            a.Coth();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cotangens for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Coth(const X: TMtxVec): TMtxVec; overload; 
   (*<summary>Calculate the hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic secant.</summary>
      
<remarks>Calculate the hyperbolic secant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcSech"/>
      <SeeAlso cref="Csch"/>*)
    function Sech: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic secant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Hyperbolic cosecant.</summary>
      
<remarks>Calculate the hyperbolic cosecant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcCsch"/>
      <SeeAlso cref="Sech"/>*)
    function Csch: TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosecant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 


    (*<summary>Absolute values.</summary>
      
<remarks>Calculate the absolute value of all calling object elemets in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Abs(); // a = [1,2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mag"/>*)
    function Abs: TMtxVec; overload; 
    (*<summary>Calculate the absolute value for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the absolute value for all X object and store the results in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Abs(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the absolute value of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic sine.</summary>
      
<remarks>Calculate the inverse hyperbolic sine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sinh"/>*)
    function ArcSinh: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic sine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic sine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic cosine.</summary>
      
<remarks>Calculate the inverse hyperbolic cosine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Cosh"/>*)
    function ArcCosh: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic tangens.</summary>
      
<remarks>Calculate the inverse hyperbolic tangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Tanh"/>*)
    function ArcTanh: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
        
<remarks>The results are storedi n the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
        properties of the calling object must be set explicitly.
        An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic cotangens.</summary>
      
<remarks>Calculate the inverse hyperbolic cotangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Coth"/>*)
    function ArcCoth: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cotangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverser hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic secant.</summary>
      
<remarks>Calculate the inverse hyperbolic secant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sech"/>*)
    function ArcSech: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic secant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse hyperbolic cosecant.</summary>
      
<remarks>Calculate the inverse hyperbolic cosecant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Csch"/>*)
    function ArcCsch: TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosecant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,8});
            a.Cbrt(); // a = [1,2]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt: TMtxVec; overload; 
    (*<summary>Calculate the cube root of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the cube root of all X object elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cbrt(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the cube root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds all calling object elements towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil: TMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards positive infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(Index,Len: integer): TMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function Ceil(const Src: TMtxVec): TMtxVec;  overload;
    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object elements [Index]..[Index+Len-1]
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(const Src: TMtxVec; SrcIndex, Index,Len: integer): TMtxVec;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,8});
            a.Ln();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)
    function Ln: TMtxVec; overload; 
    (*<summary>Calculate the natural algorithm for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties
      of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Ln(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the natural logarithm for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Ln(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the natural logarithm for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Ln(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {10,100,1000,10000});
            a.Log10(); // a = [1,2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp10"/>*)
    function Log10: TMtxVec; overload; 
    (*<summary>Calculate the log base 10 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log10(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the log base 10 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Log10(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the log base 10 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Log10(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,4,8});
            a.Log2(); // a = [0,1,2,3]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp2"/>*)
    function Log2: TMtxVec; overload; 
    (*<summary>Calculate the log base 2 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log2(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the log base 2 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Log2(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the log base 2 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Log2(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent (e^) for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,System.Math.E});
            a.Exp(); // a = [0.0,1.00000]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Ln"/>*)
    function Exp: TMtxVec; overload; 
    (*<summary>Calculate the exponent (e^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the exponent (e^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Exp(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the exponent (e^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Exp(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Exponent base 2 (2^).</summary>
      
<remarks>Calculate the exponent base 2 (2^) for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
           a.SetIt(false, new double[] {1,2,3,4});
            a.Exp2(); // a = [1,4,9,16]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log2"/>*)
    function Exp2: TMtxVec; overload; 
    (*<summary>Calculate the exponent base 2 (2^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp2(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the exponent base 2 (2^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Exp2(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the exponent base 2 (2^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Exp2(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Exponent base 10 (10^).</summary>
      
<remarks>Calculate the exponent base 10 (10^) for all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            a.Exp10(); // a = [10,100,1000,10000]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log10"/>*)
    function Exp10: TMtxVec; overload; 
    (*<summary>Calculate the exponent base 10 (10^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp10(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the exponent base 10 (10^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Exp10(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the exponent base 10 (10^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function Exp10(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Gets the imaginary part of a complex object.</summary>
      
<remarks>Gets the imaginary part of a complex object Vec and stores the real results in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> must be true otherwise the exception will be raised.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});  // a= [1+2i, 3+4i]
            b.ImagPart(a);  // b = [2, 4]
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Gets the imaginary part of complex object Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object. An exception is raised if the calling object is complex, if Vec is not complex or
      if array borders are overrun/underrun.
</remarks>
*)
    function ImagPart(const Vec: TMtxVec; VecIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power ^(Exponent) for all caling object elements using the integer parameter Exponent.
      For non integer exponents, the <see cref="Power"/> and <see cref="PowerVec"/> methods can be used.
</remarks>


      <Example>
      <code>
      var a: TVec;
      begin
        CreateIt(a);
        try
          a.SetIt(False,[1,2,3,4]);
          a.IntPower(3);
        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>
      <SeeAlso cref="PowerVec"/>*)
    function IntPower(Exponent: Integer): TMtxVec; overload;
    (*<summary>Calculate the power Base^(Exponent) for all Base object elements.</summary>
      
<remarks>Calclation uses the integer Exponent value and stores the results in calling object. Size and
      <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function IntPower(const Base: TMtxVec; Exponent: Integer): TMtxVec; overload;

    (*<summary>Inverse elements.</summary>
      
<remarks>Calculates the inverse of all calling object elements in-place without limiting inverse operation.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1, 2, 3, 4});
            b.Inv(a); // b = [1.0, 0.5, 0.3333, 0.25]
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TDenseMtxVec.Divide"/>*)
    function Inv: TMtxVec; overload; 
    (*<summary>Calculate the inverse of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Inv(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse of all calling object elements in-place. The computation occurs after first limiting the
      magnitude of each element by the lower bound of Threshold.</summary>
      
<remarks>The limiting operation is performed to avoid division by zero. Since Threshold represents a magnitude, it is always real and must
      always be positive.

      For complex versions, the magnitude of the input is limited, but the phase remains unchanged.
      Zero-valued input is assumed to have zero phase.
</remarks>
*)
    function Inv(const Threshold: double): TMtxVec; overload; 
    (*<summary>Calculate the inverse of calling object elements [Index]..[Index+Len-1] in-place after limiting
      the magnitude of each element by the lower bound of Threshold.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Inv(const Threshold: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse of all X object elements without limiting  operating.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Inv(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse of X object elements [XIndex]..[XIndex+Len-1] without limiting  operating.</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if X and calling object <see cref="Complex"/> property
      does not match or array borders are overrun/underrun.
</remarks>
*)
    function Inv(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse of all X object elements anmd store the results to calling object elements.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling obhect are adjusted automatically. The computation occurs after first limiting the
      magnitude of each element by the lower bound of Threshold. The limiting operation is performed to avoid
      division by zero. Since Threshold represents a magnitude, it is always real and must always be positive.

      For complex versions, the magnitude of the input is limited, but the phase remains unchanged.
      Zero-valued input is assumed to have zero phase.
</remarks>
*)
    function Inv(const X: TMtxVec; const Threshold: double): TMtxVec; overload; 
    (*<summary>Calculate the inverse of X object elements [XIndex]..[XIndex+Len-1] after limiting
      the magnitude of each element by the lower bound of Threshold.</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if X and calling object
      <see cref="Complex"/> property do not match or array borders are overrun/underrun.
</remarks>
*)
    function Inv(const X: TMtxVec; const Threshold: double; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Converts elements from cartesian to polar coordinate form.</summary>
      
<remarks>Converts all calling object elements from cartesian to polar coordinate form, storing the magnitude (radius)
      component of corresponding elements in the AmpltVec and the phase (angle) component of corresponding elements in
      the PhaseVec. If you want to use this method then the calling matrix <see cref="Complex"/> property must be
      true. If this is not the case, an exception is raised. Size and <see cref="Complex"/> properties of AmpltVec and
      PhaseVec are set automatically.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TMtx A, Ampl, Phase;
          MtxVec.CreateIt(out A, out Ampl, out Phase);
          try
          {
            a.SetIt(2, 2, true,
               new double[] {1,0, 2,0,
                            2,0, 4,1});
            A.CartToPolar(Amplt, Phasw);
          }
          finally
          {
            MtxVec.FreeIt(ref A, ref Amplt, ref Phase);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(const AmpltVec, PhaseVec: TMtxVec); overload; 
    (*<summary>Convert calling object elements [Index] to [Index+Len-1] from cartesian to polar form.</summary>
      
<remarks>Store the results in AmpltVec (radius values) and PhaseVec(phase values). Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is True and array borders are overrun or underrun.
</remarks>
*)
    procedure CartToPolar(const AmpltVec, PhaseVec: TMtxVec; AmpltIndex, PhaseIndex, Index, Len: integer); overload; 

    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});  // a = [1,2,3,4] i.e 1+2i ; 3+4i
            b.Copy(a);                // b = [1,2,3,4]
            c.Copy(a,b);  //concatenate a and b and store in c = [1,2,3,4,1,2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Assign"/>*)
    function Copy(const Src: TMtxVec): TMtxVec; overload; 

    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});  // a = [1,2,3,4] i.e 1+2i ; 3+4i
            b.Copy(a, mvSingle);                // convert to single precision
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Assign"/>*)
    function Copy(const Src: TMtxVec; const dstFloatPrecision: TMtxFloatPrecision): TMtxVec; overload; 

    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] in the calling object
       elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size and <see cref="Complex"/> properties must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
       is True and array borders are overrun or underrun.
</remarks>
*)
    function Copy(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Copy and convert values from TVecInt.</summary>
               
<remarks>Applies appropriate conversion and copy data from TVecInt.
               The destinaton data format is determined with dstFloatPrecision.
</remarks>
*)
    function Copy(const Src: TMtxVecInt; const dstFloatPrecision: TMtxFloatPrecision):TMtxVec; overload; virtual;

    (*<summary>Copy and convert values from TVecInt at indexes [SrcIndex]...[SrcIndex+Len-1].</summary>
               
<remarks>Applies appropriate conversion and copy data from TVecInt.
               The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
               properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)

    function Copy(const Src: TMtxVecInt; SrcIndex, Index, Len: integer):TMtxVec; overload;

    (*<summary>Copy and convert values to TVecInt.</summary>
               
<remarks>Applies appropriate conversion and copy data to TVecInt.
               The value TVecInt.IntPrecision is preserved.
</remarks>
*)
    procedure CopyTo(const Dst: TMtxVecInt; const Rounding: TRounding); overload;

    (*<summary>Copy and convert values to specified dstFloatPrecision.</summary>
               
<remarks>Dst will be sized to hold all calling object data in specified dstFloatPrecision.
               The single/double and Complex/Not Complex conversion cant be handled concurrently.
</remarks>
*)
    procedure CopyTo(const Dst: TMtxVec; const dstFloatPrecision: TMtxFloatPrecision); overload;

    (*<summary>Copy and convert values to Dst.FloatPrecision at indexes [DstIndex]...[DstIndex+Len-1].</summary>
               
<remarks>Applies appropriate conversion and copy data from Src.
               The results are stored in Dst object elements [Index]...[Index+Len-1]. Size and <see cref="TMtxVec.FloatPrecision"/>
               properties of the destination object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)

    procedure CopyTo(const Dst: TMtxVec; const DstIndex, Index: integer; Len: integer); overload;

    (*<summary>Copy and if needed convert values to Dst, but keep Dst.FloatPrecision.</summary>*)
    procedure CopyTo(const Dst: TMtxVec); overload;

    (*<summary>Copy and convert values to TVecInt at indexes [DstIndex]...[DstIndex+Len-1].</summary>
               
<remarks>Applies appropriate conversion and copy data from TVecInt.
               The results are stored in to Dst object elements [Index]...[Index+Len-1]. Size and <see cref="TMtxVecInt.IntPrecision"/>
               properties of the destination object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)

    procedure CopyTo(const Dst: TMtxVecInt; const Rounding: TRounding; DstIndex, Index, Len: integer); overload;

    (*<summary>Conjugate.</summary>
      
<remarks>Conjugate all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.Conj();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
    function Conj: TMtxVec; overload; 
    (*<summary>Conjugate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function Conj(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Conjugate each of Vec elements.</summary>
      
<remarks>Store the results in the calling object. The
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match Vec vector.
</remarks>
*)
    function Conj(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1].</summary>
      
<remarks>Store them in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is true and array borders are overrun.
</remarks>
*)
    function Conj(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Copies values from an array.</summary>
      
<remarks>Copies values from a complex array. The size and <see cref="Complex"/> properties of the
      calling object are set implicitely.
</remarks>


      <SeeAlso cref="Copy"/>
      <SeeAlso cref="CopyTo"/>
      <SeeAlso cref="Round"/>
      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="CopyToArray"/>*)
    function CopyFromArray(const Src: TSCplxArray): TMtxVec; overload; 

    (*<summary>Copies values from an array.</summary>
      
<remarks>Copies values from a complex array. The size and <see cref="Complex"/> properties of the
      calling object are set implicitely.
</remarks>


      <SeeAlso cref="Copy"/>
      <SeeAlso cref="CopyTo"/>
      <SeeAlso cref="Round"/>
      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="CopyToArray"/>*)
    function CopyFromArray(const Src: TCplxArray): TMtxVec; overload; 
    (*<summary>Copies values from the Src array [SrcIndex]..[SrcIndex+Len-1] to the calling object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from the Src array [SrcIndex]..[SrcIndex+Len-1] to the calling object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSCplxArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size and <see cref="Complex"/> properties of the calling object are set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray): TMtxVec; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from a real single precision floating point Src array.</summary>
      
<remarks>The size and <see cref="Complex"/> properties of the calling object are set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray): TMtxVec; overload; 
   (*<summary>Copies values from the single precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from an integer type Src array.</summary>
      
<remarks>The size and <see cref="Complex"/> properties of the
      calling object are set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray): TMtxVec; overload; 
    (*<summary>Copies values from a 4 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to the calling object values [Index]..[Index+len-1]. The size of the calling object is not changed.
      If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 














    (*<summary>Copies values from an 2 byte signed integer type Src array.</summary>
    
<remarks>The size and <see cref="Complex"/> properties of the calling object are set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray): TMtxVec; overload; 
    (*<summary>Copies values from a 2 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array.</summary>
      
<remarks>The size and <see cref="Complex"/> properties of the calling object are set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray): TMtxVec; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray; SrcIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Copy values to an array.</summary>
      
<remarks>Copy complex values to Dst array. The size of the array is set automatically.
      If the calling object is not complex an exception will be raised.
</remarks>


      <SeeAlso cref="Copy"/>
      <SeeAlso cref="CopyTo"/>
      <SeeAlso cref="Round"/>
      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="CopyFromArray"/>*)
    function CopyToArray(var Dst: TCplxArray): TMtxVec; overload; 
    (*<summary>Copy complex values from the calling object [Index]..[Index+len-1] to the Dst
       array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray; DstIndex, Index,Len: integer): TMtxVec; overload; 

    (*<summary>Copy values to an array.</summary>
      
<remarks>Copy complex values to Dst array. The size of the array is set automatically.
      If the calling object is not complex an exception will be raised.
</remarks>


      <SeeAlso cref="Copy"/>
      <SeeAlso cref="CopyTo"/>
      <SeeAlso cref="Round"/>
      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="CopyFromArray"/>*)
    function CopyToArray(var Dst: TSCplxArray): TMtxVec; overload; 
    (*<summary>Copy complex values from the calling object [Index]..[Index+len-1] to the Dst
       array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TSCplxArray; DstIndex, Index,Len: integer): TMtxVec; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts floating point data
       to integer numbers.</summary>
       
<remarks>The rounding method used is specified with the Rounding parameter.
</remarks>
*)
    function CopyToArray(var Dst: TIntegerArray; Rounding: TRounding): TMtxVec; overload; 

    (*<summary>Copy real or complex values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. If the calling object is complex, the
       Index and Len parameters define the number of complex elements. The method converts floating point
       values to integer. Values exceeding the range of the integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TIntegerArray; Rounding: TRounding; DstIndex, Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from the calling object to the Dst array and converts floating point data
       to 2 byte signed integer numbers.</summary>
       
<remarks>The rounding method used is specified with the Rounding parameter.
</remarks>
*)
    function CopyToArray(var Dst: TSmallIntArray; Rounding: TRounding): TMtxVec; overload; 
    (*<summary>Copy real or complex values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. If the calling object is complex, the
       Index and Len parameters define the number of complex elements. The method converts floating point values
       to integers. Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TSmallIntArray; Rounding: TRounding; DstIndex, Index,Len: integer): TMtxVec; overload; 
    (*<summary>Copies values from the calling object to the Dst array and converts floating point data
       to 1 byte unsigned integer numbers.</summary>
       
<remarks>The rounding method used is specified with the Rounding parameter.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray; Rounding: TRounding): TMtxVec; overload; 

    (*<summary>Copy real or complex values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. If the calling object is complex, the Index and Len parameters
       define the number of complex elements. The method converts floating point values to integers.
       Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray; Rounding: TRounding; DstIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Split complex calling object in real and imaginary part.</summary>
      
<remarks>Split calling object into real and imaginary components. Store all real components in ReVec and
      all imaginary components in ImVec. Size and <see cref="Complex"/> properties of ReVec and ImVec
      are set implicitly to match with the calling vector. An execption is raised if calling object is not complex.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true, new double[] {1,-2,3,4});
            a.CplxToReal(b,c);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(const ReVec, ImVec: TMtxVec); overload; 
    (*<summary>Split calling object elements [Index]..[Index+Len-1] into real and imaginary components.</summary>
      
<remarks>Store real components in ReVec elements [ReIndex]..[ReIndex+Len-1] and imaginary components in ImVec elements
      [ImIndex]..[ImIndex+Len-1]. Size and <see cref="Complex"/> properties must be set explicitly.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>  is true and array borders are overrun or underrun.
</remarks>
*)
    procedure CplxToReal(const ReVec, ImVec: TMtxVec; ReIndex, ImIndex, Index, Len: integer); overload; 

    (*<summary>DegToRad converts degrees to radians.</summary>
      <returns>DegToRad converts degrees to radians for all calling object elements in-place.<para/>
        The following transformation is used: <c> Radians = PI*180*Degrees </c></returns>

      <SeeAlso cref="DegToRad"/>*)
    function DegToRad: TMtxVec; overload; 
    (*<summary>DegToRad converts the degrees for calling object elements [Index]..[Index+Len-1] in-place to radians.</summary>
      
<remarks>An exception is raised, if array borders are overrun or underrun.
</remarks>
*)
    function DegToRad(Index, Len: integer): TMtxVec; overload; 
    (*<summary>DegToRad converts the degrees to radians for all X object elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function DegToRad(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>DegToRad converts degrees to radians for all X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised, if array borders are overrun or underrun.
</remarks>
*)
    function DegToRad(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 


    (*<summary>RadToDeg converts radians to degrees.</summary>
      <returns>RadToDeg converts radians to degrees for all calling object elements in-place.<para/>
        The following transformation is used: <c> Degrees = Radians/(PI*180) </c></returns>

      <SeeAlso cref="DegToRad"/>*)
    function RadToDeg: TMtxVec; overload; 
    (*<summary>RadToDeg converts the radians for calling object elements [Index]..[Index+Len-1] in-place to degrees.</summary>
      
<remarks>An exception is raised, if array borders are overrun or underrun.
</remarks>
*)
    function RadToDeg(Index, Len: integer): TMtxVec; overload; 
    (*<summary>RadToDeg converts the radians to degrees for all X object elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function RadToDeg(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>RadToDeg converts degrees to radians for all X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised, if array borders are overrun or underrun.
</remarks>
*)
    function RadToDeg(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Extends a real object to a complex object.</summary>
      
<remarks>Extend the calling object to complex vector. After the calling of ExtendToComplex the imaginary part becomes the same as real part if Zeros
      is false. If Zeros is true the imaginary part is set to zero. The use of the in-place version of the method is discouraged because
      it requires 3*N copy operations, while the not-in-place version requires only 2*N copy operations.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1, 2, 3, 4});
            b.ExtendToComplex(a,true);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealToCplx"/>
      <SeeAlso cref="ImagPart"/>
      <SeeAlso cref="RealPart"/>*)
    function ExtendToComplex(Zeros: boolean = True): TMtxVec; overload;
    (*<summary>Extend Vec object to complex calling object.</summary>
      
<remarks>If Zeros is true then the calling vector imaginary part is set to zero, otherwise
      the calling object imaginary part is the same as calling object real part.
</remarks>
*)
    function ExtendToComplex(const Src: TMtxVec; Zeros: Boolean): TMtxVec; overload;

    (*<summary>Converts the source to complex.</summary>
      
<remarks>Converts the source to complex by setting the imaginary part to either zero (zeros = True)
      or same as real (zeros = false). Does not set size of the calling vector.
      If there is not sufficient space available to store the result an exception
      will be raised.
</remarks>
*)
    function ExtendToComplex(const Src: TMtxVec; Zeros: Boolean; SrcIndex,DstIndex, Len: integer): TMtxVec; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.
     For arguments larger than 10000, the computation of sine/cosine is speeded up by about 10x
     for SSE2/SSE3. The performance gains for classic FPU math are also significant.
     The cost of this function call is approx 30% of the cost of the sine function.

     It is important to mention that the sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is never used
     implicitely within TMtxVec methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle: TMtxVec; overload;

    (*<summary>FixAngle for calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is True or
      if array borders are overrun/underrun.
</remarks>
*)
    function FixAngle(Index,Len: integer): TMtxVec;  overload;

    (*<summary>Sets angle in <c>[-2PI,2PI]</c> for all Src elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the Src object.
</remarks>
*)
    function FixAngle(const Src: TMtxVec): TMtxVec;  overload;

    (*<summary>Sets angle in [-2PI,2PI] for Src elements [SrcIndex]..[SrcIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly. An exception is raised
      if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function FixAngle(const Src: TMtxVec; SrcIndex, Index,Len: integer): TMtxVec;  overload;

    (*<summary>Rounds towards negative infinity.</summary>
      
<remarks>Rounds all calling object elements towards negative infinity in-place.

      <c>Floor(-2.8) = -3</c><para/>
      <c>Floor(2.8) = 2</c><para/>
      <c>Floor(-1.0) = -1</c><para/>
</remarks>
*)
    function Floor: TMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards negative infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(Index,Len: integer): TMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards negative infinity and stores the result
      in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TMtxVec): TMtxVec;  overload;

    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards negative infinity
      and stores the result in the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(const Src: TMtxVec; SrcIndex, Index,Len: integer): TMtxVec;  overload;








































    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate the calling object complex exponential in-place. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            a.Expj();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)
    function Expj: TMtxVec; overload; 
    (*<summary>Calculate the e^(j*Omega), a complex exponential.</summary>
      
<remarks>Omega must be a real object. If omega is complex, then use the <see cref="Exp"/> method.
</remarks>
*)
    function Expj(const Omega: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the complex exponential for Omega elements [OmegaIndex]..[OmegaIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elemets [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is True and array borders are overrun or underrun.
</remarks>
*)
    function Expj(const Omega: TMtxVec; OmegaIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Finds a match for X in object values.</summary>
      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>
      
<remarks>Compare real value X with all calling object elements.

      Note
        This method also supports the NAN and INF search.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2,5,1,6});
            int indes = a.Find(1.0);  // returns 2 (the arrays are zero based)
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
    function Find(const x: double): integer; overload; 
    (*<summary>Compare complex value X with all calling object elements.</summary>
      <returns>the index of last matched element. If no matching elements are
       found, the result is -1.</returns>
       
<remarks>An exception is raised if calling object <see cref="Complex"/> property is false.
</remarks>
*)
    function Find(const x: TCplx): integer; overload; 

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,5.5,-1.6,6});
            a.Frac();  // a = [0, 0.5, -0.6, 0]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac: TMtxVec; overload; 
    (*<summary>Calculates the fractional part for all X object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Frac(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the fractional part for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Calculates the fractional part for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 


    (*<summary>Complementary error functon of values.</summary>
      
<remarks>Calculates the complementary error function value for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,5.5,-1.6,6});
            a.Erfc();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc: TMtxVec; overload; 
    (*<summary>Calculates the complementary error function value for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erfc(const Src: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the complementary error function value for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Calculates the complementary error function value for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(const Src: TMtxVec; SrcIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Error functon of values.</summary>
      
<remarks>Calculates the error function for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 5.5, -1.6, 6});
            a.Erf();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf: TMtxVec; overload; 
    (*<summary>Calculates the error function for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erf(const Src: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the error function for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Calculates the error function for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(const Src: TMtxVec; SrcIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Inverse error functon of values.</summary>
      
<remarks>Calculates the Inverse error function value for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 5.5, -1.6, 6});
            a.ErfInv();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Erf"/>
      <SeeAlso cref="Erfc"/>*)
    function ErfInv: TMtxVec; overload; 
    (*<summary>Calculates the inverse error function value for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function ErfInv(const Src: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the inverse error function value for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function ErfInv(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Calculates the inverse error function value for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array
      borders are overrun or underrun.
</remarks>
*)
    function ErfInv(const Src: TMtxVec; SrcIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Flips the real and imaginary part of complex numbers.</summary>
      <returns>Flipped real and imaginary part of complex numbers for all calling object elements in-place.<para/>
        The following transformation is used: <c>a + i*b -> b + i*a</c></returns>

      <SeeAlso cref="FlipConj"/>*)
    function Flip: TMtxVec; overload; 
    (*<summary>Flips the real and imaginary part of complex numbers for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Flips the real and imaginary part of complex numbers for all X object elements.</summary>
      
<remarks>Xtore the results in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function Flip(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Flips the real and imaginary part of complex numbers for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 


    (*<summary>Flips the real and imaginary part of complex numbers and conjugates the complex numbers.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling object complex elements in-place.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj: TMtxVec; overload; 
    (*<summary>Flip calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is false or if array borders
      are overrun/underrun.
</remarks>
*)
    function FlipConj(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Flip all X object complex elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object
      are adjusted automatically.
</remarks>
*)
    function FlipConj(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Flip X object complex elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if calling
      object <see cref="Complex"/> property is false or if array borders are overrun/underrun.
</remarks>
*)
    function FlipConj(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2))</c> of all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1, 16});
            a.InvSqrt(); // returns [1,0.25]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt: TMtxVec; overload; 
    (*<summary>Calculate the inverse of square root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function InvSqrt(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse of square root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the calling vector are set
       implicitly to match the X object.
</remarks>
*)
    function InvSqrt(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse of square root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function InvSqrt(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {-1, 8});
            a.InvCbrt(); // returns [-1,0.5]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cbrt"/>*)
    function InvCbrt: TMtxVec; overload; 
    (*<summary>Calculate the inverse of cube root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function InvCbrt(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the inverse of cube root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      the calling vector are set implicitly to match the X object.
</remarks>
*)
    function InvCbrt(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the inverse of cube root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function InvCbrt(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(x[i]/y[i]).</c><para/>
      The results will be saved to the calling vector.
      X and Y must be a real and have the same length. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(false, new double[] {0,1,10,-1,-10}); // a = [0, 1, 10,    -1, -10];
            b.SetIt(false,new double[],{0,1,System.Math.PI,-1,-System.Math.PI}); // b = [0, 1, Pi,    -1, -Pi];
            c.Rem(a,b);                     // c = [0, 0, 0.5752, 0, -0.5752]
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>*)
    function Rem(const X, Y: TMtxVec): TMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Calculation uses the following formula:

      <c>z[i] = x[xi]-y[yi]*Trunc(x[xi]/y[yi]),</c><para/>
      where i in [Index..Index+Len], xi in [XIndex..XIndex+Len], yi in [YIndex..YIndex+Len].
      The results will be saved to the calling vector. X must be a real. An exception will be raised if
      <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Rem(const X, Y: TMtxVec; XIndex, YIndex, Index, Len: integer): TMtxVec; overload;

    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector.
      Size and <see cref="Complex"/> properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Rem(const X: TMtxVec; const Y: double): TMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector. An exception will be raised if
      <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Rem(const X: TMtxVec; const Y: double; XIndex, Index, Len: integer): TMtxVec; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude for all calling object elements in-place.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,-2,3,4}); // a = [1,-2, 3,4]
            a.Mag();  // a = [1, 2, 3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Abs"/>
      <SeeAlso cref="PhaseSpectrum"/>*)
    function Mag: TMtxVec; overload; 
    (*<summary>Calculate the magnitude for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
</remarks>
*)
    function Mag(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the magnitude for all X elements.</summary>
      
<remarks>Store the results in the calling object elements. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match Vec vector.
</remarks>
*)
    function Mag(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the magnitude for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array
      borders are overrun.
</remarks>
*)
    function Mag(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiply all calling object elements with Value in-place.
      This method is the same as the <see cref="Mul"/> method overloads
      multiplying with vector elements with a scalar.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2,3,5});  // a = [2,3,5]
            a.Scale(3); // a = [6,9,15]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TDenseMtxVec.Add"/>*)
    function Scale(const Factor: double): TMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(const Factor: double; Index, Len: integer): TMtxVec; overload;

    (*<summary>Multiply all calling object elements with a complex Value in-place.</summary>*)
    function Scale(const Factor: TCplx): TMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(const Factor: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary>Scatter object elements.</summary>
      
<remarks>Scatter the elements of the Src and store them in the calling object
      according to the IndexType, Increment and Offset parameters.

      The Indexes vector is used only if IndexType is either indVector
      or indMask. If IndexType is indVector, the values from the Indexes
      object denote the index positions in the calling object to which
      the values should be copied from the Src.
      The Indexes vector must have the indexes stored in the IValues
      array. The IValues integer arrays points the same memory as Values
      array.

      The Increment and Offset parameters are used only if TIndexType
      is indIncrement. They define the target offset and a fixed
      step (increment) between calling vector elements.

      If IndexType is indMaks the Indexes object must have the same
      size as the Src object. The routine will copy only those elements
      from the Src to the calling object, for which there is a 1 at the coresponding
      index in the Indexes object.The elements in the calling object are stored consecutively.

      See the <see cref="TVec.Gather"/> method to see how to perform gathering.

      The performance of the CPU heavily depends on the assumption that elements are stored at consecutive memory locations.
      If it is neccessary to apply a set of operations only to elements at specific indexes, performance-wise it can prove to be
      very helpfull, if the elements are gathered first.
</remarks>


      <SeeAlso cref="TVec.Gather"/>*)
    function Scatter(const Src: TMtxVec; const Indexes: TVecInt = nil; IndexType: TIndexType = indVector; Increment: integer = 1; Offset: integer = 0): TMtxVec; overload; 

    (*<summary> Scatters Src elements starting at Offset and with Increment to the calling object. </summary>*)
    function ScatterByIncr(const Src: TMtxVec; Increment: integer = 1; Offset: integer = 0): TMtxVec; overload;

    (*<summary> Scatters Src elements defined with indices stored in Indexes to the calling object. </summary>*)
    function ScatterByIndexes(const Src: TMtxVec; const Indexes: TVecInt): TMtxVec; overload;

    (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>
                 
<remarks>The elements are assumed to be not scattered in the Src! This is not a masked copy operation.
                 It is a reverse of GatherByMask.
</remarks>
*)
    function ScatterByMask(const Src: TMtxVec; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVec; overload;

    (*<summary>Copies those values from MaskVec, at which aMask is different from 0.</summary>*)
    function Mask(const MaskVec: TMtxVec; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Copies those values from MaskVecNot, at which aMask is equal to 0.</summary>*)
    function MaskNot(const MaskVecNot: TMtxVec; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Assigns Value at indexes of the calling object, at which aMask is different from 0.</summary>*)
    function Mask(const Value: double; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Assigns Value at indexes of the calling object, at which aMask is equal to 0.</summary>*)
    function MaskNot(const Value: double; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Assigns Value at indexes of the calling object, at which aMask is different from 0.</summary>*)
    function Mask(const Value: TCplx; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Assigns Value at indexes of the calling object, at which aMask is equal to 0.</summary>*)
    function MaskNot(const Value: TCplx; const aMask: TMtxVecInt): TMtxVec; overload;

    (*<summary>Sets object values.</summary>
      
<remarks>Set object values. Method call does not change object's size or <see cref="Complex"/> property, but
      it does check for array overrun. The elements of A array are copied to the calling object elements, starting at Index.
      If the calling object is complex, then real parts of complex numbers are on even (0,2,4..) and imaginary parts
      on odd indexes.(1,3,5,..).
</remarks>
*)
    function SetIt(Index: integer; const A: array of double): TMtxVec; overload;
    (*<summary>The elements of A array, starting at aIndex, are copied to the calling object elements, starting at Index.</summary>
      
<remarks>If the calling object is complex, then real parts of complex numbers in the A array are on even (0,2,4..) and imaginary parts
      on odd indexes.(1,3,5,..).
</remarks>
*)
    function SetIt(Index, aIndex, Len: integer;  const A: array of double): TMtxVec; overload;

    (*<summary>Sets object complex values.</summary>
      
<remarks>The a array complex values are copied to the calling object CValues from [Index]..[Index+Length(a)-1]. An
      exception is raised if calling object array borders are overrun/underrun.

      Note
        Use this method for complex array only.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetDouble"/>
      <SeeAlso cref="SetInteger"/>
      <SeeAlso cref="SetSingle"/>*)
    function SetCplx(Index: integer; const A: array of TCplx): TMtxVec; overload;

    (*<summary>Sets object values (single).</summary>
      
<remarks>The single elements of A array are copied to the calling object elements [Index]..[Index+Length(A)-1]. An
      exception is raised if calling object array borders are overrun/underrun.

      Note
        Use this method for integer array only.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetInteger"/>
      <SeeAlso cref="SetCplx"/>
      <SeeAlso cref="SetDouble"/>*)
    function SetSingle(Index: integer; const A: array of single): TMtxVec; overload;
    function SetSingle(Index: integer; aIndex, Len: integer; const A: array of single): TMtxVec; overload;

    (*<summary>Sets object values (double).</summary>
      
<remarks>The double elements of A array are copied to the calling object elements [Index]..[Index+Length(A)-1]. An
      exception is raised if calling object array borders are overrun/underrun.

      Note
        Use this method for integer array only.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetInteger"/>
      <SeeAlso cref="SetCplx"/>
      <SeeAlso cref="SetSingle"/>*)
    function SetDouble(Index: integer; const A: array of double): TMtxVec; overload;
    function SetDouble(Index: integer; aIndex, Len: integer; const A: array of double): TMtxVec; overload;

    (*<summary>Sets object values (integer).</summary>
      
<remarks>The integer elements of A array are copied to the calling object elements [Index]..[Index+Length(A)-1]. An
      exception is raised if calling object array borders are overrun/underrun.

      Note
        Use this method for integer array only.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetDouble"/>
      <SeeAlso cref="SetCplx"/>
      <SeeAlso cref="SetSingle"/>*)
    function SetInteger(Index: integer; const A: array of Integer): TMtxVec; overload;
    function SetInteger(Index: integer; aIndex, Len: integer; const A: array of Integer): TMtxVec; overload;

    (*<summary>Square.</summary>
      
<remarks>Calculate the square of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});
            a.Sqr(); // a=[1,4,9,16]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr: TMtxVec; overload; 
    (*<summary>Calculate the square of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the square of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Sqr(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the square of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> properties of the calling object are not changed.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true,new double[] {1,4,9});
            a.Sqrt(); // a=[1,2,3]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt: TMtxVec; overload; 
    (*<summary>Calculate the square root of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(Index, Len: integer): TMtxVec; overload; 
    (*<summary>Calculate the square root of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling object are adjusted automatically.
</remarks>
*)
    function Sqrt(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the square root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Log base N.</summary>
      <returns>Log base N for all calling object elements in-place.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});
            a.LogN(10.0);  // log base 10, the slow way a = [Log10(1), Log10(2),...]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function LogN(const N: double): TMtxVec; overload; 
    (*<summary>Calculate the log base N of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(const N: double; Index, Len: integer):TMtxVec; overload; 
    (*<summary>Calculate the log base N of all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function LogN(const N: double; const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculate the log base N of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(const N: double; const X: TMtxVec; XIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiplies all calling object elements with Value in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false,new double[] {2,3,5});  // a = [2,3,5]
            a.Mul(3); // a = [6,9,15]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TDenseMtxVec.Add"/>*)
    function Mul(const Value: double): TMtxVec; overload; 
    (*<summary>Multiply all calling object elements with complex Value in-place.</summary>*)
    function Mul(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with
      complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: TCplx; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Multiply each element of Vec with Value.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TMtxVec; const Value: double): TMtxVec; overload; 
    (*<summary>Multiply each element of Vec with complex Value.</summary>
      
<remarks>Store the result in the calling object. Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TMtxVec; const Value: TCplx): TMtxVec; overload; 
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set implicitly.
</remarks>
*)
    function Mul(const Vec: TMtxVec; const Value: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with complex Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Multiply elements by imaginary unit I.</summary>*)
    function MulI: TMtxVec; overload; 
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with I in-place.</summary>*)
    function MulI(Index: integer; Len: integer): TMtxVec; overload; 
    (*<summary>Multiply X elements with I and store the result in the calling object.</summary>*)
    function MulI(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Multipy X elements [XIndex]..[XIndex+Len-1] with I.</summary>
      
<remarks>Xtore the result in the calling object at locations [Index]..[Index+Len-1].
</remarks>
*)
    function MulI(const X: TMtxVec; XIndex: integer; Index: integer; Len: integer): TMtxVec; overload; 


    (*<summary>Normalize data.</summary>
      
<remarks>Normalizes values in the calling object by subtracting a constant Offset from Elements and dividing the result by constant Factor:

      <IMG name="TVec21"/>

      The results are stored in calling object. Use this method if you want to do a multiply and add (scale and offset) operations in a single method call.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false,new double[] {1,2,3,4});  // a = [1,2,3,4]
            a.Normalize(2,3);
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TDenseMtxVec.Offset"/>
      <SeeAlso cref="Scale"/>*)
    function Normalize(const SubOffset, DivFactor: double): TMtxVec; overload; 
    (*<summary>Normalize by subtracting a complex constant SubOffset from Vec elements and dividing the result by complex constant DivFactor.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised, if object is complex.
</remarks>
*)
    function Normalize(const SubOffset, DivFactor: TCplx): TMtxVec; overload; 
    (*<summary>Normalize objects complex values [Index]..[Index+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by complex constant DivFactor.</summary>
       
<remarks>An exception is raised, if object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const SubOffset, DivFactor: TCplx; Index,Len: integer): TMtxVec; overload; 
    (*<summary>Normalize object values [Index]..[Index+Len-1] by subtracting a real constant SubOffset from Vec elements and dividing the result by
      complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector values [Index]..[Index+Len-1]. An exception
      is raised, if Vec or calling object is complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const SubOffset, DivFactor: double; Index,Len: integer): TMtxVec; overload; 
    (*<summary>Normalize object by subtracting a complex constant SubOffset from elements and dividing the result by
      real constant DivFactor.</summary>  
<remarks>An exception is raised, if object is not complex.
</remarks>
*)
    function Normalize(const SubOffset: TCplx; const DivFactor: double): TMtxVec; overload; 
    (*<summary>Normalize objects complex values [Index]..[Index+Len-1] by subtracting a complex constant SubOffset from elements and dividing the result by
      real constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is raised, if the calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const SubOffset: TCplx; const DivFactor: double; Index,Len: integer): TMtxVec; overload; 

    (*<summary>Normalize object.</summary>
      
<remarks>Normalizes Vec object by subtracting a constant Offset from Vec elements and dividing the result by constant Factor:

      <IMG name="TVec21"/>

      The results are stored in calling object. Use this method if you want to do a multiply and add (scale and offset) operations in a single method call.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false,new double[] {1,2,3,4});  // a = [1,2,3,4]
            b.Normalize(a,2,3);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TDenseMtxVec.Offset"/>
      <SeeAlso cref="Scale"/>*)
    function Normalize(const Vec: TMtxVec; const SubOffset, DivFactor: Double): TMtxVec; overload; 
    (*<summary>Normalize Vec object by subtracting a complex constant SubOffset from Vec elements and dividing the result by complex constant DivFactor.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised if Vec is complex.
</remarks>
*)
    function Normalize(const Vec: TMtxVec; const SubOffset, DivFactor: TCplx): TMtxVec; overload; 
    (*<summary>Normalize Vec object complex values [VecIndex]..[VecIndex+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TMtxVec; const SubOffset, DivFactor: TCplx; VecIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Normalize Vec object values [VecIndex]..[VecIndex+Len-1] by subtracting a real constant SubOffset from Vec elements and dividing the result by
      complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TMtxVec; const SubOffset, DivFactor: Double; VecIndex,Index,Len: integer): TMtxVec; overload; 
    (*<summary>Normalize Vec object by subtracting a complex constant SubOffset from Vec elements and dividing the result by
      real constant DivFactor.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised if Vec is complex.
</remarks>
*)
    function Normalize(const Vec: TMtxVec; const SubOffset: TCplx; const DivFactor: Double): TMtxVec; overload; 
    (*<summary>Normalize Vec object complex values [VecIndex]..[VecIndex+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by
      real constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TMtxVec; const SubOffset: TCplx; const DivFactor: Double; VecIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Converts the polar magnitude/phase pairs to cartesian pairs.</summary>
      
<remarks>Convert all AmpltVec and PhaseVec elements (combined) from polar to cartesian form. If AmpltVec and PhaseVec size is not the same
      , an exeption is raised. The results are stored as complex numbers (x=Re, y=Im) in the calling
      object.  Size and <see cref="Complex"/> properties of the calling object are set implicitly to
      match AmpltVec and PhaseVec objects.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});  // a = [1,2,3, 4] //magnitude
            b.SetIt(false, new double[] {1,0,1,-1}); // b = [1,0,1,-1] /phase
            c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(const AmpltVec, PhaseVec: TMtxVec): TMtxVec; overload; 
    (*<summary>Convert  AmpltVec elements [aIndex]..[aIndex+Len-1] and PhaseVec elements [PIndex]..[PIndex+Len-1] from polar form
      (radius,angle) to cartesian form (x,y).</summary>
      
<remarks>The results are stored as complex numbers <c>(x=Re, y=Im)</c> in the calling
      object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun/underrun.
</remarks>
*)
    function PolarToCart(const AmpltVec, PhaseVec: TMtxVec; aIndex, PIndex,Index, Len: integer): TMtxVec; overload; 

    (*<summary>Raises base object elements to any power.</summary>
      
<remarks>Raises Base calling object elements to any power. The <see cref="IntPower"/> is faster, if Exponent is an integer.
      Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
      To compute a power to the negative exponent in general case or when the base is negative,
      use the complex version of the function.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});  // a = [1,2,3, 4] //magnitude
            a.Power(1.2);
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="IntPower"/>
      <SeeAlso cref="PowerVec"/>*)
    function Power(const Exponent: double): TMtxVec; overload; 
    (*<summary>Raises all calling vector elements to complex power Exponent in-place.</summary>
       
<remarks>If the calling vector is real and has negative elements, the result will be NAN
       at those entries. To obtain a valid result in this case, extend the calling
       vector to complex with <see cref="ExtendToComplex"/> method. In all real/complex
       combinations, the Power method will not automatically assume that a number is complex
       to speed up the computation.

       Alternatively the user can of course always convert the real numbers to complex before passing
       them to the Power routine.
</remarks>
*)
    function Power(const Exponent: TCplx): TMtxVec; overload; 

    (*<summary>The phase angles (spectrum) of object elements.</summary>
      
<remarks>Calculates the phase angles (spectrum) of all Vec object elements. Phase values are returned in radians and are in the range
      -PI,PI. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match Vec object. The phase angles are calculated from the following equation:

      <IMG name="TVec23"/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(true, new double[] {1,2,3,-4});
            b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PowerSpectrum"/>
      <SeeAlso cref="ArcTan2"/>*)
    function PhaseSpectrum(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the power spectrum from the Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised
      if array borders are overrun/underrun.
</remarks>
*)
    function PhaseSpectrum(const Vec: TMtxVec; VecIndex, Index,Len: integer): TMtxVec; overload; 

    (*<summary>The power spectrum from object complex values.</summary>
      
<remarks>Calculates the power spectrum from the Vec object complex values and stores the results (power spectrum)
      in the real calling object. Size and <see cref="Complex"/> properties of the calling object are set
      implicitly to match Vec object. The spectrum elements are squares of the magnitudes of the complex input elements:

      <IMG name="Tvec22"/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(true, new double[] {1,2,3,4});  // a = [1 + 2i, 3 - 4i]
            b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PhaseSpectrum"/>*)
    function PowerSpectrum(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Calculates the power spectrum from the Vec complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object real elements [Index]..[Index+Len-1]. An exception is raised
      if calling object <see cref="Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    function PowerSpectrum(const Vec: TMtxVec; VecIndex, Index,Len: integer): TMtxVec; overload; 

    (*<summary>Raises base elements to exponent power.</summary>
      
<remarks>Raises Base value to Exponent object values powers and store the results to calling object values.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>


      <SeeAlso cref="PowerVec"/>*)
    function Power(const Base: double; const Exponent: TMtxVec): TMtxVec; overload; 
    (*<summary>Raises Base complex value to Exponent object values powers.</summary>
      
<remarks>Store the results to calling object values. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TCplx; const Exponent: TMtxVec): TMtxVec; overload; 
    (*<summary>Raises each of Base object elements to corresponding power, stored in Exponenet elements.</summary>

      
<remarks><c>Power[i] = Base[i]^Exponent[i]</c><para/>

      Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised if Base and Exponent sizes do not match.
</remarks>
*)
    function Power(const Base, Exponent: TMtxVec): TMtxVec; overload; 
    (*<summary>Raises each of the Base object elements to complex Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TMtxVec; const Exponent: TCplx): TMtxVec; overload; 
    (*<summary>Raises each of the Base object elements to real Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TMtxVec; const Exponent: double): TMtxVec; overload; 

    (*<summary>Raises Base object elements to Exponent object elements power.</summary>
      
<remarks>Raises Base elements to Exponent elements power. Only positive exponents can be handled
      if exponent object <see cref="Complex"/> property is True.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true, new double[] {1,2,3,4});
            b.SetIt(true, new double[] {3,2,2,2});
            c.PowerVec(a,b);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function PowerVec(const Base, Exponent: TMtxVec): TMtxVec;

    (*<summary>Elements product.</summary>
      <returns>the product of all calling object elements</returns>

      
<remarks><IMG name="TVec25"/>

      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,2,3,4});
            double c = a.Product_(); // c= 24
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Productc"/>*)
    function Product: double; overload; 
    (*<summary>Returns the product for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Product(Index,Len: integer): double; overload; 
    (*<summary>Calculate the product of all calling object complex elements.</summary>
      
<remarks>Store the result in complex variable X.
</remarks>
*)
    procedure Product(out X: TCplx); overload; 
    (*<summary>Calculate the product for calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>Store the result in complex variable X. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure Product(out X: TCplx; Index,Len: integer); overload; 
    (*<summary>Calculate the product of all calling object elements.</summary>
      
<remarks>Store the result in real variable X.
</remarks>
*)
    procedure Product(out X: double); overload; 
    (*<summary>Calculate the product for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Store the result in real variable X. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure Product(out X: double; Index,Len: integer); overload; 

    (*<summary>Elements product.</summary>
      <returns>the complex product of all calling object complext elements.</returns>

      
<remarks><IMG name="TVec25"/>

      An exception is raised if calling object <see cref="Complex"/> property is false.
</remarks>


      <SeeAlso cref="Product"/>*)
    function Productc: TCplx; overload; 
    (*<summary>Returns the complex product for calling object complex elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun or underrun or if <see cref="Complex"/> propety is false.
</remarks>
*)
    function Productc(Index,Len: integer): TCplx; overload; 

    (*<summary>The pseudo random sample generator with Gaussian distribution.</summary>
      
<remarks>Fills the calling object values with pseudo random samples following the Gaussian distribution with
      parameters: Mean = 0, StdDev = 1 ("Standard disctribution"). The value for the seed is obtained
      from the CPU clock.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.Size(500,false);
            a.RandGauss();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RandUniform"/>*)
    function RandGauss: TMtxVec; overload; 
    (*<summary>Fills the calling object values with pseudo random samples following the Gaussian distribution.</summary>
       
<remarks>The algorithm uses parameter AMean and AStdDev. The value for the seed is obtained from the CPU clock.
</remarks>
*)
    function RandGauss(AMean, AStdDev: double): TMtxVec; overload; 
    (*<summary>Fills the calling object values with pseudo random samples following the Gaussian distribution.</summary>
      
<remarks>The algorithm uses parameters AMean, AStdDev and Seed.
</remarks>
*)
    function RandGauss(Seed: cardinal; AMean, AStdDev: double): TMtxVec; overload; 

      (*<summary>Randomly shuffles the content of the vector.</summary>
        
<remarks>Scrambles the contents of the vector randomly by using the
        Fisher Yates shuffle.
</remarks>
*)
    function RandomShuffle: TMtxVec;

    (*<summary>The pseudo random sample generator with continuous uniform distribution.</summary>
      
<remarks>Fills the calling object values with pseudo random samples following the continuous uniform distribution
      using the parameters Low = 0, High = 1. The value for the seed is obtained from the CPU clock.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.Size(100,false);
            a.RandUniform();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RandGauss"/>*)
    function RandUniform: TMtxVec; overload; 
    (*<summary>Fills the calling object values with pseudo random samples following the continuous uniform distribution.</summary>
      
<remarks>The algorithm used parameters Low and High. The value for the seed is obtained from the CPU clock.
</remarks>
*)
    function RandUniform(aLow, aHigh: double): TMtxVec; overload; 
    (*<summary>Fills the calling object values with pseudo random samples following the continuous uniform distribution.</summary>
      
<remarks>The algorithm uses parameters Low and High and Seed.
</remarks>
*)
    function RandUniform(Seed: cardinal; aLow, aHigh: double): TMtxVec;   overload; 

    (*<summary>Gets real part of complex object values.</summary>
      
<remarks>The method method gets the real part of a complex object Vec and stores the real results in the calling
      object. Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: TVec;
      begin
        CreateIt(a,b);
        try
          a.SetIt(True,[1,2,3,4]); // = [1+2i, 3+4i]
          b.RealPart(a); // b = [1,3]
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Gets the real part of a Vec object complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the results in calling object real elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun or if Vec object <see cref="Complex"/>
      propety is false.
</remarks>
*)
    function RealPart(const Vec: TMtxVec; VecIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Read values content from stream to object.</summary>
      
<remarks>Reads values content from SrcStream stream to calling objct. No other values describing the data type or length are read
      from the DstStream. Number type is defined by the Precision parameter, which can be obtained from <see cref="ReadHeader"/> method
      call. The function returns the number of bytes read.

      Note
        Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TVec;
          AStream: TFileStream;
          Precision: TPrecision;
      begin
        CreateIt(b);
        AStream := TFileStream.Create('C:\test.bin',fmOpenRead);
        try
          Precision := b.ReadHeader(AStream); // Read info for b
          b.ReadValues(AStream,Precision); // Read values of b
        finally
          AStream.Free;
          FreeIt(b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ReadHeader"/>
      <SeeAlso cref="WriteValues"/>
      <SeeAlso cref="TMtxVecBase.LoadFromStream"/>*)
    
    function ReadValues(const Src: TStream; Precision: TPrecision; Endian: TEndianness = MtxSystemEndianness): Int64; overload; virtual;
    
    

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="Complex"/> property is True.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true, new double[] {1,2, 3,4}); // 1+2i ; 3+4i
            b.SetIt(true, new double[] {2,2,3,4}); // 2+2i ; 3+4i
            c.RealToCplx(a,b);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(const ReVec, ImVec: TMtxVec): TMtxVec; overload; 
    (*<summary>Construct a complex object from the ReVec elements [ReIndex]..[ReIndex+Len-1] (real part) and the ImVec
      elements [ImIndex]..[ImIndex+Len-1] (imaginary part).</summary>
      
<remarks>The results are stored to calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders
      are overrun. An exception is also raised if ReVec or ImVec <see cref="Complex"/> property is True.
</remarks>
*)
    function RealToCplx(const ReVec, ImVec: TMtxVec; ReIndex, ImIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Resets object properties to default values.</summary>
      
<remarks>Resets object properties to default values. The method is used by the Object cache management
      to reset the properties of the object freed with a call to <see cref="MtxVec.FreeIt"/>.
</remarks>
*)
    procedure Reset; override;

    (*<summary>Search and replace a value.</summary>
      
<remarks>Replaces the SearchValue with a ReplaceValue, where the searchValue is compared with Tol to the values of the calling object.
</remarks>
*)
    function Replace(const SearchValue, ReplaceValue: double; const Tol: double = 0.0001): TMtxVec; overload; 
    function Replace(const SearchValue, ReplaceValue: single; const Tol: single = 0.0001): TMtxVec; overload; 
    (*<summary>Replaces the SearchValue with a ReplaceValue, where the searchValue is compared with Tol to the calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Replace(const SearchValue, ReplaceValue: double; Index, Len: integer; const Tol: double = 0.0001): TMtxVec; overload; 
    function Replace(const SearchValue, ReplaceValue: single; Index, Len: integer; const Tol: single = 0.0001): TMtxVec; overload; 
    (*<summary>Search and replace a complex value.</summary>*)
    function Replace(const SearchValue, ReplaceValue: TCplx; const Tol: double = 0.0001): TMtxVec; overload; 
    function Replace(const SearchValue, ReplaceValue: TSCplx; const Tol: single = 0.0001): TMtxVec; overload; 
    (*<summary>Replaces the SearchValue with a ReplaceValue, where the searchValue is compared with Tol to the calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Replace(const SearchValue, ReplaceValue: TCplx; Index, Len: integer; const Tol: double = 0.0001): TMtxVec; overload; 
    function Replace(const SearchValue, ReplaceValue: TSCplx; Index, Len: integer; const Tol: single = 0.0001): TMtxVec; overload; 

    (*<summary>Root mean square (RMS).</summary>
      
<remarks>Calculate the root mean square value for all calling object
      elements in-place. The root mean square (RMS) is defined by the equation:

      <IMG name="TVec02"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true, new double[] {1,2, 3,4}); // 1+2i ; 3+4i
            double c = a.RMS();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="StdDev"/>*)
    function RMS: double; overload; 
    (*<summary>Calculate the RMS for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function RMS(Index, Len: integer): double; overload; 

    (*<summary>Elements rounded to the nearest whole number.</summary>
      
<remarks>Rounds all calling object elements to the nearest whole number.
      The result can be stored to an array of integers or as floating
      point number.
</remarks>


      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Frac"/>*)
    function Round: TMtxVec; overload; 
    (*<summary>Round calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Round all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/> property of
      calling object are adjusted automatically.
</remarks>
*)
    function Round(const Src: TMtxVec): TMtxVec; overload; 
    (*<summary>Rounds the calling object elements to 4 byte integers.</summary>
      
<remarks>Stores the result in the Dst array. The size of the Dst array is set automatically.
       If the complex property is True then the length of the array will be equal to
       Self.Length*2.
</remarks>
*)
    function Round(var Dst: TIntegerArray): TMtxVec; overload; 
    (*<summary>Round Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> property of the calling object must be set explicitly.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(const Src: TMtxVec; SrcIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Initialize elements to Value.</summary>
      
<remarks>Set all calling object elements to Value. If the calling object is complex
      then the real part is set to Value and the imaginary is set to zero.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.Size(false, 4);
            a.SetVal(1);
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: double): TMtxVec; overload; 

    (*<summary>Set all calling object elements [Index]..[Index+Len-1] to real Value.</summary>
      
<remarks>If the calling object is complex then the real part is set to Value and the imaginary is set to zero.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: double; Index, Len: integer): TMtxVec; overload; 

    (*<summary>Set all calling object complex elements to complex Value.</summary>*)
    function SetVal(const Value: TCplx): TMtxVec; overload; 

    (*<summary>Set calling object complex elements [Index]..[Index+Len-1] to complex Value.</summary>
       
<remarks><see cref="Complex"/> property of the calling object are set to true even before the call it was false.
       An exception is raised if calling object array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: TCplx; Index: integer; Len: integer): TMtxVec; overload; 

    (*<summary>Initializes object elements to zero.</summary>
      <SeeAlso cref="SetVal"/>*)
    function SetZero: TMtxVec; overload; 
    (*<summary>Initializes calling object elements [Index]..[Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function SetZero(Index, Len: integer): TMtxVec; overload; 

    (*<summary>Changes elements sign.</summary>
      
<remarks>Changes all calling object elements sign <c>(v -> -v)</c> in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {-1,2,-3,4);
            a.Sign(); // a = 1,-2, 3, -4
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mul"/>*)
    function Sign: TMtxVec; overload; 
    (*<summary>Change calling object elements [Index]..[Index+Len-1] sign in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sign(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Change all X object elements sign.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Sign(const X: TMtxVec): TMtxVec; overload; 
    (*<summary>Change X object elements [XIndex]..[XIndex+Len-1] sign.</summary>
      
<remarks>Store the results in callingobject elements [Index]..[Index+Len-1]. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sign(const X: TMtxVec; XIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary> Computes signum function of calling object elements. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)

    function Sgn: TMtxVec; overload; 

    (*<summary> Computes signum function of calling object elements [Index..Index+Len-1]. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(Index, Len: integer): TMtxVec; overload; 

    (*<summary> Computes signum function from Src elements and stores the result in the calling object.  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TMtxVec): TMtxVec; overload; 

    (*<summary> Computes signum function from Src elements [SrcIndex..SrcIndex+Len-1] and stores the result in
                 the calling object [Index..Index+Len-1].  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TMtxVec; SrcIndex,Index,Len: integer): TMtxVec; overload; 

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of all Src object elements and multiplies
      it with the calling object elements accordingly.
      Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      The length of Src and of the calling object must match
      or an exception will be raised. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TMtxVec): TMtxVec; overload; 

    (*<summary>Size the object.</summary>
      
<remarks>Assignes the size of the Src object to the calling object.
      If the calling object is a TVec object then the Src can be of any type,
      otherwise TMtx can only obtain size from a TMtx object and TSparseMtx can only obtain size from a TSparseMtx object.

      If the calling object and Src are of different types, the complex property can be
      different, if both objects have a matching <see cref="TMtxVecBase.Length" text="Length"/> property.
      In this case only the Complex property of the calling object will changed,
      while all other properties describing the size of the object (rows, cols, length,
      nonZeros) will be preserved. This is different from simply setting the Complex property.
      Changing the Complex property directly would also change the Length, Cols and NonZeros properties.
</remarks>
*)
    function Size(const Src: TMtxVecBase): TMtxVec ; overload; 

    (*<summary>Allows the complex property of the calling object to become of
       AComplex value instead of Src.Complex value.</summary>
       
<remarks>It is also possible to pass the calling object as the Src with a different AComplex value.
       The value of the complex property can be changed without knowing the actual type of the object.
</remarks>
*)
    function Size(const Src: TMtxVecBase; AComplex: boolean): TMtxVec ; overload; virtual;

    (*<summary>Allows the floatPrecision and complex properties of the calling object to become of
       aFloatPrecision value instead of Src.FloatPrecision and Src.Complex value.</summary>
       
<remarks>It is also possible to pass the calling object as the Src with a different aFloatPrecision value.
       The value of the FloatPrecision property can be changed without knowing the actual type of the object (matrix of vector).
</remarks>
*)
    function Size(const Src: TMtxVecBase; const aFloatPrecision: TMtxFloatPrecision): TMtxVec; overload; virtual;

    (*<summary>Sizes the array.</summary>
      
<remarks>Sizes the Dst array so that it can hold all the values stored in the calling
      object. If the <see cref="Complex"/> property is false an exception will
      be raised.
</remarks>
*)
    procedure SizeToArray(var Dst: TCplxArray); overload; 

    (*<summary>Sizes the array.</summary>
      
<remarks>Sizes the Dst array so that it can hold all the values stored in the calling
      object. If the <see cref="Complex"/> property is false an exception will
      be raised.
</remarks>
*)
    procedure SizeToArray(var Dst: TSCplxArray); overload; 

    (*<summary>Compares two objects.</summary>
      <returns>True, if they are equal.</returns>
      
<remarks>Compares two objects and returns True, if they are equal. The IsEqual
      uses cmpAbsolute comparison method with comparison tolerance
      of <c>10*EPS</c> by default.

      The method compares only <see cref="Complex"/> and <see cref="TMtxVecBase.Length" text="Length"/> properties and
      coresponding values. To compare matrices or sparse matrices, use the <see cref="Equal"/> method.
</remarks>


      <SeeAlso cref="TMtx.Equal"/>
      <SeeAlso cref="TSparseMtx.Equal"/>
      <SeeAlso cref="TVec.Equal"/>*)
    function IsEqual(const Vec: TMtxVec): boolean; overload; 
    (*<summary>Compares Vec elements [VecIndex]..[VecIndex+Len-1] with calling object
       elements [Index]..[Index+Len-1].</summary>*)
    function IsEqual(const Vec: TMtxVec; VecIndex, Index, Len: integer): boolean; overload; 
    (*<summary>Compares Vec elements with coresponding calling object elements.</summary>
      
<remarks>Method uses comparison algorithm specified by <see cref="Precision"/> property.
</remarks>
*)
    function IsEqual(const Vec: TMtxVec; const Precision: double; const Compare: TCompare = cmpAbsolute): boolean; overload; 
    (*<summary>Compares Vec elements [VecIndex]..[VecIndex+Len-1] with calling object
       elements [Index]..[Index+Len-1].</summary>
       
<remarks>Algorithm uses the <paramref name="Compare"/> type and specified <paramref name="Precision"/>.
</remarks>
*)
    function IsEqual(const Vec: TMtxVec; VecIndex, Index, Len: integer; const Precision: double; const Compare: TCompare = cmpAbsolute): boolean; overload;

    (*<summary>Compares complex Value with all calling object elements.</summary>*)
    function IsEqual(const Value: TCplx): boolean; overload;
    (*<summary>Compares real Value with all calling object elements.</summary>*)
    function IsEqual(const Value: double): boolean; overload;
    (*<summary>Compares real Value with all calling object elements.</summary>
      
<remarks>Algorithm uses the Compare method and specified Tolerance.
</remarks>
*)
    function IsEqual(const Value: double; const Tolerance: double; const Compare: TCompare): boolean; overload;
   (*<summary>Compares complex Value with all calling object elements.</summary>
      
<remarks>Algorithm uses the Compare method and specified Tolerance.
</remarks>
*)
    function IsEqual(const Value: TCplx; const Tolerance: double; const Compare: TCompare): boolean; overload;

    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="Complex"/> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {-1,2,-3,4);
            double amean = a.Mean();
            double c = a.StdDev(amean);
            // alternative ... c = a.StdDev();
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>


      <SeeAlso cref="Mean"/>*)
    function StdDev: double; overload; 
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true or if array
      borders are overrun/underrun.
</remarks>
*)
    function StdDev(Index,Len: integer): double; overload; 
    (*<summary>Returns the standard deviation of all calling object complex elements.</summary>
       
<remarks>The average of all calling object elements must be passed as a AMean parameter.
       An exception is raised if calling object <see cref="Complex"/> property is false.
</remarks>
*)
    function StdDev(const AMean: TCplx): TCplx; overload; 
    (*<summary>Returns the standard deviation of calling object complex elements [Index]..[Index+Len-1].</summary>
       
<remarks>The average of all calling object elements must be passed as a AMean parameter.
       An exception is raised if calling object <see cref="Complex"/> property is false
       or if array borders are overrun/underrun.
</remarks>
*)
    function StdDev(const AMean: TCplx; Index, Len: integer): TCplx; overload; 
    (*<summary>Returns the standard deviation of all calling object elements.</summary>
       
<remarks>The average of all calling object elements must be passed as a AMean parameter.
       An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    function StdDev(const AMean: double): double; overload; 
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The average of the coresponding elements must be passed as a parameter.
      An exception is raised if calling object <see cref="Complex"/> property is true
      or if array borders are overrun/underrun.
</remarks>
*)
    function StdDev(const AMean: double; Index, Len: integer): double; overload; 

    (*<summary>Returns the standard deviation of all calling object elements.</summary>
       
<remarks>The average of all calling object elements must be passed as a AMean parameter.
       An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure MeanAndStdDev(var aMean, aStdDev: double); overload; 
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The average of the coresponding elements must be passed as a parameter.
      An exception is raised if calling object <see cref="Complex"/> property is true
      or if array borders are overrun/underrun.
</remarks>
*)
    procedure MeanAndStdDev(var aMean, aStdDev: double; Index, Len: integer); overload; 

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine for all calling object elements and stores the sines
      to SinX and cosines to CosX. Size and <see cref="Complex"/> property of SinX and CosX are
      adjusted automatically.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,s,v;
          MtxVec.CreateIt(out a, out s, out c);
          try
          {
            a.SetIt(false, new double[] {0,Math387.PiDiv2,Math387.PI);
            a.SinCos(s,c); // s=[0,1,0], c =[1,0,-1]
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(const SinX, CosX: TMtxVec); overload; 
    (*<summary>Calculates the sine and cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>stores the sines to SinX elemets [SinXIndex]..[SinXIndex+Len-1] and cosines to CosX elements [CosXIndex]..[CosXIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinX and CosX objects are not set automatically.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> and array borders are overrun/underun.
</remarks>
*)
    procedure SinCos(const SinX, CosX: TMtxVec; SinXIndex, CosXIndex, Index, Len: integer); overload; 

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for all calling object elements and stores
      the sines to SinhX and cosines to CoshX. Size and <see cref="Complex"/> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,s,v;
          MtxVec.CreateIt(out a, out s, out c);
          try
          {
            a.SetIt(false, new double[] {0,Math387.PiDiv2,Math387.PI);
            a.SinhCosh(s,c);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(const SinhX, CoshX: TMtxVec); overload; 
    (*<summary>Calculates the hyperbolic sine and hyperbolic cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the sines to SinhX elemets [SinhIndex]..[SinhIndex+Len-1] and cosines to CoshX elements [CoshIndex]..[CoshIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinhX and CoshX objects are not set automatically.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> and array borders are overrun/underun.
</remarks>
*)
    procedure SinhCosh(const SinhX, CoshX: TMtxVec; SinhIndex, CoshIndex, Index, Len: integer); overload; 

    (*<summary>Sums vector values.</summary>
      <returns>the sum of all calling object elements. An exception is raised if calling object
      <see cref="Complex"/> property is true.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,3,-2});
            doouble s = a.Sum();  // s = 2
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>


      <SeeAlso cref="Sumc"/>*)
    function Sum: double; overload; 
    (*<summary>Returns the sum of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is True or array
      borders are overrun/underrun.
</remarks>
*)
    function Sum(Index,Len: integer): double; overload; 
    (*<summary>Calculates the sum of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the result in real ASum variable. An exception is raised if calling object <see cref="Complex"/> property is True or array borders are overrun/underrun.
</remarks>
*)
    procedure Sum(out ASum: double; Index, Len: integer); overload; 
    (*<summary>Calculates the sum of all calling object complex elements.</summary>
      
<remarks>Stores the result in complex ASum variable. An exception is raised if calling
      object <see cref="Complex"/> property is False.
</remarks>
*)
    procedure Sum(out ASum: TCplx); overload; 
    (*<summary>Calculates the sum of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the result in complex ASum variable.
      An exception is raised if calling object <see cref="Complex"/> property is False or array
      borders are overrun/underrun.
</remarks>
*)
    procedure Sum(out ASum: TCplx; Index, Len: integer); overload; 

    (*<summary>Sum (complex value).</summary>
      <returns>the complex sum of all calling object complex elements.</returns>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is False.
</remarks>


      <SeeAlso cref="Sum"/>*)
    function Sumc: TCplx; overload; 
    (*<summary>Returns the complex sum of calling object complex elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if calling object <see cref="Complex"/> property is False or array borders are overrun/underrun.
</remarks>
*)
    function Sumc(Index,Len: integer): TCplx; overload; 

    (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is a lower bound for threshold operation.
      All values smaller than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, 0.1, 3, 4});
            a.ThreshBottom(0.2); // a = [2,0.2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshBottom(const Value: double): TMtxVec; overload; 
    (*<summary>Perform the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TMtxVec; const Value: double): TMtxVec; overload; 
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Vec: TMtxVec; const Value: double; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, 0.1, 3, 4});
            a.ThreshTop(0.2); // a = [0.2, 0.1, 0.2, 0.2]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshTop(const Value: double): TMtxVec; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TMtxVec; const Value: double): TMtxVec; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Vec: TMtxVec; const Value: double; VecIndex, Index, Len: integer): TMtxVec; overload; 


      (*<summary>Threshold top and bottom centered around zero.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values smaller than Value will be replaced with Value and all values bigger than -Value will be replaced with -Value.
      The comparison checks if abs(Values[i]) &lt; Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2,-0.1,3,4});
            a.ThreshAbsLT(0.2); // a = [2,-0.2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshAbsLT(const Value: Double): TMtxVec; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsLT(const Value: Double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsLT(const Src: TMtxVec; const Value: Double): TMtxVec; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsLT(const Vec: TMtxVec; const Value: Double; VecIndex, Index, Len: integer): TMtxVec; overload; 


    (*<summary>Threshold top and bottom centered around zero.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value and all values smaller than -Value will be replaced with -Value.
      The comparison checks if abs(Values[i]) &gt; Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, -1, 3, 4});
            a.ThreshAbsGT(0.2); // a = [0.2, -0.2, 0.2, 0.2]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshAbsGT(const Value: double): TMtxVec; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsGT(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsGT(const Src: TMtxVec; const Value: double): TMtxVec; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsGT(const Vec: TMtxVec; const Value: double; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is a <b>lower</b> bound for
      threshold operation.
      All values less than LTLevel will be replaced with LTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, 0.1, 3, 4});
            a.ThresholdLT(2.3,1.5); // a = [1.5,1.5,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(const LTLevel, LTValue: double): TMtxVec; overload; 
    (*<summary>Perfrom "less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdLT(const LTLevel, LTValue: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform "less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function ThresholdLT(const Vec: TMtxVec; const LTLevel, LTValue: double): TMtxVec; overload; 
    (*<summary>Perform "less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdLT(const Vec: TMtxVec; const LTLevel, LTValue: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perfrom "less than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const LTLevel: double; const LTValue: TCplx): TMtxVec; overload; 
    (*<summary>Perfrom "less than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const LTLevel: double; const LTValue: TCplx; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform "less than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TMtxVec; const LTLevel: double; const LTValue: TCplx): TMtxVec; overload; 
    (*<summary>Perform "less than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TMtxVec; const LTLevel: double; const LTValue: TCplx; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on all calling object values. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than LTLevel will be replaced with GTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, 0.1, 3, 4});
            a.ThresholdGT(2.3,3.4); // a = [2,0.1,3.4,3.4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(const GTLevel, GTValue: double): TMtxVec; overload; 
    (*<summary>Perfrom "greater than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT(const GTLevel, GTValue: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform "greater than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThresholdGT(const Vec: TMtxVec; const GTLevel, GTValue: double): TMtxVec; overload; 
    (*<summary>Perform "greater than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdGT(const Vec: TMtxVec; const GTLevel, GTValue: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perfrom "greater than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const GTLevel: double; const GTValue: TCplx): TMtxVec; overload; 
    (*<summary>Perfrom "greater than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const GTLevel: double; const GTValue: TCplx; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform "greater than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TMtxVec; const GTLevel: double; const GTValue: TCplx): TMtxVec; overload; 
    (*<summary>Perform "greater than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TMtxVec; const GTLevel: double; const GTValue: TCplx; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values less than LTLevel will be replaced with LTValue. All values bigger than GTLevel will be replaced with GTValue.
      Operation is available only for not Complex values.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {2, 0.1, 3, 4});
            a.ThresholdGT_LT(2.3,3.4,1,0.5); // a = [2,0.5,3.4,3.4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: double): TMtxVec; overload; 
    (*<summary>Perfrom "greater than and less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      An exception will be raised if the calling object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      An exception will be raised if Vec object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const Vec: TMtxVec; const GTLevel, GTValue, LTLevel, LTValue: double): TMtxVec; overload; 
    (*<summary>Perform "greater than and less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
      An exception will be raised if Vec object or the calling object contain complex numbers.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TMtxVec; const GTLevel, GTValue, LTLevel, LTValue: double; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the TruncDst object as floating point numbers. The fractional
      part is stored in the FracDst.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)
    procedure TruncAndFrac(const TruncDst: TMtxVec; const FracDst: TDenseMtxVec); overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] and store the results to TruncDst object elements
      [TruncIdx]..[TruncIdx+Len-1].</summary>
      
<remarks>The fractional parts are saved in FracDst elements [FracIdx]..[FracIdx+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src object. An exception is raised if
      array borders are overrun/underrun.
</remarks>
*)
    procedure TruncAndFrac(const TruncDst: TMtxVec; const FracDst: TDenseMtxVec; TruncIdx, FracIdx, Index, Len: integer); overload;

    (*<summary>Rounds a real number towards zero.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the calling object again as floating point numbers.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)
    function Trunc: TMtxVec; overload; 
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(Index,Len: integer): TMtxVec; overload; 
    (*<summary>Truncate all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/>
      property of calling object are adjusted automatically.
</remarks>
*)
    function Trunc(const Src: TMtxVec): TMtxVec; overload; 
    (*<summary>Truncate all calling object elements.</summary>
      
<remarks>Store the result in the Dst integer array. Length of the array is automatically adjusted.
      If the calling object is Complex, the length of the Dst array is equal to
      2*<see cref="TMtxVecBase.Length" text="Length"/>.
</remarks>
*)
    function Trunc(var Dst: TIntegerArray): TMtxVec; overload; 
    (*<summary>Truncate Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elemenents [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src
      object. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(const Src: TMtxVec; SrcIndex, Index,Len: integer): TMtxVec; overload; 

     
    constructor Create; override;
    destructor Destroy; override;
     


    
    (*<summary>Writes object Values content to a stream.</summary>
      
<remarks>Writes the calling object Values content to the DstStream stream. No other values describing the data type or length are written
      to the DstStream. Number type is defined by the Precision parameter. Rounding defines the rounding for integer types. When
      saving double precision as single precision, all overflows are saved as INF (-INF). When saving to integer types all overflows
      are clipped. Attempt to save single precision as double precision will raise an exception. The paramateres must be the same
      as for the <see cref="WriteHeader"/> method. Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TVec;
        AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt(False,[0,0,1,3,2]);
        AStream := TFileStream.Create('C:\test.Vec',fmCreate);
        try
          b.WriteHeader(AStream); // Write info for b
          b.WriteValues(AStream); // Write values of b
        finally
          AStream.Free;
        end;
        FreeIt(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ReadValues"/>
      <SeeAlso cref="WriteHeader"/>
      <SeeAlso cref="TMtxVecBase.SaveToStream"/>*)
    procedure WriteValues(const Dst: TStream;
                                Precision: TPrecision;
                                Rounding: TRounding = rnTrunc; Endian: TEndianness = MtxSystemEndianness); overload; virtual;
    (*<summary>Writes the header information for the calling vector to a stream.</summary>*)
    procedure WriteValues(const Dst: TStream); overload; virtual;
    

    

    
    (*<summary>Writes the header information for the calling vector to a stream.</summary>
      
<remarks>Writes the header information for the calling object to a DstStream stream. The header information contains information about object (size, complex, type of values
      in Values array, ...) which all define the state of the object. Number type is defined by the Precision parameter. Rounding defines the rounding for integer types.
      When saving double precision as single precision, all overflows are saved as INF (-INF). When saving to integer types all overflows are clipped.
      Attempt to save single precision as double precision will raise an exception. The data format uses VersionControl to insure backward compatibilty
      for future versions of MtxVec.
</remarks>


      <Example>
      <code>
      var b: TVec;
          AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt(False,[0,0,1,3,2]);
        AStream := TFileStream.Create('C:\test.bin',fmCreate);
        try
          b.WriteHeader(AStream); // Write info for b
          b.WriteValues(AStream); // Write values of b
        finally
          AStream.Free;
          FreeIt(b);
        end;
      end;
    </code>
    </Example>

    <SeeAlso cref="ReadHeader"/>
    <SeeAlso cref="WriteValues"/>
    <SeeAlso cref="WriteValues"/>*)

    procedure WriteHeader(const Dst: TStream;
                          Precision: TPrecision;
                          Rounding: TRounding = rnTrunc; Endian: TEndianness = MtxSystemEndianness); overload; virtual;
    (*<summary>Writes the header information for the calling vector to a stream.</summary>*)
    procedure WriteHeader(const Dst: TStream); overload; virtual;
    

    


    (*<summary>Reads the header information from a stream to object.</summary>
      
<remarks>Reads the header information from a DstStream stream to calling object. The header information contains all necessary information
      defining the object. The function returns the precision in which the data was stored.
      This information is required for the <see cref="ReadValues"/> method.
</remarks>

      <Example>
      <code>
      var b: TVec;
          AStream: TFileStream;
          Precision: TPrecision;
      begin
        CreateIt(b);
        AStream := TFileStream.Create('C:\test.bin',fmOpenRead);
        try
          Precision := b.ReadHeader(AStream); // Read info for b
          b.ReadValues(AStream,Precision); // Read values of b
        finally
          AStream.Free;
          FreeIt(b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ReadValues"/>
      <SeeAlso cref="WriteValues"/>
      <SeeAlso cref="WriteHeader"/>*)
    

    
    function ReadHeader(const Src: TStream; Endian: TEndianness = MtxSystemEndianness): TPrecision; overload; virtual;
    

  published
    (*<summary>Defines if object values are complex.</summary>
      
<remarks>If true, <see cref="TMtxVec"/> will treat it's elements as complex numbers. Two successive number in the values array be treated as real and imaginary
      part of the complex number. When property is false, object will treat it's elements as real numbers. You should always set the value of
      the Complex property before setting object size (<see cref="TMtxVecBase.Length" text="Length"/> for vector or <see cref="TMtx.Rows"/>,<see cref="TMtx.Cols"/> for matrix).
      Setting Complex from true to false does not cause memory reallocation. It simply doubles the value of the Length property. Setting Complex from false to true
      halves the vector length or number of matrix columns, but retains all data.  Complex property is initially false.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(true, new double[] {1,2,3,4});
            b.SetIt(false,new double[] {8,9,6,7});
            b.Complex = true;
            c.Mul(a,b);   // = [(1+2i)*(8*9i), (3+4i)*(6+7i)]
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="TMtxVecBase.Length"/>
      <SeeAlso cref="TMtx.Rows"/>
      <SeeAlso cref="TMtx.Cols"/>*)
    property Complex: boolean read fComplex write setComplex stored false;

    (*<summary>First element in object Values array.</summary>
      <returns>first real element in object Values array.</returns>

      <SeeAlso cref="Last"/>
      <SeeAlso cref="Firstc"/>*)
    property First: double read GetFirst write SetFirst stored false;

    (*<summary>First element in object CValues array.</summary>
      <returns>first complex element in object CValues array.</returns>

      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is false.
</remarks>


      <SeeAlso cref="Lastc"/>
      <SeeAlso cref="First"/>*)
    property Firstc: TCplx read GetFirstc write SetFirstc stored false;

    (*<summary>Last complex element in object CValues array.</summary>
      <returns>last complex element in object CValues array.</returns>

      <SeeAlso cref="Firstc"/>
      <SeeAlso cref="Last"/>*)
    property Lastc: TCplx read GetLastc write SetLastC stored false;
    (*<summary>Last element in object Values array.</summary>
      <returns>the last real element in object Values array.</returns>

      <SeeAlso cref="First"/>
      <SeeAlso cref="Lastc"/>*)
    property Last: double read GetLast write SetLast stored false;
  end;


  (*<summary>Base class for TVec and TMtx objects.</summary>*)
  TDenseMtxVec = class(TMtxVec)
  strict private
    fFFTStorageFormat: TFFTStorageFormat;
    fFFTScrambledOrder: boolean;
    fFFTOddLength: boolean;
  public
    
    procedure Assign(Src: TPersistent); override;
    procedure Reset; override;
    constructor Create; override;
     

    (*<summary>The FFT storage format.</summary>
      
<remarks>The FFT storage format specifies how will the result of an operation
      be stored in the destination. This affects both 1D and 2D FFT's.
</remarks>
*)
    property FFTStorageFormat: TFFTStorageFormat read fFFTStorageFormat write fFFTStorageFormat;
    (*<summary>Leave FFT scrambeled.</summary>
      
<remarks>The last stage (or first) of an FFT, is a sorting operation.
      This sorting will be left out, if this property will be set to True.

      Note
        This can improve performance for certain applications, if you used properly.
</remarks>
*)
    property FFTScrambled: boolean read fFFTScrambledOrder write fFFTScrambledOrder;
    (*<summary>Defines if FFT transform length is odd.</summary>
      
<remarks>The property should be set to True, if the length of a forward real to complex FFT
      is odd and if the result of an inverser complex to real FFT should be odd.
</remarks>
*)
    property FFTOddLength: boolean read fFFTOddLength write fFFTOddLength;

    (*<summary>Replaces all NAN values with Value.</summary>
               
<remarks>The function will replace all occurences of NAN. If the vector is complex, it will replace any real/imag parts, which are NAN.
</remarks>
*)
    function ReplaceNAN(const Value: double): TMtxVec; overload;

    (*<summary>Replaces all NAN values with Value within calling object elements [Index]..[Index+Len-1].</summary>
               
<remarks>The function will replace all occurences of NAN. If the vector is complex, it will replace any real/imag parts, which are NAN.
               An exception is raised, if array borders are overrun.
</remarks>
*)
    function ReplaceNAN(const Value: double; Index, Len: integer): TMtxVec; overload;

    (*<summary>Adds Value to object elements.</summary>

      <SeeAlso cref="Sub"/>*)
    function Add(const Value: double): TMtxVec; overload; 
    (*<summary>Adds complex Value to all calling object complex elements.</summary>*)
    function Add(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Adds Value to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Adds complex Value to calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: TCplx; Index, Len: integer): TMtxVec; overload; 

    (*<summary>Adds Value to the each element of the Vec object.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object are set automatically.
</remarks>
*)
    function Add(const Vec: TMtxVec; const Value: double): TMtxVec; overload; 
    (*<summary>Adds complex Value to each element of the Vec object.</summary>
      
<remarks>Store the result to the calling object. Size property of the calling object is set
      automatically. <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TMtxVec; const Value: TCplx): TMtxVec; overload; 
    (*<summary>Adds Value to each element of Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="TMtxVec.Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function Add(const Vec: TMtxVec; const Value: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Adds complex Value to each elements of the Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Array addition.</summary>
      
<remarks>Add each of Vec elements to corresponding elements in the calling object.
</remarks>

      <SeeAlso cref="Sub"/>*)
    function Add(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec .</c><para/>
      The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TMtxVec; const aScale: double): TMtxVec; overload; 
    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ Cplx(RScale,IScale)*Vec .</c><para/>
      When default values for parameters are being used the "normal" addition is being performed.
      The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TMtxVec; const RScale: double; const IScale: double): TMtxVec; overload; 
    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec .</c><para/>
      The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TMtxVec; const aScale: TCplx): TMtxVec; overload; 
    (*<summary>Add each of Vec2 elements to corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/> properties
      of the calling object are set implicitly to match Vec1 and Vec2 vectors.
</remarks>
*)
    function Add(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Add Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function Add(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The following formula is being used:

      <c>result = result + aScale*Vec .</c><para/>
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TMtxVec; const aScale: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec .</c><para/>
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TMtxVec; const aScale: TCplx; VecIndex, Index, Len: integer): TMtxVec; overload; 

    

    (*<summary> Compute X + Y*yScale </summary>*)
    function AddScaled(const X, Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale with complex arguments.</summary>*)
    function AddScaled(const X, Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale on sub arrays. </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale on sub arrays with complex arguments. </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute sqr(X + Y*yScale) </summary>
       
<remarks>By making use of yScale, it is also possible to compute the following (at the same or higher speed):

       (X - Y)^2 = X^2 - 2XY  +Y^2
</remarks>
*)
    function AddScaledSqr(const X, Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute sqr(X + Y*yScale) </summary>*)
    function AddScaledSqr(const X, Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute sqr(X + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute sqr(X + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute sqr(X) + sqr(Y)*yScale </summary>
       
<remarks>By making use of yScale, it is also possible to compute the following (at the same or higher speed):

       X^2 - Y^2
</remarks>
*)
    function SqrAddScaled(const X, Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute sqr(X) + sqr(Y)*yScale </summary>*)
    function SqrAddScaled(const X, Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute sqr(X) + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute sqr(X) + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X + Y + Z </summary>*)
    function Add(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X + Y + Z on sub arrays </summary>*)
    function Add(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X + Y + Z*zScale </summary>*)
    function AddScaled(const X, Y, Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X + Y + Z*zScale </summary>*)
    function AddScaled(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X + Y + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X + Y + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X, Y: TMtxVec; const yScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X, Y: TMtxVec; const yScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;


    (*<summary> Compute X + Y + zScalar </summary>*)
    function Add(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X + Y + zScalar </summary>*)
    function Add(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X + Y + zScalar on sub arrays </summary>*)
    function Add(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X + Y + zScalar on sub arrays </summary>*)
    function Add(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X, Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X, Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + zScalar on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X + Y*yScale + zScalar on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    

    (*<summary> Compute X - Y - Z </summary>*)
    function Sub(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X - Y - Z on sub array</summary>*)
    function Sub(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer;const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X, Y: TMtxVec; const yScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X, Y: TMtxVec; const yScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X - Y - Z*zScale </summary>*)
    function SubScaled(const X, Y, Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X - Y - Z*zScale </summary>*)
    function SubScaled(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X - Y - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X - Y - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;


    (*<summary> Compute X - Y - zScalar </summary>*)
    function Sub(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X - Y - zScalar </summary>*)
    function Sub(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X - Y - zScalar on sub array</summary>*)
    function Sub(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X - Y - zScalar on sub array</summary>*)
    function Sub(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X, Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X, Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    

    (*<summary> Compute X * Y / Z </summary>*)
    function MulAndDiv(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X * Y / Z on sub array</summary>*)
    function MulAndDiv(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    

    (*<summary> Compute (X + Y)*Z </summary>*)
    function AddAndMul(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X + Y)*Z on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X + Y)*Z*zScale </summary>*)
    function AddAndMul(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute (X + Y)*Z*zScale </summary>*)
    function AddAndMul(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + Y)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X + Y)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X + Y*yScale)*Z*zScale </summary>*)
    function AddAndMul(const X, Y: TMtxVec; const yScale: double; const Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*Z*zScale </summary>*)
    function AddAndMul(const X, Y: TMtxVec;const yScale: TCplx; const Z: TMtxVec;const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale: double; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X + Y)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X + Y)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + Y)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X + Y)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X + Y*yScale)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVec; const yScale, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVec; const yScale, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale, Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale, Z: TCplx; Index, Len: Integer): TMtxVec; overload;


    (*<summary> Compute (X + yScalar)*Z*zScale </summary>*)
    function AddAndMul(const X: TMtxVec; const Y: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z*zScale </summary>*)
    function AddAndMul(const X: TMtxVec; const Y: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVec; const xScale, Y: Double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVec; const xScale, Y: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale, Y: Double; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale, Y: TCplx; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVec; const Y: Double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVec; const Y: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y: Double; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y: TCplx; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X + yScalar)*zScalar </summary>*)
    function AddAndMul(const X: TMtxVec; const Y, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*zScalar </summary>*)
    function AddAndMul(const X: TMtxVec; const Y, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*zScalar on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y, Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X + yScalar)*zScalar on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const Y, Z: TCplx; Index, Len: integer): TMtxVec; overload;


    

    (*<summary> Compute (X - Y)*Z </summary>*)
    function SubAndMul(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X + Y)*Z on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X - Y)*Z*zScale </summary>*)
    function SubAndMul(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute (X - Y)*Z*zScale </summary>*)
    function SubAndMul(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - Y)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer;const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X - Y)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer;const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X - Y*yScale)*Z*zScale </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const yScale: double; Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*Z*zScale </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const yScale: TCplx; Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale: double; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X - Y)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X - Y)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - Y)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X - Y)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X - Y*yScale)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const yScale, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVec; const yScale, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale, Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const yScale, Z: TCplx; Index, Len: Integer): TMtxVec; overload;


    (*<summary> Compute (X - yScalar)*Z*zScale </summary>*)
    function SubAndMul(const X: TMtxVec; const Y: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z*zScale </summary>*)
    function SubAndMul(const X: TMtxVec; const Y: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVec; const xScale, Y: Double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVec; const xScale, Y: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale, Y: Double; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale, Y: TCplx; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVec; const Y: Double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVec; const Y: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y: Double; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y: TCplx; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X - yScalar)*zScalar </summary>*)
    function SubAndMul(const X: TMtxVec; const Y, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*zScalar </summary>*)
    function SubAndMul(const X: TMtxVec; const Y, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*zScalar on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y, Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X - yScalar)*zScalar on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const Y, Z: TCplx; Index, Len: integer): TMtxVec; overload;


    

    (*<summary> Compute X*Y + Z </summary>*)
    function MulAndAdd(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y + Z on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y*xyScale + Z </summary>*)
    function MulAndAdd(const X, Y: TMtxVec; const xyScale: double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale + Z </summary>*)
    function MulAndAdd(const X, Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: double; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y + Z*zScale </summary>*)
    function MulAndAdd(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute X*Y + Z*zScale </summary>*)
    function MulAndAdd(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*Y + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y + zScalar </summary>*)
    function MulAndAdd(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X*Y + zScalar </summary>*)
    function MulAndAdd(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X*Y + zScalar on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y + zScalar on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    

    (*<summary> Compute X*Y - Z </summary>*)
    function MulAndSub(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y + Z on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y*xyScale - Z </summary>*)
    function MulAndSub(const X, Y: TMtxVec; const xyScale: double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale - Z </summary>*)
    function MulAndSub(const X, Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: double; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y - Z*zScale </summary>*)
    function MulAndSub(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute X*Y - Z*zScale </summary>*)
    function MulAndSub(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*Y + Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y + Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*Y - zScalar </summary>*)
    function MulAndSub(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X*Y - zScalar </summary>*)
    function MulAndSub(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X*Y - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X*Y - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    

    (*<summary> Compute X/Y + Z </summary>*)
    function DivAndAdd(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y + Z on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y*xyScale + Z </summary>*)
    function DivAndAdd(const X, Y: TMtxVec; const xyScale: double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale + Z </summary>*)
    function DivAndAdd(const X, Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale + Z on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: double; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale + Z on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y + Z*zScale </summary>*)
    function DivAndAdd(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute X/Y + Z*zScale </summary>*)
    function DivAndAdd(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X/Y + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y + zScalar </summary>*)
    function DivAndAdd(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X/Y + zScalar </summary>*)
    function DivAndAdd(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X/Y + zScalar on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y + zScalar on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    

        (*<summary> Compute X/Y - Z </summary>*)
    function DivAndSub(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y*xyScale - Z </summary>*)
    function DivAndSub(const X, Y: TMtxVec; const xyScale: double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale - Z </summary>*)
    function DivAndSub(const X, Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: double; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y*xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y - Z*zScale </summary>*)
    function DivAndSub(const X, Y, Z: TMtxVec; const zScale: double): TMtxVec; overload;
    (*<summary> Compute X/Y - Z*zScale </summary>*)
    function DivAndSub(const X, Y, Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X/Y - Z*zScale on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y - Z*zScale on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X/Y - zScalar </summary>*)
    function DivAndSub(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X/Y - zScalar </summary>*)
    function DivAndSub(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X/Y - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute X/Y - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    

    (*<summary> Compute X*xScale + Y*yScale </summary>*)
    function AddScaled(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale </summary>*)
    function AddScaled(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute sqr(X*xScale + Y*yScale) </summary>*)
    function AddScaledSqr(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute sqr(X*xScale + Y*yScale) </summary>*)
    function AddScaledSqr(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute sqr(X*xScale + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute sqr(X*xScale + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale </summary>*)
    function SqrAddScaled(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double): TMtxVec; overload;
    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale </summary>*)
    function SqrAddScaled(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx): TMtxVec; overload;
    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X*xScale + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X*xScale + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + z on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale + Y*yScale + z on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X*xScale - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;

    (*<summary> Compute X*xScale - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: TMtxVec; yIndex: integer; const yScale: Double; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TMtxVec; yIndex: integer; const yScale: TCplx; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X*Y*Z </summary>
       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*Y
</remarks>
*)
    function Mul(const X, Y,Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X*Y*zScalar </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*zScalar
</remarks>
*)
    function Mul(const X,Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X*Y*zScalar </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*zScalar
</remarks>
*)
    function Mul(const X,Y: TMtxVec; const Z: TCplx): TMtxVec; overload;


    (*<summary> Compute X*Y*Z on sub array</summary>*)
    function Mul(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*Y*zScalar on sub array</summary>*)
    function Mul(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*Y*zScalar on sub array</summary>*)
    function Mul(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X / (Y*Z) </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing Y also for the Z parameter:

       X / (Y^2)
</remarks>
*)
    function Divide(const X, Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X/( Y*Z ) on sub array</summary>*)
    function Divide(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X / (Y*zScale) </summary>*)
    function DivideC(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X / (Y*zScale) </summary>*)
    function DivideC(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute X / (Y*zScalar) on sub array</summary>*)
    function DivideC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X / (Y*zScalar) on sub array</summary>*)
    function DivideC(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X * Y * xyScale / Z </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2 * xyScale/ Z
</remarks>
*)
    function MulAndDiv(const X, Y: TMtxVec; const xyScale: Double; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X * Y * xyScale / Z </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2 * xyScale/ Z
</remarks>
*)
    function MulAndDiv(const X, Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute X * Y * xyScale / Z on sub array</summary>*)
    function MulAndDiv(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const xyScale: Double; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X * Y * xyScale / Z on sub array</summary>*)
    function MulAndDiv(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute X * Y / zScalar </summary>*)
    function MulAndDiv(const X, Y: TMtxVec; const Z: Double): TMtxVec; overload;
    (*<summary> Compute X * Y / zScalar </summary>*)
    function MulAndDiv(const X, Y: TMtxVec; const Z: TCplx): TMtxVec; overload;

    (*<summary> Compute X * Y / zScalar on sub array</summary>*)
    function MulAndDiv(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X * Y / zScalar on sub array</summary>*)
    function MulAndDiv(const X: TMtxVec; xIndex: integer; const Y: TMtxVec; yIndex: integer; const Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute xScalar / (Y*Z) </summary>*)
    function Divide(const X: Double; const Y, Z: TMtxVec): TMtxVec; overload;
    (*<summary> Compute xScalar / (Y*Z) </summary>*)
    function Divide(const X: TCplx; const Y, Z: TMtxVec): TMtxVec; overload;

    (*<summary> Compute xScalar / (Y*Z) on sub array</summary>*)
    function Divide(const X: Double; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute xScalar / (Y*Z) on sub array</summary>*)
    function Divide(const X: TCplx; const Y: TMtxVec; yIndex: integer; const Z: TMtxVec; zIndex: integer; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale + Y*yScale)*Z*zScale</summary>

        
<remarks>The following is also possible at the same or higher speed:

        X^2 * xScale + XY * yScale

        by passing X also for the Z parameter.
</remarks>
*)
    function AddAndMul(const X: TMtxVec; xScale: Double; const Y: TMtxVec; yScale: Double; const Z: TMtxVec; zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*Z*zScale</summary>

        
<remarks>The following is also possible at the same or higher speed:

        X^2 * xScale + XY * yScale

        by passing X also for the Z parameter.
</remarks>
*)
    function AddAndMul(const X: TMtxVec; xScale: TCplx; const Y: TMtxVec; yScale: TCplx; const Z: TMtxVec; zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*Z*Scale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; xScale: Double; const Y: TMtxVec; yIndex: Integer; yScale: Double; const Z: TMtxVec; zIndex: Integer; zScale: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*Z*Scale on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; xScale: TCplx; const Y: TMtxVec; yIndex: Integer; yScale: TCplx; const Z: TMtxVec; zIndex: Integer; zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale + Y*yScale)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const xScale: Double; const Y: TMtxVec; yIndex: Integer; const yScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: Integer; const xScale: TCplx; const Y: TMtxVec; yIndex: Integer; const yScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X* xScale + yScalar)*Z*zScale</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: Double; const Y: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X* xScale + yScalar)*Z*zScale</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: TCplx; const Y: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X* xScale + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X* xScale + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale + yScalar)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: Double; const Y, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVec; const xScale: TCplx; const Y, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y, Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale + yScalar)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y, Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale - Y*yScale)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*Z*Scale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const xScale: Double; const Y: TMtxVec; yIndex: Integer; const yScale: Double; const Z: TMtxVec; zIndex: Integer; const zScale: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*Z*Scale on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const xScale: TCplx; const Y: TMtxVec; yIndex: Integer; const yScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale - Y*yScale)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: Double; const Y: TMtxVec; const yScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: TCplx; const Y: TMtxVec; const yScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const xScale: Double; const Y: TMtxVec; yIndex: Integer; const yScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: Integer; const xScale: TCplx; const Y: TMtxVec; yIndex: Integer; const yScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale - yScalar)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: Double; const Y: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: TCplx; const Y: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*xScale - yScalar)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: Double; const Y, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVec; const xScale: TCplx; const Y, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale: Double; const Y, Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*xScale - yScalar)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVec; xIndex: integer; const xScale: TCplx; const Y, Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + Z*zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: TMtxVec; zIndex: Integer; const zScale: Double; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + Z*zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer;const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*Y)*xyScale + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale + zScalar on sub array</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale + zScalar on sub array</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;


    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; const Y, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: integer; const Y, Z: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; const Y, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVec; xIndex: integer; const Y, Z: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X*Y)*xyScale - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: TMtxVec; zIndex: Integer; const zScale: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X*Y)*xyScale - zScalar</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale, Z: Double): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - zScalar</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale, Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X*Y)*xyScale - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute X*yScalar - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute X*yScalar - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVec; const Y: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute X*yScalar - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: integer; const Y: Double; const Z: TMtxVec; zIndex: integer; const zScale: Double; Index, Len: integer): TMtxVec; overload;
    (*<summary> Compute X*yScalar - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVec; xIndex: integer; const Y: TCplx; const Z: TMtxVec; zIndex: integer; const zScale: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary> Compute (X / Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing Z also for the Y parameter:

       (X / Z) * xyScale + Z * zScale
</remarks>
*)
    function DivAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X / Y)*xyScale + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: TMtxVec; zIndex: Integer; const zScale: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X / Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing Z also for the Y parameter:

       (X / Z) * xyScale + Z * zScale
</remarks>
*)
    function DivAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X / Y)*xyScale + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X / Y) * xyScale - Z*zScale </summary>*)
    function DivAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: TMtxVec; const zScale: Double): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: TMtxVec; zIndex: Integer; const zScale: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - Z*zScale </summary>*)
    function DivAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TMtxVec; const zScale: TCplx): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TMtxVec; zIndex: Integer; const zScale: TCplx; Index, Len: Integer): TMtxVec; overload;

    (*<summary> Compute (X / Y) * xyScale - zScalar</summary>*)
    function DivAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: Double; const Z: Double): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: Double; const Z: Double; Index, Len: Integer): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - zScalar</summary>*)
    function DivAndSub(const X: TMtxVec; const Y: TMtxVec; const xyScale: TCplx; const Z: TCplx): TMtxVec; overload;
    (*<summary> Compute (X / Y) * xyScale - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVec; xIndex: Integer; const Y: TMtxVec; yIndex: Integer; const xyScale: TCplx; const Z: TCplx; Index, Len: Integer): TMtxVec; overload;

      (*<summary>Calculate the cumulative product for all Vec elements.</summary>
                
<remarks>The size must be set by the user. The function will store all powers of Value:

                Values[Index + i] := IntPower(Value, i + 1)

                at the corresponding array Index.
</remarks>
*)
    function CumProduct(Value: Double; Index, Len: integer): TMtxVec; overload;

      (*<summary>Calculate the cumulative product for all Vec elements.</summary>
                
<remarks>The size must be set by the user. The function will store all powers of Value:

                Values[Index + i] := IntPower(Value, i + 1)

                at the corresponding array Index.
</remarks>
*)
    function CumProduct(const Value: TCplx; Index, Len: integer): TMtxVec; overload;

    (*<summary>Add Vec1 elements [Vec1Index]..[Vec1Index+Len-1] to Vec2 elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is
      raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)

    function Add(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Add a product of two vectors.</summary>
      
<remarks>Multiply Vec1 elements with coresponding Vec2 elements and add the result
      to the calling vector. The size of the calling vector is set implicitly.
</remarks>


      <SeeAlso cref="Mul"/>
      <SeeAlso cref="Add"/>*)
    function AddProduct(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and
      add the results to the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function AddProduct(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 

    
    (*<summary>Conjugate and multiply.</summary>
      
<remarks>Conjugate each of Vec elements and multiply them with corresponding elements in the calling object.
      The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object are set implicitly to match Vec.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {4,3,,2,1});
            c.ConjMul(a,b);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Conj"/>
      <SeeAlso cref="Mul"/>*)
    function ConjMul(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1] and multiply them with corresponding
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties are <b>not</b> set.

      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun or underrun.
</remarks>
*)
    function ConjMul(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Conjugate each of Vec2 elements and multiply them with corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of
      the calling object are set implicitly to match Vec1 and Vec2 objects.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Conjugate Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and multiply them with corresponding Vec1 elements
      [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TMtxVec.Complex"/> properties of the calling vector must be set explicitly.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Cumulative sum.</summary>
    
<remarks>Calculate the cumulative sum for calling object elements [Index]..[Index+Len-1] in-place.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});
            a.CumSum();  // a = [1,3,6,10]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sum"/>*)
    function CumSum(Index, Len: Integer): TMtxVec; overload; 
    (*<summary>Calculate the cumulative sum for Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/> properties of the
      calling object must be set explicitly. Exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>  property is True
      and array borders are overrun.
</remarks>
*)
    function CumSum(const Vec: TMtxVec; VecIndex,Index,Len: Integer): TMtxVec; overload; 

    (*<summary>The forward discrete cosine transform (DCT).</summary>
      
<remarks>Calculates the forward discrete cosine transform (DCT) of the Vec and writes the result in the calling vector.
      If Vec.Length is a power of 2, the function uses an efficient algorithm that is significantly faster than
      the direct computation of DCT. For other values of Vec Length, this function uses the direct formulas
      given below; however, the symmetry of cosine function is taken into account, which allows to perform
      about half of the multiplication operations in the formulas. In the following definition of DCT,
      <c>N=Vec.Length</c> and V is the calling vector:

      <IMG name="TVec18"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false,new double[] {1,-2,3,4});
            b.DCT(a);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="IDCT"/>
      <SeeAlso cref="FFT"/>*)
    function DCT(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Difference.</summary>
      
<remarks>Calculate the difference for Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object
      elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/> properties of the calling vector must be set explicitly.
      The following formula is used to calculate the difference:

      <IMG name="tvec19"/><para/>
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function Difference(const Vec: TMtxVec; VecIndex, Index, Len: integer; Lag: integer = 1): TMtxVec; overload; 

    (*<summary>Vector division.</summary>
      
<remarks>Divide each of Vec elements with corresponding elements in the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.

      The threshold parameter is used to prevent against division by zero and
      numerical instabilities in the follow on processing. All values of Vec smaller
      than Threshold will be replaced with Threshold.
</remarks>


      <SeeAlso cref="Mul"/>*)
    function Divide(const Vec: TMtxVec; const Threshold: double): TMtxVec; overload; 
    (*<summary>Divide each of Vec elements with corresponding elements in the calling object.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="TMtxVec.Complex"/> property
      of the calling object are set automatically.
</remarks>
*)
    function Divide(const Vec: TMtxVec; const Threshold: double; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide each of Vec1 elements with corresponding elements in the Vec2 object.</summary>
       
<remarks>Store the result in the calling vector.
</remarks>
*)
    function Divide(const Vec1, Vec2: TMtxVec; const Threshold: double): TMtxVec; overload; 
    (*<summary>Divide Vec1 elements [Vec1Index]...[Vec1Index+Len-1] with corresponding elements
       [Vec2Index]...[Vec2Index+Len-1] from the Vec object.</summary>
       
<remarks>Store the result in the calling vector [Index]...[Index+Len-1].
</remarks>
*)
    function Divide(const Vec1, Vec2: TMtxVec; const Threshold: double; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide each of Num elements with corresponding elements in Den.</summary>
      
<remarks>Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Num, Den: TMtxVec): TMtxVec; overload; 
    (*<summary>Divide each of calling vector elements with corresponding elements in the Vec object.</summary>
      
<remarks>Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically. The result
      is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Divide calling vector elements [Index]...[Index+Len-1] with corresponding elements
       [VecIndex]...[VecIndex+Len-1] from the Vec object.</summary>
       
<remarks>Store the result in the claling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
       of the calling vector is not changed. An exception is raised if
       <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun
       or underrun.

       The result of division by zero will be the INF constant. Division of zero
       by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide [NumIndex]..[NumIndex+Len-1] Num elements with [DenIndex]..[DenIndex+Len-1] elements in Den.</summary>
      
<remarks>and store result in the calling vector at positions [Index]..[Index+Len-1]
      <see cref="TMtxVec.Size"/> and <see cref="TMtxVec.Complex"/> property of the calling object are not changed.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun or underrun.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Num, Den: TMtxVec; NumIndex, DenIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Divide Value with elements of the calling object and store the result in the calling object.</summary>*)
    function DivideBy(const Value: double): TMtxVec; overload; 
    (*<summary>Divide complex Value with elements of the calling object.</summary>
      
<remarks>Store the result in the calling object. Calling vector will be extended to complex, if the calling vector is real.
</remarks>
*)
    function DivideBy(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Divide Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      An exception will be raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide complex Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      Calling vector will be extended to complex, if the calling vector is real.
      An exception will be raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(const Value: TCplx; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size and <see cref="TMtxVec.Complex"/> properties of the calling object are set automatically.
</remarks>
*)
    function DivideBy(const Value: double; const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Divide complex Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size of the calling object is set automatically.
      <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(const Value: TCplx; const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Divide Value with Vec elements [VecIndex]..[VecIndes+Len-1].</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception will be raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is
      True and array borders are overrun or underrun.
      <see cref="TMtxVec.Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function DivideBy(const Value: double; const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Divide complex Value with elements [VecIndex]..[VecIndes+Len-1] from Vec.</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object. Size of the calling object is not changed. An exception will be raised
       if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
      <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(const Value: TCplx; const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Same as the <see cref="Divide"/> method.</summary>*)
    function DivideVec(const Num, Den: TMtxVec): TMtxVec; overload;  deprecated;
    (*<summary>Same as the <see cref="Divide"/> method.</summary>*)
    function DivideVec(const Vec: TMtxVec): TMtxVec; overload;  deprecated;
    (*<summary>Same as the <see cref="Divide"/> method.</summary>*)
    function DivideVec(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload;  deprecated;

    (*<summary>Scalar product of two real arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a real scalar value.
      The dot product is defined by the equation:

      <IMG name="TVec13"/><para/>
      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            double prod = a.DotProd(b); // = 1*5 + 2*6 + * 3*7 + 4*8
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)
    function DotProd(const Vec: TMtxVec): double; overload; 
    (*<summary>Returns the scalar product between Vec elements [VecIndex]..[VecIndex+Len-1] and calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property is True.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function DotProd(const Vec: TMtxVec; VecIndex, Index, Len: integer): double; overload; 

    (*<summary>Scalar product of two complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a complex scalar value.
      An exception is raised if calling or Vec object <see cref="TMtxVec.Complex"/> property is false.
      The dot product is defined by the equation:<para/>

      <IMG name="TVec13"/><para/>

      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            double prod = a.DotProdc(b); //= (1+2i)*(5+6i)+(3+4i)*(7+8i)
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProd"/>*)
    function DotProdc(const Vec: TMtxVec): TCplx; overload; 
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property is False.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TMtxVec; VecIndex, Index, Len: integer): TCplx; overload; 
    (*<summary>Returns the scalar product between Vec and calling object complex elements.</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property is False.
</remarks>
*)
    function DotProdc(const Vec: TMtxVec; ConjVec: boolean): TCplx; overload; 
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property is False.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TMtxVec; ConjVec: boolean; VecIndex, Index, Len: integer): TCplx; overload; 

    (*<summary>Downsamples object values.</summary>
      
<remarks>Copy only every Factor sample from Src starting at SrcIndex up to Len to the calling object starting
      at Index. The phase parameter determines the initial sample offset. Phase must be less then Factor. Size and
      <see cref="TMtxVec.Complex"/> properties of the calling object are set implicitly. An exception is raised if array borders are
      overrun/underrun.
</remarks>


      <SeeAlso cref="UpSample"/>*)
    function DownSample(const Src: TMtxVec; Factor, SrcIndex, Index, Len: integer; Phase: integer = 0): TMtxVec; overload; 

    (*<summary>Fast Furier Transformation (FFT) from complex to complex or from real to complex.</summary>
      
<remarks>Calculate the FFT from Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object elements
      [Index]..[Index+Len-1]. The Len parameter must be a power of two. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun. If Vec is complex, then complex to complex forward FFT is performed. If Vec is real, then real to
      complex forward FFT is performed.

      This is the indexed version of the FFT routine <see cref="TVec.FFT"/>. Look there
      for more information on FFT parameters and storage requirements.
</remarks>
*)
    function FFT(const Vec: TMtxVec; ConjugateExtend: boolean; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The forward Fast Fourier Transformation (FFT) from real to complex.</summary>
      
<remarks>Calculate the FFT from real Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object elements
      [Index]..[Index+Len-1]. The Len parameter must not be a power of two. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and Vec is not complex or if array borders are overrun.

      This is the indexed version of the FFT routine <see cref="TVec.FFTFromReal"/>. Look there
      for more information on FFT parameters and storage requirements.
</remarks>


      <SeeAlso cref="TVec.IFFT"/>
      <SeeAlso cref="TMtx.IFFT1D"/>*)
    function FFTFromReal(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse discrete cosine transform (DCT).</summary>
      
<remarks>Calculate the inverse discrete cosine transform (DCT) from Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1]. The Len parameter must be the power of two. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.
</remarks>
*)
    function IDCT(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>The inverse FFT from complex to complex.</summary>
      
<remarks>Calculate the inverse FFT from Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1]. The Len parameter must be the power of two. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and
      array borders are overrun.

      This is the indexed version of the FFT routine <see cref="TVec.IFFT"/>. Look there
      For more information on FFT parameters and storage requirements.
</remarks>


      <SeeAlso cref="TVec.IFFT"/>
      <SeeAlso cref="TMtx.IFFT1D"/>*)
    function IFFT(const Vec: TMtxVec; VecIndex, Index, Len: integer; NoScale: boolean=False): TMtxVec; overload; 

    (*<summary>The inverse FFT from complex to real.</summary>
      
<remarks>Calculate the inverse FFT from Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1]. The Len parameter must not be a power of two. Size
      and <see cref="TMtxVec.Complex"/> properties of the calling object must be set explicitly. An exception is raised if
      <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun. <para/>
      This is the indexed version of the FFT routine <see cref="TVec.IFFTToReal"/>. Look there
      for more information on FFT parameters and storage requirements.
</remarks>


      <SeeAlso cref="TVec.IFFTToReal"/>
      <SeeAlso cref="TMtx.IFFT1DToReal"/>*)
    function IFFTToReal(const Vec: TMtxVec; VecIndex, Index, Len: integer; NoScale: boolean=False): TMtxVec; overload; 

    (*<summary>The Kurtosis (fourth central momentum).</summary>
      
<remarks>Calculate the calling object kurtosis by using mean value AMean and standard deviation AStdDev.
      Kurtosis is the fourth central moment, divided by fourth power of standard deviation:<para/>

      <IMG name="TVec06"/><para/>
</remarks>


      <SeeAlso cref="Skewness"/>
      <SeeAlso cref="Mean"/>
      <SeeAlso cref="StdDev"/>*)
    function Kurtosis(const AMean, AStdDev: double): double; overload; 
    (*<summary>Calculate the kurtosis for elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
      The AMean and AStdDev passed as parameters must be computed from the same
      elements (Index, Len) as the Kurtosis itself.
</remarks>
*)
    function Kurtosis(const AMean, AStdDev: double; Index, Len: integer): double; overload; 

    (*<summary>Median.</summary>
      
<remarks>Calculate median value for all calling object elements.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4,5});
            double c = a.Median(); // 3.0
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mean"/>*)
    function Median: double; overload; 
    (*<summary>Calculate median value for calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function Median(Index: integer; Len: integer): double; overload; 

    (*<summary>Vector multiplication.</summary>
      
<remarks>Multiply each of Vec elements with corresponding elements in the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.
</remarks>


      <SeeAlso cref="Divide"/>*)
    function Mul(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function Mul(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Multiply all Vec1 elements with corresponding Vec2 elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVec.Complex"/> property do not match.
</remarks>
*)
    function Mul(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 


    (*<summary>Vectorised maximum.</summary>
      
<remarks>Compares Vec values with corresponding elements in the calling object and stores the bigger value in Self.
      Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically.
      Supports real value data only.
</remarks>


      <SeeAlso cref="MinEvery"/>*)
    function MaxEvery(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Compare Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] and store result in Self.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property do not match or if array
      borders are overrun/underrun. Supports real value data only.
</remarks>
*)
    function MaxEvery(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and store bigger value in Self.</summary>
      
<remarks>Stores the results in to the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVec.Complex"/> property do not match.
      Supports real value data only.
</remarks>
*)
    function MaxEvery(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Compare Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1] and store bigger value in Self.</summary>
      
<remarks>Stores the results in to the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function MaxEvery(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 



    (*<summary>Vectorised minimum.</summary>
      
<remarks>Compares Vec values with corresponding elements in the calling object and stores the smaller value in Self.
      Size and <see cref="TMtxVec.Complex"/> property of the calling object are set automatically.
      Supports real value data only.
</remarks>


      <SeeAlso cref="MinEvery"/>*)
    function MinEvery(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Compare Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] and store result in Self.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVec.Complex"/> property do not match or if array
      borders are overrun/underrun. Supports real value data only.
</remarks>
*)
    function MinEvery(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and store the smaller value in Self.</summary>
      
<remarks>Stores the results in to the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVec.Complex"/> property do not match.
      Supports real value data only.
</remarks>
*)
    function MinEvery(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Compare Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1] and store the smaller value in Self.</summary>
      
<remarks>Stores the results in to the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function MinEvery(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 



    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object elements. The result is a real value.</returns>
      
<remarks>An exception is raised is calling object <see cref="TMtxVec.Complex"/> is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            double b = a.Max(); // 4.0
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Min"/>
      <SeeAlso cref="Maxc"/>
      <SeeAlso cref="MaxMin"/>*)
    function Max: double; overload; 
    (*<summary>Returns the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value. An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Max(Index,Len: integer): double; overload; 
    (*<summary>Calculate the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Max(out AMax: double; Index,Len: integer); overload; 
    (*<summary>Calculate the maximum value of all calling object elements.</summary>
      
<remarks>The AMax parameter returns the maximum value. The aIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true.
</remarks>
*)
    procedure Max(out AMax: double; out aIndex: integer); overload; 
    (*<summary>Calculate the maximum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the maximum value. The aIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Max(out AMax: double; out aIndex: integer; Index, Len: integer); overload; 
    (*<summary>Same as <see cref="Maxc"/> method.</summary>*)
    function Max(out AMax: TCplx; Index, Len: integer): integer; overload;  deprecated;

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object complex elements. Complex elements are first compared by the amplitude
        and then by the argument.</returns>

      
<remarks>An exception is raised if calling object <see cref="TMtxVec.Complex"/> is False.
</remarks>


      <SeeAlso cref="Max"/>*)
    function Maxc: TCplx; overload; 
    (*<summary>Returns the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised if calling object
      <see cref="TMtxVec.Complex"/> property is False or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(Index,Len: integer): TCplx; overload; 
    (*<summary>Calculate the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns complex maximum value. Returns the index of maximum value. Complex elements are first compared by the amplitude and then by the argument.
      The aIndex parameter returns the index of maximum value. An exception is raised if calling object <see cref="TMtxVec.Complex"/>
      property is False or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(out AMax: TCplx; Index, Len: integer): integer; overload; 

    (*<summary>Maximum and minimum value in a single pass.</summary>
      
<remarks>Calculates the maximum and minimum value of all calling object elements in a single pass. Maximum value is stored in AMax parameter,
      minimum value is stored in AMin parameter. Use this method if you require minimum AND maximum value.
</remarks>


      <SeeAlso cref="Min"/>
      <SeeAlso cref="Max"/>*)
    procedure MaxMin(out AMax,AMin: double); overload; 
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is stored in AMax parameter, minimum value is stored in AMin parameter.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure MaxMin(out AMax,AMin: double; Index, Len: integer); overload; 
    (*<summary>Calculates the maximum and minimum value of all calling object elements.</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer); overload; 
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer; Index, Len: integer); overload; 

    (*<summary>Mean value.</summary>
      
<remarks>Calculate the mean value of all calling object elements. The result is a real value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            double m = a.Mean(); // 2.5
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sum"/>
      <SeeAlso cref="Meanc"/>*)
    function Mean: double; overload; 
    (*<summary>Returns real mean value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Mean(Index, Len: integer): double; overload; 
    (*<summary>Calculate the mean value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun/underrun.
</remarks>
*)
    procedure Mean(out AMean: double; Index, Len: integer); overload; 
    (*<summary>Same as <see cref="Meanc"/>.</summary>*)
    procedure Mean(out AMean: TCplx); overload;  deprecated;
    (*<summary>Same as <see cref="Meanc"/>.</summary>*)
    procedure Mean(out AMean: TCplx; Index, Len: integer); overload;  deprecated;

    (*<summary>Mean value.</summary>
      <returns>the mean value of all calling object complex elements. The result is a complex value.</returns>
      
<remarks>An exception is raised is calling object <see cref="TMtxVec.Complex"/> is False.
</remarks>


      <SeeAlso cref="Mean"/>*)
    function Meanc: TCplx; overload; 
    (*<summary>Returns complex mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is False or array borders are overrun/underrun.
</remarks>
*)
    function Meanc(Index, Len: integer): TCplx; overload; 
    (*<summary>Calculate the mean value from all calling object complex elements.</summary>
      
<remarks>The result AMean is a complex value. An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is False.
</remarks>
*)
    procedure Meanc(out AMean: TCplx); overload; 
    (*<summary>Calculate the mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a complex value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is False or array borders are overrun/underrun.
</remarks>
*)
    procedure Meanc(out AMean: TCplx; Index, Len: integer); overload; 

    (*<summary>Minimum value.</summary>
      <returns>The minimum value of all calling object elements. The result is a real value.</returns>

      
<remarks>An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            double b = a.Min();  // 1.0
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>
      <SeeAlso cref="Minc"/>*)
    function Min: double; overload; 
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Min(Index,Len: integer): double; overload; 
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMin is a real value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Min(out AMin: double; Index,Len: integer); overload; 
    (*<summary>Calculate the minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the
      minimum value. The aIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Min(out AMin: double; out aIndex: Integer; Index, Len: integer); overload; 
    (*<summary>Calculate the minimum value of all calling object elements.</summary>
      
<remarks>The AMin parameter returns the
      minimum value. The aIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true.
</remarks>
*)
    procedure Min(out AMin: double; out aIndex: integer); overload; 
    (*<summary>Same as the <see cref="Minc"/> method.</summary>*)
    function Min(out AMin: TCplx; Index, Len: integer): integer; overload;  deprecated;

    (*<summary>Minimum value.</summary>
      
<remarks>Returns the minimum value of all calling object complex elements. Complex elements are first compared by the amplitude
      and then by the argument. An exception is raised if calling object <see cref="TMtxVec.Complex"/> is False.
</remarks>


      <SeeAlso cref="Min"/>*)
    function Minc: TCplx; overload; 
    (*<summary>Returns the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised
      if calling object. <see cref="TMtxVec.Complex"/> property is False or array borders are overrud/underrun.
</remarks>
*)
    function Minc(Index,Len: integer): TCplx; overload; 
    (*<summary>Calculate the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMin parameter returns complex minimum value.
      Returns the index of minimum value. Complex elements are first compared by the amplitude and then by the argument.
      An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is False or array borders are overrun/underrun.
</remarks>
*)
    function Minc(out AMin: TCplx; Index, Len: integer): integer; overload; 

    (*<summary>The C-norm.</summary>
      <returns>C norm: <c>||V-Vec||</c>, where V is the calling vector.</returns>
      
<remarks>If the NormC is called without any parameters, the NormC
      calculates the norm of the calling vector. The C norm of <c>||V-Vec||</c> is defined by the formula:<para/>

      <IMG name="TVec10"/><para/>
      If RelativeError is true then the computed norm is divided by the
      norm of V, and the function returns the "relative error":<para/>

      <IMG name="TVec09"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            a.SetIt(false, new double[] {4,3,2,1});
            double c = a.NormC(b,true);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormL1"/>
      <SeeAlso cref="NormL2"/>*)
    function NormC(const Vec: TDenseMtxVec; RelativeError: boolean = False): double; overload; 
    (*<summary>Calculates the C norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(const Vec: TDenseMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload; 


    (*<summary>Calculate the C-norm of calling vector.</summary>
      <returns><c>NormC = max|a[i]|, 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormC: double; overload; 
    (*<summary>Calculates the C norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(Index,Len: integer): double; overload; 

    (*<summary>The L1-norm.</summary>
       <returns>L-1 norm, defined by: <c>||V-Vec||</c>, where V is calling vector.</returns>

       
<remarks>If the NormL1 is called without any parameters, the NormL1 calculates the norm of calling vector. The L1 norm of <c>||V-Vec||</c>
       is defined by the formula:

      <IMG name="TVec12"/><para/>
      If RelativeError is true then the computed norm is divided by the
      norm of V, and the function returns the "relative error":

      <IMG name="TVec09"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            a.SetIt(false, new double[] {4,3,2,1});
            double c = a.NormL1(b,true);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL2"/>*)
    function NormL1(const Vec: TDenseMtxVec; RelativeError: boolean = False): double; overload; 
    (*<summary>Calculates the L1 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(const Vec: TDenseMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload; 

    (*<summary>Calculate the L1-norm of the calling vector.</summary>
     <returns>L-1 norm, defined by the following equation:<para/>
      <c>NormL1 = Sum(|a[i]|), 0 &lt; i &lt; Length-1</c></returns>*)
    function NormL1: double; overload; 
    (*<summary>Calculates the L1 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(Index,Len: integer): double; overload; 


    (*<summary>The L2-norm.</summary>
     <returns>L2 norm, defined by <c>||V-Vec||</c>, where V is calling vector.</returns>
     
<remarks>If the NormL2 is called without any parameters, the NormL2 calculates the
     norm of calling vector. The L2 norm of ||V-Vec|| is defined by the formula:<para/>

     <IMG name="TVec11"/><para/>
     If RelativeError is true then the computed norm is divided by the norm of V, and the function returns the "relative error":<para/>

     <IMG name="TVec09"/><para/>
</remarks>


     

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b;
          MtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            a.SetIt(false, new double[] {4,3,2,1});
            double c = a.NormL2(b,true);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL1"/>*)
    function NormL2(const Vec: TDenseMtxVec; RelativeError: boolean = False): double; overload; 
    (*<summary>Calculates the L2 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(const Vec: TDenseMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload; 

    (*<summary>Calculate the L2-norm of the calling vector.</summary>
     <returns>L2 norm, defined by: <c>NormaL2 = ( Sum(|a[i]|^2) )^0.5  , 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormL2: double; overload;  
    (*<summary>Calculates the L2 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(Index,Len: integer): double;  overload; 

    (*<summary>Add/Subtract a value.</summary>
      
<remarks>Depreciated
        Use <see cref="Add"/> (Value) instead.
</remarks>
*)
    function Offset(const Value: double): TMtxVec; overload; 
    (*<summary>Add/Subtract a complex value.</summary>*)
    function Offset(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Add/Subtract a value from [Index]..[Index+Len-1].</summary>*)
    function Offset(const Value: double; Index,Len: integer): TMtxVec; overload; 
    (*<summary>Add/Subtract a complex value from [Index]..[Index+Len-1].</summary>*)
    function Offset(const Value: TCplx; Index,Len: integer): TMtxVec; overload; 

    (*<summary>Calculate the range from all calling object elements.</summary>*)
    function Range: double; overload; 
    (*<summary>Returns real range value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Range(Index, Len: integer): double; overload; 
    (*<summary>Calculate the range from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result ARange is a real value. An exception is raised if calling object <see cref="TMtxVec.Complex"/> property is true or array borders are overrun/underrun.
</remarks>
*)
    procedure Range(out ARange: double; Index, Len: integer); overload; 

    (*<summary>Reverse vector elements.</summary>
      
<remarks>The method reverses Vec vector elements from [VecIndex].. [VecIndex+Len-1]
      and stores them in the calling vector from [Index]...[Index+Len-1]
      by using the following equation:<para/>

      <IMG name="TVec24"/><para/>

      This overload reverses calling vector elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4);
            a.Reverse();   // a = [4,3,2,1]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(Index, Len: integer): TMtxVec; overload;
    (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on vector elements in specified range [Index..Index+Len].
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Rotate(Offset: integer; Index: integer; Len: integer = MtxVecEOA): TMtxVec; overload; 

    (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>

      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Rotate(const Vec: TMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TMtxVec; overload; 

    (*<summary>Shift vector elements in range.</summary>
      
<remarks>Shifts calling vector elements in specified range [Index..Index+Len].
      The number of elements by which to shift is specified in the Offset parameter.

      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Rotate"/>*)
    function Shift(Offset: integer; Index: integer; Len: integer = MtxVecEOA): TMtxVec; overload; 

    (*<summary>A shift on vector elements in range.</summary>
     
<remarks>Performs shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Shift(const Vec: TMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TMtxVec; overload; 

    (*<summary>Subtracts Value from object elements.</summary>
      
<remarks>Subtracts Value from all calling object elements.
</remarks>


      <SeeAlso cref="Add"/>*)
    function Sub(const Value: double): TMtxVec; overload; 
    (*<summary>Subtracts complex Value from all calling object complex elements.</summary>*)
    function Sub(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Subtracts Value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: double; Index, Len: integer): TMtxVec; overload; 
    (*<summary>Subtracts complex Value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: TCplx; Index, Len: integer): TMtxVec; overload; 

    (*<summary>Subtract real Value from Src.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TMtxVec; const Value: double): TMtxVec; overload;
    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TMtxVec; const Value: TCplx): TMtxVec; overload;
    (*<summary>Subtract real Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TMtxVec.Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TMtxVec; const Value: double; SrcIndex, Index, Len: integer): TMtxVec; overload;
    (*<summary>Subtract complex Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TMtxVec.Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TMtxVec; const Value: TCplx; SrcIndex, Index, Len: integer): TMtxVec; overload;

    (*<summary>Array subtraction.</summary>
      
<remarks>Subtract each of Vec elements from corresponding elements in the calling object.
      An exception is raised if Vec and calling object size and <see cref="TMtxVec.Complex"/> properties do not match.
</remarks>

      <SeeAlso cref="Add"/>*)
    function Sub(const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Subtract Vec2 elements from Vec1 elements.</summary>
      
<remarks>Stores the results in calling object. Size and <see cref="TMtxVec.Complex"/> property of calling object are
      adjusted automatically. An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVec.Complex"/> property do not match.
</remarks>
*)
    function Sub(const Vec1, Vec2: TMtxVec): TMtxVec; overload; 
    (*<summary>Subtract Vec elements [VecIndex]..[VecIndex+Len-1] from corresponding calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Subtract Vec22 elements [Vec2Index]..[Vec2Index+Len-1] from Vec1 object elements [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>Stores the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVec.Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec1, Vec2: TMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Subtraction from value.</summary>
      
<remarks>Subtract each of calling object elements from Value.
</remarks>


      <SeeAlso cref="Add"/>
      <SeeAlso cref="Sub"/>*)
    function SubFrom(const Value: double): TMtxVec; overload; 
    (*<summary>Subtract each of calling object elements from complex Value.</summary>
      
<remarks>If the calling vector s not complex, the conversion is performed automatically in a
      performance efficient way.
</remarks>
*)
    function SubFrom(const Value: TCplx): TMtxVec; overload; 
    (*<summary>Subtract elements [Index]..[Index+Len-1] from Value.</summary>
      
<remarks>Store the result in calling vector. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is True and array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(const Value: double; Index,Len: integer): TMtxVec; overload; 
    (*<summary>Subtract elements [Index]..[Index+Len-1] from complex Value.</summary>
      
<remarks>Store the result in calling object. If the calling vector is not complex, the conversion to complex is performed
      automatically in performance efficient way. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
       and array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(const Value: TCplx; Index,Len: integer): TMtxVec; overload; 
    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="TMtxVec.Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Substract complex Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size property of the calling object is set
      automatically. <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; const Vec: TMtxVec): TMtxVec; overload; 
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from Value.</summary>
      
<remarks>Stores the result to the calling object elements [Index]..[Index+Len-1].
       Size property of the calling object is not changed. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
       <see cref="TMtxVec.Complex"/> property of the calling object is adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from complex Value and store the result to the
       calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size property of the calling object is not changed. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
       <see cref="TMtxVec.Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; const Vec: TMtxVec; VecIndex, Index, Len: integer): TMtxVec; overload; 

    (*<summary>Calculate the Sum of squares of the calling vector.</summary>
       
<remarks>For complex numbers, the routine computes the squared L2 norm.

     <c>SumOfSquares = Sum(|a[i]|^2)   , 0 &lt; i &lt; Length-1.</c>
</remarks>


     <SeeAlso cref="NormL2"/>*)
    function SumOfSquares: double; overload;  
    (*<summary>Calculates the sum of squares from the calling vector elements [Index]..[Index+Len-1].</summary>*)
    function SumOfSquares(Index,Len: integer): double;  overload; 

    (*<summary>Inserts zeroes between consecutive array values.</summary>
      
<remarks>Copy Len values from Src starting at SrcIndex to the calling object starting at position Index and place Factor-1 zeros
      between consecutive values. Size and <see cref="TMtxVec.Complex"/> properties of the calling object must be set
      explicitly. Phase parameter defines the initial sample offset and must be less then Factor. An exception is raised,
      if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="DownSample"/>*)
    function UpSample(const Src: TMtxVec; Factor,SrcIndex, Index, Len: integer; Phase: integer = 0): TMtxVec; overload; 

    (*<summary>Skewness (third central momentum).</summary>
      
<remarks>Calculate the calling object skewness by using mean value AMean and standard deviation AStdDev.
      Skewness is the third central moment, divided by third power of standard deviation:

      <IMG name="TVec07"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4);
            double d = a.Mean();
            double c = a.Skewness(d.a.StdDev(d));
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Kurtosis"/>
      <SeeAlso cref="Mean"/>
      <SeeAlso cref="StdDev"/>*)
    function Skewness(const AMean, AStdDev: double): double; overload; 
    (*<summary>Calculate the skewness for elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun.
      The AMean and AStdDev parameters must be computed from the same elements from which
      the Skewness is to be computed.
</remarks>
*)
    function Skewness(const AMean, AStdDev: double; Index, Len: integer): double; overload; 

end;


  procedure VecFunc (const Src,Dst: TMtxVec; SrcIdx,DstIdx: integer; Len: integer;
                     DoubleFunc: TDoubleFunc; ComplexFunc: TComplexFunc;
                     SingleFunc: TSingleFunc; SComplexFunc: TSComplexFunc);


  procedure EAbsIsNotAllowedOnComplexSubrange;


 (*<summary> Returns true if A &gt;= B for all elements. </summary>*)
  function GreaterThanOrEqual(const a, b: TMtxVec): boolean; overload;
 (*<summary> Returns true if A &lt;= B for all elements. </summary>*)
  function LessThanOrEqual(const a, b: TMtxVec): boolean; overload;
 (*<summary> Returns true if ALeft &lt; B for all elements. </summary>*)
  function LessThan(const ALeft: TCplx; const a: TMtxVec): Boolean;  overload;
 (*<summary> Returns true if A &lt; ARight for all elements. </summary>*)
  function LessThan(const a: TMtxVec; const ARight: TCplx): Boolean; overload;
 (*<summary> Returns true if A &lt; ARight for all elements. </summary>*)
  function LessThan(const a: TMtxVec; const ARight: double): Boolean; overload;
 (*<summary> Returns true if A &lt; ARight for all elements. </summary>*)
  function LessThan(const a, b: TMtxVec): Boolean; overload;
 (*<summary> Returns true if ALeft &lt; A for all elements. </summary>*)
  function LessThan(const ALeft: double; a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if ALeft &lt;= A for all elements. </summary>*)
  function LessThanOrEqual(const ALeft: double; a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if A &lt;= ARight for all elements. </summary>*)
  function LessThanOrEqual(const a: TMtxVec; const ARight: double): Boolean; overload;
 (*<summary> Returns true if A &lt;= ARight for all elements. </summary>*)
  function LessThanOrEqual(const a: TMtxVec; const ARight: TCplx): Boolean; overload;
 (*<summary> Returns true if ALeft &lt;= A for all elements. </summary>*)
  function LessThanOrEqual(const ALeft: TCplx; const a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if ALeft &gt; A for all elements. </summary>*)
  function GreaterThan(const ALeft: TCplx; const a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if A &gt; ARight for all elements. </summary>*)
  function GreaterThan(const a: TMtxVec; const ARight: TCplx): Boolean; overload;
 (*<summary> Returns true if A &gt; ARight for all elements. </summary>*)
  function GreaterThan(const a: TMtxVec; const ARight: double): Boolean; overload;
 (*<summary> Returns true if A &gt; B for all elements. </summary>*)
  function GreaterThan(const a, b: TMtxVec): Boolean; overload;
 (*<summary> Returns true if ALeft &gt; A for all elements. </summary>*)
  function GreaterThan(const ALeft: double; const a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if ALeft &gt;= A for all elements. </summary>*)
  function GreaterThanOrEqual(const ALeft: double; const a: TMtxVec): Boolean; overload;
 (*<summary> Returns true if A &gt;= ARight for all elements. </summary>*)
  function GreaterThanOrEqual(const a: TMtxVec; const ARight: double): Boolean; overload;
 (*<summary> Returns true if A &gt;= ARight for all elements. </summary>*)
  function GreaterThanOrEqual(const a: TMtxVec; const ARight: TCplx): Boolean; overload;
 (*<summary> Returns true if ALeft &gt;= A for all elements. </summary>*)
  function GreaterThanOrEqual(const ALeft: TCplx; const a: TMtxVec): Boolean; overload;

  procedure FindMask(const Dst: TMtxVecInt; const a: TMtxVec; const op: TMtxCompareOp; const b: TMtxVec); overload;
  procedure FindMask(const Dst: TMtxVecInt; const a: TMtxVec; const op: TMtxCompareOp; const b: TCplx); overload;
  procedure FindMask(const Dst: TMtxVecInt; const a: TMtxVec; const op: TMtxCompareOp; const b: double); overload;

  procedure FindAndSplit(const Dst: TMtxVecInt; const a: TMtxVec; const op: string; const b: TMtxVec; const MaskVec, NotMaskVec: TMtxVec); overload;
  procedure FindAndSplit(const Dst: TMtxVecInt; const a: TMtxVec; const op: string; const b: double; const MaskVec, NotMaskVec: TMtxVec); overload;
  procedure FindAndSplit(const Dst: TMtxVecInt; const a: TMtxVec; const op: string; const b: TCplx; const MaskVec, NotMaskVec: TMtxVec); overload;

  function FindIndexesAndLength(const Dst: TVecInt; const a: TMtxVec; const op: string; const b: TCplx): integer; overload;
  function FindIndexesAndLength(const Dst: TVecInt; const a: TMtxVec;const  op: string; const b: double): integer; overload;
  function FindIndexesAndLength(const Dst: TVecInt; const a: TMtxVec; const op: string; const b: TMtxVec): integer; overload;

  function aAbs(const X: double): double; overload; 
  function aAbs(const X: single): single; overload; 
  function aAbs(const X: integer): integer; overload; 


