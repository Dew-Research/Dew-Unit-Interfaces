










(*<summary>Implements abstract TMtxVecInt object.</summary>
  
<remarks>Implements abstract integer math object TMtxVecInt and supporting routines <see cref="TMtxVecInt"/>
</remarks>
*)
unit AbstractMtxVecInt;


{$I BdsppDefs.inc}

interface

uses Math387, MtxVecBase
    
    ,Classes
    ,SysUtils
    
    
    
    ;



type

   

  (*<summary> Abstract class for TVecInt and TMtxInt. </summary>*)
  TMtxVecInt = class(TMtxVecBase)
  
  protected
    FIDataOffset: integer;
    FBDataOffset: integer;
    FSDataOffset: integer;
  strict protected
    SIDataOffset: integer;
    SBDataOffset: integer;
    SSDataOffset: integer;

    

    SValuesPointer: PointerInteger;

    

    SIntPrecision: TIntPrecision;

    FBitCount: Int64;
    FScaleFactor: integer;
    FIntPrecision: TIntPrecision;
    FIntPrecisionLock: boolean;

    procedure SubRangeLoadState; override;

    procedure SetIntPrecision(const Value: TIntPrecision); virtual;
    procedure SetScaleFactor(const Value: integer);
    procedure SetIntPrecisionLock(const Value: boolean);

    function ElemSize: Integer; override;
    procedure HookPointers; override;
    procedure ReleaseMemory; override;
    procedure AllocMemory; override;
    procedure AllocCacheMemory; override;

    function Int32CachePointer: TIntegerArray; virtual;
    function Int16CachePointer: TSmallIntArray; virtual;
    function Int8CachePointer: Math387.TByteArray; virtual;
    function Int32CacheOffset: integer; virtual;
    function Int16CacheOffset: integer; virtual;
    function Int8CacheOffset: integer; virtual;

    procedure SetCapacityInElements(const value: Int64); override;
    function GetCapacityInElements: Int64; override;

    procedure CopyFromArray(const Src: TIntegerArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override; 
    procedure CopyFromArray(const Src: TSmallIntArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override; 
    procedure CopyFromArray(const Src: Math387.TWordArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; 
    procedure CopyFromArray(const Src: Math387.TByteArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; override;

    procedure CopyFromArray(const Src: TCplxArray; const Dst: TMtxVecInt; Rounding: TRounding; const SrcIdx, DstIdx, Len: integer); overload;
    procedure CopyFromArray(const Src: TDoubleArray; const Dst: TMtxVecInt; Rounding: TRounding; const SrcIdx, DstIdx, Len: integer); overload;
    procedure CopyFromArray(const Src: TSingleArray; const Dst: TMtxVecInt; Rounding: TRounding; const SrcIdx, DstIdx, Len: integer); overload;

    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TDoubleArray; const Index,DstIdx,Len: integer); overload; override; 
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TSingleArray; const Index,DstIdx,Len: integer); overload; override; 
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TIntegerArray; const Index,DstIdx,Len: integer); overload;  

    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TCplxArray; const Index,DstIdx,Len: integer); overload;
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TSCplxArray; const Index,DstIdx,Len: integer); overload;

    procedure CopyToArrayInternal(const Src: TMtxVecInt; const Dst: TSmallIntArray; const Index,DstIdx,Len: integer); overload;
    procedure CopyToArrayInternal(const Src: TMtxVecInt; const Dst: Math387.TWordArray; const Index,DstIdx,Len: integer); overload;
    procedure CopyToArrayInternal(const Src: TMtxVecInt; const Dst: Math387.TByteArray; const Index,DstIdx,Len: integer); overload;


    procedure Add(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure Add(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure AddValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure AddValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;

    procedure BinaryAnd(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure BinaryAnd(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure BinaryAndValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure BinaryAndValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt;  SrcIndex, Index, Len: integer); overload;

    procedure BinaryOr(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure BinaryOr(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure BinaryOrValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure BinaryOrValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt;  SrcIndex, Index, Len: integer); overload;

    procedure BinaryXOr(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure BinaryXOr(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure BinaryXOrValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure BinaryXOrValue(const Src: TMtxVecInt; Value: integer; const Dst: TMtxVecInt;  SrcIndex, Index, Len: integer); overload;

    procedure BinaryNot(const SrcDst: TMtxVecInt; Index, Len: integer); overload;
    procedure BinaryNot(const Src, Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;

    procedure Abs(const SrcDst: TMtxVecInt; Index, Len: integer); overload;
    procedure Abs(const Src, Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;

    procedure Subtract(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure Subtract(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure SubtractValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure SubtractValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure SubtractFromValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure SubtractFromValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt;   SrcIndex, Index, Len: integer); overload;

    procedure Multiply(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure Multiply(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure MultiplyValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure MultiplyValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;

    procedure Divide(const Src1, Src2, Dst: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer); overload;
    procedure Divide(const Src, SrcDst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure DivideValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure DivideValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;
    procedure DivideByValue(const Src: TMtxVecInt; Value: integer; Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure DivideByValue(const SrcDst: TMtxVecInt; Value, Index, Len: integer); overload;

    procedure BitShift(const Src: TMtxVecInt; Bits: integer; const Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure BitShift(const SrcDst: TMtxVecInt; Bits, Index, Len: integer); overload;

    procedure Copy(const Src, Dst: TMtxVecInt; SrcIndex, Index, Len: integer); overload;
    procedure Convert(const Src, Dst: TMtxVecInt; DstPrecision: TIntPrecision; SrcIndex, Index, Len: integer); overload;

    procedure Max1(var aIndex, aMax: integer; Index, Len: integer); overload;
    procedure Max1(var aMax: integer; Index, Len: integer); overload;

    procedure MaxMin1(out maxIndex, minIndex, aMax, aMin: integer; Index, Len: integer); overload;
    procedure MaxMin1(out aMax, aMin: integer; Index, Len: integer); overload;

    procedure Min1(var aIndex, aMin: integer; Index, Len: integer); overload;
    procedure Min1(var aMin: integer; Index, Len: integer); overload;

    procedure Move1(Offset, Index, Len: integer); overload;

    procedure Median1(const Src, Dst: TMtxVecInt; MaskSize, SrcIndex, Index,  Len: integer); overload;
    procedure Median1(const SrcDst: TMtxVecInt; MaskSize, Index, Len: integer); overload;

    procedure Sum(out aSum: integer; Index, Len: integer); overload;
    procedure SetVal1(Value, Index, Len: integer); overload;
    procedure SetZero1(Index, Len: integer); overload;

    procedure SortAscend1(Index, Len: integer); overload;
    procedure SortAscend1(const SortIdx: TMtxVecInt; SortIdxIndex, Index, Len: integer); overload;

    procedure SortDescend1(Index, Len: integer); overload;
    procedure SortDescend1(const SortIdx: TMtxVecInt; SortIdxIndex, Index, Len: integer); overload;

  
  strict private
  
    function get_First: integer;
    function get_Last: integer;


  public
    IData: TIntegerArray;
    SData: TSmallIntArray;
    BData: Math387.TByteArray;

    

    (*<summary>Allows setting/getting the 32bit integer value at position Indx.</summary>*)
    IValues: Math387.TIntegerArray;
    IValues1D: Math387.TIntegerArray;
    (*<summary>Allows setting/getting the 16bit integer value at position Indx.</summary>*)
    SValues: Math387.TSmallIntArray;
    SValues1D: Math387.TSmallIntArray;
    (*<summary>Allows setting/getting the 8bit unsigned integer value at position Indx.</summary>*)
    BValues: Math387.TByteArray;
    BValues1D: Math387.TByteArray;

    

    

    constructor Create; override;
    destructor Destroy; override;

    
    procedure ValidateParams2 (const X: TMtxVecBase; const XIndex: integer;const Index: integer; var Len: integer); overload;
    procedure ValidateParams (const X: TMtxVecBase; const XIndex: integer;const Index: integer; var Len: integer); overload;
    procedure ValidateParams (const X1,X2: TMtxVecBase; const xIndex1,xIndex2,Index: integer; var Len: integer); overload;
    procedure ValidateParams(const X1, X2, X3: TMtxVecBase; const xIndex1, xIndex2, xIndex3, Index: integer; var Len: integer); overload;

    function DataIndex(Value: integer; aPrecision: TIntPrecision): integer; overload; 
    function DataIndex(Value: integer): integer; overload; 
    

    
    (*<summary>Save the list to a stream.</summary>*)
    procedure SaveToStream(const DstStream: TStream); overload; override;
    (*<summary>Load the list from a stream.</summary>*)
    function LoadFromStream(const SrcStream: TStream): Int64; overload;  override;
    

    

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
            TVecInt a;
            MtxVecInt.CreateIt(out a);
            try
            {
              a.SetIt(new int[] {1,-2,3,4});
              a.Abs(); // a = [1,2,3,4]
            }
            finally
            {
              MtxVec.FreeIt(ref a);
            }
          }
        }
        </code></example>*)
     function Abs(): TMtxVecInt; overload;
     (*<summary>Absolute values of calling object elements [Index]..[Index+Len-1].</summary>
      <returns>calling vector elements [Index]..[Index+Len-1] ansolute vslues.</returns>
      
<remarks>An exception is raised if array borders are overrun/underrun.

      Note
        Please note the calling vector elements [Index]..[Index+Len-1] are overwritten
        with result of routine call.
</remarks>
*)
     function Abs(Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Absolute values of all Src vector elements.</summary>
      <returns>Src vector elements ansolute vslues.</returns>*)
     function Abs(const Src: TMtxVecInt): TMtxVecInt; overload;
     (*<summary>Absolute values of Src vector elements [VecIndex]..[VecIndex+Len-1].</summary>
      <returns>Src vector elements [VecIndex]..[VecIndex+Len-1] absolute values
           in calling vector elements [Index]..[Index+Len-1].</returns>
      
<remarks>An exception is raised if array borders are overrun/underrun.

      Note
        Please note the calling vector elements [Index]..[Index+Len-1] are overwritten
        with result of routine call.
</remarks>
*)
     function Abs(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary> Converts Src to packed bit storage. </summary>
       
<remarks>Returns the result of bit packing the Src values [SrcIndex]..[SrcIndex+Len-1]
                and stored in the calling object  bits from [0]..[0+Len-1].
                If Len is not divisable with 32, the remaining bits in the last sample are left at 0.
                The storage precision of the calling object is set to prInt32.
                The size of the calling object is adjusted automatically.
</remarks>
*)
     function BitPack(const Src: TMtxVecInt; const SrcIndex, Index: integer; Len: integer): TMtxVecInt; overload;

     (*<summary> Converts Src to unpacked bit storage. </summary>
                 
<remarks>If bit at Src[i] &lt;&gt; 0 then Integer (32, 16 or 8bit) at index "i" in the calling vector is set to 1.
       Returns the result of bit unpacking the bits stored in Src values [SrcIndex]..[SrcIndex + Len - 1]
                and stored in the calling object (Self) values [Index]..[Index+Len-1].
                The storage precision of the calling object is preserved. The Len parameter specifies the number
                of bits that will be unpacked from Src.
</remarks>
*)
     function BitUnpack(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary "and" between coresponding elements in Src1 and Src2.</summary>
       
<remarks>Returns the result of binary "and" of Src1 values with coresponding Src2 values
                stored in the calling object.
</remarks>
*)
     function BinaryAnd(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
     (*<summary> Apply binary "and" between coresponding elements in Src1 and Src2. </summary>
       <returns> the result of binary "and" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
       with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
       object at locations [Index]..[Index+Len-1]. </returns>*)
     function BinaryAnd(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "and"  of Src values with coresponding values in Self (this).</returns>*)
     function BinaryAnd(const Src: TMtxVecInt): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "and" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
       values in Self (this) [Index]..[Index+Len-1].</returns>*)
     function BinaryAnd(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between elements in Src and Value.</summary>
       <returns>the result of binary "and" of Value with coresponding
       values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
       at locations [Index]..[Index+Len-1].</returns>*)
     function BinaryAnd(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
       <returns>the result of binary "and" between Value and
       values in the calling object.</returns>*)
     function BinaryAnd(Value: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
       <returns>the result of binary "and" between Value and
       values in the calling object at locations [Index]..[Index+Len-1].</returns>*)
     function BinaryAnd(Value, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "and" between elements in Src and Value.</summary>
       <returns>the result of binary "and" of Value with coresponding
       values in Src stored in the calling object.</returns>*)
     function BinaryAnd(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "or" between coresponding elements in Src1 and Src2.</summary>
       <returns>the result of binary "and"  of Src1 values with coresponding Src2 values stored
                in the calling object.</returns>*)
     function BinaryOr(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
     (*<summary> Apply binary "or" between coresponding elements in Src1 and Src2. </summary>
       <returns> the result of binary "or" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
       with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
       object at locations [Index]..[Index+Len-1]. </returns>*)
     function BinaryOr(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "or" of Src values with coresponding values in Self (this).</returns>*)
     function BinaryOr(const Src: TMtxVecInt): TMtxVecInt; overload;
     (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "or" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
       values in Self (this) [Index]..[Index+Len-1].</returns>*)
     function BinaryOr(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "or" between elements in Src and Value.</summary>
       <returns>the result of binary "or" of Value with coresponding
       values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
       at locations [Index]..[Index+Len-1].</returns>*)
     function BinaryOr(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary "or" between elements in Src and Value.</summary>
       <returns>the result of binary "or" of Value with coresponding
       values in Src stored in the calling object.</returns>*)
     function BinaryOr(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;

     (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
       <returns>the result of binary "or" between Value and
       values in the calling object at locations [Index]..[Index+Len-1] stored
       back in the calling object.</returns>*)
     function BinaryOr(Value, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
       <returns>the result of binary "or" between Value and
       values in the calling object.</returns>*)
     function BinaryOr(Value: integer): TMtxVecInt; overload;

     (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
       <returns>the result of binary "xor" between Value and  values in the calling object.</returns>*)
     function BinaryXor(Value: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
       <returns>the result of binary "xor" between Value and
       values in the calling object at locations [Index]..[Index+Len-1] stored
       back in the calling object.</returns>*)
     function BinaryXor(Value, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
       <returns>the result of binary "xor" of Value with coresponding
       values in Src stored in the calling object.</returns>*)
     function BinaryXor(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
       <returns>the result of binary "xor" of Value with coresponding
       values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
       at locations [Index]..[Index+Len-1].</returns>*)
     function BinaryXor(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary "xor" between coresponding elements in Src1 and Src2.</summary>
       <returns>the result of binary "xor" of Src1 values with coresponding Src2 values stored
                in the calling object.</returns>*)
     function BinaryXor(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
     (*<summary> Apply binary "xor" between coresponding elements in Src1 and Src2. </summary>
       <returns> the result of binary "xor" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
       with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
       object at locations [Index]..[Index+Len-1]. </returns>*)
     function BinaryXor(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "xor" of Src values with coresponding values in Self (this).</returns>*)
     function BinaryXor(const Src: TMtxVecInt): TMtxVecInt; overload;

     (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
       <returns>the result of binary "xor" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
       values in Self (this) [Index]..[Index+Len-1] stored back in the calling object.</returns>*)
     function BinaryXor(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left for all elements in the calling object.</summary>
       <returns>the result of binary shift to left by number of Bits applied to values in the calling object
                and stored back in the calling object.</returns>*)
     function BitShiftLeft(Bits: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left to elements in the calling object.</summary>
       <returns>the result of binary shift to left by number of Bits applied to values [Index]..[Index+Len-1]
                in the calling object and stored back in the calling object.</returns>*)
     function BitShiftLeft(Bits, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
       <returns>the result of binary shift to left by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
                and stored in the calling object (Self) value [Index]..[Index+Len-1].</returns>*)
     function BitShiftLeft(const Src: TMtxVecInt; Bits: integer; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
       <returns>the result of binary shift to left by number of Bits applied to Src values
                and stored in the calling object (Self).</returns>*)
     function BitShiftLeft(const Src: TMtxVecInt; Bits: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the right for all elements in the calling object.</summary>
       <returns>the result of binary shift to right by number of Bits applied to values in the calling object
                and stored back in the calling object. The shift preserves the sign of the numbers.</returns>*)
     function BitShiftRight(Bits: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the right to elements in the calling object.</summary>
       <returns>the result of binary shift to right by number of Bits applied to values [Index]..[Index+Len-1]
                in the calling object and stored back in the calling object. The shift
                preserves the sign of the numbers. </returns>*)
     function BitShiftRight(Bits, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
       <returns>the result of binary shift to right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
                and stored in the calling object (Self) value [Index]..[Index+Len-1].
                The shift preserves the sign of the numbers. </returns>*)
     function BitShiftRight(const Src: TMtxVecInt; Bits: integer; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
     (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
       <returns>the result of binary shift to right by number of Bits applied to Src values
                and stored in the calling object (Self). The shift preserves the sign of the numbers.
                </returns>*)
     function BitShiftRight(const Src: TMtxVecInt; Bits: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) for all elements in the calling object.</summary>
       <returns>the result of binary shift to the left or right by number of Bits applied to values in the calling object
                and stored back in the calling object.
                The shift preserves the sign of the numbers.</returns>*)
     function BitShift(Bits: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in the calling object.</summary>
       <returns>the result of binary shift to the left or right by number of Bits applied to values [Index]..[Index+Len-1]
                in the calling object and stored back in the calling object.
                The sign of the numbers is preserved when doing right shift.
                </returns>*)
     function BitShift(Bits, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
       <returns>the result of binary shift to the left or right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
                and stored in the calling object (Self) value [Index]..[Index+Len-1].
                The sign of the numbers is preserved when doing right shift.
                </returns>*)
     function BitShift(const Src: TMtxVecInt; Bits: integer; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
       <returns>the result of binary shift to the left or right by number of Bits applied to Src values
                and stored in the calling object (Self). The sign of the numbers is preserved when doing right shift.
                </returns>*)
     function BitShift(const Src: TMtxVecInt; Bits: integer): TMtxVecInt; overload;

   (*<summary>Applies binary "not" operation to elements stored in the object.</summary>
      
<remarks>Calculate the binary "not" value of all calling object elemets in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,0,1,0});
            a.BinaryNot(); // a = [-2,-1,-2,-1]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function BinaryNot(): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to elements stored in Self (this).</summary>
     <returns>the result of binary "not" of values stored in Self (this).</returns>*)
   function BinaryNot(Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values stored in Self (this).</returns>*)
   function BinaryNot(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values [SrcIndex]..[SrcIndex+Len-1] stored in Self (this) values [Index]..[Index+Len-1].</returns>*)
   function BinaryNot(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;


   (*<summary>Applies logical "not" operation to elements stored in the object.</summary>
              
<remarks>Calculate the logical "not" value of all calling object elemets in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,0,1,0});
            a.LogicalNot(); // a = [0,1,0,1]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function LogicalNot(): TMtxVecInt; overload;
   (*<summary>Apply logical "not" to elements stored in Self (this).</summary>
     <returns>the result of logical "not" of values stored in Self (this).</returns>*)
   function LogicalNot(Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply logical "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of logical "not" of Src values stored in Self (this).</returns>*)
   function LogicalNot(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply logical "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of logical "not" of Src values [SrcIndex]..[SrcIndex+Len-1] stored in Self (this) values [Index]..[Index+Len-1].</returns>*)
   function LogicalNot(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;



    (*<summary>Finds a match for X in object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
</remarks>

      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>*)

    function BinarySearch(const X: Integer): Integer; overload;
    function BinarySearch(const X: Integer; const Index: integer; Len: integer): Integer; overload;

    (*<summary>Finds exact or closest index match for X in object values using binary search.</summary>
				<returns>True, if found and the index of the next bigger or smaller value in XIndex, if not found. </returns>
               
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
*)

    function BinarySearch(const X: Integer; var XIndex: integer): boolean; overload;
    function BinarySearch(const X: Integer; var XIndex: integer; const Index: integer; Len: integer): boolean; overload;


    (*<summary>Returns number of values within the interval.</summary>
               
<remarks>Returns number of values which fullfill condition LowValue &lt; Value[i] &lt; HighValue.
</remarks>
*)

    function CountInRange(const LowValue, HighValue: integer): integer; overload;
    (*<summary>Returns number of values within the interval.</summary>
               
<remarks>Returns number of values which fullfill condition LowValue &lt; Value[i] &lt; HighValue,
               for i traversing the range [Index]..[Index+Len-1]
</remarks>
*)
    function CountInRange(const LowValue, HighValue: integer; const Index: integer; Len: integer): integer; overload;


    (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The precision is preserved.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TWordArray): TMtxVecInt; overload;
    (*<summary>Copies values from a 2 byte unsigned integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TWordArray; SrcIndex, Index, Len: integer): TMtxVecInt; overload;


   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+2*Len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; Rounding: TRounding; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 


   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; Rounding: TRounding; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a real single precision floating point Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the single precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; Rounding: TRounding; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from an integer type Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 4 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to the calling object values [Index]..[Index+len-1]. The size of the calling object is not changed.
      If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from an 2 byte signed integer type Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 2 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray; SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts values to
               to 2 byte signed integer numbers.</summary>*)
    function CopyToArray(var Dst: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       2 byte signed integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to 2 byte signed integers. Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TSmallIntArray; DstIndex, Index,Len: integer): TMtxVecInt; overload; 

























    (*<summary>Sizes the array.</summary>
      
<remarks>Sizes the Dst array so that it can hold all the values stored in the calling
      object.
</remarks>
*)
    procedure SizeToArray(var Dst: TCplxArray); overload; 

    (*<summary>Sizes the array.</summary>
      
<remarks>Sizes the Dst array so that it can hold all the values stored in the calling
      object.
</remarks>
*)
    procedure SizeToArray(var Dst: TSCplxArray); overload; 


    (*<summary>Copies values from the calling object to the Dst array and converts data
         to complex double precision numbers.</summary>
         
<remarks>Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex + Len/2 - 1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to complex double precision floating point values. Consecutive elements become real and imaginary parts.
       Index and Len apply to the integer source array and DstIndex applies to the destination complex array.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray; DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
         to complex double precision numbers.</summary>
         
<remarks>Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TSCplxArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex + Len/2 - 1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to complex double precision floating point values. Consecutive elements become real and imaginary parts.
       Index and Len apply to the integer source array and DstIndex applies to the destination complex array.
</remarks>
*)
    function CopyToArray(var Dst: TSCplxArray; DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 4 byte signed integer numbers.</summary>*)
    function CopyToArray(var Dst: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to 4 byte signed integers. Values exceeding the range of a 4 byte signed integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TIntegerArray; DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 2 byte unsigned integer numbers.</summary>
       
<remarks>Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TWordArray): TMtxVecInt; overload; 
    (*<summary>Copy integer values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to 2 byte unsigned integers. Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TWordArray; DstIndex, Index,Len: integer): TMtxVecInt; overload; 


    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 1 byte unsigned integer numbers.</summary>
       
<remarks>Values exceeding the range of a 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray): TMtxVecInt; overload; 

    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       The method converts calling object values to 1 byte unsigned integers.
       Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray; DstIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Size the object.</summary>
      
<remarks>Assignes the size of the Src object to the calling object.
      If the calling object is a TVecInt object then the Src can be of any type,
      otherwise TMtxInt can only obtain size from a TMtxInt object and TSparseMtx
      can only obtain size from a TSparseMtx object.

      If the calling object and Src are of different types and both objects have a
      matching <see cref="TMtxVecBase.Length">Length</see> property only the IntPrecision
      property of the calling object will changed, while all other properties describing
      the size of the object (rows, cols, length) will be preserved.
</remarks>
*)
    function Size(const Src: TMtxVecBase): TMtxVecInt ; overload; virtual; 

    (*<summary>Allows the IntPrecision property of the calling object to become
               of aPrecision value instead of Src.IntPrecision value.</summary>

       
<remarks>It is also possible to pass the calling object as the Src with a different IntPrecision value.
       The value of the IntPrecision property can be changed without knowing the actual type of the object.
</remarks>
*)
    function Size(const Src: TMtxVecBase; aPrecision: TIntPrecision): TMtxVecInt ; overload; virtual;

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
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(true,new double[] {1,2,3,4});
            a.CumSum();  // a = [1,3,6,10]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sum"/>*)
    function CumSum(Index, Len: Integer): TMtxVecInt; overload; 
    (*<summary>Calculate the cumulative sum for Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="IntPrecision"/> properties of the
      calling object must be set explicitly. Exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>  property is True
      and array borders are overrun.
</remarks>
*)
    function CumSum(const Vec: TMtxVecInt; VecIndex,Index,Len: Integer): TMtxVecInt; overload; 


    (*<summary>Finds a match for X in object values.</summary>
      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>
      
<remarks>Compare real value X with all calling object elements.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {2,5,1,6});
            int indes = a.Find(1);  // returns 2 (the arrays are zero based)
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)

   function Find(const X: integer): integer; overload;
   function Find(const X: integer; const Index: integer; Len: integer): integer; overload;


   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
     <returns>the result of adding Src2 values from coresponding Src1 values.</returns>*)
   function Add(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
    
<remarks>Add Src1 elements [Src1Index..Src1Index+Len-1] to corrresponding Src2 elements [Src2Index..Src12ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add Src elements to calling vector elements.</summary>*)
   function Add(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements.</summary>
    
<remarks>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to all Src elements.</summary>
      
<remarks>Add integer Value to all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Add(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to all calling vector elements.</summary>*)
   function Add(Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract coresponding elements in Src2 from Src1.</summary>
     <returns>the result of subtracting Src2 values from coresponding Src1 values.</returns>*)
   function Subtract(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src1 from Src1.</summary>
    
<remarks>Subtract Src2  elements [Src2Index..Src2Index+Len-1] from corrresponding Src1 elements [Src1Index..Src1ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src elements from calling vector elements.</summary>*)
   function Subtract(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements.</summary>
    
<remarks>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from all Src elements.</summary>
      
<remarks>Subtract integer Value from all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from all calling vector elements.</summary>*)
   function Subtract(Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract all calling vector elements from integer Value.</summary>*)
   function SubtractFrom(Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract calling vector elements [Index..Index+Len-1] from integer Value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src vector elements from integer Value.</summary>
    
<remarks>and store thje results in calling vector. Size of calling vector is adjusted automatically.
</remarks>
*)
   function SubtractFrom(Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src vector elements [SrcIndex..SrcIndex+Len-1] from integer Value.</summary>
      
<remarks>and store thje results in calling vector elements [Index..Index+Len-1]. Size of calling vector is adjusted automatically.
      An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(Value: integer; const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Multiplies coresponding elements in Src2 with Src1.</summary>
     <returns>the result of multipliing Src2 values with coresponding Src1 values.</returns>*)
   function Multiply(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Multiply coresponding elements in Src2 with Src1.</summary>
    
<remarks>Multiply Src1 elements [Src1Index..Src1Index+Len-1] with corrresponding Src2 elements [Src2Index..Src2ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2,Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply Src elements with calling vector elements.</summary>*)
   function Multiply(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Mjultiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements.</summary>
    
<remarks>Multiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer Value with all Src elements.</summary>
      
<remarks>Multiply integer Value with all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with Src vector elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Store the results in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with all calling vector elements.</summary>*)
   function Multiply(Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(Value,Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Difference.</summary>
      
<remarks>Calculate the difference for Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object
      elements [Index]..[Index+Len-1]. Size and of the calling vector must be set explicitly.
      The following formula is used to calculate the difference:

      <IMG name="tvec19"/><para/>
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True and array borders are overrun or underrun.
</remarks>
*)
   function Difference(const Vec: TMtxVecInt; VecIndex, Index, Len: integer; Lag: integer = 1): TMtxVecInt; overload;

   (*<summary>Divides coresponding elements in Src1 with Src2.</summary>
     <returns>the result of dividing Src1 values with coresponding Src2 values.</returns>*)
   function Divide(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide coresponding elements in Src1 with Src2.</summary>
    
<remarks>Divide Src2 elements [Src2Index..Src2Index+Len-1] with corrresponding Src1 elements [Src1Index..Src1ndex+Len-1]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src1, Src2: TMtxVecInt; SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements.</summary>*)
   function Divide(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements [SrcIndex..SrcIndex+Len-1].</summary>
    
<remarks>Divide calling object elements  [Index..Index+Len-1] with Src elements [SrcIndex..SrcIndex+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all Src vector elements with integer Value.</summary>
      
<remarks>Size of calling vector is adjusted automatically.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Src vector elements [SrcIndex..SrcIndex+Len-1] with integer Value.</summary>
    
<remarks>Divide Src elements [SrcIndex..SrcIndex+Len-1] with integer Value and store tje results in
    calling vector elements [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all calling vector elements with integer value.</summary>*)
   function Divide(Value: integer): TMtxVecInt; overload;
   (*<summary>Divide calling vector elements [Index..Index+Len-1] with integer value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divide Value with calling vector elements.</summary>*)
   function DivideBy(Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Value with calling vector elements [Index..Index+Len-1].</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divide Value with Src vector elements and place result in the calling vector.</summary>
              
<remarks>Size of the calling vector is adjusted automatically.
</remarks>
*)
   function DivideBy(Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Divide Value with Src vector elements [SrcIndex..SrcIndex+Len-1] and
              place the result in the calling vector elements [Index..Index+Len-1].</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(Value: integer; const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

    

    (*<summary> Compute X + Y*yScale </summary>*)
    function AddScaled(const X, Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X + Y*yScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute sqr(X + Y*yScale) </summary>
       
<remarks>By making use of yScale, it is also possible to compute the following (at the same or higher speed):

       (X - Y)^2 = X^2 - 2XY  +Y^2
</remarks>
*)
    function AddScaledSqr(const X, Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute sqr(X + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute sqr(X) + sqr(Y)*yScale </summary>
       
<remarks>By making use of yScale, it is also possible to compute the following (at the same or higher speed):

       X^2 - Y^2
</remarks>
*)
    function SqrAddScaled(const X, Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute sqr(X) + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X + Y + Z </summary>*)
    function Add(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X + Y + Z on sub arrays </summary>*)
    function Add(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X + Y + Z*zScale </summary>*)
    function AddScaled(const X, Y, Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X + Y + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X, Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;


    (*<summary> Compute X + Y + zScalar </summary>*)
    function Add(const X, Y: TMtxVecInt; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X + Y + zScalar on sub arrays </summary>*)
    function Add(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X, Y: TMtxVecInt; const yScale: integer; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X + Y*yScale + zScalar on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    

    (*<summary> Compute X - Y - Z </summary>*)
    function Sub(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X - Y - Z on sub array</summary>*)
    function Sub(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer;const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X, Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X - Y - Z*zScale </summary>*)
    function SubScaled(const X, Y, Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X - Y - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;


    (*<summary> Compute X - Y - zScalar </summary>*)
    function Sub(const X, Y: TMtxVecInt; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X - Y - zScalar on sub array</summary>*)
    function Sub(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X, Y: TMtxVecInt; const yScale: integer; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    

    (*<summary> Compute X * Y / Z </summary>*)
    function MulAndDiv(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X * Y / Z on sub array</summary>*)
    function MulAndDiv(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    

    (*<summary> Compute (X + Y)*Z </summary>*)
    function AddAndMul(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X + Y)*Z on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X + Y)*Z*zScale </summary>*)
    function AddAndMul(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + Y)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X + Y*yScale)*Z*zScale </summary>*)
    function AddAndMul(const X, Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + Y*yScale)*Z*zScale on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X + Y)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + Y)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X + Y*yScale)*zScalar </summary>*)
    function AddAndMul(const X, Y: TMtxVecInt; yScale, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; yScale, Z: integer; Index, Len: Integer): TMtxVecInt; overload;


    (*<summary> Compute (X + yScalar)*Z*zScale </summary>*)
    function AddAndMul(const X: TMtxVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVecInt; xScale, Y: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; xScale, Y : integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X + yScalar)*Z </summary>*)
    function AddAndMul(const X: TMtxVecInt; Y: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X + yScalar)*Z on sub-array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X + yScalar)*zScalar </summary>*)
    function AddAndMul(const X: TMtxVecInt; Y, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X + yScalar)*zScalar on sub-array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; Y, Z: integer; Index, Len: integer): TMtxVecInt; overload;


    

    (*<summary> Compute (X - Y)*Z </summary>*)
    function SubAndMul(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X - Y)*Z on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X - Y)*Z*zScale </summary>*)
    function SubAndMul(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - Y)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X - Y*yScale)*Z*zScale </summary>*)
    function SubAndMul(const X, Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - Y*yScale)*Z*zScale on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X - Y)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - Y)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X - Y*yScale)*zScalar </summary>*)
    function SubAndMul(const X, Y: TMtxVecInt; yScale, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; yScale, Z: integer; Index, Len: Integer): TMtxVecInt; overload;


    (*<summary> Compute (X - yScalar)*Z*zScale </summary>*)
    function SubAndMul(const X: TMtxVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVecInt; xScale, Y: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; xScale, Y : integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X - yScalar)*Z </summary>*)
    function SubAndMul(const X: TMtxVecInt; Y: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute (X - yScalar)*Z on sub-array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X - yScalar)*zScalar </summary>*)
    function SubAndMul(const X: TMtxVecInt; Y, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X - yScalar)*zScalar on sub-array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; Y, Z: integer; Index, Len: integer): TMtxVecInt; overload;


    

    (*<summary> Compute X*Y + Z </summary>*)
    function MulAndAdd(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X*Y + Z on sub array</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y*xyScale + Z </summary>*)
    function MulAndAdd(const X, Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y + Z*zScale </summary>*)
    function MulAndAdd(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*Y + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y + zScalar </summary>*)
    function MulAndAdd(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X*Y + zScalar on sub array</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    

    (*<summary> Compute X*Y - Z </summary>*)
    function MulAndSub(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X*Y - Z on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y*xyScale - Z </summary>*)
    function MulAndSub(const X, Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X*Y*xyScale + Z on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y - Z*zScale </summary>*)
    function MulAndSub(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*Y - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y - zScalar </summary>*)
    function MulAndSub(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X*Y - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    

    (*<summary> Compute X/Y + Z </summary>*)
    function DivAndAdd(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X/Y + Z on sub array</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y*xyScale + Z </summary>*)
    function DivAndAdd(const X, Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X/Y*xyScale + Z on sub array</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y + Z*zScale </summary>*)
    function DivAndAdd(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X/Y + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y + zScalar </summary>*)
    function DivAndAdd(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X/Y + zScalar on sub array</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    

        (*<summary> Compute X/Y - Z </summary>*)
    function DivAndSub(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X/Y - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y*xyScale - Z </summary>*)
    function DivAndSub(const X, Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X/Y*xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y - Z*zScale </summary>*)
    function DivAndSub(const X, Y, Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X/Y - Z*zScale on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X/Y - zScalar </summary>*)
    function DivAndSub(const X, Y: TMtxVecInt; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X/Y - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    

    (*<summary> Compute X*xScale + Y*yScale </summary>*)
    function AddScaled(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*xScale + Y*yScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute sqr(X*xScale + Y*yScale) </summary>*)
    function AddScaledSqr(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute sqr(X*xScale + Y*yScale) on sub arrays </summary>*)
    function AddScaledSqr(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale </summary>*)
    function SqrAddScaled(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): TMtxVecInt; overload;
    (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale on sub arrays </summary>*)
    function SqrAddScaled(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X*xScale + Y*yScale + Z*zScale </summary>*)
    function AddScaled(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*xScale + Y*yScale + Z*zScale on sub arrays </summary>*)
    function AddScaled(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X*xScale + Y*yScale + zScalar </summary>*)
    function AddScaledC(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute X*xScale + Y*yScale + z on sub arrays </summary>*)
    function AddScaledC(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X*xScale - Y*yScale - Z*zScale </summary>*)
    function SubScaled(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar </summary>*)
    function SubScaledC(const X: TMtxVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): TMtxVecInt; overload;

    (*<summary> Compute X*xScale - Y*yScale - Z*zScale on sub array</summary>*)
    function SubScaled(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: TMtxVecInt; zIndex: integer; const zScale: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute X*xScale - Y*yScale - zScalar on sub array</summary>*)
    function SubScaledC(const X: TMtxVecInt; xIndex: integer; const xScale: integer; const Y: TMtxVecInt; yIndex: integer; const yScale: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X*Y*Z </summary>
       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*Y
</remarks>
*)
    function Mul(const X, Y,Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X*Y*zScalar </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Z parameter:

       X^2*zScalar
</remarks>
*)
    function Mul(const X,Y: TMtxVecInt; const Z: integer): TMtxVecInt; overload;


    (*<summary> Compute X*Y*Z on sub array</summary>*)
    function Mul(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute X*Y*zScalar on sub array</summary>*)
    function Mul(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X / (Y*Z) </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing Y also for the Z parameter:

       X / (Y^2)
</remarks>
*)
    function Divide(const X, Y, Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X / (Y*zScale) </summary>*)
    function DivideC(const X, Y: TMtxVecInt; const Z: integer): TMtxVecInt; overload;

    (*<summary> Compute X/( Y*Z ) on sub array</summary>*)
    function Divide(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute X / (Y*zScalar) on sub array</summary>*)
    function DivideC(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute X * Y * xyScale / Z </summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2 * xyScale/ Z
</remarks>
*)
    function MulAndDiv(const X, Y: TMtxVecInt; const xyScale: integer; const Z: TMtxVecInt): TMtxVecInt; overload;
    (*<summary> Compute X * Y / zScalar </summary>*)
    function MulAndDiv(const X, Y: TMtxVecInt; const Z: integer): TMtxVecInt; overload;
    (*<summary> Compute xScalar / (Y*Z) </summary>*)
    function Divide(const X: integer; const Y, Z: TMtxVecInt): TMtxVecInt; overload;

    (*<summary> Compute X * Y xyScale / Z on sub array</summary>*)
    function MulAndDiv(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const xyScale: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute X * Y / zScalar on sub array</summary>*)
    function MulAndDiv(const X: TMtxVecInt; xIndex: integer; const Y: TMtxVecInt; yIndex: integer; const Z: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute xScalar / (Y*Z) on sub array</summary>*)
    function Divide(const X: integer; const Y: TMtxVecInt; yIndex: integer; const Z: TMtxVecInt; zIndex: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale + Y*yScale)*Z*zScale</summary>

      
<remarks>The following is also possible at the same or higher speed:

      X^2 * xScale + XY * yScale

      by passing X also for the Z parameter.
</remarks>
*)
    function AddAndMul(const X: TMtxVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*Z*Scale on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; xScale: integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + Y*yScale)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: Integer; xScale: integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale + yScalar)*Z*zScale</summary>*)
    function AddAndMul(const X: TMtxVecInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + yScalar)*zScalar</summary>*)
    function AddAndMul(const X: TMtxVecInt; xScale: integer; Y, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + yScalar)*Z*zScale on sub-array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; xScale: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; zScale: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale + yScalar)*zScalar on sub array</summary>*)
    function AddAndMul(const X: TMtxVecInt; xIndex: integer; xScale: integer; Y, Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale - Y*yScale)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*Z*Scale on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; xScale: integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - Y*yScale)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: Integer; xScale: integer; const Y: TMtxVecInt; yIndex: Integer; yScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X*xScale - yScalar)*Z*zScale</summary>*)
    function SubAndMul(const X: TMtxVecInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - yScalar)*zScalar</summary>*)
    function SubAndMul(const X: TMtxVecInt; xScale: integer; Y, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - yScalar)*Z*zScale on sub-array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; xScale: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; zScale: integer; Index, Len: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*xScale - yScalar)*zScalar on sub array</summary>*)
    function SubAndMul(const X: TMtxVecInt; xIndex: integer; xScale: integer; Y, Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + Z*zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale + Z*zScale on sub array</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale + zScalar on sub array</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing X also for the Y parameter:

       X^2*xyScale + zScale
</remarks>
*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVecInt; Y, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*yScalar) + zScalar</summary>*)
    function MulAndAdd(const X: TMtxVecInt; xIndex: integer; Y, Z: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X*Y)*xyScale - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale - zScalar</summary>*)
    function MulAndSub(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale, Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X*Y)*xyScale - zScalar on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute X*yScalar - Z*zScale</summary>*)
    function MulAndSub(const X: TMtxVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute X*yScalar - Z*zScale on sub array</summary>*)
    function MulAndSub(const X: TMtxVecInt; xIndex: integer; Y: integer; const Z: TMtxVecInt; zIndex: integer; zScale: integer; Index, Len: integer): TMtxVecInt; overload;

    (*<summary> Compute (X / Y)*xyScale + Z*zScale</summary>

       
<remarks>The following expression would also run at the same or higher speed, when passing Z also for the Y parameter:

       (X / Z) * xyScale + Z * zScale
</remarks>
*)
    function DivAndAdd(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y)*xyScale + Z*zScale on sub array</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y) * xyScale + zScalar</summary>*)
    function DivAndAdd(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;

    (*<summary> Compute (X / Y) * xyScale - Z y</summary>*)
    function DivAndSub(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y) * xyScale - zScalar</summary>*)
    function DivAndSub(const X: TMtxVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y) * xyScale - Z on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; const Z: TMtxVecInt; zIndex: Integer; zScale: integer; Index, Len: Integer): TMtxVecInt; overload;
    (*<summary> Compute (X / Y) * xyScale - zScalar on sub array</summary>*)
    function DivAndSub(const X: TMtxVecInt; xIndex: Integer; const Y: TMtxVecInt; yIndex: Integer; xyScale: integer; Z: integer; Index, Len: Integer): TMtxVecInt; overload;


    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. The Size
      property of the calling object is set implicitly to match Vec object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a,b,c;
          a = new TVecInt();
          b = new TVecInt();
          a.SetIt(new int[] {1,2,3,4});  // a = [1,2,3,4] i.e 1+2i ; 3+4i
          b.Copy(a);                // b = [1,2,3,4]
        }
      }
      </code></example>*)
   function Copy(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Copy Src elements [SrcIndex]..[SrcIndex+Len-1] in the calling object
     elements [Index]..[Index+Len-1].</summary>
     
<remarks>Calling vector <see cref="Size"/> must be set explicitly. An exception is raised if array borders
     are overrun or underrun.
</remarks>
*)
   function Copy(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary> Copy each of Vec elements to the calling object. </summary>
            
<remarks>The Size property of the calling object is set implicitly to match Vec object.
            The storage precision of the calling object is set to DstIntPrecision.
</remarks>
*)
   function Convert(const Src: TMtxVecInt; DstIntPrecision: TIntPrecision): TMtxVecInt; overload;

   (*<summary>Copy Src elements [SrcIndex]..[SrcIndex+Len-1] in the calling object
     elements [Index]..[Index+Len-1].</summary>
     
<remarks>Calling vector <see cref="Size"/> must be set explicitly. An exception is raised if array borders
     are overrun or underrun. If the precision of the calling does not match DstIntPrecision, an exception will
     be raised.
</remarks>
*)
   function Convert(const Src: TMtxVecInt; DstIntPrecision: TIntPrecision; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Compares Value with all calling object elements.</summary>*)
    function IsEqual(Value: integer): boolean; overload;

    (*<summary>Compares Value with all calling object elements.</summary>*)
    function IsEqual(Value, Index, Len: integer): boolean; overload;

    (*<summary>Compares all Vec elements with calling object elements and checks also Length and IntPrecision properties, if they match.</summary>*)
    function IsEqual(Vec: TMtxVecInt): boolean; overload;

    (*<summary>Compares Vec elements [VecIndex]..[VecIndex+Len-1] with calling object
       elements [Index]..[Index+Len-1].</summary>*)
    function IsEqual(Vec: TMtxVecInt; VecIndex, Index, Len: integer): boolean; overload;

   (*<summary>First element in object Values array.</summary>
      <returns>first integer element in object Values array.</returns>

      <SeeAlso cref="Last"/>*)
    property First: integer read get_First;

    (*<summary>Last element in object Values array.</summary>
      <returns>the last integer element in calling object Values array.</returns>

      <SeeAlso cref="First"/>*)
    property Last: integer read get_Last;

    (*<summary>Vectorised maximum.</summary>
      
<remarks>Compares Vec values with corresponding elements in the calling object and stores the bigger value in Self.
      Size and <see cref="TMtxVecInt.IntPrecision"/> properties need to match.
</remarks>


      <SeeAlso cref="MinEvery"/>*)
    function MaxEvery(const Vec: TMtxVecInt): TMtxVecInt; overload; 
    (*<summary>Compare Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] and store result in Self.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TMtxVecInt.IntPrecision"/> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function MaxEvery(const Vec: TMtxVecInt; VecIndex, Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and store bigger value in Self.</summary>
      
<remarks>Stores the results in to the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVecInt.IntPrecision"/> property do not match.
</remarks>
*)
    function MaxEvery(const Vec1, Vec2: TMtxVecInt): TMtxVecInt; overload; 
    (*<summary>Compare Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1] and store bigger value in Self.</summary>
      
<remarks>Stores the results in to the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVecInt.IntPrecision"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function MaxEvery(const Vec1, Vec2: TMtxVecInt; Vec1Index, Vec2Index, Index, Len: integer): TMtxVecInt; overload; 



    (*<summary>Vectorised minimum.</summary>
      
<remarks>Compares Vec values with corresponding elements in the calling object and stores the smaller value in Self.
      Size and <see cref="TMtxVecInt.IntPrecision"/> property of the calling object are set automatically.
</remarks>


      <SeeAlso cref="MinEvery"/>*)
    function MinEvery(const Vec: TMtxVecInt): TMtxVecInt; overload; 
    (*<summary>Compare Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] and store result in Self.</summary>
      
<remarks>An exception is raised, if Vec and calling object <see cref="TMtxVecInt.IntPrecision"/> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function MinEvery(const Vec: TMtxVecInt; VecIndex, Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and store the smaller value in Self.</summary>
      
<remarks>Stores the results in to the calling object.
      Size and <see cref="TMtxVec.Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVecInt.IntPrecision"/> property do not match.
</remarks>
*)
    function MinEvery(const Vec1, Vec2: TMtxVecInt): TMtxVecInt; overload; 
    (*<summary>Compare Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1] and store the smaller value in Self.</summary>
      
<remarks>Stores the results in to the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TMtxVecInt.IntPrecision"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is True
      and array borders are overrun or underrun.
</remarks>
*)
    function MinEvery(const Vec1, Vec2: TMtxVecInt; Vec1Index, Vec2Index, Index, Len: integer): TMtxVecInt; overload; 

      (*<summary>Maximum value.</summary>
    <returns>The maximum value of all calling object elements. The result is an integer value.</returns>

    

    <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4});
            int b = a.Max();  // 4
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>*)
   function Max(): integer; overload;
   (*<summary>Calculate the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is an integer value.
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Max(Index, Len: integer): integer; overload;

   (*<summary>Minimum value.</summary>
    <returns>The minimum value of all calling object elements. The result is an integer value.</returns>

    

    <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4});
            int b = a.Min();  // 1
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>*)
   function Min(): integer; overload;
   (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is an integer value.
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Min(Index, Len: integer): integer; overload;

   (*<summary>Calculate minimum and maximum in one procedure call.</summary>
     <param name="aMax">Returns calling vector maximum value.</param>
     <param name="aMin">Returns calling vector monimum value.</param>*)
   procedure MaxMin(out aMax, aMin: integer); overload;
   (*<summary>Calculate minimum and maximum in one procedure call.</summary>
     <param name="aMax">Returns calling vector elements [Index..Index+Len-1] maximum value.</param>
     <param name="aMin">Returns calling vector elements [Index..Index+Len-1] minimum value.</param>
     <param name="Index">The starting index at which to start the search.</param>
     <param name="Len">The number of elements to search starting from Index.</param>

     
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   procedure MaxMin(out aMax, aMin: integer; Index, Len: integer); overload;

   (*<summary>Calculate the maximum value of all calling object elements.</summary>
    <returns>the maximum of all calling vector integer elements in-place.</returns>
    <param name="aIndex">Stores maximum value index.</param>

    

    <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          int ind;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4)
            int max = a.Max(out ind); //max=3, ind = 3
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Min"/>*)
   function Max(out aIndex: integer): integer; overload;
   (*<summary>Calculate the maximum value of calling object elements [Index..Index+Len-1].</summary>
     <returns>the maximum of calling vector integer elements [Index..Index+Len-1].</returns>
     <param name="aIndex">Returns maximum value index.</param>
     <param name="Index">The starting index at which to start the search.</param>
     <param name="Len">The number of elements to search starting from Index.</param>

     
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Max(out aIndex: integer; Index, Len: integer): integer; overload;

   (*<summary>Calculate the minimum value of all calling object elements.</summary>
    <returns>the minimum of all calling vector integer elements in-place.</returns>
    <param name="aIndex">Stores minimum value index.</param>

    

    <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          int ind;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4);
            int min = a.Min(out ind); //min=1, ind = 0
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>*)
   function Min(out aIndex: integer): integer; overload;
   (*<summary>Calculate the maximum value of calling object elements [Index..Index+Len-1].</summary>
     <returns>the minimum of calling vector integer elements [Index..Index+Len-1].</returns>
     <param name="aIndex">Returns minimum value index.</param>
     <param name="Index">The starting index at which to start the search.</param>
     <param name="Len">The number of elements to search starting from Index.</param>

     
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Min(out aIndex: integer; Index, Len: integer): integer; overload;

   (*<summary>Calculate minimum, maximum and their indices in one procedure call.</summary>
     <param name="aMax">Returns calling vector maximum value.</param>
     <param name="aMaxIdx">Returns calling vector maximum value index.</param>
     <param name="aMin">Returns calling vector monimum value.</param>
     <param name="aMinIdx">Returns calling vector minimum value index.</param>*)
   procedure MaxMinIdx(out aMax, aMaxIdx, aMin, aMinIdx: integer); overload;
   (*<summary>Calculate minimum, maximum and their indices in one procedure call.</summary>
     <param name="aMax">Returns calling vector elements [Index..Index+Len-1] maximum value.</param>
     <param name="aMaxIdx">Returns calling vector maximum value index.</param>
     <param name="aMin">Returns calling vector elements [Index..Index+Len-1] minimum value.</param>
     <param name="aMinIdx">Returns calling vector minimum value index.</param>
     <param name="Index">The starting index at which to start the search.</param>
     <param name="Len">The number of elements to search starting from Index.</param>

     
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   procedure MaxMinIdx(out aMax, aMaxIdx, aMin, aMinIdx: integer; Index, Len: integer); overload;

   (*<summary>Applies median filter with size Mask to the calling vector elements [Index.. Index+Len-1].</summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
            vector with the median value, calculated over the fixed range (mask) centered around that element.
</remarks>
*)
   function Median(MaskSize, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Applies median filter with size Mask to Src object elements [SrcIndex..SrcIndex+Len-1] and stores the result in the calling vector
                elements [Index..Index+Len-1]. </summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the Src
            vector with the median value, calculated over the fixed range (mask) centered around that element
            and stores the result in the calling vector.
</remarks>
*)
   function Median(const Src: TMtxVecInt; MaskSize, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Calculate median value for calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun.
</remarks>
*)
    function Median(Index: integer; Len: integer): integer; overload; 

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
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4,5});
            int c = a.Median(); // 3.0
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mean"/>*)
    function Median: integer; overload; 

    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Method uses the following rule:

      <code>Values[k] := Offset + k*Step.
      </code><para/>
      An exception is raised if calling vector array borders are overrun.
</remarks>
*)
    function Ramp(Offset,Step: double; Index,Len: integer): TMtxVecInt; overload;

   (*<summary>A cyclic shift on vector elements in range.</summary>
     
<remarks>Performs cyclic shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

     An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Rotate(Offset, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Shift"/>*)
   function Rotate(const Src: TMtxVecInt; Offset, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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
          TVecInt a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4);
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
    function Reverse(Vec: TMtxVecInt; VecIndex, Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value elements and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Size and <see cref="IntPrecision"/> properties of the calling object and of RemDst are adjusted automatically.
</remarks>
*)
    function RemDiv(Src, Value: TMtxVecInt; RemDst: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value and store result of division in the calling object and reminder in RemDst.</summary>
                              
<remarks>Store the result of division in the calling object elements [Index]..[Index+Len-1]
                              and remainder in to [RemDstIndex] .. [RemDstIndex+ Len-1]. Size and <see cref="IntPrecision"/> properties of
                              the calling object and RemDst must be set explicitly. An exception is raised
                              if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function RemDiv(Src, Value: TMtxVecInt; RemDst: TMtxVecInt; SrcIndex, ValueIndex, RemDstIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Size and <see cref="IntPrecision"/> properties of the calling object and of RemDst are adjusted automatically.
</remarks>
*)
    function RemDiv(Src: TMtxVecInt; Value: integer; RemDst: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Store the result of division in the calling object elements [Index]..[Index+Len-1]
               and remainder in to [RemDstIndex] .. [RemDstIndex+ Len-1]. Size and <see cref="IntPrecision"/> properties of
               the calling object and RemDst must be set explicitly. An exception is raised
               if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function RemDiv(Src: TMtxVecInt; Value: integer; RemDst: TMtxVecInt; SrcIndex, RemDstIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Divide Src with all Value elements and store result of division in the calling object and reminder in RemDst.</summary>
           
<remarks>Size and <see cref="IntPrecision"/> properties of the calling object and of RemDst are adjusted automatically.
</remarks>
*)
    function RemDiv(Src: integer; Value: TMtxVecInt; RemDst: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>Divide Src with Value elements and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Store the result of division in the calling object elements [Index]..[Index+Len-1]
               and remainder in to [RemDstIndex] .. [RemDstIndex+ Len-1]. Size and <see cref="IntPrecision"/> properties of
               the calling object and RemDst must be set explicitly. An exception is raised
               if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function RemDiv(Src: integer; Value: TMtxVecInt; RemDst: TMtxVecInt; ValueIndex, RemDstIndex, Index, Len: integer): TMtxVecInt; overload;

  (*<summary>Set all calling object elements [Index]..[Index+Len-1] to Value.</summary>
    
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>


    

    <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SezIt(new int[] {1,2,3,4);
            a.SetVal(5,2,2);  // a = [1,1,5,5]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function SetVal(Value, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Set all calling object elements to Value.</summary>*)
   function SetVal(Value: integer): TMtxVecInt; overload;

   (*<summary>Perform a shift on vector elements in range.</summary>
     
<remarks>Performs shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Shift(Offset, Index, Len: integer): TMtxVecInt; overload;
    (*<summary>A shift on vector elements in range.</summary>
      
<remarks>Performs a shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Rotate"/>*)
   function Shift(const Src: TMtxVecInt; Offset, SrcIndex, Index, Len: integer): TMtxVecInt; overload;


   (*<summary>Set all calling vector elements to zero.</summary>*)
   function SetZero(): TMtxVecInt; overload;
   (*<summary>Set calling vector elements [Index..Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function SetZero(Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort calling vector elements [Index..Index+Len-1] in ascending order in-place.</summary>*)
   function SortAscend(Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      <param name="SortIdxIndex">The starting index at which to store sorted values indices in DstSortIdx.</param>
      <param name="Index">The starting index at which to start the sort.</param>
      <param name="Len">The number of elements to sort starting from including zero-based Index.</param>
      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
   function SortAscend(DstSortIdx: TMtxVecInt; SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort calling vector elements [Index..Index+Len-1] in descending order in-place.</summary>*)
   function SortDescend(Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Sort all calling vector elements in descending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      <param name="SortIdxIndex">The starting index at which to store sorted values indices in DstSortIdx.</param>
      <param name="Index">The starting index at which to start the sort.</param>
      <param name="Len">The number of elements to sort starting from including zero-based Index.</param>
      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)

   function SortDescend(DstSortIdx: TMtxVecInt; SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Sums vector values.</summary>
      <returns>the sum of all calling object elements.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,3,-2});
            int s = a.Sum();  // s = 2
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function Sum(): integer; overload;
   (*<summary>Returns the sum of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Sum(Index, Len: integer): integer; overload;

    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns pointer to the current integer relative to the value of the Precision property.
</remarks>
*)
   function PValues(Index: integer): PAPointer; overload; virtual;
    (*<summary>Obtains a pointer to the 32bit signed integer value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i].
</remarks>
*)
   function PIValues(Index: integer): PAPointer; overload; virtual;

    (*<summary>Obtains a pointer to the 16bit signed integer value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i].
</remarks>
*)
   function PSValues(Index: integer): PAPointer; overload; virtual;

    (*<summary>Obtains a pointer to the 8bit unsigned integer value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i].
</remarks>
*)
   function PBValues(Index: integer): PAPointer; overload; virtual;

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

      See the <see cref="TVecInt.Gather"/> method to see how to perform gathering.

      The performance of the CPU heavily depends on the assumption that elements are stored at consecutive memory locations.
      If it is neccessary to apply a set of operations only to elements at specific indexes, performance-wise it can prove to be
      very helpfull, if the elements are gathered first.
</remarks>


      <SeeAlso cref="TVecInt.Gather"/>*)
    function Scatter(const Src: TMtxVecInt; const Indexes: TMtxVecInt = nil; IndexType: TIndexType = indVector; Increment: integer = 1; Offset: integer = 0): TMtxVecInt; overload; 

    (*<summary> Scatters Src elements starting at Offset and with Increment to the calling object. </summary>*)
    function ScatterByIncr(const Src: TMtxVecInt; Increment: integer = 1; Offset: integer = 0): TMtxVecInt; overload;

    (*<summary> Scatters Src elements defined with indices stored in Indexes to the calling object. </summary>*)
    function ScatterByIndexes(const Src: TMtxVecInt; const Indexes: TMtxVecInt): TMtxVecInt; overload;

    (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
    function ScatterByMask(const Src: TMtxVecInt; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVecInt; overload; virtual;



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
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {2, 0, 3, 4});
            a.ThreshBottom(1); // a = [2,1,3,4]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshBottom(const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perform the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Value: integer; Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="IntPrecision"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="IntPrecision"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see>
      is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Vec: TMtxVecInt; const Value: integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 

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
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {2, 0, 3, 4});
            a.ThreshTop(1); // a = [1, 0, 1, 1]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshTop(const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Value: integer; Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Vec: TMtxVecInt; const Value: integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 


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
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {2,-1,3,4});
            a.ThreshAbsLT(2); // a = [2,-2,3,4]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshAbsLT(const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsLT(const Value: Integer; Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsLT(const Src: TMtxVecInt; const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsLT(const Vec: TMtxVecInt; const Value: Integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 


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
          TVecInt a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {2, -1, 3, 4});
            a.ThreshAbsGT(2); // a = [2, -1, 2, 2]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshAbsGT(const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsGT(const Value: Integer; Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsGT(const Src: TMtxVecInt; const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThreshAbsGT(const Vec: TMtxVecInt; const Value: Integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values less than LTLevel will be replaced with LTValue. All values bigger than GTLevel will be replaced with GTValue.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {20,1,30,40});
            a.ThresholdGT_LT(23,34,10,5); // a = [20,5,34,34]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshBottom"/>
      <SeeAlso cref="ThreshTop"/>*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: Integer): TMtxVecInt; overload; 
    (*<summary>Perfrom "greater than and less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: Integer; Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size of the calling object is adjusted automatically.
</remarks>
*)
    function ThresholdGT_LT (const Vec: TMtxVecInt; const GTLevel, GTValue, LTLevel, LTValue: Integer): TMtxVecInt; overload; 
    (*<summary>Perform "greater than and less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size of the calling object must be
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck">ConditionCheck</see> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TMtxVecInt; const GTLevel, GTValue, LTLevel, LTValue: Integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 

    (*<summary>Finds matching indexes and returns their count. The b parameter is of integer type.</summary>
               
<remarks>op parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>
</remarks>
*)
    function FindIndexesAndLength(const a: TMtxVecInt; const op: string; const b: Integer): integer; overload;
    (*<summary>Finds matching indexes and returns their count. The b parameter is of <see cref="TMtxVecInt"/> type.</summary>
               
<remarks>op parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>
</remarks>
*)
    function FindIndexesAndLength(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): integer; overload;

    (*<summary>Fills the calling vector with indexes, where the logical expression is true.</summary>
      
<remarks>ills the calling vector with indexes, where the Op comparison between a and b is True. Op string parameter can be
      <c> '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;' </c>.
</remarks>


      <SeeAlso cref="FindAndGather"/>
      <SeeAlso cref="FindMask"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Gather"/>
      <SeeAlso cref="Scatter"/>*)
    (*<summary>Finds matching indexes. The b parameter is of integer type.</summary>
               
<remarks>op parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>
</remarks>
*)
    function FindIndexes(const a: TMtxVecInt; const op: string; const b: integer): TMtxVecInt; overload;
    (*<summary>Finds matching indexes. The b parameter is of <see cref="TMtxVecInt"/> type.</summary>
               
<remarks>op parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>
</remarks>
*)
    function FindIndexes(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): TMtxVecInt; overload;

     (*<summary>Finds a vector mask.</summary>
        
<remarks>The calling vector will hold the those
        elements where the Op comparison between a and b is True.
        Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>.

        The calling vector will store the mask, 1 at those index locations
        where the Op comparison was True and 0 at those index locations
        where the Op comparison was false.
</remarks>
*)
    function FindMask(const a: TMtxVecInt; const op: string; const b: integer): TMtxVecInt; overload;
    function FindMask(const a: TMtxVecInt; const op: char; const b: integer): TMtxVecInt; overload;

    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindMask(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): TMtxVecInt; overload;
    function FindMask(const a: TMtxVecInt; const op: char; const b: TMtxVecInt): TMtxVecInt; overload;


    
    (*<summary>Read values content from stream to object.</summary>
      
<remarks>Reads values content from SrcStream stream to calling objct. No other values describing the data type or length are read
      from the DstStream. Number type is defined by the Precision parameter, which can be obtained from <see cref="ReadHeader"/> method
      call.

      Note
        Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TVecInt;
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
    function ReadValues(SrcStream: TStream; aPrecision: TPrecision = prInteger; Endian: TEndianness = MtxSystemEndianness): integer; overload; virtual;
    

    


   
    (*<summary>Writes object Values content to a stream.</summary>
      
<remarks>Writes the calling object Values content to the DstStream stream. No other values describing the data type or length are written
      to the DstStream. Number type is defined by the Precision parameter, but only
      integer formats are accepted. Attempt to save single (float) precision or double precision will raise an exception.
      The paramateres must be the same as for the <see cref="WriteHeader"/> method.
      Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TVecInt;
        AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt([0,0,1,3,2]);
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
      <SeeAlso cref="SaveToStream"/>*)
    procedure WriteValues(DstStream: TStream; aPrecision: TPrecision = prInteger; Endian: TEndianness = MtxSystemEndianness); overload; virtual;    

    

    (*<summary>Writes the header information for the calling vector to a stream.</summary>
      
<remarks>Writes the header information for the calling object to a DstStream stream.
      The header information contains information about object (size, type of values
      in Values array, ...) which all define the state of the object. Number type is
      defined by the Precision parameter.
      Attempt to save single precision or double precision will raise an exception.
</remarks>


      <Example>
      <code>
      var b: TVecInt;
          AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt([0,0,1,3,2]);
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
    <SeeAlso cref="TMtxVecBase.SaveToStream"/>*)
    

    
    procedure WriteHeader(const DstStream: TStream; aPrecision: TPrecision = prInteger; Endian: TEndianness = MtxSystemEndianness); overload; virtual;
    

    (*<summary>Reads the header information from a stream to object.</summary>
      
<remarks>Reads the header information from a DstStream stream to calling object. The header information contains all necessary information
      defining the object. The function returns the precision in which the data was stored.
      This information is required for the <see cref="ReadValues"/> method.
</remarks>

      <Example>
      <code>
      var b: TVecInt;
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
    

    
    function ReadHeader(const SrcStream: TStream; Endian: TEndianness = MtxSystemEndianness): TPrecision; overload; virtual;
    
  public
    (*<summary> Defines internal storage precision which can be either 32, 16 or 8 bit.</summary>*)
    property IntPrecision: TIntPrecision read FIntPrecision write SetIntPrecision;
    (*<summary> Prevents change to IntPrecision if Lock = True.</summary>*)
    property IntPrecisionLock: boolean read FIntPrecisionLock write SetIntPrecisionLock;
    (*<summary> Specifies the power of two for the scale factor used by some methods.</summary>*)
    property ScaleFactor: integer read FScaleFactor write SetScaleFactor;
  end;

  procedure FindMask(const Dst: TMtxVecInt; const a: TMtxVecInt; const op: string; const b: integer); overload;
  procedure FindMask(const Dst: TMtxVecInt; const a: TMtxVecInt; const op: string; const b: TMtxVecInt); overload;

