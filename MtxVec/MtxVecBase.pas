










(*<summary>Implements abstract classes and shared code for TVec/TMtx/TVecInt.</summary>*)
unit MtxVecBase;


{$I BdsppDefs.inc}

interface

uses  Math387

      
          
          ,Windows
          ,SyncObjs
          
      

      
      ,Contnrs
      
      ,Classes
      ,SysUtils
      
      ,Forms
      ,Dialogs
      






    ;



type

    
    TObjectsList = TObjectList;
    

    
    TMtxVecType = (mvTVec, mvTMtx, mvTVecInt, mvTMtxInt, mvTSparseMtx);
    

    
    TMtxVecBase = class;

    TSubRangeItem = record
      Index: integer;  
      Len: integer; 
      Src: TMtxVecBase; 
    end;

    TSubRangeStack = array of TSubRangeItem;
    TSubRangeOp = (srPush, srNone);
    


   (*<summary>Base class for all one/two/../n dimensional arrays.</summary>
    
<remarks>This is the basic vector/matrix object. It handles memory
    allocation and basic vector arithmetics.

    It works with memory obtained directly from the operating system.
    The object bypasses the default Delphi memory manager and frees
    it from the load for which it is not very well suited. Memory
    allocation is cached via an "object cache" mechanism.

    <b>Error checking</b>

    All methods and properties of TMtxVec descendants are explicitly "range checked".
    Range checking ensures that the user can not read
    or write values past the size of the allocated memory. Once the code is compiled without assertations,
    range checking is disabled and higher performance can be achieved in some cases.
    Every effort has been made to prevent the user of the library to make an error that
    would result in memory overwrite. (Writing or reading to parts of the memory
    which were not allocated before and thus overwriting data of another part of the application.)
</remarks>
*)

  TMtxVecBase = class(TPersistent )
  strict private
    fCondEnabled: boolean;
    FOnSetSize: TMtxNotifyEvent;
    FBlockEnd: boolean;
  
    procedure set_Caption(const value: string);
    procedure setOnSetSize(const value: TMtxNotifyEvent);
    procedure set_Tag(const value: integer);
    procedure set_ConditionCheck(const value: boolean);
    procedure set_CacheIndex(const value: integer);
    function get_BlockEnd: boolean;
    function get_MtxVecType: TMtxVecType;
  protected
    
    procedure FreeCached; virtual;
    procedure CreateCached(aPoolIndex: integer; aCacheSizeInBytes: Int64; aMemIndex, aCacheIndex: integer); virtual;
    
    procedure DebugReport(ItemIndex: integer);
    
 strict protected
    fMemIndex: integer;
    fSubRangeStack: TSubRangeStack;
    FCacheOwned: boolean;
    fCacheIndex: integer;     
    fSubRangeLevel: integer;
    fIsDouble: boolean;
    fPoolIndex: integer;

    
        (*<summary> Pointer to the automatic reference counting block. </summary>*)
        ipoint: PIArray;
        (*<summary> Pointer to the allocated memory block. </summary>*)
        aPoint: PointerInteger;
        (*<summary> Pointer to the first element of the array. </summary>*)
        ValuesPointer: PointerInteger;
        (*<summary> Amount of allocated memory. </summary>*)
        AllocSize: Int64;
    

    FCaption: string;

    FTag: integer;
    SComplex: boolean;
    fComplex: boolean;
    fLength: integer;

    BlockSize,BlockI: integer;
    BlockLevel: integer;
    BlockSrc: TMtxVecBase;

    fIsSubRange: boolean;
    fActualSizeInBytes: Int64;
    fConditionCheck: boolean;
    subrange_refs_count: integer;
    usage_refs_count: integer;

    fAllowSubrange: boolean;
    FFTDescriptorIndex: integer;
    fCapacityInBytes: Int64;
    fCapacityStep: double;
    SActualSize: Int64;
    SFloatPrecision: TMtxFloatPrecision;
    SLength: integer;
    SOffset: integer;
    SIsDouble: boolean;
    
    SValuesPointer: PointerInteger;

    

    fMtxVecType: TMtxVecType;
    fFloatPrecision: TMtxFloatPrecision;

    fBlockSubRangeLevel: integer; 
    fDebuggerMarker: integer;   

    procedure SubRangeSaveState; virtual; abstract;
    procedure SubRangeLoadState; virtual; abstract;

    procedure SetSubRangeDirect(Index, Len: integer); overload; virtual; abstract;
    procedure SetSubRangeDirect(const Src: TMtxVecBase; Index, Len: integer); overload; virtual; abstract;

    procedure SetFullRangeInternal(PopStack: boolean); 

    procedure SetSubRangeInternal(Index: integer; Len: integer; stackOp: TSubRangeOp); overload; 
    procedure SetSubRangeInternal(const Src: TMtxVecBase; Index: integer; Len: integer; stackOp: TSubRangeOp); overload; 

    procedure InternalSubRangeStackOp(var Index: integer; var Len: integer; stackOp: TSubRangeOp); overload; 
    procedure InternalSubRangeStackOp(const Src: TMtxVecBase; Index: integer; Len: integer; stackOp: TSubRangeOp); overload; 

    procedure CopyFromArray(const Src: TIntegerArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; virtual; abstract; 
    procedure CopyFromArray(const Src: TSmallIntArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; virtual; abstract; 
    procedure CopyFromArray(const Src: Math387.TByteArray; const Dst: TMtxVecBase; const SrcIdx, DstIdx, Len: integer); overload; virtual; abstract;
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TDoubleArray; const Index,DstIdx,Len: integer); overload; virtual; abstract; 
    procedure CopyToArrayInternal(const Src: TMtxVecBase; const Dst: TSingleArray; const Index,DstIdx,Len: integer); overload; virtual; abstract; 

    procedure AddRefSubrange; 
    procedure AddRefUsage; 
    procedure ReleaseSubrange; 
    procedure ReleaseUsage; 

    
    procedure LoadBinaryStream(Stream: TStream); overload;
    procedure SaveBinaryStream(Stream: TStream); overload;
    procedure InternalReadHeader(const SrcStream: TStream); overload; virtual;
    procedure InternalWriteHeader(const DstStream: TStream); overload; virtual;
    

    

    procedure MapIndexes(const Src: TIntegerArray; var Idxs,Lens: TIntegerArray; IdxOffset: integer);
    function InsertRowHeaders(const aList: TStrings; const aListIndex: integer; const Align: TFixedTextAlign; const Rows: integer): integer;
    procedure InsertColumnHeaders(const aList: TStrings; const aListIndex: integer; const Align: TFixedTextAlign; const Cols: integer; rowLabelColumnWidth, valueColumnWidth: integer);
    function InternalStringPadding(const strList: array of string; const aList: TStrings; const aListIndex: integer; const Align: TFixedTextAlign; const Headers: boolean): integer;

    procedure AllocMemory; virtual;
    procedure AllocCacheMemory; virtual;
    procedure HookPointers; virtual;

    function AllocateMemory(Bytes: Int64): PointerInteger; 
    procedure ReleaseMemory; virtual;
    
    procedure DefineProperties(Filer: TFiler); override;
    

    function CacheLength: integer; virtual;
    function CacheLengthInBytes: Int64; virtual;
    function ElemSize: Integer; virtual;
    procedure setActualSize(const byteCount: Int64); virtual;

    function ValuesToStringsInternal(const dstList: TStrings; const aDelimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload; virtual;
    function StringsToValuesInternal(const srcList: TStrings; const aDelimiter: string): TMtxVecBase; overload; virtual;

    (*<summary>For internal use only. Read to get the amount of memory allocated in number of doubles (8byte packs).</summary>*)
    property ActualSize: Int64 read FActualSizeInBytes write setActualSize;



  strict protected

    procedure SetCapacityInBytes(const value: Int64); virtual;


    procedure SetCapacityInElements(const value: Int64); virtual;
    function GetCapacityInElements: Int64; virtual;
    procedure setCapacityStep(const value: double); virtual;
    procedure setIsDouble(const value: boolean); virtual;
    procedure SetLength(const value: integer); virtual;
  public



    
    ThreadID: TThreadID; 
    property MtxVecType: TMtxVecType read get_MtxVecType;
    constructor Create; virtual;
    destructor Destroy; override;
    

    (*<summary>Returns the current subrange stack level.</summary>
               
<remarks>When the stack has zero size, the subrange level is -1.
               Maximum level is determined with MaxSubRangeLevel global variable with default value of 4.
               This routine can be usefull for debugging purposes.
</remarks>
*)
    function SubRangeLevel: integer; 

    (*<summary>Returns the current subrange stack level used by block processing functions.</summary>
               
<remarks>When the stack has zero size, the subrange level is -1.
               This routine can be usefull for debugging purposes. When block processing is initiated with a call to BlockInit,
               the value of BlockSubRangeLevel is set to be equal to the current value of SubRangeLevel. When the block
               processing finishes, the BlockSubRangeLevel will be set to -1.
</remarks>
*)
    function BlockSubRangeLevel: integer; 

    

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted or indices adjusted.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised, if an attempt is made to change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on the
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(Index: integer; Len: integer = MtxVecEOA); overload;

    (*<summary>Defines a sub vector/matrix and pushes any previous subranges on to a stack.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted or indices adjusted. The routine allows for stack based pushing and poping of sub-ranges with
      the additional constraint that each deepening of the sub-range is strictly within the bounds of the previous one.
      The pairs of SetSubRangeLevel/SetFullRangeLevel need to match.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised, if an attempt is made to change the size of calling object.

      It is possible to call SetSubRange/SetFullRange on the object, which has been subranged with SetSubRangeLevel.

      To again obtain a view of the previous subrange level on the vector/matrix, call <see cref="SetFullRangeLevel"/>

      The Index parameter of the SetSubRange is absolute for the original memory or current stack level.
      The Index parameter for the SetSubRangeLevel is relative to the previous SetSubRangeLevel call.
</remarks>
*)

    procedure SetSubRangeLevel(Index: integer; Len: integer = MtxVecEOA); overload;
    (*<summary>Define a subvector of the Src vector.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1 of Src. No copying will occur, only
      pointers will be shifted or indices adjusted.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on the
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised, if an attempt is made to change the size of calling object.
      No exception will be raised, if Src is resized while subranged. (possibly leading to memory overwrites)

      To again obtain a view of the original vector/matrix, call <see cref="SetFullRange"/>
</remarks>
*)

    procedure SetSubRange(const Src: TMtxVecBase; Index: integer; Len: integer = MtxVecEOA); overload;

    (*<summary>Defines a sub vector/matrix of Src and pushes any previous subranges on to a stack.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1 of Src. No copying will occur, only
      pointers will be shifted or indices adjusted. The routine allows for stack based pushing and poping of sub-ranges with
      the additional constraint that each deepening of the sub-range is strictly within the bounds of the previous one.
      The pairs of SetSubRangeLevel/SetFullRangeLevel need to match.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised, if an attempt is made to change the size of calling object.
      No exception will be raised, if Src is resized while subranged. (possibly leading to memory overwrites)

      It is possible to call SetSubRange/SetFullRange on the object, which has been subranged with SetSubRangeLevel.

      To again obtain a view of the previous subrange level on the vector/matrix, call <see cref="SetFullRangeLevel"/>

      The Index parameter of the SetSubRange is always relative to the previous, if any, stack level.
      The Index parameter for the SetSubRangeLevel is relative to the current stack level.
</remarks>
*)
    procedure SetSubRangeLevel(const Src: TMtxVecBase; Index: integer; Len: integer = MtxVecEOA); overload;


  (*<summary>Initializes block processing.</summary>
      
<remarks>Initializes block processing. Because the size of the CPU cache is limited, significant performance gains can be obtained by
      splitting long vectors in to a series of short ones, which can all fit in the CPU cache entirely. The BlockInit method is
      to be used together with <see cref="BlockNext"/> and <see cref="BlockEnd"/> methods to initialize a block processing
      while loop. BlockInit will call <see cref="SetSubRange"/> to obtain subrange of the data in TVec. The <see cref="Length"/>
      of the subranged vector is determined by the global  <see cref="Math387.MtxVecBlockSize"/> variable declared in Math387 unit.
      Default value of MtxVecBlockSize is preset to 800 vector elements for double precision
      and 1600 elements for single precision. BlockInit supports nested calls and from witihin a blocked while loop you can call
      procedures and functions which are also blocked. If you use block processing, typical performance gains will range from
      2 to a maximum of 6. Block processing can not be used, or it is difficult to apply, in cases where vector elements are
      not independent of each other. The block processing while loop must be written like this:

      <code>
      a.BlockInit;
      while not A.BlockEnd do
      begin
        // .... user defined function
      a.BlockNext;
      end;
      </code>
</remarks>


      <Example>
      Normal vectorized procedure:

      <code>
      procedure ParetoPDF(X: TVec; a, b: double; Res: TVec); overload;
      begin
        Res.Size(X);
        Res.Power(x,-(a+1));
        Res.Mul(Power(b,a)*a);;
      end;
      </code>

      Vectorized and blocked version of the Pareto probability distribution procedure:

      <code>
      procedure ParetoPDF(X: TVec; a, b: double; Res: TVec); overload;
      begin
        Res.Size(X);
        Res.BlockInit;
        X.BlockInit;
        while not X.BlockEnd do
        begin
          Res.Power(x,-(a+1));
          Res.Mul(Power(b,a)*a);
          Res.BlockNext;
          X.BlockNext;
        end;
      end;
      </code>

      Alternative:

      <code>
      procedure ParetoPDF(X: TVec; a, b: double; Res: TVec); overload;
      var x1: Vector;
      begin
        Res.Size(X);
        Res.BlockInit;
        X1.BlockInit(X);
        while not X1.BlockEnd do
        begin
          Res.Power(x,-(a+1));
          Res.Mul(Power(b,a)*a);
          Res.BlockNext;
          X1.BlockNext;
        end;
      end;
      </code>

      The block version of the ParetoPDF will execute faster then the non-blocked version in cases
      where X contains 5000-10000 elements or more (double precision). Below that value the two
      versions will perform about the same, except for very short vector sizes (below 50 elements),
      where the non-blocked version will have a slight advantage, because of the absence of block
      processing methods overhead. The time is saved  between the calls  to Res.Power(x,-(a+1))
      and  Res.Mul(Power(b,a)*a), where the same memory (stored in Res vector) is accesed in
      two consecutive calls. That memory is loaded in the CPU cache on the first call, if the
      Length of the Res vector is short enough to fit in. As an excercise you can also try to
      compare the performance of the vectorized and blocked version of the function with the
      single value version (ParetoPDF(X: double; a, b: double; Res: double) and measure
      the execution time of both versions for long vectors (100 000 elements) and short
      vectors (10 elements).

      The differences with block processing strongly depend upon the fatness of individual
      functions. When the relative number of memory accesses is high in compare to amount
      of computation, block processing with thrive.

      </Example>

      <SeeAlso cref="BlockNext"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockInit; overload;  inline; 
    procedure BlockInit(const Src: TMtxVecBase); overload;  inline; 
    (*<summary>Initializes block processing.</summary>*)
    procedure BlockInit(ABlockSize: integer); overload;
    (*<summary>Initializes block processing.</summary>
               
<remarks>Block processing can be applied on possibly already subranged Src object.
               Src may not be freed or go out of scope until block processing loop has finished. There would
               be no error raised other than AV.
</remarks>
*)
    procedure BlockInit(const Src: TMtxVecBase; ABlockSize: integer); overload;
    (*<summary>Deinitializes block processing before the final block is processed.</summary>
               
<remarks>Call this routine before breaking out of a while-loop prematurely.

        <code>

        Vec.BlockInit
        While not Vec.BlockEnd do
        Begin
            If Vec.Min &lt; 0 then //check some condition, if you need to exit
            begin
              Vec.BlockFinish;  //finish the loop
              Break;  //and break out
            end;
            Vec.BlockNext;
        end

        </code>
</remarks>
*)
    procedure BlockFinish;

    (*<summary>Obtains the next subrange of the data.</summary>
      
<remarks>Obtains the next subrange of the data in TVec. The routine must be used together with
      <see cref="BlockEnd"/> and <see cref="BlockInit"/> methods.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockNext;
    (*<summary>Resets subrange defined with SetSubRange.</summary>
      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange; virtual;
    (*<summary>Pops the current subrange stack one level up.</summary>
      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRangeLevel; virtual;

    (*<summary>Prevents calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Prevents calls to <see cref="SetSubRange"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
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
          a.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9});
          a.SetSubRange(0,2);  //a = [1,2]
          a.DisableSubrange();
          a.SetSubRange(2,2); //exception raised here

          b.SetSubRange(a,2,2); //but this will work
          a.SetFullRange(); //b is not changed, it still points to [4,5]
        }
        finally
        {
          MtxVec.FreeIt(ref a, ref b);
        }
      }
    }
    </code></example>

    <SeeAlso cref="DisableSubrange"/>
    <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSubrange;

    (*<summary>Enables calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Enables calls to <see cref="SetSubRange"/> by removing
      the block set by <see cref="DisableSubrange"/>.
</remarks>
*)
    procedure EnableSubrange;

     (*<summary>Copy values to Dst array. The size of the array is set automatically.</summary>
     
<remarks>If the calling object is complex, the size of the Dst array will be equal to 2*<see cref="Length"/>.
      If the calling object is not complex an exception will be raised.
</remarks>
*)
    function CopyToArray(var Dst: TDoubleArray): TMtxVecBase; overload;
    (*<summary>Copy real or complex values from the calling object [Index]..[Index+len-1] to the Dst
       array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. If the calling object is complex,
        the Index and Len parameters define the number of complex elements.
</remarks>
*)
    function CopyToArray(var Dst: TDoubleArray; DstIndex, Index,Len: integer): TMtxVecBase; overload;

    (*<summary>Copies the calling object data to an array of single precision floating point data.</summary>
      
<remarks>Any values exceeding the range are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray): TMtxVecBase; overload; 
    (*<summary>Copy real or complex values from the calling object [Index]..[Index+len-1] to the Dst
       single precision floating point array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       If the calling object is complex, the Index and Len parameters define the number of
       complex elements.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray; DstIndex, Index,Len: integer): TMtxVecBase; overload; 

    (*<summary>Prevents calls to <see cref="Select"/>.</summary>
      
<remarks>Prevents calls to <see cref="Select"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
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
          a.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9});
          a.Select(0,2);  //a = [1,2]
          a.DisableSelect();
          a.Select(2,2); //exception raised here

          b.Select(a,2,2); //but this will work
          a.SelectAll(); //b is not changed, it still points to [4,5]
        }
        finally
        {
          MtxVec.FreeIt(ref a, ref b);
        }
      }
    }
    </code></example>

      <SeeAlso cref="DisableSubrange"/>
      <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSelect;

    (*<summary>Enables calls to <see cref="Select"/>.</summary>
      
<remarks>Enables calls to <see cref="Select"/> by removing
      the block set by <see cref="DisableSelect"/>.
</remarks>
*)
    procedure EnableSelect;

    (*<summary>Resets any defined selection.</summary>
      <SeeAlso cref="Select"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SelectAll; overload;

    
    (*<summary>Reads the header information and the values array content from a stream.</summary>
      
<remarks>Reads the header information and the values array content from the SrcStream. The return value indicates
      the number of elements read.
</remarks>

      <Example>
      <code>
      var b: TVec;
          AStream: TFileStream;
      begin
        CreateIt(b);
        try
          b.SetIt(False,[0,0,1,3,2]);
          AStream := TFileStream.Create('C:\test.Vec',fmCreate);
          try
            b.LoadFromStream(AStream); // Read info and header for b
          finally
            AStream.Free;
          end;
        finally
          FreeIt(b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SaveToStream"/>
      <SeeAlso cref="LoadFromFile"/>*)
    function LoadFromStream(const Src: TStream): Int64; overload; virtual;

    (*<summary>Writes the header information and the Values content to a stream.</summary>
      
<remarks>Writes the header information and the Values content to a DstStream stream.
      Number type is defined by the Precision parameter. Rounding defines the rounding for integer types.
      When saving double precision as single precision, all overflows are saved as INF (-INF). When saving
      to integer types all overflows are clipped. Attempt to save single precision as double precision
      will raise an exception.
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
          b.SaveToStream(AStream); // Write info and header for b
        finally
          AStream.Free;
        end
        FreeIt(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="LoadFromStream"/>
      <SeeAlso cref="WriteHeader"/>
      <SeeAlso cref="WriteValues"/>
      <SeeAlso cref="SaveToFile"/>*)
    procedure SaveToStream(const Dst: TStream); overload; virtual;
    

    (*<summary>Sizes double precision array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: TDoubleArray); overload;
    (*<summary>Size single precision array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: TSingleArray); overload;
    (*<summary>Sizes integer array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: TIntegerArray); overload;

    (*<summary>Sizes integer array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: Math387.TWordArray); overload;
    (*<summary>Sizes integer array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: TSmallIntArray); overload;
    (*<summary>Sizes integer array.</summary>
      
<remarks>If the <see cref="TMtxVec.Complex" text="Complex"/> property is true the Length of the
       Dst array will be 2*<see cref="Length"/>.
</remarks>
*)
    procedure SizeToArray(var Dst: Math387.TByteArray); overload;

    (*<summary> For internal use only. Used to return object to cache. </summary>*)
    procedure Reset; virtual;

    
    procedure Assign(Src: TPersistent);  override; 
    procedure ValidateParams (const Index: integer; var Len: integer); overload; 
    procedure ValidateCplxArrayIndexes (const SrcIndex,SrcLen,Index: integer; var Len: integer);
    procedure ValidateRealArrayIndexes (const SrcIndex,SrcLen,Index: integer; var Len: integer);
    

    (*<summary>Sets vector size to zero.</summary>
       
<remarks>Calling Clear sets <see cref="Length"/> property to 0 and <see cref="TMtxVec.Complex" text="Complex"/> property to False.
</remarks>
*)
    procedure Clear; virtual;
    (*<summary>Frees the object.</summary>
      
<remarks>If the object was created with a call to CreateIt, this routine will call FreeIt.
      If the object was created with a call to constructor (Create), this routine will
      call the destructor (Free).
</remarks>
*)
    procedure FreeToCache; virtual;

    

    





    (*<summary>Selects a set of elements from the vector.</summary>
      
<remarks>The method will define a subarray starting at
      Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted.

      All values of the original <see cref="TMtxVec"/> will be preserved.
      An exception will be raised if an attempt is made to
      change the size of the calling object after a selection has been made.

      A selection will behave exactly as if the object always contained
      only the selected values. When a selection is made from another object
      the source object and all operations done on either of the
      two objects will affect the same elements. The use of selections increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      To again obtain a view of the full vector/matrix use the <see cref="SelectAll"/> method.

      When selecting elements from other objects, care must be given not to
      use the selections after the source object has been freed:

      <code>
      var a,b: TVec;
      begin
          CreateIt(a,b);
          try
              a.SetIt(False,[1,2,3,4]);
              b.Select(a,2,2);

              FreeIt(a); //free the source object

              b.Values[0] := b.Valuess[1]; //b does not point to valid memory anymore
          finally
              FreeIt(a);
          end;
      end;
      </code>
</remarks>
*)
    procedure Select(Index: integer; Len: integer); overload; 

   (*<summary>Resets the selection.</summary>
    
<remarks>This method is the same as the <see cref="SelectAll"/> method.
</remarks>
*)
    procedure Select; overload; 

    (*<summary>Sets the subarray size to full size.</summary>
      
<remarks>This method is the same as the <see cref="SetFullRange"/> method.
</remarks>
*)
    procedure SetSubRange; overload; 

    (*<summary>Defines a subarray.</summary>
      
<remarks>The method will define a sub array starting at
      BeginIndex and ending at EndIndex (inclusive).
      All values of the original vector/matrix will be preserved.
      An exception will be raised if an attempt is made to
      change the size of calling vector/matrix.

      A subarray is array which does not have its own
      memory allocated. Instead it adopts the memory of the
      source object and all operations done on the either of the objects
      affect the same elements. The use of subarrays increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      To again obtain a view of the full vector see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubIndex(BeginIndex, EndIndex: integer); 

    (*<summary>Saves the current value of ConditionCheck property and sets it to false.</summary>
      
<remarks>Saves the current value of <see cref="ConditionCheck"/> property and sets it to false.
      You can restore back to the original setting by calling <see cref="CondEnable"/> method.
</remarks>


      <SeeAlso cref="CondEnable"/>
      <SeeAlso cref="ConditionCheck"/>*)
    procedure CondDisable;
    (*<summary>Sets the ConditionCheck property to whatever it was before the CondDisable was used.</summary>
      
<remarks>Sets the <see cref="ConditionCheck"/> property to whatever it was before the <see cref="CondDisable"/> was used.
</remarks>


      <SeeAlso cref="CondDisable"/>
      <SeeAlso cref="ConditionCheck"/>*)
    procedure CondEnable;

    (*<summary>Write object header and values to a file.</summary>
    
<remarks>Write the header describing the calling object and the values array of the calling object to the file,
    specified by the FileName. If the file already exist, the data is overwritten by default. If Append is
    True, the data is appended to the end of the file. The data is always saved with the default precision
    precision (single or double).

     * In case of '.csv' extension a comma is used as a column delimiter for matrices.
     * In case of '.txt' extension a tab is used as a column delimiter for matrices.
     * For all other extensions, the expected format is binary.

    Note
      It is recommended you use a *.mtx extension when you're saving/loading matrix to/from file. Similarly, use *.vec extension when you're saving/loading vector to/from file.
</remarks>


    <Example>
    <code>
    var Mtx: TMtx;
    begin
      CreateIt(Mtx);
      try
        Mtx.SetIt(2,2,False,[3,1,-1,5]);
        Mtx.SaveToFile('c:\test.mtx'); // Write info and header for Mtx to file
      finally
        FreeIt(Mtx);
      end;
    end;
    </code>
    </Example>

      <SeeAlso cref="LoadFromFile"/>
      <SeeAlso cref="SaveToStream"/>
      <SeeAlso cref="LoadFromStream"/>*)
    procedure SaveToFile(FileName: string; Append: boolean = False);

    (*<summary>Reads the header information and the values array content from the file.</summary>
     
<remarks>Reads the header information and the values array content from the file specified by FileName parameter.

     * In case of '.csv' extension a comma is used as a column delimiter for matrices.
     * In case of '.txt' extension a tab is used as a column delimiter for matrices.
     * For all other extensions, the expected format is binary.
</remarks>


      <Example>
      <code>
      var b: TVec;
      begin
        CreateIt(b);
        try
          b.LoadFromFile('c:\test.Vec'); // Read header and values for b
        finally
          FreeIt(b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SaveToFile"/>
      <SeeAlso cref="LoadFromStream"/>*)
    procedure LoadFromFile(FileName: string);

    (*<summary>Called when object size changes.</summary>
      
<remarks>Called when the <see cref="TMtxVec"/> object size changes.
</remarks>
*)
    property OnSetSize: TMtxNotifyEvent read FOnSetSize write setOnSetSize;
    (*<summary>Set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TVec method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read fIsSubRange;
    (*<summary>Returns true, if the currrent subrange of the vector was also the last subrange in the vector.</summary>
      
<remarks>Returns true, if the currrent subrange of the vector was also the last subrange in the vector.
      This property be used together with <see cref="BlockNext"/> and <see cref="BlockInit"/>.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockNext"/>*)
    property BlockEnd: boolean read get_BlockEnd;
    (*<summary>For internal use only. Read to get the index of this object in object cache.</summary>
               
<remarks>If the object is not from object cache, teh property will return -1.
</remarks>
*)
    property CacheIndex: integer read FCacheIndex write set_CacheIndex;
    (*<summary>Sizing the object will not allocate less than Capacity.</summary>
               
<remarks>Capacity equals CapacityInElements. Rounded up to multiple of 16bytes.

               After changing Capacity, do not assume that any existing values will be preserved.
               Any memory re-allocation needed happens at the next call to Size.
               Capacity cannot be an odd number. When setting an odd number, the Capacity will be increased to the next even number.

               If Capacity specified is less than object cache size, then the value of Capacity is not considered.
</remarks>
*)
    property Capacity: Int64 read GetCapacityInElements write setCapacityInElements;
    (*<summary>Sizing the object will not allocate less than CapacityInBytes.</summary>
               
<remarks>Rounded up to multiple of 16bytes.
</remarks>
*)
    property CapacityInBytes: Int64 read fCapacityInBytes write SetCapacityInBytes;
    (*<summary>Sizing the object will not allocate less than CapacityInElements.</summary>
               
<remarks>Rounded up to multiple of 16bytes. When the storage precision is changed, the CapacityInElements
               will be adjusted accordingly with CapacityInBytes retaining the value.
</remarks>
*)
    property CapacityInElements: Int64 read GetCapacityInElements write SetCapacityInElements;
    (*<summary>Specifies increment step for the Capacity property.</summary>
               
<remarks>If this property is 0, the Capacity will never be modified on its own.
               When the value 1, the capacity will continue increasing to match largest value
               requested. When the value is more than 1, the capacity will be increasing with
               the factor specified.
</remarks>
*)
    property CapacityStep: double read fCapacityStep write setCapacityStep;

    (*<summary>Defines the precision (single or double) of the floating point operations. </summary>*)
    property IsDouble: boolean read fIsDouble write setIsDouble;
    (*<summary>Internal. </summary>
               
<remarks>Used by object-cache to determine the thread pool.
</remarks>
*)
    property PoolIndex: integer read fPoolIndex;
  published

    (*<summary>Defines the length in number of samples.</summary>
      
<remarks>Defines the number of samples that the object can store.
      The length property does not map directly to memory reallocation
      when it is changed. Until the amount of preallocated memory
      is not exceed, there is no reallocation going on.

      Changing the Length property will preserve the existing
      values, but only if the new property value is smaller than the
      amount of preallocated memory. To properly resize the memory allocated
      use the <see cref="TVec.Resize"/> method.

      It is recommended that vector sizes do not exceed the size of
      preallocated memory to increase overall performance. The size
      of preallocated memory can be controlled with the Controller global variable
      and with the Capacity property.
</remarks>
*)
    property Length: integer read fLength write SetLength stored false;
    (*<summary>Enables/disable inline condition checking.</summary>
      
<remarks>Enables/disables inline condition checking. When true, TVec methods perform additional (range)
      checking before operations are executed. The drawback is slight loss of speed. If ConditionCheck is
      set to false then no additional checking is performed. This speeds up the execution but disables some
      safeguards. For short vectors the loss of speed can be significant. The more parameters
      the method requires the more error checking is performed. Some methods (parameterless) don't use the ConditionCheck property at all.
      ConditionCheck property is initially True. This property is an additional safeguard against array overrun or underrun errors.
      It allows you to work with explicit range checking while writing and debugging the application and once your code is running you
      can turn it off. By functionality it is similar to assertions. (See Assert Delphi procedure). You can also use compiler
      directives to check, if assertions are on and then set ConditionCheck to True.
</remarks>


      <SeeAlso cref="CondEnable"/>
      <SeeAlso cref="CondDisable"/>*)
    property ConditionCheck: boolean read fConditionCheck write set_ConditionCheck stored false;
    (*<summary>Caption.</summary>
      
<remarks>Use this property to set/get the <see cref="TMtxVec"/> object string caption.
      This can be usefull for associating description with data when making
      a user interface.
</remarks>
*)
    property Caption: string read FCaption write set_Caption stored false;
    (*<summary>Stores an integer value as part of a TMtxVec object.</summary>
      
<remarks>Stores an integer value as part of a <see cref="TMtxVec"/> object. Tag has no predefined meaning.
      The Tag property is provided for the convenience of developers. It can be used for storing an additional
      integer value or it can be typecast to any 32-bit value such as a component reference or even a pointer.
</remarks>
*)
    property Tag: integer read FTag write set_Tag stored false;
  end;

   
   
   


  TMtxVecSample = class(TMtxVecBase)
  strict protected
    FDataOffset: integer;
    FCDataOffset: integer;
    
    fFloatPrecisionLock: boolean;
  public
    SData: TDoubleArray;
    CData: TCplxDoubleArray;

    SSData: TSingleArray;
    SCData: TCplxSingleArray;



    
    procedure ValidateParams (const X: TMtxVecBase; const XIndex: integer;const Index: integer; var Len: integer); overload; 
    procedure ValidateParams (const X1, X2: TMtxVecBase; const xIndex1,xIndex2,Index: integer; var Len: integer); overload;
    procedure ValidateParams (const X1, X2, X3: TMtxVecBase; const xIndex1, xIndex2, xIndex3, Index: integer; var Len: integer); overload;
    procedure RangeCheck(const Indx: integer); overload;  
    procedure RangeCheckComplex(const Indx: integer); overload;  

    constructor Create; override;
    destructor Destroy; override;
    

    (*<summary>Obtains a pointer to the real value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i]. Under .NET this is a pointer
      to managed memory, which needs to be pinned before the address is taken.
</remarks>


     <SeeAlso cref="PCValues1D"/>
     <SeeAlso cref="TMtxVecBase.Pin"/>
     <SeeAlso cref="TMtxVecBase.UnPin"/>*)
    function PValues1D(const Index: integer): PPDouble; overload; 


    (*<summary>Obtains a pointer to the real value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i]. Under .NET this is a pointer
      to managed memory, which needs to be pinned before the address is taken.
</remarks>


     <SeeAlso cref="PValues1D"/>
     <SeeAlso cref="TMtxVecBase.Pin"/>
     <SeeAlso cref="TMtxVecBase.UnPin"/>*)
    function PSValues1D(const Index: integer): PPSingle; overload; 


    (*<summary>Returns an index pointing to element at Index for double data.</summary>
               
<remarks>The data for the current object may or may not start at index offset 0
               within the internal storage array.
</remarks>
*)
    function SDataIndex(const Index: integer): integer; overload; 

    (*<summary>Obtaines a pointer to the complex value of the vector at Index.</summary>
      
<remarks>The function returns @CValues[i]. Under .NET this is a pointer
      to managed memory, which needs to be pinned before the address is taken.
</remarks>


     <SeeAlso cref="PSCValues1D"/>
     <SeeAlso cref="TMtxVecBase.Pin"/>
     <SeeAlso cref="TMtxVecBase.UnPin"/>*)
    function PCValues1D(const Index: integer): PPCplx; overload; 

    (*<summary>Obtaines a pointer to the complex value of the vector at Index.</summary>
      
<remarks>The function returns @CValues[i]. Under .NET this is a pointer
      to managed memory, which needs to be pinned before the address is taken.
</remarks>


     <SeeAlso cref="PCValues1D"/>
     <SeeAlso cref="TMtxVecBase.Pin"/>
     <SeeAlso cref="TMtxVecBase.UnPin"/>*)
    function PSCValues1D(const Index: integer): PPSCplx; overload; 


    (*<summary>Returns an index pointing to element at Index for TCplx data.</summary>
               
<remarks>The data for the current object may or may not start at index offset 0
               within the internal storage array.
</remarks>
*)
    function CDataIndex(const Index: integer): integer; overload; 

    (*<summary> Returns machine constant depending on value of FloatPrecision property. </summary>*)
    function PrecisionEPS: double; 

    (*<summary>Sqrt(EPS) machine constant depending on value of FloatPrecision property.</summary>
    
<remarks>Square root of EPS.
</remarks>
*)
    function PrecisionSqrtEPS: double; 

    (*<summary>Returns maximum floating point number depending on the value of FloatPrecision property.</summary>
    
<remarks>Maximum floating point number.
</remarks>
*)
    function PrecisionMaxValue: double; 

    (*<summary>Minimum floating point number.</summary>
    
<remarks>Minimum floating point number: <c>2^(-1022)</c>.
</remarks>
*)
    function PrecisionMinValue: double; 
  public
    property FloatPrecisionLock: boolean read fFloatPrecisionLock write fFloatPrecisionLock;
  end;


     TPoolItemRecord = record
       Item: TMtxVecBase;
       Used: boolean;
     end;

     TCacheItem = array of TPoolItemRecord;


     (*<summary> Abstract class for object cache. </summary>*)
     TAbstractMtxVecCache = class(TPersistent)
     strict protected
        Cache: TCacheItem;
        Cache1: TCacheItem;
        FCacheWasFull: boolean;
        Count: integer;
        fCacheUsedCount: integer;
        LowCount: integer;
        fConditionCheck: boolean;
        FCacheSize: integer;
        fCacheElements: integer;





        

        
        procedure SetConditionCheck(const value: boolean);
        procedure Recollect;
        procedure DestroyCache;
        procedure CacheMemFree; virtual;
        function NewItem: TMtxVecBase; virtual;
        procedure CacheMemAllocate(ItemCount, Elements: integer); virtual;
        procedure InternalSetCacheSize;

        function GetCacheFull: boolean;
        function GetCacheInBytes: Int64; virtual;
     protected
        FMaxCount: integer;
     public
     
        csLock: Byte;
        function CreateItx: TMtxVecBase; 
        function LockedCreateItx: TMtxVecBase; 
        procedure FreeIty(const a: TMtxVecBase); overload; 
        procedure LockedFreeIty(const a: TMtxVecBase); overload; 
     
     public
     
        PoolIndex: integer;
        ThreadId: TThreadID;
     
       (*<summary> Returns the number of objects in the object cache. </summary>
                   
<remarks>To change the size of the object cache call SetCacheSize.
</remarks>
*)
        property CacheSize: integer read fCacheSize;
       (*<summary> Returns true, if object cache was too small.  </summary>
                  
<remarks>Only a limited count of items is available in the object cache.
                  If object cache runs out of objects, the code performance could
                  be seriously affected (also by 20x in some cases).
                  Do not allocate objects from cache in large numbers.
</remarks>
*)
        property CacheWasFull: boolean read fCacheWasfull;
       (*<summary> Returns true, if currently there are no free objects in cache.  </summary>*)
        property CacheFull: boolean read GetCacheFull;
        (*<summary>Returns the number of precreated  objects inside object cache.</summary>*)
        property CacheElements: integer read FCacheElements;
        property CacheInBytes: Int64 read GetCacheInBytes;
       (*<summary> Returns the number of currently used objects in the object cache. </summary>*)
        property CacheUsedCount: integer read fCacheUsedCount;
        (*<summary>Read only property. Maximum number of objects used inside object cache.</summary>
           
<remarks>Use this value to trim the size of the object cache for your application.
           Too large cache eats away too much system memory and too small cache can
           result in performance penalties.
</remarks>
*)
        property MaxCount: integer read FMaxCount;

       (*<summary> Sets ConditionCheck for all items in the list. </summary>*)
        property ConditionCheck: boolean read fConditionCheck write SetConditionCheck;

       (*<summary> Sets the cache object count to ItemCount and allocates Elements count of data elements per object. </summary>*)
        procedure SetCacheSize(ItemCount, Elements: integer);
       (*<summary> Call Enter to enable thread exclusive access. </summary> 
       		   
<remarks>Internally this has no effect other than it saves the user
       		   from allocating his own critical section.
</remarks>
*)
        procedure Enter;
	(*<summary> Call Leave to end thread exclusive access. </summary>
				
<remarks>Internally this has no effect other than it saves the user
       		    from allocation its own critical section.
</remarks>
*)
        procedure Leave; 
	(*<summary> Call Enter to enable thread exclusive access. </summary>
       		   
<remarks>Internally this has no effect other than it saves the user
       		   from allocating his own critical section. The function returns
       		   false, if thread exclusive access could not be granted immediately.
</remarks>
*)              
        function TryEnter: boolean;

        procedure DebugReport;
     
        constructor Create; virtual;
        destructor Destroy; override;
     
     end;

     TIntegersList = class
     strict private
        data: TIntegerArray;
        fCount: integer;
        function GetItems(i: integer): integer; inline;
        procedure SetItems(i: integer; const Value: integer); inline;
     public
        property Count: integer read fCount;
        function Add(const aItem: integer): integer;
        procedure Delete(const i: integer);
        procedure Insert(const i: integer);
        procedure Clear;
        property Items[i: integer]: integer read GetItems write SetItems; default;
     end;



    
    
    function LockCmpxchg(CompareVal, NewVal: Byte; AAddress: PByte): Byte;
    
    

    procedure EValidateParams(const Index, Len, Length: integer); overload; 
    procedure EValidateParamsX(const XIndex, Len, xLength: integer); overload;
    procedure EValidateParamsSelf; overload;
    procedure EValidateParamsSelf2; overload;
    procedure EValidateParamsSelf3; overload;
    procedure EValidateParamsX1(const xIndex1, Len, x1Length: integer); overload;
    procedure EValidateParamsX2(const xIndex2, Len, x2Length: integer); overload;
    procedure EValidateParamsX3(const xIndex3, Len, x3Length: integer); overload;

    procedure EValidatePrecisionCheck1(); overload;
    procedure EValidatePrecisionCheck2(); overload;
    procedure EValidatePrecisionCheck3(); overload;

    procedure aSetLength(var A: TSubRangeStack; const Len: integer); overload;
    procedure aSetLength(var A: TCacheItem; const Len: integer); overload;

    function aGetLength(const a: TSubRangeStack): integer; overload;
    function aGetLength(const a: TCacheItem): integer; overload;
    procedure GetCpuInfo(var CpuPhysicalCores, CpuLogicalCores, NumaNodes, L1Cache, L2Cache, L3Cache: integer); overload;
    function GetCPUVendorInfo: string; overload;

    

    procedure dgemm3(const A, B, C: TDoubleArray; const  alpha, beta: Double); overload; inline;
    procedure dgemm3(const A, B, C: TDoubleArray);  overload; inline;
    procedure zgemm3(const A, B, C: TCplxArray; const alpha, beta: TCplx); overload; inline;
    procedure zgemm3(const A, B, C: TCplxArray);  overload; inline;

    procedure dgemm2(const A, B, C: TDoubleArray; const  alpha, beta: Double); overload; inline;
    procedure dgemm2(const A, B, C: TDoubleArray);  overload; inline;
    procedure zgemm2(const A, B, C: TCplxArray; const alpha, beta: TCplx); overload; inline;
    procedure zgemm2(const A, B, C: TCplxArray);  overload; inline;

    procedure sgemm3(const A, B, C: TSingleArray; const  alpha, beta: Single); overload; inline;
    procedure sgemm3(const A, B, C: TSingleArray);  overload; inline;
    procedure cgemm3(const A, B, C: TSCplxArray; const alpha, beta: TSCplx); overload; inline;
    procedure cgemm3(const A, B, C: TSCplxArray);  overload; inline;

    procedure sgemm2(const A, B, C: TSingleArray; const  alpha, beta: Single); overload; inline;
    procedure sgemm2(const A, B, C: TSingleArray);  overload; inline;
    procedure cgemm2(const A, B, C: TSCplxArray; const alpha, beta: TSCplx); overload; inline;
    procedure cgemm2(const A, B, C: TSCplxArray);  overload; inline;

    procedure zgemm2x(const A, B, C: TDoubleArray); overload; inline;
    procedure zgemm2x(const A, B, C: TDoubleArray;  const alphaR, alphaI, betaR, betaI: Double); overload; inline;

    procedure cgemm2x(const A, B, C: TSingleArray); overload; inline;
    procedure cgemm2x(const A, B, C: TSingleArray;  const alphaR, alphaI, betaR, betaI: single); overload; inline;

    procedure zgemm3x(const A, B, C: TDoubleArray); overload;
    procedure zgemm3x(const A, B, C: TDoubleArray;  const alphaR, alphaI, betaR, betaI: Double); overload;

    procedure cgemm3x(const A, B, C: TSingleArray); overload;
    procedure cgemm3x(const A, B, C: TSingleArray;  const alphaR, alphaI, betaR, betaI: single); overload;


var

     MtxVecFileCode: word = $FA13;
     MtxVecTVecVer: integer = -1;
     MaxSubrangeLevel: integer = 4;



