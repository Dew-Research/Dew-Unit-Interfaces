










(*<summary>Implements TVecInt object.</summary>
  
<remarks>Implements integer math object TVecInt and supporting
  routines <see cref="TVecInt"/>
</remarks>
*)
unit MtxVecInt;


{$I BdsppDefs.inc}

interface

uses Math387, MtxVecBase, AbstractMtxVecInt
    
    ,Classes
    ,SysUtils
    
    
    
    
    
    ;

type

    TMtxInt = class;

    (*<summary>Intermediate class to allow insertion of helper classes for TMtxVecInt. </summary>*)
    TAbstractMtxVecInt = class(TMtxVecInt);


  (*<summary>Class for one dimensional integer arrays.</summary>
      
<remarks>Class for accessing and manipulating 8bit unsigned and 16 or 32 bit signed one dimensional arrays
      of integers and bits. Purpose of usage:

      * leverage the SSEX/AVX instruction sets for integers
      * take advantage of reduce storage size and fast bit (and, or, xor, not) operations
      * provide basic (saturation) math support. Saturation math means that adding two integers
        whose result would be greater than the integer range will result in clipping of
        the result to the maximum value representable by the integer type.
      * support for super-conductive object cache allows efficient multi-core threading
        of algorithms using this type.
      * Pack and unpack bit arrays for much faster boolean processing.

      The same object type can be used to work with three different integer types and
      integer precision is never changed implicitely. Mixing of different integer
      types or mixing with real types is not supported to avoid hidden cost of repeated conversions.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVecInt a = new TVecInt();
          a.SetIt(new int[] {1,2,4,3});
          a.SortAscending();  // a = 1,2,3,4
        }
      }
     </code></example>*)

  
  
  
  TVecInt = class(TAbstractMtxVecInt  )
  strict private

    function InternalResize(Len: integer; ZeroIt: boolean): TVecInt;
    function GetIntegerValues: string;

  
  
  strict protected

    function Int32CachePointer: TIntegerArray; override;
    function Int16CachePointer: TSmallIntArray; override;
    function Int8CachePointer: Math387.TByteArray; override;
    function Int32CacheOffset: integer; override;
    function Int16CacheOffset: integer; override;
    function Int8CacheOffset: integer; override;

    procedure SetSubRangeDirect(Index: integer; Len: integer); override;
    procedure SetSubRangeDirect(const Src: TMtxVecBase; Index: integer; Len: integer); override;
    procedure SubRangeSaveState; override;

    procedure HookPointers; override;
    function  CacheLength: integer; override;
    function  CacheLengthInBytes: Int64; override;
    procedure setActualSize(const byteCount: Int64); override;
    procedure SetIntPrecision(const value: TIntPrecision); override;

    function get_Bits(const Indx: Int64): boolean; 
    procedure set_Bits(const Indx: Int64; const value: boolean); 
    function StringsToValuesInternal(const srcList: TStrings; const aDelimiter: string): TMtxVecBase; overload; override;
    function ValuesToStringsInternal(const dstList: TStrings; const aDelimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload; override;


 

    protected 
    function get_DefaultArray(const Indx: integer): integer; 
    procedure set_DefaultArray(const Indx, value: integer); 

    procedure set_BitCount(const value: Int64);
    function get_BitCount: Int64;
  



  strict protected

    procedure SetLength(const Value: integer); override;
  public

    
    function ToolTip: string;

    constructor Create; overload; override;

    
    procedure Adopt(AArray: PAPointer; ALength: integer; Precision: TIntPrecision); overload;
    procedure Disown(out AArray: PAPointer; out ALength: integer); overload;
    
    procedure Assign(Src: TPersistent); override;

    procedure Disown(); overload;

    

    

    

    (*<summary>Releases this object back to cache.</summary>
      
<remarks>This method does the same as the <see cref="FreeIt"/> routine.
</remarks>


      <SeeAlso cref="FreeIt"/>*)
     procedure FreeToCache; override;

    (*<summary>Sets size to 0 and releases associated memory.</summary>*)
     procedure Clear; override;

     (*<summary>Cumulative sum.</summary>
      
<remarks>Calculate the cumulative sum for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: TVecIn;
      begin
        CreateIt(a);
        try
          a.SetIt([1,2,3,4]);
          a.CumSum; // a = [1,3,6,10]
        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

        <SeeAlso cref="Sum"/>*)
      function CumSum: TVecInt; overload;

      (*<summary>Calculate the cumulative sum for all Vec elements.</summary>
        
<remarks>Store the results in calling object. Size and  <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of
        the calling object are set implicitly.
</remarks>
*)
      function CumSum(const Vec: TVecInt): TVecInt; overload;

        (*<summary>Concatenates an array of TVecInt objects.</summary>
        
<remarks>Concatenates an array of TVecInt objects. The method copies the contents of all TVecInt objects from the Src
        array to the calling object. The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex">Complex</see>
        properties of the calling vector are set implicitly. An exception is raised, if Complex properties
        of TVecInt objects do not match.
</remarks>


        

        <example>
        <code>
        using Dew.Math;
        using Dew.Math.Units;

        namespace Dew.Examples
        {
          void Example()
          {
            TVecInt a,b,c,d;
            MtxVec.CreateIt(out a, out b, out c, out d);
            try
            {
              a.SetIt(new int[] {1,2,3});
              b.Copy(a);
              c.Concat([a,b]); // c = [1,2,3,1,2,3]
              d.Size(10);
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


        <SeeAlso cref="Copy"/>
        <SeeAlso cref="TMtxVec.Copy"/>*)
      function Concat(const Src: array of TVecInt): TVecInt; overload;
      (*<summary>Copies the contents of all TVecInt objects from the Src array to the calling vector elements, starting
        with at Index.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVec.Complex"/>
        properties of the calling vector must be set explicitly. An exception is raised, if Complex
        properties of TVecInt objects do not match or if the sum of
        Length's exceeds the Length property of the calling object.
</remarks>
*)
      function Concat(Index: integer; const Src: array of TVecInt): TVecInt; overload;

    (*<summary>Copies the k-th diagonal from the TMtxInt object.</summary>
      
<remarks>Copies the k-th diagonal from the TMtxInt object. If <c>k = 0</c> then the main diagonal is copied,
      if <c>k &lt; 0</c> then the subdiagonal is copied and if <c>k &gt; 0</c> then the k-th super diagonal is copied to the calling vector.
</remarks>


      <Example>In the following example we setup a matrix, populate it with values and then extract it's main diagonal to a vector.
      <code>
      var a: TVecInt;
          d: TMtxInt;
      begin
        CreateIt(a);
        CreateIt(d);
        try
          // setup matrix
          d.SetIt(2,2,[1,-2,
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
    function Diag(const Mtx: TMtxInt; k: integer): TVecInt;

    (*<summary> Stores distinct values from Src in to Dst. </summary>*)
    function Distinct(const Src: TMtxVecInt): TVecInt;

      (*<summary>The difference between two succesive vector elements.</summary>
        
<remarks>Calculate the difference for all calling vector elements. The following formula is used to
        calculate the difference:

        <IMG namr="tvec19"/><para/>

        The Length of calling vector is automatically decremented by one.
</remarks>
*)
      function Difference(Lag: Integer = 1): TVecInt; overload;
      (*<summary>Calculate the difference for all Vec elements.</summary>
        
<remarks>Store the results in the calling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
        of the calling vector is set to one less the length of Vec.
</remarks>
*)
      function Difference(const Vec: TMtxVecInt; Lag: Integer = 1): TVecInt; overload;




















    (*<summary>Finds and gathers vector elements.</summary>
      
<remarks>Fills the Indexes vector with indexes, of those elements where the Op
      comparison between a and b is True.  Op string parameter can be '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;'.
      The method also copies or "gathers" the matched elements to the calling vector.
      Both the calling vector and Indexes will be sized to match a.Length. On return, both will be subranged
      to reflect actual number of matching elements. The method will not raise an exception, if the calling vector (Self)
      or Indexes parameter are already subranged.
</remarks>
*)

    (*<summary>The b parameter is of integer type.</summary>*)
    function FindAndGather(const a: TMtxVecInt; const op: string; const b: integer; const Indexes: TVecInt = nil): TVecInt; overload;
    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindAndGather(const a: TMtxVecInt; const op: string; const b: TMtxVecInt; const Indexes: TVecInt = nil): TVecInt; overload;



      (*<summary>Copies a column from matrix.</summary>
        
<remarks>Copies the Col-th column from Mtx matrix to calling vector. The <see cref="TMtxVecBase.Length" text="Length"/>
        and <see cref="TMtxVecInt.IntPrecision"/> properties of calling vector are adjusted automatically.
        An exception is raised if condition checking is enabled and Col is greater than <c>Mtx.Cols-1.</c>
</remarks>


        <Example>
        <code>
        var a: TVecInt;
            b: TMtxInt;
        begin
          CreateIt(a);
          CreateIt(b);
          try
            b.SetIt(2,2,[1,2,
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
      function GetCol(const Mtx: TMtxInt; Col: integer): TVecInt; overload;
      (*<summary>Copy the Col-th column elements [Row]..[Row+Len-1] to calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>The <see cref="TMtxVecInt.IntPrecision" text="Complex"/> property of calling vector must be set explicitly. An exception is raised if condition checking is
        enabled and column array borders are overrun.
</remarks>
*)
      function GetCol(const Mtx: TMtxInt; Row, Col, Index, Len: integer): TVecInt; overload;

      (*<summary>Copy the Col-th column elements [Row]..[Row+Len-1] to calling vector.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling
        vector are adjusted automatically. An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
      function GetCol(const Mtx: TMtxInt; Row, Col, Len: integer): TVecInt; overload;

      (*<summary>Copies a row from matrix.</summary>
        
<remarks>Copies the Row-th row from Mtx matrix to calling vector. The <see cref="TMtxVecBase.Length"/> and
        <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling vector are adjusted automatically.
        An exception is raised if condition checking is enabled and Row is greater than <c>Mtx.Rows-1.</c>
</remarks>


        <Example>
        <code>
        var a: TVecInt;
            b: TMtxInt;
        begin
          CreateIt(a);
          CreateIt(b);
          try
            b.SetIt(2,2,[1,2,
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
      function GetRow(const Mtx: TMtxInt; Row: integer): TVecInt; overload;
      (*<summary>Copy the Row-th column elements [Col]..[Col+Len-1] to calling vector elements [Index]..[Index+Len-1].</summary>
        
<remarks>The <see cref="TMtxVecInt.IntPrecision" text="Complex"/> property of calling vector must be set
        explicitly. An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
      function GetRow(const Mtx: TMtxInt; Row, Col, Index, Len: integer): TVecInt; overload;
      (*<summary>Copy the Row-th column elements [Col]..[Col+Len-1] to calling vector.</summary>
        
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling
        vector are adjusted automatically. An exception is raised if condition checking is
        enabled and column array borders are overrun.
</remarks>
*)
      function GetRow(const Mtx: TMtxInt; Row, Col, Len: integer): TVecInt; overload;

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

        See the <see cref="TMtxVecInt.Scatter"/> method to see how to undo the gathering.

        The performance of the CPU heavily depends on the assumption that elements are stored at consecutive memory locations.
        If it is neccessary to apply a set of operations only to elements at specific indexes, performance-wise it can
        proof to be very helpfull, if the elements are gathered first.
</remarks>


        <SeeAlso cref="Gather"/>
        <SeeAlso cref="TMtxVecInt.Scatter"/>*)
      function Gather(const X: TMtxVecInt; const Indexes: TVecInt = nil; IndexType: TIndexType = indVector; Increment: integer = 1; Offset: integer = 0): TVecInt; overload;

      (*<summary> Gathers elements from X starting at Offset and with Increment to the calling object. </summary>*)
      function GatherByIncr (const X: TMtxVecInt; Increment: integer = 1; Offset: integer = 0): TVecInt; overload;

      (*<summary> Gathers elements from X specified by indices stored in Indexes the calling object. </summary>*)
      function GatherByIndex(const X: TMtxVecInt; const Indexes: TVecInt): TVecInt; overload;

      (*<summary> Gathers elements from X specified by non-zero values stored in the Mask to the calling object. </summary>*)
      function GatherByMask (const X: TMtxVecInt; const Mask: TMtxVecInt; const MaskNonZeroCount: integer = -1): TVecInt; overload;

      (*<summary> Counts groups of equal numbers within Bins. </summary>
                   
<remarks>The maximum integer value within SrcBins parameter defines the Size of the calling vector.
                   Within each IValues[Idx] of the result will be number of Bin items, which have value equal to "Idx".
                   The size of self is set to SrcBins.Max+1. SrcBinsMax with default value of -1 indicates that Bins.Max is
                   computed internally. The routine does not perform additional range checking, if BinsMax is provided
                   explicitely (memory overwrite is possible, if indexes are less than zero or more than calling vector length).
</remarks>
*)
      function GroupCount(const SrcBins: TMtxVecInt; const SrcBinsMax: integer =  -1): TVecInt; overload;
      (*<summary> Counts groups of equal numbers within Bins. </summary>
                   
<remarks>The routine checks the size of the calling object, if it is equal to SrcBinsMax+1, and raises
                   an exception, if not. The rouine does not size the calling object and also does not perform initialization to zero.
                   It can be used for iterative calls to fill the same histogram.
</remarks>
*)
      function GroupCountIteration(const SrcBins: TMtxVecInt; const SrcBinsMax: integer): TVecInt; overload;

      (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
      function ScatterByMask(const Src: TMtxVecInt; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVecInt; overload; override;

      (*<summary>Packs integer storage vector to bit storage. </summary>
                 
<remarks>All elements are checked only, if they are zero or not.
</remarks>
*)
      function BitPack(const Src: TVecInt): TVecInt; overload;

       (*<summary>Unpacks bit storage to specified integer storage.</summary>*)
      function BitUnpack(const Src: TVecInt; const dstPrecision: TIntPrecision = prInt32): TVecInt; overload;


     (*<summary>Applies median filter with size Mask to the calling vector.</summary>
              
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
              vector with the median value, calculated over the fixed range (mask) centered around that element.
</remarks>
*)
     function Median(MaskSize: integer): TVecInt; overload;

     (*<summary>Applies median filter with size Mask to data in Src and stores the result in the calling vector.</summary>
              
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
              vector with the median value, calculated over the fixed range (mask) centered around that element
              and stores the result in the calling vector.
</remarks>
*)
     function Median(const Src: TMtxVecInt; MaskSize: integer): TVecInt; overload;

    (*<summary>Fills the calling vector with a series following linear rule.</summary>
        
<remarks>Fills the calling vector with a series following the rule:

        <code>
        Values[k] :=  k
        </code><para/>

        (Offset is zero and Step is one).
</remarks>


        <Example>
        <code>
        var a: TVecInt;
        begin
          CreateIt(a);
          try
            a.Size(5,prInt32);
            a.Ramp(0,2); // [0, 2, 4, 6, 8]
          finally
            FreeIt(a);
          end;
        end;
        </code><para/>
        which is identical to:<para/>
        <code>
        CreateIt(a);
        try
          a.Size(5,prInt32);
          for i:= 0 to a.Length-1 do a[i] := i*2;
        finally
          FreeIt(a);
        end;
        </code>
        </Example>

        <SeeAlso cref="TMtxVecInt.SetVal"/>*)
      function Ramp: TVecInt; overload;
      (*<summary>Fills the calling vector.</summary>
        
<remarks>Method uses the following rule:

        <code>
        Values[k] := Offset + k*Step.
        </code><para/>
</remarks>
*)
      function Ramp(Offset, Step: double): TVecInt; overload;

      (*<summary>Fills the calling vector elements with linear rule and randomly shuffles the indexes.</summary>
        
<remarks>Fills the calling vector with a series following the rule:

        <code>
        Values[k] :=  k
        </code><para/>

        and then scrambles the contents of the vector randomly by using the
        Fisher–Yates shuffle.
</remarks>
*)

      function RandomRamp: TVecInt; overload;

      (*<summary>Randomly shuffles the content of the vector.</summary>
        
<remarks>Scrambles the contents of the vector randomly by using the
        Fisher–Yates shuffle.
</remarks>
*)

      function RandomShuffle: TVecInt; overload;

      (*<summary>Resizes vector size while preserving values.</summary>
        
<remarks>Resizes calling vector <see cref="TMtxVecBase.Length" text="Length"/> to Len and fills it with Src vector
        first Len values. If Src length is less than Len and ZeroIt parameter is true, the remaining
        calling vector values are set to zero.
</remarks>


        <Example>
        <code>
        var a, b: TVecInt;
        begin
          CreateIt(a,b);
          try
            a.SetIt([1,2,3]);
            b.SetIt([9]);
            b.Resize(a,7,True); // b=(9,1,2,3,4,0,0,0)
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>*)
      function Resize(const Src: TVecInt; Len: integer; ZeroIt: boolean = False): TVecInt; overload;

      (*<summary>Resizes calling vector Length to Len.</summary>
         
<remarks>If <see cref="TMtxVecBase.Length" text="Length"/> is less than Len and ZeroIt parameter is true, the remaining
        calling vector values are set to zero.
</remarks>


        <Example>
        <code>
        var a: TVecInt;
        begin
          CreateIt(a);
          try
            a.SetIt([1,2,3]);
            a.Resize(7,True); // a= [1,2,3,0,0,0,0]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>*)
      function Resize(Len: integer; ZeroIt: boolean = False): TVecInt; overload;

    

      

      
      function  ReadHeader(const SrcStream: TStream; Endian: TEndianness = MtxSystemEndianness): TPrecision; overload; override;
      procedure WriteHeader(const DstStream: TStream; aPrecision: TPrecision = prInteger; Endian: TEndianness = MtxSystemEndianness); overload; override;
      

    

      procedure Reset; override;

     (*<summary>A cyclic shift on all calling vector elements.</summary>
       
<remarks>Performs cyclic shift on all calling vector elements. Offset can be
       any integer number, positive or negative.
</remarks>


       <SeeAlso cref="Shift"/>*)
     function Rotate(Offset: integer): TVecInt; overload;
     (*<summary>A cyclic shift on vector elements in range.</summary>
        
<remarks>Performs cyclic shift on all source vector elements and stores them to
        calling vector elements [Index .. Index+Len-1]. The number of elements to
        shift is specified in the Offset parameter. Offset can be any integer
        number, positive or negative.

        An exception is raised if array borders are overrun/underrun.
</remarks>

        <SeeAlso cref="Shift"/>*)
     function Rotate(const Src: TMtxVecInt; Offset: integer): TVecInt; overload;

        (*<summary>Reverse vector elements.</summary>
          
<remarks>The method reverses vector elements by using the following equation:

          <IMG name="TVec24"/><para/>

          This overload reverses all calling vector elements in-place.
</remarks>


          <Example>
          <code>
          var a: TVecInt;
          begin
            CreateIt(a);
            try
              a.SetIt([1,2,3,4]);
              a.Reverse;   // a = [4,3,2,1]
            finally
              FreeIt(a);
            end;
          end;
          </code>
          </Example>

          <SeeAlso cref="Rotate"/>
          <SeeAlso cref="Shift"/>*)
        function Reverse: TVecInt; overload;
        (*<summary>Reverse all Vec elements.</summary>
          
<remarks>Xtore the result in the calling vector elements. The <see cref="TMtxVecBase.Length"/>
          property of the calling vector are
          set implicitly to match Vec vector.
</remarks>
*)
        function Reverse(Vec: TMtxVecInt): TVecInt; overload;



      (*<summary>Sets object values.</summary>
        
<remarks>Set object values. Method call does not change object's size, but it does
        check for array overrun. The elements of A array are copied to the calling
        object elements, starting at Index. The IntPrecision property of the calling
        object is not changed.
</remarks>
*)
      function SetIt(Index: integer; const A: array of integer): TVecInt; overload;
      (*<summary>Sets object values.</summary>
        
<remarks>Set object values. The size of the calling object is adjusted to match the length
        of the array. The elements of A array are copied to the calling
        object elements. The IntPrecision property of the calling object is not changed.
</remarks>
*)
      function SetIt(const A: array of integer): TVecInt; overload;
      function SetInteger(const A: array of integer): TVecInt; overload;


      (*<summary>Defines a sub-vector.</summary>
        
<remarks>Define a subvector of the Src vector starting at BeginIndex and ending at EndIndex (inclusive).
</remarks>


        <SeeAlso cref="SetSubRange"/>
        <SeeAlso cref="TMtxVecBase.SetFullRange"/>*)
      procedure SetSubIndex(const Src: TMtxVecInt; BeginIndex, EndIndex: integer); overload;

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


          <SeeAlso cref="TMtxVecBase.SetSubRange"/>
          <SeeAlso cref="TMtxVecBase.SetFullRange"/>*)

      procedure SetSubRange(const Src: TMtxVecInt); overload;

      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TCplxArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TDoubleArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TSingleArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TIntegerArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the Length of the calling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: TSmallIntArray); overload;
      (*<summary>Sets the size of the vector to match an array.</summary>
        
<remarks>Sets the size (Length) of the caling vector to match the length of the array.
        IntPrecision property value is preserved.
</remarks>
*)
      procedure SizeFromArray(const Src: Math387.TByteArray); overload;


     (*<summary>Do a shift on all calling vector elements.</summary>
       
<remarks>Performs shift on all calling vector elements. Offset can be
       any integer number, positive or negative.
</remarks>


       <SeeAlso cref="Rotate"/>*)
     function Shift(Offset: integer): TVecInt; overload;

     (*<summary>Do a shift on vector elements in range.</summary>
        
<remarks>Performs shift on all source vector elements and stores them to
        calling vector elements [Index .. Index+Len-1]. The number of elements to
        shift is specified in the Offset parameter. Offset can be any integer
        number, positive or negative.

       An exception is raised if array borders are overrun/underrun.
</remarks>

        <SeeAlso cref="Rotate"/>*)
     function Shift(const Src: TVecInt; Offset: integer): TVecInt; overload;


  
  
  
  
     (*<summary>Sets the size and storage precision of the calling object.</summary>*)
     function Size(aLength: integer; aPrecision: TIntPrecision = prInt32): TVecInt     ; overload;

      
      function Size(const Src: TMtxVecBase; aPrecision: TIntPrecision): TMtxVecInt ; override;
      function Size(const Src: TMtxVecBase): TMtxVecInt ; override;
      

     (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
        <SeeAlso cref="SortDescend"/>*)
     function SortAscend: TVecInt; overload;

     (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
        <returns>calling vector, storing sorted values.</returns>
        <param name="DstSortIdx">After execution stores sorted values indices.</param>
        
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
     function SortAscend(DstSortIdx: TVecInt): TVecInt; overload;


     (*<summary>Sort all calling vector elements in descending order in-place.</summary>

        <SeeAlso cref="SortAscend"/>*)
     function SortDescend: TVecInt; overload;

     (*<summary>Sort all calling vector elements in descending order in-place.</summary>
        <Returns>calling vector, storing sorted values.</Returns>
        <param name="DstSortIdx">After execution stores sorted values indices.</param>
        
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
     function SortDescend(DstSortIdx: TVecInt): TVecInt; overload;

       (*<summary>Convert strings in srcList to integers.</summary>
        
<remarks>Convert strings in srcList to integers according to the IntPrecision property
        and store them in the Values array of the calling vector.
        The <see cref="TMtxVecBase.Length"/> of the calling vector is set to
        srcList.Count.
</remarks>


        <Example>
        <code>
        var a,b: TVecInt;
        begin
          CreateIt(a,b);
          try
            a.SetIt([1,2,3,4]);
            a.Add(1)
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
      (*<summary>Convert strings in srcList to integers starting at ListIndex.</summary>
        
<remarks>Store them in the Values array of the calling vector from Index to Index+Len-1.
        The Length property of the calling vector is not changed.
        If array bounds are overrun and exception is raised.
</remarks>
*)
      function StringsToValues(const srcList: TStrings; ListIndex: integer; Index: integer = 0; Len: integer = MtxVecEOA): TVecInt; overload;

      (*<summary>Converts the content of the Values array of the calling vector to a list of strings.</summary>
        
<remarks>Converts all elements of the calling vector to strings with specified formating
        and stores them in dstList, by using the Add method of TStrings object. Values are appended to existing content of dstList.
        Set Equidistant to True, if you are using fixed width font and need to have the text right aligned.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


        <Example>
        <code>
        procedure TForm1.Button1Click(Sender: TObject);
        var a,b: TVecInt;
        begin
          CreateIt(a,b);
          try
            a.SetIt([1,2,3,4]);
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
      function ValuesToStrings(const dstList: TStrings;
                                const Align: TFixedTextAlign = ftaNone;
                                const Headers: boolean = false): integer; overload;

      (*<summary>Convert elements from Index to Index+Len-1 of the calling vector to strings.</summary>
        
<remarks>Store values in dstList starting at ListIndex. If dstList is not large enough,
        the method will use the add method of dstList object. Set Equidistant to True, if you are using fixed width font
        and need to have the text right aligned.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
      function ValuesToStrings(const dstList: TStrings; ListIndex, Index,  Len: integer;
                                const Align: TFixedTextAlign = ftaNone;
                                const Headers: boolean = false): integer; overload;

      (*<summary>Convert all vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String); overload;
      (*<summary>Convert Index..Index+Len-1 vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String; Index,  Len: integer); overload;

      

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
      function Parse(const Src: string): TVecInt;

      (*<summary>Returns content as a string suitable for display as a tooltip.</summary>*)
      property IntegerValues: string read GetIntegerValues;
  public
      
      (*<summary>Allows reading/writing individual bits of the array.</summary>
                 
<remarks>Use BitPack and BitUnpack for much faster access to bit data.
</remarks>
*)
      property Bits[const Indx: Int64]: boolean read get_Bits write set_Bits;
      

      (*<summary>Allows sizing the internal storage to accomodate desired number of bits.</summary>
       
<remarks>Use BitPack and BitUnpack for much faster access to bit data or Bits[i] property for individual
       bit access. Setting BitCount will also call the Size method. The Length of the internal arrays will be adjusted
       so that all bits can be stored with padding of the last 32bit integer element.
       The value of BitCount, which is not an integer multiple of the IntPrecision is preserved only
       by certain methods where applicable and where it is not ambiguous. The value of the property affects
       only the results of BitPack and BitUnpack routines.
</remarks>
*)
      property BitCount: Int64 read get_BitCount write set_BitCount;


      (*<summary>Allows setting/getting the integer value at position Indx.</summary>
        
<remarks>Allows setting/getting the 32bit integer value at position Indx.
        In Delphi, this property reads/writes to the same memory as <see cref="TMtxVecInt.IValues1D"/>, <see cref="TMtxVecInt.SValues1D"/> and <see cref="TMtxVecInt.BValues1D"/> properties.
</remarks>
*)
      property DefaultArray[const Indx: integer]: integer read get_DefaultArray write set_DefaultArray; default;
  end;

   (*<summary>List of TVecInt objects.</summary>
      
<remarks>List of <see cref="TVecInt"/> objects.
      The list manages the object list. The TVec objects are created
      and destroyed by the list. List size can be controled with the Count property.
</remarks>
*)
   TVecIntList = class(TObjectsList)
   strict private
      function GetItems(i: integer): TVecInt;
      procedure SetItems(i: integer; const Value: TVecInt);
      procedure SetCount(const Value: integer); reintroduce;
      function GetCount: integer; reintroduce;
   public
























      (*<summary>Creates and adds a new TVecInt object to the list.</summary>
        
<remarks>The function returns the Index at which the newly created object resides in
         the list.
</remarks>
*)
      function Add: integer; overload; virtual;
      (*<summary>Creates and adds a new TVecInt object to the list.</summary>
        
<remarks>The function returns the Index at which the object resides in the list.
        The object will not be owned (freed by the List), when the list is destroyed
        if Owned parameter remains false.
</remarks>
*)
      function Add(const Src: TVecInt): integer; overload; virtual;
      (*<summary>Assigns the source to the calling object.</summary>
        
<remarks>All the data is copied.
</remarks>
*)
      procedure Assign(Source: TVecIntList); virtual;

      procedure SetFullRange;

      destructor Destroy; override;

      
      (*<summary>Save the list to a stream.</summary>*)
      procedure SaveToStream(const DstStream: TStream); overload; virtual;
      (*<summary>Load the list from a stream.</summary>*)
      procedure LoadFromStream(const SrcStream: TStream); overload; virtual;
      

      

      
      property Item[i: integer]: TVecInt read GetItems write SetItems; default; 
      property Count: integer read GetCount write SetCount; 
      
   end;


  (*<summary>Class for 16bit one dimensional integer arrays.</summary>
      
<remarks>Class for accessing and manipulating 16bit one dimensional arrays.
      It handles memory allocation and basic vector arithmetics.
</remarks>
*)
  
  TVecSmallInt = class(TVecInt)
  strict protected
    procedure HookPointers; override;




 private 
    function get_DefaultArray(const Indx: integer): SmallInt; 
    procedure set_DefaultArray(const Indx: integer; const value: SmallInt); 
  public
    

    
    (*<summary>Allows setting/getting the 16bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 16bit integer value at position Indx.
</remarks>
*)
    Values: TSmallIntArray;
    

    (*<summary>Allows setting/getting the integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 16bit integer value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> property.
</remarks>
*)
    property DefaultArray[const Indx: integer]: SmallInt read get_DefaultArray write set_DefaultArray; default;  

    



    

    function PValues(Index: integer): PAPointer; override;
    
    constructor Create; override;
    
  end;

  (*<summary>Class for 8bit one dimensional integer arrays.</summary>
      
<remarks>Class for accessing and manipulating 8bit one dimensional arrays.
      It handles memory allocation and basic vector arithmetics.
</remarks>
*)

  
  TVecByte = class(TVecInt)
  strict protected
    procedure HookPointers; override;




 private 
    function get_DefaultArray(const Indx: integer): Byte; 
    procedure set_DefaultArray(const Indx: integer; const value: Byte); 
  public
    

    
    (*<summary>Allows setting/getting the 8bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 8bit integer value at position Indx.
</remarks>
*)
     Values: Math387.TByteArray;
    

    (*<summary>Allows setting/getting the integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 8bit value at position Indx.
      This property reads/writes to the same memory as <see cref="Values"/> property.
</remarks>
*)
    property DefaultArray[const Indx: integer]: Byte read get_DefaultArray write set_DefaultArray; default; 








    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns address of element at Values[i]. Under .NET, internal memory
      must be Pinned first. See <see cref="TMtxVecBase.Pin" text="Pin"/> and <see cref="TMtxVecBase.UnPin" text="UnPin"/>
</remarks>
*)
    function PValues(Index: integer): PAPointer; override;

    
    constructor Create; override;
    

  end;

 (*<summary>Two dimensional array - matrix.</summary>
   
<remarks>Use the matrix class to perform matix operations like multiplication
   addition, subtraction, solve a system of linear equations, compute
   eigenvalues, singular value decomposition, least squares solution,
   2D FFT's, parallel 1D FFT's and more.
</remarks>
*)

  
  
  
  TMtxInt = class(TAbstractMtxVecInt  )
  strict private
    FRows: integer;
    FCols: integer;
    fColsBitCount: integer;
    procedure SetRows(const value: integer);
    procedure SetCols(const value: integer);
    function  GetIntegerValues: string;
    procedure SetColsBitCount(const value: integer);
    procedure ValidateRange (Row, Col, ToRow, ToCol: integer);

  

  strict protected
    FLeadingCols: integer;
    SRows, SCols: integer;
    SLeadingCols: integer;

    function CacheLength: integer; override;
    function CacheLengthInBytes: Int64; override;
    procedure HookPointers; override;

    function Int32CachePointer: TIntegerArray; override;
    function Int16CachePointer: TSmallIntArray; override;
    function Int8CachePointer: Math387.TByteArray; override;
    function Int32CacheOffset: integer; override;
    function Int16CacheOffset: integer; override;
    function Int8CacheOffset: integer; override;

    procedure SetIntPrecision(const value: TIntPrecision); override;
    function LowerTriangleInternal(const Mtx: TMtxInt; ZeroUpper, Diagonal: boolean): TMtxInt; overload;
    function UpperTriangleInternal(const Mtx: TMtxInt; ZeroLower, Diagonal: boolean): TMtxInt; overload;

    procedure SetLeadingCols(const value: integer);
    function GetLeadingCols: integer; 

    procedure SubRangeSaveState; override;
    procedure SubRangeLoadState; override;
    procedure SetSubRangeDirect(Index, Len: integer); override;
    procedure SetSubRangeDirect(const Src: TMtxVecBase; Index, Len: integer); override;
    procedure SetSubRangeDirect2(const Src: TMtxVecBase; Index: Integer; aRows, aCols: Integer);
    procedure SetLength(const value: integer); override;

    function StringsToValuesInternal(const srcList: TStrings; const aDelimiter: string): TMtxVecbase; overload; override;
    function ValuesToStringsInternal(const dstList: TStrings; const Delimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload; override;

 
  public

    

    
    function get_IValues(const RowIdx, ColIdx: integer): integer; 
    function get_SValues(const RowIdx, ColIdx: integer): SmallInt; 
    function get_BValues(const RowIdx, ColIdx: integer): Byte; 
    procedure set_IValues(const RowIdx, ColIdx: integer; const value: integer); 
    procedure set_SValues(const RowIdx, ColIdx: integer; const value: SmallInt); 
    procedure set_BValues(const RowIdx, ColIdx: integer; const value: Byte); 

    function GetBits(const row, col: integer): boolean; 
    procedure SetBits(const row, col: integer; const value: boolean); 

    

    

  
  strict protected
  
    
    function  get_DefaultArray(const Indx1, Indx2: integer): integer; 
    procedure set_DefaultArray(const Indx1, Indx2: integer; const value: integer); 
    
  public
    

    {$WARN HIDING_MEMBER OFF}

    (*<summary>Access elements of the matrix.</summary>
      
<remarks>The RowIdx indicates the row index and the ColIdx parameter indiciates the column index.
</remarks>
*)
    property  IValues[const RowIdx, ColIdx: integer]: integer read get_IValues write set_IValues;

    (*<summary>Access elements of the matrix.</summary>
      
<remarks>The RowIdx indicates the row index and the ColIdx parameter indiciates the column index.
</remarks>
*)
    property  SValues[const RowIdx, ColIdx: integer]: SmallInt read get_SValues write set_SValues;

    (*<summary>Access elements of the matrix.</summary>
      
<remarks>The RowIdx indicates the row index and the ColIdx parameter indiciates the column index.
</remarks>
*)
    property  BValues[const RowIdx, ColIdx: integer]: Byte read get_BValues write set_BValues;

    (*<summary>Allows reading/writing individual bits of the array.</summary>
               
<remarks>Use BitPack and BitUnpack for much faster access to bit data.
</remarks>
*)
    property Bits[const row, col: integer]: boolean read GetBits write SetBits;






    

      (*<summary>Access elements of the matrix without specifying the name of the property.</summary>
      
<remarks>This property allows you to access elements of the matrix without specifying the name of the property.
      For example:

      <code>
      amtx[i,j] := 1; // where amtx is a TMtxInt object
      </code><para/>.

      Note
        Default array property is slower than array pointers. The default array property supports only 32bit integer values (IValues).
</remarks>
*)
    property DefArray[const Indx1, Indx2: integer]: integer read get_DefaultArray write set_DefaultArray; default;

    (*<summary>Returns content as a string suitable for display as a tooltip.</summary>*)
    property IntegerValues: string read GetIntegerValues;

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
    function Parse(const Src: string): TMtxInt;

    function DataIndex(aRow, aCol: integer; aPrecision: TIntPrecision): integer; overload; 
    function DataIndex(aRow, aCol: integer): integer; overload; 

    (*<summary>Sizes the matrix according to bit storage required.</summary>*)
    function BitSize(const bitRows, bitCols: integer): TMtxInt;

    (*<summary>Unpacks bit storage to specified integer storage.</summary>*)
    function BitUnpack(const Src: TMtxInt; const dstPrecision: TIntPrecision = prInt32): TMtxInt; overload;

    (*<summary>Packs integer storage matrix to bit storage. </summary>
               
<remarks>All elements are checked only, if they are zero or not.
</remarks>
*)
    function BitPack(const Src: TMtxInt): TMtxInt; overload;


    (*<summary>Create a new TMtxInt object.</summary>
      
<remarks>Creates a new TMtxInt object. You should use the <see cref="MtxVec.CreateIt"/> where possible and avoid using the
      Create constructor, but only in cases when the object is created and destroy within the same routine.
</remarks>


      <Example>
      <code>
      var A: TMtxInt;
      begin
        A := TMtxInt.Create;
        try
          A.Size(20,20);
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

    
    procedure FreeToCache; override;
    procedure Clear; override;
    function ToolTip: string;
    procedure Assign(Src: TPersistent); override;
    

    

    (*<summary>Copies matrix values to a 2D array.</summary>
        
<remarks>Copies all matrix values to a 2D array.
</remarks>
*)
    procedure CopyToArray(var Dst: T2DIntegerArray); overload;
    (*<summary>Copies all matrix values to a 2D array.</summary>*)
    procedure CopyToArray(var Dst: T2DSmallIntArray); overload;
    (*<summary>Copies all matrix values to a 2D array.</summary>*)
    procedure CopyToArray(var Dst: T2DByteArray); overload;

    (*<summary>Copies the matrix from a 2D array.</summary>
               
<remarks>Sizes the matrix to match the size of the 2D array and copies all the values.
</remarks>
*)
    procedure CopyFromArray(const Src: T2DIntegerArray); overload;
    (*<summary>Sizes the matrix to match the size of the 2D array and copies all the values.</summary>*)
    procedure CopyFromArray(const Src: T2DSmallIntArray); overload;
    (*<summary>Sizes the matrix to match the size of the 2D array and copies all the values.</summary>*)
    procedure CopyFromArray(const Src: T2DByteArray); overload;

    (*<summary>Sizes the Dst array to match the matrix.</summary>
      
<remarks>Sizes the Dst array to match the size of the matrix.
</remarks>
*)
    procedure SizeToArray(var Dst: T2DIntegerArray); overload;
    (*<summary>Sizes the Dst array to match the size of the matrix.</summary>*)
    procedure SizeToArray(var Dst: T2DSmallIntArray); overload;
    (*<summary>Sizes the Dst array to match the size of the matrix.</summary>*)
    procedure SizeToArray(var Dst: T2DByteArray); overload;

    (*<summary>Sizes the calling matrix to match the size of the array.</summary>
      
<remarks>Sizes the calling matrix to match the size of the array.
      IntPrecision property value is preserved.
</remarks>
*)
    procedure SizeFromArray(const Src: T2DIntegerArray); overload;
    (*<summary>Sizes the calling matrix to match the size of the array.</summary>
               
<remarks>IntPrecision property value is preserved.
</remarks>
*)
    procedure SizeFromArray(const Src: T2DSmallIntArray); overload;
    (*<summary>Sizes the calling matrix to match the size of the array.</summary>
              
<remarks>IntPrecision property value is preserved.
</remarks>
*)
    procedure SizeFromArray(const Src: T2DByteArray); overload;


    (*<summary>Sets the size of matrix.</summary>
      
<remarks>Set the calling matrix properties:
      * <see cref="Rows"/> = ARows,
      * <see cref="Cols"/> = ACols

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
      var  A: TMtxInt;
      begin
        CreateIt(A);
        try
          A.Size(2,1); // 2x1 integer matrix
          A.SetZero;
          // A becomes:
          // [0]
          // [0]
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Rows"/>
      <SeeAlso cref="Cols"/>
      <SeeAlso cref="TMtxVecInt.IntPrecision"/>*)
    function Size(ARows, ACols: integer; aPrecision: TIntPrecision): TMtxInt ; overload;

    (*<summary>Specifies size of an integer matrix.</summary>
               
<remarks>The Value of IntPrecision property is preserved.
</remarks>
*)
    function Size(ARows, ACols: integer): TMtxInt ; overload;

    
    function Size(const Src: TMtxVecBase; aPrecision: TIntPrecision): TMtxVecInt ; override;
    function Size(const Src: TMtxVecBase): TMtxVecInt ; override;
    


    (*<summary>Returns the 1D index in to the matrix.</summary>
               
<remarks>Returns: Row*Mtx.Cols + Col
</remarks>
*)
      function IndexOf(const Row, Col: integer): integer; 

    (*<summary>Obtain a pointer to a matrix from object cache.</summary>
      
<remarks>Returns a pointer to a matrix from object cache. Same as calling
      the CreateIt routine.
</remarks>
*)
     class  function CreateFromCache: TMtxInt; 

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PIValues(const Row, Col: integer): PPInteger; overload; 

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PSValues(const Row, Col: integer): PPSmallInt; overload; 

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PBValues(const Row, Col: integer): PPByte; overload; 


    (*<summary>Adopts a pointer to one dimensional array.</summary>
      
<remarks>Adopts a pointer to AArray array. The method sets the calling matrix <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property to prInt32,
      <see cref="Rows"/> to ARows, <see cref="Cols"/> to ACols and Values1D and CValues1D to Pointer(AArray).

      Note
        You must call the <see cref="Disown"/> method, before you destroy the matrix. Use this method to
        save a copy operation. You should be very careful never to resize the calling matrix while it contains the
        adopted memory or call any routines that would require the matrix to reallocate the memory.
</remarks>


      <SeeAlso cref="Disown"/>*)

    
    procedure Adopt(const AArray: PAPointer; const ARows, ACols: integer; const aIntPrecision: TIntPrecision);
    

    (*<summary>Exchange matrix columns.</summary>
      
<remarks>Exchange the i-tj and j-th columns of the calling matrix in-place. An exception is raised if matrix bounds
      are overrun.
</remarks>

      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="ColPermute"/>
      <SeeAlso cref="RowPermute"/>*)
    function ColExchange(i, j: integer): TMtxInt; overload;

    (*<summary>Permute the columns of the Src matrix.</summary>
      
<remarks>The parameter PermuteIdx contains indexes of columns P[i] to which column at index "i" is to be moved.
      The result of the permutation is stored in the calling matrix. Only entire matrix can be copied.
</remarks>


      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="RowPermute"/>*)

    function ColPermute(const Src: TMtxInt; const PermuteIdx: TVecInt): TMtxInt; overload;

    (*<summary>Permute the rows of the Src matrix.</summary>
      
<remarks>The parameter PermuteIdx contains indexes of columns P[i] to which row at index "i" is to be moved.
      The result of the permutation is stored in the calling matrix. Only entire matrix can be copied.
</remarks>


      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="ColPermute"/>*)
    function RowPermute(const Src: TMtxInt; const PermuteIdx: TVecInt): TMtxInt; overload;

    (*<summary>Concatenate an array of matrices to single matrix.</summary>
      
<remarks>Concatenate an array of matrices to form one big matrix and store the result in the calling matrix. The dimensions
      of the block matrices in the Src array must match, to form the new matrix. The block matrices must have matching integer
      precision or an exception will be raised. You must specify Arows*ACols block matrices in the Src array.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling matrix
      are adjusted automatically.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtxInt;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(2,2);
          B.Size(A);
          C.Size(A);
          D.Size(A);
          E.Concat(2,2[A,B
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
    function Concat(ARows, ACols: integer;const Src: array of TMtxInt): TMtxInt;  overload;

    (*<summary>Concenates an array of matrices horizontally.</summary>
      
<remarks>Concenate the Src matrices horizontally and store the results in the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling matrix are adjusted
      automatically. An exception is raised if any of the Src matrices IntPrecision or Rows property does not match.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtxInt;
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
    function ConcatHorz(const Src: array of TMtxInt): TMtxInt;  overload;
    (*<summary>Concenate the Src matrices horizontally and store the results in the calling matrix.</summary>
      
<remarks>The DestRow and DestCol parameters indicate the starting position (in the calling matrix) for concenating. An exception is raised,
      if the calling matrix array bounds are overrun. An exception is raised, if any of the Src matrices IntPrecision or Rows properties
      do not match.
</remarks>
*)
    function ConcatHorz(DestRow, DestCol: integer;const Src: array of TMtxInt): TMtxInt; overload;

    (*<summary>Concenates an array of matrices vertically.</summary>
      
<remarks>Concenate the Src matrices vertically and store the results in calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling matrix are adjusted automatically.
      An exception is raised, if any of the Src matrices IntPrecision or Cols properties do not match.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtxInt;
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
    function ConcatVert(const Src: array of TMtxInt): TMtxInt;  overload;
    (*<summary>Concenate the Src matrices vertically and store the results in calling matrix.</summary>
      
<remarks>The DestRow and DestCol parameters indicate the starting position (in the calling matrix) for concatenating.
      An exception is raised, if the calling matrix array bounds are overrun. An exception is raised, if any of the
      Src matrices IntPrecision or Cols properties do not match.
</remarks>
*)
    function ConcatVert(DestRow, DestCol: integer;const Src: array of TMtxInt): TMtxInt; overload;


    (*<summary>Copy the Mtx elements [MtxRow,MtxCol]..[MtxRow+NrRows-1,MtxCol+NrCols-1] to the calling matrix elements
      [Row,Col],,[Row+NrRows-1,Col+NrCols-1].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is true and bounds are
      overrun. If Transpose is true, the matrix is transposed as well.
</remarks>
*)
    function Copy(const Mtx: TMtxInt; MtxRow, MtxCol, Row, Col, NrRows, NrCols: integer; Transpose: boolean = false): TMtxInt;   overload;

    (*<summary>Copies values from vector to a matrix.</summary>
      
<remarks>Copy all Vec elements to the calling matrix. Set the calling matrix <see cref="Rows"/> property to NrRows. Set the calling
      matrix <see cref="Cols"/> property to Vec.Length div NrRow (Length = Rows*Cols). The calling matrix <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/>
      property is adjusted automatically. An exception is raised if Vec.Length mod NrRows &lt;&gt; 0.
</remarks>


      <Example>
      <code>
      var A: TMtxInt;
          v: TVecInt;
      begin
        CreateIt(A);
        Create(v);
        try
          v.SetIt([1, 0, 2, -1, 5, 1.2]) ;
          A.CopyVec(v);
        finally
          FreeIt(A);
          FreeIt(v);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Copy"/>
      <SeeAlso cref="TVec.CopyMtx"/>*)
    function CopyVec(const Vec: TVecInt; NrRows: Integer): TMtxInt; overload;

    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements starting with [Row,Col].</summary>
      
<remarks>An exception is raised if <see cref="TMtxVecBase.ConditionCheck"/> is true and bounds are overrun or if <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling
      matrix and Vec do not match.
</remarks>
*)
    function CopyVec(const Vec: TVecInt; VecIndex, Len, Row, Col: Integer): TMtxInt;   overload;

    (*<summary>Cumulative sum for each of the matrix columns.</summary>
      
<remarks>Calculate the cumulative sum for each of the calling matrix columns in-place.
      Function performs no overflow checking. It is users responsibility to ensure that the sum does not overflow the range
      of the used integer storage type.
</remarks>


      <Example>
      <code>
      var Mtx: TMtxInt;
      begin
        Mtx := TMtxInt.Create;
        try
          Mtx.Size(3,2,[1,2,
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
    function CumSum: TMtxInt; overload;
    (*<summary>Calculate the cumulative sum for each of the X matrix columns.</summary>
      
<remarks>Store the results in calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/>
      properties of the callig matrix are adjusted implicitly to match those of Mtx matrix.
</remarks>
*)
    function CumSum(const Mtx: TMtxInt): TMtxInt; overload;

    (*<summary>Disowns a pointer to an array from matrix Values1D.</summary>
      
<remarks>The method does the opposite of the <see cref="Adopt"/> method. It will set the AArrays to Pointer(Values),
      ARows and ACols to matrix's <see cref="Rows"/> and <see cref="Cols"/> and aIntPrecision to matrix
      <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property. Use the Disown method to "disconnect" AArray from the TMtxInt.IValues1D.
      Disown sets IValues1D, SValues1D and BValues1D array pointers to nil and Rows, Cols properties to ACols, but without freeing
      the allocated memory. The allocated memory can be disowned only, if it was adopted with a call to the
      <see cref="Adopt"/> method.
</remarks>


      <SeeAlso cref="Adopt"/>*)
    
    procedure Disown(out AArray: PAPointer; out ARows, ACols: integer; out aIntPrecision: TIntPrecision); overload;
    
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
      var A: TMtxInt;
          D: TVecInt;
      begin
        CreateIt(A);
        CreateIt(D);
        try
          A.SetIt(2,2,[1,1,
                       1,2]);
          D.SetIt([3,4]);
          A.Diag(D,0);
          // A becomes:
          // [3,1]
          // [1,4]
        finally
          FreeIt(D);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="TVecInt.Diag"/>*)
    function Diag(const Vec: TVecInt; k: integer): TMtxInt; overload;

    (*<summary>Compares two matrices.</summary>
      
<remarks>Compares Mtx with the calling matrix and returns true if the matrices are equal (if all elements match in position
      and value). Tolerance defines the comparison tolerance. The maximum difference between elements may not exceed: +/-Tolerance.
      If Tolerance is omitted, a direct comparison algorithm is used.
</remarks>


      <Example>
      <code>
      var A,B: TMtxInt;
        c: boolean;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,2,
                      2,4]);  // 2x2, int matrix
          B.SetIt(2,2,[1,2,
                      2,4]);  // 2x2, int matrix
          c := A.Equal(B); // Check for an exact match
        finally
          FreeIt(A,B);
        end;
      end;
      </code>
      </Example>*)
    function Equal(const Mtx: TMtxInt): boolean; overload;

    (*<summary>Constructs an eye matrix.</summary>
      
<remarks>Construct an eye matrix with ones only on main diagonal and zeros elsewhere.
      The number of rows and columns of an eye matrix are set by ARows and ACols
      parameters. The <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property
      of an eye matrix is set by aIntPrecision parameter.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
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
    function Eye(ARows,ACols: integer; aIntPrecision: TIntPrecision = prInt32): TMtxInt; overload;

    (*<summary>Flips the matrix elements horizontally.</summary>
      
<remarks>Flip calling matrix elements horizontally - element [row, j] = element [row, Cols-j]. This operation
      is performed on all calling matrix rows.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,2,
                      2,4]);  // 2x2, integer matrix
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
    function FlipHor: TMtxInt; overload;
    (*<summary>Flip all SrcMtx matrix elements horizontally.</summary>
      
<remarks>Store the results in the calling matrix. The flip operation is performed on
       all SrcMtx matrix rows. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of callign matrix are adjusted automatically.
</remarks>
*)
    function FlipHor(const SrcMtx: TMtxInt): TMtxInt; overload;

    (*<summary>Flips the matrix elements vertically</summary>
      
<remarks>Flip calling matrix elements vertically - element [j, col] = element [Rows-j, col]. This operation
      is performed on all calling matrix columns.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,2,
                       2,4]);  // 2x2, integer matrix
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
    function FlipVer: TMtxInt; overload;
    (*<summary>Flip all SrcMtx matrix elements vertically.</summary>
      
<remarks>Store the results in the calling matrix. The flip operation is performed
      on all SrcMtx matrix columns. The <see cref="Rows"/>, <see cref="Cols"/> and
      <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of callign matrix are adjusted automatically.
</remarks>
*)
    function FlipVer(const SrcMtx: TMtxInt): TMtxInt; overload;





















    (*<summary>The Kronecker product between two vectors.</summary>
      
<remarks>Calculates the Kronecker product between Vec1 and Vec2 and stores the result in the
      calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/>
      properties of calling matrix are set automatically.
</remarks>


      <Example>
      <code>
      var A: TMtxInt;
          V1,V2: TVecInt;
      begin
        CreateIt(A);
        CreateIt(V1,V2);
        try
          V1.SetIt([1,2,3]);
          V2.SetIt([4,5,6]);
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
    function Kron(const Vec1, Vec2: TVecInt): TMtxInt; overload;

    (*<summary>Constructs lower triangular matrix.</summary>
      
<remarks>The method uses Mtx matrix to construct a lower triangular matrix. The results are stored in the calling matrix.
      If the ZeroLower parameter is true then the calling matrix superdiagonal elements will be set to zero - otherwise
      the superdiagonal elements will not be initialized. If the Diagonal boolean parameter is true then the Mtx matrix
      main diagonal elements will be copied to the calling matrix main diagonal elements. If the Diagonal parameter is
      false, the calling matrix main diagonal elements will be set to zero.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
            A.SetIt(2,1,[1,2,
                         2,4]);  // 2x2, integer matrix
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
    function LowerTriangle(const Mtx: TMtxInt; ZeroUpper, Diagonal: boolean): TMtxInt; overload;

    (*<summary>Compute maximum of each row.</summary>
               
<remarks>Compute maximum of each row and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MaxRows(const Dst: TVecInt); overload;
    (*<summary>Compute minimum of each row.</summary>
               
<remarks>Compute mniimum of each row and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MinRows(const Dst: TVecInt); overload;
    (*<summary>Compute maximum of each column.</summary>
               
<remarks>Compute maximum of each column and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MaxCols(const Dst: TVecInt); overload;
    (*<summary>Compute minimum of each column.</summary>
               
<remarks>Compute minimum of each column and store the result in to Dst vector. The Dst is sized automatically.
</remarks>
*)
    procedure MinCols(const Dst: TVecInt); overload;

    (*<summary>Compute maximum and minimum of each row.</summary>
               
<remarks>Compute maximum and minimum of each row and store the result in to Dst vectors. The Dst's are sized automatically.
</remarks>
*)
    procedure MaxMinRows(const DstMaxRows, DstMinRows: TVecInt); overload;
    (*<summary>Compute maximum and minimum of each column.</summary>
               
<remarks>Compute maximum and minimum of each column and store the result in to Dst vectors. The Dst's are sized automatically.
</remarks>
*)
    procedure MaxMinCols(const DstMaxCols, DstMinCols: TVecInt); overload;

    (*<summary>Matrix array multiplication.</summary>
      
<remarks>Multiplies elements in Mtx matrix with the elements in the calling matrix (array multiplication) and stores the results in calling matrix.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of both matrices must match, otherwise an exception is
      raised.
</remarks>


      <Example>
      <code>
      var  A,B,C: TMtxInt;
      begin
        CreateIt(A,B,C);
        try
          A.SetIt(2,2,[1,2,
                       2,4]);
          B.SetIt(2,2,[1,2,
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
      </Example>*)
    function MulElem(const Mtx: TMtxInt): TMtxInt; overload;
    (*<summary>Multiplies elements in Mtx1 matrix with the elements in Mtx2 matrix (array multiplication) and stores the results in calling matrix.</summary>
      
<remarks>The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling matrix are set implicitly to match those
      of Mtx1 and Mtx2 matrices. Mtx1 and Mtx2 Rows, Cols, and IntPrecision properties must be the same, otherwise an excetion is raised.
      raised.
</remarks>
*)
    function MulElem(const Mtx1, Mtx2: TMtxInt): TMtxInt; overload;





































    
    procedure Reset; override;
    

    (*<summary>Resizes the matrix, while preserving the values in already allocated memory.</summary>
      
<remarks>Sets the <see cref="Rows"/> property to NewRows,
      <see cref="Cols"/> property to NewCols,
      while preserving the values in already allocated memory.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,3,
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
    function Resize(NewRows , NewCols: integer): TMtxInt; overload;
    (*<summary>Performs resize of the calling matrix to NewRows and NewCols.</summary>
      
<remarks>Sopies the Src matrix values to the calling matrix.
      The elements outside the newly created matrix size are not referenced.
</remarks>
*)
    function Resize(const Src: TMtxInt; NewRows, NewCols: integer): TMtxInt; overload;

    (*<summary>Resize and transpose.</summary>
      
<remarks>Resizes the calling matrix to NewRows and NewCols and then copies
      the Src matrix values to the calling matrix. The elements outside
      the newly created matrix size are not referenced. At the end
      the method also transposes the calling matrix. All this operations
      are performed in one single pass.
</remarks>
*)
    function ResizeAndTranspose(const Src: TMtxInt; NewRows, NewCols: integer): TMtxInt;

    (*<summary>Rotates matrix rows 90 degrees clockwise.</summary>
      
<remarks>Rotates all calling matrix rows 90 degrees clockwise in-place (check the scheme bellow).

      <IMG name="mtx013"/><para/>

      Note
        This operation differs from the <see cref="Transp"/> operation.<para/>
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        A.SetIt(2,2,[1,3,
                     2,4]);
        B.Rotate90(A);
        FreeIt(A,B);
      end;
      </code>
      </Example>

      <SeeAlso cref="Transp"/>*)
    function Rotate90: TMtxInt; overload;
    (*<summary>Rotate all Mtx matrix rows 90deg clockwise.</summary>
      
<remarks>Store the results in calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling
      matrix are adjusted automatically.
</remarks>
*)
    function Rotate90(const Mtx: TMtxInt): TMtxInt; overload;

    (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
    function ScatterByMask(const Src: TMtxVecInt; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVecInt; overload; override;

    (*<summary>Exchanges two matrix rows.</summary>
      
<remarks>Exchange the i-th and j-th rows of the calling matrix in-place. An exception is raised if matrix bounds are
      overrun. The indexes i and j are zero based. (the first row has index 0).
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,3,
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
    function RowExchange(i, j: integer): TMtxInt; overload;

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

    procedure SetSubRange(const Src: TMtxVecInt; Index: integer; aRows, aCols: integer); overload;

    (*<summary>Copies values from vector to matrix column.</summary>
      
<remarks>Copy all Vec elements to the calling matrix Col column. An exception is raised, if array bounds are overrun.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
            V: TVecInt;
      begin
        CreateIt(A);
        CreateIt(V);
        try
          A.Size(2,1);
          V.SetIt([1,2, 2,4]);
          A.SetCol(V,0);
        finally
          FreeIt(V);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetRow"/>*)
    function SetCol(const Vec: TMtxVecInt; Col: integer): TMtxInt; overload;
    (*<summary>Copy vector Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements [Row,Col]..[Row+Len-1,Col].</summary>
      
<remarks>An exception is raised if condition checking is enabled and array bounds are overrun.
</remarks>
*)
    function SetCol(const Vec: TMtxVecInt; VecIndex, Row, Col, Len: integer): TMtxInt; overload;

    (*<summary>Sets matrix values.</summary>
      
<remarks>Sets matrix values. This method gives you the possibility to pass large arrays of elements without having to declare constant arrays.

      Pass all elements in A array to the calling matrix. The <see cref="Rows"/> and <see cref="Cols"/> properties of the calling
      matrix are set to ARows and ACols and the <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property is set to aPrecision.
      An exception is raised if the matrix size (ARows*ACols) does not match the number of integer numbers in A.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,[1,2,
                       2,4]);
        finally
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetVal"/>*)
    function SetIt(ARows, ACols: integer; aPrecision: TIntPrecision; const A: array of Integer): TMtxInt; overload;

    function SetIt(ARows, ACols: integer; const A: array of integer): TMtxInt; overload;

    (*<summary>Copies values from vector to matrix row.</summary>
      
<remarks>Copy all Vec elements to the calling matrix Row row. The <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property of the calling
      matrix is adjusted automatically. An exception is raised, if array bounds are overrun.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
           V: TVecInt;
      begin
        CreateIt(A);
        CreateIt(V);
        try
          A.Size(1,2);
          V.SetIt([1,2, 2,4]);
          A.SetCol(V,0);
        finally
          FreeIt(V);
          FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="SetCol"/>*)
    function SetRow(const Vec: TMtxVecInt; Row: integer): TMtxInt; overload;

    (*<summary>Copy vector Vec elements [VecIndex]..[VecIndex+Len-1] to the calling matrix elements [Row,Col]..[Row,Col+Len-1].</summary>
      
<remarks>An exception is raised if condition checking is enabled and array bounds are overrun.
</remarks>
*)
    function SetRow(const Vec: TMtxVecInt; VecIndex, Row, Col, Len: integer): TMtxInt; overload;

    (*<summary>Initializes matrix values to zero.</summary>
      
<remarks>Sets the calling matrix elements [Row,Col]..[ToRow,ToCol] to zero. An exception is raised if
      <see cref="TMtxVecBase.ConditionCheck"/> is true and array bounds are overrun.
</remarks>
*)
    function SetZero(Row, Col, ToRow, ToCol: integer): TMtxInt; overload;

    (*<summary>Initializes matrix values to Value.</summary>
      
<remarks>Sets the calling matrix elements [Row,Col]..[ToRow,ToCol] to Value. An exception is raised if
      <see cref="TMtxVecBase.ConditionCheck"/> is true and array bounds are overrun.
</remarks>


      <SeeAlso cref="SetIt"/>
      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: integer; Row, Col, ToRow, ToCol: integer): TMtxInt; overload;


    (*<summary>Sorts the elements in a matrix row(s) in ascending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in ascending order in-place. 

      Note
        Each row is sorted independently from the other.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,[1,2,
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
    function SortAscend: TMtxInt; overload;
    (*<summary>Performs row based sort in ascending order. The Col parameter defines the column based on which the rows are compared with each other.</summary>*)
    function SortAscend(Col: integer): TMtxInt; overload; 

    (*<summary>Sorts the elements in a matrix row(s) in descending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in descending order in-place. 

      Note
        Each row is sorted independently from the other.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
      begin
        CreateIt(A);
        try
          A.SetIt(2,2,[1,2,
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
    function SortDescend: TMtxInt; overload;

    (*<summary>Performs row based sort in descending order. The Col parameter defines the column based on which the rows are compared with each other.</summary>*)
    function SortDescend(Col: integer): TMtxInt; overload; 


    (*<summary>Convert strings to integers and store them in the Values array.</summary>
      
<remarks>Convert strings in aList to integers and store them in to the IValues/SValues/BValues array of the calling matrix.
      <see cref="Rows"/> property is set to aList.Count. <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> and <see cref="Cols"/> propertes
      are auto-detected. Columns must be separated with a Delimiter. By default the delimiter is the tab charachter.
</remarks>


      <Example>
      <code>
      var a,b: TMtxInt;
      begin
        CreateIt(a,b);
        try
          a.SetIt(2,2,[1,2,3,4]);
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
    function StringsToValues(const srcList: TStrings; const aDelimiter: string): TMtxInt; overload;
    (*<summary>Convert strings to integers and store them in the Values array.</summary>
               
<remarks>The assumed delimiter between columns will be a tab charachter.
</remarks>
*)
    function StringsToValues(const srcList: TStrings): TMtxInt; overload;

    (*<summary>Convert strings from [ListIndex]..[ListIndex+ListLen-1] in srcList to integers.</summary>
      
<remarks>Store them in the Values array of the calling matrix, starting from element [Row,Cols]. The size of the calling matrix is not changed.
      If array bounds are overrun an exception is raised.
</remarks>
*)
    function StringsToValues(const srcList: TStrings; ListIndex,ListLen, Row,Col: Integer; Delimiter: string = kTab): TMtxInt; overload;

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
      filled with values from the calling matrix, must already have matching size and IntPrecision properties to
      cover the calling matrix or an exception will be raised.
</remarks>


      <Example>
      <code>
      var A,B,C,D,E: TMtxInt;
      begin
        CreateIt(A,B,C,D);
        CreateIt(E);
        try
          A.Size(2,2);
          B.Size(A);
          C.Size(A);
          D.Size(A);
          E.SetIt(4,4,[1,2,3,4
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
    procedure Split(ARows, ACols: integer;const Dst: array of TMtxInt);

    (*<summary>Calculates the sum of each of the calling matrix columns.</summary>
      
<remarks>Calculates the sum of each of the calling matrix columns and stores the results in Dst vector.
      The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the Dst vector are adjusted automatically.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
          V: TVecInt;
      begin
        CreateIt(A);
        CreateIt(V);
        A.Size(2,2,[1,5,
                    2,3]);

        A.SumCols(V); // V = [3,8]
        FreeIt(V);
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="SumRows"/>*)
    procedure SumCols(const Dst: TVecInt); overload;

    (*<summary>Calculates the sum of each of the calling matrix rows.</summary>
      
<remarks>Calculates the sum of each of the calling matrix rows and stores the results in Dst vector.
      The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the Dst vector
      are adjusted automatically.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
          V: TVecInt;
      begin
        CreateIt(A);
        CreateIt(V);
        A.Size(2,2, [1,2,
                     2,4]);

        A.SumRows(V); // V = [3,6]
        FreeIt(V);
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="SumCols"/>*)
    procedure SumRows(const Dst: TVecInt); overload;

    (*<summary>Calculates the tensor product of two vectors.</summary>
      
<remarks>Calculates the tensor product of Vec1 and Vec2 vectors and stores the results in the calling matrix. The <see cref="Rows"/>
      property is set to Vec1.<see cref="TMtxVecBase.Length" text="Length"/> and <see cref="Cols"/> property is set to Vec2.<see cref="TMtxVecBase.Length" text="Length"/>.
      The <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property of the calling matrix is adjusted automatically.
</remarks>



        <Example>
        <code>
        var Vec1,Vec2: TVecInt;
          V: TMtxInt;
        begin
          CreateIt(Vec1,Vec2);
          CreateIt(V);
          try
            Vec1.Size(3);
            Vec1.SetIt([0,2,3]
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
        </Example>*)
    function TensorProd(const Vec1, Vec2: TVecInt): TMtxInt; overload;

    (*<summary>Transposes matrix.</summary>
      
<remarks>Transposes calling matrix in-place. Instead of using transpose directly, try using the <see cref="TMtxOperation"/>
      parameter of many TMtxInt methods. If this operation can not be avoided try using the not-in-place version  (see bellow)
      or see the <see cref="Rotate90"/> method.
</remarks>


      <Example>
      <code>
      var  A: TMtxInt;
      begin
        CreateIt(A);
        A.Size(2,1); // 2x1 integer matrix
        A.SetVal(3);
        A.Transp;
        FreeIt(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Rotate90"/>*)
    function Transp: TMtxInt; overload;

    (*<summary>Transpose the Mtx matrix.</summary>
      
<remarks>Write the results to the calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling matrix are adjusted automatically.
</remarks>
*)
    function Transp(const Mtx: TMtxInt): TMtxInt; overload;

    (*<summary>Constructs upper triangular matrix.</summary>
      
<remarks>The method uses Mtx matrix to construct an upper triangular matrix. The results are stored in the calling matrix.
      If the ZeroLower parameter is true then the calling matrix subdiagonal elements will be set to zero - otherwise
      the subdiagonal elements will not be initialized. If the Diagonal boolean parameter is true then the Mtx matrix
      main diagonal elements will be copied to the calling matrix main diagonal elements. If the Diagonal parameter is
      false, the calling matrix main diagonal elements will be set to zero.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        A.SetIt(2,1,[1,2,
                     2,4]);  // 2x2, integer matrix
        B.UpperTriangle(A,True,True);
        // B becomes:
        //  [1,2,
        //  [0,4]
        FreeIt(A,B);
      end;
      </code>
      </Example>

      <SeeAlso cref="LowerTriangle"/>*)
    function UpperTriangle(const Mtx: TMtxInt; ZeroLower, Diagonal: boolean): TMtxInt; overload;

        (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>
          
<remarks>Convert all elements of the calling matrix to strings with text delimiter Delimiter and store them in dstList,
          by using the Add method of TStrings object.
          Set Align to have columns aligned left or right when using fixed width font (like Courier New).
          Specifiy Headers to be "true", if row and column labels are to be printed.

          Performance note:
            This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter. Use TStringList or StringList types and then
            call Memo.Lines.AddStrings(yourList) for best results.
</remarks>


          <Example>
          <code>
          procedure TForm1.Button1Click(Sender: TObject);
          var a,b: TMtxInt;
          begin
            CreateIt(a,b);
            try
              a.SetIt(2,2,[1,2,3,4]);
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
        function ValuesToStrings(const dstList: TStrings; const Delimiter: string; const Align: TFixedTextAlign; const Headers: boolean): integer; overload;

        (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>

          
<remarks>Performance note:
            This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter. Use TStringList or StringList types and then
            call Memo.Lines.AddStrings(yourList) for best results.
</remarks>
*)

        function ValuesToStrings(const dstList: TStrings; const Delimiter: string = kTab; const Align: TFixedTextAlign = ftaNone): integer; overload;

        (*<summary>Convert calling matrix elements, starting with [Row,Col] and up to [Row+RowCount-1,Col+ColCount-1] elements to strings.</summary>
          
<remarks>Use text Delimiter for columns and store the resulting strings in dstList starting at ListIndex.
          If dstList is not large enough, the method will use the Add method of dstList object. Existing items will be overwritten.
          Set Align to have columns aligned left or right when using fixed width font (like Courier New).
          Specifiy Headers to be "true", if row and column labels are to be printed.

          Performance note:
            This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter. Use TStringList or StringList types and then
            call Memo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
        function ValuesToStrings(const dstList: TStrings; ListIndex,Row,Col,RowCount,ColCount: integer;
                                  const Delimiter: string = kTab; const Align: TFixedTextAlign = ftaNone;
                                  const Headers: boolean = false): integer; overload;

        (*<summary>Converts all calling matrix elements to string.</summary>*)
        procedure ValuesToText(out Text: String; const Delimiter: string = kTab);overload;

        (*<summary>Converts Row..Col to Row+RowCount..Col+ColCount matrix elements to string.</summary>*)
        procedure ValuesToText(out Text: String; Row,Col,RowCount,ColCount: integer; const Delimiter: string = kTab); overload;

        function TextToValues(const Text: String; const Delimiter: String = kTab): TMtxInt;


        


      

        

        
        function  ReadHeader(const SrcStream: TStream; Endian: TEndianness = MtxSystemEndianness): TPrecision; override;
        procedure WriteHeader(const DstStream: TStream; Precision: TPrecision = prInteger; Endian: TEndianness = MtxSystemEndianness); override;
        

      

    published
        (*<summary>Defines number of bits stored in one row.</summary>*)
        property ColsBitCount: integer read fColsBitCount write SetColsBitCount;

        (*<summary>Defines the number of leading columns.</summary>
          
<remarks>This value defines the spacing in number of samples between rows.
          It is fixed to be the same to the value of <see cref="Cols"/> (for now).
</remarks>
*)
        property  LeadingCols: integer read GetLeadingCols write SetLeadingCols;
        (*<summary>Defines the number of matrix columns.</summary>
          
<remarks>Use this property to set or get the number of the calling matrix columns. Setting the Cols does not affect the actual amount of
          memory allocated unless Rows*Cols is bigger than <see cref="TMtxVecBase.ActualSize"/>.
</remarks>


          <SeeAlso cref="Rows"/>
          <SeeAlso cref="Size"/>*)
        property Cols: integer read FCols write SetCols default 0;

        (*<summary>Defines the number of matrix rows.</summary>
          
<remarks>Use this property to set or get the number of the calling matrix rows. Setting the Rows does not affect the actual amount of
          memory allocated unless Rows*Cols is bigger than <see cref="TMtxVecBase.ActualSize"/>.
</remarks>



          <SeeAlso cref="Cols"/>
          <SeeAlso cref="Size"/>*)
        property  Rows: integer read FRows write SetRows default 0;
    end;

  (*<summary>Class for 16bit two dimensional integer arrays.</summary>
      
<remarks>Class for accessing and manipulating 16bit two dimensional arrays.
      It handles memory allocation and basic vector arithmetics.
</remarks>
*)
  
  TMtxSmallInt = class(TMtxInt)
  strict protected
    procedure HookPointers; override;




  public
    
    
    (*<summary>Allows setting/getting the 16bit integer value at position row,col.</summary>
      
<remarks>Allows setting/getting the 16bit integer value at position row,col.
</remarks>
*)
    Values: TSmallIntArray;
    

    function get_DefaultArray(const row,col: integer): SmallInt; 
    procedure set_DefaultArray(const row,col: integer; const value: SmallInt); 

    (*<summary>Allows setting/getting the integer value at position row,col.</summary>
      
<remarks>Allows setting/getting the 16bit integer value at position row,col.
      This property reads/writes to the same memory as <see cref="Values"/> property.
</remarks>
*)
    property DefaultArray[const row,col: integer]: SmallInt read get_DefaultArray write set_DefaultArray; default; 







    function PValues(const row, col: integer): PAPointer; overload;
    
    constructor Create; override;
    
  end;

  (*<summary>Class for 8bit two dimensional integer arrays.</summary>
      
<remarks>Class for accessing and manipulating 8bit two dimensional arrays.
      It handles memory allocation and basic vector arithmetics.
</remarks>
*)

  
  TMtxByte = class(TMtxInt)
  strict protected
    procedure HookPointers; override;




  public
    
    
    (*<summary>Allows setting/getting the 8bit integer value at position row,col.</summary>
      
<remarks>Allows setting/getting the 8bit integer value at position row,col.
</remarks>
*)
    Values: Math387.TByteArray;
    
    function get_DefaultArray(const row,col: integer): Byte; 
    procedure set_DefaultArray(const row,col: integer; const value: Byte); 

    (*<summary>Allows setting/getting the integer value at position row,col.</summary>
      
<remarks>Allows setting/getting the 8bit value at position row,col.
      This property reads/writes to the same memory as <see cref="Values"/> property.
</remarks>
*)
    property DefaultArray[const row,col: integer]: Byte read get_DefaultArray write set_DefaultArray; default; 







    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns address of element at Values[i]. Under .NET, internal memory
      must be Pinned first. See <see cref="TMtxVecBase.Pin" text="Pin"/> and <see cref="TMtxVecBase.UnPin" text="UnPin"/>
</remarks>
*)
    function PValues(const row, col: integer): PAPointer; overload;

    
    constructor Create; override;
    

  end;


  (*<summary> Implementation of object cache for TVecInt objects. </summary>*)
  TVecIntCache = class(TAbstractMtxVecCache)
   public
      Int32CacheOffset: TIntegerArray;
      Int32CachePointers: T2DIntegerArray;  

      Int16CacheOffset: TIntegerArray;
      Int16CachePointers: T2DSmallIntArray;

      Int8CacheOffset: TIntegerArray;
      Int8CachePointers: Math387.T2DByteArray;
   strict protected
      Int16Cache: TSmallIntArray;
      Int8Cache: Math387.TByteArray;

      function NewItem: TMtxVecBase; override;
      procedure CacheMemFree; override;
      function GetCacheInBytes: Int64; override;
      procedure CacheMemAllocate(ItemCount, Elements: integer); override;
   public
      Int32Cache: TIntegerArray;

      constructor Create; override;

   end;


(*<summary> Implementation of object cache for TVecInt objects. </summary>*)
  TMtxIntCache = class(TAbstractMtxVecCache)
   public
      Int32CacheOffset: TIntegerArray;
      Int32CachePointers: T2DIntegerArray;  

      Int16CacheOffset: TIntegerArray;
      Int16CachePointers: T2DSmallIntArray;

      Int8CacheOffset: TIntegerArray;
      Int8CachePointers: Math387.T2DByteArray;
   strict protected
      Int8Cache: Math387.TByteArray;
      Int16Cache: TSmallIntArray;
      Int32Cache: TIntegerArray;

      function NewItem: TMtxVecBase; override;
      procedure CacheMemFree; override;
      function GetCacheInBytes: Int64; override;
      procedure CacheMemAllocate(ItemCount, Elements: integer); override;
   public

      constructor Create; override;

   end;




  
  procedure CreateIt(out a: TVecInt); overload;

  
  procedure CreateIt(out a, b: TVecInt); overload;

  
  procedure CreateIt(out a, b, c: TVecInt); overload;

  
  procedure CreateIt(out a, b, c, d: TVecInt); overload;


  
  procedure FreeIt(var a: TVecInt); overload;

  
  procedure FreeIt(var a, b: TVecInt); overload;

  
  procedure FreeIt(var a, b, c: TVecInt); overload;

  
  procedure FreeIt(var a, b, c, d: TVecInt); overload;


  
  procedure CreateIt(out a: TMtxInt); overload;

  
  procedure CreateIt(out a, b: TMtxInt); overload;

  
  procedure CreateIt(out a, b, c: TMtxInt); overload;

  
  procedure CreateIt(out a, b, c, d: TMtxInt); overload;


  
  procedure FreeIt(var a: TMtxInt); overload;

  
  procedure FreeIt(var a, b: TMtxInt); overload;

  
  procedure FreeIt(var a, b, c: TMtxInt); overload;

  
  procedure FreeIt(var a, b, c, d: TMtxInt); overload;



