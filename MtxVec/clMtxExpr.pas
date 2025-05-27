










(*<summary>Adds operator overloading support for Open CL clVector and clMatrix.</summary>
          
<remarks>The unit declares clVector and clMatrix types for +, -, *, / operator support.
</remarks>
*)
unit clMtxExpr;


interface

{$I bdsppdefs.inc}

uses Math387, clMtxVec
     
     
     ,Windows
     
     ,Classes
     
     
     
     ,AbstractMtxVec
     ,MtxVec
     ,MtxVecInt

     
     ;


    
      {$HPPEMIT END '#include "clMtxExpr.h"'}
    




type

(*<summary>Open CL vector class with overloaded operators.</summary>
    
<remarks>Declare clVector to run computation on Open CL
    Be carefull to declare clVector only for local variables with short lifetime.
    Call the Create method for clVector, if the variable is a global variable or a
    variable with a longer life.If the Create method (constuctor) is called, the clVector
    creates its own memory object in GPU memory, otherwise object cache is used.
</remarks>


    <Example>
    <code>
    var b, c: clVector;
        bv: TSingleArray;
    begin
          b := clVector(TDoubleArray.Create(1,1,1,1)); b = [1,1,1,1];
          b.Scale(2);
          c := b*2 + Cplx(2,3); c := b*2 + 2 + 3i

          b.Add(c);
          b.CopyToArray(bv); //copy data back to host from GPU
    end;
    </code>
    </Example>*)

  clVector = record
  strict private
    
    FData: TOpenCLVector;
    
    function get_Data: TOpenCLVector;
    function GetCaption: string;
    function GetComplex: boolean;
    function GetLength: integer;
    function GetTag: PointerInteger;
    procedure SetCaption(const Value: string);
    procedure SetComplex(const Value: boolean);
    procedure SetLength(const Value: integer);
    procedure SetTag(const Value: PointerInteger);
    function GetClFloatPrecision: TclFloatPrecision;
    procedure SetClFloatPrecision(const Value: TclFloatPrecision);
    function GetDevice: TOpenCLDevice;
    function GetIsSubRange: boolean;
    procedure CreateFromCache;
  private
    property Data: TOpenCLVector read get_Data;
  public
    (*<summary>Add left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: TCplx; const Right: clVector): clVector;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: clVector; const Right: TCplx): clVector;
    (*<summary>Add Left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: double; const Right: clVector): clVector;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: clVector; const Right: double): clVector;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: TOpenCLMtxVec;const Right: clVector): clVector;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: clVector; const Right: TOpenCLMtxVec): clVector;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: clVector;const Right: clVector): clVector;

    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TCplx;  const Right: clVector): clVector;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: clVector; const Right: TCplx): clVector;
    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: double; const Right: clVector): clVector;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: clVector; const Right: double): clVector;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: clVector; const Right: TOpenCLMtxVec): clVector;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TOpenCLMtxVec; const Right: clVector): clVector;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: clVector; const Right: clVector): clVector;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: clVector; const Right: TCplx): clVector;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(const Left: TCplx; const Right: clVector): clVector;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: clVector; const Right: double): clVector;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(const Left: double; const Right: clVector): clVector;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: clVector; const Right: TOpenCLMtxVec): clVector;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: TOpenCLMtxVec; const Right: clVector): clVector;
    (*<summary>Multiply all elements in Left with corresponding elements in Right</summary>*)
    class operator Multiply(const Left: clVector; const Right: clVector): clVector;

    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: clVector; const Right: TCplx): clVector;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: TCplx; const Right: clVector): clVector;
    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: clVector; const Right: double): clVector;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: double; const Right: clVector): clVector;

    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: clVector; const Right: TOpenCLMtxVec): clVector;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: TOpenCLMtxVec; const Right: clVector): clVector;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: clVector; const  Right: clVector): clVector;

    (*<summary>Negates all values inside AValue.</summary>*)
    class operator Negative(const AValue: clVector): clVector;

    class operator Explicit(const AValue: TCplxArray): clVector;
    class operator Explicit(const AValue: TDoubleArray): clVector;
    class operator Explicit(const AValue: TSingleArray): clVector;
    class operator Explicit(const AValue: clVector): TDoubleArray;
    class operator Explicit(const AValue: clVector): TSingleArray;
    class operator Explicit(const AValue: TOpenCLVector): clVector;

    
    class operator Implicit(const AValue: TVec): clVector;

    

    class operator Implicit(const AValue: clVector): TOpenCLVector;
    class operator Implicit(const AValue: clVector): TOpenCLMtxVec;
    class operator Implicit(const AValue: clVector): TOpenCLBase;
  public
    
    procedure CopyTo(const Dst: TVec); overload;
    procedure CopyTo(const Dst: TVecInt); overload;
    procedure Copy(const Src: TVecInt); overload;
    procedure Copy(const Src: TVec); overload;
    
    procedure SizeToArray(var Dst: TSingleArray); overload;
    procedure SizeToArray(var Dst: TDoubleArray); overload;
    procedure SizeToArray(var Dst: TCplxArray); overload;

    function CopyFromArray(const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(const Src: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(const Src: TCplxArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(const Src: TDoubleArray): TOpenCLMtxVec; overload;

    procedure CopyToArray(var Dst: TSingleArray); overload;
    procedure CopyToArray(var Dst: TDoubleArray); overload;
    procedure CopyToArray(var Dst: TCplxArray); overload;

    
    procedure AssignWithoutCopy(const Src: clVector); overload;
    procedure Assign(const Src: TOpenCLMtxVec); overload;
    

    (*<summary>The norm of a vector.</summary>
      
<remarks>Calculates the norm of a Vec vector and stores the results in calling vector.
      This functions works the same as <see cref="TMtxVec.PowerSpectrum"/>.
</remarks>
*)
    function Norm(const Vec: TOpenCLVector): TOpenCLVector;

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


      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetFullRange"/>*)
    procedure SetSubRange(const Src: TOpenCLMtxVec); overload;

    (*<summary>Define a subvector of the Src vector starting at Index and ending at Index+Len-1.</summary>*)
    procedure SetSubRange(const Src: TOpenCLMtxVec; Index: integer; Len: integer = MtxVecEOA); overload;

    (*<summary>Defines a sub-vector.</summary>
      
<remarks>Define a subvector of the Src vector starting at BeginIndex and ending at EndIndex (inclusive).
</remarks>


      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetFullRange"/>*)
     procedure SetSubIndex(const Src: TOpenCLMtxVec; BeginIndex, EndIndex: integer); overload;

    (*<summary>Automatically set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TOpenCLMtxVec method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read GetIsSubRange;

    (*<summary>Resets any defined subrange.</summary>
      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange;

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted or indices adjusted.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised if an attempt is made to change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on the
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(Index: integer; Len: integer); overload;


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
        TOpenCLVector a,b;

        clMtxVec.CreateIt(out a, out b);
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
      
<remarks>Enables calls to <see cref="SetSubRange"/> by removing the block set by <see cref="DisableSubrange"/>.
</remarks>
*)
    procedure EnableSubrange;

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
        TOpenCLVector a,b;

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
      var a,b: TOpenCLVector;
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
    procedure SetSubIndex(BeginIndex, EndIndex: integer); overload;

    procedure Adopt(const Src: TOpenCLVector); overload;

    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec object.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));  // a = [1,2,3,4]
          b.Copy(a);                // b = [1,2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Assign"/>*)

    function Copy(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] in the calling object
       elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size and <see cref="Complex"/> properties must be set explicitly. An exception is raised if
       array borders are overrun or underrun.
</remarks>
*)
    function Copy(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec2 elements to corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="Complex"/> properties
      of the calling object are set implicitly to match Vec1 and Vec2 vectors.
</remarks>
*)
    function Add(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec1 elements [Vec1Index]..[Vec1Index+Len-1] to Vec2 elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is
      raised if true and array borders are overrun.
</remarks>
*)
    function Add(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Adds Value to object elements.</summary>

      <SeeAlso cref="Sub"/>*)
    function Add(const Value: double): TOpenCLMtxVec; overload;

    (*<summary>Adds complex Value to all calling object complex elements.</summary>*)
    function Add(const Value: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to the each element of the Vec object.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set automatically.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each element of the Vec object.</summary>
      
<remarks>Store the result to the calling object. Size property of the calling object is set
      automatically. <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Value to each element of Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each elements of the Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array addition.</summary>
      
<remarks>Add each of Vec elements to corresponding elements in the calling object.
</remarks>

      <SeeAlso cref="Sub"/>*)
    function Add(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Split complex calling object in real and imaginary part.</summary>
      
<remarks>Split calling object into real and imaginary components. Store all real components in ReVec and
      all imaginary components in ImVec. Size and <see cref="Complex"/> properties of ReVec and ImVec
      are set implicitly to match with the calling vector. An execption is raised if calling object is not complex.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4));
          a.CplxToReal(b,c);
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLMtxVec); overload;
    (*<summary>Split calling object elements [Index]..[Index+Len-1] into real and imaginary components.</summary>
      
<remarks>Store real components in ReVec elements [ReIndex]..[ReIndex+Len-1] and imaginary components in ImVec elements
      [ImIndex]..[ImIndex+Len-1]. Size and <see cref="Complex"/> properties must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer); overload;

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="Complex"/> property is True.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          b.CopyFromArray(TSingleArray.Create(2,2,3,4));
          c.RealToCplx(a,b);
      end;
      </code>
      </Example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(ReVec, ImVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Construct a complex object from the ReVec elements [ReIndex]..[ReIndex+Len-1] (real part) and the ImVec
      elements [ImIndex]..[ImIndex+Len-1] (imaginary part).</summary>
      
<remarks>The results are stored to calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if array borders
      are overrun. An exception is also raised if ReVec or ImVec <see cref="Complex"/> property is True.
</remarks>
*)
    function RealToCplx(ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer): TOpenCLMtxVec; overload;

   (*<summary>Conjugate.</summary>
      
<remarks>Conjugate all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var c: clVector;
      begin
          c.CopyFromArray(TSingleArray.Create(1,2,3,4));  // c = [1+2i, 3+4i]
          c.Conj;                   // c = [1-2i, 3-4i]
      end;
      </code>
      </Example>*)
    function Conj: TOpenCLMtxVec; overload;
    (*<summary>Conjugate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Conj(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec elements.</summary>
      
<remarks>Store the results in the calling object. The
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match Vec vector.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1].</summary>
      
<remarks>Store them in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Extends a real object to a complex object.</summary>
      
<remarks>Extend the calling object to complex vector. After the calling of ExtendToComplex the imaginary part becomes the same as real part if Zeros
      is false. If Zeros is true the imaginary part is set to zero. The use of the in-place version of the method is discouraged because
      it requires 3*N copy operations, while the not-in-place version requires only 2*N copy operations.
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          b.ExtendToComplex(a,True);
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>
      <SeeAlso cref="ImagPart"/>
      <SeeAlso cref="RealPart"/>*)
    function ExtendToComplex(Zeros: boolean = True): TOpenCLMtxVec; overload;
    (*<summary>Extend Vec object to complex calling object.</summary>
      
<remarks>If Zeros is true then the calling vector imaginary part is set to zero, otherwise
      the calling object imaginary part is the same as calling object real part.
</remarks>
*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean): TOpenCLMtxVec; overload;

    (*<summary>Converts the source to complex.</summary>
      
<remarks>Converts the source to complex by setting the imaginary part to either zero (zeros = True)
      or same as real (zeros = false). Does not set size of the calling vector.
      If there is not sufficient space available to store the result an exception
      will be raised.
</remarks>
*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean; SrcIndex,DstIndex, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.

     Sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is not used
     implicitely within TOpenCLMtxVec methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle: TOpenCLMtxVec; overload;

    (*<summary>FixAngle for calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is True or
      if array borders are overrun/underrun.
</remarks>
*)
    function FixAngle(Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in <c>[-2PI,2PI]</c> for all Src elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the Src object.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in [-2PI,2PI] for Src elements [SrcIndex]..[SrcIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly. An exception is raised
      if array borders are overrun.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Rounds towards negative infinity.</summary>
      
<remarks>Rounds all calling object elements towards negative infinity in-place.

      <c>Floor(-2.8) = -3</c><para/>
      <c>Floor(2.8) = 2</c><para/>
      <c>Floor(-1.0) = -1</c><para/>
</remarks>
*)
    function Floor: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards negative infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards negative infinity and stores the result
      in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards negative infinity
      and stores the result in the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate the calling object complex exponential in-place. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.Expj;   // a = [e^i, e^2i, e^3i, e^4i]
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)
    function Expj: TOpenCLMtxVec; overload;
    (*<summary>Calculate the e^(j*Omega), a complex exponential.</summary>
      
<remarks>Omega must be a real object. If omega is complex, then use the <see cref="Exp"/> method.
</remarks>
*)
    function Expj(Omega: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the complex exponential for Omega elements [OmegaIndex]..[OmegaIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elemets [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Expj(Omega: TOpenCLMtxVec; OmegaIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
        ind: Integer;
      begin
          a.CopyFromArray(TSingleArray.Create(1,5.5,-1.6,6)); // a = [1, 5.5, -1.6, 6]
          a.Frac;  // a = [0, 0.5, -0.6, 0]
      end;
      </code>
      </Example>

      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac: TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for all X object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Complementary error functon of values.</summary>
      
<remarks>Calculates the complementary error function value for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,5.5,-1.6,6)); // a = [1, 5.5, -1.6, 6]
          a.Erfc;
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc: TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Error functon of values.</summary>
      
<remarks>Calculates the error function for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
        ind: Integer;
      begin
          a.CopyFromArray(TSingleArray.Create(1, 5.5, -1.6, 6)); // a = [1, 5.5, -1.6, 6]
          a.Erf;
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf: TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Flips the real and imaginary part of complex numbers.</summary>
      <returns>Flipped real and imaginary part of complex numbers for all calling object elements in-place.<para/>
        The following transformation is used: <c>a + i*b -> b + i*a</c></returns>

      <SeeAlso cref="FlipConj"/>*)
    function Flip: TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for all X object elements.</summary>
      
<remarks>Xtore the results in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Flips the real and imaginary part of complex numbers and conjugates the complex numbers.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling object complex elements in-place.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj: TOpenCLMtxVec; overload;
    (*<summary>Flip calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is false or if array borders
      are overrun/underrun.
</remarks>
*)
    function FlipConj(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flip all X object complex elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object
      are adjusted automatically.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flip X object complex elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if calling
      object <see cref="Complex"/> property is false or if array borders are overrun/underrun.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(x[i]/y[i]).</c><para/>
      The results will be saved to the calling vector.
      X and Y must be a real and have the same length. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(0,1,10,-1,-10)); // a = [0, 1, 10,    -1, -10];
          b.CopyFromArray(TSingleArray.Create(0,1,PI,-1,-PI)); // b = [0, 1, PI,    -1, -PI];
          c.Rem(a,b);                     // c = [0, 0, 0.5752, 0, -0.5752]
      end;
      </code>
      </Example>*)
    function Rem(const X, Y: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Calculation uses the following formula:

      <c>z[i] = x[xi]-y[yi]*Trunc(x[xi]/y[yi]),</c><para/>
      where i in [Index..Index+Len], xi in [XIndex..XIndex+Len], yi in [YIndex..YIndex+Len].
      The results will be saved to the calling vector. X must be a real. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X, Y: TOpenCLMtxVec; XIndex, YIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector.
      Size and <see cref="Complex"/> properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; Y: double): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; Y: double; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiply all calling object elements with Value in-place.
      This method is the same as the <see cref="Mul"/> method overloads
      multiplying with vector elements with a scalar.
</remarks>


      <Example>
      <code>
      var v: clVector;
      begin
          v.CopyFromArray(TSingleArray.Create(2,3,5));  // v = [2,3,5]
          v.Scale(3); // v = [6,9,15]
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>*)
    function Scale(Factor: double): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(Factor: double; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all calling object elements with a complex Value in-place.</summary>*)
    function Scale(const Factor: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(const Factor: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    









































    

    (*<summary>Log base N.</summary>
      <returns>Log base N for all calling object elements in-place.</returns>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4)); // a = [1,2,3,4]
          a.LogN(10.0);  // log base 10, the slow way a = [Log10(1), Log10(2),...]
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function LogN(N: double): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(N: double; Index, Len: integer):TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function LogN(N: double; X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(N: double; X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;



    (*<summary>Normalize object.</summary>
      
<remarks>Normalizes Vec object by subtracting a constant Offset from Vec elements and dividing the result by constant Factor:

      <IMG name="TVec21"/>

      The results are stored in calling object. Use this method if you want to do a multiply and add (scale and offset) operations in a single method call.
</remarks>

      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          b.Normalize(a,2,3);
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>
      <SeeAlso cref="Scale"/>*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset, DivFactor: double): TOpenCLMtxVec; overload;
    (*<summary>Normalize Vec object values [VecIndex]..[VecIndex+Len-1] by subtracting a real constant SubOffset from Vec elements and dividing the result by
      complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset, DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Normalize Vec object by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset: TCplx; DivFactor: double): TOpenCLMtxVec; overload;
     (*<summary>Normalize Vec object complex values [VecIndex]..[VecIndex+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset: TCplx; DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;




     (*<summary>Raises base object elements to any power.</summary>
      
<remarks>Raises Base calling object elements to any power. The <see cref="IntPower"/> is faster, if Exponent is an integer.
      Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
      To compute a power to the negative exponent in general case or when the base is negative,
      use the complex version of the function.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.Power(1.2);
      end;
      </code>
      </Example>

      <SeeAlso cref="IntPower"/>
      <SeeAlso cref="PowerVec"/>*)
    function Power(Exponent: double): TOpenCLMtxVec; overload;

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
    function Power(const Exponent: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Raises base elements to exponent power.</summary>
      
<remarks>Raises Base value to Exponent object values powers and store the results to calling object values.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>


      <SeeAlso cref="PowerVec"/>*)
    function Power(const aBase: double; const Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Raises Base complex value to Exponent object values powers.</summary>
      
<remarks>Store the results to calling object values. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TCplx; const Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to complex Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TOpenCLMtxVec; const Exponent: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to real Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TOpenCLMtxVec; const Exponent: double): TOpenCLMtxVec; overload;
    (*<summary>Raises each of Base object elements to corresponding power, stored in Exponenet elements.</summary>

      
<remarks><c>Power[i] = Base[i]^Exponent[i]</c><para/>

      Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised if Base and Exponent sizes do not match.
</remarks>
*)
    function Power(const aBase, Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises Base object elements to Exponent object elements power.</summary>
      
<remarks>Raises Base elements to Exponent elements power. Only positive exponents can be handled
      if exponent object <see cref="Complex"/> property is True.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          b.CopyFromArray(TSingleArray.Create(3,3,2,2));
          c.PowerVec(a,b); // c = [1,8,9,16]
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function PowerVec(const aBase, Exponent: TOpenCLMtxVec): TOpenCLMtxVec;

    

















    

    (*<summary>Elements rounded to the nearest whole number.</summary>
      
<remarks>Rounds all calling object elements to the nearest whole number.
      The result can be stored to an array of integers or as floating
      point number.
</remarks>


      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Frac"/>*)
    function Round: TOpenCLMtxVec; overload;
    (*<summary>Round calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Round all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/> property of
      calling object are adjusted automatically.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Round Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> property of the calling object must be set explicitly.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      <Example>
      <code>
      var a: clVector;
          c: double;
          aMean: double;
      begin
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          Caption := FloatToSTr(a.RMS);
      end;
      </code>
      </Example>


      <SeeAlso cref="Mean"/>*)
    function RMS: double; overload;

    (*<summary>Initialize elements to Value.</summary>
      
<remarks>Set all calling object elements to Value. If the calling object is complex
      then the real part is set to Value and the imaginary is set to zero.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
        a.Size(4,False);
        a.SetVal(1); // a = [1,1,1,1]
      end;
      </code>
      </Example>

      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: double): TOpenCLMtxVec; overload;

    (*<summary>Set all calling object elements [Index]..[Index+Len-1] to real Value.</summary>
      
<remarks>If the calling object is complex then the real part is set to Value and the imaginary is set to zero.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Set all calling object complex elements to complex Value.</summary>*)
    function SetVal(const Value: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Set calling object complex elements [Index]..[Index+Len-1] to complex Value.</summary>
       
<remarks><see cref="Complex"/> property of the calling object are set to true even before the call it was false.
       An exception is raised if calling object array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: TCplx; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Initializes object elements to zero.</summary>
      <SeeAlso cref="SetVal"/>*)
    function SetZero: TOpenCLMtxVec; overload;
    (*<summary>Initializes calling object elements [Index]..[Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function SetZero(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Changes elements sign.</summary>
      
<remarks>Changes all calling object elements sign <c>(v -> -v)</c> in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
        a.CopyFromArray(TSingleArray.Create(1,2,-3,4));
        a.Sign; // a = [-1,-2,3,-4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Mul"/>*)
    function Sign: TOpenCLMtxVec; overload;
    (*<summary>Change calling object elements [Index]..[Index+Len-1] sign in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sign(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Change all X object elements sign.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Change X object elements [XIndex]..[XIndex+Len-1] sign.</summary>
      
<remarks>Store the results in callingobject elements [Index]..[Index+Len-1]. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec; XIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)

    function Sgn: TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements [Index..Index+Len-1]. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements and stores the result in the calling object.  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements [SrcIndex..SrcIndex+Len-1] and stores the result in
                 the calling object [Index..Index+Len-1].  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of all Src object elements and multiplies
      it with the calling object elements accordingly.
      Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      The length of Src and of the calling object must match
      or an exception will be raised. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine for all calling object elements and stores the sines
      to SinX and cosines to CosX. Size and <see cref="Complex"/> property of SinX and CosX are
      adjusted automatically.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      <Example>
      <code>
      var a: clVector;
          s,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(0,PiDiv2,PI));
          a.SinCos(s,c); // s=[0,1,0], c =[1,0,-1]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(SinX, CosX: TOpenCLMtxVec); overload;
    (*<summary>Calculates the sine and cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>stores the sines to SinX elemets [SinXIndex]..[SinXIndex+Len-1] and cosines to CosX elements [CosXIndex]..[CosXIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinX and CosX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinCos(SinX, CosX: TOpenCLMtxVec; SinXIndex, CosXIndex, Index, Len: integer); overload;

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for all calling object elements and stores
      the sines to SinhX and cosines to CoshX. Size and <see cref="Complex"/> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      <Example>
      <code>
      var a: clVector;
          s,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(0,PiDiv2,PI));
          a.SinhCosh(s,c);
      end;
      </code>
      </Example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLMtxVec); overload;
    (*<summary>Calculates the hyperbolic sine and hyperbolic cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the sines to SinhX elemets [SinhIndex]..[SinhIndex+Len-1] and cosines to CoshX elements [CoshIndex]..[CoshIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinhX and CoshX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLMtxVec; SinhIndex, CoshIndex, Index, Len: integer); overload;


        (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is a lower bound for threshold operation.
      All values smaller than Value will be replaced with Value.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(2,0.1,3,4));
          a.ThreshBottom(0.2); // a = [2,0.2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshBottom(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(2,0.1,3,4));
          a.Threshtop(0.2); // a = [0.2,0.1,0.2,0.2]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshTop(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for
      threshold operation.
      All values less than LTLevel will be replaced with LTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      <example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(2,0.1,3,4));
          a.ThresholdLT(2.3,1.5); // a = [1.5,1.5,3,4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdLT(LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(LTLevel: double; const LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(LTLevel: double; const LTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel: double; const LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel: double; const LTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on all calling object values. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than LTLevel will be replaced with GTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      <example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(2,0.1,3,4));
          a.ThresholdGT(2.3,3.4); // a = [2,0.1,3.4,3.4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT(GTLevel, GTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel, GTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(GTLevel: double; const GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(GTLevel: double; const GTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel: double; const GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel: double; const GTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values less than LTLevel will be replaced with LTValue. All values bigger than GTLevel will be replaced with GTValue.
      Operation is available only for none Complex values.
</remarks>


      <example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(2,0.1,3,4));
          a.ThresholdGT_LT(2.3,3.4,1,0.5); // a = [2,0.5,3.4,3.4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than and less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      An exception will be raised if the calling object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (GTLevel, GTValue, LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      An exception will be raised if Vec object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const Vec: TOpenCLMtxVec; GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec object or the calling object contain complex numbers.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TOpenCLMtxVec; GTLevel, GTValue, LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the TruncDst object as floating point numbers. The fractional
      part is stored in the FracDst.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)

    procedure TruncAndFrac(TruncDst: TOpenCLMtxVec; FracDst: TOpenCLMtxVec); overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] and store the results to TruncDst object elements
      [TruncIdx]..[TruncIdx+Len-1].</summary>
      
<remarks>The fractional parts are saved in FracDst elements [FracIdx]..[FracIdx+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src object. An exception is raised if
      array borders are overrun/underrun.
</remarks>
*)
    procedure TruncAndFrac(TruncDst: TOpenCLMtxVec; FracDst: TOpenCLMtxVec; TruncIdx, FracIdx, Index, Len: integer); overload;

    (*<summary>Rounds a real number towards zero.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the calling object again as floating point numbers.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)
    function Trunc: TOpenCLMtxVec; overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Truncate all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/>
      property of calling object are adjusted automatically.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Truncate Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elemenents [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src
      object. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of all calling object elements in-place.
</remarks>


      <Example>
      <code>
        v.CopyFromArray(TSingleArray.Create(1,8));
        v.InvCbrt; // returns [1,0.5]
      </code>
      </Example>

      <SeeAlso cref="Cbrt"/>*)



    function InvCbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      the calling vector are set implicitly to match the X object.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2))</c> of all calling object elements in-place.
</remarks>


      <Example>
      <code>
        v.CopyFromArray(TSingleArray.Create(1,16));
        v.InvSqrt; // returns [1,0.25]
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvSqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the calling vector are set
       implicitly to match the X object.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude for all calling object elements in-place.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4)); // a = [1,-2, 3,4]
          a.Mag;      // a = [1, 2, 3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Abs"/>
      <SeeAlso cref="PhaseSpectrum"/>*)
    function Mag: TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for all X elements.</summary>
      
<remarks>Store the results in the calling object elements. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match Vec vector.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all Vec1 elements with corresponding Vec2 elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="Complex"/> property do not match.
</remarks>
*)
    function Mul(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Vector multiplication.</summary>
      
<remarks>Multiply each of Vec elements with corresponding elements in the calling object.
      Size and <see cref="Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.
</remarks>


      <SeeAlso cref="Divide"/>*)
    function Mul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiplies all calling object elements with Value in-place.
</remarks>


      <Example>
      <code>
      var v: clVector;
      begin
          v.CopyFromArray(TSingleArray.Create(2,3,5));  // v = [2,3,5]
          v.Mul(3); // v = [6,9,15]
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>*)
    function Mul(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply all calling object elements with complex Value in-place.</summary>*)
    function Mul(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with
      complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with Value.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with complex Value.</summary>
      
<remarks>Store the result in the calling object. Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set implicitly.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with complex Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply elements by imaginary unit I.</summary>*)
    function MulI: TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with I in-place.</summary>*)
    function MulI(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply X elements with I and store the result in the calling object.</summary>*)
    function MulI(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Multipy X elements [XIndex]..[XIndex+Len-1] with I.</summary>
      
<remarks>Xtore the result in the calling object at locations [Index]..[Index+Len-1].
</remarks>
*)
    function MulI(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The phase angles (spectrum) of object elements.</summary>
      
<remarks>Calculates the phase angles (spectrum) of all Vec object elements. Phase values are returned in radians and are in the range
      -PI,PI. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match Vec object. The phase angles are calculated from the following equation:

      <IMG name="TVec23"/>
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,-4));  // a = [1 + 2i, 3 - 4i]
          b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
      end;
      </code>
      </Example>

      <SeeAlso cref="PowerSpectrum"/>
      <SeeAlso cref="ArcTan2"/>*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised
      if array borders are overrun/underrun.
</remarks>
*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The power spectrum from object complex values.</summary>
      
<remarks>Calculates the power spectrum from the Vec object complex values and stores the results (power spectrum)
      in the real calling object. Size and <see cref="Complex"/> properties of the calling object are set
      implicitly to match Vec object. The spectrum elements are squares of the magnitudes of the complex input elements:

      <IMG name="Tvec22"/>
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,-4)); // a = [1 + 2i, 3 - 4i]
          b.PowerSpectrum(a);   // b = [1*1 + 2*2, 3*3+(-4)*(-4)]
      end;
      </code>
      </Example>

      <SeeAlso cref="PhaseSpectrum"/>*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object real elements [Index]..[Index+Len-1]. An exception is raised
      if calling object <see cref="Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts the polar magnitude/phase pairs to cartesian pairs.</summary>
      
<remarks>Convert all AmpltVec and PhaseVec elements (combined) from polar to cartesian form. If AmpltVec and PhaseVec size is not the same
      , an exeption is raised. The results are stored as complex numbers (x=Re, y=Im) in the calling
      object.  Size and <see cref="Complex"/> properties of the calling object are set implicitly to
      match AmpltVec and PhaseVec objects.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));  // a = [1,2,3, 4] //magnitude
          b.CopyFromArray(TSingleArray.Create(1,0,1,-1)); // b = [1,0,1,-1] /phase
          c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
      end;
      </code>
      </Example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Convert  AmpltVec elements [AIndex]..[AIndex+Len-1] and PhaseVec elements [PIndex]..[PIndex+Len-1] from polar form
      (radius,angle) to cartesian form (x,y).</summary>
      
<remarks>The results are stored as complex numbers <c>(x=Re, y=Im)</c> in the calling
      object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLMtxVec; AIndex, PIndex,Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets real part of complex object values.</summary>
      
<remarks>The method method gets the real part of a complex object Vec and stores the real results in the calling
      object. Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4)); // = [1+2i, 3+4i]
          b.RealPart(a); // b = [1,3]
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Gets the real part of a Vec object complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the results in calling object real elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun or if Vec object <see cref="Complex"/>
      propety is false.
</remarks>
*)
    function RealPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Square.</summary>
      
<remarks>Calculate the square of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.Sqr; // a=[1,4,9,16]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> properties of the calling object are not changed.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,4,9,16));
          a.Sqrt; // a= [1, 2, 3, 4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling object are adjusted automatically.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4));
          a.Sin; // Computes complex sine
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Sin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of all X object elements and store the results in calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosine.</summary>
      
<remarks>Calculate the cosine of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4));
          a.Cos; // Computes complex sine
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCos"/>
      <SeeAlso cref="SinCos"/>*)
    function Cos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object.vSize and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object and the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4));
          a.Tan; // Computes complex tangens
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)
    function Tan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan (Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan(const X : TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyCplxFromArray(TSingleArray.Create(1,-2,3,4));
          a.Cot; // Computes complex cotangens
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)
    function Cot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyCplxFromArray(TSingleArray.Create(1,-2,3,4));
          a.Sec; // Computes complex secant
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)
    function Sec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,-2,3,4));  // a = [1-2i, 3+4i]
          a.Csc; // Computes complex cosecant
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)
    function Csc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
        a.CopyFromArray(TSingleArray.Create(1,-0.5,0.11,0.9));
        a.ArcSin;
      end;
      </code>
      </Example>

      <SeeAlso cref="Sin"/>*)
    function ArcSin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
        a.CopyFromArray(TSingleArray.Create(1,-0.5,0.11,0.9));
        a.ArcCos;
      end;
      </code>
      </Example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks><para/>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object elements. Size and <see cref="Complex"/> properties of calling object are adjusted automatically
      to match those of X and Y objects. An exception is raised if X and Y size and <see cref="Complex"/> properties do not match.

      Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(Y, X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of Y/X.</summary>
      
<remarks>Calculation uses Y elements [YIndex]..[YIndex+Len-1], X elements [XIndex]..[XIndex+Len-1]
      and stores the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan2(Y, X: TOpenCLMtxVec; YIndex, XIndex, Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyCplxFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.ArcTan(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyCplxFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.ArcCot(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse secant.</summary>
      
<remarks>Calculate the inverse secant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Sec"/>
      <SeeAlso cref="ArcCsc"/>*)
    function ArcSec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse secant of all X object elements and store the results in the calling object. </summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cosecant.</summary>
      
<remarks>Calculate the inverse cosecant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Csc"/>
      <SeeAlso cref="ArcSec"/>*)
    function ArcCsc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.Sinh(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.Cosh(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.Tanh(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Tanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));
        B.Coth(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
   (*<summary>Calculate the hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic secant.</summary>
      
<remarks>Calculate the hyperbolic secant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcSech"/>
      <SeeAlso cref="Csch"/>*)
    function Sech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosecant.</summary>
      
<remarks>Calculate the hyperbolic cosecant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcCsch"/>
      <SeeAlso cref="Sech"/>*)
    function Csch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Absolute values.</summary>
      
<remarks>Calculate the absolute value of all calling object elemets in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,-2,3,4));
        B.Abs(A);  // B = [1,2,3,4]
      end;
      </code>
      </Example>


      <SeeAlso cref="Mag"/>*)
    function Abs: TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for all X object and store the results in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic sine.</summary>
      
<remarks>Calculate the inverse hyperbolic sine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sinh"/>*)
    function ArcSinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosine.</summary>
      
<remarks>Calculate the inverse hyperbolic cosine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Cosh"/>*)
    function ArcCosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic tangens.</summary>
      
<remarks>Calculate the inverse hyperbolic tangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Tanh"/>*)
    function ArcTanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
        
<remarks>The results are storedi n the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
        properties of the calling object must be set explicitly.
        An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cotangens.</summary>
      
<remarks>Calculate the inverse hyperbolic cotangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Coth"/>*)
    function ArcCoth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverser hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic secant.</summary>
      
<remarks>Calculate the inverse hyperbolic secant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sech"/>*)
    function ArcSech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosecant.</summary>
      
<remarks>Calculate the inverse hyperbolic cosecant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Csch"/>*)
    function ArcCsch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,8));
        B.Cbrt(A);  // B = [1,2]
      end;
      </code>
      </Example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of all X object elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds all calling object elements towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards positive infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;
    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object elements [Index]..[Index+Len-1]
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,2,3,4));
        B.Ln(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)
    function Ln: TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural algorithm for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties
      of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ln(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(10,100,1000,10000));  // a = [10,100,1000,10000]
          a.Log10;     // a = [1,2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp10"/>*)
    function Log10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,2,3,4));
        B.Log2(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp2"/>*)
    function Log2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent (e^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,2,3,4));
        B.Exp(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Ln"/>*)
    function Exp: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 2 (2^).</summary>
      
<remarks>Calculate the exponent base 2 (2^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,2,3,4));
        B.Exp2(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Log2"/>*)
    function Exp2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 10 (10^).</summary>
      
<remarks>Calculate the exponent base 10 (10^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var A,B: clVector;
      begin
        A.CopyFromArray(TSingleArray.Create(1,2,3,4));
        B.Exp10(A); // B = [10, 100, 1000, 10 000]
      end;
      </code>
      </Example>

      <SeeAlso cref="Log10"/>*)
    function Exp10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets the imaginary part of a complex object.</summary>
      
<remarks>Gets the imaginary part of a complex object Vec and stores the real results in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> must be true otherwise the exception will be raised.
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyCplxFromArray(TSingleArray.Create(1,2,3,4));  // a= [1+2i, 3+4i]
          b.ImagPart(a);  // b = [2, 4]
      end;
      </code>
      </Example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Gets the imaginary part of complex object Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object. An exception is raised if the calling object is complex, if Vec is not complex or
      if array borders are overrun/underrun.
</remarks>
*)
    function ImagPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power ^(Exponent) for all caling object elements using the integer parameter Exponent.
      For non integer exponents, the <see cref="Power"/> and <see cref="PowerVec"/> methods can be used.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.IntPower(3);
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>
      <SeeAlso cref="PowerVec"/>*)
    function IntPower(Exponent: Integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the power Base^(Exponent) for all Base object elements.</summary>
      
<remarks>Calclation uses the integer Exponent value and stores the results in calling object. Size and
      <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function IntPower(const aBase: TOpenCLMtxVec; Exponent: Integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse elements.</summary>
      
<remarks>Calculates the inverse of all calling object elements in-place without limiting inverse operation.
</remarks>


      <Example>
      <code>
      var a,b: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));  // a = [1, 2, 3, 4]
          b.Inv(a); // b = [1.0, 0.5, 0.3333, 0.25]
      end;
      </code>
      </Example>

      <SeeAlso cref="Divide"/>*)
    function Inv: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Inv(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse of all X object elements without limiting  operating.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Inv(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of X object elements [XIndex]..[XIndex+Len-1] without limiting  operating.</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if X and calling object <see cref="Complex"/> property
      does not match or array borders are overrun/underrun.
</remarks>
*)
    function Inv(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts elements from cartesian to polar coordinate form.</summary>
      
<remarks>Converts all calling object elements from cartesian to polar coordinate form, storing the magnitude (radius)
      component of corresponding elements in the AmpltVec and the phase (angle) component of corresponding elements in
      the PhaseVec. If you want to use this method then the calling matrix <see cref="Complex"/> property must be
      true. If this is not the case, an exception is raised. Size and <see cref="Complex"/> properties of AmpltVec and
      PhaseVec are set automatically.
</remarks>


      <Example>
      <code>
      var A,Amplt,Phase: clVector;
      begin
            A.CopyCplxFromArray(TSingleArray.Create(1,0, 2,0, 2,0  4,1));  // 4, complex matrix
            A.CartToPolar(Amplt,Phase);
      end;
      </code>
      </Example>

      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLMtxVec); overload;
    (*<summary>Convert calling object elements [Index] to [Index+Len-1] from cartesian to polar form.</summary>
      
<remarks>Store the results in AmpltVec (radius values) and PhaseVec(phase values). Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLMtxVec; AmpltIndex, PhaseIndex, Index, Len: integer); overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: double): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition,  the following formula is being used:

      <c>result = result+ Cplx(aScale)*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Add a product of two vectors.</summary>
      
<remarks>Multiply Vec1 elements with coresponding Vec2 elements and add the result
      to the calling vector. The size of the calling vector is set implicitly.
</remarks>


      <SeeAlso cref="Mul"/>
      <SeeAlso cref="Add"/>*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and
      add the results to the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Conjugate and multiply.</summary>
      
<remarks>Conjugate each of Vec elements and multiply them with corresponding elements in the calling object.
      The results are stored in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          b.CopyFromArray(TSingleArray.Create(4,3,2,1));
          c.ConjMul(a,b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Conj"/>
      <SeeAlso cref="Mul"/>*)
    function ConjMul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1] and multiply them with corresponding
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties are <b>not</b> set.

      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function ConjMul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec2 elements and multiply them with corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match Vec1 and Vec2 objects.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and multiply them with corresponding Vec1 elements
      [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling vector must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide each of Num elements with corresponding elements in Den.</summary>
      
<remarks>Size and <see cref="Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide each of calling vector elements with corresponding elements in the Vec object.</summary>
      
<remarks>Size and <see cref="Complex"/> property of the calling object are set automatically. The result
      is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide calling vector elements [Index]...[Index+Len-1] with corresponding elements
       [VecIndex]...[VecIndex+Len-1] from the Vec object.</summary>
       
<remarks>Store the result in the claling vector. The <see cref="TMtxVecBase.Length">Length</see>
       of the calling vector is not changed. An exception is raised if
       array borders are overrun or underrun.

       The result of division by zero will be the INF constant. Division of zero
       by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide [NumIndex]..[NumIndex+Len-1] Num elements with [DenIndex]..[DenIndex+Len-1] elements in Den.</summary>
      
<remarks>and store result in the calling vector at positions [Index]..[Index+Len-1]
      <see cref="Size"/> and <see cref="Complex"/> property of the calling object are not changed.
      An exception is raised if array borders are overrun or underrun.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLMtxVec; NumIndex, DenIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide Value with elements of the calling object and store the result in the calling object.</summary>*)
    function DivideBy(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements of the calling object.</summary>
      
<remarks>Store the result in the calling object.
</remarks>
*)
    function DivideBy(Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      Calling vector will be extended to complex, if the calling vector is real.
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object are set automatically.
</remarks>
*)
    function DivideBy(Value: double; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with Vec elements [VecIndex]..[VecIndes+Len-1].</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception will be raised array borders are overrun or underrun.
      <see cref="Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function DivideBy(Value: double; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [VecIndex]..[VecIndes+Len-1] from Vec.</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object. Size of the calling object is not changed. An exception will be raised
       array borders are overrun or underrun.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverse vector elements.</summary>
      
<remarks>The method reverses Vec vector elements from [VecIndex].. [VecIndex+Len-1]
      and stores them in the calling vector from [Index]...[Index+Len-1]
      by using the following equation:<para/>

      <IMG name="TVec24"/><para/>

      This overload reverses calling vector elements in-place.
</remarks>


      <Example>
      <code>
      var a: clVector;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.Reverse;   // a = [4,3,2,1]
      end;
      </code>
      </Example>


      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(Index, Len: integer): TOpenCLMtxVec; overload;
    function Reverse: TOpenCLMtxVec; overload;

    (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>

      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Rotate(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TOpenCLMtxVec; overload;

    (*<summary>A shift on vector elements in range.</summary>
     
<remarks>Performs shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Shift(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TOpenCLMtxVec; overload;
    function Shift (const Vec: TOpenCLMtxVec; Offset: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtracts Value from object elements.</summary>
      
<remarks>Subtracts Value from all calling object elements.
</remarks>


      <SeeAlso cref="Add"/>*)
    function Sub(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from all calling object complex elements.</summary>*)
    function Sub(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtracts Value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtract real Value from Src.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract real Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: double; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: TCplx; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array subtraction.</summary>
      
<remarks>Subtract each of Vec elements from corresponding elements in the calling object.
      An exception is raised if Vec and calling object size and <see cref="Complex"/> properties do not match.
</remarks>

      <SeeAlso cref="Add"/>*)
    function Sub(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec2 elements from Vec1 elements.</summary>
      
<remarks>Stores the results in calling object. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically. An exception is raised if Vec1 and Vec2 size and <see cref="Complex"/> property do not match.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec elements [VecIndex]..[VecIndex+Len-1] from corresponding calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec22 elements [Vec2Index]..[Vec2Index+Len-1] from Vec1 object elements [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>Stores the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtraction from value.</summary>
      
<remarks>Subtract each of calling object elements from Value.
</remarks>


      <SeeAlso cref="Add"/>
      <SeeAlso cref="Sub"/>*)
    function SubFrom(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract each of calling object elements from complex Value.</summary>
      
<remarks>If the calling vector s not complex, the conversion is performed automatically in a
      performance efficient way.
</remarks>
*)
    function SubFrom(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from Value.</summary>
      
<remarks>Store the result in calling vector. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(Value: double; Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from complex Value.</summary>
      
<remarks>Store the result in calling object. If the calling vector is not complex, the conversion to complex is performed
      automatically in performance efficient way. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(const Value: TCplx; Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract complex Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size property of the calling object is set
      automatically. <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from Value.</summary>
      
<remarks>Stores the result to the calling object elements [Index]..[Index+Len-1].
       Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="Complex"/> property of the calling object is adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from complex Value and store the result to the
       calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    procedure Size(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); overload;
    procedure Size(const Src: TOpenCLMtxVec; aComplex: boolean); overload;
    procedure Size(const Src: TOpenCLBase); overload;

    (*<summary>Calculate the difference for all Vec elements.</summary>
      
<remarks>Store the results in the calling vector. The <see cref="TOpenCLBase.Length">Length</see>
      of the calling vector is set to one less the length of Vec and <see cref="TOpenCLBase.Complex"/>
      property is set to Vec.Complex.

      <IMG namr="tvec19"/><para/>

      The Length of calling vector is automatically decremented by one.
</remarks>
*)
    function Difference(const Vec: TOpenCLMtxVec; Lag: Integer = 1): TOpenCLMtxVec; overload;

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
      var a: clVector;
      begin
          a.Size(5,True);
          a.Ramp(0,PI);
          a.Sin;
      end;
      </code><para/>
      which is identical to:<para/>
      <code>
          a.Size(5,True);
          for i:= 0 to a.Length-1 do a[i] := sin(i*PI);
      </code>
      </Example>

      <SeeAlso cref="SetVal"/>*)
    function Ramp: TOpenCLVector; overload;
    (*<summary>Fills the calling vector with a series.</summary>
      
<remarks>Method follow the rule:

      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
</remarks>
*)
    function Ramp(Offset, Step: TCplx): TOpenCLVector; overload;
    (*<summary>Fills the calling vector.</summary>
      
<remarks>Method uses the following rule:

      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      If the calling vector is complex, only the real part is set.
</remarks>
*)
    function Ramp(Offset, Step: double): TOpenCLVector; overload;
    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Method uses the following rule:

      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      If the calling vector is complex, only the real part is set.
      An exception is raised if calling vector
      array borders are overrun.
</remarks>
*)
    function Ramp(Offset,Step: double; Index,Len: integer): TOpenCLVector; overload;
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
    function Ramp(Offset, Step: TCplx; Index,Len: integer): TOpenCLVector; overload;
    (*<summary>The Offset is complex, but the step is real.</summary>*)
    function Ramp(Offset: TCplx; Step: double): TOpenCLVector; overload;
    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Following the rule:
      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      The Offset is complex, but the step is real.
</remarks>
*)
    function Ramp(Offset: TCplx; Step: double; Index,Len: integer): TOpenCLVector; overload;

      (*<summary>Inserts zeroes between consecutive vector values.</summary>
        
<remarks>Inserts zeroes between consecutive vector values. The method copies the values from Src to the calling vector and
        places Factor-1 zeros between consecutive values. The <see cref="TOpenCLBase.Length">Length</see> and <see cref="TOpenCLBase.Complex">Complex</see>
        properties of the calling vector are set implicitly. Phase parameter defines the initial sample offset and must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: clVector;
        begin
            b.CopyFromArray(TSingleArray.Create(0,0,1,3,2));
            a.UpSample(b,2); // a = [0,0,0,0,1,0,3,0,2,0]
        end;
        </code>
        </Example>

        <SeeAlso cref="clVector.DownSample"/>
        <SeeAlso cref="TOpenCLMtxVec.DownSample"/>
        <SeeAlso cref="TOpenCLMtxVec.UpSample"/>*)
      function UpSample(const Src: TOpenCLMtxVec; Factor: integer; Phase: integer = 0): TOpenCLVector; overload;
      function UpSample(const Src: TOpenCLMtxVec; Factor, SrcIndex, Index, Len, Phase: integer): TOpenCLMtxVec; overload;

      (*<summary>Downsamples vector values.</summary>
        
<remarks>The methods copies only every Factor sample from the Src vector to the calling vector.
        The <see cref="TOpenCLBase.Length">Length</see> and <see cref="TOpenCLBase.Complex">Complex</see> properties
        of the calling vector are set implicitly. The phase parameter determines the initial sample offset.
        Phase must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: clVector;
        begin
            b.CopyFromArray(TSingleArray.Create(0,0,0,0,1,2,3,4,5,6));
            a.DownSample(b,2); // a = [0,0,1,3,5]
        end;
        </code>
        </Example>

        <SeeAlso cref="UpSample"/>
        <SeeAlso cref="TOpenCLVector.UpSample"/>*)
      function DownSample(const Src: TOpenCLMtxVec; Factor: integer; Phase: integer = 0): TOpenCLVector; overload;

      (*<summary>Reverse all Vec elements.</summary>
        
<remarks>Store the result in the calling vector elements. The <see cref="TOpenCLBase.Length">Length</see>
        and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling vector are
        set implicitly to match Vec vector.

        <IMG name="TVec24"/><para/>

        This overload reverses all vector elements.
</remarks>


        <Example>
        <code>
        var a: clVector;
        begin
            a.CopyFromArray(TSingleArray.Create(1,2,3,4));
            b.Reverse(a);   // b = [4,3,2,1]
        end;
        </code>
        </Example>

        <SeeAlso cref="Rotate"/>
        <SeeAlso cref="Shift"/>*)
      function Reverse(const Vec: TOpenCLMtxVec): TOpenCLVector; overload;

      (*<summary>A cyclic shift on vector elements.</summary>
        
<remarks>Performs cyclic shift on vector elements. The number of elements to shift is specified in the Offset parameter. Offset can be
        any integer number, positive or negative.
</remarks>


        <Example>
        <code>
        var a: clVector;
        begin
            a.CopyFromArray(TSingleArray.Create(1,2,3,4));
            b.Rotate(a, 2);   // b = [3,4,1,2]
        end;
        </code>
        </Example>

        <SeeAlso cref="Reverse"/>
        <SeeAlso cref="Shift"/>*)
      function Rotate(const Src: TOpenCLMtxVec; Offset: integer): TOpenCLVector; overload;


      (*<summary>Mean value.</summary>
        
<remarks>Calculate the mean value of all calling object elements. The result is a real value.
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
            TOpenCLVector a;
            clMtxVec.CreateIt(out a);
            try
            {
              a.CopyFromArray(new double[] {1,2,3,4});
              double m = a.Mean(); // 2.5
            }
            finally
            {
              clMtxVec.FreeIt(ref a);
            }
          }
        }
        </code></example>

        <SeeAlso cref="Sum"/>
        <SeeAlso cref="Meanc"/>*)
      function Mean: double; overload;

      (*<summary>Returns real mean value from calling object elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
      function Mean(Index, Len: integer): double; overload;
      (*<summary>Calculate the mean value from calling object elements [Index]..[Index+Len-1].</summary>
        
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun/underrun.
</remarks>
*)
      procedure Mean(out AMean: double; Index, Len: integer); overload;
      (*<summary>Same as <see cref="Meanc"/>.</summary>*)
      procedure Mean(out AMean: TCplx); overload;
      (*<summary>Same as <see cref="Meanc"/>.</summary>*)
      procedure Mean(out AMean: TCplx; Index, Len: integer); overload;

      (*<summary>Mean value.</summary>
        <returns>the mean value of all calling object complex elements. The result is a complex value.</returns>
        
<remarks>An exception is raised is calling object <see cref="Complex"/> is .
</remarks>


        <SeeAlso cref="Mean"/>*)
      function Meanc: TCplx; overload;
      (*<summary>Returns complex mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
</remarks>
*)
      function Meanc(Index, Len: integer): TCplx; overload;
      (*<summary>Calculate the mean value from all calling object complex elements.</summary>
        
<remarks>The result AMean is a complex value. An exception is raised if calling object <see cref="Complex"/> property is .
</remarks>
*)
      procedure Meanc(out AMean: TCplx); overload;
      (*<summary>Calculate the mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
        
<remarks>The result AMean is a complex value.
        An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
</remarks>
*)
      procedure Meanc(out AMean: TCplx; Index, Len: integer); overload;

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
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {1,3,-2});
            doouble s = a.Sum();  // s = 2
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
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

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object elements. The result is a real value.</returns>
      
<remarks>An exception is raised is calling object <see cref="Complex"/> is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            double b = a.Max(); // 4.0
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Min"/>
      <SeeAlso cref="Maxc"/>
      <SeeAlso cref="MaxMin"/>*)
    function Max: double; overload;
    (*<summary>Returns the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Max(Index,Len: integer): double; overload;
    (*<summary>Calculate the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Max(out AMax: double; Index,Len: integer); overload;
    (*<summary>Calculate the maximum value of all calling object elements.</summary>
      
<remarks>The AMax parameter returns the maximum value. The AIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure Max(out AMax: double; out AIndex: integer); overload;
    (*<summary>Calculate the maximum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the maximum value. The AIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Max(out AMax: double; out AIndex: integer; Index, Len: integer); overload;
    (*<summary>Same as <see cref="Maxc"/> method.</summary>*)
    function Max(out AMax: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object complex elements. Complex elements are first compared by the amplitude
        and then by the argument.</returns>

      
<remarks>An exception is raised if calling object <see cref="Complex"/> is .
</remarks>


      <SeeAlso cref="Max"/>*)
    function Maxc: TCplx; overload;
    (*<summary>Returns the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised if calling object
      <see cref="Complex"/> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(Index,Len: integer): TCplx; overload;
    (*<summary>Calculate the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns complex maximum value. Returns the index of maximum value. Complex elements are first compared by the amplitude and then by the argument.
      The AIndex parameter returns the index of maximum value. An exception is raised if calling object <see cref="Complex"/>
      property is  or array borders are overrud/underrun.
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
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure MaxMin(out AMax,AMin: double; Index, Len: integer); overload;
    (*<summary>Calculates the maximum and minimum value of all calling object elements.</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer); overload;
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer; Index, Len: integer); overload;

    (*<summary>Minimum value.</summary>
      <returns>The minimum value of all calling object elements. The result is a real value.</returns>

      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            double b = a.Min();  // 1.0
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>
      <SeeAlso cref="Minc"/>*)
    function Min: double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Min(Index,Len: integer): double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMin is a real value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Min(out AMin: double; Index,Len: integer); overload;
    (*<summary>Calculate the minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the
      minimum value. The AIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Min(out AMin: double; out AIndex: Integer; Index, Len: integer); overload;
    (*<summary>Calculate the minimum value of all calling object elements.</summary>
      
<remarks>The AMin parameter returns the
      minimum value. The AIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure Min(out AMin: double; out AIndex: integer); overload;
    (*<summary>Same as the <see cref="Minc"/> method.</summary>*)
    function Min(out AMin: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Minimum value.</summary>
      
<remarks>Returns the minimum value of all calling object complex elements. Complex elements are first compared by the amplitude
      and then by the argument. An exception is raised if calling object <see cref="Complex"/> is .
</remarks>


      <SeeAlso cref="Min"/>*)
    function Minc: TCplx; overload;
    (*<summary>Returns the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised
      if calling object. <see cref="Complex"/> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Minc(Index,Len: integer): TCplx; overload;
    (*<summary>Calculate the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMin parameter returns complex minimum value. Returns the index of minimum value. Complex elements are first compared by the amplitude and then by the argument.
      An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
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
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            a.CopyFromArray(new double[] {4,3,2,1});
            double c = a.NormC(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormL1"/>
      <SeeAlso cref="NormL2"/>*)
    function NormC(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload;
    (*<summary>Calculates the C norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload;


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
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            a.CopyFromArray( new double[] {4,3,2,1});
            double c = a.NormL1(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL2"/>*)
    function NormL1(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload;
    (*<summary>Calculates the L1 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload;

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
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            a.CopyFromArray(new double[] {4,3,2,1});
            double c = a.NormL2(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL1"/>*)
    function NormL2(const Vec: TOpenCLMtxVec; RelativeError: boolean = False): double; overload;
    (*<summary>Calculates the L2 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload;

    (*<summary>Calculate the L2-norm of the calling vector.</summary>
     <returns>L2 norm, defined by: <c>NormaL2 = ( Sum(|a[i]|^2) )^0.5  , 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormL2: double; overload;
    (*<summary>Calculates the L2 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(Index,Len: integer): double;  overload;

    (*<summary>Computes the sum and the sum of squared elements from the elements in the calling object.</summary>*)
    procedure SumAndSqrSum(out Sum, SqrSum: double); overload;

    (*<summary>Returns the sum and the sum of squared elements.</summary>
               
<remarks>Returns the sum and the sum of squared items from calling vector elements [Index]..[Index+Len-1].
</remarks>
*)

    procedure SumAndSqrSum(out Sum, SqrSum: double; Index, Len: integer); overload;

    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="Complex"/> property is true.
</remarks>


      <Example>
      <code>
      var a: clVector;
          c: double;
          aMean: double;
      begin
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          Caption := FloatToSTr(a.StdDev);
      end;
      </code>
      </Example>


      <SeeAlso cref="Mean"/>*)
    function StdDev: double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true or if array
      borders are overrun/underrun.
</remarks>
*)
     function StdDev(Index,Len: integer): double; overload;
    (*<summary>Returns the standard deviation of all calling object elements.</summary>
       
<remarks>The sum and the sum of squares of all calling object elements must be passed as parameters.
       An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double): double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The sum and the sum of squares of the coresponding elements must be passed as parameters.
      An exception is raised if calling object <see cref="Complex"/> property is true
      or if array borders are overrun/underrun.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double; Index, Len: integer): double; overload;

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
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            double c = a.Product(); // c= 24
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
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
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            b.CopyFromArray(new double[] {5,6,7,8});
            double prod = a.DotProd(b); // = 1*5 + 2*6 + * 3*7 + 4*8
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)
    function DotProd(const Vec: TOpenCLMtxVec): double; overload;

    (*<summary>Returns the scalar product between Vec elements [VecIndex]..[VecIndex+Len-1] and calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property is True.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProd(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): double; overload;

    procedure DotProd(DstIndex: integer; const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Len: integer; const Buffer: TOpenCLMtxVec); overload;

    (*<summary>Scalar product of two real or complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the Vec1 and Vec2 stores the result in calling vector at position DstIndex.
      ConjVec parameter is ignored if data is real. If ConjVec is true, the function computes: result = Vec1*conj(Vec2)

      The dot product is defined by the equation:<para/>

      <IMG name="TVec13"/><para/>

      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>
*)

    procedure DotProd(DstIndex: integer; const Vec1, Vec2: TOpenCLMtxVec; ConjVec: boolean; const Buffer: TOpenCLMtxVec); overload;

    (*<summary>Scalar product of two real arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a real scalar value.
      The result is stored at specified index in the calling vector in the GPU memory. This variant of the function
      is non-blocking (faster), because the result does not have to be copied to the CPU memory to be used.

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
          TOpenCLVector a,b,c;
          double[] ac;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            c.DotProd(0, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.DotProd(1, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.DotProd(2, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.CopyToArray(ac); //ac = [70, 70, 70]
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)


    procedure DotProd(DstIndex: integer; const Vec1, Vec2, Buffer: TOpenCLMtxVec); overload;

    (*<summary>Scalar product of two complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a complex scalar value.
      An exception is raised if calling or Vec object <see cref="Complex"/> property is false.
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
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            b.CopyFromArray(new double[] {5,6,7,8});
            double prod = a.DotProdc(b); //= (1+2i)*(5+6i)+(3+4i)*(7+8i)
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProd"/>*)
    function DotProdc(const Vec: TOpenCLMtxVec): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TCplx; overload;
    (*<summary>Returns the scalar product between Vec and calling object complex elements.</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean; VecIndex, Index, Len: integer): TCplx; overload;


    property Tag: PointerInteger read GetTag write SetTag;
    property Caption: string read GetCaption write SetCaption;
    property Length: integer read GetLength write SetLength;
    property Complex: boolean read GetComplex write SetComplex;
    property FloatPrecision: TclFloatPrecision read GetClFloatPrecision write SetClFloatPrecision;
    (*<summary>Returns the device on which we allocated memory for this object. </summary>*)
    property Device: TOpenCLDevice read GetDevice;

    constructor Create(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean; IsCached: boolean = true); overload;
    constructor Create(TargetDevice: TOpenCLDevice; IsCached: boolean = true); overload;
    constructor Create(DeviceSource: TOpenCLMtxVec; IsCached: boolean = true); overload;
    constructor Create(IsCached: boolean); overload;
  end;

(*<summary>Open CL Matrix class with overloaded operators.</summary>
    
<remarks>Declare clMatrix to run computation on Open CL
    Be carefull to declare clMatrix only for local variables with short lifetime.
    Call the Create method for clMatrix, if the variable is a global variable or a
    variable with a longer life. If the Create method (constuctor) is called, the clMatrix
    creates its own memory object in GPU memory, otherwise object cache is used.
</remarks>
*)

  clMatrix = record
  strict private
    
    FData: TOpenCLMatrix;
    
    function get_Data: TOpenCLMatrix;
    function GetCaption: string;
    function GetComplex: boolean;
    function GetLength: integer;
    function GetTag: PointerInteger;
    procedure SetCaption(const Value: string);
    procedure SetComplex(const Value: boolean);
    procedure SetLength(const Value: integer);
    procedure SetTag(const Value: PointerInteger);
    function GetFloatPrecision: TCLFloatPrecision;
    procedure SetFloatPrecision(const Value: TCLFloatPrecision);
    function GetIsSubRange: boolean;
    procedure CreateFromCache;
    function GetCols: integer;
    function GetRows: integer;
    procedure SetRows(const Value: integer);
    procedure SetCols(const Value: integer);
  private
    property Data: TOpenCLMatrix read get_Data;
  public
    (*<summary>Add left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: TCplx; const Right: clMatrix): clMatrix;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: clMatrix; const Right: TCplx): clMatrix;
    (*<summary>Add Left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: double;const Right: clMatrix): clMatrix;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: clMatrix; const Right: double): clMatrix;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: TOpenCLMtxVec;const Right: clMatrix): clMatrix;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: clMatrix; const Right: TOpenCLMtxVec): clMatrix;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: clMatrix;const Right: clMatrix): clMatrix;

    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TCplx;   const Right: clMatrix): clMatrix;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: clMatrix; const Right: TCplx): clMatrix;
    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: double; const Right: clMatrix): clMatrix;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: clMatrix; const Right: double): clMatrix;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: clMatrix; const Right: TOpenCLMtxVec): clMatrix;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TOpenCLMtxVec; const Right: clMatrix): clMatrix;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: clMatrix;const Right: clMatrix): clMatrix;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: clMatrix; const Right: TCplx): clMatrix;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(const Left: TCplx; const Right: clMatrix): clMatrix;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: clMatrix; Right: double): clMatrix;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(Left: double; const Right: clMatrix): clMatrix;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: clMatrix; Right: TOpenCLMtxVec): clMatrix;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(Left: TOpenCLMtxVec; const Right: clMatrix): clMatrix;
    (*<summary>Multiply all elements in Left with corresponding elements in Right</summary>*)
    class operator Multiply(const Left: clMatrix; const Right: clMatrix): clMatrix;

    (*<summary>Multiplies Left with Right and returns result. </summary>*)
    class operator Multiply(const Left: clMatrix; const Right: clVector): clMatrix;
    (*<summary>Multiplies Left with Right and returns result. </summary>*)
    class operator Multiply(const Left: clVector; const Right: clMatrix): clMatrix;

    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: clMatrix; const Right: TCplx): clMatrix;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: TCplx; const Right: clMatrix): clMatrix;
    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: clMatrix; const Right: double): clMatrix;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: double; const Right: clMatrix): clMatrix;

    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: clMatrix; const Right: TOpenCLMtxVec): clMatrix;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: TOpenCLMtxVec; const Right: clMatrix): clMatrix;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: clMatrix; const  Right: clMatrix): clMatrix;

    (*<summary>Negates all values inside AValue.</summary>*)
    class operator Negative(const AValue: clMatrix): clMatrix;

    class operator Implicit(const AValue: clMatrix): TOpenCLMtxVec;
    class operator Implicit(const AValue: clMatrix): TOpenCLMatrix;





    class operator Explicit(const AValue: clMatrix): TDoubleArray;
    class operator Explicit(const AValue: clMatrix): TCplxArray;
    class operator Explicit(const AValue: clMatrix): TSingleArray;
  public
    
    procedure CopyTo(Dst: TMtx); overload;
    procedure CopyTo(Dst: TVec); overload;
    procedure CopyTo(Dst: TMtxInt); overload;
    procedure CopyTo(Dst: TVecInt); overload;
    procedure Copy(const Src: TMtxInt); overload;
    procedure Copy(const Src: TVecInt); overload;
    procedure Copy(const Src: TMtx); overload;
    procedure Copy(const Src: TVec); overload;
    
    procedure SizeToArray(var Dst: TSingleArray); overload;
    procedure SizeToArray(var Dst: TDoubleArray); overload;
    procedure SizeToArray(var Dst: TCplxArray); overload;
    function CopyFromArray(aRows, aCols: integer; const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(aRows, aCols: integer; const Src: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(aRows, aCols: integer; const Src: TCplxArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(aRows, aCols: integer; const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(aRows, aCols: integer; const Src: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(const Src: TCplxArray): TOpenCLMtxVec; overload;
    function CopyFromArray(const Src: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyToArray(var Dst: TSingleArray): TOpenCLMtxVec; overload;
    function CopyToArray(var Dst: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyToArray(var Dst: TCplxArray): TOpenCLMtxVec; overload;

    
    procedure AssignWithoutCopy(const Src: clMatrix); overload;
    procedure Assign(const Src: TOpenCLMatrix); overload;
    


   (*<summary>Automatically set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TOpenCLMtxVec method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read GetIsSubRange;

    (*<summary>Resets any defined subrange.</summary>
      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange;

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted or indices adjusted.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised if an attempt is made to change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on the
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(Index: integer; Len: integer); overload;


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
        TOpenCLVector a,b;

        clMtxVec.CreateIt(out a, out b);
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
        TOpenCLVector a,b;

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
      var a,b: TOpenCLVector;
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
    procedure SetSubIndex(BeginIndex, EndIndex: integer); overload;


    (*<summary>Matrix array multiplication.</summary>
      
<remarks>Multiplies elements in Mtx matrix with the elements in the calling matrix (array multiplication) and stores the results in calling matrix.
      The <see cref="TOpenCLMatrix.Rows"/>, <see cref="Cols"/> and <see cref="TOpenCLBase.Complex">Complex</see> properties of both matrices must match, otherwise an exception is
      raised.
</remarks>


      <Example>
      <code>
      var  A,B,C: clMatrix;
      begin
          A.CopyFromArray(2,2,TSingleArray.Create(1,2,
                                                  2,4));
          B.CopyFromArray(2,2,TSingleArray.Create(1,2,
                                                  2,4));
          C.MulElem(A,B);
          // C becomes:
          // [1, 4,
          //  4,16]
      end;
      </code>
      </Example>

      <SeeAlso cref="InvElem"/>*)
    function MulElem(Mtx: TOpenCLMatrix): TOpenCLMatrix; overload;
    (*<summary>Multiplies elements in Mtx1 matrix with the elements in Mtx2 matrix (array multiplication) and stores the results in calling matrix.</summary>
      
<remarks>The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling matrix are set implicitly to match those
      of Mtx1 and Mtx2 matrices. Mtx1 and Mtx2 Rows, Cols, and Complex properties must be the same, otherwise an excetion is raised.
      raised.
</remarks>
*)
    function MulElem(Mtx1, Mtx2: TOpenCLMatrix): TOpenCLMatrix; overload;


    (*<summary>The inverse of clMatrix elements.</summary>
      
<remarks>Calculates the inverse of all clMatrix elements in place. The computation occurs after first limiting the
      magnitude of each elements by the lower bound of Treshhold. The limiting operation is performed to avoid
      division by zero. Since Treshold represents a magnitude, it is always real and must always be positive.
      For complex versions, the magnitude of the input is limited, but the phase remains unchanged. Zero-valued
      input is assumed to have zero phase. To bypass the limiting operation set the Threshold to zero.
</remarks>


      <Example>
      <code>
      var  A: clMatrix;
      begin
          A.CopyFromArray(2,2,TSingleArray.Create(1,2,
                                                  2,4));  // 2x2, not complex clMatrix
          A.InvElem(1.0e-7);
      end;
      </code>
      </Example>

      <SeeAlso cref="MulElem"/>*)
    function InvElem(): TOpenCLMatrix; overload;
    function InvElem(const X: TOpenCLMtxVec): TOpenCLMatrix; overload;

    procedure Adopt(const Src: TOpenCLMatrix); overload;

    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec object.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // a = [1,2,3,4]
          b.Copy(a);                // b = [1,2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Assign"/>*)

    function Copy(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] in the calling object
       elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size and <see cref="Complex"/> properties must be set explicitly.
       An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Copy(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec2 elements to corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="Complex"/> properties
      of the calling object are set implicitly to match Vec1 and Vec2 vectors.
</remarks>
*)
    function Add(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec1 elements [Vec1Index]..[Vec1Index+Len-1] to Vec2 elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is
      raised if array borders are overrun.
</remarks>
*)
    function Add(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to object elements.</summary>

      <SeeAlso cref="Sub"/>*)
    function Add(Value: double): TOpenCLMtxVec; overload;

    (*<summary>Adds complex Value to all calling object complex elements.</summary>*)
    function Add(Value: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to the each element of the Vec object.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set automatically.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each element of the Vec object.</summary>
      
<remarks>Store the result to the calling object. Size property of the calling object is set
      automatically. <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Value to each element of Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each elements of the Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array addition.</summary>
      
<remarks>Add each of Vec elements to corresponding elements in the calling object.
</remarks>

      <SeeAlso cref="Sub"/>*)
    function Add(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Split complex calling object in real and imaginary part.</summary>
      
<remarks>Split calling object into real and imaginary components. Store all real components in ReVec and
      all imaginary components in ImVec. Size and <see cref="Complex"/> properties of ReVec and ImVec
      are set implicitly to match with the calling vector. An execption is raised if calling object is not complex.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.CplxToReal(b,c);
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLMtxVec); overload;
    (*<summary>Split calling object elements [Index]..[Index+Len-1] into real and imaginary components.</summary>
      
<remarks>Store real components in ReVec elements [ReIndex]..[ReIndex+Len-1] and imaginary components in ImVec elements
      [ImIndex]..[ImIndex+Len-1]. Size and <see cref="Complex"/> properties must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer); overload;

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="Complex"/> property is True.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          b.CopyFromArray(2,2,TSingleArray.Create(2,2,3,4));
          c.RealToCplx(a,b);
      end;
      </code>
      </Example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(ReVec, ImVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Construct a complex object from the ReVec elements [ReIndex]..[ReIndex+Len-1] (real part) and the ImVec
      elements [ImIndex]..[ImIndex+Len-1] (imaginary part).</summary>
      
<remarks>The results are stored to calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if array borders
      are overrun. An exception is also raised if ReVec or ImVec <see cref="Complex"/> property is True.
</remarks>
*)
    function RealToCplx(ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer): TOpenCLMtxVec; overload;

        (*<summary>Conjugate.</summary>
      
<remarks>Conjugate all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var c: clMatrix;
      begin
          c.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // c = [1+2i, 3+4i]
          c.Conj;                   // c = [1-2i, 3-4i]
      end;
      </code>
      </Example>*)
    function Conj: TOpenCLMtxVec; overload;
    (*<summary>Conjugate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Conj(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec elements.</summary>
      
<remarks>Store the results in the calling object. The
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match Vec vector.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1].</summary>
      
<remarks>Store them in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Extend Vec object to complex calling object.</summary>
      
<remarks>If Zeros is true then the calling vector imaginary part is set to zero, otherwise
      the calling object imaginary part is the same as calling object real part.
</remarks>


      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyFromArray(2,2, TSingleArray.Create(1,2,3,4));
          b.ExtendToComplex(a,True);
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>
      <SeeAlso cref="ImagPart"/>
      <SeeAlso cref="RealPart"/>*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean): TOpenCLMtxVec; overload;

    (*<summary>Converts the source to complex.</summary>
      
<remarks>Converts the source to complex by setting the imaginary part to either zero (zeros = True)
      or same as real (zeros = false). Does not set size of the calling vector.
      If there is not sufficient space available to store the result an exception
      will be raised.
</remarks>
*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean; SrcIndex,DstIndex, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.

     Sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is not used
     implicitely within TOpenCLMtxVec methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle: TOpenCLMtxVec; overload;

    (*<summary>FixAngle for calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is True or
      if array borders are overrun/underrun.
</remarks>
*)
    function FixAngle(Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in <c>[-2PI,2PI]</c> for all Src elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the Src object.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in [-2PI,2PI] for Src elements [SrcIndex]..[SrcIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly. An exception is raised
      if array borders are overrun.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Rounds towards negative infinity.</summary>
      
<remarks>Rounds all calling object elements towards negative infinity in-place.

      <c>Floor(-2.8) = -3</c><para/>
      <c>Floor(2.8) = 2</c><para/>
      <c>Floor(-1.0) = -1</c><para/>
</remarks>
*)
    function Floor: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards negative infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards negative infinity and stores the result
      in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards negative infinity
      and stores the result in the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate the calling object complex exponential in-place. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          a.Expj;   // a = [e^i, e^2i, e^3i, e^4i]
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)
    function Expj: TOpenCLMtxVec; overload;
    (*<summary>Calculate the e^(j*Omega), a complex exponential.</summary>
      
<remarks>Omega must be a real object. If omega is complex, then use the <see cref="Exp"/> method.
</remarks>
*)
    function Expj(Omega: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the complex exponential for Omega elements [OmegaIndex]..[OmegaIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elemets [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Expj(Omega: TOpenCLMtxVec; OmegaIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
        ind: Integer;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,5.5,-1.6,6)); // a = [1, 5.5, -1.6, 6]
          a.Frac;  // a = [0, 0.5, -0.6, 0]
        finally
          a.Free;
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac: TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for all X object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Complementary error functon of values.</summary>
      
<remarks>Calculates the complementary error function value for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,5.5,-1.6,6)); // a = [1, 5.5, -1.6, 6]
          a.Erfc;
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc: TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Error functon of values.</summary>
      
<remarks>Calculates the error function for all object values in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
        ind: Integer;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1, 5.5, -1.6, 6)); // a = [1, 5.5, -1.6, 6]
          a.Erf;
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf: TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Flips the real and imaginary part of complex numbers.</summary>
      <returns>Flipped real and imaginary part of complex numbers for all calling object elements in-place.<para/>
        The following transformation is used: <c>a + i*b -> b + i*a</c></returns>

      <SeeAlso cref="FlipConj"/>*)
    function Flip: TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for all X object elements.</summary>
      
<remarks>Xtore the results in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Flips the real and imaginary part of complex numbers and conjugates the complex numbers.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling object complex elements in-place.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj: TOpenCLMtxVec; overload;
    (*<summary>Flip calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is false or if array borders
      are overrun/underrun.
</remarks>
*)
    function FlipConj(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flip all X object complex elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object
      are adjusted automatically.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flip X object complex elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if calling
      object <see cref="Complex"/> property is false or if array borders are overrun/underrun.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(x[i]/y[i]).</c><para/>
      The results will be saved to the calling vector.
      X and Y must be a real and have the same length. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(0,1,10,-1,-10)); // a = [0, 1, 10,    -1];
          b.CopyFromArray(2,2,TSingleArray.Create(0,1,PI,-1,-PI)); // b = [0, 1, PI,    -1];
          c.Rem(a,b);                     // c = [0, 0, 0.5752, 0]
      end;
      </code>
      </Example>*)
    function Rem(const X, Y: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Calculation uses the following formula:

      <c>z[i] = x[xi]-y[yi]*Trunc(x[xi]/y[yi]),</c><para/>
      where i in [Index..Index+Len], xi in [XIndex..XIndex+Len], yi in [YIndex..YIndex+Len].
      The results will be saved to the calling vector. X must be a real. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X, Y: TOpenCLMtxVec; XIndex, YIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector.
      Size and <see cref="Complex"/> properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; Y: double): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; Y: double; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiply all calling object elements with Value in-place.
      This method is the same as the <see cref="Mul"/> method overloads
      multiplying with vector elements with a scalar.
</remarks>


      <Example>
      <code>
      var v: clMatrix;
      begin
          v.CopyFromArray(2,2,TSingleArray.Create(2,3,5));  // v = [2,3,5]
          v.Scale(3); // v = [6,9,15]
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>*)
    function Scale(Factor: double): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(Factor: double; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all calling object elements with a complex Value in-place.</summary>*)
    function Scale(Factor: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(Factor: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    









































    
    (*<summary>Log base N.</summary>
      <returns>Log base N for all calling object elements in-place.</returns>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4)); // a = [1,2,3,4]
          a.LogN(10.0);  // log base 10, the slow way a = [Log10(1), Log10(2),...]
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function LogN(N: double): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(N: double; Index, Len: integer):TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function LogN(N: double; X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(N: double; X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Normalize object.</summary>
      
<remarks>Normalizes Vec object by subtracting a constant Offset from Vec elements and dividing the result by constant Factor:

      <IMG name="TVec21"/>

      The results are stored in calling object. Use this method if you want to do a multiply and add (scale and offset) operations in a single method call.
</remarks>

      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          b.Normalize(a,2,3);
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>
      <SeeAlso cref="Scale"/>*)
    function Normalize(const Vec: TOpenCLMtxVec; SubOffset, DivFactor: double): TOpenCLMtxVec; overload;
    (*<summary>Normalize Vec object values [VecIndex]..[VecIndex+Len-1] by subtracting a real constant SubOffset from Vec elements and dividing the result by
      complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; SubOffset, DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Normalize Vec object by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; SubOffset: TCplx; DivFactor: double): TOpenCLMtxVec; overload;
     (*<summary>Normalize Vec object complex values [VecIndex]..[VecIndex+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; SubOffset: TCplx; DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

      (*<summary>Raises base object elements to any power.</summary>
      
<remarks>Raises Base calling object elements to any power. The <see cref="IntPower"/> is faster, if Exponent is an integer.
      Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
      To compute a power to the negative exponent in general case or when the base is negative,
      use the complex version of the function.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          a.Power(1.2);
      end;
      </code>
      </Example>

      <SeeAlso cref="IntPower"/>
      <SeeAlso cref="PowerVec"/>*)
    function Power(Exponent: double): TOpenCLMtxVec; overload;

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
    function Power(Exponent: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Raises base elements to exponent power.</summary>
      
<remarks>Raises Base value to Exponent object values powers and store the results to calling object values.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>


      <SeeAlso cref="PowerVec"/>*)
    function Power(const aBase: double; Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Raises Base complex value to Exponent object values powers.</summary>
      
<remarks>Store the results to calling object values. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TCplx; Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to complex Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TOpenCLMtxVec; Exponent: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to real Exponent power.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const aBase: TOpenCLMtxVec; Exponent: double): TOpenCLMtxVec; overload;
    (*<summary>Raises each of Base object elements to corresponding power, stored in Exponenet elements.</summary>

      
<remarks><c>Power[i] = Base[i]^Exponent[i]</c><para/>

      Size and <see cref="Complex"/> property of calling object are adjusted automatically.
      An exception is raised if Base and Exponent sizes do not match.
</remarks>
*)
    function Power(const aBase, Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises Base object elements to Exponent object elements power.</summary>
      
<remarks>Raises Base elements to Exponent elements power. Only positive exponents can be handled
      if exponent object <see cref="Complex"/> property is True.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          b.CopyFromArray(2,2,TSingleArray.Create(3,3,2,2));
          c.PowerVec(a,b); // c = [1,8,9,16]
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function PowerVec(const Base, Exponent: TOpenCLMtxVec): TOpenCLMtxVec;

    


















    


    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      <Example>
      <code>
      var a: clVector;
          c: double;
          aMean: double;
      begin
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          Caption := FloatToSTr(a.RMS);
      end;
      </code>
      </Example>


      <SeeAlso cref="Mean"/>*)
    function RMS: double; overload;

    (*<summary>Elements rounded to the nearest whole number.</summary>
      
<remarks>Rounds all calling object elements to the nearest whole number.
      The result can be stored to an array of integers or as floating
      point number.
</remarks>


      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Frac"/>*)
    function Round: TOpenCLMtxVec; overload;

    (*<summary>Round calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Round all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/> property of
      calling object are adjusted automatically.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Round Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> property of the calling object must be set explicitly.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Initialize elements to Value.</summary>
      
<remarks>Set all calling object elements to Value. If the calling object is complex
      then the real part is set to Value and the imaginary is set to zero.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
        a.Size(4,False);
        a.SetVal(1); // a = [1,1,1,1]
      end;
      </code>
      </Example>

      <SeeAlso cref="SetZero"/>*)
    function SetVal(Value: double): TOpenCLMtxVec; overload;

    (*<summary>Set all calling object elements [Index]..[Index+Len-1] to real Value.</summary>
      
<remarks>If the calling object is complex then the real part is set to Value and the imaginary is set to zero.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function SetVal(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Set all calling object complex elements to complex Value.</summary>*)
    function SetVal(Value: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Set calling object complex elements [Index]..[Index+Len-1] to complex Value.</summary>
       
<remarks><see cref="Complex"/> property of the calling object are set to true even before the call it was false.
       An exception is raised if calling object array borders are overrun/underrun.
</remarks>
*)
    function SetVal(Value: TCplx; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Initializes object elements to zero.</summary>
      <SeeAlso cref="SetVal"/>*)
    function SetZero: TOpenCLMtxVec; overload;
    (*<summary>Initializes calling object elements [Index]..[Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function SetZero(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Changes elements sign.</summary>
      
<remarks>Changes all calling object elements sign <c>(v -> -v)</c> in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(1,2,-3,4));
        a.Sign; // a = [-1,-2,3,-4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Mul"/>*)
    function Sign: TOpenCLMtxVec; overload;
    (*<summary>Change calling object elements [Index]..[Index+Len-1] sign in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sign(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Change all X object elements sign.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Change X object elements [XIndex]..[XIndex+Len-1] sign.</summary>
      
<remarks>Store the results in callingobject elements [Index]..[Index+Len-1]. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec; XIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)

    function Sgn: TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements [Index..Index+Len-1]. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements and stores the result in the calling object.  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements [SrcIndex..SrcIndex+Len-1] and stores the result in
                 the calling object [Index..Index+Len-1].  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of all Src object elements and multiplies
      it with the calling object elements accordingly.
      Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      The length of Src and of the calling object must match
      or an exception will be raised. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine for all calling object elements and stores the sines
      to SinX and cosines to CosX. Size and <see cref="Complex"/> property of SinX and CosX are
      adjusted automatically.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
          s,c: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(0,PiDiv2,PI,0));
         a.SinCos(s,c); // s=[0,1,0,0], c =[1,0,-1,1]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(SinX, CosX: TOpenCLMtxVec); overload;
    (*<summary>Calculates the sine and cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>stores the sines to SinX elemets [SinXIndex]..[SinXIndex+Len-1] and cosines to CosX elements [CosXIndex]..[CosXIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinX and CosX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinCos(SinX, CosX: TOpenCLMtxVec; SinXIndex, CosXIndex, Index, Len: integer); overload;

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for all calling object elements and stores
      the sines to SinhX and cosines to CoshX. Size and <see cref="Complex"/> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
          s,c: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(0,PiDiv2,PI,0));
         a.SinhCosh(s,c);
      end;
      </code>
      </Example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLMtxVec); overload;
    (*<summary>Calculates the hyperbolic sine and hyperbolic cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the sines to SinhX elemets [SinhIndex]..[SinhIndex+Len-1] and cosines to CoshX elements [CoshIndex]..[CoshIndex+Len-1] elements.
      Size and <see cref="Complex"/> property of SinhX and CoshX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLMtxVec; SinhIndex, CoshIndex, Index, Len: integer); overload;


    (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is a lower bound for threshold operation.
      All values smaller than Value will be replaced with Value.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(2,0.1,3,4));
          a.ThreshBottom(0.2); // a = [2,0.2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshBottom(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(2,0.1,3,4));
          a.Threshtop(0.2); // a = [0.2,0.1,0.2,0.2]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshTop(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for
      threshold operation.
      All values less than LTLevel will be replaced with LTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      <example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(2,0.1,3,4));
          a.ThresholdLT(2.3,1.5); // a = [1.5,1.5,3,4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdLT(LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(LTLevel: double; LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(LTLevel: double; LTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel: double; LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; LTLevel: double; LTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on all calling object values. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than LTLevel will be replaced with GTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      <example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(2,0.1,3,4));
          a.ThresholdGT(2.3,3.4); // a = [2,0.1,3.4,3.4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT(GTLevel, GTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel, GTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(GTLevel: double; GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(GTLevel: double; GTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel: double; GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; GTLevel: double; GTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values less than LTLevel will be replaced with LTValue. All values bigger than GTLevel will be replaced with GTValue.
      Operation is available only for none Complex values.
</remarks>


      <example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(2,0.1,3,4));
          a.ThresholdGT_LT(2.3,3.4,1,0.5); // a = [2,0.5,3.4,3.4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than and less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      An exception will be raised if the calling object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (GTLevel, GTValue, LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of the calling object are adjusted automatically.
      An exception will be raised if Vec object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const Vec: TOpenCLMtxVec; GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="Complex"/> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec object or the calling object contain complex numbers.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TOpenCLMtxVec; GTLevel, GTValue, LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the TruncDst object as floating point numbers. The fractional
      part is stored in the FracDst.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)

    procedure TruncAndFrac(TruncDst: TOpenCLMtxVec; FracDst: TOpenCLMtxVec); overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] and store the results to TruncDst object elements
      [TruncIdx]..[TruncIdx+Len-1].</summary>
      
<remarks>The fractional parts are saved in FracDst elements [FracIdx]..[FracIdx+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src object. An exception is raised if
      array borders are overrun/underrun.
</remarks>
*)
    procedure TruncAndFrac(TruncDst: TOpenCLMtxVec; FracDst: TOpenCLMtxVec; TruncIdx, FracIdx, Index, Len: integer); overload;

    (*<summary>Rounds a real number towards zero.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the calling object again as floating point numbers.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)
    function Trunc: TOpenCLMtxVec; overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Truncate all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="Complex"/>
      property of calling object are adjusted automatically.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Truncate Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elemenents [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      property of calling object must be set explicitly to match those of Src
      object. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of all calling object elements in-place.
</remarks>


      <Example>
      <code>
        v.CopyFromArray(2,1,TSingleArray.Create(1,8));
        v.InvCbrt; // returns [1,0.5]
      </code>
      </Example>

      <SeeAlso cref="Cbrt"/>*)


    function InvCbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      the calling vector are set implicitly to match the X object.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2))</c> of all calling object elements in-place.
</remarks>


      <Example>
      <code>
        v.CopyFromArray(2,1,TSingleArray.Create(1,16));
        v.InvSqrt; // returns [1,0.25]
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvSqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the calling vector are set
       implicitly to match the X object.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude for all calling object elements in-place.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4)); // a = [1,-2, 3,4]
          a.Mag;      // a = [1, 2, 3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Abs"/>
      <SeeAlso cref="PhaseSpectrum"/>*)
    function Mag: TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for all X elements.</summary>
      
<remarks>Store the results in the calling object elements. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match Vec vector.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all Vec1 elements with corresponding Vec2 elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="Complex"/> property do not match.
</remarks>
*)
    function Mul(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if and array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Vector multiplication.</summary>
      
<remarks>Multiply each of Vec elements with corresponding elements in the calling object.
      Size and <see cref="Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.
</remarks>


      <SeeAlso cref="Divide"/>*)
    function Mul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiplies all calling object elements with Value in-place.
</remarks>


      <Example>
      <code>
      var v: clMatrix;
      begin
          v.CopyFromArray(3,1,TSingleArray.Create(2,3,5));  // v = [2,3,5]
          v.Mul(3); // v = [6,9,15]
      end;
      </code>
      </Example>

      <SeeAlso cref="Add"/>*)
    function Mul(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply all calling object elements with complex Value in-place.</summary>*)
    function Mul(Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with
      complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with Value.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with complex Value.</summary>
      
<remarks>Store the result in the calling object. Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set implicitly.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with complex Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="Complex"/> propertiy of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply elements by imaginary unit I.</summary>*)
    function MulI: TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with I in-place.</summary>*)
    function MulI(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply X elements with I and store the result in the calling object.</summary>*)
    function MulI(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Multipy X elements [XIndex]..[XIndex+Len-1] with I.</summary>
      
<remarks>Xtore the result in the calling object at locations [Index]..[Index+Len-1].
</remarks>
*)
    function MulI(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The phase angles (spectrum) of object elements.</summary>
      
<remarks>Calculates the phase angles (spectrum) of all Vec object elements. Phase values are returned in radians and are in the range
      -PI,PI. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match Vec object. The phase angles are calculated from the following equation:

      <IMG name="TVec23"/>
</remarks>


      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyCplxFromArray(1,2,TSingleArray.Create(1,2,3,-4));  // a = [1 + 2i, 3 - 4i]
          b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
      end;
      </code>
      </Example>

      <SeeAlso cref="PowerSpectrum"/>
      <SeeAlso cref="ArcTan2"/>*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised
      if array borders are overrun/underrun.
</remarks>
*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The power spectrum from object complex values.</summary>
      
<remarks>Calculates the power spectrum from the Vec object complex values and stores the results (power spectrum)
      in the real calling object. Size and <see cref="Complex"/> properties of the calling object are set
      implicitly to match Vec object. The spectrum elements are squares of the magnitudes of the complex input elements:

      <IMG name="Tvec22"/>
</remarks>


      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyCplxFromArray(1,2,TSingleArray.Create(1,2,3,-4)); // a = [1 + 2i, 3 - 4i]
          b.PowerSpectrum(a);   // b = [1*1 + 2*2, 3*3+(-4)*(-4)]
      end;
      </code>
      </Example>

      <SeeAlso cref="PhaseSpectrum"/>*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object real elements [Index]..[Index+Len-1]. An exception is raised
      if calling object <see cref="Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts the polar magnitude/phase pairs to cartesian pairs.</summary>
      
<remarks>Convert all AmpltVec and PhaseVec elements (combined) from polar to cartesian form. If AmpltVec and PhaseVec size is not the same
      , an exeption is raised. The results are stored as complex numbers (x=Re, y=Im) in the calling
      object.  Size and <see cref="Complex"/> properties of the calling object are set implicitly to
      match AmpltVec and PhaseVec objects.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // a = [1,2,3, 4] //magnitude
          b.CopyFromArray(2,2,TSingleArray.Create(1,0,1,-1)); // b = [1,0,1,-1] /phase
          c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
      end;
      </code>
      </Example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Convert  AmpltVec elements [AIndex]..[AIndex+Len-1] and PhaseVec elements [PIndex]..[PIndex+Len-1] from polar form
      (radius,angle) to cartesian form (x,y).</summary>
      
<remarks>The results are stored as complex numbers <c>(x=Re, y=Im)</c> in the calling
      object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties of the calling
      object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLMtxVec; AIndex, PIndex,Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets real part of complex object values.</summary>
      
<remarks>The method method gets the real part of a complex object Vec and stores the real results in the calling
      object. Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyCplxFromArray(1,2,TSingleArray.Create(1,2,3,4)); // = [1+2i, 3+4i]
          b.RealPart(a); // b = [1,3]
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Gets the real part of a Vec object complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the results in calling object real elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun or if Vec object <see cref="Complex"/>
      propety is false.
</remarks>
*)
    function RealPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Square.</summary>
      
<remarks>Calculate the square of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.Sqr; // a=[1,4,9,16]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="Complex"/> properties of the calling object are not changed.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,4,9,16));
          a.Sqrt; // a= [1, 2, 3, 4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling object are adjusted automatically.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.Sin; // Computes complex sine
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Sin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of all X object elements and store the results in calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosine.</summary>
      
<remarks>Calculate the cosine of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.Cos; // Computes complex sine
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCos"/>
      <SeeAlso cref="SinCos"/>*)
    function Cos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object.vSize and <see cref="Complex"/> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of calling object and the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.Tan; // Computes complex tangens
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)
    function Tan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan (Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan(const X : TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.Cot; // Computes complex cotangens
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)
    function Cot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
          a.Sec; // Computes complex secant
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)
    function Sec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of all caling object elements in-place.
</remarks>

      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));  // a = [1-2i, 3+4i]
          a.Csc; // Computes complex cosecant
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)
    function Csc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(1,-0.5,0.11,0.9));
        a.ArcSin;
      end;
      </code>
      </Example>

      <SeeAlso cref="Sin"/>*)
    function ArcSin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(1,-0.5,0.11,0.9));
        a.ArcCos;
      end;
      </code>
      </Example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks><para/>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object elements. Size and <see cref="Complex"/> properties of calling object are adjusted automatically
      to match those of X and Y objects. An exception is raised if X and Y size and <see cref="Complex"/> properties do not match.

      Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(Y, X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of Y/X.</summary>
      
<remarks>Calculation uses Y elements [YIndex]..[YIndex+Len-1], X elements [XIndex]..[XIndex+Len-1]
      and stores the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan2(Y, X: TOpenCLMtxVec; YIndex, XIndex, Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      <Example>
      <code>
      var A,B: clMatrix;
      begin
        A.CopyCplxFromArray(2,2,TSingleArray.Create(1,0, 2,0
                                                    2,0  4,1));  // 2x2, complex matrix
        B.ArcTan(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      <Example>
      <code>
      var A,B: clMatrix;
      begin
        A.CopyCplxFromArray(2,2,TSingleArray.Create(1,0, 2,0
                                                    2,0  4,1));  // 2x2, complex matrix
        B.ArcCot(A);
      end;
      </code>
      </Example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse secant.</summary>
      
<remarks>Calculate the inverse secant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Sec"/>
      <SeeAlso cref="ArcCsc"/>*)
    function ArcSec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse secant of all X object elements and store the results in the calling object. </summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cosecant.</summary>
      
<remarks>Calculate the inverse cosecant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Csc"/>
      <SeeAlso cref="ArcSec"/>*)
    function ArcCsc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1, 1.5, 2, 0.3));
          a.Sinh;
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,1.5,2,0.3));
          a.Cosh;
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,1.5,2,0.3));
          a.Tanh;
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Tanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,1.5,2,0.3));
          a.Coth;
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
   (*<summary>Calculate the hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic secant.</summary>
      
<remarks>Calculate the hyperbolic secant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcSech"/>
      <SeeAlso cref="Csch"/>*)
    function Sech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosecant.</summary>
      
<remarks>Calculate the hyperbolic cosecant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcCsch"/>
      <SeeAlso cref="Sech"/>*)
    function Csch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Absolute values.</summary>
      
<remarks>Calculate the absolute value of all calling object elemets in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
        a.CopyFromArray(2,2,TSingleArray.Create(1,-2,3,4));
        a.Abs;
      end;
      </code>
      </Example>

      <SeeAlso cref="Mag"/>*)
    function Abs: TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for all X object and store the results in the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic sine.</summary>
      
<remarks>Calculate the inverse hyperbolic sine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sinh"/>*)
    function ArcSinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosine.</summary>
      
<remarks>Calculate the inverse hyperbolic cosine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Cosh"/>*)
    function ArcCosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic tangens.</summary>
      
<remarks>Calculate the inverse hyperbolic tangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Tanh"/>*)
    function ArcTanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
        
<remarks>The results are storedi n the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
        properties of the calling object must be set explicitly.
        An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cotangens.</summary>
      
<remarks>Calculate the inverse hyperbolic cotangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Coth"/>*)
    function ArcCoth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverser hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic secant.</summary>
      
<remarks>Calculate the inverse hyperbolic secant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sech"/>*)
    function ArcSech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosecant.</summary>
      
<remarks>Calculate the inverse hyperbolic cosecant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Csch"/>*)
    function ArcCsch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of all calling object elements in-place.
</remarks>


      <Example>
      <code>
        v.CopyFromArray(1,2,TSingleArray.Create(1,8));
        v.Cbrt; // v = [1,2]
      </code>
      </Example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of all X object elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds all calling object elements towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards positive infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;
    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object elements [Index]..[Index+Len-1]
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          a.Ln;
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)
    function Ln: TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural algorithm for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties
      of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ln(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(10,100,1000,10000));  // a = [10,100,1000,10000]
          a.Log10;     // a = [1,2,3,4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp10"/>*)
    function Log10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,4,8));
          a.Log2;
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp2"/>*)
    function Log2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent (e^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          a.Exp;
      end;
      </code>
      </Example>

      <SeeAlso cref="Ln"/>*)
    function Exp: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 2 (2^).</summary>
      
<remarks>Calculate the exponent base 2 (2^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // a = [1,2,3,4]
          a.Exp2;   // a = [1,4,9,16]
      end;
      </code>
      </Example>
      <SeeAlso cref="Log2"/>*)
    function Exp2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 10 (10^).</summary>
      
<remarks>Calculate the exponent base 10 (10^) for all calling object elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // a = [1,2,3,4]
          a.Exp10;        // a = [10,100,1000,10000]
      end;
      </code>
      </Example>

      <SeeAlso cref="Log10"/>*)
    function Exp10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="Complex"/>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets the imaginary part of a complex object.</summary>
      
<remarks>Gets the imaginary part of a complex object Vec and stores the real results in the calling object.
      Size and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> must be true otherwise the exception will be raised.
</remarks>


      <Example>
      <code>
      var a,b: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));  // a= [1+2i, 3+4i]
          b.ImagPart(a);  // b = [2, 4]
      end;
      </code>
      </Example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Gets the imaginary part of complex object Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object. An exception is raised if the calling object is complex, if Vec is not complex or
      if array borders are overrun/underrun.
</remarks>
*)
    function ImagPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power ^(Exponent) for all caling object elements using the integer parameter Exponent.
      For non integer exponents, the <see cref="Power"/> and <see cref="PowerVec"/> methods can be used.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2,TSingleArray.Create(1,2,3,4));
          a.IntPower(3);
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>
      <SeeAlso cref="PowerVec"/>*)
    function IntPower(Exponent: Integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the power Base^(Exponent) for all Base object elements.</summary>
      
<remarks>Calclation uses the integer Exponent value and stores the results in calling object. Size and
      <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function IntPower(const Base: TOpenCLMtxVec; Exponent: Integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Inv(Index, Len: integer): TOpenCLMtxVec; overload;





    (*<summary>Calculate the inverse of X object elements [XIndex]..[XIndex+Len-1] without limiting  operating.</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if X and calling object <see cref="Complex"/> property
      does not match or array borders are overrun/underrun.
</remarks>
*)
    function Inv(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts elements from cartesian to polar coordinate form.</summary>
      
<remarks>Converts all calling object elements from cartesian to polar coordinate form, storing the magnitude (radius)
      component of corresponding elements in the AmpltVec and the phase (angle) component of corresponding elements in
      the PhaseVec. If you want to use this method then the calling matrix <see cref="Complex"/> property must be
      true. If this is not the case, an exception is raised. Size and <see cref="Complex"/> properties of AmpltVec and
      PhaseVec are set automatically.
</remarks>


      <Example>
      <code>
      var A,Amplt,Phase: clMatrix;
      begin
          A.CopyCplxFromArray(2,2,TSingleArray.Create(1,0, 2,0
                                                      2,0  4,1));  // 2x2, complex matrix
          A.CartToPolar(Amplt, Phase);
      end;
      </code>
      </Example>

      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLMtxVec); overload;
    (*<summary>Convert calling object elements [Index] to [Index+Len-1] from cartesian to polar form.</summary>
      
<remarks>Store the results in AmpltVec (radius values) and PhaseVec(phase values). Size and <see cref="Complex"/>
      properties of the calling vector must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLMtxVec; AmpltIndex, PhaseIndex, Index, Len: integer); overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: double): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition,  the following formula is being used:

      <c>result = result+ Cplx(aScale)*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; aScale: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Add a product of two vectors.</summary>
      
<remarks>Multiply Vec1 elements with coresponding Vec2 elements and add the result
      to the calling vector. The size of the calling vector is set implicitly.
</remarks>


      <SeeAlso cref="Mul"/>
      <SeeAlso cref="Add"/>*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and
      add the results to the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Conjugate and multiply.</summary>
      
<remarks>Conjugate each of Vec elements and multiply them with corresponding elements in the calling object.
      The results are stored in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Vec.
</remarks>


      <Example>
      <code>
      var a,b,c: clMatrix;
      begin
          a.CopyCplxFromArray(2,2, TSingleArray.Create(1,2,3,4));
          b.CopyCplxFromArray(2,2, TSingleArray.Create(4,3,2,1));
          c.ConjMul(a,b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Conj"/>
      <SeeAlso cref="Mul"/>*)
    function ConjMul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1] and multiply them with corresponding
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties are <b>not</b> set.

      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function ConjMul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec2 elements and multiply them with corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="Complex"/> properties of
      the calling object are set implicitly to match Vec1 and Vec2 objects.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and multiply them with corresponding Vec1 elements
      [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling vector must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide each of Num elements with corresponding elements in Den.</summary>
      
<remarks>Size and <see cref="Complex"/> property of the calling object are set automatically.
      The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide each of calling vector elements with corresponding elements in the Vec object.</summary>
      
<remarks>Size and <see cref="Complex"/> property of the calling object are set automatically. The result
      is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide calling vector elements [Index]...[Index+Len-1] with corresponding elements
       [VecIndex]...[VecIndex+Len-1] from the Vec object.</summary>
       
<remarks>Store the result in the claling vector. The <see cref="TOpenCLBase.Length">Length</see>
       of the calling vector is not changed. An exception is raised if
       array borders are overrun or underrun.

       The result of division by zero will be the INF constant. Division of zero
       by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide [NumIndex]..[NumIndex+Len-1] Num elements with [DenIndex]..[DenIndex+Len-1] elements in Den.</summary>
      
<remarks>and store result in the calling vector at positions [Index]..[Index+Len-1]
      <see cref="Size"/> and <see cref="Complex"/> property of the calling object are not changed.
      An exception is raised if array borders are overrun or underrun.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLMtxVec; NumIndex, DenIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide Value with elements of the calling object and store the result in the calling object.</summary>*)
    function DivideBy(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements of the calling object.</summary>
      
<remarks>Store the result in the calling object.
</remarks>
*)
    function DivideBy(Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      Calling vector will be extended to complex, if the calling vector is real.
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size and <see cref="Complex"/> properties of the calling object are set automatically.
</remarks>
*)
    function DivideBy(Value: double; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with Vec elements [VecIndex]..[VecIndes+Len-1].</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception will be raised array borders are overrun or underrun.
      <see cref="Complex"/> property of the calling object is set implicitly.
</remarks>
*)
    function DivideBy(Value: double; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [VecIndex]..[VecIndes+Len-1] from Vec.</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object. Size of the calling object is not changed. An exception will be raised
       array borders are overrun or underrun.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverse vector elements.</summary>
      
<remarks>The method reverses Vec vector elements from [VecIndex].. [VecIndex+Len-1]
      and stores them in the calling vector from [Index]...[Index+Len-1]
      by using the following equation:<para/>

      <IMG name="TVec24"/><para/>

      This overload reverses calling vector elements in-place.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
      begin
          a.CopyFromArray(2,2, TSingleArray.Create(1,2,3,4));
          a.Reverse;   // a = [4,3,2,1]
      end;
      </code>
      </Example>

      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>

      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Rotate(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TOpenCLMtxVec; overload;

    (*<summary>A shift on vector elements in range.</summary>
     
<remarks>Performs shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Shift(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TOpenCLMtxVec; overload;

    (*<summary>Subtracts Value from object elements.</summary>
      
<remarks>Subtracts Value from all calling object elements.
</remarks>


      <SeeAlso cref="Add"/>*)
    function Sub(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from all calling object complex elements.</summary>*)
    function Sub(Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtracts Value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtract real Value from Src.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract real Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; Value: double; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="Complex"/> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; Value: TCplx; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array subtraction.</summary>
      
<remarks>Subtract each of Vec elements from corresponding elements in the calling object.
      An exception is raised if Vec and calling object size and <see cref="Complex"/> properties do not match.
</remarks>

      <SeeAlso cref="Add"/>*)
    function Sub(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec2 elements from Vec1 elements.</summary>
      
<remarks>Stores the results in calling object. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically. An exception is raised if Vec1 and Vec2 size and <see cref="Complex"/> property do not match.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec elements [VecIndex]..[VecIndex+Len-1] from corresponding calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec22 elements [Vec2Index]..[Vec2Index+Len-1] from Vec1 object elements [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>Stores the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="Complex"/>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtraction from value.</summary>
      
<remarks>Subtract each of calling object elements from Value.
</remarks>


      <SeeAlso cref="Add"/>
      <SeeAlso cref="Sub"/>*)
    function SubFrom(Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract each of calling object elements from complex Value.</summary>
      
<remarks>If the calling vector s not complex, the conversion is performed automatically in a
      performance efficient way.
</remarks>
*)
    function SubFrom(Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from Value.</summary>
      
<remarks>Store the result in calling vector. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(Value: double; Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from complex Value.</summary>
      
<remarks>Store the result in calling object. If the calling vector is not complex, the conversion to complex is performed
      automatically in performance efficient way. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(Value: TCplx; Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="Complex"/> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(Value: double; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract complex Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size property of the calling object is set
      automatically. <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(Value: TCplx; Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from Value.</summary>
      
<remarks>Stores the result to the calling object elements [Index]..[Index+Len-1].
       Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="Complex"/> property of the calling object is adjusted automatically.
</remarks>
*)
    function SubFrom(Value: double; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from complex Value and store the result to the
       calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(Value: TCplx; Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

      (*<summary>Mean value.</summary>
        
<remarks>Calculate the mean value of all calling object elements. The result is a real value.
        An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>


        <Example>
        <code>
        var a: clMatrix;
            b: double;
        begin
          a.Size(2,2, clFloat, false);
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          b := a.Mean; // b = 2.5
        end;
        </code>
        </Example>


        <SeeAlso cref="Sum"/>
        <SeeAlso cref="Meanc"/>*)
      function Mean: double; overload;

      (*<summary>Returns real mean value from calling object elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
      function Mean(Index, Len: integer): double; overload;
      (*<summary>Calculate the mean value from calling object elements [Index]..[Index+Len-1].</summary>
        
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun/underrun.
</remarks>
*)
      procedure Mean(out AMean: double; Index, Len: integer); overload;
      (*<summary>Same as <see cref="Meanc"/>.</summary>*)
      procedure Mean(out AMean: TCplx); overload;
      (*<summary>Same as <see cref="Meanc"/>.</summary>*)
      procedure Mean(out AMean: TCplx; Index, Len: integer); overload;

      (*<summary>Mean value.</summary>
        <returns>the mean value of all calling object complex elements. The result is a complex value.</returns>
        
<remarks>An exception is raised is calling object <see cref="Complex"/> is .
</remarks>


        <SeeAlso cref="Mean"/>*)
      function Meanc: TCplx; overload;
      (*<summary>Returns complex mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
        
<remarks>An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
</remarks>
*)
      function Meanc(Index, Len: integer): TCplx; overload;
      (*<summary>Calculate the mean value from all calling object complex elements.</summary>
        
<remarks>The result AMean is a complex value. An exception is raised if calling object <see cref="Complex"/> property is .
</remarks>
*)
      procedure Meanc(out AMean: TCplx); overload;
      (*<summary>Calculate the mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
        
<remarks>The result AMean is a complex value.
        An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
</remarks>
*)
      procedure Meanc(out AMean: TCplx; Index, Len: integer); overload;


      (*<summary>Sums vector values.</summary>
      <returns>the sum of all calling object elements. An exception is raised if calling object
      <see cref="Complex"/> property is true.</returns>

      <example>
      <code>
      var a: clMatrix;
          b: double;
      begin
        a.Size(2,2, clFloat, false);
        a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
        b := a.Sum; // b = 10
      end;
      </code>
      </example>


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

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object elements. The result is a real value.</returns>
      
<remarks>An exception is raised is calling object <see cref="Complex"/> is true.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
          b: double;
      begin
          a.Size(2,2, TclFloatPrecision.clFloat, false);
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          b := a.Max; // b = 4
      end;
      </code>
      </Example>

      <SeeAlso cref="Min"/>
      <SeeAlso cref="Maxc"/>
      <SeeAlso cref="MaxMin"/>*)
    function Max: double; overload;
    (*<summary>Returns the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Max(Index,Len: integer): double; overload;
    (*<summary>Calculate the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Max(out AMax: double; Index,Len: integer); overload;
    (*<summary>Calculate the maximum value of all calling object elements.</summary>
      
<remarks>The AMax parameter returns the maximum value. The AIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure Max(out AMax: double; out AIndex: integer); overload;
    (*<summary>Calculate the maximum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the maximum value. The AIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Max(out AMax: double; out AIndex: integer; Index, Len: integer); overload;
    (*<summary>Same as <see cref="Maxc"/> method.</summary>*)
    function Max(out AMax: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object complex elements. Complex elements are first compared by the amplitude
        and then by the argument.</returns>

      
<remarks>An exception is raised if calling object <see cref="Complex"/> is .
</remarks>


      <SeeAlso cref="Max"/>*)
    function Maxc: TCplx; overload;
    (*<summary>Returns the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised if calling object
      <see cref="Complex"/> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(Index,Len: integer): TCplx; overload;
    (*<summary>Calculate the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns complex maximum value. Returns the index of maximum value. Complex elements are first compared by the amplitude and then by the argument.
      The AIndex parameter returns the index of maximum value. An exception is raised if calling object <see cref="Complex"/>
      property is  or array borders are overrud/underrun.
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
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure MaxMin(out AMax,AMin: double; Index, Len: integer); overload;
    (*<summary>Calculates the maximum and minimum value of all calling object elements.</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer); overload;
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or if array borders are overrun/underrun.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer; Index, Len: integer); overload;

    (*<summary>Minimum value.</summary>
      <returns>The minimum value of all calling object elements. The result is a real value.</returns>

      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
          b: double;
      begin
          a.Size(2,2, clFloat, false);
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          b := a.Min; // b = 1
      end;
      </code>
      </Example>

      <SeeAlso cref="Max"/>
      <SeeAlso cref="Minc"/>*)
    function Min: double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    function Min(Index,Len: integer): double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMin is a real value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrun.
</remarks>
*)
    procedure Min(out AMin: double; Index,Len: integer); overload;
    (*<summary>Calculate the minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the
      minimum value. The AIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Min(out AMin: double; out AIndex: Integer; Index, Len: integer); overload;
    (*<summary>Calculate the minimum value of all calling object elements.</summary>
      
<remarks>The AMin parameter returns the
      minimum value. The AIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    procedure Min(out AMin: double; out AIndex: integer); overload;
    (*<summary>Same as the <see cref="Minc"/> method.</summary>*)
    function Min(out AMin: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Minimum value.</summary>
      
<remarks>Returns the minimum value of all calling object complex elements. Complex elements are first compared by the amplitude
      and then by the argument. An exception is raised if calling object <see cref="Complex"/> is .
</remarks>


      <SeeAlso cref="Min"/>*)
    function Minc: TCplx; overload;
    (*<summary>Returns the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised
      if calling object. <see cref="Complex"/> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Minc(Index,Len: integer): TCplx; overload;
    (*<summary>Calculate the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMin parameter returns complex minimum value. Returns the index of minimum value. Complex elements are first compared by the amplitude and then by the argument.
      An exception is raised if calling object <see cref="Complex"/> property is  or array borders are overrun/underrun.
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
           TOpenCLMatrix a,b;
           clMtxVec.CreateIt(out a, out b);
           try
           {
              a.Size(2,2, TclFloatPrecision.clFloat, false);
              b.Size(2,2, TclFloatPrecision.clFloat, false);

              a.CopyFromArray(new double[] {1,2,3,4});
              b.CopyFromArray(new double[] {4,3,2,1});

              double c = a.NormC(b,true);
           }
           finally
           {
              clMtxVec.FreeIt(out a, out b);
           }
        }
      }
      </code></example>

      <SeeAlso cref="NormL1"/>
      <SeeAlso cref="NormL2"/>*)
    function NormC(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload;
    (*<summary>Calculates the C norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload;


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
          TOpenCLMatrix a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              a.Size(2,2, TclFloatPrecision.clFloat, false);
              b.Size(2,2, TclFloatPrecision.clFloat, false);
              a.CopyFromArray( new double[] {1,2,3,4});
              b.CopyFromArray( new double[] {4,3,2,1});
              double c = a.NormL1(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL2"/>*)
    function NormL1(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload;
    (*<summary>Calculates the L1 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload;

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
          clMtxVec.TOpenCLMatrix a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.Size(2,2, TclFloatPrecision.clFloat, false);
            b.Size(2,2, TclFloatPrecision.clFloat, false);

            a.CopyFromArray(new double[] {1,2,3,4});
            b.CopyFromArray(new double[] {4,3,2,1});

            double c = a.NormL2(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL1"/>*)
    function NormL2(const Vec: TOpenCLMtxVec; RelativeError: boolean = False): double; overload;
    (*<summary>Calculates the L2 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload;

    (*<summary>Calculate the L2-norm of the calling vector.</summary>
     <returns>L2 norm, defined by: <c>NormaL2 = ( Sum(|a[i]|^2) )^0.5  , 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormL2: double; overload;
    (*<summary>Calculates the L2 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(Index,Len: integer): double;  overload;

    (*<summary>Computes the sum and the sum of squared elements from the elements in the calling object.</summary>*)
    procedure SumAndSqrSum(out Sum, SqrSum: double); overload;

    (*<summary>Returns the sum and the sum of squared elements.</summary>
               
<remarks>Returns the sum and the sum of squared items from calling vector elements [Index]..[Index+Len-1].
</remarks>
*)

    procedure SumAndSqrSum(out Sum, SqrSum: double; Index, Len: integer); overload;

    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="Complex"/> property is true.
</remarks>


      <Example>
      <code>
      var a: clMatrix;
          c: double;
          aMean: double;
      begin
          a.Size(2,2, clFloat, false);
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          Caption := FloatToSTr(a.StdDev);
      end;
      </code>
      </Example>

      <SeeAlso cref="Mean"/>*)
    function StdDev: double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="Complex"/> property is true or if array
      borders are overrun/underrun.
</remarks>
*)
     function StdDev(Index,Len: integer): double; overload;
    (*<summary>Returns the standard deviation of all calling object elements.</summary>
       
<remarks>The sum and the sum of squares of all calling object elements must be passed as parameters.
       An exception is raised if calling object <see cref="Complex"/> property is true.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double): double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The sum and the sum of squares of the coresponding elements must be passed as parameters.
      An exception is raised if calling object <see cref="Complex"/> property is true
      or if array borders are overrun/underrun.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double; Index, Len: integer): double; overload;


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
          clMtxVec.TOpenCLMatrix a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            double c = a.Product(); // c= 24
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
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
          clMtxVec.TOpenCLMatrix a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.Size(2,2, TclFloatPrecision.clFloat, false);
            b.Size(2,2, TclFloatPrecision.clFloat, false);

            a.CopyFromArray(new double[] {1,2,3,4});
            b.CopyFromArray(new double[] {4,3,2,1});

            double prod = a.DotProd(b); // = 1*5 + 2*6 + * 3*7 + 4*8
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)
    function DotProd(const Vec: TOpenCLMtxVec): double; overload;

    (*<summary>Returns the scalar product between Vec elements [VecIndex]..[VecIndex+Len-1] and calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property is True.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProd(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): double; overload;


    (*<summary>Scalar product of two complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a complex scalar value.
      An exception is raised if calling or Vec object <see cref="Complex"/> property is false.
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
          clMtxVec.TOpenCLMatrix a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.Size(2,2, TclFloatPrecision.clFloat, false);
            b.Size(2,2, TclFloatPrecision.clFloat, false);

            a.CopyFromArray(new double[] {1,2,3,4});
            b.CopyFromArray(new double[] {4,3,2,1});
            double prod = a.DotProdc(b); //= (1+2i)*(5+6i)+(3+4i)*(7+8i)
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProd"/>*)
    function DotProdc(const Vec: TOpenCLMtxVec): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TCplx; overload;
    (*<summary>Returns the scalar product between Vec and calling object complex elements.</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="Complex"/> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean; VecIndex, Index, Len: integer): TCplx; overload;


    (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>
      
<remarks>Convert all elements of the calling matrix to strings with  formating real parts with ReFormat, imaginary parts with ImFormat,
      using the text delimiter Delimiter and store them in aList, by using the Add method of TStrings object.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


      <Example>
      <code>
      procedure TForm1.Button1Click(Sender: TObject);
      var a,b: TOpenCLMatrix;
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
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>*)
    procedure ValuesToStrings(dstList: TStrings; const Delimiter: string = kTab;
                                               const ReFormat: string = ' 0.######;-0.#######';
                                               const ImFormat: string = '+0.######i;-0.######i'); overload;

    (*<summary>nvert calling matrix elements, starting with [Row,Col] and converting Len elements to strings.</summary>
      
<remarks>Vormating real parts with ReFormat, imaginary parts with ImFormat, using the text delimiter Delimiter and store them in aList starting at ListIndex.
      If aList is not large enough, the method will use the Add method of aList object.
</remarks>
*)
    procedure ValuesToStrings(dstList: TStrings; ListIndex,Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab;
                                     const ReFormat: string = ' 0.######;-0.######';
                                     const ImFormat: string = '+0.######i;-0.######i'); overload;



    (*<summary>Converts all calling matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Delimiter: string = kTab;
        const ReFormat: string = ' 0.######;-0.#######';
        const ImFormat: string = '+0.######i;-0.######i');overload;

    (*<summary>Converts Row..Col to Row+RowCount..Col+ColCount matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab;
                                     const ReFormat: string = ' 0.######;-0.######';
                                     const ImFormat: string = '+0.######i;-0.######i'); overload;




    function Size(aRows, aCols: integer; aPrecision: TclFloatPrecision; aComplex: boolean): TOpenCLMatrix; overload;
    function Size(ARows, ACols: integer; aPrecision: TclFloatPrecision): TOpenCLMtxVec; overload;
    function Size(const Src: TOpenCLMtxVec; aComplex: boolean): TOpenCLMtxVec; overload;
    function Size(const Src: TOpenCLBase): TOpenCLMtxVec; overload;

    property FloatPrecision: TCLFloatPrecision read GetFloatPrecision write SetFloatPrecision;
    property Tag: PointerInteger read GetTag write SetTag;
    property Caption: string read GetCaption write SetCaption;
    property Length: integer read GetLength write SetLength;
    property Complex: boolean read GetComplex write SetComplex;
    property Rows: integer read GetRows write SetRows;
    property Cols: integer read GetCols write SetCols;
    constructor Create(TargetDevice: TOpenCLDevice; IsCached: boolean = true); overload;
    constructor Create(DeviceSource: TOpenCLMtxVec; IsCached: boolean = true); overload;
    constructor Create(aRows, aCols: integer; aPrecision: TclFloatPrecision; aComplex: boolean; IsCached: boolean = true); overload;
    constructor Create(IsCached: boolean); overload;
  end;



  (*<summary>Open CL single value class with overloaded operators.</summary>
             
<remarks>Copying data from and to (from CPU separate) GPU memory can cause long delays even if that data is short.
             Sometimes it makes sense to keep even single value variables (scalars) in the GPU memory. Use this object
             to avoid copying individual values from GPU to CPU memory and reuse the scalars directly.
</remarks>
*)

  clValue = record
  strict private
    
    FData: TOpenCLValue;
    
    function get_Data: TOpenCLValue;
    function GetCaption: string;
    function GetComplex: boolean;
    function GetTag: PointerInteger;
    procedure SetCaption(const Value: string);
    procedure SetComplex(const Value: boolean);
    procedure SetTag(const Value: PointerInteger);
    function GetClFloatPrecision: TclFloatPrecision;
    procedure SetClFloatPrecision(const Value: TclFloatPrecision);
    function GetDevice: TOpenCLDevice;
    procedure CreateFromCache;
    property Data: TOpenclValue read get_Data;
  public
    (*<summary>Add left to aRight and return result.</summary>*)
    class operator Add(const Left: TCplx; const Right: clValue): clValue;
    (*<summary>Add Right to Left and return result.</summary>*)
    class operator Add(const Left: clValue; const Right: TCplx): clValue;
    (*<summary>Add Left to Right and return result.</summary>*)
    class operator Add(Left: double;const Right: clValue): clValue;
    (*<summary>Add Right to Left and return result.</summary>*)
    class operator Add(const Left: clValue; Right: double): clValue;
    (*<summary>Add Left and Right.</summary>*)
    class operator Add(Left: TOpenCLValue;const Right: clValue): clValue;
    (*<summary>Add Left and Right.</summary>*)
    class operator Add(const Left: clValue; Right: TOpenCLValue): clValue;
    (*<summary>Add Left and Right.</summary>*)
    class operator Add(const Left: clValue;const Right: clValue): clValue;

    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(const Left: TCplx;   const Right: clValue): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(const Left: clValue; const Right: TCplx): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(Left: double; const Right: clValue): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(const Left: clValue; Right: double): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(const Left: clValue; Right: TOpenCLValue): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(Left: TOpenCLValue; const Right: clValue): clValue;
    (*<summary>Subtract Right from Left.</summary>*)
    class operator Subtract(const Left: clValue;const Right: clValue): clValue;

    (*<summary>Multiply Left with Right.</summary>*)
    class operator Multiply(const Left: clValue; const Right: TCplx): clValue;
    (*<summary>Multiply Right with Left.</summary>*)
    class operator Multiply(const Left: TCplx; const Right: clValue): clValue;

    (*<summary>Multiply Left with Right.</summary>*)
    class operator Multiply(const Left: clValue; Right: double): clValue;
    (*<summary>Multiply Right with Left.</summary>*)
    class operator Multiply(Left: double; const Right: clValue): clValue;
    (*<summary>Multiply Left with Right.</summary>*)
    class operator Multiply(const Left: clValue; Right: TOpenCLValue): clValue;
    (*<summary>Multiply Left with Right.</summary>*)
    class operator Multiply(Left: TOpenCLValue; const Right: clValue): clValue;
    (*<summary>Multiply Left with Right</summary>*)
    class operator Multiply(const Left: clValue; const Right: clValue): clValue;

    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(const Left: clValue; const Right: TCplx): clValue;
    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(const Left: TCplx; const Right: clValue): clValue;
    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(const Left: clValue; Right: double): clValue;
    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(Left: double; const Right: clValue): clValue;

    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(const Left: clValue; Right: TOpenCLValue): clValue;
    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(Left: TOpenCLValue; const Right: clValue): clValue;
    (*<summary>Divide Left with Right.</summary>*)
    class operator Divide(const Left: clValue; const  Right: clValue): clValue;

    (*<summary>Negates value inside AValue.</summary>*)
    class operator Negative(const AValue: clValue): clValue;

    class operator Explicit(const AValue: TCplx): clValue;
    class operator Explicit(const AValue: double): clValue;
    class operator Explicit(const AValue: clValue): TCplx;
    class operator Explicit(const AValue: clValue): double;
    class operator Explicit(const AValue: TOpenclValue): clValue;

    class operator Implicit(const AValue: clValue): TOpenCLValue;
    class operator Implicit(const AValue: clValue): TOpenCLBase;
  public
    (*<summary>Obtains the precision (single or double) from Src and sets Complex property to aComplex.</summary>*)
    function Size(const Src: TOpenCLBase; aComplex: boolean): TOpenCLBase; overload;
    (*<summary>Specify the precision (single or double) and if the value is to be complex or real.</summary>*)
    procedure Size(aPrecision: TclFloatPrecision; aComplex: boolean); overload;

    (*<summary>Copy object value.</summary>
      
<remarks>Copy value from Src elements to the calling object. Precision and <see cref="Complex"/>
      properties of the calling object are set implicitly to match Src object.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          a.Copy(2);
          b.Copy(a);                // b = [1,2,3,4]
      end;
      </code>
      </Example>*)

    function Copy(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Copy object value from CPU to GPU memory.</summary>*)
    function Copy(const Value: double): TOpenCLValue; overload;
    (*<summary>Copy object value from CPU to GPU memory.</summary>*)
    function Copy(const Value: TCplx): TOpenCLValue; overload;

    (*<summary>Copy object value from GPU to CPU memory.</summary>*)
    function Copy: double; overload;
    (*<summary>Copy object value from GPU to CPU memory.</summary>*)
    function Copyc: TCplx; overload;

    (*<summary>Add values in Src1 and Src2.</summary>
      
<remarks>The result is stored in the calling object. <see cref="FloatPrecision"/> and <see cref="Complex"/> properties
      of the calling object are set implicitly to match Src1 and Src2.
</remarks>
*)
    function Add(const Src1, Src2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Add values in Src1 and Src2.</summary>
      
<remarks>The result is stored in the calling object. <see cref="FloatPrecision"/> and <see cref="Complex"/> properties
      of the calling object are set implicitly to match Vec and Value.
</remarks>
*)
    function Add(const Vec: TOpenCLValue; Value: double): TOpenCLValue; overload;
    (*<summary>Add values in Src1 and Src2.</summary>
      
<remarks>The result is stored in the calling object. <see cref="FloatPrecision"/> and <see cref="Complex"/> properties
      of the calling object are set implicitly to match Vec and Value.
</remarks>
*)
    function Add(const Vec: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Split complex value in real and imaginary part.</summary>
      
<remarks>Split calling objects value into real and imaginary components. Store the real component in ReVec and
      imaginary component in ImVec. FloatPrecision and <see cref="Complex"/> properties of ReVec and ImVec
      are set implicitly to match with the calling value object. An execption is raised, if the calling object is not complex.
</remarks>


      <Example>
      <code>
      var a,b,c: clValue;
      begin
          a.Copy(Cplx(3,4));    // a= [1-2i, 3+4i]
          a.CplxToReal(b,c);  // b = 3, c = 4
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLValue); overload;

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The results are stored in the calling object. Size and <see cref="Complex"/> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="Complex"/> property is True.
</remarks>


      <Example>
      <code>
      var a,b,c: clValue;
      begin
          a.Copy(3);
          b.Copy(4);
          c.RealToCplx(a,b);  //c =3+4i;
      end;
      </code>
      </Example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(ReVec, ImVec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Conjugate.</summary>
      
<remarks>Conjugate calling object value.
</remarks>


      <Example>
      <code>
      var c: clValue;
      begin
          d.Copy(Cplx(2,3));
          c.Conj(d);   // d = 2-3i
      end;
      </code>
      </Example>*)

    function Conj(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.

     Sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is not used
     implicitely within TOpenCLValue methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down. This is
     where "FixAngle" can help.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle(const Src: TOpenCLValue): TOpenCLValue;  overload;

    (*<summary>Rounds Src object value towards negative infinity and stores the result
      in the calling object.</summary>
      
<remarks>FloatPrecision and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TOpenCLValue): TOpenCLValue;  overload;

    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate the calling object complex exponential in-place. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      <Example>
      <code>
      var a: clValue;
      begin
          b.Copy(2);
          a.Expj(b);   // a = e^(2i)
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)

    function Expj(Omega: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part for all object values in-place.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.Frac(b);  // a = 0.3
      end;
      </code>
      </Example>


      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac(const X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>Complementary error functon of value.</summary>
       
<remarks>Calculates the complementary error function value for object value.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.Erfc(b);  // a = Erfc(2.3)
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Error function of value.</summary>
      
<remarks>Calculates the error function for object value.
</remarks>


       <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.Erf(b);  // a = Erf(2.3)
      end;
      </code>
      </Example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Flips the real and imaginary part of complex number.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b + i*a</c><para/>
      Method flips the real and imaginary part of complex number X and stores the result in the calling object.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function Flip(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Flips the real and imaginary part and conjugates the complex number.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling objects complex value.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj(const X: TOpenCLValue): TOpenCLValue; overload;


    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(x[i]/y[i]).</c><para/>
      The results will be saved to the calling object.
</remarks>


      <Example>
      <code>
      var a,b,c: clValue;
      begin
          a.Copy(4);
          b.Copy(3);
          c.Rem(a,b); // c = Rem(4,3)
      end;
      </code>
      </Example>*)
    function Rem(const X, Y: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Log base N.</summary>
      <returns>Computes Log with base N.</returns>

      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.LogN(4, b);  // // a = LogN(4, 2.3)
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function LogN(N: double; X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>Raises base object elements to any power.</summary>
    
<remarks>Raises Base calling object elements to any power. The <see cref="IntPower"/> is faster, if Exponent is an integer.
    Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
    To compute a power to the negative exponent in general case or when the base is negative,
    use the complex version of the function.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          c.Copy(4);
          a.Power(b, c);  // // a = b^c
      end;
      </code>
      </Example>

      <SeeAlso cref="IntPower"/>*)

    function Power(const Base, Exponent: TOpenCLValue): TOpenCLValue; overload;

    function Power(const Base: double; Exponent: TOpenCLValue): TOpenCLValue; overload;
    function Power(const Base: TCplx; Exponent: TOpenCLValue): TOpenCLValue; overload;

    function Power(const Base: TOpenCLValue; Exponent: double): TOpenCLValue; overload;
    function Power(const Base: TOpenCLValue; Exponent: TCplx): TOpenCLValue; overload;

    (*<summary>Rounds Src value.</summary>
      
<remarks>Stores the result in the calling object.
</remarks>
*)
    function Round(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary> Computes signum function from Src value and stores the result in the calling object.  </summary>
                 
<remarks>Size and <see cref="Complex"/> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)

    function Sgn(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of Src object value and multiplies it with the calling object value.
      Signum(const X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      The length of Src and of the calling object must match
      or an exception will be raised. Size and <see cref="Complex"/> property of calling object are
      adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine for calling object value and stores the sine
      to SinX and cosine to CosX.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      <Example>
      <code>
      var a, s,c: clValue;
      begin
          a.Copy(0);
          a.SinCos(s,c); // s=[0], c =[1]
      end;
      </code>
      </Example>


      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(SinX, CosX: TOpenCLValue); overload; 

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for calling object value and stores
      the sine to SinhX and cosine to CoshX. FloatPrecision and <see cref="Complex"/> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      <Example>
      <code>
      var a, s, c: clValue;
      begin
          a.Copy(0);
          a.SinhCosh(s,c);
      end;
      </code>
      </Example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLValue); overload;

   (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on Src object value. The Value parameter is a lower bound for threshold operation.
      A value smaller than Value will be replaced with Value and stored in to the calling object.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.1);
          a.ThreshBottom(b, 0.2); // a = [0.2]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)

    function ThreshBottom(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation on Src object value. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(3);
          a.ThreshTop(b, 0.2); // a = [0.2]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThreshTop"/>*)

    function ThreshTop(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on Src object value. The LTValue parameter is an <b>lower</b> bound for
      threshold operation. Value less than LTLevel will be replaced with LTValue.
      For complex number comparison is applied with the norm of the complex value.
</remarks>


      <Example>
      <code>
      var a: clValue;
      begin
          b.Copy(2));
          a.ThresholdLT(b, 2.3,1.5); // a = [1.5]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(const Vec: TOpenCLValue; LTLevel, LTValue: double): TOpenCLValue; overload;
    (*<summary>Perform "less than" threshold operation for Vec object complex number.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of the calling object
      are adjusted automatically. If Vec.Complex is false, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLValue; LTLevel: double; LTValue: TCplx): TOpenCLValue; overload;

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on Vec object value. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      Value bigger than LTLevel will be replaced with GTValue.
      For a complex number the comparison is applied with the norm of the complex value.
</remarks>


      <Example>
      <code>
      var a: clValue;
      begin
          b.Copy(2);
          a.ThresholdGT(b, 1.3,1.5); // a = [1.5]
      end;
      </code>
      </Example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(const Vec: TOpenCLValue; GTLevel, GTValue: double): TOpenCLValue; overload;

    (*<summary>Perform "greater than" threshold operation vor complex number stored in Vec object.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of the
      calling object are adjusted automatically. If Vec object value is not Complex, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLValue; GTLevel: double; GTValue: TCplx): TOpenCLValue; overload;

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on Vec value. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      Values less than LTLevel will be replaced with LTValue. Value bigger than GTLevel will be replaced with GTValue.
      Operation is available only for none Complex values.
</remarks>


      <example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(3);
          a.ThresholdGT_LT(b, 2.3,3.4,1,0.5); // a = [3.4]
      end;
      </code>
      </example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (const Vec: TOpenCLValue; GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLValue; overload;

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
          
<remarks>Rounds calling object value towards zero to an integer and stores
          the result in the TruncDst object as a floating point number. The fractional
          part is stored in the FracDst.
</remarks>


          <SeeAlso cref="Frac"/>
          <SeeAlso cref="Round"/>*)

    procedure TruncAndFrac(TruncDst: TOpenCLValue; FracDst: TOpenCLValue); overload;

    (*<summary>Truncate Src object value.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="Complex"/>
      property of calling object are adjusted automatically.
</remarks>
*)

    function Trunc(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of the X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.InvCbrt(b);  // a = 2.3^(1/3)
      end;
      </code>
      </Example>

      <SeeAlso cref="Cbrt"/>*)

    function InvCbrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2)) of X.</c>
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(2.3);
          a.InvCbrt(b);  // a = 2.3^(1/3)
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude for X.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(Cplx(2,3));
          a.Mag(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Abs"/>*)
    function Mag(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Multiply Src1 with Src2.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="Complex"/> property of calling object
      are adjusted automatically to match those of Src1 and Src2.
      An exception is raised if Src1 and Src2 size and <see cref="Complex"/> property do not match.
</remarks>
*)
    function Mul(const Src1, Src2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Multiply each element of Vec with Value.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="Complex"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Multiply Src with complex Value.</summary>
      
<remarks>Store the result in the calling object. Size of the calling object is set automatically.
      <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Multiply X with 1i and store the result in the calling object.</summary>*)
    function MulI(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Converts the polar magnitude/phase pair to cartesian pair.</summary>
      
<remarks>Convert AmpltVec and PhaseVec elements (combined) from polar to cartesian form.
      The result is stored as a complex number (x=Re, y=Im) in the calling
      object.  FloatPrecision and <see cref="Complex"/> properties of the calling object are set implicitly to
      match AmpltVec and PhaseVec objects.
</remarks>


      <Example>
      <code>
      var a,b,c: clValue;
      begin
          a.Copy(2); // a = [2] //magnitude
          b.Copy(3); // b = [1] //phase
          c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
      end;
      </code>
      </Example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Gets real part of complex object value.</summary>
      
<remarks>The method method gets the real part of a complex object Vec and stores the real result in the calling
      object. FloatPrecision and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          a.Copy(Cplx(2,3)); // = [2+3i]
          b.RealPart(a); // b = [2]
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Square.</summary>
      
<remarks>Calculate the square of X.
</remarks>


      <Example>
      <code>
      var a, b: clValue;
      begin
          b.Copy(2);
          a.Sqr(b); // a=[4]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Sqrt(b); // a=[2]
      end;
      </code>
      </Example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Sin(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Sin(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the cosine of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Cos(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Cos(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Tan(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)

    function Tan(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of x.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Cot(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)

    function Cot(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of X.
</remarks>

      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Sec(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)

    function Sec(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.Csc(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)

    function Csc(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of X. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.ArcSin(b);

      end;
      </code>
      </Example>

      <SeeAlso cref="Sin"/>*)

    function ArcSin(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine of X. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(4);
          a.ArcCos(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks><para/>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object.

      The values of X and Y must be between -2*<see cref="Math387.PI"/> and 2*<see cref="Math387.PI"/>.
      If they are not, consider adjusting the angle by calling the <see cref="Math387.FixAngle"/> routine.
      In addition, the value of X can't be 0. The result will fall in the range from -2<see cref="Math387.PI"/> and +<see cref="Math387.PI"/>
      radians. Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(Y, X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for X. The return values are expressed in radians.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.ArcTan(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens for X. The returned value is expressed in radians.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.ArcCot(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse secant of X and stores the results in the calling object. </summary>
               
<remarks>FloatPrecision and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse cosecant of X.</summary>
      
<remarks>Store the results in the calling object.
</remarks>
*)

    function ArcCsc(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.Sinh(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of all caling object elements in-place.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.Cosh(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.Tanh(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)

    function Tanh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(0.5);
          a.Coth(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the hyperbolic secant of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision
      and <see cref="Complex"/> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the hyperbolic cosecant of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision
      and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Absolute values of X.</summary>
      
<remarks>Calculate the absolute value of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(-0.5);
          a.Abs(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Mag"/>*)
    function Abs(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic sine of X.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic cosine of X.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic tangens of X.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverser hyperbolic cotangens of X.</summary>
      
<remarks>Store the result in calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic secant of X.</summary>
      
<remarks>Store the result in calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic cosecant of X.</summary>
      
<remarks>Store the result in calling object. FloatPrecision and <see cref="Complex"/> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Cbrt(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil(const Src: TOpenCLValue): TOpenCLValue;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Ln(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp"/>*)
    function Ln(const X: TOpenCLValue): TOpenCLValue; overload;


    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Log10(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp10"/>*)

    function Log10(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Log2(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Exp2"/>*)
    function Log2(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent e^X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Exp(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Ln"/>*)

    function Exp(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent base 2 (2^X).</summary>
      
<remarks>Calculate the exponent base 2 (2^X).
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Exp2(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Log2"/>*)
    function Exp2(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent base 10 (10^X).</summary>
      
<remarks>Calculate the exponent base 10 (10^X).
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Exp10(b);
      end;
      </code>
      </Example>

      <SeeAlso cref="Log10"/>*)
    function Exp10(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Gets the imaginary part of a complex object.</summary>
      
<remarks>Gets the imaginary part of a complex object Vec and stores the real result in the calling object.
      FloatPrecision and <see cref="Complex"/> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="Complex"/> must be true otherwise an exception will be raised.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(Cplx(8));
          a.ImagPart(b);  // a = 0
      end;
      </code>
      </Example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power Base^Exponent.
      For non integer exponents, the <see cref="Power"/> method can be used.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.IntPower(b, 2);
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>*)
    function IntPower(const aBase: TOpenCLValue; Exponent: Integer): TOpenCLValue; overload;

    (*<summary>Inverse (1/x).</summary>
      
<remarks>Calculates the inverse of X.
</remarks>


      <Example>
      <code>
      var a,b: clValue;
      begin
          b.Copy(8);
          a.Inv(b);  // a = 1/8
      end;
      </code>
      </Example>

      <SeeAlso cref="Divide"/>*)
    function Inv(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Converts value from cartesian to polar coordinate form.</summary>
      
<remarks>Converts calling object value from cartesian to polar coordinate form, storing the magnitude (radius)
      component in the AmpltVec and the phase (angle) component in the PhaseVec.
      If you want to use this method then the calling objects <see cref="Complex"/> property must be
      true. If this is not the case, an exception is raised. FloatPrecision and <see cref="Complex"/> properties of AmpltVec and
      PhaseVec are set automatically.
</remarks>


      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLValue); overload;

    (*<summary>Divide Num with Den.</summary>
      
<remarks>The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Divide Value with Vec and store the result in the calling object.</summary>*)
    function DivideBy(Value: double; Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Divide complex Value with Vec and store the result in the calling object.</summary>
      
<remarks>Size of the calling object is set automatically.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Subtract real Value from Src.</summary>
              
<remarks>Store the result in calling object.
</remarks>
*)
    function Sub(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="Complex"/> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Computes Vec1 - Vec2.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="Complex"/> property of the calling object are
      adjusted automatically.
      An exception is raised if Vec1 and Vec2 FloatPrecision and <see cref="Complex"/> properties do not match.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="Complex"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(Value: double; Vec: TOpenCLValue): TOpenCLValue; overload;
    (*<summary>Substract complex Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision property of the calling object is set
      automatically. <see cref="Complex"/> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(Value: TCplx; Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Returns the device on which we allocated memory for this object. </summary>*)
    property Device: TOpenCLDevice read GetDevice;
    property FloatPrecision: TCLFloatPrecision read GetclFloatPrecision write SetclFloatPrecision;
    property Tag: PointerInteger read GetTag write SetTag;
    property Caption: string read GetCaption write SetCaption;
    property Complex: boolean read GetComplex write SetComplex;
  end;






  (*<summary>Subrange the source clVector. </summary>
             
<remarks>Internally calls <see cref="clVector.Select"/>
</remarks>
*)
  
  function Select(const Src: TOpenCLVector; StartIndex, Step, StopIndex: integer): clVector; overload;

  (*<summary>Subrange the source clVector without copy operation. </summary>
             
<remarks>Internally calls <see cref="clVector.Select"/>
</remarks>
*)
  
  function Select(const Src: TOpenCLVector; StartIndex, StopIndex: integer): clVector; overload;

  (*<summary>Subrange the source clMatrix and return a clVector. </summary>
             
<remarks>Internally calls <see cref="clVector.Select"/>
</remarks>
*)
  
  function Select(const Src: TOpenCLMatrix; StartIndex, Step, StopIndex: integer): clVector; overload;

  (*<summary>Subrange the source clMatrix and return a clVector without copy operation. </summary>
             
<remarks>Internally calls <see cref="clVector.Select"/>
</remarks>
*)
  
  function Select(const Src: TOpenCLMatrix; StartIndex, StopIndex: integer): clVector; overload;

  (*<summary>Compute the sin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Sin"/>
</remarks>
*)
  
  function Sin(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the sin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sin"/>
</remarks>
*)
  
  function Sin(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the cos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Cos"/>
</remarks>
*)
  
  function Cos(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the cos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Cos"/>
</remarks>
*)
  
  function Cos(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Tan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Tan"/>
</remarks>
*)
  
  function Tan(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Tan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Tan"/>
</remarks>
*)
  
  function Tan(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Cot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Cot"/>
</remarks>
*)
  
  function Cot(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Cot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Cot"/>
</remarks>
*)
  
  function Cot(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Sec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Sec"/>
</remarks>
*)
  
  function Sec(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Sec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sec"/>
</remarks>
*)
  
  function Sec(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Csc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Csc"/>
</remarks>
*)
  
  function Csc(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Csc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Csc"/>
</remarks>
*)
  
  function Csc(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcSin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcSin"/>
</remarks>
*)
  
  function ArcSin(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcSin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcSin"/>
</remarks>
*)
  
  function ArcSin(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCos"/>
</remarks>
*)
  
  function ArcCos(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCos"/>
</remarks>
*)
  
  function ArcCos(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcTan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcTan"/>
</remarks>
*)
  
  function ArcTan(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcTan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcTan"/>
</remarks>
*)
  
  function ArcTan(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcTan2 function from X and Y and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcTan2"/>
</remarks>
*)
  
  function ArcTan2(Y,X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcTan2 function from X and Y and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcTan2"/>
</remarks>
*)
  
  function ArcTan2(Y,X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCot"/>
</remarks>
*)
  
  function ArcCot(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCot"/>
</remarks>
*)
  
  function ArcCot(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcSec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcSec"/>
</remarks>
*)
  
  function ArcSec(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcSec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcSec"/>
</remarks>
*)
  
  function ArcSec(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCsc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCsc"/>
</remarks>
*)
  
  function ArcCsc(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCsc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCsc"/>
</remarks>
*)
  
  function ArcCsc(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Sinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Sinh"/>
</remarks>
*)
  
  function Sinh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Sinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sinh"/>
</remarks>
*)
  
  function Sinh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Cosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Cosh"/>
</remarks>
*)
  
  function Cosh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Cosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Cosh"/>
</remarks>
*)
  
  function Cosh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Tanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Tanh"/>
</remarks>
*)
  
  function Tanh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Tanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Tanh"/>
</remarks>
*)
  
  function Tanh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Coth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Coth"/>
</remarks>
*)
  
  function Coth(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Coth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Coth"/>
</remarks>
*)
  
  function Coth(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Sech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Sech"/>
</remarks>
*)
  
  function Sech(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Sech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sech"/>
</remarks>
*)
  
  function Sech(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Csch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Csch"/>
</remarks>
*)
  
  function Csch(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Csch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Csch"/>
</remarks>
*)
  
  function Csch(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcSinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcSinh"/>
</remarks>
*)
  
  function ArcSinh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcSinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcSinh"/>
</remarks>
*)
  
  function ArcSinh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCosh"/>
</remarks>
*)
  
  function ArcCosh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCosh"/>
</remarks>
*)
  
  function ArcCosh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcTanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcTanh"/>
</remarks>
*)
  
  function ArcTanh(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcTanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcTanh"/>
</remarks>
*)
  
  function ArcTanh(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCoth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCoth"/>
</remarks>
*)
  
  function ArcCoth(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCoth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCoth"/>
</remarks>
*)
  
  function ArcCoth(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcSech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcSech"/>
</remarks>
*)
  
  function ArcSech(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcSech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcSech"/>
</remarks>
*)
  
  function ArcSech(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the ArcCsch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.ArcCsch"/>
</remarks>
*)
  
  function ArcCsch(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the ArcCsch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ArcCsch"/>
</remarks>
*)
  
  function ArcCsch(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Abs function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Abs"/>
</remarks>
*)
  
  function Abs(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Abs function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Abs"/>
</remarks>
*)
  
  function Abs(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Cbrt function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Cbrt"/>
</remarks>
*)
  
  function Cbrt(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Cbrt function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Cbrt"/>
</remarks>
*)
  
  function Cbrt(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Ceil function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Ceil"/>
</remarks>
*)
  
  function Ceil(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Ceil function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Ceil"/>
</remarks>
*)
  
  function Ceil(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Ln function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Ln"/>
</remarks>
*)
  
  function Ln(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Ln function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Ln"/>
</remarks>
*)
  
  function Ln(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Log10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Log10"/>
</remarks>
*)
  
  function Log10(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Log10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Log10"/>
</remarks>
*)
  
  function Log10(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Log2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Log2"/>
</remarks>
*)
  
  function Log2(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Log2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Log2"/>
</remarks>
*)
  
  function Log2(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Exp function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Exp"/>
</remarks>
*)
  
  function Exp(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Exp function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Exp"/>
</remarks>
*)
  
  function Exp(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Exp2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Exp2"/>
</remarks>
*)
  
  function Exp2(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Exp2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Exp2"/>
</remarks>
*)
  
  function Exp2(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the Exp10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clVector.Exp10"/>
</remarks>
*)
  
  function Exp10(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the Exp10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Exp10"/>
</remarks>
*)
  
  function Exp10(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clVector.ImagPart"/>
</remarks>
*)
  
  function ImagPart(const X: TOpenCLVector): clVector; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clVector.ImagPart"/>
</remarks>
*)

  function Imag(const X: TOpenCLVector): clVector; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ImagPart"/>
</remarks>
*)
  
  function ImagPart(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ImagPart"/>
</remarks>
*)

  function Imag(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the integer power function. </summary>
             
<remarks>Internally calls <see cref="clVector.IntPower"/>
</remarks>
*)
  
  function IntPower(const Base: TOpenCLVector; Exponent: integer): clVector; overload;

  (*<summary>Compute the integer power function. </summary>
             
<remarks>Internally calls <see cref="clMatrix.IntPower"/>
</remarks>
*)
  
  function IntPower(const Base: TOpenCLMatrix; Exponent: integer): clMatrix; overload;


  (*<summary>Compute 1/X. </summary>
             
<remarks>Internally calls <see cref="clVector.Inv"/>
</remarks>
*)
  
  function Inv(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute 1/X. </summary>
             
<remarks>Internally calls <see cref="clMatrix.InvElem"/>
</remarks>
*)
  
  function Inv(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clVector.InvSqrt"/>
</remarks>
*)
  
  function InvSqrt(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clMatrix.InvSqrt"/>
</remarks>
*)
  
  function InvSqrt(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clVector.InvCbrt"/>
</remarks>
*)
  
  function InvCbrt(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clMatrix.InvCbrt"/>
</remarks>
*)
  
  function InvCbrt(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Return complex conjugate of the source. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Conj"/>
</remarks>
*)
  
  function Conj(const X: TOpenCLVector): clVector; overload;

  (*<summary>Return complex conjugate of the source. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Conj"/>
</remarks>
*)
  
  function Conj(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Extend the source to complex numbers.  </summary>
             
<remarks>Internally calls <see cref="clVector.ExtendToComplex"/>
</remarks>
*)
  
  function ExtendToComplex(const X: TOpenCLVector; Zeros: boolean = true): clVector; overload;

  (*<summary>Extend the source to complex numbers.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.ExtendToComplex"/>
</remarks>
*)
  
  function ExtendToComplex(const X: TOpenCLMatrix; Zeros: boolean = true): clMatrix; overload;

  (*<summary>Make the angle fit within +/- PI.  </summary>
             
<remarks>Internally calls <see cref="clVector.FixAngle"/>
</remarks>
*)
  
  function FixAngle(const X: TOpenCLVector): clVector; overload;

  (*<summary>Make the angle fit within +/- PI.  </summary>
             
<remarks>Internally calls <see cref="clVector.FixAngle"/>
</remarks>
*)
  
  function FixAngle(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the floor function from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Floor"/>
</remarks>
*)
  
  function Floor(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the floor function from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Floor"/>
</remarks>
*)
  
  function Floor(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Computes the e^xj function from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Expj"/>
</remarks>
*)
  
  function Expj(const X: TOpenCLVector): clVector; overload;

  (*<summary>Computes the e^xj function from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Expj"/>
</remarks>
*)
  
  function Expj(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Return the fractional part of a real number in the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Frac"/>
</remarks>
*)
  
  function Frac(const X: TOpenCLVector): clVector; overload;

  (*<summary>Return the fractional part of a real number in the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Frac"/>
</remarks>
*)
  
  function Frac(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers.  </summary>
             
<remarks>Internally calls <see cref="clVector.Flip"/>
</remarks>
*)
  
  function Flip(const X: TOpenCLVector): clVector; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Flip"/>
</remarks>
*)
  
  function Flip(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers and conjugate.  </summary>
             
<remarks>Internally calls <see cref="clVector.FlipConj"/>
</remarks>
*)
  
  function FlipConj(const X: TOpenCLVector): clVector; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers and conjugate.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.FlipConj"/>
</remarks>
*)
  
  function FlipConj(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Return reminder after divison of X by Y.  </summary>
             
<remarks>Internally calls <see cref="clVector.Rem"/>
</remarks>
*)
  
  function Rem(const X,Y: TOpenCLVector): clVector; overload;

  (*<summary>Return reminder after divison of X by Y.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Rem"/>
</remarks>
*)
  
  function Rem(const X,Y: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute magnitude from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Mag"/>
</remarks>
*)
  
  function Mag(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute magnitude from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Mag"/>
</remarks>
*)
  
  function Mag(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Sqr"/>
</remarks>
*)
  
  function Sqr(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sqr"/>
</remarks>
*)
  
  function Sqr(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.Sqrt"/>
</remarks>
*)
  
  function Sqrt(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sqrt"/>
</remarks>
*)
  
  function Sqrt(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clVector.LogN"/>
</remarks>
*)
  
  function LogN(const X: TOpenCLVector; N: double): clVector; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.LogN"/>
</remarks>
*)
  
  function LogN(const X: TOpenCLMatrix; N: double): clMatrix; overload;

  (*<summary>Multiply the source with 1i complex number.  </summary>
             
<remarks>Internally calls <see cref="clVector.MulI"/>
</remarks>
*)
  
  function MulI(const X: TOpenCLVector): clVector; overload;

  (*<summary>Multiply the source with 1i complex number.  </summary>
             
<remarks>Internally calls <see cref="clMatrix.MulI"/>
</remarks>
*)
  
  function MulI(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute (X + Offset)/Factor  </summary>
             
<remarks>Internally calls <see cref="clVector.Normalize"/>
</remarks>
*)
  
  function Normalize(const X: TOpenCLVector; Offset, Factor: double): clVector; overload;

  (*<summary>Compute (X + Offset)/Factor  </summary>
             
<remarks>Internally calls <see cref="clVector.Normalize"/>
</remarks>
*)
  
  function Normalize(const X: TOpenCLVector; Offset: TCplx; Factor: double): clVector; overload;

  (*<summary>Compute (X + Offset)/Factor  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Normalize"/>
</remarks>
*)
  
  function Normalize(const X: TOpenCLMatrix; Offset, Factor: double): clMatrix; overload;


  (*<summary>Compute (X + Offset)/Factor  </summary>
             
<remarks>Internally calls <see cref="clMatrix.Normalize"/>
</remarks>
*)
  
  function Normalize(const X: TOpenCLMatrix; Offset: TCplx; Factor: double): clMatrix; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clVector.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLVector; const Exponent: double): clVector; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLMatrix; const Exponent: double): clMatrix; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clVector.Power"/>
</remarks>
*)
  
  function Power(const Base: double; const Exponent: TOpenCLVector): clVector; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Power"/>
</remarks>
*)
  
  function Power(const Base: double; const Exponent: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clVector.Power"/>
</remarks>
*)
  
  function Power(const Base: TCplx; const Exponent: TOpenCLVector): clVector; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Power"/>
</remarks>
*)
  
  function Power(const Base: TCplx; const Exponent: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clVector.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLVector; const Exponent: TOpenCLVector): clVector; overload;
  
  function Power(const Base: TOpenCLMatrix; const Exponent: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clVector.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLVector; const Exponent: TCplx): clVector; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLMatrix; const Exponent: TCplx): clMatrix; overload;

  (*<summary>Compute the phase spectrum. </summary>
             
<remarks>Internally calls <see cref="clVector.PhaseSpectrum"/>
</remarks>
*)
  
  function PhaseSpectrum(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the phase spectrum from the complex source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.PhaseSpectrum"/>
</remarks>
*)
  
  function PhaseSpectrum(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the phase from the complex source data. </summary>
             
<remarks>Internally calls <see cref="clVector.PhaseSpectrum"/>
</remarks>
*)
  
  function Phase(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the phase from the complex source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.PhaseSpectrum"/>
</remarks>
*)
  
  function Phase(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the power spectrum from the complex source data. </summary>
             
<remarks>Internally calls <see cref="clVector.PowerSpectrum"/>
</remarks>
*)
  
  function PowerSpectrum(const X: TOpenCLVector): clVector; overload;

  (*<summary>Compute the power spectrum from the complex source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.PowerSpectrum"/>
</remarks>
*)
  
  function PowerSpectrum(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the product of all elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Product"/>
</remarks>
*)
  
  function Product(const X: TOpenCLVector): double; overload;

  (*<summary>Returns the product of all elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Product"/>
</remarks>
*)
  
  function Product(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the product of all elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Productc"/>
</remarks>
*)
  
  function Productc(const X: TOpenCLVector): TCplx; overload;

  (*<summary>Returns the product of all elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Productc"/>
</remarks>
*)
  
  function Productc(const X: TOpenCLMatrix): TCplx; overload;

  (*<summary>Rounds values to the closest integer. </summary>
             
<remarks>Internally calls <see cref="clVector.Round"/>
</remarks>
*)
  
  function Round(const X: TOpenCLVector): clVector; overload;

  (*<summary>Rounds values to the closest integer. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Round"/>
</remarks>
*)
  
  function Round(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clVector.RealPart"/>
</remarks>
*)
  
  function RealPart(const X: TOpenCLVector): clVector; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clMatrix.RealPart"/>
</remarks>
*)
  
  function RealPart(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clVector.RealPart"/>
</remarks>
*)
  
  function Real(const X: TOpenCLVector): clVector; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clMatrix.RealPart"/>
</remarks>
*)
  
  function Real(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the RMS from the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.RMS"/>
</remarks>
*)
  
  function RMS(const X: TOpenCLVector): double; overload;

  (*<summary>Compute the RMS from the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.RMS"/>
</remarks>
*)
  
  function RMS(const X: TOpenCLMatrix): double; overload;

  (*<summary>Change the sign of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Sign"/>
</remarks>
*)
  
  function Sign(const X: TOpenCLVector): clVector; overload;

  (*<summary>Change the sign of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sign"/>
</remarks>
*)
  
  function Sign(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Compute the standard deviation from the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.StdDev"/>
</remarks>
*)
  
  function StdDev(const X: TOpenCLVector): double; overload;

  (*<summary>Compute the standard deviation from the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.StdDev"/>
</remarks>
*)
  
  function StdDev(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the sum of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Sum"/>
</remarks>
*)
  
  function Sum(const X: TOpenCLVector): double; overload;

  (*<summary>Returns the sum of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sum"/>
</remarks>
*)
  
  function Sum(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the sum of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Sumc"/>
</remarks>
*)
  
  function Sumc(const X: TOpenCLVector): TCplx; overload;

  (*<summary>Returns the sum of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Sumc"/>
</remarks>
*)
  
  function Sumc(const X: TOpenCLMatrix): TCplx; overload;

  (*<summary>Rounds values towards to zero to integer. </summary>
             
<remarks>Internally calls <see cref="clVector.Trunc"/>
</remarks>
*)
  
  function Trunc(const X: TOpenCLVector): clVector; overload;

  (*<summary>Rounds values towards zero to integer. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Trunc"/>
</remarks>
*)
  
  function Trunc(const X: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Limit the smallest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.ThreshBottom"/>
</remarks>
*)
  
  function ThreshBottom(const X: TOpenCLVector; Value: double): clVector; overload;

  (*<summary>Limit the smallest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ThreshBottom"/>
</remarks>
*)
  
  function ThreshBottom(const X: TOpenCLMatrix; Value: double): clMatrix; overload;

  (*<summary>Limit the largest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.ThreshTop"/>
</remarks>
*)
  
  function ThreshTop(const X: TOpenCLVector; Value: double): clVector; overload;

  (*<summary>Limit the largest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.ThreshTop"/>
</remarks>
*)
  
  function ThreshTop(const X: TOpenCLMatrix; Value: double): clMatrix; overload;

  (*<summary>Computes X + Y*Ascale </summary>
             
<remarks>Internally calls <see cref="clVector.AddScaled"/>
</remarks>
*)
  
  function AddScaled(const X,Y: TOpenCLVector; aScale: double): clVector; overload;
  (*<summary>Computes X + Y*Ascale </summary>
             
<remarks>Internally calls <see cref="clMatrix.AddScaled"/>
</remarks>
*)
  
  function AddScaled(const X,Y: TOpenCLMatrix; aScale: double): clMatrix; overload;
  (*<summary>Computes X + Y*Ascale </summary>
             
<remarks>Internally calls <see cref="clVector.AddScaled"/>
</remarks>
*)
  
  function AddScaled(const X,Y: TOpenCLVector; aScale: TCplx): clVector; overload;
  (*<summary>Computes X + Y*Ascale </summary>
             
<remarks>Internally calls <see cref="clMatrix.AddScaled"/>
</remarks>
*)
  
  function AddScaled(const X,Y: TOpenCLMatrix; aScale: TCplx): clMatrix; overload;

  (*<summary>Computes X + Y*Z </summary>
             
<remarks>Internally calls <see cref="clVector.AddProduct"/>
</remarks>
*)
  
  function AddProduct(const X, Y, Z: TOpenCLVector): clVector; overload;

  (*<summary>Computes X + Y*Z </summary>
             
<remarks>Internally calls <see cref="clMatrix.AddProduct"/>
</remarks>
*)
  
  function AddProduct(const X, Y, Z: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Conjugate Y and multiply with X </summary>
             
<remarks>Internally calls <see cref="clVector.ConjMul"/>
</remarks>
*)
  
  function ConjMul(const X, Y: TOpenCLVector): clVector; overload;

  (*<summary>Conjugate Y and multiply with X </summary>
             
<remarks>Internally calls <see cref="clMatrix.ConjMul"/>
</remarks>
*)
  
  function ConjMul(const X, Y: TOpenCLMatrix): clMatrix; overload;

  (*<summary>Returns the difference of consecutive elements of the source data.</summary>
             
<remarks>Internally calls <see cref="clVector.Difference"/>
</remarks>
*)
  
  function Difference(const X: TOpenCLVector): clVector; overload;

  (*<summary>Returns the dot product: sum(X*Y).</summary>
             
<remarks>Internally calls <see cref="clVector.DotProd"/>
</remarks>
*)
  
  function DotProd(const X,Y: TOpenCLVector): double; overload;
  (*<summary>Returns the dot product: sum(X*Y).</summary>
             
<remarks>Internally calls <see cref="clVector.DotProdc"/>
</remarks>
*)
  
  function DotProdc(const X,Y: TOpenCLVector; ConjY: boolean = false): TCplx; overload;

  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Max"/>
</remarks>
*)
  
  function Max(const X: TOpenCLVector): double; overload;
  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Max"/>
</remarks>
*)
  
  function Max(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the minimum of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Min"/>
</remarks>
*)
  
  function Min(const X: TOpenCLVector): double; overload;

  (*<summary>Returns the minimum of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Min"/>
</remarks>
*)
  
  function Min(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Maxc"/>
</remarks>
*)
  
  function Maxc(const X: TOpenCLVector): TCplx; overload;

  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Maxc"/>
</remarks>
*)
  
  function Maxc(const X: TOpenCLMatrix): TCplx; overload;

  (*<summary>Returns the minimum of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Minc"/>
</remarks>
*)
  
  function Minc(const X: TOpenCLVector): TCplx; overload;

  (*<summary>Returns the minimum of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Minc"/>
</remarks>
*)
  
  function Minc(const X: TOpenCLMatrix): TCplx; overload;

  (*<summary>Returns the average value of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Mean"/>
</remarks>
*)
  
  function Mean(const X: TOpenCLVector): double; overload;

  (*<summary>Returns the average value of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Mean"/>
</remarks>
*)
  
  function Mean(const X: TOpenCLMatrix): double; overload;

  (*<summary>Returns the average value of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Meanc"/>
</remarks>
*)
  
  function Meanc(const X: TOpenCLVector): TCplx; overload;

  (*<summary>Returns the average value of the source data. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Meanc"/>
</remarks>
*)
  
  function Meanc(const X: TOpenCLMatrix): TCplx; overload;

  (*<summary>Computes the C Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clVector.NormC"/>
</remarks>
*)
  
  function NormC(const X,Y: TOpenCLVector; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the C Norm for the source data X. </summary>
             
<remarks>Internally calls <see cref="clVector.NormC"/>
</remarks>
*)
  
  function NormC(const X: TOpenCLVector): double; overload;

  (*<summary>Computes the C Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormC"/>
</remarks>
*)
  
  function NormC(const X,Y: TOpenCLMatrix; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the C Norm for the source data X. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormC"/>
</remarks>
*)
  
  function NormC(const X: TOpenCLMatrix): double; overload;

  (*<summary>Computes the L1 Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clVector.NormL1"/>
</remarks>
*)
  
  function NormL1(const X,Y: TOpenCLVector; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the L1 Norm for X. </summary>
             
<remarks>Internally calls <see cref="clVector.NormL1"/>
</remarks>
*)
  
  function NormL1(const X: TOpenCLVector): double; overload;

  (*<summary>Computes the L1 Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormL1"/>
</remarks>
*)
  
  function NormL1(const X,Y: TOpenCLMatrix; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the L1 Norm for X. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormL1"/>
</remarks>
*)
  
  function NormL1(const X: TOpenCLMatrix): double; overload;

  (*<summary>Computes the L1 Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clVector.NormL1"/>
</remarks>
*)
  
  function NormL2(const X,Y: TOpenCLVector; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the L2 Norm for X. </summary>
             
<remarks>Internally calls <see cref="clVector.NormL2"/>
</remarks>
*)
  
  function NormL2(const X: TOpenCLVector): double; overload;

  (*<summary>Computes the L1 Norm between X and Y. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormL1"/>
</remarks>
*)
  
  function NormL2(const X,Y: TOpenCLMatrix; RelativeError: boolean = False): double; overload;

  (*<summary>Computes the L2 Norm for X. </summary>
             
<remarks>Internally calls <see cref="clMatrix.NormL2"/>
</remarks>
*)
  
  function NormL2(const X: TOpenCLMatrix): double; overload;

  (*<summary>Reverses the content of the source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Reverse"/>
</remarks>
*)
  
  function Reverse(const X: TOpenCLVector): clVector; overload;

  (*<summary>Rotates the content of the source data by Offset. </summary>
             
<remarks>Internally calls <see cref="clVector.Rotate"/>
</remarks>
*)
  
  function Rotate(const X: TOpenCLVector; Offset: integer): clVector; overload;

  (*<summary>Shifts the content of the source data X by Offset left or right. </summary>
             
<remarks>Internally calls <see cref="clVector.Shift"/>
</remarks>
*)
  
  function Shift(const X: TOpenCLVector; Offset: integer): clVector; overload;

  (*<summary>Returns the sum of squares of the source data. </summary>*)
  
  function SumOfSquares(const X: TOpenCLVector): double; overload;

  (*<summary>Returns the sum of squares of the source data. </summary>*)
  
  function SumOfSquares(const X: TOpenCLMatrix): double; overload;


  (*<summary>Reduces the sampling frequency without filtering.</summary>
             
<remarks>Internally calls <see cref="clVector.DownSample"/>
</remarks>
*)
  
  function DownSample(const Src: TOpenCLVector; Factor: integer; Phase: integer = 0): clVector; overload;

  (*<summary>Reduces the sampling frequency without filtering.</summary>
             
<remarks>Internally calls <see cref="clVector.DownSample"/>
</remarks>
*)
  
  function DownSample(const Src: TOpenCLMatrix; Factor: integer; Phase: integer = 0): clVector; overload;

  (*<summary>Returns a clVector of [0, 1, .., Len-1]  numbers. </summary>
             
<remarks>Internally calls <see cref="clVector.Ramp"/>
</remarks>
*)
  
  function Ramp(Len: integer; aPrecision: TclFloatPrecision =  clFloat): clVector; overload;

  (*<summary>Returns a clVector of [Offset, Offset+Step, .., Offset+(Len-1)*Step] numbers. </summary>
             
<remarks>Internally calls <see cref="clVector.Ramp"/>
</remarks>
*)
  
  function Ramp(Len: integer; Offset, Step: TCplx; aPrecision: TclFloatPrecision =  clFloat): clVector; overload;

  (*<summary>Returns a clVector of [Offset, Offset+Step, .., Offset+(Len-1)*Step] numbers. </summary>
             
<remarks>Internally calls <see cref="clVector.Ramp"/>
</remarks>
*)
  
  function Ramp(Len: integer; Offset, Step: double; aPrecision: TclFloatPrecision =  clFloat): clVector; overload;

  (*<summary>Returns a clVector of [Offset, Offset+Step, .., Offset+(Len-1)*Step] numbers. </summary>
             
<remarks>Internally calls <see cref="clVector.Ramp"/>
</remarks>
*)
  
  function Ramp(Len: integer; Offset: TCplx; Step: double; aPrecision: TclFloatPrecision =  clFloat): clVector; overload;


  (*<summary>Increase sampling frequency without filtering. </summary>
             
<remarks>Internally calls <see cref="clVector.UpSample"/>
</remarks>
*)
  
  function UpSample(const Src: TOpenCLMatrix; Factor: integer; Phase: integer = 0): clVector; overload;

  (*<summary>Increase sampling frequency without filtering. </summary>
             
<remarks>Internally calls <see cref="clVector.UpSample"/>
</remarks>
*)
  
  function UpSample(const Src: TOpenCLVector; Factor: integer; Phase: integer = 0): clVector; overload;

  (*<summary>Computes the Norm from source data. </summary>
             
<remarks>Internally calls <see cref="clVector.Norm"/>
</remarks>
*)
  
  function Norm(const Vec: TOpenCLVector): clVector;




































































































































































































































































































































































































































































































































































































































































































































































  (*<summary>Compute the sin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Sin"/>
</remarks>
*)
  
  function Sin(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the cos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Cos"/>
</remarks>
*)
  
  function Cos(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Tan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Tan"/>
</remarks>
*)
  
  function Tan(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Cot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Cot"/>
</remarks>
*)
  
  function Cot(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Sec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Sec"/>
</remarks>
*)
  
  function Sec(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Csc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Csc"/>
</remarks>
*)
  
  function Csc(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcSin function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcSin"/>
</remarks>
*)
  
  function ArcSin(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCos function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCos"/>
</remarks>
*)
  
  function ArcCos(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcTan function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcTan"/>
</remarks>
*)
  
  function ArcTan(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcTan2 function from X and Y and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcTan2"/>
</remarks>
*)
  
  function ArcTan2(Y,X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCot function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCot"/>
</remarks>
*)
  
  function ArcCot(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcSec function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcSec"/>
</remarks>
*)
  
  function ArcSec(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCsc function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCsc"/>
</remarks>
*)
  
  function ArcCsc(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Sinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Sinh"/>
</remarks>
*)
  
  function Sinh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Cosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Cosh"/>
</remarks>
*)
  
  function Cosh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Tanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Tanh"/>
</remarks>
*)
  
  function Tanh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Coth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Coth"/>
</remarks>
*)
  
  function Coth(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Sech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Sech"/>
</remarks>
*)
  
  function Sech(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Csch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Csch"/>
</remarks>
*)
  
  function Csch(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcSinh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcSinh"/>
</remarks>
*)
  
  function ArcSinh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCosh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCosh"/>
</remarks>
*)
  
  function ArcCosh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcTanh function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcTanh"/>
</remarks>
*)
  
  function ArcTanh(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCoth function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCoth"/>
</remarks>
*)
  
  function ArcCoth(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcSech function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcSech"/>
</remarks>
*)
  
  function ArcSech(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the ArcCsch function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.ArcCsch"/>
</remarks>
*)
  
  function ArcCsch(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Abs function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Abs"/>
</remarks>
*)
  
  function Abs(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Cbrt function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Cbrt"/>
</remarks>
*)
  
  function Cbrt(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Ceil function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Ceil"/>
</remarks>
*)
  
  function Ceil(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Ln function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Ln"/>
</remarks>
*)
  
  function Ln(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Log10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Log10"/>
</remarks>
*)
  
  function Log10(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Log2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Log2"/>
</remarks>
*)
  
  function Log2(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Exp function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Exp"/>
</remarks>
*)
  
  function Exp(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Exp2 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Exp2"/>
</remarks>
*)
  
  function Exp2(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the Exp10 function from the source and return the result. </summary>
             
<remarks>Internally calls <see cref="clValue.Exp10"/>
</remarks>
*)
  
  function Exp10(const X: TOpenCLValue): clValue; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clValue.ImagPart"/>
</remarks>
*)
  
  function ImagPart(const X: TOpenCLValue): clValue; overload;

  (*<summary>Returns the imaginary part of the source. </summary>
             
<remarks>Internally calls <see cref="clValue.ImagPart"/>
</remarks>
*)

  function Imag(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the integer power function. </summary>
             
<remarks>Internally calls <see cref="clValue.IntPower"/>
</remarks>
*)
  
  function IntPower(const Base: TOpenCLValue; Exponent: integer): clValue; overload;

  (*<summary>Compute 1/X. </summary>
             
<remarks>Internally calls <see cref="clValue.Inv"/>
</remarks>
*)
  
  function Inv(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clValue.InvSqrt"/>
</remarks>
*)
  
  function InvSqrt(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute (1/X)^2. </summary>
             
<remarks>Internally calls <see cref="clValue.InvCbrt"/>
</remarks>
*)
  
  function InvCbrt(const X: TOpenCLValue): clValue; overload;

  (*<summary>Return complex conjugate of the source. </summary>
             
<remarks>Internally calls <see cref="clMatrix.Conj"/>
</remarks>
*)
  
  function Conj(const X: TOpenCLValue): clValue; overload;

  (*<summary>Make the angle fit within +/- PI.  </summary>
             
<remarks>Internally calls <see cref="clValue.FixAngle"/>
</remarks>
*)
  
  function FixAngle(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the floor function from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Floor"/>
</remarks>
*)
  
  function Floor(const X: TOpenCLValue): clValue; overload;

  (*<summary>Computes the e^xj function from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Expj"/>
</remarks>
*)
  
  function Expj(const X: TOpenCLValue): clValue; overload;

  (*<summary>Return the fractional part of a real number in the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Frac"/>
</remarks>
*)
  
  function Frac(const X: TOpenCLValue): clValue; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers.  </summary>
             
<remarks>Internally calls <see cref="clValue.Flip"/>
</remarks>
*)
  
  function Flip(const X: TOpenCLValue): clValue; overload;

  (*<summary>Exchange the real and imaginary parts of complex numbers and conjugate.  </summary>
             
<remarks>Internally calls <see cref="clValue.FlipConj"/>
</remarks>
*)
  
  function FlipConj(const X: TOpenCLValue): clValue; overload;

  (*<summary>Return reminder after divison of X by Y.  </summary>
             
<remarks>Internally calls <see cref="clValue.Rem"/>
</remarks>
*)
  
  function Rem(const X,Y: TOpenCLValue): clValue; overload;

  (*<summary>Compute magnitude from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Mag"/>
</remarks>
*)
  
  function Mag(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Sqr"/>
</remarks>
*)
  
  function Sqr(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.Sqrt"/>
</remarks>
*)
  
  function Sqrt(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute X^2 from the source.  </summary>
             
<remarks>Internally calls <see cref="clValue.LogN"/>
</remarks>
*)
  
  function LogN(const X: TOpenCLValue; N: double): clValue; overload;

  (*<summary>Multiply the source with 1i complex number.  </summary>
             
<remarks>Internally calls <see cref="clValue.MulI"/>
</remarks>
*)
  
  function MulI(const X: TOpenCLValue): clValue; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clValue.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLValue; const Exponent: double): clValue; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clValue.Power"/>
</remarks>
*)
  
  function Power(const Base: double; const Exponent: TOpenCLValue): clValue; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clValue.Power"/>
</remarks>
*)
  
  function Power(const Base: TCplx; const Exponent: TOpenCLValue): clValue; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clValue.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLValue; const Exponent: TOpenCLValue): clValue; overload;

  (*<summary>Compute the power function. </summary>
             
<remarks>Internally calls <see cref="clValue.Power"/>
</remarks>
*)
  
  function Power(const Base: TOpenCLValue; const Exponent: TCplx): clValue; overload;

  (*<summary>Rounds values to the closest integer. </summary>
             
<remarks>Internally calls <see cref="clValue.Round"/>
</remarks>
*)
  
  function Round(const X: TOpenCLValue): clValue; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clValue.RealPart"/>
</remarks>
*)
  
  function RealPart(const X: TOpenCLValue): clValue; overload;

  (*<summary>Returns the real part of the complex number. </summary>
             
<remarks>Internally calls <see cref="clValue.RealPart"/>
</remarks>
*)
  
  function Real(const X: TOpenCLValue): clValue; overload;

  (*<summary>Change the sign of the source data. </summary>
             
<remarks>Internally calls <see cref="clValue.Mul"/>
</remarks>
*)
  
  function Sign(const X: TOpenCLValue): clValue; overload;

  (*<summary>Rounds values towards to zero to integer. </summary>
             
<remarks>Internally calls <see cref="clValue.Trunc"/>
</remarks>
*)
  
  function Trunc(const X: TOpenCLValue): clValue; overload;

  (*<summary>Limit the smallest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clValue.ThreshBottom"/>
</remarks>
*)
  
  function ThreshBottom(const X: TOpenCLValue; Value: double): clValue; overload;


  (*<summary>Limit the largest value in the source data. </summary>
             
<remarks>Internally calls <see cref="clValue.ThreshTop"/>
</remarks>
*)
  
  function ThreshTop(const X: TOpenCLValue; Value: double): clValue; overload;





  


  
  function Sgn(const Src: TOpenCLVector): clVector; overload;

  
  function Sgn(const Src: TOpenCLMatrix): clMatrix; overload;



