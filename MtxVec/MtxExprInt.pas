










(*<summary>Adds operator overloading support.</summary>
  
<remarks>The unit declares Vector and Matrix types for +, -, *, / operator support.
</remarks>
*)
unit MtxExprInt;


interface

{$I bdsppdefs.inc}

uses MtxVec, Math387, AbstractMtxVec, MtxVecInt, AbstractMtxVecInt, MtxVecBase
     
     
      ,Classes
      ,Types
     
     
     ;


    {$HPPEMIT END '#include "MtxExprInt.h"'}
{$HPPEMIT '#include <memory>'}
{$HPPEMIT '#include <string>'}




type

  (*<summary>Integer vector class for operator overloading (Delphi 2006 and later). </summary>
    
<remarks>Declare VectorInt instead of TVecInt to take advantage ofoperator overloading. Be carefull to declare
    VectorInt only for local variables with short lifetime.
    Call the Create method for VectorInt, if the variable is a global variable or a variable with a longer life.
    It also makes sense to continue to use TVecInt for global vars.
    If the Create method (constuctor) is not called, the VectorInt
    record obtains TVecInt object from object cache (fastest create).
    If the Create mehod is called, the TVecInt object is created in
    the usual way (slower). Object cache has limited size.
</remarks>


    <Example>
    <code>
    var b, c: VectorInt;
        bv: TVecInt;
    begin
          b := VectorInt(TIntegerArray.Create(1,1,1,1)); //b = [1,1,1,1];
          bv := b;   //b and bv point to same data object

          bv.Multiply(2);
          bv.Bits[76] := true;  //set 76th bit in the array to 1
          TVecInt(b).Multiply(2);  //an alternative way to scale
          c := b*2 + 3; //c := b*2 + 2 + 3i

          c := b*bv; //mix VectorInt and TVecInt

          bv.Add(c); //pass VectorInt type and implicitely convert to TVecInt pointer
          b.PackBits(c); // store bit 1 in b for all elements in c which are different from zero. Bit 0 otherwise.
    end;
    </code>
    </Example>*)

  
  VectorInt = record 
  strict private
    
    FData: TVecInt;
    
    
  private
    function get_Data: TVecInt ;
    property Data: TVecInt read get_Data;
  
  private
  
   

    function get_BlockEnd: boolean;
    function get_IsSubRange: boolean;
    function get_Caption: string;
    function get_ConditionCheck: boolean;
    function get_Length: integer;
    function get_Tag: integer;
    procedure set_Caption(const value: string);
    procedure set_ConditionCheck(const value: boolean);
    procedure set_Length(const value: integer);
    procedure set_Tag(const value: integer);
    procedure set_ScaleFactor(const value: integer);
    function get_ScaleFactor: integer;
    procedure set_IntPrecision(const value: TIntPrecision);
    function get_IntPrecision: TIntPrecision;
    function get_IntegerValues: string;
    
    function get_IValues(const Indx: integer): integer; 
    procedure set_IValues(const Indx: integer; const Value: integer); 

    function get_SValues(const Indx: integer): SmallInt;  
    procedure set_SValues(const Indx: integer; const Value: SmallInt);

    function get_BValues(const Indx: integer): Byte;  
    procedure set_BValues(const Indx: integer; const Value: Byte);

    function GetSelect(const Indx, Len: integer): VectorInt; 
    procedure SetSelect(const Indx, Len: integer; const Value: VectorInt); 

    function GetSelectIndex(const startIndex, Step, stopIndex: integer): VectorInt;
    procedure SetSelectIndex(const startIndex, Step, stopIndex: integer;  const Value: VectorInt);
    
    function GetBits(const Indx: integer): boolean; 
    procedure SetBits(const Indx: integer; const Value: boolean); 

    function get_BitCount: Int64;
    procedure set_BitCount(const value: Int64);

    function get_Capacity: Int64;   
    procedure set_Capacity(const value: Int64); 

    function get_CapacityStep: double; 
    procedure set_CapacityStep(const value: double); 

    function get_First: integer;
    function get_Last: integer;

   
  public
    
     



    

        

    (*<summary>Adopts TVecInt object.</summary>
      
<remarks>Src object will be adopted by VectorInt. When the Vector gets out of scope, the object
      will be freed.
</remarks>
*)
    procedure Adopt(const Src: TVecInt);

    (*<summary>Sets the following properties</summary>

      
<remarks><code>
      Vec.Length := ALength;
      </code>

      and preserves IntPrecision value. Default IntPrecision when
      object is created is prInt32.
</remarks>
*)
    function Size(const aLength: integer): TVecInt; overload;  


    (*<summary>Sets the following properties</summary>

      
<remarks><code>
      Vec.Precision := aPrecision;
      Vec.Length := ALength;
      </code>
</remarks>
*)
    
    procedure Size(const aLength: integer; const aPrecision: TIntPrecision); overload;

    (*<summary>Returns true, if Left and Right are of equal Length and Values.</summary>

      
<remarks><code>
        var a,b: VectorInt;
            c: TDoubleArray;
        begin
            c := TDoubleArray.Create(1,1,1,1,1);
            TVecInt(a).CopyFromArray(c);
            TVecInt(a).Copy(b);
            if a = b then ERaise('a and b are equal!');

            if a = 1 then ERaise('a equals 1!');   //compare to real value

            if a = Cplx(1,0) then ERaise('a equals 1!'); //compare to complex value
        end;
        </code>
</remarks>


     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    class operator Equal(const Left, Right: VectorInt): Boolean;
    (*<summary>Returns true, if all elements of Left are equal to Right.</summary>*)
    class operator Equal(const Left: VectorInt; const Right: integer): Boolean;
    (*<summary>Returns true, if all elements of Right are equal to Left.</summary>*)
    class operator Equal(const Left: integer;const Right: VectorInt): Boolean;
    (*<summary>Returns true, if Left and Right are of equal Length and Values.</summary>*)
    class operator Equal(const Left: TMtxVecInt; const Right: VectorInt): Boolean;
    (*<summary>Returns true, if Left and Right are of equal Length and Values.</summary>*)
    class operator Equal(const Left: VectorInt; const Right: TMtxVecInt): Boolean;

    (*<summary>Returns true, if Left and Right are not equal.</summary>
       
<remarks>Returns true, if Left and Right do not have equal Length and Values.

        <code>

        var a,b: VectorInt;
        begin
            a := VectorInt(TIntegerArray.Create(1,1,1,1,1));
            if a &lt;&gt; b then ERaise('a and b are not equal!');

            if a &lt;&gt; 1 then ERaise('a does not equals 1!');   //compare to integer value
        end;
        </code>
</remarks>


     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    class operator NotEqual(const Left, Right: VectorInt): Boolean;
    (*<summary>Returns true, if any elements of Left are not equal to Right.</summary>*)
    class operator NotEqual(const Left: VectorInt; const Right: integer): Boolean;
    (*<summary>Returns true, if any elements of Right are not equal to Left.</summary>*)
    class operator NotEqual(const Left: integer;const Right: VectorInt): Boolean;
    (*<summary>Returns true, if Left and Right do not have equal Length and Values.</summary>*)
    class operator NotEqual(const Left: TMtxVecInt; const Right: VectorInt): Boolean;
    (*<summary>Returns true, if Left and Right do not have equal Length and Values.</summary>*)
    class operator NotEqual(const Left: VectorInt; const Right: TMtxVecInt): Boolean;


    (*<summary>Returns true, if all elements of ALeft are smaller from ARight.</summary>*)
    class operator LessThan(const ALeft: VectorInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is smaller from all elements in ARight.</summary>*)
    class operator LessThan(const ALeft: integer; const ARight: VectorInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater than
      coresponding elements in ARight.</summary>*)
    class operator LessThan(const ALeft: VectorInt; const ARight: VectorInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are smaller or equal from ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: VectorInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is smaller or equal from all elements in ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: integer; const ARight: VectorInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are smaller than or equal from
      coresponding elements in ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: VectorInt; const ARight: VectorInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are greater or equal from ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: VectorInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is greater or equal from all elements in ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: integer; const ARight: VectorInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater or equal from
      coresponding elements in ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: VectorInt; const ARight: VectorInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are greater from ARight.</summary>*)
    class operator GreaterThan(const ALeft: VectorInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is greater from all elements in ARight.</summary>*)
    class operator GreaterThan(const ALeft: integer; const ARight: VectorInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater than
      coresponding elements in ARight.</summary>*)
    class operator GreaterThan(const ALeft: VectorInt; const ARight: VectorInt): Boolean;

    (*<summary>Add Left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: integer;const Right: VectorInt): VectorInt;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: VectorInt; const Right: integer): VectorInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: VectorInt;const Right: VectorInt): VectorInt;

    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: integer; const Right: VectorInt): VectorInt;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: VectorInt; const Right: integer): VectorInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: VectorInt;const Right: VectorInt): VectorInt;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: VectorInt; const Right: integer): VectorInt;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(const Left: integer; const Right: VectorInt): VectorInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: VectorInt; const Right: VectorInt): VectorInt;

    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: VectorInt; const Right: integer): VectorInt;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: integer; const Right: VectorInt): VectorInt;

    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: VectorInt; const  Right: VectorInt): VectorInt;

    (*<summary>Negates all values inside AValue.</summary>*)
    class operator Negative(const AValue: VectorInt): VectorInt;


    


    (*<summary>Explicitely convert array of complex values to VectorInt.</summary>
     
<remarks>Copies all values from the complex array to the result with precision
     set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TCplxArray): VectorInt;
    (*<summary>Explicitely convert array of double values to VectorInt.</summary>
      
<remarks>Copies all values from the double array to the result with
      precision set at 32bit signed integer;
</remarks>
*)
    class operator Explicit(const AValue: TDoubleArray): VectorInt;
    (*<summary>Explicitely convert array of single values to VectorInt.</summary>
      
<remarks>Copies all values from the AValue array to the result with precision
      set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TSingleArray): VectorInt;
    (*<summary>Explicitely convert TMtxVec to VectorInt.</summary>
       
<remarks>Copies all values from AValue to the result with precision
       set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TMtxVec): VectorInt;

    (*<summary>Explicitely convert VectorInt to array of doubles.</summary>
      
<remarks>Copies all values from AVector to an array of double.
</remarks>
*)
    class operator Explicit(const AValue: VectorInt): TDoubleArray;
    (*<summary>Explicitely convert VectorInt to array of doubles.</summary>
      
<remarks>Copies all values from AVector to an array of single.
</remarks>
*)
    class operator Explicit(const AValue: VectorInt): TSingleArray;

    (*<summary>Explicitely convert VectorInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 32bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: VectorInt): TIntegerArray;

    (*<summary>Explicitely convert VectorInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 16bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: VectorInt): TSmallIntArray;

    (*<summary>Explicitely convert VectorInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 16bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: VectorInt): Math387.TByteArray;

    


    
    


    

    (*<summary>Implicit type conversion from VectorInt to an array of 32bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TIntegerArray;
    (*<summary>Implicit type conversion from VectorInt to an array of 32bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if VectorInt.IsSubRange = True.
      PInteger type is also defined in Windows.pas unit and sometimes the type needs to be specified with the unit name (Math387.PInteger) for this
      overload to work.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): Math387.PInteger;

    (*<summary>Implicit type conversion from VectorInt to an array of 16bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TSmallIntArray;
    (*<summary>Implicit type conversion from VectorInt to an array of 16bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if VectorInt.IsSubRange = True.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): Math387.PSmallInt;

    (*<summary>Implicit type conversion from VectorInt to an array of 8bit unsigned integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): Math387.TByteArray;
    (*<summary>Implicit type conversion from VectorInt to an array of 8bit unsigned integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if VectorInt.IsSubRange = True.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): Math387.PByte;

    

    (*<summary>Implicit type conversion from VectorInt to TVecInt.</summary>
      
<remarks>Returns a pointer to the internal TVecInt object.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TVecInt;

    (*<summary>Implicit type conversion from VectorInt to TMtxVecInt.</summary>
      
<remarks>Returns a pointer to the internal TVecInt object.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TMtxVecInt;

    (*<summary>Implicit type conversion from VectorInt to TMtxVecBase.</summary>
      
<remarks>Returns a pointer to the internal TVecInt object.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TMtxVecBase;

    
    (*<summary>Implicit type conversion from VectorInt to TMtxVecBase.</summary>
      
<remarks>Returns a pointer to the internal TVecInt object.
</remarks>
*)
    class operator Implicit(const AValue: VectorInt): TObject;
    


    
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: VectorInt; const Right: VectorInt): VectorInt;
    

    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: VectorInt; const Right: VectorInt): VectorInt;

    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: VectorInt; const Right: VectorInt): VectorInt;

    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: VectorInt; const Right: VectorInt): VectorInt;

    (*<summary>Performs bitwise left shift of aValue.</summary>*)
    class operator LeftShift(const aValue: VectorInt; Bits: integer): VectorInt;
    (*<summary>Performs bitwise right shift of aValue.</summary>
               
<remarks>The shift preserves the sign of the numbers.
</remarks>
*)
    class operator RightShift(const aValue: VectorInt; BitCount: integer): VectorInt;

    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: TVecInt; const Right: VectorInt): VectorInt;
    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: VectorInt; const Right: TVecInt): VectorInt;
    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: VectorInt; const Right: VectorInt): VectorInt;

    (*<summary>Performs logical (and bitwise) "not" on the Value.</summary>*)
    class operator LogicalNot(const Value: VectorInt): VectorInt;

    

    procedure CreateFromCache(Value: boolean);

    (*<summary>Constructor of the record.</summary>
      
<remarks>Returns a Vector with internal TVecInt object created explicitely with Length property set to
      aLength and IntPrecision property set to int32. Call this constructor, if the Vector type
      variable is a global variable with a long life span.
</remarks>


      <Example>
       <code>
        var a,b: VectorInt;
            bvec: TVecInt;
        begin
            a := VectorInt.Create(true); //create from object cache via CreateIt

            //this constructor can be omitted, because it is implicitly called
            //the first time that the variable is used.
            //However:

            b := VectorInt.Create(10);

            //Similar to

            bvec := TVecInt.Create;
            try
                bvec.Size(10);
            finally
                bvec.Free;  //b is freed on the exit from the procedure
            end;
        end;
      </code>
      </Example>

     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    constructor Create(aLength: integer); overload;

    (*<summary>Internally creates TVecInt object without using object cache.</summary>
                
<remarks>Creates TVecInt object without using object cache. Suitable for declaring global variables.
</remarks>
*)
    constructor Create(aLength: integer; aPrecision: TIntPrecision); overload;
    (*<summary>Uses TVecInt object as internal storage.</summary>
                
<remarks>The resulting Vector will own Src object of TVecInt type and will release it once Vector gets out of scope.
</remarks>
*)
    constructor Create(const Src: TVecInt); overload;

    (*<summary>Constructor of the record.</summary>
      
<remarks>Returns a Vector with internal TVecInt object created from  object cache (CreateIt),
      if FromObjectCache is True. Pass false to the constructor or do not call it at all,
      if the variable is a local variable. Object cache has limited size.
</remarks>
*)
    constructor Create(FromObjectCache: boolean); overload;


  public

    (*<summary>Defines the length in number of samples.</summary>
      
<remarks>Defines the number of samples that the Vector can store.
      The length property does not map directly to memory reallocation
      when it is changed. Until the amount of preallocated memory
      is not exceed, there is no reallocation going on.

      Changing the Length property will preserve the existing
      values, but only if the new property value is smaller than the
      amount of preallocated memory. To properly resize the memory allocated
      use the <see cref="Resize"/> method.

      It is recommended that vector sizes do not exceed the size of
      preallocated memory to increase overall performance. The size
      of preallocated memory can be controlled via the Controller global
      variable which is defined in the MtxVec unit.
</remarks>
*)
    property Length: integer read get_Length write set_Length;
    (*<summary>Enables/disable inline condition checking.</summary>
      
<remarks>Enables/disables inline condition checking. When true, TVecInt methods perform additional (range)
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
    property ConditionCheck: boolean read get_ConditionCheck write set_ConditionCheck;
    (*<summary>Vector Caption.</summary>
      
<remarks>Use this property to set/get the string caption for the VectorInt.
      This can be usefull for associating description with data when making
      a user interface.
</remarks>
*)
    property Caption: string read get_Caption write set_Caption;
    (*<summary>Stores an integer value as a part of the object.</summary>
      
<remarks>Tag has no predefined meaning.
      The Tag property is provided for the convenience of developers.
      It can be used for storing an additional
      integer value or it can be typecast to any 32-bit value such as a
      component reference or even a pointer.
</remarks>
*)
    property Tag: integer read get_Tag write set_Tag;
    (*<summary>Sizing the object will not allocate less than Capacity samples.</summary>
               
<remarks>To free memory allocated before, first set Capacity to 0.
</remarks>
*)
    property Capacity: Int64 read get_Capacity write set_Capacity;

    (*<summary>Specifies increment step for the Capacity property.</summary>
               
<remarks>If this property is 0, the Capacity will never be modified on its own.
               When the value 1, the capacity will continue increasing to match largest value
               requested. When the value is more than 1, the capacity will be increasing with
               the factor specified.
</remarks>
*)
    property CapacityStep: double read get_CapacityStep write set_CapacityStep;

  public
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @IValues[i]. Under .NET this is a pointer to pinned managed memory.
</remarks>


     <SeeAlso cref="PSValues"/>
     <SeeAlso cref="PBValues"/>*)
    function PIValues(const Index: integer): PAPointer;
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @SValues[i]. Under .NET this is a pointer
      to unmanaged memory or pinned managed memory.
</remarks>


     <SeeAlso cref="PIValues"/>
     <SeeAlso cref="PBValues"/>*)
    function PSValues(const Index: integer): PAPointer;
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @BValues[i]. Under .NET this is a pointer
      to unmanaged memory or pinned managed memory.
</remarks>


     <SeeAlso cref="PIValues"/>
     <SeeAlso cref="PSValues"/>*)
    function PBValues(const Index: integer): PAPointer;

    (*<summary> Defines internal storage precision which can be either 32, 16 or 8 bit.</summary>*)
    property IntPrecision: TIntPrecision read get_IntPrecision write set_IntPrecision;

    (*<summary>Returns true, if the currrent subrange of the vector was also the last subrange in the VectorInt.</summary>
      
<remarks>Returns true, if the currrent subrange of the vector was also the last subrange in the VectorInt.
      This property be used together with <See Method="BlockNext"/> and <See Method="BlockInit"/>.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockNext"/>*)
    property BlockEnd: boolean read Get_BlockEnd;
    (*<summary>Set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TVecInt method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector
      length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read get_IsSubRange;

    

    (*<summary>Allows setting/getting the signed 32bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
     This property reads/writes to the same memory as <see cref="SValues"/> and <see cref="BValues"/> properties.
</remarks>
*)
    property IValues[const Indx: integer]: integer read get_IValues write set_IValues; default;
    (*<summary>Allows setting/getting the signed 16bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the signed 16bit integer value at position Indx.
      This property reads/writes to the same memory as <see cref="BValues"/> and <see cref="IValues"/> properties.
</remarks>
*)
    property SValues[const Indx: integer]: smallint read get_SValues write set_SValues;
    (*<summary>Allows setting/getting the 8bit unsigned integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 8bit unsigned integer value at position Indx.
      This property reads/writes to the same memory as <see cref="IValues"/> and <see cref="SValues"/> properties.
</remarks>
*)
    property BValues[const Indx: integer]: byte read get_BValues write set_BValues;

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will return a sub-ranged Vector. Similar to calling b.SetSubRange(a, Indx, Len);
      Writing this property will copy source to the specified subrange. The size of the value being assigned
      needs to match the range specified.
</remarks>
*)
    property Select[const Indx, Len: integer]: VectorInt read GetSelect write SetSelect;

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will return a copied selection of subranged vector by calling Dst.GatherByIncr(Src, startIndex, Step);
      Writing this property will copy source to the specified range by using Data.ScatterByIncr.
      The size of the value being assigned needs to match the index range specified.
      This property does not obtain a "view" unless Step equals to 1.
</remarks>
*)
    property SelectIndex[const startIndex, Step, stopIndex: integer]: VectorInt read GetSelectIndex write SetSelectIndex;

    

    
    procedure AssignWithoutCopy(const Src: VectorInt);
    procedure Assign(const Src: VectorInt);
    

    (*<summary>Initializes block processing.</summary>
      
<remarks>Initializes block processing. Because the size of the CPU cache is limited, significant performance gains can be obtained by
      splitting long vectors in to a series of short ones, which can all fit in the CPU cache entirely. The BlockInit method is
      to be used together with <see cref="BlockNext"/> and <See Method="BlockEnd"/> methods to initialize a block processing
      while loop. BlockInit will call <See Method="SetSubRange"/> to obtain subrange of the data in TVecInt. The <see cref="Length"/>
      of the subranged vector is determined by the global  <see cref="Math387.MtxVecBlockSize"/> variable
      declared in <See Unit="Math387"/> unit. Default value of MtxVecBlockSize is preset to 800 vector elements for double precision
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

      The differences with block processing will be more noticable on older CPU's without
      support for SSE2/SSE3/SSE4/AVX when using FPU instructions.
</remarks>


      <SeeAlso cref="BlockNext"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockInit; overload;
    (*<summary>Initializes block processing with specified ABLockSize.</summary>*)
    procedure BlockInit(ABlockSize: integer); overload;

    (*<summary>Initializes block processing.</summary>*)
    procedure BlockInit(const Src: TMtxVecInt); overload;

    (*<summary>Initializes block processing.</summary>
               
<remarks>Block processing can be applied on possibly already subranged Src object.
               Src may not be freed or go out of scope until block processing loop has finished. There would
               be no error raised other than AV.
</remarks>
*)
    procedure BlockInit(const Src: TMtxVecInt; ABlockSize: integer); overload;

    (*<summary>Obtains the next subrange of the data.</summary>
      
<remarks>Obtains the next subrange of the data in TVecInt. The routine must be used together with
      <see cref="BlockEnd"/> and <see cref="BlockInit"/> methods.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockNext;

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

    (*<summary>Returns number of values within the interval.</summary>
               
<remarks>Returns number of values which fullfill condition LowValue &lt; Value[i] &lt; HighValue.
               The function only supports prInt32 precision.
</remarks>
*)

    function CountInRange(const LowValue, HighValue: integer): integer; overload;
    (*<summary>Returns number of values within the interval.</summary>
               
<remarks>Returns number of values which fullfill condition LowValue &lt; Value[i] &lt; HighValue,
               for i traversing the range [Index]..[Index+Len-1]

               The function only supports prInt32 precision.
</remarks>
*)
    function CountInRange(const LowValue, HighValue: integer; const Index: integer; Len: integer): integer; overload;


    (*<summary>Prevents calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Prevents calls to <see cref="SetSubRange"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
</remarks>


      <Example>

      <code>
      var a,b: VectorInt;
      begin
          a.SetIt([1,2,3,4,5,6,7,8,9]);
          a.SetSubRange(0,2);  //a = [1,2]
          a.DisableSubrange;
          a.SetSubRange(2,2); //exception raised here

          b.SetSubRange(a,2,2); //but this will work
          a.SetFullRange; //b is not changed, it still points to [4,5]
      end;

    </code>
    </Example>


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


      <Example>
      <code>
      var a,b: VectorInt;
      begin
          a.SetIt(false, [1,2,3,4,5,6,7,8,9]);
          a.Select(0,2);  //a = [1,2]
          a.DisableSelect;
          a.Select(2,2); //exception raised here

          b.Select(a,2,2); //but this will work
          a.SelectAll; //b is not changed, it still points to [4,5]
      end;
      </code>
      </Example>

      <SeeAlso cref="DisableSubrange"/>
      <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSelect;

    (*<summary>Enables calls to <see cref="Select"/>.</summary>
      
<remarks>Enables calls to <see cref="Select"/> by removing
      the block set by <see cref="DisableSelect"/>.
</remarks>
*)
    procedure EnableSelect;

   (*<summary>First element in object Values array.</summary>
      <returns>first integer element in object Values array.</returns>

      <SeeAlso cref="Last"/>*)
    property First: integer read get_First;

    (*<summary>Last element in object Values array.</summary>
      <returns>the last integer element in calling object Values array.</returns>

      <SeeAlso cref="First"/>*)
    property Last: integer read get_Last;

    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: TCplxArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: TDoubleArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: TSingleArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: TIntegerArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: TSmallIntArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the size (Length) of the caling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: Math387.TByteArray); overload;

    (*<summary>Sizes complex array.</summary>*)
    procedure SizeToArray(var Dst: TCplxArray); overload; 
    (*<summary>Sizes double precision array.</summary>*)
    procedure SizeToArray(var Dst: TDoubleArray); overload; 
    (*<summary>Size single precision array.</summary>*)
    procedure SizeToArray(var Dst: TSingleArray); overload; 
    (*<summary>Sizes 4 byte signed integer array.</summary>*)
    procedure SizeToArray(var Dst: TIntegerArray); overload; 

    (*<summary>Sizes 2 byte unsigned integer array.</summary>*)
    procedure SizeToArray(var Dst: Math387.TWordArray); overload; 
    (*<summary>Sizes 2 byte signed integer array.</summary>*)
    procedure SizeToArray(var Dst: TSmallIntArray); overload; 
    (*<summary>Sizes 1 byte unsigned integer array.</summary>*)
    procedure SizeToArray(var Dst: Math387.TByteArray); overload; 


    (*<summary>Resets any defined selection.</summary>
      <SeeAlso cref="Select"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SelectAll; overload;

    (*<summary>Resets any defined subrange.</summary>

      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange;
    (*<summary>Resets last defined subrange and pops the subrange stack.</summary>

      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRangeLevel;


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
     procedure SetSubIndex(const BeginIndex, EndIndex: integer); overload; 

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at
      Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted.

      All values of the original <see cref="Vector"/> will be preserved.
      An exception will be raised if an attempt is made to
      change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the
      source object and all operations done on the either of the objects
      affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      Note
        To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(const Index: integer; const Len: integer); overload;
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
    procedure SetSubRangeLevel(const Index: integer; const Len: integer); overload;

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

         All this can lead to hard to find bugs. It is therefore recommended
         to contstrain the use of the Subrange within the scope of one routine.
</remarks>


        <SeeAlso cref="SetSubRange"/>
        <SeeAlso cref="SetFullRange"/>*)

    procedure SetSubRange(const Src: TMtxVecInt); overload;

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
    procedure SetSubRange(const Src: TMtxVecInt; const Index: integer; const Len: integer = MtxVecEOA); overload; 

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
    procedure SetSubRangeLevel(const Src: TMtxVecInt; const Index: integer; const Len: integer = MtxVecEOA); overload; 










































    (*<summary>Sets the subarray size to full size.</summary>
      
<remarks>This method is the same as the <see cref="SetFullRange"/> method.
</remarks>
*)
    procedure SetSubRange; overload;

    (*<summary>Defines a sub-vector.</summary>
        
<remarks>Define a subvector of the Src vector starting at BeginIndex and ending at EndIndex (inclusive).
</remarks>


        <SeeAlso cref="SetSubRange"/>
        <SeeAlso cref="SetFullRange"/>*)

    procedure SetSubIndex(const Src: TMtxVecInt; const BeginIndex, EndIndex: integer); overload;

    (*<summary>Sets vector size to zero.</summary>
      
<remarks>Calling Clear sets <see cref="Length"/> property to 0.
</remarks>
*)
    procedure Clear;

    (*<summary>Saves the current value of ConditionCheck property and sets it to false.</summary>

      
<remarks>You can restore back to the original setting by calling <See Method="CondEnable"/> method.
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

    (*<summary> Stores distinct values from Src in to Dst. </summary>*)
    function Distinct(const Src: TMtxVecInt): TVecInt;


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
            MtxVecInt.FreeIt(ref a);
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
   function Abs(const Index, Len: integer): TMtxVecInt; overload;
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
   function Abs(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "and" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "and" of Src1 values with coresponding Src2 values
              stored in the calling object.</returns>*)
   function BinaryAnd(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "and" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "and" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryAnd(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "and"  of Src values with coresponding values in Self (this).</returns>*)
   function BinaryAnd(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "and" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in Src and Value.</summary>
     <returns>the result of binary "and" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
     <returns>the result of binary "and" between Value and
     values in the calling object.</returns>*)
   function BinaryAnd(const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
     <returns>the result of binary "and" between Value and
     values in the calling object at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in Src and Value.</summary>
     <returns>the result of binary "and" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "and"  of Src1 values with coresponding Src2 values stored
              in the calling object.</returns>*)
   function BinaryOr(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "or" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "or" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryOr(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "or" of Src values with coresponding values in Self (this).</returns>*)
   function BinaryOr(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "or" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1].</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between elements in Src and Value.</summary>
     <returns>the result of binary "or" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "or" between elements in Src and Value.</summary>
     <returns>the result of binary "or" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
     <returns>the result of binary "or" between Value and
     values in the calling object at locations [Index]..[Index+Len-1] stored
     back in the calling object.</returns>*)
   function BinaryOr(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
     <returns>the result of binary "or" between Value and
     values in the calling object.</returns>*)
   function BinaryOr(const Value: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
     <returns>the result of binary "xor" between Value and  values in the calling object.</returns>*)
   function BinaryXor(const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
     <returns>the result of binary "xor" between Value and
     values in the calling object at locations [Index]..[Index+Len-1] stored
     back in the calling object.</returns>*)
   function BinaryXor(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
     <returns>the result of binary "xor" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
     <returns>the result of binary "xor" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "xor" of Src1 values with coresponding Src2 values stored
              in the calling object.</returns>*)
   function BinaryXor(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "xor" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "xor" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryXor(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "xor" of Src values with coresponding values in Self (this).</returns>*)
   function BinaryXor(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "xor" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1] stored back in the calling object.</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left for all elements in the calling object.</summary>
     <returns>the result of binary shift to left by number of Bits applied to values in the calling object
              and stored back in the calling object.</returns>*)
   function BitShiftLeft(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in the calling object.</summary>
     <returns>the result of binary shift to left by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.</returns>*)
   function BitShiftLeft(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
     <returns>the result of binary shift to left by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].</returns>*)
   function BitShiftLeft(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
     <returns>the result of binary shift to left by number of Bits applied to Src values
              and stored in the calling object (Self).</returns>*)
   function BitShiftLeft(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right for all elements in the calling object.</summary>
     <returns>the result of binary shift to right by number of Bits applied to values in the calling object
              and stored back in the calling object.
              The sign of the numbers is preserved.</returns>*)
   function BitShiftRight(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right to elements in the calling object.</summary>
     <returns>the result of binary shift to right by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
     <returns>the result of binary shift to right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
     <returns>the result of binary shift to right by number of Bits applied to Src values
              and stored in the calling object (Self).
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) for all elements in the calling object.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to values in the calling object
              and stored back in the calling object.
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in the calling object.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to Src values
              and stored in the calling object (Self).
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>If Src[i] &lt;&gt; 0 then bit at index "i" in the calling vector is set to 1.
              If Src.Length is not divisable with 32, the remaining bits in the last sample are left at 0.
              The storage precision of the calling object is set to prInt32. The size of the calling object
              is adjusted automatically. Packing if array with precision other than Int32 is 8x slower.
              Packing of array with precision other than Int32 is 8x slower.
</remarks>
*)

   function BitPack(const Src: TVecInt): TVecInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>Returns the result of bit packing the Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object bits from [0]..[0+Len-1].
              If Len is not divisable with 32, the remaining bits in the last sample are left at 0.
              The storage precision of the calling object is set to prInt32.  The size of the calling object is adjusted automatically.
              Packing of array with precision other than Int32 is 8x slower.
</remarks>
*)
   function BitPack(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>If bit at Src[i] &lt;&gt; 0 then Integer (32, 16 or 8bit) at index "i" in the calling vector is set to 1.
              The storage precision of the calling object is preserved. The size of the calling object is adjusted
              automatically. Unpack to array with precision other than Int32 is 8x slower.
</remarks>
*)
   function BitUnpack(const Src: TVecInt; dstPrecision: TIntPrecision = prInt32): TVecInt; overload;

   (*<summary> Converts Src to unpacked bit storage. </summary>
               
<remarks>If bit at Src[i] &lt;&gt; 0 then Integer (32, 16 or 8bit) at index "i" in the calling vector is set to 1.
       Returns the result of bit unpacking the bits stred Src values [0]..[Len-1]
              and stored in the calling object (Self) values [Index]..[Index+Len-1].
              The storage precision of the calling object is preserved. The Len parameter specifies the number
              of bits that will be unpacked from Src. Unpack to array with precision other than Int32 is 8x slower.
</remarks>
*)
   function BitUnpack(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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
          TMtxVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,0,1,0});
            a.BinaryNot(); // // a = [-2,-1,-2,-1]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)

   function BinaryNot(): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to elements stored in Self (this).</summary>
     <returns>the result of binary "not" of values stored in Self (this).</returns>*)
   function BinaryNot(const Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values stored in Self (this).</returns>*)
   function BinaryNot(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values [SrcIndex]..[SrcIndex+Len-1] stored in Self (this) values [Index]..[Index+Len-1].</returns>*)
   function BinaryNot(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;


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



   

   
   
   function LoadFromStream(const SrcStream: TStream): Int64; overload;
   procedure SaveToStream(DstStream: TStream); overload;
   
   

   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+2*Len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 


   (*<summary>Copies values from Src array.</summary>
      
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
    function CopyFromArray(const Src: Math387.TWordArray; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The precision is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a real single precision floating point Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the single precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from an integer type Src array.</summary>*)
    function CopyFromArray(const Src: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 4 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to the calling object values [Index]..[Index+len-1]. The size of the calling object is not changed.
      If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from an 2 byte signed integer type Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 2 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array.</summary>*)
    function CopyFromArray(const Src: Math387.TByteArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
         to double precision numbers.</summary>
         
<remarks>Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to double precision floating point values. Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copy values to Dst array. The size of the array is set automatically.</summary>*)
    function CopyToArray(var Dst: TDoubleArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TDoubleArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies the calling object data to an array of single precision floating point data.</summary>
      
<remarks>Any values exceeding the range are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       single precision floating point array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 4 byte signed integer numbers.</summary>*)
    function CopyToArray(var Dst: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to 4 byte signed integers. Values exceeding the range of a 4 byte signed integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TIntegerArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 2 byte signed integer numbers.</summary>
       
<remarks>Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: TSmallIntArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 


    (*<summary>Copies values from the calling object to the Dst array and
               converts data to 2 byte unsigned integer type.</summary>
              
<remarks>Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)


    function CopyToArray(var Dst: Math387.TWordArray): TMtxVecInt; overload; 

    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: Math387.TWordArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 1 byte unsigned integer numbers.</summary>
       
<remarks>Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray): TMtxVecInt; overload; 

    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray; const DstIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Finds a match for X in object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
</remarks>

      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>*)

    function BinarySearch(const X: Integer): Integer; overload;
    function BinarySearch(const X: Integer; const Index: integer; const Len: integer): Integer; overload;

    (*<summary>Finds exact or closest index match for X in object values using binary search.</summary>
               
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

	  
      <returns>True, if found and the index of the next bigger or smaller value in XIndex, if not found. </returns>*)

    function BinarySearch(const X: Integer; var XIndex: integer): boolean; overload;
    function BinarySearch(const X: Integer; var XIndex: integer; const Index: integer; Len: integer): boolean; overload;

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
            a.SetIt(false, new int[] {2,5,1,6});
            int ind = a.Find(1);  // returns 2 (the arrays are zero based)
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)

   function Find(const X: integer): integer; overload;
   function Find(const X: integer; const Index: integer; const Len: integer = MtxVecEOA): integer; overload;


    (*<summary>Finds the masks for a vector and splits it.</summary>
      
<remarks>The method splits the a vector in two vectors. The MaskVec will hold the those
      elements where the Op comparison between a and b is True.
      Op string parameter can be '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;'.
      NotMaksVec will hold all those elements where the Op
      comparison between a and b is false.

      The calling vector will store the mask, 1 at those index locations
      where the Op comparison was True and 0 at those index locations
      where the Op comparison was false.
</remarks>


      <SeeAlso cref="Vector.FindAndGather"/>
      <SeeAlso cref="VectorInt.FindAndGather"/>
      <SeeAlso cref="FindIndexes"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Vector.Gather"/>
      <SeeAlso cref="VectorInt.Gather"/>
      <SeeAlso cref="Scatter"/>*)
    function FindAndSplit(const a: TMtxVec; const op: string; const b: TMtxVec; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
    (*<summary>The b parameter is of <see cref="TCplx"/> type.</summary>*)
    function FindAndSplit(const a: TMtxVec; const op: string; const b: TCplx; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
    (*<summary>The b parameter is of double type.</summary>*)
    function FindAndSplit(const a: TMtxVec; const op: string; const b: double; const MaskVec,NotMaskVec: TVec): TMtxVecInt; overload;

    (*<summary>Fills the calling vector with indexes, where the logical expression is true.</summary>
      
<remarks>Fills the calling vector with indexes, where the Op comparison between a and b is True. Op string parameter can be '&lt;', '&gt;',
      '&gt;=','&lt;=','=' or '&lt;&gt;'.
</remarks>


      <Example> Ensure that the power function will return 0, if the exponent is 0.
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

      <SeeAlso cref="Vector.FindAndGather"/>
      <SeeAlso cref="VectorInt.FindAndGather"/>
      <SeeAlso cref="FindAndSplit"/>
      <SeeAlso cref="FindMask"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Vector.Gather"/>
      <SeeAlso cref="VectorInt.Gather"/>
      <SeeAlso cref="Scatter"/>*)
    function FindIndexes(const a: TMtxVec; const op: string; const b: TCplx): TMtxVecInt; overload;
    (*<summary>The b parameter is of double type.</summary>*)
    function FindIndexes(const a: TMtxVec; const op: string; const b: double): TMtxVecInt; overload;
    (*<summary>The b parameter is of <See Class="TMtxVec"/> type.</summary>*)
    function FindIndexes(const a: TMtxVec; const op: string; const b: TMtxVec): TMtxVecInt; overload;

    (*<summary>Finds a vector mask.</summary>
      
<remarks>The calling vector will hold the those
      elements where the Op comparison between a and b is True.
      Op string parameter can be '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;'.

      The calling vector will store the mask, 1 at those index locations
      * where the Op comparison was True and 0 at those index locations
      * where the Op comparison was false.

      The calling vector stores the mask as Integers in to the memory
      location occupied by Values array.
</remarks>
 

      <SeeAlso cref="Vector.FindAndGather"/>
      <SeeAlso cref="VectorInt.FindAndGather"/>
      <SeeAlso cref="FindAndSplit"/>
      <SeeAlso cref="FindIndexes"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Vector.Gather"/>
      <SeeAlso cref="VectorInt.Gather"/>
      <SeeAlso cref="Scatter"/>*)
    function FindMask(const a: TMtxVec; const op: string; const b: TCplx): TMtxVecInt; overload;
    (*<summary>The b parameter is of double type.</summary>*)
    function FindMask(const a: TMtxVec; const op: string; const b: double): TMtxVecInt; overload;
    (*<summary>The b parameter is of <See Class="TMtxVec"/> type.</summary>*)
    function FindMask(const a: TMtxVec; const op: string; const b: TMtxVec): TMtxVecInt; overload;

    (*<summary>Fills the calling vector with indexes, where the logical expression is true.</summary>
      
<remarks>ills the calling vector with indexes, where the Op comparison between a and b is True. Op string parameter can be <c>'&lt;', '&gt;',
      '&gt;=','&lt;=','=' or '&lt;&gt;'</c>.
</remarks>


      <SeeAlso cref="Vector.FindAndGather"/>
      <SeeAlso cref="VectorInt.FindAndGather"/>
      <SeeAlso cref="FindMask"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Vector.Gather"/>
      <SeeAlso cref="VectorInt.Gather"/>
      <SeeAlso cref="Scatter"/>*)
    (*<summary>The b parameter is of integer type.</summary>*)
    function FindIndexes(const a: TMtxVecInt; const op: string; const b: integer): TMtxVecInt; overload;
    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindIndexes(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>The b parameter is of integer type.</summary>*)
    function FindIndexesAndLength(const a: TMtxVecInt; const op: string; const b: Integer): integer; overload;
    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindIndexesAndLength(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): integer; overload;


    (*<summary>Finds a vector mask.</summary>
      
<remarks>The calling vector will hold the those
      elements where the Op comparison between a and b is True.
      Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or &lt;&gt; </c>.

      The calling vector will store the mask, 1 at those index locations where the Op comparison was True and 0 at those index locations
      where the Op comparison was false.
</remarks>


      <SeeAlso cref="Vector.FindAndGather"/>
      <SeeAlso cref="VectorInt.FindAndGather"/>
      <SeeAlso cref="FindIndexes"/>
      <SeeAlso cref="Find"/>
      <SeeAlso cref="Vector.Gather"/>
      <SeeAlso cref="VectorInt.Gather"/>
      <SeeAlso cref="Scatter"/>*)
    (*<summary>The b parameter is of integer type.</summary>*)
    function FindMask(const a: TMtxVecInt; const op: string; const b: integer): TMtxVecInt; overload;
    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindMask(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): TMtxVecInt; overload;


    (*<summary>Finds and gathers vector elements.</summary>
      
<remarks>Fills the Indexes vector with indexes, of those elements where the Op
      comparison between a and b is True.  Op string parameter can be '&lt;', '&gt;', '&gt;=','&lt;=','=' or '&lt;&gt;'.
      The method also copies or "gathers" the matched elements to the calling vector.
      The Length and complex property of the calling vector are set automatically.
</remarks>
*)

    (*<summary>The b parameter is of integer type.</summary>*)
    function FindAndGather(const a: TMtxVecInt; const op: string; const b: integer; const Indexes: TVecInt = nil): TVecInt; overload;
    (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary>*)
    function FindAndGather(const a: TMtxVecInt; const op: string; const b: TMtxVecInt; const Indexes: TVecInt = nil): TVecInt; overload;

    (*<summary>Copies a column from matrix.</summary>
      
<remarks>Copies the Col-th column from Mtx matrix to calling vector. The <see cref="TMtxVecBase.Length"/>
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
      
<remarks>The <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property of calling vector must be set explicitly. An exception is raised if condition checking is
      enabled and column array borders are overrun.
</remarks>
*)
    function GetCol(const Mtx: TMtxInt; Row, Col, Index, Len: integer): TVecInt; overload;
    (*<summary>Copy the Col-th column elements [Row]..[Row+Len-1] to calling vector.</summary>
      
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling vector are adjusted automatically.
      An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
    function GetCol(const Mtx: TMtxInt; Row, Col, Len: integer): TVecInt; overload;

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

        See the <see cref="Scatter"/> method to see how to undo the gathering.

        The performance of the CPU heavily depends on the assumption that elements are stored at consecutive memory locations.
        If it is neccessary to apply a set of operations only to elements at specific indexes, performance-wise it can
        proof to be very helpfull, if the elements are gathered first.
</remarks>


        <SeeAlso cref="Vector.Gather"/>
        <SeeAlso cref="VectorInt.Gather"/>
        <SeeAlso cref="Scatter"/>*)
      function Gather(const X: TMtxVecInt; const Indexes: TVecInt =nil; IndexType: TIndexType = indVector; Increment: integer = 1; Offset: integer = 0): TVecInt; overload;

      (*<summary> Gathers elements from X starting at Offset and with Increment to the calling object. </summary>*)
      function GatherByIncr (const X: TMtxVecInt; Increment: integer = 1; Offset: integer = 0): TVecInt; overload;
      (*<summary> Gathers elements from X specified by indices stored in Indexes the calling object. </summary>*)
      function GatherByIndex(const X: TMtxVecInt; const Indexes: TVecInt): TVecInt; overload;
      (*<summary> Gathers elements from X specified by non-zero values stored in the Mask to the calling object. </summary>*)
      function GatherByMask (const X: TMtxVecInt; const Mask: TMtxVecInt; const MaskNonZeroCount: integer = -1): TVecInt; overload;

      (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
      function ScatterByMask(const Src: TMtxVecInt; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVecInt; overload;

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
      
<remarks>The <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property of calling vector must be set
      explicitly. An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
    function GetRow(const Mtx: TMtxInt; Row, Col, Index, Len: integer): TVecInt; overload;
    (*<summary>Copy the Row-th column elements [Col]..[Col+Len-1] to calling vector.</summary>
      
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of calling
      vector are adjusted automatically. An exception is raised if condition checking is enabled and column array borders are overrun.
</remarks>
*)
    function GetRow(const Mtx: TMtxInt; Row, Col, Len: integer): TVecInt; overload;

   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
     <returns>the result of adding Src2 values from coresponding Src1 values.</returns>*)
   function Add(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
    
<remarks>Add Src1 elements [Src1Index..Src1Index+Len-1] to corrresponding Src2 elements [Src2Index..Src12ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src1, Src2: TMtxVecInt; const Src1Index, Src2Index, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add Src elements to calling vector elements.</summary>*)
   function Add(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements.</summary>
    
<remarks>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to all Src elements.</summary>
      
<remarks>Add integer Value to all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to all calling vector elements.</summary>*)
   function Add(const Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract coresponding elements in Src2 from Src1.</summary>
     <returns>the result of subtracting Src2 values from coresponding Src1 values.</returns>*)
   function Subtract(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src1 from Src1.</summary>
    
<remarks>Subtract Src2  elements [Src2Index..Src2Index+Len-1] from corrresponding Src1 elements [Src1Index..Src1ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src1, Src2: TMtxVecInt; const Src1Index, Src2Index, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src elements from calling vector elements.</summary>*)
   function Subtract(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements.</summary>
    
<remarks>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from all Src elements.</summary>
      
<remarks>Subtract integer Value from all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from all calling vector elements.</summary>*)
   function Subtract(const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract all calling vector elements from integer Value.</summary>*)
   function SubtractFrom(const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract calling vector elements [Index..Index+Len-1] from integer Value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src vector elements from integer Value.</summary>
    
<remarks>and store thje results in calling vector. Size of calling vector is adjusted automatically.
</remarks>
*)
   function SubtractFrom(const Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src vector elements [SrcIndex..SrcIndex+Len-1] from integer Value.</summary>
      
<remarks>and store thje results in calling vector elements [Index..Index+Len-1]. Size of calling vector is adjusted automatically.
      An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(const Value: integer; const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Multiplies coresponding elements in Src2 with Src1.</summary>
     <returns>the result of multipliing Src2 values with coresponding Src1 values.</returns>*)
   function Multiply(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Multiply coresponding elements in Src2 with Src1.</summary>
    
<remarks>Multiply Src1 elements [Src1Index..Src1Index+Len-1] with corrresponding Src2 elements [Src2Index..Src2ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2,Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply Src elements with calling vector elements.</summary>*)
   function Multiply(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Mjultiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements.</summary>
    
<remarks>Multiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer Value with all Src elements.</summary>
      
<remarks>Multiply integer Value with all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with Src vector elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Store the results in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with all calling vector elements.</summary>*)
   function Multiply(const Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Value,Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divides coresponding elements in Num with Den.</summary>
     <returns>the result of dividing Num values with coresponding Den values.</returns>*)
   function Divide(const Num, Den: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide coresponding elements in Num with Den.</summary>
    
<remarks>Divide Num elements [NumIndex..NumIndex+Len-1] with corrresponding Deb elements [DenIndex..DenIndex+Len-1]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Num, Den: TMtxVecInt; const NumIndex, DenIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements.</summary>*)

   function Divide(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements [SrcIndex..SrcIndex+Len-1].</summary>
    
<remarks>Divide calling object elements  [Index..Index+Len ] with Src elements [SrcIndex..SrcIndex+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all Src vector elements with integer Value.</summary>
      
<remarks>Size of calling vector is adjusted automatically.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Src vector elements [SrcIndex..SrcIndex+Len-1] with integer Value.</summary>
    
<remarks>Divide Src elements [SrcIndex..SrcIndex+Len-1] with integer Value and store tje results in
    calling vector elements [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all calling vector elements with integer value.</summary>*)
   function Divide(const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide calling vector elements [Index..Index+Len-1] with integer value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divide Value with calling vector elements.</summary>
              
<remarks>Teh result is stored in the calling vector.
</remarks>
*)
   function DivideBy(const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Value with calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide Value with Src vector elements and store the result in the calling vector.</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide Value with Src vector elements [SrcIndex+SrcIndex+Len-1] and store the result in the calling vector
   [Index...Index+Len-1].</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value: integer; const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;



    

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
      
<remarks>Copy each of Vec elements to the calling object. Size and precision
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
          TMtxVecInt a,b,c;
          a = new TMtxVecInt();
          b = new TMtxVecInt();
          a.SetIt(new int[] {1,2,3,4});  // a = [1,2,3,4] i.e 1+2i ; 3+4i
          b.Copy(a);                // b = [1,2,3,4]
        }
      }
      </code></example>*)
   function Copy(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Copy Src elements [SrcIndex]..[SrcIndex+Len-1] in the calling object
     elements [Index]..[Index+Len-1].</summary>
     
<remarks>Vector <see cref="Size"/> must be set explicitly. An exception is raised if array borders
     are overrun or underrun.
</remarks>
*)
   function Copy(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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
   function IsEqual(const Value: integer): boolean; overload;

   (*<summary>Compares Value with all calling object elements.</summary>*)
   function IsEqual(const Value, Index, Len: integer): boolean; overload;

   (*<summary>Compares two objects.</summary>
      <returns>True, if they are equal.</returns>
      
<remarks>Compares two objects and returns True, if they are equal.

      The method compares <see cref="Length"/> and <see cref="IntPrecision"/> properties
      and coresponding values.
</remarks>
*)
   function IsEqual(const Vec: TMtxVecInt): boolean; overload;  
   (*<summary>Compares Vec elements [VecIndex]..[VecIndex+Len-1] with calling object
     elements [Index]..[Index+Len-1].</summary>*)
   function IsEqual(const Vec: TMtxVecInt; const VecIndex, Index, Len: integer): boolean; overload;  

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
   function Max(const Index, Len: integer): integer; overload;

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
   function Min(const Index, Len: integer): integer; overload;

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
   procedure MaxMin(out aMax, aMin: integer; const Index, Len: integer); overload;

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
            a.SetIt(new int[] {1,2,3,4});
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
   function Max(out aIndex: integer; const Index, Len: integer): integer; overload;

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
   function Min(out aIndex: integer; const Index, Len: integer): integer; overload;

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
   procedure MaxMinIdx(out aMax, aMaxIdx, aMin, aMinIdx: integer; const Index, Len: integer); overload;

   (*<summary>Applies median filter with size Mask to the calling vector.</summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
            vector with the median value, calculated over the fixed range (mask) centered around that element.
</remarks>
*)
   function Median(const MaskSize: integer): TMtxVecInt; overload;
   (*<summary>Applies median filter with size Mask to the calling vector elements [Index.. Index+Len-1].</summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
            vector with the median value, calculated over the fixed range (mask) centered around that element.
</remarks>
*)
   function Median(const MaskSize, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Applies median filter with size Mask to data in Src and stores the result in the calling vector.</summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
            vector with the median value, calculated over the fixed range (mask) centered around that element
            and stores the result in the calling vector.
</remarks>
*)
   function Median(const Src: TMtxVecInt; const MaskSize: integer): TMtxVecInt; overload;
   (*<summary>Applies median filter with size Mask to Src object elements [SrcIndex..SrcIndex+Len-1] and stores the result in the calling vector
                elements [Index..Index+Len-1]. </summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the Src
            vector with the median value, calculated over the fixed range (mask) centered around that element
            and stores the result in the calling vector.
</remarks>
*)
   function Median(const Src: TMtxVecInt; const MaskSize, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

     (*<summary>Resizes vector size while preserving values.</summary>
    
<remarks>Resizes calling vector <see cref="TMtxVecBase.Length"/> to Len and fills it with Src vector
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
   function Resize(const Src: TVecInt; const Len: integer; const ZeroIt: boolean = False): TVecInt; overload;

   (*<summary>Resizes calling vector Length to Len.</summary>
     
<remarks>If <see cref="Length"/> is less than Len and ZeroIt parameter is true, the remaining
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
   function Resize(const Len: integer; const ZeroIt: boolean = False): TVecInt; overload;

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
        a.Size(5,int32);
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
      a.Size(5,int32);
      for i:= 0 to a.Length-1 do a[i] := i*2;
    finally
      FreeIt(a);
    end;
    </code>
    </Example>

    <SeeAlso cref="SetVal"/>*)
   function Ramp: TVecInt; overload;
  (*<summary>Fills the calling vector.</summary>
    
<remarks>Method uses the following rule:

    <code>
    Values[k] := Offset + k*Step.
    </code><para/>
</remarks>
*)
   function Ramp(const Offset, Step: double): TVecInt; overload;
  (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
    
<remarks>Method uses the following rule:

    <code>Values[k] := Offset + k*Step.
    </code><para/>

    An exception is raised if calling vector
    array borders are overrun.
</remarks>
*)
   function Ramp(const Offset,Step: double; const Index,Len: integer): TVecInt; overload;

   (*<summary>A cyclic shift on all calling vector elements.</summary>
     
<remarks>Performs cyclic shift on all calling vector elements. Offset can be
     any integer number, positive or negative.
</remarks>


     <SeeAlso cref="Shift"/>*)
   function Rotate(const Offset: integer): TMtxVecInt; overload;
   (*<summary>A cyclic shift on vector elements in range.</summary>
     
<remarks>Performs cyclic shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

     An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Rotate(const Offset, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on all source vector elements and stores them to
      calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Shift"/>*)
   function Rotate(const Src: TMtxVecInt; const Offset: integer): TMtxVecInt; overload;
   (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Shift"/>*)
   function Rotate(const Src: TMtxVecInt; const Offset, SrcIndex, Index, Len: integer): TMtxVecInt; overload;


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
        property of the calling vector is set to implicitly match Vec vector.
</remarks>
*)
      function Reverse(const Vec: TMtxVecInt): TVecInt; overload;

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
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,2,3,4);
            a.Reverse();   // a = [4,3,2,1]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TMtxVecInt; const VecIndex, Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(const Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value elements and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Size and <see cref="IntPrecision"/> properties of the calling object and of RemDst are adjusted automatically.
</remarks>
*)
    function RemDiv(Src, Value: TMtxVecInt; RemDst: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value and store result of division in the calling object and reminder in RemDst.</summary>
                              
<remarks>Store the result of division in the calling object elements [Index]..[Index+Len-1]
                              and remainder in to [RemDstIndex] .. [RemDstIndex+ Len-1]. Size and <see cref="IntPrecision"/> properties of
                              the calling object and RemDst must be set explicitly. An exception is raised
                              if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
               if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
               if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
</remarks>
*)
    function RemDiv(Src: integer; Value: TMtxVecInt; RemDst: TMtxVecInt; ValueIndex, RemDstIndex, Index, Len: integer): TMtxVecInt; overload;

        (*<summary>Write object header and values to a file.</summary>
    
<remarks>Write the header describing the calling object and the values array of the calling object to the file,
    specified by the FileName. If the file already exist, the data is overwritten by default. If Append is
    True, the data is appended to the end of the file.

    Note
      It is recommended you use a *.mtx extension when you're saving/loading matrix to/from file. Similarly, you should use a *.Vec extension when you're saving/loading vector to/from file.
</remarks>


    <Example>
    <code>
    var Mtx: TMtxInt;
    begin
      CreateIt(Mtx);
      try
        Mtx.SetIt(2,2,[3,1,-1,5]);
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
    procedure SaveToFile(const FileName: string; const Append: boolean = False);

    (*<summary>Reads the header information and the values array content from the file.</summary>
      
<remarks>Reads the header information and the values array content from the file specified by FileName parameter.
</remarks>

      <Example>
      <code>
      var b: TVecInt;
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
    procedure LoadFromFile(const FileName: string);


    (*<summary>Sets object values.</summary>
      
<remarks>Set object values. Method call does not change object's size, but it does
      check for array overrun. The elements of A array are copied to the calling
      object elements, starting at Index. The IntPrecision property of the calling
      object is not changed.
</remarks>
*)
    function SetIt(const Index: integer; const A: array of integer): TMtxVecInt; overload;
    (*<summary>Sets object values.</summary>
      
<remarks>Set object values. The size of the calling object is adjusted to match the length
      of the array. The elements of A array are copied to the calling
      object elements. The IntPrecision property of the calling object is not changed.
</remarks>
*)
    function SetIt(const A: array of integer): TMtxVecInt; overload;

    (*<summary>Set calling object element <c>Index</c> to Value.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
     function SetVal(const Value: integer): TMtxVecInt; overload;
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
            a.SezIt(new int[] {1,2,3,4} );
            a.SetVal(5,2,2);  // a = [1,1,5,5]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function SetVal(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Do a shift on all calling vector elements.</summary>
     
<remarks>Performs shift on all calling vector elements. Offset can be
     any integer number, positive or negative.
</remarks>


     <SeeAlso cref="Rotate"/>*)
   function Shift(const Offset: integer): TMtxVecInt; overload;
   (*<summary>Perform a shift on vector elements in range.</summary>
     
<remarks>Performs shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Shift(const Offset, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Do a shift on vector elements in range.</summary>
      
<remarks>Performs shift on all source vector elements and stores them to
      calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

     An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Rotate"/>*)
   function Shift(const Src: TVecInt; const Offset: integer): TVecInt; overload;
    (*<summary>A shift on vector elements in range.</summary>
      
<remarks>Performs a shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Rotate"/>*)
   function Shift(const Src: TMtxVecInt; const Offset, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Set all calling vector elements to zero.</summary>*)
   function SetZero(): TMtxVecInt; overload;
   (*<summary>Set calling vector elements [Index..Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function SetZero(const Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Size the object.</summary>
      
<remarks>Assigns the size of the Src object to the calling object.
      If both objects have a matching <see cref="Length"/> property, only the <see cref="IntPrecision"/>
      property of the calling object will changed, while all other properties describing the size of the object
      will be preserved.
</remarks>
*)
   function Size(const Src: TMtxVecBase): TMtxVecInt; overload;

   (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
      <SeeAlso cref="SortDescend"/>*)
   function SortAscend: TMtxVecInt; overload;
   (*<summary>Sort calling vector elements [Index..Index+Len-1] in ascending order in-place.</summary>*)
   function SortAscend(const Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
   function SortAscend(const DstSortIdx: TVecInt): TVecInt; overload;

   function SortAscend(const DstSortIdx: TMtxVecInt; const SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort all calling vector elements in descending order in-place.</summary>

      <SeeAlso cref="SortAscend"/>*)
   function SortDescend: TMtxVecInt; overload;
   (*<summary>Sort calling vector elements [Index..Index+Len-1] in descending order in-place.</summary>*)
   function SortDescend(const Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Sort all calling vector elements in descending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
   function SortDescend(const DstSortIdx: TVecInt): TVecInt; overload;
   function SortDescend(const DstSortIdx: TMtxVecInt; const SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

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
   function Sum(const Index, Len: integer): integer; overload;

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
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/>
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
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1].
      Size and  <see cref="IntPrecision"/> properties of the calling object must be set explicitly.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TMtxVecInt; const GTLevel, GTValue, LTLevel, LTValue: Integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 



    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i]. Under .NET this is a pointer
      to unmanaged memory.
</remarks>
*)
   function PValues(const Index: integer): PAPointer;

 (*<summary>Read values content from stream to object.</summary>
      
<remarks>Reads values content from SrcStream stream to calling objct. No other values describing the data type or length are read
      from the DstStream. Number type is defined by the Precision parameter, which can be obtained from <see cref="ReadHeader"/> method
      call. The function returns the number of bytes read.

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
      <SeeAlso cref="LoadFromStream"/>*)
    
    function ReadValues(const SrcStream: TStream; const aPrecision: TPrecision = prInteger): Int64; overload;
    

    


   
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
    procedure WriteValues(DstStream: TStream; const aPrecision: TPrecision = prInteger); overload;
    

    

    

    
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
    <SeeAlso cref="WriteValues"/>*)
    procedure WriteHeader(DstStream: TStream; const aPrecision: TPrecision = prInteger); overload;
    

    

    
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
    function ReadHeader(const SrcStream: TStream): TPrecision; overload;
    

     (*<summary>Convert strings in aList to integers.</summary>
      
<remarks>Convert strings in aList to integers according to the IntPrecision property
      and store them in the Values array of the calling vector.
      The <see cref="TMtxVecBase.Length"/> of the calling vector is set to
      aList.Count.
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
    procedure StringsToValues(const aList: TStrings); overload;
    (*<summary>Convert strings in aList to integers starting at ListIndex.</summary>
      
<remarks>Store them in the Values array of the calling vector from Index to Index+Len-1.
      The Length property of the calling vector is not changed.
      If array bounds are overrun and exception is raised.
</remarks>
*)
    function StringsToValues(const aList: TStrings; const ListIndex: integer; const Index: integer = 0; const Len: integer = MtxVecEOA): TVecInt; overload;


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
      procedure ValuesToStrings(const dstList: TStrings;
                                const Align: TFixedTextAlign = ftaNone;
                                const Headers: boolean = false); overload;

      (*<summary>Convert elements from Index to Index+Len-1 of the calling vector to strings.</summary>
        
<remarks>Store values in dstList starting at ListIndex. If dstList is not large enough,
        the method will use the add method of dstList object. Set Equidistant to True, if you are using fixed width font
        and need to have the text right aligned.
</remarks>
*)
      procedure ValuesToStrings(const dstList: TStrings; ListIndex, Index,  Len: integer;
                                const Align: TFixedTextAlign = ftaNone;
                                const Headers: boolean = false); overload;

    (*<summary>Convert all vector elements to text.</summary>*)
    procedure ValuesToText(out AText: String); overload;
    (*<summary>Convert Index..Index+Len-1 vector elements to text.</summary>*)
    procedure ValuesToText(out AText: String; const Index,  Len: integer); overload;

    

    (*<summary>Cumulative sum.</summary>
    
<remarks>Calculate the cumulative sum for calling object elements [Index]..[Index+Len-1] in-place.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun.
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
      calling object must be set explicitly. Exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/>  property is True
      and array borders are overrun.
</remarks>
*)
    function CumSum(const Vec: TMtxVecInt; VecIndex,Index,Len: Integer): TMtxVecInt; overload; 

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

      (*<summary>Difference.</summary>
        
<remarks>Calculate the difference for Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object
        elements [Index]..[Index+Len-1]. Size of the calling vector must be set explicitly.
        The following formula is used to calculate the difference:

        <IMG name="tvec19"/>

        An exception is raised if <see cref="ConditionCheck"/> is True and array borders are overrun or underrun.
</remarks>
*)
      function Difference(const Vec: TMtxVecInt; VecIndex, Index, Len: integer): TMtxVecInt; overload;

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

      (*<summary>Returns content as a string.</summary>
                 
<remarks>Values will be separated by line breaks.
</remarks>
*)
      function ToString: string; 

      (*<summary>Parses string content as vector.</summary>
                 
<remarks>Values need to be separated by line breaks.
                 For other parsing options call StringToValues.
                 The Parse method works in pair with the ToString method both meant to be a reverse of the other,
                 but do not preserve any other property of the vector except only values.
                 Use SaveToStream/LoadFromStream methods for binary storage option, which uses up to 10x less memory and is a lot faster to save and load.
</remarks>
*)
      class function Parse(const Src: string): VectorInt; static;
  public
    (*<summary>Allows sizing the internal storage to accomodate desired number of bits.</summary>
               
<remarks>Use BitPack and BitUnpack for much faster access to bit data or Bits[i] property for individual
               bit access. Setting BitCount will also call the Size method. The Length of the internal arrays will be adjusted
               so that all bits can be stored with padding of the last (32bit, 16bit or 8bit integer) element.
               The value of BitCount, which is not an integer multiple of the IntPrecision is preserved only
               by certain methods where applicable and where it is not ambiguous. The value of the property affects
               only the results of BitPack and BitUnpack routines.
</remarks>
*)
    property BitCount: Int64 read get_BitCount write set_BitCount;
    
    (*<summary>Allows reading/writing individual bits of the array.</summary>
               
<remarks>Use BitPack and BitUnpack for much faster access to bit data.
</remarks>
*)
    property Bits[const Indx: integer]: boolean read GetBits write SetBits;
    
     (*<summary> Returns data for inspection by the debugger. </summary>*)
     property IntegerValues: string read Get_IntegerValues;
    (*<summary> Specifies the power of two for the scale factor used by some methods.</summary>*)
     property ScaleFactor: integer read get_ScaleFactor write set_ScaleFactor;
  end;









  (*<summary>Integer vector class for operator overloading (Delphi 2006 and later). </summary>
    
<remarks>Declare VectorInt instead of TVecInt to take advantage ofoperator overloading. Be carefull to declare
    VectorInt only for local variables with short lifetime.
    Call the Create method for VectorInt, if the variable is a global variable or a variable with a longer life.
    It also makes sense to continue to use TVecInt for global vars.
    If the Create method (constuctor) is not called, the VectorInt
    record obtains TVecInt object from object cache (fastest create).
    If the Create mehod is called, the TVecInt object is created in
    the usual way (slower). Object cache has limited size.
</remarks>


    <Example>
    <code>
    var b, c: VectorInt;
        bv: TVecInt;
    begin
          b := VectorInt(TIntegerArray.Create(1,1,1,1)); //b = [1,1,1,1];
          bv := b;   //b and bv point to same data object

          bv.Multiply(2);
          bv.Bits[76] := true;  //set 76th bit in the array to 1
          TVecInt(b).Multiply(2);  //an alternative way to scale
          c := b*2 + 3; //c := b*2 + 2 + 3i

          c := b*bv; //mix VectorInt and TVecInt

          bv.Add(c); //pass VectorInt type and implicitely convert to TVecInt pointer
          b.PackBits(c); // store bit 1 in b for all elements in c which are different from zero. Bit 0 otherwise.
    end;
    </code>
    </Example>*)


  
  MatrixInt = record 
  strict private
    
    FData: TMtxInt;
    
    
  private
    function get_Data: TMtxInt ;
    property Data: TMtxInt read get_Data;
  
  private
  
   

    function get_BlockEnd: boolean;
    function get_IsSubRange: boolean;
    function get_Caption: string;
    function get_ConditionCheck: boolean;
    function get_Length: Integer;
    function get_Tag: Integer;
    procedure set_Caption(const value: string);
    procedure set_ConditionCheck(const value: boolean);
    procedure set_Length(const value: integer);
    procedure set_Tag(const value: Integer);
    procedure set_ScaleFactor(const value: integer);
    function get_ScaleFactor: integer;
    procedure set_IntPrecision(const value: TIntPrecision);
    function get_IntPrecision: TIntPrecision;
    function get_IntegerValues: string;
    function get_First: integer;
    function get_Last: integer;


    
    function get_IValues1D(const Indx: integer): integer; 
    procedure set_IValues1D(const Indx: integer; const Value: integer); 

    function get_SValues1D(const Indx: integer): SmallInt;  
    procedure set_SValues1D(const Indx: integer; const Value: SmallInt);

    function get_BValues1D(const Indx: integer): Byte;  
    procedure set_BValues1D(const Indx: integer; const Value: Byte);


    function get_IValues(const rowIdx, colIdx: integer): integer; 
    procedure set_IValues(const rowIdx, colIdx: integer; const Value: integer); 

    function get_SValues(const rowIdx, colIdx: integer): SmallInt;  
    procedure set_SValues(const rowIdx, colIdx: integer; const Value: SmallInt);

    function get_BValues(const rowIdx, colIdx: integer): Byte;  
    procedure set_BValues(const rowIdx, colIdx: integer; const Value: Byte);

    function GetSelect(const Indx, Len: integer): VectorInt;  
    procedure SetSelect(const Indx, Len: integer; const Value: VectorInt); 

    function GetSelectIndex(const startIndex, Step, stopIndex: integer): VectorInt;
    procedure SetSelectIndex(const startIndex, Step, stopIndex: integer; const Value: VectorInt);

    function GetSelect2D(const RowIdx, ColIdx, RowLen, ColLen: integer): MatrixInt;
    function GetSelectIndex2D(const StartRowIdx, StartColIdx, StopRowIdx, StopColIdx: integer): MatrixInt;

    procedure SetSelect2D(const RowIdx, ColIdx, RowLen, ColLen: integer;  const Value: MatrixInt);
    procedure SetSelectIndex2D(const StartRowIdx, StartColIdx, StopRowIdx, StopColIdx: integer; const Value: MatrixInt);


    

    function get_Bits(const row, col: integer): boolean; 
    procedure set_Bits(const row, col: integer; const Value: boolean); 
    function get_ColsBitCount: integer;
    procedure set_ColsBitCount(const value: integer);
    procedure SetCols(const Value: integer);
    procedure SetLeadingCols(const Value: integer);
    procedure SetRows(const Value: integer);
    function GetCols: integer;
    function GetLeadingCols: integer;
    function GetRows: integer;

    function get_Capacity: Int64;
    procedure set_Capacity(const value: Int64);

    function get_CapacityStep: double;
    procedure set_CapacityStep(const value: double);










   
  public
    
     



    
    
    
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

    (*<summary>Cumulative sum.</summary>
    
<remarks>Calculate the cumulative sum for calling object elements [Index]..[Index+Len-1] in-place.
      An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun.
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
      calling object must be set explicitly. Exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/>  property is True
      and array borders are overrun.
</remarks>
*)
    function CumSum(const Vec: TMtxVecInt; VecIndex,Index,Len: Integer): TMtxVecInt; overload; 

    (*<summary>Difference.</summary>
      
<remarks>Calculate the difference for Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object
      elements [Index]..[Index+Len-1]. Size of the calling vector must be set explicitly.
      The following formula is used to calculate the difference:

      <IMG name="tvec19"/>

      An exception is raised if <see cref="ConditionCheck"/> is True and array borders are overrun or underrun.
</remarks>
*)
    function Difference(const Vec: TMtxVecInt; VecIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Adopts TMtxInt object.</summary>
      
<remarks>Src object will be adopted by MatrixInt. When the MatrixInt gets out of scope, the object
      will be freed.
</remarks>
*)
    procedure Adopt(const Src: TMtxInt); overload;






































      (*<summary>Defines a subarray.</summary>
      
<remarks>The method will define a sub array starting at
      BeginIndex and ending at EndIndex (inclusive).
      All values of the original MatrixInt will be preserved.
      An exception will be raised if an attempt is made to
      change the size of calling MatrixInt.

      A subarray is array which does not have its own
      memory allocated. Instead it adopts the memory of the
      source object and all operations done on the either of the objects
      affect the same elements. The use of subarrays increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      Note
        To again obtain a view of the full Matrix see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubIndex(BeginIndex, EndIndex: integer); overload;


   (*<summary>Resets any defined selection.</summary>

      <SeeAlso cref="Select"/>
      <SeeAlso cref="SetSubIndex"/>*)

    procedure SelectAll;

    (*<summary>Prevents calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Prevents calls to <see cref="SetSubRange"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
      When subranging matrices in-place, they are temporariliy converted
      to a vector by settings Rows to 1 and Cols to Length.
</remarks>


      <Example>
      <code>
      var a: MatrixInt;
          b: VectorInt;
      begin
          a.SetIt(3,3, [1,2,3,4,5,6,7,8,9]);
          a.SetSubRange(0,2);  //a = [1,2]
          a.DisableSubrange;
          a.SetSubRange(2,2); //exception raised here

          b.SetSubRange(a,2,2); //but this will work
          a.SetFullRange; //b is not changed, it still points to [4,5]
      end;
      </code>
      </Example>


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


      <Example>
      <code>
      var a: MatrixInt;
          b: VectorInt;
      begin
          a.SetIt(3,3,[1,2,3,4,5,6,7,8,9]);
          a.Select(0,2);  //a = [1,2]
          a.DisableSelect;
          a.Select(2,2); //exception raised here

          b.Select(a,2,2); //but this will work
          a.SelectAll; //b is not changed, it still points to [4,5]
      end;
      </code>
      </Example>

      <SeeAlso cref="DisableSubrange"/>
      <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSelect;

    (*<summary>Enables calls to <see cref="Select"/>.</summary>
      
<remarks>Enables calls to <see cref="Select"/> by removing
      the block set by <see cref="DisableSelect"/>.
</remarks>
*)
    procedure EnableSelect;

    (*<summary>Returns true, if Left and Right are of equal Length and Values.</summary>

      
<remarks><code>
        var a,b: MatrixInt;
            c: TDoubleArray;
        begin
            c := TDoubleArray.Create(1,1,1,1,1,0);

            a.Size(3,3);
            a.CopyFromArray(c);
            b.Copy(a);
            if a = b then ERaise('a and b are equal!');

            if a = 1 then ERaise('a equals 1!');   //compare to real value
        end;
        </code>
</remarks>


     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    class operator Equal(const Left, Right: MatrixInt): Boolean;
    (*<summary>Returns true, if all elements of Left are equal to Right.</summary>*)
    class operator Equal(const Left: MatrixInt; const Right: integer): Boolean;
    (*<summary>Returns true, if all elements of Right are equal to Left.</summary>*)
    class operator Equal(const Left: integer;const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right have equal Rows, Cols, IntPrecision and Values.</summary>*)
    class operator Equal(const Left: TMtxInt; const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right have equal Rows, Cols, IntPrecision and Values.</summary>*)
    class operator Equal(const Left: MatrixInt; const Right: TMtxInt): Boolean;

    (*<summary>Returns true, if Left and Right are of equal Length, IntPrecision and Values.</summary>*)
    class operator Equal(const Left: TVecInt; const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right are of equal Length, IntPrecision and Values.</summary>*)
    class operator Equal(const Left: MatrixInt; const Right: TVecInt): Boolean;

    (*<summary>Returns true, if Left and Right are not equal.</summary>
       
<remarks>Returns true, if Left and Right do not have equal Length and Values.

        <code>

        var a,b: MatrixInt;
        begin
            a := MatrixInt(TIntegerArray.Create(1,1,1,1,1));
            if a &lt;&gt; b then ERaise('a and b are not equal!');

            if a &lt;&gt; 1 then ERaise('a does not equals 1!');   //compare to integer value
        end;
        </code>
</remarks>


     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    class operator NotEqual(const Left, Right: MatrixInt): Boolean;
    (*<summary>Returns true, if any elements of Left are not equal to Right.</summary>*)
    class operator NotEqual(const Left: MatrixInt; const Right: integer): Boolean;
    (*<summary>Returns true, if any elements of Right are not equal to Left.</summary>*)
    class operator NotEqual(const Left: integer;const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right do not have equal Rows, Cols, IntPrecision and Values.</summary>*)
    class operator NotEqual(const Left: TMtxInt; const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right do not have equal Rows, Cols, IntPrecision and Values.</summary>*)
    class operator NotEqual(const Left: MatrixInt; const Right: TMtxInt): Boolean;

    (*<summary>Returns true, if Left and Right do not have equal Length, IntPrecision and Values.</summary>*)
    class operator NotEqual(const Left: TVecInt; const Right: MatrixInt): Boolean;
    (*<summary>Returns true, if Left and Right do not have equal Length, IntPrecision and Values.</summary>*)
    class operator NotEqual(const Left: MatrixInt; const Right: TVecInt): Boolean;


    (*<summary>Returns true, if all elements of ALeft are smaller from ARight.</summary>*)
    class operator LessThan(const ALeft: MatrixInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is smaller from all elements in ARight.</summary>*)
    class operator LessThan(const ALeft: integer; const ARight: MatrixInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater than
      coresponding elements in ARight.</summary>*)
    class operator LessThan(const ALeft: MatrixInt; const ARight: MatrixInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are smaller or equal from ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: MatrixInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is smaller or equal from all elements in ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: integer; const ARight: MatrixInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are smaller than or equal from
      coresponding elements in ARight.</summary>*)
    class operator LessThanOrEqual(const ALeft: MatrixInt; const ARight: MatrixInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are greater or equal from ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: MatrixInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is greater or equal from all elements in ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: integer; const ARight: MatrixInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater or equal from
      coresponding elements in ARight.</summary>*)
    class operator GreaterThanOrEqual(const ALeft: MatrixInt; const ARight: MatrixInt): Boolean;

    (*<summary>Returns true, if all elements of ALeft are greater from ARight.</summary>*)
    class operator GreaterThan(const ALeft: MatrixInt; const ARight: integer): Boolean;
    (*<summary>Returns true, if ALeft is greater from all elements in ARight.</summary>*)
    class operator GreaterThan(const ALeft: integer; const ARight: MatrixInt): Boolean;
    (*<summary>Returns true, if coresponding elements in ALeft are greater than
      coresponding elements in ARight.</summary>*)
    class operator GreaterThan(const ALeft: MatrixInt; const ARight: MatrixInt): Boolean;

    (*<summary>Add Left to all elements in Right and return result.</summary>*)
    class operator Add(const Left: integer;const Right: MatrixInt): MatrixInt;
    (*<summary>Add Right to all elements in Left and return result.</summary>*)
    class operator Add(const Left: MatrixInt; const Right: integer): MatrixInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Add coresponding elements in Left and Right.</summary>*)
    class operator Add(const Left: MatrixInt;const Right: MatrixInt): MatrixInt;

    (*<summary>Subtract all elements in Right from Left.</summary>*)
    class operator Subtract(const Left: integer; const Right: MatrixInt): MatrixInt;
    (*<summary>Subtract Right from all elements in Left.</summary>*)
    class operator Subtract(const Left: MatrixInt; const Right: integer): MatrixInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Subtract coresponding elements in Right from Left.</summary>*)
    class operator Subtract(const Left: MatrixInt;const Right: MatrixInt): MatrixInt;

    (*<summary>Multiply all elements in Left with Right.</summary>*)
    class operator Multiply(const Left: MatrixInt; const Right: integer): MatrixInt;
    (*<summary>Multiply all elements in Right with Left.</summary>*)
    class operator Multiply(const Left: integer; const Right: MatrixInt): MatrixInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Multiply all elements in Left with corresponding elements in Right.</summary>*)
    class operator Multiply(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;

    (*<summary>Divide all elements in Left with Right.</summary>*)
    class operator Divide(const Left: MatrixInt; const Right: integer): MatrixInt;
    (*<summary>Divide Left with all elements Right.</summary>*)
    class operator Divide(const Left: integer; const Right: MatrixInt): MatrixInt;

    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Divide all elements in Left with coresponding elements in Right.</summary>*)
    class operator Divide(const Left: MatrixInt; const  Right: MatrixInt): MatrixInt;

    (*<summary>Negates all values inside AValue.</summary>*)
    class operator Negative(const AValue: MatrixInt): MatrixInt;


    


    (*<summary>Explicitely convert array of complex values to MatrixInt.</summary>
     
<remarks>Copies all values from the complex array to the result with precision
     set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TCplxArray): MatrixInt;
    (*<summary>Explicitely convert array of double values to MatrixInt.</summary>
      
<remarks>Copies all values from the double array to the result with
      precision set at 32bit signed integer;
</remarks>
*)
    class operator Explicit(const AValue: TDoubleArray): MatrixInt;
    (*<summary>Explicitely convert array of single values to MatrixInt.</summary>
      
<remarks>Copies all values from the AValue array to the result with precision
      set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TSingleArray): MatrixInt;
    (*<summary>Explicitely convert TMtxVec to MatrixInt.</summary>
       
<remarks>Copies all values from AValue to the result with precision
       set at 32bit signed integer.
</remarks>
*)
    class operator Explicit(const AValue: TMtxVec): MatrixInt;

    (*<summary>Explicitely convert MatrixInt to array of doubles.</summary>
      
<remarks>Copies all values from AVector to an array of double.
</remarks>
*)
    class operator Explicit(const AValue: MatrixInt): TDoubleArray;
    (*<summary>Explicitely convert MatrixInt to array of doubles.</summary>
      
<remarks>Copies all values from AVector to an array of single.
</remarks>
*)
    class operator Explicit(const AValue: MatrixInt): TSingleArray;

    (*<summary>Explicitely convert MatrixInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 32bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: MatrixInt): TIntegerArray;

    (*<summary>Explicitely convert MatrixInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 16bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: MatrixInt): TSmallIntArray;

    (*<summary>Explicitely convert MatrixInt to array of integer.</summary>
      
<remarks>Copies all values from AVector to an array of 16bit signed integers.
      If Precision does not match conversion will be performed.
</remarks>
*)
    class operator Explicit(const AValue: MatrixInt): Math387.TByteArray;

    


    
    


    

    (*<summary>Implicit type conversion from MatrixInt to an array of 32bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not
      match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TIntegerArray;

    (*<summary>Implicit type conversion from MatrixInt to an array of 32bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if MatrixInt.IsSubRange = True.
      PInteger type is also defined in Windows.pas unit and sometimes the type needs to be specified with the unit name (Math387.PInteger) for this
      overload to work.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): Math387.PInteger;

    (*<summary>Implicit type conversion from MatrixInt to an array of 16bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not
      match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TSmallIntArray;

    (*<summary>Implicit type conversion from MatrixInt to an array of 16bit signed integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if MatrixInt.IsSubRange = True.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): Math387.PSmallInt;

    (*<summary>Implicit type conversion from MatrixInt to an array of 8bit unsigned integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not
      match an exception will be raised.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): Math387.TByteArray;

    (*<summary>Implicit type conversion from MatrixInt to an array of 8bit unsigned integer.</summary>
      
<remarks>Returns a pointer to the internal array of values. If the Precision does not match an exception will be raised.
      This conversion avoids all range checking, which would be broken, if MatrixInt.IsSubRange = True.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): Math387.PByte;

    

    (*<summary>Implicit type conversion from MatrixInt to TMtxInt.</summary>
      
<remarks>Returns a pointer to the internal TMtxInt object.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TMtxInt;
    (*<summary>Implicit type conversion from MatrixInt to TMtxInt.</summary>
      
<remarks>Returns a pointer to the internal TMtxInt object.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TMtxVecInt;
    (*<summary>Implicit type conversion from MatrixInt to TMtxInt.</summary>
      
<remarks>Returns a pointer to the internal TMtxInt object.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TMtxVecBase;

    
    (*<summary>Implicit type conversion from MatrixInt to TMtxInt.</summary>
      
<remarks>Returns a pointer to the internal TMtxInt object.
</remarks>
*)
    class operator Implicit(const AValue: MatrixInt): TObject;
    

    
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Computes bitwise "and" between Left and right.</summary>*)
    class operator BitwiseAnd(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;
    

    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Computes logical "and" between Left and right.</summary>*)
    class operator LogicalAnd(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;

    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Computes logical "or" between Left and right.</summary>*)
    class operator LogicalOr(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;

    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Computes bitwise "or" between Left and right.</summary>*)
    class operator BitwiseOr(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;

    (*<summary>Performs bitwise left shift of aValue.</summary>*)
    class operator LeftShift(const aValue: MatrixInt; Bits: integer): MatrixInt;
    (*<summary>Performs bitwise right shift of aValue.</summary>
               
<remarks>The shift preserves the sign of the numbers.
</remarks>
*)
    class operator RightShift(const aValue: MatrixInt; const Bits: integer): MatrixInt;

    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: TMtxInt; const Right: MatrixInt): MatrixInt;
    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: MatrixInt; const Right: TMtxInt): MatrixInt;
    (*<summary>Performs logical "xor" between Left and Right.</summary>*)
    class operator BitwiseXor(const Left: MatrixInt; const Right: MatrixInt): MatrixInt;

    (*<summary>Performs logical (and bitwise) "not" on the Value.</summary>*)
    class operator LogicalNot(const Value: MatrixInt): MatrixInt;

    

    procedure CreateFromCache(Value: boolean);

    (*<summary>Constructor of the record.</summary>
      
<remarks>Returns a Vector with internal TMtxInt object created explicitely with Length property set to
      aLength and IntPrecision property set to int32. Call this constructor, if the Vector type
      variable is a global variable with a long life span.
</remarks>


      <Example>
       <code>
        var a,b: MatrixInt;
            bvec: TMtxInt;
        begin
            a := MatrixInt.Create(true); //create from object cache via CreateIt

            //this constructor can be omitted, because it is implicitly called
            //the first time that the variable is used.
            //However:

            b := MatrixInt.Create(10);

            //Similar to

            bvec := TMtxInt.Create;
            try
                bvec.Size(10);
            finally
                bvec.Free;  //b is freed on the exit from the procedure
            end;
        end;
      </code>
      </Example>

     <SeeAlso cref="TMtxVecInt.IsEqual"/>*)
    constructor Create(const aRows, aCols: integer); overload;

    (*<summary>Internally creates TMtxInt object without using object cache.</summary>
                
<remarks>Creates TMtxInt object without using object cache. Suitable for declaring global variables.
</remarks>
*)
    constructor Create(const aRows, aCols: integer; const aPrecision: TIntPrecision); overload;
    (*<summary>Uses TMtxInt object as internal storage.</summary>
                
<remarks>The resulting Vector will own Src object of TMtxInt type and will release it once Vector gets out of scope.
</remarks>
*)
    constructor Create(const Src: TMtxInt); overload;

    (*<summary>Constructor of the record.</summary>
      
<remarks>Returns a Vector with internal TMtxInt object created from  object cache (CreateIt),
      if FromObjectCache is True. Pass false to the constructor or do not call it at all,
      if the variable is a local variable. Object cache has limited size.
</remarks>
*)
    constructor Create(FromObjectCache: boolean); overload;


  public

    (*<summary>Defines the length in number of samples.</summary>
      
<remarks>Defines the number of samples that the Vector can store.
      The length property does not map directly to memory reallocation
      when it is changed. Until the amount of preallocated memory
      is not exceed, there is no reallocation going on.

      Changing the Length property will preserve the existing
      values, but only if the new property value is smaller than the
      amount of preallocated memory. To properly resize the memory allocated
      use the <see cref="Resize"/> method.

      It is recommended that vector sizes do not exceed the size of
      preallocated memory to increase overall performance. The size
      of preallocated memory can be controlled via the Controller global
      variable which is defined in the MtxVec unit.
</remarks>
*)
    property Length: integer read get_Length write set_Length;
    (*<summary>Enables/disable inline condition checking.</summary>
      
<remarks>Enables/disables inline condition checking. When true, TMtxInt methods perform additional (range)
      checking before operations are executed. The drawback is slight loss of speed.
      This speeds up the execution but disables some safeguards.
      For short vectors the loss of speed can be significant.
      Some methods (parameterless) don't use the ConditionCheck property at all.
      ConditionCheck property is initially True. This property is an additional safeguard against array overrun or underrun errors.
      It allows to work with explicit range checking while writing and debugging the application and once your code is running you
      can turn it off. By functionality it is similar to assertions. (See Assert Delphi procedure).
</remarks>
 

      <SeeAlso cref="CondEnable"/>
      <SeeAlso cref="CondDisable"/>*)
    property ConditionCheck: boolean read get_ConditionCheck write set_ConditionCheck;
    (*<summary>Vector Caption.</summary>
      
<remarks>Use this property to set/get the string caption for the MatrixInt.
      This can be usefull for associating description with data when making
      a user interface.
</remarks>
*)
    property Caption: string read get_Caption write set_Caption;
    (*<summary>Stores an integer value as a part of the object.</summary>
      
<remarks>Tag has no predefined meaning. The Tag property is provided for the convenience of developers.
      It can be used for storing an additional integer value or it can be typecast to any 32-bit value such as a
      component reference or even a pointer.
</remarks>
*)
    property Tag: Integer read get_Tag write set_Tag;

    (*<summary>Sizing the object will not allocate less than Capacity samples.</summary>
               
<remarks>To free memory allocated before, first set Capacity to 0.
</remarks>
*)
    property Capacity: Int64 read get_Capacity write set_Capacity;

    (*<summary>Specifies increment step for the Capacity property.</summary>
               
<remarks>If this property is 0, the Capacity will never be modified on its own.
               When the value 1, the capacity will continue increasing to match largest value
               requested. When the value is more than 1, the capacity will be increasing with
               the factor specified.
</remarks>
*)
    property CapacityStep: double read get_CapacityStep write set_CapacityStep;
  public
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @IValues[i]. Under .NET this is a pointer
      to unmanaged memory or pinned managed memory.
</remarks>


     <SeeAlso cref="PSValues"/>
     <SeeAlso cref="PBValues"/>*)
    function PIValues(const Index: integer): PPInteger; overload;
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @SValues[i]. Under .NET this is a pointer
      to unmanaged memory or pinned managed memory.
</remarks>


     <SeeAlso cref="PIValues"/>
     <SeeAlso cref="PBValues"/>*)
    function PSValues(const Index: integer): PPSmallInt; overload;
    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @BValues[i]. Under .NET this is a pointer
      to unmanaged memory or pinned managed memory.
</remarks>


     <SeeAlso cref="PIValues"/>
     <SeeAlso cref="PSValues"/>*)
    function PBValues(const Index: integer): PPByte; overload;

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PIValues(const Row, Col: integer): PPInteger; overload;

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PSValues(const Row, Col: integer): PPSmallInt; overload;

    (*<summary>Returns a pointer to the real value stored at Row and Col.</summary>*)
    function PBValues(const Row, Col: integer): PPByte; overload;


    (*<summary> Defines internal storage precision which can be either 32, 16 or 8 bit.</summary>*)
    property IntPrecision: TIntPrecision read get_IntPrecision write set_IntPrecision;

    (*<summary>Returns true, if the currrent subrange of the vector was also the last subrange in the MatrixInt.</summary>
      
<remarks>Returns true, if the currrent subrange of the vector was also the last subrange in the MatrixInt.
      This property be used together with <See Method="BlockNext"/> and <See Method="BlockInit"/>.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockNext"/>*)
    property BlockEnd: boolean read Get_BlockEnd;
    (*<summary>Set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TMtxInt method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector
      length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read get_IsSubRange;

    

    (*<summary>Allows setting/getting the signed 32bit integer value at position Indx.</summary>
       
<remarks>Allows setting/getting the real value at position Indx.
       This property reads/writes to the same memory as <see cref="SValues"/> and <see cref="BValues"/> properties.
</remarks>
*)
    property IValues[const rowIdx, colIdx: integer]: integer read get_IValues write set_IValues; default;
    (*<summary>Allows setting/getting the signed 16bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the signed 16bit integer value at position Indx.
      This property reads/writes to the same memory as <see cref="BValues"/> and <see cref="IValues"/> properties.
</remarks>
*)
    property SValues[const rowIdx, colIdx: integer]: smallint read get_SValues write set_SValues;
    (*<summary>Allows setting/getting the 8bit unsigned integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 8bit unsigned integer value at position Indx.
      This property reads/writes to the same memory as <see cref="IValues"/> and <see cref="SValues"/> properties.
</remarks>
*)
    property BValues[const rowIdx, colIdx: integer]: byte read get_BValues write set_BValues;


    (*<summary>Allows setting/getting the signed 32bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the real value at position Indx.
     This property reads/writes to the same memory as <see cref="SValues"/> and <see cref="BValues"/> properties.
</remarks>
*)
    property IValues1D[const Indx: integer]: integer read get_IValues1D write set_IValues1D;
    (*<summary>Allows setting/getting the signed 16bit integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the signed 16bit integer value at position Indx.
      This property reads/writes to the same memory as <see cref="BValues"/> and <see cref="IValues"/> properties.
</remarks>
*)
    property SValues1D[const Indx: integer]: smallint read get_SValues1D write set_SValues1D;
    (*<summary>Allows setting/getting the 8bit unsigned integer value at position Indx.</summary>
      
<remarks>Allows setting/getting the 8bit unsigned integer value at position Indx.
      This property reads/writes to the same memory as <see cref="IValues"/> and <see cref="SValues"/> properties.
</remarks>
*)
    property BValues1D[const Indx: integer]: byte read get_BValues1D write set_BValues1D;

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will return a sub-ranged Vector. Similar to calling b.SetSubRange(a, Indx, Len);
      Writing this property will copy source to the specified subrange. The size of the value being assigned
      needs to match the range specified.
</remarks>
*)

    property Select[const Indx, Len: integer]: VectorInt read GetSelect write SetSelect; 

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will return a copied selection of subranged VectorInt by calling Dst.GatherByIncr(Src, startIndex, Step);
      Writing this property will copy source to the specified range by using Data.ScatterByIncr.
      The size of the value being assigned needs to match the index range specified.
      This property does not obtain a "view" unless Step equals to 1.
</remarks>
*)
    property SelectIndex[const startIndex, Step, stopIndex: integer]: VectorInt read GetSelectIndex write SetSelectIndex; 

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will copy MatrixInt from the specified location. Writing this property will copy the Source to the specified location.
      This property does not obtain a "view".
</remarks>
*)
    property Select2D[const RowIdx, ColIdx, RowLen, ColLen: integer]: MatrixInt read GetSelect2D write SetSelect2D;

    (*<summary>Allows copying/assignment of subranges.</summary>
      
<remarks>Reading this property will copy MatrixInt from the specified location.
      Writing this property will copy the Source to the specified location.
      This property does not obtain a "view".
</remarks>
*)
    property SelectIndex2D[const StartRowIdx, StartColIdx, StopRowIdx, StopColIdx: integer]: MatrixInt read GetSelectIndex2D write SetSelectIndex2D;

    

    
    procedure AssignWithoutCopy(const Src: MatrixInt);
    procedure Assign(const Src: MatrixInt);
    

    (*<summary>Initializes block processing.</summary>
      
<remarks>Initializes block processing. Because the size of the CPU cache is limited, significant performance gains can be obtained by
      splitting long vectors in to a series of short ones, which can all fit in the CPU cache entirely. The BlockInit method is
      to be used together with <see cref="BlockNext"/> and <See Method="BlockEnd"/> methods to initialize a block processing
      while loop. BlockInit will call <See Method="SetSubRange"/> to obtain subrange of the data in TMtxInt. The <see cref="Length"/>
      of the subranged vector is determined by the global  <see cref="Math387.MtxVecBlockSize"/> variable
      declared in <See Unit="Math387"/> unit. Default value of MtxVecBlockSize is preset to 800 vector elements for double precision
      and 1600 elements for single precision. BlockInit supports nested calls and from witihin a blocked while loop you can call
      procedures and functions which are also blocked. If you use block processing, typical performance gains will range from
      2 to a maximum of 6. Block processing can not be used, or it is difficult to apply, in cases where vector elements are
      not independent of each other. The block processing while loop must be written like this:
</remarks>


      <Example>
      <code>
      a.BlockInit;
      while not A.BlockEnd do
      begin
        // .... user defined function
      a.BlockNext;
      end;
      </code>

      The differences with block processing will be more noticable on older CPU's without
      support for SSE2/SSE3/SSE4/AVX when using FPU instructions.
      </Example>

      <SeeAlso cref="BlockNext"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockInit; overload;
    (*<summary>Initializes block processing with specified ABLockSize.</summary>*)
    procedure BlockInit(ABlockSize: integer); overload;

    (*<summary>Initializes block processing.</summary>*)
    procedure BlockInit(const Src: TMtxVecInt); overload;

    (*<summary>Initializes block processing.</summary>
               
<remarks>Block processing can be applied on possibly already subranged Src object.
               Src may not be freed or go out of scope until block processing loop has finished. There would
               be no error raised other than AV.
</remarks>
*)
    procedure BlockInit(const Src: TMtxVecInt; ABlockSize: integer); overload;

    (*<summary>Obtains the next subrange of the data.</summary>
      
<remarks>Obtains the next subrange of the data in TMtxInt. The routine must be used together with
      <see cref="BlockEnd"/> and <see cref="BlockInit"/> methods.
</remarks>


      <SeeAlso cref="BlockInit"/>
      <SeeAlso cref="BlockEnd"/>*)
    procedure BlockNext;

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

   (*<summary>First element in object Values array.</summary>
      <returns>first integer element in object Values array.</returns>

      <SeeAlso cref="Last"/>*)
    property First: integer read get_First;

    (*<summary>Last element in object Values array.</summary>
      <returns>the last integer element in calling object Values array.</returns>

      <SeeAlso cref="First"/>*)
    property Last: integer read get_Last;

    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: T2DIntegerArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the Length of the calling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: T2DSmallIntArray); overload;
    (*<summary>Sets the size of the vector to match an array.</summary>
      
<remarks>Sets the size (Length) of the caling vector to match the length of the array.
</remarks>
*)

    procedure SizeFromArray(const Src: Math387.T2DByteArray); overload;

    (*<summary>Sizes complex array.</summary>*)
    procedure SizeToArray(var Dst: TCplxArray); overload; 
    (*<summary>Sizes double precision array.</summary>*)
    procedure SizeToArray(var Dst: TDoubleArray); overload; 
    (*<summary>Size single precision array.</summary>*)
    procedure SizeToArray(var Dst: TSingleArray); overload; 
    (*<summary>Sizes 4 byte signed integer array.</summary>*)
    procedure SizeToArray(var Dst: TIntegerArray); overload; 

    (*<summary>Sizes 2 byte unsigned integer array.</summary>*)
    procedure SizeToArray(var Dst: Math387.TWordArray); overload; 
    (*<summary>Sizes 2 byte signed integer array.</summary>*)
    procedure SizeToArray(var Dst: TSmallIntArray); overload; 
    (*<summary>Sizes 1 byte unsigned integer array.</summary>*)
    procedure SizeToArray(var Dst: Math387.TByteArray); overload; 


    (*<summary>Resets any defined subrange.</summary>

      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange;

    (*<summary>Resets last defined subrange and pops the subrange stack.</summary>

      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRangeLevel;

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at
      Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted.

      All values of the original <see cref="MatrixInt"/> will be preserved.
      An exception will be raised if an attempt is made to
      change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the
      source object and all operations done on the either of the objects
      affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      Note
        To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(const Index: integer; const Len: integer); overload;
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
    procedure SetSubRangeLevel(const Index: integer; const Len: integer); overload;

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
    procedure SetSubRange(const Src: TMtxVecInt; const Index: integer; const Len: integer = MtxVecEOA); overload; 

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
    procedure SetSubRangeLevel(const Src: TMtxVecInt; const Index: integer; const Len: integer = MtxVecEOA); overload; 


    (*<summary>Sets the subarray size to full size.</summary>
      
<remarks>This method is the same as the <see cref="SetFullRange"/> method.
</remarks>
*)
    procedure SetSubRange; overload;

    (*<summary>Sets vector size to zero.</summary>
      
<remarks>Calling Clear sets <see cref="Length"/> property to 0.
</remarks>
*)
    procedure Clear;

    (*<summary>Saves the current value of ConditionCheck property and sets it to false.</summary>

      
<remarks>You can restore back to the original setting by calling <See Method="CondEnable"/> method.
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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,-2,3,4});
            a.Abs(); // a = [1,2,3,4]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
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
   function Abs(const Index, Len: integer): TMtxVecInt; overload;
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
   function Abs(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "and" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "and" of Src1 values with coresponding Src2 values
              stored in the calling object.</returns>*)
   function BinaryAnd(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "and" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "and" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryAnd(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "and"  of Src values with coresponding values in Self (this).</returns>*)
   function BinaryAnd(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "and" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in Src and Value.</summary>
     <returns>the result of binary "and" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
     <returns>the result of binary "and" between Value and
     values in the calling object.</returns>*)
   function BinaryAnd(const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in the calling object and Value.</summary>
     <returns>the result of binary "and" between Value and
     values in the calling object at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryAnd(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "and" between elements in Src and Value.</summary>
     <returns>the result of binary "and" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryAnd(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "and"  of Src1 values with coresponding Src2 values stored
              in the calling object.</returns>*)
   function BinaryOr(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "or" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "or" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryOr(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "or" of Src values with coresponding values in Self (this).</returns>*)
   function BinaryOr(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "or" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1].</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between elements in Src and Value.</summary>
     <returns>the result of binary "or" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "or" between elements in Src and Value.</summary>
     <returns>the result of binary "or" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryOr(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
     <returns>the result of binary "or" between Value and
     values in the calling object at locations [Index]..[Index+Len-1] stored
     back in the calling object.</returns>*)
   function BinaryOr(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "or" between elements in the calling object and Value.</summary>
     <returns>the result of binary "or" between Value and
     values in the calling object.</returns>*)
   function BinaryOr(const Value: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
     <returns>the result of binary "xor" between Value and  values in the calling object.</returns>*)
   function BinaryXor(const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in the calling object and Value.</summary>
     <returns>the result of binary "xor" between Value and
     values in the calling object at locations [Index]..[Index+Len-1] stored
     back in the calling object.</returns>*)
   function BinaryXor(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
     <returns>the result of binary "xor" of Value with coresponding
     values in Src stored in the calling object.</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between elements in Src and Value.</summary>
     <returns>the result of binary "xor" of Value with coresponding
     values in Src [SrcIndex]..[SrcIndex+Len-1] stored in the calling object
     at locations [Index]..[Index+Len-1].</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "xor" between coresponding elements in Src1 and Src2.</summary>
     <returns>the result of binary "xor" of Src1 values with coresponding Src2 values stored
              in the calling object.</returns>*)
   function BinaryXor(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary> Apply binary "xor" between coresponding elements in Src1 and Src2. </summary>
     <returns> the result of binary "xor" of Src1 values [SrcIndex1]..[SrcIndex1+Len-1]
     with coresponding Src2 values [SrcIndex2]..[SrcIndex2+Len-1] stored in the calling
     object at locations [Index]..[Index+Len-1]. </returns>*)
   function BinaryXor(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "xor" of Src values with coresponding values in Self (this).</returns>*)
   function BinaryXor(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply binary "xor" between coresponding elements in Src and Self.</summary>
     <returns>the result of binary "xor" of Src values [SrcIndex]...[SrcIndex+Len-1] with coresponding
     values in Self (this) [Index]..[Index+Len-1] stored back in the calling object.</returns>*)
   function BinaryXor(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left for all elements in the calling object.</summary>
     <returns>the result of binary shift to left by number of Bits applied to values in the calling object
              and stored back in the calling object.</returns>*)
   function BitShiftLeft(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in the calling object.</summary>
     <returns>the result of binary shift to left by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.</returns>*)
   function BitShiftLeft(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
     <returns>the result of binary shift to left by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].</returns>*)
   function BitShiftLeft(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left to elements in Src.</summary>
     <returns>the result of binary shift to left by number of Bits applied to Src values
              and stored in the calling object (Self).</returns>*)
   function BitShiftLeft(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right for all elements in the calling object.</summary>
     <returns>the result of binary shift to right by number of Bits applied to values in the calling object
              and stored back in the calling object.
              The sign of the numbers is preserved.</returns>*)
   function BitShiftRight(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right to elements in the calling object.</summary>
     <returns>the result of binary shift to right by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
     <returns>the result of binary shift to right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary shift by number of Bits to the right to elements in Src.</summary>
     <returns>the result of binary shift to right by number of Bits applied to Src values
              and stored in the calling object (Self).
              The sign of the numbers is preserved.
              </returns>*)
   function BitShiftRight(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) for all elements in the calling object.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to values in the calling object
              and stored back in the calling object.
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Bits: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in the calling object.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to values [Index]..[Index+Len-1]
              in the calling object and stored back in the calling object.
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Bits, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object (Self) value [Index]..[Index+Len-1].
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Src: TMtxVecInt; const Bits: integer; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Apply binary shift by number of Bits to the left (positive) or right (negative) to elements in Src.</summary>
     <returns>the result of binary shift to the left or right by number of Bits applied to Src values
              and stored in the calling object (Self).
              The sign of the numbers is preserved when doing right shift.
              </returns>*)
   function BitShift(const Src: TMtxVecInt; const Bits: integer): TMtxVecInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>If Src[i] &lt;&gt; 0 then bit at index "i" in the calling vector is set to 1.
              If Src.Length is not divisable with 32, the remaining bits in the last sample are left at 0.
              The storage precision of the calling object is set to prInt32. The size of the calling object
              is adjusted automatically. Packing if array with precision other than Int32 is 8x slower.
              Packing of array with precision other than Int32 is 8x slower.
</remarks>
*)

   function BitPack(const Src: TMtxInt): TMtxInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>Returns the result of bit packing the Src values [SrcIndex]..[SrcIndex+Len-1]
              and stored in the calling object  bits from [0]..[0+Len-1].
              If Len is not divisable with 32, the remaining bits in the last sample are left at 0.
              The storage precision of the calling object is set to prInt32.  The size of the calling object is adjusted automatically.
              Packing of array with precision other than Int32 is 8x slower.
</remarks>
*)
   function BitPack(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary> Converts Src to packed bit storage. </summary>
              
<remarks>If bit at Src[i] &lt;&gt; 0 then Integer (32, 16 or 8bit) at index "i" in the calling vector is set to 1.
              The storage precision of the calling object is preserved. The size of the calling object is adjusted
              automatically. Unpack to array with precision other than Int32 is 8x slower.
</remarks>
*)

   function BitUnpack(const Src: TMtxInt; const dstPrecision: TIntPrecision = prInt32): TMtxInt; overload;

   (*<summary> Converts Src to unpacked bit storage. </summary>
               
<remarks>If bit at Src[i] &lt;&gt; 0 then Integer (32, 16 or 8bit) at index "i" in the calling vector is set to 1.

              Returns the result of bit unpacking the bits stred Src values [0]..[Len-1]
              and stored in the calling object (Self) values [Index]..[Index+Len-1].
              The storage precision of the calling object is preserved. The Len parameter specifies the number
              of bits that will be unpacked from Src. Unpack to array with precision other than Int32 is 8x slower.
</remarks>
*)

   function BitUnpack(const Src: TMtxVecInt; SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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
          TMtxVecInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(new int[] {1,0,1,0});
            a.BinaryNot(); // a = [-2,-1,-2,-1]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function BinaryNot(): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to elements stored in Self (this).</summary>
     <returns>the result of binary "not" of values stored in Self (this).</returns>*)
   function BinaryNot(const Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values stored in Self (this).</returns>*)
   function BinaryNot(const Src: TMtxVecInt): TMtxVecInt; overload;

   (*<summary>Apply binary "not" to Src elements and store the result in Self (this).</summary>
     <returns>the result of binary "not" of Src values [SrcIndex]..[SrcIndex+Len-1] stored in Self (this) values [Index]..[Index+Len-1].</returns>*)
   function BinaryNot(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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

   

   
   
   function LoadFromStream(const SrcStream: TStream): Int64; overload;
   procedure SaveToStream(DstStream: TStream); overload;
   
   

   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The integer precision of the calling object is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+2*Len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TCplxArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Method uses the following rule:

      <code>Values[k] := Offset + k*Step.
      </code><para/>

      An exception is raised if calling vector
      array borders are overrun.
</remarks>
*)
     function Ramp(const Offset,Step: double; const Index,Len: integer): TMtxInt; overload;


   (*<summary>Copies values from Src array.</summary>
      
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
    function CopyFromArray(const Src: Math387.TWordArray; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Copies values from a real Src array.</summary>
      
<remarks>The size of the calling object is set implicitely.
      The precision is preserved.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the double precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed.
    If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TDoubleArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a real single precision floating point Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; const Rounding: TRounding): TMtxVecInt; overload; 
   (*<summary>Copies values from the single precision floating point Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSingleArray; const Rounding: TRounding; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from an integer type Src array.</summary>*)
    function CopyFromArray(const Src: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 4 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to the calling object values [Index]..[Index+len-1]. The size of the calling object is not changed.
      If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TIntegerArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from an 2 byte signed integer type Src array.</summary>
      
<remarks>The size property of the calling object is set implicitely.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 2 byte signed integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: TSmallIntArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array.</summary>*)
    function CopyFromArray(const Src: Math387.TByteArray): TMtxVecInt; overload; 
    (*<summary>Copies values from a 1 byte unsigned integer type Src array [SrcIndex]..[SrcIndex+Len-1] to the calling
    object values [Index]..[Index+len-1].</summary>
    
<remarks>The size of the calling object is not changed. If the array border are overrun an exception will be raised.
</remarks>
*)
    function CopyFromArray(const Src: Math387.TByteArray; const SrcIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
         to double precision numbers.</summary>
         
<remarks>Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to double precision floating point values. Consecutive elements are converted to real and imaginary parts.
</remarks>
*)
    function CopyToArray(var Dst: TCplxArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copy values to Dst array. The size of the array is set automatically.</summary>*)
    function CopyToArray(var Dst: TDoubleArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TDoubleArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies the calling object data to an array of single precision floating point data.</summary>
      
<remarks>Any values exceeding the range are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       single precision floating point array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
</remarks>
*)
    function CopyToArray(var Dst: TSingleArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 4 byte signed integer numbers.</summary>*)
    function CopyToArray(var Dst: TIntegerArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed. The method converts calling object values
       to 4 byte signed integers. Values exceeding the range of a 4 byte signed integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: TIntegerArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 2 byte signed integer numbers.</summary>
       
<remarks>Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: TSmallIntArray): TMtxVecInt; overload; 
    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of a 2 byte signed integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: TSmallIntArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 


    (*<summary>Copies values from the calling object to the Dst array and
               converts data to 2 byte unsigned integer type.</summary>
              
<remarks>Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)


    function CopyToArray(var Dst: Math387.TWordArray): TMtxVecInt; overload; 

    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of a 2 byte unsigned integer type are clipped.
</remarks>
*)

    function CopyToArray(var Dst: Math387.TWordArray; const DstIndex, Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Copies values from the calling object to the Dst array and converts data
       to 1 byte unsigned integer numbers.</summary>
       
<remarks>Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray): TMtxVecInt; overload; 

    (*<summary>Copy values from the calling object [Index]..[Index+len-1] to the Dst
       integer array at positions [DstIndex]...[DstIndex+Len-1].</summary>
       
<remarks>The size of the Dst array is not changed.
       Values exceeding the range of 1 byte unsigned integer type are clipped.
</remarks>
*)
    function CopyToArray(var Dst: Math387.TByteArray; const DstIndex,Index,Len: integer): TMtxVecInt; overload; 

    (*<summary>Finds a match for X in object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
</remarks>

      <returns>the index of last matched element. If no matching elements are found, the result is -1.</returns>*)

    function BinarySearch(const X: Integer): Integer; overload;
    function BinarySearch(const X: Integer; const Index: integer; const Len: integer): Integer; overload;

    (*<summary>Finds a closest match for X in object values using binary search.</summary>
               
<remarks>The data in the vector must be sorted in ascending order for this function to work correctly.
</remarks>

      <returns>True, if found and the index of the closest match in XIndex. </returns>*)

    function BinarySearch(const X: Integer; var XIndex: integer): boolean; overload;
    function BinarySearch(const X: Integer; var XIndex: integer; const Index: integer; Len: integer): boolean; overload;

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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(false, new int[] {2,5,1,6});
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
   function Find(const X: integer; const Index: integer; const Len: integer = MtxVecEOA): integer; overload;

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


    (*<summary>Finds the masks for a vector and splits it.</summary>
        
<remarks>The method splits the a vector in two vectors. The MaskVec will hold the those
        elements where the Op comparison between a and b is True.
        Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or  &lt;&gt; </c>.
        NotMaksVec will hold all those elements where the Op comparison between a and b is false.

        The calling vector will store the mask, 1 at those index locations where the Op comparison was True and 0 at those index locations
        where the Op comparison was false.
</remarks>


        <SeeAlso cref="Vector.FindAndGather"/>
        <SeeAlso cref="VectorInt.FindAndGather"/>
        <SeeAlso cref="FindIndexes"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="Vector.Gather"/>
        <SeeAlso cref="VectorInt.Gather"/>
        <SeeAlso cref="Scatter"/>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: TMtxVec; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
      (*<summary>The b parameter is of <see cref="TCplx"/> type.</summary>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: TCplx; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindAndSplit(const a: TMtxVec; const op: string; const b: double; const MaskVec, NotMaskVec: TVec): TMtxVecInt; overload;

      (*<summary>Finds a vector mask.</summary>
        
<remarks>The calling vector will hold the those
        elements where the Op comparison between a and b is True.
        Op string parameter can be <c> &lt; , &gt; , &gt;= , &lt;= , =  or  &lt;&gt; </c>.

        The calling vector will store the mask, 1 at those index locations
        where the Op comparison was True and 0 at those index locations
        where the Op comparison was false.
</remarks>


        <SeeAlso cref="Vector.FindAndGather"/>
        <SeeAlso cref="VectorInt.FindAndGather"/>
        <SeeAlso cref="FindAndSplit"/>
        <SeeAlso cref="FindIndexes"/>
        <SeeAlso cref="Find"/>
        <SeeAlso cref="Vector.Gather"/>
        <SeeAlso cref="VectorInt.Gather"/>
        <SeeAlso cref="Scatter"/>*)
      function FindMask(const a: TMtx; const op: string; const b: TCplx): TMtxVecInt; overload;
      (*<summary>The b parameter is of double type.</summary>*)
      function FindMask(const a: TMtx; const op: string; const b: double): TMtxVecInt; overload;
      (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
      function FindMask(const a: TMtxVec; const op: string; const b: TMtxVec): TMtxVecInt; overload;


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

    (*<summary>The b parameter is of <see cref="TMtxVec"/> type.</summary>*)
    function FindMask(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): TMtxVecInt; overload;


   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
     <returns>the result of adding Src2 values from coresponding Src1 values.</returns>*)
   function Add(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src2 to Src1.</summary>
    
<remarks>Add Src1 elements [Src1Index..Src1Index+Len-1] to corrresponding Src2 elements [Src2Index..Src12ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src1, Src2: TMtxVecInt; const Src1Index, Src2Index, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add Src elements to calling vector elements.</summary>*)
   function Add(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements.</summary>
    
<remarks>Add Src elements [SrcIndex..SrcIndex+Len-1] to calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to all Src elements.</summary>
      
<remarks>Add integer Value to all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Add integer Value to Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to all calling vector elements.</summary>*)
   function Add(const Value: integer): TMtxVecInt; overload;
   (*<summary>Add integer value to calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Add(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract coresponding elements in Src2 from Src1.</summary>
     <returns>the result of subtracting Src2 values from coresponding Src1 values.</returns>*)
   function Subtract(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Add coresponding elements in Src1 from Src1.</summary>
    
<remarks>Subtract Src2  elements [Src2Index..Src2Index+Len-1] from corrresponding Src1 elements [Src1Index..Src1ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src1, Src2: TMtxVecInt; const Src1Index, Src2Index, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src elements from calling vector elements.</summary>*)
   function Subtract(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements.</summary>
    
<remarks>Subtract Src elements [SrcIndex..SrcIndex+Len-1] from calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from all Src elements.</summary>
      
<remarks>Subtract integer Value from all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Subtract integer Value from Src elements [SrcIndex..SrcIndex+Len-1] and store the results
      in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from all calling vector elements.</summary>*)
   function Subtract(const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract integer value from calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Subtract(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Subtract all calling vector elements from integer Value.</summary>*)
   function SubtractFrom(const Value: integer): TMtxVecInt; overload;
   (*<summary>Subtract calling vector elements [Index..Index+Len-1] from integer Value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Subtract all Src vector elements from integer Value.</summary>
    
<remarks>and store thje results in calling vector. Size of calling vector is adjusted automatically.
</remarks>
*)
   function SubtractFrom(const Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Subtract Src vector elements [SrcIndex..SrcIndex+Len-1] from integer Value.</summary>
      
<remarks>and store thje results in calling vector elements [Index..Index+Len-1]. Size of calling vector is adjusted automatically.
      An exception is raised if array borders are overrun.
</remarks>
*)
   function SubtractFrom(const Value: integer; const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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

   (*<summary>Multiplies coresponding elements in Src2 with Src1.</summary>
     <returns>the result of multipliing Src2 values with coresponding Src1 values.</returns>*)
   function Multiply(const Src1, Src2: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Multiply coresponding elements in Src2 with Src1.</summary>
    
<remarks>Multiply Src1 elements [Src1Index..Src1Index+Len-1] with corrresponding Src2 elements [Src2Index..Src2ndex+Len]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src1, Src2: TMtxVecInt; const SrcIndex1, SrcIndex2,Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply Src elements with calling vector elements.</summary>*)
   function Multiply(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Mjultiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements.</summary>
    
<remarks>Multiply Src elements [SrcIndex..SrcIndex+Len-1] with calling vector elements
    [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer Value with all Src elements.</summary>
      
<remarks>Multiply integer Value with all Src elements and store the results in calling vector.
      Size of calling vector is adjusted automatically.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with Src vector elements [SrcIndex..SrcIndex+Len-1].</summary>
      
<remarks>Store the results in calling vector elements [Index..Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with all calling vector elements.</summary>*)
   function Multiply(const Value: integer): TMtxVecInt; overload;
   (*<summary>Multiply integer value with calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Multiply(const Value,Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divides coresponding elements in Num with Den.</summary>
     <returns>the result of dividing Num values with coresponding Den values.</returns>*)
   function Divide(const Num, Den: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide coresponding elements in Num with Den.</summary>
    
<remarks>Divide Num elements [NumIndex..NumIndex+Len-1] with corrresponding Deb elements [DenIndex..DenIndex+Len-1]
    and store the results in calling vector elements [Index..Index+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Num, Den: TMtxVecInt; const NumIndex, DenIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements.</summary>*)

   function Divide(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide calling object elements with Src elements [SrcIndex..SrcIndex+Len-1].</summary>
    
<remarks>Divide calling object elements  [Index..Index+Len ] with Src elements [SrcIndex..SrcIndex+Len-1].
    An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all Src vector elements with integer Value.</summary>
      
<remarks>Size of calling vector is adjusted automatically.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Src vector elements [SrcIndex..SrcIndex+Len-1] with integer Value.</summary>
    
<remarks>Divide Src elements [SrcIndex..SrcIndex+Len-1] with integer Value and store tje results in
    calling vector elements [Index..Index+Len-1]. An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Src: TMtxVecInt; const Value, SrcIndex, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide all calling vector elements with integer value.</summary>*)
   function Divide(const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide calling vector elements [Index..Index+Len-1] with integer value.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function Divide(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Divide Value with calling vector elements.</summary>
              
<remarks>Teh result is stored in the calling vector.
</remarks>
*)
   function DivideBy(const Value: integer): TMtxVecInt; overload;
   (*<summary>Divide Value with calling vector elements [Index..Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Divide Value with Src vector elements and store the result in the calling vector.</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value: integer; const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Divide Value with Src vector elements [SrcIndex+SrcIndex+Len-1] and store the result in the calling vector
   [Index...Index+Len-1].</summary>
              
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
   function DivideBy(const Value: integer; const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

    

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
      
<remarks>Copy each of Vec elements to the calling object. Size and precision
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
          TMtxVecInt a,b,c;
          a = new TMtxVecInt();
          b = new TMtxVecInt();
          a.SetIt(new int[] {1,2,3,4});  // a = [1,2,3,4] i.e 1+2i ; 3+4i
          b.Copy(a);                // b = [1,2,3,4]
        }
      }
      </code></example>*)
   function Copy(const Src: TMtxVecInt): TMtxVecInt; overload;
   (*<summary>Copy Src elements [SrcIndex]..[SrcIndex+Len-1] in the calling object
     elements [Index]..[Index+Len-1].</summary>
     
<remarks>Caèèing vector <see cref="Size"/> must be set explicitly. An exception is raised if array borders
     are overrun or underrun.
</remarks>
*)
   function Copy(const Src: TMtxVecInt; const SrcIndex, Index, Len: integer): TMtxVecInt; overload;

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
   function IsEqual(const Value: integer): boolean; overload;

   (*<summary>Compares Value with all calling object elements.</summary>*)
   function IsEqual(const Value, Index, Len: integer): boolean; overload;

   (*<summary>Compares two matrices.</summary>
    
<remarks>Compares Mtx with the calling matrix and returns true if the matrices are equal (if all elements match in position
    and value).
</remarks>


    <Example>
    <code>
    var A,B: TMtxInt;
      c: boolean;
    begin
      CreateIt(A,B);
      try
        A.SetIt(2,2,prInt32, [1,2,
                             2,4]);  // 2x2, int matrix
        B.SetIt(2,2,prInt32, [1,2,
                             2,4]);  // 2x2, real matrix
        c := A.Equal(B); // Check for an exact match
      finally
        FreeIt(A,B);
      end;
    end;
    </code>
    </Example>*)
    function Equal(const Mtx: TMtxInt): boolean; overload;

   (*<summary>Compares two objects.</summary>
      <returns>True, if they are equal.</returns>
      
<remarks>Compares two objects and returns True, if they are equal.

      The method compares <see cref="Length"/> and <see cref="IntPrecision"/> properties
      and coresponding values.
</remarks>
*)
   function IsEqual(const Vec: TMtxVecInt): boolean; overload;  
   (*<summary>Compares Vec elements [VecIndex]..[VecIndex+Len-1] with calling object
     elements [Index]..[Index+Len-1].</summary>*)
   function IsEqual(const Vec: TMtxVecInt; const VecIndex, Index, Len: integer): boolean; overload;  

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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
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
   function Max(const Index, Len: integer): integer; overload;

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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
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
   function Min(const Index, Len: integer): integer; overload;

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
   procedure MaxMin(out aMax, aMin: integer; const Index, Len: integer); overload;

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
          TMtxInt a;
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
   function Max(out aIndex: integer; const Index, Len: integer): integer; overload;

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
          TMtxInt a;
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
   function Min(out aIndex: integer; const Index, Len: integer): integer; overload;

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
   procedure MaxMinIdx(out aMax, aMaxIdx, aMin, aMinIdx: integer; const Index, Len: integer); overload;

   (*<summary>Applies median filter with size Mask to the calling vector elements [Index.. Index+Len-1].</summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the calling
            vector with the median value, calculated over the fixed range (mask) centered around that element.
</remarks>
*)
   function Median(const MaskSize, Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Applies median filter with size Mask to Src object elements [SrcIndex..SrcIndex+Len-1] and stores the result in the calling vector
                elements [Index..Index+Len-1]. </summary>
            
<remarks>Median filter is a nonlinear filter which replaces each element of the Src
            vector with the median value, calculated over the fixed range (mask) centered around that element
            and stores the result in the calling vector.
</remarks>
*)
   function Median(const Src: TMtxVecInt; const MaskSize, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Sizes the calling matrix to NewRows and NewCols.</summary>
      
<remarks>Copies the Src matrix values to the calling matrix.
      The elements outside the newly created matrix size are not referenced.
</remarks>
*)
   function Resize(const Src: TMtxInt; NewRows, NewCols: integer): TMtxInt; overload;

(*<summary>Resizes the matrix, while preserving the values in already allocated memory.</summary>
      
<remarks>Sets the <see cref="Rows"/> property to NewRows,
      <see cref="Cols"/> property to NewCols, while preserving the values in already allocated memory.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,3,
                       2,4]);
          B.Resize(A,3,2);
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
   function Resize(NewRows, NewCols: integer): TMtxInt; overload;

    (*<summary>Resize and transpose.</summary>
      
<remarks>Resizes the calling matrix to NewRows and NewCols and then copies
      the Src matrix values to the calling matrix. The elements outside
      the newly created matrix size are not referenced. At the end
      the method also transposes the Src matrix. All this operations
      are performed in one single pass.
</remarks>
*)
    function ResizeAndTranspose(const Src: TMtxInt; NewRows, NewCols: integer): TMtxInt;


   (*<summary>A cyclic shift on vector elements in range.</summary>
     
<remarks>Performs cyclic shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

     An exception is raised if array borders are overrun/underrun.
</remarks>
*)

   function Rotate(const Offset, Index, Len: integer): TMtxInt; overload;

   (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Shift"/>*)
   function Rotate(const Src: TMtxVecInt; const Offset, SrcIndex, Index, Len: integer): TMtxInt; overload;


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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
              a.SetIt(2,2, new int[] {1,2,3,4);
              a.Reverse();   // a = [2,1,3,4]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TMtxVecInt; const VecIndex, Index, Len: integer): TMtxVecInt; overload; 
    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(const Index, Len: integer): TMtxVecInt; overload;


    (*<summary>Divide all Src elements with Value elements and store result of division in the calling object and reminder in RemDst.</summary>
               
<remarks>Size and <see cref="IntPrecision"/> properties of the calling object and of RemDst are adjusted automatically.
</remarks>
*)
    function RemDiv(Src, Value: TMtxVecInt; RemDst: TMtxVecInt): TMtxVecInt; overload;

    (*<summary>Divide all Src elements with Value and store result of division in the calling object and reminder in RemDst.</summary>
                              
<remarks>Store the result of division in the calling object elements [Index]..[Index+Len-1]
                              and remainder in to [RemDstIndex] .. [RemDstIndex+ Len-1]. Size and <see cref="IntPrecision"/> properties of
                              the calling object and RemDst must be set explicitly. An exception is raised
                              if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
               if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
               if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
</remarks>
*)
    function RemDiv(Src: integer; Value: TMtxVecInt; RemDst: TMtxVecInt; ValueIndex, RemDstIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Write object header and values to a file.</summary>
    
<remarks>Write the header describing the calling object and the values array of the calling object to the file,
    specified by the FileName. If the file already exist, the data is overwritten by default. If Append is
    True, the data is appended to the end of the file. The data is always saved with IntPrecision precision.

    Note
      It is recommended you use a *.mtx extension when you're saving/loading matrix to/from file.
      Similarly, you can use a *.Vec extension when you're saving/loading vector to/from file.
</remarks>


    <Example>
    <code>
    var Mtx: TMtxInt;
    begin
      CreateIt(Mtx);
      try
        Mtx.SetIt(2,2,[3,1,-1,5]);
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
    procedure SaveToFile(const FileName: string; const Append: boolean = False);

    (*<summary>Reads the header information and the values array content from the file.</summary>
      
<remarks>Reads the header information and the values array content from the file specified by FileName parameter.
</remarks>

      <Example>
      <code>
      var b: TMtxInt;
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
    procedure LoadFromFile(const FileName: string);

    (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
    function ScatterByMask(const Src: TMtxVecInt; const Mask: TMtxVecInt; allow_resizing: boolean = False): TMtxVecInt; overload;

    (*<summary>Sets object values.</summary>
      
<remarks>Set object values. Method call does not change object's size, but it does
      check for array overrun. The elements of A array are copied to the calling
      object elements, starting at Index. The IntPrecision property of the calling
      object is not changed.
</remarks>
*)
    function SetIt(const Rows, Cols: integer; const A: array of integer): TMtxInt; overload;

    (*<summary>Sets matrix values.</summary>

      
<remarks>Pass all elements in A array to the calling matrix. The <see cref="Rows"/> and <see cref="Cols"/> properties of the calling
      matrix are set to ARows and ACols and the <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property is set to aPrecision.
      An exception is raised if the matrix size(ARows*ACols) does not match the number of integer numbers in A.
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

    function SetIt(const Rows, Cols: integer; aPrecision: TIntPrecision; const A: array of integer): TMtxInt; overload;

    (*<summary>Set all calling object elements to Value.</summary>*)

     function SetVal(const Value: integer): TMtxInt; overload;
    (*<summary>Set calling object elements [Index]..[Index+Len-1] to Value.</summary>
      
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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2,new int[] {1,2,3,4);
            a.SetVal(5,2,2);  // a = [1,1,5,5]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
   function SetVal(const Value, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Perform a shift on vector elements in range.</summary>
     
<remarks>Performs shift on vector elements in specified range [Index..Index+Len-1].
     The number of elements to shift is specified in the Offset parameter.
     Offset can be any integer number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function Shift(const Offset, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>A shift on vector elements in range.</summary>
      
<remarks>Performs a shift on source vector elements in specified range [SrcIndex .. SrcIndex+Len-1]
      and stores them to calling vector elements [Index .. Index+Len-1]. The number of elements to
      shift is specified in the Offset parameter. Offset can be any integer
      number, positive or negative.

      An exception is raised if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="Rotate"/>*)
   function Shift(const Src: TMtxVecInt; const Offset, SrcIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Set all calling vector elements to zero.</summary>*)
   function SetZero(): TMtxVecInt; overload;

   (*<summary>Set calling vector elements [Index..Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
   function SetZero(const Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort calling vector elements [Index..Index+Len-1] in ascending order in-place.</summary>*)
   function SortAscend(const Index, Len: integer): TMtxVecInt; overload;
   (*<summary>Sort all calling vector elements in ascending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      <param name="SortIdxIndex">The starting index for the DstSortIdx parameter.</param>
      <param name="Index">The starting index at which to start the sort.</param>
      <param name="Len">The number of elements to sort starting from including zero-based Index.</param>

      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)

   function SortAscend(const DstSortIdx: TMtxVecInt; const SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort calling vector elements [Index..Index+Len-1] in descending order in-place.</summary>*)
   function SortDescend(const Index, Len: integer): TMtxVecInt; overload;

   (*<summary>Sort all calling vector elements in descending order in-place.</summary>
      <returns>calling vector, storing sorted values.</returns>
      <param name="DstSortIdx">After execution stores sorted values indices.</param>
      <param name="SortIdxIndex">The starting index for the DstSortIdx parameter.</param>
      <param name="Index">The starting index at which to start the sort.</param>
      <param name="Len">The number of elements to sort starting from including zero-based Index.</param>

      
<remarks>Size of DstSortIdx and calling vector are adjusted automatically.
</remarks>
*)
   function SortDescend(const DstSortIdx: TMtxVecInt; const SortIdxIndex, Index, Len: integer): TMtxVecInt; overload;

    (*<summary>Sorts the elements in a matrix row(s) in ascending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in ascending order in-place. If the calling matrix is complex,
      then complex values are first compared by the absolute value and then by the argument.

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
    (*<summary>Performs row based sort in ascending order. The Col parameter defines the column based on which the Rows are compared with each other.</summary>*)
    function SortAscend(Col: integer): TMtxInt; overload; 

    (*<summary>Sorts the elements in a matrix row(s) in descending order.</summary>
      
<remarks>Sorts the elements in calling matrix row(s) in descending order in-place. If the calling matrix is complex,
      then complex values are first compared by the absolute value and then by the argument.<para/>

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

    (*<summary>Performs row based sort in descending order. The Col parameter defines the column based on which the Rows are compared with each other.</summary>*)
    function SortDescend(Col: integer): TMtxInt; overload; 

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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2,new int[] {1,3,-2});
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
   function Sum(const Index, Len: integer): integer; overload;

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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2, new int[] {2, 0, 3, 4});
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
      
<remarks>Store the results in calling object. Size and <see cref="VectorInt.IntPrecision"/>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="VectorInt.IntPrecision"/>
      properties of the calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/>
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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2,new int[] {2, 0, 3, 4});
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
      
<remarks>Store the results in calling object. Size and <see cref="VectorInt.IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TMtxVecInt; const Value: integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="VectorInt.IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2, int[] {2,-1,3,4});
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
      
<remarks>Store the results in calling object. Size and <see cref="VectorInt.IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsLT(const Src: TMtxVecInt; const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="VectorInt.IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
          TMtxInt a;
          MtxVec.CreateIt(out a);
          try
          {
            a.SetIt(2,2, int[] {2, -1, 3, 4});
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
      
<remarks>Store the results in calling object. Size and <see cref="VectorInt.IntPrecision"/> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshAbsGT(const Src: TMtxVecInt; const Value: Integer): TMtxVecInt; overload; 
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="VectorInt.IntPrecision"/> properties of the
      calling object must be set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
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
          TMtxInt a;
          MtxVecInt.CreateIt(out a);
          try
          {
            a.SetIt(2,2, new int[] {20,1,30,40});
            a.ThresholdGT_LT(23,34,10,5); // a = [20,5,34,34]
          }
          finally
          {
            MtxVecInt.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>
      <SeeAlso cref="ThreshBottom"/>*)
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
      set explicitly. An exception is raised if <see cref="TMtxVecBase.ConditionCheck" text="ConditionCheck"/> is true and array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TMtxVecInt; const GTLevel, GTValue, LTLevel, LTValue: Integer; VecIndex, Index, Len: integer): TMtxVecInt; overload; 



    (*<summary>Obtains a pointer to the integer value of the vector at Index.</summary>
      
<remarks>The function returns @Values[i]. Under .NET this is a pointer
      to unmanaged memory.
</remarks>
*)
   function PValues1D(const Index: integer): PAPointer;

 (*<summary>Read values content from stream to object.</summary>
      
<remarks>Reads values content from SrcStream stream to calling objct. No other values describing the data type or length are read
      from the DstStream. Number type is defined by the Precision parameter, which can be obtained from <see cref="ReadHeader"/> method
      call. The function returns the number of bytes read.

      Note
        Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TMtxInt;
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
      <SeeAlso cref="LoadFromStream"/>*)
    
    function ReadValues(const SrcStream: TStream; const aPrecision: TPrecision = prInteger): Int64; overload;
    

    


   
    (*<summary>Writes object Values content to a stream.</summary>
      
<remarks>Writes the calling object Values content to the DstStream stream. No other values describing the data type or length are written
      to the DstStream. Number type is defined by the Precision parameter, but only
      integer formats are accepted. Attempt to save single (float) precision or double precision will raise an exception.
      The paramateres must be the same as for the <see cref="WriteHeader"/> method.
      Use this method separately only, if you want user defined storage format.
</remarks>


      <Example>
      <code>
      var b: TMtxInt;
        AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt(2,2,[0,1,3,2]);
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
    procedure WriteValues(DstStream: TStream; const aPrecision: TPrecision = prInteger); overload;
    

    

    

    
    (*<summary>Writes the header information for the calling vector to a stream.</summary>
      
<remarks>Writes the header information for the calling object to a DstStream stream.
      The header information contains information about object (size, type of values
      in Values array, ...) which all define the state of the object. Number type is
      defined by the Precision parameter.
      Attempt to save single precision or double precision will raise an exception.
</remarks>


      <Example>
      <code>
      var b: TMtxInt;
          AStream: TFileStream;
      begin
        CreateIt(b);
        b.SetIt(2,2,[0,1,3,2]);
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
    <SeeAlso cref="WriteValues"/>*)
    procedure WriteHeader(DstStream: TStream; const aPrecision: TPrecision = prInteger); overload;
    

    

    
    (*<summary>Reads the header information from a stream to object.</summary>
      
<remarks>Reads the header information from a DstStream stream to calling object. The header information contains all necessary information
      defining the object. The function returns the precision in which the data was stored.
      This information is required for the <see cref="ReadValues"/> method.
</remarks>

      <Example>
      <code>
      var b: TMtxInt;
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
    function ReadHeader(const SrcStream: TStream): TPrecision; overload;
    

    (*<summary>Convert strings to integers and store them in the Values array.</summary>
      
<remarks>Convert strings in aList to an integer number and stores them in the Values array of the calling matrix.
      <see cref="Rows"/> property is set to aList.Count. <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> and <see cref="Cols"/> propertes
      are auto-detected. Complex numbers must follow the format: a+bi. All strings must have the same number of columns
      (numbers). Columns must be separated with a Delimiter. By default the delimiter is the tab charachter.
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
    function StringsToValues(const aList: TStrings; const Delimiter: string = kTab): TMtxInt; overload;

    (*<summary>Convert strings from [ListIndex]..[ListIndex+ListLen-1] in aList to integers.</summary>
      
<remarks>Store them in the Values array of the calling matrix, starting from element [Row,Cols]. The size of the calling matrix is not changed.
      If array bounds are overrun an exception is raised. Complex numbers must follow the format: a+bi.
</remarks>
*)
    function StringsToValues(const aList: TStrings; const ListIndex, ListLen, Row,Col: Integer; Delimiter: string = kTab): TMtxInt; overload;

    (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>
          
<remarks>Convert all elements of the calling matrix to strings with text delimiter Delimiter and store them in aList,
          by using the Add method of TStrings object.
          Set Align to have columns aligned left or right when using fixed width font (like Courier New).
          Specifiy Headers to be "true", if row and column labels are to be printed.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
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
    procedure ValuesToStrings(const dstList: TStrings; const Delimiter: string = kTab;
                                                     const Align: TFixedTextAlign = ftaNone;
                                                     const Headers: boolean = false); overload;

    (*<summary>Convert calling matrix elements, starting with [Row,Col] and up to [Row+RowCount-1,Col+ColCount-1] elements to strings.</summary>
      
<remarks>Use text Delimiter for columns and store the resulting strings in aList starting at ListIndex.
      If aList is not large enough, the method will use the Add method of aList object. Existing items will be overwritten.
      Set Align to have columns aligned left or right when using fixed width font (like Courier New).
      Specifiy Headers to be "true", if row and column labels are to be printed.

      Performance note:
        This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
        call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
    procedure ValuesToStrings(const dstList: TStrings; const ListIndex, Row, Col, RowCount, ColCount: integer;
                                                     const Delimiter: string = kTab;
                                                     const Align: TFixedTextAlign = ftaNone;
                                                     const Headers: boolean = false); overload;

    (*<summary>Converts all calling matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Delimiter: string = kTab);overload;

    (*<summary>Converts Row..Col to Row+RowCount..Col+ColCount matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab); overload;

    function TextToValues(Text: String; Delimiter: String = kTab): TMtxInt;

    (*<summary>Returns content as a string.</summary>
               
<remarks>Values will be separated by line breaks.
</remarks>
*)
    function ToString: string; 

    (*<summary>Parses string content as vector.</summary>
               
<remarks>Values need to be separated by line breaks.
               For other parsing options call StringToValues.
               The Parse method works in pair with the ToString method both meant to be a reverse of the other,
               but do not preserve any other property of the vector except only values.
               Use SaveToStream/LoadFromStream methods for binary storage option, which uses up to 10x less memory and is a lot faster to save and load.
</remarks>
*)
    class function Parse(const Src: string): MatrixInt; static;


    


    function DataIndex(aRow, aCol: integer; aPrecision: TIntPrecision): integer; overload;
    function DataIndex(aRow, aCol: integer): integer; overload;

    (*<summary>Sizes the matrix according to bit storage required.</summary>*)
    function BitSize(const bitRows, bitCols: integer): TMtxInt;


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
    function Size(const ARows, ACols: integer; const aPrecision: TIntPrecision): TMtxInt; overload;

    (*<summary>Specifies size of an integer matrix.</summary>
               
<remarks>The Value of IntPrecision property is preserved.
</remarks>
*)
    function Size(const ARows, ACols: integer): TMtxInt; overload;


    (*<summary>Size the object.</summary>
      
<remarks>Assigns the size of the Src object to the calling object and changes the IntPrecision to new value.
</remarks>
*)
    function Size(const Src: TMtxVecBase; const aPrecision: TIntPrecision): TMtxInt; overload;

    (*<summary>Size the object.</summary>
      
<remarks>Assigns the size of the Src object to the calling object.
      If both objects have a matching <see cref="Length"/>, <see cref="Rows"/>, <see cref="Cols"/> properties, only the <see cref="IntPrecision"/>
      property of the calling object will changed, while all other properties describing the size of the object
      will be preserved.
</remarks>
*)
    function Size(const Src: TMtxVecBase): TMtxInt; overload;


    (*<summary>Returns the 1D index in to the matrix.</summary>
               
<remarks>Returns: Row*Mtx.Cols + Col
</remarks>
*)
      function IndexOf(const Row, Col: integer): integer;

    (*<summary>Adopts a pointer to one dimensional array.</summary>
      
<remarks>Addopts a pointer to AArray array. The method sets the calling matrix <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property to IsComplex,
      <see cref="Rows"/> to ARows, <see cref="Cols"/> to ACols and Values1D and CValues1D to Pointer(AArray).

      Note
        You must call the <see cref="Disown"/> method, before you destroy the matrix. Use this method to
        save a copy operation. You should be very careful never to resize the calling matrix while it contains the
        adopted memory or call any routines that would require the matrix to reallocate the memory.
</remarks>


      <SeeAlso cref="Disown"/>*)

    
    procedure Adopt(const AArray: PAPointer; const ARows, ACols: integer; const aIntPrecision: TIntPrecision); overload;
    

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

    (*<summary>Permute the Rows of the Src matrix.</summary>
      
<remarks>The parameter PermuteIdx contains indexes of columns P[i] to which row at index "i" is to be moved.
      The result of the permutation is stored in the calling matrix. Only entire matrix can be copied.
</remarks>


      <SeeAlso cref="RowExchange"/>
      <SeeAlso cref="ColPermute"/>*)
    function RowPermute(const Src: TMtxInt; const PermuteIdx: TVecInt): TMtxInt; overload;

    (*<summary>Concatenate an array of matrices to single matrix.</summary>
      
<remarks>Concatenate an array of matrices to form one big matrix and store the result in the calling matrix. The dimensions
      of the block matrices in the Src array must match, to form the new matrix. The block matrices must be all real or
      all complex, otherwise an exception will be raised. You must specify Arows*ACols block matrices in the Src array.
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
      automatically. An exception is raised if any of the Src matrices Complex or Rows property does not match.
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
      if the calling matrix array bounds are overrun. An exception is raised, if any of the Src matrices Complex or Rows properties
      do not match.
</remarks>
*)
    function ConcatHorz(DestRow, DestCol: integer;const Src: array of TMtxInt): TMtxInt; overload;

    (*<summary>Concenates an array of matrices vertically.</summary>
      
<remarks>Concenate the Src matrices vertically and store the results in calling matrix. The <see cref="Rows"/>,
      <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling matrix are adjusted automatically.
      An exception is raised, if any of the Src matrices Complex or Cols properties do not match.
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
      Src matrices Complex or Cols properties do not match.
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
      ARows and ACols to matrix's <see cref="Rows"/> and <see cref="Cols"/> and IsComplex to matrix
      <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> property. Use the Disown method to "disconnect" AArray from the TMtx.Values1D.
      Disown sets Values1D and CValues1D array pointers to nil and Rows, Cols properties to ACols, but without freeing
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

    (*<summary>Constructs an eye matrix.</summary>
      
<remarks>Construct an eye matrix with ones only on main diagonal and zeros elsewhere.
      The number of Rows and columns of an eye matrix are set by ARows and ACols
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
      is performed on all calling matrix Rows.
</remarks>


      <Example>
      <code>
      var  A,B: TMtxInt;
      begin
        CreateIt(A,B);
        try
          A.SetIt(2,2,[1,2,
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
    function FlipHor: TMtxInt; overload;
    (*<summary>Flip all SrcMtx matrix elements horizontally.</summary>
      
<remarks>Store the results in the calling matrix. The flip operation is performed on
       all SrcMtx matrix Rows. The <see cref="Rows"/>, <see cref="Cols"/> and
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
    function LowerTriangle(const Mtx: TMtxInt; ZeroUpper, Diagonal: boolean): TMtxInt; overload;

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
      of Mtx1 and Mtx2 matrices. Mtx1 and Mtx2 Rows, Cols, and Complex properties must be the same, otherwise an excetion is raised.
      raised.
</remarks>
*)
    function MulElem(const Mtx1, Mtx2: TMtxInt): TMtxInt; overload;





































      (*<summary>Resets object properties to default values.</summary>
      
<remarks>Resets object properties to default values. The method is used by the Object cache management
      to reset the properties of the object freed with a call to <See Routine="MtxVecInt.FreeIt"/>.
</remarks>
*)
    procedure Reset; overload;

    (*<summary>Rotates matrix Rows 90 degrees clockwise.</summary>
      
<remarks>Rotates all calling matrix Rows 90 degrees clockwise in-place (check the scheme bellow).

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

    (*<summary>Rotate all Mtx matrix Rows 90deg clockwise.</summary>
      
<remarks>Store the results in calling matrix. The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVecInt.IntPrecision" text="IntPrecision"/> properties of the calling
      matrix are adjusted automatically.
</remarks>
*)
    function Rotate90(const Mtx: TMtxInt): TMtxInt; overload;

    (*<summary>Exchanges two matrix Rows.</summary>
      
<remarks>Exchange the i-th and j-th Rows of the calling matrix in-place. An exception is raised if matrix bounds are
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
          A.Size(2,1,True);
          V.SetIt([1,2, 2,4]);  // complex vector
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
      the number of Rows and columns of the block based matrix. The matrices passed in the dst array to be
      filled with values from the calling matrix, must already have matching size and complex properties to
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

    (*<summary>Calculates the sum of each of the calling matrix Rows.</summary>
      
<remarks>Calculates the sum of each of the calling matrix Rows and stores the results in Dst vector.
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
        A.Size(2,1,True); // 2x1 complex matrix
        A.SetVal(Cplx(3,4));
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
    function UpperTriangle(const Mtx: TMtxInt; ZeroLower, Diagonal: boolean): TMtxInt; overload;

  public
    (*<summary>Allows sizing the internal storage to accomodate desired number of bits.</summary>
               
<remarks>Use BitPack and BitUnpack for much faster access to bit data or Bits[i] property for individual
               bit access. Setting BitCount will also call the Size method. The Length of the internal arrays will be adjusted
               so that all bits can be stored with padding of the last (32bit, 16bit or 8bit integer) element.
               The value of BitCount, which is not an integer multiple of the IntPrecision is preserved only
               by certain methods where applicable and where it is not ambiguous. The value of the property affects
               only the results of BitPack and BitUnpack routines.
</remarks>
*)



    
    (*<summary>Allows reading/writing individual bits of the array.</summary>
               
<remarks>Use BitPack and BitUnpack for much faster access to bit data.
</remarks>
*)
    property Bits[const row, col: integer]: boolean read get_Bits write set_Bits;
    

    (*<summary> Returns data for inspection by the debugger. </summary>*)
    property IntegerValues: string read Get_IntegerValues;

    (*<summary> Specifies the power of two for the scale factor used by some methods.</summary>*)
    property ScaleFactor: integer read get_ScaleFactor write set_ScaleFactor;

    (*<summary>Defines number of bits stored in one row.</summary>*)
    property ColsBitCount: integer read Get_ColsBitCount write Set_ColsBitCount;

    (*<summary>Defines the number of leading columns.</summary>
      
<remarks>This value defines the spacing in number of samples between Rows.
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
    property Cols: integer read GetCols write SetCols;

    (*<summary>Defines the number of matrix Rows.</summary>
      
<remarks>Use this property to set or get the number of the calling matrix Rows. Setting the Rows does not affect the actual amount of
      memory allocated unless Rows*Cols is bigger than <see cref="TMtxVecBase.ActualSize"/>.
</remarks>



      <SeeAlso cref="Cols"/>
      <SeeAlso cref="Size"/>*)
    property  Rows: integer read GetRows write SetRows;
  end;











  (*<summary>Returns the abs of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Abs"/>
</remarks>
*)
  
  function Abs(const X: TVecInt): VectorInt; overload; 








  (*<summary>Subrange the source VectorInt without copy operation. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SetSubIndex"/>
</remarks>
*)
  
  function Select(const Src: TVecInt; const StartIndex, StopIndex: integer): VectorInt; overload; 

  (*<summary>Compute magnitude from the source.  </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Abs"/>
</remarks>
*)
  
  function Mag(const X: TVecInt): VectorInt; overload; 


  (*<summary>Returns the sum of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Sum"/>
</remarks>
*)
  
  function Sum(const X: TMtxVecInt): integer; overload; 


  (*<summary>Computes the Median from the source data. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Median"/>
</remarks>
*)
  
  function Median(const X: TMtxVecInt): double; overload; 


  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Max"/>
</remarks>
*)
  
  function Max(const X: TMtxVecInt): integer; overload; 
  (*<summary>Returns the maximum of the source data. </summary>
             
<remarks>Internally calls <see cref="Matrix.Max"/>
</remarks>
*)

  (*<summary>Returns the minimum of the source data. </summary>
             
<remarks>Internally calls <see cref="Matrix.Min"/>
</remarks>
*)
  
  function Min(const X: TMtxVecInt): integer; overload; 


  (*<summary>Returns the average value of the source data. </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Sum"/> and divides with vector length.
</remarks>
*)
  
  function Mean(const X: TMtxVecInt): double; overload; 


  (*<summary>Reverses the content of the source data. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Reverse"/>
</remarks>
*)
  
  function Reverse(const X: TVecInt): VectorInt; overload; 

  (*<summary>Rotates the content of the source data by Offset. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Rotate"/>
</remarks>
*)
  
  function Rotate(const X: TVecInt; const Offset: integer): VectorInt; overload; 

  (*<summary>Shifts the content of the source data X by Offset left or right. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Shift"/>
</remarks>
*)
  
  function Shift(const X: TVecInt; const Offset: integer): VectorInt; overload; 









  (*<summary>Returns a VectorInt of [0, 1, .., Len-1]  numbers. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Ramp"/>
</remarks>
*)
  
  function Ramp(const Len: integer): VectorInt; overload; 

  (*<summary>Returns a VectorInt of [Offset, Offset+Step, .., Offset+(Len-1)*Step] numbers. </summary>
             
<remarks>Internally calls <see cref="TVecInt.Ramp"/>
</remarks>
*)
  
  function Ramp(const Len: integer; const Offset, Step: double): VectorInt; overload; 

  (*<summary>Sorts the source data ascendingly. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SortAscend"/>
</remarks>
*)
  
  function SortAscend(const Src: TVecInt): VectorInt; overload; 

  (*<summary>Sorts the source data ascendingly. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SortAscend"/>
</remarks>
*)
  
  function SortAscend(const Src, IndexVec: TVecInt): VectorInt; overload; 

  (*<summary>Sorts the source data descendingly. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SortDescend"/>
</remarks>
*)
  
  function SortDescend(const Src: TVecInt): VectorInt; overload; 

  (*<summary>Sorts the source data descendingly. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SortDescend"/>
</remarks>
*)
  
  function SortDescend(const Src, IndexVec: TVecInt): VectorInt; overload; 

  (*<summary>Converts a list of strings to VectorInt. </summary>
             
<remarks>Internally calls <see cref="TVecInt.StringsToValues"/>
</remarks>
*)
  
  function StringsToValues(const aList: TStrings): VectorInt;  overload; 

  (*<summary>Converts a list of strings to VectorInt. </summary>
             
<remarks>Internally calls <see cref="TVecInt.StringsToValues"/>
</remarks>
*)
  
  function StringsToValues(const aList: TStrings; const ListIndex: integer; const Index: integer = 0; const Len: integer = MtxVecEOA): VectorInt; overload; 




  (*<summary>Returns the abs of elements in the source data. </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Abs"/>
</remarks>
*)
  
  function Abs(const X: TMtxInt): MatrixInt; overload; 








  (*<summary>Subrange the source MatrixInt without copy operation. </summary>
             
<remarks>Internally calls <see cref="TVecInt.SetSubIndex"/>
</remarks>
*)
  
  function Select(const Src: TMtxInt; const StartIndex, StopIndex: integer): VectorInt; overload; 

  (*<summary>Compute magnitude from the source.  </summary>
             
<remarks>Internally calls <see cref="TMtxVecInt.Abs"/>
</remarks>
*)
  
  function Mag(const X: TMtxInt): MatrixInt; overload; 


  (*<summary>Compute maximum of each row.</summary>
             
<remarks>Compute maximum of each row and store the result in to Dst VectorInt. The Dst is sized automatically.
</remarks>
*)
  
  function MaxRows(const Src: TMtxInt): VectorInt;
  (*<summary>Compute minimum of each row.</summary>
             
<remarks>Compute minimum of each row and store the result in to Dst VectorInt. The Dst is sized automatically.
</remarks>
*)
  
  function MinRows(const Src: TMtxInt): VectorInt;
  (*<summary>Compute maximum of each column.</summary>
             
<remarks>Compute maximum of each column and store the result in to Dst VectorInt. The Dst is sized automatically.
</remarks>
*)
  
  function MaxCols(const Src: TMtxInt): VectorInt;
  (*<summary>Compute minimum of each column.</summary>
             
<remarks>Compute minimum of each column and store the result in to Dst VectorInt. The Dst is sized automatically.
</remarks>
*)
  
  function MinCols(const Src: TMtxInt): VectorInt;

  (*<summary>Sort in ascending order all Rows.  </summary>
             
<remarks>Internally calls <see cref="TMtxInt.SortAscend"/>
</remarks>
*)
  
  function SortAscend(const Src: TMtxInt): MatrixInt; overload;

  (*<summary>Sort in ascending order all Rows from Src according to column Col.  </summary>
             
<remarks>Internally calls <see cref="TMtxInt.SortAscend"/>
</remarks>
*)
  function SortAscend(const Src: TMtxInt; const Col: integer): MatrixInt;  overload;

  (*<summary>Sort in descending order all Rows from Src.  </summary>
             
<remarks>Internally calls <see cref="TMtxInt.SortDescend"/>
</remarks>
*)

  function SortDescend(const Src: TMtxInt): MatrixInt;  overload;

  (*<summary>Sort in descending order all Rows from Src according to column Col.  </summary>
             
<remarks>Internally calls <see cref="TMtxInt.SortDescend"/>
</remarks>
*)
  function SortDescend(const Src: TMtxInt; const Col: integer): MatrixInt; overload;

  (*<summary>Converts a list of strings to MatrixInt. </summary>
             
<remarks>Internally calls <see cref="TMtxInt.StringsToValues"/>
</remarks>
*)
  
  function StringsToValues(const aList: TStrings; const Delimiter: string): MatrixInt; overload; 

  (*<summary>Converts a list of strings to MatrixInt. </summary>
             
<remarks>Internally calls <see cref="TMtxInt.StringsToValues"/>
</remarks>
*)
  
  function StringsToValues(const aList: TStrings; const ListIndex, ListLen, Row,Col: Integer; Delimiter: string = kTab): MatrixInt; overload; 

  
  
  (*<summary> Compute X + Y*yScale </summary>*)
  
  function AddScaled(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute X + Y*yScale </summary>*)
  
  function AddScaled(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute sqr(X + Y*yScale) </summary>*)
  
  function AddScaledSqr(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute sqr(X + Y*yScale) </summary>*)
  
  function AddScaledSqr(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute sqr(X) + sqr(Y)*yScale </summary>*)
  
  function SqrAddScaled(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute sqr(X) + sqr(Y)*yScale </summary>*)
  
  function SqrAddScaled(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute X + Y + Z </summary>*)
  
  function Add(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X + Y + Z </summary>*)
  
  function Add(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X + Y + Z*zScale </summary>*)
  
  function AddScaled(const X: TVecInt; const Y, Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X + Y + Z*zScale </summary>*)
  
  function AddScaled(const X: TMtxInt; const Y, Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;

  (*<summary> Compute X + Y*yScale + Z*zScale </summary>*)
  
  function AddScaled(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X + Y*yScale + Z*zScale </summary>*)
  
  function AddScaled(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;

  (*<summary> Compute X + Y + zScalar </summary>*)
  
  function Add(const X: TVecInt; const Y: TMtxVecInt; const Z: integer): VectorInt; overload;

  (*<summary> Compute X + Y + zScalar </summary>*)
  
  function Add(const X: TMtxInt; const Y: TMtxVecInt; const Z: integer): MatrixInt; overload;

  (*<summary> Compute X + Y*yScale + zScalar </summary>*)
  
  function AddScaledC(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer; const Z: integer): VectorInt; overload;

  (*<summary> Compute X + Y*yScale + zScalar </summary>*)
  
  function AddScaledC(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer; const Z: integer): MatrixInt; overload;  

  (*<summary> Compute X - Y - Z </summary>*)
  
  function Sub(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X - Y - Z </summary>*)
  
  function Sub(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X - Y*yScale - Z*zScale </summary>*)
  
  function SubScaled(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X - Y*yScale - Z*zScale </summary>*)
  
  function SubScaled(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;

  (*<summary> Compute X - Y - Z*zScale </summary>*)
  
  function SubScaled(const X: TVecInt; const Y, Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X - Y - Z*zScale </summary>*)
  
  function SubScaled(const X: TMtxInt; const Y, Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;

  (*<summary> Compute X - Y - zScalar </summary>*)
  
  function Sub(const X: TVecInt; const Y: TMtxVecInt; const Z: integer): VectorInt; overload;

  (*<summary> Compute X - Y - zScalar </summary>*)
  
  function Sub(const X: TMtxInt; const Y: TMtxVecInt; const Z: integer): MatrixInt; overload;

  (*<summary> Compute X - Y*yScale - zScalar </summary>*)
  
  function SubScaledC(const X: TVecInt; const Y: TMtxVecInt; const yScale: integer; const Z: integer): VectorInt; overload;

  (*<summary> Compute X - Y*yScale - zScalar </summary>*)
  
  function SubScaledC(const X: TMtxInt; const Y: TMtxVecInt; const yScale: integer; const Z: integer): MatrixInt; overload; 

  (*<summary> Compute X * Y / Z </summary>*)
  
  function MulAndDiv(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X * Y / Z </summary>*)
  
  function MulAndDiv(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X + Y)*Z </summary>*)
  
  function AddAndMul(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X + Y)*Z </summary>*)
  
  function AddAndMul(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X + Y)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X + Y)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X + Y*yScale)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TVecInt; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X + Y*yScale)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TMtxInt; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X + Y)*zScalar </summary>*)
  
  function AddAndMul(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute (X + Y)*zScalar </summary>*)
  
  function AddAndMul(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X + Y*yScale)*zScalar </summary>*)
  
  function AddAndMul(const X: TVecInt; const Y: TMtxVecInt; yScale, Z: integer): VectorInt; overload;

  (*<summary> Compute (X + Y*yScale)*zScalar </summary>*)
  
  function AddAndMul(const X: TMtxInt; const Y: TMtxVecInt; yScale, Z: integer): MatrixInt; overload;

  (*<summary> Compute (X + yScalar)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X + yScalar)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TMtxInt; Y: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*Z </summary>*)
  
  function AddAndMul(const X: TVecInt; xScale, Y: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*Z </summary>*)
  
  function AddAndMul(const X: TMtxInt; xScale, Y: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X + yScalar)*Z </summary>*)
  
  function AddAndMul(const X: TVecInt; Y: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X + yScalar)*Z </summary>*)
  
  function AddAndMul(const X: TMtxInt; Y: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X + yScalar)*zScalar </summary>*)
  
  function AddAndMul(const X: TVecInt; Y, Z: integer): VectorInt; overload;

  (*<summary> Compute (X + yScalar)*zScalar </summary>*)
  
  function AddAndMul(const X: TMtxInt; Y, Z: integer): MatrixInt; overload;  

  (*<summary> Compute (X - Y)*Z </summary>*)
  
  function SubAndMul(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X - Y)*Z </summary>*)
  
  function SubAndMul(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X - Y)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X - Y)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X - Y*yScale)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TVecInt; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X - Y*yScale)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TMtxInt; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X - Y)*zScalar </summary>*)
  
  function SubAndMul(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute (X - Y)*zScalar </summary>*)
  
  function SubAndMul(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X - Y*yScale)*zScalar </summary>*)
  
  function SubAndMul(const X: TVecInt; const Y: TMtxVecInt; yScale, Z: integer): VectorInt; overload;

  (*<summary> Compute (X - Y*yScale)*zScalar </summary>*)
  
  function SubAndMul(const X: TMtxInt; const Y: TMtxVecInt; yScale, Z: integer): MatrixInt; overload;

  (*<summary> Compute (X - yScalar)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X - yScalar)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TMtxInt; Y: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*Z </summary>*)
  
  function SubAndMul(const X: TVecInt; xScale, Y: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*Z </summary>*)
  
  function SubAndMul(const X: TMtxInt; xScale, Y: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X - yScalar)*Z </summary>*)
  
  function SubAndMul(const X: TVecInt; Y: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute (X - yScalar)*Z </summary>*)
  
  function SubAndMul(const X: TMtxInt; Y: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute (X - yScalar)*zScalar </summary>*)
  
  function SubAndMul(const X: TVecInt; Y, Z: integer): VectorInt; overload;

  (*<summary> Compute (X - yScalar)*zScalar </summary>*)
  
  function SubAndMul(const X: TMtxInt; Y, Z: integer): MatrixInt; overload;

  (*<summary> Compute X*Y + Z </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X*Y + Z </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X*Y*xyScale + Z </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X*Y*xyScale + Z </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X*Y + Z*zScale </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute X*Y + Z*zScale </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute X*Y + zScalar </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute X*Y + zScalar </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;  

  (*<summary> Compute X*Y - Z </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X*Y - Z </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X*Y*xyScale - Z </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X*Y*xyScale - Z </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X*Y - Z*zScale </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute X*Y - Z*zScale </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute X*Y - zScalar </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute X*Y - zScalar </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;

  (*<summary> Compute X/Y + Z </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X/Y + Z </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X/Y*xyScale + Z </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X/Y*xyScale + Z </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X/Y + Z*zScale </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute X/Y + Z*zScale </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute X/Y + zScalar </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute X/Y + zScalar </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;

  (*<summary> Compute X/Y - Z </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X/Y - Z </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X/Y*xyScale - Z </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X/Y*xyScale - Z </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X/Y - Z*zScale </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y, Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute X/Y - Z*zScale </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y, Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute X/Y - zScalar </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y: TMtxVecInt; Z: integer): VectorInt; overload;

  (*<summary> Compute X/Y - zScalar </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y: TMtxVecInt; Z: integer): MatrixInt; overload;

  (*<summary> Compute X*xScale + Y*yScale </summary>*)
  
  function AddScaled(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute X*xScale + Y*yScale </summary>*)
  
  function AddScaled(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute sqr(X*xScale + Y*yScale) </summary>*)
  
  function AddScaledSqr(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute sqr(X*xScale + Y*yScale) </summary>*)
  
  function AddScaledSqr(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale </summary>*)
  
  function SqrAddScaled(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): VectorInt; overload;

  (*<summary> Compute sqr(X)*xScale + sqr(Y)*yScale </summary>*)
  
  function SqrAddScaled(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer): MatrixInt; overload;

  (*<summary> Compute X*xScale + Y*yScale + Z*zScale </summary>*)
  
  function AddScaled(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X*xScale + Y*yScale + Z*zScale </summary>*)
  
  function AddScaled(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;  

  (*<summary> Compute X*xScale + Y*yScale + zScalar </summary>*)
  
  function AddScaledC(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): VectorInt; overload;

  (*<summary> Compute X*xScale + Y*yScale + zScalar </summary>*)
  
  function AddScaledC(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): MatrixInt; overload;

  (*<summary> Compute X*xScale - Y*yScale - Z*zScale </summary>*)
  
  function SubScaled(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): VectorInt; overload;

  (*<summary> Compute X*xScale - Y*yScale - Z*zScale </summary>*)
  
  function SubScaled(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: TMtxVecInt; const zScale: integer): MatrixInt; overload;

  (*<summary> Compute X*xScale - Y*yScale - zScalar </summary>*)
  
  function SubScaledC(const X: TVecInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): VectorInt; overload;

  (*<summary> Compute X*xScale - Y*yScale - zScalar </summary>*)
  
  function SubScaledC(const X: TMtxInt; const xScale: integer; const Y: TMtxVecInt; const yScale: integer; const Z: integer): MatrixInt; overload;  

  (*<summary> Compute X*Y*Z </summary>*)
  
  function Mul(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X*Y*Z </summary>*)
  
  function Mul(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X*Y*zScalar </summary>*)
  
  function Mul(const X: TVecInt; const Y: TMtxVecInt; const Z: integer): VectorInt; overload;

  (*<summary> Compute X*Y*zScalar </summary>*)
  
  function Mul(const X: TMtxInt; const Y: TMtxVecInt; const Z: integer): MatrixInt; overload;  

  (*<summary> Compute X / (Y*Z) </summary>*)
  
  function Divide(const X: TVecInt; const Y, Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X / (Y*Z) </summary>*)
  
  function Divide(const X: TMtxInt; const Y, Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X / (Y*zScale) </summary>*)
  
  function DivideC(const X: TVecInt; const Y: TMtxVecInt; const Z: integer): VectorInt; overload;

  (*<summary> Compute X / (Y*zScale) </summary>*)
  
  function DivideC(const X: TMtxInt; const Y: TMtxVecInt; const Z: integer): MatrixInt; overload;  

  (*<summary> Compute X * Y * xyScale / Z </summary>*)
  
  function MulAndDiv(const X: TVecInt; const Y: TMtxVecInt; const xyScale: integer; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute X * Y * xyScale / Z </summary>*)
  
  function MulAndDiv(const X: TMtxInt; const Y: TMtxVecInt; const xyScale: integer; const Z: TMtxVecInt): MatrixInt; overload;

  (*<summary> Compute X * Y / zScalar </summary>*)
  
  function MulAndDiv(const X: TVecInt; const Y: TMtxVecInt; const Z: integer): VectorInt; overload;

  (*<summary> Compute X * Y / zScalar </summary>*)
  
  function MulAndDiv(const X: TMtxInt; const Y: TMtxVecInt; const Z: integer): MatrixInt; overload;

  (*<summary> Compute xScalar / (Y*Z) </summary>*)
  
  function Divide(const X: integer; const Y: TVecInt; const Z: TMtxVecInt): VectorInt; overload;

  (*<summary> Compute xScalar / (Y*Z) </summary>*)
  
  function Divide(const X: integer; const Y: TMtxInt; const Z: TMtxVecInt): MatrixInt; overload;  

  (*<summary> Compute (X*xScale + Y*yScale)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale + Y*yScale)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TMtxInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale + Y*yScale)*zScalar </summary>*)
  
  function AddAndMul(const X: TVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale + Y*yScale)*zScalar </summary>*)
  
  function AddAndMul(const X: TMtxInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TVecInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*Z*zScale </summary>*)
  
  function AddAndMul(const X: TMtxInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*zScalar </summary>*)
  
  function AddAndMul(const X: TVecInt; xScale: integer; Y, Z: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale + yScalar)*zScalar </summary>*)
  
  function AddAndMul(const X: TMtxInt; xScale: integer; Y, Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale - Y*yScale)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale - Y*yScale)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TMtxInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale - Y*yScale)*zScalar </summary>*)
  
  function SubAndMul(const X: TVecInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale - Y*yScale)*zScalar </summary>*)
  
  function SubAndMul(const X: TMtxInt; xScale: integer; const Y: TMtxVecInt; yScale: integer; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TVecInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*Z*zScale </summary>*)
  
  function SubAndMul(const X: TMtxInt; xScale: integer; Y: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*zScalar </summary>*)
  
  function SubAndMul(const X: TVecInt; xScale: integer; Y, Z: integer): VectorInt; overload;

  (*<summary> Compute (X*xScale - yScalar)*zScalar </summary>*)
  
  function SubAndMul(const X: TMtxInt; xScale: integer; Y, Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*Y)*xyScale + Z*zScale </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*Y)*xyScale + Z*zScale </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*Y)*xyScale + zScalar </summary>*)
  
  function MulAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): VectorInt; overload;

  (*<summary> Compute (X*Y)*xyScale + zScalar </summary>*)
  
  function MulAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*yScalar) + zScalar </summary>*)
  
  function MulAndAdd(const X: TVecInt; Y, Z: integer): VectorInt; overload;

  (*<summary> Compute (X*yScalar) + zScalar </summary>*)
  
  function MulAndAdd(const X: TMtxInt; Y, Z: integer): MatrixInt; overload;

  (*<summary> Compute (X*Y)*xyScale - Z*zScale </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X*Y)*xyScale - Z*zScale </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X*Y)*xyScale - zScalar </summary>*)
  
  function MulAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale, Z: integer): VectorInt; overload;

  (*<summary> Compute (X*Y)*xyScale - zScalar </summary>*)
  
  function MulAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale, Z: integer): MatrixInt; overload;

  (*<summary> Compute X*yScalar - Z*zScale </summary>*)
  
  function MulAndSub(const X: TVecInt; Y: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute X*yScalar - Z*zScale </summary>*)
  
  function MulAndSub(const X: TMtxInt; Y: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X / Y)*xyScale + Z*zScale </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X / Y)*xyScale + Z*zScale </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X / Y) * xyScale + zScalar </summary>*)
  
  function DivAndAdd(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): VectorInt; overload;

  (*<summary> Compute (X / Y) * xyScale + zScalar </summary>*)
  
  function DivAndAdd(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): MatrixInt; overload;

  (*<summary> Compute (X / Y) * xyScale - Z y </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): VectorInt; overload;

  (*<summary> Compute (X / Y) * xyScale - Z y </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; const Z: TMtxVecInt; zScale: integer): MatrixInt; overload;

  (*<summary> Compute (X / Y) * xyScale - zScalar </summary>*)
  
  function DivAndSub(const X: TVecInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): VectorInt; overload;

  (*<summary> Compute (X / Y) * xyScale - zScalar </summary>*)
  
  function DivAndSub(const X: TMtxInt; const Y: TMtxVecInt; xyScale: integer; Z: integer): MatrixInt; overload;

  (*<summary> Copy each of Vec elements to the calling object. </summary>
              
<remarks>The Size property of the calling object is set implicitly to match Vec object.
              The storage precision of the calling object is set to DstIntPrecision.
</remarks>
*)
  
  function Convert(const Src: TVecInt; DstIntPrecision: TIntPrecision): VectorInt; overload;  

  (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and return bigger value as result.</summary> 
           
<remarks>Size and <see cref="TMtxVec.Complex"/> property of result object are adjusted automatically to match those of Vec1 and Vec2.
           An exception is raised if Vec1 and Vec2 size and <see cref="TMtxVecInt.IntPrecision"/> property do not match.
</remarks>
*)
  
  function MaxEvery(const Vec1: TVecInt; const Vec2: TMtxVecInt): VectorInt; overload;  

  (*<summary>Compare all Vec1 elements with corresponding Vec2 elements and return the smaller value as the result.</summary>*)
  
  function MinEvery(const Vec1: TVecInt; const Vec2: TMtxVecInt): VectorInt; overload; 

  (*<summary>Applies median filter with size Mask to data in Src and returns result.</summary>
                  
<remarks>Median filter is a nonlinear filter which replaces each element from Src vector with the median value, 
                  calculated over a fixed range (mask) centered around that element and returns this as result.
</remarks>
*)
  
  function Median(const Src: TVecInt; const MaskSize: integer): VectorInt; overload;   

  (*<summary>Fills the resulting vector elements with linear rule and randomly shuffles the indexes.</summary>
              
<remarks>Fills the resulting vector with a series following the rule:
   
              <code>
              Values[k] :=  k
              </code><para/>
   
              and then scrambles the contents of the vector randomly by using the Fisher–Yates shuffle.
</remarks>
*)
  
  function RandomRamp(Len: integer; aIntPrecision: TIntPrecision): VectorInt; overload;

  (*<summary>Randomly shuffles the content of the vector.</summary>
              
<remarks>Scrambles the contents of the vector randomly by using the Fisher–Yates shuffle.
</remarks>
*)
  
  function RandomShuffle(const Src: TVecInt): VectorInt; overload;

  (*<summary>Perform threshold operation on all Src object values.</summary>
              
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function ThreshBottom(const Src: TVecInt; const Value: integer): VectorInt; overload;

  (*<summary>Perform threshold operation on all Src object values.</summary>
              
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function ThreshTop(const Src: TVecInt; const Value: integer): VectorInt; overload;

  (*<summary>Perform threshold operation on all Src object values.</summary>
             
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function ThreshAbsLT(const Src: TVecInt; const Value: Integer): VectorInt; overload;

  (*<summary>Perform threshold operation on all Src object values.</summary>
              
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function ThreshAbsGT(const Src: TVecInt; const Value: Integer): VectorInt; overload;

  (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
              
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function ThresholdGT_LT(const Vec: TVecInt; const GTLevel, GTValue, LTLevel, LTValue: Integer): VectorInt; overload; 

  (*<summary>Calculate the cumulative sum for all Vec elements.</summary>
              
<remarks>Size and <see cref="VectorInt.IntPrecision"/> properties of the result are adjusted automatically.
</remarks>
*)
  
  function CumSum(const Vec: TVecInt): VectorInt; overload;

  (*<summary>Calculate the difference for all Vec elements.</summary>
              
<remarks>The <see cref="TMtxVecBase.Length" text="Length"/> of the result is set to one less than the Vec.length.
</remarks>
*)
  
  function Difference(const Vec: TVecInt; Lag: Integer = 1): VectorInt; overload;

  (*<summary>Copies a row from matrix.</summary>*)
  
  function GetRow(const Mtx: TMtxInt; Row: integer): VectorInt; overload;

  (*<summary>Copies the k-th diagonal from the TMtxInt object.</summary>*)
  
  function Diag(const Mtx: TMtxInt; k: integer): VectorInt;

  (*<summary> Scatters Src elements defined with the Mask to the calling object. </summary>*)
  
  function ScatterByMask(const Src: TVecInt; const Mask: TMtxVecInt): VectorInt; overload;   

  (*<summary> Gathers elements from X starting at Offset and with Increment in to the result. </summary>*)
  
  function GatherByIncr(const X: TVecInt; Increment: integer = 1; Offset: integer = 0): VectorInt; overload;

  (*<summary> Gathers elements from X specified by indices stored in Indexes in to the result. </summary>*)
  
  function GatherByIndex(const X: TVecInt; const Indexes: TVecInt): VectorInt; overload;

  (*<summary> Gathers elements from X specified by non-zero values stored in the Mask in to the result. </summary>*)
  
  function GatherByMask(const X: TVecInt; const Mask: TMtxVecInt; const MaskNonZeroCount: integer = -1): VectorInt; overload;  

  (*<summary> Copies a column from matrix.</summary>*)
  
  function GetCol(const Mtx: TMtxInt; Col: integer): VectorInt; overload;

  (*<summary>The b parameter is of integer type.</summary>*)
  
  function FindMask(const a: TMtxVecInt; const op: string; const b: integer): VectorInt; overload;

  (*<summary>The b parameter is of <see cref="TMtxVecInt"/> type.</summary> 
                
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".
</remarks>
*)
  
  function FindMask(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): VectorInt; overload;

  (*<summary>Fills the resulting vector with indexes, where the logical expression is true.</summary>  
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".  
              The b parameter is of integer type.
</remarks>
*)
  
  function FindIndexes(const a: TMtxVecInt; const op: string; const b: integer): VectorInt; overload;  

  (*<summary>Fills the resulting vector with indexes, where the logical expression is true.</summary>
               
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".  
              The b parameter is of <see cref="TMtxVecInt"/> type.
</remarks>
*)
  
  function FindIndexes(const a: TMtxVecInt; const op: string; const b: TMtxVecInt): VectorInt; overload;

  (*<summary>Fills the resulting vector with indexes, where the logical expression is true.</summary>
                
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".
</remarks>
*)
  
  function FindIndexes(const a: TMtxVec; const op: string; const b: TCplx): VectorInt; overload;

  (*<summary>Fills the resulting vector with indexes, where the logical expression is true.</summary> 
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".    
              The b parameter is of double type.
</remarks>
*)
  
  function FindIndexes(const a: TMtxVec; const op: string; const b: double): VectorInt; overload;

  (*<summary>Fills the resulting vector with indexes, where the logical expression is true.</summary>   
    
<remarks>The b parameter is of <See Class="TMtxVec"/> type.
</remarks>
*)
  
  function FindIndexes(const a: TMtxVec; const op: string; const b: TMtxVec): VectorInt; overload;  

  (*<summary>Finds and returns a vector mask.</summary>
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".
</remarks>
*)
  
  function FindMask(const a: TMtxVec; const op: string; const b: TCplx): VectorInt; overload;

  (*<summary>Finds and returns a vector mask.</summary>
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=". The b parameter is of double type.
</remarks>
*)
  
  function FindMask(const a: TMtxVec; const op: string; const b: double): VectorInt; overload;

  (*<summary>Finds and returns a vector mask.</summary>
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".  
              The b parameter is of <See Class="TMtxVec"/> type.
</remarks>
*)
  
  function FindMask(const a: TMtxVec; const op: string; const b: TMtxVec): VectorInt; overload;

  (*<summary>Finds the masks for a vector and splits it.</summary> 
                
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".  
                Use ScatterByMask to distribute MaskVec and NotMaskVec back to its location.
</remarks>
*)
  
  function FindAndSplit(const a: TMtxVec; const op: string; const b: TMtxVec; const MaskVec, NotMaskVec: TVec): VectorInt; overload;

  (*<summary>Finds the masks for a vector and splits it.</summary>  
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".
              The b parameter is of <see cref="TCplx"/> type. 
              Use ScatterByMask to distribute MaskVec and NotMaskVec back to its location.
</remarks>
*)
  
  function FindAndSplit(const a: TMtxVec; const op: string; const b: TCplx; const MaskVec, NotMaskVec: TVec): VectorInt; overload;

  (*<summary>Finds the masks for a vector and splits it.</summary>
              
<remarks>The op can be "&lt;", "&gt;", "=", "&lt;=","&gt;=".    
              The b parameter is of double type. 
              Use ScatterByMask to distribute MaskVec and NotMaskVec back to its location.
</remarks>
*)
  
  function FindAndSplit(const a: TMtxVec; const op: string; const b: double; const MaskVec, NotMaskVec: TVec): VectorInt; overload;

  (*<summary>Concatenates an array of TVecInt (VectorInt) objects and returns a combined array.</summary>*)
  
  function Concat(const Src: array of TVecInt): VectorInt; overload;

  (*<summary> Returns distinct values from Src. </summary>*)
  
  function Distinct(const Src: TVecInt): VectorInt;  





