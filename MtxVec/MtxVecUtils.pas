












(*<summary>Implements base methods for vector, matrix and sparse matrix.</summary>
  
<remarks>Implements base methods for vector, matrix and sparse matrix.
</remarks>
*)
unit MtxVecUtils;



interface

{$I BdsppDefs.inc}     

uses  Math387, IppsplTypes
     
      ,Ippspl
     

     
      ,ippsplSingle
     

     

   
      ,Classes
      ,SysUtils
    
        ,Registry
    



       ;




type
TRegistryFile = TRegIniFile;








  (*<summary>Reads values from stream and converts to double type.</summary>
    
<remarks>Reads values from stream and converts to double type. The values will be read from Stream,
    intepreted as of Precision type, converted
    to double and stored in the Values array. The routine will read Length samples.
    The length of the Values array must be set to Length*(byte(Complex)+1).
</remarks>


    <SeeAlso cref="WriteBinaryValues"/>*)
  
  
  function ReadBinaryValues(Stream: TStream; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: TStream; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  

  
  (*<summary>The source is a pointer to memory, which holds Length elements stored with Precision.</summary>*)
  
  function ReadBinaryValues(Stream: Math387.TByteArray; StreamIndex: integer; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: Math387.TByteArray; StreamIndex: integer; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;


  function ReadBinaryValues(Stream: PAPointer; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: PAPointer; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;


  (*<summary>Writes double values to stream and converts to specified type.</summary>
    
<remarks>Writes TSample values to stream and converts to specified type.
    The values from Values array will be converted to Precision type,
    using the Rounding specified and written to the Stream.
    The length of the Values arrays is defined with the Length and Complex
    parameters. If Complex is True, the length of the Values array
    must be two times bigger then the Value of the Length parameter.
</remarks>


    <SeeAlso cref="ReadBinaryValues"/>*)
  
  
    procedure WriteBinaryValues(Stream: TStream; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
    procedure WriteBinaryValuesc(Stream: TStream; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  

  

  (*<summary>The destination is a pointer to memory, which will hold Length elements stored with Precision.</summary>*)
  
  procedure WriteBinaryValues(Stream: Math387.TByteArray; StreamIndex: integer; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  procedure WriteBinaryValuesc(Stream: Math387.TByteArray; StreamIndex: integer; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;


  procedure WriteBinaryValues(Stream: PAPointer; Values: TDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  procedure WriteBinaryValuesc(Stream: PAPOinter; Values: TCplxDoubleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;


  (*<summary>Reads values from stream and converts to single precision type.</summary>
    
<remarks>Reads values from stream and converts to single precision type. The values will be read from Stream,
    intepreted as of Precision type, converted
    to double precision and stored in the Values array. The routine will read Length samples.
    The length of the Values array must be set to Length*(byte(Complex)+1).
</remarks>


    <SeeAlso cref="WriteBinaryValues"/>*)
  
  
  function ReadBinaryValues(Stream: TStream; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: TStream; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  

  
  (*<summary>The source is a pointer to memory, which holds Length elements stored with Precision.</summary>*)
  
  function ReadBinaryValues(Stream: Math387.TByteArray; StreamIndex: integer; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: Math387.TByteArray; StreamIndex: integer; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;


  function ReadBinaryValues(Stream: PAPointer; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;
  function ReadBinaryValuesc(Stream: PAPointer; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Endian: TEndianness; Length: integer): Int64; overload;


  (*<summary>Writes single precision values to stream and converts to specified type.</summary>
    
<remarks>Writes Tsingle precision values to stream and converts to specified type.
    The values from Values array will be converted to Precision type,
    using the Rounding specified and written to the Stream.
    The length of the Values arrays is defined with the Length and Complex
    parameters. If Complex is True, the length of the Values array
    must be two times bigger then the Value of the Length parameter.
</remarks>


    <SeeAlso cref="ReadBinaryValues"/>*)
  
  
    procedure WriteBinaryValues(Stream: TStream; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
    procedure WriteBinaryValuesc(Stream: TStream; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  

  

  (*<summary>The destination is a pointer to memory, which will hold Length elements stored with Precision.</summary>*)
  
  procedure WriteBinaryValues(Stream: Math387.TByteArray; StreamIndex: integer; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  procedure WriteBinaryValuesc(Stream: Math387.TByteArray; StreamIndex: integer; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;


  procedure WriteBinaryValues(Stream: PAPointer; Values: TSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;
  procedure WriteBinaryValuesc(Stream: PAPOinter; Values: TCplxSingleArray; ValuesIndex: integer; Precision: TPrecision; Rounding: TRounding; Endian: TEndianness; Length: integer); overload;


  (*<summary>Convert 24 bit signed integer to single precision value.</summary>
    
<remarks>Convert 24 bit signed integer to single precision value. The n parameter defines the number
    of elements to convert. Src points to a byte array and Dst is an array of single precision elements.
    DstIndex defines where in the Dst array will the elements be placed.
</remarks>


    <SeeAlso cref="FloatTo24Bit"/>*)
  
  procedure Bit24ToFloat(n: integer; Src: PPBArray; var Dst: array of single; DstIndex: integer = 0); overload;

  (*<summary>Convert 24 bit signed integer to double precision value.</summary>
    
<remarks>Convert 24 bit signed integer to double precision value. The n parameter defines the number
    of elements to convert. Src points to a byte array and Dst is an array of double precision elements.
    DstIndex defines where in the Dst array will the elements be placed.
</remarks>


    <SeeAlso cref="FloatTo24Bit"/>*)
  
  procedure Bit24ToFloat(n: integer; Src: PPBArray; var Dst: array of double; DstIndex: integer = 0); overload;

  (*<summary>Convert single precision to 24 bit signed integer.</summary>
    
<remarks>Convert single precision to 24 bit signed integer. The n parameter
    defines the number of elements to convert. Src is an
    array of single precision elements and Dst is a pointer to a byte array.
    Rounding parameter defines the rounding type used.
</remarks>


    <SeeAlso cref="Bit24ToFloat"/>*)
  
  procedure FloatTo24Bit(n: integer; var Src: array of single; Dst: PPBArray; Rounding: TRounding = rnRound); overload;

  (*<summary>Convert double precision to 24 bit signed integer.</summary>
    
<remarks>Convert double precision to 24 bit signed integer. The n parameter
    defines the number of elements to convert. Src is an
    array of double precision elements and Dst is a pointer to a byte array.
    Rounding parameter defines the rounding type used.
</remarks>


    <SeeAlso cref="Bit24ToFloat"/>*)
  
  procedure FloatTo24Bit(n: integer; var Src: array of double; Dst: PPBArray; Rounding: TRounding = rnRound); overload;







function ReadBinaryFromStream(SrcStream: TStream; Count: integer; out Dst: TSingleArray; Endian: TEndianness): boolean; overload;
function ReadBinaryFromStream(SrcStream: TStream; Count: integer; out Dst: TIntegerArray; Endian: TEndianness): boolean; overload;
function WriteBinaryToStream(DstStream: TStream; Count: integer; const Src: TSingleArray; Endian: TEndianness): boolean; overload;
function WriteBinaryToStream(DstStream: TStream; Count: integer; const Src: TIntegerArray; Endian: TEndianness): boolean; overload;










    procedure WriteToStream(const Stream: TStream; var Values: TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;

    procedure WriteToStreamc(const Stream: TStream; var Values: TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
    procedure WriteToStreamc(const Stream: TStream; var Values: TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;


    procedure WriteToStream(const Stream: TStream; const Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64); overload;
    procedure WriteToStream(const Stream: TStream; const Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64); overload;
    procedure WriteToStream(const Stream: TStream; var Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
    procedure WriteToStream(const Stream: TStream; var Values: TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
    procedure WriteToStream(const Stream: TStream; var Values: TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
    procedure WriteToStream(const Stream: TStream; var Values: TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
    procedure WriteToStream(const Stream: TStream; var Values: TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;



procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStreamc(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStreamc(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: Math387.TByteArray; StreamIndex: integer; Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;


procedure WriteToStream(Stream: PAPointer; Values: Math387.TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStreamc(Stream: PAPointer; Values: Math387.TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStreamc(Stream: PAPointer; Values: Math387.TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure WriteToStream(Stream: PAPointer; Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;






    function ReadFromStream(const Stream: TStream; const Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64): integer; overload;
    function ReadFromStream(const Stream: TStream; const Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64): integer; overload;
    function ReadFromStream(const Stream: TStream; var Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;
    function ReadFromStream(const Stream: TStream; var Values: TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;
    function ReadFromStream(const Stream: TStream; var Values: TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;

    function ReadFromStreamc(const Stream: TStream; var Values: TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;
    function ReadFromStreamc(const Stream: TStream; var Values: TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;


    function ReadFromStream(const Stream: TStream; var Values: TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;
    function ReadFromStream(const Stream: TStream; var Values: TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;
    function ReadFromStream(const Stream: TStream; var Values: TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness): integer; overload;



procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStreamc(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStreamc(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: Math387.TByteArray; StreamIndex: Integer; Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;

procedure ReadFromStream(Stream: PAPointer; Values: Math387.TSmallIntArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TIntegerArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TCardinalArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TShortIntArray; ValuesIndex: integer; NumBytes: Int64); overload;
procedure ReadFromStreamp(Stream: PAPointer; Values: PAPointer; ValuesIndex: integer; NumBytes: Int64); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStreamc(Stream: PAPointer; Values: Math387.TCplxSingleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStreamc(Stream: PAPointer; Values: Math387.TCplxDoubleArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TWordArray; ValuesIndex: integer; NumBytes: Int64; Endian: TEndianness); overload;
procedure ReadFromStream(Stream: PAPointer; Values: Math387.TByteArray; ValuesIndex: integer; NumBytes: Int64); overload;


procedure QuickSortDescMtx(const A: TDoubleArray;const B: TIntegerArray; iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TCplxArray; const B: TIntegerArray; const C: TDoubleArray; iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TDoubleArray; const B: TIntegerArray; const C: TDoubleArray; iLo, iHi: Integer; Index: integer); overload;
procedure QuickSortDescMtx(const A: TDoubleArray; AIndex: integer; const B: TIntegerArray; BIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TCplxArray; AIndex: integer; const B: TIntegerArray; BIndex: integer; const C: TDoubleArray; CIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TDoubleArray; AIndex: integer; const B: TIntegerArray; BIndex: integer; const C: TDoubleArray; CIndex, iLo, iHi: Integer; Index: integer); overload;

procedure QuickSortAscMtx(const A: TDoubleArray; const B: TIntegerArray; iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortAscMtx(const A: TCplxArray; const B: TIntegerArray; const C: TDoubleArray; iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAscMtx(const A: TDoubleArray; const B: TIntegerArray; const C: TDoubleArray; iLo, iHi: Integer; Index: integer); overload;
procedure QuickSortAscMtx(const A: TDoubleArray; AIndex: integer; const B: TIntegerArray; BIndex, iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAscMtx(const A: TCplxArray; AIndex: integer;
                           const B: TIntegerArray; BIndex: integer;
                           const C: TDoubleArray; CIndex: integer; iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAscMtx(const A: TDoubleArray; AIndex: integer;
                           const B: TIntegerArray; BIndex: integer;
                           const C: TDoubleArray; CIndex: integer; iLo, iHi: Integer; Index: integer); overload;


procedure QuickSortDescMtx(const A: TSingleArray;const B: TIntegerArray; iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TSCplxArray; const B: TIntegerArray;const C: TSingleArray; iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TSingleArray; const B: TIntegerArray;const C: TSingleArray; iLo, iHi: Integer; Index: integer); overload;

procedure QuickSortDescMtx(const A: TSingleArray; AIndex: integer; const B: TIntegerArray; BIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TSCplxArray; AIndex: integer; const B: TIntegerArray; BIndex: integer; const C: TSingleArray; CIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortDescMtx(const A: TSingleArray; AIndex: integer; const B: TIntegerArray; BIndex: integer; const C: TSingleArray; CIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortAscMtx(const A: TSCplxArray; const B: TIntegerArray; const C: TSingleArray; iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAscMtx(const A: TSingleArray; const B: TIntegerArray; const C: TSingleArray; iLo, iHi: Integer; Index: integer); overload;

procedure QuickSortAscMtx(const A: TSingleArray; const B: TIntegerArray; iLo, iHi: Integer; Index: integer); overload;
procedure QuickSortAscMtx(const A: TSingleArray; AIndex: integer; const B: TIntegerArray; BIndex, iLo, iHi: Integer; Index: integer); overload;
procedure CQuickSortAscMtx(const A: TSCplxArray; AIndex: integer;
                           const B: TIntegerArray; BIndex: integer;
                           const C: TSingleArray; CIndex: integer; iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAscMtx(const A: TSingleArray; AIndex: integer;
                           const B: TIntegerArray; BIndex: integer;
                           const C: TSingleArray; CIndex: integer; iLo, iHi: Integer; Index: integer); overload;

procedure CQuickSortAsc(const A: TCplxArray; AIndex: integer; const C: TDoubleArray; CIndex, iLo, iHi: Integer); overload;
procedure CQuickSortAsc(const A: TSCplxArray; AIndex: integer; const C: TSingleArray; CIndex, iLo, iHi: Integer); overload;



procedure QuickSortAsc(const A: TDoubleArray; iLo, iHi: Integer); overload;
procedure CQuickSortAsc(const A: TCplxArray; const C: TDoubleArray; iLo, iHi: Integer); overload;

procedure QuickSortAsc(const A: TSingleArray; iLo, iHi: Integer); overload;
procedure CQuickSortAsc(const A: TSCplxArray; const C: TSingleArray; iLo, iHi: Integer); overload;

procedure CQuickSortDesc(const A: TCplxArray; const C: TDoubleArray; iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TDoubleArray; const C: TDoubleArray; iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TCplxArray; AIndex: integer; const C: TDoubleArray; CIndex, iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TDoubleArray; AIndex: integer; const C: TDoubleArray; CIndex, iLo, iHi: Integer); overload;
procedure QuickSortDesc(const A: TDoubleArray; iLo, iHi: Integer); overload;

procedure CQuickSortDesc(const A: TSCplxArray; const C: TSingleArray; iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TSingleArray; const C: TSingleArray; iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TSCplxArray; AIndex: integer; const C: TSingleArray; CIndex, iLo, iHi: Integer); overload;
procedure CQuickSortDesc(const A: TSingleArray; AIndex: integer; const C: TSingleArray; CIndex, iLo, iHi: Integer); overload;
procedure QuickSortDesc(const A: TSingleArray; iLo, iHi: Integer); overload;



procedure WriteShortString(const Stream: TStream;const Str: string); overload;
function ReadShortString(const Stream: TStream): string; overload;
function ReadString(const Stream: TStream): string; overload;



function aRound(const X: double): integer; overload;
function aTrunc(const X: double): integer; overload;





