










(*<summary>High performance math functions.</summary>
  <remarks>
  Introduces high performance math functions. It can serve as replacement for the default
  Delphi Math.pas unit and offers full support for complex numbers. However, in many applications,
  you can obtain better performance with vector arithmetics.
  </remarks>*)
unit Math387;            

interface

{$I BdsppDefs.inc}

Uses
       
           SysUtils ,Classes, syncobjs

           
           ,Types
           

           
               
               ,Windows
               
           

           

           
           ,Contnrs
           

       
       ;
{$Z4}



type

  (*<summary>Default real number type used in Math387.</summary>
    
<remarks>Default real number type used in Math387. If single precision library is used TSample is of type
    <b>single</b>. If double precision library is used, TSample is of type <b>double</b>.
</remarks>
*)
     TSample = double;    
  

                      (*<summary>Signed 2 byte integer.</summary>
                        
<remarks>Signed 2 byte integer.
</remarks>
*)
                      TISample = SmallInt; 

















(*<summary>MtxVec version.</summary>*)
const MtxVecVersion = 635;
const MtxVecCoreMsg = 'MtxVec Core implementation of a needed function is missing! Please contact support@dewresearch.com for more information about availability!';
const DefaultIsDouble =  true ;


const MtxVecInfo = 'MtxVec 6.3.5';
const MKL_ALL = 0;
const MKL_BLAS = 1;
const MKL_FFT = 2;
const MKL_VML = 3;
const MKL_PARDISO = 4;
const MKL_LAPACK = 5;
const MediumBlockSize = 256*1024; 
const MtxVecDebuggerMarker = 999999111;




(*<summary>End of array constant.</summary>
  
<remarks>End of array constant.
</remarks>
*)
const MtxVecEOA = -1; 




const ippnamed2 = 'MtxVec.Vmld.6.3.dll';
const ippnamed = 'MtxVec.Dspd.6.3.dll';
const nmkpd2 = 'MtxVec.Lapackd.6.3.dll';
const mvspd2 = 'MtxVec.Lapackd.6.3.dll';

const ippnames2 = 'MtxVec.Vmls.6.3.dll';
const ippnames = 'MtxVec.Dsps.6.3.dll';
const nmkps2 = 'MtxVec.Lapacks.6.3.dll';
const mvsps2 = 'MtxVec.Lapacks.6.3.dll';

const vmlnamed = 'MtxVec.Vml.6.3.dll';
const vmlnames = 'MtxVec.Vml.6.3.dll';
const nmfft = 'MtxVec.Fft.6.3.dll';
const nmrnd = 'MtxVec.Random.6.3.dll';









type

   (*<summary>Specifies MtxVec processing precision.</summary>*)

    TMtxFloatPrecision = (mvSingle, mvDouble, mvSingleComplex, mvDoubleComplex);

   
       
       TMtxRect = TRect;
       TMtxPoint = TPoint;
       
   

  
  LongInteger = integer;
  
      
      PointerInteger = cardinal;
      
  
  

  
  TThreadID = Int64;
  

  
  PLongInteger = ^LongInteger;
  PPointerInteger = ^PointerInteger;
  

  

   (*<summary>Storage Endianness.</summary>
     
<remarks>Defines the order in which the bytes of data are (to be) stored in the file
     or in to a stream.
</remarks>
*)
  TEndianness = (
  (*<summary>Intel convention</summary>*)  boLittleEndian,
  (*<summary>Motorola convention</summary>*)  boBigEndian);

   (*<summary>Storage precision.</summary>
     
<remarks>Defines the precision in which the data is (to be) stored in the file
     or in to a stream.
</remarks>
*)
  TPrecision = (
  (*<summary>8 byte floating point</summary>*)prDouble,
  (*<summary>4 byte floating point</summary>*)prSingle,
  (*<summary>4 byte signed integer.</summary>*)prInteger,
  (*<summary>4 byte unsigned integer</summary>*)prCardinal,
  (*<summary>2 byte signed integer</summary>*)prSmallInt,
  (*<summary>2 byte unsigned integer</summary>*)prWord,
  (*<summary>1 byte signed integer</summary>*)prShortInt,
  (*<summary>1 byte unsigned integer</summary>*)prByte,
  (*<summary>1 byte dynamic floating point audio compression</summary>*)prMuLaw,
  (*<summary>1 byte dynamic floating point audio compression</summary>*)prALaw,
  (*<summary>3 byte signed integer</summary>*)prInt24);

  (*<summary>Default storage precision for integer vector and matrix values.</summary>*)
  TIntPrecision = (
  (*<summary>32 bit integers.</summary>*)prInt32,
  (*<summary>16 bit integers.</summary>*)prInt16,
  (*<summary>8 bit integers.</summary>*) prInt8
   );

  (*<summary>Specifies alignment for fixed width font number to string conversion.</summary>*)
  TFixedTextAlign = (
  (*<summary>No spaces will be appended or inserted.</summary>*)ftaNone,
  (*<summary>Spaces will be inserted in front of the text.</summary>*) ftaRightAlign,
  (*<summary>Spaces will be inserted in after the text.</summary>*) ftaLeftAlign);

  TMtxCompareOp = (
  (*<summary>Less than, smaller, &lt;  .</summary>*)  cmpLT,
  (*<summary>Less than or equal, &lt;=  .</summary>*) cmpLT_EQ,
  (*<summary>Greater than, &gt;  .</summary>*)  cmpGT,
  (*<summary>Greater than or equal, &gt;= .</summary>*) cmpGT_EQ,
  (*<summary>Equal, =, ==</summary>*)  cmpEQ,
  (*<summary>Not equal, &lt;&gt; , !=</summary>*)  cmpNot_EQ
  );


  (*<summary>A pointer to TSample type.</summary>
    
<remarks>A pointer to <see cref="TSample"/> type.
</remarks>
*)
  PSample = ^TSample;
  (*<summary>A pointer to integer type.</summary>
    
<remarks>A pointer to <see cref="Integer"/> type.
</remarks>
*)
  PInteger = ^Integer;
  (*<summary>A pointer to TISample type.</summary>
    
<remarks>A pointer to <see cref="TISample"/> type.
</remarks>
*)
  PSmallInt = ^SmallInt;
  (*<summary>A pointer to Word type.</summary>
    
<remarks>A pointer to <see cref="Word"/> type.
</remarks>
*)
  PWord = ^Word;
  (*<summary>A pointer to Byte type.</summary>
    
<remarks>A pointer to <see cref="Byte"/> type.
</remarks>
*)
  PByte = ^Byte;
  
  UInt = cardinal;
  

  (*<summary>A pointer to TCplx type.</summary>
    
<remarks>A pointer to <see cref="TCplx"/> type.
</remarks>
*)
  PCplx = ^TCplx;

  (*<summary>Default complex number type used by Math387.</summary>
    
<remarks>Default complex number type used by Math387.
</remarks>
*)
  
  
  TCplx = packed record 
        (*<summary>Real part of the complex number.</summary>*)
        Re: double;
        (*<summary>Imaginary part of the complex number.</summary>*)
        Im: double;
         
        
        (*<summary> Returns true if Imaginary component is zero. </summary>*)
        function IsComplex: boolean; inline;

        (*<summary> Equal operator for TCplx types. </summary>*)
        class operator Equal(const Left, Right: TCplx): Boolean; 
        (*<summary> Equal operator for mixed TCplx and real types. </summary>*)
        class operator Equal(const Left: TCplx; Right: double): Boolean; 
        (*<summary> Equal operator for mixed TCplx and real types. </summary>*)
        class operator Equal(Left: double; const Right: TCplx): Boolean; 

        (*<summary> Not equal operator for TCplx types. </summary>*)
        class operator NotEqual(const Left, Right: TCplx): Boolean; inline;
        (*<summary> Not equal operator for mixed TCplx and real types. </summary>*)
        class operator NotEqual(const Left: TCplx; Right: double): Boolean; inline;
        (*<summary> Not equal operator for mixed TCplx and real types. </summary>*)
        class operator NotEqual(Left: double; const Right: TCplx): Boolean; inline;

        (*<summary> Less than operator for TCplx types. </summary>*)
        class operator LessThan(const ALeft, ARight: TCplx): Boolean; inline;
        (*<summary> Less than operator for mixed TCplx and real types. </summary>*)
        class operator LessThan(const ALeft: TCplx; ARight: double): Boolean; inline;
        (*<summary> Less than operator for mixed TCplx and real types. </summary>*)
        class operator LessThan(ALeft: double; const ARight: TCplx): Boolean; inline;

        (*<summary> Less than or equal operator for TCplx types. </summary>*)
        class operator LessThanOrEqual(const ALeft, ARight: TCplx): Boolean; inline;
        (*<summary> Less than or equal operator for mixed TCplx and real types. </summary>*)
        class operator LessThanOrEqual(const ALeft: TCplx; ARight: double): Boolean; inline;
        (*<summary> Less than or equal operator for mixed TCplx and rela types. </summary>*)
        class operator LessThanOrEqual(ALeft: double; const ARight: TCplx): Boolean; inline;

        (*<summary> Greater than operator for TCplx types. </summary>*)
        class operator GreaterThan(const ALeft, ARight: TCplx): Boolean; inline;
        (*<summary> Greater than operator for mixed TCplx and real types. </summary>*)
        class operator GreaterThan(const ALeft: TCplx; ARight: double): Boolean; inline;
        (*<summary> Greater than operator for mixed TCplx and real types. </summary>*)
        class operator GreaterThan(ALeft: double; const ARight: TCplx): Boolean; inline;

        (*<summary> Greater than or equal operator for TCplx types. </summary>*)
        class operator GreaterThanOrEqual(const ALeft, ARight: TCplx): Boolean; inline;
        (*<summary> Greater than or equal operator for mixed TCplx and real types. </summary>*)
        class operator GreaterThanOrEqual(const ALeft: TCplx; ARight: double): Boolean; inline;
        (*<summary> Greater than or equal operator for mixed TCplx and real types. </summary>*)
        class operator GreaterThanOrEqual(ALeft: double; const ARight: TCplx): Boolean; inline;

        {$DEFINE D24}

        (*<summary> Add operator for TCplx types. </summary>*)
        class operator Add(const Left, Right: TCplx): TCplx;
        (*<summary> Add operator for mixed TCplx and real types. </summary>*)
        class operator Add(const Left: TCplx; Right: double): TCplx;
        (*<summary> Add operator for mixed TCplx and real types. </summary>*)
        class operator Add(Left: double; const Right: TCplx): TCplx;

        (*<summary> Subtract operator for TCplx types. </summary>*)
        class operator Subtract(const Left, Right: TCplx): TCplx;
        (*<summary> Subtract operator for mixed TCplx and real types. </summary>*)
        class operator Subtract(const Left: TCplx; Right: double): TCplx;
        (*<summary> Subtract operator for mixed TCplx and real types. </summary>*)
        class operator Subtract(Left: double; const Right: TCplx): TCplx;

        (*<summary> Multiply operator for TCplx types. </summary>*)
        class operator Multiply(const Left, Right: TCplx): TCplx;
        (*<summary> Multiply operator for mixed TCplx and real types. </summary>*)
        class operator Multiply(const Left: TCplx; Right: double): TCplx;
        (*<summary> Multiply operator for mixed TCplx and real types. </summary>*)
        class operator Multiply(Left: double; const Right: TCplx): TCplx;

        (*<summary> Divide operator for TCplx types. </summary>*)
        class operator Divide(const Left, Right: TCplx): TCplx;
        (*<summary> Divide operator for mixed TCplx and real types. </summary>*)
        class operator Divide(const Left: TCplx; Right: double): TCplx;
        (*<summary> Divide operator for mixed TCplx and real types. </summary>*)
        class operator Divide(Left: double; const Right: TCplx): TCplx;

        (*<summary> Negate operator for TCplx types. </summary>*)
        class operator Negative(const AValue: TCplx): TCplx; inline;

        (*<summary> Operator for implicit conversion of string to TCplx type. </summary>*)
        class operator Implicit(const AValue: string): TCplx; inline;

        (*<summary> Operator for explicit conversion of TCplx to real type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TCplx): Double; inline;
        (*<summary> Operator for explicit conversion of TCplx to integer type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TCplx): integer; inline;
        (*<summary> Operator for explicit conversion of TCplx to int64 type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TCplx): Int64; inline;

        (*<summary> Operator for explicit conversion of type double to TCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: double): TCplx; inline;
        (*<summary> Operator for explicit conversion of type single to TCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: single): TCplx; inline;
        (*<summary> Operator for explicit conversion of type integer to TCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: integer): TCplx; inline;
        (*<summary> Operator for explicit conversion of type int64 to TCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: Int64): TCplx; inline;

        
        
        property Complex: boolean read IsComplex;
        end;


  (*<summary>Default complex number type used by Math387.</summary>
    
<remarks>Default complex number type used by Math387.
</remarks>
*)
  
  TSCplx = packed record 
        (*<summary>Real part of the complex number.</summary>*)
        Re: single;
        (*<summary>Imaginary part of the complex number.</summary>*)
        Im: single;
         
        
        (*<summary> Returns true if Imaginary component is zero. </summary>*)
        function IsComplex: boolean; inline;

        (*<summary> Equal operator for TSCplx types. </summary>*)
        class operator Equal(const Left, Right: TSCplx): Boolean; 
        (*<summary> Equal operator for mixed TSCplx and real types. </summary>*)
        class operator Equal(const Left: TSCplx; Right: single): Boolean; 
        (*<summary> Equal operator for mixed TSCplx and real types. </summary>*)
        class operator Equal(Left: single; const Right: TSCplx): Boolean; 

        (*<summary> Not equal operator for TSCplx types. </summary>*)
        class operator NotEqual(const Left, Right: TSCplx): Boolean; inline;
        (*<summary> Not equal operator for mixed TSCplx and real types. </summary>*)
        class operator NotEqual(const Left: TSCplx; Right: single): Boolean; inline;
        (*<summary> Not equal operator for mixed TSCplx and real types. </summary>*)
        class operator NotEqual(Left: single; const Right: TSCplx): Boolean; inline;

        (*<summary> Less than operator for TSCplx types. </summary>*)
        class operator LessThan(const ALeft, ARight: TSCplx): Boolean; inline;
        (*<summary> Less than operator for mixed TSCplx and real types. </summary>*)
        class operator LessThan(const ALeft: TSCplx; ARight: single): Boolean; inline;
        (*<summary> Less than operator for mixed TSCplx and real types. </summary>*)
        class operator LessThan(ALeft: single; const ARight: TSCplx): Boolean; inline;

        (*<summary> Less than or equal operator for TSCplx types. </summary>*)
        class operator LessThanOrEqual(const ALeft, ARight: TSCplx): Boolean; inline;
        (*<summary> Less than or equal operator for mixed TSCplx and real types. </summary>*)
        class operator LessThanOrEqual(const ALeft: TSCplx; ARight: single): Boolean; inline;
        (*<summary> Less than or equal operator for mixed TSCplx and rela types. </summary>*)
        class operator LessThanOrEqual(ALeft: single; const ARight: TSCplx): Boolean; inline;

        (*<summary> Greater than operator for TSCplx types. </summary>*)
        class operator GreaterThan(const ALeft, ARight: TSCplx): Boolean; inline;
        (*<summary> Greater than operator for mixed TSCplx and real types. </summary>*)
        class operator GreaterThan(const ALeft: TSCplx; ARight: single): Boolean; inline;
        (*<summary> Greater than operator for mixed TSCplx and real types. </summary>*)
        class operator GreaterThan(ALeft: single; const ARight: TSCplx): Boolean; inline;

        (*<summary> Greater than or equal operator for TSCplx types. </summary>*)
        class operator GreaterThanOrEqual(const ALeft, ARight: TSCplx): Boolean; inline;
        (*<summary> Greater than or equal operator for mixed TSCplx and real types. </summary>*)
        class operator GreaterThanOrEqual(const ALeft: TSCplx; ARight: single): Boolean; inline;
        (*<summary> Greater than or equal operator for mixed TSCplx and real types. </summary>*)
        class operator GreaterThanOrEqual(ALeft: single; const ARight: TSCplx): Boolean; inline;

        (*<summary> Add operator for TSCplx types. </summary>*)
        class operator Add(const Left, Right: TSCplx): TSCplx;
        (*<summary> Add operator for mixed TSCplx and real types. </summary>*)
        class operator Add(const Left: TSCplx; Right: single): TSCplx;
        (*<summary> Add operator for mixed TSCplx and real types. </summary>*)
        class operator Add(Left: single; const Right: TSCplx): TSCplx;

        (*<summary> Subtract operator for TSCplx types. </summary>*)
        class operator Subtract(const Left, Right: TSCplx): TSCplx;
        (*<summary> Subtract operator for mixed TSCplx and real types. </summary>*)
        class operator Subtract(const Left: TSCplx; Right: single): TSCplx;
        (*<summary> Subtract operator for mixed TSCplx and real types. </summary>*)
        class operator Subtract(Left: single; const Right: TSCplx): TSCplx;

        (*<summary> Multiply operator for TSCplx types. </summary>*)
        class operator Multiply(const Left, Right: TSCplx): TSCplx;
        (*<summary> Multiply operator for mixed TSCplx and real types. </summary>*)
        class operator Multiply(const Left: TSCplx; Right: single): TSCplx;
        (*<summary> Multiply operator for mixed TSCplx and real types. </summary>*)
        class operator Multiply(Left: single; const Right: TSCplx): TSCplx;

        (*<summary> Divide operator for TSCplx types. </summary>*)
        class operator Divide(const Left, Right: TSCplx): TSCplx;
        (*<summary> Divide operator for mixed TSCplx and real types. </summary>*)
        class operator Divide(const Left: TSCplx; Right: single): TSCplx;
        (*<summary> Divide operator for mixed TSCplx and real types. </summary>*)
        class operator Divide(Left: single; const Right: TSCplx): TSCplx;

        (*<summary> Negate operator for TSCplx types. </summary>*)
        class operator Negative(const AValue: TSCplx): TSCplx; inline;

        (*<summary> Operator for implicit conversion of string to TSCplx type. </summary>*)
        class operator Implicit(const AValue: string): TSCplx; inline;
        (*<summary> Operator for explicit conversion of string to TSCplx type. </summary>*)
        class operator Explicit(const AValue: TCplx): TSCplx; inline;
        (*<summary> Operator for explicit conversion of string to TSCplx type. </summary>*)
        class operator Explicit(const AValue: TSCplx): TCplx; inline;

        (*<summary> Operator for explicit conversion of TSCplx to real type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TSCplx): Double; inline;
        (*<summary> Operator for explicit conversion of TSCplx to integer type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TSCplx): integer; inline;
        (*<summary> Operator for explicit conversion of TSCplx to int64 type. </summary>
                     
<remarks>Imaginary value is ignored.
</remarks>
*)
        class operator Explicit(const AValue: TSCplx): Int64; inline;

        (*<summary> Operator for explicit conversion of type double to TSCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: double): TSCplx; inline;
        (*<summary> Operator for explicit conversion of type single to TSCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: single): TSCplx; inline;
        (*<summary> Operator for explicit conversion of type integer to TSCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: integer): TSCplx; inline;
        (*<summary> Operator for explicit conversion of type int64 to TSCplx type. </summary>
                     
<remarks>Imaginary value is set to 0.
</remarks>
*)
        class operator Explicit(const AValue: Int64): TSCplx; inline;

        
        
        property Complex: boolean read IsComplex;
        end;


  
  (*<summary>Base notify event.</summary>*)
  TMtxNotifyEvent = TNotifyEvent;
  

  (*<summary>Array of string.</summary>*)
  
      
      StringArray = TStringDynArray;
      
  
  T2DStringArray = array of StringArray;

  (*<summary>Complex number type for integer real and complex part.</summary>
    
<remarks>Complex number type for integer real and complex part.
</remarks>
*)
  TICplx = packed record
        (*<summary>Real part</summary>*)
        Re: TISample;
        (*<summary>Imaginary part</summary>*)
        Im: TISample;
        end;

  (*<summary>Complex number type for integer real and complex part.</summary>
    
<remarks>Complex number type for integer real and complex part.
</remarks>
*)
  TIntegerCplx = packed record
        (*<summary>Real part</summary>*)
        Re: integer;
        (*<summary>Imaginary part</summary>*)
        Im: integer;
        end;

  (*<summary>Complex number type for integer real and complex part.</summary>
    
<remarks>Complex number type for integer real and complex part.
</remarks>
*)
  TSmallIntCplx = packed record
        (*<summary>Real part</summary>*)
        Re: integer;
        (*<summary>Imaginary part</summary>*)
        Im: integer;
        end;

  (*<summary>Complex number type for double real and complex part.</summary>
    
<remarks>Complex number type for double real and complex part.
</remarks>
*)
  TDCplx = packed record
      (*<summary>Real part</summary>*)
      Re: double;
      (*<summary>Imaginary part</summary>*)
      Im: double;
      end;

  
  PDCplx = ^TDCplx;
   PPDCplx = PDCplx; 
  









  
  PSCplx = ^TSCplx;
   PPSCplx = PSCplx; 

  TSArray = array [0..200000000] of TSample; 
  PSArray = ^TSArray;     
   PPSArray = PSArray; 

  TDArray = array [0..200000000] of Double; 
  PDArray = ^TDArray;     
   PPDArray = PDArray; 

  TFArray = array [0..200000000] of Single; 
  PFArray = ^TFArray;     
   PPFArray = PFArray; 

  TCArray = array [0..20000000] of TCplx;
  PCArray = ^TCArray;
   PPCArray = PCArray; 

  TDCArray = array [0..20000000] of TDCplx;
  PDCArray = ^TDCArray;
   PPDCArray = PDCArray; 

  TSCArray = array [0..20000000] of TSCplx;
  PSCArray = ^TSCArray;
   PPSCArray = PSCArray; 

  TIArray = array [0..200000000] of integer;
  PIArray = ^TIArray;
   PPIArray = PIArray; 

  TRArray = array [0..200000000] of Cardinal;
  PRArray =  ^TRArray;
   PPRArray = PRArray; 

  TWArray = array [0..200000000] of Word;
  PWArray = ^TWArray;
   PPWArray = PWArray; 

  TSIArray = array [0..200000000] of SmallInt;
  PSIArray = ^TSIArray;
   PPSIArray = PSIArray; 

  TBArray = array [0..200000000] of byte;
  PBArray = ^TBArray;
   PPBArray = PBArray; 

  TCharArray = array [0..200000000] of char;
  PCharArray = ^TCharArray;
   PPCharArray = PCharArray; 

  THArray = array [0..200000000] of ShortInt;
  PHArray = ^THArray;
   PPHArray = PHArray; 

  TDCplxArray = array of TDCplx;
  TSCplxArray = array of TSCplx;
  
      
      TDoubleArray = array of Double;
      TSingleArray = array of Single;
      TIntegerArray = array of integer;
      
  
  PDoubleArray = ^TDoubleArray;
  PSingleArray = ^TSingleArray;
  TBooleanArray = array of boolean;
  TInt64Array = array of Int64;
  TUInt64Array = array of UInt64;
  TIntegerCplxArray = array of integer;
  PIntegerArray = ^TIntegerArray;
  TPointerIntegerArray = array of PointerInteger;
  TCardinalArray = array of cardinal;
  TWordArray = array of Word;
  TShortIntArray = array of ShortInt;
  TSmallIntArray = array of SmallInt;
  TSmallIntCplxArray = array of TSmallIntCplx;
  TByteArray = array of byte;
  TObjectArray = array of TObject;
  TCharsArray = array of char;
  TThreadIDArray = array of TThreadID;

  

  (*<summary>Array of TSample type.</summary>
    
<remarks>Array of <see cref="TSample"/> type.
</remarks>
*)
  
  TSampleArray = TDoubleArray;
  

  (*<summary>Array of TCplx type.</summary>
    
<remarks>Array of <see cref="TCplx"/> type.
</remarks>


    *)
  TCplxArray = array of TCplx;

  

  TCplxSingle = TSCplx;
  TCplxDouble = TCplx;
  TCplxSingleArray = TSCplxArray;
  TCplxDoubleArray = TCplxArray;
  

  TSmallInTCplxSampleArray = array of SmallInt;

  (*<summary>Array of Integer type.</summary>
    
<remarks>Array of <see cref="Integer"/> type.
</remarks>


    *)
  TIntArray = TIntegerArray;

  (*<summary>Array of Integer type.</summary>
    
<remarks>Array of <see cref="Integer"/> type.
</remarks>


    *)
  T2DIntegerArray = array of TIntegerArray;

  (*<summary>Array of SmallInt type.</summary>
    
<remarks>Array of <see cref="SmallInt"/> type.
</remarks>


    *)
  T2DSmallIntArray = array of TSmallIntArray;

  (*<summary>Array of SmallInt type.</summary>
    
<remarks>Array of <see cref="SmallInt"/> type.
</remarks>


    *)
  T2DByteArray = array of TByteArray;

  (*<summary>Two dimensional array of TSample type.</summary>
    
<remarks>Two dimensional array of <see cref="TSample"/> type.
</remarks>


    *)
  T2DSingleArray = array of TSingleArray;
  T2DDoubleArray = array of TDoubleArray;
  T2DCplxDoubleArray = array of TCplxDoubleArray;
  T2DCplxSingleArray = array of TCplxSingleArray;

  (*<summary>Two dimensional array of TCplx type.</summary>
    
<remarks>Two dimensional array of <see cref="TCplx"/> type.
</remarks>


    *)
  T2DCplxArray = array of TCplxArray;

  (*<summary>Two dimensional array of TSCplx type.</summary>
    
<remarks>Two dimensional array of <see cref="TSCplx"/> type.
</remarks>


    *)
  T2DSCplxArray = array of TSCplxArray;


  


  PCplxArray = ^TCplxArray;
  PIntArray = ^TIntArray;

  TBytec = record
    Re, Im: byte;
    end;

  PBytec = ^TBytec;

  TWordc = record
    Re, Im: word;
    end;

  PWordc = ^TWordc;

  TCardinalc = record
    Re, Im: cardinal;
    end;

  PCardinalc = ^TCardinalc;

  TShortintc = record
    Re, Im: ShortInt;
    end;

  PShortintc = ^TShortIntc;

  TSmallintc = record
    Re, Im: SmallInt;
    end;

  PSmallintc = ^TSmallIntc;

  TIntegerc = record
    Re, Im: cardinal;
    end;

  PIntegerc = ^TIntegerc;

  TInt64c = record
    Re, Im: Int64;
    end;

  PInt64c = ^TInt64c;
	

  


  

	
  Int32 = Integer;
  

  (*<summary>Pointer to char.</summary>*)
  PAPChar = PChar;
  (*<summary>Pointer to char.</summary>*)
  PAPAnsiChar =  PAnsiChar;

  (*<summary>Generic pointer type.</summary>*)
  PAPointer = Pointer;
  (*<summary>Pointer to complex byte.</summary>*)
  PPBytec = PBytec;
  (*<summary>Pointer to complex word.</summary>*)
  PPWordc = PWordc;
  (*<summary>Pointer to complex unsigned integer.</summary>*)
  PPCardinalc = PCardinalc;
  (*<summary>Pointer to complex signed short (1 byte) integer.</summary>*)
  PPShortIntc = PShortintc;
  (*<summary>Pointer to complex signed small (2 byte) integer.</summary>*)
  PPSmallIntc = PSmallIntc;
  (*<summary>Pointer to complex signed (4 byte) integer.</summary>*)
  PPIntegerc = PIntegerc;
  (*<summary>Pointer to complex 8 byte floating point type.</summary>*)
  PPCplx = PCplx;
  (*<summary>Pointer to complex long (8 byte) integer.</summary>*)
  PPInt64c = PInt64c;

  (*<summary>Pointer to byte</summary>*)
  PPByte = PByte;
  (*<summary>Pointer to 2 byte unsigned integer (word).</summary>*)
  PPWord = PWord;
  (*<summary>Pointer to 4 byte unsigned integer.</summary>*)
  PPCardinal = ^Cardinal;
  (*<summary>Pointer to 1 byte signed integer.</summary>*)
  PPShortInt = ^ShortInt;
  (*<summary>Pointer to 2 byte signed integer.</summary>*)
  PPSmallInt = ^SmallInt;

  PPPointerInteger = PPointerInteger;
  PPLongInteger = PLongInteger;
  (*<summary>Pointer to 4 byte signed integer.</summary>*)
  PPInteger = System.PInteger;
  (*<summary>Pointer to TSample.</summary>*)
  PPSample = PSample;
  (*<summary>Pointer to signed 8 byte integer.</summary>*)
  PPInt64 = System.PInt64;
  (*<summary>Pointer to 4 byte floating point.</summary>*)
  PPSingle = System.PSingle;
  (*<summary>Pointer to 8 byte floating point.</summary>*)
  PPDouble = System.PDouble;
  (*<summary>Pointer to byte.</summary>*)
  PPUChar = System.PByte;
  

  TPointerArray = array of PAPointer;

type
    (*<summary>Type used to define indexation type.</summary>*)
    TIndexType = (
    (*<summary>Incremented value.</summary>*)indIncrement,
    (*<summary>Vector of indices.</summary>*)indVector,
    (*<summary>Mask vector.</summary>*)indMask);

  (*<summary>The waiting method used by the sorted critical section.</summary>*)
  TCRWaitMode = (
    (*<summary>The critical section will use sleep(0) to wait. (Default)
    Use for computational threads with max-thread count less than hyper-threaded core count.</summary>*)
    crwSleep0 = 0,
    (*<summary>The critical section will enter sleep(1) to wait.
    Use when cooperative multi-threading is used.
    Number of launched threads executing concurrently is more than hyper-threaded core count
    or working with I/O devices.</summary>*)
    crwSleep1 = 1,
    (*<summary>The critical section will call SwitchToThread to wait.</summary>*)
    crwSwitchThread = 2);

  (*<summary>Enables threads in a multithreaded application a single-thread only exclusive execution of a part of the code.</summary>

     
<remarks>Critical sections in general safeguard memory access operations in a piece of code, which might cause corruption,
     if they are executed concurrently by multiple threads at the same time. They are used always when there is a need
     to share resource, like for example memory. Once a thread enters a critical section, by calling the Enter method,
     all other threads will wait at the "Enter" entry, until "Leave" is called. At this point the next thread
     (one at a time), is allowed to continue past the "Enter" entry.

     The sorted critical section implementation is a FIFO design (First-In-First-Out) and will pass threads strictly
     in the order in which they arrive. When there are many (8, 16 or more) threads all passing through the same critical section,
     the ability to guarantee the order in which they pass, leads to substantial performance improvements.
     The Windows OS standard critical section implementations (up until year 2022) throw a dice each time
     a thread enters sleep, because the sleep time is not deterministic. This can lead to starvation of some
     threads, which might never get to "Enter", while others have entered the protected code section
     multiple times already.

     If each thread needs to allocate 2 pieces of memory, and one thread is starved for this, but all threads need to
     finish to finish the job, then the execution time of multi-threaded code will be extended proportionally.

     Important:
     TFiFoCriticalSection performance will deliver best results, when the launched thread count within the process does not
     exceed hyper-threaded thread count of the system. Threads, which exceed this number, will sleep until one of the executing
     threads enters sleep or exits (Windows OS kernel as-designed feature). If launched thread count is very high. this can lead to (uneccessary),
     wakes of threads, which otherwise sleep anyway, to find the right one to continue the execution.
</remarks>
*)
  TFifoCriticalSection = class
  strict private
    EnterCounter: integer;
    ExecCounter: integer;
  public
   (*<summary>Tries to enter without locking.</summary>
              
<remarks>Checks, if the critical sections thread waiting queue is empty and enters immediately.

              Important: Always need to call "Leave" before calling "Enter" or trying to enter on the same critical section by the same thread again.
</remarks>
*)
    function TryEnter: boolean;

   (*<summary>Enter the critical section.</summary>
    
<remarks>Guarantees the sequence of threads that pass through, to match the order in which they called Enter.

    Important: Always need to call "Leave" before calling "Enter" on the same critical section by the same thread again.
</remarks>
*)
    procedure Enter;

   (*<summary>Leave the critical section.</summary>
    
<remarks>Guarantees the sequence of threads that pass through, to match the sequence in which they called Enter.

    Important: Always need to call "Leave" before calling "Enter" on the same critical section by the same thread again.
</remarks>
*)
    procedure Leave;

   (*<summary>Initialize the critical section. The default value for aWaitMode is TCRWaitMode.crwSleep.</summary>
              
<remarks>Initialization/Constructor is required. It is not enough to set all fields to zero.
</remarks>
*)
    constructor Create; virtual;
  end;

  (*<summary>Critical section with semaphore capability.</summary>
            
<remarks>This class allows multiple threads to run concurrently within a critical section.
            The maximum number of such threads will be limited to "Count".

            The threads will enter strictly in the order in which they arrive, but they may exit in any order depending
            on the workload assigned to each thread within the critical section.

            If all threads require a shared resource, this construct allows the programmer to limit the number of threads
            to the number of available shared resource(s).
</remarks>
*)

  TFairSemaphoreSection = class
  strict private
      EnterCounter: integer;
      ExecCounter: integer;
      MinCounter: integer;
      FCount: integer;
      Slots: TThreadIDArray;
  public
     (*<summary>Specifies the number of threads that can enter concurrently.</summary>*)
      property Count: integer read FCount;

     (*<summary>Enter the critical section.</summary>
      
<remarks>Guarantees the sequence of threads that pass through, to match the order in which they called Enter.
      The function will return the index of the free shared resource. The index will start at 0 and end at Count - 1 and
      is guaranteed to be unusued by any thread currently executing within the critical section.

      The Count property of this class specifies the number of threads that can enter the semaphored critical section concurrently.

      Call "Leave" before calling "Enter" on the same critical section by the same thread again.
</remarks>
*)
      function Enter: integer;

     (*<summary>Leave the critical section.</summary>

      
<remarks>Call "Leave" before calling "Enter" on the same critical section by the same thread again.
</remarks>
*)
      procedure Leave; overload;
     (*<summary>Leave the critical section.</summary>

      
<remarks>The SharedResourceIndex parameter must be the same what was returned by the Enter method when called by the same thread.
      This is the index of the shared resource to be released.

      Call "Leave" before calling "Enter" on the same critical section by the same thread again.
</remarks>
*)
      procedure Leave(const SharedResourceIndex: integer); overload;

     (*<summary>Initialize the critical section. </summary>
                
<remarks>The call of this constructor is required.
</remarks>
*)
      constructor Create(ResourceCount: integer); virtual;
  end;

  (*<summary>Enables threads in a multithreaded application a single-thread only exclusive execution of a part of the code.</summary>
             
<remarks>The record needs to be initialized with a call to init. The order in which the threads will pass is not deterministic.
</remarks>
*)

  TSimpleCriticalSection = record
  public
     (*<summary>Initializes the critical section, if the constructor was not used.</summary>*)

      class procedure Init(var csLock: integer); overload; static;
     (*<summary>Enter the critical section.</summary>
                
<remarks>The order in which the threads will pass is not deterministic.
                The waiting loop is using Sleep(0).
</remarks>
*)

      class procedure Enter(var csLock: integer; const TrueValue: integer = 1); overload; static;
     (*<summary>Enter the critical section.</summary>
                
<remarks>The order in which the threads will pass is not deterministic.
</remarks>
*)

      class function TryEnter(var csLock: integer; const TrueValue: integer = 1): boolean; overload; static;
     (*<summary>Leave the critical section.</summary>
                
<remarks>The order in which the threads will pass is not deterministic.
</remarks>
*)

      class procedure Leave(var csLock: integer); overload; static;
  end;



type

    (*<summary>Type used to define the storage format for FFT operations.</summary>
      
<remarks>Defines the storage format used when performing forward
      and inverse FFT's. The result of a forward FFT operation is
      an array of complex numbers. Notation:
      * Re(0) is the real part of the complex number stored at index 0.
      * Im(0) is the imaginary part of the complex number stored at index 0.

      The length of the FFT is N and d = N/2. N is an even value. For these
      parameters the complex to complex FFT returns the following complex array:
  	  <code>
      (Re(0), Im(0)), (Re(1), Im(1)),...,(Re(d), Im(d)), ... , (Re(N-1), Im(N-1))
      </code>
      If the source was real, the real to complex FFT computes only the first half of that
      table, namely:
      <code>
      (Re(0), Im(0)), (Re(1), Im(1)),...,(Re(d), Im(d))
      </code>
      That is because beyond index d, all elements are conjugate
      symmetric part of elements below index d. For example, it is possible
      to compute the upper half of the array doing operation's like this:
      <c>(Re(d+1), Im(d+1)) = (Re(d-1), - Im(d-1))</c><para/>

      The storage format that stores the FFT result as presented here is called the CCS format.

      A speciallity of the FFT result is that Im(0) and Im(d)
      are always zero. Because these two values are implicitely known
      in advance, they do not have to be stored. This is exploited
      by two other storage formats: pack and perm. The pack format
      simply leaves out the zeros and pushes the elements together
      and the perm format stores the Re(d) value at the position of Im(0)
      and pushes the elements together.

      If the length of the FFT N is odd, then everything remains the same,
      except that the center element at index d is absent. This means that
      pack and perm have the same storage format in this case.

    For a 2D FFT similar rules apply and the storage formats
    layout is as follows:

    <b>CCS format, m x n matrix</b>

    Even length (m = s*2)
    <code>
    Re(1,1)     0         Re(1,2)     Im(1,2)     ... Re(1,k)     Im(1,k)     Re(1,k+1)     0
    0           0         0           0           ... 0           0           0             0
    Re(2,1)     Re(2,2)   Re(2,3)     Re(2,4)     ... Re(2,n-1)   Re(2,n)     n/u         n/u
    Im(2,1)     Im(2,2)   Im(2,3)     Im(2,4)     ... Im(2,n-1)   Im(2,n)     n/u         n/u
    ...         ...       ...         ...         ... ...         ...         n/u         n/u
    Re(m/2,1)   Re(m/2,2) Re(m/2,3)   Re(m/2,4)   ... Re(m/2,n-1) Re(m/2,n)   n/u         n/u
    Im(m/2,1)   Im(m/2,2) Im(m/2,3)   Im(m/2,4)   ... Im(m/2,n-1) Im(m/2,n)   n/u         n/u
    Re(m/2+1,1) 0         Re(m/2+1,2) Im(m/2+1,2) ... Re(m/2+1,k) Im(m/2+1,k) Re(m/2+1,k+1) 0
    0           0         0           0           ... 0           0           n/u         n/u
    </code>

    Odd length (m = s*2+1)
    <code>
    Re(1,1)     0           Re(1,2)     Im(1,2)     ...   Re(1,k)       Im(1,k)     Re(1,k+1) 0
    0           0           0           0           ...   0             0           0         0
    Re(2,1)     Re(2,2)     Re(2,3)     Re(2,4)     ...   Re(2,n-1)     Re(2,n)     n/u     n/u
    Im(2,1)     Im(2,2)     Im(2,3)     Im(2,4)     ...   Im(2,n-1)     Im(2,n)     n/u     n/u
    ...         ...         ...         ...         ...   ...           ...         n/u     n/u
    Re(s,1)     Re(s,2)     Re(s,3)     Re(s,4)     ...   Re(s,n-1)     Re(s,n)     n/u     n/u
    Im(s,1)     Im(s,2)     Im(s,3)     Im(s,4)     ...   Im(s,n-1)     Im(s,n)     n/u     n/u
    </code>

    <b>PACK format,  m x n matrix</b>

    Even length (m = s*2)
    <code>
    Re(1,1)     Re(1,2)     Im(1,2)     Re(1,3)     ...   Im(1,k)     Re(1,k+1)
    Re(2,1)     Re(2,2)     Re(2,3)     Re(2,4)     ...   Re(2,n-1)   Re(2,n)
    Im(2,1)     Im(2,2)     Im(2,3)     Im(2,4)     ...   Im(2,n-1)   Im(2,n)
    ...         ...         ...         ...         ...   ...         ...
    Re(m/2,1)   Re(m/2,2)   Re(m/2,3)   Re(m/2,4)   ...   Re(m/2,n-1) Re(m/2,n)
    Im(m/2,1)   Im(m/2,2)   Im(m/2,3)   Im(m/2,4)   ...   Im(m/2,n-1) Im(m/2,n)
    z(m/2+1,1)  Re(m/2+1,2) Im(m/2+1,2) Re(m/2+1,3) ...   Im(m/2+1,k) z(m/2+1,k+1)
    </code>

    Odd length (m = s*2+1)
    <code>
    Re(1,1) Re(1,2) Im(1,2) Re(1,3) ... Im(1,k)   Re(1,n/2+1)
    Re(2,1) Re(2,2) Re(2,3) Re(2,4) ... Re(2,n-1) Re(2,n)
    Im(2,1) Im(2,2) Im(2,3) Im(2,4) ... Im(2,n-1) Im(2,n)
    ... ... ... ... ... ... ...
    Re(s,1) Re(s,2) Re(s,3) Re(s,4) ... Re(s,n-1) Re(s,n)
    Im(s,1) Im(s,2) Im(s,3) Im(s,4) ... Im(s,n-1) Im(s,n)
    </code>

    <b>PERM Format,  m x n matrix</b>

    Even length (m = s*2)
    <code>
    Re(1,1)     Re(1,k+1)     Re(1,2)       Im(1,2)     ... Re(1,k)     Im(1,k)
    Re(m/2+1,1) Re(m/2+1,k+1) Re(m/2+1,2)   Im(m/2+1,2) ... Re(m/2+1,k) Im(m/2+1,k)
    Re(2,1)     Re(2,2)       Re(2,3)       Re(2,4)     ... Re(2,n-1)   Re(2,n)
    Im(2,1)     Im(2,2)       Im(2,3)       Im(2,4)     ... Im(2,n-1)   Im(2,n)
    ...         ...           ...           ...         ... ...         ...
    Re(m/2,1)   Re(m/2,2)     Re(m/2,3)     Re(m/2,4)   ... Re(m/2,n-1) Re(m/2,n)
    Im(m/2,1)   Im(m/2,2)     Im(m/2,3)     Im(m/2,4)   ... Im(m/2,n-1) Im(m/2,n)
    </code>

    Odd length (m = s*2+1)
    <code>
    Re(1,1)     Re(1,k+1)     Re(1,2)       Im(1,2)     ... Re(1,k)     Im(1,k)
    Re(2,1)     Re(2,2)       Re(2,3)       Re(2,4)     ... Re(2,n-1)   Re(2,n)
    Im(2,1)     Im(2,2)       Im(2,3)       Im(2,4)     ... Im(2,n-1)   Im(2,n)
    ...         ...           ...           ...         ... ...         ...
    Re(s,1)     Re(s,2)       Re(s,3)       Re(s,4)     ... Re(s,n-1)   Re(s,n)
    Im(s,1)     Im(s,2)       Im(s,3)       Im(s,4)     ... Im(s,n-1)   Im(s,n)
    </code>
</remarks>
*)
    TFFTStorageFormat = (
    (*<summary>
    This is the "natural" FFT storage format. It's main disadvantages are:
    * You cannot perform an inverse FFT to real, without telling
      the routine of what length should the result be: odd or even.
    * When performing real to complex FFT, the size of the
      result is bigger than the size of the source data. This requires
      that the initial data is stored in to an array which allocates
      more memory then occupied by the source (2 real numbers more)
    </summary>*)
    fsfCCS





































    );

type

  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectTwoFunction = function(var a,b: double): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectThreeFunction = function(var a,b,c: double): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectOneFunctionC = function(var a: TCplx): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
     
   TSelectTwoFunctionC = function(var a,b: TCplx): integer; cdecl;

  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectTwoFunctionSingle = function(var a,b: single): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectThreeFunctionSingle = function(var a,b,c: single): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectOneFunctionSingleC = function(var a: TSCplx): integer; cdecl;
  (*<summary>Function type with two parameters.</summary>*)
   
   TSelectTwoFunctionSingleC = function(var a,b: TSCplx): integer; cdecl;

  
  TMKLTRFunction = procedure(var m,n: Integer; xi, fi: Pointer; CustomData: TObject); cdecl;
  

  
  Tgemm_jit_kernel_tp = procedure(jitter: PAPointer; const a, b, c: PAPointer); cdecl;

  
  Tdgemm_jit_kernel_t = procedure(jitter: PAPointer; const a, b, c: TDoubleArray); cdecl;
  
  Tzgemm_jit_kernel_t = procedure(jitter: PAPointer; const a, b, c: TCplxDoubleArray); cdecl;
  
  Tdgemm_jit_kernel_tp = procedure(jitter: PAPointer; const a, b, c: PAPointer); cdecl;
  
  Tzgemm_jit_kernel_tp = procedure(jitter: PAPointer; const a, b, c: PAPointer); cdecl;

  
  Tsgemm_jit_kernel_t = procedure(jitter: PAPointer; const a, b, c: TSingleArray); cdecl;
  
  Tcgemm_jit_kernel_t = procedure(jitter: PAPointer; const a, b, c: TCplxSingleArray); cdecl;
  
  Tsgemm_jit_kernel_tp = procedure(jitter: PAPointer; const a, b, c: PAPointer); cdecl;
  
  Tcgemm_jit_kernel_tp = procedure(jitter: PAPointer; const a, b, c: PAPointer); cdecl;

    
    
    





  TMKL_Sparse_status = (
    (*<summary>the operation was successful</summary>*)
    SPARSE_STATUS_SUCCESS           = 0,
    (*<summary>empty handle or matrix arrays</summary>*)
    SPARSE_STATUS_NOT_INITIALIZED   = 1,
    (*<summary>internal error: memory allocation failed</summary>*)
    SPARSE_STATUS_ALLOC_FAILED      = 2,
    (*<summary>invalid input value</summary>*)
    SPARSE_STATUS_INVALID_VALUE     = 3,
    (*<summary>e.g. 0-diagonal element for triangular solver, etc.</summary>*)
    SPARSE_STATUS_EXECUTION_FAILED  = 4,
    (*<summary>internal error</summary>*)
    SPARSE_STATUS_INTERNAL_ERROR    = 5,
    (*<summary>e.g. operation for double precision doesn't support other types</summary>*)
    SPARSE_STATUS_NOT_SUPPORTED     = 6
  );

  TMKL_Sparse_operation = (
    SPARSE_OPERATION_NON_TRANSPOSE       = 10,
    SPARSE_OPERATION_TRANSPOSE           = 11,
    SPARSE_OPERATION_CONJUGATE_TRANSPOSE = 12
  );

  TMKL_Sparse_matrix_type  = (
    (*<summary>General case</summary>*)
    SPARSE_MATRIX_TYPE_GENERAL            = 20,
    (*<summary>Triangular part of the matrix is to be processed</summary>*)
    SPARSE_MATRIX_TYPE_SYMMETRIC          = 21,
    SPARSE_MATRIX_TYPE_HERMITIAN          = 22,
    SPARSE_MATRIX_TYPE_TRIANGULAR         = 23,
    (*<summary>diagonal matrix; only diagonal elements will be processed</summary>*)
    SPARSE_MATRIX_TYPE_DIAGONAL           = 24,
    SPARSE_MATRIX_TYPE_BLOCK_TRIANGULAR   = 25,
    (*<summary>block-diagonal matrix; only diagonal blocks will be processed</summary>*)
    SPARSE_MATRIX_TYPE_BLOCK_DIAGONAL     = 26
  );

  TMKL_Sparse_index_base = (
    (*<summary>C-style</summary>*)
    SPARSE_INDEX_BASE_ZERO  = 0,
    (*<summary>Fortran-style</summary>*)
    SPARSE_INDEX_BASE_ONE   = 1
  );

  (*<summary> Applies to triangular matrices only ( SPARSE_MATRIX_TYPE_SYMMETRIC, SPARSE_MATRIX_TYPE_HERMITIAN, SPARSE_MATRIX_TYPE_TRIANGULAR ) </summary>*)
  TMKL_Sparse_fill_mode = (
      (*<summary>lower triangular part of the matrix is stored</summary>*)
      SPARSE_FILL_MODE_LOWER  = 40,
      (*<summary>upper triangular part of the matrix is stored</summary>*)
      SPARSE_FILL_MODE_UPPER  = 41,
      (*<summary>upper triangular part of the matrix is stored</summary>*)
      SPARSE_FILL_MODE_FULL   = 42
  );

  (*<summary> Applies to triangular matrices only ( SPARSE_MATRIX_TYPE_SYMMETRIC, SPARSE_MATRIX_TYPE_HERMITIAN, SPARSE_MATRIX_TYPE_TRIANGULAR ) </summary>*)
  TMKL_Sparse_diag_type = (
      (*<summary>Triangular matrix with non-unit diagonal</summary>*)
      SPARSE_DIAG_NON_UNIT    = 50,
      (*<summary>Triangular matrix with unit diagonal</summary>*)
      SPARSE_DIAG_UNIT        = 51
  );

  (*<summary> Applicable for Level 3 operations with dense matrices; describes storage scheme for dense matrix (row major or column major) </summary>*)
  TMKL_Sparse_layout = (
      (*<summary>C-style</summary>*)
      SPARSE_LAYOUT_ROW_MAJOR    = 101,
      (*<summary>Fortran-style</summary>*)
      SPARSE_LAYOUT_COLUMN_MAJOR = 102
  );

  (*<summary> If verbose mode activated, handle should collect and report profiling / optimization info </summary>*)
  TMKL_Verbose_mode = (
      SPARSE_VERBOSE_OFF      = 70,
      (*<summary>Output contains high-level information about optimization algorithms, issues, etc.</summary>*)
      SPARSE_VERBOSE_BASIC    = 71,
      (*<summary>Provide detailed output information</summary>*)
      SPARSE_VERBOSE_EXTENDED = 72
  );

  (*<summary> memory optimization hints from user: describe how much memory could be used on optimization stage </summary>*)
  TMKL_Sparse_memory_usage = (
      (*<summary>No memory should be allocated for matrix values and structures; auxiliary structures could be created only for workload balancing, parallelization, etc.</summary>*)
      SPARSE_MEMORY_NONE          = 80,
      (*<summary>matrix could be converted to any internal format</summary>*)
      SPARSE_MEMORY_AGGRESSIVE    = 81
  );

  TMKL_Sparse_request = (
      SPARSE_STAGE_FULL_MULT            = 90,
      SPARSE_STAGE_NNZ_COUNT            = 91,
      SPARSE_STAGE_FINALIZE_MULT        = 92,
      SPARSE_STAGE_FULL_MULT_NO_VAL     = 93,
      SPARSE_STAGE_FINALIZE_MULT_NO_VAL = 94
  );

  (*<summary> Applies to SOR interface; define type of (S)SOR operation to perform </summary>*)
  TMKL_Sparse_sor_type = (
      (*<summary>(omegaL + D)*x^1 = (D - omega*D - omega*U)*alpha*x^0 + omega*b</summary>*)
      SPARSE_SOR_FORWARD   = 110,
      (*<summary>(omegaU + D)*x^1 = (D - omega*D - omega*L)*alpha*x^0 + omega*b</summary>*)
      SPARSE_SOR_BACKWARD  = 111,
      (*<summary>SSOR, for e.g. with (omega == 1) and (alpha == 1), equal to solving a system: (L + D) X x^1 = b - U*x; (U + D) X x = b - L*x^1</summary>*)
      SPARSE_SOR_SYMMETRIC = 112
  );

  TMKL_Matrix_descr = packed record
      (*<summary>matrix type: general, diagonal or triangular / symmetric / hermitian</summary>*)
      sType: TMkl_Sparse_matrix_type;
      (*<summary>upper or lower triangular part of the matrix ( for triangular / symmetric / hermitian case)</summary>*)
      mode: TMkl_Sparse_fill_mode;
      (*<summary>unit or non-unit diagonal ( for triangular / symmetric / hermitian case)</summary>*)
      diag: TMkl_Sparse_diag_type;
  end;

  TMklSparseMatrix = PAPointer;

  TMKL_Layout = (
    MKL_ROW_MAJOR = 101,
    MKL_COL_MAJOR = 102
  );

  TMKL_Transpose = (
    MKL_NOTRANS = 111,
    MKL_TRANS = 112,
    MKL_CONJTRANS = 113,
    MKL_CONJ = 114
  );

  TMKL_Uplo = (
    MKL_UPPER = 121,
    MKL_LOWER = 122
  );

  TMKL_Diag = (
    MKL_NONUNIT = 131,
    MKL_UNIT = 132
  );

  TMKL_Side = (
    MKL_LEFT = 141,
    MKL_RIGHT = 142
  );

  TMKL_Compact_Pack = (
    MKL_COMPACT_SSE = 181,
    MKL_COMPACT_AVX = 182,
    MKL_COMPACT_AVX512 = 183
  );

  TMKL_Jit_Status = (
    MKL_JIT_SUCCESS = 0,
    MKL_NO_JIT = 1,
    MKL_JIT_ERROR = 2
  );





  

  (*<summary>Base exception class for MtxVec library.</summary>
    
<remarks>Parent class for all exception classes in MtxVec library.
</remarks>


    <seealso cref="EMtxVecRangeError"/>
    <seealso cref="EMtxVecInvalidArgument"/>*)
  
  EMtxVecException = class (Exception)
  end;
  

  (*<summary>Exception class indicates out of range error.</summary>
    
<remarks>Raises exception if an out of range error occurres.
</remarks>

    <seealso cref="EMtxVecException"/>*)
  EMtxVecRangeError = class (EMtxVecException)
  
  end;

  (*<summary>Exception class indicates invalid arguments error.</summary>
    
<remarks>Raises exception if invalid parameters were passed to the procedure.
</remarks>

    <seealso cref="EMtxVecException"/>*)
  EMtxVecInvalidArgument = class (EMtxVecException)
  
  end;

  

  (*<summary>Rounding algorithm.</summary>*)
  TRounding = (
  (*<summary>Truncate values to nearest integer.</summary>*)rnTrunc,
  (*<summary>Round values towards nearest integer.</summary>*)rnRound);

  (*<summary>Comparison type for the Equal function.</summary>
    
<remarks>Determines the method used to compare two floating point numbers in
    order to determine if they are equal or not within some tolerance.
</remarks>
*)
  TCompare = (
  (*<summary>a and b are equal, if the following expression is true:

    <c>abs(a-b) &lt; Tolerance</c>.
    </summary>*)
  cmpAbsolute,
  (*<summary>a and b are equal, if the following expression is true:

    <c>abs(a-b)/Max( abs(a),abs(b) ) &lt;= Tolerance</c>

    This method also requires a special "zero tolerance" parameter, which determines
    how small must a number be so that it can be considered to be zero.
    </summary>*)
  cmpRelative
  );




 const
  (*<summary>PI.</summary>
    
<remarks>= PI.
</remarks>
*)PI: double		= 3.14159265358979323846;
  (*<summary>PI.</summary>
    
<remarks>= PI.
</remarks>
*)PI_SINGLE: single		= 3.14159265358979323846;
  (*<summary>Ln(2).</summary>
    
<remarks>= Ln(2)
</remarks>
*)LN2: double 		= 0.69314718055994530942;
  (*<summary>Ln(2).</summary>
    
<remarks>= Ln(2)
</remarks>
*)LN2_SINGLE: single 		= 0.69314718055994530942;
  (*<summary>Ln(10).</summary>
    
<remarks>= Ln(10)
</remarks>
*)LN10: double	= 2.30258509299404568402;
  (*<summary>Ln(10).</summary>
    
<remarks>= Ln(10)
</remarks>
*)LN10_SINGLE: single	= 2.30258509299404568402;
  (*<summary>Ln(PI).</summary>
    
<remarks>= Ln(PI).
</remarks>
*)LNPI: double   	= 1.14472988584940017414;
  (*<summary>Ln(PI).</summary>
    
<remarks>= Ln(PI).
</remarks>
*)LNPI_SINGLE: single   	= 1.14472988584940017414;
  (*<summary>1/Ln(2).</summary>
    
<remarks>= 1/Ln(2)
</remarks>
*)INVLN2: double 	= 1.44269504088896340736;
  (*<summary>1/Ln(2).</summary>
    
<remarks>= 1/Ln(2)
</remarks>
*)INVLN2_SINGLE: single 	= 1.44269504088896340736;
  (*<summary>1/Ln(10).</summary>
    
<remarks>= 1/Ln(10)
</remarks>
*)INVLN10: double  	= 0.43429448190325182765;
  (*<summary>1/Ln(10).</summary>
    
<remarks>= 1/Ln(10)
</remarks>
*)INVLN10_SINGLE: single  	= 0.43429448190325182765;
  (*<summary>2*PI.</summary>
    
<remarks>= 2*PI.
</remarks>
*)TWOPI: double	= 6.28318530717958647693;
  (*<summary>2*PI.</summary>
    
<remarks>= 2*PI.
</remarks>
*)TWOPI_SINGLE: single	= 6.28318530717958647693;
  (*<summary>1/2PI.</summary>
    
<remarks>= 1/2PI
</remarks>
*)INVTWOPI: double     = 0.1591549430918953420;
  (*<summary>1/2PI.</summary>
    
<remarks>= 1/2PI
</remarks>
*)INVTWOPI_SINGLE: single     = 0.1591549430918953420;
  (*<summary>PI/2.</summary>
    
<remarks>= PI/2
</remarks>
*)PIDIV2: double       = 1.57079632679489661923;
  (*<summary>PI/2.</summary>
    
<remarks>= PI/2
</remarks>
*)PIDIV2_SINGLE: single       = 1.57079632679489661923;
  (*<summary>Sqrt(PI).</summary>
    
<remarks>= Sqrt(PI)
</remarks>
*)SQRTPI: double       = 1.77245385090551602730;
  (*<summary>Sqrt(PI).</summary>
    
<remarks>= Sqrt(PI)
</remarks>
*)SQRTPI_SINGLE: single       = 1.77245385090551602730;
  (*<summary>Sqrt(2PI)</summary>
    
<remarks>= Sqrt(2PI)
</remarks>
*)SQRT2PI: double      = 2.50662827463100050242;
  (*<summary>Sqrt(2PI)</summary>
    
<remarks>= Sqrt(2PI)
</remarks>
*)SQRT2PI_SINGLE: single      = 2.50662827463100050242;
  (*<summary>1/Sqrt(2PI).</summary>
    
<remarks>= 1/Sqrt(2PI)
</remarks>
*)INVSQRT2PI: double   = 0.39894228040143267794;
  (*<summary>1/Sqrt(2PI).</summary>
    
<remarks>= 1/Sqrt(2PI)
</remarks>
*)INVSQRT2PI_SINGLE: single   = 0.39894228040143267794;
  (*<summary>Ln(Sqrt(2*PI)).</summary>
    
<remarks>= Ln(Sqrt(2*PI)
</remarks>
*)LNSQRT2PI: double    = 0.91893853320467274178;
  (*<summary>Ln(Sqrt(2*PI)).</summary>
    
<remarks>= Ln(Sqrt(2*PI)
</remarks>
*)LNSQRT2PI_SINGLE: single    = 0.91893853320467274178;
  (*<summary>Ln(2*PI)/2.</summary>
    
<remarks>= Ln(2*PI)/2
</remarks>
*)LN2PIDIV2: double  = 0.91893853320467274178;
  (*<summary>Ln(2*PI)/2.</summary>
    
<remarks>= Ln(2*PI)/2
</remarks>
*)LN2PIDIV2_SINGLE: single    = 0.91893853320467274178;
  (*<summary>Sqrt(2).</summary>
    
<remarks>= Sqrt(2)
</remarks>
*)SQRT2: double        = 1.41421356237309504880;
  (*<summary>Sqrt(2).</summary>
    
<remarks>= Sqrt(2)
</remarks>
*)SQRT2_SINGLE: single = 1.41421356237309504880;
  (*<summary>Sqrt(2)/2.</summary>
    
<remarks>= Sqrt(2)/2
</remarks>
*)SQRT2DIV2: double  = 0.70710678118654752440;
  (*<summary>Sqrt(2)/2.</summary>
    
<remarks>= Sqrt(2)/2
</remarks>
*)SQRT2DIV2_SINGLE: single  = 0.70710678118654752440;
  (*<summary>Golden Mean.</summary>
    
<remarks>= (1 + Sqrt(5))/2
</remarks>
*)GOLD: double         = 1.61803398874989484821;
  (*<summary>Golden Mean.</summary>
    
<remarks>= (1 + Sqrt(5))/2
</remarks>
*)GOLD_SINGLE: single  = 1.61803398874989484821;
  (*<summary>2 - GOLD.</summary>
    
<remarks>= 2 - GOLD
</remarks>
*)CGOLD: double        = 0.38196601125010515179;
  (*<summary>2 - GOLD.</summary>
    
<remarks>= 2 - GOLD
</remarks>
*)CGOLD_SINGLE: single        = 0.38196601125010515179;
  (*<summary>PI/180.</summary>
    
<remarks>= PI/180
</remarks>
*)PI180: double        = 0.01745329251994329577;
  (*<summary>PI/180.</summary>
    
<remarks>= PI/180
</remarks>
*)PI180_SINGLE: single        = 0.01745329251994329577;
  (*<summary>180/PI.</summary>
    
<remarks>= 180/PI.
</remarks>
*)INVPI180: double     = 57.2957795130823208768;
  (*<summary>180/PI.</summary>
    
<remarks>= 180/PI.
</remarks>
*)INVPI180_SINGLE: single     = 57.2957795130823208768;
  (*<summary>Euler constant.</summary>
     
<remarks>Euler-Mascheroni, the so called Euler gamma constant.
</remarks>
*)
     EULERGAMMA: double = 0.5772156649015328606;
  (*<summary>Euler constant.</summary>
     
<remarks>Euler-Mascheroni, the so called Euler gamma constant.
</remarks>
*)
     EULERGAMMA_SINGLE: single = 0.5772156649015328606;
  
    S_ONE: double = 1;
    S_ZERO: double = 0;
    S_ONE_SINGLE: single = 1;
    S_ZERO_SINGLE: single = 0;
  (*<summary>Euler number (e).</summary>*)
    EULER_NUMBER: double = 2.71828182845904523536028747135;
  (*<summary>Euler number (e).</summary>*)
    EULER_NUMBER_SINGLE: single = 2.71828182845904523536028747135;

  


const
  (*<summary>Machine constant.</summary>
    
<remarks>Same as EPS=2^(-52).
</remarks>
*) MACHEP: double = 2.220446049250313E-16;
  (*<summary>Epsilon.</summary>
    
<remarks>Machine constant.
</remarks>
*) EPS = 2.220446049250313E-16;
  (*<summary>Sqrt(EPS).</summary>
    
<remarks>Square root of EPS.
</remarks>
*) SQRTEPS = 1.4901161194e-08;
  (*<summary>Maximum floating point number.</summary>
    
<remarks>Maximum floating point number.
</remarks>
*)MAXNUM  = 1.797693134862315E+308;
  (*<summary>Minimum floating point number.</summary>
    
<remarks>Minimum floating point number: <c>2^(-1022)</c>.
</remarks>
*)MINNUM: double = 2.225073858507202E-308;
  (*<summary>Largest result of a base 10 logarithm.</summary>
    
<remarks>Largest result of a base 10 logarithm.
</remarks>
*)MAXLOG: double = 709.7827128933840;
  (*<summary>Smallest result of a base 10 logarithm.</summary>
    
<remarks>Smallest result of a base 10 logarithm.
</remarks>
*)MINLOG: double = -708.3964185322641;
  (*<summary>Maximum factorial.</summary>
    
<remarks>Maximum factorial.
</remarks>
*)MAXFAC: double = 170;
  (*<summary>Maximum value of the gamma function.</summary>
    
<remarks>Maximum value of the gamma function.
</remarks>
*)MAXGAM: double = 171.624376956302;
  (*<summary>Maximum allowed value for the logarithm of the gamma function.</summary>
    
<remarks>If the input parameter to the lnGamma function is bigger, the function
    will return INF.
</remarks>
*)MAXLGM: double = 2.556348E+305;


const
  MACHEP_SINGLE: single = 1.192093E-7;               
  EPS_SINGLE:    single = 1.192093E-7;
  SQRTEPS_SINGLE:single = 0.0003452670062;
  MAXNUM_SINGLE: single =  3.402823E+38;              
  MINNUM_SINGLE: single = 1.175495E-38;              
  MAXLOG_SINGLE: single = 88.72283;
  MINLOG_SINGLE: single = -87.33655;
  MAXFAC_SINGLE: single = 33;
  MAXGAM_SINGLE: single = 34.648;
  MAXLGM_SINGLE: single = 1.0383E+36;










const 

    (*<summary>Complex zero.</summary>
      
<remarks>Complex zero.
</remarks>
*)C_ZERO: TCplx = (Re: 0.0; Im: 0.0);
    (*<summary>Complex one.</summary>
      
<remarks>= 1+0*i.
</remarks>
*)C_ONE: TCplx = (Re: 1.0; Im: 0.0);
    (*<summary>Imaginary value.</summary>
      
<remarks>= 0+1*i.
</remarks>
*)C_I: TCplx = (Re: 0.0; Im: 1.0);
    (*<summary>PI.</summary>
      
<remarks>= PI+0*i.
</remarks>
*)C_PI: TCplx = (Re: 3.14159265358979323846; Im: 0.0);
    (*<summary>PI/2.</summary>
    
<remarks>= PI/2+0*i.
</remarks>
*)C_PI_DIV_2: TCplx = (Re: 1.57079632679489661923; Im: 0.0);     

    (*<summary>Complex zero.</summary>
      
<remarks>Complex zero.
</remarks>
*)C_ZERO_SINGLE: TSCplx = (Re: 0.0; Im: 0.0);
    (*<summary>Complex one.</summary>
      
<remarks>= 1+0*i.
</remarks>
*)C_ONE_SINGLE: TSCplx = (Re: 1.0; Im: 0.0);
    (*<summary>Imaginary value.</summary>
      
<remarks>= 0+1*i.
</remarks>
*)C_I_SINGLE: TSCplx = (Re: 0.0; Im: 1.0);
    (*<summary>PI.</summary>
      
<remarks>= PI+0*i.
</remarks>
*)C_PI_SINGLE: TSCplx = (Re: 3.14159265358979323846; Im: 0.0);
    (*<summary>PI/2.</summary>
    
<remarks>= PI/2+0*i.
</remarks>
*)C_PI_DIV_2_SINGLE: TSCplx = (Re: 1.57079632679489661923; Im: 0.0);     

    
      Saved8087: word = $1332;

      kTab = Chr(9);

    




(*<summary>Incomplete Gamma function: series expansion.</summary>
  <returns>Incomplete Gamma function (series expansion).</returns>

  
<remarks>Uses series expansion to calculate the incomplete Gamma function.
</remarks>


  <SeeAlso cref="GammaFrac"/>
  <SeeAlso cref="IGamma"/>*)

function GammaSer(A, X : double) : double; overload;

(*<summary>Incomplete Gamma function : series expansion.</summary>
  
<remarks>Complex version.
</remarks>
*)

function GammaSer(A: double;const X: TCplx) : TCplx; overload;

(*<summary>Incomplete Gamma function : continued fraction expansion.</summary>
  <returns>the Incomplete Gamma function : continued fraction expansion.</returns>

  
<remarks>Uses fractional expansion to calculate the incomplete Gamma function.
</remarks>


  <SeeAlso cref="GammaFrac"/>
  <SeeAlso cref="IGamma"/>*)

function GammaFrac(A, X : double) : double; overload;

(*<summary>Incomplete Gamma function : continued fraction expansion.</summary>
<returns>Incomplete Gamma function as complex number.</returns>*)

function GammaFrac(A: double;const X : TCplx) : TCplx; overload;


(*<summary>Incomplete gamma function.</summary>
  <returns>the incomplete gamma function.</returns>
  
<remarks>The IGamma function is defined by the following equation:

  <img name="probabilities026"/>

  where Gamma is <see cref="Math387.Gamma"/> function. Depending on parameters
  a and x fractional or series expansion is used to calculate the incomplete Gamma function.
</remarks>

  <SeeAlso cref="JGamma"/>
  <SeeAlso cref="GammaFrac"/>
  <SeeAlso cref="GammaSer"/>
  <SeeAlso cref="Math387.Gamma"/>*)

function IGamma(A, X: double): double; overload;


(*<summary>Incomplete gamma function.</summary>*)

function IGamma(A: double;const X: TCplx): TCplx; overload;


(*<summary>Complement to the incomplete gamma function.</summary>
  <returns>the complement to the incomplete gamma function.</returns>
  
<remarks>The JGamma function is defined by the following equation:

  <img name="probabilities027"/>

  where Gamma is <see cref="Math387.Gamma"/> function.
</remarks>

  <SeeAlso cref="IGamma"/>*)

function JGamma(A, X: double): double; overload;

(*<summary>Complement to the incomplete gamma function.</summary>
  
<remarks>Complex version.
</remarks>
*)

function JGamma(A: double;const  X: TCplx) : TCplx; overload;


(*<summary>Error function.</summary>
  <returns>the error function for given parameter X.</returns>
  <param name="X">X in Error function definition.</param>
  
<remarks>The error function is defined by the following equation:

  <img name="probabilities011"/>
</remarks>


  <SeeAlso cref="Erfc"/>
  <SeeAlso cref="ErfInv"/>*)

function Erf(X: double): double; overload;


(*<summary>Inverse error function.</summary>
  <returns>the Inverse error function.</returns>
  
<remarks>Returns the inverse error function Erf for given y. The inverse error function satisfies the equation <c>x=Erf(y)</c>.
</remarks>


  <SeeAlso cref="Erf"/>
  <SeeAlso cref="ErfInv"/>*)

function ErfInv(y: double): double; overload;


(*<summary>Complement to the error function Erf.</summary>
  <returns>the complement to the error function Erf.</returns>
  
<remarks>The Erfc function is defined by the following equation:

  <img name="probabilities012"/>
</remarks>


  <SeeAlso cref="Erf"/>
  <SeeAlso cref="ErfInv"/>*)

function Erfc(X: double): double; overload;


(*<summary>Checks if number is NAN.</summary>
  <returns>true if complex number X is NAN.</returns>*)

function IsNaN(const X: TCplx): boolean; overload;     inline;  

function IsNaN(const X: TSCplx): boolean; overload;     inline;  
(*<summary>Checks if number is NAN.</summary>
  <returns>true if real number X is NAN.</returns>
  <seealso cref="IsInf"/>
  <seealso cref="IsInfNan"/>*)

function IsNaN(const X: double): boolean; overload;      inline;  

function IsNanf(const X: single): boolean; overload;
(*<summary>Checks if number is INF.</summary>
  <returns>true if complex number X is INF.</returns>*)

function IsInf(const X: TCplx): boolean; overload;    inline; 

function IsInf(const X: TSCplx): boolean; overload;    inline; 
(*<summary>Checks if number is INF.</summary>
  <returns>true if real number X is INF.</returns>
  <seealso cref="IsNaN"/>
  <seealso cref="IsInfNan"/>*)

function IsInf(const X: double): boolean; overload;    inline; 

function IsInff(const X: single): boolean; overload;    inline; 
(*<summary>Checks if number is INF 0r NAN.</summary>
  <returns>true if complex number X is INF or NAN.</returns>*)

function IsInfNan(const X: TCplx): boolean; overload;    inline; 
function IsInfNan(const X: TSCplx): boolean; overload;    inline; 
(*<summary>Checks if number is INF or NAN.</summary>
  <returns>true if real number X is INF or NAN.</returns>
  <seealso cref="IsNaN"/>
  <seealso cref="IsInf"/>*)

function IsInfNan(const X: double): boolean; overload;    inline; 

function IsInfNanf(const X: single): boolean; overload;    inline; 
(*<summary>Checks if number is INF or NAN.</summary>
  <returns>true if real number X is INF or NAN.</returns>
  <seealso cref="IsInfNan"/>
  <seealso cref="IsInf"/>*)

function IsNanInf(const X: TCplx): boolean; overload;    inline; 

function IsNanInf(const X: TSCplx): boolean; overload;    inline; 
(*<summary>Checks if number is INF or NAN.</summary>
  <returns>true if real number X is INF or NAN.</returns>
  <seealso cref="IsInfNan"/>
  <seealso cref="IsInfNan"/>*)

function IsNanInf(const X: double): boolean; overload;    inline; 

function IsNanInff(const X: single): boolean; overload;    inline; 

(*<summary>Minimum of two numbers.</summary>
  <returns>the minimum of two complex numbers X and Y.</returns>

  
<remarks>Complex numbers are first compared by the absolute value. If they are equal they are further compared by the
  unwrapped phase <c>-PI,PI</c> angle.
</remarks>
*)

function Min(const X, Y : TCplx) : TCplx; overload;   

function Min(const X, Y : TSCplx) : TSCplx; overload;   
(*<summary>Maximum of two numbers.</summary>
  <returns>the maximum of two complex numbers X and Y.</returns>

  
<remarks>Complex numbers are first compared by the absolute value. If they are equal they are further compared by
  the unwrapped phase <c>-PI,PI</c> angle.
</remarks>
*)

function Max(const X, Y : TCplx) : TCplx; overload;   

function Max(const X, Y : TSCplx) : TSCplx; overload;   
(*<summary>Minimum of two real numbers X and Y.</summary>
  <returns>the minimum of two real numbers X and Y.</returns>*)

function Min(X, Y: double) : double; overload;    inline; 

function Minf(X, Y: single) : single; overload;    inline; 
(*<summary>Maximum of two real numbers X and Y.</summary>
  <returns>the maximum of two real numbers X and Y.</returns>
  <seealso cref="Min"/>*)

function Max(X, Y: double): double; overload;    inline; 

function Maxf(X, Y: single): single; overload;    inline; 
(*<summary>Minimum of two integer numbers X and Y.</summary>
  <returns>the minimum of two integer numbers X and Y.</returns>
  <seealso cref="Max"/>*)

function Min(X, Y: Integer) : Integer; overload;    inline; 
(*<summary>Maximum of two integer numbers X and Y.</summary>
  <returns>the maximum of two integer numbers X and Y.</returns>
  <seealso cref="Min"/>*)

function Max(X, Y: Integer) : Integer; overload;    inline; 

(*<summary>Minimum of two 64bit integer numbers X and Y.</summary>
  <returns>the minimum of two 64bit integer numbers X and Y.</returns>
  <seealso cref="Max"/>*)

function Min(X, Y: Int64) : Int64; overload;    inline; 
(*<summary>Maximum of two 64bit integer numbers X and Y.</summary>
  <returns>the maximum of two 64bit integer numbers X and Y.</returns>
  <seealso cref="Min"/>*)

function Max(X, Y: Int64) : Int64; overload;    inline; 

(*<summary>Real part of complex number.</summary>
  <returns>the real part of complex number X.</returns>
  <seealso cref="Imag"/>*)

function Real(const X: TCplx): double; overload;  inline; 

function Real(const X: TSCplx): single; overload;  inline; 
(*<summary>Imaginary part of complex number.</summary>
  <returns>the imaginary part of complex number X.</returns>
  <seealso cref="Real"/>*)

function Imag(const X: TCplx): double; overload;  inline; 

function Imag(const X: TSCplx): single; overload;  inline; 
(*<summary>Swaps real and imaginary part.</summary>
 <returns> Flipped complex number: <c>Flip(x+i*y), becomes: y+i*x</c></returns>
 <seealso cref="FlipConj"/>*)

function Flip(const X: TCplx): TCplx; overload; 

function Flip(const X: TSCplx): TSCplx; overload;  

(*<summary>Swaps real and imaginary part and conjugates the result.</summary>
 <returns> Flipped conjugated complex number: <c>Flip(x+i*y), becomes: y-i*x</c>.</returns>

 <seealso cref="Flip"/>*)

function FlipConj(const X: TCplx): TCplx; overload; 

function FlipConj(const X: TSCplx): TSCplx; overload; 

(*<summary>Swaps real and imaginary part and conjugates the result.</summary>
  <param name="X">Defines the input parameter for swapping real and complex part.</param>
  <param name="result">Stores the result of swapping and conjugating: <c>a+i*b, becomes: b-i*a</c>.</param>*)

procedure FlipConj(const X: TCplx; out result: TCplx); overload; 

procedure FlipConj(const X: TSCplx; out result: TSCplx); overload; 

(*<summary>Multiplies complex number with I.</summary>
 <returns>original value, multiplied with i: <c> (x+I*y) becomes: -y+I*x</c></returns>*)

function MulI(const X: TCplx): TCplx; overload; 

function MulI(const X: TSCplx): TSCplx; overload; 

(*<summary>Multiplies complex number with I.</summary>
  <param name="X">Defines the input parameter for multiplying with <c>I</c>.</param>
  <param name="result">Stores the result of multiplying with <c>I</c>.</param>*)

procedure MulI(Const X: TCplx; out result: TCplx); overload; 

procedure MulI(Const X: TSCplx; out result: TSCplx); overload; 

(*<summary>The ||Z||^2.</summary>
  <returns>the squared norm of complex number: <c>(x+i*y) becomes:Sqr(x) + Sqr(y)</c>.</returns>

  <seealso cref="Pythag"/>*)

function Norm(const X: TCplx): double; overload;

function Norm(const X: TSCplx): single; overload;

function SqrAbs(const X: TCplx): double; overload;

function SqrAbs(const X: TSCplx): single; overload;

(*<summary>The 1/Z.</summary>
  <returns>the 1/X: <c>X becomes 1/X</c>.</returns>*)

function CInv(const X: TCplx): TCplx; overload;

function CInv(const X: TSCplx): TSCplx; overload;
(*<summary>The 1/Z.</summary>
  <param name="X">Defines the input parameter for inverting.</param>
  <param name="result">Stores the result of inverting: <c>X becomes: 1/X</c></param>*)

procedure CInv(Const X: TCplx; out result: TCplx); overload;

procedure CInv(Const X: TSCplx; out result: TSCplx); overload;

(*<summary>Exchange two numbers.</summary>
  
<remarks>Exchanges two complex numbers <c>X swaps Y</c>.
</remarks>
*)

procedure Swap(var X, Y : TCplx); overload;   inline; 

procedure Swap(var X, Y : TSCplx); overload;   inline; 
(*<summary>Exchange two numbers.</summary>
  
<remarks>Exchanges two real numbers <c>X swaps Y</c>.
</remarks>
*)

procedure Swap(var X, Y : double); overload;
(*<summary>Exchange two numbers.</summary>
  
<remarks>Exchanges two real numbers <c>X swaps Y</c>.
</remarks>
*)

procedure Swapf(var X, Y : single); overload;
(*<summary>Exchange two numbers.</summary>
  
<remarks>Exchanges two integer numbers <c>X swaps Y</c>.
</remarks>
*)

procedure Swap(var X, Y : Integer); overload; 
procedure Swap(var X, Y: byte); overload;

(*<summary>Signum.</summary>
 <returns>the signum of X. Signum(X) is 1 for <c>X &gt; 0</c>,
          equal to zero for <c>X = 0</c> and  -1 for <c>X &lt; 0</c>.</returns>*)

function Sgn(const Z: TCplx): Integer; overload;    inline; 

function Sgn(const Z: TSCplx): Integer; overload;    inline; 

(*<summary>Signum.</summary>
  <returns>the signum of X. Signum(X) is 1 for <c>X &gt; 0</c> , equal to zero for <c>X = 0</c> and  -1 for <c>X &lt; 0</c>.</returns>*)

function Sgn(X: double): Integer; overload;

function Sgnf(X: single): Integer; overload;


function Sgn(X: integer): Integer; overload;

(*<summary>e^x function.</summary>
  <returns>the value of <b>e</b> raised to the power of X, where <b>e</b> is the base of natural logarithm.</returns>*)

function Exp(const X: TCplx): TCplx; overload;

function Exp(const X: TSCplx): TSCplx; overload;

(*<summary>2^x function.</summary>
  <returns>Returns the value of 2 raised to the power of X. </returns>
  
<remarks>This function is twice as fast as the equivalent  call to the <see cref="Power"/> function.
</remarks>
*)

function Exp2(const X: double): double; overload; 

function Exp2f(const X: single): single; overload; 

(*<summary>2^x function.</summary>
  <returns>The value of 2 raised to the power of X. Valid result is returned for X smaller than 32. </returns>
  
<remarks>This function is more than 10x as fast as a call to the <see cref="Power"/> function.
</remarks>
*)

function Exp2Int(const X: Integer): Integer; overload;

(*<summary>2^x function.</summary>
  <returns> Returns the value of 2 raised to the power of X. Valid result is returned for X smaller than 64. </returns>
  
<remarks>This function is more than 10x as fast as a call to the <see cref="Power"/> function.
</remarks>
*)


function Exp2Int64(const X: Integer): Int64; overload;

(*<summary>2^x function.</summary>
  <returns>Returns the value of 2 raised to the power of X.  </returns>
  <seealso cref="Power"/>*)

function Exp2(const X : TCplx): TCplx; overload;

function Exp2(const X : TSCplx): TSCplx; overload;

(*<summary>10^x function.</summary>
  <returns>Returns the value of 10 raised to the power of X. </returns>
  
<remarks>This function is about 30% faster as the equivalent call to the <see cref="Power"/> routine.
</remarks>
*)

function Exp10(const X : double): double; overload;

function Exp10f(const X : single): single; overload;

(*<summary>10^x function.</summary>
  <returns>Returns the value of 10 raised to the power of X. </returns>

  
<remarks>This function is about 30% faster as the equivalent call to the <see cref="Power"/> routine
</remarks>


  <seealso cref="Power"/>*)

function Exp10(const X : TCplx): TCplx; overload;

function Exp10(const X : TSCplx): TSCplx; overload;

(*<summary>Natural logarithm.</summary>
  <returns>Returns the natural logarithm of X.</returns>

  <seealso cref="Exp"/>
  <seealso cref="Expj"/>*)

function Ln(const X: TCplx): TCplx; overload;

function Ln(const X: TSCplx): TSCplx; overload;

(*<summary>Euler formula for complex number.</summary>
  <returns>Returns the <c>e^(i*W)</c>, using the Euler formula:

    <c>e^(i*W) = cos(W) + i*sin(W)</c><para/>
  </returns>*)

function Expj(const Omega: double): TCplx; overload;

function Expjf(const Omega: single): TSCplx; overload;

(*<summary>Log base 2.</summary>
  <returns>Returns the log base 2 for real argument X.</returns>

  <seealso cref="Log10"/>
  <seealso cref="LogN"/>*)

function Log2(const X: double): double; overload;

function Log2f(const X: single): single; overload;

(*<summary>Log base 2.</summary>
  <returns>Returns the log base 2 for 64bit integer argument X.</returns>
  
<remarks>This function is more than 10x as fast as the Log2 with real argument.
</remarks>


  <seealso cref="Log10"/>
  <seealso cref="LogN"/>*)

function Log2Int64(x: Int64): Int64; overload;

function Log2Int(x: integer): integer; overload;

(*<summary>Log base 2.</summary>
  <returns>Returns the log base 2 for complex argument X.</returns>
  <seealso cref="LogN"/>*)

function Log2(const X: TCplx): TCplx; overload;

function Log2(const X: TSCplx): TSCplx; overload;

(*<summary>Log base 10.</summary>
  <returns>Returns the log base 10 for real argument X.</returns>
  <seealso cref="Log2"/>
  <seealso cref="LogN"/>*)

function Log10(const X: double): double; overload; 

function Log10f(const X: single): single; overload; 

(*<summary>Log base 10.</summary>
  <returns>Returns the log base 10 for complex argument X.</returns>

  <seealso cref="Log2"/>
  <seealso cref="LogN"/>*)

function Log10(const X: TCplx): TCplx; overload;

function Log10(const X: TSCplx): TSCplx; overload;

(*<summary>Log base N.</summary>
  <returns>Returns the log base N for real argument X.</returns>

  <seealso cref="Log2"/>
  <seealso cref="Log10"/>*)

function LogN(const N, X: double): double; overload;

function LogNf(const N, X: single): single; overload;

(*<summary>Log base N.</summary>
  <returns>Returns the log base N for complex argument X.</returns>

  <seealso cref="Log2"/>
  <seealso cref="Log10"/>*)

function LogN(const N: double; const X: TCplx): TCplx; overload;

function LogN(const N: single; const X: TSCplx): TSCplx; overload;
(*<summary>Log base N.</summary>
  <returns>Returns the log of complex base N for complex argument X.</returns>

  
<remarks>Note
    Both base N argument X are complex numbers.
</remarks>
*)

function LogN(const N, X: TCplx): TCplx; overload;

function LogN(const N, X: TSCplx): TSCplx; overload;
(*<summary>Log base N.</summary>
  <returns>Returns the log of complex base N for real argument X.</returns>
  
<remarks>Note: Only base N is complex numbers.
</remarks>
*)

function LogN(const N: TCplx; const X: double): TCplx; overload;

function LogN(const N: TSCplx; const X: single): TSCplx; overload;

(*<summary>Power function (integer power).</summary>
  <returns>Base to an integer power N.</returns>
  
<remarks>Try to avoid calling this function for small powers. N can also be negative.
</remarks>
*)

function IntPower(const Base: double; const Exponent : Integer): double; overload;

function IntPowerf(const Base: single; const Exponent : Integer): single; overload;

(*<summary>Power function (integer power).</summary>
  <returns>Returns complex Base to an integer power N.</returns>
  
<remarks>Try to avoid calling this function for small powers. N can also be negative.
</remarks>

  <seealso cref="Power"/>*)

function IntPower(const Base: TCplx; const N: Integer): TCplx; overload;

function IntPower(const Base: TSCplx; const N: Integer): TSCplx; overload;

(*<summary>Power function.</summary>
  <returns>Returns Base to any power.</returns>
  
<remarks><see cref="IntPower"/> is faster, if Exponent is an integer. Real valued
  power can handle only positive Exponent and Base. <see cref="IntPower"/> can also handle negative exponent
  and/or negative base. To compute a power to the non-integer negative exponent and base in general case,
  use the complex version of the function.
</remarks>


  <seealso cref="IntPower"/>*)

function Power(const Base, Exponent: double): double; overload; 

function Powerf(const Base, Exponent: single): single; overload; 

(*<summary>Power function.</summary>
  <returns>Returns Base to any power.</returns>
  
<remarks>Here both base and exponent can be complex values. Use this version if you
  want to compute a power to the negative exponent in general case.
</remarks>
*)

function Power(const A, X: TCplx): TCplx; overload;

function Power(const A, X: TSCplx): TSCplx; overload;
(*<summary>Power function.</summary>
  <returns>Returns Base to any power.</returns>

  
<remarks>Here base is real and exponent is complex.
</remarks>
*)

function Power(const A: double; const X : TCplx): TCplx; overload;

function Power(const A: single; const X : TSCplx): TSCplx; overload;

(*<summary>Power function.</summary>
  <returns>Returns Base to any power.</returns>

  
<remarks>Here base is complex and exponent is real.
</remarks>
*)

function Power(const A: TCplx; const X: double): TCplx; overload;

function Power(const A: TSCplx; const X: single): TSCplx; overload;

(*<summary>The Pythagora formula.</summary>
  <returns>Returns the <c>Sqrt(X*X + Y*Y)</c>.</returns>
  <seealso cref="Norm"/>*)

function Pythag(const X, Y: double): double; overload;

function Pythagf(const X, Y: single): single; overload;
(*<summary>Complex version of Pythagora formula.</summary>
  <returns>Returns the <c>Sqrt(X*X + Y*Y)</c>.</returns>*)

function Pythag(const X, Y: TCplx): TCplx; overload; 

function Pythag(const X, Y: TSCplx): TSCplx; overload; 

(*<summary>Sets angle in [-2Pi,2Pi].</summary>
  <returns>Returns ThetaRad within <c>[-2Pi,2Pi]</c> interval.</returns>

  
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve
  numerical accuracy.
</remarks>
*)

function FixAngle(const ThetaRad: double): double; overload;

function FixAnglef(const ThetaRad: single): single; overload;

(*<summary><c>e^x</c> function.</summary>
  <returns>Returns the value of <b>e</b> raised to the power of X, where <b>e</b> is the base of natural logarithm.</returns>*)

function Lnf(const X: single): single; overload; 

(*<summary><c>e^x</c> function.</summary>
  <returns>Returns the value of <b>e</b> raised to the power of X, where <b>e</b> is the base of natural logarithm.</returns>*)

function Expf(const X: single): single; overload; 

(*<summary>Cosine function.</summary>
  <returns>Returns the cosine of argument X, in radians.</returns>

  <seealso cref="ArcCosf"/>*)

function Cosf(const X :single): single; overload;  

(*<summary>Sine function.</summary>
  <returns>Returns the sine of argument X, in radians.</returns>

  <seealso cref="ArcSinf"/>*)

function Sinf(const X :single): single; overload; 


(*<summary>Inverse tangens.</summary>
  <returns>Returns the inverse tangens of a given number X.</returns>

  <seealso cref="ArcTan2"/>
  <seealso cref="Tan"/>*)

function ArcTanf(const X :single): single; overload; 

(*<summary>Square root.</summary>
  <returns>Returns single precision square root of the given number X.</returns>*)

function Sqrtf(const X: single): single; overload; 







procedure FreeAndNil(var Obj);






(*<summary>Sine.</summary>
  <returns>the sine of a complex argument X.</returns>*)

function Sin(const X: TCplx): TCplx; overload;

function Sin(const X: TSCplx): TSCplx; overload;
(*<summary>Sine.</summary>
  
<remarks>Calculates the sine of a complex argument X.
</remarks>

  <param name="X">Defines complex argument for function.</param>
  <param name="result">Returns the result of function operation.</param>*)

procedure Sin(const X: TCplx; out result: TCplx); overload;

procedure Sin(const X: TSCplx; out result: TSCplx); overload;

(*<summary>Cosine.</summary>
  <returns>the cosine of a complex argument X.</returns>*)

function Cos(const X: TCplx): TCplx; overload;

function Cos(const X: TSCplx): TSCplx; overload;

(*<summary>Cosine.</summary>
  
<remarks>Calculates the cosine of a complex argument X.
</remarks>

  <param name="X">Defines complex argument for function.</param>
  <param name="result">Returns the result of function operation.</param>*)

procedure Cos(const X: TCplx; out result: TCplx); overload;

procedure Cos(const X: TSCplx; out result: TSCplx); overload;

(*<summary>Tangens.</summary>
  <returns>the tangens of real argument X, in radians, where Tan(x) is defined as:

    <c>Tan(X) = Sin(X)/Cos(X)</c>
  </returns>

  <seealso cref="ArcTan"/>*)

function Tan(const X :double): double; overload; 

function Tanf(const X :single): single; overload; 

(*<summary>Tangens.</summary>
  <returns>Returns the tangens of complex argument X, in radians.</returns>*)

function Tan(const X: TCplx): TCplx; overload;

function Tan(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse sine.</summary>
 <returns>Returns the inverse sine of X. X must be between -1 and 1.</returns>
  
<remarks>The return value will be in the range <c>[-PI/2,PI/2]</c>, in radians for a real argument. To obtain
  an inverse sine for X outside the interval <c>[-1,1]</c> use the complex version of the function.
</remarks>


  <seealso cref="Sin"/>*)

function ArcSin(const X :double): double; overload; 

function ArcSinf(const X :single): single; overload; 

(*<summary>Inverse sine.</summary>
  <returns>Returns the inverse sine of X. X must be between -1 and 1.</returns>
  
<remarks>To obtain an inverse sine for X outside the interval <c>[-1,1]</c> use this version of function.
</remarks>
*)

function ArcSin(const X: TCplx): TCplx; overload;

function ArcSin(const X: TSCplx): TSCplx; overload;
(*<summary>Inverse cosine.</summary>
  <returns>Returns the inverse cosine of X. X must be between -1 and 1.</returns>
  
<remarks>The return value will be on closed interval <c>[-PI/2,PI/2]</c>, in radians for a real argument. To obtain
  an inverse cosine for X outside the interval <c>[-1,1]</c> use the complex version of the function.
</remarks>


  <seealso cref="Cos"/>*)

function ArcCos(const X :double): double; overload; 

function ArcCosf(const X :single): single; overload; 
(*<summary>Inverse cosine of X.</summary>
  <returns>Returns the inverse cosine of complex argument X.</returns>
  
<remarks>To obtain an inverse cosine for X outside the interval <c>[-1,1]</c> use this version of the function.
</remarks>
*)

function ArcCos(const X: TCplx): TCplx; overload; 

function ArcCos(const X: TSCplx): TSCplx; overload; 
(*<summary>Inverse tangens of X.</summary>
  <returns>Returns the inverse tangens of complex argument X.</returns>*)

function ArcTan(const X: TCplx): TCplx; overload;

function ArcTan(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse tangens of Y/X.</summary>
  <returns>Returns the inverse tangens of Y/X, and returns an angle in the correct quadrant.</returns>
  
<remarks>The return value will fall in the closed interval <c>[-PI,PI]</c> radians.

  Note
    <see cref="ArcTan"/> is calculated as <c>ArcTan2(1, X)</c>.
</remarks>


  <seealso cref="ArcTan"/>
  <seealso cref="Tan"/>*)

function ArcTan2(const Y, X: double): double; overload; 

function ArcTan2f(const Y, X:single): single; overload; 

(*<summary>Sine and cosine in single pass.</summary>
  <param name="X">Defines argument for sin and cos functions.</param>
  <param name="SinX">Returns the hyperbolic sine.</param>
  <param name="CosX">Returns the hyperbolic cosine.</param>

  
<remarks>Computes sine and cosine in one pass. Call this routine if you require sine and cosine.
</remarks>

  <seealso cref="Sin"/>
  <seealso cref="Cos"/>*)

procedure SinCos(const X: double; out SinX, CosX : double); overload;      

procedure SinCosf(const X: single; out SinX, CosX : single); overload;     

(*<summary>Hyperbolic sine.</summary>
  <returns>Returns the hyperbolic sine of real argument X.</returns>
  <seealso cref="ArcSinh"/>*)


function Sinh(const X: double): double; overload; 

function Sinhf(const X: single): single; overload; 

(*<summary>Hyperbolic sine.</summary>
  <returns>Returns the hyperbolic sine of complex argument X.</returns>*)

function Sinh(const X: TCplx): TCplx; overload;

function Sinh(const X: TSCplx): TSCplx; overload;

(*<summary>Hyperbolic sine.</summary>
  <param name="X">Defines argument for function.</param>
  <param name="result">Returns the hyperbolic sine.</param>*)

procedure Sinh(const X: TCplx; out result: TCplx); overload;

procedure Sinh(const X: TSCplx; out result: TSCplx); overload;

(*<summary>Hyperbolic cosine.</summary>
  <returns>Returns the hyperbolic cosine of real argument X.</returns>

  <seealso cref="ArcCosh"/>*)

function Cosh(const X: double): double; overload; 

function Coshf(const X: single): single; overload; 
(*<summary>Hyperbolic cosine.</summary>
  <returns>Returns the hyperbolic cosine of complex argument X.</returns>*)

function Cosh(const X: TCplx): TCplx; overload;

function Cosh(const X: TSCplx): TSCplx; overload;

(*<summary>Hyperbolic cosine.</summary>
  <param name="X">Defines argument for function.</param>
  <param name="result">Returns the hyperbolic cosine.</param>*)

procedure Cosh(const X : TCplx; out result: TCplx); overload;

procedure Cosh(const X : TSCplx; out result: TSCplx); overload;

(*<summary>Hyperbolic cosine.</summary>
  <returns>Returns the hyperbolic tangens of real argument X.</returns>

  <seealso cref="ArcTanh"/>*)

function Tanh(const X: double): double; overload; 

function Tanhf(const X: single): single; overload; 

(*<summary>Hyperbolic cosine.</summary>
  <returns>Returns the hyperbolic tangens of complex argument X.</returns>

  <seealso cref="ArcTanh"/>*)

function Tanh(const X: TCplx): TCplx; overload;

function Tanh(const X: TSCplx): TSCplx; overload;
(*<summary>Hyperbolic tangens.</summary>
  <returns>Returns the hyperbolic tangens od real argument X by using SinCosh method:
    <c>Sinh(x)/Cosh(x)</c>.</returns>

  <seealso cref="Tanh"/>*)

function TanhX(X: double): double; overload;

function TanhXf(X: single): single; overload;

(*<summary>Inverse hyperbolic sine.</summary>
  <returns>Returns the inverse hyperbolic sine of real argument X.</returns>
  <seealso cref="Sinh"/>*)

function ArcSinh(const X: double): double; overload; 

function ArcSinhf(const X: single): single; overload; 

(*<summary>Inverse hyperbolic sine.</summary>
  <returns>Returns the inverse hyperbolic sine of complex argument X.</returns>*)

function ArcSinh(const X: TCplx): TCplx; overload;

function ArcSinh(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse hyperbolic cosine.</summary>
  <returns>the inverse hyperbolic cosine of real argument X.</returns>

  <seealso cref="Cosh"/>*)

function ArcCosh(const X: double): double; overload; 

function ArcCoshf(const X: single): single; overload; 

(*<summary>Inverse hyperbolic cosine.</summary>
  <returns>the inverse hyperbolic cosine of complex argument X.</returns>*)

function ArcCosh(const X: TCplx): TCplx; overload;

function ArcCosh(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse hyperbolic tangens.</summary>
  <returns>the inverse hyperbolic tangens of real argument X.</returns>

  <seealso cref="Tanh"/>*)

function ArcTanh(const X: double): double; overload; 

function ArcTanhf(const X: single): single; overload; 

(*<summary>Inverse hyperbolic tangens.</summary>
  <returns>the inverse hyperbolic tangens of complex argument X.</returns>*)

function ArcTanh(const X: TCplx): TCplx; overload;

function ArcTanh(const X: TSCplx): TSCplx; overload;

(*<summary>The hyperbolic sine and cosine.</summary>
  <param name="X">Defines argument for sinh and cosh functions.</param>
  <param name="SinhX">Returns the hyperbolic sine.</param>
  <param name="CoshX">Returns the hyperbolic cosine.</param>

  
<remarks>Calculates the hyperbolic sine and cosine in one pass. Use this version if you
  require both Sinh and Cosh.
</remarks>


  <seealso cref="Sinh"/>
  <seealso cref="Cosh"/>
  <seealso cref="Tanh"/>*)

procedure SinhCosh(const X: double; out SinhX, CoshX : double); overload;

procedure SinhCoshf(const X: single; out SinhX, CoshX : single); overload;

(*<summary>The norm of z.</summary>
  <returns>the norm of a complex number z: <c>|z| = Sqrt[z z*]</c>.</returns>*)

function CAbs(const Z: TCplx): double; overload;

function CAbs(const Z: TSCplx): single; overload;

(*<summary>Returns the approximation of the norm of Z.</summary>
  
<remarks>The following relations are true: <c>(CAbs(Z1) &lt; CAbs(Z2)) &lt; = &gt; (CAbs1(Z1) &lt; CAbs1(Z2))</c>
  and <c>(CAbs(Z1) &gt; CAbs(Z2)) &lt; = &gt; (CAbs1(Z1) &gt; CAbs1(Z2))</c>. This function can be used
  to speed up comparing operations between complex numbers.
</remarks>

  <returns>the approximation of the norm of complex number: <c>|Z| = abs(Z.Re) + abs(Z.Im)</c>.</returns>*)

function CAbs1(const Z: TCplx): double;  overload;

function CAbs1(const Z: TSCplx): single;  overload;

(*<summary>The argument of Z.</summary>
  <returns>the argument of the complex number Z: <c>Arg(Z) = ArcTan(Z.Im/Z.Re)</c>.</returns>*)

function Arg(const Z: TCplx): double; overload;

function Arg(const Z: TSCplx): single; overload;

(*<summary>Fortran sign function.</summary>
  <returns>It uses slightly different definition for changing sign of a complex number:
    * if b>=0, it returns |a|,
    * otherwise, -|a|.
  </returns>*)

function Sign(const a,b: double): double; overload;

function Signf(const a,b: single): single; overload;
(*<summary>Changes the sign of a complex number.</summary>*)

function Sign(const Z: TCplx): TCplx; overload;   

function Sign(const Z: TSCplx): TSCplx; overload;   
(*<summary>Limit the range X.</summary>
  
<remarks>Change X to stay within [Low,High] closed interval.
</remarks>

  <param name="Low">Lower bound in [Low,High].</param>
  <param name="High">Upper bound in [Low,High].</param>
  <param name="X">Modify x to lie within closed interval [Low,High].</param>*)

procedure LimitRange(const Low: double; var X: double; const High: double); overload;

procedure LimitRangef(const Low: single; var X: single; const High: single); overload;
(*<summary>Limit the range X.</summary>
  
<remarks>Change X to stay within [Low,High] range. In this case the parameters are integers.
</remarks>


  <seealso cref="EnsureRange"/>
  <seealso cref="CheckRange"/>*)

procedure LimitRange(const Low: integer; var X: integer; const High: integer); overload;    

(*<summary>Changes the sign of Z.</summary>
  <returns>the sign of Z: <c>Z becomes -Z</c></returns>*)

function Neg(const A: TCplx): TCplx; overload; 

function Neg(const A: TSCplx): TSCplx; overload; 

(*<summary>Conjugate complex number.</summary>
  <returns>conjugated complex number <c>Z becomes Z*</c>.</returns>*)

function Conj(const A: TCplx): TCplx; overload; 

function Conj(const A: TSCplx): TSCplx; overload; 

(*<summary>Complex addition.</summary>
  <returns>the result of complex addition: <c>Z = A+B.</c></returns>*)

function CAdd(const A, B: TCplx): TCplx; overload; 

function CAdd(const A, B: TSCplx): TSCplx; overload; 

(*<summary>Complex subtraction.</summary>
  <returns>the result of complex subtraction: <c>Z = A-B.</c></returns>*)

function CSub(const A, B : TCplx): TCplx; overload; 

function CSub(const A, B : TSCplx): TSCplx; overload; 

(*<summary>Complex division.</summary>
  <returns>the result of complex division: <c>Z = A/B.</c></returns>

  <seealso cref="CMul"/>*)

function CDiv(const A, B : TCplx): TCplx; overload; 

function CDiv(const A, B : TSCplx): TSCplx; overload; 


(*<summary>Complex multipliction.</summary>
  <returns>the result of complex multiplication: <c>Z = A*B.</c></returns>

  <seealso cref="CDiv"/>*)

function CMul(const A, B : TCplx): TCplx; overload; 

function CMul(const A, B : TSCplx): TSCplx; overload; 

(*<summary>Converts two real numbers to complex number.</summary>
  <returns>complex number: <c>A+i*b</c>.</returns>*)

function Cplx(A: double): TCplx; overload; 
function Cplx(A: double; B: double): TCplx; overload; 
function CplxDouble(A: double; B: double = 0): TCplx; overload; 

function CplxSingle(A: single; B: single = 0): TSCplx; overload; 



(*<summary>Square root of a complex number.</summary>
  <returns>the square root of a complex number.</returns>

  <seealso cref="CSqr"/>*)

function CSqrt(const A: TCplx): TCplx; overload;

function CSqrt(const A: TSCplx): TSCplx; overload;

(*<summary>The root of complex number.</summary>
  <returns>'N' roots of 'A' by varying 'K' from 0..N-1.</returns>
  
<remarks>This routine uses DeMoivre's theorem to calculate all roots of complex number.
</remarks>
*)

function Root(const A: TCplx; K, N: Integer): TCplx;  overload;

function Root(const A: TSCplx; K, N: Integer): TSCplx;  overload;

(*<summary>Modified bessel function of the first kind.</summary>
  <seealso cref="BesselJ0"/>*)

function BesselI0(const X:  TCplx): TCplx; overload;

function BesselI0(const X:  TSCplx): TSCplx; overload;

(*<summary>Bessel function of the first kind.</summary>
  <seealso cref="BesselI0"/>*)

function BesselJ0(const X:  TCplx): TCplx; overload;

function BesselJ0(const X:  TSCplx): TSCplx; overload;
(*<summary>Gamma function (real).</summary>
  <returns>the gamma function. Gamma function is used for approximating the factorial of a number.</returns>

  <seealso cref="LnGamma"/>*)
function Gamma(X: double): double; overload;
(*<summary>Gamma function (complex).</summary>
  <seealso cref="LnGamma"/>*)

function Gamma(const X: TCplx): TCplx; overload;
(*<summary>Natural logarithm of the gamma function.</summary>
  <returns>the natural logarithm of the gamma function Ln(Gamma(X)).</returns>
  <seealso cref="Gamma"/>*)

function LnGamma(const X: Double): Double; overload;
(*<summary>Natural logarithm of the gamma function.</summary>
  <returns>the natural logarithm of the gamma function Ln(Gamma(X)).</returns>
  
<remarks>Complex version.
</remarks>

  <seealso cref="Gamma"/>*)

function LnGamma(const X: TCplx): TCplx; overload;


function SgnGamma(const X: double) : Integer; overload;


(*<summary>Factorial N!.</summary>
  <returns>the factorial <c>(=n!)</c> of n.</returns>
  
<remarks>First 50 numbers are precomputed. Although the argument is integer,
  the result is real. The result can exceed integer range. If you exceed the TSample range the function returns
  Inf.
</remarks>
*)

function Fact(const N: integer): double; overload;    

(*<summary>Secant.</summary>
  <returns>the secant of the real argument X.</returns>
  <seealso cref="ArcSec"/>*)

function Sec(X: double): double; overload;

function Secf(X: single): single; overload;

(*<summary>Secant.</summary>
  <returns>the secant of the complex argument X.</returns>*)

function Sec(const X: TCplx): TCplx; overload;

function Sec(const X: TSCplx): TSCplx; overload;

(*<summary>Hyperbolic secant.</summary>
  <returns>the hyperbolic secant of the real argument X.</returns>
  <seealso cref="ArcSech"/>*)

function Sech(X: double): double; overload;

function Sechf(X: single): single; overload;

(*<summary>Hyperbolic secant.</summary>
  <returns>the hyperbolic secant of the complex argument X.</returns>*)

function Sech(const X: TCplx): TCplx; overload;

function Sech(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse secant.</summary>
  <returns>the inverse secant of the real argument X.</returns>
  <seealso cref="Sec"/>*)

function ArcSec(X: double): double; overload;

function ArcSecf(X: single): single; overload;

(*<summary>Inverse secant.</summary>
  <returns>the inverse secant of the complex argument X.</returns>*)

function ArcSec(const X: TCplx): TCplx; overload;

function ArcSec(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse hyperbolic secant.</summary>
  <returns>the inverse hyperbolic secant of the real argument X.</returns>
  <seealso cref="Sech"/>*)

function ArcSech(X: double): double; overload;

function ArcSechf(X: single): single; overload;

(*<summary>Inverse hyperbolic secant.</summary>
  <returns>the inverse hyperbolic secant of the real argument X.</returns>*)

function ArcSech(const X: TCplx): TCplx; overload;

function ArcSech(const X: TSCplx): TSCplx; overload;

(*<summary>Cosecant.</summary>
  <returns>the cosecant of the real argument X.</returns>
  <seealso cref="ArcCsc"/>*)

function Csc(X: double): double; overload;

function Cscf(X: single): single; overload;

(*<summary>Cosecant.</summary>
  <returns>the cosecant of the complex argument X.</returns>*)

function Csc(const X: TCplx): TCplx; overload;

function Csc(const X: TSCplx): TSCplx; overload;

(*<summary>Hyperbolic cosecant.</summary>
  <returns>the hyperbolic cosecant of the real argument X.</returns>
  <seealso cref="ArcCsch"/>*)

function Csch(X: double): double; overload;

function Cschf(X: single): single; overload;

(*<summary>Hyperbolic cosecant.</summary>
  <returns>the hyperbolic cosecant of the complex argument X.</returns>*)

function Csch(const X: TCplx): TCplx; overload;

function Csch(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse cosecant.</summary>
  <returns>the inverse cosecant of the real argument X.</returns>
  <seealso cref="Csc"/>*)

function ArcCsc(X: double): double; overload;

function ArcCscf(X: single): single; overload;

(*<summary>Inverse cosecant.</summary>
  <returns>the inverse cosecant of the complex argument X.</returns>*)

function ArcCsc(const X: TCplx): TCplx; overload;

function ArcCsc(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse hyperbolic cosecant.</summary>
  <returns>the inverse hyperbolic cosecant of the real argument X.</returns>
  <seealso cref="Csch"/>*)

function ArcCsch(X: double): double; overload;

function ArcCschf(X: single): single; overload;

(*<summary>Inverse hyperbolic cosecant.</summary>
  <returns>the inverse hyperbolic cosecant of the complex argument X.</returns>*)

function ArcCsch(const X: TCplx): TCplx; overload;

function ArcCsch(const X: TSCplx): TSCplx; overload;

(*<summary>Cotangent.</summary>
  <returns>the cotangent of real argument X.</returns>
  <seealso cref="ArcCot"/>
  <seealso cref="Tan"/>*)

function Cot(X: double): double; overload;

function Cotf(X: single): single; overload;

(*<summary>Cotangent.</summary>
  <returns>the cotangent of complex argument X.</returns>*)

function Cot(const X: TCplx): TCplx; overload;

function Cot(const X: TSCplx): TSCplx; overload;

(*<summary>Hyperbolic cotangent.</summary>
  <returns>the hyperbolic cotangent of real argument X.</returns>
  <seealso cref="ArcCoth"/>*)

function Coth(X: double): double; overload;

function Cothf(X: single): single; overload;

(*<summary>Hyperbolic cotangent.</summary>
  <returns>the hyperbolic cotangent of complex argument X.</returns>*)

function Coth(const X: TCplx): TCplx; overload;

function Coth(const X: TSCplx): TSCplx; overload;

(*<summary>Inverse cotangent.</summary>
  <returns>the inverse cotangent of real argument X.</returns>
  <seealso cref="Cot"/>
  <seealso cref="ArcTan"/>*)

function ArcCot(X: double): double; overload;

function ArcCotf(X: single): single; overload;
(*<summary>Inverse cotangent.</summary>
  <returns>the inverse cotangent of complex argument X.</returns>*)

function ArcCot(const X: TCplx): TCplx; overload;

function ArcCot(const X: TSCplx): TSCplx; overload;
(*<summary>Inverse hyperbolic cotangent.</summary>
  <returns>the inverse hyperbolic cotangent of real argument X.</returns>
  <seealso cref="Coth"/>*)

function ArcCoth(X: double): double; overload;

function ArcCothf(X: single): single; overload;
(*<summary>Inverse hyperbolic cotangent.</summary>
  <returns>the inverse hyperbolic cotangent of complex argument X.</returns>*)

function ArcCoth(const X: TCplx): TCplx; overload;

function ArcCoth(const X: TSCplx): TSCplx; overload;

(*<summary>The reminder after division.</summary>
  <returns>the reminder after the division: <c>X/Y</c>.</returns>*)

function Rem(const X,Y:double): double; overload;

function Remf(const X,Y:single): single; overload;
(*<summary>Reminder after division.</summary>
  <returns>the reminder after complex division: <c>X/Y</c>.</returns>*)

function Rem(const X, Y: TCplx): TCplx; overload;   

function Rem(const X, Y: TSCplx): TSCplx; overload;   
(*<summary>Complex truncate function.</summary>
  <returns>the truncate function to real and imginary part of the X parameter.</returns>*)

function CTrunc(const X: TCplx): TCplx; overload;

function CTrunc(const X: TSCplx): TSCplx; overload;

(*<summary>Complex round function.</summary>
  <returns>Round function to real and imginary part of the X parameter.</returns>*)

function CRound(const X: TCplx): TCplx; overload;

function CRound(const X: TSCplx): TSCplx; overload;

(*<summary>Complex frac function.</summary>
  <returns>the frac function to real and imaginary part of the X parameter.</returns>*)

function CFrac(const X: TCplx): TCplx; overload;   

function CFrac(const X: TSCplx): TSCplx; overload;   

(*<summary>The square of complex number.</summary>
  <returns>the square of complex number.</returns>
  <seealso cref="CSqrt"/>*)

function CSqr(const X: TCplx): TCplx; overload;

function CSqr(const X: TSCplx): TSCplx; overload;

(*<summary>Greatest common divisor.</summary>
  <returns>the greatest common divisor of X and Y. X and Y must be positive integers.</returns>
  <seealso cref="Lcm"/>*)

function Gcd(const X,Y: integer): integer;

(*<summary>Smallest common multiplier.</summary>
  <returns>the smallest common multiplier of X and Y. X and Y must be positive integers.</returns>
  <seealso cref="Gcd"/>*)

function Lcm(const X,Y: integer): integer; overload; 

(*<summary>Cartesian to polar coordinate transformation.</summary>
 <returns>complex number, transformed from cartesian to polar coordinate system.</returns>
  <seealso cref="PolarToCart"/>*)

function CartToPolar(const Z: TCplx): TCplx; overload;

function CartToPolar(const Z: TSCplx): TSCplx; overload;
(*<summary>Polar to cartesian coordinate transformation.</summary>
 <returns>complex number, transformed from polar to cartesian coordinate system.</returns>
  <seealso cref="CartToPolar"/>*)

function PolarToCart(const Z: TCplx): TCplx; overload;

function PolarToCart(const Z: TSCplx): TSCplx; overload;

(*<summary>Round towards positive infinity.</summary>
  <returns>the smallest integer greater than or equal to X.

    <c>Ceil(-2.8) = -2</c><para/>
    <c>Ceil(2.8) = 3</c><para/>
    <c>Ceil(-1.0) = -1</c>
  </returns>
  <seealso cref="Floor"/>*)

function Ceil(const X: double): Int64;  overload; 

function Ceilf(const X: single): Int64;  overload; 
(*<summary>Round towards positive infinity.</summary>
  
<remarks>Real and imaginary part are handled independently.
</remarks>
*)

function Ceil(const X: TCplx): TCplx; overload;

function Ceil(const X: TSCplx): TSCplx; overload;

(*<summary>Round towards negative infinity.</summary>
  <returns>the biggest integer less than or equal to X.

    <c>Floor(-2.8) = -3</c><para/>
    <c>Floor(2.8) = 2</c><para/>
    <c>Floor(-1.0) = -1</c>
  </returns>

  <seealso cref="Ceil"/>*)

function Floor(const X: double): Int64; overload; 

function Floorf(const X: single): Int64; overload; 

(*<summary>Round towards negative infinity.</summary>
  
<remarks>Real and imaginary part are handled independently.
</remarks>
*)

function Floor(const X: TCplx): TCplx; overload;

function Floor(const X: TSCplx): TSCplx; overload;

(*<summary>Compares two numbers.</summary>
   <returns>true, if complex A and B are binary equal.</returns>*)

function Equal(const A,B: TCplx): boolean; overload;

function Equal(const A,B: TSCplx): boolean; overload;

(*<summary>Compares two numbers.</summary>
  <returns>true, if A and B are equal within a given Tolerance:<para/>
    <c>(abs(A.Re - B.Re) &lt;= Tolerance) and (abs(A.Im - B.Im) &lt;= Tolerance)</c>
  </returns>*)

function Equal(const A,B: TCplx; const Tolerance: double): boolean; overload;

function Equal(const A,B: TSCplx; const Tolerance: single): boolean; overload;

(*<summary>Compares two numbers.</summary>
  <returns>true, if A and B are equal.</returns>*)

function Equal(const A: TCplx; const B: double): boolean; overload;

function Equal(const A: TSCplx; const B: single): boolean; overload;
(*<summary>Compares two numbers.</summary>
  <returns>true, if A and B are equal.</returns>*)

function Equal(const A: double; const B: TCplx): boolean; overload;

function Equal(const A: single; const B: TSCplx): boolean; overload;

(*<summary>Compares two numbers.</summary>
  <returns>Returns true, if A and B are equal within a given Tolerance.
    <c>result := (abs(A-B) &lt;= Tol);</c>
  </returns>*)

function Equal(const A, B: double; const Tol: double): boolean; overload;

function Equalf(const A, B: single; const Tol: single): boolean; overload;

(*<summary>Compares two numbers.</summary>
  <returns>True if both A and B are binary equal.</returns>*)

function Equal(const A, B: double): boolean; overload;

function Equalf(const A, B: single): boolean; overload;


function Bigger2(const A, B: TCplx): boolean; overload;   
function Smaller2(const A, B: TCplx): boolean; overload;    
function Bigger2(const A, B: TSCplx): boolean; overload;   
function Smaller2(const A, B: TSCplx): boolean; overload;    


(*<summary>Compares two complex numbers.</summary>
 <returns>True if X is bigger then Y. If absolute values are equal it compares the phase</returns>*)

function Bigger(const X, Y: TCplx): boolean; overload;

function Bigger(const X, Y: TSCplx): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function Bigger(const X: TCplx;const Y: double): boolean; overload;

function Bigger(const X: TSCplx;const Y: single): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function Bigger(const X: double; const Y: TCplx): boolean; overload;

function Bigger(const X: single; const Y: TSCplx): boolean; overload;

(*<summary>Compare two real numbers.</summary>
  <returns>true, if A is bigger than B within defined tolerance: <c>|A-b| &gt; Tol </c>.</returns>*)

function Bigger(const A, B: double; const Tol: double = 1E-4): boolean; overload;   

function Biggerf(const A, B: single; const Tol: single = 1E-4): boolean; overload;   

(*<summary>Compares two complex numbers.</summary>
  <returns>true if X is smaller than Y. If absolute values are equal it compares the phase.</returns>*)

function Smaller(const X, Y: TCplx): boolean; overload;

function Smaller(const X, Y: TSCplx): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function Smaller(const X: TCplx;const Y: double): boolean; overload;

function Smaller(const X: TSCplx;const Y: single): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function Smaller(const X: double;const Y: TCplx): boolean; overload;

function Smaller(const X: single;const Y: TSCplx): boolean; overload;
(*<summary>Compare two real numbers.</summary>
  <returns>true, if A is smaller than B within defined tolerance: <c>|A-B| &lt; Tol </c>.</returns>*)

function Smaller(const A, B: double; const Tol: double = 1E-4): boolean; overload;   

function Smallerf(const A, B: single; const Tol: single = 1E-4): boolean; overload;   

(*<summary>Compare two complex numbers.</summary>
  <returns>True if X is bigger or equal to Y. If absolute values are equal it compares the phase.</returns>*)

function BiggerOrEqual(const X, Y: TCplx): boolean; overload;

function BiggerOrEqual(const X, Y: TSCplx): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function BiggerOrEqual(const X: TCplx; Y: double): boolean; overload;

function BiggerOrEqual(const X: TSCplx; Y: single): boolean; overload;
(*<summary>Compare one real and one complex number.</summary>*)


function BiggerOrEqual(X: double; const Y: TCplx): boolean; overload;

function BiggerOrEqual(X: single; const Y: TSCplx): boolean; overload;

(*<summary>Compare two complex numbers.</summary>
  <returns>True if X is smaller or equal to Y. If absolute values are equal it compares the phase.</returns>*)

function SmallerOrEqual(const X, Y: TCplx): boolean; overload;

function SmallerOrEqual(const X, Y: TSCplx): boolean; overload;
(*<summary>Compare one real and one complex number.</summary>*)

function SmallerOrEqual(const X: TCplx; Y: double): boolean; overload;

function SmallerOrEqual(const X: TSCplx; Y: single): boolean; overload;

(*<summary>Compare one real and one complex number.</summary>*)

function SmallerOrEqual(X: double; const Y: TCplx): boolean; overload;

function SmallerOrEqual(X: single; const Y: TSCplx): boolean; overload;

(*<summary>Closest power of 2 to n.</summary>
  <returns>the first smaller or equal power of two, if n is not power of two.</returns>*)

function SmallestExp2(n: integer): integer; overload;

(*<summary>Returns first number being power of two that is equal or greater than n.</summary>
  <returns>the first number being power of two that is equal or greater than n.</returns>
  
<remarks>It is much faster than the call of:

  <c>Exp2(Ceil(Log2(n)))</c>
</remarks>


  <seealso cref="LargestLog2"/>*)

function LargestExp2(n: integer): integer; overload;

(*<summary>Power of two which gives the first number that is equal or greater than n.</summary>
  <returns>power of two which gives the first number that is equal or greater than n.</returns>

  <seealso cref="LargestExp2"/>*)

function LargestLog2(n: integer): integer; overload;    inline; 

(*<summary>Clears pending FPU exceptions.</summary>
  
<remarks>Clears pending FPU exceptions. Use this function to clear pending FPU exceptions before calling functions that turn FPU exception on, but do
  not clear them. This includes System.Trunc, System.Frac and System.Integer functions in all Delphi versions before Delphi 6.<para/>
  Math387 has an alternative implementation of the Trunc function that overrides the default System.Trunc function. Exception flags are set
  by the FPU when the result of a floating point operation is NAN or INF. If the exceptions are not enabled nothing happens, but the exception
  flags remain set. If you then enable FPU exceptions without first clearing the exception flags, an exception will be raised. Because those exceptions
  do not come from the current code (where the debugger stops), they make the debugging process much more difficult. A typical error indicating
  this problem is a Delphi message about a " Invalid floating point operation". With introduction of Delphi 6, this problem has been greatly reduced.
  When using third party code this problem might resurface, if the code requires floating exceptions to be turned on. In such cases you should call
  ClearFPU before enabling floating exceptions and disable them again when returning back to MtxVec code. Floating point exceptions must be turned
  off for MtxVec, or some optimization algorithms will not be able to converge.
</remarks>
*)

procedure ClearFPU; 

(*<summary>The complex number Z formated as a string.</summary>
  <returns>the complex number Z formated as a string.</returns>
  <param name="Z">Defines complex number for formatting: <c>Z=Re+I*Im</c>.</param>
  <param name="ReFormat">Defines string format used for formating real part of <c>Z</c>.</param>
  <param name="ImFormat">Defines string format used for formating imaginary part of <c>Z</c>.</param>

  
<remarks>If ReFormat and ImFormat parameters are empty strings, the function calls <see cref="CplxToStr"/>.
</remarks>


  <seealso cref="FormatSample"/>*)

function FormatCplx(const Z: TCplx; const ReFormat: string = ' 0.###;-0.###';
                                    const ImFormat: string = '+0.###i;-0.###i'): string; overload;

function FormatCplx(const Z: TSCplx; const ReFormat: string = ' 0.###;-0.###';
                                     const ImFormat: string = '+0.###i;-0.###i'): string; overload;
(*<summary>The double number X formated as string.</summary>
  <returns>the double number, formated as string.</returns>

  
<remarks>The Format parameter defines the valid format string. This function is similar to Delphi FormatFloat function,
  but it also supports single precision type by displaying only 7 valid digits instead of Delphi FormatFloat 15 valid digits.
</remarks>


  <seealso cref="SampleToStr"/>*)

function FormatSample(X: double; const Format: string = '0.###'): string; overload;

function FormatSamplef(X: single; const Format: string = '0.###'): string; overload;

(*<summary>The double number X formatted as string.</summary>
  <returns>the double number, formatted as string.</returns>*)

function FormatSample(const Format: string; X: double): string; overload;

function FormatSamplef(const Format: string; X: single): string; overload;

(*<summary>Converts a string to complex number.</summary>
  <returns>string, converted to complex number.</returns>
  
<remarks>The complex number must be in the following format: <c>a + bi</c>.
    Valid examples: <c>+6, 6i, 2.4E-2i, -6E+2+2.4E-2i, 2+3i</c>.
</remarks>


  <seealso cref="CplxToStr"/>*)

function StrToCplx(const Source: string): TCplx; overload;

function StrToCplxSingle(const Source: string): TSCplx; overload;
(*<summary>Converts complex number to a string.</summary>
  <returns>complex number, converted to string.</returns>
  
<remarks>The Digits parameter controls the minimum number of zeros in the exponent (between 0 and 4).
  The conversion uses general number format with 15 significant digits for double precision and 7 digits
  if you are using single precision version of Math387.
</remarks>


  <seealso cref="StrToCplx"/>*)

function CplxToStr(const Z: TCplx;const Digits: integer = 0; Precision: integer = 15): string; overload;

function CplxToStr(const Z: TSCplx;const Digits: integer = 0; Precision: integer = 7): string; overload;

(*<summary>Converts string to a value.</summary>
  <returns>original string, transformed to double value.</returns>
  
<remarks>This routine is the same as StrToFloat except that it can also handle NAN and INF values.
  It will work with both ',' and '.' as decimal separator, but the code will run 2x faster for the
  currently default decimal separator. StrToVal is used also by <see cref="StrToCplx"/> function.
</remarks>
*)

function StrToVal(const Source: string): double;
(*<summary>Converts string to a double value.</summary>
  <returns>string, converted to double value.</returns>

  <seealso cref="SampleToStr"/>*)

function StrToSample(const Source: string): double; overload;
(*<summary>Converts string to a double value.</summary>
  <returns>string, converted to double value.</returns>

  <seealso cref="SampleToStr"/>*)

function StrToDouble(const Source: string): double; overload;
(*<summary>Converts string to a single value.</summary>
  <returns>string, converted to single value.</returns>

  <seealso cref="SampleToStr"/>*)

function StrToSingle(const Source: string): single; overload;
(*<summary>Converts TSample value to a string.</summary>
  <returns>TSample value, converted to string.</returns>
  
<remarks>The Digits parameter controls the minimum number of zeros in the exponent (between 0 and 4).
  The Precision parameter controls the number of significant digints in string representation of TSample value.
</remarks>


  <seealso cref="StrToSample"/>*)

function SampleToStr(const X: double; const Digits: integer; Precision: integer = 15 ): string; overload;

function SampleToStrf(const X: single; const Digits: integer; Precision: integer = 7  ): string; overload;


function SampleToStr(const X: double): string; overload;

function SampleToStrf(const X: single): string; overload;

(*<summary>Gets double number, extracted from string source.</summary>
  <returns>double number that was extracted from string Source looking from Offset and ended with Delimiter.</returns>
  
<remarks>The Offset is modified to point to the first character after first found Delimiter in Source string. You can use
  ExtractSample to process a string of complex numbers separated by Delimiter. If the Delimiter is not Found,
  the function returns zero.
</remarks>


  <seealso cref="ExtractCplx"/>*)

function ExtractSample(const Source: string; var Offset: integer; const Delimiter: string): double; overload;


(*<summary>Gets integer number, extracted from string source.</summary>
  <returns>Integer number that was extracted from string Source looking from Offset and ended with Delimiter.</returns>
  
<remarks>The Offset is modified to point to the first character after first found Delimiter in Source string. You can use
  ExtractInteger to process a string of integer numbers separated by Delimiter. If the Delimiter is not Found,
  the function returns zero.
</remarks>


  <seealso cref="ExtractCplx"/>*)

function ExtractInteger(const Source: string; var Offset: integer;const Delimiter: string): integer; overload;
(*<summary>Gets a complex number, extracted from string source.</summary>
  <returns>a complex number that was extracted from string Source looking from Offset and ended with Delimiter.</returns>
  
<remarks>The Offset is modified to point to the first character after first found Delimiter in Source string. You can use
  this routine for processing a string of complex numbers separated by Delimiter. If the Delimiter is not Found,
  the function returns zero.
</remarks>


  <seealso cref="ExtractSample"/>*)

function ExtractCplx(const Source: string; var Offset: integer; const Delimiter: string): TCplx; overload;
function ExtractCplxSingle(const Source: string; var Offset: integer; const Delimiter: string): TSCplx; overload;

(*<summary>Add two numbers.</summary>*)

function  CAdd(const A: TCplx; const B: double): TCplx; overload;

function  CAdd(const A: TSCplx; const B: single): TSCplx; overload;

(*<summary>Add two numbers.</summary>*)

function  CAdd(const A: double; const B: TCplx): TCplx; overload; 

function  CAdd(const A: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Subtract two numbers.</summary>*)

function CSub(const A: TCplx; const B: double): TCplx; overload; 

function CSub(const A: TSCplx; const B: single): TSCplx; overload; 

(*<summary>Subtract two numbers.</summary>*)

procedure CSub(const A: TCplx; const B: double; out result: TCplx); overload; 

procedure CSub(const A: TSCplx; const B: single; out result: TSCplx); overload; 

(*<summary>Subtract two numbers.</summary>*)

function  CSub(const A: double; const B: TCplx): TCplx; overload; 

function  CSub(const A: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Subtract two numbers.</summary>*)

procedure CSub(const A: double; const B: TCplx; out result: TCplx); overload; 

procedure CSub(const A: single; const B: TSCplx; out result: TSCplx); overload; 

(*<summary>Multiply two numbers.</summary>*)

function  CMul(const A: TCplx; const B: double): TCplx; overload; 

function  CMul(const A: TSCplx; const B: single): TSCplx; overload; 

(*<summary>Multiply two numbers.</summary>*)

function  CMul(const A: double; const B: TCplx): TCplx; overload; 

function  CMul(const A: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Divide two numbers.</summary>*)

function  CDiv(const A: TCplx; const B: double): TCplx; overload; 

function  CDiv(const A: TSCplx; const B: single): TSCplx; overload; 

(*<summary>Divide two numbers.</summary>*)

function CDiv(const A: double; const B: TCplx): TCplx; overload; 

function CDiv(const A: single; const B: TSCplx): TSCplx; overload; 


function CSubMulI(const X: TCplx): TCplx; overload; 
function CInvMulI(const X: TCplx): TCplx; overload; 

function CSubMulI(const X: TSCplx): TSCplx; overload; 
function CInvMulI(const X: TSCplx): TSCplx; overload; 


(*<summary>Add imaginary value BI to complex number A.</summary>
  
<remarks>Add imaginary value BI to complex number A: <c>R = A + b*i;</c>
</remarks>
*)

function  CAddI(const A: TCplx; const BI: double): TCplx; overload; 

function  CAddI(const A: TSCplx; const BI: single): TSCplx; overload; 

(*<summary>Add imaginary value AI to complex number B.</summary>
  
<remarks>Add imaginary value AI to complex number B: <c>R = a*i + B;</c>
</remarks>
*)

function  CAddI(const AI: double; const B: TCplx): TCplx; overload; 

function  CAddI(const AI: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Subtract imaginary value BI from complex number A.</summary>
  
<remarks>Subtract imaginary value BI from complex number A: <c>R = A - b*i;</c>
</remarks>
*)

function  CSubI(const A: TCplx; const BI: double): TCplx; overload; 

function  CSubI(const A: TSCplx; const BI: single): TSCplx; overload; 

(*<summary>Subtract complex number B from imaginary number AI.</summary>
  
<remarks>Subtract complex number B from imaginary number AI: <c>R = a*i - B;</c>
</remarks>
*)

function  CSubI(const AI: double; const B: TCplx): TCplx; overload; 

function  CSubI(const AI: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Multiply two complex numbers.</summary>
  
<remarks>Multiply complex number A with imaginary number Bi: <c>R = A*b*i;</c>
</remarks>
*)

function  CMulI(const A: TCplx; const BI: double): TCplx; overload; 

function  CMulI(const A: TSCplx; const BI: single): TSCplx; overload; 
(*<summary>Multiply complex number B with imaginary number Ai.</summary>
  
<remarks>Multiply complex number B with imaginary number Ai: <c>R = a*i*B;</c>
</remarks>
*)

function  CMulI(const AI: double; const B: TCplx): TCplx; overload; 

function  CMulI(const AI: single; const B: TSCplx): TSCplx; overload; 

(*<summary>Returns the value of a degree measurement expressed in radians.</summary>
  
<remarks>Use DegToRad to convert angles expressed in degrees to the
  corresponding value in radians, where <c>radians = degrees(pi/180)</c>.
</remarks>
*)

function DegToRad(Deg: double): double; overload;

function DegToRadf(Deg: single): single; overload;
(*<summary>Converts radians to degrees.</summary>
  
<remarks>Use RadToDeg to convert angles measured in radians to degrees, where
  <c>degrees = radians(180/pi)</c>.
</remarks>
*)

function RadToDeg(Rad: double): double; overload;

function RadToDegf(Rad: single): single; overload;

(*<summary>Raises EMtxVecException exception.</summary>
  
<remarks>Raises <see cref="EMtxVecException"/> exception.
</remarks>
*)

procedure ERaise (const msg: string); overload;
procedure ERaiseFmt(const msg: string; const Args: array of const); overload;

(*<summary>Raises EMtxVecRangeError exception.</summary>
  
<remarks>Raises <see cref="EMtxVecRangeError"/> exception.
</remarks>
*)

procedure ERangeErrorRaise (const msg: string); overload;

procedure ERangeErrorRaiseFmt(const msg: string; const Args: array of const); overload;

(*<summary>Raises EMtxVecInvalidArgument exception.</summary>
  
<remarks>Raises <see cref="EMtxVecInvalidArgument"/> exception.
</remarks>
*)

procedure EInvalidArgumentRaise (const msg: string); overload;

procedure EInvalidArgumentRaiseFmt (const msg: string; const Args: array of const); overload;

(*<summary>Check the value.</summary>
  <returns>true if X is bigger or equal then Low and smaller or equal then High.</returns>*)

function CheckRange(const Low: double;const X: double;const High: double): boolean;  overload;  inline; 

function CheckRangef(const Low: single;const X: single;const High: single): boolean;  overload;  inline; 

function CheckRange(const Low: char;const X: char;const High: char): boolean; overload;  inline; 

(*<summary>Check the value.</summary>
  
<remarks>Parameters can be integers.
</remarks>
*)

function CheckRange(const Low: integer;const X: integer;const High: integer): boolean; overload;  inline; 

(*<summary>Check the value.</summary>
  
<remarks>Parameters can be integers.
</remarks>
*)

function CheckRange(const Low: Int64; const X: Int64; const High: Int64): boolean; overload;  inline; 

(*<summary>Limits the value.</summary>
  <returns>Low if X is smaller then Low and High if X is bigger then High.</returns>*)

function EnsureRange(const Low: double;const X: double;const High: double): double; overload;  inline; 

function EnsureRangef(const Low: single;const X: single;const High: single): single; overload;  inline; 

(*<summary>Limits the value.</summary>
  
<remarks>Parameters can be integers.
</remarks>
*)

function EnsureRange(const Low: integer;const X: integer;const High: integer): integer; overload;  inline; 


 function EnsureRange(const Low: Int64 ;const X: Int64; const High: Int64): Int64; overload;  inline; 




function Pos(const substr, str: string; Offset: integer): Integer; overload;
 
function TruncDiv(X: double; Divisor: integer): integer; overload; 
function TruncDivf(X: single; Divisor: integer): integer; overload; 
function TruncDiv(X: integer; Divisor: integer): integer; overload; 


(*<summary>Splits the floating point number to integer and fractional parts.</summary>
  
<remarks>Splits the floating point number to integer and fractional parts.
</remarks>

  <param name="X">Defines real number to be split into integer and fractional part.</param>
  <param name="FractionalPart">Returns the fractional part of X.</param>
  <returns>the integer part of X.</returns>*)

function TruncAndFracf(X: single; out FractionalPart: single): Int64; overload;
function TruncAndFrac(X: double; out FractionalPart: double): Int64; overload;
function ToSingle(X: double): single; 
function ToDouble(X: double): double; 



function PointerToDouble(P: PDouble): double;
function PointerToIndexDouble(P: PDouble; Index: integer): double;
function PointerToDCplx(P: PDCplx): TDCplx;
function PointerToIndexDCplx(P: PDCplx; Index: integer): TDCplx;
procedure WriteDouble(P: PDouble; S: double);
Procedure WriteDCplx(P: PDCplx; S: TDCplx);


function GlobalAllocUnmanaged(dwBytes: PointerInteger): PointerInteger; overload;
procedure GlobalFreeUnmanaged(var MemPointer: PointerInteger); overload;
procedure GlobalFreeUnmanaged(var MemPointer: PAPointer); overload;



function AllignMemp(Address: PAPointer; ElemSize: cardinal): PAPointer; overload;
function AllignMem(Address: PointerInteger; ElemSize: cardinal): PointerInteger; overload;
function AllignMemOffset(Address: PAPointer; ElemSize: cardinal): integer; overload;
function AllignMemOffset(Address: PointerInteger; ElemSize: cardinal): integer; overload;



function StreamReadUInt64(const Src: TStream): UInt64; overload;
function StreamReadUInt64(const Src: TStream; var BytesRead: integer): UInt64; overload;

procedure StreamWriteUInt64(const Dst: TStream; Value: UInt64); overload;

function StreamReadInt16(const Src: TStream): SmallInt; overload;
function StreamReadUInt16(const Src: TStream): Word; overload;

function StreamReadInt8(const Src: TStream): ShortInt; overload;
function StreamReadUInt8(const Src: TStream): Byte; overload;

procedure StreamWriteInt16(const Dst: TStream; Value: SmallInt); overload;
procedure StreamWriteUInt16(const Dst: TStream; Value: Word); overload;

procedure StreamWriteInt8(const Dst: TStream; Value: ShortInt); overload;
procedure StreamWriteUInt8(const Dst: TStream; Value: Byte); overload;

procedure StreamWriteString(const Dst: TStream; const Value: string); overload;
function StreamReadString(const Src: TStream): string; overload;

procedure StreamWriteInt32(const Dst: TStream; Value: integer); overload;
procedure StreamWriteInt64(const Dst: TStream; Value: int64); overload;

function StreamReadUInt32(const Src: TStream; var BytesRead: integer): cardinal; overload;
function StreamReadUInt32(const Src: TStream): Cardinal; overload;

function StreamReadInt32(const Src: TStream; var BytesRead: integer): integer; overload;
function StreamReadInt32(const Src: TStream): integer; overload;

function StreamReadInt64(const Src: TStream; var BytesRead: integer): int64; overload;
function StreamReadInt64(const Src: TStream): int64; overload;

function StreamReadDouble(const Src: TStream): double; overload;
function StreamReadSingle(const Src: TStream): single; overload;
procedure StreamWriteSingle(const Dst: TStream; Value: single); overload;
procedure StreamWriteDouble(const Dst: TStream; Value: double); overload;





(*<summary>Starts the microsecond timer.</summary>
  
<remarks>Call this procedure to begin measuring time. To stop
  call <see cref="StopTimer"/>
</remarks>
*)
procedure StartTimer;overload;
(*<summary>Stops the timer with microsecond resolution.</summary>
  
<remarks>Call this procedure to stop measuring time. To begin
  call <see cref="StartTimer"/>. The result returned is
  time in seconds since the StartTimer routine was called.
</remarks>
*)
function StopTimer:double;overload;
(*<summary>Starts the timer with microsecond resolution.</summary>
  
<remarks>Call this procedure to begin measuring time. To stop
  call <see cref="StopTimer"/>. The timervar must be
  passed to the <see cref="StopTimer"/> routine.
</remarks>
*)
procedure StartTimer(var timervar:int64);overload;
(*<summary>Stops the microsecond timer.</summary>
  
<remarks>Call this procedure to stop measuring time. To begin
  call <see cref="StartTimer"/>. The timervar must be
  the same as passed to the <see cref="StartTimer"/> routine.
  The result returned is time in seconds since the StartTimer routine was called.
</remarks>
*)
function StopTimer(timervar:int64):double;overload;

(*<summary>Returns the description of the Src.</summary>*)
function IntPrecisionToStr(Src: TIntPrecision): string;overload;



(*<summary>FPU control word turning off all exceptions.</summary>
  *)
const MtxVec8087CW: Integer = $133f;
(*<summary>FPU control word used by Fortran.</summary>
  *)
const Fortran8087CW: Integer = $123f;

(*<summary>Storage default endianness.</summary>*)
const MtxSystemEndianness = boLittleEndian;








var
    
    MtxSaved8087CW: Word;
    

    (*<summary>Not a number.</summary>
     
<remarks><b>N</b>ot <b>a</b> <b>n</b>umber. Do not use this value in comparisons.
     To check whether a variable or expression evaluates to NaN, use the
     <see cref="IsNaN"/> function instead.
</remarks>


     *)
    NAN: double = 0/0;
    NAN_SINGLE: single = 0/0;
    

    (*<summary>Positive Infinity.</summary>
      
<remarks>Positive Infinity. Do not use this value in comparisons. To check whether a variable or expression represents
      infinity, use the <see cref="IsInf"/> routine instead.
</remarks>

       *)
    INF: double = 1/0;
    INF_SINGLE: single = 1/0;

    (*<summary>Complex version of NAN.</summary>
      
<remarks>Complex version of NAN.
</remarks>

       *)
    CNAN: TCplx = (Re: 0/0; Im: 0/0);
    CNAN_SINGLE: TSCplx = (Re: 0/0; Im: 0/0);

    (*<summary>Complex version of INF.</summary>
      
<remarks>Complex version of INF.
</remarks>

       *)
    CINF: TCplx = (Re: 1/0; Im: 1/0);
    CINF_SINGLE: TSCplx = (Re: 1/0; Im: 1/0);

    
    MtxRegistryKey: string = '\Software\Dew Research\MtxVec';
    MtxVecWorkingDir: string = 'C:\';
    MtxVecCopyright: string = '© 1999-2025 Dew Research';
    NilPtr: PAPointer =  nil ;
    

    
    

(*<summary>Defines the block size.</summary>
  
<remarks>When MtxVec makes use of block processing, these
  variable defines the number of elements in each block.
  The size of all blocks in bytes should approximatelly
  match the CPU cache size. The block should not be too small
  to avoid overhead when handling objects to become noticeable.
</remarks>


  *)

  var MtxVecBlockSize: integer = 1024;
  var MtxVecMemoryAlignment: Int64 = 128;
  var DebugValueCount: integer = 200;

(*<summary>Holds an array of 1000 prime numbers.</summary>
  *)

  var MtxVecPrimeArray: TIntegerArray;
  (*<summary>Gradient step size used by Richarson numeric gradient algorithm.</summary>
    *)
  var GradStepSize: double;
  var GradStepSizeSingle: single;
  var GradStepSizeDefault: double;
  var GradStepSizeSingleDefault: single;




function GetDecimalSeparator: char;
function GetThousandSeparator: char;


function IntInterlockedIncrement(var Param: integer): integer; 
function IntInterlockedDecrement(var Param: integer): integer; 
function IntInterlockedExchange(var Target: Integer; Value: Integer): Integer; 
function IntGetCurrentThreadID: TThreadID; 


    {$HPPEMIT END '#include "Math387.h"'}



procedure aSetLength(var a: TUInt64Array; const Len: NativeInt); overload;
procedure aSetLength(var a: TInt64Array; const Len: NativeInt); overload;
procedure aSetLength(var a: Math387.TShortIntArray;const Len: NativeInt); overload;
procedure aSetLength(var a: Math387.TSmallIntArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DSmallIntArray;const Len: NativeInt); overload;

procedure aSetLength(var a: Math387.TWordArray;const Len: NativeInt); overload;

procedure aSetLength(var a: Math387.TByteArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TBytes;            const Len: NativeInt); overload;

procedure aSetLength(var a: Math387.T2DByteArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TIntegerArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DIntegerArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TCplxArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DCplxArray;const Len: NativeInt); overload;
procedure aSetLength(var a: TObjectArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TSCplxArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DSCplxArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TSingleArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DSingleArray;const Len: NativeInt); overload;

procedure aSetLength(var a: TDoubleArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DDoubleArray;const Len: NativeInt); overload;

procedure aSetLength(var a: T2DCplxDoubleArray;const Len: NativeInt); overload;
procedure aSetLength(var a: T2DCplxSingleArray;const Len: NativeInt); overload;


procedure aSetLength(var a: TCardinalArray;const Len: NativeInt); overload;
procedure aSetLength(var a: string; const Len: integer); overload;

procedure aSetLength(var a: TThreadIDArray; const Len: NativeInt); overload;




procedure aSetLength(var a: WideString;const Len: integer); overload;




 

procedure aSetLength(var a: AnsiString; const Len: integer); overload;




procedure aSetLength(var a: TBooleanArray; const Len: NativeInt); overload;
procedure aSetLength(var a: StringArray; const Len: integer); overload;
procedure aSetLength(var a: T2DStringArray; const Len: integer); overload;



function aGetLength(const x: TObjectArray): integer; overload; 
function aGetLength(const x: Math387.TByteArray): integer; overload; 

function aGetLength(const x: TBytes): integer; overload; 


function aGetLength(const x: T2DDoubleArray): integer; overload; 
function aGetLength(const x: T2DSingleArray): integer; overload; 
function aGetLength(const x: T2DCplxArray): integer; overload; 
function aGetLength(const x: T2DSCplxArray): integer; overload; 
function aGetLength(const Str: string): integer; overload; 




function aGetLength(const Str: AnsiString): integer; overload; 




function aGetLength(const x: T2DStringArray): integer; overload; 
function aGetLength(const x: T2DIntegerArray): integer; overload; 
function aGetLength(const x: T2DSmallIntArray): integer; overload; 
function aGetLength(const x: T2DByteArray): integer; overload; 


function aGetLength(const x: array of cardinal): integer; overload;
function aGetLength(const x: array of integer): integer; overload;
function aGetLength(const x: array of double): integer; overload;
function aGetLength(const x: array of single): integer; overload;
function aGetLength(const x: array of SmallInt): integer; overload;
function aGetLength(const x: array of word): integer; overload;
function aGetLength(const x: array of string): integer; overload;
function aGetLength(const x: array of UInt64): integer; overload;
function aGetLength(const x: array of Int64): integer; overload;
function aGetLength(const x: array of TCplx): integer; overload;
function aGetLength(const x: array of TSCplx): integer; overload;


procedure SetInclude(var dstSet: UInt64; const Item: UInt64); overload;
procedure SetExclude(var dstSet: UInt64; const Item: UInt64); overload;
function SetContains(const Item: UInt64; const aSet: UInt64): boolean; overload;
procedure SetInclude(var dstSet: Cardinal; const Item: Cardinal); overload;
procedure SetExclude(var dstSet: Cardinal; const Item: Cardinal); overload;
function SetContains(const Item: Cardinal; const aSet: Cardinal): boolean; overload;




type
(*<summary> Mapped from Delphi System Unit </summary>*)
  TLocalCPUID = record
    EAX, EBX, ECX, EDX: UInt32;
  end;


(*<summary> Mapped from Delphi System Unit </summary>*)
function GetLocalCPUID(FunctionID: UInt32; SubFunctionID: UInt32 = 0): TLocalCPUID; overload;

(*<summary> Sets DAZ and FTZ bits to zero. </summary>
             
<remarks>DAZ bit is for "Denormals are zero"
             FTZ bit is for "Flush denormals to zero"
</remarks>
*)



