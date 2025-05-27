










{$I BdsppDefs.inc}


(*<summary> Utility routines for managing linear systems. </summary>
  
<remarks>Conversion routines between different representations of the transfer function,
  frequency transformations and bilinear transform.
</remarks>
*)
unit LinearSystems;


interface

uses MtxVec, Polynoms, Math387;



(*<summary> Convert transfer function from zero-pole to state-space form. </summary>

<remarks>Convert a transfer function defined with zeros Z and poles P and gain K in to its
   state space presentation of the real valued A matrix, B,C vectors and D value.
</remarks>

<SeeAlso cref="StateSpaceToZeroPole"/>
<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="TransferFunToStateSpace"/>*)
procedure ZeroPoleToStateSpace(const A: TMtx; const B, C: TVec; out D: Double; const z, p: TVec; const k: Double);

(*<summary>Convert transfer function from state-space to zero-pole form. </summary>

<remarks>Convert a transfer function defined in state space presentation of the real valued A matrix, B,C vectors and D value.
   in to its zero-pole form with zeros Z, poles P and gain K.
   State space system is related to its numerator/denominator representation via
  the following relation:
<code>

          num(s)      (s-sz1)*...*(s-szn)
  H(s) = -------- = K*------------------- =C*(s*I-A)^(-1)*B + D
          den(s)      (s-sp1)*...*(s-spn)

  dx/dt = Ax + Bu
  y     = Cx + Du

  x - system input
  y - system output
  szn - n'th zero
  spn - n'th pole
  K - system gain

</code>
</remarks>



<SeeAlso cref="ZeroPoleToStateSpace"/>
<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="TransferFunToStateSpace"/>
<SeeAlso cref="LTIZeros"/>*)

procedure StateSpaceToZeroPole(const z, p: TVec; out k: Double; const A: TMtx; const B,C: TVec; const D: Double);

(*<summary> Convert transfer function from state-space to numerator-denominator form. </summary>
   
<remarks>Convert a transfer function defined in state space presentation of the real valued A matrix, B,C vectors and D value
   in to its numerator Num and denominator Den form. State space
   system is related to its numerator/denominator representation via the following
   relation:
<code>

          num(s)
  H(s) = -------- = C*(s*I-A)^(-1)*B + D
          den(s)

  dx/dt = Ax + Bu
  y     = Cx + Du

  x - system input
  y - system output

</code>
</remarks>


<SeeAlso cref="ZeroPoleToStateSpace"/>
<SeeAlso cref="TransferFunToStateSpace"/>
<SeeAlso cref="StateSpaceToZeroPole"/>*)
procedure StateSpaceToTransferFun(const num, den: TVec; const A: TMtx; const B, C: TVec; const D: Double);
(*<summary> Convert transfer function from numerator-denominator to state-space form. </summary>
  
<remarks>Convert transfer function in form of a numerator Num and denominator Num in to its
  state space presentation of the real valued A matrix, B,C vectors and D value.
  State space system is related to its numerator/denominator representation via the following
  relation:
<code>

           num(s)
   H(s) = -------- = C*(s*I-A)^(-1)*B + D
           den(s)

   dx/dt = Ax + Bu
   y     = Cx + Du

   x - system input
   y - system output

</code>
</remarks>


<SeeAlso cref="ZeroPoleToStateSpace"/>
<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="StateSpaceToZeroPole"/>*)
procedure TransferFunToStateSpace(const A: TMtx; const B, C: TVec; out D: Double; const num,den: TVec);
(*<summary> Convert transfer function from zero-pole to numerator-denominator form. </summary>
  
<remarks>Convert a rational polynomial defined with zeros Z and poles P and gain K in to its
  numerator Num and denominator Den form. The numerator will be scaled
  by K and both polynomials are assumed to have only real coefficents.
  (Poles and zeros can still be complex, if they have complex conjugated pairs.)
<code>

          num(s)      (s-sz1)*...*(s-szn)
  H(s) = -------- = K*-------------------
          den(s)      (s-sp1)*...*(s-spn)

  szn - n'th zero
  spn - n'th pole
  K - system gain
</code>
</remarks>




  <example>
  The following example computes the coefficients of the rational
          polynomial. The numerator has zeros at 3 and 4 and the
          denominator has zeros at 1 and 2.
          The polynomial in zero pole form can be written as:
<code>

 (x - 3)*(x - 4)
 ---------------
 (y - 2)*(y - 1)

</code>
And in transfer function form:
<code>

  x^2 - 7*x + 12
  --------------
  x^2 - 3*x + 2

</code>
Notice that powers are falling from left to right.


  <code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
     Vector z = new Vector(0);
     Vector p = new Vector(0);
     Vector num = new Vector(0);
     Vector den = new Vector(0);
     double k;

     z.SetIt(false,new double[2] {3,4});
     p.SetIt(false,new double[2] {1,2});
     k = 1;
     LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
     MtxVecEdit.ViewValues(num,"Numerator",true);
     MtxVecEdit.ViewValues(den,"Denominator",true);
//        num = [1, -7,  12]
//        den = [1, -3,   2]
  }

</code></example>

<SeeAlso cref="ZeroPoleToStateSpace"/>
<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="StateSpaceToZeroPole"/>
<SeeAlso cref="TransferFunToZeroPole"/>*)

procedure ZeroPoleToTransferFun(const num,den: TVec; const z,p: TVec; k: Double);
(*<summary> Convert transfer function from zero-pole to second order sections form. </summary>
  
<remarks>Convert a rational polynomial defined with zeros Z and poles P and gain K in to a product
  of second order sections. The numerator in the last section will be scaled
  by K and all polynomials are assumed to have only real coefficents.
  (Poles and zeros can still be complex, if they have complex conjugated pairs.)

  Second order sectios are stored in destination vector in the order: Num1, Den1, Num2, Den2... DenN.
  Each section consists of 3 items (a,b,c) from the formula : a*x^2 + b*x + c
<code>

          num_sos1(s)              num_sosN(s)       (s-sz1)*...*(s-szn)
  H(s) = ------------- *  ...  * ------------- =   K*-------------------
          den_sos1(s)              den_sosN(s)       (s-sp1)*...*(s-spn)

  szn - n'th zero
  spn - n'th pole
  K - system gain
</code>
</remarks>
*)
procedure ZeroPoleToSOS(const sos: TVec; const z,p: TVec; k: Double);

(*<summary>Convert transfer function from numerator-denominator to zero-pole form. </summary>
  
<remarks>Convert a transfer function defined with numerator Num and denominator Den
  in to its zero-pole form with zeros Z, poles P and gain K.
  The routine calls PolyRoots routine from the Polynoms unit.
  Numerator and denominator can be real or complex.

A rational polynomial can be converted to zero pole form by
finding the roots of the numerator and denominator:
<code>

  x^2 - 7*x + 12
  --------------
  x^2 - 3*x + 2

</code>
Zero pole form:
<code>

 (x - 3)*(x - 4)
 ---------------
 (y - 2)*(y - 1)

 </code>
</remarks>




<example>
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
     Vector z = new Vector(0);
     Vector p = new Vector(0);
     Vector num = new Vector(3);
     Vector den = new Vector(3);
     double k;

     num.Values[0] = 1;
     num.Values[1] = -7;
     num.Values[2] = 12;
     den.Values[0] = 1;
     den.Values[1] = -3;
     den.Values[2] = 2;
     k = 1;
     LinearSystems.TransferFunToZeroPole(z, p, out k, num, den);
     MtxVecEdit.ViewValues(z,"Zeros",true);
     MtxVecEdit.ViewValues(p,"Poles",true);
     //        z = [4 , 3]
     //        p = [2 , 1]
     //        k = num[0]/den[0] =  1;
  }

</code></example>

<SeeAlso cref="ZeroPoleToStateSpace"/>
<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="StateSpaceToZeroPole"/>
<SeeAlso cref="ZeroPoleToTransferFun"/>*)

procedure TransferFunToZeroPole(const z, p: TVec; out k: Double; const num,den: TVec);

(*<summary> Find zeros of a linear time invariant system in state-space form. </summary>
  
<remarks>Compute zeros of a linear time invariant system represented in state
  space form. The resulting zeros are placed in Z and computed system
  gain in Gain.
</remarks>




<example>
If a linear time invariant system is represented by A,B,C,D,
a zero pole representation can be obtained like this:
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
     Vector z = new Vector(0);
     Vector p = new Vector(0);
     Vector b = new Vector(3);
     Vector c = new Vector(3);
     Matrix a = new Matrix(3, 3);
     double k,d;

     a.SetIt(3, 3, false, new double[9] {0,1,2,
                                         2,3,4,
                                         5,6,3});
     b.Values[0] = 1;
     b.Values[1] = 0.5;
     b.Values[2] = 2;
     c.Values[0] = 1;
     c.Values[1] = 2.5;
     c.Values[2] = 2;
     d = 1;
     //        ....   get a state space version and store in A,B,C,D
     a.Eig(p,TMtxType.mtGeneral); //compute poles
     LinearSystems.LTIZeros(z, out k, a, b, c, d); //get zeros and gain
     MtxVecEdit.ViewValues(z,"Zeros",true);
     MtxVecEdit.ViewValues(p,"Poles",true);
  }

  </code></example>

<SeeAlso cref="StateSpaceToTransferFun"/>
<SeeAlso cref="StateSpaceToZeroPole"/>*)

procedure LTIZeros(const z: TVec; out gain: Double; const A: TMtx; const B, C:TVec; const D: Double);

(*<summary> Compute bilinear transformation. </summary>
  
<remarks>Apply bilinear transform to transfer function represented in zero-pole form.
  FS defines the sampling frequency. If z.Length &lt; p.Length the routine
  will not add zeroes at -1 to match the number of poles unless AddZeros
  is True.

  Bilinear transformation is defined with the mapping:
<code>
       2   (1 - z^(-1))
  s -> - * ------------
       T   (1 - z^(-1))
</code>
Bilinear transform requires that the amplitude response of a continuous
system is piecewise constant and can not be used
to transform an analog differentiator to a digital filter (for example).
Bilinear transform also does not preserve the impulse or
the phase response [1].

<b>References: </b>  <para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>
*)

procedure Bilinear(const z, p: TVec; var k: Double; FS: Double = 2; AddZeros: boolean = True); overload;

(*<summary>Apply bilinear transform to a linear system represented in state-space form. </summary>



<example>
         An analog lowpass filter is converted to
         z-domain by using the bilinear transform. The analog
         filter has a normalized cutoff frequency at 1 rad/sec.
         This frequency is mapped by the bilinear
         transformation to the value as returned by the
         function: BilinearUnwarp(1). To obtain the
         required cutoff frequency of an analog lowpass
         filter, which will map to a selected frequency
         in z-domain use the BilinearPrewarp routine.
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
      Vector z = new Vector(0);
      Vector p = new Vector(0);
      Vector num = new Vector(0);
      Vector den = new Vector(0);
      Vector Response = new Vector(0);
      Vector b = new Vector(0);
      Vector c = new Vector(0);
      double k,Wc,d;
      Matrix A = new Matrix(0,0);
      double FS = 2;

      int Order = 5; //design a fifth order filter.

      IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
      //passband ripple 0.2dB, stopband attenuation 40dB
      LinearSystems.Bilinear(z,p,ref k,FS,true);  //Sampling frequency  = 2
      Wc = 0.6; //request a cutoff at 0.6 Hz
      LinearSystems.LowpassToLowpassZ(z,p,ref k,Wc, LinearSystems.BilinearUnwarp(1,FS));  //frequency transformation in z-domain
      LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);

      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64
      MtxVecTee.DrawIt(Response, "Design method 0", false);

//Alternative 1:
      IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
      LinearSystems.Bilinear(z,p,ref k,FS,true);  //Sampling frequency  = 2
      LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
      Wc = 0.6; //request a cutoff at 0.6 Hz
      LinearSystems.LowpassToLowpassZ(num,den,Wc,LinearSystems.BilinearUnwarp(1,FS));  //frequency transformation in z-domain

      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64
      MtxVecTee.DrawIt(Response, "Design method 1", false);

//Alternative 2:
      IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
      Wc = LinearSystems.BilinearPrewarp(0.6,FS); //request a cutoff at 0.6 Hz
      LinearSystems.LowpassToLowpass(z,p,ref k,Wc);  //frequency transformation in s-domain
      LinearSystems.Bilinear(z,p,ref k, FS,true);  //Sampling frequency  = 2
      LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);

      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64
      MtxVecTee.DrawIt(Response, "Design method 2", false);

//Alternative 3:
      IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
      LinearSystems.ZeroPoleToStateSpace(A,b,c,out d,z,p,k);
      Wc = LinearSystems.BilinearPrewarp(0.6,FS); //request a cutoff at 0.6 Hz
      LinearSystems.LowpassToLowpass(A,b,c,ref d,Wc); //frequency transformation in s-domain
      LinearSystems.Bilinear(A,b,c,ref d,2);
      LinearSystems.StateSpaceToZeroPole(z,p,out k,A,b,c,d);
      LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);

      SignalUtils.FrequencyResponse(num,den,Response,64,false,TSignalWindowType.wtRectangular,0); //zero padding set to 64
      MtxVecTee.DrawIt(Response,"Design method 3",false);
  }
</code></example>

<SeeAlso cref="MatchedZTransform"/>
<SeeAlso cref="LowpassToLowpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="BilinearUnwarp"/>
<SeeAlso cref="BilinearPrewarp"/>*)

procedure Bilinear(const A: TMtx;const B,C: TVec; var D: Double; FS: Double = 2); overload;

(*<summary> Frequency transformation from a lowpass to a bandstop filter in s-domain. </summary>
  
<remarks>Transform a lowpass filter prototype in zero-pole form to a bandstop filter, where the stopband has
  width BW centered around the frequency CenterFreq. Assumed sampling frequency is 2.
  The transformation is defined as ([1], p. 258):  
<code>
          s(Wu - Wl)
  s --> -------------
         s^2 + Wu*Wl

  Wl - lower cutoff frequency
  Wu - upper cutoff frequency
</code>
The routine also adds pairs of zeros at +/-j*CenterFreq,
if the number of zeros is less then number of poles, to match the number of poles.

<b>References: </b> <para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>
*)

procedure LowpassToBandstop(const z, p: TVec; var k: Double; CenterFreq,Bw: Double); overload;
(*<summary> Convert a lowpass filter prototype in state space form to a bandstop filter. </summary>

 

<example>
Design an analog bandstop filter, where the stopband
is defined with Wl = 0.2 rad/sec,  Wu = 0.6 rad/sec
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
         Vector z = new Vector(0);
         Vector p = new Vector(0);
         Vector num = new Vector(0);
         Vector den = new Vector(0);
         Vector Response = new Vector(0);
         Vector FreqFr = new Vector(0);
         double k,Wc,BW;
         double FS = 2;
         int Order = 5; //design a fifth order filter.

         IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
         Wc = Math.Sqrt(0.2*0.6);
         BW = 0.6 - 0.2;
         LinearSystems.LowpassToBandstop(z,p,ref k,Wc,BW);  //frequency transformation in s-domain
         LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
      //Define the frequency grid (logarithmic)
         FreqFr.Length = 1000;
         SignalUtils.LogRamp(FreqFr,-1,1); //between 0.1 (=10^(-1)) and 10 (=10^1) rad/sec
         SignalUtils.FrequencyResponseS(num,den,FreqFr,Response,0); //Laplace
         MtxVecTee.DrawIt(Response,"Frequency response",false); //Y axis linear, X axis logarithmic
    }

</code></example>

<SeeAlso cref="LowpassToLowpass"/>
<SeeAlso cref="LowpassToBandpass"/>
<SeeAlso cref="LowpassToHighpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>*)

procedure LowpassToBandstop(const a: TMtx; const b,c: TVec; var D: Double; CenterFreq,Bw: Double); overload;

(*<summary> Frequency transformation from a lowpass to a bandpass filter in s-domain. </summary>
  
<remarks>Transform a lowpass filter prototype in zero-pole form to bandpass filter, where the passband of width BW
  is centered around the frequency CenterFreq. Assumed sampling frequency is 2.
  The transformation is defined as ([1], p. 258):
<code>
          s^2 + Wu*Wl
  s --> -------------
          s(Wu - Wl)

  Wl - lower cutoff frequency
  Wu - upper cutoff frequency
</code>  
The routine also adds zeros at 0. It adds one zero, if the
lowpass filter order is odd and already has zeros. If the filter does
not have zeros, it adds sufficient zeros at 0 to match the order
of the lowpass filter.

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>
*)

procedure LowpassToBandpass(const z, p: TVec; var k: Double; CenterFreq, Bw: Double); overload;
(*<summary> Convert a lowpass filter prototype in state space form to a bandstop filter. </summary>



<example>
Design an analog bandpass filter, where the passband is
defined with Wl = 0.2 rad/sec,  Wu = 0.6 rad/sec
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
       Vector z = new Vector(0);
       Vector p = new Vector(0);
       Vector num = new Vector(0);
       Vector den = new Vector(0);
       Vector Response = new Vector(0);
       Vector FreqFr = new Vector(0);
       double k,Wc,BW;
       double FS = 2;
       int Order = 5; //design a fifth order filter.

       IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
       Wc = Math.Sqrt(0.2*0.6);
       BW = 0.6 - 0.2;
       LinearSystems.LowpassToBandpass(z,p,ref k,Wc,BW);  //frequency transformation in s-domain
       LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
    //Define the frequency grid (logarithmic)
       FreqFr.Length = 1000;
       SignalUtils.LogRamp(FreqFr,-1,1); //between 0.1 (=10^(-1)) and 10 (=10^1) rad/sec
       SignalUtils.FrequencyResponseS(num,den,FreqFr,Response,0); //Laplace
       MtxVecTee.DrawIt(Response,"Frequency response",false); //Y axis linear, X axis logarithmic
  }

</code></example>

<SeeAlso cref="LowpassToLowpass"/>
<SeeAlso cref="LowpassToBandstop"/>
<SeeAlso cref="LowpassToHighpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>*)
procedure LowpassToBandpass(const a: TMtx; const b,c: TVec; var D: Double; CenterFreq, BW: Double); overload;

(*<summary> Frequency transformation from a lowpass to a highpass filter in s-domain. </summary>
  
<remarks>Transform a lowpass filter prototype in zero-pole form to a highpass filter,
  where the new cutoff frequency is Freq. Assumed sampling frequency is 2.
  The transformation is defined as ([1], p. 258):
<code>
          Wu
  s --> -----
          s

  Wu - new cutoff frequency
</code>
The routine also adds zeros at 0. It adds one zero, if the
lowpass filter order is odd and already has zeros. If the filter does
not have zeros, it adds sufficient zeros at 0 to match the order
of the filter.

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>
*)

procedure LowpassToHighpass(const z, p: TVec; var k: Double; Freq: Double); overload;
(*<summary> Transform a lowpass filter prototype in state space form to highpass filter. </summary>



<example>
Design an analog highass filter, where the passband is defined with Wu = 0.2 rad/sec.

<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
       Vector z = new Vector(0);
       Vector p = new Vector(0);
       Vector num = new Vector(0);
       Vector den = new Vector(0);
       Vector Response = new Vector(0);
       Vector FreqFr = new Vector(0);
       double k,Wc;
       double FS = 2;
       int Order = 5; //design a fifth order filter.

       IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
       Wc = 0.2;
       LinearSystems.LowpassToHighpass(z,p,ref k,Wc);  //frequency transformation in s-domain
       LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
    //Define the frequency grid (logarithmic)
       FreqFr.Length = 1000;
       SignalUtils.LogRamp(FreqFr,-1,1); //between 0.1 (=10^(-1)) and 10 (=10^1) rad/sec
       SignalUtils.FrequencyResponseS(num,den,FreqFr,Response,0); //Laplace
       MtxVecTee.DrawIt(Response,"Frequency response",false); //Y axis linear, X axis logarithmic
  }

</code></example>

<SeeAlso cref="LowpassToLowpass"/>
<SeeAlso cref="LowpassToBandstop"/>
<SeeAlso cref="LowpassToBandpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>*)

procedure LowpassToHighpass(const A: TMtx; const B, C: TVec; var D: Double; Freq: Double); overload;

(*<summary> Frequency transformation from a lowpass to a lowpass filter in s-domain. </summary>
  
<remarks>Transform a lowpass filter prototype in zero-pole form to a lowpass filter, where the
  new cutoff frequency is Freq. Assumed sampling frequency is 2.
  The transformation is defined as ([1], p. 258):
<code>
           s
  s --> -----
          Wu

  Wu - new cutoff frequency
</code>
<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>
*)

procedure LowpassToLowpass(const z, p: TVec; var k: Double; Freq: Double); overload;
(*<summary> Transform a lowpass filter prototype in state space form to a lowpass filter. </summary>



<example>
Design an analog lowpass filter, where the passband is defined with Wu = 0.7 rad/sec.

<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
       Vector z = new Vector(0);
       Vector p = new Vector(0);
       Vector num = new Vector(0);
       Vector den = new Vector(0);
       Vector Response = new Vector(0);
       Vector FreqFr = new Vector(0);
       double k,Wc,BW;
       double FS = 2;
       int Order = 5; //design a fifth order filter.

       IIRFilters.EllipticAnalog(Order,0.2,40,z,p,out k);  //design analog protype
       Wc = 0.7;
       LinearSystems.LowpassToLowpass(z,p,ref k,Wc);  //frequency transformation in s-domain
       LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
    //Define the frequency grid (logarithmic)
       FreqFr.Length = 1000;
       SignalUtils.LogRamp(FreqFr,-1,1); //between 0.1 (=10^(-1)) and 10 (=10^1) rad/sec
       SignalUtils.FrequencyResponseS(num,den,FreqFr,Response,0); //Laplace
       MtxVecTee.DrawIt(Response,"Frequency response",false); //Y axis linear, X axis logarithmic
  }

</code></example>


<SeeAlso cref="LowpassToBandstop"/>
<SeeAlso cref="LowpassToBandpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>*)
procedure LowpassToLowpass(const a: TMtx; const b,c: TVec; var D: Double; Freq: Double); overload;

(*<summary> Transform the zeros and poles of a filter in s-domain to z-domain. </summary>
  
<remarks>Transform the zeros Z and poles P of a filter from s-domain
  to z-domain, where FS is the sampling frequency.
  The transformation is defined as ([1], p. 224):
<code>

  s + a --> 1 - z^(-1)*e^(-a/FS)

  FS - sampling frequency
  a - pole or zero

</code>
  If the analog system has zeros with center
  frequencies greater then half the sampling frequency,
  their z-plane positions will be greatly aliased [1].
  The transformation has the advantage of not
  affecting the phase response of the original transfer
  function.

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>




<example>
A bessel analog lowpass filter is converted to
         z-domain by using the matched Z transform. The analog
         filter has a normalized cutoff frequency at 1 rad/sec.
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
     Vector z = new Vector(0);
     Vector p = new Vector(0);
     Vector num = new Vector(0);
     Vector den = new Vector(0);
     Vector Response = new Vector(0);
     Vector FreqFr = new Vector(0);
     double k,Wc,BW;
     double FS = 2;
     int Order = 5; //design a fifth order filter.

     IIRFilters.BesselAnalog(Order,z,p,out k);  //design analog protype
     Wc = 0.5;
     LinearSystems.LowpassToLowpass(z,p,ref k,Wc);  //frequency transformation in s-domain
     LinearSystems.MatchedZTransform(z, p, FS);
     k = k/LinearSystems.ComputeGain(z,p,1);
     z.Size(p.Length,false);
     z.SetVal(-1); //add missing zeros at -1
     LinearSystems.ZeroPoleToTransferFun(num,den,z,p,k);
     SignalUtils.FrequencyResponse(num,den,Response,64,false,TSignalWindowType.wtRectangular,0); //zero padding set to 64
     MtxVecTee.DrawIt(20*MtxExpr.Log10(MtxExpr.Abs(Response)),"Magnitude",false);
     MtxVecTee.DrawIt(MtxExpr.PhaseSpectrum(Response)*(180/Math.PI),"Phase",false);
  }

</code></example>

<SeeAlso cref="Bilinear"/>*)

procedure MatchedZTransform(const z,p: TVec; FS: Double = 1); overload;
(*<summary>Returns gain of a filter with zeroes in z and poles in p. </summary>
  
<remarks>Returns gain of a filter with zeroes in z and poles in p.
  Val parameter defines the value at which to evaluate
  the rational polynomial:
<code>

  (x-z1)*....*(x-zn)      (Val-z1)*....*(Val-zn)
  ------------------ ==>  ---------------------- = number
  (x-p1)*....*(x-pn)      (Val-p1)*....*(Val-pn)

  x - variable
  z1..zn - zeros
  p1..pn - poles

</code>
The routine assumes that the coefficients of the polynomial
are all real and therefore returns only the real part of the result.
</remarks>
*)
function ComputeGain(const z, p: TVec; Val: Double = 1): Double; overload;
(*<summary> Returns gain of a filter with zeroes in z and poles in p. </summary>
  
<remarks>Val parameter defines the value at which to evaluate the rational polynomial.
</remarks>
*)
function ComputeGain(const z, p: TVec; Val: TCplx): Double; overload;

(*<summary> Replace the variable of a rational polynomial with another rational polyniomial. </summary>
  
<remarks>Num is the nominator and den is the denominator of the original polynomial.
  Nz is the nominator and Dz is the denominator of the polynom to
  substitude the variable with in the original polynomial. The function returns
  modified num and den. Num and den must have the same length.
  Nz and Dz must have the same length. The advantage of this method
  is that rooting of the polynomials is not required.
  The resulting polynomial is normalized to have the coefficient
  for the highest power in the polynomial equal to 1.
</remarks>
*)
procedure RationalSubstitution(const num,den: TVec; const Nz,Dz: TVec); overload;
(*<summary> Zeroes of the original polynomial are stored in z and poles in p vector. </summary>
  
<remarks>Nz is the nominator and Dz is the denominator of the polynom to
  substitude the variable with in the original polynomial. The function returns
  modified z and p. Nz and Dz must have the same length. k is the gain factor
  (not modified).
</remarks>




<example>
Substitute the variable:
<code>

    z^2 - 2*z + 1                z - 1
  ------------------   , z -- &gt;  -----
    z^2- 4*z  + 1                z - 2

   The resulting polynomial:

         -0.5
  ------------------
   z^2 - 3*z  + 1.5

</code>
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {
     Vector num = new Vector(0);
     Vector den = new Vector(0);
     Vector den1 = new Vector(0);
     Vector num1 = new Vector(0);

     num.SetIt(false,new double[3] {1,-2, 1});
     den.SetIt(false,new double[3] {1,-4, 1});

     num1.SetIt(false,new double[2] {1,-1});
     den1.SetIt(false,new double[2] {1,-2});

     LinearSystems.RationalSubstitution(num,den,num1,den1);
     MtxVecEdit.ViewValues(num,"Num",true);
     MtxVecEdit.ViewValues(den,"Den",true);

  //          num = [ 0, 0, -0.5]
  //          den = [ 1,-3,  1.5]
  }

</code></example>*)

procedure RationalSubstitution(const z,p: TVec; var k: Double; const Nz,Dz: TVec); overload;

(*<summary> Apply frequency band transformation from lowpass to lowpass in the z-domain. </summary>
  
<remarks>Freq is the cutoff frequency of the new filter.
  The function returns modified num and den.
  PrototypeFreq is the cutoff frequency of the prototype lowpass filter
  after it has been mapped to z-domain.
  Freq and PrototypeFreq must be between 0 and 1 (Sampling frequency = 2).
  The transformation is defined with the following mapping ([1] p. 260 and
  [2] p. 434, [3] p. 352):
<code>
                 z^(-1) - a
  z^(-1) --->  --------------
                1 - a*z^(-1)

        sin((wc - wn)/2)
  a = -------------------
        sin((wc + wn)/2)

  wc - old cutoff frequency
  wn - new (desired) cutoff frequency
</code>

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975 <para/>
[2] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989 <para/>
[3] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>
*)
procedure LowpassToLowpassZ(const num,den: TVec; Freq: Double; PrototypeFreq: Double); overload;
(*<summary> The function returns modified z (zeros),  p (poles) and k (gain). </summary>


<example>
Elliptic lowpass filter design.
        The cutoff frequency of an analog filter prototype transformed in to
        z domain is obtained with BilinearUnwarp method.
        The analog prototype filter has a normalized cutoff frequency
        at 1 rad/sec.
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {

      Vector z = new Vector(0);
      Vector p = new Vector(0);
      Vector num = new Vector(0);
      Vector den = new Vector(0);
      Vector Response = new Vector(0);
      double k, Wc;
      double FS = 2;
      int Order = 4; //design a fifth order filter.

      IIRFilters.EllipticAnalog(Order,0.1,30, z, p, out k);  //design analog protype
      LinearSystems.Bilinear(z, p, ref k, FS,true);
      Wc = 0.5;
      LinearSystems.LowpassToLowpassZ(z, p, ref k, Wc, LinearSystems.BilinearUnwarp(1,FS));
      LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64

//Alternative:
//            ...
//            LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
//            LinearSystems.LowpassToLowpassZ(num,den, Wc, LinearSystems.BilinearUnwarp(1,FS));

      MtxVecTee.DrawIt(Response, "Frequency response", false);
      //MtxVecTee.DrawIt(20 * MtxExpr.Log10(MtxExpr.Abs(Response)), "Magnitude", false);
      //MtxVecTee.DrawIt(MtxExpr.PhaseSpectrum(Response) * (180 / Math.PI), "Phase", false);
  }

</code></example>

<SeeAlso cref="Bilinear"/>
<SeeAlso cref="RationalSubstitution"/>*)
procedure LowpassToLowpassZ(const z,p: TVec;var k: Double; Freq,PrototypeFreq: Double); overload;

(*<summary> Apply frequency band transformation from lowpass to highpass in the z-domain. </summary>
  
<remarks>Freq is the cutoff frequency of the new filter.
  The function returns modified num and den.
  PrototypeFreq is the cutoff frequency of the prototype lowpass filter
  after it has been mapped to z-domain.
  Freq and PrototypeFreq must be between 0 and 1 (Sampling frequency = 2).
  The transformation is defined with the following mapping ([1] p. 260 and
  [2] p. 434, [3] p. 352):
<code>
                   z^(-1) + a
  z^(-1) --->  - -------------
                  1 + a*z^(-1)

        cos((wc - wn)/2)
  a = -------------------
        cos((wc + wn)/2)

  wc - old cutoff frequency
  wn - new (desired) cutoff frequency
</code>

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975 <para/>
[2] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989 <para/>
[3] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>
*)

procedure LowpassToHighpassZ(const num,den: TVec; Freq: Double; PrototypeFreq: Double); overload;
(*<summary> The function returns modified z (zeros),  p (poles) and k (gain). </summary>


<example>
Elliptic highpass filter design.
         The cutoff frequency of a lowpass analog filter prototype transformed in to
         z domain is obtained with BilinearUnwarp method.
         The analog prototype filter has a normalized cutoff frequency
         at 1 rad/sec.
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {

    Vector z = new Vector(0);
    Vector p = new Vector(0);
    Vector num = new Vector(0);
    Vector den = new Vector(0);
    Vector Response = new Vector(0);
    double k, Wc;
    double FS = 2;
    int Order = 4; //design a fifth order filter.

    IIRFilters.EllipticAnalog(Order,0.1,30, z, p, out k);  //design analog protype
    LinearSystems.Bilinear(z, p, ref k, FS,true);
    Wc = 0.5;
    LinearSystems.LowpassToHighpassZ(z, p, ref k, Wc, LinearSystems.BilinearUnwarp(1,FS));
    LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
    SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64

//Alternative:
//            ...
//            LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
//            LinearSystems.LowpassToHighpassZ(num,den, Wc, LinearSystems.BilinearUnwarp(1,FS));

    MtxVecTee.DrawIt(Response, "Frequency response", false);
    //MtxVecTee.DrawIt(20 * MtxExpr.Log10(MtxExpr.Abs(Response)), "Magnitude", false);
    //MtxVecTee.DrawIt(MtxExpr.PhaseSpectrum(Response) * (180 / Math.PI), "Phase", false);
  }


  </code></example>

<SeeAlso cref="Bilinear"/>
<SeeAlso cref="RationalSubstitution"/>
<SeeAlso cref="LowpassToHighpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>*)
procedure LowpassToHighpassZ(const z, p: TVec;var k: Double; Freq: Double; PrototypeFreq: Double); overload;
(*<summary> Apply frequency band transformation from lowpass to bandpass in the z-domain. </summary>
  
<remarks>Freq is the center frequency of the passband with width BW of the new filter.
  The function returns modified num and den.
  PrototypeFreq is the cutoff frequency of the prototype lowpass filter
  after it has been mapped to z-domain.
  Freq, BW and PrototypeFreq must be between 0 and 1 (Sampling frequency = 2).
  The transformation is defined with the following mapping ([1] p. 260 and
  [2] p. 434, [3] p. 352):
<code>
                -a2 + a1*z^(-1)  -   z^(-2)
  z^(-1) --->  ---------------------------
                1  - a1*z^(-1) + a2*z^(-2)


           cos((w2 + w1)/2)
  Beta = -------------------
           cos((w2 - w1)/2)

  k = cot((w2-w1)/2)*tan(wc/2)

  a1 = 2*beta*k1/(k1+1)

  a2 = (k1-1)/(k1+1)

  wc - old cutoff frequency
  w2 - desired upper cutoff frequency
  w1 - desired lower cutoff frequency
</code>

<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975 <para/>
[2] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989 <para/>
[3] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>
*)

procedure LowpassToBandpassZ(const num,den: TVec; Freq, BW: Double; PrototypeFreq: Double); overload;
(*<summary> The function returns modified z (zeros),  p (poles) and k (gain). </summary>


<example>
Elliptic bandpass filter design.
        The cutoff frequency of a lowpass analog filter prototype transformed in to
        z domain is obtained with BilinearUnwarp method.
        The analog prototype filter has a normalized cutoff frequency
        at 1 rad/sec.
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {

      Vector z = new Vector(0);
      Vector p = new Vector(0);
      Vector num = new Vector(0);
      Vector den = new Vector(0);
      Vector Response = new Vector(0);
      double k, Wc;
      double FS = 2;
      int Order = 4; //design a fourth order filter.

      IIRFilters.EllipticAnalog(Order,0.1,30, z, p, out k);  //design analog protype
      LinearSystems.Bilinear(z, p, ref k, FS,true);
      double w1 = 0.2; //start of the passband at 0.2Hz.
      double w2 = 0.5; //stop of the passband at 0.5Hz.
      Wc = Math.Sqrt(w1*w2); //center frequency of the passband
      double BW = w2-w1;  //passband width
      LinearSystems.LowpassToBandpassZ(z, p, ref k, Wc, BW, LinearSystems.BilinearUnwarp(1,FS));
      LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64

  //Alternative:
  //            ...
  //            LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
  //            LinearSystems.LowpassToBandpassZ(num,den, Wc, BW, LinearSystems.BilinearUnwarp(1,FS));

      MtxVecTee.DrawIt(Response, "Frequency response", false);
      //MtxVecTee.DrawIt(20 * MtxExpr.Log10(MtxExpr.Abs(Response)), "Magnitude", false);
      //MtxVecTee.DrawIt(MtxExpr.PhaseSpectrum(Response) * (180 / Math.PI), "Phase", false);
  }

</code></example>

<SeeAlso cref="Bilinear"/>
<SeeAlso cref="RationalSubstitution"/>
<SeeAlso cref="LowpassToBandpass"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>
<SeeAlso cref="LowpassToBandstopZ"/>*)
procedure LowpassToBandpassZ(const z, p: TVec;var k: Double;Freq, BW: Double; PrototypeFreq: Double); overload;
(*<summary> Apply frequency band transformation from lowpass to bandstop in the z-domain. </summary>
  
<remarks>Freq is the center frequency of the stopband with width BW of the new filter.
  The function returns modified num and den.
  PrototypeFreq is the cutoff frequency of the prototype lowpass filter
  after it has been mapped to z-domain.
  Freq, BW and PrototypeFreq must be between 0 and 1 (Sampling frequency = 2).
  The transformation is defined with the following mapping ([1] p. 260 and
  [2] p. 434, [3] p. 352):
<code>
                a2  - a1*z^(-1)  +   z^(-2)
  z^(-1) --->  ---------------------------
                1  - a1*z^(-1) + a2*z^(-2)

           cos((w2 + w1)/2)
  Beta = -------------------
           cos((w2 - w1)/2)

  k = tan((w2-w1)/2)*tan(wc/2)

  a1 = 2*beta/(k1+1)

  a2 = (1-k1)/(k1+1)

  wc - old cutoff frequency
  w2 - desired upper cutoff frequency
  w1 - desired lower cutoff frequency
</code>
<b>References: </b><para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975 <para/>
[2] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989 <para/>
[3] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>
*)
procedure LowpassToBandstopZ(const num, den: TVec; Freq, BW: Double; PrototypeFreq: Double); overload;
(*<summary> The function returns modified z (zeros),  p (poles) and k (gain). </summary>


<example>
Elliptic bandstop filter design.
        The cutoff frequency of a lowpass analog filter prototype transformed in to
        z domain is obtained with BilinearUnwarp method.
        The analog prototype filter has a normalized cutoff frequency
        at 1 rad/sec. 
<code>

  using Dew.Math;
  using Dew.Math.Editors;
  using Dew.Math.Units;
  using Dew.Signal;
  using Dew.Signal.Units;
  using Dew.Math.Tee;
  using Dew.Signal.Tee;

  private void button1_Click(object sender, EventArgs e)
  {

      Vector z = new Vector(0);
      Vector p = new Vector(0);
      Vector num = new Vector(0);
      Vector den = new Vector(0);
      Vector Response = new Vector(0);
      double k, Wc;
      double FS = 2;
      int Order = 4; //design a fourth order filter.

      IIRFilters.EllipticAnalog(Order,0.1,30, z, p, out k);  //design analog protype
      LinearSystems.Bilinear(z, p, ref k, FS,true);
      double w1 = 0.2; //start of the stopband at 0.2Hz.
      double w2 = 0.5; //stop of the stopband at 0.5Hz.
      Wc = Math.Sqrt(w1*w2); //center frequency of the stopband
      double BW = w2-w1;  //passband width
      LinearSystems.LowpassToBandPStopZ(z, p, ref k, Wc, BW, LinearSystems.BilinearUnwarp(1,FS));
      LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
      SignalUtils.FrequencyResponse(num, den, Response, 64, false, TSignalWindowType.wtRectangular, 0); //zero padding set to 64

  //Alternative:
  //            ...
  //            LinearSystems.ZeroPoleToTransferFun(num,den, z, p, k);
  //            LinearSystems.LowpassToBandstopZ(num,den, Wc, BW, LinearSystems.BilinearUnwarp(1,FS));

      MtxVecTee.DrawIt(Response, "Frequency response", false);
      //MtxVecTee.DrawIt(20 * MtxExpr.Log10(MtxExpr.Abs(Response)), "Magnitude", false);
      //MtxVecTee.DrawIt(MtxExpr.PhaseSpectrum(Response) * (180 / Math.PI), "Phase", false);
  }

</code></example>


<SeeAlso cref="Bilinear"/>
<SeeAlso cref="RationalSubstitution"/>
<SeeAlso cref="LowpassToBandstop"/>
<SeeAlso cref="LowpassToLowpassZ"/>
<SeeAlso cref="LowpassToHighpassZ"/>
<SeeAlso cref="LowpassToBandpassZ"/>*)
procedure LowpassToBandstopZ(const z, p: TVec;var k: Double; Freq, BW: Double; PrototypeFreq: Double); overload;

(*<summary> Returns prewarped frequency according to the bilinear transform. </summary>
   
<remarks>The function returns the frequency,
   which will map to Freq after the bilinear transformation has been
   aplied. The mapping is defined as ([1], p. 338, eq 8.28):
  <code>
  Result = (2/Pi)*ArcTan(Freq/(2*FS));
  </code>
  FS defines the sampling frequency. T = 1/FS.

<b>References: </b><para/>
[1] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>


<SeeAlso cref="Bilinear"/>
<SeeAlso cref="BilinearUnwarp"/>*)

function BilinearPrewarp(Freq: Double; FS: Double = 2): Double; overload;
(*<summary> Return unwarped frequency according to the bilinear transform. </summary>
   
<remarks>The function returns the frequency, to which the Freq will map after the bilinear transformation has been
   aplied. The mapping is defined as ([1], p. 338, eq 8.28):
  <code>
  Result = 2*FS*tan(Pi*Freq/2);
  </code>
  FS defines the sampling frequency. T = 1/FS.

<b>References: </b><para/>
[1] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000
</remarks>


<SeeAlso cref="Bilinear"/>
<SeeAlso cref="BilinearPrewarp"/>*)

function BilinearUnwarp(Freq: Double; FS: Double = 2): Double; overload;



