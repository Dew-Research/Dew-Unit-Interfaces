










{$I BdsppDefs.inc}


(*<summary>Design IIR filters.</summary>*)
unit IIRFilters;


interface

uses Math387,MtxVec,SignalUtils;

  (*<summary>Maximum IIR filter order.</summary>
    
<remarks>The constant is used to limit the maximum IIR filter order estimated.
</remarks>


   <SeeAlso cref="MaxFirLength"/>*)
  const MaxIirOrder = 50;

(*<summary>Defines supported IIR filter design methods.</summary>

   <SeeAlso cref="ButterFilter"/>
   <SeeAlso cref="ChebyshevIFilter"/>
   <SeeAlso cref="ChebyshevIIFilter"/>
   <SeeAlso cref="EllipticFilter"/>
   <SeeAlso cref="BesselFilter"/>
   <SeeAlso cref="DcFilter"/>
   <SeeAlso cref="NotchFilter"/>*)
  type  TIirFilterMethod = (
  (*<summary>Butterworth filter.</summary>*)fimButter,
  (*<summary>Chebyshev type I filter.</summary>*)fimChebyshevI,
  (*<summary>Chebyshev type II filter.</summary>*)fimChebyshevII,
  (*<summary>Elliptic filter.</summary>*)fimElliptic,
  (*<summary>DC filter.</summary>*)fimDC,
  (*<summary>Notch filter.</summary>*)fimNotch,
  (*<summary>Butterworth filter.</summary>*)fimBessel
  );

(*<summary>Methods for frequency band transformation of IIR filters.</summary>
  
<remarks>Defines supported approaches for frequency band transformations of IIR filters.
  Frequency band transformations are used to convert a normalized prototype analog
  IIR lowpass filter with a fixed cutoff frequency to arbitrary lowpass, highpass,
  bandpass or bandstop filter. The frequency band transformation can be applied before
  or after the filter has been transformed from s-domain to z-domain.
</remarks>
*)
  TIirFrequencyTransform = (
  (*<summary>Analog filter prototype in the zero-pole form is converted
        to the state-space form. Frequency band transformations are applied first
        (in the s-domain) followed by a bilinear transform and a finally the filter
        is converted from the state-space form back to the zero-pole form.</summary>*)
  ftStateSpaceAnalog,
  (*<summary>Analog filter prototype is obtained in the zero-pole form.
        Frequency band transformations are applied first (in the s-domain) followed
        by a bilinear transform.</summary>*)
  ftZeroPoleAnalog,
  (*<summary>Analog filter prototype is obtained in the zero-pole form.
        A bilinear transform is applied first to map the filter to z-domain.
        Frequency band transformations are applied in the z-domain. This method
        is usefull in cases where the mapping from s-domain to z-domain is not
        bilinear transformation, but impulse invariance or matched z-transform.</summary>*)
  ftZeroPoleDiscrete
  );




   (*<summary>Design analog Butterworth type IIR prototype filter.</summary>
      
<remarks>Design analog butterworth lowpass prototype filter of order Order.
      Place the resulting transfer function in zero-pole form in Z (zeros),
      P (poles) and K (gain).
      The cutoff frequency of the prototype filter is preset to 1 rad/sec.

      The filter has all zeros in infinity. The transfer function is defined as
      ([1], p. 277):
      <pre><c>
                         k0
      H(s) = -------------------------
             (s - s[1])*...*(s - s[n])


      The poles of the filter are located at

      s[k] := Expj(Pi*(0.5+(2*k-1)/(2*n)));

      n = order of filter
      k = 1,...,n
      k0 = gain
      </c> </pre>

      The magnitude response is down 3dB at the cutoff frequency.

<b>References: </b> <para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975.
</remarks>


  

  <example>
    Design an analog lowpass filter with cutoff frequency at 3 rad/sec.

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
      double k, Wc;
      int Order = 5; //design a fifth order filter.

      IIRFilters.ButterAnalog(Order,z, p, out k);  //design analog protype
      Wc = 3; //cutoff frequency
      LinearSystems.LowpassToLowpass(z, p, ref k, Wc);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="ButterFilter"/>
     <SeeAlso cref="LowpassToHighpass"/>
     <SeeAlso cref="Bilinear"/>*)
   procedure ButterAnalog(Order: integer; const z,p: TVec; out k: double);

   (*<summary>Design analog Chebyshev type I IIR prototype filter.</summary>
      
<remarks>Design analog Chebyshev type I lowpass prototype filter of order Order. Place the resulting
      transfer function in zero-pole form in Z (zeros), P (poles) and K (gain).
      PassRipple defines the ripple of the passband (dB). The cutoff frequency
      of the prototype filter is preset to 1 rad/sec, the unit circle.
      Chebyshevs type I filters are all-pole designs and are equiripple in
      the passband. The filter has all zeros in infinity.
      The design formulas are found in [1] p. 232:      
      <pre><c>
      Poles: p[k] = s[k] + j*W[k]

      s[k] = -sinh(Phi)*sin((2*k-1)*Pi/(2*n))
      W[k] =  cosh(Phi)*cos((2*k-1)*Pi/(2*n))

      sinh(phi) =  0.5*(v - 1/v)
      cosh(phi) =  0.5*(v + 1/v)

             1 + (1 + eps^2)^0.5
      v = ( --------------------- )^(1/n)
                    eps

      n - order of the filter
      k = 1,...,n
      </c> </pre>

<b>References: </b> <para/>
[1] "Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975".
</remarks>


  

  <example>
    Design an analog highpass filter with cutoff frequency at 2 rad/sec
    with a 0.2dB ripple in the passband.
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
      double k, Wc;
      int Order = 5; //design a fifth order filter.

      IIRFilters.ChebyshevIAnalog(Order,0.2,z, p, out k);  //design analog protype
      Wc = 2; //cutoff frequency
      LinearSystems.LowpassToLowpass(z, p, ref k, Wc);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="ChebyshevIIFilter"/>
     <SeeAlso cref="LowpassToLowpass"/>
     <SeeAlso cref="Bilinear"/>*)
   procedure ChebyshevIAnalog(Order: integer; PassRipple: double; const z,p: TVec; out k: double);

   (*<summary>Design analog Chebyshev type I IIR prototype filter.</summary>
      
<remarks>Design analog Chebyshev type II lowpass prototype filter of order Order.
      Place the resulting
      transfer function in zero-pole form in Z (zeros), P (poles) and K (gain).
      Ripple defines the StopRipple (dB) of the stopband.The cutoff frequency
      of the prototype filter is preset to 1 rad/sec, the unit circle.

      Chebyshevs type II filters have poles and zeros and are equiripple in
      the stopband. The design formulas are found in [1] p. 232:
      <pre> <c>
                                Wr
      Zeros: z[k] = j* ---------------------
                       cos((2*k-1)/(2*n)*Pi)

      Poles: p[k] = s[k] + j*W[k]

                    Wr*a[k]
      s[k]  = -----------------
               a[k]^2  + b[k]^2

                   -Wr*b[k]
      W[k]  = -----------------
               a[k]^2  + b[k]^2

      a[k] = -sinh(Phi)*sin((2*k-1)*Pi/(2*n))
      b[k] =  cosh(Phi)*cos((2*k-1)*Pi/(2*n))

      sinh(phi) =  0.5*(v - 1/v)
      cosh(phi) =  0.5*(v + 1/v)

      v = (A  + (A^2 - 1)^0.5)^(1/n),    A = 1/sr^2

      n - order of the filter
      k = 1,...,n
      Wr - stopband edge
      sr - stopband ripple
      </c> </pre>

<b>References: </b> <para/>
[1] "Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975".
</remarks>


  

  <example>
    Design an analog bandpass filter with passband between 2 and 3 rad/sec
    and with 20dB ripple in the stopband.

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
      double k, Wc;
      int Order = 5; //design a fifth order filter.

      IIRFilters.ChebyshevIIAnalog(Order,20,z, p, out k);  //design analog protype
      Wc = Math.Sqrt(3*2); //cutoff frequency
      double BW = 3 - 2;
      LinearSystems.LowpassToBandpass(z, p, ref k, Wc,BW);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="ChebyshevIFilter"/>
     <SeeAlso cref="LowpassToHighpass"/>
     <SeeAlso cref="Bilinear"/>*)
  procedure ChebyshevIIAnalog(Order: integer; StopRipple: double; const z,p: TVec; out k: double);

  (*<summary>Design analog Elliptic type IIR prototype filter.</summary>
     
<remarks>Design analog elliptic prototype filter of order Order. Place the resulting
     transfer function in zero-pole form in Z (zeros), P (poles) and K (gain).
     PassRipple defines the ripple (dB) of the passband and StopRipple defines
     the ripple of the stopband (dB). The cutoff frequency
     of the prototype filter is preset to 1 rad/sec.
     For pole and zero specifications see [1] p. 187.

<b>References: </b> <para/>
[1] Digital Filter Design, T.W.Parks and C.S.Burrs, John Wiley and Sons, 1987.
</remarks>


  

  <example>
    Design an analog bandstop filter with stopband between 1 and 3 rad/sec
    and with 20dB ripple in the stopband and 0.1dB in the passband.

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
      double k, Wc;
      int Order = 5; //design a fifth order filter.

      IIRFilters.EllipticAnalog(Order,0.1,20,z, p, out k);  //design analog protype
      Wc = Math.Sqrt(3*1); //cutoff frequency
      double BW = 3 - 1;
      LinearSystems.LowpassToBandstop(z, p, ref k, Wc,BW);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="EllipticFilter"/>
     <SeeAlso cref="LowpassToHighpass"/>
     <SeeAlso cref="Bilinear"/>*)
   procedure EllipticAnalog(Order: integer; PassRipple, StopRipple: double;
                                                  const z,p: TVec; out k: double); overload;
  (*<summary>Design analog Bessel type IIR prototype filter.</summary>
      
<remarks>Design analog Bessel prototype filter of order Order. Place the resulting
      transfer function in zero-pole form in Z (zeros), P (poles) and K (gain).
      The cutoff frequency of the prototype filter is preset to 1 rad/sec.
      The filter has all zeros in infinity.
      The transfer function is defined as([1], p. 230):
      <pre><c>
                d0
      H(s) = --------
               Bn(s)

            (2*n)!               n
      d0 = ------- ,    Bn(s) = Sum(d[k]*s^k),  k = 0,...,n
            2^n*n!              k=0

                (2*n-k)!
      d[k] = --------------    , n = order of the filter
              2^(n-k)*(n-k)!


      Filter poles must be scaled with d0^(1/n)
      </c> </pre>
      
      Roots of the Bessel polynomial Bn(s) are found with the PolyRoots
      routine. Bessel lowpass filters are
      charachterized by the property that the group delay is maximally
      flat at the origing of the s-plane. ([1], p. 228).

<b>References: </b> <para/>
[1] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975.
</remarks>


    

  <example>
    Design an analog lowpass filter with cutoff at 1.1 rad/sec.

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
      double k, Wc;
      int Order = 5; //design a fifth order filter.

      IIRFilters.BesselAnalog(Order,z, p, out k);  //design analog protype
      Wc = 1.1; //cutoff frequency
      LinearSystems.LowpassToLowpass(z, p, ref k, Wc);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="BesselFilter"/>
     <SeeAlso cref="LowpassToHighpass"/>
     <SeeAlso cref="Bilinear"/>*)
   procedure BesselAnalog(Order: integer; const z,p: TVec; out k: double); overload;

   (*<summary>Estimate the order a butterworth IIR filter.</summary>
      
<remarks>Returns the order of a butterworth type IIR filter. Bedg array must contain the band edges
      of the transition region(s) sorted in ascending order. PassRipple defines the ripple of the passband and StopRipple defines
      the ripple of the stopband. The length of the CutoffFreq array must be
      equal to one half of the length of the BEdg array and must match the
      specified FilterType. The routine returns the estimated order as a result
      and fill's the CutoffFreq array. This array can then be passed to the
      ButterFilter routine.
</remarks>


  

  <example>
    Design an analog lowpass filter with transition band between 2 and 6 rad/sec and with at least 40dB attenuation
    at the end of the transition band and. The passband should not have more then 0.2dB ripple.

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
      double k;
      double[] Wc = new double[1];
      int Order; //design a fifth order filter.

      Order = IIRFilters.ButterOrder(new double[2] { 2, 6 }, 0.2,40,TFilterType.ftLowpass,ref Wc,true);  //design analog protype
      IIRFilters.ButterAnalog(Order,z, p, out k);  //design analog protype
      LinearSystems.LowpassToLowpass(z, p, ref k, Wc[0]);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

  <SeeAlso cref="ButterFilter"/>*)
   function  ButterOrder(const BEdges: array of double;  PassRipple,StopRipple: double;
                       FilterType: TFilterType; var CutoffFreq: array of double;
                       Analog: boolean = False): integer;
   (*<summary>Estimate the order a Chebyshev type I IIR filter.</summary>
      
<remarks>Returns the order of the Chebyshev type I filter. Bedg array must contain the band edges
      of the transition region(s) sorted in ascending order. PassRipple defines the ripple of the passband and StopRipple defines
      the ripple of the stopband. The length of the CutoffFreq array must be
      equal to one half of the length of the BEdg array and must match the
      specified FilterType. The routine returns the estimated order as a result
      and fill's the CutoffFreq array. This array can then be passed to the
      ChebyshevIFilter routine.
</remarks>


  

  <example>
    Design an analog highpass filter with transition band
    between 1 and 4 rad/sec and with at least 50dB attenuation
    at the end of the transition band and. The passband
    should not have more then 0.1dB ripple.

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
      double k;
      double[] Wc = new double[1];
      int Order; //design a fifth order filter.

      Order = IIRFilters.ChebyshevIOrder(new double[2] { 1, 4 }, 0.2,40,TFilterType.ftHighpass,ref Wc,true);  //design analog protype
      IIRFilters.ChebyshevIAnalog(Order, 0.1, z, p, out k);  //design analog protype
      LinearSystems.LowpassToHighpass(z, p, ref k, Wc[0]);
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponses(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="ChebyshevIFilter"/>*)
   function  ChebyshevIOrder(const BEdges: array of double;  PassRipple,StopRipple: double;
                       FilterType: TFilterType; var CutoffFreq: array of double;
                       Analog: boolean = False): integer;

   (*<summary>Estimate the order of a Chebyshev type II IIR filter.</summary>
      
<remarks>Returns the order of the Chebyshev type II filter. Bedg array must contain the band edges
      of the transition region(s) sorted in ascending order. PassRipple defines the ripple of the passband and StopRipple defines
      the ripple of the stopband. The length of the CutoffFreq array must be
      equal to one half of the length of the BEdg array and must match the
      specified FilterType. The routine returns the estimated order as a result
      and fill's the CutoffFreq array. This array can then be passed to the
      ChebyshevIIFilter routine.
</remarks>


    

  <example>
    Design an analog bandpass filter with transition band between 1..3  and 6..9 rad/sec and with at least 50dB attenuation
    in the stopband and. The passband should not have more then 0.2dB ripple.

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
      double Wc,k;
      double[] WcArray = new double[2];
      int Order; //design a fifth order filter.

      Order = IIRFilters.ChebyshevIIOrder(new double[4] { 1, 3, 6, 9 }, 0.2, 50, TFilterType.ftBandpass, ref WcArray, true);  //design analog protype
      IIRFilters.ChebyshevIIAnalog(Order, 50, z, p, out k);  //design analog protype
      Wc = Math.Sqrt(WcArray[0] * WcArray[1]); //modified 3dB frequency
      double Bw = WcArray[1] - WcArray[0];
      LinearSystems.LowpassToBandpass(z, p, ref k, Wc, Bw);  //frequency transformation in s-domain
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);

      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="ChebyshevIIFilter"/>*)
   function  ChebyshevIIOrder(const BEdges: array of double;  PassRipple,StopRipple: double;
                       FilterType: TFilterType; var CutoffFreq: array of double;
                       Analog: boolean = False): integer;
   (*<summary>Estimate the order of the Elliptic filter.</summary>
      
<remarks>Bedg array must contain the band edges
      of the transition region(s) sorted in ascending order. PassRipple defines the ripple of the passband and StopRipple defines
      the ripple of the stopband. The length of the CutoffFreq array must be
      equal to one half of the length of the BEdg array and must match the
      specified FilterType. The routine returns the estimated order as a result
      and fill's the CutoffFreq array. This array can then be passed to the EllipticFilter routine.
</remarks>


    

  <example>
    Design a digital bandstop filter with transition band
    between 0.2..0.3 and 0.6..0.7 Hz and with at least 50dB attenuation
    in the stopband and. The passband should not have more then 0.2dB ripple.
    The sampling frequency is 2Hz.

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
      double[] WcArray = new double[2];
      int Order; //design a fifth order filter.
      double FS = 2;

      Order = IIRFilters.EllipticOrder(new double[4] { 0.2, 0.3, 0.6, 0.7 }, 0.2, 50, TFilterType.ftBandstop, ref WcArray, true);  //design analog protype
      IIRFilters.EllipticAnalog(Order, 0.2, 50, z, p, out k);  //design analog protype
      LinearSystems.Bilinear(z, p, ref k, FS, true);
      Wc = Math.Sqrt(WcArray[0] * WcArray[1]); //modified 3dB frequency
      double Bw = WcArray[1] - WcArray[0];
      LinearSystems.LowpassToBandStopZ(z, p, ref k, Wc, Bw, LinearSystems.BilinearUnwarp(1,FS));
      LinearSystems.ZeroPoleToTransferFun(num, den, z, p, k);
      SignalUtils.FrequencyResponse(num, den, Response, 32, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="EllipticFilter"/>*)
   function  EllipticOrder(const BEdges: array of double; PassRipple,StopRipple: double;
                       FilterType: TFilterType; var CutoffFreq: array of double;
                       Analog: boolean = False): integer;

   (*<summary>Design Butterworth IIR filter.</summary>
      
<remarks>Design Butterworth filter of Order with CutoffFreq frequencies and of FilterType
      type. Set Analog to True, to request and analog filter design in s-plane
      or set it to false to obtain a digital filter design in z-plane.
      The CutoffFreq must be in range between 0 and 1 (Sampling frequency = 2) in
      case of a digital filter design.

      IIrFrequencyTransform specifies when and how will the frequency band transformation be applied.
      The resulting transfer function is returned in the zero-pole form, with z,p,k variables.
</remarks>
*)
   function ButterFilter(Order: integer;const CutoffFreq: array of double;
                     FilterType: TFilterType; Analog: boolean; const z,p: TVec; out k: double;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;
   (*<summary>The resulting transfer function is returned in the numerator/denumerator form with num and den.</summary>*)
   function ButterFilter(Order: integer;const CutoffFreq: array of double;
                     FilterType: TFilterType; Analog: boolean; const num,den: TVec;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;
   (*<summary>The resulting transfer function is returned in the second order section form stored in the sos variable.</summary>
              
<remarks>The sos variable can be passed directly to the IirInitBQ filter initialization routine.
              Second order section form delivers substantially higher numerical stability and range than
              filtering with num/den form which is used by the IirInit routine.
</remarks>
*)
   function ButterFilter(Order: integer;const CutoffFreq: array of double;
                     FilterType: TFilterType; Analog: boolean; const sos: TVec;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;
   (*<summary>The resulting transfer function is returned in the state-space form with A,B,C,D variables.</summary>

   

  <example>
    Design a digital bandstop filter with transition band
    between 0.2..0.3 and 0.6..0.7 Hz and with at least 50dB attenuation
    in the stopband. The passband should not have more then 0.2dB ripple.
    The sampling frequency is 2Hz.

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
      double[] WcArray = new double[2];
      int Order; //design a fifth order filter.

      Order = IIRFilters.ButterOrder(new double[4] { 0.2, 0.3, 0.6, 0.7 }, 0.2, 50, TFilterType.ftBandstop, ref WcArray, true);  //design analog protype
      IIRFilters.ButterFilter(Order, WcArray, TFilterType.ftBandstop, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);

      // Alternative 1. Specify the order and the 3 dB frequencies explicitely:
      //
      //     IIRFilters.ButterFilter(5, new double[2] {0.2,0.7}, TFilterType.ftBandstop,false,num,den,TIirFrequencyTransform.ftStateSpaceAnalog);


      // Alternative 2. Specifying the 3 dB frequencies explicitely
      // will result in 3 dB ripple (and not 0.2 as requested) in the passband,
      // but one coulde always move the 3 dB frequencies a little:
      //
      //     IIRFilters.ButterFilter(5, new double[2] { 0.22, 0.68 }, TFilterType.ftBandstop, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);

      // Alternative 3. Specifying the order explicitely
      // will not ensure 50 dB attenuation in the edges of the stopband,
      // but one can increase filter order:
      //
      //     IIRFilters.ButterFilter(10, new double[2] { 0.22, 0.68 }, TFilterType.ftBandstop, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);

      SignalUtils.FrequencyResponse(num, den, Response, 32, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
    </code></example>

     <SeeAlso cref="ButterOrder"/>
     <SeeAlso cref="ChebyshevIFilter"/>
     <SeeAlso cref="ChebyshevIIFilter"/>
     <SeeAlso cref="EllipticFilter"/>
     <SeeAlso cref="BesselFilter"/>*)
   function ButterFilter(Order: integer;const CutoffFreq: array of double;
                     FilterType: TFilterType; Analog: boolean; const A: TMtx; const B,C: TVec; out d: double): double; overload;

   (*<summary>Design Chebyshev type I IIR filter.</summary>
      
<remarks>Design Chebyshev type I filter of Order with CutoffFreq frequencies and of FilterType
      type. Set Analog to True, to request and analog filter design in s-plane
      or set it to false to obtain a digital filter design in z-plane.
      PassRipple defines the passband ripple in dB.
      The CutoffFreq must be in range between 0 and 1 (Sampling frequency = 2) in
      case of a digital filter design.

      IIrFrequencyTransform specifies when and how will the frequency band transformation be applied.
      The resulting transfer function is returned in the zero-pole form, with z,p,k variables.
</remarks>
*)
   function ChebyshevIFilter(Order: integer; PassRipple: double;
                     const CutoffFreq: array of double; FilterType: TFilterType;
                     Analog: boolean; const z,p: TVec; out k: double;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;
   (*<summary>The resulting transfer function is returned in the numerator/denumerator form with num and den.</summary>*)
   function ChebyshevIFilter(Order: integer; PassRipple: double;
                     const CutoffFreq: array of double; FilterType: TFilterType;
                     Analog: boolean; num,den: TVec;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;
   (*<summary>The resulting transfer function is returned in the second order section form stored in the sos variable.</summary>
              
<remarks>The sos variable can be passed directly to the IirInitBQ filter initialization routine.
              Second order section form delivers substantially higher numerical stability and range than
              filtering with num/den form which is used by the IirInit routine.
</remarks>
*)
   function ChebyshevIFilter(Order: integer; PassRipple: double;
                     const CutoffFreq: array of double; FilterType: TFilterType;
                     Analog: boolean; const sos: TVec;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;


   (*<summary>The resulting transfer function is returned in the state-space form with A,B,C,D variables.</summary>

    

  <example>
    Design an analog bandpass filter with transition band
    between 1..2  and 5..7 rad/sec and with at least 50dB attenuation
    in the stopband and. The passband should not have more then 0.2dB ripple.
    <para/>
    Note:
    This example does not actually filter data. It only designs the filter.
    To see how to actually apply the filter check the IirInit and IirInitBQ routines.

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
      Vector Response = new Vector(0);
      Vector FreqFr = new Vector(0);
      double[] WcArray= new double[2];

      //design analog filter
      int Order = IIRFilters.ChebyshevIOrder(new double[4] { 1, 2, 5, 7 }, 0.2, 50, TFilterType.ftBandpass, ref WcArray, true);
      IIRFilters.ChebyshevIFilter(Order, 50, WcArray, TFilterType.ftBandpass, true, num,den, TIirFrequencyTransform.ftStateSpaceAnalog);  //design analog protype
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);
      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);

      //Alternative: Design a digital filter with the passband between 0.2 and 0.5 Hz (FS =2)
      //Order = IIRFilters.ChebyshevIOrder(new double[4] { 0.1, 0.2, 0.5, 0.6 }, 0.2, 50, TFilterType.ftBandpass, ref WcArray, false);
      //IIRFilters.ChebyshevIFilter(Order, 50, WcArray, TFilterType.ftBandpass, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);  //design digital protype
    	//FrequencyResponse(num,den,Response,64);
      //FreqFr = MtxExpr.Ramp(Response.Length, TMtxFloatPrecision.mvDouble, 0, 1.0 / Response.Length);
      //MtxVecTee.DrawIt(FreqFr, Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="IirFilter"/>
     <SeeAlso cref="ChebyshevIOrder"/>
     <SeeAlso cref="ButterFilter"/>
     <SeeAlso cref="ChebyshevIIFilter"/>
     <SeeAlso cref="EllipticFilter"/>
     <SeeAlso cref="BesselFilter"/>*)
   function ChebyshevIFilter(Order: integer; PassRipple: double;
                     const CutoffFreq: array of double; FilterType: TFilterType;
                     Analog: boolean; const A: TMtx; const B,C: TVec; out d: double): double; overload;

   (*<summary>Design Chebyshev type II IIR filter.</summary>
      
<remarks>Design Chebyshev type II filter of Order with CutoffFreq frequencies and of FilterType
      type. Set Analog to True, to request an analog filter design in s-plane
      or set it to false to obtain a digital filter design in z-plane.
      StopRipple defines the stopband ripple in dB.
      CutoffFreq must be in range between 0 and 1 (Sampling frequency = 2)
      in case of a digital filter design.

      IirFrequencyTransform specifies when and how will the frequency band transformation be applied.
      The resulting transfer function is returned in the zero-pole form, with z,p,k variables.
</remarks>
*)
   function ChebyshevIIFilter(Order: integer; StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const z, p: TVec; out k: double;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the second order section form stored in the sos variable.</summary>
              
<remarks>The sos variable can be passed directly to the IirInitBQ digital filter initialization routine.
              Second order section form delivers substantially higher numerical stability and range than
              filtering with num/den form which is used by the IirInit routine.
</remarks>
*)
   function ChebyshevIIFilter(Order: integer; StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const sos: TVec;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the numerator/denumerator form with num and den.</summary>*)
   function ChebyshevIIFilter(Order: integer; StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const num, den: TVec;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the state-space form with A,B,C,D variables.</summary>

  

  <example>
    Design a discrete highpass filter with transition band
    between 10..12 Hz, if the sampling frequency is 30 Hz.
    The stopband should have more then 50 dB attenuation and the passband
    should not have more then 0.2dB ripple. This example does not actually filter
    data. It only designs the filter. To see how to actually apply the filter
    check the IirInit and IirInitBQ routines.

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

      double[] WcArray = new double[1];
      int Order;

      Order = IIRFilters.ChebyshevIIOrder(new double[2] { 10.0*2/30, 12.0*2/30 }, 0.2, 50, TFilterType.ftHighpass, ref WcArray, false);  //design analog protype
      IIRFilters.ChebyshevIIFilter(Order, 50, WcArray, TFilterType.ftHighpass, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);
      SignalUtils.FrequencyResponse(num, den, Response, 32, false, TSignalWindowType.wtRectangular, 0);
      Vector FreqFr = MtxExpr.Ramp(Response.Length, mvDouble,0, 1.0 / Response.Length); //X axis
      DrawIt(FreqFr,20*MtxExpr.Log10(MtxExpr.Abs(Response)),"Magnitude",false);
  }
  </code></example>

     <SeeAlso cref="SignalUtils.IirFilter"/>
     <SeeAlso cref="ButterFilter"/>
     <SeeAlso cref="ChebyshevIFilter"/>
     <SeeAlso cref="EllipticFilter"/>
     <SeeAlso cref="BesselFilter"/>
     <SeeAlso cref="ChebyshevIIOrder"/>
     <SeeAlso cref="ChebyshevIAnalog"/>
     <SeeAlso cref="Bilinear"/>*)
   function ChebyshevIIFilter(Order: integer; StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const A: TMtx; const B,C: TVec; out d: double): double; overload;

   (*<summary>Design Elliptic IIR filter.</summary>
      
<remarks>Design Elliptic filter of Order with CutoffFreq frequencies and of FilterType
      type. Set Analog to True, to request an analog filter design in s-plane
      or set it to false to obtain a digital filter design in z-plane.
      CutoffFreq must be in range between 0 and 1 (Sampling frequency = 2)
      in case of a digital filter design.

      IIrFrequencyTransform specifies when and how will the frequency band transformation be applied.
      PassRipple defines the passband ripple in dB and StopRipple defines the stopband ripple in dB.
      The resulting transfer function is returned in the zero-pole form, with z,p,k variables.
</remarks>
*)
   function EllipticFilter(Order: integer; PassRipple,StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const z, p: TVec; var k: double;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the numerator/denumerator form with num and den.</summary>*)
   function EllipticFilter(Order: integer; PassRipple,StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const num, den: TVec;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the second order section form stored in the sos variable.</summary>
              
<remarks>The sos variable can be passed directly to the IirInitBQ digital filter initialization routine.
              Second order section form delivers substantially higher numerical stability and range than
              filtering with num/den form used by the IirInit function.
</remarks>
*)
   function EllipticFilter(Order: integer; PassRipple,StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const sos: TVec;
          IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the state-space form with A,B,C,D variables.</summary>

  

  <example>
    Design a discrete lowpass filter with transition band
    between 5..6 Hz, if the sampling frequency is 30 Hz.
    The stopband should have more then 40 dB attenuation and the passband
    should not have more then 0.1dB ripple.
    <para/>
    Note:
    This example does not actually filter data. It only designs the filter.
    To see how to actually apply the filter check the IirInit and IirInitBQ routines.

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

      double[] WcArray = new double[1];
      int Order; //design a fifth order filter.

      Order = IIRFilters.EllipticOrder(new double[2] { 5.0/15, 6.0/15 }, 0.1, 40, TFilterType.ftLowpass, ref WcArray, false);  //design analog protype
      IIRFilters.EllipticFilter(Order, 0.1,40, WcArray, TFilterType.ftLowpass, false, num, den, TIirFrequencyTransform.ftStateSpaceAnalog);

      SignalUtils.FrequencyResponse(num, den, Response, 32, false, TSignalWindowType.wtRectangular, 0);

      Vector FreqFr = MtxExpr.Ramp(Response.Length, TMtxFloatPrecision.mvDouble,0, 30*0.5 / Response.Length); //X axis
      MtxVecTee.DrawIt(FreqFr, Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="IirFilter"/>
     <SeeAlso cref="ButterFilter"/>
     <SeeAlso cref="ChebyshevIFilter"/>
     <SeeAlso cref="ChebyshevIIFilter"/>
     <SeeAlso cref="BesselFilter"/>
     <SeeAlso cref="EllipticAnalog"/>
     <SeeAlso cref="Bilinear"/>
     <SeeAlso cref="EllipticOrder"/>*)
   function EllipticFilter(Order: integer; PassRipple,StopRipple: double;
          const CutoffFreq: array of double; FilterType: TFilterType;
          Analog: boolean; const A: TMtx; const B, C: TVec; out d: double): double; overload;

   (*<summary>Design Bessel IIR filter.</summary>
      
<remarks>Design Bessel filter of Order with CutoffFreq frequencies and of FilterType
      type. Set Analog to True, to request an analog filter design in s-plane
      or set it to false to obtain a digital filter design in z-plane.
      CutoffFreq must be in range between 0 and 1  (Sampling frequency = 2).

      IIrFrequencyTransform specifies when and how will the frequency band transformation be applied.
      Bessel filters typically do not preserve a flat group delay once
      transformed to z-domain. The routine uses bilinear transformation to
      map from s to z-domain. Use matched-Z transform to preserve more phase
      properties of the Bessel filters in the z-domain.
      The resulting transfer function is returned in the zero-pole form, with z,p,k variables.
</remarks>
*)
   function BesselFilter(Order: integer;const CutoffFreq: array of double;
                     FilterType: TFilterType; Analog: boolean; const z, p: TVec; var k: double;
                     IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

   (*<summary>The resulting transfer function is returned in the numerator/denumerator form with num and den.</summary>*)
   function BesselFilter(Order: integer;const CutoffFreq: array of double;
                         FilterType: TFilterType; Analog: boolean; const num,den: TVec;
                         IirFrequencyTransform: TIirFrequencyTransform = ftStateSpaceAnalog): double; overload;

  (*<summary>The resulting transfer function is returned in the state-space form with A,B,C,D variables.</summary>

  

  <example>
    Design a fifth order analog lowpass filter with the cutoff frequency at 3 rad/sec.
    <para/>
    Note:
    This example does not actually filter data. It only designs the filter.
    To see how to actually apply the filter check the IirInit and IirInitBQ routines.

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
      Vector Response = new Vector(0);
      Vector FreqFr = new Vector(0);

      IIRFilters.BesselFilter(5, new double[1] {3}, TFilterType.ftLowpass, true, num,den,TIirFrequencyTransform.ftStateSpaceAnalog);  //design analog protype
      FreqFr.Length = 1000;
      SignalUtils.LogRamp(FreqFr, -1, 1);
      SignalUtils.FrequencyResponseS(num, den, FreqFr, Response, 0);
      MtxVecTee.DrawIt(Response, "Frequency response", false);
  }
  </code></example>

     <SeeAlso cref="IirFilter"/>
     <SeeAlso cref="ButterFilter"/>
     <SeeAlso cref="ChebyshevIFilter"/>
     <SeeAlso cref="ChebyshevIIFilter"/>
     <SeeAlso cref="EllipticFilter"/>
     <SeeAlso cref="BesselAnalog"/>
     <SeeAlso cref="Bilinear"/>
     <SeeAlso cref="ButterOrder"/>*)
  function BesselFilter(Order: integer;const CutoffFreq: array of double;
                         FilterType: TFilterType; Analog: boolean; const A: TMtx; const B, C: TVec; out d: double): double; overload;



  (*<summary>Convert a TIirFilterMethod type to a string.</summary>
    
<remarks>The function returns a description of TIirFilterMethod specified
    by the  IirFilterMethod variable.
</remarks>
*)
  function IirFilterMethodToString(IirFilterMethod: TIIRFilterMethod): string;

  (*<summary>Compute exact zeroes of Butterworth and Cheybshev type I filter.</summary>
    
<remarks>The order defies the Order of the filter, WC is the bilinear warped center frequency
    of the stopband of a bandstop filter. FilterType defines, if the filter is lowpass,
    highpass, bandpass or bandstop, Analog specifies, if the filter is designed
    for the s-domain or for the z-domain. The result is placed in z.
</remarks>


    <SeeAlso cref="IirFilter"/>
    <SeeAlso cref="ButterFilter"/>
    <SeeAlso cref="ChebyshevIFilter"/>*)
  procedure ExactIirZeros(Order: integer; Wc: double;
                        FilterType: TFilterType; Analog: boolean; const z: TVec); overload;




