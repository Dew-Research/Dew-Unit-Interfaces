










{$I BdsppDefs.inc}


(*<summary>Parks-McClellan optimal filter design routines.</summary>*)
unit OptimalFir;


interface

uses Math387, MtxVec, SignalUtils, MtxVecBase;

type

(*<summary>Defines the type of the filter for Parks-McClellan algorithm.</summary>
  
<remarks>Defines the type of the filter the Parks-McClellan algorithm can create.
  Althought it is possible to design Double integrators
  and Double differentiators, these filter types do not
  have phase shifted as required (Double differentiator
  requires a 180 degree phase shift), but their amplitude
  response is correct. A -90 degree phase shift is a delay and
  a +90 degree shift is possible, because the total filter delay
  is (n-1)/2, where n is the length of the impulse response. The phase
  shift is evaluated relatively to the total filter delay.
  A filter with -90 degrees phase shift can be converted to a filter
  with a +90 degree phase shift, by scaling all the taps with -1.
  Inverse Hilbert transformer can be obtained by scaling the Hilbert
  transformer with -1.
</remarks>
*)
TRemezType = (
  (*<summary>Supports all lowpass, highpass, bandpass, bandstop and multiband filters.</summary>*)
  rmtBandPass,
  (*<summary>Designs a filter with a +20dB/decade slope and +90 degree phase shift.</summary>*)
  rmtDifferentiator,
  (*<summary>Designs an allpass filter with a -90 degree phase shift.</summary>*)
  rmtHilbert,
  (*<summary>Designs a filter with a -20dB/decade slope and a -90 degree phase shift.</summary>*)
  rmtIntegrator,
  (*<summary>Designs a filter with a +40dB/decade slope and a +90 degree phase shift.</summary>*)
  rmtDoubleDifferentiator,
  (*<summary>Designs a filter with a -40dB/decade slope and a -90 degree phase shift.</summary>*)
  rmtDoubleIntegrator
  );

(*<summary>Defines filter symmetry.</summary>
  
<remarks>Define the filter symmetry, positive or negative. Positive symmetry is defined
  as h[-i] = h[i] and negative symmetry as h[-i] = -h[i], where h is centered around zero.
  Filters with negative symmetry shift the phase by 90 degrees across all frequencies.
</remarks>
*)
TFilterSymmetry = (
  (*<summary></summary>*)
  fsmPositive,
  (*<summary></summary>*)
  fsmNegative);






(*<summary>Design an optimal equiripple FIR filter with Parks-McClellan algorithm.</summary>
  <param name="h"> An array of length h.Length on entry
    and contains the filter impulse response on exit.</param>
  <param name="bands"> Defines the frequency bands. </param>
  <param name="gains"> Defines, if the band is a stopband or a passband.</param>
  <param name="Weights"> Array contains the ratios between
    required ripples for different bands.</param>
  <param name="FS"> Specifies the sampling frequency.</param>
  <param name="err"> Contains the maximum ripple error upon return.</param>
  <param name="FilterType">Choose between bandpass, hilbert, differentator and integrator.</param>
  <param name="ConstantRipple"> If True, the ripple will be constant and
    not weighted to give constant percentage error in case
    of the following filter types: rmtDifferentiator, rmtIntegrator,
    rmtDoubleDifferentiator, rmtDoubleIntegrator
</param>

  
<remarks>Designs an equiripple (optimal) FIR filter. The routine will not always converge.
  Parameters have to be specified, for which the filter exists. Most common
  causes for trouble are:
  <list>
  <item> too wide transition bands </item>
  <item> transition bands not of equal width. </item>
  <item> too strong attenuation (of the stopband) or to small ripple (of the passband) specified.</item>
  <item> wrong filter length (odd/even). </item>
  </list>
  <para/>
   The length of the filter can be estimated with the RemezLength routine.
   RemezLength routine will also properly adjust the error weights.
   An extensive explanation of the algorithm can be found in [1] Ch. 7.6, p. 462. <para/>

  <list>
   A few need to know things about FIR filters:
<item>  The length of the filter is defined as: n = Order-1. (Order is the order of the polynomial and n is the number
      of coefficients.)</item>
<item>  highpass or a bandstop filter or any filter with the passband at FS/2 has to have odd length (even order).
     RemezLength routine automatically adjusts filter  length.</item>
<item>  a filter with negative symmetry also shifts the phase by 90 degrees. The routine automatically assumes negative
     symmetry, if the FilterType is different from rmtBandpass.</item>
<item>  Required stopband attenuation is usually specified in dB. To obtain the required ripple, the following
     formula can be used: Ripple = Exp10(Att/-20);</item>
<item>  Passband ripple can also be specified in dB. To
     obtain the required linear ripple, the following formula can be used: Ripple = (1-Exp10(PassRippldB/-20))/2</item>
<item>  The passband ripple "rp" specified for the passband is a +/-rp specification. Total ripple is 2*rp.</item>
<item>  the amount of ripple is a function of filter length.
     (longer filters give less ripple).</item>
<item>  if different bands have different required ripple, this
     can be addressed by adjusting the error weights array.</item>
  </list>
     The Fortran source code can be found in [2] p. 198.<para/>

<b>References: </b>  <para/>
<see href="[1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989."/> <para/>
<see href="[2] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975."/><para/>
</remarks>


  

  <example>
    A sample lowpass filter design with sampling frequency 8000Hz, transition
    band between at 1500Hz and 2000Hz with 60 dB attenuation
    (20 * Log10(0.001)) and 0.001 ripple in the passband. To try out other
    setups comment out the lowpass and comment in the desired filter.

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
      Vector Response = new Vector(0);
      Vector Weights = new Vector(0);

      int n;
      double err;
      double FS = 2; //Sampling frequency

//Design a lowpass
      n = OptimalFir.RemezLength(new double[4] {0, 1500, 2000, 4000},
                                 new double[2] {1, 0}, new double[2] {0.001, 0.001} ,Weights, 8000);

      z.Size(n);
      OptimalFir.remez( z, new double[4] { 0, 1500, 2000, 4000 },
                           new double[2] { 1, 0 }, (double []) Weights, TRemezType.rmtBandPass, out err, 8000, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Lowpass", false);

//Design a highpass
      n = OptimalFir.RemezLength(new double[4] { 0, 1500, 2000, 4000 },
                                 new double[2] { 0, 1 }, new double[2] { 0.001, 0.001 }, Weights, 8000);
      z.Size(n);
      OptimalFir.remez(z, new double[4] { 0, 1500, 2000, 4000 },
            new double[2] { 0, 1 }, (double[])Weights, TRemezType.rmtBandPass, out err, 8000, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Highpass", false);

//Design a bandpass (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0.15, 0.25, 0.45, 0.55, 1},
                                 new double[3] {0, 1, 0},new double[3] {0.001, 0.001, 0.001},Weights,FS);
      z.Size(n);
      OptimalFir.remez(z,new double[6] {0, 0.15, 0.25, 0.45, 0.55, 1},
                         new double[3] {0, 1, 0}, (double []) Weights, TRemezType.rmtBandPass, out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Bandpass", false);

//Design a bandstop (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] { 0, 0.15, 0.25, 0.45, 0.55, 1 },
                                 new double[3] { 1, 0, 1 }, new double[3] { 0.001, 0.001, 0.001 }, Weights,FS) ;
      z.Size(n);
      OptimalFir.remez(z, new double[6] { 0, 0.15, 0.25, 0.45, 0.55, 1 },
                          new double[3] { 1, 0, 1 }, (double[]) Weights, TRemezType.rmtBandPass, out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Bandstop", false);

//Design a multiband (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[10] {0.00, 0.10, 0.20, 0.30, 0.40, 0.45, 0.55, 0.60, 0.70, 1.00},
                                 new double[5] { 1, 0, 1, 0, 1 }, new double[5] { 0.001, 0.001, 0.001, 0.001, 0.001 }, Weights, FS);
      z.Size(n);
      OptimalFir.remez(z, new double[10] {0.00, 0.10, 0.20, 0.30, 0.40,0.45, 0.55, 0.60, 0.70, 1.00},
                          new double[5] { 1, 0, 1, 0, 1 }, (double[]) Weights, TRemezType.rmtBandPass, out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Multibandpass", false);

//  Design a hilbert (type III) transformer  (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.1, 0.9, 1, 1},
                                 new double[3] {0,1,0},new double[3] {0.01,0.01,0.01},Weights,FS);
      if (n % 2 == 0) n++; //odd length type III filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.1, 0.9},
                         new double[1] {1}, new double[1] {1}, TRemezType.rmtHilbert,out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Hilbert III", false);

//Design a hilbert (type IV) transformer  (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.1, 0.9, 1, 1},
                                 new double[3] {0,1,0},new double[3] {0.01,0.01,0.01},Weights,FS);
      if (n % 2 != 0) n++; //even length type IV filter
      z.Size(n);
      OptimalFir.remez(z, new double[2] {0.1, 1},
                          new double[1] {1},new double[1] {1}, TRemezType.rmtHilbert,out err, 2, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Hilbert IV", false);

//Design a differentiator (type III) (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.1, 0.9, 1, 1},
                                 new double[3] {0,1,0}, new double[3] {0.01,0.01,0.01},Weights,FS);
      if (n % 2 == 0) n++; //odd length type III filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.1, 0.9},
                         new double[1] {1},new double[1] {1}, TRemezType.rmtDifferentiator,out err, 2, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Differentiator III", false);

//Design a differentiator (type IV) (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.1, 0.9, 1, 1},
                                 new double[3] {0,1,0},new double[3] {0.01,0.01,0.01},Weights, FS);
      if (n % 2 != 0) n++; //even length type IV filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.1, 1},
                         new double[1] {1},new double[1] {1}, TRemezType.rmtDifferentiator,out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Differentiator IV", false);

//Design a double differentiator (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.05, 0.95, 1, 1},
                                 new double[3] {0,1,0},new double[3] {0.01,0.01,0.01},Weights, FS);
      if (n %2 != 0) n++; //even length type IV filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.05, 1},
                         new double[1] {1},new double[1] {1}, TRemezType.rmtDoubleDifferentiator,out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Double differentiator IV", false);

//Design an integrator (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.05, 0.95, 1, 1},
                                 new double[3] {0,1,0},new double[3] {0.01,0.01,0.01}, Weights, FS);
      if (n %2 != 0) n++; //even length type IV filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.05, 1},
                         new double[1] {1},new double[1] {1}, TRemezType.rmtIntegrator,out err, FS, false);

      SignalUtils.FrequencyResponse(z, null, Response, 8, false, TSignalWindowType.wtRectangular, 0);
      MtxVecTee.DrawIt(Response, "Integrator IV", false);

//Design a double integrator  (Sampling frequency = 2)
      n = OptimalFir.RemezLength(new double[6] {0, 0, 0.05, 0.95, 1, 1},
                                 new double[3] {0,1,0}, new double[3] {0.01,0.01,0.01},Weights,FS);
      if (n %2 != 0) n++; //even length type IV filter
      z.Size(n);
      OptimalFir.remez(z,new double[2] {0.05, 1},
                         new double[1] {1}, new double[1] {1}, TRemezType.rmtDoubleIntegrator,out err, FS, false);

      SignalUtils.FrequencyResponse(z,null,Response,8,false,TSignalWindowType.wtRectangular,0);
      MtxVecTee.DrawIt(Response,"Double integrator IV",false);
    }
    </code></example>


  <SeeAlso cref="KaiserImpulse"/>
  <SeeAlso cref="SavGolayImpulse"/>
  <SeeAlso cref="RemezImpulse"/>
  <SeeAlso cref="RemezLength"/>*)
function remez(const h: TVec; const bands, gains, weights: array of Double;
               FilterType: TRemezType; out err: Double; FS: Double = 2; ConstantRipple: boolean = false): integer;

  (*<summary>Design an optimal equiripple FIR filter with Parks-McClellan algorithm.</summary>

  <param name="H"> H vector holds the impulse response on exit. </param>
  <param name="Ripple"> The required linear ripple of the passband and 20*Log10(Ripple) is the required attenuation of the stop band. </param>
  <param name="FilterType"> Parameter defines the filter type. </param>
  <param name="Gain"> Specifies the gain of the passband. </param>
  <param name="EnsureOdd">Resulting filter length will be odd (not divisable by 2), if set to true. Default is true.</param>
  <param name="FS"> The sampling frequency used to normalize transition band edges defined in the W array. Default value for FS is 2. </param>
  <param name="W"> Array which can hold only 2 (highpass/lowpass definition) or 4(bandpass/bandstop  definition) parameters. </param>

   
<remarks>The resulting impulse response is placed in H. Length
   of the filter is automatically estimated from the required Ripple and transition
   bandwidth. Function returns True, if the filter was succesfully designed. This does not guarantee that
   filter specifications have been meet.<para/>

   This routine is a simplified version of Remez and can be used to design:
   Lowpass, bandpass, bandstop, highpass, differentiators and hilbert
   transformers. <para/>

   Note: <para/>
    RemezImpulse routine designes FIR filters about 10-20% shorter than the KaiserImpulse routine.
</remarks>


  <SeeAlso cref="remez"/>*)
function RemezImpulse(const H: TVec;const W: array of Double; Ripple: Double; FilterType: TFilterType;
                       Gain: Double  = 1; FS: Double = 2; EnsureOdd: boolean = True): boolean; overload;

(*<summary>Design an optimal equiripple FIR filter with Parks-McClellan algorithm.</summary>
   
<remarks>Required length of the filter must be preset by setting H.Length.
   H vector holds the impulse response on exit.
</remarks>




  <example>
    RemezImpulse examples. Comment out the filter setup that you need.
  <code>

    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
   {
       Vector H = new Vector(0);
       Vector Response = new Vector(0);

  //Assumed sampling frequency = 2
       double FS = 2;
       double TransBW = 0.02; //transition bandwidth in Hz.
       double Ripple = 0.001;
//Lowpass filter
     OptimalFir.RemezImpulse(H,new double[2] {0.3,0.3+TransBW},Ripple, TFilterType.ftLowpass,1,FS,false);
//Highpass filter
       OptimalFir.RemezImpulse(H,new double[2] {0.3,0.3+TransBW},Ripple, TFilterType.ftHighpass,1,FS,false);
//Bandpass filter
       OptimalFir.RemezImpulse(H,new double[4] {0.3,0.3+TransBW, 0.5-TransBW,0.5},Ripple, TFilterType.ftBandpass,1,FS,false);
//Bandstop filter
       OptimalFir.RemezImpulse(H,new double[4] {0.3,0.3+TransBW, 0.5-TransBW,0.5},Ripple, TFilterType.ftBandstop, 1,FS,false);


//  Type III Hilbert transformer
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1-TransBW},Ripple, TFilterType.ftHilbertIII,1,FS,false);

//  Type IV Hilbert transformer
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1},Ripple, TFilterType.ftHilbertIV,1,FS,false);

//  Type III linear phase differentiator filter
       SignalUtils.KaiserImpulse(H,new double[2] {1-TransBW,1},Ripple, TFilterType.ftDifferentiatorIII,1,FS,false);
       H.Scale(FS);  //Scale by sampling frequency

//  Type IV linear phase differentiator filter
       SignalUtils.KaiserImpulse( H,new double[2] {1-TransBW,1},Ripple, TFilterType.ftDifferentiatorIV,1,FS,false);
       H.Scale(FS);  //Scale by sampling frequency

//  Type III differentiator filter
       OptimalFir.RemezImpulse(H,new double[2] {0,1-TransBW},Ripple, TFilterType.ftDifferentiatorIII,1,FS,false);
       H.Scale(FS);  //Scale by sampling frequency

//  Type IV differentiator filter
       OptimalFir.RemezImpulse( H,new double[2] {0,1-TransBW},Ripple, TFilterType.ftDifferentiatorIV,1,FS,false);
       H.Scale(FS);  //Scale by sampling frequency

// Type III 2x differentiator filter (remez)
       OptimalFir.RemezImpulse(H,new double[2] {0,1-TransBW},Ripple, TFilterType.ftDoubleDifferentiatorIII,1,FS,false);
       H.Scale(FS*FS);  //Scale by sampling frequency

// Type IV 2x differentiator filter (remez)
       OptimalFir.RemezImpulse(H,new double[2] {0,1-TransBW},Ripple, TFilterType.ftDoubleDifferentiatorIV,1,FS,false);
       H.Scale(FS*FS);  //Scale by sampling frequency

// Type III integrator  filter (remez).';
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1-TransBW},Ripple, TFilterType.ftIntegratorIII,1,FS,false);
       H.Scale(1/FS);  //Scale by sampling frequency

// Type IV integrator  filter (remez).';
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1},Ripple, TFilterType.ftIntegratorIV,1,FS,false);
       H.Scale(1/FS);  //Scale by sampling frequency

//  Type III 2x integrator  filter (remez).';
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1-TransBW},Ripple, TFilterType.ftDoubleIntegratorIII,1,FS,false);
       H.Scale(Math.Sqrt(1/FS));  //Scale by sampling frequency

//  Type IV 2x integrator  filter (remez).';
       OptimalFir.RemezImpulse(H,new double[2] {TransBW,1},Ripple, TFilterType.ftDoubleIntegratorIV,1,FS,false);
       H.Scale(Math.Sqrt(1/FS));  //Scale by sampling frequency

       SignalUtils.FrequencyResponse(H,null,Response,16,false,TSignalWindowType.wtRectangular,0);
       MtxVecTee.DrawIt(Response,"",false);
</code></example>

  <SeeAlso cref="KaiserImpulse"/>
  <SeeAlso cref="SavGolayImpulse"/>
  <SeeAlso cref="remez"/>
  <SeeAlso cref="FirImpulse"/>*)
function RemezImpulse(const H: TVec; const W: array of Double; FilterType: TFilterType;
                       Gain: Double  = 1; FS: Double = 2): boolean; overload;

 (*<summary>Estimate the length of an optimal FIR filter.</summary>
   
<remarks>Returns the length of the equiripple FIR filter,
   design with Parks-McClellan algorithm
   where the maximum allowed ripple of the pass band is Ripple and
   sampling frequency is FS. The requested stopband attenuation is
   estimated as 20*Log10(Ripple). The W array holds two or four parameters:
   the start and the stop of the transition bands, relative
   to the specified sampling frequency. The maximum filter order
   is limited with MaxFirLength global variable.

   Note
    This routine is a simplified version of RemezLength routine.
</remarks>


  <SeeAlso cref="RemezLength"/>
  <SeeAlso cref="RemezImpulse"/>
  <SeeAlso cref="KaiserFirLength"/>
  <SeeAlso cref="KaiserImpulse"/>
  <SeeAlso cref="FirImpulse"/>*)
function RemezFirLength(const W: array of Double; Ripple: Double; FilterType: TFilterType; FS: Double = 2): integer; overload;

(*<summary>Estimate the length of an optimal FIR filter.</summary>
  
<remarks>Estimate the length of an optimal FIR filter designed with Parks-McClellan algorithm.
  See the remez routine for description of parameters and examples.
</remarks>


  <SeeAlso cref="RemezFirLength"/>
  <SeeAlso cref="remez"/>*)
function RemezLength(const bands, gains, ripple: array of Double; Weights: TVec; FS: Double = 2): integer;

(*<summary>Grid densitiy for the Parks-McClellan algorithm.</summary>
  
<remarks>Defines the density of the frequency grid on which to perform the optimization of
  the equiripple FIR filter with the remez routine. Increasing the density of the grid
  might improve filter design.
</remarks>
*)

var GridDensity: integer = 16;



