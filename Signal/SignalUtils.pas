











(*<summary>General purpose signal processing routines.</summary>*)
unit SignalUtils;


interface

{$I BdsppDefs.inc}

    
      
          {$DEFINE DOWAVELETS}
      
    

uses AbstractMtxVec, MtxVec, MtxVecInt, Math387,Polynoms, MtxVecBase
    

    
      ,ippspl
    

    
      ,ippsplSingle
    



    
    ,SysUtils
    


    ;

const

(*<summary>Maximum number of taps for FIR filters.</summary>
  
<remarks>Defines the maximum number of taps returned by FIR length estimation routines.
</remarks>


  <SeeAlso cref="OptimalFir.RemezLength"/>
  <SeeAlso cref="OptimalFir.RemezFirLength"/>
  <SeeAlso cref="KaiserFirLength"/>*)
  MaxFirLength = 10000;

  (*<summary>Product information string.</summary>*)
  DspForMtxVec: string = 'DSP Master v6.3.5';

type

      (*<summary> Defines signal monitoring type. </summary>
                   
<remarks>When there is a need to monitor most recent data
                   written use mmFromEnd. To monitor most recent
                   data read, use mmFromStart. Typically when when we
                   are recording, we want to monitor most recent data
                   written and when there is playback we want to see
                   the oldest data from the buffer just to be played.
</remarks>
*)

      TMonitorMode = (
      (*<summary>The data which is monitored from the buffer is the oldest and to be read next.</summary>*)
      mmFromStart,
      (*<summary>The data which is monitored from the buffer is the newest and to exit the buffer as the last.</summary>*)
      mmFromEnd);


    (*<summary>Defines FIR filter types.</summary>
      
<remarks>Not all routines accepting TFilterType variable can design
      all filter types. All IIR filter design routines are limited to
      ftLowpass, ftBandpass, ftHighpass and ftBandstop filter types.
</remarks>


      <SeeAlso cref="KaiserImpulse"/>
      <SeeAlso cref="FirImpulse"/>*)
      TFilterType = (
      (*<summary>design a lowpass filter.</summary>*)ftLowpass,
      (*<summary>design a highpass filter.</summary>*)ftHighpass,
      (*<summary>design bandpass filter</summary>*)ftBandpass,
      (*<summary>design a bandstop filter</summary>*)ftBandstop,
      (*<summary>design a type III hilbert transformer.</summary>*)ftHilbertIII,
      (*<summary>design a type IV hilbert transformer.</summary>*)ftHilbertIV,
      (*<summary>design a type III differentiator.</summary>*)ftDifferentiatorIII,
      (*<summary>design a type IV differentiator.</summary>*)ftDifferentiatorIV,
      (*<summary>design a type III differentiator.</summary>*)ftDoubleDifferentiatorIII,
      (*<summary>design a type IV differentiator.</summary>*)ftDoubleDifferentiatorIV,
      (*<summary>design a type III integrator.</summary>*)ftIntegratorIII,
      (*<summary>design a type IV integrator.</summary>*)ftIntegratorIV,
      (*<summary>design a type III integrator.</summary>*)ftDoubleIntegratorIII,
      (*<summary>design a type IV integrator.</summary>*)ftDoubleIntegratorIV
      );

      TMusicalNotePitch = (
      (*<summary>C Note</summary>*) mnC,
      (*<summary>C# Note</summary>*)mnCSharp,
      (*<summary>D Note</summary>*) mnD,
      (*<summary>D# Note</summary>*)mnDSharp,
      (*<summary>E Note</summary>*)mnE,
      (*<summary>F Note</summary>*)mnF,
      (*<summary>F# Note</summary>*)mnFSharp,
      (*<summary>G Note</summary>*)mnG,
      (*<summary>G# Note</summary>*)mnGSharp,
      (*<summary>A Note</summary>*)mnA,
      (*<summary>A# Note</summary>*)mnASharp,
      (*<summary>B Note</summary>*)mnB);

      TGoertzPhaseCorrection = (
      (*<summary>Simple DFT</summary>*) gpcNone,
      (*<summary>Bonzanigo phase correction</summary>*) gpcBonzanigo);

      

       TDelayFilterState = record
                           SampleCount: integer;
                           Data: TVec;
                           end;

       

       



 (*<summary>Filter state used by FIR filtering methods.</summary>
        
<remarks>FIRState is the filter state defined by Intel IPP.
        UpSample stores the upsampling factor
        and DownSample stores the downsampling
        factor of a multi-rate filter. ComplexImpulse
        is True, if FIR filter has a complex impulse response.
        This state is initialized by FirInit method.
</remarks>


        <SeeAlso cref="FirFilter"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="SavGolayImpulse"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>
        <SeeAlso cref="OptimalFir.remez"/>*)
      TFirState = record
          
          State_Re: PAPointer;
          State_Im: PAPointer;
          Statec: PAPointer;
          

          

          

          UpSample: integer;
          DownSample: integer;
          ComplexImpulse: boolean;
          Initialized: boolean;
          IsDouble: boolean;
          FloatPrecision: TMtxFloatPrecision;
      end;

      (*<summary>Filter state used by sample-and-hold filtering routines.</summary>
        
<remarks>The value parameter stores the value to be held and the hold parameter specifies the time-out.
</remarks>


        <SeeAlso cref="SampleAndHoldFilter"/>
        <SeeAlso cref="SampleAndDecayFilter"/>*)
      TSampleAndHoldState = record
          Value: TCplx;
          Hold: integer;
          end;

       (*<summary>Type used by circular buffering.</summary>
         
<remarks>Record used to hold the state for circular buffering.
         WritePosition is the current write position index in the buffer
         and the ReadPosition is the current read position index.
         BufferSize is initialized with a call to InitCircularBuffer.
         BufferOverrun flag is set by WriteToCircularBuffer routine, if
         unread data is overwritten. BufferUnderrun flag is set by
         the ReadFromCircularBuffer routine, if the data read was
         already overwritten before it was read for the first time.
</remarks>


        <SeeAlso cref="InitCircularBuffer"/>
        <SeeAlso cref="WriteToCircularBuffer"/>
        <SeeAlso cref="ReadFromCircularBuffer"/>*)
      TCircularBufferState = record
          ReadPosition: integer;
          BufferSize: integer;
          DataLen: integer;
          IsBufferOverrun: boolean;
          IsBufferUnderrun: boolean;
          IncrementStep: double;
          end;

      TCztState = class
      protected
          srcLen: integer;
          srcFloatPrecision: TMtxFloatPrecision;
          k: integer;
          FStart: double;
          FStop: double;
          FS: double;
          RStart: double;
          RStop: double;
          av: TVec;
          bv: TVec;
          dv: TVec;
          b2: TVec;
      public
          constructor Create;
          destructor Destroy; override;
      end;

      (*<summary>Type used by median filter.</summary>
         
<remarks>The record type is used to hold the configuration and the delay
          line of the median filter between consecutive calls to the MedianFilter method.
          The record is initialized when passed to the MedianInit method.
</remarks>


        <SeeAlso cref="MedianInit"/>
        <SeeAlso cref="MedianFilter"/>*)

      TMedianState = record
          State: PAPointer;
          FloatPrecision: TMtxFloatPrecision;
          MaskSize: integer;
          Data: TVec;
          Buffer: TVec;
          end;

    (*<summary>Stereo channel selection.</summary>
      
<remarks>Select one of the two channels, right or left.
      The type is used by components dealing with stereo audio signals.
</remarks>
*)
    TChannel = (
    (*<summary></summary>*)chLeft,
    (*<summary></summary>*)chRight,
    (*<summary></summary>*)chThird,
    (*<summary></summary>*)chFourth,
    (*<summary></summary>*)chFifth,
    (*<summary></summary>*)chSixth
    );

     (*<summary>Defines wavelet types.</summary>
        
<remarks>Some of the wavelets require aditional parameters.
        The par1 and par2 arguments have the following meaning:<para/>

        (Reference: Intel Signal Processing library v4.5, Manual, 10-4)
</remarks>
*)
    TWaveletType = (
    (*<summary>par1 and par2 are dummies.</summary>*)
    wtHaar,
    (*<summary>par1 is the order of the wavelet (1 to 10).</summary>*)
    wtDaublet,
    (*<summary>par1 is the order of the wavelet (1 to 7).</summary>*)
    wtSymmlet,
    (*<summary>par1 is the filter length parameter (1 to 5).</summary>*)
    wtCoiflet,
    (*<summary>par1 and par2 are dummies.</summary>*)
    wtVaidyanathan,
    (*<summary>admissable combinations: <para/>
           (1,1), (1,3), (1,5)  for box splines; <para/>
           (2,2), (2,4), (2,6), (2,8) for linear splines; <para/>
           (3,1), (3,3), (3,5), (3,7), (3,9) for quadratic splines.</summary>*)
    wtBSpline,
    (*<summary>admissable combinations: <para/>
           (1,1), (1,3), (1,5)  for box splines; <para/>
           (2,2), (2,4), (2,6), (2,8) for linear splines; <para/>
           (3,1), (3,3), (3,5), (3,7), (3,9) for quadratic splines.</summary>*)
    wtBSplineDual,
    (*<summary>par1 and par2 are dummies.</summary>*)
    wtLinSpline,
    (*<summary>par1 and par2 are dummies.</summary>*)
    wtQuadSpline,
    (*<summary></summary>*)
    wtByFilter
    );

    (*<summary>Defines the result type of wavelet decomposition.</summary>*)
    TWaveletDecomp = (
    (*<summary>the result will be the "approximation".</summary>*)wtApproximation,
    (*<summary>the result will be the "detail".</summary>*)wtDetail);

     (*<summary>Window function definition.
        Specifies window function type to be applied to the signal prior to be passed to the FFT algorithm.

        <SeeAlso cref="SignalWindow"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="FrequencyResponse"/></summary>*)
    TSignalWindowType = (
    (*<summary>apply no windowing.</summary>*)
    wtRectangular,
    (*<summary>hanning window.</summary>*)
    wtHanning,
    (*<summary>hamming window.</summary>*)
    wtHamming,
    (*<summary>flat-top window.</summary>*)
    wtFlatTop,
    (*<summary>Bartlet window.</summary>*)
    wtBartlett,
    (*<summary>Blackman window.</summary>*)
    wtBlackman,
    (*<summary>blackman-harris window.</summary>*)
    wtBlackmanHarris,
    (*<summary>exact Blackman window.</summary>*)
    wtBlackmanExact,
    (*<summary>cosine tappered window.</summary>*)
    wtCosineTappered,
    (*<summary>Kaiser window.</summary>*)
    wtKaiser,
    (*<summary>Chebyshev window.</summary>*)
    wtChebyshev,
    (*<summary>Exponent-down window.</summary>*)
    wtExponent
    );

    (*<summary>Window function definition.</summary>*)
    TSignalWindowMode = (
    (*<summary>The initial and last zero element are left out, if present. This
      is required for windowed digital filter design. The window is symmetric.</summary>*)
    wmSymmetric,
    (*<summary>The initial element is kept and the last is left out.
        This is required to retain periodicity of the signal windowed and analyzed
        with frequency analysis (FFT) for the following window types:

        Hanning, Hamming, FlatTop and variants of the Blackman window.</summary>*)
    wmPeriodic);

      (*<summary>Defines frequency spectrum types.</summary>

        <SeeAlso cref="RmsOfSpectrum"/>*)
      TSpectrumType = (
      (*<summary>amplitude spectrum.</summary>*)spAmplt,
      (*<summary>RMS spectrum.</summary>*)spRMS,
      (*<summary>Power spectrum.</summary>*)spPower,
      (*<summary>Peak-to-Peak spectrum.</summary>*)spPeakPeak,
      (*<summary>Real cepstrum.</summary>*) spRealCepstrum,
      (*<summary>Complex spectrum.</summary>*)spComplex
      );


      (*<summary>Set of predefined FIR allpass filters suitable for fractional delay filtering.
                 Filter specifications have been relaxed to allow variation of the amplitude
                 in the stop band between 0 and 1 linear scale, but keeping the ripple specification in the
                 passband. When unclear what to select, falp_60dB is recommended. </summary>*)
      TFractionalImpulse = (
      (*<summary>Passband ripple -160dB, cutoff at 0.82, integer delay of 30 samples.</summary>*)
      falp_160dB,
      (*<summary>Passband ripple -140dB, cutoff at 0.85, integer delay of 30 samples.</summary>*)
        falp_140dB,
      (*<summary>Passband ripple -140dB, cutoff at 0.94, integer delay of 80 samples.</summary>*)
        falp_140dB2,
      (*<summary>Passband ripple -100dB, cutoff at 0.9, integer delay of 30 samples</summary>*)
        falp_100dB,
      (*<summary>Passband ripple -60dB, cutoff at 0.93, integer delay of 30 samples.</summary>*)
        falp_60dB,
      (*<summary>Passband ripple -50dB, cutoff at 0.82, integer delay of 7 samples.</summary>*)
        falp_50dB,
      (*<summary>Passband ripple -50dB, cutoff at 0.6, integer delay of 3 samples.</summary>*)
        falp_50dB2);

      (*<summary>Filter state type used by ConsistentParameter filter method.</summary>
        
<remarks>Filter state stores the parameter (Param) to be tracked and
        the total Time, within which this parameter has not changed its value.
</remarks>


        <SeeAlso cref="ConsistentParameterInit"/>
        <SeeAlso cref="ConsistentParameterFilter"/>*)
      TConsistentParam = record
          Param: double;
          Time: TDateTime;
          end;

       

       

     (*<summary>Filter state used by IIR filtering methods.</summary>
       
<remarks>NSPIirState is the filter state defined by Intel IPP.
       ComplexImpulse is True, if Iir filter has a complex impulse response.
       This state is initialized by IirInit method.
</remarks>


        <SeeAlso cref="IIRFilters.ButterFilter"/>
        <SeeAlso cref="IIRFilters.ChebyshevIFilter"/>
        <SeeAlso cref="IIRFilters.ChebyshevIIFilter"/>
        <SeeAlso cref="IIRFilters.EllipticFilter"/>
        <SeeAlso cref="DcFilter"/>
        <SeeAlso cref="NotchFilter"/>*)
      TIirState = record
          
          NSPIirState: PAPointer;
          NSPIirStatec: PAPointer;
          Buffer: Math387.TByteArray;
          Bufferc: Math387.TByteArray;
          

          

          

          IsDouble: boolean;
          ComplexImpulse: boolean;
          Initialized: boolean;
      end;

      (*<summary>Type used for sine function generation.</summary>
        
<remarks>Type required for tone (sine) signal generation.
        Complex is True, if the resulting sine wave should
        be complex.
</remarks>


        <SeeAlso cref="ToneInit"/>
        <SeeAlso cref="Tone"/>*)
      TToneState = record
          Freq: double;
          Mag: double;
          Phase: double;
          Complex: boolean;
          Negative: boolean;
          EdgeFreq: boolean;
          Initialized: boolean;
      end;

      (*<summary>Type used for triangle tone generation.</summary>
        
<remarks>Type required for (triangle) signal generation.
        Complex is True, if the resulting sine wave should
        be complex.
</remarks>


        <SeeAlso cref="TriangleToneInit"/>
        <SeeAlso cref="TriangleTone"/>*)
      TTriangleState = record
          Freq: double;
          Mag: double;
          Phase: double;
          Asym: double;
          Complex: boolean;
          Initialized: boolean;
      end;

      TToneStateArray = array of TToneState;

      (*<summary>Type used for square signal function generation.</summary>
        
<remarks>Type required for square signal generation.
        Complex is True, if the resulting sine wave should be complex.
</remarks>


        <SeeAlso cref="SquareToneInit"/>
        <SeeAlso cref="SquareTone"/>*)

      TSquareToneState = record
          Freq: double;
          Mag: double;
          Phase: double;
          Initialized: boolean;
          Tones: TToneStateArray;
      end;

      (*<summary>Type used by the PhaseUnwrap procedure.</summary>

      <SeeAlso cref="PhaseUnwrap"/>
      <SeeAlso cref="CplxCepstrum"/>
      <SeeAlso cref="CplxCepstrumInv"/>*)
      TRunningPhase = (
      (*<summary>The phase will be unwrapped and no further processing will be applied.</summary>*)
      rpDefault,
      (*<summary>The phase will be unwrapped and, if actual amount of zero padding is known, the
        resulting phase spectrum will be showing the exact phase relations between frequencies with the
        first value of the phase spectrum having zero phase.
        This type of a phase spectrum can be used to compute the delay
        in ms separately for each frequency.</summary>*)
      rpConstPhase,
      (*<summary>Integer lag will be subtracted, but the phase will not be unwrapped.</summary>*)
      rpIntegerLag,
      (*<summary>The phase will be unwrapped and the resulting phase spectrum will be showing the approximate
        phase relations between frequencies with the first value
        and the value at FS/2 (at Pi radians) having zero phase.</summary>*)
      rpPartial);

      (*<summary>Type used by the numerical differentiation routine.</summary>
        
<remarks>The type stores the initial conditions for the numerical
        differentiation. Start2R and Start2I are real and imaginary
        part of the first condition (x[-2]) and Start1R, Start1I are the
        real and imaginary part of the second condition (x[-1]).
</remarks>


        <SeeAlso cref="Differentiate"/>*)
      TDiffState = record
        Start2R,
        Start2I,
        Start1R,
        Start1I: double;
        end;

      (*<summary>Type used by the numerical integration routine.</summary>
        
<remarks>The type stores the initial conditions for the numerical
        integration. StartR2 and StartI2 are real and imaginary
        part of the first condition (x[-2]) and StartR1, StartI1 are the
        real and imaginary part of the second condition (x[-1]).
</remarks>


        <SeeAlso cref="Integrate"/>*)
       TIntegrateState = record
          SumR1, SumI1,
          StartR1, StartI1,
          StartR2, StartI2: double;
        end;




  (*<summary>Bartlett window.</summary>
          
<remarks>Applies Bartlett window to Src vector.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution. Bartlett window
          is a "triangular" window defined as [1] p. 248:
    <code>
               2*n              M - 1
    w[n] =  -------, 0 &lt;= n &lt;= ------
             M - 1               2

                 2*n     M - 1
    w[n] = 2 - -------, ------ &lt;= n &lt;=  M - 1
                M - 1      2

    w[n] = 0, for other n's
    </code>
    <b>References: </b>  <para/>
    [1] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000.
</remarks>


    

    <example>
     Compute the frequency response of a lowpass filter with a cutoff at 40 Hz, if the sampling
     frequency is 200Hz and the filter is designed with the bartlett window.
    <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
        Vector h = new Vector(100);
        Vector Response = new Vector(0);
        Vector FreqFr = new Vector(0);

        SignalUtils.FirImpulse(h, new double[1] {40},0, TFilterType.ftLowpass, TSignalWindowType.wtRectangular,1,200);
        SignalUtils.Bartlett(h);  //window the sinc impulse response
        SignalUtils.FrequencyResponse(h,null,Response,8,false,
                                      TSignalWindowType.wtRectangular, 0);
        FreqFr.Size(Response.Length);
        FreqFr.Ramp(0,200*0.5/Response.Length);
        MtxVecTee.DrawIt(FreqFr, Response,"Frequency response",false);
    }
    </code></example>

      <SeeAlso cref="Hanning"/>
      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="SignalWindow"/>*)
       function Bartlett(const Src: TVec): TVec; overload;
      (*<summary>Bartlett window.</summary>
          
<remarks>Applies Bartlett window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function Bartlett(const Src: TVec; Index: integer; Len: integer): TVec; overload;

       (*<summary>Blackman window.</summary>
          
<remarks>Applies Blackman window with alfa parameter to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution. The window is defined
          as [1] p. 6-7:
    <code>
            alpha + 1            2*Pi*n       alpha        4*Pi*n
    w[n] = ---------- - 0.5*cos --------  -  ------ * cos(-------)
               2                 n - 1          2          n - 1

    alpha = -0.16 (standard window)
    alpha = - 0.25 (asymptotic optimal window for large n)
    </code>

    <c>w[n] =  0.42 - 0.5*cos(2*Pi*n/(M-1)) + 0.08*cos(4*Pi*n/(M-1))</c><para/>
    <c>0 &lt;= n &lt;= M - 1</c>

    <b>References: </b>  <para/>
    [1] Intel IPP SPL v5.3 manual
</remarks>


          

    <example>
    Compute the frequency response of a lowpass
    filter with a cutoff at 40 Hz, if the sampling
    frequency is 200Hz and the filter is designed with the blackman window.

    <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
        Vector h = new Vector(100);
        Vector Response = new Vector(0);
        Vector FreqFr = new Vector(0);

        SignalUtils.FirImpulse(h, new double[1] {40},0, TFilterType.ftLowpass, TSignalWindowType.wtRectangular,1,200);
        SignalUtils.Blackman(h,0.1,TSignalWindowMode.wmSymmetric);  //window the sinc impulse response
        SignalUtils.FrequencyResponse(h,null,Response,8,false,
                                      TSignalWindowType.wtRectangular, 0);
        FreqFr.Size(Response.Length);
        FreqFr.Ramp(0,200*0.5/Response.Length);
        MtxVecTee.DrawIt(FreqFr, Response,"Frequency response",false);
    }
    </code></example>

      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="SignalWindow"/>*)
       function Blackman(const Src: TVec; alfa: double; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Blackman window.</summary>
          
<remarks>Applies Blackman window to Src vector from element at
          [Index] to element at [Index + Len - 1].
</remarks>
*)
       function Blackman(const Src: TVec; alfa: double; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;

  (*<summary>Exact blackman window.</summary>
          
<remarks>Applies exact Blackman window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.

    <c>w[n] := 0.42659071 - 0.49656062*cos(2*Pi*n/(n-1)) + 0.07684867*cos(4*Pi*n/(n-1))</c><para/>
    <c>0 &lt;= n &lt;= M - 1</c>
</remarks>


    

    <example>
    Reduce spectral leakage of the FFT by first applying exact Blackman
    window to the the signal.
    <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
        Vector h = MtxExpr.Sin(MtxExpr.Ramp(256, TMtxFloatPrecision.mvDouble,0, 2 * Math387.PI * 0.1));
        Vector h1 = new Vector(0);
        Vector Response = new Vector(0);
        Vector Response1 = new Vector(0);

        h1.Copy(h);
        SignalUtils.BlackmanExact(h, TSignalWindowMode.wmSymmetric);  //window the sinc impulse response
        MtxVecTee.DrawIt(h,"",false);

        h = h * 2.344168; //scale the signal to compensate for energy loss
        SignalUtils.FrequencyResponse(h1,null, Response,8,false, TSignalWindowType.wtRectangular, 0);
        SignalUtils.FrequencyResponse(h, null, Response1, 8, false, TSignalWindowType.wtRectangular, 0);

        MtxVecTee.DrawIt(new TVec[2] { Response, Response1 }, new string[2] { "Rectangular window", "Exact blackman" }, "Frequency response", false);
    }
    </code></example>

        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="SignalWindow"/>*)
       function BlackmanExact(const Src: TVec; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Exact Blackman window.</summary>
          
<remarks>Applies Exact Blackman window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function BlackmanExact(const Src: TVec; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;

      (*<summary>Blackman-Harris window.</summary>
          
<remarks>Applies BlackmanHarris window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.

      <c>w[n] := 0.42323 - 0.49755*cos(2*Pi*n/(n-1)) + 0.07922*cos(4*Pi*n/(n-1))</c><para/>
      <c>0 &lt;= n &lt;= M - 1</c>
</remarks>


      <SeeAlso cref="BlackmanExact"/>
      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="SignalWindow"/>*)
       function BlackmanHarris(const Src: TVec; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Blackman Harris window.</summary>
          
<remarks>Applies exact Blackman Harris window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function BlackmanHarris(const Src: TVec; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;
       (*<summary>Optimal Blackman window.</summary>
          
<remarks>Applies optimal Blackman window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution. The following
          function defines the Blackman window:

      <code>

              alpha + 1            2*Pi*n       alpha        4*Pi*n
      w[n] = ---------- - 0.5*cos --------  -  ------ * cos(-------)
                 2                 n - 1          2          n - 1

                  sin(Pi/(n-1))
      alpha = - (---------------)^2
                 sin(2*Pi/(n-1))
      </code>
</remarks>


        <SeeAlso cref="Blackman"/>
        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="SignalWindow"/>*)
       function BlackmanOptimal(const Src: TVec; Index: integer = 0; Len: integer = -1): TVec; overload;

       (*<summary>Chebyshev's window.</summary>
          
<remarks>Applies Chebyshevs equiripple window to Src.
          The attenuation of the spectral leakage is defined with the AttdB parameter
          in [dB].
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.
</remarks>

        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="SignalWindow"/>*)
       function Chebyshev(const Src: TVec; AttdB: double; Index: integer = 0; Len: integer = -1): TVec; overload;
       (*<summary>Cosine tappered window.</summary>
          
<remarks>Applies Cosine tappered window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.
          When percent parameter is 100, the CosineTappered window
          becomes equal to the hanning window. When the Percent
          parameter is 0, then the window remains rectangular
          (box car).
</remarks>


        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="SignalWindow"/>*)
       function CosineTappered(const Src: TVec; Percent: double; Index: integer = 0; Len: integer = -1): TVec; overload;


       (*<summary>Exponential window.</summary>
          
<remarks>Applies Exponential window to Src.
          Exponential window is used with impulse responses to attenuate noise.
          With impulse response, the noise is present throughout the signal
          and its average value is higher then the value of the signal
          because the signal has a much shorther duration. Because the noise is broadband,
          the modal frequency might not show up in the frequency spectrum
          or it would be very small, if the
          signal would not be windowed with an exponential window.
</remarks>


          <SeeAlso cref="Kaiser"/>
          <SeeAlso cref="SignalWindow"/>*)
       function ExponentWindow(const Src: TVec; Att: double; Index: integer = 0; Len: integer = -1): TVec; overload;

       (*<summary>Flat-top window.</summary>
          
<remarks>Applies Flat top window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.
          Flat top window was used to get more accurate amplitude information
          when frequency interpolation routines where not yet common.

        <c>w[i] = 0.2810639 - 0.5208972*cos(2*Pi*i/(n-1)) + 0.1980399*cos(4*Pi*i/(n-1)))</c><para/>
        <c>0 &lt; i &lt; n-1</c>
</remarks>


        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="SignalWindow"/>*)
       function FlatTop(const Src: TVec; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Flat top window.</summary>
          
<remarks>Applies Flat top window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function FlatTop(const Src: TVec; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;

       (*<summary>Hamming window.</summary>
          
<remarks>Applies Hamming window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.
          Hamming window can be found in [1] p. 249
    <code>
    w[i] = 0.54 - 0.46*cos(2*Pi*i/(n-1))

    0 &lt;= i &lt;= n-1

    </code>
<b>References: </b>  <para/>
[1] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000.
</remarks>


      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="SignalWindow"/>*)
       function Hamming(const Src: TVec; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Hamming window.</summary>
          
<remarks>Applies Hamming window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function Hamming(const Src: TVec; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;

       (*<summary>Hanning window.</summary>
          
<remarks>Applies Hanning window to Src.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.
          Hanning window is very widely used and sufficient for most applications.
          The equation for the Hanning window can be found in [1] p. 249:

      <c>w[i] = 0.5*(1-cos(2*Pi*i/(n-1))</c><para/>
      <c>0 &lt; i &lt; n-1</c>

      <b>References: </b>  <para/>
      [1] Digital signal processing, Vinay K. Ingle and John G. Proakis, Brooks-Cole, 2000.
</remarks>


      <SeeAlso cref="CosineTappered"/>
      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="SignalWindow"/>
      <SeeAlso cref="Hamming"/>*)
       function Hanning(const Src: TVec; WindowMode: TSignalWindowMode): TVec; overload;
      (*<summary>Hamming window.</summary>
          
<remarks>Applies Hamming window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function Hanning(const Src: TVec; WindowMode: TSignalWindowMode; Index: integer; Len: integer): TVec; overload;

       (*<summary>Kaiser window.</summary>
          
<remarks>Applies Kaiser window to Src. Beta parameter
          controls the width the of the mainlobe and spectral
          leakage. Beta allows you to find a compromise between
          the frequency resolution and attenuation of the spectral leakage.
          Window functions are applied to the signal prior to conversion
          to frequency domain with the FFT algorithm, to reduce the spectral leakage.
          Their side-effect is a lower frequency resolution.

          If a specified sidelobe attenuation is required for spectrum analysis,
          the appropriate beta can be found with the
          <see cref="KaiserBetaWindow"/> routine.

          If a specified sidelobe attenuation is required for a FIR filter,
          the appropriate beta can be found with the
          <see cref="KaiserBetaFir"/> routine.

          The equation for Kaiser window and application to the FIR filter
          design can be found in [1] p. 453.

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>

        
        <SeeAlso cref="Hanning"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="SignalWindow"/>*)
       function Kaiser(const Src: TVec; Beta: double): TVec; overload;
       (*<summary>Kaiser window.</summary>
          
<remarks>Applies Kaiser window to Src vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       function Kaiser(const Src: TVec; Beta: double; Index: integer; Len: integer): TVec; overload;

       (*<summary>Fractional Kaiser window.</summary>
          
<remarks>Additional parameters in compare to the <see cref="Kaiser"/> function,
          are the Offset and Step. They allow the windowing function to be sampled
          at the same points as <see cref="FractionalFirImpulse"/>. This function
          should therefore be called when windowing an impulse response generated with
          FractionalFirImpulse.
</remarks>
*)
       function FractionalKaiser(const Src: TVec; Beta: double; Offset, Step: double): TVec; overload;
       (*<summary>Fractional Kaiser window.</summary>
           
<remarks>Applies fractional Kaiser window to Src vector from element at
           Index to element at Index + Len - 1.
</remarks>
*)
       function FractionalKaiser(const Src: TVec; Beta: double; Offset, Step: double; Index, Len: integer): TVec; overload;

       (*<summary>Convert a TSignalWindowType to a string.</summary>
         
<remarks>The function returns a description of a TSignalWindowType specified
         by the  WindowType variable.
</remarks>


        <SeeAlso cref="SignalWindow"/>*)
       function SignalWindowToString(WindowType: TSignalWindowType): string;

       (*<summary>Apply a time window function to the signal.</summary>
          
<remarks>Apply a window to data in Vec. WindowParam should contain the parameter,
          required by the window. If the window does not have a parameter, windowParam
          can have any value. Set ScaleNorm to true, to request scaling
          of Vec such, that the energy of the stationary signal will be
          preserved after windowing.
</remarks>


        <SeeAlso cref="Hanning"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="SignalWindow"/>*)
       procedure SignalWindow(const Vec: TVec; WindowType: TSignalWindowType; WindowParam: double;
                              WindowMode: TSignalWindowMode; ScaleNorm: boolean = false); overload;
       (*<summary>AApply a time window function to the signal.</summary>
          
<remarks>Applies user window to Vec vector from element at
          Index to element at Index + Len - 1.
</remarks>
*)
       procedure SignalWindow(const Vec: TVec; WindowType: TSignalWindowType; WindowParam: double;
                              WindowMode: TSignalWindowMode; ScaleNorm: boolean; Index: integer; Len: integer); overload;

  

       (*<summary>Applies hilbert transform to Src.</summary>
         
<remarks>Applies hilbert transform to Src. Src must be real signal.
         The result is complex. Hilbert transform generates a 90 degree
         phase shifted version of the original. This becomes the imaginary
         part of the complex signal. This routine is very usefull for single
         block processing, but can not be used for streaming data. Use
         a digital FIR filter based hilbert transformer for streaming data
         or resort to quadrature sampling techniques [1], p. 297.

<b>References: </b>  <para/>
[1] Understanding digital signal processing. Richard G. Lyons, Prentice-Hall, 2001.
</remarks>


    

    <example>
    Hilbert transform of a sine signal is computed for two cases:
     * frequency of the sine falls exactly on the spectral bin (ideal case)
     * frequency of the sine is not alligned with the frequency spectrum grid.
    <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Math.Editors;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
        Vector h = MtxExpr.Sin(MtxExpr.Ramp(256, TMtxFloatPrecision.mvDouble, 0, 2 * Math387.PI * 6/256));
        Vector Re = new Vector(0);
        Vector Im = new Vector(0);
        Vector h1 = new Vector(0);

        SignalUtils.Hilbert(h);
        h.CplxToReal(Re, Im);
        MtxVecTee.DrawIt(new TVec[2] {Re,Im}, new string[2] {"Real","Imag"} ,"Integer frequency",false);
        h1.SetIt(false,new double[1] {Re.DotProd(Im)});
        MtxVecEdit.ViewValues(h1,"Dot product between Re and Im",true);

        h = MtxExpr.Sin(MtxExpr.Ramp(256, TMtxFloatPrecision.mvDouble,0, 2 * Math387.PI * 6.5/256));
        SignalUtils.Hilbert(h);
        h.CplxToReal(Re, Im);
        MtxVecTee.DrawIt(new TVec[2] {Re,Im}, new string[2] {"Real","Imag"} ,"Non-integer frequency",false);
        h1.SetIt(false,new double[1] {Re.DotProd(Im)});
        MtxVecEdit.ViewValues(h1,"Dot product between Re and Im",true);

        h = MtxExpr.Sin(MtxExpr.Ramp(256, TMtxFloatPrecision.mvDouble, 0, 2 * Math387.PI * 6.5/256));
        Re.Copy(h);
        Im.Copy(h);
        SignalUtils.KaiserImpulse(h1,new double[2] {0.95,1}, 0.01, TFilterType.ftHilbertIII,1,2,true);
        //Or use remez:    OptimalFIR.RemezImpulse(h1,new double[2] {0.05,0.95}, 0.01, TFilterType.ftHilbertIII);
        SignalUtils.FirFilter(Im,h1,1,1);  //also compensates for integer filter delay (if filter is Odd length (type III))
        MtxVecTee.DrawIt(new TVec[2] {Re,Im}, new string[2] {"Real","Imag"} ,"With FIR Filter",false);
        h1.SetIt(false,new double[1] {Re.DotProd(Im)});  //dot product between Re and Im should be zero
        MtxVecEdit.ViewValues(h1, "Dot product between Re and Im", true);
    }
    </code></example>

        <SeeAlso cref="OptimalFir.remez"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
      function Hilbert(const SrcDst: TVec): TVec; overload;

    (*<summary>Differentiate the signal.</summary>
         
<remarks>Differentiate the signal Src and place the result in Dst,
         The routine can also be used for streaming data, if the
         value of the State parameter is preserved between consecutive calls.
         dT defines the sampling period. Src can be complex or real.
         For setting the initial conditions to other then 0 see the
         TDiffState description.

         The following formula is used for numeric differentiation:

    <code>
    y[i] = (x[i] - x[i-2]) / (2 * dT)

    dT ... sampling period
    x - input
    y - output
    </code>

    The filter does not preserve linear phase, but it is
    more accurate than:

    <c>y[i] = (x[i]-x[i-1])/dT</c><para/>

    which can be rewriten as:

    <c>y[i] = ((x[i] - x[i-1]) +  (x[i-1] - x[i-2]))/2 * 1/dT</c>

    To apply a differentiator preserving linear phase see the
    <see cref="OptimalFir.RemezImpulse"/> and <see cref="KaiserImpulse"/> routines.

    Note
      Record types as well as all other variables are automatically initialized to zero,
      if they are declared as fields of an object. The state variable of the Differentiate
      routine has to be initialized before it can be used, if declared within a routine.
</remarks>


      <SeeAlso cref="Integrate"/>
      <SeeAlso cref="OptimalFir.RemezImpulse"/>
      <SeeAlso cref="KaiserImpulse"/>
      <SeeAlso cref="FirImpulse"/>*)

       function Differentiate(const Src, Dst: TVec; var State: TDiffState; Dt: double): TVec; overload;

       (*<summary>Integrate signal.</summary>
         
<remarks>Use Simpson's formula to integrate the Src and place the result in Dst.
         Src can be real or complex. dT defines the sampling period (dT = 1/FS).
         The State variable holds the initial conditions.

    <code>
                    j
    Dst[j]  = 1/6* Sum ( Src[i-2] + Src[i-1]*4 + Src[i])*dT)
                   i=0

    j = 0,1...n-1
    </code>

         The integrate routine does not preserve linear phase.
         An analytical solution for a linear phase integrator does not exists.
         In general there are two types of applications for integration:
         1. The signal has a more or less fixed mean value. (DC offset).
          An example of such a signal is the signal comming from the accelerometer
          measuring vibrations.
          The numerical integration will work well on signals whose mean value
          is exactly zero (otherwise it will rise or fall in infinity).
          Because this is often not the case, the signal
          must be passed through a DC filter first. The DC filter can be IIR
          or FIR type. An alternative to numerical integration and the Integrate routine
          is a linear phase integration filter designed with the
          <see cref="OptimalFir.RemezImpulse"/> routine.
          Linear phase integrator also removes the DC offset.
         2. The signal does not have a mean value.
          An example of such a signal is the signal comming from the accelerometer
          measuring the acceleration and deceleration of a driving car.
          In this case the Integrator routine can be used directly to obtain
          speed and/or distance from the acceleration data.
</remarks>


  

    <example>
    Single block and streaming application example:
  <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Signal;
    using Dew.Signal.Units;
    using Dew.Math.Tee;
    using Dew.Math.Editors;
    using Dew.Signal.Tee;

    private void button1_Click(object sender, EventArgs e)
    {
        int n,i;
        double FS = 1; //sampling frequency
        TIntegrateState State2 = new TIntegrateState();
        TDiffState State = new TDiffState();

        Vector h = MtxExpr.Ramp(30, TMtxFloatPrecision.mvDouble, 0, 1);
        Vector b = new Vector(h.Length);
        Vector c = new Vector(h.Length);

    //single block

        SignalUtils.Integrate(h, b, ref State2, 1.0/FS);
        SignalUtils.Differentiate(b, c, ref State, 1.0 / FS);
        MtxVecTee.DrawIt(c,"Processed in one block",false);

    //streaming

        //reset initial conditions
        State2 = new TIntegrateState();
        State = new TDiffState();

        n = h.Length / 10; //integer division (!)
        for (i = 0; i &lt; 10; i++)
        {
            h.SetSubRange(i*n,n);
            b.SetSubRange(i*n,n);
            c.SetSubRange(i*n,n);
            SignalUtils.Integrate(h,b, ref State2, 1.0/FS);

    //            Should be:  b = [0 , 1 , 3, 6, 10, 15, 21,... ]
    //            But becomes: b = [0, 0.1666, 1.1666, 3.166, 6.166, 10.166, 15.166, 21.166,... ]
    //            because of Simpson

            SignalUtils.Differentiate(b,c, ref State,1.0/FS);

    //            Should be: c = [0,1,2,3,4,5,6....]
    //            But becomes: c = [0, 0.08333, 0.5833, 1.5, 2.5, 3.5, 4.5....]

        }
        c.SetFullRange();
        MtxVecTee.DrawIt(c,"Processed per partes, but same result",false);
    }
    </code></example>

    <SeeAlso cref="Differentiate"/>
    <SeeAlso cref="DcFilter"/>
    <SeeAlso cref="OptimalFir.RemezImpulse"/>
    <SeeAlso cref="KaiserImpulse"/>
    <SeeAlso cref="FirImpulse"/>*)
       procedure Integrate(const Src, Dst: TVec; var State: TIntegrateState; Dt: double); overload;

       (*<summary>Demultiplex a channel.</summary>
         
<remarks>Dempultiplex ChannelNr from Src to Dst, if number of channels is ChannelCount.
         ChannelNr is zero based.
</remarks>


      

    <example>
    Multiplex and demultiplex test.    
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
        Vector b = new Vector(0);
        Vector c = new Vector(0);

        Vector h = MtxExpr.Ramp(6, TMtxFloatPrecision.mvDouble,0,1);     // h = [0 1 2 3 4 5]
        SignalUtils.Multiplex(h,b,3,2);  //  b = [0 0 0 0 0 1 0 0 2 0 0 3 0 0 4 0 0 5]
        SignalUtils.Demultiplex(b,c,3,2); // c = [0 1 2 3 4 5]
        MtxVecTee.DrawIt(c,"",false);
    }
    </code></example>

      <SeeAlso cref="Multiplex"/>*)
       procedure Demultiplex(const Src, Dst: TVec; ChannelCount, ChannelNr: integer); overload;
       (*<summary>Multiplex a channel.</summary>
         
<remarks>Multiplex Src as channel ChannelNr to Dst, if there is ChannelCount channels.
         ChannelNr is zero based.
</remarks>


      <SeeAlso cref="Demultiplex"/>*)
       procedure Multiplex(const Src, Dst: TVec; ChannelCount, ChannelNr: integer); overload;

       

       (*<summary>Bessel function of the first kind.</summary>
         
<remarks>Bessel function of the first kind used by the Kaiser routine.
         The description can be found in [1] p. 457.

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>
*)
       function  BesselI0(X: Double): Double; overload;
       (*<summary>Returns beta parameter for the Kaiser window FIR filter.</summary>
         
<remarks>Compute Beta parameter for the Kaiser window applied to the FIR filter,
         where the passband has specified Ripple.
         The definition can be found in [1] p. 453.

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>


        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="KaiserBetaWindow"/>*)
       function  KaiserBetaFir(Ripple: double): double; overload;
       (*<summary>Returns beta parameter for frequency analysis with the Kaiser window.</summary>
         
<remarks>Compute Beta parameter for the Kaiser window applied to the signal
         for the purpose of frequency analysis,
         where spectral leakage is limited with Ripple. The definition can be found in [1] p. 704, eq. 11.13.

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>


      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="KaiserBetaFir"/>*)
       function  KaiserBetaWindow(Ripple: double): double; overload;
    (*<summary>Returns beta parameter for frequency analysis with the Kaiser window.</summary>
         
<remarks>Compute Beta parameter for the Kaiser window applied to the signal,
         where spectral leakage is attenuated at least Att decibels.
         Attenuation is estimated from ripple as:

    <code>
    Att[dB] = -20*Log10(Ripple);

    Ripple = Exp10(Att/-20);
    </code>
</remarks>


      <SeeAlso cref="Kaiser"/>
      <SeeAlso cref="KaiserBetaWindow"/>*)
       function  KaiserBetaWindowAtt(Att: double): double; overload;
       (*<summary>Estimate the length of a windowed FIR filter. </summary>
          
<remarks>Returns the length of the FIR filter, windowed with the Kaiser window,
          where the maximum allowed ripple of the pass band is Ripple and
          sampling frequency is FS. The W array holds two parameters:
          the start and the stop of the narrowest transition band, relative
          to the specified sampling frequency.

          The equation can be found in [1] p. 453, eq. 7.93.
          The length of the filter designed with kaiser window is about 10% bigger
          then the length of the filter with the same specifications designed with the
          remez algorithm. The ripple of the passband and the stopband
          attenuation of a FIR filter designed with a Kaiser window
          are related with the equations:
   <code>
   Att[dB] = -20*Log10(Ripple);

   Ripple = Exp10(Att/-20);
   </code>
        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>
   

  

    <example>
        Design a highpass filter with at least 80 dB attenuation in
          the stopband
         and not more then 0.0001 ripple in the passband.
         Transition band is between 0.5 and 0.6 Hz.
         Sampling frequency is 2 Hz.
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
        Vector h = new Vector(0);
        Vector response = new Vector(0);

        double FS = 2;
        double Ripple = 0.0001;

        int n = SignalUtils.KaiserFirLength(new double[2] {0.5,0.6}, Ripple, FS);
        n = Math387.EnsureRange(4, n, SignalUtils.MaxFirLength);
        if (n % 2 == 0)
        {
            n++; //must be odd, if passband at FS/2
        }
        SignalUtils.FirImpulse(h.Size(n), new double[2] { 0.5, 0.6 }, TFilterType.ftHighpass, FS); //get impulse response
        SignalUtils.Kaiser(h,SignalUtils.KaiserBetaFir(Ripple)); //apply Kaiser window
        SignalUtils.FrequencyResponse(h,null,response,8,false,TSignalWindowType.wtRectangular,0); //zero padd by 8x
        MtxVecTee.DrawIt(20*MtxExpr.Log10(MtxExpr.Abs(response)),"Highpass FIR filter",false);
    }
    </code></example>

    <SeeAlso cref="OptimalFir.RemezFirLength"/>
    <SeeAlso cref="KaiserImpulse"/>
    <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       function  KaiserFirLength(W: array of double; Ripple: double; FS: double = 2): integer; overload;

        (*<summary>Design a FIR filter with rectangular window.</summary>
        
<remarks>Compute a FIR impulse response filter (no window applied) and place the result in H. The
        transition regions are defined with the W array. There must be at least one (lowpass, highpass)
        and at most two (bandpass, bandstop) transition regions (2 or 4 elements).
        Filter type is defined with TFilterType.
        The length of the filter H.Length must be preset. Filters of even length (odd order),
        must have a stop band next to the nyquist (FS/2) frequency.
        20*Log10(Ripple) is also the required attenuation of the stop band in decibel.
        FS is the sampling frequency.
</remarks>


  

    <example>
    Impulse responses of FIR filters.
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
        Vector h = new Vector(0);
        Vector response = new Vector(0);
        Vector x = new Vector(0);

        double FS = 2;
        int n = 41;

   //lowpass design with cutoff frequency
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0.5}, TFilterType.ftLowpass,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble, 0, 1.0/response.Length);
        MtxVecTee.DrawIt(x,response,"Lowpass with cutoff at 0.5",false);

    //lowpass design with transition band
        SignalUtils.FirImpulse(h.Size(n), new double[2] {0.3,0.5}, TFilterType.ftLowpass,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Lowpass with transition band: 0.3-0.5",false);

    //Highpass design with cutoff frequency
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0.5}, TFilterType.ftHighpass,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Highpass with cutoff at 0.5",false);

    //Highpass design with transition band
        SignalUtils.FirImpulse(h.Size(n), new double[2] {0.3,0.5}, TFilterType.ftHighpass,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Highpass with transition band: 0.3-0.5",false);

    //Bandpass design with transition bands
        SignalUtils.FirImpulse(h.Size(n), new double[4] {0.3,0.4,0.5,0.6}, TFilterType.ftBandpass,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Bandpass with transitoin band: 0.3-0.4, 0.5-0.6",false);

    //Bandstop design with transition bands
        SignalUtils.FirImpulse(h.Size(n), new double[4] {0.3,0.4,0.5,0.6}, TFilterType.ftBandstop,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Bandstop with transition bands: 0.3-0.4, 0.5-0.6",false);

    //Hilbert III
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0}, TFilterType.ftHilbertIII,FS);
        SignalUtils.FrequencyResponse(h,null,response,8,false,TSignalWindowType.wtRectangular,0);
        MtxVecTee.DrawIt(x,response,"Hilbert III",false);

    //Hilbert IV
        n++;  //n must be even
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0}, TFilterType.ftHilbertIV,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response,"Hilbert IV", false);

    //Differentiator III
        n--; // n must be odd
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0}, TFilterType.ftDifferentiatorIII,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response, "Differentiator III",false);

    //Differentiator IV
        n++;  //n must be even
        SignalUtils.FirImpulse(h.Size(n), new double[1] {0}, TFilterType.ftDifferentiatorIV,FS);
        SignalUtils.FrequencyResponse(h, null, response, 8, false, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(x,response, "Differentiator IV", false);
    }
    </code></example>*)
       procedure FirImpulse(const H: TVec; W: array of double; FilterType: TFilterType; FS: double = 2); overload; (*<summary>Computes just impulse response to preset length, no windowing</summary>*)

       (*<summary>Compute a FIR impulse response filter with user defined Window type.</summary>
         
<remarks>Compute a FIR impulse response filter with user defined Window type applied and place the result in H.
         Gain defines the filter gain. WindowParam holds
          the parameter (if required) for the window function. In case of a Kaiser
          window WindowParam should define the Beta parameter.  The H.FloatPrecision determines the precision on Input
</remarks>


        <SeeAlso cref="FirFilter"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>
        <SeeAlso cref="FractionalKaiserImpulse"/>
        <SeeAlso cref="FractionalFirImpulse"/>*)
       procedure FirImpulse(const H: TVec; W: array of double; WindowParam: double; FilterType: TFilterType;
                           WindowType: TSignalWindowType; Gain: double = 1; FS: double = 2); overload; (*<summary>Filter with any window, length must be preset</summary>*)

      (*<summary>Design an oversampled FIR filter with rectangular window.</summary>
        
<remarks>Compute a FIR impulse response filter (no window applied) and place the result in H.
        The transition regions are defined with the W array. There must be at least one (lowpass, highpass)
        and at most two (bandpass, bandstop) transition regions (2 or 4 elements). Filter type is defined with TFilterType.
        20*Log10(Ripple) is also the required attenuation of the stop band in decibel.
        FS is the sampling frequency. Length specifies the length of the original filter
        and Step defines the oversampling factor. The actual length of the impulse
        response vector is computed like this: H.Length := Round(Length/Step)
        Step and offset must be bigger then 0. If the Offest is 0 and step is 1,
        the routine returns the same result as FirImpulse.
        The resulting H vector contains FIR type impulse response, which
        can be passed to an interpolation routine (linear, cubic, lagrange, etc..).
        Oversampled FIR filters are used for resampling with an arbitrary sampling frequency
        and, if Offset &gt; 0 and Step = 1, fractional delay FIR filters can be implemented.
        If Step = 1 then the resulting impulse response can be passed directly to the
        FirInit and FirFilter routines. When setting Step bigger then 1, the filter designed must also
        work as an anti-aliasing filter (low-pass) or aliasing will occur.
</remarks>


    

    <example>
    Filter interpolation.

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
        Vector h = new Vector(0);
        Vector x = new Vector(0);

        double FS = 2;
        double Step = 1; //interpolate by 1x

        Step = 1;
        SignalUtils.FractionalFirImpulse(30,h,new double[2] {0.5,0.6},0,Step,TFilterType.ftLowpass,FS);
        x = MtxExpr.Ramp(h,0,1.0/h.Length);
        MtxVecTee.DrawIt(x,h,"Interpolate by 1x",false);
        Step = 0.5; //interpolate by 2x
        SignalUtils.FractionalFirImpulse(30,h,new double[2] {0.5,0.6},0,Step,TFilterType.ftLowpass,FS);
        x = MtxExpr.Ramp(h,0,1.0/h.Length);
        MtxVecTee.DrawIt(x,h,"Interpolate by 2x",false);
        Step = 0.25; //interpolate by 4x
        SignalUtils.FractionalFirImpulse(30,h,new double[2] {0.5,0.6},0,Step,TFilterType.ftLowpass,FS);
        x = MtxExpr.Ramp(h,0,1.0/h.Length);
        MtxVecTee.DrawIt(x,h,"Interpolate by 4x",false);
        Step = 0.125; //interpolate by 8x
        SignalUtils.FractionalFirImpulse(30,h,new double[2] {0.5,0.6},0,Step,TFilterType.ftLowpass,FS);
        x = MtxExpr.Ramp(h,0,1.0/h.Length);
        MtxVecTee.DrawIt(x,h,"Interpolate by 8x",false);
    }
    </code></example>

        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="FractionalKaiserImpulse"/>
        <SeeAlso cref="FractionalFirImpulse"/>*)
        procedure FractionalFirImpulse(Length: integer; const H: TVec; const W: array of double; Offset,Step: double; FilterType: TFilterType; FS: double = 2); overload;

        (*<summary>Design a fractional delay allpass FIR filter.</summary>
                   
<remarks>The filter properties can only be set to those covered by <see cref="TFractionalImpulse"/> type.
                   The result is returned in H and optionally a scaling factor or Gain can be specified.
                   The fractional delay of the filter can be specified with FractionalDelay parameter, which
                   must be between 0 and 1. The function returns the total filter delay including
                   the integer delay part.

                   This routine can be called when an allpass, linear phase, fractional FIR filter is desired.
                   The resulting impulse response stored in H can be passed directly to the FirInit and FirFilter
                   routines for filtering.

                    H.FloatPrecision value on input defines the precision (single or double) of the result on output.
</remarks>
*)

        function FractionalImpulse(const H: TVec; FractionalDelay: double; aFractionalImpulse: TFractionalImpulse = falp_60dB; Gain: double  = 1): double; overload;

       (*<summary>Design a FIR filter with a Kaiser window.</summary>
        
<remarks>Compute a FIR impulse response filter with kaiser window applied and place the result in H. The
        transition regions are defined with the W array. There must be at least one (lowpass, highpass)
        and at most two (bandpass, bandstop) transition regions (2 or 4 elements).
        Filter type is defined with TFilterType.

        FS is the sampling frequency. The length of the filter (H.Length) is based on the narrowest
        transition region in combination with the passband Ripple parameter.
        20*Log10(Ripple) is also the required attenuation of the stopband in decibel.
        Gain defines the filter gain. The resulting H vector contains FIR type impulse response, which
        can be passed to the FirInit routine.<para/>
        If EnsuredOdd is True, the filter length is guaranteed to have odd length.
        H.FloatPrecision value on input defines the precision (single or double) of the result on output.
</remarks>


    

    <example>
    Impulse responses of FIR filters windowed with the Kaiser window.
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
        Vector h = new Vector(0);
        Vector response = new Vector(0);
        Vector x;

        double FS = 2;  //sampling frequency
        double Ripple = 0.001;  // 60 dB stopband and 0.001 ripple in the passband

    // Lowpass design with transition band between 0.3 and 0.5 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.3,0.5} , Ripple, TFilterType.ftLowpass,1,FS,true);
        SignalUtils.FrequencyResponse(h, null, response,8,false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble, 0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Lowpass design with transition band between 0.3 and 0.5 Hz",false);

    // Highpass design with transition band between 0.3 and 0.5 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.3,0.5} , Ripple,TFilterType.ftHighpass,1,FS,true);
        SignalUtils.FrequencyResponse(h, null, response,8,false, TSignalWindowType.wtRectangular,0);
        MtxVecTee.DrawIt(x, response,"Highpass design with transition band between 0.3 and 0.5 Hz",false);

    // Bandpass design with transition bands between 0.3..0.4 Hz and between 0.5...06 Hz.
        SignalUtils.KaiserImpulse(h, new double[4] {0.3,0.4,0.5,0.6} , Ripple,TFilterType.ftBandpass,1,FS,true);
        SignalUtils.FrequencyResponse(h, null, response,8, false,TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Bandpass with transition bands 0.3-0.4 Hz and 0.5-0.6 Hz.",false);

    // Bandstop design with transition bands between 0.3..0.4 Hz and between 0.5...06 Hz.
        SignalUtils.KaiserImpulse(h, new double[4] {0.3,0.4,0.5,0.6} , Ripple,TFilterType.ftBandstop,1,FS,false);
        SignalUtils.FrequencyResponse(h, null, response,8, false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Bandstop with transition bands 0.3-0.4 Hz and 0.5-0.6 Hz.",false);

    //Hilbert III with transition bands between 0.0-0.1 Hz and 0.9-1.0 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.9,1} , Ripple,TFilterType.ftHilbertIII,1,FS,false);
        SignalUtils.FrequencyResponse(h, null, response,8, false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Hilbert III with transition bands between 0.0-0.1 Hz and 0.9-1.0 Hz.",false);

    //Hilbert IV with transition band between 0.0-0.1 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.9,1} , Ripple,TFilterType.ftHilbertIV,1,FS,false);
        SignalUtils.FrequencyResponse(h, null, response,8, false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Hilbert IV with transition band between 0.0-0.1 Hz",false);

    //Differentiator III with transition band between 0.0-0.1 and 0.9-1.0 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.9,1} , Ripple,TFilterType.ftDifferentiatorIII,1,FS,false);
        SignalUtils.FrequencyResponse(h, null, response,8, false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Differentiator III with transitions between 0.0-0.1 and 0.9-1.0 Hz",false);

    //Differentiator IV with transition band between 0.0 and 0.1 Hz.
        SignalUtils.KaiserImpulse(h, new double[2] {0.9,1} , Ripple,TFilterType.ftDifferentiatorIV,1,FS,false);
        SignalUtils.FrequencyResponse(h, null, response,8, false, TSignalWindowType.wtRectangular,0);
        x = MtxExpr.Ramp(response.Length, TMtxFloatPrecision.mvDouble,0,1.0/response.Length);
        MtxVecTee.DrawIt(x, response,"Differentiator IV with transitions between 0.0-0.1 Hz",false);
    }
    </code></example>

        <SeeAlso cref="FirFilter"/>
        <SeeAlso cref="KaiserFirLength"/>
        <SeeAlso cref="OptimalFir.RemezFirLength"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>
        <SeeAlso cref="Kaiser"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="FractionalKaiserImpulse"/>*)
       procedure KaiserImpulse(const H: TVec; W: array of double; Ripple: double; FilterType: TFilterType;
                 Gain: double = 1; FS: double = 2; EnsuredOdd: boolean = false); overload;

       (*<summary>Design an oversampled FIR filter with a Kaiser window.</summary>
        
<remarks>Compute a FIR impulse response filter with kaiser window applied and place the result in H. The
        transition regions are defined with the W array. There must be at least one and at most two
        transition regions (2 or 4 elements). Filter type is defined with TFilterType.
        FS is the sampling frequency. The length of the filter (H.Length) is based on the narrowest
        transition region in combination with the passband Ripple parameter.
        20*Log10(Ripple) is also the required attenuation of the stop band in decibel.
        Gain defines the filter gain and Step defines the step used to compute
        the oversampling factor: N = Trunc(1/Step). Step and Offset must be between 0 and 1.
        When offset is 0 and Step is 1, the routine returns the same as KaiserImpulse routine.
        The resulting H vector contains FIR type impulse response, which
        can be passed to an interpolation routine. Oversampled FIR filters are
        used for implementation of fractional delay filters and resampling
        with an arbitrary sampling frequency.

        If Step = 1 then the resulting impulse response can be passed directly to the
        FirInit and FirFilter routines.

        If ExtraSample is True an additional sample of the impulse
        response is be added to the right part of the impulse response.
        In this case, the filter is first designed with impulse response length of Length+2
        and then truncated by 1 on the left side.

        The function returns the length of the non-interpolated impulse response.

        H.FloatPrecision value on input defines the precision (single or double) of the result on output.
</remarks>


        <SeeAlso cref="KaiserFirLength"/>
        <SeeAlso cref="OptimalFir.RemezFirLength"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="FirImpulse"/>
        <SeeAlso cref="FractionalFirImpulse"/>*)
       function FractionalKaiserImpulse(const H: TVec; W: array of double; Ripple, Offset,Step: double; FilterType: TFilterType;
          ExtraSample: boolean = True; Gain: double  = 1; FS: double = 2): integer; overload;

       (*<summary>Design a Savitzky-Golay polynomial smoothing filter.</summary>
         
<remarks>Compute a Savitzky-Golay polynomial smoothing filter. The resulting filter is placed in matrix H.
         Only the center row of H is used for filtering, the upper and lower part of the H matrix are
         applied to the transition region when the signal starts and stops. Diff contains differentiation filters.
         Weights contain weights for the least square minization. Order must be less then FrameSize and
         FrameSize must be Odd. On input, the H.FloatPrecision and Weights.FloatPrecision (if present) need to match.
</remarks>
*)
       procedure SavGolayImpulse(const H, Diff: TMtx; FrameSize, Order: integer; const Weights: TVec = nil); overload;

      (*<summary>The resulting H vector contains FIR type impulse response, which can be passed to the FirInit routine.</summary>

    

    <example>
      Streamed filtering. A vector with a sine signal is broken down in to smaller
      pieces and they are filtered one by one.
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
        Vector h = new Vector(0);
        Vector b = MtxExpr.Ramp(300, TMtxFloatPrecision.mvDouble, 0, 0.1);
        Vector c = new Vector(b.Length);
        TFirState state = new TFirState();
        int n = 10;
        int i;

        SignalUtils.SavGolayImpulse(h,15,7,null);
        SignalUtils.FirInit(h,ref state,1,0,1,0);
        try
        {
            c.Size(b);
            n = 10;
            int bLength = b.Length; //must be outside of the "for" to prevent reevaluation
            for (i = 0; i &lt; (bLength/n); i++)
            {
               b.SetSubRange(i * n, n); //select only a small subvector of the vector
               c.SetSubRange(i * n, n);
               SignalUtils.FirFilter(b,c,ref state);
            }
            MtxVecTee.DrawIt(new TVec[2] { b, c }, new string[2] { "Filtered data", "Original data" }, "Savitzky Golay", false);
        }
        finally
        {
            SignalUtils.FirFree(ref state);
        }
    }
    </code></example>

      <SeeAlso cref="FirFilter"/>
      <SeeAlso cref="FirFree"/>
      <SeeAlso cref="FirInit"/>
      <SeeAlso cref="SavGolayFilter"/>*)
       procedure SavGolayImpulse(const H: TVec; FrameSize, Order: integer; const Weights: TVec = nil); overload;

       (*<summary>Savitzky-Golay FIR smothing filter.</summary>
          
<remarks>Apply Savitzky-Golay FIR smothing filter designed by the SavGolayImpulse
          routine and stored in the H matrix to the Data. For streamed filtering
          use the <see cref="FirFilter"/> routine.
</remarks>


        <SeeAlso cref="FirFilter"/>
        <SeeAlso cref="SavGolayFilter"/>
        <SeeAlso cref="SavGolayImpulse"/>*)
       procedure SavGolayFilter(const Data: TVec; const H: TMtx);


       (*<summary>Fast envelope detector.</summary>
         
<remarks>A simple and fast envelope detector using moving average filter.
         FrameSize defines the length of the moving average (low pass filter)
         and the downsampling factor. The result is placed in Dst.
         Dst.Length = Src.Length div FrameSize. Src.Length must be divisable
         by FrameSize. This routine
         is 10 to 100x times faster then a decimator based envelope detector.
         Its drawback is higher noise due to some aliasing.
         Envelope detection is used to find the frequency of events with "long"
         periods.

         For example:
         sampling frequency is 11kHz. The audio card is recording
         hammer impacts which occur once every five seconds. Because
         the duration of the hammer impact is very short, the 0.2 Hz frequency
         will not show up in the frequency spectrum of the signal, regardless
         of the frequency resolution, especially because the audio card filters
         out everything below 20 Hz. By selecting FrameSize = 2000
         the sampling frequency will be reduced by 2000x, by averaging together
         groups of rectified samples. The frequency spectrum of the filtered
         signal will show a clear peak at 0.2 Hz.
</remarks>


  

    <example>
    A simple test of the function:    
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
        Vector Response = new Vector(0);
        Vector x;
        double FS = 2;
        Vector b = MtxExpr.Sin(MtxExpr.Ramp(3000, TMtxFloatPrecision.mvDouble, 0,2*Math.PI*0.02/FS))*
                   MtxExpr.Sin(MtxExpr.Ramp(3000, TMtxFloatPrecision.mvDouble, 0,2*Math.PI*0.2/FS));
        Vector c = new Vector(b.Length);

        SignalUtils.EnvelopeDetector(b,c,10); //reduce sampling frequency by 10x

        SignalUtils.FrequencyResponse(b, null, Response, 8, true, TSignalWindowType.wtHanning, 0);
        x = MtxExpr.Ramp(Response.Length, TMtxFloatPrecision.mvDouble, 0,1.0/Response.Length);
        MtxVecTee.DrawIt(x, Response, "Original frequency spectrum", false);

        SignalUtils.FrequencyResponse(c,null,Response,8,true, TSignalWindowType.wtHanning, 0);
        x = MtxExpr.Ramp(Response.Length, TMtxFloatPrecision.mvDouble, 0,1.0/Response.Length);
        MtxVecTee.DrawIt(x, Response, "Envelope frequency spectrum", false);
    }
    </code></example>

      <SeeAlso cref="FirFilter"/>
      <SeeAlso cref="KaiserImpulse"/>*)
       procedure EnvelopeDetector(const Src, Dst: TVec; FrameSize: integer);

       (*<summary>Flip the frequency band.</summary>
         
<remarks>Flip the freqencies of the time domain signal stored in X, so that
         the DC becomes the Nyquist frequency and the Nyquist frequency
         becomes the DC. (mirror all frequencies around FS/4)
         The routine is very fast and can also be used for streaming.
         X.length must be even.
</remarks>


  

    <example>
        The sampling frequency is 256 Hz. A tone has a frequency 6Hz.
         After flipping the frequencies, the tone has a frequency of:
         FS/2 - 6 = 122Hz.
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
        Vector b = MtxExpr.Sin(MtxExpr.Ramp(256, TMtxFloatPrecision.mvDouble,0,2*Math.PI*6/256));
        Vector c = new Vector(b.Length);
        Vector Response1 = new Vector(0);
        Vector Response2 = new Vector(0);

        c.Copy(b);
        SignalUtils.BandFlip(b);
        MtxVecTee.DrawIt(new TVec[2] {c,b}, new string[2] {"Original signal","Flipped signal"},"Band flipped time signal", false);
        SignalUtils.FrequencyResponse(c, null, Response1, 1, true, TSignalWindowType.wtRectangular, 0);
        SignalUtils.FrequencyResponse(b, null, Response2, 1, true, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(new TVec[2] {Response1,Response2}, new string[2] {"Spectrum: original signal","Spectrum: flipped signal"}, "Band flipped frequency spectrum", false);
    }
    </code></example>

    <SeeAlso cref="FirFilter"/>*)
    procedure BandFlip(const X: TVec); overload;

       (*<summary>Remove the DC component.</summary>
         
<remarks>Subtract the mean value of Data from Data. Works for stationary signals.
         Use a highpass FIR filter for signals with varying mean value or call the DcFilter
         routine, for an IIR version of the DC filter. Removing the DC component without
         applying a true FIR
         or IIR filter is often convinient prior to frequency analysis, because there is no
         run-in or run-out and no filter delay.
</remarks>


  

    <example>
      The sampling frequency is 256 Hz. A tone has a frequency 6Hz, amplitude 1
      and DC offset is 4.
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
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector Response1 = new Vector(0);
        Vector Response2 = new Vector(0);

        SignalUtils.Tone(b, 256, 6.0 / 256, 0, 1, false);
        b = b + 4;
        c.Copy(b);
        SignalUtils.RemoveDC(b);
        MtxVecTee.DrawIt(new TVec[2] {c,b}, new string[2] {"Original signal","Signal without DC"},"Time signals", false);
        SignalUtils.FrequencyResponse(c, null, Response1, 1, true, TSignalWindowType.wtRectangular, 0);
        SignalUtils.FrequencyResponse(b, null, Response2, 1, true, TSignalWindowType.wtRectangular, 0);
        MtxVecTee.DrawIt(new TVec[2] {Response1,Response2}, new string[2] {"Spectrum: original signal","Spectrum: signal without DC"}, "Frequency spectrum", false);
      </code></example>

      <SeeAlso cref="FirFilter"/>
      <SeeAlso cref="KaiserImpulse"/>
      <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       function RemoveDC(const SrcDst: TVec; Index: integer; Len: integer = MtxVecEOA): TVec; overload;
       (*<summary>In-place version of the RemoveDC method.</summary>*)
       function RemoveDC(const SrcDst: TVec): TVec; overload;
       (*<summary>Not-In-place version of the RemoveDC method.</summary>*)
       procedure RemoveDC(const Src, Dst: TVec); overload;
       (*<summary>Not-in-place version of the RemoveDC method with source and
          destination indexes.</summary>
          
<remarks>The Src data is preserved, the destination will hold the result.
          If Len is not specified, the maximum possible value will be used.
</remarks>
*)
       procedure RemoveDC(const Src, Dst: TVec; SrcIndex, DstIndex: integer; Len: integer = MtxVecEOA); overload;

       (*<summary>Initialize an IIR filter.</summary>
          
<remarks>Initialize an IIR filter by initializing the IirState variable.
          Set ComplexData to True, if the data stream to be filtered will be complex.
          First half of the taps contains the numerator and the second half the denominator coefficients
          of the transfer function.  The length of the taps vector is equal to 2*(IIrOrder + 1), where
          IirOrder is the order of the Iir filter.
          Transfer function:

      <code>
              Sum( t[k]  *z^(-k) )
      H(z) = --------------------- ,           from k = 0 to N-1.
              Sum( t[N+k]*z^(-k) )


      N .. length of polynomial, N = Taps.Length/2.
      N-1 .. order of the polynomial
      t ... taps vector
      </code>
</remarks>
*)
      procedure IirInit(const Taps: TVec; var IirState: TIirState; ComplexData: boolean); overload;

      (*<summary>Initialize an IIR filter with second order sections.</summary>
         
<remarks>Initialize an IIR filter by initializing the IirState variable.
         Set ComplexData to True, if the data stream to be filtered will be complex.
         The Sos (real) vector variable contains second order sections as produced
         by the ZeroPoleToSOS function:

         (B00, B10, B20, A00, A10, A20), (B01, B11, B21, A01, A11, A21), (.... ),...

         B - numerator sections array
         A - denumarator sections array

         Using second order sections to compute an IIR filter results in higher
         numerical accuracy and greater filter stability at a slightly higher cost
         on performance.
</remarks>



        

    <example>
      Lowpass filter a sine signal with Chebyshev type I filter. Sampling frequency is 2Hz, cutoff frequency is 0.6 Hz.
      Passband ripple is 0.2 dB and filter order is 6.
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
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector sos = new Vector(0);
        Vector Response1 = new Vector(0);
        Vector Response2 = new Vector(0);
        TIirState state = new TIirState();
        int n;
        int i;

        SignalUtils.Tone(b,300,6.0/300,0,1,false);

        //   Alternative: try gaussian noise
        //    b = MtxExpr.RandGauss(300);

        IIRFilters.ChebyshevIFilter(6,0.2,new double[1] {0.6}, TFilterType.ftLowpass,false,sos,TIirFrequencyTransform.ftStateSpaceAnalog);
        SignalUtils.IirInit(sos,ref state,false);
        try
        {
             c.Copy(b);  //backup data
             SignalUtils.IirFilter(c,state);  //apply filter to the data
        }
        finally
        {
            SignalUtils.IirFree(ref state);   //free structure, if you dont require this IIR filter anymore
        }

        MtxVecTee.DrawIt(new TVec[2] {b,c}, new string[2] {"Original signal","Filtered signal"},"Time signals", false);
        SignalUtils.FrequencyResponse(c, null, Response1, 1, true, TSignalWindowType.wtHanning, 0);
        SignalUtils.FrequencyResponse(b, null, Response2, 1, true, TSignalWindowType.wtHanning, 0);
        MtxVecTee.DrawIt(new TVec[2] {Response1,Response2}, new string[2] {"Spectrum: original signal","Spectrum: filtered signal"}, "Frequency spectrum", false);
    }
    </code></example>

        <SeeAlso cref="SavGolayFilter"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>
        <SeeAlso cref="FirInit"/>
        <SeeAlso cref="IirInit"/>
        <SeeAlso cref="IirFree"/>*)

      procedure IirInitBQ(const Sos: TVec; var IirState: TIirState; ComplexData: boolean);

       (*<summary>Initialize an IIR filter.</summary>
         
<remarks>Num contains the numerator and Den the denominator coefficients
         of the transfer function. The length of the Num and Den must be equal.
         If the numerator is of lower order that the denominator, then the numerator
         should be padded with zeros from the left.
         Transfer function:
     <code>
             Sum( b[k]*z^(-k) )
     H(z) = ----------------------,    from k = 0 to N-1.
             Sum( a[k]*z^(-k) )

     N = Num.Length = Den.Length.
     b... Num vector
     a... Den vector


     H(z) is also written as:

             Sum( b[k]*z^(-k) )
     H(z) = ----------------------,    k = 0.. N-1, m = 1..N-1
             1 - Sum( a[m]*z^(-m) )

     a[0] must always be 1.
     </code>
     IIR filters evaluate the difference equation:
     <code>
     y[i] = b[0]*x[i] + b[1]*x[i-1] + ... + b[N]*x[i-N]
                      + a[1]*y[i-1] + ... + a[N]*y[i-N]

     x[i] ... input sample
     y[i] ... output (filtered) sample
     </code>

      <b>References: </b>  <para/>
      [1]Understanding digital signal processing, Richard G. Lyons, Prentice Hall, 2001.
</remarks>

      
      <SeeAlso cref="IirFilter"/>
      <SeeAlso cref="IirFree"/>
      <SeeAlso cref="IIRFilters.ButterFilter"/>
      <SeeAlso cref="IIRFilters.ChebyshevIFilter"/>
      <SeeAlso cref="IIRFilters.ChebyshevIIFilter"/>
      <SeeAlso cref="IIRFilters.EllipticFilter"/>
      <SeeAlso cref="IIRFilters.BesselFilter"/>*)
       procedure IirInit(const Num, Den: TVec; var IirState: TIirState; ComplexData: boolean = false); overload;

       (*<summary>Filter data with an IIR filter.</summary>
         
<remarks>Filter data in Src and place the result in Dst. IirState must be initialized with a call to IirInit.

         Note
          Use this routine for filtering of streaming data.
</remarks>
*)
       procedure IirFilter(const Src, Dst: TVec; var IirState: TIirState); overload;

       (*<summary>Filter data with an IIR filter.</summary>
          
<remarks>Filter Data and place the result back in Data. Use this routine for filtering of non-consecutive blocks of data.
          Set LinearPhase to True to double filter attenuation and ensure no phase distortion
          and no phase delay.
</remarks>
*)
       procedure IirFilter(const Data: TVec; var IirState: TIirState; LinearPhase: boolean = False); overload;

       (*<summary>Filter Data and place the result back in Data.</summary>
         
<remarks>The Iir filter is defined with a transfer function. Num is numerator and Den is denominator.
          Set LinearPhase to True to double filter attenuation and ensure no phase distortion
          and no phase delay (except for the initial transition regions).
</remarks>


  

    <example>
      Lowpass filter a sine signal with Chebyshev type I filter. Sampling frequency is 2Hz, cutoff frequency is 0.6 Hz.
      Passband ripple is 0.2 dB and filter order is 6.
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
       Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector num = new Vector(0);
        Vector den = new Vector(0);
        Vector Response1 = new Vector(0);
        Vector Response2 = new Vector(0);
        TIirState state = new TIirState();
        int n;
        int i;

        SignalUtils.Tone(b,300,6.0/300,0,1,false);

        //   Alternative: try gaussian noise
        //    b = MtxExpr.RandGauss(300);

        c.Size(b);
        IIRFilters.ChebyshevIFilter(6,0.2,new double[1] {0.6}, TFilterType.ftLowpass,false,num,den,TIirFrequencyTransform.ftStateSpaceAnalog);
        SignalUtils.IirInit(num,den,ref state,false);
        try
        {
            //Alternative 1, IIR streaming
            n = 10;
            int bLength = b.Length; //to prevente reevaluaton inside "for"
            for (i = 0; i &lt; (bLength/n); i++)
            {
                b.SetSubRange(i*n,n);
                c.SetSubRange(i*n,n);
                SignalUtils.IirFilter(b,c,ref state);
            }
        }
        finally
        {
            SignalUtils.IirFree(ref state);
        }

        //Alternative 2 single block filter (does not require TIirState)
        //                c.Copy(b);
        //                SignalUtils.IirFilter(c,num,den,true);

        //Alternative 3 single block
        //                c.Copy(b);
        //                SignalUtils.IirFilter(c,state);

        c.SetFullRange();
        b.SetFullRange();
        MtxVecTee.DrawIt(new TVec[2] {b,c}, new string[2] {"Original signal","Filtered signal"},"Time signals", false);
        SignalUtils.FrequencyResponse(c, null, Response1, 1, true, TSignalWindowType.wtHanning, 0);
        SignalUtils.FrequencyResponse(b, null, Response2, 1, true, TSignalWindowType.wtHanning, 0);
        MtxVecTee.DrawIt(new TVec[2] {Response1,Response2}, new string[2] {"Spectrum: original signal","Spectrum: filtered signal"}, "Frequency spectrum", false);
    }
    </code></example>

        <SeeAlso cref="SavGolayFilter"/>
        <SeeAlso cref="OptimalFir.RemezImpulse"/>
        <SeeAlso cref="FirInit"/>
        <SeeAlso cref="IirInit"/>
        <SeeAlso cref="IirFree"/>*)
       procedure IirFilter(const Data, Num, Den: TVec; LinearPhase: boolean = False); overload;

       (*<summary>Filter data with an IIR filter.</summary>
         
<remarks>Filters sample Src and places real valued output of the filter in Dst.
</remarks>
*)
       procedure IirFilter(Src: double; out Dst: double; var IirState: TIirState); overload;
       procedure IirFilter(Src: single; out Dst: single; var IirState: TIirState); overload;

       (*<summary>Filter data with an IIR filter.</summary>
          
<remarks>Filters sample Src and places complex output of the filter in Dst.
</remarks>
*)
       procedure IirFilter(const Src: TCplx; out Dst: TCplx; var IirState: TIirState); overload;
       procedure IirFilter(const Src: TSCplx; out Dst: TSCplx; var IirState: TIirState); overload;

       (*<summary>Free an IIR filter.</summary>
         
<remarks>Free any memory associated with the IIR filter initialized with the IirInit routine.
         The method can be called more than once. Nothing will be freed, if the filter
         is not initialized, but IirState must be initialized either with zeros
         or a call to IirInit before passing it to this routine.
</remarks>


        <SeeAlso cref="IirInit"/>
        <SeeAlso cref="IirFilter"/>*)
       procedure IirFree(var IirState: TIirState); overload;

 (*<summary>Initialize a FIR filter.</summary>
    
<remarks>Initialize the FIR filter by initializing the FirState variable. FirTaps must
    hold the filter impulse. FirState variable must be initialized with zeros.
    UpSample and DownSample define the sampling frequency change by an integer factor.
    UpDelay defines the initial FIR filter delay when upsampling and
    DownDelay defines the initial FIR filter delay when downsampling.
    IF Upsample or DownSample are different from 1, a multirate FIR filter is
    initialized. Multirate FIR filters avoid computing samples which will
    be discarded, because of the sampling rate change. The multirate filter
    first performs the upsampling and then the downsampling.
    Fir filters convolve an impulse response with the signal [1] p. 165:

    <code>
           M-1
    y[i] = Sum( h[k]*x[i-k] )
           k=0

    x.. input signal.
    h.. impulse response
    y.. output signal

    Convolution can also be written as: y[i] = h[k] ( * ) x[i]
    In frequency domain, convolution becomes multiplication:

    Y[I] = X[I]*H[I]

    Y.. frequency spectrum of the output signal.
    X.. frequency spectrum of the input signal.
    H.. frequency spectrum of the impulse response
    </code>
    Impulse response for typical filter types can be designed with
    <see cref="OptimalFir.remez"/>, <see cref="KaiserImpulse"/>,
    <see cref="OptimalFir.RemezImpulse"/> and <see cref="SavGolayImpulse"/>.

    <b>References: </b>  <para/>
    [1] Understanding digital signal processing, Richard G. Lyons, Prentice Hall, 2001.
</remarks>


    <SeeAlso cref="FirFilter"/>
    <SeeAlso cref="FirFree"/>
    <SeeAlso cref="IirInit"/>
    <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       procedure FirInit(const FirTaps: TVec; var FirState: TFirState;
                         UpSample: integer = 1;   UpDelay:   integer = 0;
                         DownSample: integer = 1; DownDelay: integer = 0); overload;

       (*<summary>Filter data with a FIR filter.</summary>
                  
<remarks>Uses comparatively 3x less memory from FirInit, but the function supports
                  the initialization of FIR filter only when the taps and the signal are real (not complex).
</remarks>
*)

       procedure FirInitReal(const FirTaps: TVec; var FirState: TFirState;
                      UpSample: integer = 1;   UpDelay: integer = 0;
                      DownSample: integer = 1; DownDelay: integer = 0);

       (*<summary>Filter data with a FIR filter.</summary>
          
<remarks>Filter data in Src and place the result in Dst. FirState must be
          initialized with a call to FirInit.
          This version of FirFilter routine can filter streaming blocks of data.
</remarks>
*)

       procedure FirFilter(const Src, Dst: TVec; var FirState: TFirState); overload;

       (*<summary>Filter data with a FIR filter.</summary>
          
<remarks>Filter real Src sample and place the filtered result in Dst sample.
</remarks>
*)
       procedure FirFilter(const Src: double; out Dst: double; var FirState: TFirState); overload;

       (*<summary>Filter data with a FIR filter.</summary>
          
<remarks>Filter real TCplx sample and place the filtered result in Dst sample.
</remarks>
*)
       procedure FirFilter(const Src: TCplx; out Dst: TCplx; var FirState: TFirState); overload;

       (*<summary>Filter Data with a FIR filter and place the result back in the Data.</summary>
          
<remarks>This version of FirFilter can not be used to filter streaming data.
          The routine compensates for group delay and returns filtered data
          delayed by 0 (odd FIR length) or 0.5 samples (even FIR length).
</remarks>



  

    <example>
         Lowpass filter a signal with a FIR filter.
         Sampling frequency is 2Hz, cutoff frequency is 0.6 Hz.
         Stopand passband ripple is 0.001.

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
        Vector b = new Vector(0);
        Vector c = new Vector(0);
        Vector h = new Vector(0);
        Vector Response1 = new Vector(0);
        Vector Response2 = new Vector(0);
        TFirState state = new TFirState();
        int n;
        int i;
        double FS = 2;

        SignalUtils.Tone(b,300,6.0/300,0,1,false);

    //   Alternative: try gaussian noise
    //    b = MtxExpr.RandGauss(300);

        c.Size(b);
        OptimalFir.RemezImpulse(h, new double[2] { 0.5, 0.7 }, 0.001, TFilterType.ftLowpass, 1, FS,false);
        SignalUtils.FirInit(h,ref state,1,0,1,0);
        try
        {
    //Alternative 1, FIR streaming
            n = 10;
            int bLength = b.Length; //to prevente reevaluaton inside "for"
            for (i = 0; i &lt; (bLength/n); i++)
            {
                b.SetSubRange(i*n,n);
                c.SetSubRange(i*n,n);
                SignalUtils.FirFilter(b,c,ref state);
            }
        }
        finally
        {
            c.SetFullRange();
            b.SetFullRange();
            SignalUtils.FirFree(ref state);
        }

//Alternative 2 single block filter (does not require TFirState)
//                c.Copy(b);
//                SignalUtils.FirFilter(c,h,1,1);

//Alternative 3 single block
//                c.Copy(b);
//                SignalUtils.FirFilter(b,c,state);

        MtxVecTee.DrawIt(new TVec[2] {b,c}, new string[2] {"Original signal","Filtered signal"},"Time signals", false);
        SignalUtils.FrequencyResponse(b, null, Response1, 1, true, TSignalWindowType.wtHanning, 0);
        SignalUtils.FrequencyResponse(c, null, Response2, 1, true, TSignalWindowType.wtHanning, 0);
        MtxVecTee.DrawIt(new TVec[2] {Response1,Response2}, new string[2] {"Spectrum: original signal","Spectrum: filtered signal"}, "Frequency spectrum", false);
    }
  </code></example>

      <SeeAlso cref="FirInit"/>
      <SeeAlso cref="FirFilter"/>*)
       procedure FirFilter(const Data, FirTaps: TVec; UpSample: integer = 1; DownSample: integer = 1); overload;

      (*<summary>Free a FIR filter.</summary>
       
<remarks>Free any memory associated with the FIR filter initialized with the FirInit routine.
       The method can be called more than once. Nothing will be freed, if the filter
       is not initialized, but FirState must be initialized either with zeros
       or a call to FirInit before passing it to this routine.
</remarks>


      <SeeAlso cref="FirInit"/>
      <SeeAlso cref="FirFilter"/>*)
       procedure FirFree(var FirState: TFirState); overload;

       (*<summary>Initialize a median filter.</summary>
         
<remarks>Initialize the median filter by initializing the State variable.
         The MaskSize defines the size of the maks of the median filter.
</remarks>


        <SeeAlso cref="MedianFilter"/>
        <SeeAlso cref="MedianFree"/>*)
       procedure MedianInit(MaskSize: integer; var State: TMedianState; const FloatPrecision: TMtxFloatPrecision); overload;

       (*<summary>Filter data with a median filter.</summary>
         
<remarks>Filter data in Src with a median filter configured with a call
          to MedianInit routine and place the result in the Dst.
          The length of the Dst will be set to match the Length of the Src.
          This MedianFilter routine can be used to filter streaming data.
</remarks>
*)
       procedure MedianFilter(const Src, Dst: TVec; const State: TMedianState); overload;

 

       (*<summary>Filter data in Src from SrcIndex to SrcIndex+Len with a median filter
          and place the result in Dst from DstIndex position on.</summary>
          
<remarks>MaskSize defines the size of the mask for the filter.
          This MedianFilter routine can be used to filter a block of data.
          The size of Dst is not adjusted.
</remarks>
*)
       procedure MedianFilter(const Src,Dst: TVec; MaskSize: integer;
                              SrcIndex: integer; DstIndex: integer; Len: integer = -1); overload;

      (*<summary>Filter data in Src with a median filter and place the result in Dst.</summary>
          
<remarks>MaskSize defines the size of the mask for the filter.
          This MedianFilter routine can be used to filter a block of data.
          The size of Dst is atomatically adjusted.
</remarks>
*)
       procedure MedianFilter(const Src,Dst: TVec; MaskSize: integer); overload;


       (*<summary>Filter data in Data with a median filter from DataIndex to DataIndex+Len and place
         the result back in the Data.</summary>
         
<remarks>Set the mask size of the median filter to MaskSize.
         If Len is MtxVecEOA, the maximum length of the Data vector will be used.
</remarks>


  

    <example>
    Median filter applied to a single block of data.
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
        Vector b = new Vector(0);
        Vector c = new Vector(0);

        b.Size(300);

        b.SetSubRange(0,150);
        b.Ramp(0,1);
        b.SetSubRange(150,150);
        b.Ramp(150,-1);            //creates a triangle

        b.SetFullRange();
        SignalUtils.MedianFilter(b,c,9);
        SignalUtils.MedianFilter(b,9,0,Math387.MtxVecEOA); //Alternative (in-place)
        if (!(b.Equal(c,0))) throw new Exception("Not equal");
    }
    </code></example>

    <SeeAlso cref="MedianInit"/>
    <SeeAlso cref="MedianFree"/>*)
       procedure MedianFilter(Data: TVec; MaskSize: integer; DataIndex: integer = 0;
                                                     Len: integer = -1); overload;
      (*<summary>Free a median filter.</summary>
        
<remarks>Deallocate any memory allocated by the median filter initialized with
        the MedianInit routine.
</remarks>


      <SeeAlso cref="MedianInit"/>
      <SeeAlso cref="MedianFilter"/>*)
       procedure MedianFree(var State: TMedianState); overload;

       (*<summary>Initialize an integer delay filter.</summary>
         
<remarks>Initialize the delay filter by initializing the State variable.
         The Delay parameter defines the delay in number of sampels.
</remarks>


        <SeeAlso cref="DelayFilter"/>
        <SeeAlso cref="DelayFree"/>*)

       procedure DelayInit(Delay: integer; var State: TDelayFilterState; const FloatPrecision: TMtxFloatPrecision); overload;
       (*<summary>Free a delay filter.</summary>
         
<remarks>Deallocate any memory allocated by the delay filter.
</remarks>

        <SeeAlso cref="DelayInit"/>
        <SeeAlso cref="DelayFilter"/>*)
       procedure DelayFree(var State: TDelayFilterState); overload;

       (*<summary>Filter data with an integer delay filter.</summary>
         
<remarks>Apply a delay to Src and place the result in Dst.
         The state parameter must be initialized with a call to DelayInit routine.
</remarks>


       <SeeAlso cref="DelayInit"/>
       <SeeAlso cref="DelayFree"/>*)
       procedure DelayFilter(const Src, Dst: TVec; const State: TDelayFilterState); overload;

       (*<summary>Design an impulse response of a moving average filter.</summary>
         
<remarks>Design moving average filter by initializing the Taps variable.
         Apply the moving average filter to your data, by passing the initialized
         Taps variable to the FirInit routine and then pass the FirState variable to the FirFilter routine.
         MaskSize defines the length of the filter in samples.

         The precision of the Taps is specified on input with Taps.FloatPrecision
</remarks>


        <SeeAlso cref="FirFilter"/>
        <SeeAlso cref="KaiserImpulse"/>
        <SeeAlso cref="MovingAverageFilter"/>*)
       procedure MovingAverageImpulse(const Taps: TVec; MaskSize: integer); overload;

       (*<summary>Filter data with a moving average filter.</summary>
         
<remarks>Applies moving average filter to Data from DataIndex to DataIndex+Len. Set Len to -1 to
         use the full Data length. MaskSize defines the length of the filter in samples.
         This MovingAverageFilter routine can be used to filter a block of data
         (not for streaming).
</remarks>


  

    <example>
    Moving average filter applied to a sine signal.    
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
        Vector b = new Vector(0);
        Vector c = new Vector(0);

        b.Size(300);

        SignalUtils.Tone(b, 300, 5.0 / 300, 0, 1, false);
        c.Copy(b);
        SignalUtils.MovingAverageFilter(b, 20, 0, Math387.MtxVecEOA); //from Index 0 to EndOfArray
        MtxVecTee.DrawIt(new TVec[2] { c, b }, new string[2] { "Unfiltered", "Filtered" }, "Moving average filter", false);
    }
    </code></example>

    <SeeAlso cref="MovingAverageImpulse"/>
    <SeeAlso cref="FirFilter"/>
    <SeeAlso cref="KaiserImpulse"/>
    <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       procedure MovingAverageFilter(Data: TVec; MaskSize: integer; DataIndex: integer = 0;
                                                           Len: integer = -1); overload;

       (*<summary>Filter data with an exponential average filter.</summary>
         
<remarks>Exponential average filter without the TIirState structure.
         The function can be called to filter sample by sample. Initialize
         the State to 0 on the first call. Decay defines the decay from sample
         to sample and is initialized to 50% by default. With each new sample, the
         filter will take average from the 50% of the new sample and 50%
         of the previous average. The
         exponential average filter implements the following
         difference equation:

    <code>
    y[i] = 1/d * x[i] + (d-1)/d * y[i-1]

    x.. input signal
    y.. output signal
    d.. Decay factor

    In terms of percentage:

    y[i] = a * x[i] + b * y[i-1] ,   a + b = 1

    a*100 ... percent of the new data used.
    b*100 ... percent of the old average used to compute the new average.
    </code>
</remarks>
*)
       procedure ExpAverageFilter(Data: TVec; var State: double; Decay: double = 50); overload;

      (*<summary>Filter data with an exponential averaging filter.</summary>
                 
<remarks>Single sample non-vectorized variant of the exponential averaging.
</remarks>
*)
      function ExpAverageFilter(const Data: double; var State: double; Decay: double = 50): double; overload;

      (*<summary>Filter data with an exponential average filter.</summary>
        
<remarks>Exponential average filter without the TIirState structure for complex data.
</remarks>
*)
       procedure ExpAverageFilter(Data: TVec; var State: TCplx; Decay: double = 50); overload;

      (*<summary>Design an exponential filter with Decay parameter and place
        the transfer function in Num (numerator) and Den (denominator).</summary>
        
<remarks>Transfer function can be used to initialize an IIR
        filter by passing num and den to the IirInit routine.
</remarks>


  

    <example>
    Exponential average filter.    
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
      Vector b;
      Vector c = new Vector(0);
      Vector num = new Vector(0);
      Vector den = new Vector(0);

      int n;
      int i;

      TIirState IirState = new TIirState();
      double state;

//Alternative 1: Tone with 5 periods
//      b := Sin(Ramp(300, mvDouble, 0,2*Pi*5/300));

//Alternative 2: Gaussian noise
      b = MtxExpr.RandGauss(300,false);
      c.Copy(b);
      n = 10;
      state = 0;
      int bLength = b.Length;
      for (i = 0; i &lt; (bLength / n); i ++) //streaming test 1
      {
          b.SetSubRange(i*n,n);
          SignalUtils.ExpAverageFilter(b,ref state, 10); //set to 10%
      }
      b.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] {c,b}, new string[2] {"Unfiltered","Filtered"},"Exponential averaging",false);

      b.Copy(c);
      SignalUtils.ExpAverageFilter(10,num,den); //set to 10x, (1/10 = 0.1, => 10%)

      SignalUtils.IirInit(num,den,ref IirState,false);
      bLength = b.Length;
      for (i = 0; i &lt; (bLength / n); i ++) //streaming test 2
      {
          b.SetSubRange(i*n,n);
          c.SetSubRange(i*n,n);
          SignalUtils.IirFilter(b,c,ref IirState);
      }
      SignalUtils.IirFree(ref IirState);

      b.SetFullRange();
      c.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] { b, c }, new string[2] { "Unfiltered", "Filtered" }, "Exponential averaging with IirFilter", false);
    }
    </code></example>

    <SeeAlso cref="MovingAverageImpulse"/>
    <SeeAlso cref="FirFilter"/>
    <SeeAlso cref="KaiserImpulse"/>
    <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       procedure ExpAverageFilter(Decay: double; Num, Den: TVec); overload;

       (*<summary>Design a DC filter.</summary>
          
<remarks>Design a DC filter with TransitionBandwidth and place
          the transfer function in Num (numerator) and Den (denominator).
          You can then use this transfer function to initialize an IIR
          filter with a call to IirInit.

          A DC filtered signal will be centered around zero.
          This DC filter is a simple differentiator/integrator pair.
          Transition bandwidth is the width of the frequency band
          where the amplitude is not yet completely attenuated.
          With DC filters, the transition band starts at 0 Hz.
          Narrow transition band (TransitionBandwidth/FS ratio is small)
          will result in filters with longer delays. FS is the sampling frequency.
          The filter implements the following difference equation:

    <code>
    y[i] = x[i] - x[i-1] + alpha*y[i-1]

    x.. input signal
    y.. output signal
    alpha.. parameter
    </code>

    Alpha paremeter can control the 3dB frequency of the transition
    bandwidth:

    <code>
    alpha := 1-(TransitionBandwidth/FS)*Pi;

    FS.. sampling frequency
    TransitionBandwidth.. frequency up to which will the filter have more
                          then 3dB attenuation. Must be less then FS/2.
    </code>
</remarks>
*)
       procedure DcFilter(TransitionBandwidth, FS: double; num,den: TVec); overload;

       (*<summary>:Design a DC filter with alpha parameter and place
          the transfer function in Num (numerator) and Den (denominator).</summary>
          
<remarks>This transfer function can be used to initialize an IIR
          filter with a call to IirInit. A DC filtered signal will be centered around zero.
          The DC filter is a simple differentiator/integrator pair.
          alpha is typically between 0.99 and 0.9999 and must be &lt; 1.
          A bigger alpha will cause longer filter delay.
</remarks>
*)
       procedure DcFilter(alpha: double; num,den: TVec); overload;

       (*<summary>State parameter holds the filter state.</summary>
         
<remarks>NewValue is the next sample and alpha is typically between 0.99 and 0.9999
          and must be &lt; 1. Big alpha will cause longer filter delay
          and more ringing. State should be initialized to zero
          before the routine is called for the first time.
</remarks>


  

    <example>
    DC filtering.    
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
      Vector b = new Vector(0);
      Vector c = new Vector(0);
      Vector num = new Vector(0);
      Vector x = new Vector(0);
      Vector den = new Vector(0);
      Vector Response = new Vector(0);

      int n;
      int i;

      TIirState IirState = new TIirState();
      TCplx DCState;

      SignalUtils.Tone(b,300,5.0/300,0,1,false); //generate sine with 5 periods in 300 samples

  //   Alternative:
  //          b.RandGauss;

      b = b + 2;
      c.Copy(b);
      n = 10;
      SignalUtils.DcFilter(0.95,num,den);
      SignalUtils.IirInit(num,den,ref IirState,false);
      int bLength = b.Length;
      for (i = 0; i &lt; (bLength/n); i++)   //only to test the streaming
      {
          b.SetSubRange(i*n,n);
          c.SetSubRange(i*n,n);
          SignalUtils.IirFilter(b,c,ref IirState);
      }
      b.SetFullRange();
      c.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] { b, c }, new string[2] { "Unfiltered", "Filtered" }, "DC IirFilter", false);

      SignalUtils.FrequencyResponse(num,den,Response,64,false,TSignalWindowType.wtRectangular,0);
      MtxVecTee.DrawIt(Response,"Frequency response",false);

      SignalUtils.DcFilter(0.05,2,num,den);
      SignalUtils.IirInit(num,den,ref IirState,false);
      bLength = b.Length;
      for (i = 0; i &lt; (bLength/n); i++)   //only to test the streaming
      {
          b.SetSubRange(i*n,n);
          c.SetSubRange(i*n,n);
          SignalUtils.IirFilter(b,c,ref IirState);
      }
      b.SetFullRange();
      c.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] { b, c }, new string[2] { "Unfiltered", "Filtered" }, "DC IirFilter", false);

      SignalUtils.FrequencyResponse(num,den,Response,64,false,TSignalWindowType.wtRectangular,0);
      MtxVecTee.DrawIt(Response,"Frequency response",false);

      DCState = Math387.C_ZERO;
      for (i = 0; i &lt; (b.Length); i++)   //only to test the streaming
      {
           c.Values[i] = SignalUtils.DcFilter(b.Values[i],ref DCState,0.95);
      }

      MtxVecTee.DrawIt(new TVec[2] { b, c }, new string[2] { "Unfiltered", "Filtered" }, "DC IirFilter", false);
 }
 </code></example>

  <SeeAlso cref="RemoveDC"/>
  <SeeAlso cref="IirFilter"/>
  <SeeAlso cref="IIRFilters.ButterFilter"/>
  <SeeAlso cref="IIRFilters.ChebyshevIFilter"/>
  <SeeAlso cref="IIRFilters.ChebyshevIIFilter"/>
  <SeeAlso cref="IIRFilters.EllipticFilter"/>
  <SeeAlso cref="OptimalFir.RemezImpulse"/>*)
       function  DcFilter(NewValue: double; var State: TCplx; alpha: double = 0.99): double; overload;

       (*<summary>Design a notch filer.</summary>
          
<remarks>Design a notch filter and place the transfer function
          in Num (numerator) and Den (denominator).
          You can then use this transfer function to initialize an IIR
          filter with a call to IirInit.
          A notch filter will filter out the notch frequency.
          and r defines the attenuation and width of the transition band.
          Typical values for r are from 0.99 to 0.9999.
</remarks>


        <SeeAlso cref="IirFilter"/>
        <SeeAlso cref="DcFilter"/>
        <SeeAlso cref="RemoveDC"/>*)
       procedure NotchFilter(NotchFrequency, r: double; num,den: TVec; FS: double = 2); overload;

       (*<summary>Computes the group delay of IIR filters.</summary>
         
<remarks>Num holds the numerator and Den the denominator of the
         transfer function for which the group delay should be computed.
         Den can be nil. Zeropadding defines the amount of zero
         padding applied with FFT. The result is placed in GrpDelay. Group delay
         is the first derivate of continuous phase: g = (d/dw)Phase, where w is
         the frequency ([1] p. 201).

        <b>References: </b>  <para/>
        [1] Understanding digital signal processing, Richard G. Lyons, Prentice-Hall, 2001. Advanced sampling techniques
</remarks>

        
        <SeeAlso cref="FrequencyResponse"/>
        <SeeAlso cref="PhaseUnwrap"/>*)
       procedure GroupDelay(GrpDelay, Num: TVec; Den: TVec = nil; ZeroPadding: integer = 1);
       
       procedure GroupDelay2(GrpDelay, Num: TVec; den: TVec = nil; ZeroPadding: integer = 1);
       

       (*<summary>Filter data with a sample-and-decay filter.</summary>
         
<remarks>Filter Data with a non-linear sample-and-decay filter. This filter is similar
         to Sample-And-Hold, except that the held value slowly decays
         until a bigger is found. The filter features streaming support via State
         variable. Data can be real or complex. The decay parameter
         defines how much will the value decay from sample to sample.
         The State variable has to be initialized with zeros before it is passed
         to the routine for the first time.
</remarks>


  

  <example>
  Sample and delay filter example on a sine.
    
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
      Vector b = new Vector(0);
      Vector c = new Vector(0);
      Vector num = new Vector(0);

      int n = 10;
      int i;
      TSampleAndHoldState State = new TSampleAndHoldState();

      SignalUtils.Tone(b,300,5.0/300,0,1,false);
      c.Copy(b);
      int bLength = b.Length;
      for (i = 0; i &lt; (bLength/n); i++)
      {
          b.SetSubRange(i*n,n);
          SignalUtils.SampleAndDecayFilter(b,ref State,0.95);
      }
      b.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] {c,b}, new string[2] {"Unfiltered","Filtered"},"SampleAndDecay",false);
  }
</code></example>
        <SeeAlso cref="MedianFilter"/>
        <SeeAlso cref="SampleAndHoldFilter"/>
        <SeeAlso cref="MovingAverageFilter"/>
        <SeeAlso cref="ExpAverageFilter"/>*)
       procedure SampleAndDecayFilter(Data: TVec; var State: TSampleAndHoldState; Decay: double = 0.99); overload;

       (*<summary>Filter data with a sample-and-hold filter.</summary>
          
<remarks>Filter Data with a non-linear sample-and-hold filter. The current value is held
          until a bigger is found or time-out occurs. Time out is defined in samples
          with Hold parameter. The filter features streaming support via the State
          variable. Data can be real or complex.
          The State variable has to be initialized with zeros before it is passed
          to the routine for the first time.
</remarks>




    <example>
    SampleAndHoldFilter example on a sine.    
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
      Vector b = new Vector(0);
      Vector c = new Vector(0);
      Vector num = new Vector(0);

      int n = 10;
      int i;
      TSampleAndHoldState State = new TSampleAndHoldState();

      SignalUtils.Tone(b,300,5.0/300,0,1,false);
      c.Copy(b);
      int bLength = b.Length;
      for (i = 0; i &lt; (bLength/n); i++)
      {
          b.SetSubRange(i*n,n);
          SignalUtils.SampleAndHoldFilter(b,ref State, 7);
      }
      b.SetFullRange();
      MtxVecTee.DrawIt(new TVec[2] {c,b}, new string[2] {"Unfiltered","Filtered"},"SampleAndDecay",false);
  }
  </code></example>

        <SeeAlso cref="SampleAndDecayFilter"/>
        <SeeAlso cref="MedianFilter"/>
        <SeeAlso cref="MovingAverageFilter"/>
        <SeeAlso cref="ExpAverageFilter"/>*)
       procedure SampleAndHoldFilter(Data: TVec; var State: TSampleAndHoldState; Hold: integer);

       (*<summary>Initialize a "consistent parameter" filter.</summary>
        
<remarks>Initialize TConsistentParam state variable. Param is the inital value of the parameter.
</remarks>


        <SeeAlso cref="ConsistentParameterFilter"/>
        <SeeAlso cref="SampleAndDecayFilter"/>*)
       procedure ConsistentParameterInit(Param: double; var State: TConsistentParam); overload;

       (*<summary>Filter data with a "consistent parameter" filter.</summary>
          
<remarks>Consistent parameter filter is a non-linear filter, that will
          return false once and reset the timer, if the Param value
          changes before TimeSpanSeconds has passed.
</remarks>
*)
       function ConsistentParameterFilter(Param: double; TimeSpanSeconds: double; var State: TConsistentParam): boolean; overload;
       (*<summary>Param can be boolean.</summary>

        <SeeAlso cref="ConsistentParameterInit"/>
        <SeeAlso cref="SampleAndDecayFilter"/>*)
       function ConsistentParameterFilter(Param: boolean; TimeSpanSeconds: double; var State: TConsistentParam): boolean; overload;


       

       (*<summary>Conjugate sort complex values.</summary>
         
<remarks>Sort complex values in Vec with tolerance Tol.
         Conjugate sort will group conjugated complex
         numbers together. This routine is usefull for
         processing complex zeros/poles.
</remarks>
*)
       procedure SortConj(const Vec: TVec; Tol: double = 1E-4); overload;

       (*<summary>Conjugate sort complex Vec values from Index to Index+Len.</summary>*)
       procedure SortConj(const Vec: TVec; Index, Len: integer; Tol: double = 1E-4); overload;

       (*<summary>Copy data to a circular buffer.</summary>
         
<remarks>Shifts the Buffer data to the left, to make room for new Source data
         and copies Source to the Buffer. BufferSize defines the maximum size
         up to which the Buffer will increase and before the shifting begins.
         When BufferSizse is exceeded the oldest data is dropped to make space
         for the new.

         Note
          This function is not very efficient for very big buffers, but it is very
            simple to use.
</remarks>


        <SeeAlso cref="InitCircularBuffer"/>
        <SeeAlso cref="WriteToCircularBuffer"/>
        <SeeAlso cref="ReadFromCircularBuffer"/>*)
       procedure CircularBuffer(Buffer, Source: TVec;  BuffSize: integer); overload;

       (*<summary>Initialize a circular buffer.</summary>
         
<remarks>Initializes circular buffer State variable. BufferSize is the new buffer size.
         Buffer holds the actual data and its size is adjusted to match BufferSize.
         IncrementStep defines the step by how much will the buffer increase
         when auto-resizing. NewBufferLength = OldBufferLength*IncrementStep;
         Auto-resizing is performed by <see cref="WriteToCircularBuffer"/> routine.
         The incrementStep parameter will be forced in to interval between 1 and 10.
</remarks>


        <SeeAlso cref="CircularBuffer"/>
        <SeeAlso cref="WriteToCircularBuffer"/>
        <SeeAlso cref="ReadFromCircularBuffer"/>
        <SeeAlso cref="ResizeCircularBuffer"/>
        <SeeAlso cref="MonitorCircularBuffer"/>*)
       procedure InitCircularBuffer(BufferSize: integer; IncrementStep: double; var State: TCircularBufferState); overload;

       (*<summary>Reset circular buffer.</summary>*)
       procedure ResetCircularBuffer(var State: TCircularBufferState); overload;

       (*<summary>Advance the read position of circular buffer.</summary>
         
<remarks>Advance the read position of the circular buffer by SamplesToSkip.
</remarks>
*)
       procedure AdvanceCircularBuffer(SamplesToSkip: integer; var State: TCircularBufferState); overload;

       (*<summary>Copy data to circular buffer.</summary>
         
<remarks>Copies Src.Length samples from Src to Buffer. If there is not enough space available and the
         data that has not yet been read will be overwritten, the BufferOverflow flag
         will be set in the State variable. The routine advances WritePosition of the buffer
         to the position where the next data block is to be written to. If AutoResize is True
         the buffer will be resized before the buffer is underrun or overrun. The size of
         the buffer will increased by IncrementStep parameter passed to the <see cref="InitCircularBuffer"/>.
</remarks>


        <SeeAlso cref="InitCircularBuffer"/>
        <SeeAlso cref="ReadFromCircularBuffer"/>
        <SeeAlso cref="ResizeCircularBuffer"/>
        <SeeAlso cref="MonitorCircularBuffer"/>
        <SeeAlso cref="CircularBuffer"/>*)
       procedure WriteToCircularBuffer(Buffer, Src: TVec; var State: TCircularBufferState; AutoResize: boolean = false); overload;

       (*<summary>Copy data from circular buffer.</summary>
          
<remarks>Copies Dst.Length samples from Buffer to Dst.
          If there is not Dst.Length samples available, the BufferUnderflow flag
          will be set in the State variable. The routine advances ReadPosition of the buffer
          to the position where the next data is to be read from.
          ForwardStep defines the number of samples to advance the read cursor.
          If the ForwardStep is -1, the read cursor is advanced Dst.Length samples.
</remarks>


  

  <example>
  Circular buffer test.
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
      Vector b = new Vector(0);
      Vector c = new Vector(0);
      Vector h = new Vector(0);

      int n = 6;
      int i;
      TCircularBufferState State = new TCircularBufferState();

      Vector h1 = MtxExpr.Ramp(30, TMtxFloatPrecision.mvDouble, 0,1);
      c.Size(h1);
      c.SetZero();

      h.Size(h1);
      h.SetZero();

      SignalUtils.InitCircularBuffer(11,2,ref State);
      int h1Length = h1.Length/n;
      for (i = 0; i &lt; h1Length; i++)
      {
          h1.SetSubRange(i * n, n);
          h.SetSubRange(i * n, n);
          c.SetSubRange(i * n, n);
          SignalUtils.WriteToCircularBuffer(b, h1, ref State,false);
          n = SignalUtils.PeekCircularBuffer(State);
          SignalUtils.MonitorCircularBuffer(b, h, ref State);
          SignalUtils.ReadFromCircularBuffer(b, c, ref State, -1);
      }
      h1.SetFullRange();
      h.SetFullRange();
      c.SetFullRange();
      MtxVecTee.DrawIt(new TVec[3] {h1,h,c}, new string[3] {"Original","Monitored","Read"},"Circular buffering",false);
    }
    </code></example>

        <SeeAlso cref="InitCircularBuffer"/>
        <SeeAlso cref="WriteToCircularBuffer"/>
        <SeeAlso cref="ResizeCircularBuffer"/>
        <SeeAlso cref="MonitorCircularBuffer"/>
        <SeeAlso cref="CircularBuffer"/>*)
       procedure ReadFromCircularBuffer(Buffer, Dst: TVec; var State: TCircularBufferState; ForwardStep: Integer = -1); overload;

       (*<summary>Get the number of available samples within the circular buffer.</summary>
         
<remarks>Returns number of available samples, that can be read by ReadFromCircularBuffer.
         This function allows you to guard against buffer underflow. (reading data that is not there yet)
</remarks>


        <SeeAlso cref="InitCircularBuffer"/>
        <SeeAlso cref="WriteToCircularBuffer"/>
        <SeeAlso cref="ResizeCircularBuffer"/>
        <SeeAlso cref="MonitorCircularBuffer"/>*)
       function  PeekCircularBuffer(const State: TCircularBufferState): integer; overload;

    (*<summary>Get the index in the buffer where the next block of data will be written.</summary>
      
<remarks>Get the index in the buffer where the next block of data will be written.
</remarks>


    <SeeAlso cref="PeekCircularBuffer"/>
    <SeeAlso cref="InitCircularBuffer"/>
    <SeeAlso cref="WriteToCircularBuffer"/>
    <SeeAlso cref="ResizeCircularBuffer"/>
    <SeeAlso cref="MonitorCircularBuffer"/>*)
       function WritePositionCircularBuffer(const State: TCircularBufferState): integer; overload;

       (*<summary>Resize the circular buffer.</summary>
         
<remarks>Resize the Buffer to BufferSize. The procedure also resets BufferUnderflow and BufferOverflow flags.
</remarks>


      <SeeAlso cref="InitCircularBuffer"/>
      <SeeAlso cref="WriteToCircularBuffer"/>
      <SeeAlso cref="PeekCircularBuffer"/>
      <SeeAlso cref="MonitorCircularBuffer"/>*)
       procedure ResizeCircularBuffer(BufferSize: integer; var State: TCircularBufferState; Buffer: TVec); overload;

       (*<summary>Monitor the circular buffer.</summary>
         
<remarks>Copies the most recent Dst.Length samples from the circular Buffer to Dst and
         does not advance the read cursor.
</remarks>


      <SeeAlso cref="InitCircularBuffer"/>
      <SeeAlso cref="WriteToCircularBuffer"/>
      <SeeAlso cref="ResizeCircularBuffer"/>
      <SeeAlso cref="PeekCircularBuffer"/>*)
       procedure MonitorCircularBuffer(Buffer, Dst: TVec; var State: TCircularBufferState; MonitorMode: TMonitorMode); overload;

       (*<summary>Compute running average.</summary>
          
<remarks>Compute running average of Data and place the result in Averaged. Count defines
          the Data block count already averaged and Decay defines exponential decay
          factor. If Decay is zero, the method will compute linear average. This can
          be usefull to save memory, if a very large amount of large Data blocks has
          to be averaged. If Decay is bigger then zero, then this routine is
          the vectorized version of the ExpAverageFilter routine.
</remarks>
*)
       procedure RunningAverage(const Averaged, Data: TVec; Count: integer; Decay: integer = 0); overload;

       (*<summary>Compute running average on TMtx data.</summary>
        <SeeAlso cref="ExpAverageFilter"/>*)
       procedure RunningAverage(const Averaged, Data: TMtx; Count: integer; Decay: integer = 0); overload;

       

       (*<summary>Compute a part of the bispectrum.</summary>
          
<remarks>Compute a single horizontal bispectral line (triple product) from Spectrum and place the result in Bispectrum.
          The index parameter defines for which frequency you want to compute the triple product.
          The triple product will computed for one frequency against all others.
          When used together with <see cref="RunningAverage"/> routine, bicoherence can be estimated.
          Bicoherence has 1 where a frequency pair is synchronized in phase.
          Frequency pairs which are unrelated in phase have a zero in the bicoherence matrix.
</remarks>


          <SeeAlso cref="BiSpectrumVert"/>*)
       procedure BiSpectrum(const BiSpectrum, Spectrum: TVec; Index: integer; Zeroed: boolean); overload;

      (*<summary>Compute a part of the bispectrum.</summary>
         
<remarks>Compute a single vertical bispectral line (triple product) from Spectrum and place the result in BiSpectrum.
          The index parameter defines for which frequency you want to compute the triple product.
          The triple product will computed for one frequency against all others.
          When used together with RunningAverage method, you can compute the bicoherence.
          Bicoherence has 1 where a frequency pair is synchronized in phase.
          Frequency pairs which are unrelated in phase have a zero in the bicoherence matrix.
</remarks>


          <SeeAlso cref="BiSpectrum"/>*)
       procedure BiSpectrumVert(const BiSpectrum, Spectrum: TVec; Index: integer; MtxPacked: boolean = False); overload;

       (*<summary>Conjugate extend a frequency spectrum.</summary>
         
<remarks>Assuming that Src contains the result of a real FFT, the
         Src will be conjugated, flipped and appended
         to itself.
</remarks>
*)
       function  ConjExtend(const Src: TVec): TVec; overload;
       (*<summary>The result will be placed in Dst.</summary>
        <SeeAlso cref="FrequencyResponse"/>*)
       procedure ConjExtend(const Src, Dst: TVec); overload;

        (*<summary>Yule-Walker method for autoregressive parameter estimation.</summary>
          
<remarks>Src contains the data on which the autoregressive parameter estimation (placed in A) should be based.
          Order defines the Order of the autoregressive process.
</remarks>


        <SeeAlso cref="ArCovariance"/>
        <SeeAlso cref="ArMCovariance"/>
        <SeeAlso cref="ArBurg"/>*)
       procedure ArYuleWalker(const Src, A: TVec; Order: integer);
       (*<summary>Computes a frequency spectrum with the autoregressive Yule-Walker method.</summary>
         
<remarks>Computes a frequency spectrum from Data and places the result in aResult.
         ArOrder is the autoregressive order used by the Yule-Walker method and zero
         padding factor for the FFT is defined with the ZeroPadding parameter.
</remarks>


  

  <example>
      Estimate the frequency spectrum of sine signal with the Yule-Walker
      method. The assumed order is 4 and zero padding is set to 16.
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
      Vector b, X;
      Vector Response = new Vector(0);

      b = MtxExpr.Sin(MtxExpr.Ramp(300, TMtxFloatPrecision.mvDouble, 0, 2 * Math.PI * 50 / 300));
      SignalUtils.ArYuleWalkerSpectrum(b, Response, 4, 32);
      X = MtxExpr.Ramp(Response.Length, TMtxFloatPrecision.mvDouble, 0, 1.0 / Response.Length);
      MtxVecTee.DrawIt(X, 20 * MtxExpr.Log10(MtxExpr.Abs(Response)),"",false);
  }
  </code></example>

    <SeeAlso cref="ArCovarianceSpectrum"/>
    <SeeAlso cref="ArMCovarianceSpectrum"/>
    <SeeAlso cref="ArBurgSpectrum"/>
    <SeeAlso cref="ArYuleWalker"/>*)
      procedure ArYuleWalkerSpectrum(const Data, aResult: TVec; ArOrder: integer = 40; ZeroPadding: integer = 8);

       (*<summary>Computes a frequency spectrum with the autoregressive Burg method.</summary>
         
<remarks>Computes a frequency spectrum from Data and places the result in aResult.
         ArOrder is the autoregressive order used by the Burg method and zero
         padding factor for the FFT is defined with the ZeroPadding parameter.
</remarks>


        <SeeAlso cref="ArBurg"/>*)
       procedure ArBurgSpectrum(const Data, aResult: TVec; ArOrder: integer = 40; ZeroPadding: integer = 8);

       (*<summary>Computes a frequency spectrum with the autoregressive "modified covariance" method.</summary>
         
<remarks>Computes a frequency spectrum from Data and places the result in aResult.
         ArOrder is the autoregressive order used by the modified covariance method and zero
         padding factor for the FFT is defined with the ZeroPadding parameter.
</remarks>


        <SeeAlso cref="ArMCovariance"/>
        <SeeAlso cref="ArBurgSpectrum"/>*)
        procedure ArMCovarianceSpectrum(const Data, aResult: TVec; ArOrder: integer = 40; ZeroPadding: integer = 8);

       (*<summary>Computes a frequency spectrum with the autoregressive "covariance" method.</summary>
         
<remarks>Computes a frequency spectrum from Data and places the result in aResult.
         ArOrder is the autoregressive order used by the covariance method and zero
         padding factor for the FFT is defined with the ZeroPadding parameter.
</remarks>


         <SeeAlso cref="ArCovariance"/>
        <SeeAlso cref="ArBurgSpectrum"/>
        <SeeAlso cref="ArMCovarianceSpectrum"/>
        <SeeAlso cref="ArYuleWalkerSpectrum"/>*)
        procedure ArCovarianceSpectrum(const Data, aResult: TVec; ArOrder: integer = 40; ZeroPadding: integer = 8);

       (*<summary>Modified covariance method for autoregressive parameter estimation.</summary>
         
<remarks>The AR parameter estimation is based on forward and backward
         prediction errors, and on direct estimation of of the
         reflection coefficients. Src contains the data on which
         the autoregressive parameter estimation (placed in A) should be based.
         Order defines the Order of the autoregressive process
         and K are the reflection coefficients. E is the prediction error.

        <b>References: </b>  <para/>
        [1] Introduction To Spectral Analysis, Petre Stoica and Randolph Moses, Prentice-Hall, 1997, Page 120.
</remarks>


        <SeeAlso cref="ArCovariance"/>
        <SeeAlso cref="ArMCovariance"/>
        <SeeAlso cref="ArYuleWalker"/>*)
       procedure ArBurg(const Src: TVec; Order: integer; const A,K: TVec; out E: double); overload;

       (*<summary>Modified covariance method for autoregressive parameter estimation.</summary>
         
<remarks>The AR parameters are estimated by minimizing the average of
         the estimated forward and backward prediction error powers.
         Src contains the data on which
         the autoregressive parameter estimation (placed in A) should be based.
         Order defines the Order of the autoregressive process. E is the prediction error.

        <b>References: </b>  <para/>
        [1] Modern spectral estimation, Steven M. Kay, Prentice-Hall, Page 225
</remarks>

        
        <SeeAlso cref="ArBurg"/>
        <SeeAlso cref="ArCovariance"/>
        <SeeAlso cref="ArYuleWalker"/>*)
       procedure ArMCovariance(const Src: TVec; Order: integer; const A: TVec; out E: double); overload;

       (*<summary>Covariance method for autoregressive parameter estimation.</summary>
         
<remarks>The AR parameters are estimated by minimizing an estimate
         of the prediction error power, but uses less data points
         then Yull-Walker (autocorrelation method) estimator.
         The covariance method can accurately extract frequencies
         of pure sinusoids.
         Src contains the data on which
         the autoregressive parameter estimation (placed in A) should be based.
         Order defines the Order of the autoregressive process. E is the prediction error.

         <b>References: </b>  <para/>
         [1] Modern spectral estimation, Steven M. Kay, Prentice-Hall, Page 221
</remarks>


         <SeeAlso cref="ArBurg"/>
         <SeeAlso cref="ArMCovariance"/>
        <SeeAlso cref="ArYuleWalker"/>*)
       procedure ArCovariance(const Src: TVec; Order: integer; const a: TVec; out E: double); overload;

       (*<summary>Compute the chirp z-transform.</summary>
          
<remarks>Compute the chirp z transform of Src and place it in aResult.
          Chirp z transform transforms the time domain signal Src in to frequency domain.
          (as does FFT). FStart is the starting frequency
          and FStop is the stop frequency. k defines the number of steps within
          that frequency band. FS is the sampling frequency. RStart is the starting
          radius of the circle in the z-domain and RStop is the final radius
          of the circle in the z-domain.
          CZT does the same as the FFT algorithm, if k = Src.Length*0.5, FStart = 0 and FStop = FS/2.
          and Src.Length is power of two. CZT is slower, but more versatile then FFT.
          It can be faster then FFT, for large zero padding factors.

          Chirp transform algorithm was first proposed by Bluestein.
          Generalization of the algorithm called Chirp-Z transform
          obtains samples of the z-transform spaced equally in angle
          on a spiral contour in z-plane. More on this subject can
          be found in [1] (p. 629) and [2] (p. 393). Rabiner [2] proposed
          the CZT algorithm.

         <b>References: </b>  <para/>
         [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989. <para/>
         [2] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975.
</remarks>


        <SeeAlso cref="FrequencyResponse"/>*)
       procedure CZT(const Src: TVec; k: integer; FStart, FStop: double; const aResult: TVec; FS: double = 2; RStart: double = 1; RStop: double = 1); overload;

        (*<summary>Initializes the chirp z-transform.</summary>

          
<remarks>If the parameters to CZT function do not change between calls, some variables can be pre-computed and stored in to a state variable.
          The length (and precision) of the source data is expected to remain fixed. This overload is about 2.5x faster than the variant without
          the state variable.

          The State variable does not need to be freed, if parameters change. It is also safe to call CztInit, if parameters did not change. Initialization will be
          simply skipped in this case.

          Chirp z transform transforms the time domain signal Src in to frequency domain (as do DFT and FFT).
          FStart is the starting frequency and FStop is the stop frequency. k defines the number of steps within
          that frequency band. FS is the sampling frequency. RStart is the starting
          radius of the circle in the z-domain and RStop is the final radius of the circle in the z-domain.
          CZT does the same as the FFT algorithm, if k = Src.Length*0.5, FStart = 0 and FStop = FS/2.
          and Src.Length is power of two. CZT is slower, but more versatile then FFT.
          It can be faster then FFT, for large zero padding factors.

          Chirp transform algorithm was first proposed by Bluestein.
          Generalization of the algorithm called Chirp-Z transform obtains samples of the z-transform spaced equally in angle
          on a spiral contour in z-plane. More on this subject can be found in [1] (p. 629) and [2] (p. 393). Rabiner [2] proposed
          the CZT algorithm.

          <b>References: </b>  <para/>
          [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989. <para/>
          [2] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975.
</remarks>


          <SeeAlso cref="FrequencyResponse"/>*)

        procedure CZTInit(const State: TCztState; SrcLen: integer; SrcFloatPrecision: TMtxFloatPrecision; k: integer; FStart, FStop: double; FS: double = 2; RStart: double = 1; RStop: double = 1); overload;
        (*<summary>Computes the chirp z-transform.</summary>

          
<remarks>If the parameters to CZT function do not change between calls, some variables can be pre-computed and stored in to a state variable.

          The result is placed in to the Dst variable. The State variable needs to be initialized with a call to CztInit method.
</remarks>
*)
       procedure CZT(const Src, Dst: TVec; const State: TCztState); overload;

       (*<summary>Compute the chirp z-transform.</summary>
         
<remarks>Compute the chirp z-transform of Src and place it in aResult.
         The starting frequency is zero and the stop frequency
         and FStop is at FS/2. k defines the number of steps within
         that band. RStart is the starting
          radius of the circle in the z-domain and RStop is the final radius
          of the circle in the z-domain.
</remarks>
*)
       procedure CZT(const Src: TVec; k: integer; const aResult: TVec; RStart: double = 1; RStop: double = 1); overload;

       (*<summary>Compute the chirp z-transform.</summary>
          
<remarks>Compute the chirp z-transform of Src and place it in aResult.
          The starting frequency is defined with Offset and frequency
          step is defined with Step. The final frequency is at Offset + k*Step.
          k defines the number of frequency bins at which to estimated
          the amplitude and phase of the frequency. Example for computing
          the Step and Offset:

          <code>
          Step := Expj(-(FStop - FStart)*2*Pi/(k*FS));
          Offset := Expj(2*Pi*FStart/FS);
          </code>
</remarks>


        <SeeAlso cref="FrequencyResponse"/>*)
       procedure CZT(const Src: TVec; k: integer; const Step, Offset: TCplx; const aResult: TVec); overload;

       (*<summary>Compute the frequency response.</summary>
         
<remarks>Computes complex frequency response of the transfer function
         with Num in the numerator and Den in the denominator. The result
         is placed in the Response. WindowType specifies the window
         to be applied and WindowParam is window parameter required by
         some window types. If denominator is 1 pass nil for Den.
         The function returns the actual zero padding factor. Actual
         zero padding will be different from ZeroPadding, if Num and/or Den
         do not have power of two length. If Normalize is True the,
         frequency spectrum will be normalized to reflect the true
         amplitude.
</remarks>


        <SeeAlso cref="FrequencyResponseS"/>*)
       function FrequencyResponse(const Num,Den,Response: TVec; ZeroPadding: integer = 2; Normalize: boolean = false; WindowType: TSignalWindowType = wtRectangular; WindowParam: double = 40): double; overload;

       (*<summary>Computes s-domain frequency response (Laplace transform).</summary>
         
<remarks>Computes complex frequency response of the transfer function
         with Num in the numerator and Den in the denominator.
         The result is placed in the Response. Frequencies vector
         holds the frequencies [rad/sec] at which the
         frequency response should be evaluated.
         The requested frequencies should be logarithmically
         spaced, but this is not mandatory. More points
         can be specified in the areas where the frequency
         response changes rapidly. <para/>
         Optionally you can also specify the alpha parameter as in:

         <c>s = alpha + j*Frequencies;</c><para/>
         The default value for alpha is zero.
</remarks>


      <SeeAlso cref="FrequencyResponse"/>*)
       procedure FrequencyResponseS(const Num, Den, Frequencies, Response: TVec; alpha: double = 0); overload;

       (*<summary>Compute a logarithmic ramp.</summary>
         
<remarks>Fills RampVec vector with (base 10) logarithmic scale values
         from StartPower to StopPower. The number of values
         returned is defined with RampVec.Length.
</remarks>


      

    <example>
    Compute 10 values of a logarithmic scale between powers of -1 and +1.
        
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
        Vector RampVec = new Vector(10);

        SignalUtils.LogRamp(RampVec,-1,1);
  //       RampVec = [0.1     0.1668 0.2782 0.4641 0.7742
  //                  1.2915  2.1544 3.5938 5.9948     10]
        MtxVecEdit.ViewValues(RampVec,"Logarithmic scale");
    }
  </code></example>*)
       procedure LogRamp(const RampVec: TVec; StartPower,StopPower: double);

       

       (*<summary>Compute Crest factor.</summary>
         
<remarks>Returns crest factor, if the zero mean signal had StdDev for
         standard deviation and Peak was the maximum deviation from
         the mean.

         Note
          Crest factor is usefull when monitoring ball bearings.
</remarks>
*)
       function Crest(aStdDev, aPeak: double): double; overload;

       (*<summary>Returns crest factor, for complex data.</summary>

        <SeeAlso cref="Peak"/>*)
       function Crest(aStdDev, aPeak: TCplx): double; overload;

       (*<summary>Find the maximum deviation from the mean.</summary>
          
<remarks>Compute maximum deviation from the mean, if the Min
          is the minimum value found, max was the maximum value found
          and Mean is the average of the signal.
</remarks>
*)
       function Peak(aMax, aMin, aMean: double): double; overload;
       (*<summary>Find the maximum deviation from the aMean for complex data.</summary>*)
       function Peak(aMax, aMin, aMean: TCplx): double; overload;

       (*<summary>Compute RMS of a frequency band from the frequency spectrum.</summary>
         
<remarks>Compute RMS value from the frequency spectrum instead from the time
          signal. Amplt contains the amplitude spectrum and SpectrumType defines
          the type of the amplitude spectrum. The RMS is computed from values
          from Index to Index+Len of the Amplt vector. This function allows
          computing an estimate of the RMS within a specified frequency band.
          RMS estimates made from frequency spectrum are not very accurate.
          The main reason for this is spectral leakage. While according to the
          Parsevals theorem, the RMS of the time signal is equal to the RMS
          of a frequency spectrum, this relation does not hold as soon
          as ones tries to compute the RMS of a subband. But this value,
          although not exactly a true RMS, may still serve its purpose
          to detect changes within specific frequency bands.
          The routine does not take in to account the amount of zero padding applied.
          If SpcTypeAdjust is True then the value of the RMS will be adjusted to compensate
          for the SpectrumType other then spAmplt.
</remarks>
*)
       function RmsOfSpectrum(const Amplt: TVec; SpectrumType: TSpectrumType; SpcTypeAdjust: boolean = True; Index: integer = 0; Len: integer = -1): double; overload;

       (*<summary>Return the index of the nearest value.</summary>
         
<remarks>Return the index of the value within data where the expression abs(Data[Index]- ASample)
         is smallest.
</remarks>
*)
       function FindNearest(const Data: TVec; ASample: double): integer; overload;

       (*<summary>Find the nearest peak.</summary>
         
<remarks>Converge to the nearest maximum in Data, if the start position is at Data[StartIndex].
         Returns the position of the found maximum: Data[i-1] &lt; Data[i] &gt; Data[i+1].
</remarks>
*)
       function NearestMaximum(const Data: TVec; const StartIndex: integer): integer; overload;
       (*<summary>Find the nearest peak.</summary>
         
<remarks>Converge to the nearest peak of Data, if the start position is at Data[PeakIndexes[i]].
         Returns the index position of the found peaks in PeakIndexes.
</remarks>
*)
       procedure NearestMaximum(const Data: TVec; const PeakIndexes: TVecInt); overload;

      
       function NearestMean(const Data: TVec; var Index: integer; window: integer; var Mean : double): boolean; overload;
      

       (*<summary>Find the nearest minumum.</summary>
        
<remarks>Converge to the nearest minimum in Data, if the start position is at Data[StartIndex].
        Returns the position of the found minimum: Data[i-1] &gt; Data[i] &lt; Data[i+1].
</remarks>
*)
       function NearestMinimum(const Data: TVec; const StartIndex: integer): integer; overload;

       (*<summary>Find the nearest zero crossing.</summary>
         
<remarks>Converge to the nearest zero crossing of Data, if the start position is at Data[Pos].
           Return the position of the found zero crossing in
           Pos and amplitude in Y. Use linear interpolation, if LinInterpolate
           is True, to determine the position of the actual intersection
           of the Data time series with zero. Use Y to determine how close
           the interpolation came . Y will be zero in case of linear interpolation.
</remarks>
*)
       procedure NearestZeroCrossing(const Data: TVec; var Pos: double; out Y: double; LinInterpolate: boolean = True); overload;

       (*<summary>Find the nearest DC crossing.</summary>
         
<remarks>Converge to the nearest DC crossing of Data, if the start position is at Data[Pos].
           Return the position of the found DC crossing in Index and amplitude in Y.
           Use linear interpolation, if LinInterpolate is True, to determine the position of the actual intersection
           of the Data time series with zero. Use Y to determine how close the interpolation came (Y should be DC).
</remarks>
*)
       procedure NearestDCCrossing(const Data: TVec; var Pos: double; out Y: double; DC: double; LinInterpolate: boolean = True); overload;

       (*<summary>Locate the next maximum.</summary>
         
<remarks>Return the next Maximum found in Src looking from
         offset to the right. Offset is modified to reflect the
         position of the found maximum. If a true maximum is not
         found the function returns false.
</remarks>
*)
       function NextMaximum(const Src: TVec; var Offset: integer): boolean; overload;

       (*<summary>Locate the next minimum.</summary>
          
<remarks>Return the next Minimum found in Src looking from
          Offset to the right. Offset is modified to reflect the
          position of the found maximum. If a true minimum is not
          found the function returns false.
</remarks>
*)
       function NextMinimum(const Src: TVec; var Offset: integer): boolean; overload;
       (*<summary>Locate the next zero crossing.</summary>
         
<remarks>Return the next zero crossing found in Src looking from
         Offset to the right. Offset is modified to reflect the
         position of the found zero crossing.
</remarks>
*)
       procedure NextZeroCrossing(const Src: TVec; var Offset: integer); overload;

       (*<summary>Locate the next DC crossing.</summary>
         
<remarks>Return the next DC crossing found in Src looking from
         Offset to the right. Offset is modified to reflect the
         position of the found DC crossing.
</remarks>
*)
       procedure NextDCCrossing(const Src: TVec; var Offset: integer; DC: double); overload;

       (*<summary>Return the number of peaks.</summary>
       
<remarks>Return the total count of peaks within Src starting
          at index to Index+Len. If Len = -1 end the search at Src.Length.
</remarks>
*)
       function  MaximumCount(const Src: TVec; Index: integer = 0; Len: integer = -1): integer; overload;

       (*<summary>Return the number of minimums.</summary>
        
<remarks>Return the total count of minimums within Src starting
          at index to Index+Len. If Len = -1 end the search at Src.Length.
</remarks>
*)
       function  MinimumCount(const Src: TVec; Index: integer = 0; Len: integer = -1): integer; overload;

       (*<summary>Return the number of zero crossings.</summary>
        
<remarks>Return the total count of zero crossings within Src starting
          at index to Index+Len. If Len = -1 end the search at Src.Length.
</remarks>
*)
       function  ZeroCrossingCount(const Src: TVec; Index: integer = 0; Len: integer = -1): integer; overload;

       (*<summary>Return the number of DC crossings.</summary>
         
<remarks>Return the total count of DC crossings within Src starting
          at index to Index+Len. If Len = -1 end the search at Src.Length.
</remarks>
*)
       function  DCCrossingCount(const Src: TVec; DC: double; Index: integer = 0; Len: integer = -1): integer; overload;

       (*<summary>Return the total count of zero crossings within Src starting at index to Index+Len.</summary>
          
<remarks>If Len = -1 end the search at Src.Length.
          Count DC crossings only in one way, from - to + (Negative = True) or
          from + to - (Negative = False).
</remarks>
*)
       function  DCCrossingCount(const Src: TVec; DC: double; Negative: boolean; Index: integer = 0; Len: integer = -1): integer; overload;

       (*<summary>Adjust Phase in radians to range between 0 and 2*Pi.</summary>
         
<remarks>Adjust Phase in radians to fall in range between 0 and 2*Pi.
</remarks>


        <SeeAlso cref="CircularFrequency"/>*)
       function CircularPhase(Phase: double): double; overload; 

       (*<summary>Adjust frequency in radians to fall in range between 0 and 0.5.</summary>
         
<remarks>Convert Frequency in radians to range between 0 and 0.5 for real
         signals and between -0.5 and +0.5 for Complex signals.
</remarks>


        <SeeAlso cref="CircularPhase"/>*)
       function CircularFrequency(Freq: double; Complex: boolean = false): double; overload; 

       (*<summary>Unwrap the phase of the phase spectrum.</summary>
         
<remarks>The phase spectrum in Src is unwrapped and the result is placed in Dst.
         RunningPhase defines how the unwrapped phased will be further processed.
         Tolerance defines the maximum change allowed between consecutive values,
         before the 360 (or 2*Pi) degrees is added (or subtracted). Tolerance
         must always be specified in degrees.

         If radians is True, the Src should contain the phase spectrum in radians, otherwise in degrees.
         If the time signal was padded with zeros, the ActualZeroPadding should
         hold the ratio: LengthAfterZeroPadding/LengthBeforeZeroPadding.
         ActualZeroPadding factor is needed to accurately determine the total
         phase delay. If phase delay is not an issue the ActualZeroPadding
         factor can be set to 1.

         The Src may not have central spectrum element as its last value. If the
         phase spectrum was obtained from FFT on a real data, the vector must
         be subranged before passed to the phase unwrapping routine.

         Phase unwrapping is extensively discussed in [1] (p. 790),
         especially because of its application in complex cepstrum estimation.

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>

        
        <SeeAlso cref="FrequencyResponse"/>
        <SeeAlso cref="PhaseWrap"/>*)
         function PhaseUnwrap(const Src, Dst: TVec; RunningPhase: TRunningPhase;
                      ActualZeroPadding: double = 1; Radians: boolean = false; Tolerance: double = 180): double;

         (*<summary>Add phase lag to the unwrapped phase.</summary>
           
<remarks>Phase lag, as returned by the PhaseUnwrap routine, will be added to Src
           and the result will be placed in Dst. If Radians is True, the phaseLag
           was computed in radians.
</remarks>


        <SeeAlso cref="PhaseUnwrap"/>
        <SeeAlso cref="FrequencyResponse"/>
        <SeeAlso cref="CplxCepstrumInv"/>*)
         procedure PhaseWrap(const Src, Dst: TVec; PhaseLag: double; Radians: boolean = false);

       (*<summary>Compute real cepstrum.</summary>
         
<remarks>Compute real cepstrum of Src and place the result in Dst.
         If DCDump is True, the DC component will be removed prior
         to computing FFT. WindowType specifies the window type
         used for the forward FFT. Real cepstrum can be used
         to find periodicites in the frequency spectrum itself.
         It is namely defined as an inverse FFT of the logarithm of the
         magnitude spectrum [1] (p. 770, eq 12.8):

         <code>
         rc = IFFT(log(abs(FFT(x)))

         x.. time series
         </code>

         Sometimes we also find the following definition:

         <c>rc = FFT(log(abs(FFT(x)))</c><para/>
         A good choice for the window function is the Hanning window.

      <b>References: </b>  <para/>
      [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989.
</remarks>


      <SeeAlso cref="CplxCepstrum"/>
      <SeeAlso cref="CplxCepstrumInv"/>*)
       procedure RealCepstrum(const Src, Dst: TVec; DumpDC: boolean = True; WindowType: TSignalWindowType = wtHanning; Magnitude: boolean = false);

       (*<summary>Compute complex cepstrum.</summary>
         
<remarks>Compute complex cepstrum of Src and place the result in Dst.
         ZeroPadding defines the amount of zero padding used.
         Large zero padding factors will give better phase unwrapping
         results. The function returns integer phase lag subtracted
         from the unwrapped phase. This parameter can then be used for inverse
         complex cepstrum. Different methods to compute complex cepstrum and its applications in homomorphic
         filtering can be found in [1] (Chapter 12) and [2] (p. 687). It is
         defined as:
         <code>
         rc = IFFT(log(FFT(x)))

         x.. time series
         </code>
         Complex cepstrum can be used (among other things)
         to detect and measure echo delays in the signal and for filtering applications.
         The user should considering windowing the time time signal prior to passing it to this routine.

         The main problem of efficient complex cepstrum
         estimation is the estimation of the complex
         logarithm.

         As mentioned, the function returns integer phase lag subtracted
         from the unwrapped phase. This lag is subtracted because
         the unwrapped phase otherwise formes a step function for the
         FFT which follows. By pushing the begining and the end of the
         unwrapped phase towards zero, the FFT which follows will be
         more effective and will give a more clear picture, because
         there will be no circular discontinuity in the unwrapped
         phase. Similar effect is achieved with window functions and when
         removing a trend.

         For example, if a time series is superimposed on a linear regression line, it makes
         sense first to subtract linear regression line (de-trend)
         and to form a zero mean signal before performing an FFT, because
         otherwise the signal does not start close to where it ends.
         (FFT sees the signal as a circular infinite signal.)

        <b>References: </b>  <para/>
        [1] Discrete-time signal processing, Oppenheim and Schafer, Prentice-Hall, 1989 <para/>
        [2] Theory and application of digital signal processing, Lawrence R. Rabiner and Bernard Gold. Prentice-Hall, 1975
</remarks>

        
        <SeeAlso cref="RealCepstrum"/>
        <SeeAlso cref="CplxCepstrumInv"/>*)
       function CplxCepstrum(const Src, Dst: TVec; ZeroPadding: integer = 16): double;
       (*<summary>Compute the inverse complex cepstrum.</summary>
         
<remarks>Compute inverse complex cepstrum of Src and place the result in Dst.
         Phase lag is the value returned by CplxCepstrum. TargetLength is
         the length of the original (not zero padded) signal passed to the CplxCepstrum
         routine.
</remarks>


    

    <example>
    Forward and inverse complex cepstrum.    
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
       Vector a = new Vector(0);
       Vector b = new Vector(0);
       double k;

       a.SetIt(false,new double[8] {1,2,3,4,3,2,-1,1});
       k = SignalUtils.CplxCepstrum(a,b,64);
       SignalUtils.CplxCepstrumInv(b,a,k,a.Length);
        // "a" again becomes the same as on the input a =  [1,2,3,4,3,2,-1,1]
       MtxVecTee.DrawIt(a,"Original time signal",false);
    }
    </code></example>

        <SeeAlso cref="RealCepstrum"/>
        <SeeAlso cref="CplxCepstrum"/>
        <SeeAlso cref="FrequencyResponse"/>*)
       procedure CplxCepstrumInv(const Src, Dst: TVec; PhaseLag: double; TargetLength: integer);

        

       (*<summary>Initialize the generation of a sine function.</summary>
        
<remarks>Initialize a sine signal ToneState with frequency Freq, initial Phase and amplitude Mag.
        Set Complex to true to generate a complex signal. ToneState is to be passed to the Tone
        routine to generate the actuall time series.
</remarks>


      <SeeAlso cref="Tone"/>
      <SeeAlso cref="TriangleTone"/>*)
       procedure ToneInit(Freq, Phase, Mag: double; var ToneState: TToneState; Complex: boolean = false);

       (*<summary>Generate a sine function.</summary>
         
<remarks>Place a tone defined with a call to ToneInit in Dst. The routine is stream capable.
</remarks>


          <SeeAlso cref="ToneInit"/>*)
       procedure Tone(const Dst: TVec; var ToneState: TToneState); overload;

       (*<summary>Generate a sine function.</summary>
         
<remarks>Generate a single tone wave signal with frequency Freq, initial Phase and amplitude Mag.
         Set Complex to true to generate a complex signal. The routine is not stream capable.
</remarks>


      <SeeAlso cref="ToneInit"/>*)
       procedure Tone(const Dst: TVec; SampleCount: integer; Freq, Phase, Mag: double; Complex: boolean = false); overload;


       (*<summary>Initialize the generation of a square tone.</summary>
        
<remarks>Initialize a square signal ToneState with frequency Freq, initial Phase and amplitude Mag.
        Set Complex to true to generate a complex signal. State is to be passed to the SquareTone
        routine to generate the actuall time series. The time series is generated by Fourier Series
        expansion. For very low frequencies, for example 1Hz, and sampling frequency of 192kHz, this would
        result in 48000 distinct sine tones to be generated and would require proportional computational power.
        The maxHarmonics parameter defines the maximum number of harmonics (including the fundamental)
        to be generated for the approximation of the square signal. This can help limit the CPU requirements
        in case of very low frequencies. About 200 Harmonics would already give a very good shape of "square".
</remarks>


      <SeeAlso cref="Tone"/>
      <SeeAlso cref="TriangleTone"/>*)
       procedure SquareToneInit(Freq, Phase, Mag: double; var State: TSquareToneState; Complex: boolean = false; const MaxHarmonics: integer = -1);

       (*<summary>Generate a square wave function.</summary>
         
<remarks>Place a tone defined with a call to ToneInit in Dst. The routine is stream capable.
         Dst.IsDouble will be used to determine floating point precision.
</remarks>


          <SeeAlso cref="ToneInit"/>*)
       procedure SquareTone(const Dst: TVec; var State: TSquareToneState); overload;

       (*<summary>Generate a square wave function.</summary>
         
<remarks>Generate a square wave signal with frequency Freq, initial Phase and amplitude Mag.
         Set Complex to true to generate a complex signal. The routine is not stream capable.
         Dst.IsDouble will be used to determine floating point precision.
</remarks>


      <SeeAlso cref="ToneInit"/>*)
       procedure SquareTone(const Dst: TVec; SampleCount: integer; Freq, Phase, Mag: double; Complex: boolean = false; const MaxHarmonics: integer = -1); overload;


       (*<summary>Initialize the generation of a triangle function.</summary>
         
<remarks>Initialize a triangular signal ToneState with frequency Freq, initial Phase, Asym assymetry and amplitude Mag.
         Set Complex to true to generate a complex signal. ToneState is to be passed to the TriangleTone
         routine to actually generate the time series.
</remarks>


        <SeeAlso cref="TriangleTone"/>
        <SeeAlso cref="Tone"/>*)
       procedure TriangleToneInit(Freq, Phase, Mag, Asym: double; var ToneState: TTriangleState; Complex: boolean = false);

       (*<summary>Generate a triangle function.</summary>
         
<remarks>Place a tone defined with a call to TriangleToneInit in Dst. The routine is stream capable.
</remarks>


        <SeeAlso cref="TriangleToneInit"/>*)
       procedure TriangleTone(const Dst: TVec; var ToneState: TTriangleState);

(*<summary>Compute the probability that given probability distributions are (not) the same.</summary>
  
<remarks>The function integrates the envelope of a set of gaussian PDF functions defined with
   Means (holding mean values) and StdDevs (holding standard deviations)
   arrays. The start of the integration interval is determined by the PDF with
   smallest value of expression: Mean-3*StdDev.
   The end of the integration interval is determined by the PDF with
   largest value of expression: Mean+3*StdDev.
   If all PDF functions are distinctly a part, the value of the integral
   will be equal to the number of PDF functions. If all PDF functions
   exactly overlapp each other, the value of the integral will be 1.
   This function is therefore a measure of overlapping of a set of
   gaussian probability density functions. The result will
   return this measure in percent. 0% for full overlapping and 100% for
   no overlapping. Precision
   defines integration precision by defining the step:
   h = (Smallest StdDev)*3/Precision.
   The drawback of this approach is that CPU usage grows
   with precision, distance between the mean values
   and smaller standard deviations.
   If the precision is too small, the function
   will degenerate in to histogram calculation.
   If you are integrating only over two distributions,
   that is not a serious problem and the result will
   remain meaningfull, but not very accurate.
</remarks>
*)
   function DistinctCDF(const Means,StdDevs: array of double; Precision: integer = 100; MaxSteps: integer = 10000): double; overload;
   (*<summary>Returns a measure for the probability that x belongs to the normal distribution
      with Mean and StdDev.</summary>
      
<remarks>The function returns the 1- (integral of PDF on the intervals [-Inf,mean-abs(mean-x)],[mean+abs(mean-x),+Inf]).
      The function returns near 100, if x is further then 4*StdDev away from mean and returns 0, if x = mean.
</remarks>
*)
   function DistinctCDF(x, Mean, StdDev: double): double; overload;
   (*<summary>Convert a TSpectrumType type to a string.</summary>
     
<remarks>The function returns a description of a TSpectrumType type
     specified by the SpectrumType parameter.
</remarks>
*)
   function SpectrumTypeToString(SpectrumType: TSpectrumType): string; overload;

   (*<summary>Align string by inserting space.</summary>
     
<remarks>AString is padded with spaces to match ColumnWidth according to the value
     of Align parameter.
</remarks>
*)
   function AlignString(const AString: string; ColumnWidth: integer; Align: TFixedTextAlign = ftaLeftAlign): string;

   (*<summary>Compute standard deviation.</summary>
     
<remarks>Returns standard deviation according to the formula:

     <code>
                          n*Sum(x^2) - Sum(x)^2
              StdDev = ( ------------------------- )^0.5
                                 n*(n-1)

     n... number of averages taken
     Sum(x^2)... sum of squares of samples
     Sum(x)... sum of samples
     </code>
</remarks>
*)
   function StdDev(SumOfSquares, Sum: double; Averages: integer): double; overload;
   (*<summary>Return standard deviation in aResult.</summary>*)
   procedure StdDev(const SumOfSquares, Sum: TVec; Averages: integer; const aResult: TVec); overload;
   (*<summary>Return standard deviation in aResult.</summary>*)
   procedure StdDev(const SumOfSquares, Sum: TMtx; Averages: integer; const aResult: TMtx); overload;

   (*<summary>Convert amplitude to decibels.</summary>
     
<remarks>Converts Data (amplitude) to decibels. Base is the base of the logarithm and should be 10,
     if decibels are desired. The logarithm is then scaled by 20 according to the formula
     LogN(Base,Value)*20. Span defines the dynamic
     range in dB on the left axis. The values below -span will be clipped.
     If Normalized is True, the maximum will be set to zero and minimum to -Span. The function returns -Span.
</remarks>
*)
  function AmpltToDb(const Data: TDenseMtxVec; Base, Span, aMax: double; Normalized: boolean): double;
   (*<summary>Convert amplitude to logarithmic scale.</summary>
     
<remarks>Applies logarithm to Data. Base is the base of the logarithm and should be 10,
     if decibels are desired. The logarithm is then scaled by aScale according to the formula
     LogN(Base,Value)*aScale. Span defines the dynamic
     range on the left axis. If aScale is 20 and base is 10, the Span is in dB.
     The values below -span will be clipped.
     If Normalized is True, the maximum will be set to zero and minimum to -Span. The function returns -Span.
</remarks>
*)
  function AmpltToLog(const Data: TDenseMtxVec; Base, Span, aMax, aScale: double; Normalized: boolean): double;

  (*<summary>Returns description of the signal.</summary>
    
<remarks>Returns description of the signal with given data. To ommit NumberOfrecords just enter a zero.
</remarks>
*)
  function SignalFormatDescription(FS: double; Precision: TPrecision; Ch: integer;  NumberOfRecords: integer = 0): string;

  (*<summary>The Discrete Fourier transformation (DFT) for a given frequency.</summary>
    
<remarks>Calculates the Discrete Fourier transformation (DFT) for a given frequency. Goertz returns the DFT at the Frequency.
    Each value of the DFT computed by the Goertzel algorithm takes 2N+2 real multiplications and 4N real
    additions. FFT computes the DFT with N*log2(N) of real multiplications and additions.

    The Goertz method is faster, than direct DFT, if we need values at consecutively different frequencies.
    At every call the Freq parameter is different.

    If the frequencies, at which we need to evalute the spectrum are fixed, then the direct DFT method is faster than Goertz.

    The function applies Bonzanigo's phase correction. This means, that phase information is reliable and correct also when
    computed at an arbitrary non-integer frequency. An "integer frequency" is such that an (exact) integer number of periods
    fits in to the data (signal) of the specific length.
</remarks>


    

    <example>
    DFT of a single frequency.

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
       Vector a = new Vector(0);

       SignalUtils.Tone(a,256,5.5/256,0,12.53,false); //generate a tone
    //And now detect the amplitude at non-integer frequency:
       MessageBox.Show("Amplitude = " + Math387.SampleToStr(Math387.CAbs(SignalUtils.Goertz(a,5.5/256))/128,0,15));
    }
    </code></example>

    <SeeAlso cref="GoertzTwo"/>*)
  function Goertz(const Src: TVec; Freq: double; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo): TCplx; overload;

  (*<summary>The Discrete Fourier transformation (DFT) for a given frequency.</summary>
             
<remarks>SrcIndex and srcLen define the sub-range of Src on which to compute the function.
</remarks>
*)
  function Goertz(const Src: TVec; Freq: double; const srcIndex: integer; srcLen: integer = -1; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo): TCplx; overload;

  (*<summary>The Discrete Fourier transformation (DFT) for given two frequencies.</summary>
    
<remarks>Calculates the Discrete Fourier transformation (DFT) for two given frequencies.
    Goertz returns the DFT at the two frequencies pass as a parameter.
    Each value of the DFT computed by the Goertzel algorithm takes 2N+2 real multiplications and 4N real
    additions. FFT computes the DFT with N*log2(N) of real multiplications and additions.

    The Goertz method is faster, than direct DFT, if we need values at consecutively different frequencies.
    if at every call the Freq0 and Freq1 parameters are different.
    If the frequencies, at which we need to evalute the spectrum are fixed, then the direct DFT method is faster.
</remarks>



    <SeeAlso cref="Goertz"/>*)
  procedure GoertzTwo(const Src: TVec; Freq0, Freq1: double; var Result0, Result1: TCplx; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo); overload;

  (*<summary>The Discrete Fourier transformation (DFT) for given two frequencies.</summary>
             
<remarks>SrcIndex and srcLen define the sub-range of Src on which to compute the function.
</remarks>
*)
  procedure GoertzTwo(const Src: TVec; Freq0, Freq1: double; var Result0, Result1: TCplx; const srcIndex: integer; srcLen: integer; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo); overload;


  (*<summary>The Discrete Fourier transformation (DFT) for vector of frequencies.</summary>
    
<remarks>Calculates the Discrete Fourier transformation (DFT) for an array of frequencies
    defined in the Freq parameter.
    Goertz returns the DFT of the frequencies pass as a parameter in the Dst variable.
    Each value of the DFT computed by the Goertzel algorithm takes 2N+2 real multiplications and 4N real
    additions. FFT computes the DFT with N*log2(N) of real multiplications and additions.

    The Goertz method is faster, than direct DFT, if we need values at consecutively different frequencies.
    If at every call the values in Freq parameter are different.
    If the frequencies, at which we need to evalute the spectrum are fixed between calls, then the direct DFT method is faster.
</remarks>


    <SeeAlso cref="Goertz"/>*)
  procedure Goertz(const Src, Freq, Dst: TVec; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo); overload;

  (*<summary>The Discrete Fourier transformation (DFT) for vector of frequencies.</summary>
             
<remarks>SrcIndex and srcLen define the sub-range of Src on which to compute the function.
</remarks>
*)
  procedure Goertz(const Src, Freq, Dst: TVec; const srcIndex: integer; srcLen: integer; PhaseCorrection: TGoertzPhaseCorrection = gpcBonzanigo); overload;

  (*<summary>Computes FIR filter from specified frequency spectrum.</summary>
    
<remarks>Amplt defines desired frequency spectrum (linear scale) and Freq defines the
    frequency at which the spectrum was sampled. Frequencies must be between 0 and 1
    and have to include 0 and 1. H contains the impulse response upon completion.
    H.Length specifies the length of impulse response on input. The accuracy of
    filter matching the desired response greatly depends upon the length of the filter
    especially, when the desired response contains sharp or steep transitions.
    The function internally performs linear interpolation between points specified
    by Amplt and Freq vectors. The desired precision will be picked from Amplt.FloatPrecision.
</remarks>
*)

  procedure InverseFilter(const Amplt, Freq, H: TVec; WindowType: TSignalWindowType = wtBlackman);

  (*<summary>Computes FIR filter with desired noise/signal coloring response.</summary>
             
<remarks>Parameter dBPerDecade specifies the filters response in terms of
             decibels per decade roll-off or roll-up. The parameter can be positive or negative. When
             euqal to 10, the FIR filter generated will have pink (noise) response.
             When equal to 20 it will generate brown(ian) (noise) response.
             Paramter FS specifies the sampling frequency and StartBW the location
             of the "knee" of the filter at which the linear dB/decade response
             begins. StartBW may not be too close to 0. The closer the value is to zero the
             longer will be the filter. Longer filter means longer delay and slower computation times.
             To produce for example pink noise first generate random noise
             and then filter the noise with the filter response H:

             x.RandGauss();
             FirFilter(x, H);

             The desired precision will be picked from H.FloatPrecision
</remarks>
*)

  procedure ColoredNoiseFIR(const H: TVec; const dBPerDecade, StartBW, FS: double);

  (*<summary>Returns string representation of TMusicalNotePitch.</summary>
             
<remarks>Returns string representation of TMusicalNotePitch including the Octave.

             Example: C#2
</remarks>
*)
  function MusicalNotePitchToStr(const Src: TMusicalNotePitch; const Octave: integer): string; overload;
  (*<summary>Initializes MusicalScale according to Pitch standard and required octave count.</summary>
            
<remarks>The MusicalScale parameter is initialized according to parameters. The MusicalScale is required
            by the MusicalNotePitch function.  Default pitch standard is 440Hz and default Ocatave count is 11.
            This should suffice for most application. The pitch standard of 440Hz applies to note A in 4th octave.
            There are however many variations around the world. Cuba uses 436, Hungary 438, Berlin orchestra 443, etc..

          <b>References: </b>  <para/>
          [1] https://www.thebeliever.net/pitch-battles/
</remarks>



        <SeeAlso cref="MusicalNotePitch"/>
        <SeeAlso cref="MusicalNotePitchToStr"/>*)
  procedure MusicalScaleInit(const MusicalScale: TVec; PitchStandard: double = 440; OctaveCount: integer = 11);
  (*<summary>Returns musical note pitch, the octave and pitch error in Hz.</summary>

        <param name="Freq">The frequency in Hz to convert to musical note pitch.</param>
        <param name="pitchError">The difference between the nearest musical note pitch and Freq in Hz.</param>
        <param name="freqOctave">Returns the octave number on output.</param>
        <param name="aMusicalScale">The musical scale as returned by the MusicalScaleInit method.  </param>

        
<remarks>Returns musical note pitch as the result, the octave number and pitch error in Hz in the parameters.
</remarks>


        <SeeAlso cref="MusicalScaleInit"/>
        <SeeAlso cref="MusicalNotePitchToStr"/>*)
  function MusicalNotePitch(const Freq: double; out pitchError: double; out freqOctave: integer; const aMusicalScale: TVec): TMusicalNotePitch; overload;
  (*<summary>Returns musical note pitch, the octave and pitch error in Hz.</summary>

        <param name="Freq">The array of frequencies in Hz to convert to musical note pitch.</param>
        <param name="pitchError">The difference between the nearest musical note pitch and Freq in Hz.
                               To obtain error in % compute:

                               pitchError[i]/Freq[i]*100%

                               </param>

        <param name="freqOctave">Contains the octave numbers on output.</param>
        <param name="aMusicalScale">The musical scale as returned by the MusicalScaleInit method. aMusicalScale.FloatPrecision
                                    will be used as the input parameter to indicate the desired precision (single or double).</param>
        <param name="aMusicalNotePitch">Contains musical note pitch on the output. The values in MusicalNotePitch need to by typcasted to an enumerated type:

                                        MusicalNotePitchToStr(TMusicalNotePitch(MusicalNotePitch[i]));

                                        </param>*)
  procedure MusicalNotePitch(const Freq: TVec; const pitchError: TVec; const freqOctave: TVecInt; const aMusicalScale: TVec; const aMusicalNotePitch: TVecInt); overload;











