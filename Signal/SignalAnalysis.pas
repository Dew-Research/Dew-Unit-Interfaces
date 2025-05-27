


















{$I BdsppDefs.inc}


(*<summary> Signal analysis components. </summary>*)
unit SignalAnalysis;


interface

{$WARN SYMBOL_DEPRECATED OFF}

uses Math387, SignalTools, MtxVec, SignalUtils, MtxBaseComp,
     SignalProcessing, MtxParseExpr, MtxParseClass, MtxVecBase, MtxVecInt

     

     

     
     ,Classes
     ,SysUtils
       
       ,Contnrs
       

     

     ;



type

    (*<summary>Defines the transformation to be applied to the time signal.</summary>
      
<remarks>Defines the transformation for the TSignalAnalyzer component.
</remarks>
*)
    TTimeTransform = (
    (*<summary>Applies a user defined time window.</summary>*)ttWindow,
    (*<summary>Computes autocorrelation.</summary>*) ttAutoCorr,
    (*<summary>Computes cross correlation.</summary>*) ttCrossCorr,
    (*<summary>Discrete cosine transform.</summary>*)  ttDct,
    (*<summary>Inverse discrete cosine transform.</summary>*) ttInverseDct,
    (*<summary>Real cepstrum.</summary>*) ttRealCepstrum,
    (*<summary>Complex cepstrum.</summary>*) ttCplxCepstrum
    );

    (*<summary> Defines the autocorrelation type. </summary>
      
<remarks>Autocorrelation type can be: Normal, Biased or unbiased.
       See TVec.AutoCorr routines... for more info.
</remarks>
*)
    TAutoCorrType = (
    (*<summary>Normal autocorrelation scaling.</summary>*) ctNormal,
    (*<summary>Biased autocorrelation scaling</summary>*) ctBiased,
    (*<summary>Unbiased autocorrelation scaling</summary>*) ctUnbiased
    );

    (*<summary> Defines integration/differentiation options. </summary>
      
<remarks>Defines the integration/differentiation for TSignalAnalyzer component.
</remarks>
*)

    TIntegration = (
    (*<summary>No integration or differentiation.</summary>*) inNone,
    (*<summary>Integrates once.</summary>*) inOnce,
    (*<summary>Integrates twice</summary>*) inTwice,
    (*<summary>Differentiates once.</summary>*) diffOnce,
    (*<summary>Differentiates twice</summary>*) diffTwice
    );

    (*<summary>Defines methods for frequency spectrum estimation. </summary>
      
<remarks>Defines the methods available to compute the frequency spectrum.
</remarks>
*)
    TSpectrumMethod = (
    (*<summary>Fast Fourier Transform.</summary>*) smFFT,
    (*<summary>Autogressive YuleWalker method.</summary>*) smYuleWalker,
    (*<summary>Burg autogressive method.</summary>*) smBurg,
    (*<summary>Covariance based autoregressive method.</summary>*) smCov,
    (*<summary>Modified covariance based autogressive method.</summary>*) smMCov,
    (*<summary>Chirp Z-transform.</summary>*) smCZT
    );

    (*<summary>Defines how to compute the logarithm of the frequency spectrum. </summary>
      
<remarks>Defines how the logarithm is to be applied to frequency spectrum.
</remarks>
*)
    TLogType = (
    (*<summary>Applies the logarithm with base 10 and produces result in dB.</summary>*)ltAbsolute,
    (*<summary>Applies the logarithm with base 10, produces result in dB and positions the maximum value at zero.</summary>*) ltRelative,
    (*<summary>Applies the logarithm. Uses additional parameters for log base and scaling.</summary>*) ltAbsoluteParam,
    (*<summary>Applies the logarithm and positions the maximum value at zero.
       Uses additional parameters for log base and scaling.</summary>*) ltRelativeParam
    );

    (*<summary>Defines the span of the frequency spectrum in dB. </summary>
      
<remarks>Defines the span of the frequency spectrum in dB. Considered only when TLogType is ltAbsolute or ltRelative.
</remarks>
*)
    TLogSpan = (
    (*<summary>30dB</summary>*)  ls30,
    (*<summary>45dB</summary>*)  ls45,
    (*<summary>60dB</summary>*)  ls60,
    (*<summary>90dB</summary>*)  ls90,
    (*<summary>120dB</summary>*)  ls120,
    (*<summary>150dB</summary>*)  ls150,
    (*<summary>200dB</summary>*)  ls200,
    (*<summary>350dB</summary>*)  ls350
    );

    (*<summary>Defines averaging type. </summary>
       
<remarks>Defines the type of averaging used by the spectrum analyzer.
       Finite average differs from infinite by stopping the averaging
       at a predefined average count. Averaging of complex spectrums
       is the same as averaging of the time signal and can be used only,
       if consecutive time signal frames are phase synchronized.
</remarks>
*)

    TAveraging = (
    (*<summary>No averaging.</summary>*) avNone,
    (*<summary>Linear infinite averaging.</summary>*) avLinearInf,
    (*<summary>Exponential infinite averaging.</summary>*) avExponentInf,
    (*<summary>Linear finite averaging.</summary>*) avLinear,
    (*<summary>Finite peak hold "averaging".</summary>*)  avPeakHold,
    (*<summary>Infinite peak hold "averaging".</summary>*) avPeakHoldInf,
    (*<summary>Peak hold and exponential decay.</summary>*) avPeakAndDecay,
    (*<summary>Finite linear average of complex spectrum's.</summary>*)  avCplxLinear,
    (*<summary>Infinite linear average of complex spectrum's.</summary>*)  avCplxLinearInf,
    (*<summary>Infinite exponential average of complex spectrum's.</summary>*)    avCplxExponentInf);

       (*<summary>Specifies how to mark peaks automatically. </summary>
         
<remarks>Defines the method used to mark peaks on the frequency spectrum
</remarks>
*)
    TPeakTraceMethod = (
      (*<summary>Existing marks will not be removed, but their amplitudes
         will be updated.</summary>*) ptNone,
      (*<summary>Existing marks will be shifted in frequency to the
         closest peaks.</summary>*)  ptCurrent,
      (*<summary>Existing marks will be removed, and a prespecified number of
         largest peaks will be marked.</summary>*)  ptLargest,
      (*<summary>Existing marks will be removed and a band limited search is
         performed to find the peak with the largest amplitude.
         The newly found peak is regarded as a fundamental frequency and a prespecified
         number of harmonics is marked.</summary>*)  ptAmpltHarmonics,
      (*<summary>All marks except the first are removed. Treats the first mark as the
         fundamental frequency and then marks a prespecified
         number of harmonics.</summary>*)  ptFreqHarmonics,
         (*<summary>Finds all peaks, which are not too close.</summary>*)
         ptAllPeaks,
         (*<summary>Finds all largest peaks, which are not too close. It then searches for the harmonic series with the strongest support.</summary>*)
         ptFirstHarmonic
         );

     (*<summary>Defines a set of frequency spectrum peak interpolators. </summary>
       
<remarks>Defines the method used to interpolate the frequency of a marked peak.
       Frequency spectrum is computed only at discrete frequencies.
       If you want to know the actual frequency of the peak that falls
       between two frequency bins, you have to use peak interpolation.
       You can also simply increase the number of frequencies at which
       the frequency spectrum is calculated, by increasing the zero padding.
       Peak interpolation is more accurate and faster.
</remarks>
*)

    TInterpolationMethod = (
    (*<summary>No frequency interpolation will be performed.</summary>*) imNone,
    (*<summary>Numerical peak interpolation is the most accurate and
       can be used with any window and any zero padding setting,
       but it is also the slowest. Absolute accuracy of the numerical
       interpolator can be increased dramatically by using a Kaiser window with high
       attenuation factors.</summary>*) imNumeric,
    (*<summary>Requires rectangular window, no zero padding, but works with amplitude spectrum only
        and is fast. Accuracy is not very good.</summary>*)  imQuadratic,
    (*<summary>Requires rectangular window, no zero padding, but works with amplitude spectrum only
        and is fast. Accuracy is not very good.</summary>*)  imBarycentric,
    (*<summary>Works with rectangular window and no zero padding.
        Requires complex frequency spectrum and is very fast.</summary>*) imQuinnFirst,
    (*<summary>Works with rectangular window and no zero padding.
        Requires complex frequency spectrum and is very fast.
        The second Quinn estimator is very accurate and fast, if
        your frequency spectrum is not based on zero padded data and
        you can afford to use the rectangular window.</summary>*) imQuinnSecond,
    (*<summary>Requires rectangular window, no zero padding, but works with amplitude spectrum only
        and is very fast. Accuracy is not very good.</summary>*)  imJain
     );

    (*<summary>Precision specification for numerical peak interpolation. </summary>
      
<remarks>Defines the precision to be achieved by the numerical peak
      interpolator.
</remarks>
*)

    TInterpolationPrecision = (
    (*<summary>Interpolate to accuracy 1E+2 higher then frequency
       spectrum resolution.</summary>*)  ip2,
    (*<summary>Interpolate to accuracy 1E+4 higher then frequency
       spectrum resolution.</summary>*)  ip4,
    (*<summary>Interpolate to accuracy 1E+6 higher then frequency
       spectrum resolution.</summary>*)  ip6,
    (*<summary>Interpolate to accuracy 1E+8 higher then frequency
       spectrum resolution.</summary>*)  ip8
       );

    (*<summary>Band reference specification. </summary>
      
<remarks>Defines the band reference used by the routines that compute statistics
       from the bands of the frequency spectrum.
</remarks>
*)
    TBandReference = (
    (*<summary>The frequency band is defined with absolute frequencies.</summary>*) fnSamplingFrequency = 0,
    (*<summary>The frequency band is defined with frequencies normalized
       with the sampling frequency frequency.</summary>*) fnNyquist = 1,
    (*<summary>The frequency band is defined relative to a special
       frequency defined with value of the OrderFrequency property.</summary>*) fnOrder = 2,
    (*<summary>The frequency band is defined relative to the frequency
       of the N'th peak.</summary>*)   fnNthPeak = 3
       );

    (*<summary>Event type passing a TVec object as a parameter. </summary>
      
<remarks>Event type passing a TVec object as a parameter.
</remarks>
*)
    TNotifyVecEvent = procedure (Sender: TObject; Vec: TVec) of object;
    (*<summary>Event type passing a TMtx object as a parameter. </summary>
      
<remarks>Event type passing a TMtx object as a parameter.
</remarks>
*)
    TNotifyMtxEvent = procedure (Sender: TObject; Mtx: TMtx) of object;


    TBandRecordDouble = packed record
       Checked: byte;
       StartTransHz: double;
       StartHz: double;
       EndHz: double;
       EndTransHz: double;
       RMSValue: double;
       BandReference: byte;
       SpcTypeAdjust: byte;
       PeakNumber: Int32;
       end;

    TBandRecordArray = array of TBandRecordDouble;
    TStaticSpectrumAnalyzer = class;



    (*<summary>Stores the definition of a frequency band. </summary>
        
<remarks>Frequency band is specified to compute RMS from a specified
        frequency range. It is also possible to specify transition
        regions to approximate digital filter response.
</remarks>
*)

    TBandRecord = class
       public
       (*<summary>If Checked is True, the statistics based on this band will be updated. </summary>*)
       Checked: boolean;
      (*<summary> StartTransHz is the starting transition of the frequency band. </summary>*)
       StartTransHz: Double;
      (*<summary> Start of the pass band. </summary>
       
<remarks>Amplitude is zero and raises lineary until it reaches StartHz, where
       the amplitude is no longer attenuated. This transition band
       can be used, to simulate the effect of a transition region
       of a digital filter.
</remarks>
*)
       StartHz: Double;
      (*<summary> End of the pass band. </summary>*)
       EndHz: Double;
      (*<summary> EndTransHz is the ending transition of the pass band. </summary>
                 
<remarks>This transition band can be used, to simulate the effect
                 of a transition region of a digital filter.
</remarks>
*)
       EndTransHz: Double;
      (*<summary>RMSValue holds the RMS estimated on the frequency band. </summary>*)
       RMSValue: Double;
      (*<summary> Band reference defines the frequency reference for the band definition. </summary>*)
       BandReference: TBandReference;
       (*<summary> If true, the RMS value will be adjusted to compensate for the spectrum type. </summary>*)
       SpcTypeAdjust: boolean;
      (*<summary> PeakNumber is an additional parameter used by the NthPeak band reference method. </summary>*)
       PeakNumber: integer;
      (*<summary> Assign all field values from Src. </summary>*)
       procedure Assign(Src: TBandRecord); overload;
      (*<summary> Assign all field values from Src. </summary>*)
       procedure Assign(const Src: TBandRecordDouble); overload;
      (*<summary> Assign all field values to Dst. </summary>*)
       procedure AssignTo(var Dst: TBandRecordDouble); overload;
      (*<summary> Maps frequency information to array indexes . </summary>*)
       procedure MapToIndexes(var Idx1, Idx2, Idx3, Idx4: integer; const Spc: TStaticSpectrumAnalyzer); overload;
       end;

      TSpectrumBandsAnalyzer = class;

      (*<summary>A list to frequency band definitions. </summary>
        
<remarks>Manages a list to frequency band definitions. The list ownes the items.
</remarks>
*)


       TSpectrumBands = class(TStreamedList)
       strict private
          function  GetItems(Index: Integer): TBandRecord;
          procedure SetItems(Index: Integer; const Value: TBandRecord);
          
          procedure WriteArrayOfRecords(var Src: TBandRecordArray; Dst: TStream;  Len: integer); overload;
          procedure ReadArrayOfRecords(Src: TStream; var Dst: TBandRecordArray; Len: integer); overload;
          
          
       strict protected
          
          FAOwner: TSpectrumBandsAnalyzer;
       public
          (*<summary>Recalculate the statistics for bands. </summary>*)
          procedure Update; override;
          (*<summary>Add a new band definition and allocate the record internally. </summary>*)
          function  Add(const Item: TBandRecord): integer;
          
          (*<summary>Save the list to stream. </summary>*)
          procedure SaveToStream(Dst: TStream); override;
          (*<summary>Load the list from the stream. </summary>*)
          procedure LoadFromStream(Src: TStream); override;
          
          
          (*<summary>Create the list and pass TSpectrumBandsAnalyzer component as the owner. </summary>*)
          constructor Create(AOwner: TSpectrumBandsAnalyzer);
          (*<summary>Default array property allows access to individual list items. </summary>*)
          property Item[Index: Integer]: TBandRecord read GetItems write SetItems; default; 
       end;

      TSpectrumAnalyzer = class;
      TCustomSpectrumAnalyzer = class;

      
      (*<summary>A list of list of lists of frequency band definitions.</summary>
        
<remarks>Manages a list of lists of frequency band definitions.
</remarks>
*)
      TSpectrumBandsAnalyzer = class(TStreamTemplates)
      strict private
          function  GetItems(Index: Integer): TBandRecord;
          procedure SetItems(Index: Integer; const Value: TBandRecord);
          procedure SetTemplate(const Value: TSpectrumBands);

          function  GetTemplate: TSpectrumBands;
      strict protected
          function GetStreamedTemplate: TStreamedList; override;
      public
          (*<summary>Default array property allows access to individual list items.</summary>*)
          property  Items[Index: Integer]: TBandRecord read GetItems write SetItems; default;
          (*<summary>Convert values to strings. </summary>
             
<remarks>The strings will be stored in TStrings object with two columns,
             alligned for display with a fixed charachter width Font. The Header of each column
             is defined with the XTitle and ATitle and PTitle properties. The header line
             will not be displayed, if you set XTitle, ATitle and PTitle to empty strings.
             The width of the columns is defined with global variable: SignalTextColumnWidth.
             If FrequencyFormat or RMSFormat are empty strings,
             full numeric precision will be used for the coresponding column.
</remarks>
*)
          procedure ValuesToStrings(Strings: TStrings; const FrequencyFormat: string = '';
                                                       const RMSFormat: string = '';
                                                       const FreqTitle: string = 'Frequency range [Hz]';
                                                       const RMSTitle: string = 'RMS';
                                                       const Delimiter: string = ''); virtual;

          constructor Create(AOwner: TComponent); overload; override;
      published

         
         (*<summary>Holds a list of objects. </summary>
          
<remarks>All changes made to this list of objects
          are preserved, when you change TemplateName or TemplateIndex.
</remarks>
*)
          property Template: TSpectrumBands read GetTemplate write SetTemplate; 
      end;

   (*<summary> Refinement of the frequency of the first harmonic. </summary>
   
<remarks>The algorithm will find the fundamental frequency and then compute the frequency of the higher harmonic.
   Once the higher harmonic is found, it will interpolate the frequency of the higher harmonic and then from that
   value compute the (improved) frequency of the fundamental. This allows for automatic detection of very long harmonic
   series (hundreds and thousands) and a much improved accuracy of the estimation of the fundamental.
</remarks>
*)
    TRecursiveHarmonics = (
    (*<summary>Do not improve accuracy of the first harmonic after locating higher harmonics.</summary>*)
    rhNone,
    (*<summary>Improve accuracy of the first harmonic after locating odd higher harmonics.</summary>*)
    rhOdd,
    (*<summary>Improve accuracy of the first harmonic after locating even higher harmonics.</summary>*)
    rhEven,
    (*<summary>Improve accuracy of the first harmonic after locating any higher harmonics.</summary>*)
    rhAll);

    

    (*<summary>Encapsulates parameters used for peak interpolation. </summary>
      
<remarks>Holds the parameters required used by peak interpolation methods.
</remarks>
*)
    TSpectrumPeakInterpolation = class(TPersistent)
    strict private
        FIPrecision: Double;
        FHarmonics: boolean;
        FMethod: TInterpolationMethod;
        FPrecision: TInterpolationPrecision;
        FRecursiveHarmonics: TRecursiveHarmonics;
        fMethodBackup: TInterpolationMethod;
        procedure SetHarmonics(const Value: boolean);
        procedure SetMethod(const Value: TInterpolationMethod);
        procedure SetPrecision(const Value: TInterpolationPrecision);
        procedure SetRecursiveHarmonics(const Value: TRecursiveHarmonics);
     public
        (*<summary>Disables interpolation. </summary>
            
<remarks>First stores the value of the Method property and then sets its value to imNone.
</remarks>
*)
        procedure Disable;
        (*<summary>Restores interpolation. </summary>
            
<remarks>Resets the value of the Method property to what it was before the call to Disable method.
</remarks>
*)
        procedure Restore;
        (*<summary>Holds the interpolation precision required. Read only. </summary>*)
        Property IPrecision: Double read FIPrecision;
     published

       
        (*<summary>Set this property to define the required interpolation precision for the numerical peak interpolator. </summary>
                   
<remarks>Values higher than ip4 will be downgraded to ip4 when the frequency spectrum was computed with single precision.
</remarks>
*)
        property Precision: TInterpolationPrecision read FPrecision write SetPrecision  default ip6 ;

       
        (*<summary>Interpolate first harmonic only. </summary>
          
<remarks>Set this property to True, if only the fundamental frequency should be interpolated and
          the frequency of the harmonics should be computed from the fundamental
          (1x, 2x, 3x...).
</remarks>
*)
        property Harmonics: boolean read FHarmonics write SetHarmonics  default False ;

       
        (*<summary> Refinement of the frequency of the first harmonic. </summary>
                    
<remarks>Higher harmonics are recursively used to improve the frequency estimation of the first harmonic.
</remarks>
*)
        property RecursiveHarmonics: TRecursiveHarmonics read FRecursiveHarmonics write SetRecursiveHarmonics;

       
        (*<summary>Defines the peak interpolation method used. </summary>*)
        property Method: TInterpolationMethod read FMethod write SetMethod  default imNumeric ;
     end;


     (*<summary>Normalized tracing options. </summary>
        
<remarks>Tracing means, that on each subsequent computed frequency spectrum, the
        frequency analyzer will recursivelly use the previous value of the
        specified marked peak (otNthPeak) to search for the new nearest peak.
        Alternativelly the marked peak can also be specified as the largest found
        marked peak (otLargest). In this case, the previous value of the
        for this peak is disregarded. Usually the largest peak is also the fundamental frequency.
        otFirstPeak can be used when examining the ratios between different
        peaks.
</remarks>
*)

    TNormTracing = (otNone,
    (*<summary>Perform tracking relative to the first marked peak from the left.</summary>*) otFirstPeak,
    (*<summary>Perform tracking relative to the N'th marked peak.</summary>*)  otNthPeak,
    (*<summary>Perform tracking relative to the largest marked peak.</summary>*) otLargest
    );

      

     (*<summary> Groups parameters for normalized frequency tracing options. </summary>*)

    TNormalizedFreq = class(TPersistent)
    strict private
      
      Spectrum: TSpectrum;

      FAppliedFrequency: Double;
      FPeakNumber: integer;
      FNormTracing: TNormTracing;
      FNormFrequency: Double;
      FActive: boolean;
      FDefaultFS: Double;
      FDefaultHzRes: Double;
      procedure SetActive(const Value: boolean);
      procedure SetNormFrequency(const Value: Double);
      procedure SetNormTracing(const Value: TNormTracing);
      procedure SetPeakNumber(const Value: integer);
      function GetList: TMarkList;
      procedure SetDefaultFS(const Value: Double);
      procedure SetDefaultHzRes(const Value: Double);
    public
      constructor Create(Aowner: TSpectrum); virtual;
        (*<summary>Computes the normalized frequency. </summary>
           
<remarks>Computes the normalized frequency with the method specified by the
           Tracing property. Active property must be true for
           this routine to have any effect. If Trace = otNthPeak then
           the PeakNumber is used to determine the peak to be used
           as a reference to compute the normalized frequency.
</remarks>
*)
      procedure ApplyNormalization;
      property AppliedFrequency: double read FAppliedFrequency;
      property List: TMarkList read GetList;
      (*<summary>Specifies the normalized frequency. </summary>
        
<remarks>Normalized frequency is a predefined frequency,
        that always has a value of 1 on the frequency axis. Normalized frequency
        can be specified absolutely in Hz or it can be specified
        relative to some marked peak. When normalized frequency is specified
        relative to a marked peak, this marked peak can be traced.
</remarks>
*)
      property NormFrequency: Double read FNormFrequency write SetNormFrequency;
       (*<summary>When Active is True, this property contains
          the actual frequency spectrum resolution. </summary>*)
      property DefaultHzRes: Double read FDefaultHzRes write SetDefaultHzRes;
       (*<summary>When Active is True, this property contains
           the actual sampling frequency. </summary>*)
      property DefaultFS: Double read FDefaultFS write SetDefaultFS;
    published

       
      (*<summary>Defines the tracing method. Order tracing determines, how
         will the normFrequency be estimated. </summary>*)
      property Tracing: TNormTracing read FNormTracing write SetNormTracing  default otNone ;

       
      (*<summary>Used when Tracing = otNthPeak. </summary>*)
      property PeakNumber: integer read FPeakNumber write SetPeakNumber default 1;

       
      (*<summary>Set this property to True, to enable normalized Frequency. </summary>
         
<remarks>When normFrequency will be applied to the frequency spectrum, the
         frequency equal to normFrequency will have a value of 1 on the
         frequency axis. You can then see the ratio of all other
         frequencies towards the normFrequency. This can be usefull when
         determining harmonics or half-harmonics.
</remarks>
*)
      property Active: boolean read FActive write SetActive  default False ;
    end;

      

   (*<summary> Groups parameters for normalized amplitude tracing options. </summary>*)

    TNormalizedAmplt = class(TPersistent)
    strict private
      
      Spectrum: TSpectrum;

      FAppliedAmplitude: Double;
      FPeakNumber: integer;
      FNormTracing: TNormTracing;
      FNormAmplitude: Double;
      FActive: boolean;
      procedure SetActive(const Value: boolean);
      procedure SetNormAmplitude(const Value: Double);
      procedure SetNormTracing(const Value: TNormTracing);
      procedure SetPeakNumber(const Value: integer);
      function GetList: TMarkList;
    public
      constructor Create(Aowner: TSpectrum); virtual;
      (*<summary>Computes the amplitude normalization factor. </summary>
         
<remarks>Computes the scaled factor with the method specified by the
         Trace property. Active property must be true for
         this routine to have any effect. If Tracing = otNthPeak then
         the PeakNumber is used to determine the peak to be used
         as a reference to compute the NormalizedAmplt factor.
</remarks>
*)
      procedure ApplyNormalization;
      property List: TMarkList read GetList;
      property AppliedAmplitude: double read FAppliedAmplitude;
      (*<summary>Specifies the normalized amplitude. </summary>
        
<remarks>Normalized amplitude is a predefined amplitude,
        that always has a value of 1 on the frequency axis. Normalized amplitude
        can be specified absolutely in Hz or it can be specified
        relative to some marked peak. When normalized amplitude is specified
        relative to a marked peak, this marked peak can be traced (followed between spectrum updates).
</remarks>
*)
      property NormAmplitude: Double read FNormAmplitude write SetNormAmplitude;
    published

       
      (*<summary>Defines the tracing method. Order tracing determines, how
         will the normAmplitude be estimated. </summary>*)
      property Tracing: TNormTracing read FNormTracing write SetNormTracing  default otNone ;

       
      (*<summary>Used when Tracing = otNthPeak. </summary>*)
      property PeakNumber: integer read FPeakNumber write SetPeakNumber default 1;

       
      (*<summary>Set this property to True, to enable normalized amplitude. </summary>
         
<remarks>When normAmplitude will be applied to the frequency spectrum, the
         amplitude equal to normAmplitude will have a value of 1 on the
         amplitude axis. You can then see the ratio of all other
         amplitudes towards the normAmplitude.
</remarks>
*)
      property Active: boolean read FActive write SetActive  default False ;
    end;

      

    (*<summary>Manages frequency spectrum peaks. </summary>
      
<remarks>Used the mark, trace, interpolate and filter peaks.
</remarks>
*)
      TSpectrumPeaksAnalyzer = class(TPersistent)
      strict private
        BandLIndex: integer;
        BandHIndex: integer;
        FLargestCount: integer;
        FTraceMethod: TPeakTraceMethod;
        FTraceInterval: Double;
        FInterpolation: TSpectrumPeakInterpolation;
        FBandwidthL: Double;
        FBandwidthH: Double;
        FUseFullBandwidth: boolean;
        FActive: boolean;
        
        FSpectrum: TSpectrum;
        FNormalizedFreq: TNormalizedFreq;
        FNormalizedAmplt: TNormalizedAmplt;
        FLargestRatio: double;
        Freq, cEst, aEst, pEst: TVec;  
        procedure SetHarmonicsCount(const Value: integer);
        procedure SetInterpolation(const Value: TSpectrumPeakInterpolation);
        procedure SetLargestCount(const Value: integer);
        procedure SetTraceInterval(const Value: Double);
        procedure SetTraceMethod(const Value: TPeakTraceMethod);
        function GetList: TMarkList;
        function GetHarmonicsCount: integer;
        procedure SetBandwidthH(const Value: Double);
        procedure SetBandwidthL(const Value: Double);
        procedure SetUseFullBandwidth(const Value: boolean);
        procedure SetActive(const Value: boolean);
        function GetMarks(i: integer): TMarkRecord;
        procedure SetMarks(i: integer; const Value: TMarkRecord);

        procedure SetNormalizedAmplt(const Value: TNormalizedAmplt);
        procedure SetNormalizedFreq(const Value: TNormalizedFreq);
        procedure SetLargestRatio(const Value: double);
      strict protected
        function  Tau(x: Double): Double;
 protected 
        aIdx: TVecInt;
      public
        property Spectrum: TSpectrum read FSpectrum;
        (*<summary>Default array property for the marks (=peaks) array. </summary>*)
        property Marks[i: integer]: TMarkRecord read GetMarks write SetMarks; default;
        (*<summary>A pointer to a list of all marked peaks (= TSpectrum.Marks). Read only. </summary>*)
        property List: TMarkList read GetList;
        (*<summary>Reestimates the order frequency and traces the peaks. </summary>
           
<remarks>Applies the OrderFrequency, traces the peaks according to
           the value of the TraceMethod property and reestimates the OrderFrequency.
</remarks>
*)
        procedure Update;
        (*<summary>Called by Update method, if TraceMethod is ptNone.</summary>*)
        procedure Adjust;
        (*<summary>Converts all peaks in dB. The associated spectrum must already be in logarithmic scale. </summary>*)
        procedure LogAmplt;
        (*<summary>Called by Update method, if TraceMethod is ptAll.</summary>
                   
<remarks>It is recommended that the ZeroPadding is 2 or more.
                   Higher values of zero padding will allow more accurate frequency and amplitude comparisons.
                   Too high values of zero padding will exponentionally increase the computational requirements.
</remarks>
*)
        procedure FindAllPeaks;
        (*<summary>Called by Update method, if TraceMethod is ptLargest. </summary>
                   
<remarks>The function will find the largest peaks count specified with the LargestCount property.
                   The search is analytical and guaranteed to return the largest peaks, when peak interpolation.method property is set to TSignalInterpolation.imNumeric.
                   The result is also influenced by the LargestRatio property.
                   The largest peaks are returned in the List property, which is sorted by descending amplitude.
                   It is recommended that the ZeroPadding is 2 or 4 for best performance.
                   ZeroPadding bigger than 1 speeds up the processing mostly when the signal being analyzed is close to pure noise.
                   Using TSpectrumAnalyzer.Window set to wtKaiser with TSpectrumAnalyzer.SidelobeAtt value equal or more than 60 will help with performance.

                   This function is called by
                   * FindFirstHarmonic
                   * FindFundamentals
                   * FindAllPeaks
</remarks>
*)
        procedure FindLargestPeaks;
        (*<summary>Called by Update method, if TraceMethod is ptFirstHarmonic. </summary>
                  
<remarks>The routine will call FindFundamentals, but keep only the first most probable peak.
</remarks>
*)
        procedure FindFirstHarmonic;
        (*<summary>Locates the first harmonics of a harmonic series. </summary>
          
<remarks>Uses FindLargestPeaks to start and then determines the amount of support
          for each frequency, that it is in fact also a first harmonic. This will work also for
          odd spaced harmonics.

          The result is a re-sorted TSpectrumPeaksAnalyzer.Marks property (frequency ascending) with lower frequencies at the begining of the list.
          The algorithm will not exclude candidates, which do not have higher harmonics.

          It is sometimes recommended to set LargestRatio parameter to 10 to improve accuracy. FindLargestPeak method uses LargestCount property to
          determine the number of largest peaks to select. The processing will use numerical peak interpolation and is guaranteed to find all peaks regardless of the
          selected spectral window method and zero-padding factor used to compute the frequency spectrum.
</remarks>
*)
        procedure FindFundamentals;

        (*<summary>Finds the largest peak on the frequency interval. </summary>
           
<remarks>Frequency interval is limited in two ways. First, it is limited
           with values of the properties: BandwidthL and BandwidthH.
           This limitation applies only, if UseFullBandwidth is False.
           And second, it is limited with the value of the TraceInterval property.
           The Frequency parameter contains the previous known value of
           the marked peak on entry and the frequency of the newly found peak
           on exit from the routine.
</remarks>
*)
        function FindFrequency(const Frequency: Double): Double; overload;

        (*<summary>Finds the largest peak on the frequency interval for each value in Frequency parameter. </summary>
               
<remarks>Frequency interval is limited in two ways. First, it is limited
               with values of the properties: BandwidthL and BandwidthH.
               This limitation applies only, if UseFullBandwidth is False.
               And second, it is limited with the value of the TraceInterval property.
               The Frequency parameter contains the previous known value of
               the marked peak on entry and the frequency of the newly found peak
               on exit from the routine.
</remarks>
*)
        procedure FindFrequency(const Frequency: TVec); overload;

        (*<summary>Filters the peak specified with Index. </summary>
           
<remarks>Filters the peak defined with position Index within the List
           from the time signal associated with the frequency spectrum
           and recalculates the frequency spectrum.
</remarks>
*)
        procedure Filter(Index: integer);

        (*<summary>Finds the maximum of the peak at Freq and applies interpolation. </summary>
           
<remarks>Applies the peak interpolation methods, specified with
           the interpolation property, to get a better estimate
           of the frequency of the peak then Freq. The value
           of the Freq can be the frequency of the closest
           frequency bin in the frequency spectrum.
</remarks>
*)
        function Approximate(Freq: Double): Double; overload;

        (*<summary>Finds the maximum of the peak at Freq and applies interpolation. </summary>*)
        procedure Approximate(const Freqs: TVec; const freqIdx: TVecInt); overload;

        (*<summary>Finds the maximum of the peak at Freq and applies interpolation. </summary>
                   
<remarks>Finds the nearest peak and applies interpolation for each srcFreq. It stores the amplitude and phase in to dstAmplt and dstPhase.
</remarks>
*)
        procedure Approximate(const srcFreq, dstAmplt, dstPhase: TVec); overload;

        (*<summary>Finds the maximum of the peak at Freq and applies interpolation. </summary>
           
<remarks>Applies the peak interpolation methods, specified with the Interpolation property, to get a better estimate
           of the frequency of the peak closest to the Freq. The value of the Freq can be the frequency of the closest
           frequency bin in the frequency spectrum. The function also returns the complex value spectral bin at the specified frequency,
           if peak interpolation was TInterpolationMethod.imNumeric. Alternatively the result of cplxEstimate is simply zero.
</remarks>
*)
        function ApproximateComplexEst(Freq: Double; out cplxEstimate: TCplx): Double; overload;

        (*<summary>Finds the maximum of the peak at Freq and applies interpolation. </summary>
                   
<remarks>Applies peak interpolation methods, specified with the Interpolation property, to get a better estimate
                   of the peaks closest to the frequencies in the Freqs. Freqs parameter will be updated with new values and cplxEst will hold the complex
                   results. The value of the Freq can be the frequency of the closest frequency bin in the frequency spectrum depending on the
                   interpolation method. cplxEst returns the complex value spectral bin at the specified frequency,
                   if peak interpolation was TInterpolationMethod.imNumeric. Alternatively the result of cplxEst is simply zero.
                   freqIdx contains the indexes in the spectrum for Freqs on output. When interpolation is active, the indexes will correspond to the
                   closest spectral line and its content can be discarded.

                   The vectorized variant of this method is roughly 10x faster than the single frequency overload, provided that Freq.Length is large enough.
                   When Freq.Length is 10 then speed up is about 3x and when it is 100, the speed up is about 10x.
</remarks>
*)
        procedure ApproximateComplexEst(const Freqs, cplxEst: TVec; const freqIdx: TVecInt); overload;

        (*<summary>Called by the Update method, if TraceMethod is ptAmpltHarmonics. </summary>*)
        procedure AmpltHarmonics;
        (*<summary>Called by the Update method, if TraceMethod is ptFreqHarmonics. </summary>*)
        procedure FreqHarmonics;
        (*<summary>Filters all peaks. </summary>
           
<remarks>Filters all the peaks within the List
           from the time signal associated with the frequency spectrum
           and recalculates the frequency spectrum.
</remarks>
*)
        procedure FilterAll;
        (*<summary>Called by the Update method, if TraceMethod is ptCurrent. </summary>*)
        procedure Trace; 
        (*<summary>Returns the index of the newly added mark at Freq. </summary>
          
<remarks>Phase and Amplitude are read from the frequency spectrum
          by using the Goertzal algorithm.
</remarks>
*)
        function AddMark(Freq: Double): integer;
      
        
        procedure Assign(Source: TPersistent); override;
        
        constructor Create(AOwner: TSpectrum); virtual;
        destructor Destroy; override;
      
      published
       
        (*<summary>Set this property to false, to disable peak tracing, marking, interpolating and
           filtering when the Update method is called. </summary>*)
        property Active: boolean read FActive write SetActive  default True ;

       
        (*<summary>Defines the peak interpolation parameters. </summary>*)
          
        property Interpolation: TSpectrumPeakInterpolation read FInterpolation write SetInterpolation;

       
        (*<summary>Defines the number of largest peaks to mark, when TraceMethod = ptLargest. </summary>*)
        property LargestCount: integer read FLargestCount write SetLargestCount default 1;

       
        (*<summary>Defines the method used to trace and mark the peaks. </summary>*)
        property TraceMethod: TPeakTraceMethod read FTraceMethod write SetTraceMethod  default ptNone ;

       
        (*<summary>Defines the interval centered around the current position of peak
            in percentage of the bandwidth, within which the new position
            of the peak is to be searched for. </summary>*)
        property TraceInterval: Double read FTraceInterval write SetTraceInterval;

       
        (*<summary>Defines the number of harmonics to be marked, if TraceMethod is ptAmpltHarmonics
           or ptFreqHarmonics. </summary>*)
        property HarmonicsCount: integer read GetHarmonicsCount write SetHarmonicsCount default 10;

       
        (*<summary>Defines the lower frequency that limits the search for peak. </summary>*)
        property BandwidthL: Double read FBandwidthL write SetBandwidthL;

       
        (*<summary>Defines the upper frequency that limits the search for peak. </summary>*)
        property BandwidthH: Double read FBandwidthH write SetBandwidthH;

       
        (*<summary>Set this property to true, to assume that BandwidthL = 0 and BandwidthH equals the Nyquist frequency. </summary>*)
        property UseFullBandwidth: boolean read FUseFullBandwidth write SetUseFullBandwidth  default True ;

       
        (*<summary> Parameters for normalized frequency. </summary>*)
        property NormalizedFreq: TNormalizedFreq read FNormalizedFreq write SetNormalizedFreq;

       
        (*<summary> Parameters for normalized amplitude. </summary>*)
        property NormalizedAmplt: TNormalizedAmplt read FNormalizedAmplt write SetNormalizedAmplt;

       
        (*<summary> Defines the ratio between the largest peak below which, the peak will not be considered. </summary>
        
<remarks>Used when TraceMethod = ptLargest and when calling FindLargestPeaks. Minimum value is 1 and the maximum and default value value is 1E+15.
</remarks>
*)
        property LargestRatio: double read FLargestRatio write SetLargestRatio;
      end;

      

    (*<summary>Encapsulates parameters required by chirp-Z transform. </summary>
      
<remarks>Chirp Z-transform, can evaluate the frequency spectrum
      by starting and ending at any frequency and calculating
      frequency bins with any frequency step.
      Chirp Z-transform is more efficient then FFT, if
      zero padding is required.
</remarks>
*)

    TSpectrumCZT = class(TPersistent)
    strict private
        
        Spectrum: TSpectrumAnalyzer;
        FStartFrequency: Double;
        FFrequencyStep: Double;
        FStopFrequency: Double;
        FStartRadius: Double;
        FSTopRadius: Double;
        procedure SetFrequencyStep(const Value: Double);
        procedure SetStartFrequency(const Value: Double);
        procedure SetStartRadius(const Value: Double);
        procedure SetStopFrequency(const Value: Double);
        procedure SetStopRadius(const Value: Double);
    public
        (*<summary>Create the object and pass TSpectrumAnalyzer as the owner. </summary>*)
        constructor Create(AOwner: TSpectrumAnalyzer); virtual;
    published
       
       (*<summary>Starting normalized frequency (FS = 1) for the Chirp Z-transform. </summary>*)
        property StartFrequency: Double read FStartFrequency write SetStartFrequency;

       
        (*<summary>Ending normalized frequency (FS = 1) for the Chirp Z-transform. </summary>*)
        property StopFrequency: Double read FStopFrequency write SetStopFrequency;

       
        (*<summary>Normalized frequency step (FS = 1) for the Chirp Z-transform. </summary>
         
<remarks>Amplitude and phase will be evaluted from CZTStart to CZTStop in frequency steps by CZTStep.
</remarks>
*)
        property FrequencyStep: Double read FFrequencyStep write SetFrequencyStep;

       
        (*<summary>Defines the starting radius in the Z-plane. </summary>*)
        property StartRadius: Double read FStartRadius write SetStartRadius;

       
        (*<summary>Defines the ending radius in the Z-plane. </summary>*)
        property StopRadius: Double read FStopRadius write SetStopRadius;
    end;

    

    (*<summary>Defines a set of items to be included in spectrum analysis report. </summary>
      
<remarks>Defines what information is to be included in the
      spectral analysis report.
</remarks>
*)
    TSpectrumReportSet = class(TPersistent)
    strict private
      FFrequencyLines: boolean;
      FGeneralInfo: boolean;
      FSFDR: boolean;
      FPhase: boolean;
      FTHD: boolean;
      FMarkedPeaks: boolean;
      FBandsRMS: boolean;
      FNF: boolean;
      FTHDN: boolean;
      FSNR: boolean;
      FSINAD: boolean;
      FDateTime: boolean;
      FRMS: boolean;
      procedure setBandsRMS(const Value: boolean);
      procedure setDateTime(const Value: boolean);
      procedure setFrequencyLines(const Value: boolean);
      procedure setGeneralInfo(const Value: boolean);
      procedure setMarkedPeaks(const Value: boolean);
      procedure setNF(const Value: boolean);
      procedure setPhase(const Value: boolean);
      procedure setRMS(const Value: boolean);
      procedure setSFDR(const Value: boolean);
      procedure setSINAD(const Value: boolean);
      procedure setSNR(const Value: boolean);
      procedure setTHD(const Value: boolean);
      procedure setTHDN(const Value: boolean);
     public
       (*<summary>Returns True, if any of the THD, THDN, NF, SFDR, SINAD, RMS. </summary>*)
       function IncludesStats: boolean;
       (*<summary>Sets all values to false. </summary>*)
       procedure Reset;
     published
       
       (*<summary>Include general info in the report. </summary>
             
<remarks>If true, parameters like SamplingFrequency, Bandwidth,
             number of sampels analyzed etc... will be included in
             the report.
</remarks>
*)
       property GeneralInfo: boolean read FGeneralInfo write setGeneralInfo;

      
       (*<summary>If true, MarkedPeaks will be included in the report. </summary>*)
       property MarkedPeaks: boolean read FMarkedPeaks write setMarkedPeaks;

      
       (*<summary>If true, frequency lines with amplitude will be included in the report. </summary>*)
       property FrequencyLines: boolean read FFrequencyLines write setFrequencyLines;

      
       (*<summary>If true, phase will be included in the report. </summary>*)
       property Phase: boolean read FPhase write setPhase;

      
       (*<summary>If true, Total Harmonic Distortion will be included in the report. </summary>*)
       property THD: boolean read FTHD write setTHD;

      
       (*<summary>If true, Total Harmonic Distortion with noise will be included in the report. </summary>*)
       property THDN: boolean read FTHDN write setTHDN;

      
       (*<summary>If true, Noise Floor will be included in the report. </summary>*)
       property NF: boolean read FNF write setNF;

      
       (*<summary>If true, Spurious Free Dynamic Range will be included in the report. </summary>*)
       property SFDR: boolean read FSFDR write setSFDR;

      
       (*<summary>If true, the RMS of defined frequency ranges (bands) will be included in the report. </summary>*)
       property BandsRMS: boolean read FBandsRMS write setBandsRMS;

      
       (*<summary>If true, Signal to Noise and Distortion will be inlcuded in the report. </summary>*)
       property SINAD: boolean read FSINAD write setSINAD;

      
       (*<summary>If true, RMS of the signal will be inlcuded in the report. </summary>*)
       property RMS: boolean read FRMS write setRMS;

      
       (*<summary>If true, Signal to Noise Ratio will be inlcuded in the report. </summary>*)
       property SNR: boolean read FSNR write setSNR;

      
       (*<summary>If true, Date and time will be inlcuded in the report. </summary>*)
       property DateTime: boolean read FDateTime write setDateTime;
     end;

      

    (*<summary>Defines parameters for generating frequency analysis report. </summary>
      
<remarks>Defines parameters for generating frequency analysis report.
</remarks>
*)

      TSpectrumReport = class(TPersistent)
          strict private
            
            Spectrum: TStaticSpectrumAnalyzer;
            FFrequencyFormat: string;
            FAmplitudeFormat: string;
            FPhaseFormat: string;
            FReportItems: TSpectrumReportSet;
            FDelimiter: string;
            FUseTab: boolean;
            procedure SetReportItems(const Value: TSpectrumReportSet);
            procedure SetAmplitudeFormat(const Value: string);
            procedure SetFrequencyFormat(const Value: string);
            procedure SetPhaseFormat(const Value: string);
            procedure SetDelimiter(const Value: string);
            procedure SetUseTab(const Value: boolean);
          public
           (*<summary>Create spectrum analysis report and store in strings. </summary>*)
           procedure CreateReport(Strings: TStrings); virtual;

           constructor Create(AOwner: TStaticSpectrumAnalyzer); virtual;
           destructor Destroy; override;

         published
          
          
           (*<summary>Defines a set of items to be included in spectrum analysis report. </summary>*)
           property ReportItems: TSpectrumReportSet read FReportItems write SetReportItems;

          
           (*<summary>Defines the floating point number formating applied to frequency values
              in the frequency analysis report. </summary>*)
           property FrequencyFormat: string read FFrequencyFormat write SetFrequencyFormat;

          
           (*<summary>Defines the floating point number formating applied to amplitude values
              in the frequency analysis report. </summary>*)
           property AmplitudeFormat: string read FAmplitudeFormat write SetAmplitudeFormat;

          
           (*<summary>Defines the floating point number formating applied to phase values
              in the frequency analysis report. </summary>*)
           property PhaseFormat: string read FPhaseFormat write SetPhaseFormat;

          
           (*<summary>A good example for the delimiter would be Tab character. </summary>
                      
<remarks>If the delimiter is not used, the columns are padded with space char and aligned to the left.
</remarks>
*)
           property Delimiter: string read FDelimiter write SetDelimiter;
          
           (*<summary>If true, tab is used to separate description of parameter from its value. </summary>
                      
<remarks>The tab will also be used, between the units and the values.
</remarks>
*)
           property UseTab: boolean read FUseTab write SetUseTab default false;
         end;

      
     (*<summary>Encapsulates properties for averaging and confidence interval calculation of the spectrum. </summary>
                
<remarks>Encapsulates properties for averaging and confidence
                interval calculation of the spectrum.
</remarks>
*)
      TSpectrumStats = class(TPersistent)
      strict private
        Work: TVecList;
        
        Spectrum: TSpectrumAnalyzer;
        FCalcStdDev: boolean;
        FCalcLimits: boolean;
        FAverages: integer;
        FAveraged: integer;
        FConfidenceInterval: Double;
        FExpDecay: integer;
        FAveraging: TAveraging;
        FStdDev: TVecList;
        FLowerLimit: TVecList;
        FUpperLimit: TVecList;
        FAveragedSpectrum: TSpectrum;
        XLimitValue: Double;
        FRecursive: boolean;
        FCalcMinMax: boolean;
        procedure SetAveraged(const Value: integer);
        procedure SetAverages(const Value: integer);
        procedure SetAveraging(const Value: TAveraging);
        procedure SetCalcLimits(const Value: boolean);
        procedure SetCalcStdDev(const Value: boolean);
        procedure SetConfidenceInterval(const Value: Double);
        procedure SetExpDecay(const Value: integer);
        procedure SetLowerLimit(const Value: TVecList);
        procedure SetStdDev(const Value: TVecList);
        procedure SetUpperLimit(const Value: TVecList);
        procedure SetAveragedSpectrum(const Value: TSpectrum);
        procedure UpdateNonRecursive;
        procedure UpdateRecursive;
        procedure SetRecursive(const Value: boolean);
        procedure SetCalcMinMax(const Value: boolean);
     public
       property AveragedSpectrum: TSpectrum read FAveragedSpectrum write SetAveragedSpectrum;

       constructor Create(AOwner: TSpectrumAnalyzer); virtual;
       destructor Destroy; override;

       (*<summary>Recalculates limits considering the current value of the ConfidenceInterval.</summary>
                  
<remarks>Works only, if Recursive property is false and StdDev is available. Works for amplitude only.
</remarks>
*)
        procedure UpdateStatsAmpltLimits;
       (*<summary>Apply averaging. Called by TSpectrumAnalyzer. </summary>*)
       procedure Update; virtual;
       (*<summary>Computes average and standard deviations of the spectrums optionally
          with upper and lower confidence levels, if Recursive property was false
          while spectrums were added. </summary>*)
       procedure UpdateStats; virtual;
       (*<summary>Recalculates limits considering the current value of the ConfidenceInterval.</summary>
                  
<remarks>Works only, if Recursive property is false and StdDev is available.  Works for both phase and amplitude.
</remarks>
*)
        procedure UpdateStatsLimits;
       (*<summary>Reset the averaging process and deallocate any associated memory. </summary>*)
       procedure Reset;
       (*<summary>Returns true, if confidence intervals have been computed and are available. </summary>*)
       function ConfidenceLimitsPresent: boolean;
     published
       
       (*<summary>Holds a list of vectors with upper confidence interval limits. </summary>
          
<remarks>The first index holds the amplitude and the second the phase
          related data. For description of vectors see TVec.Caption property.
</remarks>
*)
       property UpperLimit: TVecList read FUpperLimit write SetUpperLimit stored false;

       
       (*<summary>Holds a list of vectors with lower confidence interval limits. </summary>
          
<remarks>For description of vectors see TVec.Caption property. (example: LowerLimit[i].Caption)
</remarks>
*)
       property LowerLimit: TVecList read FLowerLimit write SetLowerLimit stored false;

       
       (*<summary>The parameter for the exponential averaging. </summary>*)
       property ExpDecay: integer read FExpDecay write SetExpDecay default 5;

       
       (*<summary>If CalcStdDev is True and frequency spectrum
          has been averaged, this property contains the standard deviation
          of Amplt and Phase vectors. </summary>
          
<remarks>Standard deviation is statistically
          meaningfull only if linear averaging is used. For description
          of vectors see TVec.Caption property. The vector count should
          not be altered. The first index holds the stddev of amplitude
          and the second index the stddev of the phase.
</remarks>
*)
       property StdDev: TVecList read FStdDev write SetStdDev stored false;

       
       (*<summary>If True, the averaging process will also compute the standard deviation for each spectral line. </summary>*)
       property CalcStdDev: boolean read FCalcStdDev write SetCalcStdDev  default false ;

       
       (*<summary>If True, the averaging process will also compute the standard deviation. </summary>*)
       property CalcMinMax: boolean read FCalcMinMax write SetCalcMinMax;

       
       (*<summary>If True, the upper and lower limits of the confidence interval will
          be calculated and placed in UpperLimit and LowerLimit. </summary>*)
       property CalcLimits: boolean read FCalcLimits write SetCalcLimits;

       
      (*<summary>Defines the confidence interval in percent to determine the upper and lower confidence limit. </summary>
         
<remarks>If ConfidenceInterval is 99 then the certainty
         that all values for that specific frequency will fall between
         upper and lower limit is 99%.
</remarks>
*)
       property ConfidenceInterval: Double read FConfidenceInterval write SetConfidenceInterval;

       
       (*<summary> Contains the number of averages made so far. </summary>*)
       property Averaged: integer read FAveraged write SetAveraged  default 0 ;

       
       (*<summary>Defines the number of averages to make. </summary>
         
<remarks>When Averaged &gt;= Averages and finite averaging mode is
         defined with Averaging property, the call to the Update will do nothing.
</remarks>
*)
       property Averages: integer read FAverages write SetAverages  default 30 ;

       
       (*<summary>Defines the type of averaging to perform. </summary>*)
       property Averaging: TAveraging read FAveraging write SetAveraging  default avNone ;

       
       (*<summary>If True the result is computed each time a new spectrum is added. </summary>
          
<remarks>If the value is false, UpdateStats has to be called to obtain
          statistics, once desired number of spectrums have been accumulated.
</remarks>
*)
       property Recursive: boolean read FRecursive write SetRecursive  default True ;
     end;

     (*<summary>Defined to support storing and loading the spectrum descriptor to/from Streams. </summary>
        
<remarks>Defined to support storing and loading the spectrum descriptor to/from Streams.
</remarks>
*)
      TSpectrumDescriptorRecord = packed record
        SpectrumMethod: integer;
        Integration: integer;
        Logarithmic: integer;
        Averages: integer;
        AveragingType: integer;
        SamplingTime: double;
        SamplingFrequency: double;
        ArOrder: integer;
        Window: integer;
        ScaleFactor: double;
        SpectrumType: integer;
        SidelobeAtt: double;
        ActualZeroPadding: double;
      end;

      (*<summary>Stores the description of the spectrum. </summary>
        
<remarks>When a frequency spectrum is saved for example to a
        database it makes sense to also store the data describing how
        that spectrum was computed. The object is used by
        spectrum analyzers to store the description of processing
        applied.
</remarks>
*)
      TSpectrumDescriptor = class(TPersistent)
      strict protected
        FSpectrumMethod: TSpectrumMethod;
        FIntegration: TIntegration;
        FLogarithmic: boolean;
        FAverages: integer;
        FAveragingType: TAveraging;
        FSamplingTime: Double;
        FSamplingFrequency: Double;
        FArOrder: integer;
        FWindow: TSignalWindowType;
        FScaleFactor: Double;
        FSpectrumType: TSpectrumType;
        FSidelobeAtt: Double;
        FActualZeroPadding: Double;
        procedure SetArOrder(const Value: integer);
        procedure SetAverages(const Value: integer);
        procedure SetAveragingType(const Value: TAveraging);
        procedure SetIntegration(const Value: TIntegration);
        procedure SetLogarithmic(const Value: boolean);
        procedure SetSamplingFrequency(const Value: Double);
        procedure SetSamplingTime(const Value: Double);
        procedure SetScaleFactor(const Value: Double);
        procedure SetSidelobeAtt(const Value: Double);
        procedure SetSpectrumMethod(const Value: TSpectrumMethod);
        procedure SetSpectrumType(const Value: TSpectrumType);
        procedure SetWindow(const Value: TSignalWindowType);
        procedure SetActualZeroPadding(const Value: Double);
      public
        (*<summary>Sampling frequency of the time signal. </summary>*)
        property SamplingFrequency: Double read FSamplingFrequency write SetSamplingFrequency;
        (*<summary>Sampling time of the time signal. </summary>*)
        property SamplingTime: Double read FSamplingTime write SetSamplingTime;
        (*<summary>Number of averages taken if any. </summary>*)
        property Averages: integer read FAverages write SetAverages;
        (*<summary>Averaging method used, if averaging was performed. </summary>*)
        property AveragingType: TAveraging read FAveragingType write SetAveragingType;
        (*<summary>Type of the spectrum computed. </summary>*)
        property SpectrumType: TSpectrumType read FSpectrumType write SetSpectrumType;
        (*<summary>Method used to compute the spectrum. </summary>*)
        property SpectrumMethod: TSpectrumMethod read FSpectrumMethod write SetSpectrumMethod;
        (*<summary>Any window methods used to window the time signal. </summary>*)
        property Window:  TSignalWindowType read FWindow write SetWindow;
        (*<summary>Scale factor by which the spectrum has been scaled. </summary>*)
        property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
        (*<summary>Integration/differentiation methods applied. </summary>*)
        property Integration: TIntegration read FIntegration write SetIntegration;
        (*<summary>True if the spectrum in logarithmic scale. </summary>*)
        property Logarithmic: boolean read FLogarithmic write SetLogarithmic;
        (*<summary>AR order used if autoregressive spectral methods were applied. </summary>*)
        property ArOrder: integer read FArOrder write SetArOrder;
        (*<summary>Kaiser sidelobe attenuation if used. </summary>*)
        property SidelobeAtt: Double read FSidelobeAtt write SetSidelobeAtt;
        (*<summary>Holds the number of zeroes added to the signal prior to
           applying the frequency transformation. </summary>*)
        property ActualZeroPadding: Double read FActualZeroPadding write SetActualZeroPadding;
        
        (*<summary>Save the data to the Dst Stream. </summary>*)
        procedure SaveToStream(Dst: TStream);  overload; virtual;
        (*<summary>Load the data from the Src Stream. </summary>*)
        procedure LoadFromStream(Src: TStream); overload; virtual;

        procedure Assign(Source: TPersistent); override;

        

        
        procedure AssignToRecord(var Dst: TSpectrumDescriptorRecord);
        procedure AssignFromRecord(const Src: TSpectrumDescriptorRecord);
      end;

    

     (*<summary>Analyzes a frequency spectrum. </summary>
       
<remarks>Use this component to analyze precomputed frequency spectrum.
       Spectrum that was stored to a database or an averaged
       frequency spectrum. The component features abilities to
       mark peaks, interpolate peaks, trace peaks, compute RMS
       of bands and create reports describing the spectrum.
       <para/>
       The amplitude spectrum should be placed in the Amplt property
       and the phase spectrum in the Phase property. The component
       supports analyzing the frequency spectrum in both polar
       and cartesian coordinates.
</remarks>
*)

        TStaticSpectrumAnalyzer = class(TSpectrum)
        strict protected
          FPeaks: TSpectrumPeaksAnalyzer;
          FReport: TSpectrumReport;
          FBands: TSpectrumBandsAnalyzer;
          FDescriptor: TSpectrumDescriptor;
          FOnPeaksUpdate: TNotifyEvent;
          FOnBandsUpdate: TNotifyEvent;
          procedure SetBands(const Value: TSpectrumBandsAnalyzer);
          procedure SetPeaks(const Value: TSpectrumPeaksAnalyzer);
          procedure SetReport(const Value: TSpectrumReport);
          procedure SetOnBandsUpdate(const Value: TNotifyEvent);
          procedure SetOnPeaksUpdate(const Value: TNotifyEvent);
        strict protected
          function InternalUpdate: TPipeState; override;
          function CreateSpectrumReport: TSpectrumReport; virtual;
        public
          property Descriptor: TSpectrumDescriptor read FDescriptor;

          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;

        published
          
          
          (*<summary>Holds the parameters needed to generate a  spectrum analysis report.
             Requires that the descriptor property is properly filled up. </summary>*)
          property Report: TSpectrumReport read FReport write SetReport;
          
          
          (*<summary>Pointer to the component that manages peak analysis. </summary>*)
          property Peaks: TSpectrumPeaksAnalyzer read FPeaks write SetPeaks;
          
          
          (*<summary>Holds the parameters for the frequency bands. </summary>*)
          property Bands: TSpectrumBandsAnalyzer read FBands write SetBands;
          (*<summary>If assigned, this event will be called instead of a call to Peaks.Update method. </summary>*)
          property OnPeaksUpdate: TNotifyEvent read FOnPeaksUpdate write SetOnPeaksUpdate;
          (*<summary>If assigned, this event will be called instead of a call to Bands.Update method. </summary>*)
          property OnBandsUpdate: TNotifyEvent read FOnBandsUpdate write SetOnBandsUpdate;
        end;

      

     (*<summary>Manages a list of TStaticSpectrumAnalyzer objects. </summary>
       
<remarks>The component overrides the AddItem
       method and declares a new default array property returning TStaticSpectrumAnalyzer type.
</remarks>
*)

      TStaticSpectrumAnalyzerList = class(TAnalysisList)
      strict private
          function GetItems(index: integer): TStaticSpectrumAnalyzer; reintroduce;
          procedure SetItems(index: integer; const Value: TStaticSpectrumAnalyzer);
      strict protected
          
          function AddItem: TMtxComponent; override;
      public

         
         constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSignal objects by Index. </summary>*)
          property Items[index: integer]: TStaticSpectrumAnalyzer read GetItems write SetItems; default; 
      end;


      

        (*<summary>Generic spectrum analyzer class. </summary>
          
<remarks>The class is used as the basis for analyzing  frequency spectrums.
</remarks>
*)
        TCustomSpectrumAnalyzer = class(TStaticSpectrumAnalyzer)
        strict protected
          FLdBSpan: double;
          FIntegration: TIntegration;
          FLogarithmic: boolean;
          FRotation: integer;
          FLogType: TLogType;
          FScaleFactor: Double;
          FLogSpan: TLogSpan;
          FSpectrumType: TSpectrumType;
          FLogBase: double;
          FLogScale: double;
          procedure SetLogarithmic(const Value: boolean);
          procedure SetLogSpan(const Value: TLogSpan);
          procedure SetLogType(const Value: TLogType);
          procedure SetRotation(const Value: integer);
          procedure SetScaleFactor(const Value: Double);
          procedure SetLogBase(const Value: double);
          procedure SetLogScale(const Value: double);
        strict protected
          procedure SetSpectrumType(const Value: TSpectrumType); virtual;
        strict protected
          procedure SetIntegration(const Value: TIntegration); virtual;
        public
          property LdBSpan: double read FLDBSpan;
          constructor Create(AOwner:TComponent); override;
          procedure IntegrateSpectrum;
        published

          
          (*<summary>Define the integration/differenation of the frequency spectrum. </summary>*)
          property Integration: TIntegration read FIntegration write SetIntegration  default inNone ;

          
          (*<summary>Specify how to apply the logarithm to the spectrum. </summary>*)
          property LogType: TLogType read FLogType write SetLogType  default ltAbsolute ;

          
          (*<summary>Set it to True, to obtain the logarithm of the spectrum in dB. </summary>*)
          property Logarithmic: boolean read FLogarithmic write SetLogarithmic  default False ;

          
          (*<summary>Defines the span of the logarithmic spectrum in dB. </summary>*)
          property LogSpan: TLogSpan read FLogSpan write SetLogSpan  default ls120 ;

          
          (*<summary>Defines the base of the logarithmic spectrum. </summary>
                     
<remarks>Considered, when LogType is not dB. If this value is 10 and LogScale is 20, the result will be in dB.
                     Default value is 10.
</remarks>
*)
          property LogBase: double read FLogBase write SetLogBase;
          (*<summary>Defines the scaling of the logarithmic spectrum. </summary>
                     
<remarks>Considered, when LogType is not dB. If this value is 10 and LogScale is 20, the result will be in dB.
                     Default value is 20.
</remarks>
*)
          property LogScale: double read FLogScale write SetLogScale;

          
          (*<summary>Frequency spectrum will be scaled by a scale factor. </summary>*)
          property ScaleFactor: Double read FScaleFactor write SetScaleFactor;

          
          (*<summary>Before the frequency analysis, the signal can be circularly rotated by a specified
             number of samples. Rotation defines the number of samples. </summary>
             
<remarks>The direction can be positive or negative.
</remarks>
*)
          property Rotation: integer read FRotation write SetRotation default 0;

          
          (*<summary>Specifies the spectrum type to be computed. </summary>*)
          property SpectrumType: TSpectrumType read FSpectrumType write SetSpectrumType  default spAmplt ;
        end;










        

        TStoredSpectrumAnalyzer = class(TCustomSpectrumAnalyzer)
        strict protected
          StoredAmplt: TVec;
          StoredPhase: TVec;
          FStoredSpectrumType: TSpectrumType;
          FStoredIntegration: TIntegration;
          FStoredLogarithmic: boolean;
          procedure SetStoredIntegration(const Value: TIntegration);
          procedure SetStoredLogarithmic(const Value: boolean);
          procedure SetStoredSpectrumType(const Value: TSpectrumType);
          function GetInput2: TSpectrum;
          procedure SetInput2(const Value: TSpectrum);
        strict protected
          function InternalUpdate: TPipeState; override;
          function CreateInputs: TAnalysisConnectorsCollection; override;
        public
      
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
     
        published
          (*<summary>Specifies the SpectrumType of the stored Spectrum. </summary>
             
<remarks>This parameter is used to convert the spectrum to other
             spectrum types.
</remarks>
*)
          property StoredSpectrumType: TSpectrumType read FStoredSpectrumType write SetStoredSpectrumType;
          (*<summary>Specifies the Logarithmic flag of the stored Spectrum. </summary>
             
<remarks>This parameter is used to compute linear and/or logarithmic spectrum.
</remarks>
*)
          property StoredLogarithmic: boolean read FStoredLogarithmic write SetStoredLogarithmic;
          (*<summary>Specifies the Integration method of the stored Spectrum. </summary>*)
          property StoredIntegration: TIntegration read FStoredIntegration write SetStoredIntegration;
          (*<summary>Specifes the Input spectrum for the analysis component. </summary>*)
          property Input: TSpectrum read GetInput2 write SetInput2 stored InputsStored; 
        end;



    

    


    (*<summary>Computes and analyzes a frequency spectrum. </summary>
       
<remarks>Use this component
       to estimate frequency spectrum with a wide range
       of different methods, average frequency spectrums,
       mark and handle peaks, compute statistics from
       selected frequency bands and more... The resulting
       spectrum is always placed in Self.Amplt (amplitude
       spectrum) and Self.Phase (phase spectrum).
</remarks>
*)

    TSpectrumAnalyzer = class(TCustomSpectrumAnalyzer)
        strict protected
          FWindowInitialized: boolean;
          FSpectrumScale: Double;
          PaddedLength: integer;
          FWindowApplyScale: Double;
          FDCDump: boolean;       
          FWindow: TSignalWindowType;
          FSidelobeAtt: Double;
          WindowNorm: Double;
          FExpWindow: Double;
          FMethod: TSpectrumMethod;
          FARORder: integer;
          FOnAfterComplexSpectrum: TNotifyEvent;
          FOnAfterAverage: TNotifyEvent;
          FOnBeforeAverage: TNotifyEvent;
          FCrossSpectral: boolean;
          FOnBeforeSpectrumType: TNotifyEvent;
          FCZT: TSpectrumCZT;
          FUnwrapPhase: boolean;
          FRunningPhase: TRunningPhase;
          FActualZeroPadding: Double;
          FStats: TSpectrumStats;
          FLogMaxAmplt: Double;
          procedure SetDCDump(const Value: boolean);
          procedure SetWindowType(const Value: TSignalWindowType);
          procedure SetSidelobeAtt(const Value: Double);
          procedure ApplyWindow(Index, Len: integer);
          function  WindowScaleFactor: Double;
          procedure SetExpWindow(const Value: Double);
          procedure SetMethod(const Value: TSpectrumMethod);
          procedure SetARORder(const Value: integer);
          procedure CheckPeaks;
          procedure SetOnAfterAverage(const Value: TNotifyEvent);
          procedure SetOnAfterComplexSpectrum(const Value: TNotifyEvent);
          procedure SetOnBeforeAverage(const Value: TNotifyEvent);
          procedure SetCrossSpectral(const Value: boolean);
          procedure SetOnBeforeSpectrumType(const Value: TNotifyEvent);
          procedure SeTSpectrumCZT(const Value: TSpectrumCZT);
          procedure SetUnwrapPhase(const Value: boolean);
          procedure SetRunningPhase(const Value: TRunningPhase);

          procedure SetActualZeroPadding(const Value: Double);
          procedure SetStats(const Value: TSpectrumStats);
        strict protected
          function  GetOutput: TSignal;
          procedure SetOutput(const Value: TSignal);
 strict protected 
           class  function EditorClass: string; override;
        strict protected
          FInishedAveraging: boolean;
          FSupportComplexSpectra: boolean; 
          procedure SetSpectrumType(const Value: TSpectrumType); override;
 protected 
          
          property Output: TSignal read GetOutput write SetOutput; 
        strict protected

          InputTemp, CepstralInput: TSignal;
          OutputTemp: TSignal;
          OrderApplied: boolean;
          SumSqrSpectrum: TSpectrum;
          TempSpectrum: TSpectrum;
          TempData, TempData2, TempData3, WindowData: TVec;
          cztState: TCztState;

          function GetTempData: TVec;
          function  InternalUpdate: TPipeState; override;
          procedure BeforeInternalUpdate; override;
          procedure ApplySpectrumMethod(TargetLength: integer; Dst: TVec = nil); virtual;
          procedure ApplyPhaseRange; virtual;
          procedure SetZeroPadding(const Value: integer); override;
          procedure InitPeakAnalysis; 
          procedure EnableStatistics; 
          function  StatisticsIsValid: boolean; 
          procedure ComputeGroupDelay; override;
          procedure ComputePhaseDelay; override;
          procedure ApplyPhaseSpectrum; virtual;
          function FetchResult(Index: integer; Data: TVec): boolean; virtual;
          procedure SetIntegration(const Value: TIntegration); override;
          procedure UpdateLogMaxAmplt; virtual;
       public
          property Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored; 
          property SupportComplexSpectra: boolean read FSupportComplexSpectra;
          property LogMaxAmplt: double read FLogMaxAmplt;

          
          (*<summary>Ready only. Returns the ratio between the original signal
             length and the length of the analyzed signal padded with zeros. </summary>*)
          property ActualZeroPadding: Double read FActualZeroPadding write SetActualZeroPadding;
          (*<summary>The scaling factor applied to the frequency spectrum. </summary>
             
<remarks>This scaling factor includes ScaleFactor, the scaling
             due to zero padding, scaling due to window used and scaling due to frequency spectrum length.
</remarks>
*)
          property SpectrumScale: Double read FSpectrumScale write FSpectrumScale;
          (*<summary>After the first call to Update method, this
             property returns a pointer to the scaled complex spectrum. </summary>*)
          property ComplexSpectrum: TVec read GetTempData;
          (*<summary>Returns the amplitude of a peak nearest to the Freq frequency
             according to the value of the Peaks property. </summary>
             
<remarks>It takes in to account the scaling factors, spectrum type,
             Integration property and Logarithmic flag.
</remarks>
*)
          function  AmpltEst(Freq: Double): Double; overload; override;
          (*<summary>Returns the amplitude of a peak nearest to the Freq frequency
             according to the value of the Peaks property. </summary>
             
<remarks>It takes in to account the scaling factors, spectrum type,
             Integration property and Logarithmic flag.
</remarks>
*)
          function  AmpltEst(Freq: Double; const SpcFreqBin: TCplx): Double; overload; override;
          (*<summary>Returns the phases of the amplitudes at Freq frequencies
             according to the value of the Peaks property. </summary>
             
<remarks>SrcSpectrum must contain complex spectrum estimated at frequencies defined in Freq.
</remarks>
*)
          procedure AmpltEst(SrcFreq, SrcSpectrum, DstAmplt: TVec); overload; override;
          (*<summary>Returns the phase of the frequency spectrum at Freq frequency according to the parameters of the Peaks property. </summary>*)
          function PhaseEst(Freq: Double): Double; overload; override;
          (*<summary>Returns the phase of the frequency spectrum at Freq frequency according to the parameters of the Peaks property. </summary>*)
          function PhaseEst(Freq: Double; const SpcFreqBin: TCplx): Double; overload; override;
          (*<summary>Converts the amplitude of the frequency spectrum at Freq frequency to dB according to the value of the Peaks property. </summary>*)
          procedure AmpltLogEst(var Amplitude: Double; Freq: Double); overload;
          (*<summary>Converts the amplitude of the frequency spectrum at Freq frequency to dB according to the value of the Peaks property. </summary>
                     
<remarks>Vectorized version.
</remarks>
*)
          procedure AmpltLogEst(const Amplitude: TVec; const Freq: TVec); overload;
          (*<summary>Returns the complex value of the Freq frequency according to the value of the Peaks property. </summary>
             
<remarks>This function will work only, if Method = smFFT.
</remarks>
*)
          function  ComplexEst(Freq: Double): TCplx; overload; virtual;
          (*<summary>Returns the complex value of the Freq frequency according to the value of the Peaks property. </summary>
             
<remarks>This function will work only, if Method = smFFT.  When peak interpolation method is other than imNumeric, then the spcEstimate1 and spcEstimate2
             will contain only the value of the nearest spectral peak.  This function takes equal amount of time as the variant taking only one Freq parameter
             and is thus two times faster.
</remarks>
*)
          procedure ComplexEst(Freq1, Freq2: Double; var spcEstimate1, spcEstimate2: TCplx); overload; virtual;
          (*<summary>Returns the complex value of the Freq frequencies according to the value of the Peaks property. </summary>
             
<remarks>This function will work only, if Method = smFFT.
</remarks>
*)
          procedure ComplexEst(Freq, aResult: TVec); overload; virtual;
          (*<summary>Compute the spectrum from Input.Data according to the state of the component and place the result in Dst. </summary>*)
          procedure Spectrum(Dst: TVec);
          (*<summary>Find the peak nearest to the Freq frequency and return the frequenyc of the newly found peak. </summary>*)
          function PeakApproximate(Freq: Double): Double; overload; override;
          (*<summary>Find the peak nearest to the Freq frequency and return the frequenyc of the newly found peak. </summary>
                    
<remarks>This overload returns also spcEstimate, which is the spectral line of the complex spectrum at approximated Freq.
                    Only the numeric peak interpolation will return something else that the nearest peak for spcEstimate.
                    This spcEstimate can then be passed on to the AmpltEst, which accepts also the complex parameter.
</remarks>
*)
          function PeakApproximate(Freq: Double; var spcEstimate: TCplx): Double; overload;
          (*<summary>Filter the peak defined with a TMarkRecord record stored at position
             Index within Marks list from Input.Data. </summary>
             
<remarks>The method will use the frequency, amplitude and phase
             stored in the TMarkRecord to subtract a sine wave of
             the same frequency, amplitude and phase from
             the signal connected to the Input property.
</remarks>
*)
          procedure FilterPeak(Index: integer); override;
          (*<summary>Compute the frequency spectrum where the Num is the numerator and
             Den is the denominator of the transfer function. </summary>
             
<remarks>FS defines the sampling frequency. The result is placed in Self.
</remarks>
*)
          procedure ComputeSpectrum(Num: TVec; Den: TVec = nil; FS: Double = 2); virtual;
          (*<summary>Compute the frequency spectrum where the SrcNum is the numerator and
             SrcDen is the denominator of the transfer function. </summary>
             
<remarks>The resulting amplitude is placed in AmpltDst and resulting
             phase in PhaseDst. You can pass nil for SrcDen, if not present.
             If you want the result to be placed in self pass nil for AmpltDst and PhaseDst also.
</remarks>
*)
          procedure Process(SrcNum: TVec; SrcDen: TVec = nil; AmpltDst: TVec = nil; PhaseDst: TVec = nil; FS: Double = 2); virtual;
          (*<summary>Reset the frequency spectrum averaging. </summary>*)
          procedure ResetAveraging; virtual;
          (*<summary>If Stats.Recursive was False, calling this method will compute the averaged
             spectrum and associated statistics. </summary>
             
<remarks>The predicted workflow is like this. First you set Stats.Recursive to True and
             specify for example Stats.Averaging := avLinear.
             Then you compute multiple frequency spectrums by calling Update or Pull method.
             Once sufficient number of averages has been obtained (Stats.Averaged),
             call UpdateSpectrumStats method to compute averaged frequency spectrum,
             optional standard deviation and confidence intervals.

             If Stats.Recursive was True, the averaged spectrum, standard deviation
             and confidence intervals were evaluated for each newly computed frequency
             spectrum.

             Consequently if the data is processed off-line, it makes more sense to set
             Stats.Recursive to false, and if live data is being observed like signal comming
             from the microphone, it makes more sense, to leave the Stats.Recursive property
             set to True. The speed of processing can be as much as 10x lower, if Recursive
             is True, standard deviation is required and confidence intervals are computed,
             because all the stats is evaluated for every iteration.
</remarks>
*)
          procedure UpdateSpectrumStats;

          (*<summary>Call the method after changing the Logarithmic property. </summary>
             
<remarks>It will apply logarithmic amplitude axis on averaged or
             not averaged signal. It allows switching Logarithmic axis
             (on/off) without averaging the signal again. If the signal
             is not averaged, the method defaults to a call to the Update
             method.
</remarks>
*)
          procedure UpdateLogarithmic; virtual;

          function IsNumericalInterpolationSupported: boolean; override;

  
          
          procedure SaveToStream(Stream: TStream); override;
          procedure LoadFromStream(Stream: TStream); override;
          

          

          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
          
  

       published
          
          property  Input: TSignal read GetInput1 write SetInput1  stored InputsStored ; 

         
          (*<summary>If True, the conjugate symmetric part of the frequency spectrum
             will be placed in 0.5..1.0 Fs (as returned by the FFT),
             and not between -0.5..0.0 Fs.</summary>
             
<remarks>The conjugate symmetic part of the frequency spectrum from the real time
             series can be placed before or after its original.)
             The property is used in derived classes.
</remarks>
*)
          property ConjFlip: boolean read FConjFlip write SetConjFlip  default True ; 
          
          (*<summary>If True, the conjugate symmetric part of the frequency spectrum
             is to be computed.</summary>
             
<remarks>The property is used in derived classes.
             The conjugate symmetic part of teh frequency spectrum
             can be used to detect aliasing, if you search for harmonics.
</remarks>
*)
          property ConjExtend: boolean read FConjExtend write SetConjExtend  default False ; 

         
          (*<summary>If True, calculate the cross spectrum and not the transfer function. </summary>
            
<remarks>The transfer function is computed on each call to update. To obtain reliable
            estimate enable averaging. The phase spectrum will also be valid.
</remarks>
*)
          property CrossSpectral: boolean read FCrossSpectral write SetCrossSpectral  default False ; 

         
          (*<summary>Defines the order for the autogressive spectral estimators. </summary>*)
          property ArOrder: integer read FARORder write SetAROrder default 100 ;

         
          (*<summary>Defines the parameters for the averaging and statistical analysis. </summary>*)
          property Stats: TSpectrumStats read FStats write SetStats;

          
          (*<summary>Holds Chirp Z-transform configuration. </summary>*)
          property CZT: TSpectrumCZT read FCZT write SetSpectrumCZT;

         
          (*<summary>If True, the DC component will be removed from the signal prior
             to the transform to the frequency domain. </summary>
             
<remarks>This can vastly improve the resolution of the frequency spectrum.
</remarks>
*)
          property DCDump: boolean read FDCdump write SetDCDump  default True ;

         
          (*<summary>The parameter for the exponential windowing function. </summary>*)
          property ExpWindow: Double read FExpWindow write SetExpWindow;

         
          (*<summary>Defines the method to be used for frequency spectrum estimation. </summary>*)
          property Method: TSpectrumMethod read FMethod write SetMethod  default smFFT ;

         
          (*<summary>If True, the phase of the phase spectrum will be "unwrapped". </summary>
             
<remarks>The quality of the unwrapp will strongly depend upon the factor
             of zero padding. Zero padding factor of 32 is recomended.
</remarks>
*)
          property UnwrapPhase: boolean read FUnwrapPhase write SetUnwrapPhase  default false ;

         
          (*<summary>Defines how to remove the constant (running) phase from the phase spectrum. </summary>*)
          property RunningPhase: TRunningPhase read FRunningPhase write SetRunningPhase  default rpConstPhase ;

         
          (*<summary>Specifies the attenuation of the main sidelobe in dB. This parameter applies
             only, if you use the Kaiser or Chebyshev window. </summary>*)
          property SidelobeAtt: Double read FSidelobeAtt write SetSidelobeAtt;

         
          (*<summary>Specifies the window type to be used. Windows decrease spectral leakage, but also
             decrease spectral resolution. </summary>*)
          property Window: TSignalWindowType read FWindow write SetWindowType  default wtRectangular ;

          (*<summary>Event triggered just after complex spectrum has been computed, but
             before averaging in the complex domain. </summary>*)
          property OnAfterComplexSpectrum: TNotifyEvent read FOnAfterComplexSpectrum write SetOnAfterComplexSpectrum;
          (*<summary>Event triggered just before spectrum type is aplied, but after
             averaging in the complex domain. </summary>*)
          property OnBeforeSpectrumType: TNotifyEvent read FOnBeforeSpectrumType write SetOnBeforeSpectrumType;
          (*<summary>Event triggered just before the average is applied. </summary>*)
          property OnBeforeAverage: TNotifyEvent read FOnBeforeAverage write SetOnBeforeAverage;
          (*<summary>Event triggered just after the average has been applied. </summary>*)
          property OnAfterAverage: TNotifyEvent read FOnAfterAverage write SetOnAfterAverage;
       end;

     
    (*<summary>Defines a set of items to be included in signal analysis report. </summary>
      
<remarks>Defines a set of items to be included in signal analysis report.
</remarks>
*)
     TSignalReportSet = class(TPersistent)
     strict protected
        FMean: boolean;
        FCrest: boolean;
        FSkewness: boolean;
        FGeneralInfo: boolean;
        FSignalValues: boolean;
        FKurtosis: boolean;
        FMinMax: boolean;
        FMarkedValues: boolean;
        FPeak: boolean;
        FRMS: boolean;
        FDateTime: boolean;
        FStdDev: boolean;
        procedure setCrest(const Value: boolean);
        procedure setDateTime(const Value: boolean);
        procedure setGeneralInfo(const Value: boolean);
        procedure setKurtosis(const Value: boolean);
        procedure setMarkedValues(const Value: boolean);
        procedure setMean(const Value: boolean);
        procedure setMinMax(const Value: boolean);
        procedure setPeak(const Value: boolean);
        procedure setRMS(const Value: boolean);
        procedure setSignalValues(const Value: boolean);
        procedure setSkewness(const Value: boolean);
        procedure setStdDev(const Value: boolean);
     public
        function IncludesStats: boolean;
     published
       
       (*<summary>If True, General information like SamplingFrequency, Bandwidth,
             number of sampels analyzed etc... will be included in the report. </summary>*)
       property GeneralInfo: boolean read FGeneralInfo write setGeneralInfo  default false ;

      
       (*<summary>If True, values that were marked will be included in the report. </summary>*)
       property MarkedValues: boolean read FMarkedValues write setMarkedValues  default false ;

      
       (*<summary>If True, all values of the signal will be included in the report. </summary>*)
       property SignalValues: boolean read FSignalValues write setSignalValues  default false ;

      
       (*<summary>If True, date and time will be included in the report. </summary>*)
       property DateTime: boolean read FDateTime write setDateTime  default false ;

      
       (*<summary>If True the mean value of the signal will be included in the report. </summary>*)
       property Mean: boolean read FMean write setMean  default false ;

      
       (*<summary>If True, standard deviation of the signal will be included in the report. </summary>*)
       property StdDev: boolean read FStdDev write setStdDev  default false ;

      
       (*<summary>If True, RMS of the signal will be included in the report. </summary>*)
       property RMS: boolean read FRMS write setRMS  default false ;

      
       (*<summary>If True, the value furthest from the mean of the signal will be
          included in the report. </summary>*)
       property Peak: boolean read FPeak write setPeak  default false ;

      
       (*<summary>If True, CREST parameter will be included in the report. </summary>*)
       property Crest: boolean read FCrest write setCrest  default false ;

      
       (*<summary>If True, Skewness of the signal will be included in the report. </summary>*)
       property Skewness: boolean read FSkewness write setSkewness  default false ;

      
       (*<summary>If True, Kurtosis of the signal will be included in the report. </summary>*)
       property Kurtosis: boolean read FKurtosis write setKurtosis  default false ;

      
       (*<summary>If True, minimum and maximmum of the signal will be included in the report. </summary>*)
       property MinMax: boolean read FMinMax write setMinMax  default false ;
     end;


       TSignalAnalyzer = class;

      

    (*<summary>Defines parameters for generating signal analysis report. </summary>
      
<remarks>Defines parameters for generating signal analysis report.
</remarks>
*)

      TSignalReport = class(TPersistent)
          strict protected
            
            Signal: TSignalAnalyzer;
            FTimeFormat: string;
            FAmplitudeFormat: string;
            FReportItems: TSignalReportSet;
            FDelimiter: string;
            FUseTab: boolean;
            procedure SetReportItems(const Value: TSignalReportSet);
            procedure SetAmplitudeFormat(const Value: string);
            procedure SetTimeFormat(const Value: string);
            procedure SetDelimiter(const Value: string);
            procedure SetUseTab(const Value: boolean);
          public
           (*<summary>Create spectrum analysis report and
              store in Strings. </summary>*)
           procedure CreateReport(Strings: TStrings);

           constructor Create(AOwner: TSignalAnalyzer); virtual;
           destructor Destroy; override;

         published
          
          
           (*<summary>Defines a set of items to be included in spectrum analysis report. </summary>*)
           property ReportItems: TSignalReportSet read FReportItems write SetReportItems;

          
           (*<summary>Defines the floating point number formating applied to frequency values
              in the frequency analysis report. </summary>*)
           property TimeFormat: string read FTimeFormat write SetTimeFormat;

          
           (*<summary>Defines the floating point number formating applied to amplitude values
              in the frequency analysis report. </summary>*)
           property AmplitudeFormat: string read FAmplitudeFormat write SetAmplitudeFormat;

          
           (*<summary>A good example for the delimiter would be Tab character. </summary>
                      
<remarks>If the delimiter is not used, the columns are padded with space char and aligned to the left.
</remarks>
*)
           property Delimiter: string read FDelimiter write SetDelimiter;

          
           (*<summary> If true, tab is used to separate description of parameter from its value. </summary>
                       
<remarks>The tab will also be used, between the units and the values.
</remarks>
*)
           property UseTab: boolean read FUseTab write SetUseTab default false;
         end;

    

      

      (*<summary>Analyzes a signal with different methods in time domain. </summary>
       
<remarks>Signal analyzer component. Use this component
       to quickly estimate auto-correlation, cross-correlation,
       time signal integration/differentiation etc...
       This component provides centralized and easy access
       to many time series analysis routines. The resulting signal is
       always placed in Self.Data.
</remarks>
*)
      TSignalAnalyzer = class(TSignal)
      strict protected
          fWindowInitialized: boolean;
          FScaleFactor: Double;
          FWindow: TSignalWindowType;
          FSidelobeAtt: Double;
          WindowData: TVec;
          FTransform: TTimeTransform;
          FAutoCorrType: TAutoCorrType;
          FCorrTaps1: integer;
          FCorrTaps2: integer;
          FIntegration: TIntegration;
          FRemoveDC: boolean;
          FTraceInterval: Double;
          FExpWindow: Double;
          FRotation: integer;
          FReport: TSignalReport;
          TempData: TVecList;
          IntgState: array [0..3] of TIntegrateState;
          DiffState: array [0..3] of TDiffState;
          procedure SetScaleFactor(const Value: Double);
          procedure SetWindow(const Value: TSignalWindowType);
          procedure SetSidelobeAtt(const Value: Double);
          procedure SetTransform(const Value: TTimeTransform);
          procedure SetCorrTaps(const Value: integer);
          procedure SetAutoCorrType(const Value: TAutoCorrType);
          procedure SetCorrTaps2(const Value: integer);
          procedure SetIntegration(const Value: TIntegration);
          procedure DoIntegrate;
          procedure SetRemoveDC(const Value: boolean);
          procedure AutoCorr;
          procedure CrossCorr;
          procedure SetExpWindow(const Value: Double);
          procedure SetRotation(const Value: integer);
          procedure SetReport(const Value: TSignalReport);
 protected 
           class  function EditorClass: string; override;
      strict protected
          function  InternalUpdate: TPipeState; override;
      public
          (*<summary>Resets initial conditions for integration and differentiation. </summary>*)
          procedure ResetIntegrator;
          (*<summary>Find the nearest minimum. The initial
             position in seconds is given with pos
             and the final position is returned with the Pos
             parameter. </summary>*)
          procedure NearestMinimum(var Pos: Double);
          (*<summary>Find the nearest maximum. The initial
             position in seconds is given with pos
             and the final position is returned with the Pos
             parameter. </summary>*)
          procedure NearestMaximum(var Pos: Double);
          (*<summary>Compute the position of the zero crossing.
             The initial position in seconds is given with pos
             and the final position is returned with the Pos
             parameter. y contains the final value
             of the time signal. In ideal case it should be zero.
             The method will use linear interpolation
             to locate the position of the zero. </summary>*)
          procedure ZeroCrossing(var Pos: Double; out y: Double);
          (*<summary>Apply the window function. </summary>*)
          procedure ApplyWindow;

          procedure Assign(Source: TPersistent); override;
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
         

      published

          
          
          property  Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored; 

          
          
          (*<summary>Generates a report/description of the signal. </summary>*)
          property Report: TSignalReport read FReport write SetReport;

          
          (*<summary> If True, subtract the average value from the signal. This is not
             usuable for streaming. </summary>*)
          property RemoveDC: boolean read FRemoveDC write SetRemoveDC  default False ;

          
          (*<summary>Select the auto-correlation type. </summary>*)
          property  AutoCorrType: TAutoCorrType read FAutoCorrType write SetAutoCorrType;

          
          (*<summary>Select number of taps for auto-correlation and cross-correlation. </summary>*)
          property  CorrTaps1: integer read FCorrTaps1 write SetCorrTaps default 100;

          
          (*<summary>Select number of taps for cross-correlation. </summary>*)
          property  CorrTaps2: integer read FCorrTaps2 write SetCorrTaps2 default 0;

          
          (*<summary>Defines sidelobe attenuation for the Kaiser and Cheybshev window. </summary>*)
          property  SidelobeAtt: Double read FSidelobeAtt write SetSidelobeAtt;

          
          (*<summary>The signal will be scaled with the ScaleFactor. </summary>*)
          property  ScaleFactor: Double read fScaleFactor write SetScaleFactor;

          
          (*<summary>Defines the window to applied to the signal. </summary>*)
          property  Window: TSignalWindowType read fWindow write SetWindow;

          
          (*<summary>Selects the type of the transform to apply to the signal. </summary>*)
          property  Transform: TTimeTransform read FTransform write SetTransform;

          
          (*<summary>Specifies, if the signal should be integrated or differentiated. </summary>*)
          property  Integration: TIntegration read FIntegration write SetIntegration;

          
          (*<summary>Final value of the exponential window. </summary>*)
          property  ExpWindow: Double read FExpWindow write SetExpWindow;

          
          (*<summary>Defines the rotation in number of samples within the  current buffer. </summary>
                     
<remarks>Frequency analysis of the power spectrum is invariant to signal buffer rotation.
</remarks>
*)
          property Rotation: integer read FRotation write SetRotation default 0;
      end;


      (*<summary>Defines which higher order statistics to calculate. </summary>
        
<remarks>Defines which higher order statistics to calculate.
</remarks>
*)
      TBiAnalysis = (
      (*<summary>Calculates bicoherence.</summary>*) hoBicoherence,
      (*<summary>Calculates bispectrum.</summary>*) hoBispectrum
      );

      (*<summary>Defines the formula used for bicoherence estimation. </summary>
         
<remarks>The parameter affects the denominator in the bicoherence
         formula. Both versions return numerically the same result,
         if the amplitudes of the freqency spectrum do not change
         during averaging.
</remarks>
*)

      TBicoherenceType = (
      (*<summary>Tripple product is computed after averaging. This version
         is exactly 2x slower then bctModified, if Recursive is True.
         If linear averaging is used and Recursive is set to false,
         then the speed differences are negligable.</summary>*) bctDefault,
      (*<summary>Tripple product is computed before averaging.</summary>*) bctModified
      );

      

      (*<summary>Encapsulates higher order spectral analysis methods. </summary>
         
<remarks>Used by the TBispectrumAnalyzer to maintain and handle
         the storage structures for bicoherence and bispectrum.
</remarks>
*)
      TBiAnalyzer = class(TPersistent)
      strict private
          
          Spectrum: TSpectrumAnalyzer;
          FFrequency: Double;
          FSingleLinesOnly: boolean;
          FTransform: TBiAnalysis;
          FBiSpectrumB: TMtx;
          FBiSpectrum: TMtx;
          FBiSpectrumRatio: TMtx;
          FLines: TVec;
          FRecursive: boolean;
          FBicoherenceType: TBicoherenceType;
          procedure SetFrequency(const Value: Double);
          procedure SetBispectrum(const Value: TMtx);
          procedure SetTransform(const Value: TBiAnalysis);
          procedure SetLines(const Value: TVec);
          procedure SetSingleLinesOnly(const Value: boolean);
          procedure SetRecursive(const Value: boolean);
          procedure SetBicoherenceType(const Value: TBicoherenceType);






      public
          (*<summary>Returns a full bispectrum/bicoherence matrix in A. </summary>
            
<remarks>If FlipVertical is True, the matrix will be vertically flipped and
            the row at Index 0 will hold the frequency FS/4 where FS is the sampling frequency.
            If SingleLinesOnly is True, the routine will insert zeroes in the part
            of the spectrum that was not computed..
</remarks>
*)
          procedure GetFullSpectrum(A: TMtx; FlipVertical: boolean = false);
          (*<summary>After the bicoherence or bispectrum has been computed, you
            can free the associated memory by calling this routine. </summary>
            
<remarks>This will free up to 75% of the memory allocated.
            This routine is called automatically, if Recursive is False,
            by the Update method..
</remarks>
*)
          procedure FreeUpMemory;
          (*<summary>Get a row from the bispectrum matrix, if the
             entire matrix is computed and maintained. </summary>*)
          procedure DecodeBispectrumMatrix(Row: TVec);
          (*<summary>Get a row from the bispectrum matrix, if only
             preselected bispectrum matrix rows is computed. </summary>*)
          procedure DecodeSingleLineMatrix(Row: TVec);
          (*<summary>Place the bispectrum (bicoherence) in the Bispectrum
             property. </summary>*)
          procedure Update;
          (*<summary>Depending on the value of the SingleLinesOnly
             and Frequency properties, the coresponding row
             from the matrix will be copied to the Owner.Amplt. </summary>
             
<remarks>The owner is TSpectrumAnalyzer.
</remarks>
*)
          procedure DecodeBiAnalysis;

          constructor Create(AOwner: TSpectrumAnalyzer);
          destructor Destroy; override;

          
          (*<summary>Holds the values defined with the value of the Transform property. </summary>*)
          property BiSpectrum: TMtx read FBispectrum write SetBispectrum;
      protected
          
          (*<summary>Internal. </summary>*)
          property BiSpectrumB: TMtx read FBispectrumB;
          
          (*<summary>Internal. </summary>*)
          property BiSpectrumRatio: TMtx read FBispectrumRatio;
      published
          
          (*<summary>Defines how the bicoherence will be computed. </summary>*)
          property BicoherenceType: TBicoherenceType read FBicoherenceType write SetBicoherenceType  default bctModified ;

          
          (*<summary>If True, bicoherence or bispectrum will be calculated
             on every call to TBispectrumAnalyzer.Update. </summary>
             
<remarks>When using runing averaging this allows you to view bicoherence
             and bispectrum in real time. When analyzing data off-line,
             this would uneccessarily slow down the routine.
             If you are interested in the final result only, set Recursive
             to false. When averaging has finished and Recursive
             was false, you have to call the Update method to compute the
             actuall bicoherence/bispectrum. Recursive is True by default.
</remarks>
*)
          property Recursive: boolean read FRecursive write SetRecursive  default True ;

          
          (*<summary>Define which transform will be copied to the Bispectrum property
             when you call the Update method. </summary>*)
          property Transform: TBiAnalysis read FTransform write SetTransform;

          
          (*<summary>Specifies the frequency of bispectral line from the Bispectrum
             matrix, that should be copied to the Onwer (TSpectrumAnalyzer). </summary>*)
          property Frequency: Double read FFrequency write SetFrequency;

          
          (*<summary>If True, only preselected frequencies defined with Lines property will
             be calculated in the bispectral matrix. </summary>*)
          property SingleLinesOnly: boolean read FSingleLinesOnly write SetSingleLinesOnly  default false ;

          
          (*<summary>Holds a real vector of frequencies, that should
             be calculated in the bispectral matrix. </summary>
             
<remarks>You can define the vector to be of any length and hold any number
             of different frequencies.
             The actual lines beeing computed will be based on the sampling
             frequency and frequency resolution of the Owner. (TSpectrumAnalyzer).
             By computing only single bispectrum lines for specific frequencies,
             you can save substantial processing time and are able to increase
             the bispectral frequency resolution to values unattainable, if you
             would be computing the full bispectrum matrix.
</remarks>
*)

          property Lines: TVec read FLines write SetLines;
      end;

      

      

      (*<summary>Estimates bicoherence and/or bispectrum. </summary>
         
<remarks>Use the component to estimate bicoherence and bispectrum
         from the Input signal. You can estimate bicoherence for
         all the frequency pairs, or just the selected ones.
         Bicoherence will be one for two frequency pairs, which
         are related in phase (like harmonics). It will be zero
         for frequency pairs unrelated in phase. The formula used
         to calculate the bicoherence:
     <code>

             |E( X[f1] * X[f2] * conj(X[f1+f2]) )|
     b =  -------------------------------------------
           (E(|X[f1]|) * E(|X[f2]|) * E(|X[f1+f2]|))

     X.. complex frequency spectrum.
     f1.. frequency index, 0 &lt; f1 &lt; (n-1),
     f2.. frequency index, 0 &lt; f2 &lt; (n/2-1),
     n.. length of the frequency spectrum
     E.. averaging function (expected value)

     Bispectrum is defined as:

     B = X[f1] * X[f2] * conj(X[f1+f2])

     </code>
         The component features two modes of operation:
         Recursive and non-recursive.
         * In Recursive mode (Recursive property = True),
         all the spectral parameters are update for every call to the Update method.
         This mode can be used to run bi spectral analysis on-line in real time, if
         you enable infinite exponential averaging.
         * In Non-recursive mode (Recursive property = false), only averaging
         is performed when calls are made to the Update method. Once the averaging
         has finished, BiAnalyzer.Update method must be called to calculate
         the actual bispectra and a call to UpdateSpectrum will place the correct
         line from bispectrum in the Amplt/Phase property. This approach is more CPU efficient then
         the first.

         Because bispectral analysis
         is based on averaging, you can not change the parameters of the
         frequency spectrum of already averaged data.
</remarks>
*)

      TBiSpectrumAnalyzer = class(TSpectrumAnalyzer)
      strict private
          FBiAnalyzer: TBiAnalyzer;
          FOnGetSpectrum: TNotifyVecEvent;
          FOnUpdateSpectrum: TNotifyEvent;
          procedure SetHigherOrder(const Value: TBiAnalyzer);
          procedure SetOnGetSpectrum(const Value: TNotifyVecEvent);
          procedure SetOnUpdateSpectrum(const Value: TNotifyEvent);
 protected 
           class  function EditorClass: string; override;
      strict protected
          function  InternalUpdate: TPipeState; override;
          (*<summary>Decode data from the bispectrum matrix and place
             it in Self.Amplt. Update peaks and bands properties. </summary>*)
          procedure BrowseBispectrum;
      public
          (*<summary>Decode data from the bispectrum matrix and place it in Self.Amplt. </summary>
              
<remarks>Also updates peaks and bands properties
             and then also triggers chart update (if connected).
</remarks>
*)
          procedure UpdateSpectrum;
          (*<summary>Resets averaging and frees internal memory. </summary>
                     
<remarks>Memory allocated for averaging are matrices and can together
                     occupy 3/2 x sqr(Length) x sizeof(Double) of space.
</remarks>
*)
          procedure Reset; override;

          function Update: TPipeState; override;
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
         

      published
          
          
          (*<summary>Parameters required by the higher order spectral analysis. </summary>*)
          property BiAnalyzer: TBiAnalyzer read FBiAnalyzer write SetHigherOrder;

          
          (*<summary>Assign the event returning the custom computed frequency spectrum
             to be used for computation of third order spectrum (bispectrum). </summary>*)
          property OnGetSpectrum: TNotifyVecEvent read FOnGetSpectrum write SetOnGetSpectrum;

          
          (*<summary>Called from the UpdateSpectrum routine. The event
            is called after the spectrum data is updated and before the chart is updated. </summary>*)
          property OnUpdateSpectrum: TNotifyEvent read FOnUpdateSpectrum write SetOnUpdateSpectrum;
      end;

      (*<summary>Defines possible results returned by the cross spectrum analyzer. </summary>
         
<remarks>Input signal is the "input" in to the system being analyzed and Output signal
         is the output from the system. For example: input in the speakers
         comes from the signal generator and output is captured with a microphone.
</remarks>
*)


      TCrossTransform = (
      (*<summary>Returns the averaged spectrum of the Input signal.</summary>*) ctInputSpectrum,
      (*<summary>Returns the averaged spectrum of the Ouput signal.</summary>*) ctOutputSpectrum,
      (*<summary>Returns the averaged cross spectrum.</summary>*) ctCrossSpectrum,
      (*<summary>Returns the coherence between the input and output spectrum. Coherence will
                      be 1 for all frequencies that were only amplified/attenuated or delayed
                      by the "system". It can be used to test the linearity of the system
                      being analyzed. If the system is completely linear, the coherence
                      will be one for all the frequencies. If the system is completely
                      non-linear the coherence will be 0 for all the frequencies.</summary>*)
                                                       ctCoherence,
      (*<summary>Returns the amplitude spectrum of the transfer function
         determined from the Input and Output signals. Use TSpectrumAnalyzer component
         to obtain also the phase response, if both Input and Output of the system
         are related in phase. If they are unrelated in phase use TCrossSpectrumAnalyzer
         to obtain the amplitude response of the system.</summary>*)   ctTransferFunction
         );

      (*<summary>Defines a set of transforms which can be computed by the TCrossSpectrumAnalyzer. </summary>
        
<remarks>Defines a set of transforms which can be computed by the TCrossAnalyzer component.
</remarks>
*)

      TCrossSpectrumAnalysis = (
      (*<summary>Compute the coherence.</summary>*) caCoherence,
      (*<summary>Compute the transfer funciton.</summary>*) caTransferFunction
      );

      

      (*<summary>Defines a set of transforms to be evaluated by the TCrossSpectrumAnalyzer component. </summary>
                  
<remarks>Defines which transforms will be evaluated on each call to the Update method.
</remarks>
*)
      TCrossAnalysisSet = class(TPersistent)
      strict private
        FTransferFunction: boolean;
        FCoherence: boolean;
        procedure setCoherence(const Value: boolean);
        procedure setTransferFunction(const Value: boolean);
      published
       
      (*<summary>If True Coherence will be evaluated. </summary>*)
        property Coherence: boolean read FCoherence write setCoherence  default true ;

       
      (*<summary>If True Transfer function will be evaluated. </summary>*)
        property TransferFunction: boolean read FTransferFunction write setTransferFunction  default true ;
      end;

      

      (*<summary>Encapsulates cross spectral analysis methods. </summary>
         
<remarks>The object is used by the TCrossSpectrumAnalyzer to maintain and handle
         the storage structures for coherence, transfer function
         and other cross spectral analysis related data.
</remarks>
*)
      TCrossAnalyzer = class(TPersistent)
      strict private
        FTempSpectrum: TVec;
        
        Spectrum: TSpectrumAnalyzer;
        FCoherence: TVec;
        FCrossSpectrum: TVec;
        FInputSpectrum: TVec;
        FOutputSpectrum: TVec;
        FCrossAnalysis: TCrossAnalysisSet;
        FCrossTransform: TCrossTransform;
        FRecursive: boolean;
        procedure SetCoherence(const Value: TVec);
        procedure SetCrossSpectrum(const Value: TVec);
        procedure SetInputSpectrum(const Value: TVec);
        procedure SetOutputSpectrum(const Value: TVec);
        procedure SetCrossAnalysis(const Value: TCrossAnalysisSet);
        procedure SetCrossTransform(const Value: TCrossTransform);
        procedure SetRecursive(const Value: boolean);
      strict protected
        procedure DoCoherence;
        procedure DoTransferFunction;
      protected
        procedure ApplySpectrumType;
        
        (*<summary>Internal. </summary>*)
        property TempSpectrum: TVec read FTempSpectrum;
      public
        
        (*<summary>Stores the complex averaged cross spectrum between Input and Output. </summary>*)
        property CrossSpectrum: TVec read FCrossSpectrum write SetCrossSpectrum;
        
        (*<summary>Stores the coherence between Input and output. The vector holds
           valid values only then, if Analysis property contains caCoherence. </summary>*)
        property Coherence: TVec read FCoherence write SetCoherence;
        
        (*<summary>Stores the averaged power spectrum of the Input. </summary>*)
        property InputSpectrum: TVec read FInputSpectrum write SetInputSpectrum;
        
        (*<summary>Stores the averaged power spectrum of the Output. </summary>*)
        property OutputSpectrum: TVec read FOutputSpectrum write SetOutputSpectrum;
        (*<summary>Apply averaging, if Recursive was false and compute
           coherence and/or transfer function. </summary>*)
        procedure Update;

        constructor Create(AOwner: TSpectrumAnalyzer);
        destructor Destroy; override;

      published
       
        (*<summary>Set Recursive to false, to disable average calculation on each iteration and
                   call the Update method yourself, after the averaging has finished. </summary>*)
        property Recursive: boolean read FRecursive write SetRecursive  default true ;

       
       
        (*<summary> Defines which transforms will be computed. </summary>*)
        property Analysis: TCrossAnalysisSet read FCrossAnalysis write SetCrossAnalysis;

       
        (*<summary>Defines which transform will be analyzed by the TSpectrumAnalyzer. </summary>*)
        property Transform: TCrossTransform read FCrossTransform write SetCrossTransform stored False;
      end;

      

      

      (*<summary>Performs cross spectrum analysis. </summary>
         
<remarks>Cross spectrum analysis is used most to determine
         transfer function of the system,  determine
         the presence of non-linearities in the analyzed system
         with the coherence and determine frequency spectrums of
         Input and Output. The coherence is defined as:
     <code>

                  |E( X * conj(Y) )|
     coherence = ---------------------
                   |E(X)| * |E(Y)|

     </code>
         Transfer function is defined as:
     <code>

            E(|Y|)
     tf = ---------
            E(|X|)

     </code>
         where X is the complex frequency spectrum of the signal entering the system
         and Y is the complex frequency spectrum of the signal exiting the system.

         The component features two modes of operation:
         Recursive and non-recursive.
         * In Recursive mode (Recursive property = True),
         all the spectral parameters are update for every call to the Update method.
         This mode can be used to run cross spectral analysis on-line in real time, if
         you enable infinite exponential averaging.
         * In Non-recursive mode (Recursive property = false), only averaging
         is performed when calls are made to the Update method. Once the averaging
         has finished, CrossAnalyzer.Update method must be called to calculate
         the actual spectra and a call to UpdateSpectrum will place the correct
         spectrum in the Amplt/Phase property. This approach is more CPU efficient then
         the first.

         Because cross spectral analysis
         is based on averaging, you can not change the parameters of the
         frequency spectrum of already averaged data.
</remarks>
*)

      TCrossSpectrumAnalyzer = class(TSpectrumAnalyzer)
      strict private
        FCrossAnalyzer: TCrossAnalyzer;
        FOnGetInputSpectrum: TNotifyVecEvent;
        FOnGetOutputSpectrum: TNotifyVecEvent;
        FOnUpdateSpectrum: TNotifyEvent;
        procedure SetCrossAnalyzer(const Value: TCrossAnalyzer);
        procedure SetOnGetInputSpectrum(const Value: TNotifyVecEvent);
        procedure SetOnGetOutputSpectrum(const Value: TNotifyVecEvent);
        procedure SetOnUpdateSpectrum(const Value: TNotifyEvent);
        function GetComplexSpectrum: TVec;
 protected 
           class  function EditorClass: string; override;
      strict protected
        function  InternalUpdate: TPipeState; override;
        procedure BrowseCrossSpectrum; virtual;
        procedure ComputePhaseDelay; override;
        procedure ApplyPhaseSpectrum; override;
      public
        procedure UpdateLogarithmic; override;
       (*<summary>This property returns a pointer to the scaled averaged complex cross spectrum. </summary>*)
        property ComplexSpectrum: TVec read GetComplexSpectrum; 
        (*<summary>Copy the spectrum specified by CrossAnalyzer.Transform property to Self.Amplt.
           and also performs peak and frequency band analysis, if specified. </summary>*)
        procedure UpdateSpectrum;
        (*<summary>The method is overriden to provide support for the CrossAnalyzer.Recursive property. </summary>*)
        function Update: TPipeState; override;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published
       
        (*<summary>Holder for all the storage and settings required by the cross spectral analysis. </summary>*)
        property CrossAnalyzer: TCrossAnalyzer read FCrossAnalyzer write SetCrossAnalyzer;

       
        (*<summary>Event returning the custom computed frequency spectrum
           to be used for computation of the input spectrum. </summary>
           
<remarks>Returned spectrum must be complex
           and its length must match the length of the Input spectrum.
</remarks>
*)
        property OnGetInputSpectrum: TNotifyVecEvent read FOnGetInputSpectrum write SetOnGetInputSpectrum;

       
        (*<summary>Assign the event returning the custom computed frequency spectrum
           to be used for computation of the output spectrum. </summary>
           
<remarks>Returned spectrum must be complex and its length must match the length of the Input spectrum.
</remarks>
*)
        property OnGetOutputSpectrum: TNotifyVecEvent read FOnGetOutputSpectrum write SetOnGetOutputSpectrum;

       
          (*<summary>This event is called from the UpdateSpectrum routine. The event
            is called after the spectrum data is updated and before the chart is updated. </summary>*)
        property OnUpdateSpectrum: TNotifyEvent read FOnUpdateSpectrum write SetOnUpdateSpectrum;

        
        (*<summary>Assign "output" of the system being analyzed. </summary>
           
<remarks>The transfer function is defined as a ratio: input/output.
           Calculate the transfer function with TSpectrumAnalyzer, if the
           phase of input and ouput is related and you are interested in the phase spectrum.
           You can also use the TCrossSpectrumAnalyzer component, if the phases are not related.
</remarks>
*)
        
        property Output;
        
      end;

      (*<summary>Method type used to return result computed by the method. </summary>*)
      TOnComputeStats = procedure (Sender: TObject; var aResult: Double) of object;

      (*<summary>Abstract class for storing statistics computed from Input data. </summary>
        
<remarks>The computation can be customized within an event or also with
        user specified math expression. Each computation returns one
        scalar value computed from the input vector.

        The values computed are stored in the Data property.
        When the buffering is reset, the next value will again
        be written at Data[0]. When Data vector is full, the Update function
        returns pipeOK and no longer pipeStream. pipeOK is returned
        once, then the counters are reset again to start writing at Data[0].

        The component usually connected to the Input of this component is
        TSignalBuffer. TSignalBuffer.StrideLength is automatically assigned
        to the StrideLength property in this case. StrideLength property
        defines the sampling frequency of the output data as:
        <c>
        SamplingFrequency := Input.SamplingFrequency/StrideLength;
        </c>
        StrideLength defines the step in number of samples between consecutive
        windows of data from which the statistical parameters are computed. The
        Length of the windows is specified with the Input.Length property.
</remarks>
*)

      TStatsBuffer = class(TSignal)
      strict private
        FExpression: TMtxExpression;
        FOnComputeStats: TOnComputeStats;
        FOnBufferFilled: TNotifyEvent;
        FStrideLength: integer;
        procedure SetExpression(const Value: TMtxExpression);
        procedure SetOnComputeStats(const Value: TOnComputeStats);
        procedure SetOnBufferFilled(const Value: TNotifyEvent);
        procedure SetStrideLength(const Value: integer);
      strict protected
        Counter: integer;
      public
         property Expression: TMtxExpression read FExpression write SetExpression;
         (*<summary>Resets buffering. </summary>*)
          procedure Reset; override;
         (*<summary>Returns the current position at which the values
            are written in the Data property. When the buffer is
            full it passes pipeOK to the connected component and
            rolls over. </summary>*)
         function CurrentCount: integer;
         constructor Create(AOwner: TComponent); override;
        
      published
         
         (*<summary>To compute custom statistics, place the computation inside
            this event and assign the result to the result variable. </summary>*)
         property OnComputeStats: TOnComputeStats read FOnComputeStats write SetOnComputeStats;

         
         (*<summary>Called when buffer is full and before the data contained
            is used by any conected components. </summary>*)
         property OnBufferFilled: TNotifyEvent read FOnBufferFilled write SetOnBufferFilled;

         
         (*<summary>Specifies the number of samples the input buffer skips forward on each
            new buffer returned. </summary>
            
<remarks>The new sampling frequency is computed as:

            SamplingFrequency := SamplingFrequency/StrideLength;
</remarks>
*)
         property StrideLength: integer read FStrideLength write SetStrideLength;
      end;

      (*<summary>Defines signal statistics that can be streamed. </summary>
        
<remarks>Defines signal statistics that can be streamed in to a discrete signal.
</remarks>
*)
      TSignalStatistics = (
      (*<summary>Signal RMS.</summary>*) ssRMS,
      (*<summary>Maximum deviation from the mean.</summary>*) ssPeak,
      (*<summary>Standard deviation.</summary>*) ssStdDev,
      (*<summary>CREST factor.</summary>*) ssCrest,
      (*<summary>Average value.</summary>*) ssMean,
      (*<summary>Kurtosis of the signal .</summary>*) ssKurtosis,
      (*<summary>Skewness of the signal.</summary>*) ssSkewness,
      (*<summary>Maximum value.</summary>*) ssMax,
      (*<summary>Minimum value.</summary>*) ssMin,
      (*<summary>Custom statistics.</summary>*) ssSignalCustom
      );

      

      

      (*<summary>Computes and stores statistics from Input data. </summary>
        
<remarks>Computes specified statistical parameter on the Input
        data and stores the result in Data property. Until the Data
        vector is full, it is returning pipeStream on Pull requests.
        Once the Data is full, it returns pipeOK once and resest the
        buffer counter to 0. The current writing position is returned
        via CurrentCount index. The component computes one value (1)
        from entire Input.Length .

        Usually the component is preceeded with <see cref="TSignalBuffer"/> to
        specify window analysis length and any window overlapping.
</remarks>
*)
      TSignalStatsBuffer = class(TStatsBuffer)
      strict private
          FMean: TSignalStatsBuffer;
          FStatistics: TSignalStatistics;
          FStdDev: TSignalStatsBuffer;
          procedure SetMean(const Value: TSignalStatsBuffer);
          procedure SetStatistics(const Value: TSignalStatistics);
          procedure SetStdDev(const Value: TSignalStatsBuffer);
      strict protected
          function InternalUpdate: TPipeState; override;
      public

         procedure ReferenceRemoved(Sender: TObject); override;
         function Update: TPipeState; override;
         constructor Create(AOwner: TComponent); override;
        

      published
         
         
         (*<summary>Contains optional object with Mean values. </summary>*)
         property Mean: TSignalStatsBuffer read FMean write SetMean;

         
         
         (*<summary> Assign optional object with StdDev values. </summary>*)
         property StdDev: TSignalStatsBuffer read FStdDev write SetStdDev;

         
         (*<summary>Specifies statistics to be stored in the component. </summary>*)
         property Statistics: TSignalStatistics read FStatistics write SetStatistics;

         
         property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
      end;

      (*<summary>Defines spectral statistics that can be streamed. </summary>
        
<remarks>Defines spectral statistics that can be streamed in to a signal.
</remarks>
*)
      TSpectrumStatistics = (
      (*<summary>Amplitude of the marked peak.</summary>*) ssPeakAmplt,
      (*<summary>Phase of the marked peak.</summary>*)    ssPeakPhase,
      (*<summary>Frequency of the marked peak.</summary>*) ssPeakFreq,
      (*<summary>Total harmonic distortion.</summary>*) ssTHD,
      (*<summary>Total harmonic distortion and noise.</summary>*)ssTHDNoise,
      (*<summary>Signal to noise ratio.</summary>*) ssSNR,
      (*<summary>Signal to noise ratio and distortion.</summary>*) ssSINAD,
      (*<summary>Spurious free dynamic range.</summary>*) ssSFDR,
      (*<summary>Noise floor.</summary>*) ssNF,
      (*<summary>RMS of a frequency band.</summary>*) ssBandRMS,
      (*<summary>Maximum amplitude.</summary>*) ssMaxAmplt,
      (*<summary>Custom spectrum statistics.</summary>*) ssSpectrumCustom
      );

      

      

      (*<summary>Stores and buffers statistics from a SpectrumAnalyzer. </summary>
        
<remarks>Computes specified statistical parameter on the SpectrumAnalyzer
        data and stores the result in Data property. Until the Data
        vector is full, it is returning pipeStream on Pull requests.
        Once the Data is full, it returns pipeOK once and resest the
        buffer counter to 0. The current writing position is returned
        via CurrentCount index.
</remarks>
*)

      TSpectrumStatsBuffer = class(TStatsBuffer)
      strict private
         FSpectrumAnalyzer: TSpectrumAnalyzer;
         FBandIndex: integer;
         FMarkedPeak: integer;
         FStatistics: TSpectrumStatistics;
         procedure SetBandIndex(const Value: integer);
         procedure SetMarkedPeak(const Value: integer);
         procedure SetSpectrumAnalyzer(const Value: TSpectrumAnalyzer);
         procedure SetStatistics(const Value: TSpectrumStatistics);




      strict protected
         function InternalUpdate: TPipeState; override;
      public

         procedure ReferenceRemoved(Sender: TObject); override;
         function Update: TPipeState; override;
         constructor Create(AOwner: TComponent); override;
        

      published
         
         (*<summary>Specifies statistics to be stored in the component. </summary>*)
         property Statistics: TSpectrumStatistics read FStatistics write SetStatistics;

         
         (*<summary>Specifies the marked peak number whose data is to be stored. </summary>*)
         property MarkedPeak: integer read FMarkedPeak write SetMarkedPeak default 0;

         
         (*<summary>Specifies the frequency band number whose RMS data is to be stored. </summary>*)
         property BandIndex: integer read FBandIndex write SetBandIndex;

         
         
         (*<summary>Connect spectrum analyzer component for the source of data and statistics. </summary>*)
         property SpectrumAnalyzer: TSpectrumAnalyzer read FSpectrumAnalyzer write SetSpectrumAnalyzer;
      end;

      

      

      (*<summary>Stores and buffers an overview of the incoming signal. </summary>
        
<remarks>The size of the preview is maintained small enough to allow
        quick redraw on a chart. At start the component holds the actual
        signal up to the size of the SpanLimit. Once SpanLimit is exceeded
        an overview of the signal is computed and the original is discarded.
        From that point on, only the overview of the signal is kept. Once the
        overview exceeds the SpanLimit a new signal overview is computed from
        the existing overview and this procedure repeats in to infinity allowing
        virtually unlimited length of the overview. The component allow's limited
        zoom-in in to the overview, because intermediate results are discarded,
        but the data discarded will not exceed a factor of sqr(Increment).
        The default value for increment is 20 and 20^2 = 400.
        For a 2GB long recording this will result in 5MB of the in-memory buffer. On the resulting
        overview PixelDownSample method should be applied again. For true navigation
        use TSignalBrowse component.

        The purpose of this component is to allow an overview of already recorded
        data to be displayed to the user. The IsOverview function returns true, if
        the component stores an overview, otherwise it is still buffering the original
        signal. This is important to know because the drawing method may differ
        substantially for signal and overview. The overview stores Max and Min
        values interleaved. The Max values form the upper and the Min values from the
        lower envelope.

        Calling Update or Pull method will return pipeOK, if the Data property
        has been updated. It will return pipeStream, if there is not enough
        data buffered yet to update.
</remarks>
*)

      TSignalBrowseBuffer = class(TSignal)
      strict private
        OverviewLevel: integer;
        Data2Len, Data1Len: integer;
        FIsOverview: boolean;
        FSpanLimit: integer;
        Buffer1, Buffer2: TSignalBuffer;
        StoreBuffer1, StoreBuffer2: TSignalStoreBuffer;
        Data1,Data2: TSignal;
        FIncrement: integer;
        procedure IncreaseData2;
        procedure IncreaseData1;
        procedure SetIncrement(const Value: integer);
        procedure SetSpanLimit(const Value: integer);
      strict protected
        function InternalUpdate: TPipeState; override;
      public
        (*<summary>Defines, if the component already stores an overview.</summary>
          
<remarks>Returns true, if the component already stores an overview.
          If the result is false, it is still buffering original signal.
</remarks>
*)
        function IsOverview: boolean;
        (*<summary>Reset the buffering and overview. </summary>*)
        procedure Reset; override;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published
         
        (*<summary>Defines the samples count at which an overview is computed. </summary>*)
        property SpanLimit: integer read FSpanLimit write SetSpanLimit default 32786;

         
        (*<summary>Defines the increment at which the overview is sampled. </summary>
                  
<remarks>The increment step is used in two stages and thus the actual increment is sqr(Increment).
</remarks>
*)
        property Increment: integer read FIncrement write SetIncrement default 40;
      end;

      

      

      (*<summary>Signals saturation warning when recording. </summary>
        
<remarks>Use the component to detect substantial clipping-off of
        the recorded signal. Many failed recordings done by inexpirienced
        users are compromised due to signal saturation (clipping).
</remarks>
*)
      TSignalSaturationWarning = class(TSignal)
      strict private
        Timer: Double;
        FSigned: boolean;
        FShortTermWindow: Double;
        FPrecisionBits: integer;
        fClipped: boolean;
        fClippedShort: boolean;
        fClippedCurrent: boolean;
        FTopLimit: Double;
        FBottomLimit: Double;
        procedure UpdateRange;
        procedure SetPrecisionBits(const Value: integer);
        procedure SetShortTermWindow(const Value: Double);
        procedure SetSigned(const Value: boolean);
      strict protected
        function InternalUpdate: TPipeState; override;
      public
        (*<summary>Resets clipped flags. </summary>*)
        procedure Reset; override;
        (*<summary>Returns the lower dynamic range limit. </summary>*)
        function LowerLimit: Double;
        (*<summary>Returns the upper dynamic range limit. </summary>*)
        function UpperLimit: Double;
        (*<summary>Returns true, if the signal has been clipped. </summary>*)
        function Clipped: boolean;
        (*<summary>Returns true, if the signal has been clipped
          within last 5 seconds. </summary>*)
        function ClippedShort: boolean;
        (*<summary>Returns true, if the current values of the signal
          are clipped. </summary>*)
        function ClippedCurrent: boolean;

        
         constructor Create(AOwner: TComponent); override;

      published
         
        (*<summary> Defines the (integer) dynamic range of the A/D. </summary>
          
<remarks>Any values exceeding the range of the precision bits are considered to be clipped off.
          If the data format is floating point specify a value of 0. The dynamic range checked will be within -1..+1
</remarks>
*)
        property PrecisionBits: integer read FPrecisionBits write SetPrecisionBits default 16;

         
        (*<summary>Signal is signed, if centered around zero. </summary>
          
<remarks>Set this value to true to indicate if the (integer) signal
          is centered around zero or spans from zero to 2^(PrecisionBits).
</remarks>
*)
        property Signed: boolean read FSigned write SetSigned  default true ;

         
        (*<summary>Defines the "clipp-hold" time in seconds after which
        the ClippedShort flag is reset. Default value is 5 seconds. </summary>*)
        property ShortTermWindow: Double read FShortTermWindow write SetShortTermWindow;
      end;

      (*<summary>Stores the history of the associated Mark. </summary>
        
<remarks>Stores the history of individual fields of
        associated TMarkRecord. The Mark to trace is specified with
        the MarkIndex property.
</remarks>
*)

      TTraceItem = class(TPersistent)
      strict private
        FMarkIndex: integer;
        FSpectralDt: Double;
        FSpectralSamplingFrequency: Double;
        FSpectralSamplingTime: Double;
        FSpectralTimeOffset: Double;
        procedure SetMarkIndex(const Value: integer);
        procedure SetSpectralSamplingFrequency(const Value: Double);
        procedure SetSpectralTimeOffset(const Value: Double);
      public
        (*<summary>Stores the history of the TMarkRecord Amplt field. </summary>*)
        Amplt: TVec;
        (*<summary>Stores the history of the TMarkRecord Freq field. </summary>*)
        Freq: TVec;
        (*<summary>Stores the history of the TMarkRecord Phase field. </summary>*)
        Phase: TVec;
        (*<summary>Stores the history of the TMarkRecord Time field. </summary>*)
        Time: TVec;
        (*<summary>Defines the index in the MarkList, to identify the
             mark whose history is to be stored. </summary>*)
        property MarkIndex: integer read FMarkIndex write SetMarkIndex;
        (*<summary>Time in seconds at which the spectrogram starts. </summary>
                    
<remarks>This parameter does not affect the computation, but can be
                    used later on for charting.
</remarks>
*)
        property SpectralTimeOffset: Double read FSpectralTimeOffset write SetSpectralTimeOffset;
        (*<summary>Total length in seconds between the first and the last spectrum. </summary>
           
<remarks>This value is equal to Count*Dt. This is a ready only property,
           but the contents of the object can be changed.
</remarks>
*)
        property SpectralSamplingTime: Double read FSpectralSamplingTime write FSpectralSamplingTime;

        constructor Create; virtual;
        destructor Destroy; override;
        
        procedure Assign(Src: TPersistent); override;
        

      published
        (*<summary>Time between two consecutive traced values. </summary>*)
        
        
        (*<summary>Sampling frequency with which the spectrums are being sampled.
           When setting this property Dt is automatically updated. </summary>*)
        property SpectralSamplingFrequency: Double read FSpectralSamplingFrequency write SetSpectralSamplingFrequency;

        
        
        
        (*<summary>Time in seconds between consecutive spectrums.  </summary>
           
<remarks>When setting this property SamplingFrequency is automatically updated.
</remarks>
*)
        property SpectralDt: Double read FSpectralDt;
      end;

      (*<summary>Form vectors from a list of TMarkList's. </summary>
        
<remarks>Each time a time signal or spectrum are updated
        their marks are also updated. This object provides
        methods for storing the history of values that marks
        have had.
</remarks>
*)

      TTraceList = class(TPersistent)
      strict private
        aList: TObjectsList;
        FTimeTrace: boolean;
        FFreqTrace: boolean;
        FPhaseTrace: boolean;
        FAmpltTrace: boolean;
        FValueCount: integer;
        procedure SetCount(const Value: integer);
        procedure SetAmpltTrace(const Value: boolean);
        procedure SetFreqTrace(const Value: boolean);
        procedure SetPhaseTrace(const Value: boolean);
        procedure SetTimeTrace(const Value: boolean);
        function GetItems(i: integer): TTraceItem;
        procedure SetItems(i: integer; const Value: TTraceItem);
        function GetCount: integer;
        procedure SetValueCount(const Value: integer);
        function GetSpectralTimeOffset: Double;
        procedure SetSpectralTimeOffset(const Value: Double);
        function GetSpectralSamplingFrequency: Double;
        procedure SetSpectralSamplingFrequency(const Value: Double);
      public
        (*<summary> Resets storage size for all traces. </summary>
                    
<remarks>ValueCount is set to zero and all memory allocated
                    to store traces is also reset to 0.
</remarks>
*)
        procedure Reset;
        (*<summary> Adjusts the allocated memory to match number of traced items. </summary>
                    
<remarks>To speed up tracing, more memory is allocated than what is immediately
                    needed by the number of traced items. At the end of tracing call EndTrace
                    to resize the memory to match the number of traced items.
</remarks>
*)
        procedure EndTrace;

        (*<summary>Number of values stored for each Item. </summary>
                  
<remarks>Usually equal to TSpetrogram.SpectrogramList.Count
</remarks>
*)
        property ValueCount: integer read FValueCount write SetValueCount;
        (*<summary>Returns trace item at index i. </summary>*)
        property Items[i: integer]: TTraceItem read GetItems write SetItems; default;
        (*<summary>If true, the Amplt field in the marks will be traced. </summary>*)
        property AmpltTrace: boolean read FAmpltTrace write SetAmpltTrace;
        (*<summary>If true, the Phase field in the marks will be traced. </summary>*)
        property PhaseTrace: boolean read FPhaseTrace write SetPhaseTrace;
        (*<summary>If true, the Freq field in the marks will be traced. </summary>*)
        property FreqTrace: boolean read FFreqTrace write SetFreqTrace;
        (*<summary>If true, the time field in the marks will be traced. </summary>*)
        property TimeTrace: boolean read FTimeTrace write SetTimeTrace;
        (*<summary>Get or set the number of TTraceItem objects. </summary>
          
<remarks>If the value is smaller, than the current count,
          they will be deleted. If the  value is bigger, they
          will be created.
</remarks>
*)
        property Count: integer read GetCount write SetCount;
        (*<summary>Add a new TTraceItem object to the list. </summary>
          
<remarks>The function returns the the index at which the new
          object has been added.
</remarks>
*)
        function Add: integer; overload;
        (*<summary>Add a new TTraceItem object to the list. </summary>
          
<remarks>Thefunction returns the the index at which the new
          object has been added.
</remarks>
*)
        function Add(Item: TTraceItem): integer; overload;
        (*<summary>Add new TTraceItem object to the list, by copying
          them for the Src TTraceList. </summary>*)
        procedure Add(Src: TTraceList); overload;
        (*<summary>Clear all items and free associated objects. </summary>*)
        procedure Clear;
        (*<summary>Delete item at Index and free associated object. </summary>*)
        procedure Delete(Index: integer);

        constructor Create; virtual;
        destructor Destroy; override;

      published
        (*<summary>Time between two consecutive traced values. </summary>*)
        
        
        (*<summary>Sampling frequency with which the spectrums are being sampled.
           When setting this property Dt is automatically updated. </summary>*)
        property SpectralSamplingFrequency: Double read GetSpectralSamplingFrequency write SetSpectralSamplingFrequency;
        (*<summary>Time in seconds at which the spectrogram starts. </summary>
                   
<remarks>This parameter does not affect the computation, but can be
                   used later on for charting. Setting this values modifes only all currently present items.
</remarks>
*)
        property SpectralTimeOffset: Double read GetSpectralTimeOffset write SetSpectralTimeOffset;
      end;

      

      

      (*<summary>Form vectors from a list of TMarkList's. </summary>
        
<remarks>Spectrogram expects a TSpectrumAnalyzer or descendant to be
        connected to its Input property. Each time the Pull method is called on
        component, the connected components are recalcuated and new spectrum
        is added to the list of spectrums within the component. There are various
        support routines avialable to help manage that data. Typical usage
        scenarios are like this (assuming single channel file):

        <code>
        SignalRead.FileName := YourFile;
        SignalRead.OpenFile;
        SignalRead.RecordTimePosition := StartingTime;
        SpectrumAnalyzer.Input := SignalRead;
        Spectrogram.Input := SpectrumAnalyzer;
        Spectrogram.PullUntilEnd;//process entire file

        Spectrogram.SpectralTimeOffset := StartingTime;
        Spectrogram.SpectralSamplingFrequency := 1/SignalRead.FramesPerSecond;
        </code>

        Alternative:

        <code>
        SignalRead.FileName := YourFile;
        SignalRead.OpenFile;
        SignalRead.RecordTimePosition := StartingTime;
        SpectrumAnalyzer.Input := SignalRead;
        SpectrumAnalyzer.Peaks.TraceMethod := ptAmpltHarmonics;

        Spectrogram.TraceOnly := true;
        Spectrogram.Input := SpectrumAnalyzer;
        Spectrogram.PullUntilEnd;//process entire file

        Spectrogram.Trace.EndTrace;
        Spectrogram.Trace.SpectralTimeOffset := StartingTime;
        Spectrogram.Trace.SpectralSamplingFrequency := 1/SignalRead.FramesPerSecond;

//        Spectrogram.Trace[i].Freq  //TVec contains gathered data
//        Spectrogram.Trace[i].Amplt //TVec contains gathered data
        </code>
</remarks>
*)

      TSpectrogram = class(TStaticSpectrumAnalyzer)
      strict private
        FSpectrumList: TStaticSpectrumAnalyzerList;
        FSpectralSamplingTime: Double;
        FSpectralDt: Double;
        FSpectralSamplingFrequency: Double;
        FMaxSpectrumCount: integer;
        FLogSpan: TLogSpan;
        FTrace: TTraceList;
        FLogarithmic: boolean;
        FSpectralTimeOffset: Double;
        FNarrowBandL: Double;
        FNarrowBandH: Double;
        FTraceOnly: boolean;
        procedure SetLogSpan(const Value: TLogSpan);
        procedure SetMaxSpectrumCount(const Value: integer);
        function GetCount: integer;
        function GetSpectrum(i: integer): TStaticSpectrumAnalyzer;
        procedure SetCount(const Value: integer);
        procedure SetSpectralDt(const Value: Double);
        procedure SetSpectralSamplingFrequency(const Value: Double);
        procedure SetSpectrum(i: integer; const Value: TStaticSpectrumAnalyzer);
        function GetInput1: TSpectrum; reintroduce;
        procedure SetInput1(const Value: TSpectrum);
        procedure SetTrace(const Value: TTraceList);
        procedure TraceAmplt(MarkIndex: integer; const Amplt: TVec);
        procedure TraceFreq(MarkIndex: integer; const Freq: TVec);
        procedure TracePhase(MarkIndex: integer; const Phase: TVec);

        procedure TraceTime(MarkIndex: integer; const Time: TVec);
        procedure SetLogarithmic(const Value: boolean);
        
        
        
        procedure SetSpectralTimeOffset(const Value: Double);
        procedure SetNarrowBandH(const Value: Double);
        procedure SetNarrowBandL(const Value: Double);
        procedure AppendTrace;
        procedure SetTraceOnly(const Value: boolean);
      strict protected

        function InternalUpdate: TPipeState; override;
        function CreateInputs: TAnalysisConnectorsCollection; override;

        (*<summary>Updates marks for all spectrums according to the
          parameters specified by the Peaks property. </summary>*)
        procedure UpdateMarks;
        (*<summary>Updates the contents of the Trace object according the value
           of its properties. </summary>*)
        procedure UpdateTrace(TraceIndex: integer);
      public
        
        (*<summary>Containes parameters and results for individual traces. </summary>
           
<remarks>A trace is a time series of one of the three parameters
           (frequency, amplitude, phase) for a predefined peak
           in dependence of time.
</remarks>
*)
        property Trace: TTraceList read FTrace write SetTrace;
        (*<summary>Time in seconds at which the spectrogram starts. </summary>
                    
<remarks>This parameter does not affect the computation, but can be
                    used later on for charting.
</remarks>
*)
        property SpectralTimeOffset: Double read FSpectralTimeOffset write SetSpectralTimeOffset;
        (*<summary>Total length in seconds between the first and the last spectrum. </summary>
           
<remarks>This value is equal to Count*Dt. This is a ready only property,
           but the contents of the object can be changed.
</remarks>
*)
        property SpectralSamplingTime: Double read FSpectralSamplingTime;
        (*<summary>Specifies the number of the spectrums in the list. </summary>
           
<remarks>Setting the property will set the number of spectrums in the list.
</remarks>
*)
        property Count: integer read GetCount write SetCount;
        (*<summary>Returns spectrum at index i. </summary>*)
        property Spectrum[i: integer]: TStaticSpectrumAnalyzer read GetSpectrum write SetSpectrum; default;
        (*<summary>Propagates settings of Peaks and Bands properties to all spectrums. </summary>*)
        procedure UpdateTraceParameters;
        (*<summary>Copies marked values from spectrogram to the Trace property. </summary>*)
        procedure FetchTraces;
        (*<summary>Updates marks for all spectrums then calls FetchTraces. </summary>*)
        procedure UpdateAllTraces;
        (*<summary>Delete the spectrum at index i. </summary>
          
<remarks>Delete spectrum at index.
</remarks>
*)
        procedure Delete(Index: integer);
        (*<summary>Insert spectrum at index. </summary>*)
        procedure Insert(Index: integer);
        (*<summary>Move spectrum from SrcIdx to DstIdx index. </summary>*)
        procedure Move(SrcIdx, DstIdx: integer);
        (*<summary>Rotate spectrums down or up in the list. </summary>*)
        procedure Rotate(Offset: integer);
        (*<summary>Copies all amplitude spectrums as rows to the destination matrix. </summary>*)
        procedure CopyAmplt(Dst: TMtx);
        (*<summary>Copies all amplitude spectrums as columns to the destination matrix. </summary>*)
        procedure CopyAmpltTransposed(Dst: TMtx); overload;
        (*<summary>Copies all amplitude spectrums as columns to the destination matrix. </summary>*)
        procedure CopyAmpltTransposed(Dst: TMtx; TimeStart, TimeStop, FreqStart, FreqStop: Double); overload;
        (*<summary>Copies all phase spectrums as rows to the destination matrix. </summary>*)
        procedure CopyPhase(Dst: TMtx);
        (*<summary>Reset the spectrogram and delete the stored data. </summary>*)
        procedure Reset; override;

        
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

      published
        
        
        (*<summary>Specifies to track only marked peaks without entire spectrums. </summary>
                   
<remarks>Only Trace property will be populated when this property is True. Call
                   Trace.EndTrace method when the last spectrum has been processed. When true
                   the contents of the marked peaks will be stored in the Trace property and
                   the contents of the spectrum will be ignored. This can be used for subsequent
                   analysis of the spectrogram where only specific frequencies are be analyzed
                   with higher resolution.
</remarks>
*)
        property TraceOnly: boolean read FTraceOnly write SetTraceOnly;
        
        
        (*<summary>Specifies Input for the data to the component. </summary>*)
        property Input: TSpectrum read GetInput1 write SetInput1 stored InputsStored; 

        
        
        (*<summary>Sampling frequency with which the spectrums are being sampled.
           When setting this property Dt is automatically updated. </summary>*)
        property SpectralSamplingFrequency: Double read FSpectralSamplingFrequency write SetSpectralSamplingFrequency;

        
        
        
        (*<summary>Time in seconds between consecutive spectrums.  </summary>
           
<remarks>When setting this property SamplingFrequency is automatically updated.
</remarks>
*)
        property SpectralDt: Double read FSpectralDt write SetSpectralDt;

        
        (*<summary>Maximum number of spectrums up to which the spectrogram will grow. </summary>*)
        property MaxSpectrumCount: integer read FMaxSpectrumCount write SetMaxSpectrumCount default 20;

        
        (*<summary>Defines the span of the logarithmic spectrum in dB. </summary>*)
        property LogSpan: TLogSpan read FLogSpan write SetLogSpan  default ls120 ;

        
        (*<summary>If true, the stored spectrums will be converted to Logarithmic scale.  </summary>
           
<remarks>New spectrums should not be added to the spectrogram, while Logarithmic
           property is checked. Calling Pull method while Logarithmic property
           is True, will raise an exception. It is possible however to toggle
           the value of the Logarithmic property.
           Changing the value of this property immediately triggers conversion.

           The value of the LogSpan property will be used as
           a parameter in the conversion. Logarithmic property can not be used,
           if incoming spectrums already have logarithmic scale: logarithmic
           scale can not be undone, because exact parameters on how it was
           applied are not known.

           All spectrum analysis methods from Peaks, Bands and Reports properties
           will comply with the value of the Logarithmic property.
</remarks>
*)
        property Logarithmic: boolean read FLogarithmic write SetLogarithmic;
        
        (*<summary> Specify value other than 0 to indicate the start of the reduced bandwidth. </summary>
                    
<remarks>Spectrogram will store only values between NarrowBandL and NarrowBandH.
</remarks>
*)
        property NarrowBandL: Double read FNarrowBandL write SetNarrowBandL;
        
        (*<summary> Specify value other than 0 to indicate the final frequency of the reduce bandwidth. </summary>
                    
<remarks>Spectrogram will store only values between NarrowBandL and NarrowBandH.
</remarks>
*)
        property NarrowBandH: Double read FNarrowBandH write SetNarrowBandH;
      end;

      

     

    (*<summary>Manages a list of TSpectrogram objects. </summary>
      
<remarks>Manages a list of TSpectrogram objects.
</remarks>
*)

      TSpectrogramList = class(TSpectrumList)
      strict private
          
          function GetItems(index: integer): TSpectrogram; reintroduce;
          procedure SetItems(index: integer; const Value: TSpectrogram);
      strict protected
          function  AddItem: TMtxComponent; override;
      public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSpectrogram components by Index. </summary>*)
          property Items[index: integer]: TSpectrogram read GetItems write SetItems; default; 
          
      published
          

          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

      (*<summary>Manages a list of TSignalSaturationWarning objects. </summary>
                
<remarks>Manages a list of TSignalSaturationWarning objects.
</remarks>
*)

      TSignalSaturationWarningList = class(TAnalysisList)
      strict private
          
          function GetItems(index: integer): TSignalSaturationWarning; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalSaturationWarning);
      strict protected
          function  AddItem: TMtxComponent; override;
      public
        
         
         constructor Create(AOwner: TComponent); override;
        
          (*<summary>Access TSignalSaturationWarning components by Index. </summary>*)
          property Items[index: integer]: TSignalSaturationWarning read GetItems write SetItems; default; 
          
     published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
        end;

      

      

    (*<summary>Manages a list of TSignalBrowseBuffer objects.
      Manages a list of TSignalBrowseBuffer objects. </summary>*)

      TSignalBrowseBufferList = class(TAnalysisList)
      strict private
          
          function GetItems(index: integer): TSignalBrowseBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalBrowseBuffer);
      strict protected
          function  AddItem: TMtxComponent; override;
        public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSignalBrowseBuffer components by Index. </summary>*)
          property Items[index: integer]: TSignalBrowseBuffer read GetItems write SetItems; default; 
          
        published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
        end;

      

      

    (*<summary>Manages a list of TSpectrumStatsBuffer objects. </summary>
      
<remarks>Manages a list of TSpectrumStatsBuffer objects.
</remarks>
*)

      TSpectrumStatsBufferList = class(TAnalysisList)
      strict private
          
          function GetItems(index: integer): TSpectrumStatsBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSpectrumStatsBuffer);
      strict protected
          function  AddItem: TMtxComponent; override;
       public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSpectrumStatsBuffer components by Index. </summary>*)
          property Items[index: integer]: TSpectrumStatsBuffer read GetItems write SetItems; default; 
          
        published

          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 

        end;

      

      

    (*<summary>Manages a list of TSignalStatsBuffer objects. </summary>
      
<remarks>Manages a list of TSignalStatsBuffer objects.
</remarks>
*)

      TSignalStatsBufferList = class(TAnalysisList)
      strict private
          
          function GetItems(index: integer): TSignalStatsBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalStatsBuffer);
      strict protected
          function  AddItem: TMtxComponent; override;
      public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSignalStatsBuffer components by Index. </summary>*)
          property Items[index: integer]: TSignalStatsBuffer read GetItems write SetItems; default; 
          
        published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
        end;

      

     


    (*<summary>Manages a list of TSpectrumAnalyzer objects. </summary>
      
<remarks>Manages a list of TSpectrumAnalyzer objects.
</remarks>
*)

      TSpectrumAnalyzerList = class(TSpectrumList)
      strict private
          
          function GetItems(index: integer): TSpectrumAnalyzer; reintroduce;
          procedure SetItems(index: integer; const Value: TSpectrumAnalyzer);
      strict protected
          function  AddItem: TMtxComponent; override;
      public

         
         constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSpectrumAnalyzer components by Index. </summary>*)
          property Items[index: integer]: TSpectrumAnalyzer read GetItems write SetItems; default; 
          
      published
          

          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

    (*<summary>Manages a list of TCrossSpectrumAnalyzer objects.
      Manages a list of TCrossSpectrumAnalyzer objects. </summary>*)
      TCrossSpectrumAnalyzerList = class(TSpectrumList)
      strict private
          
          FOutputs: TSignalList;
          FOutput: TSignal;
          function GetItems(index: integer): TCrossSpectrumAnalyzer; reintroduce;
          procedure SetItems(index: integer; const Value: TCrossSpectrumAnalyzer);
          procedure SetOutputs(const Value: TSignalList);
          procedure SetOutput(const Value: TSignal);
      strict protected
          function  AddItem: TMtxComponent; override;
          procedure InternalMatchInputs; override;
      public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSpectrumAnalyzer components by Index. </summary>*)
          property Items[index: integer]: TCrossSpectrumAnalyzer read GetItems write SetItems; default; 
          
      published
          (*<summary>Connect system output signals to this connector.  </summary>
                    
<remarks>If each system has its own output assign the list of outputs to this property.
</remarks>
*)
          property Outputs: TSignalList read FOutputs write SetOutputs stored InputsStored;
          (*<summary>Connect system output signal to this connector.  </summary>
                    
<remarks>If all systems have common output assign the output to this property.
</remarks>
*)
          property Output: TSignal read FOutput write SetOutput stored InputsStored;
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

    (*<summary>Manages a list of TBiSpectrumAnalyzer objects.
      Manages a list of TBiSpectrumAnalyzer objects. </summary>*)
      TBiSpectrumAnalyzerList = class(TSpectrumList)
      strict private
          
          function GetItems(index: integer): TBiSpectrumAnalyzer; reintroduce;
          procedure SetItems(index: integer; const Value: TBiSpectrumAnalyzer);
      strict protected
          function  AddItem: TMtxComponent; override;
        public

        
         constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSpectrumAnalyzer components by Index. </summary>*)
          property Items[index: integer]: TBiSpectrumAnalyzer read GetItems write SetItems; default; 
          
        published
          

          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
        end;

      

     

     (*<summary>Manages a list of TSignalAnalyzer objects.
                Manages a list of TSignalAnalyzer objects. </summary>*)
     TSignalAnalyzerList = class(TSignalList)
     strict private
          
          function GetItems(index: integer): TSignalAnalyzer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalAnalyzer);
     strict protected
          function  AddItem: TMtxComponent; override;
     public

         
          constructor Create(AOwner: TComponent); override;

          (*<summary>Access TSignalAnalyzer components by Index. </summary>*)
          property Items[index: integer]: TSignalAnalyzer read GetItems write SetItems; default; 
          
     published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
        end;



       (*<summary>Convert a TCrossTransform type to a string. </summary>
         
<remarks>The function returns a description of a TCrossTransform type specified
         by the  CrossTransform parameter.
</remarks>
*)
        function CrossTransformToString(CrossTransform: TCrossTransform): string;

       (*<summary>Convert a TSpectrumMethod type to a string. </summary>
         
<remarks>The function returns a description of a TSpectrumMethod type specified
         by the  SpectrumMethod parameter.
</remarks>
*)
        function SpectrumMethodToString(SpectrumMethod: TSpectrumMethod): string;
        (*<summary>Convert a TAveraging type to a string. </summary>
          
<remarks>The function returns a description of a TAveraging type
          specified by the Averaging parameter.
</remarks>
*)
        function AveragingToString(Averaging: TAveraging): string;

        (*<summary>Convert a TIntegration type to a string. </summary>
          
<remarks>The function returns a description of a TIntegration type
          specified by the Integration parameter.
</remarks>
*)
        function IntegrationToString(Integration: TIntegration): string;

        (*<summary>Convert a TInterpolationMethod type to a string. </summary>
          
<remarks>The function returns a description of a TInterpolationMethod type
          specified by the Method parameter.
</remarks>
*)
        function InterpolationMethodToString(Method: TInterpolationMethod): string;

        (*<summary>Convert a TInterpolationPrecision type to a string. </summary>
          
<remarks>The function returns a description of a TInterpolationPrecision type
          specified by the Precision parameter.
</remarks>
*)
        function InterpolationPrecisionToString(Precision: TInterpolationPrecision): string;

        (*<summary>Convert a TTimeTransform type to a string. </summary>
          
<remarks>The function returns a description of a TTimeTransform type
          specified by the Transform parameter.
</remarks>
*)
        function TimeTransformToStr(Transform: TTimeTransform): string;

        (*<summary>Convert a TAutoCorrType type to a string. </summary>
          
<remarks>The function returns a description of a TAutoCorrType type
          specified by the AutoCorrType parameter.
</remarks>
*)
        function AutoCorrTypeToStr(AutoCorrType: TAutoCorrType): string;

