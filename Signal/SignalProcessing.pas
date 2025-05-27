


























(*<summary> Signal processing components. </summary>*)
unit SignalProcessing;


interface

{$I BdsppDefs.inc}
{$WARN SYMBOL_DEPRECATED OFF}

uses MtxVec, Math387, SignalTools,
     SignalUtils, IIRFilters, Polynoms, MtxBaseComp, MtxVecBase

     
        
        ,Types
        
     

     

     
       ,Classes
       ,Sysutils
       
       ,Contnrs
       
     

     ;

(*<summary> Maximum power of two that rate conversion factor can have. </summary>*)
const MaxRateConversionPowerFactor = 20;

type

     

      TSignalChannel = class(TSignal)
      strict private
        FChannels: TIntegersList;
        procedure SetChannel(const Value: integer);
        function GetChannel: integer;
        procedure SetChannels(const Value: TIntegersList);
      public
        property Channels: TIntegersList read FChannels write SetChannels;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

      published
        (*<summary> Specifies the hardware channel number where the data for this object is to come from or go to. </summary>*)
        property Channel: integer read GetChannel write SetChannel;
      end;

      

     

      (*<summary> Demultiplex input signal. </summary>
        
<remarks>Use this component to demultiplex the Channel from the Input
        and place the result in Self (Data property).
</remarks>
*)
      TSignalDemux = class(TSignal)
      strict private
           FChannel: integer;
           procedure SetChannel(const Value: integer);
      strict protected
           function InternalUpdate: TPipeState; override;
      public

           constructor Create(AOwner: TComponent); override;
          

      published
          
          property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

          
      (*<summary> Specifies the channel number to be demultiplexed from the Input. </summary>*)
           property Channel: integer read FChannel write SetChannel default 0;
      end;

      

     

    (*<summary> Multiplex input signal. </summary>
       
<remarks>Use this component to multiplex channels from the Inputs
       and place the result in Self (Data property). Complex,Length,
       SamplingFrequency and ChannelCount properties are adjusted
       automatically. All Inputs must be of the same length and same
       sampling frequency.
</remarks>
*)
      TSignalMux = class(TSignal)
      strict private
           FInputList: TAnalysisList;
           procedure SetInputList(const Value: TAnalysisList);
      strict protected
           function InternalUpdate: TPipeState; override;
           function GetChannelCount: integer; override;
      public
          
           property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
           procedure MatchInputs; virtual;

           function InternalPull: TPipeState; override;  
           constructor Create(AOwner: TComponent); override;
          

      published

          
          
          property  Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored;  

           
           
           (*<summary> Component list connector. </summary>
             
<remarks>When this property is assigned it will override any
             changes made to the Inputs property the first time
             when the Update method is called. The inputs property will
             be set like this:

             <code>

             Inputs.Count := InputList.Count;
             for i := 0 InputList.Count-1 do Inputs[i].Input := InputList[i];

             </code>
</remarks>
*)
           property InputList: TAnalysisList read FInputList write SetInputList stored InputsStored;
      end;

      




     

      (*<summary> Form complex signal. </summary>
       
<remarks>Use this component to join real and imaginary channels from the
       RealInput and ImagInput in to a complex signal
       and place the result in Self (Data property). Complex,Length,
       SamplingFrequency and ChannelCount properties are adjusted
       automatically. All Inputs must be of the same length and same
       sampling frequency.
</remarks>
*)
      TSignalToComplex = class(TSignal)
      strict private
           procedure SetImagInput(const Value: TSignal);
           procedure SetRealInput(const Value: TSignal);
           function GetRealInput: TSignal;
            function GetImagInput: TSignal;
      strict protected
           function InternalUpdate: TPipeState; override;
      public

           function InternalPull: TPipeState; override;  
           constructor Create(AOwner: TComponent); override;
          

      published
           
           (*<summary> Real component of the complex signal. </summary>*)
           property RealInput: TSignal read GetRealInput write SetRealInput;

           
           (*<summary> Imaginary component of the complex signal. </summary>*)
           property ImagInput: TSignal read GetImagInput write SetImagInput;
      end;

      



     

      (*<summary> Increase buffer length. </summary>
        
<remarks>Use this component to increase the length of the data
         blocks being processed by Factor. Each time an update
         request is received, a new block of data is appended to
         the existing buffer. Once the Length exceeds the Input.Length*Factor
         the buffer is shifted left for each new block of data.
</remarks>
*)
      TSignalIncBuffer = class(TSignal)
      strict private
           FFactor: integer;
           procedure SetFactor(const Value: integer);
      strict protected
           function InternalUpdate: TPipeState; override;
      public
           (*<summary> Reset buffer Length to zero. </summary>*)
           procedure Reset; override;

           constructor Create(AOwner: TComponent); override;
          

      published
          
          property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
           
           (*<summary> Defines the factor by which to increase the data block. </summary>*)
           property Factor: integer read FFactor write SetFactor default 1;
      end;

      

      

      (*<summary> Decrease buffer length.  </summary>
        
<remarks>Use this component to decrease the length of the data
         blocks being processed by Factor. Each time an update
         request is received, a new block of data is obtained from
         the input buffer. Until the Input buffer is empty, the Pull
         request is not passed to it.
</remarks>
*)
      TSignalDecBuffer = class(TSignal)
      strict private
           FPosition: integer;
           FFactor: integer;
           procedure SetFactor(const Value: integer);
      strict protected
           function InternalUpdate: TPipeState; override;
      public

           function InternalPull: TPipeState; override;  
           constructor Create(AOwner: TComponent); override;
          

           (*<summary> Reset the read position of the input buffer. </summary>*)
           procedure Reset; override;
      published
          
          property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

           
           (*<summary> Defines the factor by which to decrease the data block. </summary>*)
           property Factor: integer read FFactor write SetFactor default 1;
      end;


      TBandFilterRecordDouble = packed record
        BandStart: double;
        BandEnd: double;
        Gain: double;
        AttRipple: double;
        Achieved: double;
        Checked: byte;
      end;


      (*<summary> Used by TSignalFilter for filter definition. </summary>
        
<remarks>Structure used by the TSignalFilter component
        to define the filter parameters of multiband filters.
</remarks>
*)
      TBandFilterRecord = class
      public
        (*<summary> Defines the start of the pass band. </summary>*)
        BandStart: double;
        (*<summary> Defines the end of the pass band. </summary>*)
        BandEnd: double;
        (*<summary> Defines the gain of the band. If it is bigger of equal
        to one it is a passband. If it is zero it is a stopband. </summary>*)
        Gain: double;
        (*<summary>AttRipple is the required amplitude ripple across the frequency band.</summary>*)
        AttRipple: double;
        (*<summary> Contains the achieved amplitude ripple across the frequency band after the filter has been computed. </summary>*)
        Achieved: double;
        (*<summary> If checked is True, the frequency band should be included in the filter design process. </summary>*)
        Checked: boolean;

        constructor Create; virtual;

        (*<summary> Assign all fields from Src. </summary>*)
        procedure Assign(Src: TBandFilterRecord); overload;
        (*<summary> Assign all fields from Src. </summary>*)
        procedure Assign(const Src: TBandFilterRecordDouble); overload;
        (*<summary> Assign all fields to Dst. </summary>*)
        procedure AssignTo(var Dst: TBandFilterRecordDouble); overload;
      end;

      (*<summary> Array of TBandFilterRecords. </summary>
        
<remarks>Dynamic array of TBandFilterRecords.
</remarks>
*)
      TBandFilterRecordArray = array of TBandFilterRecordDouble;

      (*<summary> Defines possible comb filter design errors. </summary>
        
<remarks>Detects filter errors, when designing filters and specifying filter bands.
</remarks>
*)

      TFilterError = (
      (*<summary>Filter was designed successfully.</summary>*) ferNone,
      (*<summary>At least two bands are required for a
      lowpass/highpass/bandstop/bandpass filter.</summary>*) ferNeedTwoBands,
      (*<summary>You can not have two consecutive stop bands, but you can have two consecutive
       pass bands.</summary>*) ferConsecutiveStopBands,
      (*<summary>Frequency band specification is invalid.</summary>*)  ferInvalidBand,
      (*<summary>Pass/stop frequency bands are overlapping. You need
       some empty space between two bands for the transition region. Very narrow
       transition regions result in filters with long time delays and long
       computation times.</summary>*) ferOverlappingBands,
      (*<summary>Frequency band exceeds the frequency range.</summary>*) ferRangeError,
      (*<summary>Invalid value of Gain or Ripple. Gain can be 1, >1, or 0.
       Ripple must smaller then 1 and bigger then zero.</summary>*) ferGainRipple);

      TSignalFilter = class;

      

      (*<summary> Stores digital filter design specification. </summary>
        
<remarks>A list of frequency bands used for digital filter definition.
        The list items are owned by the list.
</remarks>
*)
      TBandFilterList = class(TStreamedList)
      strict private
        
        function  GetItems(Index: integer): TBandFilterRecord;
        procedure SetItems(Index: integer; const Value: TBandFilterRecord);
        
        procedure WriteArrayOfRecords(var Src: TBandFilterRecordArray; Dst: TStream;  Len: integer); overload;
        procedure ReadArrayOfRecords(Src: TStream; var Dst: TBandFilterRecordArray; Len: integer); overload;
        
        
      strict protected
        
        FSignalFilter: TSignalFilter;
      public
        (*<summary> Default array property allows access to individual list items. </summary>*)
        property  Item[Index: integer]: TBandFilterRecord read GetItems write SetItems; default;  
        (*<summary> Add a new band definition. </summary>
           
<remarks>Object is owned by the list and will be deleted/freed when the deleted from the list.
</remarks>
*)
        function  Add(const Item: TBandFilterRecord): integer;
        (*<summary> Delete all frequency bands which have overlapping borders. </summary>*)
        function  DeleteOverlappingBands: boolean;
        
        (*<summary> Save the list to stream. </summary>*)
        procedure SaveToStream(Dst: TStream); override;
        (*<summary> Load the list from stream. </summary>*)
        procedure LoadFromStream(Src: TStream); override;
        

        

        (*<summary> Check the validity of the filter and return the
           error as a result. BandIndex will contain the
           index of the invalid band on exit. </summary>*)
        function  CheckFilter(out BandIndex: integer): TFilterError;
        (*<summary> Sort the frequency bands by frequency. </summary>*)
        procedure Sort;
        (*<summary> Create the list and pass a valid TSignalFilter object
           as the owner. </summary>*)
        constructor Create(AOwner: TSignalFilter);
      end;

      (*<summary> Manages a list of filter definitions. </summary>
        
<remarks>Manages a list of filter definitions.
</remarks>
*)
      TFilterList = class(TStreamTemplates)
      strict private
          function  GetItems(Index: Integer): TBandFilterRecord;
          procedure SetItems(Index: Integer; const Value: TBandFilterRecord);
          procedure SetTemplate(const Value: TBandFilterList);

          function  GetTemplate: TBandFilterList;
      strict protected
          function GetStreamedTemplate: TStreamedList; override;
      public
          (*<summary> Default array property allows you to access individual list items. </summary>*)
          property  Items[Index: Integer]: TBandFilterRecord read GetItems write SetItems; default;
          (*<summary>Create the list and pass TSpectrumBandsAnalyzer component as the owner. </summary>*)
          constructor Create(AOwner: TSignalFilter);
      published

         (*<summary> Holds a list of objects. All changes made to this list of objects
          are preserved, when you change TemplateName or TemplateIndex. </summary>*)
          property Template: TBandFilterList read GetTemplate write SetTemplate; 
      end;

      (*<summary> Defines method used for FIR filter design. </summary>
        
<remarks>Defines the methods avaliable to design a FIR filter.
        For run time filter design the windowed method is recommended, because
        the resulting filters are more likely to be valid filters.
</remarks>
*)
      TFirFilterMethod = (
      (*<summary>A window based FIR filter will be designed.</summary>*) fimWindow,
      (*<summary>The remez exchange optimal fir filter design algorithm will be used.</summary>*) fimParksMcClellan);

      (*<summary> Defines filter response type. </summary>*)
      TResponseType = (
      (*<summary>A unit impulse response equals to no filter.</summary>*) rstUnit,
      (*<summary>Finite impulse response filter.</summary>*) rstFIR,
      (*<summary>FIR based differentiator.</summary>*) rstDifferentiator,
      (*<summary>FIR based double differentiator.</summary>*) rstDoubleDifferentiator,
      (*<summary>FIR based integrator.</summary>*)      rstIntegrator,
      (*<summary>FIR based double integrator.</summary>*)  rstDoubleIntegrator,
      (*<summary>90 degree phase shift all-pass FIR.</summary>*)  rstHilbert,
      (*<summary>Infinite Impulse Response filter.</summary>*)  rstIIR,
      (*<summary>Savitzky-Golay FIR.</summary>*)rstSavGolay,
      (*<summary>Moving average FIR.</summary>*) rstMovingAverage,
      (*<summary>Non-linear median filter. Requires Taps.FloatPrecision to be set.</summary>*) rstMedian,
      (*<summary>Moving average based envelope detector (FIR).</summary>*) rstEnvelope,
      (*<summary>Wavelet response.</summary>*)  rstWavelet,
      (*<summary>Delay by integer number of samples. Requires Taps.FloatPrecision to be set.</summary>*)  rstIntegerDelay,
      (*<summary>Fractional delay filter.</summary>*)  rstFractionalDelay
      );

      (*<summary> Specifies the form in which the IIR filter can be returned. </summary>*)
      TIirResultType = (
      (*<summary>Numerator and denumerator.</summary>*) irtNumDen,
      (*<summary>Zero pole form.</summary>*) irtZeroPole,
      (*<summary>State space form.</summary>*) irtStateSpace
      );

      

      

      (*<summary> Apply digital filter to the input signal. </summary>
         
<remarks>Use this component to filter a signal, select from a large
         range of different digital filters and maintain a lists
         of predefined filters. TSignalFilterDialog is the component
         editor for this object.
</remarks>
*)
      TSignalFilter = class(TSignal)
      strict private
          FFirLength: integer;
          FUpSample: integer;
          FDownSample: integer;
          FUpDelay: integer;
          FDownDelay: integer;
          FTaps: TVec;
          FExternalTaps: boolean;
          FNormalizedFrequency: Double;
          FFilters: TFilterList;
          FUseNormalizedFrequency: boolean;
          FFirMethod: TFirFilterMethod;
          FWindow: TSignalWindowType;
          FResponse: TResponseType;
          FBeta: Double;
          FGridDensity: integer;
          FIgnoreAtt: boolean;
          FPrepared: boolean;
          FOrder: integer;
          FIirMethod: TIirFilterMethod;
          FAutoFilterOrder: boolean;
          FWaveletP1: integer;
          FWaveletP2: integer;
          FWavelet: TWaveletType;
          FWaveletDecomp: TWaveletDecomp;
          FIIRFrequencyTransform: TIirFrequencyTransform;
          FScaleFactor: Double;
          FFractionalDelay: double;
          FFractionalDesignSpec: TFractionalImpulse;
          FFracDelay: double;
          procedure SetFractionalDesignSpec(const Value: TFractionalImpulse);
          procedure SetFractionalDelay(const Value: double);
          procedure SetFirLength(const Value: integer);
          procedure SetScaleFactor(const Value: Double);
          procedure SetDownSample(const Value: integer);
          procedure SetUpSample(const Value: integer);
          procedure SetDownDelay(const Value: integer);
          procedure SetUpDelay(const Value: integer);
          procedure SetNormalizedFrequency(const Value: Double);
          procedure SetFilters(const Value: TFilterList);
          procedure SetUseNormalizedFrequency(const Value: boolean);
          procedure SetFirMethod(const Value: TFirFilterMethod);
          procedure SetWindow(const Value: TSignalWindowType);
          procedure SetResponse(const Value: TResponseType);
          procedure SetBeta(const Value: Double);
          procedure SetGridDensity(const Value: integer);
          procedure SetIgnoreAtt(const Value: boolean);
          procedure SetPrepared(const Value: boolean);

          procedure SetTaps(const Value: TVec);
          procedure SetOrder(const Value: integer);
          procedure ComputeWindowFilter(FS,W1,W2,W3,W4: Double;
                                        FilterType: TFilterType; Gain,r: Double;
                                        SectionTaps: TVec);
          procedure ComputeIirFilter(ResultType: TIirResultType; const aResult: array of TObject; out aConst: Double);
          procedure ComputeParksMcClellan;
          procedure ComputeWindowOrIIr;
          procedure SetIirMethod(const Value: TIirFilterMethod);
          procedure SetAutoFilterOrder(const Value: boolean);
          procedure SetWavelet(const Value: TWaveletType);
          procedure SetWaveletP1(const Value: integer);
          procedure SetWaveletP2(const Value: integer);
          procedure SetWaveletDecomp(const Value: TWaveletDecomp);
          procedure ComputeParksHilbert;
          procedure ComputeWindowHilbert;
          procedure ComputeParksDiff;
          procedure ComputeWindowDiff;
          procedure ComputeParksDiff2;
          procedure ComputeParksIntegrator;
          procedure ComputeParksIntegrator2;
          procedure SetIIRFrequencyTransform(const Value: TIirFrequencyTransform);
 protected 
          class function  EditorClass: string; override;
      strict protected
          procedure ApplyChanges;
          function InternalUpdate: TPipeState; override;
          procedure InternalParamUpdate(Sender: TObject);override;
      public
          (*<summary> The record holds the initialized FIR
             filter ready to be passed to FirFilter routine. </summary>*)
          FIRState: TFirState;
          (*<summary> The record holds the initialized IIR
             filter ready to be passed to FirFilter routine. </summary>*)
          IIRState: TIirState;
          (*<summary> The record holds the initialized Median
             filter ready to be passed to MedianFilter routine. </summary>*)
          MedianState: TMedianState;
          DelayFilterState: TDelayFilterState;
          
          (*<summary> Same as setting Prepared to false. Reinitializes the delay lines. </summary>*)
          procedure Reset; override;

                    
          (*<summary> Returns true, if the filter has been succesfully initialized. </summary>
             
<remarks>Set Prepared to False to request reinitialization of the filter.
</remarks>
*)
          property Prepared: boolean read FPrepared write SetPrepared;
          (*<summary> Initialize streaming filter with Taps. </summary> 
<remarks>Return True if
             succesfull. This method is called by the Prepare method.
</remarks>
*)
          function Init: boolean;
          (*<summary> Prepare the defined filter for streaming. </summary>*)
          procedure Prepare;
          (*<summary> Filter a single sample. </summary>*)
          function  ProcessSample(Sample: Double): Double; overload;
          (*<summary> Filter a single sample. </summary>*)
          function  ProcessSample(const Sample: TCplx): TCplx; overload;
          (*<summary> Filter data in Src and place the result in Dst.  </summary>*)
          procedure Process(Src,Dst: TVec);
          (*<summary> Save the component state to stream. </summary>*)
          
          procedure SaveToStream(Stream: TStream); override;
          (*<summary> Load the component from stream. </summary>*)
          procedure LoadFromStream(Stream: TStream); override;
          
          

          procedure Assign(Source: TPersistent); override;
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
          

          (*<summary> Returns the currently designed IIR filter in state space form.  </summary>
             
<remarks>If the current design is not an IIR filter, the function returns
             false. For this call to return True, the IIRFrequencyTransform
             must be ftStateSpaceAnalog.
</remarks>
*)
          function StateSpaceIIR(a: TMtx; b,c: TVec; d: Double): boolean;
          (*<summary> Returns the currently designed IIR filter in zero pole form. </summary>
             
<remarks>If the current design is not an IIR filter, the function returns
             false.
</remarks>
*)
          function ZeroPoleIIR(z,p: TVec; k: Double): boolean;
          (*<summary> Returns the currently designed IIR filter in zero pole form. </summary>
             
<remarks>If the current design is not an IIR filter, the function returns
             false.
</remarks>
*)
          function TransferFunIIR(num,den: TVec): boolean;
      published
          
          property  Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
          
          
          (*<summary> Holds a list of defined filters. </summary>*)
          property Filters: TFilterList read FFilters write SetFilters;

          
          (*<summary> Holds the taps for the filter.  </summary>
              
<remarks>This taps can be FIR or IIR type.
             If taps are from an IIR filter, they are stored as second order sections
             interleaved like this: B0[0], B0[1], B0[2], A0[0], A0[1], A0[2], B1[0], B1[1], B1[2], A1[0], A1[1], A1[2],...
</remarks>
*)
          property Taps: TVec read FTaps write SetTaps;

          
          (*<summary> The upsample factor used by the multi-rate FIR filter. </summary>*)
          property UpSample: integer read FUpSample write SetUpSample default 1;

          
          (*<summary> The phase delay  (must be less then UpSample) used by the
             multi-rate FIR filter. </summary>*)
          property UpDelay: integer read FUpDelay write SetUpDelay default 0;

          
          (*<summary> The downsample factor used by the multi-rate FIR filter. </summary>*)
          property DownSample: integer read FDownSample write SetDownSample default 1;

          
          (*<summary> The phase delay  (must be less then DownSample) used by the
             multi-rate FIR filter. </summary>*)
          property DownDelay: integer read FDownDelay write SetDownDelay default 0;

          
          (*<summary> Set this property to True, if you have defined your own filter
             response and have placed the taps in the Taps property. </summary>
             
<remarks>Be sure to define the correct Response, to match the type of your filter. (Iir of Fir)
             When changing this property, the component has to be notified when the taps
             have been changed. Set the Prepared property to False or call the Init method
             after changing the values of taps.
</remarks>
*)
          property ExternalTaps: boolean read FExternalTaps write FExternalTaps  default false ;

          
          (*<summary> Specifies the FIR filter design method to be used. </summary>*)
          property FirMethod: TFirFilterMethod read FFirMethod write SetFirMethod  default fimWindow ;

          
          (*<summary> Specifies the IIR filter design method to be used. </summary>*)
          property IirMethod: TIirFilterMethod read FIirMethod write SetIirMethod  default fimButter ;

          
          (*<summary> Specifies the IIR frequency transformation method to use. </summary>*)
          property IIRFrequencyTransform: TIirFrequencyTransform read FIIRFrequencyTransform write SetIIRFrequencyTransform  default ftStateSpaceAnalog ;

          
          (*<summary> Specifies the window type to be used when designing windowed FIR filters. </summary>*)
          property Window: TSignalWindowType read FWindow write SetWindow  default wtKaiser ;

          
          (*<summary> Specifies the type of the filter, to be designed. </summary>*)
          property Response: TResponseType read FResponse write SetResponse  default rstUnit ;

          
          (*<summary> Beta is the parameter for the Kaiser window.</summary>
             
<remarks>Kaiser window is used to design a FIR filter when Window property is set to
             wtKaiser and FirMethod property is set to to fimWindowed.
             The value of this property is used only, if IgnoreAtt is set to True.
             If IgnoreAtt is False, the beta parameter is estimated from the requested
             stopband ripple.
</remarks>
*)
          property Beta: Double read FBeta write SetBeta;

          
          (*<summary> Grid density used by the remez exchange algorithm for optimal
             FIR filter design. </summary>*)
          property GridDensity: integer read FGridDensity write SetGridDensity default 16;

          
          (*<summary> Defines the order of the IIR filters, if AutoFilterOrder is false. </summary>
             
<remarks>The order parameter is used also by the Savitzky Golay polynomial FIR filter.
</remarks>
*)
          property Order: integer read FOrder write SetOrder default 4;

          
          (*<summary> The value of the normalized frequency used when defining multiband filters. </summary>
             
<remarks>Normalized frequency is usually 1 or 2. The Nyquist frequency is then 0.5 or 1.
</remarks>
*)
          property NormalizedFrequency: Double read FNormalizedFrequency write SetNormalizedFrequency;

          
          (*<summary> True, if you want to normalize the frequency of the defined bands. </summary>
             
<remarks>Frequency normalized filters will not have its pass/cuttoff frequencies
             defined absolutely in Hz, but rather as a fraction of the sampling frequency.
</remarks>
*)
          property UseNormalizedFrequency: boolean read FUseNormalizedFrequency write SetUseNormalizedFrequency  default True ;

          
          (*<summary> If True, the Order/FirLength of the IIR/FIR filters will be automatically estimed. </summary>*)
          property AutoFilterOrder: boolean read FAutoFilterOrder write SetAutoFilterOrder  default True ;

          
          (*<summary> Defines the length of the FIR/Median filter, if the AutoFilterOrder property is false. </summary>
             
<remarks>For windowed FIR filters other then Kaiser, the length can not be estimated.
</remarks>
*)
          property FirLength: integer read FFirLength write SetFirLength default 1;

          
          (*<summary> Set it to True, if you want to explicitelly define the Beta parameter
             for the Kaiser window, when designing a FIR filter windowed with a Kaiser window. </summary>*)
          property IgnoreAtt: boolean read FIgnoreAtt write SetIgnoreAtt  default False ;

          
          (*<summary> Set the wavelet to be used for wavelete decomposition. </summary>*)
          property Wavelet: TWaveletType read FWavelet write SetWavelet  default wtHaar ;

          
          (*<summary> Defines the first wavelet parameter (if required). </summary>*)
          property WaveletP1: integer read FWaveletP1 write SetWaveletP1 default 1;

          
          (*<summary> Defines the second wavelet parameter (if required.) </summary>*)
          property WaveletP2: integer read FWaveletP2 write SetWaveletP2 default 1;

          
          (*<summary> Selects the result of wavelet decomposition. </summary>*)
          property WaveletDecomp: TWaveletDecomp read FWaveletDecomp write SetWaveletDecomp  default wtApproximation ;

          
          (*<summary> Defines the factor by which to multiply the input signal. </summary>
             
<remarks>The purpose of this parameter is to save a separate scale
             operation after the filtering.
</remarks>
*)
          property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
          
          (*<summary> Defines the combined fractional delay of the input signal (integer + fraction). </summary>
             
<remarks>Uses fractional FIR filter with Kaiser window. This filter is combined with
             Integer sample delay filter to achieve desired combined delay. The fractional filter will always
             also include some integer sample delay. How much depends on the filter specification defined with
             FractionaDesignSpec property. If the specified (integer part) of the delay is too short for the specified
             FractionalDesignSpec, an exception will be raised. Considered only, when Response is rstFractionalDelay.
</remarks>
*)
          property FractionalDelay: double read FFractionalDelay write SetFractionalDelay;
          
          (*<summary> Defines the FIR filter properties used for fractional delay filtering. </summary>
             
<remarks>Used only, when Response is rstFractionalDelay.
</remarks>
*)
          property FractionalDesignSpec: TFractionalImpulse read FFractionalDesignSpec write SetFractionalDesignSpec default falp_100dB;
      end;

      (*<summary> Specifies signal types, which can be generated by TSignalGenerator. </summary>
         
<remarks>Defines the type of the signal you want to generate.
</remarks>
*)

      TFuncSignalType = (
      (*<summary>P1-frequency, P2-amplitude, P3-phase.</summary>*) funSine,
      (*<summary>No parameters. Used for filter design.</summary>*) funUnitImpulse,
      (*<summary>Defined with duty cycles:  High time - P1, Raise time - P3, Fall time - P4, Amplitude of High - P5.</summary>*) funImpulse,
      (*<summary>P1-frequency, P2-amplitude, P3-phase; P4-asymetry.</summary>*) funTriangle,
      (*<summary>P1-freqyency, P2-phase, P3-amplitude, P4-DC offset.</summary>*) funSquare,
      (*<summary>P1-mean, P2-stddev.</summary>*) funRandGauss,
      (*<summary>P1-Low, P2-high.</summary>*) funRandUniform,
      (*<summary>P1-starting frequency, P3 - starting amplitude, P5-ending frequency, P6-ending amplitude, P7- period [s].</summary>*) funChirp,
      (*<summary>P1-constant value, P2-imaginary part, if constant is complex.</summary>*) funConstant,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput0,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput1,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput2,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput3,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput4,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput5,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput6,
      (*<summary>Used to reference the Inputs property of the TSignalFilter component.</summary>*) funInput7,
      (*<summary>No signal.</summary>*) funNone
      );

      (*<summary> Defines the operator types used by expression parser. </summary>
        
<remarks>Defines the operator type for TSignalGenerator.
</remarks>
*)
      TOperSignalType = (
      (*<summary>Addition.</summary>*) opAdd,
      (*<summary>Subtraction.</summary>*) opSub,
      (*<summary>Multiplication.</summary>*) opMul,
      (*<summary>Division.</summary>*) opDiv,
      (*<summary>Sine function</summary>*) opSin,
      (*<summary>Sine function</summary>*) opCos,
      (*<summary>Tangens function</summary>*) opTan,
      (*<summary>Exponent function.</summary>*) opExp,
      (*<summary>Natural logarithm.</summary>*) opLn,
      (*<summary>Logarithm with base 10.</summary>*) opLog,
      (*<summary>Rectification.</summary>*) opAbs,
      (*<summary>Round to the closest integer.</summary>*) opRound,
      (*<summary>Truncate towards zero.</summary>*) opTrunc,
      (*<summary>P1-lower clip limit, P2-upper clip limit.</summary>*) opLimit,
      (*<summary>No operation.</summary>*) opNon
      );

      (*<summary> Defines the type of the operand for expression parser. </summary>
        
<remarks>Defines the type of the operand for the expression parser of
        TSignalGenerator component.
</remarks>
*)
      TOpType = (
      (*<summary>The operand is a function.</summary>*)optFunction,
      (*<summary>The operand is an operator.</summary>*) optOperator
      );

      TCToneState = class
      State: TToneState;
      end;


      TCTriangleState = class
      State: TTriangleState;
      end;

      TCSquareState = class
      State: TSquareToneState;
      end;


      TFuncSignalRecordDouble = packed record
          Checked: byte;
          (*<summary> Specifies if operand is a function or operator. </summary>*)
          OpType: byte;
          (*<summary> Specifies operand if function type. </summary>*)
          Func: byte;
          (*<summary> Specifies operand if operator type. </summary>*)
          Oper: byte;
          (*<summary> True, if the operation is complex. </summary>*)
          Cplx: byte;
          (*<summary> Value of the 1st parameter. </summary>*)
          P1: double;
          (*<summary> Value of the 1nd parameter. </summary>*)
          P2: double;
          (*<summary> Value of the 3rd parameter. </summary>*)
          P3: double;
          (*<summary> Value of the 4th parameter. </summary>*)
          P4: double;
          (*<summary> Value of the 5th parameter. </summary>*)
          P5: double;
          (*<summary> Value of the 6th parameter. </summary>*)
          P6: double;
          (*<summary> Value of the 7th parameter. </summary>*)
          P7: double;
          Continuous: byte;
          ResetContinuous: byte;
          Data: Int32;
          Pointer: Int32;
          Counter: Int32;
          Done: byte;
      end;


      TFuncSignalRecordDoubleArray = array of TFuncSignalRecordDouble;


     (*<summary> Expression parser record definition. </summary>
         
<remarks>This record holds the definition of a formula element.
         Formula element can be either a function or an operator.
         (OpType). It can be checked or unchecked. Complex or real..
</remarks>
*)
      TFuncSignalRecord = class
          (*<summary> If True, this element will be used in the function evaluation. </summary>*)
          Checked: boolean;
          (*<summary> Defines, if this element is a function or an operator. </summary>*)
          OpType: TopType;
          (*<summary> Defines which function to generate, if OpType is optFunction.. </summary>*)
          Func: TFuncSignalType;
          (*<summary> Specifies which operator to apply, if OpType is optOperator. </summary>*)
          Oper: TOperSignalType;
          (*<summary> If True, the operator or function will have a complex result. </summary>*)
          Cplx: boolean;
          (*<summary> Function parameter. </summary>*)
          P1: double;
          (*<summary> Function parameter. </summary>*)
          P2: double;
          (*<summary> Function parameter. </summary>*)
          P3: double;
          (*<summary> Function parameter. </summary>*)
          P4: double;
          (*<summary> Function parameter. </summary>*)
          P5: double;
          (*<summary> Function parameter. </summary>*)
          P6: double;
          (*<summary> Function parameter. </summary>*)
          P7: double;
          (*<summary> True, if the function is to be streamed. </summary>*)
          Continuous: boolean;
          (*<summary> If True, the function will be reset to its initial state
                       on its next evaluation. </summary>*)
          ResetContinuous: boolean;
          (*<summary> A pointer to the TVec object type holding the result. </summary>*)
          Data: TVec;
          (*<summary> A pointer to signal generation specific structures. </summary>*)
          Pointer: TObject;
          (*<summary> A "Time counter" used by some signal generation functions. </summary>*)
          Counter: Int64;
          (*<summary> Internal parameter. </summary>*)
          Done: boolean;
          (*<summary> Assign all fields from Src. </summary>*)
          procedure Assign(Src: TFuncSignalRecord); overload;
          (*<summary> Assign all fields from Src. </summary>*)
          procedure Assign(const Src: TFuncSignalRecordDouble); overload;
          (*<summary> Assign all fields to Dst. </summary>*)
          procedure AssignTo(var Dst: TFuncSignalRecordDouble); overload;
        end;

      (*<summary> Holds expresion definition. </summary>
        
<remarks>The list holds the formula to be computed in
        postfix notation. (HP calculator style). The elements
        of the list are owned by the list.
</remarks>
*)

      TFuncSignalList = class(TStreamedList)
      strict private
        function GetItems(Index: integer): TFuncSignalRecord;
        procedure SetItems(Index: integer; const Value: TFuncSignalRecord);
        
        procedure WriteArrayOfRecords(var Src: TFuncSignalRecordDoubleArray; Dst: TStream;  Len: integer); overload;
        procedure ReadArrayOfRecords(Src: TStream; var Dst: TFuncSignalRecordDoubleArray; Len: integer); overload;
        

        
      public
        constructor Create(AOwnsObjects: boolean); overload;
        (*<summary> Add a new formula item definition and allocate the record internally. </summary>*)
        function Add(const Item: TFuncSignalRecord): integer;
        
        (*<summary> Save the list to stream. </summary>*)
        procedure SaveToStream(Dst: TStream); override;
        (*<summary> Load the list from stream. </summary>*)
        procedure LoadFromStream(Src: TStream); override;
        

        
        (*<summary> Default array property allows access to individual list items. </summary>*)
        property Item[Index: integer]: TFuncSignalRecord read GetItems write SetItems; default; 
      end;


      (*<summary> Triggered when reseting function phase. </summary>
        
<remarks>Triggers when the function's phase is reset to its initial value.
</remarks>
*)
      TOnResync = procedure(Sender: TObject; LineIndex: integer) of object;

      (*<summary> Manages a list of different sound formula definitions. </summary>
        
<remarks>Manages a list of different sound definitions.
</remarks>
*)

      TSignalSounds = class(TStreamTemplates)
      strict private
          function  GetItems(Index: Integer): TFuncSignalRecord;
          procedure SetItems(Index: Integer; const Value: TFuncSignalRecord);
          function  GetTemplate: TFuncSignalList;
      strict protected
          function GetStreamedTemplate: TStreamedList; override;
      public

          constructor Create(AOwner: TComponent); override;



         (*<summary> Holds a list of TFuncSignalRecord objects. </summary>
           
<remarks>All changes made to this list of objects
           are preserved, when you change TemplateName or TemplateIndex.
</remarks>
*)
          property Template: TFuncSignalList read GetTemplate; 
          (*<summary> Default array property allows you to access individual list items. </summary>*)
          property  Items[Index: Integer]: TFuncSignalRecord read GetItems write SetItems; default;
      end;

      

     

      (*<summary> Generates signals in real time. </summary>
         
<remarks>Use this component as a function parser for signal generator
         applications. The component features a stack based vectorized
         function evaluator. This component allows you to generate
         very complex signals ten to hundred times
         times faster then a simple function evaluator.
         The component has a component editor TSignalGeneratorDialog.
</remarks>
*)

      TSignalGenerator = class(TSignal)
      strict private
          stack: TFuncSignalList;
          a: array [0..20] of TFuncSignalRecord;

          FOnResync: TOnResync;
          FSounds: TSignalSounds;
          FPrepared: boolean;
          FComputeMessage: string;
          procedure ResetStack;
          procedure InitFunc(aj: TFuncSignalRecord);
          procedure FillValues(aj: TFuncSignalRecord);
          procedure ResetFuncs(aj: TFuncSignalRecord);
          procedure Compute;

          procedure SetOnResync(const Value: TOnResync);
          procedure SetSounds(const Value: TSignalSounds);
          procedure SetPrepared(const Value: boolean);
          procedure SetComputeMessage(const Value: string);
 protected 

          class function EditorClass: string; override;
      strict protected
          function  InternalUpdate: TPipeState; override;
          procedure InternalParamUpdate(Sender: TObject);override;
          procedure UpdateSetLength(Sender: TObject); override;
      public
          (*<summary> Contains any error messages encountered while trying
             to evalute the functions. </summary>*)
          property ComputeMessage: string read FComputeMessage write SetComputeMessage;

                    
          (*<summary> Returns true, if all function generators are initialized. </summary>*)
          property Prepared: boolean read FPrepared write SetPrepared;
          (*<summary> Update the function generator with the new parameters
             from the sounds property item at position i. </summary>*)
          procedure PartialUpdate(i: integer);
          (*<summary> Resets all functions to its initial phase. </summary>*)
          procedure ResyncAll;
          (*<summary> Resets the function at TemplatLineIndex position to its
             initial phase. </summary>*)
          procedure ResyncFunction(TemplateLineIndex: integer);
          (*<summary> Initialize the function (sound) to be generated. </summary>*)
          procedure Prepare;
          (*<summary> Initialize the function (sound) to be generated. </summary>*)
          procedure Reset; override;

          
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
          
          procedure SaveToStream(Stream: TStream); override;
          procedure LoadFromStream(Stream: TStream); override;
          

      published

           
           
          property  Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored; 

          
          
          (*<summary> Signal generator sound definition object. </summary>*)
          property Sounds: TSignalSounds read FSounds write SetSounds;





         
          (*<summary> Event triggered when the user calls the ResyncFunction method. </summary>*)
          property OnResync: TOnResync read FOnResync write SetOnResync;
          
      end;

      (*<summary> Used for multi-rate signal filtering. </summary>
         
<remarks>Used by TSignalMultiRate component to enable multi-stage
         filtering. Multi-stage filtering can be faster then
         a single stage filter, if you need very sharp cutoff
         frequencies. Data contains the filtered data on the current stage
         and State holds the filter definitions for the current filter stage.
</remarks>
*)
      TTapsStages = record
          Data: TVec;
          State: TFirState;
      end;

      
      (*<summary> Abstract class for multi-rate digital filters. </summary>
        
<remarks>Abstract class for multi-stage, multi-rate decimation and
        interpolation FIR filters.

        An in depth explanation of the methods used can be found in [1]
        Chapter 7.3 (Digital resampling).

<b>References: </b> <para/>
[1] "Understanding digital signal processing.", Richard G. Lyons, Prentice-Hall, 2001.
</remarks>


<SeeAlso cref="TBandlimitedInterpolator"/>
<SeeAlso cref="TSignalDecimator"/>
<SeeAlso cref="TSignalInterpolator"/>
<SeeAlso cref="TSignalRateConverter"/>*)
      TSignalMultiRate = class(TSignal)
      strict protected
        TempSignal: TSignal;
        TapsStages: array [0..MaxRateConversionPowerFactor-1] of TTapsStages;
        Stages: integer;
        FInitialized: boolean;
        FExternalTaps: boolean;
        FOnGetFilterTaps: TNotifyEvent;
        FFirInitialized: boolean;
        FFilterDelay: Double;
        FScaleFactor: Double;
        FAudioSignal: boolean;
        FHalfBand: boolean;
        procedure SetAudioSignal(const Value: boolean);
        procedure SetInitialized(const Value: boolean);
        procedure SetExternalTaps(const Value: boolean);
        procedure SetOnGetFilterTaps(const Value: TNotifyEvent);
        procedure SetFirInitialized(const Value: boolean);
        procedure SetFilterDelay(const Value: Double);
        procedure SetScaleFactor(const Value: Double);
        function GetFilterDelay: Double;
      strict protected
        FFactor: integer;
        FRipple: Double;
        FTransBW: Double;
        procedure AllocFirBuffers; virtual; abstract;
        procedure FreeFirBuffers; virtual; abstract;
        procedure Initialize; virtual;
        procedure SetRipple(const Value: Double);
        procedure SetTransBW(const Value: Double);
        procedure SetFactor(const Value: integer); virtual;
        procedure SetHalfBand(const Value: boolean); virtual;
        property  FirInitialized: boolean read FFirInitialized write SetFirInitialized;
      public
        (*<summary> Contains the FIR filter taps for all filter stages except for the last. </summary>*)
        FirTaps: TVec;
        (*<summary> Contains the FIR filter taps for the last filter stage in case
           of decimation and for the first stage filter in case of
           the interpolator. </summary>
           
<remarks>When the sampling frequency is increased by twice
           in case of the interpolation, the signal has one zero
           inserted after each sample. This zero stuffing introduces
           high frequency noise which is exactly the mirror of the low
           frequencies. To filter out this high frequencies a lowpass
           filter is needed. If the sampling frequency is 2Hz, then
           the lowpass filter should pass everything below 0.5Hz and stop
           everything above 0.5Hz. The TransBW property defines
           the width of the transition region of the lowpass filter
           centered on 0.5 Hz. The narrower the transition region, the
           longer the filter and more CPU demanding computation is required.
</remarks>
*)
        FinalStageTaps: TVec;
        (*<summary> The same as setting Initialized to false. </summary>
                     
<remarks>Resets all delay lines.
</remarks>
*)
        procedure Reset; override;

        
        (*<summary> When set to false, the next call to the update method
           will reinitialize the FIR filters. </summary>
           
<remarks>This is usefull, when ExternalTaps is True. Do not reset the filter while streaming
           is under way.
</remarks>
*)
        property Initialized: boolean read FInitialized write SetInitialized;
        (*<summary> Returns the filter delay in number of samples of the final
           sampling frequency. </summary>
           
<remarks>Read-only.
</remarks>
*)
        property FilterDelay: Double read GetFilterDelay write SetFilterDelay;

        (*<summary> Filter Src and place the result in Dst. </summary>*)
        procedure Process(Src, Dst: TVec); virtual;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        (*<summary> FirTaps and FinalStageTaps do not hold
           valid filter taps until InitFilters has
           been called. </summary>
           
<remarks>The routine is called automatically before the first data
           block is filtered.
</remarks>
*)
        procedure InitFilters; virtual;
      published
         
       (*<summary> Adjust filters for audio signal. </summary>
          
<remarks>When designing filters, the transition region can allow
          some aliasing to reduce the filter load in the last 5% of the
          frequency spectrum. From the analysis point of view, if the
          last 5% are just attenuated and/or also aliased plays no role.
          If the signal is an audio file, those 5% of aliasing would add some real
          audible distortion to the signal. If the AudioSignal processing is True,
          no aliasing will occure.
</remarks>
*)
        property AudioSignal: boolean read FAudioSignal write SetAudioSignal  default false ;

         
        (*<summary> Enables user specified filter. </summary>
           
<remarks>If set to false, the user is required to
           set the filter taps stored in FirTaps
           and FinalStageTaps fields.
           This property is True by default.
           The filters taps should be set before the first
           call to the Update method is made. Alternatively
           the OnGetFilterTaps event is also triggered, when
           the filter taps are needed. The component has to be notified
           to use new set of taps, once they were changed. Set the Initialized
           property to false to notify the component that new taps
           should be used.
</remarks>
*)
        property ExternalTaps: boolean read FExternalTaps write SetExternalTaps  default False ;
        
        property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

        
        (*<summary> Triggered when ExternalTaps is false. </summary>
          
<remarks>User should set values of Taps and FinalStageTaps vectors.
</remarks>
*)
        property OnGetFilterTaps: TNotifyEvent read FOnGetFilterTaps write SetOnGetFilterTaps;

        
        (*<summary> Defines the factor by which the signal will be scaled. </summary>
            
<remarks>This scaling operation is "free" since it does not require additional CPU power, but a change to
           the scale factor requires reinitialization of the filter bank.
           If ExternalTaps are used or the OnGetFilterTaps event, the value of the
           ScaleFactor is ignored.  ScaleFactor can not be zero.
</remarks>
*)
        property ScaleFactor: Double read FScaleFactor write SetScaleFactor;

        
        (*<summary> Filters out upper half of the bandwidth. </summary>
          
<remarks>In case of decimation, the last filter will filter upper half of the
          result from 0.5 to 1, if the sampling frequency is 2. In case of interpolation,
          the first filter will filter out the upper half from 0.5 to 1.
</remarks>
*)
        property HalfBand: boolean read FHalfBand write SetHalfBand  default false ;

        
        (*<summary> Maximum ripple allowed in the passband and in the stopband. </summary>*)
        property Ripple: Double read FRipple write SetRipple;

        
        (*<summary> Decimation/Interpolation factor. </summary>*)
        property Factor: integer read FFactor write SetFactor default 2;

        
        (*<summary> Defines the width of the transition band of the FIR filter used for interpolation/decimation. </summary>
           
<remarks>The normalized sampling frequency is 2. This parameter applies only
           to the last stage of the decimation and the first stage
           of interpolation.
</remarks>
*)
        property TransBW: Double read FTransBW write SetTransBW;
      end;

      

      

      (*<summary> Performs signal interpolation. </summary>
         
<remarks>Signal interpolation component. Use
         this component to raise the sampling
         frequency of the signal by power of two integer factor
         using fast multi-rate, multi-stage filters (See [1]).
         The CPU load increases nearly linear with the
         interpolation factor.<para/>

<b>References: </b> <para/>
<see href="[1] Understanding digital signal processing, Richard G. Lyons, Prentice-Hall, 2001."/>
</remarks>


<SeeAlso cref="TSignalDecimator"/>
<SeeAlso cref="TBandlimitedInterpolator"/>
<SeeAlso cref="TSignalRateConverter"/>*)
      TSignalInterpolator = class(TSignalMultiRate)
      strict protected
        procedure FreeFirBuffers; override;
        procedure AllocFirBuffers; override;
        function  InternalUpdate: TPipeState ; override;
      public

        constructor Create(AOwner: TComponent); override;
        
      published




      end;

      

      

       (*<summary> Performs signal decimation. </summary>
         
<remarks>Signal decimation component. Use
         this component to lower the sampling
         frequency of the signal by powers of two
         using fast multi-rate, multi-stage filters (See [1]).
         The component also features a high quality
         envelope detection, which computes the absolute
         value of the source signal before the decimation.
         Decimation factors can be as high as 1024 or 4096. This makes it possible
         to detect very low frequencies with the envelope
         detector. The CPU load increases by only about twice
         when the decimation
         factor is increased from 2 to any number (can be 1024). <para/>

<b>References: </b> <para/>
<see href="[1] Understanding digital signal processing., Richard G. Lyons, Prentice-Hall, 2001."/>
</remarks>


<SeeAlso cref="TSignalInterpolator"/>
<SeeAlso cref="TBandlimitedInterpolator"/>
<SeeAlso cref="TSignalRateConverter"/>*)
      TSignalDecimator = class(TSignalMultiRate)
      strict protected
        FAbsVec: TVec;
        FEnvelopeFactor: integer;
        FLastStageFilterOnly: boolean;
        procedure SetLastStageFilterOnly(const Value: boolean);
      strict protected
        procedure ApplyDecimation(Src: TVec); virtual;
        procedure FreeFirBuffers; override;
        procedure AllocFirBuffers; override;
        function  InternalUpdate: TPipeState; override;
        procedure SetFactor(const Value: integer); override;
        procedure SetEnvelopeFactor(const Value: integer); virtual;
        procedure SetHalfBand(const Value: boolean); override;
      public

        destructor Destroy; override;
        
        constructor Create(AOwner: TComponent); override;

      published
        
        (*<summary> Defines the filter stage at which to apply envelope
           detection. </summary>
           
<remarks>If envelopeFactor is 0 then envelope detection is left out.
           If EnvelopeFactor is 1, then envelope detection is performed before
           the first lowpass filter is applied. If envelopeFactor
           is 2, the envelope detection is applied after the first
           lowpass, and so on...
</remarks>
*)
        property EnvelopeFactor: integer read FEnvelopeFactor write SetEnvelopeFactor default 0;





        
        (*<summary> The component lowers the sampling frequency
           in steps by a factor of two. </summary>
           
<remarks>Before each stage a lowpass antialiasing filter is applied.
           If this property is True, the final stage lowpass filter will be applied,
           but the signal will not be downcoverted by a further factor of 2.
</remarks>
*)
        property LastStageFilterOnly: boolean read FLastStageFilterOnly write SetLastStageFilterOnly  default false ;
      end;

      

      

      (*<summary> Changes sampling frequency by a rational factor. </summary>
        
<remarks>The sampling frequency of the Input is changed
        by a rational factor by using bandlimited interpolation.
        An example is resampling of an audio signal with
        48kHz to 44kHz. The interpolation quality can achieve
        SNR (signal to noise ratio) of up to 180dB in double
        precision. This component does not feature
        antialising filters. More about backgrounds of
        bandlimited interpolation can be found in [1].

        Note: Data.Length may vary from
        iteration to iteration. If the input has integer
        number of samples and the rate is changed by a rational
        factor the output will have a rational number of samples
        (in general). For some pairs of input buffer length
        and resampling factors (least common nominator)
        the output will also have fixed length. If the frequency
        is changed from 8000Hz to 11025Hz, the input buffer
        must have a length which is a multiple of 320 and the output
        will be a multiple of 441. (8000/25 = 320, 11025/25 = 441) <para/>

<b>References: </b> <para/>
<see href="http://ccrma-www.stanford.edu/~jos/resample/resample.html">[1] Bandlimited interpolation</see>
</remarks>


<SeeAlso cref="TSignalRateConverter"/>
<SeeAlso cref="TSignalInterpolator"/>
<SeeAlso cref="TSignalDecimator"/>*)
      TBandlimitedInterpolator = class(TSignal)
      strict private
        FilterPrepared: boolean;
        FractionalDelay: Double;
        Buffer: TVec;
        FractionalTaps: TVec;
        Breaks: TVec;
        Coeffs: TMtx;
        FResampleFactor: Double;
        FFilterStart: Double;
        FImpulseFactor: integer;
        FFilterStop: Double;
        FRipple: Double;
        FAutoFilterLength: boolean;
        FFilterLength: integer;
        FScaleFactor: Double;
        FFilterDelay: Double;
        procedure SetFilterStart(const Value: Double);
        procedure SetFilterStop(const Value: Double);
        procedure SetImpulseFactor(const Value: integer);
        procedure SetResampleFactor(const Value: Double);
        procedure SetRipple(const Value: Double);
        procedure SetAutoFilterLength(const Value: boolean);
        procedure Linear1DSetup(YTaps: TVec);
        procedure SetScaleFactor(const Value: Double);
        function GetFilterDelay: Double;
      strict protected
        Taps: TVec;
        (*<summary> If True, the length of the interpolation filter will be computed and the
           FilterLength property will be set, when the PrepareFilter method
           is called. </summary>*)
        property AutoFilterLength: boolean read FAutoFilterLength write SetAutoFilterLength default false;
        (*<summary> Defines the starting frequency of the transition
           band of the FIR lowpass filter used for interpolation. </summary>*)
        property FilterStart: Double read FFilterStart write SetFilterStart;
        (*<summary> Defines the stop frequency of the transition
           band of the FIR lowpass filter used for interpolation. </summary>*)
        property FilterStop: Double read FFilterStop write SetFilterStop;
        (*<summary> Defines the interpolation accuracy of the fractional
           FIR impulse. </summary>*)
        property ImpulseFactor: integer read FImpulseFactor write SetImpulseFactor;
        function InternalUpdate: TPipeState; override;
        procedure SetFilterLength(const Value: integer); virtual;
      public
        (*<summary> Returns length of the filter. </summary>
           
<remarks>Read FilterLength after the PrepareFilter method was called, or
           after the first call the Update method was made to obtain
           the length of FIR filter used for interpolation.

           Filter length affects the following parameters:
          *the width of transition region of the lowpass filter.
          *the level of attenuation in the stopband of the lowpass filter.
          *the quality of interpolation.

           The filter must be long enough to meet the most strict
           specification. The automatic FIR length estimation takes
           in to account only the width of the transition region
           and the level of attenuation, but not the quality
           of interpolation. The longer the filter, the higher is the
           interpolation quality.

           The interpolation quality is also affected by the
           value of the ImpulseFactor property which defines
           the factor by which the FIR impulse response is oversampled
           (linear interpolation grid density)
           and the amount of allowed Ripple in the passband.
           Smaller ripple forces the edges
           of the impulse response quicker to zero
           thus making the "truncated" impulse response
           less sensitive to the assumption that taps
           are zero outside the given impulse response.

           Another important issue is the aliasing. If the resamplingFactor
           is smaller than 1 (the sampling frequency is being lowered), then
           the lowpass filter must attenuate all frequencies above
           the FS*ResamplingFactor frequency. (FS = original sampling frequency.)

           Usually it is best to use multirate FIR filters as
           antialiasing filters and then use this component
           only to apply interpolation where the interpolation
           filter is very short (7,9,11 or 13 taps).
           Such a setup can achieve SNR (Signal to noise ratio)
           up to 85dB. With more taps SNR of 180dB (double precision)
           can be achieved.

           The value of the allowed ripple which
           defines the beta parameter of the Kaiser window
           has a significant impact on the quality of the
           interpolation. If lowpass antialiasing filter is applied
           with a separate filter, then the lowpass interpolation
           filter can concentrate on the quality of
           interpolation only.
</remarks>
*)
        property FilterLength: integer read FFilterLength write SetFilterLength;
        (*<summary> Sets the internal FilterPrepared flag to false.
           When the Update method is called the next time,
           it will call the PrepareFilter method first. </summary>*)
        procedure Reset; override;
        (*<summary> Returns the delay of the filter in number of samples of the final sampling frequency. </summary>*)
        property FilterDelay: Double read GetFilterDelay;
        (*<summary> Initializes the lowpass interpolation filter. </summary>*)
        procedure PrepareFilter; virtual;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published
        
        (*<summary> Defines the ripple error of the anti-aliasing filters. </summary>
          
<remarks>The attenuation in dB can can be computed as:
          Att = -20*log10(Ripple). The ripple can fall in
          range between 0.001 (60dB) and 1E-8 (160dB) for double precision
          and between 0.001 and 0.0001 (80dB) for single precision.
</remarks>
*)
        property Ripple: Double read FRipple write SetRipple;

        
        (*<summary> Specifies the factor by which to change the sampling frequency. </summary>
           
<remarks>The new sampling frequency is computed as:
           <c>

    SamplingFrequency = Input.SamplingFrequency*ResampleFactor

           </c>
           The recommendation is that the factor should take values only between 0.5 and 1. For
           values smaller (larger) TSignalDecimator (TSignalInterpolator)
           should be used as a preprocessor. It is the users responsibility
           to ensure antialiasing protection. All the signals
           entering (with normalized sampling frequency 2Hz)
           may not have frequencies above 0.5Hz and not above
           the ResampleFactor Hz. Usually this condition can
           be met by interpolating the signal first
           with the TSignalInterpolator component by a factor of 2.

           The resample factor can change and the filter bank will not be recalculated.
</remarks>
*)
        property ResampleFactor: Double read FResampleFactor write SetResampleFactor;

        
        property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

        
        (*<summary> Scaling factor can optionally be applied free of charge without
          raising the cost of the processing required. </summary>
           
<remarks>The scale factor can not be zero.
</remarks>
*)
        property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
      end;

      TVariableBandlimitedInterpolator = class(TBandlimitedInterpolator)
      strict private
        FResampleVector: TVec;
        ResampleBuffer: TVec;
        procedure SetResampleVector(const Value: TVec);
      strict protected
        function InternalUpdate: TPipeState; override;
      public

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

        (*<summary> Specifies resample points as units of samples. </summary>*)
        property ResampleVector: TVec read FResampleVector write SetResampleVector;
      end;

      

      

      (*<summary> Changes the sampling frequency by any integer or rational factor. </summary>
         
<remarks>This component makes use of <see cref="TSignalDecimator"/>,
         <see cref="TSignalInterpolator"/>
         and <see cref="TBandlimitedInterpolator"/> for fast (real time)
         rate conversion. The quality of the rate
         conversion depends upon the floating point precision.
         With single precision no more then 80dB SNR (Signal to noise ratio)
         can be obtained. Double precision allows SNR up to
         180 dB. The limiting factor is the quality of the
         bandlimited interpolation. Bandlimited interpolation is used
         only, if the factor is not an integer power of two.
         Bandlimited interpolation with SNR of 80dB
         requires about 5x more CPU then simple power of two
         interpolation. Very high SNR up to 160dB may
         increase the required processing power by
         further 3x. (which may no longer be real time)

         If explicit multi-rate filtering is desired
         the TSignalFilter component should be used where the
         UpSample and DownSample factors and filter type can be defined
         explicitely.
</remarks>
*)
      TSignalRateConverter = class(TSignal)
      strict protected
        Initialized: boolean;
        DownSample: TSignalDecimator;
        UpSample: TSignalInterpolator;
        FirstUpsample: TSignalInterpolator;
        BLSample: TBandlimitedInterpolator;
        FRipple: Double;
        FFactor: Double;
        FTransBW: Double;
        FAudioSignal: boolean;
        FScaleFactor: Double;
        FHalfBand: boolean;
        FFilterDelay: Double;
        procedure SetAudioSignal(const Value: boolean);
        procedure SetRipple(const Value: Double);
        procedure SetTransBW(const Value: Double);
        procedure SetScaleFactor(const Value: Double);
        function GetFilterDelay: Double;
      strict protected
        EpsR: Double;
        function  InternalUpdate: TPipeState; override;
        procedure SetHalfBand(const Value: boolean); virtual;
        procedure SetFactor(const Value: Double); virtual;
      public
        (*<summary> Initializes filtering structures after Factor and other properties have been set. </summary>
                     
<remarks>This function can be called optionally before the filtering the starts. The component
                     will call it automatically, if it was not yet called, before the filtering will start.
</remarks>
*)
        procedure Initialize; virtual;
        (*<summary> The bandwidth will be halved during downsampling. </summary>
          
<remarks>This property is applicable only during downsampling (Factor &lt; 1).
          When the value is True, the resulting bandwidth will have the
          upper half filtered out, which can be usefull for furhter processing.
          The lowpass will be applied from BW/2 to BW, where BW is the bandwidth
          of the converted signal. The final sampling frequency will not be affected.
</remarks>
*)
        property HalfBand: boolean read FHalfBand write SetHalfBand;
        (*<summary> Returns the filter delay in number of samples of the final
           sampling frequency. </summary> 
<remarks>Read-only.
</remarks>
*)
        property FilterDelay: Double read GetFilterDelay;
        (*<summary> Reset all delay lines. </summary>*)
        procedure Reset; override;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published

        
        (*<summary> Enable when the signal processed is Audio signal. </summary>
          
<remarks>When designing filters, the transition region can allow
          some aliasing to reduce the filter load in the last 5% of the
          frequency spectrum. From the analysis point of view, if the
          last 5% are just attenuated and/or also aliased plays no role.
          If the signal is an audio file, those 5% of aliasing could add some real
          audible distortion to the signal. If the AudioSignal processing is True,
          no aliasing will occure, but the transition bandwidth will double to 10% of
          the total bandwidth.
</remarks>
*)
        property AudioSignal: boolean read FAudioSignal write SetAudioSignal  default True ;

        
        property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

        
        (*<summary> Defines the ripple of the passband and of the stopband
          of the FIR lowpass filter used for interpolation. </summary>*)
        property Ripple: Double read FRipple write SetRipple;

        
        (*<summary> Factor by which to change the sampling frequency
           of the signal connected to the Input property. </summary>*)
        property Factor: Double read FFactor write SetFactor;

        
        (*<summary> Defines the width of the transition band of the FIR filter
           used for interpolation/decimation. </summary>
           
<remarks>The normalized sampling frequency is 2. This parameter applies only
           to the last stage of the decimation and the first stage
           of interpolation. If TransBW is 0.1 then the last 10% of the
           bandwidth of the original will be aliased on the right edge.
</remarks>
*)
        property TransBW: Double read FTransBW write SetTransBW;

        
        (*<summary> Additional scaling factor which is applied without extra
           performance cost. </summary> 
<remarks>Scale factor can not be 0.
</remarks>
*)
        property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
      end;

      

      

      (*<summary> Demodulates a signal. </summary>
         
<remarks>Use the component to bring a
         prespecified frequency band down to DC.
         The process is also called amplitude demodulation.
         By lowering the sampling frequency using
         high speed decimation, very
         narrow frequency bands can be extracted in real time.
         The resulting signal
         can then be used for high-resolution (mili/micro Hertz).
         frequency analysis of the obtained frequency band.
         This process is also referred to as zoom-spectrum.
</remarks>
*)

      TSignalDemodulator = class(TSignalRateConverter)
      strict protected
        FSubBandWidth: Double;
        FSubBandFrequency: Double;
        DownToneState: TToneState;
        UpToneState: TToneState;
        ToneData: TVec;
        WorkSignal: TSignal;
        WorkConverter: TSignalRateConverter;
        procedure SetSubBandWidth(const Value: Double);
        procedure SetSubBandFrequency(const Value: Double);
      strict protected
        procedure ComputeFactor; virtual;
        function  InternalUpdate: TPipeState; override;
        procedure SetFactor(const Value: Double); override;
        procedure SetHalfBand(const Value: boolean); override;
      public

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

        procedure Initialize; override;
      published

        
        (*<summary> Defines the starting frequency of the frequency band to be extracted. </summary>
           
<remarks>The normalized sampling frequency is 1 and this value can be between 0 and 0.5.
</remarks>
*)
        property SubBandFrequency: Double read FSubBandFrequency write SetSubBandFrequency;

        
        (*<summary> Defines the width of the frequency band to be demodulated. </summary>
           
<remarks>Demodulation will move the frequency band of interest down to the DC.
           The normalized sampling frequency is 1 and this value can be between 0 and 0.5.
           SubBandFrequency + SubBandWidth may not exceed 0.5 (FS/2).
</remarks>
*)
        property SubBandWidth: Double read FSubBandWidth write SetSubBandWidth;
      end;

      

      

      (*<summary> Modulates a signal. </summary>
         
<remarks>Use the component to move the signal to a specified carrier frequency.
         The process is also called modulation.
         By increasing the sampling frequency using high speed linear phase interpolation,
         narrow frequency bands can be moved to any carrier frequency in real time.
         This is the inverse procedure to the signal demodulator. The target sampling
         frequency and carrier frequency can be independently controlled. The procedure
         generates only one sided upper side band, to the right of the carrier.

         The target sampling frequency can be specified with the Factor property and
         the CarrierFrequency property defines the carrier frequency.

         Example:
         Signal.SamplingFrequency := 10;  //in Hz
         SignalModulator.Input := Signal;
         SignalModulator.Factor := 12;
         SignalModulator.CarrierFrequency := 4.5;

         The output sampling frequency will be: 10*12 = 120Hz
         The output signal will have non-zero frequencies from 45 to 50Hz which
         will hold the information originally stored in the range from 0 to 5Hz.
</remarks>
*)
      TSignalModulator = class(TSignalRateConverter)
      strict protected
        DownToneState: TToneState;
        UpToneState: TToneState;
        ToneData: TVec;
        WorkSignal: TSignal;
        WorkConverter: TSignalRateConverter;
        FCarrierFrequency: Double;
        procedure SetCarrierFrequency(const Value: Double);
      strict protected
        function  InternalUpdate: TPipeState; override;
        procedure SetFactor(const Value: Double); override;
        procedure SetHalfBand(const Value: boolean); override;
        procedure UpdateToneState;
      public

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

        procedure Initialize; override;
      published
        
        (*<summary> Specifies where to move the Input signal in the frequency domain. </summary>
         
<remarks>The unit for the CarrierFrequency is a multiple of the Input.SamplingFrequency.
</remarks>
*)
        property CarrierFrequency: Double read FCarrierFrequency write SetCarrierFrequency;
      end;

      

      

      (*<summary> High speed narrow band linear phase bandpass filter. </summary>
         
<remarks>Use the component to apply a narrow band linear phase
         bandpass filter. Bandpass filters which would
         otherwise require 10 000 or 100 000 taps long impulse
         responses, can be applied at a small fraction of the
         processing cost. When the filter pass band width, is less than
         25% of the total signal bandwidth it becomes meaningfull to use this
         component. For bandpass widths of less than 1%, the processing requires
         10-100x less CPU than direct filter methods.
</remarks>
*)

      TSignalBandpass = class(TSignalDemodulator)
      strict protected
        UpConverter: TSignalRateConverter;
      strict protected
        function  InternalUpdate: TPipeState; override;
        procedure ComputeFactor; override;
      public

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

        procedure Initialize; override;
      end;

      



      

        (*<summary> Variable frequency rate converter. </summary>
         
<remarks>Use the component to vary the sampling frequency as a function
         of time. The function is passed as a vector of sampling points
         specified at the sampling frequency of the input signal with
         RateVector property.

         The component uses a fixed filter bank and delivers aliasing
         free output, with linear phase and user definable attenuation
         of the lowpass filters and witdh of the FIR transition band
         in the final signal.

         Fixed filter bank allows for exceptional performance as it can take
         full advantage code vectorization.

         The filter bank is specified by defining the MaxFactor and MinFactor
         properties, which define the maximum and minimum factor by which the
         sampling frequency can change with the reference to the sampling frequency
         of the input signal. Within this range, the sampling frequency can then
         be freely adjusted from one sample to the next without danger of aliasing.

         The performance of the rate conversion process depends upon the
         MaxFactor/MinFactor where the processing load will increase nearly
         linearly with the increase of these factors.
</remarks>
*)

      TSignalVariableRateConverter = class(TSignalRateConverter)
      strict protected
        intFilterDelay: integer;
        FRateVector: TVec;
        FSampleVector: TVec;
        FMaxFactor: Double;
        FMinFactor: Double;
        BLSample2: TVariableBandlimitedInterpolator;
        procedure SetRateVector(const Value: TVec);
        procedure SetMaxFactor(const Value: Double);
        procedure SetMinFactor(const Value: Double);
      strict protected
        function  InternalUpdate: TPipeState; override;
      public
        procedure Initialize; override;
        (*<summary> Defines sample positions at which to resample the Input signal. </summary>
                     
<remarks>To increase the sampling frequency by 2x this vector would contain
                     values: 0, 0.5, 1, 1.5, 2...

                     To decrease sampling frequency by 2x this vector would contain
                     values: 0, 2, 4, 6, 8....

                     The sampling frequency can also vary from sample to sample:
                     0, 2, 4.5, 6.3, 8....

                     However, the maximum increase and maximum decrease factors have
                     to be specified as parameters to MaxFactor and MinFactor properties.
                     Attempting to sample the signal out of this range would cause aliasing
                     in the signal.
</remarks>
*)
        property RateVector: TVec read FRateVector write SetRateVector;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published
        (*<summary> Specifies the maximum Factor up to which the rate converter can vary the
          sampling frequency, without introducing aliasing. </summary>
          
<remarks>The minimum value for the MaxFactor is 1. The value must be an integer
          and a power of two (2, 4, 8,...).
</remarks>
*)
        property MaxFactor: Double read FMaxFactor write SetMaxFactor;
        (*<summary> Specifies the minimum Factor down to which the rate converter can vary
           the sampling, without introducing aliasing. </summary>
           
<remarks>The maximum value for the minFactor is limited to 1. The value must be
           an inverse of an integer with power of two (1/2, 1/4, 1/8 etc...).
</remarks>
*)
        property MinFactor: Double read FMinFactor write SetMinFactor;
      end;



      

      

     (*<summary> Demodulates a signal. </summary>
       
<remarks>Uses a hilbert transformer filter to get 90 degrees phase
       shifted version of the original and then forms the envelope
       like this:
       <code>
       envelope[i] = sqrt(sqr(X[i-HilbertDelay]) + sqr(Y[i]));
       </code>
       This is an alternative way to detect the envelope in
       comparison to the method offered by <see cref="TSignalDecimator"/>.
       This approach is slower, but could result in significant
       noise reduction.
</remarks>
*)

      TSignalEnvelopeDetector = class(TSignalDecimator)
      strict protected
        HilbertTaps: TVec;
        HilbertState: TFirState;
        DelayState: TDelayFilterState;
        FHilbertRipple: Double;
        FHilbertTransBW: Double;
        procedure SetHilbertRipple(const Value: Double);
        procedure SetHilbertTransBW(const Value: Double);
      strict protected
        procedure FreeFirBuffers; override;
        procedure AllocFirBuffers; override;
        function InternalUpdate: TPipeState; override;
      public

        procedure InitFilters; override;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        

      published
        
        (*<summary> Transition bandwidth of the type III Hilbert transformer normalized to a sampling frequency of 2 Hz. </summary>*)
        property HilbertTransBW: Double read FHilbertTransBW write SetHilbertTransBW;

        
        (*<summary> Passband ripple of the type III Hilbert transformer. </summary>*)
        property HilbertRipple: Double read FHilbertRipple write SetHilbertRipple;
      end;


      (*<summary> Defines signal buffering type. </summary>
         
<remarks>Defines how the data is managed within the buffer.
</remarks>
*)

      TBufferType = (
      (*<summary>Once the data is read, it is "deleted" from the buffer.
         This mode is usefull for filtering and signal processing.</summary>*) bftStreaming,
      (*<summary>The data resides in the buffer until new data comming in to the
         buffer pushes it out. This mode is usefull for signal analysis.</summary>*) bftScrolling,
      (*<summary>The buffer will be increasing
         until some user defined size is reached. The buffer must then be reset.</summary>*) bftStoring
        );

      

      

       (*<summary> Keeps signal processing data pipe synchronized. </summary>
         
<remarks>Use the component to synchronize data processing
         pipe, where you need a large input buffer to perform
         filtering or a large scrolling buffer for
         signal analysis. The output data length is
         specified with the value of the Length property.
         This value may be bigger or smaller then the
         value of Input.Length property. The size
         of the buffer must be at least Max(Input.Length,Length).
         The size of the buffer can also be set automatically
         by the component, if you set AutoBufferSize to True.
</remarks>
*)

      TSignalBuffer = class(TSignal)
      strict protected
          Buffer: TVec;
          BufferState: TCircularBufferState;
          FBufferType: TBufferType;
          FOverlapping: Double;
          FBufferSizeLimit: integer;
          FIncrementStep: Double;
          FetchDataNextTime: boolean;
          FStrideLength: integer;
          StridePosition: integer;
          procedure SetBufferType(const Value: TBufferType);
          procedure SetOverlapping(const Value: Double);
          procedure SetBufferSizeLimit(const Value: integer);
          procedure SetIncrementStep(const Value: Double);
          
          procedure SetStrideLength(const Value: integer);
          procedure RewindCircularBuffer(var aLen: integer);
      strict protected
          function InternalUpdate: TPipeState; override;
      public
         (*<summary> Reads Dst.Length of most recent data, without advancing the read or write cursor. </summary>
                      
<remarks>The function returns true, if the buffer was long enough to fill the dst, independently of
                      the state of the buffer.
</remarks>
*)
          function Monitor(Dst: TVec; MonitorMode: TMonitorMode): boolean;
          (*<summary> Reads any data still in the buffer, but less than specified Length. Missing samples become zeros. </summary>*)
          function ReadReminder: boolean;

          function InternalPull: TPipeState; override;
          constructor Create(AOwner: TComponent); override;
          destructor Destroy; override;
          

        (*<summary> Returns true, if there is enough data in buffer to fill Data.Length. </summary>
                     
<remarks>Call this function, if you want to prevent the Pull to update components connected to the Input
                     and only want to use up the data already buffered.
</remarks>
*)
          function DataInBuffer: boolean;
          (*<summary> Returns the number of samples currently stored in the buffer. </summary>*)
          function BufferedSamplesCount: integer;
          (*<summary> Reset the buffer. </summary>*)
          procedure Reset(); overload; override;
          (*<summary> Reset the buffer. </summary>*)
          procedure Reset(BufferSize: integer);  reintroduce;  overload;
         (*<summary> Returns True, if the internal buffer has been underrun.</summary>
            
<remarks>This means that reading from the buffer was to quick and some he data not
            yet written was read.
</remarks>
*)
          function BufferUnderrun: boolean;
         (*<summary> Returns True, if the internal buffer has been overrun. </summary>
            
<remarks>This means that writing to the buffer was to quick and the some of the data not
            yet read has been overwritten.
</remarks>
*)
          function BufferOverrun: boolean;
          (*<summary> Returns the current circular buffer write position. </summary>*)
          function BufferWritePosition: integer;
          (*<summary> Returns the current circular buffer read position. </summary>*)
          function BufferReadPosition: integer;
          (*<summary> Defines the length of the internal buffer in samples. </summary>*)
          function BufferLength: integer;
          (*<summary> Reads data from internal Buffer to the Data property. </summary>
             
<remarks>Returns true, if sufficent data was available. Data.Length
             defines the amount of data to read.
</remarks>
*)
          function Read: boolean;
          (*<summary> Writes data to the buffer and resizes the buffer, if it is
             not big enough to hold everything. </summary>*)
          procedure Write; overload;
          (*<summary> Writes data to the buffer and resizes the buffer, if it is
             not big enough to hold everything. </summary>*)
          procedure Write(Src: TVec); overload;
          (*<summary> Writes data to the buffer and resizes the buffer, if it is
             not big enough to hold everything. Converts precision single/double if needed. </summary>*)
          procedure WriteTo(Src: TVec); overload;
      published
         
         (*<summary> Defines the minimum factor by which the buffer size will be increased. </summary>*)
         property IncrementStep: Double read FIncrementStep write SetIncrementStep;

         
         (*<summary> Overlapping defines the percent of previously read data to be read again. </summary>
            
<remarks>Old data is discarded. Overlapping is used, if the buffer is in bftStreaming  mode.
</remarks>
*)
         property Overlapping: Double read FOverlapping write SetOverlapping;

         
         (*<summary> StrideLength defines the number of samples that will be skipped between
            consecutive reads from the buffer. </summary>
            
<remarks>If StrideLength is 1, then on each read
            the buffer would move forward for 1 sample only. That would mean (1-1/Data.Length)*100%
            of overlapping. Setting StrideLength will set Overlapping to -1
            and Overlapping parameter is ignored.
            When overlapp would have to be negative or there is a need to specify
            fordward step in number of samples, set the strideLength. It is possible to set
            StrideLength to 1000 and have Data.Lengt of 100. This means that the buffer
            will ignore 900 samples out of every 1000 written.
</remarks>
*)
         property StrideLength: integer read FStrideLength write SetStrideLength default 0;

         
         (*<summary> Defines the type of the buffer. </summary>*)
         property BufferType: TBufferType read FBufferType write SetBufferType  default bftStreaming ;

         
         (*<summary> Limits the maximum buffer size, if AutoBufferSize is True. </summary>
            
<remarks>If this limit is breached, an exception is raised. Applicable only when BufferType equals
            bftStoring
</remarks>
*)
         property BufferSizeLimit: integer read FBufferSizeLimit write SetBufferSizeLimit default 2000000;

        
         property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
      end;

      

      

      (*<summary> Stores data and allows indexed access to elements. </summary>
        
<remarks>Stores all incoming data in the Data property increasing
        the length as neccessary without too many resizes. Uses SetSubRange to report the correct
        length of the Data property.
</remarks>
*)
      TSignalStoreBuffer = class(TSignal)
      strict protected
         FIncrementStep: Double;
         FActualBufferSize: integer;
         FInitialBufferSize: integer;
         FMaxBufferSize: integer;
         procedure SetIncrementStep(const Value: Double);
         procedure SetActualBufferSize(const Value: integer);
         procedure SetInitialBufferSize(const Value: integer);
         procedure SetMaxBufferSize(const Value: integer);
         procedure PushDataInBuffer;
      strict protected
         function InternalUpdate: TPipeState; override;
         procedure SetIsDouble(const Value: boolean); override;
         procedure SetFloatPrecision(const Value: TMtxFloatPrecision); override;
     public
         procedure Append(Src: TVec); reintroduce; overload;
         procedure Append(Sample: Double); overload; override;
         (*<summary> Reset the buffer size to initial size and no data. </summary>*)
         procedure Reset; override;
         (*<summary> Shift the data left or right by Offset samples. </summary>*)
         procedure Shift(Offset: integer);

         constructor Create(AOwner: TComponent); override;
         

      published

         
         (*<summary> If this property is bigger than 0, only latest MaxBufferSize samples
            will be retained in the buffer. </summary>*)
         property MaxBufferLength: integer read FMaxBufferSize write SetMaxBufferSize default -1;

         
       (*<summary> InitialBufferSize defines the initial size of the buffer in the number of samples. </summary>*)
         property InitialBufferSize: integer read FInitialBufferSize write SetInitialBufferSize default 16;

         
         (*<summary> Current buffer size in number of samples. </summary>
           
<remarks>The number of samples actually stored is available via the Length property.
</remarks>
*)
         property ActualBufferSize: integer read FActualBufferSize write SetActualBufferSize;

         
        (*<summary> When the buffer is too small to store a new block of data, it will be increased at
          least by a factor of IncrementStep. </summary>*)
         property IncrementStep: Double read FIncrementStep write SetIncrementStep;

        
         property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
      end;

      

      

       (*<summary> Manages a list of TSignalStoreBuffer components. </summary>
        
<remarks>Manage a list of TSignalStoreBuffer components.
</remarks>
*)
      TSignalStoreBufferList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalStoreBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalStoreBuffer);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalStoreBuffer read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;



      

      

      (*<summary> Manages a list of TSignalToComplexList components. </summary>
        
<remarks>Manage a list of TSignalToComplexList components.
</remarks>
*)

      TSignalToComplexList = class(TSignalList)
      strict private
          FRealInputs: TSignalList;
          FImagInputs: TSignalList;
          function GetItems(index: integer): TSignalToComplex; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalToComplex);
          procedure SetImagInputs(const Value: TSignalList);
          procedure SetRealInputs(const Value: TSignalList);
      strict protected
          function AddItem: TMtxComponent; override;
          procedure InternalMatchInputs; override;
      public
          
          constructor Create(AOwner: TComponent); override;
          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalToComplex read GetItems write SetItems; default; 
      published
          
          property RealInputs: TSignalList read FRealInputs write SetRealInputs;
                    
          property ImagInputs: TSignalList read FImagInputs write SetImagInputs;
      end;



      

      

       (*<summary> Manages a list of TSignalBufferList components. </summary>
                    
<remarks>Manage a list of TSignalBufferList components.
</remarks>
*)

      TSignalBufferList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalBuffer);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalBuffer read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

      (*<summary> Manages a list of TSignalFilter components. </summary>
        
<remarks>Manage a list of TSignalFilter components.
</remarks>
*)

      TSignalFilterList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalFilter; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalFilter);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalFilter read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

   (*<summary> Manage a list of TSignalGenerator components. </summary>
       
<remarks>Manage a list of TSignalGenerator components.
</remarks>
*)
      TSignalGeneratorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalGenerator; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalGenerator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalGenerator read GetItems write SetItems; default; 
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 

          
      end;



      
      TSpectrumFilter = class(TSignal)
      strict private
          FSmoothing: integer;
          procedure SetSmoothing(const Value: integer);
      strict protected
          function InternalUpdate: TPipeState; override;
      public
          constructor Create(AOwner: TComponent); override;
      published
          property Smoothing: integer read FSmoothing write SetSmoothing;
          
          property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 
      end;



      

      

     (*<summary> Manage a list of TSignalIncBuffer components.  </summary>
       
<remarks>Manage a list of TSignalIncBuffer components.
</remarks>
*)

      TSignalIncBufferList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalIncBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalIncBuffer);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalIncBuffer read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

     (*<summary> Manage a list of TSignalIncBuffer components. </summary>
       
<remarks>Manage a list of TSignalIncBuffer components.
</remarks>
*)
      TSignalDecBufferList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalDecBuffer; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalDecBuffer);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalDecBuffer read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      


      

     (*<summary> Manage a list of TSignalDecimator components. </summary>
       
<remarks>Manage a list of TSignalDecimator components.
</remarks>
*)
      TSignalDecimatorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalDecimator; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalDecimator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalDecimator read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

     (*<summary> Manage a list of TSignalInterpolator components. </summary>
       
<remarks>Manage a list of TSignalInterpolator components.
</remarks>
*)
      TSignalInterpolatorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalInterpolator; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalInterpolator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalInterpolator read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

     (*<summary> Manage a list of TSignalDemodulator components. </summary>
       
<remarks>Manage a list of TSignalDemodulator components.
</remarks>
*)
      TSignalDemodulatorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalDemodulator; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalDemodulator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalDemodulator read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

      (*<summary> Manage a list of TSignalModulator components. </summary>
       
<remarks>Manage a list of TSignalModulator components.
</remarks>
*)

      TSignalModulatorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalModulator; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalModulator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalModulator read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

     (*<summary> Manage a list of TSignalBandpass components. </summary>
       
<remarks>Manage a list of TSignalBandpass components.
</remarks>
*)
      TSignalBandpassList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalBandpass; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalBandpass);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalBandpass read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      

      

     (*<summary> Manage a list of TSignalRateConverter components. </summary>
       
<remarks>Manage a list of TSignalRateConverter components.
</remarks>
*)

      TSignalRateConverterList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalRateConverter; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalRateConverter);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalRateConverter read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      



      

     (*<summary> Manage a list of TSignalVariableRateConverter components. </summary>
       
<remarks>Manage a list of TSignalVariableRateConverter components.
</remarks>
*)

      TSignalVariableRateConverterList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalVariableRateConverter; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalVariableRateConverter);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalVariableRateConverter read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;



      

      

     (*<summary> Manage a list of TBandlimitedInterpolator components. </summary>
       
<remarks>Manage a list of TBandlimitedInterpolator components.
</remarks>
*)

      TBandlimitedInterpolatorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TBandlimitedInterpolator; reintroduce;
          procedure SetItems(index: integer; const Value: TBandlimitedInterpolator);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TBandlimitedInterpolator read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;

      


      

     (*<summary> Manage a list of TSignalEnvelopeDetector components. </summary>
       
<remarks>Manage a list of TSignalEnvelopeDetector components.
</remarks>
*)

      TSignalEnvelopeDetectorList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalEnvelopeDetector; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalEnvelopeDetector);
      strict protected
          function AddItem: TMtxComponent; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalEnvelopeDetector read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 

          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a single component. Setting the value of this property
            to other then nil, will set Inputs property to nil.
</remarks>
*)

          property Input: TAnalysis read FInput write SetInput; 
      end;

      

      

     (*<summary> Manage a list of TSignalDemux components. </summary>
       
<remarks>Manage a list of TSignalDemux components.
</remarks>
*)

      TSignalDemuxList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalDemux; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalDemux);
      strict protected
          function AddItem: TMtxComponent; override;
          procedure InternalMatchInputs; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalDemux read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 

          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a single component. Setting the value of this property
            to other then nil, will set Inputs property to nil.
</remarks>
*)

          property Input: TAnalysis read FInput write SetInput; 
      end;

      

      

     (*<summary> Manage a list of TSignalMux components. </summary>
       
<remarks>Manage a list of TSignalMux components.
</remarks>
*)

      TSignalMuxList = class(TSignalList)
      strict private
          
          function GetItems(index: integer): TSignalMux; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalMux);
      strict protected
          function AddItem: TMtxComponent; override;
          procedure InternalMatchInputs; override;
      public

          
          constructor Create(AOwner: TComponent); override;

          (*<summary> Default array property allows access to individual list items. </summary>*)
          property Items[index: integer]: TSignalMux read GetItems write SetItems; default; 
          
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs;
      end;

(*<summary> Convert a function type to string. </summary>
  
<remarks>Convert a TFuncSignalType type to string.
</remarks>
*)
function FuncToString(const par: TFuncSignalType): string; overload;
(*<summary> Convert a string to function type. </summary>
  
<remarks>Convert a string to TFuncSignalType type.
</remarks>
*)
function StringToFunc(const par: string): TFuncSignalType; overload;
(*<summary> Convert a string to operand type. </summary>
  
<remarks>Convert a string to TOperSignalType type.
</remarks>
*)
function StringToOp(const par: string): TOperSignalType; overload;
(*<summary> Convert an operand type to string. </summary>
  
<remarks>Convert an TOperSignalType type to string.
</remarks>
*)
function OpToString(const par: TOperSignalType): string; overload;



