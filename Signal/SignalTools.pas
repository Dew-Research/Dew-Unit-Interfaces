










{$I BdsppDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}


(*<summary>General purpose components for signal processing and signal analysis.</summary>*)
unit SignalTools;


interface

uses  MtxVec, Math387, MtxBaseComp, SignalUtils, MtxVecBase, AbstractMtxVec, MtxVecInt

      

      

      
        ,SysUtils
        ,Classes

        
        ,Contnrs
        

      
      ;





type






    (*<summary>Defined to support double/single compatibility.</summary>
        
<remarks>The record type is defined to support component configuration
        compatibility between signal processing package compiled in double and single precision
</remarks>
*)
      TMarkRecordDouble = packed record
          Freq: double;
          Amplt: double;
          Phase: double;
          Time: double;
          end;

      
      TMarkRecordDoubleArray = array of TMarkRecordDouble;
      

      (*<summary>Stores mark position and value info.</summary>
         
<remarks>The type used by TSpectrumAnalyzer component to keep track of marks on the frequency spectrum.
         It can also be used to to keep track of marks on the time series.
</remarks>
*)
      TMarkRecord = class
          public
          Freq: Double;
          Amplt: Double;
          Phase: Double;
          Time: Double;
          procedure Assign(Src: TMarkRecord); overload;
          procedure Assign(const Src: TMarkRecordDouble); overload;
          end;

    (*<summary>Defines how the marks will be sorted.</summary>
      
<remarks>The type is used to indicate by which parameter to sort the marks.
</remarks>
*)
    TSortType = (
    (*<summary>Sort by frequency ascending.</summary>*)srtFreq,
    (*<summary>sort by amplitude ascending.</summary>*)srtAmplt,
    (*<summary>sort by phase ascending.</summary>*)srtPhase,
    (*<summary>sort by time ascending.</summary>*)srtTime,
    (*<summary>sort by frequency descending.</summary>*)srtFreqDescending,
    (*<summary>sort by amplitude descending.</summary>*)srtAmpltDescending,
    (*<summary>sort by phase descending.</summary>*)srtPhaseDescending,
    (*<summary>sort by time descending.</summary>*)srtTimeDescending
    );

    (*<summary>Defines the measuring units for the phase.</summary>
      
<remarks>Defines the units in which phase of the frequency component is calculated.
</remarks>
*)
    TPhaseUnits = (
    (*<summary>the phase will be calculated in radians.</summary>*)puRad,
    (*<summary>the phase will be calculated in degrees.</summary>*)puDeg);

    (*<summary>Defines the range of the computed phase spectrum.</summary>*)
    TPhaseRange = (
    (*<summary>phase ranges from -Pi..+Pi.</summary>*)prPiPi,
    (*<summary>phase ranges from 0..2*Pi.</summary>*)prZeroTwoPi);

    (*<summary>Defines how to handle the phase of the frequency spectrum.</summary>*)
    TPhaseMode = (
    (*<summary>The phase will not be calculated (via ArcTan2) and you can save some computation time.</summary>*)pmIgnore,
    (*<summary>The phase will be computed within -Pi..+Pi range in radians,
      or within -180..+180 range in degrees.</summary>*)pm360,
    (*<summary>Compute the phase delay.</summary>*)pmPhaseDelay,
    (*<summary>Compute group delay in samples.</summary>*)pmGroupDelay);

    (*<summary>Manages a list of marks.</summary>
      
<remarks>Handles, sorts and maintaines a list of TMarkRecord records.
</remarks>
*)
          TMarkList = class(TObjectsList)
          strict protected
              
              FSortType: TSortType;
              

              
              procedure WriteArrayOfRecords(var Src: TMarkRecordDoubleArray; Dst: TStream;  Len: integer); overload;
              procedure ReadArrayOfRecords(Src: TStream; var Dst: TMarkRecordDoubleArray; Len: integer); overload;
              
              function  GetValue(Index: integer): TMarkRecord;
              procedure SetValue(Index: integer; Mark: TMarkRecord);
              procedure SetSortType(const Value: TSortType);
              procedure SetCount(const Value: integer); reintroduce;
              function GetCount: integer; reintroduce;
          public
              property Count: integer read GetCount write SetCount; 
          (*<summary>Default array property allows you to access any record element by index.</summary>*)
              property  Item[Index: Integer]: TMarkRecord read GetValue write SetValue; default; 
          (*<summary>Defines the parameter by which to sort the list of TMarkRecord records.</summary>*)
              property  SortType: TSortType read FSortType write SetSortType;
          (*<summary>Copy all data from Source.</summary>*)
              procedure Assign(Source: TMarkList);
          (*<summary>Add a new Mark to the list. The method creates a copy of the Mark record and adds it to the list.</summary>*)
              function  Add(const Mark: TMarkRecord): integer; overload;
          (*<summary>Add a new Mark to the list.</summary>*)
              function  Add: integer; overload;
             
             
          (*<summary>Save all marks to Dst stream.</summary>*)
              procedure SaveToStream(Dst: TStream); overload;
          (*<summary>Load marks from the Src stream.</summary>*)
              procedure LoadFromStream(Src: TStream); overload;
              
          (*<summary>Sort the list according to the value of the SortType property.</summary>*)
              procedure Sort;
          (*<summary>Sort the list relative to the Amplt field of the TMarkRecord in ascending order.</summary>*)
              procedure SortAmplt;
          (*<summary>Sort the list relative to the Freq field of the TMarkRecord in ascending order.</summary>*)
              procedure SortFreq;
          (*<summary>Sort the list relative to the Phase field of the TMarkRecord in ascending order.</summary>*)
              procedure SortPhase;
          (*<summary>Sort the list relative to the Time field of the TMarkRecord in ascending order.</summary>*)
              procedure SortTime;

          (*<summary>Sort the list relative to the Amplt field of the TMarkRecord in descending order.</summary>*)
              procedure SortAmpltDescending;
          (*<summary>Sort the list relative to the Freq field of the TMarkRecord in descending order.</summary>*)
              procedure SortFreqDescending;
          (*<summary>Sort the list relative to the Phase field of the TMarkRecord in descending order.</summary>*)
              procedure SortPhaseDescending;
          (*<summary>Sort the list relative to the Time field of the TMarkRecord in descending order.</summary>*)
              procedure SortTimeDescending;
          (*<summary>Copy contents of list item fields to corresponding dense array storage.</summary>*)
              procedure CopyToDense(const dstFreq, dstAmplt, dstPhase: TVec); overload;
          (*<summary>Copy contents of list item fields to corresponding dense array storage.</summary>*)
              procedure CopyToDense(const dstAmplt, dstTime: TVec); overload;
          (*<summary>Copy contents of list item fields from corresponding dense array storage.</summary>*)
              procedure CopyFromDense(const srcFreq, srcAmplt, srcPhase: TVec); overload;
          (*<summary>Copy contents of list item fields from corresponding dense array storage.</summary>*)
              procedure CopyFromDense(const srcAmplt, srcTime: TVec); overload;
          (*<summary>Copy contents of list item Freq field to dstFreq.</summary>*)
              procedure CopyFreqToDense(const dstFreq: TVec);
              
              constructor Create;
              
          end;

       (*<summary>Defines the state of the processing pipe.</summary>
         
<remarks>There are four possible states. Two of them, pipeStream and
         pipeBuffer, occure only, if TSignalBuffer component is connected
         in the processing chain.
</remarks>
*)
         TPipeState = (
         (*<summary>The connected component has returned a valid block of data.</summary>*)pipeOK,
         (*<summary>The connected component has not returned a valid block of data,
            because there is no more data to be processed. The connected
            components have not recalucated (no data). The processing can stop.</summary>*)pipeEnd,
         (*<summary>The buffer has not returned a new block of data,
            but the stream has not ended yet. All components in
            the chain before the buffer have been recalculated.</summary>*)pipeStream,
         (*<summary>The connected component has returned a valid block of data,
            but it came from the buffer and the update request did not
            go beyond the buffer component. All components from the
            buffer forward have recalculated. All components in the chain
            before the buffer have not yet recalculated.</summary>*)pipeBuffer
         );

       (*<summary>Abstract class for all DSP components.</summary>
          
<remarks>The basic object of the Signal processing package components implements
          behaviour common to all derived components with a number of protected
          properties and methods, which can be made public (published), if appropriate, in
          the derived classes. The component implements two basic mechanisms:
          A way to connect components in to a chain of components and
          and way to distribute centralized recalculate request to all
          components in the chain. The components are connected via two
          different properties: Input (Single channel) or Inputs (multiple channel).
          The recalculation request for one component is done with call of the Update method and
          the recalcuation request for the entire chain of components is done with a call
          of the Pull method of the last component in the chain.
          Most procesing components (blocks) can also function as servers
          featuring their own component editors. They do not have to be
          conected with other components.
</remarks>
*)
       TSignal = class;
       TSpectrum = class;
       TAnalysis = class;

       (*<summary>Abstract item class that enables connectivity of components.</summary>
         
<remarks>Abstract item class that enables connectivity of signal processing components.
</remarks>
*)
       TMtxConnectItem = class(TMtxCollectionItem)
       strict protected
          FReference: TReferenceList;
          FOnInputUpdated: TNotifyEvent;
          procedure SetOnInputUpdated(const Value: TNotifyEvent);
       public
          (*<summary>The event will be called when the Input is Updated.</summary>*)
          property OnInputUpdated: TNotifyEvent read FOnInputUpdated write SetOnInputUpdated;
       
       
          constructor Create(Collection: TCollection); override;
       
          destructor Destroy; override;
          
          procedure CustomEvent(Sender: TObject); override;
       end;

       (*<summary>Item class that enables connectivity of components.</summary>
         
<remarks>Item class that enables connectivity of signal processing components.
</remarks>
*)
       TSignalConnectItem = class(TMtxConnectItem)
       strict protected
          FDirty: boolean;
          FInput: TAnalysis;
          procedure SetInput(const Value: TAnalysis);
          procedure SetDirty(const Value: boolean);
          
          procedure ReferenceRemoved(Sender: TObject);
          
       public
          
          (*<summary>Used internally to signal new data.</summary>
            
<remarks>Used internally to signal if the Input has been updated
            or not. The Input property must be set by the component connected
            to the Input property as soon as it has fresh data. Set this
            property from inside InternalUpdate or InternalPull when
            developing new signal processing component.
</remarks>
*)
          property Dirty: boolean read FDirty write SetDirty;
          (*<summary>To connect the component as input assign it to this property.</summary>*)
          property Input: TAnalysis read FInput write SetInput;
          
          
          constructor Create(Collection: TCollection); override;
          procedure Assign(Source: TPersistent); override;
          
          
          (*<summary>Set Input to nil.</summary>*)
          procedure Reset;

       end;

      (*<summary>Collection item.</summary>
         
<remarks>The class supports handling of TSignal as a TCollection item
         and maintains a list of references to TSignal objects.
         This list is editable from within Delphi IDE in design-time.
</remarks>
*)
       TSignalItem = class(TSignalConnectItem)
       strict protected
          function GetInput: TSignal;
          procedure SetInput(const Value: TSignal);
       public
          
          
          constructor Create(Collection: TCollection); override;
          
          
       published
          (*<summary>Connect a TSignal object to the Input.</summary>*)
          
          property Input: TSignal read GetInput write SetInput; 
       end;

       (*<summary>Abstract class for TSpectrum and TSignal Collections.</summary>*)
       TConnectorsCollection = class(TMtxCollection)
       strict protected
         FOnInputUpdated: TNotifyEvent;
         procedure SetOnInputUpdated(const Value: TNotifyEvent);
       strict protected
        
         FOwner: TObject;
         
         
         function GetOwner: TPersistent; override;
         
         
       public
         (*<summary>Event to be called when any component pointed to by a collection item,
            recieves a recalculation request.</summary>*)
         property OnInputUpdated: TNotifyEvent read FOnInputUpdated write SetOnInputUpdated;
       end;

       (*<summary>Allows design time and run-time connectivity of TAnalysis.</summary>
         
<remarks>Allows design time and run-time connectivity of TAnalysis and its
         descendants. This object is used to enabled the Input/Inputs property capabilities
         to signal processing components.
</remarks>
*)
       TAnalysisConnectorsCollection = class(TConnectorsCollection)
       strict protected
         function  GetItems(index: Integer): TSignalConnectItem;
         procedure SetItems(index: Integer; const Value: TSignalConnectItem);
       public
         (*<summary>Default array property allows access to list items without typecasting. </summary>*)
         property Items[index: Integer]: TSignalConnectItem read GetItems write SetItems; default; 
       end;


      (*<summary>A collection for a list of references to TSignal objects.</summary>
        
<remarks>The class is used to handle a collection of references to TSignal objects.
        This list is editable from within Delphi IDE at design-time.
</remarks>
*)
       TSignalCollection = class(TAnalysisConnectorsCollection)
       strict protected
         function  GetItems(index: Integer): TSignalItem; reintroduce;
         procedure SetItems(index: Integer; const Value: TSignalItem);
       public
         
         function Add: TCollectionItem;
         
         (*<summary>Default array property allows access to list items without typecasting.</summary>*)
         property Items[index: Integer]: TSignalItem read GetItems write SetItems; default; 
         
         constructor Create(AOwner: TComponent);
         
       end;

      (*<summary>Spectrum collection item.</summary>
        
<remarks>The class supports handling of TSpectrum as a TCollection item
         and maintains a list of references to TSpectrum objects.
         This list is editable from within Delphi IDE at design-time.
</remarks>
*)
       TSpectrumItem = class(TSignalConnectItem)
       strict protected
          function GetInput: TSpectrum;
          procedure SetInput(const Value: TSpectrum);
       public
          
          
          constructor Create(Collection: TCollection); override;
          
          
       published
          (*<summary>Connect a TSpectrum object to the Input.</summary>*)
                    
          property Input: TSpectrum read GetInput write SetInput; 
       end;

      (*<summary>A collection for a list of references to TSpectrum objects.</summary>
        
<remarks>The class is used to handle a collection of references to TSpectrum objects.
         This list is editable from within Delphi IDE in design-time.
</remarks>
*)
       TSpectrumCollection = class(TAnalysisConnectorsCollection)
       strict protected
         function  GetItems(index: Integer): TSpectrumItem; reintroduce;
         procedure SetItems(index: Integer; const Value: TSpectrumItem);
       public
         property Items[index: Integer]: TSpectrumItem read GetItems write SetItems; default;  
         (*<summary>Create a new TSpectrumItem.</summary>*)
         
         function Add: TCollectionItem;
         
         
         constructor Create(AOwner: TComponent);
         
       end;

       TPipeStateArray = array of TPipeState;

       

       (*<summary>Abstract class for TSignal and TSpectrum components.</summary>*)
       TAnalysis = class(TMtxComponent)
       strict private
          FOnAfterUpdate: TNotifyEvent;
          FOnBeforeUpdate: TNotifyEvent;
          FOnParameterUpdate: TNotifyEvent;
          FMultiChannel: boolean;
          FUsesInputs: boolean;
          FOnNotifyUpdate: TNotifyEvent;
          FContinuous: boolean;
          FOnDisplayUpdate: TNotifyEvent;
          FSuspendNotifyUpdate: boolean;
          FOnGetInput: TNotifyEvent;
          FPipeState: TPipeState;
          FItemNumber: integer;
          aStates: TPipeStateArray;
          procedure InternalPullInputs(const aStates: TPipeStateArray);
          function ResolveInputStates(const aStates: TPipeStateArray): TPipeState;
          procedure SetPipeDirty(const Value: boolean);
          procedure SetMultiChannel(const Value: boolean);
          procedure SetOnAfterUpdate(const Value: TNotifyEvent);
          procedure SetUsesInputs(const Value: boolean);
          procedure SetOnBeforeUpdate(const Value: TNotifyEvent);
          procedure SetOnParameterUpdate(const Value: TNotifyEvent);
          procedure SetOnNotifyUpdate(const Value: TNotifyEvent);
          procedure SetContinuous(const Value: boolean);
          procedure SetOnDisplayUpdate(const Value: TNotifyEvent);
          procedure SetSuspendNotifyUpdate(const Value: boolean);
          procedure SetOnGetInput(const Value: TNotifyEvent);
          procedure SetPipeState(const Value: TPipeState);
          procedure SetItemNumber(const Value: integer);

       strict protected
          FInputs: TAnalysisConnectorsCollection;
                 
          FIsDouble: boolean;
          FFloatPrecision: TMtxFloatPrecision;
          FComplex: boolean;
          FFloatPrecisionLock: boolean;
          
          (*<summary>True, if there are still buffers whose Dirty property
             is true inside the pipe.</summary>*)
          FPipeDirty: boolean;
          (*<summary>Holds the value of the Active property.</summary>*)
          FActive: boolean;
          (*<summary>Sets property Dirty of all connected components which
             have the current one for its source, to True.</summary>*)
          procedure NotifyDirty;
          (*<summary>Sets property Dirty of all connected components which
             have the current one for its source, to false.</summary>*)
          procedure NotifyResetDirty;
          (*<summary>Sets the value of the Inputs property.</summary>*)
          procedure SetInputs(const Value: TAnalysisConnectorsCollection);
          (*<summary>Sets the value of the Input property.</summary>*)
          procedure SetInput(const Value: TAnalysis); virtual;
          (*<summary>Gets the value of the Input property.</summary>*)
          function GetInput: TAnalysis; virtual;
          (*<summary>Gets the value of the Inputs property.</summary>*)
          function GetInputs: TAnalysisConnectorsCollection; virtual;
          (*<summary>Sets the value of the Active property.</summary>*)
          procedure SetActive(const Value: boolean); virtual;
          (*<summary>Defines if the inputs collection should be streamed or not.</summary>*)
          function InputsStored: boolean; virtual;
          (*<summary>Notifies connected components that this component has
             fresh data and sets all inputs Dirty properties to false.</summary>*)
          procedure InvalidateInputs;
          (*<summary>Override this method to create custom TConnectorsCollection type
             descendants, when the component is created.</summary>
             
<remarks>By default the method creates TSignalCollection.
</remarks>
*)
          function CreateInputs: TAnalysisConnectorsCollection; virtual;
       strict protected
          property PipeDirty: boolean read FPipeDirty write SetPipeDirty;
          (*<summary>Connector property to obtain a pointer to a list of TSignal components
           holding the data to be processed.</summary>
           
<remarks>The result of the processing is to be placed in Self. Publish this property in the derived classes,
           if your processing algorithm requires a multi-channel input.
</remarks>
*)
          property  Inputs: TAnalysisConnectorsCollection read GetInputs write SetInputs stored InputsStored;
          (*<summary>Set MultiChannel to True, if Inputs property will be published, and Input
             property will remain hidden.</summary>
             
<remarks>This property is to be set only in the Create
             method of the derived classes and should be set to true, only if
             Inputs property is published and Input property remains hidden.
</remarks>
*)
          property  MultiChannel: boolean read FMultiChannel write SetMultiChannel;
          (*<summary>Request recalculation of the data stored in TSignal object connected to Input/Inputs
             and place the result in Self.</summary>
             
<remarks>The Update request is not propagated. The call returns the state of the pipe.
</remarks>
*)
          function  InternalUpdate: TPipeState; virtual;
          (*<summary>The method is called by the ParamUpdate method before a call is made
             to OnParameterUpdate event.</summary>
             
<remarks>ParamUpdate method is of TNotifyEvent type and can be assigned to other event handlers to trigger recalculation
             of the data in Self.
</remarks>
*)
          procedure InternalParamUpdate(Sender: TObject); virtual;
          (*<summary>The method is called just before a call is made to InternalUpdate
             by the Update method.</summary>*)
          procedure BeforeInternalUpdate; virtual;

          procedure SetComplex(const Value: boolean); virtual;
          procedure SetFloatPrecision(const Value: TMtxFloatPrecision); virtual;
          procedure SetFloatPrecisionLock(const Value: boolean); virtual;
          procedure SetIsDouble(const Value: boolean); virtual;

       public
          procedure Reset; override;
          (*<summary>Recursively processes all connected components backward and calls Reset method.</summary>*)
          procedure PipeReset; virtual;
          procedure DetectChannelCount; virtual;
          (*<summary>If all Inputs[i] are dirty the function returns true.</summary>*)
          function InputsDirty: boolean;

          
          (*<summary>Number to label the object when it is an item in the List.</summary>
            
<remarks>Used by List components to numerate each TAnalysis in the List.
</remarks>
*)
          property ItemNumber: integer read FItemNumber write SetItemNumber;

          
          (*<summary>Returns the state of the pipe after the last update.</summary>*)
          property PipeState: TPipeState read FPipeState write SetPipeState;

          
          (*<summary>Set this property to True, to prevent calling OnNotifyUpdate from the Update Method.</summary>
             
<remarks>This can be usefull, when the screen should not be updated for every call to Update.
</remarks>
*)
          property SuspendNotifyUpdate: boolean read FSuspendNotifyUpdate write SetSuspendNotifyUpdate;

          
          (*<summary>Set it to True to indicate, that valid signals will be connected
             to the Input or Inputs properties.</summary>
             
<remarks>The property must be false, if the Input/Inputs properties are nil.
</remarks>
*)
          property  UsesInputs: boolean read FUsesInputs write SetUsesInputs;

          (*<summary>Call this method only, if SuspendNotifyUpdate is True.</summary>
             
<remarks>This routine will simply call OnNotifyEvent, if assigned.
</remarks>
*)
          procedure UpdateNotify; virtual;

          (*<summary>Notifies all connected components that this component has fresh data.</summary>*)
          procedure Invalidate;

          (*<summary>When called, the method will pass Update requests recursively to all connected objects.</summary>
             
<remarks>The InternalUpdate method will be called in order, starting from the last
             object in the chain. The call returns the state of the pipe.
</remarks>
*)
          function  InternalPull: TPipeState; virtual;
          (*<summary>Request recalculation of the data and place the result in Self.</summary>
             
<remarks>Return pipeOk, if the recalculation was successfull.
</remarks>
*)
          function  Update: TPipeState; reintroduce; virtual; 
          (*<summary>Request recalculation of the entire chain of all connected components.</summary>
             
<remarks>Returns pipeOK, if the recalculation was successfull.
             If you have a processing chain of many connected components,
             the entire chain will be recalculated by calling the Pull
             method of the last component in the chain. If the Pull returns
             pipeStreaming, call the Pull method until it returns pipeOk.
             When the Pull method returns pipeEnd, the end of the data stream
             has been reached.
</remarks>
*)
          function  Pull: TPipeState; virtual;
          (*<summary>The method can be assigned to some other component even handler, as a notification
             that a parameter has changed and that Self should consider a recalculation.</summary>
             
<remarks>The method calls the InternalParamUpdate method and triggers the OnParameterUpdate event.
</remarks>
*)
          procedure ParamUpdate(Sender: TObject);
          (*<summary>Calls Pull until pipeEnd is returned. </summary>
             
<remarks>Calls Pull in a loop until the condition returned by at least one
             item in the list is pipeEnd. The SuspendNotifyUpdate is set to
             false for the duration of this loop.
</remarks>
*)
          procedure PullUntilEnd; virtual;
          
          procedure Assign(Source: TPersistent); override;
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
          

          (*<summary>Connector property to obtain a pointer to the TSignal component holding the data to be processed.</summary>
             
<remarks>The result of the processing is to be placed in Self.Publish this property in the derived classes,
             if your processing algorithm requires only a single channel input.
</remarks>
*)

          
          property  Input: TAnalysis read GetInput write SetInput stored InputsStored; 
       published
          
          (*<summary> Set this property to false, to request computation in 32bit floating point precision, 64bit, if true. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.
                      Changing IsDouble property will also change the value of FloatPrecision property.
</remarks>
*)

          property IsDouble: boolean read FIsDouble write SetIsDouble; 

          
          (*<summary> Set this property to true, to request computation using complex numbers. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.

                      Changing Complex/IsDobule properties will also change the value of FloatPrecision property.
</remarks>
*)
          property Complex: boolean read FComplex write SetComplex  default False ;

          
          (*<summary> Set this property to request computation in 32bit/64bit precision with real/complex numbers. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.
                      Changing IsDouble/Complex properties will also change the value of FloatPrecision property.
</remarks>
*)
          property FloatPrecision: TMtxFloatPrecision read FFloatPrecision write SetFloatPrecision; 

          
          (*<summary> Set this property to true to lock changes to FloatPrecision property. </summary>
                      
<remarks>The value of this property is NOT picked up from Input or Inputs property.
</remarks>
*)
          property FloatPrecisionLock: boolean read FFloatPrecisionLock write SetFloatPrecisionLock  default False ;

          (*<summary>Set active to false to suppress the propagation of the Pull request and
             the subsequent call to the Update method.</summary>
             
<remarks>The components connected to the Input property and the component itself
             will not be updated, if the Pull method will be called and the
             Active property will be false. A direct call to the Update method will still work!
</remarks>
*)
  
          property Active: boolean read FActive write SetActive  default True ;
          (*<summary>If True, the component will pass on the udpate request to the connected component.</summary>
             
<remarks>Calling Pull will always execute the Update method.
</remarks>
*)
  
          property Continuous: boolean read FContinuous write SetContinuous  default True ;
          (*<summary>The event is triggered after the call to the Update method.</summary>*)
  
          property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write SetOnAfterUpdate;
          (*<summary>The event is triggered just before the call to the Update method.</summary>*)
  
          property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write SetOnBeforeUpdate;
          (*<summary>The event is triggered when ParamUpdate method is called. The ParamUpdate method is usually
             called by component editors when a value was changed by the user and the editor was "Live". </summary>*)
  
          property OnParameterUpdate: TNotifyEvent read FOnParameterUpdate write SetOnParameterUpdate;
          (*<summary>The event is triggered after the OnAfterUpdate event. Use this event to update any
             associated charts or graphs or result tables.</summary>
              
<remarks>The event will be triggered only, if UpdateEditors property is True.
</remarks>
*)
  
          property OnDisplayUpdate: TNotifyEvent read FOnDisplayUpdate write SetOnDisplayUpdate;
          (*<summary>If the Input property is not assigned, the component will call OnGetInput.</summary>*)
  
          property OnGetInput: TNotifyEvent read FOnGetInput write SetOnGetInput;
          (*<summary> The event is called after a call to InternalUpdate, OnAfterUpdate and
              OnDisplayUpdate from within the Update method.</summary>*)
          property  OnNotifyUpdate: TNotifyEvent read FOnNotifyUpdate write SetOnNotifyUpdate;
      end;

      (*<summary>Defines how to process pipes.</summary>
        
<remarks>Pipes connected with TAnalysisList components can be
        processed in two ways.
</remarks>
*)
      TPipeProcessing = (
      (*<summary>All pipes are called one after another on each
        processing stage, before proceeding to the next.
        This type of operation is usefull when all channels are interdependent.
        If a signal is multiplexed (for example) then the next
        block of data for any of the channels can not be fetched
        until all channels have processed their current data.</summary>*)
      ppParallel,
      (*<summary>Each pipe is first processed in full length before
        the processing of the next pipe begins. The channels
        are independent. Fetching a new block of data for
        one channel will not affect the other channels in any way.</summary>*)
      ppSerial
      );

      (*<summary>Abstract class for all components, which hold a list of TAnalysis components.</summary>
         
<remarks>The component is a generic class designed to handle lists
         of descendants from TAnalysis class. Descendants from the
         TAnalysisList class have to override the AddItem method
         and possibly declare a default array property to access
         typed list elements. Save and load the list from stream and file,
         is automatically available. You can propagate the changes made to properties
         of the first element in the list, to all other elements, by
         calling the Propagate method. Update and Pull methods
         are also centralized.
</remarks>
*)
      
      TAnalysisList = class(TMtxComponentList)
      strict private
          FSuspendNotifyUpdate: boolean;
          FOnDisplayUpdate: TNotifyEvent;
          FOnNotifyUpdate: TNotifyEvent;
          FOnBeforeUpdate: TNotifyEvent;
          FOnAfterUpdate: TNotifyEvent;
          FOnParameterUpdate: TNotifyEvent;
          FEditIndex: integer;

          procedure SetSuspendNotifyUpdate(const Value: boolean);
          function GetItemsp(index: integer): TAnalysis;
          procedure SetItemsp(index: integer; const Value: TAnalysis);
          procedure SetOnAfterUpdate(const Value: TNotifyEvent);
          procedure SetOnBeforeUpdate(const Value: TNotifyEvent);
          procedure SetOnDisplayUpdate(const Value: TNotifyEvent);
          procedure SetOnNotifyUpdate(const Value: TNotifyEvent);
          procedure SetOnParameterUpdate(const Value: TNotifyEvent);
          procedure DoAfterUpdate(Sender: TObject);
          procedure DoBeforeUpdate(Sender: TObject);
          procedure DoDisplayUpdate(Sender: TObject);
          procedure DoNotifyUpdate(Sender: TObject);
          procedure DoParameterUpdate(Sender: TObject);
          procedure SetEditIndex(const Value: integer);
       strict protected
          FInput: TAnalysis;
          FInputs: TAnalysisList;

          FActive: boolean;
          FContinuous: boolean;
          FIsDouble: boolean;
          FFloatPrecision: TMtxFloatPrecision;
          FComplex: boolean;
          FFloatPrecisionLock: boolean;

          procedure SetInput(const Value: TAnalysis);
          procedure SetInputs(const Value: TAnalysisList);
          procedure SetActive(const Value: boolean); virtual;
          procedure SetContinuous(const Value: boolean); virtual;
          procedure SetFloatPrecision(const Value: TMtxFloatPrecision); virtual;
          procedure SetFloatPrecisionLock(const Value: boolean); virtual;
          procedure SetComplex(const Value: boolean); virtual;
          procedure SetIsDouble(const Value: boolean); virtual;

          (*<summary>Defines if the inputs collection should be streamed or not.</summary>*)
          function InputsStored: boolean; virtual;

          
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
          (*<summary>Called automatically by Pull and PullUntilEnd.</summary>
             
<remarks>Ensures the Count property will
             be set to the same number of channels as it's
             connected component list. The state of the
             first component in the list will be used as
             a template for all new components added to the list.
</remarks>
*)
          procedure InternalMatchInputs; virtual;
          procedure Add(MtxComponent: TMtxComponent); override;
      public
          (*<summary>Assign consecutive numbers to individual items.</summary>
            
<remarks>Assign consecutive numbers starting with 0 to every items ItemNumber
            property. This numbers are assigned when the Items are added automatically.
            However, if the items in the list are moved or deleted, renumbering is
            neccessary, if there are parts of code that depend upon ItemNumber property
            and ItemNumber must coincide with the Index in the list at which this
            component is located.
</remarks>
*)
          procedure Renumerate; virtual;
          
          procedure ReferenceRemoved(Sender: TObject); override;
          
          (*<summary>Call this method only, if SuspendNotifyUpdate is True.</summary>
             
<remarks>This routine will simply call OnNotifyEvent, if assigned.
             OnNotifyEvent events of the items are called by the Update
             methods, if SuspendNotifyUpdate is false.
</remarks>
*)
          procedure UpdateNotify;
          (*<summary> Match the connections of Inputs with the component. </summary>
                    
<remarks>Recursive function will be passed to the chain of all connected components.
                    Its purpose is best illustrated with an example. If we have a chain
                    of TAnalysisList components and the first item in the chain reads a file,
                    that file may have any ChannelCount. All connected components must therefore
                    adjust their Count property and connect the Input property for
                    individual Items in their lists with corresponding Items in the Inputs
                    property list.

                    This is especially usefull when there is a need to handle signal multiplexing
                    and demultiplexing.
</remarks>
*)
          procedure MatchInputs; virtual;
          (*<summary>Default array property allows access to individual list items </summary>*)
          property Items[index: integer]: TAnalysis read GetItemsp write SetItemsp; default; 
          (*<summary>Distribute Update request to all objects in the list.</summary>
             
<remarks>Return value of the PipeState property for the first item.
             Inputs or Input must have valid data. The value of their
             PipeState property will not be checked.
</remarks>
*)
          function  Update: TPipeState; reintroduce; virtual;
          (*<summary>Distribute Pull request to all objects in the list.</summary>
             
<remarks>Return true if the recalculation was successfull for all of them.
</remarks>
*)
          function  Pull: TPipeState; virtual;
          (*<summary>Calls Pull until pipeEnd is returned. </summary>
             
<remarks>Calls Pull in a loop until the condition returned by at least one
             item in the list is pipeEnd.
</remarks>
*)
          procedure PullUntilEnd; virtual;
          (*<summary>Propagates the settings of the component at SrcIndex to
                     all other components.</summary>
                     
<remarks>The settings are assigned using the SaveTemplateToStream and
                     LoadTemplateFromStream methods.
</remarks>
*)

          procedure Propagate(SrcIndex: integer); overload;
          
          constructor Create(AOwner: TComponent); override;
          

          (*<summary> Set this property to false, to request computation in 32bit floating point precision, 64bit, if true. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.
                      Changing IsDouble property will also change vaue of FloatPrecision property.
</remarks>
*)

          property IsDouble: boolean read FIsDouble write SetIsDouble;

          (*<summary> Set this property to true, to request computation using complex numbers. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.

                      Changing Complex/IsDobule properties will also change value of FloatPrecision property.
</remarks>
*)
          property Complex: boolean read FComplex write SetComplex;
      published
          
          (*<summary> Set this property to request computation in 32bit/64bit precision with real/complex numbers. </summary>
                      
<remarks>The value of this property is typically picked up from Input or Inputs property. Setting this value is only
                      meaningfull, if this component is the first in the computational pipe. Aditionally, by setting FloatPrecisionLock to True,
                      will prevent both IsDouble and Complex properties of all items to have their corresponding properties changed.
                      Changing IsDouble/Complex properties will also change value of FloatPrecision property.
</remarks>
*)
          property FloatPrecision: TMtxFloatPrecision read FFloatPrecision write SetFloatPrecision;

          
          (*<summary> Set this property to true to lock changes to FloatPrecision property. </summary>
                      
<remarks>The value of this property is NOT picked up from Input or Inputs property.
</remarks>
*)
          property FloatPrecisionLock: boolean read FFloatPrecisionLock write SetFloatPrecisionLock;

          
          (*<summary>Specifies index of the item in the list for which to display the design-time editor.</summary>
                     
<remarks>Default value is -1, which means that all items in the list will get to have the same settings specified by editor once.
                     Used only by the design-time editor within the IDE.
</remarks>
*)
          property EditIndex: integer read FEditIndex write SetEditIndex  default -1 ;

          
          (*<summary>Set active to false to suppress the propagation of the Pull request and
             the subsequent call to the Update method.</summary>
             
<remarks>The components connected to the Input property and the component itself
             will not be updated, if the Pull method will be called and the
             Active property will be false. A direct call to the Update method will still work!
</remarks>
*)
          property Active: boolean read FActive write SetActive  default True ;

          
          (*<summary>If True, the component will pass on the udpate request to the connected component.</summary>
             
<remarks>Calling Pull will always execute the Update method.
</remarks>
*)
          property Continuous: boolean read FContinuous write SetContinuous  default True ;

          
          (*<summary>Set this property to True, to prevent calling OnNotifyUpdate from the Update Method.</summary>
             
<remarks>This can be usefull, when the screen should not be updated for every call to Update.
</remarks>
*)
          property SuspendNotifyUpdate: boolean read FSuspendNotifyUpdate write SetSuspendNotifyUpdate  default false ;

          
          (*<summary>The event is triggered after the call to the Update method.</summary>*)
          property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write SetOnAfterUpdate;

          
          (*<summary>The event is triggered just before the call to the Update method.</summary>*)
          property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write SetOnBeforeUpdate;

          
          (*<summary>The event is triggered when ParamUpdate method is called.</summary>
           
<remarks>The ParamUpdate method is usually called by component editors when a value was changed by the user and the editor was "Live".
</remarks>
*)
          property OnParameterUpdate: TNotifyEvent read FOnParameterUpdate write SetOnParameterUpdate;

          
          (*<summary>The event is triggered after the OnAfterUpdate event.</summary>
           
<remarks>Use this event to update any associated charts or graphs or result tables when the associated dialog is to be updated.
             The event will be triggered only, if TMtxComponent.EditorActive property is True.
</remarks>
*)
          property OnDisplayUpdate: TNotifyEvent read FOnDisplayUpdate write SetOnDisplayUpdate;

          
          (*<summary> The event is called after a call to InternalUpdate, OnAfterUpdate and
              OnDisplayUpdate from within the Update method.</summary>*)
          property  OnNotifyUpdate: TNotifyEvent read FOnNotifyUpdate write SetOnNotifyUpdate;
      end;


      

      

       (*<summary>Class defined to store uniformly sampled data.</summary>
        
<remarks>Use TSignal component to store uniformly sampled data, with given sampling frequency.
</remarks>
*)
       TSignal = class(TAnalysis)
       strict private
          fBandwidth: Double;
          fBandwidthH: Double;
          fDt: Double;
          FMarks: TMarkList;
          FMinX: Double;
          fMaxX: Double;
          FBandWidthL: Double;
          fHzRes: Double;
          FSamplingFrequency: Double;
          FSamplingTime: Double;
          FData: TVec;
          FLength: integer;
          FMinAmplt: Double;
          FMaxAmplt: Double;
          FChannelCount: integer;
          procedure setLength(const Value: integer);
          procedure SetBandWidthL(const Value: Double);
          procedure SetSamplingTime(const Value: Double);
          procedure SetHzRes(const Value: Double);
          procedure SetDt(const Value: Double);
          procedure SetData(const Value: TVec);
       strict protected
          function GetInput1: TSignal;
          procedure SetInput1(const Value: TSignal);
          function GetInputs1: TSignalCollection;
          procedure SetInputs1(const Value: TSignalCollection);
       
       protected
          procedure DefineProperties(Filer: TFiler); override;
       
       
       
       strict protected
       
          
          procedure set_Values(const i: integer; const value: Double); 
          function  get_CValues(const i: integer): TCplx; 
          procedure set_CValues(const i: integer; const Value: TCplx); 
          function  get_Values(const i: integer): Double; 
          
        strict protected
          property  Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored; 
          function  GetChannelCount: integer; virtual;
          procedure SetChannelCount(const Value: integer); virtual;
          procedure SetSamplingFrequency(const Value: Double); virtual;
          procedure BeforeInternalUpdate; override;
          
          procedure SaveBinaryStream(Stream: TStream); overload; virtual;
          procedure LoadBinaryStream(Stream: TStream); overload; virtual;
          
          
          procedure UpdateSetLength(Sender: TObject); virtual;

          procedure SetIsDouble(const Value: boolean); override;
          procedure SetFloatPrecision(const Value: TMtxFloatPrecision); override;
          procedure SetComplex(const Value: boolean); override;
       public

          

          property Input: TSignal read GetInput1 write SetInput1 stored InputsStored; 

          
          (*<summary>Stores the minimum amplitude of the signal found by the MinMax method.</summary>*)
          property MinAmplt: Double read FMinAmplt;

          
          (*<summary>Stores the maximum amplitude of the signal found by the MinMax method.</summary>*)
          property MaxAmplt: Double read FMaxAmplt;

          (*<summary>Returns the upper bandwidth edge of the signal.</summary>
           
<remarks>It is calculated as BandwdithL+ Bandwidth.
</remarks>
*)
          property BandwidthH: Double read fBandWidtH;
          (*<summary>Returns the lower bandwidth edge of the signal.</summary>
           
<remarks>Usually this property is zero.
</remarks>
*)
          property BandwidthL: Double read FBandWidthL write SetBandWidthL;
          (*<summary>Stores the time stamps and amplitude of selected samples.</summary>*)
          property Marks: TMarkList read FMarks;

          
          (*<summary>Use the MaxX property to store the time stamp (in seconds) of the right most sample.</summary>
             
<remarks>Each time the SamplingTime property changes (the length of the signal in seconds),
             MaxX is recalculated as: MaxX := MinX + SamplingTime.
</remarks>
*)
          property MaxX: Double read FMaxX write fMaxX;

          
          (*<summary>Use the MinX property to store the time stamp of the left-most sample.</summary>
             
<remarks>MaxX value depends on the value of MinX.
</remarks>
*)
          property MinX: Double read FMinX write fMinX;

          
          (*<summary>Frequency resolution obtained, if an FFT algorithm would be run on the data.</summary>*)
          property HzRes: Double read FHzRes write SetHzRes;
          
          (*<summary>Default property to access values of a complex signal.</summary>
           
<remarks>Use the Values property to work with a real signal. You can determine, if the signal is real or complex,
             by reading the Complex property.
</remarks>
*)
          property CValues[const i: integer]: TCplx read get_CValues write set_CValues;
          
          (*<summary>Default property to access values of a real signal.</summary>
           
<remarks>Use the CValues property to work with a complex signal. You can determine, if the signal is real or complex,
             by reading the Complex property.
</remarks>
*)
          property Values[const i: integer]: Double read get_Values write set_Values; default;

          
          (*<summary>The length of the signal in seconds.</summary>
           
<remarks>SamplingTime will change, if the Length property changes.
             The length of the signal in seconds is kept proportional to the length of the signal in samples.
             The Length property will not change, if you change the SamplingTime.
</remarks>
*)
          property SamplingTime: Double read FSamplingTime write SetSamplingTime stored AllowStreaming;

                    
          (*<summary>Time between two consecutive samples.</summary>*)
          property Dt: Double read FDt write SetDt;
          (*<summary>Find minimum and maximum amplitude and store the result in the MinAmplt and MaxAmplt properties.</summary>*)
          procedure MinMax;
          (*<summary>Compute the CREST parameter.</summary>*)
          function Crest: Double;
          (*<summary>Find the maximum deviation of the signal from its mean value.</summary>*)
          function Peak: Double;
          (*<summary>Assign the size of the signal from Src.</summary>
           
<remarks>The method will assign: Complex, Length and SamplingFrequency properties in that order.
</remarks>
*)
          procedure Size(Src: TSignal);
          (*<summary>Convert marks to strings.</summary>
             
<remarks>The strings will be stored in TStrings object with two columns,
             aligned for display with a fixed charachter width Font. The Header of each column
             is defined with the XTitle and YTitle properties. The header line
             will not be displayed, if both XTitle and YTitle are set to an empty string.
             The width of the columns is defined with global variable: SignalTextColumnWidth.
             TimeFormat and AmplitudeFormat define the number formating (for example '0.00##')
             If TimeFormat or AmplitudeFromat is an empty string, full numeric precision will be used.
</remarks>
*)
          procedure MarksToStrings(Dst: TStrings; const TimeFormat: string = '';
                                                  const AmplitudeFormat: string = '';
                                                  const XTitle: string = 'Time [s]';
                                                  const YTitle: string = 'Amplitude';
                                                  const Delimiter: string =''); virtual;

          (*<summary>Convert values to strings.</summary>
           
<remarks>The Strings will be stored in TStrings object with two columns
            aligned for display with a fixec charachter width Font. The head of each column
            is defined the XTitle and YTitle properties. both XTitle and YTitle are set to an empty string.
             The width of the columns is defined with global variable: SignalTextColumnWidth.
             TimeFormat and AmplitudeFormat define the number formating (for example '0.00##')
             If TimeFormat or AmplitudeFromat is an empty string, full numeric precision will be used.
</remarks>
*)
          procedure ValuesToStrings(Dst: TStrings; const TimeFormat: string = '';
                                                   const AmplitudeFormat: string = '';
                                                   const XTitle: string = 'Time [s]';
                                                   const YTitle: string = 'Amplitude';
                                                   const Delimiter: string = ''); virtual;
          (*<summary>Copies signal Data and sampling frequency information.</summary>*)
          procedure Copy(Src: TSignal);

          (*<summary>Appends Sample to the end of the signal.</summary>
             
<remarks>This procedure should be avoided when large sets of data need to be appended.
             For each new value appended, the entire signal is copied twice.
             Use Data.Copy in that case (TVec method).
</remarks>
*)
          procedure Append(Sample: Double); virtual;
          
          destructor Destroy; override;
          
          constructor Create(AOwner: TComponent); override;
          function AllowStreaming: Boolean; override;
          
       published
          
          (*<summary>Get or set the number of channels multiplexed in the data property.</summary>*)
          property ChannelCount: integer read GetChannelCount write SetChannelCount default 1;

          
          (*<summary>Return the bandwidth of the signal.</summary>*)
          property BandWidth: Double read fBandwidth;

          
          (*<summary>Stores the  result of the processing.</summary>*)
          property Data: TVec read FData write SetData stored AllowStreaming;

          
          (*<summary>Get or set the length of the signal in samples.</summary>*)
          property Length: integer read FLength write SetLength default 128;

          
          (*<summary>Defines the sampling frequency of the signal.</summary>
            
<remarks>Sampling frequency will remain fixed, when you change the Length property.
</remarks>
*)
          property SamplingFrequency: Double read FSamplingFrequency write SetSamplingFrequency;











        end;

      

      

     (*<summary>Manages a list of TSignal objects.</summary>
       
<remarks>The component overrides the AddItem
       method and declares a new default array property returning TSignal type.
</remarks>
*)
      TSignalList = class(TAnalysisList)
      strict private
          function GetItems(index: integer): TSignal; reintroduce;
          procedure SetItems(index: integer; const Value: TSignal);
      strict protected
          
          function AddItem: TMtxComponent; override;
          
      public
         
          
          constructor Create(AOwner: TComponent); override;
          
          (*<summary>Access TSignal objects by Index.</summary>*)
          property Items[index: integer]: TSignal read GetItems write SetItems; default; 
      end;

      

      

      (*<summary>Class defined to store and manage a frequency spectrum.</summary>
        
<remarks>Use TSpectrum to compute and store frequency spectrum and work with amplitude
        and phase.
</remarks>
*)
      TSpectrum = class(TAnalysis)
      strict private
          FMainlobeWidth: integer;
          fAverages: integer;
          FPhaseMode: TPhaseMode;
          FPhaseUnits: TPhaseUnits;
          FLength: integer;
          FSamplingFrequency: Double;
          FSamplingTime: Double;
          FMaxAmplt: Double;
          FMinPhase: Double;
          FMinAmplt: Double;
          FMaxPhase: Double;
          FAmplt, FPhase: TVec;
          FMarks: TMarkList;
          FHzRes: Double;
          fBandwidth: Double;
          fBandwidthH: Double;
          fBandwidthL: Double;
          FHarmonics: integer;
          FDt,ComplexFix: Double;
          FZeroPadding: integer;
          FMaxFreqIndex: integer;
          FPhaseRange: TPhaseRange;


          function SumOfAmpltSquares(Index, Len: integer): Double;
          procedure SetMainlobeWidth(const Value: integer);
          procedure SetPhaseMode(const Value: TPhaseMode);
          procedure SetPhaseUnits(const Value: TPhaseUnits);
          procedure SetLength(const Value: integer);
          procedure SetSamplingTime(const Value: Double);
          procedure SetMaxAmplt(const Value: Double);
          procedure SetMaxPhase(const Value: Double);
          procedure SetMinAmplt(const Value: Double);
          procedure SetMinPhase(const Value: Double);
          procedure SetHzRes(const Value: Double);
          procedure SetBandWidthL(const Value: Double);
          procedure SetHarmonics(const Value: integer);
          procedure SetDt(const Value: Double);
          procedure SetMaxFreqIndex(const Value: integer);
          procedure SetSamplingFrequency(const Value: Double);
          procedure SetAmplt(const Value: TVec);
          procedure SetPhase(const Value: TVec);
          procedure SetPhaseRange(const Value: TPhaseRange);
    strict protected
          fIsSpectrumAnalyzer: boolean;
          FConjExtend: boolean;
          FConjFlip: boolean;
          FIntegrateWork: TVec;
          function GetInput1: TSignal;
          function GetInputs1: TSignalCollection;
          procedure SetInput1(const Value: TSignal);
          procedure SetInputs1(const Value: TSignalCollection);
          procedure SetConjExtend(const Value: boolean);
          procedure SetConjFlip(const Value: boolean);
   
    protected
          procedure DefineProperties(Filer: TFiler); override;
    
    strict protected
          property  Inputs: TSignalCollection read GetInputs1 write SetInputs1 stored InputsStored; 
          procedure ComputeGroupDelay; virtual;
          procedure ComputePhaseDelay; virtual;
          procedure UpdateSetLength(Sender: TObject); virtual;
          procedure SetZeroPadding(const Value: integer); virtual;
          procedure BeforeInternalUpdate; override;
          
          procedure SaveBinaryStream(Stream: TStream); overload; virtual;
          procedure LoadBinaryStream(Stream: TStream); overload; virtual;
          
          
          function AllowDStreaming: boolean; virtual;
          procedure SetComplex(const Value: boolean); override;
          procedure SetIsDouble(const Value: boolean); override;
          procedure SetFloatPrecision(const Value: TMtxFloatPrecision); override;
          procedure PhaseCorrection(const SrcDst: TVec); virtual;
    public
         (*<summary>Returns true, if the descendant class is derived from TSpectrumAnalyzer. </summary>*)
          property IsSpectrumAnalyzer: boolean read fIsSpectrumAnalyzer;
          
          property  Input: TSignal read GetInput1 write SetInput1  stored InputsStored ; 

         (*<summary>Returns the upper bandwidth edge of the signal. </summary>
              
<remarks>It is calculated  as BandwdithL+ Bandwidth.
</remarks>
*)
          property BandwidthH: Double read fBandWidthH;

          
          (*<summary>Returns the lower bandwidth edge of the signal.</summary>
           
<remarks>Usually this property is zero.
</remarks>
*)
          property BandwidthL: Double read FBandWidthL write SetBandWidthL;

          (*<summary>Stores the frequency, amplitude and phase of selected frequency bins.</summary>*)
          property Marks: TMarkList read FMarks;

          
          (*<summary>Holds the index at which the maximum amplitude was found by the MinMaxAmplt method.</summary>*)
          property MaxFreqIndex: integer read FMaxFreqIndex write SetMaxFreqIndex;

          
          (*<summary>Stores the minimum value of the Amplt spectrum found by the MinMaxAmplt method.</summary>*)
          property MinAmplt: Double read FMinAmplt write SetMinAmplt;

          
          (*<summary>Stores the maximum value of the Amplt spectrum found by the MinMaxAmplt method.</summary>*)
          property MaxAmplt: Double read FMaxAmplt write SetMaxAmplt;

          
          (*<summary>Stores the minimum value of the Phase spectrum found by the MinMaxPhase method.</summary>*)
          property MinPhase: Double read FMinPhase write SetMinPhase;

          
          (*<summary>Stores the maximum value of the Phase spectrum found by the MinMaxPhase method.</summary>*)
          property MaxPhase: Double read FMaxPhase write SetMaxPhase;

          
          (*<summary>Used to store the number of averages made.</summary>*)
          property Averages: integer read fAverages write fAverages;

          (*<summary>If True, the conjugate symmetric part of the frequency spectrum
             will be placed in 0.5..1.0 Fs (as returned by the FFT),
             and not between -0.5..0.0 Fs.</summary>
             
<remarks>The conjugate symmetic part of the frequency spectrum from the real time
             series can be placed before or after its original.)
             The property is used in derived classes.
</remarks>
*)
          property ConjFlip: boolean read FConjFlip write SetConjFlip;
          (*<summary>If True, the conjugate symmetric part of the frequency spectrum
             is to be computed.</summary>
             
<remarks>The property is used in derived classes.
             The conjugate symmetic part of teh frequency spectrum
             can be used to detect aliasing, if you search for harmonics.
</remarks>
*)
          property ConjExtend: boolean read FConjExtend write SetConjExtend;
          (*<summary>Compute amplitude spectrum from X.</summary>*)
          procedure AmpltSpectrum(X: TVec); virtual;
          (*<summary>Return amplitude at frequency Freq.</summary>
             
<remarks>Frequency which is not accurately alligned with the frequency bin
             of the spectrum is rounded to the closest frequency bin.
</remarks>
*)
          function  AmpltEst(Freq: Double): Double; overload; virtual;
          (*<summary>Return phase at frequency Freq.</summary>
             
<remarks>Frequency which is not accurately alligned with the frequency bin
             of the spectrum is rounded to the closest frequency bin.
</remarks>
*)
          function  PhaseEst(Freq: Double): Double; overload; virtual;
          (*<summary>Returns the amplitude of the frequency Freq computed from SpcFreqBin.</summary>
             
<remarks>SpcFreqBin must contain complex spectrum bin estimated at Freq.
             The Freq parameter is needed only to apply differentiation/integration in derived classes.
</remarks>
*)
          function AmpltEst(Freq: Double; const SpcFreqBin: TCplx): Double; overload; virtual;
          (*<summary>Returns the amplitudes of the frequency spectrum at Freq frequencies.</summary>
             
<remarks>Frequencies which are not accurately alligned with frequency bins
             of the spectrum are rounded to the closest frequency bin. The
             value of the Freq vector is preserved
</remarks>
*)
          procedure AmpltEst(Freq, DstAmplt: TVec); overload; virtual;
          (*<summary>Returns the amplitudes of the amplitudes at Freq frequencies.</summary>
             
<remarks>SrcSpectrum must contain complex spectrum estimated at frequencies defined in Freq.
</remarks>
*)
          procedure AmpltEst(Freq, SrcSpectrum, DstAmplt: TVec); overload; virtual;
          (*<summary>Returns the phases of the frequency Freq
             according to the value of the Peaks property.</summary>
             
<remarks>SpcFreqBin must contain complex spectrum bin estimated at Freq.
</remarks>
*)
          function PhaseEst(Freq: Double; const SpcFreqBin: TCplx): Double; overload; virtual;
          (*<summary>Returns the phases of the frequency spectrum at Freq frequencies.</summary>
             
<remarks>Frequencies which are not accurately alligned with frequency bins
             of the spectrum are rounded to the closest frequency bin. The value of the
             Freq vector is preserved.
</remarks>
*)
          procedure PhaseEst(Freq, DstPhase: TVec); overload; virtual;
          (*<summary>Returns the phases of the frequency spectrum at Freq frequencies.</summary>
             
<remarks>SrcSpectrum must contain complex spectrum estimated at frequencies defined in Freq.
             DstPhase will hold the result. The phase is corrected for peak processing.
</remarks>
*)
          procedure PhaseEst(Freq, SrcSpectrum, DstPhase: TVec); overload; virtual;
          (*<summary>Find minimum and maximum value of the Amplt spectrum.</summary>
            
<remarks>The result is stored in the MinAmplt and MaxAmplt properties.
</remarks>
*)
          procedure MinMaxAmplt;
          (*<summary>Find minimum and maximum value of the Phase spectrum.</summary>
             
<remarks>The result is stored in the MinPhase and MaxPhase properties.
</remarks>
*)
          procedure MinMaxPhase;
          (*<summary>Return the peak closest to the Freq frequency.</summary>*)
          function  PeakApproximate(Freq: Double): Double; overload; virtual;
          (*<summary>Filter the peak at Marks position Index from the signal and recalculate the spectrum.</summary>*)
          procedure FilterPeak(Index: integer); virtual;
          (*<summary>Add amplitude and phase of the ASpectrum to the current spectrum.</summary>*)
          procedure Add(ASpectrum: TSpectrum);
          (*<summary>Flip the frequency spectrum on the frequency axis.</summary>*)
          procedure BandFlip;
          (*<summary>Copy ASpectrum to the calling spectrum.</summary>*)
          procedure Copy(ASpectrum: TSpectrum); overload; virtual;
          (*<summary>Differentiate the signal in the frequency domain.</summary>
                    
<remarks>The diffCount parameter defines the differentiation count. Valid values for diffCount are 0 to 8.
</remarks>
*)
          procedure Differentiate(const diffCount: integer);
          (*<summary>Divide the calling the spectrum with ASpectrum.</summary>
             
<remarks>Amplitude part is divided and phase part is subtracted.
</remarks>
*)
          procedure Divide(ASpectrum: TSpectrum); overload; virtual;
          (*<summary>Subtract ASpectrum from the calling spectrum.</summary>*)
          procedure Subtract(ASpectrum: TSpectrum);
          (*<summary>Scale the calling spectrum with Factor.</summary>*)
          procedure Scale(Factor: Double);
          (*<summary>Square the Amplt and Phase of the ASpectrum and copy it to self.</summary>*)
          procedure Sqr(ASpectrum: TSpectrum); virtual;
          (*<summary>Computes range checked array indexes in the spectrum from frequency values.</summary>
                     
<remarks>The routine considers the value of the BandwidthL property, the HzRes and Bandwidth property.
                     Any values outside of the valid range will be clipped to the edge of the valid range.
</remarks>
*)
          procedure FreqToFreqIndex(const Freq: TVec; const DstFreqIndex: TVecInt); overload;
          (*<summary>Integrate the signal in the frequency domain.</summary>
                    
<remarks>The intCount parameter defines the differentiation count. Valid values for intCount are 0 to 8.
</remarks>
*)
          procedure Integrate(const intCount: integer);
          (*<summary>Compute the logarithm of the amplitude spectrum and convert to dB.</summary>
             
<remarks>Base defines the base of the logarithm (usually 10). Span defines the span in dB of the result,
             and AMax is the maximum amplitude found in the spectrum. If Normalized is
             True, the highest value of the spectrum will positioned at 0.
</remarks>
*)
          function LogAmplt(Base, Span, aMax, aScale: Double; Normalized: boolean): Double; virtual;
          (*<summary>Calculates the average of the noise.</summary>
            
<remarks>The signal is considered to be marked and is ignored.
             The width of the peaks is determined with the MainlobeWidth property.
             Marks are stored in Marks property.
</remarks>
*)
          function  NF(ActualZeroPadding: Double = 1): Double;
          (*<summary>Multiply the calling the spectrum with ASpectrum.</summary>
             
<remarks>Amplitude part is mulitplied and phase part is added.
</remarks>
*)
          procedure Mul(ASpectrum: TSpectrum);
          (*<summary>Calls MinMaxAmplt and MinMaxPhase.</summary>*)
          procedure MinMax;
          (*<summary>Compute power spectrum from X.</summary>*)
          procedure PowerSpectrum(X: TVec); virtual;
          (*<summary>Compute phase spectrum from X.</summary>*)
          procedure PhaseSpectrum(X: TVec); virtual;
          (*<summary>Compute RMS spectrum from X.</summary>*)
          procedure RMSSpectrum(X: TVec); virtual;
          (*<summary>Initialize spectrum to zero.</summary>*)
          procedure SetZero;  overload; virtual;
          (*<summary>Assign Length, Complex and SamplingFrequency properties.</summary>*)
          procedure Size(Src: TSpectrum);
          (*<summary>Convert values to strings.</summary>
            
<remarks>The strings will be stored in TStrings object with three columns,
             alligned for display with a fixed charachter width Font. The Header of each column
             is defined with the XTitle and ATitle and PTitle properties. The header line
             will not be displayed, if you set XTitle, ATitle and PTitle to empty strings.
             The width of the columns is defined with global variable: SignalTextColumnWidth.
             If FrequencyFormat, AmplitudeFormat or PhaseFormat is an empty string,
             full numeric precision will be used for the coresponding column.
</remarks>
*)
          procedure ValuesToStrings(Dst: TStrings; const FrequencyFormat: string = '';
                                                   const AmplitudeFormat: string = '';
                                                   const PhaseFormat: string = '';
                                                   const XTitle: string = 'Frequency [Hz]';
                                                   const ATitle: string = 'Amplitude';
                                                   const PTitle: string = 'Phase';
                                                   IncludePhase: boolean = True;
                                                   const Delimiter: string = ''); virtual;

          (*<summary>Convert marks to strings.</summary>
            
<remarks>The strings will be stored in TStrings object with two columns,
             alligned for display with a fixed charachter width Font. The Header of each column
             is defined with the XTitle and ATitle and PTitle properties. The header line
             will not be displayed, if XTitle, ATitle and PTitle are set to empty strings.
             The width of the columns is defined with the global variable: SignalTextColumnWidth.
             If FrequencyFormat, AmplitudeFormat or PhaseFormat is an empty string,
             full numeric precision will be used for the coresponding column.
</remarks>
*)
          procedure MarksToStrings(Dst: TStrings; const FrequencyFormat: string = '';
                                                  const AmplitudeFormat: string = '';
                                                  const PhaseFormat: string = '';
                                                  const XTitle: string = 'Frequency [Hz]';
                                                  const ATitle: string = 'Amplitude';
                                                  const PTitle: string = 'Phase';
                                                  IncludePhase: boolean = True;
                                                  const Delimiter: string = '');
          (*<summary>Calculates the signal-to-noise ratio.</summary>
            
<remarks>The signal is considered to be marked. The method
             sums the marks and calculates the ratio towards the rest
             and returns the result in dB.

             The width of the peaks is determined with the
             MainlobeWidth property. Marks are stored in Marks property.
</remarks>
*)
          function  SNR(ActualZeroPadding: Double = 1): Double;
          (*<summary>Signal-to-noise-and-distortion calculates the the ratio of the
             maximum mark towards the rest (noise).</summary>
             
<remarks>Marked peaks (except for the maximum) are
             considered to be the noise and are summed
             separately to compensate for FFT leakage error and
             avoid using Parsevals theorem.
</remarks>
*)
          function  SINAD(ActualZeroPadding: Double = 1): Double;
          (*<summary>Spurious-free-dynamic-range computes the ratio of the largest
             marked peak towards the second largest.</summary>
             
<remarks>Marked peaks are stored in the Marks property.
</remarks>
*)
          function  SFDR: Double;
         (*<summary>Returns the ratio of RMS of the marked peaks, excluding the first, towards the first
            marked peak and returns the result in [%].</summary>
            
<remarks>Marked peaks are stored in the Marks property
</remarks>
*)
          function  THD: Double; virtual;
          (*<summary>Searches for the closest peaks of the amplitude spectrum. </summary>
                     
<remarks>Returns the frequency as the result.
</remarks>
*)
          function ConvergeToMaximum(Freq: Double): Double; overload;
          (*<summary>Searches for the closest peaks of the amplitude spectrum. </summary>
                     
<remarks>Stores the new peak frequency back in to the Freqs parameter.
                     The center holds the central spectral line and is guaranteed not to be on the edge of the array.
                     Even if the maximum value is in fact on the edge of the array both Freqs and Center will be moved one line inside
                     to starting index 1 and final index Length-2.
</remarks>
*)
          procedure ConvergeToMaximum(const Freqs: TVec; const Center: TVecInt); overload;
          (*<summary>Returns the RMS of the spectrum. </summary>*)
          function  RMS(ActualZeroPadding: Double = 1): Double;
          (*<summary>The total-harmonic-distortion-and-noise computes
            the sum of sqares of the rest towards the maximum marked peak.</summary>
            
<remarks>All other marked peaks are considered to be noise.
            Marked peaks are stored in the Marks property.
            The width of the peaks is determined with the MainlobeWidth property.
</remarks>
*)
          function  THDN(ActualZeroPadding: Double = 1): Double; virtual;
          function IsNumericalInterpolationSupported: boolean; virtual;
          
          destructor Destroy; override;
          
          constructor Create(AOwner: TComponent); override;
          
    published
          (*<summary>Stores the amplitude of the spectrum.</summary>*)
          
          property Amplt: TVec read FAmplt write SetAmplt stored AllowDStreaming;

          (*<summary>Stores the phase of the spectrum. </summary>*)
           
          property Phase: TVec read FPhase write SetPhase stored AllowDStreaming;

          (*<summary>Returns the bandwidth of the spectrum in [Hz].</summary>*)
          
          property BandWidth: Double read fBandwidth;

          (*<summary>Defines the length of the spectrum. </summary>*)
          
          property Length: integer read FLength write SetLength default 64;

          (*<summary>Defines the width of the main lobe.
            Each frequency component in the frequency spectrum should theoretically
             be infinitely thin.</summary>
             
<remarks>Because of spectral leakege (Gibbs phenomenon),
             each spectral component has certain width (in [Hz]).
             This width depends on the spectral window used. (Hanning, Hamming etc..)
             The derived classes can set this property to match up to the spectral window
             used when computing the frequency spectrum. This parameter is used
             by different statistical methods to improve the accuracy
             of the estimate. (THDN, SINAD, RMS...).
</remarks>
*)
           
          property MainlobeWidth: integer read FMainlobeWidth write SetMainlobeWidth default 8;

          (*<summary>This property determines how to handle the phase of the frequency spectrum.</summary>
             
<remarks>If you are not interested in phase set this property to pmIgnore. This can save considerable
             computation time.
</remarks>
*)
          
          property PhaseMode: TPhaseMode read FPhaseMode write SetPhaseMode  default pm360 ;

          (*<summary>Defines the units of phase. (radian, degrees).</summary>*)
          
          property PhaseUnits: TPhaseUnits read FPhaseUnits write SetPhaseUnits  default puDeg ;

          
          (*<summary>Specify the range of the phase spectrum.</summary>*)
          property PhaseRange: TPhaseRange read FPhaseRange write SetPhaseRange;

          (*<summary>Defines the sampling frequency of the signal on which the frequency spectrum is based.</summary>*)
          
          property SamplingFrequency: Double read FSamplingFrequency write SetSamplingFrequency;

          (*<summary>Defines the length of the signal on which the frequency spectrum is based in seconds.</summary>*)
          
          property SamplingTime: Double read FSamplingTime write SetSamplingTime;
          (*<summary>Returns the frequency resolution in Hz.</summary>*)
          
          property HzRes: Double read FHzRes write SetHzRes;
          (*<summary>Returns the distance in seconds between two consecutive samples of the signal
          on which the frequency spectrum is based.</summary>*)
          
          property Dt:Double read FDt write SetDt;

          (*<summary>Defines number of harmonics to be used when calculating
             statistical parameters from the amplitude spectrum.</summary>*)
          
          property Harmonics: integer read FHarmonics write SetHarmonics default 10;

          (*<summary>Defines the level of zero padding of the source signal.</summary>
             
<remarks>While zero padding does not increase frequency resolution, it does
             improve the accuracy of the frequency, amplitude and phase estimation
             of the spectral component.
</remarks>
*)
          
          property ZeroPadding: integer read FZeroPadding write SetZeroPadding default 1;








      end;

      

      

     (*<summary> Manages a list of TSpectrum objects. </summary>
       
<remarks>The component overrides the AddItem method and declares a new default
       array property returning TSpectrum type.
</remarks>
*)
      TSpectrumList = class(TAnalysisList)
      strict private
          function GetItems(index: integer): TSpectrum; reintroduce;
          procedure SetItems(index: integer; const Value: TSpectrum);
      strict protected
          
          function AddItem: TMtxComponent; override;
          
      public
         
          
          constructor Create(AOwner: TComponent); override;
          
          (*<summary>Access TSpectrum objects by Index.</summary>*)
          property Items[index: integer]: TSpectrum read GetItems write SetItems; default; 
      end;

      

      (*<summary>Handles a list of memory streams.</summary>*)
      
      TStringStreamList = class(TStringList )
      
      strict private
        function  GetStreams(Index: integer): TMemoryStream;
        procedure SetStreams(Index: integer; const Value: TMemoryStream);
      strict protected
        procedure ReadData(Reader: TReader);
        procedure WriteData(Writer: TWriter);
        procedure DefineProperties(Filer: TFiler); override;
        procedure SaveBinaryStream(Dst: TStream); overload; virtual;
        procedure LoadBinaryStream(Src: TStream); overload; virtual;
      

      
        procedure GetObjectToByteArray(var DstBuffer: Math387.TByteArray);
        procedure SetObjectFromByteArray(const SrcBuffer: Math387.TByteArray);
      public
      
        (*<summary>Initialize a new TMemoryStream named S and return
           the position in the list as the result.</summary>*)
        function  Add(const S: string): integer; override;
        
        procedure Assign(Source: TPersistent); override;

        procedure Delete(Index: integer); override;
        procedure Clear; override;

        
        
        (*<summary>Use the streams property to access memory streams owned by the TStringStreamList.</summary>*)
        property  Streams[Index: integer]: TMemoryStream read GetStreams write SetStreams;
        
        procedure LoadFromStream(Stream: TStream); overload; override;
        procedure SaveToStream(Stream: TStream); overload; override;
        
        (*<summary>Append new items from stream.</summary>*)
        procedure AppendFromStream(Stream: TStream); overload; virtual;
        

        
        (*<summary>Append new items from another list.</summary>*)
        procedure AppendFromList(List: TStringStreamList); virtual;
        
        destructor Destroy; override;
        
      end;

     (*<summary>An abstract class for stream capable TList.</summary>
      
<remarks>The class add's generic streaming capability
      to the basic TList object and also owns its items.
      All methods are virtual, but they could also be abstract.
</remarks>
*)

     TStreamedList = class(TObjectsList)
     public
        
        procedure SaveToStream(Dst: TStream); overload; virtual;
        procedure LoadFromStream(Src: TStream); overload; virtual;
        
        
        constructor Create(AOwnsObjects: boolean); overload;
        procedure ClearPointers; virtual;
        procedure Update; virtual;
     end;

      (*<summary>Streamable list of lists.</summary>
       
<remarks>The class handles storing, loading, decoding and encoding
       of a list of streams. The list of streams is stored in
       the Templates property. Each stream holds another
       list of objects. This lists are of TStreamedList type.
       When the user sets the TemplateIndex or the TemplateName property,
       the appropriate stream is decoded to Template property,
       where individual objects contained in the list can be accessed.
       All changes to those objects are retained and Templates
       property is kept up-to-date. For TStreamTemplates to handle
       user defined type of objects, a new class should be derived
       from TStreamedList type.
</remarks>
*)
      
      
      TStreamTemplates = class(TPersistent)
      strict private
          
          FTemplate: TStreamedList;
          FTemplateIndex: integer;
          FTemplates: TStringStreamList;
          procedure SetTemplateIndex(const Value: integer);
          procedure SetTemplateName(const Value: string);
          procedure SetTemplates(const Value: TStringStreamList);
          function GetTemplateName: string;
      strict protected
          
          
          FOwner: TComponent;
          
          function GetStreamedTemplate: TStreamedList; virtual;
          procedure SetTemplate(const Value: TStreamedList);
      public
          (*<summary>Holds a list of objects.</summary>
            
<remarks>All changes made to this list of objects
            are preserved, when you change TemplateName or TemplateIndex.
</remarks>
*)
          property Template: TStreamedList read FTemplate write SetTemplate;
          (*<summary>Returns the owner object.</summary>*)
          property Owner: TComponent read FOwner;
          
          
          procedure Assign(Source: TPersistent); override;
          
          

          (*<summary>Add a new template (TMemoryStream) named S to the list.</summary>*)
          procedure AddTemplate(const S: string); virtual;
          (*<summary>Add a new template named S to the list of templates and
          set TemplateIndex to point to the template added. </summary>*)
          procedure AddCopyTemplate(const S: string); virtual;
          (*<summary>Clears the Templates and Template properties.</summary>*)
          procedure Clear; virtual;
          (*<summary>Delete the template at position Index from the Templates property.</summary>*)
          procedure Delete(Index: integer); virtual;
          
          constructor Create(AOwner: TComponent); overload; virtual;
          destructor Destroy; override;
          
          (*<summary>Call this method to ensure that Template and Templates
             properties are synchronized.</summary>*)
          procedure Consolidate;
          (*<summary>Calls the Template.Update method.</summary>*)
          procedure Update; virtual;
          
          (*<summary>Save all data to the stream. </summary>*)
          procedure SaveToStream(Stream: TStream); overload; virtual;
          (*<summary>Load all data from the stream.</summary>*)
          procedure LoadFromStream(Stream: TStream); overload; virtual;
          

          

          (*<summary>Save the object to the file.</summary>*)
          procedure SaveToFile(FileName: string); virtual;
          (*<summary>Load the object from the file.</summary>*)
          procedure LoadFromFile(FileName: String); virtual;
      published
          
          (*<summary>Holds the list of streams containing data to be decoded to the Template property.</summary>
            
<remarks>, once you set TemplateIndex of TemplateName property.
</remarks>
*)
          property Templates: TStringStreamList read FTemplates write SetTemplates;

          
          (*<summary> Set TemplateIndex to decode the stream at position TemplateIndex to Template property. </summary>
           
<remarks>When the stream is decoded, a list of objects is created.
           Before the new stream is decoded, the data in Template property is encoded and stored
           to the Templates.
</remarks>
*)
          property TemplateIndex: integer read FTemplateIndex write SetTemplateIndex  default -1 ;

         
          (*<summary>Set TemplateName to decode the stream with name TemplateName to Template property.</summary>
           
<remarks>When the stream is decoded, a list of objects is created.
           Before the new stream is decoded, the data already in the Template property is encoded and stored
           to the Templates property.
</remarks>
*)
          property TemplateName: string read GetTemplateName write SetTemplateName stored false;
      end;



(*<summary>Copies a spectrum..</summary>
  
<remarks>Copies Src from SrcIndex to Dst from Index to Index+Len.
</remarks>
*)
procedure CopySpectrum(Src,Dst: TSpectrum; SrcIndex, Index, Len: integer); overload;

(*<summary>Defins fixed column width for text reports..</summary>
  
<remarks>Defines the fixed column width for some ValuesToStrings, MarksToStrings, etc.. multi-column
  report generating routines.
</remarks>
*)

var SignalTextColumnWidth: integer = 25;




