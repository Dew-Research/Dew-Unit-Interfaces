











(*<summary>Read or write files.</summary>*)
unit FileSignal;


interface

{$I BdsppDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}


   
       {$DEFINE WINDOWS_WAV}   
   



    {$DEFINE USING_FORMS}


uses MtxVec,  Math387,
     SignalTools, MtxBaseComp

     
       

       
       ,Types
       
     

     

     

     
       ,MtxDialogs
       
       ,MMSystem
       
       ,Classes
       ,SysUtils
       
       ,Contnrs
       
     

     
     ,ippspl
     

     
     ,ippsplSingle
     
     ;

    

    

    const  WAVE_FORMAT_IEEE_FLOAT   : cardinal = $0003; 
    const  WAVE_FORMAT_PCM          : cardinal = $0001;
    const  WAVE_FORMAT_EXTENSIBLE   : cardinal = $FFFE;
    const  KSDATAFORMAT_SUBTYPE_PCM : TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
    const  KSDATAFORMAT_SUBTYPE_IEEE_FLOAT: TGUID = '{00000003-0000-0010-8000-00aa00389b71}';

    

    

type
    

    

    






















      


          {$EXTERNALSYM TWaveFormatEx}
          
          TWaveFormatEx = packed record
            wFormatTag: Word;         
            nChannels: Word;          
            nSamplesPerSec: Cardinal;  
            nAvgBytesPerSec: Cardinal; 
            nBlockAlign: Word;      
            wBitsPerSample: Word;   
            cbSize: Word;           
          end;

          
          TWaveFormatExtensible = packed record
            Format: TWaveFormatEx;
            wValidBitsPerSample : Word;   
            dwChannelMask       : cardinal;  
            SubFormat           : TGUID;
          end;

          
          TWavNonPCMHeader = packed record  
            ckId0: Byte;   
            ckId1: Byte;   
            ckId2: Byte;   
            ckId3: Byte;   
            ckSize: Cardinal;  
            wave_ckID0: Byte; 
            wave_ckID1: Byte; 
            wave_ckID2: Byte; 
            wave_ckID3: Byte; 
            fmt_ckID0: Byte;  
            fmt_ckID1: Byte;  
            fmt_ckID2: Byte;  
            fmt_ckID3: Byte;  
            fmt_ckSize: Cardinal;     
            pcm: TWaveFormatEx;
            data_ckID0: Byte;   
            data_ckID1: Byte;   
            data_ckID2: Byte;   
            data_ckID3: Byte;   
            data_ckSize: Cardinal;
          end;

                    
          TWavExtendedHeader = packed record  
            ckId0: Byte;   
            ckId1: Byte;   
            ckId2: Byte;   
            ckId3: Byte;   
            ckSize: Cardinal;  
            wave_ckID0: Byte; 
            wave_ckID1: Byte; 
            wave_ckID2: Byte; 
            wave_ckID3: Byte; 
            fmt_ckID0: Byte;  
            fmt_ckID1: Byte;  
            fmt_ckID2: Byte;  
            fmt_ckID3: Byte;  
            fmt_ckSize: Cardinal;     
            pcm: TWaveFormatExtensible;    
         end;
























       





  


  (*<summary>Defines how to browse a multi-record file.</summary>
    
<remarks>Defines how the TSignalRead component should browse a multi-record file.
</remarks>
*)
   TFraming = (
   (*<summary>Calling NextFrame method will return the next frame from the current record.</summary>*)
   frSingleRecord,
   (*<summary>Calling NextFrame method will return the frame from the next record and at the same record position.</summary>*)
   frAcrossAllRecords
   );


   (*<summary>Specifies file formats supported.</summary>
     
<remarks>File formats supported by TSignalWrite and TSignalWrite components.
</remarks>
*)
     TFileFormat=(
     (*<summary>Multi record binary file. The header contains

        32bit unsigned - sample count in the data section
        64bit float - Sampling frequency
        32bit signed int - Precision (TPrecision)
        32bit signed int - Complex (bool)
        32bit signed int - ChannelCount

        Header is followed by data.
        Each data section can again be followed by a header.</summary>*)
     ffDat,
     (*<summary>Multi record text file. First line denotes number of entries.
        Second line is the total time of sampling and the third line
        containes the samples separated with ";". The samples can be complex.
        The fourth line is empty. This four lines can repeate any number of times
        within the file.</summary>*)
     ffSgl,
     (*<summary>Backward compatibility. Replaced by the ffDat format.</summary>*)
     ffBin,
     (*<summary>Windows OS uncompressed PCM wav file.</summary>*)
     ffWav,
     (*<summary>Single record text file. First line denotes number of entries.
        Second line is the the total time of sampling and the third line is empty.
        All the samples follow, each in its own line. Samples can be complex.</summary>*)
     ffSfs,
     (*<summary>A two column text file, first column is the time axis and the
       second column is the amplitude. The second column can hold complex numbers.</summary>*)
     ffAsc,
     (*<summary>Backward compatibility. Replaced by the ffDat format.</summary>*)
     ffDbn,
     (*<summary>Backward compatibility. Replaced by the ffDat format.</summary>*)
     ffDbl,
     (*<summary>Headerless binary file format.</summary>*)
     ffRaw,
     (*<summary>Headerless binary file format.</summary>*)
     ffPCM,
     (*<summary>Headerless text file format. Each column represents one chanel. The numbers can be complex.</summary>*)
     ffTxt
     );


     

     (*<summary>Abstract class for read and writing signals.</summary>
       
<remarks>Abstract class for signal reading and writing.
</remarks>
*)
     TFileStorage = class(TSignal)
         strict protected
            FFileName: TFileName;
            FileStream: TFileStream;
            
            FFileFormat: TFileFormat;
            FAutoFileFormat: boolean;
            procedure SetFileName(const Value: TFileName);
            procedure SetFileFormat(const Value: TFileFormat);
            procedure SetAutoFileFormat(const Value: boolean);
         strict protected
            FPrecision: TPrecision;
            FileReady: boolean;
            Strings: TStringList;
            
         public
            
            constructor Create(AOwner: TComponent); override;
            destructor Destroy; override;
            
            procedure CloseFile; virtual; abstract;
            (*<summary>Saves a header file describing file contents. </summary>
            
<remarks>The file is saved in the same directory as the signal file and has
            the same file name but with a .header extension.
</remarks>
*)
            procedure SaveHeaderFile;
         published
            
            (*<summary>If True, the file format will be automatically determined from the file name extension.</summary>
              
<remarks>File name is stored in the FileName property. Recognized file extensions are:
              dat, asc, sfs, sgl, bin, dbn, wav, raw, pcm.
</remarks>
*)
            property AutoFileFormat: boolean read FAutoFileFormat write SetAutoFileFormat  default true ;

            
            (*<summary>Defines the format of the file, if AutoFileFormat property is False.</summary>*)
            property FileFormat: TFileFormat read FFileFormat write SetFileFormat  default ffDat ;

            
            (*<summary>Defines the file name to be read from or written to.</summary>*)
            property FileName: TFileName read FFileName write SetFileName;
         end;

         (*<summary>Defines how to handle data at the end of file.</summary>
            
<remarks>Defines how to handle data at the end of the file, when
            the data is being read with the Update method, which
            advances the read cursor automatically. The sample count left
            to be read may be less then the Length specified and there are
            several options what to do.
</remarks>
*)
       TLastFrameCheck = (
       (*<summary>The last block of data will be zero padded. The length property will not be changed.</summary>*)
       lfcZeroPadded,
       (*<summary>The reading of the file will stop when
        the last full block of data has been read from the file.
        The length property will not be changed.
        With lfcZeroPadded the very last block read from the file
        will contain zeros padded after the position exceeding the size of the file. This padded
        zeros can affect any averaging process working on the data blocks. By selecting
        lfcLastFullBlock the IsEndOfFile will return true for the last
        completely full data block. The Pull method calls IsEndOfFile to check
        for the end of file.</summary>*)
       lfcLastFullBlock,
       (*<summary>The Length property and the size of the data block to be
        read will be adjusted to allow reading less than specified Length samples.
        The user must reset the Length property back to its orignal when
        read from the file again at some other position.</summary>*)
       lfcAdjustLength
       );

      

     
       
     

     (*<summary>Reads a signal from the file.</summary>
        
<remarks>TSignalRead is probably the most important component to understand
        in order to make effective use of other signal processing components.

        Use TSignalRead to read wav, binary and text files and provide
        streaming capabilities with frame navigation and overlapp support.
        The read data is placed in the Data property
        and its sample count is determined with the Length property.
        Once a file is opened, the key properties immediatelly
        contain all the necessary information about the file
        size, number of records, data type etc...

        You can connect the component to other components which have
        an Input property. Example: SpectrumAnalyzer.Input = SignalRead;
        By calling SpectrumAnalyzer.Pull data will be read from the file
        and a frequency spectrum will be computed.
</remarks>


        <example> Read multichannel files:
        <code>

        SignalRead.FileName := MyFile.wav;
        SignalRead.OpenFile;
        SignalDemuxList.Input := SignalRead;
        SignalDemuxList.Pull;

        //now we have demulitplexed data stored in:
        //SignalDemuxList[0].Data
        //SignalDemuxList[1].Data
        //..
        //SignalDemuxList[SignalRead.ChannelCount-1].Data

        </code>

        </example>*)
       TSignalRead = class(TFileStorage)
         strict private
         
            
            
            
            fFileSize: Int64;
            DataPos: Int64; 
            DataSize: Cardinal;
            PrevRecordNumber: integer;
            FNumberOfRecords: integer;
            FRecordLength: int64;
            fRecordTimePosition: Double;
            FRecordNumber: integer;
            fRecordTime: Double;
            FScaleFactor: Double;
            FFraming: TFraming;
            FOverlappingPercent: Double;
            FMaxFrames: int64;
            FOverlappingSamples: integer;
            FRecordPosition: Int64;
            FFrameNumber: int64;
            FFramesPerSecond: Double;
            FLoop: boolean;
            FFilePosition: int64;  
            LoadedData: TMtx;
            FOnRecordPositionChange: TNotifyEvent;
            FLastFrameCheck: TLastFrameCheck;
            FSelectionStop: int64;
            FOnProcessAll: TNotifyEvent;
            FForceLength: boolean;
            FSelectionStart: int64;
            FLastUpdatedLength: integer;
            FPostBufferTime: Double;
            FPostBufferSamples: integer;

            procedure ImportDatFile(FileName: string);
            procedure ImportSglFile(FileName: string);
            procedure ImportBinFile(FileName: string);
            procedure ImportWavFile(FileName: string);
            procedure ImportSfsFile(FileName: string);
            procedure ImportAscFile(FileName: string);
            procedure ImportDbnFile(FileName: string);
            procedure ImportDblFile(FileName: string);
            procedure ImportRawFile(FileName: string);
            procedure ImportPcmFile(FileName: string);
            procedure ImportTxtFile(FileName: string);
            procedure ProcessDatRecord(NewRecordPosition: Int64);
            procedure ProcessSglRecord(NewRecordPosition: Int64);
            procedure ProcessBinRecord(NewRecordPosition: Int64);
            procedure ProcessWavRecord(NewRecordPosition: Int64);
            procedure ProcessSfsRecord(NewRecordPosition: Int64);
            procedure ProcessAscRecord(NewRecordPosition: Int64);
            procedure ProcessDbnRecord(NewRecordPosition: Int64);
            procedure ProcessDblRecord(NewRecordPosition: Int64);
            procedure ProcessRawRecord(NewRecordPosition: Int64);
            procedure ProcessPcmRecord(NewRecordPosition: Int64);
            procedure ProcessTxtRecord(NewRecordPosition: Int64);
            procedure SetRecordNumber(const Value: integer);
            
            
            procedure SetScaleFactor(const Value: Double);
            procedure SetFraming(const Value: TFraming);
            procedure SetOverlappingPercent(const Value: Double);
            procedure SetOverlappingSamples(const Value: integer);
            procedure SetRecordPosition(const Value: int64);
            procedure SetFrameNumber(const Value: int64);
            procedure SetFramesPerSecond(const Value: Double);
            procedure SetLoop(const Value: boolean);
            function  SeekRecordNumber: boolean;
            function  SeekDatRecordNumber: boolean;
            function  SeekDatRecordPosition(NewRecordPosition: int64): boolean;
            procedure SetPrecision(const Value: TPrecision);
            procedure SetMaxFrames(const Value: int64);
            procedure SetNumberOfRecords(const Value: integer);
            procedure SetRecordLength(const Value: int64);
            procedure SetRecordTime(const Value: Double);
            procedure SetRecordTimePosition(const Value: Double);
            procedure ReadDatHeader;
            function  SeekRecordNumber2: boolean;
            procedure SetOnRecordPositionChange(const Value: TNotifyEvent);
            procedure SetSelectionStop(const Value: int64);
            procedure SetLastFrameCheck(const Value: TLastFrameCheck);
            procedure SetOnProcessAll(const Value: TNotifyEvent);
            function  GetFileSize: Int64;
            procedure SetForceLength(const Value: boolean);
            procedure SetSelectionStart(const Value: int64);
            procedure ImportRawPcm(FileName: string);
            procedure ProcessBinaryRecord(NewRecordPosition: int64);
            function  ExtractHeaderParam(const Str: string): string;
            procedure  SetPostBufferSamples(const Value: integer);
            procedure SetPostBufferTime(const Value: Double);
            
            procedure ReadExtHeader(FileStream: TFileStream; var extHeader: TWavExtendedHeader);
            
 protected 
             class  function EditorClass: string; override;
         strict protected
            function  StandardWavFormat: boolean;
            procedure UpdateSetLength(Sender: TObject); override;
            function InternalUpdate: TPipeState; override;
            procedure ProcessRecord(NewRecordPosition: Int64);
            procedure AdvanceFrame; virtual;
            procedure SetSamplingFrequency(const Value: Double); override;
         public
            (*<summary>Reads the header file for raw file formats. </summary>
                       
<remarks>This is neccessary only, if the stored parameters were changed after the file was already opened.
</remarks>
*)
            procedure LoadHeaderFile;
            (*<summary>Checks if the file format is raw. </summary>
              
<remarks>Returns true, if the file format specified by the
              file name is raw and does not contain header that would
              specify its contents like sampling frequency, channel count
              or precision stored.
</remarks>
*)
            function RawFileFormat: boolean;
            
            function InternalPull: TPipeState; override;
            
            (*<summary>Returnes the number of samples left to the end of the file per channel.</summary>
              
<remarks>It takes in to accout the SelectionStop property.
</remarks>
*)
            function SamplesLeft: Int64;
            (*<summary>Returns the size of the file in bytes.</summary>*)
            function FileSize: Int64;
            (*<summary>Returns true, if the file is open.</summary>*)
            function FileIsOpen: boolean;
            (*<summary>Advances the record position forward in the current record
               and reads the data.</summary>
               
<remarks>The sample count skiped is determined with the Length property and OverlappingSamples property.
</remarks>
*)
            procedure NextFrame;
            (*<summary>Moves the record position backward in the current record and reads the data.</summary>
               
<remarks>The sample count skiped is determined with the Length property and OverlappingSamples property.
</remarks>
*)
            procedure PrevFrame;
            (*<summary>Sets the record position to the start of the current record and reads the data.</summary>
               
<remarks>The sample count is determined with the Length property and OverlappingSamples property.
</remarks>
*)
            procedure FirstFrame;
            (*<summary>Sets the record position to the start of the last frame of the current record and reads the data.</summary>
            
<remarks>The sample count is determined with the Length property and OverlappingSamples property.
</remarks>
*)
            procedure LastFrame;
            (*<summary>Open file.</summary>*)
            procedure OpenFile; virtual;
            (*<summary>Close file.</summary>*)
            procedure CloseFile; override;
            (*<summary>Returns true, if the current frame is the last frame.</summary>*)
            function IsEndOfFile: boolean;
            
            constructor Create(AOwner: TComponent); override;
            destructor Destroy; override;
           
            
            (*<summary>Returns the name of the file format.</summary>*)
            function FormatTagDescription: string;
            (*<summary>Returns the description of the file format.</summary>*)
            function FormatDescription: string;

            procedure DetectChannelCount; override;
         published

            
            (*<summary>Specifies how to handle the end of file. </summary>
               
<remarks>The file being read by the component can be of any size,
               but data is being read in blocks of Length samples.
               This property determines how will the last data block
               be handled when there may not be enough data left for
               one full block.
</remarks>
*)
            property LastFrameCheck: TLastFrameCheck read FLastFrameCheck write SetLastFrameCheck default lfcZeroPadded;

            
            
            (*<summary> Specifies the upper file range limit. </summary>
               
<remarks>To limit the range of samples in the file being processed
               the SelectionStop can be set to something different then -1.
               If the file ends before the specified SelectionStop
               the processing will not be affected by the value of this property.

               The value of this property affects only the IsEndOfFile method.
               SelectionStop is defined in number of samples.
</remarks>
*)
            property SelectionStop: int64 read FSelectionStop write SetSelectionStop;

            
            
            (*<summary> Specifies the lower file range limit. </summary>
               
<remarks>To limited the range of samples being processed the
               SelectionStart can be set to something different then 0.
               SelectionStart is defined in number of samples.
</remarks>
*)
            property SelectionStart: int64 read FSelectionStart write SetSelectionStart;

            
            (*<summary>If true, looping will be enabled. </summary>
               
<remarks>If True, consecutive calls to NextFrame will reset to
               the record position of the first frame, once the end
               of the current record or selection is reached.
</remarks>
*)
            property Loop: boolean read FLoop write SetLoop  default False ;

            
            (*<summary>Defines a scale factor to be applied to just read data.</summary>*)
            property ScaleFactor: Double read FScaleFactor write SetScaleFactor;

            
            
            (*<summary>Read only. Returns the number of records contained in the current file.</summary>*)
            property NumberOfRecords: integer read FNumberOfRecords write SetNumberOfRecords stored False;

            
            
            (*<summary>Read only. Returns the length of the current record in samples for one channel.</summary>*)
            property RecordLength: int64 read FRecordLength write SetRecordLength stored False;

            
            
            (*<summary>Read only. Returns the length of the current record in seconds for one channel.</summary>*)
            property RecordTime: Double read FRecordTime write SetRecordTime stored False;

            
            
            
            (*<summary>Set RecordTimePosition in seconds to position the read cursor within the current record.</summary>
               
<remarks>Read RecordTimePosition to determine the current position of the read cursor within the current record.
               If the record is multiplexed, the position is defined for one channel.
</remarks>
*)
            property RecordTimePosition: Double read FRecordTimePosition write SetRecordTimePosition stored False;

            
            
            (*<summary>Set and get position in samples. </summary>
               
<remarks>Set RecordPosition in samples to position the read cursor within the current record
               and load Length samples from the Channel.
               Read Recordosition to determine the current position of the read cursor within the current record.
               If the record is multiplexed, the position is defined for sample count per single channel.
               Setting RecordPosition will also open file, if not already open.
</remarks>
*)
            property RecordPosition: int64 read FRecordPosition write SetRecordPosition stored false;

            
            
            (*<summary>Set RecordNumber to move the read cursor within the file to the record with RecordNumber number.</summary>
               
<remarks>Read RecordNumber property to determine the current record number.
               RecordPosition will not change. First record has a number 0.
</remarks>
*)
            property RecordNumber: integer read FRecordNumber write SetRecordNumber default 0;

            
            
            (*<summary>Define the overlapping in percent of consequtive frames when reading the file.</summary>*)
            property OverlappingPercent: Double read FOverlappingPercent write SetOverlappingPercent;


            
            
            (*<summary>Define the overlapping in sampels of consequtive frames when reading the file.</summary>*)
            property OverlappingSamples: integer read FOverlappingSamples write SetOverlappingSamples default 0;

            
            
            (*<summary>Read Only. Determine the maximum number of frames. </summary>
              
<remarks>Determine the maximum number of frames within the current record with
              the current setting of OverlappingSamples property.
</remarks>
*)
            property MaxFrames: int64 read FMaxFrames write SetMaxFrames stored False;

            
            
            (*<summary>Defines the method to traverse a multi-record the file.</summary>
              
<remarks>With each consecutive call to NextFrame, you can either move along
              the same record (increasing RecordPosition) or across all the records (increasing RecordNumber).
</remarks>
*)
            property Framing: TFraming read FFraming write SetFraming  default frSingleRecord ;

            
            
            (*<summary>Returns the number of frames per second according to Framing and overlapp properties.</summary>*)
            property FramesPerSecond: Double read FFramesPerSecond write SetFramesPerSecond stored False;

            
            
            (*<summary>Set FrameNumber to position the read cursor within the current record.</summary>
               
<remarks>Read the property to determine the current frame number (position) within the current record.
</remarks>
*)
            property FrameNumber: int64 read FFrameNumber write SetFrameNumber stored false;

            
            
            (*<summary>Read the precision of the data being read from the file.</summary>*)
            property Precision: TPrecision read FPrecision write SetPrecision stored False;

            
            (*<summary>Triggered after RecordPosition changes.</summary>*)
            property OnRecordPositionChange: TNotifyEvent read FOnRecordPositionChange write SetOnRecordPositionChange;

            
            (*<summary>Event triggered when Process command is selected from
               the drop-down menu in the component editor.</summary>*)
            property OnProcessAll: TNotifyEvent read FOnProcessAll write SetOnProcessAll;

            
            (*<summary>Warn about compressed files varying Length read.</summary>
              
<remarks>In case of variable bit rate codecs or codecs whose bit rate is not
              a multiple of 8 bits, the number of samples being read from the file
              can not be always matched with the specified TSignalRead.Length property.
              If ForceLength is True and the value of the Length property can not be
              matched an exception will be raised. The position within the file with
              variable bit rate can be defined only approximately (in steps of 0.1 seconds
              for example.)

              When the data blocks are read without explicit changes to the
              RecordPosition, all data blocks are read consecutively without overlapping
              or loosing any samples. (But the Length property will vary between
              consecutive calls.
</remarks>
*)
            property ForceLength: boolean read FForceLength write SetForceLength ;

            
            
            
            (*<summary> Append zeros to end of file. </summary>
                
<remarks>Set this to greater than zero in seconds, to compensate
                for FIR filter delays and clicks at the end of the playback.
</remarks>
*)
            property PostBufferTime: Double read FPostBufferTime write SetPostBufferTime;

            
            
            (*<summary> Append zeros to then end of file. </summary>
                
<remarks>Set this to greater than zero in samples per channel, to compensate
                for FIR filter delays and clicks at the end of the playback.
                The file will be virtually extended by adding zeros at the end.
</remarks>
*)
            property PostBufferSamples: integer read FPostBufferSamples write SetPostBufferSamples;
         end;

      

       

     (*<summary>Writes a signal to a file.</summary>
       
<remarks>Use TSignalWrite to write wav, binary and text files
       and provide streaming capabilities.

       The data to be written is connected to the Input property.
       If you would like to save data in the Data property,
       connect the component to itself.
</remarks>
*)
       TSignalWrite = class(TFileStorage)
         
         strict private
            RecordInitialized: boolean;
            DoInitRecord: boolean;
            DataSize: integer;
            RecordStartOffset: Int64;
            FTotalRecordLength: Int64;
            FAppendFile: boolean;
            FRounding: TRounding;
            FRecordNumber: integer;
            FAuthorInfo: string;
            FReFormat: string;
            FImFormat: string;
            
            npcmH: TWavNonPCMHeader;

            
            procedure SetAppendFile(const Value: boolean);
            procedure SetRounding(const Value: TRounding);
            procedure SetRecordNumber(const Value: integer);
            function  SeekRecordNumber: boolean;
            procedure SetPrecision(const Value: TPrecision);
            procedure SetRecordLength(const Value: Int64);
            procedure SetRecordTime(const Value: Double);
            function GetRecordLength: int64;
            function GetRecordTime: Double;
            procedure SetTotalRecordLength(const Value: Int64);
            procedure SetAuthorInfo(const Value: string);
            procedure SaveTxtBlock;
            procedure SetReFormat(const Value: string);
            procedure SetImFormat(const Value: string);
            
            procedure WriteExtHeader(FileStream: TFileStream; var extHeader: TWavNonPCMHeader);
            
        strict protected
            function InternalUpdate: TPipeState; override;
            procedure SaveAscBlock;
        public
            (*<summary>Returns the amount of data written so far in bytes. </summary>
              
<remarks>Does not inlude file header size.
</remarks>
*)
            function BytesWritten: int64;
            (*<summary>Returns true, if the file is open.</summary>*)
            function FileIsOpen: boolean;
            (*<summary>Appends a new record to the file with Filename.</summary>
               
<remarks>The file is opened, the Data is appended
               as a single record and the file is closed.
</remarks>
*)
            procedure AppendRecord;
            (*<summary>Initialize a new record.</summary>
              
<remarks>When writing multi-record
               files, call InitRecord each time you want to start
               a new a record. For each InitRecord a call to CloseRecord
               is made first. InitRecord and CloseFile will also call CloseRecord.
               InitRecord does not write data to the file. Instead
               it allocates space for the record header.
               When a call to SaveBlock is made, InitRecord will be called
               automatically, if the record has not yet been initialized.
</remarks>
*)
            procedure InitRecord;
            (*<summary>Call the CloseRecord once the record has been written.</summary>
               
<remarks>Usually this method is used in the sequence:
               InitRecord, SaveBlock,..., SaveBlock, CloseRecord.
               InitRecord, SaveBlock,...., SaveBlock,....
               You do not have to call it after a call to InitRecord:
               InitRecord, SaveBlock,..., SaveBlock, InitRecord, SaveBlock,....
               If InitRecord is called and the record is open, CloseRecord
               is called automatically.
</remarks>
*)
            procedure CloseRecord;
            (*<summary>Save Data as a single record to the file.</summary>
              
<remarks>Close record does not write any data to the file.
</remarks>
*)
            procedure SaveRecord;
            (*<summary>Save Data to the current record and advance the file position.</summary>
               
<remarks>Call CloseRecord at the end of calls to update the total length of the record.
</remarks>
*)
            procedure SaveBlock;
            (*<summary>Prepare the file with FileName for writing.</summary>*)
            procedure SaveFile;
           (*<summary>Close the file with FileName.</summary>
            
<remarks>Usually the method is used in the call sequence:
            SaveFile, InitRecord,SaveBlock,..., SaveBlock, CloseRecord,
            IniRecord,.....  ,CloseRecord, CloseFile.
</remarks>
*)
            procedure CloseFile; override;
            (*<summary>Saves the data connected to the input property
               to the file with FileName.</summary>
               
<remarks>This method cals: SaveFile, InitRecord, SaveBlock, CloseRecord,
               CloseFile.

               Use this method when the data is not streamed.
</remarks>
*)
            procedure SaveDataToFile(const aFileName: string);

            
            function InternalPull: TPipeState; override;
            constructor Create(AOwner: TComponent); override;
           
            
        published
            
            (*<summary>Number format definition for text file formats.</summary>
              
<remarks>Specifies the number format of the real part of the complex number for
              the text file formats. To use default (general floating point) formatting, leave this property empty.
</remarks>
*)
            property ReFormat: string read FReFormat write SetReFormat;

            
            (*<summary>Number format definition for text file formats.</summary>
              
<remarks>Specifies the number format of the imaginary part of the complex number for
              the text file formats. To use default (general floating point) formatting, leave this property empty.
</remarks>
*)
            property ImFormat: string read FImFormat write SetImFormat;

            
            (*<summary>Holds the string added to the header of the saved wav file.</summary>*)
            property AuthorInfo: string read FAuthorInfo write SetAuthorInfo;

            
            (*<summary>Source data to be written to the file. </summary>*)
            property Input: TSignal read GetInput1 write SetInput1 stored InputsStored;  

            
            (*<summary>If True, the SaveFile method will set the file cursor at the end of the file.</summary>
             
<remarks>Any records written to the file will be appended to the existing ones. If the value of the property
             is false, the existing file will be overwritten.
</remarks>
*)
            property AppendFile: boolean read FAppendFile write SetAppendFile  default false ;

            
            (*<summary>Define the rounding mode when saving to integer types.</summary>*)
            property Rounding: TRounding read FRounding write SetRounding  default rnTrunc ;

            
            (*<summary>Move the file position cursor to the beginging of the record number RecordNumber.</summary>*)
            property RecordNumber: integer read FRecordNumber write SetRecordNumber default 0;

            
            (*<summary>Defines the precision with which the Data will be saved. </summary>
               
<remarks>For text file types the formating is specified with ReFormat and ImFormat.
</remarks>
*)
            property Precision: TPrecision read FPrecision write SetPrecision  default   prDouble   ;

            
            
            (*<summary>Read only. Returns the length in samples of the Data
               written so far in the current record per channel. </summary>*)
            property RecordLength: int64 read GetRecordLength write SetRecordLength stored False;

            
            
            (*<summary>Read only. Returns the length in seconds of the Data
               written so far in the current record per channel. </summary>*)
            property RecordTime: Double read GetRecordTime write SetRecordTime stored False;

            
            
            (*<summary>Returns the sample count written for all channels.</summary>
               
<remarks>Returns the length in samples of the Data written so far in the current record of all channels.
               The value of this property is the same as RecordLength, if ChannelCount is 1.
</remarks>
*)
            property TotalRecordLength: int64 read FTotalRecordLength write SetTotalRecordLength stored False;
         end;

      

     

      (*<summary>Manage a list of TSignalRead components.</summary>*)
      TSignalReadList = class(TSignalList)
      strict private
          function GetItems(index: integer): TSignalRead; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalRead);
      strict protected
          function  AddItem: TMtxComponent; override;
      public
         
         
          constructor Create(AOwner: TComponent); override;
         
          (*<summary>Add another TSignalRead item to the List.</summary>*)
          procedure Add;  reintroduce; 
          (*<summary>Close all files specified in Items[i].FileName.</summary>*)
          procedure CloseFiles;
          (*<summary>Default array property access to the items of the list.</summary>*)
          property Items[index: integer]: TSignalRead read GetItems write SetItems;  default; 
      end;

      

      

     (*<summary>Manage a list of TSignalWrite components.</summary>*)
      TSignalWriteList = class(TSignalList)
      strict private
          function GetItems(index: integer): TSignalWrite; reintroduce;
          procedure SetItems(index: integer; const Value: TSignalWrite);
      strict protected
          function  AddItem: TMtxComponent; override;
      public
         
         
          constructor Create(AOwner: TComponent); override;
         
          (*<summary>Add another TSignalWrite item to the List.</summary>*)
          procedure Add;  reintroduce; 
          (*<summary>Close the files with Items[i].FileName.</summary>*)
          procedure CloseFiles;
          (*<summary>Prepare the files with Items[i].FileName for writing.</summary>*)
          procedure SaveFiles;
          (*<summary>Default array property access to the items of the list.</summary>*)
          property Items[index: integer]: TSignalWrite read GetItems write SetItems; default; 
      published
          
          (*<summary>Data source.</summary>
            
<remarks>Defines the data source for the component, if the source
            is a list of components. Setting this property to other then nil,
            will set Input property to nil.
</remarks>
*)
          property Inputs: TAnalysisList read FInputs write SetInputs; 
      end;


      


      (*<summary>Interface the audio compression manager.</summary>
        
<remarks>Audio compression manager is a Windows service for compressing
        and decompressing audio data.
</remarks>
*)
      
      
      


    (*<summary> Stores a list of TVec objects using TSignalWrite to one of the supported formats <see cref="TFileFormat"/> </summary>
                 
<remarks>The file format is determined from the extension of the file. Precision and
                 FS (Sampling frequency) also need to be specified.
</remarks>
*)
    procedure SaveSignalFile(const a: array of TVec; FileName: string; Precision: TPrecision; FS: Double);
    (*<summary> Converts file extension from string to <see cref="TFileFormat"/> </summary>*)
    function ExtensionToFileSignalFormat(Extension: string): TFileFormat; overload;
    (*<summary> Converts file extension <see cref="TFileFormat"/> to string. </summary>*)
    function FileSignalFormatToExtension(aFormat: TFileFormat): string;
   (*<summary> Sets multiple or trailing tab charachters before or after number to space char. </summary>*)
    function NumberizeString(const src: string): string;

(*<summary>Drop down file types string.</summary>
  
<remarks>A string to be assigned to the Filter property of the TFileOpen or TFileSave dialog.
</remarks>
*)
var  SignalDialogFilter: string =
             'Binary multi precision records (*.dat)|*.dat|' +



             'Single column ASCII (*.sfs)|*.sfs|'+
             'Two column ASCII (*.asc)|*.asc|'+
             'Windows PCM (*.wav)|*.wav|'+
             'Raw PCM (*.raw)|*.raw|'+
             'Raw PCM (*.pcm)|*.pcm|'+
             'Text (*.txt)|*.txt|'+

             'All known file extensions|*.dat;*.sfs;*.asc;*.wav;*.raw;*.pcm;*.txt|' +  
             'All files (*.*)|*.*';  

var
(*<summary>Default file extension for level 1 peak files produced by <see cref="TSignalBrowse"/> .</summary>*)
 PeakFileExtension: string = '.peak';
var
(*<summary>Default file extension for level 2 peak files produced by <see cref="TSignalBrowse"/> .</summary>*)
  PeakFileExtension2: string = '.peakk';





