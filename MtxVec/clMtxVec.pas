









(*<summary>Implements objects for use of Open CL.</summary>
  
<remarks>The unit declares TOpenCLVector and TOpenCLMatrix types and object wrappers for the most of the Open CL driver features.
</remarks>
*)
unit clMtxVec;


interface

{$DEFINE STDCALL}
{$INCLUDE 'bdsppdefs.inc'}

{$WARN SYMBOL_DEPRECATED OFF}

uses
  OpenCL_Dynamic, Math387
  
    
    ,Types
    
    ,ZLib
  


  
    
      ,Classes
      ,SysUtils
      ,SyncObjs
      
      ,Dialogs
      ,Forms
      
      ,StringVar
      
    
  

  
  ,MtxVec, AbstractMtxVec, MtxVecBase, AbstractMtxVecInt, MtxVecInt
  
  

  
    
    ,Windows
    ,ShlObj
    
  

  
  ;

const

  (*<summary> Text identifying the NVIDIA platform. </summary>
               
<remarks>Use this constant to compare against the <see cref="TOpenCLPlatform.Name"/> property.
</remarks>
*)
  NVIDIA_PLATFORM = 'NVIDIA CUDA';

  (*<summary> Text identifying the AMD platform. </summary>
               
<remarks>Use this constant to compare against the <see cref="TOpenCLPlatform.Name"/> property.
</remarks>
*)
  AMD_PLATFORM = 'AMD Accelerated Parallel Processing';

  (*<summary> Text identifying the INTEL platform. </summary>
               
<remarks>Use this constant to compare against the <see cref="TOpenCLPlatform.Name"/> property.
</remarks>
*)
  INTEL_PLATFORM = 'Intel(R) OpenCL';

  (*<summary> Resource id for loading CPU optimized double precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_CPU_DOUBLE = 12386;
  (*<summary> Resource id for loading CPU optimized single precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_CPU_SINGLE = 12387;
  (*<summary> Resource id for loading GPU optimized double precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_GPU_DOUBLE = 12388;
  (*<summary> Resource id for loading GPU optimized single precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_GPU_SINGLE = 12389;

  (*<summary> Resource id for loading CPU optimized double precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_CPU_DOUBLE_STR = 'DEW_1D_KERNELS_CPU_DOUBLE';
  (*<summary> Resource id for loading CPU optimized single precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_CPU_SINGLE_STR = 'DEW_1D_KERNELS_CPU_SINGLE';
  (*<summary> Resource id for loading GPU optimized double precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_GPU_DOUBLE_STR = 'DEW_1D_KERNELS_GPU_DOUBLE';
  (*<summary> Resource id for loading GPU optimized single precision kernels. </summary>
               
<remarks>Use this constant as a parameter to the <see cref="TOpenCLProgram.LoadProgram"/> method.
</remarks>
*)
  DEW_1D_KERNELS_GPU_SINGLE_STR = 'DEW_1D_KERNELS_GPU_SINGLE';

  OPENCL_BLOCKLEN = 128;


type

  TLocalMemType = (lmtGlobal, lmtLocal);
  (*<summary> Specifies the precision to be used for Open CL computation. </summary>*)
  TclFloatPrecision = (clFloat, clDouble);

  (*<summary> Contains additional device information for NVIDIA devices. </summary>*)
  TNVDeviceInfo = class
  strict private
    FKernelExecTimeout: boolean;
    FComputeCapabilityMajor: integer;
    FComputeCapabilityMinor: integer;
    FWarpSize: integer;
    FGPUOverlap: boolean;
    FRegistersPerBlock: integer;
    FIntegratedMemory: boolean;
  private
    (*<summary> Internal use only. </summary>*)
    procedure Update(DeviceID: cl_device_id);
  public
    (*<summary> Compute capability equal to 2 implies a Fermi GPU. </summary>*)
    property ComputeCapabilityMajor: integer read FComputeCapabilityMajor;
    (*<summary> Compute capability version x in y.x </summary>
                
<remarks>Compute capability of at least 1.3 is required to allow execution of
                double precision code.
</remarks>
*)
    property ComputeCapabilityMinor: integer read FComputeCapabilityMinor;
    (*<summary>Returns number of registers available per compute block. Typically 8192. </summary>*)
    property RegistersPerBlock: integer read FRegistersPerBlock;
    (*<summary> Returns the size of the warp. Typically 32.  </summary>*)
    property WarpSize: integer read FWarpSize;
    (*<summary> Specifies if device supports overlapping copy and compute operations. </summary>
                 
<remarks>With devices 1.2 and later, it is possible to overlap kernel execution on the
                 device with data transfers between the host and the device. With devices 2.0
                 and later two independent copy engines are capable of simultaneous copy
                 in two directions concurrent with device computation. In order to overlap
                 computation and copy operations at least two command queues need to be created
                 for the device and device must support multiple command queues.
</remarks>
*)
    property GPUOverlap: boolean read FGPUOverlap;
    (*<summary> Returns true if Kernel execution can timeout.  </summary>*)
    property KernelExecTimeout: boolean read FKernelExecTimeout;
    (*<summary> Returns true if GPU has integrated memory (not Host shared).  </summary>*)
    property IntegratedMemory: boolean read FIntegratedMemory;
  end;

  (*<summary> Contains additional device information for AMD devices. </summary>*)
  TAMDDeviceInfo = class
  strict private
    FMediaOps: boolean;
    FOpenVideo: boolean;
    FEventCallback: boolean;
    FFP64: boolean;
    FPrintf: boolean;
  private
 (*<summary> Internal use only. </summary>*)
    procedure Update(const Extensions: TStrings);
  public
   (*<summary> Returns true if the device is capable of double precision processing. </summary>
                
<remarks>To actually enable support for double precision you need to include:

                #pragma OPENCL EXTENSION cl_amd_fp64 : enable

                In the source code header.
</remarks>
*)
    property FP64: boolean read FFP64;

    (*<summary> Returns true, if the device provides the ability to register event callbacks for
                 states other than cl_complete (cl_queued, cl_submitted, cl_running). </summary>
                 
<remarks>Event callbacks are enabled by default, if available.
</remarks>
*)
    property EventCallback: boolean read FEventCallback;

    (*<summary> Returns true if the device supports custom AMD media functions. </summary>
                
<remarks>The functions are: amd_pack, amd_unpack0, amd_unpack1, amd_unpack2, amd_unpack3, amd_bitalign,
                amd_lerp,... To enable support for this commands include:

                #pragma OPENCL EXTENSION cl_amd_media_ops : enable

                In the source code header.
</remarks>
*)
    property MediaOps: boolean read FMediaOps;

    (*<summary> Returns true if the device supports hardware fixed-function video compression. </summary>
                
<remarks>To enable support for this feature include:

                #pragma OPENCL EXTENSION cl_amd_open_video : enable

                In the source code header.
</remarks>
*)
    property OpenVideo: boolean read FOpenVideo;

   (*<summary> Returns true if the device supports printf command. </summary>
                
<remarks>To enable support for this feature include:

                #pragma OPENCL EXTENSION cl_amd_printf : enable

                In the source code header.
</remarks>
*)
    property Printf: boolean read FPrintf;
  end;

  (*<summary> Contains device information specific to floating point math. </summary>*)
  TFPInfo = class
  strict private
    FInfNan: boolean;
    FRoundToZero: boolean;
    FFusedMA: boolean;
    FDenorms: boolean;
    FRoundToInf: boolean;
    FRoundToNearest: boolean;
  private
    procedure Update(const Src: UInt64);
  public
    (*<summary> Device supports denormals. </summary>*)
    property Denorms: boolean read FDenorms;
    (*<summary> Device supports INF and NAN numbers. </summary>*)
    property InfNan: boolean read FInfNan;
    (*<summary> Device supports round to nearest integer. </summary>*)
    property RoundToNearest: boolean read FRoundToNearest;
    (*<summary> Device supports round to towards zero. </summary>*)
    property RoundToZero: boolean read FRoundToZero;
    (*<summary> Device supports round to infinity. </summary>*)
    property RoundToInf: boolean read FRoundToInf;
    (*<summary> Device supports fused multiply and add instruction. </summary>*)
    property FusedMA: boolean read FFusedMA;
  end;

  TOpenCLPlatform = class;
  TOpenCLCache = class;
  TOpenCLKernelList = class;
  TOpenCLProgramList = class;
  TOpenCLCommandQueueList = class;
  TOpenCLCommandQueue = class;

  (*<summary> Stores data and information associated with the Open CL computing device. </summary>
               
<remarks>The object holds all information describing the properties of the device, programs,
               command queues and cache (buffer objects). To use the device we first define the
               size of the Cache and load the Programs. Then we can access Kernels which can be
               enqueued to Command queues.
</remarks>
*)

  TOpenCLDevice = class
  strict private
    FName: string;
    FVendor: string;
    FVendorID: PointerInteger;
    FDeviceVersion: string;
    FOpenCLVersion: integer;
    FDriverVersion: string;
    FPreferredVectorWidthFloat: integer;
    FQueueOutOfOrderExec: boolean;
    FPreferredVectorWidthShort: integer;
    FImage3DMaxDepth: integer;
    FExtensions: TStringList;
    FMaxConstantBufferSize: Int64;
    FQueueProfiling: boolean;
    FMaxWorkItemDimensions: integer;
    FIsDefault: boolean;
    FIsAccType: boolean;
    FImage2DMaxHeight: integer;
    FImageSupport: boolean;
    FImage3DMaxHeight: integer;
    FErrorCorrectionSupport: boolean;
    FMaxClockFrequency: integer;
    FMaxWorkGroupSize: integer;
    FMaxComputeUnits: integer;
    FMaxReadImageArgs: integer;
    FPreferredVectorWidthDouble: integer;
    FIsCpuType: boolean;
    FLocalMemType: TLocalMemType;
    FLocalMemSize: Int64;
    FNVInfo: TNVDeviceInfo;
    FMaxWriteImageArgs: integer;
    FMaxMemAllocSize: Int64;
    FIsGpuType: boolean;
    FPreferredVectorWidthInt: integer;
    FAddressBits: integer;
    FImage2DMaxWidth: integer;
    FImage3DMaxWidth: integer;
    FPreferredVectorWidthChar: integer;
    FGlobalMemSize: Int64;
    FPreferredVectorWidthLong: integer;
    FMaxWorkItemSizes: array [0..2] of integer;
    fDeviceID: cl_device_id;
    FAvailable: boolean;
    FCompilerAvailable: boolean;
    FExecutionCapabilityNative: boolean;
    FGLobalMemCacheSize: Int64;
    FGlobalMemCachelineSize: integer;
    FMaxParameterSize: integer;
    FMaxSamplers: integer;
    FMinDataTypeAlignSize: integer;
    FMemBaseAddrAlign: integer;
    FDeviceProfile: string;
    FProfilingTimerResolution: integer;
    FFPSingleInfo: TFPInfo;
    FFP64: boolean;
    FFP16: boolean;
    FAMDInfo: TAMDDeviceInfo;
    FFPDoubleInfo: TFPInfo;
    FFPHalfInfo: TFPInfo;
    FByteAddressableStore: boolean;
    FGLSharing: boolean;
    FD3D10Sharing: boolean;
    fCommandQueue: TOpenCLCommandQueueList;
    
    fPlatform: TOpenCLPlatform;

    FCache: TOpenCLCache; 
    FIsDebugging: boolean;
    fKernels: TOpenCLKernelList;
    FMappedCopy: boolean;
    fPrograms: TOpenCLProgramList;
    FNativeCPU: boolean;
    fQueueImmediateExecution: boolean;
    procedure SetNVInfo(const Value: TNVDeviceInfo);
    procedure SetFPSingleInfo(const Value: TFPInfo);
    procedure SetAMDInfo(const Value: TAMDDeviceInfo);
    procedure SetFPDoubleInfo(const Value: TFPInfo);
    procedure SetFPHalfInfo(const Value: TFPInfo);
    procedure SetCache(const Value: TOpenCLCache);
    procedure SetIsDebugging(const Value: boolean);
    procedure SetMappedCopy(const Value: boolean);
    procedure SetNativeCPU(const Value: boolean); 
    function GetDeviceIndex: integer;
    function GetPlatformIndex: integer;
  protected
    property MappedCopy: boolean read FMappedCopy write SetMappedCopy;
    procedure Update;
  public
    (*<summary>Maximum number of work-items that can be specified in each dimension of the work-group. </summary>
                  
<remarks>Returns n entries, where n is the value of <see cref="MaxWorkItemDimensions"/>.
</remarks>
*)
    function GetMaxWorkItemSizes(Idx: integer): integer;
    (*<summary> Returns the platform of the device. </summary>*)
    property DevicePlatform: TOpenCLPlatform read fPlatform;
    (*<summary> Returns the index of the device in the list of devices for the platform. </summary>*)
    property DeviceIndex: integer read GetDeviceIndex;
    (*<summary> Returns the index of the platform in the list of platforms. </summary>*)
    property PlatformIndex: integer read GetPlatformIndex;
    (*<summary> The kernel list holds the list of kernels (Open CL functions) that are available. </summary>
                 
<remarks>Kernel list is populated when a <see cref="TOpenCLProgram">Program</see> is loaded. Multiple programs can be loaded
                 and all will have their <see cref="TOpenCLKernel">Kernels</see> added to this one list. When a program is deleted
                 corresponding kernels will also be removed from this list. The desired Kernel is searched
                 for with the help of the <see cref="TOpenCLKernelList.Locate">Locate</see> method.
</remarks>
*)
    property Kernels: TOpenCLKernelList read fKernels;

    (*<summary> The program list holds the list of compiled Open CL programs. </summary>
                 
<remarks>To add a new program call the <see cref="TOpenCLProgramList.Add">Add</see> method. Then load
                 the desired program using the <see cref="TOpenCLProgram.LoadProgram"/>.
                 The programs loaded will be compiled on the first load and have binaries stored for subsequent
                 faster loads. Programs can be loaded from resource, from text files or directly from a list
                 of strings. All kernels (Open CL functions) contained in the loaded programs will be automatically
                 added to the list of <see cref="Kernels"/>.
</remarks>
*)
    property Programs: TOpenCLProgramList read fPrograms;

    (*<summary> The Cache object holds a list of <see cref="TOpenCLVector"/> and <see cref="TOpenCLMatrix"/> objects. </summary>
                 
<remarks>The list of objects is used to prevent frequent memory allocation/deallocation and to increase memory reuse.
</remarks>
*)
    property Cache: TOpenCLCache read FCache write SetCache;
    (*<summary> The list of available command queues. There is always at least one queue.  </summary>
                  
<remarks>Each time a function is to be called, the corresponding OpenCL kernels needs to be added
                  to queue associated with the device. This object contains a list of queues to which we
                  can enqueue our Kernels. For some devices only one queue is allowed. By default one
                  queue is created when the object is initialized. Multiple queues can be useful when
                  there is a need to achieve overlap between data copy and data compute. While one
                  queue is running computation on the device, the other can be busy copying data
                  from or to the device. All Kernels enqueued to all command queues on the given
                  device share the <see cref="TOpenCLCache">Cache</see> of that device. Only one queue
                  can be associated with one thread.

                  The associated threads will swift through all code submitting possibly many hundreds
                  of Kernels to the queues and will then block on the line where the resulting data is to
                  be copied from the device until that data will be ready. While debugging however,
                  the queues will block on every breakpoint or step to allow inspection of the data
                  being processed.

                  When not debugging the threads will reach end of program long before the actual
                  computation on the device will finish and will wait there for the device finish
                  as well and to return desired results.
</remarks>
*)
    property CommandQueue: TOpenCLCommandQueueList read fCommandQueue;

    property IsDebugging: boolean read FIsDebugging write SetIsDebugging;
    (*<summary> Returns the Device ID assigned to the device by Open CL driver.  </summary>
                 
<remarks>Use this value when you need to refer to this device when calling Open CL functions.
</remarks>
*)
    property DeviceID: cl_device_id read FDeviceID;
    (*<summary> Returns true, if the device is available for use. </summary>*)
    property Available: boolean read FAvailable;
    (*<summary> Returns true, if the Open CL driver has a compiler included. </summary>
                 
<remarks>This is always true at least for Windows OS based computers.
</remarks>
*)
    property CompilerAvailable: boolean read FCompilerAvailable;
    (*<summary> Returns true, if the driver can execute native (not Open CL) kernels. </summary>*)
    property ExecutionCapabilityNative: boolean read FExecutionCapabilityNative;
    (*<summary> Returns global memory cache size in KBytes. </summary>*)
    property GLobalMemCacheSize: Int64 read FGLobalMemCacheSize;
    (*<summary> Returns global memory cache line size in Bytes. </summary>*)
    property GlobalMemCachelineSize: integer read FGlobalMemCachelineSize;
    (*<summary> Returns maximum size of the parameter in bytes. </summary>
                
<remarks>Max size in bytes of the arguments that can be passed to a kernel. The minimum value is 256 bytes.
</remarks>
*)
    property MaxParameterSize: integer read FMaxParameterSize;
    (*<summary> Maximum number of samplers that  can be used in a kernel. </summary>
                
<remarks>The minimum value is 8 if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property MaxSamplers: integer read FMaxSamplers;
    (*<summary> Minimum data alignment size. </summary>
                
<remarks>The minimum value is the size (in bytes) of the largest OpenCL builtin
                data type supported by the device (long16 in FULL profile, long16 or int16 in EMBEDDED profile).
</remarks>
*)
    property MinDataTypeAlignSize: integer read fMinDataTypeAlignSize;
    (*<summary> Allignment in bits of memory buffers. </summary>
                
<remarks>The minimum value is the size (in bits) of the largest OpenCL built-in
                data type supported by the device (long16 in FULL profile, long16 or int16 in EMBEDDED profile).
</remarks>
*)
    property MemBaseAddrAlign: integer read FMemBaseAddrAlign;
    (*<summary> OpenCL profile string. </summary>
                
<remarks>Returns the profile name supported by the device. The profile name returned can be one
                of the following strings:
                <para/>
                FULL_PROFILE – if the device supports the OpenCL specification (functionality defined
                as part of the core specification and does not require any extensions to be supported).
                <para/>
                EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile.
</remarks>
*)
    property DeviceProfile: string read FDeviceProfile;
    (*<summary> Describes the resolution of device timer. This is measured in nanoseconds. </summary>*)
    property ProfilingTimerResolution: integer read FProfilingTimerResolution;  
    (*<summary> Returns the name of the device. </summary>*)
    property Name: string read FName;
    (*<summary> Returns the name of the Vendor. </summary>*)
    property Vendor: string read FVendor;
    (*<summary> Returns the value of the Vendor ID. </summary>*)
    property VendorID: PointerInteger read FVendorID;
    (*<summary> Returns Open CL Version. </summary>
                 
<remarks>The first numeral is the major version and the second number represents the minor version.
</remarks>
*)
    property OpenCLVersion: integer read FOpenCLVersion;
    (*<summary>Returns the device version. </summary>*)
    property DeviceVersion: string read FDeviceVersion;
    (*<summary>Returns the Open CL driver version. </summary>
                 
<remarks>The form returned is major_number.minor_number
</remarks>
*)
    property DriverVersion: string read FDriverVersion;
    (*<summary>Returns true if the device is of CPU type. </summary>*)
    property IsCpuType: boolean read FIsCpuType;
    (*<summary>Returns true if the device is of GPU type. </summary>*)
    property IsGpuType: boolean read FIsGpuType;
    (*<summary>Returns true if the device is of Accelerator type. </summary>*)
    property IsAccType: boolean read FIsAccType;
    (*<summary>Returns true if the device is of the default type. </summary>*)
    property IsDefault: boolean read FIsDefault;
    (*<summary>Returns the number of parallel compute units on the OpenCL device. </summary>
                 
<remarks>A work-group executes on a single compute unit. The minimum value is 1.
</remarks>
*)
    property MaxComputeUnits: integer read FMaxComputeUnits;
    (*<summary>Maximum dimensions that specify the global and local work-item IDs used by the data parallel execution model. </summary>
                
<remarks>The minimum value is 3.
</remarks>
*)
    property MaxWorkItemDimensions: integer read FMaxWorkItemDimensions;
    (*<summary> Maximum number of work-items in a work-group executing a kernel on a single compute unit, using the data parallel execution model. </summary>*)
    property MaxWorkGroupSize: integer read FMaxWorkGroupSize;
    (*<summary> Maximum configured clock frequency of the device in MHz. </summary>*)
    property MaxClockFrequency: integer read FMaxClockFrequency; 
    (*<summary> The default compute device address space size specified as an unsigned integer value in bits. Currently supported values are 32 or 64 bits. </summary>*)
    property AddressBits: integer read FAddressBits;

    (*<summary> Max size of memory object allocation in KBytes. </summary>
                 
<remarks>The minimum value is max(1/4th of <see cref="GlobalMemSize"/> , 128*1024)
</remarks>
*)
    property MaxMemAllocSize: Int64 read FMaxMemAllocSize;   

    (*<summary> Size of global device memory in KBytes. </summary>*)
    property GlobalMemSize: Int64 read FGlobalMemSize;  

    (*<summary> Returns true, if global memory uses ECC. </summary>
                
<remarks>Is true if the device implements error correction for all accesses to compute device memory (global and
                constant). Is false if the device does not implement such error correction.
</remarks>
*)
    property ErrorCorrectionSupport: boolean read FErrorCorrectionSupport;
    (*<summary> Returns lmtLocal, if dedicated local memory storage is available. Returns lmtGlobal oterwhise. </summary>*)
    property LocalMemType: TLocalMemType read FLocalMemType;
    (*<summary> Returns the size of local memory (if present) in Bytes. </summary>
                 
<remarks>The minimum value is 32KBytes.
</remarks>
*)
    property LocalMemSize: Int64 read FLocalMemSize;
    (*<summary> Returns the size of constant memory in Bytes. </summary>
                 
<remarks>The minimum value is 64 KB.
</remarks>
*)
    property MaxConstantBufferSize: Int64 read FMaxConstantBufferSize;
    (*<summary> If true, the commands queued in the command-queue can be executed within the callers thread when the clEnqueue* is called. </summary>*)
    property QueueImmediateExecution: boolean read fQueueImmediateExecution;
    (*<summary> If true, the commands queued in the command-queue can be executed in-order or out-oforder. </summary>*)
    property QueueOutOfOrderExec: boolean read FQueueOutOfOrderExec;
    (*<summary> If true, command queues support profiling. </summary>*)
    property QueueProfiling: boolean read FQueueProfiling;
    (*<summary> Returns true, if immages are supported by the device. </summary>*)
    property ImageSupport: boolean read FImageSupport;
    (*<summary> Max number of simultaneous image objects that can be read by a kernel. </summary>
                 
<remarks>The minimum value is 128, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property MaxReadImageArgs: integer read FMaxReadImageArgs;
    (*<summary> Max number of simultaneous image objects that can be written by a kernel. </summary>
                 
<remarks>The minimum value is 8, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property MaxWriteImageArgs: integer read FMaxWriteImageArgs;
    (*<summary> Max width of 2D image in pixels. </summary>
                 
<remarks>The minimum value is 8192, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property Image2DMaxWidth: integer read FImage2DMaxWidth;
    (*<summary> Max height of 2D image in pixels. </summary>
                 
<remarks>The minimum value is 8192, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property Image2DMaxHeight: integer read FImage2DMaxHeight;
    (*<summary> Max width of 3D image in pixels. </summary>
                 
<remarks>The minimum value is 2048, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property Image3DMaxWidth: integer read FImage3DMaxWidth;
    (*<summary> Max height of 3D image in pixels. </summary>
                 
<remarks>The minimum value is 2048, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property Image3DMaxHeight: integer read FImage3DMaxHeight;
    (*<summary> Max depth of 3D image in pixels. </summary>
                 
<remarks>The minimum value is 2048, if <see cref="ImageSupport"/> is true.
</remarks>
*)
    property Image3DMaxDepth: integer read FImage3DMaxDepth;
    (*<summary> Holds a list of extension names supported by the device. </summary>
                 
<remarks>The list of extension names returned can be vendor supported extension names and
                 one or more of the following Khronos approved extension names:

                <code>
                cl_khr_fp64
                cl_khr_int64_base_atomics
                cl_khr_int64_extended_atomics
                cl_khr_fp16
                cl_khr_gl_sharing
                cl_khr_gl_event
                cl_khr_d3d10_sharing
                </code>

                The following approved Khronos extension names must be returned by all device that support OpenCL C 1.1:

                <code>
                cl_khr_global_int32_base_atomics
                cl_khr_global_int32_extended_atomics
                cl_khr_local_int32_base_atomics
                cl_khr_local_int32_extended_atomics
                cl_khr_byte_addressable_store
                </code>
</remarks>
*)
    property Extensions: TStringList read FExtensions;
    (*<summary> Holds NVidia specific device information. </summary>*)
    property NVInfo: TNVDeviceInfo read FNVInfo write SetNVInfo;
    (*<summary> Holds AMD specific device information. </summary>*)
    property AMDInfo: TAMDDeviceInfo read FAMDInfo write SetAMDInfo;
    (*<summary> Holds information about single precision floating point math. </summary>*)
    property FPSingleInfo: TFPInfo read FFPSingleInfo write SetFPSingleInfo;
    (*<summary> Returns true if 64bit double precision floating point math is supported on the device. </summary>
                 
<remarks>Contains result of checking for the corresponding extension.
</remarks>
*)
    property FP64: boolean read FFP64;
    (*<summary> Returns true if 16bit half precision floating point math is supported on the device. </summary>
                 
<remarks>Contains result of checking for the corresponding extension.
</remarks>
*)
    property FP16: boolean read FFP16;
    (*<summary> Holds information about double precision floating point math. </summary>*)
    property FPDoubleInfo: TFPInfo read FFPDoubleInfo write SetFPDoubleInfo;
    (*<summary> Holds information about half precision floating point math. </summary>*)
    property FPHalfInfo: TFPInfo read FFPHalfInfo write SetFPHalfInfo;
    (*<summary> Returns true for Open CL v1.1. </summary>*)
    property ByteAddressableStore: boolean read FByteAddressableStore;
    (*<summary> Creating CL context from a GL context or share group is supported, if this property returns true. </summary>*)
    property GLSharing: boolean read FGLSharing;
    (*<summary> If true then interoperability between OpenCL and Direct3D 10 is supported. </summary>*)
    property D3D10Sharing: boolean read FD3D10Sharing;
    (*<summary> Returns preffered vector width for Open CL char type. </summary>*)
    property PreferredVectorWidthChar: integer read FPreferredVectorWidthChar;
    (*<summary> Returns preffered vector width for Open CL short type. </summary>*)
    property PreferredVectorWidthShort: integer read FPreferredVectorWidthShort;
    (*<summary> Returns preffered vector width for Open CL int type. </summary>*)
    property PreferredVectorWidthInt: integer read FPreferredVectorWidthInt;
    (*<summary> Returns preffered vector width for Open CL long type. </summary>*)
    property PreferredVectorWidthLong: integer read FPreferredVectorWidthLong;
    (*<summary> Returns preffered vector width for Open CL float type. </summary>*)
    property PreferredVectorWidthFloat: integer read FPreferredVectorWidthFloat;
    (*<summary> Returns preffered vector width for Open CL double type. </summary>*)
    property PreferredVectorWidthDouble: integer read FPreferredVectorWidthDouble;
    (*<summary>If true, application will run optimal CPU code without Open CL when this device will be the target . </summary>
                
<remarks>Set this property to true to run native MtxVec code using Intel IPP and Intel MKL libraries
                instead of submitting kernels to the Open CL driver.
</remarks>
*)
    property NativeCPU: boolean read FNativeCPU write SetNativeCPU;


    constructor Create(aPlatform: TOpenCLPlatform; clDeviceID: cl_device_id); virtual;
    destructor Destroy; override;

  end;

  TOpenCLPlatform = class
  strict private
    FName: string;
    FVersion: string;
    FVendor: string;
    FProfile: string;
    fExtensions: TStringList;
    fContext: pcl_context;
    fBinaryPath: string;
    fPlatformID: cl_platform_id;
    aList: TObjectsList;
    procedure UpdateDevices;
    function GetDevice(i: integer): TOpenCLDevice;
    procedure SetDevice(i: integer; const Value: TOpenCLDevice);
    function GetCount: integer;
  protected
    procedure Update;
  public
    (*<summary>Array property gives access to individual devices available on the platform. </summary>*)
    property Device[i: integer]: TOpenCLDevice read GetDevice write SetDevice; default;
    (*<summary>Returns the number of devices available on the platform.</summary>*)
    property Count: integer read GetCount;
    (*<summary>Returns the name of the platform. </summary>
                
<remarks>You can use the following constants to compare against:
                <code>
                NVIDIA_PLATFORM
                AMD_PLATFORM
                INTEL_PLATFORM
                </code>
</remarks>
*)
    property Name: string read FName;
    (*<summary>Returns platform version</summary>*)
    property Version: string read fVersion;
    (*<summary>Returns vendor name</summary>*)
    property Vendor: string read fVendor;
    (*<summary>Returns platform ID as assigned to be the Open CL driver.  </summary>*)
    property PlatformID: cl_platform_id read fPlatformID;
    (*<summary>Returns the profile which can be either FULL_PROFILE or EMBEDDED_PROFILE.  </summary>*)
    property Profile: string read FProfile;
    (*<summary>Contains a list of Open CL extensions supported by all devices on the platform. </summary>*)
    property Extensions: TStringList read FExtensions;
    (*<summary>Returns the Open CL context shared by all devices on the platform. </summary>*)
    property Context: pcl_context read fContext;
    (*<summary>Returns the path to the Open CL binary. </summary>*)
    property BinaryPath: string read fBinaryPath;
    (*<summary>All queues on all devices of this platform will be disassociated with any previosuly used threads. </summary>*)
    procedure UnMarkThreads;
    
    constructor Create(platform_ID: cl_platform_id);
    destructor Destroy; override;
  
  end;

  (*<summary>Holds the list of all available platforms on the computer. </summary>
              
<remarks>Each platform requires a driver. When using graphics cards, Open CL driver
              is installed together with the cards driver. Open CL drivers for Intel CPU's and AMD
              CPU's need to be downloaded separately.
</remarks>
*)

  TOpenCLPlatformList = class
  strict private
    fIgnoreIntel: boolean;
    aList: TObjectsList;
    function GetPlatform(i: integer): TOpenCLPlatform;
    procedure SetPlatform(i: integer; const Value: TOpenCLPlatform);
    function GetCount: integer;
  public
    (*<summary>Set this property to true to avoid loading Intel Open CL code. </summary>
                
<remarks>Intel Open CL compilers excessively slow build procedure can cause minutes of delay during
                app startup when loading hundreds of kernels. This can speed up the method LoadProgramsForDevices
                for example.
</remarks>
*)
    property IgnoreIntel: boolean read fIgnoreIntel write fIgnoreIntel;
    (*<summary>Array property gives access to individual platforms available on the computer. </summary>*)
    property Platforms[i: integer]: TOpenCLPlatform read GetPlatform write SetPlatform; default;
    (*<summary>Returns the number of platforms available on the computer.</summary>*)
    property Count: integer read GetCount;
    (*<summary>Store a list of source files to the resource file and use ResID for identification. </summary>
                
<remarks>Once the files are stored in the .res file, you can embedd the resource in to your application by
                adding {$R ResFileName} to your source file. The source files are automatically encrypted.
</remarks>
*)
    procedure SaveSourceFileToRes(const ResFileName: string; ResID: Word; const SourceFiles: TStringList); overload;

    (*<summary>Store a source code to the resource file and use ResID for identification. </summary>
                
<remarks>Once the files are stored in the .res file, you can embedd the resource in to your application by
                adding {$R ResFileName} to your source file. The source code is automatically encrypted.
</remarks>
*)
    procedure SaveSourceCodeToRes(const ResFileName: string; ResID: Word; const SourceCode: TStringList); overload;

    (*<summary>Store a list of source files to the resource file and use ResName for identification. </summary>
                
<remarks>Once the files are stored in the .res file, you can embedd the resource in to your application by
                adding {$R ResFileName} to your source file. The source files are automatically encrypted.
</remarks>
*)
    procedure SaveSourceFileToRC(const ResFileName: string; const ResName: string; const SourceFiles: TStringList); overload;

    (*<summary>Store a source code to the resource file and use ResName for identification. </summary>
                
<remarks>Once the files are stored in the .res file, you can embedd the resource in to your application by
                adding {$R ResFileName} to your source file. The source code is automatically encrypted.
</remarks>
*)
    procedure SaveSourceCodeToRC(const ResFileName: string; const ResName: string; const SourceCode: TStringList); overload;

    (*<summary>Delete all and precompiled binaries. </summary>*)
    procedure ClearPrecompiledBinaries;
    (*<summary>All queues on all platforms and devices will be disassociated with any previosuly used threads. </summary>*)
    procedure UnMarkThreads;
    (*<summary> Recreates the resource files from the default Open CL sources shipped with the product. </summary>*)
    procedure SaveDefaultToRes(const SrcFilePath: string);
    (*<summary> Recreates the resource files from the default Open CL sources shipped with the product. </summary>*)
    procedure SaveDefaultToRC(const SrcFilePath: string);
    (*<summary> Loads kernels for all devices in both single and double precision. </summary>
                 
<remarks>The method will load all available kernels included with Cougar Open CL applicable to each device.
                 Both single and double precision with CPU versions for CPU devices and GPU versions for GPU
                 devices are supported. All kernels will be rebuilt unconditionally, if the Rebuild flag is true.
                 Kernels will be rebuilt in either case if driver or hardware change is detected.
</remarks>
*)
    procedure LoadProgramsForDevices(CPUSingle, CPUDouble, GPUSingle, GPUDouble: boolean; Rebuild: boolean = false);
    
    (*<summary> Returns the first found GPU device starting at first platform. </summary>
                
<remarks>If there is no GPU device found, the function returns nil.
</remarks>
*)
    function GetFirstGPUDevice: TOpenCLDevice;
    (*<summary>The function will measure time of running LoadFunction on all devices and will return the fastest device. </summary>
                
<remarks>The command queue on the fastest is marked automatically. For each device the function will be called
                twice and should therefore have a meaningfull computational load. The first call is for warming up
                the cache and waking up the device from low power mode. The second call is timed.
</remarks>
*)
    function AutoDeviceSelect(LoadFunction: TMtxNotifyEvent; CacheItemCount: integer; CacheItemLength: integer): TOpenCLDevice;
    (*<summary> Unloads the compiler(s), if this is supported by the driver(s). </summary>
                
<remarks>If needed in the future, the compilers will be reloaded again automatically.
</remarks>
*)
    procedure UnLoadCompiler;
    
    constructor Create; virtual;
    destructor Destroy; override;
  
  end;

  (*<summary>Provides method to load Open CL programs either from resource, source or files. </summary>
              
<remarks>The object also caches compiled binaries for faster load times. It makes sense to
              use at least two programs when developing applications. One can contain already tested
              (larger) code and the second one is shorter and rebuilt on every restart.
</remarks>
*)

  TOpenCLProgram = class
  strict private
    FBuildLog: TStringList;
    procedure SetBuildLog(const Value: TStringList); protected
      fCLProgram: pcl_program;
      
      fDevice: TOpenCLDevice;
      SourceFiles: TLocalAnsiStringArray;
      SourceFileLengths: TSize_tArray;
      procedure SaveProgramBinaries(const aBinaryFileName: string); overload;
  
      procedure CreateProgramWithBinaries(const aBinaryFileName: string); overload;
      procedure LoadCodeFromFile(const SourceFiles: TStringList; const SourceCode: TStringList); overload;
      procedure LoadCodeFromRes(const ResName: string; const SourceCode: TStringList); overload;
      procedure CreateProgramWithSourceCode(const SourceCode: TStrings; const SourceHeader: string);
      procedure BuildProgram(BuildOptions: string);
      function CleanEOL(const aStr: string): string;
  protected
      procedure CreateKernels;
  public
      property BuildLog: TStringList read FBuildLog write SetBuildLog;
      (*<summary> Returns the Device associated with the program. </summary>*)
      property Device: TOpenCLDevice read fDevice;
      (*<summary> Returns the Open CL program object. </summary>*)
      property clProgram: pcl_program read fCLProgram;
      (*<summary> Loads programs stored in SrcFileNames, adds SourceHeader, sets BuildOptions and compiles. </summary>
                   
<remarks>If the program to be build is already found, the binary version is loaded directly saving
                   valuable startup time for future runs. Set RebuildAlways to True to request unconditial recompile.
                   When a driver change or hardware change is detected the programs will be automatically rebuild
                   ignoring the value of RebuildAlways.
                   All Kernels in the Open CL source code are automatically added to the <see cref="TOpenCLDevice.Kernels"/> list.
</remarks>
*)
      procedure LoadProgram(const SrcFileNames: TStringList; const SourceHeader, BuildOptions: string; RebuildAlways: boolean = false); overload;
      (*<summary> Loads programs stored in the resource defined with ResName, adds SourceHeader, sets BuildOptions and compiles. </summary>
                   
<remarks>If the program to be build is already found, the binary version is loaded directly saving
                   valuable startup time for future runs. Set RebuildAlways to True to request unconditial recompile.
                   When a driver change or hardware change is detected the programs will be automatically rebuild
                   ignoring the value of RebuildAlways.
                   All Kernels in the Open CL source code are automatically added to the <see cref="TOpenCLDevice.Kernels"/> list.
</remarks>
*)
      procedure LoadProgram(const ResName: String; const SourceHeader, BuildOptions: string; RebuildAlways: boolean = false); overload;
      (*<summary> Loads programs stored in the SourceCode, adds SourceHeader, sets BuildOptions and compiles. </summary>
                   
<remarks>If the program to be build is already found, the binary version is loaded directly saving
                   valuable startup time for future runs. aBinFileName identifies the name of the binary file in which the compiler will
                   save the result. Set RebuildAlways to True to request unconditial recompile.
                   All Kernels in the Open CL source code are automatically added to the <see cref="TOpenCLDevice.Kernels"/> list.
</remarks>
*)
      procedure LoadProgram(const aBinFileName: string; const SourceCode: TStrings; const SourceHeader, BuildOptions: string; RebuildAlways: boolean = false); overload;
      (*<summary> Returns the path and name of the compiled binary when the source code comes from a file. </summary>*)
      function BinaryFileName(const SrcFileName: string): string; overload;
      (*<summary> Returns the path and name of the compiled binary when the source code comes from an embedded resource. </summary>*)
      function BinaryFileNameRes(const ResName: string): string; overload;
      
      constructor Create(const aDevice: TOpenCLDevice); overload; virtual;
      destructor Destroy; override;
    
  end;

  (*<summary> Stores a list of Open CL programs. </summary>*)
  TOpenCLProgramList = class
  strict private
    
    fDevice: TOpenCLDevice;
    aList: TObjectsList;
    function GetItem(i: integer): TOpenCLProgram;
    procedure SetItem(i: integer; const Value: TOpenCLProgram);
    function GetCount: integer;

  public
    (*<summary>Array property gives access to individual programs available on the device. </summary>*)
    property Item[i: integer]: TOpenCLProgram read GetItem write SetItem; default;
    (*<summary>Returns the number of programs already added.</summary>*)
    property Count: integer read GetCount;
    (*<summary>Call this method to add a new program.</summary>
                
<remarks>After the program object has been created use the <see cref="TOpenCLProgram.LoadProgram"/> to load
                the actual source code or precompiled binary.
</remarks>
*)
    function Add: TOpenCLProgram;
   (*<summary> Delete Open CL program from the list of programs. </summary>
                
<remarks>The related Kernels will also be removed from the <see cref="TOpenCLDevice.Kernels"/>
</remarks>
*)
    procedure Delete(ItemIdx: integer);
    
    constructor Create(const aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
  
  end;

  (*<summary> Open CL Kernels are added automatically to <see cref="TOpenCLDevice.Kernels"/> list when a  program is loaded. </summary>*)
  TOpenCLKernel = class
  strict private
    FKernel: cl_kernel;
    
    fDevice: TOpenCLDevice;
    
    fProgram: TOpenCLProgram;
    fName: string;
    FWorkGroupSize: integer;
    fLocalMemorySize: integer;
    fCompileWorkGroupSize: TSize_tArray;
    fSetArg: TBooleanArray;
    FArgCount: integer;
    procedure UpdateInfo;
    procedure SetArgCount(const Value: integer);
    procedure SetArgIndex(argIndex: integer);
  public
    (*<summary> Returns the work-group size specified in the source code. </summary>
                 
<remarks>In the source code we can specify the recommended work group size with the
                __attribute__((reqd_work_group_size(X, Y, Z))) qualifier.
</remarks>
*)
    function GetCompileWorkGroupSize(i: integer): integer;
    (*<summary>Returns the Program for which the kernel was created.</summary>*)
    property CLProgram: TOpenCLProgram read fProgram;
    (*<summary>Returns the Open CL kernel object.</summary>*)
    property Kernel: cl_kernel read FKernel;
    (*<summary>Returns the number of arguments of the Kernel (function parameter count).</summary>*)
    property ArgCount: integer read FArgCount write SetArgCount;
    (*<summary>Returns the Open CL kernel object.</summary>*)
    property Name: string read fName;
    (*<summary>Returns the maximum size of the Work Group for this Kernel.</summary>
                
<remarks>The OpenCL implementation uses the resource requirements of the kernel (register
                usage etc.) to determine what this workgroup size should be.
</remarks>
*)
    property WorkGroupSize: integer read FWorkGroupSize;
    (*<summary>Returns the amount of local memory in bytes being used by a kernel.</summary>
                
<remarks>This includes local memory that may be needed by an implementation to execute
                the kernel, variables declared inside the kernel with the __local address
                qualifier and local memory to be allocated for arguments to the kernel
                declared as pointers with the __local address qualifier and whose size is
                specified with clSetKernelArg.
</remarks>
*)
    property LocalMemorySize: integer read fLocalMemorySize;

    (*<summary>Updates the value of <see cref="LocalMemorySize"/> to reflect changes caused with calls to clSetKernelArg.</summary>*)
    procedure UpdateLocalMemoryInfo;
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgInt32(argIndex, Value: integer);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgBuffer(argIndex: integer; Value: pcl_mem);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgDouble(argIndex: integer; Value: double);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgSingle(argIndex: integer; Value: single);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgByte(argIndex: integer; Value: Byte);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgSmallInt(argIndex: integer; Value: SmallInt);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to Value.</summary>*)
    procedure SetArgSample(argIndex: integer; Value: double; Precision: TclFloatPrecision);
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to local memory size associated with the queue.</summary>
                
<remarks>The amount of local memory associated with the queue is specified by the TOpenCLcommandQueue property.
                The total amount of local memory allocated across all queues running on the same device
                may not exceed total available local memory as reported by TOpenCLDevice.LocalMemSize.
                The guaranteed minimum available memory for LocalMemSize is 32kBytes.
                The reported memory is in Bytes. The amount of local memory
                associated with the queue also may not be less than the minimum required by the kernel (if specified).
</remarks>
*)
    procedure SetArgLocalMem(argIndex: integer; const CmdQueue: TOpenCLCommandQueue); overload;
    (*<summary>Sets the Kernel (Open CL function) parameter at argIndex to local memory size associated with the queue.</summary>
                
<remarks>The memory size assigned is equal to SamplesLength*ElemSize.
</remarks>
*)
    procedure SetArgLocalMem(argIndex: integer; const CmdQueue: TOpenCLCommandQueue; SamplesLength: integer; aElemSize: integer); overload;

    (*<summary>Submits the Kernel to cmdQueue for computation with specified WorkSize.</summary>
                
<remarks>Setting CPUAdjust to true will reduce WorkSize by factor OPENCL_BLOCKLEN and assume presence of kernel internal for-loops.
                Kernel internal for-loops can significantly speed up execution of the kernel on CPU devices lowering the
                function call overhead. The CPUAdjust parameter is used only if the device is of CPU type.
                The for-loop pattern expected inside the kernel looks like this:
<code>
  size_t i = get_global_id(0);
	size_t tIdx = i*BLOCK_LEN;
	size_t tIdxLen = tIdx + BLOCK_LEN;
	if (tIdxLen &gt; Length) tIdxLen = Length;
	for (i = tIdx; i &lt; tIdxLen; i++)
	{

  }
</code>

  where BLOCK_LEN matches OPENCL_BLOCKLEN.
</remarks>
*)
    procedure Enqueue(const cmdQueue: TOpenCLCommandQueue; WorkSize: integer; CPUAdjust: boolean = true); overload;
    (*<summary>Submits the Kernel to cmdQueue for computation with specified WorkSize and LocalSize.</summary>
                
<remarks>When specified explicitely the (WorkSize mod LocalSize) is required to be zero.
                Setting CPUAdjust to true will reduce WorkSize by factor OPENCL_BLOCKLEN and assume presence of kernel internal for-loops.
                Kernel internal for-loops can significantly speed up execution of the kernel on CPU devices lowering the
                function call overhead. Kernel internal for-loops in GPU devices cause large performance penalties.
                The CPUAdjust parameter should be used only if the device is of CPU type. LocalSize is also called workgroup size.
                The for-loop pattern expected inside the kernel looks like this:

<code>
  size_t i = get_global_id(0);
	size_t tIdx = i*BLOCK_LEN;
	size_t tIdxLen = tIdx + BLOCK_LEN;
	if (tIdxLen &gt; Length) tIdxLen = Length;
	for (i = tIdx; i &lt; tIdxLen; i++)
	{

  }
</code>

  where BLOCK_LEN matches OPENCL_BLOCKLEN.
</remarks>
*)
    procedure Enqueue(const cmdQueue: TOpenCLCommandQueue; WorkSize, LocalSize: integer; CPUAdjust: boolean = true); overload;
    procedure Enqueue(const cmdQueue: TOpenCLCommandQueue; const WorkSizeOffsets, WorkSizes, LocalSizes: array of integer; CPUAdjust: boolean); overload;
    (*<summary>Resets the kernel parameter setting process.</summary>
                
<remarks>The TOpenCLKernel object tracks which parameters were already set and which not yet.
                An attempt to submitt the kernel for computation without having all its parameters
                set will raise an exception.
</remarks>
*)
    procedure ResetArgs;

    
    constructor Create(const KernelName: string; const aProgram: TOpenCLProgram); overload; virtual;
    constructor Create(aKernel: cl_kernel; const aProgram: TOpenCLProgram); overload; virtual;
    destructor Destroy; override;
  
  end;

  (*<summary> Stores a list of Open CL Kernels. </summary>*)
  TOpenCLKernelList = class
  strict private
    
    fDevice: TOpenCLDevice;
    aList: TStringList;
    function GetItem(i: integer): TOpenCLKernel;
    procedure SetItem(i: integer; const Value: TOpenCLKernel);
    function GetCount: integer;
  protected
    procedure Sort;
    procedure Add(aKernel: cl_kernel; const aProgram: TOpenCLProgram);
  public
    (*<summary>Array property gives access to Kernels programs available on the device. </summary>*)
    property Item[i: integer]: TOpenCLKernel read GetItem write SetItem; default;
    (*<summary>Returns the number of Kernels already added.</summary>*)
    property Count: integer read GetCount;
    (*<summary>Searches for and returns the kernel with KernelName.</summary>
                
<remarks>The function will raise an exception, if the Kernel is not found.
</remarks>
*)
    function Locate(const KernelName: string): TOpenCLKernel;
    (*<summary>Searches for and returns the index of the kernel with KernelName.</summary>
                
<remarks>Returns true, if the kernel with KernelName exists and the Idx parameter will contain
                the index in the list on the output.
</remarks>
*)
    function Find(const KernelName: string; var Idx: integer): boolean;
    (*<summary>Deletes and frees all kernels from the list.</summary>*)
    procedure Clear;
    (*<summary>Deletes and frees the Kernel at index specified from the list.</summary>*)
    procedure Delete(i: integer);
  
    constructor Create(const aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
  
  end;

  TJobThread = class;

  

  TEventThread = class(TThread)
  protected
     Trigger: integer;
     
     JobThread: TJobThread;
     procedure Execute; override;
  public
     destructor Destroy; override;
  end;

  

  TOnClExecuteEvent = procedure(const cmdQueue: TOpenCLCommandQueue; JobIndex: integer) of object;

  TJobThread = class
  strict private
    FOnExecute: TOnClExecuteEvent;
    FJobIndex: integer;
    Thread: TEventThread;
    
    fCommandQueue: TOpenCLCommandQueue;
    procedure SetJobIndex(const Value: integer);
    procedure SetOnExecute(const Value: TOnClExecuteEvent);
  protected
    fExceptionMessage: string;
  public
    (*<summary> Contains the exception message (if triggered) while the OnExecute was called. </summary>*)
    property ExceptionMessage: string read fExceptionMessage;
    (*<summary> Returns the Open CL command queue associated with the thread. </summary>*)
    property CommandQueue: TOpenCLCommandQueue read fCommandQueue;
    (*<summary> Specifies the JobIndex passed to the threaded function as a parameter. </summary>
                 
<remarks>Use this JobIndex to partition the jobs between mulitple devices.
</remarks>
*)
    property JobIndex: integer read FJobIndex write SetJobIndex;
    (*<summary> Threaded event function. </summary>
                 
<remarks>The event of specified type assigned to this property will be called each time the Start method is called.
                 While this event is no running the Sleeping function will be returning true.
                 Internally no thread will be created until the first assignment is made.
</remarks>
*)
    property OnExecute: TOnClExecuteEvent read FOnExecute write SetOnExecute;
    (*<summary> Call Start to launch the thread and call the function assigned to OnExecute event from within the thread. </summary>
                 
<remarks>The maximum start time is 1ms.
</remarks>
*)
    procedure Start;
    (*<summary> Sleeping will return true, if event assigned to OnExecute event is not running. </summary>*)
    function Sleeping: boolean;

    
    constructor Create(const cmdQueue: TOpenClCommandQueue); virtual;
    destructor Destroy; override;
    
  end;

  (*<summary> Every Open CL function that we want to compute needs to be submitted to the command queue. </summary>
               
<remarks>Device can have multiple command queues if supported.
</remarks>
*)
  TOpenCLCommandQueue = class
  strict private
    fJobThread: TJobThread;
    
    fDevice: TOpenCLDevice;
    fCommandQueue: pcl_command_queue;
    FImmediateExecution: boolean;
    FLocalMemorySize: integer;

    
    
    cs: TCriticalSection;
    
    

    procedure CreateCommandQue(aContext: pcl_context);
    procedure SetCommandQueueOutOfOrder(const Value: boolean);
    procedure SetCommandQueueProfiling(const Value: boolean);
    procedure SetImmediateExecution(const Value: boolean);
    procedure SetLocalMemorySize(const Value: integer);
  protected
    fQueueLength: integer;
  strict protected
    fThreadID: TThreadID;
    FCommandQueueProfiling: boolean;
    FCommandQueueOutOfOrder: boolean;
  public
    property QueueLength: integer read fQueueLength;
    property LocalMemorySize: integer read FLocalMemorySize write SetLocalMemorySize;
    property JobThread: TJobThread read fJobThread;
    property Device: TOpenCLDevice read FDevice;
    (*<summary> Returns the Open CL command queue object. </summary>*)
    property clCommandQueue: pcl_command_queue read fCommandQueue;
    (*<summary> Set this property to true, if you would like to have commands executed out of order. </summary>
                 
<remarks>Cosecutive kernels submitted to this queue will not finish in the order in which they
                 were started, if this property is set to true. The driver in this case allowed to optimize the order
                 of execution to achieve highest performance on the device. And exception will be raised
                 if this property is set to true and device does not support this feature.
                 See <see cref="TOpenCLDevice.QueueOutOfOrderExec"/> for more information.
</remarks>
*)
    property OutOfOrder: boolean read FCommandQueueOutOfOrder write SetCommandQueueOutOfOrder;
    (*<summary> If set to True, the Intel Open CL driver will execute all kernels within the callers thread. </summary>
                 
<remarks>This feature was introduced first with Intel Open CL v1.5. If True, the calls to clEnqueue*
                 will be blocking calls and the kernels will execute within the callers thread reducing the
                 call overhead by a large margin. The ImmediateExecution should typically be paired with OutOfOrder set to True
                 to allow multiple threads calling kernels without blocking each other.
</remarks>
*)
    property ImmediateExecution: boolean read FImmediateExecution write SetImmediateExecution;
    (*<summary> Set this property to true, to enable command queue profiling. </summary>*)
    property Profiling: boolean read FCommandQueueProfiling write SetCommandQueueProfiling;
    (*<summary> Calling this method will block the calling thread until all Kernels (Open CL functions) submitted for execution to this queue have finished. </summary>*)
    procedure Finish;
    (*<summary> Associates the thread from which this method was called with the command queue. </summary>*)
    procedure MarkThread;
    (*<summary> Disssociates the command queue from any threads. </summary>*)
    procedure UnmarkThread;
    (*<summary> Returns true if, the command queue is associated with the thread with ThreadID. </summary>*)
    function IsMarkedThread(ThreadID: TThreadID): boolean;
    (*<summary> Users critical section Enter point. </summary>
                 
<remarks>Provides access to ready to use critical section to control access to this object.
</remarks>
*)
    procedure Enter;
    (*<summary> Users critical section TryEnter point. </summary>
                 
<remarks>Provides access to ready to use critical section to control access to this object.
</remarks>
*)
    function TryEnter: boolean;
    (*<summary> Users critical section Leave point. </summary>
                 
<remarks>Provides access to ready to use critical section to control access to this object.
</remarks>
*)
    procedure Leave;
    
    constructor Create(const aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
    
  end;

  (*<summary> Stores a list of Open CL queues. </summary>*)
  TOpenCLCommandQueueList = class
  strict private
    
    fDevice: TOpenCLDevice;
    aList: TObjectsList;
    function GetItem(i: integer): TOpenCLCommandQueue;
    procedure SetItem(i: integer; const Value: TOpenCLCommandQueue);
    function GetCount: integer;
  public
    (*<summary>Array property gives access to command queues available for the device. </summary>*)
    property Item[i: integer]: TOpenCLCommandQueue read GetItem write SetItem; default;
    (*<summary>Returns the number of command queues already added.</summary>*)
    property Count: integer read GetCount;
    (*<summary>Add a new command queue.</summary>*)
    function Add: TOpenCLCommandQueue;
    (*<summary>Deletes and frees the command queue at index specified from the list.</summary>*)
    procedure Delete(ItemIdx: integer);
    (*<summary>Unmarks associated threads from all command queues.</summary>*)
    procedure UnMarkThreads;
    (*<summary>Searches for and returns the command queue which is associated with thread which has identified with aThreadID.</summary>
                
<remarks>The function will raise an exception, if the queue cannot be found.
</remarks>
*)
    function Locate(aThreadID: TThreadID): TOpenCLCommandQueue;
  
    constructor Create(const aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
  
  end;

  TOpenCLMtxVec = class;

  TOpenCLBase = class
  private
    IsUsed: boolean;
    BlockId: integer;
    
  strict protected
    fLength: integer;
    FCaption: string;
    FTag: PointerInteger;
    
    FDevice: TOpenCLDevice;

    fAllowSubRange: boolean;
    SLength: integer;
    SComplex: boolean;
    SBlockID: integer;
    fDebuggerMarker: integer;

    FFloatPrecision: TCLFloatPrecision;
    
    procedure SetCaption(const Value: string);
    procedure SetTag(const Value: PointerInteger);
    procedure FreeToCache; virtual;
    function ElemSize: Int64; virtual;
    function GetCaption: string;
    function GetLength: integer;
    function GetTag: PointerInteger;
    function GetCommandQueue: TOpenCLCommandQueue;

    procedure SetFloatPrecision(const Value: TCLFloatPrecision);
    function PrecisionKernel(const KernelName: string): string;
    function GetComplex: boolean;
    function GetFloatPrecision: TCLFloatPrecision;
    function Copy(const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;

    function Mul(const Vec: TOpenCLBase; const Value: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Mul(const Vec: TOpenCLBase; const Value: TCplx; VecIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Mul(const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Mul(const Vec1, Vec2: TOpenCLBase; Vec1Index, Vec2Index, Index,  Len: integer): TOpenCLBase; overload;

    function Add(const Vec: TOpenCLBase; const Value: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Add(const Vec: TOpenCLBase; const Value: TCplx; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Abs(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Add(const Vec1, Vec2: TOpenCLBase; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLBase; overload;
    function MulI(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ConjMul(const Vec1, Vec2: TOpenCLBase; Vec1Index, Vec2Index, Index,  Len: integer): TOpenCLBase; overload;
    function Divide(const Num, Den: TOpenCLBase; NumIndex, DenIndex, Index, Len: integer): TOpenCLBase; overload;
    function DivideBy(const Value: double; const Vec: TOpenCLBase; VecIndex, Index,  Len: integer): TOpenCLBase; overload;
    function DivideBy(const Value: TCplx; const Vec: TOpenCLBase; VecIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Sin(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Cos(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Tan(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Cot(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sec(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Csc(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcSin(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCos(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcTan2(const Y, X: TOpenCLBase; YIndex, XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcTan(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCot(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcSec(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCsc(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sinh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Cosh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Tanh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Coth(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sech(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Csch(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcSinh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCosh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcTanh(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCoth(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcSech(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ArcCsch(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    procedure CplxToReal(const ReVec, ImVec: TOpenCLBase; ReIndex, ImIndex, Index, Len: integer); overload;
    function RealToCplx(const ReVec, ImVec: TOpenCLBase; ReIndex, ImIndex, Index, Len: integer): TOpenCLBase; overload;
    function PolarToCart(const AmpltVec, PhaseVec: TOpenCLBase; aIndex, PIndex, Index, Len: integer): TOpenCLBase; overload;
    procedure CartToPolar(const AmpltVec, PhaseVec: TOpenCLBase; AmpltIndex, PhaseIndex, Index, Len: integer); overload;
    function RealPart(const Vec: TOpenCLBase; VecIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Exp(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Exp10(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Exp2(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function ImagPart(const Vec: TOpenCLBase; VecIndex, Index,   Len: integer): TOpenCLBase; overload;
    function IntPower(const aBase: TOpenCLBase; Exponent: Integer): TOpenCLBase; overload;
    function Inv(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function InvCbrt(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Ln(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Log10(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Log2(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Cbrt(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Ceil(const Src: TOpenCLBase; SrcIndex, Index, Len: integer): TOpenCLBase; overload;
    function InvSqrt(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sqr(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sqrt(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sub(const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Sub(const Vec1, Vec2: TOpenCLBase; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLBase; overload;
    function SubFrom(const Value: double; const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function SubFrom(const Value: TCplx; const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function Conj(const Vec: TOpenCLBase; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function FixAngle(const Src: TOpenCLBase; SrcIndex, Index, Len: integer): TOpenCLBase; overload;
    function Floor(const Src: TOpenCLBase; SrcIndex, Index, Len: integer): TOpenCLBase; overload;
    function Expj(const Omega: TOpenCLBase; OmegaIndex, Index, Len: integer): TOpenCLBase; overload;
    function Frac(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Erf(const Src: TOpenCLBase; SrcIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Erfc(const Src: TOpenCLBase; SrcIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Flip(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function FlipConj(const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Rem(const X, Y: TOpenCLBase; XIndex, YIndex, Index, Len: integer): TOpenCLBase; overload;
    function Rem(const X: TOpenCLBase; const Y: double; XIndex, Index,  Len: integer): TOpenCLBase; overload;
    function LogN(const N: double; const X: TOpenCLBase; XIndex, Index, Len: integer): TOpenCLBase; overload;
    function Power(const Base: double; const Exponent: TOpenCLBase): TOpenCLBase; overload;
    function Power(const Base: TCplx; const Exponent: TOpenCLBase): TOpenCLBase; overload;
    function Power(const Base: TOpenCLBase; const Exponent: double): TOpenCLBase; overload;
    function Power(const Base: TOpenCLBase; const Exponent: TCplx): TOpenCLBase; overload;
    function PowerVec(const Base, Exponent: TOpenCLBase): TOpenCLBase; overload;
    function Round(const Src: TOpenCLBase; SrcIndex, Index,  Len: integer): TOpenCLBase; overload;
    function Trunc(const Src: TOpenCLBase; SrcIndex, Index,  Len: integer): TOpenCLBase; overload;
    function SetVal(const Value: TCplx; Index, Len: integer): TOpenCLBase; overload;
    function SetVal(const Value: double; Index, Len: integer): TOpenCLBase; overload;
    function ExtendToComplex(const Src: TOpenCLBase; Zeros: Boolean; SrcIndex,  DstIndex, Len: integer): TOpenCLBase; overload;
    function Sgn(const Src: TOpenCLBase; SrcIndex, Index,  Len: integer): TOpenCLBase; overload;
    procedure SinCos(const SinX, CosX: TOpenCLBase; SinXIndex, CosXIndex, Index, Len: integer); overload;
    procedure SinhCosh(const SinhX, CoshX: TOpenCLBase; SinhIndex, CoshIndex,  Index, Len: integer); overload;
    function ThreshBottom(const Vec: TOpenCLBase; const Value: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function ThreshTop(const Vec: TOpenCLBase; const Value: double; VecIndex, Index,  Len: integer): TOpenCLBase; overload;
    function ThresholdLT(const Vec: TOpenCLBase; const LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function ThresholdLT(const Vec: TOpenCLBase; const LTLevel: double; const LTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function ThresholdGT(const Vec: TOpenCLBase; const GTLevel, GTValue: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function ThresholdGT(const Vec: TOpenCLBase; const GTLevel: double; const GTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    function ThresholdGT_LT(const Vec: TOpenCLBase; const GTLevel, GTValue, LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLBase; overload;
    procedure TruncAndFrac(const TruncDst, FracDst: TOpenCLBase; TruncIdx, FracIdx, Index, Len: integer); overload;
    function SgnMul(const Src: TOpenCLBase): TOpenCLBase; overload;
  public

    procedure CopyToArray(var Dst: TCplxArray; Index, Len: integer); overload;
    procedure CopyToArray(var Dst: TSCplxArray; Index, Len: integer); overload;

    procedure CopyToArray(var Dst: TDoubleArray; Index, Len: integer); overload;
    procedure CopyToArray(var Dst: TSingleArray; Index, Len: integer); overload;

    (*<summary>Copies array content. </summary>
               
<remarks>Copies Len elements from the Src array and stores them in the calling object starting at location Index.
               The value of the Complex property and size of the calling objects are not modified.
</remarks>
*)
    procedure CopyFromArray(const Src: TDoubleArray; Index, Len: integer); overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Copies Len elements from the Src array and stores them in the calling object starting at location Index.
               The value of the Complex property and size of the calling objects are not modified.
</remarks>
*)
    procedure CopyFromArray(const Src: TCplxArray; Index, Len: integer); overload;

    procedure SizeToArray(var Dst: TSCplxArray; Len: integer); overload;

    (*<summary>Sets the size of the array to match the size of the object. </summary>
               
<remarks>SizeToArray considers both Complex and Length properties when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TSingleArray); overload;

    (*<summary>Sets the size of the array to match the size of the object. </summary>
               
<remarks>SizeToArray considers both Complex and Length properties when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TDoubleArray); overload;

    (*<summary>Sets the size of the array to match the size of the object. </summary>
               
<remarks>SizeToArray considers both Complex and Length properties when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TCplxArray); overload;

    (*<summary>Sets the size of the array to match the size of the object. </summary>
               
<remarks>SizeToArray considers both Complex and Length properties when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TIntegerArray); overload;

    (*<summary>Sets the size of the array considering the value of the Complex property. </summary>
               
<remarks>SizeToArray considers the value of the Complex property and of the Len parameter when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TIntegerArray; Len: integer); overload;

    (*<summary>Sets the size of the array considering the value of the Complex property. </summary>
               
<remarks>SizeToArray considers the value of the Complex property and of the Len parameter when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TSingleArray; Len: integer); overload;
    (*<summary>Sets the size of the array considering the value of the Complex property. </summary>
               
<remarks>SizeToArray considers the value of the Complex property and of the Len parameter when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TDoubleArray; Len: integer); overload;
    (*<summary>Sets the size of the array considering the value of the Complex property. </summary>
               
<remarks>SizeToArray considers the value of the Complex property and of the Len parameter when setting the size of the array.
</remarks>
*)
    procedure SizeToArray(var Dst: TCplxArray; Len: integer); overload;

  strict protected
    fIsSubRange: boolean;
    fComplex: boolean;
    fMemBuffer: pcl_mem;
    

    procedure SetLength(const Value: integer); overload; virtual;
    procedure DebugCheck;
    procedure InitData; virtual;

    function GetRealValues: string; virtual;
    function GetComplexValues: string; virtual;

    procedure ValidateParams2(const X: TOpenCLBase; const XIndex, Index: integer;  var Len: integer); overload;
    procedure ValidateParams(const X1, X2: TOpenCLBase; const xIndex1, xIndex2,  Index: integer; var Len: integer); overload;
    procedure ValidateParams(const X: TOpenCLBase; const XIndex, Index: integer;  var Len: integer); overload;
    procedure ValidateParamsPR(const X1, X2: TOpenCLBase; const xIndex1, xIndex2,  Index: integer; var Len: integer); overload;

    procedure EnqueueKernel(const KernelName: string; Value: double; Index,  Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Value: TCplx; Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; Value: double; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; const Value: TCplx; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; Value, VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec1, Vec2: TOpenCLBase; Vec1Index, Vec2Index, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; Value, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; Value1, Value2: double; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; Value1, Value2: double; Index, Len: integer); overload;
    procedure EnqueueUpSampleKernel(const KernelName: string; const Vec: TOpenCLBase; VecIndex, Index, Len: integer; Factor, Phase: integer); overload;
    procedure SetArg(const aKernel: TOpenCLKernel; argIndex: integer; const Vec: TOpenCLBase; VecIndex: integer); overload;
    procedure MemRealloc(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); overload;
    procedure EnqueueKernel(const KernelName: string; Value1: double; const Value2: TCplx; Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase;  Value1: double; const Value2: TCplx; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; Value1, Value2, Value3, Value4: double; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; Value1, Value2, Value3, Value4: double; Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; const Value1, Value2: TCplx; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Vec: TOpenCLBase; const Value1: TCplx; Value2: double; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; Value: double; const Vec: TOpenCLBase; VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Value: TCplx; const Vec: TOpenCLBase;  VecIndex, Index, Len: integer); overload;
    procedure EnqueueKernel(const KernelName: string; const Value1, Value2: TCplx; Index, Len: integer); overload;

    
    procedure InternalCopyTo(Dst: TDenseMtxVec);overload; virtual;
    procedure InternalCopyTo(Dst: TMtxVecInt);overload; virtual;
    

    function GetSData: PCL_mem; virtual;
    procedure SetComplex(const Value: boolean);
    procedure DoSetComplex(Value: boolean); virtual;

  protected
    
    fData: TDenseMtxVec;
    
  protected
    fCacheIndex: integer;
    FDataOffset: integer;
    function DebugUpdate: integer; virtual;
  public

    procedure Size(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); overload; virtual;

    procedure Reset; virtual;

    

    property RealValues: string read GetRealValues;
    property ComplexValues: string read GetComplexValues;

    

    

    function LocateCommandQueue: TOpenCLCommandQueue;
    (*<summary>Set this property to indicate that object data values are to be treated complex.</summary>
               
<remarks>Read this property to determine, if the object stores real or complex data.
</remarks>
*)
    property Complex: boolean read GetComplex write SetComplex;
    (*<summary>Returns the queue on the Device on which we will run functions using this object. </summary>*)
    property CmdQueue: TOpenCLCommandQueue read GetCommandQueue;
    (*<summary>Returns the device on which we allocated memory for this object. </summary>*)
    property Device: TOpenCLDevice read FDevice;
    (*<summary>Allows storing optional information. </summary>*)
    property Tag: PointerInteger read GetTag write SetTag;
    (*<summary>Stores optional label. </summary>*)
    property Caption: string read GetCaption write SetCaption;
    (*<summary> Defines the length in number of elements (single or double precision) of the memory allocated. </summary>*)
    property Length: integer read GetLength write SetLength;
    (*<summary>The calling thread will block until the command queue using this object has finished. </summary>*)
    procedure Finish;
    (*<summary>Sets the calling object to match the size of the Src object. </summary>*)
    procedure Size(const Src: TOpenCLBase); overload; virtual;
    (*<summary>Sets the calling object to match the size of the Src object, and changes Complex to aComplex if needed. </summary>*)
    function Size(const Src: TOpenCLBase; aComplex: boolean): TOpenCLBase; overload; virtual;

    property FloatPrecision: TCLFloatPrecision read GetFloatPrecision write SetFloatPrecision;

    (*<summary>Returns the index offest in to the buffer.</summary>
               
<remarks>Returns the index offset in to the buffer at which real values for the this object start.
</remarks>
*)
    function SDataIndex(aIndex: integer): PointerInteger; virtual;

    (*<summary>Returns the index offest in to the buffer.</summary>
               
<remarks>Returns the index offset in to the buffer at which complex values for the this object start.
</remarks>
*)
    function CDataIndex(aIndex: integer): PointerInteger; virtual;

    
    procedure CopyTo(Dst: TVec); overload;
    procedure CopyTo(Dst: TVecInt); overload;
    

  (*<summary>Returns Open CL buffer reference.</summary>
               
<remarks>Returns the real Open CL buffer used by the object. Multiple objects may be
               using the same buffer.
</remarks>
*)
    property SData: PCL_mem read GetSData;
    (*<summary>Returns Open CL buffer reference.</summary>
               
<remarks>Returns complex Open CL buffer used by the object. Multiple objects may be
               using the same buffer.
</remarks>
*)
    property CData: PCL_mem read GetSData;

    
    
    procedure ValidateParams(const Index: integer; var Len: integer); overload;
    constructor Create(aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
    
  end;

  TOpenCLMtxVec = class(TOpenCLBase)
  strict protected
    function GetNumGroups(Len: integer): integer;

    procedure ValidateRealIndexLen(const Index, RealSrcLen: integer); overload;
    procedure ValidateCplxArrayIndexes(const SrcIndex, SrcLen, Index: integer;  var Len: integer); overload;
    procedure ValidateRealArrayIndexes(const SrcIndex, SrcLen, Index: integer; var Len: integer); overload;

    procedure SetLength(const Value: integer); overload; override;

    procedure InternalEnqueuePRKernel(const aKernel: TOpenCLKernel; const aCmdQueue: TOpenCLCommandQueue; GroupBufferLength: integer); overload;
    procedure EnqueuePRKernel(const KernelName: string; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel(const KernelName: string; const Vec: TOpenCLMtxVec; ConjVec: boolean; VecIndex, Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel2(const KernelName: string; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel2(const KernelName: string; Index, Len: integer; const GroupBuffer0, GroupBuffer1: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel2(const KernelName: string; Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel3(const KernelName: string; Index, Len: integer; const GroupBuffer0, GroupBuffer1: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel3(const KernelName: string; Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel(const KernelName: string; Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel(const KernelName: string; Index, Len: integer; const GroupBuffer0, GroupBuffer1: TOpenCLMtxVec); overload;
    procedure EnqueuePRKernel(const KernelName: string; const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload;    
    procedure EnqueuePRKernel(const KernelName: string; const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Len: integer; const GroupBuffer: TOpenCLMtxVec); overload; 
    procedure EnqueueReverseKernel(KernelName: string; Index, Len: integer); overload;

    procedure EnqueueGroupKernel(const KernelName: string); overload;

  public
    constructor Create(aDevice: TOpenCLDevice); override;

    procedure Size(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); overload; override;

    (*<summary>Automatically set to true after the SetSubIndex or SetSubRange call.</summary>
      
<remarks>This property is set to true after the <see cref="SetSubIndex"/> or <see cref="SetSubRange"/> call.
      If IsSubRange is true then the TOpenCLMtxVec method/function will be performed on subrange of values. Use
      <see cref="SetFullRange"/> to set IsSubRange back to False and thus reset sub range to full vector length.
</remarks>


      <SeeAlso cref="SetFullRange"/>*)
    property IsSubRange: boolean read fIsSubRange;

    (*<summary>Resets any defined subrange.</summary>
      <SeeAlso cref="SetSubRange"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SetFullRange; virtual;

    (*<summary>Defines a sub vector/matrix.</summary>
      
<remarks>The method will define a subarray starting at Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted or indices adjusted.

      All values of the original <see cref="TMtxVecBase"/> will be preserved.
      An exception will be raised if an attempt is made to change the size of calling object.

      A sub-vector/matrix is vector/matrix which does not neccessarily have its own
      memory allocated. Instead it adopts the memory of the source object and all operations done on the
      either of the objects affect the same elements. The use of subvectors/submatrices increases
      CPU cache reuse, lower's memory requirements, increases application performance and improves code readability.

      To again obtain a view of the full vector/matrix, see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubRange(Index: integer; Len: integer); overload; virtual;


    (*<summary>Prevents calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Prevents calls to <see cref="SetSubRange"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
</remarks>


      

    <example>
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples()
    {

      void Example()
      {
        TOpenCLVector a,b;

        clMtxVec.CreateIt(out a, out b);
        try
        {
          a.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9});
          a.SetSubRange(0,2);  //a = [1,2]
          a.DisableSubrange();
          a.SetSubRange(2,2); //exception raised here

          b.SetSubRange(a,2,2); //but this will work
          a.SetFullRange(); //b is not changed, it still points to [4,5]
        }
        finally
        {
          MtxVec.FreeIt(ref a, ref b);
        }
      }
    }
    </code></example>

    <SeeAlso cref="DisableSubrange"/>
    <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSubrange;

    (*<summary>Enables calls to <see cref="SetSubRange"/>.</summary>
      
<remarks>Enables calls to <see cref="SetSubRange"/> by removing
      the block set by <see cref="DisableSubrange"/> .
</remarks>
*)
    procedure EnableSubrange;

    (*<summary>Prevents calls to <see cref="Select"/>.</summary>
      
<remarks>Prevents calls to <see cref="Select"/> method. This can be usefull
      guard when an object is already working on a subrange and the user
      would like to further subrange an already subranged object.
</remarks>


      

    <example>
    <code>
    using Dew.Math;
    using Dew.Math.Units;

    namespace Dew.Examples()
    {

      void Example()
      {
        TOpenCLVector a,b;

        MtxVec.CreateIt(out a, out b);
        try
        {
          a.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9});
          a.Select(0,2);  //a = [1,2]
          a.DisableSelect();
          a.Select(2,2); //exception raised here

          b.Select(a,2,2); //but this will work
          a.SelectAll(); //b is not changed, it still points to [4,5]
        }
        finally
        {
          MtxVec.FreeIt(ref a, ref b);
        }
      }
    }
    </code></example>

      <SeeAlso cref="DisableSubrange"/>
      <SeeAlso cref="EnableSubrange"/>*)
    procedure DisableSelect;

    (*<summary>Enables calls to <see cref="Select"/>.</summary>
      
<remarks>Enables calls to <see cref="Select"/> by removing
      the block set by <see cref="DisableSelect"/>.
</remarks>
*)
    procedure EnableSelect;

    (*<summary>Resets any defined selection.</summary>
      <SeeAlso cref="Select"/>
      <SeeAlso cref="SetSubIndex"/>*)
    procedure SelectAll; overload;

    (*<summary>Selects a set of elements from the vector.</summary>
      
<remarks>The method will define a subarray starting at
      Index and ending at Index+Len-1. No copying will occur, only
      pointers will be shifted.

      All values of the original <see cref="TMtxVec"/> will be preserved.
      An exception will be raised if an attempt is made to
      change the size of the calling object after a selection has been made.

      A selection will behave exactly as if the object always contained
      only the selected values. When a selection is made from another object
      the source object and all operations done on either of the
      two objects will affect the same elements. The use of selections increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      To again obtain a view of the full vector/matrix use the <see cref="SelectAll"/> method.

      When selecting elements from other objects, care must be given not to
      use the selections after the source object has been freed:

      <code>
      var a,b: TOpenCLVector;
      begin
          CreateIt(a,b);
          try
              a.SetIt(False,[1,2,3,4]);
              b.Select(a,2,2);

              FreeIt(a); //free the source object

              b.Values[0] := b.Valuess[1]; //b does not point to valid memory anymore
          finally
              FreeIt(a);
          end;
      end;
      </code>
</remarks>
*)
    procedure Select(Index: integer; Len: integer); overload;

   (*<summary>Resets the selection.</summary>
    
<remarks>This method is the same as the <see cref="SelectAll"/> method.
</remarks>
*)
    procedure Select; overload;

    (*<summary>Sets the subarray size to full size.</summary>
      
<remarks>This method is the same as the <see cref="SetFullRange"/> method.
</remarks>
*)
    procedure SetSubRange; overload;

    (*<summary>Defines a subarray.</summary>
      
<remarks>The method will define a sub array starting at
      BeginIndex and ending at EndIndex (inclusive).
      All values of the original vector/matrix will be preserved.
      An exception will be raised if an attempt is made to
      change the size of calling vector/matrix.

      A subarray is array which does not have its own
      memory allocated. Instead it adopts the memory of the
      source object and all operations done on the either of the objects
      affect the same elements. The use of subarrays increases
      CPU cache reuse, lower's memory requirements, increases
      application performance and improves code readability.

      To again obtain a view of the full vector see <see cref="SetFullRange"/>
</remarks>
*)
    procedure SetSubIndex(BeginIndex, EndIndex: integer); overload;

    (*<summary>Copies array content. </summary>
               
<remarks>Copies Len elements from the Src array and stores them in the calling object starting at location Index.
               The value of the Complex property and size of the calling objects are not modified.
</remarks>
*)
    procedure CopyFromArray(const Src: TSingleArray; Index, Len: integer); overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Sets the size of the object to match the size of the Src array and copies the contents in to the calling object.
</remarks>
*)
    procedure CopyFromArray(const Src: TSingleArray); overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Sets the size of the object to match the size of the Src array and copies the contents in to the calling object.
</remarks>
*)
    procedure CopyFromArray(const Src: TDoubleArray); overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Sets the size of the object to match the size of the Src array and copies the contents in to the calling object.
</remarks>
*)
    procedure CopyFromArray(const Src: TCplxArray); overload;

    (*<summary>Copies array content. </summary>
           
<remarks>Copies Len complex elements from the Src array and stores them in the complex calling object starting at location Index.
           The value of the Complex property and size of the calling objects are not modified.
</remarks>
*)
    function CopyCplxFromArray(const Src: TSingleArray; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Sets the size of the object to match the size of the Src array and copies the contents in to the calling object.
               The complex property is set to True.
</remarks>
*)
    function CopyCplxFromArray(const Src: TSingleArray): TOpenCLMtxVec; overload;
    (*<summary>Copies array content. </summary>
           
<remarks>Copies Len complex elements from the Src array and stores them in the complex calling object starting at location Index.
           The value of the Complex property and size of the calling objects are not modified.
</remarks>
*)
    function CopyCplxFromArray(const Src: TDoubleArray; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Copies array content. </summary>
               
<remarks>Sets the size of the object to match the size of the Src array and copies the contents in to the calling object.
               The complex property is set to True.
</remarks>
*)
    function CopyCplxFromArray(const Src: TDoubleArray): TOpenCLMtxVec; overload;





    (*<summary>Copies calling object data to array. </summary>
               
<remarks>Copies Length elements from the calling object to Dst array.
               The size of the Dst array is set to Length or Length*2, if the Complex is true.
</remarks>
*)
    procedure CopyToArray(var Dst: TSingleArray); overload;




    (*<summary>Copies calling object data to array. </summary>
               
<remarks>Copies Length elements from the calling object to Dst array.
               The size of the Dst array is set to Length or Length*2, if the Complex is true.
</remarks>
*)
    procedure CopyToArray(var Dst: TDoubleArray); overload;








    (*<summary>Copies calling object data to array. </summary>
               
<remarks>Copies Length elements from the calling object to Dst array.
               The size of the Dst array is set to Length or Length/2, if the Complex is false.
</remarks>
*)
    procedure CopyToArray(var Dst: TCplxArray); overload;

    (*<summary>Copies calling object data to array. </summary>
               
<remarks>Copies Length elements from the calling object to Dst array.
               The size of the Dst array is set to Length or Length*2, if the Complex is true.
</remarks>
*)
    procedure CopyToArray(var Dst: TIntegerArray); overload;
    (*<summary>Copies calling object data to array. </summary>
               
<remarks>Copies Len elements from the calling object starting at Index to Dst array.
               The size of the Dst array is set to Len or Len*2, if the Complex is true.
</remarks>
*)
    procedure CopyToArray(var Dst: TIntegerArray; Index, Len: integer); overload;

    
    procedure Copy(const Src: TDenseMtxVec); overload; virtual;
    procedure Copy(const Src: TMtxVecInt); overload; virtual;
    

    (*<summary>Copy object values.</summary>
      
<remarks>Copy each of Vec elements to the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are set implicitly to match Vec object.
</remarks>


      <Example>
      <code>
      var a,b: TOpenCLVector;
      begin
          a.CopyCplxFromArray(TSingleArray.Create(1,2,3,4));  // a = [1+2i,3+4i]
          b.Copy(a);                // b = [1,2,3,4]
      end;
      </code>
      </Example>*)

    function Copy(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Copy Vec elements [VecIndex]..[VecIndex+Len-1] in the calling object
       elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties must be set explicitly. An exception is raised if
       array borders are overrun or underrun.
</remarks>
*)
    function Copy(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec2 elements to corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object are set implicitly to match Vec1 and Vec2 vectors.
</remarks>
*)
    function Add(const Src1, Src2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec1 elements [Vec1Index]..[Vec1Index+Len-1] to Vec2 elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is
      raised array borders are overrun.
</remarks>
*)
    function Add(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Adds Value to object elements.</summary>

      <SeeAlso cref="Sub"/>*)
    function Add(const Value: double): TOpenCLMtxVec; overload;

    (*<summary>Adds complex Value to all calling object complex elements.</summary>*)
    function Add(const Value: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Add(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Adds Value to the each element of the Vec object.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are set automatically.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each element of the Vec object.</summary>
      
<remarks>Store the result to the calling object. Size property of the calling object is set
      automatically. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Value to each element of Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set implicitly.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds complex Value to each elements of the Vec object in range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result to elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception is raised if array borders are overrun.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array addition.</summary>
      
<remarks>Add each of Vec elements to corresponding elements in the calling object.
</remarks>

      <SeeAlso cref="Sub"/>*)
    function Add(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Add Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised array borders are overrun.
</remarks>
*)
    function Add(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Split complex calling object in real and imaginary part.</summary>
      
<remarks>Split calling object into real and imaginary components. Store all real components in ReVec and
      all imaginary components in ImVec. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of ReVec and ImVec
      are set implicitly to match with the calling vector. An execption is raised if calling object is not complex.
</remarks>


      <Example>
      <code>
      var a,b,c: clVector;
      begin
          a.CopyCplxFromArray(TSingleArray.Create(1,-2,3,4));    // a= [1-2i, 3+4i]
          a.CplxToReal(b,c);  // b = [1, 3], c = [-2, 4]
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(const ReVec, ImVec: TOpenCLMtxVec); overload;
    (*<summary>Split calling object elements [Index]..[Index+Len-1] into real and imaginary components.</summary>
      
<remarks>Store real components in ReVec elements [ReIndex]..[ReIndex+Len-1] and imaginary components in ImVec elements
      [ImIndex]..[ImIndex+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure CplxToReal(const ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer); overload;

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="TOpenCLBase.Complex">Complex</see> property is True.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.CopyCplxFromArray( new double[] {1,2, 3,4}); // 1+2i ; 3+4i
            b.CopyCplxFromArray( new double[] {2,2,3,4}); // 2+2i ; 3+4i
            c.RealToCplx(a,b);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(const ReVec, ImVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Construct a complex object from the ReVec elements [ReIndex]..[ReIndex+Len-1] (real part) and the ImVec
      elements [ImIndex]..[ImIndex+Len-1] (imaginary part).</summary>
      
<remarks>The results are stored to calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling
      object must be set explicitly. An exception is raised if array borders
      are overrun. An exception is also raised if ReVec or ImVec <see cref="TOpenCLBase.Complex">Complex</see> property is True.
</remarks>
*)
    function RealToCplx(const ReVec, ImVec: TOpenCLMtxVec; ReIndex, ImIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Conjugate.</summary>
      
<remarks>Conjugate all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Conj();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>*)
    function Conj: TOpenCLMtxVec; overload;
    (*<summary>Conjugate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Conj(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec elements.</summary>
      
<remarks>Store the results in the calling object. The
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to match Vec vector.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1].</summary>
      
<remarks>Store them in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Conj(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;


   (*<summary>Extends a real object to a complex object.</summary>
      
<remarks>Extend the calling object to complex vector. After the calling of ExtendToComplex the imaginary part becomes the same as real part if Zeros
      is false. If Zeros is true the imaginary part is set to zero. The use of the in-place version of the method is discouraged because
      it requires 3*N copy operations, while the not-in-place version requires only 2*N copy operations.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray( new double[] {1, 2, 3, 4});
            b.ExtendToComplex(a,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealToCplx"/>
      <SeeAlso cref="ImagPart"/>
      <SeeAlso cref="RealPart"/>*)
    function ExtendToComplex(Zeros: boolean = True): TOpenCLMtxVec; overload;
    (*<summary>Extend Vec object to complex calling object.</summary>
      
<remarks>If Zeros is true then the calling vector imaginary part is set to zero, otherwise
      the calling object imaginary part is the same as calling object real part.
</remarks>
*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean): TOpenCLMtxVec; overload;

    (*<summary>Converts the source to complex.</summary>
      
<remarks>Converts the source to complex by setting the imaginary part to either zero (zeros = True)
      or same as real (zeros = false). Does not set size of the calling vector.
      If there is not sufficient space available to store the result an exception
      will be raised.
</remarks>
*)
    function ExtendToComplex(const Src: TOpenCLMtxVec; Zeros: Boolean; SrcIndex,DstIndex, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.

     Sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is not used
     implicitely within TOpenCLMtxVec methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle: TOpenCLMtxVec; overload;

    (*<summary>FixAngle for calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is True or
      if array borders are overrun/underrun.
</remarks>
*)
    function FixAngle(Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in <c>[-2PI,2PI]</c> for all Src elements.</summary>
      
<remarks>Stores the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the Src object.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Sets angle in [-2PI,2PI] for Src elements [SrcIndex]..[SrcIndex+Len-1] and store the results in the
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set explicitly. An exception is raised
      if array borders are overrun.
</remarks>
*)
    function FixAngle(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Rounds towards negative infinity.</summary>
      
<remarks>Rounds all calling object elements towards negative infinity in-place.

      <c>Floor(-2.8) = -3</c><para/>
      <c>Floor(2.8) = 2</c><para/>
      <c>Floor(-1.0) = -1</c><para/>
</remarks>
*)
    function Floor: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards negative infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards negative infinity and stores the result
      in the calling object.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;

    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards negative infinity
      and stores the result in the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Floor(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate the calling object complex exponential in-place. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            a.Expj();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)
    function Expj: TOpenCLMtxVec; overload;
    (*<summary>Calculate the e^(j*Omega), a complex exponential.</summary>
      
<remarks>Omega must be a real object. If omega is complex, then use the <see cref="Exp"/> method.
</remarks>
*)
    function Expj(const Omega: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the complex exponential for Omega elements [OmegaIndex]..[OmegaIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elemets [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector must be set explicitly. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Expj(const Omega: TOpenCLMtxVec; OmegaIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,5.5,-1.6,6});
            a.Frac();  // a = [0, 0.5, -0.6, 0]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac: TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for all X object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the fractional part for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Frac(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


        (*<summary>Complementary error functon of values.</summary>
      
<remarks>Calculates the complementary error function value for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,5.5,-1.6,6});
            a.Erfc();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc: TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the complementary error function value for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erfc(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Error functon of values.</summary>
      
<remarks>Calculates the error function for all object values in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 5.5, -1.6, 6});
            a.Erf();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf: TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for all Src object values.</summary>
      
<remarks>Stores the result in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculates the error function for Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1]. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Erf(const Src: TOpenCLMtxVec; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload; 


    (*<summary>Flips the real and imaginary part of complex numbers.</summary>
      <returns>Flipped real and imaginary part of complex numbers for all calling object elements in-place.<para/>
        The following transformation is used: <c>a + i*b -> b + i*a</c></returns>

      <SeeAlso cref="FlipConj"/>*)
    function Flip: TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for all X object elements.</summary>
      
<remarks>Xtore the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flips the real and imaginary part of complex numbers for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Flip(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Flips the real and imaginary part of complex numbers and conjugates the complex numbers.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling object complex elements in-place.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj: TOpenCLMtxVec; overload;
    (*<summary>Flip calling object complex elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is false or if array borders
      are overrun/underrun.
</remarks>
*)
    function FlipConj(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Flip all X object complex elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object
      are adjusted automatically.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Flip X object complex elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if calling
      object <see cref="TOpenCLBase.Complex">Complex</see> property is false or if array borders are overrun/underrun.
</remarks>
*)
    function FlipConj(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(x[i]/y[i]).</c><para/>
      The results will be saved to the calling vector.
      X and Y must be a real and have the same length. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the X object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.CopyFromArray( new double[] {0,1,10,-1,-10}); // a = [0, 1, 10,    -1, -10];
            b.CopyFromArray(new double[],{0,1,System.Math.PI,-1,-System.Math.PI}); // b = [0, 1, PI,    -1, -PI];
            c.Rem(a,b);                     // c = [0, 0, 0.5752, 0, -0.5752]
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>*)
    function Rem(const X, Y: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Calculation uses the following formula:

      <c>z[i] = x[xi]-y[yi]*Trunc(x[xi]/y[yi]),</c><para/>
      where i in [Index..Index+Len], xi in [XIndex..XIndex+Len], yi in [YIndex..YIndex+Len].
      The results will be saved to the calling vector. X must be a real. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X, Y: TOpenCLMtxVec; XIndex, YIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; const Y: double): TOpenCLMtxVec; overload;
    (*<summary>Calculates reminder after division X/Y.</summary>
      
<remarks>Reminder is calculated by using the following formula:

      <c>x[i]-y*Trunc(x[i]/y).</c><para/>
      X must be a real. The results will be saved to the calling vector. An exception will be raised if
      array borders are overrun.
</remarks>
*)
    function Rem(const X: TOpenCLMtxVec; const Y: double; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiply all calling object elements with Value in-place.
      This method is the same as the <see cref="Mul"/> method overloads
      multiplying with vector elements with a scalar.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          MtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2,3,5});  // a = [2,3,5]
            a.Scale(3); // a = [6,9,15]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Add"/>*)
    function Scale(const Factor: double): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(const Factor: double; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all calling object elements with a complex Value in-place.</summary>*)
    function Scale(const Factor: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Scale(const Factor: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;













































    (*<summary>Log base N.</summary>
      <returns>Log base N for all calling object elements in-place.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray(new double[] {1,2,3,4});
            a.LogN(10.0);  // log base 10, the slow way a = [Log10(1), Log10(2),...]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function LogN(const N: double): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(const N: double; Index, Len: integer):TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
</remarks>
*)
    function LogN(const N: double; const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base N of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function LogN(const N: double; const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;



    (*<summary>Normalize object.</summary>
      
<remarks>Normalizes Vec object by subtracting a constant Offset from Vec elements and dividing the result by constant Factor:

      <IMG name="TVec21"/>

      The results are stored in calling object. Use this method if you want to do a multiply and add (scale and offset) operations in a single method call.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});  // a = [1,2,3,4]
            b.Normalize(a,2,3);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Add"/>
      <SeeAlso cref="Scale"/>*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset, DivFactor: double): TOpenCLMtxVec; overload;
    (*<summary>Normalize Vec object values [VecIndex]..[VecIndex+Len-1] by subtracting a real constant SubOffset from Vec elements and dividing the result by
      complex constant DivFactor.</summary>
      
<remarks>Store the results in calling vector values [Index]..[Index+Len-1]. An exception
      is raised if Vec or calling object is complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset, DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Normalize Vec object by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset: TCplx; const DivFactor: double): TOpenCLMtxVec; overload;
     (*<summary>Normalize Vec object complex values [VecIndex]..[VecIndex+Len-1] by subtracting a complex constant SubOffset from Vec elements and dividing the result by real constant DivFactor.</summary>
      
<remarks>Store the results in calling vector complex values [Index]..[Index+Len-1]. An exception
      is calling object is not complex or array borders are overrun/underrun.
</remarks>
*)
    function Normalize(const Vec: TOpenCLMtxVec; const SubOffset: TCplx; const DivFactor: double; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;




        (*<summary>Raises base object elements to any power.</summary>
      
<remarks>Raises Base calling object elements to any power. The <see cref="IntPower"/> is faster, if Exponent is an integer.
      Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
      To compute a power to the negative exponent in general case or when the base is negative,
      use the complex version of the function.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});  // a = [1,2,3, 4] //magnitude
            a.Power(1.2);
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="IntPower"/>
      <SeeAlso cref="PowerVec"/>*)
    function Power(const Exponent: double): TOpenCLMtxVec; overload;

    (*<summary>Raises all calling vector elements to complex power Exponent in-place.</summary>
       
<remarks>If the calling vector is real and has negative elements, the result will be NAN
       at those entries. To obtain a valid result in this case, extend the calling
       vector to complex with <see cref="ExtendToComplex"/> method. In all real/complex
       combinations, the Power method will not automatically assume that a number is complex
       to speed up the computation.

       Alternatively the user can of course always convert the real numbers to complex before passing
       them to the Power routine.
</remarks>
*)
    function Power(const Exponent: TCplx): TOpenCLMtxVec; overload;


    (*<summary>Raises base elements to exponent power.</summary>
      
<remarks>Raises Base value to Exponent object values powers and store the results to calling object values.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>


      <SeeAlso cref="PowerVec"/>*)
    function Power(const Base: double; const Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Raises Base complex value to Exponent object values powers.</summary>
      
<remarks>Store the results to calling object values. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TCplx; const Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to complex Exponent power.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TOpenCLMtxVec; const Exponent: TCplx): TOpenCLMtxVec; overload;

    (*<summary>Raises each of the Base object elements to real Exponent power.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Power(const Base: TOpenCLMtxVec; const Exponent: double): TOpenCLMtxVec; overload;
    (*<summary>Raises each of Base object elements to corresponding power, stored in Exponenet elements.</summary>

      
<remarks><c>Power[i] = Base[i]^Exponent[i]</c><para/>

      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
      An exception is raised if Base and Exponent sizes do not match.
</remarks>
*)
    function Power(const Base, Exponent: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Raises Base object elements to Exponent object elements power.</summary>
      
<remarks>Raises Base elements to Exponent elements power. Only positive exponents can be handled
      if exponent object <see cref="TOpenCLBase.Complex">Complex</see> property is True.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.CopyCplxFromArray( new double[] {1,2,3,4});
            b.CopyCplxFromArray( new double[] {3,2,2,2});
            c.PowerVec(a,b);
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function PowerVec(const Base, Exponent: TOpenCLMtxVec): TOpenCLMtxVec;






















    (*<summary>Elements rounded to the nearest whole number.</summary>
      
<remarks>Rounds all calling object elements to the nearest whole number.
      The result can be stored to an array of integers or as floating
      point number.
</remarks>


      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Frac"/>*)
    function Round: TOpenCLMtxVec; overload;
    (*<summary>Round calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Round all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of
      calling object are adjusted automatically.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Round Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object must be set explicitly.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Round(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Initialize elements to Value.</summary>
      
<remarks>Set all calling object elements to Value. If the calling object is complex
      then the real part is set to Value and the imaginary is set to zero.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.Size(false, clFloat, 4);
            a.SetVal(1);
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="SetZero"/>*)
    function SetVal(const Value: double): TOpenCLMtxVec; overload; 

    (*<summary>Set all calling object elements [Index]..[Index+Len-1] to real Value.</summary>
      
<remarks>If the calling object is complex then the real part is set to Value and the imaginary is set to zero.
      An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: double; Index, Len: integer): TOpenCLMtxVec; reintroduce; overload; 

    (*<summary>Set all calling object complex elements to complex Value.</summary>*)
    function SetVal(const Value: TCplx): TOpenCLMtxVec; overload; 

    (*<summary>Set calling object complex elements [Index]..[Index+Len-1] to complex Value.</summary>
       
<remarks><see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are set to true even before the call it was false.
       An exception is raised if calling object array borders are overrun/underrun.
</remarks>
*)
    function SetVal(const Value: TCplx; Index: integer; Len: integer): TOpenCLMtxVec; reintroduce; overload; 

    (*<summary>Initializes object elements to zero.</summary>
      <SeeAlso cref="SetVal"/>*)
    function SetZero: TOpenCLMtxVec; overload; 
    (*<summary>Initializes calling object elements [Index]..[Index+Len-1] to zero.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function SetZero(Index, Len: integer): TOpenCLMtxVec; overload; 

    (*<summary>Changes elements sign.</summary>
      
<remarks>Changes all calling object elements sign <c>(v -> -v)</c> in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {-1,2,-3,4);
            a.Sign(); // a = 1,-2, 3, -4
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mul"/>*)
    function Sign: TOpenCLMtxVec; overload;
    (*<summary>Change calling object elements [Index]..[Index+Len-1] sign in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sign(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Change all X object elements sign.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of calling object are adjusted automatically.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Change X object elements [XIndex]..[XIndex+Len-1] sign.</summary>
      
<remarks>Store the results in callingobject elements [Index]..[Index+Len-1]. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sign(const X: TOpenCLMtxVec; XIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)

    function Sgn: TOpenCLMtxVec; overload;

    (*<summary> Computes signum function of calling object elements [Index..Index+Len-1]. </summary>
                 
<remarks>Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements and stores the result in the calling object.  </summary>
                 
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary> Computes signum function from Src elements [SrcIndex..SrcIndex+Len-1] and stores the result in
                 the calling object [Index..Index+Len-1].  </summary>
                 
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLMtxVec; SrcIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of all Src object elements and multiplies
      it with the calling object elements accordingly.
      Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      The length of Src and of the calling object must match
      or an exception will be raised. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are
      adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine for all calling object elements and stores the sines
      to SinX and cosines to CosX. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of SinX and CosX are
      adjusted automatically.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,s,v;
          MtxVec.CreateIt(out a, out s, out c);
          try
          {
            a.CopyFromArray( new double[] {0,Math387.PIDIV2,Math387.PI);
            a.SinCos(s,c); // s=[0,1,0], c =[1,0,-1]
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(const SinX, CosX: TOpenCLMtxVec); overload; 
    (*<summary>Calculates the sine and cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>stores the sines to SinX elemets [SinXIndex]..[SinXIndex+Len-1] and cosines to CosX elements [CosXIndex]..[CosXIndex+Len-1] elements.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of SinX and CosX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinCos(const SinX, CosX: TOpenCLMtxVec; SinXIndex, CosXIndex, Index, Len: integer); overload; 

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for all calling object elements and stores
      the sines to SinhX and cosines to CoshX. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,s,v;
          clMtxVec.CreateIt(out a, out s, out c);
          try
          {
            a.CopyFromArray( new double[] {0,Math387.PIDIV2,Math387.PI);
            a.SinhCosh(s,c);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(const SinhX, CoshX: TOpenCLMtxVec); overload;
    (*<summary>Calculates the hyperbolic sine and hyperbolic cosine for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the sines to SinhX elemets [SinhIndex]..[SinhIndex+Len-1] and cosines to CoshX elements [CoshIndex]..[CoshIndex+Len-1] elements.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of SinhX and CoshX objects are not set automatically.
      An exception is raised if array borders are overrun/underun.
</remarks>
*)
    procedure SinhCosh(const SinhX, CoshX: TOpenCLMtxVec; SinhIndex, CoshIndex, Index, Len: integer); overload;


   (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is a lower bound for threshold operation.
      All values smaller than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThreshBottom(0.2); // a = [2,0.2,3,4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshBottom(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThreshBottom(const Src: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation on Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshBottom(const Vec: TOpenCLMtxVec; const Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation on all calling object values. The Value parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThreshTop(0.2); // a = [0.2, 0.1, 0.2, 0.2]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)
    function ThreshTop(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom the threshold operation on calling object values [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform threshold operation on all Src object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are
      adjusted automatically.
</remarks>
*)
    function ThreshTop(const Src: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Perform a threshold operation Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThreshTop(const Vec: TOpenCLMtxVec; const Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for
      threshold operation.
      All values less than LTLevel will be replaced with LTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThresholdLT(2.3,1.5); // a = [1.5,1.5,3,4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(const LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdLT(const LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; const LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; const LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const LTLevel: double; const LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "less than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const LTLevel: double; const LTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; const LTLevel: double; const LTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "less than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLMtxVec; const LTLevel: double; const LTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on all calling object values. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values bigger than LTLevel will be replaced with GTValue.
      For complex number comparation is applied with norm of complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThresholdGT(2.3,3.4); // a = [2,0.1,3.4,3.4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(const GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function ThresholdGT(const GTLevel, GTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; const GTLevel, GTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec.Complex and Complex of the calling object are not equal.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; const GTLevel, GTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers.</summary>
      
<remarks>If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const GTLevel: double; const GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than" threshold operation for complex numbers on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      If the calling object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const GTLevel: double; const GTValue: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
      If Vec object contains none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; const GTLevel: double; const GTValue: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than" threshold operation for complex numbers on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      If Vec object or calling object contain none Complex values, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLMtxVec; const GTLevel: double; const GTValue: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform operation on all calling object values. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      All values less than LTLevel will be replaced with LTValue. All values bigger than GTLevel will be replaced with GTValue.
      Operation is available only for none Complex values.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThresholdGT_LT(2.3,3.4,1,0.5); // a = [2,0.5,3.4,3.4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perfrom "greater than and less than" threshold operation on the calling object values in range [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
      An exception will be raised if the calling object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const GTLevel, GTValue, LTLevel, LTValue: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on all Vec object values.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
      An exception will be raised if Vec object contains complex numbers.
</remarks>
*)
    function ThresholdGT_LT (const Vec: TOpenCLMtxVec; const GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLMtxVec; overload;
    (*<summary>Perform "greater than and less than" threshold operation on Vec elements from range [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and  <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be
      set explicitly. An exception is raised if array borders are overrun/underrun.
      An exception will be raised if Vec object or the calling object contain complex numbers.
</remarks>
*)
    function ThresholdGT_LT(const Vec: TOpenCLMtxVec; const GTLevel, GTValue, LTLevel, LTValue: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the TruncDst object as floating point numbers. The fractional
      part is stored in the FracDst.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)

    procedure TruncAndFrac(const TruncDst: TOpenCLMtxVec; const FracDst: TOpenCLMtxVec); overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] and store the results to TruncDst object elements
      [TruncIdx]..[TruncIdx+Len-1].</summary>
      
<remarks>The fractional parts are saved in FracDst elements [FracIdx]..[FracIdx+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      property of calling object must be set explicitly to match those of Src object. An exception is raised if
      array borders are overrun/underrun.
</remarks>
*)
    procedure TruncAndFrac(const TruncDst: TOpenCLMtxVec; const FracDst: TOpenCLMtxVec; TruncIdx, FracIdx, Index, Len: integer); overload;

    (*<summary>Rounds a real number towards zero.</summary>
      
<remarks>Rounds all calling object elements towards zero to an integer and stores
      the result in the calling object again as floating point numbers.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)
    function Trunc: TOpenCLMtxVec; overload;
    (*<summary>Truncate calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(Index,Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Truncate all Src object elements.</summary>
      
<remarks>Store the results in calling object elements. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      property of calling object are adjusted automatically.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Truncate Src object elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Store the results to calling object elemenents [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      property of calling object must be set explicitly to match those of Src
      object. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Trunc(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a;
          MtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {-1, 8});
            a.InvCbrt(); // returns [-1,0.5]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cbrt"/>*)



    function InvCbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      the calling vector are set implicitly to match the X object.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of cube root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function InvCbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2))</c> of all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          MtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 16});
            a.InvSqrt(); // returns [1,0.25]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function InvSqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling vector are set
       implicitly to match the X object.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of square root for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function InvSqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude for all calling object elements in-place.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,-2,3,4}); // a = [1,-2, 3,4]
            a.Mag();  // a = [1, 2, 3,4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Abs"/>
      <SeeAlso cref="PhaseSpectrum"/>*)
    function Mag: TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for all X elements.</summary>
      
<remarks>Store the results in the calling object elements. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match Vec vector.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the magnitude for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the
      calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Mag(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 object elements [Vec2Index]..[Vec2Index+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply all Vec1 elements with corresponding Vec2 elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 size and <see cref="TOpenCLBase.Complex">Complex</see> property do not match.
</remarks>
*)
    function Mul(const Src1, Src2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Vector multiplication.</summary>
      
<remarks>Multiply each of Vec elements with corresponding elements in the calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are set automatically.
      The result is stored in the calling object.
</remarks>


      <SeeAlso cref="Divide"/>*)
    function Mul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload; 
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TOpenCLBase.Complex">Complex</see> property do not match or if array
      borders are overrun/underrun.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload; 


    (*<summary>Multiply object elements with Value.</summary>
      
<remarks>Multiplies all calling object elements with Value in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          MtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {2,3,5});  // a = [2,3,5]
            a.Mul(3); // a = [6,9,15]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Add"/>*)
    function Mul(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply all calling object elements with complex Value in-place.</summary>*)
    function Mul(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with
      complex Value in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Mul(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with Value.</summary>
      
<remarks>Store the result in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Multiply each element of Vec with complex Value.</summary>
      
<remarks>Store the result in the calling object. Size of the calling object is set automatically.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="TOpenCLBase.Complex">Complex</see> propertiy of the calling object is set implicitly.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply Vec elements [VecIndex]..[VecIndex+Len-1] with complex Value.</summary>
      
<remarks>Store the result in calling object elements [Index]..[Index+Len-1].
      Size of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
      <see cref="TOpenCLBase.Complex">Complex</see> propertiy of the calling object is set to True.
</remarks>
*)
    function Mul(const Vec: TOpenCLMtxVec; const Value: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Multiply elements by imaginary unit I.</summary>*)
    function MulI: TOpenCLMtxVec; overload;
    (*<summary>Multipy calling object elements [Index]..[Index+Len-1] with I in-place.</summary>*)
    function MulI(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Multiply X elements with I and store the result in the calling object.</summary>*)
    function MulI(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Multipy X elements [XIndex]..[XIndex+Len-1] with I.</summary>
      
<remarks>Xtore the result in the calling object at locations [Index]..[Index+Len-1].
</remarks>
*)
    function MulI(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The phase angles (spectrum) of object elements.</summary>
      
<remarks>Calculates the phase angles (spectrum) of all Vec object elements. Phase values are returned in radians and are in the range
      -PI,PI. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling
      object are set implicitly to match Vec object. The phase angles are calculated from the following equation:

      <IMG name="TVec23"/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyCplxFromArray( new double[] {1,2,3,-4});
            b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PowerSpectrum"/>
      <SeeAlso cref="ArcTan2"/>*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised
      if array borders are overrun/underrun.
</remarks>
*)
    function PhaseSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The power spectrum from object complex values.</summary>
      
<remarks>Calculates the power spectrum from the Vec object complex values and stores the results (power spectrum)
      in the real calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set
      implicitly to match Vec object. The spectrum elements are squares of the magnitudes of the complex input elements:

      <IMG name="Tvec22"/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyCplxFromArray( new double[] {1,2,3,4});  // a = [1 + 2i, 3 - 4i]
            b.PhaseSpectrum(a);  // b = [arctan2(1,2), arctan2(3,-4)];
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PhaseSpectrum"/>*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculates the power spectrum from the Vec complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Store the results in calling object real elements [Index]..[Index+Len-1]. An exception is raised
      if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or if array borders are overrun/underrun.
</remarks>
*)
    function PowerSpectrum(const Vec: TOpenCLMtxVec; VecIndex, Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts the polar magnitude/phase pairs to cartesian pairs.</summary>
      
<remarks>Convert all AmpltVec and PhaseVec elements (combined) from polar to cartesian form. If AmpltVec and PhaseVec size is not the same
      , an exeption is raised. The results are stored as complex numbers (x=Re, y=Im) in the calling
      object.  Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to
      match AmpltVec and PhaseVec objects.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});  // a = [1,2,3, 4] //magnitude
            b.CopyFromArray( new double[] {1,0,1,-1}); // b = [1,0,1,-1] /phase
            c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(const AmpltVec, PhaseVec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Convert  AmpltVec elements [aIndex]..[aIndex+Len-1] and PhaseVec elements [PIndex]..[PIndex+Len-1] from polar form
      (radius,angle) to cartesian form (x,y).</summary>
      
<remarks>The results are stored as complex numbers <c>(x=Re, y=Im)</c> in the calling
      object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling
      object must be set explicitly. An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function PolarToCart(const AmpltVec, PhaseVec: TOpenCLMtxVec; aIndex, PIndex,Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets real part of complex object values.</summary>
      
<remarks>The method method gets the real part of a complex object Vec and stores the real results in the calling
      object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="TOpenCLBase.Complex">Complex</see> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: TOpenCLVector;
      begin
        CreateIt(a,b);
        try
          a.CopyCplxFromArray(TSingleArray.Create(1,2,3,4)); // = [1+2i, 3+4i]
          b.RealPart(a); // b = [1,3]
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Gets the real part of a Vec object complex elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the results in calling object real elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun or if Vec object <see cref="TOpenCLBase.Complex">Complex</see>
      propety is false.
</remarks>
*)
    function RealPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Square.</summary>
      
<remarks>Calculate the square of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray(new double[] {1,2,3,4});
            a.Sqr(); // a=[1,4,9,16]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and
      <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are not changed.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqr(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray(new double[] {1,4,9});
            a.Sqrt(); // a=[1,2,3]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the
      calling object are adjusted automatically.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the square root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of calling object are not changed. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sqrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Sin(); // Computes complex sine
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Sin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of all X object elements and store the results in calling object.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sin(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosine.</summary>
      
<remarks>Calculate the cosine of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Cos(); // Computes complex sine
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCos"/>
      <SeeAlso cref="SinCos"/>*)
    function Cos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object.vSize and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of calling object and the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cos(const X: TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Tan(); // Computes complex tangens
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)
    function Tan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan (Index: integer; Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tan(const X : TOpenCLMtxVec; XIndex: integer; Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Cot(); // Computes complex cotangens
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)
    function Cot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Cot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Sec(); // Computes complex secant
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)
    function Sec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Sec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of all caling object elements in-place.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Csc(); // Computes complex cosecant
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)
    function Csc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cosecant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Csc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-0.5,0.11,0.9});
            a.ArcSin(); // Computes complex inverse sine
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>*)
    function ArcSin: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse sine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSin(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine of all calling object elements in-place. Values must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-0.5,0.11,0.9});
            a.ArcCos(); // Computes complex inverse cosine
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosine of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCos(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks><para/>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object elements. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically
      to match those of X and Y objects. An exception is raised if X and Y size and <see cref="TOpenCLBase.Complex">Complex</see> properties do not match.

      Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(const Y, X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of Y/X.</summary>
      
<remarks>Calculation uses Y elements [YIndex]..[YIndex+Len-1], X elements [XIndex]..[XIndex+Len-1]
      and stores the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan2(const Y, X: TOpenCLMtxVec; YIndex, XIndex, Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TMtx A,B;
          clMtxVec.CreateIt(out A, out B);
          try
          {
            A.SetIt(2,2,true, new double[]
              {1,0, 2,0,
               2,0  4,1}));  // 2x2, complex matrix
            B.ArcTan(A);
          }
          finally
          {
            clMtxVec.FreeIt(ref A, ref B);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse tangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTan(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TMtx A,B;
          clMtxVec.CreateIt(out A, out B);
          try
          {
            A.SetIt(2,2,true, new double[]
              {1,0, 2,0,
               2,0  4,1}));  // 2x2, complex matrix
            B.ArcCot(A);
          }
          finally
          {
            clMtxVec.FreeIt(ref A, ref B);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cotangens of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCot(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse secant.</summary>
      
<remarks>Calculate the inverse secant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Sec"/>
      <SeeAlso cref="ArcCsc"/>*)
    function ArcSec: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse secant of all X object elements and store the results in the calling object. </summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse secant of X object elements [XIndex]...[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]...[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSec(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse cosecant.</summary>
      
<remarks>Calculate the inverse cosecant for all calling object elements in-place.
</remarks>

      <SeeAlso cref="Csc"/>
      <SeeAlso cref="ArcSec"/>*)
    function ArcCsc: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse cosecant of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsc(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 1.5, 2, 0.3});
            a.Sinh();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine of all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 1.5, 2, 0.3});
            a.Cosh();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 1.5, 2, 0.3});
            a.Tanh();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Tanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Tanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of all caling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1, 1.5, 2, 0.3});
            a.Coth();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
   (*<summary>Calculate the hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Coth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic secant.</summary>
      
<remarks>Calculate the hyperbolic secant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcSech"/>
      <SeeAlso cref="Csch"/>*)
    function Sech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Sech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Hyperbolic cosecant.</summary>
      
<remarks>Calculate the hyperbolic cosecant of all caling object elements in-place.
</remarks>


      <SeeAlso cref="ArcCsch"/>
      <SeeAlso cref="Sech"/>*)
    function Csch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Csch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Absolute values.</summary>
      
<remarks>Calculate the absolute value of all calling object elemets in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,-2,3,4});
            a.Abs(); // a = [1,2,3,4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mag"/>*)
    function Abs: TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value for all X object and store the results in the calling object.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the absolute value of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1].
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Abs(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic sine.</summary>
      
<remarks>Calculate the inverse hyperbolic sine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sinh"/>*)
    function ArcSinh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic sine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSinh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosine.</summary>
      
<remarks>Calculate the inverse hyperbolic cosine for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Cosh"/>*)
    function ArcCosh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosine for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCosh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic tangens.</summary>
      
<remarks>Calculate the inverse hyperbolic tangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Tanh"/>*)
    function ArcTanh: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for all X object elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic tangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
        
<remarks>The results are storedi n the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
        properties of the calling object must be set explicitly.
        An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcTanh(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cotangens.</summary>
      
<remarks>Calculate the inverse hyperbolic cotangens for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Coth"/>*)
    function ArcCoth: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverser hyperbolic cotangens for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cotangens for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCoth(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic secant.</summary>
      
<remarks>Calculate the inverse hyperbolic secant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Sech"/>*)
    function ArcSech: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic secant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcSech(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse hyperbolic cosecant.</summary>
      
<remarks>Calculate the inverse hyperbolic cosecant for all caling object elements in-place.
</remarks>

      <SeeAlso cref="Csch"/>*)
    function ArcCsch: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for calling object elements [Index]...[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse hyperbolic cosecant for X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ArcCsch(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyCplxFromArray( new double[] {1,8});
            a.Cbrt(); // a = [1,2]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt: TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of all X object elements.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the cube root of X object elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>The results are stored
      in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Cbrt(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds all calling object elements towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil: TOpenCLMtxVec; overload;
    (*<summary>Rounds calling object elements [Index]..[Index+Len-1] towards positive infinity in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(Index,Len: integer): TOpenCLMtxVec;  overload;
    (*<summary>Rounds all Src object elements towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted
      automatically.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec): TOpenCLMtxVec;  overload;
    (*<summary>Rounds Src object elements [SrcIndex]..[SrcIndex+Len-1] towards positive infinity.</summary>
      
<remarks>Stores the result in the calling object elements [Index]..[Index+Len-1]
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function Ceil(const Src: TOpenCLMtxVec; SrcIndex, Index,Len: integer): TOpenCLMtxVec;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,8});
            a.Ln();
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)
    function Ln: TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural algorithm for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Ln(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the natural logarithm for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Ln(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {10,100,1000,10000});
            a.Log10(); // a = [1,2,3,4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp10"/>*)
    function Log10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 10 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Log10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,4,8});
            a.Log2(); // a = [0,1,2,3]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp2"/>*)
    function Log2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the
      calling vector are set implicitly to match the X object.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Log2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the log base 2 for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Log2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent (e^) for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,System.Math.E});
            a.Exp(); // a = [0.0,1.00000]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Ln"/>*)
    function Exp: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent (e^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Exp(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 2 (2^).</summary>
      
<remarks>Calculate the exponent base 2 (2^) for all calling object elements in-place.
</remarks>


      

     <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
           a.CopyFromArray( new double[] {1,2,3,4});
            a.Exp2(); // a = [1,4,9,16]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log2"/>*)
    function Exp2: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 2 (2^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp2(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Exponent base 10 (10^).</summary>
      
<remarks>Calculate the exponent base 10 (10^) for all calling object elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            a.Exp10(); // a = [10,100,1000,10000]
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log10"/>*)
    function Exp10: TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for all X elements.</summary>
      
<remarks>Store the results in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the X object.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Exp10(Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the exponent base 10 (10^) for X elements [XIndex]..[XIndex+Len-1].</summary>
      
<remarks>Store the results in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object must be set explicitly. An exception is raised if
      array borders are overrun.
</remarks>
*)
    function Exp10(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Gets the imaginary part of a complex object.</summary>
      
<remarks>Gets the imaginary part of a complex object Vec and stores the real results in the calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="TOpenCLBase.Complex">Complex</see> must be true otherwise the exception will be raised.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyCplxFromArray(new double[] {1,2,3,4});  // a= [1+2i, 3+4i]
            b.ImagPart(a);  // b = [2, 4]
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Gets the imaginary part of complex object Vec elements [VecIndex]..[VecIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object. An exception is raised if the calling object is complex, if Vec is not complex or
      if array borders are overrun/underrun.
</remarks>
*)
    function ImagPart(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power ^(Exponent) for all caling object elements using the integer parameter Exponent.
      For non integer exponents, the <see cref="Power"/> and <see cref="PowerVec"/> methods can be used.
</remarks>


      <Example>
      <code>
      var a: TOpenCLVector;
      begin
        CreateIt(a);
        try
          a.CopyFromArray(TSingleArray.Create(1,2,3,4));
          a.IntPower(3);
        finally
          FreeIt(a);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="Power"/>
      <SeeAlso cref="PowerVec"/>*)
    function IntPower(Exponent: Integer): TOpenCLMtxVec; overload;
    (*<summary>Calculate the power Base^(Exponent) for all Base object elements.</summary>
      
<remarks>Calclation uses the integer Exponent value and stores the results in calling object. Size and
      <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function IntPower(const aBase: TOpenCLMtxVec; Exponent: Integer): TOpenCLMtxVec; overload;

    (*<summary>Inverse elements.</summary>
      
<remarks>Calculates the inverse of all calling object elements in-place without limiting inverse operation.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray( new double[] {1, 2, 3, 4});
            b.Inv(a); // b = [1.0, 0.5, 0.3333, 0.25]
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Divide"/>*)
    function Inv: TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of calling object elements [Index]..[Index+Len-1] in-place.</summary>
      
<remarks>An exception is raised if array borders are overrun/underrun.
</remarks>
*)
    function Inv(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Calculate the inverse of all X object elements without limiting  operating.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Inv(const X: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Calculate the inverse of X object elements [XIndex]..[XIndex+Len-1] without limiting  operating.</summary>
      
<remarks>Store the results in calling object elements [Index]..[Index+Len-1]. An exception is raised if X and calling object <see cref="TOpenCLBase.Complex">Complex</see> property
      does not match or array borders are overrun/underrun.
</remarks>
*)
    function Inv(const X: TOpenCLMtxVec; XIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Converts elements from cartesian to polar coordinate form.</summary>
      
<remarks>Converts all calling object elements from cartesian to polar coordinate form, storing the magnitude (radius)
      component of corresponding elements in the AmpltVec and the phase (angle) component of corresponding elements in
      the PhaseVec. If you want to use this method then the calling matrix <see cref="TOpenCLBase.Complex">Complex</see> property must be
      true. If this is not the case, an exception is raised. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of AmpltVec and
      PhaseVec are set automatically.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLMatrix A, Ampl, Phase;
          clMtxVec.CreateIt(out A, out Ampl, out Phase);
          try
          {
            a.SetIt(2, 2, true,
               new double[] {1,0, 2,0,
                            2,0, 4,1});
            A.CartToPolar(Amplt, Phasw);
          }
          finally
          {
            clMtxVec.FreeIt(ref A, ref Amplt, ref Phase);
          }
        }
      }
      </code></example>

      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(const AmpltVec, PhaseVec: TOpenCLMtxVec); overload;
    (*<summary>Convert calling object elements [Index] to [Index+Len-1] from cartesian to polar form.</summary>
      
<remarks>Store the results in AmpltVec (radius values) and PhaseVec(phase values). Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector must be set explicitly. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    procedure CartToPolar(const AmpltVec, PhaseVec: TOpenCLMtxVec; AmpltIndex, PhaseIndex, Index, Len: integer); overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: double): TOpenCLMtxVec; overload;

    (*<summary>Add each of Vec elements to corresponding elements in the calling object.</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec .</c><para/>
      The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      the calling object are set implicitly to match the Vec object.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition,  the following formula is being used:

      <c>result = result+ Cplx(aScale)*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: double; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Adds Vec elements [VecIndex]..[VecIndex+Len-1] to calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>In addition, the following formula is being used:

      <c>result = result+ aScale*Vec </c><para/>
      An exception is raised if array borders are overrun.
</remarks>
*)
    function AddScaled(const Vec: TOpenCLMtxVec; const aScale: TCplx; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Add a product of two vectors.</summary>
      
<remarks>Multiply Vec1 elements with coresponding Vec2 elements and add the result
      to the calling vector. The size of the calling vector is set implicitly.
</remarks>


      <SeeAlso cref="Mul"/>
      <SeeAlso cref="Add"/>*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;

    (*<summary>Multiply Vec1 elements [Vec1Index]..[Vec1Index+Len-1] with Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and
      add the results to the calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function AddProduct(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;


    (*<summary>Conjugate and multiply.</summary>
      
<remarks>Conjugate each of Vec elements and multiply them with corresponding elements in the calling object.
      The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are set implicitly to match Vec.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TVec a,b,c;
          MtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            b.CopyFromArray( new double[] {4,3,,2,1});
            c.ConjMul(a,b);
          }
          finally
          {
            MtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Conj"/>
      <SeeAlso cref="Mul"/>*)
    function ConjMul(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec elements Vec[VecIndex]..Vec[VecIndex+Len-1] and multiply them with corresponding
      calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties are <b>not</b> set.

      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function ConjMul(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Conjugate each of Vec2 elements and multiply them with corresponding elements in Vec1.</summary>
      
<remarks>The results are stored in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      the calling object are set implicitly to match Vec1 and Vec2 objects.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Conjugate Vec2 elements [Vec2Index]..[Vec2Index+Len-1] and multiply them with corresponding Vec1 elements
      [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling vector must be set explicitly.
      An exception is raised if array borders are overrun.
</remarks>
*)
    function ConjMul(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide each of Num elements with corresponding elements in Den.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are set automatically.
      The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Num, Den: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide each of calling vector elements with corresponding elements in the Vec object.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are set automatically. The result
      is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide calling vector elements [Index]...[Index+Len-1] with corresponding elements
       [VecIndex]...[VecIndex+Len-1] from the Vec object.</summary>
       
<remarks>Store the result in the claling vector. The <see cref="TOpenCLBase.Length">Length</see>
       of the calling vector is not changed. An exception is raised if
       array borders are overrun or underrun.

       The result of division by zero will be the INF constant. Division of zero
       by zero will result in NAN.
</remarks>
*)
    function Divide(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide [NumIndex]..[NumIndex+Len-1] Num elements with [DenIndex]..[DenIndex+Len-1] elements in Den.</summary>
      
<remarks>and store result in the calling vector at positions [Index]..[Index+Len-1]
      <see cref="Size"/> and <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are not changed.
      An exception is raised if array borders are overrun or underrun.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(const Num, Den: TOpenCLMtxVec; NumIndex, DenIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Divide Value with elements of the calling object and store the result in the calling object.</summary>*)
    function DivideBy(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements of the calling object.</summary>
      
<remarks>Store the result in the calling object.
</remarks>
*)
    function DivideBy(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [Index]...[Index+Len-1] from the calling object.</summary>
      
<remarks>Store the result in the calling object at position [Index]...[Index+Len-1].
      Calling vector will be extended to complex, if the calling vector is real.
      An exception will be raised if array borders are overrun or underrun.
</remarks>
*)
    function DivideBy(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set automatically.
</remarks>
*)
    function DivideBy(const Value: double; const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements from Vec and store the result in the corresponding elements of the calling object.</summary>
      
<remarks>Size of the calling object is set automatically.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(const Value: TCplx; const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Divide Value with Vec elements [VecIndex]..[VecIndes+Len-1].</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object.
      Size of the calling object is not changed. An exception will be raised array borders are overrun or underrun.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set implicitly.
</remarks>
*)
    function DivideBy(const Value: double; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Divide complex Value with elements [VecIndex]..[VecIndes+Len-1] from Vec.</summary>
      
<remarks>Store the result in the elements [Index]..[Index+Len-1] of the calling object. Size of the calling object is not changed. An exception will be raised
       array borders are overrun or underrun.
      <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(const Value: TCplx; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverse vector elements.</summary>
      
<remarks>The method reverses Vec vector elements from [VecIndex].. [VecIndex+Len-1]
      and stores them in the calling vector from [Index]...[Index+Len-1]
      by using the following equation:<para/>

      <IMG name="TVec24"/><para/>

      This overload reverses calling vector elements in-place.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          MtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4);
            a.Reverse();   // a = [4,3,2,1]
          }
          finally
          {
            MtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Rotate"/>
      <SeeAlso cref="Shift"/>*)
    function Reverse(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Reverses the calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Reverse(Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>A cyclic shift on vector elements in range.</summary>
      
<remarks>Performs cyclic shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>

      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Rotate(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer): TOpenCLMtxVec; overload;

    (*<summary>A shift on vector elements in range.</summary>
     
<remarks>Performs shift on source vector elements in specified range [Index..Index+Len] and stores them to calling vector.
      The number of elements to shift is specified in the Offset parameter.
      Offset can be any integer number, positive or negative.
</remarks>


      <SeeAlso cref="Reverse"/>
      <SeeAlso cref="Shift"/>*)
    function Shift(const Vec: TOpenCLMtxVec; Offset: integer; VecIndex,Index: integer; Len: integer = MtxVecEOA): TOpenCLMtxVec; overload;

    (*<summary>Subtracts Value from object elements.</summary>
      
<remarks>Subtracts Value from all calling object elements.
</remarks>


      <SeeAlso cref="Add"/>*)
    function Sub(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from all calling object complex elements.</summary>*)
    function Sub(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtracts Value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtracts complex Value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun.
</remarks>
*)
    function Sub(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtract real Value from Src.</summary>
      
<remarks>Store the results in calling object.
      Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Store the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract real Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: double; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract complex Value from Src elements [SrcIndex]..[SrcIndex+Len-1].</summary>
      
<remarks>Stores the result in calling object elements [Index]..[Index+Len-1].
      Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set explicitly.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Src: TOpenCLMtxVec; const Value: TCplx; SrcIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Array subtraction.</summary>
      
<remarks>Subtract each of Vec elements from corresponding elements in the calling object.
      An exception is raised if Vec and calling object size and <see cref="TOpenCLBase.Complex">Complex</see> properties do not match.
</remarks>

      <SeeAlso cref="Add"/>*)
    function Sub(const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec2 elements from Vec1 elements.</summary>
      
<remarks>Stores the results in calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are
      adjusted automatically. An exception is raised if Vec1 and Vec2 size and <see cref="TOpenCLBase.Complex">Complex</see> property do not match.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec elements [VecIndex]..[VecIndex+Len-1] from corresponding calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The results are stored in the calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract Vec22 elements [Vec2Index]..[Vec2Index+Len-1] from Vec1 object elements [Vec1Index]..[Vec1Index+Len-1].</summary>
      
<remarks>Stores the results in calling object elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object must be set explicitly. An exception is raised if
      array borders are overrun or underrun.
</remarks>
*)
    function Sub(const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Subtraction from value.</summary>
      
<remarks>Subtract each of calling object elements from Value.
</remarks>


      <SeeAlso cref="Add"/>
      <SeeAlso cref="Sub"/>*)
    function SubFrom(const Value: double): TOpenCLMtxVec; overload;
    (*<summary>Subtract each of calling object elements from complex Value.</summary>
      
<remarks>If the calling vector s not complex, the conversion is performed automatically in a
      performance efficient way.
</remarks>
*)
    function SubFrom(const Value: TCplx): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from Value.</summary>
      
<remarks>Store the result in calling vector. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(const Value: double; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Subtract elements [Index]..[Index+Len-1] from complex Value.</summary>
      
<remarks>Store the result in calling object. If the calling vector is not complex, the conversion to complex is performed
      automatically in performance efficient way. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function SubFrom(const Value: TCplx; Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract complex Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. Size property of the calling object is set
      automatically. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; const Vec: TOpenCLMtxVec): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from Value.</summary>
      
<remarks>Stores the result to the calling object elements [Index]..[Index+Len-1].
       Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is adjusted automatically.
</remarks>
*)
    function SubFrom(const Value: double; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;
    (*<summary>Substract Vec elements [VecIndex]..[VecIndex+Len-1] from complex Value and store the result to the
       calling object elements [Index]..[Index+Len-1].</summary>
       
<remarks>Size property of the calling object is not changed. An exception is raised if array borders are overrun or underrun.
       <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(const Value: TCplx; const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Inserts zeroes between consecutive array values.</summary>
      
<remarks>Copy Len values from Src starting at SrcIndex to the calling object starting at position Index and place Factor-1 zeros
      between consecutive values. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object must be set
      explicitly. Phase parameter defines the initial sample offset and must be less then Factor. An exception is raised,
      if array borders are overrun/underrun.
</remarks>

      <SeeAlso cref="DownSample"/>*)
    function UpSample(const Src: TOpenCLMtxVec; Factor,SrcIndex, Index, Len: integer; Phase: integer = 0): TOpenCLMtxVec; overload;

    (*<summary>Downsamples object values.</summary>
      
<remarks>Copy only every Factor sample from Src starting at SrcIndex up to Len to the calling object starting
      at Index. The phase parameter determines the initial sample offset. Phase must be less then Factor. Size and
      <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly. An exception is raised if array borders are
      overrun/underrun.
</remarks>


      <SeeAlso cref="UpSample"/>*)
    function DownSample(const Src: TOpenCLMtxVec; Factor, SrcIndex, Index, Len: integer; Phase: integer = 0): TOpenCLMtxVec; overload;

    (*<summary>Difference.</summary>
      
<remarks>Calculate the difference for Vec elements [VecIndex]..[VecIndex+Len-1] and store the results in the calling object
      elements [Index]..[Index+Len-1]. Size and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling vector must be set explicitly.
      The following formula is used to calculate the difference:

      <IMG name="tvec19"/><para/>
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Difference(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TOpenCLMtxVec; overload;

    (*<summary>Sums vector values.</summary>
      <returns>the sum of all calling object elements. An exception is raised if calling object
      <see cref="TOpenCLBase.Complex">Complex</see> property is true.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {1,3,-2});
            doouble s = a.Sum();  // s = 2
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>


      <SeeAlso cref="Sumc"/>*)
    function Sum: double; overload;
    (*<summary>Returns the sum of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is True or array
      borders are overrun/underrun.
</remarks>
*)
    function Sum(Index,Len: integer): double; overload;
    (*<summary>Calculates the sum of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the result in real ASum variable. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is True or array borders are overrun/underrun.
</remarks>
*)
    procedure Sum(out ASum: double; Index, Len: integer); overload;
    (*<summary>Calculates the sum of all calling object complex elements.</summary>
      
<remarks>Stores the result in complex ASum variable. An exception is raised if calling
      object <see cref="TOpenCLBase.Complex">Complex</see> property is False.
</remarks>
*)
    procedure Sum(out ASum: TCplx); overload;
    (*<summary>Calculates the sum of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>Stores the result in complex ASum variable.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False or array
      borders are overrun/underrun.
</remarks>
*)
    procedure Sum(out ASum: TCplx; Index, Len: integer); overload;

    (*<summary>Sum (complex value).</summary>
      <returns>the complex sum of all calling object complex elements.</returns>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False.
</remarks>


      <SeeAlso cref="Sum"/>*)
    function Sumc: TCplx; overload;
    (*<summary>Returns the complex sum of calling object complex elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False or array borders are overrun/underrun.
</remarks>
*)
    function Sumc(Index,Len: integer): TCplx; overload;



    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object elements. The result is a real value.</returns>
      
<remarks>An exception is raised is calling object <see cref="TOpenCLBase.Complex">Complex</see> is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            double b = a.Max(); // 4.0
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Min"/>
      <SeeAlso cref="Maxc"/>
      <SeeAlso cref="MaxMin"/>*)
    function Max: double; overload;
    (*<summary>Returns the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun.
</remarks>
*)
    function Max(Index,Len: integer): double; overload;
    (*<summary>Calculate the maximum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun.
</remarks>
*)
    procedure Max(out AMax: double; Index,Len: integer); overload;
    (*<summary>Calculate the maximum value of all calling object elements.</summary>
      
<remarks>The AMax parameter returns the maximum value. The aIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>
*)
    procedure Max(out AMax: double; out aIndex: integer); overload;
    (*<summary>Calculate the maximum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the maximum value. The aIndex parameter returns the index of maximum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Max(out AMax: double; out aIndex: integer; Index, Len: integer); overload;
    (*<summary>Same as <see cref="Maxc"/> method.</summary>*)
    function Max(out AMax: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Maximum value.</summary>
      <returns>the maximum value of all calling object complex elements. Complex elements are first compared by the amplitude
        and then by the argument.</returns>

      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> is .
</remarks>


      <SeeAlso cref="Max"/>*)
    function Maxc: TCplx; overload;
    (*<summary>Returns the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised if calling object
      <see cref="TOpenCLBase.Complex">Complex</see> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(Index,Len: integer): TCplx; overload;
    (*<summary>Calculate the maximum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns complex maximum value. Returns the index of maximum value. Complex elements are first compared by the amplitude and then by the argument.
      The aIndex parameter returns the index of maximum value. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see>
      property is  or array borders are overrud/underrun.
</remarks>
*)
    function Maxc(out AMax: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Maximum and minimum value in a single pass.</summary>
      
<remarks>Calculates the maximum and minimum value of all calling object elements in a single pass. Maximum value is stored in AMax parameter,
      minimum value is stored in AMin parameter. Use this method if you require minimum AND maximum value.
</remarks>


      <SeeAlso cref="Min"/>
      <SeeAlso cref="Max"/>*)
    procedure MaxMin(out AMax,AMin: double); overload;
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is stored in AMax parameter, minimum value is stored in AMin parameter.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure MaxMin(out AMax,AMin: double; Index, Len: integer); overload;
    (*<summary>Calculates the maximum and minimum value of all calling object elements.</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer); overload;
    (*<summary>Calculates the maximum and minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Maximum value is returned in AMax parameter, minimum
      value in AMin parameter. The MaxIdx parameter returns the index of maximum value. The MinIdx parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or if array borders are overrun/underrun.
</remarks>
*)
    procedure MaxMin(out AMax: double; out MaxIdx: integer; out AMin: double; out MinIdx: integer; Index, Len: integer); overload;

    (*<summary>Mean value.</summary>
      
<remarks>Calculate the mean value of all calling object elements. The result is a real value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(, new double[] {1,2,3,4});
            double m = a.Mean(); // 2.5
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sum"/>
      <SeeAlso cref="Meanc"/>*)
    function Mean: double; overload;

    (*<summary>Returns real mean value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun.
</remarks>
*)
    function Mean(Index, Len: integer): double; overload;
    (*<summary>Calculate the mean value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a real value. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun/underrun.
</remarks>
*)
    procedure Mean(out AMean: double; Index, Len: integer); overload;
    (*<summary>Same as <see cref="Meanc"/>.</summary>*)
    procedure Mean(out AMean: TCplx); overload;
    (*<summary>Same as <see cref="Meanc"/>.</summary>*)
    procedure Mean(out AMean: TCplx; Index, Len: integer); overload;

    (*<summary>Mean value.</summary>
      <returns>the mean value of all calling object complex elements. The result is a complex value.</returns>
      
<remarks>An exception is raised is calling object <see cref="TOpenCLBase.Complex">Complex</see> is .
</remarks>


      <SeeAlso cref="Mean"/>*)
    function Meanc: TCplx; overload;
    (*<summary>Returns complex mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is  or array borders are overrun/underrun.
</remarks>
*)
    function Meanc(Index, Len: integer): TCplx; overload;
    (*<summary>Calculate the mean value from all calling object complex elements.</summary>
      
<remarks>The result AMean is a complex value. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is .
</remarks>
*)
    procedure Meanc(out AMean: TCplx); overload;
    (*<summary>Calculate the mean value from calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMean is a complex value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is  or array borders are overrun/underrun.
</remarks>
*)
    procedure Meanc(out AMean: TCplx; Index, Len: integer); overload;

    (*<summary>Minimum value.</summary>
      <returns>The minimum value of all calling object elements. The result is a real value.</returns>

      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(, new double[] {1,2,3,4});
            double b = a.Min();  // 1.0
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Max"/>
      <SeeAlso cref="Minc"/>*)
    function Min: double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a real value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun.
</remarks>
*)
    function Min(Index,Len: integer): double; overload;
    (*<summary>Calculate the minimum value from calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result AMin is a real value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrun.
</remarks>
*)
    procedure Min(out AMin: double; Index,Len: integer); overload;
    (*<summary>Calculate the minimum value of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMax parameter returns the
      minimum value. The aIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or array borders are overrud/underrun.
</remarks>
*)
    procedure Min(out AMin: double; out aIndex: Integer; Index, Len: integer); overload;
    (*<summary>Calculate the minimum value of all calling object elements.</summary>
      
<remarks>The AMin parameter returns the
      minimum value. The aIndex parameter returns the index of minimum value.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>
*)
    procedure Min(out AMin: double; out aIndex: integer); overload;
    (*<summary>Same as the <see cref="Minc"/> method.</summary>*)
    function Min(out AMin: TCplx; Index, Len: integer): integer; overload;

    (*<summary>Minimum value.</summary>
      
<remarks>Returns the minimum value of all calling object complex elements. Complex elements are first compared by the amplitude
      and then by the argument. An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> is .
</remarks>


      <SeeAlso cref="Min"/>*)
    function Minc: TCplx; overload; 
    (*<summary>Returns the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The result is a complex value. Complex elements are first compared by the amplitude and then by the argument. An exception is raised
      if calling object. <see cref="TOpenCLBase.Complex">Complex</see> property is  or array borders are overrud/underrun.
</remarks>
*)
    function Minc(Index,Len: integer): TCplx; overload; 
    (*<summary>Calculate the minimum value of calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>The AMin parameter returns complex minimum value. Returns the index of minimum value. Complex elements are first compared by the amplitude and then by the argument.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is  or array borders are overrun/underrun.
</remarks>
*)
    function Minc(out AMin: TCplx; Index, Len: integer): integer; overload; 

    (*<summary>The C-norm.</summary>
      <returns>C norm: <c>||V-Vec||</c>, where V is the calling vector.</returns>
      
<remarks>If the NormC is called without any parameters, the NormC
      calculates the norm of the calling vector. The C norm of <c>||V-Vec||</c> is defined by the formula:<para/>

      <IMG name="TVec10"/><para/>
      If RelativeError is true then the computed norm is divided by the
      norm of V, and the function returns the "relative error":<para/>

      <IMG name="TVec09"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            a.CopyFromArray(new double[] {4,3,2,1});
            double c = a.NormC(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormL1"/>
      <SeeAlso cref="NormL2"/>*)
    function NormC(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload; 
    (*<summary>Calculates the C norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload; 


    (*<summary>Calculate the C-norm of calling vector.</summary>
      <returns><c>NormC = max|a[i]|, 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormC: double; overload; 
    (*<summary>Calculates the C norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormC(Index,Len: integer): double; overload; 

    (*<summary>The L1-norm.</summary>
       <returns>L-1 norm, defined by: <c>||V-Vec||</c>, where V is calling vector.</returns>

       
<remarks>If the NormL1 is called without any parameters, the NormL1 calculates the norm of calling vector. The L1 norm of <c>||V-Vec||</c>
       is defined by the formula:

      <IMG name="TVec12"/><para/>
      If RelativeError is true then the computed norm is divided by the
      norm of V, and the function returns the "relative error":

      <IMG name="TVec09"/><para/>
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray( new double[] {1,2,3,4});
            a.CopyFromArray( new double[] {4,3,2,1});
            double c = a.NormL1(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL2"/>*)
    function NormL1(const Vec: TOpenCLMtxVec; RelativeError: boolean = false): double; overload; 
    (*<summary>Calculates the L1 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = false): double; overload; 

    (*<summary>Calculate the L1-norm of the calling vector.</summary>
     <returns>L-1 norm, defined by the following equation:<para/>
      <c>NormL1 = Sum(|a[i]|), 0 &lt; i &lt; Length-1</c></returns>*)
    function NormL1: double; overload; 
    (*<summary>Calculates the L1 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL1(Index,Len: integer): double; overload; 

    (*<summary>The L2-norm.</summary>
     <returns>L2 norm, defined by <c>||V-Vec||</c>, where V is calling vector.</returns>
     
<remarks>If the NormL2 is called without any parameters, the NormL2 calculates the
     norm of calling vector. The L2 norm of ||V-Vec|| is defined by the formula:<para/>

     <IMG name="TVec11"/><para/>
     If RelativeError is true then the computed norm is divided by the norm of V, and the function returns the "relative error":<para/>

     <IMG name="TVec09"/><para/>
</remarks>


     

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            a.CopyFromArray(new double[] {4,3,2,1});
            double c = a.NormL2(b,true);
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="NormC"/>
      <SeeAlso cref="NormL1"/>*)
    function NormL2(const Vec: TOpenCLMtxVec; RelativeError: boolean = False): double; overload; 
    (*<summary>Calculates the L2 norm ||V-Vec|| between Vec elements [VecIndex]..[VecIndex+Len-1]
      and calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(const Vec: TOpenCLMtxVec; VecIndex,Index,Len: integer; RelativeError: boolean = False): double; overload; 

    (*<summary>Calculate the L2-norm of the calling vector.</summary>
     <returns>L2 norm, defined by: <c>NormaL2 = ( Sum(|a[i]|^2) )^0.5  , 0 &lt; i &lt; Length-1 .</c></returns>*)
    function NormL2: double; overload;  
    (*<summary>Calculates the L2 norm from calling vector elements [Index]..[Index+Len-1].</summary>*)
    function NormL2(Index,Len: integer): double;  overload; 

    (*<summary>Computes the sum and the sum of squared elements from the elements in the calling object.</summary>*)
    procedure SumAndSqrSum(out Sum, SqrSum: double); overload;

    (*<summary>Returns the sum and the sum of squared elements.</summary>
               
<remarks>Returns the sum and the sum of squared items from calling vector elements [Index]..[Index+Len-1].
</remarks>
*)

    procedure SumAndSqrSum(out Sum, SqrSum: double; Index, Len: integer); overload;

    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
              a.CopyFromArray(new double[] {-1,2,-3,4));
              double c = a.StdDev();
          }
          finally
          {
              clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>


      <SeeAlso cref="Mean"/>*)
    function StdDev: double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or if array
      borders are overrun/underrun.
</remarks>
*)
     function StdDev(Index,Len: integer): double; overload;
    (*<summary>Returns the standard deviation of all calling object elements.</summary>
       
<remarks>The sum and the sum of squares of all calling object elements must be passed as parameters.
       An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double): double; overload;
    (*<summary>Returns the standard deviation of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>The sum and the sum of squares of the coresponding elements must be passed as parameters.
      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true
      or if array borders are overrun/underrun.
</remarks>
*)
    function StdDev(aSum, aSumSqr: double; Index, Len: integer): double; overload;


    (*<summary>Standard deviation.</summary>
      
<remarks>Calculate the standard deviation of all calling object elements. The result is a real value.
      An exception is raised if calling vector <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      <Example>
      <code>
      var a: clVector;
          c: double;
          aMean: double;
      begin
          a.CopyFromArray(TDoubleArray.Create(1,2,3,4));
          Caption := FloatToSTr(a.RMS);
      end;
      </code>
      </Example>


      <SeeAlso cref="Mean"/>*)
    function RMS: double; overload;
    (*<summary>Returns the root-mean-square of calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true or if array
      borders are overrun/underrun.
</remarks>
*)
     function RMS(Index,Len: integer): double; overload;


    (*<summary>Elements product.</summary>
      <returns>the product of all calling object elements</returns>

      
<remarks><IMG name="TVec25"/>

      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is true.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a;
          clMtxVec.CreateIt(out a);
          try
          {
            a.CopyFromArray(new double[] {1,2,3,4});
            double c = a.Product(); // c= 24
          }
          finally
          {
            clMtxVec.FreeIt(ref a);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Productc"/>*)
    function Product: double; overload;
    (*<summary>Returns the product for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function Product(Index,Len: integer): double; overload;
    (*<summary>Calculate the product of all calling object complex elements.</summary>
      
<remarks>Store the result in complex variable X.
</remarks>
*)
    procedure Product(out X: TCplx); overload;
    (*<summary>Calculate the product for calling object complex elements [Index]..[Index+Len-1].</summary>
      
<remarks>Store the result in complex variable X. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure Product(out X: TCplx; Index,Len: integer); overload;
    (*<summary>Calculate the product of all calling object elements.</summary>
      
<remarks>Store the result in real variable X.
</remarks>
*)
    procedure Product(out X: double); overload;
    (*<summary>Calculate the product for calling object elements [Index]..[Index+Len-1].</summary>
      
<remarks>Store the result in real variable X. An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    procedure Product(out X: double; Index,Len: integer); overload;

    (*<summary>Elements product.</summary>
      <returns>the complex product of all calling object complext elements.</returns>

      
<remarks><IMG name="TVec25"/>

      An exception is raised if calling object <see cref="TOpenCLBase.Complex">Complex</see> property is false.
</remarks>


      <SeeAlso cref="Product"/>*)
    function Productc: TCplx; overload;
    (*<summary>Returns the complex product for calling object complex elements [Index]..[Index+Len-1].</summary>
       
<remarks>An exception is raised if array borders are overrun or underrun or if <see cref="TOpenCLBase.Complex">Complex</see> propety is false.
</remarks>
*)
    function Productc(Index,Len: integer): TCplx; overload;

    (*<summary>Scalar product of two real arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a real scalar value.
      The dot product is defined by the equation:

      <IMG name="TVec13"/><para/>
      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            double prod = a.DotProd(b); // = 1*5 + 2*6 + * 3*7 + 4*8
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)
    function DotProd(const Vec: TOpenCLMtxVec): double; overload;

    (*<summary>Returns the scalar product between Vec elements [VecIndex]..[VecIndex+Len-1] and calling object elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TOpenCLBase.Complex">Complex</see> property is True.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProd(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): double; overload;


    (*<summary>Scalar product of two complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a complex scalar value.
      An exception is raised if calling or Vec object <see cref="TOpenCLBase.Complex">Complex</see> property is false.
      The dot product is defined by the equation:<para/>

      <IMG name="TVec13"/><para/>

      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            double prod = a.DotProdc(b); //= (1+2i)*(5+6i)+(3+4i)*(7+8i)
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProd"/>*)
    function DotProdc(const Vec: TOpenCLMtxVec): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>An exception is raised if Vec and calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; VecIndex, Index, Len: integer): TCplx; overload;
    (*<summary>Returns the scalar product between Vec and calling object complex elements.</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean): TCplx; overload;
    (*<summary>Returns the scalar product between Vec (complex) elements [VecIndex]..[VecIndex+Len-1] and calling object (complex) elements
      [Index]..[Index+Len-1].</summary>
      
<remarks>If ConjVec is True, scalar product between calling object and conjugated Vec object elements is calculated.
      An exception is raised if Vec and calling object <see cref="TOpenCLBase.Complex">Complex</see> property is False.
      An exception is raised if array borders are overrun or underrun.
</remarks>
*)
    function DotProdc(const Vec: TOpenCLMtxVec; ConjVec: boolean; VecIndex, Index, Len: integer): TCplx; overload;

   

    procedure DotProd(DstIndex: integer; const Vec1, Vec2: TOpenCLMtxVec; Vec1Index, Vec2Index: integer; Len: integer; const Buffer: TOpenCLMtxVec); overload;

    (*<summary>Scalar product of two real or complex arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the Vec1 and Vec2 stores the result in calling vector at position DstIndex.
      ConjVec parameter is ignored if data is real. If ConjVec is true, the function computes: result = Vec1*conj(Vec2)

      The dot product is defined by the equation:<para/>

      <IMG name="TVec13"/><para/>

      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>
*)

    procedure DotProd(DstIndex: integer; const Vec1, Vec2: TOpenCLMtxVec;ConjVec: boolean; const Buffer: TOpenCLMtxVec); overload;

    (*<summary>Scalar product of two real arrays.</summary>
      
<remarks>Calculates the dot product (scalar value) of the calling object and Vec object and returns a real scalar value.
      The result is stored at specified index in the calling vector in the GPU memory. This variant of the function
      is non-blocking (faster), because the result does not have to be copied to the CPU memory to be used.

      The dot product is defined by the equation:

      <IMG name="TVec13"/><para/>
      Both objects must be of equal size. If they are not, the method will return the dot product of the largest sub-array.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLVector a,b,c;
          double[] ac;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.SetIt(false, new double[] {1,2,3,4});
            b.SetIt(false, new double[] {5,6,7,8});
            c.DotProd(0, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.DotProd(1, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.DotProd(2, a, b); // c[0] = 1*5 + 2*6 + * 3*7 + 4*8
            c.CopyToArray(ac); //ac = [70, 70, 70]
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="DotProdc"/>*)


    procedure DotProd(DstIndex: integer; const Vec1, Vec2, Buffer: TOpenCLMtxVec); overload;

    
    
    
    
    
  end;

 

  TOpenCLVector = class(TOpenCLMtxVec )
  private
    
    function GetData: TVec;
    

  strict protected
    
    property Data: TVec read GetData;
    
    procedure InitData; override;
    procedure FreeToCache; override;
    function GetRealValues: string; override;
    function GetComplexValues: string; override;
  protected
     function DebugUpdate: integer; override;
  public
    procedure Reset; override;
    procedure Size(const Src: TOpenCLBase); override;
    function Size(const Src: TOpenCLBase; aComplex: boolean): TOpenCLBase; override;

    procedure Size(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); override;
    procedure SetIt(aComplex: boolean; const a: array of double); overload;

    procedure SetSubRange(const Src: TOpenCLMtxVec); overload;
    procedure SetSubRange(const Src: TOpenCLMtxVec; Index: integer; Len: integer); overload;

    procedure SetSubIndex(const Src: TOpenCLMtxVec); overload;
    procedure SetSubIndex(const Src: TOpenCLMtxVec; BeginIndex, EndIndex: integer); overload;

    procedure SetSubRange(Index, Len: integer); overload; override;
    procedure SetFullRange; override;

    procedure Assign(Src: TOpenCLMtxVec); virtual;

    (*<summary>The norm of a vector.</summary>
      
<remarks>Calculates the norm of a Vec vector and stores the results in calling vector.
      This functions works the same as <see cref="TMtxVec.PowerSpectrum"/>.
</remarks>
*)
    function Norm(const Vec: TOpenCLVector): TOpenCLVector;
    (*<summary>The difference between two succesive vector elements.</summary>
      
<remarks>Store the results in the calling vector. The <see cref="TOpenCLBase.Length">Length</see>
      of the calling vector is set to one less the length of Vec and <see cref="TOpenCLBase.Complex">Complex</see>
      property is set to Vec.Complex. The following formula is used to
      calculate the difference:

      <IMG namr="tvec19"/><para/>

      The Length of calling vector is automatically decremented by one.
</remarks>
*)

    function Difference(const Vec: TOpenCLMtxVec; Lag: Integer = 1): TOpenCLVector; overload;

    (*<summary>Fills the calling vector with a series following linear rule.</summary>
      
<remarks>Fills the calling vector with a series following the rule:

      <code>
      Values[k] :=  k
      </code><para/>

      (Offset is zero and Step is one). If the calling vector is complex,
      only the real part is set.
</remarks>


      <Example>
      <code>
      var a: TOpenCLVector;
      begin
        clMtxVec.CreateIt(a);
        try
          a.Size(5,True);
          a.Ramp(0,PI);
          a.Sin;
        finally
          clMtxVec.FreeIt(a);
        end;
      end;
      </code><para/>
      which is identical to:<para/>
      <code>
      clMtxVec.CreateIt(a);
      try
        a.Size(5,True);
        for i:= 0 to a.Length-1 do a[i] := sin(i * PI);
      finally
        clMtxVec.FreeIt(a);
      end;
      </code>
      </Example>

      <SeeAlso cref="TOpenCLBase.SetVal"/>*)
    function Ramp: TOpenCLVector; overload;
    (*<summary>Fills the calling vector with a series.</summary>
      
<remarks>Method follow the rule:

      <code>
      CValues[k] := Offset + k*Step.
      </code><para/>
</remarks>
*)
    function Ramp(const Offset, Step: TCplx): TOpenCLVector; overload;
    (*<summary>Fills the calling vector.</summary>
      
<remarks>Method uses the following rule:

      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      If the calling vector is complex, only the real part is set.
</remarks>
*)
    function Ramp(const Offset, Step: double): TOpenCLVector; overload;
    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Method uses the following rule:

      <code>Values[k] := Offset + k*Step.
      </code><para/>
      If the calling vector is complex, only the real part is set.
      An exception is raised if calling vector
      array borders are overrun.
</remarks>
*)
    function Ramp(const Offset,Step: double; Index,Len: integer): TOpenCLVector; overload;
    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Following the rule:

      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      If the calling vector is complex, only the real part is set.
      An exception is raised if array borders of the calling vector
      are overrun.
</remarks>
*)
    function Ramp(const Offset, Step: TCplx; Index,Len: integer): TOpenCLVector; overload;
    (*<summary>The Offset is complex, but the step is real.</summary>*)
    function Ramp(const Offset: TCplx; const Step: double): TOpenCLVector; overload;
    (*<summary>Fills the calling vector elements [Index]..[Index+Len-1].</summary>
      
<remarks>Following the rule:
      <code>
      Values[k] := Offset + k*Step.
      </code><para/>
      The Offset is complex, but the step is real.
</remarks>
*)
    function Ramp(const Offset: TCplx; const Step: double; Index,Len: integer): TOpenCLVector; overload;

      (*<summary>Inserts zeroes between consecutive vector values.</summary>
        
<remarks>Inserts zeroes between consecutive vector values. The method copies the values from Src to the calling vector and
        places Factor-1 zeros between consecutive values. The <see cref="TOpenCLBase.Length">Length</see> and <see cref="TOpenCLBase.Complex">Complex</see>
        properties of the calling vector are set implicitly. Phase parameter defines the initial sample offset and must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: TOpenCLVector;
        begin
          clMtxVec.CreateIt(a,b);
          try
            b.CopyFromArray(TSingleArray.Create(0,0,1,3,2));
            a.UpSample(b,2); // a = [0,0,0,0,1,0,3,0,2,0]
          finally
            clMtxVec.FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="DownSample"/>
        <SeeAlso cref="TOpenCLMtxVec.DownSample"/>
        <SeeAlso cref="TOpenCLMtxVec.UpSample"/>*)
      function UpSample(const Src: TOpenCLMtxVec; Factor: integer; Phase: integer = 0): TOpenCLVector; overload;

      (*<summary>Downsamples vector values.</summary>
        
<remarks>The methods copies only every Factor sample from the Src vector to the calling vector.
        The <see cref="TOpenCLBase.Length">Length</see> and <see cref="TOpenCLBase.Complex">Complex</see> properties
        of the calling vector are set implicitly. The phase parameter determines the initial sample offset.
        Phase must be less than Factor.
</remarks>


        <Example>
        <code>
        var a,b: TOpenCLVector;
        begin
          clMtxVec.CreateIt(a,b);
          try
            b.CopyFromArray(TSingleArray.Create(0,0,0,0,1,2,3,4,5,6));
            a.DownSample(b,2); // a = [0,0,1,3,5]
          finally
            clMtxVec.FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="UpSample"/>
        <SeeAlso cref="TOpenCLVector.UpSample"/>*)
      function DownSample(const Src: TOpenCLMtxVec; Factor: integer; Phase: integer = 0): TOpenCLVector; overload;

      (*<summary>Reverse all Vec elements.</summary>
        
<remarks>Store the result in the calling vector elements. The <see cref="TOpenCLBase.Length">Length</see>
        and <see cref="TOpenCLBase.Complex">Complex</see> roperties of the calling vector are
        set implicitly to match Vec vector.

        <IMG name="TVec24"/><para/>

        This overload reverses all vector elements.
</remarks>


        <Example>
        <code>
        var a: TOpenCLVector;
        begin
          CreateIt(a,b);
          try
            a.CopyFromArray(TSingleArray.Create(1,2,3,4));
            b.Reverse(a);   // b = [4,3,2,1]
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Rotate"/>
        <SeeAlso cref="Shift"/>*)
      function Reverse(const Vec: TOpenCLMtxVec): TOpenCLVector; overload;

      function Reverse: TOpenCLVector; overload;

      (*<summary>A cyclic shift on vector elements.</summary>
        
<remarks>Performs cyclic shift on vector elements. The number of elements to shift is specified in the Offset parameter. Offset can be
        any integer number, positive or negative.
</remarks>


        <Example>
        <code>
        var a: TOpenCLVector;
        begin
          CreateIt(a);
          try
            a.CopyFromArray(TSingleArray.Create(1,2,3,4));
            b.Rotate(a, 2);   // b = [3,4,1,2]
          finally
            FreeIt(a);
          end;
        end;
        </code>
        </Example>

        <SeeAlso cref="Reverse"/>
        <SeeAlso cref="Shift"/>*)
      function Rotate(const Src: TOpenCLMtxVec; Offset: integer): TOpenCLVector; overload;

      (*<summary>Shift vector elements left or right in the array.</summary>
        
<remarks>Shifts Vec vector elements and stores the result in the calling object.
        The number of elements to shift is specified in the Offset parameter.
        Offset can be any integer number, positive or negative.
</remarks>


        <SeeAlso cref="Rotate"/>*)
      function Shift(const Vec: TOpenCLMtxVec; Offset: integer): TOpenCLVector; overload;



        (*<summary>Converts the content of the calling vector to a list of strings.</summary>
        
<remarks>Converts all elements of the calling vector to strings with formating ReFormat for the real part and ImFormat for the imaginary part
        and stores them in aList, by using the Add method of TStringsobject. If vector is not complex only the ReFormat is used.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


        <Example>
        <code>
        procedure TForm1.Button1Click(Sender: TObject);
        var a,b: TOpenCLVector;
        begin
          clMtxVec.CreateIt(a,b);
          try
            a.SetIt(False,[1,2,3,4]);
            a.Cos;
            b.Size(a);
            b.SetVal(1);
            a.Add(b);
            Richedit1.Clear;
            Memo1.Clear;
            a.ValuesToStrings(Richedit1.Lines);
            b.ValuesToStrings(Richedit1.Lines);
            a.ValuesToStrings(Memo1.Lines);
            b.ValuesToStrings(Memo1.Lines);
            Memo1.Lines.SaveToFile('C:\Test.txt');
            Memo1.Lines.LoadFromFile('C:\Test.txt');
          finally
            FreeIt(a,b);
          end;
        end;
        </code>
        </Example>*)
      procedure ValuesToStrings(dstList: TStrings; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;
      (*<summary>Convert elements from Index to Index+Len-1 of the calling vector to strings.</summary>
        
<remarks>Use ReFormat for the real part and ImFormat for the imaginary part and store them in aList starting at ListIndex. If aList is not large enough,
        the method will use the add method of aList object. If vector is not complex,
        only the ReFormat is used.

        Performance note:
          This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for dstList. Use TStringList or StringList types and then
          call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>
*)
      procedure ValuesToStrings(dstList: TStrings; ListIndex, Index,  Len: integer; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;

      (*<summary>Convert all vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;
      (*<summary>Convert Index..Index+Len-1 vector elements to text.</summary>*)
      procedure ValuesToText(out Text: String; Index,  Len: integer; const ReFormat: string = ' 0.#####;-0.#####'; const ImFormat: string = '+0.#####i;-0.#####i'); overload;


    destructor Destroy; override;
    constructor Create(aDevice: TOpenCLDevice); override;
  end;

  

  TOpenCLMatrix = class(TOpenCLMtxVec )
  strict private
    FCols: integer;
    FRows: integer;
    SCols: integer;
    SRows: integer;
    procedure SetCols(const Value: integer);
    procedure SetRows(const Value: integer);
    
    function GetData: TMtx;
    
    function GetCols: integer;
    function GetRows: integer;
    strict protected
    procedure FreeToCache; override;
    procedure InitData; override;
    procedure SetLength(const Value: integer); override;
    procedure DoSetComplex(Value: boolean); override;
    
    property Data: TMtx read GetData;
    
    function GetRealValues: string; override;
    function GetComplexValues: string; override;
  protected
    function DebugUpdate: integer; override;
  public
    constructor Create(aDevice: TOpenCLDevice); override;

    property Rows: integer read GetRows write SetRows;
    property Cols: integer read GetCols write SetCols;
    function Size(aRows, aCols: integer; aPrecision: TclFloatPrecision; aComplex: boolean): TOpenCLMatrix; overload;
    function Size(const Src: TOpenCLBase;  aComplex: boolean): TOpenCLBase; overload; override;
    procedure Size(aLength: integer; aPrecision: TclFloatPrecision; aComplex: boolean); override;
    procedure Size(const Src: TOpenCLBase); override;
    procedure Reset; override;
    procedure SetFullRange; override;
    procedure SetSubRange(Index, Len: integer); overload; override;

    procedure Copy(const Src: TDenseMtxVec); overload; override;
    procedure Copy(const Src: TMtxVecInt); overload; override;

    function CopyFromArray(aRows, aCols: integer; const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(aRows, aCols: integer; const Src: TDoubleArray): TOpenCLMtxVec; overload;
    function CopyFromArray(aRows, aCols: integer; const Src: TCplxArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(aRows, aCols: integer; const Src: TSingleArray): TOpenCLMtxVec; overload;
    function CopyCplxFromArray(aRows, aCols: integer; const Src: TDoubleArray): TOpenCLMtxVec; overload;

    procedure Assign(Src: TOpenCLMatrix); virtual;

    (*<summary>The inverse of matrix elements.</summary>
      
<remarks>Calculates the inverse of all matrix elements in place. The computation occurs after first limiting the
      magnitude of each elements by the lower bound of Treshhold. The limiting operation is performed to avoid
      division by zero. Since Treshold represents a magnitude, it is always real and must always be positive.
      For complex versions, the magnitude of the input is limited, but the phase remains unchanged. Zero-valued
      input is assumed to have zero phase. To bypass the limiting operation set the Threshold to zero.
</remarks>


      <Example>
      <code>
      var  A: TOpenCLMatrix;
      begin
        clMtxVec.CreateIt(A);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4));  // 2x2, not complex matrix
          A.InvElem(1.0e-7);
        finally
          clMtxVec.FreeIt(A);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="MulElem"/>*)
    function InvElem(): TOpenCLMatrix; overload;
    function InvElem(const Src: TOpenCLMtxVec): TOpenCLMatrix; overload;

    (*<summary>Matrix array multiplication.</summary>
      
<remarks>Multiplies elements in Mtx matrix with the elements in the calling matrix (array multiplication) and stores the results in calling matrix.
      The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex" text="Complex"/> properties of both matrices must match, otherwise an exception is
      raised.
</remarks>


      <Example>
      <code>
      var  A,B,C: TOpenCLMatrix;
      begin
        clMtxVec.CreateIt(A,B,C);
        try
          A.SetIt(2,2,False,[1,2,
                             2,4));
          B.SetIt(2,2,False,[1,2,
                             2,4));
          C.MulElem(A,B);
          // C becomes:
          // [1, 4,
          //  4,16]
        finally
          clMtxVec.FreeIt(A,B,C);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="InvElem"/>*)
    function MulElem(Mtx: TOpenCLMatrix): TOpenCLMatrix; overload;
    (*<summary>Multiplies elements in Mtx1 matrix with the elements in Mtx2 matrix (array multiplication) and stores the results in calling matrix.</summary>
      
<remarks>The <see cref="Rows"/>, <see cref="Cols"/> and <see cref="TMtxVec.Complex" text="Complex"/> properties of calling matrix are set implicitly to match those
      of Mtx1 and Mtx2 matrices. Mtx1 and Mtx2 Rows, Cols, and Complex properties must be the same, otherwise an excetion is raised.
      raised.
</remarks>
*)
    function MulElem(Mtx1, Mtx2: TOpenCLMatrix): TOpenCLMatrix; overload;


    (*<summary>Converts the content of the matrix Values array to a list of strings.</summary>
      
<remarks>Convert all elements of the calling matrix to strings with  formating real parts with ReFormat, imaginary parts with ImFormat,
      using the text delimiter Delimiter and store them in aList, by using the Add method of TStrings object.
</remarks>


      <Example>
      <code>
      procedure TForm1.Button1Click(Sender: TObject);
      var a,b: TOpenCLMatrix;
      begin
        CreateIt(a,b);
        try
          a.SetIt(2,2,False,[1,2,3,4]);
          a.Cos;
          b.Size(a);
          b.SetVal(1);
          a.Add(b);
          Richedit1.Clear;
          Memo1.Clear;
          a.ValuesToStrings(Richedit1.Lines);
          b.ValuesToStrings(Richedit1.Lines);
          a.ValuesToStrings(Memo1.Lines);
          b.ValuesToStrings(Memo1.Lines);
          Memo1.Lines.SaveToFile('C:\Test.txt');
          Memo1.Lines.LoadFromFile('C:\Test.txt');
          a.StringsToValues(Memo1.Lines);
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>*)
    procedure ValuesToStrings(dstList: TStrings; const Delimiter: string = kTab;
                                               const ReFormat: string = ' 0.######;-0.#######';
                                               const ImFormat: string = '+0.######i;-0.######i'); overload;

    (*<summary>nvert calling matrix elements, starting with [Row,Col] and converting Len elements to strings.</summary>
      
<remarks>Vormating real parts with ReFormat, imaginary parts with ImFormat, using the text delimiter Delimiter and store them in aList starting at ListIndex.
      If aList is not large enough, the method will use the Add method of aList object.
</remarks>
*)
    procedure ValuesToStrings(dstList: TStrings; ListIndex,Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab;
                                     const ReFormat: string = ' 0.######;-0.######';
                                     const ImFormat: string = '+0.######i;-0.######i'); overload;



    (*<summary>Converts all calling matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Delimiter: string = kTab;
        const ReFormat: string = ' 0.######;-0.#######';
        const ImFormat: string = '+0.######i;-0.######i');overload;

    (*<summary>Converts Row..Col to Row+RowCount..Col+ColCount matrix elements to string.</summary>*)
    procedure ValuesToText(out Text: String; Row,Col,RowCount,ColCount: integer; Delimiter: string = kTab;
                                     const ReFormat: string = ' 0.######;-0.######';
                                     const ImFormat: string = '+0.######i;-0.######i'); overload;


    
    procedure CopyTo(Dst: TMtx); overload;
    procedure CopyTo(Dst: TMtxInt); overload;
    

    destructor Destroy; override;
  end;



  

  (*<summary>Stores a single real or complex value in GPU memory.</summary>
             
<remarks>Copying data from and to (from CPU separate) GPU memory can cause long delays even if that data is short.
             Sometimes it makes sense to keep even single value variables in the GPU memory. Use this object
             to avoid copying individual values from GPU to CPU memory.
</remarks>
*)

  TOpenCLValue =  class(TOpenCLBase )
  strict private
    function GetComplexValue: string;
    function GetRealValue: string;
    
    function GetData: TVec;
    
  strict protected
    
    property Data: TVec read GetData;
    
    function GetSData: PCL_mem; override;
    procedure InitData; override;
    procedure FreeToCache; override;

    function GetRealValues: string; override;
    function GetComplexValues: string; override;
  protected
     function DebugUpdate: integer; override;  
  public
    constructor Create(aDevice: TOpenCLDevice); override;
    procedure Reset; override;

    function Size(const Src: TOpenCLBase; aComplex: boolean): TOpenCLBase; overload; override;

    procedure Size(aPrecision: TclFloatPrecision; aComplex: boolean); overload;
    (*<summary>Returns Open CL buffer reference.</summary>
               
<remarks>Returns the real Open CL buffer used by the object. Multiple objects may be
               using the same buffer.
</remarks>
*)

    property SData: PCL_mem read GetSData; 
    (*<summary>Returns Open CL buffer reference.</summary>
               
<remarks>Returns complex Open CL buffer used by the object. Multiple objects may be
               using the same buffer.
</remarks>
*)

    property CData: PCL_mem read GetSData; 

    property RealValue: string read GetRealValue;
    property ComplexValue: string read GetComplexValue;

    (*<summary>Returns the index offest in to the buffer.</summary>
               
<remarks>Returns the index offset in to the buffer at which real values for the this object start.
</remarks>
*)
    function SDataIndex(aIndex: integer): PointerInteger; override;

    (*<summary>Returns the index offest in to the buffer.</summary>
               
<remarks>Returns the index offset in to the buffer at which complex values for the this object start.
</remarks>
*)
    function CDataIndex(aIndex: integer): PointerInteger; override;

    (*<summary>Copy value.</summary>
      
<remarks>Copy Src element to the calling object. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set implicitly to match Src object.
</remarks>


      <Example>
      <code>
      var a,b: TOpenCLValue;
      begin
          a.Copy(2);
          b.Copy(a);                // b = [1,2,3,4]
      end;
      </code>
      </Example>*)

    function Copy(const Src: TOpenCLValue): TOpenCLValue; overload;
    (*<summary>Copy Value to GPU memory.</summary>*)
    function Copy(const Value: double): TOpenCLValue; overload;
    (*<summary>Copy complex Value to GPU memory.</summary>*)
    function Copy(const Value: TCplx): TOpenCLValue; overload;

    (*<summary>Copies value from GPU memory.</summary>*)
    function Copy: double; overload;
    (*<summary>Copy complex value from GPU memory.</summary>*)
    function Copyc: TCplx; overload;

    (*<summary>Add Src1 and Src1.</summary>
      
<remarks>The results are stored in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object are set implicitly to match Vec1 and Vec2 vectors.
</remarks>
*)
    function Add(const Src1, Src2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Adds Value to Vec.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are set automatically.
</remarks>
*)
    function Add(const Vec: TOpenCLValue; Value: double): TOpenCLValue; overload;
    (*<summary>Adds complex Value to Vec.</summary>
      
<remarks>Store the result to the calling object. Size property of the calling object is set
      automatically. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function Add(const Vec: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Split complex calling object in real and imaginary part.</summary>
      
<remarks>Split calling object into real and imaginary components. Stores real component in ReVec and
      imaginary component in ImVec. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of ReVec and ImVec
      are set implicitly to match with the calling vector. An execption is raised, if calling object is not complex.
</remarks>


      <Example>
      <code>
      var a,b,c: clValue;
      begin
          a.Copy(Cplx(3,4));    // a= [1-2i, 3+4i]
          a.CplxToReal(b,c);  // b = 3, c = 4
      end;
      </code>
      </Example>

      <SeeAlso cref="RealToCplx"/>*)
    procedure CplxToReal(ReVec, ImVec: TOpenCLValue); overload;

    (*<summary>Constructs a complex object from two real objects.</summary>
      
<remarks>Construct a complex object from the ReVec (real part) and the ImVec (imaginary part) objects.
      The result is stored in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling
      object are set implicitly to match ReVec and ImVec objects. An exception is raised if ReVec or ImVec
      <see cref="TOpenCLBase.Complex">Complex</see> property is True.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.Copy(2);
            b.Copy(3);
            c.RealToCplx(a,b);  //c = 2+3i
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CplxToReal"/>*)
    function RealToCplx(ReVec, ImVec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Conjugate.</summary>
      
<remarks>Complex conjugate Vec value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a;
          clMtxVec.CreateIt(out c, out d);
          try
          {
            d.Copy(Cplx(2,3));
            c.Conj(d);
          }
          finally
          {
            clMtxVec.FreeIt(ref c, ref d);
          }
        }
      }
      </code></example>*)

    function Conj(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sets angle in [-2PI,2PI].</summary>
     <returns>ThetaRad within -2<see cref="Math387.PI"/> and <see cref="Math387.PI"/> interval.</returns>
     
<remarks>Calling this function prior to passing the value to trigonometric functions can significantly improve numerical accuracy.

     Sine/cosine appear within many other functions especially
     complex versions of trigonometric functions. FixAngle method is not used
     implicitely within TOpenCLValue methods. To achieve maximum performance make
     sure that the arguments passed to complex trigonometric functions are "small" or scaled down.

     Note
      The vector must be real.
</remarks>
*)
    function FixAngle(const Src: TOpenCLValue): TOpenCLValue;  overload;

    (*<summary>Rounds Src towards negative infinity and stores the result in the calling object.</summary>
      
<remarks>FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
</remarks>
*)
    function Floor(const Src: TOpenCLValue): TOpenCLValue;  overload;


    (*<summary>A complex exponential <c>e^(j*Omega))</c>.</summary>
      
<remarks>Calculate complex exponential. An exception is raised if
      calling object is complex. If object is complex, you should use the <see cref="Exp"/> method instead.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2);
            a.Expj(b);  // a = e^(2i)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)

    function Expj(Omega: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Fractional part of values.</summary>
      
<remarks>Calculates the fractional part of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.Frac(b);  // a = 0.3
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Trunc"/>
      <SeeAlso cref="Round"/>*)
    function Frac(const X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>Complementary error function.</summary>
      
<remarks>Calculates the complementary error function value of Src.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.Erfc(b);  // a = Erfc(2.3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erfc(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Error function.</summary>
      
<remarks>Calculates the error function of Src.
</remarks>


       

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.Erf(b);  // a = Erf(2.3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ErfInv"/>
      <SeeAlso cref="Erfc"/>*)
    function Erf(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Flips the real and imaginary part of a complex number.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b + i*a</c><para/>
      Method flips the real and imaginary part of the complex number X and stores the result in the calling object.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function Flip(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Flips the real and imaginary part of the complex numbers and conjugates the result.</summary>
      
<remarks>Performs the following transformation:

      <c>a + i*bi ==> b - i*a</c><para/>
      Method flips the real and imaginary part and conjugates calling object complex value.
</remarks>


      <SeeAlso cref="Flip"/>
      <SeeAlso cref="Conj"/>*)
    function FlipConj(const X: TOpenCLValue): TOpenCLValue; overload;


    (*<summary>The Reminder after division X/Y.</summary>
      
<remarks>Calculates reminder after division according to formula:

      <c>x[i]-y[i]*Trunc(const X[i]/y[i]).</c><para/>
      The result will be saved to the calling vector.
      X and Y must be a real and have the same length. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling vector are set implicitly to match the X object.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            a.Copy(4);
            b.Copy(3);
            c.Rem(a,b); // c = Rem(4,3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a,ref b,ref c);
          }
        }
      }
      </code></example>*)
    function Rem(const X, Y: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Log base N.</summary>
      <returns>returns Log with base N for value X.</returns>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.LogN(4, b);  // a = LogN(4, 2.3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function LogN(N: double; X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>Raises base to any power.</summary>
    
<remarks>Computes Base^(Exponent). The <see cref="IntPower"/> is faster, if Exponent is an integer.
    Real valued power can handle only positive Exponent. <see cref="IntPower"/> can handle negative exponent also.
    To compute a power to the negative exponent in general case or when the base is negative,
    use the complex version of the function.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
            b.Copy(2.3);
            c.Copy(4);
            a.Power(b, c);  // a = b^c
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="IntPower"/>*)

    function Power(Base, Exponent: TOpenCLValue): TOpenCLValue; overload;

    function Power(Base: double; Exponent: TOpenCLValue): TOpenCLValue; overload;
    function Power(Base: TCplx; Exponent: TOpenCLValue): TOpenCLValue; overload;

    function Power(Base: TOpenCLValue; Exponent: double): TOpenCLValue; overload;
    function Power(Base: TOpenCLValue; Exponent: TCplx): TOpenCLValue; overload;

    (*<summary>Round Src.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of
      calling object are adjusted automatically.
</remarks>
*)
    function Round(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary> Computes signum function from Src and stores the result in the calling object.  </summary>
                 
<remarks>FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
                 Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
</remarks>
*)
    function Sgn(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Signum.</summary>
      
<remarks>Calculates the signum of Src and multiplies it with the calling object value.
      Signum(X) is 1 for X &gt; 0 , equal to zero for X = 0  and  -1 for X &lt; 0.
      FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function SgnMul(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sine and cosine.</summary>
      
<remarks>Calculates the sine and cosine of the value stored in the calling object.
      FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of SinX and CosX are adjusted automatically.

      Note
        Use this method if you require both sine and cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,s,v;
          MtxVec.CreateIt(out a, out s, out c);
          try
          {
            a.Copy(0);
            a.SinCos(s,c); // s=[0], c =[1]
          }
          finally
          {
            MtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>
      <SeeAlso cref="Cos"/>*)
    procedure SinCos(SinX, CosX: TOpenCLValue); overload; 

    (*<summary>Hyperbolic sine and cosine.</summary>
      
<remarks>Calculates the hyperbolic sine and hyperbolic cosine for the calling object value and stores
      the sine to SinhX and cosine to CoshX. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of SinhX and CoshX
      are adjusted automatically.

      Note
        Use this method, if you require hyperbolic sine and hyperbolic cosine.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,s,v;
          clMtxVec.CreateIt(out a, out s, out c);
          try
          {
          a.Copy(0);
            a.SinhCosh(s,c);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref s, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sinh"/>
      <SeeAlso cref="Cosh"/>*)
    procedure SinhCosh(SinhX, CoshX: TOpenCLValue); overload;

   (*<summary>Threshold bottom operation.</summary>
      
<remarks>Perform threshold operation on Src. The Value parameter is a lower bound for the
      threshold operation. Src value smaller than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a, b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(0.1);
            a.ThreshBottom(b, 0.2); // a = [0.2]
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)

    function ThreshBottom(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Threshold top operation.</summary>
      
<remarks>Perform threshold operation Src. The Value parameter is an <b>upper</b> bound for threshold operation.
      Src value bigger than Value will be replaced with Value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a, b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(0.1);
            a.ThreshTop(b, 0.2); // a = [0.2]
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThreshTop"/>*)

    function ThreshTop(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Threshold less than operation.</summary>
      
<remarks>Perform operation on Vec. The LTValue parameter is an <b>lower</b> bound for
      threshold operation. Vec value less than LTLevel will be replaced with LTValue.
      For a complex number the comparison is applied with the norm of the complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a, b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            a.CopyFromArray( new double[] {2, 0.1, 3, 4});
            a.ThresholdLT(b, 2.3,1.5); // a = [1.5]
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdGT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdLT(const Vec: TOpenCLValue; LTLevel, LTValue: double): TOpenCLValue; overload;

    (*<summary> Perform "less than" threshold operation for complex value in Vec. </summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are adjusted automatically.
      If Vec ovalue is not complex, an exception will be raised.
</remarks>
*)
    function ThresholdLT(const Vec: TOpenCLValue; LTLevel: double; LTValue: TCplx): TOpenCLValue; overload;

    (*<summary>Threshold greater than operation.</summary>
      
<remarks>Perform operation on Vec value. The GTValue parameter is an <b>upper</b> bound for threshold operation.
      Vec value bigger than LTLevel will be replaced with GTValue.
      For a complex number the comparison will be applied with the norm of the complex value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a, b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2);
            a.ThresholdGT(b, 1.3,1.5); // a = [1.5]
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT_LT"/>*)
    function ThresholdGT(const Vec: TOpenCLValue; GTLevel, GTValue: double): TOpenCLValue; overload;

    (*<summary>Perform "greater than" threshold operation for complex value in Vec.</summary>
      
<remarks>Store the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of the calling object are adjusted automatically. If Vec value is not complex, an exception will be raised.
</remarks>
*)
    function ThresholdGT(const Vec: TOpenCLValue; GTLevel: double; GTValue: TCplx): TOpenCLValue; overload;

    (*<summary>Threshold greater than and less than operation.</summary>
      
<remarks>Perform threshold operation on Vec value. The LTValue parameter is an <b>lower</b> bound for threshold operation.
      The GTValue parameter is an <b>upper</b> bound for threshold operation.
      Vec value less than LTLevel will be replaced with LTValue. Vec value bigger than GTLevel will be replaced with GTValue.
      Operation is available only for not complex values.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a, b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(3);
              a.ThresholdGT_LT(b, 2.3,3.4,1,0.5); // a = [3.4]
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ThresholdLT"/>
      <SeeAlso cref="ThresholdGT"/>*)
    function ThresholdGT_LT (const Vec: TOpenCLValue; GTLevel, GTValue, LTLevel, LTValue: double): TOpenCLValue; overload;

    (*<summary>Rounds a real number towards zero and returns the fractional part.</summary>
      
<remarks>Rounds calling object value towards zero to an integer and stores
      the result in the TruncDst object as a floating point numbers. The fractional
      part is stored in the FracDst.
</remarks>


      <SeeAlso cref="Frac"/>
      <SeeAlso cref="Round"/>*)

    procedure TruncAndFrac(TruncDst: TOpenCLValue; FracDst: TOpenCLValue); overload;

    (*<summary>Truncate all Src object elements.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see>
      property of calling object are adjusted automatically.
</remarks>
*)
    function Trunc(const Src: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse of cube root 1/(v)^1/3.</summary>
      
<remarks>Calculate the inverse cube root <c>(1/(element)^(1/3))</c> of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.InvCbrt(b);  // a = 2.3^(1/3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cbrt"/>*)

    function InvCbrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse of square root 1/(v)^1/2.</summary>
      
<remarks>Calculate the inverse square root <c>1/(element)^(1/2))</c> of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(2.3);
            a.InvCbrt(b);  // a = 2.3^(1/3)
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>*)
    function InvSqrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Magnitude.</summary>
      
<remarks>Calculate the magnitude of X.
      This method has the same function as the <see cref="Abs"/> method.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
            b.Copy(Cplx(2,3));
            a.Mag(b);
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Abs"/>*)
    function Mag(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Multiply Src1 with Src2.</summary>
      
<remarks>Stores the result in the calling object.
      FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically to match those of Vec1 and Vec2.
      An exception is raised if Vec1 and Vec2 FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property do not match.
</remarks>
*)
    function Mul(const Src1, Src2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Multiply Vec with Value.</summary>
      
<remarks>Stores the result in to the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLValue; Value: double): TOpenCLValue; overload;
    (*<summary>Multiply Vec with Value.</summary>
      
<remarks>Stores the result in to the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see>
      properties of the calling object are adjusted automatically.
</remarks>
*)
    function Mul(const Vec: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Multiply X with 1i and store the result in to the calling object.</summary>*)
    function MulI(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Converts the polar magnitude/phase pair to cartesian pair.</summary>
      
<remarks>Converts AmpltVec and PhaseVec values (combined) from polar to cartesian form.
      The result is stored as a complex number (x=Re, y=Im) in to the calling
      object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set
      implicitly to match AmpltVec and PhaseVec objects.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b,c;
          clMtxVec.CreateIt(out a, out b, out c);
          try
          {
              a.Copy(2);  // a = [2] //magnitude
              b.Copy(3); // b = [1] /phase
              c.PolarToCart(a,b); // result stored in c = projections to Re and Im axis
          }
          finally
          {
            clMtxVec.FreeIt(ref a, ref b, ref c);
          }
        }
      }
      </code></example>

      <SeeAlso cref="CartToPolar"/>*)
    function PolarToCart(AmpltVec, PhaseVec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Gets real part of the complex value.</summary>
      
<remarks>The method gets the real part of the complex value stored in Vec and stores the real results in the calling
      object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="TOpenCLBase.Complex">Complex</see> property must be true otherwise an exception is raised.
</remarks>


      <Example>
      <code>
      var a,b: TOpenCLValue;
      begin
        CreateIt(a,b);
        try
          a.Copy(Cplx(2,3)); // = [2+3i]
          b.RealPart(a); // b = [2]
        finally
          FreeIt(a,b);
        end;
      end;
      </code>
      </Example>

      <SeeAlso cref="ImagPart"/>*)
    function RealPart(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Square.</summary>
      
<remarks>Calculate the square of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(2);
              a.Sqr(b); // a=[4]
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqrt"/>
      <SeeAlso cref="Power"/>*)
    function Sqr(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Square root.</summary>
      
<remarks>Calculate the square root of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Sqrt(b); // a=[2]
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sqr"/>*)
    function Sqrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Sine function.</summary>
      
<remarks>Calculate the sine of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Sin(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Sin(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Cosne function.</summary>
      
<remarks>Calculate the cosine of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Cos(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSin"/>
      <SeeAlso cref="SinCos"/>*)

    function Cos(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Tangens.</summary>
      
<remarks>Calculate the tangens of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Tan(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTan"/>
      <SeeAlso cref="ArcTan2"/>*)

    function Tan(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Cotangens.</summary>
      
<remarks>Calculate the cotangens of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Cot(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>*)

    function Cot(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Secant.</summary>
      
<remarks>Calculate the secant of X.
</remarks>

      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Sec(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSec"/>
      <SeeAlso cref="Csc"/>*)

    function Sec(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Cosecant.</summary>
      
<remarks>Calculate the cosecant of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.Csc(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCsc"/>
      <SeeAlso cref="Sec"/>*)

    function Csc(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse sine.</summary>
      
<remarks>Calculate the inverse sine of X. Value must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.ArcSin(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Sin"/>*)

    function ArcSin(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>The inverse cosine.</summary>
      
<remarks>Calculate the inverse cosine X. Value must be between -1 and 1.
      The return values will be in the range [0,<see cref="Math387.PI"/>], in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(4);
              a.ArcCos(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cos"/>*)
    function ArcCos(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse tangens of Y/X.</summary>
      
<remarks><para/>Calculates the inverse tangens of Y/X, and returns an angle in the correct quadrant. The results are stored in
      calling object elements. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically
      to match those of X and Y objects. An exception is raised if X and Y FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties do not match.

      Note that <see cref="ArcTan"/> is calculated as ArcTan2(1, X).
</remarks>


      <SeeAlso cref="ArcTan"/>*)
    function ArcTan2(Y, X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse tangens.</summary>
      
<remarks>Calculate the inverse tangens for all calling object elements in-place. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.ArcTan(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>      

      <SeeAlso cref="Tan"/>
      <SeeAlso cref="ArcCot"/>
      <SeeAlso cref="ArcTan2"/>*)
    function ArcTan(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Inverse cotangens.</summary>
      
<remarks>Calculate the inverse cotangens of X. The return values are expressed in radians.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.ArcCot(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Cot"/>
      <SeeAlso cref="ArcTan"/>*)
    function ArcCot(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse secant of X. </summary>
      
<remarks>FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSec(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse cosecant of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)

    function ArcCsc(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic sine.</summary>
      
<remarks>Calculate the hyperbolic sine of X.
</remarks>


       

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.Sinh(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcSinh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Sinh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic cosine.</summary>
      
<remarks>Calculate the hyperbolic cosine of X.
</remarks>


       

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.Cosh(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcCosh"/>
      <SeeAlso cref="SinhCosh"/>*)
    function Cosh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic tangens.</summary>
      
<remarks>Calculate the hyperbolic tangens of X.
</remarks>


       

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.Tanh(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)

    function Tanh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Hyperbolic cotangens.</summary>
      
<remarks>Calculate the hyperbolic cotangens of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(0.5);
              a.Coth(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="ArcTanh"/>
      <SeeAlso cref="Coth"/>*)
    function Coth(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the hyperbolic secant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Sech(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the hyperbolic cosecant for all X object elements.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function Csch(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Absolute values.</summary>
      
<remarks>Calculate the absolute value.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(-0.5);
              a.Abs(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Mag"/>*)
    function Abs(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic sine of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties
      of calling object are adjusted automatically.
</remarks>
*)
    function ArcSinh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic cosine of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of
      calling object are adjusted automatically.
</remarks>
*)
    function ArcCosh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic tangens of X.</summary>
      
<remarks>Store the results in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcTanh(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverser hyperbolic cotangens of X.</summary>
      
<remarks>Store the results in calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCoth(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic secant of X.</summary>
      
<remarks>Store the results in calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcSech(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Calculate the inverse hyperbolic cosecant of X.</summary>
      
<remarks>Store the results in calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are adjusted automatically.
</remarks>
*)
    function ArcCsch(const X: TOpenCLValue): TOpenCLValue; overload;

   (*<summary>The cube root.</summary>
      
<remarks>Calculate the cube root of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Cbrt(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="InvCbrt"/>*)
    function Cbrt(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Rounds towards positive infinity.</summary>
      
<remarks>Rounds towards positive infinity in-place.

      <c>Ceil(-2.8) = -2</c><para/>
      <c>Ceil(2.8) = 3</c><para/>
      <c>Ceil(-1.0) = -1</c><para/>
</remarks>
*)
    function Ceil(const Src: TOpenCLValue): TOpenCLValue;  overload;

    (*<summary>Natural logarithm.</summary>
      
<remarks>Calculate the natural log of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Ln(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp"/>*)
    function Ln(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Log base 10.</summary>
      
<remarks>Calculate the log base 10 of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Log10(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp10"/>*)

    function Log10(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Log base 2.</summary>
      
<remarks>Calculate the log base 2 of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Log2(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Exp2"/>*)
    function Log2(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent (e^).</summary>
      
<remarks>Calculate the exponent (e^) of X.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Exp(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Ln"/>*)

    function Exp(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent base 2 (2^).</summary>
      
<remarks>Calculate the exponent base 2 of X ( 2^X).
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Exp2(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log2"/>*)
    function Exp2(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Exponent base 10 (10^X).</summary>
      
<remarks>Calculate the exponent base 10 (10^X).
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Exp10(b);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Log10"/>*)
    function Exp10(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Gets the imaginary part of a complex value.</summary>
      
<remarks>Gets the imaginary part of a complex value Vec and stores the real result in the calling object.
      FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set implicitly to match
      Vec object. Vec <see cref="TOpenCLBase.Complex">Complex</see> must be true otherwise the exception will be raised.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(Cplx(8));
              a.ImagPart(b);  // a = 0
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="RealPart"/>
      <SeeAlso cref="RealToCplx"/>*)
    function ImagPart(const Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Power (integer exponent).</summary>
      
<remarks>Calculate the power Base^(Exponent). For non-integer exponents, the <see cref="Power"/>
      method can be used.
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.IntPower(b,2);
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Power"/>*)
    function IntPower(Base: TOpenCLValue; Exponent: Integer): TOpenCLValue; overload;

    (*<summary>Inverse elements.</summary>
      
<remarks>Calculates the inverse of X without limiting the inverse operation (to protect against overflow).
</remarks>


      

      <example>
      <code>
      using Dew.Math;
      using Dew.Math.Units;

      namespace Dew.Examples()
      {
        void Example()
        {
          TOpenCLValue a,b;
          clMtxVec.CreateIt(out a, out b);
          try
          {
              b.Copy(8);
              a.Inv(b);  // a = 1/8
          }
          finally
          {
              clMtxVec.FreeIt(ref a, ref b);
          }
        }
      }
      </code></example>

      <SeeAlso cref="Divide"/>*)
    function Inv(const X: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Converts value from cartesian to polar coordinate form.</summary>
      
<remarks>Converts value from cartesian to polar coordinate form, storing the magnitude (radius)
      component in the AmpltVec and the phase (angle) in the PhaseVec.
      FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of AmpltVec and PhaseVec are set automatically.
</remarks>


      <SeeAlso cref="PolarToCart"/>*)
    procedure CartToPolar(AmpltVec, PhaseVec: TOpenCLValue); overload;

    (*<summary>Divide Num with Den.</summary>
      
<remarks>FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object are set automatically.
      The result is stored in the calling object.

      The result of division by zero will be the INF constant. Division of zero
      by zero will result in NAN.
</remarks>
*)
    function Divide(Num, Den: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Divide Value with Vec.</summary>
      
<remarks>FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of the calling object are set automatically.
</remarks>
*)
    function DivideBy(Value: double; Vec: TOpenCLValue): TOpenCLValue; overload;
    (*<summary>Divide Value with Vec.</summary>
      
<remarks>Size of the calling object is set automatically. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function DivideBy(Value: TCplx; Vec: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Subtract real Value from Src.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLValue; Value: double): TOpenCLValue; overload;

    (*<summary>Subtract complex Value from Src.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are adjusted automatically.
</remarks>
*)
    function Sub(const Src: TOpenCLValue; Value: TCplx): TOpenCLValue; overload;

    (*<summary>Subtract Vec2 elements from Vec1 elements.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property of calling object are
      adjusted automatically. An exception is raised, if Vec1 and Vec2 FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> property do not match.
</remarks>
*)
    function Sub(Vec1, Vec2: TOpenCLValue): TOpenCLValue; overload;

    (*<summary>Substract Vec elements from Value.</summary>
      
<remarks>Stores the result in the calling object. FloatPrecision and <see cref="TOpenCLBase.Complex">Complex</see> properties of calling object are
      adjusted automatically.
</remarks>
*)
    function SubFrom(Value: double; Vec: TOpenCLValue): TOpenCLValue; overload;
    (*<summary>Substract Vec from Value.</summary>
      
<remarks>Stores the result in the calling object. Size property of the calling object is set
      automatically. <see cref="TOpenCLBase.Complex">Complex</see> property of the calling object is set to True.
</remarks>
*)
    function SubFrom(Value: TCplx; Vec: TOpenCLValue): TOpenCLValue; overload;

    destructor Destroy; override;
  end;



  TOpenCLVectorArray = array of TOpenCLVector;
  TOpenCLMatrixArray = array of TOpenCLMatrix;
  TOpenCLValueArray = array of TOpenCLValue;

  TOpenCLCache = class
  strict private
    fBlockLength: integer;
    blockPool: TIntegerArray;
    fScalarBlockLength: integer;
    ScalarBlockPool: TIntegerArray;
    clVectors: TOpenCLVectorArray;
    clMatrices: TOpenCLMatrixArray;
    clScalars: TOpenCLValueArray;

    FMatricesUsed: integer;
    FVectorsUsed: integer;
    FScalarsUsed: integer;

    fMaxVectorsUsed: integer;
    fMaxMatricesUsed: integer;
    fMaxScalarsUsed: integer;

    
    fDevice: TOpenCLDevice;

    
    csv: TCriticalSection;
    csm: TCriticalSection;

    

    function GetFreeBlock: integer;
    function GetFreeScalar: integer;
    procedure ReleaseBlock(var BlockId: integer);
    procedure ReleaseScalar(var ScalarId: integer);

    procedure InternalMatrixCacheSize;
    procedure InternalMatrixCacheSize2;
    procedure InternalScalarCacheSize;
    procedure InternalScalarCacheSize2;
    procedure InternalVectorCacheSize;
    procedure InternalVectorCacheSize2;
  strict protected
    function ScalarLength(aPrecision: TclFloatPrecision): integer;
    procedure mEnter;
    procedure mLeave;
    procedure sEnter;
    procedure sLeave;
    procedure vEnter;
    procedure vLeave;
  protected
    clMemory: pcl_mem;
    clScalarMemory: pcl_mem;
    procedure SetBlockPoolSize(ItemCount, ItemLength: integer);
    procedure SetVecCacheSize(ItemCount: integer);
    procedure SetMatrixCacheSize(ItemCount: integer);
    procedure SetScalarCacheSize(ItemCount: integer);
    procedure SetScalarPoolSize;
    function BlockLength(aPrecision: TclFloatPrecision): integer;
  public
    procedure SetCacheSize(ItemCount, ItemLength, VecCount, MtxCount: integer; ScalarCount: integer = 32);

    procedure CreateIt(out a: TOpenCLVector); overload;
    procedure FreeIt(var a: TOpenCLVector); overload;
    procedure CreateIt(out a: TOpenCLMatrix); overload;
    procedure FreeIt(var a: TOpenCLMatrix); overload;
    procedure CreateIt(out a: TOpenCLValue); overload;
    procedure FreeIt(var a: TOpenCLValue); overload;

    property MatricesUsed: integer read FMatricesUsed;
    property VectorsUsed: integer read FVectorsUsed;
    property ScalarssUsed: integer read FScalarsUsed;

    property MaxVectorsUsed: integer read fMaxVectorsUsed;
    property MaxMatricesUsed: integer read fMaxMatricesUsed;
    property MaxScalarsUsed: integer read fMaxScalarsUsed;

    constructor Create(aDevice: TOpenCLDevice); virtual;
    destructor Destroy; override;
  end;

    (*<summary>Redirects the execution path to the command que to one on the same device with support for immediate execution.</summary>
               
<remarks>This feature is supported only for CPU devices and for now only by Intel.
               The function returns true, if a command queue was marked with ImmediateExecution property set to true.
</remarks>
*)
    function EnableCPUThreading: boolean;
    (*<summary>Redirects the execution path to the command que to one on the same device but without support for immediate execution.</summary>
               
<remarks>This feature is supported only for CPU devices and for now only by Intel.
               The function returns true, if a command queue was marked with ImmediateExecution property set to false.
</remarks>
*)
    function DisableCPUThreading: boolean;


    procedure clCheck(Status: cl_int);
    function GetCurrentCLDevice: TOpenCLDevice;
    function GetCurrentCommandQueue: TOpenCLCommandQueue;

    procedure CreateIt(out a: TOpenCLVector); overload;
    procedure CreateIt(out a: TOpenCLMatrix); overload;
    procedure CreateIt(out a, b: TOpenCLVector); overload;
    procedure CreateIt(out a, b: TOpenCLMatrix); overload;
    procedure CreateIt(out a, b, c, d: TOpenCLVector); overload;
    procedure CreateIt(out a, b, c, d: TOpenCLMatrix); overload;

    procedure FreeIt(var a: TOpenCLVector); overload;
    procedure FreeIt(var a: TOpenCLMatrix); overload;
    procedure FreeIt(var a, b: TOpenCLVector); overload;
    procedure FreeIt(var a, b, c, d: TOpenCLVector); overload;
    procedure FreeIt(var a, b: TOpenCLMatrix); overload;
    procedure FreeIt(var a, b, c, d: TOpenCLMatrix); overload;

    

     function clPlatform: TOpenCLPlatformList; overload;
 








