











(*<summary>Implements the sparse matrix class.</summary>
  
<remarks>This unit implements the <see cref="TSparseMtx"/> class.
</remarks>
*)
unit Sparse;


{$I BdsppDefs.inc}

interface

uses AbstractMtxVec, MtxVec, Math387, MtxVecBase

     
     ,Ippspl
     

     
     ,IppsplSingle
     

     ,lapack_umfpack

     

     

    
      ,SysUtils, Classes
    
     ;




    {$HPPEMIT END '#include "Sparse.h"'}

    {$HPPEMIT '#include <memory>'}
    {$HPPEMIT '#include <string>'}


(*<summary>Defines the system to be solved by the <see cref="TSparseMtx.Solve"/> method.</summary>
  
<remarks>Defines the system to be solved by the <see cref="TSparseMtx.Solve"/> method.
  In the declaration bellow L is lower triangular matrix, U is uper triangular matrix, P and Q are the permutation
  matrices and T denotes tranpose operation. <c>PAQ = LU</c>. Applies to Umfpack only.
</remarks>
*)
type TSparseSystem = (
  (*<summary><c>Ax=b</c></summary>*)spsA = UMFPACK_A,
  (*<summary><c>A'x=b</c></summary>*)spsAT = UMFPACK_At,
  (*<summary><c>A.'x=b</c></summary>*)spsAat = UMFPACK_Aat,
  (*<summary><c>P'Lx=b</c></summary>*)spsPtL = UMFPACK_Pt_L,
  (*<summary><c>Lx=b</c></summary>*)spsL = UMFPACK_L,
  (*<summary><c>L'Px=b</c></summary>*)spsLTP = UMFPACK_Lt_P,
  (*<summary><c>L.'Px=b</c></summary>*)spsLaTP = UMFPACK_Lat_P,
  (*<summary><c>L'x=b</c></summary>*)spsLT = UMFPACK_Lt,
  (*<summary><c>L.'x=b</c></summary>*)spsLaT = UMFPACK_Lat,
  (*<summary><c>UQ'x=b</c></summary>*)spsUQT = UMFPACK_U_Qt,
  (*<summary><c>Ux=b</c></summary>*)spsU = UMFPACK_U,
  (*<summary><c>QU'x=b</c></summary>*)spsQUT = UMFPACK_Q_Ut,
  (*<summary><c>QU.'x=b</c></summary>*)spsQUaT = UMFPACK_Q_Uat,
  (*<summary><c>U'x=b</c></summary>*)spsUT = UMFPACK_Ut,
  (*<summary><c>U.'x=b</c></summary>*)spsUaT = UMFPACK_Uat
  );

     (*<summary>Defines sparse matrix file format.</summary>
       
<remarks>Note:
        Currently only Matrix Market file format is supported.
</remarks>
*)
     TSparseFormat = (
      
      
      (*<summary>Matrix Market file format.</summary>*)spfMatrixMarket);

     (*<summary>Defines how to solve the matrix using the Solve method.</summary>*)
     TSparsePattern = (
      (*<summary>The matrix pattern will be analyzed, the upper triangle matrix will be
        formed and the system will be solved. In this case the pattern of matrix changes with each iteration.</summary>*)
      sppNone,
      (*<summary>The matrix pattern will be analyzed only with the first call to the <see cref="TSparseMtx.Solve"/>
        method. In this case the pattern of the matrix is fixed, but the values can change between
        consecutive calls to the Solve method. To reset the stored pattern of the matrix and prepare
        for a new pattern, call the FreeSymbolic method.</summary>*)
      sppSymbolic,
      (*<summary></summary>*)
      sppNumericIterative,
      (*<summary>The matrix pattern and the values of the matrix will remain fixed. The <see cref="TSparseMtx.Solve"/>
       method will only call the routine to solve the upper triangular matrix. The pattern analysis and
       matrix factorization will be performed only on the first call to the Solve method. To reset the
       matrix factorization call the <see cref="TSparseMtx.FreeNumeric"/> method. To also reset the matrix pattern,
       call the <see cref="TSparseMtx.FreeSymbolic"/> method after <see crf="TSparseMtx.FreeNumeric"/>.
       Both Symbolic and Numeric states are freed automatically when the <see cref="TSparseMtx.SparsePattern"/> property is changed to sppNone.</summary>*)
      sppNumeric
      );

     (*<summary>Defines which method will be used to solve system of equations.</summary>*)
     TSparseSolver = (
      (*<summary>Use one of the iterative methods.</summary>*)ssIterative,
      (*<summary>Use UMF Pack library.</summary>*)ssUmfPack,
      (*<summary>Use the Pardiso library.</summary>*)ssPardiso,
      (*<summary>Use the TAUCS library.</summary>*)ssTaucs);

     (*<summary>The method for iterative solver.</summary>*)
     TIterativeMethod = (
      (*<summary>Gauss-Seidel method iterative sparse <c>Ax=b</c> solver.</summary>*)itmSGS,
      (*<summary>Solve a symmetric positive definite linear system <c>Ax=b</c>  using the incomplete Cholesky preconditioned conjugate
        gradient method.</summary>*)
      itmICCG,
      (*<summary>Solve a general linear system <c>Ax=b</c> using the incomplete <c>LU</c> decomposition with iterative refinement.</summary>*)
      itmILUR,
      (*<summary>Solve a general linear system  <c>Ax=b</c>  using Jacobi iteration.</summary>*)
      itmJAC,
      (*<summary>Solve a linear system <c>Ax=b</c> using the BiConjugate gradient method with incomplete <c>LU</c> decomposition preconditioning.</summary>*)
      itmLUBC,
      (*<summary>Solve a general linear system <c>Ax=b</c> using the incomplete  LU decomposition with the Conjugate Gradient method applied to the normal equations, viz.,
        <c>AA'y=b, x=A'y.</c></summary>*)
      itmSLUCN,
      (*<summary>Solve a linear system <c>Ax=b</c> using the BiConjugate Gradient method with incomplete <c>LU</c> decomposition pre conditioning.</summary>*)
      itmLUCS,
      (*<summary>This routine uses the generalized minimum residual (GMRES) method with pre conditioning to solve non-symmetric linear systems of
        the form: <c>A*x=b</c>.</summary>*)
      itmLUGMRES,
      (*<summary>Solve a general linear system  <c>A*x=b</c> using the preconditioned orthomin method.</summary>*)
      itmLUOM
      );

     (*<summary>The detail of the report generated by <see cref="TSparseMtx.Solve"/> method.</summary>
      
<remarks>Note:
        Used only by UMFPack solver.
</remarks>
*)
     TReportLevel = (
      (*<summary>Leave everything out of report.</summary>*)prlNone,
      (*<summary>Include only errors in the report.</summary>*)prlErrorsOnly,
      (*<summary></summary>*)prlBasic,
      (*<summary></summary>*)prlExtensive,
      (*<summary></summary>*)prlComplete,
      (*<summary>Include full report.</summary>*)prlAll
      );

     (*<summary>The criteria for convergence check.</summary>*)
     TConvergenceCheck = (
      (*<summary>Iteration stops when the 2-norm of the residual divided by the 2-norm of the right-hand side is less than
        the value of the Tolerance property.</summary>*)
      cvcNorm2,
      (*<summary>Iteration stops when the 2-norm of M-inv times the residual divided by the 2-norm of M-inv times the
        right hand side is less than then value of Tolerance property, where M-inv is the inverse of the diagonal of A.</summary>*)
        cvcNorm2InvA
      );



     (*<summary>The reordering method for Pardiso solver.</summary>*)
     TPardisoReordering = (
      (*<summary></summary>*)prMinimumDegree,
      (*<summary>Use METIS nested dissection method.</summary>*)prMETIS,
      (*<summary>Parallel nested dissection.</summary>*) prParallel
      );

     (*<summary>Iterative solution refinment for Pardiso solver.</summary>*)
     TPardisoIterative = (
     (*<summary>The factorization is always computed as required by "phase".</summary>*)piNone,
     (*<summary>CGS iteration replaces the computation of LU.</summary>*)piCG,
     (*<summary>CGS iteration for symmetric positive definite matrices replaces the computation of LU</summary>*)piCGS
     );

     (*<summary>Parallel factorization control.</summary>*)
     TParallelFactorization = (
     (*<summary>Classic algorithm.</summary>*)pfClassic,
     (*<summary>Two-level factorization algorithm. Usefull when using more than 8 threads.</summary>*)pfTwoLevel
     );

     (*<summary>Parallel solve control.</summary>*)
     TParallelForwardBackward = (
     (*<summary>Sequential forward and backward solve.</summary>*) pfbSequential,
     (*<summary>Parallel solve step for in-core variant.</summary>*) pfbParallelIncore,
     (*<summary>Parallel forward and backward substition.</summary>*) pfbParallel
     );


     (*<summary>Out-of core sparse matrix solver.</summary>
      
<remarks>The OOC PARDISO can solve very large problems by holding the
      matrix factors in files on the disk. Because of that the
      amount of main memory required by OOC PARDISO is significantly reduced. By default,
      the OOC PARDISO uses the current directory for storing data.  All temporary data files can
      be deleted or stored when the calculations are completed in accordance with the value of
      the environmental variable <c>MKL_PARDISO_OOC_KEEP_FILE</c>.
      If it is set to 1 (default value) - all files are deleted, if it is
      set to 0 - all files are stored.

      OOC parameters can be set in the configuration file. You
      can set the path to this file and its name via environmental
      variable <c>MKL_PARDISO_OOC_CFG_PATH</c> and <c>MKL_PARDISO_OOC_CFG_FILE_NAME</c>.

      By default, the OOC PARDISO uses the current directory for  storing data,
      and all work arrays associated with the matrix factors are stored in
      files named ooc_temp with different extensions. These default values
      can be changed by using the environmental variable <c>MKL_PARDISO_OOC_PATH</c>.
      To set the environmental variables:

      * <c>MKL_PARDISO_OOC_MAX_CORE_SIZE,</c>
      * <c>MKL_PARDISO_OOC_KEEP_FILE, and</c>
      * <c>MKL_PARDISO_OOC_PATH,</c>

      the configuration file should be created with the following lines:

      * <c>MKL_PARDISO_OOC_PATH = path\ooc_file</c>
      * <c>MKL_PARDISO_OOC_MAX_CORE_SIZE = N</c>
      * <c>MKL_PARDISO_OOC_KEEP_FILE = 0 (or 1),</c>

      where

      * "path" - is the directory for storing data,
      * ooc_file - file name without extension,
      * N - memory size in MBytes,

      It is not recommended to set this value greater than the size of the available RAM (default value - 2000 MBytes).
      Alternatively the environment variables can be set via command line:

      * <c>set MKL_PARDISO_OOC_PATH = path\ooc_file</c>
      * <c>set MKL_PARDISO_OOC_MAX_CORE_SIZE = N</c>
      * <c>set MKL_PARDISO_OOC_KEEP_FILE = 0 (or 1)</c>
</remarks>


      <seealso href="[1] Reference: Intel MKL v10 User Manual."/>*)
     TOutofCoreMode = (
      (*<summary>Out of core solver is not used.</summary>*)pooNone,
      (*<summary>Out of core solver is used, if more than the amount of prespecified memory is required.
      The in-core PARDISO is used if the total memory (in MBytes) needed for storing the matrix
      factors is less than the value of the environment variable <c>MKL_PARDISO_OOC_MAX_CORE_SIZE</c>
      (it's default value is 2000), and OOC PARDISO is used otherwise.</summary>*)
      pooAutomatic,
      (*<summary>Out of core is used always.</summary>*)
      pooAlways
     );

      
      TSparseMtx = class;
      

      TPardisoWork = class
      public
        nrhs, FCols: integer; 
        IsAllocated: boolean;
        IParam: TIntegerArray;
        WorkMtx: TSparseMtx;
        Symbolic: boolean;
        Numeric: boolean;
        (*<summary>Pointer to pardiso object.</summary>*)
        Pt: TIntegerArray;
        function CheckError(Err: integer): string;
        constructor Create;
        destructor Destroy; override;
      end;

     (*<summary>Class encapsulates Pardiso sparse solver specific settings.</summary>

        <SeeAlso href="http://www.pardiso-project.org/manual/manual.pdf"/>*)
     TPardisoSettings = class(TPersistent)
     strict private
        FIterativeStep: TPardisoIterative;
        FMatrixNumber: integer;
        FMaxFactorizedMatrices: integer;
        FCpuCount: integer;
        FReordering: TPardisoReordering;
        FPivotingPerturbation: integer;
        FSymmetricScalingAndMatching: boolean;
        FSymmetricBunchKaufmanPivot: boolean;
        FOutOfCoreMode: TOutofCoreMode;
        FTranspose: TMtxOperation;
        FParallelFactorization: TParallelFactorization;
        FParallelForwardBackward: TParallelForwardBackward;
        procedure SetMatrixNumber(const Value: integer);
        procedure SetMaxFactorizedMatrices(const Value: integer);
        procedure SetCpuCount(const Value: integer);
        procedure SetReordering(const Value: TPardisoReordering);
        procedure SetIterativeStep(const Value: TPardisoIterative);
        procedure SetPivotingPerturbation(const Value: integer);
        procedure SetSymmetricScalingAndMatching(const Value: boolean);
        procedure SetSymmetricBunchKaufmanPivot(const Value: boolean);
        procedure SetOutOfCoreMode(const Value: TOutofCoreMode);
        procedure SetTranspose(const Value: TMtxOperation);
        procedure SetParallelFactorization(const Value: TParallelFactorization);
        procedure SetParallelForwardBackward(const Value: TParallelForwardBackward);
     public
        (*<summary>Class constructor.</summary>*)
        constructor Create;
        destructor Destroy; override;
     published
        
        

        (*<summary>Maximum number of factors with identical nonzero sparsity structure.</summary>
          
<remarks>It is possible to store several different factorizations with the same
          nonzero structure at the same time in the internal data management of
          the solver. In most of the applications this value is equal to 1.
</remarks>
*)
        property MaxFactorizedMatrices: integer read FMaxFactorizedMatrices write SetMaxFactorizedMatrices;
        (*<summary>Actual matrix for the solution phase.</summary>
          
<remarks>With this scalar you can define the matrix that you would like to factorize.
</remarks>
*)
        property MatrixNumber: integer read FMatrixNumber write SetMatrixNumber; 
        (*<summary>The number of CPU's in the system.</summary>
          
<remarks>Must match the OpenMP system environment variable
          and the number must be equal to the MKL environment variable MKL_NUM_THREADS
</remarks>
*)
        property CpuCount: integer read FCpuCount write SetCpuCount;
        (*<summary>Defines the fill-in reducing reordering algorithms to use.</summary>*)
        property Reordering: TPardisoReordering read FReordering write SetReordering;
        (*<summary>Defines if iterative CG/CGS factorization should be attempted.</summary>
          
<remarks>Defines if previous numeric factorization should be used as the
          initial solution for first CG step for iterative factorization.
</remarks>


          <SeeAlso cref="TSparseMtx.RefinmentIters"/>
          <SeeAlso cref="TSparseMtx.Tolerance"/>*)
        property IterativeStep: TPardisoIterative read FIterativeStep write SetIterativeStep;
        (*<summary>Specifies how to handle small pivots.</summary>
          
<remarks>Defines an Eps as <c>10^(-PivotingPerturbation)</c>. This eps is then used
          to determine the magnitude of a potential pivot. A setting of -1 defines
          a default value of 13 for general and 8 for symmetric matrices.
</remarks>
*)
        property PivotingPerturbation: integer read FPivotingPerturbation write SetPivotingPerturbation;
        (*<summary>If true, iproves accuracy for symmetric indefinite matrices.</summary>*)
        property SymmetricScalingAndMatching: boolean read FSymmetricScalingAndMatching write SetSymmetricScalingAndMatching;
        (*<summary>Potentially improves accuracy for symmetric matrices by using Bunch-Kaufman pivoting.</summary>*)
        property SymmetricBunchKaufmanPivot: boolean read FSymmetricBunchKaufmanPivot write SetSymmetricBunchKaufmanPivot;
        (*<summary>Controls Out of core solver behaviour.</summary>*)
        property OutOfCoreMode: TOutofCoreMode read FOutOfCoreMode write SetOutOfCoreMode;
        (*<summary>Defines any additional transposition while computing the solution. </summary>*)
        property Transpose: TMtxOperation read FTranspose write SetTranspose;
        (*<summary>Parallel factorization control.</summary>
          
<remarks>Considered only when SymmetricScalingAndMatching is false.
</remarks>
*)
        property ParallelFactorization: TParallelFactorization read FParallelFactorization write SetParallelFactorization;
        (*<summary>Parallel solve control.</summary>*)
        property ParallelForwardBackward: TParallelForwardBackward read FParallelForwardBackward write SetParallelForwardBackward;

     end;

   
     (*<summary>The event type called when user defined stop criteria for iterative sparse solver is requested.</summary>
       
<remarks>The "Stop" parameter should be false to continue iteration and true to stop the iteration.
       The second dpar parameter is an array with the following contents:

      * dpar[0] specifies the relative tolerance. The default value is 1.0D-6.
      * dpar[1] specifies the absolute tolerance. The default value is 0.0D-0.
      * dpar[2] specifies the Euclidean norm of the initial residual (if it is computed in the dfgmres routine). The initial value is 0.0.
      * dpar[3] service variable equal to dpar(1)*dpar(3)+dpar(2). The initial value is 0.0.
      * dpar[4] specifies the Euclidean norm of the current residual. The initial value is 0.0.
      * dpar[5] specifies the Euclidean norm of residual from the previous iteration step (if available). The initial value is 0.0.
      * dpar[6] contains the norm of the generated vector for FGMRES. The initial value is 0.0.
      * dpar[7] contains the tolerance for the zero norm of the currently generated vector for GMRES (Default value is 1.0D-12) and and Alpha parameter for CG with default value of 0.
      * dpar[8] beta parameter for CG (dpar(5)/dpar(6) with default value of 0. Not used by GMRES.
</remarks>


      <seealso href="[1] Reference: Intel MKL v10 User Manual."/>*)
     TIterativeStopTest = procedure(var Stop: boolean; var dpar: array of double) of object;

     (*<summary>Iterative preconditioner options.</summary>
      
<remarks>Preconditioners can be used to accelerate the iteration process.
      They are not to be used with Conjugate Gradient (CG) solver.
</remarks>


     <seealso href="[1] Reference: Intel MKL v10 User Manual."/>*)
     TIterativePreconditioner = (
     (*<summary>Do not use a preconditioner</summary>*) itpNone,
     (*<summary>Use ILU0 preconditioner.</summary>*)   itpILU0,
     (*<summary>Use ILUT preconditioner.</summary>*)   itpILUT);

     TIterativeSolverMethod = (
     (*<summary>Conjugate gradient solver.</summary>*) ismCG,
     (*<summary>GMRES solver.</summary>*) ismGMRES
     );

     TIterativeSettings = class
     private
       FMaxIters: integer;
       FPreconditioner: TIterativePreconditioner;
       FOnStopTest: TIterativeStopTest;
       FSolverMethod: TIterativeSolverMethod;
       procedure SetMaxIters(const Value: integer);
       procedure SetOnStopTest(const Value: TIterativeStopTest);
       procedure SetPreconditioner(const Value: TIterativePreconditioner);
       procedure SetSolverMethod(const Value: TIterativeSolverMethod);
     public
       property SolverMethod: TIterativeSolverMethod read FSolverMethod write SetSolverMethod;
     

      

     

       property Preconditioner: TIterativePreconditioner read FPreconditioner write SetPreconditioner;

    
       property MaxIters: integer read FMaxIters write SetMaxIters;
       property OnStopTest: TIterativeStopTest read FOnStopTest write SetOnStopTest;
     end;

   

      TPAPCharArray = array of PAPChar;

     (*<summary>Iterative solution refinment for Taucs.</summary>
       
<remarks>Defines the factorization method used by the Taucs sparse solver.
</remarks>
*)
     TTaucsFactorization = (
      (*<summary>Cholesky factorization.</summary>*)tfCholesky,
      (*<summary>LU factorization with partial pivoting.</summary>*)tfLU,
      (*<summary>LDL(T) factorization without pivoting.</summary>*)tfLDL,
      (*<summary>Multifrontal factorization.</summary>*)tfMultiFrontal,
      (*<summary>Left-looking factorization.</summary>*)tfLeftLooking
     );

     (*<summary>Defines the reordering method used by Taucs.</summary>*)
     TTaucsReordering = (
      (*<summary></summary>*)tpMMD,
      (*<summary></summary>*)tpAMD,
      (*<summary></summary>*)tpMD,
      (*<summary></summary>*)tpMETIS,
      (*<summary></summary>*)tpGENMMD,
      (*<summary></summary>*)tpCOLAMD,
      (*<summary></summary>*)tpRANDOM,
      (*<summary></summary>*)tpTREE,
      (*<summary></summary>*)tpIdentity
     );

     (*<summary>Iterative solution refinment for Taucs.</summary>*)
     TTaucsIterative = (
      (*<summary>No iterative algorithm is used.</summary>*)tiNone,
      (*<summary>Use the Conjugate gradient iterative algoritm to solve sparse equation.
        The Conjugate gradient is only guaranteed to work on symmetric and
        positive definite matrices.</summary>*)
       tiCG,
      (*<summary>Use the MinRes algorithm to solve sparse equation. Unlike the Conjugate
      gradient method this one requires only a symmetric matrix.</summary>*)
      tiMinRes
     );

     TTaucsWork = class
     public
        POptions: TPAPCharArray;
        
        POptionCount: integer;
        

        StoredFactorization: integer;
        procedure AppendOption(const Param: string);
        procedure ClearOption;
        function CheckError(Err: integer): string;

        constructor Create;
        destructor Destroy; override;
     end;

     (*<summary>Class encapsulates Taucs sparse solver specific settings.</summary>*)
     TTaucsSettings = class(TPersistent)
     strict private
        FIterativeStep: TTaucsIterative;
        FOutOfCore: boolean;
        FOutOfCoreFileName: string;
        FFactor: TTaucsFactorization;
        FReordering: TTaucsReordering;
        FAmwb: boolean;
        FAmwbSubGraphs: integer;
        FAmwbRandomSeed: integer;
        FInCoreMemoryAuto: boolean;
        FInCoreMemorySize: integer;
        FCustomOptions: TStringList;
        procedure SetCustomOptions(const Value: TStringList);
        procedure SetFactor(const Value: TTaucsFactorization);
        procedure SetReordering(const Value: TTaucsReordering);
        procedure SetOutOfCore(const Value: boolean);
        procedure SetOutOfCoreFileName(const Value: string);
        procedure SetIterativeStep(const Value: TTaucsIterative);
        procedure SetAmwb(const Value: boolean);
        procedure SetAmwbRandomSeed(const Value: integer);
        procedure SetAmwbSubGraphs(const Value: integer);
        procedure SetInCoreMemoryAuto(const Value: boolean);
        procedure SetInCoreMemorySize(const Value: integer);
     public
        (*<summary>Virtual constructor</summary>*)
        constructor Create; virtual;
        destructor Destroy; override;
     published
        property CustomOptions: TStringList read FCustomOptions write SetCustomOptions;
        (*<summary>Specifies the factorization method to use.</summary>
          
<remarks>Note:
            Not all methods support all matrix types!
</remarks>
*)
        property Factor: TTaucsFactorization read FFactor write SetFactor;
        (*<summary>Use Out Of Core factorization.</summary>
          
<remarks>If True, the factorization process will use the disk as data
          storage. This can improve the speed of the solver in compare to using
          the Windows virtual memory, when memory requirements are really
          big (10s of GB), or when trying to address more then 2GB of memory.
</remarks>
*)
        property OutOfCore: boolean read FOutOfCore write SetOutOfCore;
        (*<summary>Factorization data filename.</summary>
          
<remarks>Specifies the Filename (and path) of where to store the factorization data
          files sliced in to pieces of 1GB each. This file name is used only when
          OutOfCore is set to True.
</remarks>
*)
        property OutOfCoreFileName: string read FOutOfCoreFileName write SetOutOfCoreFileName;
        (*<summary>Defines the in-core memory size to use in bytes.</summary>

          <SeeAlso cref="OutOfCore"/>
          <SeeAlso cref="InCoreMemoryAuto"/>*)
        property InCoreMemorySize: integer read FInCoreMemorySize write SetInCoreMemorySize;
        (*<summary></summary>
          
<remarks>If True in-core memory size used is determined automatically.
          If False in-core memory size used by the out-of-core solver
          is NOT determined automatically.
</remarks>


          <SeeAlso cref="InCoreMemorySize"/>*)
        property InCoreMemoryAuto: boolean read FInCoreMemoryAuto write SetInCoreMemoryAuto;
        (*<summary>Defines the fill-in reducing reordering algorithms to use.</summary>*)
        property Reordering: TTaucsReordering read FReordering write SetReordering;
        (*<summary>Defines if and which iterative CG/MinRes factorization should be attempted.</summary>
          
<remarks>The <see cref="TSparseMtx.MaxIters"/> defines the maximum number of iterations and
          <see cref="TSparseMtx.Tolerance"/> the requested accuracy.
</remarks>


          <SeeAlso cref="TSparseMtx.MaxIters"/>
          <SeeAlso cref="TSparseMtx.Tolerance"/>*)
        property IterativeStep: TTaucsIterative read FIterativeStep write SetIterativeStep;
        (*<summary>Set it to true to enable maximum-weight-basis Vaidha preconditioners.</summary>*)
        property Amwb: boolean read FAmwb write SetAmwb;
        (*<summary>Desired number of graphs in the tree partitioning.</summary>
          
<remarks>Used only if <see cref="Amwb"/> is True.
</remarks>
*)
        property AmwbSubGraphs: integer read FAmwbSubGraphs write SetAmwbSubGraphs;
        (*<summary>Random seed  between 0 and High(Integer)-1.</summary>
          
<remarks>Used only if <see cref="Amwb"/> is True.
</remarks>
*)
        property AmwbRandomSeed: integer read FAmwbRandomSeed write SetAmwbRandomSeed;
     end;

    

     (*<summary>Encapsulates sparse matrix properties and methods/operations.</summary>
      
<remarks>The class encapsulates sparse matrix properties and methods/operations like
      sparse matrix addition, subtraction, multiplication, mixed operations
      with dense vectors and matrices, three types of sparse linear system solvers:
      * Umfpack,
      * Pardiso
      * Iterative

      The class also includes different matrix storage format conversion methods and more...
</remarks>
*)
     
     TSparseMtx = class(TMtxVec )
     strict private
        fTaucs: TTaucsSettings;
        fTaucsWork: TTaucsWork;
        fPardiso: TPardisoSettings;
        fPardisoWork: TPardisoWork;
        FSparseSolver: TSparseSolver;
        TripletsRow, TripletsColumn: TIntegerArray;

        TripletsValues: TDoubleArray;
        TripletsCValues: TCplxArray;

        TripletsSValues: TSingleArray;
        TripletsSCValues: TSCplxArray;

        TripletRowCount, TripletColCount,TripletsCount: integer;

        FRefinmentIters: integer;
        FIterativeMethod: TIterativeMethod;
        FSparseSystem: TSparseSystem;
        FMtxError: string;
        FAutoPattern: boolean;
        FReportLevel: TReportLevel;
        FConvergenceCheck: TConvergenceCheck;
        FTolerance: double;
        FMaxIters: integer;
        FActualIters: integer;
        FVectorCount: integer;
        FAutoClearReport: boolean;
        FRows: integer;
        FCols: integer;
        FBackError: double;
        FSparseBlockSize: integer;
        FDontStripZeros: boolean;
        FSparsePattern: TSparsePattern;


        
        
        
        procedure SetIterativeMethod(const Value: TIterativeMethod);
        procedure SetRefinmentIters(const Value: integer);
        procedure SetSparseSystem(const Value: TSparseSystem);
        procedure SetMtxError(const Value: string);
        procedure SetAutoPattern(const Value: boolean);
        procedure SetReportLevel(const Value: TReportLevel);
        procedure UmfErrorCheck(Status: integer; aName: string; Control, Info: TDoubleArray;
                               Report: boolean = False;
                               NoInfo: boolean = false); overload;
        procedure UmfErrorCheck(Status: integer; aName: string; sControl, sInfo: TSingleArray;
                               Report: boolean = False;
                               NoInfo: boolean = false); overload;
        procedure SetConvergenceCheck(const Value: TConvergenceCheck);
        procedure SetTolerance(const Value: double);
        procedure SetActualIters(const Value: integer);
        procedure SetMaxIters(const Value: integer);
        procedure SetVectorCount(const Value: integer);
        procedure SetAutoClearReport(const Value: boolean);
        procedure SetCols(const Value: integer);
        procedure SetRows(const Value: integer);
        procedure SetBackError(const Value: double);
        procedure SetSparseBlockSize(const Value: integer);
        procedure InternalTripletsToSparse(RowCount, ColCount: integer;
                  const Row, Column: TIntegerArray; const aCValues: TCplxArray;
                  const aValues: TDoubleArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true);

        procedure InternalTripletsToSparseS(RowCount, ColCount: integer;
                  const Row, Column: TIntegerArray; const aCValues: TSCplxArray;
                  const aValues: TSingleArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true);

        procedure SetDontStripZeros(const Value: boolean);
        procedure SetSparsePattern(const Value: TSparsePattern);
        procedure SetSparseSolver(const Value: TSparseSolver);
     
     strict protected
     
        function GetValue(ARow, ACol: integer): double;
        procedure SetValue(ARow, ACol: integer; const Value: double);
        procedure SetMtxCValue(ARow, ACol: integer; const CValue: TCplx);
        function GetMtxCValue(ARow, ACol: integer): TCplx;

        function GetSValue(ARow, ACol: integer): single;
        procedure SetSValue(ARow, ACol: integer; const Value: single);
        procedure SetMtxSCValue(ARow, ACol: integer; const CValue: TSCplx);
        function GetMtxSCValue(ARow, ACol: integer): TSCplx;
     strict protected
        
        Symbolic: PAPointer;
        Numeric: PAPointer;
        property AutoPattern: boolean read FAutoPattern write SetAutoPattern;

        procedure DoSetCols(Sender: TObject);
        procedure LoadFromHarwellBoeingFile(const FileName: string);
        procedure SaveToHarwellBoeingFile(const FileName: string);
        procedure DoSetComplex; override;
        procedure setActualSize(const byteCount: Int64); override;
        procedure SolveUmfPack(const B, X: TVec); overload;
        procedure SolveTaucs(const B, X: TVec; MtxType: TMtxType); overload;
        procedure SolveTaucs(const B, X: TMtx; MtxType: TMtxType); overload;
        procedure SolveTaucsInternal(const B, X: TDenseMtxVec; MtxType: TMtxType; nrhs: integer); overload;
        procedure SolveUmfPack(const B, X: TMtx); overload;
        procedure SolvePardiso(const B, X: TVec; MtxType: TMtxType); overload;
        procedure SolvePardiso(const B, X: TMtx; MtxType: TMtxType); overload;
        procedure SolveIterative2(const B, X: TVec; MtxType: TMtxType); overload;
        procedure SolveIterative2(const B, X: TMtx; MtxType: TMtxType); overload;
        procedure SolvePardisoInternal(const B, X: TDenseMtxVec; MtxType: TMtxType; nrhs: integer); overload;
        function MtxTypePardiso(MtxType: TMtxType): integer;
        function MtxTypeTaucs(MtxType: TMtxType): integer;
        procedure SetPardiso(Value: TPardisoSettings);
        procedure SetTaucs(Value: TTaucsSettings);
        function InternalRMS: double;

        function InternalEig(const D, R: TVec; const V: TMtx; EigCount: integer; Smallest, IsSymmetric: boolean; const fpm: TIntegerArray): integer;
        function InternalEigGen(const bB: TSparseMtx; const D, R: TVec; const V: TMtx; EigCount: integer; Smallest, IsSymmetric: boolean; const fpm: TIntegerArray): integer;
        function InternalSvd(const S, R: TVec; const VL, VR: TMtx; SvdCount: integer; Smallest, IsSymmetric: boolean; const fpm: TIntegerArray): integer;

        procedure SubRangeSaveState; override;
        procedure SetSubRangeDirect(Index, Len: integer); override;
        procedure SetSubRangeDirect(const Src: TMtxVecBase; Index, Len: integer); override;

        procedure UmfpackFreeSymbolic;
        procedure UmfpackFreeNumeric;
        procedure PardisoFreeSymbolic;
        procedure PardisoFreeNumeric;

        procedure SetLength(const Value: integer); override;
        
     

     public  
        (*<summary></summary>*)
        Ap: TIntegerArray;
        (*<summary></summary>*)
        Ai: TIntegerArray;
        (*<summary></summary>*)
        Control: TDoubleArray;
        SControl: TSingleArray;
        (*<summary></summary>*)
        Info: TDoubleArray;
        SInfo: TSingleArray;

        (*<summary>Defines the pattern used when solving a system of linear equations.</summary>
          
<remarks>Use this property to instruct the linear system solver which of the
          results from the previous sparse system solution can be reused. This
          can be symbolic factorization and/or numeric factorization.
</remarks>
*)
        property SparsePattern: TSparsePattern read FSparsePattern write SetSparsePattern;

        (*<summary>Holds the error string, if an error has occurred.</summary>
          
<remarks>Holds the error string, if an error has occurred. If there was no error, the string is empty.
</remarks>

          <SeeAlso cref="BackError"/>*)
        property MtxError: string read FMtxError write SetMtxError;

        (*<summary>Returns the number of actual iterations performed by <see cref="Solve"/>.</summary>
		      
<remarks>Returns the number of actual iterations performed by <see cref="Solve"/>. After running direct solver
		      (<see cref="TSparseMtx.Solve"/> method) with active iterative refinement
          (<see cref="RefinmentIters"/> bigger than 0) or iterative solvers (<see cref="SolveIterative"/> method), the
          ActualInters property contains the actual number of iterations performed.
</remarks>


          

          <example>
          <code>
          using Dew.Math;
          using Dew.Math.Units;

          namespace Dew.Examples
          {
            private void Example()
            {
              Vector x = new Vector(0);
              Vector b = new Vector(0);
              TSparseMtx sparseM = new TSparseMtx();
              x.size(b);
              sparseM.SolveIterative(b,x);
              int iters = sparseM.ActualIters;
            }
          }
          </code></example>

          <SeeAlso cref="RefinmentIters"/>
          <SeeAlso cref="Solve"/>
          <SeeAlso cref="SolveIterative"/>*)
        property ActualIters: integer read FActualIters write SetActualIters;

        (*<summary>Holds the backward or iterative stopping criteria.</summary>
          
<remarks>After running direct solver (<see cref="Solve"/> method) with active iterative refinement (<see cref="RefinmentIters"/>
           &gt; 0) the property holds the  backward error. In case of iterative solvers (<see cref="SolveIterative"/> method),
           the property holds the error according to the stoping criteria (see <see cref="ConvergenceCheck"/> property).
</remarks>


          <SeeAlso cref="ActualIters"/>
          <SeeAlso cref="RefinmentIters"/>
          <SeeAlso cref="Solve"/>
          <SeeAlso cref="SolveIterative"/>*)
        property BackError: double read FBackError write SetBackError;

        
        (*<summary>Allows setting and getting value at position Row and Col.</summary>
          
<remarks>Allows setting and getting value at position Row and Col. This property
          is to be used to form a new sparse matrix only. See the
          <see cref="TripletsToSparseAndFlush"/> method, on how to use
          this property. This property should not be used to access individual
          values of a sparse matrix. To access individual values, the following
          pattern can be used:

          <code>
          for i := 0 to SparseMtx.Cols-1 do
            for j := SparseMtx.ap[i] to SparseMtx.ap[i+1]-1 do
            begin
              aRow := SparseMtx.ai[j];
              aValue := SparseMtx.Values[j];
              //...       The aValue is stored at i'th column and aRow
              //i'th column values are stored in the Values array from index ap[i] to ap[i+1]
              //the row indices for the i'th column are stored in the Ai array
            end;
          </code>
</remarks>
*)
        property Value[Row, Col: integer]: double read GetValue write SetValue; default;
        (*<summary>Allows setting and getting double precision complex values.</summary>*)
        property CValue[Row, Col: integer]: TCplx read GetMtxCValue write SetMtxCValue;
        (*<summary>Allows setting and single precision values.</summary>*)
        property SValue[Row, Col: integer]: single read GetSValue write SetSValue;
        (*<summary>Allows setting and getting single precision complex values.</summary>*)
        property SCValue[Row, Col: integer]: TSCplx read GetMtxSCValue write SetMtxSCValue;
        
     public
        (*<summary>Copies sparse matrix from Src to the calling object.</summary>*)
        procedure Copy(const Src: TSparseMtx; StripZeros: boolean = false); overload;
        (*<summary>Copies sparse matrix from Src to the calling object.</summary>*)
        procedure CopyTo(const Dst: TSparseMtx; const aFloatPrecision: TMtxFloatPrecision); overload;

        
        procedure Assign(Source: TPersistent); override;
        

        (*<summary>Sums two sparse matrices (A+B).</summary>
          
<remarks>Sum sparse matrices A and B and place the result in the calling sparse matrix. Because the resulting matrix can be
          much less sparse, the memory requirements can increase beyond the available system memory. An exception will be
          raised if if the requested memory size will exceed the value of <paramref name="MaxNonZeroCount"/>.
</remarks>


          <SeeAlso cref="Sub"/>*)
        procedure Add(const A, B: TSparseMtx; MaxNonZeroCount: integer = 10000000);  overload;
        (*<summary>Add the content of the calling sparse matrix to the dense matrix Y.</summary>*)
        procedure Add(const Y: TMtx); overload;

        (*<summary>Construct sparse matrxix by summing lower, upper triangluar part and the diagonal.</summary>
          
<remarks>Sums up the sparse matrix from its three parts: The lower triangle (LowerTriang), main diagonal (Diag) and upper triangle
          (UpperTriang).
</remarks>


          

          <example>
          <code>
          using Dew.Math;
          using Dew.Math.Units;

          namespace Dew.Examples
          {
            private void Example()
            {
              Vector d = new Vector(0);
              TSparseMtx asp = new TSparseMtx();
              TSparseMtx bsp = new TSparseMtx();
              TSparseMtx csp = new TSparseMtx();
              TSparseMtx dsp = new TSparseMtx();
              // ...
              // #1 : split asp into lower(bsp), upper(csp) and diagonal(d) sections
              bsp.LowerTriangle(asp);
              csp.UpperTriangle(asp);
              asp.Diag(d);
              // now combine lower(bsp), upper, and diagonal(d) back
              dsp.AddSplit(bsp,d,csp);
            }
          }
          </code></example>

          <SeeAlso cref="Split"/>*)
        procedure AddSplit(const LowerTriang: TSparseMtx; const Diag: TVec; const UpperTriang: TSparseMtx); overload;
        (*<summary>Sums up the sparse matrix from its lower (LoweTriang) and upper triangle (UpperTriang).</summary>*)
        procedure AddSplit(const LowerTriang, UpperTriang: TSparseMtx); overload;

        (*<summary>Adjungate operation.</summary>
          
<remarks>Adjungate X and store the results in calling sparse matrix.
</remarks>


          <SeeAlso cref="Transp"/>
          <SeeAlso cref="ConjMul"/>*)
        function  Adjung(const X: TSparseMtx): TSparseMtx; overload;

        (*<summary>Converts banded matrix to modified compressed column sparse matrix format.</summary>
          
<remarks>Converts banded matrix Src to modified compressed column sparse matrix format.
</remarks>
*)
        procedure BandedToSparse(const Src: TMtx; ZeroThreshold: double = 0);

        (*<summary>Conjugate Sparse2 and matrix multiply it with Sparse1.</summary>
          
<remarks>Conjugate Sparse2, multiply it with Sparse1 and store the results in calling sparse matrix.
</remarks>
*)
        function  ConjMul(const Sparse1, Sparse2: TSparseMtx; MaxNonZeroCount: integer = 10000000): TSparseMtx; overload;

        
        procedure Clear; override;
        

        (*<summary>Copies the kk-th diagonal of TSparseMtx to the Vec object.</summary>
          
<remarks>Copies the  kk-th diagonal of TSparseMtx to the Vec object. If k = 0 then the main diagonal matrix is obtained,
          if k &lt; 0 then the k-th subdiagonal matrix is obtained and if k &gt; 0 the then the k-th super diagonal is copied to Vec.
          The Vec parameters size is set automatically.
</remarks>


          

          <example>
          <code>
          using Dew.Math;
          using Dew.Math.Units;

          namespace Dew.Examples
          {
            private void Example()
            {
              Vector d = new Vector(0);
              TSparseMtx asp = new TSparseMtx();
              TSparseMtx bsp = new TSparseMtx();
              TSparseMtx csp = new TSparseMtx();
              TSparseMtx dsp = new TSparseMtx();
              // ...
              // #1 : split asp into lower(bsp), upper(csp) and diagonal(d) sections
              bsp.LowerTriangle(asp);
              csp.UpperTriangle(asp);
              asp.Diag(d);
              // now combine lower(bsp), upper, and diagonal(d) back
              dsp.AddSplit(bsp,d,csp);
            }
          }
          </code></example>*)
        procedure Diag(const Vec: TVec; kk: integer = 0); overload;

        (*<summary>Calculates the number of diagonals above and below main diagonal.</summary>
          
<remarks>Calculates the number of diagonals above (Upper) and below (Lower) the main diagonal.
          This information is used by the <see cref="SparseToBanded"/> method.
</remarks>
*)
        procedure DiagonalsCount(out Upper, Lower: integer);

        (*<summary>Convert dense matrix to sparse matrix format.</summary>
          
<remarks>Convert dense matrix A to modified compressed column sparse matrix format.
</remarks>


          <example><b>Step 1:</b> Construct dense matrix,<para/>
          <code>
          var A,B: Matrix;
          SparseA: TSparseMtx;
          /// ...
          A.SetIt(3,3,false,[1,0,0,0,2,0,0,1,3]);
          </code><para/>

          <b>Step 2:</b> Convert dense matrix to sparse matrix<para/>
          <code>
          /// ...
          SparseA.DenseToSparse(A);
          </code><para/>

          <b>Step 3:</b> Convert sparse matrix back to dense and compare<para/>
          <code>
          /// ...
          SparseA.SparseToDense(B);
          if not A.Equal(B,1.0e-5) then ERaise('Not Equal!');
          </code>
          </example>

          <SeeAlso cref="SparseToDense"/>*)
        procedure DenseToSparse(const A: TMtx; ZeroThreshold: double = 0; PreserveZeroDiagElem: boolean = true);

        (*<summary>Divide individual elements of sparse matrices.</summary>
          
<remarks>Divide individual elements of the calling object with the
          Sparse matrix. The sparsitiy pattern of both matrices is assumed
          to be equal.
</remarks>
*)
        function  DivideElem(const Sparse: TSparseMtx): TSparseMtx; overload;

        (*<summary>Compares two sparse matrices.</summary>
          <param name="B">Compare B with calling sparse matrix.</param>
          <param name="Tolerance">Defines the comparison tolerance. The maximum difference between elements may not exceed: <c>+/-Tolerance</c>.
            If Tolerance is omitted, a direct comparison algorithm is used.</param>
          <returns>true, if the calling matrix is equal to sparse B matrix  (if all elements match in position and value).</returns>*)
        function  Equal(const B: TSparseMtx; Tolerance: double = 0): boolean; overload;
        (*<summary>Compares two sparse matrices.</summary>
          <returns>true, if the calling matrix is equal to dense B matrix  (if all elements match in position and value).</returns>

          
<remarks>Tolerance defines the comparison tolerance. The maximum difference between elements may not exceed: +/-Tolerance.
          If Tolerance is omitted, a direct comparison algorithm is used. If NonZerosOnly is True then only the non-zero values
          of the calling sparse matrix are compared with corresponding values in the dense matrix.
</remarks>


          <SeeAlso cref="IsEqualSize"/>*)
        function  Equal(const B: TMtx; Tolerance: double = 0; NonZerosOnly: boolean = False): boolean; overload;

        (*<summary>Frees the numeric object used by the Umfpack.</summary>
          
<remarks>Frees the numeric object used by the Umfpack and thus releases the associated memory.
</remarks>
*)
        procedure FreeNumeric;

        (*<summary>Frees the symbolic object used by the Umfpack.</summary>
          
<remarks>Frees the symbolic object used by the Umfpack and thus releases the associated memory.
</remarks>
*)
        procedure FreeSymbolic;

        
        procedure FreeToCache; override;
        

        (*<summary>Resizes the sparse matrix to a new number of non-zeros.</summary>
          
<remarks>Resizes the sparse matrix to a new number of non-zeros.
          The existing values are preserved up to the new size. This includes
          the sparsity pattern arrays Ai and Ap. This resize does not
          change the number of Columns of the sparse matrix, but only
          the number of non-zeros that can be stored in the matrix.
</remarks>
*)
        function Resize(Len: integer; ZeroIt: boolean = False): TMtxVec; overload;

        (*<summary>The inverse matrix.</summary>
          
<remarks>Calculates the inverse (Mtx^-1) of the calling matrix and stores the result in Y dense matrix.
          If the calling matrix is not quadratic, an exception is raised.
</remarks>
*)
        procedure Inv(const Y: TMtx);

        (*<summary>Compares matrix size.</summary>
          <returns>true if Sparse sparse matrix <see cref="TMtxVec.Complex"/>, <see cref="Rows"/>,
          <see cref="Cols"/> and <see cref="NonZeros"/> properties match those of calling sparse matrix.</returns>

          

          <example>
          <code>
            SparseA.Size(3,4,3,false);
            SparseB.Size(3,4,1,false);
            if SparseA.IxEqualSize(SparseB) then Console.WriteLine("Equal size");
              else Console.WriteLine("Not equal size");
          </code></example>

          <SeeAlso cref="Equal"/>*)
        function  IsEqualSize(const Sparse: TSparseMtx): boolean; virtual;

        (*<summary>Extract lower part of the Mtx.</summary>
          
<remarks>Copies only the lower triangular part of the Src sparse matrix. The result is stored in the calling sparse matrix.
          If the Diagonal boolean parameter is true then the Src matrix main diagonal elements will be copied to the calling
          matrix main diagonal elements. If the Diagonal parameter is false, the calling matrix main diagonal elements will
          be set to zero.
</remarks>


          

          <example>
          <code>
          using Dew.Math;
          using Dew.Math._Units;

          void Example()
          {
            Vector d = new Vector(0);
            TSparseMtx asp = new TSparseMtx();
            TSparseMtx bsp = new TSparseMtx();
            TSparseMtx csp = new TSparseMtx();
            TSparseMtx dsp = new TSparseMtx();
            // ...
            // #1 : split asp into lower(bsp), upper(csp) and diagonal(d) sections
            bsp.LowerTriangle(asp);
            csp.UpperTriangle(asp);
            asp.Diag(d);
            // now combine lower(bsp), upper, and diagonal(d) back
            dsp.AddSplit(bsp,d,csp);
            // should be equal
            if (!asp.Equal(dsp)()) Console.WriteLine("Not equal!");
          }
		      </code></example>

          <SeeAlso cref="UpperTriangle"/>*)
        function LowerTriangle(const Src: TSparseMtx; Diagonal: boolean = false): TSparseMtx; overload;

        (*<summary>Multiply two sparse matrices (matrix multiplication).</summary>
          
<remarks>Multiply sparse matrices A and B and place the result in the calling matrix. Because the resulting matrix
          can be much less sparse, the memory requirements can increase beyond the available system memory. This method
          will raise an exception, if the requested memory size will exceed the value of <paramref name="MaxNonZeroCount"/>.
</remarks>


          

          <example>
            Multiply SparseA and SparseB where maximum number of non-zero elements is estimated at 100.
          <code>
            SparseC.Mul(SparseA,SparseB, 100);
          </code></example>

          <SeeAlso cref="MulLeft"/>
          <SeeAlso cref="MulRight"/>*)
        procedure Mul(const C, B: TSparseMtx; MaxNonZeroCount: integer = 10000000); overload;

        (*<summary>Multiply the sparse matrix from left.</summary>
          
<remarks>Multiply calling sparse matrix from left with dense matrix X and place the result in dense
          matrix Y.
</remarks>


          

          <example>
          <code>
            Matrix a= new Matrix(3,2);
            Matrix b =  new Matrix(2,3);
            TMtxSparse aSparse = new TMtxSparse();
            aSparse.MulLeft(a,b); // result is stored in b
          </code></example>

          <SeeAlso cref="MulRight"/>*)
        procedure MulLeft(const X, Y: TMtx); overload;
        (*<summary>Multiply sparse matrix from left with vector X and place the result in vector Y.</summary>*)
        procedure MulLeft(const X, Y: TVec); overload;

        (*<summary>Multiply the sparse matrix from right.</summary>
          
<remarks>Multiply calling sparse matrix from right with dense matrix X and place the result in dense matrix Y.
</remarks>


          

          <example>
          <code>
            Matrix a= new Matrix(3,2);
            Matrix b =  new Matrix(2,3);
            TMtxSparse aSparse = new TMtxSparse();
            aSparse.MulRight(a,b); // result is stored in b
          </code></example>

          <SeeAlso cref="MulLeft"/>*)
        procedure MulRight(const X, Y: TMtx); overload;
        (*<summary>Multiply sparse matrix from right with vector X and place the result in vector Y.</summary>*)
        procedure MulRight(const X, Y: TVec); overload;

        (*<summary>Root mean squared of all non-zero elements.</summary>*)
        function RMS: double; reintroduce; overload;

        (*<summary>Minimum of all non-zero elements.</summary>

          <SeeAlso cref="Max"/>
          <SeeAlso cref="Minc"/>*)
        function Min: double; overload;

        (*<summary>Maximum of all non-zero elements.</summary>
          <SeeAlso cref="Min"/>
          <SeeAlso cref="Maxc"/>*)
        function Max: double; overload;

        (*<summary>Average of all non-zero elements.</summary>*)
        function Mean: double; overload;

        (*<summary>Minimum of all non-zero complex elements.</summary>
          <SeeAlso cref="Min"/>*)
        function Minc: TCplx; overload;

        (*<summary>Maximum of all non-zero complex elements.</summary>
          <SeeAlso cref="Max"/>*)
        function Maxc: TCplx; overload;

        (*<summary>Average of all non-zero complex elements.</summary>
          <SeeAlso cref="Mean"/>*)
        function Meanc: TCplx; overload;

        (*<summary>The size of sparse matrix.</summary>
          
<remarks>Defines the calling matrix size to match those of the AMtxVec objects.
          If AComplex is true, calling sparse matrix <see cref="TMtxVec.Complex"/> property
          is set to true.
</remarks>


          

          <example>
          <code>
            SparseA.Size(3,4,5,false);
            // is the same as calling this sequence
            SparseA.Complex = false;
            SparseA.Rows = 3;
            SparseA.Cols = 4;
            SparseA.NonZeros = 5;
          </code></example>*)
        function Size(const Src: TMtxVecBase; AComplex: boolean): TMtxVec; override;
        (*<summary>Sets sparse matrix size to match Src with new aFloatPrecision.</summary>*)
        function Size(const Src: TMtxVecBase; const aFloatPrecision: TMtxFloatPrecision): TMtxVec; override;
        (*<summary>Define sparse matrix size in single pass.</summary>
          
<remarks>Define the sparse matrix size to RowCount <see cref="Rows"/>, ColCount
          <see cref="Cols"/>, NonZeroCount non-zero elements (the <see cref="NonZeros"/>
          property) and define the matrix to be complex if IsComplex is true.
</remarks>
*)
        procedure Size(const RowCount, ColCount, NonZeroCount: integer; const aFloatPrecision: TMtxFloatPrecision); overload;
        procedure Size(const RowCount, ColCount, NonZeroCount: integer; const aComplex: boolean; const aIsDouble: boolean); overload;
        procedure Size(const RowCount, ColCount, NonZeroCount: integer; const IsComplex: boolean); overload;

        (*<summary>Direct solve.</summary>
          
<remarks>Solve the system <c>A*X = B</c>, where A is the calling matrix. The actual system being solved is defined by <see cref="SparseSystem"/> property.
          A is sparse and unsymmetric. It is based on the Unsymmetric MultiFrontal method, which factorizes PAQ into the product LU,
          where L and U are lower and upper triangular, respectively, and P are Q are permutation matrices. Both P and Q are chosen to
          reduce fill-in (new nonzeros in L and U that are not present in A). The permutation P has the dual role of reducing fill-in
          and maintaining numerical accuracy (via relaxed partial pivoting and row interchanges).

          The Solve method uses BLAS level 3 dgeem matrix multiply routine and takes full advantage of CPU specific optimized code
          and symmetric multiprocessing.
</remarks>


          <example>
          <code>
          uses MtxExpr, MtxVecTee, Sparse;

          procedure Example;
          var x,b: Vector;
            SparseA: TSparseMtx;
          begin
            // load data
            SparseA.LoadFromMatrixFile('system.mtx');
            b.LoadFromFile('coefficients.Vec');
            // set solution size
            x.Size(b);
            // solve
            SparseA.SparseSolver := ssUmfPack;
            SparseA.Solve(b,x);
            //view solution
            ViewValues(x);
          end;
          </code>
          <code lang="C++">
          #include "MtxExpr.hpp"
          #include "Sparse.hpp"
          #include "MtxVecTee.hpp"

          void  __fastcall Example()
          {
            sVector x,b;
            TSparseMtx *SparseA = new TSparseMtx();
            try
            {
              // load data
              SparseA->LoadFromMatrixFile("system.mtx");
              b->LoadFromFile("coefficients.Vec");
              // set solution size
              x->Size(b);
              // solve
              SparseA->SparseSolver = ssUmfPack;
              SparseA->Solve(b,x);
              //view solution
              ViewValues(x);
            }
            __finally
            {
              delete SparseA;
            }
          }
          </code>
          </example>

          <SeeAlso cref="SparseSolver"/>*)
        procedure Solve(const B,X: TMtx; MtxType: TMtxType = mtGeneral); overload;
        (*<summary>Vector version of Solve.</summary>*)
        procedure Solve(const B,X: TVec; MtxType: TMtxType = mtGeneral); overload;

        (*<summary>Solve the system <c>A*X = B</c> by using one of the iterative methods.</summary>
          
<remarks>The term "iterative method" refers to a wide range of techniques that use successive  approximations to obtain more accurate
          solutions to a linear system at each step. There are two major groups of iterative methods:  stationary and nonstationary
          methods. Stationary methods are older, simpler to understand and implement, but usually not as effective. Nonstationary
          methods are a relatively recent development; their analysis is usually harder to understand, but they can be highly  effective.
          The nonstationary methods are based on the idea of sequences of orthogonal vectors.

          The rate at which an iterative method converges depends greatly on the spectrum of the coefficient matrix. Hence, iterative
          methods usually involve a second matrix that transforms the coefficient matrix into one with a more favorable spectrum.
          The transformation matrix is called a pre conditioner. A good precondition improves the convergence of the iterative method,
          sufficiently to overcome the extra cost of constructing and applying the pre conditioner. Indeed, without a pre conditioner the
          iterative method may even fail to converge.


          <b>Non stationary methods:</b>

          <u>Conjugate Gradient (CG)</u><para/>
          The conjugate gradient method derives its name from the fact that it generates a sequence of conjugate (or orthogonal) vectors.
          These vectors are the residuals of the iterates. They are also the gradients of a quadratic functional, the minimization of which is
          equivalent to solving the linear system. CG is an extremely effective method when the coefficient matrix is symmetric positive definite,
          since storage for only a limited number of vectors is required.

          <u>Conjugate Gradient on the Normal Equations</u><para/>
          These methods are based on the application of the CG method to one of two forms of the normal equations for Ax = b. CGNE solves the
          system (AA T )y = b for y and then computes the solution x = A'y. When the coefficient matrix A is nonsymmetric and nonsingular, the
          normal equations matrices AA' and A'A will be symmetric and positive definite, and hence CG can be applied. The convergence may be slow,
          since the spectrum of the normal equations matrices will be less favorable than the spectrum of A.

          <u>Generalized Minimal Residual (GMRES)</u><para/>
          The Generalized Minimal Residual method computes a sequence of orthogonal vectors (like minimum residual), and combines these through a
          least-squares solve and update. However, unlike MINRES (and CG) it requires storing the whole sequence, so that a large amount of storage
          is needed. For this reason, restarted versions of this method are used. In restarted versions, computation and storage costs are limited
          by specifying a fixed number of vectors to be generated. This method is useful for general nonsymmetric matrices.

          <u>BiConjugate Gradient (BiCG)</u><para/>
          The Biconjugate Gradient method generates two CG-like sequences of vectors, one based on a system with the original coefficient matrix A,
          and one on A T . Instead of orthogonalizing each sequence, they are made mutually orthogonal, or "bi-orthogonal". This method, like CG,
          uses limited storage. It is useful when the matrix is nonsymmetric and nonsingular; however, convergence may be irregular, and there is
          a possibility that the method will break down. BiCG requires a multiplication with the coefficient matrix and with its transpose at each
          iteration.<para/>                     

          <b>Stationary methods:</b> (converge very slow and are provided only as a reference)

          <u>Jacobi</u><para/>
          The Jacobi method is based on solving for every variable locally with respect to the other variables; one iteration of the method corresponds
          to solving for every variable once. The resulting method is easy to understand and implement, but convergence is slow.

          <u>Gauss-Seidel</u><para/>
          The Gauss-Seidel method is like the Jacobi method, except that it uses updated values as soon as they are  available. In general, if the Jacobi
          method converges, the Gauss-Seidel method will converge faster than the Jacobi method, though still relatively slowly.<para/>

          <u>Reference:</u> Templates for the Solution of Linear Systems: Building Blocks for Iterative Methods 1, R. Barrett, M. Berry, T. F. Chan,
          J. Demmel, J. Donato, J. Dongarra, V. Eijkhout, R. Pozo, C. Romine, and H. Van der Vorstg , SIAM, 1994,  Philadelphia, PA.
</remarks>


          

          <example>
          <code>
          {
              TMtxSparse SparseA = new TMtxSparse();
              Vector x = new Vector(0);
              Vector b = new Vector(0);

              // load data
              SparseA.LoadFromMatrixFile("system.mtx");
              b.LoadFromFile("coefficients.Vec");
              // set solution size
              x.Size(b);
              // define iterative method - in this case Jacobi iterative method
              SparseA.RefinementIters = 4;
              SparseA.IterativeMethod = itmJAC;
              // solve
              SparseA.SolveIterative(b,x);
              // view solution
              MtxVecEdit.ViewValues(x);
          }
          </code></example>

          <SeeAlso cref="IterativeMethod"/>
          <SeeAlso cref="MaxIters"/>
          <SeeAlso cref="RefinmentIters"/>*)
        procedure SolveIterative(const B,X: TMtx; Symmetric: boolean = false); overload;
        (*<summary>Vector version of SolveIterative.</summary>*)
        procedure SolveIterative(const B,X: TVec; Symmetric: boolean = false); overload;

        (*<summary> Computes eigenvalues and eigenvectors for symmetric (hermitian) sparse matrix. </summary>
                       <param name="D"> Returns the eigenvalues. </param>
                       <param name="R"> Returns the  relative residual vector </param>
                       <param name="EpsOut"> Contains the relative error on the trace: |trac[i] - trace[i-1]|/Max(|Maximum|, |sMinimum|) </param>
                       <param name="V"> Returns the eigenvectors in rows. On Input, the matrix can contain estimate of eigenvectors, otherwise V.Length must be zero. </param>
                       <param name="Minimum"> Start of the search interval. </param>
                       <param name="Maximum"> Stop of the search interval. </param>
                       <param name="EigCount"> Contains estimated eigenvalue on input and actual count on return. </param>
                       <param name="fpm"> Processing parameter list. Leave nil, to use default values. </param>
                       <returns>
                        The function will return:
                        <list>
                        <item> 0 on success. </item>
                        <item> 1 no eigenvalues found in search interval. Try to scale up/down the matrix: (A/t) x=(Lambda/t) x  </item>
                        <item> 2 in case of no convergence (maximum iteration loops specified in fpm(4) exceeded) </item>
                        <item> 3 There are more eigenvalues present than have been estimated with EigCount </item>
                        </list>
                      </returns>

                      <remarks>
                      To compute all eigenvalues and eigenvectors would require storage equal to the size of
                      the dense matrix. For this reason, the routine allows computation of eigenvectors and
                      eigenvalues only within a specified range. The expected number of eigenvalues within
                      the Interval [Minimum, Maximum] is specified with EigCount. If the function returns with
                      a different EigCount, the initial estimate needs to be adjusted, because there was not
                      enough storage to store the result.
                      <para/>

                      The quadratic sparse matrix is expected to store only lower triangular part including the main diagonal.
                      </remarks>

                      <SeeAlso href="http://www.ecs.umass.edu/~polizzi/feast"/>*)

        function EigSym(const D, R: TVec; const V: TMtx; var EigCount: integer; var EpsOut: double; Minimum, Maximum: double; var fpm: TIntegerArray): integer; overload;

        (*<summary> Computes largest eigenvalues and eigenvectors for symmetric sparse matrix. </summary>

                      
<remarks>The sparse matrix must be symmetric and it must use full storage.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="D"> Returns the eigenvalues. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="EigVectors"> Returns the eigenvectors in rows. Pass nil for this paramter, if you don't require eigen-vectors, </param>
                      <param name="DesiredEigCount"> The function will return up to DesiredEigCount largest eigen values. </param>
                      <param name="fpm"> Processing parameter list. Leave nil, to use default values.

                      Alternatively, it is possible to specify an array of parameters "fpm" as the last parameter to the routine (Reference: Intel MKL manual).

                      Optional: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).

                      "fpm[0]" 0 Reserved for future use.
                      "fpm[1]" 6 Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]" 0 Specifies the algorithm to use:
                                 • 0 - Decided at runtime
                                 • 1 - The Krylov-Schur method
                                 • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]" This parameter is referenced only for Krylov-Schur Method. It indicates the
                              number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                              This parameter must be less than or equal to size of matrix and greater than
                              number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                              least 1.5 times larger than k0.
                      "fpm[4]" Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                              the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]" 0 Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]" 1 Used only for Krylov-Schur Method.
                              If 0, then the method only computes eigenvalues.
                              If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                              the required memory space.
                      "fpm[7]" Convergence stopping criteria.
                      "fpm[8]" Specifies if for detecting convergence the solver must compute the true residuals
                              for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                              estimates.
                              If 0, only residual norm estimates are used.
                              If 1, the solver computes not just residual norm estimates but also the true
                              residuals as defined in the description of pm[7].
                      "fpm[9]" Used only for the Krylov-Schur method and only as an output parameter.
                               Reports the reason for exiting the iteration loop of the method:
                               • If 0, the iterations stopped since convergence has been detected.
                               • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                               • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                               • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                               • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).
                      "fpm[10..128]">reserved
                      </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                      </returns>*)
        function EigSymLargest(const D, Residuals: TVec; const EigVectors: TMtx; DesiredEigCount: integer; const fpm: TIntegerArray = nil): integer; overload;
        (*<summary> Computes smallest eigenvalues and eigenvectors for a symmetric sparse matrix. </summary>

                      
<remarks>The sparse matrix must be symmetric and it must use full storage.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="D"> Returns the eigenvalues. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="EigVectors"> Returns the eigenvectors in rows. Pass nil for this paramter, if you dont require eigen-vectors, </param>
                      <param name="DesiredEigCount"> The function will return up to DesiredEigCount smallest eigen values. </param>
                      <param name="fpm"> Optiona: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).
                      "fpm[0]"> 0 Reserved for future use.
                      "fpm[1]"> 6 Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]"> 0 Specifies the algorithm to use:
                                           • 0 - Decided at runtime
                                           • 1 - The Krylov-Schur method
                                           • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]"> This parameter is referenced only for Krylov-Schur Method. It indicates the
                            number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                            This parameter must be less than or equal to size of matrix and greater than
                            number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                            least 1.5 times larger than k0.
                      "fpm[4]"> Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                            the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]"> 0 Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]"> 1 Used only for Krylov-Schur Method.
                            If 0, then the method only computes eigenvalues.
                            If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                            the required memory space.

                      "fpm[7]"> Convergence stopping criteria.
                      "fpm[8]"> Specifies if for detecting convergence the solver must compute the true residuals
                                            for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                                            estimates.
                                            If 0, only residual norm estimates are used.
                                            If 1, the solver computes not just residual norm estimates but also the true
                                            residuals as defined in the description of pm[7].
                      "fpm[9]"> Used only for the Krylov-Schur method and only as an output parameter.
                                           Reports the reason for exiting the iteration loop of the method:
                                           • If 0, the iterations stopped since convergence has been detected.
                                           • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                                           • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                                           • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                                           • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).
                      "fpm[10..128]">reserved
                      </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                      </returns>*)
        function EigSymSmallest(const D, Residuals: TVec; const EigVectors: TMtx; DesiredEigCount: integer; const fpm: TIntegerArray = nil): integer; overload;

        (*<summary> Computes generalized largest eigenvalues and eigenvectors for symmetric sparse matrix. </summary>

                      
<remarks>Solves: Ax = lambda Bx

                      The sparse matrix must be symmetric and it must use full storage.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="B"> The sparse matrix B, which must be symmetric, positive definite and have full storage. </param>
                      <param name="D"> Returns the eigenvalues. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="EigVectors"> Returns the eigenvectors in rows. Pass nil for this paramter, if you don't require eigen-vectors, </param>
                      <param name="DesiredEigCount"> The function will return up to DesiredEigCount largest eigen values. </param>
                      <param name="fpm"> Processing parameter list. Leave nil, to use default values.

                      Alternatively, it is possible to specify an array of parameters "fpm" as the last parameter to the routine (Reference: Intel MKL manual).

                      "fpm Optional: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).

                      "fpm[0]" 0 Reserved for future use.
                      "fpm[1]" 6 Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]" 0 Specifies the algorithm to use:
                                           • 0 - Decided at runtime
                                           • 1 - The Krylov-Schur method
                                           • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]" This parameter is referenced only for Krylov-Schur Method. It indicates the
                            number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                            This parameter must be less than or equal to size of matrix and greater than
                            number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                            least 1.5 times larger than k0.
                      "fpm[4]" Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                            the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]" 0 Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]" 1 Used only for Krylov-Schur Method.
                            If 0, then the method only computes eigenvalues.
                            If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                            the required memory space.

                      "fpm[7]" Convergence stopping criteria.
                      "fpm[8]" Specifies if for detecting convergence the solver must compute the true residuals
                                            for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                                            estimates.
                                            If 0, only residual norm estimates are used.
                                            If 1, the solver computes not just residual norm estimates but also the true
                                            residuals as defined in the description of pm[7].
                      "fpm[9]" Used only for the Krylov-Schur method and only as an output parameter.
                                           Reports the reason for exiting the iteration loop of the method:
                                           • If 0, the iterations stopped since convergence has been detected.
                                           • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                                           • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                                           • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                                           • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).

                      "fpm[10..128]" reserved
                       </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                      </returns>*)

        function EigSymGenLargest(const B: TSparseMtx; const D, Residuals: TVec; const EigVectors: TMtx; DesiredEigCount: integer; const fpm: TIntegerArray = nil): integer;

        (*<summary> Computes smallest generalized eigenvalues and eigenvectors for a symmetric sparse matrix. </summary>

                      
<remarks>Solves: Ax = lambda Bx

                      The sparse matrix must be symmetric and it must use full storage. The function will return up to EigCount smallest eigen values.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="B"> The sparse matrix B, which must be symmetric, positive definite and have full storage. </param>
                      <param name="D"> Returns the eigenvalues. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="EigVectors"> Returns the eigenvectors in rows. Pass nil for this paramter, if you dont require eigen-vectors, </param>
                      <param name="DesiredEigCount"> The function will return up to DesiredEigCount smallest eigen values. </param>
                      <param name="fpm"> Optiona: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).

                      "fpm[0]" Reserved for future use.
                      "fpm[1]" Default 6. Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]" Default 0. Specifies the algorithm to use:
                                           • 0 - Decided at runtime
                                           • 1 - The Krylov-Schur method
                                           • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]" This parameter is referenced only for Krylov-Schur Method. It indicates the
                            number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                            This parameter must be less than or equal to size of matrix and greater than
                            number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                            least 1.5 times larger than k0.
                      "fpm[4]" Default 0. Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                            the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]" Default 0. Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]" Default 1. Used only for Krylov-Schur Method.
                            If 0, then the method only computes eigenvalues.
                            If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                            the required memory space.

                      "fpm[7]" Convergence stopping criteria.
                      "fpm[8]" Specifies if for detecting convergence the solver must compute the true residuals
                                            for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                                            estimates.
                                            If 0, only residual norm estimates are used.
                                            If 1, the solver computes not just residual norm estimates but also the true
                                            residuals as defined in the description of pm[7].
                      "fpm[9]" Used only for the Krylov-Schur method and only as an output parameter.
                                           Reports the reason for exiting the iteration loop of the method:
                                           • If 0, the iterations stopped since convergence has been detected.
                                           • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                                           • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                                           • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                                           • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).

                      "fpm[10..128]"reserved
                      </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                      </returns>*)

        function EigSymGenSmallest(const B: TSparseMtx; const D, Residuals: TVec; const EigVectors: TMtx; DesiredEigCount: integer; const fpm: TIntegerArray = nil): integer;

        (*<summary> Computes largest singular values and signular vectors for symmetric sparse matrix. </summary>

                      
<remarks>The sparse matrix must be symmetric and it must use full storage.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="S"> Returns the singular values. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="V"> Returns left singular vectors in rows. Pass nil for this paramter, if you dont require singular vectors, </param>
                      <param name="DesiredSCount"> The function will return up to DesiredSCount largest singular values. </param>
                      <param name="RightVectors"> When V is not nil, left singular vectors are returned by default. Set this parameter to True, to obtain right singular vectors. </param>
                      <param name="fpm"> Processing parameter list. Leave nil, to use default values.

                      Alternatively, it is possible to specify an array of parameters "fpm" as the last parameter to the routine (Reference: Intel MKL manual).

                      "fpm" Optional: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).
                      "fpm[0]" 0. Reserved for future use.
                      "fpm[1]" 6. Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]" 0. Specifies the algorithm to use:
                                           • 0 - Decided at runtime
                                           • 1 - The Krylov-Schur method
                                           • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]" This parameter is referenced only for Krylov-Schur Method. It indicates the
                            number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                            This parameter must be less than or equal to size of matrix and greater than
                            number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                            least 1.5 times larger than k0.
                      "fpm[4]" Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                            the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]" 0. Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]" 1. Used only for Krylov-Schur Method.
                            If 0, then the method only computes eigenvalues.
                            If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                            the required memory space.

                      "fpm[7]" Convergence stopping criteria.
                      "fpm[8]" Specifies if for detecting convergence the solver must compute the true residuals
                                            for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                                            estimates.
                                            If 0, only residual norm estimates are used.
                                            If 1, the solver computes not just residual norm estimates but also the true
                                            residuals as defined in the description of pm[7].
                      "fpm[9]" Used only for the Krylov-Schur method and only as an output parameter.
                                           Reports the reason for exiting the iteration loop of the method:
                                           • If 0, the iterations stopped since convergence has been detected.
                                           • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                                           • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                                           • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                                           • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).

                      "fpm[10..128]" reserved
                      </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                       It is possible, that the function does not return an error, but the result is not correct, if the requsted tolerance is to high (fpm[1] is too big, asking for too small value).
                      </returns>*)
        function SvdSymLargest(const S, Residuals: TVec; const V: TMtx; DesiredSCount: integer; RightVectors: boolean = false; const fpm: TIntegerArray = nil): integer; overload;
        (*<summary> Computes smallest singular values and signular ectors for a symmetric sparse matrix. </summary>

                      
<remarks>The sparse matrix must be symmetric and it must use full storage.

                      Important:
                      * The routine does not check, if the matrix is symmetric.
                      * Inclusion of explicit zero valued elements could cause access violation.
</remarks>


                      <param name="S"> Returns the singular values. </param>
                      <param name="Residuals"> Returns the  relative residual vector </param>
                      <param name="V"> Returns left singular vectors in rows. Pass nil for this paramter, if you dont require singular vectors, </param>
                      <param name="DesiredSCount"> The function will return up to DesiredSCount smallest singular values. </param>
                      <param name="RightVectors"> When V is not nil, left singular vectors are returned by default. Set this parameter to True, to obtain right singular vectors. </param>
                      <param name="fpm"> Optional: processing parameter list. Leave nil, to use default values. If you do pass this parameter, its length must be minimum 129 items. (Reference: Intel MKL manual).

                      "fpm[0]" 0. Reserved for future use.
                      "fpm[1]" 6. Defines the tolerance for the stopping criteria: tol = 10-pm 1 + 1
                      "fpm[2]" 0. Specifies the algorithm to use:
                                           • 0 - Decided at runtime
                                           • 1 - The Krylov-Schur method
                                           • 2 - Subspace Iteration technique based on FEAST algorithm
                      "fpm[3]" This parameter is referenced only for Krylov-Schur Method. It indicates the
                            number of Lanczos/Arnoldi vectors (NCV) generated at each iteration.
                            This parameter must be less than or equal to size of matrix and greater than
                            number of eigenvalues (k0) to be computed. If unspecified, NCV is set to be at
                            least 1.5 times larger than k0.
                      "fpm[4]" Maximum number of iterations. If unspecified, this parameter is set to 10000 for
                            the Krylov-Schur method and 60 for the subspace iteration method.
                      "fpm[5]" 0. Power of Chebychev expansion for approximate spectral projector. Only referenced when pm[2]=1
                      "fpm[6]" 1. Used only for Krylov-Schur Method.
                            If 0, then the method only computes eigenvalues.
                            If 1, then the method computes eigenvalues and eigenvectors. The subspace iteration method always computes eigenvectors/singular vectors. You must allocate
                            the required memory space.

                      "fpm[7]" Convergence stopping criteria.
                      "fpm[8]" Specifies if for detecting convergence the solver must compute the true residuals
                                            for eigenpairs for the Krylov-Schur method or it can only use the residual norm
                                            estimates.
                                            If 0, only residual norm estimates are used.
                                            If 1, the solver computes not just residual norm estimates but also the true
                                            residuals as defined in the description of pm[7].
                      "fpm[9]" Used only for the Krylov-Schur method and only as an output parameter.
                                           Reports the reason for exiting the iteration loop of the method:
                                           • If 0, the iterations stopped since convergence has been detected.
                                           • If -1, maximum number of iterations has been reached and even the residual norm estimates have not converged.
                                           • If -2, maximum number of iterations has been reached despite the residual norm estimates have converged (but the true residuals for eigenpairs have not).
                                           • If -3, the iterations stagnated and even the residual norm estimates have not converged.
                                           • If -4, the iterations stagnated while the eigenvalues have converged (but the true residuals for eigenpairs do not).

                      "fpm[10..128]"reserved
                      </param>

                       <returns>
                       The function will return the number of actually found eigen-values. If there was an error during evaluation, an exception will be raised with an error message.
                      </returns>*)
        function SvdSymSmallest(const S, Residuals: TVec; const V: TMtx; DesiredSCount: integer; RightVectors: boolean = false; const fpm: TIntegerArray = nil): integer; overload;


        (*<summary> Computes eigenvalues and eigenvectors for generalized symmetric (hermitian) sparse problem. </summary>
                       <param name="B"> The symmetric positive definite matrix. </param>
                       <param name="D"> Returns the eigenvalues. </param>
                       <param name="R"> Returns the  relative residual vector </param>
                       <param name="EpsOut"> Returns the contains the relative error on the trace: |trac[i] - trace[i-1]|/Max(|Maximum|, |sMinimum|) </param>
                       <param name="V"> Returns the eigenvectors in rows. Pass nil for this paramter, if you dont require eigen-vectors, </param>
                       <param name="Minimum"> Start of the search interval. </param>
                       <param name="Maximum"> Stop of the search interval. </param>
                       <param name="EigCount"> Contains estimated eigenvalue on input and actual count on return. </param>
                       <param name="fpm"> Processing parameter list. Leave nil, to use default values. </param>
                       <returns>
                        The function will return:
                        <list>
                        <item> 0 on success. </item>
                        <item> 1 no eigenvalues found in search interval. Try to scale up/down the matrix: (A/t) x=(Lambda/t) x  </item>
                        <item> 2 in case of no convergence (maximum iteration loops specified in fpm(4) exceeded) </item>
                        <item> 3 There are more eigenvalues present than have been estimated with EigCount </item>
                        </list>
                       </returns>

                      <remarks>
                      To compute all eigenvalues and eigenvectors would require storage equal to the size of
                      the dense matrix. For this reason, the routine allows computation of eigenvectors and
                      eigenvalues only within a specified range. The expected number of eigenvalues within
                      the Interval [Minimum, Maximum] is specified with EigCount. If the function returns with
                      a different EigCount, the initial estimate needs to be adjusted, because there was not
                      enough storage to store the result.
                      <para/>
                      Matrix A is expected to be symmetric and B must be symmetric and positive definite (Hermitian).
                      Both matrices are expected to store only lower triangular part. Size of A and B is expected to be
                      equal and both matrices are to be quadratic.
                      </remarks>

                      <SeeAlso href="http://www.ecs.umass.edu/~polizzi/feast"/>*)

        function EigSymGen(const B: TSparseMtx; const D, R: TVec; const V: TMtx; var EigCount: integer; var EpsOut: double; Minimum, Maximum: double; var fpm: TIntegerArray): integer; overload;


        (*<summary>Convert sparse matrix to banded matrix.</summary>
          
<remarks>Convert calling matrix, stored in HB sparse matrix format, to banded matrix format. The result (banded matrix format) is stored in Dst matrix.
          The size and complex properties of Dst matrix are adjusted automatically.
</remarks>


          <example><b>Step 1:</b> construct banded matrix<para/>
          <code>
          var subDiag, SupDiag,MainDiag: Vector;
          A: Matrix;
          begin
            SubDiag.Size(4);
            SuperDiag.Size(4);
            MainDiag.Size(4);
            A.Size(3,4);
            A.SubDiag := 1; // one subdiagonal
            A.SuperDiag := 1; // one superdiagonal
            SubDiag.SetIt([0,1,3,2]);
            SupDiag.SetIt([-1,2,7,0]);
            MainDiag.SetIt([2,3,5,1]);
            A.SetRow(SupDiag,0);
            A.SetRow(MainDiag,1);
            A.SetRow(SubDiag,2);
          end;
          </code><para/>
          <b>Step 2:</b> convert banded matrix to sparse matrix:<para/>
          <code>
            SparseA.BandedToSparse(A);
          end;
          </code>
          </example>

          <SeeAlso cref="BandedToSparse"/>*)
        procedure SparseToBanded(const Dst: TMtx; MaxElemCount: integer = 10000000);

        (*<summary>Convert sparse matrix to banded matrix.</summary>
          
<remarks>Convert sparse matrix modified compressed column format to dense matrix.
</remarks>


          <example><b>Step 1:</b> Construct dense matrix<para/>
          <code>
          var A,B: TMtx;
          SparseA: TSparseMtx;
          // ...
          A.SetIt(3,3,false,[1,0,0,0,2,0,0,1,3]);
          </code><para/>
          <b>Step 2:</b> Convert dense matrix to sparse matrix<para/>
          <code>
          // ...
          SparseA.DenseToSparse(A);
          </code><para/>
          <b>Step 3:</b> Convert sparse matrix back to dense and compare<para/>
          <code>
          // ...
          SparseA.SparseToDense(B);
          if not A.Equal(B,1.0e-5) then ERaise('Not Equal!');
          </code>
          </example>

          <SeeAlso cref="DenseToSparse"/>*)
        procedure SparseToDense(const Dst: TMtx; MaxElemCount: integer = 10000000);

        (*<summary>Convert sparse matrix to triplets.</summary>
          
<remarks>A sparse matrix can also be presented as pairs of the three elements called triplets. For each non zero value
          in the matrix we specify it's position: Row, Column, and it's Value. SparseToTriplets will convert the calling
          sparse matrix data to the triplets format. The data will be sorted first by columns and then by rows. Triplets
          can be represented by three arrays: Row, Column and Values or they can all be stored in one TMtx matrix.
</remarks>


          

          <example>
          <code>
          TSparseMtx sparseA = new TSparseMtx();
          TSparseMtx sparseB = new TSparseMtx();

          int[] R1 = new int[sparseA.NonZeros];
          int[] C1 = new int[sparseA.NonZeros];
          double[] V1 = new double[sparseA.NonZeros];

          Matrix A = new Matrix(0,0);

          sarseA.SparseToTriplets(ref R1,ref C1,ref V1);
          sparseB.TripletsToSparse(sparseA.Cols,sparseA.Cols,R1,C1,V1,0.0);

          sparseA.SparseToDense(A,10000000);
          if (!sparseB.Equal(sparseA,1.0E-5)) throw("Not equal");
          if (!sparseA.Equal(A,0,true)) throw("Not equal");

          sparseA.SparseToTriplets(A,false);
          sparseB.TripletsToSparse(sparseA.Cols,sparseA.Cols,A);
          if (!sparseA.Equal(sparseB)) throw("Not equal");
          </code></example>

          <SeeAlso cref="TripletsToSparse"/>*)
        procedure SparseToTriplets(var DstRow, DstColumn: TIntegerArray; var DstValues: TDoubleArray); overload;
        procedure SparseToTriplets(var DstRow, DstColumn: TIntegerArray; var DstValues: TSingleArray); overload;
        (*<summary>Convert sparse matrix to triplets (complex version).</summary>*)
        procedure SparseToTriplets(var DstRow, DstColumn: TIntegerArray; var DstCValues: TCplxArray); overload;
        procedure SparseToTriplets(var DstRow, DstColumn: TIntegerArray; var DstCValues: TSCplxArray); overload;
        (*<summary>Convert sparse matrix to triplets.</summary>
          
<remarks>Triplets are stored in the first three rows of the matrix. First row stores
          the row indices, the second stores the column indices and the third row stores
          the matrix values.  In case of a complex matrix and if ImInRow is True,
          the imaginary part's of the complex numbers are stored in the fourth row.
</remarks>
*)
        procedure SparseToTriplets(const DstTriplets: TMtx; ImInRow: boolean = False); overload;

        (*<summary>Split sparse matrix into lower triangle, main diagonal and upper triangle.</summary>
          
<remarks>Splits the calling sparse matrix in to lower triangle, main diagonal and upper triangle. LowerTriang sparse matrix will
          then contain the lower triangle of the sparse matrix with or without the diagonal depending on the DiagWithLower parameter.
          UpperTriang sparse matrix will contain the upper diagonal of the calling sparse matrix.
</remarks>


          <SeeAlso cref="AddSplit"/>*)
        procedure Split(const LowerTriang: TSparseMtx; const Diag: TVec; const UpperTriang: TSparseMtx; DiagWithLower: boolean = False);

        (*<summary>Form a full symmetric matrix from upper half with diagonal included.</summary>
          
<remarks>Forms full storage sparse symmetric matrix from the upper half of the matrix.
</remarks>
*)
        procedure ExpandSymmetric;

        (*<summary>Convert strings to specific matrix format.</summary>
          
<remarks>Convert strings to specific matrix format. Currently only Matrix market format is supported.
</remarks>


          <SeeAlso cref="ValuesToStrings"/>*)
        procedure StringsToValues(const Strings: TStrings; Format: TSparseFormat = spfMatrixMarket);

        (*<summary>Subract two sparse matrices.</summary>
          
<remarks>Subtract each of Sparse2 elements from the corresponding elements in matrix Sparse1 (matrix subtraction). The results
          are stored in the calling sparse matrix. Size and <see cref="TMtxVec.Complex"/>  properties of the calling matrix are set
          implicitly to match Sparse1 and Sparse2 matrices.
</remarks>


          <SeeAlso cref="Add"/>*)
        function Sub(const sLeft, sRight: TSparseMtx): TSparseMtx; overload;

        (*<summary>The residual norm of the solution of the system of linear equations A*X=B.</summary>
          <param name="B">Defines B vector in the <c>A*X=B</c> equation.</param>
          <param name="X">Defines X vector in the <c>A*X=B</c> equation.</param>
          <param name="Transp">If true, treat A (calling matrix) as transposed matrix.</param>
          <returns>the residual norm of the solution of the system of linear equations <c>A*X=B</c>, where A is calling sparse matrix.</returns>*)
        function  ResidualNorm(const B, X: TVec; Transp: boolean = False): double;

        (*<summary>Creates quadratic non-complex non-singular sparse matrix.</summary>
          
<remarks>Creates general/symmetric quadratic non-complex non-singular sparse matrix. It returns the right hand side in vector
          Y and the solution in vector X. If Symmetric is true the method will generate a matrix with symmetric pattern. The <paramref name="FillInPercent"/>
          parameter defines the percentage of non-zero in the sparse matrix. The MaxNonZeroCount parameter defines an upper limit to
          prevent against excessive memory usage (consequence of invalid user parameters) and consecutive system lock up.

          Note:
            You have to set <see cref="Cols"/> property to definethe size of the matrix <b>before</b> calling this routine.
</remarks>


          

          <example>
          <code>
          using Dew.Math;
          using Dew.Math.Units;
          using Dew.Math.Tee;

          namespace Dew.Examples
          {
            private void Example(MtxGridSeries GridSeries)
            {
              //  ...
              SparseA.Size(7500,4000,4000,false);
              SparseA.RandomSparse(v1,v2);
              SparseA.PixelDownSample(A);
              MtxVecTee.DrawValues(A,GridSeries); // showing A
            }
          }
          </code></example>*)
        procedure RandomSparse(const X,Y: TVec; Symmetric: boolean = false; FillInPercent: double = 33; MaxNonZeroCount: integer = 10000000); overload;

        (*<summary>Convert triplets to sparse format.</summary>
          
<remarks>A sparse matrix can also be presented as pairs of the three elements called triplets. For each non zero value in the matrix we specify
          it's position: Row, Column, and it's Value. MatrixSize defines the size of the matrix and sets the Size and Cols properties.
          The maximum row and column index may not exceed rowCount and colCount. This is usually the most
          efficient way to convert a given dense matrix to sparse format. The values do not need to be ordered, the same coordinates can appear more
          than once. All values having the same coordinates will be summed together. This routine can therefore also be used as a way to sum multiple
          sparse matrices. Triplets can be represented by three arrays: Row, Column and Values or they can all be stored in one TMtx matrix.

          By default any zeros on the main diagonal will be preserved. Set the last parameter to false to drop them on conversion.
</remarks>


          

          <example>
            Test sparseToTriplets, TripletsToSparse methods.
          <code>
          using Dew.Math;
          using Dew.Math.Units;

          namespace Dew.Examples
          {
            private void Example()
            {
              Matrix A = new Matrix(0,0);
              TSparseMtx SparseA = new TSparseMtx();
              TSparseMtx SparseB = new TSparseMtx();

              int[] R1 = new int[SparseA.NonZeros];
              int[] C1 = new int[SparseA.NonZeros];
              double[] V1 = new double[SparseA.NonZeros];

              SparseA.SparseToTriplets(ref R1,ref C1,ref V1);
              SparseB.TripletsToSparse(SparseA.Cols,SparseA.Cols,R1,C1,V1,0.0);

              SparseA.SparseToDense(A);
              if (!SparseB.Equal(SparseA,1E-5)) throw("Not equal");
              if (!SparseA.Equal(A,0,true)) throw("Not equal");

              SparseA.SparseToTriplets(A);
              SparseB.>TripletsToSparse(SparseA.Cols,SparseA.Cols,A,0.0);
              if (!SparseA.Equal(SparseB)) throw("Not equal");
            }
          }
          </code></example>

          <SeeAlso cref="SparseToTriplets"/>*)
        procedure TripletsToSparse(RowCount,ColCount: integer; const SrcRow, SrcColumn: TIntegerArray; const SrcValues: TDoubleArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true); overload;
        procedure TripletsToSparse(RowCount,ColCount: integer; const SrcRow, SrcColumn: TIntegerArray; const SrcValues: TSingleArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true); overload;
        (*<summary>Convert triplets to sparse format (complex version).</summary>*)
        procedure TripletsToSparse(RowCount,ColCount: integer; const SrcRow, SrcColumn: TIntegerArray; const SrcCValues: TCplxArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true); overload;
        procedure TripletsToSparse(RowCount,ColCount: integer; const SrcRow, SrcColumn: TIntegerArray; const SrcCValues: TSCplxArray; Threshold: double = 0; PreserveZeroDiagElem: boolean = true); overload;

        (*<summary>Convert triplets to sparse format.</summary>
          
<remarks>The triplets are stored in the Triplets matrix. First row are row indices,
          second row are column indices and the third row stores the values.
          If the matrix is complex, the row and column indicies are stored
          in the real part of the complex values.
</remarks>
*)
        procedure TripletsToSparse(RowCount,ColCount: integer; const SrcTriplets: TMtx); overload;

        (*<summary>Construct a sparse matrix by specifying the position of each element in dense matrix.</summary>
          
<remarks>This method can be used to construct a sparse matrix by specifying the position of each element in dense matrix.
</remarks>


          <example>
          <code>
          A[1,0] := 1;
          A[2,3] := 5;
          A[4,0] := 2;
          A.TripletsToSparseAndFlush;
          </code>
          If you later call :A.SparseToDense, you'll construct a dense matrix of form:
          <code>
          0 0 0 0
          1 0 0 0	 &lt;=  1,0
          0 0 0 5  &lt;=  2,3
          0 0 0 0
          2 0 0 0	 &lt;=  4,0
          </code>
          </example>

          <SeeAlso cref="AddSplit"/>*)
        procedure TripletsToSparseAndFlush;

        (*<summary>Transpose sparse matrix in-place.</summary>*)
        procedure Transp; overload;
        (*<summary></summary>*)
        procedure Transp(const Src: TSparseMtx; const P: TIntegerArray; const Q: TIntegerArray); overload;

        (*<summary>Transpose Src sparse matrix.</summary>
          
<remarks>Store the results in calling sparse matrix. Size and Complex properties of
          calling sparse matrix are adjusted automatically.
</remarks>
*)
        procedure Transp(const Src: TSparseMtx); overload;

        (*<summary>Extract upper part of the Mtx.</summary>
          
<remarks>Copies only the upper triangular part of the Mtx sparse matrix. The result is stored in the calling sparse matrix.
          If the Diagonal boolean parameter is true then the Mtx matrix main diagonal elements will be copied to the calling
          matrix main diagonal elements. If the Diagonal parameter is false, the calling matrix main diagonal elements will
          be set to zero.
</remarks>


          <example>
          <code>
          var asp, bsp: TSparseMtx;
            csp, dsp: TSparseMtx;
            d: Vector;
          begin
            // ...
            // #1 : split asp into lower(bsp), upper(csp) and diagonal(d) sections
            bsp.LowerTriangle(asp);
            csp.UpperTriangle(asp);
            asp.Diag(d);
            // now combine lower(bsp), upper, and diagonal(d) back
            dsp.AddSplit(bsp,d,csp);
            // should be equal
            if not asp.Equal(dsp) then Eraise('Not Equal!');
          end;
          </code>
          </example>

          <SeeAlso cref="LowerTriangle"/>*)
        function  UpperTriangle(const Src: TSparseMtx; Diagonal: boolean = false): TSparseMtx; overload;

        (*<summary>Reduces dense matrix for screen display (bitmap) to show the non-zero pattern.</summary>
          
<remarks>Creates a reduced size Dst dense matrix for screen display (bitmap) to show the non-zero pattern. Pixels parameter defines
          the target number of pixels to reduce <see cref="Rows"/> and <see cref="Cols"/> to. The Mode parameter defines how
          reduced matrix will be constructed.
</remarks>


          

          <example>
          <code>
          Vector v1 = new Vector(0);
          Vector v2 = new Vector(0);
          Matrix A = new Matrix(0,0);
          TSparseMtx SparseA = new TSparseMtx();
          // ...
          SparseA.Size(7500,4000,4000,false);
          SparseA.RandomSparse(v1,v2);
          SparseA.PixelDownSample(A);
          MtxVecTee.DrawValues(A,GridSeries); // showing A
          </code></example>

          <SeeAlso cref="TPixelDownSample"/>*)
        procedure PixelDownSample(const Dst: TMtx; Pixels: integer = 500; Mode: TPixelDownSample = pdsPattern);

        (*<summary>Convert matrix values to strings.</summary>
          
<remarks>Converts calling sparse matrix, stored in HB sparse format to Strings.

          Performance note:
            This routine will be exceedingly slow, if TRichEdit.Lines or TMemo.Lines are passed as a parameter for Verbose parameter. Use TStringList or StringList types and then
            call TMemo.Lines.AddStrings(yourList) for best results.
</remarks>


          <SeeAlso cref="StringsToValues"/>*)
        procedure ValuesToStrings(const Dst: TStrings; Format: TSparseFormat = spfMatrixMarket; ColumnVector: boolean = True); overload;
        
        
        procedure WriteHeader(const Dst: TStream;
                              Precision: TPrecision;
                              Rounding: TRounding = rnTrunc;
                              Endian: TEndianness = MtxSystemEndianness); override;
        function ReadHeader(const Src: TStream; Endian: TEndianness = MtxSystemEndianness): TPrecision; override;
        
        

        

        (*<summary>Saves the matrix in matrix market file format.</summary>
          <SeeAlso cref="LoadFromMatrixMarketFile"/>*)
        procedure SaveToMatrixMarketFile(const FileName: string);
        (*<summary>Loads the matrix stored in a matrix market file format.</summary>
          
<remarks>Loads the matrix stored in a Matrix Market file format.
</remarks>


          <SeeAlso cref="SaveToMatrixMarketFile"/>*)
        procedure LoadFromMatrixMarketFile(const FileName: string);

        (*<summary>Reduces the number of non-zeros.</summary>
          
<remarks>Reduces the number of non-zeros by rounding all values smaller than
          Threshold to zero. All zeros on the main diagonal are retained.
</remarks>
*)
        procedure ZeroThresh(Threshold: double = 0);

        (*<summary>Required by Pardiso solver to solve for general matrices.</summary>
          
<remarks>Pardiso solver can solve for general real/complex non-symmetric
          matrices only if they are first made structurally symmetric.
          StructurizeSymmetric method makes the matrix structurually symmetric,
          by inserting additional zeros in the matrix.
</remarks>
*)
        procedure StructurizeSymmetric;

        

        constructor Create; overload; override;
        destructor Destroy; override;

        (*<summary>Frees the memory of the Pardiso solver.</summary>
          
<remarks>If the parameter is -1, it frees all the allocated memory.
          If the parameter is bigger then 0 it frees the memory
          associated with the factorized matrix at that index.
</remarks>
*)
        procedure FreePardiso(AMatrixNumber: integer = -1);
        (*<summary>Releases factorization stored by taucs.</summary>*)
        procedure FreeTaucs;
     public

    


     published

        (*<summary>Number of rows in the sparse matrix.</summary>
          
<remarks>Defines number of rows in the sparse matrix. This value is needed only by the routines for matrix
          addition and multiplication to check the matrix sizes (which can be non-rectangular).
</remarks>


          

          <example>
          <code>
            SparseA.Complex = false;
            SparseA.Rows = 3;
            SparseA.Cols = 4;
            SparseA.NonZeros = 5;
          </code></example>

          <SeeAlso cref="Cols"/>*)
        property Rows: integer read FRows write SetRows;

        (*<summary>Number of columns in the sparse matrix.</summary>
          
<remarks>Defines number of columns in the sparse matrix. This value is needed only by the routines for matrix
          addition and multiplication to check the matrix sizes (which can be non-rectangular).
</remarks>


          <SeeAlso cref="Rows"/>*)
        property Cols: integer read FCols write SetCols;

        (*<summary>Defines the number of non-zero elements in the sparse matrix.</summary>*)
        property NonZeros: integer read fLength write setLength;

        (*<summary>Prevents zeros to be stripped.</summary>
          
<remarks>Prevents zeros to be stripped when forming a sparse matrix from triplets.
</remarks>
*)
        property DontStripZeros: boolean read FDontStripZeros write SetDontStripZeros;

        (*<summary>Specifies the sparse matrix solver to use.</summary>
          
<remarks>Specifies the sparse matrix solver to be used by the <see cref="Solve"/> method.
</remarks>


          <SeeAlso cref="Solve"/>*)
        property SparseSolver: TSparseSolver read FSparseSolver write SetSparseSolver;

        (*<summary>Defines if call to Solve will clear the report.</summary>
          
<remarks>If True, each consecutive call to Solve method will clear MtxVec.Report object. Report is a global class located in <see cref="MtxVec"/> unit.
          Be sure to reduce the level off reporting when solving large matrices, or the system will be busy just by printing reports.
</remarks>


          <SeeAlso cref="ReportLevel"/>*)
        property AutoClearReport: boolean read FAutoClearReport write SetAutoClearReport;

        (*<summary>Defines how detailed report will be printed.</summary>
          
<remarks>Defines how detailed report will be printed. Be careful not to set it to prlAll for larger sparse matrices, because then all non-zero elements
          of the sparse matrix will be printed to Report cllass, a global TStringList object located in <See xref="MtxVec"/> unit. The report is
          generated by Umfpack and can be very extensive, including everything from time needed to solve the system and number of flops to details
          about the path to the solution and many other useful parameters. Reports are generated only by <see cref="Solve"/> method.
</remarks>


          <example>
          <code>
          // load report from Report and display it in Memo1
          Memo1.Lines.Clear();
          MtxVec.Report.Position := 0;
          Memo1.Lines.LoadFromStream(MtxVec.Report.Stream);
          </code>
          <code lang="C++">
          // load report from Report and display it in Memo1
          Memo1->Lines.Clear();
          MtxVec->Report->Position = 0;
          Memo1->Lines->LoadFromStream(MtxVec->Report->Stream());
          </code>
          </example>

          <SeeAlso cref="Solve"/>*)
        property ReportLevel: TReportLevel read FReportLevel write SetReportLevel;

        (*<summary>Defines number of refinement steps the <see cref="Solve"/> method should try.</summary>
          
<remarks>Defines number of refinement steps the <see cref="Solve"/> method should try. Default value is 2.
          The number of actual refinement steps is returned in <see cref="ActualIters"/> property.
</remarks>


          

          <example>
          <code>
          Vector x = new Vector(0);
          Vector b = new Veector(0);
          TSparseMtx SparseMtx = new TSparseMtx();

          // ...
          x.size(b);
          SparseMtx.RefinementIters = 4; // set to four iterations
          SparseMtx.SolveIterative(b,x);
          int maxiter = SparseMtx.ActualIters;
          //  return the actual number of iterations used
          </code></example>

          <SeeAlso cref="Solve"/>
          <SeeAlso cref="ActualIters"/>*)
        property RefinmentIters: integer read FRefinmentIters write SetRefinmentIters;

        (*<summary>Defines the system to be solved by the Solve method.</summary>
          
<remarks>Defines the system to be solved by the <see cref="Solve"/> method.
</remarks>


          <SeeAlso cref="Solve"/>*)
        property SparseSystem: TSparseSystem read FSparseSystem write SetSparseSystem;

        (*<summary>The size of the block as used by UmfPack.</summary>
          
<remarks>This parameter gets/sets the <c>UMFPACK_BLOCK_SIZE</c> control parameter
          of the Umfpack sparse solver.
</remarks>
*)
        property SparseBlockSize: integer read FSparseBlockSize write SetSparseBlockSize;

        (*<summary>Defines the iterative method for solving system of equations.</summary>
          
<remarks>The term "iterative method" refers to a wide range of techniques that use successive  approximations to obtain more accurate
          solutions to a linear system at each step. There are two major groups of iterative methods:  stationary and nonstationary
          methods. Stationary methods are older, simpler to understand and implement, but usually not as effective. Nonstationary
          methods are a relatively recent development; their analysis is usually harder to understand, but they can be highly  effective.
          The nonstationary methods are based on the idea of sequences of orthogonal vectors.

          The rate at which an iterative method converges depends greatly on the spectrum of the coefficient matrix. Hence, iterative
          methods usually involve a second matrix that transforms the coefficient matrix into one with a more favorable spectrum.
          The transformation matrix is called a pre conditioner. A good precondition improves the convergence of the iterative method,
          sufficiently to overcome the extra cost of constructing and applying the pre conditioner. Indeed, without a pre conditioner the
          iterative method may even fail to converge.
</remarks>


          <SeeAlso cref="SolveIterative"/>*)
        property IterativeMethod: TIterativeMethod read FIterativeMethod write SetIterativeMethod;

        (*<summary>Convergence criteria for iterative method.</summary>
          
<remarks>Defines the convergence criteria used by the <see cref="SolveIterative"/> method.
</remarks>


          <SeeAlso cref="SolveIterative"/>
          <SeeAlso cref="Tolerance"/>*)
        property ConvergenceCheck: TConvergenceCheck read FConvergenceCheck write SetConvergenceCheck;

        (*<summary>Defines the tolerance for iterative method.</summary>
          
<remarks>This property defines the tlerance of the solution used as a parameter for the
          <see cref="SolveIterative"/> method.
</remarks>


          <SeeAlso cref="SolveIterative"/>
          <SeeAlso cref="ConvergenceCheck"/>*)
        property Tolerance: double read FTolerance write SetTolerance;

        (*<summary>Upper limit for number of iterations.</summary>
          
<remarks>Defines the maximum number of iterations performed by the <see cref="SolveIterative"/> method.
</remarks>


          <SeeAlso cref="SolveIterative"/>
          <SeeAlso cref="ConvergenceCheck"/>*)
        property MaxIters: integer read FMaxIters write SetMaxIters;

        (*<summary>Defines maximum number of vectors used in some iterative methods.</summary>
          
<remarks>Defines the maximum number of vectors used in case of the Generalized Minimal Residual (GMRES) and Orthmin
          method by the <see cref="SolveIterative"/> method.
</remarks>
*)
        property VectorCount: integer read FVectorCount write SetVectorCount;
        (*<summary>The pardiso sparse solver settings.</summary>*)
        property Pardiso: TPardisoSettings read FPardiso write SetPardiso;
        (*<summary>The Taucs sparse solver settings.</summary>*)
        property Taucs: TTaucsSettings read FTaucs write SetTaucs;
     end;

