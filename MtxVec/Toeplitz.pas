









                                          

(*<summary>Solvers for toeplitz matrices.</summary>
  
<remarks>Solvers for real and complex toeplitz matrices.
</remarks>
*)
unit Toeplitz;


interface

{$I BdsppDefs.inc}

uses AbstractMtxVec, MtxVec, Math387
      
      ,Nmkl
      

      
      ,NmklSingle
      

     
      ,SysUtils
     
      ;




(*<summary>Solve a toepltiz system of linear equations.</summary>
  <param name="FirstRow">Defines the first row <c>R(0)..R(N-1)</c> in the equation below.
    Vector can be real or complex.</param>
  <param name="FirstCol">Defines the first column <c>C(0)..C(N-2)</c> in the equation below.
    <c>FirstCol.Length</c> must be equal to <c>FirstRow.Length-1</c>. Vector can be real or complex.</param>
  <param name="B">Defines B vector in equation below.</param>
  <param name="X">Returns X vector as solution in the equation below.</param>

  
<remarks>The procedures solves Toeplitz system of linear equations defined as:

  <code>
     [  R(0)     R(1) ...    R(N-1)  ]       [ X(0)  ]  = [  -B(0)  ]
     [  C(0)     R(0)  ...   R(N-2)  ]       [ X(1)  ]  = [  -B(1)  ]
     [   ....        .         .     ] x     [   .   ]  = [    .    ]
     [  C(N-1) C(N-2) ...    R(1)    ]       [ X(N-2)]  = [ -B(N-2) ]
     [  C(N-2) C(N-1) ...    R(0)    ]       [ X(N-1)]  = [ -B(N-1) ]
  </code>

  The computational complexity of the algorithm is O(n2) as oposed to at least O(n3)
  for solving a general matrix.
</remarks>


    

<example>
<code>
using Dew.Math;
using Dew.Math.Units;
using Dew.Math.Editors;

namespace Dew.Examples
{
	private void Example()
	{

    Vector FirstRow = new Vector(0);
		Vector FirstCol = new Vector(0);
		Vector X1 = new Vector(0);
		Vector X2 = new Vector(0);
		Vector B = new Vector(0);
		Vector R = new Vector(0;
		Matrix A = new Matrix(0,0);

    FirstRow.SetIt(false,new double[] {1,2,3,4});
    FirstCol.SetIt(false,new double[] {2,3,4});
    A.Toeplitz(FirstRow,FirstCol);
		B.SetIt(false, new double[] {-2,-3,-4,-5});
    R.Copy(FirstRow);
		R.Resize(5);
    R.Values[4] = 5;
	  Toeplitz.ToeplitzSolve(FirstRow, FirstCol,B, X1);
	  A.LUSolve(B,X2,MtxVec.mtGeneral);
    MtxVecEdit.ViewValues(X1,"Real X1",true);
   	Toeplitz.Levinson(R,X2);
   	MtxVecEdit.ViewValues(X2,"Real X2",true);
	}
}
</code></example>

<Seealso cref="Levinson"/>*)

procedure ToeplitzSolve(FirstRow,FirstCol,B,X: TVec); overload;

(*<summary>Solves toeplitz system.</summary>
  <param name="FirstRow">Defines the first row <c>R(0)..R(N-1)</c> in the equation below.</param>
  <param name="B">Defines B vector in equation below.</param>
  <param name="X">Returns X vector in the equation below.</param>

  
<remarks>Solves toeplitz system, defined as:

 <code>

     [  R(0)     R(1)* ...   R(N-1)* ]       [ X[1]   ]  = [  -B[1]  ]
     [  R(1)     R(0)  ...   R(N-2)* ]       [ X[2]   ]  = [  -B[2]  ]
     [   ....        .         .     ]   x   [   .    ]  = [    .    ]
     [  R(N-2)   R(N-3) ...   R(1)*  ]       [ X[N-1] ]  = [ -B[N-1] ]
     [  R(N-1)   R(N-2) ...   R(0)   ]       [ X[N]   ]  = [ -B[N]   ]
 </code>
</remarks>
*)

procedure ToeplitzSolve(FirstRow,B,X: TVec); overload;

(*<summary>Solves a toeplitz system.</summary>
  <param name="A">The solution is stored in vector A. The first element of A at Index 0 is set to 1.
    The sign '*' stands for conjugation.</param>
  <param name="R">can be real or complex. <c>N</c> is equal to R.Length-1.</param>

  
<remarks>The procedures solves Toeplitz system of linear equations defined as:

  <code>
     [  R(0)     R(1)* ...   R(N-1)* ]       [ A[1]   ]  = [  -R[1]  ]
     [  R(1)     R(0)  ...   R(N-2)* ]       [ A[2]   ]  = [  -R[2]  ]
     [   ....        .         .     ]   x   [   .    ]  = [    .    ]
     [  R(N-2)   R(N-3) ...   R(1)*  ]       [ A[N-1] ]  = [ -R[N-1] ]
     [  R(N-1)   R(N-2) ...   R(0)   ]       [ A[N]   ]  = [ -R[N]   ]
  </code>

  The computational complexity of the algorithm is O(n2) as oposed to at least O(n3) for LUSolve.
</remarks>


  <seealso cref="ToeplitzSolve"/>*)

procedure Levinson(R,A: TVec);




