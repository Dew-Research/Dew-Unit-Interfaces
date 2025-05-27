











(*<summary>Special math functions </summary>
   
<remarks>Introduces several special functions:
    * Airy, Bessel functions,
    * Elliptic integrals,
    * Jacoby elliptic functions,
    * Associated Legendre polynomials.
</remarks>
*)
unit SpecialFuncs;


interface

{$I BdsppDefs.inc}

uses Math387, AbstractMtxVec
      
      ,Nmkl
      

      
      ,NmklSingle
      

      
      ,SysUtils
      

      
         ,Types 
      
      ;



(*<summary>Complete elliptic integral.</summary>
  <returns>the complete elliptic integrals of the first kind, evaluated at m.
    Parameter m must lie on the (0,1) interval, otherwise an exception is raised.</returns>
  <param name="m">Defines integral evaluation point, real value on closed interval [0,1].</param>*)

function  EllipComplete(m: double):double; overload;
function  EllipComplete(m: single):single; overload;

(*<summary>Complete elliptic integral, additional options.</summary>
  <returns>the complete elliptic integrals of the first kind, evaluated at m.
    Parameter m must lie on the (0,1) interval, otherwise an exception is raised.</returns>
  <param name="m">Defines integral evaluation point, real value on closed interval [0,1].</param>
  <param name="Epsilon">Defines the accuracy for evaluating elliptic integral of the second kind.</param>
  <param name="e">Returns the value of the complete elliptic integral of the second kind,
    evaluated at m, to the accuracy Epsilon.</param>

  <seealso cref="EllipJacoby"/>*)

function  EllipComplete(m, Epsilon: double; var e: double): double; overload;
function  EllipComplete(m, Epsilon: single; var e: single): single; overload;

(*<summary>Jacoby elliptic functions.</summary>
  
<remarks>The routine calculates the values of Jacoby elliptic functions sn, cn and dn, evaluated at u and
  using the parameter m.
</remarks>

  <param name="u">Defines value at which functions will be evaluated.</param>
  <param name="m">Defines integral evaluation point, real value on closed interval [0,1].</param>
  <param name="Epsilon">Defines the accuracy for evaluating complete elliptic integral.</param>
  <param name="sn">Returns Jacoby elliptic function <b>sn</b>.</param>
  <param name="cn">Returns Jacoby elliptic function <b>cn</b>.</param>
  <param name="dn">Returns Jacoby elliptic function <b>dn</b>.</param>

  <seealso cref="EllipComplete"/>*)

procedure EllipJacoby(u,m, Epsilon: double; var sn,cn,dn: double); overload;
procedure EllipJacoby(u,m, Epsilon: single; var sn,cn,dn: single); overload;

(*<summary>Airy function of the first kind (Real argument).</summary>
  <returns>the Airy function from the K Bessel functions.</returns>
  <param name="A">Defines real value where AI should be evaluated.</param>*)

function Airy(A: double): double; overload;
function Airy(A: single): single; overload;

(*<summary>Airy function of the first kind (Complex argument).</summary>
  <returns>the Airy function from the K Bessel functions.</returns>
  <param name="Z">Defines complex value where AI should be evaluated.</param>*)

function Airy(Z: TCplx): TCplx; overload;
function Airy(Z: TSCplx): TSCplx; overload;

(*<summary>Airy function or it's derivative of the first kind (Real argument, allows derivative and scale).</summary>
  <returns>the Airy function or it's derivative from the K Bessel functions.</returns>
  <param name="A">Defines complex value where AI or it's derivative should be evaluated.</param>
  <param name="Derive"> is true, it will return derivative of Airy function of the first kind.</param>
  <param name="Scale"> is true, result is additionaly multiplied by factor (see below)</param>
  <param name="Error">Returns error code after the calculation. Return values are:
  <list>
  <item> 0, Normal return - Computation completed.  </item>
  <item> 1, input error - no computation.  </item>
  <item> 2, overflow - no computation; dreal(zta)- too large with Scaling=false.  </item>
  <item> 3, zabs(z) large - computation completed. Losses of signifcance by argument reduction
      produce less than half of machine accuracy.  </item>
  <item> 4, zabs(z) too large  - no computation; complete loss of accuracy by argument reduction. </item>
  <item> 5, error - no computation; algorithm termination condition not met. </item>
  </list>
  </param>*)

function Airy(A: double; Derive, Scale: boolean; var Error: integer): double; overload;
function Airy(A: single; Derive, Scale: boolean; var Error: integer): single; overload;

(*<summary>Airy function of the first kind or it's derivative (Complex argument, allows derivative or scale.</summary>
  <returns>If Derive is false, it will return Airy function of the first kind. </returns>

  
<remarks>If <paramref name="Derive"/> is true, it will return derivative of Airy function of the first kind.
  If <paramref name="Scale"/> is true, result is additionaly multiplied by factor (see below)
</remarks>


  <param name="Z">Function independant variable, complex value.</param>
  <param name="Error">Returns error code after the calculation. Return values are:
  <list>
  <item> 0, Normal return - Computation completed.  </item>
  <item> 1, input error - no computation.  </item>
  <item> 2, overflow - no computation; dreal(zta)- too large with Scaling=false.  </item>
  <item> 3, zabs(z) large - computation completed. Losses of signifcance by argument reduction
      produce less than half of machine accuracy.  </item>
  <item> 4, zabs(z) too large  - no computation; complete loss of accuracy by argument reduction. </item>
  <item> 5, error - no computation; algorithm termination condition not met. </item>
  </list>
  </param>
  <param name="Derive">If true, function will return Airy derivative instead of the actual Airy (AI) function.</param>
  <param name="Scale">If true, result function or it's derivative is scaled by <c>Exp(z)</c>, where <c>z = (2/3)*(Z)^(3/2)</c></param>

  <seealso cref="Biry"/>
  <seealso cref="Besh"/>
  <seealso cref="Besj"/>
  <seealso cref="Besk"/>
  <seealso cref="Besy"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Airy(Z: TCplx; Derive, Scale: boolean; var Error: integer): TCplx; overload;
function Airy(Z: TSCplx; Derive, Scale: boolean; var Error: integer): TSCplx; overload;





(*<summary>Airy function of the second kind (Real argument).</summary>
  <returns>the Airy function Airy of the second kind (BI) for real argument X.</returns>
  <param name="A">Defines real value where BI should be evaluated.</param>*)

function Biry(A: double): double; overload;
function Biry(A: single): single; overload;

(*<summary>Airy function of the second kind (Complex argument).</summary>
  <returns>the Airy function Airy of the second kind (BI) for complex argument X.</returns>
  <param name="Z">Defines complex value where BI should be evaluated.</param>*)

function Biry(Z: TCplx): TCplx; overload;
function Biry(Z: TSCplx): TSCplx; overload;

(*<summary>Airy function or it's derivative of the second kind (Real argument, allows derivative and scale).</summary>
  <returns>the Airy function or it's derivative of the second kind (BI).</returns>
  <param name="A">Defines complex value where BI or it's derivative should be evaluated.</param>
  <param name="Derive">If true, function will return Airy (BI) derivative instead of the actual Airy (BI) function.</param>
  <param name="Scale">If true, result function (BI) or it's derivative is scaled by <c>Exp(-|z|*|z|)</c>, where <c>z = z = (2/3)*(Z)^(3/2)</c></param>
  <param name="Error">Returns error code after the calculation. Return values are:
  <list>
  <item> 0, Normal return - Computation completed. </item>
  <item> 1, input error - no computation. </item>
  <item> 2, overflow - no computation; dreal(zta)- too large with Scaling=false. </item>
  <item> 3, zabs(z) large - computation completed. Losses of signifcance by argument reduction
    produce less than half of machine accuracy. </item>
  <item> 4, zabs(z) too large  - no computation; complete loss of accuracy by argument reduction. </item>
  <item> 5, error - no computation; algorithm termination condition not met. </item>
  </list>
  </param>*)

function Biry(A: double; Derive, Scale: boolean; var Error: integer): double; overload;
function Biry(A: single; Derive, Scale: boolean; var Error: integer): single; overload;

(*<summary>Airy function of the second kind or it's derivative (Complex argument, allows derivative or scale.</summary>

   
<remarks>If <paramref name="Derive"/> is true, it will return derivative of Airy function of the <b>second</b> kind (BI).
   Additionaly, if <paramref name="Scale"/> is true, result is additionaly multiplied by factor (see above).
</remarks>


  <returns>If <paramref name="Derive"/> is false, it will return Airy function of the <b>second</b> kind (BI). </returns>

  <param name="Z">Function independant variable, complex value.</param>
  <param name="Error">Returns error code after the calculation. Return values are:
  <list>
  <item> 0, Normal return - Computation completed. </item>
  <item> 1, input error - no computation. </item>
  <item> 2, overflow - no computation; dreal(zta)- too large with Scaling=false. </item>
  <item> 3, zabs(z) large - computation completed. Losses of signifcance by argument reduction
    produce less than half of machine accuracy. </item>
  <item> 4, zabs(z) too large  - no computation; complete loss of accuracy by argument reduction. </item>
  <item> 5, error - no computation; algorithm termination condition not met. </item>
  </list>
  </param>
  <param name="Derive">If true, function will return Airy (BI) derivative instead of the actual Airy (BI) function.</param>
  <param name="Scale">If true, result function (BI) or it's derivative is scaled by <c>Exp(-|z|*|z|)</c>, where <c>z = z = (2/3)*(Z)^(3/2)</c></param>

  <seealso cref="Airy"/>
  <seealso cref="Besh"/>
  <seealso cref="Besj"/>
  <seealso cref="Besk"/>
  <seealso cref="Besy"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Biry(Z: TCplx; Derive, Scale: boolean; var Error: integer): TCplx; overload;
function Biry(Z: TSCplx; Derive, Scale: boolean; var Error: integer): TSCplx; overload;





(*<summary>Bessel function of the first kind Jn(Z) (Complex argument).</summary>
  <returns>the Bessel function of the first kind Jn(Z) with NU parameter describing order of Bessel function.</returns>
  <param name="Z">Defines complex value for which Jn(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the first kind Jn.</param>*)

function Besj(NU: double; Z: TCplx): TCplx; overload;
function Besj(NU: single; Z: TSCplx): TSCplx; overload;

(*<summary>Bessel function of the first kind Jn(Z) (Real argument).</summary>
  <returns>the Bessel function of the first kind Jn(Z) with NU parameter describing order of Bessel function.</returns>
  <param name="NU">Defines the order of the Bessel function of the first kind Jn.</param>
  <param name="A">Defines real value for which Jn(A) should be evaluated.</param>*)

function Besj(NU: double; A: double): TCplx; overload;
function Besj(NU: single; A: single): TSCplx; overload;

(*<summary>Bessel function of the first kind Jn(Z) (Complex argument, can be scaled).</summary>
  <returns>the Bessel function of the first kind Jn(Z) with NU parameter describing order of Bessel
    function. If Scale parameter is true, Jn(Z) is multiplied by <c>exp(-|Im(Z)|)</c></returns>
  <param name="NU">Defines the order of the Bessel function of the first kind Jn.</param>
  <param name="Z">Defines complex value for which Jn(Z) should be evaluated.</param>
  <param name="Scale">If Scale parameter is true, Jn(Z) is multiplied by <c>exp(-|Im(Z)|)</c>.</param>

  <seealso cref="Airy"/>
  <seealso cref="Biry"/>
  <seealso cref="Besh"/>
  <seealso cref="Besk"/>
  <seealso cref="Besy"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Besj(NU: double; Z: TCplx; Scale: boolean): TCplx; overload;
function Besj(NU: single; Z: TSCplx; Scale: boolean): TSCplx; overload;

(*<summary>Bessel function of the first kind Jn(A) (Real argument, can be scaled).</summary>
  <returns>the Bessel function of the first kind Jn(Z) with NU parameter describing order of Bessel
    function. If <paramref name="Scale"/> parameter is true, Jn(Z) is multiplied by factor (see above).</returns>
  <param name="NU">Defines the order of the Bessel function of the first kind Jn.</param>
  <param name="A">Defines real value for which Jn(A) should be evaluated.</param>
  <param name="Scale">If Scale parameter is true, Jn(A) is multiplied by <c>exp(-|Im(A)|)</c>.</param>*)

function Besj(NU: double; A: double; Scale: boolean): TCplx; overload;
function Besj(NU: single; A: single; Scale: boolean): TSCplx; overload;








(*<summary>Bessel function of the second kind Yn(Z) (Complex argument).</summary>
   <returns>the Bessel function of the second kind Yn(Z), with NU parameter
    describing order of Bessel function.</returns>
  <param name="Z">Defines complex value for which JYn(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind Jn.</param>*)

function Besy(NU: double; Z: TCplx): TCplx; overload;
function Besy(NU: single; Z: TSCplx): TSCplx; overload;

(*<summary>Bessel function of the second kind Yn(Z) (Real argument).</summary>
   <returns>the Bessel function of the second kind Yn(A), with NU parameter
    describing order of Bessel function.</returns>
  <param name="A">Defines real value for which JYn(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind Jn.</param>*)

function Besy(NU, A: double): TCplx; overload;
function Besy(NU, A: single): TSCplx; overload;

(*<summary>Bessel function of the second kind Yn(Z) (Complex argument, can be scaled).</summary>
   <returns>the Bessel function of the second kind Yn(Z), with NU parameter
    describing order of Bessel function.If <paramref name="Scale"/> parameter is true, Yn(Z) is multiplied by appropriate
    factor (see below).</returns>
  <param name="Z">Defines complex value for which Yn(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind Yn.</param>
  <param name="Scale">If Scale parameter is true, Yn(Z) is multiplied by <c>-exp(-|Im(Z)|)</c>.</param>

  <seealso cref="Airy"/>
  <seealso cref="Biry"/>
  <seealso cref="Besh"/>
  <seealso cref="Besk"/>
  <seealso cref="Besj"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Besy(NU: double; Z: TCplx; Scale: boolean): TCplx; overload;
function Besy(NU: single; Z: TSCplx; Scale: boolean): TSCplx; overload;

(*<summary>Bessel function of the second kind Yn(A) (Real argument, can be scaled).</summary>
   <returns>the Bessel function of the second kind Yn(A), with NU parameter
    describing order of Bessel function. If Scale parameter is true, Yn(A) is multiplied by appropriate factor (see below).</returns>
  <param name="A">Defines real value for which Yn(A) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind >n.</param>
  <param name="Scale">If Scale parameter is true, Yn(A) is multiplied by <c>-exp(-|Im(A)|)</c>.</param>*)

function Besy(NU,A: double; Scale: boolean): TCplx; overload;
function Besy(NU,A: single; Scale: boolean): TSCplx; overload;






(*<summary>Modified Bessel function of the first kind In(Z) (complex argument).</summary>
  <returns>modified Bessel function of the first kind In(Z), with NU parameter describing
    order of modified Bessel function In(Z).</returns>
  <param name="NU">Defines the order of In(Z).</param>
  <param name="Z">Defines complex value for which In(Z) should be evaluated.</param>*)

function Besi(NU: double; Z: TCplx): TCplx; overload;
function Besi(NU: single; Z: TSCplx): TSCplx; overload;
(*<summary>Modified Bessel function of the first kind In(A) (real argument).</summary>
  <returns>modified Bessel function of the first kind In(A), with NU parameter describing
    order of modified Bessel function In(A).</returns>
  <param name="NU">Defines the order of In(A).</param>
  <param name="A">Defines real value for which In(A) should be evaluated.</param>*)

function Besi(NU, A: double): TCplx; overload;
function Besi(NU, A: single): TSCplx; overload;
(*<summary>Modified Bessel function of the first kind In(Z) (Complex argument, can be scaled).</summary>
  <returns>the modified Bessel function of the first kind In(Z), with NU parameter
    describing order of Bessel function.If <paramref name="Scale"/> parameter is true, Kn(Z) is multiplied by appropriate factor (see below).</returns>
  <param name="Z">Defines complex value for which In(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the first kind In.</param>
  <param name="Scale">If Scale parameter is true, In(Z) is multiplied by <c>exp(-|Re(Z)|)</c>.</param>

  <seealso cref="Airy"/>
  <seealso cref="Biry"/>
  <seealso cref="Besh"/>
  <seealso cref="Besy"/>
  <seealso cref="Besj"/>
  <seealso cref="Besk"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Besi(NU: double; Z: TCplx; Scale: boolean): TCplx; overload;
function Besi(NU: single; Z: TSCplx; Scale: boolean): TSCplx; overload;
(*<summary>Modified Bessel function of the first kind In(A) (Real argument, can be scaled).</summary>
  <returns>the modified Bessel function of the first kind In(A), with NU parameter
    describing order of Bessel function.If Scale parameter is true, Kn(A) is multiplied by appropriate factor (see below).</returns>
  <param name="A">Defines real value for which In(A) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the first kind In.</param>
  <param name="Scale">If Scale parameter is true, In(A) is multiplied by <c>exp(-|Re(A)|)</c>.</param>*)

function Besi(NU, A: double; Scale: boolean): TCplx; overload;
function Besi(NU, A: single; Scale: boolean): TSCplx; overload;





(*<summary>Modified Bessel function of the third kind - Hankel function H(Z) (Real argument).</summary>
  <returns>modified Bessel function of the third kind - Hankel function H(A).</returns>
  <param name="NU">Defines the order of Hankel function.</param>
  <param name="A">Defines real value for which H(A) should be evaluated.</param>*)

(*<summary>Modified Bessel function of the second kind Kn(Z) (complex argument).</summary>
  <returns>modified Bessel function of the second kind Kn(Z), with NU parameter describing order
    of modified Bessel function.</returns>
  <param name="NU">Defines the order of Hankel function.</param>
  <param name="Z">Defines complex value for which Kn((Z) should be evaluated.</param>*)

function Besk(NU: double; Z: TCplx): TCplx; overload;
function Besk(NU: single; Z: TSCplx): TSCplx; overload;
(*<summary>Modified Bessel function of the second kind Kn(A) (real argument).</summary>
  <returns>modified Bessel function of the second kind Kn(A), with NU parameter describing order
    of modified Bessel function.</returns>
  <param name="NU">Defines the order of Hankel function.</param>
  <param name="A">Defines real value for which Kn(A) should be evaluated.</param>*)

function Besk(NU, A: double): TCplx; overload;
function Besk(NU, A: single): TSCplx; overload;

(*<summary>Bessel function of the second kind Kn(Z) (Complex argument, can be scaled).</summary>
  <returns>the Bessel function of the second kind Kn(Z), with NU parameter
    describing order of Bessel function.If <paramref name="Scale"/> parameter is true, Kn(Z) is multiplied by appropriate factor (see below).</returns>
  <param name="Z">Defines complex value for which Kn(Z) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind Yn.</param>
  <param name="Scale">If Scale parameter is true, Kn(Z) is multiplied by <c>exp(-|Re(Z)|)</c>.</param>

  <seealso cref="Airy"/>
  <seealso cref="Biry"/>
  <seealso cref="Besh"/>
  <seealso cref="Besy"/>
  <seealso cref="Besj"/>
  <seealso cref="Besi"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Besk(NU: double; Z: TCplx; Scale: boolean): TCplx; overload;
function Besk(NU: single; Z: TSCplx; Scale: boolean): TSCplx; overload;

(*<summary>Bessel function of the second kind Kn(A) (Real argument, can be scaled).</summary>
  <returns>the Bessel function of the second kind Kn(A), with NU parameter
    describing order of Bessel function.If Scale parameter is true, Kn(A) is multiplied by appropriate factor (see below).</returns>
  <param name="A">Defines real value for which Kn(A) should be evaluated.</param>
  <param name="NU">Defines the order of the Bessel function of the second kind Yn.</param>
  <param name="Scale">If Scale parameter is true, Kn(A) is multiplied by <c>exp(-|Re(A)|)</c>.</param>*)

function Besk(NU,A: double; Scale: boolean): TCplx; overload;
function Besk(NU,A: single; Scale: boolean): TSCplx; overload;





(*<summary>Modified Bessel function of the third kind - Hankel function H(Z) (Complex argument).</summary>
  <returns>modified Bessel function of the third kind - Hankel function H(z).</returns>
  <param name="NU">Defines the order of Hankel function.</param>
  <param name="Z">Defines complex value for which H(Z) should be evaluated.</param>*)

function Besh(NU: double; Z: TCplx): TCplx; overload;
function Besh(NU: single; Z: TSCplx): TSCplx; overload;

(*<summary>Modified Bessel function of the third kind - Hankel function H(Z) (Real argument).</summary>
  <returns>modified Bessel function of the third kind - Hankel function H(A).</returns>
  <param name="NU">Defines the order of Hankel function.</param>
  <param name="A">Defines real value for which H(A) should be evaluated.</param>*)

function Besh(NU,A: double): TCplx; overload;
function Besh(NU,A: single): TSCplx; overload;

(*<summary>Hankel function H(z) of the first or second kind (Complex argument, first or second kind, can be scaled.</summary>
  <returns>modified Bessel function of the third kind - Hankel function H(z).
    If parameter FirstKind is true, the return function will be Hankel function of the first kind. If
    false, the return function will be Hankel function of second kind. If Scale parameter is true, the result will
    be multiplied by scale factor <c>Exp(+i*z)</c> or <c>Exp(-i*z)</c>, depending on FirstKind parameter value.</returns>
  <param name="Z">Defines complex value for which H(Z) should be evaluated.</param>
  <param name="NU">describes the order of Hankel function.</param>
  <param name="FirstKind">If true, calculate Hankel function of first kind, if false, calculate Hankel function
    of second kind.</param>
  <param name="Scale">If true, the result will be multiplied by scale factor
    <c>Exp(+i*z)</c> or <c>Exp(-i*z)</c>, depending on FirstKind parameter value.</param>

  <seealso cref="Airy"/>
  <seealso cref="Biry"/>
  <seealso cref="Besk"/>
  <seealso cref="Besy"/>
  <seealso cref="Besj"/>
  <seealso cref="Besi"/>
  <seealso cref="EllipComplete"/>
  <seealso cref="EllipJacoby"/>*)

function Besh(NU: double; Z: TCplx; Scale, FirstKind: boolean): TCplx; overload;
function Besh(NU: single; Z: TSCplx; Scale, FirstKind: boolean): TSCplx; overload;

(*<summary>Hankel function H(z) of the first or second kind (Real argument, first or second kind, can be scaled.</summary>
  <returns>modified Bessel function of the third kind - Hankel function H(A).
    If parameter <paramref name="FirstKind"/> is true, the return function will be Hankel function of the first kind. If
    false, the return function will be Hankel function of second kind. If Scale parameter is true, the result will
    be multiplied by scale factor <c>Exp(+i*A)</c> or <c>Exp(-i*A)</c>, depending on FirstKind parameter value.</returns>
  <param name="A">Defines real value for which H(Z) should be evaluated.</param>
  <param name="NU">describes the order of Hankel function.</param>
  <param name="FirstKind">If true, calculate Hankel function of first kind, if false, calculate Hankel function
    of second kind.</param>
  <param name="Scale">If true, the result will be multiplied by scale factor
    <c>Exp(+i*A)</c> or <c>Exp(-i*A)</c>, depending on FirstKind parameter value.</param>*)

function Besh(NU,A: double; Scale, FirstKind: boolean): TCplx; overload;
function Besh(NU,A: single; Scale, FirstKind: boolean): TSCplx; overload;







(*<summary>Associated Legendre polynomial <c>P(l,m,x)</c>.</summary>
  <param name="X">Defines value at which the Legendre polynomial will be evaluated.
  Valid x values are real values on closed interval <c>[-1,1]</c>.</param>
  <param name="l">Defines Legenre polynomial parameter l.</param>
  <param name="m">Defines Legenre polynomial parameter m, defined for closed interval [0,l].</param>
  <returns>The associated Legendre polynomial <c>P(l,m,x)</c>.</returns>*)

function LegendreP(l, m: Integer; X: double): double; overload;
function LegendreP(l, m: Integer; X: single): single; overload;

(*<summary>The associated Legendre polynomial <c>P(l,0,x), (m = 0)</c>.</summary>
  <param name="X">Defines value at which the Legendre polynomial will be evaluated.
    Valid x values are real values on closed interval <c>[-1,1]</c>.</param>
  <param name="l">Defines Legenre polynomial parameter l.</param>
  <returns>The associated Legendre polynomial <c>P(l,0,x), (m = 0)</c>.</returns>*)

function LegendreP(l: Integer; X: double): double; overload;
function LegendreP(l: Integer; X: single): single; overload;

(*<summary>The associated Legendre polynomials for <c>l= 0.. n, P(l,0,x)</c>.</summary>
  <param name="X">Defines value at which the Legendre polynomials will be evaluated.
    Valid x values are real values on closed interval <c>[-1,1]</c>.</param>
  <param name="n">Defines upper limit for Legentre polynomials. n must be less or equal to l.</param>
  <param name="result">Returns Legendre polynomials <c>P(0,0,x), P(1,x),...P(n,0,x)</c>.</param>*)

procedure LegendreP(n: Integer; X: double; result: TDenseMtxVec); overload;
procedure LegendreP(n: Integer; X: single; result: TDenseMtxVec); overload;

(*<summary> Computes exponentional integral Ei(X) .</summary>
  
<remarks>X may not be equal 0.
</remarks>
*)

function EI(X: Double): Double; overload;

(*<summary> Computes exponentional integral exp(-X)*Ei(X) .</summary>*)

function ExpEI(X: Double): Double;  overload;

(*<summary> Computes exponentional integral -Ei(-X) .</summary>
  
<remarks>X must be greater than zero.
</remarks>
*)


function E1(X: Double): Double;  overload;





