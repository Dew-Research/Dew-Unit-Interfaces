











(*<summary>Regression routines.</summary>
  
<remarks>Unit introduces several regression routines:
  <list type="bullet">
  <item> Simple Linear Regression,  </item>
  <item> Multiple Linear Regression,</item>
  <item> Ridge Regression,</item>
  <item> Logistic Regression,</item>
  <item> Poisson regression,</item>
  <item> Principal Component Regression</item>
  <item> Analysis of Variance routines.</item>
  </list>
</remarks>
*)
unit Regress;


{$I bdsppdefs.inc}

interface

uses MtxVec, MtxVecInt, Math387, Statistics, Optimization, MtxIntDiff
  
  ,SysUtils, Classes
  
  ;

type

  (*<summary>Stepwise regression optimization algorithm. </summary>*)
  TStepwiseMethod = (
  (*<summary>Additional variables will be included in to the existing model, if the quality criteria is improved.</summary>*)
  swForward,
  (*<summary>Additional variables will be excluded from the existing model, if the quality criteria is improved.</summary>*)
  swBackward,
  (*<summary>All variable combinations will be computed and the set with best result (according to the criteria) will be selected.
    This method allows maximum 15 variables. Variable count of more than 15 can quickly result in unresonably large memory
    and CPU requirements which increases by 2x for each additional variable.</summary>*)
  swExhaustive,
  (*<summary>Perform a single regression step, show the result with statistics and allow the user to modify the list of included variables
    before the next run.</summary>*)
  swStepwise
  );

  (*<summary>Defines regression function.</summary>
    
<remarks>The type describes the regression function of several regression
    parameters (B array) and independent variable X. The TRegressFun
    is used in several regression procedure. For higher (vectorized)
    performance see TMultiRegressFun.
</remarks>


    

    <example>
      "Eckerle4" function from NIST files:
    <code>
    private double Eckerle4(TVec B, double x)
    {
      double sqrterm = (x-B[2])/B[1])*(x-B[2])/B[1]);
      return B[0]/B[1] * Math.Exp(-0.5*(sqrterm));
    }
    </code></example>

    
    <SeeAlso cref="TDeriveProc"/>*)
  TRegressFun = function(const B: TVec; X: double): double;

  (*<summary>Defines multiple regression function.</summary>
    
<remarks>The type describes the vectorized regression function of several regression
    parameters (B array) and multiple independent variables X. The TMultiRegressFun
    is used in several regression procedure. The X holds a list of independent variables
    and the result of the function is stored in to the Y parameter.  The const parameter of "y" applies to
    the pointer and not to the contents of the object. When performance is not an issue, the TRegressFun can be easier to use.
</remarks>


    

    <example>
      "Eckerle4" function from NIST files:
    <code>
    private void Eckerle4(TVec B, TVecList x, TVec y)
    {
      //B[0]/B[1] * Exp(-0.5*Sqr((X-B[2])/B[1]));

      y.Normalize(X[0], B[2], B[1]);
      y.Sqr();
      y.Scale(-0.5);
      y.Exp();
      y.Scale(B[0]/B[1]);
    }
    </code></example>

    
    <SeeAlso cref="TDeriveProc"/>*)
  TMultiRegressFun = procedure(const B: TVec; const X: TVecList; const Y: TVec);

  (*<summary>Defines derivatives of a function.</summary>
    
<remarks>The type describes the procedure for calculating the derivatives
    of a regression function TMultiRegressFun with respect to the regression
    parameters (B array), evaluated at (X,Y). The function accepts all pairs of (x,y) values at which
    the function is to be evaluted allowing for vectorized (up to 20x faster) computation. Each independent
    variable X is stored as a separate vector in the list x. The result is returned in to the
    parameter Grad. The "const" modifier of "Grad" applies to the pointer and not to the contents of object.
</remarks>


    

    <example>
      An example of regression function and it's derivatives with respect to regression parameters:
    <code>
    private void SimplexParabolaDeriv(Dew.Stats.TMultiRegresFun RegressFun, TVecList x, TVec y, TVec Pars, TVecList Grad);
    {
      Grad[0].Sqr(x[0]);    //  Grad.Values[0] = x*x;
      Grad[1].Copy(x[0]);   //  Grad.Values[1] = x;

      Grad[2].Size(x[0]);
      Grad[2].SetVal(1);    //  Grad.Values[2] = 1.0;
    }
    </code></example>

    
    <SeeAlso cref="TMultiRegressFun"/>*)


  TMultiDeriveProc = procedure(const RegressFun: TMultiRegressFun; const X: TVecList; const Y: TVec; const Pars: TVec;const Grad: TVecList);

  (*<summary>Defines derivatives of a function.</summary>
    
<remarks>The type describes the procedure for calculating the derivatives
    of a regression function TRegressFun with respect to the regression
    parameters (B array), evaluated at (X,Y).
</remarks>


    

    <example>
      An example of regression function and it's derivatives with respect
      to regression parameters:
    <code>
    private void SimplexParabolaDeriv(Dew.Stats.TRegresFun RegressFun, double x, double y,
      TVec Pars, TVec Grad);
    {
      Grad.Values[0] = x*x;
      Grad.Values[1] = x;
      Grad.Values[2] = 1.0;
    }
    </code></example>

    
    <SeeAlso cref="TRegressFun"/>*)
  TDeriveProc=procedure(RegressFun: TRegressFun; const X, Y: double; const Pars, Grad: TVec);


  (*<summary>Defines F statistics parameters for regression test.</summary>*)
  TFStats = packed record
    SSE: double;
    SSR: double;
    (*<summary> dFE = Number_Of_Observations - Number_Of_Parameters </summary>*)
    dFE: Integer;
    (*<summary> dFR = Number_Of_Parameters - 1 </summary>*)
    dFR: Integer;
    F: double;
    Signif: double;
  end;

  (*<summary>Defines one-way ANOVA statistics parameters.</summary>
    
<remarks>This structured type defines following <see cref="ANOVA1"/> routine statistical parameters:
    * SS1	Sum of squares (source of variation between groups)
    * SS2	Sum of squares (source of variation within groups)
    * SSTotal	Sum of squares (total)
    * Deg1	Degrees of freedom (source of variation between groups)
    * Deg2	Degrees of freedom (source of variation within groups)
    * DegTotal	Degrees of freedom (total)
    * MS1	Mean squares (source of variation between groups), where MS1 = SS1/Deg1
    * MS2	Mean squares (source of variation within groups), where MS2 = SS2/Deg2
    * FDist	F statistics (the ratio of MS1 and MS2)
    * FCrit	Critical F value
</remarks>


    <SeeAlso cref="ANOVA1"/>*)
  TANOVA1Result = packed record
    SS1,SS2, SSTotal   : double;
    Deg1,Deg2,DegTotal : double;
    MS1,MS2            : double;
    FDist,FCrit        : double;
  end;

  (*<summary>Defines two-way ANOVA (with or without replications) statistics parameters.</summary>
    
<remarks>This structured type describes the <see cref="ANOVA2"/>  routine statistical parameters:
    * SS1	Sum of squares (source of variation rows)
    * SS2	Sum of squares (source of variation columns)
    * SS3	Sum of squares (source of variation interactions between rows and columns). The SS3 term is
      only valid for two-way ANOVA with replications.
    * SS4	Sum of squares (source of variation errors)
    * SSTotal	Sum of squares (total)
    * Deg1 Degrees of freedom (rows)
    * Deg2	Degrees of freedom (columns)
    * Deg3	Degrees of freedom (interactions). The Deg3 term is only valid for two-way ANOVA with
      replications.
    * Deg4	Degrees of freedom (errors)
    * DegTotal	Degrees of freedom (total)
    * MS1	Mean squares (rows)
    * MS2	Mean squares (columns)
    * MS3	Mean squares (interactions). The MS3 term is only valid for two-way ANOVA with replications.
    * MS4	Mean squares (errors)
    * FDist1	F statistics (the ratio of MS)
    * FDist2	F statistics (the ratio of MS)
    * FDist3	F statistics (the ratio of MS). The MS3 term is only valid for two-way ANOVA with replications.
    * FCrit1	Critical F value
    * FCrit2	Critical F value
    * FCrit3	Critical F value. The MS3 term is only valid if yor're performing two-way ANOVA with
      replications.
</remarks>


    <SeeAlso cref="ANOVA2"/>*)
  TANOVA2Result = packed record
    SS1,SS2,SS3,SS4,SSTotal      : double;
    Deg1,Deg2,Deg3,Deg4,DegTotal : double;
    MS1,MS2,MS3,MS4              : double;
    FDist1,FDist2,FDist3,
    FCrit1,FCrit2,FCrit3         : double;
  end;

  (*<summary>Regression statistical parameters.</summary>
    
<remarks>This structured type describes the following regression statistical parameters.
    * SSE - Residual sum of squares.
    * SST - Total sum of squares <c>TSS = SSE + SSR</c>.
    * SSR - Regression sum of squares.
    * ResidualVar - Residual variance (aka MSE).
    * R2 - Coefficient of determination.
    * AdjustedR2 - Adjusted coefficient of determination.
    * StdError = sqrt(SSE/dFE) = sqrt(MSE) = sqrt(ResidualVar)
    * dFE = Number_Of_Observations - Number_Of_Parameters
    * dFT = Number_Of_Observations - 1
    * FStat - Additional F-statistics parameters.
</remarks>


    <SeeAlso cref="RegressTest"/>*)
  TRegStats = packed record
    SSE,
    SST,
    SSR,
    ResidualVar,
    R2,
    AdjustedR2: double;
    dfE,
    dfT: Integer;
    StdError: double;
    FStats: TFStats;
  end;

  (*<summary>Callback function for custom quality criteria of stepwise regression optimization algorithm. </summary>
              
<remarks>The Stat contains the details of the regression results, b contains the coefficients and t the corresponding t-Values for each item in b.
              The ParamMask.Length equals the initial total variable count. b and t can and will have length less than this.
              Which variables are included is determined from ParamMask, which has 1 at corresponding included variable index and 0
              otherewise. aOwner is a custom optional object and will be nil if not specified as a parameter to the optimization.
</remarks>
*)
  TStepwiseQualityCriteria = function(const Stat: TRegStats; const b, t: TVec; const ParamMask: TVecInt; const aOwner: TObject): double;

  (*<summary>Defines regression equation solve method.</summary>
    
<remarks>Defines the regression equation <c>A*x=b</c> solving method. Depending on A
    several different solving methods can be used.
</remarks>
*)
  TRegSolveMethod = (
    (*<summary>Use QR decomposition to solve system of equations.</summary>*)regSolveLQR,
    (*<summary>Use SVD to solve system of equations.</summary>*)regSolveSVD,
    (*<summary>Use LU decomposition to solve system of equations.</summary>*)regSolveLU
  );




(*<summary>Performs a one-way (single-factor) analysis of variance (ANOVA).</summary>
  <param name="Data"> Data matrix, each column is separate group.</param>
  <param name="Alpha">Desired significance level.</param>
  <param name="p">Returns the significance probability of null hypothesis that two means are equal.
    If p is less than desired significance alpha then the result suggests  the null hypothesis
    (two means are equal)  can be rejected.</param>

  
<remarks>Performs a one-way (single-factor) analysis of variance.
</remarks>


  

  <example>
    The following example will perform ANOVA1 on 4 variables.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix Data = new Matrix(0,0);
      double signprob;
      TANOVA1Result ar;
      Data.SetIt(4,4,false, new double[] {3.2, 2.5, 5.5, 7.0,
                          6.2, 12.0, 3.0, 4.1,
                          1.5, 5.7, 4.9, 3.5,
                          2.5, 3.1, 3.2, 3.4});
      ar = Regress.ANOVA1(Data,out signprob,0.05);
      // ar = (SS1:12.771875; SS2:82.1475; SSTotal:94.919375;
      // Deg1:3; Deg2:12; DegTotal:15; MS1:4.2572916667;
      // MS2:6.845625; FDist:0.62189963176; FCrit:3.4902948195
      // signprob = 0.614256442
    }
  }
  </code></example>*)
function ANOVA1(const Data: TMtx; out p: double; Alpha: double = 0.05): TANOVA1Result;overload;
(*<summary>Perform the one-way analysis of variance where Data vector (one variable) is indexed by a
  second grouping GroupIndex vector variable.</summary>
  <param name="Data">Data vector of single variable.</param>
  <param name="Alpha">Desired significance level.</param>
  <param name="GroupIndex">Stores group indexes.</param>
  <param name="p">Returns the significance probability of null hypothesis that two means are equal.
    If p is less than desired significance alpha then the result suggests  the null hypothesis
    (two means are equal)  can be rejected.</param>

  

  <example>
    The following example will perform ANOVA on single variable with three
    groups (0,1,2 - stored in GroupIndex vector).
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector Data = new Vector(0);
      Vector GroupIndex = new Vector(0);
      double signprob;
      TANOVA1Result ar;
      Data.SetIt(false, new double[] {1,2,3,4,5,6,7,8,9,10,11,12});
      GroupIndex.SetIt(false, new double[] {0,0,0,0,1,1,1,1,2,2,2,2});
      ar = Regress.ANOVA1(Data,GroupIndex,out signprob,0.05);
      // ANOVARes = (SS1:128; SS2:15; SSTotal:143;
      // Deg1:2; Deg2:9; DegTotal:11; MS1:64;
      // MS2:1.6666666667; FDist:38.4; FCrit:4.2564947291)
      // signprob = 3.9210149408e-05
    }
  }
  </code></example>

  <SeeAlso cref="ANOVA2"/>*)
function ANOVA1(const Data: TVec; const GroupIndex: TVecInt; out p: double; Alpha: double = 0.05): TANOVA1Result;overload;
(*<summary>Performs balanced two-way analysis of variance (ANOVA).</summary>
  <param name="Data">Stores the data to me analyzed.</param>
  <param name="Replications">Defines the number of rows/cols replications.</param>
  <param name="Alpha">Desired significance level.</param>
  <param name="pRows">Return the significance probability for the null hypothesis that rows, cols or interacted
    terms means are equal. These values have some meaning if Replications is more than 1.
    If any p is less than desired significance alpha then the result suggests the null hypothesis (rows mean, columns
    mean or interaction mean is not equal) can be rejected.</param>
  <param name="pCols">Return the significance probability for the null hypothesis that rows, cols or interacted
    terms means are equal. These values have some meaning if Replications is more than 1.
    If any p is less than desired significance alpha then the result suggests the null hypothesis (rows mean, columns
    mean or interaction mean is not equal) can be rejected.</param>
  <param name="pInt">Return the significance probability for the null hypothesis that rows, cols or interacted
    terms means are equal. These values have some meaning if Replications is more than 1.
    If any p is less than desired significance alpha then the result suggests the null hypothesis (rows mean, columns
    mean or interaction mean is not equal) can be rejected.</param>

  
<remarks>Performs balanced two-way analysis of variance on ranks of data contained in Data matrix. The two-way analysis
  of variance compares the means of two or more rows and two or more columns. The data in different columns
  can be interpreted as change of one factor and data in different rows can be interpreted as changes in other factor.
  If there is more than one observation per row/column then you can set the number of row/column replications by changing
  Replications value to appropriate factor. An exception will be raised if (Data.Rows mod  Replications) is not zero.
  Example layout for replication factor 2:

    <code>
            Group1      Group2        Group3       Group4
    Trial1  xx          xx            xx           xx
            xx          xx            xx           xx
            xx          xx            xx           xx
    Trial2  xx          xx            xx           xx
            xx          xx            xx           xx
            xx          xx            xx           xx
    </code>

    Both trials have 3 samples and data is in 4 groups.
</remarks>


  

  <example>
    This example shows the ANOVA on data with Replications set to 2
    meaning there are two rows/cols per "cell".
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        double prows, pcols, pinteract;
        Matrix Data = new Matrix(0, 0);
        Data.SetIt(4, 4, false, new double[] {2.5, 3.2, 4.2, 3.9,
           1.9, 3.5, 3.6, 3.7,
           3.4, 3.5, 3.3, 3.4,
           4.2, 5.0, 3.1, 2.4});
        TANOVA2Result ar = Regress.ANOVA2(Data, out prows, out pcols, out pinteract, 2, 0.05);
        // ar =(SS1:0.2025; SS2:1.37; SS3:4.4675; SS4:2.39; SSTotal:8.43 ;
        // Deg1: 1; Deg2:3 ; Deg3:3; Deg4:8 ; DegTotal:15 ;
        // MS1:0.2025 ; MS2:0.45666666667 ; MS3:1.4891666667 ; MS4:0.29875 ;
        // FDist1:0.060496875 ; FDist2:0.13642916667 ; FDist3:0.44488854167 ;
        // FCrit1:5.3176550716 ; FCrit2:4.0661805514 ; FCrit3:4.0661805514 ;
        // pRows: 0.81190529881
        // pCols: 0.93549639694
        // pInteract: 0.72753486979
    }
  }
  </code></example>

  <SeeAlso cref="ANOVA1"/>*)
function ANOVA2(const Data: TMtx; out pRows,pCols,pInt: double; Replications: Integer = 1; Alpha: double = 0.05): TANOVA2Result;

(*<summary>Multivariante linear regression.</summary>*)
procedure MulLinRegress(const Y: TVec; const A: TMtx; const b: TVec; Constant: boolean = true; const YCalc: TVec = nil; const ATA: TMtx = nil; Method: TRegSolveMethod = regSolveLQR); overload;
(*<summary>Multivariante linear regression.</summary>
  <param name="Y">Defines vector of dependant variable.</param>
  <param name="A">Defines matrix of independant (also X) variables.</param>
  <param name="b">Returns calculated regression coefficiens.</param>
  <param name="Method">Use QR, SVD, or LU solver. Typically QR will yield best compromise between stability and performance.</param>
  <param name="Weights">Defines weights (optional).</param>
  <param name="Constant">If true then intercept term b(0) will be included in calculations. If false, set intercept term b(0) to 0.0.</param>
  <param name="YCalc">Returns vector of calculated dependant variable, where <c>YCalc = A*b</c>.</param>
  <param name="ATA">Returns inverse matrix of normal equations i.e <c>[A(T)*A]^-1</c>.</param>

  
<remarks>Routine fits equations to data by minimizing the sum of squared residuals:

  <c>SS = Sum [y(k) - ycalc(k)]^2 ,</c><para/>
  where y(k) and ycalc(k) are respectively the observed and calculated value of the dependent variable for observation k.
  ycalc(k) is a function of the regression parameters b(0), b(1) ... Here the observed values obey the following equation:

	<c>y(k) = b(0) + b(1) * x(1,k) + b(2) * x(2,k) + ...</c><para/>
  i.e<para/>
	<c>y = A * b.</c><para/>
  To calculate additional regression statistical values, use <see cref="RegressTest"/> routine.
</remarks>


  

  <example>
    The following example performs multiple linear regression.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
        Matrix A = new Matrix(0, 0);
        Matrix ATA = new Matrix(0, 0);
        Vector y = new Vector(0);
        Vector b = new Vector(0);
        Vector w = new Vector(0);
        Vector yhat = new Vector(0);
        Vector residuals = new Vector(0);
        Vector BStdDev = new Vector(0);

        TRegStats rs;
        // independent variables
        A.SetIt(4, 2, false, new double[] {1.0, 2.0,
                  -3.2, 2.5,
                   8.0, -0.5,
                   -2.2, 1.8});
        w.SetIt(false, new double[] { 1, 2, 2, 1 }); // weights
        y.SetIt(false, new double[] { -3.0, 0.25, 8.0, 5.5 }); // dependent variables
        Regress.MulLinRegress(y, A, b, w, true, yhat, ATA, TRegSolveMethod.regSolveLQR); //do regression
                                                                   // b=(19.093757944, -2.0141843616, -10.082487055)
        Regress.RegressTest(y, yhat, ATA, out rs, residuals, BStdDev, true, w); // do basic regression stats
                                               // RegStat = (ResidualVar:0.037230395108; R2:0.99965713428;
                                               // AdjustedR2:0.99897140285; F:1457.7968725; SignifProb: 0.01851663347)
    }
  }
  </code></example>

  <SeeAlso cref="RegressTest"/>
  <SeeAlso cref="LinRegress"/>
  <SeeAlso cref="NLinRegress"/>*)
procedure MulLinRegress(const Y: TVec; const A: TMtx; const b: TVec; const Weights: TVec; Constant: boolean = true; const YCalc: TVec = nil; const ATA: TMtx = nil;Method: TRegSolveMethod = regSolveLQR );overload;
(*<summary>Logistic regression.</summary>
  <param name="y">response vector containing binomial counts.</param>
	<param name="n">number of trials for each count. Y is assumed to be binomial(p,N).</param>
	<param name="A">matrix of covariates, including the constant vector if required.</param>
  <param name="Offset">offset if required.</param>
	<param name="b">regression parameter estimates.</param>
	<param name="YCalc">fitted values.</param>
  <param name="BStd">Regression parameter estimates errors.This is an estimate of the precision
    of the B estimates.</param>
  <param name="Tolerance">Default precision for reweighted LQR.</param>

  
<remarks>Fit logistic regression model.
</remarks>


  

  <example>
    The following example calculates coefficients for simple logistic regression.
    The counts are out of 10 in each case and there is one covariate[1].
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        Vector y = new Vector(0);
        Vector n = new Vector(0);
        Vector B = new Vector(0);
        y.SetIt(false, new double[] { 2, 0, 3, 1, 5, 5, 6, 9, 5, 9 });
        n.Size(y);
        n.SetVal(10.0);

        Regress.LogisticRegress(y, n, B, null,0,null,null, Math387.EPS);
    }
  }
  </code></example>*)
procedure LogisticRegress(const y, n: TVec; const b: TVec; const A: TMtx = nil; Offset: double = 0.0; const YCalc: TVec = nil; const BStd: TVec = nil; Tolerance: double = EPS);overload;
(*<summary>Ordinal logistic regression.</summary>
  <param name="y">Response levels.</param>
  <param name="A">Matrix of independent variables. It is assumed to have full column rank.</param>
  <param name="B">Set it to define initial estimates for B. After the call to LogisticRegress
    returns regression parameter estimates for B.</param>
  <param name="Theta">Set it to define initial estimates for Theta. After the call to LogisticRegress
    returns regression parameter estimates for Theta.</param>
  <param name="FMin">Returns logistic log-likehood function, evaluated at minimum.</param>
  <param name="StopReason">Returns why the internal Marquardt optimization method stopped.</param>
  <param name="MaxIter"> Maximum number of allowed iterations in main optimisation loop.</param>
  <param name="Tolerance">Desired tolerance for optimisation minimum.</param>
  <param name="StdErr">Returns Theta and B coefficients standard error. This is an estimate of the precision
    of the Theta and B estimates. The covariance matrix is obtained by inverting the observed information matrix evaluated
    at the maximum likelihood estimates. The standard errors are the square roots of the diagonal elements of this covariance matrix.</param>
  <param name="AutoInitEstimates">If true then B and Theta initial estimates will be calculated. If false then you must
    specify initial values for B and Theta.</param>
  <returns>number of iterations needed to converge to solution with Tolerance precision.</returns>

  
<remarks>Performs logistic <b>or</b> ordinal logistic regression. Suppose y takes values in k ordered categories, and let  p_ij
  be the cumulative probability that  y(i)  falls in the j'th category
	or higher. The ordinal logistic regression model is defined as:

  <c>logit(p_ij) = theta(j) + A_i'B , i = 1,..,length(Y), j = 1,..,k-1,</c><para/>
	where  A_i  is the i'th row of  A .  The number of ordinal
	categories k is taken to be the number of distinct values of int)y. If k is 2 the model is ordinary logistic regression[1].
</remarks>


  

  <example>
    The following example performs ordinal logistic regression. There are three levels of
    response and two intercepts in the output to distinguish the three levels.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Vector theta = new Vector(0);
      Vector se = new Vector(0);

      Matrix A = new Matrix(0,0,false);
      double min;
      TOptStopReason stopreason;
      y.SetIt(false,new double[] {1,1,2,1,3,2,3,2,3,3});
      A.SetIt(false,10,1,new double[] {1,2,3,4,5,6,7,8,9,10});
      Regression.LogisticRegress(y,A,b,theta,se,out min, out stopreason,
        100,1.0e-8,true);
      // b = (0.801), theta=(2.779,5.366)
    }
  }
  </code></example>

  <SeeAlso href="[2] Code adapted from Gordon K Smyth, U of Queensland, Australia, gks@maths.uq.oz.au"/>*)
function LogisticRegress(const y: TVec; const A: TMtx; const B, Theta: TVec; const StdErr: TVec; out FMin: double; out StopReason: TOptStopReason;
  MaxIter: Integer = 100; Tolerance: double = SQRTEPS;  AutoInitEstimates: boolean = true):integer; overload;
(*<summary>Simple linear regression.</summary>
  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals:<para/>

  <c>SS = Sum [y(k) - ycalc(k)]^2 ,</c><para/>
  where y(k) and ycalc(k) are respectively the observed and calculated value of the dependent variable for observation k.
  ycalc(k) is a function of the regression parameters b(0) and b(1). In case constant term is used, the observed values
  obey the following equation:<para/>

  <c>y(k) = b(0) + b(1) * x</c><para/>
  i.e<para/>
  <c>Y = b(0) + b(1)*X.</c><para/>
  or if constant term is NOT used:<para/>

  <c>y(k) = b(0) * x</c><para/>
  To calculate additional regression statistical parameters, use <see cref="RegressTest"/> routine.
</remarks>


  

  <example>
    The following example loads data for simple linear regression (line equation) and
    then extracts line parameters <c>b(0)=n</c> and <c>b(1)=k</c>:
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  using Steema.TeeChart;
  namespace Dew.Examples
  {
    private void Example(Styles.Line series1, Styles.Line series2)
    {
      Vector x = new Vector(0);
      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Vector yhat = new Vector(0);
      x.SetIt(false,new double[] {1.0, 1.5, 2.3, 3.8, 4.2, 5.0, 5.3, 5.9});
      y.SetIt(false,new double[] {11, 12, 12.5, 14, 14.3, 15.2, 15.3, 17});
      LinRegress(x,y,b,true,null,yhat,null);
      MtxVecTee.DrawValues(x,y,series1,false); // draw original data
      MtxVecTee.DrawValues(x,yhat,series2,false); // draw fitted data
    }
  }
  </code></example>

  <SeeAlso cref="MulLinRegress"/>
  <SeeAlso cref="RegressTest"/>*)
procedure LinRegress(const X,Y: TVec; const B: TVec; Constant: Boolean = True; const Weights: TVec = nil; const YCalc: TVec = nil; const ATA: TMtx = nil);
(*<summary>Regression by using "ridge" regression method.</summary>
  
<remarks>Calculates regression coefficients b by using "ridge regression" method. When data suffers from multicollinearity one
  of the available methods which can still be used is ridge regression. In this case least squares estimates are unbiased,
  but their variances are large so they may be far from the true value. By adding a degree of bias to the regression estimates,
  ridge regression reduces the standard errors. It is hoped that  the total effect will be to give more reliable estimates.
  Ridge regression uses following model:

  <c>y = A*b ,</c><para/>
  where y is vector of observations, A matrix of independent variables and b are regression coefficients. Additional k
  parameter is the so called "ridge parameter". Regression coefficients are then calculated from the following formula:

  <c>b = inv(AT*A + k*I) * (AT*y),</c><para/>
  where I is the identity matrix, A matrix of independent variables and AT=Transp(A).

  Note
    The routine does NOT calculate optimal k value for ridge regression.
</remarks>


  

  <example>
    An example of ridge regression method.
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Matrix A = new Matrix(0,0);
      y.SetIt(false,new double[] {-2.5, 0.1, 6.1});
      A.SetIt(3,2,false, new double[] {1.0, 2.5,
                     3.2, -1.5,
                     0.4, 0.7});
      Regress.RidgeRegress(y,A,0.0,b);
      // b = (-6.8889789853, -6.450976395)
    }
  }
  </code></example>*)
procedure RidgeRegress(const Y: TVec; const A: TMtx; k: double; const b: TVec; Normalize: Boolean = true);
(*<summary>Regression tests.</summary>
  <param name="Y">Dependant variables.</param>
  <param name="YCalc">Estimated (calculated) dependant variables.</param>
  <param name="Weights">Model weights (optional).</param>
  <param name="NumPars">Number of variables (parameters) in ML model A*b=y (number of columns in A matrix or number of rows in b).</param>
  <param name="Constant">If true then include intercept term b(0) in calculations. If false, set intercept term b(0) to 0.0.</param>
  <param name="RegStat">Returns regression statistics parameters.</param>

  
<remarks>Use regression results to calculate basic regression statistics for model:

  <c>A*b=Y</c>
</remarks>


  <SeeAlso cref="R2"/>
  <SeeAlso cref="PRESS"/>*)
procedure RegressTest(const Y, YCalc: TVec; NumPars: Integer; out RegStat: TRegStats; Constant: boolean = True; const Weights: TVec = nil); overload;
(*<summary>Regression tests.</summary>
  <param name="Y">Dependant variables.</param>
  <param name="YCalc">Estimated (calculated) dependant variables.</param>
  <param name="ATA">Inverse matrix of normal equations i.e <c>[A(T)*A]^-1</c>.</param>
  <param name="Weights">Model weights (optional).</param>
  <param name="Constant">If true then include intercept term b(0) in calculations. If false, set intercept term b(0) to 0.0.</param>
  <param name="RegStat">Returns regression statistics parameters.</param>
  <param name="Residuals">Returns residual errors.</param>
  <param name="BStdDev">Returns standard deviation.</param>

  
<remarks>Using regression results the routine calculates additional regression statistical parameters, together with model
  coefficients standard errors and model errors.
</remarks>


  

  <example>
    Perform MLR and use results to calculate additional statistical parameters.
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
        Matrix A = new Matrix(0, 0);
        Matrix ATA = new Matrix(0, 0);
        Vector y = new Vector(0);
        Vector b = new Vector(0);
        Vector w = new Vector(0);
        Vector yhat = new Vector(0);
        Vector res = new Vector(0);
        Vector bse = new Vector(0);
        TRegStats rs;

        // independent variables
        A.SetIt(4, 2, false, new double[] {1.0, 2.0,
                  -3.2, 2.5,
                   8.0, -0.5,
                   -2.2, 1.8});

        w.SetIt(false, new double[] { 1, 2, 2, 1 }); // weights
        y.SetIt(false, new double[] { -3.0, 0.25, 8.0, 5.5 }); // dependent variables
        Regress.MulLinRegress(y, A, b, w, true, yhat, ATA, TRegSolveMethod.regSolveLQR); //do regression
                                                                   // b=(19.093757944, -2.0141843616, -10.082487055)
        Regress.RegressTest(y, yhat, ATA, out rs, res, bse, true, w); // do basic regression stats
                                                                   // RegStat = (ResidualVar:0.037230395108; R2:0.99965713428;
                                                                   // AdjustedR2:0.99897140285; F:1457.7968725; SignifProb: 0.01851663347)
    }
  }
  </code></example>

  <SeeAlso cref="R2"/>
  <SeeAlso cref="PRESS"/>*)
procedure RegressTest(const Y, YCalc: TVec; const ATA: TMtx; out RegStat: TRegStats; const Residuals: TVec; const BStdDev: TVec; Constant: boolean = True; const Weights: TVec = nil); overload;
(*<summary>General non-linear regression.</summary>
  <param name="X">Vector of the independent variable. </param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="RegFun">Regression function.</param>
  <param name="DeriveProc">Procedure to calculate the derivatives of RegFun. You can define the exact derivative or use <see cref="NumericDerive"/> routine as
    numerical approximation.</param>
  <param name="B">Holds initial estimate for regression parameters. After the call to NLinRegress b returns calculated regression parameters.</param>
  <param name="Method">Defines which optimization method will be used to find regression parameters (see MtxVec.hlp TOptMethod type to learn more about this).</param>
  <param name="StopReason">Returns why regression parameters search stopped (see MtxVec.hlp TOptStopReason type to learn more about different stop reasons).</param>
  <param name="Weights">Weights (optional).</param>
  <param name="YCalc">Returns calculated values (optional).</param>
  <param name="SoftSearch">If true, internal line search algoritm will use soft line search method. Set this parameter to true if you're using numerical
    approximation for derivative. If this parameter is set to false, internal line search algorithm will use exact line search method. Set this parameter
    to false if you're using *exact* derivative.</param>
  <param name="MaxIter">Maximum allowed numer of allowed iterations.</param>
  <param name="Tol">Desired regression parameters tolerance.</param>
  <param name="GradTol">Minimum allowed gradient <c>C-Norm</c>.</param>
  <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
  <returns>Number of iterations needed to calculate regression parameters with specified tolerance.</returns>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals :

  <code>SS = Sum [y(k) - ycalc(k)]^2 ,</code><para/>
  where <c>y(k)</c> and <c>ycalc(k)</c> are respectively the observed and calculated value of the dependent variable for observation k. ycalc(k) is a function of the regression
  parameters <c>b(0), b(1) ...</c> Here the observed values obey the following (non-linear) equation:

  <code>y(k) = RegFun[x(k), b(0), b(1), ... ]</code><para/>
  <code>Y = RegFun[X,b(0),b(1), ...]</code><para/>
  where RegFun is the regression function and <c>b(0),..b(i)</c> are the regression parameters.
</remarks>


  

  <example>
    The following example uses data from NIST study involving circular interference transmittance.
    The response variable is transmittance, and the predictor variable is wavelength. First we setup the
    regression function <i>Eckerle4</i> with three regression parameters (b0,b1,b2). Then we setup data
    and specify initial estimate for regression parameters (see below):
  <code>
  using Dew.Math;
  using Dew.Math.Tee;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {

    // function definition
    private double Eckerle4(TVec B, double x)
    {
      double sqrterm = (x-B[2])/B[1])*(x-B[2])/B[1]);
      return B[0]/B[1] * Exp(-0.5*(sqrterm));
    }

    private void Example()
    {
      Vector x = new Vector(0);
      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Vector yhat = new Vector(0);
      TOptStopReason StopReason;

      x.SetIt(false,new double[] {400.0, 405.0, 410.0, 415.0,
                   420.0, 425.0, 430.0, 435.0,
                   436.5, 438.0, 439.5, 441.0,
                   442.5, 444.0, 445.5, 447.0,
                   448.5, 450.0, 451.5, 453.0,
                   454.5, 456.0, 457.5, 459.0,
                   460.5, 462.0, 463.5, 465.0,
                   470.0, 475.0, 480.0, 485.0,
                   490.0, 495.0, 500.0});
      y.SetIt(false,new double[] {0.0001575, 0.0001699, 0.0002350, 0.0003102,
                   0.0004917, 0.0008710, 0.0017418, 0.0046400,
                   0.0065895, 0.0097302, 0.0149002, 0.0237310,
                   0.0401683, 0.0712559, 0.1264458, 0.2073413,
                   0.2902366, 0.3445623, 0.3698049, 0.3668534,
                   0.3106727, 0.2078154, 0.1164354, 0.0616764,
                   0.0337200, 0.0194023, 0.0117831, 0.0074357,
                   0.0022732, 0.0008800, 0.0004579, 0.0002345,
                   0.0001586, 0.0001143, 0.0000710});
      b.SetIt(false,new double[] {1.0, 10.0, 500.0}); // initial estimates
      Regress.NLinRegress(x,y,Eckerle4,null,b,optMarquardt, out StopReason,
                null,yhat,false,300,1e-8,1e-10);
      MtxVecTee.DrawValues(x,y,Series1,false); // draw data
      MtxVecTee.DrawValues(x,yhat,Series2,false); // draw fitted value
    }
  }
  </code></example>

  <SeeAlso cref="NumericDerive"/>*)
function NLinRegress(const X, Y: TVec; RegFun: TRegressFun; DeriveProc: TDeriveProc; const B: TVec; Method: TOptMethod;
                     out StopReason: TOptStopReason; const Weights: TVec = nil; const YCalc: TVec = nil; SoftSearch: boolean = false ; MaxIter : Integer = 500;
                     Tol : double = 0.00000001; 
                     GradTol : double = 0.00000001 ; const Verbose: TStrings = nil): Integer; overload;
(*<summary>Non-linear regression with lower and upper bounds.</summary>
  <param name="X">Vector of the independent variable.</param>
  <param name="Y">Vector of the dependent variable.</param>
  <param name="RegFun">Regression function.</param>
  <param name="DeriveProc">Procedure to calculate the derivatives of RegFun. You can define the exact derivative or use <see cref="NumericDerive"/> routine as
    numerical approximation.</param>
  <param name="B">Holds initial estimate for regression parameters. After the call to NLinRegress b returns calculated regression parameters.</param>
  <param name="BLowerB">Holds lower bounds for regression parameters. If there are no lower bounds, set BLowerB values to -INF.</param>
  <param name="BUpperB">Holds upper bounds for regression parameters. If there are no upper bounds, set BUpperB values to +INF.</param>
  <param name="Method">Defines which optimization method will be used to find regression parameters (see MtxVec.hlp TOptMethod type to learn more about this).</param>
  <param name="StopReason">Returns why regression parameters search stopped (see MtxVec.hlp TOptStopReason type to learn more about different stop reasons).</param>
  <param name="Weights">Weights (optional).</param>
  <param name="YCalc">Returns calculated values (optional).</param>
  <param name="SoftSearch">If true, internal line search algoritm will use soft line search method. Set this parameter to true if you're using numerical
    approximation for derivative. If this parameter is set to false, internal line search algorithm will use exact line search method. Set this parameter
    to false if you're using *exact* derivative.</param>
  <param name="MaxIter">Maximum allowed numer of allowed iterations.</param>
  <param name="Tol">Desired regression parameters tolerance.</param>
  <param name="GradTol">Minimum allowed gradient C-Norm.</param>
  <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
  <returns>Number of iterations needed to calculate regression parameters with specified tolerance.</returns>

  
<remarks>General non-linear regression with lower and upper bounds for regression coefficients.
</remarks>
*)
function NLinRegress(const X, Y: TVec; RegFun: TRegressFun; DeriveProc: TDeriveProc; const B, BLowerB, BUpperB: TVec; Method: TOptMethod;
                     out StopReason: TOptStopReason; const Weights: TVec = nil; const YCalc: TVec = nil; SoftSearch: boolean = false ; MaxIter : Integer = 500;
                     Tol : double = 0.00000001; 
                     GradTol : double = 0.00000001 ; const Verbose: TStrings = nil): Integer; overload;

(*<summary>Numerical derivative for regress function.</summary>
  
<remarks>Routine calculates the derivatives of the regression function RegressFun at (X,Y) by numerical differentiation. The results of differentation
  are returned in Grad vector. The Length and Complex properties of Grad vector are <u>NOT</u> adjusted automatically.
</remarks>


  <SeeAlso cref="NLinRegress"/>*)
procedure NumericDerive(RegressFun: TRegressFun; const X,Y : double; const Pars: TVec; const Grad: TVec);

(*<summary>Vectorized computation of the numerical derivative for the regress function.</summary>
  
<remarks>Routine calculates the derivatives of the regression function RegressFun at (X,Y) by numerical differentiation.
  The results of differentation are returned in Grad vectors, one vector for each parameter stored in "pars".
  The "X" stores the independent variables and "Y" the dependent variable. RegressFun is the function, which
  computes the value of the function of which we are looking for derivates.

  The Length and Complex properties of Grad vector are <u>NOT</u> adjusted automatically.
  Grad.Count should typically be equal to Pars.Length and Grad[i].Length equal to Y.Length. The function returns derivates separately
  for each parameter and pair of (X0...Xi, Y).

  Vectorized computation of the derivates can be 10-20x faster than non-vectorized computation.
</remarks>


  <SeeAlso cref="NLinRegress"/>*)
procedure MultiNumericDerive(const RegressFun: TMultiRegressFun; const X: TVecList; const Y : TVec; const Pars: TVec; const Grad: TVecList);

(*<summary>Poisson generalized linear model with log-link.</summary>
  <param name="y">Defines Response vector (frequency count).</param>
  <param name="A">Defines covariates, including the constant vector (if needed).</param>
  <param name="Beta">returns regression parameter estimates.</param>
  <param name="BStdErr">returns regression parameter estimates standard errors.</param>
  <param name="Deviance">returns residual deviance.</param>
  <param name="DF">returns residual degrees of freedom,</param>
  <param name="Offset">Offset (optional).</param>
  <param name="YCalc">Returns calculated values (optional).</param>
  <param name="Tol">Desired precision in calculations.</param>
  <param name="MaxIter">Maximum number of iteratively reweighted iterations in algorithm.</param>
  <returns>number of evaluations needed for desired precision.</returns>

  
<remarks>Performs Poisson generalized linear model with log-link. Poisson regression is often used to analyze count data. It can be used to model the number of occurrences of
  an event of interest or the rate of occurrence of an event of interest, as a function of some independent variables.
  In Poisson regression it is assumed that the dependent variable Y,  number of  occurrences of an event, has a Poisson distribution given the independent variables
  <c>X1, X2, ...., Xn,</c>

  <c>P(Y=k| x1, x2, ..., xn) = Exp[-m] m^k / k!,            k=0, 1, 2, ......,</c><para/>
  where the log of the mean m is assumed to be a linear function of the independent variables. That is,

  <c>log(m) = intercept + b1*X1 +b2*X2 + ....+ b3*Xn,</c><para/>
  which implies that m is  the exponential function of independent variables,

  <c>m = exp(intercept + b1*X1 +b2*X2 + ....+ b3*Xn).</c><para/>
  In many situations the rate or incidence of  an event needs to be modeled instead of the number of occurrences.
  For example, suppose that we know the number of occurrences of certain disease by county and we want to find out
  if frequency of occurrence depends on certain demographic variables and health policy programs also recorded by
  county. Since more at risk subjects result in more occurrences of the disease, we need to adjust for the number
  of subjects at risk in each county. For such data, we can write a Poisson regression model  in the following form:

  <c>log(m) = log(N) + intercept + b1*X1 +b2*X2 + ....+ b3*Xn,</c><para/>
  where N is the total number of subjects at risk by county. The logarithm of variable N is used as an offset, that is,
  a regression variable with a constant coefficient of 1 for each observation. The log of  the incidence, log (m / N),
  is modeled now as a linear function of  independent variables.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Vector berr = new Vector(0);
      Matrix A = new Matrix(0,0);
      y.SetIt(false,new double[] {1,2,3,4});
      A.SetIt(4,2,false, new double[] {1,5,2,6,3,7,4,8});
      Regression.PoissonRegress(y,A,b,berr,out dev,out df,0.0,null,1.0e-8,300);
    }
  }
  </code></example>*)
function PoissonRegress(const y: TVec; const A: TMtx; const Beta, BStdErr: TVec; out Deviance: double; out DF: Integer;
        Offset: double = 0.0; const YCalc: TVec = nil;
        Tol : double = 0.00000001 ;
        MaxIter: Integer = 500): Integer;

(*<summary>Principal Component Regression.</summary>
  <param name="Y">Defines vector of dependant variable.</param>
  <param name="A">Defines matrix of independant variables.</param>
  <param name="NumOmmit">Defines the number of variables to ommit from initial model.</param>
  <param name="b">Returns calculated regression coefficiens.</param>
  <param name="YCalc">Returns vector of calculated dependant variable, where YCalc = A*b + constant term.</param>
  <param name="Bse">Returns principal component b coefficient standard error.</param>

  
<remarks>Performs unweighted Principal Component Regression (PCR). PCR is a technique for analyzing multiple regression data
  that suffer from multicollinearity. When multicollinearity occurs, least squares estimates are unbiased, but their
  variances are large so they may be far from the true value. By adding a degree of bias to the regression estimates,
  principal components regression reduces the standard errors.
  The algorithm first standardizes A matrix and performs PC regression on standardized matrix.
</remarks>


  

  <example>
  <code>
  using Dew.Math;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix A = new Matrix(0,0);
      Matrix ATA = new Matrix(0,0);
      Vector y = new Vector(0);
      Vector ycalc = new Vector(0);
      Vector b = new Vector(0);
      Vector error = new Vector(0);
      double mse;
      // Load data
      A.SetIt(18,3,false, new double[] {1,	2,	1,
        2,	4,	2,
        3,	6,	4,
        4,	7,	3,
        5, 7,	2,
        6,	7,	1,
        7,	8,	1,
        8,	10,	2,
        9,	12,	4,
        10,	13,	3,
        11,	13,	2,
        12,	13,	1,
        13,	14,	1,
        14,	16,	2,
        15,	18,	4,
        16,	19,	3,
        17,	19,	2,
        18,	19,	1});
      Y.SetIt(false, new double[] {3,9,11,15,13,13,17,21,25,27,25,27,29,33,35,37,37,39});
      // Perform Principal Component Regression
      Regress.PCRegress(y,A,b,ycalc,null,1);
      // Errors
      error.Sub(ycalc,y);
    }
  }
  </code></example>

  <SeeAlso cref="RidgeRegress"/>*)
procedure PCRegress(const Y: TVec; const A: TMtx; const b: TVec; const YCalc: TVec = nil; const Bse: TVec = nil ; NumOmmit: Integer = 1); overload;
(*<summary>Weighted PC regression.</summary>
  <param name="Y">Defines vector of dependant variable.</param>
  <param name="A">Defines matrix of independant variables.</param>
  <param name="b">Returns calculated regression coefficiens.</param>
  <param name="Weights">Defines weights for PC regression.</param>
  <param name="YCalc">Returns vector of calculated dependant variable, where YCalc = A*b + constant term.</param>
  <param name="Bse">Returns principal component b coefficient standard error.</param>
  <param name="NumOmmit">Defines the number of variables to ommit from initial model.</param>*)
procedure PCRegress(const Y: TVec; const A: TMtx; const b: TVec; const Weights: TVec; const YCalc: TVec = nil; const Bse: TVec = nil ; NumOmmit: Integer = 1); overload;

(*<summary>R2 value.</summary>
  <returns>The R2 value, in this case defined by the following equation:<para/>
    <c>R2 = 1.0 - SumOfSquares(Y-YCalc)/SumOfSquares(Y-Mean(Y))</c><para/>.
  </returns>
  <param name="Y">Defines vector of dependant variable.</param>
  <param name="YCalc">Defines vector of estimated dependant variable values.</param>
  <remarks>
  There are several ways to calculate R2, all equivalent for a linear model where model
  includes a constant term, but not equivalent otherwise.
  </remarks>

  <SeeAlso cref="RegressTest"/>*)
function R2(const Y, YCalc: TVec): double;

(*<summary>Prediction error sum of squares (PRESS).</summary>
  <param name="Y">Defines vector of dependant variable.</param>
  <param name="A">Defines matrix of independant variables.</param>
  <returns>calculated PRESS value.</returns>

  
<remarks>PRESS is an acronym for prediction sum of squares. It was developed for use in variable selection to validate a regression model.
  To calculate PRESS, each observation is individually omitted. The remaining N - 1 observations are used to calculate a regression
  and estimate the value of the omitted observation. This is done N times, once for each observation. The difference between the
  actual Y value and the predicted Y with the observation deleted is called the prediction error or PRESS residual. The sum of
  the squared prediction errors is the PRESS value. The smaller PRESS is, the better the predictability of the model.
</remarks>


  <SeeAlso cref="RegressTest"/>*)
function PRESS(const A: TMtx; const Y: TVec): double;

(*<summary>Optimal k value for ridge regression.</summary>
  
<remarks>Finds optimal k value for ridge regression, such that k minimizes ridge
  model MSE.
</remarks>


  <SeeAlso cref="RidgeRegress"/>*)
procedure RidgeOptimalk(const y: TVec; const A: TMtx; out k: double);

(*<summary>Stepwise regression is an optimization aglorithm aiming to improve the quality of the multiple linear regression
          by excluding noisy variables. </summary>

          <param name="aList">Contains all independent variables and the dependent variable as the last item in the list.</param>
          <param name="stdDevA">Holds standard deviation of all aList items on input.</param>
          <param name="sMethod">Specifies the stepwise regression method.</param>
          <param name="VariableMask">Length must be equal to number of independent variables (aList.Count-1).
                                        This vector needs to be allocated in "bit" mode:
                                        <c>
                                        VariableMask.BitCount := NumberOfIndependentVars;
                                        </c>
                                        </param>
            <param name="reportSSE">Matrix size of IterCount x (VarCount + 7). Each row starts with Step number followed by selection list of variables in columns followed by Standard Error (quality criteria).
                                     We strive to reduce standard error and the model with the smallest standard error is considered best. Additional columns are as follows:
                                     <list type="bullet">
                                     <item> [0] Standard error = sqrt(SSE/dFE), or custom quality criteria </item>
                                     <item> SSE = Residual sum of squares. </item>
                                     <item> [2] SSR = Regression sum of squares. </item>
                                     <item> [3] SST = Total sum of squares = SSE + SSR </item>
                                     <item> [4] R2 = Coefficient of determination </item>
                                     <item> [5] Adjusted R2 = Adjusted coefficient of determination </item>
                                     <item> [6] MSE = Residual variance </item>
                                     </list>
                                     </param>
            <param name="reportCoeff">Matrix size of (IterCount*VarCount) x 5. Each iteration adds the independent variable count rows.
                                     The columns are as follows:
                                     <list type="bullet">
                                     <item> [0] Iteration Step </item>
                                     <item> [1] Variable index </item>
                                     <item> [2] variable selection where 0 means excluded and 1 means included. </item>
                                     <item> [3] holds the normalized coeffients and Fourth column the </item>
                                     <item> [4] corresponding t-values for each coefficient </item>
                                     <item> [5] two tailed p-values. Bigger p-values suggest the probability that the model would better, if the variable would be excluded. </item>
                                     </list>
                                     </param>
            <param name="MaxIter">Limits the maximum number of iterations. The function will raise an exception if this limit is reached.</param>

            <param name="InitMask">If True, the VariableMask will be initialized to all vars excluded for Forward search and all vars included for Backward search.
                                    If False, the search can start with preselected variables within VariableMask.
                                    <code>
                                    VariableMask.BitCount := NumberOfIndependentVars;<para/>
                                    VariableMask.Bits[0] := false;<para/>
                                    VariableMask.Bits[1] := true;<para/>
                                    ...
                                    </code>
                                    For the step by step method this parameter must be false (user initialization on each step is required).
                                    </param>
            <param name="CriteriaFun">Optional extra callback function to use quality criteria other than the default "Standard Error"</param>
            <param name="CriteriaOwner">An optional object parameter to be passed to the CriteriaFun</param>
          <remarks>
            Optimal result is possible only when using the "exhaustive" search method, which will check all posibilities.
            After the final variable selection has been obtained, run the <see cref="MulLinRegress"/> followed by <see cref="RegressTest"/>, if detailed statistics data is required.

            There are many methods to solve this problem. This function implements four approches: exhaustive, forward, backward and stepwise.
            For models with less than 15 variables, the exhaustive search is the recommended method. Alternatively it is possible to perform "backward search"
            by starting with all and removing one by one variable or "forward search" by starting with none and adding one by one variable.
            Both backward and forward search can have selected variables already pre-included (or pre-excluded).
            Single step mode allows the user to manually include or exclude individual variables from the model
            after each step.

            To use quality criteria other than default "Standard Error", you can pass extra callback with the CriteriaFun. The return value
            will be used to determine, if the result is better or worse and a smaller value will be considered better.
          </remarks>


          <Example>
          We are looking for best fit of:
          <para/>
          b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 = y
          <para/>
          where b1..b5 can be either zero or not, but would like to know which are best set to zero.
          Last column of aSrc in the code example below is the dependent variable (y).

          <code>

          Uses MtxExpr, Regress, StatTools, Math387;
          procedure TForm78.RunButtonClick(Sender: TObject);
          var aSrc, reportCoeff, reportSSE: Matrix;
              stdDevA: Vector;
              aList: TVecList;
              i: integer;
              bi: VectorInt;
              sMethod: TStepwiseMethod;
          begin
              Memo.Lines.BeginUpdate;
              Memo.Lines.Clear;

              aList := TVecList.Create;
              try
                  aSrc.SetIt(15,6,false, [83,34, 65, 63, 64, 106,
                                          73, 19, 73, 48, 82, 92,
                                          54, 81, 82, 65, 73, 102,
                                          96, 72, 91, 88, 94, 121,
                                          84, 53, 72, 68, 82, 102,
                                          86, 72, 63, 79, 57, 105,
                                          76, 62, 64, 69, 64, 97,
                                          54, 49, 43, 52, 84, 92,
                                          37, 43, 92, 39, 72, 94,
                                          42, 54, 96, 48, 83, 112,
                                          71, 63, 52, 69, 42, 130,
                                          63, 74, 74, 71, 91, 115,
                                          69, 81, 82, 75, 54, 98,
                                          81, 89, 64, 85, 62, 96,
                                          50, 75, 72, 64, 45, 103]);

                  aList.DecomposeColumnMatrix(aSrc);
                  stdDevA.Size(aList.Count);
                  for i := 0 to aList.Count-1 do stdDevA[i] := aList[i].StdDev;
                  bi.BitCount := aList.Count-1;
                  sMethod := swBackward;

                  StepwiseRegression(aList, stdDevA, sMethod, bi, reportSSE, reportCoeff);

                  Memo.Lines.Add('');
                  reportSSE.ValuesToStrings(Memo.Lines, '', ftaRightAlign, '0.###', '0.###', true);
                  Memo.Lines.Add('');
                  reportCoeff.ValuesToStrings(Memo.Lines, '', ftaRightAlign, '0.###', '0.###', true);
                  Memo.Lines.Add('');
              finally
                  Memo.Lines.EndUpdate;
                  aList.Free;
              end;
          end;

          </code>
          </Example>*)

procedure StepwiseRegression(const aList: TVecList; const stdDevA: TVec; const sMethod: TStepwiseMethod; const VariableMask: TVecInt;
                             const reportSSE, reportCoeff: TMtx; const MaxIter: integer = 1000; const InitMask: boolean = true;
                             const CriteriaFun: TStepwiseQualityCriteria = nil; const CriteriaOwner: TObject = nil);


(*<summary>General vectorized non-linear regression.</summary>
  <param name="X">List of vectors of independent variable(s).</param>
  <param name="Y">Vector of the dependent variable.</param>
  <param name="RegFun">Regression function.</param>
  <param name="DeriveProc">Procedure to calculate the derivatives of RegFun. You can define the exact derivative or use <see cref="MultiNumericDerive"/> routine as
    numerical approximation.</param>
  <param name="B">Holds initial estimate for regression parameters. After the call to NLinRegress b returns calculated regression parameters.</param>
  <param name="Method">Defines which optimization method will be used to find regression parameters (see MtxVec.hlp TOptMethod type to learn more about this).</param>
  <param name="StopReason">Returns why regression parameters search stopped (see MtxVec.hlp TOptStopReason type to learn more about different stop reasons).</param>
  <param name="Weights">Weights of X values (optional).</param>
  <param name="YCalc">Returns calculated values (optional).</param>
  <param name="SoftSearch">If true, internal line search algoritm will use soft line search method. Set this parameter to true if you're using numerical
    approximation for derivative. If this parameter is set to false, internal line search algorithm will use exact line search method. Set this parameter
    to false if you're using *exact* derivative.</param>
  <param name="MaxIter">Maximum allowed numer of allowed iterations.</param>
  <param name="Tol">Desired regression parameters tolerance.</param>
  <param name="GradTol">Minimum allowed gradient <c>C-Norm</c>.</param>
  <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
  <returns>Number of iterations needed to calculate regression parameters with specified tolerance.</returns>

  
<remarks>The routine fits equations to data by minimizing the sum of squared residuals :

  <code>SS = Sum [y(k) - ycalc(k)]^2 ,</code><para/>
  where <c>y(k)</c> and <c>ycalc(k)</c> are respectively the observed and calculated value of the dependent variable for observation k. ycalc(k) is a function of the regression
  parameters <c>b(0), b(1) ...</c> Here the observed values obey the following (non-linear) equation:

  <code>y(k) = RegFun[x(k), b(0), b(1), ... ]</code><para/>
  <code>Y = RegFun[X,b(0),b(1), ...]</code><para/>
  where RegFun is the regression function and <c>b(0),..b(i)</c> are the regression parameters.
</remarks>


  

  <example>
    The following example uses data from NIST study involving circular interference transmittance.
    The response variable is transmittance, and the predictor variable is wavelength. First we setup the
    regression function <i>Eckerle4</i> with three regression parameters (b0,b1,b2). Then we setup data
    and specify initial estimate for regression parameters (see below):
  <code>
  using Dew.Math;
  using Dew.Math.Tee;
  using Dew.Stats.Units;
  using Dew.Stats;
  namespace Dew.Examples
  {

    // function definition
    private void Eckerle4(TVec B, TVecList x, TVec y)
    {
       // double a = (x-B[2])/B[1];
       // result = B[0]/B[1] * Math.Exp(-0.5*a*a);

        y.Normalize(x[0], B[2], B[1]);
        y.Sqr();
        y.Scale(-0.5);
        y.Exp();
        y.Scale(B[0]/B[1]);
    }

    private void Example()
    {
      TVecList x = new TVecList();
      x.Add();

      Vector y = new Vector(0);
      Vector b = new Vector(0);
      Vector yhat = new Vector(0);
      TOptStopReason StopReason;

      x[0].SetIt(false,new double[] {400.0, 405.0, 410.0, 415.0,
                   420.0, 425.0, 430.0, 435.0,
                   436.5, 438.0, 439.5, 441.0,
                   442.5, 444.0, 445.5, 447.0,
                   448.5, 450.0, 451.5, 453.0,
                   454.5, 456.0, 457.5, 459.0,
                   460.5, 462.0, 463.5, 465.0,
                   470.0, 475.0, 480.0, 485.0,
                   490.0, 495.0, 500.0});

      y.SetIt(false,new double[] {0.0001575, 0.0001699, 0.0002350, 0.0003102,
                   0.0004917, 0.0008710, 0.0017418, 0.0046400,
                   0.0065895, 0.0097302, 0.0149002, 0.0237310,
                   0.0401683, 0.0712559, 0.1264458, 0.2073413,
                   0.2902366, 0.3445623, 0.3698049, 0.3668534,
                   0.3106727, 0.2078154, 0.1164354, 0.0616764,
                   0.0337200, 0.0194023, 0.0117831, 0.0074357,
                   0.0022732, 0.0008800, 0.0004579, 0.0002345,
                   0.0001586, 0.0001143, 0.0000710});

      b.SetIt(false,new double[] {1.0, 10.0, 500.0}); // initial estimates
      Regress.NLinRegress(x, y, Eckerle4, null, b, TOptMethod.optMarquardt, out StopReason,  null, yhat, false, 300, 1e-8, 1e-10, null);

      MtxVecTee.DrawValues(x[0],y,Series1,false); // draw data
      MtxVecTee.DrawValues(x[0],yhat,Series2,false); // draw fitted value
    }
  }
  </code></example>

  <SeeAlso cref="NumericDerive"/>*)
function NLinRegress(const X: TVecList; const Y: TVec; RegFun: TMultiRegressFun; DeriveProc: TMultiDeriveProc; const B: TVec; Method: TOptMethod;
                     out StopReason: TOptStopReason; const Weights: TVec = nil; const YCalc: TVec = nil; SoftSearch: boolean = false ; MaxIter : Integer = 500;
                     Tol : double = 0.00000001; 
                     GradTol: double = 0.00000001 ; const Verbose: TStrings = nil): Integer; overload;

(*<summary>Non-linear regression with lower and upper bounds.</summary>
  <param name="X">List of vectors of independent variable(s).</param>
  <param name="Y">Vector of dependent variable.</param>
  <param name="RegFun">Regression function.</param>
  <param name="DeriveProc">Procedure to calculate the derivatives of RegFun. You can define the exact derivative or use <see cref="NumericDerive"/> routine as
    numerical approximation.</param>
  <param name="B">Holds initial estimate for regression parameters. After the call to NLinRegress b returns calculated regression parameters.</param>
  <param name="BLowerB">Holds lower bounds for regression parameters. If there are no lower bounds, set BLowerB values to -INF.</param>
  <param name="BUpperB">Holds upper bounds for regression parameters. If there are no upper bounds, set BUpperB values to +INF.</param>
  <param name="Method">Defines which optimization method will be used to find regression parameters (see MtxVec.hlp TOptMethod type to learn more about this).</param>
  <param name="StopReason">Returns why regression parameters search stopped (see MtxVec.hlp TOptStopReason type to learn more about different stop reasons).</param>
  <param name="Weights">Weights (optional).</param>
  <param name="YCalc">Returns calculated values (optional).</param>
  <param name="SoftSearch">If true, internal line search algoritm will use soft line search method. Set this parameter to true if you're using numerical
    approximation for derivative. If this parameter is set to false, internal line search algorithm will use exact line search method. Set this parameter
    to false if you're using *exact* derivative.</param>
  <param name="MaxIter">Maximum allowed numer of allowed iterations.</param>
  <param name="Tol">Desired regression parameters tolerance.</param>
  <param name="GradTol">Minimum allowed gradient C-Norm.</param>
  <param name="Verbose">If assigned, stores Fun, evaluated at each iteration step.
    Optionally, you can also pass <see cref="TOptControl"/> object to the Verbose parameter. This allows the optimization procedure
    to be interrupted from another thread and optionally also allows logging and iteration count monitoring. </param>
  <returns>Number of iterations needed to calculate regression parameters with specified tolerance.</returns>

  
<remarks>General non-linear regression with lower and upper bounds for regression coefficients.
</remarks>
*)

function NLinRegress(const X: TVecList; const Y: TVec; RegFun: TMultiRegressFun; DeriveProc: TMultiDeriveProc; const B, BLowerB, BUpperB: TVec; Method: TOptMethod;
                     out StopReason: TOptStopReason; const Weights: TVec = nil; const YCalc: TVec = nil; SoftSearch: boolean = false ; MaxIter : Integer = 500;
                     Tol : double = 0.00000001; 
                     GradTol : double = 0.00000001 ; const Verbose: TStrings = nil): Integer; overload;





