











(*<summary>Interfaces several Stats Master routines and introduces new components.</summary>*)
unit StatTools;


{$I bdsppdefs.inc}

interface

Uses Math387, MtxVec, MtxVecInt, Optimization, Statistics, Regress, MtxBaseComp

     

     

     
       ,Classes
       ,SysUtils
       
       ,Grids
       ,Types
       
     
     ;



type

  (*<summary>Calculates statistics from multiple linear regression parameters.</summary>
    
<remarks>Calculates statistics from multiple linear regression parameters. This class is used
    by the <see cref="TMtxMulLinReg"/> component to calculate regression statistics parameters.
</remarks>


    

    <example>
      The following example was taken from TMtxMulLinReg.Recalc method:
    <code>
    using Dew.Math;
    using Dew.Stats;
    using Dew.Stats.Units;

    namespace Dew.Examples
    {
      private void Example()
      {
        Matrix A = new Matrix(0,0);
        Matrix V = new Matrix(0,0);
        TRegResultClass regres = new TRegResultClass();
        TRegStatsClass regstat = new TRegStatsClass();
        Regress.MulLinRegress(Y,A,regres.B,true,regres.YCalc,V);
        regres.CalculateRegResults(regres.Y,V,regstatfalse,null,0.03);
      }
    }
    </code></example>

    <SeeAlso cref="TRegResultClass"/>*)
  
  
  
  TRegStatsClass = class
  private
    FResidualVar: double;
    FSignifProb: double;
    FAdjustedR2: double;
    FR2: double;
    FF: double;
    FTSS: double;
    FSSE: double;
    FRSS: double;
  public
    (*<summary>Calculates statistics parameters from multiple linear regression.</summary>*)
    procedure CalculateRegStats(Y,YCalc: TVec; V: TMtx; Constant: Boolean = true; Weights: TVec = nil);
    (*<summary>Returns the residual variance.</summary>*)
    property ResidualVar: double read FResidualVar;
    (*<summary>Returns the coefficient of determination (R squared).</summary>
      
<remarks>Returns the coefficient of determination (R squared).
      R2 is a measure of the amount of reduction in the variability of Y obtained by using the regressor variables X.

      If R lies in the [0,1] interval the regression model adequately describes relation
      between Y and X. However, a large value of R2 does not necessariliy imply that regression model
      is a good one. Adding variable to the model will always increase R2, regardless of whether or
      not the additional variable is statistically significant. Thus, models that have large values
      or R2 may yield poor predictions of new observations or estimates of the mean response.
</remarks>
*)
    property R2: double read FR2;
    (*<summary>Returns the adjusted coefficient of determination.</summary>
      
<remarks>Returns the adjusted coefficient of determination. The adjustment seeks to remove
      the distortion due to a small sample size.
</remarks>
*)
    property AdjustedR2: double read FAdjustedR2;
    (*<summary>Returns the variance ratio (explained/residual ratio).</summary>
      
<remarks>Returns the variance ratio (explained/residual ratio). This is the F (Fisher) statistic for testing
      the null hypothesis that all b[i] regression coefficients = 0.
</remarks>
*)
    property F: double read FF;
    (*<summary>Returns the probability of F.</summary>
      
<remarks>Returns the probability of F. This is the p-value for the <see cref="F"/> value. The p-value
      is the probability that the test statistic will take on a value at least as extreme as the observed
      value, assuming that the null hypothesis is true.
</remarks>
*)
    property SignifProb: double read FSignifProb;
    (*<summary>Returns total sum of squares.</summary>*)
    property TSS: double read FTSS;
    (*<summary>Returns regression sum of squares.</summary>*)
    property RSS: double read FRSS;
    (*<summary>Returns error sum of squares.</summary>*)
    property SSE: double read FSSE;

    
    Destructor Destroy; override;
    
  end;

  
  
  
  (*<summary>Calculates additional parameters from multiple linear regression.</summary>
    
<remarks>Calculates additional parameters from multiple linear regression. Use this class to calculate
    regression coefficients (B) confidence interval, regression coefficients standard deviations and
    residuals.

    <b>How to use TRegResultClass class?</b>
    1. Create TRegResultClass: TestClass := TRegResultClass.Create;
    2. Use TestClass.B and TestClass.YCalc vectors as parameters in <see cref="MulLinRegress"/> routine call.
    3. Calculate additional parameters by using the <see cref="CalculateRegResults"/> method.
</remarks>


    

    <example>
      The following example was taken from TMtxMulLinReg.Recalc method:
    <code>
    using Dew.Math;
    using Dew.Stats;
    using Dew.Stats.Units;

    namespace Dew.Examples
    {
      private void Example()
      {
         Matrix A = new Matrix(0, 0);
         Matrix V = new Matrix(0, 0);
         Vector Y = new Vector(100);
         TRegResultClass regres = new TRegResultClass();
         TRegStatsClass regstat = new TRegStatsClass();
         Regress.MulLinRegress(Y, A, regres.B, true, regres.YCalc, V, TRegSolveMethod.regSolveLQR);
         regres.CalculateRegResults(Y, V, regstat,false, null, 0.03);
      }
    }
    </code></example>

    <SeeAlso cref="TMtxMulLinReg"/>
    <SeeAlso cref="Regress.MulLinRegress"/>
    <SeeAlso cref="TRegStatsClass"/>*)
  TRegResultClass = class
  strict private
    FBConfInt: TMtx;
    FB: TVec;
    FYCalc: TVec;
    FResiduals: TVec;
    FBStdDev: TVec;
    tmpVec: TVec;
    function TVal(Y: TVec; V: TMtx; Alpha: double): double;
    procedure SetB(const Value: TVec);
  public
    (*<summary>Calculates additional parameters from multiple linear regression.</summary>*)
    procedure CalculateRegResults(Y: TVec; V: TMtx; Constant: Boolean = True; Weights: TVec = nil; Alpha: double = 0.05); overload;
    (*<summary>Calculates regression coefficient confidence intervals, standard deviation of regression coefficients, residuals and (optional) basic regression statistics.</summary>
      <param name="Y">Vector of dependent variable.</param>
      <param name="V">inverse matrix of normal equations.</param>
      <param name="Constant">If true then include intercept term b(0) in calculations.
        If false - set intercept term (0) to 0.0.</param>
      <param name="Weights">Weights (optional).</param>
      <param name="Alpha">Desired significance probability.</param>
      <param name="RegStats">Returns regression statistics.</param>

      
<remarks>The results are stored to:
      <list>
      <item> <see cref="BConfInt"/> : regression coefficients 100(1-Alpha) confidence intervals.</item>
      <item> <see cref="BStdDev"/> : standard deviation of regression coefficients.</item>
      <item> <see cref="Residuals"/> : Residuals.</item>
      <item> RegStats : Regression statistics.</item>
      </list>
</remarks>
*)
    procedure CalculateRegResults(Y: TVec; V: TMtx; RegStats: TRegStatsClass; Constant: Boolean = True; Weights: TVec = nil; Alpha: double = 0.05); overload;
    
    procedure Assign(Source: TRegResultClass);
    

    (*<summary>Regression coefficients.</summary>
      
<remarks>Defines regression coefficients. Regression coefficients can be passed
      from <see cref="Regress.MulLinRegress"/> routine or from <See Class="TMtxMulLinReg"/> component.
</remarks>


      <SeeAlso cref="BConfInt"/>
      <SeeAlso cref="BStdDev"/>*)
    property B: TVec read FB write SetB;
    (*<summary>Regression coefficients standard deviations.</summary>
      
<remarks>Returns regression coefficients standard deviations.
</remarks>


      <SeeAlso cref="BConfInt"/>
      <SeeAlso cref="B"/>*)
    property BStdDev: TVec read FBStdDev;
    (*<summary>Regression coefficients confidence intervals.</summary>
      
<remarks>BConfInt matrix returns the 100*(1-Alpha) regression coefficients
      confidence intervals. The first BConfInt column is lower confidence
      interval, the second column is upper confidence interval.
</remarks>


      <SeeAlso cref="BStdDev"/>
      <SeeAlso cref="B"/>*)
    property BConfInt: TMtx read FBConfInt;
    (*<summary>Dependent variable predicted values.</summary>
      
<remarks>Defines dependent variable predicted (calculated) values. YCalc can be passed from
      <see cref="Regress.MulLinRegress"/> routine or from <see cref="TMtxMulLinReg"/> component.
</remarks>


      <SeeAlso cref="Residuals"/>*)
    property YCalc: TVec read FYCalc;
    (*<summary>Regresion equation residuals.</summary>
      
<remarks>Returns the residuals of the regression equation. The residuals are defined
      as difference between predicted and actual values of the dependent variable:

      <c>Residual = YCalc - Y.</c><para/>
</remarks>


      <SeeAlso cref="B"/>
      <SeeAlso cref="YCalc"/>*)
    property Residuals: TVec read FResiduals;
    
    constructor Create;
    Destructor Destroy; override;
    
  end;

  (*<summary>Defines hypothesis test/method.</summary>
    
<remarks>Defines which hypothesis test/method will be used by <see cref="TMtxHypothesisTest"/> component.
</remarks>
*)
  THypothesisMethod = (
    (*<summary>Sign test (comparing data median with value).</summary>*)
    hmSignTest,
    (*<summary>Sign test on paired data (comparing medians of two datasets).</summary>*)
    hmSignTestPaired,
    (*<summary>Wilcoxon Signed Rank test (comparing data median with value).</summary>*)
    hmWilcoxonSign,
    (*<summary>Wilcoxon Signed Rank test (comparing medians of two datasets).</summary>*)
    hmWilcoxonSignPaired,
    (*<summary>Z-Test (comparing dataset mean with value. Dataset variance is known).</summary>*)
    hmZTest,
    (*<summary>One sample t-Test (comparing dataset mean with value).</summary>*)
    hmTTest1,
    (*<summary>Two sample t-Test on pooled data(comparing means of two datasets. Dataset variances are equal, but unknown).</summary>*)
    hmTTest2Pooled,
    (*<summary>Two sample t-Test on paired data (comparing means of two datasets. Dataset variances are equal, but unknown).</summary>*)
    hmTTest2Paired,
    (*<summary>One sample Chi-Squared test (comparing data variance with value).</summary>*)
    hmChiSquareTest,
    (*<summary>Two sample F-Test (comparing variances of two datasets).</summary>*)
    hmFTest,
    (*<summary>Two sample Mann-Whitney U test.</summary>*)
    hmMannWhitney,
    (*<summary>One sample Shapiro-Francia test.</summary>*)
    hmShapiroFrancia,
    (*<summary>One sample Shapiro-Wilks test.</summary>*)
    hmShapiroWilks,
    (*<summary>One sample Anderson Darling normality test.</summary>*)
    hmAndersonDarling
  );

  

  


  (*<summary>Encapsulates parametric and non-parametric hypothesis testing routines.</summary>
    
<remarks>Encapsulates parametric and non-parametric hypothesis testing routines.
    Many problems in engineering require that we decide whether to accept or
    reject a statement about some parameter. The statement is called a hypothesis
    and the decision-making procedure about the hypothesis is called hypothesis testing.

    The following hypothesis tests are supported:
    * one or two sample Sign test,
    * one or two sample Wilcoxon Signed-Rank test,
    * one or two sample (on pooled or paired data) T test,
    * one sample Z test,
    * one sample ChiSquared test,
    * two sample F test.

    <b>How to use TMtxHypothesisTest component?</b>
    * Drop a TMtxHypothesisTest component on the form.
    * Define the hypothesis test you want to perform (Sign, Wilcoxon, Z, T, ChiSquared, F).
    * Define hypothesis type (the null hypothesis type) : two, left or right tailed.
    * Depending on  hypothesis method define one or two sample datasets. You can do this by
      clicking on <see cref="DataVec1"/> (and <see cref="DataVec2"/>) properties.
    * Depending on hypothesis method you'll also have to define test mean (and in Z, F Test
      case sigma) property.
    * Define the desired significance level <see cref="Alpha"/>.
    * Call the <see cref="Recalc"/> method to calculate hypothesis test results.

    <b>Results:</b>
    * <see cref="Result"/> : The hypothesis result (accept, reject null hypothesis).
    * <see cref="Significance"/> : actual significance level.
    * <see cref="ConfLower"/>,<see cref="ConfUpper"/> : 100*(1-Alpha) confidence
      interval limits for mean (median, standard deviation).
</remarks>


    

    <example>
      How to setup and run two sample t-test.
    <code>
    using Dew.Stats.Units;
    using Dew.Stats;
    using Dew.Math;
    namespace Dew.Examples
    {
      private void Example(TMtxHypothesis hyp)
      {
        hyp.Alpha = 0.03; // desired significance level at 3%
        hyp.HypothesisType = THypothesisType.htTwoTailed; // two tailed => Ha= means are not equal
        hyp.DataVec1.SetIt(false, new double[] { 1.0, 2.0, 3.0, 4.0 }); // first dataset
        hyp.DataVec2.SetIt(false, new double[] { 1.5, 3.1, 4.2, 4.3 }); // second dataset
        hyp.HypothesisMethod = THypothesisMethod.hmTTest2Pooled;  // comparing means of two datasets
        hyp.Recalc();
        //Results ==> Significance = 0.43036618314 , Result = hrNotReject
      }
    }
    </code></example>*)

  TMtxHypothesisTest=class(TMtxComponent)
  strict private
    FResult: THypothesisResult;
    FHypothesisType: THypothesisType;
    FAlpha: double;
    FConfLower: double;
    FConfUpper: double;
    FSignificance: double;
    FDataVec1: TVec;
    FDataVec2: TVec;
    FMean,FSigma,FTestStatistics : double;
    FHypothesisMethod: THypothesisMethod;
    FAutoUpdate: boolean;
    FDirty: boolean;
    procedure SetHypothesisType(const Value: THypothesisType);
    procedure SetAlpha(const Value: double);
    procedure SetMean(const Value: double);
    procedure SetSigma(const Value: double);
    procedure SetHypothesisMethod(const Value: THypothesisMethod);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetDirty(const Value: boolean);
    procedure SetDataVec1(const Value: TVec);
    procedure SetDataVec2(const Value: TVec);
  strict protected
    
    procedure Loaded; override;
    
  public
    
    class function EditorClass: string; override;
    

    (*<summary>When any of the TMtxHypothesisTest properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty : boolean read FDirty write SetDirty;
    (*<summary>Returns calculated significance level.</summary>
      
<remarks>Returns calculated significance level. If this value is bellow the desired significance
      (<see cref="Alpha"/>), the null hypothesis is rejected.
</remarks>


      <SeeAlso cref="Alpha"/>*)
    property Significance : double read FSignificance;
    (*<summary>Returns test statistics.</summary>*)
    property TestStatistics: double read FTestStatistics;
    (*<summary>Returns lower confidence interval limit.</summary>
      
<remarks>Returns 100(1-<see cref="Alpha"/>) confidence interval upper limit.
</remarks>


      <SeeAlso cref="ConfUpper"/>*)
    property ConfLower: double read FConfLower;
    (*<summary>Returns upper confidence interval limit.</summary>
      
<remarks>Returns 100(1-<see cref="Alpha"/>) confidence interval upper limit.
</remarks>


      <SeeAlso cref="ConfLower"/>*)
    property ConfUpper: double read FConfUpper;
    (*<summary>Returns hypothesis result.</summary>*)
    property Result: THypothesisResult read FResult;
    (*<summary>Triggers hypothesis test recalculation.</summary>
      
<remarks>Triggers hypothesis test recalculation. After the Recalc call <see cref="Dirty"/> property is set to false.
      The results of hypothesis test are stored to:
      * <see cref="Result"/> - Reject/do not reject hypothesis.
      * <see cref="ConfLower"/> - Lower confidence interval for mean, standard deviation, median, ...
      * <see cref="ConfUpper"/> - Upper confidence interval for mean, standard deviation, median, ...
      * <see cref="Significance"/> - Calculated significance probability.
</remarks>


      <SeeAlso cref="ConfLower"/>
      <SeeAlso cref="ConfUpper"/>
      <SeeAlso cref="Result"/>
      <SeeAlso cref="Significance"/>*)
    procedure Recalc;
    
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy ; override;
    
  published
     (*<summary>If true then changing any of the TMtxHypothesisTest properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    (*<summary>Defines hypothesis type (left, right and two-tailed).</summary>*)
    
    property HypothesisType : THypothesisType read FHypothesisType write SetHypothesisType default htTwoTailed;
    (*<summary>Defines which hypothesis routine will be used for data testing.</summary>*)
    
    property HypothesisMethod: THypothesisMethod read FHypothesisMethod write SetHypothesisMethod default hmZTest;
    (*<summary>Desired significance level.</summary>
      
<remarks>Desired significance level. If the <see cref="Significance"/> probability is bellow the
      desired significance (<see cref="Alpha"/>), the null hypothesis is rejected.
</remarks>


      <SeeAlso cref="Significance"/>*)
    
    property Alpha : double read FAlpha write SetAlpha;
    (*<summary>Defines mean/median value for one-sample test.</summary>
      
<remarks>Defines value (mean, median) for one-sample test.
      If <see cref="HypothesisMethod"/> is hmTTest1 or hmZTest Mean will be compared with <See cewf="DataVec1"/>
      mean value. If HypothesisMethod is hmSignTest or hmWilcoxonSign Mean will be compared with
      sample, in this case <see cref="DataVec1"/> values.
      median value. Mean will be ignored for hmChiSquaredTest and all two-sample tests.
</remarks>


      <SeeAlso cref="Sigma"/>*)
    
    property Mean : double read FMean write SetMean;
    (*<summary>Defines standard deviation value for single test.</summary>
      
<remarks>Defines standard deviation for one-sample test.

      Note
        Sigma will be used only if <see cref="HypothesisMethod"/> is hmZTest,
        hmChiSquaredTest. Sigma will be ignored for all other tests.
</remarks>


      <SeeAlso cref="Mean"/>*)
    
    property Sigma : double read FSigma write SetSigma;
    (*<summary>First sample values.</summary>
      
<remarks>Stores first sample values. If <see cref="HypothesisMethod"/> property is
      hmTTest1, hmZTest then DataVec1 mean is compared with <see cref="Mean"/> (+ <see cref="Sigma"/> for Z-Test).
      If HypothesisMethod property is hmSignTest or hmWilcoxonTest then DataVec1 median
      is compared with <see cref="Mean"/>. If HypothesisMethod is hmChiSquared then
      DataVec1 Standard Deviation is compated with <see cref="Sigma"/>. When HypothesisMethod is
      something else then DataVec1 mean, median or standard deviation is compared with DataVec2 mean, median or
      standard deviation.
</remarks>


      <SeeAlso cref="DataVec2"/>*)
    
    property DataVec1: TVec read FDataVec1 write SetDataVec1 stored AllowStreaming;
    (*<summary>Second sample values.</summary>
      
<remarks>Stores second sample values. DataVec2 is used only if <see cref="HypothesisMethod"/> is
      hmSignTestPaired, hmWilcoxonSignPaired, hmTTest2Pooled, hmTTest2Paired or
      hmFTest (all two-sample tests). If HypothesisMethod is something else then DataVec2 is ignored.
</remarks>


      <SeeAlso cref="DataVec1"/>*)
    
    property DataVec2: TVec read FDataVec2 write SetDataVec2 stored AllowStreaming;
  end;

  

  

  (*<summary>Visual representation of One and Two way ANOVA.</summary>
    
<remarks>Encapsulates one and two way analysis of variance (ANOVA) routines. The assumptions of
    the one and two-way analysis of variance are:
    <list type="bullet">
    <item> The data are continuous (not discrete).     </item>
    <item> The data follow the normal probability distribution. Each group is normally distributed about the group mean.</item>
    <item> The variances of the populations are equal.</item>
    <item> The groups are independent. There is no relationship among the individuals in one group as compared to another.</item>
    <item> Each group is a simple random sample from its population. Each individual in the population has an equal probability of being selected in the sample.</item>
    </list>

    <b>How to use TMtxAnova component?</b>
    <list type="number">
    <item>  Drop a TMtxAnova component on the form.</item>
    <item>  Define if you'll do a one-way or two-way ANOVA (the IsAnova1 property).</item>
    <item>  If you'll be doing the two-way ANOVA, define the number of replications per "cell" (the <see cref="Replications"/> property).</item>
    <item>  Define the data you wish to analyze. Depending on IsAnova1  and Replications properties Data matrix can be interpreted in different ways.</item>
    <item>  Define the default number format for ANOVA table - the <see cref="FmtString"/> property. Default value is <c>'0.0000'</c>.</item>
    <item>  Define (optional) the ResultDest TStringGrid. TMtxAnova results will be outputted to a <see cref="ResultDest"/> string grid.</item>
    <item>  Perform ANOVA by calling the <see cref="Recalc"/> method.</item>
    </list>

    <b>Results:</b>
    <list type="number">
    <item> If you specified <see cref="ResultDest"/> then the standard ANOVA table will be written to ResultDest string grid.
           If ResultDest is nil, you can still use <see cref="WriteResultToGrid"/> or <see cref="WriteResultToStrings"/> methods
           to output ANOVA table to a TStringGrid or TStrings. </item>
    <item> <see cref="P"/> returns the significance probability of null hypothesis that means are equal. If P is less than desired
           significance <see cref="Alpha"/> then the result suggests, the null hypothesis (means are equal) can be rejected. </item>
    </list>
</remarks>


    

    <example>
      How to setup and run two sample t-test.
    <code>
    using Dew.Stats.Units;
    using Dew.Stats;
    using Dew.Math;
    namespace Dew.Examples
    {
      private void Example()
      {
        TMtxAnova an1 = new TMtxAnova(this);
        try
        {
            an1.Data.SetIt(6, 2, false, new double[] { 1, 2,
                                                        2, 3,
                                                        5, 7,
                                                        12, 1,
                                                        5, 8,
                                                        3, 8 });
            an1.IsAnova1 = false; // do two-way ANOVA
            an1.Replications = 3; // three rows per "group"
                                   // output ANOVA table to grid1
            an1.Recalc();
            double aresult = an1.Anova2Res.FCrit;
        }
        finally
        {
            an1.Free();
        }
      }
    }
    </code></example>*)

  TMtxAnova = class(TMtxComponent)
  strict private
    FDirty: boolean;
    FIsAnova1: boolean;
    FAutoUpdate: boolean;
    FReplications: Integer;
    FFmtString: String;
    FData: TMtx;
    FAlpha: double;
    FP: double;
    
    FA1Res: TAnova1Result;
    FA2Res: TAnova2Result;
    pRows, pCols, pInt : double;
    
    FResultDest: TStringGrid;
    
    procedure SetAlpha(const Value: double);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetData(const Value: TMtx);
    procedure SetDirty(const Value: boolean);
    procedure SetFmtString(const Value: String);
    procedure SetIsAnova1(const Value: boolean);
    procedure SetReplications(const Value: Integer);
    
    procedure SetResultDest(const Value: TStringGrid);
    procedure SetupResultGrid(const DestGrid: TStringGrid);
    
  strict protected
    
    procedure Loaded; override;
    
  public
    (*<summary>Results of the one way ANOVA.</summary>*)
    property Anova1Res: TAnova1Result read FA1Res;
    (*<summary>Results of the two way ANOVA.</summary>*)
    property Anova2Res: TAnova2Result read FA2Res;
    (*<summary>Calculated significance probability of null hypothesis.</summary>
      
<remarks>Returns the probability for the null hypothesis that the means of the groups
      are equal.
</remarks>


      <SeeAlso cref="Alpha"/>*)
    property P: double read FP;
    (*<summary>When any of the TMtxANOVA properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/> method
      will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty: boolean read FDirty write SetDirty;
    
    (*<summary>Outputs results to string grid.</summary>
      
<remarks>Outputs standard ANOVA table to DestGrid string grid.
</remarks>


      <Example>In this example we write the ANOVA table to
        StringGrid1. This is the same as connecting <see cref="ResultDest"/>
        property to StringGrid1. DestGrid dimensions are adjusted automatically.
      <code>
      MtxANOVA1.Data.SetIt(6,2,false,[1,2,
                                  2,3,
                                  5,7,
                                  12,1,
                                  5,8,
                                  3,8]);
      MtxANOVA1.FmtString := '0.00'; // 2 digits
      MtxANOVA1.IsAnova1 := true; // one-way ANOVA
      MtxANOVA1.Recalc;
      MtxANOVA1.WriteResultToGrid(StringGrid1);
      </code>
      <code lang="C++"/>
      MtxAnova1->Data->SetIt(6,2,false,OPENARRAY(double,(1,2,
                                  2,3,
                                  5,7,
                                  12,1,
                                  5,8,
                                  3,8)));
      MtxAnova1->FmtString = "0.00"; // 2 digits
      MtxAnova1->IsAnova1 = true; // one-way ANOVA
      MtxAnova1->Recalc();
      MtxAnova1->WriteResultToGrid(StringGrid1);
      </code>
      </Example>

      <SeeAlso cref="WriteResultToStrings"/>*)
    procedure WriteResultToGrid(DestGrid: TStringGrid);
    

    
    (*<summary>Outputs results to string list.</summary>
      
<remarks>Outputs standard ANOVA table to DestStrings. ANOVA table columns
      are separated by Delimiter (default value kTab - meaning tab char is
      the delimiter).
</remarks>


      <Example>In this example we write ANOVA table to Memo1.
      <code>
      MtxANOVA1.Data.SetIt(6,2,false,[1,2,
                                  2,3,
                                  5,7,
                                  12,1,
                                  5,8,
                                  3,8]);
      MtxANOVA1.FmtString := '0.00'; // 2 digits
      MtxANOVA1.IsAnova1 := true; // do one-way ANOVA
      MtxANOVA1.Recalc;
      Memo1.Lines.Clear;
      MtxANOVA1.WriteResultToStrings(Memo1.Lines);
      </code>
      <code lang="C++"/>
      MtxAnova1->Data->SetIt(6,2,false,OPENARRAY(double,(1,2,
                                  2,3,
                                  5,7,
                                  12,1,
                                  5,8,
                                  3,8)));
      MtxAnova1->FmtString = "0.00"; // 2 digits
      MtxAnova1->IsAnova1 = true; // one-way ANOVA
      MtxAnova1->Recalc();
      MtxAnova1->WriteResultToStrings(Memo1->Lines);
      </code>
      </Example>

      <SeeAlso cref="WriteResultToGrid"/>*)
    procedure WriteResultToStrings(DestStrings: TStrings;  Delimiter: string = kTab);
    
    (*<summary>Triggers ANOVA recalculation.</summary>
      
<remarks>Triggers ANOVA recalculation. After the Recalc call <see cref="Dirty"/> property is set to false.
      If <see cref="ResultDest"/> property is not nil then the standard ANOVA table will be written to
      ResultTest string grid. To visualize ANOVA table you can also use <see cref="WriteResultToGrid"/> or
      <see cref="WriteResultToStrings"/> methods.
</remarks>


      <SeeAlso cref="WriteResultToStrings"/>
      <SeeAlso cref="WriteResultToGrid"/>*)
    procedure Recalc;
    
    constructor Create(AOwner: TComponent); override;
    
    Destructor Destroy; override;
    
  published
    
    
    (*<summary>If true then changing any of the TMtxAnova properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate default false;
    
    (*<summary>Data for ANOVA.</summary>
      
<remarks>Stores the data which is analyzed with analysis of variance (ANOVA).
      Depending on <see cref="IsAnova1"/> and <see cref="Replications"/>
      properties, Data storage format is:
      * IsAnova1 = true : each column represents a separate group
      * IsAnova1 = false : each column represents changes in one factor,
        <see cref="Replications"/> rows represent changes in other factor.
</remarks>
*)
    property Data: TMtx read FData write SetData stored AllowStreaming;
    
    (*<summary>One or two-way ANOVA.</summary>
      
<remarks>If true then TMtxANOVA performs one-way ANOVA. If false, then TMtxAnova
      performs two-way ANOVA.
</remarks>


      <SeeAlso cref="Replications"/>*)
    property IsAnova1: boolean read FIsAnova1 write SetIsAnova1 default true;
    
    (*<summary>Number of observations per data matrix "cell".</summary>
      
<remarks>Defines the number of observations per <see cref="Data"/> matrix "cell",
      where each cell includes <see cref="Replications"/> number of rows. For example,
      if Data has 6 rows and Replications is 2, then the "row" factor has 6/2 = 3 levels
      (1st level row 0..1, 2nd level row 2..3, 3rd level row 4..5). An exception is raised
      if A.Rows mod Replications is not zero (if A.Rows/Replications is not an integer).
      The Replications factor is taken into the account ONLY if <see cref="IsAnova1"/>
      is false (two-way ANOVA).
</remarks>


      <Example>  Note that Data.Rows mod 3 = 0 !!
      <code>
      MtxAnova1.Data.SetIt(6,2,false,[1,2,
                                    2,3,
                                    5,7,
                                    12,1,
                                    5,8,
                                    3,8]);
      MtxAnova1.IsAnova1 := false; // do two-way ANOVA
      MtxAnova1.Replications := 3; // three rows per "group"
      MtxAnova1.ResultDest := StringGrid1;
      MtxAnova1.Recalc;
      </code>
      <code lang="C++">
      MtxAnova1->Data->SetIt(6,2,false,OPENARRAY(double,(1,2,
                                    2,3,
                                    5,7,
                                    12,1,
                                    5,8,
                                    3,8)));
      MtxAnova1->IsAnova1 = false; // do two-way ANOVA
      MtxAnova1->Replications = 3; // three rows per "group"
      MtxAnova1->ResultDest = StringGrid1;
      MtxAnova1->Recalc();
      </code>
      </Example>

      <SeeAlso cref="IsAnova1"/>*)
    property Replications: Integer read FReplications write SetReplications default 1;
    
    (*<summary>Desired significance probability.</summary>
      
<remarks>Desired significance probability, used to calculate the critical F value for
      one-way and two-way ANOVA.
</remarks>


      <SeeAlso cref="P"/>*)
    property Alpha : double read FAlpha write SetAlpha;
    
    (*<summary>Numeric format for ANOVA table cells.</summary>
      
<remarks>Defines the number format used for all TMtxANOVA floating-point values.
</remarks>


      <Example>In the following example we do two-way ANOVA with
      two rows per group and set default format for numbers to '0.000000'
      i.e. 6 digits.
      <code>
      MtxAnova1.Data.SetIt(4,4,false,[1,2,3,4,
                                      3,4,5,6,
                                      1,5,11,13,
                                      4,7,9,10]);
      MtxAnova1.FmtString := '0.000000'; // &lt; - - 6 digits
      MtxAnova1.IsAnova1 := false; //do two-way ANOVA
      MtxAnova1.Replications := 2; //two rows per "group"
      MtxAnova1.ResultDest := StringGrid1;
      MtxAnova1.Recalc;
      </code>
      <code lang="C++">
      MtxAnova1->Data->SetIt(4,4,false,OPENARRAY(double,(1,2,3,4,
                                      3,4,5,6,
                                      1,5,11,13,
                                      4,7,9,10)));
      MtxAnova1->FmtString = "0.000000"; // &lt; - - 6 digits
      MtxAnova1->IsAnova1 = false; //do two-way ANOVA
      MtxAnova1->Replications = 2; //two rows per "group"
      MtxAnova1->ResultDest = StringGrid1;
      MtxAnova1->Recalc();
      </code>
      </Example>*)
    property FmtString: String read FFmtString write SetFmtString;
    
    (*<summary>Outputs ANOVA table.</summary>
      
<remarks>Use this property to output standard ANOVA table to ResultDest string grid.
      ResultDest grid dimensions are adjusted automatically after the call to
      <see cref="Recalc"/> call.
</remarks>


      <SeeAlso cref="WriteResultToGrid"/>
      <SeeAlso cref="WriteResultToStrings"/>*)
    property ResultDest: TStringGrid read FResultDest write SetResultDest;
    
  end;
  
  

  

  (*<summary>Performs nonlinear regression.</summary>
    
<remarks><b>How to use TMtxNonLinReg component?</b>
    <list type="number">
    <item> Drop a <see cref="TMtxNonLinReg"/> component on the form. </item>
    <item> Define <see cref="X"/> the vector of independent variables. </item>
    <item> Define <see cref="Y"/> the vector of dependent variables. Make sure that X and Y have the same length. </item>
    <item> Define initial estimates for <see cref="B"/> regression parameters. </item>
    <item> (Optionaly) define <see cref="Weights"/>. If you'll use weights, set <see cref="UseWeights"/> property to true.</item>
    <item> Define regression function. </item>
    <item> (Optionally) define the derivative procedure. If you don't define derivative procedure then the numeric approximation will be used to calculate the
       derivative at specific point. </item>
    <item> Define the optimization method. Depending on your function different methods will give better/worse results. </item>
    <item> (Optionally) define desired tolerance for result. </item>
    <item> Define the maximum number of steps for optimization method. Internal optimization
        method will stop when number of iterations exceeds MaxIter or internal minimum precision is within Tolerance.</item>
    <item> Verbose property, if assigned, stores Fun, evaluated at each iteration step.
        Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose property. This allows the optimization procedure
        to be interrupted from another thread and optionally also allows logging and iteration count monitoring.</item>
    </list>

    In case of performance problems consider using TMtxMultiNonLinReg. That component internally performs vectorized
    optimization process. It can also be used to speed-up nonlinear regression with only one independent variable
    by 10-20x. <para/>

    <b>Results:</b>
    <list type="number">
    <item>  <see cref="B"/> : Regression coefficients estimates.</item>
    <item>  <see cref="YCalc"/> : Fitted Y values.</item>
    </list>
</remarks>


    

    <example>
      In the following example we use the TMtxNonLinReg component to
      fit generated data to non-linear function <c>B[0]/Power((1.0 + Exp(B[1]-B[2]*X)),1/B[3])</c>.
      In this example exact derivate procedure is not used - algorithm uses numerical
      derivatives:
    <code>
    using Dew.Math;
    using Dew.Stats;
    using Dew.Stats.Units;
    using System;
    namespace Dew.Examples
    {
      private double Rat43(TVec b, double x)
      {
        return b[0] / Math.Pow((1.0 + Math.Exp(b[1]-b[2]*x)),1/b[3]);
      }

      private void Example(TMtxNonLinReg nlr)
      {
          // Load data - independent variable
          nlr.X.SetIt(false, new double[] {9.000, 14.000, 21.000, 28.000,
                                  42.000, 57.000, 63.000, 70.000,
                                  79.000});
          // Load data - dependent variable
          nlr.Y.SetIt(false, new double[] {8.930, 10.800, 18.590, 22.330,
                                  39.350, 56.110, 61.730, 64.620,
                                  67.080});
          // Initial estimates for regression coefficients
          nlr.B.SetIt(false, new double[] { 100, 10, 1, 1 });
          // setup optimization parameters
          nlr.Tolerance = 1.0e-6; // 6 digits should do the trick
          nlr.GradTolerance = 1.0e-3; // 3 digits
          nlr.MaxIteration = 400;
          nlr.RegressFunction += Rat43; // regression function
                                       // Marquardt method
          nlr.OptMethod = TOptMethod.optMarquardt;
          nlr.Recalc();
          // MtxNinLinReg->b now stores calculated regression parameter estimates
      }
    }
    </code></example>*)

  TMtxNonLinReg=class(TMtxComponent)
  strict private
    FVerbose: TStrings;
    FOptMethod: TOptMethod;
    FX: TVec;
    FY: TVec;
    FWeights: TVec;
    FMaxIteration: Integer;
    FB: TVec;
    FGradTolerance: double;
    FDirty: boolean;
    FAutoUpdate: boolean;
    FUseWeights: boolean;
    FDeriveProcedure: TDeriveProc;
    FRegressFunction: TRegressFun;
    FYCalc: TVec;
    FStopReason: TOptStopReason;
    FSoftSearch: boolean;
    FTolerance: double;
    procedure SetVerbose(const Value: TStrings);
    procedure SetOptMethod(const Value: TOptMethod);
    procedure SetMaxIteration(const Value: Integer);
    procedure SetGradTolerance(const Value: double);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetX(const Value: TVec);
    procedure SetY(const Value: TVec);
    procedure SetWeights(const Value: TVec);
    procedure SetB(const Value: TVec);
    procedure SetUseWeights(const Value: boolean);
    procedure SetDeriveProcedure(const Value: TDeriveProc);
    procedure SetRegressFunction(const Value: TRegressFun);
    procedure SetSoftSearch(const Value: boolean);
    procedure SetTolerance(const Value: double);
  strict protected
    
    procedure Loaded; override;
    
  public
    (*<summary>Number of iterations needed to converge to solution.</summary>
      
<remarks>Returns number of iterations needed to converge to solution with
      given precision (specified by <see cref="Tolerance"/> property).
</remarks>
*)
    Iterations: Integer;
    (*<summary>If not nil then the optimization method uses it for logging each optimization step.</summary>
      
<remarks>If not nil then the optimization method uses it for logging each optimization step.
      By default the Verbose property is nil meaning no logging is done.

      If assigned, stores Fun, evaluated at each iteration step.
      Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose property. This allows the optimization procedure
      to be interrupted from another thread and optionally also allows logging and iteration count monitoring.
</remarks>


      

      <example>
        Log to Memo.Lines:
      <code>
      TStringList log = new TStringList();
      nlr.Verbose = log;
      nlr.Recalc();
      </code></example>*)
    property Verbose: TStrings read FVerbose write SetVerbose;
    (*<summary>Triggers non-linear regression recalculation.</summary>
      
<remarks>Triggers non-linear regression recalculation. After the Recalc call
      <see cref="Dirty"/> property is set to false.

      The results are returned as:
      * <see cref="B"/> : Regression coefficients estimates.
      * <see cref="YCalc"/> : Fitted Y values.
</remarks>


      <SeeAlso cref="B"/>
      <SeeAlso cref="YCalc"/>*)
    procedure Recalc;
    
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    

    (*<summary>Defines the regression function of several regression parameters.</summary>
      
<remarks>Defines the regression function of several regression parameters B and independent variable X.
</remarks>


      

      <example>
        In the following example we define general polynomial of second degree (three parameters)
        and also the procedure for derivatives:
      <code>
      // y=b0*x*x + b1*x + b2 i.e. parabola
      double SimpleParabola(TVec b, double x)
      {
        return b[0]*x*x + b[1]*x + b[2];
      }

      // procedure for derivatives
      void SimplePabolaDeriv(TRegressFun RegressFun, double x, double y, double[] pars, TVec Grad)
      {
        Grad[0] = x*x;
        Grad[1] = x;
        Grad[2] = 1;
      }

      void Example();
      {
        // ...
        Regress.MtxNonLinReg1.RegressFunction = SimpleParabola;
        Regress.MtxNonLinReg1.DeriveProcedure = SimpleParabolaDeriv;
      }
      </code></example>

      <SeeAlso cref="DeriveProcedure"/>*)
    property RegressFunction: TRegressFun read FRegressFunction write SetRegressFunction;
    (*<summary>Defines derivatives of regression function.</summary>
      
<remarks>Defines the procedure for calculating the derivatives of a regression function
      <see cref="TRegressFun"/> with respect to the regression parameters <see cref="B"/>, evaluated
      at specific set of (<see cref="X"/>,<see cref="Y"/>) points.

      If DeriveProcedure is not declared (for example if it's nil) <see cref="TMtxNonLinReg"/> will
      use the approximate numeric derivative. In this case the gradient step size is defined by
      GradientStepSize (Optimization unit) global variable. Normally the optimal stepsize depends on
      seventh partial derivatives of the function. Since they (in this case) are not available, the initial
      value for GradientStepSize is <c>Exp(Ln(EPS)/7)*0.25</c> (as suggested by Spellucci).
</remarks>
*)
    property DeriveProcedure: TDeriveProc read FDeriveProcedure write SetDeriveProcedure;
    
    (*<summary>Vector of calculated values.</summary>
      
<remarks>Returns fitted dependent <c>(YCalc=B*X)</c> values. Use YCalc to calculate the residuals:

      <c>Residuals = YCalc - Y.</c>
</remarks>
*)
    property YCalc: TVec read FYCalc;
    (*<summary>Returns the reason why regression parameters calculation stopped.</summary>
      
<remarks>Returns the reason why regression parameters calculation stopped. All stop reasons are
      declared and described in MtxVec Optimization.pas unit.
</remarks>
*)
    property StopReason: TOptStopReason read FStopReason;
    (*<summary>When any of the TMtxNonLinReg properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty: boolean read FDirty write SetDirty;
  published
    
    (*<summary>If true then changing any of the TMtxNonLinReg properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    
    
    (*<summary>Stores the regression coefficients.</summary>
      
<remarks>Stores the regression coefficients. Set B to define initial values for regression
      coefficients. After the <see cref="Recalc"/> call B stores calculated regression coefficients.
</remarks>


      <SeeAlso cref="X"/>
      <SeeAlso cref="Y"/>*)
    property B : TVec read FB write SetB stored AllowStreaming;
    
    (*<summary>Vector of independant variables.</summary>
      
<remarks>Stores independant variables.
</remarks>


      <SeeAlso cref="Y"/>*)
    property X : TVec read FX write SetX stored AllowStreaming;
    
    (*<summary>Vector of the dependant variable.</summary>
      
<remarks>Stores dependant variables.
</remarks>


      <SeeAlso cref="X"/>*)
    property Y : TVec read FY write SetY stored AllowStreaming;
    
    (*<summary>Weights.</summary>
      
<remarks>Stores non-linear regression weights.

      Note
        Weights are used only if <see cref="UseWeights"/> property is set to true.
</remarks>


      <SeeAlso cref="UseWeights"/>*)
    property Weights: TVec read FWeights write SetWeights stored AllowStreaming;
    
    (*<summary>Optimization method used.</summary>
      
<remarks>Defines which optimization method will be used to find
      regression coefficients estimates. Several different optimization algorithms are supported
      (see Dew Math Optimization assembly help for more on this topic):
      * optSimplex : Nelder-Mead optimization method.
      * optMarquardt : Marquardt optimization method.
      * optBFGS : Quasi-Newton optimization method (Broyden-Fletcher-Goldfarb-Shanno Hessian update).
      * optDFP: Quasi-Newton optimization method (Davidson-Fletcher-Power Hessian update).
      * optConjGradFR: Conjugate Gradient optimization method (Fletcher-Reeves).
      * optConjGradPR : Conjugate Gradient optimization method (Polak-Ribiere).
</remarks>
*)
    property OptMethod : TOptMethod read FOptMethod write SetOptMethod default optMarquardt;
    
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIter parameters define the conditions for terminating the regression
      coefficients calculation (number of iterations exceeds MaxIter or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="Tolerance"/>*)
    property MaxIteration : Integer read FMaxIteration write SetMaxIteration default 500;
    
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIter parameters define the conditions for terminating the regression
      coefficients calculation (number of iterations exceeds MaxIter or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="MaxIteration"/>*)
    property Tolerance: double read FTolerance write SetTolerance;
    
    (*<summary>Defines minimal allowed gradient C-Norm.</summary>
      
<remarks>Defines minimal allowed gradient <c>C-Norm</c>. If during the regression parameters calculation
      gradient C-Norm falls bellow GradTolerance then calculation will stop and return
      StopReason = OptResSmallGrad.
</remarks>


      <SeeAlso cref="MaxIteration"/>
      <SeeAlso cref="Tolerance"/>*)
    property GradTolerance : double read FGradTolerance write SetGradTolerance;
    
    (*<summary>Weighted non-linear regression.</summary>
      
<remarks>If this property is true then internal algorithm will perform weighted
      non-linear regression. In this case you must specify the weights (set the <see cref="Weights"/>
      vector).
</remarks>


      <SeeAlso cref="Tolerance"/>*)
    property UseWeights : boolean read FUseWeights write SetUseWeights default false;
    
    (*<summary>Defines line search algorithm for Quasi-Newton and Conjugate methods.</summary>
      
<remarks>If true then Quasi-Newton and Conjugate gradient method internal line search
      algoritm will use soft line search  method. Set <see cref="SoftSearch"/> to true if
      you're using numerical approximation for derivative. If SoftSearch if false, then the internal
      line search algorithm will use exact line search method. Set SoftSearch to false if you're using
      <i>exact</i> derivative procedure.
</remarks>
*)
    property SoftSearch: boolean read FSoftSearch write SetSoftSearch default true;
  end;


  

  

  (*<summary>Performs multiple-nonlinear regression.</summary>
    
<remarks><b>How to use TMtxMultiNonLinReg component?</b>
    1. Drop a <see cref="TMtxMultiNonLinReg"/> component on the form.
    2. Define <see cref="X"/> the vector of independent variables.
    3. Define <see cref="Y"/> the vector of dependent variables. Make sure
       that X and Y have the same length.
    4. Define initial estimates for <see cref="B"/> regression parameters.
    5. (Optionaly) define <see cref="Weights"/>. If you'll use weights, set
        <see cref="UseWeights"/> property to true.
    6. Define regression function.
    7. (Optionally) define the derivative procedure. If you don't define derivative
       procedure then the numeric approximation will be used to calculate the
       derivative at specific point.
    8. Define the optimization method. Depending on your function different
       methods will give better/worse results.
    9. (Optionally) define desired tolerance for result.
    10. Define the maximum number of steps for optimization method. Internal optimization
        method will stop when number of iterations exceeds MaxIter or internal minimum
        precision is within Tolerance.
    11. Verbose property, if assigned, stores Fun, evaluated at each iteration step.
        Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose property. This allows the optimization procedure
        to be interrupted from another thread and optionally also allows logging and iteration count monitoring.

    <b>Results:</b>
    1. <see cref="B"/> : Regression coefficients estimates.
    2. <see cref="YCalc"/> : Fitted Y values.

    The component internally performs vectorized optimization process. It can also be used to speed-up
    nonlinear regression with only one independent variable by 10-20x.
</remarks>


    

    <example>
      In the following example we use the TMtxMultiNonLinReg component to
      fit generated data to non-linear function <c>B[0]/Power((1.0 + Exp(B[1]-B[2]*X)),1/B[3])</c>.
      In this example exact derivate procedure is not used - algorithm uses numerical
      derivatives:
    <code>
    using Dew.Math;
    using Dew.Stats;
    using Dew.Stats.Units;
    using System;
    namespace Dew.Examples
    {
      private void Rat43(TVec b, TVecList x, TVec y)
      {
//        return b[0] / Math.Pow((1.0 + Math.Exp(b[1]-b[2]*x)),1/b[3]);

        y.Mul(x[0], -B[2]);
        y.Add(B[1]);
        y.Exp();
        y.Add(1);
        y.Power(1/B[3]);
        y.DivideBy(B[0]);
      }

      private void Example(TMtxMultiNonLinReg nlr)
      {
        // Load data - independent variable
        nlr.X.Add():
        nlr.X[0].SetIt(false,new double[] {9.000, 14.000, 21.000, 28.000,
                                    42.000, 57.000, 63.000, 70.000,
                                    79.000});
        // Load data - dependent variable
        nlr.Y.SetIt(false,new double[] {8.930, 10.800, 18.590, 22.330,
                                    39.350, 56.110, 61.730, 64.620,
                                    67.080});
        // Initial estimates for regression coefficients
        nlr.B.SetIt(false,new double[] {100,10,1,1});
        // setup optimization parameters
        nlr.Tolerance = 1.0e-6; // 6 digits should do the trick
        nlr.GradTolerance = 1.0e-3; // 3 digits
        nlr.MaxIterations = 400;
        nlr.RegressFunction = +Rat43; // regression function
        // Marquardt method
        nlr.OptMethod = TOptMethod.optMarquardt;
        nlr.Recalc();
        // MtxNinLinReg->b now stores calculated regression parameter estimates
      }
    }
    </code></example>*)

  TMtxMultiNonLinReg=class(TMtxComponent)
  strict private
    FVerbose: TStrings;
    FOptMethod: TOptMethod;
    FX: TVecList;
    FY: TVec;
    FWeights: TVec;
    FMaxIteration: Integer;
    FB: TVec;
    FGradTolerance: double;
    FDirty: boolean;
    FAutoUpdate: boolean;
    FUseWeights: boolean;
    FDeriveProcedure: TMultiDeriveProc;
    FRegressFunction: TMultiRegressFun;
    FYCalc: TVec;
    FStopReason: TOptStopReason;
    FSoftSearch: boolean;
    FTolerance: double;
    procedure SetVerbose(const Value: TStrings);
    procedure SetOptMethod(const Value: TOptMethod);
    procedure SetMaxIteration(const Value: Integer);
    procedure SetGradTolerance(const Value: double);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetX(const Value: TVecList);
    procedure SetY(const Value: TVec);
    procedure SetWeights(const Value: TVec);
    procedure SetB(const Value: TVec);
    procedure SetUseWeights(const Value: boolean);
    procedure SetDeriveProcedure(const Value: TMultiDeriveProc);
    procedure SetRegressFunction(const Value: TMultiRegressFun);
    procedure SetSoftSearch(const Value: boolean);
    procedure SetTolerance(const Value: double);
  strict protected
    
    procedure Loaded; override;
    
  public
    (*<summary>Number of iterations needed to converge to solution.</summary>
      
<remarks>Returns number of iterations needed to converge to solution with
      given precision (specified by <see cref="Tolerance"/> property).
</remarks>
*)
    Iterations: Integer;
    (*<summary>If not nil then the optimization method uses it for logging each optimization step.</summary>
      
<remarks>If not nil then the optimization method uses it for logging each optimization step.
      By default the Verbose property is nil meaning no logging is done. <para/>

      If assigned, stores Fun, evaluated at each iteration step.
      Optionally, you can also assign <see cref="TOptControl"/> object to the Verbose property. This allows the optimization procedure
      to be interrupted from another thread and optionally also allows logging and iteration count monitoring.
</remarks>


      

      <example>
        Log to Memo.Lines:
      <code>
      TStringList log = new TStringList();
      nlr.Verbose = log;
      nlr.Recalc();
      </code></example>*)
    property Verbose: TStrings read FVerbose write SetVerbose;
    (*<summary>Triggers non-linear regression recalculation.</summary>
      
<remarks>Triggers non-linear regression recalculation. After the Recalc call <see cref="Dirty"/> property is set to false.

      The results are returned as:
      * <see cref="B"/> : Regression coefficients estimates.
      * <see cref="YCalc"/> : Fitted Y values.
</remarks>


      <SeeAlso cref="B"/>
      <SeeAlso cref="YCalc"/>*)
    procedure Recalc;
    
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    

    (*<summary>Defines the regression function of several regression parameters.</summary>
      
<remarks>Defines the regression function of several regression parameters B and independent variable X.
</remarks>


      

      <example>
        In the following example we define general polynomial of second degree (three parameters)
        and also the procedure for derivatives:
      <code>
      // y=b0*x*x + b1*x + b2 i.e. parabola
      void SimpleParabola(TVec b, TVecList x, TVec y)
      {
//        return b[0]*x*x + b[1]*x + b[2];

          y.Sqr(x[0]);
          y.Scale(b[0));
          y.AddScaled(x[0], b[1]);
          y.Add(b[2]);

      }

      // procedure for derivatives
      void SimplePabolaDeriv(TRegressFun RegressFun, TVecList x, TVec y, double[] pars, TVecList Grad)
      {
          Grad[0].Sqr(x[0]);        //  Grad[0] := x*x;
          Grad[1].Copy(x[0]);       //  Grad[1] := x;

          Grad[2].Size(x[0]);       //  Grad[2] := 1;
          Grad[2].SetVal(1);
      }

      void Example();
      {
        // ...
        Regress.MtxNonLinReg1.RegressFunction = SimpleParabola;
        Regress.MtxNonLinReg1.DeriveProcedure = SimpleParabolaDeriv;
      }
      </code></example>

      <SeeAlso cref="DeriveProcedure"/>*)
    property RegressFunction: TMultiRegressFun read FRegressFunction write SetRegressFunction;
    (*<summary>Defines derivatives of regression function.</summary>
      
<remarks>Defines the procedure for calculating the derivatives of a regression function
      <see cref="TMultiRegressFun"/> with respect to the regression parameters <see cref="B"/>, evaluated
      at specific set of (<see cref="X"/>,<see cref="Y"/>) points.

      If DeriveProcedure is not declared (for example if it's nil) <see cref="TMtxMultiNonLinReg"/> will
      use the approximate numeric derivative. In this case the gradient step size is defined by
      GradientStepSize (Optimization unit) global variable. Normally the optimal stepsize depends on
      seventh partial derivatives of the function. Since they (in this case) are not available, the initial
      value for GradientStepSize is <c>Exp(Ln(EPS)/7)*0.25</c> (as suggested by Spellucci).
</remarks>
*)
    property DeriveProcedure: TMultiDeriveProc read FDeriveProcedure write SetDeriveProcedure;
    
    (*<summary>Vector of calculated values.</summary>
      
<remarks>Returns fitted dependent <c>(YCalc=B*X)</c> values. Use YCalc to calculate the residuals:

      <c>Residuals = YCalc - Y.</c>
</remarks>
*)
    property YCalc: TVec read FYCalc;
    (*<summary>Returns the reason why regression parameters calculation stopped.</summary>
      
<remarks>Returns the reason why regression parameters calculation stopped. All stop reasons are
      declared and described in MtxVec Optimization.pas unit.
</remarks>
*)
    property StopReason: TOptStopReason read FStopReason;
    (*<summary>When any of the TMtxNonLinReg properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty: boolean read FDirty write SetDirty;
  published
    
    (*<summary>If true then changing any of the TMtxMultiNonLinReg properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    
    
    (*<summary>Stores the regression coefficients.</summary>
      
<remarks>Stores the regression coefficients. Set B to define initial values for regression
      coefficients. After the <see cref="Recalc"/> call B stores calculated regression coefficients.
</remarks>


      <SeeAlso cref="X"/>
      <SeeAlso cref="Y"/>*)
    property B : TVec read FB write SetB stored AllowStreaming;
    
    (*<summary>Vector of independant variables.</summary>
      
<remarks>Stores independant variables.
</remarks>


      <SeeAlso cref="Y"/>*)
    property X : TVecList read FX write SetX stored AllowStreaming;
    
    (*<summary>Vector of dependant variables.</summary>
      
<remarks>Stores dependant variables.
</remarks>


      <SeeAlso cref="X"/>*)
    property Y : TVec read FY write SetY stored AllowStreaming;
    
    (*<summary>Weights.</summary>
      
<remarks>Stores non-linear regression weights.

      Note
        Weights are used only if <see cref="UseWeights"/> property is set to true.
</remarks>


      <SeeAlso cref="UseWeights"/>*)
    property Weights: TVec read FWeights write SetWeights stored AllowStreaming;
    
    (*<summary>Optimization method used.</summary>
      
<remarks>Defines which optimization method will be used to find
      regression coefficients estimates. Several different optimization algorithms are supported
      (see Dew Math Optimization assembly help for more on this topic):
      * optSimplex : Nelder-Mead optimization method.
      * optMarquardt : Marquardt optimization method.
      * optBFGS : Quasi-Newton optimization method (Broyden-Fletcher-Goldfarb-Shanno Hessian update).
      * optDFP: Quasi-Newton optimization method (Davidson-Fletcher-Power Hessian update).
      * optConjGradFR: Conjugate Gradient optimization method (Fletcher-Reeves).
      * optConjGradPR : Conjugate Gradient optimization method (Polak-Ribiere).
</remarks>
*)
    property OptMethod : TOptMethod read FOptMethod write SetOptMethod default optMarquardt;
    
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIter parameters define the conditions for terminating the regression
      coefficients calculation (number of iterations exceeds MaxIter or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="Tolerance"/>*)
    property MaxIteration : Integer read FMaxIteration write SetMaxIteration default 500;
    
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIter parameters define the conditions for terminating the regression
      coefficients calculation (number of iterations exceeds MaxIter or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="MaxIteration"/>*)
    property Tolerance: double read FTolerance write SetTolerance;
    
    (*<summary>Defines minimal allowed gradient C-Norm.</summary>
      
<remarks>Defines minimal allowed gradient <c>C-Norm</c>. If during the regression parameters calculation
      gradient C-Norm falls bellow GradTolerance then calculation will stop and return
      StopReason = OptResSmallGrad.
</remarks>


      <SeeAlso cref="MaxIteration"/>
      <SeeAlso cref="Tolerance"/>*)
    property GradTolerance : double read FGradTolerance write SetGradTolerance;
    
    (*<summary>Weighted non-linear regression.</summary>
      
<remarks>If this property is true then internal algorithm will perform weighted
      non-linear regression. In this case you must specify the weights (set the <see cref="Weights"/>
      vector).
</remarks>


      <SeeAlso cref="Tolerance"/>*)
    property UseWeights : boolean read FUseWeights write SetUseWeights default false;
    
    (*<summary>Defines line search algorithm for Quasi-Newton and Conjugate methods.</summary>
      
<remarks>If true then Quasi-Newton and Conjugate gradient method internal line search
      algoritm will use soft line search  method. Set <see cref="SoftSearch"/> to true if
      you're using numerical approximation for derivative. If SoftSearch if false, then the internal
      line search algorithm will use exact line search method. Set SoftSearch to false if you're using
      <i>exact</i> derivative procedure.
</remarks>
*)
    property SoftSearch: boolean read FSoftSearch write SetSoftSearch default true;
  end;


  

  

  (*<summary>Performs multiple linear regression.</summary>
    
<remarks>Use TMtxMulLinReg component to perform multiple linear regression on dataset. The TMulLinRegress component fits
    equations to data by minimizing the sum of squared residuals:

    <c>SS = Sum [y(k) - ycalc(k)]^2 ,</c><para/>
    where y(k) and ycalc(k) are respectively the observed and calculated value of the dependent variable for
    observation k. ycalc(k) is a function of the regression parameters b(0), b(1) ... Here the observed values obey
    the following equation:

    <c>y(k) = b(0) + b(1) * x(1,k) + b(2) * x(2,k) + ...</c><para/>
    i.e<para/>
    <c>y = A * b</c><para/>

    <b>How to use TMtxMulLinReg component?</b>
    1. Drop a TMtxMulLinReg component on the form.
    2. Define <see cref="A"/>, the matrix of independent variables.
    3. Define <see cref="Y"/>, vector of dependent variables.
    4. Define <see cref="Weights"/> (optional) vector.
    5. If you don't want to use intercept term b(0) in our calculations,
       set <see cref="Constant"/> property to false,
    6. Call the <see cref="Recalc"/> method to trigger calculation.

    <b>Results:</b>
    1. <see cref="RegressResult"/> : regression coefficients (b),
       regression coefficients confidence intervals (BConfInt) and standard deviation
       (BStdDev), residuals (Residuals) and calculated dependent variables (YCalc).
    2. <see cref="RegressStatistics"/> : basic regression statistics (R2, Adjusted
       R2, residuals variance, F statistics, significance probability).
</remarks>


    

    <example>
      How to setup and run multiple linear regression? In this example
      we've also used the weights. This is done by specifying weights for each variable
      and setting UseWeights property to true.
    <code>
    using Dew.Stats;
    using Dew.Stats.Units;
    namespace Dew.Examples
    {
      private void Example(StatTools.TMtxMulLinReg mlr)
      {
        // y = A*b
        mlr.A.SetIt(3,2,false,new double[] {-5,2,
                                    1,4,
                                    8,0.5});
        mlr.Y.SetIt(false,new double[] {-2,1,11});
        mlr.Weights.SetIt(false, new double[] {2,6,1});
        mlr.UseWeights = true;
        mlr.Recalc();
        // Result ==> b = ( 4.586, 0.871, -1.114 )
      }
    }
    </code></example>*)

  TMtxMulLinReg = class(TMtxComponent)
  strict private
    FY: TVec;
    FA: TMtx;
    FWeights: TVec;
    FConstant: boolean;
    FDirty: boolean;
    FAutoUpdate: boolean;
    FUseWeights: boolean;
    FRegressStatistics: TRegStatsClass;
    IV : TMtx;
    FRegressResult: TRegResultClass;
    FAlpha: double;
    FSolveMethod: TRegSolveMethod;
    procedure SetConstant(const Value: boolean);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetA(const Value: TMtx);
    procedure SetY(const Value: TVec);
    procedure SetWeights(const Value: TVec);
    procedure SetUseWeights(const Value: boolean);
    procedure SetAlpha(const Value: double);
    procedure SetSolveMethod(const Value: TRegSolveMethod);
  strict protected
    
    procedure Loaded; override;
    
  public
    
    class function EditorClass: string; override;
    

    (*<summary>Triggers multiple linear regression recalculation.</summary>
      
<remarks>Triggers multiple linear regression (MLR) recalculation. After the Recalc call <see cref="Dirty"/> property is set to false.

      The results of MLR are stored to:
     * <see cref="RegressResult"/> : regression coefficients (b),
        regression coefficients confidence intervals (BConfInt) and standard deviation
        (BStdDev), residuals (Residuals) and calculated dependent variables (YCalc).
      * <see cref="RegressStatistics"/> : basic regression statistics (R2, Adjusted
        R2, residuals variance, F statistics, significance probability).
</remarks>


      <SeeAlso cref="RegressResult"/>
      <SeeAlso cref="RegressStatistics"/>*)
    procedure Recalc;
    (*<summary>Checks if system is valid.</summary>
      
<remarks>Checks if system is valid.
</remarks>

      <returns>True if <see cref="A"/> is not zero matrix and <see cref="A"/>.Rows
      (number of observables) is equal to <see cref="Y"/>.Length.</returns>

      

      <example>
        Checks if defined system is valid:
      <code>
        // ...
        // y = A*b
        MtxMulLinReg1.A.SetIt(3,2,false,new double[]
                        {-5,2, 1,4, 8,0.5});
        MtxMulLinReg1.Y.SetIt(false,new double[] {-2, 1,11});
        MtxMulLinReg1.UseWeights = false;
        if (MtxMulLinReg1.ValidSystem()) MtxMulLinReg1.Recalc();
        // ...
      </code></example>

      <SeeAlso cref="A"/>
      <SeeAlso cref="Y"/>*)
    function ValidSystem: boolean;
    
    (*<summary>Regression results statistics.</summary>
      
<remarks>Returns regression coefficients statistics parameters: adjusted coefficient of determination,
      variance ratio (explained/residual ratio), coefficient of determination (R squared),
      residual variance and the probability of F.
</remarks>


      <SeeAlso cref="RegressResult"/>*)
    property RegressStatistics: TRegStatsClass read FRegressStatistics;
    (*<summary>When any of the TMtxMulLinReg properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty : boolean read FDirty write SetDirty;
    (*<summary>Regression results.</summary>
      
<remarks>Returns regression coefficients, their confidence intervals and standard deviations,
      residuals and predicted dependant variable values.
</remarks>


      <SeeAlso cref="RegressStatistics"/>*)
    property RegressResult: TRegResultClass read FRegressResult;
    
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    
  published
    
    (*<summary>If true then changing any of the TMtxMulLinReg properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    (*<summary>Desired significant value for the statistical tests.</summary>
      
<remarks>Defines the Alpha level for the statistical tests.
</remarks>


      <SeeAlso cref="RegressStatistics"/>*)
    
    property Alpha: double read FAlpha write SetAlpha;
    (*<summary>Matrix of independent variables.</summary>
      
<remarks>Matrix of independent variables. Number of A rows is equal to number of
      observables and number of A columns is equal to number of variables.

      You can use <see cref="ValidSystem"/> method to check whether defined
      system of equations (A.Rows = Y.Length i.e. number of observables is
      the same for A and Y) has been properly defined.
</remarks>


      <SeeAlso cref="ValidSystem"/>*)
    
    property A : TMtx read FA write SetA stored AllowStreaming;
    (*<summary>Vector of dependent variables.</summary>*)
    
    property Y : TVec read FY write SetY stored AllowStreaming;
    (*<summary>Weights for MLR.</summary>
      
<remarks>Defines weights for multiple linear regression (MLR).
      Weights are used only if <see cref="UseWeights"/> property is
      set to true.
</remarks>


      <SeeAlso cref="UseWeights"/>*)
    
    property Weights : TVec read FWeights write SetWeights stored AllowStreaming;
    (*<summary>Intercept term.</summary>
      
<remarks>Defines if intercept term is used. If true then all calculations will
      include intercept term b(0). If false then intercept term b(0) is set to 0.0.
</remarks>


      <SeeAlso cref="A"/>
      <SeeAlso cref="RegressResult"/>*)
    
    property Constant : boolean read FConstant write SetConstant default true;
    (*<summary>Weighted multiple linear regression.</summary>
      
<remarks>If true the internal algorithm will perform weighted multiple linear regression.
      In this case you must specify the weights (set the <see cref="Weights"/> vector).
</remarks>


      <SeeAlso cref="Weights"/>*)
    
    property UseWeights : boolean read FUseWeights write SetUseWeights default false;
    (*<summary>Defines A*x=b solving method.</summary>*)
    
    property SolveMethod: TRegSolveMethod read FSolveMethod write SetSolveMethod default regSolveLQR;
  end;


  

  

  (*<summary>Performs Principle Component Analysis (PCA).</summary>
    
<remarks>Principal Components Analysis - PCA - is a data analysis tool that is usually used to reduce the dimensionality
    (number of variables) of a large number of interrelated variables, while retaining as much of the information (variation)
    as possible. PCA calculates an uncorrelated set of variables (factors or PCs). These factors are ordered so
    that the first few retain most of the variation present in all of the original variables.  <para/>

    The PCA procedure is reduced to an eigenvalue-eigenvector problem. PCA routines perform a PCA on either a
    correlation or a covariance matrix. Data matrix can be either "raw" data or pre-calculated
    correlation/covariance matrix.<para/>

    <b>How to use TMtxPCA component?</b>
    <list type="number">
    <item> Drop a TMtxPCA component on the form. </item>
    <item> By setting the <see cref="PCAMode"/> property define whether PCA will use correlation or covariance matrix to calculate PCA.</item>
    <item> Define the actual <see cref="Data"/> (by changing Data matrix values).</item>
    <item> Call the <see cref="Recalc"/> method to calculate PCA results. </item>
    </list>

    <b>Results:</b>
    <list type="number">
    <item> <see cref="EigValues"/> - Data eigenvalues </item>
    <item> <see cref="PC"/> - Data eigenvectors </item>
    <item> <see cref="TotalVarPct"/> - the percentage of the total variation in the variables (columns) </item>
    <item> <see cref="ZScores"/> - Z-Scores (eigenvectors in PC space). </item>
    </list>
</remarks>


    

    <example>
      An example how to setup TMtxPCA component:
    <code>
    using Dew.Stats;
    using Dew.Stats.Units;
    using Dew.Math;
    namespace Dew.Examples
    {
      private void Example(StatTools.TMtxPCA MtxPCA1)
      {
        // ...
        MtxPCA1.Data.SetIt(4,3,false,new double[]
                                {1,2,3,
                                5,7,9,
                                1,11,13,
                                3,7,4});
        MtxPCA1.PCAMode = TPCAMode.PCARawData; // using data matrix to evaluate PCA
        MtxPCA1.Recalc(); // force recalculation
        // ...
      }
    }
    </code></example>*)

  TMtxPCA = class(TMtxComponent)
  strict private
    FZScores: TMtx;
    FData: TMtx;
    FPC: TMtx;
    FPCAMode: TPCAMode;
    FTotalVarPct: TVec;
    FEigValues: TVec;
    FDirty: boolean;
    FAutoUpdate: boolean;
    tmpMtx: TMtx;
    procedure SetData(const Value: TMtx);
    procedure SetPCAMode(const Value: TPCAMode);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetDirty(const Value: boolean);
  strict protected
    
    procedure Loaded; override;
    
  public
    (*<summary>Triggers PCA recalculation.</summary>
      
<remarks>Triggers PCA recalculation. After the Recalc call <see cref="Dirty"/> property is set to false. The results of PCA are stored to:
      <list type="bullet">
      <item> <see cref="EigValues"/> - Data eigenvalues.</item>
      <item> <see cref="PC"/> - Data eigenvectors.</item>
      <item> <see cref="TotalVarPct"/> - the percentage of the total variation in the variables (columns).</item>
      <item> <see cref="ZScores"/> - Z-Scores (eigenvectors in PC space). </item>
      </list>
</remarks>
*)
    procedure Recalc;
    
    (*<summary>Returns the principal components (PC).</summary>
      <SeeAlso cref="ZScores"/>*)
    property PC :TMtx read FPC;
    (*<summary>Returns the Z-Scores.</summary>
      
<remarks>Returns the so called "Z-scores" (data, transformed in the PC space).
</remarks>


      <SeeAlso cref="PC"/>*)
    property ZScores: TMtx read FZScores;
    (*<summary>Returns the eigenvalues.</summary>
      
<remarks>Returns the <see cref="Data"/> matrix eigenvalues. Eigenvalues can
      be used to determine how many factors to retain. When the PCA is run on
      the correlations, one rule-of-thumb is to retain those factors whose eigenvalues
      are greater than one. The sum of the eigenvalues is equal to the number of
      variables. When the PCA is run on the covariances, the sum of the eigenvalues is
      equal to the sum of the variances of the variables.
</remarks>


      <SeeAlso cref="PC"/>
      <SeeAlso cref="TotalVarPct"/>*)
    property EigValues : TVec read FEigValues;
    (*<summary>Returns the percentage of the total variation in the variables.</summary>
      
<remarks>Returns the percentage of the total variation in the variables (columns).
</remarks>


      <SeeAlso cref="EigValues"/>*)
    property TotalVarPct: TVec read FTotalVarPct;
    (*<summary>When any of the TMtxPCA properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty : boolean read FDirty write SetDirty;
    
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    
  published
    
    (*<summary>If true then changing any of the TMtxPCA properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;

    (*<summary>Defines the data to be analyzed by PCA.</summary>
      
<remarks>Defines the data to be analyzed by PCA. Depending on <see cref="PCAMode"/> property, Data can be
      interpreted as raw data or precalculated data correlation/covariance matrix. Generally, Data columns
      and model variables and Data rows are model observables.
</remarks>


      <SeeAlso cref="PCAMode"/>*)
    
    property Data : TMtx read FData write SetData stored AllowStreaming;
    (*<summary>PCA type.</summary>
      
<remarks>Defines type of PCA. Normally, the analysis is run on the scale-invariant correlation matrix since
      the scale of the variables changes the analysis when the covariance matrix is used. For example,
      when a covariance matrix was used, a variable that was measured in Celsius deg. results in a
      different analysis than if it were measured in Kelvin deg.
</remarks>
*)
    
    property PCAMode: TPCAMode read FPCAMode write SetPCAMode default PCACorrMat;
  end;

  

  

  (*<summary>Performs logistic regression.</summary>
    
<remarks>Use TMtxLogistReg component to perform ordinary or ordinal logistic regression on dataset.Suppose y takes values in k ordered categories, and let  p_ij
    be the cumulative probability that  y(i)  falls in the j'th category
    or higher.  The ordinal logistic regression model is defined as:<para/>

    <c>logit(p_ij) = theta(j) + A_i'B , i = 1,..,length(Y), j = 1,..,k-1,</c> <para/>

    where  A_i  is the i'th row of  A . The number of ordinal categories k is taken to be the number of distinct values of int(y).
    If k is 2 (two categories) the model is ordinary logistic regression. <para/>

    <b>How to use TMtxLogistReg component?</b>
    <list type="number">
    <item> Drop a TMtxLogistReg component on the form. </item>
    <item> Define <see cref="A"/>, the matrix of independent variables. </item>
    <item> Define <see cref="Y"/>, grouping (categories) vector.
       Note: All values have to be integers. </item>
    <item> Define initial estimates for <see cref="Theta"/> and <see cref="B"/>
       coefficients. Alternatively you can also set <see cref="AutoInitEstimates"/> to true
       and let the algorithm calculate initial estimates for Theta and B. </item>
    <item> (Optionally) Set <see cref="Tolerance"/> to define desired B, Theta estimates precision. Set
        <see cref="MaxIteration"/> to define maximum number of iterations in main calculation loop. </item>
    <item> Call the <see cref="Recalc"/> method to trigger calculation. </item>
    </list>

    <b>Results:</b>
    <list type="number">
    <item> <see cref="B"/> : B coefficients estimates. </item>
    <item> <see cref="Theta"/> : Theta coefficients estimates. </item>
    <item> <see cref="TBStdErr"/> : Theta and B coefficients estimates standard errors. </item>
    </list>
</remarks>


    

    <example>
      How to setup and run logistic regression? In the following example we'll perform
      ordinary (i.e. with two categories) logistic regression on data, taken from NCSS statistics program.
      Note that TMtxLogistReg.Y vector values are binary integer values.
    <code>
    using Dew.Stats;
    using Dew.Stats.Units;
    using Dew.Math;
    namespace Dew.Examples
    {
      private void Example(StatTools.TMtxLogistReg tc)
      {
        tc.A.SetIt(27,3,false, new double[]
          {0.8, 1.9, 0.996,
          0.9, 1.4, 0.992,
          0.8, 0.8, 0.982,
          1, 0.7, 0.986,
          0.9,	1.3, 0.98,
          1,	0.6, 0.982,
          0.95,	1,	0.992,
          0.95,	1.9,	1.02,
          1,	0.8,	0.999,
          0.95,	0.5, 1.038,
          0.85, 0.7, 0.988,
          0.7,	1.2, 0.982,
          0.8,	0.4, 1.006,
          0.2,	0.8, 0.99,
          1,	1.1, 0.99,
          1,	1.9, 1.02,
          0.65, 0.5,	1.014,
          1,	1,	1.004,
          0.5,	0.6, 0.99,
          1,	1.1, 0.986,
          1,	0.4,	1.01,
          0.9,	0.6, 1.02,
          1,	1,	1.002,
          0.95, 1.6, 0.988,
          1,	1.7, 0.99,
          1,	0.9, 0.986,
          1,	0.7, 0.986});;
        tc.Y.SetIt(false,new double[] {1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0});
        tc.Recalc();
        // Results =>
        // B = (9.65215222458842, 3.86710032907408, -82.073774279211)
        // Theta = (-67.6339061278272)
        // TBStdErr = (56.8875416435276, 7.75107606604495, 1.77827769017976, 61.712376172072)
        // meaning Theta StdErr = 56.8875416435276,
        //         Beta StdErrs = 7.75107606604495, 1.77827769017976, 61.712376172072
      }
    }
    </code></example>*)

  TMtxLogistReg = class(TMtxComponent)
  strict private
    FDirty: boolean;
    FAutoUpdate: boolean;
    FAutoInitEstimates: boolean;
    FMaxIteration: Integer;
    FTolerance: double;
    FA: TMtx;
    FB: TVec;
    FY: TVec;
    FTheta: TVec;
    FTBStdErr: TVec;
    FAlpha: double;
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoInitEstimates(const Value: boolean);
    procedure SetMaxIteration(const Value: Integer);
    procedure SetTolerance(const Value: double);
    procedure SetA(const Value: TMtx);
    procedure SetB(const Value: TVec);
    procedure SetTheta(const Value: TVec);
    procedure SetY(const Value: TVec);
    procedure SetAlpha(const Value: double);
  strict protected
    
    procedure Loaded; override;
    
  public
    Iterations: Integer;
    FMin: double;
    StopReason: TOptStopReason;
    (*<summary>When any of the TMtxLogistReg properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty: boolean read FDirty write SetDirty;
    (*<summary>Standard errors of Theta, B estimates.</summary>
      
<remarks>Returns Theta and B estimates standard errors.
</remarks>


      <SeeAlso cref="B"/>
      <SeeAlso cref="Theta"/>*)
    property TBStdErr: TVec read FTBStdErr;
    (*<summary>Triggers logistic regression recalculation.</summary>
      
<remarks>Triggers logistic regression (ordinary or ordinal) recalculation. After the Recalc call <see cref="Dirty"/> property is set to false.
      The results of logistic regression are stored to:

      * <see cref="B"/> : B coefficients estimates.
      * <see cref="Theta"/> : Theta coefficients estimates. For ordinary regression (two categories),
        Theta is single value vector.
      * <see cref="TBStdErr"/> : Theta and B coefficients estimates standard errors.
</remarks>
*)
    procedure Recalc;

    
    constructor Create(AOwner: TComponent); override;
    
    Destructor Destroy; override;
    
  published
    (*<summary>Desired significant value for the statistical tests.</summary>
      
<remarks>Defines the Alpha level for the statistical tests.
</remarks>
*)
    
    property Alpha: double read FAlpha write SetAlpha;
    (*<summary>If true then changing any of the TMtxLogistReg properties will trigger the
      <see cref="Recalc"/> method.</summary>

      <SeeAlso cref="Dirty"/>*)
    
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate default false;
    (*<summary>Automatically calculate initial estimates for regression coefficients.</summary>

      <SeeAlso cref="B"/>
      <SeeAlso cref="Theta"/>*)
    
    property AutoInitEstimates: boolean read FAutoInitEstimates write SetAutoInitEstimates default true;
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIteration parameters define the conditions for terminating logistic regression
      coefficients calculation (number of iterations exceeds MaxIteration or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="Tolerance"/>*)
    
    property MaxIteration: Integer read FMaxIteration write SetMaxIteration default 100;
    (*<summary>Defines one of the conditions for terminating the regression coefficient calculation.</summary>
      
<remarks>Tolerance and MaxIter parameters define the conditions for terminating the regression
      coefficients calculation (number of iterations exceeds MaxIter or internal minimum
      precision is within Tolerance).
</remarks>


      <SeeAlso cref="MaxIteration"/>*)
    
    property Tolerance: double read FTolerance write SetTolerance;
    (*<summary>Grouping variable.</summary>
      
<remarks>Defines the grouping,or classification, variable. It's values can be only integers. The internal
      algorithm will use smallest value for first group, next for second group ..., and largest integer value for last
      group. Values can be binaries in which case the smallest value will be the first group and the largest value
      will be the second group.
</remarks>
*)
    
    property Y: TVec read FY write SetY stored AllowStreaming;
    (*<summary>B coefficients estimates.</summary>
      
<remarks>Defines the logistic regression B coefficients. Suppose y takes values in k ordered categories, and let  p_ij
      be the cumulative probability that  y(i)  falls in the j'th category
	    or higher. Ordinal logistic regression model is defined as:

      <c>logit(p_ij) = theta(j) + A_i'B , i = 1,..,length(Y), j = 1,..,k-1,</c>

	    where  A_i  is the i'th row of  A . The number of ordinal categories k is taken to be the number of distinct values of int(y).
      If k is 2 the model is ordinary logistic regression.

      Set B values to define initial estimates for B coefficients. Setting B values is not mandatory. You can
      also set <see cref="AutoInitEstimates"/> to true and force the algorithm to calculate initial
      estimates for <see cref="B"/> and <see cref="Theta"/>. After the <see cref="Recalc"/> call B stores logistic regression model B coefficient estimates.
</remarks>


      <SeeAlso cref="AutoInitEstimates"/>
      <SeeAlso cref="Theta"/>*)
    
    property B: TVec read FB write SetB stored AllowStreaming;
    (*<summary>Theta coefficients estimates.</summary>
      
<remarks>Set Theta values to define initial estimates for Theta coefficients. Setting Theta values is not mandatory. You can
      also set <see cref="AutoInitEstimates"/> to true and force the algorithm to calculate initiate
      estimates for <see cref="B"/> and Theta. After the <see cref="Recalc"/> call Theta stores logistic regression model Theta coefficient estimates.
</remarks>


      <SeeAlso cref="AutoInitEstimates"/>
      <SeeAlso cref="B"/>*)
    
    property Theta: TVec read FTheta write SetTheta stored AllowStreaming;
    (*<summary>Independent variables.</summary>
      
<remarks>Defines logistic regression matrix of independent variables.
      Suppose y takes values in k ordered categories, and let  p_ij
      be the cumulative probability that  y(i)  falls in the j'th category
	    or higher. Ordinal logistic regression model is defined as:

      <c>logit(p_ij) = theta(j) + A_i'B , i = 1,..,length(Y), j = 1,..,k-1,</c>
	    where  A_i  is the i'th row of  A .  The number of ordinal categories k is taken to be the number of distinct values of int(y).
      If k is 2 the model is ordinary logistic regression.
</remarks>


      <SeeAlso cref="Y"/>
      <SeeAlso cref="B"/>
      <SeeAlso cref="Theta"/>*)
    
    property A: TMtx read FA write SetA stored AllowStreaming;
  end;

  (*<summary>Defines type of binary test.</summary>*)
  TBinaryTestType = (
    (*<summary>one-sample binary test.</summary>*)btOneSample,
    (*<summary>two-sample binary test.</summary>*)btTwoSample
  );

  
  (*<summary>Defines binary test table structure.</summary>
    
<remarks>Defines 2x2 binary test table structure. An important task in diagnostic is to measure the
    accuracy of a diagnostic test. This can be done by comparing the test result with the true
    condition status of a number of patients. The results of such a study can be displayed in a 2-by-2
    table in which the true condition is shown as the rows and the diagnostic test result is shown
    as the columns.

    <table>
    Class            Diagnostic  Test      Results
    ---------------  ----------  --------  -------
    True condition   Positive    Negative  Total
    Present(true)    T1          T0        N1
    Absent (false)   F1          F0        N0
    Total            M1          M0        N
    </table>
</remarks>
*)
  TBinaryTestTable = class(TPersistent)
  strict private
    FF0: Integer;
    FF1: Integer;
    FT0: Integer;
    FT1: Integer;
    procedure SetF0(const Value: Integer);
    procedure SetF1(const Value: Integer);
    procedure SetT0(const Value: Integer);
    procedure SetT1(const Value: Integer);
    function GetN1: Integer;
    function GetN0: Integer;
    function GetM1: Integer;
    function GetM0: Integer;
    function GetN: Integer;
    function GetSe1: double;
    function GetSe2: double;
    function GetSp1: double;
    function GetSp2: double;
    procedure CalcCI(s: double; n: Integer; var CI: TTwoElmReal);
  protected
    
    FPaired: Boolean;
    FAlpha: double;
    
  public
    
    procedure Assign(Source: TPersistent); override;
    
    
    (*<summary>Returns first row total (T1+T0).</summary>*)
    property N1: Integer read GetN1;
    
    (*<summary>Returns second row total (F1+F0).</summary>*)
    property N0: Integer read GetN0;
    
    (*<summary>Returns first column total (T1+F1).</summary>*)
    property M1: Integer read GetM1;
    
    (*<summary>Returns second column total (T0+F0).</summary>*)
    property M0: Integer read GetM0;
    
    (*<summary>Returns table grand total (T1+T0+F1+F0).</summary>*)
    property N: Integer read GetN;
    
    (*<summary>Returns test sensitivity.</summary>
      
<remarks>Returns sensitiviy, calculated from table values. For one and two sample
      unpaired binary test, sensitivity is defines as:

      <c>Se = T1/N1.</c><para/>
      For two-sample paired test, sensitivity is defines as:

      <c>Se = N1/N .</c>
</remarks>
*)
    property Se1: double read GetSe1;
    
    (*<summary>Returns test sensitivity.</summary>
      
<remarks>Returns sensitiviy, calculated from table values. For two sample
      unpaired binary test, sensitivity is defines as:

      <c>Se = T1/N1.</c><para/>
      For two-sample paired test, sensitivity is defines as:

      <c>Se = M1/N .</c>
</remarks>
*)
    property Se2: double read GetSe2;
    
    (*<summary>Returns specificity.</summary>
      
<remarks>Returns specificity, defined as:

      <c>Sp = F0/N0.</c>

      Note
        This value is valid only for unpaired test.
</remarks>
*)
    property Sp1: double read GetSp1;
    
    (*<summary>Returns specificity.</summary>
      
<remarks>Returns specificity, defined as:

      <c>Sp = F0/M0 .</c>

      Note
        This value is valid only for unpaired test.
</remarks>
*)
    property Sp2: double read GetSp2;
    (*<summary>Returns <see cref="Se1"/> confidence interval.</summary>*)
    procedure GetSe1CI(out Bottom, Top: double);
    (*<summary>Returns <see cref="Se2"/> confidence interval.</summary>*)
    procedure GetSe2CI(out Bottom, Top: double);
    (*<summary>Returns <see cref="Sp1"/> confidence interval.</summary>*)
    procedure GetSp1CI(out Bottom, Top: double);
    (*<summary>Returns <see cref="Sp2"/> confidence interval.</summary>*)
    procedure GetSp2CI(out Bottom, Top: double);
  published
    
    (*<summary>Defines number of true positive values.</summary>*)
    property T1: Integer read FT1 write SetT1;
    
    (*<summary>Defines number of true negative values.</summary>*)
    property T0: Integer read FT0 write SetT0;
    
    (*<summary>Defines number of false positive values.</summary>*)
    property F1: Integer read FF1 write SetF1;
    
    (*<summary>Defines number of false negative values.</summary>*)
   property F0: Integer read FF0 write SetF0;
  end;

  

  

  (*<summary>Performs one and two test paired or unpaired binary test.</summary>
    
<remarks>The component supports the following types of binary diagonstic tests:
    * One-sample test where the results can be arranged in single 2x2 table,
    * two-sample test where the results can be arranged in two 2x2 tables,
    * two-sample paired test where the results can be combined and arranged in single 2x2 table.

    <b>How to use TMtxBinaryTest component?</b>
    * Drop a TMtxBinaryTest component on the form.
    * Set <see cref="Paired"/> to true if you're performing paired binary diagnostic test,
      otherwise set it to false.
    * Define the Sensitivity and Specificity confidence level by setting the <see cref="CIAlpha"/>
      property.
    * Define binary test type (one, two test) by setting <see cref="TestType"/> property.
    * Define binary diagnostic test values by setting <see cref="Test1Table"/> and (only if you
      use two-sample test) <see cref="Test2Table"/> table values. See <see cref="TBinaryTestTable"/>
      class for more details.

    <b>Results:</b>
    Depending on test type, component returns the following results:
    1. One-test : The <see cref="Test1Table"/> returns test row totals, column totals, sensitivity and
       its CIAlpha confidence interval, specificity and it's CIAlpha confidence interval.
    2. Two-test unpaired: The <see cref="Test1Table"/> returns first test row and column totals, first test sensitivity
       and specificity, the <see cref="Test2Table"/> returns second test row and column totals, second test        sensitivity and specificity.
    3. Two-test paired : The <see cref="Test1Table"/> returns first and second test sensitivity i.e <see cref="TBinaryTestTable.Se1"/> and
       <see cref="TBinaryTestTable.Se2"/> and their CIAlpha confidence intervals.
</remarks>
*)

  TMtxBinaryTest = class(TMtxComponent)
  strict private
    FTestType: TBinaryTestType;
    FTest2Table: TBinaryTestTable;
    FTest1Table: TBinaryTestTable;
    FCIAlpha: double;
    FDirty: boolean;
    FAutoUpdate: boolean;
    FPaired: boolean;
    procedure SetTestType(const Value: TBinaryTestType);
    procedure SetTest1Table(const Value: TBinaryTestTable);
    procedure SetTest2Table(const Value: TBinaryTestTable);
    procedure SetCIAlpha(const Value: double);
    procedure SetDirty(const Value: boolean);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetPaired(const Value: boolean);
  public
    (*<summary>Recalculates test resutlts.</summary>*)
    procedure Recalc;
    (*<summary>When any of the TMtxBinaryTest properties changes,
      this property is automatically set to true.</summary>*)
    property Dirty: boolean read FDirty write SetDirty;
    
    constructor Create(AOwner: TComponent); override;
    
    Destructor Destroy; override;
    
  published
    
    (*<summary>If true then the table is paired.</summary>*)
    property Paired: boolean read FPaired write SetPaired default false;
    
    (*<summary>If true, any change will trigger the recalculation.</summary>*)
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    
    (*<summary>Defines Se and SP desired confidence level.</summary>*)
    property CIAlpha: double read FCIAlpha write SetCIAlpha;
    
    (*<summary><see cref="TBinaryTestType"/> type.</summary>*)
    property TestType: TBinaryTestType read FTestType write SetTestType default btOneSample;
    
    (*<summary>First test table.</summary>*)
    property Test1Table: TBinaryTestTable read FTest1Table write SetTest1Table;
    
    (*<summary>Second test (optional) table.</summary>*)
    property Test2Table: TBinaryTestTable read FTest2Table write SetTest2Table;
  end;

  (*<summary>Defines type of multidimensional scaling.</summary>*)
  TMDScalingMethod = (
    (*<summary>Classical (metric) multidimensional scaling algorithm.</summary>*)mdScalingMetric,
    (*<summary>Non-metric multidimensional scaling algorithm.</summary>*)mdScalingNonMetric
  );

  (*<summary>MD matrix data type.</summary>*)
  TMDDataFormat = (
  (*<summary>Dissimilarities represent the distance between two objects.
    They may be measured directly or approximated. MDS algorithms use the dissimilarities directly.
    A dissimilarity matrix is always symmetrical and with zero main diagonal.</summary>*)
  mdFormatDissimilarities,
  (*<summary>Similarities represent how close (in some sense) two objects are.
    Similarities must obey the rule:

    <c>similarity(i,j) &lt;= similarity(i,i) and similarity(j,j)</c><para/>
    for all i and j. Similarity matrices are symmetrical. Similarities are converted to dissimilarities
    by the following relation:

    <c>d(i,j) = Sqrt[s(i,i) + s(j,j) -2*s(i,j)]</c><para/>
    where  d(i,j) represents a dissimilarity and  s(i,j) represents a similarity.
    In case your data consists of standard measures rather than dissimilarities or similarities, you can
    create a similarity matrix by creating the correlation matrix.</summary>*)
  mdFormatSimilarities,
  (*<summary>In this case, dissimilarities matrix is calculated by using <see cref="PairwiseDistance"/> routine.</summary>*)
  mdFormatRaw
  );

  

  

  (*<summary>Performs multidimensional scaling.</summary>
    
<remarks>Use TMtxMDScaling component to perform multidimensional scaling on dataset.

    <b>How to use TMtxMDScaling component?</b>
    1. Drop a TMtxMDScaling component on the form.
    2. Set <see cref="DataFormat"/> property to define how data will be interpreted.
       Data can be interpreted as dissimilarities matrix, similarities matrix or raw data matrix.
    3. Define <see cref="Data"/>, the matrix representing dissimilarities matrix, similarities matrix
       or raw data.
    4. Set <see cref="Dimensions"/> to define number of variables in reduced space.
    5. Set <see cref="DistanceMethod"/> to define how pairwise distance will be calculated.
    6. Set <see cref="ScalingMethod"/> to define scaling algorithm. Currently only metric (classical)
       multidimensional scaling algorithm is supported.
    7. Call the <see cref="Recalc"/> method to trigger calculation.

    <b>Results:</b>
    1. <see cref="Y"/> : Point coordinates in reduced space.
    2. <see cref="EigenValues"/> : Eigenvalues in reduced space, sorted in descending order.
    3. <see cref="DHat"/> : Estimated dissimilarities matrix.
    4. <see cref="Stress"/> : Calculated stress factor.
</remarks>


    

    <example>
      The "beauty" of MDS is that we can analyze any kind of distance or similarity matrix. These similarities can represent people's ratings of
      similarities between objects, the percent agreement between judges, the number of times a subjects fails to discriminate between stimuli, etc.
      For example, MDS methods used to be very popular in psychological research on person perception where similarities between trait descriptors were
      analyzed to uncover the underlying dimensionality of people's perceptions of traits (see, for example Rosenberg, 1977).
      In this example 6x6 similarities (extracted directly from questionare correlation matrix) is used to perform classical MD scaling.
    <code>
    using Dew.Stats;
    using Dew.Stats.Units;
    using Dew.Math;
    namespace Dew.Examples
    {
      private void Example(StatTools.TMtxMDScaling mds)
      {
        // similarities matrix (symmetric with 1.0 on diagonal)
        mds.Data.SetIt(5,5,false, new double[] {
             1.00, 0.3, 0.2, 0.25, 0.33,
            0.30, 1.0, 0.11, 0.21, 0.8,
            0.20, 0.11, 1.0, 0.40, 0.5,
            0.25, 0.21, 1.0, 0.10, 0.05,
            0.33, 0.80, 0.5, 0.05, 1.00});
        mds.DataFormat = TMDDataFormat.mdFormatSimilarities;
        // use "standard" Euclidian metric
        mds.DistanceMethod = TPWDistMethod.pwdistEuclidian;
        // define number of desired dimensions (1)
        mds.Dimensions = 1;
        // Do the math
        mds.Recalc();
        // check Stress, DHat, EigeValues to evaluate GOF if (1) dimension is used
      }
    }
    </code></example>*)

  TMtxMDScaling = class(TMtxComponent)
  strict private
    FDirty: boolean;
    FScalingMethod: TMDScalingMethod;
    FAutoUpdate: boolean;
    FDataFormat: TMDDataFormat;
    FData: TMtx;
    FY: TMtx;
    FDHat: TMtx;
    FEigenValues: TVec;
    FinternalD: TMtx;
    FStress: double;
    FDimensions: Integer;
    FDistanceMethod: TPWDistMethod;
    procedure Similarity2Dissimilarity(S,D: TMtx);
    procedure SetDirty(const Value: boolean);
    procedure SetScalingMethod(const Value: TMDScalingMethod);
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetDataFormat(const Value: TMDDataFormat);
    procedure SetData(const Value: TMtx);
    procedure SetDimensions(const Value: Integer);
    procedure SetDistanceMethod(const Value: TPWDistMethod);
    procedure SetDHat(const Value: TMtx);
  public
    (*<summary>Triggers MDScalling recalculation.</summary>
      
<remarks>Triggers non-linear regression recalculation. After the Recalc call
      <see cref="Dirty"/> property is set to false.
</remarks>
*)
    procedure Recalc;
    (*<summary>When any of the TMtxMDScaling properties changes, this property is automatically set to true.</summary>
      
<remarks>If <see cref="AutoUpdate"/> and Dirty are both true then the <see cref="Recalc"/>
      method will be called automatically.
</remarks>


      <SeeAlso cref="AutoUpdate"/>
      <SeeAlso cref="Recalc"/>*)
    property Dirty: boolean read FDirty write SetDirty;
    (*<summary>Reduced space point coordinates.</summary>*)
    property Y: TMtx read FY;
    (*<summary>Reduced space eigenvalues.</summary>
      
<remarks>Returns reduced space eigenvalues, sorted in descending order.
</remarks>
*)
    property EigenValues: TVec read FEigenValues;
    (*<summary>Returns multidimensional scaling stress value which is an estimate for GOF.</summary>*)
    property Stress: double read FStress;
    (*<summary>Estimated dissimilarities matrix.</summary>
      
<remarks>Returns estimated (reduced dimension) dissimilarities matrix.
      Note
        Define DHat values ONLY if <see cref="DistanceMethod"/> property is set to pwdistCustom.
        If this is not the case, DHat values will be ignored and the internal algorithm will be used
        to calculate pairwise distances.
</remarks>


      <SeeAlso cref="DistanceMethod"/>
      <SeeAlso cref="Stress"/>*)
    property DHat: TMtx read FDHat write SetDHat;
    (*<summary>Used dissimilarities matrix.</summary>
      
<remarks>Returns used dissimilarity matrix. If <see cref="DataFormat"/> is set to dissimilarity
      matrix, it returns <see cref="Data"/> matrix, otherwise it returns calculated or user-supplied
      dissimilarity matrix.
</remarks>


      <SeeAlso cref="DataFormat"/>
      <SeeAlso cref="DistanceMethod"/>*)
    property D: TMtx read FinternalD;
    
    constructor Create(AOwner: TComponent); override;
    
    Destructor Destroy; override;
    
  published
    
    
    (*<summary>If true then changing any of the TMtxPCA properties will trigger the
      <see cref="Recalc"/> method.</summary>
      <SeeAlso cref="Dirty"/>*)
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate default false;
    
    (*<summary>Defines multidimensional scaling algorithm.</summary>
      
<remarks>Note
        Currently only classical multidimensional scaling algorithm is supported.
</remarks>
*)
    property ScalingMethod: TMDScalingMethod read FScalingMethod write SetScalingMethod default mdScalingMetric;
    (*<summary>Defines data format used in calculations.</summary>*)
    
    (*<summary>Data format.</summary>*)
    property DataFormat: TMDDataFormat read FDataFormat write SetDataFormat default mdFormatDissimilarities;
    
    (*<summary>Data to be analyzed by MDS.</summary>*)
    property Data: TMtx read FData write SetData stored AllowStreaming;
    
    (*<summary>Reduced dimension size.</summary>*)
    property Dimensions: Integer read FDimensions write SetDimensions default 1;
    
    (*<summary>Metric for distance calculation.</summary>*)
    property DistanceMethod: TPWDistMethod read FDistanceMethod write SetDistanceMethod default pwdistEuclidian;
  end;

  (*<summary>Stepwise regression variable action.</summary>*)
  TStepwiseAction = (swaUnchanged, swaRemoved, swaAdded);

  (*<summary>Callback function for custom quality criteria of stepwise regression optimization algorithm. </summary>
              
<remarks>The Stat contains the details of the regression results, b contains the coefficients and t the corresponding t-Values for each item in b.
              The ParamMask.Length equals the initial total variable count. b and t can and will have length less than this.
              Which variables are included is determined from ParamMask, which has 1 at corresponding included variable index and 0
              otherewise.
</remarks>
*)
  TOnStepwiseQualityCriteria = function(const Stat: TRegStats; const b, t: TVec; const ParamMask: TVecInt): double of object;

  

  

  (*<summary>Stepwise regression is an optimization aglorithm aiming to improve the quality of the multiple linear regression
              by excluding noisy variables. </summary>
              
<remarks>For models with less than 15 variables, the exhaustive search is the recommended
              method. Alternatively it is possible to perform "backward search" by starting with all and removing one by one or
              "forward search" by starting with none and adding one by one variable. Both backward and forward search can have
              selected variables already pre-included (or pre-excluded). Single step mode allows the user to manually include or
              exclude individual variables from the model after each step.
</remarks>
*)

  TMtxStepwiseReg = class(TMtxComponent)
  strict private
    FMethod: TStepwiseMethod;
    FA: TVecList;
    FBitMask: TVecInt;
    FReportCoeff: TMtx;
    FReportSSE: TMtx;
    FMaxIter: integer;
    FAutoInitBitMask: boolean;
    FOnQualityCriteria: TOnStepwiseQualityCriteria;
    procedure SetMethod(const Value: TStepwiseMethod);
    procedure SetA(const Value: TVecList);

    procedure SetBitMask(const Value: TVecInt);
    procedure SetReportCoeff(const Value: TMtx);
    procedure SetReportSSE(const Value: TMtx);
    procedure SetMaxIter(const Value: integer);
    procedure SetAutoInitBitMask(const Value: boolean);
    procedure SetOnQualityCriteria(const Value: TOnStepwiseQualityCriteria);
  public
    (*<summary>Triggers stepwise regression recalculation.</summary>
               
<remarks>The result of this calculation is stored in the "BitMask" vector. The BitMask
               defines the variables which are to be included in to the regression by having BitMask.Bits[i] set to True.
               For detail statistics of the final configuration of the multiple regression
               call the <see cref="MulLinRegress"/> followed by <see cref="RegressTest"/>.
</remarks>
*)
    procedure Recalc;
    
    constructor Create(AOwner: TComponent); override;
    
    Destructor Destroy; override;
    
    
    (*<summary> Holds a list of vectors with data of variables </summary>
                 
<remarks>Each item holds data for one variable. All items in the list need to have equal length and the last item is
                 the dependent variable (y) from the equation:
                 <para/>
                 y = b0 + b1*x1 + b2*x2 + ... + bn*xn;
</remarks>
*)
    property A: TVecList read FA write SetA;
    
    (*<summary> Result matrix size of IterCount x (VarCount + 7). </summary>
                 
<remarks>Each row starts with Step number followed by selection list of variables in columns followed by Standard Error (quality criteria).
                 We strive to reduce standard error and the model with the smallest standard error is considered best. Additional columns are as follows:
                 <c>
                 [0] Standard error = sqrt(SSE/dFE)
                 [1] SSE = Residual sum of squares.
                 [2] SSR = Regression sum of squares.
                 [3] SST = Total sum of squares = SSE + SSR
                 [4] R2 = Coefficient of determination
                 [5] Adjusted R2 = Adjusted coefficient of determination
                 [6] MSE = Residual variance
                 </c>
</remarks>
*)
    property ReportSSE: TMtx read FReportSSE write SetReportSSE;
    
    (*<summary> Result matrix with size of (IterCount*VarCount) x 5. </summary>
                 
<remarks>Each iteration adds independent variable count rows. The columns are as follows:
                 <c>
                 [0] Iteration Step
                 [1] Variable index
                 [2] variable selection where 0 means excluded and 1 means included.
                 [3] holds the normalized coeffients and Fourth column the
                 [4] corresponding t-values for each coefficient
                 [5] two tailed p-values. Bigger p-values suggest the probability that the model would better, if the variable would be excluded.
                 </c>
</remarks>
*)
    property ReportCoeff: TMtx read FReportCoeff write SetReportCoeff;
        
    (*<summary>Bit vector holding 1 for included and 0 for excluded vars.</summary>
               
<remarks>Initialize the BitMask with BitMask.BitCount and then set BitMask.Bits[i] and
               set AutoInitBitMask property to false. When doing forward search, those variables already 1 will
               not be excluded. When doing backward search, the variables already 0 will not be included.
</remarks>
*)
    property BitMask: TVecInt read FBitMask write SetBitMask;
  published
    (*<summary>Defines stepwise regression model.</summary>*)
    property Method: TStepwiseMethod read FMethod write SetMethod default swBackward;
    (*<summary>Maximum iteration count before an exception will be raised.</summary>*)
    property MaxIter: integer read FMaxIter write SetMaxIter default 10000;
    (*<summary>Set this property to false, if you want to manually initialize the BitMask.</summary>
              
<remarks>This value does not affect the "exhaustive" method.
</remarks>
*)
    property AutoInitBitMask: boolean read FAutoInitBitMask write SetAutoInitBitMask default True;
    (*<summary>Implement this event, to return custom quality criteria other than Std. Error to guide the optimization.</summary>*)
    property OnQualityCriteria: TOnStepwiseQualityCriteria read FOnQualityCriteria write SetOnQualityCriteria;
  end;

