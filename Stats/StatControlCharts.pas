











(*<summary>Control charts.</summary>
  
<remarks>Introduces several control charts. Control charts are used to routinely monitor quality. Depending on the number of process
  characteristics to be monitored, there are two basic types of control charts. The first, referred to as a univariate control chart,
  is a graphical display (chart) of one quality characteristic. The second, referred to as a multivariate control chart, is a
  graphical display of a statistic that summarizes or represents more than one quality characteristic.<para/>

  If a single quality characteristic has been measured or computed from a sample, the control chart shows the value of the
  quality characteristic versus the sample number or versus time. In general, the chart contains a center line that represents
  the mean value for the in-control process. Two other horizontal lines, called the upper control limit (UCL) and the lower control
  limit (LCL), are also shown on the chart. These control limits are chosen so that almost all of the data points will fall within
  these limits as long as the process remains in-control.

  Check the following <see href="http://www.itl.nist.gov/div898/handbook/eda/section3/probplot.htm">link</see> to learn more
  about probability plots.
</remarks>
*)
unit StatControlCharts;


{$I BdsppDefs.inc}

interface

uses MtxVec, Math387, Statistics
  
  ,Classes
  ,SysUtils
  ,Types
  
  ;



(*<summary>X-Chart ("XBar Chart").</summary>

  <param name="Data">Each Data row contains replicated observation taken at specific time.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie in the (0,1) interval.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>This routine calculates X-Chart ("XBar Chart") drawing values, center line, upper and lower control limit.

  When dealing with a quality characteristic that can be expressed as a measurement, it is customary
  to monitor both the mean value of the quality characteristic and its variability. Control
  over the average quality is exercised by the control chart for averages, usually called the XBar chart.
  For this chart type, the center line is defined by process grand mean and upper and lower control limits are
  defined by process standard deviation. Similarly, for R and S charts the center line is defined by process range and
  variability (standard deviation) respectively.
</remarks>



  

  <example>
    The following code loads data, stored in binary file and then creates a XBar chart.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix Data;
      Vector DrawVec;
      double cl,ucl,lcl;
      Data.LoadFromFile("ewma_data.mtx");
      StatControlCharts.QCXChart(Data,DrawVec,out cl, out ucl, out lcl, 0.025);
    }
  }
  </code></example>

  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCSChart"/>
  <SeeAlso cref="QCRChart"/>*)
procedure QCXChart(const Data: TMtx; const DrawVec: TVec; out CL, UCL, LCL: double; Confidence: double = 0.997);

(*<summary>S-Chart.</summary>
  <param name="Data">Each Data row contains replicated observation taken at specific time.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie in the (0,1) interval.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>Calculates S-Chart drawing values, center line, upper and lower control limit.
</remarks>


  

  <example>
    Place a TChart component on the chart and add a TQCSerie (Series1). The
    following code creates necessary values for S chart:
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      double cl,ucl,lcl;
      Matrix Data = new Matrix(0);
      Vector DrawVec = new Vector(0);

      Data.LoadFromFile("data.mtx"); // data is now initialized
      StatControlCharts.QCSChart(Data,DrawVec,out cl, out ucl, out lcl, 0.025);
    }
  }
  </code></example>

  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCXChart"/>
  <SeeAlso cref="QCRChart"/>*)
procedure QCSChart(const Data: TMtx; const DrawVec: TVec; out CL, UCL, LCL: double; Confidence: double = 0.997);

(*<summary>R-Chart.</summary>
  <param name="Data">Each Data row contains replicated observation taken at specific time.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie in the (0,1) interval.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>Calculates R-Chart drawing values, center line, upper and lower control limit.
</remarks>


  

  <example>
    The following code will create R chart:
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix Data = new Matrix(0,0);
      Vector DrawVec = new Vector(0);
      double cl,ucl,lcl;
      Data.LoadFromFile("data.mtx");
      StatControlCharts.QCRChart(Data,DrawVec,out cl, out ucl, out lcl, 0.025);
    }
  }
  </code></example>

  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCXChart"/>
  <SeeAlso cref="QCSChart"/>*)
procedure QCRChart(const Data: TMtx; const DrawVec: TVec; out CL, UCL, LCL: double; Confidence: double = 0.997);

(*<summary>Process Capability indexes p, Cp and Cpk.</summary>
  <param name="Data">Process data.</param>
  <param name="LB">Lower specification limit. LB and UB are boundaries within which measurements on a product characteristic must lie.</param>
  <param name="UB">Upper specification limit. LB and UB are boundaries within which measurements on a product characteristic must lie.</param>
  <param name="Alpha">Desired significance level.</param>
  <param name="p">Returns calculated significance.</param>
  <param name="CP">Returns capability index.</param>
  <param name="CPConfInt">Returns 100*(1-Alpha) Cp confindence interval.</param>
  <param name="CPK">Returns capability index for uncentred process.</param>
  <param name="CPKConfInt">Returns 100*(1-Alpha) CPK confidence interval.</param>

  
<remarks>Calculates the Process Capability indexes p, Cp(aka CPR) and Cpk(AKA CPRk).
</remarks>


  

  <example>
    In the following example capability indexes and their condifence are calculated.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
        double p, lb, ub, cp, cpk;
        double[] CPConfInt, CPKConfInt;
        Vector data = new Vector(0);
        lb = -0.1;
        ub = 0.1;
        data.LoadFromFile("PCDATA.vec");
        StatControlCharts.QCCapIndexes(data, lb, ub, out p, out cp, out cpk, out CPConfInt, out CPKConfInt, 0.05);
    }
  }
  </code></example>*)
procedure QCCapIndexes(const Data: TVec; const LB, UB: double; out p, CP, CPK : double;
                       var CPConfInt, CPKConfInt: TTwoElmReal; Alpha: double = 0.05);

(*<summary>P-Chart.</summary>
  
<remarks>Calculates the P-Chart (Control chart for proportions) drawing values, center line, upper and lower control limits.
  Control limits are based on the normal approximation to the binomial distribution.
  When p is small, the normal approximation may not always be adequate. In such cases, we may use control limits obtained directly
  from a table of binomial probabilities. If P is small, the lower control limit obtained from the normal approximation may be a
  negative number. If this should occur, it is customary to consider zero as the lower control limit.

  Note
    The assumption is all samples have the same SampleSize.
</remarks>
*)
procedure QCPChart(const Data: TVec; SampleSize : Integer; const DrawVec: TVec; out CL, UCL, LCL : double; Confidence: double = 0.997); overload;
(*<summary>In this case each sample can have different size. You must store sizes in SampleSize vector.</summary>
  <param name="Data">Data to be analyzed. Each value represents number of defects.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie
    in the (0,1) interval.</param>
  <param name="SampleSize">Sample size. Can be integer or vector.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>An exeption is raised if Data and SampleSize length do not match.
</remarks>


  

  <example>
    The following code will create P chart:
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Dew.Stats.QCSeries QCSeries1)
    {
      Vector drawvec = new Vector(0);
      Matrix data = new Matrix(0,0);
      double cl,lcl,ucl;
      StatControlCharts.QCPChart(data,drawvec,out cl,out ucl,out lcl,0.05);
      // Setup series properties
      QCSeries->UCL = ucl;
      QCSeries->LCL = lcl;
      QCSeries->CL = cl;
      MtxVecTee.DrawValues(drawVec,QCSeries1,0,1,false);
    }
  }
  </code></example>

  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCNPChart"/>
  <SeeAlso cref="QCUChart"/>
  <SeeAlso cref="QCCChart"/>*)
procedure QCPChart(const Data: TVec; const SampleSize : TVec; const DrawVec: TVec; out CL: double; const UCL,LCL: TVec; Confidence: double = 0.997); overload;

(*<summary>Constructs nP-Chart.</summary>
  
<remarks>Constructs nP-Chart drawing values, center line, upper and lower control limit.
  The assumption is all samples have the same SampleSize.
</remarks>
*)
procedure QCNPChart(const Data: TVec; SampleSize : Integer; const DrawVec: TVec; out CL, UCL, LCL : double; Confidence: double = 0.997); overload;
(*<summary>In this case each sample can have different size. You must store sizes in SampleSize vector.</summary>
  <param name="Data">Data to be analyzed. Each value represents number of defects.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie
    in the (0,1) interval.</param>
  <param name="SampleSize">Sample size. Can be integer or vector.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>An exeption is raised if Data and SampleSize length do not match.
</remarks>


  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCPChart"/>
  <SeeAlso cref="QCUChart"/>
  <SeeAlso cref="QCCChart"/>*)
procedure QCNPChart(const Data: TVec; const SampleSize :TVec; const DrawVec: TVec; const CL, UCL, LCL: TVec; Confidence: double); overload;

(*<summary>U-Chart.</summary>
  
<remarks>Constructs U-Chart drawing values, center line, upper and lower control limit.
  The assumption is all samples have the same SampleSize.
</remarks>
*)
procedure QCUChart(const Data: TVec; SampleSize :Integer; const DrawVec: TVec; out CL, UCL, LCL : double; Confidence: double = 0.997); overload;
(*<summary>In this case each sample can have different size. You must store sizes in SampleSize vector.</summary>
  
<remarks>An exception is raised if Data and SampleSize length do not match.
</remarks>


  <param name="Data">Data to be analyzed. Each value represents number of defects.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie
    in the (0,1) interval.</param>
  <param name="SampleSize">Sample size. Can be integer or vector.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>
  <SeeAlso cref="QCPChart"/>
  <SeeAlso cref="QCNPChart"/>
  <SeeAlso cref="QCCChart"/>*)
procedure QCUChart(const Data: TVec; const SampleSize :TVec; const DrawVec: TVec; out CL: double; const UCL, LCL: TVec; Confidence: double = 0.997); overload;

(*<summary>Constructs C-Chart.</summary>
  <param name="Data">Data to be analyzed. Each value represents number of defects.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie
    in the (0,1) interval.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>Constructs C-Chart drawing values, center line, upper and lower control limit.
</remarks>


  <SeeAlso cref="QCPChart"/>
  <SeeAlso cref="QCNPChart"/>
  <SeeAlso cref="QCUChart"/>*)
procedure QCCChart(const Data: TVec; const DrawVec: TVec; out CL, UCL, LCL : double; Confidence: double);

(*<summary>Constructs EWMA Control Chart.</summary>
  
<remarks>Calculates the Exponential Weighted Moving Average (EWMA) control chart. In this case UCL and LCL are constant (asymptote) limits.
  This chart is also known as exponentially smoothed or geometric moving average chart. It evaluates the process level using an
  exponentially smoothed moving average. Here, by the term exponentially, we mean the procedure by which individual observations
  or subgroups are given progressively less importance or weight. When compared to the XChart, the EWMA chart is more sensitive
  to smaller shifts in the process level.

  The exponentially weighted moving average is defined as
  <code>
  XHat[t] = r*XHat[t] + (1-r)XHat[t-1] ,
  </code><para/>
  where r is a constant and XHat[t] are EWMA chart points. The starting value for the first sample at time t = 1 = XHat[0] is
  grand mean value.
</remarks>
*)
procedure EWMAChart(const Data: TMtx; const DrawVec: TVec; out CL, UCL, LCL: double; r: double = 0.3; Confidence: double = 0.997); overload;
(*<summary>Calculates the Exponential Weighted Moving Average (EWMA) control chart.</summary>
  <param name="Data">Data of grouped responses. Each row contais a response at specific time.
    It's asumed the rows are in time order.</param>
  <param name="r">Weighting constant that weights past and current information. If,
    for example, r=0.3, 70% of the weight will be given to past information and
    30% to current information. Typically a r between 0.1 and 0.4 provides a
    reasonable balance between past and current information and 0.2 is very common in actual practice.</param>
  <param name="Confidence"></param>
  <param name="DrawVec">Returns the calculated EWMA chart points.</param>
  <param name="CL">Returns EWMA chart center line.</param>
  <param name="LCL">Returns EWMA chart lower control limits. In this case UCL and LCL are constant (asymptote) limits.</param>
  <param name="UCL">Returns EWMA chart upper control limits. In this case UCL and LCL are constant (asymptote) limits.</param>

  
<remarks>In this case UCL and LCL are not constant, but use an exact formula to calculate control limits for each point. It's worth noting
  that UCL and LCL values rapidly approach the asymptote value.
</remarks>



  

  <example>
    The following code will load data from file and create EWMA chart with
    r=0.25 and significance 95%:
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Steema.TeeChart.Styles.Line Series1, Steema.TeeChart.Styles.Line Series2,
      Steema.TeeChart.Styles.Line Series3)
    {
      Matrix data = new Matrix(0,0);
      Vector lcl = new Vector(0);
      Vector ucl = new Vector(0);
      double cl;
      data.LoadFromFile("ewma_data.vec");
      StatControlCharts.EWMAChart(data,drawvec,out cl,ucl,lcl,0.25,0.95);
      MtxVecTee.DrawValues(drawvec,Series1,0,1,false);
      // Series2 and Series3 are used for displaying control limits.
      MtxVecTee.DrawValues(lcl,Series2,0,1,false);
      MtxVecTee.DrawValues(ucl,Series3,0,1,false);
    }
  }
  </code></example>

  <SeeAlso cref="TQCSeries"/>
  <SeeAlso cref="Dew.Stats.Tee.QCSeries"/>*)
procedure EWMAChart(const Data: TMtx; const DrawVec: TVec; out CL: double; UCL, LCL: TVec; r: double = 0.3; Confidence: double = 0.997); overload;

(*<summary>Compares values agains Westgard rules.</summary>
  <param name="Data">Data to-be-checked with Westgard rules.</param>
  <param name="OutControl">Vector storing each point status. If point(i) is out-of-control, then
    OutControl.Values[i] will be greater than zero (see explanation above).</param>
  <param name="dataMean">In most cases equal to data mean, but you can specify any value.</param>
  <param name="dataSigma">In most cases equal to data standard deviation, but you can specify any value.</param>

  
<remarks>Data individual values are tested to determine if they are in, or out, of control using a set of five rules called the Westgard rules. These rules indicate
  which points are out-of-control. The Westgard rules are (see <see href="http://www.westgard.com/mltirule.htm">www.westgard.com/mltirule.htm</see> for details):

  <list type="table">
   <listheader>
           <term>Description</term> <term>Rule</term>
   </listheader>

   <item>  <term>1S3</term>  <term>One value beyond three sigma from the mean.</term>   </item>
   <item>  <term>1S2</term>  <term>One value beyond two sigma from the mean.</term> </item>
   <item>  <term>2S2</term>  <term>Two consecutive values either greater than, or less than, two sigma from the mean.</term> </item>
   <item>  <term>RS4</term>  <term>A difference between consecutive values greater than four sigma.</term> </item>
   <item>  <term>41S</term>  <term>Four consecutive values greater than, or less than, one sigma from the mean.</term> </item>
   <item>  <term>10X</term>  <term>Ten consecutive values all greater than, or less than, the mean.</term></item>
  </list>

  For each point several rules can be violated at the same time. Each Westgard rule violation has different value:
  <list type="table">
   <listheader>
           <term>Rule violated</term> <term>Value</term>
  </listheader>
   <item>  <term>   1S3     </term>  <term>      1=2^0  </term>   </item>
   <item>  <term>   1S2     </term>  <term>      2=2^1  </term>   </item>
   <item>  <term>   2S2   * </term>  <term>      4=2^2  </term>   </item>
   <item>  <term>   RS4     </term>  <term>      8=2^3  </term>   </item>
   <item>  <term>   4S1     </term>  <term>      16=2^4 </term>   </item>
   <item>  <term>   10X   * </term>  <term>      32=2^5 </term>   </item>
  </list>

  For example, if rules 2S2 and 10X were violated, then the sum of violations for point would be 4 + 32 = 36 = 100100.
</remarks>


  

  <example>
    Load process data, then check if any points are out-of-control.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(Dew.Stats.QCSeries QCSeries1)
    {
      Vector data = new Vector(0);
      Vector outofcontrol = new Vector(0);
      VectorInt indexes = new VectorInt(0);
      data.LoadFromFile("process_data.vec");
      StatControlCharts.QCWestgardRules(data,outofcontrol,data.Mean(),data.StdDev());
      // Now find indexes of out-of-control points
      indexes.FindIndexes(outofcontrol,"&gt;",0);
      // indexes.IValues now stores the indices of out-of-control points
    }
  }
  </code></example>*)
procedure QCWestgardRules(const Data: TVec; const OutControl: TVec; dataMean, dataSigma: double);

(*<summary>Moving range chart.</summary>
  <param name="Data">Data to be analyzed. Each value represents number of defects.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie
    in the (0,1) interval.</param>
  <param name="CL">Returns control Chart centerline.</param>
  <param name="UCL">Returns control Chart upper control limit.</param>
  <param name="LCL">Returns control Chart lower control limit.</param>

  
<remarks>Uses Data values to construct a moving-range quality control chart. This QC chart type can be
  used for individual measurements (sample size = 1).
</remarks>
*)
procedure QCMRChart(const Data: TVec; out CL, UCL, LCL: double; Confidence: double = 0.997);

(*<summary>CUMSum chart.</summary>
  <param name="Data">Each Data row contains replicated observation taken at specific time.</param>
  <param name="Confidence">Confidence level for upper and lower control limit. Confidence must lie in the (0,1) interval.</param>
  <param name="DrawVec">Returns values to be drawn.</param>
  <param name="mean">Defines the estimate of the in-control mean.</param>
  <param name="sigma">Defines known (or estimated) standard deviation of the sample means.</param>
  <param name="k">Design parameter of the V-mask. k is the rise in the V-arm corresponding to one sampling unit.</param>
  <param name="h">Design parameter of the V-mask. h defines the rise in the arm coresponding to the distance
    from the origin to point vertex.</param>
  <param name="SHigh">If set, returns high values for cumsum plot. When SHigh exceeds value h, the process is said to be out-of-control.
    Compare SHigh values to h to find if a process is out-of-control.</param>
  <param name="SLow">If set, returns low values for cumsum plot. When SLow exceeds value h, the process is said to be out-of-control.
    Compare SLow values to h to find if a process is out-of-control.</param>

  
<remarks>Calculates CumSum Chart drawing values and additonal values, needed for deciding if some samples are
  out-of-control.
  See <see href="http://www.itl.nist.gov/div898/handbook/pmc/section3/pmc323.htm">www.itl.nist.gov/div898/handbook/pmc/section3/pmc323.htm</see> to
  learn more about Cumsum QC charts.
</remarks>


  

  <example>
    Perform CumSum QC to determine if process is out-of-control.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example()
    {
      Matrix data = new Matrix(0,0);
      data.LoadFromFile("process_data.mtx");

      Vector sh = new Vector(0);
      Vector sl = new Vector(0);
      Vector s = new Vector(0);
      VectorInt outofcontrol = new VectorInt(0);

      // estimate k=0.32, h = 4.7
      double k = 0.32;
      double h = 4.7;
      // estimate process mean and sigma
      double m = 230.3;
      double sig = 5.2;
      StatControlCharts.QCCumSumChart(data,s,k,h,m,sig,sh,sl,0.997);
      // find point indexes which exceed h (out-of-control points)
      outofcontrol.FindIndexes(sh,"&gt;",h);
    }
  }
  </code></example>*)
procedure QCCumSumChart(const Data: TMtx; const DrawVec: TVec; k, h: double; mean: double; sigma: double = 1.0;
  const SHigh: TVec = nil; const SLow: TVec = nil; Confidence: double = 0.997);





