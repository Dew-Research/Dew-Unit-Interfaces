











(*<summary>Probabilities plots.</summary>
  
<remarks>The probability plot (Chambers 1983) is a graphical technique for assessing whether or not a data set
  follows a given distribution such as the normal or Weibull. The data are plotted against a theoretical distribution
  in such a way that the points should form approximately a straight line. Departures from this straight line indicate
  departures from the specified distribution.

  The correlation coefficient associated with the linear fit to the data in the probability plot is a measure of the
  goodness of the fit. Estimates of the location and scale parameters of the distribution are given by the intercept
  and slope. Probability plots can be generated for several competing distributions to see which provides the best fit,
  and the probability plot generating the highest correlation coefficient is the best choice since it generates the
  straightest probability plot.

  For distributions with shape parameters (not counting location and scale parameters), the shape parameters must
  be known in order to generate the probability plot. For distributions with a single shape parameter, the probability
  plot correlation coefficient (PPCC) plot provides an excellent method for estimating the shape parameter.

  Check the following <see href="http://www.itl.nist.gov/div898/handbook/eda/section3/probplot.htm">link</see> to learn more
  about probability plots.
</remarks>
*)
unit StatProbPlots;


{$I BdsppDefs.inc}

interface

uses MtxVec, Math387
  
  ,Classes
  ,SysUtils
  
  ;



(*<summary>Constructs the Normal Probability Chart.</summary>
  <param name="Data">Data to be drawn. The assumption is data values are sorted.</param>
  <param name="DataSorted">If true, algorithm assumes Data is already sorted in ascending order.
    If Data is not sorted, you must set this parameter to false so that internal algorithm will
    automatically do the sorting.</param>
  <param name="XDrawVec">Returns normal probability plot horizontal values to be drawn - &gt; estimated quantiles
    from Data vector or in this case <b>sorted</b> Data values.</param>
  <param name="YDrawVec">Returns normal probability plot vertical values to be drawn - >
    Values are generated from theoretical standard normal distribution with
    parameters <c>mu=0, sigma^2=1</c>.</param>
  <param name="MinX">Returns slope line start X point, XDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MinY">Returns slope line start Y point, YDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxX">Returns slope line end X point, XDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxY">Returns slope line end Y point, YDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="StdErrs">Standard error</param>

  
<remarks><b>How to construct Normal distribution probability plot?</b><para/>
  <list type="number">
  <item> If needed, Data values are sorted (DataSorted parameter must be set to false). </item>
  <item> Abscissa drawing values are formed by estimated data quantiles - ordered Data values. After
      calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
      properties of XDrawVec are adjusted automatically. </item>
  <item> Ordinate drawing values are formed by using theoretical probability <c>p</c> i.e the so
      <c>Z</c> values. After calculation they are copied to YDrawVec. <see cref="TMtxVecBase.Length"/> and
      <see cref="TMtxVec.Complex"/> properties of YDrawVec are adjusted automatically.</item>
  <item> XDrawVec and YDrawVec 25th and 75th percentile points are used to construct a reference line.
      Drawing points departures from this straight line indicate departures from normality.</item>
  </list>


  The normal probability plot is used to answer the following questions:
  <list type="bullet">
  <item> Are the data normally distributed?</item>
  <item> What is the nature of the departure from normality (data skewed, shorter than expected tails, longer than expected tails)? </item>
  </list>
</remarks>


  

  <example>
  The following code will create probability plot and then plot calculated values.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(ProbabilityPlot Series1)
    {
      double x1,x2;
      double y1,y2;

      Vector data = new Vector(100,false);
      Vector xvec = new Vector(0);
      Vector yvec = new Vector(0);

      data.RandGauss(0.0, 1.0); // standard distribution
      StatProbPlots.StatNormalPlot(data, xvec, yvec, out x1, out x2, out y1, out y2, null,false);
      Series1.MinX = x1;
      Series1.MaxX = x2;
      Series1.MinY = y1;
      Series1.MaxY = y2;
      MtxVecTee.DrawValues(xvec,yvec,Series1,false);
    }
  }
  </code></example>

  <SeeAlso cref="Dew.Stats.Tee.ProbabilityPlot"/>*)
procedure StatNormalPlot(const Data: TVec; const XDrawVec, YDrawVec: TVec; out MinX, MaxX, MinY, MaxY: double;
  const StdErrs: TVec = nil; const DataSorted: boolean = true);

(*<summary>Constructs the Quantile-Quantile probability plot.</summary>
  <param name="XData">X Data (first dataset).</param>
  <param name="YData">Y Data (second dataset).</param>
  <param name="XDataSorted">If true, algorithm assumes XData is already sorted in ascending order.
    If XData is not sorted, you must set this parameter to false so that internal algorithm will
    automatically do the sorting.</param>
  <param name="YDataSorted"> If true, algorithm assumes YData is already sorted in ascending order.
    If YData is not sorted, you must set this parameter to false so that internal algorithm will
    automatically do the sorting.</param>
  <param name="XDrawVec">Returns q-q plot horizontal values to be drawn - &gt; estimated quantiles
    from XData vector or in this case <b>sorted</b> XData values.</param>
  <param name="YDrawVec">Returns q-q plot vertical values to be drawn - &gt; estimated quantiles
    from YData vector or in this case <b>sorted</b> YData values.</param>
  <param name="MinX">Returns slope line start X point, XDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MinY">Returns slope line start Y point, YDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxX">Returns slope line end X point, XDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxY">Returns slope line end Y point, YDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>

  
<remarks>Constructs the Quantile-Quantile Chart. Use <see cref="TStatProbSeries"/> to visualize/plot constructed values.
  The QQ chart is a graphical technique for determining if two data sets come from populations with a common distribution.
  Specifically, QQ chart is a plot of the quantiles of the first data set against the quantiles of the second data set.
  By a quantile, we mean the fraction (or percent) of points below the given value. That is, the 0.3 (or 30%) quantile is the point
  at which 30% percent of the data fall below and 70% fall above that value. A 45-degree reference line is also plotted. If the two
  sets come from a population with the same distribution, the points should fall approximately along reference line. The greater
  the departure from this reference line, the greater the evidence for the conclusion that the two data sets have come from populations
  with different distributions.<para/>

  The advantages of the q-q plot are:
  <list type="bullet">
  <item> The sample sizes do not need to be equal.</item>
  <item> Many distributional aspects can be simultaneously tested. For example, shifts in location, shifts in scale, changes in symmetry,
    and the presence of outliers can all be detected from this plot. For example, if the two data sets come from populations whose distributions
    differ only by a shift in location, the points should lie along a straight line that is displaced either up or down from the 45-degree
    reference line.</item>
  </list>

  The QQ Chart is similar to a probability plot. For a probability plot, the quantiles for one of the data samples are replaced with the
  quantiles of a theoretical distribution. The QQ chart is used to answer the following questions:
  <list type="bullet">
  <item> Do two data sets come from populations with a common distribution? </item>
  <item> Do two data sets have common location and scale? </item>
  <item> Do two data sets have similar distributional shapes? </item>
  <item> Do two data sets have similar tail behavior? </item>
  </list>

  When there are two data samples, it is often desirable to know if the assumption of a common distribution is justified. If so, then
  location and scale estimators can pool both data sets to obtain estimates of the common location and scale. If two samples do differ,
  it is also useful to gain some understanding of the differences. The QQ Chart can provide more insight into the nature of the difference
  than analytical methods such as the <see cref="Statistics.GOFChi2Test"/> and <see cref="Statistics.GOFKolmogorov"/> two sample tests.<para/>

  If the data sets have the same size, the q-q plot is essentially a plot of sorted X against sorted Y. If the data sets are not of equal
  size, the quantiles are usually picked to correspond to the sorted values from the smaller data set and then the quantiles for the
  larger data set are interpolated.<para/>

  <b>How to construct two datasets Q-Q plot?</b>
  <list type="number">
  <item>  If needed, XData values or YData are sorted (DataSorted parameter set to false).</item>
  <item> Abscissa drawing values are formed by estimated XData quantiles - ordered XData values. After
      calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
      properties of XDrawVec are adjusted automatically.</item>
  <item> Ordinate drawing values are formed by estimated YData quantiles - ordered YData values. After
      calculation they are copied to YDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
      properties of YDrawVec are adjusted automatically.</item>
  <item> XDrawVec and YDrawVec 25th and 75th percentile points are used to construct a reference line.
      Drawing points departures from this straight line indicate XData and YData do <b>not</b> come from
      the same distribution.</item>
  </list>
</remarks>


  

  <example>
    The following code will create probability plot and then plot calculated values.
  <code>
  using Dew.Math;
  using Dew.Math.Units;
  using Dew.Stats.Units;
  namespace Dew.Examples
  {
    private void Example(ProbabilityPlot Series1)
    {
      double x1,x2;
      double y1,y2;

      Vector xdata = new Vector(100,false);
      Vector ydata = new Vector(100,false);
      Vector xvec = new Vector(0);
      Vector yvec = new Vector(0);

      xdata.RandGauss(0.0,1.0); // standard distribution
      ydata.RandGauss(1.0,2.3); // standard distribution
      StatProbPlots.StatQQPlot(xdata, ydata, xvec, yvec, out x1, out x2, out y1, out y2, false,false);
      Series1.MinX = x1;
      Series1.MaxX = x2;
      Series1.MinY = y1;
      Series1.MaxY = y2;
      MtxVecTee.DrawValues(xvec,yvec,Series1,false);
    }
  }
  </code></example>

  <SeeAlso cref="Dew.Stats.Tee.ProbabilityPlot"/>*)
procedure StatQQPlot(const XData, YData: TVec; const XDrawVec, YDrawVec: TVec; out MinX, MaxX, MinY, MaxY: double;
                     const XDataSorted: boolean = true; const YDataSorted: boolean = true); overload;

(*<summary>Constructs an Q-Q plot of the quantiles of the data set Data againsts the quantiles of a
  standard Normal distribution.</summary>

  
<remarks><b>How to construct Q-Q normal distribution probability plot?</b>
  <list type="number">
  <item> If needed, Data values are sorted (DataSorted parameter set to false). </item>
  <item> Abscissa drawing values are formed by estimated data quantiles - ordered Data values. After
      calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
      properties of XDrawVec are adjusted automatically. </item>
  <item> Ordinate drawing values are formed by using theoretical probabilitie <c>p</c> i.e the so
      <c>Z</c> values. After calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and
      <see cref="TMtxVec.Complex"/> properties of XDrawVec are adjusted automatically. </item>
  <item> XDrawVec and YDrawVec 25th and 75th percentile points are used to construct a reference line.
      Drawing points departures from this straight line indicate departures from normality.   </item>
  </list>

  Note:
    Use this overload if you want to check if specific data comes from standard Normal distribution
    <c>(mean = 0, sigma^2 = 1.0)</c>
</remarks>
*)
procedure StatQQPlot(const Data: TVec; const XDrawVec, YDrawVec: TVec; out MinX, MaxX, MinY, MaxY: double; const DataSorted: boolean = true); overload;

(*<summary>Constructs the Weibull Probability Chart.</summary>
  <param name="Data">Data to be drawn.</param>
  <param name="DataSorted">If true, algorithm assumes Data is already sorted in ascending order.
    If Data is not sorted, you must set this parameter to false so that internal algorithm will
    automatically do the sorting.</param>
  <param name="XDrawVec">Returns vector of X values to be drawn - >
    Data estimated quantiles or in this case <b>ordered</b> data values.</param>
  <param name="YDrawVec">Returns vector of Y values to be drawn - &gt; theretical Weibull probability values
    or in this case <c>ln(ln(1/(1-p))</c>, where <c>p</c> are predefined theoretical probabilities.</param>
  <param name="MinX">Returns slope line start X point, XDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MinY">Returns slope line start Y point, YDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxX">Returns slope line end X point, XDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxY">Returns slope line end Y point, YDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>

  
<remarks>Constructs the Weibull Probability Chart. Use <see cref="Dew.Stats.Tee.ProbabilityPlot"/>
  to visualize/plot constructed values. The Weibull plot is a graphical technique for determining if a data set comes from a population
  that would logically be fitted by a 2-parameter Weibull distribution (the location is assumed to be zero).<para/>

  The Weibull plot has special scales that are designed so that if the data do in fact follow a Weibull distribution,
  the points will be linear (or nearly linear). The least squares fit of this line yields estimates for the shape and scale
  parameters of the Weibull distribution. Weibull distribution (the location is assumed to be zero).<para/>

  <b>How to construct Weibull distribution probability plot?</b>
  <list type="number">
  <item> If needed, Data values are sorted (DataSorted parameter set to false). </item>
  <item> Abscissa drawing values are formed by estimated data quantiles - ordered Data values. After
      calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
      properties of XDrawVec are adjusted automatically. </item>
  <item> Ordinate drawing values are formed by using theoretical probability <c>p</c>
      to <c> p - &gt; ln[ln[1/(1-p)]]</c>. After calculation they are copied to YDrawVec. <see cref="TMtxVecBase.Length"/> and
      <see cref="TMtxVec.Complex"/> properties of YDrawVec are adjusted automatically. </item>
  <item> XDrawVec and YDrawVec 25th and 75th percentile points are used to construct a reference line.
      Drawing points departures from this straight line indicate departures from Weibull distribution. </item>
  </list>

  The Weibull plot can be used to answer the following questions:
  <list type="bullet">
  <item> Do the data follow a 2-parameter Weibull distribution?   </item>
  <item> What is the best estimate of the shape parameter for the 2-parameter Weibull distribution? </item>
  <item> What is the best estimate of the scale (= variation) parameter for the 2-parameter Weibull distribution? </item>
  </list>
</remarks>


  

  <example>
    ExampleThe following code will create probability plot and then plot calculated values.
    <code>
    using Dew.Math;
    using Dew.Math.Units;
    using Dew.Stats.Units;
    namespace Dew.Examples
    {
      private void Example(ProbabilityPlot Series1)
      {
        double x1,x2;
        double y1,y2;

        Vector data = new Vector(100,false);
        Vector xvec = new Vector(0);
        Vector yvec = new Vector(0);

        StatRandom.RandomWeibull(3.0, 1.2,data,-1); // some random values
        StatProbPlots.StatWeibullPlot(data, xvec, yvec, out x1, out x2, out y1, out y2, false);
        Series1.MinX = x1;
        Series1.MaxX = x2;
        Series1.MinY = y1;
        Series1.MaxY = y2;
        MtxVecTee.DrawValues(xvec,yvec,Series1,false);
      }
    }
    </code></example>

  <SeeAlso cref="Dew.Stats.Tee.ProbabilityPlot"/>*)
procedure StatWeibullPlot(const Data: TVec; const XDrawVec, YDrawVec: TVec; out MinX, MaxX, MinY, MaxY: double; const DataSorted: boolean = true);

(*<summary>Exponential distribution probability plot.</summary>
  <param name="Data">Data to be drawn.</param>
  <param name="DataSorted">If true, algorithm assumes Data is already sorted in ascending order.
    If Data is not sorted, you must set this parameter to false so that internal algorithm will
    automatically do the sorting.</param>
  <param name="XDrawVec">Returns vector of X values to be drawn - >
    Data estimated quantiles or in this case <b>ordered</b> data values.</param>
  <param name="YDrawVec">Returns vector of Y values to be drawn - &gt; theretical Exponential probability values
    or in this case <c>-ln(1-p)</c>, where <c>p</c> are predefined theoretical probabilities.</param>
  <param name="MinX">Returns slope line start X point, XDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MinY">Returns slope line start Y point, YDrawVec <c>25th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxX">Returns slope line end X point, XDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.</param>
  <param name="MaxY">Returns slope line end Y point, YDrawVec <c>75th</c> percentile.
    These value are used by <see cref="Dew.Stats.Tee.ProbabilityPlot"/> series.
  </param>

  
<remarks><b>How to construct Exponential distribution probability plot?</b>
  <list type="number">
  <item> If needed, Data values are sorted (DataSorted parameter set to false). </item>
  <item> Abscissa drawing values are formed by estimated data quantiles - ordered Data values. After
         calculation they are copied to XDrawVec. <see cref="TMtxVecBase.Length"/> and <see cref="TMtxVec.Complex"/>
         properties of XDrawVec are adjusted automatically. </item>
  <item> Ordinate drawing values are formed by using theoretical probability <c>p</c>
         0 to <c> p -> -ln(1-p)</c>. After calculation they are copied to YDrawVec. <see cref="TMtxVecBase.Length"/> and
         <see cref="TMtxVec.Complex"/> properties of YDrawVec are adjusted automatically. </item>
  <item> XDrawVec and YDrawVec 25th and 75th percentile points are used to construct a reference line.
         Drawing points departures from this straight line indicate departures from Exponential distribution. </item>
  </list>
</remarks>


  <SeeAlso cref="Dew.Stats.Tee.ProbabilityPlot"/>*)
procedure StatExpPlot(const Data: TVec; const XDrawVec, YDrawVec: TVec; out MinX, MaxX, MinY, MaxY: double; const DataSorted: boolean = true);



