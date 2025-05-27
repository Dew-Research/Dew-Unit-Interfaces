











unit AdaptiveFiltering;


interface

{$I BdsppDefs.inc}
                              
uses
  Math387, MtxVec

  
    
      ,Types
    
  

  ;

type

  (*<summary> Abstract class for the Kalman filtering algorithm(s). </summary>*)

  TBaseKalmanFilter = class
  strict protected
    Z_IsScalar: boolean;
    X_IsScalar: boolean;
    R_IsScalar: boolean;
    Q_IsScalar: boolean;
    H_IsScalar: boolean;
    TimeInitialized: boolean;
    MeasurementInitialized: boolean;

    tmpV: TMtx;
    tmpS: TMtx;
    tmpM: TMtx;
    tmpA: TMtx;
    procedure OnSize(Sender: TObject);
  public
    (*<summary> Optional parameter. </summary>*)
    Iter: integer;
    (*<summary> The process estimates. </summary>*)
    x: TMtx;
    (*<summary> The measured values. </summary>*)
    z: TMtx;

    (*<summary> Maps x(k-1) to x(k) without noise or system input. </summary>
                  
<remarks>In case of the extended Kalman filter this the
                  matrix of partial derivates of non-linear f with respect to x.
                  The variable is in some sources refered to as the "F".
</remarks>
*)

    A: TMTx;
    (*<summary> Relates process values x(k) to the measurement z(k). </summary>
                 
<remarks>In case of the extended Kalman filter this is the
                 matrix of partial derivates of non-linear h with respect to w, where
                 w is the process noise.
</remarks>
*)
    H: TMtx;
    (*<summary> Kalman gain. </summary>*)
    K: TMtx;
    (*<summary> Estimation error covariance. </summary>*)
    P: TMtx;
    (*<summary> Process noise covariance. </summary>*)
    Q: TMtx;
    (*<summary> Measurement noise covariance. </summary>*)
    R: TMtx;
    (*<summary> Applies one "time update". </summary>*)
    procedure TimeUpdate; virtual;
    (*<summary> Applies one "measurement update". </summary>*)
    procedure MeasurementUpdate; virtual;
    (*<summary> Advances the computation by one iteration. </summary>
                 
<remarks>All public matrix and vector fields like x, z, A, H, P, Q, R,...
                 can be modified between consecutive calls to Update by the
                 user, if neccessary.
</remarks>
*)
    procedure Update;

    constructor Create; virtual;
    destructor Destroy; override;

  end;

  (*<summary> Kalman filter component implements standard Kalman filtering algorithm. </summary>

    
<remarks>The standard Kalman filtering equations are defined as following:

    Time update:

    <c> x(k) = A*x(k-1) + B*u(k-1) </c> <para/>
    <c> P(k) = A*P(k-1)*A^T + Q </c>

    Measurement update:

    <c> K(k) = P(k)*H^T*(H*P(k)*H^T +R)^-1 </c> <para/>
    <c> x(k) = x(k) + K(k)*(z(k) - H*x(k)) </c> <para/>
    <c> P(k) = (I - K(k)*H)*P(k) </c>

    The process x(k) has a known mathemathical model. When we have an application
    where we need to measure x(k) as accurately as possible the Kalman filter
    can help us reduce the noise. We use the added information from the
    mathemathical model to more effectively filter the signal.

    Description of symbols:

    <c> s - number of parallel inputs (columns in x and z). </c> <para/>
    <c> x(k) - size: n x s. vector state (value) of the process </c> <para/>
    <c> A - size: n x n. Maps x(k-1) to x(k) without noise or system input </c> <para/>
    <c> u(k) - control vector input (optional). size: l x s </c> <para/>
    <c> B - size: n x l maps control input u(k-1) to x(k) </c> <para/>
    <c> Q - process noise covariance </c> <para/>
    <c> R - measurement noise covariance </c> <para/>
    <c> z - size: m x s. Measurement vector </c> <para/>
    <c> H - size: m x n. Relates x(k) to the measurement z(k) </c> <para/>
    <c> w - process noise </c> <para/>
    <c> v - measurement noise </c>

    All parameters can be modified by the user before each iteration of the filter.
    One iteration of the filter is achieved by calling the Update method.
    The process noise and measurement noise are assumed to be independent
    and gaussian.

    <seealso href="http://www.cs.unc.edu/~welch/kalman/kalmanIntro.html">[1] An Introduction to the Kalman Filter, Greg Welch and Gary Bishop</seealso>
</remarks>
*)

  TKalmanFilter = class(TBaseKalmanFilter)
  strict protected
    FOnDrivingFunction: TMtxNotifyEvent;
    u: TMtx;
    B_IsScalar: boolean;
    procedure SetOnDrivingFunction(const Value: TMtxNotifyEvent);
  public
  (*<summary> Matrix B is used in the x(k) = A*x(k-1) + B*u(k-1). </summary>*)
    B: TMtx;

    procedure TimeUpdate; override;
    procedure MeasurementUpdate; override;
    constructor Create; override;
    destructor Destroy; override;

  public
  (*<summary> Event type used to provide the driving function for the Kalman filter. </summary>
               
<remarks>The driving function is the  u(k) in the formula  x(k) = A*x(k-1) + B*u(k-1).
               The parameter of the event is of type of TMtx and should hold the new u(k) data
               when the function returns.
</remarks>
*)
    property OnDrivingFunction: TMtxNotifyEvent read FOnDrivingFunction write SetOnDrivingFunction;
  end;

  (*<summary> Event type used by Extended Kalman filter. </summary>
                
<remarks>The z is to be filled with "measurement" data, the function h(x,0).
</remarks>
*)
  TKalmanMeasurementEvent = procedure (z: TMtx) of object;

  (*<summary> Event type used by Extended Kalman filter. </summary>
            
<remarks>The x is to be filled with "process" data, the function x(k) = f(x(k-1), u(k-1), 0 ).
</remarks>
*)
  TKalmanProcessEvent = procedure (x: TMtx) of object;

  (*<summary> Extended Kalman filter component allows filtering of a non-linear process.</summary>

    
<remarks>Kalman filter that will linearize non-linear process around the covariance and current average value is called
    the extended Kalman filter. The equations are defined as following:

    Time update:

    <c> x(k) = f(x(k-1), u(k-1), 0 ) </c> <para/>
    <c> P(k) = A(k)*P(k-1)*A(k)^T + W(k)*Q*W(k)^T  </c>

    Measurement update:

    <c> K(k) = P(k)*H(k)^T *( H(k)*P(k)*H(k)^T + V(k)* R*V(k)^T )^-1 </c> <para/>
    <c> x(k) = x(k) + K(k) * (z(k) - h(x(k), 0)) </c> <para/>
    <c> P(k) = (I - K(k)*H(k))*P(k) </c>

    The process x(k) has a known mathemathical model. When we have an application
    where we need to measure x(k) as accurately as possible the Kalman filter
    can help us reduce the noise. We use the added information from the
    mathemathical model to more effectively filter the signal.

    Description of symbols:

    <c> s - number of parallel inputs (columns in x and z). </c> <para/>
    <c> f - non-linear function relating x(k-1) to x(k) </c> <para/>
    <c> x(k) - size: n x s. vector state (value) of the process in each column </c> <para/>
    <c> u(k) - control vector input. size: l x s </c> <para/>
    <c> B - size: n x l maps control input u(k-1) to x(k) </c> <para/>
    <c> Q - process noise covariance </c> <para/>
    <c> R - measurement noise covariance </c> <para/>
    <c> z - size: m x s. Measurement vector </c> <para/>
    <c> h - Non-linear function relates x(k) to the measurement z(k) </c> <para/>
    <c> w(k) - process noise </c> <para/>
    <c> v(k) - measurement noise </c> <para/>
    <c> A(k) - size: n x n. matrix of partial derivates of f with respect to x </c> <para/>
    <c> H(k) - matrix of partial derivates of h with respect to x </c> <para/>
    <c> W(k) - matrix of partial derivates of f with respect to w </c> <para/>
    <c> V(k) - matrix of partial derivates of h with respect to v </c>

    All parameters can be modified by the user before each iteration of the filter.
    One iteration of the filter is achieved by calling the Update method.
    The process noise and measurement noise are assumed to be independent
    and gaussian.

    <seealso href="http://www.cs.unc.edu/~welch/kalman/kalmanIntro.html">[1] An Introduction to the Kalman Filter, Greg Welch and Gary Bishop</seealso>
</remarks>
*)

  TExtendedKalmanFilter = class(TBaseKalmanFilter)
  strict protected
    V_IsOne: boolean;
    tmpR: TMtx;
    tmpB: TMtx;
    FOnGenerateMeasurement: TKalmanMeasurementEvent;
    FOnGenerateProcess: TKalmanProcessEvent;
    procedure SetOnGenerateMeasurement(const Value: TKalmanMeasurementEvent);
    procedure SetOnGenerateProcess(const Value: TKalmanProcessEvent);
  public
    (*<summary> Matrix of partial derivates of f with respect to w. w is the process noise. </summary>*)
    W: TMtx;
    (*<summary> Matrix of partial derivates of h with respect to v. v is the measurement noise. </summary>*)
    V: TMtx;

    procedure TimeUpdate; override;
    procedure MeasurementUpdate; override;
    constructor Create; override;
    destructor Destroy; override;

  public
    (*<summary> Parameter is of type TVec and has to be filled with result of non-linear process function f. </summary>*)
    property OnGenerateProcess: TKalmanProcessEvent read FOnGenerateProcess write SetOnGenerateProcess;
    (*<summary> Parameter is of type TVec and has to be filled with result of non-linear measurement function h. </summary>*)
    property OnGenerateMeasurement: TKalmanMeasurementEvent read FOnGenerateMeasurement write SetOnGenerateMeasurement;
  end;


