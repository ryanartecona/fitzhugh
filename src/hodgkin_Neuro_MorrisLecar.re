let logisticCurve = Hodgkin_Util.logisticCurve;

type state = {
  v: float,
  n: float
};

let initState = {v: (-65.), n: 0.};

let lowV = (-60.);

let highV = 10.;

let lowN = 0.;

let highN = 0.5;

let step
    ::g_L=8.
    /* ::e_L=(-80.) */
    ::e_L=(-78.)
    ::g_Na=20.
    ::e_Na=60.
    ::g_K=10.
    ::e_K=(-90.)
    ::m_inf=(logisticCurve midValue::(-20.) slopeInv::15.)
    /* ::n_inf=(logisticCurve midValue::(-25.) slopeInv::5.) */
    ::n_inf=(logisticCurve midValue::(-45.) slopeInv::5.)
    ::input=0.
    ::t=1.
    st => {
  let {v, n} = st;
  let leakCurrent = g_L *. (v -. e_L);
  let sodiumCurrent = g_Na *. m_inf v *. (v -. e_Na);
  let potassiumCurrent = g_K *. n *. (v -. e_K);
  let dv = input -. leakCurrent -. sodiumCurrent -. potassiumCurrent;
  let dn = n_inf v -. n;
  {v: v +. dv *. t, n: n +. dn *. t}
};
