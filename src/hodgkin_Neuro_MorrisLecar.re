let logisticCurve = Hodgkin_Util.logisticCurve;

type state = {
  v: float,
  n: float
};

type deriv = {
  dv: float,
  dn: float
};

let initState = {v: (-65.), n: 0.};

let minV = (-80.);

let maxV = 10.;

let restV = (-60.);

let minN = 0.;

let maxN = 0.8;

let restN = 0.;

let slope
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
    st => {
  let {v, n} = st;
  let leakCurrent = g_L *. (v -. e_L);
  let sodiumCurrent = g_Na *. m_inf v *. (v -. e_Na);
  let potassiumCurrent = g_K *. n *. (v -. e_K);
  let dv = input -. leakCurrent -. sodiumCurrent -. potassiumCurrent;
  let dn = n_inf v -. n;
  {dv, dn}
};

let stepEuler ::t (f: state => deriv) (st: state) :state => {
  let d = f st;
  {v: st.v +. d.dv *. t, n: st.n +. d.dn *. t}
};
