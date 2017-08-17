type state = {
  v: float,
  n: float
};

let initState = {v: 0.5, n: 0.5};

let step
    ::g_L=8.
    ::e_L=(-80.)
    ::g_Na=20.
    ::e_Na=60.
    ::g_K=10.
    ::e_K=(-90.)
    ::m_inf
    ::n_inf
    ::input=0.
    ::tau=1.
    st => {
  let {v, n} = st;
  let leakCurrent = g_L *. (v -. e_L);
  let sodiumCurrent = g_Na *. m_inf v *. (v -. e_Na);
  let potassiumCurrent = g_K *. n *. (v -. e_K);
  let dv = input -. leakCurrent -. sodiumCurrent -. potassiumCurrent;
  let dn = n_inf v -. n;
  {v: v +. dv *. tau, n: n +. dn *. tau}
};
