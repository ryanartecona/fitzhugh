type state = {
  v: float,
  w: float
};

type deriv = {
  dv: float,
  dw: float
};

let initState = {v: 0.5, w: 0.5};

let slope ::a=(-0.1) ::b=0.01 ::c=0.02 ::input=0. st => {
  let {v, w} = st;
  let dv = v *. (a -. v) *. (v -. 1.) -. w +. input;
  let dw = b *. v -. c *. w;
  {dv, dw}
};

let stepEuler ::t (f: state => deriv) (st: state) :state => {
  let d = f st;
  {v: st.v +. d.dv *. t, w: st.w +. d.dw *. t}
};
