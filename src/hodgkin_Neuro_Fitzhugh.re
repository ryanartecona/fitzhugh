type state = {
  v: float,
  w: float
};

let initState = {v: 0.5, w: 0.5};

let step ::a=(-0.1) ::b=0.01 ::c=0.02 ::input=0. ::tau=1. st => {
  let {v, w} = st;
  let dv = v *. (a -. v) *. (v -. 1.) -. w +. input;
  let dw = b *. v -. c *. w;
  {v: v +. dv *. tau, w: w +. dw *. tau}
};
