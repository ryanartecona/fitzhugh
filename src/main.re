open Reprocessing;

module Color = {
  let gray: colorT = {r: 0x88, g: 0x88, b: 0x88};
  let darkGray: colorT = {r: 0x55, g: 0x55, b: 0x55};
  let orange: colorT = {r: 0xCC, g: 0x55, b: 0x11};
};

module Helper = {
  let withStyle f env => {
    Draw.pushStyle env;
    f ();
    Draw.popStyle env
  };
  let drawRectCenter ::width ::height env =>
    Draw.rect pos::(- width / 2, - height / 2) ::width ::height env;
  let drawRectfCenter ::width ::height env =>
    Draw.rectf pos::(-. width /. 2., -. height /. 2.) ::width ::height env;
};

module Neuro = {
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
  let colorOfState st :colorT => {
    r: 0xCC - int_of_float (float_of_int 0x77 *. st.w *. 5.),
    g: 0x22 + int_of_float (float_of_int 0x77 *. st.w *. 5.),
    b: 0x55 + int_of_float (float_of_int 0x77 *. st.w *. 5.)
  };
};

type state = {
  rotation: float,
  modelState: Neuro.state
};

let setup env => {
  Env.size width::1200 height::700 env;
  {rotation: 0., modelState: Neuro.initState}
};

let draw state env => {
  let (frameWidth, frameHeight) = (Env.width env, Env.height env);
  let squareSize = 200.;
  Draw.background Constants.black env;
  env |>
  Helper.withStyle (
    fun () => {
      Draw.strokeWeight 0 env;
      Draw.translate
        x::(float_of_int (frameWidth / 2))
        y::(float_of_int (frameHeight / 2))
        env;
      Draw.rotate state.rotation env;
      env |>
      Helper.withStyle (
        fun () => {
          Draw.fill Color.darkGray env;
          let scale = 1. +. max 0. (-. state.modelState.v);
          let size = squareSize *. scale;
          Helper.drawRectfCenter width::size height::size env
        }
      );
      Draw.fill Color.gray env;
      Helper.drawRectfCenter width::squareSize height::squareSize env;
      let neuroColor = Neuro.colorOfState state.modelState;
      env |>
      Helper.withStyle (
        fun () => {
          Draw.fill neuroColor env;
          let scale = max 0. state.modelState.v;
          let size = squareSize *. 0.1 +. squareSize *. scale;
          Helper.drawRectfCenter width::size height::size env
        }
      )
    }
  );
  let frameRate = Env.frameRate env;
  let rotateRate = Constants.pi /. 4.;
  let nextModelState = Neuro.step a::0.1 input::0.04 tau::1. state.modelState;
  {
    rotation: state.rotation +. rotateRate /. float_of_int frameRate,
    modelState: nextModelState
  }
};

run ::setup ::draw ();
