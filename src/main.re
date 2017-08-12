open Reprocessing;

module Color = {
  let gray: colorT = {r: 0x88, g: 0x88, b: 0x88};
  let darkGray: colorT = {r: 0x55, g: 0x55, b: 0x55};
  let orange: colorT = {r: 0xCC, g: 0x55, b: 0x11};
};

module Helper = {
  let withContext f env => {
    Draw.pushStyle env;
    Draw.pushMatrix env;
    f ();
    Draw.popStyle env;
    Draw.popMatrix env
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
  let draw ::squareSize=200. state env =>
    env |>
    Helper.withContext (
      fun () => {
        env |>
        Helper.withContext (
          fun () => {
            Draw.fill Color.darkGray env;
            let scale = 1. +. max 0. (-. state.v);
            let size = squareSize *. scale;
            Helper.drawRectfCenter width::size height::size env
          }
        );
        Draw.fill Color.gray env;
        Helper.drawRectfCenter width::squareSize height::squareSize env;
        let neuroColor = colorOfState state;
        env |>
        Helper.withContext (
          fun () => {
            Draw.fill neuroColor env;
            let scale = max 0. state.v;
            let size = squareSize *. 0.1 +. squareSize *. scale;
            Helper.drawRectfCenter width::size height::size env
          }
        )
      }
    );
};

type modelState = {
  exc1: Neuro.state,
  exc2: Neuro.state,
  inh1: Neuro.state,
  inh2: Neuro.state
};

type state = {modelState};

let setup env => {
  Env.size width::800 height::500 env;
  {
    modelState: {
      exc1: Neuro.initState,
      exc2: Neuro.initState,
      inh1: Neuro.initState,
      inh2: {v: 0.1, w: 0.0}
    }
  }
};

let draw state env => {
  let (frameWidth, frameHeight) = (Env.width env, Env.height env);
  let mouseIsDown = Env.mousePressed env;
  let {exc1, exc2, inh1, inh2} = state.modelState;
  let squareSize = 150.;
  Draw.background Constants.black env;
  env |>
  Helper.withContext (
    fun () => {
      /* draw excitatory pair on left */
      Draw.translate
        x::(float_of_int (frameWidth / 4))
        y::(float_of_int (frameHeight / 4))
        env;
      Neuro.draw ::squareSize exc1 env;
      Draw.translate x::0. y::(float_of_int (frameHeight * 2 / 4)) env;
      Neuro.draw ::squareSize exc2 env
    }
  );
  env |>
  Helper.withContext (
    fun () => {
      /* draw inhibitory pair on right */
      Draw.translate
        x::(float_of_int (frameWidth * 3 / 4))
        y::(float_of_int (frameHeight / 4))
        env;
      Neuro.draw ::squareSize inh1 env;
      Draw.translate x::0. y::(float_of_int (frameHeight * 2 / 4)) env;
      Neuro.draw ::squareSize inh2 env
    }
  );
  let staticInput = 0.04;
  let userInput = if mouseIsDown {(-0.04)} else {staticInput};
  let nextExc1 =
    Neuro.step a::0.1 input::(userInput +. exc2.v *. 0.01) tau::1. exc1;
  let nextExc2 =
    Neuro.step a::0.1 input::(staticInput +. exc1.v *. 0.01) tau::1. exc2;
  let nextInh1 =
    Neuro.step a::0.1 input::(userInput -. inh2.v *. 0.01) tau::1. inh1;
  let nextInh2 =
    Neuro.step a::0.1 input::(staticInput -. inh1.v *. 0.01) tau::1. inh2;
  {
    modelState: {exc1: nextExc1, exc2: nextExc2, inh1: nextInh1, inh2: nextInh2}
  }
};

run ::setup ::draw ();
