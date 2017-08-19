open Reprocessing;

open Hodgkin;

type modelState = {exc1: Neuro.MorrisLecar.state};

type state = {
  modelState,
  input: float
};

let setup env => {
  Env.size width::800 height::500 env;
  {modelState: {exc1: Neuro.MorrisLecar.initState}, input: 0.}
};

let draw state env => {
  let (frameWidth, frameHeight) = (Env.width env, Env.height env);
  let mouseIsDown = Env.mousePressed env;
  let {input} = state;
  let {exc1} = state.modelState;
  let neuronSize = 150.;
  Draw.background Constants.black env;
  env |>
  Util.withContext (
    fun () => {
      Draw.translate
        x::(float_of_int (frameWidth / 2))
        y::(float_of_int (frameHeight / 2))
        env;
      Neuro.drawMorrisLecar size::neuronSize exc1 env
    }
  );
  let staticInput = 12.0;
  let rampedInput =
    if mouseIsDown {
      input +. 0.03
    } else {
      staticInput
    };
  let nextExc1 =
    Util.iterN 80 exc1 Neuro.MorrisLecar.(step input::rampedInput t::0.001);
  {modelState: {exc1: nextExc1}, input: rampedInput}
};

run ::setup ::draw ();
