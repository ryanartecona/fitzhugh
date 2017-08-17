open Reprocessing;

open Hodgkin;

type modelState = {
  exc1: Neuro.Fitzhugh.state,
  exc2: Neuro.Fitzhugh.state,
  inh1: Neuro.Fitzhugh.state,
  inh2: Neuro.Fitzhugh.state
};

type state = {modelState};

let setup env => {
  Env.size width::800 height::500 env;
  {
    modelState: {
      exc1: Neuro.Fitzhugh.initState,
      exc2: Neuro.Fitzhugh.initState,
      inh1: Neuro.Fitzhugh.initState,
      inh2: Neuro.Fitzhugh.{v: 0.1, w: 0.0}
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
  Util.withContext (
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
  Util.withContext (
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
    Neuro.Fitzhugh.(
      step a::0.1 input::(userInput +. exc2.v *. 0.01) tau::1. exc1
    );
  let nextExc2 =
    Neuro.Fitzhugh.(
      step a::0.1 input::(staticInput +. exc1.v *. 0.01) tau::1. exc2
    );
  let nextInh1 =
    Neuro.Fitzhugh.(
      step a::0.1 input::(userInput -. inh2.v *. 0.01) tau::1. inh1
    );
  let nextInh2 =
    Neuro.Fitzhugh.(
      step a::0.1 input::(staticInput -. inh1.v *. 0.01) tau::1. inh2
    );
  {
    modelState: {exc1: nextExc1, exc2: nextExc2, inh1: nextInh1, inh2: nextInh2}
  }
};

run ::setup ::draw ();
