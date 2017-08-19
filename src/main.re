open Reprocessing;

open Hodgkin;

type modelState = {exc1: Neuro.MorrisLecar.state};

type state = {modelState};

let setup env => {
  Env.size width::800 height::500 env;
  {modelState: {exc1: Neuro.MorrisLecar.initState}}
};

let draw state env => {
  let (frameWidth, frameHeight) = (Env.width env, Env.height env);
  let mouseIsDown = Env.mousePressed env;
  let {exc1} = state.modelState;
  let squareSize = 150.;
  Draw.background Constants.black env;
  env |>
  Util.withContext (
    fun () => {
      /* draw excitatory pair on left */
      Draw.translate
        x::(float_of_int (frameWidth / 2))
        y::(float_of_int (frameHeight / 2))
        env;
      Neuro.drawMorrisLecar ::squareSize exc1 env
    }
  );
  let staticInput = 5.0;
  let userInput = if mouseIsDown {(-0.00)} else {staticInput};
  let nextExc1 = Neuro.MorrisLecar.(step input::userInput t::0.1 exc1);
  {modelState: {exc1: nextExc1}}
};

run ::setup ::draw ();
