open Reprocessing;

open Hodgkin;

type modelState = {exc1: Neuro.MorrisLecar.state};

type state = {
  modelState,
  input: float,
  modelMonitorLine: Shape.monitorLine,
  inputMonitorLine: Shape.monitorLine
};

let (minInput, maxInput) = (12., 50.);

let setup = (env) => {
  Env.size(~width=600, ~height=400, env);
  {
    modelState: {exc1: Neuro.MorrisLecar.initState},
    input: 0.,
    modelMonitorLine:
      Neuro.MorrisLecar.{
        min: minV,
        max: maxV,
        color: Utils.lerpColor(~low=Color.red, ~high=Color.white, ~value=0.2),
        strokeWeight: 2,
        points: [||],
        maxPointCount: 400
      },
    inputMonitorLine: {
      min: minInput,
      max: maxInput,
      color: Color.grayScale(~a=0.8, 0.5),
      strokeWeight: 4,
      points: [||],
      maxPointCount: 400
    }
  }
};

let draw = (state, env) => {
  let (frameWidth, frameHeight) = (Env.width(env), Env.height(env));
  let mouseIsDown = Env.mousePressed(env);
  let {modelMonitorLine, input, inputMonitorLine} = state;
  let {exc1} = state.modelState;
  let neuronSize = 150.;
  Draw.background(Constants.black, env);
  Util.withContext(
    () => {
      Draw.translate(~x=10., ~y=float_of_int(frameHeight) -. 90., env);
      Shape.(
        monitor(
          ~width=float_of_int(frameWidth - 20),
          ~height=80.,
          [inputMonitorLine, modelMonitorLine],
          env
        )
      )
    },
    env
  );
  Util.withContext(
    () => {
      Draw.translate(
        ~x=float_of_int(frameWidth / 2),
        ~y=float_of_int(frameHeight / 2),
        env
      );
      Neuro.drawMorrisLecar(~size=neuronSize, exc1, env)
    },
    env
  );
  let staticInput = minInput;
  let rampedInput =
    if (mouseIsDown) {
      min(maxInput, input +. 0.03)
    } else {
      staticInput
    };
  let nextExc1 =
    Neuro.MorrisLecar.(
      Util.iterN(20, exc1, stepRK4(slope(~input=rampedInput), ~t=0.004))
    );
  {
    modelState: {exc1: nextExc1},
    input: rampedInput,
    modelMonitorLine: Shape.appendDatum(modelMonitorLine, exc1.v),
    inputMonitorLine: Shape.appendDatum(inputMonitorLine, rampedInput)
  }
};

run(~setup, ~draw, ());
