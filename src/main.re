open Reprocessing;

module Color = {
  let softBlue: colorT = {r: 0x00, g: 0xAA, b: 0xAA};
};

module Helper = {
  let withStyle f env => {
    Draw.pushStyle env;
    f ();
    Draw.popStyle env
  };
};

type state = {rotation: float};

let setup env => {
  Env.size width::1200 height::700 env;
  {rotation: 0.}
};

let draw state env => {
  let (width, height) = (Env.width env, Env.height env);
  Draw.background Constants.black env;
  env |>
  Helper.withStyle (
    fun () => {
      Draw.strokeWeight 0 env;
      Draw.fill Color.softBlue env;
      Draw.translate
        x::(float_of_int (width / 2)) y::(float_of_int (height / 2)) env;
      Draw.rotate state.rotation env;
      Draw.rect pos::((-100), (-100)) width::200 height::200 env
    }
  );
  let frameRate = Env.frameRate env;
  let rotateRate = Constants.pi /. 4.;
  {rotation: state.rotation +. rotateRate /. float_of_int frameRate}
};

run ::setup ::draw ();
