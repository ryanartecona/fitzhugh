open Reprocessing;

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
