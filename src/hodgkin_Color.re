open Reprocessing.Constants;

open Reprocessing.Utils;

let whiteA = (a) => colorf(~r=1., ~g=1., ~b=1., ~a);

let blackA = (a) => colorf(~r=0., ~g=0., ~b=0., ~a);

let grayScale = (~a=1., x) => lerpColor(~low=whiteA(a), ~high=blackA(a), ~value=x);

let white = whiteA(1.);

let black = blackA(1.);

let gray = grayScale(0.5);

let darkGray = grayScale(0.7);

let orange = colorf(~r=0.8, ~g=0.333, ~b=0.066, ~a=1.);

let redA = (a) => colorf(~r=0.8, ~g=0.133, ~b=0.333, ~a);

let red = redA(1.);

let blueA = (a) => colorf(~r=0.333, ~g=0.6, ~b=0.8, ~a);

let blue = blueA(1.);
