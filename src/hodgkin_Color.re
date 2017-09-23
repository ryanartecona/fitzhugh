open Reprocessing.Constants;

open Reprocessing.Utils;

let whiteA a => color r::0xFF g::0xFF b::0xFF ::a ();

let blackA a => color r::0x00 g::0x00 b::0x00 ::a ();

let grayScale ::a=1. x => lerpColor low::(whiteA a) high::(blackA a) value::x;

let white = whiteA 1.;

let black = blackA 1.;

let gray = grayScale 0.5;

let darkGray = grayScale 0.7;

let orange = color r::0xCC g::0x55 b::0x11 ();

let redA a => color r::0xCC g::0x22 b::0x55 ::a ();

let red = redA 1.;

let blueA a => color r::0x55 g::0x99 b::0xCC ::a ();

let blue = blueA 1.;
