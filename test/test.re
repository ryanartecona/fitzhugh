open Alcotest;

let qed = ("QED.", `Quick, () => check(int, "0=0", 0, 0));

module HodgkinTests = {
  module Util = {
    let test_iterN = () => {
      let count = Hodgkin.Util.iterN(20, 0, (x) => x + 10);
      check(int, "counted 20 times", count, 200)
    };
    let tests = ("Hodgkin.Util", [("iterN", `Quick, test_iterN)]);
  };
  module Neuro = {
    module type N = {let n: int;};
    module ArrayODE = (N: N) => {
      type state = array(float);
      type deriv = array(float);
      let zero_deriv: deriv = Array.make(N.n, 0.);
      let scale_deriv = (d, s) => Array.map((x) => x *. s, d);
      let add_derivs = (d1, d2) => Array.mapi((i, d1_i) => d1_i +. d2[i], d1);
      let step = (st, d, t) => Array.mapi((i, st_i) => st_i +. d[i] *. t, st);
    };
    module ArrayODE1 =
      ArrayODE(
        {
          let n = 1;
        }
      );
    let test_stepEuler = () => {
      let input = [|1.|];
      let output = [|1.2|];
      let f = (_) => [|2.|];
      check(
        array(float(0.0001)),
        "steps a constant function 1 step correctly",
        output,
        Hodgkin.Neuro.stepEuler(~t=0.1, f, input)
      )
    };
    module SimArray1 = Hodgkin.Neuro.Sim(ArrayODE1);
    let test_stepRKEuler = () => {
      let input = [|1.|];
      let expect = [|1.2|];
      let f = (_) => [|2.|];
      check(
        array(float(0.0001)),
        "steps a constant function 1 step correctly",
        expect,
        SimArray1.stepRKEuler(~t=0.1, f, input)
      )
    };
    let test_stepRK4 = () => {
      let input = [|1.|];
      let output = [|1.2|];
      let f = (_) => [|2.|];
      check(
        array(float(0.0001)),
        "steps a constant function 1 step correctly",
        output,
        SimArray1.stepRK4(~t=0.1, f, input)
      )
    };
    let tests = (
      "Hodgkin.Neuro",
      [
        ("stepEuler", `Quick, test_stepEuler),
        ("stepRKEuler", `Quick, test_stepRKEuler),
        ("stepRK4", `Quick, test_stepRK4)
      ]
    );
  };
};

let util_tests =
  run("hodgkin", HodgkinTests.[("hello", [qed]), Util.tests, Neuro.tests]);
