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
    module SimArray1 = Hodgkin.Neuro.Sim(ArrayODE1);
    let testSimBasic = (simStep, ()) => {
      let t = check(array(float(0.0001)));
      t(
        "takes a linear step",
        [|1.2|],
        simStep(~t=0.1, (_) => [|2.|], [|1.|])
      );
      t("takes a quadratic step", [|8.|], simStep(~t=1., (x) => x, [|4.|]))
    };
    let test_stepEuler = testSimBasic(SimArray1.stepEuler);
    let test_stepRKEuler = testSimBasic(SimArray1.stepRKEuler);
    let test_stepRK4 = testSimBasic(SimArray1.stepRK4);
    let test_stepDoPri = testSimBasic(SimArray1.stepDoPri);
    let tests = (
      "Hodgkin.Neuro",
      [
        ("stepEuler", `Quick, test_stepEuler),
        ("stepRKEuler", `Quick, test_stepRKEuler),
        ("stepRK4", `Quick, test_stepRK4),
        ("stepDoPri", `Quick, test_stepDoPri)
      ]
    );
  };
};

let util_tests =
  run("hodgkin", HodgkinTests.[("hello", [qed]), Util.tests, Neuro.tests]);
