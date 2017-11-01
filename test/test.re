open Alcotest;

let qed = ("QED.", `Quick, fun () => check int "0=0" 0 0);

module HodgkinTests = {
  module Util = {
    let test_iterN () => {
      let count = Hodgkin.Util.iterN 20 0 (fun x => x + 10);
      check int "counted 20 times" count 200
    };
    let tests = ("Hodgkin.Util", [("iterN", `Quick, test_iterN)]);
  };
  module Neuro = {
    let test_stepEuler () => {
      let input = [|1.|];
      let output = [|1.2|];
      let f _ => [|2.|];
      check
        (array (float 0.0001))
        "steps a constant function 1 step correctly"
        output
        (Hodgkin.Neuro.stepEuler t::0.1 f input)
    };
    let test_stepRKEuler () => {
      let input = [|1.|];
      let expect = [|1.2|];
      let f _ => [|2.|];
      check
        (array (float 0.0001))
        "steps a constant function 1 step correctly"
        expect
        (Hodgkin.Neuro.stepRKEuler t::0.1 f input)
    };
    let test_stepRK4 () => {
      let input = [|1.|];
      let output = [|1.2|];
      let f _ => [|2.|];
      check
        (array (float 0.0001))
        "steps a constant function 1 step correctly"
        output
        (Hodgkin.Neuro.stepRK4 t::0.1 f input)
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
  run "hodgkin" HodgkinTests.[("hello", [qed]), Util.tests, Neuro.tests];
