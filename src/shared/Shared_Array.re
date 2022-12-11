let toTuple2 =
  fun
  | [|a, b|] => (a, b)
  | _ => raise(Failure("Cannot convert array to Tuple2 - invalid input"));

let toTuple3 =
  fun
  | [|a, b, c|] => (a, b, c)
  | _ => raise(Failure("Cannot convert array to Tuple3 - invalid input"));