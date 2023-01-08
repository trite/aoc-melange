// Int.rangeAsArray(-5, 10)
// |> Array.map(y =>
//      Int.rangeAsArray(-10, 5)
//      |> Array.map(x => {j|($x,$y)|j})
//      |> Array.String.joinWith(" ")
//    )
// |> Array.reverse
// |> Array.String.joinWith("\n")
// |> Js.log;

// Int.rangeAsArray(-5, 10)
// |> Array.map(_ =>
//      Int.rangeAsArray(-10, 5)
//      |> Array.map(_ => ".")
//      |> Array.String.joinWith(" ")
//    )
// |> Array.reverse
// |> Array.String.joinWith("\n")
// |> Js.log;

let test =
  [((-5), 0), (0, (-5)), (5, 0), (0, 5)] |> List.map(Position.fromTuple);

test |> Position.List.min |> Js.log;
test |> Position.List.max |> Js.log;
