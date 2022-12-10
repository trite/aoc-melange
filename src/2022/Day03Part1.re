let findSharedItems = ((a, b)) =>
      a
      |> String.toArray
      |> Array.filter(x =>
        b
        |> String.toArray
        |> Array.containsBy(String.eq, x))
      |> Array.head
      |> Option.getOrThrow;

let charCodeToPriority = x =>
  switch(x) {
  | x when x >= 97 && x <= 122 => x - 96
  | x when x >= 65 && x <= 90  => x - 38
  | _ => raise(Failure("invalid character code"))
  };

// 'a' |> Char.code |> charCodeToPriority |> Js.log; // 1
// 'z' |> Char.code |> charCodeToPriority |> Js.log; // 26
// 'A' |> Char.code |> charCodeToPriority |> Js.log; // 27
// 'Z' |> Char.code |> charCodeToPriority |> Js.log; // 52

let getPriority = x =>
    x
    |> String.splitAt((x |> String.length) / 2)
    |> findSharedItems
    |> String.charCodeAt(0)
    |> Option.getOrThrow
    |> charCodeToPriority;

let doWork = (description, data) => 
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.map(getPriority)
  |> Array.Int.sum
  |> Int.show
  |> Shared.Log.logWithDescription(description);

Shared.File.read("data/2022/day03test.txt")
|> doWork("Test  ");

Shared.File.read("data/2022/day03.txt")
|> doWork("Result");

/*
$ node _build/default/src/2022/Day03Part1.bs.js
Test   : 157
Result : 7850
*/