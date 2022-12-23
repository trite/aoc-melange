let findSharedItem = ((a, b)) =>
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
    |> findSharedItem
    |> String.charCodeAt(0)
    |> Option.getOrThrow
    |> charCodeToPriority;

let doWork = 
  String.splitArray(~delimiter="\n")
  >> Array.map(getPriority)
  >> Array.Int.sum
  >> Int.show;

let testData = "data/2022/day03test.txt";
let problemData = "data/2022/day03.txt";

Shared.IO.readFile(testData)
|> IO.map(doWork)
|> Shared.IO.unsafeRunAndLog("Part 1 Test  ");

Shared.IO.readFile(problemData)
|> IO.map(doWork)
|> Shared.IO.unsafeRunAndLog("Part 1 Result");

/*
$ node _build/default/src/2022/Day03Part1.bs.js
Test   : 157
Result : 7850
*/