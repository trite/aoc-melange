let findSharedItemP1 = ((a, b)) =>
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

let getPriorityP1 = x =>
    x
    |> String.splitAt((x |> String.length) / 2)
    |> findSharedItemP1
    |> String.charCodeAt(0)
    |> Option.getOrThrow
    |> charCodeToPriority;

let part1 =
  Array.map(getPriorityP1);

let doWork = (part) =>
  String.splitArray(~delimiter="\n")
  >> part
  >> Array.Int.sum
  >> Int.show;

let findSharedItemP2 = ((a, b, c)) => {
  let findSharedItems = (a, b) =>
    a
    |> String.toArray
    |> Array.filter(x =>
      b
      |> String.toArray
      |> Array.containsBy(String.eq, x))
    |> Array.distinctBy(String.eq)
    |> Array.String.join;

  a
  |> findSharedItems(b)
  |> findSharedItems(c);
}

let chunkToPriority = 
  Shared.Array.toTuple3
  >> findSharedItemP2
  >> String.charCodeAt(0)
  >> Option.getOrThrow
  >> charCodeToPriority;

let part2 =
  Array.chunk(3)
  >> Array.map(chunkToPriority);

let testData = "data/2022/day03test.txt";
let problemData = "data/2022/day03.txt";

Shared.IO.readRunLogAll(
  ~testData,
  ~problemData,
  ~part1=doWork(part1),
  ~part2=doWork(part2),
  ()
);

/*
$ node _build/default/src/2022/Day03.bs.js
Part 1 Test   : 157
Part 1 Result : 7850
Part 2 Test   : 70
Part 2 Result : 2581
*/