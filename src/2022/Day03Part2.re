let findSharedItem = ((a, b, c)) => {
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

let charCodeToPriority = x =>
  switch(x) {
  | x when x >= 97 && x <= 122 => x - 96
  | x when x >= 65 && x <= 90  => x - 38
  | _ => raise(Failure("invalid character code"))
  };

let chunkToPriority = 
  Shared.Array.arrayToTuple3
  >> findSharedItem
  >> String.charCodeAt(0)
  >> Option.getOrThrow
  >> charCodeToPriority;

let doWork = (description, data) => 
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.chunk(3)
  |> Array.map(chunkToPriority)
  |> Array.Int.sum
  |> Int.toString
  |> Shared.Log.logWithDescription(description);

Shared.File.read("data/2022/day03test.txt")
|> doWork("Test  ");

Shared.File.read("data/2022/day03.txt")
|> doWork("Result");

/*
$ node _build/default/src/2022/Day03Part2.bs.js
Test   : 70
Result : 2581
*/