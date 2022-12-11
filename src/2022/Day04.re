let parse =
  String.splitArray(~delimiter=",")
  >> Array.map(
    String.splitArray(~delimiter="-")
    >> Array.map(Int.fromString >> Option.getOrThrow)
    >> Shared.Array.toTuple2)
  >> Shared.Array.toTuple2;

/*
$ node _build/default/src/2022/Day04.bs.js
[
  [ [ 2, 4 ], [ 6, 8 ] ],
  [ [ 2, 3 ], [ 4, 5 ] ],
  [ [ 5, 7 ], [ 7, 9 ] ],
  [ [ 2, 8 ], [ 3, 7 ] ],
  [ [ 6, 6 ], [ 4, 6 ] ],
  [ [ 2, 6 ], [ 4, 8 ] ]
]
*/

let fullyContains = ((a, b)) => {
  let doesAContainB = (((a1, a2), (b1, b2))) =>
    a1 <= b1 && a2 >= b2;

  doesAContainB((a, b)) || doesAContainB((b, a));
}

let overlaps = (((a1, a2), (b1, b2))) => {
  (a1 <= b2 && a2 >= b1) || (b1 <= a2 && b2 >= a1)
};

let doWork = (description, comparison, data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.filter(parse >> comparison)
  |> Array.count
  |> Int.toString
  |> Shared.Log.logWithDescription(description);

Shared.File.read("data/2022/day04test.txt")
|> doWork("Part 1 Test  ", fullyContains);

Shared.File.read("data/2022/day04.txt")
|> doWork("Part 1 Result", fullyContains);

Shared.File.read("data/2022/day04test.txt")
|> doWork("Part 2 Test  ", overlaps);

Shared.File.read("data/2022/day04.txt")
|> doWork("Part 2 Result", overlaps);

/*
$ node _build/default/src/2022/Day04.bs.js
Part 1 Test   : 2
Part 1 Result : 576
Part 2 Test   : 4
Part 2 Result : 905
*/