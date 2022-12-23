let parse =
  String.splitArray(~delimiter=",")
  >> Array.map(
    String.splitArray(~delimiter="-")
    >> Array.map(Int.fromString >> Option.getOrThrow)
    >> Shared.Array.toTuple2)
  >> Shared.Array.toTuple2;

let fullyContains = ((a, b)) => {
  let doesAContainB = (((a1, a2), (b1, b2))) =>
    a1 <= b1 && a2 >= b2;

  doesAContainB((a, b)) || doesAContainB((b, a));
}

let overlaps = (((a1, a2), (b1, b2))) => {
  (a1 <= b2 && a2 >= b1) || (b1 <= a2 && b2 >= a1)
};

let doWork = (comparison, data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.filter(parse >> comparison)
  |> Array.count
  |> Int.toString;

let testData = "data/2022/day04test.txt";

let problemData = "data/2022/day04.txt";

Shared.IO.readRunLogAll(
  ~testData,
  ~problemData,
  ~part1=doWork(fullyContains),
  ~part2=doWork(overlaps),
  ()
);

/*
$ node _build/default/src/2022/Day04.bs.js
Part 1 Test   : 2
Part 1 Result : 576
Part 2 Test   : 4
Part 2 Result : 905
*/