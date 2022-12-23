let parse =
  String.splitList(~delimiter="\n")
  >> Shared.List.split(~delimiter="")
  >> List.map(
    List.map(Int.fromString >> Option.getOrThrow)
    >> List.foldLeft((+), 0)
  );

let doWork = partFunc =>
  parse
  >> partFunc
  >> Int.toString;

let part1 =
  List.maxBy(Int.compare)
  >> Option.getOrThrow;

let part2 =
  List.sortBy(Int.compare |> Ord.reverse)
  >> List.take(3)
  >> List.Int.sum;

let testData = "data/2022/day01test.txt";
let problemData = "data/2022/day01.txt";

Shared.IO.readRunLogAll(
  ~testData,
  ~problemData,
  ~part1=doWork(part1),
  ~part2=doWork(part2),
  ()
);

/*
$ node _build/default/src/2022/Day01.bs.js
Part 1 Test   : 24000
Part 1 Result : 72017
Part 2 Test   : 45000
Part 2 Result : 212520
*/