let parse =
  String.splitList(~delimiter="\n")
  >> Shared.List.split(~delimiter="")
  >> List.map(
    List.map(int_of_string)
    >> List.foldLeft((+), 0)
  );

let log = (description, data) =>
  data
  |> Int.toString
  |> (++)(description ++ " : ")
  |> Js.log;

let doWork = (description, partFunc, data) =>
  data
  |> parse
  |> partFunc
  |> log(description);

let part1 =
  List.maxBy(Int.compare)
  >> Option.getOrThrow;

let part2 =
  List.sortBy(Int.compare |> Ord.reverse)
  >> List.take(3)
  >> List.Int.sum;

Shared.File.read("data/2022/day01test.txt")
|> doWork("Part 1 Test  ", part1);

Shared.File.read("data/2022/day01.txt")
|> doWork("Part 1 Result", part1);

Shared.File.read("data/2022/day01test.txt")
|> doWork("Part 2 Test  ", part2);

Shared.File.read("data/2022/day01.txt")
|> doWork("Part 2 Result", part2);

/*
$ node _build/default/src/2022/Day01.bs.js
Part 1 Test   : 24000
Part 1 Result : 72017
Part 2 Test   : 45000
Part 2 Result : 212520
*/