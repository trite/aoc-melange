let doWork = (description, uniquePart, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> Shared.List.split(~delimiter="")
  |> List.map(
    List.map(int_of_string)
    >> List.foldLeft((+), 0)
  )
  |> uniquePart
  |> Int.toString
  |> (++)(description ++ " : ")
  |> Js.log;

let part1 =
  List.maxBy(Int.compare)
  >> Option.getOrThrow;

let part2 =
  List.sortBy(Int.compare |> Ord.reverse)
  >> List.take(3)
  >> List.Int.sum;

Shared.File.read("data/day01test.txt")
|> doWork("Part 1 Test  ", part1);

Shared.File.read("data/day01.txt")
|> doWork("Part 1 Result", part1);

Shared.File.read("data/day01test.txt")
|> doWork("Part 2 Test  ", part2);

Shared.File.read("data/day01.txt")
|> doWork("Part 2 Result", part2);