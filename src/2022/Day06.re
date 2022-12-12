let parseTestData =
  String.splitList(~delimiter="\n")
  >> List.map(
    String.splitList(~delimiter=" ")
    >> Shared.List.toTuple2);

let (^::) = List.cons;

let tails = lst => {
  // List.scanRight((lstIn, lstAcc) => List.drop(1, lstIn) ^:: lstAcc, [], lst)
  let rec go = (lst) =>
    switch(lst) {
    | [] => []
    | x => {
      let newX = x |> List.drop(1);
      newX ^:: go(newX)}
    };

  lst ^:: go(lst)
};

// TODO: need a filterMap implementation to make this less ugly
let windows = (n, xs) =>
  List.map(List.takeExactly(n), tails(xs))
  |> List.filter(Option.isSome)
  |> List.map(Option.getOrThrow);

let unique = lst => {
  let oLen = lst |> List.length;
  let nLen = lst |> List.distinctBy(String.eq) |> List.length;
  oLen == nLen
};

let firstUniqueWithCount = (x, lst) => {
  let rec go = (lst, count) =>
    switch(lst) {
    | [] => raise(Failure("Shouldn't reach the end of the list without finding the answer"))
    | [subList, ...rest] =>
      unique(subList)
      ? x + count
      : go(rest, count+1)
    }
  
  go(lst, 0)
};

let doWork = (description, count, s) =>
  s
  |> String.toList
  |> windows(count)
  |> firstUniqueWithCount(count)
  |> Int.toString
  |> Shared.Log.logWithDescription(description);

// Should update the test data to also validate these answers eventually
Shared.File.read("data/2022/day06test.txt")
|> parseTestData
|> List.forEach(
  Tuple.second
  >> doWork("Part 1 Test  ", 4));

Shared.File.read("data/2022/day06.txt")
|> doWork("Part 1 Result", 4);

Shared.File.read("data/2022/day06test.txt")
|> parseTestData
|> List.forEach(
  Tuple.second
  >> doWork("Part 2 Test  ", 14));

Shared.File.read("data/2022/day06.txt")
|> doWork("Part 2 Result", 14);