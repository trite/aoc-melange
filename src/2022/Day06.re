let parseTestData =
  String.splitList(~delimiter="\n")
  >> List.map(
    String.splitList(~delimiter=" ")
    >> Shared.List.toTuple3);

let (^::) = List.cons;

let tails = lst => {
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
    | [] => raise(Failure(
      "Reached end of list without finding anything unique!"))
    | [subList, ...rest] =>
      unique(subList)
      ? x + count
      : go(rest, count+1)
    }
  
  go(lst, 0)
};

let doWork = (count, s) =>
  s
  |> String.toList
  |> windows(count)
  |> firstUniqueWithCount(count)
  |> Int.toString;

let testData = "data/2022/day06test.txt";
let problemData = "data/2022/day06.txt";

let runTest = (work, partNum, (p1Answer, p2Answer, data)) =>{
  let result = data |> work;
  let answer =
    switch(partNum) {
    | 1 => p1Answer
    | 2 => p2Answer
    | _ => raise(Failure(
      "This shouldn't be a throw, but being lazy for now"))
    };

  result == answer
  ? Ok(result)
  : Error(
    {j|Something went wrong.
  Expected answer: $answer
  Actual result  : $result
    |j}
  )
};

Shared.IO.readRunLogAll(
  ~testData,
  ~problemData,
  ~part1=doWork(4),
  ~part2=doWork(14),
  ~testStyle=Shared.IO.MultipleTests((fileText, work, partNum) =>
    fileText
    |> parseTestData
    |> List.toArray
    |> Array.map(runTest(work, partNum))
  ),
  ()
);

/*
$ node _build/default/src/2022/Day06.bs.js
Part 1 Test (Success) : 7
Part 1 Test (Success) : 5
Part 1 Test (Success) : 6
Part 1 Test (Success) : 10
Part 1 Test (Success) : 11

Part 1 Result : 1760

Part 2 Test (Success) : 19
Part 2 Test (Success) : 23
Part 2 Test (Success) : 23
Part 2 Test (Success) : 29
Part 2 Test (Success) : 26

Part 2 Result : 2974
*/