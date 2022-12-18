// Part 1
let getGridDimensions = (grid: array(array(int))) =>
  (
    grid |> Array.head |> Option.getOrThrow |> Array.length,
    grid |> Array.length,
  );

let getItemAt = (x, y, grid) =>
  grid
  |> Array.at(y)
  |> Option.flatMap(Array.at(x))
  |> fun
     | None => raise(Failure({j|Failed to find element at ($x, $y) \n $grid|j}))
     | Some(x) => x;

let getItemsDirection = (~fx, ~fy, ~switchOn, ~bailOn, x, y, grid) => {
  let rec go = (x, y, grid) => 
    (x, y) |> switchOn == bailOn
    ? [grid |> getItemAt(x, y)]
    : [grid |> getItemAt(x, y), ...(grid |> go(fx(x), fy(y)))];

  (
    (x, y) |> switchOn == bailOn
    ? []
    : go(fx(x), fy(y), grid)
  )
  |> List.toArray
};

let sub1 = (-)(_, 1);
let add1 = (+)(1);

let getItemsAbove = getItemsDirection(~fx=id, ~fy=sub1, ~switchOn=Tuple.second);
let getItemsBelow = getItemsDirection(~fx=id, ~fy=add1, ~switchOn=Tuple.second);
let getItemsLeft = getItemsDirection(~fx=sub1, ~fy=id, ~switchOn=Tuple.first);
let getItemsRight = getItemsDirection(~fx=add1, ~fy=id, ~switchOn=Tuple.first);

let getLen = side =>
  getGridDimensions
  >> side
  >> sub1;

let getAllRelevantItems = (x, y, grid) =>
  [|
    getItemsAbove(~bailOn=0, x, y, grid),
    getItemsBelow(~bailOn=grid |> getLen(Tuple.second), x, y, grid),
    getItemsLeft(~bailOn=0, x, y, grid),
    getItemsRight(~bailOn=grid |> getLen(Tuple.first), x, y, grid)
  |];

let isSpotVisible = (x, y, grid) => {
  let treeToExamine = grid |> getItemAt(x, y);

  grid
  |> getAllRelevantItems(x, y)
  |> Array.map(toCompare => toCompare |> Array.all(i => i < treeToExamine))
  |> Array.any(id)
  ? 1
  : 0
};
  
let part1 = grid => {
  let (lenX, lenY) = grid |> getGridDimensions;

  Int.rangeAsArray(0, lenX)
  |> Array.map(x =>
    Int.rangeAsArray(0, lenY)
    |> Array.map(y =>
      grid |> isSpotVisible(x, y)
    )
    |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
  )
  |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
};

// Part 2

// Program boundry IO

let run = (description, partSpecificStuff, data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.map(
    String.splitArray(~delimiter="")
    >> Array.map(
      String.toInt
      >> Option.getOrThrow))
  |> partSpecificStuff
  |> Int.toString
  |> Shared.Log.logWithDescription(description);

Shared.File.read("data/2022/day08test.txt")
|> run("Part 1 Test  ", part1);

Shared.File.read("data/2022/day08.txt")
|> run("Part 1 Result", part1);

// Shared.File.read("data/2022/day08test.txt")
// |> doWork("Part 2 Test  ", part2);

// Shared.File.read("data/2022/day08.txt")
// |> doWork("Part 2 Result", part2);