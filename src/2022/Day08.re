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

let getItemsDirection = (fx, fy, switchOn, bailOn, x, y, grid) => {
  let rec go = (x, y, grid) => {
    switch((x, y) |> switchOn) {
    | _discard when _discard == bailOn => [grid |> getItemAt(x, y)]
    | _ => [grid |> getItemAt(x, y), ...(grid |> go(fx(x), fy(y)))]
    }
  }

  switch((x, y) |> switchOn) {
  | _discard when _discard == bailOn => []
  | _ => go(fx(x), fy(y), grid)
  }
  |> List.toArray
};

let sub1 = (-)(_, 1);
let add1 = (+)(1);

let getItemsAbove = getItemsDirection(id, sub1, Tuple.second);
let getItemsBelow = getItemsDirection(id, add1, Tuple.second);
let getItemsLeft = getItemsDirection(sub1, id, Tuple.first);
let getItemsRight = getItemsDirection(add1, id, Tuple.first);

// let arrayMax = Array.length >> sub1;
let getLen = side =>
  getGridDimensions
  >> side
  >> sub1;

let getAllRelevantItems = (x, y, grid) =>
  [|
    getItemsAbove(0, x, y, grid),
    getItemsBelow(grid |> getLen(Tuple.second), x, y, grid),
    getItemsLeft(0, x, y, grid),
    getItemsRight(grid |> getLen(Tuple.first), x, y, grid)
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