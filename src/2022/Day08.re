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

/***
  ~fx: increment/decrement/identity func to apply to the x value when needed
  ~fy: equivalent func for the y value (identity for ignored side)
  ~switchOn: run the (x,y) tuple through this to decide which one to switch on
  ~bailOn: compare to x or y in the switch statement
 */
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

// let sub1 = (-)(_, 1);
let sub1 = (+)(-1);
let add1 = (+)(1);

let getLen = side =>
  getGridDimensions
  >> side
  >> sub1;

// TODO: review later - good use of currying or no?
let getItemsAbove =
  getItemsDirection(
    ~fx=id,
    ~fy=sub1,
    ~switchOn=Tuple.second,
    ~bailOn=0);

let getItemsBelow = (x, y, grid) =>
  getItemsDirection(
    ~fx=id,
    ~fy=add1,
    ~switchOn=Tuple.second,
    ~bailOn=(grid |> getLen(Tuple.second)),
    x, y, grid);

let getItemsLeft =
  getItemsDirection(
    ~fx=sub1,
    ~fy=id,
    ~switchOn=Tuple.first,
    ~bailOn=0);

let getItemsRight = (x, y, grid) =>
  getItemsDirection(
    ~fx=add1, 
    ~fy=id, 
    ~switchOn=Tuple.first,
    ~bailOn=(grid |> getLen(Tuple.first)),
    x, y, grid);

let getAllRelevantItems = (x, y, grid) =>
  [|
    getItemsAbove(x, y, grid),
    getItemsBelow(x, y, grid),
    getItemsLeft(x, y, grid),
    getItemsRight(x, y, grid)
  |];

let isSpotVisible = (x, y, grid) => {
  let treeToExamine = grid |> getItemAt(x, y);

  grid
  |> getAllRelevantItems(x, y)
  |> Array.map(Array.all(i => i < treeToExamine))
  |> Array.any(id)
  ? 1
  : 0
};

let calculateScenicScore = (x, y, grid) => {
  let treeToExamine = grid |> getItemAt(x, y);
    
  let rec doTheThing = (lst) => {
    switch(lst |> List.head) {
    | None => []
    | Some(i) when i >= treeToExamine => [i]
    | Some(i) => [i, ...doTheThing(lst |> List.tailOrEmpty)]
    }
  };
  
  grid
  |> getAllRelevantItems(x, y)
  |> Array.map(arr =>
    arr
    |> Array.toList
    |> doTheThing
    |> List.length
  )
  |> Array.Int.product
};

let mapGrid = (toMap, followUp, grid) => {
  let (lenX, lenY) = grid |> getGridDimensions;

  Int.rangeAsArray(0, lenY)
  |> Array.map(x =>
    Int.rangeAsArray(0, lenX)
    |> Array.map(y =>
      grid |> toMap(x, y)
    )
    |> followUp
  )
  |> followUp
};

// TODO: Figure out a better way to organize this
//       The (toMap, followUp) tuple being passed all over the place feels like an anti-pattern
//       Or maybe it doesn't really matter when these are so tightly coupled together right now?
let part1 = (isSpotVisible, Array.Int.sum);

let part2 = (calculateScenicScore, Array.Int.max >> Option.getOrThrow);

let run = (description, (toMap, followUp), data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.map(
    String.splitArray(~delimiter="")
    >> Array.map(String.toInt >> Option.getOrThrow))
  |> mapGrid(toMap, followUp)
  |> Int.toString
  |> Shared.Log.logWithDescription(description);

Shared.File.read("data/2022/day08test.txt")
|> run("Part 1 Test  ", part1);

Shared.File.read("data/2022/day08.txt")
|> run("Part 1 Result", part1);

Shared.File.read("data/2022/day08test.txt")
|> run("Part 2 Test  ", part2);

Shared.File.read("data/2022/day08.txt")
|> run("Part 2 Result", part2);

/*
$ node _build/default/src/2022/Day08.bs.js
Part 1 Test   : 21
Part 1 Result : 1560
Part 2 Test   : 8
Part 2 Result : 252000
*/