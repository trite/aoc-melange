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
  |> Array.map(Array.all(i => i < treeToExamine))
  |> Array.any(id)
  ? 1
  : 0
};
  
// let part1 = grid => {
//   let (lenX, lenY) = grid |> getGridDimensions;

//   Int.rangeAsArray(0, lenX)
//   |> Array.map(x =>
//     Int.rangeAsArray(0, lenY)
//     |> Array.map(y =>
//       grid |> isSpotVisible(x, y)
//     )
//     |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
//   )
//   |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
// };

let calculateScenicScore = (x, y, grid) => {
  let treeToExamine = grid |> getItemAt(x, y);

  // let rec go = (x, y, grid) => 
  //   switch(grid |> )
  //   | i when i >= treeToExamine => [i]
  //   | i => [i, ...go(x, y, )]
    
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

// let part2 = grid => {
//   let (lenX, lenY) = grid |> getGridDimensions;

//   Int.rangeAsArray(0, lenX)
//   |> Array.map(x =>
//     Int.rangeAsArray(0, lenY)
//     |> Array.map(y =>
//       grid |> calculateScenicScore(x, y)
//     )
//     |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
//   )
//   |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
// };

/*
let part2 = grid => {
  calculateScenicScore(2, 2, grid)
  |> Js.log;

  -1
};
$ node _build/default/src/2022/Day08.bs.js
Part 1 Test   : 21
Part 1 Result : 1560
// order matches how each row would look to the observer from the tree at (2,2)
[ [ 5, 3 ], [ 5, 3 ], [ 5, 6 ], [ 3, 2 ] ] 
Part 2 Test   : -1
*/

// TODO: Figure out a better way to organize this
//       The (toMap, followUp) tuple being passed all over the place feels like an anti-pattern
let mapGrid = ((toMap, followUp), grid) => {
  let (lenX, lenY) = grid |> getGridDimensions;

  Int.rangeAsArray(0, lenX)
  |> Array.map(x =>
    Int.rangeAsArray(0, lenY)
    |> Array.map(y =>
      grid |> toMap(x, y)
    )
    |> followUp
  )
  |> followUp
};

let part1 = (isSpotVisible, Array.Int.sum);

let part2 = (calculateScenicScore, Array.Int.max >> Option.getOrThrow);

let run = (description, part, data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.map(
    String.splitArray(~delimiter="")
    >> Array.map(String.toInt >> Option.getOrThrow))
  |> mapGrid(part)
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