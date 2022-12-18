// Part 1

let getGridDimensions = (grid: array(array(int))) =>
  (
    grid |> Array.length,
    grid |> Array.head |> Option.getOrThrow |> Array.length
  );

let testGrid =
  [|
    // [| (0,0), (0,1), (0,2), (0,3), (0,4) |],
    // [| (1,0), (1,1), (1,2), (1,3), (1,4) |],
    // [| (2,0), (2,1), (2,2), (2,3), (2,4) |],
    // [| (3,0), (3,1), (3,2), (3,3), (3,4) |],
    // [| (4,0), (4,1), (4,2), (4,3), (4,4) |],
    [| (0,0), (1,0), (2,0), (3,0), (4,0) |],
    [| (0,1), (1,1), (2,1), (3,1), (4,1) |],
    [| (0,2), (1,2), (2,2), (3,2), (4,2) |],
    [| (0,3), (1,3), (2,3), (3,3), (4,3) |],
    [| (0,4), (1,4), (2,4), (3,4), (4,4) |],
  |];

let getItemAt = (x, y, grid) =>
  grid
  |> Array.at(y)
  |> Option.flatMap(Array.at(x))
  |> fun
     | None => raise(Failure({j|Failed to find element at ($x, $y) \n $grid|j}))
     | Some(x) => x;
  // |> Option.getOrThrow;
  // |> Option.getOrElse(raise(Failure({j|Failed to find element at ($x, $y) \n $grid|j})));

// let rec getItemsAbove = (x, y, grid) =>
//   y == 0
//   ? []
//   : [grid |> getItemAt(x, y), ...(grid |> getItemsAbove(x, y-1))];

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

let getItemsAbove = getItemsDirection(id, (-)(_, 1), Tuple.second);
let getItemsBelow = getItemsDirection(id, (+)(_, 1), Tuple.second);
let getItemsLeft = getItemsDirection((-)(_, 1), id, Tuple.first);
let getItemsRight = getItemsDirection((+)(1), id, Tuple.first);

// let getItemsAbove = (x, y, grid) => {
//   let rec go = (x, y, grid) => {
//     switch(y) {
//     | 0 => [grid |> getItemAt(x, y)]
//     | _ => [grid |> getItemAt(x, y), ...(grid |> go(x, y-1))]
//     }
//   }

//   switch(x) {
//   | 0 => []
//   | _ => go(x, y-1, grid)
//   }
// };

let arrayMax = Array.length >> (-)(_, 1);

let getAllRelevantItems = (x, y, grid) =>
  [|
    getItemsAbove(0, x, y, grid),
    getItemsBelow(grid |> arrayMax, x, y, grid),
    getItemsLeft(0, x, y, grid),
    getItemsRight(grid |> arrayMax, x, y, grid)
  |]

/*
testGrid
|> getAllRelevantItems(3, 3)
|> Js.log;

$ node _build/default/src/2022/Day08.bs.js
[
  [ [ 3, 2 ], [ 3, 1 ], [ 3, 0 ] ],
  [ [ 3, 4 ] ],
  [ [ 2, 3 ], [ 1, 3 ], [ 0, 3 ] ],
  [ [ 4, 3 ] ]
]
*/


// let isSpotVisible = ((x: int, y: int) grid: array(array(int))): bool =>
  
  
  
  
// let _part1 = getItemAt(3, 1);

// Part 2

// Program boundry IO

// let _run = (_description, partSpecificStuff, data) =>
//   data
//   |> String.splitArray(~delimiter="\n")
//   |> Array.map(
//     String.splitArray(~delimiter="")
//     >> Array.map(
//       String.toInt
//       >> Option.getOrThrow))
//   |> partSpecificStuff
//   |> ignore;
//   // |> Js.log;

// Shared.File.read("data/2022/day08test.txt")
// |> run("Part 1 Test  ", part1);

// Shared.File.read("data/2022/day08.txt")
// |> doWork("Part 1 Result", part1);

// Shared.File.read("data/2022/day08test.txt")
// |> doWork("Part 2 Test  ", part2);

// Shared.File.read("data/2022/day08.txt")
// |> doWork("Part 2 Result", part2);