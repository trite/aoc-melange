// Part 1

let getGridDimensions = (grid: array(array(int))) =>
  (
    grid |> Array.length,
    grid |> Array.head |> Option.getOrThrow |> Array.length
  );

let testGrid =
  [|
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
  let len = grid |> Array.length;

  Int.rangeAsArray(0, len)
  |> Array.map(x =>
    Int.rangeAsArray(0, len)
    |> Array.map(y =>
      grid |> isSpotVisible(x, y)
    )
    |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
  )
  |> Array.Int.sum // comment out both Array.Int.sum lines to see grid
};
/* 0 = not visible, 1 = visible
$ node _build/default/src/2022/Day08.bs.js
[
  [ 1, 1, 1, 1, 1 ],
  [ 1, 1, 1, 0, 1 ],
  [ 1, 1, 0, 1, 1 ],
  [ 1, 0, 1, 0, 1 ],
  [ 1, 1, 1, 1, 1 ]
]
*/

/*
let part1 = (x, y, grid) =>
  (
    grid |> getItemAt(x, y),
    grid |> getAllRelevantItems(x, y),
    grid |> isSpotVisible(x, y)
  );

// Position: (1, 1)
$ node _build/default/src/2022/Day08.bs.js
[
  5,                                          // height of tree in question
  [ [ 0 ], [ 5, 3, 5 ], [ 2 ], [ 5, 1, 2 ] ], // height of trees above/below/left/right (each as their own array)
  [ true, false, true, false ]                // is tree visible above/below/left/right
]
// Position: (3, 1)
$ node _build/default/src/2022/Day08.bs.js
[
  1,
  [ [ 7 ], [ 3, 4, 9 ], [ 5, 5, 2 ], [ 2 ] ],
  [ false, false, false, false ]
]
*/


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
  // |> Js.log;

Shared.File.read("data/2022/day08test.txt")
|> run("Part 1 Test  ", part1);

Shared.File.read("data/2022/day08.txt")
|> run("Part 1 Result", part1);

// Shared.File.read("data/2022/day08test.txt")
// |> doWork("Part 2 Test  ", part2);

// Shared.File.read("data/2022/day08.txt")
// |> doWork("Part 2 Result", part2);