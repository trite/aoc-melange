let mapRight = (f, (x, y)) =>
  (x, y |> f);

let (^::) = List.cons;

type move =
  | Up(int)
  | Down(int)
  | Left(int)
  | Right(int);

let formatMove =
  fun
  | Up(x) => {j|Up($x)|j}
  | Down(x) => {j|Down($x)|j}
  | Left(x) => {j|Left($x)|j}
  | Right(x) => {j|Right($x)|j};

let parseMove = ((direction, distance)) =>
  switch(direction) {
  | "U" => Up(distance)
  | "D" => Down(distance)
  | "L" => Left(distance)
  | "R" => Right(distance)
  | _ => raise(Failure("Unexpected character"))
  };

let makeGridOfSize = (x, y, f) =>
  Int.rangeAsArray(0, y)
  |> Array.map(y =>
    Int.rangeAsArray(0, x)
    |> Array.map(x =>
      f(x, y)
    ));

let determineGridSize = moves => {
  let startingPosition = (0, 0, 0, 0, 0, 0);
  // let startingPosition = (0, 0, 0, 0);

  let applyMove = ((startX, startY, maxX, maxY, minX, minY), move) =>
    switch(move) {
    | Up(y) => (startX, startY + y, maxX, Int.max(maxY, startY + y), minX, Int.min(minY, startY + y))
    | Down(y) => (startX, startY - y, maxX, Int.max(maxY, startY - y), minX, Int.min(minY, startY - y))
    | Left(x) => (startX - x, startY, Int.max(maxX, startX - x), maxY, Int.min(minX, startX - x), minY)
    | Right(x) => (startX + x, startY, Int.max(maxX, startX + x), maxY, Int.min(minX, startX + x), minY)
    };

  // let applyMove = ((startX, startY, maxX, maxY), move) =>
  //   switch(move) {
  //   | Up(y) => (startX, startY + y, maxX, Int.max(maxY, startY + y))
  //   | Down(y) => (startX, startY - y, maxX, Int.max(maxY, startY - y))
  //   | Left(x) => (startX - x, startY, Int.max(maxX, startX - x), maxY)
  //   | Right(x) => (startX + x, startY, Int.max(maxX, startX + x), maxY)
  //   };

  let applyMoves = (moves, position) =>
    List.foldLeft(applyMove, position, moves);

  applyMoves(moves, startingPosition)
  |> ((_, _, x, y, a, b)) => (x, y, a, b)
  // |> ((_, _, x, y)) => (x, y)
};

/* grid sizes using above:
Test   : [ 5, 4, 0, 0 ]
Actual : [ 300, 166, -55, -51 ]
*/

let makeEmptyGridOfSize = (x, y) =>
  makeGridOfSize(x, y, (_, _) => false);

// let expandGrid = (xBy, yBy, grid) =>

let makeTestGridOfSize = (x, y) =>
  makeGridOfSize(x, y, Tuple2.make);
  // Int.rangeAsArray(0, y)
  // |> Array.map(y =>
  //   Int.rangeAsArray(0, x)
  //   |> Array.map(x =>
  //     (x, y)
  //   ));
  // Array.repeat

makeTestGridOfSize(5,6)
|> Js.log;

type position = (int, int);

type positionInfo = {
  head: position,
  tail: position,
  // tailVisited: Set.t(position, position)
  tailVisited: list(position)
};

let applyMoveIncrement = ({head, tail, tailVisited}: positionInfo, move: move) => {
  // let applyMove = (move, (startX, startY)) =>
  //   switch(move) {
  //   | Up(y) => (startX, startY + y)
  //   | Down(y) => (startX, startY - y)
  //   | Left(x) => (startX - x, startY)
  //   | Right(x) => (startX + x, startY)
  //   };
  let applyMove = (move, (startX, startY)) =>
    switch(move) {
    | Up(y) => ((startX, startY + 1), Up(y-1))
    | Down(y) => ((startX, startY - 1), Down(y-1))
    | Left(x) => ((startX - 1, startY), Left(x-1))
    | Right(x) => ((startX + 1, startY), Right(x-1))
    };

  let moveIfNeeded = ((hx, hy), (tx, ty)) => {
    switch(hx - tx, hy - ty) {
    | (0, 0) => (tx, ty)    // same spot, don't move
    | (0, -1) => (tx, ty)   // down but don't move
    | (0, -2) => (tx, ty-1) // down
    | (0, 1) => (tx, ty)    // up but don't move
    | (0, 2) => (tx, ty+1)  // up
    | (-1, 0) => (tx, ty)   // left don't move
    | (-2, 0) => (tx-1, ty) // left
    | (1, 0) => (tx, ty)    // right don't move
    | (2, 0) => (tx+1, ty)  // right
    | (-1, -1) => (tx, ty)                // lower-left don't move
    | (-1, -2) | (-2, -1) => (tx-1, ty-1) // lower-left
    | (-1, 1) => (tx, ty)                 // upper-left don't move
    | (-1, 2) | (-2, 1) => (tx-1, ty+1)   // upper-left
    | (1, -1) => (tx, ty)                 // lower-right don't move
    | (1, -2) | (2, -1) => (tx+1, ty-1)   // lower-right
    | (1, 1) => (tx, ty)                  // upper-right don't move
    | (1, 2) | (2, 1) => (tx+1, ty+1)     // upper-right
    | _ => raise(
      Failure({j|
        This shouldn't be possible!
        (hx, hy), (tx, ty)
        ($hx, $hy), ($tx, $ty)
      |j}))
    // | (-2, -1) | (-1, -2) => (tx-1, ty-1)
    // | (2, -1) | (1, -2) => (tx+1, ty-1)
    }
    // let go = (h, t) =>
    //   switch(h - t) {
    //   | -1 | 0 | 1 => t
    //   | -2 => t - 1
    //   | 2 => t + 1
    //   | _ => raise(Failure({j|
    //     Head/tail more than 2 steps apart, this shouldn't happen!
    //     (hx, hy), (tx, ty)
    //     ($hx, $hy), ($tx, $ty)
    //     |j}))
    //   };

    // let newX = go(hx, tx);

    // let newY = go(hy, ty);

    // (newX, newY)
  };

  let (newHead, restOfMove) = head |> applyMove(move);

  let newTail = tail |> moveIfNeeded(newHead);

  let calcDists = ((dx, dy), (sx, sy)) =>
    (dx - sx, dy - sy);

  // let (distHead, distTail) = ()
  let distHead = calcDists(newHead, head);
  let distTail = calcDists(newTail, tail);

  Js.log({j|
    ----
    (($head), ($tail)) => (($newHead), ($newTail))
    ($distHead, $distTail)
  |j});

  (
    {
      head: newHead,
      tail: newTail,
      // tailVisited: tailVisited |> Set.add(newTail)
      tailVisited: newTail ^:: tailVisited
    },
    restOfMove
  )
};

let rec applyMove = ((positionInfo, move)) =>
  switch(move) {
  | Up(0) | Down(0) | Left(0) | Right(0) =>
    positionInfo
  | _ => applyMove(applyMoveIncrement(positionInfo, move))
  }

let applyMoves = (position, moves) =>
  List.foldLeft((a, b) => applyMove((a, b)), position, moves);

let startingPosition = {
  head: (0,0),
  tail: (0,0),
  tailVisited: [(0, 0)]
};

let part1 = ({head: _, tail: _, tailVisited}) =>
  tailVisited
  |> List.distinctBy(((hx, hy), (tx, ty)) => hx == tx && hy == ty)
  |> List.toArray
  |> Array.length
  // |> List.length
  // |> List.distinctBy(Tuple.EqBy2(Int.eq, Int.eq))
;

let run = (_description, part, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    String.splitList(~delimiter=" ")
    >> Shared.List.toTuple2
    >> mapRight(String.toInt >> Option.getOrThrow)
    >> parseMove
    // >> formatMove
  )
  |> applyMoves(startingPosition)
  // |> determineGridSize
  // |> List.toArray
  |> part
  |> Js.log;
  // |> Shared.Log.logWithDescription(_description);


Shared.File.read("data/2022/day09test.txt")
|> run("Part 1 Test  ", part1);

// Shared.File.read("data/2022/day09.txt")
// |> run("Part 1 Result", part1);

// Shared.File.read("data/2022/day09test.txt")
// |> run("Part 2 Test  ", part2);

// Shared.File.read("data/2022/day09.txt")
// |> run("Part 2 Result", part2);