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

// makeTestGridOfSize(5,6)
// |> Js.log;

// type position = (int, int);
type position = Position.t;

type positionInfo = {
  head: position,
  // tail: position,
  tail: list(position),
  // tailVisited: Set.t(position, position)
  // tailVisited: list(position)
  tailVisited: Position.Set.t
};

let moveTailIfNeeded = ((hx, hy), (tx, ty)) => 
  switch(hx - tx, hy - ty) {
  // No movement
  | (0, 0)                              // same spot
  | (0, -1)                             // down
  | (0, 1)                              // up
  | (-1, 0)                             // left
  | (1, 0)                              // right
  | (-1, -1)                            // lower-left
  | (-1, 1)                             // upper-left
  | (1, -1)                             // lower-right
  | (1, 1) => (tx, ty)                  // upper-right

  // Causes movement
  | (0, -2) => (tx, ty-1)               // down
  | (0, 2) => (tx, ty+1)                // up
  | (-2, 0) => (tx-1, ty)               // left
  | (2, 0) => (tx+1, ty)                // right
  | (-1, -2) | (-2, -1) => (tx-1, ty-1) // lower-left
  | (-1, 2) | (-2, 1) => (tx-1, ty+1)   // upper-left
  | (1, -2) | (2, -1) => (tx+1, ty-1)   // lower-right
  | (1, 2) | (2, 1) => (tx+1, ty+1)     // upper-right

  // OH NOES
  | _ => raise(
    Failure({j|
      This shouldn't be possible!
      (hx, hy), (tx, ty)
      ($hx, $hy), ($tx, $ty)
    |j}))
  };

let applyMoveIncrement = (
    {head, tail, tailVisited}: positionInfo,
    move: move,
    debug) => {
  let applyMove = (move, (startX, startY)) =>
    switch(move) {
    | Up(y) => ((startX, startY + 1), Up(y-1))
    | Down(y) => ((startX, startY - 1), Down(y-1))
    | Left(x) => ((startX - 1, startY), Left(x-1))
    | Right(x) => ((startX + 1, startY), Right(x-1))
    };

  let (newHead, restOfMove) = head |> applyMove(move);

  let rec updateTail =
    fun
    | [a, b, ...rest] => {
      let nt = moveTailIfNeeded(a, b);

      if (debug) {
        // Debug movements
        let calcDists = ((dx, dy), (sx, sy)) =>
          (dx - sx, dy - sy);
        let distHead = calcDists(newHead, head);
        let distTail = calcDists(nt, b);
        Js.log({j|
          ----
          (($head), ($tail)) => (($newHead), ($nt))
          ($distHead, $distTail)
        |j});
      };

      [nt, ...updateTail([nt, ...rest])]
    }
    | [_] => []
    | [] => raise(Failure("This shouldn't be completely empty!"));

  let newTail =
    (newHead ^:: tail)
    |> updateTail
    |> List.reverse;

  // let newTail =
  //   (newHead ^:: tail)
  //   |> List.foldLeft((h, t) => moveTailIfNeeded(h, t))

  // let newTail = tail |> moveTailIfNeeded(newHead);

  // Debug movements

  let newVisited =
    newTail
    |> List.foldLeft((tv, n) => Position.Set.add(n, tv), tailVisited);

  (
    {
      head: newHead,
      tail: newTail,
      tailVisited: newVisited
      // tailVisited: newTail ^:: tailVisited
      // tailVisited: Position.Set.add(newTail, tailVisited)
    },
    restOfMove
  )
};

// let applyMoveIncrement = ({head, tail, tailVisited}: positionInfo, move: move) => {
//   let applyMove = (move, (startX, startY)) =>
//     switch(move) {
//     | Up(y) => ((startX, startY + 1), Up(y-1))
//     | Down(y) => ((startX, startY - 1), Down(y-1))
//     | Left(x) => ((startX - 1, startY), Left(x-1))
//     | Right(x) => ((startX + 1, startY), Right(x-1))
//     };

//   let (newHead, restOfMove) = head |> applyMove(move);

//   let newTail = tail |> moveTailIfNeeded(newHead);

//   // Debug movements
//   // let calcDists = ((dx, dy), (sx, sy)) =>
//   //   (dx - sx, dy - sy);
//   // let distHead = calcDists(newHead, head);
//   // let distTail = calcDists(newTail, tail);
//   // Js.log({j|
//   //   ----
//   //   (($head), ($tail)) => (($newHead), ($newTail))
//   //   ($distHead, $distTail)
//   // |j});

//   (
//     {
//       head: newHead,
//       tail: newTail,
//       // tailVisited: newTail ^:: tailVisited
//       tailVisited: Position.Set.add(newTail, tailVisited)
//     },
//     restOfMove
//   )
// };

let rec applyMove = ((positionInfo, move), debug) =>
  switch(move) {
  | Up(0) | Down(0) | Left(0) | Right(0) => positionInfo
  | _ => applyMove(applyMoveIncrement(positionInfo, move, debug), debug)
  }

let applyMoves = (position, debug, moves) =>
  List.foldLeft((a, b) => applyMove((a, b), debug), position, moves);

let startingPositionPart1 = {
  head: (0,0),
  tail: [(0,0)],
  tailVisited:
    Position.Set.empty
    |> Position.Set.add((0, 0))
  // tailVisited: [(0, 0)]
};

let getTailVisited = ({head: _, tail: _, tailVisited}) =>
  tailVisited;

let positionEq = ((hx, hy), (tx, ty)) =>
  hx == tx && hy == ty;

let part1 = (debug, moves) =>
  moves
  |> applyMoves(startingPositionPart1, debug)
  |> getTailVisited
  // >> List.distinctBy(positionEq)
  |> Position.Set.toArray
  // >> List.toArray
  |> Array.length;

let startingPositionPart2 = {
  head: (0,0),
  // tail: [(0,0)],
  tail: List.repeat(2, (0,0)),
  tailVisited:
    Position.Set.empty
    |> Position.Set.add((0, 0))
  // tailVisited: [(0, 0)]
};

let part2 = (debug, moves) =>
  moves
  |> applyMoves(startingPositionPart2, debug)
  |> getTailVisited
  // >> List.distinctBy(positionEq)
  |> Position.Set.toArray
  // >> List.toArray
  |> Array.length;

let run = (part, debug, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    String.splitList(~delimiter=" ")
    >> Shared.List.toTuple2
    >> mapRight(String.toInt >> Option.getOrThrow)
    >> parseMove
    // >> formatMove
  )
  // |> applyMoves(startingPosition)
  // |> determineGridSize
  // |> List.toArray
  |> part(debug)
  |> Int.toString;

let testData = "data/2022/day09test.txt";
let problemData = "data/2022/day09.txt";

Shared.IO.({
  readRunLog(
    ~path=testData,
    ~work=run(part1, false),
    ~description=test(SingleTest, 1)
  );

  readRunLog(
    ~path=problemData,
    ~work=run(part1, false),
    ~description=result(1)
  );

  // readRunLog(
  //   ~path=testData,
  //   ~work=run(part2, false),
  //   ~description=test(SingleTest, 2)
  // );

  // readRunLog(
  //   ~path=problemData,
  //   ~work=run(part2, false),
  //   ~description=result(2)
  // );
});

/*
$ node _build/default/src/2022/Day09.bs.js
Part 1 Test   : 13
Part 1 Result : 6011
*/