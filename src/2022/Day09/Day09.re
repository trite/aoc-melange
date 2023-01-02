let mapRight = (f, (x, y)) => (x, y |> f);

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
  switch (direction) {
  | "U" => Up(distance)
  | "D" => Down(distance)
  | "L" => Left(distance)
  | "R" => Right(distance)
  | _ => raise(Failure("Unexpected character"))
  };


let makeGridOfSize = (x, y, f) =>
  Int.rangeAsArray(0, y)
  |> Array.map(y => Int.rangeAsArray(0, x) |> Array.map(x => f(x, y)));

let determineGridSize = moves => {
  let startingPosition = (0, 0, 0, 0, 0, 0);

  let applyMove = ((startX, startY, maxX, maxY, minX, minY), move) =>
    switch (move) {
    | Up(y) => (
        startX,
        startY + y,
        maxX,
        Int.max(maxY, startY + y),
        minX,
        Int.min(minY, startY + y),
      )
    | Down(y) => (
        startX,
        startY - y,
        maxX,
        Int.max(maxY, startY - y),
        minX,
        Int.min(minY, startY - y),
      )
    | Left(x) => (
        startX - x,
        startY,
        Int.max(maxX, startX - x),
        maxY,
        Int.min(minX, startX - x),
        minY,
      )
    | Right(x) => (
        startX + x,
        startY,
        Int.max(maxX, startX + x),
        maxY,
        Int.min(minX, startX + x),
        minY,
      )
    };

  let applyMoves = (moves, position) =>
    List.foldLeft(applyMove, position, moves);

  applyMoves(moves, startingPosition)
  |> (((_, _, x, y, a, b)) => (x, y, a, b));
};

/* grid sizes using above:
   Test   : [ 5, 4, 0, 0 ]
   Actual : [ 300, 166, -55, -51 ]
   */

// let makeEmptyGridOfSize = (x, y) => makeGridOfSize(x, y, (_, _) => false);

// let makeTestGridOfSize = (x, y) => makeGridOfSize(x, y, Tuple2.make);

type positionInfo = {
  head: Position.t,
  middle: list(Position.t),
  tail: Position.t,
  tailVisited: Position.Set.t,
};

let getTranslationToApply =
    ({x: x1, y: y1}: Position.t, {x: x2, y: y2}: Position.t): Translation.t =>
  switch (x1 - x2, y1 - y2) {
  // No movement
  | (0, 0) // same spot
  | (0, 1) // up
  | (0, (-1)) // down
  | ((-1), 0) // left
  | (1, 0) // right
  | ((-1), (-1)) // lower-left
  | ((-1), 1) // upper-left
  | (1, (-1)) // lower-right
  | (1, 1) => {dx: 0, dy: 0} // upper-right

  // Causes movement
  | (0, 2) => {dx: 0, dy: 1} // up
  | (0, (-2)) => {dx: 0, dy: (-1)} // down
  | ((-2), 0) => {dx: (-1), dy: 0} // left
  | (2, 0) => {dx: 1, dy: 0} // right
  | ((-1), (-2))
  | ((-2), (-1)) => {dx: (-1), dy: (-1)} // lower-left
  | ((-1), 2)
  | ((-2), 1) => {dx: (-1), dy: 1} // upper-left
  | (1, (-2))
  | (2, (-1)) => {dx: 1, dy: (-1)} // lower-right
  | (1, 2)
  | (2, 1) => {dx: 1, dy: 1} // upper-right

  // OH NOES
  | _ =>
    raise(
      Failure(
        {j|
      This shouldn't be possible!
      (x1, y1), (x2, y2)
      ($x1, $y1), ($x2, $y2)
    |j},
      ),
    )
  };

let getAndApplyTranslation = (p1, p2) =>
  Position.applyTranslation(getTranslationToApply(p1, p2), p2);

  
let showPosition = ({x, y}: Position.t) =>
  {j|{x: $x, y: $y}|j};

let showTranslation = ({dx, dy}: Translation.t) =>
  {j|{dx: $dx, dy: $dy}|j};

let showPositionInfo =
  (
    ~description="",
    {head, middle, tail, tailVisited}: positionInfo
  ) => {
    let surround = (b, e, t) =>
      b ++ t ++ e;

    let lHead = head |> showPosition;

    let lMiddle =
      (
        middle
        |> List.map(showPosition)
        // |> List.intersperse(", ")
        |> List.String.joinWith(", ")
      )
      |> surround("[", "]");

    let lTail = tail |> showPosition;

    let lTailVisited =
      tailVisited
      |> Set.toList
      |> List.map(showPosition)
      |> List.String.joinWith(", ")
      |> surround("[", "]");

      {j|
  ----position info ($description)----
    head: $lHead,
    middle: $lMiddle,
    tail: $lTail,
    tailVisited: $lTailVisited
  ------------------------------------
|j}
  };

let applyMoveIncrement =
    (
      {head, middle, tail, tailVisited}: positionInfo,
      move: move,
      debug: option(positionInfo),
    ) => {
  let applyMoveInner =
      (move: move, {x: startX, y: startY}: Position.t): (move, Position.t) =>
    switch (move) {
    | Up(y) => (Up(y - 1), {x: startX, y: startY + 1})
    | Down(y) => (Down(y - 1), {x: startX, y: startY - 1})
    | Left(x) => (Left(x - 1), {x: startX - 1, y: startY})
    | Right(x) => (Right(x - 1), {x: startX + 1, y: startY})
    };

  let (restOfMove, newHead) = head |> applyMoveInner(move);

  let rec updateMiddle =
    fun
    | [hd, tl, ...rest] => {
        let nextTranslation = getTranslationToApply(hd, tl);
        let newTl = tl |> Position.applyTranslation(nextTranslation);



        switch (debug) {
        | None => Js.log("No debug")
        | Some(info) => {
          let _distTail = Position.distance(newTl, tl) |> showTranslation;
          let lHd = hd |> showPosition;
          let lTl = tl |> showPosition;
          let lNewTl = newTl |> showPosition;
          let _lNextTranslation = nextTranslation |> showTranslation;
          let _lInfo = info |> showPositionInfo;

          Js.log({j|
    (hd: $lHd, tl: $lTl) => newTl: $lNewTl
          |j});
        }
        };

        [newTl, ...updateMiddle([newTl, ...rest])];
      }
    | [_] => []
    | [] => raise(Failure("This shouldn't be completely empty!"));

  let newMiddle = newHead ^:: middle |> updateMiddle;

  let newTail =
    getAndApplyTranslation(
      newMiddle |> List.length == 0
        ? newHead : newMiddle |> List.last |> Option.getOrThrow,
      tail,
    );

  let newVisited = tailVisited |> Position.Set.add(newTail);

  (
    {
      head: newHead,
      middle: newMiddle,
      tail: newTail,
      tailVisited: newVisited,
    },
    restOfMove,
  );
};

let rec applyMove = ((positionInfo, move), debug) => {
  let debugOpt =
    debug
    ? Some(positionInfo)
    : None;

  debug
  ? Js.log(positionInfo |> showPositionInfo(~description="before"))
  : ();

  let (newPositionInfo, newMove) = applyMoveIncrement(positionInfo, move, debugOpt);

  debug
  ? Js.log(newPositionInfo |> showPositionInfo(~description="after"))
  : ();

  switch (move) {
  | Up(a)
  | Down(a)
  | Left(a)
  | Right(a) when a == 0 => positionInfo
  | Up(a)
  | Down(a)
  | Left(a)
  | Right(a) when a < 0 => raise(Failure("Check your logic!"))
  | _ => applyMove((newPositionInfo, newMove), debug)
  };
}

let applyMoves = (position, debug: bool, moves) =>
  List.foldLeft(
    (a, b) => applyMove((a, b), debug),
    position,
    moves,
  );

let startingPositionPart1 = {
  head: {
    x: 0,
    y: 0,
  },
  middle: [],
  tail: {
    x: 0,
    y: 0,
  },
  tailVisited: Position.Set.empty |> Position.Set.add({x: 0, y: 0}),
};

let getTailVisited = ({head: _, middle: _, tail: _, tailVisited}) => tailVisited;

let positionEq = ((hx, hy), (tx, ty)) => hx == tx && hy == ty;

let part1 = (debug, moves) =>
  moves
  |> applyMoves(startingPositionPart1, debug)
  |> getTailVisited
  |> Position.Set.toArray
  |> Array.length;

let startingPositionPart2 = {
  head: {
    x: 0,
    y: 0,
  },
  middle: List.repeat(9, {x: 0, y: 0}: Position.t),
  tail: {
    x: 0,
    y: 0,
  },
  tailVisited: Position.Set.empty |> Position.Set.add({x: 0, y: 0}),
};

let part2 = (debug, moves) =>
  moves
  |> applyMoves(startingPositionPart2, debug)
  |> getTailVisited
  |> Position.Set.toArray
  |> Array.length;

let parseMoves =
  String.splitList(~delimiter="\n")
  >> List.map(
       String.splitList(~delimiter=" ")
       >> Shared.List.toTuple2
       >> mapRight(String.toInt >> Option.getOrThrow)
       >> parseMove,
     );

let run = (part, debug: bool, data) =>
  data
  |> parseMoves
  |> part(debug)
  |> Int.toString;

let testData = "data/2022/day09test.txt";
let problemData = "data/2022/day09.txt";

Shared.IO.(
  {
    readRunLog(
      ~path=testData,
      ~work=run(part1, false),
      ~description=test(SingleTest, 1),
    );

    readRunLog(
      ~path=problemData,
      ~work=run(part1, false),
      ~description=result(1),
    );

    readRunLog(
      ~path=testData,
      ~work=run(part2, true),
      ~description=test(SingleTest, 2),
    );
  }
);
// readRunLog(
//   ~path=problemData,
//   ~work=run(part2, false),
//   ~description=result(2)
// );

/*
 $ node _build/default/src/2022/Day09.bs.js
 Part 1 Test   : 13
 Part 1 Result : 6011
 */
