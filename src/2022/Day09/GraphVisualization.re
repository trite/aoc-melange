module SimState = {
  module P = {
    type positionInfo = {
      head: Position.t,
      middle: list(Position.t),
      tail: Position.t,
      visited: Position.Set.t,
    };

    type movingMiddle = {
      head: Position.t,
      middleDone: list(Position.t),
      middleLeft: list(Position.t),
      tail: Position.t,
      visited: Position.Set.t,
    };

    type tailInfo = {
      head: Position.t,
      middle: list(Position.t),
      tail: Position.t,
      toCompare: Position.t,
      visited: Position.Set.t,
    };
  };

  module M = {
    type betweenMoves = list(Day09.move);

    type moveInProgress = {
      current: Day09.move,
      remaining: list(Day09.move),
    };
  };

  module Warning = {
    type t =
      | UnexpectedTranslationDistance(string)
      | NoWarning;
  };

  type state =
    | Start(P.positionInfo, M.betweenMoves) // ready to move head
    | Head(P.positionInfo, M.moveInProgress) // ready to start moving middle
    | Middle(P.movingMiddle, M.moveInProgress) // middle is moving or done and
    | Tail(P.tailInfo, M.moveInProgress); // done moving middle

  type frame = {
    state,
    warnings: list(Warning.t),
  };

  type frameHistory = list(frame);
  // | TailMoved(positionInfo, betweenMoves);

  let zeroZero: Position.t = {x: 0, y: 0};

  let initialState = (moves: list(Day09.move)) =>
    Start(
      {
        head: zeroZero,
        // middle: List.repeat(8, zeroZero),
        middle: [],
        tail: zeroZero,
        visited: Position.Set.empty |> Position.Set.add(zeroZero),
      }: P.positionInfo,
      moves,
    );

  let stateToPositionList = (state: state): list(Position.t) =>
    (
      switch (state) {
      | Start({head, middle, tail, visited}, _)
      | Head({head, middle, tail, visited}, _) => [
          [head],
          middle,
          [tail],
          visited |> Position.Set.toList,
        ]
      | Middle({head, middleDone, middleLeft, tail, visited}, _) => [
          [head],
          middleDone,
          middleLeft,
          [tail],
          visited |> Position.Set.toList,
        ]
      | Tail({head, middle, tail, toCompare, visited}, _) => [
          [head],
          middle,
          [tail],
          [toCompare],
          visited |> Position.Set.toList,
        ]
      }
    )
    |> List.foldLeft(List.concat, []);

  // let stateToPositionInfo = (state: state): P.positionInfo =>
  //   switch (state) {
  //   | Start({head, middle, tail, visited}, _)
  //   | Head({head, middle, tail, visited}, _)
  //   | Tail({head, middle, tail, toCompare: _, visited}, _) => {
  //       head,
  //       middle,
  //       tail,
  //       visited,
  //     }

  //   | Middle({head, middleDone, middleLeft, tail, visited}, _) => {
  //       head,
  //       middle: middleDone |> List.concat(middleLeft),
  //       tail,
  //       visited,
  //     }
  //   };

  // Instead of `stateToPositionInfo`
  // `populateGridPositions` as name maybe?
  // Make a function that takes a state
  //   and returns a function: array(array(Position.t)) => array(array(string))
  // Next step will be to return from `stateToGrid` I think

  let getPositionLimits = (state: state): (Position.t, Position.t) =>
    state
    |> stateToPositionList
    |> (
      positionList => (
        positionList |> Position.List.min |> Option.getOrThrow,
        positionList |> Position.List.max |> Option.getOrThrow,
      )
    );

  let makeGrid =
      (({x: xMin, y: yMin}: Position.t, {x: xMax, y: yMax}: Position.t)) =>
    Int.rangeAsArray(xMin, xMax + 1)
    |> Array.map(_ => Int.rangeAsArray(yMin, yMax + 1) |> Array.map(_ => "."));

  let updateGridAtPosition = (~position as {x, y}: Position.t, ~update, grid) =>
    grid |> Array.updateAt(y, Array.updateAt(x, _ => update));

  // let populateGridPositions =
  //     (state: state): (array(array(Position.t)) => array(array(string))) => {
  //       switch(state) {
  //         | Start({head, middle, tail, visited}, _)
  //         | Head({head, middle, tail, visited}, _) =>

  //       }
  //     };

  // let stateToGrid = (state: state): array(array(string)) => {
  //   let baseGrid = state |> getPositionLimits |> makeGrid;
  //   let positionInfo = ();
  //   ();
  //   // let grid = Int.rangeAsArray();
  //   // ();
  //   // [|[||]|];
  // };

  let gridToString = (grid: array(array(string))): string =>
    grid |> Array.map(Array.String.join) |> Array.String.joinWith("\n");
  // grid
  // |> Array.map(
  //      Array.map(Array.String.join) >> Array.String.joinWith("\n"),
  //    );

  // let stateToGridString = stateToGrid >> gridToString;

  let move1 = x =>
    switch (x) {
    | x when x > 0 => 1
    | x when x < 0 => (-1)
    | _ => 0
    };

  let getTranslationToApply =
      ({x: x1, y: y1}: Position.t, {x: x2, y: y2}: Position.t)
      : (Translation.t, Warning.t) =>
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
    | (1, 1) => ({dx: 0, dy: 0}, NoWarning) // upper-right

    // Causes movement
    | (0, 2) => ({dx: 0, dy: 1}, NoWarning) // up
    | (0, (-2)) => ({dx: 0, dy: (-1)}, NoWarning) // down
    | ((-2), 0) => ({dx: (-1), dy: 0}, NoWarning) // left
    | (2, 0) => ({dx: 1, dy: 0}, NoWarning) // right
    | ((-1), (-2))
    | ((-2), (-1)) => ({dx: (-1), dy: (-1)}, NoWarning) // lower-left
    | ((-1), 2)
    | ((-2), 1) => ({dx: (-1), dy: 1}, NoWarning) // upper-left
    | (1, (-2))
    | (2, (-1)) => ({dx: 1, dy: (-1)}, NoWarning) // lower-right
    | (1, 2)
    | (2, 1) => ({dx: 1, dy: 1}, NoWarning) // upper-right

    // Unexpected - best guess + warning
    | (x, y) => (
        {dx: x |> move1, dy: y |> move1},
        UnexpectedTranslationDistance(
          {j|This wasn't an expected move, review carefully!
  (x1, y1), (x2, y2)
  ($x1, $y1), ($x2, $y2)|j},
        ),
      )
    };

  let moveToTranslation: Day09.move => Translation.t =
    fun
    | Up(_) => {dx: 1, dy: 0}
    | Down(_) => {dx: (-1), dy: 0}
    | Left(_) => {dx: 0, dy: (-1)}
    | Right(_) => {dx: 0, dy: 1};

  let advanceMiddle =
      (~head, ~middleDone, ~middleLeft, ~tail, ~visited, ~current, ~remaining) =>
    switch (middleLeft) {
    | [] => {
        state:
          Tail(
            {head, middle: middleDone, tail, toCompare: head, visited},
            {current, remaining},
          ),
        warnings: [],
      }
    | [midHead] => {
        state:
          Tail(
            {head, middle: middleDone, tail, toCompare: midHead, visited},
            {current, remaining},
          ),
        warnings: [],
      }
    | [midHead, ...middleLeft] =>
      let (translation, warning) = getTranslationToApply(head, midHead);

      let middleDone = [
        midHead |> Position.applyTranslation(translation),
        ...middleDone,
      ];

      {
        state:
          Middle(
            {head, middleDone, middleLeft, tail, visited},
            {current, remaining},
          ),
        warnings: [warning],
      };
    };

  let decrementMove: Day09.move => Day09.move =
    fun
    | Up(x) => Up(x - 1)
    | Down(x) => Down(x - 1)
    | Left(x) => Left(x - 1)
    | Right(x) => Right(x - 1);

  let isMoveDone: Day09.move => bool =
    fun
    | Up(x)
    | Down(x)
    | Left(x)
    | Right(x) when x <= 0 => true
    | _ => false;

  let advance = (state: state) =>
    // TODO: after `current` is applied and has its count
    //       reduced by 1, simply cons it back onto the
    //       list when moving from `TailMove` to `BetweenMoves`
    switch (state) {
    | Start({head, middle, tail, visited}, moves) =>
      let (current, remaining) = moves |> List.uncons |> Option.getOrThrow;

      let head =
        head |> Position.applyTranslation(current |> moveToTranslation);

      {
        state: Head({head, middle, tail, visited}, {current, remaining}),
        warnings: [],
      };

    | Head({head, middle, tail, visited}, {current, remaining}) =>
      advanceMiddle(
        ~head,
        ~middleDone=[],
        ~middleLeft=middle,
        ~tail,
        ~visited,
        ~current,
        ~remaining,
      )

    | Middle(
        {head, middleDone, middleLeft, tail, visited},
        {current, remaining},
      ) =>
      advanceMiddle(
        ~head,
        ~middleDone,
        ~middleLeft,
        ~tail,
        ~visited,
        ~current,
        ~remaining,
      )

    | Tail({head, middle, tail, toCompare, visited}, {current, remaining}) =>
      let updatedMoves =
        isMoveDone(current)
          ? remaining : [current |> decrementMove, ...remaining];

      let (translation, warning) = getTranslationToApply(toCompare, tail);

      let tail = toCompare |> Position.applyTranslation(translation);

      {
        state:
          Start(
            {head, middle: middle |> List.reverse, tail, visited},
            updatedMoves,
          ),
        warnings: [warning],
      };
    };

  let make = (~offset, ~count, data) => {
    let rec run = (count, state) => {
      let {state, warnings} = state |> advance;
      let warnings = warnings |> List.filter((!=)(Warning.NoWarning));

      switch (count) {
      | 0 => []
      | count => [{state, warnings}, ...run(count - 1, state)]
      };
      // switch (frame) {
      // | {state: Tail(_, {current, remaining}), warnings: _}
      //     when current |> isMoveDone && remaining == [] =>
      //   []
      // | {state: Start(_, _), warnings: _}
      // | {state: Head(_, _), warnings: _}
      // | {state: Middle(_, _), warnings: _}
      // | {state: Tail(_, _), warnings: _} => [
      //     frame,
      //     ...run(count - 1, state),
      //   ]
      // };
    };

    data |> Day09.parseMoves |> initialState |> run(offset + count);
  };
};

module Theme = {
  open Css;

  let basePadding = px(5);
  let baseMargin = px(5);
  let textColor = black;
};

module Styles = {
  open Css;

  let container =
    style([
      display(flexBox),
      // flexDirection(rowReverse),
      justifyContent(center),
      backgroundColor(white),
      border(px(1), solid, rgb(0, 0, 0)),
      gap(em(1.0)),
    ]);

  let box =
    style([
      border(px(1), solid, rgb(0, 0, 0)),
      padding(Theme.basePadding),
    ]);
};

let add1 = (+)(1);
let sub1 = x => x - 1;

module Frame = {
  module Styles = {
    open Css;

    let mainContainer = style([display(flexBox), flexDirection(column)]);
    // let
  };

  [@react.component]
  let make = (~frame as {state, warnings} as _frame: SimState.frame) => {
    <div className=Styles.mainContainer>
      // <pre> {SimState.stateToGridString(state) |> React.string} </pre>

        <p>
          {(
             switch (state) {
             | Start(_, _) => "Start"
             | Head(_, _) => "Head"
             | Middle(_, _) => "Middle"
             | Tail(_, _) => "Tail"
             }
           )
           |> (++)("State: ")
           |> React.string}
        </p>
        <p>
          {// let ({x:xMin, y:yMin}, {x:xMax, y:yMax}) = SimState.getPositionLimits(state)
           SimState.getPositionLimits(state)
           |> (
             (({x: xMin, y: yMin}, {x: xMax, y: yMax})) => {j|($xMin,$yMin),($xMax,$yMax)|j}
           )
           |> React.string}
        </p>
        <p>
          {warnings
           |> List.length
           |> Int.toString
           |> (++)("Warnings: ")
           |> React.string}
        </p>
        {warnings
         |> List.map(
              fun
              | SimState.Warning.UnexpectedTranslationDistance(s) =>
                <p>
                  {{j|Unexpected Translation Distance: $s|j} |> React.string}
                </p>
              | NoWarning => <p> {"NoWarning" |> React.string} </p>,
            )
         |> List.toArray
         |> React.array}
      </div>;
      // <p> {
      //   warnings
      //   |>
      // } </p>
  };
};

// module StateDisplay = {
//   [@react.component]
//   let make = (~offset, ~count, data) => {
//     <div>
//       {data
//        |> SimState.make
//        |> List.drop(offset)
//        |> List.take(count)
//        |> List.map(frame => <Frame frame />)
//        |> List.toArray
//        |> React.array}
//     </div>;
//   };
// };

module App = {
  [@react.component]
  let make = () => {
    let (countText, setCountText) = React.useState(() => "5");

    let (count, setCount) = React.useState(() => 5);

    let (offsetText, setOffsetText) = React.useState(() => "0");

    let (offset, setOffset) = React.useState(() => 0);

    let (data, setData) =
      React.useState(() => {j|D 1
D 4
L 3
D 1
R 4
D 1
L 5
R 2|j});

    let getValue = e => e->ReactEvent.Form.target##value;

    let onChange = (set, setText, e: ReactEvent.Form.t): unit => {
      // let value = e->ReactEvent.Form.target##value;
      let value = e |> getValue;

      value
      |> String.toInt
      |> (
        fun
        | Some(x) => set(_ => x)
        | None => ()
      );

      setText(value);
    };

    <div>
      <div>
        <label> {"Data" |> React.string} </label>
        // <input
        //   type_="text"
        <textarea value=data onChange={e => setData(_ => e |> getValue)} />
      </div>
      <div>
        <label> {{j|Frame count: ($count)|j} |> React.string} </label>
        // <input
        //   type_="text"
        //   onChange={onChange(setCount, setCountText)}
        //   value=countText
        // />
        <input
          type_="range"
          onChange={onChange(setCount, setCountText)}
          value=countText
          min="1"
          max="10"
        />
      </div>
      // <input
      //   type_="button"
      //   onClick={_ => {
      //     setCount(sub1);
      //     setCountText(
      //       Int.fromString >> Option.fold("oh noes", sub1 >> Int.toString),
      //     );
      //   }}
      //   value="-"
      // />
      // <input
      //   type_="button"
      //   onClick={_ => {
      //     setCount(add1);
      //     setCountText(
      //       Int.fromString >> Option.fold("oh noes", add1 >> Int.toString),
      //     );
      //   }}
      //   value="+"
      // />
      <div>
        <label> {"Frame offset:" |> React.string} </label>
        <input
          type_="text"
          onChange={onChange(setOffset, setOffsetText)}
          value=offsetText
        />
        <input
          type_="button"
          onClick={_ => {
            setOffset(sub1);
            setOffsetText(
              Int.fromString >> Option.fold("oh noes", sub1 >> Int.toString),
            );
          }}
          value="-"
        />
        <input
          type_="button"
          onClick={_ => {
            setOffset(add1);
            setOffsetText(
              Int.fromString >> Option.fold("oh noes", add1 >> Int.toString),
            );
          }}
          value="+"
        />
      </div>
      <div className=Styles.container>
        {data
         |> SimState.make(~offset, ~count)
         |> List.drop(offset)
         |> List.take(count)
         |> List.map(frame => <Frame frame />)
         |> List.toArray
         |> React.array}
      </div>
    </div>;
  };
};

ReactDOM.querySelector("#root")
|> (
  fun
  | Some(root) => ReactDOM.render(<App />, root)
  | None => Js.Console.error("#root element not found, can't start!")
);
