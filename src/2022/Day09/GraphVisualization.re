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
  };

  module M = {
    type betweenMoves = list(Day09.move);

    type moveInProgress = {
      current: Day09.move,
      remaining: list(Day09.move),
    };
  };

  type state =
    | BetweenMoves(P.positionInfo, M.betweenMoves) // ready to move head
    | HeadMove(P.positionInfo, M.moveInProgress) // ready to start moving middle
    | MovingMiddle(P.movingMiddle, M.moveInProgress) // middle is moving or done and
    | TailMove(P.positionInfo, M.moveInProgress); // done moving middle

  type history = list(state);
  // | TailMoved(positionInfo, betweenMoves);

  let zeroZero: Position.t = {x: 0, y: 0};

  let initialState = (moves: M.betweenMoves) =>
    BetweenMoves(
      {
        head: zeroZero,
        middle: List.repeat(8, zeroZero),
        tail: zeroZero,
        visited: Position.Set.empty |> Position.Set.add(zeroZero),
      }: P.positionInfo,
      moves,
    );

  let move1 = x =>
    switch (x) {
    | x when x > 0 => 1
    | x when x < 0 => (-1)
    | _ => raise(Failure("This shouldn't be possible..."))
    };

  let getTranslationToApply =
      ({x: x1, y: y1}: Position.t, {x: x2, y: y2}: Position.t)
      : (Translation.t, option(string)) =>
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
    | (1, 1) => ({dx: 0, dy: 0}, None) // upper-right

    // Causes movement
    | (0, 2) => ({dx: 0, dy: 1}, None) // up
    | (0, (-2)) => ({dx: 0, dy: (-1)}, None) // down
    | ((-2), 0) => ({dx: (-1), dy: 0}, None) // left
    | (2, 0) => ({dx: 1, dy: 0}, None) // right
    | ((-1), (-2))
    | ((-2), (-1)) => ({dx: (-1), dy: (-1)}, None) // lower-left
    | ((-1), 2)
    | ((-2), 1) => ({dx: (-1), dy: 1}, None) // upper-left
    | (1, (-2))
    | (2, (-1)) => ({dx: 1, dy: (-1)}, None) // lower-right
    | (1, 2)
    | (2, 1) => ({dx: 1, dy: 1}, None) // upper-right

    // Unexpected - best guess + warning
    | (x, y) => (
        {dx: x |> move1, dy: y |> move1},
        Some(
          {j|This wasn't an expected move, review carefully!
  (x1, y1), (x2, y2)
  ($x1, $y1), ($x2, $y2)|j},
        ),
      )
    };

  let advance = (state: state) =>
    // TODO: after `current` is applied and has its count
    //       reduced by 1, simply cons it back onto the
    //       list when moving from `TailMove` to `BetweenMoves`
    switch (state) {
    | BetweenMoves({head, middle, tail, visited}, moves) =>
      let (current, remaining) = moves |> List.uncons |> Option.getOrThrow;

      HeadMove(
        {
          head, // TODO: advance head? might make more sense like it is now...
          middle,
          tail,
          visited,
        },
        {current, remaining},
      );

    | HeadMove({head, middle, tail, visited}, {current, remaining}) =>
      MovingMiddle({
        
      })
    };
};

// let dataFile = Day09.testData;
// let data = Shared_File.read(dataFile);
// let moves = data |> Day09.parseMoves;

// let states =

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

module StateDisplay = {
  [@react.component]
  let make = (~offset, ~count) => {
    <div>
       <div /> </div>;
      // let states =
  };
};

module App = {
  [@react.component]
  let make = () => {
    let (countText, setCountText) = React.useState(() => "5");

    let (count, setCount) = React.useState(() => 5);

    let (offsetText, setOffsetText) = React.useState(() => "0");

    let (offset, setOffset) = React.useState(() => 0);

    let (data, setData) =
      React.useState(() => {j|R 4
U 4
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
        {List.makeWithIndex(count, id)
         |> List.map(x => <div> {x |> Int.toString |> React.string} </div>)
         //  <State
         /***
          //               <pre className=Styles.box>
          //                 {{j|
          //             $x ($offset)
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // .........2345.............
          // ........1...6.............
          // ........H...7.............
          // ............8.............
          // ............9.............
          // ..........................
          // ..........................
          // ...........s..............
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          // ..........................
          //             |j}
          //                  |> React.string}
          //               </pre>
          */
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
