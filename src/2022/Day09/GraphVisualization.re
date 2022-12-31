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
      flexDirection(rowReverse),
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

module App = {
  [@react.component]
  let make = () => {
    let (countText, setCountText) = React.useState(() => "5");

    let (count, setCount) = React.useState(() => 5);

    let onChange = (e: ReactEvent.Form.t): unit => {
      let value = e->ReactEvent.Form.target##value;

      value
      |> String.toInt
      |> (
        fun
        | Some(x) when 1 <= x && x <= 5 => setCount(_ => x)
        | Some(_)
        | None => ()
      );

      setCountText(value);
    };

    <div>
      <input type_="text" onChange value=countText />
      <div className=Styles.container>
        {List.makeWithIndex(count, id)
         |> List.map(x =>
              <pre className=Styles.box>
                {{j|
            $x
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
.........2345.............
........1...6.............
........H...7.............
............8.............
............9.............
..........................
..........................
...........s..............
..........................
..........................
..........................
..........................
..........................
            |j}
                 |> React.string}
              </pre>
            )
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
