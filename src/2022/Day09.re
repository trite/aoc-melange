let mapRight = (f, (x, y)) =>
  (x, y |> f);

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
  

let part1 = id;

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
  |> List.toArray
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