type move = {
  count: int,
  start: string,
  destination: string
};

let parseInput = {
  let parseLayoutAndMoves = ((layout, moves: list(string))) => {
    let parseLayoutLine = line => {
      let rec go = (acc, rest) =>
        rest
        |> fun
        | [ _lb, x, _rb, _space, ...r ] =>
          go([x, ...acc], r)
        | [ _lb, x, _rb ] =>
          [x, ...acc] |> List.reverse
        | _ => raise(Failure("Unable to parse layout line"));
        
      line
      |> String.toList
      |> go([])
    };

    let parseLayout = 
      List.map(parseLayoutLine)
      >> Shared.List.transpose
      >> List.map(
        List.reverse
        >> List.filter(x => x != " ")
        >> List.uncons 
        >> Option.getOrThrow)
      >> String.Map.fromList
      // >> Map.forEach((s, slist) => Js.log((s, slist |> List.toArray))) // to examine contents
      ; 

    // let parseMove =
    //   String.splitList
    let parseMove =
      fun
      | ["move", count, "from", start, "to", destination] =>
        {
          count: count |> Int.fromString |> Option.getOrThrow, 
          start,
          destination
        }
      | _ => raise(Failure("Unable to parse move!"));

    let parseMoves =
      List.map(
        String.splitList(~delimiter=" ")
        >> parseMove
      )
      // >> List.toArray // to examine contents
      ;
      // >> List.map(parseMove);

    (
      layout |> parseLayout,
      moves |> parseMoves
    )
  };

  String.splitList(~delimiter="\n")
  >> Shared.List.split(~delimiter = "")
  >> Shared.List.toTuple2
  >> parseLayoutAndMoves;
};


let applyMoves = ((layout, moves)) => {
  let moveItem = (layout, start, destination) => {
    let (toMove, startStack) =
      layout
      |> Map.get(start)
      |> Option.flatMap(List.uncons)
      |> Option.getOrThrow;

    let destinationStack =
      layout
      |> Map.get(destination)
      |> Option.getOrThrow
      |> List.cons(toMove);

    layout
    |> Map.update(start, _ => Some(startStack))
    |> Map.update(destination, _ => Some(destinationStack))
  };

  let rec applyMove = (layout, move) =>
    switch(move) {
    | {count: 0, start: _, destination: _} => layout
    | {count: x, start, destination} => 
      applyMove(moveItem(layout, start, destination), {count: x-1, start, destination})
    };

  moves
  |> List.foldLeft(applyMove, layout)
}

// TODO: result was wrong, trying to figure out why
//       might be worth implementing a better way to display the crate layout

let doWork = (_description, data) =>
  data
  |> parseInput
  |> (((layout, _)) => layout)
  // |> (((layout, moves)) =>{
  //   let m = moves |> List.head |> Option.getOrThrow;
  //   applyMove(layout, m)
  // })
  |> Map.map(List.toArray)
  |> Map.toArray
  |> Js.log;
  // |> applyMoves
  // |> Map.toArray
  // |> Array.map(((x, lst)) => (x, lst |> List.reverse |> List.toArray))
  // // |> Array.map(((_, lst)) => lst |> List.reverse |> List.head)
  // |> Js.log;

Shared.File.read("data/2022/day05test.txt")
|> doWork("Part 1 Test  ");

/*
[
  [
    [ ' ', 'D', ' ' ],
    [ 'N', 'C', ' ' ],
    [ 'Z', 'M', 'P' ],
    [ '1', '2', '3' ]
  ],
  [
    'move 1 from 2 to 1',
    'move 3 from 1 to 3',
    'move 2 from 2 to 1',
    'move 1 from 1 to 2'
  ]
]
*/



// Shared.File.read("data/2022/day05.txt")
// |> doWork("Part 1 Result");

// Shared.File.read("data/2022/day05test.txt")
// |> doWork("Part 2 Test  ");

// Shared.File.read("data/2022/day05.txt")
// |> doWork("Part 2 Result");