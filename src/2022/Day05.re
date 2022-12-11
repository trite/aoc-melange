let parseInput = {
  let parseLayoutAndMoves = ((layout, moves)) => {
    let parseLayoutLine = line => {
      let rec go = (acc, rest) =>
        rest
        |> fun
        | [ _lb, x, _rb, _space, ...r ] =>
          go([x, ...acc], r)
        | [ _lb, x, _rb ] =>
          [x, ...acc] |> List.reverse |> List.toArray
        | _ => raise(Failure("This shouldn't happen"));
        
      line
      |> String.toList
      |> go([])
    }

    let parseLayout = 
      Array.map(parseLayoutLine);

    let parseMoves = id; // TODO

    (
      layout |> parseLayout,
      moves |> parseMoves
    )
  };

  String.splitList(~delimiter="\n")
  >> Shared.List.split(~delimiter = "")
  >> List.toArray
  >> Array.map(List.toArray)
  >> Shared.Array.arrayToTuple2
  >> parseLayoutAndMoves;
}

let doWork = (_description, data) =>
  data
  |> parseInput
  |> Js.log;

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