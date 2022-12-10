type move =
  | Rock
  | Paper
  | Scissors

let moveToScore =
  fun 
  | Rock     => 1
  | Paper    => 2
  | Scissors => 3;

type outcome =
  | Lose
  | Draw
  | Win;

let outcomeToScore =
  fun
  | Lose => 0
  | Draw => 3
  | Win  => 6;

let parseMove =
  fun
  | "A" | "X" => Rock
  | "B" | "Y" => Paper
  | "C" | "Z" => Scissors
  | _ => raise(Failure("Cannot parse move"));

let parseString =
  String.splitList(~delimiter=" ")
  >> fun
     | [a, b, ..._rest] => (a |> parseMove, b |> parseMove)
     | _ => raise(Failure("Cannot parse string"));

let doWork = (description, calculation, data) => 
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    parseString
    >> calculation)
  |> List.Int.sum
  |> Int.toString
  |> (++)(description ++ " : ")
  |> Js.log;

let calculatePart1 = ((opponent, self)) => {
  let result =
    switch(opponent, self) {
    | (Rock, Rock)     | (Paper, Paper)    | (Scissors, Scissors) => Draw
    | (Rock, Paper)    | (Paper, Scissors) | (Scissors, Rock)     => Win
    | (Rock, Scissors) | (Paper, Rock)     | (Scissors, Paper)    => Lose
    };

  (self |> moveToScore) + (result |> outcomeToScore)
};

Shared.File.read("data/2022/day02test.txt")
|> doWork("Part 1 Test  ", calculatePart1);

Shared.File.read("data/2022/day02.txt")
|> doWork("Part 1 Result", calculatePart1);





/* Part 2 */

// map what we thought was our move to become our goal for the round's outcome
let convertMoveToOutcome =
  fun
  | Rock     => Lose
  | Paper    => Draw
  | Scissors => Win;

let loseTo =
  fun
  | Rock     => Scissors
  | Paper    => Rock
  | Scissors => Paper;

let winAgainst =
  fun
  | Rock     => Paper
  | Paper    => Scissors
  | Scissors => Rock;

let calculatePart2 = ((opponent, self)) => {
  switch(self |> convertMoveToOutcome) {
  | Lose => (Lose |> outcomeToScore) + (opponent |> loseTo     |> moveToScore)
  | Draw => (Draw |> outcomeToScore) + (opponent |>               moveToScore)
  | Win  => (Win  |> outcomeToScore) + (opponent |> winAgainst |> moveToScore)
  }
};

Shared.File.read("data/2022/day02test.txt")
|> doWork("Part 2 Test  ", calculatePart2);

Shared.File.read("data/2022/day02.txt")
|> doWork("Part 2 Result", calculatePart2);

/*
$ node _build/default/src/2022/Day02.bs.js
Part 1 Test   : 15
Part 1 Result : 8933
Part 2 Test   : 12
Part 2 Result : 11998
*/