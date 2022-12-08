let testInput = {js|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|js};

// let ( ^:: ) = List.cons;

let split = (~delimiter, lst: list(string)) => {
  let rec go = (accList, acc, delimiter, lst) =>
    switch(lst) {
    | [] =>
      [acc, ...accList]
      |> List.reverse
    | [x, ...xs] =>
      x == delimiter
      ? go([acc, ...accList], [], delimiter, xs)
      : go(accList, [x, ...acc], delimiter, xs)
    };
    // not sure yet which one I like better ^v
    // switch(lst |> List.uncons) {
    // | None =>
    //   List.cons(acc, accList)
    //   |> List.reverse
    // | Some((x, xs)) =>
    //   x == delimiter
    //   ? go(List.cons(acc, accList), [], delimiter, xs)
    //   : go(accList, List.cons(x, acc), delimiter, xs)
    // };
  go([], [], delimiter, lst)
};


testInput
|> String.splitList(~delimiter="\n")
|> split(~delimiter="")
// TODO: y u no work?
/*
File "Day01Part1.re", lines 43-45, characters 0-23:
43 | testInput
44 | |> String.splitList(~delimiter="\n")
45 | |> split(~delimiter="")
Error: This expression has type string list list
       but an expression was expected of type 'a list BsBastet.List.Functor.t
       BsBastet.List.Functor.t is abstract because no corresponding cmi file was found in path.

Can be quickly reproduced with:
  let test = (x: list(list(string))) =>
    x |> List.map(List.reverse);
*/
// |> List.map(List.reverse)
|> List.toArray
|> Array.map(List.reverse >> List.toArray)
|> Js.log;

