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

let split = (~delimiter, lst) => {
  let rec go = (accOuter, accInner, delim, lst) =>
    switch(lst |> List.uncons) {
    | None =>
      List.cons(accInner, accOuter)
      |> List.reverse
    | Some((x, xs)) =>
      x == delimiter
      ? go(List.cons(accInner, accOuter), [], delim, xs)
      : go(accOuter, List.cons(x, accInner), delim, xs)
    };
  go([], [], delimiter, lst)
};


testInput
|> String.splitList(~delimiter="\n")
|> split(~delimiter="")
|> List.toArray
|> Array.map(List.reverse >> List.toArray)
|> Js.log