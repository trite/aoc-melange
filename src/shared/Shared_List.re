// TODO: pretty sure there are better ways to implement this
let split = (~delimiter, lst) => {
  let rec go = (accList, acc, delimiter, lst) =>
    switch(lst) {
    | [] =>
      [acc, ...accList]
      |> Relude.List.reverse
    | [x, ...xs] =>
      x == delimiter
      ? go([acc, ...accList], [], delimiter, xs)
      : go(accList, [x, ...acc], delimiter, xs)
    };
  go([], [], delimiter, lst)
  |> Relude.List.map(Relude.List.reverse)
};