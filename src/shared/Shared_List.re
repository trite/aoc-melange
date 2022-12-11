module L = Relude.List;
module O = Relude.Option;

let split = (~delimiter) =>
  L.foldLeft(
    (b, a) =>
      a == delimiter
        ? b |> L.append([])
        : b
          |> List.initOrEmpty
          |> L.append(
               b
               |> L.last
               |> O.getOrElse([])
               |> L.append(a),
             ),
    [[]],
  );

let toTuple2 =
  fun
  | [a, b] => (a, b)
  | _ => raise(Failure("Cannot convert array to Tuple2 - invalid input"));

let toTuple3 =
  fun
  | [a, b, c] => (a, b, c)
  | _ => raise(Failure("Cannot convert array to Tuple3 - invalid input"));

let unsafeHead = (lst) =>
  lst
  |> L.head
  |> O.getOrThrow;

let unsafeTail = (lst) =>
  lst
  |> L.tail
  |> O.getOrThrow;

/* Not using this version, but leaving here for now to poke at

  From https://stackoverflow.com/a/3989823

  Ocaml implementation:
    let rec transpose list =
      function
      | []             -> []
      | []      :: xss -> transpose xss
      | (x::xs) :: xss ->
          List.(
            (x :: map hd xss) :: transpose (xs :: map tl xss)
          ) 
*/
let rec _transpose_not_used =
  fun
  | [] => []
  | [[], ...xss] => xss |> _transpose_not_used
  | [[x, ...xs], ...xss] =>
      [
        [x, ...L.map(unsafeHead, xss)], 
        ..._transpose_not_used([xs, ...L.map(unsafeTail, xss)])
      ];

/* TODO: Still have a hard time understanding this, should spend more time with it at some point

  From: https://stackoverflow.com/a/56599499

  OCaml implementation:
    let rec transpose =
      function
      | [] 
      | [] :: _ -> []
      | rows    -> 
          List.map List.hd rows :: transpose (List.map List.tl rows)
*/
let rec transpose =
  fun
  | [] | [[], ..._] => []
  | rows =>
    [L.map(unsafeHead, rows), ...transpose(L.map(unsafeTail, rows))]