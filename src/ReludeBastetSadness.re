let test = (x: list(list(string))) =>
  x |> List.map(List.reverse);

/*
File "ReludeBastetSadness.re", line 2, characters 2-3:
2 |   x |> List.map(List.reverse);
      ^
Error: This expression has type string list list
       but an expression was expected of type 'a list BsBastet.List.Functor.t
       BsBastet.List.Functor.t is abstract because no corresponding cmi file was found in path.
*/