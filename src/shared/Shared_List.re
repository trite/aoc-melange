let split = (~delimiter) =>
  Relude.List.foldLeft(
    (b, a) =>
      a == delimiter
        ? b |> Relude.List.append([])
        : b
          |> List.initOrEmpty
          |> Relude.List.append(
               b
               |> Relude.List.last
               |> Option.getOrElse([])
               |> Relude.List.append(a),
             ),
    [[]],
  );
