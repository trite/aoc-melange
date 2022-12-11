let split = (~delimiter) => Relude.List.foldl([[]], (b, a) => a == delimiter ? [...b, []] : b |> List.last |> List.append(a));
