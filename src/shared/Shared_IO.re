let readFile = (path) =>
  IO.suspend(
    () => Node.Fs.readFileAsUtf8Sync(path)
  );

// let logWithDescription = (description, toLog) =>
//   IO.suspend(() =>
//     toLog
//     |> (++)(description ++ " : ")
//     |> Js.log
//   );

let unsafeRunAndLog = (description, io) =>
  io |> IO.unsafeRunAsync(Result.fold(
    (++)("ERROR: ") >> Js.log,
    (++)(description ++ " : ") >> Js.log,
  ));