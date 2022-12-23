let map = IO.map;

let readFile = (path) =>
  IO.suspend(
    () => Node.Fs.readFileAsUtf8Sync(path)
  );

let partNumberType = (type_, num) =>
  {j|Part $num $type_|j};

let test = "Test  ";
let result = "Result";

let test = partNumberType(test);
let result = partNumberType(result);

let unsafeRunAndLog = (description, io) =>
  io |> IO.unsafeRunAsync(Result.fold(
    (++)("ERROR: ") >> Js.log,
    (++)(description ++ " : ") >> Js.log,
  ));

let readRunLog = (path, work, description) =>
  readFile(path)
  |> map(work)
  |> unsafeRunAndLog(description);

let readRunLogAll =
    (~testData, ~problemData, ~part1, ~part2, ()) =>
  {
    readRunLog(testData,    part1, test(1));
    readRunLog(problemData, part1, result(1));
    readRunLog(testData,    part2, test(2));
    readRunLog(problemData, part2, result(2));
  };
