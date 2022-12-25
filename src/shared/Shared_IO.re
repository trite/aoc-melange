let map = IO.map;

let readFile = (path) =>
  IO.suspend(
    () => Node.Fs.readFileAsUtf8Sync(path)
  );

type multipleTestHandler =
  (
    string,
    string => string,
    int
  ) => array(Result.t(string, string));

type testStyle =
  | SingleTest
  | MultipleTests(multipleTestHandler);

let partNumberType = (type_, num) =>
  {j|Part $num $type_|j};

let testSingle = "Test  ";
let testMultiple = "Test";
let result = "Result";

let test = (style, num) =>
  switch(style) {
  | SingleTest       => partNumberType(testSingle, num)
  | MultipleTests(_) => partNumberType(testMultiple, num)
  };

let result = partNumberType(result);

let unsafeRunAndLog = (description, io) =>
  io |> IO.unsafeRunAsync(Result.fold(
    (++)("ERROR: ") >> Js.log,
    (++)(description ++ " : ") >> Js.log,
  ));

let readRunLog = (~path, ~work, ~description) =>
  readFile(path)
  |> map(work)
  |> unsafeRunAndLog(description);

let readRunLogTest =
    (path, work, description, testStyle, partNum) =>
  switch(testStyle) {
  | SingleTest => readRunLog(~path, ~work, ~description)
  | MultipleTests(handler) =>
    readFile(path)
    |> map(contents => handler(contents, work, partNum))
    |> IO.unsafeRunAsync(
      Result.fold(
        Js.log,
        Array.forEach(
          Result.fold(
            (++)(description ++ " (Failure) : ") >> Js.log,
            (++)(description ++ " (Success) : ") >> Js.log,
          )
        )
      )
    )
  };

let multipleTestSpacing = style =>
  style == SingleTest
  ? ()
  : Js.log("");

let readRunLogAll =
  (
    ~testData,
    ~problemData,
    ~part1,
    ~part2,
    ~testStyle: testStyle = SingleTest,
    ()
  ) =>
  {
    let test = test(testStyle);

    readRunLogTest(testData, part1, test(1), testStyle, 1);
    multipleTestSpacing(testStyle);

    readRunLog(
      ~path=problemData,
      ~work=part1,
      ~description=result(1)
    );
    multipleTestSpacing(testStyle);

    readRunLogTest(testData, part2, test(2), testStyle, 2);
    multipleTestSpacing(testStyle);

    readRunLog(
      ~path=problemData,
      ~work=part2,
      ~description=result(2)
    );
    multipleTestSpacing(testStyle);
  };
