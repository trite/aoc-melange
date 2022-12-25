let map : ('a => 'b, IO.t('a, 'c)) => IO.t('b, 'c);

let readFile : string => IO.t(string, 'a);

type multipleTestHandler =
  (
    string,
    string => string,
    int
  ) => array(Result.t(string, string));

type testStyle =
  | SingleTest
  | MultipleTests(multipleTestHandler);

// let logWithDescription : (string, string) => IO.t(unit, 'a);

// let unsafeRunAndLog : IO.t(string, string) => unit;

let unsafeRunAndLog : (string, IO.t(string, string)) => unit;

let test : testStyle => int => string;
let result : int => string;

let readRunLog : 
  (
    ~path: string,
    ~work: string => string, 
    ~description: string
  ) => unit;

// let readRunLogAll :
//   (
//     ~testData: string,
//     ~problemData: string,
//     ~part1: string => string,
//     ~part2: string => string,
//     unit
//   ) => unit;


let readRunLogAll : 
  (
    ~testData: string,
    ~problemData: string,
    ~part1: string => string,
    ~part2: string => string,
    ~testStyle: testStyle=?,
    unit
  ) => unit