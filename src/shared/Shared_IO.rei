let map : ('a => 'b, IO.t('a, 'c)) => IO.t('b, 'c);

let readFile : string => IO.t(string, 'a);

// let logWithDescription : (string, string) => IO.t(unit, 'a);

// let unsafeRunAndLog : IO.t(string, string) => unit;

let unsafeRunAndLog : (string, IO.t(string, string)) => unit;

let test : int => string;
let result : int => string;

let readRunLog : (string, string => string, string) => unit;

let readRunLogAll :
  (
    ~testData: string,
    ~problemData: string,
    ~part1: string => string,
    ~part2: string => string,
    unit
  ) => unit;