let readFile : string => IO.t(string, 'a);

// let logWithDescription : (string, string) => IO.t(unit, 'a);

// let unsafeRunAndLog : IO.t(string, string) => unit;

let unsafeRunAndLog : (string, IO.t(string, string)) => unit;