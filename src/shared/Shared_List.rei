let split : (~delimiter: 'a, list('a)) => list(list('a));

let toTuple2 : list('a) => ('a, 'a);
let toTuple3 : list('a) => ('a, 'a, 'a);

let transpose : list(list('a)) => list(list('a));

// let unsafeHead : list('a) => 'a;
// let unsafeTail : list('a) => list('a);