// module Position = {
  type t = (int, int);

  let compare = ((x1, y1), (x2, y2)) =>
    switch(Int.compare(x1, x2)) {
    | `equal_to => Int.compare(y1, y2)
    | other => other
    };

  let eq = (t1, t2) =>
    compare(t1, t2) == `equal_to;

  module Ord = {
    type nonrec t = t;
    let compare = compare;
    let eq = eq;
  }
  module Set = Set.WithOrd(Ord);
// };
