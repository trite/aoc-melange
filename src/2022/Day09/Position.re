type t = {
  x: int,
  y: int,
};

let fromTuple: ((int, int)) => t = ((x, y)) => {x, y};

let toTuple: t => (int, int) = ({x, y}) => (x, y);

let compare: (t, t) => BsBastet.Interface.ordering =
  ({x: x1, y: y1}, {x: x2, y: y2}) =>
    switch (Int.compare(x1, x2)) {
    | `equal_to => Int.compare(y1, y2)
    | other => other
    };

let eq: (t, t) => bool = (p1, p2) => compare(p1, p2) == `equal_to;

module Ord = {
  type nonrec t = t;
  let compare = compare;
  let eq = eq;
};
module Set = Set.WithOrd(Ord);

let applyTranslation: (Translation.t, t) => t =
  ({dx, dy}, {x, y}) => {x: x + dx, y: y + dy};

let distance: (t, t) => Translation.t =
  ({x: x1, y: y1}, {x: x2, y: y2}) => {dx: x1 - x2, dy: y1 - y2};
