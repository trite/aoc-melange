// type t = (int, int);
type t = {
  dx: int,
  dy: int,
};

let fromTuple: ((int, int)) => t = ((dx, dy)) => {dx, dy};

let toTuple: t => (int, int) = ({dx, dy}) => (dx, dy);
