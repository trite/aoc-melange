// module Graph = {
/***
  Graph direction
   * Traditional algebraic graph: UpPositive, RightPositive
   * Screen coords sometimes: UpNegative, RightPositive,
   * Not sure if the yAxis flip will be helpful or useless
 */

type xAxis =
  | UpPositive
  | UpNegative;

// type yAxis =
//   | RightPositive
//   | RightNegative;

type t('a) = {
  matrix: array(array('a)),
  xAxis,
  // originOffset is where (0,0) lives compared to absolute origin
  originOffset: Position.t,
};

let absoluteOrigin: Position.t = {x: 0, y: 0};
