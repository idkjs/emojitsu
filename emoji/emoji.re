/** Zero Width Joiner: https://en.wikipedia.org/wiki/Zero-width_joiner */

let zwj = "‍";

let zwj_code_point = 0x200D;

[@deriving show]
type t = {
  code_points: list(int),
  string,
  name: string,
};

[@deriving show]
type table = list(t);
