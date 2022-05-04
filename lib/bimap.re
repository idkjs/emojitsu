/** A bidirecitonal map between As and Bs */
module type S = {
  type t;

  module A: {type key;};

  module B: {type key;};

  let empty: t;

  let is_empty: t => bool;

  let add: (A.key, B.key, t) => t;

  module FromA: {
    let mem: (A.key, t) => bool;

    let find_opt: (A.key, t) => option(B.key);
  };

  module FromB: {
    let mem: (B.key, t) => bool;

    let find_opt: (B.key, t) => option(A.key);
  };
};

module Make =
       (As: Map.OrderedType, Bs: Map.OrderedType)
       : (S with type A.key = As.t and type B.key = Bs.t) => {
  module A = {
    include Map.Make(As);

    type nonrec t = t(Bs.t);
  };

  module B = {
    include Map.Make(Bs);

    type nonrec t = t(A.key);
  };

  type t = {
    a: A.t,
    b: B.t,
  };

  let empty: t = ({a: A.empty, b: B.empty}: t);

  let cardinal: t => int = (t => A.cardinal(t.a): t => int);

  let is_empty = t => cardinal(t) == 0;

  let add: (A.key, B.key, t) => t = (
    (a, b, t) => {a: A.add(a, b, t.a), b: B.add(b, a, t.b)}:
      (A.key, B.key, t) => t
  );

  /** Asymetric operations from As to Bs */
  module FromA = {
    let mem: (A.key, t) => bool = (
      (a, t) => A.mem(a, t.a): (A.key, t) => bool
    );

    let find_opt: (A.key, t) => option(B.key) = (
      (a, t) => A.find_opt(a, t.a): (A.key, t) => option(B.key)
    );
  };

  /** Asymetric operations from Bs to As */
  module FromB = {
    type nonrec t = t;

    let mem: (B.key, t) => bool = (
      (b, t) => B.mem(b, t.b): (B.key, t) => bool
    );

    let find_opt: (B.key, t) => option(A.key) = (
      (b, t) => B.find_opt(b, t.b): (B.key, t) => option(A.key)
    );
  };
};
