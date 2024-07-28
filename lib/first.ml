open Base
open Base_quickcheck

type binop = Add | Sub | Mul | Div [@@deriving sexp, quickcheck]
type expr = Binary of binop * expr * expr | Literal of int [@@deriving sexp]

let rec gen_expr (lit_prob_inv : int) : expr Generator.t =
  let open Generator.Let_syntax in
  let unif = Generator.int_uniform_inclusive 0 100 in
  let p = 1.0 /. Float.of_int lit_prob_inv in
  Generator.weighted_union
    [ ( p,
        let%map i = unif in
        Literal i );
      ( 1.0 -. p,
        let%map lhs = gen_expr (lit_prob_inv / 2)
        and rhs = gen_expr (lit_prob_inv / 2)
        and op = quickcheck_generator_binop in
        Binary (op, lhs, rhs) )
    ]

let rec interp (e : expr) : int =
  match e with
  | Binary (op, lhs, rhs) -> (
    let lhs = interp lhs in
    let rhs = interp rhs in
    match op with
    | Add -> lhs + rhs
    | Sub -> lhs - rhs
    | Mul -> lhs * rhs
    | Div -> lhs / rhs)
  | Literal num -> num
