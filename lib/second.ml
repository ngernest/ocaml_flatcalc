open Base 
open Base_quickcheck

type binop = Add | Sub | Mul | Div 
[@@deriving sexp, quickcheck]


(** In this "flattened" implementation, references are just indices into a 
    array where the [expr]s are stored. We use the "newtype" pattern 
    instead of a plain [int] here to clarify where the
    reference is supposed to be used. 
    - For efficiency purposes, this type is unboxed. *)
type expr_ref = ExprRef of int [@@unboxed]
[@@deriving sexp]

type expr = 
  | Binary of binop * expr_ref * expr_ref
  | Literal of int 
[@@deriving sexp]

(** An "arena" for storing expressions that can refer to each other. 
    This is just a plain, dense array where a family of [expr]s live.
    - For efficiency purposes, this type is unboxed. *)
type expr_pool = ExprPool of expr array [@@unboxed]

(** Create an empty pool. *)
let default () : expr_pool = 
  ExprPool (Array.create ~len:100_000_000 (Literal 0))

(** Dereference an AST node reference, obtaining the underlying [expr] *)  
let get (ExprPool pool : expr_pool) (ExprRef eref : expr_ref) : expr = 
  pool.(eref)

(** Add an expression to the pool and get a reference to it *)  
let add (ExprPool pool : expr_pool) (e : expr) : expr_ref = 
  let idx = Array.length pool in 
  pool.(idx - 1) <- e;
  ExprRef idx

(** An alternative interpreter that exploits the flat structure. 
   Instead of recursively traversing from the root, we take advantage of the fact that our
   expressions only refer "backward" in the pool. Therefore, it suffices to evaluate each
   expression in the pool {i in order}. No recursion required. *)
let flat_interp (ExprPool pool : expr_pool) (ExprRef root : expr_ref) : int = 
  let len = Array.length pool in 
  let state : int array = Array.create ~len 0 in 
  Array.iteri pool ~f:(fun i expr -> 
    let res = 
      (match expr with 
      | Binary(op, ExprRef l, ExprRef r) -> 
        let lhs = state.(l) in 
        let rhs = state.(r) in 
        (match op with 
        | Add -> lhs + rhs 
        | Sub -> lhs - rhs 
        | Mul -> lhs * rhs 
        | Div -> if rhs = 0 then 0 else lhs / rhs)
      | Literal num -> num) in 
    state.(i) <- res);
  state.(root)

  
let rec gen_expr (expr_pool : expr_pool) (lit_prob_inv: int) : expr_ref Generator.t = 
  let open Generator.Let_syntax in 
  let unif = Generator.int_uniform_inclusive 0 100 in 
  let p = 1.0 /. Float.of_int lit_prob_inv in 
  Generator.weighted_union [
    (p, let%map i = unif in add expr_pool (Literal i));
    (1.0 -. p, 
      let%map lhs = gen_expr expr_pool (lit_prob_inv / 2) 
      and rhs = gen_expr expr_pool (lit_prob_inv / 2) 
      and op = quickcheck_generator_binop in 
      add expr_pool @@ Binary (op, lhs, rhs))
  ]