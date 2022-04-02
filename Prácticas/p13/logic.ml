type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp

type oper = Not

type biOper = Or | And | If | Iff

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop

let rec prop_of_log_exp =
    let p = prop_of_log_exp in function
        Const x -> C x
      | Var x -> V x
      | Neg x -> Op (Not, p x)
      | Disj (x, y) -> BiOp (Or, p x, p y)
      | Conj (x, y) -> BiOp (And, p x, p y)        
      | Cond (x, y) -> BiOp (If, p x, p y)
      | BiCond (x, y) -> BiOp (Iff, p x, p y);;

let rec log_exp_of_prop =
    let p = log_exp_of_prop in function
        C x -> Const x
      | V x -> Var x
      | Op (_, x) -> Neg (p x)
      | BiOp (op, x, y) -> match op with
                              Or  -> Disj   (p x, p y)
                            | And -> Conj   (p x, p y)
                            | If  -> Cond   (p x, p y)
                            | Iff -> BiCond (p x, p y);;

let opval = function
    Not -> not;;

let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> (fun p q -> p || (not q) )
  | Iff -> (fun p q -> (not (p || q)) || (p && q) );;

let rec peval aux = function
    C x -> x
  | V x -> List.assoc x aux
  | Op (op, p) -> (opval op) (peval aux p)
  | BiOp (op, p1, p2) -> (biopval op) (peval aux p1) (peval aux p2);;

let is_tau p =
     peval [ ("p", false); ("q", false) ] p &&
     peval [ ("p", true ); ("q", true ) ] p &&
     peval [ ("p", true ); ("q", false) ] p &&
     peval [ ("p", false); ("q", true ) ] p;;
     
     
     
     
     
