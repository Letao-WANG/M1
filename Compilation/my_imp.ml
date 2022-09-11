(**
    Operation binary : +, *, <
 *)
type binop = Add | Mul | Lt


(**
    
 *)
type expression = 
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * expression * expression
  | Call  of string * expression list
(**
    (1 + x) * f(3, true)
 *)
let e = Binop(Mul, Binop(Add, Cst 1, Var "x"), Call("f", [Cst 3; Bool true]))


type instruction = 
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
and sequence = instruction list
(**
  while (c < 58){
    putchar(c);
    c = c + 1;
  }
 *)
let i = While(Binop(Lt, Var "c", Cst 58), [Putchar(Var "c"); Set("c", Binop(Add, Var "c", Cst 1))])


type function_def = {
  name: string;
  params: string list;
  locals: string list;
  code: sequence;
}
(**
  function chiffres(depart){
    var c;
    c = depart + 48;
    while (c < 58){
      putchar(c);
      c = c + 1;
    }
  }
 *)
let f = {
  name = "chiffres";
  params = ["depart"];
  locals = ["c"];
  code = [Set("c", Binop(Add, Var "depart", Cst 48)); i]
}


type program = {
  globals: string list;
  functions: function_def list;
}
(**
  var zero;

  function main(){
    zero = 0;
    chiffres(zero);
  }

  function chiffres(depart){
    var c;
    c = depart + 48;
    while (c < 58){
      putchar(c);
      c = c + 1;
    }
  }
 *)
let p = {
  globals = ["zero"];
  functions = [{
    name = "main";
    params = [];
    locals = [];
    code = [Set("zero", Cst 0); Expr(Call("chiffres", [Var "zero"]))]}; f]
}
