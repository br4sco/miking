-- This file contains language fragments and functions related to free
-- variables.

include "set.mc"
include "name.mc"

include "mexpr/ast.mc"
include "mexpr/symbolize.mc"
include "mexpr/boot-parser.mc"

lang FreeVars = Ast
  -- Returns the set of free variables for a given expression.
  sem freeVars : Expr -> Set Name
  sem freeVars =| t -> setOfKeys (freeVarsCount t)

  -- Returns the set of free variables and their number of occurances for a
  -- given expression.
  sem freeVarsCount : Expr -> Map Name Int
  sem freeVarsCount =| t -> freeVarsCountExpr (mapEmpty nameCmp) t

  sem freeVarsCountExpr : Map Name Int -> Expr -> Map Name Int
  sem freeVarsCountExpr acc =
  | t -> sfold_Expr_Expr freeVarsCountExpr acc t
end

lang VarFreeVars = FreeVars + VarAst
  sem freeVarsCountExpr acc =
  | TmVar r -> mapInsertWith addi r.ident 1 acc
end

lang LamFreeVars = FreeVars + LamAst
  sem freeVarsCountExpr acc =
  | TmLam r ->
    mapUnionWith addi acc (mapRemove r.ident (freeVarsCount r.body))
end

lang LetFreeVars = FreeVars + LetAst
  sem freeVarsCountExpr acc =
  | TmLet r ->
    mapUnionWith addi acc
      (freeVarsCountExpr (mapRemove r.ident (freeVarsCount r.inexpr)) r.body)
end

lang RecLetsFreeVars = FreeVars + RecLetsAst
  sem freeVarsCountExpr acc =
  | TmRecLets r ->
    let acc = foldl (lam acc. lam b.
      freeVarsCountExpr acc b.body) (freeVarsCountExpr acc r.inexpr) r.bindings
    in
    foldl (lam acc. lam b. mapRemove b.ident acc) acc r.bindings
end

lang MatchFreeVars = FreeVars + MatchAst + NamedPat + SeqEdgePat
  sem freeVarsCountExpr acc =
  | TmMatch r ->
    mapUnionWith addi acc
      (freeVarsCountExpr
         (freeVarsCountExpr
            (bindVarsCountPat
               (freeVarsCount r.thn)
               r.pat)
            r.els)
         r.target)

  sem bindVarsCountPat : Map Name Int -> Pat -> Map Name Int
  sem bindVarsCountPat acc =
  | PatNamed {ident = PName ident} -> mapRemove ident acc
  | pat & (PatSeqEdge {middle = PName ident}) ->
    let acc = mapRemove ident acc in
    sfold_Pat_Pat bindVarsCountPat acc pat
  | pat -> sfold_Pat_Pat bindVarsCountPat acc pat
end

lang MExprFreeVars =
  VarFreeVars + LamFreeVars + LetFreeVars + RecLetsFreeVars + MatchFreeVars
end

lang TestLang = MExprFreeVars + MExprSym + BootParser end

mexpr

use TestLang in

let parseProgram : String -> Expr =
  lam str.
    let parseArgs =
      {defaultBootParserParseMExprStringArg with allowFree = true}
    in
    let ast = parseMExprString parseArgs str in
    ast
in

-------------------
-- Test freeVars --
-------------------

let testFreeVars = lam prog.
  let fv = freeVars prog in
  sort cmpString (map nameGetStr (setToSeq fv))
in

let prog = parseProgram "
  lam x. x x y y y
  "
in

utest testFreeVars prog with ["y"] in


let prog = parseProgram "
  let x = z in x x y y y
  "
in

utest testFreeVars prog with ["y", "z"] in


let prog = parseProgram "
  recursive let f = lam x. w f (f x) in
  recursive let g = lam y. z f (g y) in
  w z (f (g u))
  "
in

utest testFreeVars prog with ["u", "w", "z"] in


let prog = parseProgram "
  match u with (x, (y, z)) in
  x y y z z z u w w
  "
in

utest testFreeVars prog with ["u", "w"] in


let prog = parseProgram "
  match t with [x] ++ xs in
    x xs t r
  "
in

utest testFreeVars prog with ["r", "t"] in


let prog = parseProgram "
  match t with [first] ++ mid ++ [last] in
    first mid f r last t
  "
in

utest testFreeVars prog with ["f", "r", "t"] in


let prog = parseProgram "
  x (lam x. x x y y y)
  "
in

utest testFreeVars prog with ["x", "y"] in


let prog = parseProgram "
  x (let x = z in x x y y y)
  "
in

utest testFreeVars prog with ["x", "y", "z"] in


let prog = parseProgram "
  x (match z with x in x x y y y)
  "
in

utest testFreeVars prog with ["x", "y", "z"] in

------------------------
-- Test freeVarsCount --
------------------------

let testFreeVarsCount = lam prog.
  let fv = freeVarsCount prog in
  sort
    (lam x. lam y. cmpString x.0 y.0)
    (map (lam x. (nameGetStr x.0, x.1)) (mapToSeq fv))
in

let prog = parseProgram "
  lam x. x x y y y
  "
in

utest testFreeVarsCount prog with [("y", 3)] in


let prog = parseProgram "
  let x = z in x x y y y
  "
in

utest testFreeVarsCount prog with [("y", 3), ("z", 1)] in


let prog = parseProgram "
  recursive let f = lam x. w f (f x) in
  recursive let g = lam y. z f (g y) in
  w z (f (g u))
  "
in

utest testFreeVarsCount prog with [("u", 1), ("w", 2), ("z", 2)] in


let prog = parseProgram "
  match u with (x, (y, z)) in
  x y y z z z u w w
  "
in

utest testFreeVarsCount prog with [("u", 2), ("w", 2)] in


let prog = parseProgram "
  match t with [x] ++ xs in
    x xs t r
  "
in

utest testFreeVarsCount prog with [("r", 1), ("t", 2)] in


let prog = parseProgram "
  match t with [first] ++ mid ++ [last] in
    first mid f r last t
  "
in

utest testFreeVarsCount prog with [("f", 1), ("r", 1), ("t", 2)] in


let prog = parseProgram "
  x (lam x. x x y y y)
  "
in

utest testFreeVarsCount prog with [("x", 1), ("y", 3)] in


let prog = parseProgram "
  x (let x = z in x x y y y)
  "
in

utest testFreeVarsCount prog with [("x", 1), ("y", 3), ("z", 1)] in


let prog = parseProgram "
  x (match z with x in x x y y y)
  "
in

utest testFreeVarsCount prog with [("x", 1), ("y", 3), ("z", 1)] in

()
