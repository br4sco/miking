include "char.mc"
include "option.mc"
include "seq.mc"
include "string.mc"
include "assoc.mc"

let spacing = lam indent. makeSeq indent ' '
let newline = lam indent. concat "\n" (spacing indent)

-- Set spacing on increment
let incr = lam indent. addi indent 2

let symbolDelim = "'"

---------------------------------
-- PRETTY PRINTING ENVIRONMENT --
---------------------------------

type Env = {

  -- Used to keep track of strings assigned to names with symbols
  nameMap: AssocMap Name String,

  -- Used to keep track of strings assigned to names without symbols
  strMap: AssocMap String String,

  -- Count the number of occurrences of each (base) string to assist with
  -- assigning unique strings.
  count: AssocMap String Int

}

let pprintHelperEnvEmpty =
  {nameMap = assocEmpty, strMap = assocEmpty, count = assocEmpty}

let _ppLookupName = assocLookup {eq = nameEqSym}
let _ppLookupStr = assocLookup {eq = eqstr}
let _ppInsertName = assocInsert {eq = nameEqSym}
let _ppInsertStr = assocInsert {eq = eqstr}

-- Look up the string associated with a name in the environment
let _lookup : Name -> Env -> Option String = lam name. lam env.
  match env with { nameMap = nameMap, strMap = strMap } then
    match _ppLookupName name nameMap with Some str then
      Some str
    else match _ppLookupStr (nameGetStr name) strMap with Some str then
      Some str
    else None ()
  else never

-- Check if a string is free in the environment.
let _free : String -> Env -> Bool = lam str. lam env.
  match env with { nameMap = nameMap, strMap = strMap } then
    let f = lam _. lam v. eqstr str v in
    not (or (assocAny f nameMap) (assocAny f strMap))
  else never

-- Add a binding to the environment
let _add : Name -> String -> Int -> Env -> Env =
  lam name. lam str. lam i. lam env.
    let baseStr = nameGetStr name in
    match env with {nameMap = nameMap, strMap = strMap, count = count} then
      let count = _ppInsertStr baseStr i count in
      if nameHasSym name then
        let nameMap = _ppInsertName name str nameMap in
        {nameMap = nameMap, strMap = strMap, count = count}
      else
        let strMap = _ppInsertStr baseStr str strMap in
        {nameMap = nameMap, strMap = strMap, count = count}
    else never

-- Get a string for the current name. Returns both the string and a new
-- environment.
let pprintHelperGetStr : Name -> Env -> (Env, String) = lam name. lam env.
  match _lookup name env with Some str then (env,str)
  else
    let baseStr = nameGetStr name in
    if _free baseStr env then (_add name baseStr 1 env, baseStr)
    else
      match env with {count = count} then
        let start = match _ppLookupStr baseStr count with Some i then i else 1 in
        recursive let findFree : String -> Int -> (String, Int) =
          lam baseStr. lam i.
            let proposal = concat baseStr (int2string i) in
            if _free proposal env then (proposal, i)
            else findFree baseStr (addi i 1)
        in
        match findFree baseStr start with (str, i) then
          (_add name str (addi i 1) env, str)
        else never
      else never
