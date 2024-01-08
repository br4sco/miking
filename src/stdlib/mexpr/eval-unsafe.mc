include "utest.mc"
include "list.mc"
include "map.mc"
include "seq.mc"

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/symbolize.mc"
include "mexpr/ast-builder.mc"

let _uc = unsafeCoerce

lang MExprDeBruijn = MExprAst
  syn Expr =
  | TmVarDeBruijn {
    idx : Int,
    var : {
      ident : Name,
      ty: Type,
      info: Info,
      frozen: Bool
    }
  }

  sem tyTm =
  | TmVarDeBruijn r -> r.var.ty

  sem toDeBruijn : Expr -> Expr
  sem toDeBruijn =| tm -> toDeBruijnExpr listEmpty tm

  sem toDeBruijnExpr : List Name -> Expr -> Expr
  sem toDeBruijnExpr env =
  | TmVar r ->
    optionMapOrElse
      (lam. error "name error")
      (lam idx. TmVarDeBruijn { idx = idx, var = r })
      (listIndex (nameEq r.ident) env)
  | TmLam r ->
    TmLam { r with body = toDeBruijnExpr (Cons (r.ident, env)) r.body }
  | TmLet r ->
    TmLet {
      r with body = toDeBruijnExpr env r.body,
      inexpr = toDeBruijnExpr (Cons (r.ident, env)) r.inexpr
    }
  | TmRecLets r ->
    let env =
      foldl (lam env. lam b. Cons (b.ident, env)) env (reverse r.bindings)
    in
    let bindings =
      map (lam b. { b with body = toDeBruijnExpr env b.body }) r.bindings
    in
    TmRecLets {
      r with bindings = bindings,
      inexpr = toDeBruijnExpr env r.inexpr
    }
  | TmMatch r ->
    let newEnv = toDeBruijnPat env r.pat in
    TmMatch {
      r with target = toDeBruijnExpr env r.target,
      thn = toDeBruijnExpr newEnv r.thn,
      els = toDeBruijnExpr env r.els
    }
  -- | TmExt r ->
  --   TmExt { r with inexpr = toDeBruijnExpr (Cons (r.ident, env)) r.inexpr }
  | tm -> smap_Expr_Expr (toDeBruijnExpr env) tm

  sem toDeBruijnPat env =
  | PatNamed { ident = PName ident } -> Cons (ident, env)
  | PatSeqEdge r ->
    let env = foldl toDeBruijnPat env r.prefix in
    let env =
      match r.middle with PName ident then (Cons (ident, env)) else env
    in
    foldl toDeBruijnPat env r.postfix
  | pat -> sfold_Pat_Pat toDeBruijnPat env pat
end

let _emptyRec = mapEmpty cmpSID
let _0 = stringToSid "0"
let _1 = stringToSid "1"
let _2 = stringToSid "2"
let _3 = stringToSid "3"
let _4 = stringToSid "4"
let _5 = stringToSid "5"
let _6 = stringToSid "6"
let _7 = stringToSid "7"

lang MExprUnsafeEval = MExprDeBruijn + Eq + MExprPrettyPrint
  --NOTE(oerikss, 2024-01-07): Opaque values, hence there will never be any
  --constructors for this type but rather is is a untagged union-type for
  syn Value = -- Int | Float | Bool | [Value] | Map SID Value
              -- | {ident : Name, body : Value, ty : Type}
              -- | Cls<opaque>

  syn ConValue =
  | ConValue (Name, Value, Type)

  syn Expr =
  | TmVal { val : Value, ty : Type, info : Info }

  sem infoTm =
  | TmVal r -> r.info

  sem tyTm =
  | TmVal r -> r.ty

  sem withInfo (info : Info) =
  | TmVal r -> TmVal { r with info = info }

  sem withType (ty : Type) =
  | TmVal r -> TmVal { r with ty = ty }

  sem isAtomic =
  | TmVal _ -> true

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmVal r -> (env, "Value")

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmVal _ -> error "Cannot check equality of values"

  sem unsafeEval : [String] -> Expr -> Expr
  sem unsafeEval arg =| tm ->
    _readback (unsafeEvalExpr arg listEmpty (toDeBruijn tm)) (tyTm tm)

  sem unsafeEvalExpr : [String] -> List Value -> Expr -> Value
  sem unsafeEvalExpr arg env =
  | TmVarDeBruijn r ->
    optionGetOrElse (lam. error "name error") (listNth r.idx env)
  | TmApp r ->
    _uc (unsafeEvalExpr arg env r.lhs) (unsafeEvalExpr arg env r.rhs)
  | TmLam r ->
    _uc (lam v. unsafeEvalExpr arg (Cons (v, env)) r.body)
  | TmLet r ->
    _uc
      (let v = unsafeEvalExpr arg env r.body in
       unsafeEvalExpr arg (Cons (v, env)) r.inexpr)
  | TmRecLets r ->
    _uc (
      switch r.bindings
    case [{body = TmLam lamr}] then
      recursive let f = lam v.
        unsafeEvalExpr arg (Cons (v, (Cons (_uc f, env)))) lamr.body
      in
      unsafeEvalExpr arg (Cons (_uc f, env)) r.inexpr
    case [{body = TmLam lamr1}, {body = TmLam lamr2}] then
      recursive
        let f1 = lam v.
          unsafeEvalExpr arg
            (Cons (v, (Cons (_uc f1, (Cons (_uc f2, env)))))) lamr1.body
        let f2 = lam v.
          unsafeEvalExpr arg
            (Cons (v, (Cons (_uc f1, (Cons (_uc f2, env)))))) lamr2.body
      in
      unsafeEvalExpr arg (Cons (_uc f1, (Cons (_uc f2, env)))) r.inexpr
    case bindings then
      let fixMutual = lam l.
        let l = listMap (lam li. (li,)) l in
        fix (lam self. lam l. listMap (lam li. lam x. li.0 (self l) x) l) l
      in
      let bindings =
        _uc (listMap
               (lam b. lam fs. unsafeEvalExpr arg (listConcat fs env) b.body)
               (listFromSeq bindings))
      in
      unsafeEvalExpr arg (_uc listConcat (fixMutual bindings) env) r.inexpr
    end)
  | TmRecord r ->
    _uc (mapMap (unsafeEvalExpr arg env) r.bindings)
  | TmRecordUpdate r ->
    _uc mapInsert r.key
      (unsafeEvalExpr arg env r.value) (unsafeEvalExpr arg env r.rec)
  | TmSeq r ->
    _uc (map (unsafeEvalExpr arg env) r.tms)
  | TmConApp r ->
    _uc (r.ident, unsafeEvalExpr arg env r.body, tyTm r.body)
  | TmMatch r ->
    optionMapOrElse
      (lam. unsafeEvalExpr arg env r.els)
      (lam env. unsafeEvalExpr arg env r.thn)
      (unsafeTryMatch env (unsafeEvalExpr arg env r.target) r.pat)
  | TmConDef r -> unsafeEvalExpr arg env r.inexpr
  | TmType r -> unsafeEvalExpr arg env r.inexpr
  | TmConst { val = CInt r } -> _uc r.val
  | TmConst { val = CFloat r } -> _uc r.val
  | TmConst { val = CChar r } -> _uc r.val
  | TmConst { val = CBool r } -> _uc r.val
  | TmConst { val = CUnsafeCoerce _ } -> _uc unsafeCoerce
  -- Integer numbers
  | TmConst { val = CAddi _ } -> _uc addi
  | TmConst { val = CSubi _ } -> _uc subi
  | TmConst { val = CMuli _ } -> _uc muli
  | TmConst { val = CDivi _ } -> _uc divi
  | TmConst { val = CModi _ } -> _uc modi
  | TmConst { val = CNegi _ } -> _uc negi
  | TmConst { val = CLti _ } -> _uc lti
  | TmConst { val = CLeqi _ } -> _uc leqi
  | TmConst { val = CGti _ } -> _uc gti
  | TmConst { val = CGeqi _ } -> _uc geqi
  | TmConst { val = CEqi _ } -> _uc eqi
  | TmConst { val = CNeqi _ } -> _uc neqi
  | TmConst { val = CSlli _ } -> _uc slli
  | TmConst { val = CSrli _ } -> _uc srli
  | TmConst { val = CSrai _ } -> _uc srai
  -- , ("arity", Carity ())   -- Arity is not yet implemented
  -- Floating-point numbers
  | TmConst { val = CAddf _ } -> _uc addf
  | TmConst { val = CSubf _ } -> _uc subf
  | TmConst { val = CMulf _ } -> _uc mulf
  | TmConst { val = CDivf _ } -> _uc divf
  | TmConst { val = CNegf _ } -> _uc negf
  | TmConst { val = CLtf _ } -> _uc ltf
  | TmConst { val = CLeqf _ } -> _uc leqf
  | TmConst { val = CGtf _ } -> _uc gtf
  | TmConst { val = CGeqf _ } -> _uc geqf
  | TmConst { val = CEqf _ } -> _uc eqf
  | TmConst { val = CNeqf _ } -> _uc neqf
  | TmConst { val = CFloorfi _ } -> _uc floorfi
  | TmConst { val = CCeilfi _ } -> _uc ceilfi
  | TmConst { val = CRoundfi _ } -> _uc roundfi
  | TmConst { val = CInt2float _ } -> _uc int2float
  | TmConst { val = CStringIsFloat _ } -> _uc stringIsFloat
  | TmConst { val = CString2float _ } -> _uc string2float
  | TmConst { val = CFloat2string _ } -> _uc float2string
  -- Characters
  | TmConst { val = CEqc _ } -> _uc eqc
  | TmConst { val = CChar2Int _ } -> _uc char2int
  | TmConst { val = CInt2Char _ } -> _uc int2char
  -- Sequences
  | TmConst { val = CCreate _ } -> _uc create
  | TmConst { val = CCreateList _ } -> _uc createList
  | TmConst { val = CCreateRope _ } -> _uc createRope
  | TmConst { val = CIsList _ } -> _uc isList
  | TmConst { val = CIsRope _ } -> _uc isRope
  | TmConst { val = CLength _ } -> _uc length
  | TmConst { val = CConcat _ } -> _uc concat
  | TmConst { val = CGet _ } -> _uc get
  | TmConst { val = CSet _ } -> _uc set
  | TmConst { val = CCons _ } -> _uc cons
  | TmConst { val = CSnoc _ } -> _uc snoc
  | TmConst { val = CSplitAt _ } ->
    _uc
      (lam seq. lam i.
        let t = splitAt seq i in
        mapInsert _0 t.0 (mapInsert _1 t.1 _emptyRec))
  | TmConst { val = CReverse _ } -> _uc reverse
  | TmConst { val = CHead _ } -> _uc head
  | TmConst { val = CTail _ } -> _uc tail
  | TmConst { val = CNull _ } -> _uc null
  | TmConst { val = CMap _ } -> _uc map
  | TmConst { val = CMapi _ } -> _uc mapi
  | TmConst { val = CIter _ } -> _uc (lam f. lam seq. iter f seq; _emptyRec)
  | TmConst { val = CIteri _ } -> _uc (lam f. lam seq. iteri f seq; _emptyRec)
  | TmConst { val = CFoldl _ } -> _uc foldl
  | TmConst { val = CFoldr _ } -> _uc foldr
  | TmConst { val = CSubsequence _ } -> _uc subsequence
  -- Random numbers
  | TmConst { val = CRandIntU _ } -> _uc randIntU
  | TmConst { val = CRandSetSeed _ } -> _uc (lam s. randSetSeed s; _emptyRec)
  -- MCore intrinsics: Time
  | TmConst { val = CWallTimeMs _ } -> _uc (lam. wallTimeMs ())
  | TmConst { val = CSleepMs _ } -> _uc (lam t. sleepMs t; _emptyRec)
  -- MCore intrinsics: Debug and I/O
  | TmConst { val = CPrint _ } -> _uc (lam str. print str; _emptyRec)
  | TmConst { val = CPrintError _ } -> _uc (lam str. printError str; _emptyRec)
  | TmConst { val = CDPrint _ } -> _uc (lam x. dprint x; _emptyRec)
  | TmConst { val = CFlushStdout _ } -> _uc (lam. flushStdout (); _emptyRec)
  | TmConst { val = CFlushStderr _ } -> _uc (lam. flushStderr (); _emptyRec)
  | TmConst { val = CReadLine _ } -> _uc (lam. readLine ())
  -- | TmConst { val = CReadBytesAsString _ } ->
  --   _uc
  --     (lam n.
  --       let t = readBytesAsString n in
  --       mapInsert _0 t.0 (mapInsert _1 t.1 _emptyRec))
  | TmConst { val = CArgv _ } -> _uc arg
  | TmConst { val = CFileRead _ } -> _uc readFile
  | TmConst { val = CFileWrite _ } ->
    _uc (lam path. lam file. writeFile path file; _emptyRec)
  | TmConst { val = CFileExists _ } -> _uc fileExists
  | TmConst { val = CFileDelete _ } ->
    _uc (lam path. deleteFile path; _emptyRec)
  | TmConst { val = CCommand _ } -> _uc command
  | TmConst { val = CError _ } -> _uc error
  | TmConst { val = CExit _ } -> _uc exit
  -- Constructor tags
  | TmConst { val = CConstructorTag _ } ->
    _uc (lam r. optionMapOr 0 sym2hash (nameGetSym r.0))
  -- Symbols
  | TmConst { val = CEqsym _ } -> _uc eqsym
  | TmConst { val = CGensym _ } -> _uc (lam. gensym ())
  | TmConst { val = CSym2hash _ } -> _uc sym2hash
  -- References
  | TmConst { val = CRef _ } -> _uc ref
  | TmConst { val = CDeRef _ } -> _uc deref
  | TmConst { val = CModRef _ } -> _uc (lam r. lam v. modref r v; _emptyRec)
  -- Tensors
  | TmConst { val = CTensorCreateUninitInt _ } -> _uc tensorCreateUninitInt
  | TmConst { val = CTensorCreateUninitFloat _ } -> _uc tensorCreateUninitFloat
  | TmConst { val = CTensorCreateInt _ } -> _uc tensorCreateCArrayInt
  | TmConst { val = CTensorCreateFloat _ } -> _uc tensorCreateCArrayFloat
  | TmConst { val = CTensorCreate _ } -> _uc tensorCreateDense
  | TmConst { val = CTensorGetExn _ } -> _uc tensorGetExn
  | TmConst { val = CTensorSetExn _ } -> _uc tensorSetExn
  | TmConst { val = CTensorLinearGetExn _ } -> _uc tensorLinearGetExn
  | TmConst { val = CTensorLinearSetExn _ } -> _uc tensorLinearSetExn
  | TmConst { val = CTensorRank _ } -> _uc tensorRank
  | TmConst { val = CTensorShape _ } -> _uc tensorShape
  | TmConst { val = CTensorReshapeExn _ } -> _uc tensorReshapeExn
  | TmConst { val = CTensorCopy _ } -> _uc tensorCopy
  | TmConst { val = CTensorTransposeExn _ } -> _uc tensorTransposeExn
  | TmConst { val = CTensorSliceExn _ } -> _uc tensorSliceExn
  | TmConst { val = CTensorSubExn _ } -> _uc tensorSubExn
  | TmConst { val = CTensorIterSlice _ } -> _uc tensorIterSlice
  | TmConst { val = CTensorEq _ } -> _uc tensorEq
  | TmConst { val = CTensorToString _ } -> _uc tensor2string
  -- Boot parser
  | TmConst { val = CBootParserParseMExprString _ } ->
    _uc (lam t. bootParserParseMExprString (_uc (mapFindExn _0 t,)))
  | TmConst { val = CBootParserParseMCoreFile _ } ->
    _uc
      (lam t.
        bootParserParseMCoreFile
          (_uc (mapFindExn _0 t,
                mapFindExn _1 t,
                mapFindExn _2 t,
                mapFindExn _3 t,
                mapFindExn _4 t,
                mapFindExn _5 t)))
  | TmConst { val = CBootParserGetId _ } -> _uc bootParserGetId
  | TmConst { val = CBootParserGetTerm _ } -> _uc bootParserGetTerm
  | TmConst { val = CBootParserGetType _ } -> _uc bootParserGetType
  | TmConst { val = CBootParserGetString _ } -> _uc bootParserGetString
  | TmConst { val = CBootParserGetInt _ } -> _uc bootParserGetInt
  | TmConst { val = CBootParserGetFloat _ } -> _uc bootParserGetFloat
  | TmConst { val = CBootParserGetListLength _ } -> _uc bootParserGetListLength
  | TmConst { val = CBootParserGetConst _ } -> _uc bootParserGetConst
  | TmConst { val = CBootParserGetPat _ } -> _uc bootParserGetPat
  | TmConst { val = CBootParserGetInfo _ } -> _uc bootParserGetInfo
  | tm -> dprint tm; error "here"

  sem unsafeTryMatch : List Value -> Value -> Pat -> Option (List Value)
  sem unsafeTryMatch env val =
  | PatNamed {ident = PName _} -> Some (Cons (val, env))
  | PatNamed {ident = PWildcard ()} -> Some env
  | PatSeqTot {pats = pats} ->
    if eqi (_uc length val) (length pats) then
      optionFoldlM
        (lam env. lam pair. unsafeTryMatch env pair.0 pair.1)
        env
        (_uc zip val pats)
    else None ()
  | PatSeqEdge {prefix = pre, middle = middle, postfix = post} ->
    if geqi (_uc length val) (addi (length pre) (length post)) then
      match _uc splitAt val (length pre) with (preVal, vals) in
      match splitAt vals (subi (length vals) (length post))
        with (vals, postVal)
      in
      optionBind
        (optionFoldlM
           (lam env. lam pair. unsafeTryMatch env pair.0 pair.1)
           env
           (_uc zip preVal pre))
        (lam env.
          let env =
            switch middle
            case PName _ then Cons (_uc vals, env)
            case PWildcard _ then env
            end
          in
          optionFoldlM
            (lam env. lam pair. unsafeTryMatch env pair.0 pair.1)
            env
            (_uc zip postVal post))
    else None ()
  | PatRecord r ->
    let f = lam pat. lam val.
      match (pat, val) with (Some p, Some v) then
        Some (lam env. unsafeTryMatch env v p)
      else None ()
    in
    mapFoldlOption
      (lam env. lam. lam f. f env)
      env
      (mapMerge f r.bindings (_uc val))
  | PatCon {ident = ident, subpat = subpat} ->
    match _uc val with (vident, val, ty) in
    if nameEqSymUnsafe ident vident then
      unsafeTryMatch env val subpat
    else None ()
  | PatInt i ->
    if eqi i.val (_uc val) then Some env else None ()
  | PatChar ch ->
    if eqChar ch.val (_uc val) then Some env else None ()
  | PatBool b ->
    if b.val then
      match _uc val with true then Some env else None ()
    else
      match _uc val with false then Some env else None ()
  | PatAnd {lpat = l, rpat = r} ->
    optionBind (unsafeTryMatch env val l) (lam env. unsafeTryMatch env val r)
  | PatOr {lpat = l, rpat = r} ->
    optionOrElse (lam. unsafeTryMatch env val r) (unsafeTryMatch env val l)
  | PatNot {subpat = p} ->
    switch unsafeTryMatch env val p
    case None _ then Some env
    case Some _ then None ()
    end

  sem _readback : Value -> Type -> Expr
  sem _readback val =| ty -> withType ty (_readbackH val (unwrapType ty))

  sem _readbackH : Value -> Type -> Expr
  sem _readbackH val =
  | TyInt _ -> int_ (_uc val)
  | TyFloat _ -> float_ (_uc val)
  | TyChar _ -> char_ (_uc val)
  | TyBool _ -> bool_ (_uc val)
  | ty & TyRecord r -> TmRecord {
    bindings = _uc mapIntersectWith _readback val r.fields,
    ty = ty,
    info = NoInfo ()
  }
  | TySeq r -> seq_ (_uc map (lam v. _readback v r.ty) val)
  | TyCon r ->
    match _uc val with (ident, val, ty) in
    nconapp_ ident (_readback val ty)
  | ty -> TmVal { val = val, ty = ty, info = NoInfo () }
end

lang TestLang = MExprDeBruijn + MExprUnsafeEval +
  MExprPrettyPrint + MExprSym + MExprTypeCheck + MExprEq + BootParser
end

mexpr

use TestLang in

let _fromString = lam str.
  typeCheck
    (symbolize
       (parseMExprStringExn (_defaultBootParserParseMExprStringArg ()) str))
in

let _toString = utestDefaultToString expr2str expr2str in

let eval = unsafeEval [] in

-- ┌───────────┐
-- │ Constants │
-- └───────────┘

let prog = _fromString "
  1
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  1.
  "
in
let value = _fromString "
  1.
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  true
  "
in
let value = _fromString "
  true
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  false
  "
in
let value = _fromString "
  false
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌───────────────────────┐
-- │ Sequences and Records │
-- └───────────────────────┘

let prog = _fromString "
  []
  "
in
let value = _fromString "
  []
  "
in
utest eval prog with value using eqExpr else _toString in


let prog = _fromString "
  [addi 1 2]
  "
in
let value = _fromString "
  [3]
  "
in
utest eval prog with value using eqExpr else _toString in


let prog = _fromString "
  ()
  "
in
let value = _fromString "
  ()
  "
in
utest eval prog with value using eqExpr else _toString in


let prog = _fromString "
  (addi 1 2, true)
  "
in
let value = _fromString "
  (3, true)
  "
in
utest eval prog with value using eqExpr else _toString in


let prog = _fromString "
  { { a = 1, b = false } with a = 2, b = true}
  "
in
let value = _fromString "
  { a = 2, b = true }
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌────────────────────┐
-- │ Constant Functions │
-- └────────────────────┘

-- Integer Arithmetic

let prog = _fromString "
  addi 1 2
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  subi 2 1
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  muli 2 3
  "
in
let value = _fromString "
  6
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  divi 6 2
  "
in let value = _fromString "
  3
  "
in utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  modi 4 2
  "
in
let value = _fromString "
  0
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  negi (negi 1)
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

-- Sequence Operations

let prog = _fromString "
  map (addi 1) [1, 2, 3]
  "
in
let value = _fromString "
  [2, 3, 4]
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  (splitAt [1, 2, 3] 1).1
  "
in
let value = _fromString "
  [2, 3]
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌──────────────────────────┐
-- │ Lambdas and let-bindings │
-- └──────────────────────────┘

let prog = _fromString "
  (lam x. x) 1
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let x = 1 in x
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  (lam x. addi 1 x) 2
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let x = 2 in addi 1 x
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌─────────┐
-- │ Symbols │
-- └─────────┘

let prog = _fromString "
  let s = gensym () in
  eqsym s s
  "
in
let value = _fromString "
  true
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let s1 = gensym () in
  let s2 = gensym () in
  eqsym s1 s2
  "
in
let value = _fromString "
  false
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let s1 = gensym () in
  let s2 = gensym () in
  eqi (sym2hash s1) (sym2hash s2)
  "
in
let value = _fromString "
  false
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let s = gensym () in
  eqi (sym2hash s) (sym2hash s)
  "
in
let value = _fromString "
  true
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌──────────────────┐
-- │ Constructor Tags │
-- └──────────────────┘

let prog = _fromString "
  type A in
  con C : Int -> A in
  con B : Char -> A in
  eqi (constructorTag (C 0)) (constructorTag (C 1))
  "
in
let value = _fromString "
  true
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  type A in
  con C : Int -> A in
  con B : Char -> A in
  eqi (constructorTag (C 0)) (constructorTag (B \'c\'))
  "
in
let value = _fromString "
  false
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
type Value in
con VInt : () -> Value in
con VFloat : () -> Value in
con VPair : (Value, Value) -> Value in

let v1 = VPair (VInt (), VFloat ()) in
let v2 = VPair (v1, VInt ()) in
[
  -- Terms which are applications of the same constructor have the same
  -- constructor tag.
  eqi (constructorTag (VInt ())) (constructorTag (VInt ())),
  eqi (constructorTag (VFloat ())) (constructorTag (VFloat ())),
  eqi (constructorTag v1) (constructorTag v2),

  -- Terms in the same type, but with applications of different constructors,
  -- have different constructor tags.
  eqi (constructorTag (VInt ())) (constructorTag (VFloat ())),
  eqi (constructorTag (VInt ())) (constructorTag v1),
  eqi (constructorTag (VFloat ())) (constructorTag v2)
]
  "
in
let value = _fromString "
  [true, true, true, false, false, false]
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌───────┐
-- │ Match │
-- └───────┘

let prog = _fromString "
  match 1 with x then x else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match 1 with _ then 1 else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match [1, 2] with [x, y] then addi x y else 2
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match [1, 2] with [x, y, z] then addi x y else 2
  "
in
let value = _fromString "
  2
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match [1, 2, 3, 4] with [x] ++ _ ++ [y] then addi x y else 2
  "
in
let value = _fromString "
  5
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match [1, 4] with [x] ++ _ ++ [y] then addi x y else 2
  "
in
let value = _fromString "
  5
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match { a = 1, b = 2 } with {a = a, b = b} in addi a b
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  type A in
  con C : Int -> A in
  con B : Char -> A in
  match C 1 with C x then x else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  type A in
  con C : Int -> A in
  con B : Char -> A in
  match B 'c' with C x then x else 2
  "
in
let value = _fromString "
  2
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match 1 with 1 then 1 else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match 'a' with 'a' then 1 else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  if true then 1 else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  if eqi 0 0 then 1 else 2
  "
in
let value = _fromString "
  1
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match (1, 2) with (x, y) then addi x y else 0
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  match ([1], [2]) with ([x], [y]) then addi x y else 0
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  let t = (1, 2) in addi t.0 t.1
  "
in
let value = _fromString "
  3
  "
in
utest eval prog with value using eqExpr else _toString in

-- ┌─────────────────────┐
-- │ Recursive Functions │
-- └─────────────────────┘

let prog = _fromString "
  recursive let fac = lam n.
    if eqi n 0 then 1 else muli n (fac (subi n 1))
  in
  fac 3
  "
in
let value = _fromString "
  6
  "
in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString "
  recursive let fac = lam n.
    match n with 0 then 1 else muli n (fac (subi n 1))
  in
  fac 3
  "
in
let value = _fromString "
  6
  "
in
utest eval prog with value using eqExpr else _toString in

let oddEvenStr = "
recursive
  let odd = lam n.
      if eqi n 1 then true
      else if lti n 1 then false
      else even (subi n 1)
  let even = lam n.
      if eqi n 0 then true
      else if lti n 0 then false
      else odd (subi n 1)
in
  "
in

let prog = _fromString (concat oddEvenStr "odd 11") in
let value = _fromString "true" in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString (concat oddEvenStr "odd 20") in
let value = _fromString "false" in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString (concat oddEvenStr "even 11") in
let value = _fromString "false" in
utest eval prog with value using eqExpr else _toString in

let prog = _fromString (concat oddEvenStr "even 20") in
let value = _fromString "true" in
utest eval prog with value using eqExpr else _toString in

()
