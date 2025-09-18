include "map.mc"
include "name.mc"
include "list.mc"
include "mexpr/ast.mc"
include "mexpr/eval.mc"
include "mexpr/cmp.mc"
include "mexpr/ast-builder.mc"
include "mexpr/side-effect.mc"
include "mexpr/const-arity.mc"

include "./peval.mc"
include "./monads.mc"

let _c = ref 0

let _optionMapMapWithKeysM
  : all k. all a. all b. (k -> a -> Option b) -> Map k a -> Option (Map k b)
  = lam f. lam m.
    mapFoldlOption (lam m. lam k. lam v.
      optionMap (lam v. mapInsert k v m) (f k v))
      (mapEmpty (mapGetCmpFun m))
      m

let _optionMapMapM
  : all k. all a. all b. (a -> Option b) -> Map k a -> Option (Map k b)
  = lam f. _optionMapMapWithKeysM (lam. f)

lang PEInterface = MExprAst + ConstAst + SideEffect + Eval
  type PEEnv = List (Name, PEVal)

  sem pEnvLookup : Name -> PEEnv -> Option PEVal
  sem pEnvLookup id =| env ->
    let p = lam entry. nameEqSymUnsafe id entry.0 in
    match listFind p env with Some (_, e) then Some e else None ()

  type VarRecord = {ident : Name, ty : Type, info : Info, frozen : Bool}
  type ConstRecord = {val : Const, ty : Type, info : Info}
  type SConstRecord = {c : ConstRecord, args : [(PEVal, Info)]}
  type LamRecord = {
    ident : Name,
    tyAnnot : Type,
    tyParam : Type,
    body : Expr,
    ty : Type,
    info : Info
  }
  type SClsRecord = {
    lamr : LamRecord,
    env : () -> PEEnv,
    lamcount : Int,
    apps : [Info],
    ident : Option Name,
    fix : Bool
  }
  type NeverRecord = {
    ty : Type,
    info : Info
  }
  type DRecordRecord = {
    bindings : Map SID PEVal,
    ty : Type,
    info : Info
  }
  type DConAppRecord = {
    ident : Name,
    body : PEVal,
    ty : Type,
    info: Info
  }
  type DSeqRecord = {
    vals : [PEVal],
    ty: Type,
    info: Info
  }

  syn PEStaticVal =
  | SConst SConstRecord
  | SCls SClsRecord
  | SNever NeverRecord

  syn PEVal =
  | DVar VarRecord
  | DRecord DRecordRecord
  | DConApp DConAppRecord
  | DSeq DSeqRecord
  | PEStatic PEStaticVal

  sem tyStaticVal =
  | SConst r -> r.c.ty
  | SCls r -> r.lamr.ty
  | SNever r -> r.ty

  sem tyVal =
  | DVar r -> r.ty
  | DRecord r -> r.ty
  | DConApp r -> r.ty
  | DSeq r -> r.ty
  | PEStatic val -> tyStaticVal val

  sem withTyVal ty =
  | DVar r -> DVar { r with ty = ty }
  | DRecord r -> DRecord { r with ty = ty }
  | DConApp r -> DConApp { r with ty = ty }
  | DSeq r -> DSeq { r with ty = ty }
  | PEStatic val -> PEStatic (withTyStaticVal ty val)

  sem withTyStaticVal ty =
  | SConst r -> SConst { r with c = { r.c with ty = ty }}
  | SCls r -> SCls { r with lamr = { r.lamr with ty = ty }}
  | SNever r -> SNever { r with ty = ty }

  sem withInfoVal i =
  | DVar r -> DVar { r with info = i }
  | DRecord r -> DRecord { r with info = i }
  | DConApp r -> DConApp { r with info = i }
  | DSeq r -> DSeq { r with info = i }
  | PEStatic val -> PEStatic (withInfoStaticVal i val)

  sem withInfoStaticVal i =
  | SConst r -> SConst { r with c = { r.c with info = i }}
  | SCls r -> SCls { r with lamr = { r.lamr with info = i }}
  | SNever r -> SNever { r with info = i }

  type PEState = {
    cache : Map Expr {ident : Name, ty : Type, info : Info},
    calltrace : Map Name [[PEVal]],
    effects : SideEffectEnv
  }

  sem exprToVal =
  | TmVar r -> Some (DVar r)
  | TmConst c -> Some (PEStatic (SConst { c = c, args = [] }))
  | TmRecord r ->
    optionMap
      (lam bs. DRecord { bindings = bs, ty = r.ty, info = r.info })
      (_optionMapMapM exprToVal r.bindings)
  | TmConApp r ->
    optionMap
      (lam body. DConApp {
        ident = r.ident, body = body, ty = r.ty, info = r.info })
      (exprToVal r.body)
  | TmSeq r ->
    optionMap
      (lam vs. DSeq { vals = vs, ty = r.ty, info = r.info })
      (optionMapM exprToVal r.tms)
  | _ -> None ()

  sem pEStateEmpty : () -> PEState
  sem pESpecialize : Expr -> Expr
  sem pECanonicalize : Expr -> Expr

  sem pEAppDescicionHeuristics state cls =| args ->
    if cls.fix then
      let isStatic = lam val. match val with PEStatic _ then true else false in
      -- any isStatic args
      false
    else true
end

lang PE = PEInterface +
  MExprAst + MExprCmp + MExprEval + MExprSideEffect + MExprArity +
  PEvalLetInline

  sem pEStateEmpty =| () -> {
    cache = mapEmpty cmpExpr,
    calltrace = mapEmpty nameCmp,
    effects = sideEffectEnvEmpty ()
  }

  -----------------
  --- Debugging ---
  -----------------

  sem _pEPrintCache =| cache ->
    printLn "==== Cache: ====================================================================";
    mapMapWithKey (lam tm. lam r.
      printLn "--- Entry ----";
      printLn "--- Expr: ----";
      printLn (expr2str tm);
      printLn "--- Ident: ---";
      printLn (nameGetStr r.ident);
      ())
      cache;
    printLn "================================================================================"

  sem _pEPrintEnvAndExpr env =| tm ->
    printLn "--------------------------------------------------------------------------------";
    print "Env: ";
    iter (lam t. print (nameGetStr t.0); print " ") (listToSeq env);
    printLn "";
    printLn "--------------------------------------------------------------------------------";
    printLn (expr2str tm);
    printLn "--------------------------------------------------------------------------------"

  --------------------
  --- AST builders ---
  --------------------

  sem _pEVar i ty =| n -> DVar {
    ident = n, ty = ty, info = i, frozen = false
  }
  sem _pELet i n ty =| tm -> DeclLet {
    ident = n, tyAnnot = ty, tyBody = tyTm tm, body = tm, info = i
  }
  sem _pEi i =| val -> TmConst {
    val = CInt { val = val }, ty = ityint_ i, info = i
  }
  sem _pEf i =| val -> TmConst {
    val = CFloat { val = val }, ty = ityfloat_ i, info = i
  }
  sem _pEb i =| val -> TmConst {
    val = CBool { val = val }, ty = itybool_ i, info = i
  }
  sem _pEc i =| val -> TmConst {
    val = CChar { val = val }, ty = itychar_ i, info = i
  }

  ----------------------------
  --- Intrinsic Operations ---
  ----------------------------

  sem _pEOpfi =
  | CFloorfi _ -> floorfi
  | CCeilfi _ -> ceilfi
  | CRoundfi _ -> roundfi

  sem _pEOpiii =
  | CAddi _ -> addi
  | CSubi _ -> subi
  | CMuli _ -> muli
  | CDivi _ -> divi
  | CModi _ -> modi
  | CSlli _ -> slli
  | CSrli _ -> srli
  | CSrai _ -> srai

  sem _pEOpfff =
  | CAddf _ -> addf
  | CSubf _ -> subf
  | CMulf _ -> mulf
  | CDivf _ -> divf

  sem _pEOpiib =
  | CEqi _ -> eqi
  | CNeqi _ -> neqi
  | CLti _ -> lti
  | CGti _ -> gti
  | CLeqi _ -> leqi
  | CGeqi _ -> geqi

  sem _pEOpffb =
  | CEqf _ -> eqf
  | CNeqf _ -> neqf
  | CLtf _ -> ltf
  | CGtf _ -> gtf
  | CLeqf _ -> leqf
  | CGeqf _ -> geqf

  --------------------------------
  --- Categorizing Expressions ---
  --------------------------------

  sem _pEIsSimpleExpr =
  | TmConst _ -> true
  | tm & (TmConApp _ | TmSeq _ | TmRecord _) ->
    sfold_Expr_Expr (lam x. lam y. and x (_pEIsSimpleExpr y)) true tm
  | _ -> false

  sem _pEIsSimpleExprWithVar =
  | TmVar _ | TmConst _ -> true
  | tm & (TmConApp _ | TmSeq _ | TmRecord _) ->
    sfold_Expr_Expr (lam x. lam y. and x (_pEIsSimpleExprWithVar y)) true tm
  | _ -> false

  -----------------------------------
  --- Expression Canonicalization ---
  -----------------------------------

  sem pECanonicalize =
  | TmApp r ->
    let r = { r with lhs = pECanonicalize r.lhs, rhs = pECanonicalize r.rhs } in
    _pECCApp1 r (r.lhs, r.rhs)
  | TmMatch r ->
    let target = pECanonicalize r.target in
    if _pEIsSimpleExpr target then
      switch tryMatch (Nil ()) target r.pat
      case Some (Nil _) then pECanonicalize r.thn
      case None () then pECanonicalize r.els
      case _ then TmMatch {
        r with
        target = target,
        thn = pECanonicalize r.thn,
        els = pECanonicalize r.els
      }
      end
    else
      if _pEIsSimpleExprWithVar target then
        switch tryMatch listEmpty target r.pat
        case Some (Nil _) then pECanonicalize r.thn
        case _ then TmMatch {
          r with
          target = target,
          thn = pECanonicalize r.thn,
          els = pECanonicalize r.els
        }
        end
      else TmMatch {
        r with
        target = target,
        thn = pECanonicalize r.thn,
        els = pECanonicalize r.els
      }
  | TmMatch (r & {thn = TmVar vr, els = TmNever _}) ->
    let target = pECanonicalize r.target in
    match tryMatch (Nil ()) target r.pat
      with Some (Cons ((ident, tm), Nil _)) then
      if nameEq ident vr.ident then tm
      else TmMatch { r with target = target }
    else TmMatch { r with target = target }
  | tm -> smap_Expr_Expr pECanonicalize tm

  sem _pECCApp1 r =
  | (TmApp r2, rhs) -> _pECanonApp2 r r2 (r2.lhs, r2.rhs, rhs)
  --- Int ---
  | (TmConst {val = CNegi _}, TmConst {val = CInt i}) ->
    _pEi r.info (negi i.val)
  --- Float ---
  | (TmConst {val = CNegf _}, TmConst {val = CFloat f}) ->
    _pEf r.info (negf f.val)
  --- Float-Int conversion ---
  | (TmConst {val = op & (CFloorfi _ | CCeilfi _ | CRoundfi _ )},
     TmConst {val = CFloat f}) ->
    _pEi r.info (_pEOpfi op f.val)
  | (TmConst {val = CInt2float _}, TmConst {val = CInt i}) ->
    _pEf r.info (int2float i.val)
  --- Int-Char conversion ---
  | (TmConst {val = CInt2Char _}, TmConst {val = CInt i}) ->
    _pEc r.info (int2char i.val)
  | (TmConst {val = CChar2Int _}, TmConst {val = CChar ch}) ->
    _pEi r.info (char2int ch.val)
  | _ -> TmApp r

  sem _pECanonApp2 r1 r2 =
  --- Int ---
  | (TmConst {val = op & (
    CAddi _ | CSubi _ | CMuli _ | CDivi _ | CModi _
                                          | CSlli _ | CSrli _ | CSrai _)},
     TmConst {val = CInt i1},
     TmConst {val = CInt i2}) ->
    _pEi r1.info (_pEOpiii op i1.val i2.val)
  | (TmConst {val = CAddi _}, TmConst {val = CInt {val = 0}}, rhs & TmVar _) ->
    rhs
  | (TmConst {val = CAddi _ | CSubi _},
     lhs & TmVar _,
     TmConst {val = CInt {val = 0}}) ->
    lhs
  | (TmConst {val = CAddi _}, lhs & TmConst {val = CInt _}, rhs & TmVar _) ->
    TmApp {
      r1 with
      lhs = TmApp { r2 with rhs = rhs },
      rhs = lhs
    }
  | (TmConst (c & {val = CAddi _ }), lhs & TmVar lv, rhs & TmVar rv) ->
    let cmp = nameCmp lv.ident rv.ident in
    if eqi cmp 0 then TmApp {
      r1 with
      lhs = TmApp {
        r2 with
        lhs = TmConst { c with val = CMuli () },
        rhs = _pEi r1.info 2
      },
      rhs = rhs
    }
    else
      if gti cmp 0 then TmApp {
        r1 with
        lhs = TmApp { r2 with rhs = rhs },
        rhs = lhs
      }
      else TmApp r1
  | (TmConst {val = CMuli _}, TmConst {val = CInt {val = 0}}, TmVar _) ->
    _pEi r1.info 0
  | (TmConst {val = CMuli _}, TmVar _, TmConst {val = CInt {val = 0}}) ->
    _pEi r1.info 0
  | (TmConst {val = CMuli _}, TmConst {val = CInt {val = 1}}, rhs & TmVar _) ->
    rhs
  | (TmConst {val = CMuli _}, lhs & TmVar _, TmConst {val = CInt {val = 1}}) ->
    lhs
  --- Float ---
  | (TmConst {val = op & (CAddf _ | CSubf _ | CMulf _ | CDivf _)},
     TmConst {val = CFloat f1},
     TmConst {val = CFloat f2}) ->
    _pEf r1.info (_pEOpfff op f1.val f2.val)
  | (TmConst {val = CAddf _}, lhs & TmConst {val = CFloat f}, rhs & TmVar _) ->
    if eqf f.val 0. then rhs
    else TmApp {
      r1 with
      lhs = TmApp { r2 with rhs = rhs },
      rhs = lhs
    }
  | (TmConst {val = CAddf _ | CSubf _},
     lhs & TmVar _,
     TmConst {val = CFloat f}) ->
    if eqf f.val 0. then lhs else TmApp r1
  | (TmConst (c & {val = CAddf _ }), lhs & TmVar lv, rhs & TmVar rv) ->
    let cmp = nameCmp lv.ident rv.ident in
    if eqi cmp 0 then TmApp {
      r1 with
      lhs = TmApp {
        r2 with
        lhs = TmConst { c with val = CMulf () },
        rhs = _pEf r1.info 2.
      },
      rhs = rhs
    }
    else
      if gti cmp 0 then TmApp {
        r1 with
        lhs = TmApp { r2 with rhs = rhs },
        rhs = lhs
      }
      else TmApp r1
  | (TmConst {val = CMulf _}, TmConst {val = CFloat f}, rhs & TmVar _) ->
    if eqf f.val 0. then _pEf r1.info 0.
    else
      if eqf f.val 1. then rhs
      else TmApp r1
  | (TmConst {val = CMulf _},  lhs & TmVar _, rhs & TmConst {val = CFloat f}) ->
    if eqf f.val 0. then _pEf r1.info 0.
    else
      if eqf f.val 1. then rhs
      else TmApp {
        r1 with
        lhs = TmApp { r2 with rhs = rhs },
        rhs = lhs
      }
  --- Cmp ---
  | (TmConst {val = op & (CEqi _ | CNeqi _ | CLti _ | CGti _ | CLeqi _ | CGeqi _)},
     TmConst {val = CInt i1},
     TmConst {val = CInt i2}) ->
    _pEb r1.info (_pEOpiib op i1.val i2.val)
  | (TmConst {val = op & (CEqf _ | CNeqf _ | CLtf _ | CGtf _ | CLeqf _ | CGeqf _)},
     TmConst {val = CFloat f1},
     TmConst {val = CFloat f2}) ->
    _pEb r1.info (_pEOpffb op f1.val f2.val)
  | (TmConst {val = CEqc _}, TmConst {val = CChar c1}, TmConst {val = CChar c2}) ->
    _pEb r1.info (eqc c1.val c2.val)
  | (TmConst (c & {val = (CEqi _ | CEqf _ | CEqc _)}),
     lhs & TmVar lv,
     rhs & TmVar rv) ->
    let cmp = nameCmp lv.ident rv.ident in
    if eqi cmp 0 then _pEb r1.info true
    else
      if gti cmp 0 then TmApp {
        r1 with
        lhs = TmApp { r2 with rhs = rhs },
        rhs = lhs
      }
      else TmApp r1
  --- Ordering ---
  | (TmConst {val = (CMuli _ | CMulf _)},
     lhs & TmVar _,
     rhs & TmConst {val = (CInt _ | CFloat _)}) ->
    TmApp {
      r1 with
      lhs = TmApp { r2 with rhs = rhs },
      rhs = lhs
    }
  | (TmConst (c & {val = (CMuli _ | CMulf _)}),
     lhs & TmVar lv,
     rhs & TmVar rv) ->
    let cmp = nameCmp lv.ident rv.ident in
    if gti cmp 0 then TmApp {
      r1 with
      lhs = TmApp { r2 with rhs = rhs },
      rhs = lhs
    }
    else TmApp r1
  --- Remaining
  | _ -> TmApp r1

  ------------------------
  --- Monad Shorthands ---
  ------------------------

  type SpecM a = ReaderStateWriterM PEEnv PEState [(Decl, Info)] a

  sem _return : all a. a -> SpecM a
  sem _return =| x -> rswmReturn { identity = [], add = concat } x

  sem _bind : all a. all b. SpecM a -> (a -> SpecM b) -> SpecM b
  sem _bind =| x -> rswmBind { identity = [], add = concat } x

  sem _bind2 : all a. all b. all c. SpecM a -> SpecM b -> (a -> b -> SpecM c) -> SpecM c
  sem _bind2 =| x -> rswmBind2 { identity = [], add = concat } x

  sem _bind3 : all a. all b. all c. all d.
    SpecM a -> SpecM b -> SpecM c -> (a -> b -> c -> SpecM d) -> SpecM d
  sem _bind3 =| x -> rswmBind3 { identity = [], add = concat } x

  sem _map : all a. all b. (a -> b) -> SpecM a -> SpecM b
  sem _map =| x -> rswmMap { identity = [], add = concat } x

  sem _ask : SpecM PEEnv
  sem _ask =| x -> rswmAsk { identity = [], add = concat } x

  sem _get : SpecM PEState
  sem _get =| x -> rswmGet { identity = [], add = concat } x

  sem _put : PEState -> SpecM ()
  sem _put =| x -> rswmPut { identity = [], add = concat } x

  sem _mod : (PEState -> PEState) -> SpecM ()
  sem _mod =| x -> rswmMod { identity = [], add = concat } x

  sem _record : [(Decl, Info)] -> SpecM ()
  sem _record =| x -> rswmRecord x

  sem _mapM : all a. all b. (a -> SpecM b) -> [a] -> SpecM [b]
  sem _mapM =| x -> rswmMapM { identity = [], add = concat } x

  sem _mapiM : all a. all b. (Int -> a -> SpecM b) -> [a] -> SpecM [b]
  sem _mapiM =| x -> rswmMapiM { identity = [], add = concat } x

  sem _foldlM : all a. all b. (a -> b -> SpecM a) -> a -> [b] -> SpecM a
  sem _foldlM =| x -> rswmFoldlM { identity = [], add = concat } x

  sem _foldrM : all a. all b. (b -> a -> SpecM a) -> a -> [b] -> SpecM a
  sem _foldrM =| x -> rswmFoldrM { identity = [], add = concat } x

  sem _mapMapM : all k. all a. all b. (a -> SpecM b) -> Map k a -> SpecM (Map k b)
  sem _mapMapM =| x -> rswmMapMapM { identity = [], add = concat } x

  sem _run : all a. SpecM a -> PEEnv -> PEState -> (PEState, [(Decl, Info)], a)
  sem _run =| x -> rswmRun x

  ----------------------
  --- Specialization ---
  ----------------------

  sem pESpecialize : Expr -> Expr
  sem pESpecialize =| tm ->
    match _run (pESpecializeExprM tm) listEmpty (pEStateEmpty ())
      with (state, decls, val) in
    match smRun (pEGeneralizeM decls val) state with (state2, tm) in
    pevalInlineLets state2.effects tm

  sem pESpecializeExprM : Expr -> SpecM PEVal
  sem pESpecializeExprM =| tm ->
    _bind _ask (lam env.
      -- _pEPrintEnvAndExpr env tm;
      pESpecializeExprMH tm)

  sem pESpecializeExprMH : Expr -> SpecM PEVal
  sem pESpecializeExprMH =
  | TmVar r ->
    _bind _ask (lam env.
      match pEnvLookup r.ident env with Some val then _return val
      -- else _return (DVar r))
      else error
             (join [ "pESpecializeExprH: "
                   , nameGetStr r.ident
                   , " is not in the environment" ]))
  | tm & TmLam r ->
    _bind _ask (lam env.
      _return (PEStatic (SCls { lamr = r,
                                env = lam. env,
                                lamcount = countArityExpr 0 tm,
                                apps = [],
                                ident = None (),
                                fix = false })))
  | TmApp r ->
    _bind2 (pESpecializeExprM r.lhs) (pESpecializeExprM r.rhs)
      (lam lhs. lam rhs. pESpecializeAppM r.info r.ty (lhs, rhs))
  | TmConst r -> _return (PEStatic (SConst { c = r, args = [] }))
  | TmDecl (r & {decl = DeclLet declr}) ->
    pESpecializeExprM
      (TmApp { lhs = TmLam { ident = declr.ident,
                             tyAnnot = declr.tyAnnot,
                             tyParam = declr.tyBody,
                             body = r.inexpr,
                             ty = ityarrow_ declr.info declr.tyBody r.ty,
                             info = declr.info },
               rhs = declr.body,
               ty = r.ty,
               info = r.info })
  | TmDecl r ->
    _bind2 (pESpecializeDeclM r.info r.decl) _get (lam env. lam s.
      match _run (pESpecializeExprM r.inexpr) env s with (s2, decls, val) in
      _bind2 (_put s2) (_record decls) (lam. lam. _return val))
  | TmRecord r ->
    match unzip (mapBindings r.bindings) with (keys, tms) in
    _bind (_mapM pESpecializeExprM tms) (lam vals.
      _return (DRecord {
        bindings = mapFromSeq cmpSID (zip keys vals),
        ty = r.ty,
        info = r.info }))
  | TmRecordUpdate r ->
    let err = lam. error "pESpecializeExprM: Record Type Error" in
    let f = lam newval. lam val.
      match val with Some _ then Some newval
      else err () in
    _bind2 (pESpecializeExprM r.rec) (pESpecializeExprM r.value)
      (lam rec. lam value.
        switch rec
        case PEStatic (SNever _) then
          _return (PEStatic (SNever { ty = r.ty, info = r.info }))
        case DRecord rr then
          _return (DRecord {
            rr with bindings = mapUpdate r.key (f value) rr.bindings })
        case DVar _ then
          _bind2 (pEGeneralizeValM rec) (pEGeneralizeValM value)
            (lam rec2. lam value2.
              let tm = TmRecordUpdate { r with rec = rec2, value = value2 } in
              _letBindExpr r.info tyunknown_ tm)
        case _ then err ()
        end)
  | TmConApp r ->
    _bind (pESpecializeExprM r.body) (lam body.
      _return (DConApp {
        ident = r.ident, body = body, ty = r.ty, info = r.info }))
  | TmSeq r ->
    _bind (_mapM pESpecializeExprM r.tms) (lam vals.
      _return (DSeq { vals = vals, info = r.info, ty = r.ty }))
  | TmMatch r ->
    _bind2 (pESpecializeExprM r.target) _ask (lam target. lam env.
      match target with PEStatic (SNever _) then
        _return (PEStatic (SNever { ty = r.ty, info = r.info }))
      else
        _bind2 (_letBindAllClss r.info target) _get (lam target. lam s.
          switch pETryMatch (Match listEmpty) target r.pat
          case Match env3 then
            match _refreshPatNames env r.pat with (env2, pat) in
            -- Shadow refreshed pattern names with matches
            let env2 = listConcat env3 env2 in

            match _run (pESpecializeExprM r.thn) (listConcat env2 env) s
              with (s2, decls, thn) in
            _bind2 (_put s2) (_record decls) (lam. lam. _return thn)
          case StaticNoMatch _ then
            pESpecializeExprM r.els
          case m & (PartialMatch _ | DynamicMatch _) then
            let dropCache = lam s2. { s2 with cache = s.cache } in

            match _refreshPatNames env r.pat with (env2, pat) in
            -- Shadow refreshed pattern names with matches
            let env2 = match m with PartialMatch env3 then listConcat env3 env2
                       else env2 in

            match _run (pESpecializeExprM r.thn) env2 s with (s2, decls, thn) in
            match smRun (pEGeneralizeM decls thn) s2 with (s3, thn) in
            let s3 = dropCache s3 in

            match _run (pESpecializeExprM r.els) env s3 with (s4, decls, els) in
            match smRun (pEGeneralizeM decls els) s2 with (s5, els) in
            let s5 = dropCache s5 in

            _bind2 (_put s5) (pEGeneralizeValM target) (lam. lam target.
              _letBindExpr r.info tyunknown_ (TmMatch {
                r with target = target, pat = pat, thn = thn, els = els }))
          end))
  | TmNever r -> _return (PEStatic (SNever r))
  | tm -> error (expr2str tm)

  sem pESpecializeAppM : Info -> Type -> (PEVal, PEVal) -> SpecM PEVal
  sem pESpecializeAppM info ty =
  | (PEStatic (SNever _), _) | (_, (PEStatic (SNever _))) ->
    _return (PEStatic (SNever { ty = ty, info = info }))
  | (lhs, rhs) ->
    let rhs2 =
      -- We need to let-bind all closures in the RHS because they are about
      -- to be substituted for.
      switch rhs
      case PEStatic (SCls (rclsr & {ident = None _})) then
        -- If we have single closure we give it a non-generic name.
        _map (lam v. PEStatic v)
          (match lhs with PEStatic (SCls (lclsr & {ident = None _})) then
            _letBindCls info
              { rclsr with ident = Some (nameSetNewSym lclsr.lamr.ident) }
           else _letBindCls info rclsr)
      case _ then _letBindAllClss info rhs
      end in
    _bind rhs2 (lam rhs.
      let residualizeApp = lam lhs.
        _bind2 (pEGeneralizeValM lhs) (pEGeneralizeValM rhs)
          (lam lhs. lam rhs.
            _letBindExpr info tyunknown_
              (TmApp { lhs = lhs, rhs = rhs, ty = ty, info = info })) in
      let specBody = lam env. lam body.
        _bind _get (lam s.
          match _run (pESpecializeExprM body) env s with
            (s2, decls, val) in
          _bind2 (_put s2) (_record decls) (lam. lam. _return val)) in
      switch lhs
      case DVar _ then
        -- We cannot apply a dynamic LHS.
        residualizeApp lhs
      case PEStatic (SConst sc) then
        -- We evaluate fully applied intrinsics functions.
        let sc2 = { sc with args = snoc sc.args (rhs, info) } in
        if eqi (length sc2.args) (constArity sc2.c.val) then
          pESpecializeConstM ty sc2
        else _return (PEStatic (SConst sc2))
      case PEStatic (SCls (cls & {ident = None _})) then
        -- This closure is not bound to anything, so it only occurs once in
        -- the program and we therefore just apply it to its argument.
        specBody (listCons (cls.lamr.ident, rhs) (cls.env ())) cls.lamr.body
      case PEStatic (SCls (cls & {ident = Some ident})) then
        let env = listCons (cls.lamr.ident, rhs) (cls.env ()) in
        let apps = cons info cls.apps in
        if neqi (length apps) cls.lamcount then
          let lamr = match cls.lamr.body with TmLam lamr then lamr
                     else error "fail" in
          _return (PEStatic (SCls {
            cls with lamr = lamr, env = lam. env, apps = apps }))
        else
          -- If the closure is bound somewhere it may be applied multiple
          -- times and we therefore us heuristics to determine if we should
          -- evaluate the application.
          let args =
            (foldl
               (lam t. lam.
                 match t.1 with Cons ((_, arg), env) then
                   (cons arg t.0, env)
                 else error "fail")
               ([], env)
               apps).0
          in
          _bind2 (_updateCalltrace ident args)
            _get (lam. lam s.
              if pEAppDescicionHeuristics s cls args then
                _bind (specBody env cls.lamr.body) (lam retval.
                  -- If the result of the application is another closure we
                  -- need to let-bind it.
                  match retval with PEStatic (SCls retcls) then
                    let ident2 =
                      nameSym (join [nameGetStr ident, "_spec"]) in
                    _map (lam v. PEStatic v)
                      (_letBindCls info {
                        retcls with ident = Some ident2 })
                  else _return retval)
              else residualizeApp lhs)
      case _ then error "pESpecializeExprM: Application Type Error"
      end)

  sem pESpecializeDeclM : Info -> Decl -> SpecM PEEnv
  sem pESpecializeDeclM info =
  | decl & DeclRecLets r -> _bind _ask (lam env.
    recursive let buildenv = lam.
      let wraplambda = lam ident. lam tm.
        match pECanonicalize tm with TmLam lamr then
          PEStatic (SCls { lamr = lamr,
                           env = buildenv,
                           lamcount = countArityExpr 0 tm,
                           apps = [],
                           ident = Some ident,
                           fix = true })
        else error "fail"
      in
      foldl (lam env. lam bind.
        listCons (bind.ident, wraplambda bind.ident bind.body) env)
        env r.bindings
    in
    let specBindingBody : PEEnv -> Expr -> StateM PEState Expr = lam env. lam body.
      smBind smGet (lam s.
        match _run (pESpecializeExprM body) env s with (s2, decls, val) in
        smBind (smPut s2) (lam. pEGeneralizeM decls val)) in
    match
      mapAccumL (lam acc. lam bind.
        let freshident = nameSetNewSym bind.ident in
        let env =
          listCons (bind.ident, _pEVar bind.info bind.tyBody freshident) env in
        (env, { bind with ident = freshident }))
        ([], env) r.bindings
      with (env2, bindings) in
    _bind _get (lam s.
      match
        smRun
          (smMapM (specBindingBody env2) (map (lam bind. bind.body) bindings))
          s
        with (s2, bodies) in
      let bindings =
        zipWith (lam bind. lam body. { bind with body = body })
          bindings bodies in
      let decl = DeclRecLets { r with bindings = bindings } in
      _bind3
        (_put s2)
        (_updateEffects decl)
        (_record [(decl, info)])
        (lam. lam. lam. _return (buildenv ()))))
  | decl ->
    -- TODO(oerikss, 2025-09-17): We should probably refresh all names.
    _bind (_record [(decl, info)]) (lam. _ask)

  syn MatchResult =
  | Match PEEnv
  | StaticNoMatch ()
  | PartialMatch PEEnv
  | DynamicMatch ()

  sem pETryMatch : MatchResult -> PEVal -> Pat -> MatchResult
  sem pETryMatch mr val =
  | PatNamed {ident = PName name} ->
    _andMR (mr, Match (listSingleton (name, val)))
  | PatNamed {ident = PWildcard _} -> mr
  | pat & PatSeqTot {pats = pats} ->
    switch val
    case DSeq r then
      if eqi (length r.vals) (length pats) then
        foldl2 pETryMatch mr r.vals pats
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | pat & PatSeqEdge {prefix = pre, middle = middle, postfix = post} ->
    switch val
    case DSeq r then
      if geqi (length r.vals) (addi (length pre) (length post)) then
        match splitAt r.vals (length pre) with (preVals, vals) in
        match splitAt r.vals (subi (length vals) (length post))
          with (vals, postVals) in
        let mr =
          foldl2 pETryMatch mr (concat preVals postVals) (concat pre post) in
        switch middle
        case PName name then
          _andMR (mr, Match (listSingleton (name, DSeq { r with vals = vals })))
        case PWildcard _ then mr
        end
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | pat & PatRecord {bindings = bs} ->
    switch val
    case DRecord r then
      mapFoldWithKey
        (lam mr. lam k. lam pat.
          let val =
            mapFindOrElse (lam. error "pETryMatch: Type Error") k r.bindings in
          pETryMatch mr val pat)
        mr
        bs
    case _ then _andMR (mr, DynamicMatch ())
    end
  | pat & PatCon {ident = ident, subpat = subpat, info = info} ->
    switch val
    case DConApp r then
      if nameEqSymUnsafe ident r.ident then
        pETryMatch mr r.body subpat
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | PatInt i ->
    switch val
    case PEStatic (SConst {c = {val = CInt r}}) then
      if eqi i.val r.val then mr
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | PatChar ch ->
    switch val
    case PEStatic (SConst {c = {val = CChar r}}) then
      if eqc ch.val r.val then mr
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | PatBool b ->
    switch val
    case PEStatic (SConst {c = {val = CBool r}}) then
      if xnor b.val r.val then mr
      else StaticNoMatch ()
    case _ then _andMR (mr, DynamicMatch ())
    end
  | PatAnd {lpat = l, rpat = r} ->
    pETryMatch (pETryMatch mr val l) val r
  | PatOr {lpat = l, rpat = r} ->
    switch  pETryMatch (Match listEmpty) val l
    case Match env then _andMR (mr, Match env)
    case StaticNoMatch _ then pETryMatch mr val r
    case PartialMatch _ | DynamicMatch _ then _andMR (mr, DynamicMatch ())
    end
  | PatNot {subpat = p} ->
    switch pETryMatch (Match listEmpty) val p
    case Match _ then StaticNoMatch ()
    case StaticNoMatch _ then mr
    case PartialMatch _ | DynamicMatch _ then _andMR (mr, DynamicMatch ())
    end

  sem _pEbv i =| b -> PEStatic (SConst {
    c = { val = CBool { val = b }, ty = TyBool { info = i }, info = i },
    args = [] })
  sem _pEiv i =| n -> PEStatic (SConst {
    c = { val = CInt { val = n }, ty = TyInt { info = i }, info = i },
    args = [] })
  sem _pErecordv i =| bs ->
    let bindings = mapFromSeq cmpSID bs in
    DRecord {
      bindings = bindings,
      ty = TyRecord {
        fields = mapMap tyVal bindings,
        info = i },
      info = i }

  sem pESpecializeConstM : Type -> SConstRecord -> SpecM PEVal
  sem pESpecializeConstM retty =
  | {c = {val = CHead _}, args = [(DSeq r, i)]} ->
    _return (withInfoVal i (head r.vals))
  | {c = {val = CTail _}, args = [(DSeq r, i)]} ->
    _return (DSeq { r with vals = tail r.vals, info = i })
  | {c = {val = CNull _}, args = [(DSeq r, i)]} ->
    _return (_pEbv i (null r.vals))
  | {c = {val = CGet _}, args = [
    (DSeq r, _),
    (PEStatic (SConst {c = {val = CInt n}}), i) ]} ->
    _return (withInfoVal i (get r.vals n.val))
  | {c = {val = CSet _}, args = [
    (DSeq r, _),
    (PEStatic (SConst {c = {val = CInt n}}), _),
    (val, i) ]} ->
    _return (DSeq { r with vals = set r.vals n.val val, info = i })
  | {c = {val = CCons _}, args = [(val, _), (DSeq r, i)]} ->
    _return (DSeq { r with vals = cons val r.vals, info = i })
  | {c = {val = CSnoc _}, args = [(DSeq r, _), (val, i)]} ->
    _return (DSeq { r with vals = snoc r.vals val, info = i })
  | {c = {val = CConcat _}, args = [(DSeq lr, _), (DSeq rr, i)]} ->
    _return (DSeq { rr with vals = concat lr.vals rr.vals, info = i })
  | {c = {val = CLength _}, args = [(DSeq r, i)]} ->
    _return (_pEiv i (length r.vals))
  | {c = {val = CReverse _}, args = [(DSeq r, i)]} ->
    _return (DSeq { r with vals = reverse r.vals, info = i })
  | {c = {val = CSplitAt _}, args = [
    (DSeq r, _),
    (PEStatic (SConst {c = {val = CInt n}}), i)
  ]} ->
    match splitAt r.vals n.val with (ls, rs) in
    _return (_pErecordv i
               [ (stringToSid "0", DSeq { r with vals = ls, info = i }),
                 (stringToSid "1", DSeq { r with vals = rs, info = i }) ])
  | {c = {val = CSubsequence _}, args = [
    (DSeq r, _),
    (PEStatic (SConst {c = {val = CInt n1}}), _),
    (PEStatic (SConst {c = {val = CInt n2}}), i)
  ]} ->
    _return (DSeq { r with vals = subsequence r.vals n1.val n2.val, info = i })
  | {c = {val = CMap _}, args = [(fn, _), (DSeq r, i)]} ->
    match unwrapType retty with TySeq tyr then
      let f = lam x. pESpecializeAppM i tyr.ty (fn, x) in
      _bind (_mapM f r.vals) (lam vals.
        _return (DSeq { r with vals = vals, info = i, ty = retty }))
    else error "pESpecializeConstM: TypeError"
  | {c = {val = CMapi _}, args = [(fn, _), (seq & DSeq r, i)]} ->
    match (unwrapType (tyVal seq), unwrapType retty)
      with (TySeq tyr1, TySeq tyr2) then
      let ty1 = ityarrow_ i tyr1.ty tyr2.ty in
      let f = lam j. lam x.
        _bind (pESpecializeAppM i ty1 (fn, _pEiv i j)) (lam fn.
          pESpecializeAppM i tyr2.ty (fn, x)) in
      _bind (_mapiM f r.vals) (lam vals.
        _return (DSeq { r with vals = vals, info = i, ty = retty }))
    else error "pESpecializeConstM: TypeError"
  | {c = {val = CIter _}, args = [(fn, _), (DSeq r, i)]} ->
    let unit = _pErecordv i [] in
    let f = lam. lam x. pESpecializeAppM i tyunit_ (fn, x) in
    _bind (_foldlM f unit r.vals) (lam. _return unit)
  | {c = {val = CIteri _}, args = [(fn, _), (seq & DSeq r, i)]} ->
    match unwrapType (tyVal seq) with TySeq tyr then
      let ty1 = ityarrow_ i tyr.ty tyunit_ in
      let unit = _pErecordv i [] in
      let f = lam j. lam x.
        _bind (pESpecializeAppM i ty1 (fn, _pEiv i j)) (lam fn.
          _bind (pESpecializeAppM i tyunit_ (fn, x)) (lam.
            _return (addi j 1))) in
      _bind (_foldlM f 0 r.vals) (lam. _return unit)
    else error "pESpecializeConstM: TypeError"
  | {c = {val = CFoldl _}, args = [(fn, _), (acc, _), (seq & DSeq r, i)]} ->
    match unwrapType (tyVal seq) with TySeq tyr then
      let ty1 = ityarrow_ i tyr.ty retty in
      let f = lam acc. lam x.
        _bind (pESpecializeAppM i ty1 (fn, acc)) (lam fn.
          pESpecializeAppM i retty (fn, x)) in
      _bind (_foldlM f acc r.vals) (lam acc. _return (withInfoVal i acc))
    else error "pESpecializeConstM: TypeError"
  | {c = {val = CFoldr _}, args = [(fn, _), (acc, _), (DSeq r, i)]} ->
    let ty1 = ityarrow_ i retty retty in
    let f = lam x. lam acc.
      _bind (pESpecializeAppM i ty1 (fn, x)) (lam fn.
        pESpecializeAppM i retty (fn, acc)) in
    _bind (_foldrM f acc r.vals) (lam acc. _return (withInfoVal i acc))
  | {c = {val = CCreate _}, args = [
    (PEStatic (SConst {c = {val = CInt n}}), _),
    (fn, i) ]} ->
    match unwrapType retty with TySeq tyr then
      let f = lam j. pESpecializeAppM i tyr.ty (fn, _pEiv i j) in
      _bind (_mapM f (create n.val (lam j. j))) (lam vals.
        _return (DSeq { vals = vals, ty = retty, info = i }))
    else error "pESpecializeConstM: TypeError"
  | sc ->
    _bind (pEGeneralizeStaticValM (SConst sc)) (lam tm.
      let tm2 = pECanonicalize tm in
      match exprToVal tm2 with Some val then _return val
      else _letBindExpr sc.c.info tyunknown_ tm2)

  sem _andMR : (MatchResult, MatchResult) -> MatchResult
  sem _andMR =
  | (Match e1, Match e2) -> Match (listConcat e1 e2)
  | (PartialMatch e1, PartialMatch e2)
  | (PartialMatch e1, Match e2)
  | (Match e1, PartialMatch e2) -> PartialMatch (listConcat e1 e2)
  | (Match e, DynamicMatch _)
  | (DynamicMatch _, Match e) -> PartialMatch e
  | (PartialMatch e, DynamicMatch _)
  | (DynamicMatch _, PartialMatch e) -> PartialMatch e
  | (_, StaticNoMatch _)
  | (StaticNoMatch _, _) -> StaticNoMatch ()

  sem _refreshPatNames : PEEnv -> Pat -> (PEEnv, Pat)
  sem _refreshPatNames env =
  | PatNamed (r & {ident = PName name}) ->
    let freshname = nameSetNewSym name in
    ( listCons (name, _pEVar r.info r.ty freshname) env
    , PatNamed { r with ident = PName freshname } )
  | PatSeqEdge (r & { middle = PName name }) ->
    let freshname = nameSetNewSym name in
    let env = listCons (name, _pEVar r.info r.ty freshname) env in
    let pat = PatSeqEdge { r with middle = PName freshname } in
    smapAccumL_Pat_Pat _refreshPatNames env pat
  | pat -> smapAccumL_Pat_Pat _refreshPatNames env pat

  sem pEIsMemoizable =
  | TmApp _ | TmMatch _ | TmRecordUpdate _ -> true
  | TmDecl r ->
    sfold_Expr_Expr (lam acc. lam tm. and acc (pEIsMemoizable tm))
      true r.inexpr
  | _ -> false

  sem _letBindExpr : Info -> Type -> Expr -> SpecM PEVal
  sem _letBindExpr info tyAnnot =| tm ->
    _bind (_cacheLookup tm) (lam var.
      match var with Some var then _return var
      else
        let freshident = nameSym "t" in
        _bind (_cacheMaybeInsert info freshident tm) (lam.
          let decl = _pELet info freshident tyAnnot tm in
          _bind2 (_updateEffects decl) (_record [(decl, info)]) (lam. lam.
            _return (_pEVar info (tyTm tm) freshident))))

  sem _letBindCls : Info -> SClsRecord -> SpecM PEStaticVal
  sem _letBindCls info =| clsr ->
    let ident = match clsr.ident with Some ident then ident
                else nameSym "anon_f" in
    _bind (pEGeneralizeStaticValM (SCls { clsr with ident = None () })) (lam tm.
      let decl = _pELet info ident clsr.lamr.tyAnnot tm in
      _bind2 (_record [(decl, info)]) (_updateEffects decl) (lam. lam.
        _return (SCls { clsr with ident = Some ident })))

  sem _letBindAllClss : Info -> PEVal -> SpecM PEVal
  sem _letBindAllClss info =
  | PEStatic val -> _map (lam v. PEStatic v) (_letBindAllClssStatic info val)
  | DRecord r ->
    _bind (_mapMapM (_letBindAllClss info) r.bindings) (lam bs.
      _return (DRecord { r with bindings = bs }))
  | DConApp r ->
    _bind (_letBindAllClss info r.body) (lam body.
      _return (DConApp { r with body = body }))
  | DSeq r ->
    _bind (_mapM (_letBindAllClss info) r.vals) (lam vals.
      _return (DSeq { r with vals = vals }))
  | val -> _return val

  sem _letBindAllClssStatic : Info -> PEStaticVal -> SpecM PEStaticVal
  sem _letBindAllClssStatic info =
  | SCls (cls & {ident = None _}) -> _letBindCls info cls
  | val -> _return val

  sem _hasEffect : Expr -> SpecM Bool
  sem _hasEffect =| tm ->
    _bind _get (lam s. _return (exprHasSideEffect s.effects tm))

  sem _updateEffects : Decl -> SpecM ()
  sem _updateEffects =| decl ->
    _mod (lam s.
      { s with effects = constructSideEffectEnvH s.effects (bind_ decl unit_) })

  sem _cacheLookup : Expr -> SpecM (Option PEVal)
  sem _cacheLookup =| tm ->
    if pEIsMemoizable tm then
      _bind _get (lam s.
        match mapLookup tm s.cache with Some r then
          _return (Some (_pEVar r.info r.ty r.ident))
        else _return (None ()))
    else _return (None ())

  sem _cacheMaybeInsert : Info -> Name -> Expr -> SpecM ()
  sem _cacheMaybeInsert info ident =| tm ->
    if (pEIsMemoizable tm) then
       _bind (_hasEffect tm) (lam effect.
         if effect then _return () else
           _mod (lam s.
             let val = { ident = ident, ty = tyTm tm, info = info } in
             { s with cache = mapInsert tm val s.cache }))
       else _return ()

  sem _updateCalltrace : Name -> [PEVal] -> SpecM ()
  sem _updateCalltrace ident =| args ->
    _mod (lam s.
      let f = optionMapOr [args] (cons args) in
      { s with calltrace = mapUpdate ident (lam x. Some (f x)) s.calltrace })

  ----------------------
  --- Generalization ---
  ----------------------

  sem pEGeneralizeValM : PEVal -> SpecM Expr
  sem pEGeneralizeValM =| val ->
    _bind _get (lam s.
      match smRun (pEGeneralizeValMH val) s with (s2, tm) in
      _bind (_put s2) (lam. _return tm))

  sem pEGeneralizeStaticValM : PEStaticVal -> SpecM Expr
  sem pEGeneralizeStaticValM =| val ->
    _bind _get (lam s.
      match smRun (pEGeneralizeStaticValMH val) s with (s2, tm) in
      _bind (_put s2) (lam. _return tm))

  sem pEGeneralizeM : [(Decl, Info)] -> PEVal -> StateM PEState Expr
  sem pEGeneralizeM decls =| val ->
    smBind (pEGeneralizeValMH val) (lam tm.
      let ty = tyTm tm in
      let tm =
        foldr
          (lam t. lam tm.
            TmDecl { decl = t.0, inexpr = tm, info = t.1, ty = ty })
          tm
          decls in
      smReturn tm)

  sem pEGeneralizeValMH : PEVal -> StateM PEState Expr
  sem pEGeneralizeValMH =
  | DVar r -> smReturn (TmVar r)
  | DRecord r ->
    smBind (smMapMapM pEGeneralizeValMH r.bindings) (lam bs.
      smReturn (TmRecord { bindings = bs, ty = r.ty, info = r.info }))
  | DConApp r ->
    smBind (pEGeneralizeValMH r.body) (lam body.
      smReturn (TmConApp {
        ident = r.ident, body = body, ty = r.ty, info = r.info }))
  | DSeq r ->
    smBind (smMapM pEGeneralizeValMH r.vals) (lam tms.
      smReturn (TmSeq { tms = tms, ty = r.ty, info = r.info }))
  | PEStatic val -> pEGeneralizeStaticValMH val

  sem pEGeneralizeStaticValMH : PEStaticVal -> StateM PEState Expr
  sem pEGeneralizeStaticValMH =
  | SCls (r & {ident = Some ident}) ->
    match
      foldl (lam acc. lam info.
        match acc with (ty, args, Cons ((_, arg), env)) in
        (tyarrow_ (tyVal arg) ty, cons (arg, info) args, env))
        (r.lamr.ty, [], r.env ())
        r.apps
      with (ty, args) in
    let fn = TmVar {
      ident = ident, ty = ty, info = r.lamr.info, frozen = false } in
    _generalizeAppChain fn args
  | SCls (r & {ident = None _}) ->
    let freshident = nameSetNewSym r.lamr.ident in
    let var = _pEVar r.lamr.info r.lamr.tyParam freshident in
    let env = listCons (r.lamr.ident, var) (r.env ()) in
    smBind smGet (lam s.
      match _run (pESpecializeExprM r.lamr.body) env s
        with (s2, decls, val) in
      -- Drop the expression cache from the body because it is not valid outside
      -- of it.
      smBind (smPut { s2 with cache = s.cache }) (lam.
        smBind (pEGeneralizeM decls val) (lam body.
          let fn = TmLam { r.lamr with ident = freshident, body = body } in
          smReturn fn)))
  | SConst (r & {args = []}) -> smReturn (TmConst r.c)
  | SConst r -> _generalizeAppChain (TmConst r.c) r.args
  | SNever r -> smReturn (TmNever r)

  sem _generalizeAppChain : Expr -> [(PEVal, Info)] -> StateM PEState Expr
  sem _generalizeAppChain fn =| args ->
    smFoldlM
      (lam lhs. lam arg.
        smBind (pEGeneralizeValMH arg.0) (lam rhs.
          match tyTm lhs with TyArrow ar then smReturn (TmApp {
            lhs = lhs,
            rhs = rhs,
            ty = ar.to,
            info = arg.1 })
          else error "pEResidualizeStaticVal: Type Error"))
      fn
      args
end
