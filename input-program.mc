-- include "avl.mc"

mexpr
-- let u = addi 1 5 in
-- let g = lam z. lam h. addi (h z) u in
-- lam x. (lam f. f) (g x (lam y. addi x y))

-- type A in
-- con B : [Int] -> A in
-- con C : Int -> A in
-- let f = lam x. addi 1 x in
-- let g = lam f. lam y. print "f"; (f y) in
-- let h = lam x. lam y. lam z. addi (addi x y) z in
-- let h1 = lam x. h x in
-- -- lam u. h1 u
-- lam u.
--   let w = B [u] in
--   let x = addi (addi 1 0) ((lam x. x) u) in
--   muli
--     (g (lam z. print "a"; f z)
--        (subi (f (muli (x) (addi u 1))) u))
--     (g (lam z. print "b"; f z)
--        (subi (f (muli (addi u 1) (addi u 1))) u))

let base = 1 in
let pow = lam x.
  recursive let recur = lam n.
    if lti n 1 then base
    else muli x (recur (subi n 1))
  in
  recur
in
lam a. lam b. pow a 10


-- lam x. lam y. lam z.
  -- let f = lam acc. lam x. modref z (addi 1 x) in
  -- foldr cons [] [1, 2, 3]

-- let f = lam x. lam y. lam z. muli (addi x y) z in
-- lam u. (f, f u, f u u, f u u u)

-- (lam x. lam y. 1) 1
-- let f = (lam x. lam y. lam z. 0) in
-- lam u. f u u u

-- type A in
-- con B : [Int] -> A in
-- con C : Int -> A in
-- (lam f. lam x. {{ a = B [1], b = f } with a = C 1 }) (lam y. y)

-- (lam t. (t, t)) (lam x. x, )

-- (lam x.
--   match x with z | y then addi x y else 2)

-- mexpr

-- let x = negi (negi 1) in
-- { a = (x,
--        negf (negf 1.0),
--        floorfi 2.4,
--        ceilfi 2.4,
--        roundfi 2.4,
--        int2float 3,
--        int2char 48,
--        char2int (int2char 0),
--        addi 1 2,
--        subi 1 2,
--        muli 2 3,
--        divi 4 2,
--        modi 5 2,
--        slli 2 1,
--        srli 2 1,
--        srai (negi 4) 1,
--        addf 1. 2.,
--        subf 1. 2.,
--        mulf 2. 3.,
--        divf 4. 2.
--        ),
--   a01 = lam x. addi 1 x,
--   a02 = lam x. addi 0 x,
--   a03 = lam x. addi x 0,
--   a04 = lam x. subi x 0,
--   a05 = lam x. lam y. addi y x,
--   a06 = lam x. addi x x,
--   a07 = lam x. muli x 2,
--   a08 = lam x. muli x 0,
--   a09 = lam x. muli x 1,
--   a10 = lam x. muli 0 x,
--   a11 = lam x. muli 1 x,
--   a12 = lam x. lam y. muli y x,
--   b01 = lam x. addf 1. x,
--   b02 = lam x. addf 0. x,
--   b03 = lam x. addf x 0.,
--   b04 = lam x. subf x 0.,
--   b05 = lam x. lam y. addf y x,
--   b06 = lam x. addf x x,
--   b07 = lam x. mulf x 2.,
--   b08 = lam x. mulf x 0.,
--   b09 = lam x. mulf x 1.,
--   b10 = lam x. mulf 0. x,
--   b11 = lam x. mulf 1. x,
--   b12 = lam x. lam y. mulf y x,
--   c = (eqi 1 1, eqi 1 2, neqi 1 1, neqi 1 2, lti 1 2, lti 1 1, gti 1 2, gti 1 1, leqi 1 2, leqi 1 1, geqi 1 2, geqi 1 1),
--   d = (eqf 1. 1., eqf 1. 2., neqf 1. 1., neqf 1. 2., ltf 1. 2., ltf 1. 1., gtf 1. 2., gtf 1. 1., leqf 1. 2., leqf 1. 1., geqf 1. 2., geqf 1. 1.),
--   c01 = lam x. eqi x x,
--   c02 = lam x. eqf x x,
--   c03 = lam x. eqc x x,
--   c04 = lam x. lam y. eqi y x,
--   c05 = lam x. lam y. eqf y x,
--   c06 = lam x. lam y. eqc y x,
--   e = (if true then 1 else 2,
--        if false then 1 else 2,
--        match (1,2) with (1, 2)then 1 else 2,
--        lam x. match x with _ then 1 else 2,
--        lam x. match x with y then y else 2,
--        (1, 2).0,
--        lam x. (2, x).1,
--        lam x. x.0,
--        lam x. match [1, x, 2] with [_,y] ++ _ in y)

-- }

-- include "avl.mc"
-- mexpr ()
/-

-- include "ext/mat-ext.mc"
-- include "ext/dist-ext.mc"

-- include "map.mc"

-- let _cmpSym = lam a. lam b. subi (sym2hash a) (sym2hash b)

-- === Distributions ===

type PDist a =
  { sample : () -> a
  , logObserve : a -> Float
  }
let p_sample : all a. PDist a -> a = lam dist. dist.sample ()
let p_logObserve : all a. PDist a -> a -> Float = lam dist. lam val. dist.logObserve val

let p_bernoulli : Float -> PDist Bool
  = lam p.
    { sample = lam. bernoulliSample p
    , logObserve = lam v. bernoulliLogPmf p v
    }


-- === PVal implementation ===

type Node a =
  { value : Ref a
  -- Identifying symbol, unique amongst all nodes
  , symbol : Symbol
  }
let _mkNode : all a. a -> Node a
  = lam value.
    { value = ref value
    , symbol = gensym ()
    }

type PVal a
con PPure : all a. a -> PVal a
type PMapRec a b =
  { f : a -> b
  , original : PVal a
  , node : Node b
  }
con PMap : all a. all b. PMapRec a b -> PVal b
type PMapWeightRec a b =
  { f : a -> (Float, b)
  , original : PVal a
  , selfWeight : Ref Float
  , node : Node b
  }
con PMapWeight : all a. all b. PMapWeightRec a b -> PVal b
type PApplyRec a b c =
  { f : a -> b -> c
  , a : PVal a
  , b : PVal b
  , node : Node c
  }
con PApply : all a. all b. all c. PApplyRec a b c -> PVal c
con PAssume : all a.
  { dist : PVal (PDist a)
  , oldWeight : Ref Float
  , toDrift : Ref (Option (a -> PDist a))
  , node : Node a
  } -> PVal a
con PCache : all a.
  { eq : a -> a -> Bool
  , original : PVal a
  , node : Node a
  } -> PVal a

let _getNode : all a. PVal a -> Node a
  = lam p. switch p
    case PMap x then x.node
    case PMapWeight x then x.node
    case PApply x then x.node
    case PAssume x then x.node
    case PCache x then x.node
    end

let _shallowReadPVal : all a. PVal a -> a
  = lam p.
    match p with PPure x
    then x
    else deref (_getNode p).value

let _isChanged : all a. PVal a -> Map Symbol Bool -> Bool
  = lam p. lam changed.
    match p with PPure _ then false else
    mapLookupOr false (_getNode p).symbol changed

type PUpdateState =
  { reset : [(Bool, () -> ())]
  , changed : Map Symbol Bool
  , permanentWeight : Float
  , temporaryWeight : Float
  }
let p_update : all a. PVal a -> PUpdateState -> PUpdateState
  = lam p. lam st.
    switch p
    case PPure _ then st
    case PMap x then
      let shouldChange = _isChanged x.original st.changed in
      let prevValue = deref x.node.value in
      (if shouldChange then
        let value = x.f (_shallowReadPVal x.original) in
        modref x.node.value value
       else ());
      { st with reset = snoc st.reset (shouldChange, lam. modref x.node.value prevValue)
      , changed = mapInsert x.node.symbol shouldChange st.changed
      }
    case PMapWeight x then
      let shouldChange = _isChanged x.original st.changed in
      let prevValue = deref x.node.value in
      let prevWeight = deref x.selfWeight in
      let deltaWeight =
        if shouldChange then
          match x.f (_shallowReadPVal x.original) with (newWeight, newValue) in
          modref x.node.value newValue;
          modref x.selfWeight newWeight;
          subf newWeight prevWeight
        else 0.0 in
      { st with reset = snoc st.reset
        (shouldChange, lam. modref x.node.value prevValue; modref x.selfWeight prevWeight)
      , changed = mapInsert x.node.symbol shouldChange st.changed
      , permanentWeight = addf st.permanentWeight deltaWeight
      }
    case PApply x then
      let shouldChange = or (_isChanged x.a st.changed) (_isChanged x.b st.changed) in
      let prevValue = deref x.node.value in
      (if shouldChange then
        let value = x.f (_shallowReadPVal x.a) (_shallowReadPVal x.b) in
        modref x.node.value value
       else ());
      { st with reset = snoc st.reset (shouldChange, lam. modref x.node.value prevValue)
      , changed = mapInsert x.node.symbol shouldChange st.changed
      }
    case PAssume x then
      let shouldResample = mapLookupOr false x.node.symbol st.changed in
      let shouldReweigh = or shouldResample (_isChanged x.dist st.changed) in
      let prevWeight = deref x.oldWeight in
      let prevValue = deref x.node.value in
      let deltaWeight =
        if shouldResample then
          -- TODO(vipa, 2025-09-04): `toDrift` is an Option because
          -- it's better for the other mode of operations, here we
          -- expect it to always be set to `Some`
          let drift = optionGetOrElse (lam. error "Impossible") (deref x.toDrift) in
          let kernel = drift prevValue in
          let proposal = p_sample kernel in
          let reverseKernel = drift proposal in
          let newWeight = p_logObserve (_shallowReadPVal x.dist) proposal in
          let prevToProposalProb = p_logObserve kernel proposal in
          let proposalToPrevProb = p_logObserve reverseKernel prevValue in
          modref x.oldWeight newWeight;
          modref x.node.value proposal;
          addf (subf newWeight prevWeight) (subf proposalToPrevProb prevToProposalProb)
        else if shouldReweigh then
          let newWeight = p_logObserve (_shallowReadPVal x.dist) prevValue in
          modref x.oldWeight newWeight;
          subf newWeight prevWeight
        else 0.0 in
      { st with changed = mapInsert x.node.symbol shouldResample st.changed
      , reset = concat st.reset
        [ (shouldResample, lam. modref x.node.value prevValue)
        , (shouldReweigh, lam. modref x.oldWeight prevWeight)
        ]
      , temporaryWeight = addf st.temporaryWeight deltaWeight
      }
    case PCache x then
      let prevValue = deref x.node.value in
      -- NOTE(vipa, 2025-09-04): This is the only place we introduce
      -- dynamic data into st.changed
      let shouldChange = if _isChanged x.original st.changed
        then not (x.eq prevValue (_shallowReadPVal x.original))
        else false in
      (if shouldChange then
        modref x.node.value (_shallowReadPVal x.original)
       else ());
      { st with changed = mapInsert x.node.symbol shouldChange st.changed
      , reset = snoc st.reset (shouldChange, lam. modref x.node.value prevValue)
      }
    end

let p_compileUpdate
  : [PUpdateState -> PUpdateState]
  -> Map Symbol Bool
  -> ({permanentWeight : Float} -> ({permanentWeight : Float, temporaryWeight : Float}, () -> ()))
  = lam updates. lam changed. lam st.
    let st =
      { permanentWeight = st.permanentWeight
      , temporaryWeight = 0.0
      , reset = []
      , changed = changed
      } in
    let st = foldl (lam st. lam f. f st) st updates in
    ( {permanentWeight = st.permanentWeight, temporaryWeight = st.temporaryWeight}
    , lam. for_ st.reset
      (lam pair. if pair.0 then pair.1 () else ())
    )

-- Pointed. Produce a "probabilistic" value that never changes.
let p_pure : all a. a -> PVal a
  = lam value. PPure value

-- Functor.
let p_map : all a. all b. (a -> b) -> PVal a -> PVal b
  = lam f. lam original.
    let res = f (_shallowReadPVal original) in
    let node = _mkNode res in
    PMap {f = f, original = original, node = node}

-- Functor + weight. Used for weight and observe. Map over a
-- probabilistic value. The Float is (log) weight accumulated through
-- `weight` and `observe`.
let p_mapWeight : all a. all b. (a -> (Float, b)) -> PVal a -> PVal b
  = lam f. lam original.
    let res = f (_shallowReadPVal original) in
    let node = _mkNode res.1 in
    PMapWeight {f = f, original = original, selfWeight = ref res.0, node = node}

-- Applicative functor. Combine two probabilistic values.
let p_apply : all a. all b. all c. (a -> b -> c) -> PVal a -> PVal b -> PVal c
  = lam f. lam a. lam b.
    let res = f (_shallowReadPVal a) (_shallowReadPVal b) in
    let node = _mkNode res in
    PApply {f = f, a = a, b = b, node = node}
let p_ap : all a. all b. PVal (a -> b) -> PVal a -> PVal b
  = lam f. lam a.
    p_apply (lam f. lam a. f a) f a
let p_apWeight : all a. all b. PVal (a -> (Float, b)) -> PVal a -> PVal b
  = lam f. lam a.
    p_mapWeight (lam x. x) (p_ap f a)

-- Sampling of values. Also produces a function that can be called to
-- resample this particular assume.
let p_assume : all a. PVal (PDist a) -> ((a -> PDist a) -> (), PVal a)
  = lam dist.
    let distV = _shallowReadPVal dist in
    let value = p_sample distV in
    let w = p_logObserve distV value in
    let node = _mkNode value in
    let toDrift = ref (None ()) in
    let doResample = lam drift.
      modref toDrift (Some drift) in
    (doResample, PAssume {dist = dist, oldWeight = ref w, toDrift = toDrift, node = node})

-- Don't recompute dependent values if a newly produced value is equal
-- to the previously produced value.
let p_cache : all a. (a -> a -> Bool) -> PVal a -> PVal a
  = lam eq. lam original.
    let value = _shallowReadPVal original in
    let node = _mkNode value in
    PCache {eq = eq, original = original, node = node}


mexpr
-- === Actual code that I want to PEval ===

let dist = p_bernoulli 0.5 in
match p_assume (p_pure dist) with (driftA, a) in
match p_assume (p_pure dist) with (driftB, b) in
let res = p_apply and a b in

-- NOTE(vipa, 2025-09-04): This is a topological sort of all nodes in
-- the graph. Later we'll compute it automatically from the graph
-- itself.
let updates =
  [ p_update a
  , p_update b
  , p_update res
  ] in

-- NOTE(vipa, 2025-09-04): The old interface would require setting
-- drift kernel each time we resample, here we can set them once in
-- the beginning.
driftA (lam. dist);
driftB (lam. dist);

-- NOTE(vipa, 2025-09-04): The functions below are the ones I want to
-- peval. I write my guesstimate for a peval'd version of the program
-- afterwards.
let resampleA : {permanentWeight : Float} -> ({permanentWeight : Float, temporaryWeight : Float}, () -> ()) =
  let toUpdate = mapFromSeq _cmpSym
    [ ((_getNode a).symbol, true)
    ] in
  p_compileUpdate updates toUpdate in

let resampleB : {permanentWeight : Float} -> ({permanentWeight : Float, temporaryWeight : Float}, () -> ()) =
  let toUpdate = mapFromSeq _cmpSym
    [ ((_getNode b).symbol, true)
    ] in
  p_compileUpdate updates toUpdate in

let resampleBoth : {permanentWeight : Float} -> ({permanentWeight : Float, temporaryWeight : Float}, () -> ()) =
  let toUpdate = mapFromSeq _cmpSym
    [ ((_getNode a).symbol, true)
    , ((_getNode b).symbol, true)
    ] in
  p_compileUpdate updates toUpdate in


()
-/
