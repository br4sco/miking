--------------------
--- Reader Monad ---
--------------------

type ReaderM r a = r -> a

let rmReturn : all r. all a. a -> ReaderM r a
  = lam x. lam. x

let rmBind : all r. all a. all b. ReaderM r a -> (a -> ReaderM r b) -> ReaderM r b
  = lam m. lam f. lam r. f (m r) r

let rmAsk : all r. ReaderM r r
  = lam r. r

let rmRun : all r. all a. ReaderM r a -> r -> a
  = lam m. lam r. m r

-------------------
--- State Monad ---
-------------------

type StateM s a = s -> (s, a)

let smReturn : all s. all a. a -> StateM s a
  = lam x. lam s. (s, x)

let smBind : all s. all a. all b. StateM s a -> (a -> StateM s b) -> StateM s b
  = lam m. lam f. lam s. match m s with (s2, x) in f x s2

let smBind2 : all s. all a. all b. all c.
  StateM s a -> StateM s b -> (a -> b -> StateM s c) -> StateM s c
  = lam m1. lam m2. lam f. smBind m1 (lam x. smBind m2 (lam y. f x y))

let smMap : all s. all a. all b. (a -> b) -> StateM s a -> StateM s b
  = lam f. lam m. smBind m (lam x. smReturn (f x))

let smMap2 : all s. all a. all b. all c.
  (a -> b -> c) -> StateM s a -> StateM s b -> StateM s c
  = lam f. lam m1. lam m2. smBind2 m1 m2 (lam x. lam y. smReturn (f x y))

let smGet : all s. StateM s s
  = lam s. (s, s)

let smPut : all s. s -> StateM s ()
  = lam s. lam. (s, ())

let smMod : all s. (s -> s) -> StateM s ()
  = lam f. smBind smGet (lam s. smPut (f s))

let smFoldlM : all s. all a. all b. (a -> b -> StateM s a) -> a -> [b] -> StateM s a
  = lam f. lam acc. foldl (lam acc. lam x. smBind acc (lam acc. f acc x)) (smReturn acc)

let smMapM : all s. all a. all b. (a -> StateM s b) -> [a] -> StateM s [b]
  = lam f. foldl (lam acc. lam x. smMap2 snoc acc (f x)) (smReturn [])

let smMapMapWithKeysM : all s. all k. all a. all b.
  (k -> a -> StateM s b) -> Map k a -> StateM s (Map k b)
  = lam f. lam m.
    smBind
      (smMapM (lam t. smMap (lam v. (t.0, v)) (f t.0 t.1)) (mapBindings m)) (lam bs.
      smReturn (mapFromSeq (mapGetCmpFun m) bs))

let smMapMapM : all s. all k. all a. all b.
  (a -> StateM s b) -> Map k a -> StateM s (Map k b)
  = lam f. smMapMapWithKeysM (lam. f)

let smRun : all s. all a. StateM s a -> s -> (s, a)
  = lam m. lam s. m s

--------------------
--- Writer Monad ---
--------------------

type Monoid a = {
  identity : a,
  add : a -> a -> a
}

type WriterM w a = (w, a)

let wmReturn : all w. all a. Monoid w -> a -> WriterM w a
  = lam mo. lam x. (mo.identity, x)

let wmBind : all w. all a. all b.
  Monoid w -> WriterM w a -> (a -> WriterM w b) -> WriterM w b
  = lam mo. lam m. lam f.
    match m with (w1, x) in match f x with (w2, y) in (mo.add w1 w2, y)

let wmRecord : all w. w -> WriterM w ()
  = lam w. (w, ())

let wmRun : all w. all a. WriterM w a -> (w, a)
  = lam m. m

---------------------------
--- Reader State Monad ---
---------------------------

type ReaderStateM r s a = r -> s -> (s, a)

let rsmReturn : all r. all s. all a. a -> ReaderStateM r s a
  = lam x. lam. lam s. (s, x)

let rsmBind : all r. all s. all a. all b.
  ReaderStateM r s a -> (a -> ReaderStateM r s b) -> ReaderStateM r s b
  = lam m. lam f. lam r. lam s. match m r s with (s2, a) in f a r s2

let rsmAsk : all r. all s. ReaderStateM r s r
  = lam r. lam s. (s, r)

let rsmRun : all r. all s. all a. ReaderStateM r s a -> r -> s -> (s, a)
  = lam m. lam r. lam s. m r s

---------------------------------
--- Reader State Writer Monad ---
---------------------------------

type ReaderStateWriterM r s w a = r -> s -> (s, w, a)

let rswmReturn : all r. all s. all w. all a. Monoid w -> a -> ReaderStateWriterM r s w a
  = lam mo. lam x. lam. lam s. (s, mo.identity, x)

let rswmBind : all r. all s. all w. all a. all b.
  Monoid w ->
    ReaderStateWriterM r s w a ->
      (a -> ReaderStateWriterM r s w b) ->
       ReaderStateWriterM r s w b
  = lam mo. lam m. lam f. lam r. lam s.
    match m r s with (s2, w1, a) in
    match f a r s2 with (s3, w2, b) in
    (s3, mo.add w1 w2, b)

let rswmBind2 : all r. all s. all w. all a. all b. all c.
  Monoid w ->
    ReaderStateWriterM r s w a ->
      ReaderStateWriterM r s w b ->
        (a -> b -> ReaderStateWriterM r s w c) ->
         ReaderStateWriterM r s w c
  = lam mo. lam m1. lam m2. lam f.
    rswmBind mo m1 (lam x. rswmBind mo m2 (lam y. f x y))

let rswmBind3 : all r. all s. all w. all a. all b. all c. all d.
  Monoid w ->
    ReaderStateWriterM r s w a ->
      ReaderStateWriterM r s w b ->
        ReaderStateWriterM r s w c ->
          (a -> b -> c -> ReaderStateWriterM r s w d) ->
           ReaderStateWriterM r s w d
  = lam mo. lam m1. lam m2. lam m3. lam f.
    rswmBind2 mo m1 m2 (lam x. lam y. rswmBind mo m3 (lam z. f x y z))

let rswmMap : all r. all s. all w. all a. all b.
  Monoid w -> (a -> b) -> ReaderStateWriterM r s w a -> ReaderStateWriterM r s w b
  = lam mo. lam f. lam m. rswmBind mo m (lam x. rswmReturn mo (f x))

let rswmMap2 : all r. all s. all w. all a. all b. all c.
  Monoid w ->
    (a -> b -> c) ->
     ReaderStateWriterM r s w a ->
       ReaderStateWriterM r s w b ->
         ReaderStateWriterM r s w c
  = lam mo. lam f. lam m1. lam m2. rswmBind2 mo m1 m2 (lam x. lam y. rswmReturn mo (f x y))

let rswmAsk : all r. all s. all w. Monoid w -> ReaderStateWriterM r s w r
  = lam mo. lam r. lam s. (s, mo.identity, r)

let rswmGet : all r. all s. all w. Monoid w -> ReaderStateWriterM r s w s
  = lam mo. lam. lam s. (s, mo.identity, s)

let rswmPut : all r. all s. all w. Monoid w -> s -> ReaderStateWriterM r s w ()
  = lam mo. lam s. lam. lam. (s, mo.identity, ())

let rswmMod : all r. all s. all w. Monoid w -> (s -> s) -> ReaderStateWriterM r s w ()
  = lam mo. lam f. rswmBind mo (rswmGet mo) (lam s. rswmPut mo (f s))

let rswmRecord : all r. all s. all w. w -> ReaderStateWriterM r s w ()
  = lam w. lam. lam s. (s, w, ())

let rswmMapM : all r. all s. all w. all a. all b.
  Monoid w ->
    (a -> ReaderStateWriterM r s w b) ->
     [a] ->
       ReaderStateWriterM r s w [b]
  = lam mo. lam f. foldl (lam acc. lam x. rswmMap2 mo snoc acc (f x)) (rswmReturn mo [])

let rswmFoldlM : all r. all s. all w. all a. all b.
  Monoid w ->
    (a -> b -> ReaderStateWriterM r s w a) ->
     a ->
       [b] ->
         ReaderStateWriterM r s w a
  = lam mo. lam f. lam acc.
    foldl (lam acc. lam x. rswmBind mo acc (lam acc. f acc x)) (rswmReturn mo acc)

let rswmFoldrM : all r. all s. all w. all a. all b.
  Monoid w ->
    (b -> a -> ReaderStateWriterM r s w a) ->
     a ->
       [b] ->
         ReaderStateWriterM r s w a
  = lam mo. lam f. lam acc.
    foldr (lam x. lam acc. rswmBind mo acc (lam acc. f x acc)) (rswmReturn mo acc)

let rswmMapiM : all r. all s. all w. all a. all b.
  Monoid w ->
    (Int -> a -> ReaderStateWriterM r s w b) ->
     [a] ->
       ReaderStateWriterM r s w [b]
  = lam mo. lam f.
    foldli (lam acc. lam i. lam x. rswmMap2 mo snoc acc (f i x)) (rswmReturn mo [])

let rswmMapMapWithKeysM : all r. all s. all w. all k. all a. all b.
  Monoid w ->
    (k -> a -> ReaderStateWriterM r s w b) ->
     Map k a ->
       ReaderStateWriterM r s w (Map k b)
  = lam mo. lam f. lam m.
    rswmBind mo
      (rswmMapM mo
         (lam t. rswmMap mo (lam v. (t.0, v)) (f t.0 t.1))
         (mapBindings m))
      (lam bs. rswmReturn mo (mapFromSeq (mapGetCmpFun m) bs))

let rswmMapMapM : all r. all s. all w. all k. all a. all b.
  Monoid w ->
    (a -> ReaderStateWriterM r s w b) ->
     Map k a ->
       ReaderStateWriterM r s w (Map k b)
  = lam mo. lam f. rswmMapMapWithKeysM mo (lam. f)

let rswmRun : all r. all s. all w. all a.
  ReaderStateWriterM r s w a -> r -> s -> (s, w, a)
  = lam m. lam r. lam s. m r s

mexpr

let mo = { identity = 0, add = addi } in
let run = rswmRun in
let bind = lam x. rswmBind mo x in
let map = lam x. rswmMap mo x in
let put = lam x. rswmPut mo x in
let mod = lam x. rswmMod mo x in
let ask = lam x. rswmAsk mo x in
let record = rswmRecord in
let return = lam x. rswmReturn mo x in

utest
  run
    (map (lam x. subi x 1)
       (bind (put 0) (lam.
         bind (mod (addi 1)) (lam.
           bind ask (lam r.
             bind (record (get r 1)) (lam.
               return (get r 2)))))))
    [1, 2, 4] 100
  with (1, 2, 3) in
()
