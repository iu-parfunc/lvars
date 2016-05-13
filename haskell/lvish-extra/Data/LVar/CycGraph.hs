{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O2 #-}

{-|

In contrast with "Data.LVar.Memo", this module provides a way to run a computation
for each node of a graph WITH support for cycles.  Cycles are explicitly recognized
and then may be handled in an application specific fashion.

 -}

module Data.LVar.CycGraph
       (
         -- * An idiom for fixed point computations
         exploreGraph_seq,
         Response(..),

         -- * A parallel version
         exploreGraph, NodeValue(..), NodeAction,

         -- * Debugging aides
         ShortShow(..), shortTwo
       )
       where
-- Standard:
import           Control.Monad
import           Data.Char        (ord)
import qualified Data.Foldable    as F
import           Data.Int
import           Data.IORef
import           Data.List        (intersperse)
import qualified Data.Map         as M
import           Data.Set         (Set)
import qualified Data.Set         as S
import           System.IO.Unsafe

-- LVish:
import           Control.LVish
import qualified Control.LVish.Internal                 as LV
import           Data.LVar.IVar                         as IV
import qualified Data.LVar.PureMap                      as IM
import           Data.LVar.PureSet                      as IS

----- For debugging: ----
#ifdef DEBUG_MEMO
import           Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree as G
import           Data.GraphViz                     as GV
import qualified Data.GraphViz.Attributes.Colors   as GC
import qualified Data.GraphViz.Attributes.Complete as GA
import           Data.Text.Lazy                    (pack)
import           System.Environment                (getEnvironment)
#endif
--------------------------------------------------------------------------------
-- Simple atomic Set accumulators
--------------------------------------------------------------------------------

#if 0

-- | Could use a more scalable structure here... but we need union as well as
-- elementwise insertion.
type SetAcc a = IORef (S.Set a)

-- Here @SetAcc@s are LINKED to downstream SetAcc's which must receive all the same
-- inserts that they do.
-- newtype SetAcc a = SetAcc (IORef (S.Set a, [SetAcc a]))

newSetAcc :: Par e s (SetAcc a)
newSetAcc = LV.WrapPar $ LI.liftIO $ newIORef S.empty
readSetAcc :: (SetAcc a) -> Par e s (S.Set a)
readSetAcc r = LV.WrapPar $ LI.liftIO $ readIORef r
insertSetAcc :: Ord a => a -> SetAcc a -> Par e s (S.Set a)
insertSetAcc x ref = LV.WrapPar $ LI.liftIO $
                     atomicModifyIORef' ref (\ s -> let ss = S.insert x s in (ss,ss))
unionSetAcc :: Ord a => Set a -> SetAcc a -> Par e s (S.Set a)
unionSetAcc x ref = LV.WrapPar $ LI.liftIO $
                    atomicModifyIORef' ref (\ s -> let ss = S.union x s in (ss,ss))

#endif
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A Memo-table that stores cached results of executing a `Par` computation.
--
--   This, enhanced, version of the Memo-table also is required to track all the keys
--   that are reachable from each key (for cycle-detection).
data Memo (e::EffectSig) s k v =
  -- Here we keep both a Ivars of return values, and a set of keys whose computations
  -- have traversed through THIS key.  If we see a cycle there, we can catch it.
--       !(IM.IMap k s (SetAcc k, IVar s v))

  Memo !(IS.ISet s k)
       -- EXPENSIVE version:
       !(IM.IMap k s (NodeRecord s k v))
         -- ^ Store all the keys that we know *can reach this key*

-- | All the information associated with one node in the graph of keys.
data NodeRecord s k v = NodeRecord
  { mykey    :: k
  , chldrn   :: [k]
  , reachme  :: !(IS.ISet s k)  -- ^ Which keys are upstream of me in the graph
  , in_cycle :: !(IVar s Bool)  -- ^ Does this node participate in any cycle?
  , result   :: !(IVar s v)     -- ^ The result of the per-node computation.
  } deriving (Eq)

--------------------------------------------------------------------------------
-- Cycle-detecting mapping of a computation over graph neighborhoods
--------------------------------------------------------------------------------

-- | A means of building a dynamic graph.  The node computation returns a response
-- which may either be a final value, or a request to explore more nodes (together
-- with a continuation for the resulting value).
--
-- Note that because only one key is requested at a time, this cannot express
-- parallel graph traversals.
data Response par key ans =
    Done !ans
  | Request !key (RequestCont par key ans)

type RequestCont par key ans = (ans -> par (Response par key ans))

--------------------------------------------------------------------------------
-- Sequential version:

-- | This supercombinator does a parallel depth-first search of a dynamic graph, with
-- detection of cycles.
--
-- Each node in the graph is a computation whose input is the `key` (the vertex ID).
-- Each such computation dynamically computes which other keys it depends on and
-- requests the values associated with those keys.
--
-- This implementation uses a sequential depth-first-search (DFS), starting from the
-- initially requested key.  One can picture this search as a directed tree radiating
-- from the starting key.  When a cycle is detected at any leaf of this tree, an
-- alternate cycle handler is called instead of running the normal computation for
-- that key.
exploreGraph_seq :: forall e s k v . (Ord k, Eq v, Show k, Show v) =>
                          (k -> Par e s (Response (Par e s) k v)) -- ^ The computation to perform for new requests
                       -> (k -> Par e s v)  -- ^ Handler for a cycle on @k@.  The
                                            -- value it returns is in lieu of running
                                            -- the main computation at this
                                            -- particular node in the graph.
                          -> k              -- ^ Key to lookup.
                       -> Par e s v
exploreGraph_seq initCont cycHndlr initKey = do
  -- Start things off:
  resp <- initCont initKey
  v <- loop initKey (S.singleton initKey) resp return
  return v
 where
   loop :: k -> S.Set k -> (Response (Par e s) k v) -> (v -> Par e s v) -> Par e s v
   loop current hist resp kont = do
    dbgPr (" [MemoFixedPoint] going around loop, key "++showID current++", hist size "++show (S.size hist))
    case resp of
      Done ans -> do dbgPr ("  !! Final result, answer "++show ans)
                     kont ans
      Request key2 newCont
        -- Here we have hit a cycle, and label it as such for the CURRENT node.
        | S.member key2 hist -> do
          dbgPr ("    Stopping before hitting a cycle on "++showID key2++", call cycHndlr on "++showID current)
          ans <- cycHndlr current
          kont ans
        | otherwise -> do
          dbgPr ("  Requesting child computation with key "++showWID key2)
          resp' <- initCont key2
          loop key2 (S.insert key2 hist) resp' $ \ ans2 -> do
            dbgPr ("  DONE blocking on child key, cont invoked with answer: "++show ans2)
            resp'' <- newCont ans2
            -- Popping back to processing the current key, which may not be finished.
            loop current hist resp'' kont

-- --            if wasloop then do
--             if False then do
--                -- Here the child computation ended up being processed as a cycle, so we must be as well:
--                dbgPr ("    Child comp "++showID key2++" of "++showID current++" hit a cycle...")
--                ans3 <- cycHndlr current
--                kont (True,ans3)


--------------------------------------------------------------------------------

type IsCycle = Bool

-- | The handler at a particular node (key) in the graph.  This takes as argument a
--   key, along with a boolean indicating whether the current node has been found to
--   be part of a cycle.
--
--   Also, for each child node, this handler is provided a way to demand the
--   resulting value of that child node, plus an indication of whether the child node
--   participates in a cycle.
--
--   Finally, this handler is expected to produce a value which becomes associated
--   with the key.
type NodeAction e s k v =
--     Bool -> k  -> [(Bool,Par e s v)] -> Par e s v
     IsCycle -> k  -> [(k,IsCycle,IV.IVar s v)] -> Par e s (NodeValue k v)
  -- One thing that's missing here is WHICH child node(s) puts us in a cycle.

-- | At the end of the handler execution, the value of a node is either ready, or it
-- is instead deferred to be exactly the value provided by another key.
data NodeValue k v = FinalValue !v | Defer k
  deriving (Show,Eq,Ord)


-- | This combinator provides parallel exploration of a graph that contains cycles.
-- The limitation is that the work to be performed at each node (`NodeAction`) is not
-- invoked until the graph is fully traversed, i.e. after a barrier.  Thus the graph
-- explored is not a "dynamic graph" in the sense of being computed on the fly by the
-- `NodeAction`.
--
-- The algorithm used in this function is fairly expensive.  For each node, it uses a
-- monotonic data structure to track the full set of other nodes that can reach it in
-- the graph.
#ifdef DEBUG_MEMO
exploreGraph :: forall s k v e . (Ord k, Eq v, ShortShow k, Show v, HasPut e, HasGet e, HasFreeze e) =>
#else
exploreGraph :: forall s k v e . (Ord k, Eq v, Show k, Show v, HasPut e, HasGet e, HasFreeze e) =>
#endif
                      (k -> Par e s [k])  -- ^ Sketch the graph: map a key onto its children.
                   -> NodeAction e s k v  -- ^ The computation to run at each graph node.
                   -> k                          -- ^ The initial node (key) from which to explore.
                   -> Par e s v
exploreGraph keyNbrs nodeHndlr initKey = do

  -- First: propogate key requests.
  -- This will not diverge because the Set here suppressed duplicate callbacks:
  set <- IS.newEmptySet
  -- The map stores results:
  mp  <- IM.newEmptyMap

  keywalkHP <- newPool

  IS.forEachHP (Just keywalkHP) set $ \ key0 -> do
    dbgPr ("![MemoFixedPoint] Start new key "++show key0)
    -- Make some empty space for results:
    key0_res   <- IV.new
    key0_cycle <- IV.new
    key0_reach <- IS.newEmptySet
    -- Next fetch the child node identities:
    child_keys <- keyNbrs key0
    IM.insert key0 (NodeRecord key0 child_keys key0_reach key0_cycle key0_res) mp
    dbgPr ("  Computed nbrs of "++showID key0++" to be: "++ (showIDs child_keys))

    case child_keys of
      [] -> return () -- IV.put_ key0_cycle False
      _  -> do
       -- Spawn traversals of child nodes:
       forM_ child_keys (`IS.insert` set)

       -- Establish the (expensive) cycle-checker handler:
       IS.forEachHP (Just keywalkHP) key0_reach $ \ key1 ->
         when (key1 == key0) $ do
           dbgPr ("   !! Cycle detected on key "++showID key0)
           IV.put_ key0_cycle True

       -- Now we must wait for records to come up, and establish ourselves as upstream
       -- of each child:
       chldrecs <- forM child_keys $ \child -> do
         nrec@NodeRecord{reachme} <- IM.getKey child mp
         IS.insert key0 reachme -- Child is reachable from us.
         -- Further, what reaches us, reaches the child:
         copyTo keywalkHP key0_reach reachme
         dbgPr ("   Inserted ourselves ("++showID key0++") in reachme list of child: "++showID child)
         return nrec

       -- If all our children are do not participate in a cycle, neither do we.
       -- fork $ let loop [] = IV.put_ key0_cycle False
       --            loop (NodeRecord{in_cycle}:tl) = do
       --                bl <- IV.get in_cycle
       --                case bl of
       --                  True  -> return ()
       --                  False -> loop tl
       --        in loop chldrecs
       -- FINISHME: If we have some cycle children and some leafish ones....
       -- then we may need to do an unsafe peek at our reachme set, no?
       return ()

  IS.insert initKey set
  quiesce keywalkHP
  -- fset <- IS.freezeSet set
  frmap <- IM.freezeMap mp

  dbgPr ("Froze map: "++show (M.keys frmap))

  -- TODO: need parallel traversable:
  let getcyc vr = do mb <- IV.freezeIVar vr
                     if mb == Just True
                       then return True
                       else return False
      showCyc bl = if bl then "cycle" else "Nocyc"
      fn NodeRecord{mykey, chldrn, reachme,in_cycle=mecyc,result=myres} () = fork$ do
          bl  <- getcyc mecyc
          bls <- mapM (getcyc . in_cycle . (frmap #)) chldrn
          dbgPr ("   !! Invoking node handler at key "++showID mykey++" "++
               showCyc bl ++" chldrn "++concat (intersperse " "$ map showCyc bls))
          x  <- nodeHndlr bl mykey [ (k, b, result (frmap # k)) | b <- bls
                                                                | k <- chldrn ]
          case x of
            FinalValue vv -> do
              dbgPr ("   !! Writing result into key "++showID mykey++" value: "++show x)
              IV.put_ myres vv
            Defer tokey -> do dbgPr ("   !! No result yet on key "++showID mykey++", DEFERing to key "++showID tokey)
                              fork $ do kv <- IV.get (result(frmap # tokey))
                                        dbgPr ("   .. Delegated key "++showID tokey++", of key "++showID mykey++" produced result: "++show kv)
                                        IV.put_ myres kv
  F.foldrM fn () frmap

  let NodeRecord{result} = frmap # initKey
  final <- IV.get result
  ------------------------------------------------------------
  -- TEMP: Debugging
  ------------------------------------------------------------
#ifdef DEBUG_MEMO
  when (dbg_lvl >= 4) $ do
     dbgPr ("| START creating dot graph...")
     dg <- debugVizMemoGraph True initKey frmap
     unsafePerformIO (GV.runGraphviz dg GV.Pdf "MemoCyc_short.pdf")
       `seq` return ()
     dg <- debugVizMemoGraph False initKey frmap
     unsafePerformIO (GV.runGraphviz dg GV.Pdf "MemoCyc.pdf")
       `seq` return ()
     dbgPr ("| DONE creating dot graph...")
#endif
  ------------------------------------------------------------
  return final
--  return $! Memo set mp

{-


-- | This version watches for, and catches, cyclic requests to the memotable that
-- would normally diverge.  Once caught, the user specifies what to do with these
-- cycles by providing a handler.  The handler is called on the key which formed the
-- cycle.  That is, computing the invocation spawned by that key results in a demand
-- for that key.
makeMemoCyclic :: (MemoTable d s a b -> a -> Par e s b) -> (a -> Par e s b) -> Par e s (MemoTable d s a b)
makeMemoCyclic normalFn ifCycle  = undefined
-- FIXME: Are there races where more than one cycle can be hit?  Can we guarantee
-- that all are hit?



-- | Cancel an outstanding speculative computation.  This recursively attempts to
-- cancel any downstream computations in this or other memo-tables that are children
-- of the given `MemoFuture`.
cancel :: MemoFuture Det s b -> Par Det s ()
-- FIXME: Det needs to be replaced here with "GetOnly".
cancel fut = undefined

-}

--------------------------------------------------------------------------------
-- Misc Helpers and Utilities
--------------------------------------------------------------------------------

(#) :: (Ord a1, Show a1) => M.Map a1 a -> a1 -> a
m # k = case M.lookup k m of
         Nothing -> error$ "Key was missing from map: "++show k
         Just x  -> x

showMapContents :: (Eq t1, Show a, Show a1) => IM.IMap a1 s (IORef (Set a), IV.IVar t t1) -> IO String
showMapContents (IM.IMap lv) = do
  mp <- readIORef (LV.state lv)
  let lst = M.toList mp
  return$ "    Map Contents: (length "++ show (length lst) ++")\n" ++
    concat [ "      "++fullempt++" "++showWID k++" -> "++vals++"\n"
           | (k,(v,IV.IVar ivr)) <- lst
--            , let vals = "hello"
           , let lst = S.toList $ unsafePerformIO (readIORef v)
           , let vals = "#"++show (length lst)++"["++ (concat $ intersperse ", " $ map showID lst)  ++"]"
           , let fullempt = if Nothing == unsafePerformIO (readIORef (LV.state ivr))
                            then "[empty]"
                            else "[full]"
           ]

showMapContents2 :: (Eq t3, Show t1, Show a) => IM.IMap a s (ISet t t1, IV.IVar t2 t3) -> IO String
showMapContents2 (IM.IMap lv) = do
  mp <- readIORef (LV.state lv)
  let lst = M.toList mp
  return$ "    Map Contents: (length "++ show (length lst) ++")\n" ++
    concat [ "      "++fullempt++" "++showWID k++" -> "++vals++"\n"
           | (k,(IS.ISet setlv, IV.IVar ivr)) <- lst
--            , let vals = "hello"
           , let lst = S.toList $ unsafePerformIO (readIORef (LV.state setlv))
           , let vals = "#"++show (length lst)++"["++ (concat $ intersperse ", " $ map showID lst)  ++"]"
           , let fullempt = if Nothing == unsafePerformIO (readIORef (LV.state ivr))
                            then "[empty]"
                            else "[full]"
           ]

-- | Variant of `union` that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
copyTo :: (HasPut e, Ord a) => HandlerPool -> IS.ISet s a -> IS.ISet s a -> Par e s ()
copyTo hp sfrom sto = do
  IS.forEachHP (Just hp) sfrom (`insert` sto)

{-# INLINE dbgPr #-}
dbgPr :: Monad m => String -> m ()
#ifdef DEBUG_MEMO
dbgPr s | dbg_lvl >= 1 = trace s (return ())
        | otherwise = return ()
#else
dbgPr _ = return ()
#endif

showWID :: Show a => a -> String
showWID x = let str = (show x) in
            if length str < 10
            then str
            else showID x++"__"++str

showID :: Show a => a -> String
showID x = let str = (show x) in
           if length str < 10 then str
           else (show (length str))++"-"++ show (checksum str)

showIDs :: Show a => [a] -> [Char]
showIDs ls = ("{"++(concat$ intersperse ", " $ map showID ls)++"}")

checksum :: String -> Int
checksum str = sum (map ord str)


--------------------------------------------------------------------------------
-- DEBUGGING
--------------------------------------------------------------------------------

-- | A show class that tries to stay under a budget.
class Show t => ShortShow t where
  shortShow :: Int -> t -> String
  shortShow n x = take n (show x)

instance ShortShow Bool where
  shortShow 1 True  = "t"
  shortShow 1 False = "f"
  shortShow 2 True  = "#t"
  shortShow 2 False = "#f"
  shortShow n b     = take n (show b)

instance ShortShow Integer where shortShow = shortShowNum
instance ShortShow Int   where shortShow = shortShowNum
instance ShortShow Int8  where shortShow = shortShowNum
instance ShortShow Int16 where shortShow = shortShowNum
instance ShortShow Int32 where shortShow = shortShowNum
instance ShortShow Int64 where shortShow = shortShowNum

shortShowNum :: Show a => Int -> a -> String
shortShowNum n num =
    let str = show num
        len = length str in
    if len > n then
      (take (n-2) str)++".."
    else str

instance ShortShow String where
  shortShow n str =
    let len = length str in
    if len > 2 && n ==2
    then ".."
    else if len > 1 && n == 1
    then "?"
    else take n str

instance (ShortShow a, ShortShow b) => ShortShow (a,b) where
  shortShow 1 _ = "?"
  shortShow 2 _ = ".."
  shortShow n (a,b) = let (l,r) = shortTwo (n-3) a b
                      in "("++ l ++","++ r ++")"

-- | Combine two things within a given size budget.
shortTwo :: (ShortShow t, ShortShow t1) => Int -> t -> t1 -> (String, String)
-- this could be better...
shortTwo n a b = (left, shortShow (half+remain) b)
   where
     remain = abs (half - length left)
     left = shortShow half a
     (q,r) = quotRem (abs(n-3)) 2
     half = q + r

--------------------------------------------------------------------------------

#ifdef DEBUG_MEMO

-- | Debugging flag shared by all accelerate-backend-kit modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbg_lvl :: Int
dbg_lvl = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         trace (" ! Responding to env Var: DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

defaultDbg :: Int
defaultDbg = 0

debugVizMemoGraph :: forall s t t1 t2 e .
                     (Ord t1, ShortShow t1, Show t2, F.Foldable t,
                      HasGet e, HasFreeze e) =>
                     Bool                       -- ^ Use shorter `showID` for keys.
                     -> t1                      -- ^ The inital key.
                     -> t (NodeRecord s t1 t2)  -- ^ A frozen map of graph nodes.
--                     Par e s (Gr (Bool,String) ())
                     -> Par e s (GV.DotGraph G.Node)
debugVizMemoGraph idOnly initKey frmap = do
  let showKey = if idOnly then showID
                else shortShow 40
  let gcons :: (HasFreeze e) =>
               NodeRecord s t1 t2
            ->                (M.Map t1 G.Node, G.Gr (Bool,t1,t2) ())
            -> Par e s (M.Map t1 G.Node, G.Gr (Bool,t1,t2) ())
      gcons NodeRecord{mykey, in_cycle,result}
            (labmap, gracc) = do
        dbgPr (" .. About to wait for node result, key "++show mykey)
        res <- IV.get result
        dbgPr (" .. About to wait for node in_cycle, key "++show mykey)
        cyc <- IV.freezeIVar in_cycle
        let num = 1 + G.noNodes gracc
            gr' = G.insNode (num, (cyc == Just True,mykey,res)) $
                  gracc
            labmap' = M.insert mykey num labmap
        return (labmap',gr')

      gedges :: NodeRecord s t1 t2
            ->         (M.Map t1 G.Node, G.Gr (Bool,t1,t2) ())
            -> Par e s (M.Map t1 G.Node, G.Gr (Bool,t1,t2) ())
      gedges NodeRecord{mykey, chldrn }
            (labmap, gracc) = do
        let chldnodes = map (labmap #) chldrn
            num = labmap # mykey
            gr' = G.insEdges [ (num,cnd::Int,()) | cnd <- chldnodes ] $
                  gracc
            labmap' = M.insert mykey num labmap
        return (labmap',gr')

  dbgPr (" !! Creating graphviz graph from MemoCyc map of size "++show (F.foldr (\ _ n -> 1+n) 0 frmap))
--  dbgPr (" !! All keys "++show frmap)

  -- Two passes, first add nodes, then edges:
  (lm,graph0) <- F.foldrM gcons (M.empty, G.empty) frmap
  dbgPr (" .. Added all nodes to the graph...")
  (_,graph)   <- F.foldrM gedges (lm, graph0) frmap
  dbgPr (" .. Added all edges to the graph...")
  let -- dg = graphToDot nonClusteredParams graph
      myparams :: GV.GraphvizParams G.Node (Bool,t1,t2) () () (Bool,t1,t2)
      myparams = GV.defaultParams { GV.fmtNode= nodeAttrs }

      nodeAttrs :: (Int, (Bool,t1,t2)) -> [GA.Attribute]
--      nodeAttrs :: (Int, String) -> [GA.Attribute]
      nodeAttrs (_num, (cyc,key,res)) =
        let lbl = showKey key++"\n=> "++ show res in
        [ GA.Label$ GA.StrLabel $ pack lbl ] ++
        (if key == initKey
         then [GA.Color [weighted$ GA.X11Color GV.Red]]
         else []) ++
        (if cyc then []
         else [GA.Shape GA.BoxShape])

      dg = GV.graphToDot myparams graph -- (G.nmap uid graph)
  return dg

weighted c = GC.WC {GC.wColor=c, GC.weighting=Nothing}

#endif
-- End DEBUG_MEMO
