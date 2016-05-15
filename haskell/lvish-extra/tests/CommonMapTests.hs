
-- FRAGMENT: not a complete file.

-- ASSUMES: module "IM" refers to the Map implementation.

#include "CommonMapWriteTests.hs"

--------------------------------------------------------------------------------
-- Handlers:

-- | Add one then subtract one to all entries.
prop_traverse :: [Int] -> Bool
prop_traverse ls = mkSimpleIdentityProp fn (zip ls ls)
  where
    fn mp = IM.traverseMap (\_ x -> return (x+1)) mp >>=
            IM.traverseMap (\_ x -> return (x-1))

--------------------------------------------------------------------------------
-- Blocking reads/gets:

-- | Should always be able to get the things that go in.
prop_getKey :: [Int] -> Bool
prop_getKey ls = mkSimpleIdentityProp fn (zip ls ls)
  where
    uniq = L.length $ L.nub ls
    fn mp = do forM_ ls $ \ key ->
                 IM.getKey key mp
               return mp

-- | Wait until it is full before proceeding.
prop_waitSize :: [Int] -> Bool
prop_waitSize ls = mkSimpleIdentityProp fn (zip ls ls)
  where
    uniq = L.length $ L.nub ls
    fn mp = do IM.waitSize uniq mp
               return mp

-- | Wait until it is full before proceeding.
prop_waitVal :: [Int] -> Bool
prop_waitVal ls = mkSimpleIdentityProp fn (zip ls ls)
  where
    uniq = L.length $ L.nub ls
    fn mp = do forM_ ls $ \ v -> IM.waitValue v mp
               return mp

--------------------------------------------------------------------------------
-- Read and write:

case_v7a :: Assertion
case_v7a = assertEqual "basic imap test"
           [1.0,2.0,3.0,100.1,201.1] =<<
           v7a

v7a :: IO [Float]
v7a = fmap (L.sort . F.toList) $
  runParNonDet $ isND $ IM.freezeMap =<<
  do mp <- IM.newEmptyMap
     fork $ do IM.waitSize 3 mp
               IM.insert (100::Int) (100.1::Float) mp
     fork $ do IM.waitValue 100.1 mp
               v <- IM.getKey 1 mp
               IM.insert 200 (200.1 + v) mp
     IM.insert 1 1 mp
     IM.insert 2 2 mp
     logDbgLn 1 "[v7a] Did the first two puts.."
     I.liftIO$ threadDelay 1000
     IM.insert 3 3 mp
     logDbgLn 1 "[v7a] Did the first third put."
     IM.waitSize 5 mp
     return mp


case_doubleInsert :: Assertion
case_doubleInsert = assertEqual "double insert on a map"
   True
   (runPar $ isDet $ do
     mp <- IM.newEmptyMap
     IM.insert (3::Int) True mp
     IM.insert 3 True mp
     IM.getKey 3 mp)


--------------------------------------------------------------------------------
-- Tests that use `forEachHP`
--------------------------------------------------------------------------------

case_v8c :: Assertion
case_v8c = assertEqual "forEachHP on maps" [101,102] =<< v8c

-- | Similar test with Maps instead of Sets.
v8c :: IO [Int]
v8c = fmap (L.sort . F.toList) $
      runParNonDet $ ioButNoBump $ do
  hp <- newPool
  m1 <- IM.newFromList ([(1,1),(2,2)]::[(Int,Int)])
  m2 <- IM.newEmptyMap
  let cb k v = do logDbgLn 1$" [v8c]  Inside callback for Map.. key="++show k
                  IM.insert k (v+100) m2
  IM.forEachHP (Just hp) m1 cb
  logDbgLn 1 " [v8c] Everything set up; about to quiesce..."
  quiesce hp
  logDbgLn 1 " [v8c] quiesce finished, next freeze:"
  IM.freezeMap m2


case_v8d :: Assertion
case_v8d = assertEqual "union on maps" [40,50,101,102] =<< runParQuasiDet v8d
           -- fmap (L.sort . F.toList)  (runParNonDet v8d)
           -- stressTest 0 30 v8d (== [40,50,101,102])

-- fmap (L.sort . F.toList) $
-- v8d :: Par QuasiDet s (TheMap Int Frzn Int)
-- v8d :: QuasiDeterministic e => Par e s [Int]
v8d :: (HasFreeze e, HasPut e, NoBump e) => Par e s [Int]
v8d = do
  hp <- newPool
  logDbgLn 1 " [v8d] Got a new pool..."
  m1 <- IM.newFromList [(1,1),(2,2) :: (Int,Int)]
  m2 <- IM.newFromList [(40,40),(50,50)]
  logDbgLn 1 " [v8d] Got two fresh maps..."
  let cb k v = do logDbgLn 1$" [v8d]  Inside callback for traverse.. key="++show k
                  return (v+100)
  m3 <- IM.traverseMapHP (Just hp) cb m1
  m4 <- IM.unionHP       (Just hp) m2 m3
  IM.forEachHP (Just hp) m4 $ \ k elm ->
    logDbgLn 1 $ " [v8d] [foreach] Got element: "++show (k,elm)
  logDbgLn 1 " [v8d] Everything set up; about to quiesce..."
  quiesce hp
--  quiesceAll
  logDbgLn 1 " [v8d] quiesce finished, next freeze::"
  m <- IM.freezeMap m4
  return (L.sort $ F.toList m)
-- [2013.12.13] I've observed this dying at several different points.
-- After the four "got element" messages, or before any of them.


-- | Check for sufficient parallelism in handlers
case_v9a :: Assertion
case_v9a = assertEqual "make sure there's sufficient parallelism" () =<<
  (assertNoTimeOut 1.0 $
   runParNonDet $ ioButNoBump $ do
     mp <- IM.newEmptyMap
     IM.forEach mp $ \ c v -> do
       case c of
         'a' -> do IM.getKey 'b' mp
                   IM.insert 'c' () mp
         'b' -> return ()
         'c' -> return ()
     IM.insert 'a' () mp
     IM.insert 'b' () mp
     IM.getKey 'c' mp
     return ())
-- [2013.12.11] Whoa!  I'm seeing nondeterministic failures on this that make NO
-- sense.  assertNoTimeOut must be busted.  It reports 1-second timeout when the
-- whole process took 10ms.


--------------------------------------------------------------------------------
-- Issue related:
--------------------------------------------------------------------------------

ioButNoBump :: Par ('Ef 'P 'G 'F 'NB 'NI) s a ->
               Par ('Ef 'P 'G 'F 'NB 'NI) s a
ioButNoBump x = x

-- Issue #27, spurious duplication.
case_handlrDup :: Assertion
case_handlrDup = runParNonDet $ ioButNoBump $ do 
  logDbgLn 1 "[case_handlrDup] Step 1"
  ctr <- I.liftIO$ newIORef 0
  mp  <- IM.newEmptyMap
  logDbgLn 1 "[case_handlrDup] Step 2"
  hp  <- newPool
--  ls <- I.liftIO$ IM.levelCounts mp
--  logDbgLn 1 $ "[case_handlrDup] Step 3: check slm counts: "++show ls
  -- Register handler FIRST.. no race.
  IM.forEachHP (Just hp) mp $ \ (k::Int) (v::Int) -> do
    logDbgLn 1 $ "[case_handlrDup] Callback executing: " ++ show (k,v)
    I.liftIO $ incr ctr
--  ls2 <- I.liftIO$ IM.levelCounts mp
--  logDbgLn 1 $ "[case_handlrDup] Step 4: Main thread, done registering callback, slm: "++show ls2
  IM.insert 2 2 mp
  IM.insert 3 3 mp
  quiesce hp
  sum <- I.liftIO $ readIORef ctr
  I.liftIO $ assertEqual "Should be no duplication in this case" 2 sum

incr :: IORef Int -> IO ()
incr ref = atomicModifyIORef' ref (\x -> (x+1,()))

--------------------------------------------------------------------------------
-- Parallel insertion
--------------------------------------------------------------------------------

-- -- | Perform a fork-join computation and populate a SkipListMap in parallel.
fillOne :: (HasPut e, HasGet e) => [(Int, Int)] -> Par e s (TheMap Int s Int)
-- fillOne :: PC.ParMonad p => [(Int, Int)] -> p (TheMap Int Int)
fillOne chunks = do
  mp <- IM.newEmptyMap
  vars <- forM chunks $ \ (start,end) -> do
    iv <- IV.new
    fork $ do
      T.for_ (start, end)$ \n -> IM.insert n n mp
      IV.put iv () -- Mark the chunk as complete.
    return iv
  -- Sequentially block on all of them:
  forM_ vars IV.get
  return mp

insertionTest :: [(Int, Int)] -> IO (Bool, Word64)
insertionTest chunks = do
  mp <- timeit$ runParThenFreezeIO $ isND $ fillOne chunks
  let matches = PC.fold (\b (k::Int,v::Int) -> b && k == v) True mp
--  summed  <- SLM.foldlWithKey id (\s _ v -> return $! s + fromIntegral v) 0 slm
--  return (matches, summed)
  error "FINISHME"


-- -- Concurrent insertion of the same values:
-- slm2 :: IO (Bool, Word64)
-- slm2 = insertionTest (replicate numCapabilities (1,mediumSize))
-- case_slm2 :: Assertion
-- case_slm2 = slm2 >>= assertEqual "test concurrent insertion for SkipListMap (#2)" (True, expectedSum)

-- -- Same, but in the opposite order:
-- -- Takes much longer (in parallel)!! Why?
-- slm3 :: IO (Bool, Word64)
-- slm3 = insertionTest (replicate numCapabilities (mediumSize,1))
-- case_slm3 :: Assertion
-- case_slm3 = slm3 >>= assertEqual "test concurrent insertion for SkipListMap (#3)" (True, expectedSum)

-- slm4 :: IO (Bool, Word64)
-- slm4 = insertionTest (splitRange numCapabilities (1,mediumSize))
-- case_slm4 :: Assertion
-- case_slm4 = slm4 >>= assertEqual "test concurrent insertion for SkipListMap (#4)" (True, expectedSum)



--------------------------------------------------------------------------------
-- Parallel folding
--------------------------------------------------------------------------------

-- case_parfoldslm1 :: Assertion
-- case_parfoldslm1 =
--   assertEqual "test concurrent insertion for SkipListMap (#4)" expectedSum =<<
--     (do slm <- fillOne (splitRange numCapabilities (1,mediumSize))
--         return expectedSum
--     )



--------------------------------------------------------------------------------

-- | Aggregate the included tests with the ones in this file:
tests_common :: TestTree
tests_common = testGroup "Common" [ $(testGroupGenerator) , tests_writeOnly ]
