{-# LANGUAGE CPP #-}
#include "Runner.hs"

-- Plain monad par version... or could do "Eval" monad.

bf_traverse :: Int             -- iteration counter
               -> Graph2       -- graph
               -> IS.IntSet    -- set of "seen" node labels, initially size 0
               -> IS.IntSet    -- set of "new" node labels, initially size 1
               -> WorkFn       -- function to be applied to each node
               -> Par (IS.IntSet)
bf_traverse 0 _ seen_rank new_rank _ = do
  when verbose $ prnt $ "bf_traverse finished! seen/new size: "
    ++ show (IS.size seen_rank, IS.size new_rank)
  return (IS.union seen_rank new_rank)

bf_traverse k !g  !seen_rank !new_rank !f = do 
  when verbose $ prnt  $"bf_traverse call... "
    ++ show k ++ " seen/new size "
    ++ show (IS.size seen_rank, IS.size new_rank)
  -- Nothing in the new_rank set means nothing left to traverse.
  if IS.null new_rank
  then return seen_rank
  else do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' = IS.union seen_rank new_rank
        allNbr'    = IS.fold (\i acc -> IS.union (g V.! i) acc) 
                        IS.empty new_rank
        new_rank'  = IS.difference allNbr' seen_rank' 
    bf_traverse (k-1) g  seen_rank' new_rank' f

start_traverse :: Int       -- iteration counter
                  -> Graph2 -- graph
                  -> Int    -- start node
                  -> WorkFn -- function to be applied to each node
                  -> IO ()
start_traverse k !g startNode f = do
  runParIO $ do        
        prnt $ " * Running on " ++ show numCapabilities ++ " parallel resources..."
        
        -- pass in { startNode } as the initial "new" set
        set <- bf_traverse k g IS.empty (IS.singleton startNode) f
        
        prnt $ " * Done with bf_traverse..."

        resLs <- parMap f (IS.toList set)
        let set2 = Set.fromList resLs

        prnt $ " * Done parMap(f)..."
        prnt $ "  * Set size: " ++ show (Set.size set2)
        prnt $ "  * Set sum: " ++ show (Set.fold (\(x,_) y -> x+y) 0 set2)




--------------------------------------------------------------------------------
-- bf_pure :: Int             -- iteration counter
--                -> Graph2       -- graph
--                -> IS.IntSet    -- set of "seen" node labels, initially size 0
--                -> IS.IntSet    -- set of "new" node labels, initially size 1
--                -> WorkFn       -- function to be applied to each node
--                -> IS.IntSet
-- bf_pure 0 _ seen_rank new_rank _ = do
--   -- when verbose $ prnt $ "bf_pure finished! seen/new size: "
--   --   ++ show (IS.size seen_rank, IS.size new_rank)
--   (IS.union seen_rank new_rank)

-- bf_pure k !g  !seen_rank !new_rank !f = do 
--   -- when verbose $ prnt  $"bf_traverse call... "
--   --   ++ show k ++ " seen/new size "
--   --   ++ show (IS.size seen_rank, IS.size new_rank)
--   if IS.null new_rank
--   then seen_rank
--   else do
--     -- Add new_rank stuff to the "seen" list
--     let seen_rank' = IS.union seen_rank new_rank
-- -- TODO: parMap version
-- --        allNbr     = IS.fold IS.union                      
--         allNbr'    = IS.fold (\i acc -> IS.union (g V.! i) acc) 
--                         IS.empty new_rank
--         new_rank'  = IS.difference allNbr' seen_rank' 
--     bf_pure (k-1) g seen_rank' new_rank' f


-- start_traverse :: Int       -- iteration counter
--                   -> Graph2 -- graph
--                   -> Int    -- start node
--                   -> WorkFn -- function to be applied to each node
--                   -> IO ()
-- start_traverse k !g startNode f = do
--   do        
--     putStrLn $ " * Running on " ++ show numCapabilities ++ " parallel resources..."
--     let set = bf_pure k g IS.empty (IS.singleton startNode) f
-- --        set2 = Set.fromList$ Strat.parMap Strat.rdeepseq f (IS.toList set)
--         set2 = Set.fromList$ Strat.parMap Strat.rwhnf f (IS.toList set)
-- --        set2 = Set.fromList (map f (IS.toList set))
--         size = Set.size set2
--     t0 <- getCurrentTime    
--     evaluate set
--     t1 <- getCurrentTime     
--     putStrLn $ " * Bf_pure result in WHNF (done with bf_pure?) "++show(diffUTCTime t1 t0)
--     putStr$ "  * Set size: " ++ show size ++ " "
--     t2 <- getCurrentTime     
--     print (diffUTCTime t2 t1)

--     evaluate set2
--     let ls = Set.toList set2
--     putStrLn$ " * first element of result: "++show (head ls)
--     putStrLn$ " * first 10 elements of result: "++show (take 10 ls)
--     t3 <- getCurrentTime  
--     putStrLn$ " * Analyze function should be finished! "++show(diffUTCTime t3 t2)

-- --    putStrLn$ " * Full set2 "++show set2
--     putStrLn$ " * Set sum: " ++ show (Set.fold (\(_,x) y -> x+y) 0 set2)
