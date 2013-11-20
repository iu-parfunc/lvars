
-- NOT a module... this is included into other files:
--------------------------------------------------------------------------------

type ParVec2T s1 elt1 elt2 parM ans =
     ParST (STTup2 (FLIPTY elt1) (FLIPTY elt2) s1) parM ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
{-# INLINE runParVec2T #-}
runParVec2T :: forall elt1 elt2 parM ans .
               (CONSTRAINT(elt1)) => (CONSTRAINT(elt2)) => (ParThreadSafe parM) =>
             (Int,Int)
             -> (forall s1 . ParVec2T s1 elt1 elt2 parM ans)
             -> parM ans
runParVec2T (size1,size2) comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec1 <- liftST $ MU.new size1
    vec2 <- liftST $ MU.new size2
    S.put (STTup2 (FLPIT vec1) (FLPIT vec2))
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
{-# INLINE reify #-}
reify :: ParThreadSafe parM => ParVec2T s1 elt1 elt2 parM (MU.MVector s1 elt1, MU.MVector s1 elt2)
reify = do
  (STTup2 (FLPIT vec1) (FLPIT vec2)) <- S.get
  return (vec1,vec2)

--------------------------------------------------------------------------------

-- | Swap the two state vectors.
{-# INLINE swapState #-}
swapState :: ParThreadSafe parM => ParVec2T s elt elt parM ()
swapState = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put$ STTup2 (FLPIT vecR) (FLPIT vecL)

-- | Write to the (implicit) left vector state.
{-# INLINE writeL #-}
writeL :: ParThreadSafe parM => Int -> eltL -> ParVec2T s eltL eltR parM ()
writeL ind val = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST$ MU.write vecL ind val

-- | Read the (implicit) left vector state.
{-# INLINE readL #-}
readL :: ParThreadSafe parM => Int -> ParVec2T s eltL eltR parM eltL
readL ind = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST$ MU.read vecL ind 

-- | Return the length of the (implicit) left vector state.
{-# INLINE lengthL #-}
lengthL :: ParThreadSafe parM => ParVec2T s1 eltL eltR parM Int
lengthL = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  return $ MU.length vecL

-- | Update the left vector state by swapping two elements.
{-# INLINE swapL #-}
swapL :: ParThreadSafe parM => Int -> Int -> ParVec2T s1 eltL eltR parM ()
swapL x y = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get  
  liftST$ MU.swap vecL x y

-- | Update the left vector state by dropping the first @n@ elements.
{-# INLINE dropL #-}
dropL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
dropL n = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put$ STTup2 (FLPIT (MU.drop n vecL)) (FLPIT vecR)

-- | Update the left vector state by taking the first @n@ elements, discarding the rest.
{-# INLINE takeL #-}
takeL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
takeL n = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put$ STTup2 (FLPIT (MU.take n vecL)) (FLPIT vecR)

-- | Destructively replace the left vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
{-# INLINE growL #-}
growL :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
growL n = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  vecL2 <- liftST$ MU.grow vecL n
  S.put$ STTup2 (FLPIT vecL2) (FLPIT vecR)

-- | Mutate all the elements of the left vector, setting them to the
-- given value.
{-# INLINE setL #-}
setL :: ParThreadSafe parM => eltL -> ParVec2T s1 eltL eltR parM ()
setL val = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST $ MU.set vecL val

-- Helpers for the other vector in the state.

-- | Write to the (implicit) right vector state.
{-# INLINE writeR #-}
writeR :: ParThreadSafe parM => Int -> eltR -> ParVec2T s eltL eltR parM ()
writeR ind val = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST$ MU.write vecR ind val

-- | Read the (implicit) right vector state.
{-# INLINE readR #-}
readR :: ParThreadSafe parM => Int -> ParVec2T s eltL eltR parM eltR
readR ind = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST$ MU.read vecR ind 

-- | Return the length of the (implicit) right vector state.
{-# INLINE lengthR #-}
lengthR :: ParThreadSafe parM => ParVec2T s1 eltL eltR parM Int
lengthR = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  return $ MU.length vecR

-- | Update the vector state by swapping two elements.
{-# INLINE swapR #-}
swapR :: ParThreadSafe parM => Int -> Int -> ParVec2T s1 eltL eltR parM ()
swapR x y = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get  
  liftST$ MU.swap vecR x y

-- | Update the right vector state by dropping the first @n@ elements.
{-# INLINE dropR #-}
dropR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
dropR n = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put$ STTup2 (FLPIT vecL) (FLPIT (MU.drop n vecR))

-- | Update the right vector state by taking the first @n@ elements,
-- discarding the rest.
{-# INLINE takeR #-}
takeR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
takeR n = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put$ STTup2 (FLPIT vecL) (FLPIT (MU.take n vecR))


-- | Destructively replace the right vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
{-# INLINE growR #-}
growR :: ParThreadSafe parM => Int -> ParVec2T s1 eltL eltR parM ()
growR n = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  vecR2 <- liftST$ MU.grow vecR n
  S.put$ STTup2 (FLPIT vecL) (FLPIT vecR2)

-- | Mutate all the elements of the right vector, setting them to the
-- given value.
{-# INLINE setR #-}
setR :: ParThreadSafe parM => eltR -> ParVec2T s1 eltL eltR parM ()
setR val = do 
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  liftST $ MU.set vecR val


