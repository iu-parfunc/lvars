
-- A file where I make an attempt at testing code.

import Test.HUnit

foo x = (1,x)

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (assertEqual "factorial 5" 120 120)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

-- To run this code simply type "ghci HelloTesting.hs" and then at the
-- prompt "runTestTT tests".
