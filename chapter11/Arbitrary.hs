import Test.QuickCheck

data Ternary = Yes 
 | No 
 | Unknown 
 deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

--instance Arbitrary Ternary where
--  arbitrary     = do
--      n <- choose (0, 2) :: Gen Int
--      return $ case n of
--                    0 -> Yes
--                    1 -> No
--                    _ -> Unknown

{- already defined in QuickCheck 
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where 
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (x,y)

instance Arbitrary Char where
    arbitrary = elements (['a'..'z'] ++ ['A'..'Z'] ++ " ~!@#$%^&*()")
-}