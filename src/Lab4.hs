--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Lab 4: Custom types                                                        --
--------------------------------------------------------------------------------

module Lab4 where
import Data.List(elemIndex)

--------------------------------------------------------------------------------

-- From lectures. Just and Nothing are in prelude.
data MaybeInt a = Nd | Jst a deriving Show

safediv :: Integer -> Integer -> MaybeInt Double
safediv x 0 = Nd
safediv x y = Jst(fromIntegral x / fromIntegral y)

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)


-- Red-black trees

-- These type declarations are unnecesary but help me
-- understand what data types and the are.
--Red :: Colour | Black :: Colour
data Colour = Red | Black

-- These are pattern matching implementations of show/Eq instances for Colour.
-- It can also be done using a deriving statement as done with Tree a.
instance Show Colour where
    show Red   = "Red"
    show Black = "Black"

-- See above comment.
instance Eq Colour where
    Red   == Red   = True
    Black == Black = True
    Red   == Black = False
    Black == Red   = False

-- Type declaration not needed. Leaf and Node are type constructors.
-- Leaf :: Tree a | Node :: Colour -> Tree a -> a -> Tree a -> Tree a
data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Node Red Leaf a Leaf

makeBlack :: Tree a -> Tree a
makeBlack (Node _ l x r) = (Node Black l x r)
makeBlack tree           = tree --why is this line necessary?

depth :: Tree a -> Int
depth Leaf           = 0
depth (Node _ l _ r) = 1 + max (depth l) (depth r)

toList :: Tree a -> [a]
toList Leaf           = []
toList (Node _ l a r) = toList l ++ [a] ++ toList r
    -- inorder, so recursively traverse the left tree, visit the middle node,
    -- recursively traverse the right node tree.

member :: Ord a => a -> Tree a -> Bool
member x (Node _ l a r)
    | x == a          = True
    | x > a           = member x r
    |otherwise        = member x l

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d = -- z y x (left left)
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = -- z x y (left right)
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = -- x y z (right right)
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = -- x z y (right left)
    Node Red (Node Black a x b) y (Node Black c z d)
balance c l x r = Node c l x r

-- makeBlack is only called once. insert2 is the auxiliary function
-- which does the actual insertion.
insert  :: Ord a => Tree a -> a -> Tree a
insert tree a = makeBlack (insert2 tree a)

-- insert2 uses balance to balance the tree when necessary i.e. when
-- the tree matches with any of the four cases defined in balance.
insert2 :: Ord a => Tree a -> a -> Tree a
insert2 Leaf a          = singleton a
insert2 (Node c l x r) a
    | a > x             = balance c l x (insert2 r a)
    | a == x            = Node c l x r
    | otherwise         = balance c (insert2 l a) x r






--------------------------------------------------------------------------------
