--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Lab 4: Custom types                                                        --
--------------------------------------------------------------------------------

module Lab4 where
import Data.List((++), elemIndex)

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

-- This type declaration does not mean anything, it helps me
-- understand what data types are.
--Red :: Colour and Black :: Colour
data Colour = Red | Black

instance Show Colour where
    show Red   = "Red"
    show Black = "Black"

instance Eq Colour where
    Red == Red     = True
    Black == Black = True

--Type declaration not needed. Node is the type constructor (?)
--Tree :: Leaf | Node -> Colour -> Tree
data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

instance Eq (Tree a) where
    Leaf == Leaf                             = True
    -- (Node c1 l1 a1 r1) == (Node c2 l2 a2 r2) =
    --     ((c1 == c2) && (l1 == l2) && (a1 == a2)  && (r1 == r2))

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Node Red empty a empty

makeBlack :: Tree a -> Tree a
makeBlack (Node c l x r) = (Node Black l x r)

depth :: Tree a -> Int
depth Leaf           = 0
depth (Node c l a r) = 1 + max (depth l) (depth r)

toList :: Tree a -> [a]
toList Leaf           = []
toList (Node c l a r) =
    toList l ++ [a] ++ toList r
    -- inorder, so recursively traverse the left tree, visit the middle node,
    -- recursively traverse the right node tree.

member :: Ord a => a -> Tree a -> Bool
member a tree = not ( (elemIndex a (toList tree)) == Nothing )

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

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf a           = singleton a
insert (Node c l x r) a
    | a > x             = f (g (tree1 c l x r a))
    | a == x            = Node c l x r
    | otherwise         = f (g (tree2 c l x r a))
    where
        tree1 c l x r a = Node c l x (insertnb r a) -- Insert into the right
        tree2 c l x r a = Node c (insertnb l a) x r -- Insert into the left
        insertnb Leaf aa -- A "non makeBlack" version of insert
                        = insert Leaf aa
        insertnb (Node cc le xx ri) aa
            | aa > xx   = (tree1 cc le xx ri aa)
            | aa == xx  = Node cc le xx ri
            | otherwise = (tree2 cc le xx ri aa)
        g (Node c l x r)
                        = balance c l x r
        f y             = makeBlack y




--------------------------------------------------------------------------------
