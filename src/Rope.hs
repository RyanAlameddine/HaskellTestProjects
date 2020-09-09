module Rope where
--Rope data structure

data Rope = 
      Node { left::Rope, right::Rope, weight::Int} 
    | Leaf { text::String, weight::Int}
    deriving (Show, Read, Eq)  

index :: Int -> Rope -> Char
index = recurseToIndex (\ i r -> text r !! i)

concatenate :: Rope -> Rope -> Rope
concatenate l r = Node l r (weightOf l)

--this doesnt work yet
split :: Int -> Rope -> (Rope, Rope)
split i (Node l r w)
    | w > i      = split i l
    | otherwise  = split (i - w) r 
split i leaf = (leaf, leaf)

insert :: Int -> Rope -> String -> Rope
insert i rope t = concatenate l $ concatenate (Leaf t $ length t) r
    where
        (l, r) = split i rope

delete :: Int -> Int -> Rope -> Rope
delete i j r = concatenate left right
    where
        (_, right) = split (i + j) r
        (left, _)  = split i r

unRope :: Rope -> String
unRope (Leaf text _) = text
unRope (Node l r _)  = unRope l ++ unRope r

--private methods
recurseToIndex :: (Int -> Rope -> a) -> Int -> Rope -> a
recurseToIndex f i (Node l r w)
    | w > i      = recurseToIndex f i l
    | otherwise  = recurseToIndex f (i - w) r 
recurseToIndex f i leaf = f i leaf

weightOf :: Rope -> Int
weightOf (Leaf _ w) = w
weightOf (Node _ r w) = w + weightOf r