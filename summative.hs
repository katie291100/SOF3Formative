data TreeP a = Leaf Int | Node (TreeP a) a (TreeP a) deriving (Eq, Show)

emptyTreeP :: TreeP a
emptyTreeP = Leaf 0

itP :: Ord a => a -> TreeP a -> TreeP a
itP val (Node left a right) | a > val = (Node left a (itP val right))
                       | val > a =  (Node (itP val left) a right)
                       | a == val = (Node left a right)
itP val (Leaf a) | a > val = (Node (Leaf val) a emptyTreeP)
                 | val > a = (Node emptyTreeP a (Leaf val))
