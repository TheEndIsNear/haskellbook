data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then putStrLn "yup OK!"
  else putStrLn "test failed"


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =  (:) a (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Band news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Band news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Band news bears."

foldrTree ::  (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree _ initial Leaf = initial
foldrTree f initial (Node left a right) = foldrTree f (f a (foldrTree f initial right)) left

foldlTree ::  (a -> b -> b) -> b -> BinaryTree a -> b
foldlTree _ initial Leaf = initial
foldlTree f initial (Node left a right) = foldlTree f (f a (foldlTree f initial left)) right

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
