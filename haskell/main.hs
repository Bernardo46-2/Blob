import qualified BinaryTree

main :: IO()
main = do
    let tree = BinaryTree.fromList [4, 2, 5, 1, 6, 8]
    print $ BinaryTree.preOrder id tree
    print $ BinaryTree.inOrder id tree
    print $ BinaryTree.posOrder id tree
    print $ BinaryTree.contains 8 tree
    print $ BinaryTree.preOrder id $ BinaryTree.pop 1 tree
    print $ BinaryTree.preOrder id $ BinaryTree.pop 2 tree
    print $ BinaryTree.preOrder id $ BinaryTree.pop 3 tree
    print $ BinaryTree.size tree
    print $ BinaryTree.height tree
    print $ BinaryTree.toList tree
    print $ (*4) <$> tree
