-- len :: [a] -> Int
-- len [] = 0
-- len [x] = 1
-- len (x : xs) = 1 + len xs

-- myreverse :: [a] -> [a]
-- myreverse [] = []
-- myreverse [x] = [x]
-- myreverse (x : xs) = myreverse xs ++ [x]

-- sum1 :: (Int, Int) -> Int
-- sum1 (x, y) = x + y

-- sum' :: Int -> Int -> Int
-- sum' x = (\y -> x + y)

-- main = do
--   print $ myreverse [1, 2, 3, 4, 5, 6, 7, 8]

-- data Color = Red | Green | Blue

-- c :: Color
-- c = Red

-- colorName :: Color -> [Char]
-- colorName Red = "red"
-- colorName Blue = "blue"
-- colorName Green = "green"

-- data LinkedList = None | Node (Int, LinkedList)

-- llist = Node (7, Node (8, Node (8, None)))

-- llistLength None = 0
-- llistLength (Node (_, next)) = 1 + llistLength next

numbers3 = [1 ..]

main = do
  print $ take 101 numbers3