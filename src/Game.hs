module Game where
  cls :: IO()
  cls  = putStr "\ESC[2J"

  type Pos = (Int,Int)

  writeat :: Pos -> String -> IO()
  writeat p xs = do goto p
                    putStr xs

  goto :: Pos -> IO()
  goto(x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

  width :: Int
  width = 150
  height :: Int
  height = 150
  type Board = [Pos]
  glider :: Int -> Board
  glider n = [(4,2),(2,3),(4,3),(3,4),(4,4)]
  coolPattern1 :: Board
  coolPattern2 ::(Int,Int) -> Board
  coolPattern2 (start,end) = [(x,y)| x <- [start..end], y <- [start..end]]
  primes :: (Int,Int) -> Board
  power :: Int -> (Int, Int) -> Board
  fibNumbers :: Int ->  Board
  fibNumbers n  = [(x,y) | x <- fibs , y <- [0..n], x /= y]  
      where fib 0 = 1
            fib 1 = 1
            fib x = fib (x - 1) + fib (x - 2) 
            fibs = map fib [0..n]
  power ind (start, end) = [(x,y)| x <- square_set , y <- [start..end]]
          where
            square_set = map f [start..end]
            f x = x ^ ind
            
  primes (start,end) = [(x,y)| x <- [start..end], y <- [start..end], isPrime x]
      where factors z = [f | f <- [1..z], z `mod` f == 0]
            isPrime :: Int -> Bool
            isPrime p = factors p == [1,p]
  coolPattern1 = [(10,5),(10,6),(10,7),(10,8),(10,9),(13,5),(13,6),(13,7),(13,8),(13,9),(13,5),(13,7),(13,9)]
  showcells :: Board -> IO()
  showcells b = sequence_ [writeat p "0" | p <- b]
  isAlive :: Board -> Pos -> Bool
  isAlive b p = p `elem` b

  isEmpty :: Board -> Pos -> Bool
  isEmpty b p = not (isAlive b p)

  neighbs :: Pos -> [Pos]
  neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
  wrap :: Pos -> Pos
  wrap (x,y) = (((x-1) `mod` width) + 1,
               ((y-1) `mod` height) +1)
  liveneighbs :: Board -> Pos -> Int
  liveneighbs b = length . filter (isAlive b) . neighbs
  survivors :: Board -> [Pos]
  survivors b = [p | p <- b, liveneighbs b p `elem` [2,3]]
  births :: Board -> [Pos]
  births b = [p| p <-rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p == 3]
  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : rmdups(filter (/= x) xs)
  nextgen :: Board -> Board
  nextgen b = survivors b ++ births b
  life :: Board -> IO()
  life b = do cls
              showcells b
              wait 500000
              life(nextgen b)
  wait :: Int -> IO()
  wait n = sequence_ [return() | _ <- [1..n]]

