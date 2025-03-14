duplica :: Int -> Int
duplica x = (2*x)


mas1 :: Int -> Int
mas1 x = x+1


esPar:: Int -> Bool

esPar x| mod x 2 == 0 = True 
       | mod x 2 /= 0 = False 
                 
generalMap:: [Int] -> (Int -> Int) ->  [Int]
generalMap [] f = []
generalMap (x:xs) f = f x : generalMap xs f


morePolGeneralMap :: [a] -> (a -> b) -> [b]
morePolGeneralMap [] f = []
morePolGeneralMap (x:xs) f = f x : morePolGeneralMap xs f

--Ejercicio 2
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x : soloPares xs
                 | mod x 2 /= 0 = soloPares xs
