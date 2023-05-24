data Color = Rojo | Amarillo | Azul | Verde
       deriving (Show, Eq)

data Forma = Triangulo | Cuadrado | Rombo | Circulo
      deriving (Show, Eq)

type Figura = (Forma, Color, Int)

tam :: Figura -> Int

tam (f, c, t) = t

rojo :: Figura -> Bool
rojo (f ,c ,t) = c == Rojo

azul :: Figura -> Bool
azul (f,c,t) = c == Azul

verde :: Figura -> Bool
verde (f,c,t) = c == Verde

amarillo :: Figura -> Bool
amarillo (f,c,t) = c == Amarillo

circulo :: Figura -> Bool
circulo (f,c,t) = f == Circulo

triangulo :: Figura -> Bool
triangulo (f,c,t) = f == Triangulo

rombo :: Figura -> Bool
rombo (f,c,t) = f == Rombo

cuadrado:: Figura -> Bool
cuadrado (f,c,t) = f == Cuadrado

propA ::[Figura] -> Bool

propA [] = True
propA (x:xs) = rojo x && propA xs

propB :: [Figura] -> Bool
propB [] = True
propB (x:xs) =  tam x<5 && propB xs

propC :: [Figura] -> Bool
propC [] = True
propC (x:xs) | triangulo x = rojo x && propC xs
             | not (triangulo x) = propC xs

propD :: [Figura] -> Bool
propD [] = False
propD (x:xs) | cuadrado x = verde x || propD xs
             | not (cuadrado x)= propD xs
             
propE :: [Figura] -> Bool
propE [] = True
propE (x:xs) | circulo x = azul x && tam x<10 && propE xs
             | not (circulo x) = propE xs
          
propF :: [Figura] -> Bool
propF [] = True
propF (x:xs) | triangulo x && azul x = False
             |not (triangulo x && azul x) = propF xs
             
propG :: [Figura] -> Bool
propG [] = True
propG (x:xs) |(circulo x && amarillo x) || (circulo x && verde x) = False
             | otherwise = propG xs
             
propH :: [Figura] -> Bool
propH [] = False
propH (x:xs) | cuadrado x = tam x<5 || propH xs
             | not (cuadrado x) = propH xs
             
propI :: [Figura] -> Bool
propI [] = False
propI (x:xs) = not (circulo x && rojo x) ||(cuadrado x && rojo x) || propI xs
