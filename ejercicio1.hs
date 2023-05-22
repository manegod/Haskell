data Color = Rojo | Amarillo | Azul | Verde
        deriving (Show, Eq)
        
data Forma = Triangulo | Cuadrado | Rombo | Circulo
        deriving (Show, Eq)
        
type Figura = (Forma, Color, Int)

azul :: Figura -> Bool
azul (f, c, t) = c == Azul

