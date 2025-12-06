module LabDis where

---------------------------------------------------------------------------------------------------------------------


data ArbolN a = Void | Node a [ArbolN a]
                deriving (Show, Eq) 

data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "(¬ " ++ show p ++ ")"
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

crearArbol :: a -> [ArbolN a] -> ArbolN a
crearArbol x [] = Node x []
crearArbol x lr = Node x lr

formulatoArbol :: Prop -> ArbolN String
formulatoArbol (Var p) = crearArbol p []
formulatoArbol (Not p) = crearArbol "¬" [formulatoArbol p]
formulatoArbol (And p q) = crearArbol "∧" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Or p q) = crearArbol "∨" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Impl p q) = crearArbol "→" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Syss p q) = crearArbol "↔" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Cons True) = crearArbol "Verdadero" []
formulatoArbol (Cons False) = crearArbol "Falso" []

arbolaformula :: ArbolN String -> Prop
arbolaformula Void = error "El arbol no puede ser vacio"
arbolaformula (Node "¬" [l]) = Not (arbolaformula l)
arbolaformula (Node "∧" [l,r]) = And (arbolaformula l) (arbolaformula r)
arbolaformula (Node "∨" [l,r]) = Or (arbolaformula l) (arbolaformula r)
arbolaformula (Node "→" [l,r]) = Impl (arbolaformula l) (arbolaformula r)
arbolaformula (Node "↔" [l,r]) = Syss (arbolaformula l) (arbolaformula r)
arbolaformula (Node "Verdadero" []) = Cons True
arbolaformula (Node "Falso" []) = Cons False
arbolaformula (Node x []) = Var x       



----------------------------------------------------------------------------------------------------------------------------


-- EJERCICIO 3, SECCION: ARBOLES DE SINTAXIS ABSTRACTA

-- Función auxiliar para buscar una variable en el estado
buscarVariable :: String -> [(String, Bool)] -> Bool
buscarVariable var [] = error ("Variable '" ++ var ++ "' no encontrada en el estado")
buscarVariable var ((nombre, valor):resto)
    | var == nombre = valor
    | otherwise = buscarVariable var resto

-- Función que evalúa un árbol de sintaxis abstracta dado un estado de variables
-- El estado se representa como una lista de tuplas (nombre de variable, valor)
evaluarArbol :: ArbolN String -> [(String, Bool)] -> Bool
evaluarArbol Void _ = error "No se puede evaluar un árbol vacío"
evaluarArbol (Node "¬" [hijo]) estado = not (evaluarArbol hijo estado)
evaluarArbol (Node "∧" [izq, der]) estado = evaluarArbol izq estado && evaluarArbol der estado
evaluarArbol (Node "∨" [izq, der]) estado = evaluarArbol izq estado || evaluarArbol der estado
evaluarArbol (Node "→" [izq, der]) estado = not (evaluarArbol izq estado) || evaluarArbol der estado
evaluarArbol (Node "↔" [izq, der]) estado = evaluarArbol izq estado == evaluarArbol der estado
evaluarArbol (Node "Verdadero" []) _ = True
evaluarArbol (Node "Falso" []) _ = False
evaluarArbol (Node var []) estado = buscarVariable var estado
evaluarArbol (Node x _) _ = error ("Operador desconocido: " ++ x)


-- SECCION: OTRAS FUNCIONES

-- La funcion mapping convierte una lista de "a" a una lista de "b" de
-- acuerdo a la funcion dada
-- La funcion es usada en todo (Ejercicios 5, 6, 7, 8)
mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (x:xs) = (f x):(mapping f xs)

-- Función auxiliar sumar: suma todos los elementos de una lista numérica
sumar :: Num a => [a] -> a
sumar [] = 0
sumar (x:xs) = x + sumar xs

-- Función auxiliar concatenar (necesaria para concatenarMapping)
concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- Función auxiliar concatenarMapping
concatenarMapping :: (a -> [b]) -> [a] -> [b]
concatenarMapping f xs = concatenar (mapping f xs)

-- EJERCICIO 1: Para revisar cuántos elementos tiene un árbol n-ario
numeroElementos :: ArbolN a -> Int
numeroElementos Void = 0
numeroElementos (Node _ hijos) = 1 + sumar (mapping numeroElementos hijos)

-- EJERCICIO 2: Buscar elementos en un árbol n-ario.
busca :: Eq a => ArbolN a -> a -> Bool
busca Void _ = False
busca (Node x hijos) y = x == y || any (\hijo -> busca hijo y) hijos

-- EJERCICIO 3: Sumar los elementos del árbol con tipos numéricos
sumaElementos :: Num a => ArbolN a -> Int
sumaElementos Void = 0
sumaElementos (Node _ hijos) = 1 + sumar (mapping sumaElementos hijos)

-- EJERCICIO 4: Funciones preorden y postorden.
-- Recorrido en preorden (raíz -> hijos de izquierda a derecha)
preorden :: ArbolN a -> [a]
preorden Void = []
preorden (Node x hijos) = [x] ++ concatenarMapping preorden hijos

-- Recorrido en postorden (hijos de izquierda a derecha -> raíz)
postorden :: ArbolN a -> [a]
postorden Void = []
postorden (Node x hijos) = concatenarMapping postorden hijos ++ [x]











