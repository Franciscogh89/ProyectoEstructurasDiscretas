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
                    show (Not p) = "(~ " ++ show p ++ ")"
                    show (Or p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


-- =============================================================
-- ARBOLES DE PRUEBA
t0 :: ArbolN Int
t0 = Void

t1 :: ArbolN Int
t1 = Node 1 []

t2 :: ArbolN Int
t2 = Node 1 [Node 2 [], Node 3 []]

t3 :: ArbolN Int
t3 = Node 10
        [ Node 5  []
        , Node 7  [Node 8 [], Node 9 []]
        , Node 12 []
        ]

t4 :: ArbolN Char
t4 = Node 'a'
        [ Node 'b'
            [ Node 'c'
                [ Node 'd' [], Node 'e' [] ]
            ]
        , Node 'f' []
        ]

t5 :: ArbolN String
t5 = Node "root"
        [ Node "a" []
        , Node "b" []
        , Node "c" []
        , Node "d" []
        , Node "e" []
        ]

t6 :: ArbolN Int
t6 = Node 1
        [ Void
        , Node 2 [Void, Node 3 []]
        , Void
        ]

t7 :: ArbolN Int
t7 = Node 1
        [ Node 2
            [ Node 4 []
            , Node 5
                [Node 8 [], Node 9 []]
            ]
        , Node 3
            [ Node 6 []
            , Node 7
                [ Node 10 [], Node 11 [], Node 12 [] ]
            ]
        ]

t8 :: ArbolN Int
t8 = Node 1
        [ Node 1 []
        , Node 2 [Node 1 [], Node 3 []]
        ]
-- ================================================================


crearArbol :: a -> [ArbolN a] -> ArbolN a
crearArbol x [] = Node x []
crearArbol x lr = Node x lr

formulatoArbol :: Prop -> ArbolN String
formulatoArbol (Var p) = crearArbol p []
formulatoArbol (Not p) = crearArbol "~" [formulatoArbol p]
formulatoArbol (And p q) = crearArbol "^" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Or p q) = crearArbol "v" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Impl p q) = crearArbol "->" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Syss p q) = crearArbol "<->" [formulatoArbol p , formulatoArbol q]
formulatoArbol (Cons True) = crearArbol "Verdadero" []
formulatoArbol (Cons False) = crearArbol "Falso" []

arbolaformula :: ArbolN String -> Prop
arbolaformula Void = error "El arbol no puede ser vacio"
arbolaformula (Node "~" [l]) = Not (arbolaformula l)
arbolaformula (Node "^" [l,r]) = And (arbolaformula l) (arbolaformula r)
arbolaformula (Node "v" [l,r]) = Or (arbolaformula l) (arbolaformula r)
arbolaformula (Node "->" [l,r]) = Impl (arbolaformula l) (arbolaformula r)
arbolaformula (Node "<->" [l,r]) = Syss (arbolaformula l) (arbolaformula r)
arbolaformula (Node "Verdadero" []) = Cons True
arbolaformula (Node "Falso" []) = Cons False
arbolaformula (Node x []) = Var x       


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



-- SECCION OTRAS FUNCIONES


-- EJERCICIO 1: Para revisar cuántos elementos tiene un árbol n-ario
numeroElementos :: ArbolN a -> Int
numeroElementos Void = 0
numeroElementos (Node _ hijos) = 1 + sumar (mapping numeroElementos hijos)

-- EJERCICIO 2: Buscar elementos en un árbol n-ario.
busca :: Eq a => ArbolN a -> a -> Bool
busca Void _ = False
busca (Node x hijos) y = x == y || alguno (\hijo -> busca hijo y) hijos

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



-- Ejercicio 5
altura :: ArbolN a -> Int
altura Void = 0
altura (Node _ []) = 1
altura (Node _ xs) = 1 + (encontrarMayor (mapping altura xs))


-- Ejercicio 6
espejo :: ArbolN a -> ArbolN a
espejo Void = Void
espejo (Node x []) = Node x []
espejo (Node y (xs)) = Node y (reversa(mapping espejo (xs)))


-- Ejercicio 7
podar :: Int -> ArbolN a -> ArbolN a
podar _ Void = Void
podar y (Node x []) = if (y == 0) then Void else (Node x [])
podar y (Node z (x:xs)) = if (y == 0) then Void else (Node z (mapping (podar (y - 1)) (x:xs)))


-- Ejercicio 8
elementosProfundidad :: Int -> ArbolN a -> [a]
elementosProfundidad _ Void = []
elementosProfundidad n (Node y []) = if (n == 0) then [y] else []
elementosProfundidad n (Node y xs) = if (n == 0) then [y] else desenvolver (mapping (elementosProfundidad (n - 1)) (xs))



-- FUNCIONES AUXILIARES

-- La funcion mapping convierte una lista de "a" a una lista de "b" de
-- acuerdo a la funcion dada
-- La funcion es usada en todo (Ejercicios 5, 6, 7, 8)
mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (x:xs) = (f x):(mapping f xs)


-- La funcion reverse invierte el orden de los elementos de una lista
-- La funcion es usada en espejo
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]


-- La funcion encontrarMayor regresa el numero mayor en una lista de Int
-- La funcion es usada en altura
encontrarMayor :: [Int] -> Int
encontrarMayor [] = 0
encontrarMayor (x:xs) = auxEnMayor x xs

-- Auxiliar de encontrarMayor
auxEnMayor :: Int -> [Int] -> Int
auxEnMayor x [] = x
auxEnMayor x (y:ys) = if (x > y) then (auxEnMayor x ys) else (auxEnMayor y ys)


---- La funcion length regresa el numero de elementos en una lista
---- La funcion es usada en elementosProfundidad
--contar :: [a] -> Int
--contar [] = 0
--contar (_:xs) = 1 + contar xs


-- La funcion desenvolver toma una lista de listas de algun tipo y devuelve una lista
-- con todos los elementos de las listas
-- La funcion es usada en elementosProfundidad
desenvolver :: [[a]] -> [a]
desenvolver [] = []
desenvolver (x:xs) = x ++ (desenvolver xs)


-- Función alguno: verifica si algún elemento de la lista cumple con el predicado
alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False
alguno f (x:xs) = f x || alguno f xs

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