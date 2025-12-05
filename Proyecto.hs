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




-- SECCION OTRAS FUNCIONES

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

                          -----------------  DUDAS  -------------------
podar :: Int -> ArbolN a -> ArbolN a
podar _ Void = Void
podar y (Node x []) = if (y == 0) then Void else (Node x [])
podar y (Node z (x:xs)) = if (y == 0) then Void else (Node z (mapping (podar (y - 1)) (x:xs)))

-- Ejercicio 8
elementosProfundidad :: Int -> ArbolN a -> [a]
elementosProfundidad _ Void = []
elementosProfundidad n (Node y []) = if (n == 0) then [y] else []
elementosProfundidad n (Node y xs) = if (n == 0) then [y] else desenvolver (mapping (elementosProfundidad (n - 1)) (xs))


-- ======================== Auxiliar ===========================
desenvolver :: [[a]] -> [a]
desenvolver [] = []
desenvolver (x:xs) = x ++ (desenvolver xs)



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
-- La funcion es usada en 
encontrarMayor :: [Int] -> Int
encontrarMayor [] = 0
encontrarMayor (x:xs) = auxEnMayor x xs


auxEnMayor :: Int -> [Int] -> Int
auxEnMayor x [] = x
-- ================== DUDA ====================================================
auxEnMayor x (y:ys) = if (x > y) then (auxEnMayor x ys) else (auxEnMayor y ys)


---- La funcion length regresa el numero de elementos en una lista
---- La funcion es usada en elementosProfundidad
--contar :: [a] -> Int
--contar [] = 0
--contar (_:xs) = 1 + contar xs

