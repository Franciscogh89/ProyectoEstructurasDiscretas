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






