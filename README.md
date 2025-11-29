# ProyectoEstructurasDiscretas

## Integrantes

En esta sección deben eliminar esta línea de texto, borrar la leyenda "Integrante n" y escribir su nombre empezando por apellidos y su número de cuenta.

    +Gonzalez Huerta Francisco
        -No. de Cuenta: 321491163
    +Mendoza Aragón Edith Alejandra
        -No. de Cuenta: 323238702
    +Martinez Ortiz Diego
        -No. de Cuenta: 323019675

## Ejercicios

-  Explicando la implementacion del tipo de dato
Usamos "Void" para poder representar los arboles vacios. Despues, para representar los arboles con n ramas agregamos "Node a" para indicar que el primer elemento sera el nodo (raiz) y despues agregamos [ArbolesN a] para representar que el resto sera una lista de ArbolesN. Usando "Node a [ArbolesN a]" poder implementar claramentre una raiz y podemos decir que sus ramas sera una lista, asi no nos preocupamos de tener que definir un numero exacto de ramas.

- Ejemplo para probar funcione formulatoArbol:
formulatoArbol (Impl (And (Var "p") (Not (Var "q"))) (Var "r"))

- Ejemplo para probar funcione arbolaformula:
arbolaformula (Node "∧" [Node "p" [], Node "→" [Node "¬" [Node "q" []], Node "r" []]])


