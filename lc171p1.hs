-- Práctica 1 - Lógica de Predicados
-- Lógica Computacional 2017-1
-- Posgrado en Ciencia e Ingeniería de la Computación UNAM
-- Dr. Favio E. Miranda Perea  - Mtra. Selene Linares Arévalo

import Data.List

-- El índice de una variable es un entero
type VarIndex = Integer

-- El nombre de una función o un predicado es una cadena.
-- Por convención utilizamos minúsculas para funciones y mayúsculas para predicados.
type Name = String

-- Tipo de dato para representar términos.
-- Un término es una variable o un símbolo de función aplicado a una lista de términos. 
data Term = X VarIndex | Fn Name [Term] deriving (Show,Eq)

-- Tipo de dato para representar fórmulas.
data Form  = Top | Bot | Pr Name [Term] | Eq Term Term | Neg Form | Or Form Form | And Form Form
            | Impl Form Form | Syss Form Form |  All TVar Form | Ex TVar Form deriving (Show,Eq)

            
-- Las variables son términos.            
type TVar = Term

-- Una sustitución es una lista de pares cuya primer entrada es una variable 
-- y la segunda entrada es el término por el cual sustituimos la variable.
type Sust = [(TVar,Term)]

--Función para construir constantes recibiendo como parámetro, el nombre de la constante.
ct c = Fn c []







-- Ejemplos de términos y fórmulas.



-- t1 =  g(a,f(x5),h(b,x9))
t1 = Fn "g" [ct "a", Fn "f" [X 5], Fn "h" [ct "b", X 9]] 

-- f1 = All x1 . P(x1, x43, f(x1))
f1 = All (X 1) (Pr "P" [X 1, X 43, Fn "f" [X 1]])

-- f2 = Ex x0. Ex x1. P(x0,g(a)) /\ Q(x3,g(x3))
f2 = Ex (X 0) (Ex (X 1) (And (Pr "P" [X 0, Fn "g" [Fn "a" []]]) (Pr "Q" [X 3,Fn "g" [X 3]])))

