-- Implementaciones de lógica proposicional
-- Lógica Computacional 2017-1
-- Posgrado en Ciencia e Ingeniería de la Computación UNAM
-- Dr. Favio E. Miranda Perea.

import Data.List

type VarIndex = Int

data Prop = Top | Bot | P VarIndex | Neg Prop | Or Prop Prop | And Prop Prop
            | Impl Prop Prop | Syss Prop Prop deriving Eq

instance Show Prop where
 show Top = "T"
 show Bot = "F"
 show (P x) = "P "++show x
 show (Neg p) = "~ "++show p
 show (Or p1 p2) = "("++(show p1)++" v "++(show p2)++")"
 show (And p1 p2) = "("++(show p1)++" ^ "++(show p2)++")"
 show (Impl p1 p2) = "("++(show p1)++" -> "++(show p2)++")"
 show (Syss p1 p2) = "("++(show p1)++" <-> "++(show p2)++")"                                                       
-- Ejemplos de fórmulas:

--p = P 0
--q = P 1
--p1 = Neg ( And (P 1) (P 2)) 
--p2 = Impl p1 (Or (Neg (P 4)) (P 7))
mp = Impl (And (Impl p q) p) q 


-- Numero de Conectivos
nc :: Prop -> Int

nc (P _) = 0
nc Top = 0
nc Bot = 0

nc (Neg a) = nc a + 1
nc (And a b) = nc a + nc b + 1
nc (Or a b) = nc a + nc b + 1
nc (Impl a b) = nc a + nc b + 1
nc (Syss a b) = nc a + nc b + 1



vars' :: Prop -> [VarIndex]

vars' Top = []
vars' Bot = []
vars' (P x) = [x]
vars' (Neg p) = (vars' p)
vars' (Or p1 p2) = (vars' p1)++(vars' p2)
vars' (And p1 p2) = (vars' p1)++(vars' p2)
vars' (Impl p1 p2) = (vars' p1)++(vars' p2)
vars' (Syss p1 p2) = (vars' p1)++(vars' p2)


--vars :: Prop -> [Prop]

vars = nub . vars'

-- Quita implicaciones.
qie :: Prop -> Prop

qie (Top) = Top

qie (Bot) = Bot

qie (P i) = P i

qie (Neg a) = Neg (qie a)

qie (And a b) = And (qie a) (qie b)

qie (Or a b) = Or (qie a) (qie b)

qie (Impl a b) =  Or (Neg (qie a)) (qie b)

qie (Syss a b) =  And (Or (Neg (qie a)) (qie b)) (Or (Neg (qie b)) (qie a))

-- Forma Normal Negativa
fnn :: Prop -> Prop

fnn (P i) = P i

fnn Top = Top

fnn Bot = Bot

fnn (Neg Top) = Bot

fnn (Neg Bot) = Top

fnn (Neg (P i)) = Neg (P i)

fnn (Neg (Neg a)) = fnn a

fnn (Neg (And a b)) = Or (fnn (Neg a)) (fnn (Neg b))

fnn (Neg (Or a b)) = And (fnn (Neg a)) (fnn (Neg b))

fnn (And a b) = And (fnn a) (fnn b)

fnn (Or a b) = Or (fnn a) (fnn b)


fnne = fnn . qie

-- Distrivutiva.
distr :: Prop -> Prop -> Prop

distr (And a11 a12) a2 = And (distr a11 a2) (distr a12 a2)

distr a1 (And a21 a22)  = And (distr a1 a21) (distr a1 a22)

distr a b = Or a b

-- Forma Normal Conjuntiva
fnc (And a b) = And (fnc a) (fnc b)

fnc (Or a b) = distr (fnc a) (fnc b)

fnc a = a

-- Semántica

type Estado = [VarIndex]


-- sem i a = True syss I(a) = 1

sem :: Estado -> Prop -> Bool

--sem e (P i) = if elem i e == True then True else False

sem e (P i) = elem i e

sem e Top = True

sem e Bot = False

sem e (Neg a) = not (sem e a)

sem e (And a b) = sem e a && sem e b

sem e (Or a b) = sem e a || sem e b

sem e (Impl a b) = not (sem e a) || sem e b

sem e (Syss a b) = sem e a == sem e b

sat e a  = sem e a == True



--estados :: Prop -> [[Prop]]

--estados a = subsequences (vars a)


lestados :: Prop -> [Estado]

--lestados a = subsequences (vars a)

lestados = subsequences . vars

--taut a = 

-- Variables para RCU
p = P 0
q = P 1
r = P 2
s = P 3
conjuntoClausulas = [[Neg(p), q],[Neg(p), r],[Neg(p), s],[s],[Neg(s), p],[Neg(r)]]
--ejmClausula =[Neg(p), s]
ejmClausula =[r, q]

-- Regla de clausula unitaria.
--rcu :: [[Prop,Prop]] -> Bool
--rcu ([[Prop,Prop]]) = True
--tamano [] = []
--tamano (x:xs) = if (length x)>=2 then [x]++tamano xs else tamano xs

obtieneUnitarias [] = []
obtieneUnitarias (x:xs) = if (length x)==1 then x++obtieneUnitarias xs else obtieneUnitarias xs

quitaLiteral [] _= []
quitaLiteral (x:xs) y = if x == y then quitaLiteral xs y else [x]++quitaLiteral xs y

quitarUnitariaDelConjuntoClausula [] _= []
quitarUnitariaDelConjuntoClausula (x:xs) y = [(quitaLiteral x y)]++quitarUnitariaDelConjuntoClausula xs y

quitarUnitariasDelConjuntoClausula [] = []
quitarUnitariasDelConjuntoClausula (x:xs) = if (length x)==2
then [x]++quitarUnitariasDelConjuntoClausula xs
else quitarUnitariasDelConjuntoClausula xs

quitarUnitarias [] [] = []
quitarUnitarias (x:xs) (y:ys) = quitarUnitarias (quitarUnitariaDelConjuntoClausula ([x]++xs) y) ys
quitarUnitarias [] (y:ys) = []
quitarUnitarias (x:xs) [] = [x]++xs


rcu [] = []
rcu (x:xs) = obtieneUnitarias ([x]++xs)

-- ya merengues
