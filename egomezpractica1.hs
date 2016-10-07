-- Práctica 1
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

--Quita las implicaciones de las fórmulas
qie :: Prop -> Prop
qie (Top) = Top
qie (Bot) = Bot
qie (P i) = P i
qie (Neg a) = Neg (qie a)
qie (And a b) = And (qie a) (qie b)
qie (Or a b) = Or (qie a) (qie b)
qie (Impl a b) =  Or (Neg (qie a)) (qie b)
qie (Syss a b) =  And (Or (Neg (qie a)) (qie b)) (Or (Neg (qie b)) (qie a))


--Convierte la fórmula en forma normal negativa
fnn :: Prop -> Prop
fnn Top = Top
fnn Bot = Bot
fnn (P i) = P i
fnn (Neg Top) = Bot
fnn (Neg Bot) = Top
fnn (Neg (P i)) = Neg (P i)
fnn (Neg (Neg a)) = fnn a
fnn (Neg (And a b)) = Or (fnn (Neg a)) (fnn (Neg b))
fnn (Neg (Or a b)) = And (fnn (Neg a)) (fnn (Neg b))
fnn (And a b) = And (fnn a) (fnn b)
fnn (Or a b) = Or (fnn a) (fnn b)
fnn ((a))= fnn(a)
fnne = fnn . qie



-- Distribuye la conjunción en la disyunción
-- ejemplo:  
--    distcd (p /\ (q \/ r))  ==>  ((p /\ q) \/ (p /\ r))
--    distcd ((p \/ q) /\ r)  ==>  ((p /\ r) \/ (q /\ r))
-- Nota: Se supone que f está en forma normal negativa.

distcd :: Prop -> Prop
distcd (And (Or a b) g) = distcd (Or (distcd (And (distcd a) (distcd g))) (distcd (And (distcd b) (distcd g))))

--distcd (And (Or a b) g) = distcd (Or (And (distcd a) (distcd g)) (And (distcd b) (distcd g)))
--distcd (And f (Or c d)) = distcd (Or (And (distcd f) (distcd c)) (And (distcd f) (distcd d)))

distcd (And f (Or c d)) = distcd (Or (distcd (And (distcd f) (distcd c))) (distcd (And (distcd f) (distcd d))))
distcd (Or f g) = Or (distcd f) (distcd g)
distcd (a) = a

fnd :: Prop -> Prop
fnd f = distcd (fnne f)

--Ejemplos
p2 = Impl (P 1) (Or (Neg (P 4)) (P 7))                                  
p4 = And (Or (P 1) (P 2)) (P 0)
p1 = And (And (P 1) (P 2)) (Or (P 3) (P 4))
p5 = And (And (Or (P 1) (P 2)) (P 0)) (P 4)

t2 = And (Or (Neg (P 1)) (P 2)) (Or (P 3) (P 4))
t1 = Or (Neg (Impl (P 1) (P 2))) (Neg (Or (Syss (Neg (P 3)) (P 1)) (And (P 2) (P 3)) ))
