module Practica05 where

import Terminos

-- Aplicar una sustitución a un término

apsubT :: Term -> Subst -> Term
apsubT (Var x) s =
  case lookup x s of
    Just u  -> u
    Nothing -> Var x

apsubT (Fun f ts) s =
  Fun f (apsubLista ts s)

-- Aplicar sustitución a una lista de términos

apsubLista :: [Term] -> Subst -> [Term]
apsubLista [] s = []

apsubLista (t:ts) s =
  apsubT t s : apsubLista ts s

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus [] = []
simpSus ((x,t):xs)
  | t == Var x = simpSus xs
  | otherwise = (x,t) : simpSus xs
  
--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = simpSus (aplicados ++ restantes)
  where
    aplicados = [ (x, apsubT t s2) | (x, t) <- s1]
    restantes = [ (y, t) | (y, t) <- s2, notElem y (map fst s1) ]
    
--Funcion que devuelve un umg de dos terminos, si es que lo hay

occurs :: Nombre -> Term -> Bool
occurs name (Var x)      = name == x
occurs name (Fun _ args) = any (occurs name) args

-- Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica (Var x) (Var y)
  | x == y    = [[]]
  | otherwise = [[(x, Var y)]]
unifica (Var x) t
  | occurs x t = []
  | otherwise  = [[(x, t)]]
unifica t (Var x)
  | occurs x t = []
  | otherwise  = [[(x, t)]] -- Regla SWAP: pasamos la variable a la izquierda
unifica (Fun f1 ts1) (Fun f2 ts2)
  | f1 == f2 && length ts1 == length ts2 = unificaListas ts1 ts2
  | otherwise                            = []
  
--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]
unificaListas (t1:ts1) (t2:ts2) = 
    case unifica t1 t2 of
        []   -> []
        [s1] -> case unificaListas (apsubLista ts1 s1) (apsubLista ts2 s1) of
                  []   -> []
                  [s2] -> [compSus s1 s2]
unificaListas _ _ = []

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj []   = [[]]
unificaConj [t]  = [[]]
unificaConj (t1:t2:ts) = case unifica t1 t2 of
    []   -> []
    [u1] -> case unificaConj (apsubLista (t2:ts) u1) of
              []   -> []
              [u2] -> [compSus u1 u2]