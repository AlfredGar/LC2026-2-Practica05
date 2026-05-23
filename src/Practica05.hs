module Practica05 where

import Terminos

--Aplicar una sustitucion a un termino

apsubT :: Term -> Subst -> Term
apsubT (Var name) sub = case lookup name sub of
    Just t  -> t
    Nothing -> Var name
apsubT (Fun name args) sub = Fun name (aplicarLista args sub)

-- Función auxiliar para aplicar la sustitución a una lista de términos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista ts sub = map (\t -> apsubT t sub) ts

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
unifica :: Term -> Term -> [Subst]
unifica = undefined


--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = undefined

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj = undefined

