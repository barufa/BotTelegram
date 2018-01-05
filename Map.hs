module Map where

import Data.Tree.RBTree as RBT

type Map a b = RBTree (a,b)

emptyMap :: Map a b
emptyMap = emptyRB

insert :: Ord a => Map a b -> a -> b -> Map a b--Inserta un par (clave,valor)
insert m k v = RBT.insert ins (erase m k) (k,v)--Elimino k de m, y lo agrego con el nuevo valor
               
search :: Ord a => Map a b -> a -> Maybe b--Dada una clave y un map, me devuelve el valor
search m k = do{(x,y)<-searchFast eql m k;return y}

erase :: Ord a => Map a b -> a -> Map a b--Dada una clave, la elimina del arbol m
erase m k = case searchFast eql m k of
              Nothing    -> m
              Just (x,y) -> delete ins m (x,y)

---Funcion de comparacion para insertar comparando por la primer componente
ins :: Ord a => (a,b) -> (a,b) -> Ordering
ins (x,y) z = eql x z

eql :: Ord a => a -> (a,b) -> Ordering
eql x (y,z) = compare x y
