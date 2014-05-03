module Conjunto (
	Conjunto,
	conjuntoVacio,
	estaVacio,
	anadirAConjunto,
	perteneceAConjunto,
	eliminarDelConjunto,
	(/\),
	(\/),
	interseccion,
	union)where
		
		data Conjunto a = Conjunto [a] 

		conjuntoVacio = Conjunto []
		estaVacio (Conjunto []) = True
		estaVacio (Conjunto _) = False


		anadirAConjunto x c@(Conjunto xs) 
			| elem x xs = c 
			| otherwise = Conjunto ( x:xs )

		perteneceAConjunto x (Conjunto xs) = elem x xs

		eliminarDelConjunto x (Conjunto xs) = Conjunto (filter (not.(x==)) xs)


		--unirConjuntos conjunto (Conjunto []) = conjunto 
		--unirConjuntos conjunto conjuntoY@(Conjunto (y:ys)) = unirConjuntos (anadirAConjunto y conjunto) (eliminarDelConjunto y conjuntoY)

		infixl \/
		conjunto \/ (Conjunto ys) = foldr anadirAConjunto conjunto ys

		infixl /\ 
		(/\) :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
		cs /\ (Conjunto ys) = Conjunto [x | x <- ys, perteneceAConjunto x cs]

		interseccion a b = (a/\b)
		union a b = (a\/b)


		instance Show a => Show (Conjunto a) where
			show (Conjunto xs) = "{" ++ show' xs where
				show'[] = "}"
				show'[x] = show x ++ show'[]
				show'(x:xs) = show x ++ "," ++ show' xs





