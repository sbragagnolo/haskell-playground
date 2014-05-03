module Diccionario(
	Diccionario
) where
	
	data Asoc a b = Asoc a b deriving Show
	
	type Diccionario a b = [Asoc a b]
	
	
	diccionarioVacio = []
	
	agregar k v diccionario
		| elem (Asoc k v) diccionario = error "Ya existe una entrada para" + k
		| otherwise = Asoc k v:diccionario
		
	
	
	
	