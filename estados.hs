data Estados generador estado = Estado (generador -> generador) estado


instance Monad (Estados generador) where
	return estado = (Estado (id) estado)
	Estado generador estado >>= f = (Estado (generador.f) estado)

	
