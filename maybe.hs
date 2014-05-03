data Quiza a = Solo a | Nada deriving Show

instance Monad Quiza where
	return valor = (Solo valor)
	Nada >>= f = Nada
	Solo valor >>= f =f valor
	
sumar Nada _ = Nada
sumar _ Nada = Nada
sumar (Solo unNumero) (Solo otroNumero) = return (unNumero + otroNumero)

	
