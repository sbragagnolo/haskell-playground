data Errorable a = Error | Exito a deriving Show

instance Monad Errorable where
	return a = (Exito a)
	Error >>= f = Error
	Exito a >>= f = f a
	


suma :: Num a => Errorable a -> Errorable a -> Errorable a
suma (Exito a) (Exito b) = Exito (a+b)
divide (Exito a) (Exito 0) = Error
divide (Exito a) (Exito b) = Exito (a/b)


suma1 n = suma (Exito 1) (Exito n)
dividePorCero x = divide (Exito x) (Exito 0)
dividePorDos x = divide (Exito x) (Exito 2)

try (Exito e) catch = e
try (Error) catch = catch 

hayError = 0








