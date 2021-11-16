-- Consigna 1
-- funcion1:: Integer -> Integer
funcion1 x n
    | n > 0 = x + 1
    | n < 0 = x - 1
    | otherwise = 0

-- Consigna 2
-- funcion2::[Integer] -> Integer -> [Integer]
funcion2 [] n = []
funcion2 (x:xs) n = funcion1 x n : funcion2 xs n

-- Consigna 3 
-- factorial::Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)
-- funcion3::Integer -> Integer -> Float
funcion3 x 0 = 1
funcion3 x n = fromIntegral(x^n) / fromIntegral(factorial n) + (funcion3 x (n-1))


-- Consigna 4
-- funcion4Bis:: Float->Integer->Float
funcion4Bis x 0 = x
funcion4Bis x n
    | x < 0 || x > 6.284 = -2
    | otherwise = ( fromIntegral((-1)^n) / fromIntegral(factorial (2*n+1))) * x^(2*n+1) + (funcion4Bis x (n-1))
--funcion4::Float->Float
funcion4 x = funcion4Bis x 9

-- Consigna 5
--funcion5::[Float]->[(Float, Float)]
funcion5 xs = [(x, funcion4 x) | x <- xs]

-- Consigna 6
--funcion6::Integer->(Integer, Integer)
funcion6 x n 
    | n == 0 = (0, funcion1 x n)
    | otherwise = (x, funcion1 x n)
