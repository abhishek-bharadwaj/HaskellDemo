doubleMe x = x + x

square x = x * x

computeHyperT x y = sqrt(square x + square y)

checkIfRightAngle p b h = if h == p `computeHyperT` b
                            then True
                            else False

printAllRightAngle n = [[x, y, z] | x <-[1..n], y <- [1..n], z <- [1..n], checkIfRightAngle x y z]


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (x: xs) = x + sumList(xs)

pickNumber :: (RealFloat a) => a -> String
pickNumber numb
    | numb <= 0.0   = "You retard!"
    | numb <= 10.0  = "Eh! Playing safe"
    | numb <= 100.0 = "You got a big number.."
    | otherwise     = "You moron"


bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
