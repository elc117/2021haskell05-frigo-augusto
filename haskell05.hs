--Pratica 5 de haskell
--Nome: Augusto Pagnossim Frigo

bmi :: Float -> Float -> String
bmi weight height =
  let imc = weight/(height^2) 
  in
    case () of _
                | imc < 18.5 -> "ABAIXO"
                | imc > 18.5 && imc < 30 -> "NORMAL"
                | imc > 30 -> "ACIMA"



bmi' :: Float -> Float -> String
bmi' weight height =
    case () of _
                | imc < 18.5 -> "ABAIXO"
                | imc > 18.5 && imc < 30 -> "NORMAL"
                | imc > 30 -> "ACIMA"
    where
        imc = weight / height^2

cpfValid :: [Int] -> Bool
cpfValid cpf =
 dv1 == cpf !! 9 && dv2 == cpf !! 10
 where 
    digits = take 9 cpf
    dv1 = cpfDV digits [10,9..]
    dv2 = cpfDV (digits ++ [dv1]) [11,10..]


cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
 let expr = (sum $ zipWith (*) digits mults) `mod` 11
 in if expr < 2 then 0 else 11-expr

andTable :: [(Bool, Bool, Bool)]
andTable = let aux = [True, False]
           in [(x,y,(x&&y)) | x <- aux, y <- aux] 