Q1.1 
monExp Int->Int->Int 
monExp 0 _ = 0 
monExp a n = a * monExp a n-1  

// Case of 
monExp :: Int->Int->Int 
monExp a n = case a of 
            | a == 0 = 1 
            | a * monExp a n-1 

Q 1.2 
// recursion terminale 
monExp Int->Int->Int  
monExp a n = expAux n 1 


        whre 
          expAux 0 acc = 1 
          expAux n acc = expAux n-1 acc*a
       
Q1.3 

monExp :: Int->-Int->Int  
monExp _  0 = 1 
monExp a n | r==0 = e*e 
           | otherwise: e*e*acc 

    where 
        (q,r) = quotRem n , 2 
        e = monExp a q 

Q 1.4 
 monExp Int->Int->Int 
 monExp a n = product replicate a n 

 where 
    product [Int]->Int 
    product [] = 0 
    product (x:xs) = x * product xs 

    replicate Int->Int->Int
    replicate _ 0 = [] 
    replicate a n = a:replicate a n-1 

Exercice 2 : 

compteOcc Int -> [Int] -> Int 
compteOcc _ [] = 0 
compteOcc a (x:l) |a==x = 1 + compteOcc l 
                  |otherwise compteOcc l 

elim Int->[Int]->[Int] 
elim _ [] = [] 
elim a (x:l) = |a==x = eli 
               |otherwise x : eli  
        where 
            eli = elim a l

compte [Int] -> [(Int , Int)]  
compte [] = [] 
compte a:l = (a , n):compte l' 
        where
            n = compteOcc a l
            l'= elim a l