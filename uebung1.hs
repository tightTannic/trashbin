{- 
  Aufgabe 1
-}

isParenthesis :: Char -> Bool
isParenthesis c = if (c=='[')||(c==']')||(c=='(')||(c==')')||(c=='{')||(c=='}') then True else False

leap_year :: Int -> Bool
leap_year y = (mod y 400 == 0) || (not ((mod y 100 == 0) && (not (mod y 400 == 0)))) || ((mod y 4 == 0) && (not (mod y 100 == 0)))
{-  immer True :( -}

{-
weekDay :: Int -> Int -> Int -> Int
weekDay day month year
| leap_year year
           where
                month==2 || day>28 = "Achtung. Schaltjahr" 
| year < 0 = show(year) ++ "illegal year value"
| month < 1 || month > 12 = show(month) ++ "illegal month value"
-}




{--
    Funktionale Programmierung, U2, 2019/2020
    Author: M. Esponda
--}

paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)


-- Beispielsfunktionen für die 5. Aufgabe des 1. Übungsblatt

diag (x,y,size) = if (x==y) then 'a' else ' '

quad (x,y,size) = if (x>s && x<3*s && y>s && y<3*s) then ' ' else '+'
                  where
                    s = div size 4

gitter (x,y,size) = if k || p  then ' ' else '0'
                    where
                         k = (mod x space)==0
                         p = (mod y space)==0
                         space = div size 4

rectangles :: (Int, Int, Int) -> Char
rectangles (x,y,z)
    | div z 2 > x = '*'
    | div z 4 * 3 >= x && div z 2 < y = '8'
    | div z 4 * 3 <= x && div z 4 * 3 < y = '|'
    | otherwise = ' '
    
circles :: (Int, Int, Int) -> Char
circles (x,y,z)
    | (x-p)^2+(y-p)^2 < (p-1)^2  = ' '         --- Äußerer Kreis, Subtraktion von p für den Rand um den Kreis
    --- | (x-p)^2+(y-p)^2 < (q-1)^2  = '.' 
    | otherwise = '*'
        where p = div z 2  --- center point of circles (x & y same values becuase squared canvas)
        --- where q = div (div z 2) 2
    
    

{- Testbeispiele -}

test1 = paintChars diag 60
test2 = paintChars quad 60
test3 = paintChars gitter 60
test4 = paintChars rectangles 60
test5 = paintChars circles 60