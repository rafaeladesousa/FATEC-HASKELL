-- INTEGRANTES: HENRIQUE VIOLA; KARINA GOMES; RAFAELA SOUSA; RODRIGO MARIANO 

-- 4.1) Faça uma função que retorne a média de um [Double], usando foldl.
retornaMedia :: [Double] -> Double
retornaMedia xs = foldl (+) 0 xs / fromIntegral (length xs)

-- 4.2) Faça uma função que receba uma [String] e retornetodos os elementos palíndromos. Ver exercício 3.7.
verificaPalindromo :: String -> Bool
verificaPalindromo x = x == reverse x

filtraPalindromos :: [String] -> [String]
filtraPalindromos = filter verificaPalindromo

-- 4.4) Filtre os números primos de uma lista recebida porparâmetro.
verificaNumerosPrimos :: Int -> Bool
verificaNumerosPrimos n = length [x | x <- [1 .. n], n `mod` x == 0] == 2

filtraNumerosPrimos :: [Int] -> [Int]
filtraNumerosPrimos = filter verificaNumerosPrimos

-- 4.7) Crie um tipo Dia contendo os dias da semana. Faça uma função que receba uma lista de Dias e filtre as Terças.
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving Show

ehTerca :: Dia -> Bool
ehTerca Terca = True
ehTerca _ = False

filtraAsTercas :: [Dia] -> [Dia]
filtraAsTercas = filter ehTerca

-- 4.8) Implemente o tipo Dinheiro que contenha os camposvalor e correncia ( Real ou Dolar ), e uma
-- função queconverta todos os "dinheiros" de uma lista para dólar (e outra para real). Com isso,
-- implemente funções para:Filtrar todos os Dolares de uma lista de Dinheiro .Somar todos os Dolares
-- de uma lista.Contar a quantidade de Dolares de uma lista.

data Dinheiro =  Dinheiro { valor :: Double, correncia :: String} deriving Show

converteDolarParaReal :: [Dinheiro] -> [Dinheiro]
converteDolarParaReal = map (\dn -> Dinheiro (valor dn * 4.18) "Real")
converteRealParaDolar :: [Dinheiro] -> [Dinheiro]
converteRealParaDolar = map (\dn -> Dinheiro (valor dn * 0.24) "Dolar")

filtraDolar :: [Dinheiro] -> [Dinheiro]
filtraDolar = filter (\dn -> correncia dn == "Dolar")
somadorDeDolares :: [Dinheiro] -> Double
somadorDeDolares d = foldl (\soma dn -> soma + valor dn) 0 $ filtraDolar d

contadorDolares :: [Dinheiro] -> Int
contadorDolares d = length $ filtraDolar d

-- 4.9) Usando a função foldl , crie lambdas para:Contar números negativos de uma lista de Int .Contar
-- letras 'P' de uma String .Para contar Sabados em uma lista de um[DiaSemana] .Para, a partir de uma
-- lista de [DiaSemana] , retornara soma dos dias. Exemplo: [Segunda, Segunda,Quarta] deve retornar
-- 5 . Use uma função auxiliarpara converter DiaSemana para Int.

contaNegativos :: [Int] -> Int
contaNegativos xs = foldl (\cont n -> if n < 0 then cont + 1 else cont) 0 xs

contaLetrasP :: String -> Int
contaLetrasP str = foldl (\cont letra -> if letra == 'P' then cont + 1 else cont) 0 str

data DiaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Show, Eq)

contaSabados :: [DiaSemana] -> Int
contaSabados xs = foldl (\cont dia -> if dia == Sabado then cont + 1 else cont) 0 xs

converteDiaSemana :: DiaSemana -> Int
converteDiaSemana Domingo = 0
converteDiaSemana Segunda = 1
converteDiaSemana Terca = 2
converteDiaSemana Quarta = 3
converteDiaSemana Quinta = 4
converteDiaSemana Sexta = 5
converteDiaSemana Sabado = 6

somaDias :: [DiaSemana] -> Int
somaDias xs = foldl (\cont dia -> cont + converteDiaSemana dia) 0 xs
