-- INTEGRANTES: HENRIQUE VIOLA; KARINA GOMES; RAFAELA SOUSA; RODRIGO MARIANO 

-- 2.1) Gere as listas:
-- A
item1 :: [Int]
item1 = take 7 $ iterate (*11)1

-- B 
item2 :: [Int] = [x|x <- [1..32], mod x 4 /=0]

{- C -}
item3 :: [String] = [x:y: "BB" | x<-['A'], y <-['a'..'g']]

-- D
item4 = [x | x <- [5,8..41], x /= 14, x/=23, x/= 35]

-- E
item5 = take 6 $ iterate (/2) 1.0

-- H
item6 = [ x| x <- ['@'..'L'], x/='B', x/='K']

-- 2.2 Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno.
item7 = "item7"
contadorTexto :: String -> Bool
contadorTexto texto = even(length texto)

-- 2.3 Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem reversa.
reverso :: [a] -> [a]
reverso item8 = reverse item8

{- 2.4 Escreva uma função que receba um vetor de Strings e
retorne uma lista com o tamanho de cada String. As palavras de
tamanho par devem ser excluídas da resposta.-}

comprimentoTexto :: [String] -> [Int]
comprimentoTexto listaImp = [ length x | x <- listaImp, length x `mod` 2 /= 0]

-- 2.5 Escreva a função head como composição de duas outras.
item10 :: [Int] -> Int
item10 xs = (last.reverse) xs

-- 2.6 Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False .
conferePalindromo :: String -> Bool
conferePalindromo x = x == reverse x

{- 2.7 Faça uma função que receba um inteiro e retorne uma tupla, contendo: o dobro deste número na primeira coordenada, o
triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.-}
tupla :: Int -> (Int, Int, Int, Int)
tupla n = (n*2, n*3, n*4, n*5)

{- 3.3 Implemente uma função que simule o vencedor de uma partida de pedra, papel e tesoura usando tipos criados(implemente os tipos). Casos de
empate devem ser considerados em seu tipo.-}
data Jogada = Pedra | Papel | Tesoura

jokempo :: Jogada -> Jogada -> String

jokempo Papel Pedra = "VENCEDOR: JOGADOR 1!!"
jokempo Pedra Tesoura = "VENCEDOR: JOGADOR 1!!"
jokempo Tesoura Papel = "VENCEDOR: JOGADOR 1!!"

jokempo Pedra Papel = "VENCEDOR: JOGADOR 2!!"
jokempo Tesoura Pedra = "VENCEDOR: JOGADOR 2!!"
jokempo Papel Tesoura = "VENCEDOR: JOGADOR 2!!"

jokempo Pedra Pedra = "EMPATE"
jokempo Tesoura Tesoura = "EMPATE"
jokempo Papel Papel = "EMPATE"

{- 3.4 Faça uma função que retorne uma string, com todas as vogais maiúsculas 
e minúsculas eliminadas de uma string passada por parâmetro usando list compreenshion.-}
eliminaVogais :: String -> String
eliminaVogais palavraRecebida = [ x | x <- palavraRecebida, x `notElem` "aeiouAEIOU" ]

