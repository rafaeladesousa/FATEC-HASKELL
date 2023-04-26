-- 1.  (valor 2 pontos)Qual  ́e o sistema de tipos do Haskell, escolha a opcao correta.
-- (b)  (X) Fortemente/estaticamente tipado

-- 2.  (valor 2 pontos)Usando map/filter/foldl/list comprehension implemente fuņcoes que
-- (a)  (0.5) Mostre a quantidade de pares de um[Int]
contaPares :: [Int] -> Int
contaPares xs = length [x | x <- xs, even x]

-- (b)  (0.5) Mostre a quantidade de negativos excluindo o zero de um vetor de[Int]
contaNegativos :: [Int] -> Int
contaNegativos xs = length [ x | x <- xs, x <0]

-- (c)  (1.0) Retorne os numeros ımpares positivos de um[Int]
contaImparesPositivos :: [Int] -> Int
contaImparesPositivos xs = length [ x | x <- xs , x>0 , odd x]

-- (d)  (0.5) Retorne a metade da soma de todos os numeros de um[Double]
metadeSoma :: [Double] -> Double
metadeSoma xs = foldl1 (+) xs / 2

-- 3.  (valor 4 pontos)
-- (a)  (0.5 ponto) Crie um tipo de dado algebrico Regiao que contenha os value constructors: America,Europa,Asia.

data Regiao = America | Europa | Asia deriving Show

-- (b)  (0.5 ponto) Crie o tipo Eletronico que connha os value constructors:  Computador e VideoGame.  Valueconstructor  Computador  possui  campo  marca,modelo,preco.   
-- O  value  constructor  VideoGame  possui  ocampo marca,modelo,preco e regiao do tipo Regiao;

data Eletronico = Computador { marca :: String, modelo :: String, preco:: Float} |
                  VideoGame { marca :: String, modelo :: String, preco:: Float, regiao :: Regiao} deriving Show

-- (c)  (2  pontos)  Implemente  a  funcao  filtraComputadores  que  recebe  uma  lista  do  tipo  Eletronico  e  retorna apenas uma lista com valores do tipo Computador.

filtraComputadores :: [Eletronico] -> [Eletronico]
filtraComputadores eletronicos = filter ehComputador eletronicos
    where
        ehComputador :: Eletronico -> Bool
        ehComputador (Computador _ _ _) = True 
        ehComputador _ = False

-- (d)  (1 pontos) Implemente a funcao totalEletronicosAsia que recebe uma lista do tipo Eletronico e retorna aquantidade de eletronicos de origem Asiatica.  A funcao anterior pode ajudar.

-- totalEletronicosAsia :: [Eletronico] -> Int
-- totalEletronicosAsia eletronicos = length eletronicosAsia 
--     where 
--     eletronicosAsia = filter ehVideoGameAsia eletronicos

--     ehVideoGameAsia :: Eletronico -> Bool
--     ehVideoGameAsia (VideoGame _ _ _ reg) = regiao == Asia
--     ehVideoGameAsia _ = False 


-- 4.  (valor 2)Implemente uma funcao que calcule o tribonacci:Tk=(1k = 1 ou k = 2 ou k = 3TkTk−1+Tk−2+Tk−3 
-- Nao esqueca de tratar entradas incorretas.
trib :: Int -> Int
trib 0 = 0
trib 1 = 0
trib 2 = 1
trib valor = trib(valor -1) + trib(valor -2) + trib(valor -3)

-- 5.  (valor 2 pontos)
-- Calcule as expressoes abaixo (mostre o resultado):
-- (a)map (\ z -> z + 1 * 4 / 2 ) [3,4,5]

-- [5.0,6.0,7.0]

-- (b)filter odd [-8..20]

-- [-7,-5,-3,-1,1,3,5,7,9,11,13,15,17,19]

-- (c)foldl (\ b a ->  [a] ++ b ) "" ['F','A']

-- "AF"

-- (d):t reverse "FA" == foldl (\ b a ->  [a] ++ b ) "" ['F','A']

-- reverse "FA" == foldl (\ b a ->  [a] ++ b ) "" ['F','A'] :: Bool

-- (e):t (,)

-- (,) :: a -> b -> (a, b)

-- (f):t (,"Ola")

-- (,"Ola") :: t -> (t, String)