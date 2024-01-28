module Lista1 where
import Data.Char



{-
1. (valor 3 pontos)
1.1 Construa o list comprehension que gere:
(a) [(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
(b) Uma lista de ’a’ a ’z’ sem as vogais.
(c) Uma lista de 0 a 50 sem os n´umeros 2, 7, 13, 35 e 42
(d) Uma lista com todas as coordenadas dde um tabuleiro de damas 8x8:
([(’a’,1),(’a’,2),(’a’,3) ... (’h’,7), (’h’,8)])
(e) Crie uma fun¸c˜ao que verifique se o tamanho de uma String ´e par ou n˜ao. Use Bool como retorno
-}

exercicioA :: [Integer]
exercicioA = [0,5..50]

exercicioB :: String
exercicioB = [ x | x <- ['a'..'z'], x /= 'a', x /= 'e', x /= 'i', x /= 'o', x /= 'u' ]

exercicioC :: [Integer]
exercicioC = [ x | x <- [0..50], x /= 2, x /= 7, x /= 35, x /= 42 ]

exercicioD :: [(Char, Integer)]
exercicioD = [ (c,i) | c <-['a'..'h'], i <-[1..8] ]

exercicioE :: [Char] -> Bool
exercicioE strg = even (length strg)

{-
1.2 Implemente as seguintes fun¸c˜oes:
(b) Escreva uma fun¸c˜ao que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem
reversa
(c) Escreva a fun¸c˜ao head como composi¸c˜ao de duas outras.
(d) Implemente uma fun¸c˜ao que receba um tipo Int e retorne a convers˜ao dele para bin´ario. A saida da fun¸c˜ao
deve ter um [Int] ou
-}

exercicioF :: [Char] -> [[Char]]
exercicioF string = reverse [ [x] | x <- string]

exercicioG :: [Char] -> Char
exercicioG = head . reverse

decimalToBinario :: Int -> [Int]
decimalToBinario 0 = []
decimalToBinario decimal = bin : decimalToBinario (div decimal 2)
    where
        bin = mod decimal 2

{-
2. (valor 3 pontos)
2.1 Fa¸ca um novo tipo chamado Mes , que possui como valores todos os meses do ano. Implemente:
• A fun¸c˜ao checaFim , que retorna o n´umero de dias que cada mˆes possui (considere fevereiro tendo 28 dias).
• A fun¸c˜ao prox , que recebe um mˆes atual e retorna o pr´oximo mˆes.
• A fun¸c˜ao estacao , que retorna a esta¸c˜ao do ano de acordo com o mˆes e com o hemisf´erio. (Use apenas
tipos criados pela palavra data aqui.)
-}

data Mes = Janeiro  | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving Show
data Hemisferio = Norte | Sul deriving Show
data Estacao = Verao | Outono | Primavera | Inverno deriving Show

checafim :: Mes -> String
checafim Janeiro = "Janeiro tem 31 dias"
checafim Fevereiro = "Fevereiro tem 28 dias"
checafim Marco = "Marco tem 31 dias"
checafim Abril = "Abril tem 30 dias"
checafim Maio = "Maio tem 31 dias"
checafim Junho = "Junho tem 30 dias"
checafim Julho = "Julho tem 31 dias"
checafim Agosto = "Agosto tem 30 dias"
checafim Setembro = "Setembro tem 31 dias"
checafim Outubro = "Outubro tem 30 dias"
checafim Novembro = "Novembro tem 31 dias"
checafim Dezembro = "Dezembro tem 30 dias"

prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

estacao :: Mes -> Hemisferio -> Estacao
estacao Janeiro Norte = Inverno
estacao Janeiro Sul = Verao
estacao Fevereiro Norte = Inverno
estacao Fevereiro Sul = Verao
estacao Marco Norte = Primavera
estacao Marco Sul = Outono
estacao Abril Norte = Primavera
estacao Abril Sul = Outono
estacao Maio Norte = Primavera
estacao Maio Sul = Outono
estacao Junho Norte = Verao
estacao Junho Sul = Inverno
estacao Julho Norte = Verao
estacao Julho Sul = Inverno
estacao Agosto Norte = Verao
estacao Agosto Sul = Inverno
estacao Setembro Norte = Outono
estacao Setembro Sul = Primavera
estacao Outubro Norte = Outono
estacao Outubro Sul = Primavera
estacao Novembro Norte = Outono
estacao Novembro Sul = Primavera
estacao Dezembro Norte = Inverno
estacao Dezembro Sul = Verao
{-
2.2 Fa¸ca o tipo Cripto que possua dois values constructors Mensagem e Cifrado , ambos com um campo String
e um value constructor Erro . Fa¸ca as fun¸c˜oes encriptar e decriptar , seguindo cada exemplo a seguir
Prelude>encriptar (Mensagem "FATEC")
Cifrado "GBUFD"
Prelude>decriptar (Cifrado "DBTB")
Mensagem "CASA"
Veja que a encripta¸c˜ao deve empurrar cada letra a frente e a decripta¸c˜ao faz o inverso, empurrando uma letra
para tr´as. Use as fun¸c˜oes succ e pred , e tamb´em list compreeshions. N˜ao ´e poss´ıvel encriptar mensagens cifradas
e decriptar mensagens
-}

data Cripto = Mensagem String | Cifrado String | Erro String deriving Show

encriptar :: Cripto -> Cripto
encriptar (Cifrado _ ) = Erro "Erro"
encriptar (Mensagem string) = Cifrado [ chr (ord x + 1) | x <- string]

decriptar :: Cripto -> Cripto
decriptar (Mensagem _) = Erro "Erro"
decriptar (Cifrado string) = Mensagem [ chr (ord x - 1) | x <- string]


{-
3. (valor 2 pontos)
3.1 Implemente uma fun¸c˜ao que filtre os n´umeros pares e outra que filtre os ´ımpares de uma lista recebida
via parˆametro.
-}
filtrarPar :: [Int] -> [Int]
filtrarPar listNum = [ x | x <- listNum, even x  ]

filtrarImpar :: [Int] -> [Int]
filtrarImpar listNum = [ x | x <- listNum, odd x]
{-
3.2 Implemente o tipo Dinheiro que contenha os campos valor e correncia ( Real ou Dolar ), e uma fun¸c˜ao que
converta todos os ”dinheiros”de uma lista para d´olar (e outra para real). Com isso, implemente fun¸c˜oes para:
(a) Filtrar todos os Dolares de uma lista de Dinheiro .
(b) Somar todos os Dolares de uma lista.
(c) Aumentar a quantidade o valor dos reais em uma [Dinheiro].
-}
data CorrenciaType = Dolar | Real deriving (Show,Eq, Ord)
data Dinheiro = Valor {val :: Integer, correncia :: CorrenciaType} | ErroConvercao {val :: Integer, correncia :: CorrenciaType} deriving (Show,Eq, Ord)

convertDolarToReal :: [Dinheiro] -> [Dinheiro] -- Considerando dólar 5 reais
convertDolarToReal [Valor {val=_, correncia=Real}] =  [ErroConvercao 0 Dolar]
convertDolarToReal [Valor {val=dinheiro, correncia=Dolar}] = [Valor (div dinheiro 5) Real | (Valor dinheiro Real) <- [Valor dinheiro Dolar]] 

filtrarDolar :: [Dinheiro] -> [Dinheiro]
filtrarDolar [Valor dinheiro _ ] = [Valor dinheiro Dolar]

somarDolar :: [Dinheiro] -> [Dinheiro]
somarDolar a = a

aumentarReais :: [Dinheiro] -> Int -> [Dinheiro]
aumentarReais a b = a


{-
4. (valor 2)
4.1 Dado o tipo de dado:
data Lista a = Nulo | a :>: (Lista a) deriving Show
Implemente a fun¸c˜ao removerElemento que recebe um elemento ”a”qualquer, uma Lista a e retorne uma Lista
a com o elemento removido. removerElemento :: a -> Lista a -> Lista a
-}

data Lista a = Nulo | a :>: (Lista a) deriving Show

verificaElemento :: (Eq a) => a -> Lista a -> Bool
verificaElemento _ Nulo = False
verificaElemento elementoD (elementoL :>: restoLista)
    | elementoL == elementoD = True
    | otherwise = verificaElemento elementoD restoLista

removerElemento :: a -> Lista a -> Lista a
removerElemento a list = list 

{-
4.2 Crie o tipo Paridade com os values constructors Par e Impar .Crie o typeclass ParImpar que cont´em a fun¸c˜ao
" decide :: a -> Paridade " e possui as instˆancias:
• Para Int : no¸c˜ao de Par/Impar de Int .
• Para [a] : uma lista de elementos qualquer ´e Par se o n´umero de elementos o for.
• Bool : False como Par , True como Impar.
-}

data Paridade = Par | Impar deriving Show

class ParImpar a where
    decide :: a -> Paridade

-- instance decide [a] where
--     decide a
--             | a == [a] = [a]
--             | otherwise [a] = [a]

-- instance decide Bool where
--     decide a = True

{-
4.3 Crie o tipo TipoProduto que possui os values constructors Escritorio , Informatica , Livro , Filme e Total .
O tipo Produto possui um value constructor - de mesmo nome - e os campos valor ( Double ), tp ( TipoProduto
) e um value constructor Nada , que representa a ausˆencia de um Produto .
Deseja-se calcular o valor total de uma compra, de modo a n˜ao ter nenhuma convers˜ao para inteiro e de forma
combin´avel. Crie uma instˆancia de semigrupo e monoide para Produto , de modo que o retorno sempre tenha
Total no campo tp e a soma dos dois produtos m valor . Explique como seria o exerc´ıcio sem o uso de monoides.
Qual(is) seria(m) a(s) diferen¸ca(s)?
4.4 Dado o tipo de dados
data Arvore a = Galho a (Arvore a) (Arvore a) | Folha a | Nulo deriving show
• Implemente os percursos p´os-ordem e pr´e-ordem. Via coment´ario, fa¸ca os ”testes de mesa”para os dois
percursos da ´arvore Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula)) (Raiz 20 Nula (Raiz 22 (Folha
21) Nula))
• Usando a estrutura de ´arvore vista, fa¸ca uma fun¸c˜ao que some todos os elementos de uma ´arvore de n´umeros.
-}

{-
5. (Bˆonus 2 pontos) Dado o tipo e as implementa¸c˜oes:
module Rec where
data Nat = Z | Suc Nat deriving Show
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n
-- input: natToInt (Suc (Suc (Suc (Suc Z))))
-- output: 4
somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)
-- input: somar (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))
mult :: Nat -> Nat -> Nat
mult x Z = Z
mult x (Suc Z) = x
mult x (Suc n) = somar x (mult x n)
-- input: mult (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))
fat :: Int -> Int
fat n
| n <= 0 = 1
| otherwise = n * fat (n - 1)
fatt :: Nat -> Nat
fatt Z = Suc Z
fatt (Suc n) = mult (Suc n) (fatt n)
Implemente a fun¸c˜ao fibb :: Nat -> Nat .
-}
data Nat = Z | Suc Nat deriving Show
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n
-- input: natToInt (Suc (Suc (Suc (Suc Z))))
-- output: 4
somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)
-- input: somar (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))
mult :: Nat -> Nat -> Nat
mult x Z = Z
mult x (Suc Z) = x
mult x (Suc n) = somar x (mult x n)
-- input: mult (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))
fat :: Int -> Int
fat n
    | n <= 0 = 1
    | otherwise = n * fat (n - 1)
fatt :: Nat -> Nat
fatt Z = Suc Z
fatt (Suc n) = mult (Suc n) (fatt n)

fibb :: Nat -> Nat
fibb Z = Suc Z
fibb (Suc n) = somar (Suc n) (Suc (Suc n))