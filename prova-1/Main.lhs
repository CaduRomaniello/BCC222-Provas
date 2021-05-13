Avaliação 1 de Programação Funcional
========================

ATENÇÃO
-------

* A interpretação dos enunciados faz parte
da avaliação.

* A avaliação deve ser resolvida INDIVIDUALMENTE.
Se você discutir soluções com outros alunos da
disciplina, deverá mencionar esse fato como
parte dos comentários de sua solução.

* Se você utilizar recursos disponíveis na internet
e que não fazem parte da bibliografia, você deverá
explicitamente citar a fonte apresentando o link
pertinente como um comentário em seu código.

* Todo código produzido por você deve ser acompanhado
por um texto explicando a estratégia usada para a
solução. Lembre-se: meramente parafrasear o código
não é considerado uma explicação!

* Não é permitido modificar a seção Setup inicial
do código, seja por incluir bibliotecas ou por
eliminar a diretiva de compilação -Wall.

* Seu código deve ser compilado sem erros e warnings
de compilação. A presença de erros acarretará em
uma penalidade de 20% para cada erro de compilação e
de 10% para cada warning. Esses valores serão descontados
sobre a nota final obtida pelo aluno.

* Todo o código a ser produzido por você está marcado
usando a função "undefined". Sua solução deverá
substituir a chamada a undefined por uma implementação
apropriada.

* Todas as questões desta avaliação possuem casos de
teste para ajudar no entendimento do resultado
esperado. Para execução dos casos de teste, basta
executar os seguintes comandos:

```
$> stack build
$> stack exec prova1-exe
```

* Sobre a entrega da solução:

1. A entrega da solução da avaliação deve ser feita
como um único arquivo .zip contendo todo o projeto
stack usado.

2. O arquivo .zip a ser entregue deve usar a seguinte
convenção de nome: MATRÍCULA.zip, em que matrícula é
a sua matrícula. Exemplo: Se sua matrícula for
20.1.2020 então o arquivo entregue deve ser
2012020.zip. A não observância ao critério de nome e
formato da solução receberá uma penalidade de 20%
sobre a nota obtida na avaliação.

3. O arquivo de solução deverá ser entregue usando a
atividade "Entrega da Avaliação 1" no Moodle dentro do
prazo estabelecido.

4. É de responsabilidade do aluno a entrega da solução
dentro deste prazo.

5. Sob NENHUMA hipótese serão aceitas soluções fora do
prazo ou entregues usando outra ferramenta que
não a plataforma Moodle.


Setup inicial
-------------

> {-# OPTIONS_GHC -Wall #-}

> module Main where

> import Test.Tasty
> import Test.Tasty.HUnit

> main :: IO ()
> main = defaultMain tests

> tests :: TestTree
> tests
>   = testGroup "Unit tests"
>         [ question1Tests
>         , question2Tests
>         , question3Tests
>         , question4Tests
>         , question5Tests
>         ]

Introdução
----------

Considere o seguinte tipo de dados que modela uma locomotiva
que pode transportar diferentes tipos de produtos.

> data Train a
>    = Empty
>    | Wagon a (Train a)
>    | Locomotive Weight (Train a)
>    deriving (Eq, Ord, Show)

> type Weight = Int

O construtor `Empty` é usado para indicar o fim de uma composição
de locomotiva. Por sua vez, o construtor `Wagon` representa um
vagão que armazena elementos de um tipo `a` e é ligado a um
valor de tipo `Train a`, que representa a continuação da composição
ferroviária. Finalmente, o construtor `Locomotive` denota uma locomotiva
que capaz de transportar um peso (representado pelo tipo `Weight`) e
uma sequência de valores de tipo `Train a`.

Locomotivas podem transportar pessoas e cargas. O tipo de dados
`Cargo` modela os diferentes tipos de carga que pode ser armazenada
em um trem. O constructor `NoCargo` indica que um elemento está
vazio (não leva pessoas ou produtos). Um vagão de pessoas é representado
pelo construtor `Persons`, que armazena uma lista dos pesos das pessoas
nele contido. Finalmente, o constructor `Products` armazena o peso total
de carga armazenada.

> data Cargo
>     = NoCargo
>     | Persons [Weight]
>     | Products Weight
>     deriving (Eq, Ord, Show)

Como exemplo de um valor destes tipos, considere:

> aTrain :: Train Cargo
> aTrain = Locomotive 1000 w1

> w1 :: Train Cargo
> w1 = Wagon (Products 100) w2

> w2 :: Train Cargo
> w2 = Wagon (Persons [70, 90, 110, 60]) w3

> w3 :: Train Cargo
> w3 = Wagon (Products 200) Empty

Dizemos que um valor de tipo `Train a` é válido se as seguintes condições
são verdadeiras:

1. O primeiro construtor de um valor do tipo `Train a` deve ser `Locomotive`.

2. O último construtor de um valor do tipo `Train a` deve ser Empty.

3. A soma de pesos transportados por um comboio representado por um  valor de
tipo `Train a` deve ser menor que o peso máximo suportado pela locomotivas
que "puxam" a composição.

Com base nas 3 condições, podemos concluir que o valor `aTrain` é válido. Porém,
o valor

> wrong1 :: Train Cargo
> wrong1 = Locomotive 100 (Wagon (Products 50) Empty)

Não é válido, pois está transportando uma carga com peso superior ao suportado
por uma locomotiva.

Composições podem ser formadas por mais de uma locomotiva, como o exemplo a seguir:

> sample :: Train Cargo
> sample = Locomotive 100 (Wagon (Products 90)
>         (Locomotive 90 (Wagon (Products 100) Empty)))

O valor `sample` é considerado válido por iniciar por atender todas as restrições.
Observe que a segunda locomotiva (que suporta uma carga de peso 90) adiciona potência
adicional ao comboio para transportar um vagão de peso 100.

De posse da descrição acima, resolva os exercícios a seguir.


Exercícios
----------

1. (Valor 2,0 pts). A função `foldr` para listas é definida como:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x : xs) = f x (foldr f v xs)
```

De maneira intuitiva, `foldr f v xs` substitui a lista vazia pelo
valor `v` e o construtor `:` pela função `f`. Podemos definir uma
função `fold` para o tipo `Train a`. A chamada `foldTrain f v t` para
`t :: Train a`, remove o construtor `Locomotive`, substitui o
construtor `Wagon` pela função `f` e `Empty` por `v`.

Com base no apresentado, implemente a função `foldTrain` para o tipo
`Train`.


Explicação

Como dito anteriormente, a função 'foldr' aplica uma função a todos os elementos de uma lista, que no caso do exercício 1 seria o tipo recursivo 'Train', e concatena tudo novamente. Conforme solicitado no enunciado, quando o parâmetro do tipo 'Train' for uma locomotiva, ela deve ser removida, portanto chamei a funcao 'foldTrain' novamente para o proximo item. Quando o parametro 'Train' for um vagao, apliquei a funcao q foi passada ao seu campo 'Cargo' e chamei 'foldTrain' novamente para o proximo item ate que ele seja do tipo 'Empty'.

Para algumas questões, tive que criar a seção \begin{code}\end{code} e colocar os códigos que estavam na frente do símbolo '>' nessa sação, pois o compilador estava apontando erro de parse, e com a criação da seção o problema foi sanado.

\begin{code}
  foldTrain :: (a -> b -> b) -> b -> Train a -> b
  foldTrain _ v Empty = v
  foldTrain f v (Wagon c w) = c `f` (foldTrain f v w)
  foldTrain f v (Locomotive _ w) = foldTrain f v w
\end{code}

Sua implementação deve atender os seguintes casos de teste:

> question1Tests :: TestTree
> question1Tests
>   = testGroup "Question 1"
>         [
>            testCase "Test 1" $ foldTrain g [] aTrain @?= [70, 90, 110, 60]
>         ,  testCase "Test 2" $ foldTrain g [] sample @?= []
>         ]
>     where
>       g (Persons ps) ac = ps ++ ac
>       g _ ac = ac

2. (Valor 2,0 pts). A função `filter` para listas é definida como:

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
```

De maneira intuitiva, `filter p xs` retorna uma lista em que todos os
elementos satisfazem a condição expressa pelo predicado `p :: a -> Bool`.
Podemos definir uma função `filter` para o tipo `Train a`. A chamada
`filterTrain p t` retorna um valor do tipo `Train` em que todos os vagões
(construtor `Wagon`) satisfazem a condição expressa pelo predicado `p`.

Com base no apresentado, implemente a função `filterTrain`.

Explicação

Nessa questão, a função 'filterTrain' deve retornar um 'Train' contendo a locomotiva e todos os vagões que satisfazem a condição passada como parâmetro. Sendo assim, listei os casos. Quando for 'Empty', retorna 'Empty', pois não é do tipo 'Wagon'. Quando for do tipo 'Locomotive', retorna a 'Locomotive' com a chamada recursiva da função 'filterTrain' para os seus 'Wagons'. Quando for do tipo 'Wagon', analisa se ele satisfaz a condição passada como parâmetro, caso verdadeiro retorna o próprio 'Wagon' com a chamada recursiva para os seus 'Wagons', caso contrário retorna a chamada recursiva para os seus 'Wagons'. Nessa questão, o aluno Carlos Gabriel (19.1.4009) foi consultado, ele ajudou na lógica do caso de o parâmetro 'Train a' ser do tipo 'Locomotive'.

\begin{code}

  filterTrain :: (a -> Bool) -> Train a -> Train a
  filterTrain _ Empty = Empty
  filterTrain p (Locomotive peso w) = Locomotive peso (filterTrain p w)
  filterTrain p (Wagon c w) = if (p c) then (Wagon c (filterTrain p w)) else filterTrain p w

\end{code}

Sua implementação deve atender os seguintes casos de teste:

\begin{code}
  question2Tests :: TestTree
  question2Tests
    = testGroup "Question 2"
          [
            testCase "Test 1" $ filterTrain p aTrain @?= aTrain'
          , testCase "Test 2" $ filterTrain p sample @?= sample
          ]
    where
      p (Products _) = True
      p _            = False
      aTrain' =  Locomotive 1000 w1'
      w1' = Wagon (Products 100) w3
\end{code}


3. (Valor 2,0 pts). Termine a implementação da função

Explicação

Nessa questão, o aluno gabriel Mace (19.1.4013) colaborou no pensamento da lógica dos casos da função 'step', pois os casos originalmente listados não estavam retornando os valores desejados pelos testes. Essa função chama a função 'foldTrain' para o parâmetro do tipo 'Train Cargo' executando a função 'step' para cada um dos vagões. A função 'step' verifica o se o campo 'Cargo' do parâmetro 'Train' é do tipo 'Persons' ou 'Products', caso seja do tipo 'Persons' ela retorna a soma do acumulador com os valores presentes na '[Weight]' e caso seja do tipo 'Products' retorna a soma do acumulador com o 'Weight'. No caso base retorna 'base' que é 0.

\begin{code}
  weight :: Train Cargo -> Weight
  weight = foldTrain step base
    where
      step (Persons xs) acc = (sum xs) + acc
      step (Products x) acc = x + acc
      step NoCargo acc = acc
      base = 0
\end{code}

que calcula o peso total transportado por um comboio ferroviário
representado por um valor de tipo `Train Cargo`. Sua solução deve
apenas prover a implementação das funções `step` e `base` de
maneira apropriada.

Sua implementação deve atender os seguintes casos de teste:

\begin{code}
  question3Tests :: TestTree
  question3Tests
    = testGroup "Question 3"
          [
            testCase "Test 1" $ weight aTrain @?= 630
          , testCase "Test 2" $ weight sample @?= 190
          ]
\end{code}

4. (Valor 2,0 pts). A função `map` para listas é definida como:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs
```

De maneira intuitiva, `map f xs` aplica a função `f` a cada um
dos elementos da lista `xs`. Podemos definir a função `map` para
valores do tipo `Train` de forma similar. A chamada `mapTrain f t`
aplicará a função `f` a cada um dos valores de tipo `a` armazenados
pelo construtor `Wagon` em um valor de tipo `Train a`.

Com base no apresentado, implemente a função `mapTrain`:

Explicação

Essa função aplica uma função para todos os itens presentes no parâmetro 'Train'. Primeiramente foi listado todos os casos para o tipo 'Train'. Para 'Empty' foi retornado apenas o 'Empty'. Para 'Locomotive' foi retornado a própria locomotiva com a chamada recursiva para os seus 'Wagons'. Para 'Wagon' foi aplicada a função passada como parâmetro em seu 'Cargo' e aplicada a chamada recursiva da função em seus 'Wagons'.

> mapTrain :: (a -> b) -> Train a -> Train b
> mapTrain _ Empty = Empty
> mapTrain f (Locomotive peso w) = Locomotive peso (mapTrain f w)
> mapTrain f (Wagon c w) = (Wagon (f c) (mapTrain f w))

Sua implementação deve atender os seguintes casos de teste:

> question4Tests :: TestTree
> question4Tests
>   = testGroup "Question 4"
>         [
>           testCase "Test 1" $ mapTrain f aTrain @?= aTrain'
>         , testCase "Test 2" $ mapTrain f sample @?= sample
>         ]
>         where
>           f (Persons _) = Persons []
>           f x           = x
>           aTrain' =  Locomotive 1000 w1'
>           w1' = Wagon (Products 100) w2'
>           w2' = Wagon (Persons []) w3


5. (Valor 2,0 pts). Implemente a função:

Explicação

Para a resolução dessa questão, ela foi dividida em duas etapas. A primeira parte seria somar os pesos dos itens do tipo 'Cargo' para setar a capacidade da locomotiva, para isso foi criada uma função que recebe a '[Cargo]' e a percorre somando seus 'Weight' de acordo com o seu tipo ('Products', 'Persosn', 'NoCargo'). Na segunda parte deveria-se criar os 'Wagons' da locomotiva baseado na '[Cargo]', para isso foi criada uma função 'f' que recebe '[Cargo]' e vai criando 'Wagons' recursivamente para todos os itens da '[Cargo]' e colocando eles na locomotiva.

\begin{code}
  buildTrain :: [Cargo] -> Train Cargo
  buildTrain i = Locomotive (pesoTotal i) (f i)
    where
      pesoTotal [] = 0
      pesoTotal (NoCargo:xs) = 0 + (pesoTotal xs)
      pesoTotal ((Products p):xs) = p + (pesoTotal xs)
      pesoTotal ((Persons ys):xs) = (sum ys) + (pesoTotal xs)
      f [] = Empty
      f (x:xs) = Wagon x (f xs)
\end{code}

Que a partir de uma lista de itens a serem transportados,
retorna um valor do tipo `Train` que
representa o comboio ferroviário que transporta toda a carga presente
na lista fornecida como segundo argumento. É importante notar que
o comboio deve iniciar com uma locomotiva que possui como peso máximo
suportado o valor da soma do peso de todos os itens levados em seus
vagões.

Sua implementação deve atender os seguintes casos de teste:

\begin{code}
  itens :: [Cargo]
  itens = [Persons [10], Products 2000, Products 1000]

  question5Tests :: TestTree
  question5Tests
    = testGroup "Question 5"
          [
            testCase "Test 1" $ buildTrain itens @?= train1
          ]
          where
            train1 = Locomotive 3010 w1'
            w1' = Wagon (Persons [10]) w2'
            w2' = Wagon (Products 2000) w3'
            w3'  = Wagon (Products 1000) Empty
\end{code}
