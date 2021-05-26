---
author: Programação Funcional
title: Live de dúvidas e resolução de exercícios.
date: Prof. Rodrigo Ribeiro
---

Introdução
==========

Os exercícios a seguir envolvem o conteúdo
de tipos de dados algébricos e classes de
tipos.

> module Main where

Essa função é utilizada como marcação para
as definições que devem ser implementadas
por você.

> tODO :: a
> tODO = undefined


> main :: IO ()
> main = return ()

Números Naturais
----------------

O conjunto dos números naturais pode ser
gerado recursivamente usando as seguintes
regras:

* zero é um número natural

* se n é um número natural então o sucessor
de n é um número natural.

Dessa forma, podemos representar o conjunto
de números naturais da seguinte forma:

Nat = {zero, suc zero, suc (suc zero), ...}

que corresponde ao conunto N:

N   = { 0  ,  1      , 2             , ... }

A partir do apresentado, faça o que se pede:

1. Apresente um tipo de dados algébrico para
representar números naturais de acordo com a
definição recursiva descrita.

> data Nat = TODO

2. Implemente a função

> fromNat :: Nat -> Int
> fromNat = tODO

que converte um valor do tipo Nat no número
inteiro correspondente.

3. Implemente a função

> toNat :: Int -> Maybe Nat
> toNat = tODO

que converte um inteiro no valor do tipo Nat
correspondente. Caso o inteiro fornecido seja
negativo, retorne o valor Nothing.

4. Defina a operação de adição sobre
valores do tipo Nat:

> (.+.) :: Nat -> Nat -> Nat
> _ .+. _ = tODO

Sua função deve ser definida por recursão
sobre a estrutura do primeiro parâmetro
de tipo Nat.

5. Defina a operação de multiplicação
sobre valores de tipo Nat:

> (.*.) :: Nat -> Nat -> Nat
> _ .*. _ = tODO

6. Usando o tipo Nat, podemos definir
uma função de ordem superior que funciona
como fold para o tipo Nat. Implemente a função

> rec :: a -> (Nat -> a -> a) -> Nat -> a
> rec v ih _ = tODO
> rec v ih _ = tODO

que codifica o princípio de indução para
números naturais. Note que quando o número
natural fornecido como terceiro parâmetro
for igual a zero, devemos retornar como
resultado o primeiro parâmetro. Quando o
terceiro parâmetro for sucessor de um
número n', devemos construir o resultado
final usando o segundo parâmetro e o
resultado da chamada recursiva para rec.

7. Usando a função rec, apresente uma
definição alternativa para a adição:

> (.++.) :: Nat -> Nat -> Nat
> n .++. m = tODO

8. Defina a função de subtração sobre o
tipo Nat. Adote, como convenção que quando
o primeiro parâmetro for menor que o segundo
(isto é, o resultado será um número negativo),
sua função deverá retornar zero.

> (.-.) :: Nat -> Nat -> Nat
> n .-. m = tODO

9. A classe Num presente na biblioteca Prelude
possui a seguinte definição:

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

Apresente uma instância da classe Num para o
tipo Nat. Com isso, você será capaz de escrever
números arábicos para denotar valores do tipo Nat.
