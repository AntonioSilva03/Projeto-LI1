-- |
-- Module      : Tarefa1_2021li1g020
-- Description : Validação de um potencial mapa
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
module Tarefa1_2021li1g020 where

import LI12122

-- | O verificarCoordenadas vê se não existem duas peças com as mesmas coordenadas, o que não pode ser possível
verificarCoordenadas :: [(Peca, Coordenadas)] -> Coordenadas -> Bool
verificarCoordenadas [] _ = True
verificarCoordenadas (p : ps) coords = (coords `notElem` p) && verificarCoordenadas ps coords

-- | O verificarPorta procura as portas todas do mapa e se só existir uma, então o mapa poderá ser válido
verificarPorta :: [(Peca, Coordenadas)] -> Int
verificarPorta [] = 0
verificarPorta (p : ps) =
  if fst p == Porta
    then 1 + verificarPorta ps
    else verificarPorta ps

-- | O obterSuportes pega em todas as caixas e obtem o as coordenadas do objeto que poderá estar debaixo delas
obterSuportes :: [(Peca, Coordenadas)] -> [Coordenadas]
obterSuportes [] = []
obterSuportes ((peca, (x, y)) : ps) = if peca == Caixa then (x, y + 1) : obterSuportes ps else obterSuportes ps

-- | A verificarSuportes vai verificar a existência de um bloco ou caixa na lista de coordenadas obtida na função anterior
verificarSuportes :: [(Peca, Coordenadas)] -> [Coordenadas] -> Bool
verificarSuportes l [] = True
verificarSuportes l (c : cs) =
  if (Bloco, c) `elem` l || (Caixa, c) `elem` l
    then verificarSuportes l cs
    else False

-- | Utilizando as funções obterSuportes e verificarSuportes vai procurar a existência de caixas flutuantes, que não podem exisitr
verificarFlutuantes :: [(Peca, Coordenadas)] -> Bool
verificarFlutuantes l = verificarSuportes l (obterSuportes l)

-- | O obterMaiorX vai procurar a peça com o maior X
obterMaiorX :: [(Peca, Coordenadas)] -> Int -> Int
obterMaiorX [] mX = mX
obterMaiorX ((_, (x, _)) : ps) mX = if x > mX then obterMaiorX ps x else obterMaiorX ps mX

-- | O obterMaiorY vai procurar a peça com o maior Y
obterMaiorY :: [(Peca, Coordenadas)] -> Int -> Int
obterMaiorY [] mY = mY
obterMaiorY ((_, (_, y)) : ps) mY = if y > mY then obterMaiorY ps y else obterMaiorY ps mY

-- | O obterEspaços vai medir a área desta matriz
obterEspacos :: Coordenadas -> Int
obterEspacos (x, y) = (x + 1) * (y + 1)

-- | O verificarVazio vai ver se existe algum Vazio no mapa inteiro, se não existir o mapa não pode válido
verificarVazio :: [(Peca, Coordenadas)] -> Coordenadas -> Bool
verificarVazio [] _ = False
verificarVazio l@(p : ps) (x, y) = (fst p == Vazio) || length l < obterEspacos (x, y) || verificarVazio ps (x, y)

-- | Verifica se o mapa tem uma base composta simplesmente por blocos
verificarBase :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> Int -> Bool
verificarBase l currX currY mX mY =
  if currX < mX
    then
      if currY < mY
        then
          if (Bloco, (currX, currY)) `elem` l
            then verificarBase l (currX + 1) 0 mX mY
            else verificarBase l currX (currY + 1) mX mY
        else False
    else True

-- |
--
--    == FUNÇÃO FINAL:

-- | Função final na qual juntamos todas as funções que irão ver se a lista de Peças e Coordenadas poderia ser um Mapa
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l@(p : ps) =
  verificarCoordenadas ps (snd p)
    && verificarPorta l == 1
    && verificarFlutuantes l
    && verificarVazio l (x, y)
    && verificarBase l 0 0 x y
  where
    (x, y) = (obterMaiorX l 0, obterMaiorY l 0)
