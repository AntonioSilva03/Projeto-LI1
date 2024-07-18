-- |
-- Module      : Tarefa2_2021li1g020
-- Description : Construção/Desconstrução do mapa
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
module Tarefa2_2021li1g020 where

import LI12122
import Tarefa1_2021li1g020

-- | A partir da lista de peças e coordenadas, vai construir a linha constituida pelos blocos, caixas, possível porta e, se o objeto não estiver identificado, e ainda não tiver chegado ao fim da lista, o Vazio.
constroiLinha :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> [Peca]
constroiLinha _ _ _ 0 = []
constroiLinha ps l currX mX
  | (Bloco, (currX, l)) `elem` ps = Bloco : constroiLinha ps l (currX + 1) (mX - 1)
  | (Caixa, (currX, l)) `elem` ps = Caixa : constroiLinha ps l (currX + 1) (mX - 1)
  | (Porta, (currX, l)) `elem` ps = Porta : constroiLinha ps l (currX + 1) (mX - 1)
  | (Vazio, (currX, l)) `elem` ps = Vazio : constroiLinha ps l (currX + 1) (mX - 1)
  | otherwise = Vazio : constroiLinha ps l (currX + 1) (mX - 1)

-- | Pegando no constroiLinha vai diminuindo o y (mudar de linha) até chegar ao 0
constroiColunas :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> Mapa
constroiColunas _ _ _ 0 = []
constroiColunas ps mX currY mY = constroiLinha ps currY 0 mX : constroiColunas ps mX (currY + 1) (mY - 1)

-- | Se o conjunto de Peças e Coordenadas for válido para fazer um mapa, então este pegará no constroiColunas e começando no maior X e maior Y vai diminuido até chegar à posição (0,0).
--     Por esse motivo que se tem de começar adicionando 1 ao obterMaiorX e ao obterMaiorY pois este começa a contar no 0
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa l =
  if validaPotencialMapa l
    then constroiColunas l x 0 y
    else []
  where
    (x, y) = (obterMaiorX l 0 + 1, obterMaiorY l 0 + 1)

-- | O desconstroiLinha pega numa lista de Peças e de acordo com a sua posição adiciona-lhe umas coordenadas e no caso de ser Vazio, não devolve nada
desconstroiLinha :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiLinha [] _ _ = []
desconstroiLinha (x : xs) xi yi
  | x == Bloco = (Bloco, (xi, yi)) : desconstroiLinha xs (xi + 1) yi
  | x == Caixa = (Caixa, (xi, yi)) : desconstroiLinha xs (xi + 1) yi
  | x == Porta = (Porta, (xi, yi)) : desconstroiLinha xs (xi + 1) yi
  | x == Vazio = desconstroiLinha xs (xi + 1) yi
  | otherwise = desconstroiLinha xs (xi + 1) yi

-- | O desconstroiColuna vai pegar num Mapa e com o desconstroiLinha vai desconstruir todas as linhas de y==0 ate ao mapa ficar vazio e se tenha uma lista completa de Peças e Coordenadas
desconstroiColuna :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiColuna [] _ = []
desconstroiColuna (x : xs) yi = desconstroiLinha x 0 yi ++ desconstroiColuna xs (yi + 1)

-- | O desconstroiMapa simplesmente pega no desconstroiColuna e começa no y==0
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiColuna mapa 0
