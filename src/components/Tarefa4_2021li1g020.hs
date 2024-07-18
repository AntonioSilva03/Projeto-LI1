{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      : Tarefa4_2021li1g020
-- Description : Movimentação do personagem
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
module Tarefa4_2021li1g020 where

import LI12122
import Niveis
import Tarefa1_2021li1g020
import Tarefa2_2021li1g020

-- | A função cair serve para ver quantos y vamos ter de aumentar até o Jogador chegar novamente a um bloco ou caixa ou quando blocos a caixa tem de cair depois de ser largado.

-- TODO: Detetar se cai numa porta
cair :: Mapa -> Coordenadas -> Int
cair mapa (x, y) =
  if mapa !! (y + 1) !! x == Vazio
    then cair mapa (x, y + 1)
    else y

-- | A função andar verifica os movimentos AndarDireita e AndarEsquerda, se estes podem acontecer ou não.
-- AndarDireita: ele não pode andar para a direita se já estiver no ponto extremo direito, se tiver um bloco ou caixa à frente dele e se ele tiver uma caixa e houver um bloco no caminho da caixa.
-- AndarEsquerda: ele não pode andar para a esquerda se já estiver no ponto extremo esquerdo, se tiver um bloco ou caixa à frente dele e se ele tiver uma caixa e houver um bloco no caminho da caixa.
andar :: Jogo -> Movimento -> Jogo
andar (Jogo mapa (Jogador (x, y) _ caixa)) direcao = case direcao of
  AndarDireita ->
    if x + 1 > length (mapa !! y) - 1
      then Jogo mapa (Jogador (x, y) Este caixa)
      else
        if caixa
          then
            if mapa !! y !! (x + 1) == Bloco || mapa !! y !! (x + 1) == Caixa || mapa !! (y - 1) !! (x + 1) == Bloco || x + 1 > length (mapa !! y) - 1
              then Jogo mapa (Jogador (x, y) Este True)
              else if (mapa !! y !! (x + 1) == Porta) || (mapa !! (cair mapa (x + 1, y) + 1) !! (x + 1) == Porta) then jogo2 else Jogo mapa (Jogador (x + 1, cair mapa (x + 1, y)) Este True)
          else
            if mapa !! y !! (x + 1) == Bloco || mapa !! y !! (x + 1) == Caixa || x + 1 > length (mapa !! y) - 1
              then Jogo mapa (Jogador (x, y) Este False)
              else if (mapa !! y !! (x + 1) == Porta) || (mapa !! (cair mapa (x + 1, y) + 1) !! (x + 1) == Porta) then jogo2 else Jogo mapa (Jogador (x + 1, cair mapa (x + 1, y)) Este False)
  AndarEsquerda ->
    if x - 1 < 0
      then Jogo mapa (Jogador (x, y) Oeste caixa)
      else
        if caixa
          then
            if mapa !! y !! (x - 1) == Bloco || mapa !! y !! (x - 1) == Caixa || mapa !! (y - 1) !! (x - 1) == Bloco || x - 1 < 0
              then Jogo mapa (Jogador (x, y) Oeste True)
              else if (mapa !! y !! (x - 1) == Porta) || (mapa !! ((cair mapa (x - 1, y)) + 1) !! (x - 1) == Porta) then jogo2 else Jogo mapa (Jogador (x - 1, cair mapa (x - 1, y)) Oeste True)
          else
            if mapa !! y !! (x - 1) == Bloco || mapa !! y !! (x - 1) == Caixa || x - 1 < 0
              then Jogo mapa (Jogador (x, y) Oeste False)
              else if (mapa !! y !! (x - 1) == Porta) || (mapa !! ((cair mapa (x - 1, y)) + 1) !! (x - 1) == Porta) then jogo2 else Jogo mapa (Jogador (x - 1, cair mapa (x - 1, y)) Oeste False)

-- | A função trepar serve para o Jogador subir o bloco/caixa para o qual está a olhar. Se não existir nenhum bloco/caixa para onde ele está a olhar então a personagem fica no mesmo sítio.
-- Se existirem dois blocos ou duas caixas empilhadas para onde ele está a olhar, ele também não pode trepar, visto que o Jogador só consegue trepar um bloco.
-- Se ele tiver uma caixa e quando o Jogador trepar, a caixa ficasse no local onde existe um bloco, então ele já não pode trepar.
trepar :: Jogo -> Jogo
trepar jogo@(Jogo mapa (Jogador (x, y) direcao caixa)) = case direcao of
  Este ->
    if caixa
      then
        if mapa !! y !! (x + 1) == Vazio || mapa !! y !! (x + 1) /= Vazio && mapa !! (y - 1) !! (x + 1) /= Vazio || mapa !! y !! (x + 1) /= Vazio && mapa !! (y - 2) !! (x + 1) /= Vazio
          then jogo
          else
            if mapa !! y !! (x + 1) == Porta
              then jogo2
              else Jogo mapa (Jogador (x + 1, y - 1) Este True)
      else
        if mapa !! y !! (x + 1) == Vazio || mapa !! y !! (x + 1) /= Vazio && mapa !! (y - 1) !! (x + 1) /= Vazio
          then jogo
          else
            if mapa !! y !! (x + 1) == Porta
              then jogo2
              else Jogo mapa (Jogador (x + 1, y - 1) Este False)
  Oeste ->
    if caixa
      then
        if mapa !! y !! (x - 1) == Vazio || mapa !! y !! (x - 1) /= Vazio && mapa !! (y - 1) !! (x - 1) /= Vazio || mapa !! y !! (x - 1) /= Vazio && mapa !! (y - 2) !! (x - 1) /= Vazio
          then jogo
          else
            if mapa !! y !! (x - 1) == Porta
              then jogo2
              else Jogo mapa (Jogador (x - 1, y - 1) Oeste True)
      else
        if mapa !! y !! (x - 1) == Vazio || mapa !! y !! (x - 1) /= Vazio && mapa !! (y - 1) !! (x - 1) /= Vazio
          then jogo
          else
            if mapa !! y !! (x - 1) == Porta
              then jogo2
              else Jogo mapa (Jogador (x - 1, y - 1) Oeste False)

-- | O manuziarCaixa simplesmente vê onde é que a caixa fica ou o que tem de ficar vazio depois de se ter interagido com ela.
manuziarCaixa :: Mapa -> Coordenadas -> Peca -> Mapa
manuziarCaixa [] _ _ = []
manuziarCaixa (l : ls) (x, y) p =
  if y == 0
    then aux l x p : ls
    else l : manuziarCaixa ls (x, y - 1) p
  where
    aux :: [Peca] -> Int -> Peca -> [Peca]
    aux [] _ _ = []
    aux (x : xs) n p
      | n == 0 = p : xs
      | otherwise = x : aux xs (n - 1) p

-- | O interageCaixa pega no manuziarCaixa e vê todos os casos possíveis: quando a personagem já tem caixa, quando consegue pegar nela, se ele está a olhar para ela e assim consegue pegar nela, ou se pode ser colocado para o sítio onde estão a olhar, ou quando blocos ela tem de cair depois de ser largada.
interageCaixa :: Jogo -> Jogo
interageCaixa jogo@(Jogo mapa (Jogador (x, y) direcao caixa)) = case direcao of
  Este ->
    if caixa
      then
        if mapa !! y !! (x + 1) == Vazio
          then Jogo (manuziarCaixa mapa (x + 1, cair mapa (x + 1, y)) Caixa) (Jogador (x, y) direcao False)
          else
            if mapa !! (y - 1) !! (x + 1) == Vazio
              then Jogo (manuziarCaixa mapa (x + 1, y - 1) Caixa) (Jogador (x, y) direcao False)
              else jogo
      else
        if mapa !! y !! (x + 1) == Caixa && mapa !! (y -1) !! (x + 1) == Vazio
          then Jogo (manuziarCaixa mapa (x + 1, y) Vazio) (Jogador (x, y) direcao True)
          else jogo
  Oeste ->
    if caixa
      then
        if mapa !! y !! (x - 1) == Vazio
          then Jogo (manuziarCaixa mapa (x - 1, cair mapa (x - 1, y)) Caixa) (Jogador (x, y) direcao False)
          else
            if mapa !! (y - 1) !! (x - 1) == Vazio
              then Jogo (manuziarCaixa mapa (x - 1, y - 1) Caixa) (Jogador (x, y) direcao False)
              else jogo
      else
        if mapa !! y !! (x - 1) == Caixa && mapa !! (y -1) !! (x - 1) == Vazio
          then Jogo (manuziarCaixa mapa (x - 1, y) Vazio) (Jogador (x, y) direcao True)
          else jogo

-- | O moveJogador simplesmente pega em todos os casos de movimento e interpreta-os de acordo com a sua função.
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo@(Jogo mapa jogador) movimento =
  if validaPotencialMapa (desconstroiMapa mapa)
    then case movimento of
      AndarEsquerda -> andar jogo movimento
      AndarDireita -> andar jogo movimento
      Trepar -> trepar jogo
      InterageCaixa -> interageCaixa jogo
    else Jogo [] (Jogador (0, 0) Este False)

-- | O correrMovimentos simplesmente pega no moveJogador e utiliza uma lista de movimentos que vê da esquerda para a direita.
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo@(Jogo mapa jogador) (x : xs) = if mapa == [] then jogo else correrMovimentos jogo (x : xs)
correrMovimentos jogo xs = foldl moveJogador jogo xs
