{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      : Tarefa5_2021li1g020
-- Description : Renderiza gráficamente um nível
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
module Tarefa5_2021li1g020 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Niveis
import Tarefa4_2021li1g020

-- Tipos de Data
type Estado = (Jogo, ([Picture], Float))

-- Constantes
pixelSize :: Float
pixelSize = 45

frameRate :: Int
frameRate = 60

-- O estado é determinado pelo jogo atual
-- Desenha-se um estado da seguinte forma:
-- 1. Começa-se por desenhar cada linha, uma a uma;
-- 2. De seguida, junta-se as linhas todas, dando uma lista de Picture, que formam o desenho do mapa;
-- 3. Por fim, junta-se os objetos Picture todos, com a função Pictures, dando assim a imagem final do estado do jogo.
-- Nota: [Picture] é composta da seguinte forma: [jogadorEsteImg, jogadorOesteImg, jogadorEsteCaixaImg, jogadorOesteCaixaImg, blocoImg, caixaImg, portaImg, menuImg]
estadoInicial :: [Picture] -> Estado
estadoInicial imgs = (menu, (imgs, 0.0))

-- | Função que desenha as peças todas do mapa
desenharLinha :: [Peca] -> Float -> Float -> Coordenadas -> Direcao -> Bool -> [Picture] -> [Picture]
desenharLinha [] _ _ _ _ _ _ = []
desenharLinha (p : ps) x y pCoords dir caixa imgs
  | (round x, round y) == pCoords =
    case dir of
      Este ->
        if caixa
          then Translate translateX translateY jogadorEsteCaixaImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
          else Translate translateX translateY jogadorEsteImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
      Oeste ->
        if caixa
          then Translate translateX translateY jogadorOesteCaixaImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
          else Translate translateX translateY jogadorOesteImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
  | p == Bloco = Translate translateX translateY blocoImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
  | p == Caixa = Translate translateX translateY caixaImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
  | p == Porta = Translate translateX translateY portaImg : desenharLinha ps (x + 1) y pCoords dir caixa imgs
  | p == Vazio = desenharLinha ps (x + 1) y pCoords dir caixa imgs
  where
    -- Constantes
    translateX = pixelSize * (x + 0.5)
    translateY = - pixelSize * (y - 0.5)
    jogadorEsteImg = head imgs
    jogadorOesteImg = imgs !! 1
    jogadorEsteCaixaImg = imgs !! 2
    jogadorOesteCaixaImg = imgs !! 3
    blocoImg = imgs !! 4
    caixaImg = imgs !! 5
    portaImg = imgs !! 6

-- | Função que pega na função anterior e cria o mapa todo
desenharMapa :: Mapa -> Float -> Coordenadas -> Direcao -> Bool -> [Picture] -> [Picture]
desenharMapa [] _ _ _ _ _ = []
desenharMapa (l : ls) n pCoords dir caixa imgs = desenharLinha l 0 n pCoords dir caixa imgs ++ desenharMapa ls (n + 1) pCoords dir caixa imgs

-- | De acordo com o Estado, mostra o mapa ou menu
desenharEstado :: Estado -> Picture
desenharEstado (Jogo mapa (Jogador pCoords dir caixa), (imgs, float)) =
  if mapa == []
    then imgs !! 7
    else Translate (- xMult * pixelSize) (yMult * pixelSize) (Pictures (desenharMapa mapa 0 pCoords dir caixa imgs))
  where
    xMult = if even (length (head mapa)) then fromIntegral (length (head mapa) `div` 2) else fromIntegral (length (head mapa) `div` 2) + 0.5
    yMult = if even (length mapa) then fromIntegral (length mapa `div` 2 - 1) else fromIntegral (length mapa `div` 2) - 0.5

-- A função que trata da lógica dos eventos
-- Utiliza as funções previamente definidas na Tarefa4 para movimentar o Jogador conforme os diferentes eventos
-- Devolve um novo jogo a cada evento, para depois ser representado um novo estado
eventos :: Event -> Estado -> Estado
eventos (EventKey (SpecialKey KeyUp) Down _ _) (jogo, (imgs, e)) = (moveJogador jogo Trepar, (imgs, e))
eventos (EventKey (SpecialKey KeyDown) Down _ _) (jogo, (imgs, e)) = (moveJogador jogo InterageCaixa, (imgs, e))
eventos (EventKey (SpecialKey KeyLeft) Down _ _) (jogo, (imgs, e)) = (moveJogador jogo AndarEsquerda, (imgs, e))
eventos (EventKey (SpecialKey KeyRight) Down _ _) (jogo, (imgs, e)) = (moveJogador jogo AndarDireita, (imgs, e))
eventos (EventKey (Char s) Down _ _) (jogo, (imgs, e)) = (jogo1, (imgs, e)) -- Começa o jogo
eventos (EventKey (Char r) Down _ _) (jogo, (imgs, e)) = (jogo, (imgs, e)) -- Reinicia o mapa
eventos _ estado = estado -- ignora qualquer outro evento

tempo :: Float -> Estado -> Estado
tempo n estado = estado

-- Janela onde o jogo será representado
dm :: Display
dm = InWindow "MINEDUDE" (sizeX + margin, sizeY + margin) (100, 100)
  where
    -- Constantes
    margin = 50 -- margem entre o mapa e as bordas da janela
    sizeX = 1000
    sizeY = 500
