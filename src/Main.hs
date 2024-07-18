-- |
-- Module      : Main
-- Description : O ficheiro responsável pela execução do jogo
-- Copyright   : José Rodrigo Ferreira Matos <a100612@alunos.uminho.pt>;
--               António Filipe Castro Silva <a100533@alunos.uminho.pt>;
--
-- Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
module Main where

import Graphics.Gloss
import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Tarefa5_2021li1g020
import Graphics.Gloss.Interface.Pure.Simulate (makeColor)

main :: IO ()
main =
  do
    jogadorEsteImg <- loadBMP "./images/jogadorEste.bmp"
    jogadorOesteImg <- loadBMP "./images/jogadorOeste.bmp"
    jogadorEsteCaixaImg <- loadBMP "./images/jogadorEsteCaixa.bmp"
    jogadorOesteCaixaImg <- loadBMP "./images/jogadorOesteCaixa.bmp"
    blocoImg <- loadBMP "./images/bloco.bmp"
    caixaImg <- loadBMP "./images/caixa.bmp"
    portaImg <- loadBMP "./images/porta.bmp"
    -- Foi usado o seguinte website para a criação desta imagem: https://textcraft.net/
    menuImg <- loadBMP "./images/menu.bmp"
    play
      dm
      (makeColor 0.89 1 1 1)
      frameRate
      (estadoInicial [jogadorEsteImg, jogadorOesteImg, jogadorEsteCaixaImg, jogadorOesteCaixaImg, blocoImg, caixaImg, portaImg, menuImg])
      desenharEstado
      eventos
      tempo