module Tarefa4_2021li1g020_Spec where

import Fixtures
import LI12122
import Tarefa3_2021li1g020
import Tarefa4_2021li1g020
import Test.HUnit

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=? moveJogador m1e1 AndarEsquerda,
      "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=? moveJogador m1e1 AndarDireita,
      "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar,
      "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=? moveJogador m1e1 InterageCaixa,
      "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=? correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda],
      "Tarefa 4 - Teste movimentos m1e2 Caixa1"
        ~: Jogo
          [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
          ]
          (Jogador (3, 3) Este True)
        ~=? correrMovimentos m1e2 [AndarDireita, InterageCaixa],
      "Tarefa 4 - Teste movimentos m1e2 Caixa2"
        ~: Jogo
          [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
          ]
          (Jogador (2, 3) Oeste False)
          ~=? correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa],
      "Tarefa 4 - Teste movimentos m1e2 Trepar"
        ~: Jogo
          [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
            [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
          ]
          (Jogador (4, 2) Este False)
        ~=? correrMovimentos m1e2 [AndarDireita, Trepar],
      "Tarefa 4 - Teste movimentos m1e2 Trepar Caixa1"
        ~: Jogo
          [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
            [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
          ]
          (Jogador (5, 3) Oeste True)
        ~=? correrMovimentos m1e2 [AndarDireita, Trepar, AndarDireita, AndarEsquerda, InterageCaixa]
    ]