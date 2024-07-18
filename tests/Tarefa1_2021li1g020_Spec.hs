module Tarefa1_2021li1g020_Spec where

import Fixtures
import LI12122
import Tarefa1_2021li1g020
import Test.HUnit

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True,
      "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False,
      "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0, 0)), (Porta, (1, 0))] ~=? False,
      "Tarefa 1 - Teste Valida Mapa com sobreposição" ~: validaPotencialMapa [(Bloco, (0, 0)), (Caixa, (0, 0)), (Porta, (2, 2))] ~=? False,
      "Tarefa 1 - Teste Valida Mapa com caixa sem suporte" ~: validaPotencialMapa [(Caixa, (1, 3)), (Bloco, (1, 1)), (Porta, (1, 2))] ~=? False,
      "Tarefa 1 - Teste Valida Mapa sem espaços vazios" ~: validaPotencialMapa [(Porta, (0, 0)), (Bloco, (1, 0)), (Caixa, (1, 1)), (Bloco, (0, 1))] ~=? False
    ]
