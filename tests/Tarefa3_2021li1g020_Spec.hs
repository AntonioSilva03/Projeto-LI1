module Tarefa3_2021li1g020_Spec where

import Fixtures
import Tarefa3_2021li1g020
import Test.HUnit

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=? show m1e1,
      "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=? show m1e2,
      "Tarefa 3 - Teste Imprime Jogo m2e1" ~: "       \n       \nP>    X\nXXC X X\nXXXXXXX" ~=? show m2e1,
      "Tarefa 3 - Teste Imprime Jogo m2e2" ~: "       \n       \nP   > X\nXXC X X\nXXXXXXX" ~=? show m2e2
    ]