# Laboratórios de Informática I

## Repositório

O sistema de controlo de versões utilizado é o git. O repositório encontra-se disponível [nesta organização](https://gitlab.com/uminho-di/li1/2122). Para obter o repositório na sua máquina, garanta que tem a chave pública SSH adicionada na sua conta do GitLab com o email instituicional ([User Settings/SSH Keys](https://gitlab.com/-/profile/keys)), depois basta efetuar clone ao repositório.

```bash
$ git clone git@gitlab.com:uminho-di/li1/2122/2021li1g020.git
$ cd 2021li1g020 
```

## Compilação

Com o uso do ghc [GHC](https://www.haskell.org/ghc/), o jogo pode ser compilado num executável da seguinte forma.

```bash
# Dentro do diretório "src/", faça:
$ ghc -i="components" Main.hs
$ ./Main # Executa o jogo
```


## Testes

O projecto contém testes unitários escritos usando a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit). Os testes podem ser executados da seguinte forma.

```bash
$ ghci -i="src" -i="tests" tests/Tests.hs
>>> runTestsT1 -- Correr os testes tarefa 1
>>> runTestsT2 -- Correr os testes tarefa 2
>>> runTestsT3 -- Correr os testes tarefa 3
>>> runTestsT4 -- Correr os testes tarefa 4
>>> runAllTests -- Correr todos os testes
```

## Grupo 20

- **A100612** José Rodrigo Ferreira Matos;
- **A100533** António Filipe Castro Silva;
