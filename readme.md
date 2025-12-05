# Linguagens de Programação - Trabalho Final

### Informações Gerais
<b>Instituição: </b>Universidade Federal da Fronteira Sul, Campus Chapecó - SC.\
<b>Disciplina: </b>Linguagens de Programação - Turma 2025/2.\
<b>Doscente: </b> Samuel Feitosa \<samuel.feitosa@uffs.edu.br\> \
<b>Discentes: </b> 
- Gabriel Pietro de Souza \<gabriel.souza@estudante.uffs.edu.br\> - 2221101067
- Julya Brustolin Marssona \<julya.marssona@estudante.uffs.edu.br\> - 2221101068

----

### Objetivo
O presente trabalho se propõe a apresentar uma nova linguagem de programação, realizada no Componente Curricular supracitado, baseada em Haskell. 

As principais funcionalidades foram implementadas no decorrer das aulas da disciplina, onde o papel do trabalho final é a finalização de algumas operações e a implementação de uma nova funcionalidade na linguagem onde os estudantes escolheram criar o novo tipo de dados "Lista" e suas operações, com base no capítulo 11.12 do livro proposto [Types and Programming Languages](./docs/Types%20and%20Programming%20Languages.pdf).

### Dependências
Certifique-se de possuir o [Stack](https://docs.haskellstack.org/en/stable/) para o Haskell no seu Sistema Operacional.

### Como gerar o executável da linguagem?
Para buildar o nosso projeto a fim de transformar o código da linguagem em uma linguagem executável de fato, abra um prompt de comando na raiz do projeto e rode o seguinte comando:
```bash
stack build
```
Se necessário configure o projeto no arquivo [package.yml](./package.yaml) e execute o comando de build novamente, observando o executável gerado.

### Como executar?
Abra um novo prompt de comando, ou continue no aberto anteriormente, e insira o comando abaixo:
```bash
stack exec ./src/Main.exe < input.txt
```
Pode existir algumas variações de acordo com o SO utilizado, pode ser necessário re-gerar o executável da linguagem.

### Como o ambiente foi gerado?
Inicialmente foi necessário criar o arquivo package.yaml para configurar o nosso projeto, usado pelo HPack para gerar o arquivo .cabal (lido pelo ghc). Após isso, inicializamos o ambiente do projeto com o stack através do comando:
```bash
stack init
```

### Exemplos de inputs da linguagem
É possível encontrar os tokens disponíveis para a linguagem no arquivo [Lexer.hs](./src/Lexer.hs) e perceber a sintaxe da linguagem no [Parser.y](./src/Parser.y) com a estrutura das expressões. Para executar diferentes expressões na linguagem, altere o arquivo [input.txt](./input.txt) e acrescente algum dos exemplos abaixo ou gere uma expressão de acordo com a sintaxe da linguagem.

- Execução de uma operação matemática
    ```txt
    5 + 2 * (if true 10 20)
    ```
- Verificação booleana de uma expressão
    ```txt
    if (true && (isnil [numeric] (nil [numeric]))) true false
    ```
- Verificação de uma lista vazia
    ```txt
    isnil [numeric] (nil [numeric])
    ```
- Criação de uma lista, com 2 elementos booleanos:
    ```txt
    listcons [bool] true (listcons [bool] false (nil [bool]))
    ```
- Obtenção da cabeça de uma lista (head list):
    ```txt
    head [numeric] (listcons [numeric] 2 (nil [numeric]))
    ```
- Obtenção da calda de uma lista (tail list):
    ```txt
    tail [numeric] (listcons [numeric] 2 (nil [numeric]))
    ```


### Descrição
O [Trabalho Final](./docs/CC_-_LP_-_Trabalho_Final_2025-2.pdf) da disciplina de Linguagens de Programação, turma 2025-2, do curso de Ciência da Computação da UFFS - Campus Chapecó, consiste na escolha de uma funcionalidade para ser implementada no Interpretador do Lambda Cálculo desenvolvido em sala, ou em alguma variação. Este trabalho deve ser desenvolvido em duplas ou trios.

Funcionalidades possíveis: tipos de dados algébricos, uso de variáveis mutáveis (estado), exceções, listas, tuplas, registros, etc.

Variações: implementar um compilador do lambda cálculo para a linguagem C ou para alguma outra representação intermediária; Implementar uma linguagem diferente como IMP, Featherweight Java, etc.

### Implementação
A implementação escolhida dentre as permitidas foi o novo tipo de dados de "Lista" e suas operações.

Para realizar a implementação, foi utilizado como base o capítulo 11.12 do livro "Types and Programming Languages". As etapas foram:
- 0 - Inicialmente resolvemos todos os comentários (funcionalidades) solicitadas no código original.
- 1 - Criamos novos tokens descritos na forma sintatica como TokenNil, TokenListCons, TokenIsNil, TokenHead, TokenTail, TokenLSquare, TokenRSquare
    para as operações de Lista no arquivo Lexer.hs.
- 2 - Criamos novos tokens TokenKWNum, TokenKWBool para identificar os tipos possiveis de uma lista no arquivo Lexer.hs.
- 3 - Adicionamos um mapeamento de entradas do usuário para os tokens no arquivo Lexer.hs.
- 4 - Adicionamos o mesmo mapeamento de entradas nos tokens descrito no Parser.y.
- 5 - Criamos uma tipagem auxiliar para englobar os tipos que - lista pode ter, Ty -> TNum | TBool, no Parser.y.
- 6 - Criamos as expressões de mapeamento das operações de uma lista no Parser.y.
- 7 - Geramos um novo Parser.hs utilizando a ferramenta "happy".
- 8 - Criamos as implementações de validação de tipos para as operações de uma lista, descritas no livro, no TypeChecker.hs
- 9 - Ajustamos a função "isValue" para que a redução de uma lista seja considerado um valor válido no Interpreter.hs
- 10 - Ajustamos a função "subst" para desestruturar todas as operações de uma lista no Interpreter.hs
- 11 - Criamos os passos necessários para executar as operações uma lista através do "eval", no Interpreter.hs

### Dificuldades
Transcrição da lógica para código. Para realizar a transcrição correta, foi necessário entender como ler a lógica proposta no livro mencionado e compreender como ela funciona. 

### Conclusão
Para implementação, foi utilizado o conceito de "pattern matching", assim foi possível criar apenas apenas uma função que atende diversas variações. Além disso foi utilizada guarda com a função "isValid" que foi implementada em aula.

---