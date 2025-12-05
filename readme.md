# Linguagens de Programação - Trabalho Final

<b>Doscente: </b> Samuel Feitosa

<b>Discentes: </b> Gabriel Pietro de Souze e Julya Brustolin Marssona

----

### Descrição
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
- 5 - Criamos uma tipagem auxiliar para englobar os tipos que - lista pode ter, BaseListTy -> TNum | TBool, no Parser.y.
- 6 - Criamos as expressões de mapeamento das operações de uma lista no Parser.y.
- 7 - Geramos um novo Parser.hs utilizando a ferramenta "happy".
- 8 - Criamos as implementações de validação de tipos para as operações de uma lista, descritas no livro, no TypeChecker.hs
- 9 - Ajustamos a função "isValue" para que a redução de uma lista seja considerado um valor válido no Interpreter.hs
- 10 - Ajustamos a função "subst" para desestruturar todas as operações de uma lista no Interpreter.hs
- 11 - Criamos os passos necessários para executar as operações uma lista através do "eval", além do passo auxiliar para computar resultados sem o uso de lambda, no Interpreter.hs

### Dificuldades
Transcrição da lógica para código. Para realizar a transcrição correta, foi necessário entender como ler a lógica proposta no livro mencionado e compreender como ela funciona. 

### Conclusão
Para implementação, foi utilizado o conceito de "pattern matching", assim foi possível criar apenas apenas uma função que atende diversas variações. Além disso foi utilizada guarda com a função "isValid" que foi implementada em aula.

---