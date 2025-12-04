# Linguagens de Programação - Trabalho Final

<b>Doscente: </b> Samuel Feitosa

<b>Discentes: </b> Gabriel Pietro de Souze e Julya Brustolin Marssona
----

## Sobre o trabalho

### Descrição
Funcionalidades possíveis: tipos de dados algébricos, uso de variáveis mutáveis (estado), exceções, listas, tuplas, registros, etc.

Variações: implementar um compilador do lambda cálculo para a linguagem C ou para alguma outra representação intermediária; Implementar uma linguagem diferente como IMP, Featherweight Java, etc.

### Implementação
Foi selecionado para implementação o uso de variáveis mutáveis. Para isso, foi uso de variáveis mutáveis (referências). Para isso, foi necessário estender o modelo semântico operacional do cálculo lambda, que tradicionalmente é puramente funcional, para incluir e gerenciar efeitos colaterais de memória.

Para realizar a implementação, foi utilizado como base o capítulo 13 do livro "Types and Programming Languages". As etapas foram:

    1- Inclusão Explícita do Estado: O conceito de estado da máquina abstrata foi expandido. Ele não é mais representado apenas pelo termo (t) sendo avaliado, mas sim pelo par (termo, armazenamento) (t ∣ μ). Consequentemente, a relação de avaliação de um passo deve ser alterada para t ∣ μ → t' μ'

    2- Definição do Store (μ): Foi preciso definir o armazenamento (μ) como uma função parcial que mapeia localizações (l) (ponteiros abstratos) para valores (v). Foi utilizado um mapa de valores para realizer estudo.

    3- Extensão da Sintaxe: A sintaxe da linguagem precisou ser estendida para incluir os novos termos e valores relacionados à mutabilidade. (ref, :=, ! e outros)

    4- Implementação das Regras de Mutação: E-REFV, E-DEREFOC, E-ASSING - conforme lógica especifidada no capítulo do livro. 


### Dificuldades
Transcrição da lógica para código. Para realizar a transcrição correta, foi necessário entender como ler a lógica proposta e compreender como ela funciona. Como exemplo: 

E-DEREF: "to evaluate a dereferencing expression !t1, we must first reduce t1 until it becomes a value: "
``` 

Premisa -> t1 | µ -→ t1'| µ'

Conclusão: !t1 | µ -→ !t1'| µ'
```
Ou seja, enquanto t1 não for um valor devemos reduzir t1 e o seu respectivo armazenamento até que ele vire uma "localização".

Para implementação, foi utilizado o conceito de "pattern matching", assim foi possível criar apenas apenas uma função que atende diversas variações. Além disso foi utilizada guarda com a função "isValid" que foi implementada em aula - para esse caso em especifico, foi apenas incrementada com as condições necessárias. Também foi utilizado recursão para garantir que "a expressão será reduzida até que vire um valor", como no exemplo acima.


 

