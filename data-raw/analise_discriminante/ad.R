#####################################################
###########     Roteiro aula prática       ##########
###########     ANÁLISE DISCRIMINANTE       #########
#####################################################

#Instalando o pacote necessário
install.packages("MASS")
#Carregando o paconte na memória do RStudio
library(MASS)
library(gtsummary)
library(dplyr)

################################################################################
########                   ANÁLISE AD Simples                             ######
################################################################################

#Importe o arquivo "Dados" e salve com o nome de "fal"

library(readxl)
fal <- read_excel("C:/Users/rafae/Dropbox/UFES/ANÁLISE MULTIVARIADA/Dados.xlsx")
View(fal)

#x1: fluxo de caixa/ total de débitos; 
#x2: receita líquida da empresa/ativos totais; 
#x3: ativos correntes / débitos correntes; 
#x4: ativos correntes/receita líquida das vendas. 
#pop: empresas que faliram são denotadas por pop = 0; 
#     as que não faliram por pop = 1.

cat('

Análise discriminante para determinar as empresas em falência/não falência

O exemplo é baseado no trabalho de Johnson (1987)
A amostra é formada por 21 empresas que faliram e 25 empresas que não faliram
As variáveis independentes são de 2 anos anos da falência das empresas. 
    ')


# Crie a tabela com as estatísticas descritivas e adicione o teste de média
tabela_resumo <- fal %>%
  select(pop, x1, x2, x3, x4) %>%  # Selecione as variáveis relevantes
  tbl_summary(by = pop,              # Agrupe pela variável categórica
              statistic = all_continuous() ~ "{mean} ({sd})",  # Estatísticas para variáveis contínuas
              digits = all_continuous() ~ 2,                   # Número de casas decimais
              missing = "no") %>%                              # Tratamento de valores ausentes
  add_n() %>%                                                  # Adicione a contagem (n)
  add_overall() %>%                                            # Adicione a coluna total
  add_p(test = all_continuous() ~ "t.test")                    # Adicione o teste de média


# Modifique a nota de rodapé número 2
tabela_resumo <- tabela_resumo %>%
  modify_footnote(
    update = starts_with("p.value") ~ "Welch Two Sample t-test between category 0 and 1"
  )

# Exiba a tabela
tabela_resumo


#----------------------------------------------------
#Estimando a função discriminante linear 

cat('
Existem várias formas de gerar as matrizes discriminantes no pacote.
Vamos utilizar duas formas: AD linear ou AD quadrática
A AD linear é utilizada quando as matrizes de COV da população 1 e população 2 são iguais.
A AD quadrática é utilizada quando as matrizes de COV da população 1 e população 2 são diferentes ')


# Instale e carregue o pacote
install.packages("heplots")
library(heplots)

# Supondo que o dataframe 'dados' tem as variáveis de interesse
resultado <- boxM(fal[, c("x1", "x2", "x3", "x4" )], fal$pop)
print(resultado)

cat('
O teste de Box-s M pode ser sensível a desvios da normalidade. 
    Se as suposições de normalidade não forem satisfeitas, considere usar testes robustos 
    ou métodos alternativos para comparar as matrizes de covariância.
    
A análise da normalidade multivariada das variáveis explicativas pode ser feito com o teste de Mardia,
mas muitos ainda utilizam somente o teste de normalidade das variáveis (não é a mesma coisa)
    
    ')

#Teste de Mardia
install.packages("MVN")
library(MVN)

resultado <- mvn(data = fal, mvnTest = "mardia")
print(resultado$multivariateNormality)

cat('
O teste de Mardia gera dois valores de estatística:

Skewness (Assimetria): Avalia a simetria dos dados. Se o valor p associado for pequeno 
(por exemplo, menor que 0,05), sugere que os dados não são simétricos.

Kurtosis (Curtose): Avalia a "pontualidade" ou "achatamento" da distribuição. Um valor p pequeno também sugere 
que os dados não seguem uma distribuição normal multivariada.

Se ambos os testes (ou um deles) rejeitarem a normalidade (valor p < 0.05), isso indica que os dados provavelmente
não seguem uma distribuição normal multivariada.
    
    ')

#ANÁLISE DE MULTICOLINEARIDADE

#MATRIZ DE CORRELAÇÃO
matriz_correlacao <- cor(fal)
print(matriz_correlacao)

#VIF
ml <- lm (pop ~ x1 + x2 + x3 + x4, data=fal)
library(car)
vif(ml)

############################################
############################################
#ESTIMAÇÃO DA ANÁLISE DISCRIMIANTE

d.l <- lda (pop ~ x1 + x2 + x3 + x4, data=fal)

#Obtd.l#Obtendo os resultados
print(d.l)
# fd(x) = 0.626866x1 + 4.446502x2 + 0.890372x3 -1.197667x4

cat(' 
1) Probabilidades à priori = Probabilidade antes da estimação e é baseado na proporção de 0 ou 1.
2) Média dos grupos = Podemos observar aquelas variáveis que tem valores médios relativamente diferentes entre si.
3) Coeficientes

Se o valor do Y estimado for < 0, classificamos como pertencente ao grupo FALÊNCIA. Se for > 1, classificamos como NÃO FALÊNCIA

O número de funções discriminantes será sempre igual o total de categorias menos 1.
Se tivéssemos 3 categorias, teríamos 2 funções discriminantes


TESTES DE SIGNIFICÂNCIA (?) EM AD:

Na análise discriminante linear (LDA) com duas categorias, ao contrário da regressão logística, os coeficientes 
discriminantes não têm testes de significância associados (como valores p) diretamente. 
A LDA é baseada em maximizar a separação entre os grupos e não utiliza uma modelagem probabilística que permita 
a geração de valores p diretamente para os coeficientes.
    
Coeficientes Discriminantes: Na LDA, os coeficientes obtidos indicam a direção e magnitude em que as variáveis 
explicativas contribuem para a discriminação entre as categorias. No entanto, esses coeficientes não têm valores 
p associados diretamente.

Função Discriminante: A função discriminante linear é usada para projetar os dados em um espaço onde a separação 
entre os grupos é maximizada.    
    
Testes de Significância em LDA:
Embora a LDA não forneça valores p para os coeficientes diretamente, você pode realizar testes adicionais para avaliar a
significância das variáveis:

Teste de Wilks Lambda: Avalia a significância global das variáveis no modelo.
Análise univariada com ANOVA ou t-test: Para testar a diferença de médias entre os grupos para cada variável 
de forma individual.    
    
    ')
library(MASS)
library(car)
print(d.l$scaling)
# Realizar um teste MANOVA
manova_model <- manova(cbind(x1, x2, x3, x4) ~ pop, data = fal)
summary(manova_model, test = "Wilks")


#----------------------------------------------------
#Fazendo predições
cat(' 
Podemos utilizar os dados, estimados para a função discriminante, para classificar outras empresas.
Podemos estimar probabilidades para outras empresas.
  
Podemos fazer isso de várias formas.
Inicialmente vamos gerar uma previsão chamada p.l, ou seja um objeto que armazena os coeficientes encontrados em d.l
      ')

## The tidyverse is an opinionated collection of R 
## packages designed for data science
install.packages("tidyverse") 
library("tidyverse")
## Package caret streamline the process for creating 
## predictive models.
install.packages("caret")
library(caret)

p.l <- d.l %>% predict(fal)
# dis - nome do objeto que possui o modelo da função 
## discriminante estimada;
# predict - função
# fal - nome do banco de dados
# predicitions - salvar as predições o objeto predictions

cat(' 
CLASS remete à classificação em grupos de cada objeto
PORTERIOR é a probabilidade à posteriori, após a estimação.
X Valor da função discriminante para os dados utiliziados para a predição

Neste primeiro exemplo, usamos os mesmos dados para predizer os coeficientes e para classificar, mas existem outros métodos.  

No nosso exemplo, o primeiro caso tem 99% de estar o grupo de empresas falidas.
      
    ')
names(p.l)
# [1] "class"     "posterior" "x" 

# Analsando os objetos
# class - indica em qual grupo cada objeto foi classificado;
# posterior - probabilidade a posteriori estimado pelo modelo;
# x - valor da função discriminante estimada

## Classificação dos 18 primeiros elementos (quando se tem muitos dados é bom usar head)
head(p.l$class, 18)
## Probabilidade de pertencer à pop 0 e 1
head(p.l$posterior,18)
## Valor do escore estimado pela função discriminante
head(p.l$x,18)

#----------------------------------------------------
#Análise da qualidade do modelo (por resubistituição)
# Compara os grupos preditos com os verdadeiros grupos 
l<-mean(p.l$class == fal$pop)
print(l)

cat(' 
Neste método, chamado de divisão da amostra, estimamos a função discriminante com 80% dos dados
Os 20% restante dos dados utilizamos para  realizar a classificação.

Tivemos 91% de classificação correta.
    ')

#-------------------------------------------------------
# Classificação dos elementos por população
## Número de elementos classificados
table(fal$pop, p.l$class, dnn=c("Real","Classificação"))
## Classificação percentual
table(fal$pop, p.l$class, dnn=c("Real","Classificação")) %>%
  prop.table(1) %>% round(3)
## Equival a calcule a tabela de acertos, então, calcule a probabilidade
## e por fim, arredonde a 3 casas decimais.
#Observe que a rotina acima é equivalente a 
round(prop.table((table(fal$pop, p.l$class, dnn=c("Real","Classificação"))),1),3)
## Assim, o pepe (%>%) permite um comando mais simples 

#-----------------------------------------------------
#Matriz de confusão
predito <- p.l$class
observado <- as.factor(fal$pop)
confusionMatrix(predito, observado)
#Para analisar cada um dos indicadores, utilise a ajuda
?caret::confusionMatrix
#ou, salvando no objeto c.l
c.l<-confusionMatrix(predito, observado)
18/(18+3)
#como salvamos no objetio c.l, precisamos da função print
print(c.l)

# MAPA TERRITORIAL
library(MASS)  # Para a função lda
library(ggplot2)

# Obter as funções discriminantes
qla_scores <- predict(d.l)$x
qla_scores_df <- as.data.frame(qla_scores)
qla_scores_df$pop <- fal$pop

# Criar o gráfico
ggplot(qla_scores_df, aes(x = LD1, color = pop)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Distribuição das Funções Discriminantes",
       x = "Função Discriminante 1",
       y = "Frequência") +
  theme_minimal()

# Aqui a discriminação se dá para valores abaixo e acima de 0.

#####################################################
#####################################################

# Função discriminante quadrática

# Calculando a função discriminane quadrática
d.q <- qda (pop ~ x1 + x2 + x3 + x4, data=fal)

# Exibindo os resultados
print(d.q)

#

print(d.q$scaling)

cat('
Matriz de Covariância por Grupo

QDA: Não utiliza uma matriz scaling porque é uma técnica não linear que calcula 
funções discriminantes baseadas em covariâncias separadas para cada grupo.

Na QDA, cada grupo (ou classe) tem sua própria matriz de covariância. 
Essas matrizes são usadas para calcular as funções discriminantes não lineares 
para separar as classes. Para cada grupo, a matriz de covariância indica como as 
variáveis se relacionam entre si dentro desse grupo.    

        ')

#----------------------------------------------------
#Análise da qualidade do modelo (por resubistituição)
#Obtendo os valores preditos 
p.q <- d.q %>% predict(fal)

#Acerto global do modelo
q<-mean(p.q$class == fal$pop)
print(q)

table(fal$pop, p.q$class, dnn=c("Real","Classificação")) %>%
  prop.table(1) %>% round(3)

#Matriz de confusão
predito.q <- p.q$class
observado <- as.factor(fal$pop)
c.q<-confusionMatrix(predito.q, observado)
c.q

#####
#MAPA TERRITORIAL
# Obter as funções discriminantes
qda_scores <- predict(d.q)$x
qda_scores_df <- as.data.frame(qla_scores)
qda_scores_df$pop <- fal$pop

# Criar o gráfico
ggplot(qda_scores_df, aes(x = LD1, color = pop)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Distribuição das Funções Discriminantes",
       x = "Função Discriminante 1",
       y = "Frequência") +
  theme_minimal()

# Aqui a discriminação se dá para valores abaixo e acima de 0.

###############################################################################
##############################################################################
