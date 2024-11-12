################################################################################
#                             ANÁLISE FATORIAL CONFIRMATÓRIA                   #
#                                 Exemplo da Drogaria                          #
################################################################################

cat('Um gerente de uma drogaria queria conhecer a percepção dos consumidores em 
    relação a oito atributos, e questionou 1700 clientes. 
    Os respondentes dariam uma nota de 0 a 10 nesses atributos
    Veja a base de dados para conhecer os atributos
    
    Os 8 atributos sairam de uma pesquisa anterior que o gerente estava estudando,
    e, com base nisso, a ideira era formar 3 fatores.
    
    Fator 1 = Produtos e Ambiente da Loja
          Formado pelos 5 primeiros atributos
    Fator 2 = Atendimento
          Formado somente pelo 6 atributo
    Fator 3 = Preços e políticas de desconto
          Formado pelos atributos 7 e 8
    
    Com base nessas informações, realize um análise fatoria confirmatória.
    
        ')




################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
# Pacotes utilizados
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "plot3D", #gráficos 3D
             "car", #gráficos 3D com função 'scatter3d'
             "Hmisc", #matriz de correlações com p-values
             "ltm") #determinação do alpha de Cronbach pela função 'cronbach.alpha'

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
################################################################################

# Carregamento da base de dados
load("C:/Users/rafae/Dropbox/UFES/ANÁLISE MULTIVARIADA/FATORIAL/capitulo-10/BANCOS DE DADOS, CÓDIGOS e PROJECT - R/PercepcaoDrogaria.RData")
view(PercepcaoDrogaria)

# Visualização da base de dados
PercepcaoDrogaria %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# ATENÇÃO - Se tiver pergunta reversa, temos que reverter na base, para ter o mesmo sentido das demais.
#            Se fosse o caso, nesse exemplo que vai de 0 a 10.
#            Imagine que tenho uma resposta igual a 0, mas está reversa e deveria ser 10.
#            Logo eu crio uma variável 10-(velha_variável) = nova_variável.
#             Substituindo: 10-(0)= 10
            
            
# Estatísticas descritivas
summary(PercepcaoDrogaria)

#Matriz de correlação
rho <- cor(PercepcaoDrogaria, method = "pearson")
rho

# Coeficientes de correlação de Pearson para cada par de variáveis, com
#respectivos p-values
#função 'rcorr' do pacote 'Hmisc'
rho2 <- rcorr(as.matrix(PercepcaoDrogaria), type="pearson")

rho2$r # matriz de correlações
rho2$P # matriz com p-values das correlações

# A função 'chart.Correlation' do pacote 'PerformanceAnalytics' propicia a
#plotagem das distribuições das variáveis, scatters, valores das correlações de
#Pearson e respectivas significâncias
chart.Correlation(PercepcaoDrogaria, histogram = TRUE, pch = "+")


################################################################################
#         ELABORAÇÃO DA ANÁLISE FATORIAL POR COMPONENTES PRINCIPAIS            #
################################################################################
# Estatística KMO e teste de esfericidade de Bartlett
# Funções 'KMO' e 'cortest.bartlett' do pacote 'psych'
# Farei uma para todas as variáveis, como se fosse exploratória e depois para os fatores pré-concebidos

KMO(PercepcaoDrogaria)
cortest.bartlett(PercepcaoDrogaria)

#Fator 1 - produtos e ambiente da loja 
KMO(PercepcaoDrogaria[, 1:5])
cortest.bartlett(PercepcaoDrogaria[, 1:5])

#Não precisa para o fator atendimento, por ser somente uma variável

#fator 3 - preços e política de descontos
KMO(PercepcaoDrogaria[, 7:8])
cortest.bartlett(PercepcaoDrogaria[, 7:8])


cat(' KMO - Com base no quadro apresentado nos slides, a adequação global da AF é média
      Bartkett - O teste chi-quadrado rejeita a hipótese nula que a diferença da matriz 
    de correlação e sua matriz identidade é igual a 0. Portanto, a extração de fatores é
    viável')

#################################################################################
# Elaboração da análise fatorial por componentes principais sem rotação
# Função 'principal' do pacote 'psych'
fator1 <- principal(PercepcaoDrogaria[, 1:5],
                      nfactors = 1,
                      rotate = "varimax",
                      scores = TRUE)
fator1

#Se a carga fatorial for baixa (geralmente <0,5), sugere-se excluir do fator

PercepcaoDrogaria <- cbind(PercepcaoDrogaria, fator1$scores)

# Alpha de Cronbach pela função 'alpha' do pacote 'psych'
alpha(PercepcaoDrogaria[, 1:5])

####################
fator2 <- principal(PercepcaoDrogaria[6],
                    nfactors = 1,
                    rotate = "varimax",
                    scores = TRUE)
fator2

#Se a carga fatorial for baixa (geralmente <0,5), sugere-se excluir do fator

PercepcaoDrogaria <- cbind(PercepcaoDrogaria, fator2$scores)

###################

fator3 <- principal(PercepcaoDrogaria[, 7:8],
                    nfactors = 1,
                    rotate = "varimax",
                    scores = TRUE)
fator3

#Se a carga fatorial for baixa (geralmente <0,5), sugere-se excluir do fator

PercepcaoDrogaria <- cbind(PercepcaoDrogaria, fator3$scores)

# Alpha de Cronbach pela função 'alpha' do pacote 'psych'
alpha(PercepcaoDrogaria[, 7:8])

###################

cat(' Para ficar mais fácil a leitura dos resultados, haja vista que estamos usando escalas de 0 a 10 
para todos as perguntas ,após testado os fatores, faz sentido gerar uma média das variáveis como fator. ')

# Criando a média por linha das colunas especificadas
PercepcaoDrogaria$Fator1 <- rowMeans(PercepcaoDrogaria[, c("sortimento", "reposicao", "layout", "conforto", "limpeza")], na.rm = TRUE)
PercepcaoDrogaria$Fator2 <- PercepcaoDrogaria$atendimento
PercepcaoDrogaria$Fator3 <- rowMeans(PercepcaoDrogaria[, c("preco", "desconto")], na.rm = TRUE)

rho3 <- rcorr(as.matrix(PercepcaoDrogaria[,9:14]), type="pearson")
rho3$r

rho4 <- rcorr(as.matrix(PercepcaoDrogaria[,9:11]), type="pearson")
rho4$r

##########################
