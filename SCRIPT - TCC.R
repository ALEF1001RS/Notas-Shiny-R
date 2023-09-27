
## MBA USP - ESALQ
## TCC - Cria??o de um aplicativo para ajudar candidatos do ENEM (Sisu) de cursos de exatas na escolha da universidade


# Alef Rodolfo dos Santos

# Instala??o e carregamento dos pacotes utilizados

pacotes <- c("plotly", #plataforma gr?fica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto para 'ggplot2' que ajudam a
                        #evitar sobreposi??o de textos
             "knitr", "kableExtra", #formata??o de tabelas
             "reshape2", #fun??o 'melt'
             "misc3d", #gr?ficos 3D
             "plot3D", #gr?ficos 3D
             "cluster", #fun??o 'agnes' para elabora??o de clusters hier?rquicos
             "factoextra", #fun??o 'fviz_dend' para constru??o de dendogramas
             "ade4", #fun??o 'ade4' para matriz de dist?ncias em vari?veis bin?rias
             "readxl",# fun??o para leitura do Excel
             "shiny", #biblioteca Shiny do R para desenvolver app
              "dplyr", 
             "data.table")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


setwd("C:/Users/user/OneDrive/?rea de Trabalho/MBA - USP ESALQ")


# Carregamento da base de dados 
load(file = ".RData")



dados <- read_csv('notas_do_ENEM_1.csv')


# Visualiza??o da base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

colnames(dados)

library(ggplot2)




view(dados)





#Data Manipulation



categoria <- 'Cota Rc EP 1,5sm'

curso <- c('Eng Ambiental','Eng Química')

região <- c('Sudeste','Nordeste')


estado<-c('Paraná','Pernambuco')

dados_select<-dados %>% filter(Categoria == categoria &
                   Curso%in%curso & Região%in%região & Estado%in%estado)






theme_set(theme_bw())




unid<-dados_select%>%
  group_by(Unidade)
  ggplot(dados_select) +
  geom_line(aes(x=Curso, y = Nota))
  
  dtable<-dados
  

##=======================================##
  
  dados$Categoria
  length(unique(dados$Categoria))
  
  
  
  dados$Curso
  length(unique(dados$Curso))
  
  dados$Estado
  length(unique(dados$Estado))
  
  dados$Região
  length(unique(dados$Região))
  
 
view(dados)
  

dtable<-dados
##---------- Esquema de aglomera??o hier?rquico---------------------------------

# Matriz de dissimilaridades
matriz_D <- dados %>%
  select(Nota, Ensino, Pesquisa, Mercado, Inova??o , Internacionaliza??o) %>% 
  dist(method = "euclidean")

## Em 'dist.binary', method = 2 indica similaridade por emparelhamento simples


# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = )


# Elabora??o da clusteriza??o hier?rquica
cluster_hier <- agnes(x = matriz_D, method = "average")

# Outras op??es de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Defini??o do esquema hier?rquico de aglomera??o

# As dist?ncias para as combina??es em cada est?gio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomera??o. Interpreta??o do output:

## As linhas são os estágios de aglomera??o
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))

names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")

esquema



# Visualiza??o do esquema hier?rquico aglomerativo
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)





# Visualização da base de dados com a alocação das observações nos clusters
dados %>%
  select(Universidade, cluster_H) %>%
  arrange(Universidade) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# FIM!

