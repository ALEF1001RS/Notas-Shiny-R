
## MBA USP - ESALQ
## TCC - Criacao de um aplicativo para ajudar candidatos do ENEM (Sisu) de cursos de exatas na escolha da universidade


# Alef Rodolfo dos Santos

# Instalacao e carregamento dos pacotes utilizados

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
             "data.table",
             "ggmap")



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



dados <- read_xlsx('notas_do_ENEM_1.xlsx')


# Visualiza??o da base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)



colnames(dados)
view(dados)





#Data Manipulation

library(ggmap)
library(ggplot2)
library(sp)




categoria <- 'Cota Rc EP 1,5sm'

curso <- c('Eng Ambiental','Eng Química')


região <- c('Sudeste','Nordeste')



estado<-c('Paraná','Pernambuco')

dados_select<-dados %>% filter(Categoria == categoria &
                   Curso%in%curso & Região%in%região & Estado%in%estado)






theme_set(theme_bw())



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
##---------- Esquema de aglomeração hierárquico---------------------------------

# Matriz de dissimilaridades
matriz_D <- dtable %>% 
  select(Nota, Ensino, Pesquisa, Mercado, Inovação, Internacionalização) %>% 
  dist(method = "euclidean")

# Method: parametrização da distância a ser utilizada

## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "complete")


# O input é a matriz de distâncias obtida anteriormente

# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes



# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema


# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

DT::datatable(matriz_D)


# Dendrograma com visualização dos clusters (definição de 3 clusters)
fviz_dend(x = cluster_hier,
         k = 3,
        k_colors = c("deeppink4", "darkviolet", "deeppink"),
       color_labels_by_k = F,
      rect = T,
     rect_fill = T,
    lwd = 1,
   ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
dtable$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
 dtable %>%
  kable() %>%
 kable_styling(bootstrap_options = "striped", 
              full_width = FALSE,
             font_size = 20)


