### Carregamento de bibliotecas
### Descrição das bibliotecas
#assertthat: validações e verificações.
#dplyr e purrr: manipulação de dados e programação funcional.
#igraph: análise de grafos.
#ggplot2, ggraph e ggmap: visualização de dados e mapas.

library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)


#################################################################################
###leitura de banco de dados

nodes=read.table("nodes.txt", header=T)
colnames(nodes)=c('id', 'lon', 'lat', 'name') ##renomear colunas todas minusculas
attach(nodes)
head(nodes)

##########################################################################################
### Configuração de parametros aleatórios
### 15 países, cada um com um ID, geo-coordenadas ( lon e lat) e um nome. 
Esses são nossos nós de gráfico. Agora criaremos algumas conexões aleatórias ( edges) entre nossos nós:


set.seed(123)  # set random generator state for the same output

N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 4
N_CATEGORIES <- 4


###############################################################################################
# edges:Geração das arestas
edges <- map_dfr(nodes$id, function(id) {
  n <- floor(runif(1, N_EDGES_PER_NODE_MIN, N_EDGES_PER_NODE_MAX+1))
  to <- sample(1:max(nodes$id), n, replace = FALSE)
  to <- to[to != id]
  categories <- sample(1:N_CATEGORIES, length(to), replace = TRUE)
  weights <- runif(length(to))
  data_frame(from = id, to = to, weight = weights, category = categories)
})

edges <- edges %>% mutate(category = as.factor(category))


###############################################################################################
###Criação do grafo: Constrói um grafo não direcionado usando os dados de arestas (edges) e vértices (nodes).

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)



###############################################################################################
#Preparação de dados para o plot: Junta as coordenadas dos nós from e to às arestas, criando as colunas x, y, 
##xend, e yend, que representam as posições de origem e destino de cada conexão.

edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))



###############################################################################################
####Adição de pesos aos nós:Calcula o grau de cada nó (número de conexões) e adiciona como uma nova coluna chamada weight.

nodes$weight = degree(g)


###############################################################################################
## Configuração do tema do mapa:Define o estilo visual do mapa:
	•	#Remove elementos como grades e títulos de eixo.
	•	#Ajusta a posição da legenda.
	•	#Aplica um fundo cinza-escuro.


maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) 
  #theme(panel.background = element_rect(fill = "lightgray"))
  + theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))


###############################################################################################
## Adição dos contornos do mapa-múndi: Adiciona os contornos dos países ao mapa, com um estilo visual.

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

##map_data('world')





###############################################################################################

### Plotagem do grafo no mapa
#Desenha:
	0.	#Os contornos do mapa (country_shapes).
	0.	#As arestas como curvas (geom_curve), com cores e larguras baseadas na categoria e peso.
	0.	#Os nós como pontos (geom_point).
	0.	#Os rótulos dos nós (geom_text) com nomes.
	0.	#Aplica as coordenadas (mapcoords) e o tema (maptheme).

ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = category, size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat),                          # draw nodes
             shape = 21, size = 3, fill = 'white',
             color = 'black', stroke = 0.5) +
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme
  
  
  
  
  
  
  
  
  






