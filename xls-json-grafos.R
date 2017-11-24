
#En esta primera parte:
# Usaremos un excel como fuente de los datos de Facebook que ya han trabajado
# Aprovechando que algunos datos vienen sucios

# Para lectura de Excel
library(readxl)
#Para manejar fechas
library(lubridate)
#Limpieza de cadenas de caracteres
library(stringr)
#Operador pipe
library(magrittr)
#Manejo de data frames
library(dplyr)
#Manejo de JSON
library(jsonlite)

#Grafos
library(igraph)

#Directorio de trabajo
setwd('C:/Users/david/Documents/p3rsonal/cl/variedad')

# Carga de datos. sheet el número de hoja
posts <- read_excel('Consolidado_archivo_de_Facebook.xlsx',sheet = 8, col_names = T)
head(posts)
#No nos da un data.frame, sino un tibble
posts[1,]
summary(posts)
#Revisemos lo que tiene...
posts$post_text[1:10]

#Parseemos fechas!
#Original
posts$post_published
#Modificada
ymd_hms(posts$post_published)
#Lubridate se encarga del trabajo 'sucio', incluso toma en consideración la zona horaria
#Esto es importante en aplicaciones de alcance mundial

#Ahora, reemplazaremos algunas cosas en los comentarios. 
#Inicialmente los datos se cargarían correctamente si el encoding es correcto,
#Pero en este caso somos afortunados de tener basura qué limpiar en el texto
# http://www.i18nqa.com/debug/utf8-debug.html
# http://www.utf8-chartable.de/unicode-utf8-table.pl?start=56896&utf8=char
posts$comment_message[1000]
posts$comment_message[grep('Ã|í',posts$comment_message)]

#Hacemos una función para reemplazar los datos
reemplazo <- function(vector) {
  vtemp <- vector
  caracteres <- data.frame(original=c('Ã¡','Ã©','Ã','Ã³','Ãº','Â¿','Ã±','í³','íº','í±','í“'),nuevo=c('á','é','í','ó','ú','¿','ñ','ó','ú','ñ','Ó'),stringsAsFactors = F)
  caracteres
  for(c in 1:nrow(caracteres)) {
    vtemp <- str_replace_all(vtemp,caracteres[c,1],caracteres[c,2])
    vtemp <- vtemp %>% gsub(pattern='[,.¡?¿!;:]',replacement=' ') %>% gsub(pattern='\\s+',replacement=' ')
  }
  vtemp
}
x <- reemplazo(posts$comment_message)
x[1:10]
x[grep('Ã|í',x)]


#Position se puede partir en dos: un identificador (pequeño) del post y un identificador de secuencia
posts$position[1:10]
posts %>% separate(position,into=c('idpost','idsecuencia'),sep='_')


#apply(caracteres,1,function(x) str_replace_all(posts$comment_message[1:3],x[1],x[2]))



#Hagámoslo con pipe  y mutate en una sola pasada
# y de paso hacemos los caracteres a minúsculas
posts.limpios <- posts %>%
  mutate(post_published=ymd_hms(post_published)) %>%
  mutate(comment_published=ymd_hms(comment_published)) %>%
  mutate(post_text=tolower(reemplazo(post_text))) %>%
  mutate(comment_message=tolower(reemplazo(comment_message))) %>%
  separate(position,into=c('idpost','idsecuencia'),sep='_')
head(posts.limpios)


#OK. Ya tenemos un dataset limpio


#Hagamos algunos análisis...

#Análisis de sentimiento
# Usaremos la forma más simple de este análisis, es decir,  con el diccionario AFINN
# Hay otras más complicadas, que usan métodos conocidos (Bayes, SVM, etc)
#Carguemos el diccionario (json)
# https://github.com/davidemiceli/sentiment-multilang/blob/master/lib/AFINN.js
#afinn <- rjson::fromJSON(paste(readLines('afinn-es.json'), collapse=""))
j <- rjson::fromJSON(file='afinn-es.json')
afinn <- data.frame(palabra=gsub('(es.)','',names(unlist(j))),valor=c(unlist(j)),stringsAsFactors = F)
afinn

# Hoy calculamos -a mano- el score...
# Luego haremos la versión resumida. 
#   Una buena regla de mano es: si se puede hacer con for, se puede hacer de una manera mejor

x <- posts.limpios$comment_message[1000]
str_split(x,' ')
grep (afinn$palabra)

obtiene.score <- function(cadena) {
  #print(str_split(cadena,' '))
  sum(afinn[which(afinn$palabra %in% str_split(cadena,' ')),2])
}

#No arroja mucho. Esto se debe a que el diccionario no es el mejor
sentimiento <- posts.limpios[,'comment_message'] %>% apply(1,function(x) obtiene.score(x))

table(sentimiento)

# TODO: Mejorar el diccionario / veremos otros métodos


# Grafos
attributes(posts.limpios)
# Tomaremos los 6 primeros caracteres del comment_by... no es recomendable para muchos usuarios. Mejor otro parámetro
df.nodos <- with(posts.limpios,data.frame(post=idpost,usuario=substr(comment_by,1,6),n=1))

g.nodos <- graph.data.frame(df.nodos,directed=F)

plot(g.nodos)

color.vertices <- ifelse(str_length(names(V(g.nodos)))==6,'#ff3245','#12e355')
plot(g.nodos,vertex.color=color.vertices,vertex.size=5,vertex.label.cex=.3)
plot(g.nodos,vertex.color=color.vertices,vertex.size=4,vertex.label.cex=.001)

#Comunidades
comunidad <- walktrap.community(g.nodos)
comunidad$membership
plot(comunidad,g.nodos,vertex.color=color.vertices,vertex.size=4,vertex.label.cex=.001)

