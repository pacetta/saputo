```{r, fig.height= 9}
datos8 <- pivot_longer(datos, cols = c(21:25), names_to = "pregunta") %>% 
  mutate(pregunta = str_remove(pregunta, " situación seleccionada"))

ggplot(datos8) +
  aes(x = value, y = ..count../sum(..count..)*100) +
  geom_bar() +
  facet_grid(.~ `2.  ¿En qué rango de edad se encuentra?`) +
  labs(y = "%" , fill = "", x = "Situación", title = titulo) + 
  tema +   geom_text(aes(label = paste(round(..count../sum(..count..)*100,1),"%")), 
                     stat = "count", vjust = 0, hjust = 0) +
  coord_flip() +  scale_x_discrete(labels = function(x) str_wrap(x, 30))
```

```{r, fig.height= 12}
ggplot(datos8) +
  aes(x = value, y = ..count../sum(..count..)*100) +
  geom_bar() +
  facet_grid(`2.  ¿En qué rango de edad se encuentra?`~ `4. ¿Qué tamaño tiene su tambo, en cantidad de vacas ordeño?`) +
  labs(y = "%" , fill = "", x = "Situación", title = titulo) + 
  tema +   geom_text(aes(label = paste(round(..count../sum(..count..)*100,1),"%")), 
                     stat = "count", vjust = 0, hjust = 0) +
  coord_flip() +  scale_x_discrete(labels = function(x) str_wrap(x, 30)) 
```

OTRA OPCION DE PRESENTAR

```{r, fig.height= 9}
datos8_ <- pivot_longer(datos, cols = c(21:25), names_to = "pregunta") %>% 
  mutate(pregunta = str_remove(pregunta, " situación seleccionada")) %>% 
  filter(pregunta %in% c("Primera (más urgente)", "Quinta"))

ggplot(datos8_) +
  aes(x = value, fill = pregunta, y = ..count../sum(..count..)*100) +
  geom_bar() +
  facet_grid(.~ `2.  ¿En qué rango de edad se encuentra?`) +
  labs(y = "%" , fill = "", x = "Situación", title = titulo) + 
  tema +   geom_text(aes(label = paste(round(..count../sum(..count..)*100,1),"%")), 
                     stat = "count", vjust = 0, hjust = 0) +
  coord_flip() +  scale_x_discrete(labels = function(x) str_wrap(x, 30))
```

```{r, fig.height= 12}
ggplot(datos8_) +
  aes(x = value, fill = pregunta, y = ..count../sum(..count..)*100) +
  geom_bar() +
  facet_grid(`2.  ¿En qué rango de edad se encuentra?`~ `4. ¿Qué tamaño tiene su tambo, en cantidad de vacas ordeño?`) +
  labs(y = "%" , fill = "", x = "Situación", title = titulo) + 
  tema +   geom_text(aes(label = paste(round(..count../sum(..count..)*100,1),"%")), 
                     stat = "count", vjust = 0, hjust = 0) +
  coord_flip() +  scale_x_discrete(labels = function(x) str_wrap(x, 30)) 

graf <- datos8_ %>% 
  filter(`3. ¿Podría seleccionar en qué provincia se localiza el establecimiento lechero?` == c("Córdoba", "Buenos Aires", "Santa Fe")) %>%
  group_by(`3. ¿Podría seleccionar en qué provincia se localiza el establecimiento lechero?`  ) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ ggplot(.x) +
                      aes(x = value, fill = pregunta, y = ..count../sum(..count..)*100) +
                      geom_bar() +
                      facet_grid(`4. ¿Qué tamaño tiene su tambo, en cantidad de vacas ordeño?` ~ `2.  ¿En qué rango de edad se encuentra?`) +
                      labs(y = "%" , fill = "", x = "Situación", 
                           title = paste0(titulo,": " , `3. ¿Podría seleccionar en qué provincia se localiza el establecimiento lechero?`)) + 
                      tema +   geom_text(aes(label = paste(round(..count../sum(..count..)*100,1),"%")), 
                                         stat = "count", vjust = 0, hjust = 0) +
                      coord_flip() +  scale_x_discrete(labels = function(x) str_wrap(x, 30)) ))

graf$plot
```
