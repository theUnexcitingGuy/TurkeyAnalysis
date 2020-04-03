
# import libraries --------------------------------------------------------
library(dplyr)
library(tidyverse)
library(bibliometrix)
library(igraph) 
library(rgexf) 
library(widyr)

# import the downloaded file from Web of Science --------------------------
D <- readFiles("savedrecs.txt", "savedrecs (1).txt")
M <- convert2df(D, dbsource = "isi", format = "plaintext")
View(M)


# renaming the columns ----------------------------------------------------
data <- M %>%
  rename(
    abstract = AB,
    title = TI,
    year = PY,
    keywords1 = DE,
    keywords2 = ID
  )
colnames(data)


# selecting the columns ---------------------------------------------------
final_data <- data %>%
  select(title, abstract, year, keywords1, keywords2)


# uniting keywords columns ------------------------------------------------
new_table <- final_data %>% 
  mutate(keyword = paste0(keywords1, "; ", keywords2)) %>% 
  mutate(keyword = str_split(keyword, "; ")) %>% 
  unnest() %>% 
  select(title, abstract, year, keyword)


# creating the gephi file -------------------------------------------------
gephi <- new_table %>% 
  mutate(keyword = str_to_lower(keyword)) %>% 
  filter(!keyword %in% c('na', 'technology', 'turkey', 'model', 'system', 'impact', 'systems', 'countries')) %>% 
  pairwise_count(keyword, title, sort = T, upper = F) %>% 
  filter(n > 2) %>% 
  rename("C1" = item1, "C2" = item2, "weight" = n)

g <- gephi %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify()

mygraph <- igraph.to.gexf(g, position=NULL)
sink("Turkey.gexf")

print(mygraph)
View(g)


# technological trends ----------------------------------------------------
trends_in_technologies  %>% 
  filter(type == 'Total') %>% 
  group_by(technology) %>% 
  filter(!technology %in% c('Big data', 'artificial intelligence', 'augmented reality', 'nanomaterials')) %>% 
  mutate(count = (count - min(count))/(max(count)-min(count))) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = count)) +
  geom_point(size = 1) +
  geom_smooth(method = glm, formula = y ~ splines::bs(x, 3), se = FALSE) +
  facet_wrap(~technology) + #con questo comando faccio tred per ciascuna tecnologia in plot separati
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab('Percentage')







