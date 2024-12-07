library(tidyverse)
df <- read_csv("nepal_anthro.csv")
df %>% head()
data  <- df
rescale_01 <- function(x){
    min_x <- min(x,na.rm = TRUE)
    max_x <- max(x,na.rm = TRUE)
    (x - min_x)/(max_x - min_x)
    }

df %>% select(-id,-sex) %>% 
map_dfr(rescale_01)

colnames(df)
for (i in colnames(df)){
    df[[i]]  <- rescale_01(df[[i]])
}

df %>% na.omit() %>% 
ggplot(aes(x = age,y = weight))+
geom_point()+
geom_smooth()+
theme_bw()

df[["age"]]
df["age"]

values <- 1:10
values

map_dbl(values,\(x)2 * x) 
map_dbl(values, ~.x, 2*x)


map_dbl(values, \(x) x^3)
map_dbl(values, ~.x^3)

df <-  df %>% select(-id)

df %>% 
summarise(across(
    where(is.numeric),
    ~mean(.x,na.rm = TRUE))
)

letters <- c('a',"b","c")
times <- 1:3
map2_chr(letters,times,\(x,y) paste(rep(x,y),collapse = ""))

map2_chr(letters,times,~paste(rep(.x,.y),collapse = ""))
df

penguins <- read_csv("palmerpenguins.csv")
penguins <- penguins %>% na.omit()

Peng_nest <- penguins %>% 
group_by(species) %>% 
nest()
Peng_nest
splot <- function(species,data){
    ggplot(data,aes( x= bill_length_mm, y = body_mass_g))+
    geom_point()+
    labs(title = paste("Species:",species))
}

pmap(Peng_nest,splot)


Peng_nest <- penguins %>% 
group_by(species,sex) %>% 
nest()

splot <- function(species,sex,data){
    ggplot(data,aes( x= bill_length_mm, y = body_mass_g))+
    geom_point()+
    labs(title = paste("Species:",sex, species))
}

pmap(Peng_nest,splot)
plots <- pmap(Peng_nest,splot)
walk2(paste0("plots/species_sex_",Peng_nest$species,"_",Peng_nest$sex,".png"),plots,ggsave)

dir("plots",pattern = ".png")






