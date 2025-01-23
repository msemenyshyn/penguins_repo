library (tidyverse)
chinstrap_data <- read_csv("data/chinstrap_data.csv")
chinstrap_data <- read_csv("data/chinstrap_data.csv")
adelie_data <- read_csv("data/adelie_data.csv")
penguin_data <- bind_rows(adelie_data, chinstrap_data)
gentoo_data <-read_csv("data/gentoo_data.csv")
penguin_data <- bind_rows(adelie_data, chinstrap_data, gentoo_data)
table(penguin_data$Species) #we look up particular data 
write_csv(penguin_data, "data/penguin.csv") #we create a file





ggplot(penguins_raw) +
  aes(x = `Body Mass (g)` , y = `Flipper Length (mm)`, color = Island, shape = Sex) +
geom_point() +
scale_color_brewer(palette = "Set1") +
  scale_shape(na.translate = TRUE, na.value = 18) + #we assigned a shape to a penguin with non specified gender
scale_x_continuous(name = "Body Mass (gramms)") +
  scale_x_continuous(name = "Flipper Length (millimeters)")+
theme_classic() +
  theme(axis.text = element_text(color = "black"))

theme_cool <- theme_classic() +
  theme(axis.text = element_text (color = "black"))
        
ggplot(penguins_raw)+
  aes(x = `Body Mass (g)`, fill = Species) +
  geom_histogram() +
  theme_cool + 
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 30)), expand = expansion()) +
  theme(legend.position = "top", legend.direction = "vertical")


ggplot(penguins_raw) + 
  aes(x = Island, y = `Culmen Length (mm)`)+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_cool

ggplot(penguins_raw)+
  aes(x = `Delta 15 N (o/oo)` , y = `Delta 13 C (o/oo)`)+
  geom_point()+
  geom_density_2d()+
  geom_density2d_filled(alpha = 0.5)+
  theme_cool+
  scale_x_continuous(limits = c(7, 10.5))+
  scale_y_continuous(limits = c(-27.5, -23.5)) 
  
  
ggplot(penguins_raw) +
  aes(x = `Body Mass (g)` , y = `Flipper Length (mm)`, color = Island, shape = Sex) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_shape(na.translate = TRUE, na.value = 18) + #we assigned a shape to a penguin with non specified gender
  scale_x_continuous(name = "Body Mass (gramms)") +
  scale_x_continuous(name = "Flipper Length (millimeters)")+
  theme_classic() +
  theme(axis.text = element_text(color = "black"))

  g1 <- ggplot(penguins_raw) +
  aes(x = `Body Mass (g)` , y = `Flipper Length (mm)`, color = Island, shape = Sex) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_shape(na.translate = TRUE, na.value = 18) + #we assigned a shape to a penguin with non specified gender
  scale_x_continuous(name = "Body Mass (gramms)") +
  scale_x_continuous(name = "Flipper Length (millimeters)")+
  theme_classic() +
  theme(axis.text = element_text(color = "black"))


#we save it and it appears in global environmnet 

g2 <- ggplot(penguins_raw)+
  aes(x = `Delta 15 N (o/oo)` , y = `Delta 13 C (o/oo)`)+
  geom_point()+
  geom_density_2d()+
  geom_density2d_filled(alpha = 0.5, show.legend = FALSE)+
  theme_cool+
  scale_x_continuous(limits = c(7, 10.5))+
  scale_y_continuous(limits = c(-27.5, -23.5))
#we save it and it appears in global environment


library(patchwork) #extends the 'grammar of graphics'

 g1+g2

 
 g3 <- ggplot(penguins_raw)+
   aes(x = Island, y = `Culmen Length (mm)`) +
   geom_boxplot()+
   theme_cool
#theme_classic()
 
 
 (g1 | g2 )/ g3 #we set a particular 

 gg <-(g1 | g2 )/ g3 #we saved it
 
 ggsave("figures/penguins_1.pdf", gg, height = 10, width = 10) #we saved as pdf
 ggsave("figures/penguins_1.jpg", gg, height = 10, width = 10) #we saves as jpg
 
