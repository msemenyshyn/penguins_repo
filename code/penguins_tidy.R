library(tidyverse)
library(palmerpenguins)

penguins_raw <-penguins_raw
glimpse(penguins_raw)

#Select columns
penguins_selected_cols <-select(penguins_raw, Species, Island, 'Individual ID')

penguins_selected_cols <- penguins_raw |>
 select(Species, Island, 'Flipper Length (mm)', 'Individual ID')

penguins_raw |>
  select(Species: 'Individual ID')

#we are desecring Species

penguins_raw |>
  select(-Species)

#we select columns which have word "Length" in it

penguins_raw |>
  select(Species, matches("Length"))

#we put Species in front of all columns in a dataset
penguins_raw |>
  select(Species, everything())

penguins_raw_reduced <-penguins_raw |>
  select(Species, matches("Length"))

penguins_raw |>
  select(Species, Stage, 'Individual ID', `Clutch Completion`, 'Date Egg')


#Renaming columns

penguins_raw |>
  rename(id = 'Individual ID',
flipper_length = 'Flipper Length (mm)')

penguins_raw |>
  select(Species, 'Individual ID', matches("Length"))|>
  rename(id = 'Individual ID',
flipper_length = 'Flipper Length (mm)')


#Creating columns

penguins_raw |>
  select(Species, matches ("Length")) |>
  rename(flipper_length = 'Flipper Length (mm)') |>
  
  #create a new column
  
  mutate(flipper_length_cm = flipper_length *0.1, 
         culmen_length_cm = `Culmen Length (mm)` * 0.1)

penguins_raw|> #should do on my own
  
#Filtering rows
  penguins_raw |>
  filter(Species != "Gentoo penguin (Pygoscelis papua)")

penguins_raw |>
  filter(Island %in% c ("Torgersen", "Bicsoe"))

penguins_raw |>
  filter(Island == "Torgesen" & Sex == "MALE")

penguins_raw |>
  filter(`Flipper Lenght (mm)` > 200)

#Exercise 

penguins_raw |>
  select(Species, `Body Mass (g)`, matches ("Length")) |>
  rename(flipper_length = `Flipper Length (mm)`) |>
  mutate(length_mass_ratio = flipper_length/ `Body Mass (g)`)

#Exercise
 penguins_raw |>
   filter(`Clutch Completion` == "Yes" & Island == 
          "Torgersen")

##Ordering rows
 penguins_raw |>
   arrange(`Body Mass (g)`)

mass_ordered <-penguins_raw |>
  filter(Island == "Biscoe") |>
  arrange (-`Body Mass (g)`) #from smallest to largest, note "-" sign

mass_ordered <-penguins_raw |>
  filter(Island == "Biscoe") |>
  arrange (Sex, `Body Mass (g)`) #if we do not tell, so it sorts intiitively, like from smallest to largest

#Summarizing data 
penguins_raw |>
  summarize(mean_flipper_lenght = mean(`Flipper Length (mm)`), 
            mean_body_mass = mean(`Body Mass (g)`))

penguins_raw |>
  summarize(mean_flipper_lenght = mean(`Flipper Length (mm)`, na.rm =T), 
            mean_body_mass = mean(`Body Mass (g)`, na.rm = T)) #we removed N/A/missing values



penguins_raw |>
  filter(`Clutch Completion`== "Yes") |>
  summarize(max_mean_flipper_lenght = max(`Flipper Length (mm)`, na.rm =T), 
            min_body_mass = min(`Body Mass (g)`, na.rm = T))

penguins_raw |>
  filter(`Clutch Completion`== "Yes") |>
  summarize(max_mean_flipper_lenght = max(`Flipper Length (mm)`, na.rm =T), 
            min_flipper_length = min(`Flipper Lenght (mm)`, na.rm = T),
.by = Sex)


#Recoding variables 
penguins_raw |>
  count(Species) #we counted penguins

chinstrap <- penguins_raw |>
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", 1, 0)) |>
  count(chinstrap)

chinstrap <- penguins_raw |>
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", "Chinstrap", "Other")) |>
  count(chinstrap)

chinstrap <- penguins_raw |>
  mutate(species = case_when(
    ## syntac for case_when is condition ~ value
    Species == "Adelie Penguin (Pygoscelis adeliae)"~ "Adelie", 
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinsrap",
    Species == "Gentoo penguin (Pygoscelis papua)"~ "Gentoo", 
    .default = "Other" #what to do for all other cases
  )) |> 
  
  ##export this table 
  library(knitr)

#summarising data (once again)

penguins_raw |>
  summarise(mean_fllipper_length = mean(`Flipper Length (mm)`),
            mean_body_mass = mean(`Body Mass (g)`))

#how to delete missing values 

penguins_raw |> 
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`, na.rm = T),
            mean_body_mass = mean(`Body Mass (g)`, na.rm = T)) 

penguins_raw |> 
  filter(`Clutch Completion` == "Yes") |> # to filter out penguins that did not have an (observed) full nest 
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm)`, na.rm = T))

penguins_raw |> 
  filter(`Clutch Completion` == "Yes") |> # to filter out penguins that did not have an (observed) full nest 
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm)`, na.rm = T),
            .by = Sex)  #  to group by sex 
penguins_raw |> 
  filter(`Clutch Completion` == "Yes") |> # to filter out penguins that did not have an (observed) full nest 
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm)`, na.rm = T),
            .by = Sex) |>  #  to group by sex 
  filter(is.na(Sex) == F) # to drop the penguins with an unknown sex 

penguins_raw |> 
  # the arguments are condition, true (what to do if true), false
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", 1, 0)) |> 
  count(chinstrap)

penguins_raw |> 
  # the arguments are condition, true (what to do if true), false
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", "Chinstrap", "Other")) |> 
  count(chinstrap)

penguins_raw |> 
  # syntax is condition ~ value
  mutate(species = case_when(
    Species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
    Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    .default = "Other" # what to do in all other cases
  )) |> 
  count(species)            

species_table <- penguins_raw |> 
  # syntax is condition ~ value
  mutate(species = case_when(
    Species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
    Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    .default = "Other" # what to do in all other cases
  )) |> 
  count(species) 

penguins_raw |> 
  # syntax is condition ~ value
  mutate(species = case_when(
    Species %in% c("Adelie Penguin (Pygoscelis adeliae)", "Gentoo penguin (Pygoscelis papua)") ~ "Adelie/Gentoo",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",  
    .default = "Other" # what to do in all other cases
  )) |> 
  count(species)
penguins_raw |> 
  # syntax is condition ~ value
  mutate(species = case_when(
    Species %in% c("Adelie Penguin (Pygoscelis adeliae)", "Gentoo penguin (Pygoscelis papua)") ~ "Adelie/Gentoo",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",  
    .default = "Other" # what to do in all other cases
  )) |> 
  count(species)
summary(penguins_raw)

penguins_ratio_data <-penguins_raw |>
  select(Species, `Body Mass (g)`, matches("Length")) |>
  rename(flipper_length = `Flipper Length (mm)`) |>
  mutate(length_mass_ratio = flipper_length/`Body Mass (g)`) |>
  arrange(length_mass_ratio)

write_csv(penguins_ratio_data, file = "data/penguins_ratio_data.csv")













