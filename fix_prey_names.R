# Fix prey names 
library(tidyverse)    # data tidying
library(rgbif)        # retrieve taxonomy

prey <- read_csv("tidy_prey.csv") %>% 
  select(listed_species, prey_diet_binomial) 

# out of date names -> GBIF names
prey <- prey %>%
  mutate(prey_diet_binomial_updates = recode(
    prey_diet_binomial,
    "Allotruxalis strigata"   = "Allotruxalis gracilis",
    "Tetralimonius ornatulus" = "Limonius ornatulus", 
    "Caprimulgus vociferus"   = "Antrostomus vociferus"
  ))


# get full taxonomy 
full_names_listed <- prey %>% 
  select(listed_species) %>% 
  distinct() %>% 
  mutate(listed_species = ifelse(
    listed_species == "Grus canadensis not subspecies", 
    "Grus canadensis", 
    listed_species)) 

full_names_listed <- full_names_listed %>%
  select(listed_species) %>%
  name_backbone_checklist() %>% 
  select(order, class, family, "listed_species" = verbatim_name)

full_names_listed <- full_names_listed %>%
  rename_with(.fn = ~paste0(.x, "_listed"), !contains("listed"))

full_names_prey <- prey %>% 
  select(prey_diet_binomial_updates) %>% 
  distinct() %>% 
  name_backbone_checklist() %>%
  select(scientificName, order, class, family, species, verbatim_name)

full_names_prey <- full_names_prey %>%
  mutate(subspecies = str_count(verbatim_name , "\\S+")) %>%
  mutate(subspecies = ifelse(subspecies > 2, "yes", "no"))

names_to_fix <- full_names_prey %>% 
  filter(verbatim_name != species & subspecies == "no") %>%
  select(species, verbatim_name) 

# the subspecies name seem correct
full_names_prey %>%
  filter(verbatim_name != species & subspecies == "yes") %>%
  select(species, verbatim_name)

write_csv(names_to_fix, "prey_names_to_fix.csv")

# make vector to recode prey names
prey_recode <- names_to_fix$species
names(prey_recode) <- names_to_fix$verbatim_name

prey <- prey %>%
  mutate(prey_diet_binomial_updates = recode(prey_diet_binomial_updates, !!!prey_recode))

# I don't know why gbif doesn't have Perognathus inornatus and Neogale frenata
# but I am including them even though they aren't "Accepted" by gbif taxonomy
full_names_prey <- full_names_prey %>%
  mutate(species = ifelse(is.na(species) | subspecies == "yes", verbatim_name, species)) %>%
  select(-verbatim_name, -subspecies, -scientificName)

full_names_prey <- full_names_prey %>%
  rename_with(.fn = ~paste0(.x, "_prey")) %>%
  distinct()

# data for prey plot
prey_plot <- prey %>% 
  group_by(listed_species, prey_diet_binomial_updates) %>% 
  summarize(n = n())

# data for prey table
prey_table <- prey %>% 
  select(listed_species, prey_diet_binomial_updates) %>% 
  distinct()

prey_joined2 <- prey %>%
  left_join(full_names_listed, by = c("listed_species" = "listed_species")) %>%
  left_join(full_names_prey, by = c("prey_diet_binomial_updates" = "species_prey"))

# need to fix prey without class information

# find species with no class info
prey_noclass <- prey_joined2 %>% 
  filter(is.na(class_prey)) %>% 
  select(prey_diet_binomial_updates) %>%
  distinct()

# make column for new class info
prey_noclass$class <- NA

# for loop to run tax_name and retrieve class for each species
for(i in 1:length(prey_noclass$prey_diet_binomial_updates)){
  tmp <- taxize::tax_name(prey_noclass[i,1], get = "class")
  prey_noclass$class[i] <- tmp$class
}

# checked one species with no match, update to appropriate name
prey_noclass$class[is.na(prey_noclass$class)] <- "Teleostei"

# join back to main data frame
prey_joined2 <- prey_joined2 %>% 
  left_join(prey_noclass, by = c("prey_diet_binomial_updates")) %>%
  mutate(class_prey = ifelse(is.na(class_prey), class, class_prey)) %>%
  select(-class)

# listed species without order info
listed_noorder <- prey_joined2 %>%
  filter(is.na(order_listed))

# fix listed species order info
prey_joined2 <- prey_joined2 %>%
  mutate(order_listed = case_when(
    is.na(order_listed) & listed_species == "Thamnophis gigas" ~ "Squamata",
    is.na(order_listed) & listed_species == "Gambelia sila" ~ "Squamata",
    is.na(order_listed) & listed_species == "Grus canadensis not subspecies" ~ "Gruiformes",
    !is.na(order_listed) ~ order_listed
  ))

rm(prey_noclass, listed_noorder)

# fix prey without order info
prey_noorder <- filter(prey_joined2, is.na(order_prey))

prey_noorder <- prey_noorder %>%
  select(prey_diet_binomial_updates, order_prey) %>%
  distinct()

prey_noorder$order <- NA

for(i in 1:length(prey_noorder$prey_diet_binomial_updates)){
  tmp <- taxize::tax_name(prey_noorder[i,1], get = "order")
  prey_noorder$order[i] <- tmp$order
}

prey_noorder <- prey_noorder %>% 
  mutate(order = ifelse(is.na(order), replace_na("Squamata"), order)) %>%
  select(-order_prey)

# join back to main data frame
prey_joined2 <- prey_joined2 %>% 
  left_join(prey_noorder, by = c("prey_diet_binomial_updates")) %>%
  mutate(order_prey = ifelse(is.na(order_prey), order, order_prey)) %>%
  select(-order)

# fix grus canadensis not subspecies
prey_joined2$class_listed[prey_joined2$listed_species == "Grus canadensis not subspecies"] <- "Aves"
prey_joined2$family_listed[prey_joined2$listed_species == "Grus canadensis not subspecies"] <- "Gruidae"
prey_joined2$listed_species[prey_joined2$listed_species == "Grus canadensis not subspecies"] <- "Grus canadensis"

write_csv(prey_joined2, "tidy_prey_full_names.csv")


rm(prey_table, full_names_listed, full_names_prey, prey_noorder)

