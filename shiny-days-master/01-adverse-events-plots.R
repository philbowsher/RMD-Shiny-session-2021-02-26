library(ggthemes)
library(readr)
library(GGally)
library(gridExtra)
library(kableExtra)
library(tidyverse)
library(GGally)

# Load Data

MTP_Data <- read_csv("MTP_Data.csv")
mtp_data2 <- read_csv("mtp_data2.csv")

mtp <- mtp_data2 %>% left_join(MTP_Data)

#Initial Review of the data
glimpse(mtp)

# Add Total Sale and Total volume to the data frame
# Add Total Sale and Total volume to the data frame
mtp<- mutate(mtp, TotalSale = units*price) %>% mutate(mtp, TotalVolume = units*volume) %>% separate(brand, into = c("company", "brand"), sep = "\\ ", extra = "merge")


mtp$promo<- factor(mtp$promo)

mtp$promo<- recode(mtp$promo, "0" = "No Promo", "1" = "Promo")

mtp$ad<- recode(mtp$ad, "NONE" = "No Ad", "A" = "Big Ad", "B" = "Mid/Small Ad")

mtp$company<- recode(mtp$company, "GENERAL" = "GENERAL MILLS")

mtp$brand<- recode(mtp$brand, "MILLS CINNAMON TST CR" = "CINNAMON TST CR")

mtp$brand<- recode(mtp$brand, "MILLS CHEERIOS" = "CHEERIOS")

mtp$brand<- recode(mtp$brand, "MILLS LUCKY CHARMS" = "LUCKY CHARMS")

mtp$brand<- recode(mtp$brand, "MILLS COCOA PUFFS" = "COCOA PUFFS")

mtp$brand<- recode(mtp$brand, "MILLS KIX" = "KIX")

#review the updated data
glimpse(mtp)

distinct(mtp, brand)

distinct(mtp, company)

write_csv(mtp, "mtp.csv")


company_name <- "KELLOGGS"


mtp_KELLOGGS <- filter(mtp, company == company_name )

#STOP Open adverse with esquisse


# plot all events  
mtp_KELLOGGS %>% 
  group_by(brand) %>% 
  summarise(units = sum(units)) %>% 
  ggplot() +
  geom_bar(aes(reorder(brand,units), units), stat = 'identity') +
  coord_flip() +
  labs(
    title = company_name,
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

# plot by gender
ggplot(mtp_KELLOGGS) +
  geom_bar(aes(reorder(brand,units), units, fill = flavor), stat = 'identity') +
  facet_wrap(~flavor)+
  coord_flip() +
  labs(
    title = company_name,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() + 
  guides(fill = FALSE) + 
  scale_fill_manual(values = c("#d54a30","#4c83b6", "#d0d530", "#9030d5"))

