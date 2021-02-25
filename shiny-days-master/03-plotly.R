library(plotly)

(mtp %>%
    filter(!(brand %in% c("GENERAL MILLS CINNAMON TST CR", "GENERAL MILLS CHEERIOS", 
                          "GENERAL MILLS LUCKY CHARMS", "GENERAL MILLS COCOA PUFFS", "GENERAL MILLS KIX", "POST SHREDDED WHEAT", 
                          "POST GRAPE NUTS"))) %>%
    filter(package %in% "BOX") %>%
    filter(!(ad %in% "NONE")) %>%
    ggplot() +
    aes(x = units) +
    geom_histogram(bins = 30L, fill = "#0c4c8a") +
    theme_minimal() +
    facet_wrap(vars(brand))) %>% ggplotly()