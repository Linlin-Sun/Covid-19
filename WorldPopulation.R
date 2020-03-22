library(rvest)

getWorldPopulation<- function() {
    url <- "https://www.worldometers.info/world-population/population-by-country/"
    h <- read_html(url)
    class(h)
    # We learned that tables in html are associated with the table node.  Use the html_nodes() function and the 
    # table node type to extract the first table. Store it in an object nodes:
    
    nodes <- html_nodes(h, "table")
    class(nodes)
    nodes
    
    world_population <- html_table(nodes[[1]])
    
    world_population$Population <-  world_population$`Population (2020)`
    world_population$Density <-  world_population$`Density (P/Km²)`
    world_population$Median_Age <- world_population$`Med. Age`
    
    world_population$Country.Region = world_population[,2]
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "United States", "US", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "South Korea", "Korea, South", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Bahamas", "Bahamas, The", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Côte d'Ivoire", "Cote d'Ivoire", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Czech Republic (Czechia)", "Czechia", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Timor-Leste", "East Timor", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Gambia", "Gambia, The", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "Taiwan", "Taiwan*", world_population$Country.Region)
    world_population$Country.Region <- 
        ifelse(world_population$Country.Region == "St. Vincent & Grenadines", "Saint Vincent and the Grenadines", world_population$Country.Region)
    
    world_population
}
