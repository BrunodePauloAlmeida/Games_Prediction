library(dplyr)
library(rvest)
library(stringr)

scrape_esportspedia <- function(url) {
  
  pag = url %>%
    read_html() 
  
  tab = pag %>% 
    html_node("#md-table") %>% 
    html_table(fill = TRUE)
  
  names(tab) = tab[2,]
  
  etapa = str_extract(tab[,1], "(?<=\\[showhide\\]).*")
  
  for(i in 2:length(etapa)) {
    if(is.na(etapa[i])) {
      etapa[i] = etapa[i-1]
    }
  }
  
  deletar = sort(c(which(!is.na(str_extract(tab[,1], "showhide"))), which(!is.na(str_extract(tab[,1], "Team 1")))))
  
  etapa = etapa[-deletar]
  tab = tab[-deletar,]
  
  tab$Etapa = etapa
  
  tab[,c(1:3, ncol(tab))] %>%
    distinct(.keep_all = TRUE) %>%
    mutate(`Team 1` = str_sub(`Team 1`, start = 3),
           `Team 2` = str_sub(`Team 2`, start = 3))
}


worlds_season1 = scrape_esportspedia("https://lol.gamepedia.com/Season_1_World_Championship")
worlds_season2 = scrape_esportspedia("https://lol.gamepedia.com/Season_2_World_Championship")
worlds_season3 = scrape_esportspedia("https://lol.gamepedia.com/Season_3_World_Championship")
worlds_season4 = scrape_esportspedia("https://lol.gamepedia.com/2014_Season_World_Championship")
worlds_season5 = scrape_esportspedia("https://lol.gamepedia.com/2015_Season_World_Championship")
worlds_season6 = scrape_esportspedia("https://lol.gamepedia.com/2016_Season_World_Championship")

worlds_season7_pin = scrape_esportspedia("https://lol.gamepedia.com/2017_Season_World_Championship/Play-In")
worlds_season7_mev = scrape_esportspedia("https://lol.gamepedia.com/2017_Season_World_Championship/Main_Event")
worlds_season7 = rbind(worlds_season7_pin, worlds_season7_mev)

worlds_season8_pin = scrape_esportspedia("https://lol.gamepedia.com/2018_Season_World_Championship/Play-In")
worlds_season8_mev = scrape_esportspedia("https://lol.gamepedia.com/2018_Season_World_Championship/Main_Event")
worlds_season8 = rbind(worlds_season8_pin, worlds_season8_mev)

worlds_season9_pin = scrape_esportspedia("https://lol.gamepedia.com/2019_Season_World_Championship/Play-In")
worlds_season9_mev = scrape_esportspedia("https://lol.gamepedia.com/2019_Season_World_Championship/Main_Event")
worlds_season9 = rbind(worlds_season9_pin, worlds_season9_mev)

worlds_season10_pin = scrape_esportspedia("https://lol.gamepedia.com/2020_Season_World_Championship/Play-In")
worlds_season10_mev = scrape_esportspedia("https://lol.gamepedia.com/2020_Season_World_Championship/Main_Event")
worlds_season10 = rbind(worlds_season10_pin, worlds_season10_mev)

write.table(worlds_season1, file = "Worlds/worlds_2011.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season2, file = "Worlds/worlds_2012.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season3, file = "Worlds/worlds_2013.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season4, file = "Worlds/worlds_2014.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season5, file = "Worlds/worlds_2015.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season6, file = "Worlds/worlds_2016.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season7, file = "Worlds/worlds_2017.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season8, file = "Worlds/worlds_2018.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season9, file = "Worlds/worlds_2019.csv", sep = ";", na = "", row.names = FALSE)
write.table(worlds_season10, file = "Worlds/worlds_2020.csv", sep = ";", na = "", row.names = FALSE)


#Run every day


worlds_season10_pin = scrape_esportspedia("https://lol.gamepedia.com/2020_Season_World_Championship/Play-In")
worlds_season10_mev = scrape_esportspedia("https://lol.gamepedia.com/2020_Season_World_Championship/Main_Event")
worlds_season10 = rbind(worlds_season10_pin, worlds_season10_mev)
worlds_season10
write.table(worlds_season10, file = "Worlds/worlds_2020.csv", sep = ";", na = "", row.names = FALSE)



