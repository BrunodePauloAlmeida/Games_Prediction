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

lck_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LCK/2020_Season/Summer_Season")
lck_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LCK/2020_Season/Summer_Playoffs")
lck_fasedegrupos
write.table(lck_fasedegrupos, file = "2020_2/LCK_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lck_playoffs, file = "2020_2/LCK_po.csv", sep = ";", na = "", row.names = FALSE)

cblol_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/CBLOL/2020_Season/Split_2")
cblol_playoffs = scrape_esportspedia("https://lol.gamepedia.com/CBLOL/2020_Season/Split_2_Playoffs")
write.table(cblol_fasedegrupos, file = "2020_2/CBLOL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(cblol_playoffs, file = "2020_2/CBLOL_po.csv", sep = ";", na = "", row.names = FALSE)

lcs_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LCS/2020_Season/Summer_Season")
lcs_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LCS/2020_Season/Summer_Playoffs")
write.table(lcs_fasedegrupos, file = "2020_2/LCS_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lcs_playoffs, file = "2020_2/LCS_po.csv", sep = ";", na = "", row.names = FALSE)

lec_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LEC/2020_Season/Summer_Season")
lec_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LEC/2020_Season/Summer_Playoffs")
write.table(lec_fasedegrupos, file = "2020_2/LEC_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lec_playoffs, file = "2020_2/LEC_po.csv", sep = ";", na = "", row.names = FALSE)
  
lpl_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LPL/2020_Season/Summer_Season")
lpl_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LPL/2020_Season/Summer_Playoffs")
write.table(lpl_fasedegrupos, file = "2020_2/LPL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lpl_playoffs, file = "2020_2/LPL_po.csv", sep = ";", na = "", row.names = FALSE)
  
lcl_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LCL/2020_Season/Summer_Season")
lcl_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LCL/2020_Season/Summer_Playoffs")
write.table(lcl_fasedegrupos, file = "2020_2/LCL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lcl_playoffs, file = "2020_2/LCL_po.csv", sep = ";", na = "", row.names = FALSE)

ljl_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LJL/2020_Season/Summer_Season")
ljl_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LJL/2020_Season/Summer_Playoffs")
write.table(ljl_fasedegrupos, file = "2020_2/LJL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(ljl_playoffs, file = "2020_2/LJL_po.csv", sep = ";", na = "", row.names = FALSE)

lla_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/LLA/2020_Season/Closing_Season")
lla_playoffs = scrape_esportspedia("https://lol.gamepedia.com/LLA/2020_Season/Closing_Playoffs")
write.table(lla_fasedegrupos, file = "2020_2/LLA_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(lla_playoffs, file = "2020_2/LLA_po.csv", sep = ";", na = "", row.names = FALSE)

opl_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/OPL/2020_Season/Split_2")
opl_playoffs = scrape_esportspedia("https://lol.gamepedia.com/OPL/2020_Season/Split_2_Playoffs")
write.table(opl_fasedegrupos, file = "2020_2/OPL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(opl_playoffs, file = "2020_2/OPL_po.csv", sep = ";", na = "", row.names = FALSE)
  
pcs_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/PCS/2020_Season/Summer_Season")
pcs_playoffs = scrape_esportspedia("https://lol.gamepedia.com/PCS/2020_Season/Summer_Playoffs")
write.table(pcs_fasedegrupos, file = "2020_2/PCS_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(pcs_playoffs, file = "2020_2/PCS_po.csv", sep = ";", na = "", row.names = FALSE)

tcl_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/TCL/2020_Season/Summer_Season")
tcl_playoffs = scrape_esportspedia("https://lol.gamepedia.com/TCL/2020_Season/Summer_Playoffs")
write.table(tcl_fasedegrupos, file = "2020_2/TCL_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(tcl_playoffs, file = "2020_2/TCL_po.csv", sep = ";", na = "", row.names = FALSE)

vcs_fasedegrupos = scrape_esportspedia("https://lol.gamepedia.com/VCS/2020_Season/Summer_Season")
vcs_playoffs = scrape_esportspedia("https://lol.gamepedia.com/VCS/2020_Season/Summer_Playoffs")
write.table(vcs_fasedegrupos, file = "2020_2/VCS_fg.csv", sep = ";", na = "", row.names = FALSE)
write.table(vcs_playoffs, file = "2020_2/VCS_po.csv", sep = ";", na = "", row.names = FALSE)



