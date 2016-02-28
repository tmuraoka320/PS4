###
### Problem Set 04
### Taishi Muraoka
### March 03
###



library(rvest)

wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract 
## the tables. You need to subset temp to find the data you're interested 
## in (HINT: html_table())

result_table <- wikiURL %>% 
  read_html %>% 
  html_nodes("table") %>% 
  .[[2]] %>%
  html_nodes("tr") %>%
  html_nodes("td") 

year <- as.numeric(result_table[seq(2, length(result_table), by=11)] %>% html_text())

winner <- as.character(result_table[seq(3, length(result_table), by=11)] %>% 
                         html_nodes("a") %>% 
                         html_text())

winner_party <- as.character(result_table[seq(4, length(result_table), by=11)] %>% 
                               html_text())

pv_percent <- as.numeric(substring(result_table[seq(5, length(result_table), by=11)] %>% 
                                     html_text(), 
                                   first=1, last=5))/100

pv_percent_margin_temp <- result_table[seq(6, length(result_table), by=11)] %>% 
  html_text()

cleanup1 <- function(x){
  vec1 <- unlist(strsplit(x, split="%"))
  vec2 <- vec1[length(vec1)]
  vec3 <- unlist(strsplit(vec2, split="^"))
  vec4 <- vec3[length(vec3)]
  vec5 <- gsub("'", "-", vec4)
  vec6 <- as.numeric(vec5)/100
  return(vec6)
}

pv_percent_margin <- as.vector(sapply(pv_percent_margin_temp, cleanup1))

pv_temp <- result_table[seq(7, length(result_table), by=11)] %>% html_text()

pv <- as.numeric(gsub(",", "", pv_temp))

loser <- as.character(result_table[seq(9, length(result_table), by=11)] %>% 
                        html_nodes("a") %>% 
                        html_text())

loser_party <- as.character(result_table[seq(10, length(result_table), by=11)] %>% 
                              html_text())

turnout <- as.numeric(substring(result_table[seq(11, length(result_table), by=11)] %>% 
                                  html_text(),
                                first=1, last=5))/100

result_df <- data.frame(year, winner, winner_party, pv_percent, 
                              pv_percent_margin, pv, loser, loser_party, turnout)

unique(result_df$winner_party)

party_color <- ifelse(result_df$winner_party=="D.-R.", rgb(0,1,0,0.5),
                      ifelse(result_df$winner_party=="Rep.", rgb(1,0,0,0.5), 
                             ifelse(result_df$winner_party=="Dem.", rgb(0,0,1,0.5),
                                    rgb(1,1,0,0.5))))

pdf("Muraoka_PloblemSet04plot.pdf", height=8, width=7.5)

par(mfrow=c(2,1))

plot(result_df$year, result_df$turnout, pch=20, cex=2, col=party_color,
     main="Figure 1. Changes in Turnout Rate over time", 
     xlab="Year", ylab="Turnout Rate", ylim=c(0, 1))

o1 <- order(result_df$year)

lines(result_df$year[o1], result_df$turnout[o1], col="gray")

legend("bottomright", 
       legend=c("Democratic Win", "Republican Win", "Democratic-Republican Win", 
                "Whig Win"), 
       bty="n", pch=c(20), 
       col=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5), rgb(0,1,0,0.5), rgb(1,1,0,0.5)))

withoutDR <- subset(result_df, result_df$winner_party!="D.-R.")

party_color2 <- ifelse(withoutDR$winner_party=="Rep.", rgb(1,0,0,0.5), 
                       ifelse(withoutDR$winner_party=="Dem.", rgb(0,0,1,0.5),
                              rgb(1,1,0,0.5)))

plot(withoutDR$turnout, withoutDR$pv_percent_margin, col=party_color2, pch=20,
     main="Figure 2. Relationship between Turnout Rate and the Margin of Victory\n(The 1824 Election is Excluded Due to very Low Turnout Rate)",
     xlab="Turnout Rate", ylab="Margin of Victory")

pl <- loess(pv_percent_margin ~ turnout, data=withoutDR, span=10)

o2 <- order(withoutDR$turnout)

lines(pl$x[o2], pl$fitted[o2], lwd=2)

win_candid <- data.frame("candid"=result_df$winner, "party"=result_df$winner_party)

lose_candid <- data.frame("candid"=result_df$loser, "party"=result_df$loser_party)

all_candid <- unique(rbind(win_candid, lose_candid))

all_candid$firstname <- sapply(as.character(all_candid$candid), 
                               function(x){unlist(strsplit(x, split=" "))[1]})

table(all_candid$firstname)



dev.off()