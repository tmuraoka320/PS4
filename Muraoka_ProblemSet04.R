###
### Problem Set 04
### Taishi Muraoka
### March 03
###



##
## 1. Extract the table and plot something
##

library(rvest)

wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract 
## the tables. You need to subset temp to find the data you're interested 
## in (HINT: html_table())

# here I decide to extract td tabs and create a vector for each column, instead of
# table because, if I just extract table, elements are not nicely organized

result_table <- wikiURL %>% 
  read_html %>% 
  html_nodes("table") %>% # read table
  .[[2]] %>% # select the second one
  html_nodes("tr") %>% # get all tr tabs
  html_nodes("td")# get all td tabls

# get year
year <- as.numeric(result_table[seq(2, length(result_table), by=11)] %>% html_text())

# ger the name of the winner
winner <- as.character(result_table[seq(3, length(result_table), by=11)] %>% 
                         html_nodes("a") %>% 
                         html_text())

# get the party name of the winner
winner_party <- as.character(result_table[seq(4, length(result_table), by=11)] %>% 
                               html_text())

# get popular vote percentage
pv_percent <- as.numeric(substring(result_table[seq(5, length(result_table), by=11)] %>% 
                                     html_text(), 
                                   first=1, last=5))/100

# get the marin of victory for popular vote percentage 
pv_percent_margin_temp <- result_table[seq(6, length(result_table), by=11)] %>% 
  html_text() # first just extract text

hat <- substr(pv_percent_margin_temp[1], start=8, stop=8) # extract "^"

comma <- substr(pv_percent_margin_temp[1], start=9, stop=9) # extract "'"

cleanup1 <- function(x){ # function to clean up pv_percent_margin_temp
  vec1 <- unlist(strsplit(x, split="%")) # delete all %
  vec2 <- vec1[length(vec1)] # get the last element of each vec1
  vec3 <- unlist(strsplit(vec2, split=hat)) # split by "^"
                                            # which is applied to the first four
                                            # elements (- sign seems to cause the 
                                            # issue)
  vec4 <- vec3[length(vec3)] # get the last element of each vec3
  vec5 <- gsub(comma, "-", vec4) # change "'" to "-"
  vec6 <- as.numeric(vec5)/100 # change to a 0-1 scale
  return(vec6)
}

# apply the function above
pv_percent_margin <- as.vector(sapply(pv_percent_margin_temp, cleanup1))

# get popular vote
pv_temp <- result_table[seq(7, length(result_table), by=11)] %>% html_text()

pv <- as.numeric(gsub(",", "", pv_temp)) # delete ","

# get popular voter margin
pv_margin_temp <- result_table[seq(8, length(result_table), by=11)] %>% html_text()

hat <- substr(pv_margin_temp[1], start=8, stop=8) # extract "^"

comma <- substr(pv_margin_temp[1], start=9, stop=9) # extract "'"

cleanup2 <- function(x){ # function to clean up pv_margin_temp
  vec1 <- gsub(comma, "-", x) # replace "'" with "-"
  vec2 <- gsub(",", "", vec1) # delete ","
  vec3 <- unlist(strsplit(vec2, split=hat)) # split by "^"
  if(length(vec3)==2){ # if, after split by "^", length is 2
    vec4 <- vec3[2] # get the second element
  }
  else{ # if length is 1
    vec4 <- vec3 # just get the same element
  }
  vec5 <- unlist(strsplit(vec4, split="")) # split to sigle number
  while(vec5[1]=="0"){ # while the first element is 0
      if(vec5[1]=="0" & vec5[2]=="0"){ # if the first and second elements are 0
        vec5 <- vec5[2:length(vec5)] # delete the first numbers
      }
      else{ # if only the first number is 0
        vec5 <- paste(vec5[2:length(vec5)], collapse="") # delete the first number
      }
  }
  vec6 <- paste(vec5, collapse="") # merge numbers
  if(unlist(strsplit(vec6, split=""))[1] != "-"){ # if the first element is not "-"
    vec_len <- length(unlist(strsplit(vec6, split=""))) # get the length of number
    vec6 <- substr(vec6, start=vec_len/2+1, stop=vec_len)
                       # just use the second half of numbers
  }
  return(vec6) # this is the margin of popular vote
}

# apply the function above
pv_margin <- as.numeric(sapply(pv_margin_temp, cleanup2))

# get the name of the loser
loser <- as.character(result_table[seq(9, length(result_table), by=11)] %>% 
                        html_nodes("a") %>% 
                        html_text())

# get the party name of the loser
loser_party <- as.character(result_table[seq(10, length(result_table), by=11)] %>% 
                              html_text())

# get turnout rate
turnout <- as.numeric(substring(result_table[seq(11, length(result_table), by=11)] %>% 
                                  html_text(),
                                first=1, last=5))/100 # change to a 0-1 scale

# combine all the vectors above and create data.frame
result_df <- data.frame(year, winner, winner_party, pv_percent, 
                        pv_percent_margin, pv, pv_margin, loser, loser_party, 
                        turnout)

rownames(result_df) <- 1:dim(result_df)[1] # clean up row names

unique(result_df$winner_party) # check unique party names

party_color <- ifelse(result_df$winner_party=="D.-R.", rgb(0,1,0,0.5),
                      ifelse(result_df$winner_party=="Rep.", rgb(1,0,0,0.5), 
                             ifelse(result_df$winner_party=="Dem.", rgb(0,0,1,0.5),
                                    rgb(1,1,0,0.5)))) # set color for each party

pdf("Muraoka_PloblemSet04plot.pdf", height=13, width=8) # setting pdf output

par(mfrow=c(3,1))

# Figure 1 shows changes in turnout rate overime. It also shows which party won the
# election (blue=Democratic, red=Republican, green=D-R, yellow=Whig). In 1824, 
# turnout rate was quite low. Around the 1840s to 1900s, turnout rate was pretty
# high (around 75%). However, after then, turnout rate steeply declined, and it 
# has been flactuating around 50% for the last eight decades.

plot(result_df$year, result_df$turnout, pch=20, cex=2, col=party_color,
     main="Figure 1. Changes in Turnout Rate overtime", 
     xlab="Year", ylab="Turnout Rate", ylim=c(0, 1))

o1 <- order(result_df$year)

lines(result_df$year[o1], result_df$turnout[o1], col="gray") # connect dots

legend("bottomright", 
       legend=c("Democratic Win", "Republican Win", "Democratic-Republican Win", 
                "Whig Win"), 
       bty="n", pch=c(20), 
       col=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5), rgb(0,1,0,0.5), rgb(1,1,0,0.5)))
       # add legend

withoutDR <- subset(result_df, result_df$winner_party!="D.-R.")
                    # exclude the 1824 election

party_color2 <- ifelse(withoutDR$winner_party=="Rep.", rgb(1,0,0,0.5), 
                       ifelse(withoutDR$winner_party=="Dem.", rgb(0,0,1,0.5),
                              rgb(1,1,0,0.5))) # set new colors

# Figure 2 shows the relationship between turnout rate and the margin of victory.
# Here, the result of 1984 election is excluded because of its very low turnout rate.
# The loess smoother (black line) suggests the moderately negative link between 
# turnout rate and the margin of victory. What this implies may be that as turnout 
# rate increases, the election becomes more tightly contested.

plot(withoutDR$turnout, withoutDR$pv_percent_margin, col=party_color2, pch=20,
     main="Figure 2. Relationship between Turnout Rate and the Margin of Victory",
     xlab="Turnout Rate", ylab="Margin of Victory")

pl <- loess(pv_percent_margin ~ turnout, data=withoutDR, span=10) # fit loess

o2 <- order(withoutDR$turnout)

lines(pl$x[o2], pl$fitted[o2], lwd=2) # plot a loess curve

win_candid <- data.frame("candid"=result_df$winner, "party"=result_df$winner_party)
                         # extract winner names and their party affiliation

lose_candid <- data.frame("candid"=result_df$loser, "party"=result_df$loser_party)
                          # extract loser names and their party affiliation

all_candid <- unique(rbind(win_candid, lose_candid)) # combine the two and
                                                     # eliminate duplicates

all_candid$first <- sapply(as.character(all_candid$candid),
                           function(x){unlist(strsplit(x, split=" "))[1]})
                           # get the first names of candidates

table(all_candid$first)

drw_candid <- all_candid[all_candid$party=="Dem."|all_candid$party=="Rep."|
                           all_candid$party=="Whig"|all_candid$party=="D.-R.",]
                         # subset to candidates from Democratic, Republican, Whig,
                         # or D-R

table(drw_candid$first)

library(igraph)

name_party <- graph.data.frame(drw_candid[,2:3]) # get a igraph object
                                                 # with cndidates' first names and 
                                                 # party affiliation

V(name_party)$type <- V(name_party)$name %in% drw_candid[,2] # assign names

name_party_mat <- as.matrix(get.adjacency(name_party, sparse=FALSE))[1:4,5:49]
                            # get a bipartite matrix (party*candidate's first name)

name_party_mat <- name_party_mat[,apply(name_party_mat,2, sum)>1]
                  # just use first names that appear more than once

name_party_mat # this is the matrix party*candidate's first name

library(MASS)

name_party_crp <- corresp(name_party_mat, nf=4) # correspondence analysis
                                                # between candidates' first names
                                                # that have appeared more than once
                                                # in the US hisotry and their party
                                                # affiliation

par(mar=c(3.1,4.1,5.2,2.1))

# Are particular first names associated with political parties' presidential
# candidates? I use first names of candidates who won or lost the presidential
# election and examine which name is associated with which party. To do so, I just
# focus on candidates from the Democratic party, Republican party, Whig party, and
# Democratic-Republican party, which are only parties that appear more than once in
# the table. Further, of the candidates from these four parties, I just focus on
# candidates' names that appear more than once in the table, which include 
# Al, Andrew, Franklin, George, James, John, William, and Winfield. To reiterate
# the question, I explore how these names are associated with each party. The
# party*first name matrix I use is "name_party_mat" above.

# Figure 3 shows the result of correspondence analysis. It suggest that George and
# James are strongly associated with the Republican party, whereas Al, John, and
# Franklin are associated with the Democratic party. Further, there is some
# associaiton between Andrew and the Democratic-Republican party and between
# Winfield and the Whig party. William is located somewhere between the Democratic
# and Whig party. As for the relationship between parties, the Democratic and
# Republican parties are relatively close. On the other hand, the Whig and
# Democratic-Republican parties are distant from the other parties. In short, 
# Figure 3 suggests some affinity between party and presidential candidates' 
# first names.

biplot(name_party_crp, xlim=c(-1.7, 2), ylim=c(-1.5, 0.7),
       col=c(rgb(0.1,0.1,0.1,0.8), rgb(0.2,0.7,0.4,0.8)), cex=c(1.3,1),
       main="Figure3. Correspondence Analysis of \nCandidates' First Names and Party Affiliation\n")

dev.off()



##
## 2. Extract and merge the table
##

library(rvest)

wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

ec_table <- wikiURL2 %>% 
  read_html %>% 
  html_nodes("table") %>% # read table
  .[[3]] %>% # select the second one
  html_table() # get the table

# change column names
colnames(ec_table)[2] <- "year"

colnames(ec_table)[4] <- "other"

# change year to numeric
ec_table$year <- as.numeric(substring(ec_table$year, first=1, last=4))

# subset ec_table so that its years are consistent with result_df
ec_table <- ec_table[ec_table$year %in% result_df$year,]

aed <- substr(ec_table$Winner[1], start=44, stop=47) # extract "â???" " 

get_number1 <- function(x){ # function to get the winner's number of electoral college
  second_ele <- unlist(strsplit(x, split=aed))[2] # split by "â???" " and
                                                  # get the second element
  first_ele <- unlist(strsplit(second_ele, split="[", fixed=TRUE))[1]
                      # split by "[" and get the first element
  return(as.numeric(first_ele)) # return the number
}

# apply the function above to get the winners' number of electoral college 
ec_table$winner_ec <- sapply(ec_table$Winner, get_number1)

get_number2 <- function(x, y){ # function to get the loser's number of electoral college
  ele1 <- unlist(strsplit(x, split="\n")) # split by "\n"
  ele2 <- unlist(strsplit(ele1, split="[", fixed=TRUE)) # split by "["
  ele3 <- unlist(strsplit(ele2, split=" ")) # split by " "
  if(ele3[1]=="Horace"){ # except for the 1872 election, loser's number of electoral
                         # college is equal to the maximum number in the column
                         # "ec_table$other". I do not know what happend in 1872, but
                         # it is the only unusual pattern. Hence, if the lost
                         # candiate's first name is "Horace", ele4 is 0, which is the
                         # number of electoral college for Horace Greeley
    ele4 <- 0
  }
  else{ # if the lost candidate's first name is not "Horace", get the max number in
        # a vector
    ele4 <- max(na.omit(as.numeric(ele3))) # force a vector to be numeric, and get
                                           # the maximum number in it (this function
                                           # gives worning because when forcing a
                                           # vector to be numeric, NA is introduced
                                           # by coercion).
  }
  return(ele4)
}

# apply the function above and get the losers' number of electoral college
ec_table$loser_ec <- sapply(ec_table$other, get_number2)

# merge the two dataset by "year"
final_df <- merge(result_df, ec_table[,c(2,5,6)], by="year")

# export to .RData
save(final_df, file="Muraoka_ProblemSet04Dataset.RData")