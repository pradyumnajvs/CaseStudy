# When using the raw UN data, please cite: Erik Voeten "Data and Analyses of Voting 
#in the UN General Assembly" Routledge Handbook of International Organization, 
#edited by Bob Reinalda (published May 27, 2013). 
#Available at SSRN: http://ssrn.com/abstract=2111149 
#When using the ideal point data, please cite: Bailey, M.A., Strezhnev, A. and Voeten, E., 2015. 
#Estimating dynamic state preferences from United Nations voting data. Journal of Conflict Resolution, p.0022002715595700.)


votes <- read.table("/Users/Leo/Documents/Case\ Studies/United\ Nations/RawVotingdata.csv" , sep=";", header = T)


install.packages("countrycode")
library(countrycode)
library(ggplot2)

head(votes)

#filter data, 

votes <- votes[votes$vote <= 3,]

#create column "year"

votes$year <- votes$session + 1945

head(votes)

#add country to the DF
#The following strings can be used as arguments for origin or
#destination: "cowc", "cown", "iso3c", "iso3n", "iso2c", "imf", "fips104", "fao", "ioc", "un", "wb", "country.name".
#The following strings can be used as arguments for destination only: "continent", "region"

votes$country <- countrycode(votes$ccode, "cown", "country.name")

head(votes)

str(votes)
#find total number of votes

str(votes)

total_votes <- nrow(votes)

total_votes

#find total percet of positive notes

percent_yes <- nrow(votes[votes$vote == 1,]) / nrow(votes)

percent_yes

#group by year

total_votes_by_year <- setNames(aggregate(votes$year, by = list(year = votes$year), FUN = "length"), c('Year', 'Total_Votes'))

positive_votes_by_year <- setNames(aggregate(votes[votes$vote == 1, "year"], by = list(year = votes[votes$vote == 1, "year"]), FUN = "length"), c('Year', 'Positive_Votes'))

abstain_votes_by_year <-  setNames(aggregate(votes[votes$vote == 2, "year"], by = list(year = votes[votes$vote == 2, "year"]), FUN = "length"), c('Year', 'Abstain_Votes'))

negative_votes_by_year <-  setNames(aggregate(votes[votes$vote == 3, "year"], by = list(year = votes[votes$vote == 3, "year"]), FUN = "length"), c('Year', 'Negative_Votes'))

#merging data frames with Reduce method

votes_by_year <-  Reduce(function(x, y) merge(x, y, all=TRUE), list(total_votes_by_year, positive_votes_by_year, abstain_votes_by_year, negative_votes_by_year))

head(votes_by_year)

#persent of Positive

votes_by_year$Percent_Positive <- round((votes_by_year$Positive_Votes / votes_by_year$Total_Votes), 2)

#persent of Abstain 

votes_by_year$Percent_Abstain <- round((votes_by_year$Abstain_Votes / votes_by_year$Total_Votes), 2)

#persent of Negative

votes_by_year$Percent_Negative <- round((votes_by_year$Negative_Votes / votes_by_year$Total_Votes), 2)

head(votes_by_year)


#visualiziong persent changes over years. 

ggplot(votes_by_year, aes(x = Year, y = Percent_Positive)) + geom_line() + geom_smooth()

ggplot(votes_by_year, aes(x = Year, y = Percent_Negative)) + geom_line() + geom_smooth()

ggplot(votes_by_year, aes(x = Year, y = Percent_Abstain)) + geom_line() + geom_smooth()

#find top 5 countries which vote mostly positive and mostly negative. 

#group by country

total_votes_by_country <- setNames(aggregate(votes$country, by = list(year = votes$country), FUN = "length"), c('Country', 'Total_Votes'))

positive_votes_by_country <- setNames(aggregate(votes[votes$vote == 1, "country"], by = list(year = votes[votes$vote == 1, "country"]), FUN = "length"), c('Country', 'Positive_Votes'))

negative_votes_by_country <- setNames(aggregate(votes[votes$vote == 3, "country"], by = list(year = votes[votes$vote == 3, "country"]), FUN = "length"), c('Country', 'Negative_Votes'))

#merging data frames with Reduce method

votes_by_country <-  Reduce(function(x, y) merge(x, y, all=TRUE), list(total_votes_by_country, positive_votes_by_country, negative_votes_by_country))

# Percent Positive by country

votes_by_country$Percent_Positive <- round((votes_by_country$Positive_Votes / votes_by_country$Total_Votes), 3)

# Percent Negative by country 

votes_by_country$Percent_Negative <- round((votes_by_country$Negative_Votes / votes_by_country$Total_Votes), 3)

#top 5 positive

#sorting by percent positive

sorted_Positive <- votes_by_country[order(votes_by_country$Percent_Positive, decreasing = T),]

#selecting top 

top5_Positive <- sorted_Positive[1:5, c(1, 5:6)]

#It's time to learn geography 

top5_Positive

#top5 negative

sorted_Negative <- votes_by_country[order(votes_by_country$Percent_Negative, decreasing = T),]

# selecting top

top5_Negative <- sorted_Negative[1:5, c(1, 5:6)]

#These looks more familiar :)

top5_Negative

#Concatenate dataframes

PosNeg <-  rbind(top5_Positive, top5_Negative)

#some visualization tricks. Things, which ggplot can do for you. 

votes_factor <- votes

votes_factor$vote <- factor(votes_factor$vote)

str(votes_factor)

#renamig factors

levels(votes_factor$vote)[levels(votes_factor$vote) == "1"] <- "Positive"
levels(votes_factor$vote)[levels(votes_factor$vote) == "2"] <- "Abstain"
levels(votes_factor$vote)[levels(votes_factor$vote) == "3"] <- "Negative"

#stacked bar 

ggplot(votes_factor, aes(year, fill=vote)) + geom_bar()

#grouped bar

ggplot(votes_factor[votes_factor$year > 1985,], aes(year, fill=vote)) + geom_bar(position = "dodge")

#visualizing top positive and top negative together, interesting stuff, but I don't know include this or no. Code need to be improved.

topFilter <- votes_factor$country %in% PosNeg$Country

voteFilter <- votes_factor$vote %in% c("Negative", "Positive")

votes_factor_filtered <- votes_factor[topFilter & voteFilter,]

ggplot(votes_factor_filtered, aes(country, fill = vote)) + geom_bar()