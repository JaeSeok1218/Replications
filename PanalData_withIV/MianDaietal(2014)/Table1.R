######################################
#Table1###############################
#First Way############################by JaeSeok
library(tidyverse)
library(modelsummary)
library(kableExtra)

#Filtering Data by dummies
filtered_data1 <- data_ap %>%
  filter(mono == 1)
filtered_data2 <- data_ap %>%
  filter(duo==1)
filtered_data3 <- data_ap %>%
  filter(comp==1)
#Make columns with filtered data
column1 <- filtered_data1$fgini     #Monopoly
column2 <- filtered_data2$fgini     #Duopoly
column3 <- filtered_data3$fgini     #Competitive
column4 <- data_ap$fgini            #For all mkt types
#Make a list
list_of_columns1 <- list(column1, column2, column3, column4)

#Make the length of columns to be the same
max_length <- max(lengths(list_of_columns1))
list_of_columns_padded <- lapply(list_of_columns1, function(x) {
  if (length(x) < max_length) {
    c(x, rep(NA, max_length - length(x)))
  } else {
    x
  }
})
df1 <- as.data.frame(list_of_columns_padded)


#HHI#################################
column5 <- filtered_data1$hhi
column6 <- filtered_data2$hhi
column7 <- filtered_data3$hhi
column8 <- data_ap$hhi

list_of_columns2 <- list(column5, column6, column7, column8)

max_length <- max(lengths(list_of_columns2))

list_of_columns_padded <- lapply(list_of_columns2, function(x) {
  if (length(x) < max_length) {
    c(x, rep(NA, max_length - length(x)))
  } else {
    x
  }
})

df2 <- as.data.frame(list_of_columns_padded)

#NC##############################
column9 <- filtered_data1$nc
column10 <- filtered_data2$nc
column11 <- filtered_data3$nc
column12 <- data_ap$nc

list_of_columns3 <- list(column9, column10, column11, column12)

max_length <- max(lengths(list_of_columns3))

list_of_columns_padded <- lapply(list_of_columns3, function(x) {
  if (length(x) < max_length) {
    c(x, rep(NA, max_length - length(x)))
  } else {
    x
  }
})

df3 <- as.data.frame(list_of_columns_padded)

datasummary1 <- datasummary(Heading("Monopoly") * column9 + Heading("Duopoly") * column10 + Heading("Competitive") * column11 + Heading("All") * column12 ~ mean + sd, 
                            fmt=3, data = df1, output='data.frame')
datasummary2 <- datasummary(Heading("Monopoly") * column5 + Heading("Duopoly") * column6 + Heading("Competitive") * column7 + Heading("All") * column8 ~ mean + sd, 
                            fmt=3, data = df2, add_columns = datasummary1[,c(2,3)], output='data.frame')
datasummary3 <- datasummary(Heading("Monopoly") * column1 + Heading("Duopoly") * column2 + Heading("Competitive") * column3 + Heading("All") * column4 ~ mean + sd, 
                            fmt=3, data = df3, add_columns = datasummary2[,c(2,3,4,5)], output = 'data.frame')

# Using kableExtra, generate Latex Command lines
kbl(datasummary3, booktabs = T, align = "c", caption = "PRICE DISPERSION BY MARKET STRUCTURE", "latex") %>%
  add_header_above(c(" ", "GINI" = 2, "HHI" = 2, "N" = 2)) %>%
  footnote(general = " $N$ represents carrier counts. Definitions of monopoly, duopoly, and competitive route are in note 19. 
           Standard deviations are in parentheses. Number of observations is 248,513", threeparttable = T)


