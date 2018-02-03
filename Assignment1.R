# TASK 1: MANIPULATION
#1.1
install.packages("ggplot2")
library(ggplot2)
data("diamonds")

#1.2
help(diamonds)
str(diamonds)

#1.3
#Subsetting the bestColor dataframe with color "D"
bestColor <- data.frame(subset(diamonds, color=="D"))
#Removing the color variable
bestColor <- subset(bestColor, select = -color)
#Ordering by first carat, then price in decreasing order
bestColor[with(bestColor, order(carat, price, decreasing = TRUE)),]
#Saving dataframe with row names in workdirectory
write.table(bestColor,"johannesmehlem.txt", sep="\t", row.names = TRUE, col.names = NA)
#Check the number of rows in the dataframe
NROW(bestColor)
#Check the number of variables in the dataframe
NCOL(bestColor)

#1.4
#Function with for loop
countESI2 <- function(n)  {
  k <- 0
  for(i in 1:n) {
    if(diamonds$color[i]=="E" & diamonds$clarity[i]=="SI2") {
      k <- k + 1  }
  }
  return(k)
}
countESI2(50)
countESI2(150)

#1.5
#Function avoiding for loop
countESI2alt <- function(n) {
  k <- 0
  diamondsColorSub <- diamonds$color[1:n]
  diamondsClaritySub <- diamonds$clarity[1:n]
  k <-ifelse(diamondsColorSub=="E" & diamondsClaritySub=="SI2", k + 1, 0) 
  sum(k)
}
countESI2alt(50)
countESI2alt(150)
  

#TASK 2: ANALYSIS
#2.1
#Sorting the diamonds by price
priceDiamonds<- diamonds[with(diamonds, order(price, decreasing = T)),]
#Isolating the row of the most expensive diamond
maxPriceDiamond <- priceDiamonds[1,]
#Calculating the size of the most expensive diamond
sizeMaxPriceDiamond <- maxPriceDiamond[,8] * maxPriceDiamond[,9] * maxPriceDiamond[,10]
sizeMaxPriceDiamond

#Finding the row of the diamond price USD 1344
rowUSDprice <- diamonds$price == "1344"
#Calculating the size of that specific diamond
sizeUSDprice <- diamonds$x[rowUSDprice] * diamonds$y[rowUSDprice] * diamonds$z[rowUSDprice]
sizeUSDprice

#2.2
#Subsetting for "Ideal" cut
idealCut <- subset(diamonds, cut == "Ideal")
#Ordering by price in decreasing order
idealCut <- idealCut[with(idealCut, order(price, decreasing = T)),]
#Showing the price of the first 7 rows of idealCut list
idealCut[1:7,7]

#2.3
#Finding the best clarity
head(diamonds$clarity)
#Subsetting for "Ideal" cut, best color and best clarity
CCCbest <- subset(diamonds, cut == "Ideal" & color == "E" & clarity == "SI2")
#Counting the number of rows of subsetted list for which the three given conditions are true
CCCbestcount <- NROW(CCCbest)
CCCbestcount

#2.4
#Creating a dataframe with clarity and color
clarityColordf <- data.frame(diamonds$clarity, diamonds$color)
clarityColor <- table(clarityColordf)
#Printing combination frequency table
clarityColor
#Finding the value of the most common combination
clarityColorMax <-max(clarityColor)
clarityColorMax

#2.5
#Selecting occurences color F and VS1 clarity
colorFVS1clarity <- clarityColor[5,3]
#Calculating probabily of occurence of selected combination
colorFVS1clarity / sum(clarityColor)


#TASK 3: CREATIVITY
#Plotting carat against price
plot(diamonds$carat, diamonds$price, type = "h", main = "Price over carat", 
     xlab = "Carat", ylab = "Price")
#Subsetting carat and price
caratPriceDf <- data.frame(diamonds$carat, diamonds$price)
#Using cut to create factors from a list of bins
caratBins <- cut(diamonds$carat, c(0, 0.5, 1, 1.5, 2, 5), 
    labels = c('<0.5', '0.5-1', '1-1.5', '1.5-2', '>2'))
priceBins<- cut(diamonds$price, c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000,30000), 
    labels = c('<2000', '2000-4000','4000-6000','6000-8000','8000 - 10000',
               '10000-12000', '12000-14000', '14000-16000','16000-18000', '>18000'))
caratPrice <- table(caratBins, priceBins)
caratPrice
write.csv(caratPrice,"caratPrice.csv", row.names = TRUE, col.names = NA)
#Percentage of diamonds less than 0.5 carat
caratLess0.5 <- sum(caratPrice[1,])
percCaratLess0.5 <- caratLess0.5/sum(caratPrice)
percCaratLess0.5
#Percentage of diamonds less than 1 carat
caratLess1 <- sum(caratPrice[1:2,])
percCaratLess1 <- caratLess1/sum(caratPrice)
percCaratLess1
#Barplot for "Diamond Carat to Price Relationship"
barplot(caratPrice, xlab="Price", ylab="Number of Diamonds", main = "Diamond Carat to Price Relationship", 
        col=c("yellow", "orange", "red", "green", "darkblue"), legend = c("<0.5 Carat", "0.5-1 Carat", 
        "1-1.5 Carat", "1.5-2 Carat", ">2 Carat"))
