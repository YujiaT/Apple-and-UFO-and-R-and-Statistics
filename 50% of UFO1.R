# Just try to build a simplified version linear mixed model on 50% UFO for practice and fun

source("libraries.R") #libraries
source("global_parameters.R") #Global parameters for images 
source("data.R") #import files
source("compile_lists.R") #compiled lists with filtered years

#since this is a simplified practice version, 
#I only use data two planets: G UFO vs D UFO
#I will only look at the correlation between Occurance for 50% UFO ~ apples
#year & UFO will be random effects
# 2001, 2006, 2007, 2013 removed because they have fewer than 50 appled were eaten
# 2012 removed because no apple data

#year 99,00,02,04,05 --> model
#year 08,09,10,11,14 --->test

###Model

Apple_Model = ldply(Aplist2[11:24])
#I also removed duplicates in Accumulated degree days, otherwise the merge function will
#generate duplicates of the number of flies emerged
Apple_Model = subset(Apple_Model, !duplicated(subset(Apple_Model, select = c(DateNum, UFO))))
Eat_Model = ldply(list2[11:24])

Apple_Model = subset(Apple_Model, select=c(Date, UFO, DateNum, AccumulatedApple))
Eat_Model = subset(Eat_Model, select=c(Year,DateNum, UFO, Total))

Eaten2 = Eaten[Eaten$UFO == "D" | Eaten$UFO == "G",] 
#Remove duplicates of Eaten based on UFO and year
Eaten2 = subset(Eaten, !duplicated(subset(Eaten2, select=c(Year, UFO))))
Eaten2 = Eaten2[!Eaten2$Year =="2012", ]
Eaten_Model = Eaten2[Eaten2$Year == "1999"|Eaten2$Year == "2000"
                           |Eaten2$Year == "2002"|
                       Eaten2$Year == "2004"|Eaten2$Year == "2005",]

#merge two data frames(accumulated degree days & emergence) by matching DateNum & UFO
Occurance_Model$UFO = gsub("D.A", "D", Occurance_Model$UFO)
Occurance_Model$UFO = gsub("G.A", "G", Occurance_Model$UFO)
Occurance_Eat = merge(Occurance_Model, Eat_Model, by = intersect(names(Occurance_Model), names(Eat_Model)))

#get cumsum & sum by grouping as Year & UFO
Occurance_Eat <- within(Occurance_Eat, {
  AccEme <- ave(Total, Year, UFO, FUN = cumsum)
})
Occurance_Eat <- within(Occurance_Eat, {
  Sumeme <- ave(Total, Year, UFO, FUN = sum)
})

#get 50% emergence
#if it is first row of accumulated sum larger than half of the sum(emergence)
#not entirely 50%, but ... close I suppose
Acc_over50 <- subset(Occurance_Eat, Occurance_Eat$AccEme >= Occurance_Eat$Sumeme/2 )
Acc_50 = subset(Acc_over50, select=c(Year, UFO, DateNum, AccumulatedApple, AccEme, Sumeme)) #to simplify df
Acc_50 <- Acc_50[order(Acc_50$UFO, Acc_50$DateNum),]
Acc_50 = aggregate(Acc_50, list(Acc_50$Year, Acc_50$UFO), FUN=head, 1)

final <- merge(Acc_50, Eaten_Model, by = intersect(names(Acc_50), names(Eaten_Model)))
Occurance_50 <- final$AccumulatedApple
eat3245 <- final$eatin_.Hours_11.1.2.28.9_between_32_and_45
eat45 <- final$eathrs_blw45
Year <- final$Year
UFO <- final$UFO

#Finally!
lmermodel1 <-lmer(Occurance_50 ~ eat3245 + (1|Year) + (1|UFO), data=final)
anova(lmermodel1,test="Chisq")
summary(lmermodel1)

lmermodel2 <-lmer(Occurance_50 ~ eat45 + (1|Year) + (1|UFO), data=final)
anova(lmermodel2,test="Chisq")
summary(lmermodel2)


#TEST 
Occurance_Test = ldply(DDlist2[1:11])
#I also removed duplicates in Accumulated degree days, otherwise the merge function will
#generate duplicates of the number of flies emerged
Occurance_Test = subset(Occurance_Test, !duplicated(subset(Occurance_Test, select = c(DateNum, UFO))))
Eat_Test = ldply(Eatlist2[1:11])

Occurance_Test = subset(Occurance_Test, select=c(Date, UFO, DateNum, AccumulatedApple))
Eat_Test = subset(Eat_Test, select=c(Year,DateNum, UFO, Total))

Eaten_Test = Eaten2[Eaten2$Year == "2014"|Eaten2$Year == "2008"
                           |Eaten2$Year == "2009"|Eaten2$Year == "2010"
                           |Eaten2$Year == "2011",]

#merge two data frames(accumulated degree days & emergence) by matching DateNum & UFO
Occurance_Test$UFO = gsub("D.A", "D", Occurance_Test$UFO)
Occurance_Test$UFO = gsub("G.A", "G", Occurance_Test$UFO)
Occurance_Eat_Test = merge(Occurance_Test, Eat_Test, by = intersect(names(Occurance_Test), names(Eat_Test)))

#get cumsum & sum by grouping as Year & UFO
Occurance_Eat_Test <- within(Occurance_Eat_Test, {
  AccEme <- ave(Total, Year, UFO, FUN = cumsum)
})
Occurance_Eat_Test <- within(Occurance_Eat_Test, {
  Sumeme <- ave(Total, Year, UFO, FUN = sum)
})

#get 50% UFO
#if it is first row of accumulated sum larger than half of the sum(emergence)
#not entirely 50%, but ... close I suppose
Acc_over50_Test <- subset(Occurance_Eat_Test, Occurance_Eat_Test$AccEme >= Occurance_Eat_Test$Sumeme/2 )
Acc_50_Test = subset(Acc_over50_Test, select=c(Year, UFO, DateNum, AccumulatedApple, AccEme, Sumeme)) #to simplify df
Acc_50_Test <- Acc_50_Test[order(Acc_50_Test$UFO, Acc_50_Test$DateNum),]
Acc_50_Test = aggregate(Acc_50_Test, list(Acc_50_Test$Year, Acc_50_Test$UFO), FUN=head, 1)

final_Test <- merge(Acc_50_Test, Eaten_Test, by = intersect(names(Acc_50_Test), names(Eaten_Test)))

final_Test $ predOccurance = predict(lmermodel1, newdata = final_Test, type = "response",allow.new.levels = TRUE)

compare = subset(final_Test, select=c(Year, UFO, DateNum, AccumulatedApple, predOccurance))

View(compare)

#eeeeeeee, not very accurate prediction


