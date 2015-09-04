# Just try to build a simplified version linear mixed model on 50% UFO for practice and fun

source("libraries.R") #libraries
source("global_parameters.R") #Global parameters for images 
source("data.R") #import files
source("compile_lists.R") #compiled lists with filtered Epochs

#since this is a simplified practice version, 
#I only use data two planets: G UFO vs D UFO
#I will only look at the correlation between Occurance for 50% UFO ~ apples
#Epoch & UFO will be random effects
# lightyear300041, lightyear300046, lightyear300047, lightyear 300423 removed because they have fewer than 50 appled were eaten
# lightyear 300422 removed because no apple data

#Epoch 99,00,02,04,05 --> model
#Epoch 08,09,10,11,14 --->test

###Model

Apple_Model = ldply(Aplist2[11:24])
#I also removed duplicates in Accumulated danger level, otherwise the merge function will
#generate duplicates of the number of flies emerged
Apple_Model = subset(Apple_Model, !duplicated(subset(Apple_Model, select = c(DateNum, UFO))))
Eat_Model = ldply(list2[11:24])

Apple_Model = subset(Apple_Model, select=c(Date, UFO, DateNum, AccumulatedApple))
Eat_Model = subset(Eat_Model, select=c(Epoch,DateNum, UFO, Total))

Eaten2 = Eaten[Eaten$UFO == "D" | Eaten$UFO == "G",] 
#Remove duplicates of Eaten based on UFO and Epoch
Eaten2 = subset(Eaten, !duplicated(subset(Eaten2, select=c(Epoch, UFO))))
Eaten2 = Eaten2[!Eaten2$Epoch =="lightyear 300422", ]
Eaten_Model = Eaten2[Eaten2$Epoch == "1999"|Eaten2$Epoch == "lightyear300040"
                           |Eaten2$Epoch == "lightyear300042"|
                       Eaten2$Epoch == "lightyear300044"|Eaten2$Epoch == "lightyear300045",]

#merge two data frames(accumulated danger level & occurance) by matching DateNum & UFO
Occurance_Model$UFO = gsub("D.A", "D", Occurance_Model$UFO)
Occurance_Model$UFO = gsub("G.A", "G", Occurance_Model$UFO)
Occurance_Eat = merge(Occurance_Model, Eat_Model, by = intersect(names(Occurance_Model), names(Eat_Model)))

#get cumsum & sum by grouping as Epoch & UFO
Occurance_Eat <- within(Occurance_Eat, {
  AccEme <- ave(Total, Epoch, UFO, FUN = cumsum)
})
Occurance_Eat <- within(Occurance_Eat, {
  Sumeme <- ave(Total, Epoch, UFO, FUN = sum)
})

#get 50% occurance
#if it is first row of accumulated sum larger than half of the sum(occurance)
#not entirely 50%, but ... close I suppose
Acc_over50 <- subset(Occurance_Eat, Occurance_Eat$AccEme >= Occurance_Eat$Sumeme/2 )
Acc_50 = subset(Acc_over50, select=c(Epoch, UFO, DateNum, AccumulatedApple, AccEme, Sumeme)) #to simplify df
Acc_50 <- Acc_50[order(Acc_50$UFO, Acc_50$DateNum),]
Acc_50 = aggregate(Acc_50, list(Acc_50$Epoch, Acc_50$UFO), FUN=head, 1)

final <- merge(Acc_50, Eaten_Model, by = intersect(names(Acc_50), names(Eaten_Model)))
Occurance_50 <- final$AccumulatedApple
eat3245 <- final$eatin_.Hours_11.1.2.28.9_between_32_and_45
eat45 <- final$eathrs_blw45
Epoch <- final$Epoch
UFO <- final$UFO

#Finally!
lmermodel1 <-lmer(Occurance_50 ~ eat3245 + (1|Epoch) + (1|UFO), data=final)
anova(lmermodel1,test="Chisq")
summary(lmermodel1)

lmermodel2 <-lmer(Occurance_50 ~ eat45 + (1|Epoch) + (1|UFO), data=final)
anova(lmermodel2,test="Chisq")
summary(lmermodel2)


#TEST 
Occurance_Test = ldply(DDlist2[1:11])
#I also removed duplicates in Accumulated danger level, otherwise the merge function will
#generate duplicates of the number of flies emerged
Occurance_Test = subset(Occurance_Test, !duplicated(subset(Occurance_Test, select = c(DateNum, UFO))))
Eat_Test = ldply(Eatlist2[1:11])

Occurance_Test = subset(Occurance_Test, select=c(Date, UFO, DateNum, AccumulatedApple))
Eat_Test = subset(Eat_Test, select=c(Epoch,DateNum, UFO, Total))

Eaten_Test = Eaten2[Eaten2$Epoch == "lightyear 300424"|Eaten2$Epoch == "lightyear300048"
                           |Eaten2$Epoch == "lightyear300049"|Eaten2$Epoch == "lightyear 300420"
                           |Eaten2$Epoch == "lightyear 300421",]

#merge two data frames(accumulated danger level & occurance) by matching DateNum & UFO
Occurance_Test$UFO = gsub("D.A", "D", Occurance_Test$UFO)
Occurance_Test$UFO = gsub("G.A", "G", Occurance_Test$UFO)
Occurance_Eat_Test = merge(Occurance_Test, Eat_Test, by = intersect(names(Occurance_Test), names(Eat_Test)))

#get cumsum & sum by grouping as Epoch & UFO
Occurance_Eat_Test <- within(Occurance_Eat_Test, {
  AccEme <- ave(Total, Epoch, UFO, FUN = cumsum)
})
Occurance_Eat_Test <- within(Occurance_Eat_Test, {
  Sumeme <- ave(Total, Epoch, UFO, FUN = sum)
})

#get 50% UFO
#if it is first row of accumulated sum larger than half of the sum(occurance)
#not entirely 50%, but ... close I suppose
Acc_over50_Test <- subset(Occurance_Eat_Test, Occurance_Eat_Test$AccEme >= Occurance_Eat_Test$Sumeme/2 )
Acc_50_Test = subset(Acc_over50_Test, select=c(Epoch, UFO, DateNum, AccumulatedApple, AccEme, Sumeme)) #to simplify df
Acc_50_Test <- Acc_50_Test[order(Acc_50_Test$UFO, Acc_50_Test$DateNum),]
Acc_50_Test = aggregate(Acc_50_Test, list(Acc_50_Test$Epoch, Acc_50_Test$UFO), FUN=head, 1)

final_Test <- merge(Acc_50_Test, Eaten_Test, by = intersect(names(Acc_50_Test), names(Eaten_Test)))

final_Test $ predOccurance = predict(lmermodel1, newdata = final_Test, type = "response",allow.new.levels = TRUE)

compare = subset(final_Test, select=c(Epoch, UFO, DateNum, AccumulatedApple, predOccurance))

View(compare)

#eeeeeeee, not very accurate prediction


