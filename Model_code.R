
setwd("C:/Users/Roshan.Zubair/Desktop/Purchase Behaviour")

#data prep

chaid_data<-read.csv("Analytic_Data_PB.csv")
store_info<-read.csv("store_info.csv")
ga_remove<-read.csv("GA_remove.csv")

chaid_data_storeinfo<-merge(x=chaid_data,y=ga_remove,by.x="uniqueid",by.y="uniqueid",all.x=TRUE)
chaid_data<-subset(chaid_data_storeinfo,is.na(id),select=-id)

#removing >Oct stores

test <- read.delim("C:/Users/Roshan.Zubair/Desktop/Cric 2.0/Purchase behaviour analysis/Complete Open Store 6 27 with Lat Long Append.txt")
View(test)
test$Actual_Opening_Date = as.Date(test$Actual_Opening_Date, "%m/%d/%Y")
View(test)

test1<-subset(test,Actual_Opening_Date<"2015-10-01",select=uniqueid)
test1$id=1
chaid_data<-merge(x=chaid_data,y=test1,by.x="uniqueid",by.y="uniqueid",all.x=TRUE)
chaid_data<-chaid_data[complete.cases(chaid_data$id),]

## load chaid data

chaid_data_nocos<-subset(chaid_data, Revised_Location_Type!="COS")

chaid_data_rural<-subset(chaid_data,urbanicity_cat=="Rural"|urbanicity_cat=="Suburban")
chaid_data_urban<-subset(chaid_data,urbanicity_cat=="Metro_Urban")

write.csv(chaid_data_rural,"chaid_data_rural.csv")
write.csv(chaid_data_urban,"chaid_data_urban.csv")

quantile(chaid_data_rural$GA,c(.1,.9))
quantile(chaid_data_urban$GA,c(.1,.9))

chaid_data_rural_noexec<-subset(chaid_data_rural,GA>41 & GA<178)
chaid_data_urban_noexec<-subset(chaid_data_urban,GA>39 & GA<160)

## t test 

t.test(chaid_data_rural_noexec$Competitors,chaid_data_urban_noexec$Competitors)
t.test(chaid_data_rural_noexec$Grp2,chaid_data_urban_noexec$Grp2)
t.test(chaid_data_rural_noexec$pct_high_school_new,chaid_data_urban_noexec$pct_high_school_new)
t.test(chaid_data_rural_noexec$median_household_income_new,chaid_data_urban_noexec$median_household_income_new)
t.test(chaid_data_rural_noexec$pct_hispanic,chaid_data_urban_noexec$pct_hispanic)
t.test(chaid_data_rural_noexec$GA,chaid_data_urban_noexec$GA)


#model rural

#chaid model rural

#chaid variables
# Competitors+l5prizm_botpct+l5prizm_midpct+m5_varp6d
# +Grp2+pct_high_school_new+pct_hispanic+m5_varp6c
# +pct_two_race+sum_3_mile+Bigbox+median_household_income_new

model_rural<-lm(GA~ Competitors
                   #+l5prizm_botpct+l5prizm_midpct+m5_varp6d
                   +Grp2+pct_high_school_new
                   #+pct_two_race
                +sum_5_mile
                  ,data=chaid_data_rural_noexec)

summary(model_rural)

library(car)

vif(model_rural)
chaid_data_rural_noexec$pred <- predict.lm(model_rural,newdata = chaid_data_rural_noexec)
mean(abs(chaid_data_rural_noexec$GA-chaid_data_rural_noexec$pred)/chaid_data_rural_noexec$GA)

#Rural Stepwise model variable selection

null=lm(GA~1, data=chaid_data_rural_noexec)

model_rural<-lm(formula = GA ~ Age16+	m5_varp6f+
                  american_indian_new+	m5_varp6h+
                  asian_new+	population_white_new+	median_house_value_new+
                  Bigbox+	Prizm_17+	median_household_income_new+
                  chain.stores+	Prizm_22 +
                  Competitors+	Prizm_39+
                  Cricket_Market+	Prizm_46+	pct_african_american+
                  Cricket_SubMarket+	Prizm_50+	pct_american_ind+
                  Educational_institutions+	Prizm_56+	pct_asian+
                  Grp1+	Prizm_57+	pct_bachelor_degree_new+
                  Grp2+	Prizm_66+	pct_caucassian+
                  Grp3+	Prizm_7+	pct_female_new+
                  Grp4+
                  Grp5+	pct_high_school_new+
                  Grp6+	Service_and_Farm+	pct_hispanic+
                  Grp7+	pct_male_new+
                  hispanic_new+	sum_0_25_mile+
                  l5prizm_botpct+	sum_0_5_mile+	pct_prof_grad_degree_new+
                  l5prizm_lowpct+	sum_1_mile+	pct_renter_new
                  +	sum_2_mile+
                  sum_3_mile+	pct_unemployed_new+
                  sum_5_mile+	m5_varp6b+	sum_7_5_mile+	
                  m5_varp6c+	+	
                  m5_varp6d+	m5_varp6e + Revised_Location_Type
                , data = chaid_data_rural_noexec)

step(null, scope=list(lower=null, upper=model_rural), direction="both")

#rural stepwise model
chaid_data_rural_noexec$sum_3_mile=chaid_data_rural_noexec$sum_3_mile-1
model_rural<- lm(formula = GA ~ Cricket_Market + Grp2 + 
       pct_high_school_new  + Grp5 + (sum_3_mile) + 
       Competitors + pct_renter_new + pct_unemployed_new 
       , data = chaid_data_rural_noexec)

summary(model_rural)

vif(model_rural)
chaid_data_rural_noexec$pred <- predict.lm(model_rural,newdata = chaid_data_rural_noexec)
mean(abs(chaid_data_rural_noexec$GA-chaid_data_rural_noexec$pred)/chaid_data_rural_noexec$GA)

chaid_data_rural_noexec$cook <- data.frame(cooks.distance(model_rural))
chaid_data_rural_nocook <- chaid_data_rural_noexec[which(chaid_data_rural_noexec$cook <= (4/870)),]

mean(abs(chaid_data_rural_nocook$GA-chaid_data_rural_nocook$pred)/chaid_data_rural_nocook$GA)

var=c("pct_renter_new",	"Prizm_39",	"sum_0_25_mile","pct_male_new",	"Prizm_56",	"sum_0_5_mile",
      "pct_female_new",	"Prizm_7",	"sum_1_mile","pct_high_school_new",	"Prizm_66",	"sum_2_mile",
      "pct_bachelor_degree_new",	"Prizm_22",	"sum_3_mile","pct_prof_grad_degree_new",	"Prizm_57",	"sum_5_mile",
      "pct_unemployed_new",	"Prizm_46",	"sum_7_5_mile","median_house_value_new",	"Prizm_17",	"l5prizm_midpct",
      "population_density",	"Prizm_50",	"l5prizm_lowpct","population_white_new",	"median_household_income_new",	"l5prizm_botpct",
      "population_black_new",	"Service_and_Farm",	"m5_varp6b","american_indian_new",	"Age16",	"m5_varp6c",
      "asian_new",	"pct_caucassian",	"m5_varp6d","native_hawaiian_new",	"pct_african_american",	"m5_varp6e",
      "other_race_new",	"pct_american_ind",	"m5_varp6f","two_or_more_races_new",	"pct_asian",	"m5_varp6h",
      "hispanic_new",	"pct_hawaiian",	"tot_pop_segment",	"pct_other_race",	"Grp2",	"pct_two_race",	
      "Grp5",	"pct_hispanic",	"Grp7",	"Educational_institutions",	"Grp1",	"chain.stores",	"Grp3",	"Bigbox",	"Grp4",	"Competitors","Grp6")

#model urban

#chaid variables
# Competitors+Prizm_7+Prizm_56+Prizm_50
# +Grp2+pct_high_school_new+pct_hispanic+m5_varp6h+population_white_new
# +sum_7_5_mile+median_household_income_new+Age16+asian_new

model_urban<-lm(formula = GA ~ Competitors + pct_high_school_new + sum_7_5_mile + 
                  asian_new + Prizm_7, data = chaid_data_urban_noexec)

summary(model_urban)
vif(model_urban)
chaid_data_urban_noexec$pred <- predict.lm(model_urban,newdata = chaid_data_urban_noexec)
mean(abs(chaid_data_urban_noexec$GA-chaid_data_urban_noexec$pred)/chaid_data_urban_noexec$GA)

chaid_data_urban_noexec$cook <- data.frame(cooks.distance(model_urban))
chaid_data_urban_nocook <- chaid_data_urban_noexec[which(chaid_data_urban_noexec$cook <= (4/1310)),]
mean(abs(chaid_data_urban_nocook$GA-chaid_data_urban_nocook$pred)/chaid_data_urban_nocook$GA)

model_urban<-lm(formula = GA ~ Competitors + pct_hispanic + sum_7_5_mile + 
                asian_new + Grp2  + m5_varp6h, 
                data = chaid_data_urban_noexec)

summary(model_urban)
vif(model_urban)
chaid_data_urban_noexec$pred <- predict.lm(model_urban,newdata = chaid_data_urban_noexec)
mean(abs(chaid_data_urban_noexec$GA-chaid_data_urban_noexec$pred)/chaid_data_urban_noexec$GA)

chaid_data_urban_noexec$cook <- data.frame(cooks.distance(model_urban))
chaid_data_urban_nocook <- chaid_data_urban_noexec[which(chaid_data_urban_noexec$cook <= (4/1310)),]
mean(abs(chaid_data_urban_nocook$GA-chaid_data_urban_nocook$pred)/chaid_data_urban_nocook$GA)

#urban stepwise model variable selection 

null=lm(GA~1, data=chaid_data_urban_noexec)

model_urban<-lm(formula = GA ~ Age16+  population_black_new+	m5_varp6f+
                  american_indian_new+	m5_varp6h+
                  asian_new+	population_white_new+	median_house_value_new+
                  Bigbox+	Prizm_17+	median_household_income_new+
                  chain.stores+	Prizm_22+	native_hawaiian_new+
                  Competitors+	Prizm_39+	other_race_new+
                  Cricket_Market+	Prizm_46+	pct_african_american+
                  Cricket_SubMarket+	Prizm_50+	pct_american_ind+
                  Educational_institutions+	Prizm_56+	pct_asian+
                  Grp1+	Prizm_57+	pct_bachelor_degree_new+
                  Grp2+	Prizm_66+	pct_caucassian+
                  Grp3+	Prizm_7+	pct_female_new+
                  Grp4+	pct_hawaiian+
                  Grp5+	pct_high_school_new+
                  Grp6+	Service_and_Farm+	pct_hispanic+
                  Grp7+	pct_male_new+
                  hispanic_new+	sum_0_25_mile+	pct_other_race+
                  l5prizm_botpct+	sum_0_5_mile+	pct_prof_grad_degree_new+
                  l5prizm_lowpct+	sum_1_mile+	pct_renter_new+
                  l5prizm_midpct+	sum_2_mile+	pct_two_race+
                  sum_3_mile+	pct_unemployed_new+
                  sum_5_mile+	
                  m5_varp6b+	sum_7_5_mile+	
                  m5_varp6c+	tot_pop_segment+	
                  m5_varp6d+	
                  m5_varp6e+	two_or_more_races_new
                , data = chaid_data_urban_noexec)

summary(model_urban)
step(null, scope=list(lower=null, upper=model_urban), direction="both")

#urban stepwise model

model_urban<-lm(formula = GA ~ Cricket_Market + Competitors + Prizm_50 + Grp2 + 
                  Grp3 + chain.stores + asian_new  + Grp1 
                  , data = chaid_data_urban_noexec)

summary(model_urban)

vif(model_urban)  
chaid_data_urban_noexec$pred <- predict.lm(model_urban,newdata = chaid_data_urban_noexec)
mean(abs(chaid_data_urban_noexec$GA-chaid_data_urban_noexec$pred)/chaid_data_urban_noexec$GA)

chaid_data_urban_noexec$cook <- data.frame(cooks.distance(model_urban))
chaid_data_urban_nocook <- chaid_data_urban_noexec[which(chaid_data_urban_noexec$cook <= (4/1310)),]
mean(abs(chaid_data_urban_nocook$GA-chaid_data_urban_nocook$pred)/chaid_data_urban_nocook$GA)

#cook s distance 

chaid_data_rural_noexec$cook <- data.frame(cooks.distance(model_rural))
chaid_data_rural_nocook <- chaid_data_rural_noexec[which(chaid_data_rural_noexec$cook <= (4/1325)),]
mean(abs(chaid_data_rural_nocook$GA-chaid_data_rural_nocook$pred)/chaid_data_rural_nocook$GA)

#stepwise variable selection

null=lm(GA~1, data=chaid_data_rural_noexec)

model_rural<-lm(formula = GA ~ Age16+  population_black_new+	m5_varp6f+
                  american_indian_new+	m5_varp6h+
                  asian_new+	population_white_new+	median_house_value_new+
                  Bigbox+	Prizm_17+	median_household_income_new+
                  chain.stores+	Prizm_22+	native_hawaiian_new+
                  Competitors+	Prizm_39+	other_race_new+
                  Cricket_Market+	Prizm_46+	pct_african_american+
                  Cricket_SubMarket+	Prizm_50+	pct_american_ind+
                  Educational_institutions+	Prizm_56+	pct_asian+
                  Grp1+	Prizm_57+	pct_bachelor_degree_new+
                  Grp2+	Prizm_66+	pct_caucassian+
                  Grp3+	Prizm_7+	pct_female_new+
                  Grp4+	pct_hawaiian+
                  Grp5+	pct_high_school_new+
                  Grp6+	Service_and_Farm+	pct_hispanic+
                  Grp7+	pct_male_new+
                  hispanic_new+	sum_0_25_mile+	pct_other_race+
                  l5prizm_botpct+	sum_0_5_mile+	pct_prof_grad_degree_new+
                  l5prizm_lowpct+	sum_1_mile+	pct_renter_new+
                  l5prizm_midpct+	sum_2_mile+	pct_two_race+
                  	sum_3_mile+	pct_unemployed_new+
                  	sum_5_mile+	
                  m5_varp6b+	sum_7_5_mile+	
                  m5_varp6c+	tot_pop_segment+	
                  m5_varp6d+	
                  m5_varp6e+	two_or_more_races_new
                 , data = chaid_data_rural_noexec)

summary(model_rural)
vif(model_rural)  
chaid_data_rural_noexec$pred <- predict.lm(model_rural,newdata = chaid_data_rural_noexec)
mean(abs(chaid_data_rural_noexec$GA-chaid_data_rural_noexec$pred)/chaid_data_rural_noexec$GA)
step(null, scope=list(lower=null, upper=model_urban), direction="both")

#variable selection

set.seed(123)
boruta.train <- Boruta(GA~, data = chaid_data_rural, doTrace = 2)
print(boruta.train)

# random forest

library(randomForest)

r <- randomForest(GA~Age16+  population_black_new+	m5_varp6f+
                    american_indian_new+	population_density+	m5_varp6h+
                    asian_new+	population_white_new+	median_house_value_new+
                    Bigbox+	Prizm_17+	median_household_income_new+
                    chain.stores+	Prizm_22+	native_hawaiian_new+
                    Competitors+	Prizm_39+	other_race_new+
                  	Prizm_46+	pct_african_american+
                  	Prizm_50+	pct_american_ind+
                    Educational_institutions+	Prizm_56+	pct_asian+
                    Grp1+	Prizm_57+	pct_bachelor_degree_new+
                    Grp2+	Prizm_66+	pct_caucassian+
                    Grp3+	Prizm_7+	pct_female_new+
                    Grp4+	pct_hawaiian+
                    Grp5+	pct_high_school_new+
                    Grp6+	Service_and_Farm+	pct_hispanic+
                    Grp7+	pct_male_new+
                    hispanic_new+	sum_0_25_mile+	pct_other_race+
                    l5prizm_botpct+	sum_0_5_mile+	pct_prof_grad_degree_new+
                    l5prizm_lowpct+	sum_1_mile+	pct_renter_new+
                    l5prizm_midpct+	sum_2_mile+	pct_two_race+
                    sum_3_mile+	pct_unemployed_new+
                    m5_varp6b+	sum_7_5_mile+	
                    m5_varp6c+	tot_pop_segment+	
                    m5_varp6d+	
                    m5_varp6e+	two_or_more_races_new
                  ,chaid_data_urban_noexec)

k <- importance(r)

View(k)
Mape <-  mean((predict(r) - data_set_rf$Avg_per_month)/ data_set_rf$Avg_per_month)
cor(chaid_data_rural)

#varclus variable selection

vars <- sapply(chaid_data_rural_noexec, is.numeric)
test<-chaid_data_rural_noexec[,vars]
clust <- varclus(test)

# mixed model

library(nlme)
library(lme4)

model_rural<-lme(GA ~  Competitors+l5prizm_midpct+m5_varp6d
                +Grp2+pct_high_school_new+m5_varp6c
                +pct_two_race+sum_5_mile,random=~1|Zip_code/Cricket_Market
                , data = chaid_data_rural_noexec)

model_rural<-lme(GA~ Competitors
                 #+l5prizm_botpct+l5prizm_midpct+m5_varp6d
                 +Grp2+pct_high_school_new
                 +pct_two_race+sum_5_mile,random=~1|Cricket_SubMarket
                  , data = chaid_data_rural_noexec)

summary(model_rural)

chaid_data_rural_noexec$pred <- predict(model_rural,chaid_data_rural_noexec)
mean(abs(chaid_data_rural_noexec$GA-chaid_data_rural_noexec$pred)/chaid_data_rural_noexec$GA)

test<-coef(model_rural)

# k means clustering

set.seed(20)
irisCluster <- kmeans(chaid_data_rural_noexec[,8:70 ], 3, nstart = 10)

chaid_data_rural_noexec<-chaid_data_rural_noexec[order(chaid_data_rural_noexec$GA,decreasing = TRUE),]

plot(chaid_data_urban_noexec$Grp2,chaid_data_urban_noexec$GA)
