# This is an R script for the final stage analysis on the relationship between SOE reform in 1998 and migration. It conducts two-way fixed effects analyses and various robust checks. 
# All source datasets can be found on my github page: https://github.com/ZIBOWANGKANGYU/SOE-Migration
# This script file can be found in https://github.com/ZIBOWANGKANGYU/SOE-Migration/blob/master/R%20script%20Kangyu%20(Mark)%20Wang%20kaw129%40ucsd.edu%20Data%20Analyst.R
Sys.setenv(LANG = "en")
library(xlsx)
library(stringi)
library(stargazer)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(haven)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(data.table)
library(gdata)
library(readxl)
library(plm)
library(gplots)
library(gmodels)
library(dotwhisker)
library(lmtest)
library(gridExtra)
library(repmis)
library(digest)
## Part 1: data loading and cleaning
# load in cleaned dataset on SOE reform and migration 
source_data("https://github.com/ZIBOWANGKANGYU/SOE-Migration/blob/master/cleaning.RData?raw=true")
# load in base map
source_data("https://github.com/ZIBOWANGKANGYU/SOE-Migration/blob/master/prefecture_92_99_map.RData?raw=true")
baseline<-as.data.frame(prefecture_92_99_map%>%select(-geometry))
# load in dataset of control variables
source_data("https://github.com/ZIBOWANGKANGYU/SOE-Migration/blob/master/prefecture_92_99_long.RData?raw=true")
controls<-prefecture_92_99_long
# set up pabel structure of cities dataframe
cities<-pdata.frame(total_cities,index=c("CITYGB","year"))
# post_t is a dummy variable that takes the value of 1 if year>=1998 and 0 otherwise
cities$post_t<-as.numeric(as.character(cities$year))>=1998
# write cleaned csv
write.csv(cities, file = "tables/cities.csv")

## Part 2: detecting outliers 
# linear regressions to find outliers: population. Outliers are caused by re-drawing of prefecure borders that made data on the same prefecture of different years not compatible. 
# linear regression of total population on linear time variable is conducted on each prefecture to get the R squared. 
city_unique<-unique(cities$CITYGB)
city_reg<-data.frame(city_unique)
city_reg$R2_pop<-NA
for (i in 1:length(city_unique)){
  lm_time<-lm(total_population_adm~year_linear, data=cities[as.character(cities$CITYGB)==as.character(city_unique[i]),])
  city_reg$R2_pop[as.character(city_reg$city_unique)==as.character(city_unique[i])]<-summary(lm_time)$r.squared
}
# create lagged total population and population change rate variables, the highest and lowest population change rates of each prefecture are alao gathered
cities$total_population_adm_lag<-plm::lag(cities$total_population_adm, 1)
cities$total_population_adm_change<-cities$total_population_adm-cities$total_population_adm_lag
cities$total_population_adm_change_rate<-cities$total_population_adm_change/cities$total_population_adm_lag
city_reg<-merge(city_reg, cities%>%group_by(CITYGB)%>%summarise(pop_chg_rate_max=max(total_population_adm_change_rate, na.rm=TRUE)), by.x="city_unique", by.y="CITYGB", all.x = TRUE)
city_reg<-merge(city_reg, cities%>%group_by(CITYGB)%>%summarise(pop_chg_rate_min=min(total_population_adm_change_rate, na.rm=TRUE)), by.x="city_unique", by.y="CITYGB", all.x = TRUE)
# Outliers are defined as the following: they will be excluded from regression analyses. 
city_reg$include_pop_adm<-TRUE
city_reg$include_pop_adm[(city_reg$R2_pop<=0.5 & (city_reg$pop_chg_rate_max>0.01|city_reg$pop_chg_rate_min<(-0.01)) | (city_reg$R2_pop>=0.8 & (city_reg$pop_chg_rate_max>0.05|city_reg$pop_chg_rate_min<(-0.05))))]<-FALSE

## Part 3: building dependent, independent and control variables
# SOE reform is defined as the change of number of SOE employees (zhigong)
cities$zhigong_number_adm_lag<-plm::lag(cities$zhigong_number_adm, 1)
cities$zhigong_number_adm_change<-cities$zhigong_number_adm-cities$zhigong_number_adm_lag
# change population and emlpoyee (zhigong) numbers to thousands
cities$total_population_adm_th<-cities$total_population_adm*10
cities$zhigong_number_adm_th<-cities$zhigong_number_adm*10
# The dependent variable is net migration as a proportion of 1994 population NM_to_1994, which is calculated using population and natural growth rate
cities_1994<-cities[cities$year==1994,]
city_reg<-merge(city_reg, cities_1994%>%select("CITYGB", "total_population_adm"), by.x="city_unique", by.y="CITYGB")
colnames(city_reg)[6]<-"total_population_adm_94"
cities<-merge(cities, city_reg%>%select("city_unique", "include_pop_adm", "total_population_adm_94"), by.x="CITYGB", by.y="city_unique")
cities<-pdata.frame(cities,index=c("CITYGB","year"))
cities$natural_growth_admin_lag<-plm::lag(cities$natural_growth_admin, 1)
cities$NM_to_1994<-(cities$total_population_adm-cities$total_population_adm_lag*(1+cities$natural_growth_admin_lag/1000))/cities$total_population_adm_94
# The independent variables is SOE reform in 1998 as a proportion to total population in 1994. Also get other variables in 1998
cities_1998<-cities[cities$year==1998,]
cities_1998$SOE_dec_98_abs<-(cities_1998$zhigong_number_adm-cities_1998$zhigong_number_adm_lag)
cities_1998$fdi_real_adm_98<-cities_1998$fdi_real_adm
cities_1998$gdp_adm_98<-cities_1998$gdp_adm
cities<-merge(cities, cities_1998%>%select("CITYGB", "SOE_dec_98_abs", "fdi_real_adm_98", "gdp_adm_98"), by="CITYGB")
cities$SOE_dec_98<-cities$SOE_dec_98_abs/cities$total_population_adm_94
# Similarly, get variables in 1997
cities_1997<-cities[cities$year==1997,]
cities_1997$fdi_real_adm_97<-cities_1997$fdi_real_adm
cities<-merge(cities, cities_1997%>%select("CITYGB", "fdi_real_adm_97"), by="CITYGB")

# add control variables
cities<-merge(cities, controls%>%select(CITYGB, year, SOE_zhigong, CE_zhigong), by=c("CITYGB", "year"), all.x = TRUE )
# For data quality issues, emlpoyee (zhigong) number is 1998 of province 13 is omitted
cities$SOE_zhigong[cities$year==1998 & cities$PROVGB==13]<-NA
cities$CE_zhigong[cities$year==1998 & cities$PROVGB==13]<-NA
# defining importance of non-state sectors
cities$nonstate_imp<-1-(cities$SOE_zhigong+cities$CE_zhigong)/(cities$total_population_adm*10000)

## Part 4: regression analyses
# two-way fixed effect: without controls
tw_0<-plm(NM_to_1994~SOE_dec_98*(year==1998), data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_0)
tw_1<-plm(NM_to_1994~SOE_dec_98*post_t, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_1)
tw_3<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995), data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_3)
stargazer(tw_0, tw_1, tw_3, out = "tables/baseline.html", column.labels= c("(a)", "(b)", "(c)"),column.separate = c(1, 1), dep.var.labels = "ratio of net migration to 1994 population",
          add.lines = list(c("Year fixed effect", "yes", "yes", "yes"), c("Prefecture fixed effect", "yes", "yes", "yes")), model.numbers = FALSE,
          title ="Two-way fixed effects, without controls", covariate.labels=c("SOE reform 1998*year 2002", "SOE reform 1998*year 2001", "SOE reform 1998*year 2000", "SOE reform 1998*year 1999", "SOE reform 1998*year 1998", "SOE reform 1998*year at and after 1998","SOE reform 1998*year 1997", "SOE reform 1998*year 1996", "SOE reform 1998*year 1995"),
          notes ="This table shows the two-way fixed effect regression results of net migration rate on the interaction term between SOE refrom in 1998 and time dummies. All models include year and prefecture fixed effects. Column (a) shows that SOE reform in 1998 is positively associated with out-migration in the same year. Column (b) and (c) shows that the impact of SOE reform in 1998 on migration in and after 1999 is not statistically significant.",
          style="apsr")
dwplotlabels<- c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "in and after 1998")
dwplot(list(tw_1, tw_3, tw_0), vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), size=10)+
  theme_bw() + xlab("Coefficient Estimate") + ylab("time fixed effect")+
  labs(caption = "Colored dots show the estimated coefficients of the interactions between SOE reform in 1998 and time dummies, \nand whiskers show confidence intervals. Spikes in year 1998 of equation (4) and (6) coefficients show the impact \nof 1998 SOE reform on migration in the same year. Positive coeffcient of equation (5) reflects an average positive \nimpact on out-migration in and after 1998 of SOE reform in 1998. Whiskers of pre-1998 coefficients of equation \n(6) intersecred with x axis, which shows parallel trend assumption is met.")+
  ggtitle("Coefficients on interaction terms of 1998 SOE downsizing and time fixed effect") +
  scale_color_brewer(name="model", labels=c("equation (5)", "equation (6)", "equation (4)"), palette="Dark2")+
  coord_flip()+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.caption = element_text(hjust = 0))+
  scale_y_discrete(labels= dwplotlabels)

# two-way fixed effect, with controls 
# GDP per capita
cities$gdppc_th<-cities$gdp_adm/cities$total_population_adm/1000
tw_5<-plm(NM_to_1994~SOE_dec_98*post_t+gdppc_th, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_5)
tw_6<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995)+gdppc_th, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_6)
# share of non-state sector
cities$gdppc<-cities$gdp_adm/cities$total_population_adm
tw_7<-plm(NM_to_1994~SOE_dec_98*post_t+nonstate_imp, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_7)
tw_8<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995)+nonstate_imp, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_8)
# add both 
tw_9<-plm(NM_to_1994~SOE_dec_98*post_t+gdppc_th+nonstate_imp, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_9)
tw_10<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995)+gdppc_th+nonstate_imp, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_10)
stargazer(tw_5, tw_6, tw_7, tw_8, tw_9, tw_10, out = "tables/controls.html", column.labels= c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),column.separate = c(1, 1, 1, 1 , 1, 1), dep.var.labels = "ratio of net migration to 1994 population",
          add.lines = list(c("Year fixed effect", "yes", "yes", "yes", "yes", "yes", "yes"), c("Prefecture fixed effect", "yes", "yes","yes", "yes", "yes", "yes")), model.numbers = FALSE,
          title ="two-way fixed effects with controls", covariate.labels=c("GDP per capita (thousand Chinese Yuan)", "non-state sector share of total population", "SOE reform 1998*post 1998", "SOE reform 1998*year 2002", "SOE reform 1998*year 2001", "SOE reform 1998*year 2000", "SOE reform 1998*year 1999", "SOE reform 1998*year 1998", "SOE reform 1998*year 1997", "SOE reform 1998*year 1996", "SOE reform 1998*year 1995"),
          notes ="This table adds control variables to regression functions as in table 3.2.1.1. Columns (a) and (b) show that the positive relationship between SOE reform in 1998 and out-migration in the same year remains after controlling for local GDP per capita. Colunms (c) to (f) show that after controlling for non-state sector share of total population, the effect sizes of SOE reforms in 1998 become a bit smaller. Non-state sector share in local economies is negatively associated with out-migration",
          style="apsr")
 
## Part 5: robust checks
# excluding Jiangsu province, which saw exogenous shock of increading FDI inflow
tw_0_JS<-plm(NM_to_1994~SOE_dec_98*(year==1998), data = cities[cities$include_pop_adm==TRUE & cities$EPROV!="Jiangsu",], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_0_JS)
tw_1_JS<-plm(NM_to_1994~SOE_dec_98*post_t, data = cities[cities$include_pop_adm==TRUE & cities$EPROV!="Jiangsu",], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_1_JS)
tw_3_JS<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995), data = cities[cities$include_pop_adm==TRUE & cities$EPROV!="Jiangsu",], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_3_JS)
stargazer(tw_0_JS, tw_1_JS, tw_3_JS, out = "tables/baseline_JS.html", column.labels= c("(a)", "(b)", "(c)"),column.separate = c(1, 1), dep.var.labels = "ratio of net migration to 1994 population",
          add.lines = list(c("Year fixed effect", "yes", "yes", "yes"), c("Prefecture fixed effect", "yes", "yes", "yes")), model.numbers = FALSE,
          title ="two-way fixed effects, excluding Jiangsu province", covariate.labels=c("SOE reform 1998*year 2002", "SOE reform 1998*year 2001", "SOE reform 1998*year 2000", "SOE reform 1998*year 1999", "SOE reform 1998*year 1998", "SOE reform 1998*year at and after 1998","SOE reform 1998*year 1997", "SOE reform 1998*year 1996", "SOE reform 1998*year 1995"), 
          style = "apsr", notes = "This table reports regression results with prefectures of Jiangsu province excluded from sample. There is virtually no change as compared to results reported in table 3.2.1.1.")

# add FDI as a proxy for 1998 Asian Financial Crisis shock
cities$fdi_real_adm_bil<-cities$fdi_real_adm/100000
tw_0_fdi<-plm(NM_to_1994~SOE_dec_98*(year==1998)+fdi_real_adm_bil, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_0_fdi)
tw_1_fdi<-plm(NM_to_1994~SOE_dec_98*post_t+fdi_real_adm_bil, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_1_fdi)
tw_3_fdi<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995)+fdi_real_adm_bil, data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_3_fdi)
cities$fdi_shock_98<-(cities$fdi_real_adm_98-cities$fdi_real_adm_97)/cities$gdp_adm_98
tw_0_fdi_1<-plm(NM_to_1994~SOE_dec_98*(year==1998)+fdi_shock_98*as.numeric(year==1998), data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_0_fdi_1)
tw_1_fdi_1<-plm(NM_to_1994~SOE_dec_98*post_t+fdi_shock_98*as.numeric(year==1998), data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_1_fdi_1)
tw_3_fdi_1<-plm(NM_to_1994~SOE_dec_98*(year==2002)+SOE_dec_98*(year==2001)+SOE_dec_98*(year==2000)+SOE_dec_98*(year==1999)+SOE_dec_98*(year==1998)+SOE_dec_98*(year==1997)+SOE_dec_98*(year==1996)+SOE_dec_98*(year==1995)+fdi_shock_98*as.numeric(year==1998), data = cities[cities$include_pop_adm==TRUE,], index=c("CITYGB","year"), model = "within", effect = "twoways" )
summary(tw_3_fdi_1)
stargazer(tw_0_fdi, tw_1_fdi, tw_3_fdi, tw_0_fdi_1, tw_1_fdi_1, tw_3_fdi_1, out = "tables/baseline_fdi.html", column.labels= c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),column.separate = c(1, 1), dep.var.labels = "ratio of net migration to 1994 population",
          add.lines = list(c("Year fixed effect", "yes", "yes", "yes", "yes", "yes", "yes"), c("Prefecture fixed effect", "yes", "yes", "yes", "yes", "yes", "yes")), model.numbers = FALSE,
          title ="two-way fixed effects, controlling for FDI", covariate.labels=c("FDI (billion US$)", "SOE reform 1998*year 2002", "SOE reform 1998*year 2001", "SOE reform 1998*year 2000", "SOE reform 1998*year 1999", "SOE reform 1998*year 1998", "SOE reform 1998*year at and after 1998","SOE reform 1998*year 1997", "SOE reform 1998*year 1996", "SOE reform 1998*year 1995", "relative FDI increase in 1998*year1998"),
          style="apsr", notes = "This table controls for FDI. Columns (a) to (c) controls for FDI in each year of each prefecture, and columns (d) to (f) controls for the interaction between relative FDI decrease in 1998 and year dummy 1998. There is virtually no change as compared to results reported in table 3.2.1.1.")

## Part 6: Tables and plots
# examples of outliers and non-outliers 
cities$year<-as.numeric(as.character(cities$year))
lm_chongqing<-lm(total_population_adm~year, data=cities[cities$CITYGB==50,])
summary(lm_chongqing)
plot31111<-ggplot(cities[cities$CITYGB==50,], aes(x=year, y=total_population_adm/100))+geom_point()+geom_smooth(method = "lm", se=TRUE)+
  labs(caption = "R2=0.64", title="Usual Residential Population of Chongqing", subtitle= "1994-2004", y="Usual Residential Population, million")
lm_shanghai<-lm(total_population_adm~year, data=cities[cities$CITYGB==31,])
summary(lm_shanghai)
plot31112<-ggplot(cities[cities$CITYGB==31,], aes(x=year, y=total_population_adm/100))+geom_point()+geom_smooth(method = "lm", se=TRUE)+
  labs(caption = "R2=0.94", title="Usual Residential Population of Shanghai", subtitle= "1994-2004", y="Usual Residential Population, million")
grid.arrange(plot31111, plot31112, nrow=1)
g<-arrangeGrob(plot31111, plot31112, nrow=1)
ggsave(file="plots/Usual Residential Population of Shanghai and Chongqing.pdf", plot=g, width = 10)

# timetrend of total residential population: nationwide
cities_wide<-reshape(cities, direction = "wide", idvar="CITYGB", timevar = "year", sep="_")
cities_wide_complete<-cities_wide[is.na(cities_wide$total_population_adm_1994)==FALSE & is.na(cities_wide$total_population_adm_2004)==FALSE,]
cities_complete<-cities[cities$CITYGB%in% cities_wide_complete$CITYGB,]
cities_complete_agg<-cities_complete%>%group_by(year)%>%summarise(total_population_adm=sum(total_population_adm))
ggplot(cities_complete_agg, aes(x=year, y=total_population_adm))+geom_line()+labs(y="total population in sample", title="Time trend of total usual residential population", subtitle = "1994-2004")
ggsave("plots/Nationawide total residential population in sample.pdf", width = 8)
# timetrend of total residential population: selected prefectures
ggplot(rbind(cities[cities$CITYGB==2305,],cities[cities$CITYGB==3502,]) ,aes(x=year))+geom_line(aes(col=ECITY,  y=total_population_adm*10),size=1)+
  labs(title="Usual Residential Population of Shuangyashan and Xiamen", subtitle= "1994-2004", y="Usual Residential Population, thousand")
ggsave("plots/Residential population of selected cities.pdf", width = 8)
# timetrend of net migration rates: selected prefectures
ggplot(rbind(cities[cities$CITYGB==2307,],cities[cities$CITYGB==3706,],cities[cities$CITYGB==3301,],cities[cities$CITYGB==3706,],cities[cities$CITYGB==4401,]) ,aes(x=year))+geom_line(aes(col=ECITY,  y=NM_to_1994),size=1)+
  labs(title="Net Migration rate of Selected Prefectures", subtitle= "1994-2004", y="net migration as a fraction of 1994 population, thousand")
ggsave("plots/Net migration rates of selected cities.pdf", width = 8)
# map of net migration rate in 1998
NM_1998<-cities[cities$year==1998,]%>%select(CITYGB, include_pop_adm,NM_to_1994)
prefecture_92_99_map<-merge(prefecture_92_99_map, NM_1998, by="CITYGB", all.x=TRUE)
tm_1<-tm_shape(prefecture_92_99_map)+tm_polygons("NM_to_1994", style="quantile", palette="Greens")+tm_borders(lwd=0.1, col="white")+tm_layout(title="Net Migration in 1998 to 1994 population", frame = FALSE, title.size = 1)
tmap_save(tm = tm_1, filename = "plots/Net migration rate in 1998 by prefecture.pdf")

# timetrend of SOE employment (zhigong) change: nationwide
cities_complete_agg<-cities_complete%>%group_by(year)%>%summarise(total_zhigong=sum(zhigong_number_adm, na.rm=TRUE))
ggplot(cities_complete_agg[cities_complete_agg$year!=1994,], aes(x=year, y=total_zhigong/100))+geom_line()+labs(y="total SOE employment in sample, million", title="Time trend of SOE employment", subtitle = "1995-2004")+xlim(1994, 2004)
ggsave("plots/Nationawide SOE employment in sample.pdf", width = 8)

# timetrend of SOE employment (zhigong) change: selected cities
ggplot(rbind(rbind(cities[cities$CITYGB==2305,],cities[cities$CITYGB==3502,])) ,aes(x=year))+geom_line(aes(col=ECITY, y=zhigong_number_adm*10),size=1)+
  labs(title="Total number of SOE workers in Shuangyashan and Xiamen", subtitle= "1994-2004", y="total number of SOE workers, thousand")
ggsave("plots/SOE employment of selected cities.pdf", width = 8)

# map of SOE reform in 1998
ref_1998<-cities[cities$year==1998,]%>%select(CITYGB, SOE_dec_98)
prefecture_92_99_map<-merge(prefecture_92_99_map, ref_1998, by="CITYGB", all.x=TRUE)
tm_2<-tm_shape(prefecture_92_99_map)+tm_polygons("SOE_dec_98", style="quantile")+tm_borders(lwd=0.1, col="white")+tm_layout(title="SOE downsizing in 1998 to 1994 population", frame = FALSE, title.size = 1)
tmap_save(tm = tm_2, filename = "plots/SOE employment change rate in 1998 by prefecture.pdf")

# timetrend of net migration rate by SOE employment change in 1998: by quartile
ggplot(cities_wide_complete[cities_wide_complete$include_pop_adm_1998==TRUE,], aes(x=SOE_dec_98_1998))+geom_density(size=1)+labs(title = "distribution of change of SOE emlpoyment in 1998", x="change in SOE emlpoyment in 1998", y="density")
ggsave("plots/Nationwide distribution of SOE employment change in 1998.pdf", width = 8)

quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
cities_1998<-cities[cities$year==1998,]
cities_1998$SOE_dec_98_qt<-NA
cities_1998$SOE_dec_98_qt[cities_1998$SOE_dec_98<quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[1]]<-1
cities_1998$SOE_dec_98_qt[cities_1998$SOE_dec_98>quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[1] & cities_1998$SOE_dec_98<=quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[2]]<-2
cities_1998$SOE_dec_98_qt[cities_1998$SOE_dec_98>quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[2] & cities_1998$SOE_dec_98<=quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[3]]<-3
cities_1998$SOE_dec_98_qt[cities_1998$SOE_dec_98>quantile(cities_wide_complete$SOE_dec_98_1998, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)[3]]<-4
cities<-merge(cities, cities_1998[cities_1998$include_pop_adm==TRUE,]%>%select("CITYGB", "SOE_dec_98_qt"), by="CITYGB", all.x = TRUE)
cities_SOE_dec_98_qt<-cities[cities$include_pop_adm==TRUE,]%>%group_by(SOE_dec_98_qt, year)%>%summarise(NM_to_1994_avr=mean(NM_to_1994, na.rm=TRUE))
cities_SOE_dec_98_qt$quartile<-factor(cities_SOE_dec_98_qt$SOE_dec_98_qt)
ggplot(cities_SOE_dec_98_qt[is.na(cities_SOE_dec_98_qt$quartile)==FALSE,], aes(x=year))+geom_line(aes(y=NM_to_1994_avr, col=quartile), size=1)+
  labs(title="Average net migration rate versus SOE downsizing in 1998", subtitle="1995-2004", y="Ratio of net migration to 1994 population")
ggsave("plots/Average net migration rate versus SOE downsizing in 1998, by quartile.pdf", width = 8)

# timetrend of net migration rate by SOE employment change in 1998: by half
cities$SOE_dec_98_hf[cities$SOE_dec_98_qt<=2]<-1
cities$SOE_dec_98_hf[cities$SOE_dec_98_qt>2]<-0
cities$SOE_dec_98_hf<-as.factor(cities$SOE_dec_98_hf)
cities_SOE_dec_98_hf<-cities[cities$include_pop_adm==TRUE,]%>%group_by(SOE_dec_98_hf, year)%>%summarise(NM_to_1994_avr=mean(NM_to_1994, na.rm=TRUE))
cities_SOE_dec_98_hf$treated<-cities_SOE_dec_98_hf$SOE_dec_98_hf
ggplot(cities_SOE_dec_98_hf[is.na(cities_SOE_dec_98_hf$SOE_dec_98_hf)==FALSE,], aes(x=year))+geom_line(aes(y=NM_to_1994_avr, col=treated), size=1)+
  labs(title="Average net migration rate versus SOE downsizing in 1998", subtitle="1995-2004", y="Ratio of net migration to 1994 population")
ggsave("plots/Average net migration rate versus SOE downsizing in 1998, by half.pdf", width = 8)
# summary statistics
stargazer(cities%>%select(total_population_adm_th, natural_growth_admin, zhigong_number_adm_th, NM_to_1994, SOE_dec_98, nonstate_imp, gdppc_th), out = "tables/summary statistics.html",
          covariate.labels = c("usual residential population (thousand)", "population natural growth rate (per thousand)", "SOE emlpoyment (thousand)", "net migration as a fraction of 1994 population", "change in 1998 SOE emlpoyment to 1994 total population", "non-sate sector employmemnt share", "GDP per capita (thousand Chinese Yuan)"))
