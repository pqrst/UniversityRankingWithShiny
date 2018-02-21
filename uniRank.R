library(ggplot2) 
library(dplyr)
library(RColorBrewer)
library(tidyr)
options(warn=-1)

cdata<-read.csv("C:/c/Rawls/kaggle/UniRankingData/cwurData.csv")
expense<-read.csv("C:/c/Rawls/kaggle/UniRankingData/education_expenditure_supplementary_data.csv")
attain<-read.csv("C:/c/Rawls/kaggle/UniRankingData/educational_attainment_supplementary_data.csv")
school<-read.csv("C:/c/Rawls/kaggle/UniRankingData/school_and_country_table.csv")
time<-read.csv("C:/c/Rawls/kaggle/UniRankingData/timesData.csv")

str(cdata)
head(cdata, 5)

View(cdata)
View(expense)
View(attain)
View(school)
View(time)

options(repr.plot.width = 5, repr.plot.height = 5)
ggplot(cdata, aes(x=score)) + geom_histogram(fill="royalblue") + facet_wrap(~year)

options(repr.plot.width = 5, repr.plot.height = 5)
theme_b <- theme(axis.text.x = element_blank(),legend.position = "none")
cdata %>% select(world_rank,institution,year) %>% group_by(institution,year) %>% 
  filter(world_rank <=10 & year == 2015) %>% arrange(world_rank) %>% ggplot(aes(x = institution,y = world_rank)) + 
  geom_bar(stat="identity",fill="hotpink3") + theme_b + labs(title="Top 10 Universities in World as on 2015") + 
  geom_label(aes(label=institution),size=3)

options(repr.plot.width=8, repr.plot.height=5)
topc<-cdata %>%select(world_rank,institution,country,year)%>% filter(year==2015 )%>%
  group_by(country)%>%summarise(university_count=n())
  ggplot(topc,aes(x=country,y=university_count))+geom_bar(stat="identity",fill="orchid4")+
  theme(axis.text.x = element_text(angle=90))+geom_text(aes(label=university_count),vjust=-.4,size=2)+
  labs(title="Countrywise University Count")
  

time %>% select(world_rank,university_name,year) %>% group_by(university_name,year) %>% 
  filter(world_rank <=10 & year == 2015) %>% arrange(time$num_students) %>% ggplot(aes(x = university_name,y = time$num_students)) + 
  geom_bar(stat="identity",fill="hotpink3") + theme_b + labs(title="Top 10 Universities in World as on 2015") + 
  geom_label(aes(label=institution),size=3) 
  
  
options(repr.plot.width=8, repr.plot.height=5)
time$num_students<-as.numeric(time$num_students)
time$world_rank<-as.numeric(time$world_rank)
time$year<-as.factor(time$year)
time %>% select(num_students,university_name,year,world_rank)%>%filter(world_rank<=25)%>%
  arrange(desc(num_students))%>%group_by(university_name)%>%
  ggplot(aes(x=factor(university_name,levels=university_name),y=num_students,fill=world_rank))+
  geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90))+scale_fill_continuous(low="pink",high="brown")  
  
  