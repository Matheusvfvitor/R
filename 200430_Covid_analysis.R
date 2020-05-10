getwd()
setwd(project)

raw_data =read.csv("covid_19_clean_complete.csv")

install.packages("tidyverse")
install.packages("janitor")
install.packages("treemap")
install.packages("googleVis")

library(tidyverse)
library(janitor)
library(ggplot2)
library(treemap)
library(googleVis)
suppressPackageStartupMessages(library(googleVis))


geral_data = raw_data %>% select(-Lat,-Long)
geral_data = raw_data %>% group_by(Date, Country.Region) %>% 
  summarise(Total.Cases = sum(Confirmed), Total.Deaths = sum(Deaths), Total.Recovery = sum(Recovered))

geral_data = geral_data %>% mutate(Actives = Total.Cases - Total.Deaths - Total.Recovery)

geral_data = tibble(geral_data)
geral_data$Date = as.character(geral_data$Date)
geral_data$Date = as.Date(geral_data$Date, format = "%m/%d/%Y")

geral_data
gen_data = geral_data

max = max(geral_data$Date)
last_data = geral_data %>% filter(Date == max)

last_data

world_pop = read.csv("population_by_country_2020.csv")
world_pop = world_pop %>% select(Country..or.dependency.,Population..2020., Land.Area..Km.., Urban.Pop..)
world_pop = world_pop %>% mutate(Density.Pop = Population..2020. / Land.Area..Km..)

gen_last_data = left_join(last_data, world_pop, by = c( "Country.Region" = "Country..or.dependency."))

na_data = gen_last_data %>% filter(is.na(Population..2020.))
na_data = na_data %>% group_by(Country.Region) %>% summarise()

## Burma está errado o nome correto é Mynamar, como no Dataset WorldPopulation

last_data
last_data$Country.Region =  as.character(last_data$Country.Region)
last_data[last_data$Country.Region == "Burma",2] = "Myanmar"

##Congo está dividido entre duas províncias
congo = last_data %>% filter(Country.Region == "Congo (Brazzaville)" | Country.Region == "Congo (Kinshasa)")

congo = congo %>% adorn_totals("row", name="Congo")
congo[congo$Country.Region=="-",2] = "Congo"
congo[congo$Date == "Congo",1] = "20-04-30"

congo = tibble(congo)
congo$Date = as.Date(congo$Date, format = "%Y-%m-%d")

last_data = last_data %>% rbind(congo)
last_data = last_data %>% filter(Country.Region !="Congo (Brazzaville)" & Country.Region !="Congo (Kinshasa)")

##Adequando os nomes
last_data[last_data$Country.Region == "Cote d'Ivoire",2] <- "Côte d'Ivoire"
last_data[last_data$Country.Region == "Saint Vincent and the Grenadines",2] <- "St. Vincent & Grenadines"
last_data[last_data$Country.Region == "Sao Tome and Principe",2] = "Sao Tome & Principe"
last_data[last_data$Country.Region ==  "Taiwan*",2] = "Taiwan"
last_data[last_data$Country.Region == "US",2] = "United States"
last_data[last_data$Country.Region == "Saint Kitts and Nevis",2] = "Saint Kitts & Nevis"

world_pop = tibble(world_pop)
world_pop$Country..or.dependency. = as.character(world_pop$Country..or.dependency.)

world_pop[world_pop$Country..or.dependency. == "Czech Republic (Czechia)",1] <- "Czechia"

#Criação da linha para Kosovo na world_pop // Dados do Wikipedia
kosovo_df <- data.frame(Country..or.dependency. =  "Kosovo",
                        Population..2020. = 1809740,
                        Land.Area..Km.. = 10887,
                        Urban.Pop.. = "92%")

kosovo_df <- kosovo_df %>% mutate(Density.Pop = Population..2020. / Land.Area..Km..)
world_pop = world_pop %>% rbind(kosovo_df)

gaza_df <- data.frame(Country..or.dependency. =  "West Bank and Gaza",
                      Population..2020. = 4569000,
                      Land.Area..Km.. = 6220,
                      Urban.Pop.. = "32%") %>% mutate (Density.Pop = Population..2020./Land.Area..Km..)

world_pop = world_pop %>% rbind(gaza_df)

last_data = last_data %>% filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")

gen_last_data = left_join(last_data, world_pop, by = c( "Country.Region" = "Country..or.dependency."))

na_data = gen_last_data %>% filter(is.na(Population..2020.))
na_data = na_data %>% group_by(Country.Region) %>% summarise()

gen_last_data -> df
df = df %>% top_n(10,Total.Cases)
df = df %>% mutate(Mortality = Total.Deaths / Total.Cases)
df = df %>% gather("Status", value = "Occurences", Total.Cases, Total.Deaths, Total.Recovery, Actives, Mortality)
df = df %>% select(-Population..2020.,-Density.Pop, - Land.Area..Km.., - Urban.Pop..)


gen_last_data -> df2
df2 = df2 %>% top_n(40,Total.Cases)
df2 = df2 %>% mutate(Mortality = Total.Deaths / Total.Cases)
df2 = df2 %>% gather("Status", value = "Occurences", Total.Cases, Total.Deaths, Total.Recovery, Actives, Mortality)
df2 = df2 %>% select(-Population..2020.,-Density.Pop, - Land.Area..Km.., - Urban.Pop..)

chart = df2 %>% spread(key="Status", value= "Occurences", c("Total.Cases", "Total.Deaths", "Mortality", "Total.Recovery"))

chart = tibble(chart)
chart$Actives = as.numeric(chart$Actives)
chart$Total.Cases = as.numeric(chart$Total.Cases)
chart$Total.Deaths = as.numeric(chart$Total.Deaths)
chart$Total.Recovery = as.numeric(chart$Total.Recovery)
chart$Mortality = as.numeric(chart$Mortality)

ggplot(chart, aes(Total.Cases, Total.Deaths, size = Mortality, color = Country.Region, alpha = 0.8))+
  geom_point()+
  mts_theme

chart2 = chart %>% filter(Country.Region != "United States")
ggplot(chart2, aes(Total.Cases, Total.Deaths, size = Mortality, color = Country.Region, alpha = 0.8))+
  geom_point()+
  mts_theme

df_cases = df %>% filter (Status == "Total.Cases")

group = df_cases$Country.Region
value = df_cases$Occurences

data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title.legend = "Total de Casos", 
        title = "Top 10 - Total de Casos")

df_deaths = df %>% filter (Status == "Total.Deaths")

group = df_deaths$Country.Region
value = df_deaths$Occurences

data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title = " Top 20 - Total de Mortes")

df_recovery = df %>% filter (Status == "Total.Recovery")

group = df_recovery$Country.Region
value = df_recovery$Occurences

data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title = " Top 20 - Total de Recuperados")

df_actives = df %>% filter (Status == "Actives")

group = df_actives$Country.Region
value = df_actives$Occurences

data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title = " Top 20 - Total de Doentes")

df = df %>% filter(Status != "Total.Cases")

ggplot(df, aes(x = reorder(Country.Region, Occurences), y = Occurences, fill = Status))+ 
  geom_col()+
  mts_theme

group = df_cases$Country.Region
value = df_cases$Occurences

df_deaths = df %>% filter(Status == "Total.Deaths")

df_deaths = arrange(df_deaths, Occurences)
ggplot(df_deaths, aes(x = reorder(Country.Region, Occurences), y = Occurences))+geom_col()+theme_classic()

gen_last_data = gen_last_data %>% mutate(Confirmed.per.Mi.Hab = Total.Cases*1000000/Population..2020.,
                                         Deaths.per.Mi.Hab = Total.Deaths*1000000/Population..2020.,
                                         Recovery.per.Mi.Hab = Total.Recovery*1000000/Population..2020.,
                                         Actives.per.Mi.Hab = Actives*1000000/Population..2020.)
gen_last_data
options("scipen"=100, "digits"=4)

mts_theme = theme_classic()+theme(axis.text.x = element_text(angle = 90), 
                                  axis.title = element_text(color="darkblue"),
                                  title = element_text(color = "darkblue"))


top = gen_last_data %>% top_n(50, Total.Cases)

top = gen_last_data %>% top_n(10,Deaths.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Deaths.per.Mi.Hab), y = Deaths.per.Mi.Hab))+
  geom_col(fill = "darkred")+
  ggtitle("Deaths per Milion Habitants")+
  mts_theme

top = gen_last_data %>% top_n(10,Actives.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Actives.per.Mi.Hab), y = Actives.per.Mi.Hab))+
  geom_col(fill = "orange")+
  ggtitle("Actives per Milion Habitants")+
  mts_theme

top = gen_last_data %>% top_n(10,Recovery.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Recovery.per.Mi.Hab), y = Recovery.per.Mi.Hab))+
  geom_col(fill = "darkgreen")+
  ggtitle("Recovery per Milion Habitants")+
  mts_theme

top = gen_last_data %>% top_n(20, Total.Cases)

top = top %>% top_n(20,Deaths.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Deaths.per.Mi.Hab), y = Deaths.per.Mi.Hab))+
  geom_col(fill = "darkred")+
  ggtitle("Deaths per Milion Habitants")+
  mts_theme

top = top %>% top_n(20,Actives.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Actives.per.Mi.Hab), y = Actives.per.Mi.Hab))+
  geom_col(fill = "orange")+
  ggtitle("Actives per Milion Habitants")+
  mts_theme

top = top %>% top_n(20,Recovery.per.Mi.Hab) %>% arrange(desc(Deaths.per.Mi.Hab))

ggplot(top, aes(x = reorder(Country.Region, Recovery.per.Mi.Hab), y = Recovery.per.Mi.Hab))+
  geom_col(fill = "darkgreen")+
  ggtitle("Recovery per Milion Habitants")+
  mts_theme

gen_data = gen_data %>% select(-Population..2020., -Land.Area..Km.., -Urban.Pop.., - Density.Pop)

lag()

x = gen_data %>% mutate(Days.after.first.death = 0, Days.after.first.case = 0)
x$Days.after.first.death = as.numeric(x$Days.after.first.death)
x$Days.after.first.death = as.numeric(x$Days.after.first.case)
x = tibble(x)

x
x$Date = as.Date(x$Date, format = "%m/%d/%y")

x = x %>% arrange(Country.Region, Date)
x
##Days after first Death
for(i in 1:nrow(x)){
  if(x[i,4] >0){
    x[i,7] <- x[i-1,7]+1
  }
}

##Days after first case
for(i in 1:nrow(x)){
  if(x[i,3] >0){
    x[i,8] <- x[i-1,8]+1
  }
}

df <- x

maxDate = max(df$Date)

top20 = df %>% filter(Date == maxDate) %>% top_n(10,Total.Cases)

top20 = top20$Country.Region



y = df %>% gather(key = "Status", value= "Occ.", c("Total.Cases", "Total.Deaths", "Total.Recovery","Actives"))
time_table = y

time_10 = time_table %>% filter(Country.Region == top20)
time_10 = time_10 %>% filter(Status == "Total.Cases")
ggplot(time_10, aes(Days.after.first.case, Occ. ,color = Country.Region))+
  geom_line()+
  ggtitle("Casos após primeiro dia")+
  mts_theme

time_10 = time_table %>% filter(Country.Region == top20)
time_10 = time_10 %>% filter(Status == "Total.Deaths")

ggplot(time_10, aes(Days.after.first.case, Occ. ,color = Country.Region))+
  geom_line()+
  ggtitle("Mortes após primeiro dia")+
  mts_theme

top10 = df %>% filter(Date == maxDate) %>% top_n(10,Total.Cases)
top10$Country.Region = as.character(top10$Country.Region) 
top10 = top10 %>% filter(Country.Region != "US") 
top10 = top10$Country.Region

y = df %>% gather(key = "Status", value= "Occ.", c("Total.Cases", "Total.Deaths", "Total.Recovery","Actives"))
time_table = y

time_10 = time_table %>% filter(Country.Region == top20)
time_10 = time_10 %>% filter(Status == "Total.Cases")
ggplot(time_10, aes(Days.after.first.case, Occ. ,color = Country.Region))+
  geom_line()+
  ggtitle("Casos após primeiro dia")+
  mts_theme

y = df %>% gather(key = "Status", value= "Occ.", c("Total.Cases", "Total.Deaths", "Total.Recovery","Actives"))
time_table = y
time_10 = time_table %>% filter(Country.Region == top20)

time_10 = time_10 %>% filter(Status == "Total.Deaths")
ggplot(time_10, aes(Days.after.first.death, Occ. ,color = Country.Region))+
  geom_line()+
  ggtitle(" Mortes após primeiro dia")+
  mts_theme

df = df%>% mutate(New.Cases = ifelse(Country.Region == lag(Country.Region,1),Total.Cases - lag(Total.Cases,1,0),0), 
                  New.Deaths = ifelse(Country.Region == lag(Country.Region,1), Total.Deaths - lag(Total.Deaths,1,0),0))

new_data = df %>% 
  gather(key = "Status", value = "Occ.", 
         c("Total.Cases", "Total.Deaths", "Total.Recovery", "Actives", "New.Cases","New.Deaths"))

new_data = new_data %>% filter(Country.Region %in% top20)
new_cases = new_data %>% filter(Status == "New.Cases")
new_cases = tibble(new_cases)
new_cases[new_cases$Occ. <0,6] = 0
new_cases = new_cases %>% filter (Occ. >0)

ggplot(new_cases, aes(Date, Occ., fill = Country.Region))+geom_col()+mts_theme

new_deaths = new_data %>% filter(Status == "New.Deaths")
new_deaths[new_deaths$Occ. <0,6] = 0
new_deaths = new_deaths %>% filter (Occ. >0)
ggplot(new_deaths, aes(Date, Occ., fill = Country.Region))+
  geom_col()+
  ggtitle("New Deaths")+
  mts_theme

ggplot(new_deaths, aes(Date, Occ., color = Country.Region))+
  geom_line()+
  ggtitle("New Deaths by Date")+
  mts_theme

ggplot(new_deaths, aes(Days.after.first.death, Occ., color = Country.Region))+
  geom_line()+
  ggtitle("New Deaths by Date")+
  mts_theme


new_data_br = df %>% filter(Country.Region == "Brazil")
new_data_br = new_data_br %>% gather(key = "Status", value = "Occ.", 
                                     c("Total.Cases", "Total.Deaths", "Actives", "New.Cases", "New.Deaths"))

new_deaths_br = new_data_br %>% filter(Status == "New.Deaths")


new_data = df %>% gather(key = "Status", value = "Occ.", 
                         c("Total.Cases", "Total.Deaths", "Actives", "New.Cases", "New.Deaths","Total.Recovery"))

new_deaths = new_data %>% filter(Status == "New.Deaths")


top20 = df %>% filter(Date == maxDate) %>% top_n(10,Total.Cases)

new_deaths = new_deaths %>% filter(Country.Region %in% top20$Country.Region)

ggplot(new_deaths, aes(Date, Occ., color = Country.Region))+
  geom_line()+
  ggtitle("New Deaths by Date")+
  mts_theme

ggplot(new_deaths, aes(Days.after.first.death, Occ., color = Country.Region))+
  geom_line()+
  ggtitle("New Deaths by Date")+
  mts_theme

ggplot(new_deaths, aes(Date, Occ., color = Country.Region))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~Country.Region)+
  ggtitle("New Deaths by Date")+
  mts_theme

ggplot(new_deaths, aes(Days.after.first.death, Occ., color = Country.Region))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~Country.Region)+
  ggtitle("New Deaths by Date")+
  mts_theme

calendar_chart = gvisCalendar(new_deaths,
                              datevar = "Date",
                              numvar = "Occ.",
                              options=list(
                                title="Calendar of Deaths",
                                height=560,
                                calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}"))

plot(calendar_chart)


top10_data = new_data %>% filter(Country.Region %in% top20$Country.Region)


data_chart = top10_data %>% 
  filter(Status !="Total.Cases" & Status != "New.Deaths" & Status !="New.Cases" ) %>% 
  ggplot(aes(Date, Occ., color = Status))+
  geom_line()+
  ggtitle("Top 10")+
  facet_wrap(~Country.Region)+
  mts_theme

data_chart

data_chart = top10_data %>% 
  filter(Status !="Total.Cases" & Status != "New.Deaths" & Status !="New.Cases" ) %>% 
  ggplot(aes(Days.after.first.case, Occ., color = Status))+
  geom_line()+
  ggtitle("Top 10")+
  facet_wrap(~Country.Region)+
  mts_theme

data_chart

data_chart = top10_data %>% 
  filter(Status !="Total.Cases" & Status != "New.Deaths" & Status !="New.Cases", Country.Region != "US" ) %>% 
  ggplot(aes(Date, Occ., color = Status))+
  geom_line()+
  ggtitle("Top 10")+
  facet_wrap(~Country.Region)+
  mts_theme

data_chart

data_chart = top10_data %>% 
  filter(Status !="Total.Cases" & Status != "New.Deaths" & Status !="New.Cases", Country.Region != "US" ) %>% 
  ggplot(aes(Days.after.first.case, Occ., color = Status))+
  geom_line()+
  ggtitle("Top 10")+
  facet_wrap(~Country.Region)+
  mts_theme

data_chart

data_chart = top10_data %>% 
  filter(Status !="Total.Cases" & Status != "New.Deaths" & Status !="New.Cases", Country.Region == "Brazil" ) %>% 
  ggplot(aes(Days.after.first.case, Occ., color = Status))+
  geom_line()+
  geom_smooth(linetype = "dashed")+
  ggtitle("Brasil Current Situation")+
  mts_theme

data_chart

