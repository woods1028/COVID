require(countrycode)
require(rvest)
require(ggplot2)
require(cowplot)
require(plyr);require(dplyr)
require(magrittr)
require(scales)
require(data.table)
require(ggrepel)
require(xlsx)
require(colortools)
require(stringr)
require(lubridate)
require(randomForest)
require(prophet)
theme_update(
    panel.background = element_rect(fill = "gray25"),panel.grid = element_line(color = "gray45"),
    axis.title = element_text(size = 14),axis.title.y = element_text(angle = 0,vjust = .5),
    axis.text = element_text(size = 12),legend.key = element_rect(fill = "gray25"),
    plot.title = element_text(size= 18,hjust = .5),legend.text = element_text(size = 12),
    strip.text = element_text(size = 16,face = "bold",color = "white"),
    strip.background = element_rect(fill = "firebrick")
  )

color_generator <- function(n,color) {
  x <- rep(1, n)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  color_list <- setColors(color, n)
  
  if (n%%2==0) {
    vec <- lapply(1:(n/2),function(x) {
      c(color_list[x],color_list[x+(n/2)])
    }) %>% 
      unlist
  } else {
    vec <- lapply(1:(floor(n/2)),function(x) {
      c(color_list[x],color_list[x+(n/2)])
    }) %>% 
      unlist 
    vec <- c(vec,color_list[n])
  }
  
  return(vec)
}


#### Daily Update ####

cases_by_country <- "https://covid.ourworldindata.org/data/ecdc/full_data.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate(Country_Code=countrycode(location,origin = "country.name",destination = "iso2c")) %>% 
  mutate_at(vars(new_cases,new_deaths,total_cases,total_deaths),~ifelse(is.na(.),0,.))

tests_by_date <- "https://github.com/owid/covid-19-data/raw/master/public/data/testing/covid-testing-all-observations.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate(Country=substr(Entity,1,regexpr(" - ",Entity)-1)) %>% 
  mutate(Country_Code=countrycode(Country,origin = "country.name",destination = "iso2c")) %>% 
  group_by(Country_Code) %>% 
  mutate(Entry=seq(n())) %>% 
  ungroup

total_tests <- tests_by_date %>% 
  mutate_at(vars(Date),as.Date) %>% 
  arrange(Date) %>% 
  group_by(Country_Code) %>% 
  slice(n())

exposure_by_country <- cases_by_country %>% 
  mutate_at(vars(date),as.Date) %>% 
  filter(total_cases>10) %>% 
  group_by(Country_Code) %>% 
  slice(1,n()) %>% 
  mutate(Exposure=lead(date)-date) %>% 
  ungroup %>% 
  mutate_at(vars(Exposure),as.integer) %>% 
  filter(!is.na(Exposure)) %>% 
  select(Country_Code,Exposure)

#### Other Data ####

worldpop <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)" %>% 
  read_html %>% 
  html_nodes("table") %>% 
  extract2(4) %>% 
  html_table %>% 
  setNames(c("Country","Region","Statistical_Region","Population_2018","Population_2019","Change")) %>% 
  mutate_at(vars(Country),~sub("\\[[a-z]\\]","",.)) %>% 
  mutate(Country_Code=countrycode(Country,origin = "country.name",destination = "iso2c")) %>% 
  mutate_at(vars(starts_with("Population")),~gsub(",","",.) %>% as.integer)

sex_ratio <- "share-population-female.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  filter(Year==max(Year)) %>% 
  setNames(c("Country","Code","Year","Ratio")) %>% 
  mutate(Country_Code=countrycode(Code,origin = "iso3c",destination = "iso2c")) %>% 
  mutate_at(vars(Ratio),~./100)

who_codes <- "https://www.who.int/countries/en/" %>% 
  read_html %>% 
  html_nodes("#alphabetical li") %>% 
  list(
    bind_cols(
      Country=html_text(.),
      Link=html_children(.) %>% 
        html_attr("href")
    )
  ) %>% 
  extract2(2) %>% 
  mutate_at(vars(Country),trimws) %>% 
  mutate(Code=str_extract(Link,"/[a-z]{3}/")) %>% 
  mutate_at(vars(Code),~gsub("/","",.) %>% toupper)

# smoking_link <- "https://apps.who.int/gho/athena/data/GHO/TOBACCO_0000000344,TOBACCO_0000000192?
# filter=COUNTRY:*;REGION:*;SEX:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO;SEX&profile=crosstable&format=csv"

smoking_by_country <- "TOBACCO_0000000344,TOBACCO_0000000192.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  select(1:4) %>% 
  setNames(as.character(.[1,])) %>% 
  filter(Country!="Country") %>% 
  setNames(sub(" ","",colnames(.))) %>% 
  mutate_at(vars(Male,Female),~sub(" .+","",.)) %>% 
  mutate_at(vars(Male,Female),as.numeric) %>% 
  mutate_at(vars(Year),~sub(" ","",.) %>% as.integer) %>% 
  filter(Year==2020) %>% 
  left_join(
    who_codes %>% 
      select(-Link),
    by="Country"
  ) %>% 
  mutate(iso2c=countrycode(Code,origin = "iso3c",destination = "iso2c"))

smokers_by_country_composite <- worldpop %>% 
  select(Country_Code,Population=Population_2019,Statistical_Region) %>% 
  left_join(
    sex_ratio %>% 
      select(Country_Code,Ratio),
    by="Country_Code"
  ) %>% 
  mutate(Female=Population*Ratio,
         Male=Population*(1-Ratio)) %>% 
  setDT %>% 
  melt(id.vars = "Country_Code",measure.vars = c("Female","Male"),variable.name="Sex",value.name="Population",variable.factor = F) %>% 
  filter(!is.na(Country_Code)) %>% 
  left_join(
    smoking_by_country %>% 
      select(Country_Code=iso2c,Male,Female) %>% 
      setDT %>% 
      melt(id="Country_Code",variable.name="Sex",value.name="Percentage",variable.factor = F) %>% 
      mutate_at(vars(Percentage),~./100),
    by=c("Country_Code","Sex")
  ) %>% 
  group_by(Country_Code) %>% 
  summarise(Smokers=weighted.mean(Percentage,w = Population)) %>% 
  ungroup

median_age <- "median-age.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  filter(Year<=2020) %>% 
  filter(Year==max(Year)) %>% 
  setNames(c("Country","Code","Year","Median_Age")) %>% 
  mutate(Country_Code=countrycode(Code,origin = "iso3c",destination = "iso2c"))

greetings <- "Greetings.xlsx" %>% 
  read.xlsx(sheetIndex = 1,stringsAsFactors = F) 
  
greeting_scores <- greetings %>% 
  left_join(
    data.frame(
      Among_Whom=c('All','Man to Man','Man to Woman','Strangers','Family/Friends','Men','Man To Woman','Woman to Woman'),
      Who=c(1,.5,.5,.33,.5,.5,.5,.5),
      stringsAsFactors = F
    ),
    by=c("Among.Whom"="Among_Whom")
  ) %>% 
  left_join(
    data.frame(
      Greeting=c('1 Kiss','2 Kisses','3 Kisses','Handshake','Bow','Obscene','Nothing','Embrace'),
      Greeting_Value=c(10,15,20, 3, 1,0,0,7),
      stringsAsFactors = F
    ),
    by="Greeting"
  ) %>% 
  group_by(Country) %>% 
  summarise(GS=mean(Greeting_Value*Who)) %>% 
  ungroup %>% 
  mutate(Country_Code=countrycode(Country,origin = "country.name",destination = "iso2c"))

beds_by_country <- "https://en.wikipedia.org/wiki/List_of_countries_by_hospital_beds" %>% 
  read_html %>% 
  html_nodes("table") %>% 
  extract2(1) %>%
  html_table(fill = T) %>% 
  setNames(c("Rank","Country","Continent","Y2013","Y2014","Y2015","Y2016","Y2017","Actual","Proportional","Occupancy","ICU_Beds","Ventilators")) %>% 
  filter(Rank!="Rank") %>% 
  mutate_at(vars(matches("\\d{4}")),as.numeric) %>% 
  mutate_at(vars(Y2017),~coalesce(.,Y2016,Y2015,Y2014,Y2013)) %>% 
  select(Country,Hospital_Beds=Y2017,Occupancy,ICU_Beds) %>% 
  mutate_at(vars(ICU_Beds),~str_extract(.,"\\d+\\.\\d+")) %>% 
  mutate_at(vars(Hospital_Beds,Occupancy,ICU_Beds),as.numeric) %>% 
  mutate(Country_Code=countrycode(Country,origin = "country.name",destination = "iso2c"))

#### Graphs ####

regions1 <- c("Eastern Asia","South-eastern Asia")
regions2 <- c("Northern Europe","Southern Europe","Western Europe")
regions3 <- c("Northern America")
regions4 <- "Oceania"

#### Death Path ####

cases_by_country %>% 
  filter(!is.na(Country_Code),total_deaths>=10) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  ungroup %>% 
  mutate_at(vars(total_deaths),log) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region),
    by="Country_Code"
  ) %>% 
  left_join(
    group_by(.,Country_Code) %>% 
      slice(n()) %>% 
      ungroup %>% 
      mutate(Point="Point",Label=Country_Code) %>% 
      select(Country_Code,Point,date,Label),
    by=c("Country_Code","date")
  ) %>%
  mutate_at(vars(Point),~ifelse(.=="Point",2,1)) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3,regions4)) %>% {
    ggplot(.,mapping = aes(x=Days,y=total_deaths,color=Country_Code))+
      geom_line(lwd = 1.5)+
      geom_point(aes(size=Point))+
      geom_label_repel(aes(label = Label),fill = "gray25",size = 6)+
      labs(x="Days Since 10th Death",y="log\nTotal\nDeaths")+
      scale_color_manual(values = color_generator(n = n_distinct(.$Country_Code),color = "turquoise1"))+
      theme(legend.position = "none")
  }
  
#### Cases Per Capita ####

cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  filter(Statistical_Region %in% c(regions1,regions3),!is.na(Country_Code)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="HK",181,.)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="TW",100,.)) %>% 
  mutate(total_non_fatal_cases=total_cases-total_deaths) %>% 
  mutate_at(vars(total_non_fatal_cases,total_deaths),list(per_capita=~./(Population/1000000))) %>% 
  rename(`Fatal Cases`=total_deaths_per_capita,
         `Non-Fatal Cases`=total_non_fatal_cases_per_capita) %>% 
  setDT %>% 
  melt(id.vars="Country_Code",measure.vars=c("Non-Fatal Cases","Fatal Cases"),
       variable.name = "Type",value.name="Total_Cases",variable.factor = F) %>% 
  ggplot(mapping = aes(x=Country_Code,y=Total_Cases,fill=Type))+
  geom_bar(stat = "identity",position = "stack")+
  facet_wrap(~Type,scales = "free")+
  labs(y="Total\nCases",x="",fill="")+
  scale_fill_manual(labels = c("Fatal","Non-Fatal"),values = c("red","deepskyblue"))+
  ggtitle("Cases Per Million People")+
  theme(legend.position = "none")

#### Percent Positive by Country ####

cases_by_country %>% 
  mutate_at(vars(date),as.Date) %>% 
  left_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  filter(!is.na(Tests)) %>% 
  mutate(Pct_Positive=total_cases/Tests) %>% 
  filter(total_cases>500,Country_Code!="AU") %>%
  ggplot(mapping = aes(x=Country_Code,y=Pct_Positive,size=total_cases))+
  geom_point(color = "white")+
  scale_size_continuous(range=c(2,15),labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  labs(y="Percent\nPositive",x="Country",size="Total Cases")

#### Total Tests by Country ####

cases_by_country %>% 
  filter(!is.na(Country_Code),total_deaths>10) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  mutate_at(vars(date),as.Date) %>% 
  left_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  filter(!is.na(Tests)) %>% 
  mutate(Negatives=Tests-total_cases) %>% 
  melt(id.vars = c("date","Country_Code"),measure.vars = c("total_cases","Negatives")) %>% 
  ggplot(mapping = aes(x=Country_Code,y=value,fill=variable))+
  geom_bar(stat = "identity",position = "stack")+
  scale_y_continuous(labels = comma)+
  labs(y="Total\nTests",x="",fill="")+
  scale_fill_manual(labels = c("Positive","Negative"),values = c("red","green2"))

#### Total Tests per Capita ####

cases_by_country %>% 
  filter(!is.na(Country_Code)) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  mutate_at(vars(date),as.Date) %>% 
  full_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  filter(!is.na(Tests)) %>% 
  mutate(Negatives=Tests-ifelse(is.na(total_cases),0,total_cases)) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  filter(Statistical_Region %in% c(regions1,regions3,regions2)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="HK",181,.)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="TW",100,.)) %>% 
  mutate_at(vars(Positives=total_cases,Negatives),list(Per_Capita=~./(Population/10000))) %>% 
  select(Country_Code,matches("Per_Capita")) %>% 
  setDT %>% 
  melt(id="Country_Code",variable.factor = F) %>% 
  mutate_at(vars(variable),~sub("_Per_Capita","",.)) %>% 
  mutate_at(vars(variable),~sub("s$","",.)) %>% 
  mutate_at(vars(variable),~factor(.,levels = c("Negative","Positive"))) %>% 
  ggplot(mapping = aes(x=Country_Code,y=value,fill=variable))+
  geom_bar(stat = "identity")+
  ggtitle("Tests Per 10,000 People")+
  labs(y="Total\nTests",x="",fill="")+
  scale_fill_manual(values = c("green2","red"))

#### Tests Per Capita against Percent Positive ####
  
cases_by_country %>% 
  filter(!is.na(Country_Code)) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  mutate_at(vars(date),as.Date) %>% 
  full_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  filter(!is.na(Tests)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="HK",181,.)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="TW",100,.)) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3)) %>% 
  mutate(Positive_Pct=total_cases/Tests,
         Tests_per_Capita=Tests/(Population/10000)) %>% 
  ggplot(mapping = aes(x=Tests_per_Capita,y=Positive_Pct,color = Country_Code))+
  geom_point(size = 4)+
  geom_label_repel(aes(label = Country_Code),fill = "gray25",size = 6)+
  theme(legend.position = "none")+
  labs(y="Percent\nPositive",x="Tests Per 10,000 People")+
  scale_y_continuous(labels = percent_format(accuracy = 1))

#### Tests Per Capita against CFR ####

cases_by_country %>% 
  filter(!is.na(Country_Code)) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  mutate_at(vars(date),as.Date) %>% 
  full_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases) %>% 
  filter(!is.na(Tests)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="HK",181,.)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="TW",100,.)) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3)) %>% 
  mutate(Positive_Pct=total_cases/Tests,
         Tests_per_Capita=Tests/(Population/10000)) %>% 
  #filter(Tests_per_Capita<=10) %>% 
  ggplot(mapping = aes(x=Tests_per_Capita,y=CFR,color = Country_Code,size = total_cases))+
  geom_point()+
  geom_label_repel(aes(label = Country_Code),fill = "gray25",size = 6,show.legend = F)+
  labs(y="CFR",x="Tests Per 10,000 People",size = "Total Cases")+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  #scale_x_continuous(limits = c(0,10))+
  scale_size_continuous(range = c(1,15),labels = comma)+
  guides(color = F,
        size = guide_legend(override.aes = list(color="gray90")))

  #### Percent Positive Against CFR ####

cases_by_country %>% 
  filter(!is.na(Country_Code)) %>% 
  group_by(Country_Code) %>% 
  mutate(Days=seq(n())) %>% 
  mutate_at(vars(date),as.Date) %>% 
  full_join(
    total_tests %>% 
      select(Country_Code,Date,Tests=Cumulative.total),
    by=c("Country_Code","date"="Date")
  ) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  filter(!is.na(Tests)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="HK",181,.)) %>% 
  mutate_at(vars(total_cases),~ifelse(Country_Code=="TW",100,.)) %>% 
  mutate(CFR=total_deaths/total_cases,
         Positive_Pct=total_cases/Tests,
         Tests_per_Capita=Tests/(Population/10000)) %>% 
  #mutate_at(vars(total_cases),log) %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3)) %>% 
  ggplot(mapping = aes(x=Positive_Pct,y=CFR,color=Country_Code,size=total_cases,label = Country_Code))+
  geom_point()+
  geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
  scale_size_continuous(range = c(1,25),
                        guide = guide_legend(override.aes = list(color = "white")),
                        labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  scale_x_continuous(labels = percent_format(accuracy = 1))+
  labs(x="Percent Positive",size="Total Cases")+
  guides(color = F)

#### CFR by Smokers ####

cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  left_join(
    smokers_by_country_composite,
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3),!is.na(Smokers),location!="World") %>% 
  ggplot(mapping = aes(x=Smokers,y=CFR,color=Country_Code,size=total_cases,label = Country_Code))+
  geom_point()+
  geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
  scale_size_continuous(range = c(1,25),
                        guide = guide_legend(override.aes = list(color = "white")),
                        labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  scale_x_continuous(labels = percent_format(accuracy = 1))+
  labs(x="Smokers",size="Total Cases")+
  guides(color = F)

### CFR By Age ####

cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  left_join(
    median_age %>% 
      select(Country_Code,Median_Age),
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3),location!="World") %>% 
  ggplot(mapping = aes(x=Median_Age,y=CFR,color=Country_Code,size=total_cases,label = Country_Code))+
  geom_point()+
  geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
  scale_size_continuous(range = c(1,25),
                        guide = guide_legend(override.aes = list(color = "white")),
                        labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  labs(x="Median Age",size="Total Cases")+
  guides(color = F)

cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  left_join(
    greeting_scores %>% 
      select(Country_Code,GS),
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3),location!="World") %>% 
  ggplot(mapping = aes(x=GS,y=CFR,color=Country_Code,size=total_cases,label = Country_Code))+
  geom_point()+
  geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
  scale_size_continuous(range = c(1,25),
                        guide = guide_legend(override.aes = list(color = "white")),
                        labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  labs(x="Greeting Score",size="Total Cases")+
  guides(color = F)

cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  left_join(
    exposure_by_country,
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3),location!="World") %>% 
  ggplot(mapping = aes(x=Exposure,y=CFR,color=Country_Code,size=total_cases,label = Country_Code))+
  geom_point()+
  geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
  scale_size_continuous(range = c(1,25),
                        guide = guide_legend(override.aes = list(color = "white")),
                        labels = comma)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  labs(x="Exposure",size="Total Cases")+
  guides(color = F)

#### Pace ####

cases_by_country %>% 
  group_by(Country_Code) %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Population=Population_2019,Statistical_Region),
    by="Country_Code"
  ) %>%
  mutate_at(vars(new_cases,new_deaths),~./(Population/1e6)) %>% 
  mutate_at(vars(date),as.Date) %>% 
  filter(Statistical_Region %in% c(regions1,regions2,regions3)) %>% 
  filter(Country_Code %in% c("IT","ES","FR","DE","US","JP","KR","CA","GB","NL")) %>% 
  filter(date>=(Sys.Date()-days(17))) %>% 
  setDT %>% 
  melt(id.vars=c("date","Country_Code"),measure.vars=c("new_cases","new_deaths")) %>% 
  group_by(Country_Code,variable) %>% 
  mutate(Rank=frankv(value,ties.method = "first",order = -1)) %>% 
  mutate(Label=ifelse(Rank==1,Country_Code,"")) %>% 
  ungroup %>% 
  mutate_at(vars(variable),~ifelse(.=="new_cases","New Cases","New Deaths")) %>% {
    ggplot(.,mapping = aes(x=date,y=value,color=Country_Code,label = Label))+
      geom_line(lwd = 1)+
      geom_point()+
      geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
      facet_wrap(~variable,scales = "free")+
      scale_color_manual(values = color_generator(n = n_distinct(.$Country_Code),color = "deepskyblue"))+
      theme(legend.position = "none")+
      labs(x="",y="Total\nPer\nMillion\nPeople")
  }

#### Modeling ####

training_table <- cases_by_country %>% 
  group_by(Country_Code) %>% 
  slice(n()) %>% 
  ungroup %>% 
  left_join(
    worldpop %>% 
      select(Country_Code,Statistical_Region,Population=Population_2019),
    by="Country_Code"
  ) %>% 
  left_join(
    median_age %>% 
      select(Country_Code,Median_Age),
    by="Country_Code"
  ) %>% 
  left_join(
    smokers_by_country_composite,
    by="Country_Code"
  ) %>% 
  left_join(
    greeting_scores %>% 
      select(Country_Code,GS),
    by="Country_Code"
  ) %>% 
  left_join(
    exposure_by_country,
    by="Country_Code"
  ) %>% 
  left_join(
    beds_by_country,
    by="Country_Code"
  ) %>% 
  mutate(CFR=total_deaths/total_cases)

model_formula <- as.formula(CFR~Median_Age+GS+Exposure+Hospital_Beds+Smokers)

fit_errors <- 1:500 %>% 
  lapply(function(x) {
    
    train <- sample(nrow(training_table),.7*nrow(training_table))
    
    lmfit <- lm(
      formula = model_formula,
      data = training_table %>% 
        slice(train),
      weights = total_cases
    )
    
    forest_fit <- randomForest(
      formula = model_formula,
      data = (
        rfImpute(
          x = model_formula,
          data = training_table
        )
      ) %>% 
        slice(train),
      weights = total_cases,
      na.action = na.omit
    ) 
    
    errors <- training_table %>% 
      mutate(LmCFR=predict(lmfit,.),
             FoCFR=predict(forest_fit,.)) %>% 
      mutate_at(vars(LmCFR,FoCFR),list(Error=~CFR-.)) %>% 
      slice(-train) %>% 
      summarise_at(vars(ends_with("Error")),
                   list(
                     Mean=~weighted.mean(.,w = total_cases,na.rm = T),
                     Median=~median(.,na.rm = T),
                     Max=~max(.,na.rm = T),
                     Min=~min(.,na.rm = T)
                   )) %>% 
      mutate(Trial=x) %>% 
      setDT %>% 
      melt(id="Trial",variable.name="Metric",value.name="Value") %>% 
      mutate(Model=substr(Metric,1,regexpr("_",Metric)-1),
             Metric=str_extract(Metric,"M[a-z]+$"))
    
    lmfit$model <- lmfit$model %>% 
      rename(total_cases=`(weights)`)
    
    require(lm.beta)
    
    z_coeff <- lm.beta(lmfit) %>% 
      coefficients %>% 
      data.frame(
        Estimate=.,
        variable=names(.),
        stringsAsFactors = F,
        row.names = NULL
      ) %>% 
      arrange(-Estimate) %>% 
      select(variable,Estimate)
    
    rf_importance <- importance(forest_fit) %>% 
      data.frame(stringsAsFactors = F) %>% 
      mutate(variable=row.names(.))
    
    list(errors,z_coeff,rf_importance)
    
  })

fit_errors %>% 
  lapply(function(x) {
    x[[1]]
  }) %>% 
  bind_rows %>% 
  group_by(Model,Metric) %>% 
  summarise_at(vars(Value),mean) %>% 
  ungroup %>% 
  list(
    fit_errors %>% 
      lapply(function(x) {
        x[[2]]
      }) %>% 
      bind_rows %>% 
      group_by(variable) %>% 
      summarise_at(vars(Estimate),mean) %>% 
      ungroup %>% 
      arrange(-Estimate),
    fit_errors %>% 
      lapply(function(x) {
        x[[3]]
      }) %>% 
      bind_rows %>% 
      group_by(variable) %>% 
      summarise_at(vars(IncNodePurity),mean) %>% 
      ungroup %>% 
      arrange(-IncNodePurity)
  )

lmfit <- lm(
  formula = model_formula,
  data = training_table,
  weights = total_cases
)

forest_fit <- randomForest(
  formula = model_formula,
  data = (
    rfImpute(
      x = model_formula,
      data = training_table
    )
  ),
  weights = total_cases
) 

training_table %>%
  mutate(Lm=predict(lmfit,.),
         Forest=predict(forest_fit,.)) %>% 
  setDT %>% 
  melt(id.vars=c("Country_Code","CFR"),measure.vars = c("Lm","Forest"),
       variable.name = "Metric",value.name="Value",variable.factor = F) %>% 
  mutate(PlotLimit=pmax(CFR,Value)) %>% 
  filter(!is.na(Value)) %>%  {
    ggplot(.,mapping = aes(x=Value,y=CFR,color = Country_Code,label = Country_Code))+
      geom_abline(color = "white")+
      geom_point(size = 5,alpha = .8)+
      geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
      facet_wrap(vars(Metric)) +
      scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,max(.$PlotLimit,na.rm = T)+.01))+
      scale_x_continuous(labels = percent_format(accuracy = 1),limits = c(0,max(.$PlotLimit,na.rm = T)+.01))+
      labs(x="xCFR",y="CFR")+
      scale_color_manual(values = color_generator(n = n_distinct(.$Country_Code),color = "deepskyblue"))+
      guides(color = F)
  }
