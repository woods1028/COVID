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

#### Daily Update ####

states_daily <- "http://covidtracking.com/api/states/daily.csv" %>% 
  read.csv(stringsAsFactors = F,
           colClasses = c("date"="character")) %>% 
  mutate_at(vars(date),~as.Date(.,format = "%Y%m%d")) %>% 
  arrange(date,state) %>% 
  mutate_at(vars(positive,negative,pending,hospitalized,death),~ifelse(is.na(.),0,.))

#### Other Data ####

state_pop <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population" %>% 
  read_html %>% 
  html_table(fill=T) %>% 
  extract2(1) %>% 
  setNames(make.unique(colnames(.))) %>% 
  select(State,Population_2019=`Census population`) %>% 
  mutate_at(vars(Population_2019),~gsub(",","",.) %>% as.integer) %>% 
  filter(!is.na(Population_2019)) %>% 
  left_join(
    data.frame(
      State_Abb=state.abb,
      State_Name=state.name,
      stringsAsFactors=F
    ),
    by=c("State"="State_Name")
  ) %>% 
  mutate_at(vars(State_Abb),~ifelse(State=="District of Columbia","DC",
                                    ifelse(State=="Puerto Rico","PR",.)))

#### Death Path ####

states_daily %>% 
  filter(death>10) %>% 
  group_by(state) %>% 
  mutate(Days=seq(n())) %>% 
  ungroup %>% 
  mutate_at(vars(death),log) %>% 
  left_join(
    group_by(.,state) %>% 
      slice(n()) %>% 
      ungroup %>% 
      mutate(Point="Point",Label=state) %>% 
      select(state,Point,date,Label),
    by=c("state","date")
  ) %>% 
{ ggplot(.,mapping = aes(x=Days,y=death,color=state))+
    geom_line(lwd = 1.5)+
    geom_point(aes(size=Point))+
    geom_label_repel(aes(label = Label),fill = "gray25",size = 6)+
    labs(x="Days Since 10th Death",y="log\nTotal\nDeaths")+
    scale_color_manual(values = color_generator(n = n_distinct(.$state),color = "turquoise1"))+
    theme(legend.position = "none")
}

#### Total Tests ####

states_daily %>% 
  group_by(state) %>% 
  slice(n()) %>% 
  arrange(desc(positive)) %>% 
  head(15) %>% 
  melt(id.vars="state",measure.vars=c("positive","negative"),variable.name="Type",value.name="Total_Tests") %>% 
  ggplot(mapping = aes(x=state,y=Total_Tests,fill=Type))+
  geom_bar(stat = "identity",position = "stack")+
  labs(x="",y="Total\nTests",fill="")+
  scale_fill_manual(labels = c("Positive","Negative"),values = c("red","green2"))+
  scale_y_continuous(labels = comma)

#### Total Tests Per Capita ####

states_daily %>% 
  group_by(state) %>% 
  slice(n()) %>% 
  arrange(desc(positive)) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(positive,negative),~./(Population/10000)) %>% 
  arrange(desc(negative)) %>% 
  head(15) %>% 
  melt(id.vars="state",measure.vars=c("positive","negative"),variable.name="Type",value.name="Total_Tests") %>% 
  ggplot(mapping = aes(x=state,y=Total_Tests,fill=Type))+
  geom_bar(stat = "identity",position = "stack")+
  labs(x="",y="Total\nTests\nPer\n10,000\nPeople",fill="")+
  scale_fill_manual(labels = c("Positive","Negative"),values = c("red","green2"))+
  scale_y_continuous(labels = comma)

#### Test Path ####

states_daily %>% 
  filter(state %in% (
    group_by(.,state) %>% 
      filter(death==max(death)) %>% 
      ungroup %>% 
      select(.,state,death) %>% 
      distinct %>% 
      arrange(desc(death)) %>% 
      head(10) %>% 
      pull(state)
  )) %>% 
  rename(`Total Tests`=totalTestResults,Positives=positive,Deaths=death) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(`Total Tests`,Positives,Deaths),list(Per_Capita=~./Population)) %>% 
  setDT %>% 
  melt(id.vars = c("date","state"),
       measure.vars = c("Total Tests","Positives","Deaths",
                        "Total Tests_Per_Capita","Positives_Per_Capita","Deaths_Per_Capita")) %>% 
  left_join(
    group_by(.,state) %>% 
      slice(n()) %>% 
      ungroup %>% 
      mutate(Point="Point",Label=state) %>% 
      select(state,Point,date,Label),
    by=c("state","date")
  ) %>% 
  { ggplot(.,mapping = aes(x=date,y=value,color=state))+
      geom_line(lwd = 1.5)+
      geom_point(aes(size=Point))+
      geom_label_repel(aes(label = Label),fill = "gray25",size = 6)+
      facet_wrap(~variable,scales = "free_y")+
      labs(y="Total",y="")+
      scale_color_manual(values = color_generator(n = n_distinct(.$state),color = "turquoise1"))+
      theme(legend.position = "none")+
      scale_y_continuous(labels = comma)
  }

#### Tests Per Capita Against Percent Positive ####

states_daily %>% 
  group_by(state) %>% 
  slice(n()) %>% 
  ungroup %>% 
  mutate(percent_positive=positive/totalTestResults) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(totalTestResults),~./(Population/10000))  %>% 
  filter(!is.na(totalTestResults)) %>% 
  ggplot(mapping = aes(x=totalTestResults,y=percent_positive,color = state))+
  geom_point(size = 4)+
  geom_label_repel(aes(label = state),fill = "gray25",size = 6)+
  theme(legend.position = "none")+
  labs(y="Percent\nPositive",x="Tests Per 10,000 People")+
  scale_y_continuous(labels = percent_format(accuracy = 1))

#### Tests Per Capita against CFR ####

states_daily %>%  
  group_by(state) %>% 
  slice(n()) %>% 
  ungroup %>% 
  mutate(resolved=death+recovered) %>% 
  mutate(CFR=death/resolved) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(totalTestResults),~./(Population/10000))  %>% 
  filter(!is.na(totalTestResults)) %>% 
  ggplot(mapping = aes(x=totalTestResults,y=CFR,color = state))+
  geom_point(size = 4)+
  geom_label_repel(aes(label = state),fill = "gray25",size = 6)+
  theme(legend.position = "none")+
  labs(y="CFR",x="Tests Per 10,000 People")+
  scale_y_continuous(labels = percent_format(accuracy = 1))

#### Percent Positive against CFR ####

states_daily %>%  
  group_by(state) %>% 
  slice(n()) %>% 
  ungroup %>% 
  mutate(resolved=death+recovered) %>% 
  mutate(CFR=death/resolved,
         percent_positive=positive/totalTestResults) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(totalTestResults),~./(Population/10000))  %>% 
  filter(!is.na(totalTestResults)) %>% 
  filter(state!="PR") %>% 
  ggplot(mapping = aes(x=percent_positive,y=CFR,color = state))+
  geom_point(size = 4)+
  geom_label_repel(aes(label = state),fill = "gray25",size = 6)+
  theme(legend.position = "none")+
  labs(y="CFR",x="Percent Positive")+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  scale_x_continuous(labels = percent_format(accuracy = 1))

#### Pace ####

states_daily %>%
  group_by(state) %>% 
  mutate(
    new_cases=positive-lag(positive),
    new_deaths=death-lag(death),
    new_tests=totalTestResults-lag(totalTestResults)
  ) %>% 
  ungroup %>% 
  # filter(state %in% (
  #   group_by(.,state) %>%
  #     filter(death==max(death)) %>%
  #     select(.,state,death) %>%
  #     ungroup %>%
  #     distinct %>%
  #     mutate(Rank=frankv(death,order=-1,ties.method = "first")) %>%
  #     filter(Rank<=15) %>%
  #     pull(state)
  # )) %>%
  #filter(state %in% c("TN","AL","FL","GA","MS","LA","TX","SC")) %>% 
  filter(state %in% c("AL","NY","CA","AZ","AL","TX","FL")) %>% 
  left_join(
    state_pop %>% 
      select(state=State_Abb,Population=Population_2019),
    by="state"
  ) %>% 
  mutate_at(vars(new_cases,new_deaths),~./(Population/10000)) %>% 
  filter(date>=(Sys.Date()-days(70))) %>% 
  setDT %>% 
  melt(id.vars=c("date","state"),measure.vars=c("new_cases","new_deaths","new_tests")) %>% 
  group_by(state,variable) %>% 
  mutate(Rank=frankv(value,ties.method = "first",order = -1)) %>% 
  mutate(Label=ifelse(Rank==1,state,"")) %>% 
  ungroup %>% 
  mutate_at(vars(variable),~sub("_"," ",.)) %>% 
  {ggplot(.,mapping = aes(x=date,y=value,color=state,label = Label))+
      geom_line(lwd = 1)+
      geom_point()+
      geom_label_repel(fill = "gray25",size = 5,show.legend = F)+
      facet_grid(variable~.,scales = "free")+
      scale_color_manual(values = color_generator(n = n_distinct(.$state),color = "deepskyblue"))+
      theme(legend.position = "none")+
      labs(x="",y="Total\nPer\n10,000\nPeople")
  }


#### Growth by Groups of Days ####

length_of_group <- 2

day_groups <- 1:100 %>% split(ceiling(seq_along(.)/length_of_group))

group_df <- 1:length(day_groups) %>% 
  lapply(function(i) {
    day_groups[[i]] %>% 
      data.frame(stringsAsFactors = F) %>% 
      setNames("Day") %>% 
      mutate(Group=names(day_groups)[i])
  }) %>% 
  bind_rows %>% 
  mutate_at(vars(Group),as.integer)

states_daily %>%
  # filter(state %in% (
  #   group_by(.,state) %>% 
  #     filter(death==max(death)) %>% 
  #     select(.,state,death) %>% 
  #     ungroup %>% 
  #     distinct %>% 
  #     mutate(Rank=frankv(death,order=-1,ties.method = "first")) %>% 
  #     filter(Rank<=5) %>% 
  #     pull(state)
  # )) %>% 
  group_by(state) %>% 
  mutate(Day=1:n()) %>% 
  left_join(
    group_df,
    by="Day"
  ) %>% 
  group_by(Group,state) %>% 
  summarise(
    positive=sum(positive),
    death=sum(death),
    totalTestResults=sum(totalTestResults),
    total=sum(total),
    NumDays=n_distinct(date)
  ) %>% 
  ungroup %>% 
  filter(NumDays==length_of_group) %>% 
  group_by(state) %>% 
  mutate_at(vars(positive,death,totalTestResults,total),
            list(new=~.-lag(.))) %>% 
  mutate_at(vars(ends_with("new")),~ifelse(is.na(.),0,.)) %>% 
  mutate_at(vars(ends_with("new")),list(growth=~1-lag(.)/.)) %>% 
  ungroup %>% 
  filter(state %in% c("AL","AZ")) %>% 
  #filter(date>=Sys.Date()-days(14)) %>% 
  setDT %>% 
  melt(id.vars=c("Group","state"),
       measure.vars=patterns("growth")) %>% 
  ggplot(mapping = aes(x=Group,y=value,color=state))+
  facet_wrap(~variable,scales = "free_y")+
  geom_point(size = 3)+
  geom_smooth(method = "lm",formula = y~x)+
  scale_y_continuous(labels = percent)

#### Counties ####

county_time_series <- function(State,County,metric,return_type = "graph",growth_type = "linear") {
  
  df <- top6 %>% 
    rename(Positives=cases_new,Deaths=deaths_new) %>% 
    data.table::setnames(old = metric,new = "Variable") %>% 
    filter(county==County,state==State)
  
  if (metric=="Percent Positive") {
    df <- df %>% 
      filter(date>="2020-04-01") %>% 
      filter(Variable!=1,Variable>=0)
    growth_type <- "logistic"
  }
  
  m <- df %>%
    select(ds = date, y = Variable) %>%
    mutate(cap=1,floor=0) %>% 
    prophet(
      holidays = NULL,
      daily.seasonality = F,
      yearly.seasonality = F,
      changepoint.range = .95,
      growth = growth_type
    )
  
  forecast <- predict(m,make_future_dataframe(m,periods = 30) %>% mutate(cap=1,floor=0))
  
  p1 <- forecast %>%
    select(Date=ds,Predicted=yhat,Trend=trend) %>%
    mutate(state=State) %>% 
    setDT %>%
    melt(id="Date",measure.vars = c("Predicted","Trend"),variable.factor = F) %>%
    mutate_at(vars(Date),as.Date) %>%
    mutate_at(vars(value),~ifelse(.<=0,0,.)) %>%
    rbind(
      df %>%
        select(Date=date,value=Variable) %>%
        mutate(variable="Observed")
    ) %>% 
    #mutate_at(vars(value),exp) %>% 
    arrange(desc(variable)) %>% 
    filter(Date<=Sys.Date()+days(3)) %>% 
    ggplot(aes(x=Date,y=value,color=variable))+
    geom_line(lwd = 1.5)+
    scale_y_continuous(labels = comma)+
    ggtitle(County)+
    labs(x="",y="",color="")+
    scale_color_manual(values = c("Observed"="red2","Predicted"="yellow2","Trend"="dodgerblue"))
  
  daily_slope <- forecast %>%
    mutate(Date=as.Date(ds)) %>%
    filter(Date<=Sys.Date()) %>%
    select(Date,trend,yhat) %>%
    mutate(
      Slope1=trend-lag(trend)/as.numeric(Date-lag(Date)),
      Slope2=(lead(trend)-trend)/as.numeric(lead(Date)-Date)
    ) %>% 
    mutate_at(vars(Slope1,Slope2),~round(.,4)) %>% 
    filter(Slope1!=Slope2|is.na(Slope1)) %>% 
    pull(Slope2) %>% 
    tail(1)
  
  daily_trend <- forecast %>% 
    filter(as.Date(ds)<=Sys.Date()) %>% 
    tail(1) %>% 
    pull(trend)
  
  figures <- data.frame(
    Metric=metric,
    Daily=daily_trend,
    Slope=daily_slope,
    Increase=daily_slope/daily_trend,
    stringsAsFactors = F
  )
  
  # Weekdaze #
  
  by_weekday <- forecast %>%
    mutate_at(vars(ds),as.Date) %>%
    mutate(Weekday=format(ds,"%A")) %>%
    group_by(Weekday) %>%
    summarise_at(vars(yhat),mean) %>%
    ungroup %>%
    mutate_at(vars(Weekday),
              ~factor(.,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))) %>%
    mutate(`Per Day`=yhat-mean(yhat)) %>%
    mutate(Pct=`Per Day`/mean(df$Variable)) %>% 
    mutate_at(vars(Weekday),~factor(.,levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))) %>% 
    arrange(Weekday)
  
  if (return_type=="graph") {
    return(p1)
  } else if (return_type=="percentages") {
    return(figures)
  }
  
}


nyt_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate_at(vars(date),as.Date) %>% 
  arrange(date,state,county)

State <- "Alabama"

top6 <- nyt_counties %>% 
  group_by(county,state) %>% 
  mutate_at(vars(cases,deaths),list(new=~.-lag(.))) %>% 
  mutate_at(vars(cases_new,deaths_new),~ifelse(is.na(.),0,.)) %>% 
  ungroup %>% 
  filter(state==State) %>% 
  left_join(
    group_by(.,county) %>% 
      slice(n()) %>% 
      ungroup %>% 
      select(county,cases_total=cases,deaths_total=deaths) %>% 
      mutate_at(vars(cases_total,deaths_total),list(rank=~frankv(.,order = -1,ties.method = "min"))),
    by="county"
  )
  # filter(county %in% c("Tuscaloosa","Madison","Jefferson","Baldwin","Montgomery","Shelby","Marshall","Mobile","Lee")) %>%
  # group_by(county) %>%
  # slice(n()) %>%
  # ungroup %>%
  # arrange(cases_total_rank)
#  filter(cases_total_rank<=6)

top6 %>% 
  filter(cases_total_rank<=6) %>% 
  filter(county=="Jefferson") %>% 
  tail(6)

c("Jefferson","Montgomery","Mobile","Tuscaloosa","Marshall","Lee") %>% 
  lapply(function(x) {
    county_time_series(
      State = State,
      County = x,
      metric = "Positives",
      return_type = "graph"
    )+
      theme(legend.position = "none")
  }) %>% 
  plot_grid(
    plotlist = .
  ) %>% 
  plot_grid(
    county_time_series(
      State=State,
      County = "Jefferson",
      metric = "Positives",
      return_type = "graph"
    ) %>% 
      get_legend,
    nrow = 1,
    rel_widths = c(8,1)
  )
  
  
  
