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

time_series <- function(State,metric,return_type = "graph",growth_type = "linear",geotype = "state") {
  
  df <- states_daily %>% 
    group_by(state) %>% 
    mutate_at(vars(positive,death,totalTestResults,total,hospitalizedCumulative,inIcuCumulative),list(new=~.-lag(.))) %>% 
    ungroup %>% 
    mutate_at(vars(ends_with("new")),~ifelse(is.na(.),0,.)) %>% 
    select(date,state,ends_with("new")) %>% 
    rename(Positives=positive_new,
           Deaths=death_new,
           Results=totalTestResults_new,
           `Total Tests`=total_new,
           Hospitalized=hospitalizedCumulative_new,
           `In ICU`=inIcuCumulative_new) %>% 
    mutate(
      `Percent Positive`=ifelse(`Total Tests`==0,NA,Positives/`Total Tests`),
      Negatives=`Total Tests`-Positives
    ) %>% 
    data.table::setnames(old = metric,new = "Variable") %>% 
    # left_join(
    #   state_pop %>% 
    #     select(state=State_Abb,Population=Population_2019),
    #   by="state"
    # ) %>% 
    #mutate_at(vars(Variable),~ifelse(.==0,0,log(.))) %>% 
    #mutate_at(vars(Variable),~./(Population/10000)) %>% 
    filter(state==State)
  
  if (metric=="Percent Positive") {
    df <- df %>% 
      filter(date>="2020-04-01") %>% 
      filter(Variable!=1,Variable>=0)
    growth_type <- "logistic"
  }
  
  m <- df %>%
    select(ds = date, y = Variable) %>%
    arrange(ds) %>% 
    tail(120) %>% 
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
    # left_join(
    #   state_pop %>% 
    #     select(state=State_Abb,Population=Population_2019),
    #   by="state"
    # ) %>% 
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
    ggtitle(metric)+
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
  } else if (return_type=="weekdays") {
    return(by_weekday)
  }
  
}

#### Daily Update ####

states_daily <- "http://covidtracking.com/api/states/daily.csv" %>% 
  read.csv(stringsAsFactors = F,
           colClasses = c("date"="character")) %>% 
  mutate_at(vars(date),~as.Date(.,format = "%Y%m%d")) %>% 
  arrange(date,state) %>% 
  mutate_at(vars(positive,negative,pending,hospitalized,death),~ifelse(is.na(.),0,.))

state_trends <- states_daily %>% 
  pull(state) %>% 
  unique %>% 
  sort %>% 
  lapply(function(y) {
    print(y)
    c("Total Tests","Positives","Deaths","Percent Positive","Hospitalized","In ICU") %>% 
      lapply(function(x) {
        time_series(State = y,metric = x,return_type = "percentages")
      }) %>% 
      bind_rows %>% 
      mutate(State = y)
  })
beepr::beep(sound = "coin")


state_trends %>% 
  bind_rows %>% 
  filter(State %in% state.abb) %>% 
  filter(State %in% (
    filter(.,Metric=="Positives") %>% 
      arrange(-Daily) %>%  
      pull(State) %>% 
      head(15)
  )) %>% 
  # arrange(-Daily) %>% 
  # head(15) %>% 
  ggplot(aes(x=State,y=Daily,fill=State))+
  geom_bar(stat = "identity")+
  facet_wrap(~Metric,scales = "free_y")+
  theme(legend.position = "none")
  mutate_at(vars(Daily,Slope),~ifelse(Metric=="Percent Positive",percent(.,accuracy = .1),comma(.,accuracy = 1))) %>% 
  mutate_at(vars(Increase),~percent(.,accuracy = .1))

y <- "AL"

states_daily %>% 
  group_by(state) %>% 
  mutate_at(vars(positive,death,totalTestResults,total,hospitalizedCumulative,inIcuCumulative),list(new=~.-lag(.))) %>% 
  ungroup %>% 
  mutate_at(vars(ends_with("new")),~ifelse(is.na(.),0,.)) %>% 
  filter(state==y) %>% 
  select(date,state,ends_with("new")) %>% 
  select(date,state,Positives=positive_new,
         Deaths=death_new,
         Results=totalTestResults_new,
         `Total Tests`=total_new,
         Hospitalized=hospitalizedCumulative_new,
         `In ICU`=inIcuCumulative_new) %>% 
  tail(5)

c("Total Tests","Positives","Deaths","Percent Positive","Hospitalized","In ICU") %>% 
  lapply(function(x) {
    time_series(State = y,metric = x,return_type = "percentages")
  }) %>% 
  bind_rows %>% 
  mutate(State = y) %>% 
  mutate_at(vars(Daily,Slope),~ifelse(Metric=="Percent Positive",percent(.,accuracy = .1),comma(.,accuracy = .1))) %>% 
  mutate_at(vars(Increase),~percent(.,accuracy = .1))

c("Total Tests","Positives","Hospitalized","Percent Positive","In ICU","Deaths") %>% 
  lapply(function(x) {
    time_series(State = y,metric = x,return_type = "weekdays") %>% 
      mutate(Type=x)
  }) %>% 
  bind_rows %>% 
  filter(Type %in% c("Total Tests","Positives"))

c("Total Tests","Positives","Hospitalized","Percent Positive","In ICU","Deaths") %>% 
  lapply(function(x) {
    time_series(State = y,metric = x,return_type = "graph")+
      theme(legend.position = "none")
  }) %>% 
  align_plots(
    plotlist = .,
    align = "hv"
  ) %>% 
  plot_grid(
    plotlist = .
  ) %>% 
  plot_grid(
    time_series(State = y,metric = "Total Tests",return_type = "graph") %>% 
      get_legend,
    rel_widths = c(8,1)
  )
