library(tidyverse)
library(janitor)
library(rvest)
library(ggthemes)
library(gghighlight)
library(tidytuesdayR)


#devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)


members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


##climbing
expeditions %>% 
  group_by(year, peak_name) %>% 
  summarise(number = n()) %>% 
  arrange(desc(number)) %>% 
  filter(peak_name == "Everest") %>% 
  ggplot(aes(year, (number))) +
  geom_area(col = "grey") +
  geom_smooth() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank())


lm_data <- expeditions %>% 
  group_by(year, peak_name) %>% 
  summarise(number = n()) %>% 
  arrange(desc(number)) %>% 
  filter(peak_name == "Everest")

## Model
model <- lm(log(number) ~ year, data = lm_data )

## Percent Change
round((exp(model$coefficients[2])-1)*100,1)

model_data <- augment(model)

model_data %>% 
  clean_names() %>% 
ggplot(aes(year, exp(log_number))) +
  geom_area(col = "grey") +
  geom_line(aes(year, exp(fitted)), col = "grey70", size = 1.5) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank())

##Staff deaths
expeditions %>% 
  filter(peak_name == "Everest") %>% 
  group_by(year) %>% 
  summarise(staff = sum(hired_staff),
            staff_deaths = sum(hired_staff_deaths),
            fatality = round((staff_deaths/staff)*100,1)) %>% 
  arrange(desc(fatality)) %>% 
  filter(fatality <= 500) %>% 
  filter(year > 1950) %>% 
  ggplot(aes(year, (fatality))) +
  geom_area(col = "grey") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank())

##members deaths
expeditions %>% 
  filter(peak_name == "Everest") %>% 
  group_by(year) %>% 
  summarise(members = sum(members),
            members_deaths = sum(member_deaths),
            fatality = round((members_deaths/members)*100,1)) %>% 
  arrange(desc(fatality)) %>% 
  filter(fatality <= 500) %>% 
  filter(year > 1950) %>% 
  ggplot(aes(year, (fatality))) +
  geom_area(col = "grey") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank())


## Total deaths

expeditions %>% 
  filter(peak_name == "Everest") %>% 
  group_by(year) %>% 
  summarise(members = sum(members),
            members_deaths = sum(member_deaths),
            staff = sum(hired_staff),
            staff_deaths = sum(hired_staff_deaths),
            fatality = round(((members_deaths+staff_deaths)/(members+staff))*100,1),
            number = n(),
            Total = ((members+staff))) %>% 
  arrange(desc(fatality)) %>% 
  filter(fatality <= 500) %>% 
  filter(year > 1950) %>%
  ggplot(aes(year, (fatality))) +
  geom_area(col = "grey60", fill = "grey45") +
 # scale_fill_gradient(low = "white", high = "grey") +
  scale_fill_gradient_tableau() +
  theme(panel.background = element_rect(fill = "grey8"),
        plot.background = element_rect(fill = "grey5"),
        panel.grid = element_blank(),
        plot.title = element_text(colour = "grey"),
        axis.title = element_text(color = "grey50"),
        axis.text = element_text(color = "grey70")) +
  annotate("text", x = 2010, y = 10, label = "On an average 1 in every 100 climbers died \n during everest expedition since 2000 ",
           col = "grey50", size = 3.5) +
  ggtitle("Proportion of deaths among climbers") + xlab("Period") + ylab("Proportion of deaths")
