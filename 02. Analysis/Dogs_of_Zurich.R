#Download packages

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

dog_owners <- read.csv("C:/Dogs_in_Zurich/20170308hundehalter.csv")
dog_breeds <- read.csv("C:/Dogs_in_Zurich/zuordnungstabellehunderassehundetyp.csv")

# Rename columns

dog_breeds <- rename(dog_breeds, breed = "HUNDERASSE", breed_type_short = "HUNDERASSENTYP_KURZ", breed_type_long = "HUNDERASSENTYP")
dog_owners <- rename(dog_owners, owner_id = "HALTER_ID", age = "ALTER", owner_gender = "GESCHLECHT", district = "STADTKREIS", breed = "RASSE1", secondary_breed = "RASSE2", birth_year = "GEBURTSJAHR_HUND", dog_gender = "GESCHLECHT_HUND", color = "HUNDEFARBE")

owners_dogs_merged <- merge(dog_owners, dog_breeds, by = c("breed"))
owners_dogs_merged <- select(owners_dogs_merged, -secondary_breed, -RASSENTYP, -STADTQUARTIER, -RASSE1_MISCHLING, -RASSE2_MISCHLING)
owners_dogs_merged <- owners_dogs_merged %>%
  select(owner_id, age, owner_gender, district, breed, birth_year, dog_gender, color, breed_type_short, breed_type_long) %>%
  drop_na()

# Check duplicates

sum(duplicated(dog_owners))

dog_owners <- dog_owners %>%
  distinct()

# Who owns dog more men or women by age group?
#Calculate and plot gender distribution

owner_gender_percentage <- dog_owners %>%
  group_by(owner_gender) %>%
  summarise(number = n()) %>%
  mutate(percent = percent(number / sum(number))) 

owner_gender_percentage$number <- as.numeric(owner_gender_percentage$number)

owner_gender_age <- dog_owners %>%
  group_by(age, owner_gender) %>%
  summarise(num_of_people = n()) %>%
  drop_na()
  
owner_gender_percentage %>%
  ggplot(aes(x = "", y = percent, fill = owner_gender)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#7872f9", "#f3f972")) +
  labs(title= "Owner's Gender Distribution")

ggplot(owner_gender_age, aes(fill=owner_gender, y=num_of_people, x=age)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "Men vs Women Ownership by Age Group", y = "Number of People")+
  scale_fill_manual(values = c("#7872f9", "#f3f972"))


# what is the most popular breed?
#Calculate and plot gender distribution

dog_breed_percentage <- dog_owners %>%
  group_by(breed) %>%
  summarise(number = n()) %>%
  mutate(percent = percent(number / sum(number)))


dog_breed_percentage %>%
  top_n(7, percent) %>%
  ggplot(aes(x = reorder(breed, -number), number, fill = breed)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label=percent, angle = 30),
            position = position_stack(vjust = 1))+
  theme(axis.title.x= element_blank(),
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold")) +
  labs(title= "Dog's Breed Distribution", y = "Number of Dogs") +
  scale_fill_discrete(breaks=c("Mischling klein","Chihuahua","Labrador Retriever", 
                               "Yorkshire Terrier", "Jack Russel Terrier", "Französische Bulldogge", 
                               "Mischling gross"))
  
# What is the most popular color of the dog? 

dog_color <- dog_owners %>%
  group_by(color) %>%
  summarise(number = n()) %>%
  mutate(percent = percent(number / sum(number)))

dog_color$number <- as.numeric(dog_color$number)

dog_color %>%
  top_n(7, percent) %>%
  ggplot(aes(x = reorder(color, - number), number, fill = color)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent, angle = 30),
            position = position_stack(vjust = 1))+
  theme(axis.title.x= element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold")) +
  labs(title= "Dog's Color Distribution", y = "Number of Dogs") +
  scale_fill_discrete(breaks=c("tricolor","weiss","braun", "schwarz/weiss", 
                               "schwarz/braun", "beige", "braun/weiss"))

  

# Dogs gender distribution by district?

dog_gender_district <- owners_dogs_merged %>%
  group_by(district, dog_gender) %>%
  summarise(num_of_dogs = n()) %>%
  drop_na() %>%
  mutate(dog_gender = recode(dog_gender,w = "f"))

ggplot(dog_gender_district, aes(district, num_of_dogs, fill=dog_gender)) + 
  geom_bar(position="dodge", stat="identity", width = 0.6)+
  theme_bw()+
  theme(axis.text.x = element_text(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "Dog's Gender by District", y = "Number of Dogs")+
  scale_fill_manual(values = c("#a9efbe", "#efa9da"))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                      labels = c('1','2','3','4','5','6','7','8','9','10', '11', '12'))

# What kind of dogs old people prefer?

old_people_breed <- owners_dogs_merged %>%
  group_by(breed, age) %>%
  summarise(num_of_dogs = n())

old_people_breed %>% 
  filter(age >= 61&age <=70, num_of_dogs > 25) %>%
  ggplot(aes(age, num_of_dogs, fill = reorder(breed, -num_of_dogs))) +
  geom_bar(position="dodge", stat="identity", width = 0.6)+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.title.x= element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "People of age 61-70", y = "Number of Dogs", legend = "Breed")

old_people_breed %>% 
  filter(age >= 71&age <=80, num_of_dogs > 15) %>%
  ggplot(aes(age, num_of_dogs, fill = reorder(breed, -num_of_dogs))) +
  geom_bar(position="dodge", stat="identity", width = 0.6)+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.title.x= element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "People of age 71-80", y = "Number of Dogs")
  
old_people_breed %>% 
  filter(age >= 81&age <=90, num_of_dogs > 5) %>%
  ggplot(aes(age, num_of_dogs, fill = reorder(breed, -num_of_dogs))) +
  geom_bar(position="dodge", stat="identity", width = 0.6)+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.title.x= element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "People of age 81-90", y = "Number of Dogs")

# Average dogs age per owner age group

owners_dogs_merged$birth_year <- as.numeric(owners_dogs_merged$birth_year)
owners_dogs_merged$birth_year <-as.Date(ISOdate(owners_dogs_merged$birth_year, 1, 1))

owners_dogs_merged$current_age = as.numeric(difftime(Sys.Date(),owners_dogs_merged$birth_year, units = "week"))/52.25


avg_dog_age <- owners_dogs_merged %>%
  group_by(age) %>%
  summarise(num_of_dogs = n(), average_age = mean(current_age))

avg_dog_age %>%
  ggplot(aes(age, average_age)) +
  geom_bar(position="dodge", stat="identity", width = 0.6, fill = "#9490e1")+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.title.x= element_blank(),
        legend.title=element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "Average Dog's Age by Owner's Age Group", y = "Years of Dog")
  
#Popularity of Different Breeds over time
  
dogs_breed_over_time <- owners_dogs_merged %>%
  group_by(birth_year, breed) %>%
  summarise(num_of_dogs = n())

dogs_breed_over_time %>% 
  filter(breed %in% c("Mischling klein", "Chihuahua", "Labrador Retriever", 
                      "Französische Bulldogge", "Malteser", "Yorkshire Terrier", 
                      "Jack Russel Terrier")) %>%
  ggplot(aes(birth_year, num_of_dogs, color = breed)) +
  geom_line(aes(color=breed))+
  geom_point(aes(color=breed), size = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.title.x= element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, face = "bold"))+
  labs(title= "Dog's popularity over time", y = "Number of Dogs")+
  scale_color_discrete(breaks=c("Mischling klein", "Chihuahua", "Labrador Retriever", 
                                "Französische Bulldogge", "Malteser", "Yorkshire Terrier", 
                                "Jack Russel Terrier"))
