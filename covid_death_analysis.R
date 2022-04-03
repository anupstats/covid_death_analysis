rm(list = ls())
library(readxl)
DS <- read_excel("Death Summary.xlsx", col_types = c("date", "numeric", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text",  "text", "text", "text", "text", "numeric", "text"))
library(tidyverse)
library(textclean)
DS<- as_tibble(DS)
library(lubridate)
Death_Summary <- DS %>% mutate(Date = as_date(Date))



   Death_Summary$AgeGr <-  recode(Death_Summary$Age, "0:10 = '0-10 years'; 11:20 = '11-20 years' ; 21:30 = '20-30 years'; 31-40 = '31-40 years'; 41:50 = '41-50 years'; 51:60 = '51-60 years'; else = '60+'")
   Death_Summary$Comorbidities <- str_to_title(Death_Summary$Comorbidities)
   
   Death_Summary <-  Death_Summary %>% 
     mutate(Comorbidities = str_split(Comorbidities, ", ")) %>% 
     mutate(Comorbidities = str_split(Comorbidities, ",")) %>% 
     unnest(Comorbidities) 
 
   #for Heart Disease  
   HD <- c('Coronary', 'Cad', 'Heart', 'Vascular', 'Cardiac', 'Mi', 'Lvf', 'Arf', 'Cardiomyopathy', 'Ptca', 'Acs') # words for heart disease
   
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, HD, "<<HD>>" , ignore.case = TRUE)) %>%  
mutate(HD = str_detect(Comorbidities, "<<HD>>"))  %>% 
mutate(Comorbidities = if_else(HD == "TRUE", "Heart Disease", Comorbidities ))
   
   #for Renal Disease  
   RD <- c('Renal', 'Aki', 'Kidney', 'Ckd') # words for renal disease
   
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, RD, "<<RD>>" , ignore.case = TRUE)) %>%  
     mutate(RD = str_detect(Comorbidities, "<<RD>>"))  %>% 
     mutate(Comorbidities = if_else(RD == "TRUE", "Renal Disease", Comorbidities ))
   
   #for Liver Disease  
   LD <- c('Liver', 'Hepatic') # words for renal disease
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, LD, "<<LD>>" , ignore.case = TRUE)) %>%  
     mutate(LD = str_detect(Comorbidities, "<<LD>>"))  %>% 
     mutate(Comorbidities = if_else(LD == "TRUE", "Liver Disease", Comorbidities ))
   
   #for Diabetes
   DM <- c('Diabetes', 'Dm', 'Type') # words for renal disease
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, DM, "<<DM>>" , ignore.case = TRUE)) %>%  
     mutate(DM = str_detect(Comorbidities, "<<DM>>"))  %>% 
     mutate(Comorbidities = if_else(DM == "TRUE", "Diabetes Millitus", Comorbidities ))
   
   #for Stroke
   ST <- c('Stroke', 'Mca', 'Cva', 'Cerebrovascular') # words for renal disease
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, ST, "<<ST>>" , ignore.case = TRUE)) %>%  
     mutate(ST = str_detect(Comorbidities, "<<ST>>"))  %>% 
     mutate(Comorbidities = if_else(ST == "TRUE", "Stroke", Comorbidities ))
   
   #for Cancer
   CA <- c('Ca', 'Carcinoma', 'Malignant') # words for Cancer 
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, CA, "<<CA>>" , ignore.case = TRUE)) %>%  
     mutate(CA = str_detect(Comorbidities, "<<CA>>"))  %>% 
     mutate(Comorbidities = if_else(CA == "TRUE", "Cancer", Comorbidities ))
   
   #for Pulmonary/TB
   TB <- c('Tb', 'Pulmonary', 'Tubercular', 'Tuberculosis', 'Copd') # words for TB/Pulmonary 
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, TB, "<<TB>>" , ignore.case = TRUE)) %>%  
     mutate(TB = str_detect(Comorbidities, "<<TB>>"))  %>% 
     mutate(Comorbidities = if_else(TB == "TRUE", "TB/Pulmonary", Comorbidities ))
   
   #for Thyroid Disease  
   TH <- c('Hypothyroidism', 'Thyroid') # words for thyroid disease
   Death_Summary <-  Death_Summary %>% mutate(Comorbidities = replace_tokens(Comorbidities, TH, "<<TH>>" , ignore.case = TRUE)) %>%  
     mutate(TH = str_detect(Comorbidities, "<<TH>>"))  %>% 
     mutate(Comorbidities = if_else(TH == "TRUE", "Hypothyroidism", Comorbidities ))
   
   
   
  
   

   
 Death_Summary %>% 
   group_by(AgeGr) %>% 
     count(Comorbidities, sort = TRUE) %>% filter(n > 10) %>%
   ungroup() %>% 
    mutate(Comorbidities = reorder(Comorbidities, n)) %>%
    ggplot(aes(Comorbidities, n)) +
    geom_col(aes(fill = Comorbidities), position = "dodge") +
   theme(legend.position = "none") +
   geom_text(aes(label = n), position = position_dodge(0.9), size = 3) +
    facet_wrap(~AgeGr, scales = "free") +
    xlab(NULL) +
    coord_flip()
   
   
 
 geom_col(aes(fill = grp), position = "dodge") +
   geom_text(aes(label = y), position = position_dodge(0.9))
 
 
 Death_Summary  %>% 
   group_by(Site) %>% summarise(site = n()) %>% 
   arrange(desc(site))%>% 
   filter(site>50)
 
 
 ggplot(Death_Summary, mapping = aes(x=Age, fill = Gender)) +
   geom_histogram(bins = 20, color = "white") 
 
 
 ggplot(Death_Summary, mapping = aes(x=Age, col = Gender)) +
   geom_freqpoly(binwidth = 20)
 
 
 
   library(tidytext)
   
   cormorb <- Death_Summary %>%
     unnest_tokens(word, Comorbidities)
   cormorb %>%
     count(word, sort = TRUE) 
   
   cormorb %>%
     group_by(AgeGr) %>%
     count(word, sort = TRUE) %>%
     filter(n > 10) %>%
     mutate(word = reorder(word, n)) %>%
     ggplot(aes(word, n)) +
     geom_col() +
    facet_wrap(~AgeGr, scales = "free_y")+
     xlab(NULL) +
     coord_flip()
   


   cormorb2 <- Death_Summary %>%
     unnest_tokens(bigram, Comorbidities, token = "ngrams", n = 2)
   
   cormorb2 %>%
     count(bigram, sort = TRUE) 
   
   
cormorb2 %>%
     count(bigram, sort = TRUE) %>%
     filter(n > 10) %>%
     mutate(bigram = reorder(bigram, n)) %>%
     ggplot(aes(bigram, n)) +
     geom_col() +
     xlab(NULL) +
     coord_flip()

cormorb2 %>%
  group_by(AgeGr) %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  facet_wrap(~AgeGr, scales = "free_y")+
  xlab(NULL) +
  coord_flip()

library(wordcloud)

cormorb%>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


x<-rep(1:3,3)
x

Death_Summary$AgeGr <-  recode(Death_Summary$Age, "0:10 = '0-10 years'; 11:20 = '11-20 years' ; 21:30 = '20-30 years'; 31-40 = '31-40 years'; 41:50 = '41-50 years'; 51:60 = '51-60 years'; else = '60+'")

datewise_death <- Death_Summary  %>% group_by(Date) %>% summarise(death = n())

datewise_death %>% ggplot(mapping = aes(x = Date, y = death)) +
  geom_line() + geom_smooth()

