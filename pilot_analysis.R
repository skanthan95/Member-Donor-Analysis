---
title: "pilot analysis"
output: html_document
---
  
library(tidyverse)    
library(knitr)
library(here)
library(glue)
library(ggraph)
library(patchwork)
library(broom)
library(modelr)
library(glmnet)
library(caret)
library(psych)
library(coefplot)
library(anytime)
library(scales)


# Purpose: Compare Members, Member-Donors, and Donor wealth-based capacity,
# gender, and other demographic measures; compare Member-Donor and Donor
# donation, lapse, and renewal patterns

# Section 1: Demographic analyses
# Section 2: Donation/lapse analyses

## Files corresponding to Member-Donor, Only-Member, and Only-Donor information respectively

memb_donors <- here("member-donors_merged.csv") %>%
  read_csv(col_types = cols()) 

memb_only <- here("members_only_merged.csv") %>%
  read_csv(col_types = cols())

donor_only <- here("donors_only_merged.csv") %>%
  read_csv(col_types = cols())

## Data cleaning and recoding (main, other smaller cases done as needed later in the code)

memb_s <- memb_only %>%
  mutate(type = 'Member Only') 

memb_don_s <- memb_donors %>%
  mutate(type = 'Member and Donor')

memb_sub <- memb_s %>%
  select(intersect(colnames(memb_s),colnames(memb_don_s)))

memb_don_sub <- memb_don_s %>%
  select(intersect(colnames(memb_s),colnames(memb_don_s)))

all <- rbind(memb_sub, memb_don_sub)

all$gender <- ifelse(is.na(all$Gender), all$MD_DemographicsGender, all$Gender)
all <- all %>%
  rename(memb_type = 'Membership Type') %>%
  rename(start_date = 'Start Date') %>%
  rename(wbc = 'Wealth-Based Capacity') %>%
  rename(qs = "Quality Score") %>%
  rename(next_term = "Next Term") %>%
  rename(end_date = 'End Date') %>%
  rename(hon_fellow = 'Honorary Fellow Status') %>%
  rename(auto_renew = 'Auto Renew') %>%
  rename(ds_rating = 'DS Rating') %>%
  rename(dem_gift_cnt = 'Democratic Gift Count') %>%
  rename(healthcare_cnt = 'Healthcare Count') %>%
  rename(healthcare_tot = 'Healthcare Total') %>%
  rename(higher_ed_cnt = "Higher Education Total") %>%
  rename(largest_gift = "Largest Gift Found") %>%
  rename(pol_likely_cnt = "Political Likely Count") %>%
  rename(pol_likely_tot = "Political Likely Total")
all <- all %>% 
  mutate(gender = recode(gender,
                         "Female/woman" = "Female",
                         "F - Female" = "Female",
                         "Nonbinary" = "Self-identify",
                         "Neutral" = "Self-identify",
                         "Male/man" = "Male",
                         "M - Male" = "Male",
                         "Male" = "Male",
                         "Self-identify" = "Self-identify"))

n_last <- 4
all$start_date <- substr(all$start_date,nchar(all$start_date) - n_last + 1, nchar(all$start_date))
all$start_date <- as.character(all$start_date)
all$end_date_spec <- anydate(all$end_date)
all$end_date <- substr(all$end_date, nchar(all$end_date) - n_last + 1, nchar(all$end_date))
all$end_date <- as.character(all$end_date)
all$MD_HouseholdIncome = factor(all$MD_HouseholdIncome, levels = c("$0-$15,000", "$15,001-$20,000", "$20,001-$30,000",
                                                                   "$30,001-$40,000", "$40,001-$50,000", "$50,001-$60,000", 
                                                                   "$60,001-$75,000", "$75,001-$100,000", "$100,001-$125,000",
                                                                   "$125,001-$150,000", "$150,001+"))
all <- all %>% 
  mutate(Ethnicity = recode(Ethnicity,
                            "D - White" = "White",
                            "C - Asian" = "Asian/Pacific Islander",
                            "Asian" = "Asian/Pacific Islander",
                            "E - Hispanic/Latino" = "Hispanic or Latinx",
                            "A - African American/Black" = "Black or African American",
                            "Hispanic/Latino" = "Hispanic or Latinx",
                            "X - Other" = "Multiple ethnicities/other",
                            "Other" = 'Multiple ethnicities/other',
                            "African American/Black" = "Black or African American",
                            "B - Amer Indian/Alaska Native"= "American Indian/Alaska Native",
                            "Native Hawaiian/Other PacificIslander" = "Asian/Pacific Islander"))

all$wbc = factor(all$wbc, levels = c("A - $100 Million+", "B - $50 Million - $100 Million",
                                     "C - $10 Million - $50 Million", "D - $5,000,000 - $9,999,999",
                                     "E - $1,000,000 - $4,999,999", "F - $500,000 - $999,999",
                                     "G - $250,000 - $499,999", "H - $100,000 - $249,999",
                                     "I - $50,000 - $99,999", "J - $25,000 - $49,999",
                                     "K - $15,000 - $24,999", "L - $10,000 - $14,999",
                                     "M - $5,000 - $9,999", "N - $2,500 - $4,999",
                                     "O - $1 - $2,499", "P - Unable to Rate", "NA"))


all$award <- all$Award
all$award <- ifelse(is.na(all$award), "Did not receive Award", "Received Award")
all$next_term[is.na(all$next_term)] <- "Did not renew"
all$birthyear <- as.character(all$MD_DateOfBirth)
all$birthyear <- substr(all$birthyear, 0, 4)

all$stpf <- ifelse(is.na(all$Fellowship_Class_Exit), "Not STP Fellow", "STP Fellow")
all$hon_fellow <- ifelse(is.na(all$hon_fellow), "Not Honorary Fellow", "Honorary Fellow")

don_only <- donor_only %>%
  mutate(type = 'Donor Only') %>%
  rename(fgd = 'First Gift Date') %>%
  mutate(lapse_date = NU__LapsedOn__c) %>%
  mutate(lapse_date = as.character(lapse_date))
don_only$lapse_date <- substr(don_only$lapse_date, 0, 4)
don_only$lapse_date <- as.character(don_only$lapse_date)
n_last <- 4
don_only$fgd <- substr(don_only$fgd, 
                       nchar(don_only$fgd) - n_last + 1, nchar(don_only$fgd))
don_only$fgd <- as.character(don_only$fgd)

memb_don_s <- memb_don_s %>%
  rename(start_date = 'Start Date') %>%
  rename(end_date = 'End Date') %>%
  rename(fgd = 'First Gift Date') %>%
  mutate(lapse_date = NU__LapsedOn__c) %>%
  mutate(lapse_date = as.character(lapse_date))
memb_don_s$fgd <- substr(memb_don_s$fgd, 
                         nchar(memb_don_s$fgd) - n_last + 1, nchar(memb_don_s$fgd))
memb_don_s$fgd <- as.character(memb_don_s$fgd)
memb_don_s$lapse_date <- substr(memb_don_s$lapse_date, 0, 4)
memb_don_s$lapse_date <- as.character(memb_don_s$lapse_date)
memb_don_s$start_date <- substr(memb_don_s$start_date, nchar(memb_don_s$start_date) - n_last + 1, nchar(memb_don_s$start_date))
memb_don_s$end_date <- substr(memb_don_s$end_date, nchar(memb_don_s$end_date) - n_last + 1, nchar(memb_don_s$end_date))
memb_don_s$start_date <- as.character(memb_don_s$start_date)
memb_don_s$end_date <- as.character(memb_don_s$end_date)

don_only <- don_only %>%
  select(intersect(colnames(don_only),colnames(memb_don_s)))

memb_don_sub <- memb_don_s %>%
  select(intersect(colnames(don_only),colnames(memb_don_s)))

all_donors <- rbind(don_only, memb_don_sub)
don_cols <- c("2021 Giving","2020 Giving","2019 Giving", "2018 Giving", "2017 Giving",
              "2016 Giving","2015 Giving","2014 Giving","2013 Giving",
              "2012 Giving", "2011 Giving","2010 Giving")
all_donors[don_cols] <- lapply(all_donors[don_cols], gsub, pattern='[$,]', replacement='')
all_donors[don_cols] <- lapply(all_donors[don_cols], as.numeric)

# Section 1: Demographic analyses
## Member vs. Member-Donor analysis 

# Member type 

all %>%
  select(type, memb_type) %>%
  #drop_na(memb_type) %>%
  group_by(type, memb_type) %>% 
  summarize(cnt = n()) %>% # counts frequency of member-type subtypes BY member type
  # **** this code is used for almost all remaining graphs, that is always the logic****
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%

  ggplot(aes(x=type, y=cnt, fill=memb_type)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 


all <- all %>%
  mutate(Student_Scientist_Enthusiast = recode(memb_type,
                                               "Regular" = "Scientist",
                                               "Paid Life" = "Scientist",
                                               "Lifetime" = "Scientist",
                                               "Silver" = "Scientist",
                                               "Gold" = "Scientist",
                                               "Platinum" = "Scientist",
                                               "Postdoc" = "Scientist",
                                               "Student" ="Student",
                                               "Student Silver" = "Student",
                                               "Student Gold" = "Student",
                                               "Sponsored" = "Student",
                                               "Sponsored Student Silver" = "Student",
                                               "Advocate" = "Enthusiast",
                                               "Retired" = "Enthusiast",
                                               "Teacher" = "Enthusiast",
                                               "Supporting" = "Enthusiast"))




all %>%
  select(type, Student_Scientist_Enthusiast) %>%
  #drop_na(memb_type) %>%
  group_by(type, Student_Scientist_Enthusiast) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  
  ggplot(aes(x=type, y=cnt, fill=Student_Scientist_Enthusiast)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey') 


# Gender

all %>%
  select(type, gender, end_date_spec) %>%
  #drop_na(memb_type) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  group_by(type, gender) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 1))) %>% 
  arrange(desc(pct)) %>%
  
  
  ggplot(aes(x=type, y=cnt, fill=gender)) +
  geom_col() +
  geom_text(aes(label=pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey') 


all %>%
  select(type, gender, end_date_spec) %>%
  #drop_na(memb_type) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  group_by(type) %>%
  summarize(cnt = n()) 


# Year Joined

all <- all %>%
  mutate(year_clust = case_when(
    start_date >= 2010 ~ '2010-2021',
    start_date >= 2000 & start_date <= 2009 ~ '2000-2009',
    start_date >= 1990 & start_date <= 1999 ~ '1990-1999',
    start_date >= 1980 & start_date <= 1989 ~ '1980-1989',
    start_date >= 1970 & start_date <= 1979 ~ '1970-1979',
    start_date >= 1960 & start_date <= 1969 ~ '1960-1969',
    start_date >= 1950 & start_date <= 1959 ~ '1950-1959',
    start_date >= 1940 & start_date <= 1949 ~ '1940-1949',
    start_date >= 1930 & start_date <= 1939 ~ '1930-1939',
    start_date >= 1920 & start_date <= 1929 ~ '1920-1929',
    start_date <= 1919 ~ 'Before 1919'))

all$order_date <- all$`Order Date`
all$order_date <- substr(all$order_date, 
                         nchar(all$order_date) - n_last + 1, nchar(all$order_date))
all$order_date <- as.character(all$order_date)
all %>%
  group_by(type, order_date) %>%
  summarize(count = n())

drop_ids <-c([redacted])

all_ord <- all %>%
  filter(!Customer_Number_norm %in% drop_ids) %>% 
  select(order_date, type)

all_ord %>%
  group_by(order_date) %>%
  summarize(count = n())

all_ord <- replace(all_ord, is.na(all_ord), "Joined before 2001")

all_ord <- all_ord %>%
  mutate(order_date = recode(order_date,
                             '2001' = '2001-2010',
                             '2002' = '2001-2010',
                             '2003' = '2001-2010',
                             '2004' = '2001-2010',
                             '2005' = '2001-2010',
                             '2006' = '2001-2010',
                             '2007' = '2001-2010',
                             '2008' = '2001-2010',
                             '2009' = '2001-2010',
                             '2010' = '2001-2010',
                             '2011' = '2011-2021',
                             '2012' = '2011-2021',
                             '2013' = '2011-2021',
                             '2014' = '2011-2021',
                             '2015' = '2011-2021',
                             '2016' = '2011-2021',
                             '2017' = '2011-2021',
                             '2018' = '2011-2021',
                             '2019' = '2011-2021',
                             '2020' = '2011-2021',
                             '2021' = '2011-2021'))

all_ord %>%
  group_by(type, order_date) %>%
  summarize(count = n())

all_ord %>%
  select(type, order_date) %>%
  #drop_na(year_join) %>%
  #filter(end_date_spec <= as.Date("2021-09-26")) %>%
  group_by(type, order_date) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=order_date)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  labs(title="Year Joined - Only Members vs. Members/Donors") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2') 


all %>%
  select(type, year_clust, end_date_spec) %>%
  #drop_na(year_join) %>%
  filter(end_date_spec <= as.Date("2021-09-26")) %>%
  group_by(type) %>%
  summarize(cnt = n()) 


# Political party affiliation

ppa <- all %>%
  select(type, MD_PoliticalParty, end_date_spec) %>%
  #drop_na(MD_PoliticalParty) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  group_by(type, MD_PoliticalParty) %>%
  arrange(desc(end_date_spec))

all %>%
  select(type, MD_PoliticalParty, end_date_spec) %>%
  #drop_na(MD_PoliticalParty) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  group_by(type, MD_PoliticalParty) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=MD_PoliticalParty)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  labs(title="Political Affiliation - Only Members vs. Members/Donors") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey') 

all %>%
  select(type, MD_PoliticalParty, end_date_spec) %>%
  #drop_na(MD_PoliticalParty) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  group_by(type) %>%
  summarize(cnt = n())


# Wealth Based Capacity

all_donors$wbc <-all_donors$`Wealth-Based Capacity`
all_mwbc <- all_donors %>%
  select(type, wbc) %>%
  #drop_na(wbc) %>%
  mutate(wealth_bc = case_when(
    wbc == "A - $100 Million+" ~ '>1mil',
    wbc == "B - $50 Million - $100 Million" ~ '>1mil',
    wbc == "C - $10 Million - $50 Million" ~ '>1mil',
    wbc == "D - $5,000,000 - $9,999,999" ~ '>1mil',
    wbc == "E - $1,000,000 - $4,999,999" ~ '>1mil',
    wbc == "F - $500,000 - $999,999" ~ '500k-<1mil',
    wbc == "G - $250,000 - $499,999" ~ '100k-500k',
    wbc == "H - $100,000 - $249,999" ~ '100k-500k',
    wbc == "I - $50,000 - $99,999" ~ '<100k',
    wbc == "J - $25,000 - $49,999" ~ '<100k',
    wbc == "K - $15,000 - $24,999" ~ '<100k',
    wbc == "L - $10,000 - $14,999"~ '<100k',
    wbc == "M - $5,000 - $9,999" ~ '<100k',
    wbc == "N - $2,500 - $4,999" ~ '<100k',
    wbc == "O - $1 - $2,499" ~ '<100k',
    wbc == "P - Unable to Rate" ~ 'NA')) 

all_mwbc <- all_mwbc %>% 
  mutate(wealth_bc = na_if(wealth_bc,
                           "NA"))

all_mwbc$wealth_bc <- factor(all_mwbc$wealth_bc, levels = c('<100k', '100k-500k', '500k-<1mil', '>1mil', NA))

all_wbc <- all %>%
  select(type, wbc, end_date_spec) %>%
  #drop_na(wbc) %>%
  filter(end_date_spec >= as.Date("2021-09-26")) %>%
  mutate(wealth_bc = case_when(
    wbc == "A - $100 Million+" ~ '>1mil',
    wbc == "B - $50 Million - $100 Million" ~ '>1mil',
    wbc == "C - $10 Million - $50 Million" ~ '>1mil',
    wbc == "D - $5,000,000 - $9,999,999" ~ '>1mil',
    wbc == "E - $1,000,000 - $4,999,999" ~ '>1mil',
    wbc == "F - $500,000 - $999,999" ~ '500k-<1mil',
    wbc == "G - $250,000 - $499,999" ~ '100k-500k',
    wbc == "H - $100,000 - $249,999" ~ '100k-500k',
    wbc == "I - $50,000 - $99,999" ~ '<100k',
    wbc == "J - $25,000 - $49,999" ~ '<100k',
    wbc == "K - $15,000 - $24,999" ~ '<100k',
    wbc == "L - $10,000 - $14,999"~ '<100k',
    wbc == "M - $5,000 - $9,999" ~ '<100k',
    wbc == "N - $2,500 - $4,999" ~ '<100k',
    wbc == "O - $1 - $2,499" ~ '<100k',
    wbc == "P - Unable to Rate" ~ 'NA')) 

all_wbc <- all_wbc %>% 
  mutate(wealth_bc = na_if(wealth_bc,
                           "NA"))

all_wbc$wealth_bc <- factor(all_wbc$wealth_bc, levels = c('<100k', '100k-500k', '500k-<1mil', '>1mil', NA))
all_wbc %>%
  group_by(type, wealth_bc) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=wealth_bc)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  labs(title="Wealth-Based Capacity") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey')

all_wbc %>%
  group_by(type) %>%
  summarize(cnt = n()) 

#### Donors WBC


all_mwbc %>%
  group_by(type, wealth_bc) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%

  ggplot(aes(x=type, y=cnt, fill=wealth_bc)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  labs(title="Wealth-Based Capacity") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey')


# Ethnicity


all %>%
  select(type, Ethnicity, end_date_spec) %>%
  filter(end_date_spec <= as.Date("2021-09-26")) %>%
  #drop_na(Ethnicity) %>%
  group_by(type, Ethnicity) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=Ethnicity)) +
  geom_col() +
  geom_text(aes(label= ifelse(pct >= 2.5, pct, "")), position = position_stack(vjust=0.5),color='black', size=3)+
  scale_fill_discrete(name = " ") +
  facet_wrap(~type, scales='free') +
  labs(title="Ethnicity - Only Members vs. Members/Donors") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "") +
  theme_classic() +
  scale_fill_brewer(palette='Set2', na.value='grey') 

all %>%
  select(type, Ethnicity, end_date_spec) %>%
  filter(end_date_spec <= as.Date("2021-09-26")) %>%
  #drop_na(Ethnicity) %>%
  group_by(type) %>%
  summarize(cnt = n())


# AAAS Awards (No award vs. award), NA treated as "no award"

all %>%
  select(type, award, next_term) %>%
  filter(award == "Received Award") %>%
  count(type, award, next_term) %>%
  group_by(type, award) %>%
  rename(a_count='n') %>%
  mutate(pct = scales::percent((round(a_count/sum(a_count), 2)))) %>%
  
  ggplot(aes(x=type, y=a_count, fill=next_term)) +
  geom_col() +
  geom_text(aes(label=pct), position = position_stack(vjust=0.8), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  scale_fill_manual("Legend", values = c("Did not renew" = "grey36", "Renewed" = "cornflowerblue")) +
  labs(title="Renewals - Award") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 

award_only <- all %>%
  select(award, type) %>%
  mutate(award_type = 'Award') %>%
  mutate(status = recode(award,
                         "Did not receive Award" = "No",
                         "Received Award" = "Yes")) %>%
  select(type, status, award_type) %>%
  count(type, award_type, status) %>%
  group_by(type) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 4), "%"))

hon_fell_only <- all %>%
  select(hon_fellow, type) %>%
  mutate(award_type = 'Honorary Fellow')%>%
  mutate(status = recode(hon_fellow,
                         "Not Honorary Fellow" = "No",
                         "Honorary Fellow" = "Yes")) %>%
  select(type, status, award_type)%>%
  count(type, award_type, status) %>%
  group_by(type) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 3), "%"))

stpf_only <- all %>%
  select(stpf, type) %>%
  mutate(award_type = 'ST&P Fellow') %>%
  mutate(status = recode(stpf,
                         "Not STP Fellow" = "No",
                         "STP Fellow" = "Yes")) %>%
  select(type, status, award_type) %>%
  count(type, award_type, status) %>%
  group_by(type) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 3), "%"))

award_df <- rbind(award_only, hon_fell_only, stpf_only)

options(scipen=999)
award_df %>%
  ggplot(aes(x=award_type, y=n, fill=status)) +
  geom_col() +
  geom_text(aes(label=rel.freq), position = position_stack(vjust=0.8), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  ylab("Count") +
  xlab("Award Type") +
  scale_fill_brewer(palette='Set3') +
  theme(axis.text.x = element_text(size=8.5))


# STP Fellows

all %>%
  select(type, stpf, next_term) %>%
  filter(stpf == "STP Fellow") %>%
  count(type, stpf, next_term) %>%
  group_by(type, stpf) %>%
  rename(a_count='n') %>%
  mutate(pct = scales::percent((round(a_count/sum(a_count), 2)))) %>%
  
  
  ggplot(aes(x=type, y=a_count, fill=next_term)) +
  geom_col() +
  geom_text(aes(label=pct), position = position_stack(vjust=0.5), color='black')+
  facet_wrap(~type, scales='free') +
  scale_fill_manual("Legend", values = c("Did not renew" = "grey36", "Renewed" = "cornflowerblue")) +
  labs(title="Renewals - S&T Policy Fellows") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 


# Honorary Fellow


all %>%
  select(type, hon_fellow, next_term) %>%
  filter(hon_fellow == "Not Honorary Fellow") %>%
  count(type, hon_fellow, next_term) %>%
  group_by(type, hon_fellow) %>%
  rename(a_count='n') %>%
  mutate(pct = scales::percent((round(a_count/sum(a_count), 2)))) %>%
  
  
  ggplot(aes(x=type, y=a_count, fill=next_term)) +
  geom_col() +
  geom_text(aes(label=pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  scale_fill_manual("Legend", values = c("Did not renew" = "grey36", "Renewed" = "cornflowerblue")) +
  labs(title="Renewals - Not Honorary Fellows") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 


#  birth year

all$age <- 2021 - as.numeric(all$birthyear)

all %>%
  select(type, age) %>%
  #drop_na(Age) %>%
  group_by(type, age) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=age)) +
  geom_col() +
  facet_wrap(~type, scales='free') +
  labs(title="Age") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() +
  theme(legend.position = "none")

all_age<-all %>% 
  group_by(type, Age) %>% 
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) 

all_age <- all %>%
  #drop_na(Age) %>%
  mutate(agegroup = case_when(
    Age >= 100 ~ '100+',
    Age >= 90 & Age <= 99 ~ '90-99',
    Age >= 80 & Age <= 89 ~ '80-89',
    Age >= 70 & Age <= 79 ~ '70-79',
    Age >= 60 & Age <= 69 ~ '60-69',
    Age >= 50 & Age <= 59 ~ '50-59',
    Age >= 40 & Age <= 49 ~ '40-49',
    Age >= 30  & Age <= 39 ~ '30-29',
    Age >= 20  & Age <= 29 ~ '20-29',
    Age >= 10  & Age <= 19 ~ '10-19'))

all_age$agegroup <- factor(all_age$agegroup, levels = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                                        "70-79", "80-89", "90-99", "100+"))


all_age %>% 
  #drop_na(agegroup) %>%
  group_by(type, agegroup) %>% 
  ggplot(aes(x=agegroup)) +
  geom_bar(aes(y=..prop.., group=1), color='black', fill='cornflowerblue') +
  facet_wrap(~type) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text.y = element_text(size=8))
#dfm <- as.data.frame(dfm)
#write.csv(dfm, 'agegroup.csv')


all_age %>%
  group_by(type, agegroup) %>% 
  drop_na(agegroup) %>%
  summarize(count = n()) %>%
  mutate(pct = scales::percent(round(count / sum(count), 2))) 

all_age %>%
  group_by(type) %>% 
  drop_na(agegroup) %>%
  summarize(avg = mean(Age), med = median(Age))

write_csv(all, 'all_memb_for_model.csv')


## did lapse vs. end date calculations by filtering where end date was null and lapse date wasn't and counting the rows present & taking percentage of total member-donors, etc.


# Donors vs. Member-donors analysis


# first year gift

all_donors %>%
  select(type, fgd) %>%
  #drop_na(fgd) %>%
  group_by(type, fgd) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  ggplot(aes(x=type, y=cnt, fill=fgd)) +
  geom_col() +
  geom_text(aes(label= pct), position = position_stack(vjust=0.5), color='black', size=3)+
  facet_wrap(~type, scales='free') +
  labs(title="First Gift Year Date") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 

# birth year

all_donors$birthyear <- substr(all_donors$MD_DateOfBirth, 0, 4)

all_donors %>%
  select(type, birthyear) %>%
  #drop_na(birthyear) %>%
  group_by(type, birthyear) %>%
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) %>% 
  arrange(desc(pct)) %>%
  
  ggplot(aes(x=type, y=cnt, fill=birthyear)) +
  geom_col() +
  facet_wrap(~type, scales='free') +
  labs(title="Birth Year - Only Donors vs. Members/Donors") +
  ylab("Count") +
  xlab("Type") +
  scale_x_discrete(labels= "Percentage") +
  theme_classic() 

all_donors_age <- all_donors %>%
  #drop_na(age) %>%
  mutate(agegroup = case_when(
    Age >= 100 ~ '100+',
    Age >= 90 & Age <= 99 ~ '90-99',
    Age >= 80 & Age <= 89 ~ '80-89',
    Age >= 70 & Age <= 79 ~ '70-79',
    Age >= 60 & Age <= 69 ~ '60-69',
    Age >= 50 & Age <= 59 ~ '50-59',
    Age >= 40 & Age <= 49 ~ '40-49',
    Age >= 30  & Age <= 39 ~ '30-29',
    Age >= 20  & Age <= 29 ~ '20-29',
    Age >= 10  & Age <= 19 ~ '10-19'))

all_donors_age$agegroup <- factor(all_donors_age$agegroup, levels = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                                                      "70-79", "80-89", "90-99", "100+"))

#df <- # using fordonors only, getting memb and memb from all_age
# bc more accurate
all_donors_age %>% 
  drop_na(Age) %>%
  group_by(type, agegroup) %>% 
  summarize(cnt = n()) %>%
  mutate(pct = scales::percent(round(cnt / sum(cnt), 2))) 

#df <- as.data.frame(df)
#write.csv(df, 'agegroup_data.csv')

all_donors_age %>%
  group_by(type) %>%
  drop_na(agegroup) %>%
  summarize(mean = mean(Age), med = median(Age))


all_donors_age %>%
  group_by(type) %>%
  drop_na(agegroup) %>%
  summarize(count = n())




all_donors$yob <- as.numeric(substr(all_donors$MD_DateOfBirth,1,4))
all_donors$now <- 2021
all_donors$age <- all_donors$now - all_donors$yob

all_donors_age <- all_donors %>%
  #drop_na(age) %>%
  mutate(by_group = case_when(
    age >= 100 ~ '100+',
    age >= 90 & age <= 99 ~ '90-99',
    age >= 80 & age <= 89 ~ '80-89',
    age >= 70 & age <= 79 ~ '70-79',
    age >= 60 & age <= 69 ~ '60-69',
    age >= 50 & age <= 59 ~ '50-59',
    age >= 40 & age <= 49 ~ '40-49',
    age >= 30  & age <= 39 ~ '30-29',
    age >= 20  & age <= 29 ~ '20-29',
    age >= 10  & age <= 19 ~ '10-19'))

all_donors_age$by_group <- factor(all_donors_age$by_group, levels = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                                                      "70-79", "80-89", "90-99", "100+"))


all_donors_pivot <- all_donors %>%
  select(type, "2021 Giving","2020 Giving",
         "2019 Giving","2018 Giving","2017 Giving",
         "2016 Giving","2015 Giving","2014 Giving",
         "2013 Giving","2012 Giving","2011 Giving","2010 Giving") %>%
  pivot_longer(!type, names_to = 'year_donate', values_to = "amount") %>%
  mutate(amount = as.numeric(amount))


all_donors_pivot %>% 
  #drop_na(agegroup) %>%
  group_by(type, year_donate) %>%
  filter(!is.na(amount)) %>%
  #summarize(mean = mean(amount), na.rm=TRUE) %>%
  
  ggplot(aes(x=year_donate, y=amount)) +
  geom_boxplot() +
  #geom_jitter(color='black', size=0.4, alpha=0.9) +
  #scale_y_log10() +
  facet_wrap(~type)

### Sum giving by type

all_donors_sub <- all_donors %>%
  select(don_cols, type, lapse_date, Customer_Number_norm, Email_norm)

all_donors_sub$total_don <- rowSums( all_donors_sub[1:12] )

all_donors_sub <- all_donors_sub %>% 
  #filter(!is.na(total_don)) %>%
  group_by(type) %>% 
  mutate(mean = mean(total_don))

all_donors_sub %>%
  ggplot(aes(x=total_don))+
  geom_histogram(bins=10, 
                 color="black", 
                 fill="dodgerblue",
                 alpha=0.5) +
  facet_wrap(~type) +
  scale_x_log10() +
  geom_vline(aes(xintercept=mean(total_don)),
             color="red",  size=1)


all_donors_sub$total_don <- rowSums( all_donors_sub[1:12] )


all_donors_sub %>% 
  filter(!is.na(total_don)) %>%
  group_by(type) %>% 
  summarize(max = max(total_don), median= median(total_don), avg = mean(total_don)) 

all_donors %>%
  group_by(type) %>%
  summarize(count = n())

#apply(X = all_donors_sub, MARGIN = 1, FUN = sum)
#write.csv(all_donors_sub, 'all_donors_sub.csv')


# Section 2: Donation/lapse analyses
## Lapse vs. donation information


lapse_don <- here("lapse_don_info_gc_final.csv") %>%
  read_csv(col_types = cols(Customer_Number_norm = col_character()))

max_con_one_lapse_zero <- lapse_don %>%
  filter(Num_Lapses == 0 & Max_Consecutive_Donations == 1)

con_one_lapse_zero <- lapse_don %>%
  filter(Num_Lapses == 0 & Num_Donations == 1)

lapse_don %>%
  group_by(Type)%>%
  summarize(count = n())

## Graphing start vs. end and fgd vs. lapse dates for one time donors vs. multi time donors

ggplot(md_multiple_don, aes(x=fgd, y=start_date)) + geom_point(color="darkgreen", alpha=0.1) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text.y = element_text(size=8))

## Num Donation and lapse analysis 

lapse_don %>%
  group_by(Type, Num_Donations)%>%
  summarize(Frequency = n()) %>%
  mutate(pct = scales::percent(round(Frequency / sum(Frequency), 2))) %>%

  ggplot(aes(x = Num_Donations, y = Frequency, label = pct, width = 0.7)) + 
           geom_bar(stat ='identity', fill = 'cornflowerblue', color='black') +
           facet_wrap(~Type, scales = 'free') +
           geom_text(aes(label = pct), vjust = -1, size=2.5, face='bold') +
            scale_x_continuous(breaks = c(0:12), 
                               labels = factor(0:12))+
           ggtitle("Number of Donations and Percent of Frequency (2010 - 2021: 12 years)")

lapse_don %>%
  group_by(Type) %>%
  summarize(mean = mean(Num_Donations))


lapse_don %>%
  filter(Num_Donations > 0) %>%
  group_by(Type, Num_Lapses)%>%
  summarize(Frequency = n()) %>%
  mutate(pct = scales::percent(round(Frequency / sum(Frequency), 3))) %>%

  ggplot(aes(x = Num_Lapses, y = Frequency, label = pct, width = 0.7)) + 
           geom_bar(stat ='identity', fill = 'cornflowerblue', color='black') +
           facet_wrap(~Type, scales='free') +
           geom_text(aes(label = pct), vjust = -1, size=2.5, face='bold') +
           ggtitle("Number of Lapses and Percent of Frequency (2010 - 2021: 12 years)") +
           scale_x_continuous(breaks=seq(0,4,by=1))


### Total donations per year by donor type

options(scipen = 100)

lapse_pivot <- lapse_don %>%
  rename('2010' = '2010 Giving') %>%
  rename('2011' = '2011 Giving') %>%
  rename('2012' = '2012 Giving') %>%
  rename('2013' = '2013 Giving') %>%
  rename('2014' = '2014 Giving') %>%
  rename('2015' = '2015 Giving') %>%
  rename('2016' = '2016 Giving') %>%
  rename('2017' = '2017 Giving') %>%
  rename('2018' = '2018 Giving') %>%
  rename('2019' = '2019 Giving') %>%
  rename('2020' = '2020 Giving') %>%
  rename('2021' = '2021 Giving') %>%
 pivot_longer(
   cols = starts_with("20"),
   names_to = "Year",
   values_to = "dons"
 ) 

graph_text <- lapse_pivot %>%
  select(Year, Type, dons) %>%
  group_by(Year, Type) %>%
  summarize(total_donation = sum(dons)) %>%
  mutate(pct = total_donation/sum(total_donation))

  ggplot(graph_text, aes(x=Year, y=total_donation, fill=Type)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 2, aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5)) +
  scale_y_continuous(breaks = c(250000, 500000, 750000, 1000000, 1250000, 1500000, 1750000, 2000000, 2250000, 2500000, 2750000, 3000000), labels=dollar_format()) +
  scale_fill_brewer(palette='Set2') 

### same graph but for science news donors only

sci_pivot <- sci_don_orig %>%
  filter(!Num_Donations == 0) %>%
  mutate(times = case_when(
    Num_Donations == 1 ~ 'one-time',
    Num_Donations >= 2 ~ 'multi-time')) %>%
  rename('2010' = '2010_Giving') %>%
  rename('2011' = '2011_Giving') %>%
  rename('2012' = '2012_Giving') %>%
  rename('2013' = '2013_Giving') %>%
  rename('2014' = '2014_Giving') %>%
  rename('2015' = '2015_Giving') %>%
  rename('2016' = '2016_Giving') %>%
  rename('2017' = '2017_Giving') %>%
  rename('2018' = '2018_Giving') %>%
  rename('2019' = '2019_Giving') %>%
  rename('2020' = '2020_Giving') %>%
  rename('2021' = '2021_Giving') %>%
 pivot_longer(
   cols = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021'),
   names_to = "Year",
   values_to = "dons"
 ) 

sci_text <- sci_pivot %>%
  select(Year, Type, dons, times) %>%
  group_by(Year, Type, times) %>%
  summarize(total_donation = sum(dons)) %>%
  mutate(pct = total_donation/sum(total_donation))

  ggplot(sci_text, aes(x=Year, y=total_donation, fill=Type)) +
    facet_wrap(~times) +
  geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=7.5)) +
  geom_text(size = 2, aes(label= ifelse(pct > 1, paste0(sprintf("%1.1f", pct*100),"%"), "")), position=position_stack(vjust=0.5)) +
    scale_y_continuous(labels=dollar_format())+
      scale_fill_brewer(palette='Set2') +
    ggtitle("News from Science Donors by Donor Type")


sci_pivot %>%
  select(Year, Type, dons, times) %>%
  group_by(Type, times) %>%
  summarize(total_donation = sum(dons)) %>%
  mutate(pct = total_donation/sum(total_donation))


sci_ids <- unlist(sci_don_id)


#### answering: What percentage of one-time Donors are News from Science Donors?


df <- lapse_don_all %>%
  mutate(sci = case_when(
    Customer_Number_norm %in% sci_ids ~ 'News from Science Donor',
    !Customer_Number_norm %in% sci_ids ~ 'Regular Donor')) %>%
  mutate(times = case_when(
    Num_Donations == 1 ~ 'one-time',
    Num_Donations >= 2 ~ 'multi-time',
    Num_Donations == 0 ~ 'never donated')) %>%
  filter(times != 'never donated') %>%
  select(type, sci, times) %>%
  group_by(type, times, sci) %>%
  summarize(count = n())

df <- transform(df, percent = ave(count, times, FUN = prop.table))
df$percent <- round(df$percent*100, 2)
df$percent <- lapply(df$percent, paste0, "%")

  
  ggplot(df, aes(x=type, y=count, fill=sci)) +
    facet_wrap(~times, scales='free') +
  geom_bar(stat = 'identity') + 
  geom_text(size = 2, aes(label= percent), position=position_stack(vjust=0.5)) +
      scale_fill_brewer(palette='Set2') 
    

#### total number of donations by news for science vs. regular donors 


lapse_don_all %>%
  mutate(sci = case_when(
    Customer_Number_norm %in% sci_ids ~ 'News from Science Donor',
    !Customer_Number_norm %in% sci_ids ~ 'Regular Donor')) %>%
  select(type, sci, Num_Donations) %>%
  group_by(type, sci) %>%
  summarize(total_donations = sum(Num_Donations)) %>%
  mutate(pct = total_donations/sum(total_donations)) %>%

  ggplot(aes(x=type, y=total_donations, fill=type)) +
  facet_wrap(~sci) +
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette='Set2') +
  ggtitle("Total Number of Donations - News from Science vs. Regular Donors")


 lapse_don_all %>%
  mutate(sci = case_when(
    Customer_Number_norm %in% sci_ids ~ 'News from Science Donor',
    !Customer_Number_norm %in% sci_ids ~ 'Regular Donor')) %>%
  select(type, sci) %>%
  group_by(type, sci) %>%
  summarize(count = n()) 


### proportion of regular donors to member-donors


df3 <- all_donors_sub %>%
  mutate(all_donors = 'All Donors') %>%
  group_by(all_donors,type) %>%
  summarize(count = n()) %>%
  mutate(pct = count/sum(count)) 
  
df3$pct <- round(df3$pct*100, 2)
df3$pct <- lapply(df3$pct, paste0, "%")

  ggplot(df3, aes(x=all_donors, y=count, fill = type)) +
  geom_col() + 
  coord_flip() +
  scale_fill_brewer(palette='Set2') +
  geom_text(size = 3, aes(label= pct), position=position_stack(vjust=0.5)) 


#### proportions of donor-only vs. member-donors in news from science donors vs. regular donors


df2 <- lapse_don_all %>%
  mutate(sci = case_when(
    Customer_Number_norm %in% sci_ids ~ 'News from Science Donor',
    !Customer_Number_norm %in% sci_ids ~ 'Regular Donor')) %>%
  select(type, sci) %>%
  group_by(type, sci) %>%
  #filter(sci == 'News from Science Donor') %>%
  summarize(count = n()) 

df2 <- transform(df2, pct = ave(count, sci, FUN = prop.table))
df2$pct <- round(df2$pct*100, 2)
df2$pct <- lapply(df2$pct, paste0, "%")


  ggplot(df2, aes(x=sci, y=count, fill=type)) +
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette='Set2') +
  geom_text(size = 3, aes(label= pct), position=position_stack(vjust=0.5)) 

### Lapse and max consecutive donations


lapse_don %>%
  group_by(Type, Percent_Donations)%>%
  summarize(Frequency = n()) %>%
  mutate(pct = scales::percent(round(Frequency / sum(Frequency), 2))) %>%

  ggplot(aes(x = Percent_Donations, y = Frequency, label = pct)) + 
           geom_bar(stat ='identity', fill = 'cornflowerblue', color='black') +
           facet_wrap(~Type, scales = 'free') +
           geom_text(aes(label = pct), vjust = -1, size=2.5, face='bold') +
           ggtitle("Percent of Donations (2010 - 2021: 12 years)")

lapse_don %>%
  group_by(Type, Max_Consecutive_Donations)%>%
  filter(Num_Donations > 0) %>%
  summarize(Frequency = n()) %>%
  mutate(pct = scales::percent(round(Frequency / sum(Frequency), 2))) %>%

  ggplot(aes(x = Max_Consecutive_Donations, y = Frequency, label = pct)) + 
           geom_bar(stat ='identity', fill = 'cornflowerblue', color='black') +
           facet_wrap(~Type, scales = 'free') +
           geom_text(aes(label = pct), vjust = -1, size=2.5, face='bold') +
            scale_x_continuous(breaks = c(0:12), 
                               labels = factor(0:12))+
           ggtitle("Longest Consecutive Donations (2010 - 2021: 12 years)")










