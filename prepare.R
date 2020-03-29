#prepare.R
rm(list=ls())
library(tidyverse)
library(rlang)
library(splitstackshape)
library(cowplot)
theme_set(theme_cowplot())


# UPLOAD AND SAVE FOR SAKE OF SIZE ON GITHUB ------------------------------

fed_raw = read_csv('data/FEVS_2018_PRDF.csv.gz')
glimpse(fed_raw)


# SET UP FUNCTION FOR TRANSLATING CODES -----------------------------------

#create a translation list for clarity when plotting
my_codes = read_tsv('data/my_demongraphy_codes.tsv') #build manually from 2018 PRDF Codebook.xlsx
my_codes

translation_list = list()
for (f in my_codes$feature){
  translation_list[[f]] = list()
  fsub = my_codes %>% 
    filter(feature==f)
  for (c in fsub$code){
    value = fsub %>% 
      filter(code==c) %>% 
      pull(value)
    translation_list[[f]][[c]] = value
  }
}

translate_codes = function(feature, coded_vector){
  translation_list[[feature]][coded_vector] %>% 
    unlist()
}


# CLEAN OUTCOME VARIABLE --------------------------------------------------
#want just a binary response, yes or no
#check frequencies and collapse 

#get frequencies
outcome_freqs = fed_raw %>% 
  pull(DLEAVING) %>% 
  table() %>% 
  data.frame(row.names = 1) %>% 
  rownames_to_column('code') %>% 
  mutate(outcome = translate_codes('DLEAVING', code))
outcome_freqs

#plot frequency of outcomes in the set
outcome_freqs %>% 
  mutate(outcome = paste('(',code,') ', outcome, sep='')) %>% 
  ggplot(aes(x = outcome,
             y = Freq)) +
  geom_bar(stat='identity') +
  labs(y='frequency',
       subtitle='DLEAVING answer frequencies')

#collapse
yes_codes = c('B', 'C')
fed_mod1 = fed_raw %>% 
  mutate(my_leaving = if_else(DLEAVING %in% yes_codes,
                              'yes',
                              'no')) %>% 
  select(-DLEAVING)

#re-check
fed_mod1 %>% 
  ggplot(aes(x=my_leaving)) +
  geom_bar() +
  labs(x='leaving',
       subtitle='my_leaving frequencies')


# CLEAN QUESTION RESPONSES ------------------------------------------------
#assess how often we have "X" indicating "dont' know"
#these trash our ordinal data, so need to deal with them

#isolate question responses
head(fed_mod1)
fed_qs = fed_mod1 %>% 
  select(grep("^Q", colnames(fed_mod1))) 
glimpse(fed_qs)

#count instances with 'I don't know'
x_count = apply(fed_qs, 2, function(x) table(x)['X'])
x_df = tibble('question' = names(x_count),
                  'nX' = x_count/nrow(fed_qs)) %>% 
  mutate(has_idk = if_else(is.na(nX),
                           "no X",
                           "has X"))

#how often does at least one person say "don't know"?
x_df %>% 
  ggplot(aes(x=has_idk)) +
  geom_bar() +
  labs(x='response type',
       subtitle = "frequency of questions with Xs")
#so we can't just remove these

#when someone said "don't know" what proportion usually do?
x_df %>% 
  filter(!is.na(nX)) %>% 
  ggplot(aes(x=nX*100)) + 
  geom_density() +
  labs(x="% responding X",
       subtitle = 'frequency of X responses within questions')
#it's always less than 10%, so we can impute these without much worry
#will replace all Xs with the median response for the question


#how often is data missing altogether (NAs)
na_count = sum(is.na(fed_qs))
na_count / (nrow(fed_qs)*ncol(fed_qs))
#these are also fairly rare, so will impute those as well


#function to get median for each column even if it has Xs
get_median = function(vector){
  num_vector = vector[vector != "X"] %>% 
    as.numeric()
  median(num_vector, na.rm=TRUE)
}

#function to impute Xs and NAs
impute_x = function(column, values_to_impute = c('X')){
  med = get_median(column)
  column[column %in% values_to_impute]<-med 
  column[is.na(column)]<-med
  as.numeric(column)
}

#run imputing
mod_fed_qs = apply(fed_qs, 2, function(x) impute_x(x, c('X'))) %>% 
  data.frame()
head(mod_fed_qs)
dim(mod_fed_qs)
dim(fed_qs)



#recombine into new df
fed_mod2 = fed_mod1 %>% 
  select(-grep("^Q", colnames(fed_mod1))) %>% 
  cbind(mod_fed_qs) %>% 
  as_tibble()
dim(fed_mod1)
dim(fed_mod2)



# CLEAN DEMOGRAPHIC DATA --------------------------------------------------

#isolate demongraphic data
demo_df = fed_mod2 %>% 
  select(RANDOM, AGENCY:DMINORITY)

#check frequency of missing data
demo_df %>% 
  pivot_longer(AGENCY:DMINORITY) %>% 
  group_by(name) %>% 
  summarize(prop_NA = sum(is.na(value))/n()) %>% 
  data.frame()
  
#how many would we lose?
na_rows = apply(demo_df, 1, function(x) sum(is.na(x))) > 0
sum(na_rows)
sum(na_rows) / nrow(demo_df)

#which are the NA rows?
na_df = demo_df %>% 
  filter(na_rows)


#hard decision, is client is mostly interested in 
#how question responses influence decision to leave,
#then we should keep these NA rows and remove the 
#demographic features. If on the other hand we 
#are primarily interested in the potential role
#of these features in decision to quit, we should
#remove these. Will go the removal option for demo.

fed_mod3 = fed_mod2 %>% 
  filter(!na_rows)



# TRANSLATE DEMOGRAPHIC CODES ---------------------------------------------

#now that we removed the missing data from demographic featuers
#we can swap to a fully translated dataframe

#get columns to translate
cols_to_trans = my_codes %>% 
  filter(feature !='DLEAVING') %>% 
  pull(feature) %>% 
  unique()

#do translations
fed_mod4 = data.frame(fed_mod3)
for (col in cols_to_trans){
  print(col)
  col_sym = rlang::sym(col)
  col_vec = fed_mod4 %>% 
    pull(col)
  trans_vec = translate_codes(col, col_vec)
  fed_mod4[,col] = trans_vec
}

fed_mod4 = as_tibble(fed_mod4)
fed_mod4


# FINAL CLEANED OUTPUT ----------------------------------------------------

#rename and order for output
my_fed = fed_mod4 %>% 
  select(my_leaving,
         RANDOM,
         everything())


#CREATE TRAINING AND TEST SETS

#choose feature to stratify on
strat_features = demo_df %>% 
  select(-RANDOM) %>% 
  colnames()

#make stratified test set
set.seed(12345)
fed_test = stratified(indt = data.frame(my_fed),
                      group = strat_features,
                      size = 0.20) %>% 
  as_tibble()

#split out training set
fed_train = my_fed %>% 
  filter(!RANDOM %in% fed_test$RANDOM)

#check
nrow(fed_test) / nrow(my_fed)
nrow(fed_train) / nrow(my_fed)



#write out 
write_csv(fed_test,
          path='data/test_set.csv')
write_csv(fed_train,
          path='data/train_set.csv')

#compress to fit on github
system('gzip data/test_set.csv')
system('gzip data/train_set.csv')

#also save R objects
save(fed_test, file='data/test_set.Rdata')
save(fed_train, file='data/train_set.Rdata')
