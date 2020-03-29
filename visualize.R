#visualize.R
rm(list=ls())
source('my_functions.R')

# LOAD --------------------------------------------------------------------

#load cleaned data otuput from prepare.R
ll=load('data/train_set.Rdata')
ll


# LEAVING FREQUENCIES AMONG GROUPS ------------------------------------------------
#are there any groups particularly wanting to leave?


#first get the grand 'leaving rate'
grand_prop_leaving = sum(fed_train$my_leaving=='yes') / nrow(fed_train)

#split out demographic with outcome
demo_leave = fed_train %>% 
  select(my_leaving, RANDOM:DMINORITY)

#get proportions leaving for each demographic feature
prop_leaving = demo_leave %>% 
  select(-AGENCY, -LEVEL1) %>% 
  pivot_longer(DSEX:DMINORITY,
               names_to = 'demo_feature',
               values_to = 'value') %>% 
  group_by(demo_feature, value) %>% 
  summarize(prop_leaving = sum(my_leaving=='yes')/n())

#function to look at proportion leaving for a given demographic column
barplot_proportion_leaving = function(col){
  prop_leaving %>% 
    filter(demo_feature==col) %>% 
    ggplot(aes_string(x='value',
               y='prop_leaving')) +
    geom_bar(stat='identity') +
    geom_hline(yintercept = grand_prop_leaving,
               lty=2) +
    labs(x=col,
         y='proportion leaving')
}

#plot for the demographic columns
demo_types = demo_leave %>% 
  select(DSEX:DMINORITY) %>% 
  colnames()
bp_list = map(demo_types, barplot_proportion_leaving)  
plot_grid(plotlist = bp_list,
          nrow=3)  


# PRINCIAPL COMPONENT ANLAYSIS OF QUESTIONAIRE -------------
#do different demograpohic groupings explain variance in question respones?

# splice out the question data
q_df = fed_train %>%
  select(my_leaving,
         grep("^Q", colnames(fed_train)))

#RUN PCA

#function to plot pca from normalized expression counts
run_pca = function(df, trait_df, pcs = 2){
  #build pca
  pca <- prcomp(as.matrix(df))
  percentVar <- pca$sdev^2/sum(pca$sdev^2)
  score_df = data.frame(pca$x[,1:pcs])
  res_df = cbind(trait_df, score_df)
  attr(res_df, "percentVar") <- percentVar[1:pcs]
  return(res_df)
}

#run pca 
pca_df = q_df %>% 
  select(-my_leaving) %>% 
  run_pca(demo_leave)

#PLOT PCA

#choose groups
head(pca_df)
groups_to_plot = demo_leave %>% 
  select(-RANDOM, -AGENCY, -LEVEL1) %>% 
  colnames()

#choose pcs
pcs_to_plot = paste('PC', 1:2, sep='')

#plot the densities for each group and pc
pca_plt_list = list()
for (pc in pcs_to_plot){
  for (group in groups_to_plot){
    pca_plt_list[[paste(group,pc,sep='-')]] = plot_pc_density(pca_df, pc, group)
  }
}


#plot accross groups and PCs 1-4
plot_grid(plotlist = pca_plt_list,
          nrow=length(pcs_to_plot))

#plot just leaving
plot_pc_density(pca_df, 'PC1', 'my_leaving') + 
  theme(legend.position = 'right') +
  labs(fill='leaving')

#leaving is skewed to right on PC1, which captures nearly half the variation
#in questionare responses. So it looks like the questionare has useful information
#in predicting whether people will leave.


# LOOK AT CORRELATIONS WITH LEAVING ------------------------------------------------
#how well do the different questions predict 'leaving'?

#convert leaving categories to numeric
num_leaving = q_df %>% 
  mutate(my_leaving = if_else(my_leaving=='yes',
                              1,
                              0))

#get the correlation matrix for question data

cor_df = num_leaving %>% 
  cor() %>% 
  data.frame() %>% 
  rownames_to_column('question') %>% 
  as_tibble() %>% 
  arrange(my_leaving)

#plot distribution of correlations
cor_df %>% 
  filter(question != 'my_leaving') %>% 
  ggplot(aes(x=my_leaving)) +
  geom_histogram() +
  labs(x='correlation with leaving') +
  scale_x_continuous(breaks = seq(-0.4, 0, 0.1), limits = c(-.4, 0))

#so all the questions indicate good things,
#and lower values indicate dissatisfaction


#check top questions
top_qs = cor_df %>% 
  filter(my_leaving < -0.28) %>% 
  pull(question)
top_qs


#Overall questions are strong predictors
# Q69: Considering everything, how satisfied are you with your job?
# Q71: Considering everything, how satisfied are you with your organization?
# Q40: I recommend my organization as a good place to work.

#More interesting, opportunity to rise and feeling appreciated are important
# Q67: How satisfied are you with your opportunity to get a better job in your organization?
# Q11: My talents are used well in the workplace.


# GET CORRELATION BETWEEN QUESTIONS ---------------------------------------
#how similar are the questions?

#get correlation matrix between questions
q_cor_df = q_df %>% 
  select(-my_leaving) %>% 
  cor() %>% 
  data.frame() %>% 
  rownames_to_column('compare') %>% 
  as_tibble() %>% 
  pivot_longer(-compare,
               names_to = 'q',
               values_to = 'cor') %>% 
  filter(!compare==q) %>% 
  arrange(desc(cor)) %>% 
  mutate(i=1:length(compare)) %>% 
  filter(!i%%2==0) %>% 
  select(-i)

#plot histogram
q_cor_df %>% 
  ggplot(aes(x=cor)) +
  geom_histogram()

#top correlated questions
q_cor_df

#So there are simply some highly similar questions
# Q51: I have trust and confidence in my supervisor
# Q52: Overall, how good a job do you feel is being done by your immediate supervisor?
# Q58: Managers promote communication among different work units
# Q59: Managers support collaboration across work units to accomplish work objectives

#could merge these into single questions by taking their mean,
#or could eliminate the one that correlates more weakly with leaving status


# COLLAPSE INTO SINGLE FEATURE --------------------------------------------
#make a simple dissatisfaction feature 

#slice response as numeric
y = q_df %>% 
  mutate(y=my_leaving=='yes') %>% 
  pull(y) %>% 
  as.numeric()

#get mean response
get_mean_q_response = function(df){
  qnum_df = df %>% 
    select(starts_with('Q')) %>% 
    apply(1, mean)
}

#get the mean response
mean_q = get_mean_q_response(fed_train)
sat_df = tibble(leaving = fed_train$my_leaving,
                satis = mean_q)

#correlations
cor(y, mean_q)
cor(pca_df$PC1, y)
cor(pca_df$PC1, mean_q)
cor(mean_q, fed_train$Q69) # Q69: Considering everything, how satisfied are you with your job?


#plot the densities for the mean score
pd1 = plot_density(pwr = 1)
pd2 = plot_density(pwr = 2)
pwr=2
splitter = pd2[['int']]

#build simplified feature set
simple_df = demo_leave %>%
  mutate(satis = mean_q,
         splitter = splitter)

#make simple prediction
library(caret)
simple_confusion = function(df){
  pred_df = df %>% 
  mutate(satisfaction = satis^pwr) %>% 
  mutate(pred = if_else(satisfaction > splitter,
                        'no',
                        'yes'))

  caret::confusionMatrix(data=factor(pred_df$pred),
                         reference = factor(pred_df$my_leaving),
                         positive = 'yes')
}

simple_confusion(simple_df)


# OUTPUT SIMPLE DATAFRAMES ------------------------------------------------

#write out the simplified training set
simple_df %>% 
  write_csv('data/train_simple.csv')

#apply simple model to test set
ll=load('data/test_set.Rdata')
ll
test_satis =get_mean_q_response(fed_test)
simple_test = fed_test %>% 
  mutate(satis = test_satis,
         splitter = splitter) %>% 
  select(colnames(simple_df))

#test simple model
simple_confusion(simple_test)

#write out
simple_test %>% 
  write_csv('data/test_simple.csv')

#compress to fit on github
system('gzip data/train_simple.csv')
system('gzip data/test_simple.csv')



# DOES PREDICTIVE VALUE OF QUESTIONS VARY BY DEMOGRAPHY? ------------------
#how does the correlation between questions and leaving vary between groups?

#look at out demographic types
demo_types

#pivot longer for demographic groups
long_dem = fed_train %>% 
  mutate(my_leaving = if_else(my_leaving=='yes',
                              1,
                              0)) %>%
  select(-RANDOM, -POSTWT, -AGENCY, -LEVEL1) %>% 
  pivot_longer(DSEX:DMINORITY,
               names_to = 'trait',
               values_to = 'trait_value')

#get outcome
y = long_dem %>% 
  pull(my_leaving)


#function to subset a dataframe into the levels for a given column
sub_by_col = function(df, col){
  df=data.frame(df)
  lvls = unique(df[,col])
  df_list = list()
  for (l in lvls){
    df_list[[l]] = df[df[,col]==l,]
  }
  print('levels:')
  print(lvls)
  return(df_list)
}

#get an indepdendent dataframe for each group
dem_dfs = sub_by_col(long_dem, 'trait_value')
names(dem_dfs)

#function to get correlation between each question and leaving
get_cor_leaving = function(df, colname){
  leaving = df %>% 
    pull(my_leaving)
  x = df %>% 
    select(grep("^Q", colnames(df)))
  cors = cor(x, leaving) 
  cor_df = tibble('q' = rownames(cors),
                      'c' = cors[,1])
  colnames(cor_df) = c('q', colname)
  return(cor_df)
}

#populate a list of question-leaving correlations
cor_dfs = list()
for (n in names(dem_dfs)){
  print(n)
  cor_dfs[[n]] = get_cor_leaving(dem_dfs[[n]], n)
}


#assemble into single df
dem_cor_df = purrr::reduce(cor_dfs, left_join, 'q')
qs = dem_cor_df %>% 
  pull(q)

#get correlation of correlations
cor_cors = dem_cor_df %>% 
  dplyr::select(-q) %>% 
  cor()

#plot
library(pheatmap)
pheatmap(cor_cors)






