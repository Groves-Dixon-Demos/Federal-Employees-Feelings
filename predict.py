#predict.py

#-------- LIBS
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import os
from pprint import pprint
os. chdir('/Users/grovesdixon/gitreps/Federal-Employees-Feelings')
exec(open("my_functions.py").read())

#--------- GLOBAL VAR
CV=4
N_JOBS=1
RAND_STATE=123


#---------- LOAD THE DATA


fed_train = pd.read_csv('data/train_set.csv.gz', compression='gzip')
fed_train.head()
fed_train.describe()


#--------- PREP DATA FOR MODELING

def prep_data(df):
    
    #get outcome in numerical binary
    y = np.array(df['my_leaving'])
    y = (y=='yes').astype('int')
    
    #format features
    to_drop = ['my_leaving', 'RANDOM', 'AGENCY', 'LEVEL1', 'POSTWT']
    X0 = df.drop(to_drop, 1)
    to_onehot = ['DSEX','DEDUC','DFEDTEN','DSUPER','DMINORITY']
    X = pd.get_dummies(X0, columns = to_onehot)
    return(X, y)


X, y = prep_data(fed_train)
X.shape # (408064, 83)
y.shape # (408064,)

sum(y)

#-------- LOGISTIC REG
#get a baseline prediction with logistic regression

#set up model
from sklearn.linear_model import LogisticRegression
log_reg = LogisticRegression(max_iter=1000)

#train
log_reg.fit(X, y)

#performance on training
probs_log_t = log_reg.predict_proba(X)
scores_log_t = probs_log_t[:, 1]
log_perf_t = get_model_performance('logistic regression training', X, scores_log_t, y, pos_label=1)

#cross validation
probs_log_cv = cross_val_predict(log_reg, X, y, cv=CV, method="predict_proba")
scores_log_cv = probs_log_cv[:, 1]
log_perf_cv = get_model_performance('logistic regression CV', X, scores_log_cv, y, pos_label=1)


#-------- RANDOM FOREST
#build random forest model

#set up model
from sklearn.ensemble import RandomForestClassifier
rf_clf = RandomForestClassifier(n_jobs=N_JOBS)

#train
rf_clf.fit(X, y)

#performance on training
probs_rf_t = rf_clf.predict_proba(X)
scores_rf_t = probs_rf_t[:, 1]
rf_perf_t = get_model_performance('random forest', X, scores_rf_t, y, pos_label=1)


#cross validate
probs_rf_cv = cross_val_predict(rf_clf, X, y, cv=CV,method="predict_proba")
scores_rf_cv = probs_rf_cv[:, 1]
np.mean(scores_rf_cv)
rf_perf_cv = get_model_performance('random forest', X, scores_rf_cv, y, pos_label=1)


#------- OPTIMIZE RF
#use RandomizedSearchCV() to optimized Random forest hyperparameters
from sklearn.model_selection import RandomizedSearchCV
pprint(rf_clf.get_params())

#choose parameter grid values
max_features = ['auto', 'sqrt']
max_depth = [10, 50, 100, 200, 500, None]
min_samples_split = [2, 10, 50, 100, 250, 500]
min_samples_leaf = [1, 5, 10, 25, 100, 250, 500, 1000]
bootstrap = [True, False]

#make grid dict
random_grid = {'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}


# ### run random search
# rf_search = RandomizedSearchCV(estimator = rf_clf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=RAND_STATE, n_jobs = 1)
# rf_search.fit(X, y)
# rf_search.best_params_


#select the best parameters
best_params = {'min_samples_split': 250,
  'min_samples_leaf': 5,
  'max_features': 'sqrt',
  'max_depth': 100,
  'bootstrap': True}


#------- GET OPTIMIZED PERFORMANCE

#set up optimized rf model
rf_opt = RandomForestClassifier(n_jobs=N_JOBS,
                                min_samples_split = best_params['min_samples_split'],
                                min_samples_leaf = best_params['min_samples_leaf'],
                                max_features = best_params['max_features'],
                                max_depth = best_params['max_depth'],
                                bootstrap = best_params['bootstrap'])

#train
rf_opt.fit(X, y)

#performance on training set
probs_opt_t = rf_opt.predict_proba(X)
scores_opt_t = probs_rf_t[:, 1]
opt_perf_t = get_model_performance('optimized random forest training', X, scores_opt_t, y, pos_label=1)



#cross validate
probs_opt_cv = cross_val_predict(rf_opt, X, y, cv=CV, method="predict_proba")
scores_opt_cv = probs_opt_cv[:, 1]
np.mean(scores_opt_cv)
opt_perf_cv = get_model_performance('optimized random forest CV', X, scores_opt_cv, y, pos_label=1)


#------- TRY ON TEST

#read in test set

fed_test = pd.read_csv('data/test_set.csv.gz', compression='gzip')
X_test, y_test = prep_data(fed_train)

#predict for test set
test_probs = rf_opt.predict_proba(X_test)
test_scores = test_probs[:, 1]
test_perf = get_model_performance('optimized random forest TEST', X_test, test_scores, y_test, pos_label=1)


#----- COMPARE WITH SIMPLE CUTOFF 

simple_train = pd.read_csv('data/train_simple.csv.gz', compression='gzip')
simple_test = pd.read_csv('data/test_simple.csv.gz', compression='gzip')

def run_simple(df):
    y=(df['my_leaving'].values=='yes').astype('int')
    satisSq = df['satis']**2
    pred = (satisSq < df['splitter']).astype('int')
    return y, pred

#run simple cutoff on training
s_y, s_pred = run_simple(simple_train)
print(classification_report(s_y, s_pred))


#run simple cutoff on test
s_y_test, s_pred_test = run_simple(simple_test)
print(classification_report(s_y_test, s_pred_test))
