#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.metrics import precision_recall_curve
from sklearn.metrics import roc_curve
from sklearn.metrics import roc_auc_score
from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, f1_score
from sklearn.metrics import classification_report



def plot_roc_curve(fpr, tpr, label=None, title=None):
    """Function to plot roc curve from roc_curve() output"""
    plt.plot(fpr, tpr, linewidth=2, label=label)
    plt.plot([0, 1], [0, 1], 'k--')
    plt.axis([0, 1, 0, 1])
    plt.xlabel('False positive rate')
    plt.ylabel('True positive rate')
    plt.title(title)
    plt.show()
    
    

def plot_precision_recall_vs_threshold(precisions, recalls, thresholds, title):
    """Function to plot precision and recall against threshold for calling
    'yes' leaving"""
    
    #precision recall intersect
    prec_rec_df = pd.DataFrame({'prec':precisions[:-1],
                                'rec' : recalls[:-1],
                                'cut' : thresholds})
    prec_rec_df['dist'] = np.absolute(prec_rec_df['prec'] - prec_rec_df['rec'])
    non_zero_df = prec_rec_df.loc[(prec_rec_df['prec'] > 0) & (prec_rec_df['rec'] > 0), ]
    is_min = non_zero_df['dist']==min(non_zero_df['dist'])
    opt_threshold = non_zero_df.loc[is_min, 'cut'].values[0]
    
    #plot precision vs recall
    plt.plot(recalls[:-1], precisions[:-1], "b")
    plt.xlabel("Recall")
    plt.ylabel("Precision")
    plt.ylim([0, 1])
    plt.xlim([0, 1])
    plt.title(title)
    plt.show()
    
    
    #plot
    plt.plot(thresholds, precisions[:-1], "b--", label="Precision")
    plt.plot(thresholds, recalls[:-1], "g-", label="Recall")
    plt.plot(opt_threshold, 0.01, 'k^')
    plt.xlabel("Threshold")
    plt.legend(loc="lower right")
    plt.ylim([0, 1])
    plt.title(title)
    plt.show()
    
    #return intersection
    return(opt_threshold)


def get_model_performance(model_lab, X_train, y_scores, y_actual, pos_label):
    """Function to build precision-recall curve and ROC curve for
    a given model. Returns a dictionary with the precision, recalls,
    thresholds, and AUC."""

    #precision recall curve
    precisions, recalls, thresholds = precision_recall_curve(y_actual, y_scores, pos_label=pos_label)
    opt_threshold = plot_precision_recall_vs_threshold(precisions, recalls, thresholds, title="model={}".format(model_lab))
    
    #print confusion matrix
    opt_pred = (y_scores > opt_threshold).astype('int')
    y_num = (y_actual==pos_label).astype('int')
    print('Confusion matrix for threshold = {}:'.format(round(opt_threshold, 2)))
    cm = confusion_matrix(y_num, opt_pred)
    print(cm)
    print('Classification report:')
    print(classification_report(y_num, opt_pred))
    
    
    #roc curve
    fpr, tpr, thresholds = roc_curve(y_actual, y_scores, pos_label=pos_label)
    auc = round(roc_auc_score(y_actual, y_scores), 3)
    roc_plt = plot_roc_curve(fpr, tpr, title="model={}  AUC={}".format(model_lab, auc))
    
    
    #return results
    res = {'fpr':fpr, 'tpr':tpr, 'thresholds':thresholds, 'auc': auc, 'roc_plt':roc_plt, 'opt_threshold' : opt_threshold}
    return(res)
