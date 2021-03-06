#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 16 17:18:25 2017

@author: sathish
"""

import pandas as pd
import numpy as np
#from sklearn.datasets import load_iris
from sklearn.cross_validation import train_test_split as tt
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score


#Loading the iris dataset from the direstory
iris = pd.read_csv("/home/sathish/Downloads/iris.csv")

#Converting the data to train the model
iris['Species'] = iris.Species.map({'setosa': 0, 'versicolor': 1, 'virginica': 2})

#train,test split using the train_test_split function of test file size 40%
train, test = tt(iris, test_size=0.4, random_state=5)

#Target Variable
train_target = train.Species
test_target = test.Species

#Droping the target variable from the training data
train.drop(['Species'],axis=1,inplace=True)
test.drop(['Species'],axis=1,inplace=True)

###################
## Logistic Regr ##
###################

from sklearn.linear_model import LogisticRegression

#Logistic Regression
logreg = LogisticRegression()
logr = logreg.fit(train,train_target)
logr_pred = logreg.predict(test)

#Confusion Matrix and Accuracy Score
confusion_matrix(test_target, logr_pred)
accuracy_score(test_target,logr_pred)


###################
## Decision Tree ##
###################

from sklearn import tree

dtree = tree.DecisionTreeClassifier()
dtree.fit(train,train_target)
d_pred = dtree.predict(test)

print(test_target)
print(d_pred)

#Confusion Matrix and Accuracy Score
confusion_matrix(test_target, d_pred)
accuracy_score(test_target,d_pred)

##################
#####  KNN  ######  
##################

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors=12)

knn.fit(train,train_target)
knn_pred = knn.predict(test)

#Confusion Matrix and Accuracy Score
print(confusion_matrix(test_target,knn_pred))
accuracy_score(test_target,knn_pred)

##################
# Random Forest  #
##################

from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators=500)
rf.fit(train,train_target)
rf_pred = rf.predict(test)

#Confusion Matrix and Accuracy Score
print(confusion_matrix(test_target,rf_pred))
accuracy_score(test_target,rf_pred)


###################
##      SVM      ##
###################

from sklearn import svm

svm = svm.SVC()

svm.fit(train,train_target)
svm_pred = svm.predict(test)

#Confusion Matrix and Accuracy Score
print(confusion_matrix(test_target,svm_pred))
accuracy_score(test_target,svm_pred)

