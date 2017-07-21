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


iris = pd.read_csv("/home/sathish/Downloads/iris.csv")

iris['Species'] = iris.Species.map({'setosa': 0, 'versicolor': 1, 'virginica': 2})

train, test = tt(iris, test_size=0.4, random_state=5)

train_target = train.Species
test_target = test.Species

train.drop(['Species'],axis=1,inplace=True)
test.drop(['Species'],axis=1,inplace=True)

###################
## Logistic Regr ##
###################

from sklearn.linear_model import LogisticRegression

logreg = LogisticRegression()

logr = logreg.fit(train,train_target)
logr_pred = logreg.predict(test)

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

confusion_matrix(test_target, d_pred)

accuracy_score(test_target,d_pred)

##################
#####  KNN  ######  
##################

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors=12)

knn.fit(train,train_target)
knn_pred = knn.predict(test)

print(confusion_matrix(test_target,knn_pred))
accuracy_score(test_target,knn_pred)



