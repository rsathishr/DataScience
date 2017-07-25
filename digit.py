#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 22 13:22:05 2017

@author: sathish
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


dig = pd.read_csv("/media/sathish/GENERAL/IOA/Datasets/Digit Recognizer/digit_train.csv")

dig.shape

a = dig.iloc[0,1:]
np.sqrt(len(a))
a = a.reshape(28,28)

plt.imshow(a)


############   Train Test Split   #############
from sklearn.cross_validation import train_test_split as tt

train,test = tt(dig, test_size = 0.3,random_state=555)

train_target = train['label']
test_target = test['label']

train = train.iloc[:,1:]
test = test.iloc[:,1:]

############   Model   ##############
# KNN #

from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score


dig_knn = KNeighborsClassifier(n_neighbors=5)
dig_knn.fit(train, train_target)
knn_pred = dig_knn.predict(test)

print(confusion_matrix(test_target, knn_pred))
print(accuracy_score(test_target, knn_pred))

# Random Forest #
from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators=500)
rf.fit(train, train_target)
rf_pred = rf.predict(test)

print(confusion_matrix(test_target, rf_pred))
print(accuracy_score(test_target, rf_pred))

# SVM #

from sklearn import svm

svm_model = svm.SVC()
svm_model.fit(train, train_target)
svm_pred = svm_model.predict(test)

print(confusion_matrix(test_target, svm_pred))
print(accuracy_score(test_target, svm_pred))


############ Dimension Reduction ##############
############     PCA & KNN       ##############

from sklearn.decomposition import PCA

pca = PCA(n_components=0.8)

pca.fit(train)

train1 = pca.transform(train)
test1 = pca.transform(test)


from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score


dig_knn = KNeighborsClassifier(n_neighbors=6)
dig_knn.fit(train1, train_target)
knn_pred = dig_knn.predict(test1)

print(confusion_matrix(test_target, knn_pred))
print(accuracy_score(test_target, knn_pred))
