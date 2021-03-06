# -*- coding: utf-8 -*-
"""diabeetus.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1FzRrWC9nne8lenXodyqfP-3gUEXpbmWi
"""

import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler, Normalizer, QuantileTransformer, PowerTransformer, PolynomialFeatures
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasRegressor
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.utils import shuffle

data = pd.read_csv("/content/diabetes.csv")
data = shuffle(data)

data.head()

X_set_unscaled = np.array([data["Glucose"], data["BloodPressure"], data["BMI"], data["Age"]])

X_set_unscaled.shape

Y_set = data["Outcome"]

Y_set.shape

scaler = PolynomialFeatures(2)

X_set_scaled = scaler.fit_transform(X_set_unscaled)
Y_set_scaled = scaler.fit_transform(np.array([Y_set]))

Y_set_scaled

X_train, X_test, Y_train, Y_test = train_test_split(X_set_scaled.T, Y_set_scaled.T, shuffle=True, random_state=4, train_size=0.9)

X_train.shape

data.shape

model = Sequential()
model.add(Dense(250, activation='relu', input_dim=4))
model.add(Dense(125, activation='relu'))
model.add(Dense(1, activation='sigmoid'))
model.compile(optimizer='adam', 
              loss='binary_crossentropy', 
              metrics=['accuracy'])

res = model.fit(X_train, Y_train, epochs=50, validation_split=0.3 )

model.evaluate(X_test, Y_test)

model.save("diabeetus.h5")

