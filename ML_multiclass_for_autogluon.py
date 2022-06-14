from functools import total_ordering
import os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from tqdm.contrib import itertools
from tqdm import tqdm
tqdm.pandas()

from datetime import datetime
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from lightgbm import LGBMClassifier
from catboost import CatBoostClassifier
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler, Normalizer
from sklearn import decomposition
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn.metrics import *

from autogluon.tabular import TabularPredictor

import warnings
warnings.filterwarnings("ignore")
import multiclass_model_evaulation

"""
本程式碼主旨：針對一組資料，進行模型訓練後，進行模型評估
輸入：訓練資料、驗證資料、測試資料、輸入特徵、目標變數
輸出：報表
Pipeline: 
1. Fit all Models
2. Get all Model Names
3. Model Evaluation for all class
4. 
"""

def model_fit(trainData: pd.DataFrame, 
              valiData: pd.DataFrame, 
              testData: pd.DataFrame, 
              input_features: list, 
              target_label: str, 
              number_to_class: dict, 
              result_folderpath: str, 
              preprocessing_params: dict, 
              hyperparameter_tuning = "bayesopt", 
              feature_importances = False):
    totalResult = list()
    totalFeatureImportanceResult = list()

    # Step1. Fit all Models
    predictor = TabularPredictor(label = target_label, verbosity = 0, problem_type = "multiclass", path = "C://RECYCLE")\
                            .fit(train_data = trainData[input_features + [target_label]], 
                                 tuning_data = valiData[input_features + [target_label]], 
                                 hyperparameter_tune_kwargs = hyperparameter_tuning, 
                                 refit_full = True,
                                 time_limit = 30)

    # Step2. Get all Model Names
    all_model_list = predictor.get_model_names()

    # Step3. Model Evaluation for all class
    for one_model_name, (set_name, set) in\
        itertools.product(all_model_list, zip(["train", "vali", "test"], [trainData, valiData, testData])):

        # Define the file and title name of ROC Curve, PRC Curve, Confusion Matrix
        confusion_matrix_filename = os.path.join(result_folderpath, f"Confusion Matrix of " + one_model_name.replace('\\', '_') + " with Normalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}.png")
        confusion_matrix_titlename = f"Confusion Matrix of {one_model_name}\nNormalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}"
        roc_curve_filename = os.path.join(result_folderpath, f"ROC Curve of " + one_model_name.replace('\\', '_') + " with Normalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}.png")
        roc_curve_titlename = f"Confusion Matrix of {one_model_name}\nNormalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}"
        prc_curve_filename = os.path.join(result_folderpath, f"PRC Curve of " + one_model_name.replace('\\', '_') + " with Normalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}.png")
        prc_curve_titlename = f"Confusion Matrix of {one_model_name}\nNormalization-{preprocessing_params['normalization_or_not']} set-{set_name} in round-{preprocessing_params['round']}" 

        print(f"Get {one_model_name}, {set_name} evaluation")

        # Step3. 將測試資料放入訓練好的模型作預測
        yhat_test = predictor.predict(data = set, model = one_model_name)
        yhat_proba_test = predictor.predict_proba(data = set, model = one_model_name)
        one_model_all_score = multiclass_model_evaulation.model_evaluation(ytrue = set[target_label].values,
                                                                        ypred = yhat_test.values,
                                                                        ypred_proba = yhat_proba_test.values,
                                                                        number_to_class = number_to_class,
                                                                        roc_curve_filename = roc_curve_filename,
                                                                        roc_curve_titlename = roc_curve_titlename,
                                                                        prc_curve_filename = prc_curve_filename,
                                                                        prc_curve_titlename = prc_curve_titlename)

        basic_information = {
            "Model": one_model_name,
            "Features": input_features,
            "Set": set_name,
            "yTrue": set[target_label].values.tolist(),
            "yPred": yhat_test.values.tolist()         
        }

        totalResult.append({**basic_information, **one_model_all_score})

        # Step4. Draw the Confusion Matrix
        plt.figure(figsize = (10, 10))
        cm = confusion_matrix(y_true = set[target_label], y_pred = yhat_test)
        disp = ConfusionMatrixDisplay(cm, display_labels = number_to_class.values())
        disp.plot()
        plt.title(confusion_matrix_titlename)
        plt.xticks(rotation = 90)
        plt.tight_layout()
        plt.savefig(confusion_matrix_filename)
        plt.show()

        # Step5. 變數重要性
        if feature_importances:
            print(f"Get {one_model_name}, {set_name} feature importances")
            feature_importances_information = {
                "Model": [one_model_name] * input_features.__len__(),
                "Set": [set_name] * input_features.__len__()
            }

            feature_importance_result = predictor.feature_importance(
                data = set,
                model = one_model_name,
                features = input_features
            ).reset_index()

            # print(feature_importance_result)
            feature_importance_result = pd.concat([
                pd.DataFrame(feature_importances_information), feature_importance_result
            ], axis = 1).to_dict("records")

            totalFeatureImportanceResult += [{**feature_importances_information, **one_feature_importance_information} for one_feature_importance_information in feature_importance_result]
    if feature_importances == True:
        return totalResult, totalFeatureImportanceResult
    else:
        return totalResult