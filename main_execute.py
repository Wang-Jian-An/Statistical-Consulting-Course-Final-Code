import os
import gc
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tqdm.contrib.itertools

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

import ML_multiclass_for_autogluon

"""
Pipeline: 
1. Load Data
2. Define Input Features and Outcome
3. Define Train, Validation, and Test Data (Loop)
4. Use Train and Validation Data to Fit Model (Loop)
5. Model Evaluation (Loop)

Output:
1. Round Number
2. Model Name
3. Data Set
4. Accuracy
5. Precision for each Class
6. Macro Precision
7. Recall for each Class
8. Macro Recall
9. F1-Score for each Class
10. Macro F1-Score
11. ROC Curve for each Class
12. PRC Curve for each Class
13. Hyper-parameter Search 

"""

# Step1. Load Data
row_data = pd.read_excel("Dataset//DryBeanDataset//Dry_Bean_Dataset.xlsx")

# Step2. Define Input Features and Outcome
target = "Class"
unique_class = row_data[target].unique().tolist()
number_to_class = {index: name for index, name in enumerate(unique_class)}
class_to_number = {name: index for index, name in enumerate(unique_class)}
numerical_features = [i for i in row_data.columns if i != target]
row_data[target] = row_data[target].apply(lambda x: class_to_number[x])

totalResult = list()
for i, normalization_or_not in tqdm.contrib.itertools.product(range(1, 16), [True, False]):
    preprocessing_params = {
        "round": i,
        "normalization_or_not": normalization_or_not
    }

    # Step3. Define Train, Validation and Test Data
    trainData, testData = train_test_split(row_data, test_size = 0.2, shuffle = True)
    trainData, valiData = train_test_split(row_data, test_size = 0.25, shuffle = True)

    if normalization_or_not:
        standardization = StandardScaler().fit(trainData[numerical_features].values)
        normalized_trainData = standardization.transform(trainData[numerical_features].values)
        normalized_valiData = standardization.transform(valiData[numerical_features].values)
        normalized_testData = standardization.transform(testData[numerical_features].values)

        trainData = pd.concat([
            pd.DataFrame(normalized_trainData, columns = numerical_features), trainData[[target]].reset_index(drop = True)
        ], axis = 1)
        
        valiData = pd.concat([
            pd.DataFrame(normalized_valiData, columns = numerical_features), valiData[[target]].reset_index(drop = True)
        ], axis = 1)

        testData = pd.concat([
            pd.DataFrame(normalized_testData, columns = numerical_features), testData[[target]].reset_index(drop = True)
        ], axis = 1)

    # Step4. Use Train and Validation Data to Fit Model
    feature_importances = True
    result_folderpath = "row_report"
    one_round_result = ML_multiclass_for_autogluon.model_fit(result_folderpath = result_folderpath,
                trainData = trainData, 
                valiData = valiData, 
                testData = testData, 
                input_features = numerical_features, 
                target_label = target, 
                number_to_class = number_to_class, 
                preprocessing_params = preprocessing_params,
                hyperparameter_tuning = "bayesopt", 
                feature_importances = feature_importances)

    if feature_importances:
        one_round_result, one_ID_key_Feature_Importance = one_round_result
        #
        one_ID_key_Feature_Importance = [{"round": i, "normalization_or_not": normalization_or_not, **one_result} for one_result in one_ID_key_Feature_Importance]
        one_ID_key_Feature_Importance.extend(one_ID_key_Feature_Importance)
        pd.DataFrame(one_ID_key_Feature_Importance).to_csv(f"row_report//python_AutoML_feature_importance_20220613.csv", index = None)
    else:
        one_round_result = one_round_result.copy()

    one_round_result = [{"round": i, "normalization_or_not": normalization_or_not, **one_result} for one_result in one_round_result]
    totalResult.extend(one_round_result)
    writer = pd.ExcelWriter(f"row_report//python_AutoML_result_20220613.xlsx", engine = "xlsxwriter")
    pd.DataFrame(totalResult).to_excel(writer, index = None)
    writer.close()  