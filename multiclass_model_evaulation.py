import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.metrics import *

"""
Pipeline
1. Identify ytrue and ypred for each class
2. 

"""


def model_evaluation(ytrue: np.array, 
                     ypred: np.array, 
                     ypred_proba: np.array,
                     number_to_class: dict,
                     roc_curve_filename: str,
                     roc_curve_titlename: str,
                     prc_curve_filename: str,
                     prc_curve_titlename: str):

    # Step1. Identify ytrue and ypred for each class
    ytrue_dict = {
        class_name: np.where(ytrue == class_number, 1, 0).flatten().tolist() for class_number, class_name in number_to_class.items()
    }
    ypred_dict = {
        class_name: np.where(ytrue == class_number, 1, 0).flatten().tolist() for class_number, class_name in number_to_class.items()
    }
    ypred_proba_dict = {
        class_name: ypred_proba[:, class_number].flatten().tolist() for class_number, class_name in number_to_class.items()
    }

    # Find fpr, tpr via ROC Curve
    fpr_dict, tpr_dict, roc_threshold_dict, roc_auc_dict = dict(), dict(), dict(), dict()
    for class_number, class_name in number_to_class.items():
        fpr, tpr, threshold = roc_curve(y_true = ytrue_dict[class_name], y_score = ypred_proba_dict[class_name])
        fpr_dict = {**fpr_dict, f"fpr_{class_name}": fpr}
        tpr_dict = {**tpr_dict, f"tpr_{class_name}": tpr}
        roc_threshold_dict = {**roc_threshold_dict, f"roc_threshold_{class_name}": threshold}
        roc_auc_dict = {**roc_auc_dict, f"roc_auc_{class_name}": auc(x = fpr, y = tpr)}
    
    # Draw ROC Curve
    plt.figure(figsize = (10, 10))
    for class_name in number_to_class.values():
        plt.plot(fpr_dict[f"fpr_{class_name}"], tpr_dict[f"tpr_{class_name}"], label = f"{class_name} - AUC = " + str(round(roc_auc_dict[f"roc_auc_{class_name}"], 4)))
    plt.title(roc_curve_titlename)
    plt.xlabel("False Positive Rate")
    plt.ylabel("True Positive Rate")
    plt.legend()
    plt.savefig(roc_curve_filename)
    plt.show()

    # Find recall, precision via PRC Curve
    prc_recall_dict, prc_precision_dict, prc_threshold_dict, prc_auc_dict = [dict()] * 4
    for class_number, class_name in number_to_class.items():
        prc_precision, prc_recall, prc_threshold = precision_recall_curve(y_true = ytrue_dict[class_name],
                                                                          probas_pred = ypred_proba_dict[class_name])
        prc_precision_dict = {**prc_precision_dict, f"prc_precision_{class_name}": prc_precision}
        prc_recall_dict = {**prc_recall_dict, f"prc_recall_{class_name}": prc_recall}
        prc_threshold_dict = {**prc_threshold_dict, f"prc_threshold_{class_name}": prc_threshold}
        prc_auc_dict = {**prc_auc_dict, f"prc_auc_{class_name}": auc(prc_recall, prc_precision)}

    # Draw PRC Curve
    plt.figure(figsize = (10, 10))
    for class_name in number_to_class.values():
        plt.plot(prc_recall_dict[f"prc_recall_{class_name}"], prc_precision_dict[f"prc_precision_{class_name}"], label = f"{class_name} - AUC = " + str(round(prc_auc_dict[f"prc_auc_{class_name}"], 4)) )
    plt.title(prc_curve_titlename)
    plt.xlabel("Recall")
    plt.ylabel("Precision")
    plt.legend()
    plt.savefig(prc_curve_filename)
    plt.show()

    # F1-Score
    f1_score_dict = {
        f"f1-score_{class_name}": f1_score(y_true = ytrue_dict[class_name], y_pred = ypred_dict[class_name], pos_label = 1) \
            for class_number, class_name in number_to_class.items()
    }
    macro_f1_score = f1_score(y_true = ytrue, y_pred = ypred, average = "macro")

    # Precision 
    precision_dict ={
        f"precision_{class_name}": precision_score(y_true = ytrue_dict[class_name], y_pred = ypred_dict[class_name], pos_label = 1) \
            for class_number, class_name in number_to_class.items()
    }
    macro_precision = precision_score(y_true = ytrue, y_pred = ypred, average = "macro")

    # Recall
    recall_dict ={
        f"recall_{class_name}": recall_score(y_true = ytrue_dict[class_name], y_pred = ypred_dict[class_name], pos_label = 1) \
            for class_number, class_name in number_to_class.items()
    }
    macro_recall = recall_score(y_true = ytrue, y_pred = ypred, average = "macro")

    #  Accuracy
    accuracy_dict ={
        f"accuracy_{class_name}": accuracy_score(y_true = ytrue_dict[class_name], y_pred = ypred_dict[class_name]) \
            for class_number, class_name in number_to_class.items()
    }
    totalAccuracy = accuracy_score(y_true = ytrue, y_pred = ypred)

    # Combine all
    ytrue_dict = {
        f"yTrue_{class_name}": class_contents for class_name, class_contents in ytrue_dict.items()
    }
    ypred_dict = {
        f"yPred_{class_name}": class_contents for class_name, class_contents in ypred_dict.items()
    }
    ypred_proba_dict = {
        f"yPred_proba_{class_name}": class_contents for class_name, class_contents in ypred_proba_dict.items()
    }

    all_score = {
        "totalAccuracy": totalAccuracy,
        "Macro Precision": macro_precision,
        "Macro Recall": macro_recall,
        "Macro F1-Score": macro_f1_score,
        **fpr_dict,
        **tpr_dict,
        **roc_threshold_dict,
        **roc_auc_dict,
        **prc_recall_dict,
        **prc_precision_dict,
        **prc_threshold_dict,
        **prc_auc_dict,
        **f1_score_dict,
        **precision_dict,
        **recall_dict,
        **accuracy_dict,
        **ytrue_dict,
        **ypred_dict,
        **ypred_proba_dict
    }
    return all_score