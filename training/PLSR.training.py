 

# Import necessary libraries
import numpy as np  # For numerical operations
import csv  # For CSV file operations
import os  # For operating system related operations
from datetime import datetime  # For handling date and time
import glob  # For file path pattern matching
import matplotlib.pyplot as plt  # For plotting graphs
import pandas as pd  # For data manipulation and analysis
from sklearn.cross_decomposition import PLSRegression  # For PLS Regression
from sklearn.metrics import mean_squared_error  # For calculating mean squared error
from sklearn.cross_decomposition import PLSSVD  # For Partial Least Squares Singular Value Decomposition
# Note: mean_squared_error is imported again, which is redundant
from sklearn.datasets import make_regression  # For generating regression data
from sklearn.cross_decomposition import CCA  # For Canonical Correlation Analysis
from sklearn.datasets import make_multilabel_classification  # For generating multilabel classification data
from sklearn.datasets import make_classification  # For generating classification data
from sklearn.preprocessing import StandardScaler  # For standardizing features
from sklearn.model_selection import train_test_split  # For splitting data into train and test sets
from sklearn.metrics import accuracy_score, confusion_matrix  # For calculating accuracy and confusion matrix
from sklearn.cross_decomposition import PLSCanonical  # For PLS Canonical
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis  # For Linear Discriminant Analysis
from sklearn.decomposition import PCA  # For Principal Component Analysis
from sklearn.ensemble import RandomForestClassifier  # For Random Forest Classifier
from pyopls import OPLS  # For Orthogonal Projections to Latent Structures
from sklearn.model_selection import cross_val_score, KFold
 
 
 


# Define a function for PLS Regression with an option for training
def PLSR2(X, Y, X_test, n_components=2, training=False):
    if training:
        pls = PLSRegression(n_components=n_components)  # Initialize PLS Regression with specified components
        pls.fit(X, Y)  # Fit the model to the data
        pred = pls.predict(X_test)  # Predict using the fitted model
        return pred
    else:
        pred = pls.predict(X_test)  # Predict using a previously fitted model, 'pls' should be defined globally
        return pred

# Define a function for Linear Discriminant Analysis with an option for training
def LDA(X, Y, X_test, training=False):
    if training:
        lda = LinearDiscriminantAnalysis()  # Initialize LDA
        lda.fit(X, Y)  # Fit the model to the data
        pred = lda.predict(X_test)  # Predict using the fitted model
        return pred
    else:
        pred = lda.predict(X_test)  # Predict using a previously fitted model, 'lda' should be defined globally
        return pred

# Define a function for PLS Canonical with an option for training
def PLSR_Canonical(X, Y, X_test, n_components=2, training=False):
    if training:
        plsc = PLSCanonical(n_components=n_components)  # Initialize PLS Canonical with specified components
        plsc.fit(X, Y)  # Fit the model to the data
        pred = plsc.predict(X_test)  # Predict using the fitted model
        return pred
    else:
        pred = cca.predict(X_test)  # Predict using a previously fitted model, 'cca' should be defined globally
        return pred

# Define a function for Canonical Correlation Analysis with an option for training
def PLSR_CCA(X, Y, X_test, n_components=2, training=False):
    if training:
        cca = CCA(n_components=n_components)  # Initialize CCA with specified components
        cca.fit(X, Y)  # Fit the model to the data
        pred = cca.predict(X_test)  # Predict using the fitted model
        return pred
    else:
        pred = cca.predict(X_test)  # Predict using a previously fitted model, 'cca' should be defined globally
        return pred

# Define a function for combining PCA with Random Forest for classification
def PLSR_PCA(X, Y, X_test, n_components=2, training=False):
    if training:
        pca = PCA(n_components=n_components)  # Initialize PCA with specified components
        X_train_pca = pca.fit_transform(X)  # Fit PCA and apply dimensionality reduction on X
        X_test_pca = pca.transform(X_test)  # Apply dimensionality reduction on X_test
        model = RandomForestClassifier(random_state=42)  # Initialize Random Forest Classifier
        model.fit(X_train_pca, Y)  # Fit the model to the transformed data
        pred = model.predict(X_test_pca)  # Predict using the fitted model
        return pred
    else:
        pred = model.predict(X_test_pca)  # Predict using a previously fitted model, 'model' should be defined globally
        return pred



def CrossVal(X, Y):    
    
    # Function to perform cross-validation and calculate average MSE
    def cross_validate_pls(X, Y, n_components, cv_folds=25):
        pls = PLSRegression(n_components=n_components)
        kf = KFold(n_splits=cv_folds, shuffle=True, random_state=42)
        mse_scores = -cross_val_score(pls, X, Y, cv=kf, scoring='neg_mean_squared_error')
        return mse_scores.mean()
    
    # Range of PLS components to evaluate
    n_components_range = range(1, min(len(X), len(X[0])) + 1)  # Up to min(n_samples, n_features)
    
    # Evaluate models with different numbers of components
    mse_results = []
    for n_components in n_components_range:
        mse = cross_validate_pls(X, Y, n_components)
        mse_results.append((n_components, mse))
        print(f'Number of Components: {n_components}, MSE: {mse}')
    
    # Find the number of components with the lowest MSE
    optimal_components, optimal_mse = min(mse_results, key=lambda x: x[1])
    print(f'Optimal number of PLS components: {optimal_components}, with MSE: {optimal_mse}')



# Preprocess data
X_scaled = StandardScaler().fit_transform(X)  # Scale predictor variables

# Hierarchical clustering
cluster = AgglomerativeClustering(n_clusters=None, distance_threshold=0, affinity='euclidean', linkage='ward')
cluster_labels = cluster.fit_predict(X_scaled)

# Calculate cluster summaries
cluster_summaries = np.array([X_scaled[:, cluster_labels == i].mean(axis=1) for i in range(max(cluster_labels) + 1)]).T

# PLS Regression
plsr = PLSRegression(n_components=optimal_components)
plsr.fit(cluster_summaries, Y)


# Start of script execution, reading training dataset
path = r'./bs1'  # Define the path to training data
all_files = glob.glob(os.path.join(path, "car*bs1"))  # Use glob to find all files matching the pattern
mpg = []  # Initialize an empty list to store dataframes
for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)  # Read each CSV file into a dataframe
    mpg.append(df)  # Append the dataframe to the list

npg = np.concatenate(mpg)  # Concatenate all dataframes in the list
results_train = np.array(npg).astype("float")  # Convert the concatenated array to float
X_Training = results_train[:, 0:15]  # Extract features for training
Y_Training = results_train[:, 15:]  # Extract target variable for training


CrossVal(X_Training,Y_Training)
# Start of script execution, reading test dataset
if True:
    path = r'./concatenated_bs2'  # Define the path to test data
    all_files = glob.glob(os.path.join(path, "concatenated_carfentanil_bs2"))  # Use glob to find all files matching the pattern
    mpg = []  # Reinitialize the list to store dataframes for test data
    for filename in all_files:
        df = pd.read_csv(filename, index_col=None, header=0)  # Read each CSV file into a dataframe

    results_test = np.array(df).astype("float")  # Convert the dataframe to float

    batch_Size = 256  # Define batch size for processing
    samples = int(np.floor(len(results_test) / batch_Size) * batch_Size)  # Calculate the number of samples to process
    X_test = results_test[0:samples, 0:15]  # Extract features for testing
    Y_true = results_test[0:samples, 15:]  # Extract true target values for testing

    # Perform predictions using different models
    Y_pred_PLSR = PLSR2(X=X_Training, Y=Y_Training, X_test=X_test, n_components=15, training=True)  # Predict using PLSR
    Y_pred_PLSR_CCA = PLSR_CCA(X=X_Training, Y=Y_Training, X_test=X_test, n_components=15, training=True)  # Predict using CCA
    Y_pred_PLSR_Canonical = PLSR_Canonical(X=X_Training, Y=Y_Training, X_test=X_test, n_components=15, training=True)  # Predict using PLS Canonical
    Y_pred_PLSR_PCA = PLSR_PCA(X=X_Training, Y=Y_Training, X_test=X_test, training=True)  # Predict using PCA combined with Random Forest

    # Calculate and print mean squared errors for different models
    msePLSR = mean_squared_error(Y_true, Y_pred_PLSR)  # Calculate MSE for PLSR
    print(msePLSR)  # Print MSE for PLSR
    mseCCA = mean_squared_error(Y_true, Y_pred_PLSR_CCA)  # Calculate MSE for CCA
    print(mseCCA)  # Print MSE for CCA
    mseCanonical = mean_squared_error(Y_true, Y_pred_PLSR_Canonical)  # Calculate MSE for PLS Canonical
    print(mseCanonical)  # Print MSE for PLS Canonical
    # mseLDA and PLSR_kernel are commented out, indicating they were considered but not implemented or used

    # Save the PLSR predictions to a new CSV file
    newfilename = 'concatenated_bs2/' + "concatenated_predicted_carfentanil_bs2_PLSR"  # Define the filename for the output
    with open(newfilename, 'w') as csvfile2:  # Open a new CSV file to write the predictions
        np.savetxt(csvfile2, Y_pred_PLSR, fmt='%.2e', delimiter=',')  # Save the predictions in scientific notation with a comma delimiter




