import numpy as np  #  
import csv  #  
import os  #  
from datetime import datetime  #  
import glob  #  
import matplotlib.pyplot as plt  #  
import pandas as pd  #  
import os.path  # 
from sklearn.metrics import mean_squared_error  #  

naloxone_Dose = 1  # Set naloxone dose to 1


# Caution: The provided data encompasses all 540 scenarios. Therefore, the results in panels A and B may not accurately reflect specific configurations unless the data is filtered accordingly. 
# Consider separating and uploading data specific to each of the 12 opioid doses, 5 potential naloxone doses, and 3 possible delays to ensure accurate analysis and representation.


# Define file paths for semi-mechanical, PLSR, traditional, and true data
File_semiMechanical = 'concatenated_bs2/concatenated_predicted_carfentanil_bs2_AI_semiMechanical'
File_name_PLSR = 'concatenated_bs2/concatenated_predicted_carfentanil_bs2_PLSR'
File_name_traditional = 'concatenated_bs2/concatenated_predicted_carfentanil2_bs2_Traditional'
File_name_true = 'concatenated_bs2/concatenated_carfentanil_bs2'

if (True):  # Start of a block that always executes               
    with open(File_name_true) as csvfile1:  # Open the true data file
        mpg = list(csv.reader(csvfile1, quoting=csv.QUOTE_NONNUMERIC))  # Read the CSV file as numeric values
        vali = np.array(mpg).astype("float")  # Convert the read data to a NumPy array of type float

    with open(File_name_PLSR) as csvfile1:  # Open the PLSR data file
        mpg = list(csv.reader(csvfile1, quoting=csv.QUOTE_NONNUMERIC))  # Read the CSV file as numeric values
        predPLSR = np.array(mpg).astype("float")  # Convert the read data to a NumPy array of type float

    with open(File_semiMechanical) as csvfile1:  # Open the semi-mechanical data file
        mpg = list(csv.reader(csvfile1, quoting=csv.QUOTE_NONNUMERIC))  # Read the CSV file as numeric values
        predSemi = np.array(mpg).astype("float")  # Convert the read data to a NumPy array of type float

    with open(File_name_traditional) as csvfile1:  # Open the traditional data file
        mpg = list(csv.reader(csvfile1, quoting=csv.QUOTE_NONNUMERIC))  # Read the CSV file as numeric values
        predTrad = np.array(mpg).astype("float")  # Convert the read data to a NumPy array of type float

batch_Size = 256  # Set batch size to 256
available_data = int(np.floor(len(vali) / batch_Size) * batch_Size)  # Calculate the number of data points that fit into complete batches
vali = vali[0:available_data, :]  # Trim the validation data to only include complete batches

def Bar():
    # Define a function to calculate and return bar chart data
    
    idx = vali[:, 12] != 10  # Create an index for filtering data where the 13th column is not equal to 10
    vali2 = vali[idx, :]  # Apply the filter to the validation data
    predPLSR2 = predPLSR[idx, :]  # Apply the filter to the PLSR predictions
    predSemi2 = predSemi[idx, :]  # Apply the filter to the semi-mechanical predictions
    predTrad2 = predTrad[idx, :]  # Apply the filter to the traditional predictions

    Mean2 = np.median(vali2[:, 15:], 0)  # Calculate the median of the validation data from the 16th column onwards
    Mean3 = np.median(predSemi2, 0)  # Calculate the median of the semi-mechanical predictions
    Mean3J = np.median(predTrad2, 0)  # Calculate the median of the traditional predictions
    Mean3T = np.median(predPLSR2, 0)  # Calculate the median of the PLSR predictions

    MSE_Semi = mean_squared_error(Mean2, Mean3, multioutput='raw_values', squared=False)  # Calculate RMSE for semi-mechanical predictions
    MSE_trad = mean_squared_error(Mean2, Mean3J, multioutput='raw_values', squared=False)  # Calculate RMSE for traditional predictions
    MSE_PLSR = mean_squared_error(Mean2, Mean3T, multioutput='raw_values', squared=False)  # Calculate RMSE for PLSR predictions
    MSE = [MSE_Semi[0], MSE_trad[0], MSE_PLSR[0]]  # Compile RMSE values into a list

    a1 = Mean2 - Mean3  # Calculate the difference between median validation data and semi-mechanical predictions
    a2 = Mean2 - Mean3J  # Calculate the difference between median validation data and traditional predictions
    a3 = Mean2 - Mean3T  # Calculate the difference between median validation data and PLSR predictions
    RMSE_Median = np.sqrt(np.dot(a1, a1) / (np.size(Mean2)))  # Calculate RMSE for the median values of semi-mechanical predictions
    RMSEJ_Median = np.sqrt(np.dot(a2, a2) / (np.size(Mean2)))  # Calculate RMSE for the median values of traditional predictions
    RMSET_Median = np.sqrt(np.dot(a3, a3) / (np.size(Mean2)))  # Calculate RMSE for the median values of PLSR predictions
    MSE2 = [RMSE_Median, RMSEJ_Median, RMSET_Median]  # Compile the median RMSE values into a list

    Mean2 = np.percentile(vali2[:, 15:], [2.5, 97.5], axis=0)  # Calculate the 2.5th and 97.5th percentiles of the validation data from the 16th column onwards
    Mean3 = np.percentile(predSemi2, [2.5, 97.5], axis=0)  # Calculate the 2.5th and 97.5th percentiles of the semi-mechanical predictions
    Mean3J = np.percentile(predTrad2, [2.5, 97.5], axis=0)  # Calculate the 2.5th and 97.5th percentiles of the traditional predictions
    Mean3T = np.percentile(predPLSR2, [2.5, 97.5], axis=0)  # Calculate the 2.5th and 97.5th percentiles of the PLSR predictions

    RMSE_upper = np.sqrt(np.dot(a1, a1) / (np.size(Mean2)))  # Recalculate RMSE for the upper percentile values (seems to be a repeated step)
    RMSEJ_upper = np.sqrt(np.dot(a2, a2) / (np.size(Mean2)))  # Recalculate RMSE for the upper percentile values of traditional predictions (repeated step)
    RMSET_upper = np.sqrt(np.dot(a3, a3) / (np.size(Mean2)))  # Recalculate RMSE for the upper percentile values of PLSR predictions (repeated step)
    MSE2 = [RMSE_Median, RMSEJ_Median, RMSET_Median]  # Reassign the median RMSE values to the list (seems to be a repeated step)

    a1 = Mean2[0, :] - Mean3[0, :]  # Calculate the difference between the lower percentile of validation data and semi-mechanical predictions
    a2 = Mean2[0, :] - Mean3J[0, :]  # Calculate the difference between the lower percentile of validation data and traditional predictions
    a3 = Mean2[0, :] - Mean3T[0, :]  # Calculate the difference between the lower percentile of validation data and PLSR predictions
    RMSE_Lower = np.sqrt(np.dot(a1, a1) / (np.size(Mean2[0, :])))  # Calculate RMSE for the lower percentile of semi-mechanical predictions
    RMSEJ_Lower = np.sqrt(np.dot(a2, a2) / (np.size(Mean2[0, :])))  # Calculate RMSE for the lower percentile of traditional predictions
    RMSET_Lower = np.sqrt(np.dot(a3, a3) / (np.size(Mean2[0, :])))  # Calculate RMSE for the lower percentile of PLSR predictions

    a1 = Mean2[1, :] - Mean3[1, :]  # Calculate the difference between the upper percentile of validation data and semi-mechanical predictions
    a2 = Mean2[1, :] - Mean3J[1, :]  # Calculate the difference between the upper percentile of validation data and traditional predictions
    a3 = Mean2[1, :] - Mean3T[1, :]  # Calculate the difference between the upper percentile of validation data and PLSR predictions
    RMSE_Upper = np.sqrt(np.dot(a1, a1) / (np.size(Mean2[1, :])))  # Calculate RMSE for the upper percentile of semi-mechanical predictions
    RMSEJ_Upper = np.sqrt(np.dot(a2, a2) / (np.size(Mean2[1, :])))  # Calculate RMSE for the upper percentile of traditional predictions
    RMSET_Upper = np.sqrt(np.dot(a3, a3) / (np.size(Mean2[1, :])))  # Calculate RMSE for the upper percentile of PLSR predictions

    Semi = [RMSE_Lower, RMSE_Median, RMSE_Upper]  # Compile RMSE values for semi-mechanical predictions into a list
    Trad = [RMSEJ_Lower, RMSEJ_Median, RMSEJ_Upper]  # Compile RMSE values for traditional predictions into a list
    PLSR = [RMSET_Lower, RMSET_Median, RMSET_Upper]  # Compile RMSE values for PLSR predictions into a list

    TMP = (max(Semi), max(Trad), max(PLSR))  # Create a tuple of the maximum RMSE values from each method
    Ylim = max(TMP) + np.std(TMP) / 3  # Calculate the Y-axis limit for the plot
    
    return list([Semi, Trad, PLSR, Ylim])  # Return a list containing the RMSE values and Y-axis limit

def Vent(TRUE, PRED):
    # Define a function to calculate and return data for ventilation analysis
    
    idx = TRUE[:, 12] == naloxone_Dose  # Create an index for filtering data where the 13th column equals the naloxone dose
    TRUEtmp = 1 - TRUE[idx, 15:]  # Calculate the fractional minute ventilation for the true data
    PREDtmp = 1 - PRED[idx, :]  # Calculate the fractional minute ventilation for the predictions

    medianTrue = np.median(TRUEtmp, 0)  # Calculate the median of the true fractional minute ventilation
    medianPred = np.median(PREDtmp, 0)  # Calculate the median of the predicted fractional minute ventilation

    percentile_TRUE = np.percentile(TRUEtmp, [5, 95], axis=0)  # Calculate the 5th and 95th percentiles of the true fractional minute ventilation
    percentile_Pred = np.percentile(PREDtmp, [5, 95], axis=0)  # Calculate the 5th and 95th percentiles of the predicted fractional minute ventilation

    return list([medianTrue, percentile_TRUE, medianPred, percentile_Pred])  # Return a list containing the median and percentile data








if __name__ == "__main__":
    
    t1 = np.array(range(0, 301, 1))  # Create an array of time points from 0 to 300 with a step of 1
    t2 = np.array(range(305, 3605, 5))  # Create an array of time points from 305 to 3600 with a step of 5
    time = np.concatenate((t1, t2), axis=0)  # Concatenate the two arrays of time points
       
    plt.clf()  # Clear the current figure
    fig, axes = plt.subplots(2, 2, figsize=(10, 8))  # Create a new figure and a 2x2 grid of subplots
    
    # Plot Precision for semi-mechanical model
    medianTrue, percentile_TRUE, medianPred, percentile_Pred = Vent(vali, predSemi)  # Get data for ventilation analysis using semi-mechanical predictions
    
    axes[0, 0].plot(time, medianTrue, linewidth='2.0', ls='-', color='black')  # Plot the median true fractional minute ventilation
    axes[0, 0].plot(time, medianPred, linewidth='2.0', ls='-', color='blue')  # Plot the median predicted fractional minute ventilation
    
    axes[0, 0].plot(time, percentile_TRUE[0, :], linewidth='2.0', ls='--', color='black')  # Plot the lower percentile of true fractional minute ventilation
    axes[0, 0].plot(time, percentile_Pred[0, :], linewidth='2.0', ls='--', color='blue')  # Plot the lower percentile of predicted fractional minute ventilation
    
    axes[0, 0].plot(time, percentile_TRUE[1, :], linewidth='2.0', ls='--', color='black')  # Plot the upper percentile of true fractional minute ventilation
    axes[0, 0].plot(time, percentile_Pred[1, :], linewidth='2.0', ls='--', color='blue')  # Plot the upper percentile of predicted fractional minute ventilation
    
    axes[0, 0].set_ylabel('Fractional Minute Ventilation', fontsize=16)  # Set y-axis label
    axes[0, 0].set_xlabel('Time (s)', fontsize=16)  # Set x-axis label
    axes[0, 0].tick_params(axis='x', labelsize=16)  # Set x-axis tick parameters
    axes[0, 0].tick_params(axis='y', labelsize=16)  # Set y-axis tick parameters
    axes[0, 0].set_ylim(-0.05, 1.05)  # Set y-axis limits
    
    # Plot Precision for traditional model
    medianTrue, percentile_TRUE, medianPred, percentile_Pred = Vent(vali, predTrad)  # Get data for ventilation analysis using traditional predictions
    
    axes[0, 1].plot(time, medianTrue, linewidth='2.0', ls='-', color='black')  # Plot the median true fractional minute ventilation
    axes[0, 1].plot(time, medianPred, linewidth='2.0', ls='-', color='red')  # Plot the median predicted fractional minute ventilation
    
    axes[0, 1].plot(time, percentile_TRUE[0, :], linewidth='2.0', ls='--', color='black')  # Plot the lower percentile of true fractional minute ventilation
    axes[0, 1].plot(time, percentile_Pred[0, :], linewidth='2.0', ls='--', color='red')  # Plot the lower percentile of predicted fractional minute ventilation
    
    axes[0, 1].plot(time, percentile_TRUE[1, :], linewidth='2.0', ls='--', color='black')  # Plot the upper percentile of true fractional minute ventilation
    axes[0, 1].plot(time, percentile_Pred[1, :], linewidth='2.0', ls='--', color='red')  # Plot the upper percentile of predicted fractional minute ventilation
    
    axes[0, 1].set_ylabel('Fractional Minute Ventilation', fontsize=16)  # Set y-axis label
    axes[0, 1].set_xlabel('Time (s)', fontsize=16)  # Set x-axis label
    axes[0, 1].tick_params(axis='x', labelsize=16)  # Set x-axis tick parameters
    axes[0, 1].tick_params(axis='y', labelsize=16)  # Set y-axis tick parameters
    axes[0, 0].set_ylim(-0.05, 1.05)  # Ensure y-axis limits are consistent across plots (possibly a typo, should be axes[0, 1])
    
    # Plot Precision for PLSR model
    medianTrue, percentile_TRUE, medianPred, percentile_Pred = Vent(vali, predPLSR)  # Get data for ventilation analysis using PLSR predictions
    
    axes[1, 0].plot(time, medianTrue, linewidth='2.0', ls='-', color='black')  # Plot the median true fractional minute ventilation
    axes[1, 0].plot(time, medianPred, linewidth='2.0', ls='-', color='orange')  # Plot the median predicted fractional minute ventilation
    
    axes[1, 0].plot(time, percentile_TRUE[0, :], linewidth='2.0', ls='--', color='black')  # Plot the lower percentile of true fractional minute ventilation
    axes[1, 0].plot(time, percentile_Pred[0, :], linewidth='2.0', ls='--', color='orange')  # Plot the lower percentile of predicted fractional minute ventilation
    
    axes[1, 0].plot(time, percentile_TRUE[1, :], linewidth='2.0', ls='--', color='black')  # Plot the upper percentile of true fractional minute ventilation
    axes[1, 0].plot(time, percentile_Pred[1, :], linewidth='2.0', ls='--', color='orange')  # Plot the upper percentile of predicted fractional minute ventilation
    
    axes[1, 0].set_ylabel('Fractional Minute Ventilation', fontsize=16)  # Set y-axis label
    axes[1, 0].set_xlabel('Time (s)', fontsize=16)  # Set x-axis label
    axes[1, 0].tick_params(axis='x', labelsize=16)  # Set x-axis tick parameters
    axes[1, 0].tick_params(axis='y', labelsize=16)  # Set y-axis tick parameters
    axes[1, 0].set_ylim(-0.15, 1.05)  # Set y-axis limits
    
    # Plot RMSE bar chart
    barWidth = 0.3  # Define the width of the bars in the bar chart
    
    Semi, Trad, PLSR, Ylim = Bar()  # Get the RMSE data for semi-mechanical, traditional, and PLSR models, and the y-axis limit
    br1 = np.arange(len(Semi))  # Define the bar positions for the semi-mechanical model
    br2 = [x + barWidth for x in br1]  # Define the bar positions for the traditional model
    br3 = [y + barWidth * 2 for y in br1]  # Define the bar positions for the PLSR model
    
    axes[1, 1].bar(br1, Semi, width=barWidth, label='Semi-Mechanistic', capsize=4, color='blue')  # Plot bars for the semi-mechanical model
    axes[1, 1].bar(br2, Trad, width=barWidth, label='Black Box', capsize=4, color='red')  # Plot bars for the traditional model
    axes[1, 1].bar(br3, PLSR, width=barWidth, label='PLSR', capsize=4, color='orange')  # Plot bars for the PLSR model
    plt.xticks([r + barWidth / 2 for r in range(len(Semi))], ['2.5%', 'Median', '97.5%'])  # Set x-tick labels
    axes[1, 1].set_ylabel('RMSE', fontsize=16)  # Set y-axis label
    axes[1, 1].set_xlabel('Percentile', fontsize=16)  # Set x-axis label
    axes[1, 1].tick_params(axis='x', labelsize=16)  # Set x-axis tick parameters
    axes[1, 1].tick_params(axis='y', labelsize=16)  # Set y-axis tick parameters
    axes[1, 1].set_ylim(0, Ylim)  # Set y-axis limits
    
    plt.tight_layout()  # Adjust spacing between subplots for clarity
    
    plt.savefig('4_panel_plot.pdf', format='pdf')  # Save the plot to a PDF file
    plt.show()  # Display the plot
