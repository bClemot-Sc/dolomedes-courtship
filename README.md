# Impact of Female Mating Status on Male Courtship Behaviour in the Sexually Cannibalistic New Zealand Fishing Spider *Dolomedes minor* (Araneae, Dolomedidae)

# Data Analysis (R Version 4.3.3)

All the statistical analysis performed in our study can be reproduced using the R scripts and data present in this repository. Please find in this document the indication on which script to use for every analysis, sorted in their order of appearance in the Results section of our paper.

## Description of Male Courtship Behaviour

The statistics (Mean and SE) used for the description of the Courtship Behaviours in the unmated group can be computed using the **scoring_analysis.R** script. The script will compute the following elements for both groups:

- The mean and SE of the time proportion of each combined and global behaviour, which can be found in the *final_results* object.
- The mean and SE of the occurrence of each discrete behaviours, which can be found in the *discrete_results* object.

## Influence of Female Mating Status on Male Courtship Timing Components

The complete analysis of the four timing components that are, **i)** the Latency to Court, the **ii)** the Approach Duration, **iii)** the Latency to Mount and **iv)** the Mounting Duration, can be performed using the **timing_components.R** script. 

This includes in order:
- The assessment of data normality for all four data, including Q-Q plots, histograms and Shapiro tests.
- The published Wilcoxon Signed-Rank Test for paired data, as well as additional non-paired tests and paired tests with manually removed incomplete data for sanity check (unpublished).
- The published graphical representation of all four data with the data points removed for pairwise comparison being represented in red and excluded from the Violin plots distribution.
- The same graphical representation but with the removed data points being part of the Violin plots distribution (unpublished).

## Influence of Female Mating Status on Male Courtship Structure

### **Construction of Courtship Behavioural Network (iGraph version 2.1.4)**

The reconstruction of the courtship behavioural networks for the two groups can be performed using the **network_construction.R** script. This script follows the method detailed in our Statical analysis section to get the adjacency matrix of significant behavioural transitions between behaviours, and then use the iGraph R package to plot the obtained network. 

The most relevant information from this script are:
- The observed matrix of the **number of behavioural transition** for both groups, respectively named *adjacency.matrix1* and *adjacency.matrix2* for the unmated and mated groups.
- The observed matrix of **behavioural transition probabilities** for both groups, respectively named *trans.prob1* and *trans.prob2* for the unmated and mated groups.
- The matrix of **significant behavioural transition probabilities** for both groups, respectively named *keep1* and *keep2* for the unmated and mated groups.
- The **networks of significant behavioural transition** plotted for both groups at the end of the script and respectively named *plot1* and *plot2* for the unmated and mated groups.

---

### **Statistical Comparison of Courtship Structures**

The whole statistical comparison of the courtship structure between the two groups is separated into two scripts for convenience. 

First, the Paired Wilcoxon tests for the time proportion of each courtship elements, as well as for the number of occurrences for the discrete behaviour are both found in the **scoring_analysis.R** script. They can respectively be found in the *final_results* and *discrete_results* objects for the non-discrete and discrete behaviours.

Secondly, the statistical tests for the comparison of the behavioural transition probability between the two groups can be performed using the **adjacency_analysis.R** script. This script will produce a dataframe object called *test_results* with for all the possible behavioural transitions, the p-values obtained from Paired Wilcoxon tests and the corrected p-values using the False Discovery rate correction.

## Please, feel free to let us know about any questions or points of discussion!
# Thank you.