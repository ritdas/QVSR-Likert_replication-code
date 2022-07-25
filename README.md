# QVSR-Likert_replication-code
 Replication code for QVSR-Likert analysis
The code analyzes the QVSR-Likert data in 3 steps:-
Step 1. Cleans the data and horseraces different ML models to estimate the mean squared errors.
Step 2. The minimum mean squared error is used as the outcome variable (with negative coefficient) for the policy tree algorithm to assign the best treatment arm resulting in the minimum Brier Score.
Step3. The treatment asignment from step 2 is analyzed for heterogeneity with educational attainment as the variable of interest.

We have considered voter turnout, letter writing and donation variables as our outcome variables. Voter turnout is binary. Letter writing has two policy issues- writing for abortion and writing for minimum wages, we have considered both the binary version (whether or not written) and the categorical version (how many characters written). Similarly theere is donation for two political issues- donation to immigration related charities (wall donation) and donation for gun control, we have again considered the binary version (whether or not donated) and the categorical version (amount donated). For the categorical version, we have also considered the absolute amount donated as well as direction fo donation (negative for conservative charities and positive for liberal charities).
