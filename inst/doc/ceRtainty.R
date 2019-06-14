## ------------------------------------------------------------------------
library(ceRtainty)
data("profitSWG")
#
# Computing the CE values, for a RAC range of 0.5-4.0, and Power utility function.
#

# Obtaining the CE table
certainty(data = profitSWG, ival = 0.5, fval = 4, utility = "Power")$CE_values
# Obtaining the RAC vector
certainty(data=profitSWG,ival=.5,fval=4,utility="Power")$RAC
# Performing the CE plot
certainty(data=profitSWG,ival=.5,fval=4,utility="Power")$CE_plot()


## ------------------------------------------------------------------------
library(ceRtainty)
data("profitSWG")
#
# Computing and storing the CE values using Power utility function
# 
ces <- certainty(data = profitSWG, ival = 0.5, fval = 4, utility = "Power")

ces_values  <- ces$CE_values  # store CE table
ces_rac     <- ces$RAC        # store RAC vector

# Computing the RP values respect to SERENADE treatment
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$PremiumRisk

# Computing the RP values in percentage respect to SERENADE treatment
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$PremiumRiskPer100

# Plotting the RP absolute values
premium(tbase = "serenade",ce_data = ces_values, rac = ces_rac, utility = "Power")$RP_plot()


## ------------------------------------------------------------------------
library(ceRtainty)
data("profitSWG")

rac_generator(data = profitSWG$control, ini = 0.5, fin = 4.0)

