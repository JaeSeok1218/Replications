library(haven) # Read .dta file
library(dplyr)
library(plm)
library(fixest)
library(modelsummary)
library(kableExtra)


setwd("/Users/jaeseokoh/Library/Mobile Documents/com~apple~CloudDocs/Statistics Tools(Lectures)/Stata/Fall2023 IO/HW4")
# Read data as '.csv'
data <- read_dta("ap_pair_rep.dta")
write.csv(data, "ap_pair.csv", row.names = FALSE)
ap_data <- read.csv("ap_pair_rep.csv")

data <- read_dta("city_pair_rep.dta")
write.csv(data, "ct_pair.csv", row.names = FALSE)
ct_data <- read.csv("ct_pair.csv")
# Setting as Panel data
ap_panel <- pdata.frame(ap_data, index = c("rcid", "date")) # id = rcid, t = date
time_dummies <- factor(ap_panel$date) # generate time dummies

ct_panel <- pdata.frame(ct_data, index = c("rcid", "date")) # id = rcid, t = date
time_dummies <- factor(ct_panel$date) # generate time dummies
# Get 75 and 25 quantile
ap_p25 <- quantile(ap_data$tot_route, 0.25)
ap_p75 <- quantile(ap_data$tot_route, 0.75)

ct_p25 <- quantile(ct_data$tot_route, 0.25)
ct_p75 <- quantile(ct_data$tot_route, 0.75)

# Run the bunch of regressions and Generate Table
model <- list(
  "Low" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ap, panel.id = ~ rcid + date, subset = filtered_data_ap$tot_route <= ap_p25),
  "Medium" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                   filtered_data_ap, panel.id = ~ rcid + date, subset = filtered_data_ap$tot_route<ap_p75 & filtered_data_ap$tot_route>ap_p25),
  "High" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                 filtered_data_ap, panel.id = ~ rcid + date, subset = filtered_data_ap$tot_route>=ap_p75),
  "Low" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date, subset = filtered_data_ct$tot_route<=ct_p25),
  "Medium" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                   filtered_data_ct, panel.id = ~ rcid + date, subset = filtered_data_ct$tot_route<ct_p75 & filtered_data_ct$tot_route>ct_p25),
  "High" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | lhhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                 filtered_data_ct, panel.id = ~ rcid + date, subset = filtered_data_ct$tot_route>=ct_p75)
)

cm <- c('fit_lhhi' = "log(HHI)")

modelsummary(model, vcov=~rid, coef_map = cm, gof_map = c("nobs"), stars=TRUE, fmt=3, title = "HETEROGENEOUS EFFECT OF COMPETITION", output = 'latex')%>%
  add_header_above(c(" ", "A. AirportPairs" = 3, "B. City Pairs" = 3))%>%
  footnote("The hat represents the instrumented endogenous variable. Carrier-route and year-quarter fixed effects are included in all specifications. Robust standard errors in parentheses are adjusted for correlation within market. control variables are log(asset), log2(asset), cash (as % of asset), operating cost (as % of asset), and nonoperating net income (as % of asset) a dummy indicator if the carrier is under bankruptcy protection. The list of instruments can be found in section A3."
           , threeparttable = T)