# Table 3 
# IV Fixed Effects
library(dplyr)
library(fixest)
library(lmtest)
library(modelsummary)
library(kableExtra)
library(haven)

setwd("/Users/jaeseokoh/Library/Mobile Documents/com~apple~CloudDocs/Statistics Tools(Lectures)/Stata/Fall2023 IO/HW4")
# Read data as '.csv'
data <- read_dta("ap_pair_rep.dta")
write.csv(data, "ap_pair.csv", row.names = FALSE)
ap_data1 <- read.csv("ap_pair_rep.csv")

data <- read_dta("city_pair_rep.dta")
write.csv(data, "ct_pair.csv", row.names = FALSE)
ct_data <- read.csv("ct_pair.csv")
# Setting as Panel data
ap_panel <- pdata.frame(ap_data, index = c("rcid", "date")) # id = rcid, t = date
time_dummies <- factor(ap_panel$date) # generate time dummies

ct_panel <- pdata.frame(ct_data, index = c("rcid", "date")) # id = rcid, t = date
time_dummies <- factor(ct_panel$date) # generate time dummies

# Filter Singleton values
filtered_data <- ap_panel %>%
  group_by(rcid) %>%
  filter(n() > 1) %>%
  ungroup()

singleton_groups <- ave(rep(1, nrow(ap_panel)), ap_panel$rcid, FUN = length) == 1
filtered_data_ap <- ap_panel[!singleton_groups, ]

filtered_data_ct <- ct_panel %>%
  group_by(rcid) %>%
  filter(n() > 1) %>%
  ungroup()

singleton_groups <- ave(rep(1, nrow(ct_panel)), ct_panel$rcid, FUN = length) == 1
filtered_data_ct <- ct_panel[!singleton_groups, ]

# Run the bunch of regressions and Generate Table
model <- list(
  "(1)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | hhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ap, panel.id = ~ rcid + date),
  "(2)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | hhi + hhi2 ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ap, panel.id = ~ rcid + date),
  "(3)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | mono + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ap, panel.id = ~ rcid + date),
  "(4)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | nc ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ap, panel.id = ~ rcid + date),
  "(5)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | hhi ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date),
  "(6)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | hhi + hhi2 ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date),
  "(7)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | mono + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date),
  "(8)" = feols(fgini ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | nc ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date)
)

cm <- c('fit_hhi'    = "HHI",
        'fit_hhi2'    = "HHI2",
        'fit_mono' = 'mono',
        'fit_comp' = 'comp',
        'fit_nc' = 'N')

modelsummary(model, vcov=~rid, coef_map = cm, gof_map = c("nobs"), stars=TRUE, fmt=3, title = "NONMONOTONIC EFFECT OF COMPETITION", output = "latex")%>%
    add_header_above(c(" ", "A. AirportPairs" = 4, "B. City Pairs" = 4))%>%
    footnote("The dependent variable is Gini, and the hat represents the instrumented endogenous variable. Carrier-route and year-quarter fixed effects are included in all specifications. Robust standard erros in parentheses are adjusted for correlation within market. Control variables are log(asset), log2(asset), cash (as % of asset), operating cost (as % of asset), and nonoperating net income (as % of asset), a dummy indicator if the carrier is under bankruptcy protextion. The list of instruments can be found in section A3."
           , threeparttable = T)


summary(model_iv_fe, cluster = "rid")