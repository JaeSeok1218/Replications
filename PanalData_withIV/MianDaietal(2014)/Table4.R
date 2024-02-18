# Table 4
# IV Fixed Effects
library(dplyr)
library(fixest)
library(lmtest)
library(modelsummary)
library(kableExtra)

# Setting as Panel data
ap_panel <- pdata.frame(ap_data, index = c("rcid", "date")) # id = rcid, t = date
ct_panel <- pdata.frame(ct_data, index = c("rcid", "date")) # id = rcid, t = date

# Filter Singleton values
filtered_data <- ap_panel %>%
  group_by(rcid) %>%
  filter(n() > 1) %>%
  ungroup()

singleton_groups <- ave(rep(1, nrow(ap_panel)), ap_panel$rcid, FUN = length) == 1
filtered_data <- ap_panel[!singleton_groups, ]

filtered_data_ct <- ct_panel %>%
  group_by(rcid) %>%
  filter(n() > 1) %>%
  ungroup()

singleton_groups <- ave(rep(1, nrow(ct_panel)), ct_panel$rcid, FUN = length) == 1
filtered_data_ct <- ct_panel[!singleton_groups, ]

# Run the bunch of regressions and Generate Table
model <- list(
  "log(P90)" = feols(lp90 ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | duo + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data, panel.id = ~ rcid + date),
  "log(P10)" = feols(lp10 ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | duo + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data, panel.id = ~ rcid + date),
  "log(P90)" = feols(lp90 ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | duo + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date),
  "log(P10)" = feols(lp10 ~ lasset + lass_sq + cash + op_expenses + non_op_income + bankr + factor(date) | rcid | duo + comp ~ lamean_msa + lamean_msa_sq + lgmean_msa + lgmean_msa_sq + ltot_route + ltot_route_sq + genp + genp_sq, 
                filtered_data_ct, panel.id = ~ rcid + date)
)

cm <- c('fit_duo'    = "duo",
        'fit_comp'    = "comp")

modelsummary(model, vcov=~rid, coef_map = cm, gof_map = c("nobs"), stars=TRUE, fmt=3, title = "EFFECT OF COMPETITION ON PRICE LEVEL", output = "latex")%>%
  add_header_above(c("", "A. AirportPairs" = 2, "B. City Pairs" = 2))