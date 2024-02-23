# Table 5
library(modelsummary)
library(kableExtra)

p25 <- quantile(ap_data$tot_route, 0.25)
p75 <- quantile(ap_data$tot_route, 0.75)

# Generate Categorical variable
ap_data$category <- ifelse(ap_data$tot_route < p25, "Low",
                           ifelse(ap_data$tot_route < p75 & ap_data$tot_route > p25, "Medium", "High"))

# Observation Row
a <- length(ap_data$hhi[ap_data$tot_route > p75])
b <- length(ap_data$hhi[ap_data$tot_route < p25])
c <- length(ap_data$hhi) - a - b

# Order
custom_order <- c("Low", "Medium", "High")
ap_data$category <- factor(ap_data$category, levels = custom_order)

new_rows <- data.frame('observations',' ', b,' ',c,' ',a)

# Datasummary
datasummary(Heading("HHI")*hhi + Heading("N")*nc ~ category * (Mean*Arguments("%.3f") + SD *Arguments("%.3f")), 
            title = 'DISTRIBUTION OF CONCENTRATION BY TRAFFIC VOLUME',
            fmt=0, data = ap_data, add_rows = new_rows
                    )%>%
  footnote("N represents carrier counts. Standard deviations are in parentheses", threeparttable=T)

