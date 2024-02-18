# Table 5

# Generate Categorical variable
ap_data$category <- ifelse(ap_data$tot_route < p25, "Low",
                           ifelse(ap_data$tot_route < p75 & ap_data$tot_route > p25, "Medium", "High"))

# Observation Row
a <- length(ap_data$hhi[ap_data$tot_route > p75])
b <- length(ap_data$hhi[ap_data$tot_route < p25])
c <- length(ap_data$hhi) - a - b

new_rows <- data.frame('observations',' ', b,' ',c,' ',a)

# Datasummary
datasummary(Heading("HHI")*hhi + Heading("N")*nc ~ category * (Mean*Arguments("%.3f") + SD *Arguments("%.3f")), 
            title = 'DISTRIBUTION OF CONCENTRATION BY TRAFFIC VOLUME',
            fmt=0, data = ap_data, add_rows = new_rows, output = "latex"
                    )%>%
  footnote("N represents carrier counts. Standard deviations are in parentheses", threeparttable=T)

