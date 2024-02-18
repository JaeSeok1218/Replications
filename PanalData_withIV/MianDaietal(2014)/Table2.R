text_tbl <- data.frame(
  Variable = c("lasset", "lasset$^{2}$", "cash", "opexp", "otherinc", "bankr"),
  Definition = c("Logged total assets", "Lasset squared", "Cash available", "Operating expenses", "Nonoperating income", "Bankruptcy indicator"),
  Mean = c(round(mean(ap_data$lasset), digits=3), round(mean(ap_data$lass_sq), digits=3), 
           round(mean(ap_data$cash, na.rm=TRUE), digits=3), round(mean(ap_data$op_expenses), digits=3), 
           round(mean(ap_data$non_op_income), digits=3), round(mean(ap_data$bankr), digits=3)),
  SD = c(round(sd(ap_data$lasset), digits=3), round(sd(ap_data$lass_sq), digits=3), 
         round(sd(ap_data$cash, na.rm=TRUE), digits=3), round(sd(ap_data$op_expenses), digits=3), 
         round(sd(ap_data$non_op_income), digits=3), round(sd(ap_data$bankr), digits=3))
)

kbl(text_tbl,align = "c", caption = "CONTROL VARIABLES" , booktabs = T, 'latex') %>%
  kable_styling(full_width = ) %>%
  footnote(general = "Number of observations is 248,513. cash, opexp, and otherinc are computed as percentage of total assets", threeparttable = T)