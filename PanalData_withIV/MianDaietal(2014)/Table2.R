text_tbl <- data.frame(
  Variable = c("lasset", "lasset$^{2}$", "cash", "opexp", "otherinc", "bankr"),
  Definition = c("Logged total assets", "Lasset squared", "Cash available", "Operating expenses", "Nonoperating income", "Bankruptcy indicator"),
  Mean = c(round(mean(data_ap$lasset), digits=3), round(mean(data_ap$lass_sq), digits=3), 
           round(mean(data_ap$cash), digits=3), round(mean(data_ap$op_expenses), digits=3), 
           round(mean(data_ap$non_op_income), digits=3), round(mean(data_ap$bankr), digits=3)),
  SD = c(round(sd(data_ap$lasset), digits=3), round(sd(data_ap$lass_sq), digits=3), 
         round(sd(data_ap$cash), digits=3), round(sd(data_ap$op_expenses), digits=3), 
         round(sd(data_ap$non_op_income), digits=3), round(sd(data_ap$bankr), digits=3))
)

kbl(text_tbl,align = "c", caption = "CONTROL VARIABLES" , booktabs = T, 'latex') %>%
  kable_styling(full_width = ) %>%
  footnote(general = "Number of observations is 248,513. cash, opexp, and otherinc are computed as percentage of total assets", threeparttable = T)