text_tbl <- data.frame(
  Variable = c("lasset", "lasset$^{2}$", "cash", "opexp", "otherinc", "bankr"),
  Definition = c("Logged total assets", "Lasset squared", "Cash available", "Operating expenses", "Nonoperating income", "Bankruptcy indicator"),
  Mean = c(mean(data_ap$lasset), mean(data_ap$lass_sq), mean(data_ap$cash), mean(data_ap$op_expenses), mean(data_ap$non_op_income), mean(data_ap$bankr)),
  SD = c(sd(data_ap$lasset), sd(data_ap$lass_sq), sd(data_ap$cash), sd(data_ap$op_expenses), sd(data_ap$non_op_income), sd(data_ap$bankr))
)

kbl(text_tbl, booktabs = T, 'latex') %>%
  kable_styling(full_width = )