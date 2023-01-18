setwd("E:/Kuliah/SMT 7/Regional/Tugas IO")
library(tidyverse)
library(readxl)
library(ggplot2)
library(writexl)
pale
RColorBrewer::
Z_sumut <- read_xls("sumut IO.xls", range = "D4:U21")
sektor <- colnames(Z_sumut)[1:17]
F_sumut <- read_xls("sumut IO.xls", range = "V4:AD21")
V_sumut <- read_xls("sumut IO.xls", range = "C22:T26", col_names = F)


col_V_sumut <- V_sumut$...1
V_sumut <- V_sumut %>% select(-...1) %>% t()
V_sumut %>% rowSums() /2
colnames(V_sumut) <- col_V_sumut

output_sumut <- read_xls("sumut IO.xls", range = "AE4:AE21")

A_sumut <- as.matrix(select(Z_sumut, - `1800`)) %*% diag(1/output_sumut$`3100`)
L_sumut <- diag(rep(1,17),17,17) - A_sumut
IL_sumut <- solve(diag(rep(1,17),17,17) - A_sumut)



################################ BL
colSums(IL_sumut)
domestic_v <- V_sumut[,5]

######## Value Added Multplier
V_Value <- diag(domestic_v/(output_sumut$`3100`),17,17)
V_mult <-  V_Value %*% IL_sumut
V_mult_1 <- V_Value %*% IL_sumut %*% solve(V_Value)
colSums(V_mult)
########labor mult
lab_sumut <- read.csv("lab_sumut.csv") %>% filter(!is.na(kbli2020_1))
V_labor <- diag(lab_sumut$pdrbpersektor/(output_sumut$`3100`*1000000),17,17)
V_labor_mult <-  V_labor %*% IL_sumut
V_labor_mult_1 <- V_labor %*% IL_sumut %*% solve(V_labor)

colSums(V_labor)
write_xlsx(as.data.frame(V_labor), "labor.xlsx")
#######wage mult
lab_sumut <-lab_sumut %>% mutate(upah_tahunan = input_upah *12)
v_wage <- lab_sumut$upah_tahunan/(output_sumut*1000000)
V_wage <- diag(v_wage$`3100`,17,17)
V_wage_mult <-  V_wage %*% IL_sumut
V_wage_mult_1 <- V_wage %*% IL_sumut %*% solve(V_wage)

v_wage$`3100`
lab_sumut$upah_tahunan/ lab_sumut$pdrbpersektor
colSums(V_wage_mult_1)
#############
expr <- list(value_impact = as.data.frame(V_mult),
     Value_multiplier = as.data.frame(V_mult_1),
     Labor_impact = as.data.frame(V_labor_mult),
     Labor_multiplier = as.data.frame(V_labor_mult_1),
     Wage_impact = as.data.frame(V_wage_mult),
     Wage_multiplier = as.data.frame(V_wage_mult_1))
writexl::write_xlsx(x = expr, "expr1.xlsx")
###########forward linkage

B_sumut <- diag(1/output_sumut$`3100`)  %*% as.matrix(select(Z_sumut, - `1800`)) 
G_sumut <- diag(rep(1,17),17,17) - t(B_sumut)
IG_sumut <- solve(diag(rep(1,17),17,17) - t(B_sumut))
colSums(IG_sumut)

excel_export <- list(A = as.data.frame( A_sumut),
                     B = as.data.frame(B_sumut),
                     Z = as.data.frame(Z_sumut),
                     `F` = as.data.frame(F_sumut),
                     V = as.data.frame(V_sumut),
                     L = as.data.frame(L_sumut),
                     G = as.data.frame(G_sumut),
                     IL = as.data.frame(IL_sumut),
                     IG = as.data.frame(IG_sumut)
                     )
writexl::write_xlsx(list(as.data.frame(G_sumut), as.data.frame(IG_sumut)), "rev.xlsx")

baclink <- colSums(IL_sumut)
forlink <- rowSums(IG_sumut)
avg_baclink <- baclink/mean(baclink)
avg_forlink <- forlink / mean(forlink)
colSums(IG_sumut)
linkages <- data.frame(Name = sektor, Forward_Linkage = forlink, Backward_Linkage = baclink, Avg_BL = avg_baclink, Avg_FL = avg_forlink)

linkages %>% ggplot(aes(avg_forlink, avg_baclink, col = Name)) + geom_point()  + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + 
    scale_x_continuous(trans = "log10") +geom_text_repel(aes(label = Name)) +xlab("Forward Linkages") + ylab("Backward Linkages") +
  ggtitle("Perbandingan Total Linkages Berbagai Sektor Sumatera Utara") + theme(legend.position = "none")
?scale_x_continuous
