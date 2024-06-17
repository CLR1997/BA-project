#### stargazer!


library(stargazer)
library(knitr)
Sem1_to <- c("Sem 2", "Sem 3", "Sem 4")
kappa_value <- c(0.64, 0.70, 0.67)
#t(data.frame(Sem1_to, kappa_value))
stargazer(t(data.frame(Sem1_to, kappa_value)))
kable(print(paper.page7table, printToggle=FALSE, noSpaces = TRUE) , format = "latex")

suppressWarnings(print(plot.page5))  
suppressWarnings(print(plot.page5.nottransformed)) 
suppressWarnings(print(plot.page5.nottransformed.focused)) 


suppressWarnings(print(plot.alcgpa2))
suppressWarnings(print(plot.alcgpa.outliers2))

suppressWarnings(print(plot.MJGPA))


stargazer(summary(full.model.ARMA11))

stargazer(df_scores_Cor[,-3], summary = FALSE, flip = F, title = "Correlation Structures Comparison")

cowplot::plot_grid(residuals.AR1, residuals.Toelpitz, residuals.ARMA11, residuals.ARMA12, nrow = 2)
cowplot::plot_grid(residuals.Unstructured, residuals.CompSymm, residuals.Exponential, residuals.Gaussian)

stargazer(Anova(pseudo.trajectory.ARMA11), summary = FALSE, flip = F, title = "Significance Pseudo Trajectory")
stargazer(full.model.ARMA11, title = "Summary Full Model", type = "text", font.size = "small")

plot(effect("Semester*Cluster_current", full.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Full Model: Cluster & Semester")
plot(effect("Cluster_current*Semester", small.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Small Model: Cluster & Semester")
plot(effect("Cluster_current*Time", time.slope.ARMA11, robust= "hc3", correction = "Sidak"), main = "Time Model: Cluster & Semester")



library(broom)
