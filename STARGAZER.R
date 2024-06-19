#### stargazer!


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
stargazer(full.model.ARMA11, small.model.ARMA11, title = "Summary 'main' Model effects", type = "latex", font.size = "small")

plot(effect("Semester*Cluster_current", full.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Full Model: Cluster & Semester")
plot(effect("Cluster_current*Semester", small.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Small Model: Cluster & Semester")
plot(effect("Cluster_current*Time", time.slope.ARMA11, robust= "hc3", correction = "Sidak"), main = "Time Model: Cluster & Semester")

plot(effect("Sex", full.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Effect of Gender")
plot(effect("Fager4_binary", full.model.ARMA11, robust= "hc3", correction = "Sidak"), main = "Effect of Smoking")

plot(effect("Group_transition1", pseudo.trajectory.ARMA11, robust= "hc3", correction = "Sidak"), main = "Effect Cluster Transition")

ggplot.fm
ggplot.sm
ggplot.pt

range.ri <- data.frame(full.model = as.vector(range(ranef(full.model.ARMA11))),
                       small.model = as.vector(range(ranef(small.model.ARMA11))),
                       pseudo.trajectory = as.vector(range(ranef(pseudo.trajectory.ARMA11))))

rownames(range.ri) <- c("min", "max")
range.ri

stargazer(range.ri) ## I've plugged in the values in the table, because they were to small, the function rounded to zero
# max(as.vector(range(ranef(full.model.ARMA11)))) > 0.000001
# [1] TRUE
# max(as.vector(range(ranef(full.model.ARMA11)))) > 0.000001
# [1] TRUE
# 

stargazer(vif(full.model.gls))
stargazer(vif(pseudo.trajectory.gls))



stargazer(df_scores_sm.r2[,-3], summary = FALSE)
stargazer(df_scores_fmri.r2[,-3], summary = FALSE)


stargazer(df_interaction[,-3], summary = FALSE)

stargazer(sm.outlier.barebones, fm.outlier.barebones, fm.outlier.int)

range.numeric <- data.frame(full.model = as.vector(range(ranef(fm.outlier.barebones))),
                       small.model = as.vector(range(ranef(sm.outlier.barebones))))
rownames(range.numeric) <- c("min", "max")
range.numeric
stargazer(range.numeric) ## I had to adjust the table a lot manually. 

ggplot.fmo + ggtitle("Numerical Full Model residuals vs fitted")
ggplot.smo + ggtitle("Numerical Small Model residuals vs fitted")

r.squaredGLMM(fm.outlier.barebones)


stargazer(df.sm.spline[,-3], summary = FALSE)

stargazer(sm.splines.mix)
summary(sm.splines.mix$lme)
plot.gam(sm.splines.mix$gam, all.terms = TRUE, main = "Marijuana consumption spline function")
ggplot.mix + ggtitle("GAMM residuals vs fitted")

range(random.effects(sm.splines.mix$lme))
