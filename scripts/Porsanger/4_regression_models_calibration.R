#### REGRESSION MODELS ####

data4regression <- readRDS("/Users/pedronicolau/OccupancyAbundanceCalibration/data/data_for_regression.rds")

# remove station 4
jointset3 <- filter(data4regression,station!="G4")

# MODEL RAW LIVE TRAPS VS VOLE CAMERA PICS #

# smooth term model
# checking linearity
day2inla_linear_raw <- inla(cr~after_2
                            +f(station,model = "iid")
                            +f(trapseason,
                               model = "ar1", 
                               hyper = list(prec=list(prior="pc.prec", param=c(U=1,alpha=.01)),
                                            rho=list(prior="pc.prec", param=c(U=0.5,alpha=0.1)))),
                            data = jointset3, family="poisson",
                            control.compute = list(dic=TRUE, cpo=TRUE))

day2inla_linear_raw$summary.fixed
plot(day2inla_linear_raw$summary.random$trapseason$`0.5quant`, type="p", ylim=c(-3,3))
lines(day2inla_linear_raw$summary.random$trapseason$`0.025quant`, lty=2)
lines(day2inla_linear_raw$summary.random$trapseason$`0.975quant`, lty=2)

par(mfrow=c(1,3))
# Linear 2 days after
day2inla_linear_after2 <- inla(cr~after_2
                               +f(station,model = "iid"), 
                               data = jointset3, family="poisson",
                               control.compute = list(dic=TRUE, cpo=TRUE))

day2inla_linear_after2$summary.fixed

plot(day2inla_linear_after1$marginals.fixed$a1, type = "l", xlim = c(0,.1), xlab="Beta 1", main="Posterior Density")
lines(day2inla_linear_after2$marginals.fixed$after_2, type = "l", col = "red")
lines(day2inla_linear_during$marginals.fixed$during_2, type = "l", col = "blue")
legend("topright", c("1 day after", "2 days after (mean)", "during (mean)"), lwd = 1, col = c(1,2,4))

plot(jointset3$cr,day2inla_linear_after2$summary.fitted.values$mean, xlab="True number of trapped individuals",
     ylab = "Predicted number of indivuals", ylim=c(0,50), pch=19, col=alpha("gray30",.9), cex=1.3, main = "2 Days After")
abline(a=0,b=1, lty=2)

res_a2_1 <- jointset3$cr-day2inla_linear_after2$summary.fitted.values$mean
res_a2_2 <- res_a2_1/sd(res_a2_1)


# Linear 2 days during
day2inla_linear_during <- inla(cr~during_2
                               +f(station,model = "iid"), 
                               data = jointset3, family="poisson",
                               control.compute = list(dic=TRUE, cpo=TRUE))

res_d2_1 <- jointset3$cr-day2inla_linear_during$summary.fitted.values$mean
res_d2_2 <- res_d2_1/sd(res_d2_1)

# Linear 1 days during
day2inla_linear_after1 <- inla(cr~a1
                               +f(station,model = "iid")+trapseason, 
                               data = jointset3, family="poisson",
                               control.compute = list(dic=TRUE, cpo=TRUE))

day2inla_linear_after1$summary.fixed

cor(jointset3$vgamcr, jointset3$a1)
cor(jointset3$vgamcr, jointset3$after_2)
cor(jointset3$vgamcr, jointset3$during_2)


lm1 <- lm(cr ~ during_2+trapseason+station, data=jointset3)
summary(lm1)
plot(lm1)
res_a1_1 <- jointset3$cr-day2inla_linear_after1$summary.fitted.values$mean
res_a1_2 <- res_a1_1/sd(res_a1_1)


# QQ Plots
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_a2_2), pch=19, col="gray30", main = "After 2", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_a1_2), pch=19, col="gray30", main = "After 1", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_d2_2), pch=19, col="gray30", main = "During 2", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)


length(res_d2_2)
.99-.01
.98/52
plot(day2inla_linear_after2$summary.fitted.values$mean, res_a2_2)

res1 <- jointset3$cr-day2inla_linear_after2$summary.fitted.values$mean
res2 <- res1/sd(res1)
abline(h=1.96, lty=2)
abline(h=-1.96, lty=2)
plot(res2)
plot(jointset3$cr,day2inla_linear_after1$summary.fitted.values$mean, xlab="True number of trapped individuals",
     ylab = "Predicted number of indivuals", ylim=c(0,50), pch=19, col=alpha("gray30",.9), cex=1.3, main = "1 Day After")
abline(a=1,b=1, lty=2)

plot(jointset3$cr,day2inla_linear_during$summary.fitted.values$mean, xlab="True number of trapped individuals",
     ylab = "Predicted number of indivuals", ylim=c(0,50), pch=19, col=alpha("gray30",.9), cex=1.3, main = "2 Days During")
abline(a=1,b=1, lty=2)

day2inla_smooth <- inla(vgamcr~f(after_2, model="rw2", scale.model=TRUE,
                                 hyper = list(prec= list(prior="pc.prec", param=c(U=3,alpha=0.5))))
                        +f(station,model = "iid")+f(trapseason, model="ar1"), 
                        data = jointset3, family="gaussian",
                        control.compute = list(dic=TRUE, cpo=TRUE))

plot(day2inla_smooth$summary.random$after_2$ID, day2inla_smooth$summary.random$after_2$mean, type="l",
     xlab="f(after_2, model = rw2)", ylab="f(trap number)")
lines(day2inla_smooth$summary.random$after_2$ID, day2inla_smooth$summary.random$after_2$`0.025quant`, lty=2)
lines(day2inla_smooth$summary.random$after_2$ID, day2inla_smooth$summary.random$after_2$`0.975quant`, lty=2)


### ABUNDANCE MODELS ####
# Linear 2 days after
abund_linear_after2_v <- inla(vgamcr~after_2
                              +f(station,model = "iid"), 
                              data = jointset3, family="poisson",
                              control.compute = list(dic=TRUE, cpo=TRUE))

abund_linear_after2_i <- inla(inlacr~after_2
                              +f(station,model = "iid"), 
                              data = jointset3, family="poisson",
                              control.compute = list(dic=TRUE, cpo=TRUE))


plot(abund_linear_after2_v$marginals.fixed$after_2, type = "l", xlim = c(0,.08), xlab="Beta 1", main="Posterior Density", ylim=c(0,100), 
     col = "red", lwd=1)
lines(day2inla_linear_after2$marginals.fixed$after_2, type = "l", col = "orange", lwd=1)
lines(abund_linear_after2_i$marginals.fixed$after_2, type = "l", col = "gold", lwd=1)
legend("topright", c("raw counts", "VGAM abundance estimate", "INLA abundance estimate"), col = c("red","orange","gold"), lwd=1)

plot(jointset3$cr,abund_linear_after2$summary.fitted.values$mean, xlab="True number of trapped individuals",
     ylab = "Predicted number of indivuals", ylim=c(0,50), pch=19, col=alpha("gray30",.9), cex=1.3, main = "2 Days After")
abline(a=0,b=1, lty=2)

res_a2_1 <- jointset3$cr-day2inla_linear_after2$summary.fitted.values$mean
res_a2_2 <- res_a2_1/sd(res_a2_1)


# Linear 2 days during
abund_linear_during <- inla(inlacr~during_2
                            +f(station,model = "iid"), 
                            data = jointset3, family="poisson",
                            control.compute = list(dic=TRUE, cpo=TRUE))

res_d2_1 <- jointset3$cr-day2inla_linear_during$summary.fitted.values$mean
res_d2_2 <- res_d2_1/sd(res_d2_1)

# Linear 1 days during
abund_linear_after1 <- inla(inlacr~a1
                            +f(station,model = "iid"), 
                            data = jointset3, family="poisson",
                            control.compute = list(dic=TRUE, cpo=TRUE))



res_a1_1 <- jointset3$cr-day2inla_linear_after1$summary.fitted.values$mean
res_a1_2 <- res_a1_1/sd(res_a1_1)


# QQ Plots
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_a2_2), pch=19, col="gray30", main = "After 2", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_a1_2), pch=19, col="gray30", main = "After 1", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)
plot(qnorm(seq(0.01,0.99,.99/52)), sort(res_d2_2), pch=19, col="gray30", main = "During 2", xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles")
abline(a=0,b=1, lty=2)

##########

abline(h=0)


plot(jointset3$cr,day2inla_smooth$summary.fitted.values$mean)



day2inla_linear$summary.fixed
plot(jointset3$cr,day2inla_linear$summary.fitted.values$mean, xlab="True number of trapped individuals",
     ylab = "Predicted number of indivuals", ylim=c(0,50))
abline(a=1,b=1, lty=2)

plot(jointset3$cr,jointset3$after_2)
plot(day2inla_linear$summary.random$station$mean)

# set pc prior with the same hyper parameters
# and set scale model equals to true
# plot the estimated model with the scatter plot
# 

par(mfrow=c(1,1))


day2inla$summary.random$station$mean
lines(day2inla$summary.random$a2$`0.025quant`)
lines(day2inla$summary.random$a2$`0.975quant`)



day2inla$summary.fixed
par(mfrow=c(1,1))
plot(log(cr)~log(after_2),data=jointset2)
abline(a=1,b=1)

day2glm <- glm(cr~after_2,family="quasipoisson",data = jointset3)
summary(day2glm)
plot(day2glm)

day2glmx <- glm(cr~during_2,family="quasipoisson",data = jointset3)
summary(day2glmx)

day2loglm <- lm(logcr~logafter_2,data = jointset3)
summary(day2loglm)

plot(jointset3$after_2,jointset3$cr)
abline(a=day2loglm$coefficients[1],b=day2loglm$coefficients[2], lty=2)
day2glm
summary(day2glm)

day2lmd <- lm(cr~during_2,data = jointset3)
summary(day2lmd)
plot(jointset3$during_2,jointset3$cr)
abline(a=day2lm$coefficients[1],b=day2loglm$coefficients[2], lty=2)

plot(day2glm)


# predit
abline(a=1,b=1,lty=1)
# cr as response
# poisson likelihood
