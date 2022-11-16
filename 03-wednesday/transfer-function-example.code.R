
library(vegan)
library(analogue)

data(ImbrieKipp)
data(SumSST)
data(WinSST)
data(Salinity)
data(V12.122)

names(ImbrieKipp)
names(SumSST)
head(SumSST)

ik <- ImbrieKipp / 100

mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = ik,
           family = binomial(link = "logit"))
summary(mod)

mod1 <- glm(G.pacL ~ SumSST, data = ik,
            family = binomial(link = "logit"))
mod1
summary(mod1)

anova(mod1, mod, test = "Chisq")

pred.dat <- data.frame(SumSST = seq(min(SumSST), max(SumSST), length = 100))

pred.mod <- predict(mod, pred.dat, type = "response")

plot(G.pacL ~ SumSST, data = ik, pch = 21, col = "red", bg = "yellow")

lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)

library(mgcv)

gam.mod <- gam(G.pacR ~ s(SumSST), data = ik,
            family = binomial(link = "log"))

pred.gam.mod <- predict(gam.mod, pred.dat, type = "response")
plot(G.pacR ~ SumSST, data = ik, pch = 21, col = "red", bg = "yellow")
lines(pred.gam.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)

# left_coiling_gauss
mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = ik,
           family = binomial(link = "logit"),
           control = glm.control(trace = TRUE))
pred.dat <- data.frame(SumSST = seq(min(SumSST), max(SumSST), length = 100))
pred.mod <- predict(mod, pred.dat, type = "response")
plot(G.pacL ~ SumSST, data = ik, pch = 21, col = "red", bg = "yellow")
lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)

# create figure on the slide
opar <- par(mfrow = c(2,1))
mod2 <- glm(G.pacR ~ SumSST + I(SumSST^2), data = imbrie,
            family = binomial(link = "log"))
pred.mod2 <- predict(mod2, pred.dat, type = "response")
plot(G.pacR ~ SumSST, data = imbrie, type = "n")
abline(h = 0, col = "grey")
points(G.pacR ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod2 ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
beta <- coef(mod2)
h <- exp(beta[1] - (beta[2]^2 / (4 * beta[3])))
u <- -(beta[2]/(2*beta[3]))
t <- sqrt(-(1/(2*beta[3])))
wa.opt <- with(imbrie, weighted.mean(SumSST, G.pacR))
rug(u, side = 3, lwd = 2, col = "blue")
rug(wa.opt, side = 3, lwd = 2, col = "red")
mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = imbrie,
           family = binomial(link = "logit"))
pred.mod <- predict(mod, pred.dat, type = "response")
plot(G.pacL ~ SumSST, data = imbrie, type = "n")
abline(h = 0, col = "grey")
points(G.pacL ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
beta <- coef(mod)
h <- exp(beta[1] - (beta[2]^2 / (4 * beta[3])))
u <- -(beta[2]/(2*beta[3]))
t <- sqrt(-(1/(2*beta[3])))
wa.opt <- with(imbrie, weighted.mean(SumSST, G.pacL))
#rug(u, side = 3, lwd = 2, col = "blue")
rug(wa.opt, side = 3, lwd = 2, col = "red")
box()
par(opar)
#lines(c(u,u), c(0,h))


# Weighted averaging
mod <- wa(SumSST ~ ., data = ik, deshrink = "inverse")
mod
opar <- par(mfrow = c(1,2))
plot(mod)
par(opar)

fos <- V12.122 / 100
pred <- predict(mod, fos)
reconPlot(pred, use.labels = TRUE, ylab = "SumSST", xlab = "Depth")

# MAT
data(swapdiat, swappH, rlgh)
dat <- join(swapdiat, rlgh, verbose = TRUE)
swapdiat <- with(dat, swapdiat / 100)
rlgh <- with(dat, rlgh / 100)
swap.mat <- mat(swappH ~ ., data = swapdiat, method = "SQchord")
swap.mat
opar <- par(mfrow = c(2,2))
plot(swap.mat)
par(opar)

rlgh.mat <- predict(swap.mat, rlgh, k = 10)
rlgh.mat
reconPlot(rlgh.mat, use.labels = TRUE, ylab = "pH", xlab = "Depth (cm.)")

# max bias plot
plot(swap.mat, which = 2)

# minimum dissimilarity - do we have close analogues
rlgh.mdc <- minDC(rlgh.mat)
plot(rlgh.mdc, use.labels = TRUE, xlab = "Depth (cm.)")

quantile(as.dist(swap.mat$Dij), prob = c(0.01,0.025,0.05, 0.1))

swap.boot
set.seed(1234)
rlgh.boot <- predict(swap.mat, rlgh, bootstrap = TRUE, n.boot = 200)
pdf("sample_specific_error_plot.pdf", height = 6, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
reconPlot(rlgh.boot, use.labels = TRUE, ylab = "pH", xlab = "Depth (cm.)",
          display.error = "bars", predictions = "bootstrap")
dev.off()
