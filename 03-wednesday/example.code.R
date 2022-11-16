
require(vegan)

load("imbrie.env.rda")
load("imbrie.rda")
ls()

names(imbrie)
names(imbrie.env)

SumSST <- imbrie.env$SumSST
imbrie <- imbrie / 100

mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = imbrie,
           family = binomial(link = "logit"))

mod1 <- glm(G.pacL ~ SumSST, data = imbrie,
            family = binomial(link = "logit"))
mod1
summary(mod1)

anova(mod1, mod, test = "Chisq")

pred.dat <- data.frame(SumSST = seq(min(SumSST), max(SumSST), length = 100))

pred.mod <- predict(mod, pred.dat, type = "response")

plot(G.pacL ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")

lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)

library(mgcv)

gam.mod <- gam(G.pacR ~ s(SumSST), data = imbrie,
            family = binomial(link = "log"))

pred.gam.mod <- predict(gam.mod, pred.dat, type = "response")
plot(G.pacR ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.gam.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)



pdf("left_coiling_gauss.pdf", height = 8, width = 8,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = imbrie,
           family = binomial(link = "logit"),
           control = glm.control(trace = TRUE))
pred.dat <- data.frame(SumSST = seq(min(SumSST), max(SumSST), length = 100))
pred.mod <- predict(mod, pred.dat, type = "response")
plot(G.pacL ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
dev.off()

pdf("right_coiling_gauss.pdf", height = 8, width = 8,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
mod2 <- glm(G.pacR ~ SumSST + I(SumSST^2), data = imbrie,
            family = binomial(link = "log"))
pred.mod2 <- predict(mod2, pred.dat, type = "response")
plot(G.pacR ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod2 ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
dev.off()

pdf("three_forams_gauss.pdf", height = 4, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
mod <- glm(G.pacL ~ SumSST + I(SumSST^2), data = imbrie, family =
           binomial(link = "logit"))
mod2 <- glm(G.pacR ~ SumSST + I(SumSST^2), data = imbrie, family =
            binomial(link = "log"))
mod3 <- glm(O.univ ~ SumSST + I(SumSST^2), data = imbrie, family =
            binomial(link = "log"))
pred.mod <- predict(mod, pred.dat, type = "response")
pred.mod2 <- predict(mod2, pred.dat, type = "response")
pred.mod3 <- predict(mod3, pred.dat, type = "response")
opar <- par(mfrow = c(1,3))
plot(G.pacL ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
plot(G.pacR ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod2 ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
plot(O.univ ~ SumSST, data = imbrie, pch = 21, col = "red", bg = "yellow")
lines(pred.mod3 ~ SumSST, data = pred.dat, col = "blue", lwd = 2)
par(opar)
dev.off()

pdf("g_pachy_wa.pdf", height = 9, width = 6,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
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
dev.off()
#lines(c(u,u), c(0,h))

pdf("deshrinking.pdf", height = 6, width = 6,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
opt <- w.avg(as.matrix(imbrie), SumSST)
pred <- ((as.matrix(imbrie) %*% opt) / rowSums(as.matrix(imbrie)))[,1]
plot(SumSST ~ pred, pch = 21, col = "red", bg = "yellow",
     xlab = "WA estimated SumSST")
abline(0,1)
abline(lm(SumSST ~ pred), col = "blue", lwd = 2)
leg.txt <- paste("WA range:", paste(round(range(pred), 2),
                                   collapse = " - "))
leg.txt <- c(leg.txt, paste("Observed range:", paste(round(range(SumSST), 2),
                                           collapse = " - ")))
legend("topleft", legend = leg.txt, bty = "n")
dev.off()

pdf("wa_diag_plots.pdf", height = 6, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
mod <- wa(SumSST ~ ., data = imbrie, deshrink = "inverse")
mod
opar <- par(mfrow = c(1,2))
plot(mod)
par(opar)
dev.off()

pdf("reconPlot.pdf", height = 6, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
load("imbrie.fos.rda")
pred <- predict(mod, imbrie.fos)
reconPlot(pred, use.labels = TRUE, ylab = "SumSST", xlab = "Depth")
dev.off()

data(swapdiat, swappH, rlgh)
dat <- join(swapdiat, rlgh, verbose = TRUE)
swapdiat <- with(dat, swapdiat / 100)
rlgh <- with(dat, rlgh / 100)
swap.mat <- mat(swappH ~ ., data = swapdiat, method = "SQchord")
swap.mat
opar <- par(mfrow = c(2,2))
plot(swap.mat)
par(opar)

pdf("mat_diag_plots.pdf", height = 8, width = 8,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
opar <- par(mfrow = c(2,2))
plot(swap.mat)
par(opar)
dev.off()

rlgh.mat <- predict(swap.mat, rlgh, k = 10)
rlgh.mat
reconPlot(rlgh.mat, use.labels = TRUE, ylab = "pH", xlab = "Depth (cm.)")

pdf("mat_reconPlot.pdf", height = 6, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
reconPlot(rlgh.mat, use.labels = TRUE, ylab = "pH", xlab = "Depth (cm.)")
dev.off()

pdf("mat_bias_plot.pdf", height = 8, width = 8,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
plot(swap.mat, which = 2)
dev.off()

pdf("minDC_plot.pdf", height = 6, width = 12,
    pointsize = 12, paper = "special", version = "1.4",
    onefile = FALSE)
rlgh.mdc <- minDC(rlgh.mat)
plot(rlgh.mdc, use.labels = TRUE, xlab = "Depth (cm.)")
dev.off()

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
