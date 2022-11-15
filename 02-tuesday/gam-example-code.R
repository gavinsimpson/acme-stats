# Let's load the ARA2B data from Pangaea
library("pangaear")
doi <- "10.1594/PANGAEA.868790"
ara2b <- pg_data(doi)
ara2b_df <- ara2b[[1]]$data
ara2b_df

# Packages needed for the code below
library("mgcv")
library("gratia")
library("ggplot2")
library("dplyr")
library("patchwork")

# clean the names and create a negative age variable so we can model time in
# the right direction
cleaned <- ara2b_df %>%
    janitor::clean_names() %>%
    mutate(neg_age = - age_ka_bp)

# fit a smooth of `neg_age`
m <- gam(brassicasterol_toc_mg_g ~ s(neg_age), data = cleaned,
    method = "REML")

# model diagnostics
appraise(m)
# some non-constant variance

# basis size looks OK though
k.check(m)

# model significance
summary(m)

# plot the smooth
draw(m)

# residuals looks bad. Need a distribution that is constrained to be
# with non-negative
m2 <- gam(brassicasterol_toc_mg_g ~ s(neg_age), data = cleaned,
    method = "REML",
    family = tw())

appraise(m2)

k.check(m2)

summary(m2)

draw(m2)

dt <- derivatives(m2, type = "central")
dt %>% draw()

draw(m2) + draw(dt) + plot_layout(nrow = 2)
