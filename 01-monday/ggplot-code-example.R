library("pangaear")

res <- pg_data(doi = "10.1594/PANGAEA.744844")

str(res, max = 1)

res[[1]]$meta

baffin <- res[[1]]$data

names(baffin)

baffin <- baffin %>%
  rename(d18o = `N. pachyderma s δ18O [‰ PDB]`,
         d13c = `N. pachyderma s δ13C [‰ PDB]`) %>%
  janitor::clean_names() %>%
  mutate(event = factor(event))

baffin %>%
group_by(event) %>%
count()

ylabel <- expression(delta^{18} * O ~ "[‰ VPDB]")
xlabel <- "Depth [m]"

baffin %>%
filter(event == "HUD85-027-16") %>%
ggplot(aes(y = d18o, x = depth_m)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel)

baffin %>%
ggplot(aes(y = d18o, x = depth_m, colour = event)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel)
  
baffin %>%
ggplot(aes(y = d18o, x = depth_m)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel) +
  facet_wrap(~ event)

baffin %>%
ggplot(aes(y = d18o, x = depth_m, colour = event)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  geom_smooth() +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel) +
  facet_wrap(~ event)

