library("pangaear")
library("janitor")
library("dplyr"
library("ggplot2")

# load foram isotope data from Baffin Bay
res <- pg_data(doi = "10.1594/PANGAEA.744844")

# look at the format
str(res, max = 1)

# can extract and display the metadata
res[[1]]$meta

# extract the data for the data set
baffin <- res[[1]]$data

# what variables do we have?
names(baffin)

# rename the two isotope data columns and then clean the remaining names
baffin <- baffin %>%
  rename(d18o = `N. pachyderma s δ18O [‰ PDB]`,
         d13c = `N. pachyderma s δ13C [‰ PDB]`) %>%
  janitor::clean_names() %>%
  mutate(event = factor(event)) # convert event to a factor

# how many samples per core site
baffin %>%
group_by(event) %>%
count()

# store some labels for the plot
ylabel <- expression(delta^{18} * O ~ "[‰ VPDB]")
xlabel <- "Depth [m]"

# plot a single core
baffin %>%
filter(event == "HUD85-027-16") %>%
ggplot(aes(y = d18o, x = depth_m)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel)

# plot all cores but colour by `event`
baffin %>%
ggplot(aes(y = d18o, x = depth_m, colour = event)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel)
  
# plot all cores but facet by `event`, no colour
baffin %>%
ggplot(aes(y = d18o, x = depth_m)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel) +
  facet_wrap(~ event)

# combine both colour and facetting
baffin %>%
ggplot(aes(y = d18o, x = depth_m, colour = event)) +
  geom_point() +
  geom_path(alpha = 0.3) +
  geom_smooth() +
  scale_x_reverse() +
  labs(y = ylabel, x = xlabel) +
  facet_wrap(~ event)

