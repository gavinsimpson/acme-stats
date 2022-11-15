# Let's load the dinocyst data from pangaear
library("vegan")
library("pangaear")
doi <- "10.1594/PANGAEA.908494"
cyst_all <- pg_data(doi)

cyst_df <- cyst[[1]]$data

names(cyst_df)
cyst_spp <- cyst_df %>%
    select(`A. andalousiensis [%]`:`D. chathamensis [%]`) %>%
    janitor::clean_names()

cyst_meta <- cyst_df %>%
    select(`Sample label`:Longitude) %>%
    janitor::clean_names()

cyst_env <- cyst_df %>%
    select(`Distance [km] (to the coast)`:`PP C area [g/m**2/a]`) %>%
    janitor::clean_names()

# unconstrained ordination

# pca
cyst_pca <- rda(decostand(cyst_spp, method = "hellinger"), scale = FALSE)

# How many axes to retain?
# How much variance is explained on those axes?

# Plot the ordination diagram
biplot(cyst_pca, scaling = "species", correlation = TRUE,
    type = c("text", "points"))

# Try CA
cyst_ca <- cca(cyst_spp)

plot(cyst_ca, scaling = "species", hill = TRUE,
    type = c("points"))

plot(cyst_ca, scaling = "species", type = "n")
ordipointlabel(cyst_ca, scaling = "species", display = "species",
    add = TRUE)

# Try using envfit() or ordisurf() to look at relationships between env
# and ordination scores

# constrained ordination

# build a model using one or more variable from cyst_env to explain the
# cyst relative abundances
# You won't want to use all the variables in cyst_env, so choose wisely