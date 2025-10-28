library(dplyr)
library(readxl)
library(factoextra)

# Load the data
df_raw <- read_excel("raw_data/CMO-Historical-Data-Annual.xlsx", 
                   sheet = "Annual Prices (Real)",
                   skip = 5, 
                   col_names = FALSE)

colnames(df_raw) <- paste(df_raw[1, ], df_raw[2, ], sep = "\n")

# Remove the first two rows from the data
df <- df_raw[-c(1,2), ] %>%
  rename(
    year = `NA\nNA`  
  ) %>%
  select(c(
    year,
    `Platinum\n($/troy oz)`,
    `Gold\n($/troy oz)`,
    `Silver\n($/troy oz)`,
    `Zinc\n($/mt)`,
    `Nickel\n($/mt)`,
    `Tin\n($/mt)`,
    `Copper\n($/mt)`,
    `Lead\n($/mt)`,
    `Iron ore, cfr spot\n($/dmtu)`,
    `Phosphate rock\n($/mt)`
  )) %>%
  filter(year >= 2000 & year <= 2022)

#standardize the data
df_std <- df %>%
  select(-year) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale() %>%
  as.data.frame()

#variance-covariance matrix
cov_matrix <- cov(df_std)

pca <- prcomp(df_std, center = TRUE, scale. = TRUE)
summary(pca)
biplot(pca) 

# Highlight Gold and Oil only
fviz_pca_biplot(pca,
                label = "var",
                col.var = "black",
                select.var = list(name = c("Gold\n($/troy oz)", "Tin\n($/mt)", "Iron ore, cfr spot\n($/dmtu)")),
                repel = TRUE)


loadings <- pca$rotation  # Columns = PCs, Rows = original variables
print(loadings)

gold_vector <- loadings["Gold\n($/troy oz)", ]

# Cosine similarity between gold and others
cosine_sim <- apply(loadings, 1, function(x) sum(x * gold_vector) / (sqrt(sum(x^2)) * sqrt(sum(gold_vector^2))))

# Remove gold from comparison
cosine_sim <- cosine_sim[names(cosine_sim) != "Gold\n($/troy oz)"]

# Most aligned (cosine ~ 1)
most_similar <- names(which.max(cosine_sim))

# Most orthogonal (cosine ~ 0)
most_orthogonal <- names(which.min(abs(cosine_sim)))

# Print results
cat("Most similar to Gold:", most_similar, "\n")
cat("Most orthogonal to Gold:", most_orthogonal, "\n")
