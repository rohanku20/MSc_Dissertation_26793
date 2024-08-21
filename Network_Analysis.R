# Setup
library(openxlsx)
library(dplyr)
library(igraph)
library(tidyr)
library(ggplot2)
full_network <- read.xlsx("promotion_race.xlsx")
original_network <- full_network

# excel to igraph object function
create_network <- function(full_network) {
  # Create unique identifiers for each individual
  full_network <- full_network %>%
    mutate(id = paste(first_name, last_name, sep = "_")) %>%
    distinct(id, .keep_all = TRUE)
  
  # Create a vertex list with unique individuals and their attributes
  vertices <- full_network %>%
    distinct() %>%
    select(id, promotion, predicted_ethnicity, season)
  
  # Create an edge list where edges exist between individuals in the same franchise
  edges <- full_network %>%
    select(franchise, id) %>%
    group_by(franchise) %>%
    summarize(pairs = combn(id, 2, simplify = FALSE)) %>%
    unnest(pairs) %>%
    unnest(pairs)
  
  # Convert edge list to a dataframe
  edge_list <- data.frame(
    from = sapply(edges$pairs, `[`, 1),
    to = sapply(edges$pairs, `[`, 2)
  )
  
  # Initialize an empty edge list
  edge_list <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  # Iterate over the rows in pairs
  for (i in seq(1, nrow(edges), by = 2)) {
    from_name <- edges$pairs[i]
    to_name <- edges$pairs[i + 1]
    edge_list <- rbind(edge_list, data.frame(from = from_name, to = to_name, stringsAsFactors = FALSE))
  }
  
  # Create the graph
  fn_graph <- graph_from_data_frame(d = edge_list, vertices = vertices, directed = FALSE)
  
  return(fn_graph)
}

# Split data set
unique_seasons <- unique(full_network$season)

for (year in unique_seasons) {
  assign(paste0("network_", year), subset(full_network, season == year))
}

graphs <- list()
for (year in 2013:2023) {
  # Construct the dataset name
  dataset_name <- paste0("network_", year)
  
  # Use get() to retrieve the dataset and pass it to create_network
  graphs[[as.character(year)]] <- create_network(get(dataset_name))
}

# Assign each graph to a variable with a name like graph2013, graph2014, etc.
# Constructing individual graph object
for (year in 2013:2023) {
  assign(paste0("graph", year), graphs[[as.character(year)]])
}

###########################################################################
org_graph <- create_network(original_network)
summary(org_graph)

full_network <- full_network %>%
  mutate(id = paste(first_name, last_name, sep = "_")) %>%
  distinct(id, .keep_all = TRUE)
full_network <- full_network %>%
  rename(name = id)

full_cent <- degree(org_graph)
hist(full_cent)

############################################################################

# Calculate degree centrality for each graph
degree_centralities <- lapply(graphs, degree)

# Convert centralities into data frames
centrality_dfs <- lapply(degree_centralities, function(x) {
  data.frame(Node = names(x), Centrality = x, row.names = NULL)
})

# Merge all data frames by Node
centrality_df <- Reduce(function(x, y) merge(x, y, by = "Node", all = TRUE), centrality_dfs)
colnames(centrality_df) <- c("Node", paste0("Year_", 2013:2023))

# Replace NA values with 0 (for nodes not present in certain years)
centrality_df[is.na(centrality_df)] <- 0

# Calculate cumulative centrality
# Initialize a data frame to store cumulative centrality
cum_centrality_df <- centrality_df
cum_centrality_df[, -1] <- 0  # Set all centrality columns to 0 initially

# Calculate cumulative centrality
for (i in seq_along(2013:2023)) {
  if (i == 1) {
    cum_centrality_df[, paste0("Year_", 2013)] <- centrality_df[, paste0("Year_", 2013)]
  } else {
    cum_centrality_df[, paste0("Year_", 2013 + i - 1)] <- 
      rowSums(centrality_df[, paste0("Year_", 2013:(2013 + i - 1))])
  }
}

########### Descriptive Statistics for Results Section

# Histogram
hist(cum_centrality_df$Year_2023,
     breaks = 30,  # Adjust the number of breaks (bins) as needed
     main = "Histogram of Cumulative Centrality in 2023",
     xlab = "Cumulative Centrality (2023)",
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "black")

# Plot of initial year
plot(graph2013,
     main = "2013 Coaching Network",
     vertex.label.cex = NA)

##############################################################################

# Eigenvector centrality calculations - NOT STANDARDIZED OR CUMULATIVE
# STANDARDIZATION AND CUMULATIVE IS IN THE NEXT FILE (MODELS)
eigen_c <- data.frame(name = character(), stringsAsFactors = FALSE)

# Loop over the years from 2013 to 2023
for (year in 2013:2023) {
  # Get the graph for the current year
  current_graph <- graphs[[as.character(year)]]
  
  # Calculate eigenvector centrality for the current year
  centrality <- eigen_centrality(current_graph)$vector
  
  # Extract the vertex names
  vertex_names <- V(current_graph)$name
  
  # Create a dataframe for the current year's centrality
  temp_df <- data.frame(
    name = vertex_names,
    centrality = centrality,
    stringsAsFactors = FALSE
  )
  
  # Merge with the eigen_c dataframe, matching on 'name'
  eigen_c <- merge(eigen_c, temp_df, by = "name", all = TRUE, suffixes = c("", paste0("_", year)))
  
  # Fill NA with 0 for individuals not present before their entry year
  eigen_c[is.na(eigen_c)] <- 0
  
  # Create or update the cumulative centrality for the current year
  eigen_c[[paste0("year_", year)]] <- ifelse(eigen_c$centrality == 0, 
                                             ifelse(year > 2013, eigen_c[[paste0("year_", year - 1)]], 0), 
                                             eigen_c$centrality)
  eigen_c$centrality <- NULL
}

# Merging + Exporting dtaset with centrality included 
cum_centrality_df <- cum_centrality_df %>%
  rename(name = Node)

original_network <- original_network %>%
  mutate(id = paste(first_name, last_name, sep = "_"))

original_network <- original_network %>%
  rename(name = id)

merged_cum_eigen <- inner_join(cum_centrality_df, eigen_c, by = "name")
final_merged_df <- left_join(original_network, merged_cum_eigen, by = "name")

write.xlsx(final_merged_df, "allcovariates.xlsx")

