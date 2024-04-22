# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Read in the OTU table
DAT <- read_tsv("otu_table.tsv")

# Reshape the data to long format
DAT <- DAT %>%
  pivot_longer(-otu_id, names_to = "Sample_id", values_to = "count")

# Read in the taxonomy data
Taxonomy <- read_tsv("otu_taxonomy.tsv", col_types = cols(.default = "character"))

# Merge taxonomy data into the main dataset
DAT <- DAT %>%
  left_join(Taxonomy, by = "otu_id")

# Read in sample metadata
Meta <- read_tsv("sample_metadata.tsv", col_types = cols(.default = col_character()))

# Merge sample metadata into the main dataset
DAT <- DAT %>%
  left_join(Meta, by = c("Sample_id" = "sample_id"))

# Check the structure of DAT to ensure Phylum.y is present
str(DAT)

# Plot the relative abundance of bacterial species
DAT %>%
  ggplot(aes(x = Sample_id, y = count)) +
  facet_grid(~ fraction + soil, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = Phylum), stat = "identity", position = "fill", width = 1) +
  scale_fill_brewer(palette = "Paired")

# Define the order of Phylum levels based on abundance
phyla_order <- c("Proteobacteria", "Actinobacteria", "Bacteroidetes", "Acidobacteria",
                 "Firmicutes", "Cyanobacteria", "Verrucomicrobia", "Gemmatimonadetes",
                 "Armatimonadetes", "Chloroflexi", "unclassified")

# Apply the Phylum order
DAT <- DAT %>%
  mutate(Phylum = factor(Phylum, levels = phyla_order))

# Final plot with customized appearance
DAT %>%
  ggplot(aes(x = Sample_id, y = count)) +
  facet_grid(~ fraction + soil, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = Phylum), stat = "identity", position = "fill", width = 1) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(name = "Relative abundance of the bacterial species", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        axis.text.y = element_text(color = "black"),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank())

# Define custom colors
standard_colors <- c("maroon", "#ff7f0e", "navy", "magenta", "red", "darkgreen", 
                     "springgreen", "yellow", "cyan", "purple", "orange", "blue")

# Plot with manual fill colors
RAD <- DAT %>%
  ggplot(aes(x = Sample_id, y = count, fill = Phylum)) +
  facet_grid(~ fraction + soil, scales = "free_x", space = "free_x") +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = standard_colors) +  
  scale_y_continuous(name = "Relative abundance of the bacterial species", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        axis.text.y = element_text(color = "black"),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank())

# Save the plot as a PNG file
ggsave("Relative_abundance_plot.png", RAD, width = 15, height = 12, dpi = 800)
