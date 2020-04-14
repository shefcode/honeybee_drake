library(tidyverse)
library(purrr)
library(here)
library(drake)


### Drake Stuff
bee_analysis <- drake::drake_plan(
    raw_df = read.csv(here("../honeybee_data.csv"))
    )

drake_plan

make(bee_analysis)
loadd(raw_df)

### Simple data analysis set up [To be included in the drake in the future]

raw_df <- read.csv(here("../honeybee_data.csv"))
names(raw_df)[1] <- "Domain"

# WORKS, YEAH!
test_plot <- ggplot(data = raw_df,
                    aes(x = Family, y = SRR2)) +
  geom_point()
test_plot

# geom_bar()
test_bar <- ggplot(data = raw_df,
                   aes(x = Family)) +
  geom_bar(aes(fill = SRR2))
test_bar

# Works, Creates a column with the sum of the number of hits in total
test_mutate <- raw_df %>% 
  mutate(sum_hit = rowSums(select(., SRR1:SRR10)))

# Works, Create a gpplot to show the number of hits in each sample [to help with the visualization of the data] 
test_mutate_graph <- raw_df %>% 
  mutate(sum_hit = rowSums(select(., SRR1:SRR10))) %>% 
  ggplot(aes(x = Family, y = sum_hit)) +
  geom_point() +
  geom_text(aes(label = sum_hit))
test_mutate_graph

#
test_mutate <- raw_df %>%
  group_by(Family) %>% 
  summarize(Sum_hits = rowSums(select(., SRR1:SRR10))) %>% 
  ggplot(aes(x = Family, y = count)) + # reorder
  geom_point() + 
  coord_flip() # flip the two axes



#### Automating exploratory plot [using ggplot and purrr]

# Setting variables
tax = names(raw_df)[1:8]
tax

tax = set_names(tax)

sample = names(raw_df)[8:18]
sample

sample = set_names(sample)


scatter_fun <- function(x, y) {
  ggplot(raw_df, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point()
}  

scatter_fun("Family", "SRR1")


family_plots = map(sample, ~scatter_fun(.x, "Family"))

family_plots


all_plots <- map(sample,
                 ~map(tax, scatter_fun, y = .x))
all_plots

cowplot::plot_grid(plotlist = all_plots[[1]])

pdf("all_plots.pdf")
all_plots
dev.off()






    complete_dir = sort(list.files(path = path, pattern = "fastq.gz")),
    fnFs = sort(list.files(path, pattern= "1.fastq.gz", full.names = TRUE)),
    fnRs = sort(list.files(path, pattern= "2.fastq.gz", full.names = TRUE)),
    complete_sample_name = sapply(strsplit(basename(fnFs), "_"), `[`, 1),
    df_location = as.data.frame(complete_dir) %>% 
      set_colnames("location"),
    df_samples = sapply(strsplit(basename(complete_dir), "_"), `[`, 1) %>% 
      as.data.frame() %>% 
      set_colnames("sample"),
    df = cbind(df_samples, df_location),
    filtFs = file.path(path, "filtered", paste0(complete_sample_name, "_F_filt.fastq.gz")), 
    filtRs = file.path(path, "filtered", paste0(complete_sample_name, "_R_filt.fastq.gz")),
    errF = learnErrors(filtFs, multithread=TRUE)
  )