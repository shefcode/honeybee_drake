source("package.R")
source("functions.R")

bee_analysis <- drake_plan(
  raw_df = read.csv(here("../honeybee_data.csv"),
                    header = TRUE),
  sum_hits = raw_df %>% 
    mutate(sum_hit = rowSums(select(., SRR1:SRR10))),
  tax = names(raw_df)[1:8],
  tax_named = tax[1:8] %>%
    purrr::set_names(tax),
  sample = names(raw_df)[9:18],
  sample_named = sample[1:10] %>% 
    purrr::set_names(sample)
)
  all_plot = map(sample_named,
                 ~map(tax_named, scatter_fun, y = .x))
)

make(bee_analysis)
loadd(raw_df, sum_hits, tax, tax_named, sample, sample_named)
      all_plot)

scatter_fun <- function(x, y) {
  ggplot(raw_df, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point()
}  
all_plots  <-  map(sample_named, ~map(tax_named, scatter_fun, y = .x))
all_plots

sample_cowplot <- function(x){
  cowplot::plot_grid(plotlist = all_plots[[x]])
}

sample_cowplot(2)
#### As a function: Part to incorporate in drake [Can remove after completing it]
cowplot::plot_grid(plotlist = all_plots[[3]])

pdf("all_plots.pdf")
all_plots
dev.off()
############################################################


# Simple graph for set variable
raw_df %>% 
  ggplot(aes(x = Family, y = SRR1)) +
  geom_point()

### Simple data analysis set up [To be included in the drake in the future]

raw_df <- read.csv(here("../honeybee_data.csv"))
names(raw_df)[1] <- "Domain"

df <-  raw_df %>%
  select(ï..Domain) %>% 
  set_colnames(c("Domain"))

# Creates a column with the sum of the number of hits in total
test_mutate <- raw_df %>% 
  mutate(sum_hit = rowSums(select(., SRR1:SRR10)))

# Create a gpplot to show the number of hits in each sample [to help with the visualization of the data] 
test_mutate_graph <- raw_df %>% 
  mutate(sum_hit = rowSums(select(., SRR1:SRR10))) %>% 
  ggplot(aes(x = Family, y = sum_hit)) +
  geom_point() +
  geom_text(aes(label = sum_hit))
test_mutate_graph

# Error
test_mutate <- raw_df %>%
  group_by(Family) %>% 
  summarize(Sum_hits = rowSums(select(., SRR1:SRR10))) %>% 
  ggplot(aes(x = Family, y = count)) + # reorder
  geom_point() + 
  coord_flip() # flip the two axes