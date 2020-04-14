# Functions

#1 To create a plot with taxonomy on the X-axis and data for a single sample on the Y-axis
group_plot <- function(tax_named, sample_named) {
  ggplot(data = raw_df,
         aes (x = .data[[tax_named]],
              y = .data[[sample_named]])) +
    geom_point() +
    geom_text(aes(label = .data[[sample_named]],
                  hjust = "top",
                  vjust = "top"),
              position = "identity",
              check_overlap = TRUE)
} 
# To test: group_plot("Family", "SRR1")
# This scatter_fun has an error, which might need to be resolved to have consistent data
# scatter_fun = function(x, y) {
#ggplot(raw_df, aes_string(x = .data[[tax_named]],
#                          y = .data[[sample_named]]) ) +
#  geom_point()
#}

#2 To pull data for a single sample and total hits to help create contrast between different sample sets
sample_specific <- function(samp) {
  sample_analysis <- sum_hits %>% 
    select(Phylum, Class, Order, Family, Genus, Species, samp, sum_hit)
}  
# To test: single_sample_analysis <- sample_specific("SRR1")

#3 To create a plot for a single sample set vs all the taxonomy levels
sample_tax_plot <- function(sample_name) {
  map(tax_named, ~group_plot(.x, sample_name))
}
# To test:sample_tax_plot("SRR3")

#4 To create a plot for a single taxonomy level vs all the sample
tax_sample_plot <- function(sample_name) {
  map(sample_named, ~group_plot("Family", .x))
}
# To test: tax_sample_plot("Family")

#5 To create a graph of all tax vs samples
#scatter_fun <- function(x, y) {
#  ggplot(raw_df, aes(x = .data[[x]], y = .data[[y]]) ) +
#    geom_point()
}  
# To test: scatter_fun("Family", "SRR1")
# Or ALL: all_plots  <-  map(sample_named, ~map(tax_named, scatter_fun, y = .x) )