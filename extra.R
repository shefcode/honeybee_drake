# Outdated/Extra

# A function has been created for the following code
test_plot <- ggplot(data = raw_df,
                    aes(x = Family, y = SRR2)) +
  geom_point()
test_plot

# A function NEEDS to be created for the following code!
# Provides insight to the number of hits and the number of the times it shows up in a sample set
test_bar <- ggplot(data = raw_df,
                   aes(x = Family)) +
  geom_bar(aes(fill = SRR2))
test_bar
