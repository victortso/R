# Create the 4 graphs that represent Electricity, Gas, Water usage.
# The last one is total amount $ spent
# 3/11/2018 - tso First Revision
#

# Plot
# color plattete 
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

create_usage_graphs <- function(df) {
  # Electricity
  plot_e <-
    ggplot(data = billing_df,
           mapping = aes(x = DATE, y = `ELECTRIC KWHS PER DAY`)) +
    geom_line(size = 1, color = "blue") +
    ggtitle("Monthly Electric Usage") +
    labs(x = "Time", y = "Electricity KWHS") +
    theme(
      axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
      axis.title.y = element_text(color = "slateblue4", size = 14, face =
                                    "bold")
    )
  
  # Gas Usage
  plot_g <-
    ggplot(data = billing_df,
           mapping = aes(x = DATE, y = `GAS THERMS PER DAY`)) +
    geom_line(size = 1, color = "indianred4") +
    ggtitle("Monthly Gas Usage") +
    labs(x = "Time", y = "Gas THERMS") +
    theme(
      axis.title.x = element_text(color = "firebrick3", size = 14, face = "bold"),
      axis.title.y = element_text(color = "firebrick3", size = 14, face =
                                    "bold")
    )
  
  # Water Usage
  plot_w <-
    ggplot(data = billing_df,
           mapping = aes(x = DATE, y = `WATER GAL PER DAY`)) +
    geom_line(size = 1, color = "springgreen4") +
    ggtitle("Monthly Water Usage") +
    labs(x = "Time", y = "Water GAL") +
    theme(
      axis.title.x = element_text(color = "green3", size = 14, face = "bold"),
      axis.title.y = element_text(color = "green3", size = 14, face = "bold")
    )
  
  # Monthly Billing $
  plot_amount <-
    ggplot(data = billing_df, mapping = aes(x = DATE, y = `BILLING`)) +
    geom_line(size = 1, color = "goldenrod3") +
    ggtitle("Billing in Dollar") +
    labs(x = "Time", y = "$") +
    theme(
      axis.title.x = element_text(color = "gold2", size = 14, face = "bold"),
      axis.title.y = element_text(color = "gold2", size = 14, face = "bold")
    )
  
  plots <- list()
  plots$plot_e <- plot_e
  plots$plot_g <- plot_g
  plots$plot_w <- plot_w
  plots$plot_amount <- plot_amount
  
  return (plots)
}