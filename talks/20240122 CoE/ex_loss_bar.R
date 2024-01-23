library(ggplot2)
library(dplyr)

# example 1

ex1_data <- data.frame(
    location = c("", "", "", ""),
    good_bad = c("good", "good", "bad", "bad"),
    name = factor(
      c("purchase quantity", "need: met", "need: unmet", "purchase quantity"),
      levels = c("purchase quantity", "need: met", "need: unmet")),
    value = c(5, 5, 10, 0))

alloc_plot <- ggplot(mapping = aes(x=location, y=value, fill=name)) +
  geom_col(
    data = ex1_data |>
      dplyr::filter(good_bad == "bad"),
    position = "dodge"
  ) +
  geom_col(
    data = ex1_data |>
      dplyr::filter(good_bad == "good"),
    position = "dodge"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c("purchase quantity",
               "need: unmet", "need: met"),
    values = c("#abd9e9", "#d7191c", "#fdae61")) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  scale_x_discrete(
    labels = c("purchased\nneeded")
  ) +
  ylab("Resource level") +
  xlab(NULL) +
  # geom_col(position="identity") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf('talks/20240122 CoE/ex1_loss_bar.pdf', width = 4.4, height = 4)
print(alloc_plot)
dev.off()



# example 3
ex3_data <- tidyr::expand_grid(
  location = c("1", "2"),
  prov_need = c("allocation", "need"),
  good_bad = c("good", "bad")
) |>
  dplyr::mutate(
    name = paste0(
      prov_need,
      ": ",
      ifelse(
        prov_need == "allocation",
        ifelse(good_bad == "good", "used", "unused"),
        ifelse(good_bad == "good", "met", "unmet")
      )
    ),
    value = c(1, 2, 1, 1,
              8, 8, 8, 10)
  )


alloc_plot <- ggplot(mapping = aes(x=location, y=value, fill=name)) +
  geom_col(
    data = ex3_data |>
      dplyr::filter(good_bad == "bad"),
    position = "dodge"
  ) +
  geom_col(
    data = ex3_data |>
      dplyr::filter(good_bad == "good"),
    position = "dodge"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c("allocation: unused", "allocation: used",
               "need: unmet", "need: met"),
    values = c("#2c7bb6", "#abd9e9", "#d7191c", "#fdae61")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_discrete(
    labels = c("allocation\nneed", "allocation\nneed")
  ) +
  ylab("Resource level") +
  xlab("Location") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

alloc_plot


pdf('talks/20240122 CoE/ex3_loss_bar.pdf', width = 5, height = 4)
print(alloc_plot)
dev.off()

