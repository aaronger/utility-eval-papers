source(here(codepath,"setup.R"))

score_viz <- tribble(
  ~palette,  ~linetypes, ~model,
  "#004488", "dashed",   "BPagano-RtDriven",
  "#117733", "dotdash",  "COVIDhub-ensemble",
  "#694A49", "twodash",  "CU-select",
  "#88CCEE", "dashed",   "IHME-CurveFit",
  "#4A708B", "solid",  "JHUAPL-Bucky",
  "#999933", "longdash", "JHUAPL-Gecko",
  "#CD2626", "solid",    "MUNI-ARIMA",
  "#882255", "longdash", "USC-SI_kJalpha",
  "#FF8C00", "twodash",    "UVA-Ensemble",
  "#000000", "solid",    "COVIDhub-baseline"
)

score_palette <- deframe(score_viz[c("model", "palette")])
score_linetypes <- deframe(score_viz[c("model", "linetypes")])

peak_day <- bind_rows(slim_dfs) %>% filter(reference_date == "2021-12-27")

three_days <- bind_rows(slim_dfs) %>% filter(reference_date %in% forecast_dates[13:15])

plot_scores_slim(peak_day)
plot_scores_slim(three_days) + lims(x=c(500, 30000))

p_peak <- plot_scores_slim(peak_day, palette = score_palette, linetypes = score_linetypes) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(5000,40000))

p_peak_out <- p_peak + 
  annotate(geom = "text", x = 22000, y = -250, size = 3,
           label = paste("Total hospitalizations on 2022-01-10 →"), hjust = 1) +
  labs(
    x = expression(paste("Total Allocation Constraint,  ", italic("K"))),
    y = TeX("Allocation Score = $\\sum \\max (y_i - x_i^F, 0) - \\max (\\sum y_i - K,0)$")) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

p_peak_out_min <- p_peak + theme_bw() + 
  theme(legend.position = "none")

ggsave(plot = p_peak_out, filename = "peak_alloscore.png",
       width = 8, height = 6, path = save_dir, dpi = 600)

ggsave(plot = p_peak_out_min, filename = "peak_alloscore_min.png",
       width = 6, height = 6, path = save_dir)

# plot score time series

p_K10k <- plot_scores_slim(bind_rows(slim_dfs), Ks = 10000)
p_K20k <- plot_scores_slim(bind_rows(slim_dfs), K = 20000)
p_K4k <- plot_scores_slim(bind_rows(slim_dfs), K = 4000)
p_K10k
p_K20k
p_K4k

y_nats <- truth_data %>% filter(location == "US") %>%
  mutate(reference_date = target_end_date - 14) %>%
  filter(reference_date %in% as.Date(forecast_dates)) %>% pull(value)

# now score everybody at these KS

# or approximate
y_nat_approx <- round(y_nats / 200) * 200

p_ytot <- bind_rows(slim_dfs) %>%
  plot_scores_slim(Ks = y_nat_approx, ts = TRUE, ts_dates = forecast_dates[11:13])
p_ytot + theme_classic()

+ scale_x_date(date_labels = "%b %y")

p_ytot +lims(x=c(as.Date("2021-12-27"), as.Date("2021-12-27") + 14)) + theme_classic()


# wis scores

tar_load(score_data)
wis <- score_data %>% align_forecasts() %>%
  left_join(hub_locations %>% select(location = fips, population), by = c("location")) %>%
  filter(model %in% mkeep) %>%
  select(model, location, reference_date, wis, population)

wis_sum <-  wis %>%
  group_by(model, reference_date) %>% mutate(
    ave_wis = mean(wis, na.rm = TRUE),
    pop_ave_wis = weighted.mean(wis, population, na.rm = TRUE),
    sum_wis = sum(wis, na.rm = TRUE)) %>%
  summarise(
    sum_wis = unique(sum_wis),
    ave_wis = unique(ave_wis),
    pop_ave_wis = unique(pop_ave_wis)) %>% arrange(reference_date)

wis_sum %>% filter(reference_date == as.Date("2021-12-27"))


wis_sum_ts <- expand_grid(model = mkeep, reference_date = as.Date(forecast_dates)) %>%
  left_join(wis_sum, by = c("model", "reference_date"))

p_wis_ts <- wis_sum_ts %>%
  ggplot(aes(x=reference_date, y=sum_wis, color = model, linetype = model)) +
  geom_line() + geom_point() +
  scale_x_date(breaks = as.Date(forecast_dates)) +
  scale_color_manual(values = score_palette) +
  scale_linetype_manual(values = score_linetypes)

# contrast alloscore and WIS
library(patchwork)
p1 <- p_wis_ts +
  lims(x= as.Date(forecast_dates[c(11,13)])) +
  labs(x = NULL, y = NULL) +
  ggtitle("Sum of State-level WISs") + theme_classic()
p2 <- p_ytot +
  labs(x = NULL, y = NULL) +
  ggtitle("Allocation Score") + theme_classic()
p_wis_v_alloscore <- p1 + p2 + plot_layout(guides = "collect")

ggsave(plot = p_wis_v_alloscore, filename = "p_wis_v_alloscore.png",
       width = 10, height = 5, path = save_dir)

