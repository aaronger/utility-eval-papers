library(targets)
library(tidyverse)
library(ggbump)

theme_set(theme_bw())

tar_load(starts_with("alloscore"))

alloscore_data <- do.call(rbind, mget(ls()))

alloscore_data_summaries <- alloscore_data |>
  mutate(Kdiff = abs(ytot-K)) |>
  group_by(forecast_date) |>
  filter(Kdiff == min(Kdiff))|>
  mutate(nmodels = n(),
         allo_rank = rank(value, ties.method = "random"),
         allo_rank_std = 1- (allo_rank-1)/(nmodels-1)) |>
  ungroup() |>
  complete(model, forecast_date)

p1 <- ggplot(alloscore_data_summaries,
       aes(x=as.Date(forecast_date), y=allo_rank_std, color=model)) +
  geom_bump() +
  xlab(NULL)
p2 <- ggplot(alloscore_data_summaries,
       aes(x=as.Date(forecast_date), y=value, color=model)) +
  geom_point() + geom_line()+
  xlab(NULL)


tar_load("score_data")

alloscore_models <- unique(alloscore_data$model)
score_data_summaries <- score_data |>
  mutate(reference_date = target_end_date - 14) |>
  filter(model %in% alloscore_models) |>
  group_by(model, reference_date) |>
  summarize(mwis = mean(wis)) |>
  ungroup() |>
  group_by(reference_date) |>
  mutate(nmodels = n(),
         mwis_rank = rank(mwis, ties.method = "random"),
         mwis_rank_std = 1- (mwis_rank-1)/(nmodels-1)) |>
  ungroup() |>
  complete(model, reference_date)

p3 <- ggplot(score_data_summaries,
       aes(x=as.Date(reference_date), y=mwis_rank_std, color=model)) +
  geom_bump() +
  xlab(NULL)

p4 <- ggplot(score_data_summaries,
       aes(x=as.Date(reference_date), y=mwis, color=model)) +
  geom_point() + geom_line() +
  xlab(NULL)

gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

all_data <- left_join(alloscore_data_summaries |> mutate(forecast_date = as.Date(forecast_date)),
                      score_data_summaries |> rename(forecast_date = reference_date))

ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=model)) +
  geom_point() + xlim(0,1) + ylim(0,1) +
  scale_color_brewer(palette="Paired")

## faceted by model, colored by model
ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=model)) +
  geom_point() + xlim(0,1) + ylim(0,1) +
  scale_color_brewer(palette="Paired") +
  facet_wrap(.~model) +
  geom_abline(slope=1, intercept=0, linetype=2, color="gray") +
  theme(legend.position = "none")

## faceted by model, colored by date
ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=forecast_date)) +
  geom_point() + xlim(0,1) + ylim(0,1) +
  facet_wrap(.~model) +
  geom_abline(slope=1, intercept=0, linetype=2, color="gray")

## faceted by model, colored by ytot
ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=ytot)) +
  geom_point(alpha=.5) + xlim(0,1) + ylim(0,1) +
  facet_wrap(.~model) +
  geom_abline(slope=1, intercept=0, linetype=2, color="gray")

ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=forecast_date, size=ytot)) +
  geom_point(alpha=.8) + xlim(0,1) + ylim(0,1) +
  facet_wrap(.~model) +
  geom_abline(slope=1, intercept=0, linetype=2, color="gray")


ggplot(all_data, aes(x=mwis_rank_std, y=allo_rank_std, color=model)) +
  geom_density_2d() +
  scale_color_brewer(palette="Paired") +
  facet_wrap(.~model)
