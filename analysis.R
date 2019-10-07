library(ggplot2)
library(tidyverse)
library(ebbr)
library(theme538)

### SQL to generate the data from nfldb. https://github.com/BurntSushi/nfldb

# DROP TABLE IF EXISTS __defense_study;
# CREATE TABLE __defense_study AS
# SELECT a.gsis_id,
# c.season_year as season,
# c.week, a.passing_att,
# a.passing_cmp_air_yds,
# a.passing_incmp_air_yds,
# a.receiving_yac_yds,
# a.receiving_yds,
# a.receiving_rec,
# a.receiving_tds,
# a.rushing_yds,
# a.rushing_att,
# a.rushing_tds,
#
# CASE
# WHEN b.pos_team = c.home_team THEN c.away_team
# ELSE c.home_team
# END AS defense,
# CASE
# WHEN b.pos_team = c.home_team THEN c.home_team
# ELSE c.away_team
# END AS opponent
# FROM agg_play a
# INNER JOIN drive b ON a.drive_id = b.drive_id AND a.gsis_id = b.gsis_id
# INNER JOIN game c ON a.gsis_id = c.gsis_id
# WHERE c.season_type = 'Regular' AND c.season_year = 2019;

week_filter = 5

# Read the file
raw <- read_csv('data/__defense_study.csv') %>%
   filter(season == 2019,
          week <= week_filter)

# group and summarize
grouped <- raw %>%
   group_by(defense) %>%
   summarize(att = sum(passing_att),
             rec = sum(receiving_rec))

# Create an empirical beta prior. Ebbr available from: https://github.com/dgrtwo/ebbr
prior <- grouped %>%
   ebb_fit_prior(rec, att)

# Now fit each player's actual results to the prior
fitted <- augment(prior, data = grouped) %>%
   arrange(-.fitted)

# Set up the plot labels
chart_title <- paste("Through week", week_filter, "2019, prior to Monday Night Football")
fitted$att <- paste(fitted$defense, " (", fitted$rec, "/", fitted$att, ")", sep="")

# Graph the plot
fitted %>%
   top_n(n = 32, wt = .fitted) %>%
   mutate(name = reorder(att, .fitted)) %>%
   ggplot(aes(x = .fitted, y = name)) +
   geom_point() +
   geom_point(aes(.raw, color='red')) +
   geom_errorbarh(aes(xmin = .low, xmax = .high)) +
   labs(x = "Completion % against\nTrue in black w/ 95% Credible Interval. Observed in red.",
        y = "",
        title = "New England really does have\na better defense than Washington",
        subtitle = chart_title) +
   theme_538 +
   theme(legend.position="none") +
   # # Plot margins
   theme(plot.margin = unit(c(1, 1, .1, .65), "cm")) +
   scale_x_continuous(minor_breaks=0,breaks=seq(0.5,0.8,.05),limits=c(0.5,0.8))
