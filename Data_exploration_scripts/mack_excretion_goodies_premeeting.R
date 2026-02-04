# Script Details ----------------------------------------------------------
###project: LTER SPARC Consumer Functional Diversity
###author(s): MW
###goal(s): generate trait key for my assigned datasets
###date(s): October 2025
###note(s): 

# Housekeeping ------------------------------------------------------------
### load necessary libraries ----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, scales, broom, purrr, dataRetrieval,
                 splitstackshape, forcats)

### define custom functions ----
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

dat <- read_csv('04_harmonized_consumer_excretion_sparc_cnd_site.csv')
glimpse(dat)
nacheck(dat)

dat1 <- dat |> 
      mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
             subsite_level2 = replace_na(subsite_level2, "Not Available"),
             subsite_level3 = replace_na(subsite_level3, "Not Available")) |> 
      select(project, year, month, habitat, temp_c, site, subsite_level1, subsite_level2, subsite_level3,
             scientific_name, diet_cat, nind_ug.hr, pind_ug.hr, count_num, density_num.m2, density_num.m3, 
             biomass_g, dmperind_g.ind,
             kingdom, phylum, class, order, family, genus)
glimpse(dat1)
nacheck(dat1)

dt_total <- dat1 |> 
      group_by(project, habitat, year, month, 
               site, subsite_level1, subsite_level2, subsite_level3) |> 
      summarize(
            ### calculate total nitrogen supply at each sampling unit and then sum to get column with all totals
            total_nitrogen.m2 = sum(nind_ug.hr * density_num.m2, na.rm = TRUE),
            total_nitrogen.m3 = sum(nind_ug.hr * density_num.m3, na.rm = TRUE),
            ### create column with total_nitrogen contribution for each program, regardless of units
            total_nitrogen = sum(total_nitrogen.m2 + total_nitrogen.m3, na.rm = TRUE),
            ### calculate total phosphorus supply at each sampling unit and then sum to get column with all totals
            total_phosphorus.m2 = sum(pind_ug.hr * density_num.m2, na.rm = TRUE),
            total_phosphorus.m3 = sum(pind_ug.hr * density_num.m3, na.rm = TRUE),
            ### create column with total_phosphorus contribution for each program, regardless of units
            total_phosphorus = sum(total_phosphorus.m2 + total_phosphorus.m3, na.rm = TRUE),
            ### calculate total biomass at each sampling unit and then sum to get column with all totals
            total_bm.m2 = sum(dmperind_g.ind*density_num.m2, na.rm = TRUE),
            total_bm.m3 = sum(dmperind_g.ind*density_num.m3, na.rm = TRUE),
            ### create column with total_biomass for each program, regardless of units
            total_biomass = sum(total_bm.m2 + total_bm.m3, na.rm = TRUE)) |> 
      ungroup() |>
      arrange(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3)
nacheck(dt_total)

dt_total |> 
      mutate(project = as.factor(project)) |> 
      filter(!project %in% c('Arctic', 'FCE')) |> 
      group_by(project, year) |> 
      summarize(
            mean = mean(total_nitrogen + 1, na.rm = TRUE),
            median = median(total_nitrogen + 1, na.rm = TRUE),
            .groups = 'drop'
      ) |> 
      mutate(
            project = fct_reorder(project, median)
      ) |> 
      ggplot(aes(x = project, y = mean)) + 
      geom_jitter(aes(fill = project), shape = 21, width = 0.3,
                  color ='black', size = 2, stroke = 1, alpha = 0.6) +
      geom_boxplot(aes(fill = project), position = position_dodge(0.8),
                   outlier.shape = NA, alpha = 1, size = 1, color = 'black') +
      scale_y_log10(
            breaks = 10^(-2:6),
            labels = label_number(big.mark = ",")
      ) +
      labs(x = 'Project', y = 'Nitrogen Supply (ug/hr/area)',
           title = 'Mean Annual Community Nitrogen Supply') + 
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", colour = "black",
                                      hjust = 0.5)
      )

dt_total |> 
      mutate(project = as.factor(project)) |> 
      filter(!project %in% c('Arctic', 'FCE')) |> 
      group_by(project, year) |> 
      summarize(
            mean = mean(total_phosphorus + 1, na.rm = TRUE),
            median = median(total_phosphorus + 1, na.rm = TRUE),
            .groups = 'drop'
      ) |> 
      mutate(
            project = fct_reorder(project, median)
      ) |> 
      ggplot(aes(x = project, y = mean)) + 
      geom_jitter(aes(fill = project), shape = 21, width = 0.3,
                  color ='black', size = 2, stroke = 1, alpha = 0.6) +
      geom_boxplot(aes(fill = project), position = position_dodge(0.8),
                   outlier.shape = NA, alpha = 1, size = 1, color = 'black') +
      scale_y_log10(
            breaks = 10^(-2:6),
            labels = label_number(big.mark = ",")
      ) +
      labs(x = 'Project', y = 'Phosphorus Supply (ug/hr/area)',
           title = 'Mean Annual Community Phosphorus Supply') + 
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", colour = "black",
                                      hjust = 0.5)
      )

dt_total |> 
      mutate(project = as.factor(project)) |> 
      filter(!project %in% c('Arctic', 'FCE')) |> 
      group_by(project, year) |> 
      summarize(
            mean = mean(total_biomass + 1, na.rm = TRUE),
            median = median(total_biomass + 1, na.rm = TRUE),
            .groups = 'drop'
      ) |> 
      mutate(
            project = fct_reorder(project, median)
      ) |> 
      ggplot(aes(x = project, y = mean)) + 
      geom_jitter(aes(fill = project), shape = 21, width = 0.3,
                  color ='black', size = 2, stroke = 1, alpha = 0.6) +
      geom_boxplot(aes(fill = project), position = position_dodge(0.8),
                   outlier.shape = NA, alpha = 1, size = 1, color = 'black') +
      scale_y_log10(
            breaks = 10^(-2:6),
            labels = label_number(big.mark = ",")
      ) +
      labs(x = 'Project', y = 'Biomass (g/area)',
           title = 'Mean Annual Community Biomass') + 
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", colour = "black",
                                      hjust = 0.5)
      )


systems <- dat |> 
      select(project, habitat, site, subsite_level1, subsite_level2, subsite_level3) |> 
      distinct()

dt_total |> 
      mutate(project = as.factor(project)) |> 
      filter(!project %in% c('Arctic', 'FCE')) |> 
      mutate(
            system = case_when(
                  project == 'SBC' & habitat == 'beach' ~ site,
                  project == 'SBC' & habitat == 'ocean' ~ site,
                  project == 'Palmer' ~ site,
                  project == 'CCE' ~ site,
                  project == 'NGA' ~ subsite_level1,
                  project == 'NorthLakes' ~ site,
                  project == 'VCR' ~ site,
                  project == 'PIE' ~ site,
                  project == 'MCR' ~ subsite_level1,
                  project == 'CoastalCA' & site == 'CENTRAL' ~ subsite_level1,
                  project == 'CoastalCA' & site == 'SOUTH' ~ subsite_level1,
                  project == 'FISHGLOB' ~ site,
                  project == 'RLS' ~ site,
                  
            )
      ) |> 
      group_by(project, system, year) |> 
      summarize(
            mean = mean(total_nitrogen + 1, na.rm = TRUE),
            median = median(total_nitrogen + 1, na.rm = TRUE),
            .groups = 'drop'
      ) |> 
      # filter(project == 'MCR') |> 
      ggplot(aes(x = year, y = mean, fill = system, color = system, group = system)) + 
      geom_jitter(aes(fill = system), shape = 21, width = 0.3,
                  color ='black', size = 2, stroke = 1, alpha = 0.6) +
      geom_smooth(method = 'loess') +
      # geom_boxplot(aes(fill = project), position = position_dodge(0.8),
      #              outlier.shape = NA, alpha = 1, size = 1, color = 'black') +
      # scale_y_log10(
      #       breaks = 10^(-2:6),
      #       labels = label_number(big.mark = ",")
      # ) +
      facet_wrap(~project, scale = 'free') + 
      labs(x = 'Project', y = 'Nitrogen Supply (ug/hr/area)',
           title = 'Mean Annual Community Nitrogen Supply by Site') + 
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", colour = "black",
                                      hjust = 0.5)
      )

# Questions that need addressed -------------------------------------------
# 1. No estimate for FCE... seems that all the m2 and m3 sites were integrated with sampling area in the harmonization process
# 2. No estimate for the Arctic... seems that it is likely missing the body size information to estimate what we need here
# 3. Palmer estimates exist for N cycling, but appear to be crazy low
# 4. Appears that estimates for RLS and maybe FISHGLOB are not at the m2 resolution?

