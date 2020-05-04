
source("R/02_utils.R")
source("R/08_import_icu_network.R")

# Local network characteristics -------------------------------------------

type_label <- c("Expected Influence", "Strength")

#Node centrality meansures

  boot_filtered <- grouped_results_boot  %>%
      filter(type %in% c("expectedInfluence", "strength")) %>%
        group_by(multiverse_id, type, id) %>%
       summarize(med = median(value, na.rm = TRUE),
                 CI_low = med - 2*sd(value, na.rm = TRUE),
                 CI_up = med + 2*sd(value, na.rm = TRUE)) %>%
        group_by(type, id) %>%
           summarize(med_med = median(med, na.rm = TRUE),
                 CI_low = median(CI_low, na.rm = TRUE),
                 CI_up = median(CI_up, na.rm = TRUE)) %>% ungroup() %>%
            left_join(lpe_df) %>%
              mutate(type = fct_recode(type,
                                       "Expected Influence" = "expectedInfluence",
                                       "Strength" = "strength"))


  node_plot <- boot_filtered %>% ggplot(aes(x = med_med,
                                            y = fct_rev(id),
                                            color = factor,
                                            shape = factor)) +
                                   geom_point(size = 3) +
                                   scale_shape_manual(values = c(15, 16, 17, 18))+
                                   geom_errorbar(aes(xmin = CI_low,
                                                     xmax = CI_up),
                                                 width = .2)+
                                              #   position = position_dodge(0.001)) +
                                   facet_grid(cols = vars(type)) +
        theme_classic()+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white",
                                              colour = "black"),
              panel.border = element_rect(fill = NA,
                                          colour = "white"),
              strip.background = element_blank(),
              strip.text.x = element_text(size = 12,
                                          color = "black",
                                          face = "bold"),
              panel.grid.major = element_line(color = "#e0e0e0"),
              panel.grid.minor = element_blank())+
              scale_x_continuous(breaks = seq(0, 1.6, 0.1),
                                 labels = c("", "", "0.2", "", "0.4", "", "0.6", "", "0.8",
                                            "", "1", "", "1.2", "", "1.4", "", "1.6")) +
              scale_y_discrete(sec_axis(~.))+
         labs(shape = "Factor",
              color = "Factor")



# Meso-scale network characteristics --------------------------------------


  meso_plot_median_df <- meso_plot_df %>%
                          group_by(multiverse_id, type, id) %>%
                            summarize(med = median(value, na.rm = TRUE),
                                     CI_low = med - 2*sd(value, na.rm = TRUE),
                                     CI_up = med + 2*sd(value, na.rm = TRUE)) %>%
                          group_by(type, id) %>%
                             summarize(med_med = median(med, na.rm = TRUE),
                                   CI_low = median(CI_low, na.rm = TRUE),
                                   CI_up = median(CI_up, na.rm = TRUE)) %>% ungroup() %>%
                              left_join(lpe_df)


  meso_plot <- meso_plot_median_df %>% ggplot(aes(x = med_med,
                                              y = fct_rev(id),
                                              color = factor,
                                              shape = factor)) +
                                   geom_point(size = 3) +
                                   scale_shape_manual(values = c(15, 16, 17, 18))+
                                   geom_errorbar(aes(xmin = CI_low,
                                                     xmax = CI_up),
                                                 width = .2)+
                                              #   position = position_dodge(0.001)) +
                                   facet_grid(cols = vars(type)) +
        theme_classic()+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white",
                                              colour = "black"),
              panel.border = element_rect(fill = NA,
                                          colour = "white"),
              strip.background = element_blank(),
              strip.text.x = element_text(size = 12,
                                          color = "black",
                                          face = "bold"),
              panel.grid.major = element_line(color = "#e0e0e0"),
              panel.grid.minor = element_blank())+
              scale_x_continuous(breaks = seq(0, 1.2, 0.1),
                                 labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1", "", "1.2")) +
              scale_y_discrete(sec_axis(~.))+
         labs(shape = "Factor",
              color = "Factor")
