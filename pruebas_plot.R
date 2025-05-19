library(ggplot2)
library(dplyr)

# Ensure proper order of groups
AVdata <- AVdata %>%
  mutate(
    group = interaction(trat, clip, sep = "_"),
    group = factor(group, levels = c("ex_no", "ex_yes", "gra_no", "gra_yes"))  # Set order explicitly
  )


#  Option 1: Violin + Jitter (Your Base Plot, Improved Order)
ggplot(AVdata, aes(x = group, y = LDMC, color = trat, shape = clip)) +
  geom_violin(aes(fill = trat), alpha = 0.4, color = NA) +
  geom_jitter(width = 0.1, alpha = 0.8, size = 2) +
  labs(title = "AV LDMC: Violin Plot",
       x = "Treatment + Clipping",
       y = "LDMC",
       color = "Tratamiento",
       shape = "Clipping",
       fill = "Treatment") +
  theme_minimal()


# Option 2: Boxplots + Jitter
ggplot(AVdata, aes(x = group, y = LDMC, color = trat)) +
  geom_boxplot(aes(fill = trat), alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(shape = clip), width = 0.1, size = 2, alpha = 0.8) +
  labs(title = "AV LDMC: Boxplot with Jitter",
       x = "Treatment + Clipping",
       y = "LDMC") +
  theme_minimal()


#  Option 3: Mean ± Standard Error
ggplot(ggline_data, aes(x = group, y = mean, color = trat, shape = clip, group = interaction(trat, clip))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line() +
  labs(title = "AV LDMC: Mean ± SE",
       x = "Treatment + Clipping",
       y = "Mean LDMC ± SE") +
  theme_minimal()




#OVERLAPPING VIOLINPLOT
ggplot(AVdata, aes(x = trat, y = LDMC, fill = clip, color = clip)) +
  geom_violin(position = position_identity(), alpha = 0.4) +
  geom_jitter(position = position_jitter(width = 0.1),
              size = 2, alpha = 0.7) +
  labs(title = "AV LDMC: Overlapping Violins by Clipping",
       x = "Treatment",
       y = "LDMC",
       fill = "Clipping",
       color = "Clipping") +
  theme_minimal()

# OVERLAPPING BOXPLOT
ggplot(AVdata, aes(x = trat, y = LDMC, fill = clip, color = clip)) +
  geom_boxplot(position = position_identity(), alpha = 0.4, width = 0.5) +
  geom_jitter(position = position_jitter(width = 0.1),
              size = 2, alpha = 0.7) +
  labs(title = "AV LDMC: Overlapping Boxplots by Clipping",
       x = "Treatment",
       y = "LDMC",
       fill = "Clipping",
       color = "Clipping") +
  theme_minimal()


# Define your color palette
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  # Violin or boxplot with different linetypes for clip
  geom_violin(aes(color = trat, linetype = clip, group = interaction(trat, clip)),
               fill = NA, position = position_identity(), width = 0.5, size = 1) +
  
  
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = c("no" = "solid", "yes" = "dashed")) +
  scale_shape_manual(values = c("no" = 16, "yes" = 17)) +  # Change if you want other shapes
  
  labs(title = "AV LDMC: ex vs gra with Clipping Distinction",
       x = "Treatment",
       y = "LDMC",
       color = "Treatment",
       shape = "Clipping",
       linetype = "Clipping") +
  
  theme_minimal()



library(ggplot2)
library(dplyr)

# Custom treatment colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")
# Lighter versions for clip = yes
lighter_colors <- c("ex" = "#66C266", "gra" = "#FF99F9")

# Prepare color & fill mappings
AVdata <- AVdata %>%
  mutate(
    outline_color = ifelse(clip == "no", trat, NA),
    fill_color = ifelse(clip == "yes", trat, NA)
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  # Boxplot layer for clip = no (solid line, no fill)
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(color = trat),
    fill = NA, position = position_identity(), width = 0.5, size = 1
  ) +
  
  # Boxplot layer for clip = yes (no border, light fill)
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(fill = trat),
    color = NA, position = position_identity(), width = 0.5, alpha = 0.5
  ) +
  
  
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = lighter_colors) +
  scale_shape_manual(values = c("no" = 16, "yes" = 17)) +
  
  labs(title = "AV LDMC: ex vs gra with Clipping Distinction",
       x = "Treatment",
       y = "LDMC",
       color = "Treatment",
       fill = "Treatment (clip = yes)",
       shape = "Clipping") +
  
  theme_minimal()







library(ggplot2)
library(dplyr)

# Custom treatment colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")
# Lighter versions for clip = yes (used for both fill and outline)
lighter_colors <- c("ex" = "#66C266", "gra" = "#FF99F9")

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  # Boxplot for clip = no: dark outline, no fill
  geom_boxplot(
    data = filter(AVdata, clip == "no"),
    aes(color = trat),
    fill = NA,
    position = position_identity(),
    width = 0.5,
    size = 1
  ) +
  
  # Boxplot for clip = yes: light outline and light fill
  geom_boxplot(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat, fill = trat),
    position = position_identity(),
    width = 0.5,
    size = 1,
    alpha = 0.5
  ) +
  
  scale_color_manual(values = lighter_colors) +  # yes uses lighter outlines
  scale_fill_manual(values = lighter_colors) +   # yes uses lighter fill
  labs(
    title = "AV LDMC: ex vs gra with Clipping Distinction",
    x = "Treatment",
    y = "LDMC",
    color = "Clipping = yes",
    fill = "Clipping = yes"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Define your colors
dark_colors  <- c("ex" = "#006400", "gra" = "#FF10F0")   # for clip = no
light_colors <- c("ex" = "#66C266", "gra" = "#FF99F9")   # for clip = yes

# Add color columns to your data
AVdata <- AVdata %>%
  mutate(
    outline_color = case_when(
      clip == "no"  ~ dark_colors[trat],
      clip == "yes" ~ light_colors[trat]
    ),
    fill_color = ifelse(clip == "yes", light_colors[trat], NA)
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  # Boxplot for clip = no
  geom_boxplot(
    data = filter(AVdata, clip == "no"),
    aes(color = outline_color),
    fill = NA,
    width = 0.5,
    size = 1,
    position = position_identity()
  ) +
  
  # Boxplot for clip = yes
  geom_boxplot(
    data = filter(AVdata, clip == "yes"),
    aes(color = outline_color, fill = fill_color),
    width = 0.5,
    size = 1,
    alpha = 0.5,
    position = position_identity()
  ) +
  
  scale_color_identity() +
  scale_fill_identity() +
  
  labs(
    title = "AV LDMC: Treatment vs Clipping",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()




library(ggplot2)
library(dplyr)

# Define colors
dark_colors  <- c("ex" = "#006400", "gra" = "#FF10F0")
light_colors <- c("ex" = "#66C266", "gra" = "#FF99F9")

# Add columns for fill and outline
AVdata <- AVdata %>%
  mutate(
    fill_color = light_colors[trat],  # same fill for both
    outline_color = ifelse(clip == "no", dark_colors[trat], light_colors[trat])
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  geom_boxplot(
    aes(fill = fill_color, color = outline_color),
    width = 0.5,
    size = 1,
    alpha = 0.6,
    position = position_identity()
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(
    title = "AV LDMC: Treatment vs Clipping (Filled Both)",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Define treatment colors (same for both clip types)
dark_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

# Add color column
AVdata <- AVdata %>%
  mutate(
    plot_color = dark_colors[trat]
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  # Bottom layer: clip == "no" (solid)
  geom_boxplot(
    data = filter(AVdata, clip == "no"),
    aes(color = plot_color, fill = plot_color),
    width = 0.5,
    size = 1,
    alpha = 1,
    position = position_identity()
  ) +
  
  # Top layer: clip == "yes" (transparent)
  geom_boxplot(
    data = filter(AVdata, clip == "yes"),
    aes(color = plot_color, fill = plot_color),
    width = 0.5,
    size = 1,
    alpha = 0.3,
    position = position_identity()
  ) +
  
  scale_fill_identity() +
  scale_color_identity() +
  labs(
    title = "AV LDMC: Transparent Overlay for Clipping",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()




library(ggplot2)
library(dplyr)

# Colors for treatments (dark for outlines and light for fills)
dark_colors  <- c("ex" = "#006400", "gra" = "#FF10F0")
light_colors <- c("ex" = "#66C266", "gra" = "#FF99F9")

# Prepare the data with columns for fill and outline colors and alpha
AVdata <- AVdata %>%
  mutate(
    outline_color = dark_colors[trat],
    fill_color = light_colors[trat],
    alpha_val = ifelse(clip == "yes", 0.5, 1)  # Yes is mostly transparent
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  geom_boxplot(
    aes(color = outline_color, fill = fill_color, alpha = alpha_val),
    width = 0.5,
    size = 1,
    position = position_identity()
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_identity() +  # use alpha from data directly
  labs(
    title = "AV LDMC: Dark outline & light fill; 'yes' transparent",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()




ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots for clip == "no" (opaque)
  geom_boxplot(
    data = filter(AVdata, clip == "no"),
    aes(color = outline_color, fill = fill_color),
    alpha = 1,
    width = 0.5,
    size = 1,
    position = position_identity()
  ) +
  
  # Boxplots for clip == "yes" (transparent)
  geom_boxplot(
    data = filter(AVdata, clip == "yes"),
    aes(color = outline_color, fill = fill_color),
    alpha = 0.2,
    width = 0.5,
    size = 1,
    position = position_identity()
  ) +
  
  scale_color_identity() +
  scale_fill_identity() +
  
  labs(
    title = "AV LDMC: Dark outline & light fill; 'yes' transparent",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()




library(scales)  # for alpha()

# Prepare colors with transparency for outlines of 'yes'
AVdata <- AVdata %>%
  mutate(
    outline_color = ifelse(clip == "yes",
                           alpha(dark_colors[trat], 0.2),  # transparent outline for yes
                           dark_colors[trat]),             # opaque outline for no
    fill_color = light_colors[trat]
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  geom_boxplot(
    aes(color = outline_color, fill = fill_color),
    width = 0.5,
    size = 1,
    position = position_identity()
  ) +
  
  scale_color_identity() +
  scale_fill_identity() +
  
  labs(
    title = "AV LDMC: Transparent outlines and fills for 'yes'",
    x = "Treatment",
    y = "LDMC"
  ) +
  theme_minimal()




library(ggplot2)
library(dplyr)

# Your custom colors for treatments
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

ggplot(AVdata, aes(x = trat, y = LDMC, color = trat)) +
  geom_boxplot(fill = NA, size = 1, width = 0.5) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "AV LDMC: ex vs gra (No Fill)",
    x = "Treatment",
    y = "LDMC",
    color = "Treatment"
  ) +
  theme_minimal()





library(ggplot2)
library(dplyr)

# Custom colors for treatments
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: no fill, colored outline
  geom_boxplot(aes(color = trat), fill = NA, size = 1, width = 0.5) +
  
  # Violin for clip == "no": filled, no outline
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat),
    color = NA,
    alpha = 0.5,
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  # Violin for clip == "yes": no fill, dashed outline
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat),
    fill = NA,
    size = 1,
    linetype = "dashed",
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = "AV LDMC: Boxplots + Violin plots by Clip",
    x = "Treatment",
    y = "LDMC",
    color = "Treatment",
    fill = "Treatment"
  ) +
  
  theme_minimal()




library(ggplot2)
library(dplyr)

# Custom colors for treatments
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: no fill, colored outline
  geom_boxplot(aes(color = trat), fill = NA, size = 1, width = 0.5) +
  
  # Violin for clip == "no": filled, no outline, lighter fill
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat),
    color = NA,
    alpha = 0.2,  # lighter fill for subtle background
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  # Violin for clip == "yes": no fill, dashed outline
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat),
    fill = NA,
    size = 1,
    linetype = "dashed",
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = "AV LDMC: Boxplots + Violin plots by Clip",
    x = "Treatment",
    y = "LDMC",
    color = "Treatment",
    fill = "Treatment"
  ) +
  
  theme_minimal()




library(ggplot2)
library(dplyr)
library(scales)  # for alpha()

# Custom dark colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")

# Lighter outlines using alpha
lighter_colors <- c("ex" = alpha("#006400", 0.3),
                    "gra" = alpha("#FF10F0", 0.3))

# Add outline_color column for the light dashed violin outlines
AVdata <- AVdata %>%
  mutate(outline_color = lighter_colors[trat])

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: no fill, strong colored outline
  geom_boxplot(aes(color = trat), fill = NA, size = 1, width = 0.5) +
  
  # Violin for clip == "no": filled, no outline, transparent fill
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat),
    color = NA,
    alpha = 0.2,
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  # Violin for clip == "yes": dashed light outline, no fill
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = outline_color),
    fill = NA,
    size = 1,
    linetype = "dashed",
    position = position_dodge(width = 0.75),
    width = 0.8
  ) +
  
  scale_color_identity() +
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = "AV LDMC: Boxplots + Light Violin Outlines",
    x = "Treatment",
    y = "LDMC",
    color = "Treatment",
    fill = "Treatment"
  ) +
  
  theme_minimal()




library(ggplot2)
library(dplyr)
library(scales)

# Custom dark colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")
# Lighter versions for outlines
lighter_colors <- c("ex" = alpha("#006400", 0.3), "gra" = alpha("#FF10F0", 0.3))

# Add color mappings to data
AVdata <- AVdata %>%
  mutate(
    color_dark = custom_colors[trat],
    color_light = lighter_colors[trat]
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: no fill, dark outline
  geom_boxplot(
    data = AVdata,
    aes(color = color_dark),
    fill = NA,
    size = 1,
    width = 0.5
  ) +
  
  # Violin for clip == "no": filled with dark color, no outline
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat),
    color = NA,
    alpha = 0.2,
    width = 0.8
  ) +
  
  # Violin for clip == "yes": no fill, light dashed outline
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = color_light),
    fill = NA,
    size = 1,
    linetype = "dashed",
    width = 0.8
  ) +
  
  # Manual fill scale for violins
  scale_fill_manual(values = custom_colors) +
  
  # Turn off color scale (since we mapped exact hex values)
  scale_color_identity() +
  
  labs(
    title = "AV LDMC: Boxplots + Violin by Clip (Light Outline)",
    x = "Treatment",
    y = "LDMC",
    fill = "Treatment"
  ) +
  
  theme_minimal()





library(ggplot2)
library(dplyr)
library(scales)

# Custom dark and light colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")
lighter_colors <- c("ex" = alpha("#006400", 0.3), "gra" = alpha("#FF10F0", 0.3))

# Prepare color columns
AVdata <- AVdata %>%
  mutate(
    color_dark = custom_colors[trat],
    color_light = lighter_colors[trat],
    clip = factor(clip, levels = c("no", "yes"))
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: no fill, dark outline
  geom_boxplot(
    aes(color = trat),
    fill = NA,
    size = 1,
    width = 0.5
  ) +
  
  # Violin for clip == "no": filled with dark color, no outline
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat, group = interaction(trat, clip), linetype = clip),
    color = NA,
    alpha = 0.2,
    width = 0.3,
    scale = "area"
  ) +
  
  # Violin for clip == "yes": light dashed outline, no fill
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat, group = interaction(trat, clip), linetype = clip),
    fill = NA,
    size = 0.5,
    width = 0.3, 
    scale = "area"
  ) +
  
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_linetype_manual(values = c("no" = "solid", "yes" = "dashed")) +
  
  labs(
    title = "AV LDMC: Boxplots + Violin (Clipping shown in style)",
    x = "Treatment",
    y = "LDMC",
    color = "Treatment",
    fill = "Treatment",
    linetype = "Clipping"
  ) +
  
  theme_minimal()



library(ggplot2)
library(dplyr)
library(scales)  # for alpha()

# Custom dark and light colors
custom_colors <- c("ex" = "#006400", "gra" = "#FF10F0")
lighter_colors <- c("ex" = alpha("#006400", 0.3), "gra" = alpha("#FF10F0", 0.3))

# Ensure 'clip' is a factor with consistent levels
AVdata <- AVdata %>%
  mutate(
    clip = factor(clip, levels = c("no", "yes")),
    color_dark = custom_colors[trat],
    color_light = lighter_colors[trat]
  )

ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: dark outline, no fill
  geom_boxplot(
    aes(color = trat),
    fill = NA,
    size = 1,
    width = 0.5
  ) +
  
  # Violin for clip == "no": dark fill, no outline
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat, group = interaction(trat, clip), linetype = clip),
    color = NA,
    alpha = 0.2,
    width = 0.3,
    scale = "area"
  ) +
  
  # Violin for clip == "yes": dashed outline, no fill
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat, group = interaction(trat, clip), linetype = clip),
    fill = NA,
    size = 0.5,
    width = 0.3,
    scale = "area"
  ) +
  
  # Manual scales
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_linetype_manual(
    values = c("no" = "solid", "yes" = "dashed"),
    labels = c("WITHOUT HERBIVORY SIMULATION", "WITH HERBIVORY SIMULATION")
  ) +
  
  # Axis and legend labels
  scale_x_discrete(
    labels = c("ex" = "WITHOUT GRAZING", "gra" = "WITH GRAZING")
  ) +
  
  labs(
    x = "Procedence",
    y = "LDMC",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_blank()
  )






ggplot(AVdata, aes(x = trat, y = LDMC)) +
  
  # Boxplots: dark outline, no fill
  geom_boxplot(
    aes(color = trat),
    fill = NA,
    size = 1,
    width = 0.5
  ) +
  
  # Violin for clip == "no": dark fill, no outline
  geom_violin(
    data = filter(AVdata, clip == "no"),
    aes(fill = trat, group = interaction(trat, clip), linetype = clip),
    color = NA,
    alpha = 0.2,
    width = 0.5,
    scale = "area"
  ) +
  
  # Violin for clip == "yes": dashed outline, no fill
  geom_violin(
    data = filter(AVdata, clip == "yes"),
    aes(color = trat, group = interaction(trat, clip), linetype = clip),
    fill = NA,
    size = 0.5,
    width = 0.5,
    scale = "area"
  ) +
  
  # Manual scales
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_linetype_manual(
    values = c("no" = "solid", "yes" = "dashed"),
    labels = c("WITHOUT HERBIVORY SIMULATION", "WITH HERBIVORY SIMULATION")
  ) +
  
  # Axis and legend labels
  scale_x_discrete(
    labels = c("ex" = "WITHOUT GRAZING", "gra" = "WITH GRAZING")
  ) +
  
  labs(
    x = "Procedence",
    y = "LDMC",
    color = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_blank()
  )


