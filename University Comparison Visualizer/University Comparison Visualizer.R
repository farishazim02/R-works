
# Libraries used
library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(lubridate)
library(stringr)
library(scales)
library(ggbump)
library(ggrepel)
library(rsconnect)

options(warn = -1)

# Preliminary setup

my_theme <- theme_classic() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_rect(fill = "#FFFFFF")
  )

theme_set(my_theme)

# Data cleaning/setup
qs <- read_csv("qs_17-22.csv") %>%
  mutate(research_output = recode(research_output, "Very high" = "Very High")) %>%
  mutate(size = fct_relevel(size, c("S", "M", "L", "XL"))) %>%
  mutate(research_output = fct_relevel(research_output, c(
    "Low", "Medium",
    "High", "Very High"
  ))) %>%
  mutate(
    international_students = replace(international_students, is.na(international_students), 0),
    faculty_count = replace(faculty_count, is.na(faculty_count), 0),
    student_faculty_ratio = replace(student_faculty_ratio, is.na(student_faculty_ratio), 0)
  )

qs_regions <- unique(as.character(qs$region))

# All texts
descriptive_text <- p("Learn how universities perform over the years from 2017 - 2022.
                     Select from two display modes to visualize the graphs.")

start_instructions <- p("The General display shows a series of statistics about all universities
                 for a particular year. The Specific display provides insights
                 into the performance (rank or score) of universities.")

notes <- HTML(
  "<strong><span style = 'color: #77A690;'>Instructions/Notes:</span></strong><br>",
  "1. A region filter is only available when display mode is set to specific.<br>",
  "2. For the <strong> Specific display mode, click on a row within the rankings table </strong> to
display information about the university.<br>",
  "3. Current version shows individual rankings/scores for universities with complete data from 2017 - 2022 only.<br><br><br>"
)

filter_instructions <- p("Choose to filter the data based on various aspects of university population.
                        Plots and the rankings table will be updated accordingly.")

comparison_instructions <- p("Choose the type of comparison for the universities.")

specific_display_instructions <- p(
  "Choose to graph the university performance based on either rank or score.
                                  Additionally, visualize how this university stands in comparison to other universities.", br(),
  br(), "Note:", br(), "1. In choosing to visualize multiple university
                                  performances on a single graph, the bump charts will be color-coded accordingly.",
  br(), "2. For universities with a lower average rank commpared to the focus university,
                                  these institutions will be colored red.", br(),
  "3. Conversely, for universities with a higher average rank in relation to the focus
                                  university, they will be colored green.", br()
)

noti_message <- "plots and rankings table updated."

colors <- c(
  "Europe" = "#0081C8", "Asia" = "#FCB131", "Africa" = "#000000",
  "Oceania" = "#00A651", "North America" = "#EE334E", "Latin America" = "#8a2be2"
)

# Some summaries
min_max <- qs %>%
  drop_na(score) %>%
  summarize(min = min(score), max = max(score))

filter_options_is <- qs %>%
  drop_na(international_students) %>%
  summarize(min = min(international_students), max = max(international_students))

filter_options_sfr <- qs %>%
  drop_na(student_faculty_ratio) %>%
  summarize(min = min(student_faculty_ratio), max = max(student_faculty_ratio))

filter_options_fc <- qs %>%
  drop_na(faculty_count) %>%
  summarize(min = min(faculty_count), max = max(faculty_count))

available_rank_score <- qs %>%
  drop_na(rank_display, score) %>%
  mutate(valid_rank = as.numeric(!is.na(rank_display)), valid_score = as.numeric(!is.na(score))) %>%
  group_by(university) %>%
  summarize(num_valid_rank = sum(valid_rank), num_valid_score = sum(valid_score)) %>%
  filter(num_valid_rank == 6 & num_valid_score == 6) %>%
  pull(university)


# Helper functions:

# 1) General method to create plotly
create_plotly <- function(plot) {
  ggplotly(plot, tooltip = "text") %>%
    layout(
      hoverlabel = list(font = list(size = 20), bgcolor = "black"),
      xaxis = list(title = list(standoff = 10))
    )
}

# 2) Helper function 1 for average score plot
helper_avg_score <- function(df) {
  df %>%
    drop_na(score) %>%
    group_by(year, region) %>%
    summarize(avg_score = mean(score)) %>%
    ungroup()
}

# 3) Helper function 2 for average score plot; adds more summary statistics
helper_avg_score2 <- function(df) {
  df %>%
    drop_na(score) %>%
    group_by(year, region) %>%
    summarize(
      avg_score = mean(score), lowest = min(score),
      lowest_uni = university[which(score == lowest)],
      highest = max(score), highest_uni = university[which(score == highest)]
    ) %>%
    ungroup()
}

# 4) Setup for specific display; checks whether ranks/scores for a university are complete for all years
setup_uni_plot <- function(df) {
  df %>%
    drop_na(rank_display, score) %>%
    count() %>%
    pull(n)
}

# 5) Transforms current format of rank into a single numerical value for plotting later
process_rank <- function(rank) {
  if (str_detect(rank, "-")) {
    rank_range <- as.numeric(str_split(rank, "-")[[1]])
    return(floor(mean(rank_range)))
  } else {
    return(as.numeric(rank))
  }
}

min_max_rank <- qs %>%
  drop_na(rank_display) %>%
  mutate(rank = unlist(lapply(rank_display, process_rank))) %>%
  summarize(min_rank = min(rank), max_rank = max(rank))


# 6) How university rankings are displayed within the data table
data_display <- function(df) {
  df$rank_display[!is.na(df$rank_display)] <- unlist(lapply(df$rank_display[!is.na(df$rank_display)], process_rank))

  df %>%
    mutate(rank_display = as.numeric(rank_display)) %>%
    select(rank_display, everything(), -link, -region, -logo, -student_faculty_ratio, -international_students, -faculty_count, -year) %>%
    rename(
      "University" = "university",
      "Rank" = "rank_display",
      "Score" = "score",
      "Country" = "country",
      "City" = "city",
      "Type" = "type",
      "Research Output" = "research_output",
      "Size" = "size"
    ) %>%
    datatable(rownames = FALSE, selection = "single", style = "default")
}


# Create plots for general mode:

# 1) Number of universities ranked per region
num_unis_plot <- function(df) {
  order_bar <- helper_avg_score(df) %>%
    arrange(desc(avg_score)) %>%
    pull(region)

  p <- df %>%
    count(region) %>%
    mutate(region = fct_relevel(region, order_bar)) %>%
    ggplot(aes(x = region, y = n, fill = region, text = paste("Total - ", n))) +
    geom_col(color = "black") +
    scale_fill_manual(values = colors) +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x = "Region", y = "Number of Universities",
      title = "Number of Universities Ranked",
    )

  create_plotly(p)
}

# 2) Average score of universities by region; highlights the year chosen
avg_score_plot <- function(df, helper_df, chosen_year) {
  temp_df <- helper_avg_score(helper_df)
  temp_df2 <- helper_avg_score2(df)
  temp_df3 <- temp_df %>%
    filter(year <= chosen_year)

  p <- ggplot(data = temp_df, aes(x = year, y = avg_score)) +
    geom_bump(aes(group = region), alpha = 0.1) +
    geom_point(alpha = 0.1) +
    geom_point(data = temp_df3, aes(x = year, y = avg_score, color = region)) +
    geom_point(data = temp_df2, aes(
      x = year, y = avg_score, color = region,
      text = paste("Average Score - ", round(avg_score, 2),
        "\nHighest Score - ", highest, " [",
        highest_uni, "]",
        "\nLowest Score - ", lowest, " [",
        lowest_uni, "]",
        sep = ""
      )
    ), size = 3) +
    scale_color_manual(values = colors) +
    labs(
      x = "Year", y = "Average Score", color = "Region",
      title = "Average University Score"
    )

  if (length(unique(temp_df3$year)) > 1) {
    p <- p +
      geom_bump(data = temp_df3, aes(x = year, y = avg_score, group = region, color = region))
  }

  create_plotly(p) %>%
    layout(legend = list(
      x = 0.25, y = -0.3,
      xanchor = "left",
      yanchor = "bottom",
      orientation = "h"
    ))
}

# 3) Research output/Type v. Score
additional_plots_gen <- function(df, val) {
  xlabel <- ""
  title_plot <- ""
  if (val == "type") {
    xlabel <- "Type"
    title_plot <- "Average Score by University Type"
  } else {
    xlabel <- "Research Output"
    title_plot <- "Average Score by University Research Output"
  }

  p <- df %>%
    drop_na(.data[[val]], score) %>%
    group_by(.data[[val]]) %>%
    summarize(avg_score = mean(score)) %>%
    ggplot(aes(
      x = .data[[val]], y = avg_score,
      text = paste("Average Score -", round(avg_score, 2))
    )) +
    geom_bar(stat = "identity", color = "black", fill = "#CAD5E5") +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x = xlabel, y = "Average Score",
      title = title_plot
    )

  create_plotly(p)
}


# Create plots for specific mode:

# 1) University rank/score over the years (2017-2022)
uni_rank_score_plot <- function(df, val, chosen_year, unis) {
  df <- df %>%
    ungroup() %>%
    select(year, university, rank_display, score) %>%
    mutate(rank = unlist(lapply(rank_display, process_rank)))

  this_rank <- mean(df$rank)
  this_score <- mean(df$score)

  df2 <- df %>%
    filter(year == chosen_year)


  # Data for the other universities that are being compared
  # (if statement implemented because when app is initialized, there is nothing to filter)
  if (length(unis) > 0) {
    compare_unis <- qs %>%
      filter(university %in% unis) %>%
      ungroup() %>%
      select(year, university, rank_display, score) %>%
      mutate(rank = unlist(lapply(rank_display, process_rank))) %>%
      group_by(university) %>%
      mutate(
        average_rank = mean(rank),
        better_worse_rank = as.numeric(average_rank <= this_rank),
        coloring_rank = ifelse(better_worse_rank == 1, "Better Avg Rank", "Lower Avg Rank"),
        average_score = mean(score),
        better_worse_score = as.numeric(average_score >= this_score),
        coloring_score = ifelse(better_worse_rank == 1, "Better Avg Score", "Lower Avg Score")
      )

    compare_unis_current <- compare_unis %>%
      filter(year == chosen_year)
  }

  # if/else allows for easy customizability of graph
  # (since rank/scores are displayed a little differently)

  # Graph for rank
  if (val == "rank_display") {
    p <- ggplot(data = df, aes(x = year, y = rank)) +
      geom_bump(aes(group = 1)) +
      geom_point(aes(text = ifelse(year == chosen_year, "", paste("Rank -", rank_display))), size = 3) +
      geom_point(
        data = df2, aes(text = paste("Current Year\nRank -", rank_display)),
        size = 5, color = "#2D708EFF"
      ) +
      labs(
        x = "Year", y = "Rank (Log Scale)",
        title = "University Ranking Over the Years"
      )

    # Additional graph layers when comparing with other universities
    if (length(unis) > 0) {
      p <- p +
        geom_bump(data = compare_unis, aes(
          x = year, y = rank, col = coloring_rank,
          group = university
        )) +
        geom_point(data = compare_unis, aes(
          col = coloring_rank,
          text = ifelse(year == chosen_year, "",
            paste("Rank -", rank_display)
          )
        ), size = 3) +
        geom_point(
          data = compare_unis_current, aes(text = paste("Current Year\nRank -", rank_display)),
          size = 5, color = "#2D708EFF"
        ) +
        geom_text(
          data = compare_unis %>% filter(year == max(year)),
          aes(
            x = year + 0.3, y = rank + 0.06, col = coloring_rank,
            label = str_wrap(university, width = 20)
          ),
          size = 4, hjust = 1
        ) +
        geom_text(
          data = df %>% filter(year == max(year)),
          aes(
            x = year + 0.3, y = rank + 0.06,
            label = str_wrap(university, width = 20)
          ),
          size = 4, hjust = 1
        ) +
        scale_color_manual(values = c(
          "Better Avg Rank" = "darkgreen",
          "Lower Avg Rank" = "red"
        )) +
        scale_y_continuous(trans = c("log10", "reverse")) +
        labs(col = "")

      # Looking at only this university
    } else if (length(unis) == 0) {
      p <- p +
        scale_y_continuous(
          limits = c(min_max_rank$max_rank, min_max_rank$min_rank),
          trans = c("log10", "reverse")
        )
    }

    # Graph for score
  } else if (val == "score") {
    p <- ggplot(data = df, aes(x = year, y = score)) +
      geom_bump(aes(group = 1)) +
      geom_point(aes(text = ifelse(year == chosen_year, "", paste("Score -", score))), size = 3) +
      geom_point(
        data = df2, aes(text = paste("Current Year\nScore -", score)),
        size = 5, color = "#2D708EFF"
      ) +
      scale_y_continuous(limits = c(min_max$min, min_max$max)) +
      labs(x = "Year", y = "Score", title = "University Score Over the Years")

    # Additional graph layers when comparing with other universities
    if (length(unis) > 0) {
      p <- p +
        geom_bump(data = compare_unis, aes(
          x = year, y = score, col = coloring_score,
          group = university
        )) +
        geom_point(data = compare_unis, aes(
          col = coloring_score,
          text = ifelse(year == chosen_year, "",
            paste("Score -", score)
          )
        ), size = 3) +
        geom_point(
          data = compare_unis_current, aes(text = paste("Current Year\nScore -", score)),
          size = 5, color = "#2D708EFF"
        ) +
        geom_text(
          data = compare_unis %>% filter(year == max(year)),
          aes(
            x = year + 0.3, y = score - 0.06, col = coloring_score,
            label = str_wrap(university, width = 20)
          ),
          size = 4, hjust = 1
        ) +
        geom_text(
          data = df %>% filter(year == max(year)),
          aes(
            x = year + 0.3, y = score - 0.06,
            label = str_wrap(university, width = 20)
          ),
          size = 4, hjust = 1
        ) +
        scale_color_manual(values = c(
          "Better Avg Score" = "darkgreen",
          "Lower Avg Score" = "red"
        )) +
        labs(col = "")
    }
  }

  create_plotly(p) %>%
    layout(legend = list(
      x = 0.37, y = -0.18,
      xanchor = "left",
      yanchor = "bottom",
      orientation = "h"
    ))
}


# Main UI for the Shiny App
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "simplex",
    fg = "#261912",
    bg = "#FFFFFF",
    primary = "#77A690",
    base_font = font_google("Poppins"),
    heading_font = font_google("Oswald")
  ),
  titlePanel("University Comparison Visualizer"),
  sidebarLayout(
    sidebarPanel(
      descriptive_text,
      start_instructions,
      notes,
      radioButtons(
        "display_mode", "Display Mode",
        c(
          "General" = "gen",
          "Specific" = "spec"
        )
      ),
      selectInput("year", "Year", seq(2017, 2022, by = 1)),

      # Region dropdown menu only appears when user chooses the 'specific' display mode
      conditionalPanel(
        condition = "input.display_mode === 'spec'",
        selectInput("region", "Region", choices = c("All Regions" = "all", qs_regions))
      )
    ),
    mainPanel(
      DTOutput("rankings_table")
    )
  ),

  # Updates UI accordingly for a cleaner and logical plotting canvas
  uiOutput("dynamic_ui")
)


# Main server for the Shiny App
server <- function(input, output) {
  # How data should look like after selecting the options within the
  # sidebar panel
  current_data <- reactive(label = "current_data", {
    if (input$region == "all" & input$display_mode != "gen") {
      new_qs <- qs %>%
        filter(year == input$year)
      return(new_qs)
    } else if (input$region != "all" & input$display_mode == "spec") {
      new_qs <- qs %>%
        filter(region == input$region & year == input$year)
      return(new_qs)
    } else if (input$display_mode == "gen") {
      new_qs <- qs %>%
        filter(year == input$year)

      if (user_interact_is$count > 0) {
        new_qs <- new_qs %>%
          filter(international_students >= input$filter_is[1] & international_students <= input$filter_is[2])
      }

      if (user_interact_sfr$count > 0) {
        new_qs <- new_qs %>%
          filter(student_faculty_ratio >= input$filter_sfr[1] & student_faculty_ratio <= input$filter_sfr[2])
      }

      if (user_interact_fc$count > 0) {
        new_qs <- new_qs %>%
          filter(faculty_count >= input$filter_fc[1] & faculty_count <= input$filter_fc[2])
      }

      return(new_qs)
    }
  })

  # Secondary data set to help plot average score per region (used in general display mode)
  helper_data_avg_score <- reactive(label = "helper_data_avg_score", {
    qs %>%
      filter(international_students >= input$filter_is[1] & international_students <= input$filter_is[2]) %>%
      filter(faculty_count >= input$filter_fc[1] & faculty_count <= input$filter_fc[2]) %>%
      filter(student_faculty_ratio >= input$filter_sfr[1] & student_faculty_ratio <= input$filter_sfr[2])
  })

  # Extracts university name and link from the click event on data table
  # Changes are apparent only in specific mode
  get_uni <- reactiveVal(NULL)
  get_link <- reactiveVal(NULL)
  observeEvent(
    input$rankings_table_rows_selected,
    {
      row <- input$rankings_table_rows_selected
      get_df <- current_data()
      get_uni(unlist(get_df[row, "university"]))
      get_link(unlist(get_df[row, "link"]))
    }
  )

  # Notification for each time a filtering operation is carried out -
  # each operation has a specific message
  user_interact_is <- reactiveValues(label = "user_interact_is", count = 0)
  observeEvent((input$filter_is), {
    if (user_interact_is$count > 0) {
      showNotification(paste("Range for international students changed:\n", noti_message),
        id = "noti_is", type = "message", duration = 2
      )
    }
    user_interact_is$count <- user_interact_is$count + 1
  })

  user_interact_fc <- reactiveValues(label = "user_interact_fc", count = 0)
  observeEvent((input$filter_fc), {
    if (user_interact_fc$count > 0) {
      showNotification(paste("Range for faculty count changed:\n", noti_message),
        id = "noti_fc", type = "message", duration = 2
      )
    }
    user_interact_fc$count <- user_interact_fc$count + 1
  })

  user_interact_sfr <- reactiveValues(label = "user_interact_sfr", count = 0)
  observeEvent((input$filter_sfr), {
    if (user_interact_sfr$count > 0) {
      showNotification(paste("Range for student-faculty-ratio changed:\n", noti_message),
        id = "noti_sfr", type = "message", duration = 2
      )
    }
    user_interact_sfr$count <- user_interact_sfr$count + 1
  })

  # Gets the data for a chosen university (used in specific display mode)
  filtered_uni_data <- reactive(label = "filtered_uni_data", {
    if (length(get_uni()) != 0) {
      qs %>%
        filter(university == get_uni())
    }
  })

  # Data table
  output$rankings_table <- renderDT({
    data_display(current_data())
  })

  # UI is updated according to the display mode chosen
  output$dynamic_ui <- renderUI({
    # When general display mode is chosen, UI shows summary statistics of universities as a whole
    if (input$display_mode == "gen") {
      div(
        style = "margin-top: 30px; margin-bottom: 15px;",
        h2(paste("Summary Statistics in", input$year), style = "text-decoration: underline;"),
        h3("Filtering Options:"),
        filter_instructions,
        div(
          style = "margin-top: 15px; margin-bottom: 15px;  margin-left: 190px; margin-right: 40px;",
          fluidRow(
            column(4, sliderInput("filter_is", "Range of International Students", 0, 32000, c(0, 32000), sep = "")),
            column(4, sliderInput("filter_fc", "Range of Faculty Count", 0, 21000, c(0, 21000), sep = "")),
            column(4, sliderInput("filter_sfr", "Range of Student-faculty Ratio", 0, 70, c(0, 70), sep = ""))
          )
        ),
        h3("Comparison Options:"),
        comparison_instructions,
        div(
          style = "margin-top: 15px; margin-bottom: 15px;",
          fluidRow(
            column(12, selectInput("select_comparison", "Type",
              choices = c("Region-based" = "region", "Miscellaneous" = "misc")
            )),
          )
        ),

        # Based on user selection, the dashboard updates whether to show region-based or
        # miscellaneous comparisons
        conditionalPanel(
          condition = "input.select_comparison === 'region'",
          h3("Region-based Comparisons:"),
          div(
            style = "margin-top: 15px;",
            fluidRow(
              column(12, plotlyOutput("num_unis", height = 550)),
            ),
            fluidRow(
              column(12, plotlyOutput("avg_score", height = 550))
            )
          )
        ),
        conditionalPanel(
          condition = "input.select_comparison === 'misc'",
          h3("Miscellaneous Comparisons:"),
          div(
            style = "margin-top: 15px;",
            fluidRow(
              column(6, plotlyOutput("type", height = 550)),
              column(6, plotlyOutput("research_output", height = 550))
            )
          )
        )
      )

      # When specific display mode is chosen, UI shows the ranking/score of a chosen university
      # However, if data is incomplete, the app redirects users to the university's
      # webpage on QS
    } else if (input$display_mode == "spec") {
      div(
        style = "margin-top: 30px; margin-bottom: 15px;",
        h2(get_uni(), style = "text-decoration: underline;"),
        div(
          style = "margin-top: 15px;",
          fluidRow(
            column(
              12,
              if (length(get_uni()) != 0) {
                if (setup_uni_plot(filtered_uni_data()) == 6) {
                  specific_display_instructions
                }
              }
            ),
            column(
              12,
              if (length(get_uni()) != 0) {
                if (setup_uni_plot(filtered_uni_data()) == 6) {
                  selectInput("uni_comparison", "Comparison Type",
                    choices = c(
                      "By Rank" = "rank_display",
                      "By Score" = "score"
                    )
                  )
                }
              }
            ),
            column(
              12,
              if (length(get_uni()) != 0) {
                if (setup_uni_plot(filtered_uni_data()) == 6) {
                  selectInput("add_unis", "Compare with Other Universities",
                    choices = available_rank_score[!available_rank_score == get_uni()],
                    multiple = TRUE
                  )
                }
              }
            )
          ),
          fluidRow(
            column(
              12,
              if (length(get_uni()) != 0) {
                if (setup_uni_plot(filtered_uni_data()) != 6) {
                  p(
                    "Data about this university is incomplete/unavailable. Please visit",
                    a("this link", href = get_link()), "for a more complete
                           overview of the university's performance."
                  )
                } else {
                  plotlyOutput("uni_rank_score", height = 750)
                }
              }
            )
          )
        )
      )
    }
  })


  # Render Plots

  output$num_unis <- renderPlotly({
    num_unis_plot(current_data())
  })

  output$avg_score <- renderPlotly({
    # To avoid error, check if graph can be plotted based on the data
    test_condition <- current_data() %>%
      filter(!is.na(score)) %>%
      count() %>%
      pull(n)

    if (test_condition > 0) {
      return(avg_score_plot(current_data(), helper_data_avg_score(), input$year))
    }

    # Return a blank canvas since test condition is false
    return()
  })

  output$type <- renderPlotly({
    additional_plots_gen(current_data(), "type")
  })

  output$research_output <- renderPlotly({
    additional_plots_gen(current_data(), "research_output")
  })

  output$uni_rank_score <- renderPlotly({
    uni_rank_score_plot(filtered_uni_data(), input$uni_comparison, input$year, input$add_unis)
  })
}

# Runs the app
app <- shinyApp(ui, server)
