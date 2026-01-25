
# Libraries used
library(tidyverse)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(stringr)
library(DT)
library(lubridate)
library(scales)
library(packcircles)

options(warn = -1)


# Preliminary setup

# 1. Theme for plots / style for dashboard UI
my_theme <- theme_classic() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    title = element_text(size = 22),
    legend.text = element_text(size = 18),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_rect(fill = "#FFFFFF")
  )

theme_set(my_theme)

ui_stylings <-
  ".sidebar-menu .fas {
    margin-right: 10px;
 }

 .sidebar-menu li a {
    font-family: 'Roboto', sans-serif;
    font-size: 16px;
 }

 .main-header .logo {
    font-family: 'Georgia';
    font-weight: bold;
    font-size: 26px;
  }

  h1 {
    font-family: 'Georgia';
    font-weight: bold;
    font-size: 40px;
  }

  .box.box-primary>.box-header {
    border-bottom: none;
  }

  .box.box-solid>.box-header {
    border-bottom: none;
  }

  .box-footer {
    border-top: none;
  }

  .box.box-primary {
    border-top-color: green;
  }

  .selectize-input {
    font-size: 16px;
    font-family: 'Roboto', sans-serif;
  }

  p, li {
    font-size: 20px;
    font-family: 'Roboto', sans-serif;
    text-align: justify;
  }
"

box_title_styles <- "font-size: 26px; font-family: 'Georgia'; color: #77A690"
box_footer_styles <- "font-size: 16px; font-family: 'Roboto', sans-serif;"


# 2. Load data

# Read layoff data and prepare it for visualizations
layoff <- read_csv("layoff_w_city.csv") %>%
  mutate(city = sub(",.*", "", headquarters)) %>%
  select(company, layoff_number, layoff_percent, announcement_date, industry, headquarters, city, everything()) %>%
  mutate(announcement_date = mdy(announcement_date)) %>%
  mutate(announcement_date = paste(month(announcement_date, label = T), year(announcement_date))) |>
  mutate(temp = myd(paste(announcement_date, "01"))) %>%
  mutate(announcement_date = fct_reorder(announcement_date, temp)) %>%
  select(-temp, -source, -notes)


# Available company data
layoff_financial <- read_csv("cleaned_layoff_vs_price.csv") %>%
  mutate(DateLayoff = mdy(DateLayoff))
full_price <- read_csv("cleaned_price.csv")

common_ticker <- layoff_financial %>%
  semi_join(full_price, by = "Ticker") %>%
  group_by(Ticker) %>%
  summarise(Name = paste(unique(Name), collapse = ", ")) %>%
  ungroup()


# 3. Texts
app_desc <- p(
  strong("TechQuake"), "is your comprehensive tech layoff visualizer. Our platform
             provides multiple functionalities that allow you to conduct an in-depth
             analysis of the shifting technology landscape. We offer historical,
             geographical, sector-specific, and much more insights, enabling you
             to assess the impacts of the recent layoff waves from distinct angles.
             Additionally, we provide statistics on individual tech company profiles for
             users to gauge the severity of such a scenario on a particular organization.
             Choose either the 'General' or 'Company' view to get started!",
  br(),
  br(),
  br(),
  "Our data is updated as of February 22, 2024."
)

gen_instructions <- p("Learn the various layoff patterns of the tech industry itself.
                      Data is based on available layoff figures.")

company_instructions <- p("Gain insights into how a company\'s performance impact its
                         decision to cut employees. Choose up to 2 companies for comparison!")

main_plot_instructions <- p(
  "Brush over a certain time period to focus on specific months/years.
  All graphs in this view will be updated to reflect the chosen period.", br(),
  strong("Note:"), "Please ensure that your brush touches both bar charts of a particular month.
  The app defaults to selecting the entire layoff period if the brush does not highlight any month."
)

most_layoff_add_info <- p("Hover over each bubble for additional information.")


# Helper functions:

# 1. Convert to plotly
create_plotly <- function(plot) {
  ggplotly(plot, tooltip = "text") %>%
    layout(
      hoverlabel = list(font = list(size = 15), bgcolor = "black"),
      xaxis = list(title = list(standoff = 10)),
      yaxis = list(title = list(standoff = 10))
    )
}

# 2. Closing prices and layoffs (Company-specific)
plot_price <- function(ticker, company) {
  covid_19 <- data.frame(
    xmin = as.Date(c("2020-01-01", "2020-03-15", "2020-05-15", "2021-04-01")),
    xmax = as.Date(c("2020-03-15", "2020-05-15", "2021-01-01", "2021-06-01")),
    fill = c("Infection", "Social Distancing", "Management", "Eradication")
  )

  temp_data <- full_price %>%
    filter(Ticker %in% ticker)

  layoff_date <- layoff_financial %>%
    filter(Ticker %in% ticker) %>%
    select(Ticker, DateLayoff)

  if (length(company) == 2) {
    get_baseline <- temp_data %>%
      filter(Date == "2020-01-02") %>%
      select(Ticker, Close) %>%
      rename(Baseline = Close)

    temp_data <- temp_data %>%
      left_join(get_baseline, by = "Ticker") %>%
      group_by(Ticker) %>%
      mutate(Close = Close / Baseline) %>%
      select(-Baseline)

    title_str_builder <- paste0("Closing Prices and Layoffs for ", company[1], " and ", company[2])
    y_builder <- "Normalized Price (Jan 1, 2020 = 1.0)"
  } else {
    title_str_builder <- paste("Closing Prices and Layoffs for", company)
    y_builder <- "Price ($)"
  }


  p <- ggplot(temp_data) +
    geom_line(aes(x = Date, y = Close, col = Ticker)) +
    geom_rect(
      data = covid_19, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = factor(fill)),
      alpha = 0.44
    ) +
    geom_vline(
      data = layoff_date, aes(xintercept = DateLayoff, color = Ticker),
      linetype = "dashed", show.legend = F
    ) +
    scale_fill_manual(
      values = c(
        "Infection" = "#0E6251",
        "Social Distancing" = "#148F77",
        "Management" = "#1ABC9C",
        "Eradication" = "#76D7C4"
      ),
      breaks = c("Infection", "Social Distancing", "Management", "Eradication")
    ) +
    scale_color_manual(
      values = c("red3", "blue1"),
      labels = company
    ) +
    labs(
      y = y_builder,
      col = "Company & Layoff Dates",
      fill = "Covid-19 Phases",
      title = title_str_builder
    )

  if (length(company) <= 1) {
    p <- p +
      guides(col = "none")

    if (length(company) == 0) {
      p <- p +
        guides(fill = "none")
    }
  }

  return(p)
}

# 3. Helper function - Data for layoffs by month and year
layoff_main_data <- function() {
  layoff %>%
    filter(!is.na(layoff_number)) %>%
    group_by(announcement_date) %>%
    summarise(number_of_layoffs = sum(layoff_number), number_of_companies = length(unique(company)))
}

# 4. Helper function - Get selected dates
get_selected <- function(selected) {
  layoff_main_data() %>%
    filter(selected) %>%
    pull(announcement_date)
}

# 5. Layoff records (Company-specific)
layoff_records_company <- function(ticker) {
  layoff_financial %>%
    mutate(PctLayoff = round(PctLayoff * 100, 2)) %>%
    arrange(desc(DateLayoff)) %>%
    filter(Ticker %in% ticker) %>%
    rename(
      "Company" = "Name",
      "Stock Ticker" = "Ticker",
      "Number of Layoffs" = "Layoff",
      "Percent of Layoffs (%)" = "PctLayoff",
      "Date of Layoff" = "DateLayoff",
      "Financing Stage" = "FinancingStage"
    ) %>%
    datatable(selection = "single", style = "default")
}

# 6. Most layoffs (General)
plot_most_layoffs <- function(df, top) {
  df <- df %>%
    group_by(company) %>%
    mutate(total_layoffs = sum(layoff_number)) %>%
    distinct(company, .keep_all = TRUE) %>%
    ungroup() %>%
    slice_max(order_by = total_layoffs, n = top) %>%
    select(company, industry, headquarters, financing_stage, total_layoffs)

  center <- circleProgressiveLayout(df$total_layoffs)
  polygon <- circleLayoutVertices(center)

  center <- center %>%
    bind_cols(df) %>%
    mutate(row = row_number())

  polygon <- polygon %>%
    left_join(center[, c("company", "industry", "headquarters", "financing_stage", "total_layoffs", "row")],
      by = c("id" = "row")
    )

  p <- ggplot() +
    geom_polygon(
      data = polygon, aes(x = x, y = y, group = id, fill = -id, text = paste(
        company,
        "\nIndustry -", industry,
        "\nHeadquarters -", headquarters,
        "\nFinancing Stage -", financing_stage,
        "\nTotal Layoffs -", comma(total_layoffs)
      )),
      colour = "black", show.legend = F, alpha = 0.8
    ) +
    geom_text(data = center, aes(x = x, y = y, label = paste(
      company,
      "\nCuts:", comma(total_layoffs)
    ), text = paste(
      company,
      "\nIndustry -", industry,
      "\nHeadquarters -", headquarters,
      "\nFinancing Stage -", financing_stage,
      "\nTotal Layoffs -", comma(total_layoffs)
    ))) +
    scale_fill_gradient(low = "#FFDAB9", high = "orangered3") +
    theme(axis.ticks = element_blank()) +
    labs(x = "", y = "")

  p <- ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(font = list(size = 15), bgcolor = "black", align = "auto"),
      hoveron = "points+fills",
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, showline = FALSE),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = list(l = 0, r = 5, t = 0, b = 0)
    )

  return(p)
}

# 7. Layoffs by Financing Stage (General)
plot_layoff_v_fs <- function(df) {
  p <- df %>%
    ggplot(aes(x = reorder(financing_stage, layoff_number, median, decreasing = T), y = layoff_number)) +
    geom_boxplot(fill = "orangered3", color = "black", alpha = 0.8) +
    scale_y_log10(label = comma) +
    labs(
      x = "Financing Stage",
      y = "Number of Layoffs (Log scale)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}

# 8. Summary table of Layoffs by Financing Stage (General)
layoff_financing_summary <- function(df) {
  df %>%
    rename("Financing Stage" = "financing_stage") %>%
    datatable(
      rownames = FALSE, selection = "single", style = "default",
      options = list(lengthChange = FALSE, pageLength = 12)
    )
}

# 9. Tech Layoffs by Month and Year (General)
plot_layoff_main <- function(df, selected) {
  sub_df <- df %>%
    filter(selected)

  df %>%
    ggplot(aes(x = announcement_date)) +
    geom_col(aes(y = number_of_layoffs),
      fill = "orangered3", width = 0.35, position = position_nudge(x = -0.2), col = "black", alpha = 0.1
    ) +
    geom_col(aes(y = number_of_companies * 500),
      fill = "deepskyblue4", width = 0.35, position = position_nudge(x = 0.2), col = "black", alpha = 0.1
    ) +
    geom_col(
      data = sub_df, aes(x = announcement_date, y = number_of_layoffs, fill = "Layoffs"),
      width = 0.35, position = position_nudge(x = -0.2), col = "black", alpha = 0.8
    ) +
    geom_col(
      data = sub_df, aes(x = announcement_date, y = number_of_companies * 500, fill = "Companies"),
      width = 0.35, position = position_nudge(x = 0.2), col = "black", alpha = 0.8
    ) +
    scale_fill_manual(
      values = c("Layoffs" = "orangered3", "Companies" = "deepskyblue4"),
      labels = c("Layoffs", "Companies"),
      name = ""
    ) +
    scale_y_continuous(
      label = comma, expand = c(0, 0, 0.1, 0.1),
      sec.axis = sec_axis(transform = ~ . / 500, name = "Total Companies")
    ) +
    labs(x = "Period", y = "Total Layoffs") +
    theme(
      axis.title.y.right = element_text(color = "deepskyblue4"),
      axis.text.y.right = element_text(color = "deepskyblue4"),
      axis.title.y.left = element_text(color = "orangered3"),
      axis.text.y.left = element_text(color = "orangered3"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
    ) +
    guides(fill = guide_legend(override.aes = list(fill = c("orangered3", "deepskyblue4"))))
}

# 10. Helper function - Setup for heatmap
heatmap_setup <- function(df) {
  df <- df %>%
    select(company, layoff_number, announcement_date, industry)

  dates <- df %>%
    pull(announcement_date) %>%
    unique()

  industries <- df %>%
    pull(industry) %>%
    unique()

  temp_df <- data.frame(
    industry = rep(industries, each = length(dates)),
    announcement_date = rep(dates, times = length(industries))
  )

  final_df <- df %>%
    group_by(industry, announcement_date) %>%
    summarize(layoff_mean = mean(layoff_number), .groups = "drop") %>%
    full_join(temp_df, by = c("industry", "announcement_date")) %>%
    mutate(layoff_mean = ifelse(is.na(layoff_mean), 0, layoff_mean)) %>%
    arrange(industry, announcement_date)
}

# 11. Tech Layoffs by Subindustry (General)
plot_layoff_v_subindustry <- function(df) {
  df <- heatmap_setup(df)

  p <- df %>%
    ggplot() +
    geom_tile(
      aes(x = announcement_date, y = reorder(industry, layoff_mean), fill = log(1 + layoff_mean)),
      color = "white"
    ) +
    scale_fill_gradient(low = "#FFDAB9", high = "orangered3") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(margin = margin(b = 15))
    ) +
    labs(
      x = "Period",
      y = "Subindustry",
      fill = "Log(1 + mean layoffs)"
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))
  
  ggsave(file="subindustry.svg", plot=p, width = 20, height= 12)
  return(p)
}

# 12. Helper function - Data for layoffs v. financing stage
financing_stage_data <- function(df) {
  df %>%
    filter(financing_stage != "Unknown") %>%
    group_by(financing_stage) %>%
    filter(n() > 5) %>%
    ungroup()
}

# 13. City-based view of tech layoffs
plot_layoff_city <- function(df) {
  plot_ly(df,
    labels = ~city, values = ~layoffs_city, type = "pie",
    textposition = "inside",
    textinfo = "label+percent",
    insidetextorientation = "radial",
    hoverinfo = "text",
    text = ~ paste(city, "<br>Number of Layoffs:", comma(layoffs_city)),
    marker = list(line = list(color = "black", width = 1)),
    showlegend = F
  ) %>%
    layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      legend = list(font = list(size = 18)),
      font = list(size = 18),
      hoverlabel = list(font = list(size = 15), bgcolor = "black", align = "auto"),
      margin = list(l = 0, r = 0.7, t = 0, b = 0),
      plot_bgcolor = "rgba(0,0,0,0)", # Set plot background color to transparent
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


# Main UI for the Shiny App
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = tagList(
    span(class = "logo-lg", "TechQuake")
  )),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      startTab = "general",
      menuItem("General View", tabName = "general", icon = icon("industry", lib = "font-awesome")),
      menuItem("Company View", tabName = "company", icon = icon("briefcase", lib = "font-awesome")),
      menuItem("App Information", tabName = "info", icon = icon("circle-exclamation", lib = "font-awesome"), selected = T)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(ui_stylings)
      )
    ),
    tabItems(

      # General tab (main view)
      tabItem(
        tabName = "general",
        fluidRow(
          column(
            width = 10,
            h1("General Statistics"),
            gen_instructions
          ),
          column(
            width = 2,
            actionButton("refresh", "Refresh",
              icon = icon("refresh"),
              style = "margin-top: 67px; float: right; font-size: 16px"
            )
          )
        ),
        br(),
        fluidRow(
          box(
            title = p(strong("Tech Layoffs over Time"), style = box_title_styles),
            footer = p(strong(uiOutput("dynamic_reminder")), style = box_footer_styles),
            status = "primary", width = 12, main_plot_instructions,
            plotOutput("time_graph_layoffs",
              brush = brushOpts("plot_brush", direction = "x"),
              height = 500
            )
          )
        ),
        fluidRow(
          box(
            title = p(strong("Tech Companies with Most Layoffs"), style = box_title_styles),
            status = "primary", width = 12, most_layoff_add_info,
            selectInput("choose_top", p("Select Cutoff"),
              width = "350px",
              choices = c(
                "Top 5" = "t5",
                "Top 10" = "t10",
                "Top 20" = "t20"
              )
            ),
            plotlyOutput("most_layoffs", height = 500)
          )
        ),
        fluidRow(
          column(6, selectInput("add_viz", p("Select Additional Visualization Type"),
            width = "350px",
            choices = c(
              "City-based" = "city",
              "Financing stage-based" = "financing_stage",
              "Subindustry-based" = "industry"
            )
          )),
          column(6, conditionalPanel(
            condition = "input.add_viz == 'city'",
            uiOutput("dynamic_city_choices")
          ))
        ),
        conditionalPanel(
          condition = "input.add_viz == 'city'",
          fluidRow(
            box(
              title = p(strong("Overall Concentration of US Tech Layoffs"),
                style = box_title_styles
              ),
              status = "primary", width = 6, style = "max-height: 670px;",
              plotlyOutput("layoff_all_cities", height = 550)
            ),
            box(
              title = p(strong("Comparing Layoffs between Selected Cities"),
                style = box_title_styles
              ),
              status = "primary", width = 6, style = "max-height: 650px;",
              plotlyOutput("layoff_selected_cities", height = 550)
            )
          )
        ),
        conditionalPanel(
          condition = "input.add_viz == 'financing_stage'",
          fluidRow(
            box(
              title = p(strong("Distribution of Layoffs by Financing Stage"),
                style = box_title_styles
              ),
              footer = p(strong("(Only shows distribution of financing stages with more than 5 observations)"),
                style = box_footer_styles
              ),
              status = "primary", width = 6, style = "min-height: 635px;",
              plotOutput("layoff_financing_stage", height = 600)
            ),
            box(
              title = p(strong("Layoff Distribution Summary"),
                style = box_title_styles
              ),
              status = "primary", width = 6, style = "min-height: 687px;",
              div(DTOutput("dt_financing_layoff_sum"), style = "font-size: 16px;
                  font-family: 'Roboto', sans-serif;")
            )
          )
        ),
        conditionalPanel(
          condition = "input.add_viz == 'industry'",
          fluidRow(
            box(
              title = p(strong("Tech Layoffs by Subindustries"),
                style = box_title_styles
              ),
              status = "primary", width = 12,
              plotOutput("layoff_subindustry", height = 650)
            )
          )
        )
      ),

      # Company-specific tab
      tabItem(
        tabName = "company",
        h1("Company-specific Statistics"),
        company_instructions,
        selectizeInput("company", p("Select a company"),
          choices = sort(common_ticker$Name),
          selected = sort(common_ticker$Name)[1], multiple = T, options = list(maxItems = 2)
        ),
        br(),
        fluidRow(
          box(
            title = p(strong("Tech Layoffs Through the Lens of the Stock Market"), style = box_title_styles),
            status = "primary", width = 12,
            plotOutput("price_plot", height = 550)
          )
        ),
        fluidRow(
          box(
            title = p(strong("Layoff Records"), style = box_title_styles),
            status = "primary", width = 12,
            div(DTOutput("dt_layoffs_company"), style = "font-size: 16px;
                  font-family: 'Roboto', sans-serif;")
          )
        )
      ),

      # Information tab
      tabItem(
        tabName = "info",
        h1("App Information"),
        br(),
        fluidRow(
          box(
            title = p(strong("Welcome, User!"), style = box_title_styles),
            app_desc, status = "primary", width = 12
          )
        )
      )
    )
  )
)


# Main server for the Shiny App
server <- function(input, output, session) {
  # Selected announcement dates
  selected <- reactiveVal(rep(T, nrow(layoff_main_data())))

  # Filter dates for General view; if user tries to not highlight any bar charts, program defaults
  # to including all periods
  observeEvent(input$plot_brush, {
    if (sum(brushedPoints(layoff_main_data(), input$plot_brush, allRows = T)$selected_) == 0) {
      session$resetBrush("plot_brush")
      selected(rep(T, nrow(layoff_main_data())))
    } else if (sum(brushedPoints(layoff_main_data(), input$plot_brush, allRows = T)$selected_) > 0) {
      selected(brushedPoints(layoff_main_data(), input$plot_brush, allRows = T)$selected_)
    }
  })

  # Filtered data by announcement date
  filtered_data <- reactive({
    layoff %>%
      filter(announcement_date %in% get_selected(selected())) %>%
      drop_na(layoff_number)
  })

  # Setups for city-based visualization
  all_cities_data <- reactive({
    filtered_data() %>%
      mutate(total_layoffs = sum(layoff_number)) %>%
      group_by(city) %>%
      mutate(layoffs_city = sum(layoff_number)) %>%
      ungroup() %>%
      mutate(percentage = layoffs_city / total_layoffs * 100) %>%
      select(city, layoffs_city, percentage) %>%
      distinct() %>%
      mutate(city = ifelse(percentage < 2, "Other", city)) %>%
      group_by(city) %>%
      summarise(layoffs_city = sum(layoffs_city), percentage = sum(percentage)) %>%
      arrange(desc(percentage))
  })

  selected_cities_data <- reactive({
    filtered_data() %>%
      filter(city %in% input$choose_cities) %>%
      group_by(city) %>%
      mutate(layoffs_city = sum(layoff_number)) %>%
      select(city, layoffs_city) %>%
      distinct() %>%
      arrange(desc(layoffs_city))
  })

  # Get ticker of the selected company
  selected_ticker <- reactive({
    common_ticker$Ticker[common_ticker$Name %in% input$company]
  })

  # Refresh General View tab
  observeEvent(input$refresh, {
    session$resetBrush("plot_brush")
    selected(rep(T, nrow(layoff_main_data())))
    updateSelectInput(session, "choose_top", selected = "t5")
    updateSelectInput(session, "add_viz", selected = "city")
    updateSelectizeInput(session, "choose_cities", selected = sort(unique(filtered_data()$city))[1:2])
  })

  # Resets previous tab's inputs when visiting other tabs
  observeEvent(input$tabs, {
    reset("choose_top")
    reset("company")
    reset("add_viz")
    reset("choose_cities")
    session$resetBrush("plot_brush")
    selected(rep(T, nrow(layoff_main_data())))
  })

  # Resets the city selections when changing to other visualization options
  observeEvent(input$add_viz, {
    updateSelectizeInput(session, "choose_cities", selected = sort(unique(filtered_data()$city))[1:2])
  })

  # Reminds the users about the period chosen
  output$dynamic_reminder <- renderUI({
    get_dates <- get_selected(selected())

    start_date <- get_dates[1]
    end_date <- get_dates[length(get_dates)]
    updated_footer <- paste("(Data below covers the period from", start_date, "to", end_date, ")")

    p(strong(updated_footer), style = box_footer_styles)
  })

  # Available choices for city
  output$dynamic_city_choices <- renderUI({
    selectizeInput("choose_cities", p("City-based Exclusive Option: Select Up to Five Cities"),
      width = "500px", choices = sort(unique(filtered_data()$city)),
      selected = sort(unique(filtered_data()$city))[1:2],
      multiple = T, options = list(maxItems = 5)
    )
  })

  # Data tables
  output$dt_layoffs_company <- renderDT({
    layoff_records_company(selected_ticker())
  })

  output$dt_financing_layoff_sum <- renderDT({
    num_sum <- financing_stage_data(filtered_data()) %>%
      mutate(financing_stage = as.factor(financing_stage)) %>%
      mutate(financing_stage = fct_reorder(financing_stage, layoff_number, median, .desc = T)) %>%
      group_by(financing_stage) %>%
      summarize(
        Maximum = max(layoff_number), Q3 = unname(quantile(layoff_number, probs = 0.75)),
        Median = median(layoff_number), Mean = round(mean(layoff_number), 2),
        Q1 = unname(quantile(layoff_number, probs = 0.25)), Minimum = min(layoff_number)
      )

    layoff_financing_summary(num_sum)
  })

  # Render Plots
  output$price_plot <- renderPlot({
    company_ticker_str_builder <- input$company
    plot_price(selected_ticker(), company_ticker_str_builder)
  })

  output$time_graph_layoffs <- renderPlot({
    plot_layoff_main(layoff_main_data(), selected())
  })

  output$most_layoffs <- renderPlotly({
    get_cutoff <- as.numeric(str_extract(input$choose_top, "[0-9]+"))
    plot_most_layoffs(filtered_data(), get_cutoff)
  })

  output$layoff_financing_stage <- renderPlot({
    plot_layoff_v_fs(financing_stage_data(filtered_data()))
  })

  output$layoff_subindustry <- renderPlot({
    plot_layoff_v_subindustry(filtered_data())
  })

  output$layoff_selected_cities <- renderPlotly({
    plot_layoff_city(selected_cities_data())
  })

  output$layoff_all_cities <- renderPlotly({
    plot_layoff_city(all_cities_data())
  })
}

# Runs the app
app <- shinyApp(ui, server)
