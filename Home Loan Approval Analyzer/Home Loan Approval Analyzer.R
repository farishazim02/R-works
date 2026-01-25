
# Q&A:

# 1) When I choose a different plot / PDP type, the previous graph disappears. Why?
#    A: That is intentional so that audiences can focus only on the relevant plot / PDP type.

# 2) Why does the graph disappear when I change the grouping variable for the categorical PDP
#    graph but not for other types (Numerical PDP / CP)?
#    A: The explanatory variable selection for the categorical PDP graph is reactive. In my
#       opinion, it does not make so much sense to choose the same grouping/explanatory variable.
#       Due to it being dependent on the grouping variable (which results in passing a reactive
#       expression for the `choices =` parameter), It seems as if it is triggering renderPlot twice
#       and resetting the graph. Unfortunately, I was not able to find a fix / not sure if there is a fix.


# Execute this first!
#install.packages("remotes")
#remotes::install_version("xgboost", version = "1.7.8.1")

required_packages <- c("tidyverse", "shiny", "shinyWidgets", "bslib", "DT", 
                       "plotly", "lubridate", "stringr", "scales", "rsconnect", 
                       "imbalance", "tidymodels", "caret", "DALEX", "Metrics")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages) != 0){
  install.packages(new_packages[!(new_packages == "imbalance")])
  
  # Please install the appropriate version of Rtools first (based on R version)
  url <- "http://cran.r-project.org/src/contrib/Archive/imbalance/imbalance_1.0.2.1.tar.gz"
  imbalance_file <- "imbalance_1.0.2.1.tar.gz"
  download.file(url = url, destfile = imbalance_file)
  
  # imbalance library dependencies
  install.packages(c("bnlearn", "KernelKnn", "smotefamily", "FNN", "C50"))
  
  install.packages(pkgs = imbalance_file, type = "source", repos = NULL)
  unlink(imbalance_file)
}


# Libraries used
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib)
library(DT)
library(plotly)
library(lubridate)
library(stringr)
library(scales)
library(rsconnect)
library(imbalance)
library(tidymodels)
library(caret)
library(DALEX)
library(Metrics)
library(xgboost)

# Preliminary setup
# Ensures reproducibility
set.seed(436)

my_theme <- theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(
      size = 20, margin =
        margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")
    ),
    strip.text = element_text(size = 20),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F7F7F7"),
    panel.border = element_rect(fill = NA, color = "#0c0c0c", linewidth = 0.6),
    legend.position = "bottom",
    panel.spacing = unit(15, "pt")
  )

theme_set(my_theme)

options(warn = -1)

# Data cleaning/setup - From https://www.kaggle.com/datasets/rishikeshkonapure/home-loan-approval
loan_train <- read_csv("loan_sanction_train.csv") %>%
  select(-Loan_ID) %>%
  filter(ApplicantIncome < 23000 & CoapplicantIncome < 20000) # Remove outlier values

# Possible values for columns with missing values
gender <- unique(loan_train$Gender[!is.na(loan_train$Gender)])
married <- unique(loan_train$Married[!is.na(loan_train$Married)])
dependents <- unique(loan_train$Dependents[!is.na(loan_train$Dependents)])
self_employed <- unique(loan_train$Self_Employed[!is.na(loan_train$Self_Employed)])
loan_term <- unique(loan_train$Loan_Amount_Term[!is.na(loan_train$Loan_Amount_Term)])
credit_history <- unique(loan_train$Credit_History[!is.na(loan_train$Credit_History)])
missing_loan <- mean(loan_train$LoanAmount, na.rm = T)

# Data imputation
loan_train <- loan_train %>%
  mutate(
    Gender = ifelse(is.na(Gender), sample(gender, n(), replace = T), Gender),
    Married = ifelse(is.na(Married), sample(married, n(), replace = T), Married),
    Dependents = ifelse(is.na(Dependents), sample(dependents, n(), replace = T), Dependents),
    Self_Employed = ifelse(is.na(Self_Employed), sample(self_employed, n(), replace = T), Self_Employed),
    Loan_Amount_Term = ifelse(is.na(Loan_Amount_Term), sample(loan_term, n(), replace = T), Loan_Amount_Term),
    Credit_History = ifelse(is.na(Credit_History), sample(credit_history, n(), replace = T), Credit_History),
    LoanAmount = ifelse(is.na(LoanAmount), missing_loan, LoanAmount)
  )

education <- unique(loan_train$Education)
property_area <- unique(loan_train$Property_Area)

# Oversampling to ensure train data is balanced
temp_df <- loan_train %>%
  select(Loan_Status, where(is.numeric), -Credit_History) %>%
  rename("Class" = "Loan_Status") %>%
  mutate(Class = as.factor(Class)) %>%
  as.data.frame()

temp_df <- mwmote(temp_df, numInstances = 228) %>%
  mutate(
    Gender = sample(gender, n(), replace = T),
    Married = sample(married, n(), replace = T),
    Dependents = sample(dependents, n(), replace = T),
    Self_Employed = sample(self_employed, n(), replace = T),
    Credit_History = sample(credit_history, n(), replace = T),
    Education = sample(education, n(), replace = T),
    Property_Area = sample(property_area, n(), replace = T)
  ) %>%
  rename("Loan_Status" = "Class")

loan_train <- rbind(loan_train, temp_df)

# Ensure data is in the right format
loan_train <- loan_train %>%
  mutate(across(where(is.character), as.factor),
    Credit_History = as.factor(ifelse(Credit_History == 1, "Meets Guidelines", "Does Not Meet Guidelines"))
  ) %>%
  rename(
    "Loan Status" = "Loan_Status",
    "Self-employed" = "Self_Employed",
    "Monthly Applicant Income ($)" = "ApplicantIncome",
    "Monthly Co-applicant Income ($)" = "CoapplicantIncome",
    "Loan Amount (1000*$)" = "LoanAmount",
    "Loan Term (Months)" = "Loan_Amount_Term",
    "Credit History" = "Credit_History",
    "Property Area" = "Property_Area"
  )

# Choices for group
possible_grouping <- loan_train %>%
  select(where(is.factor), -`Loan Status`) %>%
  colnames()

# Numeric variables
num_vars <- loan_train %>%
  select(where(is.numeric)) %>%
  colnames()

# Levels for numerical explanatory variables
num_level <- c(
  "Monthly Applicant Income ($)",
  "Monthly Co-applicant Income ($)", "Loan Amount (1000*$)",
  "Loan Term (Months)"
)

# Levels for categorical explanatory variables
cat_level <- c(
  "Gender", "Married", "Dependents", "Education",
  "Self-employed", "Credit History", "Property Area"
)

# Machine Learning

# Fit an extreme gradient boosting model
tune_grid <- expand.grid(
  nrounds = 50,
  max_depth = 2,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.75
)


# 84% accuracy and 91% AUC score on training data
xgb_fit <- train(`Loan Status` ~ .,
  data = loan_train, method = "xgbTree", tuneGrid = tune_grid,
  verbose = FALSE, verbosity = 0
)

preds <- predict(xgb_fit, loan_train)
accuracy_train <- mean(preds == loan_train$`Loan Status`)

preds_prob <- predict(xgb_fit, loan_train, type = "prob")[, 2]
loan_status <- loan_train %>%
  mutate(`Loan Status` = ifelse(`Loan Status` == "Y", 1, 0)) %>%
  pull(`Loan Status`)

auc_train <- auc(loan_status, preds_prob)

# Explainer
explanation <- explain(xgb_fit,
  data = loan_train[, -12],
  y = loan_train$`Loan Status` == "Y"
)


# All texts

descriptive_text <- p(
  style = "text-align: justify;",
  "Explore the relationship between various customer information and one's approval
for a home loan. The underlying model is trained using extreme gradient boosting
with a satisfactory ~85% training data accuracy."
)

section1 <- HTML(
  "<strong><span style = 'color: #77A690;'>Instructions:</span></strong><br>"
)

instructions <- p(
  style = "text-align: justify;",
  "Start by selecting the metrics based on your analytical objectives!
  Click the 'Generate' button to generate the corresponding graph.
  Also, use our predictive tool to see whether your loan request is approved or not."
)

section2 <- HTML(
  "<strong><span style = 'color: #77A690;'>Notes:</span></strong><br>",
)

notes <- p(
  style = "text-align: justify;",
  "i. The 'Explanatory Variables' option will appear after choosing plot type and grouping variable.", br(),
  "ii. The available choices for this option are dependent on the chosen plot type and the grouping variable.", br(),
  "iii. Categorical explanatory variables can only be visualized side-by-side with other categorical explanatory variables.
  The same applies for numerical explanatory variables. (Only for PDP)", br(), br()
)

predictive_tool_instructions <- p(
  style = "text-align: justify;",
  "Enter all the required information and then click 'Predict'."
)


# Helper functions:

# 1) Coloring for each group
groups_color <- function(feature) {
  color_label <- list()

  color_label[["Gender"]] <- c("#c90076", "#303CEE")
  color_label[["Married"]] <- c("#9B110E", "#1B9E77")
  color_label[["Dependents"]] <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00")
  color_label[["Education"]] <- c("#0F52BA", "#FFBF00")
  color_label[["Self-employed"]] <- c("#9B110E", "#1B9E77")
  color_label[["Credit History"]] <- c("#9B110E", "#1B9E77")
  color_label[["Property Area"]] <- c("#006400", "#808000", "#4682B4")

  return(color_label[[feature]])
}

# 2) Plot the PDP (categorical) graph based on user customization
pdpcat_builder <- function(type, group, exp_vars, explainer) {
  profile <- make_profile(group, exp_vars, explainer)

  p <- plot(profile, geom = type) +
    my_theme +
    scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
    facet_wrap(~ factor(`_vname_`, levels = cat_level), ncol = 2, scales = "free_x") +
    labs(
      subtitle = "", y = "Average Probability of Loan Approval",
      title = "Partial Dependence Profile for Loan Approval"
    )

  if (group == "None") {
    return(p)
  } else {
    p <- p +
      scale_fill_manual(values = groups_color(group)) +
      labs(fill = group)

    return(p)
  }
}

# 3) Plot the CP/PDP (numerical) graph based on user customization
cp_pdpnum_builder <- function(type, group, exp_vars, explainer) {
  profile <- make_profile(group, exp_vars, explainer)

  p <- plot(profile, geom = type) +
    my_theme +
    scale_x_continuous(label = comma, expand = c(0.01, 0, 0.01, 0)) +
    scale_y_continuous(expand = c(0.01, 0, 0.01, 0)) +
    facet_wrap(~ factor(`_vname_`, levels = num_level), ncol = 2, scales = "free_x") +
    labs(
      subtitle = "", y = "Probability of Loan Approval",
      title = "Ceterus Paribus Profile for Loan Approval"
    )

  if (group == "None") {
    return(p)
  } else {
    p <- p +
      scale_color_manual(values = groups_color(group)) +
      labs(color = group)

    return(p)
  }
}

# 4) Helper function to create and clean profile
make_profile <- function(group, exp_vars, explainer) {
  if (group == "None") {
    profile <- model_profile(explainer,
      variables = exp_vars
    )
  } else {
    profile <- model_profile(explainer,
      groups = group,
      variables = exp_vars
    )
  }

  profile$agr_profiles <- profile$agr_profiles %>%
    mutate(
      `_label_` = str_remove(`_label_`, "train.formula_")
    )

  return(profile)
}

# 5) Predict approval for home loan request
predict_loan <- function(gndr, mar, dep, ed, se, app_inc, coapp_inc, amt, term,
                         cred_hist, prop_area) {
  df <- data.frame(matrix(ncol = length(colnames(loan_train)) - 1, nrow = 1))
  colnames(df) <- colnames(loan_train[, c(-12)])

  vals <- c(gndr, mar, dep, ed, se, app_inc, coapp_inc, amt, term, cred_hist, prop_area)

  for (i in 1:length(df)) {
    df[i] <- vals[i]
  }

  df <- df %>%
    mutate(
      `Monthly Applicant Income ($)` = as.numeric(`Monthly Applicant Income ($)`),
      `Monthly Co-applicant Income ($)` = as.numeric(`Monthly Co-applicant Income ($)`),
      `Loan Amount (1000*$)` = as.numeric(`Loan Amount (1000*$)`),
      `Loan Term (Months)` = as.numeric(`Loan Term (Months)`)
    )

  prediction <- predict(xgb_fit, df)

  return(as.character(prediction))
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
  titlePanel("Home Loan Approval Analyzer"),
  sidebarLayout(
    sidebarPanel(
      section1,
      descriptive_text,
      instructions,
      br(),
      section2,
      notes,
      radioButtons(
        "cp_or_pdp", HTML("<b>1. Select a plot type</b>"),
        c(
          "Ceterus Paribus Profile" = "profiles",
          "Partial Dependence Profile" = "aggregates"
        ),
        selected = character(0)
      ),
      pickerInput("grouping", HTML("<b>2. Select a grouping feature</b>"),
        choices = c("No Grouping" = "None", possible_grouping),
        options = list(
          style = I("background-color: white;")
        )
      ),
      uiOutput("vars")
    ),
    mainPanel(
      plotOutput("model_viz", height = 750)
    )
  ),
  div(
    style = "margin-top: 30px; margin-bottom: 15px;",
    h2("Check if You Meet the Requirements!", style = "text-decoration: underline;"),
    predictive_tool_instructions,
    div(
      style = "margin-top: 15px; margin-bottom: 80px;",
      h3("General:"),
      fluidRow(
        column(3, selectInput("gender", "Gender", choices = c("", gender))),
        column(3, selectInput("married", "Marriage Status", choices = c("", "Married" = "Yes", "Not Married" = "No"))),
        column(3, selectInput("dependents", "Number of Dependents", choices = c("", dependents))),
        column(3, selectInput("educ", "Education Status", choices = c("", education)))
      ),
      h3("Work-related:"),
      fluidRow(
        column(3, selectInput("employed", "Employment Status", choices = c("",
          "Self-employed" = "Yes",
          "Employed" = "No"
        ))),
        column(3, numericInput("applicant_income", "Monthly Applicant Income ($)", 0, min = 0)),
        column(3, numericInput("coapplicant_income", "Monthly Co-applicant Income ($)", 0, min = 0)),
      ),
      h3("Loan Information:"),
      fluidRow(
        column(3, numericInput("loan_amount", "Loan Amount ($)", 0, min = 0)),
        column(3, numericInput("loan_term", "Loan Term (months)", 12, min = 12, step = 12, max = 480)),
        column(3, selectInput("credit_history", "Credit History", choices = c("", "Meets Guidelines", "Does Not Meet Guidelines"))),
        column(3, selectInput("property", "Property Area", choices = c("", property_area)))
      ),
      conditionalPanel(
        condition =
          "input.gender !== '' && input.married !== '' && input.dependents !== '' &&
        input.educ !== '' && input.employed !== '' && input.credit_history !== '' &&
        input.property !== '' && typeof input.applicant_income === 'number' &&
        typeof input.coapplicant_income === 'number' && typeof input.loan_term === 'number' &&
        typeof input.loan_amount === 'number'",
        br(),
        actionButton("predict", "Predict",
          class = "btn-primary",
          style = "color: #FFFFFF"
        ),
        br(),
        br(),
        htmlOutput("result")
      ),
    )
  )
)


# Main server for the Shiny App
server <- function(input, output, session) {
  # Grouping variable does not appear in the 'Explanatory Variables' choices
  available_choices <- reactive({
    possible_grouping[!(possible_grouping %in% input$grouping)]
  })

  # Resets grouping and PDP type selection when changing plot types
  observeEvent(input$cp_or_pdp, {
    updatePickerInput(session, "grouping", selected = "None")
    updateRadioButtons(session, "cat_or_num", selected = "category")
  })

  # Preserves previous selection for Categorical PDP graph and removes the selection if it is the same
  # as the grouping variable
  selected_explanatory_variable_pdp_cat <- reactiveVal()

  observeEvent(input$explain_variable_pdp_cat,
    {
      selected_explanatory_variable_pdp_cat(input$explain_variable_pdp_cat)
    },
    ignoreNULL = T
  )

  observeEvent(input$grouping,
    {
      current_choices <- available_choices()
      previous_selection <- selected_explanatory_variable_pdp_cat()
      valid_selections <- previous_selection[previous_selection %in% current_choices]
      not_selected <- current_choices[!current_choices %in% previous_selection]
      updatePickerInput(session, "explain_variable_pdp_cat",
        selected = if (length(valid_selections) > 0) valid_selections else not_selected[1]
      )
    },
    ignoreInit = T
  )

  # Renders UI depending on plot type selection
  output$vars <- renderUI({
    if (length(input$cp_or_pdp) != 0) {
      # CP graph customization options
      if (input$cp_or_pdp == "profiles") {
        tagList(
          pickerInput("explain_variable_cp", HTML("<b>3. Select explanatory variable(s)</b>"),
            choices = list(`Numerical Variables` = num_vars),
            selected = num_vars[1], multiple = T,
            options = list(
              style = I("background-color: white;")
            )
          ),
          conditionalPanel(
            condition = "input.explain_variable_cp.length > 0",
            actionButton("generate_cp", "Generate",
              class = "btn-primary",
              style = "color: #FFFFFF"
            )
          )
        )

        # PDP graph customization options
      } else if (input$cp_or_pdp == "aggregates") {
        tagList(
          radioButtons(
            "cat_or_num", HTML("<b>3a. Select type of explanatory variable(s)</b>"),
            c(
              "Categorical" = "category",
              "Numerical" = "numeric"
            ),
            selected = "category"
          ),
          uiOutput("pdp_specific")
        )
      }
    } else {
      NULL # Render nothing
    }
  })

  # Separated from above code so that I can use the value from input$cat_or_num
  output$pdp_specific <- renderUI({
    # Categorical PDP
    if (input$cat_or_num == "category") {
      tagList(
        pickerInput("explain_variable_pdp_cat", HTML("<b>3b. Select explanatory variable(s)</b>"),
          choices =
            list(`Categorical Variables` = available_choices()),
          selected = isolate(available_choices())[1], multiple = T,
          options = list(style = I("background-color: white;")),
        ),
        conditionalPanel(
          condition = "input.explain_variable_pdp_cat.length > 0",
          actionButton("generate_pdp_cat", "Generate",
            class = "btn-primary",
            style = "color: #FFFFFF"
          )
        )
      )

      # Numerical PDP -> almost the same logic as CP graph
    } else if (input$cat_or_num == "numeric") {
      tagList(
        pickerInput("explain_variable_pdp_num", HTML("<b>3b. Select explanatory variable(s)</b>"),
          choices =
            list(`Numerical Variables` = num_vars),
          selected = num_vars[1], multiple = T,
          options = list(style = I("background-color: white;"))
        ),
        conditionalPanel(
          condition = "input.explain_variable_pdp_num.length > 0",
          actionButton("generate_pdp_num", "Generate",
            class = "btn-primary",
            style = "color: #FFFFFF"
          )
        )
      )
    } else {
      NULL
    }
  })

  # To ensure graph updates only happen through the clicking of the 'Generate' action button

  plot_data_cp <- eventReactive(input$generate_cp,
    {
      req(length(input$explain_variable_cp) > 0)
      cp_pdpnum_builder(
        input$cp_or_pdp, input$grouping,
        input$explain_variable_cp, explanation
      )
    },
    ignoreNULL = T
  )

  plot_data_pdp_num <- eventReactive(input$generate_pdp_num,
    {
      req(length(input$explain_variable_pdp_num) > 0)
      cp_pdpnum_builder(
        input$cp_or_pdp, input$grouping,
        input$explain_variable_pdp_num, explanation
      ) +
        labs(
          title = "Partial Dependence Profile for Loan Approval",
          y = "Average Probability of Loan Approval"
        )
    },
    ignoreNULL = T
  )

  plot_data_pdp_cat <- eventReactive(input$generate_pdp_cat,
    {
      req(length(input$explain_variable_pdp_cat) > 0)
      pdpcat_builder(
        input$cp_or_pdp, input$grouping,
        input$explain_variable_pdp_cat, explanation
      )
    },
    ignoreNULL = T
  )

  # Renders the correct plot type
  output$model_viz <- renderPlot({
    if (length(input$cp_or_pdp) == 0) {
      NULL
    } else if (input$cp_or_pdp == "profiles") {
      plot_data_cp()
    } else if (input$cp_or_pdp == "aggregates") {
      if (length(input$cat_or_num) == 0) {
        NULL
      } else if (input$cat_or_num == "numeric") {
        plot_data_pdp_num()
      } else if (input$cat_or_num == "category") {
        plot_data_pdp_cat()
      }
    }
  })

  # Triggers only when user clicks the button
  predict_user <- eventReactive(input$predict,
    {
      predict_loan(
        input$gender, input$married, input$dependents, input$educ, input$employed,
        as.numeric(input$applicant_income), as.numeric(input$coapplicant_income),
        as.numeric(input$loan_amount / 1000), as.numeric(input$loan_term),
        input$credit_history, input$property
      )
    },
    ignoreNULL = T
  )

  # Prints the result of the prediction
  output$result <- renderText({
    if (predict_user() == "N") {
      paste(
        "Unfortunately, your loan approval is predicted to be",
        "<font color=\"#FF0000\"><b>", "UNSUCCESSFUL", "</b></font>"
      )
    } else if (predict_user() == "Y") {
      paste(
        "Congrats! Your loan approval is predicted to be",
        "<font color=\"#5CB85C\"><b>", "SUCCESSFUL", "</b></font>"
      )
    }
  })
}

# Runs the app
app <- shinyApp(ui, server)
