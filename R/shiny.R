#' Interactive shiny app to visualize results
#'
#' Interactive shiny app for visualization of results. The app can be
#' initialized with an \code{IBRAData} object. If no object is provided, truth
#' and results are loaded into the app from text files (see the Instructions tab
#' of the app for formatting instructions). Properly formatted text files can
#' also be obtained using the function \code{\link{IBRAData_to_text}}.
#'
#' @param ibradata An (optional) \code{IBRAData} object. If not given, the user
#'   can load results from text files.
#' @author Charlotte Soneson
#' @return Returns (and runs) an object representing the shiny app.
#' @import shiny
#' @export
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100),
#'                    row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)),
#'                     row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#'
#' \dontrun{
#' IBRAapp(ibradata)
#' }
IBRAapp <- function(ibradata = NULL) {
  ## ------------------------------------------------------------------ ##
  ##                          Define UI                                 ##
  ## ------------------------------------------------------------------ ##

  p_layout <-
    shinydashboard::dashboardPage(
      skin = "blue",
      shinydashboard::dashboardHeader(
        title = paste0("IBRA - Comparative evaluation ",
                       "of methods for ranking and binary assignment (v0.3.1)"),
        titleWidth = 800),
      shinydashboard::dashboardSidebar(
        width = 350,
        ## Settings and inputs for the truth
        shinydashboard::menuItem("Truth", icon = icon("database"),
                 ## Load the file containing the truth.
                 uiOutput("choose_truth_file"),
                 shinyBS::bsTooltip(
                   "truth", paste0("Select the file containing the true status",
                                   " of each feature. See the Instructions ",
                                   "tab for formatting instructions."),
                   "right", options = list(container = "body")),

                 ## Define the column giving the feature identifier
                 ## (for both truth and results)
                 uiOutput("choose_feature_id"),

                 ## Define the column giving the (binary) truth
                 uiOutput("choose_binary_truth"),

                 ## Define the column giving the (continuous) truth
                 uiOutput("choose_continuous_truth"),

                 ## Define the variable used to stratify the results,
                 ## based on the columns available in the truth file.
                 uiOutput("splitvar"),
                 shinyBS::bsTooltip(
                   "splitvar",
                   paste0("Select a feature attribute by which to stratify ",
                          "the result representations."),
                           "right", options = list(container = "body")),

                 ## Define the maximal number of categories to keep
                 ## in stratification.
                 uiOutput("choosemaxsplit"),
                 shinyBS::bsTooltip(
                   "choosemaxsplit",
                   paste0("Set the number of categories to show if the results",
                          " are stratified by a variable annotation. The most ",
                          "frequent categories with both positive and negative",
                          " instances will be retained"),
                   "right", options = list(container = "body")),

                 ## Decide whether or not to include the "overall" category when
                 ## stratifying the results.
                 uiOutput("chooseincludeoverall"),
                 shinyBS::bsTooltip(
                   "includeoverall",
                   paste0("Select whether or not to include the 'overall' ",
                          "class when showing stratified results."),
                   "right", options = list(container = "body"))),

        ## Settings and inputs for results
        shinydashboard::menuItem("Results", icon = icon("folder-o"),
                 ## Load the file containing the results.
                 uiOutput("choose_result_file"),
                 shinyBS::bsTooltip(
                   "file1", paste0("Select file containing results from one or",
                                   " multiple methods. See the Instructions ",
                                   "tab for formatting instructions."),
                   "right", options = list(container = "body")),

                 ## Decide which methods to include in the results.
                 ## Depends on the loaded result files.
                 uiOutput("columns"),
                 shinyBS::bsTooltip(
                   "columns",
                   paste0("Select the methods for which to show the results."),
                   "right", options = list(container = "body")),

                 ## Decide whether to base evaluations on all genes in
                 ## truth table, or only on variables for which both a truth
                 ## value and a result is provided
                 checkboxInput(
                   inputId = "onlyshared",
                   label = paste0("Calculate performance based only on ",
                                  "features shared between truth and result ",
                                  "tables."),
                   value = FALSE),
                 shinyBS::bsTooltip(
                   "onlyshared",
                   paste0("Calculate performance based only on ",
                          "features shared between truth and result ",
                          "tables. Otherwise, all features in the ",
                          "truth table will be used."),
                   "right", options = list(container = "body")),

                 actionButton("goButton", "Start calculation!",
                              icon = icon("plane"))),

        ## Plot settings
        shinydashboard::menuItem("Plot settings", icon = icon("paint-brush"),
                 ## Choose color scheme
                 selectInput(
                   "colorscheme", "Select color palette",
                   c("hue_pal", "Accent (max 7 methods)",
                     "Dark2 (max 7 methods)", "Paired (max 11 methods)",
                     "Pastel1 (max 8 methods)", "Pastel2 (max 7 methods)",
                     "Set1 (max 8 methods)", "Set2 (max 7 methods)",
                     "Set3 (max 11 methods)", "rainbow", "heat", "terrain",
                     "topo", "cm"),
                   selectize = TRUE),
                 shinyBS::bsTooltip(
                   "colorscheme",
                   paste0("Choose color palette. Some palettes are only ",
                          "applicable if the number of methods ",
                          "(or method/stratification level combinations) is ",
                          "below a certain threshold. If this threshold is ",
                          "exceeded, the colorscheme will default to hue_pal."),
                   "right", options = list(container = "body")),

                 ## Decide what to include in FDR/TPR plots.
                 checkboxGroupInput(
                   "plottype",
                   paste0("Display full curve and/or points in FDR/TPR and ",
                          "FDR/NBR plots"),
                   c("curve", "points"), c("points")),

                 ## Decide whether or not to facet plots.
                 checkboxGroupInput(
                   "facet_opt",
                   "Split plots into panels by stratifying variable",
                   "split", "split"),

                 ## Define the q-value thresholds to use in the plots.
                 textInput(inputId = "fdrthresholds", label = "FDR thresholds",
                           value = "0.01, 0.05, 0.1"),
                 shinyBS::bsTooltip("fdrthresholds",
                           paste0("Specific FDR thresholds at which the ",
                                  "performance will be evaluated. ",
                                  "Separate multiple values with comma"),
                           "right", options = list(container = "body")),

                 ## Define the plot height (in pixels).
                 numericInput(inputId = "plotheight",
                              label = "Plot height (numeric, in pixels)",
                              value = 600, min = 200, max = 2000, step = 10),
                 shinyBS::bsTooltip("plotheight",
                           paste0("The height of the plots (in pixels). ",
                                  "Default 800."),
                           "right", options = list(container = "body")),

                 ## Define the pointsize used in the plots.
                 numericInput(inputId = "pointsize", label = "Point size",
                              value = 5),
                 shinyBS::bsTooltip("pointsize",
                           paste0("The point size used in the plots."),
                           "right", options = list(container = "body")),

                 ## Define the fontsize used in the panel headers.
                 numericInput(inputId = "stripsize",
                              label = "Font size for panel headers",
                              value = 15),
                 shinyBS::bsTooltip("stripsize",
                           paste0("The font size used for panel headers in",
                                  " facetted plots."),
                           "right", options = list(container = "body")))
      ),

      ## Outputs
      shinydashboard::dashboardBody(fluidRow(
        shinydashboard::tabBox(
          width = 12,
          tabPanel("Instructions",
                   includeMarkdown(system.file("extdata", "instructions.md",
                                               package = "IBRA")),
                   value = "instructions"),

          tabPanel("TPR vs FDR", shinyBS::bsAlert("fdrtpr_message"),
                   uiOutput("plot.fdrtprcurve"),
                   fluidRow(
                     column(3, sliderInput(inputId = "xrange_fdrtpr",
                                           label = "x-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(3, sliderInput(inputId = "yrange_fdrtpr",
                                           label = "y-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(2, br(), downloadButton("export.fdrtprcurve",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.fdrtpr.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), uiOutput("fdrtpr.df.tsv.button"))
                   ),
                   DT::dataTableOutput("fdrtprcurve_click_info"),
                   textOutput("message"),
                   #textOutput("duplication_alert"),
                   value = "fdrtprcurve"),

          tabPanel("NBR vs FDR", shinyBS::bsAlert("fdrnbr_message"),
                   uiOutput("plot.fdrnbrcurve"),
                   fluidRow(
                     column(4, sliderInput(inputId = "xrange_fdrnbr",
                                           label = "x-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(2, br(), downloadButton("export.fdrnbrcurve",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.fdrnbr.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), uiOutput("fdrnbr.df.tsv.button"))
                   ),
                   DT::dataTableOutput("fdrnbrcurve_click_info"),
                   value = "fdrnbrcurve"),

          tabPanel("TPR", shinyBS::bsAlert("tpr_message"), uiOutput("plot.tpr"),
                   fluidRow(
                     column(4, sliderInput(inputId = "xrange_tpr",
                                           label = "x-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(2, br(), downloadButton("export.tpr",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.tpr.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.tpr.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("tpr_click_info"), value = "tpr"),

          tabPanel("FPR", shinyBS::bsAlert("fpr_message"), uiOutput("plot.fpr"),
                   fluidRow(
                     column(4, sliderInput(inputId = "xrange_fpr",
                                           label = "x-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(2, br(), downloadButton("export.fpr",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.fpr.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.fpr.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("fpr_click_info"), value = "fpr"),

          tabPanel("ROC", shinyBS::bsAlert("roc_message"), uiOutput("plot.roc"),
                   fluidRow(
                     column(3, sliderInput(inputId = "xrange_roc",
                                           label = "x-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(3, sliderInput(inputId = "yrange_roc",
                                           label = "y-axis limits",
                                           min = 0, max = 1, value = c(0, 1),
                                           step = 0.01)),
                     column(2, br(), downloadButton("export.roc",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.roc.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.roc.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("roc_click_info"), value = "roc"),

          tabPanel("False discovery curves", shinyBS::bsAlert("fpc_message"),
                   uiOutput("plot.fpc"),
                   fluidRow(
                     column(3, numericInput(inputId = "maxnfdc",
                                            label = "Maximal rank to display",
                                            value = 500, min = 10, max = 10000,
                                            step = 10)),
                     column(2, br(), downloadButton("export.fpc",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.fpc.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.fpc.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("fpc_click_info"), value = "fpc"),

          tabPanel("Correlation", shinyBS::bsAlert("corr_message"),
                   uiOutput("plot.corr"),
                   fluidRow(
                     column(3, sliderInput(inputId = "xrange_corr",
                                           label = "x-axis limits",
                                           min = -1, max = 1, value = c(-1, 1),
                                           step = 0.01)),
                     column(2, radioButtons(inputId = "corrtype",
                                            label = "Correlation measure",
                                            choices = c("pearson", "spearman"),
                                            selected = "spearman")),
                     column(2, br(), downloadButton("export.corr",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.corr.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.corr.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("corr_click_info"), value = "corr"),

          tabPanel("Scatter", shinyBS::bsAlert("scatter_message"),
                   uiOutput("plot.scatter"),
                   fluidRow(
                     column(2, checkboxInput("doflip", "Flip axes", FALSE),
                            checkboxInput("dolog", "Log-transform", FALSE)),
                     column(2, br(), downloadButton("export.scatter",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.scatter.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.scatter.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("scatter_click_info"),
                   value = "scatter"),

          tabPanel("Deviations", shinyBS::bsAlert("deviation_message"),
                   uiOutput("plot.deviation"),
                   fluidRow(
                     column(2, checkboxInput("dojitter",
                                             "Include jittered points", FALSE),
                            checkboxInput("dosquare",
                                          "Square deviations", FALSE)),
                     column(1, radioButtons(
                       inputId = "devtype", label = "Plot type",
                       choices = c("boxplot", "violinplot"),
                       selected = "violinplot")),
                     column(3, uiOutput("axislimitsdeviation")),
                     column(2, br(), downloadButton("export.deviation",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.deviation.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.deviation.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   DT::dataTableOutput("deviation_click_info"),
                   value = "deviation"),

          tabPanel("Venn diagram", shinyBS::bsAlert("overlap_message"),
                   uiOutput("plot.overlap"),
                   fluidRow(
                     column(1, radioButtons(inputId = "incltruth",
                                            label = "Include truth",
                                            choices = c("yes", "no"),
                                            selected = "yes")),
                     shinyBS::bsTooltip(
                       "incltruth",
                       paste0("Whether or nor to include the truth as a ",
                              "(perfect) method in the Venn diagrams. Note ",
                              "that maximally five methods (including the ",
                              "truth) can be included."),
                       "bottom", options = list(container = "body")),
                     column(3, numericInput(
                       inputId = "adjpVenn",
                       label = "Adjusted p-value threshold",
                       value = 0.05, min = 0, max = 1, step = 0.01)),
                     shinyBS::bsTooltip(
                       "adjpVenn",
                       paste0("The adjusted p-value threshold used to extract ",
                              "the sets of significant variables to use for ",
                              "the Venn diagram. "),
                       "bottom", options = list(container = "body")),
                     column(2, br(), downloadButton("export.overlap",
                                                    label = "Download plot")),
                     column(2, br(), downloadButton("export.overlap.df.rdata",
                                                    label = "Download Rdata")),
                     column(2, br(), downloadButton("export.overlap.df.tsv",
                                                    label = "Download tsv"))
                   ),
                   value = "overlap"),
          selected = "fdrtprcurve"
        )
      ))
    )

  options(shiny.maxRequestSize = 15*1024^2)
  ## ------------------------------------------------------------------ ##
  ##                          Define server                             ##
  ## ------------------------------------------------------------------ ##

  server_function <- function(input, output, session) {
    values <- reactiveValues()
    values$my_ibradata <- IBRAData()

    output$choose_truth_file <- renderUI({
      if (!is.null(ibradata)) {
        NULL
      } else {
        return(fileInput(inputId = "truth", label = "Load truth file",
                         accept = c("text/csv", "text/comma-separated-values",
                                    "text/tab-separated-values", "text/plain",
                                    ".csv", ".tsv"), multiple = FALSE))
      }
    })

    truthFile <- reactive({
      if (is.null(input$truth))
        return(NULL)
      trf <- read.delim(input$truth$datapath, header = TRUE, as.is = TRUE,
                        sep = "\t", quote = "", check.names = FALSE)
      isolate(values$my_ibradata <-
                IBRAData(truth = trf,
                         object_to_extend = values$my_ibradata))
      return(trf)
    })

    ## Render the UI element to choose the column containing the feature ID
    output$choose_feature_id <- renderUI({
      if (is.null(truthFile()))
        return(NULL)
      selectInput("feature_id", "Select column containing feature identifiers",
                  colnames(truthFile()), selectize = TRUE)
    })

    ## Render the UI element to choose the binary truth column
    output$choose_binary_truth <- renderUI({
      values$message <- NULL
      if (!is.null(ibradata)) {
        ccl <- sapply(truth(ibradata), class)
        only01 <- sapply(truth(ibradata), function(i) {
          if (all(i %in% c(0, 1, NA))) "binary"
          else "notbinary"
        })
      } else {
        if (is.null(truthFile()))
          return(NULL)
        ccl <- sapply(truthFile(), class)
        only01 <- sapply(truthFile(), function(i) {
          if (all(i %in% c(0, 1, NA))) "binary"
          else "notbinary"
        })
      }
      keepcols <- intersect(which(ccl %in% c("numeric", "integer")),
                            which(only01 == "binary"))
      if (length(keepcols) == 0) {
        isolate(values$message <- "No binary truth column")
      }
      if (!is.null(ibradata)) {
        selectInput("binary_truth", "Select column containing binary truth",
                    c(colnames(truth(ibradata))[keepcols], "none"),
                    selectize = TRUE)
      } else {
        selectInput("binary_truth", "Select column containing binary truth",
                    c(colnames(truthFile())[keepcols], "none"),
                    selectize = TRUE)
      }
    })

    ## Render the UI element to choose the continuous truth column
    output$choose_continuous_truth <- renderUI({
      values$message <- NULL
      if (!is.null(ibradata)) {
        ccl <- sapply(truth(ibradata), class)
      } else {
        if (is.null(truthFile()))
          return(NULL)
        ccl <- sapply(truthFile(), class)
      }
      keepcols <- which(ccl %in% c("numeric", "integer"))
      if (length(keepcols) == 0) {
        isolate(values$message <- "No continuous truth column")
        # return(NULL)
      }
      if (!is.null(ibradata)) {
        selectInput("cont_truth", "Select column containing continuous truth",
                    c("none", colnames(truth(ibradata))[keepcols]),
                    selectize = TRUE)
      } else {
        selectInput("cont_truth", "Select column containing continuous truth",
                    c("none", colnames(truthFile())[keepcols]),
                    selectize = TRUE)
      }
    })

    ## Render the UI element to choose which attribute (from the truth file)
    ## to split panels by.
    output$splitvar <- renderUI({
      if (!is.null(ibradata)) {
        selectInput("splv", "Select variable to stratify by",
                    c("none", colnames(truth(ibradata))), selectize = TRUE)
      } else {
        trf <- truthFile()
        if (is.null(trf))
          return(NULL)
        selectInput("splv", "Select variable to stratify by",
                    c("none", colnames(trf)), selectize = TRUE)
      }
    })

    ## Render the UI element to choose how many categories to include.
    output$choosemaxsplit <- renderUI({
      numericInput(inputId = "maxsplit",
                   label = "Maximum number of levels to show when stratifying",
                   value = 3, min = 1)
    })

    ## Render the UI element to determine whether the overall category should
    ## be included when stratifying
    output$chooseincludeoverall <- renderUI({
      radioButtons(inputId = "includeoverall",
                   label = "Include 'overall' class when stratifying",
                   choices = c("yes", "no"), selected = "yes")
    })

    ## Set message regarding missing/wrongly formatted status column
    output$message <- renderText({
      if (!is.null(values$message))
        values$message
      else
        NULL
    })

    output$choose_result_file <- renderUI({
      if (!is.null(ibradata)) {
        NULL
      } else {
        return(fileInput(inputId = "file1", label = "Add file with results",
                         accept = c("text/csv", "text/comma-separated-values",
                                    "text/tab-separated-values", "text/plain",
                                    ".csv", ".tsv"), multiple = FALSE))
      }
    })

    ## Generate the UI object for selecting methods to include, based on the
    ## uploaded result files.
    output$columns <- renderUI({
      if (!is.null(ibradata)) {
        allm <- unique(c(colnames(pval(ibradata)), colnames(padj(ibradata)),
                         colnames(score(ibradata))))
        isolate(values$my_ibradata <- ibradata)
        isolate(values$all_methods <- allm)
        isolate(values$my_ibradata <- calculate_adjp(values$my_ibradata))

        ## Create the UI element for selecting methods to include.
        checkboxGroupInput("cols", "Select methods",
                           isolate(values$all_methods),
                           isolate(values$all_methods))
      } else {
        rownames(truth(values$my_ibradata)) <-
          truth(values$my_ibradata)[, input$feature_id]

        ## Read file
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        v <- read.delim(inFile$datapath, header = TRUE, as.is = TRUE,
                        sep = "\t", quote = "", check.names = FALSE)

        ## Perform some checks on the file, if ok then continue
        if (res_check(v)) {
          ## Extract all columns corresponding to methods
          ii <- 1

          while(ii <= ncol(v)) {
            i <- colnames(v)[ii]
            coltype <- get_coltype(i)

            if (!(is.null(coltype))) {
              tmp <- v[i]
              rownames(tmp) <- v[, input$feature_id]
              i <- gsub(":score$", "", gsub(":adjP$", "", gsub(":P$", "", i)))
              colnames(tmp) <- i

              ## If a gene is present multiple times, keep only the smallest p
              ## (excluding missing/NaN values).
              tmp <- fix_duplicates(tmp, i)

              isolate(values$all_methods <- unique(c(values$all_methods, i)))

              if (coltype == "pval") {
                isolate(values$my_ibradata <-
                          IBRAData(pval = tmp,
                                   object_to_extend = values$my_ibradata))
              } else if (coltype == "padj") {
                isolate(values$my_ibradata <-
                          IBRAData(padj = tmp,
                                   object_to_extend = values$my_ibradata))
              } else if (coltype == "score") {
                isolate(values$my_ibradata <-
                          IBRAData(score = tmp,
                                   object_to_extend = values$my_ibradata))
              }
            }
            ii <- ii + 1
          }
          isolate(values$my_ibradata <- calculate_adjp(values$my_ibradata))

          ## Create the UI element for selecting methods to include.
          checkboxGroupInput("cols", "Select methods",
                             isolate(values$all_methods),
                             isolate(values$all_methods))
        }
      }
    })

    plotvalues_TPR <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        thrs <- sort(unique(as.numeric(gsub(" ", "",
                                            unlist(strsplit(input$fdrthresholds,
                                                            ","))))))
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "tpr", thrs = thrs,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_FDR <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        thrs <- sort(unique(as.numeric(gsub(" ", "",
                                            unlist(strsplit(input$fdrthresholds,
                                                            ","))))))
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "fdrtpr", thrs = thrs,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_FPR <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        thrs <- sort(unique(as.numeric(gsub(" ", "",
                                            unlist(strsplit(input$fdrthresholds,
                                                            ","))))))
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "fpr", thrs = thrs,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_ROC <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "roc", thrs = NULL,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_FPC <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "fpc", thrs = thrs,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_FDRTPR <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "fdrtprcurve", thrs = thrs,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_overlap <- reactive({
      if (input$goButton > 0 & input$binary_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = input$binary_truth,
                                     cont_truth = NULL,
                                     aspects = "overlap", thrs = NULL,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = input$adjpVenn))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_corr <- reactive({
      if (input$goButton > 0 & input$cont_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = NULL,
                                     cont_truth = input$cont_truth,
                                     aspects = "corr", thrs = NULL,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_scatter <- reactive({
      if (input$goButton > 0 & input$cont_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = NULL,
                                     cont_truth = input$cont_truth,
                                     aspects = "scatter", thrs = NULL,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues_deviation <- reactive({
      if (input$goButton > 0 & input$cont_truth != "none") {
        return(calculate_performance(values$my_ibradata,
                                     binary_truth = NULL,
                                     cont_truth = input$cont_truth,
                                     aspects = "deviation", thrs = NULL,
                                     splv = input$splv,
                                     maxsplit = input$maxsplit,
                                     onlyshared = input$onlyshared,
                                     thr_venn = NULL))
      } else {
        return(IBRAPerformance())
      }
    })

    plotvalues <- reactive({
      if (input$goButton > 0) {
        ## Put together results for current methods
        if (length(input$cols) != 0) {
          withProgress(message = "Calculating...", value = 0, {
            all_vals <-
              IBRAPerformance(tpr = tpr(plotvalues_TPR()),
                              fdrtpr = fdrtpr(plotvalues_FDR()),
                              fdrnbr = fdrtpr(plotvalues_FDR()),
                              fdrtprcurve = fdrtprcurve(plotvalues_FDRTPR()),
                              fdrnbrcurve = fdrnbrcurve(plotvalues_FDRTPR()),
                              fpr = fpr(plotvalues_FPR()),
                              roc = roc(plotvalues_ROC()),
                              fpc = fpc(plotvalues_FPC()),
                              overlap = overlap(plotvalues_overlap()),
                              maxsplit = input$maxsplit,
                              splv = input$splv,
                              corr = corr(plotvalues_corr()),
                              scatter = scatter(plotvalues_scatter()),
                              deviation = deviation(plotvalues_deviation()))

            all_vals <- prepare_data_for_plot(
              ibraperf = all_vals,
              keepmethods = input$cols,
              incloverall = (input$includeoverall == "yes"),
              colorscheme = input$colorscheme,
              facetted = ("split" %in% input$facet_opt),
              incltruth = (input$incltruth == "yes"))

            ## This is a bit of a hack, just to make the plots directly
            ## depend on the truth input. Otherwise, they will not be
            ## automatically updated if the truth file is changed
            ## (not until something else that directly affects the plots
            ## is changed as well, like the FDR thresholds or plot size etc).
            ## The title is rendered in white, so it is not visible.
            title <- input$truth$name

            list(all_vals = all_vals, title = title)
          })
        }
      } else {
        NULL
      }
    })

    ## -------------------------- OVERLAP ------------------------------ ##
    ## Generate Venn diagram plot
    output$plot.overlap <- renderUI({
      plotOutput("overlap", width = "100%",
                 height = paste0(input$plotheight, "px"))
    })

    ## Figure for exporting
    output$export.overlap <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, height = 12, width = 12)
        if (length(values$all_methods) == 0 | length(input$cols) == 0)
          return(NULL)
        plot_overlap(plotvalues()$all_vals)
        dev.off()
      })

    ## Data for exporting
    output$export.overlap.df.rdata <- downloadHandler(
      filename = "overlap-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.overlap.df.tsv <- downloadHandler(
      filename = "overlap-data.tsv",
      content = function(file) {
        overlap_df <- overlap(plotvalues()$all_vals)
        if (class(overlap_df) == "list") {
          overlap_df <- lapply(overlap_df,
                               function(w) cbind(feature = rownames(w), w))
          for (i in 1:length(overlap_df)) {
             overlap_df[[i]]$featureclass <- names(overlap_df)[i]
          }
          overlap_df <- do.call(rbind, overlap_df)
        }
        else {
          overlap_df <- cbind(feature = rownames(overlap_df), overlap_df)
        }
        write.table(overlap_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$overlap <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_overlap")
          shinyBS::createAlert(
            session, anchorId = "overlap_message",
            alertId = "message_overlap", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(overlap(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_overlap")
          shinyBS::createAlert(
            session, anchorId = "overlap_message",
            alertId = "message_overlap",
            content = paste0("Venn diagrams can not be displayed (check that ",
                             "binary_truth is not 'none', and that adjusted ",
                             " p-values are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else if ((class(overlap(plotvalues()$all_vals)) == "list" &&
                    ncol(overlap(plotvalues()$all_vals)[[1]]) > 5) |
                   (class(overlap(plotvalues()$all_vals)) == "data.frame" &&
                    ncol(overlap(plotvalues()$all_vals)) > 5)) {
          shinyBS::closeAlert(session, alertId = "message_overlap")
          shinyBS::createAlert(
            session, anchorId = "overlap_message",
            alertId = "message_overlap",
            content = paste0("Venn diagrams can not be constructed if ",
                             "more than five methods are selected ",
                             "(including the truth, if applicable)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_overlap")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          return(NULL)
        }

        plot_overlap(plotvalues()$all_vals)
      })
    })

    ## ---------------------------- TPR -------------------------------- ##
    output$plot.tpr <- renderUI({
      plotOutput("tpr", width = "100%", height = paste0(input$plotheight, "px"),
                 hover = "tpr_plot_click")
    })

    ## Figure for exporting
    output$export.tpr <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(tpr(plotvalues()$all_vals)))
          return(NULL)
        print(plot_tpr(ibraplot = plotvalues()$all_vals,
                       title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       pointsize = input$pointsize,
                       xaxisrange = input$xrange_tpr))
        dev.off()
      })

    ## Data for exporting
    output$export.tpr.df.rdata <- downloadHandler(
      filename = "tpr-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.tpr.df.tsv <- downloadHandler(
      filename = "tpr-data.tsv",
      content = function(file) {
        tpr_df <- isolate(tpr(plotvalues()$all_vals))
        write.table(tpr_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$tpr <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_tpr")
          shinyBS::createAlert(
            session, anchorId = "tpr_message",
            alertId = "message_tpr", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(tpr(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_tpr")
          shinyBS::createAlert(
            session, anchorId = "tpr_message",
            alertId = "message_tpr",
            content = paste0("TPR can not be displayed (check that ",
                             "binary_truth is not 'none', and that adjusted ",
                             "p-values are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_tpr")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(tpr(plotvalues()$all_vals))) {
          return(NULL)
        }

        plot_tpr(ibraplot = plotvalues()$all_vals, title = plotvalues()$title,
                 stripsize = input$stripsize, titlecol = "white",
                 pointsize = input$pointsize, xaxisrange = input$xrange_tpr)
      })
    })

    output$tpr_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(tpr(plotvalues()$all_vals)))
        return(NULL)
      all_data <- isolate(tpr(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$tpr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          yvar = "num_method", xvar = "TPR")
      } else {
        res <- nearPoints(all_data, input$tpr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, yvar = "num_method", xvar = "TPR")
      }
      fix_res(res, methodcol = "fullmethod", aspcts = c("TPR"),
              tabtype = "large")
    })

    ## ---------------------------- CORR ------------------------------- ##
    output$plot.corr <- renderUI({
      plotOutput("corr", width = "100%",
                 height = paste0(input$plotheight, "px"),
                 hover = "corr_plot_click")
    })

    ## Figure for exporting
    output$export.corr <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(corr(plotvalues()$all_vals)))
          return(NULL)
        print(plot_corr(ibraplot = plotvalues()$all_vals,
                        title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       pointsize = input$pointsize,
                       xaxisrange = input$xrange_corr,
                       corrtype = input$corrtype))
        dev.off()
      })

    ## Data for exporting
    output$export.corr.df.rdata <- downloadHandler(
      filename = "corr-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.corr.df.tsv <- downloadHandler(
      filename = "corr-data.tsv",
      content = function(file) {
        tpr_df <- isolate(corr(plotvalues()$all_vals))
        write.table(tpr_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$corr <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_corr")
          shinyBS::createAlert(
            session, anchorId = "corr_message",
            alertId = "message_corr", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(corr(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_corr")
          shinyBS::createAlert(
            session, anchorId = "corr_message",
            alertId = "message_corr",
            content = paste0("Correlation plots can not be displayed (check ",
                             "that cont_truth is not 'none', and that scores ",
                             "are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_corr")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(corr(plotvalues()$all_vals)))
          return(NULL)

        plot_corr(ibraplot = plotvalues()$all_vals, title = plotvalues()$title,
                 stripsize = input$stripsize, titlecol = "white",
                 pointsize = input$pointsize, xaxisrange = input$xrange_corr,
                 corrtype = input$corrtype)
      })
    })

    output$corr_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(corr(plotvalues()$all_vals)))
        return(NULL)
      all_data <- isolate(corr(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$corr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          yvar = "num_method", xvar = toupper(input$corrtype))
      } else {
        res <- nearPoints(all_data, input$corr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, yvar = "num_method",
                          xvar = toupper(input$corrtype))
      }
      fix_res(res, methodcol = "fullmethod", aspcts = toupper(input$corrtype),
              tabtype = "corr")
    })

    ## -------------------------- DEVIATION ---------------------------- ##
    output$axislimitsdeviation <- renderUI({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 ||
          !is_plottable(deviation(plotvalues()$all_vals)))
        return(NULL)
      else {
        if (isTRUE(input$dosquare))
          tmp <- (deviation(plotvalues()$all_vals))$DEVIATION^2
        else
          tmp <- (deviation(plotvalues()$all_vals))$DEVIATION
        mn <- signif(min(tmp[is.finite(tmp)]), 3)
        mx <- signif(max(tmp[is.finite(tmp)]), 3)
        sliderInput(inputId = "xrange_deviation", label = "x-axis limits",
                    min = mn, max = mx, value = c(mn, mx), step = 0.01)
      }
    })

    output$plot.deviation <- renderUI({
      plotOutput("deviation", width = "100%",
                 height = paste0(input$plotheight, "px"),
                 hover = "deviation_plot_click")
    })

    ## Figure for exporting
    output$export.deviation <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(deviation(plotvalues()$all_vals)))
          return(NULL)
        print(plot_deviation(ibraplot = plotvalues()$all_vals,
                             title = plotvalues()$title,
                             stripsize = input$stripsize, titlecol = "white",
                             xaxisrange = input$xrange_deviation,
                             plottype = input$devtype,
                             dojitter = input$dojitter,
                             squaredevs = input$dosquare))
        dev.off()
      })

    ## Data for exporting
    output$export.deviation.df.rdata <- downloadHandler(
      filename = "deviation-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.deviation.df.tsv <- downloadHandler(
      filename = "deviation-data.tsv",
      content = function(file) {
        deviation_df <- isolate(deviation(plotvalues()$all_vals))
        write.table(deviation_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$deviation <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_deviation")
          shinyBS::createAlert(
            session, anchorId = "deviation_message",
            alertId = "message_deviation",
            content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(deviation(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_deviation")
          shinyBS::createAlert(
            session, anchorId = "deviation_message",
            alertId = "message_deviation",
            content = paste0("Deviation plots can not be displayed (check ",
                             "that cont_truth is not 'none', and that scores ",
                             "are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_deviation")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(deviation(plotvalues()$all_vals)))
          return(NULL)
        plot_deviation(ibraplot = plotvalues()$all_vals,
                       title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       xaxisrange = input$xrange_deviation,
                       plottype = input$devtype, dojitter = input$dojitter,
                       squaredevs = input$dosquare)
      })
    })

    output$deviation_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 ||
          !is_plottable(deviation(plotvalues()$all_vals)))
        return(NULL)
      all_data <- isolate(deviation(plotvalues()$all_vals))
      if (isTRUE(input$dosquare))
        all_data$sqDEVIATION <- all_data$DEVIATION^2
      tmp_click <- input$deviation_plot_click
      if (!is.null(tmp_click))
        tmp_click$y <- round(tmp_click$y)
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, tmp_click,
                          threshold = 50, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          yvar = "num_method",
                          xvar = ifelse(isTRUE(input$dosquare), "sqDEVIATION",
                                        "DEVIATION"))
      } else {
        res <- nearPoints(all_data, tmp_click,
                          threshold = 50, maxpoints = 100,
                          addDist = TRUE, yvar = "num_method",
                          xvar = ifelse(isTRUE(input$dosquare), "sqDEVIATION",
                                        "DEVIATION"))
      }
      fix_res(res, methodcol = "fullmethod",
              aspcts = ifelse(isTRUE(input$dosquare), "sqDEVIATION",
                              "DEVIATION"), tabtype = "deviation")
    })

    ## ---------------------------- FPR -------------------------------- ##
    output$plot.fpr <- renderUI({
      plotOutput("fpr", width = "100%", height = paste0(input$plotheight, "px"),
                 hover = "fpr_plot_click")
    })

    ## Figure for exporting
    output$export.fpr <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(fpr(plotvalues()$all_vals)))
          return(NULL)
        print(plot_fpr(ibraplot = plotvalues()$all_vals,
                       title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       pointsize = input$pointsize,
                       xaxisrange = input$xrange_fpr))
        dev.off()
      })

    ## Data for exporting
    output$export.fpr.df.rdata <- downloadHandler(
      filename = "fpr-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.fpr.df.tsv <- downloadHandler(
      filename = "fpr-data.tsv",
      content = function(file) {
        fpr_df <- isolate(fpr(plotvalues()$all_vals))
        write.table(fpr_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$fpr <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_fpr")
          shinyBS::createAlert(
            session, anchorId = "fpr_message",
            alertId = "message_fpr", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(fpr(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fpr")
          shinyBS::createAlert(
            session, anchorId = "fpr_message",
            alertId = "message_fpr",
            content = paste0("FPR can not be displayed (check that ",
                             "binary_truth is not 'none', and that adjusted ",
                             "p-values are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_fpr")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(fpr(plotvalues()$all_vals)))
          return(NULL)
        plot_fpr(ibraplot = plotvalues()$all_vals, title = plotvalues()$title,
                 stripsize = input$stripsize, titlecol = "white",
                 pointsize = input$pointsize, xaxisrange = input$xrange_fpr)
      })
    })

    output$fpr_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(fpr(plotvalues()$all_vals)))
        return(NULL)
      all_data <- isolate(fpr(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$fpr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          yvar = "num_method", xvar = "FPR")
      } else {
        res <- nearPoints(all_data, input$fpr_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, yvar = "num_method", xvar = "FPR")
      }
      fix_res(res, methodcol = "fullmethod", aspcts = c("FPR"),
              tabtype = "large")
    })

    ## ---------------------------- ROC -------------------------------- ##
    output$plot.roc <- renderUI({
      plotOutput("roc", width = "100%", height = paste0(input$plotheight, "px"),
                 hover = "roc_plot_click")
    })

    ## Figure for exporting
    output$export.roc <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(roc(plotvalues()$all_vals)))
          return(NULL)
        print(plot_roc(ibraplot = plotvalues()$all_vals,
                       title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       xaxisrange = input$xrange_roc,
                       yaxisrange = input$yrange_roc))
        dev.off()
      })

    ## Data for exporting
    output$export.roc.df.rdata <- downloadHandler(
      filename = "roc-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.roc.df.tsv <- downloadHandler(
      filename = "roc-data.tsv",
      content = function(file) {
        roc_df <- isolate(roc(plotvalues()$all_vals))
        write.table(roc_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$roc <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_roc")
          shinyBS::createAlert(
            session, anchorId = "roc_message",
            alertId = "message_roc", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(roc(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_roc")
          shinyBS::createAlert(
            session, anchorId = "roc_message",
            alertId = "message_roc",
            content = paste0("ROC curves can not be displayed (check that ",
                             "binary_truth is not 'none')."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_roc")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(roc(plotvalues()$all_vals)))
          return(NULL)
        plot_roc(ibraplot = plotvalues()$all_vals, title = plotvalues()$title,
                 stripsize = input$stripsize, titlecol = "white",
                 xaxisrange = input$xrange_roc, yaxisrange = input$yrange_roc)
      })
    })

    output$roc_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(roc(plotvalues()$all_vals)))
        return(NULL)

      all_data <- isolate(roc(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$roc_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          xvar = "FPR", yvar = "TPR")
      } else {
        res <- nearPoints(all_data, input$roc_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "FPR", yvar = "TPR")
      }
      fix_res(res, methodcol = "fullmethod", aspcts = c("FPR", "TPR"),
              tabtype = "small")
    })

    ## -------------------------- SCATTER ------------------------------ ##
    output$plot.scatter <- renderUI({
      plotOutput("scatter", width = "100%",
                 height = paste0(input$plotheight, "px"),
                 hover = "scatter_plot_click")
    })

    ## Figure for exporting
    output$export.scatter <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(scatter(plotvalues()$all_vals)))
          return(NULL)
        print(plot_scatter(ibraplot = plotvalues()$all_vals,
                           title = plotvalues()$title,
                           stripsize = input$stripsize, titlecol = "white",
                           pointsize = input$pointsize, doflip = input$doflip,
                           dolog = input$dolog))
        dev.off()
      })

    ## Data for exporting
    output$export.scatter.df.rdata <- downloadHandler(
      filename = "scatter-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.scatter.df.tsv <- downloadHandler(
      filename = "scatter-data.tsv",
      content = function(file) {
        scatter_df <- isolate(scatter(plotvalues()$all_vals))
        write.table(scatter_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$scatter <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_scatter")
          shinyBS::createAlert(
            session, anchorId = "scatter_message",
            alertId = "message_scatter", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(scatter(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_scatter")
          shinyBS::createAlert(
            session, anchorId = "scatter_message",
            alertId = "message_scatter",
            content = paste0("Scatter plots can not be displayed (check that ",
                             "cont_truth is not 'none', and that scores are ",
                             "provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_scatter")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(scatter(plotvalues()$all_vals)))
          return(NULL)
        plot_scatter(ibraplot = plotvalues()$all_vals,
                     title = plotvalues()$title,
                     stripsize = input$stripsize, titlecol = "white",
                     pointsize = input$pointsize, doflip = input$doflip,
                     dolog = input$dolog)
      })
    })

    output$scatter_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(scatter(plotvalues()$all_vals)))
        return(NULL)

      all_data <- isolate(scatter(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$scatter_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "OBSERVATION",
                          yvar = "TRUTH")
      } else {
        res <- nearPoints(all_data, input$scatter_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "OBSERVATION", yvar = "TRUTH")
      }
      fix_res(res, methodcol = "fullmethod", aspcts = c("observation", "truth"),
              tabtype = "scatter")
    })

    ## ---------------------------- FPC -------------------------------- ##
    output$plot.fpc <- renderUI({
      plotOutput("fpc", width = "100%", height = paste0(input$plotheight, "px"),
                 hover = "fpc_plot_click")
    })

    ## Figure for exporting
    output$export.fpc <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(fpc(plotvalues()$all_vals)))
          return(NULL)
        print(plot_fpc(ibraplot = plotvalues()$all_vals,
                       title = plotvalues()$title,
                       stripsize = input$stripsize, titlecol = "white",
                       maxnfdc = input$maxnfdc))
        dev.off()
      })

    ## Data for exporting
    output$export.fpc.df.rdata <- downloadHandler(
      filename = "fpc-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$export.fpc.df.tsv <- downloadHandler(
      filename = "fpc-data.tsv",
      content = function(file) {
        fpc_df <- isolate(fpc(plotvalues()$all_vals))
        write.table(fpc_df, file = file, quote = FALSE, row.names = FALSE,
                    col.names = TRUE, sep = "\t")
      })

    output$fpc <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_fpc")
          shinyBS::createAlert(
            session, anchorId = "fpc_message",
            alertId = "message_fpc", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if (!is_plottable(fpc(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fpc")
          shinyBS::createAlert(
            session, anchorId = "fpc_message",
            alertId = "message_fpc",
            content = paste0("False discovery curves can not be displayed ",
                             "(check that binary_truth is not 'none')."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_fpc")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 || !is_plottable(fpc(plotvalues()$all_vals)))
          return(NULL)

        plot_fpc(ibraplot = plotvalues()$all_vals, title = plotvalues()$title,
                 stripsize = input$stripsize, titlecol = "white",
                 maxnfdc = input$maxnfdc)
      })
    })

    output$fpc_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0 || !is_plottable(fpc(plotvalues()$all_vals)))
        return(NULL)
      all_data <- isolate(fpc(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(all_data, input$fpc_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          xvar = "topN", yvar = "FP")
      } else {
        res <- nearPoints(all_data, input$fpc_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "topN", yvar = "FP")
      }
      fix_res(res, methodcol = "fullmethod",
              aspcts = c("Number of detections", "FP"), tabtype = "small")
    })

    ## -------------------------- FDRTPR ------------------------------- ##
    output$plot.fdrtprcurve <- renderUI({
      plotOutput("fdrtprcurve", width = "100%",
                 height = paste0(input$plotheight, "px"),
                 hover = "fdrtprcurve_plot_click")
    })

    ## Figure for exporting
    output$export.fdrtprcurve <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(fdrtprcurve(plotvalues()$all_vals)))
          return(NULL)
        print(plot_fdrtprcurve(ibraplot = plotvalues()$all_vals,
                               title = plotvalues()$title,
                               stripsize = input$stripsize, titlecol = "white",
                               pointsize = input$pointsize,
                               xaxisrange = input$xrange_fdrtpr,
                               yaxisrange = input$yrange_fdrtpr,
                               plottype = input$plottype))
        dev.off()
      })

    ## Data for exporting
    output$export.fdrtpr.df.rdata <- downloadHandler(
      filename = "fdrtpr-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$fdrtpr.df.tsv.button <- renderUI({
      if ("curve" %in% input$plottype)
        downloadButton("export.fdrtpr.df.tsv", label = "Download tsv (curve)")
      else
        downloadButton("export.fdrtpr.df.tsv", label = "Download tsv (points)")
    })

    output$export.fdrtpr.df.tsv <- downloadHandler(
      filename = "fdrtpr-data.tsv",
      content = function(file) {
        fdrtpr_curve_df <- isolate(fdrtprcurve(plotvalues()$all_vals))
        fdrtpr_point_df <- isolate(fdrtpr(plotvalues()$all_vals))
        if ("curve" %in% input$plottype)
          write.table(fdrtpr_curve_df, file = file, quote = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = "\t")
        else if ("points" %in% input$plottype)
          write.table(fdrtpr_point_df, file = file, quote = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = "\t")
      })

    output$fdrtprcurve <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_fdrtpr")
          shinyBS::createAlert(
            session, anchorId = "fdrtpr_message",
            alertId = "message_fdrtpr", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if ("curve" %in% input$plottype &
                   !is_plottable(fdrtprcurve(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fdrtpr")
          shinyBS::createAlert(
            session, anchorId = "fdrtpr_message",
            alertId = "message_fdrtpr",
            content = paste0("FDR/TPR curves can not be displayed (check ",
                             "that binary_truth is not 'none')."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else if ("points" %in% input$plottype &
                   !is_plottable(fdrtpr(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fdrtpr")
          shinyBS::createAlert(
            session, anchorId = "fdrtpr_message",
            alertId = "message_fdrtpr",
            content = paste0("FDR/TPR at specified adjusted p-value ",
                             "thresholds can not be displayed (check that ",
                             "binary_truth is not 'none', and that adjusted ",
                             "p-values are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else if (!any(c("points", "curve") %in% input$plottype)) {
          shinyBS::closeAlert(session, alertId = "message_fdrtpr")
          shinyBS::createAlert(
            session, anchorId = "fdrtpr_message",
            alertId = "message_fdrtpr",
            content = paste0("No plot type selected."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_fdrtpr")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0)
          return(NULL)
        if ("curve" %in% input$plottype &
            !is_plottable(fdrtprcurve(plotvalues()$all_vals)))
          return(NULL)
        if ("points" %in% input$plottype &
            !is_plottable(fdrtpr(plotvalues()$all_vals)))
          return(NULL)
        if (!any(c("curve", "points") %in% input$plottype))
          return(NULL)
        plot_fdrtprcurve(ibraplot = plotvalues()$all_vals,
                         title = plotvalues()$title,
                         stripsize = input$stripsize, titlecol = "white",
                         pointsize = input$pointsize,
                         xaxisrange = input$xrange_fdrtpr,
                         yaxisrange = input$yrange_fdrtpr,
                         plottype = input$plottype)
      })
    })

    output$fdrtprcurve_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0)
        return(NULL)
      if ("curve" %in% input$plottype &
          !is_plottable(fdrtprcurve(plotvalues()$all_vals)))
        return(NULL)
      if ("points" %in% input$plottype &
          !is_plottable(fdrtpr(plotvalues()$all_vals)))
        return(NULL)
      if ("curve" %in% input$plottype)
        tab_data <- isolate(fdrtprcurve(plotvalues()$all_vals))
      else tab_data <- isolate(fdrtpr(plotvalues()$all_vals))
      if ("split" %in% isolate(input$facet_opt)) {
        res <- nearPoints(tab_data, input$fdrtprcurve_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          xvar = "FDR", yvar = "TPR")
      } else {
        res <- nearPoints(tab_data, input$fdrtprcurve_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "FDR", yvar = "TPR")
      }
      fix_res(res, methodcol = "fullmethod", aspcts = c("FDR", "TPR"),
              tabtype = "large")
    })

    ## -------------------------- FDRNBR ------------------------------- ##
    output$plot.fdrnbrcurve <- renderUI({
      plotOutput("fdrnbrcurve", width = "100%",
                 height = paste0(input$plotheight, "px"),
                 hover = "fdrnbrcurve_plot_click")
    })

    ## Figure for exporting
    output$export.fdrnbrcurve <- downloadHandler(
      filename = "shiny-plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = input$plotheight/67)
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0 ||
            !is_plottable(fdrnbrcurve(plotvalues()$all_vals)))
          return(NULL)
        print(plot_fdrnbrcurve(ibraplot = plotvalues()$all_vals,
                               title = plotvalues()$title,
                               stripsize = input$stripsize, titlecol = "white",
                               pointsize = input$pointsize,
                               xaxisrange = input$xrange_fdrnbr,
                               plottype = input$plottype))
        dev.off()
      })

    ## Data for exporting
    output$export.fdrnbr.df.rdata <- downloadHandler(
      filename = "fdrnbr-data.Rdata",
      content = function(file) {
        ibraplot <- isolate(plotvalues()$all_vals)
        save(ibraplot, file = file)
      })

    output$fdrnbr.df.tsv.button <- renderUI({
      if ("curve" %in% input$plottype)
        downloadButton("export.fdrnbr.df.tsv", label = "Download tsv (curve)")
      else
        downloadButton("export.fdrnbr.df.tsv", label = "Download tsv (points)")
    })

    output$export.fdrnbr.df.tsv <- downloadHandler(
      filename = "fdrnbr-data.tsv",
      content = function(file) {
        fdrnbr_curve_df <- isolate(fdrnbrcurve(plotvalues()$all_vals))
        fdrnbr_point_df <- isolate(fdrnbr(plotvalues()$all_vals))
        if ("curve" %in% input$plottype)
          write.table(fdrnbr_curve_df, file = file, quote = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = "\t")
        else if ("points" %in% input$plottype)
          write.table(fdrnbr_point_df, file = file, quote = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = "\t")
      })


    output$fdrnbrcurve <- renderPlot({
      withProgress(message = "Updating plot...", value = 0, {
        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0) {
          shinyBS::closeAlert(session, alertId = "message_fdrnbr")
          shinyBS::createAlert(
            session, anchorId = "fdrnbr_message",
            alertId = "message_fdrnbr", content = paste0("No input provided."),
            style = "danger", append = FALSE, dismiss = FALSE)
        } else if ("curve" %in% input$plottype &
                   !is_plottable(fdrnbrcurve(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fdrnbr")
          shinyBS::createAlert(
            session, anchorId = "fdrnbr_message",
            alertId = "message_fdrnbr",
            content = paste0("FDR/NBR curves can not be displayed (check ",
                             "that binary_truth is not 'none')."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else if ("points" %in% input$plottype &
                   !is_plottable(fdrnbr(plotvalues()$all_vals))) {
          shinyBS::closeAlert(session, alertId = "message_fdrnbr")
          shinyBS::createAlert(
            session, anchorId = "fdrnbr_message",
            alertId = "message_fdrnbr",
            content = paste0("FDR/NBR at specified adjusted p-value ",
                             "thresholds can not be displayed (check that ",
                             "binary_truth is not 'none', and that adjusted ",
                             "p-values are provided)."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else if (!any(c("points", "curve") %in% input$plottype)) {
          shinyBS::closeAlert(session, alertId = "message_fdrnbr")
          shinyBS::createAlert(
            session, anchorId = "fdrnbr_message",
            alertId = "message_fdrnbr",
            content = paste0("No plot type selected."),
            style = "info", append = FALSE, dismiss = FALSE)
        } else {
          shinyBS::closeAlert(session, alertId = "message_fdrnbr")
        }

        if (length(values$all_methods) == 0 | length(input$cols) == 0 |
            input$goButton == 0)
          return(NULL)
        if ("curve" %in% input$plottype &
            !is_plottable(fdrnbrcurve(plotvalues()$all_vals)))
          return(NULL)
        if ("points" %in% input$plottype &
            !is_plottable(fdrnbr(plotvalues()$all_vals)))
          return(NULL)
        if (!any(c("curve", "points") %in% input$plottype))
          return(NULL)
        plot_fdrnbrcurve(ibraplot = plotvalues()$all_vals,
                         title = plotvalues()$title,
                         stripsize = input$stripsize, titlecol = "white",
                         pointsize = input$pointsize,
                         xaxisrange = input$xrange_fdrnbr,
                         plottype = input$plottype)
      })
    })

    output$fdrnbrcurve_click_info <- DT::renderDataTable({
      if (length(values$all_methods) == 0 | length(input$cols) == 0 |
          input$goButton == 0)
        return(NULL)
      if ("curve" %in% input$plottype &
          !is_plottable(fdrnbrcurve(plotvalues()$all_vals)))
        return(NULL)
      if ("points" %in% input$plottype &
          !is_plottable(fdrnbr(plotvalues()$all_vals)))
        return(NULL)
      if ("curve" %in% input$plottype)
        tab_data <- isolate(fdrnbrcurve(plotvalues()$all_vals))
      else tab_data <- isolate(fdrnbr(plotvalues()$all_vals))
      if ("split" %in% input$facet_opt) {
        res <- nearPoints(tab_data, input$fdrnbrcurve_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, panelvar1 = "splitval",
                          xvar = "FDR", yvar = "NBR")
      } else {
        res <- nearPoints(tab_data, input$fdrnbrcurve_plot_click,
                          threshold = 5, maxpoints = 100,
                          addDist = TRUE, xvar = "FDR", yvar = "NBR")
      }
      methodcol <- "fullmethod"
      fix_res(res, methodcol = methodcol,
              aspcts = c("FDR", "Number of detections"), tabtype = "large")
    })
  }
  shinyApp(ui = p_layout, server = server_function)
}
