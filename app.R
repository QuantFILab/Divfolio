require(remotes)
remotes::install_github("dreamRs/shinypop")
require(shinypop)
require(httr)
require(tidyverse)
require(rvest)
require(jsonlite)
require(plotly)
require(BatchGetSymbols)
require(shinydashboard)
require(shiny)
require(shinycustomloader)
require(shinydashboardPlus)
require(waiter)
require(shinyWidgets)
require(dashboardthemes)
require(quantmod)
require(PerformanceAnalytics)
require(reshape2)
require(viridis)
require(shinyjqui)
require(DT)
require(BatchGetSymbols)
require(lubridate)
require(robustbase)
require(shinyBS)
require(cluster)
require(huge)
require(qgraph)
require(magic)
require(shinyFeedback)
require(tmap)
source('FunctionsR')


options(shiny.sanitize.errors = FALSE)

ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
  dashboardHeader(title = shinyDashboardLogo(
                    theme = "blue_gradient",
                    boldText = "DivFolio",
                    mainText = "App",
                    badgeText = "v. Beta"
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("User Guide", tabName = "userguide", icon = icon("folder-plus")),
      menuItem("Divestment Plan", tabName = "div", icon = icon("tree")),
      menuItem("Our Team", tabName = "about_us", icon = icon("user-plus"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
    useShinyFeedback(),
    use_noty(),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden;}",
               ".shiny-output-error:after {visibility: hidden;}"
    ),

    tabItems(
      tabItem(tabName = "welcome",
              h2('Divfolio: Green Finance Wealth Management Software'),
                  tags$li(HTML('Sustainablising portfolio by divestment')),
                  tags$li(HTML('Comparing performance of portfolios before and after divestment')),
                  tags$li(HTML('Comparing performance of portfolios in general purpose')),
              br(),
              HTML("Changes in the contemporary climate include both global warming and its effects on the planet's weather patterns. Climate change has occurred in the past, but the current rate of change is significantly more fast and is not attributable to natural causes. It is instead caused by the release of greenhouse gases, primarily carbon dioxide (CO2) and methane. The majority of these emissions are caused by the burning of fossil fuels for energy production. At the current warming level of 1.2 C (2.2 F), several of these effects can already be noticed. Additional warming will amplify these effects and may set off tipping points, such as the Greenland ice sheet melting. Under the Paris Agreement of 2015, governments pledged to limit global warming 'well below 2 C'. Despite the commitments made under the Agreement, global warming would still reach roughly 2.7 degrees Celsius (4.9 degrees Fahrenheit) by the end of the century. To limit warming to 1.5 C, emissions must be cut in half by 2030 and reach net-zero levels by 2050. <br><br>
                   Divestment and exclusion are approaches that limit investment in carbon-intensive assets or industries, based on an organization or its stakeholders guiding principles. Thus, individuals who seek to avoid carbon-intensive assets frequently favor this policy. These stakeholders say that divestment and exclusion increase the cost of capital for carbon-intensive operations, provide a market signal to enterprises engaged in these activities to encourage a more rapid transition, and ensure that investors are comfortable with the sources of their returns. <br><br>
                   Divfolio offers a comparison of the risk profiles, ESG scores, and customized attributes of portfolios, such as carbon intensity, before and after divestment based on the simulation using historical data as well as advanced options such as assessing stability of portfolio via clustering and correlation structure. The tool is useful for investigating the impact of divestment on portfolio performance in multidimensional views.<br><br>"),
              HTML('<script src="https://climateclock.world/widget-v2.js" async></script><climate-clock />'),
              
              ),

      tabItem(tabName = "userguide",
              tabBox(width = 12,
                     tabPanel(title = "Introduction",
                              tags$div(
                                tags$ul(
                                  HTML("As awareness of the global warming problem, many countries, institutions, and agents are willing to move toward a more sustainable economy. Similarly to the financial and investment sector, sustainability trends are becoming more necessary. In the asset management industry, a debate has arisen over how to most effectively incentivize corporations and encourage private sector initiatives that will actively induce decision making to reduce environmental impact and drive change in positive mitigation strategies for reducing carbon emissions, the primary cause of climate change. These movements and activities are sometimes referred to as divestment strategies from fossil fuels. Investors and asset managers are urged to liquidate their holdings in companies classified as non-compliant or with poor performance on the environmental component of environmental social governance or ESG ratings, which reflect compliance with current best practices addressing carbon emissions. Not only does divestment deprive fossil fuel businesses of funding to promote change, but it is also a prudent business move for ESG reporting with long-term viability in the spotlight. If the cost of capital increases or refinancing becomes prohibitively expensive or simply unavailable, the valuations of carbon-intensive enterprises will be impacted, and they may become unviable. <br><br> 
                                       <b>Divfolio</b> is a software developed in R shiny environment that offers portfolio analytic related to divestment. The tool offers a comparison of the risk profiles, ESG scores, and customized attributes of portfolios before and after divestment based on the simulation using historical data as well as advanced options such as assessing stability of portfolio via clustering and correlation structure. The tool is useful for investigating the impact of divestment on portfolio performance in multidimensional views. The idea of the divestment methodology is given in our paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b>,  available on SSRN. The tool also benefits the general purpose of portfolio performance comparison, especially in ESG investing and sustainable investing. Users can customize the attributes of comparable portfolios by uploading prepared CSV files instead of generating portfolios on the tool."),
                                  a("See our paper here", href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449", target="_blank"),
                                  img(src = 'paperssrn.png',height="70%", width="70%")
                                )
                              )
                              ),
                     tabPanel(title = "Workflow",
                              box(
                                title = h4("Workflow"),   
                                    HTML("DivFolio App tool comprises two core components as will be detailed in the panels below. The first component involves a set of sequential STEPs that users undertake consisting of STEP 1 through to STEP 5, which must be performed sequentially. The second component involves a collection of three independent optional STEPs that can be performed individually. For the optional steps, so long as the user has the relevant data prepared in CSV format already, for instance as output from STEPS 1 to 5 in Component 1, then they can upload this directly into any of the STEPS in Component 2 instead of generating them again from the tool. The uploaded files are required to be in a specific format, described in the section <b>Types of Uploading Files, at the end of this page</b>. Uploading the files is recommended as it will speed up the process significantly and avoid blocking due to too frequent calling data via API.<br>"),
                                    img(src = 'daig.jpg',height="75%", width="75%", align = 'center'),
                                    tags$br(""),
                                    tags$li(HTML("<b>Step 1: Asset Performance Investigation</b> <br>
                                                This STEP is used for developing a list of potential portfolio assets. The user can view ESG data, accounting ratios, candlestick charts, and a summary of asset risk profiles in multiple periods. The tool allows users to investigate the performance of assets and add them to portfolio in STEP 2 that will display them. If a user already possesses a list of possible assets, Users can skip this step and go to STEP 2.")),
                                    tags$li(HTML("<b>Step 2: Selecting Divestable and Investable Assets</b> <br>
                                                This STEP is used for assigning the investment status to the selected assets, either 'invest' or 'divest'. Users can download the companies and ESG information from the tickers in STEP 1. The tool allows two modes of divestment asset selection: manually where the users selects assets to divest based on their expert opinion or automatically where a quantile ranking based method is used to select divestment assets by the quantile of their current ESG score. The comparison of the percentage of change in ESG scores or/and other attributes is given.")),
                                    tags$li(HTML("<b>Step 3: Batching Historical Data</b> <br>
                                                 This step is used for generating the time series of the daily price of assets. The tool will import the tickers in STEP 2 to receive the daily price data via API to calculate returns. The table of the return of each asset is given in this part as well as the distribution of the risk profiles in the comparable boxplots.")),
                                    tags$li(HTML("<b> Step 4: Constructing Investment Portfolio</b> <br>
                                                This step is used for generating the portfolio weights from the returns obtained in STEP 3 as well as to illustrate the portfolio performance. There are five options of portfolio weight available. The tool will calculate the weighted ESG scores or attributes and risk profiles of the portfolio over time.  Those results are displayed by the tables, the plot of time series, and distribution boxplots.")),
                                    tags$li(HTML("<b> Step 5: Divestment</b> <br>
                                                This STEP is used for generating the divestment schedule - sequence of limit of the investment weights for divestable assets at a given time. The tool will import the return and the portfolio weight  from STEP 3 and STEP 4 to generate investment weight of divested portfolio that the assets in divesting list from the portfolio according to the divestment schedule. The divestment strategy is to allocate capital from the divestable assets to the investable assets proportionally to their original weights. The full details of the divestment strategy can be found in the paper; :
Marupanthorn, Pasin and Sklibosios Nikitopoulos, Christina and Ofosu-Hene, Eric and Peters, Gareth and Richards, Kylie-Anne and Richards, Kylie-Anne, Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns (June 8, 2022). Available at SSRN: https://ssrn.com/abstract=4131449 or http://dx.doi.org/10.2139/ssrn.4131449. The comparison of performances to the non-divest portfolio will be given in the STEP 5.")),
                                    tags$li(HTML("<b> Option I: Multiple Portfolios Comparison </b> <br>
                                                 This optional STEP is used for analysis and comparing the performance of many portfolios. The results in this STEP are similar to STEP 5 but in comparison to more than two portfolios. Generally, it can be used for comparing any portfolio, not limited to divestment.")),
                                    tags$li(HTML("<b> Option II: Stability Analysis via Clustering </b> <br>
                                                 This optional STEP is used for investigating the stability of portfolio performance. We employ the clustering algorithm, namely Clustering Large Applications or CLARA, an approximate extension of k-medoids methods to achieve stability investigation. Portfolios that have similar relative behavior will belong to the same cluster over time. The features used in clustering were: return, cumulative return, volatility, sharpe ratio, value at risk, max drawdown, and sortino.")),
                                    tags$li(HTML("<b> Option III: Graph Structure Correlation Analysis </b> <br>
                                                 This optional STEP is used for investigating the correlation structure of the assets in portfolios using graph LASSO. This function is suitable for studying the dynamic correlation structure of portfolios and comparison of their diversification as portfolio dynamically evolves, for instance as divestment is progressively performed on different sets of assets.")
                                ),
                              id = "mybox",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              closable = TRUE,
                              width = 13
                              ),
                              
                              box(
                                title = h4("Types of Uploading Files"),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F0</b>; List of tickers and, optional, their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) an optional column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                  img(src = 'file0.png'),
                                  #tags$br(""),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F1</b>; List of tickers, their investment statuses, and some numerical attributes. The file requires; <br>
                                              i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                              ii) a compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest', and <br>
                                              iii) at least one columns of any name that contain numerical attributes of the asset.")),
                                  img(src = 'file1.png'),
                                  #tags$br(""),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F2</b>; The table of assets return. The file requires; <br>
                                              i) a compulsory column of the name 'Date', consisting of dates in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the return of each asset over time.")), 
                                  img(src = 'file2.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F3</b>; The table of portfolio weight. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                  img(src = 'file3.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F4</b>; The table of divestment schedule. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'Bound', containing the sequence of limit of the investment weight of the divestable assets.")),
                                  img(src = 'file4.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F5</b>; The table of portfolio weight with multiple portfolios. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'PORTNAME', containing any name of portfolio as the label for separating data by portfolio type, and
                                              iii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                  img(src = 'file5.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F6</b>; List of tickers and their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                  img(src = 'file6.png'),
                                  hr(),
                              id = "mybox",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              closable = TRUE,
                              width = 13
                              )
                             ),
                     
                     tabPanel(title = "Packages and Dependency",
                              img(src = 'dependency.png',align = 'center'),
                              "The application was built under R version 4.1.2",
                              includeHTML('example.html')
                              )
              )
              ),
      
      tabItem(tabName = "div",
        tabBox(width = 12,
            tabPanel(title = "Step 1",
               fixedRow(
                 column(1,
                        dropdown(textInput("ticker", tags$span("Company's Ticker", bsButton("comtick", label = "", icon = icon("info"), style = "info", size = "extra-small")), "AAP"),
                                 bsPopover(
                                   id = "comtick",
                                   title = "More information",
                                   content = paste0(
                                     "The ticker is required in ",
                                     a("Yahoo Finance format", href = "https://finance.yahoo.com/", target="_blank")
                                   ),
                                   placement = "right",
                                   trigger = "focus",
                                   options = list(container = "body")),
                                 
                                 style = "unite", icon = icon("gear"),
                                 status = "primary", width = "300px",
                                 animate = animateOptions(
                                   enter = animations$fading_entrances$fadeInLeftBig,
                                   exit = animations$fading_exits$fadeOutRightBig
                                 ),
                                 actionBttn(
                                   inputId = "submit",
                                   label = "submit",
                                   style = "pill", 
                                   color = "success",
                                   size = "sm"
                                 )),
                        dropdown(textInput("ticker_sel", tags$span("Add assets to portfolio", bsButton("s1", label = "", icon = icon("info"), style = "info", size = "extra-small")), ticker_rec),
                                 bsPopover(
                                   id = "s1",
                                   title = "More information",
                                   content = "Please add one asset per time. Assets will be added to portfolio in Step 2",
                                   placement = "right",
                                   trigger = "focus",
                                   options = list(container = "body")),
                                 style = "unite", icon = icon("cart-plus"),
                                 status = "primary", width = "300px",
                                 animate = animateOptions(
                                   enter = animations$fading_entrances$fadeInLeftBig,
                                   exit = animations$fading_exits$fadeOutRightBig
                                 ),
                                 HTML("<b><h5>Assets in Portfolio</h5></b>"),
                                 verbatimTextOutput("verb"),
                                 actionBttn(
                                   inputId = "add",
                                   label = "add",
                                   style = "pill", 
                                   color = "success",
                                   size ="sm"
                                 ))
                 ),
                 column(11,
                        hr(style = "border-top: 6px solid #0784BF;"),
                        h3("Step 1: Asset Performance Investigation"),
                        hr(style = "border-top: 6px solid #0784BF;"),
                        box(
                          title = h4("User Guide"),
                              "This phase is optional for users who wish to investigate performance of assets and add them to portfolio. 
                          The portfolio will be shown in STEP 2. If you already have list of potential assets, please go to STEP 2.",
                              tags$br(""),
                              "How to use this page?",
                              tags$br(""),
                              tags$li(HTML("Click <b>Gear icon</b> <i class='fa-solid fa-gear' style = 'color:#0072B2;'></i>, input a ticker, and click submit to get company's data")),
                              tags$li(HTML("Click <b>Cart icon</b> <i class='fa-solid fa-cart-plus' style = 'color:#0072B2;'></i>, input a ticker, and click add to add asset to portfolio")),
                              hr(),
                              "The remarks for this STEP is listed below",
                              tags$br(""),
                              tags$li("The ticker is required in Yahoo Finance format. For example, the ticker of companies in the LSE does have .L after, in Tokyo have .T after, and in NYSE does not have the abbreviation after."),
                              tags$li("The sustainability score ratings measure unmanaged risk on a 0-100 scale, with lower scores indicating less unmanaged ESG Risk."),
                              tags$li("All charts are interactive, gernerated by 'plotly' package. Users can zoom, export and hide some plots."),
                              tags$li("The table at the bottom of the page displays the average performances over the past week, month, three months, year, and entire time period."),
                              tags$br(""),
                              "* User Guide box is collapsible",
                          id = "mybox",
                          collapsible = TRUE,
                          collapsed = TRUE,
                          closable = TRUE,
                          width = 13
                        ),
                        h3(textOutput('comp_name')),
                        h6(textOutput('comp_market')),
                        h6(textOutput('comp_sec')),
                        h6(textOutput('comp_sum')),
                        hr(),
                        h5("Sustainability Score"),
                        hr())),
               
               fixedRow(  
                 column(1),
                 column(11,
                        fluidRow(
                          withLoader(valueBoxOutput("lev_esg", width = 3), loader = 'loader4'),
                          withLoader(valueBoxOutput("total_esg", width = 3), loader = 'loader4'),
                          withLoader(valueBoxOutput("e_score", width = 2), loader = 'loader4'),
                          withLoader(valueBoxOutput("s_score", width = 2), loader = 'loader4'),
                          withLoader(valueBoxOutput("g_score", width = 2), loader = 'loader4'))
                 ),
                 column(12,   
                        fixedRow(  
                          column(1),
                          column(11,
                                 hr(),
                                 h5("Historical Data"),
                                 hr(),
                                 plotlyOutput("candel")
                          ))),
                 column(12,   
                        fixedRow(  
                          column(1),
                          column(11,
                                 plotlyOutput("perf_fig")
                          ))),
                 column(12,
                        fixedRow( 
                          column(1),
                          column(11,
                                 #div(dataTableOutput("perf_table"), style = "font-size:80%")
                                 dataTableOutput("perf_table")
                          )))
               )),
               
          tabPanel(title = "Step 2",
              fixedRow(
            column(1,
                   dropdown(fileInput("fileticker", "F0: CSV File of Ticker", 
                                        accept = c(".csv")),
                              style = "unite", icon = icon("gear"),
                              status = "primary", width = "300px",
                              animate = animateOptions(
                                enter = animations$fading_entrances$fadeInLeftBig,
                                exit = animations$fading_exits$fadeOutRightBig
                              )),
                   dropdown(fileInput("file1", "F1: CSV File of Investment Status", 
                                      accept = c(".csv")),
                            style = "unite", icon = icon("check"),
                            status = "primary", width = "300px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ))),
              column(11,
                       hr(style = "border-top: 6px solid #0784BF;"),
                       h3("Step 2: Selecting Divestable and Investable Assets"),
                       hr(style = "border-top: 6px solid #0784BF;"),
                     box(
                       title = h4("User Guide"),
                           "This STEP is to determine which assets should be divested or invested. 
                           Users can also download the ESG data of assets in portfolio.",
                           tags$br(""),
                           "How to use this page?",
                           tags$br(""),
                           HTML("<b> 1. Already have data in CSV format </b>"),
                           tags$li(HTML("Click <b>Gear icon</b> <i class='fa-solid fa-gear' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Ticker</b> with one column of the name 'Ticker', containing company's tickers. Another column of the name 'Status' with either 'Invest' or 'Divest' is an optional. This option helps users not to add one by one ticker in STEP 1. If you have <b>CSV File of Investment Status</b>, you do not need to upload this file.")),
                           img(src = 'file0.png'),
                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, upload <b>CSV File of Investment Status</b> with <br> i) a column of the name 'Ticker', containing company's tickers, and
                                        <br> ii) a column of the name 'Status', containing statuses; 'Divest' or 'Invest', and <br>
                                        iii) at least one numeric columns of any name <br>")),
                           img(src = 'file1.png'),
                           
                           tags$br(""),
                           HTML("<b>or 2. Download ESG data from a network </b>"),
                           tags$li(HTML("Add potential assets in STEP 1 if you don,t upload <b>CSV File of Ticker</b>. The selected assets will appear in the part <b>Potential Assets</b>.")),
                           tags$li(HTML("Drag and drop assets from the part <b>Potential Assets</b> in the part <b>Investable Assets</b> or <b>Divestable Assets</b>. The assets in the part <b>Potential Assets</b> will not be included in the portfolio.")),
                           tags$li(HTML("Click <b>summary</b> button to get the data")),
                           tags$br(""),
                           hr(),
                           "The remarks for this STEP is listed below",
                           tags$br(""),
                           tags$li(HTML("Assets in the part <b>Potential Assets</b> will not be included in the portfolio.")),
                           tags$li(HTML("Assets in parts <b>Potential Assets</b>, <b>Investable Assets</b>, and <b>Divestable Assets</b> are exchangeable by draging and dropping.")),
                           tags$li(HTML("Assign divestment status to asset by dropping in either <b>Investable Assets</b> or <b>Divestable Assets</b>.")),
                           tags$li(HTML("Click <b>update summary</b> botton when you finish updating divestment status of the assets.")),
                           tags$li(HTML("Select the option <b>Manage Divestment</b> if you want to assign divestment status by ranking the assets according to selected factor. The criteria is upper or lower quantile.")),
                           tags$li(HTML("<b>Table of Attributes</b> shows the uploaded <b>CSV File of Investment Status</b> or the table of ESG scores where the data is downloaded from the network.")),
                           tags$li(HTML("<b>Comparison of Attributes Before-After Divestment in Percentage</b> displays the score of attributes before and after divestment as a percentage of change under the assumption that all assets are equally valuable. The formula is given by <br> 
                                   $$R = \\frac{A_{after} - A_{before}}{A_{before}} \\times 100\\%,$$ <br> 
                                        where $A_{before}$ and $A_{after}$ is the attribute of the asset before and after divestment, respectively.")),
                           tags$br(""),
                           "* User Guide box is collapsible",
                       id = "mybox",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       closable = TRUE,
                       width = 13
                     ),
                     box(
                       title = h4("Seleacting Assets from World Major Indices"),
                       pickerInput(
                         inputId = "selindex",
                         label = "Select Index", 
                         choices = index_choice
                       ),
                       plotOutput("map"),
                    actionBttn(
                      inputId = "subindex",
                      label = "submit index",
                      style = "pill", 
                      color = "success",
                      size = "xs"
                    ),
                       id = "mybox2",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       closable = TRUE,
                       width = 13
                     ),

                       orderInput('inter', 'Potential Assets', items = NULL, item_class = 'info',placeholder = 'Add Ticker from Assets Section Panel', connect = c('source','divt'), width = "1000px"),
                       hr(),
                       orderInput('source', 'Investable Assets', items = NULL, item_class = 'info',placeholder = 'Drag Ticker here', connect = c('divt','inter'), width = "1000px"),
                       hr(),
                       orderInput('divt', 'Divestable Assets', items = NULL, item_class = 'danger',placeholder = 'Drag Ticker here', connect = c('source','inter'), width = "1000px"),
                       hr(),
                       actionBttn(
                         inputId = "subesg",
                         label = "summary",
                         style = "pill", 
                         color = "success",
                         size = "xs"
                       ),
                       actionBttn(
                         inputId = "subup",
                         label = "update summary",
                         style = "pill", 
                         color = "success",
                         size = "xs"
                       ),                       
                       hr(),
                       awesomeCheckbox(
                         inputId = "autosel",
                         label = "Manage Divestment", 
                         value = FALSE,
                         status = "info"
                       ),
                       conditionalPanel(condition ="input.autosel == 1",
                                        column(4,
                                          pickerInput(
                                          inputId = "selq",
                                          label = "Select Investment Criteria", 
                                          choices = NULL
                                        )),
                                        column(4,
                                               textInput("perc", "Percentile (0 - 1)", 0.5)),
                                        column(4,
                                               pickerInput(
                                                 inputId = "seldec",
                                                 label = "Top or Bottum", 
                                                 choices = list('More than' = 1, 'Less than' = 0)
                                               )),
                                        actionBttn(
                                          inputId = "subman",
                                          label = "update summary by quantile",
                                          style = "pill", 
                                          color = "success",
                                          size = "xs"
                                        )
                                        ),
                       h4("Table of Attributes"),
                       hr(),
                       withLoader(DT::dataTableOutput("ui_esg_table", width = "auto", height = "auto")),
                       h4("Comparison of Attributes Before-After Divestment in Percentage"),
                       hr(),
                       withLoader(DT::dataTableOutput("ui_esg_table_com", width = "auto", height = "auto"))
                     ))
              
),

tabPanel(title = "Step 3",
        fixedRow(
          column(1,
                 dropdown(dateInput("stdate", "From:", value = ymd(as.Date(Sys.time())) %m-% months(1)),
                          dateInput("enddate", "To:", value = as.Date(Sys.time())),
                          style = "unite", icon = icon("calendar"),
                          status = "primary", width = "300px",
                          animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                          ),
                          actionBttn(
                            inputId = "subtime",
                            label = "get historical data",
                            style = "pill", 
                            color = "success",
                            size ="sm"
                          )),
                 dropdown(fileInput("file2", "F2: CSV File of Historical Data", 
                                    accept = c(".csv")),
                          style = "unite", icon = icon("check"),
                          status = "primary", width = "300px",
                          animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                          ))),
          column(11,
                   hr(style = "border-top: 6px solid #0784BF;"),
                   h3("Step 3: Batching Historical Data"),
                   hr(style = "border-top: 6px solid #0784BF;"),
                 
                 box(
                   title = h4("User Guide"),
                       "This STEP is to download the return of assets in a portfolio. Note that the process in STEP 2 needs to be completed before running this STEP.",
                       tags$br(""),
                       "How to use this page?",
                       tags$br(""),
                       HTML("<b> 1. Already have data in CSV format </b>"),
                       tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Historical</b> Data with one column of the name 'Date', containing date in form of 'year-month-day'. Other columns are named by the tickers and the values in cells are the asset returns.")),
                       img(src = 'file2.png'),
                       tags$br(""),
                       HTML("<b>or 2. Download return data from a network </b>"),
                       tags$li(HTML("Click <b>Calendar icon</b> <i class='fa-solid fa-calendar-days' style = 'color:#0072B2;'></i>, set time period to obtain the returns, and click <b>get historical data</b> button.")),
                       hr(),
                       "The remarks for this STEP is listed below",
                       tags$br(""),
                       tags$li(HTML("<b>Table of Historical Relative Return</b> shows the return uploaded from <b>CSV File of Historical Data</b> or the relative returns where the data is downloaded from the network. the ralative return is calculated by the adjust closing price by
                                    $$Re = \\frac{P_{t+1} - P_{t}}{P_{t}}$$ where $P_{t+1}$ and $P_{t}$ are the adjusted closing prices at time $t+1$ and $t$, respectively.")),
                       tags$li(HTML("<b>Table of Assets Performance</b> displays the average of asset performances over the selected period.")),
                       tags$li(HTML("<b>Distribution of Asset Returns</b> displays the distribution of the returns of the assets over the selected period. Users can select the picker option 
                                    <b>Arranging Plot by</b> to sort the assets according to the interesting performance.")),
                       tags$br(""),
                       "* User Guide box is collapsible",
                   id = "mybox",
                   collapsible = TRUE,
                   collapsed = TRUE,
                   closable = TRUE,
                   width = 13
                 ),
                 
                   h4("Table of Historical Relative Return"),
                   hr(),
                   withLoader(DT::dataTableOutput("tab_return", width = "auto", height = "auto")),
                   h4("Table of Assets Performance"),
                   hr(),
                   withLoader(DT::dataTableOutput("reportreturn", width = "auto", height = "auto")),
                   h4("Distribution of Asset Returns"),
                   hr(),
                 pickerInput(
                   inputId = "rankby",
                   label = "Arranging Plot by", 
                   choices = NULL
                 ),
                   withLoader(plotlyOutput("boxasset"))
                 )
                 )
                   ),
tabPanel(title = "Step 4",
      fixedRow(
            column(1,
                   dropdown(pickerInput(
                                  inputId = "type",
                                  label = "Select Portfolio Type", 
                                  choices = list("Passive Equal Weight" = "0", "Active Equal Weight" = "1",
                                                 "Global Minimum Variance" = "2", "Maximum Sharpe Ration" = "3",
                                                 "Principal Portfolio" = "4")),
                            HTML('<h5><b>Limit Short-Selling Weight</b></h5>'),                            
                            switchInput(
                              inputId = "limselect"
                            ),
                            conditionalPanel(
                              condition = "(input.limselect == 1)&(input.type == 2||input.type == 3||input.type == 4)",
                              textInput("shortlim", "Short-Selling Limit", 0.3)
                            ),
                              textInput("reb",  tags$span("Rebalancing Frequency [in day]", bsButton("iter", label = "", icon = icon("info"), style = "info", size = "extra-small")), 20),
                            br(),
                              textOutput(outputId = "textnum"),
                            bsPopover(
                              id = "iter",
                              title = "Suggestion",
                              content = "Number of rebalancing iterations = (Number of Days in Selected Period)/(Rebalancing Frequency). More number of iterations cost more computation time, espacially for a large portfolio. We suggest trying small number as no more than 10 iterations before running more number of iterations."
                              ),
                            br(),
                              textInput("tau", "Number of Days for Calculating Weights", 20),
                            actionBttn(
                              inputId = "subweight",
                              label = "submit",
                              style = "pill", 
                              color = "success",
                              size = "sm"
                            ),

                              style = "unite", icon = icon("chart-bar"),
                              status = "primary", width = "300px",
                              animate = animateOptions(
                                enter = animations$fading_entrances$fadeInLeftBig,
                                exit = animations$fading_exits$fadeOutRightBig
                              )),
                   dropdown(fileInput("file3", "F3: CSV File of Portfolio's Weight", 
                                      accept = c(".csv")),
                            style = "unite", icon = icon("check"),
                            status = "primary", width = "300px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ))
                   ),
            column(11,
                     hr(style = "border-top: 6px solid #0784BF;"),
                     h3("Step 4: Constructing Investment Portfolio"),
                     hr(style = "border-top: 6px solid #0784BF;"),
                   
                   box(
                     title = h4("User Guide"),
                         "This STEP is to construct an investment portfolio from the selected assets. The analysis of the performances of the portfolio will be given in the STEP. Note that the process in STEP 2 and STEP 3 need to be completed before running this STEP.",
                         tags$br(""),
                         "How to use this page?",
                         tags$br(""),
                         HTML("<b> 1. Already have data in CSV format </b>"),
                         tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Portfolio's Weight</b> with one column of the name 'Date', containing rebalancing dates in form of 'year-month-day'. Other columns are named by the tickers and the values in cells are the asset weights which sum to one over row.")),
                         img(src = 'file3.png'),
                         tags$br(""),
                         HTML("<b>or 2. Calculate portfolio weight using the tool</b>"),
                         tags$li(HTML("Click <b>Chart icon</b> <i class='fa-solid fa-chart-bar' style = 'color:#0072B2;'></i>, select type of portfolio in <b>Select Portfolio Type</b>. There are five options for users; <br> 
                                      <p style='color:blue'>1. Passive Equal Weight</p> means giving each asset the same weight by $1/N$, where $N$ is the number of assets. This portfolio is long-only and the portfolio weight will not change over time. <br> 
                                      <p style='color:blue'>2. Active Equal Weight</p> means giving each asset proportional to its return. High-return assets gain more investment weight. This portfolio is long-only. <br> 
                                      <p style='color:blue'>3. Global Minimum Variance</p> means assigning investment weights to achieve the most stable return by the minimising variance of the portfolio's returns. This portfolio allows both long and short positions. <br> 
                                      <p style='color:blue'>4. Maximum Sharpe Ratio</p> means assigning investment weights to achieve the highest return per risk or Sharpe ratio. This portfolio allows both long and short positions. <br> 
                                      <p style='color:blue'>5. Principle Portfolio</p> means assigning investment weights based on the principal components analysis. This portfolio is risk-seeking as the investment weight will be gained when the asset is more risky. It allows both long and short positions.<br>
                                      Note here that the calculating process may be expensive for a large portfolio for the last three portfolios <br> <br>
                                      The details in deep of all portfolios can be found in our paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b> available on SSRN. <br> <br>")),
                         tags$li(HTML("<b>Limit Short-Selling Weight</b> is the option to control leverage risk by setting the upper bound of the short-selling ratio. This option is available for portfolios that allow short selling. Switch on and set the limit to using this option.")),
                         tags$li(HTML("<b>Rebalancing Frequency [in a day]</b> is the number of days before rebalancing portfolio. The number is no more than a number of days in the studying period.")),
                         tags$li(HTML("<b>Number of Days for Calculating Weights</b> is days in the past used for calculating a covariance matrix. It is required for the last three portfolios as they are covariance-based. It is usually set to be equal to the rebalancing frequency.")),
                         tags$li(HTML("Click <b>submit</b> button after filling all parameters to generate portofolio.")),
                         hr(),
                         "The remarks for this STEP is listed below",
                         tags$br(""),
                         tags$li(HTML("<b>Table of Rebalancing Portfolio Weight</b> shows the portfolio weights uploaded from <b>CSV File of Portfolio's Weight</b> or portfolio weights each rebalancing where the portfolio weights are generated from the tool.")),
                         tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over time. Users can observe the dynamic behavior of each asset across time.")),
                         tags$li(HTML("<b>Assets Allocation</b> displays the charts of portfolio weights at given time, <b>Select Date</b>. Users can observe the attribute of each asset by using the picker <b>Select Factor</b>.")),
                         tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                         tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                         tags$li(HTML("<b>Table of Time Series of Attributes</b> displays the expected attribute of the portfolio over time separated by holding positions")),
                         tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution")),
                         tags$li(HTML("<b>Efficiency Frontier</b> displays the plot of portfolio standard deviation versus return. The picker has multiple options that allow user to observe many curves in a plot.")),
                         tags$li(HTML("<b>Table of Risk Profiles</b> displays the table of risk profiles calculated backward before each rebalancing versus time.")),
                         tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                         tags$br(""),
                         "* User Guide box is collapsible",
                     id = "mybox",
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = TRUE,
                     width = 13
                   ),
                   progressBar(
                     id = "pb",
                     value = 0,
                     total = 100,
                     title = "Process does not start yet",
                     display_pct = TRUE,
                     status = "custom"
                   ),
                   tags$style(".progress-bar-custom {background-color: #25c484;}"),
                     h4("Table of Rebalancing Portfolio Weight"),
                     hr(),
                     withLoader(DT::dataTableOutput("weight", width = "auto", height = "auto")),
                     fixedRow(
                       column(6,
                        h4("Time Series of Asset Weight"),
                        hr(),
                     pickerInput(
                       inputId = "selcom",
                       label = "Select Company", 
                       choices = colnames(file3_Weight)[-1]
                     ),
                     withLoader(plotlyOutput("plotw3", width = "auto", height = "auto"))),
                     column(6,
                        h4("Assets Allocation"),
                        hr(),
                        fixedRow(
                        column(6,
                        pickerInput(
                        inputId = "selcat",
                        label = "Select Factor", 
                        choices = colnames(file1_Status)
                            )),
                        column(6,
                        pickerInput(
                          inputId = "seldate",
                          label = "Select Date", 
                          choices = unlist(file3_Weight$Date, use.names = F)
                        ))),
                        withLoader(plotlyOutput("plotw3port", width = "auto", height = "auto"))
                            )),
                     h4("Allocation of Attributes in Portfolio"),
                     withLoader(plotlyOutput("plotw3F", width = "auto", height = "auto")),
                     fixedRow(
                       column(6,
                              h4("Distribution of Attributes for Long Position"),
                              hr(),
                              withLoader(plotlyOutput("plotw3Flong"))
                       ),
                       column(6,
                              h4("Distribution of Attributes for Short Position"),
                              hr(),
                              withLoader(plotlyOutput("plotw3Fshort")))
                     ),
                     h4("Table of Time Sereis of Attributes"),
                     hr(),
                     withLoader(DTOutput("plotw3Ftab", width = "auto", height = "auto")),
                     fixedRow(
                       column(6,
                              h4("Distribution of Portfolio Risk Profiles"),
                              hr(),
                              withLoader(plotlyOutput("radar"))
                              ),
                       column(6,
                              h4("Efficiency Frontier"),
                              hr(),
                              actionBttn(
                                inputId = "subeff",
                                label = "submit",
                                style = "pill", 
                                color = "success",
                                size = "xs"
                              ),
                              pickerInput(
                                inputId = "seldateeff",
                                label = "Select Dates", 
                                choices = file3_Weight[,1],
                                multiple = TRUE,
                                options = list(
                                  `selected-text-format` = "count > 3")
                              ),
                              withLoader(plotlyOutput("eff", width = "auto", height = "auto")))
                       ),
                     h4("Table of Risk Profiles"),
                     hr(),
                     withLoader(DT::dataTableOutput("plotw3risk", width = "auto", height = "auto")),
                     h4("Time Series of Risk Profiles"),
                     hr(),
                     withLoader(plotlyOutput("plotw3riskplot", width = "auto", height = "auto"))
                     )
            )),

tabPanel(title = "Step 5",
      fixedRow(
            column(1,
                   dropdown(
                     pickerInput(
                     inputId = "rate",
                     label = "Select Rate of Divestment", 
                     choices = list("Instant" = 0, "Linear" = 1,
                                    "Hyperbolic" = 2)),
                     conditionalPanel(
                       condition = "(input.rate == 1)||(input.rate == 2)",
                       pickerInput(
                         inputId = "lastdate",
                         label = "Select End Date of Divestment", 
                         choices = NULL)
                     ),
                    
                     conditionalPanel(
                       condition = "input.rate == 1",
                       withMathJax("$$D(t) = m\\times (t - t_{end})$$"),
                       textInput("m", "Slope (m)", -0.1)
                     ),
                     conditionalPanel(
                       condition = "input.rate == 2",
                       withMathJax("$$D(t) = 1/t^a$$"),
                       textInput("a", "Exponent (a)", 0.5)
                     ),
                     actionBttn(
                       inputId = "subdivpreview",
                       label = "preview",
                       style = "pill", 
                       color = "success",
                       size ="sm"
                     ),
                     actionBttn(
                       inputId = "subdiv",
                       label = "submit",
                       style = "pill", 
                       color = "success",
                       size ="sm"
                     ),
                     withLoader(plotlyOutput("plot4divpreview", width = "auto", height = "auto")),
                     style = "unite", icon = icon("minus"),
                     status = "primary", width = "300px",
                     animate = animateOptions(
                       enter = animations$fading_entrances$fadeInLeftBig,
                       exit = animations$fading_exits$fadeOutRightBig)
                     ),
                   dropdown(fileInput("file4", "F4: CSV File of Divestment Schedule", 
                                      accept = c(".csv")),
                            style = "unite", icon = icon("check"),
                            status = "primary", width = "300px",
                            animate = animateOptions(
                              enter = animations$fading_entrances$fadeInLeftBig,
                              exit = animations$fading_exits$fadeOutRightBig
                            ))
                   ),
            column(11,
                     hr(style = "border-top: 6px solid #0784BF;"),
                     h3("Step 5: Divestment"),
                     hr(style = "border-top: 6px solid #0784BF;"),
                   
                   box(
                     title = h4("User Guide"),
                     HTML("This STEP is to divest the assets in divesting list from the portfolio according to the divestment schedule - limit of the investment weights for divestable assets at a given time. The comparison of performances to the non-divest portfolio will be given in the STEP The divestment strategy is to allocate capital from the divestable assets to the investable assets proportionally to their original weights. The full details of the divestment strategy can be found in the paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b> available on SSRN. Note that the process in STEP 2, STEP 3, and STEP 4 need to be completed before running this STEP."),
                         tags$br(""),
                         "How to use this page?",
                         tags$br(""),
                         HTML("<b> 1. Already have data in CSV format </b>"),
                         tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b> CSV File of Divestment Schedule</b> with two columns; the first column named 'Date', containing rebalancing dates in the form of 'year-month-day'. Another column of the name 'Bound', is a limit on the total investment weight of the divestable assets")),
                         img(src = 'file4.png'),
                         tags$br(""),
                         HTML("<b>or 2. Calculate the divestment schedule using the tool</b>"),
                         tags$li(HTML("Click <b>Minus icon</b> <i class='fa-solid fa-minus' style = 'color:#0072B2;'></i>, select type of portfolio in <b>Select Portfolio Type</b>. There are three options in <b>Select Rate of Divestment</b> for users; <br> 
                                      <p style='color:blue'>1. Instant</p> is to withdraw all divestable assets at the beginning. The governing equation is given by 
                                      $$D(t) = 0,$$ for all $t$, where $D(t)$ is the divestment schedule at time $t$.<br> 
                                      <p style='color:blue'>2. Linear</p> is to gradually withdraw divestable assets by the equally capital, $m$, each time step. The governing equation is given by 
                                      $$D(t) = m \\times (t -t_{end}),$$ where $t_{end}$ is the index of last date of divestment. <br> 
                                      <p style='color:blue'>3. Hyperbolic</p> is to fast withdraw divestable assets in the early time step based on a hyperbolic decreasing function with exponent $a$. The governing equation is given by $$D(t) = 1/t^a$$, where $a$ is the non-negative exponent. <br>")),
                         tags$br(""),
                         tags$li(HTML("<b>Select End Date of Divestment</b> is the option to set the last date of divestment. After this date, the sum of weights of all divestable assets is set to be zero.")),
                         tags$li(HTML("Click <b>submit</b> button after filling all parameters to obtain the performance of the divested portfolio.")),
                         hr(),
                         "The remarks for this STEP is listed below",
                         tags$br(""),
                         tags$li(HTML("<b>Table of Divestment Schedule</b> shows the divestment schedule uploaded from <b>CSV File of Divestment Schedule</b> or the divestment schedule generated by the tool.")),
                         tags$li(HTML("<b>Portfolio Weight After Divestment</b> shows the weights of the divested portfolio over time according to the divestment schedule.")),
                         tags$li(HTML("<b>Sum of Weights of Divestable Assets</b> shows the chart of the sum of the weights of divestable assets over time. The weights will decrease over time, and the capital will be allocated to the investable assets.")),
                         tags$li(HTML("<b>Sum of Allocated Weights and Investable Assets Weights</b> shows the chart of the sum of the weights allocated from the divestable and investable asset weights. ")),
                         tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over the time. Users can observe the dynamic behavior of each asset across the time.")),
                         tags$li(HTML("<b>Assets Allocation</b> displays the charts of portfolio weights at given time, <b>Select Date</b>. Users can observe the attribute of each asset by using the picker <b>Select Factor</b>.")),
                         tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                         tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                         tags$li(HTML("<b>Table of Time Series of Attributes</b> displays the expected attribute of the portfolio over time separated by holding positions.")),
                         tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution.")),
                         tags$li(HTML("<b>Efficiency Frontier</b> displays the plot of portfolio standard deviation versus return. The picker has multiple options that allow user to observe many curves in one plot.")),
                         tags$li(HTML("<b>Table of Risk Profiles</b> displays the table of risk profiles calculated backward before each rebalancing versus time.")),
                         tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                         tags$li(HTML("<b>Distribution of Percentage of Relative Change between Non-divested and divested Portfolios</b> displays the boxplots of the distribution of the percentage of non-divested and divested Portfolios spread out. It is calculated by,
                                      $$PR = \\frac{AP_{divest} - AP_{non-divest}}{AP_{non-divest}} \\times 100\\%$$, where $AP_{non-divest}$ and $AP_{divest}$ are the attribute or risk profile of the portfolio before, and 
                                      after divestment, respectively")),
                         tags$br(""),
                         "* User Guide box is collapsible",
                     id = "mybox",
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = TRUE,
                     width = 13
                   ),
                   
                     h4("Table of Divestment Schedule"),
                     hr(),
                   progressBar(
                     id = "pb2",
                     value = 0,
                     total = 100,
                     title = "Process does not start yet",
                     display_pct = TRUE,
                     status = "custom"
                   ),
                     withLoader(DT::dataTableOutput("plot4divtab", width = "auto", height = "auto")),
                     h4("Portfolio Weight After Divestment"),
                     hr(),
                     withLoader(DT::dataTableOutput("plot4divtabsum", width = "auto", height = "auto")),
                     fixedRow(
                       column(6,
                              h4("Sum of Weights of Divestable Assets"),
                              hr(),
                              withLoader(plotlyOutput("plotw3InDiv1", width = "auto", height = "auto")
                       )),
                       column(6,
                              h4("Sum of Allocated Weights and Investable Assets Weights"),
                              hr(),
                              withLoader(plotlyOutput("plotw3InDiv2", width = "auto", height = "auto"))
                     )),
                     fixedRow(
                       column(6,
                              h4("Time Series of Asset Weight"),
                              hr(),
                              pickerInput(
                                inputId = "selcomdiv",
                                label = "Select Company", 
                                choices = colnames(file5_Weight_Div)[-1]
                              ),
                              withLoader(plotlyOutput("plotw3div", width = "auto", height = "auto"))),
                       column(6,
                              h4("Assets Allocation"),
                              hr(),
                              fixedRow(
                                column(6,
                                       pickerInput(
                                         inputId = "selcatdiv",
                                         label = "Select Factor", 
                                         choices = colnames(file1_Status)
                                       )),
                                column(6,
                                       pickerInput(
                                         inputId = "seldatediv",
                                         label = "Select Date", 
                                         choices = unlist(file5_Weight_Div$Date, use.names = F)
                                       ))),
                              withLoader(plotlyOutput("plotw3portdiv", width = "auto", height = "auto"))),
                              h4("Attribute Allocation in Portfolio"),
                              hr(),
                              withLoader(plotlyOutput("plotw3Fdiv", width = "auto", height = "auto"))
                       ),
                     fixedRow(
                       column(6,
                              h4("Distribution of Attributes for Long Position"),
                              hr(),
                              withLoader(plotlyOutput("plotw3Flongdiv"))
                       ),
                       column(6,
                              h4("Distribution of Attributes for Short Position"),
                              hr(),
                              withLoader(plotlyOutput("plotw3Fshortdiv")))
                     ),
                     withLoader(DTOutput("plotw3Ftabdiv", width = "auto", height = "auto")),
                     fixedRow(
                       column(6,
                              h4("Distribution of Portfolio Risk Profiles"),
                              hr(),
                              withLoader(plotlyOutput("radardiv"))
                       ),
                       column(6)
                     ),
                     h4("Table of Risk Profiles"),
                     hr(),
                     withLoader(DT::dataTableOutput("plotw3riskdiv", width = "auto", height = "auto")),
                     h4("Time Series of Risk Profiles"),
                     hr(),
                     withLoader(plotlyOutput("plotw3riskplotdiv", width = "auto", height = "auto")),
                     h4("Table of Time Series of Risk Profiles"),
                     hr(),
                     withLoader(DT::dataTableOutput("plotdiftable", width = "auto", height = "auto")),
                     h4("Distribution of Percentage of Relative Change between Non-divested and Divested Portfolios"),
                     hr(),
                     textInput('ly.st5','Set Limit of y-axis',NA),
                     actionBttn(
                        inputId = "sub.ly.st5",
                        label = "submit",
                        style = "pill", 
                        color = "success",
                        size = "xs"
                   ),
                     withLoader(plotlyOutput("plotdifboxplot", width = "auto", height = "auto"))
                     
                     ))
      ),

tabPanel(title  = "Option I",
        fixedRow(
          column(1, 
                 dropdown(fileInput("file6", "F5: CSV File of Weights Dynamic", 
                                    accept = c(".csv")),
                          fileInput("file7", "F1: CSV File of Attributes", 
                                    accept = c(".csv")),
                          fileInput("file8", "F2: CSV File of Returns", 
                                    accept = c(".csv")),
                          style = "unite", icon = icon("check"),
                          status = "primary", width = "300px",
                          animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                          ),
                          actionBttn(
                            inputId = "subcomp",
                            label = "submit",
                            style = "pill", 
                            color = "success",
                            size ="sm"
                          )
                 )),
          column(11,
                   hr(style = "border-top: 6px solid #0784BF;"),
                   h3("Option I: Multiple Portfolios Comparison"),
                   hr(style = "border-top: 6px solid #0784BF;"),
                 
                 box(
                   title = h4("User Guide"),
                       "This optional STEP is used for analysis and comparing the performance of many portfolios. It works independently of other STEP can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP.",
                       tags$br(""),
                       "How to use this page?",
                       tags$br(""),
                       tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                       img(src = 'file5.png'),
                       tags$br(""),
                       tags$li(HTML("Upload <b>CSV File of Attributes</b> that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest', and <br>
                                    iii) at least one numeric column of any name.")),
                       img(src = 'file1.png'),
                       tags$br(""),
                       tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                       img(src = 'file2.png'),
                       tags$br(""),
                       hr(),
                       "The remarks for this STEP is listed below",
                       tags$br(""),
                       tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                       tags$li(HTML("<b>Table of Attributes</b> shows the attributes of assets uploaded from <b>CSV File of Attributes</b>.")),
                       tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                       tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over time. Users can observe the dynamic behavior of each asset across time.")),
                       tags$li(HTML("<b>Sum of Weights of Divestable Assets</b> shows the chart of the sum of the weights of divestable assets over time. The weights will decrease over time, and the capital will be allocated to the investable assets.")),
                       tags$li(HTML("<b>Sum of Allocated Weights and Investable Assets Weights</b> shows the chart of the sum of the weights allocated from the divestable and investable asset weights. ")),
                       tags$li(HTML("<b>Table of Time Sereis of Attributes</b> displays the attribute weighted by portfolio weights over time, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately.")),
                       tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights over time. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                       tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                       tags$li(HTML("<b>Table of Time Series of Risk Profiles</b> displays the risk profiles of the portfolio over time separated by holding positions.")),
                       tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution.")),
                       tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                       tags$li(HTML("<b>Percentage of Relative Change compared to Benchmarks</b> is the measure of the difference between the benchmark portfolios from others. The formula is given by
                       $$PR = \\frac{AP_{other} -AP_{benchmark}}{AP_{benchmark}} \\times 100\\%$$, where $AP_{benchmark}$ and $AP_{other}$ are the attribute or risk profile of the benchmark, and comparing portfolio, respectively <br>
                       <b>Select Benchmarks</b> is the option to set the benchmark portfolios. Users can select more than one benchmark. According to the order of the list in the picker, Portfolios after the benchmarks are compared to the nearest benchmark before them.")),
                       tags$li(HTML("<b>Table of Percentage of Relative Change compared to Benchmarks</b> displays the percentages of the relative change of attributes and risk profiles of benchmarks and other portfolios.")),
                       tags$li(HTML("<b>Distribution of Percentage of Relative Change compared to Benchmarks</b> displays the distribution of percentages of relative change of attributes and risk profiles of benchmarks and other portfolios. The white dot represents the average value of the distribution.")),
                       tags$br(""),
                       "* User Guide box is collapsible",
                   id = "mybox",
                   collapsible = TRUE,
                   collapsed = TRUE,
                   closable = TRUE,
                   width = 13
                 ),
                 
                   h4("Table of Investment Weights"),
                   hr(),
                   withLoader(DT::dataTableOutput("div_dynamic", width = "auto", height = "auto")),
                   h4("Table of Attributes"),
                   hr(),
                   withLoader(DT::dataTableOutput("found", width = "auto", height = "auto")),
                   h4("Table of Returns"),
                   hr(),
                   withLoader(DT::dataTableOutput("hist_return", width = "auto", height = "auto")),
  
                            h4("Time Series of Assets Weight"),
                            hr(),
                            pickerInput(
                              inputId = "selcomcom",
                              label = "Select Company", 
                              choices = NULL
                            ),
                   withLoader(plotlyOutput("plotcom1", width = "auto", height = "auto")),
                   fixedRow(
                     column(6,
                            h4("Sum of Weights of Divestable Assets"),
                            hr(),
                            withLoader(plotlyOutput("plotcom2", width = "auto", height = "auto")
                            )),
                     column(6,
                            h4("Sum of Weights Allocated to Investable Assets"),
                            hr(),
                            withLoader(plotlyOutput("plotcom3", width = "auto", height = "auto"))
                     )),
                 h4("Table of Time Sereis of Attributes"),
                 hr(),
                 withLoader(DT::dataTableOutput("tab2", width = "auto", height = "auto")),
                 h4("Attribute Allocation in Portfolio"),
                 hr(),
                 withLoader(plotlyOutput("plotcom6", width = "auto", height = "auto")),
                   fixedRow(
                     column(6,
                            h4("Distribution of Attributes for Long Position"),
                            hr(),
                            withLoader(plotlyOutput("plotcom7", width = "auto", height = "auto")
                            )),
                     column(6,
                            h4("Distribution of Attributes for Short Position"),
                            hr(),
                            withLoader(plotlyOutput("plotcom8", width = "auto", height = "auto"))
                     )),
                   h4("Table of Time Series of Risk Profiles"),
                   hr(),
                   withLoader(DT::dataTableOutput("tab1", width = "auto", height = "auto")),
                   h4("Distribution of Risk Profiles"),
                   hr(),
                   withLoader(plotlyOutput("plotcom4", width = "auto", height = "auto")),
                 h4("Time Series of Risk Profiles"),
                 hr(),
                 withLoader(plotlyOutput("plotcom5", width = "auto", height = "auto")),
                 h4("Percentage of Relative Change compared to Benchmarks"),
                 hr(),       
                 pickerInput(
                   inputId = "selben.op1",
                   label = tags$span("Select Benchmarks", bsButton("bent", label = "", icon = icon("info"), style = "info", size = "extra-small")), 
                   choices = NULL,
                   multiple = TRUE,
                   options = list(
                     `selected-text-format` = "count > 3")
                 ),
          bsPopover(
            id = "bent",
            title = "More information",
            content = "Users can select more than one benchmark. The non-benchmark portfolios will be compared to the nearest benchmark before them. The names of portfolios will be arranged according to the input CSV file.",
            placement = "right",
            trigger = "focus",
            options = list(container = "body")),
                 
                 actionBttn(
                   inputId = "submul",
                   label = "submit",
                   style = "pill", 
                   color = "success",
                   size = "xs"
                 ),

                   h4("Table of Percentage of Relative Change compared to Benchmarks"),
                   hr(),
                   DT::dataTableOutput("tabben.op1", width = "auto", height = "auto"),
                   h4("Distribution of Percentage of Relative Change compared to Benchmarks"),
                   hr(),
                   textInput("ly.opt1", "Set Limit of y-asix", NA),
                   plotlyOutput("plotben.op1", width = "auto", height = "auto")
                   )

                     )
                   ),
tabPanel(title  = "Option II",
         fixedRow(
           column(1,
                  dropdown(fileInput("file6.op2", "F5: CSV File of Weights Dynamic", 
                                     accept = c(".csv")),
                           fileInput("file8.op2", "F2: CSV File of Returns", 
                                     accept = c(".csv")),
                           style = "unite", icon = icon("check"),
                           status = "primary", width = "300px",
                           animate = animateOptions(
                             enter = animations$fading_entrances$fadeInLeftBig,
                             exit = animations$fading_exits$fadeOutRightBig
                           ),
                           textInput("tau.op2", "Number of Days in Time Window $\\tau$", 20),
                           textInput("sept.op2", "Number of Days in Cluster Analysis, $t$", 20),
                           actionBttn(
                             inputId = "subclust",
                             label = "submit",
                             style = "pill", 
                             color = "success",
                             size ="sm"
                           )
                  )
                  ),
           column(11, 
                  hr(style = "border-top: 6px solid #0784BF;"),
                  h3("Option II: Stability Analysis via Clustering"),
                  hr(style = "border-top: 6px solid #0784BF;"),
                  
                  box(
                    title = h4("User Guide"),
                    HTML("This optional STEP is used for investigating the stability of portfolio performance.  We employ the clustering algorithm, namely Clustering Large Applications or CLARA, an approximate extension of k-medoids methods to achieve stability investigation. The CLARA is applied to cluster the aggregation time series of risk profiles with a time window of $\\tau$ days. The clustering algorithm is applied every $t$ day. Those parameters can be set in the tool. The optimal number of clusters will be determined by the clustering silhouette. Portfolios that have similar relative behavior will belong to the same cluster over time. It works independently of other STEP. They can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP."),
                        tags$br(""),
                        "How to use this page?",
                        tags$br(""),
                        tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                        img(src = 'file5.png'),
                        tags$br(""),
                        tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                        img(src = 'file2.png'),
                        tags$br(""),
                        tags$li(HTML("<b>Number of Days in Time Window, $\\tau$ </b> is the number of days in the time window for calculating the aggregate time series of risk profiles.")),
                        tags$li(HTML("<b>Number of Days in Cluster Analysis, $t$ </b> is the number of days for applying a clustering method. The algorithm is applied $\\lceil \\frac{\\text{number of days whole period}}{\\text{number of days in cluster analysis}}\\rceil$ times.")),
                        hr(),
                        "The remarks for this STEP is listed below",
                        tags$br(""),
                        tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                        tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                        tags$li(HTML("<b>Table of Aggregate Time Series of Risk Profiles</b> displays the aggregate time series of risk profiles of portfolios versus time.")),
                        tags$li(HTML("<b>Aggregate Time Series of Risk Profiles</b> displays the plots of the aggregate time series of risk profiles of portfolios versus time.")),
                        tags$li(HTML("<b>Table of Clustering Silhouette</b> gives the silhouette information of clustering. The column name is the number of clusters.")),
                        tags$li(HTML("<b>Clustering Results</b> displays the heatmaps of the clustering of all portfolios over time. The number inside the cells and color represents the label of the cluster.")),
                        
                        tags$br(""),
                        "* User Guide box is collapsible",
                    id = "mybox",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    closable = TRUE,
                    width = 13
                  ),
                  
           h4("Table of Investment Weights"),
           hr(),
           DT::dataTableOutput("tab.op2", width = "auto", height = "auto"),
           h4("Table of Returns"),
           hr(),
           DT::dataTableOutput("tab.op2.2", width = "auto", height = "auto"),
           h4("Table of Aggregate Time Series of Risk Profiles"),
           hr(),
           DT::dataTableOutput("op2tab1", width = "auto", height = "auto"),
           h4("Time Series of Risk Profiles"),
           hr(),
           withLoader(plotlyOutput("op2graph", width = "auto", height = "auto")),
           h4("Table of Clustering Silhouette"),
           hr(),
           DT::dataTableOutput("op2tab2", width = "auto", height = "auto"),
           h4("Clustering Results"),
           hr(),
           withLoader(plotlyOutput("op2hm", width = "auto", height = "auto"))
           ))),

tabPanel(title  = "Option III",         
         fixedRow(
           column(1,
                  dropdown(fileInput("file6.op3", "F5: CSV File of Weights Dynamic", 
                                     accept = c(".csv")),
                           fileInput("file7.op3", "F6: CSV File of Status Divestment, optional", 
                                     accept = c(".csv")),
                           fileInput("file8.op3", "F2: CSV File of Returns", 
                            accept = c(".csv")),
                           style = "unite", icon = icon("check"),
                           status = "primary", width = "300px",
                           animate = animateOptions(
                             enter = animations$fading_entrances$fadeInLeftBig,
                             exit = animations$fading_exits$fadeOutRightBig
                           ))),
             column(11, 
                    hr(style = "border-top: 6px solid #0784BF;"),
                    h3("Option III: Graph Structure Correlation Analysis"),
                    hr(style = "border-top: 6px solid #0784BF;"),
                    
                    box(
                      title = h4("User Guide"),
                      HTML("This optional STEP is used for investigating the correlation structure of the assets in portfolios using graph LASSO. This function is suitable for studying the dynamic correlation structure of portfolios and comparison of their diversification as portfolio dynamically evolves, for instance as divestment is progressively performed on different sets of assets.
                          It works independently of other STEP users. They can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP."),
                          tags$br(""),
                          "How to use this page?",
                          tags$br(""),
                          tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                          img(src = 'file5.png'),
                          tags$br(""),
                          tags$li(HTML("Upload <b>CSV File of Status Divestment</b> is optional that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest'.")),
                          img(src = 'file1.png'),
                          tags$br(""),
                          tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                          img(src = 'file2.png'),
                          tags$br(""),
                          hr(),
                          "The remarks for this STEP is listed below",
                          tags$br(""),
                          tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                          tags$li(HTML("<b>Table of Divestment Status</b> shows the tickers and their investment status uploaded from <b>CSV File of Status Divestment</b>.")),
                          tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                          tags$li(HTML("<b>Covariance Portfolio Network Structure</b> displays the network obtained by the graph LASSO algorithm. The tool displays three timesteps for comparing structures. To process the algorithm, select a time in <b> Select Date</b> 
                                       and click the <b> submit</b> button. The correlation of the asset is represented by the link, and its strength is indicated by the thickness. The negative and positive correlations are colored in red and green, respectively. Nodes of the network represent the assets, 
                                       and their colors indicate investment status if a <b>CSV File of Status Divestment</b> was uploaded. The investable assets are colored blue and the divestable assets are colored yellow. The label of nodes are ordered clockwise and alphabetically order with the first node at the top-center position.
                                       It should be noted that even in a small portfolio, the process can be computationally expensive. The computation time varies by the number of assets and the number of portfolios.")),
                          
                          tags$br(""),
                          "* User Guide box is collapsible",
                      id = "mybox",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = TRUE,
                      width = 13
                    ),
                    h4("Table of Investment Weights"),
                    hr(),
                    DT::dataTableOutput("tab.op3", width = "auto", height = "auto"),
                    h4("Table of Divestment Status"),
                    hr(),
                    DT::dataTableOutput("tab.op3.1", width = "auto", height = "auto"),
                    h4("Table of Returns"),
                    hr(),
                    DT::dataTableOutput("tab.op3.2", width = "auto", height = "auto"),
                    h4("Portfolio Network Structure of Covariance"),
                    hr(),

                    fixedRow(
                      column(4,
                      progressBar(
                         id = "pbg1",
                        value = 0,
                        total = 100,
                        title = "Process does not start yet",
                        display_pct = TRUE,
                        status = "custom"), 
                    pickerInput(
                      inputId = "gdate1",
                      label = "Select Date", 
                      choices = NULL),
                    actionBttn(
                      inputId = "subgdate1",
                      label = "submit",
                      style = "pill", 
                      color = "success",
                      size = 'xs'
                    ),
                    plotOutput('op3.1', height = "1000px")
                    ),
                    column(4,
                           progressBar(
                             id = "pbg2",
                             value = 0,
                             total = 100,
                             title = "Process does not start yet",
                             display_pct = TRUE,
                             status = "custom"
                           ),
                           pickerInput(
                             inputId = "gdate2",
                             label = "Select Date", 
                             choices = NULL),
                           actionBttn(
                             inputId = "subgdate2",
                             label = "submit",
                             style = "pill", 
                             color = "success",
                             size = 'xs'
                           ),
                    plotOutput('op3.2', height = "1000px")
                    ),
                    column(4,
                           progressBar(
                             id = "pbg3",
                             value = 0,
                             total = 100,
                             title = "Process does not start yet",
                             display_pct = TRUE,
                             status = "custom"
                           ),
                           pickerInput(
                             inputId = "gdate3",
                             label = "Select Date", 
                             choices = NULL),
                           actionBttn(
                             inputId = "subgdate3",
                             label = "submit",
                             style = "pill", 
                             color = "success",
                             size = 'xs'
                           ),
                    plotOutput('op3.3', height = "1000px")
                    ))))),
         
         )),


                      tabItem(tabName = "about_us",
                            fixedRow(
                              userBox(
                                width = 6,
                                title = userDescription(
                                  title = HTML("<p style='color:white'>Christina Nikitopoulos Sklibosios</p>"),
                                  subtitle = HTML("<p style='color:white'>Associate Professor Finance Discipline Group, <br> University of Technology Sydney </p>"),
                                  backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                                  image = "cns.jpg",
                                  type = 2
                                ),
                                status = "teal",
                                boxToolSize = "xl",
                                collapsible = FALSE,
                                footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> Christina.Nikitopoulos@uts.edu.au<br><br>
                                <a href='https://www.linkedin.com/in/kylie-anne-richards/?originalSubdomain=au'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Kylie_Anne_Richards2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2060205'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com.au/citations?user=nQ6iMnEAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://profiles.uts.edu.au/Christina.Nikitopoulos'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                                              #
                              ),
                              userBox(
                                width = 6,
                                title = userDescription(
                                  title = HTML("<p style='color:white'>Eric Ofusu Hene</p>"),
                                  subtitle = HTML("<p style='color:white'>Senior Lecturer in Accounting and Finance, <br> De Montfort University</p>"),
                                  backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                                  image = "eoh.jpg",
                                  type = 2
                                ),
                                status = "teal",
                                boxToolSize = "xl",
                                collapsible = FALSE,
                                footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> eric.ofusu-hene@dmu.ac.uk<br><br>
                                <a href='https://www.linkedin.com/in/eric-dei-ofosu-hene-phd-526b3a96/?originalSubdomain=uk'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Eric-Ofosu-Hene'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2756712'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com/citations?user=YBJxo5gAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.dmu.ac.uk/about-dmu/academic-staff/business-and-law/eric-dei-ofosu-hene/eric-dei-ofosu-hene.aspx'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                                
                              ),
                             
                              userBox(
                                width = 6,
                                title = userDescription(
                                  title = HTML("<p style='color:white'>Gareth W. Peters</p>"),
                                  subtitle = HTML("<p style='color:white'>Janet & Ian Duncan Endowed Chair of Actuarial Science, <br>
                                  Chair Professor of Statistics for Risk and Insurance, <br>
                                  University of California, Santa Barbara</p>"),
                                  backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                                  image = "gwp.png",
                                  type = 2
                                ),
                                status = "teal",
                                boxToolSize = "xl",
                                collapsible = FALSE,
                                footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> garethpeters@ucsb.edu<br><br>
                                <a href='https://www.linkedin.com/in/prof-dr-gareth-w-peters-3928b4139/'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Gareth-Peters'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2019678'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.co.jp/citations?user=lsb_nJoAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.qrslab.com/'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                              ),
                              userBox(
                                width = 6,
                                title = userDescription(
                                  title = HTML("<p style='color:white'>Kylie-Anne Richards</p>"),
                                  subtitle = HTML("<p style='color:white'>Investment Practices, Fortlake <br>
                                                  Lecturer Finance Discipline Group, <br> University of Technology Sydney</p>"),
                                  backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                                  image = "kar.png",
                                  type = 2
                                ),
                                status = "teal",
                                boxToolSize = "xl",
                                collapsible = FALSE,
                                footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> Kylie-Anne.Richards@uts.edu.au<br><br>
                                <a href='https://www.linkedin.com/in/kylie-anne-richards-9660765/?originalSubdomain=au'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Kylie_Anne_Richards2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2060205'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://profiles.uts.edu.au/Kylie-Anne.Richards'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                              ),
                              userBox(
                                width = 6,
                                title = userDescription(
                                  title = HTML("<p style='color:white'>Pasin Marupanthorn</p>"),
                                  subtitle = HTML("<p style='color:white'>Graduate Student in Actuarial Mathematics, <br> Heriot-Watt University and <br> The Maxwell Institute for Mathematical Sciences</p>"),
                                  backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                                  image = "pm.jpg",
                                  type = 2
                                ),
                                status = "teal",
                                boxToolSize = "xl",
                                collapsible = FALSE,
                                footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> pm122@hw.ac.uk<br><br>
                                <a href='https://www.linkedin.com/in/pasin-marupanthorn/?originalSubdomain=uk'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Pasin-Maruphanton-2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=5286317'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com/citations?user=NcoXQYYAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://oporkabbb.wixsite.com/math'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                              )
                            )
                            )
)
)
)


server <- function(input, output, session) {
data_yahoo  <- eventReactive(input$submit, {get_data(input$ticker)})
data_stock <- eventReactive(input$submit, {get_stock_profile(input$ticker)})
candel_stock <- eventReactive(input$submit, {candlestick_plot(data_yahoo(),input$ticker)})
perf_stock <- eventReactive(input$submit, {performance_plot(data_yahoo(),input$ticker)})
re_ticker <- eventReactive(input$add, {
  main_port <<- unique(c(main_port,toupper(as.character(input$ticker_sel))))
  main_port})
output$lev_esg <- renderValueBox({
  valueBox(
    value = tags$p(data_stock()[4], style = "font-size: 70%;"),
    subtitle = tags$p(data_stock()[10], style = "font-size: 70%;"),
    color = ifelse(data_stock()[5] %in% validcol,data_stock()[5],'black'),
    icon = icon('seedling')
  )})
output$total_esg <- renderValueBox({
  valueBox(
    value = tags$p(data_stock()[6], style = "font-size: 70%;"),
    subtitle = tags$p("Total ESG Score ", style = "font-size: 70%;"),
    color = ifelse(data_stock()[5] %in% validcol,data_stock()[5],'black'),
    icon = icon('tree')
  )})#icon("thumbs-up", lib = "glyphicon")
output$e_score<- renderValueBox({
  valueBox(
    value = tags$p(data_stock()[7], style = "font-size: 70%;"),
    subtitle = tags$p('Environment', style = "font-size: 70%;"),
    color = 'light-blue',
    icon = icon('solar-panel')
  )})
output$s_score<- renderValueBox({
  valueBox(
    value = tags$p(data_stock()[8], style = "font-size: 70%;"),
    subtitle = tags$p('Social', style = "font-size: 70%;"),
    color = 'aqua',
    icon = icon('hashtag')
  )})
output$g_score <- renderValueBox({
  valueBox(
    value = tags$p(data_stock()[9], style = "font-size: 70%;"),
    subtitle = tags$p('Governance', style = "font-size: 70%;"),
    color = 'blue',
    icon = icon('handshake')
  )})

table_esg <- eventReactive(input$subesg, {
  port <- unique(c(as.character(input$source),as.character(input$divt)));
  tab_esg <- sapply(seq_along(port), function(i) get_esg_profile(port[i])) %>% t() %>% data.frame() %>% `colnames<-`(c("Name","Ticker","Sector","Subsector","ESG Level","ESG","Enironment","Social","Governance"));
  for(i in 6:9){tab_esg[,i] <- as.numeric(tab_esg[,i])};
  tab_esg$Status <- ifelse((port %in% as.character(input$source)),"Invest","Divest");
  file1_Status <<- tab_esg;
  output$ui_esg_table <- DTtable(file1_Status)
  file1_Status
})

observeEvent(input$subup, {
  if(is.null(file1_Status)){
    feedbackWarning(
    inputId = "subup",
    show = FALSE,
    text = "Upload File before")
  }else{
    port <- unique(c(as.character(input$source),as.character(input$divt)))
    port_up <- setdiff(port,file1_Status$Ticker)
    if(length(port_up) != 0){
      tab_esg <- sapply(seq_along(port_up), function(i) get_esg_profile(port_up[i])) %>% t() %>% data.frame() %>% `colnames<-`(c("Name","Ticker","Sector","Subsector","ESG Level","ESG","Enironment","Social","Governance"))
      for(i in 6:9){
        tab_esg[,i] <- as.numeric(tab_esg[,i])
      }
      tab_esg$Status <- ifelse((port_up %in% as.character(input$source)),"Invest","Divest")
      file1_Status <<- rbind(file1_Status,tab_esg) %>% subset(Ticker %in% port)
    }else{
      dum <- rep(0,nrow(file1_Status))
      for(i in 1:length(file1_Status$Ticker)){
        dum[i] <-  ifelse((file1_Status$Ticker[i] %in% as.character(input$source)),"Invest","Divest")
      }
      file1_Status$Status <<- dum 
      file1_Status <<- file1_Status %>% subset(Ticker %in% port)
    }
    
    output$ui_esg_table <- DTtable(file1_Status)
  }
  })

observeEvent(input$fileticker, {
  File <- input$fileticker
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  tick <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  if(ncol(tick) == 1){
    tick$inv <- rep(1,nrow(tick))
  }else if(ncol(tick) == 2){
    tick <- tick
  }else{
    tick <- NULL
  }
  inv_asset <<- as.character(tick[,1][tick[,2] == 1])
  div_asset <<- as.character(tick[,1][tick[,2] == 0])
  updateOrderInput(session,inputId = "source", items =  inv_asset, item_class = 'info')
  updateOrderInput(session,inputId = "divt", items =  div_asset, item_class = 'info')
})

observeEvent(input$file1, {
  File <- input$file1
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  f1 <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  con1 <- (sum(sapply(f1, class) %in% c('interger', 'numeric')) == 0)
  con2 <- (sum(c("Status",'Ticker') %in% colnames(f1)) != 2)
  if(con1){
    showFeedbackWarning(
      inputId = "file1",
      text = "Require at least on column to be numeric")
    }else if(con2){
      showFeedbackWarning(
      inputId = "file1",
      text = "Missing columns of the names 'Ticker' and 'Status'" )
    }
    else if(FALSE %in% (unlist(f1$Status) %in% c('Invest', 'Divest'))){
    showFeedbackWarning(
      inputId = "file1",
      text = "Require Status to be 'Invest' or 'Divest'" )
    }else if(sum(is.na(f1)) != 0){
    showFeedbackWarning(
      inputId = "file1",
      text = "Missing Data Detected" )
  }else{
    file1_Status <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$ui_esg_table <- DTtable(file1_Status)
    inv_asset <<- (file1_Status %>% dplyr::filter(Status == 'Invest'))$Ticker 
    div_asset <<- (file1_Status %>% dplyr::filter(Status == 'Divest'))$Ticker
    updateOrderInput(session,inputId = "source", items =  inv_asset, item_class = 'info')
    updateOrderInput(session,inputId = "divt", items =  div_asset, item_class = 'info')
  }
}, ignoreNULL = FALSE)


observeEvent(input$file2, {
  File <- input$file2
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  f2 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f2,c("Date"))
  
  if(cons[1]){
    showFeedbackWarning(
      inputId = "file2",
      text = "Require at least on column to be numeric")
  }else if(cons[2]){
    showFeedbackWarning(
      inputId = "file2",
      text = "Missing the column of the names 'Date'" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file2",
      text = "Missing Data Detected" )
  }else{
    file2_Historical <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    plottab <- file2_Historical
    output$tab_return <- DTtable(round_tab(plottab))
    output$reportreturn <-  DTtable(return_summary_update(file2_Historical))
  }
  
}, ignoreNULL = FALSE)


observeEvent(input$add, {
  updateOrderInput(session,inputId = "inter", items =  re_ticker(), item_class = 'info')
})  

observeEvent(input$reasset, {
  updateOrderInput(session,inputId = "inter", items =  NULL, item_class = 'info')
})  


batch <- eventReactive(input$subtime, {
  if(is.null(file1_Status)){
    noty(text = "Require Uploading File in Step 2 before", type = "warning")
  }else{
  
  data <- BatchGetSymbols(file1_Status$Ticker,
                                                               as.Date(input$stdate),
                                                               as.Date(input$enddate),
                                                               0.1)
dataframe <- data[[2]] %>% dplyr::select(ticker,ref.date,ret.adjusted.prices) %>% drop_na() %>% dcast(ref.date ~ ticker) 
colnames(dataframe)[which(names(dataframe) == "ref.date")] <- "Date"
file2_Historical <<- dataframe
file2_Historical}
})

output$tab_return <- DTtable(NULL);
observeEvent(input$subtime,{
  shiny::validate(need(!is.null(file1_Status)," "))
  plottab <- batch();
plottab[,2:ncol(plottab)] <- round(plottab[,2:ncol(plottab)],6);
output$tab_return <- DTtable(plottab);
output$reportreturn <-  DTtable(return_summary_update(batch()))})




output$comp_name <- renderText(data_stock()[1])
output$comp_market <- renderText(data_stock()[14])
output$comp_sec <- renderText(paste0("Sector: ", data_stock()[2],", Subsector: ",data_stock()[3]))
output$comp_sum <- renderText(paste0("Market Cap : ", data_stock()[11],", PE Ratio: ",data_stock()[12],", Forward Dividend & Yield: ",data_stock()[13]))
output$perf_fig <- renderPlotly(perf_stock()[[2]])  
output$perf_table <- DTtable(perf_stock()[[1]]) 
output$verb <- renderText({re_ticker()})

output$ui_esg_table <-  DTtable(table_esg())

output$reportreturn <-  DTtable(return_summary(batch()[[2]]))

output$tab_return <- DTtable(NULL)


output$ui_esg_table_com <-  DTtable(NULL)

observeEvent(list(input$file1, input$subesg, input$subup), {
  shiny::validate(
    need((!is.null(input$file1))||(input$subesg == TRUE)||(input$subup == TRUE), "Please select a weight")
  )
 output$ui_esg_table_com  <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'));
 updatePickerInput(session, inputId = "selq", choices = file1_Status[, sapply(file1_Status, class) %in% c('numeric','integer'), drop=FALSE] %>% colnames())})

observeEvent(input$subman, {
  if((as.numeric(input$perc) > 1)||(as.numeric(input$perc) < 0)||(is.na(as.numeric(input$perc)))){
    feedbackWarning(
      inputId = "perc",
      show = TRUE,
      text = "Please input number from 0 to 1"
    )}else if(is.null(file1_Status)){
      noty("Require Table of Attributes", type = 'warning')
    }else{
  file1_Status <<- auto_selection(file1_Status, input$selq, as.numeric(input$perc),input$seldec)
  output$ui_esg_table <- DTtable(file1_Status)
  output$ui_esg_table_com  <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'))
  updateOrderInput(session,inputId = "source", items = (file1_Status %>% dplyr::filter(Status == "Invest") %>% drop_na())$Ticker, item_class = 'info')
  updateOrderInput(session,inputId = "divt", items =  (file1_Status %>% dplyr::filter(Status == "Divest") %>% drop_na())$Ticker, item_class = 'info')
 
  }})
  
  output$candel <- renderPlotly(candel_stock())
  
  
  observeEvent(input$file2,{
    updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
  })
  observeEvent(input$subtime,{
    shiny::validate(need(!is.null(file1_Status)," "))
    updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
  })

  
  output$boxasset <- renderPlotly(NULL)
  
  observeEvent(input$rankby, {
    shiny::validate(
      need(!is.null(input$rankby),"Please upload Historical Return")
    )
    output$boxasset <- renderPlotly(boxplot_assets(file2_Historical, as.character(input$rankby)))
    })


  observeEvent(input$file3, {
    File <- input$file3
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f3 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f3,c("Date"))
    
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file3",
        text = "Require at least on column to be numeric")
    }else if(cons[2]){
      showFeedbackWarning(
        inputId = "file3",
        text = "Missing the column of the names 'Date'" )
    }else if(cons[3]){
      showFeedbackWarning(
        inputId = "file3",
        text = "Missing Data Detected" )
    }else if(sum(sapply(f3[,-1],class) %in% c('integer','numeric')) != ncol(f3[,-1])){
      showFeedbackWarning(
        inputId = "file3",
        text = "Detected Non-numeric Colunms" )
    }else if(ncol(f3) <= 2){
      showFeedbackWarning(
        inputId = "file3",
        text = "Portfolio need more than one asset" )
    }else{
      file3_Weight <<- f3
      output$weight <- DTtable(file3_Weight)
    }
  }, ignoreNULL = FALSE)

  get_weight <- eventReactive(list(input$subweight,input$file3), {
  shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    if(is.null(file1_Status)||is.null(file2_Historical)){
      noty(text = "Require Uploading Files in Step 2 and 3 before", type = "warning")
    }else{
    if(is.null(input$file3)){
        updateProgressBar(id = 'pb', value = 0, total = 100, title = "Processing")
        x <- file2_Historical
        type <- input$type
        tau <- as.integer(input$tau)
        reb <- as.integer(input$reb)
        lim <- as.integer(input$limselect)*as.numeric(input$shortlim)
        filew <- NULL
        one <- as.matrix(rep(1,ncol(x)-1))
        first <- head(x[-(1:tau),],1)$Date
        last <- tail(x[-(1:tau),],1)$Date
        
        rebdate <- x[seq(reb,nrow(x),reb),]$Date
        
        returnmat <- lapply(seq_along(rebdate), function(i) {rowre <- which(x$Date == as.Date(rebdate[i])); x[(rowre-tau):(rowre-1),-1]}) %>%
          lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {ifelse(length(m[m == 0]) == 0, m <- m,m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m}) 
        
        covmat <- vector("list", length = length(returnmat))
        invcov <- vector("list", length = length(returnmat))
        for(j in 1:length(returnmat)){
          covmat[[j]] <- cov(returnmat[[j]])
          if(log10(kappa(covmat[[j]])) >= 5){
            covmat[[j]] <- as.matrix(covOGK(returnmat[[j]],sigmamu = s_mad)$cov)  
            dimnames(covmat[[j]]) <- list(colnames(x)[-1],colnames(x)[-1])
            s <- as.matrix(solve(as.matrix(covmat[[j]],tol = 1E-1000)))
            s[lower.tri(s)] <- t(s)[lower.tri(s)]
            invcov[[j]] <- s
          }else{
            s <- as.matrix(solve(as.matrix(covmat[[j]],tol = 1E-1000)))
            s[lower.tri(s)] <- t(s)[lower.tri(s)]
            invcov[[j]] <- s
          }
          title <- ifelse(j == length(returnmat),"Done","Calculating, please wait")
          updateProgressBar(id = 'pb', value = j, total = length(returnmat), title = title)
        }
        
  
        if(is.null(filew)){
          switch(as.character(type),
                 "0" = {w <- matrix(rep(1/ncol(x[,-1]),(ncol(x[,-1])*length(rebdate))),length(rebdate),ncol(x[,-1])) %>% `colnames<-`(colnames(x[,-1])) %>% as.data.frame(); covmat <- NULL},
                 "1" = {w <- lapply(returnmat, function(y) {m <- apply(y,2,mean); m <- m+ifelse(min(m) >= 0,0,-min(m)+0.001); m/sum(m)}) %>% bind_rows() %>% as.data.frame(); covmat <- NULL},
                 "2" = {w <- lapply(invcov, function(y) {numer <- t(one)%*%y;  numer/sum(numer)}) %>% lapply(as.data.frame) %>% bind_rows()},
                 "3" = {w <- sapply(seq_along(invcov), function(i) {mu <- matrix(apply(returnmat[[i]] ,2,mean)); numer <- t(mu)%*%invcov[[i]];
                 numer/sum(numer)}) %>% t() %>% as.data.frame() %>% `colnames<-`(colnames(x)[-1])},
                 "4" = {w <- lapply(covmat, function(y) {ec <- eigen(y); as.matrix(apply(ec$vectors,2,function(n) n/sum(n)))%*%matrix(ec$values/sum(ec$values))}) %>%
                   lapply(function(m) as.data.frame(t(m))) %>% bind_rows() %>% `colnames<-`(colnames(x)[-1]) }
          )
          
          if(lim != 0){w <- apply(w,1,box_constrain, Short_Limit = lim) %>% t() %>% data.frame()}
          
          w <- cbind(rebdate,w) %>% as.data.frame() %>% `colnames<-`(c('Date',colnames(w)))
        }else{
          w <- filew
          w[,2:ncol(w)] <- round(w[,2:ncol(w)],6)
        }
        Weightset <- list(w,returnmat,covmat,rebdate)}else{
    Weightset <-  list(file3_Weight, multipywtoreturn(file3_Weight,file2_Historical),NULL,file3_Weight$Date)
    }
  file3_Weight <<- Weightset[[1]];
  file3_Weight[,2:ncol(file3_Weight)] <<- round(file3_Weight[,2:ncol(file3_Weight)],6);
  list(file3_Weight,Weightset[[2]],Weightset[[3]],Weightset[[4]])}})
  
  observeEvent(input$reb, {
    output$textnum <- renderText({ifelse(is.null(input$reb), "Select Rebalancing Frequency", paste0("Number of Rebalancing Iterations is now ", ceiling(nrow(file2_Historical)/as.integer(input$reb))))
      })
  })
  
  
  output$weight <- DTtable(NULL) #For render
  

  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$weight <- DTtable(get_weight()[[1]])
  }) #For update
  
  
  output$plotw3 <- renderPlotly(NULL)  #For render

  shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]],as.character(input$selcom)))
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])
    }) 
  observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot())) #For update plot

  output$plotw3F <- renderPlotly(NULL)  #For render
  output$plotw3Fshort <- renderPlotly(NULL)  #For render
  output$plotw3Flong <- renderPlotly(NULL)  #For render
  output$plotw3Ftab <- renderDT(NULL)  #For render
  
  shiny_plot_fundamental <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_fundamental(get_weight()[[1]],file1_Status)})
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$plotw3F <- renderPlotly(shiny_plot_fundamental()[[1]])
    output$plotw3Fshort <- renderPlotly(shiny_plot_fundamental()[[3]])
    output$plotw3Flong <- renderPlotly(shiny_plot_fundamental()[[2]])
    output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
    }) 

 
  output$plotw3port <- renderPlotly(NULL)  #For render
  
  shiny_plot_portfolio_weight <- eventReactive(list(input$subweight,input$file3), plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))
  observeEvent(list(input$subweight,input$file3), {
    output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
    updatePickerInput(session, "selcat", choices = colnames(file1_Status))
    updatePickerInput(session, "seldate", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))
    }) #For update choice
  observeEvent(input$selcat, output$plotw3pOort  <- renderPlotly(plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))) #For update plot
  observeEvent(input$seldate, output$plotw3port  <- renderPlotly(plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))) #For update plot
  

  risk <- eventReactive(list(input$subweight,input$file3),{
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    all_performance_table(file2_Historical, file3_Weight)})

  output$plotw3risk <- DTtable(NULL)  #For render
  
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$plotw3risk <- DTtable({tabr <- risk();
  tabr[,sapply(tabr, class) %in% c('numeric','integer')] <- round(tabr[,sapply(tabr, class) %in% c('numeric','integer')],6);
  tabr})}) #For update plot
  

  output$plotw3riskplot <- renderPlotly(NULL)  #For render
 
  shiny_plot_performance_table <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_performance_table(risk())})
  observeEvent(list(input$subweight,input$file3) , {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$plotw3riskplot <- renderPlotly(shiny_plot_performance_table())}) #For update plot
  
  output$radar <- renderPlotly(NULL)  #For render
  
  shiny_plot_radar <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_radar(risk())})
  
  observeEvent(list(input$subweight,input$file3) , {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$radar <- renderPlotly(plot_radar(risk()))}) #For update plot

  output$eff <- renderPlotly(NULL)  #For render
  
  shiny_efficient_fontier <- eventReactive(input$subeff, efficient_fontier(file2_Historical,file3_Weight))
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "seldateeff", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))}) #For update choice
  observeEvent(input$subeff , output$eff <- renderPlotly(plot_efficient_fontier(shiny_efficient_fontier()[[1]],file2_Historical,shiny_efficient_fontier()[[2]],input$seldateeff)))
  observeEvent(input$seldateff, output$eff  <- renderPlotly(plot_efficient_fontier(shiny_efficient_fontier()[[1]],file2_Historical,shiny_efficient_fontier()[[2]],input$seldateeff))) #For update plot
  

  observeEvent(input$file4, {
    File <- input$file4
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f4 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f4,c("Date","Bound"))
    
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file4",
        text = "Require at least on column to be numeric")
    }else if(cons[2]){
      showFeedbackWarning(
        inputId = "file4",
        text = "Missing the column of the names 'Date' or 'Bound'" )
    }else if(cons[3]){
      showFeedbackWarning(
        inputId = "file4",
        text = "Missing Data Detected" )
    }else{
      file4_Schedule  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$subdiv, {
    if((is.null(file1_Status))||(is.null(file2_Historical))||(is.null(file3_Weight))){
      noty(text ="Require Uploading Files in Step 2, 3 and 4 before", type = "warning")
    }else{
    file4_Schedule <<- div_schedule(as.integer(input$rate), c(as.numeric(input$m),as.numeric(input$a)),file3_Weight,as.Date(input$lastdate))}})

  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "lastdate", choices = as.character(unlist(get_weight()[[1]]$Date)[-1], use.names = FALSE))}) #For update choice
  
  output$plot4divtab <- DTtable(NULL)  #For render
  
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((!is.null(file3_Weight)),""))
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plot4divtab <- DTtable({file4_Schedule})}) #For update plot
  
  output$plot4divtabsum <- DTtable(NULL)  #For render
  
  
  output$plot4divpreview <- DTtable(NULL) 
  observeEvent(input$subdivpreview,{ 
    shiny::validate(need((!is.null(file3_Weight)),""))
    shiny::validate(need((input$subdivpreview >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plot4divpreview <- renderPlotly(div_preview(as.integer(input$rate), c(as.numeric(input$m),as.numeric(input$a)),as.Date(input$lastdate),file3_Weight, file1_Status))
  })
  
  shiny_div_weight <- eventReactive(list(input$subdiv,input$file4),{ 
                                    shiny::validate(need((!is.null(file3_Weight)),""))
                                    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
                                    updateProgressBar(id = 'pb2', value = 0, total = 100, title = "Processing")
                                      w <- file3_Weight
                                      Schedule <- file4_Schedule
                                      found <- file1_Status
                                      Invest_List <- found %>% dplyr::filter(Status == 'Invest') %>% .$Ticker
                                      
                                      Divest_List <- found %>% dplyr::filter(Status == 'Divest') %>% .$Ticker
                                      
                                      Bound <- unlist(Schedule$Bound,use.names =  FALSE)
                                      
                                      final_frame <- c()
                                      
                                      for(k in 1:nrow(w)){
                                        
                                        Invest <- w[,Invest_List][k,]
                                        Divest <- w[,Divest_List][k,]
                                        
                                        Long_Divest <- Divest[which(Divest >= 0)] 
                                        Short_Divest <- Divest[which(Divest < 0)]
                                        if(length(Short_Divest)==0){Short_Divest <- NULL}
                                        
                                        #Sum
                                        Long_Divest_Sum <- if (length(Long_Divest)==0) {0} else sum(Long_Divest)
                                        #Prevent Long only Port
                                        Short_Divest_Sum <- if (length(Short_Divest)==0) {0} else -sum(Short_Divest)
                                        
                                        if((Long_Divest_Sum > Bound[k]) || (Short_Divest_Sum > Bound[k])){
                                          if(Long_Divest_Sum > Bound[k]){
                                            Long_Divest_Scale <- (Long_Divest/Long_Divest_Sum)*Bound[k]
                                          }else{
                                            Long_Divest_Scale <- Long_Divest
                                          }
                                          if(Short_Divest_Sum  > Bound[k]){
                                            Short_Divest_Scale <- (Short_Divest/Short_Divest_Sum)*Bound[k]
                                          }else{
                                            Short_Divest_Scale <- Short_Divest
                                          }
                                          
                                          Excess_Long <- if (length(Long_Divest)==0) {0} else sum(Long_Divest) - sum(Long_Divest_Scale)
                                          Excess_Short <- if (length(Short_Divest)==0) {0} else -sum(Short_Divest) + sum(Short_Divest_Scale)
                                          New_Invest <- Invest + (Invest/sum(Invest))*(Excess_Long - Excess_Short)
                                          
                                          List_of_Weight <- list(New_Invest,
                                                                 Long_Divest_Scale, Short_Divest_Scale)
                                          
                                          Combine_Weight <- Filter(Negate(is.null), List_of_Weight) %>% 
                                            bind_cols()
                                          
                                          Weight_Divest <- Combine_Weight[,colnames(w)[-1]]
                                        }else{
                                          Weight_Divest <- w[k,-1]
                                        }
                                        
                                        final_frame <- rbind(final_frame, Weight_Divest)
                                        title <- ifelse(k == nrow(w),"Done","Calculating, please wait")
                                        updateProgressBar(id = 'pb2', value = k, total = nrow(w), title = title)
                                      }
                                      Date <- w$Date
                                      as.data.frame(cbind(Date,final_frame))
                                    })
  
  observeEvent(list(input$subdiv,input$file4),{
    shiny::validate(need((!is.null(file3_Weight)),""))
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plot4divtabsum <- DTtable({tab <- shiny_div_weight(); file5_Weight_Div <<- tab;
  tab[,2:ncol(tab)] <- round(tab[,2:ncol(tab)],6); tab})}) 
 
  output$plotw3InDiv1 <- renderPlotly(NULL)  
  output$plotw3InDiv2 <- renderPlotly(NULL)  

  shiny_plot_div_Sch <- eventReactive(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    plot_div_Sch(file3_Weight,file1_Status,file4_Schedule, file5_Weight_Div)})
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plotw3InDiv1 <- renderPlotly(shiny_plot_div_Sch()[[1]])}) 
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
     output$plotw3InDiv2 <- renderPlotly(shiny_plot_div_Sch()[[2]])}) 

  output$plotw3div <- renderPlotly(NULL) 

  shiny_asset_weight_plot_div <- eventReactive(input$selcomdiv, asset_weight_plot_div(file3_Weight,file5_Weight_Div,as.character(input$selcomdiv)))
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    updatePickerInput(session, "selcomdiv", choices = colnames(shiny_div_weight())[-1])}) 
  observeEvent(input$selcomdiv, output$plotw3div <- renderPlotly(shiny_asset_weight_plot_div())) 

  output$plotw3portdiv <- renderPlotly(NULL)  

  shiny_plot_portfolio_weight_div <- eventReactive(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    plot_portfolio_weight_div(file3_Weight,shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv))
    })
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    updatePickerInput(session, "selcatdiv", choices = colnames(file1_Status))
    updatePickerInput(session, "seldatediv", choices = as.character(unlist(shiny_div_weight()$Date), use.names = FALSE))}) 

  observeEvent(input$selcatdiv, output$plotw3portdiv  <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv)))) #For update plot
  observeEvent(input$seldatediv, output$plotw3portdiv  <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv)))) #For update plot
  
  
  riskdiv <- eventReactive(list(input$subdiv,input$file4),{
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    all_performance_table(file2_Historical, shiny_div_weight())})
 
  output$plotw3riskdiv <- DTtable(NULL)  
  

  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plotw3riskdiv <- DTtable({tabr <- riskdiv();
  tabr[,sapply(tabr, class) %in% c('numeric','integer')] <- round(tabr[,sapply(tabr, class) %in% c('numeric','integer')],6);
  tabr})}) 
  

  output$plotw3riskplotdiv <- renderPlotly(NULL) 

  shiny_plot_performance_table_div <- eventReactive(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    plot_performance_table_div(risk(),riskdiv())})
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plotw3riskplotdiv <- renderPlotly(shiny_plot_performance_table_div())}) 
  
  shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]],as.character(input$selcom)))
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])})
  observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot())) 
  output$plotw3Fdiv <- renderPlotly(NULL)  
  output$plotw3Fshortdiv <- renderPlotly(NULL)  
  output$plotw3Flongdiv <- renderPlotly(NULL)  
  output$plotw3Ftabdiv <- renderDT(NULL) 

  shiny_plot_fundamental_div <- eventReactive(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    plot_fundamental_div(file3_Weight, file5_Weight_Div, file1_Status)})
  observeEvent(list(input$subdiv,input$file4), 
               {shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
               output$plotw3Fdiv <- renderPlotly(shiny_plot_fundamental_div()[[1]])
               output$plotw3Fshortdiv <- renderPlotly(shiny_plot_fundamental_div()[[3]])
               output$plotw3Flongdiv <- renderPlotly(shiny_plot_fundamental_div()[[2]])
               output$plotw3Ftabdiv <- DTtable(shiny_plot_fundamental_div()[[4]])
               }) 

  output$radardiv <- renderPlotly(NULL)  #For render
  
  shiny_plot_radar_div <- eventReactive(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    plot_radar(risk(),riskdiv())})
  observeEvent(list(input$subdiv,input$file4), {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$radardiv <- renderPlotly(plot_radar_div(risk(),riskdiv()))}) #For update plot
  output$plotdiftable <- renderDT(NULL)  #For render
  output$plotdifboxplot <- renderPlotly(NULL)  #For render
  shiny_diff_summary <- eventReactive(input$sub.ly.st5, {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    diff_summary(risk(),riskdiv(),shiny_plot_fundamental()[[4]],shiny_plot_fundamental_div()[[4]],as.numeric(input$ly.st5))})  
  observeEvent(input$sub.ly.st5, {
    shiny::validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
    output$plotdiftable <- DTtable(cbind(shiny_diff_summary()[[1]][,1],round(shiny_diff_summary()[[1]][,-1],6)))
    output$plotdifboxplot <- renderPlotly(shiny_diff_summary()[[2]])}) #For update plot

  
  output$div_dynamic <- DTtable(NULL)
  output$hist_return  <- DTtable(NULL)
  output$found  <- DTtable(NULL)
  
  observeEvent(input$file6, {
    File <- input$file6
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f6,c("Date","PORTNAME"))
    
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Require at least on column to be numeric")
    }else if(cons[2]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Missing the column of the names 'PORTNAME' or 'Date" )
    }else if(cons[3]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Missing Data Detected" )
    }else{
      file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$div_dynamic <- DTtable(file6_Div_Dynamic)
    }
  }, ignoreNULL = FALSE)

  
  observeEvent(input$file7, {
    File <- input$file7
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f7,c("Ticker","Status"))
    
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file7",
        text = "Require at least on column to be numeric")
    }else if(cons[2]){
      showFeedbackWarning(
        inputId = "file7",
        text = "Missing the column of the names 'PORTNAME' or 'Date" )
    }else if(cons[3]){
      showFeedbackWarning(
        inputId = "file7",
        text = "Missing Data Detected" )
    }else{
      file7_Attribute  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$found <- DTtable(file7_Attribute)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$file8, {
    File <- input$file8
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )

    f8 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f8,c("Date"))
    
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file8",
        text = "Require at least on column to be numeric")
    }else if(cons[2]){
      showFeedbackWarning(
        inputId = "file8",
        text = "Missing the column of the name 'Date" )
    }else if(cons[3]){
      showFeedbackWarning(
        inputId = "file8",
        text = "Missing Data Detected" )
    }else{
      file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$hist_return  <- DTtable(round_tab(file8_Return))
    }
  }, ignoreNULL = FALSE)

output$plotcom1 <- renderPlotly(NULL)  #For render

observeEvent(input$subcomp, updatePickerInput(session, "selcomcom", choices = colnames(file6_Div_Dynamic)[-c(1,ncol(file6_Div_Dynamic))])) #For update choice
observeEvent(input$subcomp, output$plotcom1 <- renderPlotly(multicomp(file6_Div_Dynamic,as.character(input$selcomcom)))) #For update plot

output$plotcom2 <- renderPlotly(NULL)  #For render
output$plotcom3 <- renderPlotly(NULL)  #For render

shiny_plot_div_Sch_comp <- eventReactive(input$subcomp, plot_div_Sch_comp(file6_Div_Dynamic,file7_Attribute)) 
observeEvent(input$subcomp, output$plotcom2 <- renderPlotly(shiny_plot_div_Sch_comp()[[1]])) #For update plot
observeEvent(input$subcomp, output$plotcom3 <- renderPlotly(shiny_plot_div_Sch_comp()[[2]])) #For update plot

output$plotcom4 <- renderPlotly(NULL)  #For render
output$plotcom5 <- renderPlotly(NULL)  #For render
output$plotcom6 <- renderPlotly(NULL)  #For render
output$plotcom7 <- renderPlotly(NULL)  #For render
output$plotcom8 <- renderPlotly(NULL)  #For render
output$tab1 <- DTtable(NULL)
output$tab2 <- DTtable(NULL)

shiny_compareplot <- eventReactive(input$subcomp, compareplot(file8_Return, file6_Div_Dynamic,file7_Attribute)) 
observeEvent(input$subcomp, output$plotcom4 <- renderPlotly(shiny_compareplot()[[3]])) #For update plot
observeEvent(input$subcomp, output$plotcom5 <- renderPlotly(shiny_compareplot()[[4]]))
observeEvent(input$subcomp, output$plotcom6 <- renderPlotly(shiny_compareplot()[[5]]))
observeEvent(input$subcomp, output$plotcom7 <- renderPlotly(shiny_compareplot()[[6]]))
observeEvent(input$subcomp, output$plotcom8 <- renderPlotly(shiny_compareplot()[[7]]))
observeEvent(input$subcomp, output$tab1 <- DTtable(round_tab(shiny_compareplot()[[1]])))
observeEvent(input$subcomp, output$tab2 <- DTtable(round_tab(shiny_compareplot()[[2]])))

observeEvent(input$subcomp, updatePickerInput(session, inputId = 'selben.op1', choices = unique(file6_Div_Dynamic$PORTNAME)))
shiny_diff_summary_mul <- eventReactive(input$submul, {diff_summary_mul(shiny_compareplot()[[1]],shiny_compareplot()[[2]],as.character(input$selben.op1), as.numeric(input$ly.opt1)) })
observeEvent(input$submul, output$tabben.op1 <- DTtable(round_tab(shiny_diff_summary_mul()[[1]]))) 
observeEvent(input$submul, output$plotben.op1 <- renderPlotly(shiny_diff_summary_mul()[[2]])) 


observeEvent(input$file6.op2, {
  File <- input$file6.op2
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  
  
  f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f6,c("Date","PORTNAME"))
  
  if(cons[1]){
    showFeedbackWarning(
      inputId = "file6.op2",
      text = "Require at least on column to be numeric")
  }else if(cons[2]){
    showFeedbackWarning(
      inputId = "file6.op2",
      text = "Missing the columns of the name 'Date' or 'PORTNAME'" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file6.op2",
      text = "Missing Data Detected" )
  }else{
    file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$tab.op2 <- DTtable(round_tab(file6_Div_Dynamic))
  }

}, ignoreNULL = FALSE)



observeEvent(input$file8.op2, {
  File <- input$file8.op2
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f6,c("Date"))
  
  if(cons[1]){
    showFeedbackWarning(
      inputId = "file8.op2",
      text = "Require at least on column to be numeric")
  }else if(cons[2]){
    showFeedbackWarning(
      inputId = "file8.op2",
      text = "Missing the column of the name 'Date" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file8.op2",
      text = "Missing Data Detected" )
  }else{
    file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$tab.op2.2 <- DTtable(round_tab(file8_Return))
  }

}, ignoreNULL = FALSE)

shiny_clust.option <-  eventReactive(input$subclust, clust.option(file6_Div_Dynamic, file8_Return, as.numeric(input$tau.op2), as.numeric(input$sept.op2)))

output$op2graph <- renderPlotly(NULL)
output$op2hm <- renderPlotly(NULL)
observeEvent(input$subclust, {
  output$op2graph <- renderPlotly(shiny_clust.option()[[3]])
  output$op2hm <- renderPlotly(shiny_clust.option()[[4]])
  output$op2tab1 <- renderDT(round_tab(shiny_clust.option()[[1]]))
  output$op2tab2 <- renderDT(round_tab(shiny_clust.option()[[2]]))
}) 



observeEvent(input$file6.op3, {
  File <- input$file6.op3
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  
  f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f6,c("Date","PORTNAME"))
  
  if(cons[1]){
    showFeedbackWarning(
      inputId = "file6.op3",
      text = "Require at least on column to be numeric")
  }else if(cons[2]){
    showFeedbackWarning(
      inputId = "file6.op3",
      text = "Missing the columns of the name 'Date' or 'PORTNAME'" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file6.op3",
      text = "Missing Data Detected" )
  }else{
    file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$tab.op3 <- DTtable(round_tab(file6_Div_Dynamic))
  }
  
  updatePickerInput(session,inputId = "gdate1", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
  updatePickerInput(session,inputId = "gdate2", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
  updatePickerInput(session,inputId = "gdate3", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
}, ignoreNULL = FALSE)


observeEvent(input$file7.op3, {
  File <- input$file7.op3
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  
  f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f7,c("Ticker","Status"))
  
  if(cons[2]){
    showFeedbackWarning(
      inputId = "file7.op3",
      text = "Missing the columns of the name 'Ticker' or 'Status'" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file7.op3",
      text = "Missing Data Detected" )
  }else{
    file7_Attribute  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$tab.op3.1 <- DTtable(round_tab(file7_Attribute))
  }

}, ignoreNULL = FALSE)

observeEvent(input$file8.op3, {
  File <- input$file8.op3
  shiny::validate(
    need(File != "", "No data has been uploaded")
  )
  f8 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
  cons <- req.cons.file(f8,c("Date"))
  
  if(cons[1]){
    showFeedbackWarning(
      inputId = "file8.op3",
      text = "Require at least on column to be numeric")
  }else if(cons[2]){
    showFeedbackWarning(
      inputId = "file8.op3",
      text = "Missing the columns of the name 'Date'" )
  }else if(cons[3]){
    showFeedbackWarning(
      inputId = "file8.op3",
      text = "Missing Data Detected" )
  }else{
    file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    output$tab.op3.2 <- DTtable(round_tab(file8_Return))
  }
}, ignoreNULL = FALSE)

shiny_get.graph.1 <-  eventReactive(input$subgdate1, {
    updateProgressBar(id = 'pbg1', value = 0, total = 100, title = "Processing")
    div_dynamic <- file6_Div_Dynamic
    return_mat <- file8_Return 
    data.index <- as.integer(input$gdate1)
    found <- file7_Attribute
    nameport <- unique(div_dynamic$PORTNAME)
    div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME,levels = unique(nameport))
    w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
    names(w_list) <- nameport
    sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
    list.cov <- sel.port %>% lapply(sturcture.cov)
    par(mfrow=c(length(nameport),1))
    for(i in seq_along(list.cov)){
      plot.qgraph(list.cov[[i]],names(sel.port)[i],found)
      title <- ifelse(i == length(list.cov),"Done","Calculating, please wait")
      updateProgressBar(id = 'pbg1', value = i, total = length(list.cov), title = title)
    }
    })

shiny_get.graph.2 <-  eventReactive(input$subgdate2, {
  updateProgressBar(id = 'pbg2', value = 0, total = 100, title = "Processing")
  div_dynamic <- file6_Div_Dynamic
  return_mat <- file8_Return 
  data.index <- as.integer(input$gdate2)
  found <- file7_Attribute
  nameport <- unique(div_dynamic$PORTNAME)
  div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME,levels = unique(nameport))
  w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
  names(w_list) <- nameport
  sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
  list.cov <- sel.port %>% lapply(sturcture.cov)
  par(mfrow=c(length(nameport),1))
  for(i in seq_along(list.cov)){
    plot.qgraph(list.cov[[i]],names(sel.port)[i],found)
    title <- ifelse(i == length(list.cov),"Done","Calculating, please wait")
    updateProgressBar(id = 'pbg2', value = i, total = length(list.cov), title = title)
  }
})


shiny_get.graph.3 <-  eventReactive(input$subgdate3, {
  updateProgressBar(id = 'pbg3', value = 0, total = 100, title = "Processing")
  div_dynamic <- file6_Div_Dynamic
  return_mat <- file8_Return 
  data.index <- as.integer(input$gdate3)
  found <- file7_Attribute
  nameport <- unique(div_dynamic$PORTNAME)
  div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME,levels = unique(nameport))
  w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
  names(w_list) <- nameport
  sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
  list.cov <- sel.port %>% lapply(sturcture.cov)
  par(mfrow=c(length(nameport),1))
  for(i in seq_along(list.cov)){
    plot.qgraph(list.cov[[i]],names(sel.port)[i],found)
    title <- ifelse(i == length(list.cov),"Done","Calculating, please wait")
    updateProgressBar(id = 'pbg3', value = i, total = length(list.cov), title = title)
  }
})

observeEvent(input$subgdate1, output$op3.1 <- renderPlot(shiny_get.graph.1())) #For update plot
observeEvent(input$subgdate2, output$op3.2 <- renderPlot(shiny_get.graph.2())) #For update plot
observeEvent(input$subgdate3, output$op3.3 <- renderPlot(shiny_get.graph.3())) #For update plot


shiny_index_map <- eventReactive(input$selindex, map_index(input$selindex))
observeEvent(input$selindex, output$map <- renderPlot(shiny_index_map()[[1]]))

observeEvent(input$subindex, {
  updateOrderInput(session,inputId = "source", items = (shiny_index_map()[[2]]), item_class = 'info')
})

}

shinyApp(ui, server, options = list(launch.browser = T))
