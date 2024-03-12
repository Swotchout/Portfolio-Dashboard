library(shiny)
library(shinythemes)
library(quantmod)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(leaflet)

shinyUI(navbarPage(title = div(
  style = "height: 100%;",
  div(
    style = "float: left; padding-right: 15px; margin-top: -20px;",
    img(src = "Logofinvest.jpg", height = 55)
  )
),
collapsible = TRUE,

theme = shinythemes::shinytheme("cerulean"),
footer = div(
  style = "position: fixed; bottom: 0; right: 0; margin-bottom: 15px; margin-right: 15px;",
  img(src = "Fin.jpg", height = 150),
),

tabsetPanel(id = "navbar",
            tabPanel("Übersicht",
                     div(style = "margin-top: 10px;"),
                     div(style = "position: relative; height: 100%; display: flex; justify-content: center; align-items: center;",
                         img(src = "Start.png", height = 400)
                     ),
                     hr(),
                     # Buttons am unteren Rand der Seite platzieren
                     fluidRow(style = "position: absolute; bottom: -10; width: 100%;",
                              column(width = 2),
                              column(width = 4, align = "center",
                                     actionButton("easy_invest_button", "Easy-Invest", class = "btn-primary", style = "width: 100%; height: 50px; font-size: 20px;"),
                                     div(style = "margin-top: 30px;"),
                                     p("Ideal für Einsteiger, bietet Easy-Invest einen simplen Ansatz zur Geldanlage. Geben Sie Ihre Risikobereitschaft, Diversifikationsziele und Volatilitätspräferenzen an, um eine massgeschneiderte Portfolioempfehlung zu erhalten. Eine klare Anlagestrategie ohne Komplexität.", style = "text-align:center;")
                              ),
                              column(width = 3),
                              column(width = 4, align = "center",
                                     actionButton("pro_invest_button", "Pro-Invest", class = "btn-primary", style = "width: 100%; height: 50px; font-size: 20px;"),
                                     div(style = "margin-top: 30px;"),
                                     p("Pro-Invest richtet sich an erfahrene Anleger und ermöglicht die Auswahl und Anpassung von Rendite- und Risikomodellen. Erhalten Sie detaillierte Analysen und Einblicke in die Performance Ihres Portfolios und treffen Sie fundierte Anlageentscheidungen mit dieser flexiblen Lösung.", style = "text-align:center;")
                              ),
                              column(width = 2)
                     )
            

            ),


         
                           tabPanel("Easy-Invest",
                                    sidebarLayout(
                                      sidebarPanel(id = "Easy",
                                                   width = 5,
                                                   h4(strong("Bitte beantworten Sie die folgenden Fragen:")),
                                                   hr(),
                                                   
                                                   h4("Risikobereitschaft:", style = "color: black; font-weight: bold;"),
                                                   # Frage 1
                                                   selectInput("frage1", "Ich bin bereit, in meinem Portfolio höhere Risiken einzugehen, um potenziell höhere Renditen zu erzielen.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   # Frage 2
                                                   selectInput("frage2", "Schwankungen in der Wertentwicklung meines Portfolios bereiten mir Sorge.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 4, "Stimme eher nicht zu" = 3, "Neutral" = 2, "Stimme eher zu" = 1, "Stimme voll und ganz zu" = 0)),
                                                   # Frage 3
                                                   selectInput("frage3", "Ich habe vor, mein Portfolio langfristig zu halten.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   # Frage 4
                                                   selectInput("frage4", "Verluste in meinem Portfolio würden mich sehr verärgern.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 4, "Stimme eher nicht zu" = 3, "Neutral" = 2, "Stimme eher zu" = 1, "Stimme voll und ganz zu" = 0)),
                                                   
                                                   h4("Diversifikation:", style = "color: black; font-weight: bold"),
                                                   # Frage 5
                                                   selectInput("frage5", "Ich halte es für wichtig, mein Portfolio breit zu diversifizieren, um potenzielle Risiken zu minimieren.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   # Frage 8
                                                   selectInput("frage6", "Ich bevorzuge ein Portfolio mit einer grossen Anzahl von Aktien, um das Risiko besser einschätzen zu können.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   
                                                   h4("Nachhaltigkeit (ESG):", style = "color: black; font-weight: bold"),
                                                   # Frage 9
                                                   selectInput("frage7", "Ich bevorzuge eine nachhaltige Anlagestrategie.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   # Frage 10
                                                   selectInput("frage8", "Geschäftspraktiken wie Tabak, Waffen oder fossile Brennstoffe möchte ich aus meinen Investitionen auszuschliessen.",
                                                               choices = c("Bitte auswählen" = "", "Stimme überhaupt nicht zu" = 0, "Stimme eher nicht zu" = 1, "Neutral" = 2, "Stimme eher zu" = 3, "Stimme voll und ganz zu" = 4)),
                                                   
                                                   br(),
                                                   actionButton("submit", "Auswertung"),
                                                   
                                                   
                                      ),
                                      
                                      # Ausgabe des Ergebnisses
                                      column(width = 7,
                                             mainPanel(
                                               tabsetPanel(
                                                 tabPanel("Prognose",
                                                          hr(),
                                                          plotlyOutput("RISK_gauge", height = 300, width = 300),
                                                          plotlyOutput("DIVERS_gauge", height = 300, width = 300),
                                                          plotlyOutput("VOL_gauge", height = 300, width = 300),
                                                          
                                                          
                                                 ),
                                                 tabPanel("Portfolioempfehlung",
                                                          hr(),
                                                          actionButton("tics", "Portfolio Empfehlung berechnen"),
                                                          hr(),
                                                          verbatimTextOutput("print_output"),
                                                          hr(),
                                                          plotlyOutput("ef"),
                                                          div(style = "margin-top: 30px;"),
                                                          hr(),
                                                          plotlyOutput("plotPie")
                                                 ), 
                                                 tabPanel("Performance-Analyse",
                                                          dateRangeInput("time_range",
                                                                         "Zeitraum:",
                                                                         start = "2014-01-01",
                                                                         end = Sys.Date(),
                                                                         min = "2014-01-01",
                                                                         max = Sys.Date(),
                                                                         format = "yyyy-mm-dd",
                                                                         separator = " bis "),
                                                          width = "100%",
                                                          HTML("Info: Klicken Sie erneut auf Auswertung, um den Zeitraum zu aktualisieren."),
                                                          hr(),
                                                          plotlyOutput("bt"),
                                                          div(style = "margin-top: 120px;"),
                                                          div(style = "text-align: center;",
                                                              tableOutput("portfolio_summary"))
                                                          

                                                 
                                                 )
                                               )
                                             )
                                             
                                      )
                                    )
                           ),
                           
                   
            tabPanel("Pro-Invest",
                     sidebarLayout(
                       sidebarPanel(id = 'Pro',
                                    radioButtons("returnModel", "Rendite-Modell:",
                                                 c("Mean Historical"="MH", 
                                                   "Exponentially Weighted Mean Historical"="EWMH", 
                                                   "Capital Asset Pricing Model (CAPM)"="CAPM")),
                                    hr(),
                                    radioButtons("riskModel", "Risiko-Modell:",
                                                 c("Sample Covariance"="Cov", 
                                                   "Exponentially Weighted Covariance"="ExpCov")),
                                    HTML('<h2><br></h2>'),
                                    dateRangeInput("date_range", "Zeitraum:", start = "2014-01-01", end = Sys.Date(), min = "2014-01-01", max = Sys.Date(),separator = " bis "),
                                    checkboxInput("hasMarket", "Market-Index anwenden", FALSE),
                                    fluidRow(
                                      column(6, textInput("market", "Market index:", 
                                                          placeholder = "z.B. S&P 500")),
                                      column(6, textInput("rf", "Risikofreier-Zins (%):", 
                                                          placeholder = "enter without %"))),
                                    textInput("additional_tickers", "Weitere Ticker hinzufügen (getrennt durch Kommas):", 
                                              placeholder = "z.B. AAPL,GOOG,AMZN"),
                                    hr(),
                                    actionButton("go", "Fertig", class = "btn-primary"),
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel('Returns', plotOutput('plotReturn'),
                                    hr(),
                                    fluidRow(tags$h5("Legende Basiwerte")),
                                    fluidRow(tags$p("'^SSMI' = SMI")),
                                    fluidRow(tags$p("'GC=F' = Gold")),
                                    fluidRow(tags$p("'BTC-USD' = Bitcoin")),
                                    fluidRow(tags$p("'^GSPC' = S&P500")),
                                    fluidRow(tags$p("'CSBGC0.SW' = Swiss Bonds")),
                                    fluidRow(tags$p("'^TNX' = US Bonds"))
                                    
                                    ),
                           tabPanel('Risk Matrix', plotOutput('plotRisk'))
                         ))
                       
                     )
            ),
            
            
                   
 

                   tabPanel("Pro-Performance",
                            sidebarLayout(
                              sidebarPanel(id = 'sidebar',
                                           uiOutput("assetCheckBox")
                                           
                              ),
                              mainPanel(
                                tabsetPanel(
                                  
                                  tabPanel('Daily Prices', 
                                           plotOutput("plots_P",
                                                      click = "plot_click_P",
                                                      dblclick = "plot_dblclick_P",
                                                      hover = "plot_hover_P",
                                                      brush = "plot_brush_P"), 
                                           h1(''),
                                           verbatimTextOutput("plotInfo_P")),
                                  tabPanel('Daily Returns', 
                                           plotOutput("plots_R_daily",
                                                      click = "plot_click_R_daily",
                                                      dblclick = "plot_dblclick_R_daily",
                                                      hover = "plot_hover_R_daily",
                                                      brush = "plot_brush_R_daily"), 
                                           h1(''),
                                           verbatimTextOutput("plotInfo_R_daily")),
                                  tabPanel('Returns Histogram', 
                                           plotOutput("plots_R_hist",
                                                      click = "plot_click_R_hist",
                                                      dblclick = "plot_dblclick_R_hist",
                                                      hover = "plot_hover_R_hist",
                                                      brush = "plot_brush_R_hist"),
                                           h1(''),
                                           verbatimTextOutput("plotInfo_R_hist")),
                                  fluidRow(tags$h5("Legende Basiwerte")),
                                  fluidRow(tags$p("'^SSMI' = SMI")),
                                  fluidRow(tags$p("'GC=F' = Gold")),
                                  fluidRow(tags$p("'BTC-USD' = Bitcoin")),
                                  fluidRow(tags$p("'^GSPC' = S&P500")),
                                  fluidRow(tags$p("'CSBGC0.SW' = Swiss Bonds")),
                                  fluidRow(tags$p("'^TNX' = US Bonds"))
        
                                  
                                  
                                )
                              )
                            )
                   ),
                   
                   tabPanel("Pro-Analyitics",
                            sidebarLayout(
                              sidebarPanel(id = 'sidebar',
                                           radioButtons("FrontierChoice", "Wähle Effizienzgrenze:", 
                                                        c("Efficient"="eff",
                                                          "Minimum Variance"="both")),
                                           hr(),
                                           h5('Wählen Sie ihre persönliche Gewichtung der Anlageklassen.'),
                                           uiOutput('userWeights'),
                                           HTML('<h2><br></h2>'),
                                           actionButton('portfolioGO','Zeige mein Portfolio',class='btn-primary')
                              ),
                              mainPanel(
                                plotOutput('efficientFrontier',
                                           click = "plot_click_EF",
                                           dblclick = "plot_dblclick_EF",
                                           hover = "plot_hover_EF",
                                           brush = "plot_brush_EF"),
                                h1(''),
                                verbatimTextOutput("plotInfo_EF"),
                                h1(''),
                                verbatimTextOutput('userPortfolio'),
                                fluidRow(tags$h5("Legende Basiwerte")),
                                fluidRow(tags$p("'^SSMI' = SMI")),
                                fluidRow(tags$p("'GC=F' = Gold")),
                                fluidRow(tags$p("'BTC-USD' = Bitcoin")),
                                fluidRow(tags$p("'^GSPC' = S&P500")),
                                fluidRow(tags$p("'CSBGC0.SW' = Swiss Bonds")),
                                fluidRow(tags$p("'^TNX' = US Bonds"))
                              )
                            )
                   ),
                   tabPanel("Anleitung",
                            navlistPanel('Anleitung',
                                         
                                           
                                         tabPanel("Easy-Invest > Fragebogen",
                                                  fluidRow(
                                                    column(
                                                      width = 6,
                                                      h4("Fragebogen:"),
                                                      p("Auf dieser Seite können Sie Ihre Anlagepräferenzen auswerten. Diese Werte werden durch eine sorgfältige Auswertung des von Ihnen ausgefüllten Fragebogens ermittelt."),
                                                      h4("Risikobereitschaft:"),
                                                      p("Die Risikobereitschaft im Investmentbereich bezieht sich auf Ihre persönliche Bereitschaft und Fähigkeit, Risiken bei Ihren Investitionen einzugehen. Sie wird durch unterschiedliche Faktoren bestimmt, wie Ihre finanzielle Stabilität, Ihre Anlageziele, Ihren Zeithorizont und Ihre individuelle Einstellung zum Risiko. Eine hohe Risikobereitschaft kann potenziell zu höheren Renditen führen, birgt jedoch gleichzeitig das Risiko grösserer Verluste. Es ist von essenzieller Bedeutung, Ihre persönliche Risikotoleranz zu verstehen und eine Anlagestrategie auszuwählen, die Ihren persönlichen Zielen und Ihrer Komfortzone hinsichtlich Risiken entspricht."),
                                                      h4("Diversifikation:"),
                                                      p("Die Diversifikation im Investmentbereich bezieht sich auf die Streuung Ihres Kapitals über verschiedene Anlageklassen, um das Risiko zu minimieren. Durch die Investition in unterschiedliche Wertpapiere, Branchen oder Märkte können Sie das Verlustrisiko reduzieren. Das Ziel der Diversifikation ist es, potenzielle Gewinne zu maximieren, während gleichzeitig das Verlustrisiko minimiert wird. Eine gut diversifizierte Anlagestrategie kann langfristig zu einer stabilen Rendite führen."),
                                                      h4("Nachhaltigkeit (ESG):"),
                                                      p("Nachhaltigkeit im Investment bezieht sich auf die Integration von Umwelt-, Sozial- und Governance-Faktoren (ESG) in Anlageentscheidungen. Es geht darum, langfristig rentable Investitionen zu tätigen, die positive Auswirkungen auf die Umwelt, die Gesellschaft und das Unternehmensmanagement haben. ESG-Kriterien dienen dazu, Risiken zu minimieren, Chancen zu identifizieren und einen nachhaltigen Wandel in der Wirtschaft zu fördern.")
                                                    ),
                                                    column(
                                                      width = 6,
                                                      img(src = "EasyInvest_Prognose.png", width = "70%", height = "auto")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Easy-Invest > Portfolioempfehlung",
                                                  h4("Portfolioempfehlung:"),
                                                  img(src = "EasyInvest_Portfolio.png", width = "50%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Auf dieser Seite wird Ihnen eine individuelle Portfolioempfehlung präsentiert, die anhand der Auswertung des vorangegangenen Fragebogens erstellt wurde. Diese Empfehlung illustriert die erwartete Rendite in Abhängigkeit vom eingegangenen Risiko. Verschiedene Portfolios mit unterschiedlichen Gewichtungen werden durch blaue Punkte repräsentiert. Die grauen Punkte kennzeichnen fünf speziell entwickelte Portfolios, die im Rahmen des Easy Invest Angebots zur Verfügung stehen. Ihr massgeschneidertes Portfolio ist durch den roten Punkt markiert."),
                                                  hr(),
                                                  h4("Empfohlene Gewichtung:"),
                                                  img(src = "EasyInvest_Gewichte.png", width = "50%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Das Kreisdiagramm 'Empfohlene Gewichtung' präsentiert Ihnen eine visuelle Darstellung Ihrer idealen Portfolio-Zusammensetzung, basierend auf den Antworten aus dem Fragebogen. Jeder Sektor in diesem Diagramm steht für eine spezifische Anlageklasse: Gold, Swiss Bond, S&P 500, US-Bond, Bitcoin und SMI. Die Grösse jedes Sektors symbolisiert den empfohlenen Anteil dieser Anlageklasse in Ihrem Portfolio. Ein grösserer Sektor für Gold etwa bedeutet eine stärkere Gewichtung dieses Edelmetalls in Ihrem Portfolio, während ein kleinerer Sektor für den S&P 500 eine geringere Gewichtung dieser Anlageklasse ausweist"),
                                                  div(style = "margin-top: 150px;"),
                                         ),
                                         tabPanel("Easy-Invest > Performance-Analyse",
                                                  h4("Performance Analyse:"),
                                                  img(src = "EasyInvest_PerformanceAnalyse.png", width = "50%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Auf dieser Seite wird Ihnen ein eingehender Vergleich der historischen Performance der fünf verschiedenen Portfolios angeboten, die im Rahmen des Easy Invest Programms verfügbar sind. Den Zeitraum, für welchen diese historische Performance dargestellt wird, kann oberhalb der Grafik definiert werden. Klicken Sie bitte anschliessend erneut auf den Button 'Auswertung' um den aktualisierten Zeitraum anzuzeigen. Der prozentuale Portfoliowert startet bei 100% und entwickelt sich dannach gemäss den historischen Kursbewegungen der verschiedenen Assets."),
                                                  hr(),
                                                  img(src = "EasyInvest_PerformanceLegende.png", width = "40%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Für den ausgewählten Zeitraum werden Ihnen die Renditen und die Volatilitäten der Portfolios angezeigt. So können Sie die Renditen der verschiedenen Portfolios über verschiedene Zeiträume vergleichen. Diese Analyse hilft Ihnen dabei, die Stärken und Schwächen jedes Portfolios zu erkennen und zu verstehen, wie die Portfolios im Laufe der Zeit performt haben. Mit diesen Informationen sind Sie besser gerüstet, eine fundierte Entscheidung zu treffen und das Portfolio zu wählen, das am besten zu Ihren persönlichen finanziellen Zielen und Ihrer Risikotoleranz passt. Damit können Sie sicherstellen, dass Ihre Anlagen in Einklang mit Ihren finanziellen Zielen und Erwartungen sind."),
                                                  div(style = "margin-top: 200px;")
                                         ),
                                         tabPanel("Pro-Invest",
                                                  div(style = "margin-top: 20px;"),
                                                  img(src = "ProInvest_Wahltool.png", width = "35%", height = "auto"),
                                                  h4("Mean Historical:"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Das 'Mean Historical' Renditemodell basiert auf historischen Durchschnittsrenditen. Es verwendet vergangene Renditedaten, um einen Schätzwert für die zukünftige Rendite eines Vermögenswerts oder einer Anlageklasse zu ermitteln. Es geht davon aus, dass die zukünftigen Renditen dem historischen Durchschnitt entsprechen werden."),
                                                  hr(),
                                                  h4("Exponentially Weighted Mean Historical:"),
                                                  p("Das 'Exponentially Weighted Mean Historical' Renditemodell ist eine Methode, bei der historische Renditen unter Berücksichtigung ihrer zeitlichen Gewichtung analysiert werden. Es verwendet einen exponentiellen Abnahmefaktor, um jüngeren Renditen ein höheres Gewicht zu geben als älteren. Dies ermöglicht eine stärkere Berücksichtigung aktueller Trends und Veränderungen bei der Schätzung der zukünftigen Rendite."),
                                                  hr(),
                                                  h4("Capital Asset Pricing Model (CAPM):"),
                                                  p("Das Capital Asset Pricing Model (CAPM) ist ein Finanzmodell, das versucht, die erwartete Rendite eines Vermögenswerts basierend auf seinem systematischen Risiko zu erklären. Es verwendet den risikofreien Zinssatz, die Marktrisikoprämie und die Beta-Koeffizienten des Vermögenswerts, um eine angemessene Renditeerwartung abzuleiten. Das CAPM dient als grundlegender Rahmen für die Bestimmung der Renditeanforderungen von Investoren und für die Bewertung von Vermögenswerten im Verhältnis zu ihrem Risiko."),
                                                  hr(),
                                                  h4("Returns:"),
                                                  img(src = "ProInvest_Returns.png", width = "50%", height = "auto"),
                                                  p("In dieser Grafik werden die verschiedenen Renditen als Balkendiagramm dargestellt."),
                                                  hr(),
                                                  h4("Risk Matrix:"),
                                                  img(src = "ProInvest_RiskMatrix.png", width = "50%", height = "auto"),
                                                  p("Das Konzept des Risikos wird anhand der Schwankungsbreite der Renditen eines Vermögenswerts definiert. Mit anderen Worten wird die Standardabweichung der Renditeverteilung verwendet. Um das Risiko zu reduzieren, wird empfohlen, das Anlageportfolio durch die Auswahl von Vermögenswerten, die nur schwach korreliert sind, zu diversifizieren. Idealweise haben diese Vermögenswerte eine Korrelation nahe -1. Das Risiko kann mit verschiedenen Modellen gemessen werden:"),
                                                  p("1. Sample Covariance: Hierbei wird die Kovarianzmatrix mithilfe herkömmlicher Methoden berechnet."),
                                                  p("2. Exponentially Weighted Covariance: Dieses Modell berechnet eine gewichtete Kovarianzmatrix, wobei jüngeren Daten eine höhere Bedeutung zukommt."),
                                                  div(style = "margin-top: 200px;")
                                         ),
                                         tabPanel("Pro-Performance",
                                                  h4("Daily Prices:"),
                                                  img(src = "ProPerformance_DailyPrices.png", width = "80%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Die tägliche Preisgrafik für den Golfpreis zeigt die historischen Preisbewegungen des Golfs über einen bestimmten Zeitraum. Die Grafik wird normalerweise mit dem Datum auf der horizontalen Achse und dem Preis auf der vertikalen Achse dargestellt. Jeder Datenpunkt repräsentiert den Schlusskurs des Golfs an einem bestimmten Tag. Durch die Analyse dieser Grafik können Investoren und Händler Trends, Muster und Volatilität im Golfpreis erkennen, um fundierte Entscheidungen zu treffen."),
                                                  hr(),
                                                  h4("Daily Returns:"),
                                                  img(src = "ProPerformance_DailyReturns.png", width = "80%", height = "auto"),
                                                  p("Eine tägliche Renditegrafik zeigt die prozentualen Veränderungen des Preises eines Wertpapiers oder Vermögenswerts über einen bestimmten Zeitraum. Die Grafik wird in der Regel mit dem Datum auf der horizontalen Achse und den Renditeprozentwerten auf der vertikalen Achse dargestellt. Jeder Datenpunkt repräsentiert die prozentuale Veränderung des Wertes des Wertpapiers oder Vermögenswerts im Vergleich zum vorherigen Tag. Durch die Analyse dieser Grafik können Investoren und Händler die täglichen Schwankungen, Trends und Volatilität eines Wertpapiers oder Vermögenswerts besser verstehen und daraus Schlüsse ziehen, um Anlageentscheidungen zu treffen."),
                                                  hr(),
                                                  h4("Returns Histogram:"),
                                                  img(src = "ProPerformance_ReturnsHistogram.png", width = "80%", height = "auto"),
                                                  p("Eine tägliche Preisgrafik mit prozentualer Häufigkeit stellt die Verteilung der prozentualen Preisveränderungen eines bestimmten Vermögenswerts über einen festgelegten Zeitraum dar. Die Grafik ermöglicht es Anlegern, die Verteilung der Preisbewegungen zu visualisieren und Muster wie z.B. Normalverteilungen, Schiefe oder Ausreisser zu erkennen. Dies kann dazu beitragen, Einblicke in die Volatilität und das Risiko des Vermögenswerts zu gewinnen und Entscheidungen zu treffen."),
                                                  div(style = "margin-top: 200px;")
                                         ),
                                         tabPanel("Pro-Analytics",
                                                  h4("Effizienzgrenze & persönliches Portfolio:"),
                                                  img(src = "ProAnalytics_Wahl.png", width = "40%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Die obere Linie in der Grafik stellt die Effizienzgrenze oder auch effiziente Frontier dar. Diese Linie zeigt die Kombinationen von Portfolios, die das bestmögliche Verhältnis von Rendite zu Risiko bieten. Jeder Punkt entlang der Linie repräsentiert ein Portfolio mit einer bestimmten Risiko-Rendite-Kombination."),
                                                  p("Die Minimum-Varianz-Grenze kann ein- oder ausgeblendet werden und zeigt den Bereich der Portfolios mit der geringsten Varianz oder Standardabweichung der Renditen. Dieser Bereich kennzeichnet die Portfolios mit dem niedrigsten Risiko für ein gegebenes Renditeniveau. Durch die Optimierung der Gewichtungen verschiedener Vermögenswerte innerhalb dieser Grenze kann ein Anleger das Portfolio mit dem geringsten Risiko erreichen."),
                                                  div(style = "margin-top: 20px;"),
                                                  img(src = "ProAnalytics_Graphik.png", width = "80%", height = "auto"),
                                                  div(style = "margin-top: 20px;"),
                                                  p("Die runden, blauen Punkte in der Grafik repräsentieren Portfolios, bei denen nur ein einzelnes der Produkte ausgewählt wurde. Der violette Rhombus hingegen stellt ein Portfolio dar, das aus verschiedenen Produkten mit unterschiedlichen Mengen zusammengestellt wurde. Hier werden verschiedene Produkte in bestimmten Anteilen kombiniert, um ein ausgewogenes Portfolio zu schaffen."),
                                                  div(style = "margin-top: 200px;")
                                         )
                            )
                   ),

            tabPanel("Über uns",
                     mainPanel(
                       fluidRow(
                         # Team section
                         column(width = 12,
                                h2("Team", style = "font-size: 28px; text-align: center; margin-top: 20px; margin-bottom: 40px;"),
                                div(
                                  class = "row",
                                  style = "margin-bottom: 60px;",
                                  
                                  # Team member 1 - Jan Pfyffer
                                  column(width = 3,
                                         style = "text-align: center;",
                                         tags$img(src = "jan_pfyffer.jpg", height = "100px", class = "img-thumbnail", alt = "Jan Pfyffer"),
                                         h4("Jan Pfyffer", style = "font-weight: bold;"),
                                         p("Tel. 078 600 600"),
                                         p("jan.pfyffer@finvestag.ch"),
                                         p(tags$a(href = "https://www.linkedin.com/in/jan-pfyffer-94b24b14b/", "LinkedIn", target = "_blank"))
                                  ),
                                  
                                  # Team member 2 - Colin Weuste
                                  column(width = 3,
                                         style = "text-align: center;",
                                         tags$img(src = "colin_weuste.jpg", height = "100px", class = "img-thumbnail", alt = "Colin Weuste"),
                                         h4("Colin Weuste", style = "font-weight: bold;"),
                                         p("Tel. 078 600 600"),
                                         p("colin.Weuste@finvestag.ch"),
                                         p(tags$a(href = "https://www.linkedin.com/in/colin-weuste-500819226/", "LinkedIn", target = "_blank"))
                                  ),
                                  
                                  # Team member 3 - Dominik Meier
                                  column(width = 3,
                                         style = "text-align: center;",
                                         tags$img(src = "dominik_meier.jpg", height = "100px", class = "img-thumbnail", alt = "Dominik Meier"),
                                         h4("Dominik Meier", style = "font-weight: bold;"),
                                         p("Tel. 078 600 600"),
                                         p("dominik.meier@finvestag.ch"),
                                         p(tags$a(href = "https://www.linkedin.com/in/dominik-meier-680688172/", "LinkedIn", target = "_blank"))
                                  ),
                                  
                                  # Team member 4 - Raphael te Brake
                                  column(width = 3,
                                         style = "text-align: center;",
                                         tags$img(src = "raphael_te_brake.jpg", height = "100px", class = "img-thumbnail", alt = "Raphael te Brake"),
                                         h4("Raphael te Brake", style = "font-weight: bold;"),
                                         p("Tel. 078 600 600"),
                                         p("raphael.tebrake@finvestag.ch"),
                                         p(tags$a(href = "https://www.linkedin.com/in/raphael-te-brake-6974572a/", "LinkedIn", target = "_blank"))
                                  )
                                )
                                
                         ),
                         
                         # Office section
                         column(width = 6,
                                h2("Büro", style = "font-size: 28px; text-align: center; margin-top: 20px; margin-bottom: 40px;"),
                                tags$img(src = "Büro.webp", height = "200px", class = "img-thumbnail", alt = "Finvest_Team"),
                                style = "text-align: center;"
                         ),
                         
                         # Location section
                         
                         
                         column(width = 6,
                                h2("Standort", style = "font-size: 28px; text-align: center; margin-top: 20px; margin-bottom: 40px;"),
                                leaflet(height = 400) %>%
                                  addTiles() %>%
                                  setView(lng = 8.5159, lat = 47.1712, zoom = 15) %>%
                                  addMarkers(lng = 8.5159, lat = 47.1712, popup = "Finvest AG<br>Zugerstrasse 45<br>6300, Zug, Schweiz"),
                                style = "text-align: center;",
                                div(style = "margin-top: 30px;"),
                                p("Zugerstrasse 45"),
                                p("6300 Zug"),
                                p("Schweiz"),
                                p("Nummer: 044 300 300"),
                                p("Mail: info@finvestag.ch")
                         )
                         
                       
                     )
            )

                      ),

)))
