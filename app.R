#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Convex Set Norms",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h4("Closest distance of road to village problem: Consider some road defined by a line, what is the point on the line that is closest 
         to a village situated at the origin with the given norm?"),
      h3("Define the Norm"),
      h3("Octagonal Style"),
      h4(jaxD("||(u,v)|| = max(a_1|u|, a_2|v|, b_1|u|+ b_2|v|)")),
      sliderInput("sla1","Coefficient a1", min = 0, max= 4, value = 3, step = 0.1),
      sliderInput("sla2","Coefficient a2", min = 0, max= 4, value = 3, step = 0.1),
      sliderInput("slb1","Coefficient b1", min = 0, max= 3, value = 2, step = 0.1),
      sliderInput("slb2","Coefficient b2", min = 0, max= 3, value = 2, step = 0.1),
      actionBttn("convexOct","Show the Octagonal Convex Set"),
      h3("Elliptical Style"),
      h4(jaxD("||(u,v)|| = `sqrt{u^2/a^2 + v^2/b^2}")),
      sliderInput("sla","Major axis a", min = 0, max= 2, value = 1, step = 0.1),
      sliderInput("slb","Minor axis b", min = 0, max= 1, value = 1/2, step = 0.1),
      actionBttn("convexEll","Show the Elliptical Convex Set"),
      h3("Taxicab Style"),
      h4(jaxD("||(u,v)|| = a|x|+b|y|")),
      sliderInput("slat", "Coefficient a", min = 1, max = 4, value = 2, step = 0.1),
      sliderInput("slbt", "Coefficient b", min = 1, max = 4, value = 2, step = 0.1),
      actionBttn("convexTaxi", "Show the Taxicab Convex Set"),
      h3("Infinity Style"),
      h4(jaxD("||(u,v)|| = max(a|x|, b|y|)")),
      sliderInput("slai", "Coefficient a", min = 2, max = 4, value = 3, step = 0.1),
      sliderInput("slbi", "Coefficient b", min = 2, max = 4, value = 3, step = 0.1),
      actionBttn("convexInf", "Show the Infinity Convex Set")
    ),
    column(width = 4,
      h3("Geometric Visualization"),
      h4("Click on two points to define a line not in the convex set."),
      uiOutput("msgx"),
      plotOutput("plane", height = 600, click = "plot_click")
    ),
    column(width = 4,
      h3("Analytical Solution"),
      uiOutput("outpt1"),
      uiOutput("outpt2"),
      uiOutput("outcoord"),
      uiOutput("outres")
    )
      
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

graph.line <- function(m,n,p, col = "black"){
  if (abs(m)+abs(n) == 0) return()
  if (m== 0 ) abline(h=p/n,col = col)
  else if (n== 0 ) abline(v=p/m,col = col)
  else abline(p/n,-m/n,col = col)
}

server <- function(session, input, output) {
  norm <- function(x,y) {0}
  v <- numeric(2)
  w <- numeric(2)
  y <- numeric(2)
  fM <- 0
  chooseV <- TRUE   #state variable
  showPlot <- function() {
    z <- matrix(nrow = 200, ncol = 200)
    x <- seq(from = -1, to = 1, length.out = 200)
    y <- seq(from = -1, to = 1, length.out = 200)
    z <- outer(x,y,norm)
    output$plane <- renderPlot({
      contour(x,y,z,levels = 1, col = "black", asp = 1, drawlabels = FALSE)
      # First point is selected
      if (norm(v[1],v[2]) >0) {
        points(v[1],v[2],pch = '.',cex = 5, col = "red")
        text(v[1]+0.05,v[2],"p1",cex = 2)
        output$outpt1 <- renderUI(h3("First selected point p1 is (", round(v[1],4), ", ", round(v[2],4), ")"))
      }
      # Second point is selected
      if (norm(w[1],w[2]) >0) {
        output$outpt2 <- renderUI(h3("Second selected point p2 is (", round(w[1],4), ", ", round(w[2],4), ")"))
        points(w[1], w[2], pch = '.', cex = 5, col = "red")
        text(w[1]+0.05,w[2],"p2",cex = 2)
        slope <<- (w[2]-v[2])/(w[1]-v[1])
        intercept <<- -1*slope*v[1]+v[2]
        abline(intercept, slope)
        
        # find the min norm
        candidate_x <- seq(-2,2,by=0.0001)
        candidate_y <- slope * candidate_x + intercept
        
        op_norm <- min(norm(candidate_x, candidate_y))
        
        # Find the point closest to the line
        opx <- candidate_x[which.min(norm(candidate_x, candidate_y))]
        opy <- slope * opx + intercept

        contour(x,y,z, levels = c(1,op_norm), col = c("black","green"), asp = 1, drawlabels = FALSE)
        abline(-1*slope*v[1]+v[2], slope)
        points(v[1], v[2], pch = '.', cex = 5, col = "red")
        text(v[1]+0.05,v[2],"p1",cex = 2)
        points(w[1], w[2], pch = '.', cex = 5, col = "red")
        text(w[1]+0.05,w[2],"p2",cex = 2)
        
        points(opx, opy, pch = '.', cex = 5, col = "blue")
        text(opx+0.05, opy, "x", cex=2)
        if(opx == 0){
          abline(v=opx)
        }else{
          abline(0, opy/opx)
        }
        
        output$outres <- renderUI(h3("Distance to village at closest point is ",  round(op_norm,4)))
        output$outcoord <- renderUI(h3("Point on the line closest to the village x is"," (", round(opx,4), ", ", round(opy,4), ")"))
      }
    })
    
  }
  observeEvent(input$convexOct,{
    a1 <- input$sla1
    a2 <- input$sla2
    b1 <- input$slb1
    b2 <- input$slb2
    nrm <- function(x,y) {max(a1*abs(x),a2*abs(y), b1*abs(x)+ b2*abs(y))}
    norm <<- Vectorize(nrm, c("x","y"))
    chooseV <<- TRUE
    v <<- c(0,0)
    w <<- c(0,0)
    y <<- c(0,0)
    showPlot()
  })
  observeEvent(input$convexEll,{
    a <- input$sla
    b <- input$slb
    nrm <- function(x,y) {sqrt(x^2/a^2+y^2/b^2)}
    norm <<- Vectorize(nrm, c("x","y"))
    chooseV <<- TRUE
    v <<- c(0,0)
    w <<- c(0,0)
    y <<- c(0,0)
    showPlot()
  })
  observeEvent(input$convexTaxi, {
    a <- input$slat
    b <- input$slbt
    nrm <- function(x,y) {a*abs(x) + b*abs(y)}
    norm <<- Vectorize(nrm, c("x", "y"))
    chooseV <<- TRUE
    v <<- c(0,0)
    w <<- c(0,0)
    y <<- c(0,0)
    showPlot()
  })
  observeEvent(input$convexInf, {
    a <- input$slai
    b <- input$slbi
    nrm <- function(x,y) {max(a*abs(x), b*abs(y))}
    norm <<- Vectorize(nrm, c("x", "y"))
    chooseV <<- TRUE
    v <<- c(0,0)
    w <<- c(0,0)
    y <<- c(0,0)
    showPlot()
  })
  observeEvent(input$plot_click,{
    if (chooseV) {
      v[1] <<- input$plot_click$x
      v[2] <<- input$plot_click$y
      chooseV <<- FALSE
      fM <<- 1/norm(v[1],v[2])
      showPlot()
      return()
    }
    w[1] <<- input$plot_click$x
    w[2] <<- input$plot_click$y
    showPlot()
  })
}

#Run the app
shinyApp(ui = ui, server = server)