library(shiny)
library(shinythemes)



ui <- fluidPage(theme = shinytheme("slate"),
 
  titlePanel('Pre-treatment and Principal component analysis of multivariate data'),
 
# define inputs
     
fluidRow( 
  column(4, 
         fileInput(inputId = 'file',
                   label = 'Choose your file')),
  
     
  column(4,strong('Select a treatment') ,
    
  checkboxInput(inputId = "baseline", 
                label = strong("baseline correction"), 
                value = FALSE),
  conditionalPanel(condition = 'input.baseline == true',
                   selectInput(inputId = 'baseline method', 
                               label = 'Method for baseline correction',
                               choices = 'rubberband' ),

                                 ),
   
            

         ) , 
  
  
  column(4, 
         
         checkboxInput(inputId = "derivative", 
                                     label = strong("derivative"), 
                                     value = FALSE),
                    
                    
            conditionalPanel(condition = 'input.derivative == true',
                             selectInput(inputId = 'difforder', 
                             label = 'differentiation order',
                            choices = c(1,2), selected =2 ),
          conditionalPanel(condition = 'input.derivative == true',
                           selectInput(inputId = 'polyorder', 
                                       label = 'polynomial order for sav.golay smoothing',
                                       choices = c(1,2,3,4,5,6,7),
                                       selected =3),
          conditionalPanel(condition = 'input.derivative == true',
                           sliderInput(inputId = 'window', 
                                     label = 'window size (must be odd)',
                                     min = 1, max = 400,
                                     value = 21
                                     )
                                    
                   
                   
                   
                  
     )
  )
    
)

)
),

# Define outputs
fluidRow(
  column(4, 'dimensions of your data',
         verbatimTextOutput(outputId = 'dim',
                            ))
),


fluidRow(
  column(4,
sliderInput(inputId= 'spectramin',
          label = 'x.min', min = 400, max = 4000,
          value = 400),
sliderInput(inputId= 'spectramax',
          label = 'x.max',min = 400, max = 4000,
          value = 1700),
plotOutput(outputId ='spectra'
           )
)
,
column(4,
       conditionalPanel(condition = 'input.baseline == true',
                      plotOutput(outputId ='corrected')
       )
)
,
column(4,
       conditionalPanel(condition = 'input.derivative == true',
                        plotOutput(outputId ='derivative')
       
       )
)


)
)

server <- function(input, output, session){

  #  Create a plot of the selected spectra
  data <- reactive({
   
    
     inFile <- input$file
  
   
       datos <- read.table(inFile$datapath
                          
                        
                          )
  
       
return(datos)
      
  })
  
  output$dim <- try(renderPrint({
    dim(data())
  }), silent =T)
  # observeEvent(data(), print(dim(data)))
  
  output$spectra <- renderPlot({

    
    
    for(i in 1:length(rownames(data()))){
      plot(as.numeric(sub('X','',colnames(data()))),
           data()[i,],
           xlab = 'wave number cm-1',
           ylab = 'absorbance a.u.',
           xlim = c(input$spectramax,
                    input$spectramin),
           ylim = c(0,max(data())),
           type = 'l')
      par(new = T)
    }

  })
 
  output$corrected <- renderPlot({
  
  library(hyperSpec)
  spc <- new('hyperSpec', spc = data(), wavelength = as.numeric(sub('X','',colnames(data()))))
  bend <- 0.1 * wl.eval (spc, function (x) x^6+x^5+x^4+x^3+x^2, normalize.wl=normalize01)
  
  bl <- spc.rubberband (spc+bend, noise = 1e-4, df=20)-bend
  suma <- spc+bend
  spc3 <- spc - bl
  
  plot(spc, wl.reverse = TRUE)
  plot(bl, add=TRUE, col=2,wl.reverse = TRUE)
  plot(spc3,wl.reverse = TRUE)
  corregido <- as.data.frame(spc3[1:nrow(spc3$data)])
  corregido2 <- as.data.frame(corregido[,1])
  })
  
  output$derivative <- renderPlot({
    
    library(prospectr)
    
    w2 <-reactive({as.numeric(input$window)})
    
    
    sg <- reactiveValues(a = matrix(ncol= as.numeric(ncol(corregido2)-w2()-1),
                                    nrow= nrow (corregido2))
    )

  
    # for (i in 1:nrow(data())){

      sg$a <- savitzkyGolay(X = corregido2,
                             m = as.numeric(input$difforder),
                             p = as.numeric(input$polyorder),
                             w = input$window,
                             delta.wav=2)
    # }
    # 
      # colnames(sg) <- colnames( savitzkyGolay(X = data()
      #                                         ,m = 2,
      #                                         p = 3,
      #                                         w = 41,
      #                                         delta.wav=2))
    # #   
    #   
      
      
      # matplot(as.numeric(colnames(sg)),sg)
      
      for(i in 1:nrow(sg$a))
        {

        plot(as.numeric(colnames(sg$a)),
             as.numeric(sg$a[i,]),
             xlab = 'wave number cm-1',
             ylab = '2nd derivative of abs',
             xlim = c(1700,400),
             ylim = c(max(sg$a),
                      min(sg$a)),
             type = 'l',
             col = 'black')
        par(new = T)


      }
    
    # }
    # colnames(sg) <- colnames( savitzkyGolay(X = data()[1,]
    #                                         ,m = 2,
    #                                         p = 3,
    #                                         w = w2+1,
    #                                         delta.wav=2))
    
    
    
  })
  



}

  
  
shinyApp(ui, server)