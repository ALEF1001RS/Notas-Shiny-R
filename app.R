devtools::install_github("ramnathv/htmlwidgets#351")
library(shiny)
library(dplyr)
library(DT)
library(tidyverse)



df<-dados

ui <- fluidPage(

    titlePanel("Aplicativo de notas do ENEM"),
    sidebarLayout(
        sidebarPanel(
            selectInput("idCategoria", "Selecione a categoria:",
                        choices=sort(unique(df$Categoria))),
             
            selectInput("idCurso", "Selecione o curso:",
                        choices=sort(unique(df$Curso))),
            selectInput("idRegiao", "Selecione a região:",
                        choices=sort(unique(df$Região))),
            selectInput("idEstado", "Selecione o estado:",
                        choices=sort(unique(df$Estado))),
            numericInput("num", "Insira sua nota:", min=0, max=1000, value=0)
        ),

        mainPanel(
          dataTableOutput("dtable")
           
        )
    )
)


server <- function(input, output, session) {
  
 # observe({
  #  categ<-df$Categoria %>%
   #   unique() %>%
    #  sort()
  #  updatePickerInput(
   #   session,
   # "idCategoria",
   #   choices = categ
   # )
 
  
  
   output$dtable<-renderDataTable({
     
     datat<-df%>%filter(categoria==input$idCategoria )
     
      datatable(datat)
     
   })
    }
   
  
  

shinyApp(ui = ui, server = server)



