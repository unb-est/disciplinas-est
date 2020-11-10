# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(tm)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Dados -------------------------------------------------------------------

historico <- read_rds("historico_limpo.rds")
vagas <- read_rds("vagas_limpo.rds")

bacharelado <- historico %>% filter(tipo == "Bacharelado")
servico <- historico %>% filter(tipo == "Serviço")

# App ---------------------------------------------------------------------

sidebar<-dashboardSidebar(
    sidebarMenu(
        menuItem("Estatística", tabName = "geral", icon = icon("university")),
        menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
        menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-line")),
        switchInput(
            inputId = "Id015",
            label = "%", 
            size = "mini"
        )
    )
    
)


header<-dashboardHeader(title = "Disciplinas da EST")

body<-dashboardBody(
    
# Página Inicial ----------------------------------------------------------
    
    tabItems(
        tabItem(tabName = "geral",
                fluidRow(
                    infoBoxOutput("alunos"),
                    infoBoxOutput("professores"),
                    infoBoxOutput("disciplinas"),
                    infoBoxOutput("aprovacoes")
                )            
        ),
        
        
# Disciplinas de Serviço --------------------------------------------------
        
        tabItem(tabName = "serviço",
                
                fluidRow(
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'serv_disc', 'Filtre pela(s) disciplina(s):', choices = c("Estatística aplicada","Cálculo de probabilidade 1"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "semestre",
                            label = "Filtre pelo(s) semestre(s):", 
                            choices = c("1/2016","2/2016","1/2017","2/2017","1/2018","2/2018","1/2019","2/2019","1/2020","2/2020"),
                            selected = c("1/2018", "1/2018")
                        ),
                        selectInput(
                            'curso', 'Filtre pelo(s) curso(s):', choices = c("Biologia","Economia","Ciência da computação","Contabilidade"),
                            selectize = FALSE,selected = "None"
                        ),
                        selectInput(
                            'professor', 'Filtre pelo(s) professor(es):', choices = c("Ana Maria Nogales","Eduardo Monteiro","Jhames Sampaio","Maria Teresa"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "horário",
                            label = "Filtre pelo horário:", 
                            choices = c("8h", "10h", "12h", "14h", "16h",'18h')
                        ),
                        awesomeCheckboxGroup(
                            inputId = "turma",
                            label = "Filtre pela(s) turma(s):", 
                            choices = c( "A", "B","C"),
                            selected = "A",
                            inline = TRUE, 
                            status = "warning"
                        ),
                        actionBttn(
                            inputId = "button",
                            label = "Atualizar",
                            style = "unite", 
                            color = "primary"
                        )
                    ),
                    infoBoxOutput("aprovacoes1"),
                    infoBoxOutput("reprovacoes1")
                )             
        ),
        
# Bacharelado -------------------------------------------------------------
        
        tabItem(tabName = "bacharelado",
                
                fluidRow(
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'bach_disc', 'Filtre pela(s) disciplina(s):', 
                            choices = sort(unique(bacharelado$disciplina)),
                            selected = "None",
                            multiple = TRUE
                        ),
                        sliderTextInput(
                            inputId = "período",
                            label = "Filtre pelo(s) periodo(s):", 
                            choices = sort(unique(bacharelado$periodo)),
                            selected = c(as.character(min(bacharelado$periodo)), as.character(max(bacharelado$periodo)))
                        ),
                        selectInput(
                            'professor', 'Filtre pelo(s) professor(es):', 
                            choices = sort(unique(bacharelado$professor)),
                            selected = "None",
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "horário",
                            label = "Filtre pelo(s) horário(s):", 
                            choices = sort(unique(bacharelado$horario)),
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "turma",
                            label = "Filtre pela(s) turma(s):", 
                            choices = sort(as.character(unique(bacharelado$turma))),
                            multiple = TRUE
                        ),
                        actionBttn(
                            inputId = "button",
                            label = "Atualizar",
                            style = "unite", 
                            color = "primary"
                        )
                    ),
                    infoBoxOutput("aprovacoes2"),
                    infoBoxOutput("reprovacoes2"),
                    box(width = 8,
                        title="Menções",
                        plotlyOutput("bach_mencoes", height = "800px")),
                    box(width = 12,
                        title="Proporção de Aprovação, Reprovação e Trancamentos",
                        plotlyOutput("bach_resultados")  
                    )
                )
        )
        
        
        
    )
    
    
    
    
    
)

ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- function(input, output) {
    
    output$alunos <- renderInfoBox({
        infoBox(
            "Alunos", "100" ,
            color = "blue", fill = TRUE,icon=icon("user-friends")
        )
    })
    
    output$professores <- renderInfoBox({
        infoBox(
            "Professores", "30" ,
            color = "yellow", fill = TRUE,icon=icon("chalkboard-teacher")
        )
    })
    
    output$disciplinas <- renderInfoBox({
        infoBox(
            "Disciplinas", "20" ,
            color = "red", fill = TRUE,icon=icon("book-reader")
        )
    })


# Serviço -----------------------------------------------------------------

    output$aprovacoes <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$aprovacoes1 <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$reprovacoes1 <- renderInfoBox({
        infoBox(
            "Reprovação média", "230 (30%)" ,
            color = "red", fill = TRUE,icon=icon("prohibited")
        )
    })
    

# Bacharelado -------------------------------------------------------------

    disc_selecionadas <- reactive({
      if(is.null(input$bach_disc)){
        return(bacharelado)
      }
      filter(bacharelado, disciplina %in% input$bach_disc)
      })
    
    output$aprovacoes2 <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$reprovacoes2 <- renderInfoBox({
        infoBox(
            "Reprovação média", "230 (30%)" ,
            color = "red", fill = TRUE,icon=icon("prohibited")
        )
    })
    
    output$bach_resultados <-renderPlotly({
        p <- bacharelado %>% 
            group_by(periodo, resultado) %>% 
            summarise(n = n()) %>% summarise(prop = n/sum(n), resultado = resultado) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line() + 
            labs(x="Período", y="Proporção") +
            scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
        ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        
    })
    
    output$bach_mencoes <- renderPlotly({
        p <- disc_selecionadas() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao)), position = "fill") + 
            labs(x="", y="Proporção", fill = "") +
            scale_fill_brewer(palette = "RdBu", direction = -1) + 
            coord_flip()
        
        ggplotly(p)
        
    })
    
}

shinyApp(ui, server)
