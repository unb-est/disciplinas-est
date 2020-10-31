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

historico <- read_rds("historico.rds")

qtd_vagas<- read_rds("qtd_vagas.rds")

# Aplicativo --------------------------------------------------------------

sidebar<-dashboardSidebar(
    sidebarMenu(
        menuItem("Estatística", tabName = "geral", icon = icon("university")),
        menuItem("Disciplinas em serviço", tabName = "serviço", icon = icon("handshake")),
        menuItem("Disciplinas da EST", tabName = "EST", icon = icon("chart-line")),
        switchInput(
            inputId = "Id015",
            label = "%", 
            size = "mini"
        )
    )
    
)

header<-dashboardHeader(title = "Disciplinas da EST")


body<-dashboardBody(
    
    
    tabItems(
        #primeira pagina
        tabItem(tabName = "geral",
                box(title="Semestre",status = "primary",solidHeader = T,width=12,
                    selectInput(
                        'semestre1', '', choices = unique(historico$Período),
                        selectize = FALSE,selected = 2019.2
                    )
                ),
                fluidRow(
                    infoBoxOutput("alunos"),
                    infoBoxOutput("professores"),
                    infoBoxOutput("disciplinas"),
                    infoBoxOutput("aprovacoes")
                ),
                box(title="Proporção de aprovação",status = "warning",solidHeader = T,
                    plotlyOutput("prop_aprov1")
                ),
                box(title="Menções por disciplina",status = "warning",solidHeader = T,
                    uiOutput("disc1"),
                    plotOutput("mencoes1")
                )
        ),
        
        #segunda pagina
        tabItem(tabName = "serviço",
                
                fluidRow(
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'disc', 'Selecione a disciplina:', choices = c("Estatística aplicada","Cálculo de probabilidade 1"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "semestre",
                            label = "Selecione o(s) semestre(s) a ser(em) avaliado(s):", 
                            choices = c("1/2016","2/2016","1/2017","2/2017","1/2018","2/2018","1/2019","2/2019","1/2020","2/2020"),
                            selected = c("1/2018", "1/2018")
                        ),
                        selectInput(
                            'curso', 'Selecione o curso:', choices = c("Biologia","Economia","Ciência da computação","Contabilidade"),
                            selectize = FALSE,selected = "None"
                        ),
                        selectInput(
                            'professor', 'Selecione o professor:', choices = c("Ana Maria Nogales","Eduardo Monteiro","Jhames Sampaio","Maria Teresa"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "horário",
                            label = "Selecione o horário:", 
                            choices = c("8h", "10h", "12h", "14h", "16h",'18h')
                        ),
                        awesomeCheckboxGroup(
                            inputId = "turma",
                            label = "Selecione a(s) turma(s):", 
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
        
        #terceira pagina
        tabItem(tabName = "EST",
                
                fluidRow(
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'disc_EST', 'Selecione a disciplina:', choices = unique(historico$Disciplina), #colocar so disciplinas da EST
                            selectize = FALSE
                        ),
                        uiOutput("sem_EST"),
                        uiOutput("prof_EST"),
                        uiOutput("tur_EST")
                    ),
                    infoBoxOutput("aprovacoes2"),
                    infoBoxOutput("reprovacoes2")
                ),
                box(title="Proporção de Aprovação",
                    plotlyOutput("prop_aprov_EST")  
                ),
                box(title="Menções",
                    uiOutput("sem_EST1"),
                    plotOutput("men_EST")),
                box(title = "Falta de vagas",
                    plotlyOutput("vagas_EST"))
                
        )
        
        
        
        
    )
)


ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- function(input, output, session) {
    
    ###################################################################1ª página 
    #reactive
    data1<- reactive({historico %>% filter(Curso=="Estatística",Período==input$semestre1)}) 
    
    data2<-reactive({merge(historico %>% filter(Curso=="Estatística",Período<=input$semestre1) %>% group_by(Período,Resultado) %>% summarise(Prop=length(Resultado)),
                           historico %>% filter(Curso=="Estatística",Período<=input$semestre1) %>% group_by(Período) %>% summarise(N=length(Resultado)),
                           by="Período") %>% group_by(Período,Resultado) %>% summarise(Num=Prop,
                                                                                       Prop=round((Prop/N)*100,2)  ) %>%
            filter(Resultado=="Aprovação" | Resultado=="Reprovação" ) 
    })  
    
    data3<-reactive({merge(data1() %>% group_by(Disciplina,Menção) %>% summarise(Prop=length(Resultado)),
                           data1() %>% group_by(Disciplina) %>% summarise(N=length(Resultado)),
                           by="Disciplina") %>% group_by(Disciplina,Menção) %>% summarise(Num=Prop,
                                                                                          Prop=round((Prop/N)*100,2))
    })   
    
    #Número de alunos
    output$alunos <- renderInfoBox({
        infoBox(
            "Alunos:", nrow(data1() %>% summarise(unique(Matrícula))) ,
            color = "blue", fill = TRUE,icon=icon("user-friends")
        )
    })
    
    #Número de professores
    output$professores <- renderInfoBox({
        infoBox(
            "Professores", nrow(data1() %>% summarise(unique(`Matricula Prof`))) ,
            color = "yellow", fill = TRUE,icon=icon("chalkboard-teacher")
        )
    })
    
    #Número de disciplinas
    output$disciplinas <- renderInfoBox({
        infoBox(
            "Disciplinas", nrow(data1() %>% summarise(unique(Disciplina))) ,
            color = "red", fill = TRUE,icon=icon("book-reader")
        )
    })
    
    #Proporção de aprovação
    output$aprovacoes <- renderInfoBox({
        infoBox(
            "Aprovação:", paste(round((nrow(data1() %>% filter(Resultado=="Aprovação"))/nrow(data1()))*100,2),"%",sep="") ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    #Gráfico proporção de aprovação 
    
    output$prop_aprov1<-renderPlotly({
        
        ggplotly(ggplot(data2(),aes(x=Período, y=Prop, colour=Resultado)) +
                     geom_line(size=1) + geom_point(size=1.5) +
                     labs(x="Semestres", y="Proporção de aprovação") +
                     scale_colour_manual(name="Legenda", values = c("#00A4CD", "#F08080"), labels = c("Aprovação", "Reprovação"))+
                     scale_x_discrete(limits = c(seq(min(data2()$Período), max(data2()$Período), 4))) +
                     theme_bw() +
                     theme(axis.title.y=element_text(colour="black", size=12),
                           axis.title.x = element_text(colour="black", size=12),
                           axis.text = element_text(colour = "black", size=9.5),
                           panel.border = element_blank(),
                           axis.line = element_line(colour = "black")))
        
    })
    
    #Gráfico das menções
    output$disc1<-renderUI({
        selectInput(
            'disc1', 'Selecione a disciplina:', choices = unique(data3()$Disciplina),
            selectize = FALSE
        )
        
    })
    
    output$mencoes1<-renderPlot({
        
        ggplot(data3() %>% filter(Disciplina==input$disc1), aes(x = Menção,y=Prop)) + 
            geom_bar(stat='identity' ,fill="#1B998B") +
            labs(x="Menção", y="Frequência") +
            theme_bw() +
            theme(axis.title.y=element_text(colour="black", size=12),
                  axis.title.x = element_text(colour="black", size=12),
                  axis.text = element_text(colour = "black", size=9.5),
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black"))+
            scale_fill_discrete(limits=c("SS","MS","MM","MI","II","SR","TR","TJ"))
        
    })  
    
    ########################################################################## Terceira Pagina
    
    data_p<-reactive({
        
        historico[historico$Disciplina==input$disc_EST,]
        
    })
    
    output$sem_EST<-renderUI({
        sliderTextInput(
            inputId = "sem_EST",
            label = "Selecione o(s) semestre(s) a serem avaliados:", 
            choices = unique(data_p()$Período),
            selected = c(max(unique(data_p()$Período)), max(unique(data_p()$Período)))
        )
    })
    
    
    output$prof_EST<-renderUI({
        
        selectInput(
            'prof_EST', 'Selecione o professor:', choices =c("Todos",unique((data_p() %>% filter(Período>=as.numeric(input$sem_EST[1]) & Período<=as.numeric(input$sem_EST[2])))$Professor)),
            selectize = FALSE,selected = "Todos"
        )
        
    })
    
    data_p2<- reactive({
        if(input$prof_EST=="Todos"){
            data_p() %>% filter(Período>=as.numeric(input$sem_EST[1]) & Período<=as.numeric(input$sem_EST[2]))
        }else{
            data_p() %>% filter(Período>=as.numeric(input$sem_EST[1]) & Período<=as.numeric(input$sem_EST[2]),
                                Professor==input$prof_EST)
        }
        
    })
    
    output$tur_EST<-renderUI({
        selectInput(
            inputId = "tur_EST",
            label = "Selecione a turma:", 
            choices = c("Todas",unique(data_p2()$Turma)),
            selectize = FALSE,selected = "Todas"
        )
        
    })
    
    data4<- reactive({
        
        if(input$tur_EST=="Todas"){
            data_p2()
        } else {
            data_p2()[data_p2()$Turma==input$tur_EST,]
        }
        
    })
    
    data5<- reactive({
        
        merge(
            data4() %>% group_by(Período,Resultado) %>% summarise(Prop=length(Resultado)),
            data4() %>% group_by(Período) %>% summarise(N=length(Resultado)),
            by="Período") %>% 
            group_by(Período,Resultado) %>% summarise(Prop=round((Prop/N)*100,2)  ) %>%
            filter(Resultado=="Aprovação" | Resultado=="Reprovação" ) 
        
    })
    
    data6<- reactive({
        
        merge(data4() %>% group_by(Período,Disciplina,Menção) %>% summarise(Prop=length(Nome)),
              data4() %>% group_by(Período) %>% summarise(N=length(Nome)),
              by="Período") %>% 
            group_by(Período,Disciplina,Menção) %>% summarise(Num=Prop,Prop=round((Prop/N)*100,2)) 
        
    })
    
    data7<-reactive({
        
        qtd_vagas%>% filter(Disciplina==input$disc_EST,
                            Semestre>=as.numeric(input$sem_EST[1]) & Semestre<=as.numeric(input$sem_EST[2]))
        
    })
    
    #Proporção de aprovação
    output$aprovacoes2 <- renderInfoBox({
        infoBox(
            "Aprovação:", paste(round((nrow(data4() %>%filter(Resultado=="Aprovação") %>% summarise(unique(Matrícula)))/nrow(data4()))*100,2),"%",sep="") ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    #Proporção de reprovação
    output$reprovacoes2 <- renderInfoBox({
        infoBox(
            "Reprovação:", paste(round((nrow(data4() %>%filter(Resultado=="Reprovação") %>% summarise(unique(Matrícula)))/nrow(data4()))*100,2),"%",sep="") ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$prop_aprov_EST<-renderPlotly({
        
        ggplotly(
            
            ggplot(data5(),aes(x=Período, y=Prop, colour=Resultado)) +
                geom_line(size=1) + geom_point(size=1.5)+
                labs(x="Semestres", y="Proporção de aprovação") +
                scale_colour_manual(name="Legenda", values = c("#00A4CD", "#F08080"), labels = c("Aprovação", "Reprovação"))+
                theme_bw() +
                theme(axis.title.y=element_text(colour="black", size=12),
                      axis.title.x = element_text(colour="black", size=12),
                      axis.text = element_text(colour = "black", size=9.5),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"))+
                scale_x_discrete(limits = c(seq(min(data2()$Período), max(data2()$Período), 4)))
            
        )
        
    })
    
    #Gráfico das menções
    output$sem_EST1<-renderUI({
        selectInput(
            'sem_EST1', 'Semestre:', choices =unique(data6()$Período),
            selectize = FALSE,selected = max(unique(data6()$Período))
        )
    })
    
    output$men_EST<-renderPlot({
        
        ggplot(data6() %>% filter(Período==input$sem_EST1), aes(x = Menção, y=Prop)) + 
            geom_bar(stat="identity",fill="#1B998B") +
            labs(x="Menção", y="Proporção de alunos") +
            theme_bw() +
            theme(axis.title.y=element_text(colour="black", size=12),
                  axis.title.x = element_text(colour="black", size=12),
                  axis.text = element_text(colour = "black", size=9.5),
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black"))
        
    })
    
    #Falta de vagas
    output$vagas_EST<-renderPlot({
        
        
        ggplot(data7(), aes(x=Semestre, y=`Quantitativo de alunos que não conseguiram se matricular por falta de vagas`)) +
            geom_line(size=1.5,colour="#1B998B") + geom_point(colour="#1B998B")+
            labs(x="Semestre", y="Alunos que não conseguiram se matricular por falta de vagas") +
            theme_bw() +
            theme(axis.title.y=element_text(colour="black", size=7),
                  axis.title.x = element_text(colour="black", size=8),
                  axis.text = element_text(colour = "black", size=9.5),
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black"))+
            scale_x_discrete(limits = c(seq(min(data7()$Semestre), max(data7()$Semestre), 4)))
        
    })
    
    
    
}


shinyApp(ui, server)

