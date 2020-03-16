library(shiny)
library(dendextend)
# library(tableHTML)
library(colourpicker)
library(shinyWidgets)
library(shinyjs)
# library(shinyHeatmaply)
library(factoextra)
library(d3heatmap)
# library(circlize)
library(ape)
# library(gplots)
# library(phylocanvas)

options(shiny.maxRequestSize = 1000*1024^2)

jsCode <- "shinyjs.pageCol = function(params)
            {$('body').css('background', params);}"

sideCode <- "shinyjs.sideCol = function(params1)
            {$('#side').css('background', params1);}"

fontCode <- "shinyjs.fontCol = function(params2)
            {$('#side').css('color', params2);}"

titleCode <- "shinyjs.titleCol = function(params3)
            {$('#title').css('color', '#192658');
             $('#title').css('text-align', 'left');}"

sliderCode <- "shinyjs.sliderCol = function(params4)
              {$('.irs-bar').css('background', params4);
               $('.irs-from').css('background', 'grey');
               $('.irs-to').css('background', 'grey');}"


ui <- fluidPage(
    
    useShinyjs(),
    extendShinyjs(text = jsCode),
    extendShinyjs(text = sideCode),
    extendShinyjs(text = fontCode),
    extendShinyjs(text = titleCode),
    extendShinyjs(text = sliderCode),
    
    div(id = "title", 
        titlePanel(strong("Visualization Browser"))
    ),
    fluidRow(),
    fluidRow(),
    fluidRow(
        column(width = 3, colourpicker::colourInput("colorB", "Background Color", "#FFFFFF")),
        column(width = 3, colourpicker::colourInput("colorF", "Font Color", "White")),
        column(width = 3, colourpicker::colourInput("color1", "Plot Color - 1", "#E62929")),
        column(width = 3, colourpicker::colourInput("color2", "Plot Color - 2", "#77AB77"))
        
    ), 
    # panel(
    #     # fluidRow(
    #     #     column(width= 5, tags$ul(htmlOutput("picture"))),
    #     #     column(width= 4, offset=3, textOutput("textTomato"))
    #     # )
    # ),
    
    tags$head(tags$style("#textTomato{color: red;font-style: italic;}")),
    
    
    sidebarLayout(
        sidebarPanel(
            id="side",
            
            conditionalPanel(
                "input.tabs == 'View Data'",
                sliderInput(
                    inputId = "slider_rec",
                    label = "Display Records",
                    min = 1,
                    max = 20,
                    value = c(1,20)
                    
                )
            ),
            
            radioButtons(
                "filterYN","Remove columns with all zeroes?",
                choices = list("Yes", "No", "N/A"),selected = "N/A"
            ),
            conditionalPanel(
                "input.filterYN === 'Yes'",
                radioButtons("transposeYN","Transpose Data?", 
                             choices = list("Yes", "No"),selected = "No"
                ),
                checkboxGroupInput(
                    "cols_filter", "Check columns with rows having all 0s to remove",
                    choices = names(df)
                )
            ),
            conditionalPanel(
                "input.filterYN === 'No'",
                radioButtons("transposeYN1","Transpose Data?", 
                             choices = list("Yes", "No"),selected = "No")
            )
        ),
        
        mainPanel(width=8,
                  verbatimTextOutput("tempText"),
                  
                  div(id="tab",style="background:#FFFFFC
                      ",
                      tabsetPanel(id= "tabs", type = c("tabs"),
                                  tabPanel("Upload a File", icon=icon("file-upload")),
                                  tabPanel("View Data", icon = icon("table"),
                                           {
                                               tableOutput("file_info")
                                               # tableHTML_output("file_info")
                                           }),
                                  tabPanel("Summary", icon = icon("list-alt")),
                                  navbarMenu("Visualizations", icon = icon("globe"),
                                             tabPanel("Box Plot", icon=icon("box"), 
                                                      # plotlyOutput("boxplot")),
                                                      plotOutput("boxplot")),
                                             tabPanel("Bar Plot", icon = icon("bar-chart-o"),
                                                      plotOutput("barplot")),
                                             tabPanel("Line Plot", icon = icon("chart-line"),
                                                      plotOutput("lineplot")),
                                             tabPanel("Scatter Plot", icon = icon("braille"),
                                                      plotOutput("scatterplot")),
                                             tabPanel("Donut Plot", icon=icon("chart-pie"),
                                                      plotOutput("donutplot"))
                                  ),
                                  navbarMenu("Clustering", icon=icon("object-group"),
                                             tabPanel("Hierarchical Clustering", plotOutput("plot")),
                                             tabPanel("K-Means Clustering", plotOutput("kplot")),
                                             tabPanel("Correlation Matrix", plotOutput("distplot")),
                                             tabPanel("Phylogenetic Tree", plotOutput("phyloplot"))
                                  ),
                                  tabPanel("HeatMap", icon=icon("map"),d3heatmapOutput("heatmap"))
                                  
                      ),
                      #       ******** Input Fields on Main Panel *********
                      conditionalPanel(
                          "input.tabs == 'K-Means Clustering'",
                          numericInput("num_C", "Clusters", 2,min = 1, max = 8)
                      ),
                      
                      conditionalPanel(
                          "input.tabs == 'Phylogenetic Tree'",
                          selectInput("phylo_Type", "Select a Phylo Type", 
                                      choices = c("phylogram","cladogram", "fan", "unrooted", "radial"),
                                      selected = "phylogram")
                      ),
                      
                      conditionalPanel(
                          "input.tabs == 'HeatMap'",
                          checkboxInput("cluster", "Apply clustering", value=TRUE)
                      )
                  ),
                  
                  conditionalPanel(
                      "input.tabs == 'Upload a File'",
                      wellPanel(style="background:#FFFFFF",
                                tags$hr(),
                                fileInput("csvFile", "Drag csv file over here!")
                                # fileInput("file1", "Choose CSV File",
                                #           accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                # checkboxInput("header", "Header", TRUE)
                                # radioButtons('sep', 'Separator', c(Comma = ',', Semicolon = ';', Tab = '\t'), ','),
                                # radioButtons('quote', 'Quote', c(None = '', 'Double Quote'='"', 'Single Quote' = "'"),'"')
                                # 
                      )
                  )
        )
    )
)

server <- function(input, output, session){
    
    observeEvent(input$colorB, {
        js$pageCol(input$colorB)
    })
    
    observeEvent(input$color1, {
        js$sideCol(input$color1)
        js$titleCol(input$color1)
        
    })
    observeEvent(input$colorF, {js$fontCol(input$colorF)})
    
    df1 <- eventReactive(input$csvFile, {
        read.csv(input$csvFile$datapath)
    })
    
    observe({
        req(df1())
        x <- df1()
        y <- max(length(x[,1]))
        updateCheckboxGroupInput(session, "cols_filter", choices = names(x))
        updateSliderInput(session, "slider_rec", max=y, value = c(1,y))
    })
    
    observeEvent(input$slider_rec, {
        js$sliderCol(input$colorB)
    })
    observeEvent(input$colorB, {
        js$sliderCol(input$colorB)
    })
    
    temp <- reactive({
        req(df1())
        df <- df1()
        # if(length(input$cols_filter) > 1 & 
        if(length(input$cols_filter) > 1 & input$filterYN == "Yes") {
            temp = df[,c(input$cols_filter)];
            a <- temp[apply(temp, MARGIN = 1, function(x) any(x > 0)),]
            if(input$transposeYN == "Yes"){b <- t(a)} else {b <- a}
            return(b)
        } else {
            a <- df
            if(input$transposeYN1 == "Yes"){b <- t(a)} else {b <- a}
            return(b)
        }
    })
    
    d1 <- reactive({
        req(temp())
        pts <- temp()
        d <- dist(pts, method="euclidian")
        h <- hclust(d, method="complete")
        i <- which.max(diff(h$height))
        cut_height <- (h$height[i] + h$height[i+1])/2
        clusters <- stats::cutree(h, h=cut_height)
        par(mfrow=c(1,2))
        
        dend <- as.dendrogram(h)
        
        c <- color_branches(dend, k=2, col = c(input$color1, input$color2))
        return(c)
    })
    
    d2 <- reactive({
        d1() %>% set("leaves_pch", c(18, 19)) %>%  # node point type
            set("leaves_cex", 2) %>%  # node point size
            set("leaves_col", c(input$color1, input$color2)) %>% #node point color
            plot(ylab = "Height")
    })
    
    output$plot <- renderPlot({d2()})   #Hierarchical Clustering Plot
    
    
    #   ******HEATMAP PLOT******
    d3 <- reactive({
        req(df1())
        df <- df1()
        df2 <- df[(1:50),c(7:ncol(df))]     #From Column 7 to last Column for first 50 genes
        row.names(df2)<-df[(1:50),1]         #Row Names:Gene IDs
        
        data<-df2[apply(df2, MARGIN = 1, function(x) any(x > 0)),]
        return(data)
    })
    
    d4 <- reactive({
        req(d3())
        data <- d3()
        data_matrix<-data.matrix(data)
        return(data_matrix)
    })
    
    output$heatmap <- renderD3heatmap({
        d3heatmap(d4(), scale = "column",
                  color = scales::col_quantile(c(input$color1,"yellow", input$color2), NULL,5),
                  dendrogram = if (input$cluster) "both" else "none",
                  labRow = rownames(d4()), ylab = colnames(d4()),
                  xaxis_font_size = "12px",
                  k_row = 3, k_col = 3)
    })
    
    #   ******END HEATMAP PLOT******
    
    #   ******DISTANCE PLOT**********
    
    #Calculate distance and plot fviz_dist plot on the distance
    dist_data <- reactive({
        pts <- t(d3())
        na.omit(pts)
        d <- dist(pts, method="euclidian")
        return(d)
    })
    
    output$distplot <- renderPlot({
        factoextra::fviz_dist(dist_data(),
                  gradient = list(low = input$color1, mid = "white", high = input$color2),
                  show_labels = TRUE)
    })
    
    #   ******END DISTANCE PLOT ********
    
    #   ******PHYLOGENETIC TREE PLOT**********
    
    # Extract the .nwk formats from heatmaps by the following conversion steps:
    #           dendro --> hcclust --> phylo --> nwk
    
    # phylo <- reactive({
    phylo <- reactive({
        req(d3())
        dm <- heatmap.2(as.matrix(d3()))
        
        # Extract dendrograms for rows and columns from 'dm'
        row.dendro <- dm$rowDendrogram
        col.dendro <- dm$colDendrogram
        
        
        # Convert dendrograms to nwk (via .hcclust and .phylo formats!)
        as.hclust (row.dendro)  ->row.hcclust
        as.phylo  (row.hcclust) ->row.phylo
        write.tree(row.phylo)   ->row.nwk
        
        as.hclust (col.dendro)  ->col.hcclust
        as.phylo  (col.hcclust) ->col.phylo
        
        write.tree(col.phylo, file="C:/Users/varni/Desktop/Varnika Data/R Software + Project/phylo")
        
    })
    
    output$phyloplot <- renderPlot({
        
        t <- ape::read.tree("C:/Users/varni/Desktop/Varnika Data/R Software + Project/phylo")
        X <- c(input$color1, "orange", input$color2, "blue")
        plot.phylo(
            t,
            type=input$phylo_Type,
            show.tip.label = TRUE, show.node.label = TRUE,
            edge.color = X,
            tip.color = X,
            font=3,
            edge.width = 2,
            root.edge = TRUE
        )
        edgelabels(round(t$edge.length,2), col=X, font=1, bg = "white")
    })
    #   ******END PHYLOGENETIC TREE PLOT******
    
    #   ******BOX PLOT******
    # output$boxplot <- renderPlotly({
    output$boxplot <- renderPlot({
        d <- d3()
        outlier_values <- boxplot.stats(d4())$out
        print(outlier_values)
        boxplot(d4(),
                col=c("Orange", "Orange", "Orange", 
                      "Red", "Red", "Red", 
                      "yellow", "yellow", "yellow",
                      "DarkGreen", "DarkGreen", "DarkGreen"),
                # col=c("Orange", input$color1, input$color2),
                pars = list(boxwex = 0.6),
                border=c("DarkGreen", "DarkGreen", "DarkGreen",
                         "DarkGreen", "DarkGreen", "DarkGreen",
                         "DarkGreen", "DarkGreen", "DarkGreen",
                         "Red", "Red", "Red"),
                # border=c(input$color2, input$color1, "Orange"),
                xlab= "Time Points-->",
                ylab = "Reads-->")
        # plot_ly(d, y = d[,1],x = d[,-1],type = "box")
    })
    
    #   ******END BOX PLOT******
    
    #   ******K-MEANS CLUSTERING******
    
    km <- reactive({
        req(d3())
        pts <- t(d3()) 
        kmeans(pts, input$num_C, nstart=10)
    })
    
    output$kplot <- renderPlot({
        factoextra::fviz_cluster(km(), data=t(d3()),
                     ggtheme = theme_minimal(), 
                     ellipse.type = "norm")
    })
    #   ******END K-MEANS CLUSTERING******
    
    #   ******BEGIN BARPLOT********
    output$barplot <- renderPlot({
        barplot(
            d4(), 
            sub="Genes Expressed at different Time Intervals",
            xlab="Time Points",
            ylab="Gene Expressions", col=c("darkblue",input$color1),
            beside=TRUE, 
            # legend.text = rownames(d3()),
            args.legend = list(x = "topright"),
            names.arg = colnames(d3()),
            axisnames = TRUE)
        
    })
    #   ******END BARPLOT********
    #   ******BEGIN LINEPLOT********
    output$lineplot <- renderPlot({
        
        x1 <- d3()[,2]
        y1 <- d3()[,3]
        col1_nm <- colnames(d3())[1]
        col2_nm <- colnames(d3())[2]
        ggplot(d3(), aes(x1,y1)) + 
            geom_point(colour='green') +
            geom_line(colour = "red")
        
    })
    #   ******END LINEPLOT********
    
    #   ******BEGIN SCATTERPLOT********
    output$scatterplot <- renderPlot({
        
        x1 <- d3()[,2]
        y1 <- d3()[,3]
        col1_nm <- colnames(d3())[1]
        col2_nm <- colnames(d3())[2]
        print(col1_nm)
        # h <- ggplot(d3(), aes(x1,y1)) 
        # h + geom_point(colour='red')
        # create the graph
        plot(x1,y1,col=c(input$color1,input$color2),
             pch=16, xlim=c(min(x1),max(x1)),
             ylim=c(min(y1),max(y1)),
             xlab="Time Point1",ylab="Time Point2", las=1, cex.axis=1.0)
        legend("topright",inset=.05,c(col1_nm, col2_nm),
               pch=16,col=c(input$color1,input$color2),cex=0.8)
        
    })
    #   ******END SCATTERPLOT**********
    
    # output$plot1 <- renderPlot({
    #     plot(pts, col=c("red","green")[clusters], pch=17)
    # })
    
    output$file_info <- renderTable({
        # output$file_info <- render_tableHTML({
        req(df1())
        
        data1 <- data.frame(temp());
        data <- if(input$transposeYN == "Yes" | input$transposeYN1 == "Yes"){
            data1[,input$slider_rec[1]:input$slider_rec[2]]
        }else{
            data1[input$slider_rec[1]:input$slider_rec[2],]};
        data
        # data %>% tableHTML(rownames = FALSE) %>% 
        #     add_css_conditional_column(conditional = "!=", value = 0,
        #                                css = list(c('background-color', 'color', 'border'), 
        #                                           c(input$color2, input$colorF, input$color1)),
        #                                columns = 1:ncol(data))
    })
    
    
    # output$picture <- renderText({
    #     c('<img src="',
    #       "https://portlandnursery.com/plants/images/tomato/tomato-crop750.jpg",'"/>')
    # })
    # output$textTomato <- renderText({paste("Innovative breeding to improve production and nutrition of Tomatoes.")})
    
    # output$tempText <- renderText({dend()})
}

shinyApp(ui, server)