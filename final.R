library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(RColorBrewer)
library(sf)
library(scales)
library(leaflet)
library(glue)
library(googleCharts)
library(tidyr)
#--------------------------------------------------------------------------------------------------------------------------

# Load data
## Data Penduduk
data_penduduk <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/Penduduk.csv")
data_penduduk$Penduduk <- as.numeric(data_penduduk$Penduduk)

## Data Pendidikan
data_pendidikan <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/pendidikan.csv")
data_pendidikan$Pulau <- as.factor(data_pendidikan$Pulau)
data_pendidikan$Literacy <- as.numeric(data_pendidikan$Literacy)
data_pendidikan$Education_expenditure <- as.numeric(data_pendidikan$Education_expenditure)

## Data Kesehatan
data_kesehatan <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/AHH.csv")
data_kesehatan$AHH <- as.numeric(data_kesehatan$AHH)

## Data Penduduk
data_ketenagakerjaan <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/TPT.csv")
data_ketenagakerjaan$TPT <- as.numeric(data_ketenagakerjaan$TPT)

## Data IPTEK
dt3 <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/IPTEK1.csv")
dt3$Persentase <- as.numeric(dt3$Persentase)
#dt3$Umur <- ifelse(dt3$Umur == 19024, "19-24",
#                   ifelse(dt3$Umur == 16018, "16-18",
#                          ifelse(dt3$Umur == 13015, "13-15",
#                                 ifelse(dt3$Umur == 44693, "05-12", "25+"))))
thn <- unique(dt3$Tahun)
kelumur <- unique(dt3$Umur)
dt3 <- dt3 %>% filter(Provinsi != "Indonesia")

## Data Ekonomi
dt1 <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/GDP.csv")
dt1$GDP <- as.numeric(dt1$GDP)
dt1 <- dt1 %>% filter(Provinsi != "Indonesia")
prov <- unique(dt1$Provinsi)

## Data Pariwisata
dt2 <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/Pariwisata.csv")
dt2 <- dt2 %>% filter(Provinsi != "Indonesia")

## Data Kesejahteraan
idn <- st_read(dsn = "D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/gadm36_IDN_1.shp", layer = "gadm36_IDN_1")
data_kemiskinan <- read.csv("D:/Kuliah Pasca IPB/Semester 3/EVD Sesi UAS/Tugas Shiny/Final/Kemiskinan.csv")

#--------------------------------------------------------------------------------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: brown;
      }
    "))
  ),
  navbarPage("Indonesia Emas 2045",
             
             tags$style(type="text/css", ".widget_banner1 {
                        background-color: #0BB5E7;
                        color: #FFFFFF;
                        }"),
             tags$style(type="text/css", ".widget_banner2 {
                        background-color: #7ACA53;
                        color: #FFFFFF;
                        }"),
             tags$style(type="text/css", ".widget_banner3 {
                        background-color: #E7A80B;
                        color: #FFFFFF;
                        }"),
             tags$style(type="text/css", ".widget_banner4 {
                        background-color: #e70b5f;;
                        color: #FFFFFF;
                        }"),
             tags$style(type="text/css", ".widget_banner5 {
                        background-color: #720be7;
                        color: #FFFFFF;
                        }"),
             
             tabPanel("Kependudukan",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "Tahun1",
                                      label = "Tahun",
                                      min = 2010,
                                      max = 2045,
                                      value = 2010, animate = TRUE)
                          ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("overall1") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("tertinggi1") %>%
                              tagAppendAttributes(class = 'widget_banner2'),
                            valueBoxOutput("terendah1") %>%
                              tagAppendAttributes(class = 'widget_banner3')
                          ),
                          hr(),
                          plotlyOutput(outputId = "pyramid",height = "600px")
                        )
                      )),
             tabPanel("Pendidikan",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "Tahun17",
                                      label = "Tahun",
                                      min = 2010,
                                      max = 2045,
                                      value = 2010, animate = TRUE)
                        ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("ratlit") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("ratex") %>%
                              tagAppendAttributes(class = 'widget_banner2')
                          ),
                          hr(),
                          plotlyOutput(outputId = "bubble",height = "600px"),
                          hr(),
                          h4("Literasi adalah kemampuan individu dalam membaca, menulis, berbicara, menghitung, dan 
                             memecahkan masalah pada tingkat keahlian yang diperlukan, dalam keluarga, pekerjaan, dan 
                             masyarakat (National Insititute for Literacy). Tahun 2020, UNESCO menyebutkan minat baca 
                             masyarakat Indonesia hanya 0.001% atau dari 1000 orang, hanya 1 saja yang rajin membaca..")
                        )
                      )),
             tabPanel("Kesehatan",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "Tahun3",
                                      h3("Tahun"),
                                      min = 2010,
                                      max = 2021,
                                      value = 2010, animate = TRUE), 
                          selectizeInput(inputId = "Wilayah", h3("Wilayah"), 
                                      choices = c(prov, "Indonesia"),
                                      selected = "Aceh",
                                      multiple = TRUE)
                          ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("overall3") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("tertinggi3") %>%
                              tagAppendAttributes(class = 'widget_banner2'),
                            valueBoxOutput("terendah3") %>%
                              tagAppendAttributes(class = 'widget_banner3')
                          ),
                          hr(),
                          plotlyOutput(outputId = "density", height = "600px"),
                          hr(),
                          h4("Angka Harapan Hidup (AHH) merupakan rata-rata tahun hidup yang masih akan dijalani oleh seseorang yang telah berhasil mencapai umur x,
                                   pada suatu tahun tertentu, dalam situasi mortalitas yang berlaku di lingkungan masyarakatnya.")
                        )
                      )),
             tabPanel("Ketenagakerjaan",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "Tahun4",
                                      label = "Tahun",
                                      min = 2010,
                                      max = 2021,
                                      value = 2010,
                                      animate = TRUE)
                        ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("overall") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("tertinggi") %>%
                              tagAppendAttributes(class = 'widget_banner2'),
                            valueBoxOutput("terendah") %>%
                              tagAppendAttributes(class = 'widget_banner3')
                            ),
                          hr(),
                          plotlyOutput(outputId = "bar1", height = "650px"),
                          hr(),
                          h4("Tingkat pengangguran terbuka (TPT) adalah persentase jumlah pengangguran terhadap 
                             jumlah angkatan kerja. Angkatan Kerja adalah penduduk usia kerja (15 tahun ke atas) 
                             yang bekerja atau punya pekerjaan namun sementara tidak bekerja, dan penggangguran.")
                        )
                      )),
             tabPanel("IPTEK",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "TahunInput2",
                                      label = h3("Tahun"),
                                      min = 2012,
                                      max = 2021,
                                      value = 2012,
                                      animate = TRUE),
                          checkboxGroupInput(inputId = "UmurInput", 
                                             label = h3("Kelompok Umur"),
                                             choices = kelumur,
                                             selected = kelumur)
                        ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("kel1") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("kel2") %>%
                              tagAppendAttributes(class = 'widget_banner2'),
                            valueBoxOutput("kel3") %>%
                              tagAppendAttributes(class = 'widget_banner3'),
                            valueBoxOutput("kel4") %>%
                              tagAppendAttributes(class = 'widget_banner4'),
                            valueBoxOutput("kel5") %>%
                              tagAppendAttributes(class = 'widget_banner5')
                          ),
                          hr(),
                          plotlyOutput(outputId = "stacked", height = "600px")
                        )
                      )),
             tabPanel("Ekonomi",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "TahunInput",
                                      label = h3("Tahun"),
                                      min = 2010,
                                      max = 2021,
                                      value = c(2010, 2021),
                                      sep = ""),
                          
                          selectizeInput(inputId = "ProvinsiInput", 
                                         label = h3("Provinsi"), 
                                         choices = prov, 
                                         selected = c("Aceh"),
                                         multiple = TRUE)
                        ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("overall5") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("tertinggi5") %>%
                              tagAppendAttributes(class = 'widget_banner2'),
                            valueBoxOutput("terendah5") %>%
                              tagAppendAttributes(class = 'widget_banner3')
                          ),
                          hr(),
                          plotlyOutput(outputId = "lineChart", height = "600px"),
                          hr(),
                          h4("Produk Domestik Regional Bruto (PDRB) merupakan salah satu indikator penting untuk mengetahui 
                             kondisi ekonomi di suatu daerah dalam suatu periode tertentu, baik atas dasar harga berlaku maupun 
                             atas dasar harga konstan. PDRB pada dasarnya merupakan jumlah nilai tambah yang dihasilkan oleh 
                             seluruh unit usaha dalam suatu daerah tertentu, atau merupakan jumlah nilai barang dan jasa akhir 
                             yang dihasilkan oleh seluruh unit ekonomi pada suatu daerah.")
                        )
                      )),
             tabPanel("Pariwisata",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "TahunInput1",
                                      label = h3("Tahun"),
                                      min = 2003,
                                      max = 2021,
                                      value = 2003,
                                      animate = TRUE),
                          radioGroupButtons(inputId = "WNInput", 
                                            label = h3("Kewarganegaraan"),
                                            choiceNames = list("WNA", "WNI"),
                                            choiceValues = list("WNA", "WNI")),
                          radioGroupButtons(inputId = "HotelInput", 
                                            label = h3("Jenis Hotel"),
                                            choiceNames = list("Berbintang", "Tidak Berbintang"),
                                            choiceValues = list("Berbintang", "Non-Bintang"))
                        ),
                        mainPanel(
                          fluidRow(
                            valueBoxOutput("jumwni") %>%
                              tagAppendAttributes(class = 'widget_banner1'),
                            valueBoxOutput("jumwna") %>%
                              tagAppendAttributes(class = 'widget_banner2')
                          ),
                          hr(),
                          plotlyOutput(outputId = "bar", height = "600px")
                        )
                      )),
             tabPanel("Kesejahteraan",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "Tahun5", 
                                      label = h3("Tahun"), 
                                      choices = 2007:2022, 
                                      selected = 2007)
                        ),
                        mainPanel(
                          leafletOutput(outputId = "mapindo", width="100%", height="500px")
                        )
                      ))
  )
)

#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram ----
server <- function(input, output){

#---------------------------------PENDUDUK---------------------------------------------------------------------------------------- 
  
    output$pyramid <- renderPlotly({
    data10 <- data_penduduk %>% filter(Tahun == input$Tahun1)
    ggplotly(ggplot(data10, aes(x = Umur, fill = Jk, label = Penduduk,
                      y = ifelse(test = Jk == "Laki-laki",
                                 yes = -Penduduk, no = Penduduk))) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = abs, limits = 13000 * c(-1,1)) +
      labs(title = "Piramida Penduduk", x = "Umur", y = "Jumlah Penduduk (jiwa)") +
      theme(text = element_text(size = 14)) + 
      labs(fill = "Jenis Kelamin") +
      coord_flip() + theme_tufte(), tooltip = c("label"))
    })

    output$overall1 <- renderValueBox({
      data10 <- data_penduduk %>% filter(Tahun == input$Tahun1)
      indval1 <- sum(data10$Penduduk)
      valueBox(paste0(indval1), 
               "Jumlah Penduduk Indonesia (Ribu Jiwa)")
    })
    
    output$tertinggi1 <- renderValueBox({
      data10 <- data_penduduk %>% filter(Tahun == input$Tahun1, Jk == "Laki-laki")
      laki <- sum(data10$Penduduk)
      valueBox(paste0(laki, " Jiwa"), 
               "Jumlah Penduduk Laki-laki")
    })
    
    output$terendah1 <- renderValueBox({
      data10 <- data_penduduk %>% filter(Tahun == input$Tahun1, Jk == "Perempuan")
      perempuan <- sum(data10$Penduduk)
      valueBox(paste0(perempuan, " Jiwa"), 
               "Jumlah Penduduk Perempuan")
    })
    
#---------------------------------PENDIDIKAN---------------------------------------------------------------------------------------- 

    output$bubble <- renderPlotly({
      data17 <- data_pendidikan %>% filter(Time == input$Tahun17)
      slope <- 0.025
      data17$size <- (data17$Total_Population * slope)
      colors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
      
      fig <- plot_ly(data17, x = ~Education_expenditure, y = ~Literacy, color = ~Pulau, size = ~size, colors = colors,
                     type = 'scatter', mode = 'markers', sizes = c(min(data17$size), max(data17$size)),
                     marker = list(symbol = 'circle', sizemode = 'diameter',
                                   line = list(width = 2, color = '#FFFFFF')),
                     text = ~paste('Province:', Provinces_Name, '<br>Literacy:', Literacy, '<br>Edu Expend:', Education_expenditure,
                                   '<br>Pop.:', Total_Population))
      fig <- fig %>% layout(title = 'Literacy vs Education Expenditure',
                            xaxis = list(title = 'Education Expenditure (Bio)',
                                         gridcolor = 'rgb(255, 255, 255)',
                                         range = c(3, 10000),
                                         zerolinewidth = 1,
                                         ticklen = 5,
                                         gridwidth = 2),
                            yaxis = list(title = 'Literacy (Percentage)',
                                         range = c(0.6,1),
                                         gridcolor = 'rgb(255, 255, 255)',
                                         zerolinewidth = 1,
                                         ticklen = 5,
                                         gridwith = 2),
                            paper_bgcolor = 'rgb(243, 243, 243)',
                            plot_bgcolor = 'rgb(243, 243, 243)')
    })
    
    output$ratlit <- renderValueBox({
      data17 <- data_pendidikan %>% filter(Time == input$Tahun17)
      rat1 <- round(mean(data17$Literacy)*100,2)
      valueBox(paste0(rat1, " %"), 
               "Rata-rata Literasi penduduk Indonesia")
    })
    
    output$ratex <- renderValueBox({
      data17 <- data_pendidikan %>% filter(Time == input$Tahun17)
      rat2 <- round(mean(data17$Education_expenditure)*100,2)
      valueBox(paste0(rat2), 
               "Rata-rata Education Expenditure Indonesia (Miliar Rupiah")
    })
  
#---------------------------------KESEHATAN----------------------------------------------------------------------------------------

    output$density <- renderPlotly({
      data3 <- reactive({
        validate(
          need(input$Wilayah != "", "Pilih Provinsi!")
        )
      filtered <- data_kesehatan %>% filter(Tahun == input$Tahun3,
                                         Propinsi == input$Wilayah)
      })
      
    if (length(unique(data3()$Propinsi)) >= 2){
      ggplotly(ggplot(data3(), aes(x=AHH, group=Propinsi, fill=Propinsi)) +
      xlim(10, 80) +
      geom_density(adjust=1.5, alpha=.4) +
      labs(title = "Sebaran Angka Harapan Hidup", x = "Angka Harapan Hidup (Tahun)") +
      theme_tufte() + theme(text = element_text(size = 14)), tooltip = c("Propinsi"))
      }
    else{
      ggplotly(ggplot(data3(), aes(x=AHH, group=Jk, fill=Jk)) +
        xlim(10, 80) +
        geom_density(adjust=1.5, alpha=.4) +
        theme(text = element_text(size = 14)) +
        labs(fill = "Jenis Kelamin", title = "Sebaran Angka Harapan Hidup", x = "Angka Harapan Hidup (Tahun)") +
        theme_tufte(), tooltip = c("Jk"))
      }
    })
 
    output$overall3 <- renderValueBox({
      data3 <- data_kesehatan %>% filter(Tahun == input$Tahun3, Kab.kota == "Indonesia")
      indval3 <- round(mean(data3$AHH),2)
      valueBox(paste0(indval3, " persen"), 
               "AHH Indonesia", color = "yellow")
    })
    
    output$tertinggi3 <- renderValueBox({
      data3 <- data_kesehatan %>% filter(Tahun == input$Tahun3)
      maxval3 <- data3[order(-data3$AHH),]
      valueBox(paste0(maxval3[1,5], " persen"), 
               paste0("AHH Tertinggi Kabupaten ", maxval3[1,4]))
    })
    
    output$terendah3 <- renderValueBox({
      data3 <- data_kesehatan %>% filter(Tahun == input$Tahun3)
      minval3 <- data3[order(data3$AHH),]
      valueBox(paste0(minval3[1,5], " persen"), 
               paste0("AHH Terendah Kabupaten ", minval3[1,4]))
    })

#---------------------------------KETENAGAKERJAAN----------------------------------------------------------------------------------------

  output$bar1 <- renderPlotly({
    data4 <- data_ketenagakerjaan %>% filter(Tahun == input$Tahun4)
    data4$Wilayah <- factor(data4$Wilayah, levels = data4$Wilayah[order(data4$TPT)])
    ggplotly(ggplot(data4, aes(x=Wilayah, y=TPT)) +
      geom_bar(stat = "identity", fill="darkblue") +
      scale_y_continuous(limits = c(0, 15)) +
      theme(text = element_text(size = 14)) +
      labs(title = "Persentase Tingkat Pengangguran Terbuka (TPT)", x = "Provinsi", y = "Tingkat Pengangguran Terbuka (Persen)") +
      theme_tufte() +coord_flip())
    })
  
  output$overall <- renderValueBox({
    data4 <- data_ketenagakerjaan %>% filter(Tahun == input$Tahun4)
    indval <- max(data4$TPT)
    valueBox(paste0(indval, " persen"), 
             "TPT Indonesia", color = "yellow")
  })
  
  output$tertinggi <- renderValueBox({
    data4 <- data_ketenagakerjaan %>% filter(Tahun == input$Tahun4)
    maxval <- data4[order(-data4$TPT),]
    valueBox(paste0(maxval[1,3], " persen"), 
             paste0("TPT Tertinggi Propinsi ", maxval[1,2]))
  })

  output$terendah <- renderValueBox({
    data4 <- data_ketenagakerjaan %>% filter(Tahun == input$Tahun4)
    minval <- data4[order(data4$TPT),]
    valueBox(paste0(minval[1,3], " persen"), 
             paste0("TPT Terendah Propinsi ", minval[1,2]))
  })

#---------------------------------IPTEK----------------------------------------------------------------------------------------

  data2 <- reactive({
    filtered2 <- dt3 %>% filter(Tahun == input$TahunInput2,
                                Umur %in% input$UmurInput)
  })
  
  output$stacked <- renderPlotly({
    ggplotly(ggplot(data2(), aes(y=reorder(Provinsi, Persentase), x=Persentase, fill=Umur), label = Persentase) +
               geom_bar(position="stack", stat="identity")+ 
               xlim(0, 101) +
               scale_fill_brewer(palette="Paired") +
               xlab("Persentase Pengguna Internet Di Atas Umur 5 Tahun") + 
               theme_tufte(), tooltip = "Persentase")
  })
  
  output$kel1 <- renderValueBox({
    data2 <- dt3 %>% filter(Tahun == input$TahunInput2, Umur == "05--12")
    avg1 <- round(sum(data2$Persentase)/34, 2)
    valueBox(paste0(avg1, " %"), 
             "Rata-rata pengguna internet usia 05-12 tahun")
  })
  
  output$kel2 <- renderValueBox({
    data2 <- dt3 %>% filter(Tahun == input$TahunInput2, Umur == "13-15")
    avg2 <- round(mean(data2$Persentase),2)
    valueBox(paste0(avg2, " %"), 
             "Rata-rata pengguna internet usia 13-15 tahun")
  })
  
  output$kel3 <- renderValueBox({
    data2 <- dt3 %>% filter(Tahun == input$TahunInput2, Umur == "16-18")
    avg3 <- round(mean(data2$Persentase),2)
    valueBox(paste0(avg3, " %"), 
             "Rata-rata pengguna internet usia 16-18 tahun")
  })
  
  output$kel4 <- renderValueBox({
    data2 <- dt3 %>% filter(Tahun == input$TahunInput2, Umur == "19-24")
    avg4 <- round(mean(data2$Persentase),2)
    valueBox(paste0(avg4, " %"), 
             "Rata-rata pengguna internet usia 19-24 tahun")
  })
  
  output$kel5 <- renderValueBox({
    data2 <- dt3 %>% filter(Tahun == input$TahunInput2, Umur == "25+")
    avg5 <- round(mean(data2$Persentase),2)
    valueBox(paste0(avg5, " %"), 
             "Rata-rata pengguna internet usia 25+ tahun", width = 2)
  })
#---------------------------------EKONOMI----------------------------------------------------------------------------------------
  
  data <- reactive({
    validate(
      need(input$ProvinsiInput != "", "Pilih Provinsi!")
    )
    filtered <- dt1 %>% filter(Tahun >= input$TahunInput[1],
                               Tahun <= input$TahunInput[2],
                               Provinsi %in% input$ProvinsiInput)
  })
  
  output$lineChart <- renderPlotly({
    ggplotly(ggplot(data(), aes(x = Tahun, y = GDP, color = Provinsi)) +
               xlim(2010, 2021) +
               geom_line(size = 1) +
               geom_point(size = 1) +
               ylab("PDRB (Miliar Rupiah)") +
               theme_tufte())
  })
 
   output$overall5 <- renderValueBox({
    data <- dt1 %>% filter(Tahun == input$TahunInput[2])
    indval5 <- sum(data$GDP)
    valueBox(paste0(indval5), 
             "PDB Indonesia (Miliar Rupiah)")
  })
  
  output$tertinggi5 <- renderValueBox({
    data <- dt1 %>% filter(Tahun == input$TahunInput[2])
    maxval5 <- data[order(-data$GDP),]
    valueBox(paste0(maxval5[1,3]), 
             paste0("PDRB Tertinggi Propinsi ", maxval5[1,1], " (Miliar Rupiah)"))
  })
  
  output$terendah5 <- renderValueBox({
    data <- dt1 %>% filter(Tahun == input$TahunInput[2])
    minval5 <- data[order(data$GDP),]
    valueBox(paste0(minval5[1,3]), 
             paste0("PDRB Terendah Propinsi ", minval5[1,1], " (Miliar Rupiah)"))
  })
  
#---------------------------------PARIWISATA----------------------------------------------------------------------------------------

  data1 <- reactive({
    filtered1 <- dt2 %>% filter(Tahun == input$TahunInput1,
                                Tamu == input$WNInput,
                                Hotel == input$HotelInput)
  })
  
  ub <- reactive({
    req(input$WNInput,
        input$HotelInput)
    
    if(input$WNInput == "WNA" && input$HotelInput == "Berbintang"){
      v <- 10000
    }
    else if(input$WNInput == "WNA" && input$HotelInput == "Non-Bintang"){
      v <- 2.5e+6
    }
    else if(input$WNInput == "WNI" && input$HotelInput == "Berbintang"){
      v <- 13000
    }
    else {
      v <- 10e+6
    }
  })
  
  output$bar <- renderPlotly(
    ggplotly(ggplot(data1(), aes(x=Jumlah, y=reorder(Provinsi, Jumlah)), label = Jumlah) + 
               geom_bar(stat = "identity", fill = "darkblue") +
               xlim(0, ub()) +
               ylab("Provinsi") +
               theme_tufte(), tooltip = "Jumlah")
  )
  
  output$jumwni <- renderValueBox({
    data1 <- dt2 %>% filter(Tahun == input$TahunInput1, Tamu == "WNI")
    tamuwni <- sum(data1$Jumlah)
    valueBox(paste0(tamuwni), 
             "Jumlah Tamu Hotel WNI (Ribu Orang)")
  })
  
  output$jumwna <- renderValueBox({
    data1 <- dt2 %>% filter(Tahun == input$TahunInput1, Tamu == "WNA")
    tamuwna <- sum(data1$Jumlah)
    valueBox(paste0(tamuwna), 
             "Jumlah Tamu Hotel WNA (Ribu Orang)")
  })

#---------------------------------KESEJAHTERAAN----------------------------------------------------------------------------------------

  output$mapindo <- renderLeaflet({
    dtmap <- data_kemiskinan %>% filter(Tahun == input$Tahun5)
    
    dtjoin <- dtmap %>% 
      left_join(idn, by = c("Provinsi" = "NAME_1")) %>% 
      drop_na(GID_0) %>% 
      st_as_sf()
    
    dtleaflet <- dtjoin %>%  
      st_simplify(preserveTopology = T, dTolerance = 0.001)
    
    # Create color pallete
    pal <- colorNumeric(palette = "Greens", domain = dtleaflet$P0)
    
    labels <- glue("
  <b>{dtleaflet$Provinsi}</b><br>
  Tahun : {input$Tahun5} <br>
  Indeks Kemiskinan : {prettyNum(dtleaflet$P0, big.mark = ',')} <br>
  "
    ) %>% lapply(htmltools::HTML)
    
    leaflet(dtleaflet) %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      addPolygons(
        label = labels,
        fillColor = ~pal(P0),
        fillOpacity = .8,
        weight = 2,
        color = "white",
        highlight = highlightOptions(
          weight = 5,
          color = "black", 
          bringToFront = TRUE,
          opacity = 0.8
        )
      )%>% 
      addLegend(
        pal = pal,
        values = ~P0, 
        labels = P0,
        opacity = 1,
        title = "Indeks Kemiskinan",
        position = "bottomright"
      )
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)