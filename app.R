library(shiny)
library(shinythemes)
library(ggplot2)
library(leaflet)

# Data rumah sakit dengan latitude dan longitude
rumah_sakit <- data.frame(
  wilayah = c(rep("Yogyakarta", 12)),
  nama = c("RSU Queen Latifa", "Happy Land Medical Centre", "Hidayatullah Islamic Hospital", 
           "RSUD Kota Yogyakarta", "RS Pratama", "PKU Muhammadiyah Hospital Of Yogyakarta", 
           "RSU Sakina Idaman", "Dr. Sardjito General Hospital", "JIH Hospital", "Bethesda Hospital", 
           "Siloam Hospitals Yogyakarta", "Panti Rapih Hospital"),
  lat = c(-7.575660, -7.771879, -7.782376, -7.797209, -7.803581, -7.779182, 
          -7.766049, -7.773536, -7.746907, -7.783342, -7.758173, -7.773591),
  lng = c(110.399766, 110.390992, 110.379507, 110.366987, 110.351251, 110.381547, 
          110.393198, 110.361366, 110.387226, 110.360112, 110.399246, 110.379604),
  link_gmaps = c(
    "https://maps.app.goo.gl/ox8HhswiUuSDgazr6",
    "https://maps.app.goo.gl/Br9R4BGWDU8bgLGBA",
    "https://maps.app.goo.gl/3i1q4pWNVJpqvKPGA",
    "https://maps.app.goo.gl/o5SmPGTVWtvoUeXL7",
    "https://maps.app.goo.gl/JKCiMgwKLLfCRKhW7",
    "https://maps.app.goo.gl/RcnGXwMcsqUZBuaq5",
    "https://maps.app.goo.gl/T9mUFE7DCz8TJRBe8",
    "https://maps.app.goo.gl/LccSzKhimoFKHriMA",
    "https://maps.app.goo.gl/h6ddaWVfDdQ3cu2M6",
    "https://maps.app.goo.gl/f13eXW3s7JNvadYS8",
    "https://maps.app.goo.gl/HCCF5Uwx7hZ7kGq79",
    "https://maps.app.goo.gl/QjSgv5DkLz6uiPzm9"
  )
)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
  background: rgba(230, 104, 145, 0.8) !important;
  background-image: url('bg.png');
  background-size: cover;
  background-position: center center;
  background-repeat: no-repeat;
  background-attachment: fixed;

  font-family: 'Poppins', sans-serif !important;
  color: #000000;
  padding-bottom: 0 !important;
  margin-bottom: 0 !important;
  overflow-x: hidden;
}

      h2, .title {
        font-size: 22px !important;
        font-family: 'Poppins', sans-serif !important;
        font-weight: 700 !important;
        margin-bottom: 20px;
      }

      .card-style {
  background-color: rgba(244, 209, 171, 0.8); /* Transparan */
  padding: 20px;
  border-radius: 20px;
  box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1);
  margin-bottom: 20px;
}

      .well {
        background-color: transparent !important;
        border: none !important;
        box-shadow: none !important;
      }

      .container-fluid > .row {
        align-items: flex-start !important;
      }

      .pesan-container {
        margin-top: 5px; 
        padding: 15px;
        background: linear-gradient(to right, #da1286 , #e66891);
        color: white;
        border-radius: 10px;
      }

      .rs-container {
        margin-top: 40px;
      }

      .bmi-container {
  background: linear-gradient(to right, #da1286, #e66891);
  padding: 15px;
  border-radius: 10px;
  margin-bottom: 30px;
  color: #f9efe6;
}


      .hitung-container {
        padding: 10px;
        margin-top: 20px;
        border-radius: 10px;
      }

      .btn-danger {
        background-color: #ff0940 !important;
        border-color: #ff0940 !important;
        color: white !important;
        border-radius: 12px !important;
        font-weight: normal;
      }

      h3, h4 {
        font-weight: normal;
      }
    "))
  ),
  
  titlePanel(
    div(class = "title",
        HTML("Diatrack: Aplikasi Deteksi Risiko Diabetes")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      div(class = "card-style",
          # --- Semua input selain wilayah dipertahankan di sini ---
          selectInput("umur", "Berapa usiamu?", choices = c(
            "Pilih salah satu" = NA,
            "Di bawah 35" = "0",
            "35 - 45" = "1",
            "46 - 55" = "2",
            "56 - 65" = "3",
            "Di atas 65" = "4"
          )),
          selectInput("keturunan", "Apakah kamu memiliki keturunan diabetes?", choices = c(
            "Pilih salah satu" = NA,
            "Tidak" = "0",
            "Nenek atau Kakek" = "3",
            "Orang Tua" = "5"
          )),
          selectInput("pola_makan", "Apakah kamu menjaga pola makan gizi seimbang?", choices = c(
            "Pilih salah satu" = NA,
            "Ya" = "0",
            "Tidak" = "1"
          )),
          selectInput("gula_darah", "Apakah kamu memiliki kadar gula darah yang tinggi?", choices = c(
            "Pilih salah satu" = NA,
            "Tidak" = "0",
            "Ya" = "5"
          )),
          selectInput("olahraga", "Apakah kamu sering olahraga?", choices = c(
            "Pilih salah satu" = NA,
            "Ya" = "0",
            "Tidak" = "2"
          )),
          selectInput("merokok", "Apakah kamu merokok?", choices = c(
            "Pilih salah satu" = NA,
            "Tidak" = "0",
            "Ya" = "2"
          )),
          selectInput("alkohol", "Apakah kamu mengonsumsi alkohol?", choices = c(
            "Pilih salah satu" = NA,
            "Tidak" = "0",
            "Ya" = "2"
          )),
          
          div(class = "bmi-container",
              numericInput("berat", "Berat Badan (kg):", value = NA),
              numericInput("tinggi", "Tinggi Badan (m):", value = NA)
          ),
          
          div(class = "bmi-container",
              textOutput("bmi_output")
          ),
          
          div(class = "hitung-container",
              actionButton("hitung", "Hitung Poin", class = "btn btn-danger btn-lg btn-block")
          )
      )
    ),
    
    mainPanel(
      div(class = "card-style",
          h3("Hasil Penilaian Risiko"),
          uiOutput("hasil"),
          plotOutput("grid")
      ),
      
      div(id = "pesan-container", class = "pesan-container",
          h4("Pesan Untukmu!"),
          uiOutput("pesan")
      ),
      
      div(class = "rs-container",
          div(class = "card-style",
              h4("Pilih Rumah Sakit Terdekat"),
              selectInput("wilayah", "Pilih Wilayah", choices = c("Yogyakarta")),
              selectInput("rs", "Pilih Rumah Sakit:", choices = NULL),
              htmlOutput("map"),
              uiOutput("link_gmaps")
          )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Update daftar RS saat wilayah berubah
  observe({
    wilayah_terpilih <- input$wilayah
    rs_list <- subset(rumah_sakit, wilayah == wilayah_terpilih)
    updateSelectInput(session, "rs", choices = rs_list$nama)
  })
  
  output$map <- renderUI({
    req(input$rs)
    rs_terpilih <- subset(rumah_sakit, nama == input$rs)
    tags$iframe(src = paste("https://maps.google.com/maps?q=", rs_terpilih$nama, "&output=embed"),
                width = "100%", height = "400", style = "border:0;")
  })
  
  output$link_gmaps <- renderUI({
    req(input$rs)
    rs_terpilih <- subset(rumah_sakit, nama == input$rs)
    tags$a(href = rs_terpilih$link_gmaps, target = "_blank", "Lihat di Google Maps", style = "font-size: 18px; color: #000000;")
  })
  
  bmi_val <- reactive({
    req(input$tinggi, input$berat)
    round(input$berat / (input$tinggi^2), 1)
  })
  
  output$bmi_output <- renderText({
    bmi <- bmi_val()
    kategori <- ifelse(bmi < 18.5, "Berat badan kurang",
                       ifelse(bmi < 25, "Berat badan normal",
                              ifelse(bmi < 30, "Kelebihan berat badan", "Obesitas")))
    paste("Skor BMI-mu:", bmi, "-", kategori)
  })
  
  hitung_poin <- eventReactive(input$hitung, {
    bmi <- bmi_val()
    bmi_point <- ifelse(bmi < 18.5, 0,
                        ifelse(bmi < 25, 1,
                               ifelse(bmi < 30, 2, 5)))
    total <- sum(
      as.numeric(input$umur),
      as.numeric(input$keturunan),
      as.numeric(input$pola_makan),
      as.numeric(input$gula_darah),
      as.numeric(input$olahraga),
      as.numeric(input$merokok),
      as.numeric(input$alkohol),
      bmi_point
    )
    total
  })
  
  output$hasil <- renderUI({
    total <- hitung_poin()
    
    if (total <= 6) {
      "Hebat! RISIKO DIABETESMU RENDAH. 🌿\nAsal kamu tau, 1 dari 100 orang mengidap diabetes loh!"
    } else if (total <= 11) {
      "Loh! KAMU PUNYA SEDIKIT RISIKO DIABETES. 🌼\nAsal kamu tau, 1 dari 25 orang mengidap diabetes loh!"
    } else if (total <= 14) {
      "Eh! RISIKO DIABETESMU SEDANG. 🚶\nAsal kamu tau, 1 dari 6 orang mengidap diabetes loh!"
    } else if (total <= 19) {
      "Aduh! RISIKO DIABETESMU TINGGI. ⚠️\nAsal kamu tau, 1 dari 3 orang mengidap diabetes loh!"
    } else {
      "Gawat! RISIKO DIABETESMU SANGAT TINGGI. ⚠️\nAsal kamu tau, 1 dari 2 orang mengidap diabetes loh!"
    }
  })
  
  output$grid <- renderPlot({
    total <- hitung_poin()
    size <- 10
    n <- size * size
    risk_index <- if (total <= 6) 1 else if (total <= 11) 4 else if (total <= 14) 17 else if (total <= 19) 33 else 50
    
    df <- expand.grid(x = 1:size, y = 1:size)
    df$risk <- 1:n <= risk_index
    
    col_grad <- if (total <= 6) c("darkgreen", "lightgreen")
    else if (total <= 11) c("goldenrod", "lightyellow")
    else if (total <= 14) c("orange", "khaki")
    else if (total <= 19) c("red", "pink")
    else c("darkred", "red")
    
    ggplot(df, aes(x, y, fill = risk)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("TRUE" = col_grad[1], "FALSE" = col_grad[2]), guide = FALSE) +
      coord_equal() +
      theme_void() +
      theme(
        panel.background = element_rect(fill = NA, color = NA),  # hilangkan latar panel
        plot.background = element_rect(fill = NA, color = NA)    # hilangkan latar plot
      )
  }, bg = "transparent")  # <--- Ini penting untuk menjadikan plot transparan di Shiny
  
  output$pesan <- renderUI({
    total <- hitung_poin()
    if (total <=6) {
      h5("Terus pertahankan gaya hidup sehatmu!")
    } else if (total <=14) {
      h5("Cobalah untuk lebih memperhatikan pola makan dan olahraga secara teratur.")
    } else {
      h5("Segera konsultasikan dengan dokter untuk pemeriksaan lebih lanjut.")
    }
  })
}

shinyApp(ui, server)
