library(shiny)
library(randomForest)
library(bslib)

addResourcePath(prefix = 'assets', directoryPath = 'www')

data <- read.csv("netflix_customer_churn.csv")
data$customer_id <- NULL
data$churned <- as.factor(data$churned)
model <- randomForest(churned ~ ., data=data, ntree=100)

ui <- fluidPage(
  theme = bs_theme(bg = "#141414", fg = "#FFFFFF", primary = "#E50914"),
  
  tags$head(
    tags$style(HTML("
      body {
        /* We use 'assets' instead of 'www' here */
        background-image: url('assets/netflix_background1.jpeg'); 
        background-size: cover;
        background-position: center;
        background-attachment: fixed;
      }
      .card-box { 
        background-color: rgba(0,0,0,0.88); 
        padding: 25px; 
        border-radius: 15px; 
        border: 2px solid #E50914;
      }
      h1, h2, h3 { color: #E50914; font-weight: bold; }
    "))
  ),

  titlePanel("🍿 Netflix Churn Predictor"),
  br(),

  fluidRow(
    column(4,
      div(class = "card-box",
        h3("Viewer Behaviour"),
        numericInput("watch_hours", "Watch Hours", 10),
        numericInput("last_login_days", "Days Since Last Login", 10),
        numericInput("profiles", "Profiles", 2),
        numericInput("avg_watch", "Avg Watch Time Per Day", 1),
        br(),
        actionButton("predict", "Predict Netflix Fate 🔮", 
                     style="background-color:#E50914; color:white; width:100%; border:none; height:45px;")
      )
    ),
    
    column(8,
      div(class = "card-box",
        h2("Prediction Result"),
        h1(textOutput("result_text"), style="text-align:center;"),
        br(),
        div(style="text-align:center;", uiOutput("meme_container"))
      )
    )
  )
)

server <- function(input, output) {
  state <- reactiveValues(msg = "Ready...", img = NULL)

  observeEvent(input$predict, {
    newdata <- data.frame(
      age=30, gender="Male", subscription_type="Standard",
      watch_hours=input$watch_hours, last_login_days=input$last_login_days,
      region="Asia", device="Mobile", monthly_fee=13.99,
      payment_method="Credit Card", number_of_profiles=input$profiles,
      avg_watch_time_per_day=input$avg_watch, favorite_genre="Action"
    )

    pred <- predict(model, newdata)

    if(pred == 0){
      msgs <- c("🍿 Just one more episode...", "📺 Binge watching mode activated", 
                "🛋 Couch potato detected", "😴 Sleep schedule destroyed", 
                "🔒 Locked in for the season", "⛔ Do not disturb")
      imgs <- c("just_one_more_episode.jpeg", "bingewatching.jpeg", 
                "couch_potato.jpeg", "darkcircles.jpeg", 
                "lock_in.jpeg", "don't_disturb.jpeg")
    } else {
      msgs <- c("💀 Netflix lost another soldier", "🌱 User went outside to touch grass", 
                "📈 Productivity arc begins", "🏋️ Gym era started", 
                "📚 Trying to study now", "🔋 Social battery recharging")
      imgs <- c("lost_another_soldier.jpeg", "touch_grass.jpeg", 
                "producktive.jpeg", "gym_era.jpeg", 
                "trying_to_study.jpeg", "social_battery.jpeg")
    }

    i <- sample(1:length(msgs), 1)
    state$msg <- msgs[i]
    state$img <- imgs[i]
  })

  output$result_text <- renderText({ state$msg })

  output$meme_img <- renderImage({
    req(state$img)
    
    list(src = file.path("www", state$img), width = "100%", alt = "Meme")
  }, deleteFile = FALSE)

  output$meme_container <- renderUI({
    req(state$img)
    imageOutput("meme_img", height = "auto")
  })
}

shinyApp(ui, server)