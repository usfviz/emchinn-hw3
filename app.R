rm(list=ls())
cat("\014")
setwd('~/DataViz/dataset_diabetes/')


if(!require("tidyr")){
  install.packages("tidyr")
}
library("tidyr")

if(!require("ggplot2")){
  install.packages("ggplot2")
}
library("ggplot2")

if(!require("shiny")){
  install.packages("shiny")
}
library("shiny")

if(!require("scales")){
  install.packages("scales")
}
library("scales")

if(!require("plyr")){
  install.packages("plyr")
}
library("plyr")

if(!require("reshape")){
  install.packages("reshape")
}
library("reshape")

if(!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")

if(!require("GGally")){
  install.packages("GGally")
}
library("GGally")


## Read in and Clean Data ##
Diabetes <- read.csv("diabetic_data.csv")
Diabetes[Diabetes == "?"] <- NA
Diabetes[Diabetes == "Unknown/Invalid"] <- NA
Diabetes <- Diabetes %>% filter(!medical_specialty %in% c("Dentistry", "Psychiatry"))

# check for NA's and drop in important fields
Diabetes <- Diabetes %>% drop_na(weight, gender, medical_specialty)
Diabetes$weight <- droplevels(Diabetes$weight)
Diabetes$gender <- droplevels(Diabetes$gender)
Diabetes$medical_specialty <- droplevels(Diabetes$medical_specialty)
levels(Diabetes$medical_specialty)[4] <-"Obstetrics/Gynecology"

# reorder levels
Diabetes$weight <- factor(Diabetes$weight, 
                          levels = c("[0-25)", "[25-50)", "[50-75)", "[75-100)",
                                     "[100-125)", "[125-150)", "[150-175)", 
                                     "[175-200)", ">200"))

Diabetes$age <- factor(Diabetes$age, 
                          levels = c("[0-10)", "[10-20)", "[20-30)", "[30-40)",
                                     "[40-50)", "[50-60)", "[60-70)", 
                                     "[70-80)", "[80-90)", "[90-100)"))

# reduce feature size
Diabetes <- Diabetes %>% select(patient_nbr, gender, age, weight, time_in_hospital,
                                num_medications, num_lab_procedures, number_diagnoses,
                                number_emergency, diabetesMed, medical_specialty)


# DF for Bubble Plots
MeanDF <- aggregate(Diabetes[, c("num_medications", "num_lab_procedures", "time_in_hospital")], list(Diabetes$medical_specialty), mean)
CountDF <- aggregate(Diabetes[, c("num_medications")], list(Diabetes$medical_specialty), length)
bubbleDF <- merge(MeanDF, CountDF, by = "Group.1")
colnames(bubbleDF) <- c("Medical.Specialty", "Mean.Medication", "Mean.LabProcedures", "Mean.Time.In.Hospital", "Total.Patients")


# Cleaning for later
names(Diabetes)[5:9] <- c("Days.In.Hospital","Number.of.Medications", "Number.of.Lab.Procedures", 
                          "Number.of.Diagnoses", "Number.of.Prior.Emergencies")



# Checkbox Input Function
# inspired by http://stackoverflow.com/questions/41813960/
# how-to-make-the-checkboxgroupinput-color-coded-in-shiny

my_checkboxGroupInput <- function(variable, label, choices, selected, colors){
  choices_names <- choices
  if(length(names(choices))>0) my_names <- names(choices)
  div(id=variable, class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML(paste0('<label class="control-label" for="',variable,'">',label,'</label>')),
      div( class="shiny-options-group",
           HTML(paste0('<div class="checkbox" style="color:', colors,'">',
                       '<label>',
                       '<input type="checkbox" name="', variable, 
                       '" value="', choices, 
                       '"', ifelse(choices %in% selected, 'checked = "checked"', ''), 
                       '/>',
                       '<span>', choices_names,'</span>',
                       '</label>',
                       '</div>', collapse = " "))
      )
  )
}


my_names <- levels(Diabetes$medical_specialty)
my_selected <- my_names # initialize checkbox selection to all
my_colors <-c("firebrick", "#ff9900", "#3366cc", "deeppink", "forestgreen", "turquoise")

ui <- fluidPage(
  headerPanel("Diabetes Patients from 130 US Hospitals (1999-2008)"),
  sidebarPanel(
    conditionalPanel(condition = "input.conditionedPanels == 1", 
                     selectInput("variable1", label = h5(strong("Average Number of Lab Procedures Per Patient By:")), 
                                 choices = list("Number of Medications" = "Mean.Medication",
                                                "Time in Hospital" = "Mean.Time.In.Hospital"), 
                                 selected = 1), width = 3),
    conditionalPanel(condition = "input.conditionedPanels == 2", 
                     selectInput("variable2", label = h5(strong("Compare Patients By:")), 
                                 choices = list("Weight" = "weight", "Age" = "age"), 
                                 selected = 1), width = 3),
    conditionalPanel(condition = "input.conditionedPanels == 3",
                     uiOutput("med_spec"), width = 3)
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Bubble Plot", plotOutput("plot_bubble", width = "100%",
                                         hover = hoverOpts(id = "plot_hover", delay = 100, 
                                                           delayType = "debounce")),
               uiOutput("hover_info"), value = 1),
      tabPanel("Small Multiples Plots", plotOutput("plot_multi"), value = 2),
      tabPanel("Parallel Coordinates Plot", plotOutput("plot_parallel"), value = 3),
      id = "conditionedPanels"
      )
  ) 
)

server <- function(input, output) {

  output$plot_bubble <- renderPlot({
    theme1 <- theme(
      axis.text = element_text(size=13),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      legend.position = "bottom",
      panel.grid.major = element_line(colour = "grey60"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
    if(input$variable1 == "Mean.Medication"){
      ggplot(data = bubbleDF, aes(x= Mean.Medication, y= Mean.LabProcedures,
                                  colour = Medical.Specialty, size = Total.Patients)) +
        geom_point(aes(x= Mean.Medication, y= Mean.LabProcedures,
                           colour = Medical.Specialty, size = Total.Patients), shape = 1) +
        geom_point(alpha = 0.6) +
        labs(x= "\nAvg Number of Medications", y= "Avg Number of Lab Procedures\n") +
        scale_y_continuous(limits=c(0, 75)) +
        scale_x_continuous(limits=c(0, 20)) +
        scale_colour_manual(values = c("Family/GeneralPractice" = "#ff9900", "InternalMedicine" = "#3366cc",
                                       "Obstetrics/Gynecology" = "deeppink", "Pediatrics" = "forestgreen",
                                       "Cardiology" = "firebrick", "Surgery-General" = "turquoise")) +
        scale_size(range = c(3,14)) +
        guides(colour = guide_legend("Medical Specialties", title.position = "top", title.hjust = 0.5,
                                     title.theme = element_text(size = 12, angle = 0),
                                     label.theme = element_text(size = 10, angle = 0)),
               size = guide_legend("Total Patients", title.position = "top", title.hjust = 0.5,
                                   title.theme = element_text(size = 12, angle = 0), nrow = 1)) +
        theme1
    }
    else{
      ggplot(data = bubbleDF, aes(x= Mean.Time.In.Hospital, y= Mean.LabProcedures,
                                  colour = Medical.Specialty, size = Total.Patients)) +
        geom_point(aes(x= Mean.Time.In.Hospital, y= Mean.LabProcedures,
                       colour = Medical.Specialty, size = Total.Patients), shape = 1) +
        geom_point(alpha = 0.6) +
        labs(x= "\nAvg Time in Hospital (Days)", y= "Avg Number of Lab Procedures\n") +
        scale_y_continuous(limits=c(0, 75)) +
        scale_x_continuous(limits=c(0, 6)) +
        scale_colour_manual(values = c("Family/GeneralPractice" = "#ff9900", "InternalMedicine" = "#3366cc",
                                       "Obstetrics/Gynecology" = "deeppink", "Pediatrics" = "forestgreen",
                                       "Cardiology" = "firebrick", "Surgery-General" = "turquoise")) +
        scale_size(range = c(3,14)) +
        guides(colour = guide_legend("Medical Specialties", title.position = "top", title.hjust = 0.5,
                                     title.theme = element_text(size = 12, angle = 0),
                                     label.theme = element_text(size = 10, angle = 0)),
               size = guide_legend("Total Patients", title.position = "top", title.hjust = 0.5,
                                   title.theme = element_text(size = 12, angle = 0), nrow = 1)) +
        theme1
    }
  })

  
  output$plot_multi <- renderPlot({
    theme2 <- theme(
      axis.text = element_text(size = 11),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      strip.text.x = element_text(size = 13),
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
    
    if(input$variable2 == "weight"){
      ggplot(data = Diabetes, aes(x= weight, fill = gender)) +
        geom_bar(stat = "count", position = "dodge", width = 0.5, alpha = 0.5) +
        facet_wrap(~ medical_specialty, strip.position = "top") +
        labs(x= paste("\nWeight of Patient"), y= "Number of Patients\n") +
        scale_fill_manual(values = c("Male" = "#3366cc", "Female" = "deeppink")) + 
        guides(fill = guide_legend("Gender", title.position = "top", title.hjust = 0.5,
                                     title.theme = element_text(size = 12, angle = 0),
                                     label.theme = element_text(size = 10, angle = 0))) +
        theme2
    }
    else{
      ggplot(data = Diabetes, aes(x= age, fill = gender)) +
        geom_bar(stat = "count", position = "dodge", width = 0.5, alpha = 0.5) +
        facet_wrap(~ medical_specialty, strip.position = "top") +
        labs(x= paste("\nAge of Patient"), y= "Number of Patients\n") +
        scale_fill_manual(values = c("Male" = "#3366cc", "Female" = "deeppink")) + 
        guides(fill = guide_legend("Gender", title.position = "top", title.hjust = 0.5,
                                     title.theme = element_text(size = 12, angle = 0),
                                     label.theme = element_text(size = 10, angle = 0))) +
        theme2
    }
  })

  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(bubbleDF, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.65); ",
                    "left:", left_px + 2 , "px; top:", top_px + 2, "px; padding: 2px 2px 0px 2px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Avg Num of Medications: </b>", round(point$Mean.Medication, 2), "<br/>",
                    "<b> Avg Days in Hospital: </b>", round(point$Mean.Time.In.Hospital, 2), "<br/>",
                    "<b> Avg Num of Procedures: </b>", round(point$Mean.LabProcedures, 2), "<br/>",
                    "<b> Total Patients: </b>", point$Total.Patients, "<br/>")))
    )
  })
  
  output$med_spec <- renderUI(
    # checkboxGroupInput resulting from function above
    my_checkboxGroupInput("med_spec", "Compare Medical Specialties Below:", choices = my_names,
                          selected = my_selected, colors = my_colors))
  Data <- reactive({
    # filter to the desired year
    Diabetes %>% filter(medical_specialty %in% input$med_spec)
    
  })
  
  output$plot_parallel <- renderPlot({
    theme3 <- theme(
      axis.text.x = element_text(size=13, vjust = 0),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_line(colour = "grey88"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white")
    )
    
    c <- ggparcoord(data= Data(), columns = c(5:9), groupColumn = 11, scale = "uniminmax", 
                    alphaLines = 0.6, mapping = aes(color = medical_specialty)) + 
      scale_colour_manual(values = c("Family/GeneralPractice" = "#ff9900", "InternalMedicine" = "#3366cc",
                                     "Obstetrics/Gynecology" = "deeppink", "Pediatrics" = "forestgreen",
                                     "Cardiology" = "firebrick", "Surgery-General" = "turquoise")) +
      theme3
    
    # Labeling for parcoord
    # inspired by http://stackoverflow.com/questions/22952931/
    # parallel-co-ordinates-plot-in-r-ggparcoord
    
    # Figure out y-axis range after GGally scales the data
    min_y <- min(c$data$value)
    max_y <- max(c$data$value)
    pad_y <- (max_y - min_y) * 0.1
    
    # Calculate label positions for each vertical bar
    lab_x <- rep(1:5, times = 2) # 2 times, 1 for min 1 for max
    lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = 5)
    
    # Get min and max values from original dataset
    lab_z <- c(sapply(Data()[, 5:9], min), sapply(Data()[, 5:9], max))
    
    # Convert to character for use as labels
    lab_z <- as.character(lab_z)
    
    # Add labels to plot
    c <- c + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 5)
    c
  })
  
}

shinyApp(ui = ui, server = server)
