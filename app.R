# Load required packages
library("dplyr")
library("ggmap")
library("ggplot2")
library("gridExtra")
library("plotly")
library("shiny")

# Load the data
education_data <- read.csv("data/barro_lee_dataset.csv",
  stringsAsFactors = FALSE
)

# Create a background page containing information about our problem and our
# research questions
page_one <- tabPanel(
  "Background",
  titlePanel("Background"),
  p(
    "Today, education remains inaccessible for millions of children and
    adolescents around the world. According to",
    a("UNICEF,", href = "https://www.unicef.org/education"),
    '"roughly one in five school-aged children are not in school at all."',
    "This is due to various reasons such as poverty, gender, location,
    ethnicity, etc."
  ),
  p("We hope to bring more awareness to the high levels of education inequality
    occurring across countries, particularly developed countries vs. developing
    countries, with our visualizations."),
  br(),
  h3("Research Questions"),
  tags$ul(
    tags$li("How does average years of primary schooling attained differ across
            the countries in a region?"),
    tags$li("How has the percent of no schooling attained in the population
            changed over the years for each region?"),
    tags$li("How does average years of total schooling attained differ across
            the world?")
  )
)

# Create a page containing a horizontal bar graph of average years of primary
# school attained by country for a region
page_two <- tabPanel(
  "Primary School Completion",
  titlePanel("Primary School Completion"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Choose a region",
        choices = unique(education_data$region_code),
        selected = "Middle East and North Africa"
      )
    ),
    mainPanel(plotlyOutput("completion_plot"))
  )
)

# Create a page containing line graphs of no schooling attained rates over the
# years
page_three <- tabPanel(
  "No Schooling Percentage",
  titlePanel("No Schooling Percentage"),
  br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "region_checkbox",
        label = ("Select Region(s)"),
        choices = unique(education_data$region_code),
        selected = "Middle East and North Africa"
      )
    ),
    mainPanel(plotOutput("no_schooling_plot"))
  )
)

# Create a page containing a world choropleth map of average years of schooling
page_four <- tabPanel(
  "Average Years of Schooling",
  titlePanel("Average Years of Schooling"),
  br(),
  plotlyOutput("world_map")
)

# Create a conclusion page
page_five <- tabPanel(
  "Conclusion",
  titlePanel("Conclusion"),
  p("Our first visualization shows that even among regions in 2010, the average
    years of primary schooling attained differed greatly. For instance, in
    Sub-Saharan Africa, Mali's average was 1.3 years while Botswana's was 2.6
    years."),
  p("Our second visualization shows that although the percentage of no
    schooling has decreased over time for all regions, there is a large gap
    in levels across the regions. For instance, the percentage of no
    schooling was about 3% in Advanced Economies and about 1% in Europe and
    Central Asia while the percentage was about 34% in Sub-Saharan Africa
    and 32% in South Asia."),
  p("Our third visualization shows the vast difference in average years of
    schooling across the world. North America, Europe, and Australia tended
    to have the most average years of schooling while South America, Africa,
    and South/Southeast Asia tended to have the least"),
  p("These various visualizations show that, although education inequality has
    improved, in 2010 there were still a lot of youth in the world not
    receiving a proper education.")
)

# Create a page describing the technology we used
page_six <- tabPanel(
  "About the Tech",
  titlePanel("About the Tech"),
  br(),
  p("For this project, we used dplyr to organize our data and ggmap, ggplot2,
    gridExtra, and plotly to create our visualizations. We did not use any
    APIs."),
  p(
    "For more information read our",
    a("technical report.",
      href =
        "https://github.com/kaylamchea/info201-project/wiki/Technical-Report"
    )
  )
)

# Create an about us page
page_seven <- tabPanel(
  "About Us",
  titlePanel("About Us"),
  p("We are a group of students interested in social change."),
  br(),
  h4("Team Members"),
  tags$ul(
    tags$li("Kayla Chea - Sophomore in Informatics"),
    tags$li("Kevin Chou - Sophomore in Pre Sciences"),
    tags$li("Sinan Zhao - Sophomore in Economics"),
    tags$li("Yanpei Xu - Sophomore in Economics")
  )
)

# Pass each page to a multi-page layout
ui <- fluidPage(
  titlePanel("Youth Education Across the World"),
  tabsetPanel(
    page_one,
    page_two,
    page_three,
    page_four,
    page_five,
    page_six,
    page_seven
  )
)

server <- function(input, output, session) {
  # Define a horizontal bar graph of average years of primary school
  # to render in the UI
  output$completion_plot <- renderPlotly({
    # Create a data frame of the passed in region's average years of
    # primary schooling attined in 2010
    get_specific_yr_sch_pri <- function(selected_region) {
      education_data %>%
        filter(year == 2010) %>%
        select(year, country, yr_sch_pri, region_code) %>%
        na.omit() %>%
        group_by(country) %>%
        summarise(
          yr_sch_pri = sum(yr_sch_pri),
          region_code = region_code
        ) %>%
        filter(region_code == selected_region)
    }

    # Create a bar graph of the data frame
    bar_graph <- ggplot(
      get_specific_yr_sch_pri(input$region),
      aes(
        x = reorder(country, -yr_sch_pri),
        y = yr_sch_pri,
        text = yr_sch_pri
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste0(
          "Average Years of Primary School in ",
          input$region, ", 2010"
        ),
        x = "Country", y = "% of Primary School Completion"
      )

    # Make the bar graph interactive and return it
    ggplotly(bar_graph, tooltip = "text")
  })

  # Create a line graph for the percentage of no schooling attained by year to
  # render in the UI
  output$no_schooling_plot <- renderPlot({
    # Create a line graph of the data frame
    avg_lu_for_region <- function(region_of_interest) {
      avg_lu <- education_data %>%
        filter(region_code == region_of_interest) %>%
        group_by(year) %>%
        summarize(lu = mean(lu))

      line_graph <- ggplot(
        data = avg_lu,
        mapping = aes(x = year, y = lu)
      ) +
        geom_point() +
        geom_line() +
        ylim(0, 100) +
        labs(
          title = paste(
            "% of No Schooling in",
            region_of_interest,
            "by Year"
          ),
          x = "Year", y = "% of No Schooling"
        )
    }

    if (length(input$region_checkbox) == 0) {
      ggplot(data.frame())
    } else {
      gl <- lapply(input$region_checkbox, avg_lu_for_region)
      grid.arrange(grobs = gl, nrow = 1)
    }
  })

  # Create a world choropleth map of average years of schooling attained in 2010
  # to render in the UI
  output$world_map <- renderPlotly({
    # Create a data frame of the average years of schooling in 2010 for each
    # country
    locations <- education_data %>%
      filter(year == "2010") %>%
      select(country, yr_sch)

    # Plot the countries' average years of schooling on a world map
    map <- plot_geo(
      type = "choropleth",
      locations = locations$country,
      locationmode = "country names",
      z = locations$yr_sch,
      colorscale = "Blues"
    ) %>%
      colorbar(title = "Average Years of Schooling") %>%
      layout(
        title = "Average Years of Schooling Attained by Country, 2010"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
