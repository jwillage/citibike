library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    useShinyjs(),
    fluidRow(
      HTML("
           <h1 align = 'center', class = 'text-primary'><i>The Citi Bike Project</i></h1>"),
      column(5, 
        HTML("
             <img src = 'img/citibike_640.gif' />
             <br>
             &nbsp;&nbsp;&nbsp;<b>Citi Bike Station Popularity Over Month</b>
             ")
      ),
      column(5,
             HTML("
                  Citi Bike launched in New York in the summer of 2013. It was a highly anticipated project in the year leading up to the launch, started by Motivate (formerly Alta Bicycle Share). Seemingly overnight, dozens of docking stations sprouted up across the city. On May 27, 2013, the program opened to the public. <br><br>
                  Citi Bike’s historical trip data is released on their website, dating back to the program’s second month. Analysis shows that the program got off to a busy start, and despite controversy and several issues, has increased in popularity. <br><br>
                  <a href = 'img/trips_by_month.png'><img height = 150, width = 225 src = 'img/trips_by_month.png')/></a><br><br>
                  One of the interesting variables to look at is the user type, “subscriber” or “customer”. Citi Bike has close to 90,000 annual subscribers [<a target='_blank' href = 'https://www.citibikenyc.com/assets/pdf/September_2015_Citi_Bike_Monthly_Report.pdf'>1</a>], who account for 88% of all trips taken. However, the trends seen throughout the program hold true for both subscribers and general customers. For instance, even though bikes can be checked out for 30 minutes (45 for subscribers), the average trip for both user types is between 10 and 14 minutes. I found that surprising, given that I’m always scrambling to return to a station in time.<br><br>
                  An interesting subject is station popularity. The data confirms that the most popular stations are in midtown Manhattan. The plot on the left shows how many trips start and end at a given station by month. Even in winter months, when other stations remain full or sometimes closed, the midtown locations are still in use. And, once the data is normalized for number of customers, it’s easy to see that in general there's an even split for station majority (52% of stations have subscriber majority vs 48% for customer majority). Although in Brooklyn, where the program is not as widespread, most stations are dominated by subscribers.<br><br>
                  <a href = 'img/customer_subscriber.png'><img height = 200, width = 300 src = 'img/customer_subscriber.png')/></a><br><br>
                  Another system-wide trend is length of trips by gender. The only demographic data in Citi Bike’s data sets are related to gender and year of birth. While most people skip over the birth year question, 88% of trips have an associated gender. For this subset of data with gender identified, it turns out that female’s trips last about 18% longer than men’s. It’s uncertain if this is due to the speed of riding, the distance of the trip, or some other unknown reason. It’s also worth noting that about 75% of all trips (with gender identified) are taken by men. Even if women made up the entirety of the unidentified trips, they would still only account for 33% of all trips. <br><br>
                  <a href = 'img/gender_duration.png'><img height = 200, width = 300 src = 'img/gender_duration.png')/></a><br><br>"
                  )
      )
    )
  )
)