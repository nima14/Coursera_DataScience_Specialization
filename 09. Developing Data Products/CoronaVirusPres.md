Corona Virus Dashboard
========================================================
author: Nima Taghidoost
date: Wednesday, 15 April 2020
autosize: true

Corona Virus
========================================================

Now a tiny virus has changed so many lives in the world.
We should watch the trend of this little creature from the begining until now!

Dataset
========================================================

I used a dataset in  "Kaggle.com"  to create this dashboard.




```r
colnames(CoronaDF)
```

```
[1] "Province.State" "Country.Region" "Lat"            "Long"          
[5] "Date"           "Confirmed"      "Deaths"         "Recovered"     
```
As you can see it has 8 fields.


Corona Virus Dashboard
========================================================

The dashboard consists of three plots which two of them
were made by "Plotly" and one of them with "googleVis".

Here is the link to the dashboard:
https://nimataghidoost.shinyapps.io/CoronaShiny

How to work with dashboards
========================================================

In the first dashboard you can set the number of countries to show and also decide
which type of cases to see (Confirmed,Death or Recovered).

According to large number
of people who got the disease in US, we decided to put a checkbox to optionally choose
whether to see US's data or not.


In third dashboard also you can choose the type of cases and the country.
