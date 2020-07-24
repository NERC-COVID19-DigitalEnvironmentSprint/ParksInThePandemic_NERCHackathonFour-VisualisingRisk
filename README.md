## Parkcast: a web app for forecasting the busyness of parks during the pandemic
### COVID-19 Hackathon 4: Visualising risk (Team: parksinthepandemic)

<p align="center">
<img src="https://drmattg.github.io/PiP_GH_pages/posts/01_Introduction/Hex_Logo_4th_mockup-01.png" width="40%">
</p>

### Summary

With the onset of COVID-19 in the UK, the importance of recreational park use was brought into sharp focus as one of very few recreational opportunities that was permissible during the initial lockdown period. On the one hand, this potentially presents opportunities for maintaining good health in otherwise isolated times. On the other, a lack of alternative recreational opportunities could mean that parks become busier hotspots for recreation and therefore present risks to the transmission of COVID-19. 

We have created an interactive app (https://mattlloydjones.shinyapps.io/parksinthepandemic/) that allows the public to  visualise the risks and benefits of using parks over time and space. The app allows users to visualise and experimentally forecast how busy parks (including local parks, national parks, public beaches, marinas, dog parks, plazas, and public gardens) in their area will be over the next few days. The app visualises openly-available, aggregated and anonymised mobile device data from Google (https://www.google.com/covid19/mobility/) that tracks (with user’s consent) how many people are in parks each day in districts of England. Our key innovation is to:

* Combine the Google park busyness data with Met Office weather data and UK Government data relating to park usage and access to green spaces
* Build a random forest regression model to understand how weather, park usage and access to greenspace is related to park busyness (as defined by Google)
* Use OpenWeather forecast data to forecast how busy parks will be in the future (next few days) - based on the weather and social science data, in an interactive app.

Please see our website https://drmattg.github.io/PiP_GH_pages/ for details on our Rationale, how we meet the SMART criteria, the team and the app.

### How to run the app

#### Instructions for running the app online (easier but less stable)
1. Go to https://mattlloydjones.shinyapps.io/parksinthepandemic/
2. Interact with the dropdowns to select the time period, district, whether you would like to view all of the historical data with no prediction or whether you would like to view each weekday with a prediction of the next weekday. **NOTE: It WILL crash the Shiny servers when you do this a few times - this will be fixed in due course - for now, just reload the page in the bottom left when it crashes, and it will load the data you want! Sorry!**

#### Instructions for locally running the app (harder but more stable)
1. Clone the repository - Clone this repository to your local computer.
2. Run ‘downloadOSFdata.R’ in the cloned repo directory.
3. Run ‘app.R’ (set working directory to source file location first)

### Data used

The primary data on park visits displayed in the app is Google data from the Google Community Mobility Reports - Google LLC Google COVID-19 Community Mobility Reports. https://www.google.com/covid19/mobility/

The map of the boundaries of the Google Community Mobility Reports UK districts are located at Open Science Framework (https://osf.io/c7kg4/). These shapefiles (named googleboundaries__XXXX) contain OS data © Crown copyright and database right 2020, derived from derived from OSopen boundary-line (https://www.ordnancesurvey.co.uk/business-government/products/boundaryline). 

Historical weather data plotted on the bar graph and used to inform the forecast model is supplied by the Met Office (c) Crown Copyright, 2020 as part of their their COVID-19 response https://metdatasa.blob.core.windows.net/covid19-response/index.html. This data is released under the open government license version 3.0. Contains Met Office data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

Natural England data used to inform the forecast model is the ‘Visits’ data ( We http://publications.naturalengland.org.uk/file/6746346730291200) from Natural England’s  Monitor of Engagement with the Natural Environment. The national survey on people and the natural environment. Technical Report to the 2009—2019 surveys. This data is released under the open government license version 3.0. Contains Natural England data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

Greenspace data used to inform the forecast model contains OS data © Crown copyright and database right 2020 Contains Royal Mail data © Royal Mail copyright and Database right 2020 Contains National Statistics data © Crown copyright and database right 2020. This data is released under the open government license version 3.0. Contains OS, Royal Mail and National Statistics data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/Available at https://www.ons.gov.uk/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain

Weather forecast data is from the OpenWeather 5-day/3 hour forecast weather API (https://openweathermap.org/api) obtained under a ‘Free’ pricing plan.
