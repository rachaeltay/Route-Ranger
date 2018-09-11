# Route-Ranger

“theRouteRanger” is a web application which provides crucial and relevant information to all NUS bus riders (students and faculty staff) who are planning their travel route within NUS campus to reach their destination on time. The application will provide coverage for NUS shuttle bus services A1, A2, D1 and D2, as these are the most frequently used bus services in campus. The application scope is within the most common operation hours of the bus services which are from 0715 to 2300 on both weekdays and weekends (excluding public holidays). Our main focus is to optimise the application for mobile usage.

The application will provide pre-riders the following information; the carrying capacity of a bus service, and the estimated arrival time of buses as well as the estimated time required to reach their destination. This will be done by taking into account the traffic conditions as well as ridership demand for bus services. Users will be able to view this information in a map-based UI. This allows pre-riders to make an informed decision regarding the travelling time and the availability to board if they were to utilize NUS shuttle buses.

We intend to source information regarding buses’ carrying capacity through post-riders. This will be done by collecting GeoLocation information from their phones to identify their location. This data will then be analysed in real time and used to generate the information mentioned above as useful insights for the users. One of such implementation, which will be further elaborated later, is the comparison in proximity of the bus drivers from the users of the application to determine how many users are on the bus. Henceforth, this will be referred to as ‘crowdsourcing’.

## Built Using
- R - statistical analysis and data visualisations
- R Shiny - server used
- MongoDB - database used
