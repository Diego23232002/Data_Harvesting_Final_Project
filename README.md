# Data_Harvesting_Final_Project
In this repository, we will scrape data from the official ATP website to obtain crucial match metrics and analyze them.

In order to reproduce the code, you only need to paste your user agent where I put mine (html_atp_website, html_atp_website2 and html_atp_website3). Once you have done that, you just have to run the code. There is a minor problem with the first part where you have to add your user agent (html_atp_website) because it returns an error when you first run it but, if you try a second time, everything works perfectly fine and the rest of the code runs without an issue. 

This is the error I am talking about: (Error in read_html.response(httr::GET(atp_link, httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 Edg/134.0.0.0\n; Diego Fernandez / dieguitus23@gmail.com"))) : Forbidden (HTTP 403).) 

the rest of the code requires Selenium to scrape the necesary information from the ATP website. We used Rselenium to connect to the chrome browser with the port 4445L.
