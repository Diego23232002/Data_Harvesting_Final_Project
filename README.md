# Data_Harvesting_Final_Project
## ATP Stats Web Scraping with Selenium in R

In this repository, we will scrape data from the official ATP website to obtain crucial match metrics and analyze them. This project automates web scraping using RSelenium.

In order to reproduce the code, you only need to paste your user agent where I put mine (html_atp_website, html_atp_website2 and html_atp_website3). Once you have done that, you just have to run the code. There is a minor problem with the first part where you have to add your user agent (html_atp_website) because it returns an error when you first run it but, if you try a second time, everything works perfectly fine and the rest of the code runs without an issue. 

This is the error I am talking about: (Error in read_html.response(httr::GET(atp_link, httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 Edg/134.0.0.0\n; Diego Fernandez / dieguitus23@gmail.com"))) : Forbidden (HTTP 403).) 

the rest of the code requires Selenium to scrape the necesary information from the ATP website. We used a chromeDriver. 
- Our Google Chrome version is 134.0.6998.89 (make sure the ChromeDriver version matches).
- Once the ChromeDriver is installed, we added it to our system’s PATH.
- You can download the matching ChromeDriver version from here (https://googlechromelabs.github.io/chrome-for-testing/#stable) 

## How to run the script

this is a short summary of the steps we have taken in order to extract the information and present it in a clean table.

- Start Selenium and the rest of the packages in R
- Launch ChromeDriver: remDr <- rsDriver(browser = "chrome", port = 4444L)
- Navigate to the ATP stats page
- Extract the table data
- Convert the data to a clean table
- Close the session: driver$close()
- Repeat the process for every single variable we want to scrape
- Join all the tables and perform data cleaning
- Save the table and use it to create shiny app
