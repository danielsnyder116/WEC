# =============================================================================
#           ACCESSSING GOOGLE SHEETS VIA PYTHON
# =============================================================================


import pandas as pd
from pathlib import Path


import gspread
from oauth2client.service_account import ServiceAccountCredentials


path = Path.home() / "Desktop/volunteer/WEC/Data Extraction & Transformation/Google Sheets"

#Define the scope of the application 
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name(path /'first_project_credentials.json', scope)

#Authorize the client sheet
client = gspread.authorize(creds)


#Get the instance of the spreadsheet
sheet = client.open("All Volunteers (Teachers, Tutors, Librarians, Special) from 2006 to Fall 2020_1212")

sheet_1 = sheet.get_worksheet(0)



