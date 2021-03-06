# =============================================================================
#           ACCESSSING GOOGLE SHEETS VIA PYTHON
# =============================================================================

import pandas as pd
from pathlib import Path

import gspread
from oauth2client.service_account import ServiceAccountCredentials


path = Path.home() / "Desktop/volunteer/WEC/Data Extraction & Transformation/Google Sheets"

#Define the scope of the application 
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive',
         'https://www.googleapis.com/auth/spreadsheets']

creds = ServiceAccountCredentials.from_json_keyfile_name(path /'first_project_credentials.json', scope)

#Authorize the client sheet
client = gspread.authorize(creds)


def to_df(workbook_name, sheet_name_index):
    """This function takes in the relevant sheet/workbook information and converts it to a pandas
        dataframe. This will save code space and make it cleaner. Outputs a dataframe with proper columns.
        
        Inputs: name of relevant sheet/workbook, name OR index of relevant sheet
        
    """
    
    sheet = client.open(workbook_name)
    
    if isinstance(sheet_name_index, int):
        indiv_sheet = sheet.get_worksheet(sheet_name_index)
            
    else:
        indiv_sheet = sheet.worksheet(sheet_name_index)
        
    #Get all the actual data
    data = indiv_sheet.get_all_values()
    
    #Way to get the header row #pop allows you to slice by index using google sheets
    header = data.pop(0)
    
    
    df = pd.DataFrame(data, columns=header)
    
    return df
    



df = to_df('All Volunteers (Teachers, Tutors, Librarians, Special) from 2006 to Fall 2020_1212', 0)

#https://bookdown.org/rdpeng/RProgDA/error-handling-and-generation.html
#https://adv-r.hadley.nz/oo.html














