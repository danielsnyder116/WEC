# =============================================================================
#           CUSTOM MODULE WITH ALL NEEDED LIBRARIES AND FUNCTIONS
# =============================================================================


import pandas as pd
from pathlib import Path

import gspread
from oauth2client.service_account import ServiceAccountCredentials
from googleapiclient.discovery import build

#TAKES CARE OF AUTHORIZATION AND CONNECTION
#------------------------------------------
path = Path.home() / "Desktop/volunteer/WEC/Data Extraction & Transformation/Google Sheets"
cred_path = Path.home() / "Desktop/volunteer"

#Define the scope of the application 
scope = ['https://www.googleapis.com/auth/drive',
         'https://www.googleapis.com/auth/spreadsheets'] #, 
         #'https://www.googleapis.com/drive/v3/files',
         #'https://spreadsheets.google.com/feeds']

creds = ServiceAccountCredentials.from_json_keyfile_name(cred_path /"service_account_creds.json", scope)

#Authorize the client sheet
client = gspread.authorize(creds)


#CUSTOM FUNCTIONS
def sheet_to_df(workbook_name, sheet_name_index):
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
    








