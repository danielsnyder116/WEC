# =============================================================================
#   More streamlined code that lets packages take care of tough stuff 
#   PyDrive is a wrapper package for google-api
# =============================================================================
#!pip install pydrive

#https://medium.com/analytics-vidhya/how-to-connect-google-drive-to-python-using-pydrive-9681b2a14f20


import os
import pandas as pd
from pathlib import Path

cwd = Path.home() / "Desktop/volunteer/WEC/Data Extraction & Transformation/Google Sheets"

os.chdir(cwd)

from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

#Authenticates google connection
g_auth = GoogleAuth()
g_auth.LocalWebserverAuth()


#Local Instance of Google Drive
drive = GoogleDrive(g_auth)


#See all files (excluding trashed) on drive
#Other queries: "title contains '^Copy' and trashed=false"
#q is query
files = drive.ListFile({'q':"trashed=false"}).GetList()

#List comprehension to get title attribute for all files
file_names = [i['title'] for i in files]










