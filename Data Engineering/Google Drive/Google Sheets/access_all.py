# =============================================================================
#   More streamlined code that lets packages take care of tough stuff 
#   PyDrive is a wrapper package for google-api
# =============================================================================
#!pip install pydrive

#https://medium.com/analytics-vidhya/how-to-connect-google-drive-to-python-using-pydrive-9681b2a14f20
#https://github.com/googlearchive/PyDrive

import os
import pandas as pd
from pathlib import Path

from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

cwd = Path.home() / "Desktop/WEC/Code"

os.chdir(cwd)

#list(cwd.glob("*"))

#Authenticates google connection
g_auth = GoogleAuth()
g_auth.LocalWebserverAuth()


#Local Instance of Google Drive
drive = GoogleDrive(g_auth)


#See all files (excluding trashed) on drive
#Other queries: "title contains '^Copy' and trashed=false"
#q is query
#'root'for all in My Drive
#wec_drive_id = '1qGEHvhuxZKhRXq6CntpSkDXgOx6mhgcl'

files = drive.ListFile({'q':"'0AO0qQjqQb1rKUk9PVA' in parents and trashed=false",
                        'corpora':'teamDrive', 
                        'teamDriveId':'0AO0qQjqQb1rKUk9PVA',
                        'includeTeamDriveItems':'True',
                        'supportsAllDrives':'True'}).GetList()




#List comprehension to get title attribute for all files
file_names = [i['title'] for i in files]


#How to actually read in a file

downloaded_file = drive.CreateFile({'id':'1aZVn_-lvnz4v0agQNfK7M3VzwuQyq8wtG8um46gwjx8'})
downloaded_file.GetContentFile('test.csv')






