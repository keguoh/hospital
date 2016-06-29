import pyodbc as po
import numpy as np
import matplotlib.pyplot as plt

# DBfile = 'C:/Users/huangke/Dropbox/Research/3RD PROJECT/HomeHospital/database/April2004/April2004.mdb'
DBfile = '/Users/keguoh/GitHub/hospital/HomeHospital/database/April2004/April2004.mdb'
conn = po.connect('DRIVER={Microsoft Access Driver (*.mdb)};DBQ='+DBfile)
#use below conn if using with Access 2007, 2010 .accdb file
#conn = pyodbc.connect(r'Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ='+DBfile)

cursor = conn.cursor()

cursor.execute("select patient_id, entry_day, entry_time from visits")

rows = cursor.fetchall()

days = []
for row in rows:
    days.append(row.entry_day)
 
##print np.max(days) - np.min(days) + 1
##print len(days)
##
##plt.hist(days, bins=30)
##plt.title("Gaussian Histogram")
##plt.xlabel("Value")
##plt.ylabel("Frequency")
##plt.show()

seconds = []
for row in rows:
    seconds.append(row.entry_time)

plt.hist(seconds, bins=30*24)
plt.title("Gaussian Histogram")
plt.xlabel("Value")
plt.ylabel("Frequency")
plt.show()
