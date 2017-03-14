# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Drought.py
# Prototype version for datamining forecast data for prediction of cholera outbreaks due to periods of drought
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy
import datetime
import csv

#Drypoly = [None] * 15

mydate = datetime.date.today()

mydate = str(mydate)
#mydate = mydate.replace("-","")

arcpy.AddMessage(mydate)

template_file_location = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\'

template_file_name_input = template_file_location + 'RainTemplatePoint.shp'

Drydata_input_folder = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Drycode\\2015-12-16\\'# + mydate + '\\'

Output_folder = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Drycode\\Test\\'# + mydate + '/'

def convertcsv(myfile,outfile):
	#--first lets make a list of all of the fields in the table  
	fields = arcpy.ListFields(myfile) #was table  
	field_names = [field.name for field in fields]  
	
	with open(outfile,'wb') as f:  
		w = csv.writer(f)  
		#--write all field names to the output file  
		w.writerow(field_names)  
  
		#--now we make the search cursor that will iterate through the rows of the table  
		for row in arcpy.SearchCursor(myfile):  #was table
			field_vals = [row.getValue(field.name) for field in fields]  
			w.writerow(field_vals)  
		del row  
	#AddMessage

DryTemplatePoint_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\RainTemplatePoint.shp"


PolyOut_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Drycode\\Test\\PolyOut.shp"
DryTemplate_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\DryTemplate.shp"
	
for i in range(0,15):
	#arcpy.AddMessage(str(i))
	outputfile = Output_folder + 'DPoin' + str(i) + '.shp'
	inputDryfile = Drydata_input_folder + str(i) + '_dry.tif'
	DryTemplate_SpatialJoin = Output_folder + 'RPol' + str(i) + '.shp'
	#if i ==0:
	#	arcpy.AddMessage(str(template_file_name_input))
	#	arcpy.AddMessage(str(inputDryfile))
	#	arcpy.AddMessage(str(outputfile))
	arcpy.gp.ExtractValuesToPoints_sa(template_file_name_input, inputDryfile, outputfile)
	
	if i == 0:
		outputfile2 = outputfile.replace('\\','\\\\')
	#arcpy.AddMessage('What is ths: ' + outputfile)
		DryTemplate_shp = DryTemplate_shp.replace('\\','\\\\')
		DryTemplate_SpatialJoin = DryTemplate_SpatialJoin.replace('\\','\\\\')
	
	
	
	
	
	outcsvfile = Output_folder + str(i) + '_DryPoly.csv'
	#arcpy.AddMessage('CSV File: ' + DryTemplate_SpatialJoin)
	arcpy.AddMessage('CSV File: ' + outputfile)
	#convertcsv(DryTemplate_SpatialJoin,outcsvfile)
	convertcsv(outputfile,outcsvfile)
		#arcpy.SpatialJoin_analysis(DryTemplate_shp, outputfile, PolyOut_shp, jointext)
		#arcpy.SpatialJoin_analysis(DryTemplate_shp, outputfile2, DryTemplate_SpatialJoin, jointext)
		#arcpy.AddJoin_management (DryTemplate_shp, 'SLID', outputfile2, 'SLID')






