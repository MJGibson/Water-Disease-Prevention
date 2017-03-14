# -*- coding: utf-8 -*-
# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Rain.py
# Prototype version for datamining forecast data for prediction of cholera outbreaks due to periods of rain
# ---------------------------------------------------------------------------


# Import arcpy module
import arcpy
import datetime
import csv

#rainpoly = [None] * 15

mydate = datetime.date.today()

mydate = str(mydate)
#mydate = mydate.replace("-","")

arcpy.AddMessage(mydate)

template_file_location = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\'

template_file_name_input = template_file_location + 'RainTemplatePoint.shp'

raindata_input_folder = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Raincode\\2015-12-16\\'# + mydate + '\\'

Output_folder = 'C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Raincode\\Test\\'# + mydate + '/'

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

RainTemplatePoint_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\RainTemplatePoint.shp"
#RainTemplate_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\RainTemplate.shp"


v0_rain_tif = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Raincode\\20170305\\0_rain.tif"
Rainpoly_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Raincode\\Test\\Rainpoly.shp"

# Process: Extract Values to Points
#arcpy.gp.ExtractValuesToPoints_sa(RainTemplatePoint_shp, v0_rain_tif, Rainpoly_shp, "NONE", "VALUE_ONLY")

PolyOut_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Raincode\\Test\\PolyOut.shp"
RainTemplate_shp = "C:\\Users\\Barry Evans\\Documents\\Work\\Barry\\Hackathon\\Kenya\\Animate\\RainTemplate.shp"
	
for i in range(0,15):
	#arcpy.AddMessage(str(i))
	outputfile = Output_folder + 'RPoin' + str(i) + '.shp'
	inputrainfile = raindata_input_folder + str(i) + '_rain.tif'
	RainTemplate_SpatialJoin = Output_folder + 'RPol' + str(i) + '.shp'
	#if i ==0:
	#	arcpy.AddMessage(str(template_file_name_input))
	#	arcpy.AddMessage(str(inputrainfile))
	#	arcpy.AddMessage(str(outputfile))
	arcpy.gp.ExtractValuesToPoints_sa(template_file_name_input, inputrainfile, outputfile)
	
	if i == 0:
		outputfile2 = outputfile.replace('\\','\\\\')
	#arcpy.AddMessage('What is ths: ' + outputfile)
		RainTemplate_shp = RainTemplate_shp.replace('\\','\\\\')
		RainTemplate_SpatialJoin = RainTemplate_SpatialJoin.replace('\\','\\\\')
	

	
	
	
	#if i == 0:
		
		
		#arcpy.AddMessage('arcpy.SpatialJoin_analysis(RainTemplate_shp, outputfile2, RainTemplate_SpatialJoin, ' + jointext +')' )
	#arcpy.SpatialJoin_analysis(RainTemplate_shp, outputfile2, RainTemplate_SpatialJoin)
	
	outcsvfile = Output_folder + str(i) + '_Poly.csv'
	#arcpy.AddMessage('CSV File: ' + RainTemplate_SpatialJoin)
	arcpy.AddMessage('CSV File: ' + outputfile)
	#convertcsv(RainTemplate_SpatialJoin,outcsvfile)
	convertcsv(outputfile,outcsvfile)
		





