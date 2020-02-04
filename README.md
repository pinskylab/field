This repository contains scripts that we use in the field every year.  

[Tissue Collection Protocol](https://docs.google.com/document/d/1DhmiVXxVyic64oMsA5WZfsOiwRqU6oV0IFdTb5PSNaM/edit?usp=sharing)  

[data entry file](https://drive.google.com/open?id=1rIM4YCmJGv5awoQiPno-2LyGmtE3tMHphcM6reFjjJs)

[Maps](https://github.com/pinskylab/Phils_GIS_R) during the field season and all of the GIS info is kept in the [Phils_GIS_R](https://github.com/pinskylab/Phils_GIS_R) repository.

**Survey data** after the field season is kept in the [`leyteBuildDB`](https://github.com/pinskylab/leyteBuildDB) repository in the data folder. 

The "weird things that happen in the field" is kept in the [clownfish-group-decisions](https://github.com/pinskylab/clownfish_group_decisions/blob/master/docs/Clownfish_data_collection_and_database_notes.md) repo.

[data](data) contains the data for the field season.  This should be transferred to the Philippines storage folder on amarel and deleted for a clean slate before the start of the next field season.  

[plots](plots) contains some maps we made of Albuera for our letter regarding the Cabatoan gravel project.  

[protocols](protocols) contains the 2017 collection protocol that is no longer used and protocols for exchanging hobos and using sim cards.  

[scripts](scripts) contains scripts in order to import data, prepare it for making maps, and check the data for errors.  

# Each night while in the field
1. Rinse electronics with fresh water, soak in a tub of fresh water if possible.  
2. Replace sea water with ethanol in the fin clip tubes.  
3. 1 person transfer the day's photos onto the photo usb hard drive.  
4. 1 person transfer GPS gpx files onto laptop and push to the [github](data) field repo/data folder.  
5. 1 person sync the pit scanner with laptop and push to the [github](data) field repo/data folder.  
6. All available people enter data into the google spreadsheet, sync a copy with the [github](data) field repo/data folder once everyone is done for the night.  
7. Run the 01checkxl.Rmd script to find common errors that occur during data entry.  
8. Run the 02trimgpx.Rmd script to trim the gpx files down to only the times during survey.  
9. Run the 03anemstoqgis.Rmd script to plot the anemones on the QGIS map.

