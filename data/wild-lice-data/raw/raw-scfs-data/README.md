# Notes on Cleaning these Data

These data are all the different sources of data compiled by Stephanie Peacock for her original analysis. 

**Notes from Steph upon delivery:**

* The folder 2016 Ecology Data paper contains the data compilation I did back in 2015 to pull in earlier years. I tried to document this carefully, but it was (is) messy! From the ReadMe in 2016 Ecology Data paper > Data support files: 
  * *“In November 2015, Steph checked the Morton monitoring data against various datasets provided my Brendan and Marty.  The data from Brendan and Marty had more detail, in some cases, than the data that Steph had on file from the Peacock et al. (2013 Ecol Appl) paper (which had been updated by Steph since 2010 to including more recent data as it became available). Steph went through and switched to using the more detailed data where it made sense. More detailed data were not available for 2001, and we are still not sure whether the louse data for that year are all Leps, or if they include Caligus.  Marty has told me (10Dec2015) that the "data for impact.xls" that match the 2001 data are from"*
* It seems there was a bit of uncertainty around which data to include in 2001 and different sources of the data via Marty, Brendan, or directly from Alex) didn’t always match up. Not awesome, but that’s life. 
* I attached the relevant files “data for impact.xls” (also in Alex from MK folder in zip file) and "pink study 2001.xls” (also in Alex from MK > Not used)
  * The "pink study 2001.xls” seems to have more detail but I can’t make sense of how it relates to what’s included. Seems like I decided to use the “data for impact.xls” instead, but if you want to dig in again I am supportive!

## Cleaning Method 

Since the only problem year is 2001, I searched for all the files with 2001 in the title. That returned the following files: 

* `Morton_sea_lice_monitoring_lice_data_2001-2019.csv`
* `Morton_sea_lice_monitoring_site_data_2001-2019.csv`
* `pinkstudy 2001.xls`

I began by going through each file in the order listed here, to try and get more information about the sampling that happened in 2001. 

### `Morton_sea_lice_monitoring_lice_data_2001-2019.csv`

In this data, there are no non-NA observations for 2001, in explicitly lep-related columns. All data are just in the chalimus data column. 

### `Morton_sea_lice_monitoring_site_data_2001-2019.csv`

This is obviously not lice data related. 

### `pinkstudy 2001.xls`

This seems to be the data I want, there's non-NA leps specific data which is good. I'll compare with the data I'm using now to see if it matches up in any meaningful way. 
  