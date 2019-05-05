#!/usr/bin/env python

"""Python script to automate the Introduction to QGIS Practical - Merging rasters, format conversion and zonal statistics. 
Script to convert bioclim temperature and rainfall data and CORINE landcover files into a shared projection and resolution 
(BNG 2km grid) and then extract mean climatic values within land cover classes

Code is PEP8 compliant: https://www.python.org/dev/peps/pep-0008"""

__author__ = 'Saul Moore (sm5911@imperial.ac.uk)'
__version__ = '0.0.1'

# Imports:
import numpy # 'numpy' is a powerful N-dimensional array object, enabling the user to load data types, such as rasters, as arrays and provide no data masking
import gdal  # 'gdal' package can handle raster data files
import os # 'os' module provides the ability to interface with the underlying operating system that Python is running on
import csv

# define the two variable prefixes
bioclimVars = ['bio1', 'bio12']

# A) Use GDAL command line tools to convert the bioclim variables and
#    the CORINE data into a 2km UK grid

for eachVar in bioclimVars:

    # - merge the two panes
    # - NOTE: using ('a ' 'b ' 'c') = 'a b c' to wrap long lines cleanly
    cmd = ('gdal_merge.py -n -32768 -a_nodata -32768 -of GTiff ' # 'gdal_merge.py' calls the following .tif files, and combines adjacent rasters together 
           '-o ../Data/{f}_merge.tif ../Data/{f}_15.tif ../Data/{f}_16.tif')
    os.system(cmd.format(f=eachVar)) # 'os.system' calls the terminal from within python, running a bash shell
    # f=eachVar replaces the '{f}' with each named prefix as python loops over the files

    # - cut out a UK block
    cmd = ('gdal_translate -projwin -12.0 60.0 4.0 48.0 -of GTiff ' # 'gdal_translate' is used to change the dimension/scale of the rasters (size of map)
           '../Data/{f}_merge.tif ../Data/{f}_UK.tif')
    os.system(cmd.format(f=eachVar))

    # - reproject and resample into a 2km grid
    cmd = ('gdalwarp -s_srs EPSG:4326 -t_srs EPSG:27700 -r cubic -of GTiff '
           '-dstnodata -32768 -tr 2000 2000 -te -220000 -10000 680000 1080000 '
           '../Data/{f}_UK.tif ../Data/{f}_UK_BNG.tif') # 'gdalwarp' is used to change the projection
    os.system(cmd.format(f=eachVar))

# Use GDAL command line to convert the landcover data
# - cut out a UK block
cmd = ('gdal_translate -projwin 2800000.0 4200000.0 4000000.0 3000000.0 '
       '-of GTiff ../Data/g250_06.tif ../Data/g250_06_UK.tif')
os.system(cmd)

# - reproject and resample into a 2km grid
cmd = ('gdalwarp -overwrite -s_srs EPSG:3035 -t_srs EPSG:27700 -r near '
       '-of GTiff -tr 2000 2000 -te -220000 -10000 680000 1080000 '
       '../Data/g250_06_UK.tif ../Data/G250_06_UK_BNG.tif')
os.system(cmd)

# tidy up the intermediate files
intermediates = ['../Data/g250_06_UK.tif', '../Data/bio1_merge.tif', '../Data/bio1_UK.tif',
                 '../Data/bio12_merge.tif', '../Data/bio12_UK.tif']
for x in intermediates:
    os.remove(x)

# B) Use python to get zonal stats
#    First define a couple of functions


def zonalStats(layer, classMap, classCodes):
    """ Loads a bioclim layer and runs the mean extraction
    within each landcover class.
    """
    # load the data into an array
    bioclimLayer = gdal.Open('../Data/{f}_UK_BNG.tif'.format(f=layer))
    bcData = bioclimLayer.ReadAsArray()

    # get and mask the no data values
    bioclimBand = bioclimLayer.GetRasterBand(1)
    noData = bioclimBand.GetNoDataValue()
    bcData = numpy.ma.masked_where(bcData == noData, bcData)

    # run the mean extraction across it
    means = [meanExtract(lcc, classMap, bcData) for lcc in classCodes]

    # close the layer
    bioclimLayer = None

    return means


def meanExtract(lcc, classMap, data):
    """ Extracts the values from the bioclim layer at a given landcover class
    and then finds the mean. Assumes no data values are already masked.
    """
    # get a paired list of indices in the landcover that
    # match the input value
    locs = numpy.where(classMap == lcc)

    # extract those indices from the bioclim data
    vals = data[locs]

    return round(vals.mean(), 2)

# Load the landcover data into an array
landcover = gdal.Open('../Data/G250_06_UK_BNG.tif')
lcData = landcover.ReadAsArray()

# find the set of landcover classes
lcClasses, lcFreq = numpy.unique(lcData, return_counts=True)

# get a list of values for each variable
zonalMeans = [zonalStats(x, lcData, lcClasses) for x in bioclimVars]

# create a list of  columns to export (LCC codes, bio1 mean, bio12 mean)
columns = [list(lcClasses)] + zonalMeans
# unpack into rows of data
rows = zip(*columns)

# write the rows out to a file
with open('../Results/zonalstats.csv', 'wb') as csvfile:
    c = csv.writer(csvfile)
    c.writerow(['LCC', 'bio1', 'bio12'])
    for x in rows:
        c.writerow(list(x))

