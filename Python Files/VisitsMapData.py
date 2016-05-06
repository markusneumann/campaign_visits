#This code was originally written by Nathan Yau, I appropriated it for the campaign visits data
#Source: http://flowingdata.com/2009/11/12/how-to-make-a-us-county-thematic-map-using-free-tools/

##Python version used: 2.7.6
##BeautifulSoup version used: 3
##BeautifulSoup version 4.3.2. will NOT WORK because it has a slightly different syntax

#The script is currently set to output the Democratic visits in 2012 (figure 4.2 in the paper)
#To change this, change the value 12 in 'rate = float( row[12].strip() )' to the respective column in 'MAPRESULTS.csv'
#(note that in Python, '0' is the first column)
#Additionally, for outputting a Republican map, remove the '#' at the respective color vector
#and add one at the one for Democrats

#To make the map, open a console and type 'python VisitsMapData.py > MapYearParty.svg
#This will only work if the Python directory is added as environmental variable in ~Path (at least for Windows)
#Otherwise, this code file as well as the 'MAPRESULTS.csv' file both need to be in the Python directory
#Then navigate to the python directory in the console and type the above
#Vector graphics can be converted to PDFs (for LaTeX) with various tools, I used Inkscape

import csv
from BeautifulSoup import BeautifulSoup

# Read in unemployment rates
unemployment = {}
min_value = 100; max_value = 0
reader = csv.reader(open('MAPRESULTS.csv'), delimiter=",")
for row in reader:
    try:
        full_fips = row[0]
        rate = float( row[12].strip() )
        unemployment[full_fips] = rate
    except:
        pass


# Load the SVG map
svg = open('counties.svg', 'r').read()

# Load into Beautiful Soup
soup = BeautifulSoup(svg, selfClosingTags=['defs','sodipodi:namedview'])

# Find counties
paths = soup.findAll('path')

# Map colors
#Republicans
#colors = ["#F1EEF6", "#D89396", "#B54848", "#AD393F", "#A52730", "#910E10", "#8E0004"]
#Democrats
colors = ["#F1EEF6", "#7B83AF", "#5E6999", "#4F538E", "#3B3D87", "#252882", "#171C7A"]

# County style
path_style = 'font-size:12px;fill-rule:nonzero;stroke:#FFFFFF;stroke-opacity:1;stroke-width:0.1;stroke-miterlimit:4;stroke-dasharray:none;stroke-linecap:butt;marker-start:none;stroke-linejoin:bevel;fill:'

# Color the counties based on visits
for p in paths:

    if p['id'] not in ["State_Lines", "separator"]:
        try:
            rate = unemployment[p['id']]
        except:
            continue


        if rate > 20:
            color_class = 6
        elif rate > 10:
            color_class = 5
        elif rate > 5:
            color_class = 4
        elif rate > 2:
            color_class = 3
        elif rate > 1:
            color_class = 2
        elif rate > 0:
            color_class = 1
        else:
            color_class = 0


        color = colors[color_class]
        p['style'] = path_style + color

print soup.prettify()