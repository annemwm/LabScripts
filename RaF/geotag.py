from geopy.geocoders import Nominatim
from geopy.exc import GeocoderServiceError
from time import sleep
import os
import glob
import csv
import copy
#import socket

#socket.setdefaulttimeout(30000)

geolocator = Nominatim()

os.chdir('/Users/dhlabadmin1/Desktop/work')
files=glob.glob('*.txt')

reg_dict = {}
reg_occ = {}

def country_extract(textfile):
    countrylist = []
    with open('%s' % textfile, 'r') as readfile:
        for line in readfile:
            try:
                location = geolocator.geocode('%s' % line, language='en')
                sleep(1)
                location = location.address.split(',')
                location = location[-1].strip()
                countrylist.append(location)
            except AttributeError:
                print("attr error: %s%s\n" % (line, textfile))              
    return(countrylist)

def region_counts(countrylist):
    temp_occ=copy.deepcopy(reg_occ)
    for country in countrylist:
        try:
            temp_occ[reg_dict[country]] = temp_occ[reg_dict[country]]+1
        except KeyError:
            print("Key: %s" % country)
    return(temp_occ)

def dic_setup():
    with open('Region_Tags.txt','r') as file:
        for line in file:
            line = line.rstrip('\n')
            line = line.split('\t')
            region = line[0]
            reg_occ[region]=0
            for country in line[1:]:
                reg_dict[country.strip()]=region

def main():
    dic_setup()
    
    with open('failures.txt', 'w') as fail:
        with open('geonum.csv', 'w') as csv_file:
            keys=[k for k in reg_occ.keys()]
            writer = csv.writer(csv_file, delimiter=',')
            writer.writerow(['CHAR_ID']+keys)
            for file in files:
                try:
                    clist = country_extract(file)
                    counts = region_counts(clist)
                    in_order = []
                    for key in keys:
                        in_order.append(counts[key])
                    writer.writerow([file.replace('.txt','')]+in_order)
                except:
                    print("error: %s\n" % file)
                    fail.write("%s\n" % file)

                    
def failures():
    dic_setup()
    with open('failures2.txt', 'w') as failing:
        with open('failures.txt', 'r') as failed:
            with open('geonum2.csv', 'a+') as csv_file:
                writer = csv.writer(csv_file, delimiter=',')
                reader = csv.reader(csv_file)
                append = csv_file.tell()
                csv_file.seek(0)
                headers = next(reader)
                headers = headers[0].replace('"','').split('\t')
    
                fails = []
                for file in failed:
                    fails.append(file.replace('\n',''))
                for fail in fails:
                    try:
                        flist = country_extract(fail)
                        fcounts = region_counts(flist)
                        in_order = []
                        for key in headers:
                            in_order.append(fcounts[key])
                        writer.writerow([fail.replace('.txt','')]+in_order)
                    except:                    
                        print("error: %s\n" % fail)
                        failing.write("%s\n" % fail)


#main()
failures()

    

#print(region_counts(country_extract(files[15])))
#print(reg_occ)
#print(reg_dict[country_extract(files[15])[1]])
