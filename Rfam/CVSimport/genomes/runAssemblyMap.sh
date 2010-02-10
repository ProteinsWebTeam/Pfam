#!/bin/sh

#this parses the list of databases and coordinate system names and versions I have stored in file CoordSystem.
#this file was generated using: perl ./getCoordSystem2.pl > CoordSystem

#echo enter file name
dir=/lustre/pfam/rfam/Users/jd7/Rfam_builds/RELEASE_10.0/mkrelease
fname=/lustre/pfam/rfam/Users/jd7/Rfam_builds/RELEASE_10.0/mkrelease/ENS_56_dblist.coordmapping

#read $fname
exec<$fname
while read line;
do
echo $line | { IFS=" " read a b c d ; perl  $dir/assembly_mappingV3.pl -all -dbname $a  -dbuser anonymous -dbhost ensembldb.ensembl.org -dbport 5306 -coord_system_version $c -coord_system_name $b -lowest_rank $d -outfile $dir/$a.out2 ;} 

done


#    exec<$fname
#    value=0;
#    while read line;
#    do
#    echo $line;
#    done


