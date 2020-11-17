#!/usr/local/bin/bash

if [ $# != 1 ] || [ ! -f $1 ] #Check a list is supplied and exists
then
    echo "You need to supply a list of families on the command line. Eg $0 list"
    exit 1
fi

mkdir -p Done   #Make the Done dir if it doesn't exist

i=1
for fam in `cat $1`
do 
    echo -e "\n\n\nSeed $i"                           
    echo -n "$fam "                                   #Print family accession  
    grep ^ID $fam/DESC                                #Print family id
    grep -v deleted $fam/seed_surgery.log             #Print all the changed seqs 
    echo -ne "\nEdited: "                    
    grep -v deleted $fam/seed_surgery.log | wc -l     #Print number of changed seqs
    echo -ne "Deleted: "
    grep deleted $fam/seed_surgery.log | wc -l        #Print all the deleted seqs 
    belvu $fam/SEED                                   #Open in belvu
    echo "Moving $fam to Done direcotory"
    mv $fam Done                                      #Move to Done dir
    ((i++))
done





