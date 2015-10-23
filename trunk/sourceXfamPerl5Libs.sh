#!/bin/bash

xfamDir=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
echo "$xfamDir"

subDirs="iPfamLib PfamWeb Rfam Rfam/Schemata XfamWeb PfamDatabaseModel DasViewer 
PfamSchemata iPfamWeb iPfamClayDiagram PfamBackend PfamScripts iPfamScripts
EbiRestClient Bio-Easel WikiApp PfamTests Dfam PfamConfig PfamLib PfamAlyzer
DasSources PfamBase PfamBase/lib"

for dir in $subDirs
do
	echo "Adding $xfamDir/$dir:${PERL5LIB}"
	export PERL5LIB="$xfamDir/$dir:${PERL5LIB}"
done

