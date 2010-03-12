#!/bin/sh

log=/lustre/scratch103/sanger/pg5/wiki_cronjob/Rfamwatchlist.`date '+%Y.%m.%d'`
errorlog=/lustre/scratch103/sanger/pg5/wiki_cronjob/errors.`date '+%Y.%m.%d'`


export PERL5LIB=/software/pfam/perl/lib/perl/5.8.4:/software/rfam/perl/lib/perl/5.8:/software/rfam/perl/share/perl/5.8.4:/software/rfam/perl/software/perl-5.8.8/lib/site_perl/5.8.8:/software/rfam/scripts/Modules:/software/pfam/bioperl_1.5.2:/software/pfam/Modules/PfamLib:/software/rfam/perl:/software/rfam/perl/lib/perl/5.8.4

export http_proxy=http://wwwcache.sanger.ac.uk:3128/


/software/rfam/bin/scrape_cronjob.pl -pages all -update -changes 1 > $log  2> $errorlog


mail -s "wiki cronjob" jd7 pg5 agb < $log

/software/rfam/bin/mysqldump -h pfamdb2a -u pfam -pmafp1 -P 3301 rfam_9_1 wikitext     | /software/rfam/bin/mysql -h pfamdb1 -u pfamwebadmin -pmafpwa rfam_9_1

cp -f $log /lustre/scratch103/sanger/pg5/wiki_cronjob/Rfamwatchlist
cp -f $errorlog /lustre/scratch103/sanger/pg5/wiki_cronjob/errors

rm -rf $log 
rm -rf $errorlog
