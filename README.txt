Pfam can be run locally if there is a properly setup config file in the PfamConfig/PfamWeb directory. The server is started up as follows:

perl pfamweb-run.pl PfamConfig/PfamWeb/pfamweb_maq.conf

The script will setup and write a new autogenerated config file in the same directory as the file supplied as an argument to pfamweb-run.pl.
