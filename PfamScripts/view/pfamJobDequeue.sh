#!/bin/sh
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

#You will need to configure this for your system.
#General

# Pfam config
export PFAMROOT=/software/pfam
export PATH=$PFAMROOT/PfamBackend/scripts:$PFAMWEBROOT/src/hmmer-3.0b3/bin:$PFAMWEBROOT/src/wise2.2.0/src/bin:/sbin:$PFAMROOT/subversion/bin:$PFAMROOT/bin:$PFAMROOT/perl/bin:/software/bin:$PFAMROOT/Scripts/PfamH3Scripts/view:$PFAMROOT/bin:$PATH
export PERL5LIB=$PFAMROOT/Modules/PfamSchemata:$PFAMROOT/Modules/PfamLib:$PFAMROOT/perl/share/perl/5.8:$PFAMROOT/perl/lib/site_perl/5.8.8/x86_64-linux-thread-multi:$PFAMROOT/perl/share/perl/5.8.8:$PFAMROOT/perl/lib/5.8.8:$PFAMROOT/perl:$PFAMROOT/perl/lib/site_perl/5.8.8:$PFAMROOT/perl/lib/perl/5.8.4:$PFAMROOT/perl/share/perl/5.8.4:/software/pubseq/PerlModules:/software/pubseq/PerlModules/Modules:$PFAMROOT/bioperl:$PFAMROOT/src/GraphViz:/pfam/db/miRNA/scripts/Modules:$PFAMROOT/Modules/iPfamLib:/pfam/db/miRNA/scripts/Modules:$PFAMROOT/Modules/iPfamLib

export PFAM_CONFIG=$PFAMROOT/Conf/PfamConfigWTSI/PfamRepos/pfam_code.conf 

#Put the stuff in place where we want the 
#These will need to be changed
export PFAM_GRAPHVIZ_MOD=/lustre/pfam/db/Pfam/software/GraphViz
export BIOPERL_DIR=$PFAMROOT/bioperl 

# set default project for new farm
export LSB_DEFAULTPROJECT=pfam

# Add Pfam places to the path
export PERL5LIB=$BIOPERL_DIR:$PFAM_GRAPHVIZ_MOD:$PERL5LIB 

# Get LSF configuration no matter where we run
. /usr/local/lsf/conf/profile.lsf

export DEBUG=0;


#Config for this shell script
DAEMON=$PFAMROOT/Scripts/PfamScripts/view/pfamJobDequeue.pl 
NAME=pfamJobDequeue
DESC="Pfam view process jobs daemon"


#End of config
test -f $DAEMON || exit 0


export PIDDIR=/var/lock

set -e
case "$1" in
  start)
 echo -n "Starting $DESC: "
 start-stop-daemon --start --verbose --pidfile ${PIDDIR}/${NAME}.pid \
  -c pfam-pipe  --exec /usr/local/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  stop)
 echo -n "Stopping $DESC: "
 
 start-stop-daemon --stop --verbose --pidfile ${PIDDIR}/${NAME}.pid \
   -c pfam-pipe --exec /usr/local/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  restart|force-reload)
 echo -n "Restarting $DESC: "
 start-stop-daemon --stop --verbose -c pfam-pipe --pidfile \
  ${PIDDIR}/${NAME}.pid  --exec /usr/local/bin/perl --startas $DAEMON
 sleep 1
 start-stop-daemon --start --verbose -c pfam-pipe --pidfile ${PIDDIR}/${NAME}.pid \
   --exec /usr/local/bin/perl --startas  $DAEMON
 echo "$NAME."
 ;;
  *)
 N=/etc/init.d/$NAME
 # echo "Usage: $N {start|stop|restart|reload|force-reload}" >&2
 echo "Usage: $N {start|stop|restart|force-reload}" >&2
 exit 1
 ;;
esac

exit 0
