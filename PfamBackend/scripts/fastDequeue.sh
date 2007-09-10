#!/bin/sh

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/Users/rdf/Work/PfamWebsite/PfamBackend/scripts/fastDequeue.pl
NAME=fastDequeue
DESC="Fast queue daemon"

test -f /home/pfamweb/.bashrc || exit 0
. /home/pfamweb/.bashrc

test -f $DAEMON || exit 0

set -e
case "$1" in
  start)
 echo -n "Starting $DESC: "
 start-stop-daemon --start --quiet --pidfile /home/pfamweb/var/run/$NAME.pid \
  --exec /usr/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  stop)
 echo -n "Stopping $DESC: "
 # --quiet
 start-stop-daemon --stop --signal 15 --pidfile /home/pfamweb/var/run/$NAME.pid \
  --exec /usr/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  restart|force-reload)
 echo -n "Restarting $DESC: "
 start-stop-daemon --stop --quiet --pidfile \
  /home/pfamweb/var/run/$NAME.pid --exec /usr/bin/perl --startas $DAEMON
 sleep 1
 start-stop-daemon --start --quiet --pidfile \
  /home/pfamweb/var/run/$NAME.pid --exec /usr/bin/perl --startas $DAEMON
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
