#!/bin/sh
### BEGIN INIT INFO
# Provides:          pastewatch
# Required-Start:    $network $remote_fs
# Required-Stop:     $network $remote_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Pastewatch
# Description:       Watch paste sites for stuff of interest
### END INIT INFO

# Author: Arthur Clune <arthur@clune.org>

PATH=/sbin:/usr/sbin:/bin:/usr/bin
DESC="Description of the service"
NAME=pastewatch
DAEMON=/usr/bin/$NAME
DAEMON_ARGS="+RTS -N2 -T -RTS /etc/pastewatch.conf"
PIDFILE=/var/run/$NAME.pid
SCRIPTNAME=/etc/init.d/$NAME
[ -x "$DAEMON" ] || exit 0

# Load the VERBOSE setting and other rcS variables
. /lib/init/vars.sh

# Define LSB log_* functions.
# Depend on lsb-base (>= 3.2-14) to ensure that this file is present
# and status_of_proc is working.
. /lib/lsb/init-functions
VERBOSE="yes"

do_start()
{
    start-stop-daemon --background  --start --quiet  --make-pidfile  --pidfile $PIDFILE --exec $DAEMON -- $DAEMON_ARGS \
        || return 2
}

do_stop()
{
    if [ ! -f /var/run/pastewatch.pid ];
        then
            return 1
        fi
    kill -9 `cat /var/run/pastewatch.pid`
    rm -f $PIDFILE
    return 0
}

case "$1" in
  start)
    [ "$VERBOSE" != no ] && log_daemon_msg "Starting $DESC" "$NAME"
    do_start
    case "$?" in
        0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
        2)   [ "$VERBOSE" != no ] && log_end_msg 1 ;;
    esac
    ;;
  stop)
    [ "$VERBOSE" != no ] && log_daemon_msg "Stopping $DESC" "$NAME"
    do_stop
    case "$?" in
        0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
        2)   [ "$VERBOSE" != no ] && log_end_msg 1 ;;
    esac
    ;;
  restart|force-reload)
    #
    # If the "reload" option is implemented then remove the
    # 'force-reload' alias
    #
    log_daemon_msg "Restarting $DESC" "$NAME"
    do_stop
    case "$?" in
      0|1)
        do_start
        case "$?" in
            0) log_end_msg 0 ;;
            1) log_end_msg 1 ;; # Old process is still running
            *) log_end_msg 1 ;; # Failed to start
        esac
        ;;
      *)
        # Failed to stop
        log_end_msg 1
        ;;
    esac
    ;;
  *)
    echo "Usage: $SCRIPTNAME {start|stop|status|restart|force-reload}" >&2
    exit 3
    ;;

esac
:
