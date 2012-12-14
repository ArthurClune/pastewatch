#!/bin/sh

DAEMON=/opt/pastewatch/pastewatch
DAEMON_OPTS="+RTS -N4 -k100m -T -RTS /etc/pastewatch.config"

case "$1" in
    start)
        start-stop-daemon --start --background --quiet --make-pidfile --pidfile /var/run/pastewatch.pid --exec $DAEMON -- $DAEMON_OPTS
    ;;
    stop)
        kill -9 `cat /var/run/pastewatch.pid`
    ;;
esac
