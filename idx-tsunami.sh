#!/bin/sh

HOST=`hostname -s`
NAME=idx-tsunami
CONTROLLER=tsunami_controller
TSUNAMIPATH=%prefix%/erlang/tsunami-%VSN%/ebin
CONF_OPT="-tsunami_controller config_file \"%prefix%/etc/idx-tsunami.xml\""
BOOT_OPT="-boot_var TSUNAMIPATH %prefix%/erlang -boot %prefix%/bin/tsunami_controller"
ERL_OPTS="-rsh ssh +A 1 +Mea r10b -shared"
COOKIE='tsunami'

stop() {
    erl $ERL_OPTS -noshell  -sname killer -setcookie $COOKIE -pa $TSUNAMIPATH -s tsunami_controller stop_all $HOST -s init stop
}

start() {
    erl $ERL_OPTS -detached -sname $CONTROLLER -setcookie $COOKIE  $BOOT_OPT $CONF_OPT
}

debug() {
    erl $ERL_OPTS -sname $CONTROLLER -setcookie $COOKIE  $BOOT_OPT $CONF_OPT
}

status() {
    echo "status: not yet implemented"
}

usage() {
    prog=`basename $1`
    echo "$prog start|stop|restart|debug|status"
}

while getopts ":f:" Option
do
    case $Option in
        f) CONF_OPT="-tsunami_controller config_file \"$OPTARG\" ";;
		*) usage ;;
    esac
done	
shift $(($OPTIND - 1))

case $1 in
    start)
        start
        ;;

    debug)
        debug
        ;;

    stop)
        stop
        ;;

    status)
        status
        ;;

    restart)
        stop
        start
        ;;

    *)
        usage $0
        ;;
esac
