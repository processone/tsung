#!/bin/sh

HOST=`hostname -s`
NAME=idx-tsunami
CONTROLLER=tsunami_controller
RECORDER=tsunami_recorder
TSUNAMIPATH=%prefix%/erlang/tsunami-%VSN%/ebin
CONF_OPT="-tsunami_controller config_file \"%prefix%/etc/idx-tsunami.xml\""
BOOT_OPT="-boot_var TSUNAMIPATH %prefix%/erlang -boot %prefix%/bin/tsunami_controller"
REC_BOOT_OPT="-boot_var TSUNAMIPATH %prefix%/erlang -boot %prefix%/bin/tsunami_recorder"
ERL_OPTS="-rsh ssh +A 1 +Mea r10b -shared"
COOKIE='tsunami'

stop() {
    erl $ERL_OPTS -noshell  -sname killer -setcookie $COOKIE -pa $TSUNAMIPATH -s tsunami_controller stop_all $HOST -s init stop
}

stop_recorder() {
    erl $ERL_OPTS -noshell  -sname killer -setcookie $COOKIE -pa $TSUNAMIPATH -s tsunami_recorder stop_all $HOST -s init stop
}

start() {
    erl $ERL_OPTS -detached -sname $CONTROLLER -setcookie $COOKIE  $BOOT_OPT $CONF_OPT
}

recorder() {
    erl $ERL_OPTS -detached -sname $RECORDER -setcookie $COOKIE  $REC_BOOT_OPT $CONF_OPT
}

debug() {
    erl $ERL_OPTS -sname $CONTROLLER -setcookie $COOKIE  $BOOT_OPT $CONF_OPT
}

status() {
    echo "status: not yet implemented"
}

usage() {
    prog=`basename $1`
    echo "$prog start|stop|restart|debug|status|recorder|stop_recorder"
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

    recorder)
        recorder
        ;;

    debug)
        debug
        ;;

    stop)
        stop
        ;;
    stop_recorder)
        stop_recorder
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
