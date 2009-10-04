#! /bin/bash
set -e

rm -rf /tmp/test074
mkdir -p /tmp/test074

v0=/tmp/test074/sto
v1=/tmp/test074/sto.v1
v2=/tmp/test074/sto.v2

pid=""

logf=/tmp/test074/log
sb=$PWD

# invoke this script from its directory or its parent

if ! test -x $sb/sbs
then sb=${sb%/*}
fi

if ! test -x $sb/sbs
then echo "$sb/sbs: not exectuable"
     exit 1
fi

# put this in the environment to pass it to `test-074.rc'

export SBTEST_SB=${sb}

PATH=${sb}/client:$PATH
bootf=${sb}/test/test-074.scm

start() {
  $sb/sbs $bootf -e "$1" -e '(bd)' > $logf &
  #mrs -image /tmp/cache.fasl/sb.fas 
  # there is a miss window here, but let's hope it doesn't happen
  pid=$!
  sleep 8
}

stop() {
  kill -INT $pid
  sleep 1
  pid=""
}

trap 'if ! test -z "$pid" ; then kill -INT $pid ; fi' EXIT

vacuum() {
  kill -HUP $pid
  sleep 1
}

# ----------------------------------------------------------------------
# Volume 0 -- Initial creation and population
# ----------------------------------------------------------------------

start '(init)'

export SB_LOGIN=sbtester
export SB_FILESPACE=north
export SB_TOP=/tmp/test074/tmp
export SB_SERVER=$(hostname):2123
export SB_GROUP=carl

rm -rf $SB_TOP
mkdir $SB_TOP
cat > $SB_TOP/.top <<EOF
export SB_LOGIN=sbtester
export SB_FILESPACE=north
export SB_TOP=/tmp/test074/tmp
export SB_SERVER=$(hostname):2123
export SB_GROUP=carl
EOF

cd $SB_TOP

sb --Group --create --name alice --parent world
sb --Group --create --name bob --parent alice
sb --Group --create --name carl --parent alice

sb --filesystem --create --name north --group alice
sb --filesystem --create --name south --group alice

sbmkdir app
cd app
sbmkdir sb
cd sb

(cd ${sb} ; tar -cf - .) | tar -xvf -
chmod 664 *.scm
sbcreat *.scm

for d in callback versionids util datamodel kernel ui pstore api shell \
         telnetd cli migrate
do sbmkdir $d
   cd $d
   sbcreat *.scm
   cd ..
done

sb --ls

sb --filesystem north --policy want-snapshot --on

sb --changereq --group alice --title "Fix it, bub" --summary "Needs fixing." \
   --remarks "This thing really needs fixing."

stop

# ----------------------------------------------------------------------
# Volume 1 -- More stuff
# ----------------------------------------------------------------------

lssctl -cx $v1 $v0 -.

start '(open1)'

sb --changereq 1 --research
sb --changereq 1 --fixing

sbmkdir -y1 dux
cd dux
date > now.dat
sbcreat -y1 now.dat

sb --file now.dat --checkout --lock
sleep 2
date > now.dat
sb --file now.dat --checkin --remarks "updated the date" --request 1

# online pack volume 1
vacuum

# shut down

stop

# to restart, do this:
# ./sbs test/test-074.scm -e '(open0)'
