
#RIX=/usr/src/rs/rs-0.7.3.1-b32/src/fshell/fshell -image /tmp/cache.fasl/rix_2065_308435.fas
RIX=./rix

test::
	./rix -cvf /tmp/foo.sto -u msg1
	formail -s ${RIX} -vf /tmp/foo.sto -u - < msgs2

