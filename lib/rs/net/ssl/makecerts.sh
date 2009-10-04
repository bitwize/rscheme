#! /bin/sh

#
#  Generate sample private keys and certificates for use
#  in testing the sslmgr executable and the RScheme https server
#

SUBJ='/C=US/ST=Texas/L=Austin/O=RScheme Develoment Group'
if test "$SERVER" = ""
then SERVER=$(hostname)
fi
DIR=security
mkdir -p $DIR

openssl req -passout pass:foobar \
            -keyout ${DIR}/root.privatex -out ${DIR}/root.cert \
            -newkey rsa:768 -x509 \
            -subj "$SUBJ/CN=keyman"

openssl rsa -in ${DIR}/root.privatex -out ${DIR}/root.private \
            -passin pass:foobar -passout pass:

openssl req -passout pass:foobar \
            -keyout ${DIR}/server.prot -out ${DIR}/server.csr \
            -newkey rsa:768 \
            -subj "$SUBJ/CN=$SERVER"

openssl req -passout pass:foobar \
            -keyout ${DIR}/client.private -out ${DIR}/client.csr \
            -newkey rsa:768 \
            -subj "$SUBJ/CN=A.User"

openssl x509 -req -set_serial 124 -CA ${DIR}/root.cert -CAkey ${DIR}/root.private \
             -in ${DIR}/server.csr -out ${DIR}/server.cert

openssl x509 -req -set_serial 457 -CA ${DIR}/root.cert -CAkey ${DIR}/root.private \
             -in ${DIR}/client.csr -out ${DIR}/client.cert

  

# Remove the passphrase (for some reason, 'req' wont let you specify
# an empty passphrase)

openssl rsa -in ${DIR}/server.prot -out ${DIR}/server.private \
            -passin pass:foobar -passout pass:

cp ${DIR}/root.cert ${DIR}/self.cert

# The concatenated file (private key and certificate) is used
# by curl as in `curl -E security/client.both:foobar' where "foobar"
# is the passphrase established above

cat ${DIR}/client.private ${DIR}/client.cert > ${DIR}/client.both

# Build a PKCS#12 file for import into a browser,
# including the CA cert along with the client (user) cert
openssl pkcs12 -export -certfile ${DIR}/root.cert \
                       -in ${DIR}/client.both \
                       -out ${DIR}/client.pfx \
                       -passin pass:foobar -passout pass:foobar
