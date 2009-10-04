#include <rscheme.h>

rs_bool rs_bignum_prob_primeq( obj n );

obj rs_exp_modulo( obj base, obj exp, obj mod );

obj rs_mod2exp( obj n, unsigned exp );

obj rs_invertmod( obj n, obj mod );

obj rs_export_bignum( obj n );
obj rs_import_bignum( obj s );

