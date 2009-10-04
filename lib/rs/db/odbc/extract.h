#include <iodbc.h>
#include <isql.h>
#include <rscheme/scheme.h>

obj compile_odbc_extraction_plan( obj meta );
obj run_odbc_extraction_plan( HSTMT hstmt, obj plan );
