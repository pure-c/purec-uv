#include <stdlib.h>
#include <purescript.h>

PURS_FFI_FUNC_2(Test_Main_exit, _i, _, {
	exit(purs_any_get_int(_i));
});
