#include <uv.h>
#include <purescript.h>

PURS_FFI_FUNC_1(UV_Error_strerror, _err, {
	return purs_any_string_new(uv_strerror(purs_any_get_int(_err)));
});
