#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"

static const purs_any_t * utils = NULL;
PURS_FFI_FUNC_4(UV_Internal_mkUtils, Left, Right, Nothing, Just, {
	if (utils == NULL) {
		purec_uv_utils_t * tmp = purs_new(purec_uv_utils_t);
		tmp->Left = Left;
		tmp->Right = Right;
		tmp->Nothing = Nothing;
		tmp->Just = Just;
		utils = purs_any_foreign_new(NULL, tmp);
	}
	return utils;
});

void purec_uv_alloc_buf_cb(uv_handle_t* handle,
			   size_t suggested_size,
			   uv_buf_t* buf) {
	buf->base = purs_malloc(suggested_size);
	buf->len = suggested_size;
}

int purec_uv_build_flags (const purs_any_t * flags) {
	const purs_vec_t * arr = purs_any_get_array(flags);
	const purs_any_t * tmp;
	int i;
	int output = 0;
	purs_vec_foreach(arr, tmp, i) {
		output |= purs_any_get_int(tmp);
	}
	return output;
}
