#include <uv.h>
#include <purescript.h>

#include "UV.Handle.h"

void purec_uv_handle_on_close(uv_handle_t* handle) {
	int i;
	purs_any_t* cb;
	purec_uv_base_handle_t* data = handle->data;
	purs_vec_foreach(data->close_cbs, cb, i) {
		purs_any_app(cb, NULL);
	}
}

PURS_FFI_FUNC_3(UV_Handle_closeImpl, cb, _handle, _, {
	uv_handle_t *handle = purs_any_get_foreign(_handle)->data;
	purec_uv_base_handle_t *data = handle->data;
	if (data->close_cbs == NULL) {
		data->close_cbs = purs_vec_new();
	}
	purs_vec_push_mut(data->close_cbs, cb);
	uv_close(handle, purec_uv_handle_on_close);
	return NULL;
});
