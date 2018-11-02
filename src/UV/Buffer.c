#include <uv.h>
#include <purescript.h>

PURS_FFI_FUNC_2(UV_Buffer_fromString, _str, _, {
	const void *str = purs_any_get_string(_str);
	uv_buf_t *buf = purs_new(uv_buf_t);
	size_t str_size = purs_string_size(str);
	void *base = purs_malloc(str_size);
	memcpy(base, str, str_size);
	buf->len = purs_string_size(str);
	buf->base = base;
	return purs_any_foreign_new(NULL, buf);
});

PURS_FFI_FUNC_2(UV_Buffer_toString, _buf, _, {
	const uv_buf_t *buf = purs_any_get_foreign(_buf)->data;
	char *dat = purs_malloc(buf->len + 1);
	memcpy(dat, buf->base, buf->len);
	dat[buf->len] = '\0';
	return purs_any_string_new_mv(dat);
});
