#ifndef PUREC_UV_INTERNAL_H
#define PUREC_UV_INTERNAL_H

#include <uv.h>
#include <purescript.h>

PURS_FFI_EXPORT(UV_Internal_mkUtils);

typedef struct purec_uv_utils_s {
	const purs_any_t * Left;
	const purs_any_t * Right;
	const purs_any_t * Nothing;
	const purs_any_t * Just;
} purec_uv_utils_t;

#define BASE_HANDLE_FIELDS\
	purec_uv_utils_t* utils;

/* abstract */
struct purec_uv_base_handle_s {
	BASE_HANDLE_FIELDS
};

#define HANDLE_GET_UTILS(HANDLE)\
	((struct purec_uv_base_handle_s*)((HANDLE)->data))->utils

#define INIT_BASE_HANDLE(HANDLE, TYPE, UTILS)\
	(HANDLE)->data = purs_new(TYPE);\
	((struct purec_uv_base_handle_s*)(HANDLE)->data)->utils = \
		((purec_uv_utils_t*)(UTILS))

/* Return an 'Either Int V' based on a given expression */
#define TO_EITHER(UTILS, EXPRESSION, V)\
	({\
		int __ret = EXPRESSION;\
		(__ret == 0)\
			? purs_any_app((UTILS)->Right, V)\
			: purs_any_app((UTILS)->Left, purs_any_int_new(__ret));\
	})

/* Build a flag from an array of flags */
int purec_uv_build_flags (const purs_any_t *);

/* Buffer allocation callback for libuv */
void purec_uv_alloc_buf_cb(uv_handle_t*, size_t, uv_buf_t*);

#endif // PUREC_UV_INTERNAL_H
