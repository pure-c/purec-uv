#ifndef PUREC_UV_HANDLE_H
#define PUREC_UV_HANDLE_H

#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"

#define BASE_HANDLE_FIELDS\
	purec_uv_utils_t* utils;\
	purs_vec_t* close_cbs;

/* base */
typedef struct purec_uv_base_handle_s {
	BASE_HANDLE_FIELDS
} purec_uv_base_handle_t;

#define HANDLE_GET_UTILS(HANDLE)\
	((struct purec_uv_base_handle_s*)((HANDLE)->data))->utils

#define INIT_BASE_HANDLE_INTERNAL(DATA, UTILS)\
	(DATA)->utils = UTILS;\
	(DATA)->close_cbs = NULL

#define INIT_BASE_HANDLE(HANDLE, TYPE, UTILS)\
	(HANDLE)->data = purs_new(TYPE);\
	INIT_BASE_HANDLE_INTERNAL(\
		((struct purec_uv_base_handle_s*)(HANDLE)->data), \
		((purec_uv_utils_t*)(UTILS)))

PURS_FFI_EXPORT(UV_Handle_closeImpl);

#endif // PUREC_UV_HANDLE_H
