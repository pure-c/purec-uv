#ifndef PUREC_UV_INTERNAL_H
#define PUREC_UV_INTERNAL_H

#include <uv.h>
#include <purescript.h>

/* Return an 'Either Int V' based on a given expression */
#define TO_EITHER(EXPRESSION, V)\
	({\
		int __ret = EXPRESSION;\
		(__ret == 0)\
			? purs_any_app(Right, V)\
			: purs_any_app(Left, purs_any_int_new(__ret));\
	})

/* Build a flag from an array of flags */
int purec_uv_build_flags (const purs_any_t *);

/* Buffer allocation callback for libuv */
void purec_uv_alloc_buf_cb(uv_handle_t*, size_t, uv_buf_t*);

#define BASE_HANDLE_FIELDS\
	const purs_any_t * Left;\
	const purs_any_t * Right;\
	const purs_any_t * Nothing;\
	const purs_any_t * Just;

#define UNPACK_BASE_CTX(DATA)\
	const purs_any_t * Left = ((DATA))->Left;\
	const purs_any_t * Right = ((DATA))->Right;\
	const purs_any_t * Nothing = ((DATA))->Nothing;\
	const purs_any_t * Just = ((DATA))->Just;

#define INIT_BASE_HANDLE(HANDLE, STRUCT_TYPE)\
	(HANDLE)->data = purs_new(STRUCT_TYPE);\
	((STRUCT_TYPE*)((HANDLE)->data))->Left = Left;\
	((STRUCT_TYPE*)((HANDLE)->data))->Right = Right;\
	((STRUCT_TYPE*)((HANDLE)->data))->Nothing = Nothing;\
	((STRUCT_TYPE*)((HANDLE)->data))->Just = Just;

#endif // PUREC_UV_INTERNAL_H
