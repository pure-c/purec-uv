#include <uv.h>
#include <purescript.h>

PURS_FFI_FUNC_2(UV_Common_ip4Addr, _ip, _port, {
	const void *ip = purs_any_get_string(_ip);
	int port = purs_any_get_int(_port);
	struct sockaddr_in *addr = purs_new(struct sockaddr_in);
	uv_ip4_addr((const char*) ip, port, addr);
	return purs_any_foreign_new(NULL, addr);
});
