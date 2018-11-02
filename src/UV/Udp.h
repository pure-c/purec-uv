#ifndef PUREC_UV_UDP_H
#define PUREC_UV_UDP_H

#include <purescript.h>

#include "UV.Internal.h"

typedef struct purec_uv_udp_ctx_s purec_uv_udp_ctx_t;
struct purec_uv_udp_ctx_s {
	BASE_HANDLE_FIELDS;
	const purs_any_t * on_recv_cont;
};

PURS_FFI_EXPORT(UV_Udp__UdpIpv6Only);
PURS_FFI_EXPORT(UV_Udp__UdpPartial);
PURS_FFI_EXPORT(UV_Udp__UdpReuseAddr);
PURS_FFI_EXPORT(UV_Udp_udpNewImpl);
PURS_FFI_EXPORT(UV_Udp_udpBindImpl);
PURS_FFI_EXPORT(UV_Udp_udpRecvStartImpl);
PURS_FFI_EXPORT(UV_Udp_udpSetBroadcastImpl);
PURS_FFI_EXPORT(UV_Udp_udpSendImpl);

#endif // PUREC_UV_UDP_H
