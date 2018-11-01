#ifndef PUREC_UV_H
#define PUREC_UV_H

#include <purescript.h>

PURS_FFI_EXPORT(UV_strerror);
PURS_FFI_EXPORT(UV__RunDefault);
PURS_FFI_EXPORT(UV__RunOnce);
PURS_FFI_EXPORT(UV__RunNoWait);
PURS_FFI_EXPORT(UV_defaultLoop);
PURS_FFI_EXPORT(UV_newLoop);
PURS_FFI_EXPORT(UV_runImpl);

PURS_FFI_EXPORT(UV_listenImpl);
PURS_FFI_EXPORT(UV_readStartImpl);
PURS_FFI_EXPORT(UV_writeImpl);

PURS_FFI_EXPORT(UV_ip4Addr);

PURS_FFI_EXPORT(UV_bufferFromString);
PURS_FFI_EXPORT(UV_bufferToString);

PURS_FFI_EXPORT(UV_tcpNewImpl);
PURS_FFI_EXPORT(UV_tcpBindImpl);
PURS_FFI_EXPORT(UV_tcpConnectImpl);

PURS_FFI_EXPORT(UV__UdpIpv6Only);
PURS_FFI_EXPORT(UV__UdpPartial);
PURS_FFI_EXPORT(UV__UdpReuseAddr);
PURS_FFI_EXPORT(UV_udpNewImpl);
PURS_FFI_EXPORT(UV_udpBindImpl);
PURS_FFI_EXPORT(UV_udpRecvStartImpl);
PURS_FFI_EXPORT(UV_udpSetBroadcastImpl);
PURS_FFI_EXPORT(UV_udpSendImpl);

#endif // PUREC_UV_H
