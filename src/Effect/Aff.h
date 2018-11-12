#ifndef EFFECT_AFF_H
#define EFFECT_AFF_H

#include <purescript.h>

PURS_FFI_EXPORT(Effect_Aff_runFiber);
PURS_FFI_EXPORT(Effect_Aff_makeFiberImpl);
PURS_FFI_EXPORT(Effect_Aff_makeAff);
PURS_FFI_EXPORT(Effect_Aff__fork);
PURS_FFI_EXPORT(Effect_Aff__map);
PURS_FFI_EXPORT(Effect_Aff__bimap);
PURS_FFI_EXPORT(Effect_Aff__pure);
PURS_FFI_EXPORT(Effect_Aff__bind);
PURS_FFI_EXPORT(Effect_Aff__liftEffect);
PURS_FFI_EXPORT(Effect_Aff__throwError);
PURS_FFI_EXPORT(Effect_Aff__catchError);
PURS_FFI_EXPORT(Effect_Aff__joinFiber);
PURS_FFI_EXPORT(Effect_Aff_generalBracket);
PURS_FFI_EXPORT(Effect_Aff_isSuspended);
PURS_FFI_EXPORT(Effect_Aff__killFiber);

#endif // EFFECT_AFF_H
