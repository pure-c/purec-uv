#ifndef EFFECT_AFF_H
#define EFFECT_AFF_H

#include <purescript.h>

PURS_FFI_EXPORT(Effect_Aff_runFiber);
PURS_FFI_EXPORT(Effect_Aff_makeFiberImpl);
PURS_FFI_EXPORT(Effect_Aff_makeAff);
PURS_FFI_EXPORT(Effect_Aff__map);
PURS_FFI_EXPORT(Effect_Aff__pure);
PURS_FFI_EXPORT(Effect_Aff__bind);
PURS_FFI_EXPORT(Effect_Aff__liftEffect);

#endif // EFFECT_AFF_H
