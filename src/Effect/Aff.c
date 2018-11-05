#include <purescript.h>

#define TO_FOREIGN(V) purs_any_foreign_new(NULL, (void*) V)
#define FROM_FOREIGN(V) purs_any_get_foreign(V)->data

typedef enum {
	AFF_TAG_PURE = 0,
	AFF_TAG_BIND = 1,
	AFF_TAG_SYNC = 2,
	AFF_TAG_ASYNC = 3,
} aff_tag_t;

typedef struct aff_s aff_t;
typedef struct step_s step_t;

typedef enum {
	STEP_TAG_AFF = 0,
	STEP_TAG_VAL = 1,
} step_tag_t;

struct step_s {
	step_tag_t tag;
	union {
		const purs_any_t * val;
		const aff_t * aff;
	};
};

const step_t * step_val_new (const purs_any_t * val) {
	step_t * step = purs_new(step_t);
	step->tag = STEP_TAG_VAL;
	step->val = val;
	return step;
}

const step_t * step_aff_new (const aff_t * aff) {
	step_t * step = purs_new(step_t);
	step->tag = STEP_TAG_AFF;
	step->aff = aff;
	return step;
}

struct aff_s {
	aff_tag_t tag;

	union {
		/* Pure a */
		struct {
			const purs_any_t * value0;
		} pure;

		/* Async ((Either Error a -> Effect Unit) -> Effect Canceler) */
		struct {
			const purs_any_t * value0;
		} async;

		/* Sync (Effect a) */
		struct {
			const purs_any_t * value0;
		} sync;

		/* ∀ b. Bind  (Aff b) (b -> Aff a) */
		struct {
			const aff_t * value0;
			const purs_any_t * value1;
		} bind;
	};
};

aff_t * aff_pure_new(const purs_any_t * a) {
	aff_t * aff = purs_new(aff_t);
	aff->tag = AFF_TAG_PURE;
	aff->pure.value0 = a;
	return aff;
}

aff_t * aff_async_new(const purs_any_t * value0) {
	aff_t * aff = purs_new(aff_t);
	aff->tag = AFF_TAG_ASYNC;
	aff->async.value0 = value0;
	return aff;
}


aff_t * aff_sync_new(const purs_any_t * value0) {
	aff_t * aff = purs_new(aff_t);
	aff->tag = AFF_TAG_SYNC;
	aff->sync.value0 = value0;
	return aff;
}

aff_t * aff_bind_new(const aff_t * value0, const purs_any_t * value1) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_BIND;
	aff_->bind.value0 = value0;
	aff_->bind.value1 = value1;
	return aff_;
}

typedef struct cons_s cons_t;
struct cons_s {
	const step_t * head;
	const cons_t * tail;
};

cons_t * cons_new(const step_t * head, const cons_t * tail) {
	cons_t * cons = purs_new(cons_t);
	cons->head = head;
	cons->tail = tail;
	return cons;
}

typedef enum {
	FIBER_STATE_SUSPENDED = 0,
	FIBER_STATE_CONTINUE = 1,
	FIBER_STATE_STEP_BIND = 2,
	FIBER_STATE_STEP_RESULT = 3,
	FIBER_STATE_PENDING = 4,
	FIBER_STATE_RETURN = 5,
	FIBER_STATE_COMPLETED = 6,
} fiber_state;

typedef struct utils_s utils_t;
struct utils_s {
	const purs_any_t * is_left;
	const purs_any_t * is_right;
	const purs_any_t * from_left;
	const purs_any_t * from_right;
	const purs_any_t * Left;
	const purs_any_t * Right;
};

typedef struct fiber_s fiber_t;
struct fiber_s {
	/* Monotonically increasing tick, increased on each asynchronous turn.
	 */
	uint32_t run_tick;

	/* The current branch of the state machine. */
	fiber_state state;

	/* The current point of interest for the state machine branch. */
	const step_t * step;
	const step_t * failure;
	const aff_t * interrupt;

	/* Stack of continuations for the current fiber. */
	const purs_any_t * bhead;
	const cons_t * btail;

	/* Stack of attempts and finalizers for error recovery. Every `Cons` is
	   also tagged with current `interrupt` state. We use this to track
	   which items should be ignored or evaluated as a result of a kill. */
	aff_t * attempts;

	/* A special state is needed for Bracket, because it cannot be
	   killed. When we enter a bracket acquisition or finalizer, we
	   increment the counter, and then decrement once complete. */
	uint32_t bracket_count;

	/* anicillary utilities */
	const utils_t * utils;
};

#define init_fiber(FIBER, UTILS, AFF)\
	(FIBER)->run_tick = 0;\
	(FIBER)->state = FIBER_STATE_SUSPENDED;\
	(FIBER)->step = step_aff_new(AFF);\
	(FIBER)->failure = NULL;\
	(FIBER)->interrupt = NULL;\
	(FIBER)->bhead = NULL;\
	(FIBER)->btail = NULL;\
	(FIBER)->attempts = NULL;\
	(FIBER)->bracket_count = 0;\
	(FIBER)->utils = UTILS;

int utils_is_right (const utils_t * utils, const purs_any_t * v) {
	return purs_any_is_true(purs_any_app(utils->is_right, v));
}

int utils_is_left (const utils_t * utils, const purs_any_t * v) {
	return purs_any_is_true(purs_any_app(utils->is_left, v));
}

const purs_any_t * utils_from_right (const utils_t * utils, const purs_any_t * v) {
	return purs_any_app(utils->from_right, v);
}

const purs_any_t * utils_from_left (const utils_t * utils, const purs_any_t * v) {
	return purs_any_app(utils->from_left, v);
}

const purs_any_t * utils_to_right (const utils_t * utils, const purs_any_t * v) {
	return purs_any_app(utils->Right, v);
}

const purs_any_t * utils_to_left (const utils_t * utils, const purs_any_t * v) {
	return purs_any_app(utils->Left, v);
}

const purs_any_t * run_sync(const utils_t * utils,
			    const purs_any_t * effect) {
	return purs_any_app(utils->Right,
			    purs_any_app(effect, NULL));
}

const purs_any_t * run_async(const utils_t * utils,
			     const purs_any_t * effect,
			     const purs_any_t * k) {
	return purs_any_app(purs_any_app(effect, k), NULL);
}

PURS_FFI_FUNC_4(runAsync, _localRunTick, _fiber, result, _, {
	printf("HELLO!!!\n");
	return NULL;
});

/* run_async_1(void * ctx, purs_any_t * result, va_list _) { */

              /* return function () { */
              /*   if (runTick !== localRunTick) { */
              /*     return; */
              /*   } */
              /*   runTick++; */
              /*   Scheduler.enqueue(function () { */
              /*     status = STEP_RESULT; */
              /*     step   = result; */
              /*     run(runTick); */
              /*   }); */
              /* }; */

void fiber_run(fiber_t * fiber, uint32_t local_run_tick) {
	while (1) {
		printf("fiber->state: %i\n", fiber->state);
		switch (fiber->state) {

		case FIBER_STATE_STEP_BIND: {
			assert(fiber->bhead != NULL);
			assert(fiber->step != NULL);
			assert(fiber->step->tag == STEP_TAG_VAL);
			fiber->state = FIBER_STATE_CONTINUE;
			fiber->step = step_aff_new(
				FROM_FOREIGN(
					purs_any_app(
						fiber->bhead,
						fiber->step->val)));

			/* update the stack pointers */
			if (fiber->btail == NULL) {
				fiber->bhead = NULL;
			} else {
				assert(fiber->btail != NULL);
				assert(fiber->btail->head->tag == STEP_TAG_AFF);
				assert(fiber->btail->head->aff->tag == AFF_TAG_BIND);
				fiber->bhead = fiber->btail->head->aff->bind.value1;
				fiber->btail = fiber->btail->tail;
			}
			break;
		}

		case FIBER_STATE_STEP_RESULT: {
			assert(fiber->step != NULL);
			assert(fiber->step->tag == STEP_TAG_VAL);
			assert(fiber->step->val != NULL);
			if (utils_is_left(fiber->utils, fiber->step->val)) {
				fiber->state = FIBER_STATE_RETURN;
				fiber->failure = fiber->step;
				fiber->step = NULL;
			} else if (fiber->bhead == NULL) {
				fiber->state = FIBER_STATE_RETURN;
			} else {
				fiber->state = FIBER_STATE_STEP_BIND;
				fiber->step = step_val_new(
					utils_from_right(fiber->utils,
							 fiber->step->val));
			}
			break;
		}

		case FIBER_STATE_CONTINUE: {
			assert(fiber->step != NULL);
			switch (fiber->step->tag) {
			case STEP_TAG_AFF: {
				printf("fiber->step->aff->tag: %i\n", fiber->step->aff->tag);
				switch (fiber->step->aff->tag) {
				case AFF_TAG_BIND: {
					if (fiber->bhead != NULL) {
						fiber->btail =
							cons_new(step_val_new(fiber->bhead),
								 fiber->btail);
					}
					fiber->bhead = fiber->step->aff->bind.value1;
					fiber->step = step_aff_new(fiber->step->aff->bind.value0);
					break;
				}
				case AFF_TAG_PURE: {
					if (fiber->bhead == NULL) {
						fiber->state = FIBER_STATE_RETURN;
						fiber->step = step_val_new(
							utils_to_right(fiber->utils,
								       fiber->step->aff->pure.value0));
					} else {
						fiber->state = FIBER_STATE_STEP_BIND;
						fiber->step = step_val_new(fiber->step->aff->pure.value0);
					}
					break;
				}
				case AFF_TAG_SYNC: {
					fiber->state = FIBER_STATE_STEP_RESULT;
					fiber->step = step_val_new(
						run_sync(fiber->utils,
							 fiber->step->aff->sync.value0));
					break;
				}
				case AFF_TAG_ASYNC: {
					fiber->state = FIBER_STATE_PENDING;
					fiber->step = step_val_new(
						run_async(
							fiber->utils,
							fiber->step->aff->async.value0,
							purs_any_app(
								purs_any_app(
									runAsync,
									purs_any_int_new(local_run_tick)),
								TO_FOREIGN(fiber))));
					break;
				}
				}
				break;
			}
			case STEP_TAG_VAL: {
			}
			}
			break;
		}

		case FIBER_STATE_SUSPENDED:
			fiber->state = FIBER_STATE_CONTINUE;
			break;

		case FIBER_STATE_RETURN:
			return;

		case FIBER_STATE_PENDING:
			return;
		}
	}
}

PURS_FFI_FUNC_8(Effect_Aff_makeFiberImpl,
		is_left,
		is_right,
		from_left,
		from_right,
		Left,
		Right,
		aff,
		_, {

	// TODO(felix) keep utils in static storage
	utils_t *utils = purs_new(utils_t);
	utils->is_left = is_left;
	utils->is_right = is_right;
	utils->from_left = from_left;
	utils->from_right = from_right;
	utils->Left = Left;
	utils->Right = Right;

	fiber_t *fiber = purs_new(fiber_t);
	init_fiber(fiber, utils, FROM_FOREIGN(aff));

	return TO_FOREIGN(fiber);
});

/* PURS_FFI_FUNC_1(Effect_Aff_makeAff, action, { */
/* 	return TO_FOREIGN(aff_async_new(action)); */
/* }); */

PURS_FFI_FUNC_2(Effect_Aff_runFiber, _fiber, _, {
	fiber_t *fiber = FROM_FOREIGN(_fiber);
	// TODO: scheduler.enqueue (see Aff.js)
	fiber_run(fiber, fiber->run_tick);
	return NULL;
});
      /* run: function () { */
      /*   if (status === SUSPENDED) { */
      /*     if (!Scheduler.isDraining()) { */
      /*       Scheduler.enqueue(function () { */
      /*         run(runTick); */
      /*       }); */
      /*     } else { */
      /*       run(runTick); */
      /*     } */
      /*   } */
      /* } */

PURS_FFI_FUNC_1(Effect_Aff__pure, a, {
	return TO_FOREIGN(aff_pure_new(a));
});

const purs_any_t * aff_bind (const void * ctx, const purs_any_t * arg, va_list _) {
	const purs_any_t * f = ctx;
	return TO_FOREIGN(aff_pure_new(purs_any_app(f, arg)));
}

PURS_FFI_FUNC_2(Effect_Aff__map, f, _aff, {
	const aff_t * aff = FROM_FOREIGN(_aff);
	if (aff->tag == AFF_TAG_PURE) {
		return TO_FOREIGN(
			aff_pure_new(purs_any_app(f, aff->pure.value0)));
	} else {
		return TO_FOREIGN(
			aff_bind_new(aff,
				     purs_any_cont_new((void *) f, aff_bind)));
	}
});

PURS_FFI_FUNC_2(Effect_Aff__bind, aff, k, {
	return TO_FOREIGN(aff_bind_new(FROM_FOREIGN(aff), k));
});

PURS_FFI_FUNC_1(Effect_Aff__liftEffect, effect, {
	return TO_FOREIGN(aff_sync_new(effect));
});
