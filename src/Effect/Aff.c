#include <purescript.h>

#define TO_FOREIGN(V) purs_any_foreign_new(NULL, (void*) V)
#define FROM_FOREIGN(V) purs_any_get_foreign(V)->data

#define DO_RETHROW   1
#define DONT_RETHROW 0

#define AFF_TAG_MAP(XX)\
	XX(AFF_TAG_PURE,   0)\
	XX(AFF_TAG_BIND,   1)\
	XX(AFF_TAG_SYNC,   2)\
	XX(AFF_TAG_ASYNC,  3)\
	XX(AFF_TAG_THROW,  4)\
	XX(AFF_TAG_CATCH,  5)\
	XX(AFF_TAG_CONS,   6)\
	XX(AFF_TAG_RESUME, 7)\
	XX(AFF_TAG_FORK,   8)\

#define TO_ENUM_MEMBER(N, V) N = V,
#define TO_LOOKUP_MEMBER(N, V) # N,

typedef enum {
	AFF_TAG_MAP(TO_ENUM_MEMBER)
} aff_tag_t;

char* aff_tag_str_lookup[] = {
	AFF_TAG_MAP(TO_LOOKUP_MEMBER)
};

typedef struct aff_s aff_t;
typedef struct step_s step_t;

#define STEP_TAG_MAP(XX)\
	XX(STEP_TAG_AFF, 0)\
	XX(STEP_TAG_VAL, 1)\

typedef enum {
	STEP_TAG_MAP(TO_ENUM_MEMBER)
} step_tag_t;

char* step_tag_str_lookup[] = {
	STEP_TAG_MAP(TO_LOOKUP_MEMBER)
};

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

		/* Catch (Aff e a) (e -> Aff e a) */
		struct {
			const aff_t * value0;
			const purs_any_t * value1;
		} catch;

		/* Throw e */
		struct {
			const purs_any_t * value0;
		} throw;

		/* ∀ b. Bind  (Aff e b) (b -> Aff e a) */
		struct {
			const aff_t * value0;
			const purs_any_t * value1;
		} bind;

		/* ∀ b. Fork Boolean (Aff e b) */
		struct {
			int value0;
			const aff_t * value1;
		} fork;

		/* ??? */
		struct {
			const aff_t * value0; /* ??? */
			const aff_t * value1; /* ??? */
			const purs_any_t * value2; /* interrupt */
		} cons;

		/* ??? */
		struct {
			const purs_any_t * value0; /* ??? */
			const cons_t * value1; /* ??? */
		} resume;
	};
};

aff_t * aff_pure_new(const purs_any_t * value0) {
	aff_t * aff = purs_new(aff_t);
	aff->tag = AFF_TAG_PURE;
	aff->pure.value0 = value0;
	return aff;
}

aff_t * aff_cons_new(const aff_t * value0,
		     const aff_t * value1,
		     const purs_any_t * value2) {
	aff_t * aff = purs_new(aff_t);
	aff->tag = AFF_TAG_CONS;
	aff->cons.value0 = value0;
	aff->cons.value1 = value1;
	aff->cons.value2 = value2;
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

aff_t * aff_throw_new(const purs_any_t * value0) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_THROW;
	aff_->throw.value0 = value0;
	return aff_;
}

aff_t * aff_resume_new(const purs_any_t * value0,
		       const cons_t * value1) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_RESUME;
	aff_->resume.value0 = value0;
	aff_->resume.value1 = value1;
	return aff_;
}

aff_t * aff_catch_new(const aff_t * value0, const purs_any_t * value1) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_CATCH;
	aff_->catch.value0 = value0;
	aff_->catch.value1 = value1;
	return aff_;
}

aff_t * aff_fork_new(int value0, const aff_t * value1) {
	assert(value1 != NULL);
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_FORK;
	aff_->fork.value0 = value0;
	aff_->fork.value1 = value1;
	return aff_;
}

#define FIBER_STATE_MAP(XX)\
	XX(FIBER_STATE_SUSPENDED,   0)\
	XX(FIBER_STATE_CONTINUE,    1)\
	XX(FIBER_STATE_STEP_BIND,   2)\
	XX(FIBER_STATE_STEP_RESULT, 3)\
	XX(FIBER_STATE_PENDING,     4)\
	XX(FIBER_STATE_RETURN,      5)\
	XX(FIBER_STATE_COMPLETED,   6)\

typedef enum {
	FIBER_STATE_MAP(TO_ENUM_MEMBER)
} fiber_state;

char* fiber_state_str_lookup[] = {
	FIBER_STATE_MAP(TO_LOOKUP_MEMBER)
};

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

typedef struct join_s join_t;
struct join_s {
	int rethrow;
	fiber_t * fiber;
	const purs_any_t * callback;
};

static inline join_t * join_new(fiber_t * fiber,
				const purs_any_t * callback,
				int rethrow) {
	join_t * join = purs_new(join_t);
	join->fiber = fiber;
	join->callback = callback;
	join->rethrow = rethrow;
	return join;
}

typedef struct joins_table_s {
	int id;
	join_t join;
	UT_hash_handle hh;
} join_table_t;

struct fiber_s {
	/* Monotonically increasing tick, increased on each asynchronous turn.
	 */
	uint32_t run_tick;

	/* The current branch of the state machine. */
	fiber_state state;

	/* The current point of interest for the state machine branch. */
	const step_t * step;
	const purs_any_t * failure;
	const purs_any_t * interrupt;

	/* Stack of continuations for the current fiber. */
	const purs_any_t * bhead;
	const cons_t * btail; // TODO: use aff_t under AFF_TAG_CONS?

	/* Stack of attempts and finalizers for error recovery. Every `Cons` is
	   also tagged with current `interrupt` state. We use this to track
	   which items should be ignored or evaluated as a result of a kill. */
	const aff_t * attempts;

	/* A special state is needed for Bracket, because it cannot be
	   killed. When we enter a bracket acquisition or finalizer, we
	   increment the counter, and then decrement once complete. */
	uint32_t bracket_count;

	/* Each join gets a new id so they can be revoked. */
	join_table_t * joins;
	int join_id;
	int rethrow;

	/* ancillary utilities */
	const utils_t * utils;

};

fiber_t * fiber_new(const utils_t * utils, const aff_t * aff) {
	fiber_t * fiber = purs_new(fiber_t);
	fiber->run_tick = 0;
	fiber->state = FIBER_STATE_SUSPENDED;
	fiber->step = step_aff_new(aff);
	fiber->failure = NULL;
	fiber->interrupt = NULL;
	fiber->bhead = NULL;
	fiber->btail = NULL;
	fiber->attempts = NULL;
	fiber->bracket_count = 0;
	fiber->joins = NULL;
	fiber->join_id = 0;
	fiber->rethrow = 1;
	fiber->utils = utils;
	return fiber;
}

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

void fiber_run(fiber_t*, uint32_t);

PURS_FFI_FUNC_4(runAsync, _localRunTick, _fiber, result, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);

	/* prevent re-entrance */
	if (purs_any_get_int(_localRunTick) != fiber->run_tick) {
		return NULL;
	}

	fiber->run_tick++;

	/* TODO scheduler.enqueue the below */
	fiber->state = FIBER_STATE_STEP_RESULT;
	fiber->step = step_val_new(result);
	fiber_run(fiber, fiber->run_tick);

	return NULL;
});

PURS_FFI_FUNC_1(noop_canceler, _, {
	return NULL;
});

PURS_FFI_FUNC_2(join_canceler, _entry, _, {
	join_table_t * entry = FROM_FOREIGN(_entry);
	HASH_DEL(entry->join.fiber->joins, entry);
	return NULL;
});

PURS_FFI_FUNC_2(onComplete, _join, _, {
	const join_t * join = FROM_FOREIGN(_join);
	printf("onComplete: join->fiber->state: %s\n",
	       fiber_state_str_lookup[join->fiber->state]);
	if (join->fiber->state == FIBER_STATE_COMPLETED) {
		join->fiber->rethrow = join->fiber->rethrow && join->rethrow;
		assert(join->fiber->step->tag == STEP_TAG_VAL);
		purs_any_app(purs_any_app(join->callback,
					  join->fiber->step->val), NULL);
		return noop_canceler;
	} else {
		join_table_t * entry = purs_new(join_table_t);
		entry->id = join->fiber->join_id++;
		entry->join = *join;
		HASH_ADD_INT(join->fiber->joins, id, entry);
		return purs_any_app(join_canceler, TO_FOREIGN(entry));
	}
});

PURS_FFI_FUNC_3(Effect_Aff__joinFiber, _fiber, cb, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	printf("joinFiber: fiber->state: %s\n", fiber_state_str_lookup[fiber->state]);
	const purs_any_t * canceler =
		purs_any_app(
			purs_any_app(onComplete,
				     TO_FOREIGN(join_new(fiber,
							 cb,
							 DONT_RETHROW))),
			NULL);
	if (fiber->state == FIBER_STATE_SUSPENDED) {
		fiber_run(fiber, fiber->run_tick);
	}
	return canceler;
});

void fiber_run(fiber_t * fiber, uint32_t local_run_tick) {
	while (1) {
		printf("fiber->state: %s\n", fiber_state_str_lookup[fiber->state]);
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
				if (fiber->btail->head->tag == STEP_TAG_AFF) {
					assert(fiber->btail->head->aff->tag == AFF_TAG_BIND);
					fiber->bhead = fiber->btail->head->aff->bind.value1;
				} else {
					assert(fiber->btail->head->tag == STEP_TAG_VAL);
					fiber->bhead = fiber->btail->head->val;
				}
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
				fiber->failure = fiber->step->val;
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
			printf("fiber->step->tag: %s\n",
			       step_tag_str_lookup[fiber->step->tag]);
			switch (fiber->step->tag) {
			case STEP_TAG_AFF: {
				printf("fiber->step->aff: %p\n", fiber->step->aff);
				printf("fiber->step->aff->tag: %s\n",
				       aff_tag_str_lookup[fiber->step->aff->tag]);
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
				case AFF_TAG_THROW: {
					fiber->state = FIBER_STATE_RETURN;
					fiber->failure =
						utils_to_left(
							fiber->utils,
							fiber->step->aff->throw.value0);
					fiber->step = NULL;
					break;
				}
				/* Enqueue the Catch so that we can call the
				   error handler later on in case of an
				   exception. */
				case AFF_TAG_CATCH: {
					if (fiber->bhead == NULL) {
						fiber->attempts =
							aff_cons_new(
								fiber->step->aff,
								fiber->attempts,
								fiber->interrupt);
					} else {
						fiber->attempts =
							aff_cons_new(
								fiber->step->aff,
								aff_cons_new(
									aff_resume_new(fiber->bhead, fiber->btail),
									fiber->attempts,
									fiber->interrupt),
								fiber->interrupt);
					}
					fiber->bhead = NULL;
					fiber->btail = NULL;
					fiber->state = FIBER_STATE_CONTINUE;
					fiber->step = step_aff_new(fiber->step->aff->catch.value0);
					break;
				}

				case AFF_TAG_FORK: {
					fiber->state = FIBER_STATE_STEP_RESULT;
					fiber_t * tmp =
						fiber_new(fiber->utils,
							  fiber->step->aff->fork.value1);
					if (0 /* supervisor */) {
						/* TODO: add support for supervisors */
					}
					if (fiber->step->aff->fork.value0) {
						fiber_run(tmp, 0);
					}
					fiber->step =
						step_val_new(
							utils_to_right(fiber->utils,
								       TO_FOREIGN(tmp)));
					break;
				}
				}
				break;
			}
			case STEP_TAG_VAL: {
				printf("fiber->step->val->tag: %s\n",
				       purs_any_tag_str(fiber->step->val->tag));
			}
			}
			break;
		}

		case FIBER_STATE_SUSPENDED:
			fiber->state = FIBER_STATE_CONTINUE;
			break;

		case FIBER_STATE_RETURN:
			fiber->bhead = NULL;
			fiber->btail = NULL;

			/* If the current stack has returned, and we have no
			   other stacks to resume or finalizers to run, the
			   fiber has halted and we can invoke all join
			   callbacks. Otherwise we need to resume. */
			if (fiber->attempts == NULL) {
				fiber->state = FIBER_STATE_COMPLETED;
				fiber->step =
					fiber->interrupt != NULL
					? step_val_new(fiber->interrupt)
					: fiber->failure != NULL
					? step_val_new(fiber->failure)
					: fiber->step;
			} else {
				assert(fiber->attempts->tag == AFF_TAG_CONS);
				const purs_any_t * tmp = fiber->attempts->cons.value2;
				const aff_t * attempt = fiber->attempts->cons.value0;
				fiber->attempts = fiber->attempts->cons.value1;
				switch (attempt->tag) {
				case AFF_TAG_CATCH:
					if (fiber->interrupt && fiber->interrupt != tmp) {
						fiber->state = FIBER_STATE_RETURN;
					} else if (fiber->failure != NULL) {
						fiber->state = FIBER_STATE_CONTINUE;
						fiber->step = step_aff_new(
							FROM_FOREIGN(purs_any_app(attempt->catch.value1,
										  utils_from_left(fiber->utils,
												  fiber->failure))));
						fiber->failure = NULL;
					}
					break;
				}
			}
			break;

		case FIBER_STATE_PENDING:
			return;

		case FIBER_STATE_COMPLETED:
			// TODO: evaluate joins
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

	return TO_FOREIGN(fiber_new(utils, FROM_FOREIGN(aff)));
});

PURS_FFI_FUNC_2(Effect_Aff__catchError, aff, k, {
		printf("%p\n", aff); //fiber->step->aff->catch.value0);
		printf("x%p\n", FROM_FOREIGN(aff)); //fiber->step->aff->catch.value0);
	return TO_FOREIGN(aff_catch_new(FROM_FOREIGN(aff), k));
});

PURS_FFI_FUNC_1(Effect_Aff__throwError, e, {
	return TO_FOREIGN(aff_throw_new(e));
});

PURS_FFI_FUNC_1(Effect_Aff_makeAff, action, {
	return TO_FOREIGN(aff_async_new(action));
});

PURS_FFI_FUNC_2(Effect_Aff_runFiber, _fiber, _, {
	fiber_t *fiber = FROM_FOREIGN(_fiber);
	// TODO: scheduler.enqueue (see Aff.js)
	fiber_run(fiber, fiber->run_tick);
	return NULL;
});

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

PURS_FFI_FUNC_2(Effect_Aff__fork, immediate, aff, {
	return TO_FOREIGN(
		aff_fork_new(purs_any_get_int(immediate),
			     FROM_FOREIGN(aff)));
});
