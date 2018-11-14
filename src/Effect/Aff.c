#include <purescript.h>

#define TO_FOREIGN(V) purs_any_foreign_new(NULL, (void*) V)
#define FROM_FOREIGN(V) purs_any_get_foreign(V)->data

#define DO_RETHROW   1
#define DONT_RETHROW 0

#define AFF_TAG_MAP(XX)\
	XX(AFF_TAG_PURE)\
	XX(AFF_TAG_BIND)\
	XX(AFF_TAG_SYNC)\
	XX(AFF_TAG_ASYNC)\
	XX(AFF_TAG_THROW)\
	XX(AFF_TAG_CATCH)\
	XX(AFF_TAG_CONS)\
	XX(AFF_TAG_RESUME)\
	XX(AFF_TAG_FORK)\
	XX(AFF_TAG_BRACKET)\
	XX(AFF_TAG_RELEASE)\
	XX(AFF_TAG_FINALIZED)\

#define TO_ENUM_MEMBER(N) N,
#define TO_LOOKUP_MEMBER(N) # N,

typedef enum {
	AFF_TAG_MAP(TO_ENUM_MEMBER)
} aff_tag_t;

char* aff_tag_str_lookup[] = {
	AFF_TAG_MAP(TO_LOOKUP_MEMBER)
};

typedef struct aff_s aff_t;
typedef struct step_s step_t;

#define STEP_TAG_MAP(XX)\
	XX(STEP_TAG_AFF)\
	XX(STEP_TAG_VAL)\

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

/* curried function application for common arities */
#define app_1(FN, A1) purs_any_app(FN, A1)
#define app_2(FN, A1, A2) purs_any_app(app_1(FN, A1), A2)
#define app_3(FN, A1, A2, A3) purs_any_app(app_2(FN, A1, A2), A3)


/* data Aff e a */
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

		/* ∀ r. Bracket
		 *        (Aff e r)              -- acquire
		 *        (r -> a -> Aff e Unit) -- completed
		 *        (r -> e -> Aff e Unit) -- failed
		 *        (r -> e -> Aff e Unit) -- killed
		 *        (r -> Aff e a)         -- action
		 */
		struct {
			const aff_t * acquire;
			const purs_any_t * completed;
			const purs_any_t * failed;
			const purs_any_t * killed;
			const purs_any_t * action;
		} bracket;

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

		/* ??? */
		struct {
			const aff_t * bracket; /* ??? */
			const purs_any_t * result; /* ??? */
		} release;

		/* ??? */
		struct {
			const purs_any_t * result; /* ??? */
			const purs_any_t * failure; /* ??? */
		} finalized;
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
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_FORK;
	aff_->fork.value0 = value0;
	aff_->fork.value1 = value1;
	return aff_;
}

aff_t * aff_bracket_new(const aff_t * acquire,
			const purs_any_t * completed,
			const purs_any_t * killed,
			const purs_any_t * failed,
			const purs_any_t * action) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_BRACKET;
	aff_->bracket.acquire = acquire;
	aff_->bracket.completed = completed;
	aff_->bracket.killed = killed;
	aff_->bracket.failed = failed;
	aff_->bracket.action = action;
	return aff_;
}

aff_t * aff_release_new(const aff_t * bracket,
			const purs_any_t * result) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_RELEASE;
	aff_->release.bracket = bracket;
	aff_->release.result = result;
	return aff_;
}

aff_t * aff_finalized_new(const purs_any_t * result,
			  const purs_any_t * failure) {
	aff_t * aff_ = purs_new(aff_t);
	aff_->tag = AFF_TAG_FINALIZED;
	aff_->finalized.result = result;
	aff_->finalized.failure = failure;
	return aff_;
}

#define FIBER_STATE_MAP(XX)\
	XX(FIBER_STATE_SUSPENDED)\
	XX(FIBER_STATE_CONTINUE)\
	XX(FIBER_STATE_STEP_BIND)\
	XX(FIBER_STATE_STEP_RESULT)\
	XX(FIBER_STATE_PENDING)\
	XX(FIBER_STATE_RETURN)\
	XX(FIBER_STATE_COMPLETED)\

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

#define AFF_SCHEDULER_LIMIT 1024

typedef struct scheduler_s {
	int size;
	int ix;
	int is_draining;
	const purs_any_t* queue[AFF_SCHEDULER_LIMIT];
} scheduler_t;

scheduler_t * scheduler_new() {
	scheduler_t * scheduler = purs_new(scheduler_t);
	scheduler->size = 0;
	scheduler->ix = 0;
	scheduler->is_draining = 0;
	return scheduler;
}

void scheduler_drain(scheduler_t * scheduler) {
	const purs_any_t * thunk = NULL;
	scheduler->is_draining = 1;
	while (scheduler->size != 0) {
		scheduler->size--;
		thunk = scheduler->queue[scheduler->ix];
		scheduler->queue[scheduler->ix] = NULL;
		scheduler->ix = (scheduler->ix + 1) % AFF_SCHEDULER_LIMIT;
		app_1(thunk, NULL);
	}
	scheduler->is_draining = 0;
}

int scheduler_is_draining(const scheduler_t * scheduler) {
	return scheduler->is_draining;
}

void scheduler_enqueue(scheduler_t * scheduler, const purs_any_t * thunk) {
	int i, tmp;
	if (scheduler->size == AFF_SCHEDULER_LIMIT) {
		tmp = scheduler->is_draining;
		scheduler_drain(scheduler);
		scheduler->is_draining = tmp;
	}

	scheduler->queue[(scheduler->ix + scheduler->size) % AFF_SCHEDULER_LIMIT] = thunk;
	scheduler->size++;

	if (scheduler->is_draining == 0) {
		scheduler_drain(scheduler);
	}
}

struct fiber_s {
	/* Monotonically increasing tick, increased on each asynchronous turn.
	 */
	uint32_t run_tick;

	/* The current branch of the state machine. */
	fiber_state state;

	/* action to take for errors that were not caught */
	const purs_any_t * on_uncaught_error;

	/* how to break current execution context */
	const purs_any_t * set_timeout;

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

	/* scheduler */
	scheduler_t * scheduler;
};

fiber_t * fiber_new(const utils_t * utils,
		    const purs_any_t * set_timeout,
		    const purs_any_t * on_uncaught_error,
		    const aff_t * aff) {
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
	fiber->set_timeout = set_timeout;
	fiber->on_uncaught_error = on_uncaught_error;
	fiber->utils = utils;
	fiber->scheduler = scheduler_new();
	return fiber;
}

int utils_is_right (const utils_t * utils, const purs_any_t * v) {
	return purs_any_is_true(app_1(utils->is_right, v));
}

int utils_is_left (const utils_t * utils, const purs_any_t * v) {
	return purs_any_is_true(app_1(utils->is_left, v));
}

const purs_any_t * utils_from_right (const utils_t * utils, const purs_any_t * v) {
	return app_1(utils->from_right, v);
}

const purs_any_t * utils_from_left (const utils_t * utils, const purs_any_t * v) {
	return app_1(utils->from_left, v);
}

const purs_any_t * utils_to_right (const utils_t * utils, const purs_any_t * v) {
	return app_1(utils->Right, v);
}

const purs_any_t * utils_to_left (const utils_t * utils, const purs_any_t * v) {
	return app_1(utils->Left, v);
}

const purs_any_t * run_sync(const utils_t * utils,
			    const purs_any_t * effect) {
	return app_1(utils->Right, app_1(effect, NULL));
}

const purs_any_t * run_async(const utils_t * utils,
			     const purs_any_t * effect,
			     const purs_any_t * k) {
	return app_2(effect, k, NULL);
}

void fiber_run(fiber_t*, uint32_t);

PURS_FFI_FUNC_4(onKilledCallback, fn, arg, _ /* unused */, __ /* thunk */, {
	return app_1(fn, arg);
});

PURS_FFI_FUNC_3(runAsyncInner, _fiber, result, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	fiber->state = FIBER_STATE_STEP_RESULT;
	fiber->step = step_val_new(result);
	fiber_run(fiber, fiber->run_tick);
	return NULL;
});

PURS_FFI_FUNC_4(runAsync, _localRunTick, _fiber, result, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	/* prevent re-entrance */
	if (purs_any_get_int(_localRunTick) != fiber->run_tick) {
		return NULL;
	} else {
		fiber->run_tick++;
		scheduler_enqueue(fiber->scheduler,
				  app_2(runAsyncInner, _fiber, result));
		return NULL;
	}
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
		app_2(join->callback,
		      join->fiber->step->val,
		      NULL);
		return noop_canceler;
	} else {
		join_table_t * entry = purs_new(join_table_t);
		entry->id = join->fiber->join_id++;
		entry->join = *join;
		HASH_ADD_INT(join->fiber->joins, id, entry);
		return app_1(join_canceler, TO_FOREIGN(entry));
	}
});

PURS_FFI_FUNC_3(Effect_Aff__joinFiber, _fiber, cb, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	printf("joinFiber: fiber->state: %s\n", fiber_state_str_lookup[fiber->state]);
	const purs_any_t * canceler =
		app_2(onComplete,
		      TO_FOREIGN(join_new(fiber, cb, DONT_RETHROW)),
		      NULL);
	if (fiber->state == FIBER_STATE_SUSPENDED) {
		fiber_run(fiber, fiber->run_tick);
	}
	return canceler;
});

PURS_FFI_FUNC_4(Effect_Aff__killFiber, error, k, _fiber, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	if (fiber->state == FIBER_STATE_COMPLETED) {
		app_1(utils_to_right(fiber->utils, NULL), NULL);
		return noop_canceler;
	}

	const purs_any_t * canceler =
		app_2(onComplete,
		      TO_FOREIGN(
			join_new(fiber,
				 app_2(onKilledCallback,
				       k,
				       utils_to_right(fiber->utils, NULL)),
				 DONT_RETHROW)),
		      NULL);

	switch (fiber->state) {
	case FIBER_STATE_SUSPENDED:
		fiber->interrupt = utils_to_left(fiber->utils, error);
		fiber->state = FIBER_STATE_COMPLETED;
		fiber->step = step_val_new(fiber->interrupt);
		fiber_run(fiber, fiber->run_tick);
		break;
	case FIBER_STATE_PENDING:
		if (fiber->interrupt == NULL) {
			fiber->interrupt = utils_to_left(fiber->utils, error);
		}
		if (fiber->bracket_count == 0) {
			if (fiber->state == FIBER_STATE_PENDING) {
				fiber->attempts =
					aff_cons_new(
						aff_finalized_new(
							/* TODO: check this is right */
							app_1(fiber->step->val,
							      error),
							NULL
						),
						fiber->attempts,
						fiber->interrupt
					);
			}
			fiber->state = FIBER_STATE_RETURN;
			fiber->step = NULL;
			fiber->failure = NULL;
			fiber_run(fiber, ++fiber->run_tick);
		}
		break;
	default:
		if (fiber->interrupt == NULL) {
			fiber->interrupt = utils_to_left(fiber->utils, error);
		}
		if (fiber->bracket_count == 0) {
			fiber->state = FIBER_STATE_RETURN;
			fiber->step = NULL;
			fiber->failure = NULL;
		}
		break;
	}

	return canceler;
});

PURS_FFI_FUNC_4(onCompletedCheckRethrow, _fiber, _error, _reCheck, _, {
	fiber_t * fiber = FROM_FOREIGN(_fiber);
	if (purs_any_get_int(_reCheck)) {
		if (fiber->rethrow) {
			app_2(fiber->on_uncaught_error, _error, NULL);
		}
	} else {
		app_2(fiber->on_uncaught_error, _error, NULL);
	}
	return NULL;
});

/* evaluate the fiber up to the next ASYNC boundary
 */
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
					app_1(fiber->bhead,
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
							app_2(runAsync,
							      purs_any_int_new(local_run_tick),
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
					printf("fiber->bhead: %p\n", fiber->bhead);
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
									aff_resume_new(fiber->bhead,
										       fiber->btail),
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
				case AFF_TAG_BRACKET: {
					fiber->bracket_count++;
					if (fiber->bhead == NULL) {
						fiber->attempts =
							aff_cons_new(fiber->step->aff,
								     fiber->attempts,
								     fiber->interrupt);
					} else {
						fiber->attempts =
							aff_cons_new(
								fiber->step->aff,
								aff_cons_new(
									aff_resume_new(fiber->bhead,
										       fiber->btail),
									fiber->attempts,
									fiber->interrupt),
								fiber->interrupt);
					}
					fiber->bhead = NULL;
					fiber->btail = NULL;
					fiber->state = FIBER_STATE_CONTINUE;
					fiber->step = step_aff_new(fiber->step->aff->bracket.acquire);
					break;
				}
				case AFF_TAG_FORK: {
					fiber->state = FIBER_STATE_STEP_RESULT;
					fiber_t * tmp =
						fiber_new(fiber->utils,
							  fiber->set_timeout,
							  fiber->on_uncaught_error,
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
				assert(0);
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
			printf ("fiber->attempts: %p\n", fiber->attempts);
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
				const aff_t * attempt = fiber->attempts->cons.value0;
				const purs_any_t * tmp = fiber->attempts->cons.value2;
				fiber->attempts = fiber->attempts->cons.value1;

				printf("attempt->tag: %s\n",
				       aff_tag_str_lookup[attempt->tag]);
				switch (attempt->tag) {
				case AFF_TAG_CATCH:
					if (fiber->interrupt && fiber->interrupt != tmp) {
						fiber->state = FIBER_STATE_RETURN;
					} else if (fiber->failure != NULL) {
						fiber->state = FIBER_STATE_CONTINUE;
						fiber->step = step_aff_new(
							FROM_FOREIGN(app_1(attempt->catch.value1,
									   utils_from_left(fiber->utils,
											   fiber->failure))));
						fiber->failure = NULL;
					}
					break;
				case AFF_TAG_RESUME:
					/* As with Catch, we only want to ignore
					   in the case of an interrupt since
					   enqueing the item. */
					if ((fiber->interrupt != NULL &&
					     fiber->interrupt != tmp) ||
					    fiber->failure != NULL) {
						fiber->state = FIBER_STATE_RETURN;
					} else {
						assert(fiber->step != NULL);
						assert(fiber->step->tag == STEP_TAG_VAL);
						fiber->bhead = attempt->resume.value0;
						fiber->btail = attempt->resume.value1;
						fiber->state = FIBER_STATE_STEP_BIND;
						fiber->step =
							step_val_new(utils_from_right(fiber->utils,
										      fiber->step->val));
					}
					break;

				/* If we have a bracket, we should enqueue the
				handlers, and continue with the success branch
				only if the fiber has not been interrupted. If
				the bracket acquisition failed, we should not
				run either. */
				case AFF_TAG_BRACKET:
					fiber->bracket_count--;
					if (fiber->failure == NULL) {
						const purs_any_t * result =
							utils_from_right(fiber->utils,
									 fiber->step->val);
						fiber->step = step_val_new(result);

						/* We need to enqueue the
						Release with the same interrupt
						status as the Bracket that is
						initiating it. */
						fiber->attempts =
							aff_cons_new(aff_release_new(attempt, result),
								     fiber->attempts,
								     tmp);

						/* We should only continue as
						   long as the interrupt status
						   has not changed or we are
						   currently within a
						   non-interruptable
						   finalizer. */
						if (fiber->interrupt == tmp ||
						    fiber->bracket_count > 0) {
							fiber->state = FIBER_STATE_CONTINUE;
							fiber->step =
								step_aff_new(
									FROM_FOREIGN(
										app_1(attempt->bracket.action,
										      result)));
						}
					}
					break;

				/* Enqueue the appropriate handler. We increase
				the bracket count because it should not be
				cancelled. */
				case AFF_TAG_RELEASE:
					fiber->bracket_count++;
					fiber->attempts =
						aff_cons_new(
							aff_finalized_new(
								fiber->step == NULL
									? NULL
									: fiber->step->val,
								fiber->failure),
							fiber->attempts,
							fiber->interrupt);
					fiber->state = FIBER_STATE_CONTINUE;
					/* It has only been killed if the
					    interrupt status has changed since
					    we enqueued the item. */
					if (fiber->interrupt &&
					    fiber->interrupt != tmp) {
						fiber->step =
							step_aff_new(
								FROM_FOREIGN(
									app_2(attempt->release.bracket->bracket.killed,
									      utils_from_left(fiber->utils, fiber->interrupt),
									      attempt->release.result)));
					} else if (fiber->failure != NULL) {
						fiber->step =
							step_aff_new(
								FROM_FOREIGN(
									app_2(attempt->release.bracket->bracket.failed,
									      utils_from_left(fiber->utils, fiber->failure),
									      attempt->release.result)));
					} else {
						fiber->step =
							step_aff_new(
								FROM_FOREIGN(
									app_2(attempt->release.bracket->bracket.completed,
									      utils_from_right(fiber->utils, fiber->step->val),
									      attempt->release.result)));
					}

					fiber->failure = NULL;
					break;

				case AFF_TAG_FINALIZED:
					fiber->bracket_count--;
					fiber->state = FIBER_STATE_RETURN;
					fiber->step = step_val_new(attempt->finalized.result);
					fiber->failure = attempt->finalized.failure;
					break;

				default:
					assert(0 /* not implemented */);
				}
			}
			break;

		case FIBER_STATE_PENDING:
			return;

		case FIBER_STATE_COMPLETED: {
			/* Invoke join handlers */
			join_table_t * entry, * tmp;
			assert(fiber->step != NULL);
			assert(fiber->step->tag == STEP_TAG_VAL);

			HASH_ITER(hh, fiber->joins, entry, tmp) {
				fiber->rethrow = fiber->rethrow && entry->join.rethrow;
				app_2(entry->join.callback,
				      fiber->step->val,
				      NULL);
			}
			fiber->joins = NULL;

			/* If we have an interrupt and a fail, then the thread
			   threw while running finalizers. This is irrecoverable
			   */
			printf("interrupt:%p/failure:%p/rethrow:%i/is-left:%i\n",
			       fiber->interrupt,
			       fiber->failure,
			       fiber->rethrow,
			       utils_is_left(fiber->utils, fiber->step->val));

			if (fiber->interrupt != NULL && fiber->failure != NULL) {
				app_3(fiber->set_timeout,
				      purs_any_int_new(0),
				      app_3(onCompletedCheckRethrow,
					    TO_FOREIGN(fiber),
					    utils_from_left(fiber->utils, fiber->failure),
					    purs_any_false),
				      NULL);
			} else if (utils_is_left(fiber->utils,
						 fiber->step->val) && fiber->rethrow) {
				app_3(fiber->set_timeout,
				      purs_any_int_new(0),
				      app_3(onCompletedCheckRethrow,
					    TO_FOREIGN(fiber),
					    utils_from_left(fiber->utils, fiber->step->val),
					    purs_any_true),
				      NULL);
			}
			return;
		}
		}
	}
}

PURS_FFI_FUNC_10(Effect_Aff_makeFiberImpl,
		is_left,
		is_right,
		from_left,
		from_right,
		Left,
		Right,
		set_timeout,
		on_uncaught_error,
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

	return TO_FOREIGN(fiber_new(utils,
				    set_timeout,
				    on_uncaught_error,
				    FROM_FOREIGN(aff)));
});

PURS_FFI_FUNC_2(Effect_Aff__catchError, aff, k, {
	return TO_FOREIGN(aff_catch_new(FROM_FOREIGN(aff), k));
});

PURS_FFI_FUNC_1(Effect_Aff__throwError, e, {
	return TO_FOREIGN(aff_throw_new(e));
});

PURS_FFI_FUNC_1(Effect_Aff_makeAff, k, {
	return TO_FOREIGN(aff_async_new(k));
});

PURS_FFI_FUNC_3(runFiberThunk, _fiber, _tick, _, {
	fiber_t *fiber = FROM_FOREIGN(_fiber);
	fiber_run(fiber, purs_any_get_int(_tick));
	return NULL;
});

PURS_FFI_FUNC_2(Effect_Aff_runFiber, _fiber, _, {
	fiber_t *fiber = FROM_FOREIGN(_fiber);
	if (scheduler_is_draining(fiber->scheduler) == 0) {
		scheduler_enqueue(fiber->scheduler,
				  app_2(runFiberThunk,
					_fiber,
					purs_any_int_new(fiber->run_tick)));
	} else {
		fiber_run(fiber, fiber->run_tick);
	}
	return NULL;
});

PURS_FFI_FUNC_1(Effect_Aff__pure, a, {
	return TO_FOREIGN(aff_pure_new(a));
});

const purs_any_t * aff_bind (const void * ctx, const purs_any_t * arg, va_list _) {
	const purs_any_t * f = ctx;
	return TO_FOREIGN(aff_pure_new(app_1(f, arg)));
}

PURS_FFI_FUNC_2(Effect_Aff__map, f, _aff, {
	const aff_t * aff = FROM_FOREIGN(_aff);
	if (aff->tag == AFF_TAG_PURE) {
		return TO_FOREIGN(
			aff_pure_new(app_1(f, aff->pure.value0)));
	} else {
		return TO_FOREIGN(
			aff_bind_new(aff,
				     purs_any_cont_new((void *) f, aff_bind)));
	}
});

const purs_any_t * aff_throw (const void * ctx, const purs_any_t * arg, va_list _) {
	const purs_any_t * f = ctx;
	return TO_FOREIGN(aff_throw_new(app_1(f, arg)));
}

PURS_FFI_FUNC_3(Effect_Aff__bimap, lf, rf, _aff, {
	const aff_t * aff = FROM_FOREIGN(_aff);
	if (aff->tag == AFF_TAG_PURE) {
		return TO_FOREIGN(aff_pure_new(app_1(rf, aff->pure.value0)));
	} else if (aff->tag == AFF_TAG_THROW) {
		return TO_FOREIGN(aff_throw_new(app_1(lf, aff->throw.value0)));
	} else {
		return TO_FOREIGN(
			aff_bind_new(aff_catch_new(aff,
						   purs_any_cont_new((void *) lf,
								     aff_throw)),
				      purs_any_cont_new((void *) rf,
							aff_bind)));
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

PURS_FFI_FUNC_2(Effect_Aff_isSuspended, _fiber, _, {
	const fiber_t * fiber = FROM_FOREIGN(_fiber);
	return purs_any_int_new(fiber->state == FIBER_STATE_SUSPENDED);
});

PURS_FFI_FUNC_3(Effect_Aff_generalBracket, acquire, _options, k, {
	const purs_record_t* options = purs_any_get_record(_options);
	const purs_any_t * killed = purs_record_find_by_key(options, "killed")->value;
	const purs_any_t * failed = purs_record_find_by_key(options, "failed")->value;
	const purs_any_t * completed = purs_record_find_by_key(options, "completed")->value;
	return TO_FOREIGN(aff_bracket_new(FROM_FOREIGN(acquire),
					  completed,
					  killed,
					  failed,
					  k));
});
