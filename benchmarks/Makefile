B := ../_build/default/benchmarks/bench.exe
WARMUP := 5
ITER := 5
bench : test_params counter skiplist btree

test_params :
	dune build
	printf "TESTPARAMS -- Warmup = %d, Iterations = %d\n" $(WARMUP) $(ITER)

C_OPS := 10_000_000
counter : counter.ml
	printf "Counter-Sequential\n"
	$(B) counter-sequential --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP)

	printf "Counter-Coarse-Grained\n"
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-coarse-grained --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8

	printf "Counter-Batched\n"
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8

S_PRESET := 1_000_000
S_INS := 1_000_000
# Need to fix assertion error with search
S_SEARCH := 0
S_SIZE := 0
skiplist : skiplist.ml
	printf "Skiplist-Sequential\n"
	$(B) skiplist-sequential --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP)

	printf "Skiplist-Coarse-Grained\n"
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 1
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 2
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 3
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 4
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 5
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 6
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 7
	$(B) skiplist-coarse-grained --init-count=$(S_PRESET) --count=$(S_INS) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -i $(ITER) -w $(WARMUP) -D 8

	printf "Skiplist-Batched\n"
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

	printf "Skiplist-Lazy\n"
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

B_PRESET := 0
B_OPS := 1_000_000
btree : btree.ml
	printf "Btree-Sequential\n"
	$(B) btree-sequential --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP)

	printf "Btree-Coarse-Grained\n"
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-coarse-grained --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

	printf "Btree-Batched\n"
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

btree_batched :
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8

skiplist_lazy :
	printf "Skiplist-Lazy\n"
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-lazy --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

skiplist_explicit :
	printf "Skiplist-explicit\n"
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-explicit --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

btree_batched_validate :
	printf "Btree-batched\n"
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) btree-batched --init-count=$(B_PRESET) --count=$(B_OPS) -i $(ITER) -w $(WARMUP) -D 8 -T

skiplist_batched :
	printf "Skiplist-Batched\n"
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

counter_batched :
	printf "Counter-Batched\n"
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 1
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 2
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 3
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 4
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 5
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 6
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 7
	$(B) counter-batched --count=$(C_OPS) --rand -i $(ITER) -w $(WARMUP) -D 8


AVL_PRESET := 2_000_000
AVL_OPS := 0
AVL_SEARCH := 1_000_000
avltree : avltree.ml
	printf "Avltree-Sequential\n"
	$(B) avltree-sequential --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T

	printf "Avltree-Batched\n"
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 8 -T

avltree-batched :
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) avltree-batched --init-count=$(AVL_PRESET) --count=$(AVL_OPS) --no-searches=$(AVL_SEARCH) -i $(ITER) -w $(WARMUP) -D 8 -T


RB_PRESET := 2_000_000
RB_OPS := 1_000_000
RB_SEARCH := 0
rbtree : rbtree.ml
	printf "rbtree-Sequential\n"
	$(B) rbtree-sequential --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP)

	printf "rbtree-Batched\n"
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 8 -T

rbtree-explicitly-batched :
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) rbtree-explicitly-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 8 -T

rbtree-batched :
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 1
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 2
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 3
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 4
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 5
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 6
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 7
	$(B) rbtree-batched --init-count=$(RB_PRESET) --count=$(RB_OPS) --no-searches=$(RB_SEARCH) -i $(ITER) -w $(WARMUP) -D 8


TREAP_PRESET := 2_000_000
TREAP_OPS := 1_000_000
TREAP_SEARCH := 0
treap : treap.ml
	printf "treap-Sequential\n"
	$(B) treap-sequential --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -T

	printf "treap-Batched\n"
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 1 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 2 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 3 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 4 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 5 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 6 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 7 -T
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 8 -T

treap-batched :
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 1
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 2
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 3
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 4
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 5
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 6
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 7
	$(B) treap-batched --init-count=$(TREAP_PRESET) --count=$(TREAP_OPS) --no-searches=$(TREAP_SEARCH) -i $(ITER) -w $(WARMUP) -D 8


VEB_PRESET := 2_000_000
VEB_INS := 1_000_000
VEB_SEARCH := 0
vebtree : vebtree.ml
	printf "vebtree-Sequential\n"
	$(B) vebtree-sequential --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP)

	# printf "vebtree-Coarse-Grained\n"
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 1
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 2
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 3
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 4
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 5
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 6
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 7
	# $(B) vebtree-coarse-grained --init-count=$(VEB_PRESET) --count=$(VEB_INS) --no-searches=$(VEB_SEARCH) -i $(ITER) -w $(WARMUP) -D 8

	printf "vebtree-Batched\n"
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 1
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 2
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 3
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 4
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 5
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 6
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 7
	$(B) vebtree-batched --init-count=$(VEB_PRESET) --count=$(VEB_INS) -i $(ITER) --no-searches=$(VEB_SEARCH) -w $(WARMUP) -D 8


XFAST_PRESET := 2_000_000
XFAST_INS := 1_000_000
XFAST_SEARCH := 0
xfast : xfast.ml
	printf "xfast-Sequential\n"
	$(B) xfast-sequential --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP)

	# printf "xfast-Coarse-Grained\n"
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 1
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 2
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 3
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 4
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 5
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 6
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 7
	# $(B) xfast-coarse-grained --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) --no-searches=$(XFAST_SEARCH) -i $(ITER) -w $(WARMUP) -D 8

	printf "xfast-Batched\n"
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 1
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 2
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 3
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 4
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 5
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 6
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 7
	$(B) xfast-batched --init-count=$(XFAST_PRESET) --count=$(XFAST_INS) -i $(ITER) --no-searches=$(XFAST_SEARCH) -w $(WARMUP) -D 8

YFAST_PRESET := 2_000_000
YFAST_INS := 1_000_000
YFAST_SEARCH := 0
yfast : yfast.ml
	printf "yfast-Sequential\n"
	$(B) yfast-sequential --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) --no-searches=$(YFAST_SEARCH) -i $(ITER) -w $(WARMUP)

	printf "yfast-Batched\n"
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 1
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 2
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 3
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 4
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 5
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 6
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 7
	$(B) yfast-batched --init-count=$(YFAST_PRESET) --count=$(YFAST_INS) -i $(ITER) --no-searches=$(YFAST_SEARCH) -w $(WARMUP) -D 8


S_PRESET := 1_000_000
S_INS := 2_000_000
S_SEARCH := 0
S_SIZE := 0
skiplist-batched :
	printf "Skiplist-Batched\n"
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 1
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 2
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 3
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 4
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 5
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 6
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 7
	$(B) skiplist-batched --init-count=$(S_PRESET) --count=$(S_INS) -i $(ITER) --no-searches=$(S_SEARCH) --no-size=$(S_SIZE) -w $(WARMUP) -D 8

D_PRESET := 30_000
D_NODES := 200
D_INSERT := 5_000
D_SEARCH := 45_000
datalog-batched :
	printf "Datalog-Batched\n"
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 1
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 2
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 3
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 4
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 5
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 6
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 7
	$(B) datalog-batched --init-count=$(D_PRESET) --graph-nodes=$(D_NODES) --count=$(D_INSERT) -i $(ITER) --no-searches=$(D_SEARCH) -w $(WARMUP) -D 8