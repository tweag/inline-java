#include <Rts.h>
#include "bctable.h"

struct inline_java_linked_list* inline_java_bctable;

void inline_java_linked_list_cons(struct inline_java_dot_class *dc, int dc_count) {
	struct inline_java_linked_list *n =
		(struct inline_java_linked_list*)
		malloc(sizeof(struct inline_java_linked_list));
	n->count = dc_count;
	n->element = dc;
	n->next = inline_java_bytecode_table;
	inline_java_bytecode_table = n;
}
;
