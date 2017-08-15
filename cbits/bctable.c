#include "bctable.h"
#include <stdlib.h>

struct inline_java_pack *inline_java_new_pack(
	struct inline_java_pack *next,
	struct inline_java_dot_class classes[],
	size_t size)
{
	struct inline_java_pack *new = malloc(sizeof(struct inline_java_pack));
	new->next = next;
	new->size = size;
	new->classes = classes;

	return new;
}

struct inline_java_pack *inline_java_bctable;
