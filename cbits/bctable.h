/// A type of bytecodes
struct inline_java_dot_class {
	char *ij_dc_class_name;
	int ij_dc_bc_count;
	unsigned char *ij_dc_class_bytecode;
};

/// A type of linked lists for bytecode
///
/// NULL is the empty linked list.
struct inline_java_linked_list {
	int ij_ll_dc_count;
	struct inline_java_dot_class *ij_ll_dc;
	struct inline_java_linked_list *ij_ll_next;
};

/// The bytecode table
///
/// All modules insert the bytecode of the classes they need when
/// they are loaded.
///
/// inline-java reads this table to load the classes in loadJavaWrappers.
extern struct inline_java_linked_list *inline_java_bctable;

/// Adds an array of bytecodes at the front of the bctable.
///
/// dc_count indicates the amount of bytecodes in the array.
void inline_java_linked_list_cons(struct inline_java_dot_class *dc, int dc_count);
