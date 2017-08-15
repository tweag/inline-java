#include <stdlib.h>

/** List of JVM class bytecode definitions. The content corresponds to
 * that of a .class file. */
struct inline_java_dot_class {
	char *name;
	size_t bytecode_sz;
	unsigned char *bytecode;
};


/** A set of .class files contents. */
struct inline_java_pack {
	struct inline_java_pack *next;
	struct inline_java_dot_class *classes;
	size_t size;
};

/** Smart constructor for class file packs. */
struct inline_java_pack *inline_java_new_pack(
	struct inline_java_pack *next,
	struct inline_java_dot_class classes[],
	size_t size);

/** Global list of class files.
 *
 * All modules insert the bytecode of the classes they need when they
 * are loaded.
 *
 * inline-java reads this table to load the classes in loadJavaWrappers.
 */
extern struct inline_java_pack *inline_java_bctable;
