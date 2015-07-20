#include "arc.h"

void print_logo() {
	printf("Arcadia %s\n", VERSION);
}

void repl() {
	char *input;

	while ((input = readline("> ")) != NULL) {
		int ss = stack_size;
	read_start:
		arc_reader_unclosed = 0;
#ifdef READLINE
		if (input && *input)
			add_history(input);
#endif

		char *buf = (char *)malloc(strlen(input) + 4);
		sprintf(buf, "(%s\n)", input);
		const char *p = buf;
		error err;
		atom result;

		atom code_expr;
		err = read_expr(p, &p, &code_expr);
		if (arc_reader_unclosed > 0) { /* read more lines */
			char *line = readline("  ");
			if (!line) break;
			input = strcat_alloc(&input, "\n");
			input = strcat_alloc(&input, line);
			goto read_start;
		}
		stack_add(code_expr);
		if (!err) {
			while (!no(code_expr)) {
				err = macex_eval(car(code_expr), &result);
				if (err) {
					print_error(err);
					break;
				}
				else {
					print_expr(result);
					putchar('\n');
				}
				code_expr = cdr(code_expr);
			}
		}
		stack_restore(ss);
		free(buf);
		free(input);
	}
}

int main(int argc, char **argv)
{
	if (argc == 1) { /* REPL */
		print_logo();
		arc_init(argv[0]);
		repl();
		puts("");
		return 0;
	}
	else if (argc == 2) {
		char *opt = argv[1];
		if (strcmp(opt, "-h") == 0) {
			puts("Usage: arcadia [OPTIONS...] [FILES...]");
			puts("");
			puts("OPTIONS:");
			puts("    -h    print this screen.");
			puts("    -v    print version.");
			return 0;
		}
		else if (strcmp(opt, "-v") == 0) {
			puts(VERSION);
			return 0;
		}
	}

	/* execute files */
	arc_init(argv[0]);
	int i;
	error err;
	for (i = 1; i < argc; i++) {
		err = arc_load_file(argv[i]);
		if (err) {
			fprintf(stderr, "Cannot open file: %s\n", argv[i]);
		}
	}
	return 0;
}
