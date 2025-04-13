#include "arc.h"
#define VERSION "0.40"

void print_logo() {
	printf("Arcadia %s\n", VERSION);
}

void repl() {
	struct string input;

	while ((input.str = readline("> ")) != NULL) {
		input.cap = (input.len = strlen(input.str)) + 1;
	read_start:;

		const char *p = input.str;
		error err;

		atom expr;
		err = read_expr(p, &p, &expr);
		if (err == ERROR_FILE) { /* read more lines */
			char *line = readline("  ");
			if (!line) break;
			string_cat(&input, "\n");
			string_cat(&input, line);
			free(line);
			goto read_start;
		}

#ifdef READLINE
		add_history(input.str);
#endif

		if (!err) {
			while (1) {
				atom result;
				error err = macex_eval(expr, &result);
				if (err) {
					print_error(err);
					printf("error in expression:\n");
					print_expr(expr);
					putchar('\n');
					break;
				}
				else {
					print_expr(result);
					puts("");
				}
				err = read_expr(p, &p, &expr);
				if (err != ERROR_OK) {
					break;
				}
			}
		} else {
			print_error(err);
		}
		free(input.str);
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
			fprintf(stderr, "In file %s:\n", argv[i]);
			print_error(err);
			break;
		}
	}
	return 0;
}
