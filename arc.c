#include "arc.h"

char *error_string[] = { "", "Syntax error", "Symbol not bound", "Wrong number of arguments", "Wrong type", "File error", "" };
int stack_capacity = 0;
int stack_size = 0;
atom *stack = NULL;
struct pair *pair_head = NULL;
struct str *str_head = NULL;
struct table *table_head = NULL;
int alloc_count = 0;
int alloc_count_old = 0;
char **symbol_table = NULL;
int symbol_size = 0;
int symbol_capacity = 0;
const atom nil = { T_NIL };
atom env; /* the global environment */
atom sym_t, sym_quote, sym_assign, sym_fn, sym_if, sym_mac, sym_apply, sym_while, sym_cons, sym_sym, sym_string, sym_num, sym__, sym_o, sym_table, sym_int, sym_char;
atom cur_expr;
int arc_reader_unclosed = 0;
atom thrown;

void stack_add(atom a) {
	switch (a.type) {
	case T_CONS:
	case T_CLOSURE:
	case T_MACRO:
	case T_STRING:
	case T_TABLE:
		break;
	default:
		return;
	}
	stack_size++;
	if (stack_size > stack_capacity) {
		stack_capacity = stack_size * 2;
		stack = realloc(stack, stack_capacity * sizeof(atom));
	}
	stack[stack_size - 1] = a;
}

void stack_restore(int saved_size) {
	stack_size = saved_size;
	/* if there is waste of memory, realloc */
	if (stack_size < stack_capacity / 4) {
		stack_capacity /= 2;
		stack = realloc(stack, stack_capacity * sizeof(atom));
	}
}

void consider_gc() {
	if (alloc_count > 4 * alloc_count_old) {
		gc();
	}
}

atom cons(atom car_val, atom cdr_val)
{
	struct pair *a;
	atom p;

	alloc_count++;
	consider_gc();

	a = malloc(sizeof(struct pair));
	a->mark = 0;
	a->next = pair_head;
	pair_head = a;

	p.type = T_CONS;
	p.value.pair = a;

	car(p) = car_val;
	cdr(p) = cdr_val;

	stack_add(p);

	return p;
}

void gc_mark(atom root)
{
	struct pair *a;
	struct str *as;
	struct table *at;

	switch (root.type) {
	case T_CONS:
	case T_CLOSURE:
	case T_MACRO:
		a = root.value.pair;
		if (a->mark) return;
		a->mark = 1;
		gc_mark(car(root));
		gc_mark(cdr(root));
		break;
	case T_STRING:
		as = root.value.str;
		if (as->mark) return;
		as->mark = 1;
		break;
	case T_TABLE: {
		at = root.value.table;
		if (at->mark) return;
		at->mark = 1;
		int i;
		for (i = 0; i < at->capacity; i++) {
			gc_mark(at->data[i]);
		}
		break; }
	default:
		return;
	}
}

void gc()
{
	struct pair *a, **p;
	struct str *as, **ps;
	struct table *at, **pt;

	/* mark atoms in the stack */
	int i;
	for (i = 0; i < stack_size; i++) {
		gc_mark(stack[i]);
	}

	alloc_count_old = 0;
	/* Free unmarked "cons" allocations */
	p = &pair_head;
	while (*p != NULL) {
		a = *p;
		if (!a->mark) {
			*p = a->next;
			free(a);
		}
		else {
			p = &a->next;
			a->mark = 0; /* clear mark */
			alloc_count_old++;
		}
	}

	/* Free unmarked "string" allocations */
	ps = &str_head;
	while (*ps != NULL) {
		as = *ps;
		if (!as->mark) {
			*ps = as->next;
			free(as->value);
			free(as);
		}
		else {
			ps = &as->next;
			as->mark = 0; /* clear mark */
			alloc_count_old++;
		}
	}

	/* Free unmarked "table" allocations */
	pt = &table_head;
	while (*pt != NULL) {
		at = *pt;
		if (!at->mark) {
			*pt = at->next;
			free(at->data);
			free(at);
		}
		else {
			pt = &at->next;
			at->mark = 0; /* clear mark */
			alloc_count_old++;
		}
	}
	alloc_count = alloc_count_old;
}


atom make_number(double x)
{
	atom a;
	a.type = T_NUM;
	a.value.number = x;
	return a;
}

atom make_sym(const char *s)
{
	atom a;

	int i;
	for (i = 0; i < symbol_size; i++) {
		char *s2 = symbol_table[i];
		if (strcmp(s2, s) == 0) {
			a.type = T_SYM;
			a.value.symbol = s2;
			return a;
		}
	}

	a.type = T_SYM;
	a.value.symbol = (char*)strdup(s);
	if (symbol_size >= symbol_capacity) {
		symbol_capacity *= 2;
		symbol_table = realloc(symbol_table, symbol_capacity * sizeof(char *));
	}
	symbol_table[symbol_size] = a.value.symbol;
	symbol_size++;
	return a;
}

atom make_builtin(builtin fn)
{
	atom a;
	a.type = T_BUILTIN;
	a.value.builtin = fn;
	return a;
}

error make_closure(atom env, atom args, atom body, atom *result)
{
	atom p;

	if (!listp(body))
		return ERROR_SYNTAX;

	/* Check argument names are all symbols or conses */
	p = args;
	while (!no(p)) {
		if (p.type == T_SYM)
			break;
		else if (p.type != T_CONS	||
						 (car(p).type != T_SYM && car(p).type != T_CONS))
			return ERROR_TYPE;
		if (car(p).type == T_CONS && !is(car(car(p)), sym_o))
			return ERROR_SYNTAX;
		p = cdr(p);
	}

	*result = cons(env, cons(args, body));
	result->type = T_CLOSURE;

	return ERROR_OK;
}

atom make_string(char *x)
{
	atom a;
	struct str *s;
	alloc_count++;
	consider_gc();
	s = a.value.str = malloc(sizeof(struct str));
	s->value = x;
	s->mark = 0;
	s->next = str_head;
	str_head = s;

	a.type = T_STRING;
	stack_add(a);
	return a;
}

atom make_input(FILE *fp) {
	atom a;
	a.type = T_INPUT;
	a.value.fp = fp;
	return a;
}

atom make_output(FILE *fp) {
	atom a;
	a.type = T_OUTPUT;
	a.value.fp = fp;
	return a;
}

atom make_char(char c) {
	atom a;
	a.type = T_CHAR;
	a.value.ch = c;
	return a;
}

void print_expr(atom a)
{
	char *s = to_string(a, 1);
	printf("%s", s);
	free(s);
}

void pr(atom a)
{
	char *s = to_string(a, 0);
	printf("%s", s);
	free(s);
}

error lex(const char *str, const char **start, const char **end)
{
	const char *ws = " \t\r\n";
	const char *delim = "()[] \t\r\n;";
	const char *prefix = "()[]'`";
start:
	str += strspn(str, ws);

	if (str[0] == '\0') {
		*start = *end = NULL;
		return ERROR_SYNTAX;
	}

	*start = str;

	if (strchr(prefix, str[0]) != NULL)
		*end = str + 1;
	else if (str[0] == ',')
		*end = str + (str[1] == '@' ? 2 : 1);
	else if (str[0] == '"') {
		arc_reader_unclosed++;
		str++;
		while (*str != 0) {
			if (*str == '\\') str++;
			else if (*str == '"') {
				arc_reader_unclosed--;
				break;
			}
			str++;
		}
		*end = str + 1;
	}
	else if (str[0] == ';') { /* end-of-line comment */
		str += strcspn(str, "\n");
		goto start;
	}
	else
		*end = str + strcspn(str, delim);

	return ERROR_OK;
}

error parse_simple(const char *start, const char *end, atom *result)
{
	char *buf, *p;

	/* Is it a number? */
	double val = strtod(start, &p);
	if (p == end) {
		result->type = T_NUM;
		result->value.number = val;
		return ERROR_OK;
	}
	else if (start[0] == '"') { /* "string" */
		result->type = T_STRING;
		size_t length = end - start - 2;
		char *buf = (char*)malloc(length + 1);
		const char *ps = start + 1;
		char *pt = buf;
		while (ps < end - 1) {
			if (*ps == '\\') {
				char c_next = *(ps + 1);
				switch (c_next) {
				case 'r':
					*pt = '\r';
					break;
				case 'n':
					*pt = '\n';
					break;
				case 't':
					*pt = '\t';
					break;
				default:
					*pt = c_next;
				}
				ps++;
			}
			else {
				*pt = *ps;
			}
			ps++;
			pt++;
		}
		*pt = 0;
		buf = realloc(buf, pt - buf + 1);
		*result = make_string(buf);
		return ERROR_OK;
	}
	else if (start[0] == '#') { /* #\char */
		buf = malloc(end - start + 1);
		memcpy(buf, start, end - start);
		buf[end - start] = 0;
		size_t length = strlen(buf);
		if (length == 3 && buf[1] == '\\') { /* plain character e.g. #\a */
			*result = make_char(buf[2]);
			free(buf);
			return ERROR_OK;
		}
		else {
			char c;
			if (strcmp(buf, "#\\nul") == 0)
				c = '\0';
			else if (strcmp(buf, "#\\return") == 0)
				c = '\r';
			else if (strcmp(buf, "#\\newline") == 0)
				c = '\n';
			else if (strcmp(buf, "#\\tab") == 0)
				c = '\t';
			else if (strcmp(buf, "#\\space") == 0)
				c = ' ';
			else {
				free(buf);
				return ERROR_SYNTAX;
			}
			free(buf);
			*result = make_char(c);
			return ERROR_OK;
		}
	}

	/* NIL or symbol */
	buf = malloc(end - start + 1);
	memcpy(buf, start, end - start);
	buf[end - start] = 0;

	if (strcmp(buf, "nil") == 0)
		*result = nil;
	else if (strcmp(buf, ".") == 0)
		*result = make_sym(buf);
	else {
		atom a1, a2;
		long length = end - start, i;
		for (i = length - 1; i >= 0; i--) { /* left-associative */
			if (buf[i] == '.') { /* a.b => (a b) */
				if (i == 0 || i == length - 1) {
					free(buf);
					return ERROR_SYNTAX;
				}
				error err;
				err = parse_simple(buf, buf + i, &a1);
				if (err) return ERROR_SYNTAX;
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) return ERROR_SYNTAX;
				free(buf);
				*result = cons(a1, cons(a2, nil));
				return ERROR_OK;
			} else if (buf[i] == '!') { /* a!b => (a 'b) */
				if (i == 0 || i == length - 1) {
					free(buf);
					return ERROR_SYNTAX;
				}
				error err;
				err = parse_simple(buf, buf + i, &a1);
				if (err) return ERROR_SYNTAX;
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) return ERROR_SYNTAX;
				free(buf);
				*result = cons(a1, cons(cons(sym_quote, cons(a2, nil)), nil));
				return ERROR_OK;
			} else if (buf[i] == ':') { /* a:b => (compose a b) */
				if (i == 0 || i == length - 1) {
					free(buf);
					return ERROR_SYNTAX;
				}
				error err;
				err = parse_simple(buf, buf + i, &a1);
				if (err) return ERROR_SYNTAX;
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) return ERROR_SYNTAX;
				free(buf);
				*result = cons(make_sym("compose"), cons(a1, cons(a2, nil)));
				return ERROR_OK;
			}
		}
		if (length >= 2 && buf[0] == '~') { /* ~a => (complement a) */
			atom a1;
			error err = parse_simple(buf + 1, buf + length, &a1);
			if (err) return ERROR_SYNTAX;
			*result = cons(make_sym("complement"), cons(a1, nil));
			return ERROR_OK;
		}
		*result = make_sym(buf);
	}

	free(buf);

	return ERROR_OK;
}

error read_list(const char *start, const char **end, atom *result)
{
	atom p;

	*end = start;
	p = *result = nil;

	for (;;) {
		const char *token;
		atom item;
		error err;

		err = lex(*end, &token, end);
		if (err)
			return err;

		if (token[0] == ')') {
			arc_reader_unclosed--;
			return ERROR_OK;
		}

		if (!no(p) && token[0] == '.' && *end - token == 1) {
			/* Improper list */
			if (no(p)) return ERROR_SYNTAX;

			err = read_expr(*end, end, &item);
			if (err) return err;

			cdr(p) = item;

			/* Read the closing ')' */
			err = lex(*end, &token, end);
			if (!err && token[0] != ')') {
				err = ERROR_SYNTAX;
			}
			arc_reader_unclosed--;
			return err;
		}

		err = read_expr(token, end, &item);
		if (err)
			return err;

		if (no(p)) {
			/* First item */
			*result = cons(item, nil);
			p = *result;
		}
		else {
			cdr(p) = cons(item, nil);
			p = cdr(p);
		}
	}
}

/* [...] => (fn (_) (...)) */
error read_bracket(const char *start, const char **end, atom *result)
{
	atom p;

	*end = start;
	p = *result = nil;

	/* First item */
	*result = cons(sym_fn, nil);
	p = *result;

	cdr(p) = cons(cons(sym__, nil), nil);
	p = cdr(p);

	atom body = nil;

	for (;;) {
		const char *token;
		atom item;
		error err;

		err = lex(*end, &token, end);
		if (err) return err;
		if (token[0] == ']') {
			arc_reader_unclosed--;
			return ERROR_OK;
		}

		err = read_expr(token, end, &item);
		if (err) return err;

		if (no(body)) {		  
		  body = cons(item, nil);
		  cdr(p) = cons(body, nil);
		  p = body;
		} else {
		  cdr(p) = cons(item, nil);
		  p = cdr(p);
		}
	}
}

error read_expr(const char *input, const char **end, atom *result)
{
	const char *token;
	error err;

	err = lex(input, &token, end);
	if (err)
		return err;

	if (token[0] == '(') {
		arc_reader_unclosed++;
		return read_list(*end, end, result);
	}
	else if (token[0] == ')')
		return ERROR_SYNTAX;
	else if (token[0] == '[') {
		arc_reader_unclosed++;
		return read_bracket(*end, end, result);
	}
	else if (token[0] == ']')
		return ERROR_SYNTAX;
	else if (token[0] == '\'') {
		*result = cons(make_sym("quote"), cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else if (token[0] == '`') {
		*result = cons(make_sym("quasiquote"), cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else if (token[0] == ',') {
		*result = cons(make_sym(
			token[1] == '@' ? "unquote-splicing" : "unquote"),
			cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else
		return parse_simple(token, *end, result);
}

#ifndef READLINE
char *readline(char *prompt) {
	size_t size = 80;
	/* The size is extended by the input with the value of the provisional */
	char *str;
	int ch;
	size_t len = 0;
	printf(prompt);
	str = malloc(sizeof(char)* size); /* size is start size */
	if (!str) return NULL;
	while (EOF != (ch = fgetc(stdin)) && ch != '\n') {
		str[len++] = ch;
		if (len == size){
			str = realloc(str, sizeof(char)*(size *= 2));
			if (!str) return NULL;
		}
	}
	if (ch == EOF && len == 0) return NULL;
	str[len++] = '\0';

	return realloc(str, sizeof(char)*len);
}
#endif /* READLINE */

char *readline_fp(char *prompt, FILE *fp) {
	size_t size = 80;
	/* The size is extended by the input with the value of the provisional */
	char *str;
	int ch;
	size_t len = 0;
	printf(prompt);
	str = malloc(sizeof(char)* size); /* size is start size */
	if (!str) return NULL;
	while (EOF != (ch = fgetc(fp)) && ch != '\n') {
		str[len++] = ch;
		if (len == size){
			str = realloc(str, sizeof(char)*(size *= 2));
			if (!str) return NULL;
		}
	}
	if (ch == EOF && len == 0) return NULL;
	str[len++] = '\0';

	return realloc(str, sizeof(char)*len);
}

atom env_create(atom parent)
{
	return cons(parent, make_table(2));
}

error env_get(atom env, char *symbol, atom *result)
{
	while (1) {
		atom parent = car(env);
		struct table *ptbl = cdr(env).value.table;
		struct pair *a = table_get_sym(ptbl, symbol);
		if (a) {
			*result = a->cdr;
			return ERROR_OK;
		}
		if (no(parent)) {
			/*printf("%s: ", symbol.value.symbol);*/
			return ERROR_UNBOUND;
		}
		env = parent;
	}
}

error env_assign(atom env, char *symbol, atom value) {
	struct table *ptbl = cdr(env).value.table;
	table_set_sym(ptbl, symbol, value);
	return ERROR_OK;
}

error env_assign_eq(atom env, char *symbol, atom value) {
	while (1) {
		atom parent = car(env);
		struct table *ptbl = cdr(env).value.table;
		struct pair *a = table_get_sym(ptbl, symbol);
		if (a) {
			a->cdr = value;
			return ERROR_OK;
		}
		if (no(parent)) {
			return env_assign(env, symbol, value);
		}
		env = parent;
	}
}

int listp(atom expr)
{
	atom *p = &expr;
	while (!no(*p)) {
		if (p->type != T_CONS)
			return 0;
		p = &cdr(*p);
	}
	return 1;
}

long len(atom xs) {
	atom *p = &xs;
	long ret = 0;
	while (!no(*p)) {
		if (p->type != T_CONS)
			return 0;
		p = &cdr(*p);
		ret++;
	}
	return ret;
}

atom copy_list(atom list)
{
	atom a, p;

	if (no(list))
		return nil;

	a = cons(car(list), nil);
	p = a;
	list = cdr(list);

	while (!no(list)) {
		cdr(p) = cons(car(list), nil);
		p = cdr(p);
		list = cdr(list);
		if (list.type != T_CONS) { /* improper list */
			p = list;
			break;
		}
	}

	return a;
}

error apply(atom fn, atom args, atom *result)
{
	atom env, arg_names, body;

	if (fn.type == T_BUILTIN)
		return (*fn.value.builtin)(args, result);
	else if (fn.type == T_CLOSURE) {
		env = env_create(car(fn));
		arg_names = car(cdr(fn));
		body = cdr(cdr(fn));

		/* Bind the arguments */
		while (!no(arg_names)) {
			if (arg_names.type == T_SYM) {
				env_assign(env, arg_names.value.symbol, args);
				args = nil;
				break;
			}
			atom arg_name = car(arg_names);
			if (arg_name.type == T_SYM) {
				if (no(args)) /* missing argument */
					return ERROR_ARGS;
				env_assign(env, arg_name.value.symbol, car(args));
				args = cdr(args);
			}
			else { /* (o ARG [DEFAULT]) */
				atom val;
				if (no(args)) { /* missing argument */
					if (no(cdr(cdr(arg_name))))
						val = nil;
					else {
						error err = eval_expr(car(cdr(cdr(arg_name))), env, &val);
						if (err) return err;
					}
				} else {
					val = car(args);
					args = cdr(args);
				}
				env_assign(env, car(cdr(arg_name)).value.symbol, val);
			}
			arg_names = cdr(arg_names);
		}
		if (!no(args))
			return ERROR_ARGS;

		/* Evaluate the body */
		*result = nil;
		while (!no(body)) {
			error err = eval_expr(car(body), env, result);
			if (err)
				return err;
			body = cdr(body);
		}

		return ERROR_OK;
	}
	else if (fn.type == T_CONTINUATION) {
		if (len(args) != 1) return ERROR_ARGS;
		thrown = car(args);
		longjmp(*fn.value.jb, 1);
	}
	else if (fn.type == T_STRING) { /* implicit indexing for string */
		if (len(args) != 1) return ERROR_ARGS;
		long index = (long)(car(args)).value.number;
		*result = make_char(fn.value.str->value[index]);
		return ERROR_OK;
	}
	else if (fn.type == T_CONS && listp(fn)) { /* implicit indexing for list */
		if (len(args) != 1) return ERROR_ARGS;
		long index = (long)(car(args)).value.number;
		atom a = fn;
		long i;
		for (i = 0; i < index; i++) {
			a = cdr(a);
			if (no(a)) {
				*result = nil;
				return ERROR_OK;
			}
		}
		*result = car(a);
		return ERROR_OK;
	}
	else if (fn.type == T_TABLE) { /* implicit indexing for table */
		long len1 = len(args);
		if (len1 != 1 && len1 != 2) return ERROR_ARGS;
		atom *pkey = &car(args);
		struct pair *pair = table_get(fn.value.table, *pkey);
		if (pair) {
			*result = pair->cdr;
		}
		else {
			if (len1 == 2) /* default value is specified */
				*result = car(cdr(args));
			else
				*result = nil;
		}
		return ERROR_OK;
	}
	else {
		return ERROR_TYPE;
	}
}

error builtin_car(atom args, atom *result)
{
	if (no(args) || !no(cdr(args)))
		return ERROR_ARGS;

	if (no(car(args)))
		*result = nil;
	else if (car(args).type != T_CONS)
		return ERROR_TYPE;
	else
		*result = car(car(args));

	return ERROR_OK;
}

error builtin_cdr(atom args, atom *result)
{
	if (no(args) || !no(cdr(args)))
		return ERROR_ARGS;

	if (no(car(args)))
		*result = nil;
	else if (car(args).type != T_CONS)
		return ERROR_TYPE;
	else
		*result = cdr(car(args));

	return ERROR_OK;
}

error builtin_cons(atom args, atom *result)
{
	if (no(args) || no(cdr(args)) || !no(cdr(cdr(args))))
		return ERROR_ARGS;

	*result = cons(car(args), car(cdr(args)));

	return ERROR_OK;
}

/* appends two lists */
atom append(atom a, atom b) {
	atom a1 = copy_list(a),
		b1 = copy_list(b);
	atom p = a1;
	if (no(p)) return b1;
	while (1) {
		if (no(cdr(p))) {
			cdr(p) = b1;
			return a1;
		}
		p = cdr(p);
	}
	return nil;
}

/*
+ args
Addition. This operator also performs string and list concatenation.
*/
error builtin_add(atom args, atom *result)
{
	if (no(args)) {
		*result = make_number(0);
	}
	else {
		if (car(args).type == T_NUM) {
			double r = car(args).value.number;
			args = cdr(args);
			while (!no(args)) {
				if (args.type != T_CONS) return ERROR_ARGS;
				if (car(args).type != T_NUM) return ERROR_TYPE;
				r += car(args).value.number;
				args = cdr(args);
			}
			*result = make_number(r);
		}
		else if (car(args).type == T_STRING) {
			char *buf = str_new();
			while (!no(args)) {
				if (args.type != T_CONS) return ERROR_ARGS;
				char *s = to_string(car(args), 0);
				strcat_alloc(&buf, s);
				free(s);
				args = cdr(args);
			}
			*result = make_string(buf);
		}
		else if (car(args).type == T_CONS || car(args).type == T_NIL) {
			atom acc = nil;
			while (!no(args)) {
				if (args.type != T_CONS) return ERROR_ARGS;
				acc = append(acc, car(args));
				args = cdr(args);
			}
			*result = acc;
		}
	}
	return ERROR_OK;
}

error builtin_subtract(atom args, atom *result)
{
	if (no(args)) { /* 0 argument */
		*result = make_number(0);
		return ERROR_OK;
	}
	if (no(cdr(args))) { /* 1 argument */
		if (car(args).type != T_NUM) return ERROR_TYPE;
		*result = make_number(-car(args).value.number);
		return ERROR_OK;
	}
	if (car(args).type != T_NUM) return ERROR_TYPE;
	double r = car(args).value.number;
	args = cdr(args);
	while (!no(args)) {
		if (args.type != T_CONS) return ERROR_ARGS;
		if (car(args).type != T_NUM) return ERROR_TYPE;
		r -= car(args).value.number;
		args = cdr(args);
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_multiply(atom args, atom *result)
{
	double r = 1;
	while (!no(args)) {
		if (args.type != T_CONS) return ERROR_ARGS;
		if (car(args).type != T_NUM) return ERROR_TYPE;
		r *= car(args).value.number;
		args = cdr(args);
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_divide(atom args, atom *result)
{
	if (no(args)) { /* 0 argument */
		*result = make_number(1);
		return ERROR_OK;
	}
	if (no(cdr(args))) { /* 1 argument */
		if (car(args).type != T_NUM) return ERROR_TYPE;
		*result = make_number(1.0 / car(args).value.number);
		return ERROR_OK;
	}
	if (car(args).type != T_NUM) return ERROR_TYPE;
	double r = car(args).value.number;
	args = cdr(args);
	while (!no(args)) {
		if (args.type != T_CONS) return ERROR_ARGS;
		if (car(args).type != T_NUM) return ERROR_TYPE;
		r /= car(args).value.number;
		args = cdr(args);
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_less(atom args, atom *result)
{
	if (no(args) || no(cdr(args))) {
		*result = sym_t;
		return ERROR_OK;
	}
	switch (car(args).type) {
	case T_NUM:
		while (!no(cdr(args))) {
			if (car(args).value.number >= car(cdr(args)).value.number) {
				*result = nil;
				return ERROR_OK;
			}
			args = cdr(args);
		}
		*result = sym_t;
		return ERROR_OK;
	case T_STRING:
		while (!no(cdr(args))) {
			if (strcmp(car(args).value.str->value, car(cdr(args)).value.str->value) >= 0) {
				*result = nil;
				return ERROR_OK;
			}
			args = cdr(args);
		}
		*result = sym_t;
		return ERROR_OK;
	default:
		return ERROR_TYPE;
	}
}

error builtin_greater(atom args, atom *result)
{
  atom a, b;
  if (no(args) || no(cdr(args))) {
    *result = sym_t;
    return ERROR_OK;
  }
  switch (car(args).type) {
  case T_NUM:
    while (!no(cdr(args))) {
      a = car(args);
      b = car(cdr(args));
      if (a.value.number <= b.value.number) {
	*result = nil;
	return ERROR_OK;
      }
      args = cdr(args);
    }
    *result = sym_t;
    return ERROR_OK;
  case T_STRING:
    while (!no(cdr(args))) {
      a = car(args);
      b = car(cdr(args));
      if (strcmp(a.value.str->value, b.value.str->value) <= 0) {
	*result = nil;
	return ERROR_OK;
      }
      args = cdr(args);
    }
    *result = sym_t;
    return ERROR_OK;
  default:
    return ERROR_TYPE;
  }
}

error builtin_apply(atom args, atom *result)
{
	atom fn;

	if (no(args) || no(cdr(args)) || !no(cdr(cdr(args))))
		return ERROR_ARGS;

	fn = car(args);
	args = car(cdr(args));

	if (!listp(args))
		return ERROR_SYNTAX;

	return apply(fn, args, result);
}

int is(atom a, atom b) {
	if (a.type == b.type) {
		switch (a.type) {
		case T_NIL:
			return 1;
		case T_CONS:
		case T_CLOSURE:
		case T_MACRO:
			return (a.value.pair == b.value.pair);
		case T_SYM:
			return (a.value.symbol == b.value.symbol);
		case T_NUM:
			return (a.value.number == b.value.number);
		case T_BUILTIN:
			return (a.value.builtin == b.value.builtin);
		case T_STRING:
			return strcmp(a.value.str->value, b.value.str->value) == 0;
		case T_CHAR:
			return (a.value.ch == b.value.ch);
		case T_TABLE:
			return a.value.table == b.value.table;
		case T_INPUT:
		case T_OUTPUT:
			return a.value.fp == b.value.fp;
		case T_CONTINUATION:
			return a.value.jb == b.value.jb;
		}
	}
	return 0;
}

error builtin_is(atom args, atom *result)
{
  atom a, b;
  if (no(args) || no(cdr(args))) {
    *result = sym_t;
    return ERROR_OK;
  }
  while (!no(cdr(args))) {
    a = car(args);
    b = car(cdr(args));
    if (!is(a, b)) {
      *result = nil;
      return ERROR_OK;
    }
    args = cdr(args);
  }
  *result = sym_t;
  return ERROR_OK;
}

error builtin_scar(atom args, atom *result) {
	atom place = car(args), value;
	if (place.type != T_CONS) return ERROR_TYPE;
	value = car(cdr(args));
	place.value.pair->car = value;
	*result = value;
	return ERROR_OK;
}

error builtin_scdr(atom args, atom *result) {
	atom place = car(args), value;
	if (place.type != T_CONS) return ERROR_TYPE;
	value = car(cdr(args));
	place.value.pair->cdr = value;
	*result = value;
	return ERROR_OK;
}

error builtin_mod(atom args, atom *result) {
	atom dividend = car(args);
	atom divisor = car(cdr(args));
	double r = fmod(dividend.value.number, divisor.value.number);
	if (dividend.value.number * divisor.value.number < 0 && r != 0) r += divisor.value.number;
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_type(atom args, atom *result) {
	atom x = car(args);
	switch (x.type) {
	case T_CONS: *result = sym_cons; break;
	case T_SYM:
	case T_NIL: *result = sym_sym; break;
	case T_BUILTIN:
	case T_CLOSURE:
	case T_CONTINUATION:
		*result = sym_fn; break;
	case T_STRING: *result = sym_string; break;
	case T_NUM: *result = sym_num; break;
	case T_MACRO: *result = sym_mac; break;
	case T_TABLE: *result = sym_table; break;
	case T_CHAR: *result = sym_char; break;
	default: *result = nil; break; /* impossible */
	}
	return ERROR_OK;
}

/* string-sref obj value index */
error builtin_string_sref(atom args, atom *result) {
	atom index, obj, value;
	if (len(args) != 3) return ERROR_ARGS;
	index = car(cdr(cdr(args)));
	obj = car(args);
	if (obj.type != T_STRING) return ERROR_TYPE;
	value = car(cdr(args));
	obj.value.str->value[(long)index.value.number] = (char)value.value.ch;
	*result = make_char(value.value.ch);
	return ERROR_OK;
}

/* disp [arg [output-port]] */
error builtin_disp(atom args, atom *result) {
	long l = len(args);
	FILE *fp;
	switch (l) {
	case 0:
		*result = nil;
		return ERROR_OK;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = car(cdr(args)).value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	char *s = to_string(car(args), 0);
	fprintf(fp, "%s", s);
	free(s);
	*result = nil;
	return ERROR_OK;
}

error builtin_writeb(atom args, atom *result) {
	long l = len(args);
	FILE *fp;
	switch (l) {
	case 0: return ERROR_ARGS;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = car(cdr(args)).value.fp;
		break;
	default: return ERROR_ARGS;
	}
	fputc((int)car(args).value.number, fp);
	*result = nil;
	return ERROR_OK;
}

error builtin_expt(atom args, atom *result) {
	atom a, b;
	if (len(args) != 2) return ERROR_ARGS;
	a = car(args);
	b = car(cdr(args));
	*result = make_number(pow(a.value.number, b.value.number));
	return ERROR_OK;
}

error builtin_log(atom args, atom *result) {
	atom a;
	if (len(args) != 1) return ERROR_ARGS;
	a = car(args);
	*result = make_number(log(a.value.number));
	return ERROR_OK;
}

error builtin_sqrt(atom args, atom *result) {
	atom a;
	if (len(args) != 1) return ERROR_ARGS;
	a = car(args);
	*result = make_number(sqrt(a.value.number));
	return ERROR_OK;
}

error builtin_readline(atom args, atom *result) {
	long l = len(args);
	char *str;
	if (l == 0) {
		str = readline("");
	}
	else if (l == 1) {
		str = readline_fp("", car(args).value.fp);
	}
	else {
		return ERROR_ARGS;
	}
	if (str == NULL) *result = nil; else *result = make_string(str);
	return ERROR_OK;
}

error builtin_quit(atom args, atom *result) {
	if (len(args) != 0) return ERROR_ARGS;
	exit(0);
}

double rand_double() {
	return (double)rand() / ((double)RAND_MAX + 1.0);
}

error builtin_rand(atom args, atom *result) {
	long alen = len(args);
	if (alen == 0) *result = make_number(rand_double());
	else if (alen == 1) *result = make_number(floor(rand_double() * car(args).value.number));
	else return ERROR_ARGS;
	return ERROR_OK;
}

error builtin_read(atom args, atom *result) {
	long alen = len(args);
	char *s;
	if (alen == 0) {
		s = readline("");
		if (s == NULL) return ERROR_SYNTAX;
		const char *buf = s;
		error err = read_expr(buf, &buf, result);

		/* bring back remaining expressions so that "(read) (read)" works */
		if (buf) {
			if (*buf) ungetc('\n', stdin);
			const char *b0 = buf;
			for (; *buf; buf++) {
			}
			for (buf--; buf >= b0; buf--) {
				ungetc(*buf, stdin);
			}
		}
		free(s);
		return err;
	}
	else if (alen == 1) {
		s = car(args).value.str->value;
		const char *buf = s;
		error err = read_expr(buf, &buf, result);
		return err;
	}
	else return ERROR_ARGS;
	return ERROR_OK;
}

error builtin_macex(atom args, atom *result) {
	long alen = len(args);
	if (alen == 1) {
		error err = macex(car(args), result);
		return err;
	}
	else return ERROR_ARGS;
	return ERROR_OK;
}

error builtin_string(atom args, atom *result) {
	char *s = str_new();
	while (!no(args)) {
		char *a = to_string(car(args), 0);
		strcat_alloc(&s, a);
		free(a);
		args = cdr(args);
	}
	*result = make_string(s);
	return ERROR_OK;
}

error builtin_sym(atom args, atom *result) {
	long alen = len(args);
	if (alen == 1) {
		*result = make_sym(to_string(car(args), 0));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_system(atom args, atom *result) {
	long alen = len(args);
	if (alen == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;
		*result = make_number(system(car(args).value.str->value));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_eval(atom args, atom *result) {
	if (len(args) == 1) return macex_eval(car(args), result);
	else return ERROR_ARGS;
}

error builtin_load(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;
		*result = nil;
		return arc_load_file(a.value.str->value);
	}
	else return ERROR_ARGS;
}

error builtin_int(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		switch (a.type) {
		case T_STRING:
			*result = make_number(round(atof(a.value.str->value)));
			break;
		case T_SYM:
			*result = make_number(round(atof(a.value.symbol)));
			break;
		case T_NUM:
			*result = make_number(round(a.value.number));
			break;
		case T_CHAR:
			*result = make_number(a.value.ch);
			break;
		default:
			return ERROR_TYPE;
		}
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_trunc(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(trunc(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_sin(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(sin(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_cos(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(cos(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_tan(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(tan(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_bound(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_SYM) return ERROR_TYPE;
		error err = env_get(env, a.value.symbol, result);
		*result = (err ? nil : sym_t);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_infile(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;
		FILE *fp = fopen(a.value.str->value, "r");
		*result = make_input(fp);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_outfile(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;
		FILE *fp = fopen(a.value.str->value, "w");
		*result = make_output(fp);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_close(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_INPUT && a.type != T_OUTPUT) return ERROR_TYPE;
		fclose(a.value.fp);
		*result = nil;
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_readb(atom args, atom *result) {
	long l = len(args);
	FILE *fp;
	switch (l) {
	case 0:
		fp = stdin;
		break;
	case 1:
		fp = car(args).value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	*result = make_number(fgetc(fp));
	return ERROR_OK;
}

/* sread input-port eof */
error builtin_sread(atom args, atom *result) {
	if (len(args) != 2) return ERROR_ARGS;
	FILE *fp = car(args).value.fp;
	atom eof = car(cdr(args));
	error err;
	if (feof(fp)) {
		*result = eof;
		return ERROR_OK;
	}
	char *s = slurp_fp(fp);
	const char *p = s;
	err = read_expr(p, &p, result);
	return err;
}

/* write [arg [output-port]] */
error builtin_write(atom args, atom *result) {
	long l = len(args);
	FILE *fp;
	switch (l) {
	case 0:
		*result = nil;
		return ERROR_OK;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = car(cdr(args)).value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	atom a = car(args);
	if (a.type == T_STRING) fputc('"', fp);
	char *s = to_string(a, 1);
	fprintf(fp, "%s", s);
	if (a.type == T_STRING) fputc('"', fp);
	free(s);
	*result = nil;
	return ERROR_OK;
}

/* newstring length [char] */
error builtin_newstring(atom args, atom *result) {
	long arg_len = len(args);
	long length = (long) car(args).value.number;
	char c = 0;
	char *s;
	switch (arg_len) {
	case 1: break;
	case 2:
		c = car(cdr(args)).value.ch;
		break;
	default:
		return ERROR_ARGS;
	}
	s = malloc((length + 1) * sizeof(char));
	int i;
	for (i = 0; i < length; i++)
		s[i] = c;
	s[length] = 0; /* end of string */
	*result = make_string(s);
	return ERROR_OK;
}

error builtin_table(atom args, atom *result) {
	long arg_len = len(args);
	if (arg_len != 0) return ERROR_ARGS;
	*result = make_table(8);
	return ERROR_OK;
}

/* maptable proc table */
error builtin_maptable(atom args, atom *result) {
	long arg_len = len(args);
	if (arg_len != 2) return ERROR_ARGS;
	atom *proc = &car(args);
	atom *tbl = &car(cdr(args));
	if (proc->type != T_BUILTIN && proc->type != T_CLOSURE) return ERROR_TYPE;
	if (tbl->type != T_TABLE) return ERROR_TYPE;
	int i;
	for (i = 0; i < tbl->value.table->capacity; i++) {
		atom *p = &tbl->value.table->data[i];
		while (!no(*p)) {
			atom *pair = &car(*p);
			error err = apply(*proc, cons(car(*pair), cons(cdr(*pair), nil)), result);
			if (err) return err;
			p = &cdr(*p);
		}
	}
	*result = *tbl;
	return ERROR_OK;
}

/* table-sref obj value index */
error builtin_table_sref(atom args, atom *result) {
	atom index, obj, value;
	if (len(args) != 3) return ERROR_ARGS;
	index = car(cdr(cdr(args)));
	obj = car(args);
	if (obj.type != T_TABLE) return ERROR_TYPE;
	value = car(cdr(args));
	table_set(obj.value.table, index, value);	
	*result = value;
	return ERROR_OK;
}

/* coerce obj type */
/*
Coerces object to a new type.
A char can be coerced to int, num, string, or sym.
A number can be coerced to int, char, or string.
A string can be coerced to sym, cons (char list), num, or int.
A list of characters can be coerced to a string.
A symbol can be coerced to a string.
*/
error builtin_coerce(atom args, atom *result) {
	atom obj, type;
	if (len(args) != 2) return ERROR_ARGS;
	obj = car(args);
	type = car(cdr(args));
	switch (obj.type) {
	case T_CHAR:
		if (is(type, sym_int) || is(type, sym_num)) *result = make_number(obj.value.ch);
		else if (is(type, sym_string)) {
			char *buf = malloc(2);
			buf[0] = obj.value.ch;
			buf[1] = '\0';
			*result = make_string(buf);
		}
		else if (is(type, sym_sym)) {
			char buf[2];
			buf[0] = obj.value.ch;
			buf[1] = '\0';
			*result = make_sym(buf);
		}
		else if (is(type, sym_char))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	case T_NUM:
		if (is(type, sym_int)) *result = make_number(floor(obj.value.number));
		else if (is(type, sym_char)) *result = make_char((char)obj.value.number);
		else if (is(type, sym_string)) {
			*result = make_string(to_string(obj, 0));
		}
		else if (is(type, sym_num))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	case T_STRING:
		if (is(type, sym_sym)) *result = make_sym(obj.value.str->value);
		else if(is(type, sym_cons)) {
			*result = nil;
			int i;
			for (i = strlen(obj.value.str->value) - 1; i >= 0; i--) {
				*result = cons(make_char(obj.value.str->value[i]), *result);
			}
		}
		else if(is(type, sym_num)) *result = make_number(atof(obj.value.str->value));
		else if(is(type, sym_int)) *result = make_number(atoi(obj.value.str->value));
		else if (is(type, sym_string))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	case T_CONS:
		if (is(type, sym_string)) {
			char *s = str_new();
			atom p;
			char buf[2];
			buf[1] = 0;
			for (p = obj; !no(p); p = cdr(p)) {
				buf[0] = car(p).value.ch;
				strcat_alloc(&s, buf);
			}
			*result = make_string(s);
		}
		else if (is(type, sym_cons))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	case T_SYM:
		if (is(type, sym_string)) {
			*result = make_string(strdup(obj.value.symbol));
		}
		else if (is(type, sym_sym))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	default:
		*result = obj;
	}
	return ERROR_OK;
}

error builtin_flushout(atom args, atom *result) {
  if (len(args) != 0) return ERROR_ARGS;
  fflush(stdout);
  *result = sym_t;
  return ERROR_OK;
}

error builtin_err(atom args, atom *result) {
  if (len(args) == 0) return ERROR_ARGS;
  cur_expr = nil;
  atom p = args;
  for (; !no(p); p = cdr(p)) {
    char *s = to_string(car(p), 0);
    puts(s);
    free(s);
  }
  return ERROR_USER;
}

error builtin_len(atom args, atom *result) {
  if (len(args) != 1) return ERROR_ARGS;
  atom a = car(args);
  if (a.type == T_CONS) {
	*result = make_number(len(a));
  }
  else if (a.type == T_STRING) {
    *result = make_number(strlen(a.value.str->value));
  }
  else if (a.type == T_TABLE) {
    *result = make_number(a.value.table->size);
  }
  else {
    *result = make_number(0);
  }
  return ERROR_OK;
}

atom make_continuation(jmp_buf *jb) {
  atom a;
  a.type = T_CONTINUATION;
  a.value.jb = jb;
  return a;
}

error builtin_ccc(atom args, atom *result) {
  if (len(args) != 1) return ERROR_ARGS;
  atom a = car(args);
  if (a.type != T_BUILTIN && a.type != T_CLOSURE) return ERROR_TYPE;
  jmp_buf jb;
  int val = setjmp(jb);
  if (val) {
    *result = thrown;
    return ERROR_OK;
  }
  return apply(a, cons(make_continuation(&jb), nil), result);
}

/* end builtin */

char *strcat_alloc(char **dst, char *src) {
	size_t len = strlen(*dst) + strlen(src);
	*dst = realloc(*dst, (len + 1) * sizeof(char));
	strcat(*dst, src);
	return *dst;
}

char *str_new() {
	char *s = malloc(1 * sizeof(char));
	s[0] = 0;
	return s;
}

char *to_string(atom a, int write) {
	char *s = str_new();
	char buf[80];
	switch (a.type) {
	case T_NIL:
		strcat_alloc(&s, "nil");
		break;
	case T_CONS:
		strcat_alloc(&s, "(");
		strcat_alloc(&s, to_string(car(a), write));
		a = cdr(a);
		while (!no(a)) {
			if (a.type == T_CONS) {
				strcat_alloc(&s, " ");
				strcat_alloc(&s, to_string(car(a), write));
				a = cdr(a);
			}
			else {
				strcat_alloc(&s, " . ");
				strcat_alloc(&s, to_string(a, write));
				break;
			}
		}
		strcat_alloc(&s, ")");
		break;
	case T_SYM:
		strcat_alloc(&s, a.value.symbol);
		break;
	case T_STRING:
		if (write) strcat_alloc(&s, "\"");
		strcat_alloc(&s, a.value.str->value);
		if (write) strcat_alloc(&s, "\"");
		break;
	case T_NUM:
		sprintf(buf, "%.16g", a.value.number);
		strcat_alloc(&s, buf);
		break;
	case T_BUILTIN:
		sprintf(buf, "#<builtin:%p>", a.value.builtin);
		strcat_alloc(&s, buf);
		break;
	case T_CLOSURE:
	{
		atom a2 = cons(sym_fn, cdr(a));
		strcat_alloc(&s, to_string(a2, write));
		break;
	}
	case T_MACRO:
		strcat_alloc(&s, "#<macro:");
		strcat_alloc(&s, to_string(cdr(a), write));
		strcat_alloc(&s, ">");
		break;
	case T_INPUT:
		strcat_alloc(&s, "#<input>");
		break;
	case T_OUTPUT:
		strcat_alloc(&s, "#<output>");
		break;
	case T_TABLE: {
		strcat_alloc(&s, "#<table:");
		int i;
		for (i = 0; i < a.value.table->capacity; i++) {
			atom *data = &a.value.table->data[i];
			if (!no(*data)) {
				char *s2 = to_string(*data, write);
				strcat_alloc(&s, s2);
				free(s2);
			}
		}
		strcat_alloc(&s, ">");
		break;}
	case T_CHAR:
		if (write) {
			strcat_alloc(&s, "#\\");
			switch (a.value.ch) {
			case '\0': strcat_alloc(&s, "nul"); break;
			case '\r': strcat_alloc(&s, "return"); break;
			case '\n': strcat_alloc(&s, "newline"); break;
			case '\t': strcat_alloc(&s, "tab"); break;
			case ' ': strcat_alloc(&s, "space"); break;
			default:
				buf[0] = a.value.ch;
				buf[1] = '\0';
				strcat_alloc(&s, buf);
			}
		}
		else {
			s[0] = a.value.ch;
			s[1] = '\0';
		}
		break;
	case T_CONTINUATION:
		strcat_alloc(&s, "#<continuation>");
		break;
	default:
		strcat_alloc(&s, "#<unknown type>");
		break;
	}
	return s;
}

unsigned int hash_code(atom a) {
	union hash_uint {
		unsigned int v_uint;
		double v_double;
	};
	unsigned int r = 0;
	switch (a.type) {
	case T_NIL:
		return 0;
	case T_CONS:		
		while (!no(a)) {
			r *= 31;
			if (a.type == T_CONS) {
				r += hash_code(car(a));
				a = cdr(a);
			}
			else {
				r += hash_code(a);
				break;
			}
		}
		return r;
	case T_SYM:
	  return (unsigned int)a.value.symbol / sizeof(a.value.symbol);
	case T_STRING: {
		char *v = a.value.str->value;
		for (; *v != 0; v++) {
			r *= 31;
			r += *v;
		}
		return r; }
	case T_NUM: {
		union hash_uint h;
		h.v_double = a.value.number;
		return h.v_uint; }
	case T_BUILTIN:
	  return (unsigned int)a.value.builtin / sizeof(a.value.builtin);
	case T_CLOSURE:
		return hash_code(cdr(a));
	case T_MACRO:
		return hash_code(cdr(a));
	case T_INPUT:
	case T_OUTPUT:
		return (unsigned int)a.value.fp / sizeof(a.value.fp);
	default:
		return 0;
	}
}

atom make_table(int capacity) {
	atom a;
	struct table *s;
	alloc_count++;
	consider_gc();
	s = a.value.table = malloc(sizeof(struct table));
	s->capacity = capacity;
	s->size = 0;
	s->data = malloc(capacity * sizeof(atom));
	int i;
	for (i = 0; i < capacity; i++) {
		s->data[i] = nil;
	}
	s->mark = 0;
	s->next = table_head;
	table_head = s;
	a.value.table = s;
	a.type = T_TABLE;
	stack_add(a);
	return a;
}

/* return 1 if found */
int table_set(struct table *tbl, atom k, atom v) {
	struct pair *p = table_get(tbl, k);
	if (p) {
		p->cdr = v;
		return 1;
	}
	else {
		table_add(tbl, k, v);
		return 0;
	}
}

/* return 1 if found */
int table_set_sym(struct table *tbl, char *k, atom v) {
	struct pair *p = table_get_sym(tbl, k);
	if (p) {
		p->cdr = v;
		return 1;
	}
	else {
		atom k2;
		k2.type = T_SYM;
		k2.value.symbol = k;
		table_add(tbl, k2, v);
		return 0;
	}
}

void table_add(struct table *tbl, atom k, atom v) {
	/* insert new item */
	atom *p = &tbl->data[hash_code(k) % tbl->capacity];
	*p = cons(cons(k, v), *p);
	tbl->size++;
	if (tbl->size > 0.75 * tbl->capacity) { /* rehash, load factor = 0.75 */
		int new_capacity = tbl->capacity * 2;
		struct atom *data2 = malloc(new_capacity * sizeof(struct atom));
		int i;
		for (i = 0; i < new_capacity; i++) {
			data2[i] = nil;
		}
		for (i = 0; i < tbl->capacity; i++) {
			atom *p = &tbl->data[i];
			while (!no(*p)) {
				atom *pair1 = &car(*p);
				atom *p2 = &data2[hash_code(car(*pair1)) % new_capacity];
				*p2 = cons(*pair1, *p2);
				p = &cdr(*p);
			}
		}
		free(tbl->data);
		tbl->data = data2;
		tbl->capacity = new_capacity;
	}
}

/* return pair. return NULL if not found */
struct pair *table_get(struct table *tbl, atom k) {
	int pos = hash_code(k) % tbl->capacity;
	atom *p = &tbl->data[pos];
	while (!no(*p)) {
		struct pair *pair = car(*p).value.pair;
		if (is(pair->car, k)) {
			return pair;
		}
		p = &cdr(*p);
	}
	return NULL;
}

/* return pair. return NULL if not found */
struct pair *table_get_sym(struct table *tbl, char *k) {
	int pos = ((unsigned int)k / sizeof(char *)) % tbl->capacity;
	atom *p = &tbl->data[pos];
	while (!no(*p)) {
		struct pair *pair = car(*p).value.pair;
		if (pair->car.value.symbol == k) {
			return pair;
		}
		p = &cdr(*p);
	}
	return NULL;
}

char *slurp_fp(FILE *fp) {
	char *buf;
	long len;

	fseek(fp, 0, SEEK_END);
	len = ftell(fp);
	if (len < 0) return NULL;
	fseek(fp, 0, SEEK_SET);

	buf = (char *)malloc(len + 1);
	if (!buf)
		return NULL;

	fread(buf, 1, len, fp);
	buf[len] = 0;
	fclose(fp);

	return buf;
}

char *slurp(const char *path)
{
	FILE *fp = fopen(path, "rb");
	if (!fp) {
		/* printf("Reading %s failed.\n", path); */
		return NULL;
	}	
	return slurp_fp(fp);
}

/* compile-time macro */
error macex(atom expr, atom *result) {
	error err = ERROR_OK;

	cur_expr = expr; /* for error reporting */

	if (expr.type == T_SYM) {
		*result = expr;
		return ERROR_OK;
	}
	else if (expr.type != T_CONS) {
		*result = expr;
		return ERROR_OK;
	}
	else if (!listp(expr)) {
		*result = expr;
		return ERROR_OK;
	}
	else {
		int ss = stack_size; /* save stack point */
		stack_add(expr);
		stack_add(env);
		atom op = car(expr);
		atom args = cdr(expr);

		if (op.type == T_SYM) {
			/* Handle special forms */

			if (op.value.symbol == sym_quote.value.symbol) {
				if (no(args) || !no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				*result = expr;
				stack_restore(ss);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				atom name, macro;

				if (no(args) || no(cdr(args)) || no(cdr(cdr(args)))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				name = car(args);
				if (name.type != T_SYM) {
					stack_restore(ss);
					return ERROR_TYPE;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = T_MACRO;
					*result = cons(sym_quote, cons(car(args), nil));
					err = env_assign(env, name.value.symbol, macro);
					stack_restore(ss);
					return err;
				}
				else {
					stack_restore(ss);
					return err;
				}
			}
		}

		/* Is it a macro? */
		if (op.type == T_SYM && !env_get(env, op.value.symbol, result) && result->type == T_MACRO) {
			/* Evaluate operator */
			err = eval_expr(op, env, &op);
			if (err) {
				stack_restore(ss);
				return err;
			}

			op.type = T_CLOSURE;
			atom result2;
			err = apply(op, args, &result2);
			if (err) {
				stack_restore(ss);
				return err;
			}
			stack_add(result2);
			err = macex(result2, result); /* recursive */
			if (err) {
				stack_restore(ss);
				return err;
			}
			stack_restore(ss);
			return ERROR_OK;
		}
		else {
			/* preprocess elements */
			atom expr2 = copy_list(expr);
			atom p = expr2;
			while (!no(p)) {
				err = macex(car(p), &car(p));
				if (err) {
					stack_restore(ss);
					return err;
				}
				p = cdr(p);
			}
			*result = expr2;
			stack_restore(ss);
			return ERROR_OK;
		}
	}
}

error macex_eval(atom expr, atom *result) {
	atom expr2;
	error err = macex(expr, &expr2);
	if (err) return err;
	/*printf("expanded: ");
	print_expr(expr2);
	puts("");*/
	return eval_expr(expr2, env, result);
}

error arc_load_file(const char *path)
{
	char *text;
	error err = ERROR_OK;
	/* printf("Reading %s...\n", path); */
	text = slurp(path);
	if (text) {
		const char *p = text;
		atom expr;
		while (1) {
			int ss = stack_size;
			if (read_expr(p, &p, &expr) != ERROR_OK) {
				stack_restore(ss);
				break;
			}
			atom result;
			err = macex_eval(expr, &result);
			if (err) {
				print_error(err);
				printf("error in expression:\n\t");
				print_expr(expr);
				putchar('\n');
				break;
			}
			stack_restore(ss);
			/*else {
			print_expr(result);
			putchar(' ');
			}*/
		}
		/*puts("");*/
		free(text);
		return err;
	}
	else {
		return ERROR_FILE;
	}
}

error eval_expr(atom expr, atom env, atom *result)
{
	error err = ERROR_OK;

	cur_expr = expr; /* for error reporting */
	if (expr.type == T_SYM) {
		err = env_get(env, expr.value.symbol, result);
		return err;
	}
	else if (expr.type != T_CONS) {
		*result = expr;
		return ERROR_OK;
	}
	else if (!listp(expr)) {
		return ERROR_SYNTAX;
	}
	else {
		int ss = stack_size; /* save stack point */
		stack_add(expr);
		stack_add(env);
		atom op = car(expr);
		atom args = cdr(expr);

		if (op.type == T_SYM) {
			/* Handle special forms */
			if (op.value.symbol == sym_if.value.symbol) {
				atom cond;
				atom *p = &args;
				while (!no(*p)) {
					err = eval_expr(car(*p), env, &cond);
					if (err) {
						stack_restore(ss);
						return err;
					}
					if (no(cdr(*p))) {
						*result = cond;
						stack_restore(ss);
						return ERROR_OK;
					}
					if (!no(cond)) {
						err = eval_expr(car(cdr(*p)), env, result);
						stack_restore(ss);
						return err;
					}
					p = &cdr(cdr(*p));
				}
				*result = nil;
				stack_restore(ss);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_assign.value.symbol) {
				atom sym;
				if (no(args) || no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				sym = car(args);
				if (sym.type == T_SYM) {
					atom val;
					err = eval_expr(car(cdr(args)), env, &val);
					if (err) {
						stack_restore(ss);
						return err;
					}

					*result = val;
					err = env_assign_eq(env, sym.value.symbol, val);
					stack_restore(ss);
					return err;
				}
				else {
					stack_restore(ss);
					return ERROR_TYPE;
				}
			}
			else if (op.value.symbol == sym_quote.value.symbol) {
				if (no(args) || !no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				*result = car(args);
				stack_restore(ss);
				return ERROR_OK;
			}			
			else if (op.value.symbol == sym_fn.value.symbol) {
				if (no(args)) {
					stack_restore(ss);
					return ERROR_ARGS;
				}
				err = make_closure(env, car(args), cdr(args), result);
				stack_restore(ss);
				return err;
			}			
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				atom name, macro;

				if (no(args) || no(cdr(args)) || no(cdr(cdr(args)))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				name = car(args);
				if (name.type != T_SYM) {
					stack_restore(ss);
					return ERROR_TYPE;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = T_MACRO;
					*result = name;
					err = env_assign(env, name.value.symbol, macro);
					stack_restore(ss);
					return err;
				}
				else {
					stack_restore(ss);
					return err;
				}
			}
			else if (op.value.symbol == sym_while.value.symbol) {
				atom pred;
				if (no(args)) {
					stack_restore(ss);
					return ERROR_ARGS;
				}
				pred = car(args);
				int ss2 = stack_size;
				while (err = eval_expr(pred, env, result), !no(*result)) {
					if (err) {
						stack_restore(ss);
						return err;
					}
					atom e = cdr(args);
					while (!no(e)) {
						err = eval_expr(car(e), env, result);
						if (err) {
							stack_restore(ss);
							return err;
						}
						e = cdr(e);
					}
					stack_restore(ss2);
				}
				stack_restore(ss);
				return ERROR_OK;
			}
		}

		/* Evaluate operator */
		err = eval_expr(op, env, &op);
		if (err) {
			stack_restore(ss);
			return err;
		}

		/* Is it a macro? */
		if (op.type == T_MACRO) {
			atom expansion;
			op.type = T_CLOSURE;
			err = apply(op, args, &expansion);
			stack_add(expansion);
			if (err) {
				stack_restore(ss);
				return err;
			}
			err = eval_expr(expansion, env, result);
			stack_restore(ss);
			return err;
		}

		/* Evaulate arguments */
		args = copy_list(args);
		atom *p = &args;
		while (!no(*p)) {
			err = eval_expr(car(*p), env, &car(*p));
			if (err) {
				stack_restore(ss);
				return err;
			}

			p = &cdr(*p);
		}
		err = apply(op, args, result);
		stack_restore(ss);
		return err;
	}
}

void arc_init(char *file_path) {
	srand((unsigned int)time(0));
	env = env_create(nil);

	symbol_capacity = 500;
	symbol_table = malloc(symbol_capacity * sizeof(char *));

	/* Set up the initial environment */
	sym_t = make_sym("t");
	sym_quote = make_sym("quote");
	sym_assign = make_sym("assign");
	sym_fn = make_sym("fn");
	sym_if = make_sym("if");
	sym_mac = make_sym("mac");
	sym_apply = make_sym("apply");
	sym_while = make_sym("while");
	sym_cons = make_sym("cons");
	sym_sym = make_sym("sym");
	sym_string = make_sym("string");
	sym_num = make_sym("num");
	sym__ = make_sym("_");
	sym_o = make_sym("o");
	sym_table = make_sym("table");
	sym_int = make_sym("int");
	sym_char = make_sym("char");

	env_assign(env, sym_t.value.symbol, sym_t);
	env_assign(env, make_sym("nil").value.symbol, nil);
	env_assign(env, make_sym("car").value.symbol, make_builtin(builtin_car));
	env_assign(env, make_sym("cdr").value.symbol, make_builtin(builtin_cdr));
	env_assign(env, make_sym("cons").value.symbol, make_builtin(builtin_cons));
	env_assign(env, make_sym("+").value.symbol, make_builtin(builtin_add));
	env_assign(env, make_sym("-").value.symbol, make_builtin(builtin_subtract));
	env_assign(env, make_sym("*").value.symbol, make_builtin(builtin_multiply));
	env_assign(env, make_sym("/").value.symbol, make_builtin(builtin_divide));
	env_assign(env, make_sym("<").value.symbol, make_builtin(builtin_less));
	env_assign(env, make_sym(">").value.symbol, make_builtin(builtin_greater));
	env_assign(env, make_sym("apply").value.symbol, make_builtin(builtin_apply));
	env_assign(env, make_sym("is").value.symbol, make_builtin(builtin_is));
	env_assign(env, make_sym("scar").value.symbol, make_builtin(builtin_scar));
	env_assign(env, make_sym("scdr").value.symbol, make_builtin(builtin_scdr));
	env_assign(env, make_sym("mod").value.symbol, make_builtin(builtin_mod));
	env_assign(env, make_sym("type").value.symbol, make_builtin(builtin_type));
	env_assign(env, make_sym("string-sref").value.symbol, make_builtin(builtin_string_sref));
	env_assign(env, make_sym("writeb").value.symbol, make_builtin(builtin_writeb));
	env_assign(env, make_sym("expt").value.symbol, make_builtin(builtin_expt));
	env_assign(env, make_sym("log").value.symbol, make_builtin(builtin_log));
	env_assign(env, make_sym("sqrt").value.symbol, make_builtin(builtin_sqrt));
	env_assign(env, make_sym("readline").value.symbol, make_builtin(builtin_readline));
	env_assign(env, make_sym("quit").value.symbol, make_builtin(builtin_quit));
	env_assign(env, make_sym("rand").value.symbol, make_builtin(builtin_rand));
	env_assign(env, make_sym("read").value.symbol, make_builtin(builtin_read));
	env_assign(env, make_sym("macex").value.symbol, make_builtin(builtin_macex));
	env_assign(env, make_sym("string").value.symbol, make_builtin(builtin_string));
	env_assign(env, make_sym("sym").value.symbol, make_builtin(builtin_sym));
	env_assign(env, make_sym("system").value.symbol, make_builtin(builtin_system));
	env_assign(env, make_sym("eval").value.symbol, make_builtin(builtin_eval));
	env_assign(env, make_sym("load").value.symbol, make_builtin(builtin_load));
	env_assign(env, make_sym("int").value.symbol, make_builtin(builtin_int));
	env_assign(env, make_sym("trunc").value.symbol, make_builtin(builtin_trunc));
	env_assign(env, make_sym("sin").value.symbol, make_builtin(builtin_sin));
	env_assign(env, make_sym("cos").value.symbol, make_builtin(builtin_cos));
	env_assign(env, make_sym("tan").value.symbol, make_builtin(builtin_tan));
	env_assign(env, make_sym("bound").value.symbol, make_builtin(builtin_bound));
	env_assign(env, make_sym("infile").value.symbol, make_builtin(builtin_infile));
	env_assign(env, make_sym("outfile").value.symbol, make_builtin(builtin_outfile));
	env_assign(env, make_sym("close").value.symbol, make_builtin(builtin_close));
	env_assign(env, make_sym("stdin").value.symbol, make_input(stdin));
	env_assign(env, make_sym("stdout").value.symbol, make_output(stdout));
	env_assign(env, make_sym("stderr").value.symbol, make_output(stderr));
	env_assign(env, make_sym("disp").value.symbol, make_builtin(builtin_disp));
	env_assign(env, make_sym("readb").value.symbol, make_builtin(builtin_readb));
	env_assign(env, make_sym("sread").value.symbol, make_builtin(builtin_sread));
	env_assign(env, make_sym("write").value.symbol, make_builtin(builtin_write));
	env_assign(env, make_sym("newstring").value.symbol, make_builtin(builtin_newstring));
	env_assign(env, make_sym("table").value.symbol, make_builtin(builtin_table));
	env_assign(env, make_sym("maptable").value.symbol, make_builtin(builtin_maptable));
	env_assign(env, make_sym("table-sref").value.symbol, make_builtin(builtin_table_sref));
	env_assign(env, make_sym("coerce").value.symbol, make_builtin(builtin_coerce));
	env_assign(env, make_sym("flushout").value.symbol, make_builtin(builtin_flushout));
	env_assign(env, make_sym("err").value.symbol, make_builtin(builtin_err));
	env_assign(env, make_sym("len").value.symbol, make_builtin(builtin_len));
	env_assign(env, make_sym("ccc").value.symbol, make_builtin(builtin_ccc));

	char *dir_path = get_dir_path(file_path);
	char *lib = malloc((strlen(dir_path) + 1) * sizeof(char));
	strcpy(lib, dir_path);
	strcat_alloc(&lib, "library.arc");
	if (arc_load_file(lib) != ERROR_OK) {
		strcpy(lib, dir_path);
		strcat_alloc(&lib, "../library.arc");
		arc_load_file(lib);
	}
	free(lib);
	free(dir_path);
}

char *get_dir_path(char *file_path) {
	size_t len = strlen(file_path);
	long i = len - 1;
	for (; i >= 0; i--) {
		char c = file_path[i];
		if (c == '\\' || c == '/') {
			break;
		}
	}
	size_t len2 = i + 1;
	char *r = malloc((len2 + 1) * sizeof(char));
	memcpy(r, file_path, len2);
	r[len2] = 0;
	return r;
}

void print_error(error e) {
  if (e != ERROR_USER) {
	printf("%s : ", error_string[e]);
	print_expr(cur_expr);
	puts("");
  }
}
