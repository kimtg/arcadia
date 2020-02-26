#include "arc.h"
#include <ctype.h>

char *error_string[] = { "", "Syntax error", "Symbol not bound", "Wrong number of arguments", "Wrong type", "File error", "" };
size_t stack_capacity = 0;
size_t stack_size = 0;
atom *stack = NULL;
struct pair *pair_head = NULL;
struct str *str_head = NULL;
struct table *table_head = NULL;
size_t alloc_count = 0;
size_t alloc_count_old = 0;
char **symbol_table = NULL;
size_t symbol_size = 0;
size_t symbol_capacity = 0;
const atom nil = { T_NIL };
atom env; /* the global environment */
/* symbols for faster execution */
atom sym_t, sym_quote, sym_quasiquote, sym_unquote, sym_unquote_splicing, sym_assign, sym_fn, sym_if, sym_mac, sym_apply, sym_cons, sym_sym, sym_string, sym_num, sym__, sym_o, sym_table, sym_int, sym_char, sym_do;
atom cur_expr;
atom thrown;

/* Be sure to free after use */
void vector_new(struct vector *a) {
	a->capacity = sizeof(a->static_data) / sizeof(a->static_data[0]);
	a->size = 0;
	a->data = a->static_data;
}

void vector_add(struct vector *a, atom item) {
	if (a->size + 1 > a->capacity) {
		a->capacity *= 2;
		if (a->data == a->static_data) {
			a->data = malloc(a->capacity * sizeof(atom));
			memcpy(a->data, a->static_data, a->size * sizeof(atom));
		}
		else {
			a->data = realloc(a->data, a->capacity * sizeof(atom));
		}		
	}
	a->data[a->size] = item;
	a->size++;
}

void vector_clear(struct vector *a) {
	a->size = 0;
}

void vector_free(struct vector *a) {
	if (a->data != a->static_data) free(a->data);
}

atom vector_to_atom(struct vector *a, int start) {
	atom r = nil;
	int i;
	for (i = a->size - 1; i >= start; i--) {
		r = cons(a->data[i], r);
	}
	return r;
}

/* Be sure to free after use */
void atom_to_vector(atom a, struct vector *v) {
	vector_new(v);
	for (; !no(a); a = cdr(a)) {
		vector_add(v, car(a));
	}
}

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
	consider_gc();
}

void stack_restore(int saved_size) {
	stack_size = saved_size;
	/* if there is waste of memory, realloc */
	if (stack_size < stack_capacity / 4) {
		stack_capacity = stack_size * 2;
		stack = realloc(stack, stack_capacity * sizeof(atom));
	}
}

void stack_restore_add(int saved_size, atom a) {
	stack_restore(saved_size);
	stack_add(a);
}

void consider_gc() {
	if (alloc_count > 2 * alloc_count_old)
		gc();
}

atom cons(atom car_val, atom cdr_val)
{
	struct pair *a;
	atom p;

	alloc_count++;

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
		size_t i;
		for (i = 0; i < at->capacity; i++) {
			struct table_entry *e = at->data[i];
			while (e) {
				gc_mark(e->k);
				gc_mark(e->v);
				e = e->next;
			}
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
	size_t i;
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
			size_t i;
			for (i = 0; i < at->capacity; i++) {
				struct table_entry *e = at->data[i];
				while (e) {
					struct table_entry *next = e->next;
					free(e);
					e = next;
				}
			}
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
	for (i = symbol_size - 1; i >= 0; i--) { /* compare recent symbol first */
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
		else if (p.type != T_CONS || (car(p).type != T_SYM && car(p).type != T_CONS))
			return ERROR_TYPE;
		p = cdr(p);
	}

	if (no(cdr(body))) { /* 1 form only: do form not required */
		p = body;
	}
	else {
		p = cons(cons(sym_do, body), nil);
	}
	*result = cons(env, cons(args, p));
	result->type = T_CLOSURE;

	return ERROR_OK;
}

atom make_string(char *x)
{
	atom a;
	struct str *s;
	alloc_count++;
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
		return ERROR_FILE;
	}

	*start = str;

	if (strchr(prefix, str[0]) != NULL)
		*end = str + 1;
	else if (str[0] == ',')
		*end = str + (str[1] == '@' ? 2 : 1);
	else if (str[0] == '"') {
		str++;
		while (*str != 0) {
			if (*str == '\\') str++;
			else if (*str == '"') {
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
	char *p;

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
		char *buf = malloc(end - start + 1);
		memcpy(buf, start, end - start);
		buf[end - start] = 0;
		size_t length = end - start;
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
	char *buf = malloc(end - start + 1);
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
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				free(buf);
				*result = cons(a1, cons(a2, nil));
				return ERROR_OK;
			}
			else if (buf[i] == '!') { /* a!b => (a 'b) */
				if (i == 0 || i == length - 1) {
					free(buf);
					return ERROR_SYNTAX;
				}
				error err;
				err = parse_simple(buf, buf + i, &a1);
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				free(buf);
				*result = cons(a1, cons(cons(sym_quote, cons(a2, nil)), nil));
				return ERROR_OK;
			}
			else if (buf[i] == ':') { /* a:b => (compose a b) */
				if (i == 0 || i == length - 1) {
					free(buf);
					return ERROR_SYNTAX;
				}
				error err;
				err = parse_simple(buf, buf + i, &a1);
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				err = parse_simple(buf + i + 1, buf + length, &a2);
				if (err) {
					free(buf);
					return ERROR_SYNTAX;
				}
				free(buf);
				*result = cons(make_sym("compose"), cons(a1, cons(a2, nil)));
				return ERROR_OK;
			}
		}
		if (length >= 2 && buf[0] == '~') { /* ~a => (complement a) */
			atom a1;
			error err = parse_simple(buf + 1, buf + length, &a1);
			free(buf);
			if (err) {
				return ERROR_SYNTAX;
			}
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
			return ERROR_OK;
		}

		err = read_expr(token, end, &item);
		if (err) return err;

		if (no(body)) {
			body = cons(item, nil);
			cdr(p) = cons(body, nil);
			p = body;
		}
		else {
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
		return read_list(*end, end, result);
	}
	else if (token[0] == ')')
		return ERROR_SYNTAX;
	else if (token[0] == '[') {
		return read_bracket(*end, end, result);
	}
	else if (token[0] == ']')
		return ERROR_SYNTAX;
	else if (token[0] == '\'') {
		*result = cons(sym_quote, cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else if (token[0] == '`') {
		*result = cons(sym_quasiquote, cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else if (token[0] == ',') {
		*result = cons(
			token[1] == '@' ? sym_unquote_splicing : sym_unquote,
			cons(nil, nil));
		return read_expr(*end, end, &car(cdr(*result)));
	}
	else
		return parse_simple(token, *end, result);
}

#ifndef READLINE
char *readline(char *prompt) {
	return readline_fp(prompt, stdin);
}
#endif /* READLINE */

char *readline_fp(char *prompt, FILE *fp) {
	size_t size = 80;
	/* The size is extended by the input with the value of the provisional */
	char *str;
	int ch;
	size_t len = 0;
	printf("%s", prompt);
	str = malloc(sizeof(char)* size); /* size is start size */
	if (!str) return NULL;
	while (EOF != (ch = fgetc(fp)) && ch != '\n') {
		str[len++] = ch;
		if (len == size) {
			void *p = realloc(str, sizeof(char)*(size *= 2));
			if (!p) {
				free(str);
				return NULL;
			}
			str = p;
		}
	}
	if (ch == EOF && len == 0) {
		free(str);
		return NULL;
	}
	str[len++] = '\0';

	return realloc(str, sizeof(char)*len);
}

atom env_create(atom parent, size_t capacity)
{
	return cons(parent, make_table(capacity));
}

error env_get(atom env, char *symbol, atom *result)
{
	while (1) {
		struct table *ptbl = cdr(env).value.table;
		struct table_entry *a = table_get_sym(ptbl, symbol);
		if (a) {
			*result = a->v;
			return ERROR_OK;
		}
		if (no(car(env))) {
			/* printf("%s: ", symbol); */
			return ERROR_UNBOUND;
		}
		env = car(env);
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
		struct table_entry *a = table_get_sym(ptbl, symbol);
		if (a) {
			a->v = value;
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

size_t len(atom xs) {
	atom *p = &xs;
	size_t ret = 0;
	while (!no(*p)) {
		if (p->type != T_CONS)
			return ret + 1;
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

error destructuring_bind(atom arg_name, atom val, int val_unspecified, atom env) {
	if (no(arg_name)) {
		if (no(val))
			return ERROR_OK;
		else {
			return ERROR_ARGS;
		}
	}
	else if (arg_name.type == T_SYM) {		
		return env_assign(env, arg_name.value.symbol, val);		
	}
	else if (arg_name.type == T_CONS) {
		if (is(car(arg_name), sym_o)) { /* (o ARG [DEFAULT]) */
			if (val_unspecified) { /* missing argument */
				if (!no(cdr(cdr(arg_name)))) {
					error err = eval_expr(car(cdr(cdr(arg_name))), env, &val);
					if (err) {
						return err;
					}
				}
			}
			return env_assign(env, car(cdr(arg_name)).value.symbol, val);
		}
		else {
			if (val.type != T_CONS) {
				return ERROR_ARGS;
			}
			error err = destructuring_bind(car(arg_name), car(val), 0, env);
			if (err) {
				return err;
			}
			return destructuring_bind(cdr(arg_name), cdr(val), no(cdr(val)), env);
		}
	}
	else {
		return ERROR_ARGS;
	}
}

error env_bind(atom env, atom arg_names, struct vector *vargs) {
	/* Bind the arguments */
	size_t i = 0;
	while (!no(arg_names)) {
		if (arg_names.type == T_SYM) {
			env_assign(env, arg_names.value.symbol, vector_to_atom(vargs, i));
			i = vargs->size;
			break;
		}
		atom arg_name = car(arg_names);
		atom val;
		int val_unspecified = 0;
		if (i < vargs->size) {
			val = vargs->data[i];
		}
		else {
			val = nil;
			val_unspecified = 1;
		}
		error err = destructuring_bind(arg_name, val, val_unspecified, env);
		if (err) {
			return err;
		}
		arg_names = cdr(arg_names);
		i++;
	}
	if (i < vargs->size) {
		return ERROR_ARGS;
	}
	return ERROR_OK;
}

error apply(atom fn, struct vector *vargs, atom *result)
{
	if (fn.type == T_BUILTIN)
		return (*fn.value.builtin)(vargs, result);
	else if (fn.type == T_CLOSURE) {		
		atom arg_names = car(cdr(fn));
		atom env = env_create(car(fn), len(arg_names));
		atom body = cdr(cdr(fn));

		error err = env_bind(env, arg_names, vargs);
		if (err) {
			return err;
		}

		/* Evaluate the body */
		err = eval_expr(car(body), env, result);
		if (err) {
			return err;
		}
		return ERROR_OK;
	}
	else if (fn.type == T_CONTINUATION) {
		if (vargs->size != 1) return ERROR_ARGS;
		thrown = vargs->data[0];
		longjmp(*fn.value.jb, 1);
	}
	else if (fn.type == T_STRING) { /* implicit indexing for string */
		if (vargs->size != 1) return ERROR_ARGS;
		long index = (long)(vargs->data[0]).value.number;
		*result = make_char(fn.value.str->value[index]);
		return ERROR_OK;
	}
	else if (fn.type == T_CONS && listp(fn)) { /* implicit indexing for list */
		if (vargs->size != 1) return ERROR_ARGS;
		long index = (long)(vargs->data[0]).value.number;
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
		long len1 = vargs->size;
		if (len1 != 1 && len1 != 2) return ERROR_ARGS;
		struct table_entry *pair = table_get(fn.value.table, vargs->data[0]);
		if (pair) {
			*result = pair->v;
		}
		else {
			if (len1 == 2) /* default value is specified */
				*result = vargs->data[1];
			else
				*result = nil;
		}
		return ERROR_OK;
	}
	else {
		return ERROR_TYPE;
	}
}

error builtin_car(struct vector *vargs, atom *result)
{
	if (vargs->size != 1)
		return ERROR_ARGS;

	atom a = vargs->data[0];
	if (no(a))
		*result = nil;
	else if (a.type != T_CONS)
		return ERROR_TYPE;
	else
		*result = car(a);

	return ERROR_OK;
}

error builtin_cdr(struct vector *vargs, atom *result)
{
	if (vargs->size != 1)
		return ERROR_ARGS;

	atom a = vargs->data[0];
	if (no(a))
		*result = nil;
	else if (a.type != T_CONS)
		return ERROR_TYPE;
	else
		*result = cdr(a);

	return ERROR_OK;
}

error builtin_cons(struct vector *vargs, atom *result)
{
	if (vargs->size != 2)
		return ERROR_ARGS;

	*result = cons(vargs->data[0], vargs->data[1]);

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
error builtin_add(struct vector *vargs, atom *result)
{
	if (vargs->size == 0) {
		*result = make_number(0);
	}
	else {
		if (vargs->data[0].type == T_NUM) {
			double r = vargs->data[0].value.number;
			size_t i;
			for (i = 1; i < vargs->size; i++) {
				if (vargs->data[i].type != T_NUM) return ERROR_TYPE;
				r += vargs->data[i].value.number;
			}
			*result = make_number(r);
		}
		else if (vargs->data[0].type == T_STRING) {
			char *buf = str_new();
			size_t i;
			for (i = 0; i < vargs->size; i++) {
				char *s = to_string(vargs->data[i], 0);
				strcat_alloc(&buf, s);
				free(s);
			}
			*result = make_string(buf);
		}
		else if (vargs->data[0].type == T_CONS || vargs->data[0].type == T_NIL) {
			atom acc = nil;
			size_t i;
			for (i = 0; i < vargs->size; i++) {
				acc = append(acc, vargs->data[i]);
			}
			*result = acc;
		}
	}
	return ERROR_OK;
}

error builtin_subtract(struct vector *vargs, atom *result)
{
	if (vargs->size == 0) { /* 0 argument */
		*result = make_number(0);
		return ERROR_OK;
	}
	if (vargs->data[0].type != T_NUM) return ERROR_TYPE;
	if (vargs->size == 1) { /* 1 argument */
		*result = make_number(-vargs->data[0].value.number);
		return ERROR_OK;
	}
	double r = vargs->data[0].value.number;
	size_t i;
	for (i = 1; i < vargs->size; i++) {
		if (vargs->data[i].type != T_NUM) return ERROR_TYPE;
		r -= vargs->data[i].value.number;
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_multiply(struct vector *vargs, atom *result)
{
	double r = 1;
	size_t i;
	for (i = 0; i < vargs->size; i++) {
		if (vargs->data[i].type != T_NUM) return ERROR_TYPE;
		r *= vargs->data[i].value.number;
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_divide(struct vector *vargs, atom *result)
{
	if (vargs->size == 0) { /* 0 argument */
		*result = make_number(1);
		return ERROR_OK;
	}
	if (vargs->data[0].type != T_NUM) return ERROR_TYPE;
	if (vargs->size == 1) { /* 1 argument */
		*result = make_number(1.0 / vargs->data[0].value.number);
		return ERROR_OK;
	}
	double r = vargs->data[0].value.number;
	size_t i;
	for (i = 1; i < vargs->size; i++) {
		if (vargs->data[i].type != T_NUM) return ERROR_TYPE;
		r /= vargs->data[i].value.number;
	}
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_less(struct vector *vargs, atom *result)
{
	if (vargs->size <= 1) {
		*result = sym_t;
		return ERROR_OK;
	}
	size_t i;
	switch (vargs->data[0].type) {
	case T_NUM:
		for (i = 0; i < vargs->size - 1; i++) {
			if (vargs->data[i].value.number >= vargs->data[i + 1].value.number) {
				*result = nil;
				return ERROR_OK;
			}
		}
		*result = sym_t;
		return ERROR_OK;
	case T_STRING:
		for (i = 0; i < vargs->size - 1; i++) {
			if (strcmp(vargs->data[i].value.str->value, vargs->data[i + 1].value.str->value) >= 0) {
				*result = nil;
				return ERROR_OK;
			}
		}
		*result = sym_t;
		return ERROR_OK;
	default:
		return ERROR_TYPE;
	}
}

error builtin_greater(struct vector *vargs, atom *result)
{
	if (vargs->size <= 1) {
		*result = sym_t;
		return ERROR_OK;
	}
	size_t i;
	switch (vargs->data[0].type) {
	case T_NUM:
		for (i = 0; i < vargs->size - 1; i++) {
			if (vargs->data[i].value.number <= vargs->data[i + 1].value.number) {
				*result = nil;
				return ERROR_OK;
			}
		}
		*result = sym_t;
		return ERROR_OK;
	case T_STRING:
		for (i = 0; i < vargs->size - 1; i++) {
			if (strcmp(vargs->data[i].value.str->value, vargs->data[i + 1].value.str->value) <= 0) {
				*result = nil;
				return ERROR_OK;
			}
		}
		*result = sym_t;
		return ERROR_OK;
	default:
		return ERROR_TYPE;
	}
}

error builtin_apply(struct vector *vargs, atom *result)
{
	atom fn;

	if (vargs->size != 2)
		return ERROR_ARGS;

	fn = vargs->data[0];
	struct vector v;
	atom_to_vector(vargs->data[1], &v);
	error err = apply(fn, &v, result);
	vector_free(&v);
	return err;
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

int iso(atom a, atom b) {
	if (a.type == b.type) {
		switch (a.type) {
		case T_CONS:
		case T_CLOSURE:
		case T_MACRO:
			return iso(a.value.pair->car, b.value.pair->car) && iso(a.value.pair->cdr, b.value.pair->cdr);
		default:
			return is(a, b);
		}
	}
	return 0;
}

error builtin_is(struct vector *vargs, atom *result)
{
	atom a, b;
	if (vargs->size <= 1) {
		*result = sym_t;
		return ERROR_OK;
	}
	size_t i;
	for (i = 0; i < vargs->size - 1; i++) {
		a = vargs->data[i];
		b = vargs->data[i + 1];
		if (!is(a, b)) {
			*result = nil;
			return ERROR_OK;
		}
	}
	*result = sym_t;
	return ERROR_OK;
}

error builtin_scar(struct vector *vargs, atom *result) {
	if (vargs->size != 2) return ERROR_ARGS;
	atom place = vargs->data[0], value;
	if (place.type != T_CONS) return ERROR_TYPE;
	value = vargs->data[1];
	place.value.pair->car = value;
	*result = value;
	return ERROR_OK;
}

error builtin_scdr(struct vector *vargs, atom *result) {
	if (vargs->size != 2) return ERROR_ARGS;
	atom place = vargs->data[0], value;
	if (place.type != T_CONS) return ERROR_TYPE;
	value = vargs->data[1];
	place.value.pair->cdr = value;
	*result = value;
	return ERROR_OK;
}

error builtin_mod(struct vector *vargs, atom *result) {
	if (vargs->size != 2) return ERROR_ARGS;
	atom dividend = vargs->data[0];
	atom divisor = vargs->data[1];
	double r = fmod(dividend.value.number, divisor.value.number);
	if (dividend.value.number * divisor.value.number < 0 && r != 0) r += divisor.value.number;
	*result = make_number(r);
	return ERROR_OK;
}

error builtin_type(struct vector *vargs, atom *result) {
	if (vargs->size != 1) return ERROR_ARGS;
	atom x = vargs->data[0];
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

/* sref obj value index
     obj: cons, string, table
 */
error builtin_sref(struct vector *vargs, atom *result) {
	atom index, obj, value;
	size_t i;
	if (vargs->size != 3) return ERROR_ARGS;
	obj = vargs->data[0];
	value = vargs->data[1];
	index = vargs->data[2];
	switch (obj.type) {
	case T_CONS:
	  for (i=0; i<(size_t)index.value.number; i++) {
	    obj = cdr(obj);
	  }
	  car(obj) = value;
	  *result = value;
	  return ERROR_OK;
	case T_STRING:
	  obj.value.str->value[(long)index.value.number] = (char)value.value.ch;
	  *result = value;
	  return ERROR_OK;
	case T_TABLE:
	  table_set(obj.value.table, index, value);
	  *result = value;
	  return ERROR_OK;
	default:
	  return ERROR_TYPE;
	}
}

/* disp [arg [output-port]] */
error builtin_disp(struct vector *vargs, atom *result) {
	long l = vargs->size;
	FILE *fp;
	switch (l) {
	case 0:
		*result = nil;
		return ERROR_OK;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = vargs->data[1].value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	char *s = to_string(vargs->data[0], 0);
	fprintf(fp, "%s", s);
	free(s);
	*result = nil;
	return ERROR_OK;
}

error builtin_writeb(struct vector *vargs, atom *result) {
	long l = vargs->size;
	FILE *fp;
	switch (l) {
	case 0: return ERROR_ARGS;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = vargs->data[1].value.fp;
		break;
	default: return ERROR_ARGS;
	}
	fputc((int)vargs->data[0].value.number, fp);
	*result = nil;
	return ERROR_OK;
}

error builtin_expt(struct vector *vargs, atom *result) {
	atom a, b;
	if (vargs->size != 2) return ERROR_ARGS;
	a = vargs->data[0];
	b = vargs->data[1];
	*result = make_number(pow(a.value.number, b.value.number));
	return ERROR_OK;
}

error builtin_log(struct vector *vargs, atom *result) {
	atom a;
	if (vargs->size != 1) return ERROR_ARGS;
	a = vargs->data[0];
	*result = make_number(log(a.value.number));
	return ERROR_OK;
}

error builtin_sqrt(struct vector *vargs, atom *result) {
	atom a;
	if (vargs->size != 1) return ERROR_ARGS;
	a = vargs->data[0];
	*result = make_number(sqrt(a.value.number));
	return ERROR_OK;
}

error builtin_readline(struct vector *vargs, atom *result) {
	long l = vargs->size;
	char *str;
	if (l == 0) {
		str = readline("");
	}
	else if (l == 1) {
		str = readline_fp("", vargs->data[0].value.fp);
	}
	else {
		return ERROR_ARGS;
	}
	if (str == NULL) *result = nil; else *result = make_string(str);
	return ERROR_OK;
}

error builtin_quit(struct vector *vargs, atom *result) {
	if (vargs->size != 0) return ERROR_ARGS;
	exit(0);
}

double rand_double() {
	return (double)rand() / ((double)RAND_MAX + 1.0);
}

error builtin_rand(struct vector *vargs, atom *result) {
	long alen = vargs->size;
	if (alen == 0) *result = make_number(rand_double());
	else if (alen == 1) *result = make_number(floor(rand_double() * vargs->data[0].value.number));
	else return ERROR_ARGS;
	return ERROR_OK;
}

error read_fp(FILE *fp, atom *result) {
	char *s = readline_fp("", fp);
	if (s == NULL) return ERROR_FILE;
	const char *buf = s;
	error err = read_expr(buf, &buf, result);

	/* bring back remaining expressions so that "(read) (read)" works */
	if (buf) {
		if (*buf) ungetc('\n', fp);
		const char *b0 = buf;
		for (; *buf; buf++) {
		}
		for (buf--; buf >= b0; buf--) {
			ungetc(*buf, fp);
		}
	}
	free(s);
	return err;
}

/* read [input-source [eof]]
Reads a S-expression from the input-source, which can be either a string or an input-port. If the end of file is reached, nil is returned or the specified eof value. */
error builtin_read(struct vector *vargs, atom *result) {
	size_t alen = vargs->size;
	error err;
	if (alen == 0) {
		err = read_fp(stdin, result);
	}
	else if (alen <= 2) {
		atom src = vargs->data[0];
		if (src.type == T_STRING) {
			char *s = vargs->data[0].value.str->value;
			const char *buf = s;
			err = read_expr(buf, &buf, result);
		}
		else if (src.type == T_INPUT) {
			err = read_fp(src.value.fp, result);
		}
		else {
			return ERROR_TYPE;
		}
	}
	else {
		return ERROR_ARGS;
	}

	if (err == ERROR_FILE) {
		atom eof = nil; /* default value when EOF */
		if (alen == 2) { /* specified return value when EOF */
			eof = vargs->data[1];
		}

		*result = eof;
		return ERROR_OK;
	}
	else {
		return err;
	}
}

error builtin_macex(struct vector *vargs, atom *result) {
	long alen = vargs->size;
	if (alen == 1) {
		error err = macex(vargs->data[0], result);
		return err;
	}
	else return ERROR_ARGS;
	return ERROR_OK;
}

/* 
 * From Arc tutorial:
 * Every argument will appear as it would look if printed out by pr,
 * except nil, which is ignored.
 */
error builtin_string(struct vector *vargs, atom *result) {
	char *s = str_new();
	size_t i;
	for (i = 0; i < vargs->size; i++) {
		if (!no(vargs->data[i])) {
			char *a = to_string(vargs->data[i], 0);
			strcat_alloc(&s, a);
			free(a);
		}
	}
	*result = make_string(s);
	return ERROR_OK;
}

error builtin_sym(struct vector *vargs, atom *result) {
	long alen = vargs->size;
	if (alen == 1) {
		*result = make_sym(to_string(vargs->data[0], 0));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_system(struct vector *vargs, atom *result) {
	long alen = vargs->size;
	if (alen == 1) {
		atom a = vargs->data[0];
		if (a.type != T_STRING) return ERROR_TYPE;
		*result = make_number(system(vargs->data[0].value.str->value));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_eval(struct vector *vargs, atom *result) {
	if (vargs->size == 1) return macex_eval(vargs->data[0], result);
	else return ERROR_ARGS;
}

error builtin_load(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_STRING) return ERROR_TYPE;
		*result = nil;
		return arc_load_file(a.value.str->value);
	}
	else return ERROR_ARGS;
}

error builtin_int(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
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

error builtin_trunc(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(trunc(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_sin(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(sin(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_cos(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(cos(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_tan(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_NUM) return ERROR_TYPE;
		*result = make_number(tan(a.value.number));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_bound(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_SYM) return ERROR_TYPE;
		error err = env_get(env, a.value.symbol, result);
		*result = (err ? nil : sym_t);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_infile(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_STRING) return ERROR_TYPE;
		FILE *fp = fopen(a.value.str->value, "r");
		*result = make_input(fp);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_outfile(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_STRING) return ERROR_TYPE;
		FILE *fp = fopen(a.value.str->value, "w");
		*result = make_output(fp);
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_close(struct vector *vargs, atom *result) {
	if (vargs->size == 1) {
		atom a = vargs->data[0];
		if (a.type != T_INPUT && a.type != T_OUTPUT) return ERROR_TYPE;
		fclose(a.value.fp);
		*result = nil;
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_readb(struct vector *vargs, atom *result) {
	long l = vargs->size;
	FILE *fp;
	switch (l) {
	case 0:
		fp = stdin;
		break;
	case 1:
		fp = vargs->data[0].value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	*result = make_number(fgetc(fp));
	return ERROR_OK;
}

/* sread input-port eof */
error builtin_sread(struct vector *vargs, atom *result) {
	if (vargs->size != 2) return ERROR_ARGS;
	FILE *fp = vargs->data[0].value.fp;
	atom eof = vargs->data[1];
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
error builtin_write(struct vector *vargs, atom *result) {
	long l = vargs->size;
	FILE *fp;
	switch (l) {
	case 0:
		*result = nil;
		return ERROR_OK;
	case 1:
		fp = stdout;
		break;
	case 2:
		fp = vargs->data[1].value.fp;
		break;
	default:
		return ERROR_ARGS;
	}
	atom a = vargs->data[0];
	if (a.type == T_STRING) fputc('"', fp);
	char *s = to_string(a, 1);
	fprintf(fp, "%s", s);
	if (a.type == T_STRING) fputc('"', fp);
	free(s);
	*result = nil;
	return ERROR_OK;
}

/* newstring length [char] */
error builtin_newstring(struct vector *vargs, atom *result) {
	long arg_len = vargs->size;
	long length = (long)vargs->data[0].value.number;
	char c = 0;
	char *s;
	switch (arg_len) {
	case 1: break;
	case 2:
		c = vargs->data[1].value.ch;
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

error builtin_table(struct vector *vargs, atom *result) {
	long arg_len = vargs->size;
	if (arg_len != 0) return ERROR_ARGS;
	*result = make_table(8);
	return ERROR_OK;
}

/* maptable proc table */
error builtin_maptable(struct vector *vargs, atom *result) {
	long arg_len = vargs->size;
	if (arg_len != 2) return ERROR_ARGS;
	atom proc = vargs->data[0];
	atom tbl = vargs->data[1];
	if (tbl.type != T_TABLE) return ERROR_TYPE;
	size_t i;
	for (i = 0; i < tbl.value.table->capacity; i++) {
		struct table_entry *p = tbl.value.table->data[i];
		while (p) {
			vector_clear(vargs);
			vector_add(vargs, p->k);
			vector_add(vargs, p->v);
			error err = apply(proc, vargs, result);
			if (err) return err;
			p = p->next;
		}
	}
	*result = tbl;
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
error builtin_coerce(struct vector *vargs, atom *result) {
	atom obj, type;
	if (vargs->size != 2) return ERROR_ARGS;
	obj = vargs->data[0];
	type = vargs->data[1];
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
		else if (is(type, sym_cons)) {
			*result = nil;
			int i;
			for (i = strlen(obj.value.str->value) - 1; i >= 0; i--) {
				*result = cons(make_char(obj.value.str->value[i]), *result);
			}
		}
		else if (is(type, sym_num)) *result = make_number(atof(obj.value.str->value));
		else if (is(type, sym_int)) *result = make_number(atoi(obj.value.str->value));
		else if (is(type, sym_string))
			*result = obj;
		else
			return ERROR_TYPE;
		break;
	case T_CONS:
		if (is(type, sym_string)) {
			char *s = str_new();
			atom p;
			for (p = obj; !no(p); p = cdr(p)) {
				atom x;
				struct vector v; /* (car(p) string) */
				vector_new(&v);
				vector_add(&v, car(p));
				vector_add(&v, sym_string);
				error err = builtin_coerce(&v, &x);
				vector_free(&v);
				if (err) return err;
				strcat_alloc(&s, x.value.str->value);
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

error builtin_flushout(struct vector *vargs, atom *result) {
	if (vargs->size != 0) return ERROR_ARGS;
	fflush(stdout);
	*result = sym_t;
	return ERROR_OK;
}

error builtin_err(struct vector *vargs, atom *result) {
	if (vargs->size == 0) return ERROR_ARGS;
	cur_expr = nil;
	size_t i;
	for (i = 0; i < vargs->size; i++) {
		char *s = to_string(vargs->data[i], 0);
		puts(s);
		free(s);
	}
	return ERROR_USER;
}

error builtin_len(struct vector *vargs, atom *result) {
	if (vargs->size != 1) return ERROR_ARGS;
	atom a = vargs->data[0];
	if (a.type == T_STRING) {
		*result = make_number(strlen(a.value.str->value));
	}
	else if (a.type == T_TABLE) {
		*result = make_number(a.value.table->size);
	}
	else {
		*result = make_number(len(a));
	}
	return ERROR_OK;
}

atom make_continuation(jmp_buf *jb) {
	atom a;
	a.type = T_CONTINUATION;
	a.value.jb = jb;
	return a;
}

error builtin_ccc(struct vector *vargs, atom *result) {
	if (vargs->size != 1) return ERROR_ARGS;
	atom a = vargs->data[0];
	if (a.type != T_BUILTIN && a.type != T_CLOSURE) return ERROR_TYPE;
	jmp_buf jb;
	int val = setjmp(jb);
	if (val) {
		*result = thrown;
		return ERROR_OK;
	}
	vector_clear(vargs);
	vector_add(vargs, make_continuation(&jb));
	return apply(a, vargs, result);
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
		if (listp(a) && len(a) == 2 && is(car(a), sym_quote)) {
			strcat_alloc(&s, "'");
			strcat_alloc(&s, to_string(car(cdr(a)), write));
		}
		else if (listp(a) && len(a) == 2 && is(car(a), sym_quasiquote)) {
			strcat_alloc(&s, "`");
			strcat_alloc(&s, to_string(car(cdr(a)), write));
		}
		else if (listp(a) && len(a) == 2 && is(car(a), sym_unquote)) {
			strcat_alloc(&s, ",");
			strcat_alloc(&s, to_string(car(cdr(a)), write));
		}
		else if (listp(a) && len(a) == 2 && is(car(a), sym_unquote_splicing)) {
			strcat_alloc(&s, ",@");
			strcat_alloc(&s, to_string(car(cdr(a)), write));
		}
		else {
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
		}
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
		size_t i;
		for (i = 0; i < a.value.table->capacity; i++) {
			struct table_entry *p = a.value.table->data[i];
			while (p) {
				char *s2 = to_string(p->k, write);
				strcat_alloc(&s, " ");
				strcat_alloc(&s, s2);
				free(s2);
				strcat_alloc(&s, ":");
				s2 = to_string(p->v, write);
				strcat_alloc(&s, s2);
				free(s2);
				p = p->next;
			}
		}
		strcat_alloc(&s, ">");
		break; }
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

size_t hash_code_sym(char *s) {
	return (size_t)s / sizeof(s) / 2;
}

size_t hash_code(atom a) {
	size_t r = 1;
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
		return hash_code_sym(a.value.symbol);
	case T_STRING: {
		char *v = a.value.str->value;
		for (; *v != 0; v++) {
			r *= 31;
			r += *v;
		}
		return r; }
	case T_NUM:
		return (size_t)a.value.number;
	case T_BUILTIN:
		return (size_t)a.value.builtin;
	case T_CLOSURE:
		return hash_code(cdr(a));
	case T_MACRO:
		return hash_code(cdr(a));
	case T_INPUT:
	case T_OUTPUT:
		return (size_t)a.value.fp / sizeof(*a.value.fp);
	default:
		return 0;
	}
}

atom make_table(size_t capacity) {
	atom a;
	struct table *s;
	alloc_count++;
	s = a.value.table = malloc(sizeof(struct table));
	s->capacity = capacity;
	s->size = 0;
	s->data = malloc(capacity * sizeof(struct table_entry *));
	size_t i;
	for (i = 0; i < capacity; i++) {
		s->data[i] = NULL;
	}
	s->mark = 0;
	s->next = table_head;
	table_head = s;
	a.value.table = s;
	a.type = T_TABLE;
	stack_add(a);
	return a;
}

struct table_entry *table_entry_new(atom k, atom v, struct table_entry *next) {
	struct table_entry *r = malloc(sizeof(*r));
	r->k = k;
	r->v = v;
	r->next = next;
	return r;
}


/* return 1 if found */
int table_set(struct table *tbl, atom k, atom v) {
	struct table_entry *p = table_get(tbl, k);
	if (p) {
		p->v = v;
		return 1;
	}
	else {
		table_add(tbl, k, v);
		return 0;
	}
}

/* return 1 if found. k is symbol. */
int table_set_sym(struct table *tbl, char *k, atom v) {
	struct table_entry *p = table_get_sym(tbl, k);
	if (p) {
		p->v = v;
		return 1;
	}
	else {
		atom s = { T_SYM,.value.symbol = k };
		table_add(tbl, s, v);
		return 0;
	}
}

void table_add(struct table *tbl, atom k, atom v) {
	if (tbl->size + 1 > tbl->capacity) { /* rehash, load factor = 1 */
		size_t new_capacity = (tbl->size + 1) * 2;
		struct table_entry **data2 = malloc(new_capacity * sizeof(struct table_entry *));
		size_t i;
		for (i = 0; i < new_capacity; i++) {
			data2[i] = NULL;
		}
		for (i = 0; i < tbl->capacity; i++) {
			struct table_entry *p = tbl->data[i];
			while (p) {
				struct table_entry **p2 = &data2[hash_code(p->k) % new_capacity];
				struct table_entry *next = p->next;
				*p2 = table_entry_new(p->k, p->v, *p2);
				free(p);
				p = next;
			}
		}
		free(tbl->data);
		tbl->data = data2;
		tbl->capacity = new_capacity;
	}
	/* insert new item */
	struct table_entry **p = &tbl->data[hash_code(k) % tbl->capacity];
	*p = table_entry_new(k, v, *p);
	tbl->size++;
}

/* return entry. return NULL if not found */
struct table_entry *table_get(struct table *tbl, atom k) {
	if (tbl->size == 0) return NULL;
	int pos = hash_code(k) % tbl->capacity;
	struct table_entry *p = tbl->data[pos];
	while (p) {
		if (iso(p->k, k)) {
			return p;
		}
		p = p->next;
	}
	return NULL;
}

/* return entry. return NULL if not found */
struct table_entry *table_get_sym(struct table *tbl, char *k) {
	if (tbl->size == 0) return NULL;
	int pos = hash_code_sym(k) % tbl->capacity;
	struct table_entry *p = tbl->data[pos];
	while (p) {
		if (p->k.value.symbol == k) {
			return p;
		}
		p = p->next;
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

	return buf;
}

char *slurp(const char *path)
{
	FILE *fp = fopen(path, "rb");
	if (!fp) {
		/* printf("Reading %s failed.\n", path); */
		return NULL;
	}
	char *r = slurp_fp(fp);
	fclose(fp);
	return r;
}

/* compile-time macro */
error macex(atom expr, atom *result) {
	error err = ERROR_OK;

	cur_expr = expr; /* for error reporting */

	if (expr.type != T_CONS || !listp(expr)) {
		*result = expr;
		return ERROR_OK;
	}
	else {
		int ss = stack_size; /* save stack point */
		atom op = car(expr);

		/* Handle quote */
		if (op.type == T_SYM && op.value.symbol == sym_quote.value.symbol) {
			*result = expr;
			return ERROR_OK;
		}

		atom args = cdr(expr);

		/* Is it a macro? */
		if (op.type == T_SYM && !env_get(env, op.value.symbol, result) && result->type == T_MACRO) {
			/* Evaluate operator */
			op = *result;

			op.type = T_CLOSURE;

			atom result2;
			struct vector vargs;
			atom_to_vector(args, &vargs);
			err = apply(op, &vargs, &result2);
			if (err) {
				vector_free(&vargs);
				stack_restore(ss);
				return err;
			}
			err = macex(result2, result); /* recursive */
			if (err) {
				vector_free(&vargs);
				stack_restore(ss);
				return err;
			}
			vector_free(&vargs);
			stack_restore_add(ss, *result);
			return ERROR_OK;
		}
		else {
			/* macex elements */
			atom expr2 = copy_list(expr);
			atom h;
			for (h = expr2; !no(h); h = cdr(h)) {
				err = macex(car(h), &car(h));
				if (err) {
					stack_restore(ss);
					return err;
				}
			}
			*result = expr2;
			stack_restore_add(ss, *result);
			return ERROR_OK;
		}
	}
}

error macex_eval(atom expr, atom *result) {
	atom expr2;
	error err = macex(expr, &expr2);
	if (err) return err;
	/*	printf("macex_eval: ");
		print_expr(expr);
		puts("");
		printf("expanded: ");
		print_expr(expr2);
		puts("\n");
	*/
	return eval_expr(expr2, env, result);
}

error load_string(const char *text) {
	error err = ERROR_OK;
	const char *p = text;
	atom expr;
	while (*p) {
		err = read_expr(p, &p, &expr);
		if (err) {
			break;
		}
		atom result;
		err = macex_eval(expr, &result);
		if (err) {
			break;
		}
		/*else {
			print_expr(result);
			putchar(' ');
		}*/
		while (*p && isspace(*p)) p++;
	}
	/*puts("");*/
	return err;
}

error arc_load_file(const char *path)
{
	char *text;
	error err = ERROR_OK;
	/* printf("Reading %s...\n", path); */
	text = slurp(path);
	if (text) {
		err = load_string(text);
		free(text);
		return err;
	}
	else {
		return ERROR_FILE;
	}
}

error eval_expr(atom expr, atom env, atom *result)
{
	error err;
	int ss = stack_size; /* save stack point */
start_eval:
	cur_expr = expr; /* for error reporting */
	if (expr.type == T_SYM) {
		err = env_get(env, expr.value.symbol, result);
		return err;
	}
	else if (expr.type != T_CONS) {
		*result = expr;
		return ERROR_OK;
	}
	else {
		atom op = car(expr);
		atom args = cdr(expr);

		if (op.type == T_SYM) {
			/* Handle special forms */
			if (op.value.symbol == sym_if.value.symbol) {
				atom *p = &args;
				while (!no(*p)) {
					atom cond;
					if (no(cdr(*p))) { /* else */
						/* tail call optimization of else part */
						expr = car(*p);
						goto start_eval;
					}
					err = eval_expr(car(*p), env, &cond);
					if (err) {
						stack_restore(ss);
						return err;
					}
					if (!no(cond)) { /* then */
						/* tail call optimization of err = eval_expr(car(cdr(*p)), env, result); */
						expr = car(cdr(*p));
						goto start_eval;
					}
					p = &cdr(cdr(*p));
				}
				*result = nil;
				stack_restore_add(ss, *result);
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
					stack_restore_add(ss, *result);
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
				stack_restore_add(ss, *result);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_fn.value.symbol) {
				if (no(args)) {
					stack_restore(ss);
					return ERROR_ARGS;
				}
				err = make_closure(env, car(args), cdr(args), result);
				stack_restore_add(ss, *result);
				return err;
			}
			else if (op.value.symbol == sym_do.value.symbol) {
				/* Evaluate the body */
				*result = nil;
				while (!no(args)) {
					if (no(cdr(args))) {
						/* tail call */
						expr = car(args);
						stack_restore(ss);
						goto start_eval;
					}
					error err = eval_expr(car(args), env, result);
					if (err) {
						return err;
					}
					args = cdr(args);
				}
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
					*result = name;
					err = env_assign(env, name.value.symbol, macro);
					stack_restore_add(ss, *result);
					return err;
				}
				else {
					stack_restore(ss);
					return err;
				}
			}
		}

		/* Evaluate operator */
		atom fn;
		err = eval_expr(op, env, &fn);
		if (err) {
			stack_restore(ss);
			return err;
		}

		/* Evaulate arguments */
		struct vector vargs;
		vector_new(&vargs);
		atom *p = &args;
		while (!no(*p)) {
			atom r;
			err = eval_expr(car(*p), env, &r);
			if (err) {
				vector_free(&vargs);
				stack_restore(ss);
				return err;
			}
			vector_add(&vargs, r);
			p = &cdr(*p);
		}

		/* tail call optimization of err = apply(fn, args, result); */
		if (fn.type == T_CLOSURE) {			
			atom arg_names = car(cdr(fn));
			env = env_create(car(fn), len(arg_names));
			expr = car(cdr(cdr(fn)));

			/* Bind the arguments */
			err = env_bind(env, arg_names, &vargs);
			if (err) {
				return err;
			}
			vector_free(&vargs);
			goto start_eval;
		}
		else {
			err = apply(fn, &vargs, result);
			vector_free(&vargs);
		}
		stack_restore_add(ss, *result);
		return err;
	}
}

void arc_init(char *file_path) {
#ifdef READLINE
	rl_bind_key('\t', rl_insert); /* prevent tab completion */
#endif
	srand((unsigned int)time(0));
	env = env_create(nil, 500);

	symbol_capacity = 500;
	symbol_table = malloc(symbol_capacity * sizeof(char *));

	/* Set up the initial environment */
	sym_t = make_sym("t");
	sym_quote = make_sym("quote");
	sym_quasiquote = make_sym("quasiquote");
	sym_unquote = make_sym("unquote");
	sym_unquote_splicing = make_sym("unquote-splicing");
	sym_assign = make_sym("assign");
	sym_fn = make_sym("fn");
	sym_if = make_sym("if");
	sym_mac = make_sym("mac");
	sym_apply = make_sym("apply");
	sym_cons = make_sym("cons");
	sym_sym = make_sym("sym");
	sym_string = make_sym("string");
	sym_num = make_sym("num");
	sym__ = make_sym("_");
	sym_o = make_sym("o");
	sym_table = make_sym("table");
	sym_int = make_sym("int");
	sym_char = make_sym("char");
	sym_do = make_sym("do");

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
	env_assign(env, make_sym("sref").value.symbol, make_builtin(builtin_sref));
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
	env_assign(env, make_sym("coerce").value.symbol, make_builtin(builtin_coerce));
	env_assign(env, make_sym("flushout").value.symbol, make_builtin(builtin_flushout));
	env_assign(env, make_sym("err").value.symbol, make_builtin(builtin_err));
	env_assign(env, make_sym("len").value.symbol, make_builtin(builtin_len));
	env_assign(env, make_sym("ccc").value.symbol, make_builtin(builtin_ccc));

	const char *stdlib =
		#include "library.h"
		;
	error err = load_string(stdlib);
	if (err) {
		print_error(err);
	}
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
