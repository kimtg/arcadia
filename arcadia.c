#define VERSION "0.5.2"

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <time.h>

#ifdef _MSC_VER
#define strdup _strdup
#endif

enum type {
	T_NIL,
	T_CONS,
	T_SYMBOL,
	T_NUM,
	T_BUILTIN,
	T_CLOSURE,
	T_MACRO,
	T_STRING
};

typedef enum {
	ERROR_OK = 0, ERROR_SYNTAX, ERROR_UNBOUND, ERROR_ARGS, ERROR_TYPE, ERROR_FILE
} error;

char *error_string[] = { "", "Syntax error", "Symbol not bound", "Wrong number of arguments", "Wrong type", "File error" };

typedef struct atom atom;
typedef error(*builtin)(atom args, atom *result);

struct atom {
	enum type type;

	union {
		double number;
		struct pair *pair;
		char *symbol;
		builtin builtin;
	} value;
};

static atom sym_table = { T_NIL };

struct pair {
	struct atom car, cdr;
};

/* forward declarations */
error apply(atom fn, atom args, atom *result);
int listp(atom expr);
char *slurp(const char *path);
error eval_expr(atom expr, atom env, atom *result);
void gc_mark(atom root);
void gc();
void stack_restore(int saved_size);
error macex(atom expr, atom *result);
char *to_string(atom atom);
char *strcat_alloc(char **dst, char *src);
char *str_new();
error macex_eval(atom expr, atom env, atom *result);
error load_file(atom env, const char *path);
/* end forward */

#define car(p) ((p).value.pair->car)
#define cdr(p) ((p).value.pair->cdr)
#define no(atom) ((atom).type == T_NIL)

static const atom nil = { T_NIL };
/* symbols for faster execution */
static atom sym_t, sym_quote, sym_assign, sym_fn, sym_if, sym_mac, sym_apply, sym_while, sym_cons, sym_sym, sym_fn, sym_string, sym_num;
atom code_expr;
atom env; /* the global environment */

struct allocation {	
	struct pair pair;
	char mark;
	struct allocation *next;
};

struct allocation_string {
	char *string;
	char mark;
	struct allocation_string *next;
};

struct allocation *global_allocations = NULL;
struct allocation_string *global_allocations_string = NULL;
int cons_count = 0;

atom *stack = NULL;
int stack_capacity = 0;
int stack_size = 0;

void stack_add(atom a) {
	if (!(a.type == T_CONS
		|| a.type == T_CLOSURE
		|| a.type == T_MACRO))
		return;
	stack_size++;
	if (stack_size > stack_capacity) {
		stack_capacity = stack_size * 2;
		stack = realloc(stack, stack_capacity * sizeof(atom));
	}
	stack[stack_size - 1] = a;
}

atom cons(atom car_val, atom cdr_val)
{
	struct allocation *a;
	atom p;

	cons_count++;

	a = malloc(sizeof(struct allocation));
	if (a == NULL) {
		puts("Not enough memory.");
		exit(1);
	}
	a->mark = 0;
	a->next = global_allocations;
	global_allocations = a;

	p.type = T_CONS;
	p.value.pair = &a->pair;

	car(p) = car_val;
	cdr(p) = cdr_val;

	stack_add(p);
	
	return p;
}

void gc_mark(atom root)
{
	struct allocation *a;
	struct allocation_string *as;

	switch (root.type) {
	case T_CONS:
	case T_CLOSURE:
	case T_MACRO:
		a = (struct allocation *)
			((char *)root.value.pair
			- offsetof(struct allocation, pair));

		if (a->mark)
			return;

		a->mark = 1;

		gc_mark(car(root));
		gc_mark(cdr(root));
		break;
	case T_STRING:
		as = (struct allocation_string *)
			((char *)root.value.symbol
			- offsetof(struct allocation_string, string));

		if (as->mark)
			return;

		as->mark = 1;
		break;
	default:
		return;
	}
}

void gc()
{
	struct allocation *a, **p;
	struct allocation_string *as, **ps;

	gc_mark(sym_table);
	gc_mark(code_expr);	

	/* mark atoms in the stack */
	int i;
	for (i = 0; i < stack_size; i++) {
		gc_mark(stack[i]);
	}
	
	/* Free unmarked allocations */
	p = &global_allocations;
	while (*p != NULL) {
		a = *p;
		if (!a->mark) {
			*p = a->next;
			free(a);
		}
		else {
			p = &a->next;
			a->mark = 0; /* clear mark */
		}
	}

	/* Free unmarked string allocations */
	ps = &global_allocations_string;
	while (*ps != NULL) {
		as = *ps;
		if (!as->mark) {
			*ps = as->next;
			free(as->string);
			free(as);
		}
		else {
			ps = &as->next;
			as->mark = 0; /* clear mark */
		}
	}
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
	atom a, p;

	p = sym_table;
	while (!no(p)) {
		a = car(p);
		if (strcmp(a.value.symbol, s) == 0)
			return a;
		p = cdr(p);
	}

	a.type = T_SYMBOL;
	a.value.symbol = (char*)strdup(s);
	sym_table = cons(a, sym_table);
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

	/* Check argument names are all symbols */
	p = args;
	while (!no(p)) {
		if (p.type == T_SYMBOL)
			break;
		else if (p.type != T_CONS
			|| car(p).type != T_SYMBOL)
			return ERROR_TYPE;
		p = cdr(p);
	}

	*result = cons(env, cons(args, body));
	result->type = T_CLOSURE;

	return ERROR_OK;
}

atom make_string(char *x)
{
	atom a;
	struct allocation_string *alloc;
	cons_count++;
	alloc = malloc(sizeof(struct allocation_string));
	alloc->string = x;
	alloc->mark = 0;
	alloc->next = global_allocations_string;
	global_allocations_string = alloc;

	a.type = T_STRING;
	a.value.symbol = x;
	stack_add(a);
	return a;
}

void print_expr(atom atom)
{
	switch (atom.type) {
	case T_NIL:
		printf("nil");
		break;
	case T_CONS:
		putchar('(');
		print_expr(car(atom));
		atom = cdr(atom);
		while (!no(atom)) {
			if (atom.type == T_CONS) {
				putchar(' ');
				print_expr(car(atom));
				atom = cdr(atom);
			}
			else {
				printf(" . ");
				print_expr(atom);
				break;
			}
		}
		putchar(')');
		break;
	case T_SYMBOL:
		printf("%s", atom.value.symbol);
		break;
	case T_NUM:
		printf("%.16g", atom.value.number);
		break;
	case T_BUILTIN:
		printf("#<builtin:%p>", atom.value.builtin);
		break;
	case T_CLOSURE:
		printf("(closure ");
		print_expr(cdr(atom));
		putchar(')');
		break;
	case T_STRING:
		printf("\"%s\"", atom.value.symbol);
		break;
	case T_MACRO:
		printf("(macro ");
		print_expr(cdr(atom));
		putchar(')');
		break;
	default:
		printf("(unknown type)");
		break;
	}
}

void pr(atom atom)
{
	switch (atom.type) {
	case T_NIL:
		printf("nil");
		break;
	case T_CONS:
		putchar('(');
		print_expr(car(atom));
		atom = cdr(atom);
		while (!no(atom)) {
			if (atom.type == T_CONS) {
				putchar(' ');
				print_expr(car(atom));
				atom = cdr(atom);
			}
			else {
				printf(" . ");
				print_expr(atom);
				break;
			}
		}
		putchar(')');
		break;
	case T_SYMBOL:
		printf("%s", atom.value.symbol);
		break;
	case T_NUM:
		printf("%.16g", atom.value.number);
		break;
	case T_BUILTIN:
		printf("#<builtin:%p>", atom.value.builtin);
		break;
	case T_CLOSURE:
		printf("(closure ");
		print_expr(cdr(atom));
		putchar(')');
		break;
	case T_STRING:
		printf("%s", atom.value.symbol);
		break;
	case T_MACRO:
		printf("(macro ");
		print_expr(cdr(atom));
		putchar(')');
		break;
	default:
		printf("(unknown type)");
		break;
	}
}

error lex(const char *str, const char **start, const char **end)
{
	const char *ws = " \t\r\n";
	const char *delim = "() \t\r\n;";
	const char *prefix = "()'`";
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
		str++;
		while (*str != '"' && *str != 0) {
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

error read_expr(const char *input, const char **end, atom *result);

error parse_simple(const char *start, const char *end, atom *result)
{
	char *buf, *p;

	/* Is it an integer? */
	double val = strtod(start, &p);
	if (p == end) {
		result->type = T_NUM;
		result->value.number = val;
		return ERROR_OK;
	}
	else if (start[0] == '"') {
		result->type = T_STRING;
		result->value.symbol = (char*)malloc(end - start - 1);
		memcpy(result->value.symbol, start + 1, end - start);
		result->value.symbol[end - start - 2] = 0;
		return ERROR_OK;
	}

	/* NIL or symbol */
	buf = malloc(end - start + 1);
	memcpy(buf, start, end - start);
	buf[end - start] = 0;

	if (strcmp(buf, "nil") == 0)
		*result = nil;
	else
		*result = make_sym(buf);

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

		if (token[0] == ')')
			return ERROR_OK;

		if (token[0] == '.' && *end - token == 1) {
			/* Improper list */
			if (no(p))
				return ERROR_SYNTAX;

			err = read_expr(*end, end, &item);
			if (err)
				return err;

			cdr(p) = item;

			/* Read the closing ')' */
			err = lex(*end, &token, end);
			if (!err && token[0] != ')')
				err = ERROR_SYNTAX;

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

error read_expr(const char *input, const char **end, atom *result)
{
	const char *token;
	error err;

	err = lex(input, &token, end);
	if (err)
		return err;

	if (token[0] == '(')
		return read_list(*end, end, result);
	else if (token[0] == ')')
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

char *readline(char *prompt) {
  size_t size = 80;
  /* The size is extended by the input with the value of the provisional */
  char *str;
  int ch;
  size_t len = 0;
  printf(prompt);
  str = malloc(sizeof(char) * size); /* size is start size */
  if (!str) return NULL;
  while (EOF != (ch = fgetc(stdin)) && ch != '\n') {
	str[len++] = ch;
	if(len == size){
	  str = realloc(str, sizeof(char)*(size*=2));
	  if (!str) return NULL;
	}
  }
  if (ch == EOF && len == 0) return NULL;
  str[len++]='\0';

  return realloc(str, sizeof(char)*len);
}

atom env_create(atom parent)
{
	return cons(parent, nil);
}

error env_get(atom env, atom symbol, atom *result)
{
	atom parent = car(env);
	atom bs = cdr(env);

	while (!no(bs)) {
		atom b = car(bs);
		if (car(b).value.symbol == symbol.value.symbol) {
			*result = cdr(b);
			return ERROR_OK;
		}
		bs = cdr(bs);
	}

	if (no(parent)) {
		/*printf("%s: ", symbol.value.symbol);*/
		return ERROR_UNBOUND;
	}

	return env_get(parent, symbol, result);
}

error env_assign(atom env, atom symbol, atom value)
{
	atom bs = cdr(env);
	atom b = nil;

	while (!no(bs)) {
		b = car(bs);
		if (car(b).value.symbol == symbol.value.symbol) {
			cdr(b) = value;
			return ERROR_OK;
		}
		bs = cdr(bs);
	}

	b = cons(symbol, value);
	cdr(env) = cons(b, cdr(env));

	return ERROR_OK;
}

error env_assign_eq(atom env, atom symbol, atom value) {
	atom env_origin = env;
	while (!no(env)) {
		atom bs = cdr(env);

		while (!no(bs)) {
			atom b = car(bs);
			if (car(b).value.symbol == symbol.value.symbol) {
			  cdr(b) = value;
			  return ERROR_OK;
			}
			bs = cdr(bs);
		}
		env = car(env);
	}

	return env_assign(env_origin, symbol, value);
}

int listp(atom expr)
{
	while (!no(expr)) {
		if (expr.type != T_CONS)
			return 0;
		expr = cdr(expr);
	}
	return 1;
}

long len(atom xs) {
  atom a = xs;
  long ret = 0;
  if (!listp(xs)) return 0;
  while (!no(a)) {
	ret++;
	a = cdr(a);
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
			if (arg_names.type == T_SYMBOL) {
				env_assign(env, arg_names, args);
				args = nil;
				break;
			}

			if (no(args))
				return ERROR_ARGS;
			env_assign(env, car(arg_names), car(args));
			arg_names = cdr(arg_names);
			args = cdr(args);
		}
		if (!no(args))
			return ERROR_ARGS;

		/* Evaluate the body */
		while (!no(body)) {
			error err = eval_expr(car(body), env, result);
			if (err)
				return err;
			body = cdr(body);
		}

		return ERROR_OK;
	}
	else if (fn.type == T_STRING) { /* implicit indexing for string */
		if (len(args) != 1) return ERROR_ARGS;
		long index = (long)(car(args)).value.number;
		*result = make_number(fn.value.symbol[index]);
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

error builtin_add(atom args, atom *result)
{
	atom acc = make_number(0);
	atom a;
	if (!listp(args)) return ERROR_ARGS;

	a = args;
	while (!no(a)) {
		if (car(a).type != T_NUM) return ERROR_TYPE;
		acc.value.number += car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return ERROR_OK;
}

error builtin_subtract(atom args, atom *result)
{
	atom acc;
	atom a;
	if (!listp(args)) return ERROR_ARGS;
	if (no(args)) { /* 0 argument */
		*result = make_number(0);
		return ERROR_OK;
	}
	if (no(cdr(args))) { /* 1 argument */
		if (car(args).type != T_NUM) return ERROR_TYPE;
		*result = make_number(-car(args).value.number);
		return ERROR_OK;
	}
	acc = make_number(car(args).value.number);
	a = cdr(args);
	while (!no(a)) {
		if (car(a).type != T_NUM) return ERROR_TYPE;
		acc.value.number -= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return ERROR_OK;
}

error builtin_multiply(atom args, atom *result)
{
	atom acc = make_number(1);
	atom a;
	if (!listp(args)) return ERROR_ARGS;

	a = args;
	while (!no(a)) {
		if (car(a).type != T_NUM) return ERROR_TYPE;
		acc.value.number *= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return ERROR_OK;
}

error builtin_divide(atom args, atom *result)
{
	atom acc;
	atom a;
	if (!listp(args)) return ERROR_ARGS;
	if (no(args)) { /* 0 argument */
		*result = make_number(1);
		return ERROR_OK;
	}
	if (no(cdr(args))) { /* 1 argument */
		if (car(args).type != T_NUM) return ERROR_TYPE;
		*result = make_number(1.0 / car(args).value.number);
		return ERROR_OK;
	}
	acc = make_number(car(args).value.number);
	a = cdr(args);
	while (!no(a)) {
		if (car(a).type != T_NUM) return ERROR_TYPE;
		acc.value.number /= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return ERROR_OK;
}

error builtin_less(atom args, atom *result)
{
	atom a, b;
	if (no(args) || no(cdr(args)) || !no(cdr(cdr(args)))) return ERROR_ARGS;
	a = car(args);
	b = car(cdr(args));
	if (a.type != T_NUM || b.type != T_NUM) return ERROR_TYPE;
	*result = (a.value.number < b.value.number) ? sym_t : nil;
	return ERROR_OK;
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

error builtin_is(atom args, atom *result)
{
	atom a, b;
	int eq = 0;

	if (no(args) || no(cdr(args)) || !no(cdr(cdr(args))))
		return ERROR_ARGS;

	a = car(args);
	b = car(cdr(args));

	if (a.type == b.type) {
		switch (a.type) {
		case T_NIL:
			eq = 1;
			break;
		case T_CONS:
		case T_CLOSURE:
		case T_MACRO:
			eq = (a.value.pair == b.value.pair);
			break;
		case T_SYMBOL:
			eq = (a.value.symbol == b.value.symbol);
			break;
		case T_NUM:
			eq = (a.value.number == b.value.number);
			break;
		case T_BUILTIN:
			eq = (a.value.builtin == b.value.builtin);
			break;
		default:
			/* impossible */
			break;
		}
	}

	*result = eq ? sym_t : nil;
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
	*result = make_number((long)dividend.value.number % (long)divisor.value.number);
	return ERROR_OK;
}

error builtin_type(atom args, atom *result) {
  atom x = car(args);
  switch (x.type) {
  case T_CONS: *result = sym_cons; break;
  case T_SYMBOL:
  case T_NIL: *result = sym_sym; break;
  case T_BUILTIN:
  case T_CLOSURE: *result = sym_fn; break;
  case T_STRING: *result = sym_string; break;
  case T_NUM: *result = sym_num; break;
  case T_MACRO: *result = sym_mac; break;
  default: *result = nil; break; /* impossible */
  }
  return ERROR_OK;
}

error builtin_string_sref(atom args, atom *result) {
	atom index, obj, value;
	if (len(args) != 3) return ERROR_ARGS;
	index = car(cdr(cdr(args)));
	obj = car(args), value;
	if (obj.type != T_STRING) return ERROR_TYPE;
	value = car(cdr(args));
	obj.value.symbol[(long)index.value.number] = (char)value.value.number;
	*result = make_number(value.value.number);
	return ERROR_OK;
}

error builtin_pr(atom args, atom *result) {
  if (no(args)) {
	*result = nil;
	return ERROR_OK;
  }
  *result = car(args);
  while (!no(args)) {
	pr(car(args));
	args = cdr(args);
  }
  return ERROR_OK;
}

error builtin_writeb(atom args, atom *result) {
  if (len(args) != 1) return ERROR_ARGS;
  putchar((int)car(args).value.number);
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
	if (len(args) != 0) return ERROR_ARGS;
	char *str = readline("");
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
		const char *buf = s;
		error err = read_expr(buf, &buf, result);
		free(s);
		return err;
	}
	else if (alen == 1) {
		s = car(args).value.symbol;
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
		char *a = to_string(car(args));
		strcat_alloc(&s, a);
		args = cdr(args);
	}
	*result = make_string(s);
	return ERROR_OK;
}

error builtin_sym(atom args, atom *result) {
	long alen = len(args);
	if (alen == 1) {
		*result = make_sym(to_string(car(args)));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_system(atom args, atom *result) {
	long alen = len(args);
	if (alen == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;
		*result = make_number(system(car(args).value.symbol));
		return ERROR_OK;
	}
	else return ERROR_ARGS;
}

error builtin_eval(atom args, atom *result) {
	if (len(args) == 1) return macex_eval(car(args), env, result);
	else return ERROR_ARGS;
}

error builtin_load(atom args, atom *result) {
	if (len(args) == 1) {
		atom a = car(args);
		if (a.type != T_STRING) return ERROR_TYPE;		
		*result = nil;
		return load_file(env, a.value.symbol);
	}
	else return ERROR_ARGS;
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

char *to_string(atom atom) {
	char *s = str_new();
	char buf[80];
	switch (atom.type) {
	case T_NIL:
		printf("*nilnilnil*");
		strcat_alloc(&s, "nil");		
		break;
	case T_CONS:
		strcat_alloc(&s, "(");
		strcat_alloc(&s, to_string(car(atom)));
		atom = cdr(atom);
		while (!no(atom)) {
			if (atom.type == T_CONS) {
				strcat_alloc(&s, " ");
				strcat_alloc(&s, to_string(car(atom)));
				atom = cdr(atom);
			}
			else {
				strcat_alloc(&s, " . ");
				strcat_alloc(&s, to_string(atom));
				break;
			}
		}
		strcat_alloc(&s, ")");
		break;
	case T_SYMBOL:
	case T_STRING:
		strcat_alloc(&s, atom.value.symbol);
		break;
	case T_NUM:
		sprintf(buf, "%.16g", atom.value.number);
		strcat_alloc(&s, buf);
		break;
	case T_BUILTIN:		
		sprintf(buf, "#<builtin:%p>", atom.value.builtin);
		strcat_alloc(&s, buf);
		break;
	case T_CLOSURE:
		strcat_alloc(&s, "(closure ");
		strcat_alloc(&s, to_string(cdr(atom)));
		strcat_alloc(&s, ")");
		break;
	case T_MACRO:
		strcat_alloc(&s, "(macro ");
		strcat_alloc(&s, to_string(cdr(atom)));
		strcat_alloc(&s, ")");
		break;
	default:
		strcat_alloc(&s, "(unknown type)");
		break;
	}
	return s;
}

char *slurp(const char *path)
{
	FILE *file;
	char *buf;
	long len;

	file = fopen(path, "rb");
	if (!file) {
		/* printf("Reading %s failed.\n", path); */
		return NULL;
	}
	fseek(file, 0, SEEK_END);
	len = ftell(file);
	if (len < 0) return NULL;
	fseek(file, 0, SEEK_SET);

	buf = (char *)malloc(len + 1);
	if (!buf)
		return NULL;

	fread(buf, 1, len, file);
	buf[len] = 0;
	fclose(file);

	return buf;
}

/* compile-time macro */
error macex(atom expr, atom *result) {
	error err = ERROR_OK;
	int ss = stack_size; /* save stack point */

	if (cons_count > 10000) {
		stack_add(expr);
		stack_add(env);
		gc();
		cons_count = 0;
	}

	if (expr.type == T_SYMBOL) {
		*result = expr;		
		stack_restore(ss);
		stack_add(*result);
		return ERROR_OK;
	}
	else if (expr.type != T_CONS) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return ERROR_OK;
	}
	else if (!listp(expr)) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return ERROR_OK;
	}
	else {
		atom op = car(expr);
		atom args = cdr(expr);

		if (op.type == T_SYMBOL) {
			/* Handle special forms */

			if (op.value.symbol == sym_quote.value.symbol) {
				if (no(args) || !no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				*result = expr;
				stack_restore(ss);
				stack_add(*result);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				atom name, macro;

				if (no(args) || no(cdr(args)) || no(cdr(cdr(args)))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				name = car(args);
				if (name.type != T_SYMBOL) {
					stack_restore(ss);
					return ERROR_TYPE;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = T_MACRO;
					*result = cons(sym_quote, cons(car(args), nil));
					err = env_assign(env, name, macro);
					stack_restore(ss);
					stack_add(*result);
					return err;
				}
				else {
					stack_restore(ss);
					return err;
				}
			}
		}

		/* Is it a macro? */
		if (op.type == T_SYMBOL && !env_get(env, op, result) && result->type == T_MACRO) {
			/* Evaluate operator */
			err = eval_expr(op, env, &op);
			if (err) {
				stack_restore(ss);
				stack_add(*result);
				return err;
			}

			op.type = T_CLOSURE;
			err = apply(op, args, result);
			if (err) {
				stack_restore(ss);
				return err;
			}
			stack_restore(ss);
			stack_add(*result);
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
					stack_add(*result);
					return err;
				}
				p = cdr(p);
			}
			*result = expr2;
			stack_restore(ss);
			stack_add(*result);
			return ERROR_OK;
		}
	}
}

error macex_eval(atom expr, atom env, atom *result) {
	atom expr2;
	error err = macex(expr, &expr2);
	if (err) return err;
	/*printf("expanded: ");
	print_expr(expr2);
	puts("");*/
	return eval_expr(expr2, env, result);
}

error load_file(atom env, const char *path)
{
	char *text;

	/* printf("Reading %s...\n", path); */
	text = slurp(path);
	if (text) {
		const char *p = text;
		atom expr;
		while (read_expr(p, &p, &expr) == ERROR_OK) {
			atom result;
			error err = macex_eval(expr, env, &result);
			if (err) {
				puts(error_string[err]);
				printf("error in expression:\n\t");
				print_expr(expr);
				putchar('\n');
			}
			/*else {
				print_expr(result);
				putchar(' ');
			}*/
		}
		/*puts("");*/
		free(text);
		return ERROR_OK;
	}
	else {
		return ERROR_FILE;
	}
}

void stack_restore(int saved_size) {
	stack_size = saved_size;
	/* if there is waste of memory, realloc */
	if (stack_size < stack_capacity / 4) {
		stack_capacity /= 2;
		stack = realloc(stack, stack_capacity * sizeof(atom));
	}
}

error eval_expr(atom expr, atom env, atom *result)
{
	error err = ERROR_OK;
	int ss = stack_size; /* save stack point */

	/*if (cons_count > 10000) {
	  gc_mark(expr);
	  gc_mark(env);
	  gc();
	  cons_count = 0;
	}*/
	if (cons_count > 10000) {
		stack_add(expr);
		stack_add(env);
		gc();
		cons_count = 0;
	}

	if (expr.type == T_SYMBOL) {
		err = env_get(env, expr, result);
		stack_restore(ss);
		stack_add(*result);
		return err;
	}
	else if (expr.type != T_CONS) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return ERROR_OK;
	}
	else if (!listp(expr)) {
		stack_restore(ss);
		return ERROR_SYNTAX;
	}
	else {
		atom op = car(expr);
		atom args = cdr(expr);

		if (op.type == T_SYMBOL) {
			/* Handle special forms */

			if (op.value.symbol == sym_quote.value.symbol) {
				if (no(args) || !no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				*result = car(args);
				stack_restore(ss);
				stack_add(*result);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_assign.value.symbol) {
				atom sym;
				if (no(args) || no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				sym = car(args);
				if (sym.type == T_SYMBOL) {
					atom val;
					err = eval_expr(car(cdr(args)), env, &val);
					if (err) {
						stack_restore(ss);
						stack_add(*result);
						return err;
					}

					*result = val;
					err = env_assign_eq(env, sym, val);
					stack_restore(ss);
					stack_add(*result);
					return err;
				}
				else {
					stack_restore(ss);
					return ERROR_TYPE;
				}
			}
			else if (op.value.symbol == sym_fn.value.symbol) {
				if (no(args) || no(cdr(args))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}
				err = make_closure(env, car(args), cdr(args), result);
				stack_restore(ss);
				stack_add(*result);
				return err;
			}
			else if (op.value.symbol == sym_if.value.symbol) {
			  atom cond;
				while (!no(args)) {
				  err = eval_expr(car(args), env, &cond);
				  if (err) {
					stack_restore(ss);
					stack_add(*result);
					return err;
				  }
				  if (no(cdr(args))) {
					*result = cond;
					stack_restore(ss);
					stack_add(*result);
					return ERROR_OK;
				  }
				  if (!no(cond)) {
					err = eval_expr(car(cdr(args)), env, result);					
					stack_restore(ss);
					stack_add(*result);
					return err;
				  }				  
				  args = cdr(cdr(args));
				}
				*result = nil;
				stack_restore(ss);
				stack_add(*result);
				return ERROR_OK;
			}
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				atom name, macro;

				if (no(args) || no(cdr(args)) || no(cdr(cdr(args)))) {
					stack_restore(ss);
					return ERROR_ARGS;
				}

				name = car(args);
				if (name.type != T_SYMBOL) {
					stack_restore(ss);
					return ERROR_TYPE;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = T_MACRO;
					*result = name;
					err = env_assign(env, name, macro);
					stack_restore(ss);
					stack_add(*result);
					return err;
				}
				else {
					stack_restore(ss);
					stack_add(*result);
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
				stack_add(*result);
				return ERROR_OK;
			}
		}

		/* Evaluate operator */			
		err = eval_expr(op, env, &op);
		if (err) {
			stack_restore(ss);
			stack_add(*result);
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
			  stack_add(*result);
			  return err;
		  }
		  err = eval_expr(expansion, env, result);
		  stack_restore(ss);
		  stack_add(*result);
		  return err;
		}
			  
		/* Evaulate arguments */
		args = copy_list(args);
		atom p = args;
		while (!no(p)) {
		  err = eval_expr(car(p), env, &car(p));
		  if (err) {
			  stack_restore(ss);
			  stack_add(*result);
			  return err;
		  }
				
		  p = cdr(p);
		}
		err = apply(op, args, result);
		stack_restore(ss);
		stack_add(*result);
		return err;
	}
}

void print_logo() {
	printf("Arcadia %s\n", VERSION);
}

void init(atom *env) {
	srand((unsigned int)time(0));
	*env = env_create(nil);

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

	env_assign(*env, make_sym("car"), make_builtin(builtin_car));
	env_assign(*env, make_sym("cdr"), make_builtin(builtin_cdr));
	env_assign(*env, make_sym("cons"), make_builtin(builtin_cons));
	env_assign(*env, make_sym("+"), make_builtin(builtin_add));
	env_assign(*env, make_sym("-"), make_builtin(builtin_subtract));
	env_assign(*env, make_sym("*"), make_builtin(builtin_multiply));
	env_assign(*env, make_sym("/"), make_builtin(builtin_divide));
	env_assign(*env, sym_t, sym_t);
	env_assign(*env, make_sym("<"), make_builtin(builtin_less));
	env_assign(*env, make_sym("apply"), make_builtin(builtin_apply));
	env_assign(*env, make_sym("is"), make_builtin(builtin_is));
	env_assign(*env, make_sym("scar"), make_builtin(builtin_scar));
	env_assign(*env, make_sym("scdr"), make_builtin(builtin_scdr));
	env_assign(*env, make_sym("mod"), make_builtin(builtin_mod));
	env_assign(*env, make_sym("type"), make_builtin(builtin_type));
	env_assign(*env, make_sym("string-sref"), make_builtin(builtin_string_sref));
	env_assign(*env, make_sym("pr"), make_builtin(builtin_pr));
	env_assign(*env, make_sym("writeb"), make_builtin(builtin_writeb));
	env_assign(*env, make_sym("expt"), make_builtin(builtin_expt));
	env_assign(*env, make_sym("log"), make_builtin(builtin_log));
	env_assign(*env, make_sym("sqrt"), make_builtin(builtin_sqrt));
	env_assign(*env, make_sym("readline"), make_builtin(builtin_readline));
	env_assign(*env, make_sym("quit"), make_builtin(builtin_quit));
	env_assign(*env, make_sym("rand"), make_builtin(builtin_rand));
	env_assign(*env, make_sym("read"), make_builtin(builtin_read));
	env_assign(*env, make_sym("macex"), make_builtin(builtin_macex));
	env_assign(*env, make_sym("string"), make_builtin(builtin_string));
	env_assign(*env, make_sym("sym"), make_builtin(builtin_sym));
	env_assign(*env, make_sym("system"), make_builtin(builtin_system));
	env_assign(*env, make_sym("eval"), make_builtin(builtin_eval));
	env_assign(*env, make_sym("load"), make_builtin(builtin_load));

	if (load_file(*env, "library.arc") != ERROR_OK) {
		load_file(*env, "../library.arc");
	}
}

void print_env(atom env) {
	/* print the environment */
	puts("Environment:");
	atom a = cdr(env);
	while (!no(a)) {
		atom env_pair = car(a);
		printf(" %s", car(env_pair).value.symbol);
		a = cdr(a);
	}
	puts("");
}

void repl(atom env) {
	char *input;

	while ((input = readline("> ")) != NULL) {
		char *buf = (char *)malloc(strlen(input) + 4);
		sprintf(buf, "(%s\n)", input);
		const char *p = buf;
		error err;
		atom result;

		err = read_expr(p, &p, &code_expr);

		while (!no(code_expr)) {
			if (!err)
				err = macex_eval(car(code_expr), env, &result);
			if (err)
				puts(error_string[err]);
			else {
				print_expr(result);
				putchar('\n');
			}
			code_expr = cdr(code_expr);
		}
		free(buf);
		free(input);
	}
}

int main(int argc, char **argv)
{	
	if (argc == 1) { /* REPL */
		print_logo();
		init(&env);
		print_env(env);
		repl(env);
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
	init(&env);
	int i;
	for (i = 1; i < argc; i++) {
		if (!load_file(env, argv[i])) {
			fprintf(stderr, "Cannot open file: %s\n", argv[i]);
		}
	}
	return 0;
}
