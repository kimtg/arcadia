// #qo CFLAGS: -s -Wall -O3
// #qo LDFLAGS: -static

#define VERSION "0.4.8"

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#ifdef _MSC_VER
#define strdup _strdup
#endif

enum AtomType {
	AtomType_Nil,
	AtomType_Pair,
	AtomType_Symbol,
	AtomType_Number,
	AtomType_Builtin,
	AtomType_Closure,
	AtomType_Macro,
	AtomType_String
};

typedef enum {
	Error_OK = 0, Error_Syntax, Error_Unbound, Error_Args, Error_Type
} Error;

char *error_string[] = { "", "Syntax error", "Symbol not bound", "Wrong number of arguments", "Wrong type" };

typedef struct Atom Atom;
typedef Error(*Builtin)(Atom args, Atom *result);

struct Atom {
	enum AtomType type;

	union {
		double number;
		struct Pair *pair;
		char *symbol;
		Builtin builtin;
	} value;
};

static Atom sym_table = { AtomType_Nil };

struct Pair {
	struct Atom car, cdr;
};

/* forward declarations */
Error apply(Atom fn, Atom args, Atom *result);
int listp(Atom expr);
char *slurp(const char *path);
Error eval_expr(Atom expr, Atom env, Atom *result);
void gc_mark(Atom root);
void gc();
void stack_restore(int saved_size);

#define car(p) ((p).value.pair->car)
#define cdr(p) ((p).value.pair->cdr)
#define nilp(atom) ((atom).type == AtomType_Nil)

static const Atom nil = { AtomType_Nil };
/* symbols for faster comparison */
static Atom sym_t, sym_quote, sym_set, sym_fn, sym_if, sym_mac, sym_apply, sym_while;
Atom code_expr;

struct Allocation {	
	struct Pair pair;
	char mark;
	struct Allocation *next;
};

struct Allocation *global_allocations = NULL;
int cons_count = 0;

Atom *stack = NULL;
int stack_capacity = 0;
int stack_size = 0;

void stack_add(Atom a) {
	stack_size++;
	if (stack_size > stack_capacity) {
		stack_capacity = stack_size * 2;
		stack = realloc(stack, stack_capacity * sizeof(Atom));
	}
	stack[stack_size - 1] = a;
}

Atom cons(Atom car_val, Atom cdr_val)
{
	struct Allocation *a;
	Atom p;

	cons_count++;

	a = malloc(sizeof(struct Allocation));
	if (a == NULL) {
		puts("Not enough memory.");
		exit(1);
	}
	a->mark = 0;
	a->next = global_allocations;
	global_allocations = a;

	p.type = AtomType_Pair;
	p.value.pair = &a->pair;

	car(p) = car_val;
	cdr(p) = cdr_val;

	stack_add(p);
	
	return p;
}

void gc_mark(Atom root)
{
	struct Allocation *a;

	if (!(root.type == AtomType_Pair
		|| root.type == AtomType_Closure
		|| root.type == AtomType_Macro))
		return;

	a = (struct Allocation *)
		((char *)root.value.pair
		- offsetof(struct Allocation, pair));

	if (a->mark)
		return;

	a->mark = 1;

	gc_mark(car(root));
	gc_mark(cdr(root));
}

void gc()
{
	struct Allocation *a, **p;

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
}


Atom make_number(double x)
{
	Atom a;
	a.type = AtomType_Number;
	a.value.number = x;
	return a;
}

Atom make_sym(const char *s)
{
	Atom a, p;

	p = sym_table;
	while (!nilp(p)) {
		a = car(p);
		if (strcmp(a.value.symbol, s) == 0)
			return a;
		p = cdr(p);
	}

	a.type = AtomType_Symbol;
	a.value.symbol = (char*)strdup(s);
	sym_table = cons(a, sym_table);

	return a;
}

Atom make_builtin(Builtin fn)
{
	Atom a;
	a.type = AtomType_Builtin;
	a.value.builtin = fn;
	return a;
}

Error make_closure(Atom env, Atom args, Atom body, Atom *result)
{
	Atom p;

	if (!listp(body))
		return Error_Syntax;

	/* Check argument names are all symbols */
	p = args;
	while (!nilp(p)) {
		if (p.type == AtomType_Symbol)
			break;
		else if (p.type != AtomType_Pair
			|| car(p).type != AtomType_Symbol)
			return Error_Type;
		p = cdr(p);
	}

	*result = cons(env, cons(args, body));
	result->type = AtomType_Closure;

	return Error_OK;
}



void print_expr(Atom atom)
{
	switch (atom.type) {
	case AtomType_Nil:
		printf("nil");
		break;
	case AtomType_Pair:
		putchar('(');
		print_expr(car(atom));
		atom = cdr(atom);
		while (!nilp(atom)) {
			if (atom.type == AtomType_Pair) {
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
	case AtomType_Symbol:
		printf("%s", atom.value.symbol);
		break;
	case AtomType_Number:
		printf("%.16g", atom.value.number);
		break;
	case AtomType_Builtin:
		printf("#<BUILTIN:%p>", atom.value.builtin);
		break;
	case AtomType_Closure:
		printf("closure ");
		print_expr(cdr(atom));
		break;
	case AtomType_String:
		printf("\"%s\"", atom.value.symbol);
		break;
	case AtomType_Macro:
		printf("macro ");
		print_expr(cdr(atom));
		break;
	default:
		printf("unknown type");
		break;
	}
}

Error lex(const char *str, const char **start, const char **end)
{
	const char *ws = " \t\r\n";
	const char *delim = "() \t\r\n";
	const char *prefix = "()'`";

	str += strspn(str, ws);

	if (str[0] == '\0') {
		*start = *end = NULL;
		return Error_Syntax;
	}

	*start = str;

	if (strchr(prefix, str[0]) != NULL)
		*end = str + 1;
	else if (str[0] == ',')
		*end = str + (str[1] == '@' ? 2 : 1);
	else if (str[0] == '"') {
		*end = str + strcspn(str + 1, "\"") + 2;
	}
	else
		*end = str + strcspn(str, delim);

	return Error_OK;
}

Error read_expr(const char *input, const char **end, Atom *result);

Error parse_simple(const char *start, const char *end, Atom *result)
{
	char *buf, *p;

	/* Is it an integer? */
	double val = strtod(start, &p);
	if (p == end) {
		result->type = AtomType_Number;
		result->value.number = val;
		return Error_OK;
	}
	else if (start[0] == '"') {
		result->type = AtomType_String;
		result->value.symbol = (char*)malloc(end - start - 1);
		memcpy(result->value.symbol, start + 1, end - start);
		result->value.symbol[end - start - 2] = 0;
		return Error_OK;
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

	return Error_OK;
}

Error read_list(const char *start, const char **end, Atom *result)
{
	Atom p;

	*end = start;
	p = *result = nil;

	for (;;) {
		const char *token;
		Atom item;
		Error err;

		err = lex(*end, &token, end);
		if (err)
			return err;

		if (token[0] == ')')
			return Error_OK;

		if (token[0] == '.' && *end - token == 1) {
			/* Improper list */
			if (nilp(p))
				return Error_Syntax;

			err = read_expr(*end, end, &item);
			if (err)
				return err;

			cdr(p) = item;

			/* Read the closing ')' */
			err = lex(*end, &token, end);
			if (!err && token[0] != ')')
				err = Error_Syntax;

			return err;
		}

		err = read_expr(token, end, &item);
		if (err)
			return err;

		if (nilp(p)) {
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

Error read_expr(const char *input, const char **end, Atom *result)
{
	const char *token;
	Error err;

	err = lex(input, &token, end);
	if (err)
		return err;

	if (token[0] == '(')
		return read_list(*end, end, result);
	else if (token[0] == ')')
		return Error_Syntax;
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
	char ret[2000]; /* one screenful */
	printf(prompt);
	fgets(ret, sizeof(ret), stdin);
	if (feof(stdin)) return NULL;
	return (char*)strdup(ret);
}

Atom env_create(Atom parent)
{
	return cons(parent, nil);
}

Error env_get(Atom env, Atom symbol, Atom *result)
{
	Atom parent = car(env);
	Atom bs = cdr(env);

	while (!nilp(bs)) {
		Atom b = car(bs);
		if (car(b).value.symbol == symbol.value.symbol) {
			*result = cdr(b);
			return Error_OK;
		}
		bs = cdr(bs);
	}

	if (nilp(parent)) {
		/*printf("%s: ", symbol.value.symbol);*/
		return Error_Unbound;
	}

	return env_get(parent, symbol, result);
}

Error env_set(Atom env, Atom symbol, Atom value)
{
	Atom bs = cdr(env);
	Atom b = nil;

	while (!nilp(bs)) {
		b = car(bs);
		if (car(b).value.symbol == symbol.value.symbol) {
			cdr(b) = value;
			return Error_OK;
		}
		bs = cdr(bs);
	}

	b = cons(symbol, value);
	cdr(env) = cons(b, cdr(env));

	return Error_OK;
}

Error env_set_eq(Atom env, Atom symbol, Atom value) {
	Atom env_origin = env;
	while (!nilp(env)) {
		Atom bs = cdr(env);

		while (!nilp(bs)) {
			Atom b = car(bs);
			if (car(b).value.symbol == symbol.value.symbol) {
			  cdr(b) = value;
			  return Error_OK;
			}
			bs = cdr(bs);
		}
		env = car(env);
	}

	return env_set(env_origin, symbol, value);
}

int listp(Atom expr)
{
	while (!nilp(expr)) {
		if (expr.type != AtomType_Pair)
			return 0;
		expr = cdr(expr);
	}
	return 1;
}

Atom copy_list(Atom list)
{
	Atom a, p;

	if (nilp(list))
		return nil;

	a = cons(car(list), nil);
	p = a;
	list = cdr(list);

	while (!nilp(list)) {
		cdr(p) = cons(car(list), nil);
		p = cdr(p);
		list = cdr(list);
		if (list.type != AtomType_Pair) { /* improper list */
			p = list;
			break;
		}
	}

	return a;
}

Error apply(Atom fn, Atom args, Atom *result)
{
	Atom env, arg_names, body;

	if (fn.type == AtomType_Builtin)
		return (*fn.value.builtin)(args, result);
	else if (fn.type == AtomType_Closure) {
		env = env_create(car(fn));
		arg_names = car(cdr(fn));
		body = cdr(cdr(fn));

		/* Bind the arguments */
		while (!nilp(arg_names)) {
			if (arg_names.type == AtomType_Symbol) {
				env_set(env, arg_names, args);
				args = nil;
				break;
			}

			if (nilp(args))
				return Error_Args;
			env_set(env, car(arg_names), car(args));
			arg_names = cdr(arg_names);
			args = cdr(args);
		}
		if (!nilp(args))
			return Error_Args;

		/* Evaluate the body */
		while (!nilp(body)) {
			Error err = eval_expr(car(body), env, result);
			if (err)
				return err;
			body = cdr(body);
		}

		return Error_OK;
	}
	else if (fn.type == AtomType_String) { /* implicit indexing for string */
		long index = (long)(car(args)).value.number;
		*result = make_number(fn.value.symbol[index]);
		return Error_OK;
	}
	else if (fn.type == AtomType_Pair && listp(fn)) { /* implicit indexing for list */
		long index = (long)(car(args)).value.number;
		Atom a = fn;
		long i;
		for (i = 0; i < index; i++) {
			a = cdr(a);
			if (nilp(a)) {
				*result = nil;
				return Error_OK;
			}
		}
		*result = car(a);
		return Error_OK;
	}
	else {
		return Error_Type;
	}
}

Error builtin_car(Atom args, Atom *result)
{
	if (nilp(args) || !nilp(cdr(args)))
		return Error_Args;

	if (nilp(car(args)))
		*result = nil;
	else if (car(args).type != AtomType_Pair)
		return Error_Type;
	else
		*result = car(car(args));

	return Error_OK;
}

Error builtin_cdr(Atom args, Atom *result)
{
	if (nilp(args) || !nilp(cdr(args)))
		return Error_Args;

	if (nilp(car(args)))
		*result = nil;
	else if (car(args).type != AtomType_Pair)
		return Error_Type;
	else
		*result = cdr(car(args));

	return Error_OK;
}

Error builtin_cons(Atom args, Atom *result)
{
	if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
		return Error_Args;

	*result = cons(car(args), car(cdr(args)));

	return Error_OK;
}

Error builtin_add(Atom args, Atom *result)
{
	Atom acc = make_number(0);
	Atom a;
	if (!listp(args)) return Error_Args;

	a = args;
	while (!nilp(a)) {
		acc.value.number += car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return Error_OK;
}

Error builtin_subtract(Atom args, Atom *result)
{
	Atom acc;
	Atom a;
	if (!listp(args)) return Error_Args;
	if (nilp(args)) { /* 0 argument */
		*result = make_number(0);
		return Error_OK;
	}
	if (nilp(cdr(args))) { /* 1 argument */
		*result = make_number(-car(args).value.number);
		return Error_OK;
	}
	acc = make_number(car(args).value.number);
	a = cdr(args);
	while (!nilp(a)) {
		acc.value.number -= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return Error_OK;
}

Error builtin_multiply(Atom args, Atom *result)
{
	Atom acc = make_number(1);
	Atom a;
	if (!listp(args)) return Error_Args;

	a = args;
	while (!nilp(a)) {
		acc.value.number *= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return Error_OK;
}

Error builtin_divide(Atom args, Atom *result)
{
	Atom acc;
	Atom a;
	if (!listp(args)) return Error_Args;
	if (nilp(args)) { /* 0 argument */
		*result = make_number(1);
		return Error_OK;
	}
	if (nilp(cdr(args))) { /* 1 argument */
		*result = make_number(1.0 / car(args).value.number);
		return Error_OK;
	}
	acc = make_number(car(args).value.number);
	a = cdr(args);
	while (!nilp(a)) {
		acc.value.number /= car(a).value.number;
		a = cdr(a);
	}
	*result = acc;
	return Error_OK;
}

Error builtin_less(Atom args, Atom *result)
{
	Atom a, b;

	if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
		return Error_Args;

	a = car(args);
	b = car(cdr(args));

	if (a.type != AtomType_Number || b.type != AtomType_Number)
		return Error_Type;

	*result = (a.value.number < b.value.number) ? sym_t : nil;

	return Error_OK;
}

Error builtin_apply(Atom args, Atom *result)
{
	Atom fn;

	if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
		return Error_Args;

	fn = car(args);
	args = car(cdr(args));

	if (!listp(args))
		return Error_Syntax;

	return apply(fn, args, result);
}

Error builtin_is(Atom args, Atom *result)
{
	Atom a, b;
	int eq = 0;

	if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
		return Error_Args;

	a = car(args);
	b = car(cdr(args));

	if (a.type == b.type) {
		switch (a.type) {
		case AtomType_Nil:
			eq = 1;
			break;
		case AtomType_Pair:
		case AtomType_Closure:
		case AtomType_Macro:
			eq = (a.value.pair == b.value.pair);
			break;
		case AtomType_Symbol:
			eq = (a.value.symbol == b.value.symbol);
			break;
		case AtomType_Number:
			eq = (a.value.number == b.value.number);
			break;
		case AtomType_Builtin:
			eq = (a.value.builtin == b.value.builtin);
			break;
		default:
			/* impossible */
			break;
		}
	}

	*result = eq ? sym_t : nil;
	return Error_OK;
}

Error builtin_pairp(Atom args, Atom *result)
{
	if (nilp(args) || !nilp(cdr(args)))
		return Error_Args;

	*result = (car(args).type == AtomType_Pair) ? sym_t : nil;
	return Error_OK;
}

Error builtin_no(Atom args, Atom *result)
{
	if (nilp(args) || !nilp(cdr(args)))
		return Error_Args;

	*result = (nilp(car(args))) ? sym_t : nil;
	return Error_OK;
}

Error builtin_scar(Atom args, Atom *result) {
	Atom place = car(args);
	Atom value = car(cdr(args));
	place.value.pair->car = value;
	*result = value;
	return Error_OK;
}

Error builtin_scdr(Atom args, Atom *result) {
	Atom place = car(args);
	Atom value = car(cdr(args));
	place.value.pair->cdr = value;
	*result = value;
	return Error_OK;
}

Error builtin_mod(Atom args, Atom *result) {
	Atom dividend = car(args);
	Atom divisor = car(cdr(args));
	*result = make_number((long)dividend.value.number % (long)divisor.value.number);
	return Error_OK;
}

char *slurp(const char *path)
{
	FILE *file;
	char *buf;
	long len;

	file = fopen(path, "rb");
	if (!file) {
		printf("Reading %s filed.\n", path);
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
Error preprocess(Atom expr, Atom env, Atom *result) {
	Error err = Error_OK;
	int ss = stack_size; /* save stack point */

	if (cons_count > 10000) {
		stack_add(expr);
		stack_add(env);
		gc();
		cons_count = 0;
	}

	if (expr.type == AtomType_Symbol) {
		*result = expr;		
		stack_restore(ss);
		stack_add(*result);
		return Error_OK;
	}
	else if (expr.type != AtomType_Pair) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return Error_OK;
	}
	else if (!listp(expr)) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return Error_OK;
	}
	else {
		Atom op = car(expr);
		Atom args = cdr(expr);

		if (op.type == AtomType_Symbol) {
			/* Handle special forms */

			if (op.value.symbol == sym_quote.value.symbol) {
				if (nilp(args) || !nilp(cdr(args))) {
					stack_restore(ss);
					return Error_Args;
				}

				*result = expr;
				stack_restore(ss);
				stack_add(*result);
				return Error_OK;
			}
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				Atom name, macro;

				if (nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args)))) {
					stack_restore(ss);
					return Error_Args;
				}

				name = car(args);
				if (name.type != AtomType_Symbol) {
					stack_restore(ss);
					return Error_Type;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = AtomType_Macro;
					*result = cons(sym_quote, cons(car(args), nil));
					err = env_set(env, name, macro);
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
		if (op.type == AtomType_Symbol && !env_get(env, op, result) && result->type == AtomType_Macro) {
			/* Evaluate operator */
			err = eval_expr(op, env, &op);
			if (err) {
				stack_restore(ss);
				stack_add(*result);
				return err;
			}

			op.type = AtomType_Closure;
			err = apply(op, args, result);
			if (err) {
				stack_restore(ss);
				return err;
			}
			stack_restore(ss);
			stack_add(*result);
			return Error_OK;
		}
		else {
			/* preprocess elements */
			Atom expr2 = copy_list(expr);
			Atom p = expr2;
			while (!nilp(p)) {
				err = preprocess(car(p), env, &car(p));
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
			return Error_OK;
		}
	}
}

Error preprocess_eval(Atom expr, Atom env, Atom *result) {
	Atom expr2;
	Error err = preprocess(expr, env, &expr2);
	if (err) return err;
	/*printf("expanded: ");
	print_expr(expr2);
	puts("");*/
	return eval_expr(expr2, env, result);
}

int load_file(Atom env, const char *path)
{
	char *text;

	printf("Reading %s...\n", path);
	text = slurp(path);
	if (text) {
		const char *p = text;
		Atom expr;
		while (read_expr(p, &p, &expr) == Error_OK) {
			Atom result;
			Error err = preprocess_eval(expr, env, &result);
			if (err) {
				puts(error_string[err]);
				printf("Error in expression:\n\t");
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
		return 1;
	}
	else {
		return 0;
	}
}

void stack_restore(int saved_size) {
	stack_size = saved_size;
	/* if there is waste of memory, realloc */
	if (stack_size < stack_capacity / 4) {
		stack_capacity /= 2;
		stack = realloc(stack, stack_capacity * sizeof(Atom));
	}
}

Error eval_expr(Atom expr, Atom env, Atom *result)
{
	Error err = Error_OK;
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

	if (expr.type == AtomType_Symbol) {
		err = env_get(env, expr, result);
		stack_restore(ss);
		stack_add(*result);
		return err;
	}
	else if (expr.type != AtomType_Pair) {
		*result = expr;
		stack_restore(ss);
		stack_add(*result);
		return Error_OK;
	}
	else if (!listp(expr)) {
		stack_restore(ss);
		return Error_Syntax;
	}
	else {
		Atom op = car(expr);
		Atom args = cdr(expr);

		if (op.type == AtomType_Symbol) {
			/* Handle special forms */

			if (op.value.symbol == sym_quote.value.symbol) {
				if (nilp(args) || !nilp(cdr(args))) {
					stack_restore(ss);
					return Error_Args;
				}

				*result = car(args);
				stack_restore(ss);
				stack_add(*result);
				return Error_OK;
			}
			else if (op.value.symbol == sym_set.value.symbol) {
				Atom sym;
				if (nilp(args) || nilp(cdr(args))) {
					stack_restore(ss);
					return Error_Args;
				}

				sym = car(args);
				if (sym.type == AtomType_Symbol) {
					Atom val;
					err = eval_expr(car(cdr(args)), env, &val);
					if (err) {
						stack_restore(ss);
						stack_add(*result);
						return err;
					}

					*result = val;
					err = env_set_eq(env, sym, val);
					stack_restore(ss);
					stack_add(*result);
					return err;
				}
				else {
					stack_restore(ss);
					return Error_Type;
				}
			}
			else if (op.value.symbol == sym_fn.value.symbol) {
				if (nilp(args) || nilp(cdr(args))) {
					stack_restore(ss);
					return Error_Args;
				}
				err = make_closure(env, car(args), cdr(args), result);
				stack_restore(ss);
				stack_add(*result);
				return err;
			}
			else if (op.value.symbol == sym_if.value.symbol) {
				Atom cond, val;

				if (nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args)))
					|| !nilp(cdr(cdr(cdr(args))))) {
					stack_restore(ss);
					return Error_Args;
				}

				err = eval_expr(car(args), env, &cond);
				if (err) {
					stack_restore(ss);
					stack_add(*result);
					return err;
				}

				val = nilp(cond) ? car(cdr(cdr(args))) : car(cdr(args));
				err = eval_expr(val, env, result);
				stack_restore(ss);
				stack_add(*result);
				return err;
			}
			else if (op.value.symbol == sym_mac.value.symbol) { /* (mac name (arg ...) body) */
				Atom name, macro;

				if (nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args)))) {
					stack_restore(ss);
					return Error_Args;
				}

				name = car(args);
				if (name.type != AtomType_Symbol) {
					stack_restore(ss);
					return Error_Type;
				}

				err = make_closure(env, car(cdr(args)), cdr(cdr(args)), &macro);
				if (!err) {
					macro.type = AtomType_Macro;
					*result = name;
					err = env_set(env, name, macro);
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
				Atom pred;
				if (nilp(args)) {
					stack_restore(ss);
					return Error_Args;
				}
				pred = car(args);
				while (eval_expr(pred, env, result), !nilp(*result)) {
					Atom e = cdr(args);
					while (!nilp(e)) {
						eval_expr(car(e), env, result);
						e = cdr(e);
					}
				}
				stack_restore(ss);
				stack_add(*result);
				return Error_OK;
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
		if (op.type == AtomType_Macro) {
		  Atom expansion;
		  op.type = AtomType_Closure;
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
		Atom p = args;
		while (!nilp(p)) {
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

int main(int argc, char **argv)
{
	printf("Arcadia %s\n", VERSION);	
	Atom env;
	char *input;

	env = env_create(nil);

	/* Set up the initial environment */
	sym_t = make_sym("t");
	sym_quote = make_sym("quote");
	sym_set = make_sym("set");
	sym_fn = make_sym("fn");
	sym_if = make_sym("if");
	sym_mac = make_sym("mac");
	sym_apply = make_sym("apply");
	sym_while = make_sym("while");

	env_set(env, make_sym("car"), make_builtin(builtin_car));
	env_set(env, make_sym("cdr"), make_builtin(builtin_cdr));
	env_set(env, make_sym("cons"), make_builtin(builtin_cons));
	env_set(env, make_sym("+"), make_builtin(builtin_add));
	env_set(env, make_sym("-"), make_builtin(builtin_subtract));
	env_set(env, make_sym("*"), make_builtin(builtin_multiply));
	env_set(env, make_sym("/"), make_builtin(builtin_divide));
	env_set(env, sym_t, sym_t);
	env_set(env, make_sym("<"), make_builtin(builtin_less));
	env_set(env, make_sym("apply"), make_builtin(builtin_apply));
	env_set(env, make_sym("is"), make_builtin(builtin_is));
	env_set(env, make_sym("pair?"), make_builtin(builtin_pairp));
	env_set(env, make_sym("no"), make_builtin(builtin_no));
	env_set(env, make_sym("scar"), make_builtin(builtin_scar));
	env_set(env, make_sym("scdr"), make_builtin(builtin_scdr));
	env_set(env, make_sym("mod"), make_builtin(builtin_mod));

	if (!load_file(env, "library.arc")) {
		load_file(env, "../library.arc");
	}

	/* print the environment */
	puts("Environment:");
	Atom a = cdr(env);
	while (!nilp(a)) {
		Atom env_pair = car(a);
		printf(" %s", car(env_pair).value.symbol);
		a = cdr(a);
	}
	puts("");

	while ((input = readline("> ")) != NULL) {
		char *buf = (char *)malloc(strlen(input) + 3);
		sprintf(buf, "(%s)", input);
		const char *p = buf;
		Error err;
		Atom result;

		err = read_expr(p, &p, &code_expr);

		while (!nilp(code_expr)) {
			if (!err)
				err = preprocess_eval(car(code_expr), env, &result);
			if (err)
				puts(error_string[err]);
			else {
				print_expr(result);
				putchar('\n');
			}
			code_expr = cdr(code_expr);
			/*gc_mark(code_expr);
			gc_mark(env);
			gc();*/
		}		
		free(buf);
		free(input);
	}

	return 0;
}
