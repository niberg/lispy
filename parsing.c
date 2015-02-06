#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpc.h"

#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { \
    lval* err = lval_err(fmt, ##__VA_ARGS__); \
    lval_del(args); \
    return err; \
  }

#define LASSERT_TYPE(func, args, index, expect) \
  LASSERT(args, args->cell[index]->type == expect, \
      "Function '%s' passed incorrect type for argument %i. " \
      "Got %s, expected %s.", \
      func, index, ltype_name(args->cell[index]->type), ltype_name(expect))

#define LASSERT_NUM(func, args, num) \
  LASSERT(args, args->count == num, \
      "Function '%s' passed incorrect number of arguments. " \
      "Got %i, expected %i.", \
      func, args->count, num)

#define LASSERT_NOT_EMPTY(func, args, index) \
  LASSERT(args, args->cell[index]->count != 0, \
      "Function '%s' passed {} for argument %i. ", func, index);

/* If we are compiling on Windows compile these functions */
#ifdef _WIN32
#include <string.h>

static char buffer[2048];

/* Fake readline function */
char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

/* Fake add_history function */
void add_history(char* unused) {}

/* Otherwise include the editline headers */
#else
#include <editline/readline.h>
#include <editline/history.h>
#endif

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;
typedef lval*(*lbuiltin)(lenv*, lval*);

typedef union val {
  long num;
  double doub;
} val;

struct lval {
  int type;
  /* Error and Symbol types have some string data */
  val* u;
  char* err;
  char* sym;
  /* Function */
  lbuiltin builtin;
  lenv* env;
  lval* formals;
  lval* body;
  /* Expression */
  int count;
  struct lval** cell;
};

struct lenv {
  lenv* par;
  int count;
  char** syms;
  lval** vals;
};

/* Create Enumeration of Possible lval Types */
enum { LVAL_ERR, LVAL_DOUBLE, LVAL_NUM, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR, LVAL_FUN };

/* Create Enumeration of Possible Error Types */
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };


lval* lval_take(lval* v, int i);
void lval_print(lval* v);
lval* lval_pop(lval* v, int i);
lval* builtin_op(lenv* e, lval* a, char* op);
lval* builtin(lenv* e, lval* a, char* func);
lval* lval_join(lval* x, lval* y);
void lval_del(lval* v);
lval* lval_copy(lval* v);
lval* lval_err(char* fmt, ...);
lval* lval_eval(lenv* e, lval* v);
char* ltype_name(int t);
lval* lval_eval_sexpr(lenv* e, lval* v);
lval* lval_call(lenv* e, lval* f, lval* a);
lval* builtin_eval(lenv* e, lval* a);
lval* builtin_var(lenv* e, lval* a, char* func);
lval* builtin_list(lenv* e, lval* a);

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lenv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void lenv_del(lenv* e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

lval* lenv_get(lenv* e, lval* k) {
  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      return lval_copy(e->vals[i]);
    }
  }

  if (e->par) {
    return lenv_get(e->par, k);
  } else {
    return lval_err("Unbound symbol '%s'!", k->sym);
  }
}

void lenv_put(lenv* e, lval* k, lval* v) {
  for (int i = 0;  i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      lval_del(e->vals[i]);
      e->vals[i] = lval_copy(v);
      return;
    }
  }
  // If no existing entry
  e->count++;
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  e->vals[e->count-1] = lval_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym) + 1);
  strcpy(e->syms[e->count-1], k->sym);
}

lenv* lenv_copy(lenv* e) {
  lenv* n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i]) + 1);
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

void lenv_def(lenv* e, lval* k, lval* v) {
  /* Iterate until e has no parent */
  while (e->par) {
    e = e->par;
  }
  /* Put value in e */
  lenv_put(e, k, v);
}

lval* lval_fun(lbuiltin func) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = func;
  return v;
}

lval* lval_lambda(lval* formals, lval* body) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  /* Set builtin to Null */
  v->builtin = NULL;
  /* Build new environment */
  v->env = lenv_new();
  /* Set formals and body */
  v->formals = formals;
  v->body = body;
  return v;
}

/* Construct a pointer to a new Number lval */ 
lval* lval_num(long x) {
  lval* v = malloc(sizeof(lval));
  v->u = malloc(sizeof(val));
  v->type = LVAL_NUM;
  v->u->num = x;
  return v;
}

/* Construct a pointer to a new Double lval */ 
lval* lval_doub(double x) {
  lval* v = malloc(sizeof(lval));
  v->u = malloc(sizeof(val));
  v->type = LVAL_DOUBLE;
  v->u->doub = x;
  return v;
}

/* Construct a pointer to a new Error lval */ 
lval* lval_err(char* fmt, ...) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  /* Create a va list and initialize it */
  va_list va;
  va_start(va, fmt);

  v->err = malloc(512);

  /* Printf the error string with a maximum of 511 characters */
  vsnprintf(v->err, 511, fmt, va);

  /* Reallocate to number of bytes actually used */
  v->err = realloc(v->err, strlen(v->err)+1);

  /* Clean up the va list */
  va_end(va);
  return v;
}

/* Construct a pointer to a new Symbol lval */ 
lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

/* A pointer to a new empty Sexpr lval */
lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

/* A pointer to a new empty Qexpr lval */
lval* lval_qexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_del(lval* v) {

  switch (v->type) {
    /* Do nothing special for number type */
    case LVAL_FUN:
      if (!v->builtin) {
        lenv_del(v->env);
        lval_del(v->formals);
        lval_del(v->body);
      }
      break;
    case LVAL_NUM:
    case LVAL_DOUBLE:
      free(v->u);
      break;

      /* For Err or Sym free the string data */
    case LVAL_ERR: free(v->err); break;
    case LVAL_SYM: free(v->sym); break;

                   /* If Sexpr or Qexpr then delete all elements inside */
    case LVAL_QEXPR:
    case LVAL_SEXPR:
                   for (int i = 0; i < v->count; i++) {
                     lval_del(v->cell[i]);
                   }
                   /* Also free the memory allocated to contain the pointers */
                   free(v->cell);
                   break;
  }

  /* Free the memory allocated for the "lval" struct itself */
  free(v);
}

lval* lval_read_num(mpc_ast_t* t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ?
    lval_num(x) : lval_err("invalid number");
}

lval* lval_read_doub(mpc_ast_t* t) {
  errno = 0;
  double x = strtod(t->contents, NULL);
  return errno != ERANGE ?
    lval_doub(x) : lval_err("invalid number");
}

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval* lval_read(mpc_ast_t* t) {

  /* If Symbol or Number return conversion to that type */
  if (strstr(t->tag, "double")) { return lval_read_doub(t); }
  if (strstr(t->tag, "number")) { return lval_read_num(t); }
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }

  /* If root (>) or sexpr then create empty list */
  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0) { x = lval_sexpr(); } 
  if (strstr(t->tag, "sexpr"))  { x = lval_sexpr(); }
  if (strstr(t->tag, "qexpr"))  { x = lval_qexpr(); }

  /* Fill this list with any valid expression contained within */
  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
    x = lval_add(x, lval_read(t->children[i]));
  }

  return x;
}


void lval_expr_print(lval* v, char open, char close) {
  if (v->count == 0) {
    puts("ok");
    return;
  }

  putchar(open);
  for (int i = 0; i < v->count; i++) {

    /* Print Value contained within */
    lval_print(v->cell[i]);

    /* Don't print trailing space if last element */
    if (i != (v->count-1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void lval_print(lval* v) {
  switch (v->type) {
    case LVAL_DOUBLE:  printf("%g", v->u->doub); break;
    case LVAL_NUM:     printf("%li", v->u->num); break;
    case LVAL_ERR:     printf("Error: %s", v->err); break;
    case LVAL_SYM:     printf("%s", v->sym); break;
    case LVAL_SEXPR:   lval_expr_print(v, '(', ')'); break;
    case LVAL_QEXPR:   lval_expr_print(v, '{', '}'); break;
    case LVAL_FUN:
      if (v->builtin) {
        puts("<builtin>");
      } else {
        printf("(\\ ");
        lval_print(v->formals);
        putchar(' ');
        lval_print(v->body);
        putchar(')');
      }
      break;
  }
}


void lval_println(lval* v) { lval_print(v); putchar('\n'); }

lval* lval_eval_sexpr(lenv* e, lval* v) {

  /* Evaluate Children */
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }

  /* Error Checking */
  for (int i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
  }

  /* Empty Expression */
  if (v->count == 0) { return v; }

  /* Single Expression */
  if (v->count == 1) { return lval_take(v, 0); }

  /* Ensure First Element is a function after evaluation */
  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_FUN) {
    lval* err = lval_err(
        "S-Expression starts with incorrect type. "
        "Got %s, expected %s.",
        ltype_name(f->type), ltype_name(LVAL_FUN));
    lval_del(f); lval_del(v);
    return err;
  }

  /* Call builtin with operator */
  lval* result = lval_call(e, f, v);
  lval_del(f);
  return result;
}

lval* lval_eval(lenv* e, lval* v) {
  if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e, v);
    lval_del(v);
    return x;
  }
  /* Evaluate Sexpressions */
  if (v->type == LVAL_SEXPR) { return lval_eval_sexpr(e, v); }
  /* All other lval types remain the same */
  return v;
}

lval* lval_pop(lval* v, int i) {
  /* Find the item at "i" */
  lval* x = v->cell[i];

  /* Shift memory after the item at "i" over the top */
  memmove(&v->cell[i], &v->cell[i+1],
      sizeof(lval*) * (v->count-i-1));

  /* Decrease the count of items in the list */
  v->count--;

  /* Reallocate the memory used */
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  return x;
}

lval* lval_take(lval* v, int i) {
  lval* x = lval_pop(v, i);
  lval_del(v);
  return x;
}

lval* lval_copy(lval* v) {
  lval* x = malloc(sizeof(lval));
  x->type = v->type;

  switch (v->type) {

    // Copy functions and numbers directly
    case LVAL_FUN:
      if (v->builtin) {
        x->builtin = v->builtin;
      } else {
        x->builtin = NULL;
        x->env = lenv_copy(v->env);
        x->formals = lval_copy(v->formals);
        x->body = lval_copy(v->body);
      }
      break;
                   //Possibly need to malloc for union
    case LVAL_NUM:
                   x->u = malloc(sizeof(val));
                   x->u->num = v->u->num;
                   break;

    case LVAL_DOUBLE:
                   x->u = malloc(sizeof(val));
                   x->u->doub = v->u->doub;
                   break;

                   // Copy strings using malloc and strcpy
    case LVAL_ERR:
                   x->err = malloc(strlen(v->err) + 1);
                   strcpy(x->err, v->err); break;

    case LVAL_SYM:
                   x->sym = malloc(strlen(v->sym) + 1);
                   strcpy(x->sym, v->sym); break;

                   // Copy lists by copying each subexpression
    case LVAL_SEXPR:
    case LVAL_QEXPR:
                   x->count = v->count;
                   x->cell = malloc(sizeof(lval*) * x->count);
                   for (int i = 0; i < x->count; i++) {
                     x->cell[i] = lval_copy(v->cell[i]);
                   }
                   break;
  }

  return x;
}

lval* lval_call(lenv* e, lval* f, lval* a) {
  
  /* If builtin, apply it */
  if (f->builtin)
    return f->builtin(e, a);

  /* Record arguments */
  int given = a->count;
  int total = f->formals->count;

  while (a->count) {
    
    if (f->formals->count == 0) {
      lval_del(a);
      return lval_err(
          "Function passed too many arguments. "
          "Got %i, expected %i.", given, total);
    }

    /* Pop the first symbol from the formals */
    lval* sym = lval_pop(f->formals, 0);

    /* Special case to deal with & */
    if (strcmp(sym->sym, "&") == 0) {

      /* Ensure '&' is followed by another symbol */
      if (f->formals->count != 1) {
        lval_del(a);
        return lval_err("Function format invalid. "
            "Symbol '&' not followed by single symbol.");
      }

      /* Next formal should be bound to remaining arguments */
      lval* nsym = lval_pop(f->formals, 0);
      lenv_put(f->env, nsym, builtin_list(e, a));
      lval_del(sym);
      lval_del(nsym);
      break;
    }
    /* Pop the next argument from the list */
    lval* val = lval_pop(a, 0);

    /* Bind a copy into the function's environment */
    lenv_put(f->env, sym, val);

    /* Delete symbol and value */
    lval_del(sym);
    lval_del(val);
  }

  /* Argument list is now bound so can be cleaned up */
  lval_del(a);

  /* If '&' remains in formal list bind to empty list */
  if (f->formals->count > 0 &&
      strcmp(f->formals->cell[0]->sym, "&") == 0) {

    /* Check to ensure that '&' is not passed invalidly. */
    if (f->formals->count != 2) {
      return lval_err("Function format invalid. "
          "Symbol '&' not followed by single symbol.");
    }

    /* Pop and delete '&' symbol */
    lval_del(lval_pop(f->formals, 0));

    /* Pop next symbol and create empty list */
    lval* sym = lval_pop(f->formals, 0);
    lval* val = lval_qexpr();

    /* Bind to environment and delete */
    lenv_put(f->env, sym, val);
    lval_del(sym);
    lval_del(val);
  }
  /* If all formals have been bound, evaluate */
  if (f->formals->count == 0) {

    /* Set environment parent to evaluation environment */
    f->env->par = e;

    /* Evaluate and return */
    return builtin_eval(
        f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
  } else {
    /* Otherwise return partially evaluated function */
    return lval_copy(f);
  }
}

lval* builtin_op(lenv* e, lval* a, char* op) {

  /* Ensure all arguments are numbers or doubles */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != LVAL_NUM && a->cell[i]->type != LVAL_DOUBLE) {
      lval* err = lval_err("Function '%s' passed incorrect "
          "type for argument %i. Got %s, expected %s or %s.",
          op, i+1, ltype_name(a->cell[i]->type), ltype_name(LVAL_NUM),
          ltype_name(LVAL_DOUBLE));
      lval_del(a);
      return err;
    }

  }

  /* Pop the first element */
  lval* x = lval_pop(a, 0);

  /*printf("Read x %d\n", x->type);*/
  /* If no arguments and sub then perform unary negation */
  if ((strcmp(op, "-") == 0) && a->count == 0) {
    if (x->type == LVAL_NUM)
      x->u->num = -x->u->num;
    else
      x->u->doub = -x->u->doub;
  }

  /* While there are still elements remaining */
  while (a->count > 0) {

    /* Pop the next element */
    lval* y = lval_pop(a, 0);

    /*printf("Read y %d\n", y->type);*/
    if (y->type == LVAL_NUM && x->type == LVAL_NUM) {
      if (strcmp(op, "+") == 0) { x->u->num += y->u->num; }
      if (strcmp(op, "-") == 0) { x->u->num -= y->u->num; }
      if (strcmp(op, "*") == 0) { x->u->num *= y->u->num; }
      if (strcmp(op, "%") == 0) { x->u->num %= y->u->num; }
      if (strcmp(op, "^") == 0) { x->u->num = pow(x->u->num, y->u->num); }
      if (strcmp(op, "min") == 0) { if (x->u->num > y->u->num) x->u->num = y->u->num; }
      if (strcmp(op, "max") == 0) { if (x->u->num < y->u->num) x->u->num = y->u->num; }
      if (strcmp(op, "/") == 0) {
        if (y->u->num == 0) {
          lval_del(x); lval_del(y);
          x = lval_err("Division By Zero!"); break;
        }
        x->u->num /= y->u->num;
      }
    } else if (y->type == LVAL_DOUBLE && x->type == LVAL_DOUBLE) {
      if (strcmp(op, "+") == 0) { x->u->doub += y->u->doub; }
      if (strcmp(op, "-") == 0) { x->u->doub -= y->u->doub; }
      if (strcmp(op, "*") == 0) { x->u->doub *= y->u->doub; }
      if (strcmp(op, "%") == 0) { x->u->doub = fmod(x->u->doub, y->u->doub); }
      if (strcmp(op, "^") == 0) { x->u->doub = pow(x->u->doub, y->u->doub); }
      if (strcmp(op, "min") == 0) { if (x->u->doub > y->u->doub) x->u->doub = y->u->doub; }
      if (strcmp(op, "max") == 0) { if (x->u->doub < y->u->doub) x->u->doub = y->u->doub; }
      if (strcmp(op, "/") == 0) {
        if (y->u->doub == 0) {
          lval_del(x); lval_del(y);
          x = lval_err("Division By Zero!"); break;
        }
        x->u->doub /= y->u->doub;
      }
    } else if (y->type == LVAL_NUM && x->type == LVAL_DOUBLE) {
      if (strcmp(op, "+") == 0) { x->u->doub += (double)y->u->num; }
      if (strcmp(op, "-") == 0) { x->u->doub -= (double)y->u->num; }
      if (strcmp(op, "*") == 0) { x->u->doub *= (double)y->u->num; }
      if (strcmp(op, "%") == 0) { x->u->doub = fmod(x->u->doub, (double)y->u->num); }
      if (strcmp(op, "^") == 0) { x->u->doub = pow(x->u->doub, (double)y->u->num); }
      if (strcmp(op, "min") == 0) { if (x->u->doub > (double) y->u->num) x->u->doub = (double) y->u->num; }
      if (strcmp(op, "max") == 0) { if (x->u->doub < (double) y->u->num) x->u->doub = (double) y->u->num; }
      if (strcmp(op, "/") == 0) {
        if (y->u->num == 0) {
          lval_del(x); lval_del(y);
          x = lval_err("Division By Zero!"); break;
        }
        x->u->doub /= (double)y->u->num;
      }
    } else if (y->type == LVAL_DOUBLE && x->type == LVAL_NUM) {
      //Set type to double
      x->type = LVAL_DOUBLE;
      if (strcmp(op, "+") == 0) { x->u->doub = (double) x->u->num + y->u->doub; }
      if (strcmp(op, "-") == 0) { x->u->doub = (double) x->u->num - y->u->doub; }
      if (strcmp(op, "*") == 0) { x->u->doub = (double) x->u->num * y->u->doub; }
      if (strcmp(op, "%") == 0) { x->u->doub = fmod((double) x->u->num, y->u->doub); }
      if (strcmp(op, "^") == 0) { x->u->doub = pow((double) x->u->num, y->u->doub); }
      if (strcmp(op, "min") == 0) { if ((double) x->u->num > y->u->doub) x->u->doub = y->u->doub; else x->u->doub = (double) x->u->num; }
      if (strcmp(op, "max") == 0) { if ((double) x->u->num < y->u->doub) x->u->doub = y->u->doub; else x->u->doub = (double) x->u->num; }
      if (strcmp(op, "/") == 0) {
        if (y->u->doub == 0) {
          lval_del(x); lval_del(y);
          x = lval_err("Division By Zero!"); break;
        }
        x->u->doub = (double) x->u->num / y->u->doub;
      }
    }

    lval_del(y);
  }

  lval_del(a); return x;
}

lval* builtin_add(lenv* e, lval* a) {
  return builtin_op(e, a, "+");
}

lval* builtin_sub(lenv* e, lval* a) {
  return builtin_op(e, a, "-");
}

lval* builtin_mul(lenv* e, lval* a) {
  return builtin_op(e, a, "*");
}

lval* builtin_div(lenv* e, lval* a) {
  return builtin_op(e, a, "/");
}

lval* builtin_pow(lenv* e, lval* a) {
  return builtin_op(e, a, "^");
}

lval* builtin_mod(lenv* e, lval* a) {
  return builtin_op(e, a, "%");
}

lval* builtin_max(lenv* e, lval* a) {
  return builtin_op(e, a, "max");
}

lval* builtin_min(lenv* e, lval* a) {
  return builtin_op(e, a, "min");
}

lval* builtin_len(lenv* e, lval* a) {
  LASSERT_NUM("len", a, 1);
  LASSERT_TYPE("len", a, 0, LVAL_QEXPR);
  lval* v = lval_num(a->cell[0]->count);
  lval_del(a);
  return v;

}

lval* builtin_init(lenv* e, lval* a) {
  LASSERT_NUM("init", a, 1);
  LASSERT_TYPE("init", a, 0, LVAL_QEXPR);
  LASSERT_NOT_EMPTY("init", a, 0);
  int i = a->cell[0]->count;
  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, i-1));
  return v;
}


lval* builtin_head(lenv* e, lval* a) {
  LASSERT_NUM("head", a, 1);
  LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
  LASSERT_NOT_EMPTY("head", a, 0);

  lval* v = lval_take(a, 0);  
  while (v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  LASSERT_NUM("tail", a, 1);
  LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);
  LASSERT_NOT_EMPTY("tail", a, 0);

  lval* v = lval_take(a, 0);  
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_list(lenv* e, lval* a) {
  a->type = LVAL_QEXPR;
  return a;
}

lval* builtin_eval(lenv* e, lval* a) {
  LASSERT_NUM("eval", a, 1);
  LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);

  lval* x = lval_take(a, 0);
  x->type = LVAL_SEXPR;
  return lval_eval(e, x);
}

lval* builtin_join(lenv* e, lval* a) {

  for (int i = 0; i < a->count; i++) {
    LASSERT_TYPE("join", a, i, LVAL_QEXPR);
  }

  lval* x = lval_pop(a, 0);

  while (a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }

  lval_del(a);
  return x;
}

lval* builtin_put(lenv* e, lval* a) {
  return builtin_var(e, a, "=");
}

lval* builtin_def(lenv* e, lval* a) {
  return builtin_var(e, a, "def");
}

lval* builtin_var(lenv* e, lval* a, char* func) {
  LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

  lval* syms = a->cell[0];

  for (int i = 0; i < syms->count; i++) {
    LASSERT(a, syms->cell[i]->type == LVAL_SYM,
        "Function '%s' cannot define non-symbol! "
        "Got %s, expected %s. ", func,
        ltype_name(syms->cell[i]->type),
        ltype_name(LVAL_SYM));
  }

  LASSERT(a, syms->count == a->count-1,
      "Function '%s' passed too many arguments for symbols. "
      "Got %i, expected %i.", func, syms->count, a->count-1);

  //Assign copies of values to symbols
  for (int i = 0; i < syms->count; i++) {
    /* If 'def' define in global env, else local */
    if (strcmp(func, "def") == 0) {
      lenv_def(e, syms->cell[i], a->cell[i+1]);
    }
    if (strcmp(func, "=") == 0) {
      lenv_put(e, syms->cell[i], a->cell[i+1]);
    }
  }

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_lambda(lenv* e, lval* a) {
  /* Check two args, each of which are q-expressions */
  LASSERT_NUM("\\", a, 2);
  LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
  LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

  /* Assert first q-expr contains only symbols */
  for (int i = 0; i < a->cell[0]->count; i++) {
    LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
        "Cannot define non-symbol. Got %s, expected %s.",
        ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
  }

  /* Pop first two arguments and pass them to lval_lambda */
  lval* formals = lval_pop(a, 0);
  lval* body = lval_pop(a, 0);
  lval_del(a);

  return lval_lambda(formals, body);
}

int is_numerical(lval* a) {
  return (a->type == LVAL_NUM || a->type == LVAL_DOUBLE);
}

int lval_eq(lval* x, lval* y) {
  /* Different types are always unequal */
  if (!is_numerical(x) && !is_numerical(y))
    if (x->type != y->type)
      return 0;

  switch (x->type) {
    
    case LVAL_NUM:
      if (y->type == LVAL_NUM)
        return (x->u->num == y->u->num);
      else if (y->type == LVAL_DOUBLE)
        return ((double) x->u->num == y->u->doub);
    case LVAL_DOUBLE:
      if (y->type == LVAL_DOUBLE)
        return (x->u->doub == y->u->doub);
      else if (y->type == LVAL_NUM)
        return (x->u->doub == (double) y->u->num);
    case LVAL_ERR: return (strcmp(x->err, y->err) == 0);
    case LVAL_SYM: return (strcmp(x->sym, y->sym) == 0);

    case LVAL_FUN:
      if (x->builtin || y->builtin) {
        return x->builtin == y->builtin;
      } else {
        return lval_eq(x->formals, y->formals)
          && lval_eq(x->body, y->body);
      }

    case LVAL_QEXPR:
    case LVAL_SEXPR:
      if (x->count != y->count)
        return 0;
      for (int i = 0; i < x->count; i++) {
        if (!lval_eq(x->cell[i], y->cell[i]))
          return 0;
      }
      return 1;
    break;
  }
  return 0;
}

lval* builtin_ord(lenv* e, lval* a, char* op) {
  LASSERT_NUM(op, a, 2);
  if (a->cell[0]->type != LVAL_NUM && a->cell[0]->type != LVAL_DOUBLE) {
    lval* err = lval_err("Function '%s' received incorrect parameters "
        "Got %s, expected %s or %s.", op, ltype_name(a->cell[0]->type),
        ltype_name(LVAL_NUM), ltype_name(LVAL_DOUBLE));
    lval_del(a);
    return err;
  }
  if (a->cell[1]->type != LVAL_NUM && a->cell[1]->type != LVAL_DOUBLE) {
    lval* err = lval_err("Function '%s' received incorrect parameters "
        "Got %s, expected %s or %s.", op, ltype_name(a->cell[1]->type),
        ltype_name(LVAL_NUM), ltype_name(LVAL_DOUBLE));
    lval_del(a);
    return err;
  }

  int r;
  if (a->cell[0]->type == LVAL_NUM && a->cell[1]->type == LVAL_NUM) {
    if (strcmp(op, ">") == 0) {
      r = (a->cell[0]->u->num > a->cell[1]->u->num);
    }
    if (strcmp(op, "<") == 0) {
      r = (a->cell[0]->u->num < a->cell[1]->u->num);
    }
    if (strcmp(op, ">=") == 0) {
      r = (a->cell[0]->u->num >= a->cell[1]->u->num);
    }
    if (strcmp(op, "<=") == 0) {
      r = (a->cell[0]->u->num <= a->cell[1]->u->num);
    }
    if (strcmp(op, "&&") == 0) {
      r = (a->cell[0]->u->num && a->cell[1]->u->num);
    }
    if (strcmp(op, "||") == 0) {
      r = (a->cell[0]->u->num || a->cell[1]->u->num);
    }
  } else if (a->cell[0]->type == LVAL_NUM && a->cell[1]->type == LVAL_DOUBLE) {
    if (strcmp(op, ">") == 0) {
      r = ((double) a->cell[0]->u->num >  a->cell[1]->u->doub);
    }
    if (strcmp(op, "<") == 0) {
      r = ((double) a->cell[0]->u->num <  a->cell[1]->u->doub);
    }
    if (strcmp(op, ">=") == 0) {
      r = ((double) a->cell[0]->u->num >=  a->cell[1]->u->doub);
    }
    if (strcmp(op, "<=") == 0) {
      r = ((double) a->cell[0]->u->num <=  a->cell[1]->u->doub);
    }
    if (strcmp(op, "&&") == 0) {
      r = (a->cell[0]->u->num && a->cell[1]->u->doub);
    }
    if (strcmp(op, "||") == 0) {
      r = (a->cell[0]->u->num || a->cell[1]->u->doub);
    }
  } else if (a->cell[0]->type == LVAL_DOUBLE && a->cell[1]->type == LVAL_NUM) {
    if (strcmp(op, ">") == 0) {
      r = (a->cell[0]->u->doub > (double) a->cell[1]->u->num);
    }
    if (strcmp(op, "<") == 0) {
      r = (a->cell[0]->u->doub < (double) a->cell[1]->u->num);
    }
    if (strcmp(op, ">=") == 0) {
      r = (a->cell[0]->u->doub >= (double) a->cell[1]->u->num);
    }
    if (strcmp(op, "<=") == 0) {
      r = (a->cell[0]->u->doub <= (double) a->cell[1]->u->num);
    }
    if (strcmp(op, "&&") == 0) {
      r = (a->cell[0]->u->doub && (double) a->cell[1]->u->num);
    }
    if (strcmp(op, "||") == 0) {
      r = (a->cell[0]->u->doub || (double) a->cell[1]->u->num);
    }
  } else if (a->cell[0]->type == LVAL_DOUBLE && a->cell[1]->type == LVAL_DOUBLE) {
    if (strcmp(op, ">") == 0) {
      r = (a->cell[0]->u->doub >  a->cell[1]->u->doub);
    }
    if (strcmp(op, "<") == 0) {
      r = (a->cell[0]->u->doub <  a->cell[1]->u->doub);
    }
    if (strcmp(op, ">=") == 0) {
      r = (a->cell[0]->u->doub >=  a->cell[1]->u->doub);
    }
    if (strcmp(op, "<=") == 0) {
      r = (a->cell[0]->u->doub <=  a->cell[1]->u->doub);
    }
    if (strcmp(op, "&&") == 0) {
      r = (a->cell[0]->u->doub &&  a->cell[1]->u->doub);
    }
    if (strcmp(op, "||") == 0) {
      r = (a->cell[0]->u->doub ||  a->cell[1]->u->doub);
    }
  }
  lval_del(a);
  return lval_num(r);
}

lval* builtin_gt(lenv* e, lval* a) {
  return builtin_ord(e, a, ">");
}

lval* builtin_lt(lenv* e, lval* a) {
  return builtin_ord(e, a, "<");
}

lval* builtin_ge(lenv* e, lval* a) {
  return builtin_ord(e, a, ">=");
}

lval* builtin_le(lenv* e, lval* a) {
  return builtin_ord(e, a, "<=");
}

lval* builtin_and(lenv* e, lval* a) {
  return builtin_ord(e, a, "&&");
}

lval* builtin_or(lenv* e, lval* a) {
  return builtin_ord(e, a, "||");
}

lval* builtin_cmp(lenv* e, lval* a, char* op) {
  LASSERT_NUM(op, a, 2);
  int r;
  
  if (strcmp(op, "==") == 0) {
    r = lval_eq(a->cell[0], a->cell[1]);
  }
  if (strcmp(op, "!=") == 0) {
    r = !lval_eq(a->cell[0], a->cell[1]);  
  }
  lval_del(a);
  return lval_num(r);
}

lval* builtin_eq(lenv* e, lval* a) {
  return builtin_cmp(e, a, "==");
}

lval* builtin_ne(lenv* e, lval* a) {
  return builtin_cmp(e, a, "!=");
}

lval* builtin_if(lenv* e, lval* a) {
  LASSERT_NUM("if", a, 3);
  LASSERT_TYPE("if", a, 0, LVAL_NUM);
  LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
  LASSERT_TYPE("if", a, 2, LVAL_QEXPR);

  /* Mark both expressions as evaluable */
  lval* x;
  a->cell[1]->type = LVAL_SEXPR;
  a->cell[2]->type = LVAL_SEXPR;

  if (a->cell[0]->u->num) {
    x = lval_eval(e, lval_pop(a, 1));
  } else {
    x = lval_eval(e, lval_pop(a, 2));
  }

  lval_del(a);
  return x;
}

lval* builtin_not(lenv* e, lval* a) {
  LASSERT_NUM("!", a, 1);
  LASSERT_TYPE("!", a, 0, LVAL_NUM);
  int r = !(a->cell[0]->u->num);
  lval_del(a);
  return lval_num(r);
}

lval* lval_join(lval* x, lval* y) {

  /* For each cell in 'y' add it to 'x' */
  while (y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }

  /* Delete the empty 'y' and return 'x' */
  lval_del(y);  
  return x;
}

char* ltype_name(int t) {
  switch(t) {
    case LVAL_FUN: return "Function";
    case LVAL_NUM: return "Number";
    case LVAL_DOUBLE: return "Double";
    case LVAL_ERR: return "Error";
    case LVAL_SYM: return "Symbol";
    case LVAL_SEXPR: return "S-Expression";
    case LVAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
  }
}


void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(func);
  lenv_put(e, k, v);
  lval_del(k);
  lval_del(v);
}

void lenv_add_builtins(lenv* e) {
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "len", builtin_len);
  lenv_add_builtin(e, "init", builtin_init);

  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "=", builtin_put);
  lenv_add_builtin(e, "\\", builtin_lambda);

  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
  lenv_add_builtin(e, "^", builtin_pow);
  lenv_add_builtin(e, "%", builtin_mod);
  lenv_add_builtin(e, "max", builtin_max);
  lenv_add_builtin(e, "min", builtin_min);

  lenv_add_builtin(e, "if", builtin_if);
  lenv_add_builtin(e, "==", builtin_eq);
  lenv_add_builtin(e, "!=", builtin_ne);
  lenv_add_builtin(e, ">",  builtin_gt);
  lenv_add_builtin(e, "<",  builtin_lt);
  lenv_add_builtin(e, ">=", builtin_ge);
  lenv_add_builtin(e, "<=", builtin_le);
  lenv_add_builtin(e, "!", builtin_not);
  lenv_add_builtin(e, "&&", builtin_and);
  lenv_add_builtin(e, "||", builtin_or);
}
int main(int argc, char** argv) {
  mpc_parser_t* Double = mpc_new("double");
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr  = mpc_new("sexpr");
  mpc_parser_t* Qexpr  = mpc_new("qexpr");
  mpc_parser_t* Expr   = mpc_new("expr");
  mpc_parser_t* Lispy  = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
      "                                            \
      double : /-?[0-9]*[\.]{1}[0-9]+/;          \
      number : /-?[\.]{0}[0-9]+[\.]{0}/;         \
      symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&^%|]+/ ;         \
      sexpr  : '(' <expr>* ')' ;                 \
      qexpr  : '{' <expr>* '}' ;                 \
      expr   : <double> | <number> | <symbol> | <sexpr> | <qexpr> ; \
      lispy  : /^/ <expr>* /$/ ;               \
      ",
      Double, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  puts("Lispy Version 0.0.0.0.9");
  puts("Press Ctrl+c to Exit\n");

  lenv* e = lenv_new();
  lenv_add_builtins(e);

  while (1) {

    /* Now in either case readline will be correctly defined */
    char* input = readline("lispy> ");
    add_history(input);

    /* Attempt to Parse the user Input */
    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval* x = lval_eval(e, lval_read(r.output));
      lval_println(x);
      lval_del(x);
      mpc_ast_delete(r.output);
    } else {
      /* Otherwise Print the Error */
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
    free(input);

  }

  lenv_del(e);
  /* Undefine and Delete our Parsers */
  mpc_cleanup(7, Double, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

  return 0;
}

