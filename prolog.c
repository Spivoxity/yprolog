/* Output from p2c, the Pascal-to-C translator */
/* From input file "prolog.p" */


#include <p2c/p2c.h>


/* ...

PORTABLE PROLOG INTERPRETER

Version = 'Portable Prolog Release 2.1(j).  Wed Jul 22 16:41:23 PDT 1987.';

J.M. Spivey,
Software Technology Research Centre,
Department of Computer Science,
University of York,
Heslington,
York, Y01 5DD,
England.

***                                                              ***
***  Copyright (c) 1984 J.M. Spivey.                             ***
***                                                              ***
***  Distribution and use of this software is subject to a       ***
***  contract with the Department of Computer Science,           ***
***  University of York.                                         ***
***                                                              ***

This program is a portable interpreter for the predicate logic
programming language Prolog. It is written in Pascal which conforms to
the ISO standard. The following features are not used:  conformant
array schemas, procedural and functional parameters, set types whose
base type has cardinality larger then 18. Three non-standard procedure
bodies must be supplied for each machine: these open and close named
files, and cannot be written portably.

The structure of the program is as follows:

      [1]   File Handling.
      [2]   Error Reporting.
      [3]   Trail.
      [4]   Stack Mechanism.
      [5]   Functions on Terms.
      [6]   Atom Table.
      [7]   ReadIn.
      [8]   WriteOut.
      [9]   Database.
      [10]  Unify.
      [11]  Execute.
      [12]  Evaluable Predicates.
      [13]  Top Level.

The following changes are likely to be needed when this program is
installed on a new machine:

   (i)  The value of 'ordmaxchar' (and, exceptionally, that of
   'ordminchar') in the outermost constant declaration must be
   adjusted.

   (ii)  The assignments to the array CharClass in InitRead [7] must
   be adjusted allow for extra white-space characters such as TAB.

   (iii)  The bodies of the functions OpenFile and OpenLib and of
   the procedure CloseFile [1] must be supplied.  These depend on the
   Pascal implementation and on the underlying operating system.

... */



#define ArgLen          80


typedef Char stringarg[80];


extern boolean CanRead PP((Char *name));

extern boolean SysCmd PP((Char *name));

extern boolean GetEnv PP((Char *name, Char *value));

extern Void CatchInt PV();

extern boolean TestInt PV();


#define ProlibName      "/usr/mike/src/yprolog/prolib"


/* Return to top level. */
/* Exit. */

#define Version         "Portable Prolog Release 2.1(j).  Wed Jul 22 16:41:23 PDT 1987."

/* Max. no. of active goals. */

#define MaxFrames       1000
/* Max. no. of local variables. */
#define LocSize         2500

/* Bound for recursion on terms. */
#define MaxDepth        200
/* Size of stack in ReadIn. */
#define ReadSize        100
/* Max. nesting depth in ReadIn. */
#define ReadDepth       20
/* Max. nesting depth in WriteOut. */
#define WriteDepth      100
/* Max. list length in WriteOut. */
#define WriteLength     100
/* Max. no. of files open at once. */
#define MaxFiles        5
/* File descriptor for standard files. */
#define StandFD         0

/* Size of string buffer. */

#define StringSpace     40000L
/* Size of hash table. */

#define HashSize        100
/* Size of var. map in AddClause. */
#define MaxVars         50
/* Max. length of input line. */
#define MaxLength       200
/* Max. arity for evaluable predicates. */
#define MaxEvalArity    4

/* Max. operator precedence. */
#define MaxPrec         1200
/* Max. prec. for subterms. */
#define SubPrec         999

/* Size and layout of flag vector. */
#define FlagSize        10
#define sysmode         1
#define breaklevel      2
#define tracing         3
#define debugging       4
/* Flags 5 - 8 used by spy-points. */

/*
   The following two constants must be adjusted for different
   character sets.  Typical values are
      ASCII: (0, 127)
      EBCDIC: (0, 255).
*/
/* Smallest character ordinal. */
#define ordminchar      0
/* Largest character ordinal. */
#define ordmaxchar      127


typedef short env;


typedef enum {
  funcT, intT, varT, anonT, skelT
} nodetag;

typedef struct nodeinfo {
  nodetag tag;
  union {
    struct {
      struct atomentry *name;
      long arity;
      struct node *son;
    } U0;
    long ival;
    struct node *val;
    long offset;
  } UU;
} nodeinfo;

typedef struct node {
  /* Next among args of parent functor. */
  struct node *brother;
  /* Used to link nodes on global stack. */
  struct node *chain;
  enum {
    globalF, localF, heapF
  } field;
  /* Used with field to give lifetime. */
  long scope;
  nodeinfo info;
} node;

typedef long key;

typedef struct clause {
  /* Skeleton for head of clause. */
  node *head;
  /* First goal in skeletal body. */
  node *body;
  /* No. of local vars excluding anons. */
  long nvars;
  /* Set by 'deny' via '$zap'. */
  boolean denied;
  /* Number of activations. */
  long refcount;
  /* Saved result of Hash(head). */
  long keyval;
  /* Next clause in procedure. */
  struct clause *chain;
  /* Previous clause. */
  struct clause *backchain;
  /* Chain of nodes in skeleton. */
  node *heapchain;
} clause;

typedef enum {
  inZ, outZ
} inout;
typedef char filedesc;


typedef unsigned short strindex;

typedef struct astring {
  /* Chars are stringbuf[index + 1] ... */
  strindex index;
  /* ... to stringbuf[index + length]. */
  long length;
} astring;

typedef enum {
  fxO, fyO, xfO, yfO, xfxO, xfyO, yfxO, nonO
} optype;
typedef short prec;

typedef enum {
  normP, evalP
} predtype;

typedef enum {
  callR, cutR, readR, writeR, get0R, putR, nlR, eolnR, eofR, nameR, opR,
  abortR, atomR, integerR, varR, flagR, setflgR, isR, ltR, addclR, clenvR,
  getclR, advclR, zapR, nonspR, functorR, argR, seeR, seeingR, tellR,
  tellingR, closeR, ucodeR, haltR
} evalpred;
typedef char evalarity;


typedef struct atomentry {
  astring ident;
  long atomno;
  struct atomentry *chain;
  optype oclass;
  prec oprec;
  boolean sys;
  predtype pclass;
  union {
    clause *proc;
    struct {
      evalpred routine;
      evalarity arity;
    } U1;
  } UU;
} atomentry;

typedef struct trailentry {
  node *boundvar;
  struct trailentry *chain;
} trailentry;

typedef enum {
  arityE, argsE, assertE, atomspaceE, badcddE, badcharE, badcommaE, baddotE,
  badexpE, badketE, badvbarE, commentE, depthE, divideE, eofE, framespaceE,
  localspaceE, needopE, needquoteE, needrandE, precE, readstackE, sysprocE,
  weirdchE, nvarsE, iodirE, filespaceE, nofileE, varexpE
} error;

typedef enum {
  goalD, provedD
} tracemessage;

typedef enum {
  smallC, largeC, digitC, specialC, quoteC, stropC, lparC, rparC, braC, ketC,
  lcurlyC, rcurlyC, commaC, cutC, semiC, vbarC, spaceC, weirdC
} class_;


typedef struct _REC_filetable {
  /* File name: nil if entry unused. */
  atomentry *fname;
  /* The file itself. */
  FILE *filep;
  /* Whether for input or output. */
  inout direction;
} _REC_filetable;

typedef struct _REC_display {
  /* Invoking goal. */
  node *Fcall;
  /* Environment for the goal */
  env Fenv;
  /* Choicepoint at activation. */
  env Fchoice;
  /* Active clause. */
  clause *Fclause;
  /* Head of trail at activation. */
  trailentry *Ftrail;
  /* Top of global stack at activation. */
  node *Fglotop;
  /* Base of frame in locstack. */
  short Fbase;
} _REC_display;


Static filedesc current[2];   /* Current input and output files. */
Static _REC_filetable filetable[MaxFiles];   /* Table of open files. */
Static Char linebuf[MaxLength];   /* Buffer for reflection of input line. */
/* True when about to start new line. */
Static boolean linefinished;
/* Position in current input line. */
Static uchar charpos;
/* Position of error marker. */
Static uchar errpos;

Static Char stringbuf[StringSpace];   /* Space to store atom names. */
/* Top of permanent entries. */
Static strindex atomhwm;
/* Temporary atom name under construction. */
Static astring newatom;
/* Number of distinct atoms. */
Static long atomcount;
Static atomentry *hashtable[HashSize];   /* Hash table for atom names. */

/* Various constant atoms. */
Static atomentry *commaA, *nilA, *consA, *cutA, *semiA, *questionA, *arrowA,
		 *fxA, *fyA, *xfA, *yfA, *xfxA, *xfyA, *yfxA, *curlyA, *callA,
		 *plusA, *minusA, *timesA, *divideA, *modA, *negA, *trueA,
		 *failA, *repeatA, *topA, *userA, *gramA, *consultA, *endA;

Static clause *andG, *or1G;
/* Clauses transparent to cut. */
Static clause *or2G;

/* Stack pointer for trail. */
Static trailentry *trailend;

/* Stack pointer for locstack. */
Static short loctop;
/* Stack pointer for display. */
Static env envtop;
/* Last non-determinate choice. */
Static env choicepoint;
/* Stack pointer for global stack. */
Static node *glotop;
/* Size of global stack. */
Static long glosize;

/* The local stack is in two parts.  Clause activation records are
   kept in display, and local variables in nodes pointed at by elements
   of locstack. This inelegancy is forced by the type rules of Pascal. */
Static node *locstack[LocSize];
Static _REC_display display[MaxFrames];

/* Flags for switch settings etc. */
Static long flag[FlagSize];

/* Table of actions for Unify. */
Static enum {
  funcU, intU, VTbindU, TVbindU, VVbindU, succeedU, failU
} Uaction[(long)anonT - (long)funcT + 1][(long)anonT - (long)funcT + 1];

/* Table of character types. */
Static class_ CharClass[256];




/* ...

[1]  FILE HANDLING

Files are represented in the main part of the interpreter by
file descriptors.  These are indices in the array 'filetable'.
Input files are double-buffered to aid production of helpful
syntax error messages.

The bodies of the functions OpenFile and OpenLib and of
the procedure CloseFile must be supplied for each machine.

... */


Static Void ExecError PP((error e));

Static Void InternalError PP((long n));


Static Void CopyArg(src, dst)
astring src;
Char *dst;
{
  /* Copy a string from src to dst. */
  long k;

  /* CopyArg */
  for (k = 0; k <= ArgLen - 2; k++) {
    if (k + 1 <= src.length)
      dst[k] = stringbuf[src.index + k];
    else
      dst[k] = ' ';
  }
  dst[ArgLen - 1] = ' ';
}


Static boolean OpenFile(f, name, dir)
FILE **f;
astring name;
inout dir;
{
  /*
     The characters stringbuf[name.index + 1 .. name.index + name.length]
     are the name of a file.  Associate the textfile variable f with this
     file and open it for input (if dir = inZ) or output (if dir = outZ).
     Return true if f is successfully opened, and false otherwise.  The
     body of this function is installation-specific.
  */
  boolean Result;
  stringarg namebuf;
  Char STR1[256];

  /* OpenFile */
  CopyArg(name, namebuf);
  switch (dir) {

  case inZ:
    if (CanRead(namebuf)) {
      if (*f != NULL) {
	sprintf(STR1, "%.80s", namebuf);
	*f = freopen(STR1, "r", *f);
      } else {
	sprintf(STR1, "%.80s", namebuf);
	*f = fopen(STR1, "r");
      }
      if (*f == NULL)
	_EscIO(FileNotFound);
      Result = true;
    } else
      Result = false;
    break;

  case outZ:
    if (*f != NULL) {
      sprintf(STR1, "%.80s", namebuf);
      *f = freopen(STR1, "w", *f);
    } else {
      sprintf(STR1, "%.80s", namebuf);
      *f = fopen(STR1, "w");
    }
    if (*f == NULL)
      _EscIO(FileNotFound);
    Result = true;
    break;
  }
  return Result;
}


Static boolean OpenLib(f)
FILE **f;
{
  /*
     Associate the textfile variable f with the Prolog system library file
     and open it for input.  Return true if f is successfully opened, and
     false otherwise.  The body of this function is installation-specific.
  */
  stringarg envname, name;
  Char STR1[256];

  /* OpenLib */
  memcpy(envname,
    "prolib                                                                          ",
    sizeof(stringarg));
  if (GetEnv(envname, name)) {
    if (*f != NULL) {
      sprintf(STR1, "%.80s", name);
      *f = freopen(STR1, "r", *f);
    } else {
      sprintf(STR1, "%.80s", name);
      *f = fopen(STR1, "r");
    }
    if (*f == NULL)
      _EscIO(FileNotFound);
    return true;
  }
  if (*f != NULL)
    *f = freopen(ProlibName, "r", *f);
  else
    *f = fopen(ProlibName, "r");
  if (*f == NULL)
    _EscIO(FileNotFound);
  return true;
}


Static Void CloseFile(f, dir)
FILE **f;
inout dir;
{
  /*
     Close the textfile variable f.  The body of this procedure is
     installation-specific.
  */
  /* CloseFile */
  fflush(*f);
  P_ioresult = 0;
}


Static boolean UserCode(arg)
astring arg;
{
  /* A place to put users' own code. */
  stringarg argbuf;

  /* UserCode */
  CopyArg(arg, argbuf);
  return (SysCmd(argbuf));
}


Static Void StartLine()
{
  /* Begin recording a new line in 'linebuf'. */
  /* StartLine */
  linefinished = false;
  charpos = 0;
  errpos = 0;
}


Static boolean FindFile(name, fd)
atomentry *name;
filedesc *fd;
{
  /*
     Try to find a named file in the file table, returning true if
     successful and assigning the file descriptor to fd.  Otherwise,
     return false and set fd to a free descriptor if possible,
     and to 0 otherwise.
  */
  filedesc k;
  boolean found;

  /* FindFile */
  found = false;
  *fd = 0;
  k = 0;
  while (k < MaxFiles && !found) {
    k++;
    if (filetable[k - 1].fname == name) {
      *fd = k;
      found = true;
    } else if (filetable[k - 1].fname == NULL)
      *fd = k;
  }
  return found;
}


Static Void Select(fd, dir)
filedesc fd;
inout dir;
{
  /* Select fd for input or output. */
  /* Select */
  current[(long)dir] = fd;
  if (dir == inZ)
    StartLine();
}


Static boolean SelectFile(name, dir, e)
atomentry *name;
inout dir;
error *e;
{
  /* Select a named file for input or output. */
  filedesc fd;
  boolean ok;
  _REC_filetable *WITH;

  /* SelectFile */
  ok = true;
  if (name == userA)
    fd = StandFD;
  else if (FindFile(name, &fd)) {
    if (filetable[fd - 1].direction != dir) {
      *e = iodirE;
      ok = false;
    }
  } else {
    if (fd == 0)
      ExecError(filespaceE);
    WITH = &filetable[fd - 1];
    if (OpenFile(&WITH->filep, name->ident, dir)) {
      WITH->fname = name;
      WITH->direction = dir;
    } else {
      *e = nofileE;
      ok = false;
    }
  }
  if (ok)
    Select(fd, dir);
  return ok;
}


Static Void DropFile(name)
atomentry *name;
{
  /* Close a named file, and make input or output revert to the standard
     file if necessary. */
  filedesc fd;
  _REC_filetable *WITH;

  /* DropFile */
  if (name == userA)
    return;
  if (!FindFile(name, &fd))
    return;
  WITH = &filetable[fd - 1];
  if (current[(long)WITH->direction] == fd)
    Select(StandFD, WITH->direction);
  WITH->fname = NULL;
  CloseFile(&WITH->filep, WITH->direction);
}


Static Void SelectLib()
{
  /* Prepare for input from the library file. */
  _REC_filetable *WITH;

  /* SelectLib */
  WITH = filetable;
  if (!OpenLib(&WITH->filep))
    InternalError(7L);
  WITH->direction = inZ;
  current[(long)inZ] = 1;
  StartLine();
}


Static Void DropLib()
{
  /* Close the library file. */
  /* DropLib */
  CloseFile(&filetable[0].filep, inZ);
  Select(StandFD, inZ);
}


Static atomentry *CurrFile(dir)
inout dir;
{
  /* Return the name of the current input or output file. */
  /* CurrFile */
  if (current[(long)dir] == StandFD)
    return userA;
  else
    return (filetable[current[(long)dir] - 1].fname);
}


Static boolean FileEnded()
{
  /* Is the current input file finished? */
  /* FileEnded */
  if (current[(long)inZ] == StandFD) {
    if (P_eof(stdin)) {
      rewind(stdin);
      putchar('\n');
      return true;
    } else
      return false;
  } else
    return P_eof(filetable[current[(long)inZ] - 1].filep);
}


Static boolean LineEnded()
{
  /* Is the current input file at the end of a line? */
  /* LineEnded */
  if (FileEnded())
    ExecError(eofE);
  if (current[(long)inZ] == StandFD)
    return P_eoln(stdin);
  else
    return P_eoln(filetable[current[(long)inZ] - 1].filep);
}


Static Void GetChar(ch)
Char *ch;
{
  /* Input a character from the current input file. */
  /* GetChar */
  if (FileEnded())
    ExecError(eofE);
  if (linefinished)
    StartLine();
  linefinished = LineEnded();
  if (linefinished) {
    if (current[(long)inZ] == StandFD) {
      scanf("%*[^\n]");
      getchar();
    } else {
      fscanf(filetable[current[(long)inZ] - 1].filep, "%*[^\n]");
      getc(filetable[current[(long)inZ] - 1].filep);
    }
    *ch = ' ';
  } else {
    if (current[(long)inZ] == StandFD) {
      *ch = getchar();
      if (*ch == '\n')
	*ch = ' ';
    } else {
      *ch = getc(filetable[current[(long)inZ] - 1].filep);
      if (*ch == '\n')
	*ch = ' ';
    }
  }
  charpos++;
  if (charpos <= MaxLength)
    linebuf[charpos - 1] = *ch;
}


Static Void PutChar(fd, ch)
filedesc fd;
Char ch;
{
  /* Output a character to file fd. */
  /* PutChar */
  if (fd == StandFD)
    putchar(ch);
  else
    putc(ch, filetable[fd - 1].filep);
}


Static Void PutNum(fd, n)
filedesc fd;
long n;
{
  /* Output a number to file fd. */
  /* PutNum */
  if (fd == StandFD)
    printf("%ld", n);
  else
    fprintf(filetable[fd - 1].filep, "%ld", n);
}


Static Void PutLn(fd)
filedesc fd;
{
  /* Start a new line on file fd. */
  /* PutLn */
  if (fd == StandFD)
    putchar('\n');
  else
    putc('\n', filetable[fd - 1].filep);
}


Static Void SkipToDot()
{
  /* Skip characters from current input file to find a full stop. */
  Char ch, lastch;

  /* SkipToDot */
  GetChar(&ch);
  do {
    lastch = ch;
    GetChar(&ch);
  } while (lastch != '.' || ch != ' ');
}


Static Void Recover()
{
  /* Recover from a syntax error. */
  Char ch;
  long i;

  /* Recover */
  while (!linefinished)
    GetChar(&ch);
  i = 0;
  while (i < charpos - 1 && i < MaxLength) {
    i++;
    putchar(linebuf[i - 1]);
  }
  if (charpos > MaxLength + 1)
    printf(" ...");
  putchar('\n');
  i = 0;
  while (i < errpos - 1 && i < MaxLength) {
    i++;
    if (CharClass[linebuf[i - 1]] == spaceC)
      putchar(linebuf[i - 1]);
    else
      putchar(' ');
  }
  if (errpos <= MaxLength)
    printf("!\n");
  else
    printf("  !\n");
  if (current[(long)inZ] == StandFD)
    return;
  if (charpos > MaxLength + 1)
    SkipToDot();
  else if (linebuf[charpos - 2] != '.')
    SkipToDot();
}


Static Void InitFiles()
{
  long k;

  /* InitFiles */
  for (k = 0; k < MaxFiles; k++)
    filetable[k].fname = NULL;
  Select(StandFD, inZ);
  Select(StandFD, outZ);
}


Static Void FreeFiles()
{
  /* Close all opened files. */
  long k;
  _REC_filetable *WITH;

  /* FreeFiles */
  current[(long)inZ] = StandFD;
  current[(long)outZ] = StandFD;
  for (k = 0; k < MaxFiles; k++) {
    WITH = &filetable[k];
    if (WITH->fname != NULL) {
      CloseFile(&WITH->filep, WITH->direction);
      WITH->fname = NULL;
    }
  }
}



/* ...

[2]  ERROR REPORTING

There are three kinds of error: syntax errors, which result in
failure of the function ReadIn [7], execution errors, which
result in abortion and a return to the top level, and internal
errors, which never happen, but would result in leaving the Prolog
system.  Production of error messages is mediated by the procedure
Report, which pre-emptively crashes the system if errors occur
during initialization.  The procedure Recover [1] produces an
indication of the position of syntax errors.

... */


Static Void KillStacks PP((int newtop));

Static jmp_buf _JL999;


Static Void Crash()
{
  /* Exit from the Prolog system. */
  /* Crash */
  longjmp(_JL999, 1);
}


Static jmp_buf _JL100;


Static Void Abort()
{
  /* Abort the current execution if any. */
  /* Abort */
  KillStacks(0);
  if (flag[sysmode - 1] == 1)
    Crash();
  printf("[Execution aborted]\n");
  longjmp(_JL100, 1);
}


Static Void Report(e)
error e;
{
  /* Output an error message. */
  /* Report */
  if (flag[sysmode - 1] == 1 && flag[debugging - 1] == 0) {
    printf("[Error during initialization]\n");
    Crash();
  }
  printf("Error: ");
  switch (e) {

  case arityE:
    printf("wrong number of arguments.\n");
    break;

  case argsE:
    printf("unsuitable form of arguments.\n");
    break;

  case assertE:
    printf("asserting unsuitable term.\n");
    break;

  case atomspaceE:
    printf("out of atom space.\n");
    break;

  case badcddE:
    printf("probably malformed ',..'.\n");
    break;

  case badcharE:
    printf("character value out of range.\n");
    break;

  case badcommaE:
    printf("comma in tail of list.\n");
    break;

  case baddotE:
    printf("closing bracket missing.\n");
    break;

  case badexpE:
    printf("malformed expression.\n");
    break;

  case badketE:
    printf("unmatched closing bracket.\n");
    break;

  case badvbarE:
    printf("',..' or '|' not in list tail.\n");
    break;

  case commentE:
    printf("unterminated comment.\n");
    break;

  case depthE:
    printf("nesting too deep: probably cyclic term.\n");
    break;

  case divideE:
    printf("dividing by zero.\n");
    break;

  case eofE:
    printf("reading past end of file.\n");
    break;

  case framespaceE:
    printf("out of frame space.\n");
    break;

  case localspaceE:
    printf("out of local stack space\n");
    break;

  case needopE:
    printf("infix or postfix operator expected.\n");
    break;

  case needquoteE:
    printf("closing quote expected.\n");
    break;

  case needrandE:
    printf("operand or prefix operator expected.\n");
    break;

  case precE:
    printf("cannot resolve operator precedence.\n");
    break;

  case readstackE:
    printf("input term too complicated.\n");
    break;

  case sysprocE:
    printf("accessing or modifying system procedures.\n");
    break;

  case weirdchE:
    printf("illegal character in input.\n");
    break;

  case nvarsE:
    printf("asserting term with too many variables.\n");
    break;

  case iodirE:
    printf("opening file for both input and output.\n");
    break;

  case filespaceE:
    printf("too many files open at once.\n");
    break;

  case nofileE:
    printf("can't open file.\n");
    break;

  case varexpE:
    printf("uninstantiated variable in expression.\n");
    break;
  }
}


Static Void ExecError(e)
error e;
{
  /* (e: error) */
  /* Report an error in execution. */
  /* ExecError */
  Report(e);
  Abort();
}


Static Void InternalError(n)
long n;
{
  /* (n: integer) */
  /* Report an internal error and crash. */
  /* InternalError */
  printf("[Internal system error %ld]\n", n);
  Crash();
}



/* ...

[3]  TRAIL

When backtracking occurs, it is necessary to undo the variable bindings
introduced during execution of the failed clauses.  For this purpose,
certain critical bindings are recorded on an auxiliary stack called the
trail.  The critical bindings are those involving variables created in
environments older than choicepoint: those newer than choicepoint will
disappear when the stacks contract.

... */

Static boolean Critical(v)
node *v;
{
  /* Need v be recorded on the trail? */
  boolean Result;

  /* Critical */
  if (choicepoint == 0)
    return false;
  switch (v->field) {

  case globalF:
    Result = (v->scope <= display[choicepoint - 1].Fglotop->scope);
    break;

  case localF:
    Result = (v->scope <= display[choicepoint - 1].Fbase);
    break;

  case heapF:
    InternalError(1L);
    break;
  }
  return Result;
}


Static Void TrailVar(v)
node *v;
{
  /* Record v on the trail if necessary. */
  trailentry *p;

  /* TrailVar */
  if (!Critical(v))
    return;
  p = (trailentry *)Malloc(sizeof(trailentry));
  p->boundvar = v;
  p->chain = NULL;
  trailend->chain = p;
  trailend = p;
}


Static Void TrimTrail(base)
trailentry *base;
{
  /*
     Remove references to variables newer than choicepoint. Some of the
     Ftrail entries in 'display' may be made invalid by this operation,
     but it doesn't matter, since they will never be used for
     backtracking.
  */
  trailentry *p, *q;

  /* TrimTrail */
  p = base;
  q = p->chain;
  while (q != NULL) {
    if (!Critical(q->boundvar)) {
      p->chain = q->chain;
      Free(q);
    } else
      p = q;
    q = p->chain;
  }
  trailend = p;
}


Static Void Untrail(newtrail)
trailentry *newtrail;
{
  /*
     Undo all variable bindings recorded a final segment of the trail,
     starting with the one after 'newtrail'. Untriail is also used at the
     end of execution to recover the storage used for the trail.
  */
  trailentry *p, *q;
  nodeinfo *WITH1;

  /* Untrail */
  trailend = newtrail;
  p = trailend->chain;
  trailend->chain = NULL;
  while (p != NULL) {
    WITH1 = &p->boundvar->info;
    WITH1->tag = varT;
    WITH1->UU.val = NULL;
    q = p->chain;
    Free(p);
    p = q;
  }
}


Static Void InitTrail()
{
  /* Set up the trail with a dummy list head. */
  trailentry *WITH;

  /* InitTrail */
  trailend = (trailentry *)Malloc(sizeof(trailentry));
  WITH = trailend;
  WITH->boundvar = NULL;
  WITH->chain = NULL;
}



/* ...

[4]  STACK MECHANISM

The abstract Prolog machine contains two stacks, the local stack and
the global stack.  The local stack is held in the global array
'display', with local variables in the global array 'locstack'.  These
arrays have stack pointers 'envtop' and 'loctop' respectively.  The
global stack is held as a chain of nodes starting at 'glotop'.

Activation records for currently active clauses are kept on the local
stacks together with their local variables.  A reference count of
current activations is maintained in each clause.

... */


Static Void DeleteClause PP((clause *cl));


Static Void NewGlobal(x)
node **x;
{
  /* Create a new node on the global stack. */
  node *WITH;

  /* NewGlobal */
  glosize++;
  *x = (node *)Malloc(sizeof(node));
  WITH = *x;
  WITH->brother = NULL;
  WITH->chain = glotop;
  WITH->field = globalF;
  WITH->scope = glosize;
  glotop = *x;
}


Static Void FreeChain(start, finish)
node *start, *finish;
{
  /* Release a chain of nodes. */
  node *p, *q;

  /* FreeChain */
  p = start;
  while (p != finish) {
    q = p->chain;
    Free(p);
    p = q;
  }
}


Static Void NewEnv(e, callp, envp, clausep, nvars)
env *e;
node *callp;
env envp;
clause *clausep;
long nvars;
{
  /* Create a new environment e. */
  long n;
  _REC_display *WITH;
  long FORLIM;
  nodeinfo *WITH1;

  /* NewEnv */
  if (envtop >= MaxFrames)
    ExecError(framespaceE);
  if (loctop + nvars > LocSize)
    ExecError(localspaceE);
  envtop++;
  *e = envtop;
  WITH = &display[*e - 1];
  WITH->Fcall = callp;
  WITH->Fenv = envp;
  WITH->Fchoice = choicepoint;
  WITH->Fclause = clausep;
  WITH->Ftrail = trailend;
  WITH->Fglotop = glotop;
  WITH->Fbase = loctop;
  if (clausep != NULL)
    clausep->refcount++;
  FORLIM = loctop + nvars;
  for (n = loctop; n < FORLIM; n++) {
    WITH1 = &locstack[n]->info;
    WITH1->tag = varT;
    WITH1->UU.val = NULL;
  }
  loctop += nvars;
}


Static Void AccEnv(e, callp, envp, clausep)
env e;
node **callp;
env *envp;
clause **clausep;
{
  /* Access information in environment e. */
  _REC_display *WITH;

  /* AccEnv */
  WITH = &display[e - 1];
  *callp = WITH->Fcall;
  *envp = WITH->Fenv;
  *clausep = WITH->Fclause;
}


Static Void FreeLocal(newtop)
env newtop;
{
  /* Release all frames above newtop on the local stack. */
  env e;
  clause *cl;

  /* FreeLocal */
  e = newtop;
  while (e < envtop) {
    e++;
    cl = display[e - 1].Fclause;
    if (cl != NULL) {
      cl->refcount--;
      if (cl->denied && cl->refcount == 0)
	DeleteClause(cl);
    }
  }
  if (envtop > newtop)
    loctop = display[newtop].Fbase;
  envtop = newtop;
}


Static Void DisposeEnv()
{
  /* Recover the top frame on the local stack. */
  /* DisposeEnv */
  FreeLocal(envtop - 1);
}


Static Void Cut(e)
env e;
{
  /*
     Cut environment e.  On entry, all goals on the local stack above e
     must be descended from e.  The newest ancestor of e (including e
     itself) which is not a clause for (_, _), (_; _) or call(_) is made
     determinate.  Local stack space above e is reclaimed.
  */
  env envp;
  clause *cl;
  _REC_display *WITH;

  /* Cut */
  envp = e;
  cl = display[envp - 1].Fclause;
  while (display[envp - 1].Fchoice > 0 &&
	 (cl == andG || cl == or1G || cl == or2G || cl == NULL)) {
    envp = display[envp - 1].Fenv;
    cl = display[envp - 1].Fclause;
  }
  WITH = &display[envp - 1];
  choicepoint = WITH->Fchoice;
  TrimTrail(WITH->Ftrail);
  FreeLocal(e);
}


Static Void KillStacks(newtop)
env newtop;
{
  /* (newtop: env) */
  /*
     Dispose of all environments after newtop, together with all
     associated global storage, and undo critical variable bindings.
  */
  node *oldglotop;
  _REC_display *WITH;

  /* KillStacks */
  oldglotop = glotop;
  if (envtop > newtop) {
    WITH = &display[newtop];
    Untrail(WITH->Ftrail);
    choicepoint = WITH->Fchoice;
    glotop = WITH->Fglotop;
  }
  FreeLocal(newtop);
  FreeChain(oldglotop, glotop);
  glosize = glotop->scope;
}


Static node *EnvRef(offset, e)
long offset;
env e;
{
  /*
     Access a local variable in environment e.  The body of this function
     is inserted directly in the function Deref [5].
  */
  /* EnvRef */
  return (locstack[display[e - 1].Fbase + offset - 1]);
}


Static Void ChangeClause(e, newcl)
env e;
clause *newcl;
{
  /* Change e to have newcl as its Fclause entry: this is used in
     some evaluable predicates. */
  clause *oldcl;

  /* ChangeClause */
  oldcl = display[e - 1].Fclause;
  if (oldcl != NULL) {
    oldcl->refcount--;
    if (oldcl->denied && oldcl->refcount == 0)
      DeleteClause(oldcl);
  }
  if (newcl != NULL)
    newcl->refcount++;
  display[e - 1].Fclause = newcl;
}


Static Void InitStacks()
{
  /* Set up the stack mechanism. */
  long n;
  node *v;

  /* InitStacks */
  envtop = 0;
  loctop = 0;
  for (n = 1; n <= LocSize; n++) {
    v = (node *)Malloc(sizeof(node));
    v->brother = NULL;
    v->chain = NULL;
    v->field = localF;
    v->scope = n;
    locstack[n - 1] = v;
  }
  glosize = 0;
  glotop = NULL;
  NewGlobal(&v);
}



/* ...

[5]  FUNCTIONS ON TERMS

This module contains an assortment of useful functions and procedures
for handling terms.

... */


Static node *MakeFunc(a, m, s)
atomentry *a;
long m;
node *s;
{
  /* Construct a functor node on the global stack. */
  node *x;
  nodeinfo *WITH;

  /* MakeFunc */
  NewGlobal(&x);
  WITH = &x->info;
  WITH->tag = funcT;
  WITH->UU.U0.name = a;
  WITH->UU.U0.arity = m;
  WITH->UU.U0.son = s;
  return x;
}


Static node *MakeInt(i)
long i;
{
  /* Construct an integer node on the global stack. */
  node *x;
  nodeinfo *WITH;

  /* MakeInt */
  NewGlobal(&x);
  WITH = &x->info;
  WITH->tag = intT;
  WITH->UU.ival = i;
  return x;
}


Static node *MakeVar(v)
node *v;
{
  /* Construct a variable node on the global stack. */
  node *x;
  nodeinfo *WITH;

  /* MakeVar */
  NewGlobal(&x);
  WITH = &x->info;
  WITH->tag = varT;
  WITH->UU.val = v;
  return x;
}


Static node *MakeBros(x, y)
node *x, *y;
{
  /* Return x after making y its brother. */
  /* MakeBros */
  x->brother = y;
  return x;
}


Static boolean IsFunc(x, a, m)
node *x;
atomentry *a;
long m;
{
  /* True iff x is a functor node with name a and arity m. */
  nodeinfo *WITH;

  /* IsFunc */
  WITH = &x->info;
  if (WITH->tag != funcT)
    return false;
  else
    return (WITH->UU.U0.name == a && WITH->UU.U0.arity == m);
}


Static boolean IsAtom(x)
node *x;
{
  /* True iff x is an atom. */
  nodeinfo *WITH;

  /* IsAtom */
  WITH = &x->info;
  if (WITH->tag != funcT)
    return false;
  else
    return (WITH->UU.U0.arity == 0);
}


Static node *Deref(x, e)
node *x;
env e;
{
  /*
     Dereference x as far as possible.  The result y is reached from x by
     a possible environment reference followed by a (possibly empty)
     chain of variable references.  The result cannot be subjected to
     further dereferencing, so y satisfies

        (y^.info.tag in [funcT, intT, varT, anonT]) and
           ((y^.info.tag = varT) => (y^.info.val = nil)).

     This function is used heavily by all parts of the interpreter.  The
     body of EnvRef [4] is inserted directly where indicated.
  */
  node *y, *z;

  /* Deref */
  y = x;
  if (y->info.tag == skelT)
    y = locstack[display[e - 1].Fbase + y->info.UU.offset - 1];
	/* EnvRef(y^.info.offset, e) */
  while (y->info.tag == varT) {
    z = y->info.UU.val;
    if (z == NULL)
      goto _L1;
    y = z;
  }
_L1:
  return y;
}


Static Void BindVars(v1, v2)
node *v1, *v2;
{
  /*
     Bind variables v1 and v2 by assigning to one of them.  The following
     rules must be obeyed when variable bindings are introduced:

        (1) No variable on the global stack may be bound to a
           variable on the local stack.  On success, the local stack may
           contract, and this must not affect the global stack.

        (2) For much the same reason, no variable on the local stack
           may be bound to a more recently created variable on the local
           stack.

     In addition, it helps to reduce the size of the trail and to make
     global stack reclamation more fruitful (should it ever get
     implemented!) if rule (2) is observed for the global stack too.
  */
  /* BindVars */
  if (v1 == v2)
    return;
  if ((long)v1->field > (long)v2->field ||
      v1->field == v2->field && v1->scope > v2->scope) {
    v1->info.UU.val = v2;
    TrailVar(v1);
  } else {
    v2->info.UU.val = v1;
    TrailVar(v2);
  }
}


Static Void Bind PP((node *v, node *x, int ev, int e, long depth));

/* Local variables for Bind: */
struct LOC_Bind {
  env e;
  long depth;
} ;

Local node *Copy(x, LINK)
node *x;
struct LOC_Bind *LINK;
{
  /* Copy x onto the global stack. */
  node *y, *z;

  /* Copy */
  y = Deref(x, LINK->e);
  z = MakeVar(NULL);
  switch (y->info.tag) {

  case funcT:
  case intT:
    Bind(z, y, 0, LINK->e, LINK->depth + 1);
    break;

  case varT:
    BindVars(y, z);
    break;

  case anonT:
    break;
    /* null */
  }
  return z;
}

Local node *CopyArgs(s, LINK)
node *s;
struct LOC_Bind *LINK;
{
  /* Copy the arguments of a functor node. */
  node *t, *u, *v;

  /* CopyArgs */
  if (s == NULL)
    return NULL;
  else {
    u = Copy(s, LINK);
    t = s->brother;
    v = u;
    while (t != NULL) {
      v->brother = Copy(t, LINK);
      t = t->brother;
      v = v->brother;
    }
    return u;
  }
}


Static Void Bind(v, x, ev, e_, depth_)
node *v, *x;
env ev, e_;
long depth_;
{
  /*
     Bind v to the value of x.  Usually it suffices to copy the 'info'
     field of the value, but if x is a functor in a clause, its arguments
     must be copied onto the global stack to make them independent of the
     environment.
  */
  struct LOC_Bind V;
  node *y, *z;
  nodeinfo *WITH;

  /* Bind */
  V.e = e_;
  V.depth = depth_;
  if (V.depth > MaxDepth)
    ExecError(depthE);
  y = Deref(x, V.e);
  if (y->info.tag != funcT || y->field != heapF) {
    v->info = y->info;
    return;
  }
  z = CopyArgs(y->info.UU.U0.son, &V);
  WITH = &v->info;
  WITH->tag = funcT;
  WITH->UU.U0.name = y->info.UU.U0.name;
  WITH->UU.U0.arity = y->info.UU.U0.arity;
  WITH->UU.U0.son = z;
}


Static Void GetBody(v, b, ev, eb)
node *v, *b;
env ev, eb;
{
  /* Bind v to a term representing the clause body b. */
  /* GetBody */
  if (b == NULL) {
    Bind(v, MakeFunc(trueA, 0L, NULL), ev, 0, 0L);
    return;
  }
  if (b->brother == NULL) {
    Bind(v, b, ev, eb, 0L);
    return;
  }
  Bind(v, MakeFunc(commaA, 2L, MakeBros(MakeVar(NULL), MakeVar(NULL))), 0, eb,
       0L);
  Bind(v->info.UU.U0.son, b, 0, eb, 0L);
  GetBody(v->info.UU.U0.son->brother, b->brother, 0, eb);
}



/* ...

[6]  ATOM TABLE

Each atom is associated with operator and clause information which is
stored in an 'atomentry'.  The identifiers for atoms in the input are
mapped to the corresponding entry through a hash table.  Collisions are
handled by chaining together atom entries.

... */


Static Void StartAtom()
{
  /* Prepare to accept characters of an atom. */
  /* StartAtom */
  newatom.index = atomhwm;
  newatom.length = 0;
}


Static Void AtomChar(c)
Char c;
{
  /* Store c as the next char of an atom. */
  /* AtomChar */
  if (newatom.index + newatom.length >= StringSpace)
    ExecError(atomspaceE);
  newatom.length++;
  stringbuf[newatom.index + newatom.length - 1] = c;
}


Static boolean SameString(s1, s2)
astring s1, s2;
{
  /* Test whether s1 and s2 are the same string. */
  long j;
  boolean same;

  /* SameString */
  if (s1.length != s2.length)
    return false;
  else {
    j = 0;
    same = true;
    while (j != s1.length && same) {
      j++;
      same = (stringbuf[s1.index + j - 1] == stringbuf[s2.index + j - 1]);
    }
    return same;
  }
}


Static atomentry *LookUp()
{
  /* Enter an atom and return its value. */
  char h;
  atomentry *a;
  boolean found;

  /* LookUp */
  /* Compute hash function: should work in 16 bit arithmetic. */
  if (newatom.length >= 1) {
    h = (stringbuf[newatom.index] * 8 +
	 stringbuf[newatom.index + newatom.length - 1] + newatom.length) %
	HashSize + 1;
/* p2c: prolog.p, line 1357:
 * Note: Using % for possibly-negative arguments [317] */
  } else
    h = 1;

  a = hashtable[h - 1];
  found = false;
  while (a != NULL && !found) {
    if (SameString(a->ident, newatom))
      found = true;
    else
      a = a->chain;
  }
  if (found)
    return a;
  a = (atomentry *)Malloc(sizeof(atomentry));
  atomcount++;
  a->ident = newatom;
  a->atomno = atomcount;
  a->chain = hashtable[h - 1];
  a->oclass = nonO;
  a->oprec = 0;
  a->sys = false;
  a->pclass = normP;
  a->UU.proc = NULL;
  atomhwm += newatom.length;
  hashtable[h - 1] = a;
  return a;
}


Static Void WriteAtom(fd, a, literal)
filedesc fd;
atomentry *a;
boolean literal;
{
  /* Write an atom to file fd.  If literal is false and the atom
     contains weird characters, put it in single quotes. */
  long n;
  long fashion;
  boolean quote;
  astring *WITH;
  long FORLIM;

  /* WriteAtom */
  WITH = &a->ident;
  if (literal || a == semiA || a == cutA || a == nilA || a == curlyA)
    quote = false;
  else if (WITH->length == 0 || a == consA)
    quote = true;
  else if (((1L << ((long)CharClass[stringbuf[WITH->index]])) &
	    ((1L << ((long)smallC)) | (1L << ((long)specialC)))) == 0)
    quote = true;
  else {
    n = WITH->index + 2;
    quote = false;
    switch (CharClass[stringbuf[WITH->index]]) {

    case smallC:
      fashion = (1L << ((long)smallC)) | (1L << ((long)largeC)) |
		(1L << ((long)digitC));
      break;

    case specialC:
      fashion = 1L << ((long)specialC);
      break;
    }
    while (n <= WITH->index + WITH->length && !quote) {
      if (((1L << ((long)CharClass[stringbuf[n - 1]])) & fashion) != 0)
	n++;
      else
	quote = true;
    }
  }
  if (quote)
    PutChar(fd, '\'');
  FORLIM = WITH->index + WITH->length;
  for (n = WITH->index; n < FORLIM; n++) {
    if (quote && stringbuf[n] == '\'') {
      PutChar(fd, '\'');
      PutChar(fd, '\'');
    } else
      PutChar(fd, stringbuf[n]);
  }
  if (quote)
    PutChar(fd, '\'');
}


Static node *ListRep(s)
astring s;
{
  /* A Prolog list of the characters of s: cf. 'atom'. */
  node *x;
  long n;

  /* ListRep */
  x = MakeFunc(nilA, 0L, NULL);
  for (n = s.index + s.length - 1; n >= s.index; n--)
    x = MakeFunc(consA, 2L, MakeBros(MakeInt((long)stringbuf[n]), x));
  return x;
}


#define M               8


typedef Char word[M];


Local Void R(w, a)
Char *w;
atomentry **a;
{
  /* Bind a to the atom with name w. */
  char i;

  /* R */
  StartAtom();
  i = 1;
  while (w[i - 1] != ' ') {
    AtomChar(w[i - 1]);
    i++;
  }
  *a = LookUp();
}

Local Void S(w, m, p)
Char *w;
evalarity m;
evalpred p;
{
  /* Associate the evaluable predicate p with the functor of
     name w and arity m. */
  atomentry *a;

  /* S */
  R(w, &a);
  a->sys = true;
  a->pclass = evalP;
  a->UU.U1.routine = p;
  a->UU.U1.arity = m;
}


Static Void InitAtoms()
{
  /* Initialise the character heap and define a few useful atoms. */
  long j;

  /* InitAtoms */
  atomhwm = 0;
  atomcount = 0;
  for (j = 0; j < HashSize; j++)
    hashtable[j] = NULL;
  R(",       ", &commaA);
  R(";       ", &semiA);
  R(".       ", &consA);
  R("!       ", &cutA);
  R("[]      ", &nilA);
  R(":-      ", &arrowA);
  R("?-      ", &questionA);
  R("call    ", &callA);
  R("fail    ", &failA);
  R("repeat  ", &repeatA);
  R("end     ", &endA);
  R("fx      ", &fxA);
  R("fy      ", &fyA);
  R("xf      ", &xfA);
  R("yf      ", &yfA);
  R("xfx     ", &xfxA);
  R("xfy     ", &xfyA);
  R("yfx     ", &yfxA);
  R("+       ", &plusA);
  R("-       ", &minusA);
  R("*       ", &timesA);
  R("/       ", &divideA);
  R("mod     ", &modA);
  R("~       ", &negA);
  R("{}      ", &curlyA);
  R("$top    ", &topA);
  R("true    ", &trueA);
  R("user    ", &userA);
  R("-->     ", &gramA);
  R("consult ", &consultA);
  S("$read   ", 2, readR);
  S("$write  ", 2, writeR);
  S("get0    ", 1, get0R);
  S("put     ", 1, putR);
  S("eoln    ", 0, eolnR);
  S("eof     ", 0, eofR);
  S("nl      ", 0, nlR);
  S("$op     ", 3, opR);
  S("$flag   ", 2, flagR);
  S("$setflg ", 2, setflgR);
  S("atom    ", 1, atomR);
  S("integer ", 1, integerR);
  S("var     ", 1, varR);
  S("halt    ", 0, haltR);
  S("is      ", 2, isR);
  S("<       ", 2, ltR);
  S("$addcl  ", 2, addclR);
  S("functor ", 3, functorR);
  S("arg     ", 3, argR);
  S("abort   ", 0, abortR);
  S("$clenv  ", 2, clenvR);
  S("$getcl  ", 4, getclR);
  S("$advcl  ", 1, advclR);
  S("$zap    ", 1, zapR);
  S("call    ", 1, callR);
  S("!       ", 0, cutR);
  S("name    ", 2, nameR);
  S("see     ", 1, seeR);
  S("seeing  ", 1, seeingR);
  S("tell    ", 1, tellR);
  S("telling ", 1, tellingR);
  S("close   ", 1, closeR);
  S("$ucode  ", 1, ucodeR);
  S("$nonsp  ", 1, nonspR);
}

#undef M



/* ...

[7]  READIN

ReadIn reads a Prolog sentence from the current input file and builds a
term from it.  The sentence is parsed using a shift- reduce parsing
algorithm which depends on operator information in atom entries.

... */


Static prec Lprec(a)
atomentry *a;
{
  /* The precedence for a left operand of a. */
  /* Lprec */
  return (a->oprec - (((1L << ((long)a->oclass)) & ((1L << ((long)xfO)) |
			 (1L << ((long)xfxO)) | (1L << ((long)xfyO)))) != 0));
}


Static prec Rprec(a)
atomentry *a;
{
  /* The precedence for a right operand of a. */
  /* Rprec */
  return (a->oprec - (((1L << ((long)a->oclass)) & ((1L << ((long)fxO)) |
			 (1L << ((long)xfxO)) | (1L << ((long)yfxO)))) != 0));
}


/* Near end of ReadIn. */

typedef enum {
  outerK, innerK, funcK, listK, endlistK, curlyK, finalK
} readstate;
typedef long stateset;

typedef enum {
  termL, opL, funcL, markL
} elemtag;
typedef elemtag atomtag;


typedef struct _REC_stack {
  elemtag tag;
  union {
    node *tval;
    atomentry *aval;
  } UU;
} _REC_stack;

typedef struct _REC_statestack {
  readstate Scontext;
  prec Shiprec;
} _REC_statestack;

/* Local variables for ReadIn: */
struct LOC_ReadIn {
  node **vtable;
  jmp_buf _JL666;
  Char ch;
  readstate context;
  enum {
    opX, randX
  } expected;
  prec hiprec, loprec;
  boolean result;

  char top;
  _REC_stack stack[ReadSize];

  char statetop;
  _REC_statestack statestack[ReadDepth];
} ;


Local Void SyntaxError(e, LINK)
error e;
struct LOC_ReadIn *LINK;
{
  /* Report a syntax error and recover. */
  /* SyntaxError */
  Report(e);
  Recover();
  LINK->result = false;
  longjmp(LINK->_JL666, 1);
}

Local Void Push(t, LINK)
elemtag t;
struct LOC_ReadIn *LINK;
{
  /* Push a new element onto the stack. */
  /* Push */
  if (LINK->top >= ReadSize)
    SyntaxError(readstackE, LINK);
  LINK->top++;
  LINK->stack[LINK->top - 1].tag = t;
}

Local Void ShiftTerm(x, LINK)
node *x;
struct LOC_ReadIn *LINK;
{
  /* Push x onto the stack. */
  /* ShiftTerm */
  Push(termL, LINK);
  LINK->stack[LINK->top - 1].UU.tval = x;
}

Local Void Shift(t, a, LINK)
atomtag t;
atomentry *a;
struct LOC_ReadIn *LINK;
{
  /* Push a onto the stack, either as an operator or as a functor. */
  /* Shift */
  Push(t, LINK);
  LINK->stack[LINK->top - 1].UU.aval = a;
}

Local node *Pop(LINK)
struct LOC_ReadIn *LINK;
{
  /* Pop a term off the stack. */
  node *Result;

  /* Pop */
  Result = LINK->stack[LINK->top - 1].UU.tval;
  LINK->top--;
  return Result;
}

Local Void Reduce(p, lp, LINK)
prec p, lp;
struct LOC_ReadIn *LINK;
{
  /*
     Collapse items on the stack. Before each reduction step, the
     operator a on top of the stack is "balanced" against the
     precedences p = b^.oprec and lp = Lprec(b) of a new operator b,
     to see if a could be a left operand of b, or b a right operand of
     a. If neither is possible or both are possible, a precedence
     conflict is reported.  If only the first is possible, a reduction
     step is taken. If only the second is possible, reduction is
     complete.
  */
  node *x;
  atomentry *a;
  boolean reduced;

  /* Reduce */
  x = Pop(LINK);
  reduced = false;
  while (LINK->stack[LINK->top - 1].tag == opL && !reduced) {
    a = LINK->stack[LINK->top - 1].UU.aval;
    if (Rprec(a) >= p && a->oprec > lp) {
      reduced = true;
      break;
    }
    if (Rprec(a) >= p || a->oprec > lp) {
      SyntaxError(precE, LINK);
      continue;
    }
    LINK->top--;
    switch (a->oclass) {

    case fxO:
    case fyO:
      x = MakeFunc(a, 1L, x);
      break;

    case xfxO:
    case xfyO:
    case yfxO:
      x = MakeFunc(a, 2L, MakeBros(Pop(LINK), x));
      break;
    }
  }
  ShiftTerm(x, LINK);
}

Local Void CheckDelim(s, e, LINK)
stateset s;
error e;
struct LOC_ReadIn *LINK;
{
  /*
     Attempt to force the state required for a delimiter.
     This state must satisfy the predicate

        (expected = opX) and (context in s).

     If initially (expected = randX) and the top item on the stack
     is a prefix operator, this operator is converted to an atom.
     This allows for constructions such as (?-) in which a prefix
     operator occurs as an atom.
  */
  atomentry *a;

  /* CheckDelim */
  if (LINK->expected == randX) {
    if (LINK->stack[LINK->top - 1].tag != opL)
      SyntaxError(needrandE, LINK);
    a = LINK->stack[LINK->top - 1].UU.aval;
    if (((1L << ((long)a->oclass)) &
	 ((1L << ((long)fxO)) | (1L << ((long)fyO)))) == 0)
      SyntaxError(needrandE, LINK);
    LINK->top--;
    ShiftTerm(MakeFunc(a, 0L, NULL), LINK);
  }
  if (((1L << ((long)LINK->context)) & s) == 0)
    SyntaxError(e, LINK);
  Reduce(MaxPrec, MaxPrec, LINK);
}

Local Void EnterContext(k, h, LINK)
readstate k;
prec h;
struct LOC_ReadIn *LINK;
{
  /* Save the current context and set up a new one. */
  _REC_statestack *WITH;

  /* EnterContext */
  if (LINK->statetop >= ReadDepth)
    SyntaxError(readstackE, LINK);
  LINK->statetop++;
  WITH = &LINK->statestack[LINK->statetop - 1];
  WITH->Scontext = LINK->context;
  WITH->Shiprec = LINK->hiprec;
  LINK->context = k;
  LINK->expected = randX;
  LINK->hiprec = h;
}

Local Void ExitContext(x, LINK)
node *x;
struct LOC_ReadIn *LINK;
{
  /* Return from an inner context, leaving its value x on the stack. */
  _REC_statestack *WITH;

  /* ExitContext */
  LINK->top--;
  ShiftTerm(x, LINK);
  WITH = &LINK->statestack[LINK->statetop - 1];
  LINK->context = WITH->Scontext;
  LINK->hiprec = WITH->Shiprec;
  LINK->statetop--;
  LINK->expected = opX;
  LINK->loprec = 0;
}

Local node *GetFunc(LINK)
struct LOC_ReadIn *LINK;
{
  /*
     Assemble a functor and arguments. On entry, the stack
     holds a funcL element, followed by one termL element for
     each argument.
  */
  node *x;
  long n;

  /* GetFunc */
  x = Pop(LINK);
  n = 1;
  while (LINK->stack[LINK->top - 1].tag == termL) {
    x = MakeBros(Pop(LINK), x);
    n++;
  }
  return (MakeFunc(LINK->stack[LINK->top - 1].UU.aval, n, x));
}

Local node *GetList(LINK)
struct LOC_ReadIn *LINK;
{
  /*
     Assemble a list. On entry, the stack holds a markL
     element, followed by a termL element for each list
     element, then a termL element for the list continuation.
  */
  node *x;

  /* GetList */
  x = Pop(LINK);
  do {
    x = MakeFunc(consA, 2L, MakeBros(Pop(LINK), x));
  } while (LINK->stack[LINK->top - 1].tag == termL);
  return x;
}

/* Local variables for StowAtom: */
struct LOC_StowAtom {
  struct LOC_ReadIn *LINK;
  atomentry *a;
} ;

Local Void SquashRand(LINK)
struct LOC_StowAtom *LINK;
{
  /* Check precedence and reduce a left operand. */
  prec p, lp;

  /* SquashRand */
  p = LINK->a->oprec;
  lp = Lprec(LINK->a);
  if (lp < LINK->LINK->loprec ||
      (p > SubPrec &&
       ((1L << ((long)LINK->LINK->context)) & ((1L << ((long)outerK)) |
	  (1L << ((long)innerK)) | (1L << ((long)curlyK)))) == 0))
/* p2c: prolog.p, line 2082: 
 * Note: Line breaker spent 0.0 seconds, 5000 tries on line 2328 [251] */
    SyntaxError(precE, LINK->LINK);
  Reduce(p, lp, LINK->LINK);
}

Local Void StowAtom(a_, LINK)
atomentry *a_;
struct LOC_ReadIn *LINK;
{
  /* Process an atom. */
  struct LOC_StowAtom V;

  V.LINK = LINK;
  /* StowAtom */
  V.a = a_;
  switch (LINK->expected) {

  case randX:
    if (LINK->ch == '(') {
      /* '(' follows an atom with no space: a functor in
         standard notation. */
      Shift(funcL, V.a, LINK);
      EnterContext(funcK, SubPrec, LINK);
      GetChar(&LINK->ch);
    } else if (((1L << ((long)V.a->oclass)) &
		((1L << ((long)fxO)) | (1L << ((long)fyO)))) != 0) {
      /* A prefix operator. */
      if (V.a->oprec > LINK->hiprec)
	SyntaxError(precE, LINK);
      Shift(opL, V.a, LINK);
      LINK->expected = randX;
      LINK->hiprec = Rprec(V.a);
    } else {
      /* An atom, i.e. a functor of arity 0. */
      ShiftTerm(MakeFunc(V.a, 0L, NULL), LINK);
      LINK->expected = opX;
      LINK->loprec = 0;
    }
    break;

  case opX:
    switch (V.a->oclass) {

    case xfO:
    case yfO:
      /* A postfix operator. */
      SquashRand(&V);
      ShiftTerm(MakeFunc(V.a, 1L, Pop(LINK)), LINK);
      LINK->expected = opX;
      LINK->loprec = V.a->oprec;
      break;

    case xfxO:
    case xfyO:
    case yfxO:
      /* An infix operator. */
      SquashRand(&V);
      Shift(opL, V.a, LINK);
      LINK->expected = randX;
      LINK->hiprec = Rprec(V.a);
      break;

    case fxO:
    case fyO:
    case nonO:
      SyntaxError(needopE, LINK);
      break;
    }
    break;
  }
}

Local long ScanInt(LINK)
struct LOC_ReadIn *LINK;
{
  /* Read a positive integer starting with ch. */
  long n;

  /* ScanInt */
  n = 0;
  do {
    n = n * 10 + LINK->ch - '0';
    GetChar(&LINK->ch);
  } while (CharClass[LINK->ch] == digitC);
  return n;
}

Local Void ScanQuote(q, LINK)
Char q;
struct LOC_ReadIn *LINK;
{
  /* Read an atom or string quoted by 'q' and store its characters
     in the atom table, translating pairs of embedded quotes. */
  boolean done;

  /* ScanQuote */
  StartAtom();
  done = false;
  do {
    if (LineEnded()) {
      errpos = charpos;
      SyntaxError(needquoteE, LINK);
    }
    GetChar(&LINK->ch);
    if (LINK->ch == q) {
      GetChar(&LINK->ch);
      done = (LINK->ch != q);
    }
    if (!done)
      AtomChar(LINK->ch);
  } while (!done);
}

Local node *EnterVar(LINK)
struct LOC_ReadIn *LINK;
{
  /* Enter a variable and return it as a term. */
  atomentry *newname;
  node *v, *p;
  boolean found;
  nodeinfo *WITH;

  /* EnterVar */
  newname = LookUp();
  p = *LINK->vtable;
  found = false;
  while (p->info.UU.U0.name != nilA && !found) {
    if (p->info.UU.U0.son->info.UU.U0.name == newname)
      found = true;
    else
      p = p->info.UU.U0.son->brother;
  }
  if (found)
    return (MakeVar(p->info.UU.U0.son->info.UU.U0.son));
  else {
    v = MakeVar(NULL);
    WITH = &p->info;
    WITH->UU.U0.name = consA;
    WITH->UU.U0.arity = 2;
    WITH->UU.U0.son = MakeBros(MakeFunc(newname, 1L, MakeVar(v)),
			       MakeFunc(nilA, 0L, NULL));
    return v;
  }
}


Static boolean ReadIn(termread, vtable_)
node **termread, **vtable_;
{
  /*
     Input and parse a Prolog sentence and build a term
     from it, returning true if successful and assigning the term
     to termread.  Return false if a syntax error is found.

     The finite state part of the parser is characterized by the variables
     'context' and 'expected'.  'context' indicates the construct being
     parsed:

        outerK       The outermost level of a sentence.
        innerK       An expression in parentheses.
        funcK        The arguments of a functor.
        listK        The elements of a list.
        endlistK     A list continuation (between '|' or ',..'
                     and ']' in a list).
        curlyK       An expression in curly brackets.
        finalK       None. The expression is complete.

     'expected' indicates whether the next symbol is to be an operator
     (opX) or an operand (randX).

     Two stacks are used: one, represented by the array 'stack', to hold
     parts of incompletely parsed terms, the other, represented by the
     array 'statestack', to hold contextual information during parsing of
     nested constructs. In fact, the parsing algorithm corresponds to a
     stack machine with a single stack, and two stacks are used only as a
     matter of convenience.
  */
  struct LOC_ReadIn V;
  Char lastch;

  /* ReadIn */
  V.vtable = vtable_;
  if (setjmp(V._JL666))
    goto _L666;
  V.top = 0;
  V.statetop = 0;
  V.result = true;   /* Default value. */
  *V.vtable = MakeFunc(nilA, 0L, NULL);
  Push(markL, &V);
  V.context = outerK;
  V.expected = randX;
  V.hiprec = MaxPrec;
  V.ch = ' ';

  do {
    if (FileEnded()) {
      /* End of file - represented by ?- end. */
      *termread = MakeFunc(questionA, 1L, MakeFunc(endA, 0L, NULL));
      V.context = finalK;
    } else {
      errpos = charpos;

      switch (CharClass[V.ch]) {

      case smallC:
	StartAtom();
	do {
	  AtomChar(V.ch);
	  GetChar(&V.ch);
	} while (((1L << ((long)CharClass[V.ch])) & ((1L << ((long)smallC)) |
		    (1L << ((long)largeC)) | (1L << ((long)digitC)))) != 0);
	StowAtom(LookUp(), &V);
	break;

      case stropC:
	ScanQuote('\'', &V);
	StowAtom(LookUp(), &V);
	break;

      case quoteC:
	if (V.expected == opX)
	  SyntaxError(needopE, &V);
	ScanQuote('"', &V);
	ShiftTerm(ListRep(newatom), &V);
	V.expected = opX;
	V.loprec = 0;
	break;

      case specialC:
	lastch = V.ch;
	GetChar(&V.ch);
	if (lastch == '/' && V.ch == '*') {
	  /* A comment. Comments don't nest. */
	  GetChar(&V.ch);
	  do {
	    lastch = V.ch;
	    GetChar(&V.ch);
	  } while (lastch != '*' || V.ch != '/');
	  GetChar(&V.ch);
	} else if (lastch == '~' && CharClass[V.ch] == digitC) {
	  /* A negative number. */
	  if (V.expected == opX)
	    SyntaxError(needopE, &V);
	  ShiftTerm(MakeInt(-ScanInt(&V)), &V);
	  V.expected = opX;
	  V.loprec = 0;
	} else if (lastch == '.' && CharClass[V.ch] == spaceC) {
	  /* A full stop. */
	  CheckDelim(1L << ((long)outerK), baddotE, &V);
	  *termread = Pop(&V);
	  V.context = finalK;
	} else {
	  StartAtom();
	  AtomChar(lastch);
	  while (CharClass[V.ch] == specialC) {
	    AtomChar(V.ch);
	    GetChar(&V.ch);
	  }
	  StowAtom(LookUp(), &V);
	}
	break;

      case largeC:
	if (V.expected == opX)
	  SyntaxError(needopE, &V);
	lastch = V.ch;
	GetChar(&V.ch);
	if (lastch == '_' &&
	    ((1L << ((long)CharClass[V.ch])) & ((1L << ((long)smallC)) |
	       (1L << ((long)largeC)) | (1L << ((long)digitC)))) == 0) {
	  /* An anonymous variable: replaced by a unique
	     ordinary variable. */
	  ShiftTerm(MakeVar(NULL), &V);
	} else {
	  StartAtom();
	  AtomChar(lastch);
	  while (((1L << ((long)CharClass[V.ch])) & ((1L << ((long)smallC)) |
		    (1L << ((long)largeC)) | (1L << ((long)digitC)))) != 0) {
	    AtomChar(V.ch);
	    GetChar(&V.ch);
	  }
	  ShiftTerm(EnterVar(&V), &V);
	}
	V.expected = opX;
	V.loprec = 0;
	break;

      case digitC:
	if (V.expected == opX)
	  SyntaxError(needopE, &V);
	ShiftTerm(MakeInt(ScanInt(&V)), &V);
	V.expected = opX;
	V.loprec = 0;
	break;

      case lparC:
	if (V.expected == opX)
	  SyntaxError(needopE, &V);
	Push(markL, &V);
	EnterContext(innerK, MaxPrec, &V);
	GetChar(&V.ch);
	break;

      case rparC:
	CheckDelim((1L << ((long)innerK)) | (1L << ((long)funcK)), badketE,
		   &V);
	switch (V.context) {

	case innerK:
	  ExitContext(Pop(&V), &V);
	  break;

	case funcK:
	  ExitContext(GetFunc(&V), &V);
	  break;
	}
	GetChar(&V.ch);
	break;

      case braC:
	GetChar(&V.ch);
	if (V.ch == ']') {
	  /* The empty list []. */
	  GetChar(&V.ch);
	  StowAtom(nilA, &V);
	} else {
	  if (V.expected == opX)
	    SyntaxError(needopE, &V);
	  Push(markL, &V);
	  EnterContext(listK, SubPrec, &V);
	}
	break;

      case ketC:
	CheckDelim((1L << ((long)listK)) | (1L << ((long)endlistK)), badketE,
		   &V);
	if (V.context == listK)
	  ShiftTerm(MakeFunc(nilA, 0L, NULL), &V);
	ExitContext(GetList(&V), &V);
	GetChar(&V.ch);
	break;

      case lcurlyC:
	GetChar(&V.ch);
	if (V.ch == '}') {
	  /* The 'curly' atom. */
	  GetChar(&V.ch);
	  StowAtom(curlyA, &V);
	} else {
	  if (V.expected == opX)
	    SyntaxError(needopE, &V);
	  Push(markL, &V);
	  EnterContext(curlyK, MaxPrec, &V);
	}
	break;

      case rcurlyC:
	CheckDelim(1L << ((long)curlyK), badketE, &V);
	ExitContext(MakeFunc(curlyA, 1L, Pop(&V)), &V);
	GetChar(&V.ch);
	break;

      case commaC:
	GetChar(&V.ch);
	if (V.ch == '.') {
	  /* Hope to find ',..', which is punned to '|'. */
	  GetChar(&V.ch);
	  if (V.ch != '.')
	    SyntaxError(badcddE, &V);
	  V.ch = '|';
	} else {
	  if (((1L << ((long)V.context)) & ((1L << ((long)outerK)) |
		 (1L << ((long)innerK)) | (1L << ((long)curlyK)))) != 0)
	    StowAtom(commaA, &V);
	  else {
	    CheckDelim((1L << ((long)funcK)) | (1L << ((long)listK)),
		       badcommaE, &V);
	    V.expected = randX;
	    V.hiprec = SubPrec;
	  }
	}
	break;

      case cutC:
	GetChar(&V.ch);
	StowAtom(cutA, &V);
	break;

      case semiC:
	GetChar(&V.ch);
	StowAtom(semiA, &V);
	break;

      case vbarC:
	CheckDelim(1L << ((long)listK), badvbarE, &V);
	V.context = endlistK;
	V.expected = randX;
	V.hiprec = SubPrec;
	GetChar(&V.ch);
	break;

      case spaceC:
	GetChar(&V.ch);
	break;

      case weirdC:
	SyntaxError(weirdchE, &V);
	break;
      }
    }
  } while (V.context != finalK);
_L666:   /* Come here after a syntax error. */
  return V.result;
}


#define n               28


typedef Char list[n];


Local Void P(s, t)
Char *s;
class_ t;
{
  long i;

  /* P */
  i = 1;
  while (s[i - 1] != ' ') {
    CharClass[s[i - 1]] = t;
    i++;
  }
}


Static Void InitRead()
{
  /* Set up table of character classes. */
  Char c;
  short TEMP;

  /* InitRead */
  for (TEMP = (Char)ordminchar; TEMP <= (Char)ordmaxchar; TEMP++) {
    c = TEMP;
    CharClass[c] = weirdC;
  }
  P("ABCDEFGHIJKLMNOPQRSTUVWXYZ_ ", largeC);
  P("abcdefghijklmnopqrstuvwxyz  ", smallC);
  P("0123456789                  ", digitC);
  P("#$%&*+-./:<=>?@\\^`~         ", specialC);
  CharClass['\''] = stropC;
  CharClass['"'] = quoteC;
  CharClass['('] = lparC;
  CharClass[')'] = rparC;
  CharClass['['] = braC;
  CharClass[']'] = ketC;
  CharClass['{'] = lcurlyC;
  CharClass['}'] = rcurlyC;
  CharClass[','] = commaC;
  CharClass['!'] = cutC;
  CharClass[';'] = semiC;
  CharClass['|'] = vbarC;
  CharClass[' '] = spaceC;
  CharClass['\t'] = spaceC;
}

#undef n


/* Local variables for WriteOut: */
struct LOC_WriteOut {
  filedesc fd;
  env e;
  long style;
} ;

Local Void WriteTerm PP((node *x, int p, long depth,
			 struct LOC_WriteOut *LINK));

Local Void WriteVar(x, LINK)
node *x;
struct LOC_WriteOut *LINK;
{
  /* Write a variable. */
  /* WriteVar */
  PutChar(LINK->fd, '_');
  if (x->field == localF)
    PutChar(LINK->fd, '_');
  PutNum(LINK->fd, x->scope);
}

/* Local variables for WriteTerm: */
struct LOC_WriteTerm {
  struct LOC_WriteOut *LINK;
  prec p;
  long depth;
  node *y;
} ;

Local Void WriteStand(LINK)
struct LOC_WriteTerm *LINK;
{
  /* Write a complex term in standard notation. */
  node *s;
  nodeinfo *WITH;

  /* WriteStand */
  WITH = &LINK->y->info;
  WriteAtom(LINK->LINK->fd, WITH->UU.U0.name, LINK->LINK->style == 1);
  PutChar(LINK->LINK->fd, '(');
  WriteTerm(WITH->UU.U0.son, SubPrec, LINK->depth + 1, LINK->LINK);
  s = WITH->UU.U0.son->brother;
  while (s != NULL) {
    PutChar(LINK->LINK->fd, ',');
    PutChar(LINK->LINK->fd, ' ');
    WriteTerm(s, SubPrec, LINK->depth + 1, LINK->LINK);
    s = s->brother;
  }
  PutChar(LINK->LINK->fd, ')');
}

Local Void WriteOp(LINK)
struct LOC_WriteTerm *LINK;
{
  /* Write an operator expression. */
  nodeinfo *WITH;

  /* WriteOp */
  WITH = &LINK->y->info;
  switch (WITH->UU.U0.name->oclass) {

  case fxO:
  case fyO:
    WriteAtom(LINK->LINK->fd, WITH->UU.U0.name, LINK->LINK->style == 1);
    PutChar(LINK->LINK->fd, ' ');
    WriteTerm(WITH->UU.U0.son, Rprec(WITH->UU.U0.name), LINK->depth + 1,
	      LINK->LINK);
    break;

  case xfO:
  case yfO:
    WriteTerm(WITH->UU.U0.son, Lprec(WITH->UU.U0.name), LINK->depth + 1,
	      LINK->LINK);
    PutChar(LINK->LINK->fd, ' ');
    WriteAtom(LINK->LINK->fd, WITH->UU.U0.name, LINK->LINK->style == 1);
    break;

  case xfxO:
  case xfyO:
  case yfxO:
    WriteTerm(WITH->UU.U0.son, Lprec(WITH->UU.U0.name), LINK->depth + 1,
	      LINK->LINK);
    if (WITH->UU.U0.name != commaA && WITH->UU.U0.name != semiA)
      PutChar(LINK->LINK->fd, ' ');
    WriteAtom(LINK->LINK->fd, WITH->UU.U0.name,
	      LINK->LINK->style == 1 || WITH->UU.U0.name == commaA);
    PutChar(LINK->LINK->fd, ' ');
    WriteTerm(WITH->UU.U0.son->brother, Rprec(WITH->UU.U0.name),
	      LINK->depth + 1, LINK->LINK);
    break;
  }
}

Local Void WriteSolo(LINK)
struct LOC_WriteTerm *LINK;
{
  /* Write an atom on its own: parentheses are needed to protect
     prefix operators. */
  boolean bracket;
  nodeinfo *WITH;

  /* WriteSolo */
  WITH = &LINK->y->info;
  bracket = (LINK->depth > 0 &&
	     ((1L << ((long)WITH->UU.U0.name->oclass)) &
	      ((1L << ((long)fxO)) | (1L << ((long)fyO)))) != 0);
  if (bracket)
    PutChar(LINK->LINK->fd, '(');
  WriteAtom(LINK->LINK->fd, WITH->UU.U0.name, LINK->LINK->style == 1);
  if (bracket)
    PutChar(LINK->LINK->fd, ')');
}

Local Void WriteList(LINK)
struct LOC_WriteTerm *LINK;
{
  /* Write a list in square bracket notation. */
  long n;
  node *z;

  /* WriteList */
  PutChar(LINK->LINK->fd, '[');
  WriteTerm(LINK->y->info.UU.U0.son, SubPrec, LINK->depth + 1, LINK->LINK);
  n = 1;
  z = Deref(LINK->y->info.UU.U0.son->brother, LINK->LINK->e);
  while ((n < WriteLength) & IsFunc(z, consA, 2L)) {
    PutChar(LINK->LINK->fd, ',');
    PutChar(LINK->LINK->fd, ' ');
    WriteTerm(z->info.UU.U0.son, SubPrec, LINK->depth + 1, LINK->LINK);
    z = Deref(z->info.UU.U0.son->brother, LINK->LINK->e);
    n++;
  }
  if (!IsFunc(z, nilA, 0L)) {
    if (n < WriteLength) {
      PutChar(LINK->LINK->fd, ' ');
      PutChar(LINK->LINK->fd, '|');
      PutChar(LINK->LINK->fd, ' ');
      WriteTerm(z, SubPrec, LINK->depth + 1, LINK->LINK);
    } else {
      PutChar(LINK->LINK->fd, ',');
      PutChar(LINK->LINK->fd, ' ');
      PutChar(LINK->LINK->fd, '.');
      PutChar(LINK->LINK->fd, '.');
      PutChar(LINK->LINK->fd, '.');
    }
  }
  PutChar(LINK->LINK->fd, ']');
}

Local Void WriteFunc(LINK)
struct LOC_WriteTerm *LINK;
{
  /* Write a complex term. */
  boolean bracket;
  nodeinfo *WITH;

  /* WriteFunc */
  WITH = &LINK->y->info;
  if (WITH->UU.U0.arity == 0) {
    WriteSolo(LINK);
    return;
  }
  if (LINK->LINK->style == 3) {
    WriteStand(LINK);
    return;
  }
  if (WITH->UU.U0.arity == 1 && WITH->UU.U0.name == curlyA) {
    PutChar(LINK->LINK->fd, '{');
    PutChar(LINK->LINK->fd, ' ');
    WriteTerm(WITH->UU.U0.son, MaxPrec, LINK->depth + 1, LINK->LINK);
    PutChar(LINK->LINK->fd, ' ');
    PutChar(LINK->LINK->fd, '}');
    return;
  }
  if (WITH->UU.U0.arity == 2 && WITH->UU.U0.name == consA) {
    WriteList(LINK);
    return;
  }
  if ((WITH->UU.U0.arity != 1 || ((1L << ((long)WITH->UU.U0.name->oclass)) &
				  ((1L << ((long)fxO)) | (1L << ((long)fyO)) |
				   (1L << ((long)xfO)) | (1L << ((long)yfO)))) == 0) &&
      (WITH->UU.U0.arity != 2 ||
       ((1L << ((long)WITH->UU.U0.name->oclass)) & ((1L << ((long)xfxO)) |
	  (1L << ((long)xfyO)) | (1L << ((long)yfxO)))) == 0)) {
/* p2c: prolog.p, line 2297: 
 * Note: Line breaker spent 0.0 seconds, 5000 tries on line 2985 [251] */
    WriteStand(LINK);
    return;
  }
  bracket = (WITH->UU.U0.name->oprec > LINK->p);
  if (bracket)
    PutChar(LINK->LINK->fd, '(');
  WriteOp(LINK);
  if (bracket)
    PutChar(LINK->LINK->fd, ')');
}

Local Void WriteTerm(x, p_, depth_, LINK)
node *x;
prec p_;
long depth_;
struct LOC_WriteOut *LINK;
{
  /* Write a term with maximum precedence p. */
  struct LOC_WriteTerm V;
  node *WITH;
  nodeinfo *WITH1;

  V.LINK = LINK;
  /* WriteTerm */
  V.p = p_;
  V.depth = depth_;
  if (V.depth == WriteDepth) {
    PutChar(LINK->fd, '.');
    PutChar(LINK->fd, '.');
    PutChar(LINK->fd, '.');
    return;
  }
  V.y = Deref(x, LINK->e);
  WITH = V.y;
  WITH1 = &WITH->info;
  switch (WITH1->tag) {

  case funcT:
    WriteFunc(&V);
    break;

  case intT:
    if (WITH1->UU.ival >= 0)
      PutNum(LINK->fd, WITH1->UU.ival);
    else {
      PutChar(LINK->fd, '~');
      PutNum(LINK->fd, -WITH1->UU.ival);
    }
    break;

  case varT:
    WriteVar(V.y, LINK);
    break;

  case anonT:
    PutChar(LINK->fd, '_');
    break;
  }
}



/* ...

[8] WRITEOUT

WriteOut writes a term to a file given by file descriptor.
The style of output depends on the parameter 'style':

   style = 1   'write'
   style = 2   'writeq'
   style = 3   'display'.

... */


Static Void WriteOut(fd_, x, e_, style_)
filedesc fd_;
node *x;
env e_;
long style_;
{
  /* Write a term to file fd. */
  struct LOC_WriteOut V;

  /* WriteOut */
  V.fd = fd_;
  V.e = e_;
  V.style = style_;
  WriteTerm(x, MaxPrec, 0L, &V);
}


Static boolean SysPred(a)
atomentry *a;
{
  /* True if a is a system predicate, i.e. has the 'sys' flag set
     or begins with '$'. */
  /* SysPred */
  if (a->sys)
    return true;
  else if (a->ident.length > 0)
    return (stringbuf[a->ident.index] == '$');
  else
    return false;
}


Static Void Trace(m, x, e)
tracemessage m;
node *x;
env e;
{
  /* Output a trace message. */
  node *y;

  /* Trace */
  y = Deref(x, e);
  /* Don't trace evaluable predicates unless debugging interpreter. */
  if (!((flag[debugging - 1] == 1) | (!SysPred(y->info.UU.U0.name))))
    return;
  switch (m) {

  case goalD:
    printf("GOAL:   ");
    break;

  case provedD:
    printf("PROVED: ");
    break;
  }
  WriteOut(StandFD, y, e, 2L);
  putchar('\n');
}


#define Infinity        859



/* ...

[9]  DATABASE

The clauses which constitute the Prolog program are stored in skeletal
form, with each variable replaced either by an anonymous variable or by
a skeletal reference containing an offset from the base of a frame on
the local stack.

Access to the clauses for a predicate is via the atom entry for the
name.  Space occupied by denied clauses is reclaimed when the clause
is no longer in use: this is determined by a reference count maintained
by the stack mechanism [4].

... */


Static long Hash(x, e)
node *x;
env e;
{
  /*
     Hash function for terms.  The value of Hash depends on the root
     of the first argument of the term.  Zero is returned if the first
     argument is a variable.
  */
  long Result;
  node *y, *z;
  nodeinfo *WITH;

  /* Hash */
  y = Deref(x, e);
  if (y->info.UU.U0.arity == 0)
    return Infinity;
  z = Deref(y->info.UU.U0.son, e);
  WITH = &z->info;
  switch (WITH->tag) {

  case funcT:
    Result = Infinity + WITH->UU.U0.name->atomno;
    break;

  case intT:
    Result = WITH->UU.ival - (WITH->UU.ival <= 0);
    break;

  case varT:
  case anonT:
    Result = 0;
    break;
  }
  return Result;
}

#undef Infinity


Static boolean FindClause(cl, x, e)
clause **cl;
node *x;
env e;
{
  /*
     Advance cl to the first applicable clause. Hash is used to compare
     clause heads with the goal x. If either has hash function zero, a
     variable is present and a match is always possible.
  */
  key k;
  boolean ok;
  clause *WITH;

  /* FindClause */
  k = Hash(x, e);
  ok = false;
  while (*cl != NULL && !ok) {
    WITH = *cl;
    if (WITH->denied || WITH->keyval != 0 && k != 0 && WITH->keyval != k)
      *cl = WITH->chain;
    else
      ok = true;
  }
  return ok;
}


typedef struct _REC_varmap {
  node *sourcevar;
  boolean alloc;
  union {
    node *firstref;
    long address;
  } UU;
} _REC_varmap;

/* Local variables for AddClause: */
struct LOC_AddClause {
  env envp;
  boolean asserta;
  jmp_buf _JL888;
  /* Near end of AddClause */

  node *heaptop;
  char varcount;
  long framesize;
  boolean result;

  _REC_varmap varmap[MaxVars];
} ;

Local node *Skeleton PP((node *x, long depth, struct LOC_AddClause *LINK));

Local Void AddClError(e, LINK)
error e;
struct LOC_AddClause *LINK;
{
  /* Report an error and return false. */
  /* AddClError */
  Report(e);
  LINK->result = false;
  longjmp(LINK->_JL888, 1);
}

Local Void NewSkel(x, LINK)
node **x;
struct LOC_AddClause *LINK;
{
  /* Create a new node for a skeleton. */
  node *WITH;

  /* NewSkel */
  *x = (node *)Malloc(sizeof(node));
  WITH = *x;
  WITH->brother = NULL;
  WITH->chain = LINK->heaptop;
  WITH->field = heapF;
  WITH->scope = 0;
  LINK->heaptop = *x;
}

Local node *SkelVar(v, LINK)
node *v;
struct LOC_AddClause *LINK;
{
  /*
     Produce a skeleton for a variable v. When the first occurrence of
     v is encountered, it is tentatively translated as an anonymous
     variable, and a pointer to this variable is stored in the
     'varmap' entry. If a second occurrence is encountered, the
     anonymous variable is changed to a skeletal reference.
  */
  char n;
  node *w;
  boolean found;
  _REC_varmap *WITH;
  nodeinfo *WITH1;

  /* SkelVar */
  n = 0;
  found = false;
  while (n != LINK->varcount && !found) {
    n++;
    found = (LINK->varmap[n - 1].sourcevar == v);
  }
  if (found) {
    WITH = &LINK->varmap[n - 1];
    /* This is not the first occurrence. */
    if (!WITH->alloc) {
      /* This is the second occurrence -
         allocate space on the local stack. */
      LINK->framesize++;
      WITH1 = &WITH->UU.firstref->info;
      WITH1->tag = skelT;
      WITH1->UU.offset = LINK->framesize;
      WITH->alloc = true;
      WITH->UU.address = LINK->framesize;
    }
    NewSkel(&w, LINK);
    WITH1 = &w->info;
    WITH1->tag = skelT;
    WITH1->UU.offset = WITH->UU.address;
    return w;
  }
  /* This is the first occurrence - make an anonymous
     variable and keep a pointer to it. */
  if (LINK->varcount >= MaxVars)
    AddClError(nvarsE, LINK);
  LINK->varcount++;
  NewSkel(&w, LINK);
  w->info.tag = anonT;
  WITH = &LINK->varmap[LINK->varcount - 1];
  WITH->sourcevar = v;
  WITH->alloc = false;
  WITH->UU.firstref = w;
  return w;
}

/* Local variables for Skeleton: */
struct LOC_Skeleton {
  struct LOC_AddClause *LINK;
  long depth;
} ;

Local node *SkelArgs(s, LINK)
node *s;
struct LOC_Skeleton *LINK;
{
  /* Produce a skeleton for the arguments of a functor node. */
  node *t, *u, *v;

  /* SkelArgs */
  if (s == NULL)
    return NULL;
  else {
    u = Skeleton(s, LINK->depth + 1, LINK->LINK);
    t = s->brother;
    v = u;
    while (t != NULL) {
      v->brother = Skeleton(t, LINK->depth + 1, LINK->LINK);
      t = t->brother;
      v = v->brother;
    }
    return u;
  }
}

Local node *Skeleton(x, depth_, LINK)
node *x;
long depth_;
struct LOC_AddClause *LINK;
{
  /* Produce a skeleton for x. */
  struct LOC_Skeleton V;
  node *y, *z;
  nodeinfo *WITH;

  V.LINK = LINK;
  /* Skeleton */
  V.depth = depth_;
  if (V.depth > MaxDepth)
    AddClError(depthE, LINK);
  y = Deref(x, LINK->envp);
  switch (y->info.tag) {

  case funcT:
    NewSkel(&z, LINK);
    WITH = &z->info;
    WITH->tag = funcT;
    WITH->UU.U0.name = y->info.UU.U0.name;
    WITH->UU.U0.arity = y->info.UU.U0.arity;
    WITH->UU.U0.son = SkelArgs(y->info.UU.U0.son, &V);
    break;

  case intT:
    NewSkel(&z, LINK);
    WITH = &z->info;
    WITH->tag = intT;
    WITH->UU.ival = y->info.UU.ival;
    break;

  case varT:
    z = SkelVar(y, LINK);
    break;

  case anonT:
    NewSkel(&z, LINK);
    z->info.tag = anonT;
    break;
  }
  return z;
}

Local node *SkelCall(x, LINK)
node *x;
struct LOC_AddClause *LINK;
{
  /* Produce a skeleton for a goal in a clause body. A variable
     X is mapped onto call(X). */
  node *Result, *y, *z;
  nodeinfo *WITH;

  /* SkelCall */
  y = Deref(x, LINK->envp);
  switch (y->info.tag) {

  case funcT:
    Result = Skeleton(y, 0L, LINK);
    break;

  case varT:
    NewSkel(&z, LINK);
    WITH = &z->info;
    WITH->tag = funcT;
    WITH->UU.U0.name = callA;
    WITH->UU.U0.arity = 1;
    WITH->UU.U0.son = SkelVar(y, LINK);
    Result = z;
    break;

  case intT:
  case anonT:
    AddClError(assertE, LINK);
    break;
  }
  return Result;
}

Local node *SkelHead(x, LINK)
node *x;
struct LOC_AddClause *LINK;
{
  /* Produce a skeleton for a clause head. */
  node *y;

  /* SkelHead */
  y = Deref(x, LINK->envp);
  if (y->info.tag != funcT)
    AddClError(assertE, LINK);
  return (Skeleton(y, 0L, LINK));
}

Local node *SkelBody(x, depth, LINK)
node *x;
long depth;
struct LOC_AddClause *LINK;
{
  /* Produce a skeleton for a clause body. */
  node *y;

  /* SkelBody */
  if (depth > MaxDepth)
    AddClError(depthE, LINK);
  y = Deref(x, LINK->envp);
  if (IsFunc(y, commaA, 2L))
    return (MakeBros(SkelCall(y->info.UU.U0.son, LINK),
		     SkelBody(y->info.UU.U0.son->brother, depth + 1, LINK)));
  else
    return (SkelCall(y, LINK));
}

Local Void PlugA(cl, cp, prev, LINK)
clause *cl, **cp, *prev;
struct LOC_AddClause *LINK;
{
  /* Insert cl at the start of chain cp, making its backchain
     point to prev. */
  /* PlugA */
  cl->chain = *cp;
  cl->backchain = prev;
  if (*cp != NULL)
    (*cp)->backchain = cl;
  *cp = cl;
}

Local Void PlugZ(cl, cp, LINK)
clause *cl, **cp;
struct LOC_AddClause *LINK;
{
  /* Insert clause cl at the end of chain cp. */
  clause *p;

  /* PlugZ */
  if (*cp == NULL) {
    PlugA(cl, cp, NULL, LINK);
    return;
  }
  p = *cp;
  while (p->chain != NULL)
    p = p->chain;
  PlugA(cl, &p->chain, p, LINK);
}

Local clause *MakeClause(h, b, k, LINK)
node *h, *b;
long k;
struct LOC_AddClause *LINK;
{
  /* Make a clause with head h and body b, where the head hashes to k. */
  clause *cl;

  /* MakeClause */
  cl = (clause *)Malloc(sizeof(clause));
  cl->head = h;
  cl->body = b;
  cl->nvars = LINK->framesize;
  cl->denied = false;
  cl->refcount = 0;
  cl->keyval = k;
  cl->chain = NULL;
  cl->backchain = NULL;
  cl->heapchain = LINK->heaptop;
  return cl;
}

Local Void AddCl(x, LINK)
node *x;
struct LOC_AddClause *LINK;
{
  /* The guts of AddClause. */
  node *y, *head, *body;
  key keyval;
  atomentry *WITH;

  /* AddCl */
  y = Deref(x, LINK->envp);
  if (IsFunc(y, questionA, 1L) | IsFunc(y, gramA, 2L))
    AddClError(assertE, LINK);
  if (IsFunc(y, arrowA, 2L)) {
    head = SkelHead(y->info.UU.U0.son, LINK);
    body = SkelBody(y->info.UU.U0.son->brother, 0L, LINK);
    keyval = Hash(y->info.UU.U0.son, LINK->envp);
  } else {
    head = SkelHead(y, LINK);
    body = NULL;
    keyval = Hash(y, LINK->envp);
  }
  WITH = head->info.UU.U0.name;
  if (flag[sysmode - 1] == 0 && flag[debugging - 1] == 0 && WITH->sys ||
      WITH->pclass == evalP)
    AddClError(sysprocE, LINK);
  if (flag[sysmode - 1] == 1)
    WITH->sys = true;
  if (LINK->asserta)
    PlugA(MakeClause(head, body, keyval, LINK), &WITH->UU.proc, NULL, LINK);
  else
    PlugZ(MakeClause(head, body, keyval, LINK), &WITH->UU.proc, LINK);
}


Static boolean AddClause(newclause, envp_, asserta_)
node *newclause;
env envp_;
boolean asserta_;
{
  /*
     Produce a skeleton for newclause and add it to the database.  The new
     clause is added at the front of the clause chain if asserta is true,
     otherwise at the end.  Normally, the result is true, but false
     is returned after an error.
  */
  struct LOC_AddClause V;


  /* AddClause */
  V.envp = envp_;
  V.asserta = asserta_;
  if (setjmp(V._JL888))
    goto _L888;
  V.heaptop = NULL;
  V.varcount = 0;
  V.framesize = 0;
  V.result = true;   /* Default value. */
  AddCl(newclause, &V);
_L888:   /* Come here after an error. */
  return V.result;
}


Static Void DeleteClause(cl)
clause *cl;
{
  /* (cl: clptr) */
  /* Delete clause cl. */
  /* DeleteClause */
  if (flag[debugging - 1] == 1)
    printf("DeleteClause\n");
  if (cl->backchain != NULL)
    cl->backchain->chain = cl->chain;
  else
    cl->head->info.UU.U0.name->UU.proc = cl->chain;
  if (cl->chain != NULL)
    cl->chain->backchain = cl->backchain;
  FreeChain(cl->heapchain, NULL);
  Free(cl);
}


Static boolean Unify PP((node *x1, node *x2, int e1, int e2, long depth));

/* Local variables for Unify: */
struct LOC_Unify {
  env e1, e2;
  long depth;
} ;

Local boolean UnifyArgs(s1, s2, LINK)
node *s1, *s2;
struct LOC_Unify *LINK;
{
  /* Unify the arguments of a pair of functor nodes. */
  node *t1, *t2;
  boolean ok;

  /* UnifyArgs */
  t1 = s1;
  t2 = s2;
  ok = true;
  while (t1 != NULL && ok) {
    ok = Unify(t1, t2, LINK->e1, LINK->e2, LINK->depth + 1);
    t1 = t1->brother;
    t2 = t2->brother;
  }
  return ok;
}



/* ...

[10]  UNIFY

Unify implements the unification algorithm, which finds the most
general common instance of a pair of terms. It performs the matching
substitution by introducing variable bindings.  As tradition dictates,
no occurrence check is made before variable bindings are introduced.
Unify is used by Execute [11] to match goals against the heads of
clauses in the database.

... */


Static boolean Unify(x1, x2, e1_, e2_, depth_)
node *x1, *x2;
env e1_, e2_;
long depth_;
{
  /* Unify x1 and x2.  Perform the matching substitution
     by binding variables. */
  struct LOC_Unify V;
  boolean Result;
  node *y1, *y2;

  /* Unify */
  V.e1 = e1_;
  V.e2 = e2_;
  V.depth = depth_;
  if (V.depth > MaxDepth)
    ExecError(depthE);
  y1 = Deref(x1, V.e1);
  y2 = Deref(x2, V.e2);
  switch (Uaction[(long)y1->info.tag - (long)funcT]
	  [(long)y2->info.tag - (long)funcT]) {

  case funcU:
    if (y1->info.UU.U0.name == y2->info.UU.U0.name &&
	y1->info.UU.U0.arity == y2->info.UU.U0.arity)
      Result = UnifyArgs(y1->info.UU.U0.son, y2->info.UU.U0.son, &V);
    else
      Result = false;
    break;

  case intU:
    Result = (y1->info.UU.ival == y2->info.UU.ival);
    break;

  case VTbindU:
    Bind(y1, y2, V.e1, V.e2, 0L);
    TrailVar(y1);
    Result = true;
    break;

  case TVbindU:
    Bind(y2, y1, V.e2, V.e1, 0L);
    TrailVar(y2);
    Result = true;
    break;

  case VVbindU:
    BindVars(y1, y2);
    Result = true;
    break;

  case succeedU:
    Result = true;
    break;

  case failU:
    Result = false;
    break;
  }
  return Result;
}


Static Void InitUnify()
{
  /* Set up table of actions for Unify. */
  /* InitUnify */
  Uaction[0][0] = funcU;
  Uaction[(long)intT - (long)funcT][(long)intT - (long)funcT] = intU;
  Uaction[(long)varT - (long)funcT][0] = VTbindU;
  Uaction[(long)varT - (long)funcT][(long)intT - (long)funcT] = VTbindU;
  Uaction[0][(long)varT - (long)funcT] = TVbindU;
  Uaction[(long)intT - (long)funcT][(long)varT - (long)funcT] = TVbindU;
  Uaction[(long)varT - (long)funcT][(long)varT - (long)funcT] = VVbindU;
  Uaction[(long)anonT - (long)funcT][0] = succeedU;
  Uaction[(long)anonT - (long)funcT][(long)intT - (long)funcT] = succeedU;
  Uaction[(long)anonT - (long)funcT][(long)varT - (long)funcT] = succeedU;
  Uaction[0][(long)anonT - (long)funcT] = succeedU;
  Uaction[(long)intT - (long)funcT][(long)anonT - (long)funcT] = succeedU;
  Uaction[(long)varT - (long)funcT][(long)anonT - (long)funcT] = succeedU;
  Uaction[(long)anonT - (long)funcT][(long)anonT - (long)funcT] = succeedU;
  Uaction[0][(long)intT - (long)funcT] = failU;
  Uaction[(long)intT - (long)funcT][0] = failU;
}



/* ...

[11]  EXECUTE

Execute is the finite state control of the abstract Prolog machine. It
executes the goal 'goalp' by manipulating the local and global stacks
[4], and uses Unify [10] to match goals against clauses from the
database [9]. CallEvalPred [12] handles evaluable predicates.

... */


Static boolean CallEvalPred PP((node *call, int callenv, evalpred routine,
				long arity));


Static boolean Execute(goalp, goalenv)
node *goalp;
env goalenv;
{
  /* Execute a goal. */
  boolean Result;
  node *callp;
  env envp, callenv, baseenv;
  clause *clausep;
  enum {
    callQ, procQ, bodyQ, returnQ, failQ, finalQ
  } state;
  atomentry *WITH;

  /* Execute */
  callp = Deref(goalp, goalenv);
  callenv = goalenv;
  baseenv = envtop;
  state = callQ;

  do {
    switch (state) {

    case callQ:
      /* 'callp' holds a goal and 'callenv' its environment. */
      if (TestInt())
	Abort();
      if (flag[tracing - 1] == 1)
	Trace(goalD, callp, callenv);
      WITH = callp->info.UU.U0.name;
      switch (WITH->pclass) {

      case normP:
	clausep = WITH->UU.proc;
	state = procQ;
	break;

      case evalP:
	if (CallEvalPred(callp, callenv, WITH->UU.U1.routine,
			 (long)WITH->UU.U1.arity))
	  state = returnQ;
	else
	  state = failQ;
	break;
      }
      break;

    case procQ:
      /* 'clausep' points to a chain of untried clauses
         for the goal in 'callp'. */
      if (FindClause(&clausep, callp, callenv)) {
	NewEnv(&envp, callp, callenv, clausep, clausep->nvars);
	if (clausep->chain != NULL)
	  choicepoint = envp;
	if (Unify(clausep->head, callp, envp, callenv, 0L)) {
	  callp = clausep->body;
	  callenv = envp;
	  state = bodyQ;
	} else
	  state = failQ;
      } else
	state = failQ;
      break;

    case bodyQ:
      /* 'callp' points to a chain of uncalled goals in the body
         of some clause, and 'callenv' to the environment for
         the clause. */
      if (callp == NULL) {
	envp = callenv;
	AccEnv(envp, &callp, &callenv, &clausep);
	if (envp > choicepoint)
	  DisposeEnv();
	if (flag[tracing - 1] == 1)
	  Trace(provedD, callp, callenv);
	state = returnQ;
      } else
	state = callQ;
      break;

    case returnQ:
      /* The subgoal in 'callp' has just succeeded. */
      if (callenv > goalenv) {
	callp = callp->brother;
	state = bodyQ;
      } else {
	Result = true;
	state = finalQ;
      }
      break;

    case failQ:
      /* Failure has occurred.  'choicepoint' is the newest
         environment with a nondeterminate choice. */
      if (choicepoint > baseenv) {
	AccEnv(choicepoint, &callp, &callenv, &clausep);
	KillStacks(choicepoint - 1);
	clausep = clausep->chain;
	state = procQ;
      } else {
	Result = false;
	state = finalQ;
      }
      break;
    }
  } while (state != finalQ);
  return Result;
}



/* ...

[12]  EVALUABLE PREDICATES

The following evaluable predicates are implemented directly:

      call, !, get0, put, nl, abort,
      atom, integer, var, name, is, <,
      functor, arg, eoln, eof, see, seeing, tell,
      telling, close.

Other evaluable predicates are defined in the library file in
terms of either these or the following "secret" predicates:

      '$op', '$clenv', '$getcl', '$advcl', '$zap', '$addcl',
      '$read', '$write', '$setflg', '$flag', '$ucode', '$nonsp'.

To each of the directly implemented evaluable predicates corresponds a
value of the enumerated type 'evalpred'.

... */


Static boolean IntResult(x, e, i)
node *x;
env e;
long i;
{
  /*
     Specialized unification algorithm for returning integer results.
     IntResult(x, e, i) is equivalent to Unify(x, MakeInt(i), e, 0, 0)
     but avoids allocating a global node.
  */
  boolean Result;
  node *y;
  nodeinfo *WITH;

  /* IntResult */
  y = Deref(x, e);
  WITH = &y->info;
  switch (WITH->tag) {

  case funcT:
    Result = false;
    break;

  case intT:
    Result = (WITH->UU.ival == i);
    break;

  case varT:
    WITH->tag = intT;
    WITH->UU.ival = i;
    TrailVar(y);
    Result = true;
    break;

  case anonT:
    Result = true;
    break;
  }
  return Result;
}


/* Local variables for CallEvalPred: */
struct LOC_CallEvalPred {
  node *call;
  env callenv;
  long arity;
  boolean result;
  node *argval[MaxEvalArity];
} ;

Local Void ShowContext(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Output context information after an error. */
  node *goal, *parent;
  env goalenv, parentenv;
  clause *cl;

  /* ShowContext */
  goal = LINK->call;
  goalenv = LINK->callenv;
  AccEnv(goalenv, &parent, &parentenv, &cl);
  while ((parentenv > 0 && parent->info.UU.U0.name->sys) &
	 (!IsFunc(parent, callA, 1L))) {
    goal = parent;
    goalenv = parentenv;
    AccEnv(goalenv, &parent, &parentenv, &cl);
  }
  printf("- in call: ");
  WriteOut(StandFD, goal, goalenv, 2L);
  printf(".\n");
  if (parent->info.UU.U0.name->sys)
    return;
  printf("- called from ");
  WriteAtom(StandFD, parent->info.UU.U0.name, false);
  printf("(%ld).\n", parent->info.UU.U0.arity);
}

Local Void PredError(e, LINK)
error e;
struct LOC_CallEvalPred *LINK;
{
  /* Report an error in a built-in predicate. */
  /* PredError */
  Report(e);
  ShowContext(LINK);
  Abort();
}

Local Void GetArgs(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Fill in argval with the dereferenced arguments. */
  long i;
  node *x, *a;
  long FORLIM;

  /* GetArgs */
  x = Deref(LINK->call, LINK->callenv);
  if (LINK->arity != x->info.UU.U0.arity)
    PredError(arityE, LINK);
  a = x->info.UU.U0.son;
  FORLIM = LINK->arity;
  for (i = 0; i < FORLIM; i++) {
    LINK->argval[i] = Deref(a, LINK->callenv);
    a = a->brother;
  }
}

Local long Evaluate(x, depth, LINK)
node *x;
long depth;
struct LOC_CallEvalPred *LINK;
{
  /* Evaluate x as an arithmetic expression. */
  long Result;
  node *y;
  long a, b;
  nodeinfo *WITH;

  /* Evaluate */
  if (depth > MaxDepth)
    PredError(depthE, LINK);
  y = Deref(x, LINK->callenv);
  WITH = &y->info;
  switch (WITH->tag) {

  case funcT:
    if (WITH->UU.U0.arity == 2) {
      if (WITH->UU.U0.name == consA)
	Result = Evaluate(WITH->UU.U0.son, depth + 1, LINK);
      else {
	a = Evaluate(WITH->UU.U0.son, depth + 1, LINK);
	b = Evaluate(WITH->UU.U0.son->brother, depth + 1, LINK);
	if (WITH->UU.U0.name == plusA)
	  Result = a + b;
	else if (WITH->UU.U0.name == minusA)
	  Result = a - b;
	else if (WITH->UU.U0.name == timesA)
	  Result = a * b;
	else if (WITH->UU.U0.name == divideA) {
	  if (b == 0)
	    PredError(divideE, LINK);
	  Result = a / b;
	} else if (WITH->UU.U0.name == modA) {
	  if (b == 0)
	    PredError(divideE, LINK);
	  Result = a % b;
/* p2c: prolog.p, line 3034:
 * Note: Using % for possibly-negative arguments [317] */
	} else
	  PredError(badexpE, LINK);
      }
    } else if (WITH->UU.U0.name == negA && WITH->UU.U0.arity == 1)
      Result = -Evaluate(WITH->UU.U0.son, depth + 1, LINK);
    else
      PredError(badexpE, LINK);
    break;

  case intT:
    Result = WITH->UU.ival;
    break;

  case varT:
  case anonT:
    PredError(varexpE, LINK);
    break;
  }
  return Result;
}

Local Void DoCall(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'call'. This code is tricky. */
  node *x;
  env e1;

  /* DoCall */
  if (LINK->argval[0]->info.tag != funcT)
    PredError(argsE, LINK);
  NewEnv(&e1, LINK->call, LINK->callenv, NULL, 1L);
  x = EnvRef(1L, e1);
  Bind(x, LINK->argval[0], e1, LINK->callenv, 0L);
  LINK->result = Execute(x, e1);
  if (e1 > choicepoint)
    DisposeEnv();
}

Local Void DoRead(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$read'. */
  node *x, *v;

  /* DoRead */
  if (current[(long)inZ] == StandFD) {
    fflush(stdout);
    P_ioresult = 0;
  }
/* p2c: prolog.p, line 3067:
 * Warning: Symbol 'STDOUT' is not defined [221] */
  if (!ReadIn(&x, &v)) {
    LINK->result = false;
    return;
  }
  if (!Unify(LINK->argval[0], x, LINK->callenv, 0, 0L))
    LINK->result = false;
  else
    LINK->result = Unify(LINK->argval[1], v, LINK->callenv, 0, 0L);
}

Local Void DoWrite(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate 'write'. */
  /* DoWrite */
  if (LINK->argval[1]->info.tag != intT)
    InternalError(11L);
  WriteOut(current[(long)outZ], LINK->argval[0], LINK->callenv,
	   LINK->argval[1]->info.UU.ival);
}

Local Void DoGet0(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'get0'. */
  Char ch;

  /* DoGet0 */
  if (current[(long)inZ] == StandFD) {
    fflush(stdout);
    P_ioresult = 0;
  }
/* p2c: prolog.p, line 3087:
 * Warning: Symbol 'STDOUT' is not defined [221] */
  GetChar(&ch);
  LINK->result = IntResult(LINK->argval[0], LINK->callenv, (long)ch);
}

Local Void DoPut(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'put'. */
  long ch;

  /* DoPut */
  ch = Evaluate(LINK->argval[0], 0L, LINK);
  if ((unsigned long)ch > ordmaxchar)
    PredError(badcharE, LINK);
  PutChar(current[(long)outZ], (Char)ch);
}

Local Void DoOp(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate '$op'. */
  long p;
  atomentry *a;
  optype f;
  atomentry *WITH;

  /* DoOp */
  if ((LINK->argval[0]->info.tag != intT) | (!IsAtom(LINK->argval[1])) |
      (!IsAtom(LINK->argval[2])))
    PredError(argsE, LINK);
  p = LINK->argval[0]->info.UU.ival;
  a = LINK->argval[1]->info.UU.U0.name;
  if (p < 1 || p > MaxPrec)
    PredError(argsE, LINK);
  if (a == fxA)
    f = fxO;
  else if (a == fyA)
    f = fyO;
  else if (a == xfA)
    f = xfO;
  else if (a == yfA)
    f = yfO;
  else if (a == xfxA)
    f = xfxO;
  else if (a == xfyA)
    f = xfyO;
  else if (a == yfxA)
    f = yfxO;
  else
    PredError(argsE, LINK);
  WITH = LINK->argval[2]->info.UU.U0.name;
  WITH->oclass = f;
  WITH->oprec = p;
}

Local Void DoName(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'name'. */
  node *x, *y;
  long ch;

  /* DoName */
  if (IsAtom(LINK->argval[0])) {
    LINK->result = Unify(LINK->argval[1],
			 ListRep(LINK->argval[0]->info.UU.U0.name->ident),
			 LINK->callenv, 0, 0L);
    return;
  }
  if (((1L << ((long)LINK->argval[0]->info.tag)) &
       ((1L << ((long)varT)) | (1L << ((long)anonT)))) == 0) {
    PredError(argsE, LINK);
    return;
  }
  StartAtom();
  x = LINK->argval[1];
  while (IsFunc(x, consA, 2L)) {
    y = Deref(x->info.UU.U0.son, LINK->callenv);
    if (y->info.tag != intT)
      PredError(argsE, LINK);
    ch = y->info.UU.ival;
    if ((unsigned long)ch > ordmaxchar)
      PredError(badcharE, LINK);
    AtomChar((Char)ch);
    x = Deref(x->info.UU.U0.son->brother, LINK->callenv);
  }
  if (!IsFunc(x, nilA, 0L))
    PredError(argsE, LINK);
  LINK->result = Unify(LINK->argval[0], MakeFunc(LookUp(), 0L, NULL),
		       LINK->callenv, 0, 0L);
}

Local Void DoAddcl(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$addcl'. */
  /* DoAddcl */
  if (LINK->argval[1]->info.tag != intT)
    InternalError(13L);
  if (!AddClause(LINK->argval[0], LINK->callenv,
		 LINK->argval[1]->info.UU.ival == 1)) {
    ShowContext(LINK);
    Abort();
  }
}

Local Void DoFunctor(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'functor'. */
  node *x;
  long i, m;

  /* DoFunctor */
  switch (LINK->argval[0]->info.tag) {

  case funcT:
    if (!IntResult(LINK->argval[2], LINK->callenv,
		   LINK->argval[0]->info.UU.U0.arity))
      LINK->result = false;
    else
      LINK->result = Unify(LINK->argval[1],
	  MakeFunc(LINK->argval[0]->info.UU.U0.name, 0L, NULL), LINK->callenv,
	  0, 0L);
    break;

  case intT:
    if (!IntResult(LINK->argval[2], LINK->callenv, 0L))
      LINK->result = false;
    else
      LINK->result = IntResult(LINK->argval[1], LINK->callenv,
			       LINK->argval[0]->info.UU.ival);
    break;

  case varT:
  case anonT:
    if (LINK->argval[2]->info.tag != intT)
      PredError(argsE, LINK);
    m = LINK->argval[2]->info.UU.ival;
    if (IsAtom(LINK->argval[1]) && m >= 0) {
      x = NULL;
      for (i = m; i >= 1; i--)
	x = MakeBros(MakeVar(NULL), x);
      LINK->result = Unify(LINK->argval[0],
			   MakeFunc(LINK->argval[1]->info.UU.U0.name, m, x),
			   LINK->callenv, 0, 0L);
    } else if (LINK->argval[1]->info.tag == intT && m == 0)
      LINK->result = IntResult(LINK->argval[0], LINK->callenv,
			       LINK->argval[1]->info.UU.ival);
    else
      PredError(argsE, LINK);
    break;
  }
}

Local Void DoArg(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'arg'. */
  node *x;
  long i, n;

  /* DoArg */
  if (LINK->argval[0]->info.tag != intT || LINK->argval[1]->info.tag != funcT) {
    LINK->result = false;
    return;
  }
  n = LINK->argval[0]->info.UU.ival;
  if (n < 1 || n > LINK->argval[1]->info.UU.U0.arity) {
    LINK->result = false;
    return;
  }
  x = LINK->argval[1]->info.UU.U0.son;
  for (i = 2; i <= n; i++)
    x = x->brother;
  LINK->result = Unify(LINK->argval[2], x, LINK->callenv, LINK->callenv, 0L);
}

/* The next four functions are used for 'clause', 'deny' and
   'listing'.  The code here is extremely tricky. */

Local Void DoClEnv(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$clenv'. */
  env e1;
  atomentry *WITH;

  /* DoClEnv */
  if (LINK->argval[0]->info.tag != funcT)
    PredError(argsE, LINK);
  WITH = LINK->argval[0]->info.UU.U0.name;
  if (flag[debugging - 1] == 0 && WITH->sys || WITH->pclass == evalP)
    PredError(sysprocE, LINK);
  NewEnv(&e1, NULL, 0, WITH->UU.proc, 0L);
  LINK->result = IntResult(LINK->argval[1], LINK->callenv, (long)e1);
}

Local Void DoGetCl(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$getcl'. */
  env e1, e2;
  clause *cl;

  /* DoGetCl */
  if (LINK->argval[0]->info.tag != funcT ||
      LINK->argval[1]->info.tag != intT ||
      LINK->argval[2]->info.tag != varT || LINK->argval[3]->info.tag != varT)
    InternalError(2L);
  e1 = LINK->argval[1]->info.UU.ival;
  cl = display[e1 - 1].Fclause;
  if (!FindClause(&cl, LINK->argval[0], LINK->callenv)) {
    LINK->result = false;
    return;
  }
  NewEnv(&e2, NULL, 0, NULL, cl->nvars);
  Bind(LINK->argval[2], cl->head, LINK->callenv, e2, 0L);
  TrailVar(LINK->argval[2]);
  GetBody(LINK->argval[3], cl->body, LINK->callenv, e2);
  TrailVar(LINK->argval[3]);
  DisposeEnv();
  ChangeClause(e1, cl);
  LINK->result = true;
}

Local Void DoAdvCl(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$advcl'. */
  env e1;
  clause *cl;

  /* DoAdvCl */
  if (LINK->argval[0]->info.tag != intT)
    InternalError(3L);
  e1 = LINK->argval[0]->info.UU.ival;
  cl = display[e1 - 1].Fclause;
  if (cl == NULL)
    InternalError(4L);
  ChangeClause(e1, cl->chain);
}

Local Void DoZap(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$zap'. */
  env e1;
  clause *cl;

  /* DoZap */
  if (LINK->argval[0]->info.tag != intT)
    InternalError(5L);
  e1 = LINK->argval[0]->info.UU.ival;
  cl = display[e1 - 1].Fclause;
  if (cl == NULL)
    InternalError(6L);
  cl->denied = true;
}

Local Void DoSelect(dir, LINK)
inout dir;
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicates 'see' and 'tell'. */
  error e;

  /* DoSelect */
  if (!IsAtom(LINK->argval[0]))
    PredError(argsE, LINK);
  if (!SelectFile(LINK->argval[0]->info.UU.U0.name, dir, &e))
    PredError(e, LINK);
}

Local Void DoClose(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Evaluable predicate 'close'. */
  /* DoClose */
  if (!IsAtom(LINK->argval[0]))
    PredError(argsE, LINK);
  DropFile(LINK->argval[0]->info.UU.U0.name);
}

Local Void DoUcode(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$ucode'. */
  /* DoUcode */
  if (!IsAtom(LINK->argval[0]))
    PredError(argsE, LINK);
  LINK->result = UserCode(LINK->argval[0]->info.UU.U0.name->ident);
}

Local Void DoNonsp(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable prodicate '$nonsp'. */
  /* DoNonsp */
  if (LINK->argval[0]->info.tag != intT)
    InternalError(12L);
  LINK->result = (CharClass[(Char)LINK->argval[0]->info.UU.ival] != spaceC);
}

Local Void DoFlag(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$flag'. */
  /* DoFlag */
  if (LINK->argval[0]->info.tag != intT)
    InternalError(9L);
  LINK->result = IntResult(LINK->argval[1], LINK->callenv,
			   flag[LINK->argval[0]->info.UU.ival - 1]);
}

Local Void DoSetflg(LINK)
struct LOC_CallEvalPred *LINK;
{
  /* Hidden evaluable predicate '$setflg'. */
  /* DoSetflg */
  if (LINK->argval[0]->info.tag != intT || LINK->argval[1]->info.tag != intT)
    InternalError(10L);
  flag[LINK->argval[0]->info.UU.ival - 1] = LINK->argval[1]->info.UU.ival;
}


Static boolean CallEvalPred(call_, callenv_, routine, arity_)
node *call_;
env callenv_;
evalpred routine;
long arity_;
{
  /* (call: term; callenv: env; routine: evalpred;
                                          arity: integer): boolean */
  /* Call an evaluable predicate. */
  struct LOC_CallEvalPred V;


  /* CallEvalPred */
  V.call = call_;
  V.callenv = callenv_;
  V.arity = arity_;
  GetArgs(&V);
  V.result = true;   /* Default value. */
  switch (routine) {

  case cutR:
    Cut(V.callenv);
    break;

  case callR:
    DoCall(&V);
    break;

  case readR:
    DoRead(&V);
    break;

  case writeR:
    DoWrite(&V);
    break;

  case get0R:
    DoGet0(&V);
    break;

  case putR:
    DoPut(&V);
    break;

  case nlR:
    PutLn(current[(long)outZ]);
    break;

  case eolnR:
    V.result = LineEnded();
    break;

  case eofR:
    V.result = FileEnded();
    break;

  case opR:
    DoOp(&V);
    break;

  case abortR:
    Abort();
    break;

  case haltR:
    Crash();
    break;

  case flagR:
    DoFlag(&V);
    break;

  case setflgR:
    DoSetflg(&V);
    break;

  case atomR:
    V.result = IsAtom(V.argval[0]);
    break;

  case integerR:
    V.result = (V.argval[0]->info.tag == intT);
    break;

  case varR:
    V.result = (((1L << ((long)V.argval[0]->info.tag)) &
		 ((1L << ((long)varT)) | (1L << ((long)anonT)))) != 0);
    break;

  case nameR:
    DoName(&V);
    break;

  case isR:
    V.result = IntResult(V.argval[0], V.callenv,
			 Evaluate(V.argval[1], 0L, &V));
    break;

  case ltR:
    V.result = (Evaluate(V.argval[0], 0L, &V) < Evaluate(V.argval[1], 0L, &V));
    break;

  case addclR:
    DoAddcl(&V);
    break;

  case functorR:
    DoFunctor(&V);
    break;

  case argR:
    DoArg(&V);
    break;

  case clenvR:
    DoClEnv(&V);
    break;

  case getclR:
    DoGetCl(&V);
    break;

  case advclR:
    DoAdvCl(&V);
    break;

  case zapR:
    DoZap(&V);
    break;

  case seeR:
    DoSelect(inZ, &V);
    break;

  case seeingR:
    V.result = Unify(MakeFunc(CurrFile(inZ), 0L, NULL), V.argval[0], 0,
		     V.callenv, 0L);
    break;

  case tellR:
    DoSelect(outZ, &V);
    break;

  case tellingR:
    V.result = Unify(MakeFunc(CurrFile(outZ), 0L, NULL), V.argval[0], 0,
		     V.callenv, 0L);
    break;

  case closeR:
    DoClose(&V);
    break;

  case ucodeR:
    DoUcode(&V);
    break;

  case nonspR:
    DoNonsp(&V);
    break;
  }
  return V.result;
}



/* ...

[13]  TOP LEVEL

TopLevel calls the top level dialogue defined by the predicate '$top' in
the library file.  Before the top level is entered for the first
time, the library file itself is read and processed by the procedure
ReadLib.

... */


Static Void ReadLib()
{
  /* Read and process the library file. */
  node *x, *v;
  env e;
  boolean success;

  /* ReadLib */
  flag[sysmode - 1] = 1;
  CharClass['$'] = smallC;
  SelectLib();
  while (!FileEnded()) {
    choicepoint = 0;
    NewEnv(&e, NULL, 0, NULL, 0L);
    if (ReadIn(&x, &v)) {
      if (flag[debugging - 1] == 1) {
	WriteOut(StandFD, x, 0, 2L);
	putchar('\n');
      }
      if (IsFunc(x, questionA, 1L))
	success = Execute(x->info.UU.U0.son, 0);
      else
	success = AddClause(x, 0, false);
    }
    KillStacks(0);
  }
  flag[sysmode - 1] = 0;
  CharClass['$'] = specialC;
  DropLib();
}


Static Void InitFlags()
{
  /* Zero the flag vector. */
  long i;

  /* InitFlags */
  for (i = 0; i < FlagSize; i++)
    flag[i] = 0;
}


Static Void Initialise()
{
  /* Initialise the Prolog system. */
  /* Initialise */
  InitAtoms();
  InitFiles();
  InitTrail();
  InitStacks();
  InitRead();
  InitUnify();
  InitFlags();
  ReadLib();
  failA->sys = true;
  repeatA->UU.proc->chain = repeatA->UU.proc;
  andG = commaA->UU.proc;
  or1G = semiA->UU.proc->chain->chain;
  or2G = or1G->chain;
}


Static Void ConsultArgs()
{
  /* Consult files named as arguments. */
  long i, j;
  stringarg arg;
  boolean success;
  env e;

  /* ConsultArgs */
  for (i = 1; i < P_argc; i++) {
    P_sun_argv(arg, sizeof(stringarg), (int)i);
    StartAtom();
    j = 1;
    while (arg[j - 1] != ' ') {
      AtomChar(arg[j - 1]);
      j++;
    }
    NewEnv(&e, NULL, 0, NULL, 0L);
    success = Execute(MakeFunc(callA, 1L, MakeFunc(consultA, 1L,
				 MakeFunc(LookUp(), 0L, NULL))), 0);
    KillStacks(0);
  }
}


Static Void TopLevel()
{
  /* Call the top level. */
  env e;
  boolean success;

  /* TopLevel */
  flag[breaklevel - 1] = 0;
  choicepoint = 0;
  NewEnv(&e, NULL, 0, NULL, 0L);
  success = Execute(MakeFunc(topA, 0L, NULL), 0);
}



main(argc, argv)
int argc;
Char *argv[];
{
  PASCAL_MAIN(argc, argv);
  /* Prolog */
  if (setjmp(_JL100))
    goto _L100;
  if (setjmp(_JL999))
    goto _L999;
  puts(Version);
  Initialise();
  CatchInt();
  ConsultArgs();
_L100:   /* Come here after execution error. */
  TopLevel();
_L999:   /* Come here after fatal error. */
  FreeFiles();
  printf("\n[Leaving Prolog]\n");
  exit(EXIT_SUCCESS);
}



/* End. */
