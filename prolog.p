program Prolog(input, output);

(* ...

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

... *)



#include "util.h"
#include "catchint.h"
#include "libname.h"

label
   100                          (* Return to top level. *),
   999                          (* Exit. *);

const
   Version = 'Portable Prolog Release 2.1(j).  Wed Jul 22 16:41:23 PDT 1987.';

   MaxFrames = 1000             (* Max. no. of active goals. *);
   LocSize = 2500               (* Max. no. of local variables. *);

   MaxDepth = 200               (* Bound for recursion on terms. *);
   ReadSize = 100               (* Size of stack in ReadIn. *);
   ReadDepth = 20               (* Max. nesting depth in ReadIn. *);
   WriteDepth = 100             (* Max. nesting depth in WriteOut. *);
   WriteLength = 100            (* Max. list length in WriteOut. *);
   MaxFiles = 5                 (* Max. no. of files open at once. *);
   StandFD = 0                  (* File descriptor for standard files. *);

   StringSpace = 40000          (* Size of string buffer. *);
   HashSize = 100               (* Size of hash table. *);
   MaxVars = 50                 (* Size of var. map in AddClause. *);
   MaxLength = 200              (* Max. length of input line. *);
   MaxEvalArity = 4             (* Max. arity for evaluable predicates. *);

   MaxPrec = 1200               (* Max. operator precedence. *);
   SubPrec = 999                (* Max. prec. for subterms. *);

   (* Size and layout of flag vector. *)
   FlagSize = 10;
   sysmode = 1;
   breaklevel = 2;
   tracing = 3;
   debugging = 4;
   (* Flags 5 - 8 used by spy-points. *)

   (*
      The following two constants must be adjusted for different
      character sets.  Typical values are
         ASCII: (0, 127)
         EBCDIC: (0, 255).
   *)
   ordminchar = 0               (* Smallest character ordinal. *);
   ordmaxchar = 127             (* Largest character ordinal. *);


type
   env = 0 .. MaxFrames;
   atom = ^atomentry;

   term = ^node;
   nodetag = (funcT, intT, varT, anonT, skelT);

   nodeinfo =
      record
      case tag: nodetag of
         funcT: ( name: atom;
                  arity: integer;
                  son: term );
         intT:  ( ival: integer );
         varT:  ( val: term );
         anonT: ( );
         skelT: ( offset: integer )
      end;

   node =
      record
         brother: term          (* Next among args of parent functor. *);
         chain: term            (* Used to link nodes on global stack. *);
         field: (globalF, localF, heapF);
         scope: integer         (* Used with field to give lifetime. *);
         info: nodeinfo 
      end;

   clptr = ^clause;
   key = integer;
   clause =
      record
         head: term             (* Skeleton for head of clause. *);
         body: term             (* First goal in skeletal body. *);
         nvars: integer         (* No. of local vars excluding anons. *);
         denied: boolean        (* Set by 'deny' via '$zap'. *);
         refcount: integer      (* Number of activations. *);
         keyval: key            (* Saved result of Hash(head). *);
         chain: clptr           (* Next clause in procedure. *);
         backchain: clptr       (* Previous clause. *);
         heapchain: term        (* Chain of nodes in skeleton. *)
      end;

   inout = (inZ, outZ);
   filedesc = 0 .. MaxFiles;

   strindex = 0 .. StringSpace;
   astring =
      record
         index: strindex        (* Chars are stringbuf[index + 1] ... *);
         length: integer        (* ... to stringbuf[index + length]. *)
      end;

   optype = (fxO, fyO, xfO, yfO, xfxO, xfyO, yfxO, nonO);
   prec = 0 .. MaxPrec;
   predtype = (normP, evalP);

   evalpred = (callR, cutR, readR, writeR, get0R, putR, nlR, eolnR, eofR,
      nameR, opR, abortR, atomR, integerR, varR, flagR, setflgR,
      isR, ltR, addclR, clenvR, getclR, advclR, zapR, nonspR,
      functorR, argR, seeR, seeingR, tellR, tellingR, closeR, ucodeR,
      haltR);
   evalarity = 0 .. MaxEvalArity;

   atomentry =
      record
         ident: astring;
         atomno: integer;
         chain: atom;
         oclass: optype;
         oprec: prec;
         sys: boolean;
      case pclass: predtype of
         normP: ( proc: clptr );
         evalP: ( routine: evalpred;
                  arity: evalarity )
      end;

   trail = ^trailentry;
   trailentry =
      record
         boundvar: term;
         chain: trail
      end;

   error = (arityE, argsE, assertE, atomspaceE, badcddE, badcharE,
      badcommaE, baddotE, badexpE, badketE, badvbarE, commentE, depthE,
      divideE, eofE, framespaceE, localspaceE, needopE, needquoteE,
      needrandE, precE, readstackE, sysprocE, weirdchE,
      nvarsE, iodirE, filespaceE, nofileE, varexpE);

   tracemessage = (goalD, provedD);

   class =
      (smallC, largeC, digitC, specialC, quoteC, stropC, lparC,
         rparC, braC, ketC, lcurlyC, rcurlyC, commaC, cutC, semiC,
         vbarC, spaceC, weirdC);


var
   current:                     (* Current input and output files. *)
      array [inout] of filedesc;
   filetable:                   (* Table of open files. *)
      array [1 .. MaxFiles] of
         record
            fname: atom         (* File name: nil if entry unused. *);
            filep: text         (* The file itself. *);
            direction: inout    (* Whether for input or output. *)
         end;
   linebuf:                     (* Buffer for reflection of input line. *)
      packed array [1 .. MaxLength] of char;
   linefinished: boolean        (* True when about to start new line. *);
   charpos: 0 .. MaxLength      (* Position in current input line. *);
   errpos: 0 .. MaxLength       (* Position of error marker. *);

   stringbuf:                   (* Space to store atom names. *)
      packed array [1 .. StringSpace] of char;
   atomhwm: strindex            (* Top of permanent entries. *);
   newatom: astring             (* Temporary atom name under construction. *);
   atomcount: integer           (* Number of distinct atoms. *);
   hashtable:                   (* Hash table for atom names. *)
      array [1 .. HashSize] of atom;

   (* Various constant atoms. *)
   commaA, nilA, consA, cutA, semiA, questionA, arrowA, fxA, fyA, xfA,
      yfA, xfxA, xfyA, yfxA, curlyA, callA, plusA, minusA, timesA,
      divideA, modA, negA, trueA, failA, repeatA, topA, userA, gramA,
      consultA, endA: atom;

   andG, or1G, or2G: clptr      (* Clauses transparent to cut. *);

   trailend: trail              (* Stack pointer for trail. *);

   loctop: 0 .. LocSize         (* Stack pointer for locstack. *);
   envtop: env                  (* Stack pointer for display. *);
   choicepoint: env             (* Last non-determinate choice. *);
   glotop: term                 (* Stack pointer for global stack. *);
   glosize: integer             (* Size of global stack. *);

   (* The local stack is in two parts.  Clause activation records are
      kept in display, and local variables in nodes pointed at by elements
      of locstack. This inelegancy is forced by the type rules of Pascal. *)
   locstack: array [1 .. LocSize] of term;
   display: array [1 .. MaxFrames] of
      record
         Fcall: term            (* Invoking goal. *);
         Fenv: env              (* Environment for the goal *);
         Fchoice: env           (* Choicepoint at activation. *);
         Fclause: clptr         (* Active clause. *);
         Ftrail: trail          (* Head of trail at activation. *);
         Fglotop: term          (* Top of global stack at activation. *);
         Fbase: 0 .. LocSize    (* Base of frame in locstack. *)
      end;

   flag: array [1 .. FlagSize] of integer
                                (* Flags for switch settings etc. *);

   (* Table of actions for Unify. *)
   Uaction: array [funcT .. anonT, funcT .. anonT] of
      (funcU, intU, VTbindU, TVbindU, VVbindU, succeedU, failU);

   (* Table of character types. *)
   CharClass: array [char] of class;




(* ...

[1]  FILE HANDLING

Files are represented in the main part of the interpreter by
file descriptors.  These are indices in the array 'filetable'.
Input files are double-buffered to aid production of helpful
syntax error messages.

The bodies of the functions OpenFile and OpenLib and of
the procedure CloseFile must be supplied for each machine.

... *)


procedure ExecError(e: error);
   forward;

procedure InternalError(n: integer);
   forward;


procedure CopyArg(src: astring; var dst: stringarg);
(* Copy a string from src to dst. *)
   var k: integer;
begin
   for k := 1 to ArgLen - 1 do
      if k <= src.length then
         dst[k] := stringbuf[src.index + k]
      else
         dst[k] := ' ';
   dst[ArgLen] := ' '
end (* CopyArg *);


function OpenFile(var f: text; name: astring; dir: inout): boolean;
(*
   The characters stringbuf[name.index + 1 .. name.index + name.length]
   are the name of a file.  Associate the textfile variable f with this
   file and open it for input (if dir = inZ) or output (if dir = outZ).
   Return true if f is successfully opened, and false otherwise.  The
   body of this function is installation-specific.
*)
   var namebuf: stringarg;
begin
   CopyArg(name, namebuf);
   case dir of
      inZ: begin
            if CanRead(namebuf) then begin
               reset(f, namebuf);
               OpenFile := true
            end
            else
               OpenFile := false
         end;
      outZ: begin
            rewrite(f, namebuf);
            OpenFile := true
         end
   end
end (* OpenFile *);


function OpenLib(var f: text): boolean;
(*
   Associate the textfile variable f with the Prolog system library file
   and open it for input.  Return true if f is successfully opened, and
   false otherwise.  The body of this function is installation-specific.
*)
   var envname, name: stringarg;
begin
   envname := 'prolib';
   if GetEnv(envname, name) then
      reset(f, name)
   else
      reset(f, ProlibName);
   OpenLib := true
end (* OpenLib *);


procedure CloseFile(var f: text; dir: inout);
(*
   Close the textfile variable f.  The body of this procedure is
   installation-specific.
*)
begin
   flush(f)
end (* CloseFile *);


function UserCode(arg: astring): boolean;
(* A place to put users' own code. *)
   var argbuf: stringarg;
begin
   CopyArg(arg, argbuf);
   UserCode := SysCmd(argbuf)
end (* UserCode *);


procedure StartLine;
(* Begin recording a new line in 'linebuf'. *)
begin
   linefinished := false;
   charpos := 0;
   errpos := 0
end (* StartLine *);


function FindFile(name: atom; var fd: filedesc): boolean;
(*
   Try to find a named file in the file table, returning true if
   successful and assigning the file descriptor to fd.  Otherwise,
   return false and set fd to a free descriptor if possible,
   and to 0 otherwise.
*)
   var
      k: filedesc;
      found: boolean;
begin
   found := false; fd := 0; k := 0;
   while (k < MaxFiles) and not found do begin
      k := k + 1;
      if filetable[k].fname = name then begin
         fd := k;
         found := true
      end
      else if filetable[k].fname = nil then
         fd := k
   end;
   FindFile := found
end (* FindFile *);


procedure Select(fd: filedesc; dir: inout);
(* Select fd for input or output. *)
begin
   current[dir] := fd;
   if dir = inZ then StartLine
end (* Select *);


function SelectFile(name: atom; dir: inout; var e: error): boolean;
(* Select a named file for input or output. *)
   var
      fd: filedesc;
      ok: boolean;
begin
   ok := true;
   if name = userA then
      fd := StandFD
   else if FindFile(name, fd) then begin
      if filetable[fd].direction <> dir then begin
         e := iodirE;
         ok := false
      end
   end
   else begin
      if fd = 0 then ExecError(filespaceE);
      with filetable[fd] do begin
         if OpenFile(filep, name^.ident, dir) then begin
            fname := name;
            direction := dir
         end
         else begin
            e := nofileE;
            ok := false
         end
      end
   end;
   if ok then Select(fd, dir);
   SelectFile := ok
end (* SelectFile *);


procedure DropFile(name: atom);
(* Close a named file, and make input or output revert to the standard
   file if necessary. *)
   var fd: filedesc;
begin
   if name <> userA then begin
      if FindFile(name, fd) then
         with filetable[fd] do begin
            if current[direction] = fd then
               Select(StandFD, direction);
            fname := nil;
            CloseFile(filep, direction)
         end
   end
end (* DropFile *);


procedure SelectLib;
(* Prepare for input from the library file. *)
begin
   with filetable[1] do begin
      if not OpenLib(filep) then InternalError(7);
      direction := inZ
   end;
   current[inZ] := 1;
   StartLine
end (* SelectLib *);


procedure DropLib;
(* Close the library file. *)
begin
   CloseFile(filetable[1].filep, inZ);
   Select(StandFD, inZ)
end (* DropLib *);


function CurrFile(dir: inout): atom;
(* Return the name of the current input or output file. *)
begin
   if current[dir] = StandFD then
      CurrFile := userA
   else
      CurrFile := filetable[current[dir]].fname
end (* CurrFile *);


function FileEnded: boolean;
(* Is the current input file finished? *)
begin
   if current[inZ] = StandFD then
      if eof(input) then begin
         reset(input);
         writeln;
         FileEnded := true
      end
      else
         FileEnded := false
   else
      FileEnded := eof(filetable[current[inZ]].filep)
end (* FileEnded *);


function LineEnded: boolean;
(* Is the current input file at the end of a line? *)
begin
   if FileEnded then ExecError(eofE);
   if current[inZ] = StandFD then
      LineEnded := eoln(input)
   else
      LineEnded := eoln(filetable[current[inZ]].filep)
end (* LineEnded *);


procedure GetChar(var ch: char);
(* Input a character from the current input file. *)
begin
   if FileEnded then ExecError(eofE);
   if linefinished then StartLine;
   linefinished := LineEnded;
   if linefinished then begin
      if current[inZ] = StandFD then
         readln
      else
         readln(filetable[current[inZ]].filep);
      ch := ' '
   end
   else begin
      if current[inZ] = StandFD then
         read(ch)
      else
         read(filetable[current[inZ]].filep, ch)
   end;
   charpos := charpos + 1;
   if charpos <= MaxLength then
      linebuf[charpos] := ch
end (* GetChar *);


procedure PutChar(fd: filedesc; ch: char);
(* Output a character to file fd. *)
begin
   if fd = StandFD then
      write(ch)
   else
      write(filetable[fd].filep, ch)
end (* PutChar *);


procedure PutNum(fd: filedesc; n: integer);
(* Output a number to file fd. *)
begin
   if fd = StandFD then
      write(n: 1)
   else
      write(filetable[fd].filep, n: 1)
end (* PutNum *);


procedure PutLn(fd: filedesc);
(* Start a new line on file fd. *)
begin
   if fd = StandFD then
      writeln
   else
      writeln(filetable[fd].filep)
end (* PutLn *);


procedure SkipToDot;
(* Skip characters from current input file to find a full stop. *)
   var ch, lastch: char;
begin
   GetChar(ch);
   repeat
      lastch := ch; GetChar(ch)
   until (lastch = '.') and (ch = ' ')
end (* SkipToDot *);


procedure Recover;
(* Recover from a syntax error. *)
   var
      ch: char;
      i: integer;
begin
   while not linefinished do GetChar(ch);
   i := 0;
   while (i < charpos - 1) and (i < MaxLength) do begin
      i := i + 1;
      write(linebuf[i])
   end;
   if (charpos > MaxLength + 1) then write(' ...');
   writeln;
   i := 0;
   while (i < errpos - 1) and (i < MaxLength) do begin
      i := i + 1;
      if CharClass[linebuf[i]] = spaceC then
         write(linebuf[i])
      else
         write(' ')
   end;
   if errpos <= MaxLength then
      writeln('!')
   else
      writeln('  !');
   if current[inZ] <> StandFD then begin
      if charpos > MaxLength + 1 then
         SkipToDot
      else if linebuf[charpos - 1] <> '.' then
         SkipToDot
   end
end (* Recover *);


procedure InitFiles;
   var k: integer;
begin
   for k := 1 to MaxFiles do
      filetable[k].fname := nil;
   Select(StandFD, inZ);
   Select(StandFD, outZ)
end (* InitFiles *);


procedure FreeFiles;
(* Close all opened files. *)
   var k: integer;
begin
   current[inZ] := StandFD;
   current[outZ] := StandFD;
   for k := 1 to MaxFiles do
     with filetable[k] do
        if fname <> nil then begin
           CloseFile(filep, direction);
           fname := nil
        end
end (* FreeFiles *);



(* ...

[2]  ERROR REPORTING

There are three kinds of error: syntax errors, which result in
failure of the function ReadIn [7], execution errors, which
result in abortion and a return to the top level, and internal
errors, which never happen, but would result in leaving the Prolog
system.  Production of error messages is mediated by the procedure
Report, which pre-emptively crashes the system if errors occur
during initialization.  The procedure Recover [1] produces an
indication of the position of syntax errors.

... *)


procedure KillStacks(newtop: env);
   forward;


procedure Crash;
(* Exit from the Prolog system. *)
begin
   goto 999
end (* Crash *);


procedure Abort;
(* Abort the current execution if any. *)
begin
   KillStacks(0);
   if (flag[sysmode] = 1) then Crash;
   writeln('[Execution aborted]');
   goto 100
end (* Abort *);


procedure Report(e: error);
(* Output an error message. *)
begin
   if (flag[sysmode] = 1) and (flag[debugging] = 0) then begin
      writeln('[Error during initialization]');
      Crash
   end;
   write('Error: ');
   case e of
      arityE:      writeln('wrong number of arguments.');
      argsE:       writeln('unsuitable form of arguments.');
      assertE:     writeln('asserting unsuitable term.');
      atomspaceE:  writeln('out of atom space.');
      badcddE:     writeln('probably malformed '',..''.');
      badcharE:    writeln('character value out of range.');
      badcommaE:   writeln('comma in tail of list.');
      baddotE:     writeln('closing bracket missing.');
      badexpE:     writeln('malformed expression.');
      badketE:     writeln('unmatched closing bracket.');
      badvbarE:    writeln(''',..'' or ''|'' not in list tail.');
      commentE:    writeln('unterminated comment.');
      depthE:      writeln('nesting too deep: probably cyclic term.');
      divideE:     writeln('dividing by zero.');
      eofE:        writeln('reading past end of file.');
      framespaceE: writeln('out of frame space.');
      localspaceE: writeln('out of local stack space');
      needopE:     writeln('infix or postfix operator expected.');
      needquoteE:  writeln('closing quote expected.');
      needrandE:   writeln('operand or prefix operator expected.');
      precE:       writeln('cannot resolve operator precedence.');
      readstackE:  writeln('input term too complicated.');
      sysprocE:    writeln('accessing or modifying system procedures.');
      weirdchE:    writeln('illegal character in input.');
      nvarsE:      writeln('asserting term with too many variables.');
      iodirE:      writeln('opening file for both input and output.');
      filespaceE:  writeln('too many files open at once.');
      nofileE:     writeln('can''t open file.');
      varexpE:     writeln('uninstantiated variable in expression.')
   end
end (* Report *);


procedure ExecError (* (e: error) *);
(* Report an error in execution. *)
begin
   Report(e);
   Abort
end (* ExecError *);


procedure InternalError (* (n: integer) *);
(* Report an internal error and crash. *)
begin
   writeln('[Internal system error ', n: 1, ']');
   Crash
end (* InternalError *);



(* ...

[3]  TRAIL

When backtracking occurs, it is necessary to undo the variable bindings
introduced during execution of the failed clauses.  For this purpose,
certain critical bindings are recorded on an auxiliary stack called the
trail.  The critical bindings are those involving variables created in
environments older than choicepoint: those newer than choicepoint will
disappear when the stacks contract.

... *)

function Critical(v: term) : boolean;
(* Need v be recorded on the trail? *)
begin
   if choicepoint = 0 then
      Critical := false
   else
      case v^.field of
      globalF:
         Critical := v^.scope <= display[choicepoint].Fglotop^.scope;
      localF:
         Critical := v^.scope <= display[choicepoint].Fbase;
      heapF:
         InternalError(1)
      end
end (* Critical *);

procedure TrailVar(v: term);
(* Record v on the trail if necessary. *)
   var p: trail;
begin
   if Critical(v) then begin
      new(p);
      with p^ do begin
         boundvar := v;
         chain := nil
      end;
      trailend^.chain := p;
      trailend := p
   end
end (* TrailVar *);


procedure TrimTrail(base: trail);
(*
   Remove references to variables newer than choicepoint. Some of the
   Ftrail entries in 'display' may be made invalid by this operation,
   but it doesn't matter, since they will never be used for
   backtracking.
*)
   var p, q: trail;
begin
   p := base; q := p^.chain;
   while q <> nil do begin
      if not Critical(q^.boundvar) then begin
         p^.chain := q^.chain;
         dispose(q)
      end
      else
         p := q;
      q := p^.chain
   end;
   trailend := p
end (* TrimTrail *);


procedure Untrail(newtrail: trail);
(*
   Undo all variable bindings recorded a final segment of the trail,
   starting with the one after 'newtrail'. Untriail is also used at the
   end of execution to recover the storage used for the trail.
*)
   var p, q: trail;
begin
   trailend := newtrail;
   p := trailend^.chain;
   trailend^.chain := nil;
   while p <> nil do begin
      with p^ do begin
         with boundvar^.info do begin
            tag := varT;
            val := nil
         end;
         q := chain
      end;
      dispose(p);
      p := q
   end
end (* Untrail *);


procedure InitTrail;
(* Set up the trail with a dummy list head. *)
begin
   new(trailend);
   with trailend^ do begin
      boundvar := nil;
      chain := nil
   end
end (* InitTrail *);



(* ...

[4]  STACK MECHANISM

The abstract Prolog machine contains two stacks, the local stack and
the global stack.  The local stack is held in the global array
'display', with local variables in the global array 'locstack'.  These
arrays have stack pointers 'envtop' and 'loctop' respectively.  The
global stack is held as a chain of nodes starting at 'glotop'.

Activation records for currently active clauses are kept on the local
stacks together with their local variables.  A reference count of
current activations is maintained in each clause.

... *)


procedure DeleteClause(cl: clptr);
   forward;


procedure NewGlobal(var x: term);
(* Create a new node on the global stack. *)
begin
   glosize := glosize + 1;
   new(x);
   with x^ do begin
      brother := nil;
      chain := glotop;
      field := globalF;
      scope := glosize
   end;
   glotop := x
end (* NewGlobal *);


procedure FreeChain(start, finish: term);
(* Release a chain of nodes. *)
   var p, q: term;
begin
   p := start;
   while p <> finish do begin
      q := p^.chain;
      dispose(p);
      p := q
   end
end (* FreeChain *);


procedure NewEnv(var e: env; callp: term; envp: env;
                              clausep: clptr; nvars: integer);
(* Create a new environment e. *)
   var n: integer;
begin
   if envtop >= MaxFrames then ExecError(framespaceE);
   if loctop + nvars > LocSize then ExecError(localspaceE);
   envtop := envtop + 1;
   e := envtop;
   with display[e] do begin
      Fcall := callp;
      Fenv := envp;
      Fchoice := choicepoint;
      Fclause := clausep;
      Ftrail := trailend;
      Fglotop := glotop;
      Fbase := loctop
   end;
   if clausep <> nil then
      clausep^.refcount := clausep^.refcount + 1;
   for n := loctop + 1 to loctop + nvars do
      with locstack[n]^.info do begin
         tag := varT;
         val := nil
      end;
   loctop := loctop + nvars
end (* NewEnv *);


procedure AccEnv(e: env; var callp: term; var envp: env;
                                          var clausep: clptr);
(* Access information in environment e. *)
begin
   with display[e] do begin
      callp := Fcall;
      envp := Fenv;
      clausep := Fclause
   end
end (* AccEnv *);


procedure FreeLocal(newtop: env);
(* Release all frames above newtop on the local stack. *)
   var
      e: env;
      cl: clptr;
begin
   e := newtop;
   while e < envtop do begin
      e := e + 1;
      cl := display[e].Fclause;
      if cl <> nil then begin
         cl^.refcount := cl^.refcount - 1;
         if cl^.denied and (cl^.refcount = 0) then DeleteClause(cl)
      end
   end;
   if envtop > newtop then
      loctop := display[newtop + 1].Fbase;
   envtop := newtop
end (* FreeLocal *);


procedure DisposeEnv;
(* Recover the top frame on the local stack. *)
begin
   FreeLocal(envtop - 1)
end (* DisposeEnv *);


procedure Cut(e: env);
(*
   Cut environment e.  On entry, all goals on the local stack above e
   must be descended from e.  The newest ancestor of e (including e
   itself) which is not a clause for (_, _), (_; _) or call(_) is made
   determinate.  Local stack space above e is reclaimed.
*)
   var
      envp: env;
      cl: clptr;
begin
   envp := e;
   cl := display[envp].Fclause;
   while (display[envp].Fchoice > 0) and ((cl = andG) or
         (cl = or1G) or (cl = or2G) or (cl = nil)) do begin
      envp := display[envp].Fenv;
      cl := display[envp].Fclause
   end;
   with display[envp] do begin
      choicepoint := Fchoice;
      TrimTrail(Ftrail)
   end;
   FreeLocal(e)
end (* Cut *);


procedure KillStacks (* (newtop: env) *);
(*
   Dispose of all environments after newtop, together with all
   associated global storage, and undo critical variable bindings.
*)
   var oldglotop: term;
begin
   oldglotop := glotop;
   if envtop > newtop then
      with display[newtop + 1] do begin
         Untrail(Ftrail);
         choicepoint := Fchoice;
         glotop := Fglotop
      end;
   FreeLocal(newtop);
   FreeChain(oldglotop, glotop);
   glosize := glotop^.scope
end (* KillStacks *);


function EnvRef(offset: integer; e: env): term;
(*
   Access a local variable in environment e.  The body of this function
   is inserted directly in the function Deref [5].
*)
begin
   EnvRef := locstack[display[e].Fbase + offset]
end (* EnvRef *);


procedure ChangeClause(e: env; newcl: clptr);
(* Change e to have newcl as its Fclause entry: this is used in
   some evaluable predicates. *)
   var oldcl: clptr;
begin
   oldcl := display[e].Fclause;
   if oldcl <> nil then begin
      oldcl^.refcount := oldcl^.refcount - 1;
      if oldcl^.denied and (oldcl^.refcount = 0) then
         DeleteClause(oldcl)
   end;
   if newcl <> nil then
      newcl^.refcount := newcl^.refcount + 1;
   display[e].Fclause := newcl
end (* ChangeClause *);


procedure InitStacks;
(* Set up the stack mechanism. *)
   var
      n: integer;
      v: term;
begin
   envtop := 0;
   loctop := 0;
   for n := 1 to LocSize do begin
      new(v);
      with v^ do begin
         brother := nil;
         chain := nil;
         field := localF;
         scope := n
      end;
      locstack[n] := v
   end;
   glosize := 0;
   glotop := nil;
   NewGlobal(v)
end (* InitStacks *);



(* ...

[5]  FUNCTIONS ON TERMS

This module contains an assortment of useful functions and procedures
for handling terms.

... *)


function MakeFunc(a: atom; m: integer; s: term): term;
(* Construct a functor node on the global stack. *)
   var x: term;
begin
   NewGlobal(x);
   with x^.info do begin
      tag := funcT;
      name := a;
      arity := m;
      son := s
   end;
   MakeFunc := x
end (* MakeFunc *);


function MakeInt(i: integer): term;
(* Construct an integer node on the global stack. *)
   var x: term;
begin
   NewGlobal(x);
   with x^.info do begin
      tag := intT;
      ival := i
   end;
   MakeInt := x
end (* MakeInt *);


function MakeVar(v: term): term;
(* Construct a variable node on the global stack. *)
   var x: term;
begin
   NewGlobal(x);
   with x^.info do begin
      tag := varT;
      val := v
   end;
   MakeVar := x
end (* MakeVar *);


function MakeBros(x, y: term): term;
(* Return x after making y its brother. *)
begin
   x^.brother := y;
   MakeBros := x
end (* MakeBros *);


function IsFunc(x: term; a: atom; m: integer): boolean;
(* True iff x is a functor node with name a and arity m. *)
begin
   with x^.info do
      if tag <> funcT then
         IsFunc := false
      else
         IsFunc := (name = a) and (arity = m)
end (* IsFunc *);


function IsAtom(x: term): boolean;
(* True iff x is an atom. *)
begin
   with x^.info do
      if tag <> funcT then
         IsAtom := false
      else
         IsAtom := arity = 0
end (* IsAtom *);


function Deref(x: term; e: env): term;
(*
   Dereference x as far as possible.  The result y is reached from x by
   a possible environment reference followed by a (possibly empty)
   chain of variable references.  The result cannot be subjected to
   further dereferencing, so y satisfies

      (y^.info.tag in [funcT, intT, varT, anonT]) and
         ((y^.info.tag = varT) => (y^.info.val = nil)).

   This function is used heavily by all parts of the interpreter.  The
   body of EnvRef [4] is inserted directly where indicated.
*)
   label 1;
   var y, z: term;
begin
   y := x;
   if y^.info.tag = skelT then
      y := (* EnvRef(y^.info.offset, e) *)
            locstack[display[e].Fbase + y^.info.offset];
   while y^.info.tag = varT do begin
      z := y^.info.val;
      if z = nil then goto 1;
      y := z
   end;
1:
   Deref := y
end (* Deref *);


procedure BindVars(v1, v2: term);
(*
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
*)
begin
   if v1 <> v2 then
      if (v1^.field > v2^.field) or (v1^.field = v2^.field) and
                               (v1^.scope > v2^.scope) then begin
         v1^.info.val := v2;
         TrailVar(v1)
      end
      else begin
         v2^.info.val := v1;
         TrailVar(v2)
      end
end (* BindVars *);


procedure Bind(v, x: term; ev, e: env; depth: integer);
(*
   Bind v to the value of x.  Usually it suffices to copy the 'info'
   field of the value, but if x is a functor in a clause, its arguments
   must be copied onto the global stack to make them independent of the
   environment.
*)
   var y, z: term;

   function Copy(x: term): term;
   (* Copy x onto the global stack. *)
      var y, z: term;
   begin
      y := Deref(x, e);
      z := MakeVar(nil);
      case y^.info.tag of
         funcT, intT:
            Bind(z, y, 0, e, depth + 1);
         varT:
            BindVars(y, z);
         anonT:
            (* null *)
      end;
      Copy := z
   end (* Copy *);

   function CopyArgs(s: term): term;
   (* Copy the arguments of a functor node. *)
      var t, u, v: term;
   begin
      if s = nil then
         CopyArgs := nil
      else begin
         u := Copy(s);
         t := s^.brother; v := u;
         while t <> nil do begin
            v^.brother := Copy(t);
            t := t^.brother; v := v^.brother
         end;
         CopyArgs := u
      end
   end (* CopyArgs *);

begin (* Bind *)
   if depth > MaxDepth then ExecError(depthE);
   y := Deref(x, e);
   if (y^.info.tag = funcT) and (y^.field = heapF) then begin
      z := CopyArgs(y^.info.son);
      with v^.info do begin
         tag := funcT;
         name := y^.info.name;
         arity := y^.info.arity;
         son := z
      end
   end
   else
      v^.info := y^.info
end (* Bind *);


procedure GetBody(v, b: term; ev, eb: env);
(* Bind v to a term representing the clause body b. *)
begin
   if b = nil then
      Bind(v, MakeFunc(trueA, 0, nil), ev, 0, 0)
   else if b^.brother = nil then
      Bind(v, b, ev, eb, 0)
   else begin
      Bind(v, MakeFunc(commaA, 2, MakeBros(MakeVar(nil),
                                           MakeVar(nil))), 0, eb, 0);
      Bind(v^.info.son, b, 0, eb, 0);
      GetBody(v^.info.son^.brother, b^.brother, 0, eb)
   end
end (* GetBody *);



(* ...

[6]  ATOM TABLE

Each atom is associated with operator and clause information which is
stored in an 'atomentry'.  The identifiers for atoms in the input are
mapped to the corresponding entry through a hash table.  Collisions are
handled by chaining together atom entries.

... *)


procedure StartAtom;
(* Prepare to accept characters of an atom. *)
begin
   with newatom do begin
      index := atomhwm;
      length := 0
   end
end (* StartAtom *);


procedure AtomChar(c: char);
(* Store c as the next char of an atom. *)
begin
   with newatom do begin
      if index + length >= StringSpace then ExecError(atomspaceE);
      length := length + 1;
      stringbuf[index + length] := c
   end
end (* AtomChar *);


function SameString(s1, s2: astring): boolean;
(* Test whether s1 and s2 are the same string. *)
   var
      j: integer;
      same: boolean;
begin
   if s1.length <> s2.length then
      SameString := false
   else begin
      j := 0; same := true;
      while (j <> s1.length) and same do begin
         j := j + 1;
         same := stringbuf[s1.index + j] = stringbuf[s2.index + j]
      end;
      SameString := same
   end
end (* SameString *);


function LookUp: atom;
(* Enter an atom and return its value. *)
   var
      h: 1 .. HashSize;
      a: atom;
      found: boolean;
begin
   (* Compute hash function: should work in 16 bit arithmetic. *)
   with newatom do
      if length >= 1 then
         h := (8 * ord(stringbuf[index + 1]) +
                ord(stringbuf[index + length]) + length) mod HashSize + 1
      else
         h := 1;

   a := hashtable[h];
   found := false;
   while (a <> nil) and not found do
      if SameString(a^.ident, newatom) then
         found := true
      else
         a := a^.chain;
   if not found then begin
      new(a);
      atomcount := atomcount + 1;
      with a^ do begin
         ident := newatom;
         atomno := atomcount;
         chain := hashtable[h];
         oclass := nonO;
         oprec := 0;
         sys := false;
         pclass := normP;
         proc := nil
      end;
      atomhwm := atomhwm + newatom.length;
      hashtable[h] := a
   end;
   LookUp := a
end (* LookUp *);


procedure WriteAtom(fd: filedesc; a: atom; literal: boolean);
(* Write an atom to file fd.  If literal is false and the atom
   contains weird characters, put it in single quotes. *)
   var
      n: integer;
      fashion: set of class;
      quote: boolean;
begin
   with a^.ident do begin
      if literal or (a = semiA) or (a = cutA) or
            (a = nilA) or (a = curlyA) then
         quote := false
      else if (length = 0) or (a = consA) then
         quote := true
      else if not (CharClass[stringbuf[index + 1]] in
            [smallC, specialC]) then
         quote := true
      else begin
         n := index + 2; quote := false;
         case CharClass[stringbuf[index + 1]] of
         smallC:
            fashion := [smallC, largeC, digitC];
         specialC:
            fashion := [specialC]
         end;
         while (n <= index + length) and not quote do
            if CharClass[stringbuf[n]] in fashion then
               n := n + 1
            else
               quote := true
      end;
      if quote then PutChar(fd, '''');
      for n := index + 1 to index + length do
         if quote and (stringbuf[n] = '''') then begin
            PutChar(fd, ''''); PutChar(fd, '''')
         end
         else
            PutChar(fd, stringbuf[n]);
      if quote then PutChar(fd, '''')
   end
end (* WriteAtom *);


function ListRep(s: astring): term;
(* A Prolog list of the characters of s: cf. 'atom'. *)
   var
      x: term;
      n: integer;
begin
   x := MakeFunc(nilA, 0, nil);
   for n := s.index + s.length downto s.index + 1 do
      x := MakeFunc(consA, 2, MakeBros(MakeInt(ord(stringbuf[n])), x));
   ListRep := x
end (* ListRep *);


procedure InitAtoms;
(* Initialise the character heap and define a few useful atoms. *)
   const M = 8;
   type word = packed array [1 .. M] of char;
   var j: integer;

   procedure R(w: word; var a: atom);
   (* Bind a to the atom with name w. *)
      var i: 1 .. M;
   begin
      StartAtom;
      i := 1;
      while w[i] <> ' ' do begin
         AtomChar(w[i]);
         i := i + 1
      end;
      a := LookUp
   end (* R *);

   procedure S(w: word; m: evalarity; p: evalpred);
   (* Associate the evaluable predicate p with the functor of
      name w and arity m. *)
      var a: atom;
   begin
      R(w, a);
      with a^ do begin
         sys := true;
         pclass := evalP;
         routine := p;
         arity := m
      end
   end (* S *);

begin (* InitAtoms *)
   atomhwm := 0;
   atomcount := 0;
   for j := 1 to HashSize do hashtable[j] := nil;
   R(',       ', commaA);       R(';       ', semiA);
   R('.       ', consA);        R('!       ', cutA);
   R('[]      ', nilA);         R(':-      ', arrowA);
   R('?-      ', questionA);    R('call    ', callA);
   R('fail    ', failA);        R('repeat  ', repeatA);
   R('end     ', endA);         R('fx      ', fxA);
   R('fy      ', fyA);          R('xf      ', xfA);
   R('yf      ', yfA);          R('xfx     ', xfxA);
   R('xfy     ', xfyA);         R('yfx     ', yfxA);
   R('+       ', plusA);        R('-       ', minusA);
   R('*       ', timesA);       R('/       ', divideA);
   R('mod     ', modA);         R('~       ', negA);
   R('{}      ', curlyA);       R('$top    ', topA);
   R('true    ', trueA);        R('user    ', userA);
   R('-->     ', gramA);
   R('consult ', consultA);
   S('$read   ', 2, readR);     S('$write  ', 2, writeR);
   S('get0    ', 1, get0R);     S('put     ', 1, putR);
   S('eoln    ', 0, eolnR);     S('eof     ', 0, eofR);
   S('nl      ', 0, nlR);       S('$op     ', 3, opR);
   S('$flag   ', 2, flagR);     S('$setflg ', 2, setflgR);
   S('atom    ', 1, atomR);     S('integer ', 1, integerR);
   S('var     ', 1, varR);      S('halt    ', 0, haltR);
   S('is      ', 2, isR);       S('<       ', 2, ltR);
   S('$addcl  ', 2, addclR);    S('functor ', 3, functorR);
   S('arg     ', 3, argR);      S('abort   ', 0, abortR);
   S('$clenv  ', 2, clenvR);    S('$getcl  ', 4, getclR);   
   S('$advcl  ', 1, advclR);    S('$zap    ', 1, zapR);
   S('call    ', 1, callR);     S('!       ', 0, cutR);
   S('name    ', 2, nameR);
   S('see     ', 1, seeR);      S('seeing  ', 1, seeingR);
   S('tell    ', 1, tellR);     S('telling ', 1, tellingR);
   S('close   ', 1, closeR);    S('$ucode  ', 1, ucodeR);
   S('$nonsp  ', 1, nonspR);
end (* InitAtoms *);



(* ...

[7]  READIN

ReadIn reads a Prolog sentence from the current input file and builds a
term from it.  The sentence is parsed using a shift- reduce parsing
algorithm which depends on operator information in atom entries.

... *)


function Lprec(a: atom): prec;
(* The precedence for a left operand of a. *)
begin
   Lprec := a^.oprec - ord(a^.oclass in [xfO, xfxO, xfyO])
end (* Lprec *);


function Rprec(a: atom): prec;
(* The precedence for a right operand of a. *)
begin
   Rprec := a^.oprec - ord(a^.oclass in [fxO, xfxO, yfxO])
end (* Rprec *);


function ReadIn(var termread, vtable: term): boolean;
(*
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
*)

   label
      666 (* Near end of ReadIn. *);

   type
      readstate = (outerK, innerK, funcK, listK, endlistK, curlyK, finalK);
      stateset = set of readstate;
      elemtag = (termL, opL, funcL, markL);
      atomtag = opL .. funcL;

   var
      ch, lastch: char;
      context: readstate;
      expected: (opX, randX);
      hiprec, loprec: prec;
      result: boolean;

      top: 0 .. ReadSize;
      stack: array [1 .. ReadSize] of
         record
            case tag: elemtag of
               termL: ( tval: term );
               opL, funcL:
                      ( aval: atom );
               markL: ( )
         end;

      statetop: 0 .. ReadDepth;
      statestack: array [1 .. ReadDepth] of
         record
            Scontext: readstate;
            Shiprec: prec
         end;


   procedure SyntaxError(e: error);
   (* Report a syntax error and recover. *)
   begin
      Report(e);
      Recover;
      result := false;
      goto 666
   end (* SyntaxError *);

   procedure Push(t: elemtag);
   (* Push a new element onto the stack. *)
   begin
      if top >= ReadSize then SyntaxError(readstackE);
      top := top + 1;
      stack[top].tag := t
   end (* Push *);

   procedure ShiftTerm(x: term);
   (* Push x onto the stack. *)
   begin
      Push(termL);
      stack[top].tval := x
   end (* ShiftTerm *);

   procedure Shift(t: atomtag; a: atom);
   (* Push a onto the stack, either as an operator or as a functor. *)
   begin
      Push(t);
      stack[top].aval := a
   end (* Shift *);

   function Pop: term;
   (* Pop a term off the stack. *)
   begin
      Pop := stack[top].tval;
      top := top - 1
   end (* Pop *);

   procedure Reduce(p, lp: prec);
   (*
      Collapse items on the stack. Before each reduction step, the
      operator a on top of the stack is "balanced" against the
      precedences p = b^.oprec and lp = Lprec(b) of a new operator b,
      to see if a could be a left operand of b, or b a right operand of
      a. If neither is possible or both are possible, a precedence
      conflict is reported.  If only the first is possible, a reduction
      step is taken. If only the second is possible, reduction is
      complete.
   *)
      var
         x: term;
         a: atom;
         reduced: boolean;
   begin
      x := Pop;
      reduced := false;
      while (stack[top].tag = opL) and not reduced do begin
         a := stack[top].aval;
         if (Rprec(a) >= p) and (a^.oprec > lp) then
            reduced := true
         else if (Rprec(a) < p) and (a^.oprec <= lp) then begin
            top := top - 1;
            case a^.oclass of
               fxO, fyO:
                  x := MakeFunc(a, 1, x);
               xfxO, xfyO, yfxO:
                  x := MakeFunc(a, 2, MakeBros(Pop, x))
            end
         end
         else
            SyntaxError(precE)
      end;
      ShiftTerm(x)
   end (* Reduce *);

   procedure CheckDelim(s: stateset; e: error);
   (*
      Attempt to force the state required for a delimiter.
      This state must satisfy the predicate

         (expected = opX) and (context in s).

      If initially (expected = randX) and the top item on the stack
      is a prefix operator, this operator is converted to an atom.
      This allows for constructions such as (?-) in which a prefix
      operator occurs as an atom.
   *)
      var a: atom;
   begin
      if expected = randX then begin
         if stack[top].tag <> opL then SyntaxError(needrandE);
         a := stack[top].aval;
         if not (a^.oclass in [fxO, fyO]) then SyntaxError(needrandE);
         top := top - 1;
         ShiftTerm(MakeFunc(a, 0, nil));
      end;
      if not (context in s) then SyntaxError(e);
      Reduce(MaxPrec, MaxPrec)
   end (* CheckDelim *);

   procedure EnterContext(k: readstate; h: prec);
   (* Save the current context and set up a new one. *)
   begin
      if statetop >= ReadDepth then SyntaxError(readstackE);
      statetop := statetop + 1;
      with statestack[statetop] do begin
         Scontext := context;
         Shiprec := hiprec
      end;
      context := k; expected := randX; hiprec := h
   end (* EnterContext *);

   procedure ExitContext(x: term);
   (* Return from an inner context, leaving its value x on the stack. *)
   begin
      top := top - 1;
      ShiftTerm(x);
      with statestack[statetop] do begin
         context := Scontext;
         hiprec := Shiprec
      end;
      statetop := statetop - 1;
      expected := opX; loprec := 0
   end (* ExitContext *);

   function GetFunc: term;
   (*
      Assemble a functor and arguments. On entry, the stack
      holds a funcL element, followed by one termL element for
      each argument.
   *)
      var
         x: term;
         n: integer;
   begin
      x := Pop;
      n := 1;
      while stack[top].tag = termL do begin
         x := MakeBros(Pop, x);
         n := n + 1
      end;
      GetFunc := MakeFunc(stack[top].aval, n, x)
   end (* GetFunc *);

   function GetList: term;
   (*
      Assemble a list. On entry, the stack holds a markL
      element, followed by a termL element for each list
      element, then a termL element for the list continuation.
   *)
      var x: term;
   begin
      x := Pop;
      repeat
         x := MakeFunc(consA, 2, MakeBros(Pop, x))
      until stack[top].tag <> termL;
      GetList := x
   end (* GetList *);

   procedure StowAtom(a: atom);
   (* Process an atom. *)

      procedure SquashRand;
      (* Check precedence and reduce a left operand. *)
         var p, lp: prec;
      begin
         p := a^.oprec; lp := Lprec(a);
         if (lp < loprec) or (p > SubPrec) and not
            (context in [outerK, innerK, curlyK]) then SyntaxError(precE);
         Reduce(p, lp)
      end (* SquashRand *);

   begin (* StowAtom *)
      case expected of
         randX:
            if ch = '(' then begin
               (* '(' follows an atom with no space: a functor in
                  standard notation. *)
               Shift(funcL, a);
               EnterContext(funcK, SubPrec);
               GetChar(ch)
            end
            else if a^.oclass in [fxO, fyO] then begin
               (* A prefix operator. *)
               if a^.oprec > hiprec then SyntaxError(precE);
               Shift(opL, a);
               expected := randX; hiprec := Rprec(a)
            end
            else begin
               (* An atom, i.e. a functor of arity 0. *)
               ShiftTerm(MakeFunc(a, 0, nil));
               expected := opX; loprec := 0
            end;

         opX:
            case a^.oclass of
               xfO, yfO: begin
                     (* A postfix operator. *)
                     SquashRand;
                     ShiftTerm(MakeFunc(a, 1, Pop));
                     expected := opX; loprec := a^.oprec
                  end;
               xfxO, xfyO, yfxO: begin
                     (* An infix operator. *)
                     SquashRand;
                     Shift(opL, a);
                     expected := randX; hiprec := Rprec(a)
                  end;
               fxO, fyO, nonO:
                  SyntaxError(needopE)
            end
      end
   end (* StowAtom *);

   function ScanInt: integer;
   (* Read a positive integer starting with ch. *)
      var n: integer;
   begin
      n := 0;
      repeat
         n := 10 * n + (ord(ch) - ord('0'));
         GetChar(ch)
      until CharClass[ch] <> digitC;
      ScanInt := n
   end (* ScanInt *);

   procedure ScanQuote(q: char);
   (* Read an atom or string quoted by 'q' and store its characters
      in the atom table, translating pairs of embedded quotes. *)
      var done: boolean;
   begin
      StartAtom;
      done := false;
      repeat
         if LineEnded then begin
            errpos := charpos;
            SyntaxError(needquoteE)
         end;
         GetChar(ch);
         if ch = q then begin
            GetChar(ch);
            done := ch <> q
         end;
         if not done then AtomChar(ch)
      until done
   end (* ScanQuote *);

   function EnterVar: term;
   (* Enter a variable and return it as a term. *)
      var
         newname: atom;
         v, p: term;
         found: boolean;
   begin
      newname := LookUp;
      p := vtable; found := false;
      while (p^.info.name <> nilA) and not found do
         if p^.info.son^.info.name = newname then
            found := true
         else
            p := p^.info.son^.brother;
      if found then
         EnterVar := MakeVar(p^.info.son^.info.son)
      else begin
         v := MakeVar(nil);
         with p^.info do begin
            name := consA;
            arity := 2;
            son := MakeBros(MakeFunc(newname, 1, MakeVar(v)),
                            MakeFunc(nilA, 0, nil))
         end;
         EnterVar := v
      end
   end (* EnterVar *);

begin (* ReadIn *)
   top := 0;
   statetop := 0;
   result := true (* Default value. *);
   vtable := MakeFunc(nilA, 0, nil);
   Push(markL);
   context := outerK; expected := randX; hiprec := MaxPrec;
   ch := ' ';

   repeat
      if FileEnded then begin
         (* End of file - represented by ?- end. *)
         termread := MakeFunc(questionA, 1, MakeFunc(endA, 0, nil));
         context := finalK
      end
      else begin
         errpos := charpos;

         case CharClass[ch] of
            smallC: begin
                  StartAtom;
                  repeat
                     AtomChar(ch); GetChar(ch)
                  until not (CharClass[ch] in [smallC, largeC, digitC]);
                  StowAtom(LookUp)
               end;

            stropC: begin
                  ScanQuote('''');
                  StowAtom(LookUp)
               end;

            quoteC: begin
                  if expected = opX then SyntaxError(needopE);
                  ScanQuote('"');
                  ShiftTerm(ListRep(newatom));
                  expected := opX; loprec := 0
               end;

            specialC: begin
                  lastch := ch; GetChar(ch);
                  if (lastch = '/') and (ch = '*') then begin
                     (* A comment. Comments don't nest. *)
                     GetChar(ch);
                     repeat
                        lastch := ch; GetChar(ch)
                     until (lastch = '*') and (ch = '/');
                     GetChar(ch)
                  end
                  else if (lastch = '~') and
                        (CharClass[ch] = digitC) then begin
                     (* A negative number. *)
                     if expected = opX then SyntaxError(needopE);
                     ShiftTerm(MakeInt(-ScanInt));
                     expected := opX; loprec := 0
                  end
                  else if (lastch = '.') and
                        (CharClass[ch] = spaceC) then begin
                     (* A full stop. *)
                     CheckDelim([outerK], baddotE);
                     termread := Pop;
                     context := finalK;
                  end
                  else begin
                     StartAtom;
                     AtomChar(lastch);
                     while CharClass[ch] = specialC do begin
                        AtomChar(ch); GetChar(ch)
                     end;
                     StowAtom(LookUp)
                  end
               end;

            largeC: begin
                  if expected = opX then SyntaxError(needopE);
                  lastch := ch; GetChar(ch);
                  if (lastch = '_') and not
                        (CharClass[ch] in [smallC, largeC, digitC]) then
                     (* An anonymous variable: replaced by a unique
                        ordinary variable. *)
                     ShiftTerm(MakeVar(nil))
                  else begin
                     StartAtom;
                     AtomChar(lastch);
                     while CharClass[ch] in [smallC, largeC, digitC] do begin
                        AtomChar(ch); GetChar(ch)
                     end;
                     ShiftTerm(EnterVar)
                  end;
                  expected := opX; loprec := 0
               end;

            digitC: begin
                  if expected = opX then SyntaxError(needopE);
                  ShiftTerm(MakeInt(ScanInt));
                  expected := opX; loprec := 0
               end;

            lparC: begin
                  if expected = opX then SyntaxError(needopE);
                  Push(markL);
                  EnterContext(innerK, MaxPrec);
                  GetChar(ch)
               end;

            rparC: begin
                  CheckDelim([innerK, funcK], badketE);
                  case context of
                     innerK: ExitContext(Pop);
                     funcK:  ExitContext(GetFunc)
                  end;
                  GetChar(ch)
               end;

            braC: begin
                  GetChar(ch);
                  if ch = ']' then begin
                     (* The empty list []. *)
                     GetChar(ch);
                     StowAtom(nilA)
                  end
                  else begin
                     if expected = opX then SyntaxError(needopE);
                     Push(markL);
                     EnterContext(listK, SubPrec)
                  end
               end;

            ketC: begin
                  CheckDelim([listK, endlistK], badketE);
                  if context = listK then ShiftTerm(MakeFunc(nilA, 0, nil));
                  ExitContext(GetList);
                  GetChar(ch)
               end;

            lcurlyC: begin
                  GetChar(ch);
                  if ch = '}' then begin
                     (* The 'curly' atom. *)
                     GetChar(ch);
                     StowAtom(curlyA)
                  end
                  else begin
                     if expected = opX then SyntaxError(needopE);
                     Push(markL);
                     EnterContext(curlyK, MaxPrec)
                  end
               end;

            rcurlyC: begin
                  CheckDelim([curlyK], badketE);
                  ExitContext(MakeFunc(curlyA, 1, Pop));
                  GetChar(ch)
               end;

            commaC: begin
                  GetChar(ch);
                  if ch = '.' then begin
                     (* Hope to find ',..', which is punned to '|'. *)
                     GetChar(ch);
                     if ch <> '.' then SyntaxError(badcddE);
                     ch := '|'
                  end
                  else
                     if context in [outerK, innerK, curlyK] then
                        StowAtom(commaA)
                     else begin
                        CheckDelim([funcK, listK], badcommaE);
                        expected := randX; hiprec := SubPrec
                     end
               end;

            cutC: begin
                  GetChar(ch);
                  StowAtom(cutA)
               end;

            semiC: begin
                  GetChar(ch);
                  StowAtom(semiA)
               end;

            vbarC: begin
                  CheckDelim([listK], badvbarE);
                  context := endlistK;
                  expected := randX; hiprec := SubPrec;
                  GetChar(ch)
               end;

            spaceC:
               GetChar(ch);

            weirdC:
               SyntaxError(weirdchE)
         end
      end
   until context = finalK;
666:  (* Come here after a syntax error. *)
   ReadIn := result
end (* ReadIn *);


procedure InitRead;
(* Set up table of character classes. *)

   const n = 28;

   type list = packed array [1 .. n] of char;

   var c: char;

   procedure P(s: list; t: class);
      var i: integer;
   begin
      i := 1;
      while s[i] <> ' ' do begin
         CharClass[s[i]] := t;
         i := i + 1
      end
   end (* P *);

begin
   for c := chr(ordminchar) to chr(ordmaxchar) do
      CharClass[c] := weirdC;
   P('ABCDEFGHIJKLMNOPQRSTUVWXYZ_ ', largeC);
   P('abcdefghijklmnopqrstuvwxyz  ', smallC);
   P('0123456789                  ', digitC);
   P('#$%&*+-./:<=>?@\^`~         ', specialC);
   CharClass[''''] := stropC;
   CharClass['"'] := quoteC;
   CharClass['('] := lparC;
   CharClass[')'] := rparC;
   CharClass['['] := braC;
   CharClass[']'] := ketC;
   CharClass['{'] := lcurlyC;
   CharClass['}'] := rcurlyC;
   CharClass[','] := commaC;
   CharClass['!'] := cutC;
   CharClass[';'] := semiC;
   CharClass['|'] := vbarC;
   CharClass[' '] := spaceC;
   CharClass[chr(9)] := spaceC
end (* InitRead *);



(* ...

[8] WRITEOUT

WriteOut writes a term to a file given by file descriptor.
The style of output depends on the parameter 'style':

   style = 1   'write'
   style = 2   'writeq'
   style = 3   'display'.

... *)


procedure WriteOut(fd: filedesc; x: term; e: env; style: integer);
(* Write a term to file fd. *)

   procedure WriteVar(x: term);
   (* Write a variable. *)
   begin
      PutChar(fd, '_');
      if x^.field = localF then PutChar(fd, '_');
      PutNum(fd, x^.scope)
   end (* WriteVar *);

   procedure WriteTerm(x: term; p: prec; depth: integer);
   (* Write a term with maximum precedence p. *)

      var y: term;

      procedure WriteStand;
      (* Write a complex term in standard notation. *)
         var s: term;
      begin
         with y^.info do begin
            WriteAtom(fd, name, style = 1);
            PutChar(fd, '(');
            WriteTerm(son, SubPrec, depth + 1);
            s := son^.brother;
         end;
         while s <> nil do begin
            PutChar(fd, ','); PutChar(fd, ' ');
            WriteTerm(s, SubPrec, depth + 1);
            s := s^.brother
         end;
         PutChar(fd, ')')
      end (* WriteStand *);

      procedure WriteOp;
      (* Write an operator expression. *)
      begin
         with y^.info do
            case name^.oclass of
               fxO, fyO: begin
                     WriteAtom(fd, name, style = 1);
                     PutChar(fd, ' ');
                     WriteTerm(son, Rprec(name), depth + 1)
                  end;
               xfO, yfO: begin
                     WriteTerm(son, Lprec(name), depth + 1);
                     PutChar(fd, ' ');
                     WriteAtom(fd, name, style = 1)
                  end;
               xfxO, xfyO, yfxO: begin
                     WriteTerm(son, Lprec(name), depth + 1);
                     if (name <> commaA) and (name <> semiA) then
                        PutChar(fd, ' ');
                     WriteAtom(fd, name, (style = 1) or (name = commaA));
                     PutChar(fd, ' ');
                     WriteTerm(son^.brother, Rprec(name), depth + 1)
                  end
            end
      end (* WriteOp *);

      procedure WriteSolo;
      (* Write an atom on its own: parentheses are needed to protect
         prefix operators. *)
         var bracket: boolean;
      begin
         with y^.info do begin
            bracket := (depth > 0) and (name^.oclass in [fxO, fyO]);
            if bracket then PutChar(fd, '(');
            WriteAtom(fd, name, style = 1);
            if bracket then PutChar(fd, ')')
         end
      end (* WriteSolo *);

      procedure WriteList;
      (* Write a list in square bracket notation. *)
         var
            n: integer;
            z: term;
      begin
         PutChar(fd, '[');
         WriteTerm(y^.info.son, SubPrec, depth + 1);
         n := 1;
         z := Deref(y^.info.son^.brother, e);
         while (n < WriteLength) and IsFunc(z, consA, 2) do begin
            PutChar(fd, ','); PutChar(fd, ' ');
            WriteTerm(z^.info.son, SubPrec, depth + 1);
            z := Deref(z^.info.son^.brother, e);
            n := n + 1
         end;
         if not IsFunc(z, nilA, 0) then begin
            if n < WriteLength then begin
               PutChar(fd, ' '); PutChar(fd, '|'); PutChar(fd, ' ');
               WriteTerm(z, SubPrec, depth + 1)
            end
            else begin
               PutChar(fd, ','); PutChar(fd, ' ');
               PutChar(fd, '.'); PutChar(fd, '.'); PutChar(fd, '.')
            end
         end;
         PutChar(fd, ']')
      end (* WriteList *);

      procedure WriteFunc;
      (* Write a complex term. *)
         var bracket: boolean;
      begin
         with y^.info do
            if arity = 0 then WriteSolo
            else if style = 3 then WriteStand
            else if (arity = 1) and (name = curlyA) then begin
               PutChar(fd, '{'); PutChar(fd, ' ');
               WriteTerm(son, MaxPrec, depth + 1);
               PutChar(fd, ' '); PutChar(fd, '}')
            end
            else if (arity = 2) and (name = consA) then WriteList
            else if (arity = 1) and
                       (name^.oclass in [fxO, fyO, xfO, yfO]) or
                    (arity = 2) and
                       (name^.oclass in [xfxO, xfyO, yfxO]) then begin
               bracket := name^.oprec > p;
               if bracket then PutChar(fd, '(');
               WriteOp;
               if bracket then PutChar(fd, ')')
            end
            else
               WriteStand
         end (* WriteFunc *);

   begin (* WriteTerm *)
      if depth = WriteDepth then begin
         PutChar(fd, '.'); PutChar(fd, '.'); PutChar(fd, '.')
      end
      else begin
         y := Deref(x, e);
         with y^, info do
            case tag of
               funcT:
                  WriteFunc;
               intT:
                  if ival >= 0 then
                     PutNum(fd, ival)
                  else begin
                     PutChar(fd, '~'); PutNum(fd, -ival)
                  end;
               varT:
                  WriteVar(y);
               anonT:
                  PutChar(fd, '_')
            end
      end
   end (* WriteTerm *);

begin (* WriteOut *)
   WriteTerm(x, MaxPrec, 0)
end (* WriteOut *);


function SysPred(a: atom): boolean;
(* True if a is a system predicate, i.e. has the 'sys' flag set
   or begins with '$'. *)
begin
   if a^.sys then
      SysPred := true
   else if a^.ident.length > 0 then
      SysPred := stringbuf[a^.ident.index + 1] = '$'
   else
      SysPred := false
end (* SysPred *);


procedure Trace(m: tracemessage; x: term; e: env);
(* Output a trace message. *)
   var y: term;
begin
   y := Deref(x, e);
   (* Don't trace evaluable predicates unless debugging interpreter. *)
   if (flag[debugging] = 1) or not SysPred(y^.info.name) then begin
      case m of
         goalD:   write('GOAL:   ');
         provedD: write('PROVED: ')
      end;
      WriteOut(StandFD, y, e, 2);
      writeln
   end
end (* Trace *);



(* ...

[9]  DATABASE

The clauses which constitute the Prolog program are stored in skeletal
form, with each variable replaced either by an anonymous variable or by
a skeletal reference containing an offset from the base of a frame on
the local stack.

Access to the clauses for a predicate is via the atom entry for the
name.  Space occupied by denied clauses is reclaimed when the clause
is no longer in use: this is determined by a reference count maintained
by the stack mechanism [4].

... *)


function Hash(x: term; e: env): key;
(*
   Hash function for terms.  The value of Hash depends on the root
   of the first argument of the term.  Zero is returned if the first
   argument is a variable.
*)
   const Infinity = 859;
   var y, z: term;
begin
   y := Deref(x, e);
   if y^.info.arity = 0 then
      Hash := Infinity
   else begin
      z := Deref(y^.info.son, e);
      with z^.info do
         case tag of
            funcT: Hash := Infinity + name^.atomno;
            intT: Hash := ival - ord(ival <= 0);
            varT, anonT: Hash := 0
         end
   end
end (* Hash *);


function FindClause(var cl: clptr; x: term; e: env): boolean;
(*
   Advance cl to the first applicable clause. Hash is used to compare
   clause heads with the goal x. If either has hash function zero, a
   variable is present and a match is always possible.
*)
   var
      k: key;
      ok: boolean;
begin
   k := Hash(x, e);
   ok := false;
   while (cl <> nil) and not ok do
      with cl^ do
         if denied or (keyval <> 0) and (k <> 0) and (keyval <> k) then
            cl := chain
         else
            ok := true;
   FindClause := ok
end (* FindClause *);


function AddClause(newclause: term; envp: env; asserta: boolean): boolean;
(*
   Produce a skeleton for newclause and add it to the database.  The new
   clause is added at the front of the clause chain if asserta is true,
   otherwise at the end.  Normally, the result is true, but false
   is returned after an error.
*)

   label
      888 (* Near end of AddClause *);

   var
      heaptop: term;
      varcount: 0 .. MaxVars;
      framesize: integer;
      result: boolean;

      varmap: array [1 .. MaxVars] of
         record
            sourcevar: term;
            case alloc: boolean of
               false: ( firstref: term );
               true:  ( address: integer )
         end;

   procedure AddClError(e: error);
   (* Report an error and return false. *)
   begin
      Report(e);
      result := false;
      goto 888
   end (* AddClError *);

   procedure NewSkel(var x: term);
   (* Create a new node for a skeleton. *)
   begin
      new(x);
      with x^ do begin
         brother := nil;
         chain := heaptop;
         field := heapF;
         scope := 0
      end;
      heaptop := x
   end (* NewSkel *);

   function SkelVar(v: term): term;
   (*
      Produce a skeleton for a variable v. When the first occurrence of
      v is encountered, it is tentatively translated as an anonymous
      variable, and a pointer to this variable is stored in the
      'varmap' entry. If a second occurrence is encountered, the
      anonymous variable is changed to a skeletal reference.
   *)
      var
         n: 0 .. MaxVars;
         w: term;
         found: boolean;
   begin
      n := 0;
      found := false;
      while (n <> varcount) and not found do begin
         n := n + 1;
         found := varmap[n].sourcevar = v
      end;
      if found then
         (* This is not the first occurrence. *)
         with varmap[n] do begin
            if not alloc then begin
               (* This is the second occurrence -
                  allocate space on the local stack. *)
               framesize := framesize + 1;
               with firstref^.info do begin
                  tag := skelT;
                  offset := framesize
               end;
               alloc := true;
               address := framesize
            end;
            NewSkel(w);
            with w^.info do begin
               tag := skelT;
               offset := address
            end
         end
      else begin
         (* This is the first occurrence - make an anonymous
            variable and keep a pointer to it. *)
         if varcount >= MaxVars then AddClError(nvarsE);
         varcount := varcount + 1;
         NewSkel(w);
         w^.info.tag := anonT;
         with varmap[varcount] do begin
            sourcevar := v;
            alloc := false;
            firstref := w
         end
      end;
      SkelVar := w
   end (* SkelVar *);

   function Skeleton(x: term; depth: integer): term;
   (* Produce a skeleton for x. *)
      var y, z: term;

      function SkelArgs(s: term): term;
      (* Produce a skeleton for the arguments of a functor node. *)
         var t, u, v: term;
      begin
         if s = nil then
            SkelArgs := nil
         else begin
            u := Skeleton(s, depth + 1);
            t := s^.brother;
            v := u;
            while t <> nil do begin
               v^.brother := Skeleton(t, depth + 1);
               t := t^.brother;
               v := v^.brother
            end;
            SkelArgs := u
         end
      end (* SkelArgs *);

   begin (* Skeleton *)
      if depth > MaxDepth then AddClError(depthE);
      y := Deref(x, envp);
      case y^.info.tag of
         funcT: begin
               NewSkel(z);
               with z^.info do begin
                  tag := funcT;
                  name := y^.info.name;
                  arity := y^.info.arity;
                  son := SkelArgs(y^.info.son)
               end
            end;
         intT: begin
               NewSkel(z);
               with z^.info do begin
                  tag := intT;
                  ival := y^.info.ival
               end
            end;
         varT:
            z := SkelVar(y);
         anonT: begin
               NewSkel(z);
               z^.info.tag := anonT
            end
      end;
      Skeleton := z
   end (* Skeleton *);

   function SkelCall(x: term): term;
   (* Produce a skeleton for a goal in a clause body. A variable
      X is mapped onto call(X). *)
      var y, z: term;
   begin
      y := Deref(x, envp);
      case y^.info.tag of
         funcT:
            SkelCall := Skeleton(y, 0);
         varT: begin
               NewSkel(z);
               with z^.info do begin
                  tag := funcT;
                  name := callA;
                  arity := 1;
                  son := SkelVar(y)
               end;
               SkelCall := z
            end;
         intT, anonT:
            AddClError(assertE)
      end
   end (* SkelCall *);

   function SkelHead(x: term): term;
   (* Produce a skeleton for a clause head. *)
      var y: term;
   begin
      y := Deref(x, envp);
      if y^.info.tag <> funcT then AddClError(assertE);
      SkelHead := Skeleton(y, 0)
   end (* SkelHead *);

   function SkelBody(x: term; depth: integer): term;
   (* Produce a skeleton for a clause body. *)
      var y: term;
   begin
      if depth > MaxDepth then AddClError(depthE);
      y := Deref(x, envp);
      if IsFunc(y, commaA, 2) then
         SkelBody := MakeBros(SkelCall(y^.info.son),
                              SkelBody(y^.info.son^.brother, depth + 1))
      else
         SkelBody := SkelCall(y)
   end (* SkelBody *);

   procedure PlugA(cl: clptr; var cp: clptr; prev: clptr);
   (* Insert cl at the start of chain cp, making its backchain
      point to prev. *)
   begin
      cl^.chain := cp;
      cl^.backchain := prev;
      if cp <> nil then cp^.backchain := cl;
      cp := cl
   end (* PlugA *);

   procedure PlugZ(cl: clptr; var cp: clptr);
   (* Insert clause cl at the end of chain cp. *)
      var p: clptr;
   begin
      if cp = nil then
         PlugA(cl, cp, nil)
      else begin
         p := cp;
         while p^.chain <> nil do p := p^.chain;
         PlugA(cl, p^.chain, p)
      end
   end (* PlugZ *);

   function MakeClause(h, b: term; k: key): clptr;
   (* Make a clause with head h and body b, where the head hashes to k. *)
      var cl: clptr;
   begin
      new(cl);
      with cl^ do begin
         head := h;
         body := b;
         nvars := framesize;
         denied := false;
         refcount := 0;
         keyval := k;
         chain := nil;
         backchain := nil;
         heapchain := heaptop
      end;
      MakeClause := cl
   end (* MakeClause *);

   procedure AddCl(x: term);
   (* The guts of AddClause. *)
      var
         y, head, body: term;
         keyval: key;
   begin
      y := Deref(x, envp);
      if IsFunc(y, questionA, 1) or IsFunc(y, gramA, 2) then
         AddClError(assertE);
      if IsFunc(y, arrowA, 2) then begin
         head := SkelHead(y^.info.son);
         body := SkelBody(y^.info.son^.brother, 0);
         keyval := Hash(y^.info.son, envp)
      end
      else begin
         head := SkelHead(y);
         body := nil;
         keyval := Hash(y, envp)
      end;
      with head^.info.name^ do begin
         if (flag[sysmode] = 0) and (flag[debugging] = 0) and sys or
              (pclass = evalP) then
            AddClError(sysprocE);
         if (flag[sysmode] = 1) then sys := true;
         if asserta then
            PlugA(MakeClause(head, body, keyval), proc, nil)
         else
            PlugZ(MakeClause(head, body, keyval), proc)
      end
   end (* AddCl *);


begin (* AddClause *)
   heaptop := nil;
   varcount := 0;
   framesize := 0;
   result := true (* Default value. *);
   AddCl(newclause);
888: (* Come here after an error. *)
   AddClause := result
end (* AddClause *);


procedure DeleteClause (* (cl: clptr) *);
(* Delete clause cl. *)
begin
   if flag[debugging] = 1 then writeln('DeleteClause');
   with cl^ do begin
      if backchain <> nil then
         backchain^.chain := chain
      else
         head^.info.name^.proc := chain;
      if chain <> nil then chain^.backchain := backchain;
      FreeChain(heapchain, nil)
   end;
   dispose(cl)
end (* DeleteClause *);



(* ...

[10]  UNIFY

Unify implements the unification algorithm, which finds the most
general common instance of a pair of terms. It performs the matching
substitution by introducing variable bindings.  As tradition dictates,
no occurrence check is made before variable bindings are introduced.
Unify is used by Execute [11] to match goals against the heads of
clauses in the database.

... *)


function Unify(x1, x2: term; e1, e2: env; depth: integer): boolean;
(* Unify x1 and x2.  Perform the matching substitution
   by binding variables. *)

   var y1, y2: term;

   function UnifyArgs(s1, s2: term): boolean;
   (* Unify the arguments of a pair of functor nodes. *)
      var
         t1, t2: term;
         ok: boolean;
   begin
      t1 := s1; t2 := s2;
      ok := true;
      while (t1 <> nil) and ok do begin
         ok := Unify(t1, t2, e1, e2, depth + 1);
         t1 := t1^.brother; t2 := t2^.brother
      end;
      UnifyArgs := ok
   end (* UnifyArgs *);

begin (* Unify *)
   if depth > MaxDepth then ExecError(depthE);
   y1 := Deref(x1, e1);
   y2 := Deref(x2, e2);
   case Uaction[y1^.info.tag, y2^.info.tag] of
      funcU:
         if (y1^.info.name = y2^.info.name) and
            (y1^.info.arity = y2^.info.arity) then
            Unify := UnifyArgs(y1^.info.son, y2^.info.son)
         else
            Unify := false;
      intU:
         Unify := y1^.info.ival = y2^.info.ival;
      VTbindU: begin
            Bind(y1, y2, e1, e2, 0);
            TrailVar(y1);
            Unify := true
         end;
      TVbindU: begin
            Bind(y2, y1, e2, e1, 0);
            TrailVar(y2);
            Unify := true
         end;
      VVbindU: begin
            BindVars(y1, y2);
            Unify := true
         end;
      succeedU:
         Unify := true;
      failU:
         Unify := false
   end
end (* Unify *);


procedure InitUnify;
(* Set up table of actions for Unify. *)
begin
   Uaction[funcT, funcT] := funcU;
   Uaction[intT,  intT ] := intU;
   Uaction[varT,  funcT] := VTbindU;
   Uaction[varT,  intT ] := VTbindU;
   Uaction[funcT, varT ] := TVbindU;
   Uaction[intT,  varT ] := TVbindU;
   Uaction[varT,  varT ] := VVbindU;
   Uaction[anonT, funcT] := succeedU;
   Uaction[anonT, intT ] := succeedU;
   Uaction[anonT, varT ] := succeedU;
   Uaction[funcT, anonT] := succeedU;
   Uaction[intT,  anonT] := succeedU;
   Uaction[varT,  anonT] := succeedU;
   Uaction[anonT, anonT] := succeedU;
   Uaction[funcT, intT ] := failU;
   Uaction[intT,  funcT] := failU
end (* InitUnify *);



(* ...

[11]  EXECUTE

Execute is the finite state control of the abstract Prolog machine. It
executes the goal 'goalp' by manipulating the local and global stacks
[4], and uses Unify [10] to match goals against clauses from the
database [9]. CallEvalPred [12] handles evaluable predicates.

... *)


function CallEvalPred(call: term; callenv: env; routine: evalpred;
                                     arity: integer): boolean;
   forward;


function Execute(goalp: term; goalenv: env): boolean;
(* Execute a goal. *)
   var
      callp: term;
      envp, callenv, baseenv: env;
      clausep: clptr;
      state: (callQ, procQ, bodyQ, returnQ, failQ, finalQ);
begin
   callp := Deref(goalp, goalenv);
   callenv := goalenv;
   baseenv := envtop;
   state := callQ;

   repeat
      case state of
         callQ:
            (* 'callp' holds a goal and 'callenv' its environment. *)
            begin
               if TestInt then Abort;
               if flag[tracing] = 1 then Trace(goalD, callp, callenv);
               with callp^.info.name^ do
                  case pclass of
                     normP: begin
                           clausep := proc;
                           state := procQ
                        end;
                     evalP:
                        if CallEvalPred(callp, callenv, routine, arity) then
                           state := returnQ
                        else
                           state := failQ
                  end
            end;

         procQ:
            (* 'clausep' points to a chain of untried clauses
               for the goal in 'callp'. *)
            if FindClause(clausep, callp, callenv) then
               with clausep^ do begin
                  NewEnv(envp, callp, callenv, clausep, nvars);
                  if chain <> nil then choicepoint := envp;
                  if Unify(head, callp, envp, callenv, 0) then begin
                     callp := body;
                     callenv := envp;
                     state := bodyQ
                  end
                  else
                     state := failQ
               end
            else
               state := failQ;

         bodyQ:
            (* 'callp' points to a chain of uncalled goals in the body
               of some clause, and 'callenv' to the environment for
               the clause. *)
            if callp = nil then begin
               envp := callenv;
               AccEnv(envp, callp, callenv, clausep);
               if envp > choicepoint then DisposeEnv;
               if flag[tracing] = 1 then Trace(provedD, callp, callenv);
               state := returnQ
            end
            else
               state := callQ;

         returnQ:
            (* The subgoal in 'callp' has just succeeded. *)
            if callenv > goalenv then begin
               callp := callp^.brother;
               state := bodyQ
            end
            else begin
               Execute := true;
               state := finalQ
            end;

         failQ:
            (* Failure has occurred.  'choicepoint' is the newest
               environment with a nondeterminate choice. *)
            if choicepoint > baseenv then begin
               AccEnv(choicepoint, callp, callenv, clausep);
               KillStacks(choicepoint - 1);
               clausep := clausep^.chain;
               state := procQ
            end
            else begin
               Execute := false;
               state := finalQ
            end
      end
   until state = finalQ;
end (* Execute *);



(* ...

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

... *)


function IntResult(x: term; e: env; i: integer): boolean;
(*
   Specialized unification algorithm for returning integer results.
   IntResult(x, e, i) is equivalent to Unify(x, MakeInt(i), e, 0, 0)
   but avoids allocating a global node.
*)
   var y: term;
begin
   y := Deref(x, e);
   with y^.info do
      case tag of
         funcT:
            IntResult := false;
         intT:
            IntResult := ival = i;
         varT: begin
               tag := intT;
               ival := i;
               TrailVar(y);
               IntResult := true
            end;
         anonT:
            IntResult := true
      end
end (* IntResult *);


function CallEvalPred (* (call: term; callenv: env; routine: evalpred;
                                        arity: integer): boolean *);
(* Call an evaluable predicate. *)

   var
      result: boolean;
      argval: array [1 .. MaxEvalArity] of term;

   procedure ShowContext;
   (* Output context information after an error. *)
      var
         goal, parent: term;
         goalenv, parentenv: env;
         cl: clptr;
   begin
      goal := call; goalenv := callenv;
      AccEnv(goalenv, parent, parentenv, cl);
      while (parentenv > 0) and parent^.info.name^.sys and
            not IsFunc(parent, callA, 1) do begin
         goal := parent; goalenv := parentenv;
         AccEnv(goalenv, parent, parentenv, cl)
      end;
      write('- in call: ');
      WriteOut(StandFD, goal, goalenv, 2);
      writeln('.');
      if not parent^.info.name^.sys then begin
         write('- called from ');
         WriteAtom(StandFD, parent^.info.name, false);
         writeln('(', parent^.info.arity: 1, ').')
      end
   end (* ShowContext *);

   procedure PredError(e: error);
   (* Report an error in a built-in predicate. *)
   begin
      Report(e);
      ShowContext;
      Abort
   end (* PredError *);

   procedure GetArgs;
   (* Fill in argval with the dereferenced arguments. *)
      var
         i: integer;
         x, a: term;
   begin
      x := Deref(call, callenv);
      if arity <> x^.info.arity then PredError(arityE);
      a := x^.info.son;
      for i := 1 to arity do begin
         argval[i] := Deref(a, callenv);
         a := a^.brother
      end
   end (* GetArgs *);

   function Evaluate(x: term; depth: integer): integer;
   (* Evaluate x as an arithmetic expression. *)
      var
         y: term;
         a, b: integer;
   begin
      if depth > MaxDepth then PredError(depthE);
      y := Deref(x, callenv);
      with y^.info do
         case tag of
            funcT:
               if arity = 2 then
                  if name = consA then
                     Evaluate := Evaluate(son, depth + 1)
                  else begin
                     a := Evaluate(son, depth + 1);
                     b := Evaluate(son^.brother, depth + 1);
                     if      name = plusA  then Evaluate := a + b
                     else if name = minusA then Evaluate := a - b
                     else if name = timesA then Evaluate := a * b
                     else if name = divideA then begin
                        if b = 0 then PredError(divideE);
                        Evaluate := a div b
                     end
                     else if name = modA then begin
                        if b = 0 then PredError(divideE);
                        Evaluate := a mod b
                     end
                     else
                        PredError(badexpE)
                  end
               else if (name = negA) and (arity = 1) then
                  Evaluate := - Evaluate(son, depth + 1)
               else
                  PredError(badexpE);
            intT:
               Evaluate := ival;
            varT, anonT:
               PredError(varexpE)
         end
   end (* Evaluate *);

   procedure DoCall;
   (* Evaluable predicate 'call'. This code is tricky. *)
      var
         x: term;
         e1: env;
   begin
      if argval[1]^.info.tag <> funcT then PredError(argsE);
      NewEnv(e1, call, callenv, nil, 1);
      x := EnvRef(1, e1);
      Bind(x, argval[1], e1, callenv, 0);
      result := Execute(x, e1);
      if e1 > choicepoint then DisposeEnv
   end (* DoCall *);

   procedure DoRead;
   (* Hidden evaluable predicate '$read'. *)
      var x, v: term;
   begin
      if current[inZ] = StandFD then flush(stdout);
      if not ReadIn(x, v) then
         result := false
      else if not Unify(argval[1], x, callenv, 0, 0) then
         result := false
      else
         result := Unify(argval[2], v, callenv, 0, 0)
   end (* DoRead *);

   procedure DoWrite;
   (* Hidden evaluable predicate 'write'. *)
   begin
      if argval[2]^.info.tag <> intT then InternalError(11);
      WriteOut(current[outZ], argval[1], callenv, argval[2]^.info.ival)
   end (* DoWrite *);

   procedure DoGet0;
   (* Evaluable predicate 'get0'. *)
      var ch: char;
   begin
      if current[inZ] = StandFD then flush(stdout);
      GetChar(ch);
      result := IntResult(argval[1], callenv, ord(ch))
   end (* DoGet0 *);

   procedure DoPut;
   (* Evaluable predicate 'put'. *)
      var ch: integer;
   begin
      ch := Evaluate(argval[1], 0);
      if (ch < ordminchar) or (ch > ordmaxchar) then PredError(badcharE);
      PutChar(current[outZ], chr(ch))
   end (* DoPut *);

   procedure DoOp;
   (* Evaluable predicate '$op'. *)
      var
         p: integer;
         a: atom;
         f: optype;
   begin
      if (argval[1]^.info.tag <> intT) or not IsAtom(argval[2]) or
         not IsAtom(argval[3]) then PredError(argsE);
      p := argval[1]^.info.ival;
      a := argval[2]^.info.name;
      if (p < 1) or (p > MaxPrec) then PredError(argsE);
      if      a = fxA  then f := fxO
      else if a = fyA  then f := fyO
      else if a = xfA  then f := xfO
      else if a = yfA  then f := yfO
      else if a = xfxA then f := xfxO
      else if a = xfyA then f := xfyO
      else if a = yfxA then f := yfxO
      else PredError(argsE);
      with argval[3]^.info.name^ do begin
         oclass := f;
         oprec := p
      end
   end (* DoOp *);

   procedure DoName;
   (* Evaluable predicate 'name'. *)
      var
         x, y: term;
         ch: integer;
   begin
      if IsAtom(argval[1]) then
         result :=
            Unify(argval[2], ListRep(argval[1]^.info.name^.ident),
               callenv, 0, 0)
      else if argval[1]^.info.tag in [varT, anonT] then begin
         StartAtom;
         x := argval[2];
         while IsFunc(x, consA, 2) do begin
            y := Deref(x^.info.son, callenv);
            if y^.info.tag <> intT then PredError(argsE);
            ch := y^.info.ival;
            if (ch < ordminchar) or (ch > ordmaxchar) then
               PredError(badcharE);
            AtomChar(chr(ch));
            x := Deref(x^.info.son^.brother, callenv)
         end;
         if not IsFunc(x, nilA, 0) then PredError(argsE);
         result := Unify(argval[1], MakeFunc(LookUp, 0, nil), callenv, 0, 0)
      end
      else
         PredError(argsE)
   end (* DoName *);

   procedure DoAddcl;
   (* Hidden evaluable predicate '$addcl'. *)
   begin
      if argval[2]^.info.tag <> intT then InternalError(13);
      if not AddClause(argval[1], callenv,
                       argval[2]^.info.ival = 1) then begin
         ShowContext;
         Abort
      end
   end (* DoAddcl *);

   procedure DoFunctor;
   (* Evaluable predicate 'functor'. *)
      var
         x: term;
         i, m: integer;
   begin
      case argval[1]^.info.tag of
         funcT:
            if not IntResult(argval[3], callenv, argval[1]^.info.arity) then
               result := false
            else
               result := Unify(argval[2],
                  MakeFunc(argval[1]^.info.name, 0, nil), callenv, 0, 0);
         intT:
            if not IntResult(argval[3], callenv, 0) then
               result := false
            else
               result := IntResult(argval[2], callenv, argval[1]^.info.ival);
         varT, anonT: begin
               if argval[3]^.info.tag <> intT then PredError(argsE);
               m := argval[3]^.info.ival;
               if IsAtom(argval[2]) and (m >= 0) then begin
                  x := nil;
                  for i := m downto 1 do
                     x := MakeBros(MakeVar(nil), x);
                  result :=
                     Unify(argval[1], MakeFunc(argval[2]^.info.name, m, x),
                        callenv, 0, 0)
               end
               else if (argval[2]^.info.tag = intT) and (m = 0) then
                  result :=
                     IntResult(argval[1], callenv, argval[2]^.info.ival)
               else
                  PredError(argsE)
         end
      end
   end (* DoFunctor *);

   procedure DoArg;
   (* Evaluable predicate 'arg'. *)
      var
         x: term;
         i, n: integer;
   begin
      if (argval[1]^.info.tag <> intT) or (argval[2]^.info.tag <> funcT) then
         result := false
      else begin
         n := argval[1]^.info.ival;
         if (n < 1) or (n > argval[2]^.info.arity) then
            result := false
         else begin
            x := argval[2]^.info.son;
            for i := 2 to n do x := x^.brother;
            result := Unify(argval[3], x, callenv, callenv, 0)
         end
      end
   end (* DoArg *);

   (* The next four functions are used for 'clause', 'deny' and
      'listing'.  The code here is extremely tricky. *)

   procedure DoClEnv;
   (* Hidden evaluable predicate '$clenv'. *)
      var e1: env;
   begin
      if argval[1]^.info.tag <> funcT then PredError(argsE);
      with argval[1]^.info.name^ do begin
         if (flag[debugging] = 0) and sys or (pclass = evalP) then
            PredError(sysprocE);
         NewEnv(e1, nil, 0, proc, 0)
      end;
      result := IntResult(argval[2], callenv, e1)
   end (* DoClEnv *);

   procedure DoGetCl;
   (* Hidden evaluable predicate '$getcl'. *)
      var
         e1, e2: env;
         cl: clptr;
   begin
      if (argval[1]^.info.tag <> funcT) or
         (argval[2]^.info.tag <> intT) or
         (argval[3]^.info.tag <> varT) or
         (argval[4]^.info.tag <> varT) then InternalError(2);
      e1 := argval[2]^.info.ival;
      cl := display[e1].Fclause;
      if FindClause(cl, argval[1], callenv) then begin
         with cl^ do begin
            NewEnv(e2, nil, 0, nil, nvars);
            Bind(argval[3], head, callenv, e2, 0);
            TrailVar(argval[3]);
            GetBody(argval[4], body, callenv, e2);
            TrailVar(argval[4]);
            DisposeEnv
         end;
         ChangeClause(e1, cl);
         result := true
      end
      else
         result := false
   end (* DoGetCl *);

   procedure DoAdvCl;
   (* Hidden evaluable predicate '$advcl'. *)
      var
         e1: env;
         cl: clptr;
   begin
      if argval[1]^.info.tag <> intT then InternalError(3);
      e1 := argval[1]^.info.ival;
      cl := display[e1].Fclause;
      if cl = nil then InternalError(4);
      ChangeClause(e1, cl^.chain)
   end (* DoAdvCl *);

   procedure DoZap;
   (* Hidden evaluable predicate '$zap'. *)
      var
         e1: env;
         cl: clptr;
   begin
      if argval[1]^.info.tag <> intT then InternalError(5);
      e1 := argval[1]^.info.ival;
      cl := display[e1].Fclause;
      if cl = nil then InternalError(6);
      cl^.denied := true;
   end (* DoZap *);

   procedure DoSelect(dir: inout);
   (* Evaluable predicates 'see' and 'tell'. *)
      var e: error;
   begin
      if not IsAtom(argval[1]) then PredError(argsE);
      if not SelectFile(argval[1]^.info.name, dir, e) then
         PredError(e)
   end (* DoSelect *);

   procedure DoClose;
   (* Evaluable predicate 'close'. *)
   begin
      if not IsAtom(argval[1]) then PredError(argsE);
      DropFile(argval[1]^.info.name)
   end (* DoClose *);

   procedure DoUcode;
   (* Hidden evaluable predicate '$ucode'. *)
   begin
      if not IsAtom(argval[1]) then PredError(argsE);
      result := UserCode(argval[1]^.info.name^.ident)
   end (* DoUcode *);

   procedure DoNonsp;
   (* Hidden evaluable prodicate '$nonsp'. *)
   begin
      if argval[1]^.info.tag <> intT then InternalError(12);
      result := CharClass[chr(argval[1]^.info.ival)] <> spaceC
   end (* DoNonsp *);

   procedure DoFlag;
   (* Hidden evaluable predicate '$flag'. *)
   begin
      if argval[1]^.info.tag <> intT then InternalError(9);
      result :=
         IntResult(argval[2], callenv, flag[argval[1]^.info.ival])
   end (* DoFlag *);

   procedure DoSetflg;
   (* Hidden evaluable predicate '$setflg'. *)
   begin
      if (argval[1]^.info.tag <> intT) or
         (argval[2]^.info.tag <> intT) then InternalError(10);
      flag[argval[1]^.info.ival] := argval[2]^.info.ival
   end (* DoSetflg *);


begin (* CallEvalPred *)
   GetArgs;
   result := true  (* Default value. *);
   case routine of
      cutR:     Cut(callenv);
      callR:    DoCall;
      readR:    DoRead;
      writeR:   DoWrite;
      get0R:    DoGet0;
      putR:     DoPut;
      nlR:      PutLn(current[outZ]);
      eolnR:    result := LineEnded;
      eofR:     result := FileEnded;
      opR:      DoOp;
      abortR:   Abort;
      haltR:    Crash;
      flagR:    DoFlag;
      setflgR:  DoSetflg;
      atomR:    result := IsAtom(argval[1]);
      integerR: result := argval[1]^.info.tag = intT;
      varR:     result := argval[1]^.info.tag in [varT, anonT];
      nameR:    DoName;
      isR:      result :=
                   IntResult(argval[1], callenv, Evaluate(argval[2], 0));
      ltR:      result :=
                   Evaluate(argval[1], 0) < Evaluate(argval[2], 0);
      addclR:   DoAddcl;
      functorR: DoFunctor;
      argR:     DoArg;
      clenvR:   DoClEnv;
      getclR:   DoGetCl;
      advclR:   DoAdvCl;
      zapR:     DoZap;
      seeR:     DoSelect(inZ);
      seeingR:  result :=
                   Unify(MakeFunc(CurrFile(inZ), 0, nil),
                      argval[1], 0, callenv, 0);
      tellR:    DoSelect(outZ);
      tellingR: result :=
                   Unify(MakeFunc(CurrFile(outZ), 0, nil),
                      argval[1], 0, callenv, 0);
      closeR:   DoClose;
      ucodeR:   DoUcode;
      nonspR:   DoNonsp
   end;
   CallEvalPred := result
end (* CallEvalPred *);



(* ...

[13]  TOP LEVEL

TopLevel calls the top level dialogue defined by the predicate '$top' in
the library file.  Before the top level is entered for the first
time, the library file itself is read and processed by the procedure
ReadLib.

... *)


procedure ReadLib;
(* Read and process the library file. *)
   var
      x, v: term;
      e: env;
      success: boolean;
begin
   flag[sysmode] := 1;
   CharClass['$'] := smallC;
   SelectLib;
   while not FileEnded do begin
      choicepoint := 0;
      NewEnv(e, nil, 0, nil, 0);
      if ReadIn(x, v) then begin
         if flag[debugging] = 1 then begin
            WriteOut(StandFD, x, 0, 2);
            writeln
         end;
         if IsFunc(x, questionA, 1) then
            success := Execute(x^.info.son, 0)
         else
            success := AddClause(x, 0, false)
      end;
      KillStacks(0)
   end;
   flag[sysmode] := 0;
   CharClass['$'] := specialC;
   DropLib
end (* ReadLib *);


procedure InitFlags;
(* Zero the flag vector. *)
   var i: integer;
begin
   for i := 1 to FlagSize do flag[i] := 0;
end (* InitFlags *);


procedure Initialise;
(* Initialise the Prolog system. *)
begin
   InitAtoms;
   InitFiles;
   InitTrail;
   InitStacks;
   InitRead;
   InitUnify;
   InitFlags;
   ReadLib;
   failA^.sys := true;
   repeatA^.proc^.chain := repeatA^.proc;
   andG := commaA^.proc;
   or1G := semiA^.proc^.chain^.chain;
   or2G := or1G^.chain
end (* Initialise *);


procedure ConsultArgs;
(* Consult files named as arguments. *)
   var
      i, j: integer;
      arg: stringarg;
      success: boolean;
      e: env;
begin
   for i := 1 to argc - 1 do begin
      argv(i, arg);
      StartAtom;
      j := 1;
      while arg[j] <> ' ' do begin
         AtomChar(arg[j]);
         j := j + 1
      end;
      NewEnv(e, nil, 0, nil, 0);
      success :=
         Execute(MakeFunc(callA, 1, MakeFunc(consultA, 1,
                       MakeFunc(LookUp, 0, nil))), 0);
      KillStacks(0)
   end
end (* ConsultArgs *);


procedure TopLevel;
(* Call the top level. *)
   var
      e: env;
      success: boolean;
begin
   flag[breaklevel] := 0;
   choicepoint := 0;
   NewEnv(e, nil, 0, nil, 0);
   success := Execute(MakeFunc(topA, 0, nil), 0)
end (* TopLevel *);



begin (* Prolog *)
   writeln(Version);
   Initialise;
   CatchInt;
   ConsultArgs;
100: (* Come here after execution error. *)
   TopLevel;
999: (* Come here after fatal error. *)
   FreeFiles;
   writeln;
   writeln('[Leaving Prolog]')
end (* Prolog *).
