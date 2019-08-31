#
/* CatchInt - catch signals and set intflag. */

#define SIGINT 2

int intflag = 0;

handler()
{
        signal(SIGINT, handler);
        intflag = 1;
}


CatchInt()
{
        intflag = 0;
        signal(SIGINT, handler);
}

TestInt()
{
        int f = intflag;
	intflag = 0;
  	return f;
}
