#
/*

        C subroutines for Prolog system.

*/
#include "cdefs.h"

#define R_OK 4

extern char *getenv();


/* Canread - test if file exists with read permission. */
CanRead(name)
char *name;
{
        TrimSpaces(name);
        return access(name, R_OK) >= 0;
}


/* SysCmd - execute shell command. */
SysCmd(cmd)
char *cmd;
{
        TrimSpaces(cmd);
        return system(cmd) == 0;
}


/* GetEnv - access environment. */
GetEnv(name, value)
char *name, *value;
{
        int k;
        char *result;

        TrimSpaces(name);
        if ((result = getenv(name)) == 0)
                return 0;

        for (k = 0; result[k] != '\0'; k++)
                value[k] = result[k];
        for (; k < ArgLen; k++)
                value[k] = ' ';
        return 1;
}

TrimSpaces(s)
char *s;
{
        int k = ArgLen - 1;

        while (k >= 0 && s[k] == ' ')
                k--;
        s[k+1] = '\0';
}
