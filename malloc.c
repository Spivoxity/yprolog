#
/*

        malloc, free - storage allocation.

*/

#include "malloc.h"

union store allocs[2];  /*initial arena*/
union store *allocp;    /*search ptr*/
union store *alloct;    /*arena top*/

char *
malloc(nbytes)
unsigned nbytes;
{
        register union store *p, *q;
        register int nw;
        static int temp;        /*coroutines assume no auto*/

        if(allocs[0].ptr == 0) {        /*first time*/
                allocs[0].ptr = setbusy(&allocs[1]);
                allocs[1].ptr = setbusy(&allocs[0]);
                alloct = &allocs[1];
                allocp = &allocs[0];
        }
        nw = (nbytes+WORD+WORD-1)/WORD;
        for(p=allocp; ; ) {
                for(temp=0; ; ) {
                        if(!testbusy(p->ptr)) {
                                while(!testbusy((q=p->ptr)->ptr)) {
                                        allocp = p;
                                        p->ptr = q->ptr;
                                }
                                if(q >= p+nw && p+nw >= p)
                                        goto found;
                        }
                        q = p;
                        p = clearbusy(p->ptr);
                        if(p <= q) {
                                if(q != alloct || p != allocs)
                                        return(NULL);
                                if(++temp > 1)
                                        break;
                        }
                }
                temp = ((nw+BLOCK/WORD)/(BLOCK/WORD))*(BLOCK/WORD);
                q = (union store *)sbrk(0);
                if(q+temp < q) {
                        return(NULL);
                }
                q = (union store *)sbrk(temp*WORD);
                if((INT)q == -1) {
                        return(NULL);
                }
                alloct->ptr = q;
                if(q != alloct+1)
                        alloct->ptr = setbusy(alloct->ptr);
                alloct = q->ptr = q+temp-1;
                alloct->ptr = setbusy(allocs);
        }
found:
        allocp = p + nw;
        if(q > allocp) {
                allocp->ptr = p->ptr;
        }
        p->ptr = setbusy(allocp);
        return((char*)(p+1));
}

/*      freeing strategy tuned for LIFO allocation */

void
free(ap)
register char *ap;
{
        register union store *p = (union store *)ap;

        allocp = --p;
        p->ptr = clearbusy(p->ptr);
}
