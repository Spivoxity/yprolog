#define INT int
#define ALIGN int
#define NALIGN 1
#define WORD sizeof(union store)
#define BLOCK 1024      /* a multiple of WORD*/
#define BUSY 1
#define NULL 0
#define testbusy(p) ((INT)(p)&BUSY)
#define setbusy(p) (union store *)((INT)(p)|BUSY)
#define clearbusy(p) (union store *)((INT)(p)&~BUSY)

union store {
        union store *ptr;
        ALIGN dummy[NALIGN];
};

extern char *sbrk();

extern union store allocs[2];   /*initial arena*/
extern union store *allocp;     /*search ptr*/
extern union store *alloct;     /*arena top*/
