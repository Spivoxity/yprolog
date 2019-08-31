const ArgLen = 80;

type stringarg = packed array [1 .. 80] of char;

function CanRead(var name: stringarg): boolean;
   external;

function SysCmd(var name: stringarg): boolean;
   external;

function GetEnv(var name, value: stringarg): boolean;
   external;
