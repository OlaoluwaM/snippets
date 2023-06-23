// Types A and B are the same type
type Equal<A, B> = [A] extends [B] ? ([B] extends [A] ? true : false) : false;

type Not<T> = T extends true ? false : true;

// A is assignable to B but they are not the same type
type Assignable<A, B> = [A] extends [B]
  ? [B] extends [A]
    ? false
    : true
  : false;
