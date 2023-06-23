type IsEmptyString<S> = S extends Exclude<Primitives, string>
  ? S
  : S extends `${Alphabets}${string}`
  ? false
  : true;

type Primitives =
  | number
  | string
  | symbol
  | null
  | undefined
  | bigint
  | boolean;

type Alphabets = _Alphabet | Uppercase<_Alphabet>;

type _Alphabet =
  | "a"
  | "b"
  | "c"
  | "d"
  | "e"
  | "f"
  | "g"
  | "h"
  | "i"
  | "j"
  | "k"
  | "l"
  | "m"
  | "n"
  | "o"
  | "p"
  | "q"
  | "r"
  | "s"
  | "t"
  | "u"
  | "v"
  | "w"
  | "x"
  | "y"
  | "z";


type _0 = IsEmptyString<"fcwc">;
type _1 = IsEmptyString<"E">;
type _2 = IsEmptyString<"">;
