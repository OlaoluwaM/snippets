namespace smartGet {
  declare function get<
    T extends Record<string | number, any>,
    S extends string
  >(obj: T, path: S): Get<T, S>;

  type Get<
    T extends Record<string | number, any>,
    S extends string | ParsedPathStr
  > =
  T extends Primitives
    ? T
    : S extends string
        ? Get<T, ParsePathStr<S>>
        : S extends [infer FirstProp, ...infer Rest]
            ? Get<T[As<FirstProp, string | number>], As<Rest, ParsedPathStr>>
            : T;

  type ParsedPathStr = (string | number)[];

  type Primitives =
    | string
    | number
    | bigint
    | symbol
    | boolean
    | null
    | undefined;

  type As<T, U> = T extends U ? T : never;

  type ParsePathStr<PathStr extends string> = ToNumFromStringMap<
    FilterEmptyStrings<
      Split<ReplaceSeparatorsWithSpace<PathStr, ObjPathSeparators>, " ">
    >
  >;

  type Split<
    Str,
    Separator extends string
  > = Str extends `${infer First}${Separator}${infer Rest}`
    ? [First, ...Split<Rest, Separator>]
    : [Str];

  type ReplaceSeparatorsWithSpace<
    S extends string,
    Separators extends string,
    O extends string = ""
  > = S extends `${infer First}${infer Rest}`
    ? First extends Separators
      ? ReplaceSeparatorsWithSpace<Rest, Separators, `${O} `>
      : ReplaceSeparatorsWithSpace<Rest, Separators, `${O}${First}`>
    : O;

  type ObjPathSeparators = "." | "[" | "]";

  type FilterEmptyStrings<T> = T extends [infer First, ...infer Rest]
    ? First extends ""
      ? FilterEmptyStrings<Rest>
      : [First, ...FilterEmptyStrings<Rest>]
    : [];

  type ToNumFromStringMap<T> = T extends [infer First, ...infer Rest]
    ? [ToNumFromString<First>, ...ToNumFromStringMap<Rest>]
    : [];

  type ToNumFromString<T> = T extends `${infer Num extends number}` ? Num : T;

  // several object keys
  declare const obj1: { a: { b: { c: string } } };
  const res1 = get(obj1, "a.b.c");

  // objects and arrays
  declare const obj2: { author: { friends: [{ age: 29 }] } };
  const res2 = get(obj2, "author.friends[0].age");

  // accessing a precise index of a tuple type
  declare const obj3: { author: { friends: [undefined, { name: "Bob" }] } };
  const res3 = get(obj3, "author.friends[1].name");

  // several tuple types
  declare const obj4: [1, 2, [3, [{ title: "🎉" }]]];
  const res4 = get(obj4, "[2][1][0].title");
}
