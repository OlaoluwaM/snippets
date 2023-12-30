type AnyArray = any[];
type AnyFunction = (...args: any[]) => any;

type SnakeCaseToCamelStr<S> = S extends `${infer First}_${infer Rest}`
  ? `${Lowercase<First>}${Capitalize<SnakeCaseToCamelStr<Rest>>}`
  : S;

type DeepSnakeToCamelCaseObject<T> = T extends [infer First, ...infer Rest]
  ? [DeepSnakeToCamelCaseObject<First>, ...DeepSnakeToCamelCaseObject<Rest>]
  : T extends []
  ? []
  : {
      [P in keyof T as SnakeCaseToCamelStr<P>]: T[P] extends AnyFunction
        ? T[P]
        : DeepSnakeToCamelCaseObject<T[P]>;
    };

type _1 = DeepSnakeToCamelCaseObject<{
  d_f: [{ l_f: number }, 2, 1, 3];
  l_p: () => void;
  lre_ipo: { lll: boolean };
}>;

const snakeCaseToCamelStr = <T extends string>(s: T) => {
  return s.replace(/_[a-z]/g, (letter) =>
    letter[1].toUpperCase()
  ) as SnakeCaseToCamelStr<T>;
};

const deepConvertSnakeToCamelCaseObject = <T extends object>(
  obj: T
): DeepSnakeToCamelCaseObject<T> => {
  const isNotAValidObj =
    typeof obj !== "object" || obj === null || Array.isArray(obj);

  if (isNotAValidObj) {
    return obj as DeepSnakeToCamelCaseObject<T>;
  }

  return Object.fromEntries(
    Object.entries(obj).map(([key, val]) => [
      snakeCaseToCamelStr(key),
      deepConvertSnakeToCamelCaseObject(val),
    ])
  ) as DeepSnakeToCamelCaseObject<T>;
};
