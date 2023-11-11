type AnyFunction = (...args: any[]) => any;
type AnyObject = Record<string, any>;
type AnyArray = any[];

type RawTypes = Lowercase<
  | "Function"
  | "Object"
  | "Array"
  | "Null"
  | "Undefined"
  | "String"
  | "Number"
  | "Boolean"
>;

interface RawTypesMap {
  function: AnyFunction;
  object: AnyObject;
  array: AnyArray;
  null: null;
  undefined: undefined;
  string: string;
  number: number;
  boolean: boolean;
}

type ToRawType<T> = T extends AnyFunction
  ? "function"
  : T extends AnyObject
  ? "object"
  : T extends AnyArray
  ? "array"
  : T extends null
  ? "null"
  : T extends undefined
  ? "undefined"
  : T extends string
  ? "string"
  : T extends number
  ? "number"
  : T extends boolean
  ? "boolean"
  : never;

export function rawTypeOf<T>(value: T) {
  return Object.prototype.toString
    .call(value)
    .replace(/\[|\]|object|\s/g, "")
    .toLocaleLowerCase() as RawTypesMap[ToRawType<T>];
}
