// Map
type MapLoop<List extends unknown[]> = List extends [infer First, ...infer Rest]
  ? [SomeMapOp<First>, ...MapLoop<Rest>]
  : [];

type SomeMapOp<V> = V extends string ? `${V} vvs` : V;

type Grr = MapLoop<["fr", "erf", "wefw"]>;

// Filter
type FilterLoop<
  List extends unknown[],
> = List extends [infer First, ...infer Rest]
  ? Condition<First> extends true // Condition should be a constructor that returns either the true singleton or the false singleton type
    ? [First, ...FilterLoop<Rest>]
    : FilterLoop<Rest>
  : [];

type Condition<T> = T extends 3 ? true : false;

type FooPrime = FilterLoop<[1, 2, 3, 4]>;

// Reduce
declare const unset: unique symbol;
type Unset = typeof unset;

type ReduceLoop<List extends unknown[], Final = Unset> = List extends [
  infer First,
  ...infer Rest
]
  ? Final extends Unset
    ? ReduceLoop<Rest, First>
    : ReduceLoop<Rest, SomeCombineOp<Final, First>>
  : Final;

type SomeCombineOp<T, U> = T extends string
  ? U extends string
    ? `${T} ${U}`
    : never
  : never;

type Hh = ReduceLoop<["fd", "fdfd", "fdfe"], "">;


// Boilerplate

// Map
type SomeMapLoop<List> =
  List extends [infer First, ...infer Rest]
    ? [ /* ... 🤖 your logic */ , ...SomeMapLoop<Rest>]
    : [];

// Filter
type SomeFilter<List> =
  List extends [infer First, ...infer Rest]
    ? First extends  /* ... ❓ your condition */
      ? [First, ...SomeFilter<Rest>]
      : SomeFilter<Rest>
    : [];

// Reduce
type SomeReduce<Tuple, Acc = /* ... 📦 initial value */> =
  Tuple extends [infer First, ...infer Rest]
  ? SomeReduce<Rest, /* ... 🤖 logic */> // Tail recursive because the rcursive result is returned without modification
  : Acc;
