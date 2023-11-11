type UnionToIntersection<Union> = (
  Union extends any ? (arg: Union) => void : never
) extends (arg: infer Intersection) => void
  ? Intersection
  : never;

type _0 = UnionToIntersection<{ a: string } | { b: number } | { c: boolean }>;

type GetLastUnionMember<Union> = UnionToIntersection<
  Union extends any ? () => Union : never
> extends () => infer Last
  ? Last
  : never;

type _1 = GetLastUnionMember<{ a: string } | { b: number } | { c: boolean }>;
