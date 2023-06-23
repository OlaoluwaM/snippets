type GenerateTupleOfN<
  N extends number,
  Output extends any[] = []
> = Output["length"] extends N ? Output : GenerateTupleOfN<N, [...Output, number]>;

function pairSum(ns: number[]): number[] {
  return ns
    .map((n, ind, arr) => (ind === arr.length - 1 ? -1 : n + arr[ind + 1]))
    .filter((n) => n !== -1);
}

function pascalTriangle<N extends number>(n: N): GenerateTupleOfN<N> {
  if (n == 1) return [1] as GenerateTupleOfN<N>;
  if (n == 2) return [1, 1] as GenerateTupleOfN<N>;
  return [1, pairSum(pascalTriangle(n - 1)), 1].flat() as GenerateTupleOfN<N>
}

function generatePascalTriangleToN(n: number): number[][] {
  return Array.from({length: n}, (_,ind) => ind + 1).map(pascalTriangle)
}

console.log(pascalTriangle(6));
console.log(generatePascalTriangleToN(10))
