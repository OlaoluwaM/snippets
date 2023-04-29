type _Add<V extends number, W extends number, VA extends number[] = [], WA extends number[] = []> = 
	VA["length"] extends V
		? WA["length"] extends W
			? [...VA, ...WA]["length"]
			: _Add<V, W, VA, [...WA, 1]>
		: _Add<V, W, [...VA, 1], WA>

type F = _Add<4, 5>

type _AddOne<Arr extends number[], Inner extends number[] = []> = 
	Arr extends [infer First extends number, ...infer Rest extends number[]]
		? _AddOne<Rest, [...Inner, _Add<First, 1>]>
		: Inner

type O = _AddOne<[1, 2, 3, 4]>
