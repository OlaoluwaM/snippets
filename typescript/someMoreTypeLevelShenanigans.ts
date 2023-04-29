type Length<X extends any[]> = X["length"]

type ToTuple<L extends number, I extends 0[] = []> = I['length'] extends L ? I : ToTuple<L, [...I, 0]>

type Add<A extends number, B extends number> = [...ToTuple<A>,...ToTuple<B>]['length']

type Ex = Negative<5>

type Negative<A extends number> = `-${A}` extends `${infer T extends number}` ? T : never

type Sub<A extends number, B extends number> = ToTuple<A> extends [...ToTuple<B>, ...infer C] ? C['length'] : Negative<Sub<B, A>>


type Foo = Add<1, 2>
type Bar = Sub<5, 2>
