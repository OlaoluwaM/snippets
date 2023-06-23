type CondWithGuardClause<A> =
  unknown extends A // Guard clause as this will always take us to the falsy branch unless `A` is `unknown` or `any`
    ? never
    : /*Other pieces of code*/
