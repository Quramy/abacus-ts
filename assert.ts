type AssertSuccess = {
  readonly ok: unique symbol;
};

export type AssertSub<T1, T2> = T1 extends T2 ? AssertSuccess : [actual: T1, expected: T2];

export type AssertIs<T1, T2> = T1 extends T2
  ? T2 extends T1
    ? AssertSuccess
    : [actual: T1, expected: T2]
  : [actual: T1, expected: T2];

export type TestSuite = {
  [caseName: string]: TestSuite | AssertSuccess;
};
