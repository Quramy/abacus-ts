// Assert util
import {
  E,
  NaN,
  ToNat,
  ToNumber,
  Add,
  Sub,
  Multiply,
  Divide,
  Evaluate,
  Tokenize,
  NatToken,
  TokenizeFailure,
  Append,
  Unshift,
  TimesToken,
  LPToken,
  PlusToken,
  RPToken,
  LiteralNode,
  Parse,
  ParseFailure,
  Calc,
} from ".";

/* Assert Utility */
type AssertSuccess = {
  readonly Ok: unique symbol;
};
type AssertFailure<A, B> = [actual: A, expected: B];
type AssertSub<T1, T2> = T1 extends T2 ? AssertSuccess : [actual: T1, expected: T2];
type AssertIs<T1, T2> = T1 extends T2
  ? T2 extends T1
    ? AssertSuccess
    : [actual: T1, expected: T2]
  : [actual: T1, expected: T2];
type TestSuite = {
  [caseName: string]: AssertSuccess;
};

/* Example Nat values */
type S0 = [];
type S1 = [E];
type S2 = [E, E];
type S3 = [E, E, E];
type S4 = [E, E, E, E];

/* Example AST Node */

// Represents 1 + 1
type Ex1 = {
  kind: "Binary";
  operation: "Add";
  left: {
    kind: "Literal";
    value: S1;
  };
  right: {
    kind: "Literal";
    value: S1;
  };
};
// Represents 1 + 1 + 3 = (1 + 1) + 3
type Ex2 = {
  kind: "Binary";
  operation: "Add";
  left: Ex1;
  right: {
    kind: "Literal";
    value: S3;
  };
};

// Represents (1 + 1) * 3
type Ex3 = {
  kind: "Binary";
  operation: "Multiply";
  left: Ex1;
  right: {
    kind: "Literal";
    value: S3;
  };
};

interface ConvertTest extends TestSuite {
  toNumber: AssertIs<ToNumber<[E, E, E]>, 3>;
  toNat: AssertIs<ToNat<2>, [E, E]>;
}

interface OperationTest extends TestSuite {
  add: AssertIs<Add<S2, S3>, ToNat<5>>;
  mul: AssertIs<Multiply<S2, S3>, ToNat<6>>;

  sub1: AssertIs<Sub<S3, S2>, S1>;
  sub2: AssertIs<Sub<S2, S3>, NaN>;

  divide1: AssertIs<Divide<S3, S1>, S3>;
  divide2: AssertIs<Divide<S3, S2>, S1>;
  divide3: AssertIs<Divide<S3, S0>, NaN>;

  complex1: AssertIs<Add<Multiply<S2, S3>, S1>, ToNat<7>>;
}

interface EvaluationTest extends TestSuite {
  eval1: AssertIs<Evaluate<Ex1>, S2>;
  eval2: AssertIs<Evaluate<Ex2>, ToNat<5>>;
  eval3: AssertIs<Evaluate<Ex3>, ToNat<6>>;
}

interface TokenizerTest extends TestSuite {
  testAppend: AssertIs<Append<{ tokens: []; buffer: S1 }>, { tokens: [NatToken<S1>]; buffer: null }>;
  testUnshift: AssertIs<Unshift<{ tokens: []; buffer: S1 }, 2>, { tokens: []; buffer: ToNat<12> }>;

  testTokenize1_1: AssertIs<Tokenize<"0">, [NatToken<S0>]>;
  testTokenize1_2: AssertIs<Tokenize<"1">, [NatToken<S1>]>;
  testTokenize1_4: AssertSub<Tokenize<"?">, TokenizeFailure>;

  testTokenize2_1: AssertIs<Tokenize<" 0">, [NatToken<S0>]>;
  testTokenize2_2: AssertIs<Tokenize<"  0 ">, [NatToken<S0>]>;

  testTokenize3_1: AssertIs<Tokenize<" 10">, [NatToken<ToNat<10>>]>;
  testTokenize3_2: AssertIs<Tokenize<"11">, [NatToken<ToNat<11>>]>;
  testTokenize3_3: AssertIs<Tokenize<"01">, [NatToken<S1>]>;

  testTokenize4_1: AssertIs<Tokenize<"19">, [NatToken<ToNat<19>>]>;
  testTokenize4_2: AssertIs<Tokenize<"1 + 1">["length"], 3>;
  testTokenize4_3: AssertIs<
    Tokenize<"3 * (1 + 2)">,
    [NatToken<ToNat<3>>, TimesToken, LPToken, NatToken<ToNat<1>>, PlusToken, NatToken<ToNat<2>>, RPToken]
  >;
}

interface ParserTest extends TestSuite {
  testParse1_1: AssertIs<Parse<Tokenize<"1+1">>, Ex1>;
  testParse2: AssertIs<Parse<Tokenize<"1+1+3">>, Ex2>;
  testParse3_1: AssertIs<Parse<Tokenize<"(2)">>, LiteralNode<S2>>;
  testParse3_2: AssertIs<Parse<Tokenize<"3+(1+1)">>["right"], Ex1>;
  testParse3_3: AssertSub<Parse<Tokenize<"+1">>, ParseFailure>;
  testParse3_4: AssertSub<Parse<Tokenize<"(2">>, ParseFailure>;
  testParse3_5: AssertSub<Parse<Tokenize<")10">>, ParseFailure>;
  testParse4_1: AssertIs<Parse<Tokenize<"(1+1)*3">>, Ex3>;
}

// Put it all together
interface TestCalc extends TestSuite {
  resultFinal1: AssertIs<Calc<"1+1">, 2>;
  resultFinal2: AssertIs<Calc<"(2*3-2)/2">, 2>;
  resultFinal3: AssertIs<Calc<"1-4">, NaN>;
}
