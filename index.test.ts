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
  TokenizeFailure,
  Append,
  Unshift,
  NatToken,
  TimesToken,
  LPToken,
  PlusToken,
  RPToken,
  LiteralNode,
  ExpressionNode,
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
type S5 = [E, E, E, E, E];
type S6 = [E, E, E, E, E, E];

type ExpressionsExampleBase = { [ex: string]: ExpressionNode };
interface Expressions extends ExpressionsExampleBase {
  "1": {
    kind: "Literal";
    value: S1;
  };
  "1+1": {
    kind: "BinaryExpression";
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
  "1+1+3": {
    kind: "BinaryExpression";
    operation: "Add";
    left: Expressions["1+1"];
    right: {
      kind: "Literal";
      value: S3;
    };
  };
  "1+(1+3)": {
    kind: "BinaryExpression";
    operation: "Add";
    left: {
      kind: "Literal";
      value: S1;
    };
    right: {
      kind: "BinaryExpression";
      operation: "Add";
      left: {
        kind: "Literal";
        value: S1;
      };
      right: {
        kind: "Literal";
        value: S3;
      };
    };
  };
  "(1+1)*3": {
    kind: "BinaryExpression";
    operation: "Multiply";
    left: Expressions["1+1"];
    right: {
      kind: "Literal";
      value: S3;
    };
  };
}

interface ConvertTest extends TestSuite {
  resultOfToNumber: AssertIs<ToNumber<[E, E, E]>, 3>;
  resultOfToNct: AssertIs<ToNat<2>, [E, E]>;
}

interface OperationTest extends TestSuite {
  resultOfAdd: AssertIs<Add<S2, S3>, S5>;
  resultOfMul: AssertIs<Multiply<S2, S3>, S6>;

  resultOfSub1: AssertIs<Sub<S3, S2>, S1>;
  resultOfSub2: AssertIs<Sub<S2, S3>, NaN>;

  resultOfDivide1: AssertIs<Divide<S3, S1>, S3>;
  resultOfDivide2: AssertIs<Divide<S3, S2>, S1>;
  resultOfDivide3: AssertIs<Divide<S3, S0>, NaN>;

  resultOfComplex: AssertIs<Add<Multiply<S1, S2>, S2>, S4>;
}

interface EvaluationTest extends TestSuite {
  resultOfEval1: AssertIs<Evaluate<Expressions["1+1"]>, S2>;
  resultOfEval2: AssertIs<Evaluate<Expressions["1+1+3"]>, S5>;
  resultOfEval3: AssertIs<Evaluate<Expressions["(1+1)*3"]>, S6>;
}

interface TokenizerTest extends TestSuite {
  resultOfAppend: AssertIs<Append<{ tokens: []; buffer: S1 }>, { tokens: [NatToken<S1>]; buffer: null }>;
  resultOfUnshift: AssertIs<Unshift<{ tokens: []; buffer: S1 }, 2>, { tokens: []; buffer: ToNat<12> }>;

  resultOfTokenize1_1: AssertIs<Tokenize<"0">, [NatToken<S0>]>;
  resultOfTokenize1_2: AssertIs<Tokenize<"1">, [NatToken<S1>]>;
  resultOfTokenize1_4: AssertSub<Tokenize<"?">, TokenizeFailure>;

  resultOfTokenize2_1: AssertIs<Tokenize<" 0">, [NatToken<S0>]>;
  resultOfTokenize2_2: AssertIs<Tokenize<"  0 ">, [NatToken<S0>]>;

  resultOfTokenize3_1: AssertIs<Tokenize<" 10">, [NatToken<ToNat<10>>]>;
  resultOfTokenize3_2: AssertIs<Tokenize<"11">, [NatToken<ToNat<11>>]>;
  resultOfTokenize3_3: AssertIs<Tokenize<"01">, [NatToken<S1>]>;

  resultOfTokenize4_1: AssertIs<Tokenize<"19">, [NatToken<ToNat<19>>]>;
  resultOfTokenize4_2: AssertIs<Tokenize<"1 + 1">["length"], 3>;
  resultOfTokenize4_3: AssertIs<
    Tokenize<"3 * (1 + 2)">,
    [NatToken<ToNat<3>>, TimesToken, LPToken, NatToken<ToNat<1>>, PlusToken, NatToken<ToNat<2>>, RPToken]
  >;
}

interface ParserTest extends TestSuite {
  resultOfParse1_1: AssertIs<Parse<Tokenize<"1+1">>, Expressions["1+1"]>;
  resultOfParse2: AssertIs<Parse<Tokenize<"1+1+3">>, Expressions["1+1+3"]>;
  resultOfParse3_1: AssertIs<Parse<Tokenize<"(1)">>, Expressions["1"]>;
  resultOfParse3_2: AssertIs<Parse<Tokenize<"1+(1+3)">>, Expressions["1+(1+3)"]>;
  resultOfParse3_3: AssertSub<Parse<Tokenize<"+1">>, ParseFailure>;
  resultOfParse3_4: AssertSub<Parse<Tokenize<"(2">>, ParseFailure>;
  resultOfParse3_5: AssertSub<Parse<Tokenize<")10">>, ParseFailure>;
  resultOfParse4_1: AssertIs<Parse<Tokenize<"(1+1)*3">>, Expressions["(1+1)*3"]>;
}

// Put it all together
interface TestCalc extends TestSuite {
  resultOf1: AssertIs<Calc<"1+1">, 2>;
  resultOf2: AssertIs<Calc<"(2*3-2)/2">, 2>;
  resultOf3: AssertIs<Calc<"1-4">, NaN>;
}
