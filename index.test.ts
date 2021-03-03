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

import { AssertIs, AssertSub, TestSuite } from "./assert";

type S0 = [];
type S1 = [E];
type S2 = [E, E];
type S3 = [E, E, E];
type S4 = [E, E, E, E];
type S5 = [E, E, E, E, E];
type S6 = [E, E, E, E, E, E];

interface Expressions extends Record<string, ExpressionNode> {
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
  "Nat to number": AssertIs<ToNumber<[E, E, E]>, 3>;
  "number to Nat": AssertIs<ToNat<2>, [E, E]>;
}

interface ArithmeticOperationsTest extends TestSuite {
  add: {
    result: AssertIs<Add<S2, S3>, S5>;
  };
  multiply: {
    result: AssertIs<Multiply<S2, S3>, S6>;
  };

  sub: {
    result1: AssertIs<Sub<S3, S2>, S1>;
    result2: AssertIs<Sub<S2, S3>, NaN>;
  };

  divide: {
    result1: AssertIs<Divide<S3, S1>, S3>;
    result2: AssertIs<Divide<S3, S2>, S1>;
    result3: AssertIs<Divide<S3, S0>, NaN>;
  };

  complex: {
    result: AssertIs<Add<Multiply<S1, S2>, S2>, S4>;
  };
}

interface EvaluationTest extends TestSuite {
  resultOfEval1: AssertIs<Evaluate<Expressions["1+1"]>, S2>;
  resultOfEval2: AssertIs<Evaluate<Expressions["1+1+3"]>, S5>;
  resultOfEval3: AssertIs<Evaluate<Expressions["(1+1)*3"]>, S6>;
}

interface TokenizerTest extends TestSuite {
  "state reducer": {
    resultOfAppend: AssertIs<Append<{ tokens: []; buffer: S1 }>, { tokens: [NatToken<S1>]; buffer: null }>;
    resultOfUnshift: AssertIs<Unshift<{ tokens: []; buffer: S1 }, 2>, { tokens: []; buffer: ToNat<12> }>;
  };

  "tokenize 1 character": {
    result1: AssertIs<Tokenize<"0">, [NatToken<S0>]>;
    result2: AssertIs<Tokenize<"1">, [NatToken<S1>]>;
    result3: AssertSub<Tokenize<"?">, TokenizeFailure>;
  };

  "tokenize with whitespace": {
    result1: AssertIs<Tokenize<" 0">, [NatToken<S0>]>;
    result2: AssertIs<Tokenize<"  0 ">, [NatToken<S0>]>;
  };

  "tokenize with 2 digits number": {
    result1: AssertIs<Tokenize<" 10">, [NatToken<ToNat<10>>]>;
    result2: AssertIs<Tokenize<"11">, [NatToken<ToNat<11>>]>;
    result3: AssertIs<Tokenize<"01">, [NatToken<S1>]>;
  };

  "tokenize complex": {
    result1: AssertIs<Tokenize<"19">, [NatToken<ToNat<19>>]>;
    result2: AssertIs<Tokenize<"1 + 1">["length"], 3>;
    result3: AssertIs<
      Tokenize<"3 * (1 + 2)">,
      [NatToken<ToNat<3>>, TimesToken, LPToken, NatToken<ToNat<1>>, PlusToken, NatToken<ToNat<2>>, RPToken]
    >;
  };
}

interface ParserTest extends TestSuite {
  "simple add operaion": {
    result1: AssertIs<Parse<Tokenize<"1+1">>, Expressions["1+1"]>;
    result2: AssertIs<Parse<Tokenize<"1+1+3">>, Expressions["1+1+3"]>;
  };

  primary: {
    result1: AssertIs<Parse<Tokenize<"(1)">>, Expressions["1"]>;
    result2: AssertIs<Parse<Tokenize<"((1))">>, Expressions["1"]>;
    result3: AssertIs<Parse<Tokenize<"1+(1+3)">>, Expressions["1+(1+3)"]>;
  };

  "failure pattern": {
    result1: AssertSub<Parse<Tokenize<"+1">>, ParseFailure>;
    result2: AssertSub<Parse<Tokenize<"(2">>, ParseFailure>;
    result3: AssertSub<Parse<Tokenize<")10">>, ParseFailure>;
  };

  complex: AssertIs<Parse<Tokenize<"(1+1)*3">>, Expressions["(1+1)*3"]>;
}

// Put it all together
interface TestCalc extends TestSuite {
  result1: AssertIs<Calc<"1+1">, 2>;
  result2: AssertIs<Calc<"(2*3-2)/2">, 2>;
  result3: AssertIs<Calc<"1-4">, NaN>;
}
