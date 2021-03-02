// // Assert util
// type AssertSub<T1, T2> = T1 extends T2 ? true : T1;
// type AssertIs<T1, T2> = T1 extends T2 ? (T2 extends T1 ? true : false) : T1;

// Internal represation of non-negative integer
type E = null;
type Nat = E[];

type S0 = [];
type S1 = [E];

type S2 = [E, E];
type S3 = [E, E, E];
type S4 = [E, E, E, E];

// Type Operators for converting number between IR.
export type ToNumber<N extends Nat> = N["length"];
export type ToNat<L extends number, N extends Nat = S0> = ToNumber<N> extends L ? N : ToNat<L, [E, ...N]>;

// Binary oprations
export type Add<A extends Nat, B extends Nat> = [...A, ...B];
export type Multiply<A extends Nat, B extends Nat> = A extends S0
  ? S0
  : B extends S0
  ? S0
  : A extends S1
  ? B
  : A extends [...S1, ...infer A2]
  ? A2 extends Nat
    ? Add<Multiply<A2, B>, B>
    : never
  : never;

type Operations_bk1<A extends Nat, B extends Nat> = {
  Add: Add<A, B>;
  Multiply: Multiply<A, B>;
};

export type NaN = "NaN";
export type Sub<A extends Nat, B extends Nat> = A extends [...B, ...infer S] ? S : NaN;
export type Divide<A extends Nat, B extends Nat, R extends Nat = S0> = B extends S0
  ? NaN
  : B extends S1
  ? A
  : Sub<A, B> extends infer A2
  ? A2 extends Nat
    ? Divide<A2, B, Add<R, S1>>
    : R
  : never;

type Operations<A extends Nat | NaN, B extends Nat | NaN> = {
  Add: A extends Nat ? (B extends Nat ? Add<A, B> : NaN) : NaN;
  Sub: A extends Nat ? (B extends Nat ? Sub<A, B> : NaN) : NaN;
  Multiply: A extends Nat ? (B extends Nat ? Multiply<A, B> : NaN) : NaN;
  Divide: A extends Nat ? (B extends Nat ? Divide<A, B> : NaN) : NaN;
};

// AST
// Node kinds
type LiteralNode_bk = {
  kind: "Literal";
  value: Nat;
};

type LiteralNode<V extends Nat = Nat> = {
  kind: "Literal";
  value: V;
};

type BaseBinaryExpressionNode = {
  kind: "Binary";
  left: ExpressionNode;
  right: ExpressionNode;
  operation: keyof Operations<any, any>;
};

type AddNode = BaseBinaryExpressionNode & {
  operation: "Add";
};

type MultiplyNode = BaseBinaryExpressionNode & {
  operation: "Multiply";
};

type SubNode = BaseBinaryExpressionNode & {
  operation: "Sub";
};
type DivideNode = BaseBinaryExpressionNode & {
  operation: "Divide";
};

type ExpressionNode_bk1 = LiteralNode | AddNode;
type BinaryExpressionNode_bk2 = AddNode | MultiplyNode;
type BinaryExpressionNode = AddNode | SubNode | MultiplyNode | DivideNode;
type ExpressionNode = LiteralNode | BinaryExpressionNode;

// Evaluation
type Evaluate_bk1<T extends ExpressionNode> = T extends LiteralNode
  ? T["value"]
  : T extends AddNode
  ? Evaluate<T["left"]> extends infer L
    ? L extends Nat
      ? Evaluate<T["right"]> extends infer R
        ? R extends Nat
          ? Add<L, R>
          : never
        : never
      : never
    : never
  : never;

type EvalLeft<T extends BaseBinaryExpressionNode> = Evaluate<T["left"]> extends infer N
  ? N extends Nat
    ? N
    : NaN
  : never;
type EvalRight<T extends BaseBinaryExpressionNode> = Evaluate<T["right"]> extends infer N
  ? N extends Nat
    ? N
    : NaN
  : never;

export type Evaluate<T extends ExpressionNode> = T extends LiteralNode
  ? T["value"]
  : T extends BinaryExpressionNode
  ? Operations<EvalLeft<T>, EvalRight<T>>[T["operation"]]
  : never;

// Tokeninzer

// Tokeninze Step0
type NatToken<N extends Nat = Nat> = {
  tokenKind: "NatToken";
  value: N;
};
type LPToken = {
  tokenKind: "LeftParenthesisToken";
};
type RPToken = {
  tokenKind: "RightParenthesisToken";
};
type PlusToken = {
  tokenKind: "PlusToken";
};
type TimesToken = {
  tokenKind: "TimesToken";
};
type MinusToken = {
  tokenKind: "MinusToken";
};
type SlashToken = {
  tokenKind: "SlashToken";
};

type Token_bk4 = NatToken;
type Token_bk5 = NatToken | LPToken | RPToken | PlusToken | TimesToken;
// with sub, divide
type Token = NatToken | LPToken | RPToken | PlusToken | MinusToken | TimesToken | SlashToken;

type Tokens = Token[];
type TokenizeFailure<S extends string = string> = {
  failure: true;
  input: S;
  message: "Invalid input string.";
};

// prettier-ignore
type Tokeninze_bk1<Text extends string> = 
  Text extends "0" ? [NatToken<ToNat<0>>] :
  Text extends "1" ? [NatToken<ToNat<1>>] :
  TokenizeFailure<Text>;

// prettier-ignore
type Tokenize_bk2<Text extends string> = 
  Text extends "" ? [] :
  Text extends ` ${infer Rest}` ? Tokenize<Rest> :
  Text extends "0" ? [NatToken<ToNat<0>>] :
  Text extends "1" ? [NatToken<ToNat<1>>] :
  TokenizeFailure<Text>;

type TokenizeState = {
  tokens: Tokens;
  buffer: Nat | null;
};

type InitialState = {
  tokens: [];
  buffer: null;
};

type OneDigitNum = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

type Unshift<S extends TokenizeState, L extends OneDigitNum> = {
  tokens: S["tokens"];
  buffer: S["buffer"] extends S0
    ? ToNat<L>
    : S["buffer"] extends Nat
    ? Add<Multiply<S["buffer"], ToNat<10>>, ToNat<L>>
    : ToNat<L>;
};

type Append_bk3<S extends TokenizeState> = S["buffer"] extends null
  ? S
  : S["buffer"] extends Nat
  ? {
      tokens: [...S["tokens"], NatToken<S["buffer"]>];
      buffer: null;
    }
  : never;

type Append<S extends TokenizeState, T extends Tokens = []> = S["buffer"] extends null
  ? {
      tokens: [...S["tokens"], ...T];
      buffer: null;
    }
  : S["buffer"] extends Nat
  ? {
      tokens: [...S["tokens"], NatToken<S["buffer"]>, ...T];
      buffer: null;
    }
  : never;

// prettier-ignore
type Tokenize_bk3<Text extends string, S extends TokenizeState = InitialState> = 
  Text extends "" ? Append<S>["tokens"] :
  Text extends ` ${infer Rest}` ? Tokenize<Rest, Append<S>> :
  Text extends `0${infer Rest}` ? Tokenize<Rest, Unshift<S, 0>> :
  Text extends `1${infer Rest}` ? Tokenize<Rest, Unshift<S, 1>> :
  TokenizeFailure<Text>;

// prettier-ignore
type Tokenize_bk4<Text extends string, S extends TokenizeState = InitialState> = 
  Text extends "" ? Append<S>["tokens"] :
  Text extends ` ${infer Rest}` ? Tokenize<Rest, Append<S>> :
  Text extends `(${infer Rest}` ? Tokenize<Rest, Append<S, [LPToken]>> :
  Text extends `)${infer Rest}` ? Tokenize<Rest, Append<S, [RPToken]>> :
  Text extends `+${infer Rest}` ? Tokenize<Rest, Append<S, [PlusToken]>> :
  Text extends `*${infer Rest}` ? Tokenize<Rest, Append<S, [TimesToken]>> :
  Text extends `0${infer Rest}` ? Tokenize<Rest, Unshift<S, 0>> :
  Text extends `1${infer Rest}` ? Tokenize<Rest, Unshift<S, 1>> :
  Text extends `2${infer Rest}` ? Tokenize<Rest, Unshift<S, 2>> :
  Text extends `3${infer Rest}` ? Tokenize<Rest, Unshift<S, 3>> :
  Text extends `4${infer Rest}` ? Tokenize<Rest, Unshift<S, 4>> :
  Text extends `5${infer Rest}` ? Tokenize<Rest, Unshift<S, 5>> :
  Text extends `6${infer Rest}` ? Tokenize<Rest, Unshift<S, 6>> :
  Text extends `7${infer Rest}` ? Tokenize<Rest, Unshift<S, 7>> :
  Text extends `8${infer Rest}` ? Tokenize<Rest, Unshift<S, 8>> :
  Text extends `9${infer Rest}` ? Tokenize<Rest, Unshift<S, 9>> :
  TokenizeFailure<Text>;

// prettier-ignore
export type Tokenize<Text extends string, S extends TokenizeState = InitialState> = 
  Text extends "" ? Append<S>["tokens"] :
  Text extends ` ${infer Rest}` ? Tokenize<Rest, Append<S>> :
  Text extends `(${infer Rest}` ? Tokenize<Rest, Append<S, [LPToken]>> :
  Text extends `)${infer Rest}` ? Tokenize<Rest, Append<S, [RPToken]>> :
  Text extends `+${infer Rest}` ? Tokenize<Rest, Append<S, [PlusToken]>> :
  Text extends `-${infer Rest}` ? Tokenize<Rest, Append<S, [MinusToken]>> :
  Text extends `*${infer Rest}` ? Tokenize<Rest, Append<S, [TimesToken]>> :
  Text extends `/${infer Rest}` ? Tokenize<Rest, Append<S, [SlashToken]>> :
  Text extends `0${infer Rest}` ? Tokenize<Rest, Unshift<S, 0>> :
  Text extends `1${infer Rest}` ? Tokenize<Rest, Unshift<S, 1>> :
  Text extends `2${infer Rest}` ? Tokenize<Rest, Unshift<S, 2>> :
  Text extends `3${infer Rest}` ? Tokenize<Rest, Unshift<S, 3>> :
  Text extends `4${infer Rest}` ? Tokenize<Rest, Unshift<S, 4>> :
  Text extends `5${infer Rest}` ? Tokenize<Rest, Unshift<S, 5>> :
  Text extends `6${infer Rest}` ? Tokenize<Rest, Unshift<S, 6>> :
  Text extends `7${infer Rest}` ? Tokenize<Rest, Unshift<S, 7>> :
  Text extends `8${infer Rest}` ? Tokenize<Rest, Unshift<S, 8>> :
  Text extends `9${infer Rest}` ? Tokenize<Rest, Unshift<S, 9>> :
  TokenizeFailure<Text>;

// Parse

//
// Parser Step 1
//
// ```
// expr = num("+" num)?
// num  = "1" | "2" | "3" | ,,,
// ```
//
type ParseResult<T extends Tokens = Tokens, N extends ExpressionNode = ExpressionNode> = {
  tokens: T;
  node: N;
};

type ConsumeToken<Symbol extends Token, T extends Tokens> = [Symbol, ...T];
type ConsumeNatToken<NT extends NatToken, T extends Tokens> = [NT, ...T];

type ParseNum_bk2<T extends Tokens> = T extends ConsumeNatToken<infer NT, infer T>
  ? ParseResult<T, LiteralNode<NT["value"]>>
  : never;

type ParseExpr_bk1<T extends Tokens> = ParseNum<T> extends ParseResult<infer T, infer Left>
  ? T extends ConsumeToken<PlusToken, infer T>
    ? ParseNum<T> extends ParseResult<infer T, infer Right>
      ? ParseResult<T, { kind: "Binary"; operation: "Add"; left: Left; right: Right }>
      : never
    : Left
  : never;

type ParseFailure<T extends Tokens = Tokens, S extends string = string> = {
  failure: true;
  tokens: T;
  message: S;
};

type ParseNum<T extends Tokens> = T extends ConsumeNatToken<infer NT, infer T>
  ? ParseResult<T, LiteralNode<NT["value"]>>
  : ParseFailure<T, "Number expected.">;

//
// Parser Step 2
//
// ```
// expr = num("+" num)*
// num  = "1" | "2" | "3" | ,,,
// ```
//
type ParseExpr_bk2<T extends Tokens> = ParseNum<T> extends ParseResult<infer T, infer Left>
  ? ParseExprLoop_bk2<T, Left>
  : never;
type ParseExprLoop_bk2<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<PlusToken, infer T>
  ? ParseNum<T> extends ParseResult<infer T, infer Right>
    ? ParseExprLoop_bk2<T, { kind: "Binary"; operation: "Add"; left: Left; right: Right }>
    : never
  : ParseResult<T, Left>;

//
// Parser Step 3
//
// ```
// expr     = primary("+" primary)*
// primary  = "(" expr ")" | num
// num      = "1" | "2" | "3" | ,,,
// ```
//
type ParsePrimary<T extends Tokens> = T extends ConsumeToken<LPToken, infer T>
  ? ParseExpr<T> extends ParseResult<infer T, infer N>
    ? T extends ConsumeToken<RPToken, infer T>
      ? ParseResult<T, N>
      : ParseFailure<T, "')' expected.">
    : never
  : ParseNum<T>;

type ParseExpr_bk3<T extends Tokens> = ParsePrimary<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseExprLoop_bk3<T, Left>
    : R
  : never;
type ParseExprLoop_bk3<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<PlusToken, infer T>
  ? ParsePrimary<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop_bk3<T, { kind: "Binary"; operation: "Add"; left: Left; right: Right }>
      : R
    : never
  : ParseResult<T, Left>;

//
// Parser Step 4
//
// ```
// expr     = mul("+" mul)*
// mul      = primary("*" primary)*
// primary  = "(" expr ")" | num
// num      = "1" | "2" | "3" | ,,,
// ```
//
type ParseExpr_bk4<T extends Tokens> = ParseMul<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseExprLoop_bk4<T, Left>
    : R
  : never;
type ParseExprLoop_bk4<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<PlusToken, infer T>
  ? ParseMul<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop_bk4<T, { kind: "Binary"; operation: "Add"; left: Left; right: Right }>
      : R
    : never
  : ParseResult<T, Left>;

type ParseMul_bk4<T extends Tokens> = ParsePrimary<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseMulLoop_bk4<T, Left>
    : R
  : never;
type ParseMulLoop_bk4<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<TimesToken, infer T>
  ? ParsePrimary<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseMulLoop_bk4<T, { kind: "Binary"; operation: "Multiply"; left: Left; right: Right }>
      : R
    : never
  : ParseResult<T, Left>;

//
// Parser Step 5 (with sub, divide)
//
// ```
// expr     = mul("+" mul | "-" mul)*
// mul      = primary("*" primary | "/" primary)*
// primary  = "(" expr ")" | num
// num      = "1" | "2" | "3" | ,,,
// ```
//
type ParseExpr<T extends Tokens> = ParseMul<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseExprLoop<T, Left>
    : R
  : never;
type ParseExprLoop<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<PlusToken, infer T>
  ? ParseMul<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop<T, { kind: "Binary"; operation: "Add"; left: Left; right: Right }>
      : R
    : never
  : T extends ConsumeToken<MinusToken, infer T>
  ? ParseMul<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop<T, { kind: "Binary"; operation: "Sub"; left: Left; right: Right }>
      : R
    : never
  : ParseResult<T, Left>;

type ParseMul<T extends Tokens> = ParsePrimary<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseMulLoop<T, Left>
    : R
  : never;
type ParseMulLoop<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<TimesToken, infer T>
  ? ParsePrimary<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseMulLoop<T, { kind: "Binary"; operation: "Multiply"; left: Left; right: Right }>
      : R
    : never
  : T extends ConsumeToken<SlashToken, infer T>
  ? ParsePrimary<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseMulLoop<T, { kind: "Binary"; operation: "Divide"; left: Left; right: Right }>
      : R
    : never
  : ParseResult<T, Left>;

export type Parse<T extends Tokens> = ParseExpr<T> extends infer R
  ? R extends ParseResult<[], infer N>
    ? N
    : R
  : never;

export type Calc<S extends string> = Tokenize<S> extends infer T
  ? T extends Tokens
    ? Parse<T> extends infer N
      ? N extends ExpressionNode
        ? Evaluate<N> extends infer V
          ? V extends Nat
            ? ToNumber<V>
            : V
          : never
        : N
      : never
    : T
  : never;

// // Example AST Node
//
// // Represents 1 + 1
// type Ex1 = {
//   kind: "Binary";
//   operation: "Add";
//   left: {
//     kind: "Literal";
//     value: S1;
//   };
//   right: {
//     kind: "Literal";
//     value: S1;
//   };
// };
//
// // Represents 1 + 1 + 3 = (1 + 1) + 3
// type Ex2 = {
//   kind: "Binary";
//   operation: "Add";
//   left: Ex1;
//   right: {
//     kind: "Literal";
//     value: S3;
//   };
// };
//
// // Represents (1 + 1) * 3
// type Ex3 = {
//   kind: "Binary";
//   operation: "Multiply";
//   left: Ex1;
//   right: {
//     kind: "Literal";
//     value: S3;
//   };
// };
//
// // Convertor test
// const testToNumber: AssertIs<ToNumber<[E, E, E]>, 3> = true;
// const testToNat: AssertIs<ToNat<2>, [E, E]> = true;
//
// // Operation test
// const testAdd: AssertIs<Add<S2, S3>, ToNat<5>> = true;
// const testMul: AssertIs<Multiply<S2, S3>, ToNat<6>> = true;
//
// const testComplex1: AssertIs<Add<Multiply<S2, S3>, S1>, ToNat<7>> = true;
//
// const testSub1: AssertIs<Sub<S3, S2>, S1> = true;
// const testSub2: AssertIs<Sub<S2, S3>, NaN> = true;
//
// const testDivide1: AssertIs<Divide<S3, S1>, S3> = true;
// const testDivide2: AssertIs<Divide<S3, S2>, S1> = true;
// const testDivide3: AssertIs<Divide<S3, S0>, NaN> = true;
//
// // Evaluation test
//
// const testEval1_1: AssertIs<Evaluate<Ex1>, S2> = true;
// const testEval1_2: AssertIs<Evaluate<Ex2>, ToNat<5>> = true;
// const testEval2_1: AssertIs<Evaluate<Ex3>, ToNat<6>> = true;
//
// // Tokenizer test
// const testTokenize1_1: AssertIs<Tokenize<"0">, [NatToken<S0>]> = true;
// const testTokenize1_2: AssertIs<Tokenize<"1">, [NatToken<S1>]> = true;
// const testTokenize1_3: AssertSub<Tokeninze_bk1<"+">, TokenizeFailure> = true;
// const testTokenize1_4: AssertSub<Tokenize<"?">, TokenizeFailure> = true;
//
// const testTokenize2_1: AssertIs<Tokenize<" 0">, [NatToken<S0>]> = true;
// const testTokenize2_2: AssertIs<Tokenize<"  0 ">, [NatToken<S0>]> = true;
//
// const testAppend3: AssertIs<Append<{ tokens: []; buffer: S1 }>, { tokens: [NatToken<S1>]; buffer: null }> = true;
// const testUnshift3: AssertIs<Unshift<{ tokens: []; buffer: S1 }, 2>, { tokens: []; buffer: ToNat<12> }> = true;
// const testTokenize3_1: AssertIs<Tokenize<" 10">, [NatToken<ToNat<10>>]> = true;
// const testTokenize3_2: AssertIs<Tokenize<"11">, [NatToken<ToNat<11>>]> = true;
// const testTokenize3_3: AssertIs<Tokenize<"01">, [NatToken<S1>]> = true;
//
// const testTokenize4_1: AssertIs<Tokenize<"19">, [NatToken<ToNat<19>>]> = true;
// const testTokenize4_2: AssertIs<Tokenize<"1 + 1">["length"], 3> = true;
// const testTokenize4_3: AssertIs<
//   Tokenize<"3 * (1 + 2)">,
//   [NatToken<ToNat<3>>, TimesToken, LPToken, NatToken<ToNat<1>>, PlusToken, NatToken<ToNat<2>>, RPToken]
// > = true;
//
// // Parser test
// const testParseNum1_1: AssertIs<ParseNum<Tokenize<"1">>["node"], LiteralNode<S1>> = true;
// const testParseNum3_1: AssertSub<ParseNum<Tokenize<"+">>, ParseFailure> = true;
// const testParseExpr1_1: AssertIs<ParseExpr<Tokenize<"1+1">>["node"], Ex1> = true;
//
// const testParseExpr2: AssertIs<ParseExpr<Tokenize<"1+1+3">>["node"], Ex2> = true;
//
// const testParseExpr3_1: AssertIs<ParseExpr<Tokenize<"(2)">>["node"], LiteralNode<S2>> = true;
// const testParseExpr3_2: AssertIs<ParseExpr<Tokenize<"3+(1+1)">>["node"]["right"], Ex1> = true;
// const testParseExpr3_3: AssertSub<ParseExpr<Tokenize<"+1">>, ParseFailure> = true;
// const testParseExpr3_4: AssertSub<ParseExpr<Tokenize<"(2">>, ParseFailure> = true;
// const testParseExpr3_5: AssertSub<ParseExpr<Tokenize<")10">>, ParseFailure> = true;
//
// const testParseExpr4_1: AssertIs<ParseExpr<Tokenize<"(1+1)*3">>["node"], Ex3> = true;
//
// // Put it all together
// type ResultFinal1 = Calc<"30+(1+3)*(1+3)">;
// type ResultFinal2 = Calc<"(2+3)*5-1">;
// type ResultFinal3 = Calc<"(1+2)*6/(4-1)">;
// type ResultFinal4 = Calc<"1-4">;
//
// const testCalc3: AssertIs<Calc<"(1+2)*6/(4-1)">, 6> = true;
