// EBNF
//
// ```
// expr     = mul("+" mul | "-" mul)*
// mul      = primary("*" primary | "/" primary)*
// primary  = "(" expr ")" | num
// num      = "1" | "2" | "3" | ,,,
// ```

// Internal represation of non-negative integer
export type E = null;
export type Nat = E[];
export type NaN = "NaN";

export type S0 = [];
export type S1 = [E];

// Type Operators for converting number between IR.
export type ToNumber<N extends Nat> = N["length"];
export type ToNat<L extends number, N extends Nat = S0> = ToNumber<N> extends L ? N : ToNat<L, [E, ...N]>;

// Binary oprations
export type Add<A extends Nat, B extends Nat> = [...A, ...B];

export type Sub<A extends Nat, B extends Nat> = A extends [...B, ...infer S] ? S : NaN;

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

// Tokeninzer

export type NatToken<N extends Nat = Nat> = {
  tokenKind: "NatToken";
  value: N;
};
export type LPToken = {
  tokenKind: "LeftParenthesisToken";
};
export type RPToken = {
  tokenKind: "RightParenthesisToken";
};
export type PlusToken = {
  tokenKind: "PlusToken";
};
export type TimesToken = {
  tokenKind: "TimesToken";
};
export type MinusToken = {
  tokenKind: "MinusToken";
};
export type SlashToken = {
  tokenKind: "SlashToken";
};

export type Token = NatToken | LPToken | RPToken | PlusToken | MinusToken | TimesToken | SlashToken;

export type Tokens = Token[];

export type TokenizeFailure<S extends string = string> = {
  failure: true;
  input: S;
  message: "Invalid input string.";
};

export type TokenizeState = {
  tokens: Tokens;
  buffer: Nat | null;
};

export type InitialState = {
  tokens: [];
  buffer: null;
};

type OneDigitNum = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

export type Unshift<S extends TokenizeState, L extends OneDigitNum> = {
  tokens: S["tokens"];
  buffer: S["buffer"] extends S0
    ? ToNat<L>
    : S["buffer"] extends Nat
    ? Add<Multiply<S["buffer"], ToNat<10>>, ToNat<L>>
    : ToNat<L>;
};

export type Append<S extends TokenizeState, T extends Tokens = []> = S["buffer"] extends null
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

// AST
// Node kinds

export type LiteralNode<V extends Nat = Nat> = {
  kind: "Literal";
  value: V;
};

export type BinaryExpressionNode = {
  kind: "BinaryExpression";
  left: ExpressionNode;
  right: ExpressionNode;
  operation: keyof Operations<any, any>;
};

export type ExpressionNode = LiteralNode | BinaryExpressionNode;

type CreateLiteral<V extends Nat> = {
  kind: "Literal";
  value: V;
};

type CreateBinaryExpression<
  Op extends keyof Operations<any, any>,
  Left extends ExpressionNode,
  Right extends ExpressionNode
> = {
  kind: "BinaryExpression";
  operation: Op;
  left: Left;
  right: Right;
};

// Parse

export type ParseResult<T extends Tokens = Tokens, N extends ExpressionNode = ExpressionNode> = {
  tokens: T;
  node: N;
};

export type ParseFailure<T extends Tokens = Tokens, S extends string = string> = {
  failure: true;
  tokens: T;
  message: S;
};

type ConsumeToken<Symbol extends Token, T extends Tokens> = [Symbol, ...T];
type ConsumeNatToken<NT extends NatToken, T extends Tokens> = [NT, ...T];

type ParseNum<T extends Tokens> = T extends ConsumeNatToken<infer NT, infer T>
  ? ParseResult<T, CreateLiteral<NT["value"]>>
  : ParseFailure<T, "Number expected.">;

type ParsePrimary<T extends Tokens> = T extends ConsumeToken<LPToken, infer T>
  ? ParseExpr<T> extends ParseResult<infer T, infer N>
    ? T extends ConsumeToken<RPToken, infer T>
      ? ParseResult<T, N>
      : ParseFailure<T, "')' expected.">
    : never
  : ParseNum<T>;

type ParseExpr<T extends Tokens> = ParseMul<T> extends infer R
  ? R extends ParseResult<infer T, infer Left>
    ? ParseExprLoop<T, Left>
    : R
  : never;
type ParseExprLoop<T extends Tokens, Left extends ExpressionNode> = T extends ConsumeToken<PlusToken, infer T>
  ? ParseMul<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop<T, CreateBinaryExpression<"Add", Left, Right>>
      : R
    : never
  : T extends ConsumeToken<MinusToken, infer T>
  ? ParseMul<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseExprLoop<T, CreateBinaryExpression<"Sub", Left, Right>>
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
      ? ParseMulLoop<T, CreateBinaryExpression<"Multiply", Left, Right>>
      : R
    : never
  : T extends ConsumeToken<SlashToken, infer T>
  ? ParsePrimary<T> extends infer R
    ? R extends ParseResult<infer T, infer Right>
      ? ParseMulLoop<T, CreateBinaryExpression<"Divide", Left, Right>>
      : R
    : never
  : ParseResult<T, Left>;

export type Parse<T extends Tokens> = ParseExpr<T> extends infer R
  ? R extends ParseResult<[], infer N>
    ? N
    : R
  : never;

// Evaluation
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

// Put it all together

export type Calc<S extends string> = Tokenize<S> extends infer MaybeTokenizeError
  ? MaybeTokenizeError extends Tokens
    ? Parse<MaybeTokenizeError> extends infer MaybeParseError
      ? MaybeParseError extends ExpressionNode
        ? Evaluate<MaybeParseError> extends infer MaybeNaN
          ? MaybeNaN extends Nat
            ? ToNumber<MaybeNaN>
            : MaybeNaN
          : never
        : MaybeParseError
      : never
    : MaybeTokenizeError
  : never;
