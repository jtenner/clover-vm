import { Range } from "byte-parse-as/assembly";

export const enum Type {
  invalid,
  void,
  object,
  string,
  func,
  i32,
  u32,
  i64,
  u64,
  f32,
  f64,
}

export function getTypeFromTypeName(str: string): Type {
  switch (str) {
    case "void": return Type.void;
    case "object": return Type.object;
    case "string": return Type.string;
    case "func": return Type.func;
    case "i32": return Type.i32;
    case "u32": return Type.u32;
    case "i64": return Type.i64;
    case "u64": return Type.u64;
    case "f32": return Type.f32;
    case "f64": return Type.f64;
    default: return Type.invalid;
  }
}

export abstract class Node {
   constructor(
    public range: Range,
   ) {}
}

export class ProgramNode extends Node {
  constructor(
    range: Range,
    public body: BlockStatementNode,
  ) {
    super(range);
  }
}

export abstract class StatementNode extends Node {
  constructor(range: Range) {
    super(range);
  }
}

export class BlockStatementNode extends StatementNode {
  constructor(
    range: Range,
    public statements: StatementNode[] = [],
  ) {
    super(range);
  }
}

export abstract class ExpressionNode extends Node {
  constructor(
    range: Range,
  ) {
    super(range);
  }
}

export class FunctionExpressionNode extends ExpressionNode {
  constructor(
    range: Range,
    public name: IdentifierNode,
    public parameters: ArgumentsNode | null,
    public body: BlockStatementNode,
  ) {
    super(range);
  }
}

export class IdentifierNode extends Node {
  constructor(
    range: Range,
    public value: string,
  ) {
    super(range);
  }
}

export class FunctionDeclarationStatementNode extends StatementNode {
  constructor(
    range: Range,
    public func: FunctionExpressionNode,
  ) {
    super(range);
  }
}

export class TypeNode extends Node {
  constructor(
    range: Range,
    public type: Type,
  ) {
    super(range);
  }
}

export class ParameterNode extends Node {
  constructor(
    range: Range,
    public name: IdentifierNode,
    public type: TypeNode,
  ) {
    super(range);
  }
}

export class ArgumentsNode extends Node {
  constructor(
    range: Range,
    public args: ParameterNode[] | null,
  ) {
    super(range);
  }
}

export class VariableDeclaratorNode extends Node {
  constructor(
    range: Range,
    public name: IdentifierNode,
    public type: TypeNode | null,
    public value: ExpressionNode,
  ) {
    super(range);
  }
}

export class VariableDeclarationStatementNode extends StatementNode {
  constructor(
    range: Range,
    public declarators: VariableDeclaratorNode[],
  ) {
    super(range);
  }
}

export class CommaExpressionNode extends ExpressionNode {
  constructor(
    range: Range,
    public expressions: ExpressionNode[],
  ) {
    super(range);
  }
}

export class TernaryExpressionNode extends ExpressionNode {
  constructor(
    range: Range,
    public condition: ExpressionNode,
    public truthy: ExpressionNode,
    public falsy: ExpressionNode,
  ) {
    super(range);
  }
}
