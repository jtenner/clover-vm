import { AnyOfRule, AnyRule, BetweenInclusiveRule, ByteSink, EqualsRule, EveryRule, KeywordRule, ManyRule, OptionalRule, Range, Rule } from "byte-parse-as/assembly";
import { ArgumentsNode, BlockStatementNode, CommaExpressionNode, ExpressionNode, FunctionDeclarationStatementNode, FunctionExpressionNode, getTypeFromTypeName, IdentifierNode, Node, ParameterNode, ProgramNode, StatementNode, TernaryExpressionNode, Type, TypeNode, VariableDeclarationStatementNode, VariableDeclaratorNode } from "./ast";

export class EOF extends Rule {
  test(buffer: ByteSink, index: i32, range: Range): bool {
    if (buffer.byteLength == index) {
      range.start = index;
      range.end = index;
      return true;
    }
    return false;
  }

}

class Box<T> { constructor(public value: T) {} }

/**
 * Clover grammar is as follows:
 * 
 * Program -> Statement:*
 * 
 * Statement -> (FunctionStatement | VariableStatement | IfStatement | WhileStatement | ExpressionStatement)
 * 
 * FunctionStatement -> FunctionExpression
 * 
 * VariableStatement -> "let" _ List[VariableDeclarator, _ "," _] _ ("," _):? ";";
 * 
 * VariableDeclarator -> Identifier _ "=" _ Expression
 * 
 * ExpressionStatiement -> Expression _ ";"
 * 
 * Expression -> CommaExpression
 * 
 * CommaExpression -> List[TernaryExpression, _ "," _] | TernaryExpression
 * 
 * TernaryExpression -> (AssignmentExpression _ "?" _ Expression _ ":" _ Expression) | AssignmentExpression
 * 
 * AssignmentExpression -> Identifier _ "=" _ Expression | NullishExpression
 * 
 * NullishExpression -> OrExpression _ "??" _ NullishExpression | OrExpression
 * 
 * OrExpression -> AndExpression _ "||" _ OrExpression | AndExpression
 * 
 * AndExpression -> BitwiseOrExpression _ "&&" _ AndExpression | BitwiseOrExpression
 * 
 * BitwiseOrExpression -> XORExpression _ "|" _ BitwiseOrExpression | XORExpression
 * 
 * XORExpression -> BitwiseAndExpression _ "^" _ XORExpression | BitwiseAndExpression
 * 
 * BitwiseAndExpression -> EqualityExpresion _ "^" _ BitwiseAndExpression | EqualityExpresion
 * 
 * EqualityExpresion -> ComparisonExpression _ ("==" | "!=") _ EqualityExpresion | ComparisonExpression
 * 
 * ComparisonExpression -> BitShiftExpression _ ("in" | ">=" | "<=" | ">" | "<") _ ComparisonExpression | BitShiftExpression
 * 
 * BitShiftExpression -> AdditionExpression _ (">>>" | "<<" | ">>") _ BitShiftExpression | AdditionExpression
 * 
 * AdditionExpression -> MultiplicationExpression _ ("+" | "-") _ AdditionExpression | MultiplicationExpression
 * 
 * MultiplicationExpression -> ExponentiationExpression _ ("*" | "/" | "%") _ MultiplicationExpression | ExponentiationExpression
 * 
 * ExponentiationExpression -> NotExpression _ "**" _ ExponentiationExpression | NotExpression
 * 
 * NotExpression -> ("~" | "!") _ NotExpression | PropertyAccessExpression
 * 
 * PropertyAccessExpression -> (GroupExpression | Identifier | StringLiteral) _ (
 *    "." _ PropertyAccessExpression
 *  | "[" _ Expression _ "]"
 *  | "(" _ Arguments _ ")"
 * ):+ | PrimaryExpression
 * 
 * PrimaryExpression -> GroupExpression | Identifier | Literal | FunctionExpression
 * 
 * GroupExpression -> "(" _ Expression _ ")"
 * 
 * FunctionExpression ->  "func" _ Identifier _ "(" _ List[Parameter, _ "," _] _ ("," _):? ")" _ "->"  _ Block
 * 
 * List[A, Sep] -> A (SEP A):*  
 * 
 */
export class CloverParser {
  eof: Rule = new EOF();
  ows: Rule = new OptionalRule(
    new ManyRule(
      new AnyRule([
        new AnyOfRule(" \t\r\n"),
        new KeywordRule("\uFEFF"),
      ]),
    ),
  );
  rws: Rule = new ManyRule(
    new AnyRule([
      new AnyOfRule(" \t\r\n"),
      new KeywordRule("\uFEFF"),
    ]),
  );
  identifier: Rule = new EveryRule([
    new AnyRule([
      new BetweenInclusiveRule(0x41, 0x5A),
      new BetweenInclusiveRule(0x61, 0x7A),
      new AnyOfRule("$_")
    ]),
    new OptionalRule(
      new ManyRule(
        new AnyRule([
          new BetweenInclusiveRule(0x41, 0x5A),
          new BetweenInclusiveRule(0x61, 0x7A),
          new BetweenInclusiveRule(0x30, 0x39),
          new AnyOfRule("$_")
        ]),
      ),
    ),
  ]);
  func: Rule = new KeywordRule("func");
  open_paren: Rule = new EqualsRule(0x28);
  close_paren: Rule = new EqualsRule(0x29);
  open_brace: Rule = new EqualsRule(0x7B);
  close_brace: Rule = new EqualsRule(0x7D);
  comma: Rule = new EqualsRule(0x2C);
  colon: Rule = new EqualsRule(0x3A);
  semicolon: Rule = new EqualsRule(0x3B);
  equals: Rule = new EqualsRule(0x3D);
  typeKeyword: Rule = new AnyRule([
    new KeywordRule("void"),
    new KeywordRule("object"),
    new KeywordRule("string"),
    new KeywordRule("func"),
    new KeywordRule("i32"),
    new KeywordRule("u32"),
    new KeywordRule("i64"),
    new KeywordRule("u64"),
    new KeywordRule("f32"),
    new KeywordRule("f64"),
  ]);
  whitespacePaddedColon: Rule = new EveryRule([
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
    new EqualsRule(0x3A),
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
  ]);
  whitespacePaddedComma: Rule = new EveryRule([
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
    new EqualsRule(0x2C),
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
  ]);
  whitespacePaddedQuestion: Rule = new EveryRule([
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
    new EqualsRule(0x3F),
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
  ]);
  whitespaceComma: Rule = new EveryRule([
    new OptionalRule(new ManyRule(new AnyOfRule(" \t\r\n"))),
    new EqualsRule(0x2C),
  ]);
  let: Rule = new KeywordRule("let");

  result: Box<Node | null> = new Box(null);

  parse(buffer: ByteSink): ProgramNode | null {
    let range = new Range(0, 0, buffer);
    if (this.parseProgram(buffer, 0, range)) {
      return <ProgramNode>this.result.value!;
    }
    return null;
  }

  parseProgram(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    // skip leading whitespace
    if (this.ows.test(buffer, index, range)) {
      index = range.end;
    }

    if (this.parseStatements(buffer, index, range)) {
      let statementsRange = range.copy();
      index = range.end;
      let block = <BlockStatementNode>this.result.value!;
      this.result.value = new ProgramNode(statementsRange, block);
    }

    // skip ending whitespace
    if (this.ows.test(buffer, index, range)) {
      index = range.end;
    }

    // eof
    if (this.eof.test(buffer, index, range)) {
      range.start = start;
      range.end = index;
      return true;
    }
    return false;
  }

  parseStatements(buffer: ByteSink, index: i32, range: Range): bool {
    let statements = [] as StatementNode[];
    let start = index;
    let end = 0;

    while (true) {
      if (this.parseStatement(buffer, index, range)) {
        statements.push(<StatementNode>this.result.value!);
        end = range.end;
        // advance the index
        index = range.end;

        // parse the whitespace between the statements and ignore it
        if (this.ows.test(buffer, index, range)) {
          index = range.end;
        }
        continue;
      }
      break;
    }

    // set the range and return
    range.start = start;
    range.end = end;
    this.result.value = new BlockStatementNode(range.copy(), statements);
    return true;
  }

  // Statement -> FunctionStatement | VariableStatement | IfStatement | WhileStatement | ExpressionStatement
  parseStatement(buffer: ByteSink, index: i32, range: Range): bool {
    return this.parseFunctionDeclarationStatement(buffer, index, range)
      || this.parseVariableDeclarationStatement(buffer, index, range)
      || this.parseIfStatement(buffer, index, range)
      || this.parseWhileStatement(buffer, index, range)
      || this.parseBlock(buffer, index, range)
      || this.parseExpressionStatement(buffer, index, range);
  }

  parseFunctionDeclarationStatement(buffer: ByteSink, index: i32, range: Range): bool {
    if (this.parseFunctionExpression(buffer, index, range)) {
      let func = <FunctionExpressionNode>this.result.value!;
      let declaration = new FunctionDeclarationStatementNode(range.copy(), func);
      this.result.value = declaration;
      return true;
    }
    return false;
  }

  parseIdentifier(buffer: ByteSink, index: i32, range: Range): bool {
    if (this.identifier.test(buffer, index, range)) {
      let name = range.toString();
      this.result.value = new IdentifierNode(range.copy(), name);
      return true;
    }
    return false;
  }

  parseFunctionExpression(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;
    if (this.func.test(buffer, index, range)) {
      index = range.end;
      
      // required whitespace
      if (this.rws.test(buffer, index, range)) {
        index = range.end;

        if (this.parseIdentifier(buffer, index, range)) {
          let name = <IdentifierNode>this.result.value!;
          index = range.end;

          if (this.ows.test(buffer, index, range)) {
            index = range.end;
          }

          // open paren
          if (this.open_paren.test(buffer, index, range)) {
            index = range.end;

            // parse the whitespace between the statements and ignore it
            if (this.ows.test(buffer, index, range)) {
              index = range.end;
            }

            if (this.parseArguments(buffer, index, range)) {
              index = range.end;
              let args = <ArgumentsNode | null>this.result.value;

              if (this.ows.test(buffer, index, range)) {
                index = range.end;
              }

              if (this.close_paren.test(buffer, index, range)) {
                index = range.end;

                if (this.ows.test(buffer, index, range)) {
                  index = range.end;
                }

                // parse block
                if (this.parseBlock(buffer, index, range)) {
                  let body = <BlockStatementNode>this.result.value!;
                  range.start = start;
                  this.result.value = new FunctionExpressionNode(range.copy(), name, args, body);
                  return true;
                }
              }
            }
          }
        }
      }
    }
    return false;
  }

  parseBlock(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.open_brace.test(buffer, index, range)) {
      index = range.end;

      if (this.ows.test(buffer, index, range)) {
        index = range.end;
      }

      if (this.parseStatements(buffer, index, range)) {
        let result = this.result.value!;
        this.result.value = null;
        index = range.end;

        if (this.ows.test(buffer, index, range)) {
          index = range.end;
        }

        if (this.close_brace.test(buffer, index, range)) {
          range.start = start;
          this.result.value = result;
          return true;
        }
      }
    }

    return false;
  }

  parseArguments(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.parseParameterNode(buffer, index, range)) {
      let args = [] as ParameterNode[];
      index = range.end;
      args.push(<ParameterNode>this.result.value!);

      while (true) {
        // * List[A, Sep] -> A (SEP A):*  
        if (this.whitespacePaddedComma.test(buffer, index, range)) {
          let next = range.end;
          
          if (this.parseParameterNode(buffer, next, range)) {
            args.push(<ParameterNode>this.result.value!);
            index = range.end; // we advance the index only on a match
            continue;
          }
        }
        break;
      }

      if (this.whitespaceComma.test(buffer, index, range)) {
        index = range.end;
      }
      range.start = start;
      this.result.value = new ArgumentsNode(range.copy(), args);
      return true;
    } else {
      this.result.value = null;
      range.start = index;
      range.end = index;
      this.result.value = new ArgumentsNode(range.copy(), null);
      return true;
    }
  }

  parseParameterNode(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.parseIdentifier(buffer, index, range)) {
      index = range.end;
      let name = <IdentifierNode>this.result.value!;

      if (this.ows.test(buffer, index, range)) {
        index = range.end;
      }

      if (this.colon.test(buffer, index, range)) {
        index = range.end;

        if (this.ows.test(buffer, index, range)) {
          index = range.end;
        }

        if (this.parseType(buffer, index, range)) {
          let parameterType = <TypeNode>this.result.value!;
          if (parameterType.type == Type.void || parameterType.type == Type.invalid) return false;
          range.start = start;
          index = range.end;
          this.result.value = new ParameterNode(range.copy(), name, parameterType);
          return true;
        }
      }
    }

    return false;
  }

  parseType(buffer: ByteSink, index: i32, range: Range): bool {
    if (this.typeKeyword.test(buffer, index, range)) {
      this.result.value = new TypeNode(range.copy(), getTypeFromTypeName(range.toString()));
      return true;
    }
    return false;
  }

  parseVariableDeclarationStatement(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.let.test(buffer, index, range)) {
      index = range.end;
      let declarators = [] as VariableDeclaratorNode[];

      if (this.rws.test(buffer, index, range)) {
        index = range.end;

        if (this.parseVariableDeclarator(buffer, index, range)) {
          index = range.end;
          declarators.push(<VariableDeclaratorNode>this.result.value!);
          
          while (true) {
            if (this.whitespacePaddedComma.test(buffer, index, range)) {
              let next = range.end;
              
              if (this.parseVariableDeclarator(buffer, next, range)) {
                declarators.push(<VariableDeclaratorNode>this.result.value!);
                index = range.end;
                continue;
              }
            }
            break;
          }

          if (this.ows.test(buffer, index, range)) {
            index = range.end;
          }

          if (this.semicolon.test(buffer, index, range)) {
            range.start = start;
            this.result.value = new VariableDeclarationStatementNode(range.copy(), declarators);
            return true;
          }
        }
      }
    }
    return false;
  }

  parseVariableDeclarator(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;
    let variableType: TypeNode | null = null;

    if (this.parseIdentifier(buffer, index, range)) {
      let name = <IdentifierNode>this.result.value!;
      index = range.end;

      if (this.ows.test(buffer, index, range)) {
        index = range.end;
      }

      if (this.colon.test(buffer, index, range)) {
        index = range.end;

        if (this.ows.test(buffer, index, range)) {
          index = range.end;
        }

        if (this.parseType(buffer, index, range)) {
          index = range.end;
          variableType = <TypeNode>this.result.value!;
        } else {
          return false;
        }
      }

      if (this.equals.test(buffer, index, range)) {
        index = range.end;

        if (this.ows.test(buffer, index, range)) {
          index = range.end;
        }

        if (this.parseExpression(buffer, index, range)) {
          index = range.end;
          let expression = <ExpressionNode>this.result.value!;

          range.start = start;
          this.result.value = new VariableDeclaratorNode(range.copy(), name, variableType, expression);
          return true;
        }
      }
    }
    return false;
  }

  parseExpression(buffer: ByteSink, index: i32, range: Range): bool {
    return this.parseCommaExpression(buffer, index, range);
  }

  parseCommaExpression(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.parseTernaryExpression(buffer, index, range)) {
      let first = <ExpressionNode>this.result.value!;
      index = range.end;

      let expressions = [] as ExpressionNode[];

      while (true) {
        if (this.whitespacePaddedComma.test(buffer, index, range)) {
          let next = range.end;

          if (this.parseTernaryExpression(buffer, next, range)) {
            index = range.end;
            expressions.push(this.result.value!);
            continue;
          }
        }
        break;
      }

      if (expressions.length > 0) {
        expressions.unshift(first);
        range.start = start;
        this.result.value = new CommaExpressionNode(range.copy(), expressions);
      } else {
        range.start = first.range.start;
        range.end = first.range.end;
        this.result.value = first;
      }

      return true;
    }
    return false;
  }

  parseTernaryExpression(buffer: ByteSink, index: i32, range: Range): bool {
    let start = index;

    if (this.parseAssignmentExpression(buffer, index, range)) {
      index = range.end;
      let condition = <ExpressionNode>this.result.value!;

      if (this.whitespacePaddedQuestion.test(buffer, index, range)) {
        index = range.end;

        if (this.parseExpression(buffer, index, range)) {
          index = range.end;
          let truthy = <ExpressionNode>this.result.value!;

          if (this.whitespacePaddedColon.test(buffer, index, range)) {
            index = range.end;
            
            if (this.parseExpression(buffer, index, range)) {
              let falsy = <ExpressionNode>this.result.value!;
              range.start = start;

              this.result.value = new TernaryExpressionNode(range.copy(), condition, truthy, falsy);
              return true;
            }
          }
        }
      }

      range.start = condition.range.start;
      range.end = condition.range.end;
      this.result.value = condition;
      return true;
    }
    return false;
  }
}