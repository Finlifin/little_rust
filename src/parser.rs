use std::collections::HashMap;

// Import Lexer and TokenType from the previous step
use crate::lexer::{Lexer, Token, TokenType};

// Re-export Type and Expr for Parser
pub use crate::bidir::{Expr, Type};
// pub use self::{Block, Statement, LetStatement, IfStatement, ReturnStatement, ExpressionStatement};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        actual: Token,
        line: usize,
        column: usize,
    },
    UnexpectedEOF {
        line: usize,
        column: usize,
    },
    CustomError {
        message: String,
        line: usize,
        column: usize,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, actual, line, column } => {
                write!(f, "At line {}, column {}: Expected {:?} but found {:?}", 
                    line, column, expected, actual.token_type)
            }
            ParseError::UnexpectedEOF { line, column } => {
                write!(f, "At line {}, column {}: Unexpected end of file", line, column)
            }
            ParseError::CustomError { message, line, column } => {
                write!(f, "At line {}, column {}: {}", line, column, message)
            }
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    pub lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    _phantom: std::marker::PhantomData<&'a ()>, // Add lifetime parameter
}

impl<'a> Parser<'a> {
    pub fn new(input: String) -> ParseResult<Parser<'a>> {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Ok(Parser {
            lexer,
            current_token,
            peek_token,
            _phantom: std::marker::PhantomData,
        })
    }

    fn advance_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn consume_token(&mut self, expected_type: TokenType) -> ParseResult<()> {
        if self.current_token.token_type == expected_type {
            self.advance_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected_type,
                actual: self.current_token.clone(),
                line: self.current_token.line,
                column: self.current_token.column,
            })
        }
    }

    fn current_token_type(&self) -> TokenType {
        self.current_token.token_type.clone()
    }

    fn peek_token_type(&self) -> TokenType {
        self.peek_token.token_type.clone()
    }

    // 添加帮助方法来获取当前位置
    fn current_position(&self) -> (usize, usize) {
        (self.current_token.line, self.current_token.column)
    }

    // --- Parser Functions for Grammar Productions ---

    // program ::= item* EOF
    pub fn try_program(&mut self) -> ParseResult<Vec<Item>> {
        let mut items = Vec::new();
        while self.current_token_type() != TokenType::EOF {
            items.push(self.try_item()?);
        }
        self.consume_token(TokenType::EOF)?;
        Ok(items)
    }

    // item ::= function_definition | struct_definition
    fn try_item(&mut self) -> ParseResult<Item> {
        match self.current_token_type() {
            TokenType::Fn => Ok(Item::Function(self.try_function_definition()?)),
            TokenType::Struct => Ok(Item::StructDef(self.try_struct_definition()?)),
            _ => Err(ParseError::UnexpectedToken {
                expected: TokenType::Fn, // Or Struct, but Fn is more likely start of program
                actual: self.current_token.clone(),
                line: self.current_token.line,
                column: self.current_token.column,
            }),
        }
    }

    // function_definition ::= FN Identifier type_parameters? parameters return_type block
    fn try_function_definition(&mut self) -> ParseResult<FunctionDef> {
        self.consume_token(TokenType::Fn)?;
        let name = match self.current_token_type() {
            TokenType::Identifier(ref ident_name) => {
                let name = ident_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("function name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };

        let type_params = if self.current_token_type() == TokenType::TypeParamStart {
            Some(self.try_type_parameters()?)
        } else {
            None
        };

        let params = self.try_parameters()?;
        let return_type = self.try_return_type()?;
        let body = self.try_block()?;

        Ok(FunctionDef {
            name,
            type_params,
            params,
            return_type,
            body,
        })
    }

    // type_parameters ::= '<' Identifier (',' Identifier)* '>'
    fn try_type_parameters(&mut self) -> ParseResult<Vec<String>> {
        self.consume_token(TokenType::TypeParamStart)?;
        let mut params = Vec::new();
        loop {
            match self.current_token_type() {
                TokenType::Identifier(ref type_param_name) => {
                    params.push(type_param_name.clone());
                    self.advance_token();
                    if self.current_token_type() == TokenType::Comma {
                        self.consume_token(TokenType::Comma)?;
                    } else if self.current_token_type() == TokenType::TypeParamEnd {
                        break;
                    } else {
                        let (line, column) = self.current_position();
                        return Err(ParseError::UnexpectedToken {
                            expected: TokenType::Comma, // or TypeParamEnd
                            actual: self.current_token.clone(),
                            line,
                            column,
                        });
                    }
                }
                _ => {
                    let (line, column) = self.current_position();
                    return Err(ParseError::UnexpectedToken {
                        expected: TokenType::Identifier("type parameter name".to_string()),
                        actual: self.current_token.clone(),
                        line,
                        column,
                    })
                }
            }
        }
        self.consume_token(TokenType::TypeParamEnd)?;
        Ok(params)
    }

    // parameters ::= '(' (parameter (',' parameter)* )? ')'
    fn try_parameters(&mut self) -> ParseResult<Vec<Parameter>> {
        self.consume_token(TokenType::OpenParen)?;
        let mut params = Vec::new();
        if self.current_token_type() != TokenType::CloseParen {
            loop {
                params.push(self.try_parameter()?);
                if self.current_token_type() == TokenType::Comma {
                    self.consume_token(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }
        self.consume_token(TokenType::CloseParen)?;
        Ok(params)
    }

    // parameter ::= Identifier ':' type
    fn try_parameter(&mut self) -> ParseResult<Parameter> {
        let name = match self.current_token_type() {
            TokenType::Identifier(ref param_name) => {
                let name = param_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("parameter name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };
        self.consume_token(TokenType::Colon)?;
        let param_type = self.try_type()?;
        Ok(Parameter { name, param_type })
    }

    // return_type ::= '->' type | /* empty */
    fn try_return_type(&mut self) -> ParseResult<Type> {
        if self.current_token_type() == TokenType::Arrow
        {
            self.consume_token(TokenType::Arrow)?;
            self.consume_token(TokenType::TypeParamEnd)?;
            self.try_type()
        } else {
            Ok(Type::Unknown) // Void or inferred return type
        }
    }

    // block ::= '{' statement* '}'
    fn try_block(&mut self) -> ParseResult<Block> {
        self.consume_token(TokenType::OpenBrace)?;
        let mut statements = Vec::new();
        while self.current_token_type() != TokenType::CloseBrace {
            statements.push(self.try_statement()?);
        }
        self.consume_token(TokenType::CloseBrace)?;
        Ok(Block { statements })
    }

    // statement ::= let_statement | if_statement | return_statement | expression_statement
    fn try_statement(&mut self) -> ParseResult<Statement> {
        match self.current_token_type() {
            TokenType::Let => Ok(Statement::Let(self.try_let_statement()?)),
            TokenType::If => Ok(Statement::If(self.try_if_statement()?)),
            TokenType::Return => Ok(Statement::Return(self.try_return_statement()?)),
            _ => Ok(Statement::Expression(self.try_expression_statement()?)), // Default to expression statement
        }
    }

    // let_statement ::= LET Identifier (':' type)? '=' expression ';'
    fn try_let_statement(&mut self) -> ParseResult<LetStatement> {
        self.consume_token(TokenType::Let)?;
        let name = match self.current_token_type() {
            TokenType::Identifier(ref var_name) => {
                let name = var_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("variable name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };
        let declared_type = if self.current_token_type() == TokenType::Colon {
            self.consume_token(TokenType::Colon)?;
            Some(self.try_type()?)
        } else {
            None
        };
        self.consume_token(TokenType::Equals)?;
        let initializer = self.try_expression()?;
        self.consume_token(TokenType::Semicolon)?;
        Ok(LetStatement {
            name,
            declared_type,
            initializer,
        })
    }

    // if_statement ::= IF expression block ('ELSE' block)?
    fn try_if_statement(&mut self) -> ParseResult<IfStatement> {
        self.consume_token(TokenType::If)?;
        let condition = self.try_expression()?;
        let then_block = self.try_block()?;
        let else_block = if self.current_token_type() == TokenType::Else {
            self.consume_token(TokenType::Else)?;
            Some(self.try_block()?)
        } else {
            None
        };
        Ok(IfStatement {
            condition,
            then_block,
            else_block,
        })
    }

    // return_statement ::= RETURN expression? ';'
    fn try_return_statement(&mut self) -> ParseResult<ReturnStatement> {
        self.consume_token(TokenType::Return)?;
        let return_value = if self.current_token_type() != TokenType::Semicolon {
            Some(self.try_expression()?)
        } else {
            None
        };
        self.consume_token(TokenType::Semicolon)?;
        Ok(ReturnStatement { return_value })
    }

    // expression_statement ::= expression ';'
    fn try_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.try_expression()?;
        self.consume_token(TokenType::Semicolon)?;
        Ok(ExpressionStatement { expression })
    }

    // struct_definition ::= STRUCT Identifier type_parameters? '{' struct_fields '}'
    fn try_struct_definition(&mut self) -> ParseResult<StructDef> {
        self.consume_token(TokenType::Struct)?;
        let name = match self.current_token_type() {
            TokenType::Identifier(ref struct_name) => {
                let name = struct_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("struct name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };

        let type_params = if self.current_token_type() == TokenType::TypeParamStart {
            Some(self.try_type_parameters()?)
        } else {
            None
        };

        self.consume_token(TokenType::OpenBrace)?;
        let fields = self.try_struct_fields()?;
        self.consume_token(TokenType::CloseBrace)?;

        Ok(StructDef {
            name,
            type_params,
            fields,
        })
    }

    // struct_fields ::= (struct_field (',' struct_field)* )?
    fn try_struct_fields(&mut self) -> ParseResult<HashMap<String, Type>> {
        let mut fields = HashMap::new();
        if self.current_token_type() != TokenType::CloseBrace {
            loop {
                let field = self.try_struct_field()?;
                fields.insert(field.name, field.field_type);
                if self.current_token_type() == TokenType::Comma {
                    self.consume_token(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }
        Ok(fields)
    }

    // struct_field ::= Identifier ':' type
    fn try_struct_field(&mut self) -> ParseResult<StructField> {
        let name = match self.current_token_type() {
            TokenType::Identifier(ref field_name) => {
                let name = field_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("field name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                });
            }
        };
        self.consume_token(TokenType::Colon)?;
        let field_type = self.try_type()?;
        Ok(StructField { name, field_type })
    }

    // type ::= IntType | BoolType | Identifier (type_arguments?)
    fn try_type(&mut self) -> ParseResult<Type> {
        match self.current_token_type() {
            TokenType::IntType => {
                self.advance_token();
                Ok(Type::Int)
            }
            TokenType::BoolType => {
                self.advance_token();
                Ok(Type::Bool)
            }
            TokenType::Identifier(ref type_name) => {
                let name = type_name.clone();
                self.advance_token();
                // type_arguments ::= '<' type (',' type)* '>'
                if self.current_token_type() == TokenType::TypeParamStart {
                    self.consume_token(TokenType::TypeParamStart)?;
                    let mut type_args = Vec::new();
                    loop {
                        type_args.push(self.try_type()?);
                        if self.current_token_type() == TokenType::Comma {
                            self.consume_token(TokenType::Comma)?;
                        } else if self.current_token_type() == TokenType::TypeParamEnd {
                            break;
                        } else {
                            let (line, column) = self.current_position();
                            return Err(ParseError::UnexpectedToken {
                                expected: TokenType::Comma, // or TypeParamEnd
                                actual: self.current_token.clone(),
                                line,
                                column,
                            });
                        }
                    }
                    self.consume_token(TokenType::TypeParamEnd)?;
                    // Simplified generic type handling - assuming Identifier<Type, Type, ...> is a struct
                    let fields = HashMap::new(); // For now, assume no fields are parsed here in type context
                    Ok(Type::Struct {
                        name: name.clone(),
                        fields,
                    }) // Treat generic type as struct for simplicity in this example
                } else {
                    Ok(Type::Generic(name)) // Or could be struct name, need symbol table for full resolution
                }
            }
            _ => {
                let (line, column) = self.current_position();
                Err(ParseError::UnexpectedToken {
                    expected: TokenType::IntType, // Or BoolType, or Identifier, but IntType is just an example
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        }
    }

    // --- Pratt Parser for Expressions ---
    fn try_expression(&mut self) -> ParseResult<Expr> {
        self.try_pratt_expression(0)
    }

    fn try_pratt_expression(&mut self, precedence: i32) -> ParseResult<Expr> {
        let mut left_expr = self.try_primary_expression()?;

        while precedence < self.get_operator_precedence(&self.current_token_type()) {
            left_expr = self.try_infix_expression(left_expr)?;
        }

        Ok(left_expr)
    }

    fn try_infix_expression(&mut self, left_expr: Expr) -> ParseResult<Expr> {
        let operator_token = self.current_token.clone();
        let operator_precedence = self.get_operator_precedence(&operator_token.token_type);
        self.advance_token(); // Consume operator

        let right_expr = self.try_pratt_expression(operator_precedence)?; // Parse right operand with precedence

        match operator_token.token_type {
            TokenType::Plus => Ok(Expr::FnCall {
                name: "+".to_string(),
                args: vec![left_expr, right_expr],
            }), // Assuming '+' is resolved to a function
            TokenType::Minus => Ok(Expr::FnCall {
                name: "-".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::Star => Ok(Expr::FnCall {
                name: "*".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::Slash => Ok(Expr::FnCall {
                name: "/".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::EqualsEquals => Ok(Expr::FnCall {
                name: "==".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::NotEquals => Ok(Expr::FnCall {
                name: "!=".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::GreaterThan => Ok(Expr::FnCall {
                name: ">".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::LessThan => Ok(Expr::FnCall {
                name: "<".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::And => Ok(Expr::FnCall {
                name: "&&".to_string(),
                args: vec![left_expr, right_expr],
            }),
            TokenType::Or => Ok(Expr::FnCall {
                name: "||".to_string(),
                args: vec![left_expr, right_expr],
            }),

            _ => Err(ParseError::CustomError {
                message: format!("Unexpected infix operator: {:?}", operator_token.token_type),
                line: self.current_token.line,
                column: self.current_token.column,
            }), // Should not happen based on precedence check
        }
    }

    fn get_operator_precedence(&self, token_type: &TokenType) -> i32 {
        match token_type {
            TokenType::Or => 10,
            TokenType::And => 20,
            TokenType::EqualsEquals
            | TokenType::NotEquals
            | TokenType::GreaterThan
            | TokenType::LessThan => 30,
            TokenType::Plus | TokenType::Minus => 40,
            TokenType::Star | TokenType::Slash => 50,
            _ => 0, // Default precedence for non-operators or lower precedence
        }
    }

    // primary_expression ::= Identifier | LiteralInt | LiteralBool | '(' expression ')' | struct_literal | function_call
    fn try_primary_expression(&mut self) -> ParseResult<Expr> {
        let mut expr = match self.current_token_type() {
            TokenType::Identifier(..) => {
                let ident_name = self.current_token.lexeme.clone();
                self.advance_token();
                // Function call check: Identifier '(' ... ')'
                if self.current_token_type() == TokenType::OpenParen {
                    self.try_function_call(ident_name)?
                } else {
                    Expr::Variable(ident_name) // Just a variable
                }
            }
            TokenType::IntLiteral(val) => {
                let literal_value = val;
                self.advance_token();
                Expr::LiteralInt(literal_value)
            }
            TokenType::BoolLiteral(val) => {
                let literal_value = val;
                self.advance_token();
                Expr::LiteralBool(literal_value)
            }
            TokenType::OpenParen => {
                self.consume_token(TokenType::OpenParen)?;
                let expr = self.try_expression()?;
                self.consume_token(TokenType::CloseParen)?;
                expr
            }
            TokenType::Struct => self.try_struct_literal()?,
            TokenType::Return => {
                // Handle 'return' keyword as primary expression start for return statements
                self.try_return_expression_primary()?
            }

            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("identifier, literal, or '('".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };

        // Handle field access chain
        while self.current_token_type() == TokenType::Dot {
            self.consume_token(TokenType::Dot)?;
            match self.current_token_type() {
                TokenType::Identifier(field_name) => {
                    let field = field_name.clone();
                    self.advance_token();
                    expr = Expr::Select {
                        base: Box::new(expr),
                        field,
                    };
                }
                _ => {
                    let (line, column) = self.current_position();
                    return Err(ParseError::UnexpectedToken {
                        expected: TokenType::Identifier("field name".to_string()),
                        actual: self.current_token.clone(),
                        line,
                        column,
                    });
                }
            }
        }

        Ok(expr)
    }

    // return_expression_primary ::= RETURN expression?
    fn try_return_expression_primary(&mut self) -> ParseResult<Expr> {
        self.consume_token(TokenType::Return)?;
        let return_value = if self.current_token_type() != TokenType::Semicolon
            && self.current_token_type() != TokenType::CloseBrace
        {
            // Check for ';' or '}' to end return expr
            Some(self.try_expression()?)
        } else {
            None
        };
        match return_value {
            Some(expr) => Ok(Expr::Return(Box::new(expr))),
            None => Ok(Expr::Return(Box::new(Expr::LiteralBool(true)))), // Placeholder for void return, replace with proper Unit type later
        }
    }

    // struct_literal ::= STRUCT Identifier '{' (field_init (',' field_init)* )? '}'
    fn try_struct_literal(&mut self) -> ParseResult<Expr> {
        self.consume_token(TokenType::Struct)?;
        let name = match self.current_token_type() {
            TokenType::Identifier(ref struct_name) => {
                let name = struct_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("struct name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };
        self.consume_token(TokenType::OpenBrace)?;
        let fields = self.try_field_initializers()?;
        self.consume_token(TokenType::CloseBrace)?;
        Ok(Expr::StructLiteral { name, fields })
    }

    // field_initializers ::= (field_init (',' field_init)* )?
    fn try_field_initializers(&mut self) -> ParseResult<HashMap<String, Expr>> {
        let mut fields = HashMap::new();
        if self.current_token_type() != TokenType::CloseBrace {
            loop {
                let field_init = self.try_field_initializer()?;
                fields.insert(field_init.name, field_init.expr);
                if self.current_token_type() == TokenType::Comma {
                    self.consume_token(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }
        Ok(fields)
    }

    // field_initializer ::= Identifier ':' expression
    fn try_field_initializer(&mut self) -> ParseResult<FieldInitializer> {
        let name = match self.current_token_type() {
            TokenType::Identifier(ref field_name) => {
                let name = field_name.clone();
                self.advance_token();
                name
            }
            _ => {
                let (line, column) = self.current_position();
                return Err(ParseError::UnexpectedToken {
                    expected: TokenType::Identifier("field name".to_string()),
                    actual: self.current_token.clone(),
                    line,
                    column,
                })
            }
        };
        self.consume_token(TokenType::Colon)?;
        let expr = self.try_expression()?;
        Ok(FieldInitializer { name, expr })
    }

    // function_call ::= Identifier '(' (arguments)? ')'
    fn try_function_call(&mut self, name: String) -> ParseResult<Expr> {
        self.consume_token(TokenType::OpenParen)?;
        let args = self.try_arguments()?;
        self.consume_token(TokenType::CloseParen)?;
        Ok(Expr::FnCall { name, args })
    }

    // arguments ::= expression (',' expression)*
    fn try_arguments(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();
        if self.current_token_type() != TokenType::CloseParen {
            loop {
                args.push(self.try_expression()?);
                if self.current_token_type() == TokenType::Comma {
                    self.consume_token(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }
        Ok(args)
    }

    // 添加便利方法用于格式化错误消息
    pub fn format_error(&self, error: &ParseError) -> String {
        let lines: Vec<&str> = self.lexer.input.lines().collect();
        match error {
            ParseError::UnexpectedToken { line, column, .. } |
            ParseError::UnexpectedEOF { line, column } |
            ParseError::CustomError { line, column, .. } => {
                let error_line = if *line <= lines.len() {
                    lines[line - 1]
                } else {
                    "<end of file>"
                };
                
                format!("{}\n{}\n{}^",
                    error,
                    error_line,
                    " ".repeat(*column - 1)
                )
            }
        }
    }
}

// --- AST Node Definitions ---
#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Function(FunctionDef),
    StructDef(StructDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub type_params: Option<Vec<String>>,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    If(IfStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: String,
    pub declared_type: Option<Type>,
    pub initializer: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDef {
    pub name: String,
    pub type_params: Option<Vec<String>>,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldInitializer {
    pub name: String,
    pub expr: Expr,
}
