#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Keywords
    Fn,
    Struct,
    Let,
    If,
    Else,
    Return,

    // Data Types
    IntType,
    BoolType,

    // Literals
    IntLiteral(i32),
    BoolLiteral(bool),
    Identifier(String),

    // Operators
    Equals,       // =
    Plus,         // +
    Minus,        // -
    Arrow,        // ->
    Star,         // *
    Slash,        // /
    GreaterThan,  // >
    LessThan,     // <
    EqualsEquals, // ==
    NotEquals,    // !=
    And,          // &&
    Or,           // ||
    Not,          // !
    QuestionMark, // ?
    Quote,        // `
    Dot,          // .

    // Punctuation
    OpenParen,      // (
    CloseParen,     // )
    OpenBrace,      // {
    CloseBrace,     // }
    OpenBracket,    // [
    CloseBracket,   // ]
    Comma,          // ,
    Colon,          // :
    Semicolon,      // ;
    TypeParamStart, // <
    TypeParamEnd,   // >

    // End of File
    EOF,

    // Error
    Error(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,   // 添加行号
    pub column: usize, // 添加列号
}

pub struct Lexer {
    pub input: String,
    chars: std::iter::Peekable<std::str::Chars<'static>>,
    byte_position: usize, // 跟踪字节位置
    current_char: Option<char>,
    line: usize,   // 添加行号跟踪
    column: usize, // 添加列号跟踪
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars = unsafe {
            std::mem::transmute::<
                std::iter::Peekable<std::str::Chars<'_>>,
                std::iter::Peekable<std::str::Chars<'static>>,
            >(input.chars().peekable())
        };

        Lexer {
            input,
            chars,
            byte_position: 0,
            current_char: None,
            line: 1,
            column: 1,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn advance_char(&mut self) -> Option<char> {
        let next_char = self.chars.next();
        if let Some(c) = next_char {
            self.byte_position += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.current_char = Some(c);
        }
        next_char
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.advance_char();
        }
    }

    fn create_token(&self, token_type: TokenType, lexeme: String) -> Token {
        Token {
            token_type,
            lexeme,
            line: self.line,
            column: self.column,
        }
    }

    fn read_identifier(&mut self) -> Token {
        let start_pos = self.byte_position;
        let mut identifier = String::new();

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' || c > '\x7F' {
                // 支持非ASCII标识符
                identifier.push(c);
                self.advance_char();
            } else {
                break;
            }
        }

        let lexeme = identifier.clone();
        let token_type = match identifier.as_str() {
            "fn" => TokenType::Fn,
            "struct" => TokenType::Struct,
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "i32" => TokenType::IntType,
            "bool" => TokenType::BoolType,
            "true" => TokenType::BoolLiteral(true),
            "false" => TokenType::BoolLiteral(false),
            _ => TokenType::Identifier(identifier),
        };

        self.create_token(token_type, lexeme)
    }

    fn read_number(&mut self) -> Token {
        let mut number_str = String::new();
        while let Some(c) = self.peek_char() {
            if c.is_digit(10) {
                number_str.push(c);
                self.advance_char();
            } else {
                break;
            }
        }

        let token_type = match number_str.parse::<i32>() {
            Ok(value) => TokenType::IntLiteral(value),
            Err(_) => TokenType::Error(format!("Invalid number: {}", number_str)),
        };

        self.create_token(token_type, number_str)
    }

    // 添加新的辅助方法
    fn has_space_around(&mut self, position: usize) -> bool {
        let before = if position > 0 {
            self.input
                .chars()
                .nth(position - 1)
                .map_or(false, |c| c.is_whitespace())
        } else {
            true
        };

        let after = self.peek_char().map_or(true, |c| c.is_whitespace());

        before && after
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.peek_char() {
            Some(c) => match c {
                '`' => {
                    self.advance_char();
                    self.create_token(TokenType::Quote, "`".to_string())
                }
                '>' => {
                    let current_pos = self.byte_position;
                    self.advance_char();
                    if self.has_space_around(current_pos) {
                        self.create_token(TokenType::GreaterThan, ">".to_string())
                    } else {
                        self.create_token(TokenType::TypeParamEnd, ">".to_string())
                    }
                }
                '<' => {
                    let current_pos = self.byte_position;
                    self.advance_char();
                    if self.has_space_around(current_pos) {
                        self.create_token(TokenType::LessThan, "<".to_string())
                    } else {
                        self.create_token(TokenType::TypeParamStart, "<".to_string())
                    }
                }
                '?' => {
                    self.advance_char();
                    self.create_token(TokenType::QuestionMark, "?".to_string())
                }
                '(' => {
                    self.advance_char();
                    self.create_token(TokenType::OpenParen, "(".to_string())
                }
                ')' => {
                    self.advance_char();
                    self.create_token(TokenType::CloseParen, ")".to_string())
                }
                '{' => {
                    self.advance_char();
                    self.create_token(TokenType::OpenBrace, "{".to_string())
                }
                '}' => {
                    self.advance_char();
                    self.create_token(TokenType::CloseBrace, "}".to_string())
                }
                '[' => {
                    self.advance_char();
                    self.create_token(TokenType::OpenBracket, "[".to_string())
                }
                ']' => {
                    self.advance_char();
                    self.create_token(TokenType::CloseBracket, "]".to_string())
                }
                ',' => {
                    self.advance_char();
                    self.create_token(TokenType::Comma, ",".to_string())
                }
                ':' => {
                    self.advance_char();
                    self.create_token(TokenType::Colon, ":".to_string())
                }
                ';' => {
                    self.advance_char();
                    self.create_token(TokenType::Semicolon, ";".to_string())
                }
                '.' => {
                    self.advance_char();
                    self.create_token(TokenType::Dot, ".".to_string())
                }

                '=' => {
                    self.advance_char();
                    if self.peek_char() == Some('=') {
                        self.advance_char();
                        self.create_token(TokenType::EqualsEquals, "==".to_string())
                    } else {
                        self.create_token(TokenType::Equals, "=".to_string())
                    }
                }
                '+' => {
                    self.advance_char();
                    self.create_token(TokenType::Plus, "+".to_string())
                }
                '-' => {
                    let current_pos = self.byte_position;
                    self.advance_char();
                    if self.has_space_around(current_pos) {
                        self.create_token(TokenType::Minus, "-".to_string())
                    } else {
                        self.create_token(TokenType::Arrow, "->".to_string())
                    }
                }
                '*' => {
                    self.advance_char();
                    self.create_token(TokenType::Star, "*".to_string())
                }
                '/' => {
                    self.advance_char();
                    self.create_token(TokenType::Slash, "/".to_string())
                }
                '!' => {
                    self.advance_char();
                    if self.peek_char() == Some('=') {
                        self.advance_char();
                        self.create_token(TokenType::NotEquals, "!=".to_string())
                    } else {
                        self.create_token(TokenType::Not, "!".to_string())
                    }
                }
                '&' => {
                    self.advance_char();
                    if self.peek_char() == Some('&') {
                        self.advance_char();
                        self.create_token(TokenType::And, "&&".to_string())
                    } else {
                        self.create_token(
                            TokenType::Error(format!("Unexpected character: &")),
                            "&".to_string(),
                        )
                    }
                }
                '|' => {
                    self.advance_char();
                    if self.peek_char() == Some('|') {
                        self.advance_char();
                        self.create_token(TokenType::Or, "||".to_string())
                    } else {
                        self.create_token(
                            TokenType::Error(format!("Unexpected character: |")),
                            "|".to_string(),
                        )
                    }
                }

                c if c.is_digit(10) => self.read_number(),
                c if c.is_alphabetic() || c == '_' => self.read_identifier(),

                unexpected_char => {
                    self.advance_char();
                    self.create_token(
                        TokenType::Error(format!("Unexpected character: {}", unexpected_char)),
                        unexpected_char.to_string(),
                    )
                }
            },
            None => self.create_token(TokenType::EOF, "".to_string()),
        }
    }
}
