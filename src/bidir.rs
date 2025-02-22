use std::collections::HashMap;

// Simplified Type System
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Bool,
    Generic(String), // Type variable for generics
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },
    Unknown, // For type inference placeholders
    GenericVar(usize),  // Add numeric IDs for type variables
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Generic(name) => write!(f, "Generic<{}>", name),
            Type::Function {
                params,
                return_type,
            } => {
                write!(f, "Fn(")?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Struct { name, .. } => write!(f, "Struct<{}>", name),
            Type::Unknown => write!(f, "Unknown"),
            Type::GenericVar(id) => write!(f, "T{}", id),
        }
    }
}

// Add type substitution tracking
#[derive(Debug, Default)]
struct TypeSubstitution {
    substitutions: HashMap<usize, Type>,
}

impl TypeSubstitution {
    fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    fn insert(&mut self, var: usize, typ: Type) {
        self.substitutions.insert(var, typ);
    }

    fn get(&self, var: usize) -> Option<&Type> {
        self.substitutions.get(&var)
    }

    fn apply(&self, typ: &Type) -> Type {
        match typ {
            Type::GenericVar(id) => {
                if let Some(substituted) = self.get(*id) {
                    substituted.clone()
                } else {
                    typ.clone()
                }
            }
            Type::Function { params, return_type } => Type::Function {
                params: params.iter().map(|t| self.apply(t)).collect(),
                return_type: Box::new(self.apply(return_type)),
            },
            Type::Struct { name, fields } => Type::Struct {
                name: name.clone(),
                fields: fields.iter().map(|(k, v)| (k.clone(), self.apply(v))).collect(),
            },
            _ => typ.clone(),
        }
    }
}

// 在 TypeChecker 定义之前添加错误类型
#[derive(Debug)]
pub enum TypeError {
    TypeMismatch {
        expected: Type,
        found: Type,
        context: String,
        location: String, // 添加位置信息
    },
    UnboundVariable {
        name: String,
        location: String,
    },
    UnboundFunction {
        name: String,
        location: String,
    },
    UnboundStruct {
        name: String,
    },
    WrongArgumentCount {
        function: String,
        expected: usize,
        found: usize,
    },
    UnknownField {
        struct_name: String,
        field_name: String,
    },
    InvalidFieldType {
        struct_name: String,
        field_name: String,
        expected: Type,
        found: Type,
    },
    NonStructFieldAccess {
        found: Type,
    },
    Custom(String),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TypeMismatch { expected, found, context, location } => {
                write!(f, "Type mismatch at {}: expected {}, found {} (in {})", 
                    location, expected, found, context)
            }
            TypeError::UnboundVariable { name, location } => {
                write!(f, "Unbound variable '{}' at {}", name, location)
            }
            TypeError::UnboundFunction { name, location } => {
                write!(f, "Unbound function: {}", name)
            }
            TypeError::UnboundStruct { name } => {
                write!(f, "Unbound struct: {}", name)
            }
            TypeError::WrongArgumentCount { function, expected, found } => {
                write!(f, "Function '{}' expects {} arguments, but got {}", function, expected, found)
            }
            TypeError::UnknownField { struct_name, field_name } => {
                write!(f, "Unknown field '{}' for struct '{}'", field_name, struct_name)
            }
            TypeError::InvalidFieldType { struct_name, field_name, expected, found } => {
                write!(f, "Invalid type for field '{}' in struct '{}': expected {}, found {}", 
                    field_name, struct_name, expected, found)
            }
            TypeError::NonStructFieldAccess { found } => {
                write!(f, "Attempted to access field on non-struct type: {}", found)
            }
            TypeError::Custom(msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}

pub type TypeResult<T> = Result<T, TypeError>;

// Simplified AST for Expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Variable(String),
    LiteralInt(i32),
    LiteralBool(bool),
    If {
        condition: Box<Expr>,
        then_block: Block,
        else_block: Option<Block>,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    StructLiteral {
        name: String,
        fields: HashMap<String, Expr>,
    },
    Block(Block), // Changed: Now using parser::Block
    Return(Box<Expr>),
    Select {
        base: Box<Expr>,
        field: String,
    },
}

// 在这里添加对 Block 的 re-export
pub use crate::parser::Block;
use crate::parser::{IfStatement, Item, Statement};

#[derive(Debug)]
enum Scope {
    Module(HashMap<String, Type>),
    Block(HashMap<String, Type>),
}

impl Scope {
    fn new_module() -> Self {
        Scope::Module(HashMap::new())
    }

    fn new_block() -> Self {
        Scope::Block(HashMap::new())
    }

    fn get(&self) -> &HashMap<String, Type> {
        match self {
            Scope::Module(map) | Scope::Block(map) => map
        }
    }

    fn get_mut(&mut self) -> &mut HashMap<String, Type> {
        match self {
            Scope::Module(map) | Scope::Block(map) => map
        }
    }
}

pub struct TypeChecker {
    scope_stack: Vec<Scope>,
    pub struct_definitions: HashMap<String, HashMap<String, Type>>,
    current_function: Option<String>,  // 当前正在分析的函数名
    current_block: Vec<String>,        // 块级别的位置栈
    type_substitutions: TypeSubstitution,
    next_type_var: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut module_scope = Scope::new_module();
        if let Scope::Module(ref mut env) = module_scope {
            // Add builtin operators
            env.insert("+".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Int),
            });
            env.insert("-".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Int),
            });
            env.insert("*".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Int),
            });
            env.insert("/".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Int),
            });
            env.insert("==".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Bool),
            });
            env.insert("!=".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Bool),
            });
            env.insert(">".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Bool),
            });
            env.insert("<".to_string(), Type::Function {
                params: vec![Type::Int, Type::Int],
                return_type: Box::new(Type::Bool),
            });
            env.insert("&&".to_string(), Type::Function {
                params: vec![Type::Bool, Type::Bool],
                return_type: Box::new(Type::Bool),
            });
            env.insert("||".to_string(), Type::Function {
                params: vec![Type::Bool, Type::Bool],
                return_type: Box::new(Type::Bool),
            });
        }

        TypeChecker {
            scope_stack: vec![module_scope],
            struct_definitions: HashMap::new(),
            current_function: None,
            current_block: Vec::new(),
            type_substitutions: TypeSubstitution::new(),
            next_type_var: 0,
        }
    }

    // 添加位置跟踪辅助方法
    fn get_current_location(&self) -> String {
        if let Some(ref func) = self.current_function {
            if self.current_block.is_empty() {
                format!("function '{}'", func)
            } else {
                format!("function '{}' in {}", func, self.current_block.join(" > "))
            }
        } else {
            "global scope".to_string()
        }
    }

    fn enter_function(&mut self, name: &str) {
        self.current_function = Some(name.to_string());
        self.current_block.clear();
    }

    fn exit_function(&mut self) {
        self.current_function = None;
        self.current_block.clear();
    }

    fn enter_block(&mut self, block_desc: &str) {
        self.current_block.push(block_desc.to_string());
    }

    fn exit_block(&mut self) {
        self.current_block.pop();
    }

    // Scope management
    fn enter_scope(&mut self, scope_type: Scope) {
        self.scope_stack.push(scope_type);
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn get_variable(&self, name: &str) -> Option<Type> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(typ) = scope.get().get(name) {
                return Some(typ.clone());
            }
        }
        None
    }

    fn declare_variable(&mut self, name: String, typ: Type) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.get_mut().insert(name, typ);
        }
    }

    // New AST analysis methods
    pub fn analyze_program(&mut self, items: &[Item]) -> TypeResult<()> {
        // First pass: collect all module-level declarations
        for item in items {
            self.declare_item(item)?;
        }

        // Second pass: check implementations
        for item in items {
            self.check_item(item)?;
        }
        Ok(())
    }

    fn declare_item(&mut self, item: &Item) -> TypeResult<()> {
        match item {
            Item::Function(fn_def) => {
                let mut param_types = Vec::new();
                for param in &fn_def.params {
                    param_types.push(param.param_type.clone());
                }
                self.declare_variable(
                    fn_def.name.clone(), 
                    Type::Function {
                        params: param_types,
                        return_type: Box::new(fn_def.return_type.clone()),
                    }
                );
            }
            Item::StructDef(struct_def) => {
                self.add_struct_definition(
                    struct_def.name.clone(),
                    struct_def.fields.clone()
                );
            }
        }
        Ok(())
    }

    fn check_item(&mut self, item: &Item) -> TypeResult<()> {
        match item {
            Item::Function(fn_def) => {
                self.enter_function(&fn_def.name);
                self.enter_scope(Scope::Block(HashMap::new()));
                
                // Declare parameters in function scope
                for param in &fn_def.params {
                    self.declare_variable(param.name.clone(), param.param_type.clone());
                }

                // Check function body
                let result = self.check_block(&fn_def.body, &fn_def.return_type);
                
                self.exit_scope();
                self.exit_function();
                result?
            }
            Item::StructDef(_) => { /* Already processed in declare_item */ }
        }
        Ok(())
    }

    // Modify check_block to handle scopes
    fn check_block(&mut self, block: &Block, expected_type: &Type) -> TypeResult<()> {
        self.enter_block("block");
        self.enter_scope(Scope::Block(HashMap::new()));
        
        let result = if let Some(last_stmt) = block.statements.last() {
            for stmt in block.statements.iter().take(block.statements.len() - 1) {
                self.check_statement(stmt, &Type::Unknown)?;
            }
            self.check_statement(last_stmt, expected_type)
        } else {
            if *expected_type == Type::Unknown {
                Ok(())
            } else {
                Err(TypeError::TypeMismatch {
                    expected: expected_type.clone(),
                    found: Type::Unknown,
                    context: "empty block".to_string(),
                    location: self.get_current_location(),
                })
            }
        };

        self.exit_scope();
        self.exit_block();
        result
    }

    // Add method to create fresh type variables
    fn fresh_type_var(&mut self) -> Type {
        let var = self.next_type_var;
        self.next_type_var += 1;
        Type::GenericVar(var)
    }

    // Add unification method
    fn unify(&mut self, t1: &Type, t2: &Type) -> TypeResult<()> {
        let t1 = self.type_substitutions.apply(t1);
        let t2 = self.type_substitutions.apply(t2);

        match (&t1, &t2) {
            (Type::GenericVar(id1), Type::GenericVar(id2)) if id1 == id2 => Ok(()),
            (Type::GenericVar(id), t) | (t, Type::GenericVar(id)) => {
                // Occur check would go here in a full implementation
                self.type_substitutions.insert(*id, t.clone());
                Ok(())
            }
            (Type::Function { params: p1, return_type: r1 }, 
             Type::Function { params: p2, return_type: r2 }) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: t1.clone(),
                        found: t2.clone(),
                        context: "function types".to_string(),
                        location: self.get_current_location(),
                    });
                }
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    self.unify(param1, param2)?;
                }
                self.unify(r1, r2)
            }
            (Type::Struct { name: n1, fields: f1 },
             Type::Struct { name: n2, fields: f2 }) if n1 == n2 => {
                for (name, type1) in f1 {
                    if let Some(type2) = f2.get(name) {
                        self.unify(type1, type2)?;
                    }
                }
                Ok(())
            }
            (t1, t2) if t1 == t2 => Ok(()),
            _ => Err(TypeError::TypeMismatch {
                expected: t1,
                found: t2,
                context: "type unification".to_string(),
                location: self.get_current_location(),
            }),
        }
    }

    // Modify check and infer methods to use new scope management
    fn check(&mut self, expr: &Expr, expected_type: &Type) -> TypeResult<()> {
        let inferred = self.infer(expr)?;
        self.unify(&inferred, expected_type)
    }

    pub fn infer(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::LiteralInt(_) => Ok(Type::Int),
            Expr::LiteralBool(_) => Ok(Type::Bool),
            Expr::Variable(name) => {
                if let Some(typ) = self.get_variable(name) {
                    // Instantiate fresh type variables for generics
                    Ok(self.instantiate_generics(&typ))
                } else {
                    Err(TypeError::UnboundVariable {
                        name: name.clone(),
                        location: self.get_current_location(),
                    })
                }
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.enter_block("if condition");
                self.check(condition, &Type::Bool)?; // Check condition is Bool
                self.exit_block();

                self.enter_block("then branch");
                let then_type = self.infer_block(then_block)?; // Infer type of then branch
                self.exit_block();

                if let Some(else_block) = else_block {
                    self.enter_block("else branch");
                    let else_type = self.infer_block(else_block)?; // Infer type of else branch
                    self.exit_block();

                    if then_type == else_type {
                        Ok(then_type) // Unify branches to the same type
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: then_type,
                            found: else_type,
                            context: "if expression".to_string(),
                            location: self.get_current_location(),
                        })
                    }
                } else {
                    Ok(then_type)
                }
            }
            Expr::FnCall { name, args } => {
                match self.get_variable(name) {
                    Some(Type::Function {
                        params,
                        return_type,
                    }) => {
                        if args.len() != params.len() {
                            return Err(TypeError::WrongArgumentCount {
                                function: name.clone(),
                                expected: params.len(),
                                found: args.len(),
                            });
                        }
                        for (arg, param_type) in args.iter().zip(params.iter()) {
                            unsafe {
                                Self::check(
                                    &mut *(self as *const Self as *mut Self),
                                    arg,
                                    param_type,
                                )?
                            }; // Check arguments against parameter types
                        }
                        Ok(*return_type.clone()) // Infer return type from function signature
                    }
                    Some(actual_type) => Err(TypeError::TypeMismatch {
                        expected: Type::Function {
                            params: vec![],
                            return_type: Box::new(Type::Unknown),
                        },
                        found: actual_type.clone(),
                        context: format!("function '{}'", name),
                        location: self.get_current_location(),
                    }),
                    None => Err(TypeError::UnboundFunction {
                        name: name.clone(),
                        location: self.get_current_location(),
                    }),
                }
            }
            Expr::StructLiteral { name, fields } => {
                match self.struct_definitions.get(name) {
                    Some(struct_fields_def) => {
                        if fields.len() != struct_fields_def.len() {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::Struct {
                                    name: name.clone(),
                                    fields: struct_fields_def.clone(),
                                },
                                found: Type::Struct {
                                    name: name.clone(),
                                    fields: fields.iter().map(|(k, v)| (k.clone(), Type::Unknown)).collect(),
                                },
                                context: format!("struct '{}'", name),
                                location: self.get_current_location(),
                            });
                        }
                        for (field_name, field_expr) in fields.iter() {
                            match struct_fields_def.get(field_name) {
                                Some(expected_field_type) => {
                                    unsafe {
                                        Self::check(
                                            &mut *(self as *const Self as *mut Self),
                                            field_expr,
                                            expected_field_type,
                                        )?
                                    };
                                    // Check field expr against defined type
                                }
                                None => {
                                    return Err(TypeError::UnknownField {
                                        struct_name: name.clone(),
                                        field_name: field_name.clone(),
                                    })
                                }
                            }
                        }
                        Ok(Type::Struct {
                            name: name.clone(),
                            fields: struct_fields_def.clone(),
                        }) // Infer struct type
                    }
                    None => Err(TypeError::UnboundStruct {
                        name: name.clone(),
                    }),
                }
            }
            Expr::Block(block) => {
                if let Some(last_stmt) = block.statements.last() {
                    for stmt in block.statements.iter().take(block.statements.len() - 1) {
                        self.infer_statement(stmt)?; // 需要新增此方法
                    }
                    self.infer_statement(last_stmt) // 需要新增此方法
                } else {
                    Ok(Type::Unknown)
                }
            }
            Expr::Return(expr) => {
                self.infer(expr) // Infer type of the return expression
            }
            Expr::Select { base, field } => {
                match self.infer(base)? {
                    Type::Struct { fields, .. } => {
                        match fields.get(field) {
                            Some(field_type) => Ok(field_type.clone()), // Infer field type
                            None => Err(TypeError::UnknownField {
                                struct_name: format!("{:?}", base),
                                field_name: field.clone(),
                            }),
                        }
                    }
                    _ => Err(TypeError::NonStructFieldAccess {
                        found: self.infer(base)?,
                    }),
                }
            }
        }
    }

    // 新增方法：处理语句的类型检查
    fn check_statement(&mut self, stmt: &Statement, expected_type: &Type) -> TypeResult<()> {
        match stmt {
            Statement::Let(let_stmt) => {
                let inferred_type = self.infer(&let_stmt.initializer)?;
                if let Some(ref declared_type) = let_stmt.declared_type {
                    if *declared_type != inferred_type {
                        return Err(TypeError::TypeMismatch {
                            expected: declared_type.clone(),
                            found: inferred_type,
                            context: format!("let statement for variable '{}'", let_stmt.name),
                            location: self.get_current_location(),
                        });
                    }
                }
                self.declare_variable(let_stmt.name.clone(), inferred_type);
                Ok(())
            }
            Statement::Expression(expr_stmt) => self.check(&expr_stmt.expression, expected_type),
            Statement::Return(ret_stmt) => {
                if let Some(ref expr) = ret_stmt.return_value {
                    self.check(expr, expected_type)
                } else {
                    if *expected_type == Type::Unknown {
                        Ok(())
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: expected_type.clone(),
                            found: Type::Unknown,
                            context: "return statement".to_string(),
                            location: self.get_current_location(),
                        })
                    }
                }
            }
            Statement::If(if_stmt) => {
                self.check_if_statement(if_stmt, expected_type)
            }
        }
    }

    // 新增方法：处理语句的类型推导
    fn infer_statement(&mut self, stmt: &Statement) -> TypeResult<Type> {
        match stmt {
            Statement::Let(let_stmt) => {
                let inferred_type = self.infer(&let_stmt.initializer)?;
                if let Some(ref declared_type) = let_stmt.declared_type {
                    if *declared_type != inferred_type {
                        return Err(TypeError::TypeMismatch {
                            expected: declared_type.clone(),
                            found: inferred_type,
                            context: format!("let statement for variable '{}'", let_stmt.name),
                            location: self.get_current_location(),
                        });
                    }
                }
                self.declare_variable(let_stmt.name.clone(), inferred_type.clone());
                Ok(inferred_type)
            }
            Statement::Expression(expr_stmt) => self.infer(&expr_stmt.expression),
            Statement::Return(ret_stmt) => {
                if let Some(ref expr) = ret_stmt.return_value {
                    self.infer(expr)
                } else {
                    Ok(Type::Unknown)
                }
            }
            Statement::If(if_stmt) => self.infer_if_statement(if_stmt),
        }
    }

    // 新增方法：处理 if 语句的类型检查
    fn check_if_statement(&mut self, if_stmt: &IfStatement, expected_type: &Type) -> TypeResult<()> {
        self.check(&if_stmt.condition, &Type::Bool)?;
        self.check_block(&if_stmt.then_block, expected_type)?;
        if let Some(ref else_block) = if_stmt.else_block {
            self.check_block(else_block, expected_type)?;
        }
        Ok(())
    }

    // 新增方法：处理 if 语句的类型推导
    fn infer_if_statement(&mut self, if_stmt: &IfStatement) -> TypeResult<Type> {
        self.check(&if_stmt.condition, &Type::Bool)?;
        let then_type = self.infer_block(&if_stmt.then_block)?;
        if let Some(ref else_block) = if_stmt.else_block {
            let else_type = self.infer_block(else_block)?;
            if then_type == else_type {
                Ok(then_type)
            } else {
                Err(TypeError::TypeMismatch {
                    expected: then_type,
                    found: else_type,
                    context: "if statement".to_string(),
                    location: self.get_current_location(),
                })
            }
        } else {
            Ok(then_type)
        }
    }

    // // 新增方法：处理块的类型检查
    // fn check_block(&mut self, block: &Block, expected_type: &Type) -> TypeResult<()> {
    //     self.enter_scope(Scope::Block(HashMap::new()));
        
    //     let result = if let Some(last_stmt) = block.statements.last() {
    //         for stmt in block.statements.iter().take(block.statements.len() - 1) {
    //             self.check_statement(stmt, &Type::Unknown)?;
    //         }
    //         self.check_statement(last_stmt, expected_type)
    //     } else {
    //         if *expected_type == Type::Unknown {
    //             Ok(())
    //         } else {
    //             Err(TypeError::TypeMismatch {
    //                 expected: expected_type.clone(),
    //                 found: Type::Unknown,
    //                 context: "empty block".to_string(),
    //             })
    //         }
    //     };

    //     self.exit_scope();
    //     result
    // }

    // 新增方法：处理块的类型推导
    fn infer_block(&mut self, block: &Block) -> TypeResult<Type> {
        if let Some(last_stmt) = block.statements.last() {
            for stmt in block.statements.iter().take(block.statements.len() - 1) {
                self.infer_statement(stmt)?;
            }
            self.infer_statement(last_stmt)
        } else {
            Ok(Type::Unknown)
        }
    }

    fn add_struct_definition(&mut self, name: String, fields: HashMap<String, Type>) {
        self.struct_definitions.insert(name, fields);
    }

    // Add method to instantiate generic types
    fn instantiate_generics(&mut self, typ: &Type) -> Type {
        match typ {
            Type::Generic(_) => self.fresh_type_var(),
            Type::Function { params, return_type } => Type::Function {
                params: params.iter().map(|t| self.instantiate_generics(t)).collect(),
                return_type: Box::new(self.instantiate_generics(return_type)),
            },
            Type::Struct { name, fields } => Type::Struct {
                name: name.clone(),
                fields: fields.iter().map(|(k, v)| (k.clone(), self.instantiate_generics(v))).collect(),
            },
            _ => typ.clone(),
        }
    }
}

impl Into<Expr> for Block {
    fn into(self) -> Expr {
        Expr::Block(self)
    }
}