use std::{collections::HashMap, fs::read_to_string};

mod bidir;
use bidir::*;
mod lexer;
use lexer::*;
mod parser;
use parser::*;

fn main() -> ParseResult<()> {
    let input_code = read_to_string("test.rs").expect("Failed to read file");
    let mut parser = Parser::new(input_code)?;

    match parser.try_program() {
        Ok(program) => {
            println!("Parsing successful!");
            let mut type_checker = TypeChecker::new();
            let reuslt =  type_checker.analyze_program(&program);
            match reuslt {
                Ok(_) => {
                    println!("Type checking successful!");
                }
                Err(err) => {
                    eprintln!("Type checking error: {}", err);
                    std::process::exit(1);
                }
            }
        }
        Err(err) => {
            eprintln!("Parsing error:\n{}", parser.format_error(&err));
            std::process::exit(1);
        }
    }

    Ok(())
}

// fn main() {
//     let mut type_checker = TypeChecker::new();

//     // Define a struct
//     let point_fields = HashMap::from([
//         ("x".to_string(), Type::Int),
//         ("y".to_string(), Type::Int),
//     ]);
//     type_checker.add_struct_definition("Point".to_string(), point_fields);

//     // Define functions in the environment
//     type_checker.environment.insert(
//         "add".to_string(),
//         Type::Function {
//             params: vec![Type::Int, Type::Int],
//             return_type: Box::new(Type::Int),
//         },
//     );
//     type_checker.environment.insert(
//         "is_positive".to_string(),
//         Type::Function {
//             params: vec![Type::Int],
//             return_type: Box::new(Type::Bool),
//         },
//     );
//     type_checker.environment.insert(
//         "create_point".to_string(),
//         Type::Function {
//             params: vec![Type::Int, Type::Int],
//             return_type: Box::new(Type::Struct{name: "Point".to_string(), fields: type_checker.struct_definitions.get("Point").unwrap().clone()}), // Return Point struct
//         },
//     );

//     // Example expressions
//     let expr1 = Expr::LiteralInt(10);
//     let expr2 = Expr::Variable("x".to_string());
//     let expr3 = Expr::FnCall {
//         name: "add".to_string(),
//         args: vec![Expr::LiteralInt(5), Expr::Variable("y".to_string())],
//     };
//     let expr4 = Expr::If {
//         condition: Box::new(Expr::FnCall {
//             name: "is_positive".to_string(),
//             args: vec![Expr::Variable("z".to_string())],
//         }),
//         then_branch: Box::new(Expr::LiteralInt(1)),
//         else_branch: Box::new(Expr::LiteralInt(-1)),
//     };
//     let expr5 = Expr::StructLiteral {
//         name: "Point".to_string(),
//         fields: HashMap::from([
//             ("x".to_string(), Expr::LiteralInt(100)),
//             ("y".to_string(), Expr::LiteralInt(200)),
//         ]),
//     };
//     let expr6 = Expr::Block(vec![
//         Expr::LiteralInt(1),
//         Expr::LiteralBool(true),
//         Expr::Return(Box::new(Expr::FnCall{
//             name: "create_point".to_string(),
//             args: vec![Expr::LiteralInt(5), Expr::LiteralInt(10)]
//         }))
//     ]);

//     // Set up environment for variables used in expressions
//     type_checker.environment.insert("x".to_string(), Type::Int);
//     type_checker.environment.insert("y".to_string(), Type::Int);
//     type_checker.environment.insert("z".to_string(), Type::Int);

//     // Type check expressions and print results
//     println!("Expression 1: {:?} - Expected Type: Int", expr1);
//     match type_checker.check(&expr1, &Type::Int) {
//         Ok(_) => println!("Type check successful"),
//         Err(err) => println!("Type check error: {}", err),
//     }

//     println!("\nExpression 2: {:?} - Expected Type: Int", expr2);
//     match type_checker.check(&expr2, &Type::Int) {
//         Ok(_) => println!("Type check successful"),
//         Err(err) => println!("Type check error: {}", err),
//     }

//     println!("\nExpression 3: {:?} - Inferred Type:", expr3);
//     match type_checker.infer(&expr3) {
//         Ok(inferred_type) => println!("Inferred type: {}", inferred_type),
//         Err(err) => println!("Type inference error: {}", err),
//     }

//     println!("\nExpression 4: {:?} - Inferred Type:", expr4);
//     match type_checker.infer(&expr4) {
//         Ok(inferred_type) => println!("Inferred type: {}", inferred_type),
//         Err(err) => println!("Type inference error: {}", err),
//     }

//     println!("\nExpression 5: {:?} - Expected Type: Struct<Point>", expr5);
//     match type_checker.check(&expr5, &Type::Struct{name: "Point".to_string(), fields: type_checker.struct_definitions.get("Point").unwrap().clone()}) {
//         Ok(_) => println!("Type check successful"),
//         Err(err) => println!("Type check error: {}", err),
//     }

//      println!("\nExpression 6: {:?} - Inferred Type:", expr6);
//     match type_checker.infer(&expr6) {
//         Ok(inferred_type) => println!("Inferred type: {}", inferred_type),
//         Err(err) => println!("Type inference error: {}", err),
//     }

//     // Example of a type error
//     let error_expr = Expr::FnCall {
//         name: "add".to_string(),
//         args: vec![Expr::LiteralInt(5), Expr::LiteralBool(true)], // Type error: Bool instead of Int
//     };
//     println!("\nError Expression: {:?} - Inferred Type:", error_expr);
//     match type_checker.infer(&error_expr) {
//         Ok(inferred_type) => println!("Inferred type: {}", inferred_type),
//         Err(err) => println!("Type inference error: {}", err), // Expecting an error here
//     }
// }

// fn main() {
//     let input_code = read_to_string("test.rs").expect("Failed to read file");

//     let mut lexer = Lexer::new(input_code.to_string());
//     loop {
//         let token = lexer.next_token();
//         println!("{:?}", token);

//         match token.token_type {
//             TokenType::EOF => {
//                 println!("End of file");
//                 break;
//             }
//             TokenType::Error(e) => {
//                 println!("Lexer encountered an error: {}", e);
//                 break;
//             }
//             _ => {}
//         }
//     }
// }
