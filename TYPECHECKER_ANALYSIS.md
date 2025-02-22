# Type Checker Algorithm Analysis

1. **Two-Pass Process**
   - **Declaration Pass:**  
     `declare_item` registers all module–level declarations (functions and structs) to the current scope.
   - **Checking Pass:**  
     `check_item` then visits each item to analyze and type-check function bodies and struct usage.

2. **Scope Management**
   - The type checker maintains a stack of scopes (module and block), ensuring that variable declarations remain valid only in their declared contexts.
   - Methods like `enter_scope`/`exit_scope` and `enter_block`/`exit_block` manage nested scopes.

3. **Type Inference and Checking**
   - **Expression Inference:**  
     The `infer` function recursively determines types for all kinds of expressions (literals, variables, if expressions, function calls, etc.) and uses `infer_block` for block expressions.
   - **Statement Handling:**  
     Specialized methods (`infer_statement` and `check_statement`) first infer the types of statements (like let statements, returns, and if statements) and perform checks against expected types.

4. **Handling Generics and Fresh Type Variables**
   - The checker instantiates generic types via the `instantiate_generics` method, which creates fresh type variables (using `fresh_type_var`) as needed.

5. **Type Unification**
   - The `unify` method drives type compatibility by:
     - Replacing generic type variables with concrete types using a substitution map.
     - Recursively verifying function types (matching parameters and return types).
     - Checking struct types by ensuring both name and fields are compatible.
   - When type conflicts are detected, detailed error messages (via `TypeError`) are generated.

6. **Error Reporting**
   - Errors include mismatches, unbound variables/functions, wrong argument count, unknown fields, and invalid field types.
   - Location information (current function and block) is appended to error messages via `get_current_location`.

Overall, the type checker is designed to first record all declarations, then apply a recursive descent type inference and checking process with scope awareness and unification for generic types.
