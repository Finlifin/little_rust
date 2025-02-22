fn create_point(x: i32, y: i32) -> Point<i32> {
    return struct Point { x: x, y: y };
}

fn add(x: i32, y: i32) -> i32 {
    let result = x + y;
    return result;
}

struct Point<T> {
    x: T,
    y: T
}

fn main() -> Void {
    let a: i32 = 10;
    let b: i32 = 20;
    let sum = add(a, b);
    if is_positive(sum) {
        let p: Point<i32> = create_point(sum, 0);
        println(p);
    } 
}

fn is_positive(n: i32) -> bool {
    return n > 0;
}

struct EmptyStruct {}