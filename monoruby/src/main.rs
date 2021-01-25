extern crate monoruby;
use monoruby::codegen::Codegen;

fn main() {
    let program = "
    return fact(10)
    def fact(x)
        if x == 1 then return 1 end
        return x * fact(x-1)
    end
        ";
    //eprintln!("{}", program);
    let x = 40;
    let f = Codegen::exec_script(program);
    let ret = f(x);
    println!("return value = {}", ret);
    assert_eq!(3628800, ret);
}
