extern crate monoruby;
use monoruby::codegen::Codegen;

fn main() {
    let program = "
    def fibo(x)
        if x == 0 then return 0 end
        if x == 1 then return 1 end
        return fibo(x-1) + fibo(x-2)
    end
    return fibo(40)
        ";
    //eprintln!("{}", program);
    let x = 40;
    let f = Codegen::exec_script(program);
    let ret = f(x);
    println!("return value = {}", ret);
    assert_eq!(102334155, ret);
}
