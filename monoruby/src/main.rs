extern crate monoruby;
use monoruby::codegen::Codegen;

fn main() {
    let program = "
    return fibo(40)
    def fibo(x)
        if x == 0 then return 0 end
        if x == 1 then return 1 end
        return fibo(x-1) + fibo(x-2)
    end
        ";
    eprintln!("{}", program);
    let x = 40;
    let ret = Codegen::exec_script(program)(x);
    println!("return value = {}", ret);
    //assert_eq!(102334155, ret);
}
