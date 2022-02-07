use std::arch::asm;

// Utility functions
pub const PUTC: *const fn() = putc as *const fn();
pub const PUTINT: *const fn() = putint as *const fn();
pub const PANIC: *const fn() = panic as *const fn();
pub const DUMP: *const fn() = dump as *const fn();

extern "C" fn putc(ch: u8) {
    eprint!("{}", ch as char);
}

extern "C" fn putint(i: u64) {
    eprintln!("{:?}", i);
}

extern "C" fn panic() {
    panic!("panic() is called.")
}

extern "C" fn dump() {
    #[allow(unused_assignments)]
    let (mut rax, mut rdi, mut rsi, mut rdx, mut rcx, mut r8) =
        (0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    macro_rules! reg_save {
        ($reg:ident) => {
            unsafe {
                asm!(
                    concat!("mov {}, ", stringify!($reg)),
                    out(reg) $reg,
                );
            }
        };
    }
    macro_rules! reg_restore {
        ($reg:ident) => {
            unsafe {
                asm!(
                    concat!("mov ", stringify!($reg),", {}"),
                    in(reg) $reg,
                );
            }
        };
    }
    macro_rules! reg_print {
        ($reg:ident) => {
            eprint!(" {:3}: {:016x}", stringify!($reg), $reg);
        };
    }
    reg_save!(rax);
    reg_save!(rdi);
    reg_save!(rsi);
    reg_save!(rdx);
    reg_save!(rcx);
    reg_save!(r8);

    reg_print!(rax);
    reg_print!(rdi);
    reg_print!(rsi);
    reg_print!(rdx);
    reg_print!(rcx);
    eprintln!();
    reg_print!(r8);
    eprintln!();

    reg_restore!(rax);
    reg_restore!(rdi);
    reg_restore!(rsi);
    reg_restore!(rdx);
    reg_restore!(rcx);
    reg_restore!(r8);
}
