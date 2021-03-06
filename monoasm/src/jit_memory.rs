//-----------------------------------------------------
//
// JIT module runtime
//
//-----------------------------------------------------

use crate::*;
//use monoasm_inst::Reg;
use region::{protect, Protection};
use std::alloc::{alloc, Layout};

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Pointer to the heap.
    contents: *mut u8,
    /// Current position
    counter: Pos,
    /// Current label id.
    label_count: usize,
    /// Relocation information.
    reloc: Relocations,
}

impl Index<Pos> for JitMemory {
    type Output = u8;

    fn index(&self, index: Pos) -> &u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &*self.contents.offset(index.0 as isize) }
    }
}

impl IndexMut<Pos> for JitMemory {
    fn index_mut(&mut self, index: Pos) -> &mut u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &mut *self.contents.offset(index.0 as isize) }
    }
}

impl std::default::Default for JitMemory {
    fn default() -> Self {
        Self::new()
    }
}

impl JitMemory {
    /// Create new JitMemory.
    ///
    /// This function try to allocate heap memory of 4KB for JIT assemble.
    ///
    /// ### panic
    /// Panic if Layout::from_size_align() or region::protect() returned Err.
    pub fn new() -> JitMemory {
        let size = 4096;
        let layout = Layout::from_size_align(size, PAGE_SIZE).expect("Bad Layout.");
        let contents = unsafe { alloc(layout) };

        unsafe {
            protect(contents, size, Protection::READ_WRITE_EXECUTE).expect("Mprotect failed.");
        }
        let mut res = JitMemory {
            contents,
            counter: Pos(0),
            label_count: 0,
            reloc: Relocations::new(),
        };
        res.emitb(0xc3);
        res.counter = Pos(0);
        res
    }

    pub fn get_current(&self) -> usize {
        self.counter.0
    }

    fn _p(&self) {
        for i in 0..self.counter.0 {
            print!("{:>02x} ", self[Pos(i)]);
        }
        println!();
    }

    /// Create new label and returns `DestLabel`.
    pub fn label(&mut self) -> DestLabel {
        let label = self.label_count;
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        DestLabel(label)
    }

    /// Bind the current location to `label`.
    pub fn bind_label(&mut self, label: DestLabel) {
        self.reloc[label].loc = Some(self.counter);
    }

    /// Bind the current location to `label`.
    pub fn bind_label_to_pos(&mut self, label: DestLabel, pos: usize) {
        self.reloc[label].loc = Some(Pos::from(pos));
    }

    /// Bind the current location to `label`.
    pub fn get_label_pos(&mut self, label: DestLabel) -> usize {
        self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .0
    }

    /// Save relocaton slot for `DestLabel`.
    pub fn save_reloc(&mut self, dest: DestLabel, size: u8) {
        self.reloc[dest].disp.push((size, self.counter));
    }

    /// Resolve all relocations.
    pub fn resolve_relocs(&mut self) {
        let mut relocs: Vec<(Pos, i32)> = vec![];
        for rel in self.reloc.iter_mut() {
            if let Some(pos) = rel.loc {
                for (size, dest) in &mut rel.disp {
                    let disp = pos.0 as i64 - dest.0 as i64 - *size as i64;
                    if i32::min_value() as i64 > disp || disp > i32::max_value() as i64 {
                        panic!("Relocation overflow");
                    }
                    relocs.push((*dest, disp as i32));
                }
            }
        }
        for (dest, disp) in relocs {
            self.write32(dest, disp);
        }
    }

    /// Resolve all relocations and return the top addresss of generated machine code as a function pointer.
    pub fn finalize<T, U>(&mut self) -> fn(T) -> U {
        self.resolve_relocs();
        unsafe { mem::transmute(self.contents) }
    }

    pub fn get_label_addr<T, U>(&mut self, label: DestLabel) -> extern "C" fn(T) -> U {
        let counter = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .0;
        let adr = self.contents;
        unsafe { mem::transmute(adr.add(counter)) }
    }

    /// Emit a byte.
    pub fn emitb(&mut self, val: u8) {
        let c = self.counter;
        self[c] = val;
        self.counter = c + 1;
    }

    /// Emit a word.
    pub fn emitw(&mut self, val: u16) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self.counter = c + 2;
    }

    /// Emit a long word.
    pub fn emitl(&mut self, val: u32) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self[c + 2] = (val >> 16) as u8;
        self[c + 3] = (val >> 24) as u8;
        self.counter = c + 4;
    }

    /// Write 32bit data `val` on `loc`.
    fn write32(&mut self, loc: Pos, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    /// Emit a quad word.
    pub fn emitq(&mut self, val: u64) {
        self.emitl(val as u32);
        self.emitl((val >> 32) as u32);
    }
}

#[allow(dead_code)]
impl JitMemory {
    /// Encoding: Opcode +rd  
    /// Op+ rd
    pub fn enc_o(&mut self, op: u8, reg: Reg) {
        self.rex(Reg(0), reg, Reg(0));
        self.op_with_rd(op, reg);
    }

    /// Encoding: Opcode +rd  
    /// REX.W Op+ rd
    pub fn enc_rexw_o(&mut self, op: u8, reg: Reg) {
        self.rexw(Reg(0), reg, Reg(0));
        self.op_with_rd(op, reg);
    }

    /// Encoding: MI  
    /// REX.W Op ModRM:r/m
    pub fn enc_rexw_mi(&mut self, op: u8, rm_op: Or) {
        self.enc_rexw_mr(op, Reg(0), rm_op)
    }

    /// Encoding: MR or RM
    /// REX.W Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rexw_mr(&mut self, op: u8, reg: Reg, rm_op: Or) {
        self.enc_mr_main(&[op], true, reg, rm_op);
    }

    pub fn enc_mr_main(
        &mut self,
        op: &[u8],
        //rex: fn(&mut Self, Reg, Reg, Reg),
        is_rexw: bool,
        reg: Reg,
        rm: Or,
    ) {
        if rm.mode != Mode::Reg && (rm.base.0 & 0b111) == 4 {
            // If mode != Reg and r/m == 4/12 (rsp/r12), use SIB.
            // Currently, only Mode::Ind is supported.
            assert!(rm.mode == Mode::Ind);
            // set index to 4 when [rm] is to be used.
            let index = Reg(4); // magic number
            let scale = 0;
            if is_rexw {
                self.rexw(reg, rm.base, index);
            } else {
                self.rex(reg, rm.base, index);
            }
            op.iter().for_each(|o| self.emitb(*o));
            self.modrm(reg, rm.mode, rm.base);
            self.sib(scale, index, rm.base);
            self.emit_disp(rm);
        } else if rm.mode == Mode::Ind && (rm.base.0 & 0b111) == 5 {
            // If mode == Ind and r/m == 5/13 (rbp/r13), use [rbp/r13 + 0(disp8)].
            if is_rexw {
                self.rexw(reg, rm.base, Reg(0));
            } else {
                self.rex(reg, rm.base, Reg(0));
            }
            op.iter().for_each(|o| self.emitb(*o));
            self.modrm(reg, Mode::InD8, rm.base);
            self.emitb(0);
        } else {
            if is_rexw {
                self.rexw(reg, rm.base, Reg(0));
            } else {
                self.rex(reg, rm.base, Reg(0));
            }
            op.iter().for_each(|o| self.emitb(*o));
            self.modrm(reg, rm.mode, rm.base);
            self.emit_disp(rm);
        }
    }

    /// Encoding: D  
    /// Op cd
    pub fn enc_d(&mut self, op: &[u8], dest: DestLabel) {
        op.iter().for_each(|o| self.emitb(*o));
        self.save_reloc(dest, 4);
        self.emitl(0);
    }

    /// Encoding: /n  
    /// Op /n
    pub fn enc_digit(&mut self, op: &[u8], reg: Reg, digit: u8) {
        self.rex(Reg(0), reg, Reg(0));
        op.iter().for_each(|o| self.emitb(*o));
        self.modrm_digit(digit, Mode::Reg, reg);
    }

    /// Encoding: /n  
    /// REX.W Op /n
    pub fn enc_rexw_digit(&mut self, op: &[u8], rm: Or, digit: u8) {
        self.rexw(Reg(0), rm.base, Reg(0));
        op.iter().for_each(|o| self.emitb(*o));
        self.modrm_digit(digit, rm.mode, rm.base);
        self.emit_disp(rm);
    }
}

impl JitMemory {
    /// ModRM
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field |  mod  |    reg    |    r/m    |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     r     |     b     |
    /// +-------+-------+-----------+-----------+
    fn modrm_digit(&mut self, digit: u8, mode: Mode, base: Reg) {
        let modrm = (mode as u8) << 6 | (digit & 0b111) << 3 | (base.0 & 0b111);
        self.emitb(modrm);
    }

    fn modrm(&mut self, reg: Reg, mode: Mode, base: Reg) {
        let modrm = (mode as u8) << 6 | (reg.0 & 0b111) << 3 | (base.0 & 0b111);
        self.emitb(modrm);
    }

    /// REX.W
    ///      bit
    /// +---+---+------------------------------------------------+
    /// | W | 3 | 1 = 64 bit operand size                        |
    /// +---+---+------------------------------------------------+
    /// | R | 2 | rex_r = ext of reg field of ModRM              |
    /// +---+---+------------------------------------------------+
    /// | X | 1 | rex_i = ext of index field of SIB              |
    /// +---+---+------------------------------------------------+
    /// | B | 0 | rex_b = ext of r/m(ModRM) or base(SIB)         |
    /// |   |   |           or reg field of Op.                  |
    /// +---+---+------------------------------------------------+
    fn rexw(&mut self, reg: Reg, base: Reg, index: Reg) {
        let rexw = 0x48 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
        self.emitb(rexw);
    }

    fn rex(&mut self, reg: Reg, base: Reg, index: Reg) {
        if reg.0 > 7 || base.0 > 7 || index.0 > 7 {
            let rex_prefix =
                0x40 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
            self.emitb(rex_prefix);
        };
    }

    fn op_with_rd(&mut self, op: u8, reg: Reg) {
        let op = op | (reg.0 & 0b0111);
        self.emitb(op);
    }

    /// SIB
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field | scale |   index   |    base   |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     x     |     b     |
    /// +-------+-------+-----------+-----------+
    ///
    /// scale: 00|01|10|11
    ///  mul : na| 2| 4| 8
    ///
    /// index: register number (with rex.x)
    ///
    /// base: register number (with rex.b)
    ///     rex.b:0 base:101 => use RBP  mode:00/disp32 01/RBP+disp8 10/RBP+disp32
    ///     rex.b:1 base:101 => use R13  mode:00/disp32 01/R13+disp8 10/R13+disp32
    ///
    fn sib(&mut self, scale: u8, index: Reg, base: Reg) {
        assert!(scale < 4);
        assert!(index.0 < 8);
        let sib = (scale << 6) | (index.0 << 3) | (base.0 & 0b111);
        self.emitb(sib);
    }

    fn emit_disp(&mut self, rm: Or) {
        match rm.disp {
            Disp::D8(d) => self.emitb(d as u8),
            Disp::D32(d) => self.emitl(d as u32),
            Disp::None => {}
        }
    }
}
