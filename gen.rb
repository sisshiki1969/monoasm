#
# Automatic verification tool for monoasm
#

def reg_template(size)
  if size ==8
    ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
  elsif size == 4
    ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"]
  elsif size == 2
    ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"]
  elsif size == 1
    ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"]
  else
    raise "invalid size descriptor #{size}"
  end
end

INDEX_TEMPLATE = [
  "rax", 
  "r15", 
]

def indirect_template(size)
  (reg_template(8) + ["rip"]).map do |r|
    [ 
      "[#{r}]",
      "[#{r} + 16]",
      "[#{r} + 512]"
    ]
  end.flatten +
  reg_template(8).map do |r|
    INDEX_TEMPLATE.map do |i|
      ["1", "8"].map do |s|
        [
          "[#{r} + #{i} * #{s}]",
          "[#{r} + #{i} * #{s} + 20]"
        ]
      end
    end
  end.flatten
end

def asm_indirect_template(size)
  prefix = if size == 8
    "QWORD PTR"
  elsif size == 4
    "DWORD PTR"
  elsif size == 2
    "WORD PTR"
  elsif size == 1
    "BYTE PTR"
  else
    raise "invalid size descriptor #{size}"
  end
  (reg_template(8) + ["rip"]).map do |r|
    [
      "#{prefix} [#{r}]", 
      "#{prefix} [#{r} + 16]",
      "#{prefix} [#{r} + 512]"
    ]
  end.flatten +
  reg_template(8).map do |r|
    INDEX_TEMPLATE.map do |i|
      ["1", "8"].map do |s|
        [
          "#{prefix} [#{r} + #{i} * #{s}]",
          "#{prefix} [#{r} + #{i} * #{s} + 20]",
        ]
      end
    end
  end.flatten
end

IMM_TEMPLATE = ["1", "18"]

ASM_HEADER = <<EOS
.intel_syntax noprefix
.text
EOS

class Inst
  MODE_REG = 0
  MODE_INDIRECT = 1
  MODE_IMMEDIATE = 2

  def self.size
    @size
  end

  def self.inst
    @inst
  end

  def self.inst_name(size)
    @inst + 
    case size
    when 8
      "q"
    when 4
      "l"
    when 2
      "w"
    when 1
      "b"
    end
  end

  def self.make_file(inst, size)
    @monoasm = header
    @asm = ""

    gen(inst, size)

    f = File.open "monoasm/tests/#{inst}.rs", "w"
    f.write @monoasm + footer(inst)
    f.close


    f = File.open "monoasm/tests/#{inst}.s", "w"
    f.write ASM_HEADER + @asm
    f.close

    `as monoasm/tests/#{inst}.s -o monoasm/tests/#{inst}`
    `objcopy -O binary --only-section=.text monoasm/tests/#{inst} monoasm/tests/#{inst}.bin`
    `rm monoasm/tests/#{inst}`
  end

  def self.compare(inst)
    res = `diff monoasm/tests/#{inst}.bin monoasm/tests/#{inst}_monoasm.bin`
    if res.length != 0
      puts res
      `objdump -D -Mintel,x86-64 -b binary -m i386 monoasm/tests/#{inst}.bin > monoasm/tests/#{inst}.txt`
      `objdump -D -Mintel,x86-64 -b binary -m i386 monoasm/tests/#{inst}_monoasm.bin > monoasm/tests/#{inst}_monoasm.txt`
    end
  end

  private

  def self.header
<<EOS
  extern crate monoasm;
  extern crate monoasm_macro;
  use std::io::Write;

  use monoasm::*;
  use monoasm_macro::monoasm;

  #[test]
  fn #{@inst}() {
      let mut jit: JitMemory = JitMemory::new();
      monoasm!(
          jit,
EOS
  end

  def self.footer(inst)
    <<EOS
      );
      jit.finalize();
      let mut buf = std::fs::File::create("tests/#{inst}_monoasm.bin").unwrap();
      buf.write_all(jit.as_slice()).unwrap();
  }
EOS
  end

  def self.template(mode, size)
    case mode
    when MODE_REG
      [reg_template(8), reg_template(size)]
    when MODE_INDIRECT
      [indirect_template(size), asm_indirect_template(size)]
    when MODE_IMMEDIATE
      [IMM_TEMPLATE, IMM_TEMPLATE]
    end
  end

  def self.operand(inst, size, mode1, mode2=nil)
    if mode2.nil?

      templ = template(mode1, size)
      
      templ[0].each do |op|
        @monoasm += "\t#{inst} #{op};\n"
      end
      
      templ[1].each do |op|
        @asm += "\t#{inst} #{op};\n"
      end
      
    else
      
      templ1 = template(mode1, size)
      templ2 = template(mode2, size)

      templ1[0].each do |op1|
        templ2[0].each do |op2|
          @monoasm += "\t#{inst} #{op1}, #{op2};\n"
        end
      end

      templ1[1].each do |op1|
        templ2[1].each do |op2|
          @asm += "\t#{@asm_inst} #{op1}, #{op2};\n"
        end
      end
    end
  end

  def self.rm(inst, size)
    operand(inst, size, MODE_REG)
    operand(inst, size, MODE_INDIRECT)
  end

  def self.r_r(inst, size)
    operand(inst, size, MODE_REG, MODE_REG)
  end

  def self.rm_i(inst, size)
    operand(inst, size, MODE_REG, MODE_IMMEDIATE)
    operand(inst, size, MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.rm_1(inst, size)
    operand(inst, size, MODE_REG, MODE_IMMEDIATE)
    operand(inst, size, MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.m_r(inst, size)
    operand(inst, size, MODE_INDIRECT, MODE_REG)
  end

  def self.r_m(inst, size)
    operand(inst, size, MODE_REG, MODE_INDIRECT)
  end

  def self.gen(inst, size)
    r_r(inst, size)
    rm_i(inst, size)
    m_r(inst, size)
    r_m(inst, size)
  end

end

class Movq < Inst
  @inst = "movq"
  @asm_inst = "mov"
  @size = 8
end

class Movl < Inst
  @inst = "movl"
  @asm_inst = "mov"
  @size = 4
end

class Add < Inst
  @inst = "add"
  @asm_inst = "add"
  @size = [8, 2, 4, 1]
end

class Sub < Inst
  @inst = "sub"
  @asm_inst = "sub"
  @size = [8, 2, 4, 1]
end

class Adc < Inst
  @inst = "adc"
  @asm_inst = "adc"
  @size = [8, 2, 4, 1]
end

class Sbb < Inst
  @inst = "sbb"
  @asm_inst = "sbb"
  @size = [8, 2, 4, 1]
end

class And < Inst
  @inst = "and"
  @asm_inst = "and"
  @size = [8, 2, 4, 1]
end

class Or < Inst
  @inst = "or"
  @asm_inst = "or"
  @size = [8, 2, 4, 1]
end

class Xor < Inst
  @inst = "xor"
  @asm_inst = "xor"
  @size = [8, 2, 4, 1]
end

class Cmp < Inst
  @asm_inst = "cmp"
  @inst = "cmp"
  @size = [8, 2, 4, 1]
end

class Test < Inst
  @inst = "testq"
  @asm_inst = "test"
  @size = 8

  def self.gen(inst, size)
    r_r(inst, size)
    rm_i(inst, size)
    m_r(inst, size)
  end
end

class Push < Inst
  @inst = "pushq"
  @asm_inst = "push"
  @size = 8

  def self.gen(inst, size)
    rm(inst, size)
  end
end

class Pop < Inst
  @inst = "popq"
  @asm_inst = "pop"
  @size = 8

  def self.gen(inst, size)
    rm(inst, size)
  end
end

class Negq < Inst
  @inst = "negq"
  @asm_inst = "neg"
  @size = 8

  def self.gen(inst, size)
    rm(inst, size)
  end
end

class Shift < Inst
  def self.gen(inst, size)
    rm_i(inst, size)
    rm_1(inst, size)
  end
end

class Shl < Shift
  @inst = "shlq"
  @asm_inst = "shl"
  @size = 8
end

class Shr < Shift
  @inst = "shrq"
  @asm_inst = "shr"
  @size = 8
end

class Sal < Shift
  @inst = "salq"
  @asm_inst = "sal"
  @size = 8
end

class Sar < Shift
  @inst = "sarq"
  @asm_inst = "sar"
  @size = 8
end

class Rol < Shift
  @inst = "rolq"
  @asm_inst = "rol"
  @size = 8
end

class Ror < Shift
  @inst = "rorq"
  @asm_inst = "ror"
  @size = 8
end

instructions = [Movq, Add, Adc, Sub, Sbb, And, Or, Xor, Cmp, Shl, Shr, Sal, Sar, Rol, Ror] +
[Movl] +
[Test, Push, Pop, Negq]

`rm monoasm/tests/*.txt`
instructions.each do |inst|
  if inst.size.is_a?(Array)
    inst.size.each do |size|
      inst_name = inst.inst_name(size)
      inst.make_file(inst_name, size)
    end
  else
    inst.make_file(inst.inst, inst.size)
  end

end
`cargo test`
instructions.each do |inst|
  if inst.size.is_a?(Array)
    inst.size.each do |size|
      inst_name = inst.inst_name(size)
      inst.compare(inst_name)
    end
  else
    inst.compare(inst.inst)
  end
end
`rm monoasm/tests/*.bin`
`rm monoasm/tests/*.s`
#`rm monoasm/tests/*.rs`