#
# Automatic verification tool for monoasm
#

def reg_template(size)
  if size ==8
    ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
  elsif size == 4
    ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"]
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

  def self.make_file
    @monoasm = header
    @asm = ""

    gen

    f = File.open "monoasm/tests/#{@inst}.rs", "w"
    f.write @monoasm + footer
    f.close


    f = File.open "monoasm/tests/#{@inst}.s", "w"
    f.write ASM_HEADER + @asm
    f.close

    `as monoasm/tests/#{@inst}.s -o monoasm/tests/#{@inst}`
    `objcopy -O binary --only-section=.text monoasm/tests/#{@inst} monoasm/tests/#{@inst}.bin`
    `rm monoasm/tests/#{@inst}`
  end

  def self.compare
    puts `diff -s monoasm/tests/#{@inst}.bin monoasm/tests/#{@inst}_monoasm.bin`
    #`rm monoasm/tests/*.bin`
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

  def self.footer
    <<EOS
      );
      jit.finalize();
      let mut buf = std::fs::File::create("tests/#{@inst}_monoasm.bin").unwrap();
      buf.write_all(jit.as_slice()).unwrap();
  }
EOS
  end

  def self.template(mode)
    case mode
    when MODE_REG
      [reg_template(8), reg_template(@size)]
    when MODE_INDIRECT
      [indirect_template(@size), asm_indirect_template(@size)]
    when MODE_IMMEDIATE
      [IMM_TEMPLATE, IMM_TEMPLATE]
    end
  end

  def self.operand(mode1, mode2=nil)
    if mode2.nil?

      templ = template(mode1)
      
      templ[0].each do |op|
        @monoasm += "\t#{@inst} #{op};\n"
      end
      
      templ[1].each do |op|
        @asm += "\t#{@inst} #{op};\n"
      end
      
    else
      
      templ1 = template(mode1)
      templ2 = template(mode2)

      templ1[0].each do |op1|
        templ2[0].each do |op2|
          @monoasm += "\t#{@inst} #{op1}, #{op2};\n"
        end
      end

      templ1[1].each do |op1|
        templ2[1].each do |op2|
          @asm += "\t#{@asm_inst} #{op1}, #{op2};\n"
        end
      end
    end
  end

  def self.rm
    operand(MODE_REG)
    operand(MODE_INDIRECT)
  end

  def self.r_r
    operand(MODE_REG, MODE_REG)
  end

  def self.rm_i
    operand(MODE_REG, MODE_IMMEDIATE)
    operand(MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.rm_1
    operand(MODE_REG, MODE_IMMEDIATE)
    operand(MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.m_r
    operand(MODE_INDIRECT, MODE_REG)
  end

  def self.r_m
    operand(MODE_REG, MODE_INDIRECT)
  end

  def self.gen
    r_r
    rm_i
    m_r
    r_m
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

class Addq < Inst
  @inst = "addq"
  @asm_inst = "add"
  @size = 8
end

class Addl < Inst
  @inst = "addl"
  @asm_inst = "add"
  @size = 4
end

class Subq < Inst
  @inst = "subq"
  @asm_inst = "sub"
  @size = 8
end

class Subl < Inst
  @inst = "subl"
  @asm_inst = "sub"
  @size = 4
end

class Adcq < Inst
  @inst = "adcq"
  @asm_inst = "adc"
  @size = 8
end

class Adcl < Inst
  @inst = "adcl"
  @asm_inst = "adc"
  @size = 4
end

class Sbbq < Inst
  @inst = "sbbq"
  @asm_inst = "sbb"
  @size = 8
end

class Sbbl < Inst
  @inst = "sbbl"
  @asm_inst = "sbb"
  @size = 4
end

class Andq < Inst
  @inst = "andq"
  @asm_inst = "and"
  @size = 8
end

class Andl < Inst
  @inst = "andl"
  @asm_inst = "and"
  @size = 4
end

class Orq < Inst
  @inst = "orq"
  @asm_inst = "or"
  @size = 8
end

class Orl < Inst
  @inst = "orl"
  @asm_inst = "or"
  @size = 4
end

class Xorq < Inst
  @inst = "xorq"
  @asm_inst = "xor"
  @size = 8
end

class Xorl < Inst
  @inst = "xorl"
  @asm_inst = "xor"
  @size = 4
end

class Cmpq < Inst
  @inst = "cmpq"
  @asm_inst = "cmp"
  @size = 8
end

class Cmpl < Inst
  @inst = "cmpl"
  @asm_inst = "cmp"
  @size = 4
end

class Test < Inst
  @inst = "testq"
  @asm_inst = "test"
  @size = 8

  def self.gen
    r_r
    rm_i
    m_r
  end
end

class Push < Inst
  @inst = "pushq"
  @asm_inst = "push"
  @size = 8

  def self.gen
    rm
  end
end

class Pop < Inst
  @inst = "popq"
  @asm_inst = "pop"
  @size = 8

  def self.gen
    rm
  end
end

class Negq < Inst
  @inst = "negq"
  @asm_inst = "neg"
  @size = 8

  def self.gen
    rm
  end
end

class Shift < Inst
  def self.gen
    rm_i
    rm_1
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

instructions = [Movq, Addq, Adcq, Subq, Sbbq, Andq, Orq, Xorq, Cmpq, Shl, Shr, Sal, Sar, Rol, Ror] +
[Movl, Addl, Adcl, Subl, Andl, Orl, Xorl, Cmpl] +
[Test, Push, Pop, Negq]

instructions.map do |inst|
  inst.make_file
end
`cargo test`
instructions.map do |inst|
  inst.compare
end
`rm monoasm/tests/*.bin`
`rm monoasm/tests/*.s`