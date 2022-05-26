#
# Automatic verification tool for monoasm
#

REG_TEMPLATE = [
  "rax", 
  "rcx", 
  "rdx", 
  "rbx", 
  "rsp", 
  "rbp", 
  "rsi", 
  "rdi", 
  "r8", 
  "r9", 
  "r10", 
  "r11", 
  "r12", 
  "r13", 
  "r14", 
  "r15", 
]

INDEX_TEMPLATE = [
  "rax", 
  "r15", 
]

INDIRECT_TEMPLATE = (REG_TEMPLATE + ["rip"]).map do |r|
  [ 
    "[#{r}]",
    "[#{r} + 16]",
    "[#{r} + 512]"
  ]
end.flatten +
REG_TEMPLATE.map do |r|
  INDEX_TEMPLATE.map do |i|
    ["1", "8"].map do |s|
      [
        "[#{r} + #{i} * #{s}]",
        "[#{r} + #{i} * #{s} + 20]"
      ]
    end
  end
end.flatten

ASM_INDIRECT_TEMPLATE = (REG_TEMPLATE + ["rip"]).map do |r|
  [
    "QWORD PTR [#{r}]", 
    "QWORD PTR [#{r} + 16]",
    "QWORD PTR [#{r} + 512]"
  ]
end.flatten +
REG_TEMPLATE.map do |r|
  INDEX_TEMPLATE.map do |i|
    ["1", "8"].map do |s|
      [
        "QWORD PTR [#{r} + #{i} * #{s}]",
        "QWORD PTR [#{r} + #{i} * #{s} + 20]",
      ]
    end
  end
end.flatten

IMM_TEMPLATE = ["1", "18"]

ASM_HEADER = <<EOS
.intel_syntax noprefix
.text
EOS

class Inst
  MODE_REG = 0
  MODE_INDIRECT = 1
  MODE_IMMIDIATE = 2

  def self.make_file
    @monoasm = header
    @asm = ""

    gen

    f = File.open "monoasm/tests/#{@inst}.rs", "w"
    f.write @monoasm + footer
    f.close


    f = File.open "monoasm/tests/#{@asm_inst}.s", "w"
    f.write ASM_HEADER + @asm
    f.close

    `as monoasm/tests/#{@asm_inst}.s -o monoasm/tests/#{@asm_inst}`
    `objcopy -O binary --only-section=.text monoasm/tests/#{@asm_inst} monoasm/tests/#{@asm_inst}.bin`
    `rm monoasm/tests/#{@asm_inst}`
  end

  def self.compare
    puts `diff -s monoasm/tests/#{@asm_inst}.bin monoasm/tests/#{@inst}_monoasm.bin`
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
      [REG_TEMPLATE, REG_TEMPLATE]
    when MODE_INDIRECT
      [INDIRECT_TEMPLATE, ASM_INDIRECT_TEMPLATE]
    when MODE_IMMIDIATE
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
    operand(MODE_REG, MODE_IMMIDIATE)
    operand(MODE_INDIRECT, MODE_IMMIDIATE)
  end

  def self.rm_1
    operand(MODE_REG, MODE_IMMIDIATE)
    operand(MODE_INDIRECT, MODE_IMMIDIATE)
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

class Mov < Inst
  @inst = "movq"
  @asm_inst = "mov"
end

class Add < Inst
  @inst = "addq"
  @asm_inst = "add"
end

class Sub < Inst
  @inst = "subq"
  @asm_inst = "sub"
end

class Adc < Inst
  @inst = "adcq"
  @asm_inst = "adc"
end

class Sbb < Inst
  @inst = "sbbq"
  @asm_inst = "sbb"
end

class And < Inst
  @inst = "andq"
  @asm_inst = "and"
end

class Or < Inst
  @inst = "orq"
  @asm_inst = "or"
end

class Xor < Inst
  @inst = "xorq"
  @asm_inst = "xor"
end

class Cmp < Inst
  @inst = "cmpq"
  @asm_inst = "cmp"
end

class Test < Inst
  @inst = "testq"
  @asm_inst = "test"

  def self.gen
    r_r
    rm_i
    m_r
  end
end

class Push < Inst
  @inst = "pushq"
  @asm_inst = "push"

  def self.gen
    rm
  end
end

class Pop < Inst
  @inst = "popq"
  @asm_inst = "pop"

  def self.gen
    rm
  end
end

class Neg < Inst
  @inst = "negq"
  @asm_inst = "neg"

  def self.gen
    rm
  end
end

class Shl < Inst
  @inst = "shlq"
  @asm_inst = "shl"

  def self.gen
    rm_i
    rm_1
  end
end

class Shr < Inst
  @inst = "shrq"
  @asm_inst = "shr"

  def self.gen
    rm_i
    rm_1
  end
end

instructions = [Mov, Add, Adc, Sub, Sbb, And, Or, Xor, Cmp, Test, Push, Pop, Neg, Shl, Shr]
instructions.map do |inst|
  inst.make_file
end
`cargo test`
instructions.map do |inst|
  inst.compare
end
`rm monoasm/tests/*.bin`
`rm monoasm/tests/*.s`