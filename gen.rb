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

CONDITIONS = [
  "o",
  "no",
  "b",
  "c",
  "nae",
  "ae",
  "nc",
  "eq",
  "e",
  "z",
  "ne",
  "nz",
  "be",
  "na",
  "a",
  "nbe",
  "s",
  "ns",
  "p",
  "pe",
  "np",
  "po",
  "lt",
  "nge",
  "ge",
  "nl",
  "le",
  "ng",
  "gt",
  "nle"
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

  def self.inst_name(size, cond = "")
    @inst + cond + case size
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

  def self.make_files
    @files = []
    if @size.is_a?(Array)
      @size.each do |size|
        if @has_cond then
          for cond in CONDITIONS do
            asm_cond = if cond == "lt"
              "l"
            elsif cond == "gt"
              "g"
            elsif cond == "eq"
              "e"
            else
              cond
            end
            inst_name = self.inst_name(size, cond)
            make_file(inst_name, @inst + asm_cond, size)
          end
        else
          inst_name = self.inst_name(size)
          make_file(inst_name, @inst, size)
        end
      end
    else
      if @has_cond then
        for cond in CONDITIONS do
          asm_cond = if cond == "lt"
            "l"
          elsif cond == "gt"
            "g"
          elsif cond == "eq"
            "e"
          else
            cond
          end
          make_file(@inst + cond, @inst + asm_cond, size)
        end
      else
        make_file(@inst, @inst, size)
      end
    end
  end

  def self.make_file(inst, asm_inst, size)
    puts "Generating #{inst}/#{asm_inst}/#{size}..."
    @monoasm = header
    @asm = ""

    gen.each do |p|
      send(p, inst, asm_inst, size)
    end

    f = File.open "monoasm/tests/gen_#{inst}.rs", "w"
    f.write @monoasm + footer(inst)
    f.close


    f = File.open "monoasm/tests/gen/#{inst}.s", "w"
    f.write ASM_HEADER + @asm
    f.close

    `as monoasm/tests/gen/#{inst}.s -o monoasm/tests/gen/#{inst}`
    `objcopy -O binary --only-section=.text monoasm/tests/gen/#{inst} monoasm/tests/gen/#{inst}.bin`
    `rm monoasm/tests/gen/#{inst}`

    @files << inst
  end

  def self.compare_files
    @files.each do |file|
      compare(file)
    end
  end

  def self.compare(inst)
    res = `diff -y monoasm/tests/gen/#{inst}.bin monoasm/tests/gen/#{inst}_monoasm.bin`
    if res.length != 0
      puts res
      `objdump -D -Mintel,x86-64 -b binary -m i386 monoasm/tests/gen/#{inst}.bin > monoasm/tests/gen/#{inst}.txt`
      `objdump -D -Mintel,x86-64 -b binary -m i386 monoasm/tests/gen/#{inst}_monoasm.bin > monoasm/tests/gen/#{inst}_monoasm.txt`
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
      monoasm!{
          &mut jit,
EOS
  end

  def self.footer(inst)
    <<EOS
      }
      jit.finalize();
      let mut buf = std::fs::File::create("tests/gen/#{inst}_monoasm.bin").unwrap();
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

  def self.operand(inst, asm_inst, size, mode1, mode2=nil)
    if mode2.nil?

      templ = template(mode1, size)
      
      templ[0].each do |op|
        @monoasm += "\t#{inst} #{op};\n"
      end
      
      templ[1].each do |op|
        @asm += "\t#{asm_inst} #{op};\n"
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
          @asm += "\t#{asm_inst} #{op1}, #{op2};\n"
        end
      end
    end
  end

  def self.rm(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_REG)
    operand(inst, asm_inst, size, MODE_INDIRECT)
  end

  def self.r_r(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_REG, MODE_REG)
  end

  def self.rm_i(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_REG, MODE_IMMEDIATE)
    operand(inst, asm_inst, size, MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.rm_1(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_REG, MODE_IMMEDIATE)
    operand(inst, asm_inst, size, MODE_INDIRECT, MODE_IMMEDIATE)
  end

  def self.m_r(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_INDIRECT, MODE_REG)
  end

  def self.r_m(inst, asm_inst, size)
    operand(inst, asm_inst, size, MODE_REG, MODE_INDIRECT)
  end

  def self.gen
    [:r_r, :rm_i, :m_r, :r_m]
  end

end

class Mov < Inst
  @inst = "mov"
  @size = [8, 4, 2, 1]
end

class Add < Inst
  @inst = "add"
  @size = [8, 4, 2, 1]
end

class Sub < Inst
  @inst = "sub"
  @size = [8, 4, 2, 1]
end

class Adc < Inst
  @inst = "adc"
  @size = [8, 4, 2, 1]
end

class Sbb < Inst
  @inst = "sbb"
  @size = [8, 4, 2, 1]
end

class And < Inst
  @inst = "and"
  @size = [8, 4, 2, 1]
end

class Or < Inst
  @inst = "or"
  @size = [8, 4, 2, 1]
end

class Xor < Inst
  @inst = "xor"
  @size = [8, 4, 2, 1]
end

class Cmp < Inst
  @inst = "cmp"
  @size = [8, 4, 2, 1]
end

class Xchg < Inst
  @inst = "xchg"
  @size = [8]

  def self.gen
    [:r_r, :m_r, :r_m]
  end
end

class Test < Inst
  @inst = "test"
  @size = [8, 1]

  def self.gen
    [:r_r, :rm_i, :m_r]
  end
end

class Push < Inst
  @inst = "push"
  @size = [8]

  def self.gen
    [:rm]
  end
end

class Pop < Inst
  @inst = "pop"
  @size = [8]

  def self.gen
    [:rm]
  end
end

class Neg < Inst
  @inst = "neg"
  @size = [8]

  def self.gen
    [:rm]
  end
end

class Shift < Inst
  def self.gen
    [:rm_i, :rm_1]
  end
end

class Shl < Shift
  @inst = "shl"
  @size = [8, 4]
end

class Shr < Shift
  @inst = "shr"
  @size = [8, 4]
end

class Sal < Shift
  @inst = "sal"
  @size = [8, 4]
end

class Sar < Shift
  @inst = "sar"
  @size = [8, 4]
end

class Rol < Shift
  @inst = "rol"
  @size = [8, 4]
end

class Ror < Shift
  @inst = "ror"
  @size = [8, 4]
end

class Setcc < Inst
  @inst = "set"
  @has_cond = true
  @size = 1

  def self.gen
    [:rm]
  end
end

class Cmov < Inst
  @inst = "cmov"
  @has_cond = true
  @size = [8, 4, 2]

  def self.gen
    [:r_r, :r_m]
  end
end

instructions = [Mov, Add, Adc, Sub, Sbb, And, Or, Xor, Cmp, Shl, Shr, Sal, Sar, Rol, Ror] +
[Test, Xchg, Push, Pop, Neg, Cmov, Setcc]

`rm -rf monoasm/tests/gen`
`mkdir monoasm/tests/gen`
instructions.each do |inst|
  inst.make_files
end
`cargo test`
instructions.each do |inst|
  inst.compare_files
end
`rm -rf monoasm/tests/gen`
`rm monoasm/tests/gen_*.rs`