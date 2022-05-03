#
# Automatic verification tool for monoasm
#

REG = [
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
ASM_HEADER = <<EOS
.intel_syntax noprefix
.text
EOS

class Inst

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

  def self.r_r
    @monoasm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@inst} #{r1}, #{r2};\n"
      end.join + "\n"
    end.join + "\n"

    @asm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@asm_inst} #{r1}, #{r2};\n"
      end.join + "\n"
    end.join + "\n"
  end

  def self.rm_i
    @monoasm += REG.map do |r|
      "\t#{@inst} #{r}, 16;\n" +
      "\t#{@inst} [#{r}], 16;\n" +
      "\t#{@inst} [#{r} + 16], 16;\n"
    end.join() + "\n"

    @asm += REG.map do |r|
      "\t#{@asm_inst} #{r}, 16;\n" +
      "\t#{@asm_inst} QWORD PTR [#{r}], 16;\n" +
      "\t#{@asm_inst} QWORD PTR [#{r} + 16], 16;\n"
    end.join() + "\n"
  end

  def self.m_r
    @monoasm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@inst} [#{r1}], #{r2};\n" +
        "\t#{@inst} [#{r1} + 16], #{r2};\n"
      end.join + "\n"
    end.join + "\n"

    @asm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@asm_inst} QWORD PTR [#{r1}], #{r2};\n" +
        "\t#{@asm_inst} QWORD PTR [#{r1} + 16], #{r2};\n"
      end.join + "\n"
    end.join + "\n"
  end

  def self.r_m
    @monoasm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@inst} #{r1}, [#{r2}];\n" +
        "\t#{@inst} #{r1}, [#{r2} + 16];\n"
      end.join + "\n"
    end.join + "\n"

    @asm += REG.map do |r1|
      REG.map do |r2|
        "\t#{@asm_inst} #{r1}, QWORD PTR [#{r2}];\n" +
        "\t#{@asm_inst} #{r1}, QWORD PTR [#{r2} + 16];\n"
      end.join + "\n"
    end.join + "\n"
  end

  def self.gen
    r_r
    rm_i
    m_r
    r_m
  end

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

instructions = [Mov, Add, Adc, Sub, Sbb, And, Or, Xor, Cmp, Test]
instructions.map do |inst|
  inst.make_file
end
`cargo test`
instructions.map do |inst|
  inst.compare
end
`rm monoasm/tests/*.bin`
`rm monoasm/tests/*.s`