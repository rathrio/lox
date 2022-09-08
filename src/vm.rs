use std::convert::{TryFrom, TryInto};
use std::fmt::Write;

#[derive(Debug)]
enum Op {
    Return,
}

impl TryFrom<u8> for Op {
    type Error = String;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            op if op == Op::Return as u8 => Ok(Op::Return),
            b => Err(format!("Unknown op code {}", b)),
        }
    }
}

#[derive(Debug)]
struct Chunk {
    code: Vec<u8>,
}

impl Chunk {
    fn new() -> Self {
        Self { code: Vec::new() }
    }

    fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }

    fn disassemble(&self, name: &str) -> String {
        let mut s = format!("== {} ==\n", name);

        for (offset, instruction) in self.code.iter().enumerate() {
            write!(s, "{:0>4} ", offset).unwrap();

            let op: Op = (*instruction).try_into().unwrap();
            match op {
                Op::Return => writeln!(s, "OP_RETURN").unwrap(),
            }
        }

        s
    }
}

#[derive(Debug)]
struct VM {
    instructions: Chunk,
}

impl VM {
    fn new(instructions: Chunk) -> Self {
        Self { instructions }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foo() {
        let mut instructions = Chunk::new();
        instructions.write(Op::Return as u8);

        let vm = VM::new(instructions);
        println!("{}", vm.instructions.disassemble("test chunk"));
    }
}
