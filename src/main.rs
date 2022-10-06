use std::{fmt::Display, process::exit};

const OUTPUT_FILE: &str = "rom.hex";

trait Instruction {
    fn compile(&self) -> u16;
}

struct RdRsInstruction {
    opcode: u8,
    opcode_x: u8,
    rd: u8,
    rs: u8,
}

impl Instruction for RdRsInstruction {
    #[allow(clippy::identity_op)]
    fn compile(&self) -> u16 {
        (((self.opcode & 0b1111) as u16) << 12)
            | (((self.rd & 0b1111) as u16) << 8)
            | (((self.opcode_x & 0b1111) as u16) << 4)
            | (((self.rs & 0b1111) as u16) << 0)
    }
}

struct RdImmInstruction {
    opcode: u8,
    rd: u8,
    imm: u8,
}

impl Instruction for RdImmInstruction {
    #[allow(clippy::identity_op)]
    fn compile(&self) -> u16 {
        (((self.opcode & 0b1111) as u16) << 12)
            | (((self.rd & 0b1111) as u16) << 8)
            | (((self.imm & 0b11111111) as u16) << 0)
    }
}

struct ImmInstruction {
    opcode: u8,
    opcode_x: u8,
    imm: u8,
}

impl Instruction for ImmInstruction {
    #[allow(clippy::identity_op)]
    fn compile(&self) -> u16 {
        (((self.opcode & 0b1111) as u16) << 12)
            | (((self.opcode_x & 0b1111) as u16) << 8)
            | (((self.imm & 0b11111111) as u16) << 0)
    }
}

#[derive(Debug)]
enum ImmError {
    Malformed,
    ParseError(String),
}

impl Display for ImmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ImmError::Malformed => "Malformed immediate value".to_string(),
                ImmError::ParseError(value) => format!("Problem parsing immediate value {}", value),
            }
        )
    }
}

fn parse_imm(raw_value: Option<&str>) -> Result<u8, ImmError> {
    if let Some(raw_value) = raw_value {
        let (base, value) = match &raw_value[0..2] {
            "0x" => (16, &raw_value[2..]),
            "0b" => (2, &raw_value[2..]),
            "0o" => (8, &raw_value[2..]),
            _ => (10, raw_value),
        };

        u8::from_str_radix(value, base).map_err(|_| ImmError::ParseError(raw_value.to_string()))
    } else {
        Err(ImmError::Malformed)
    }
}

#[derive(Debug)]
enum RegisterError {
    Malformed,
    ParseError(String),
}

impl Display for RegisterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RegisterError::Malformed => "Malformed register".to_string(),
                RegisterError::ParseError(value) => format!("Problem parsing register {}", value),
            }
        )
    }
}

fn parse_register(raw_value: Option<&str>) -> Result<u8, RegisterError> {
    if let Some(raw_value) = raw_value {
        if raw_value.len() > 1 {
            if let (Some('r'), number) = (
                raw_value.chars().next(),
                raw_value.chars().skip(1).collect::<String>(),
            ) {
                return number
                    .parse::<u8>()
                    .map_err(|_| RegisterError::ParseError(raw_value.to_string()));
            }
        }

        Err(RegisterError::ParseError(raw_value.to_string()))
    } else {
        Err(RegisterError::Malformed)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
enum OpcodeError {
    Malformed,
    Unknown(String),
}

impl Display for OpcodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OpcodeError::Malformed => "Malformed opcode".to_string(),
                OpcodeError::Unknown(value) => format!("Unknown opcode {}", value),
            }
        )
    }
}

fn parse_opcode(raw_value: &str) -> Result<(u8, Option<u8>), OpcodeError> {
    let imm_instruction = raw_value.chars().last().map_or(false, |c| c == 'i');

    let short_opcode = if imm_instruction {
        &raw_value[..raw_value.len() - 1]
    } else {
        raw_value
    };

    if let Some(mut opcode) = match short_opcode {
        "and" => Some(0b0001),
        "or" => Some(0b0110),
        "xor" => Some(0b0111),
        "add" => Some(0b0100),
        "lsl" => Some(0b1001),
        "asl" => Some(0b1000),
        "lsr" => Some(0b1011),
        "asr" => Some(0b1010),
        "ld" => Some(0b0011),
        "sw" => Some(0b0010),
        _ => None,
    } {
        let opcode_x = if imm_instruction { None } else { Some(opcode) };
        if !imm_instruction {
            opcode = 0b1100;
        }

        Ok((opcode, opcode_x))
    } else {
        Err(OpcodeError::Unknown(raw_value.to_string()))
    }
}

fn main() {
    let input = "\
ldi r4, 0xC4
ldi r7, 0x3F
and r4, r7
addi r3, 0x34
j 0xD2
";

    let mut compiled: Vec<u16> = Vec::with_capacity(input.len());

    for (line_number, raw_line) in input.lines().enumerate() {
        let line = raw_line.replace(',', "");

        let (instruction, mut operands) = {
            let mut iter = line.split(' ');

            (iter.next(), iter.to_owned())
        };

        if let Some(instruction) = instruction {
            let parsed_instruction_result: Result<Box<dyn Instruction>, String> = match {
                let mut chars = instruction.chars();
                (chars.next(), chars.last())
            } {
                (Some('j'), _) => {
                    let opcode_x: Option<u8> = match instruction {
                        "jeq" => Some(0b0000),
                        "jge" => Some(0b0001),
                        "jgt" => Some(0b0010),
                        "jhi" => Some(0b0011),
                        "jhs" => Some(0b0100),
                        "jne" => Some(0b1000),
                        "jlt" => Some(0b1001),
                        "jle" => Some(0b1010),
                        "jls" => Some(0b1011),
                        "jlo" => Some(0b1100),
                        "j" => Some(0b0111),
                        _ => None,
                    };
                    let imm = parse_imm(operands.next());

                    match (opcode_x, imm) {
                        (Some(opcode_x), Ok(imm)) => Ok(Box::new(ImmInstruction {
                            opcode: 0b1110,
                            opcode_x,
                            imm,
                        })),
                        (None, _) => Err(format!("Unknown jump condition {}", instruction)),
                        (_, Err(imm_error)) => {
                            Err(format!("Problem with immediate value: {}", imm_error))
                        }
                    }
                }
                (_, Some('i')) => {
                    // rd imm instruction
                    let opcodes = parse_opcode(instruction);
                    let rd = parse_register(operands.next());
                    let imm = parse_imm(operands.next());

                    match opcodes {
                        Ok((opcode, None)) => match (rd, imm) {
                            (Ok(rd), Ok(imm)) => Ok(Box::new(RdImmInstruction { opcode, rd, imm })),
                            (Err(rd_error), _) => {
                                Err(format!("Problem with rd register: {}", rd_error))
                            }
                            (_, Err(imm_error)) => {
                                Err(format!("Problem with immediate value: {}", imm_error))
                            }
                        },
                        Ok((_, Some(_))) => unreachable!(
                            "rd-imm instruction should not result in opcode-x being generated"
                        ),
                        Err(error) => Err(format!("Problem with instruction: {}", error)),
                    }
                }
                _ => {
                    // rd rs instruction
                    let opcodes = parse_opcode(instruction);
                    let rd = parse_register(operands.next());
                    let rs = parse_register(operands.next());

                    match opcodes {
                        Ok((opcode, Some(opcode_x))) => match (rd, rs) {
                            (Ok(rd), Ok(rs)) => Ok(Box::new(RdRsInstruction {
                                opcode,
                                opcode_x,
                                rd,
                                rs,
                            })),
                            (Err(rd_error), _) => {
                                Err(format!("Problem with rd register: {}", rd_error))
                            }
                            (_, Err(imm_error)) => {
                                Err(format!("Problem with immediate value: {}", imm_error))
                            }
                        },
                        Ok((_, None)) => unreachable!(
                            "rd-rs instruction should result in opcode-x being generated"
                        ),
                        Err(error) => Err(format!("Problem with instruction: {}", error)),
                    }
                }
            };

            match parsed_instruction_result {
                Ok(instruction) => compiled.push(instruction.compile()),
                Err(error) => {
                    eprintln!("Error while compiling on line {}:", line_number + 1);
                    eprintln!("    {}", raw_line);
                    eprintln!();
                    eprintln!("    {}", error);

                    exit(1);
                }
            };
        }
    }

    // Convert compiled bytes into Intel Hex format
    let mut output: Vec<String> = compiled
        .iter()
        .enumerate()
        .map(|(starting_address, word)| {
            let num_data_bytes = 2;
            let record_type = 0;

            let line = format!(
                "{:02x}{:04x}{:02x}{:04x}",
                num_data_bytes, starting_address, record_type, word
            );

            let mut checksum: u8 = 0;

            let mut shrinking_line = line.clone();
            while !shrinking_line.is_empty() {
                (checksum, _) = checksum
                    .overflowing_add(u8::from_str_radix(&shrinking_line[0..2], 16).unwrap());
                shrinking_line = shrinking_line[2..].to_string();
            }

            checksum = (!checksum) + 1;

            format!(":{}{:02x}", line, checksum)
        })
        .collect();
    output.push(":00000001FF".to_string());

    let output = output.join("\n");

    if let Err(e) = std::fs::write(OUTPUT_FILE, output) {
        eprintln!("Problem writing to output file");
        eprintln!("{:?}", e);
    } else {
        println!("Successfully write to output file");
    }
}
