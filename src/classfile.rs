const CAFEBABE: u32 = 0xCAFEBABE;
const MAJOR_VERSION: u16 = 52;
const MINOR_VERSION: u16 = 0;

#[derive(Debug)]
pub struct Classfile {
    magic: u32,
    minor_version: u16,
    major_version: u16,
    constant_pool: Vec<Constant>,
    access_flags: u16,
    this_class: u16,
    super_class: u16,
    interfaces: Vec<Interface>,
    fields: Vec<Field>,
    methods: Vec<Method>,
    attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub enum Constant {
    String(u16),
    Utf8(String),
    Class(u16),
    NameAndType(u16, u16),
    Fieldref(u16, u16),
    Methodref(u16, u16),
}

#[derive(Debug)]
pub struct Interface;

#[derive(Debug)]
pub struct Field;

#[derive(Debug)]
pub struct Method {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub enum Attribute {
    Code(u16, u16, u16, Vec<Instruction>, Vec<ExceptionTableEntry>, Vec<Attribute>),
}

#[derive(Debug)]
pub struct ExceptionTableEntry;

#[derive(Debug)]
pub enum Instruction {
    GetStatic(u16),
    LoadConstant(u8),
    InvokeVirtual(u16),
    Bipush(u8),
    Iadd,
    Return,
}

impl Classfile {
    pub fn new(constants: Vec<Constant>, access_flags: u16, this_class: u16, super_class: u16, methods: Vec<Method>) -> Classfile {
        Classfile {
            magic: CAFEBABE,
            minor_version: MINOR_VERSION,
            major_version: MAJOR_VERSION,
            constant_pool: constants,
            access_flags: access_flags,
            this_class: this_class,
            super_class: super_class,
            interfaces: vec![],
            fields: vec![],
            methods: methods,
            attributes: vec![],
        }
    }
}

impl Method {
    pub fn new(access_flags: u16, name_index: u16, descriptor_index: u16, attributes: Vec<Attribute>) -> Method {
        Method {
            access_flags: access_flags,
            name_index: name_index,
            descriptor_index: descriptor_index,
            attributes: attributes,
        }
    }
}

pub trait Serialize {
    fn serialize(self, &mut Vec<u8>);
}

impl Serialize for Classfile {
    fn serialize(self, buf: &mut Vec<u8>) {
        self.magic.serialize(buf);
        self.minor_version.serialize(buf);
        self.major_version.serialize(buf);
        self.constant_pool.serialize(buf);
        self.access_flags.serialize(buf);
        self.this_class.serialize(buf);
        self.super_class.serialize(buf);
        self.interfaces.serialize(buf);
        self.fields.serialize(buf);
        self.methods.serialize(buf);
        self.attributes.serialize(buf);
    }
}

impl Serialize for u8 {
    fn serialize(self, buf: &mut Vec<u8>) {
        buf.push(self)
    }
}

impl Serialize for u16 {
    fn serialize(self, buf: &mut Vec<u8>) {
        buf.push((self >> 8) as u8);
        buf.push(self as u8);
    }
}

impl Serialize for u32 {
    fn serialize(self, buf: &mut Vec<u8>) {
        buf.push((self >> 24) as u8);
        buf.push((self >> 16) as u8);
        buf.push((self >> 8) as u8);
        buf.push(self as u8);
    }
}

impl Serialize for String {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for b in self.as_bytes() {
            b.serialize(buf);
        }
    }
}

impl Serialize for Vec<u8> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u32).serialize(buf); // byte vectors use a 4-byte length prefix, not 2-byte
        for b in self.into_iter() {
            b.serialize(buf);
        }
    }
}

impl Serialize for Vec<Constant> {
    fn serialize(self, buf: &mut Vec<u8>) {
        ((self.len() + 1) as u16).serialize(buf); // IMPORTANT: constant_pool_length is len + 1
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<Interface> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<Field> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<Method> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<Attribute> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<ExceptionTableEntry> {
    fn serialize(self, buf: &mut Vec<u8>) {
        (self.len() as u16).serialize(buf);
        for constant in self.into_iter() {
            constant.serialize(buf);
        }
    }
}

impl Serialize for Vec<Instruction> {
    fn serialize(self, buf: &mut Vec<u8>) {
        let mut code = vec![];
        for inst in self.into_iter() {
            inst.serialize(&mut code);
        }
        code.serialize(buf);
    }
}

impl Serialize for Constant {
    fn serialize(self, buf: &mut Vec<u8>) {
        match self {
            Constant::String(string_index) => {
                (8 as u8).serialize(buf);
                string_index.serialize(buf);
            },
            Constant::Utf8(string) => {
                (1 as u8).serialize(buf);
                string.serialize(buf);
            },
            Constant::Class(name_index) => {
                (7 as u8).serialize(buf);
                name_index.serialize(buf);
            },
            Constant::NameAndType(name_index, descriptor_index) => {
                (12 as u8).serialize(buf);
                name_index.serialize(buf);
                descriptor_index.serialize(buf);
            },
            Constant::Fieldref(class_index, name_and_type_index) => {
                (9 as u8).serialize(buf);
                class_index.serialize(buf);
                name_and_type_index.serialize(buf);
            },
            Constant::Methodref(class_index, name_and_type_index) => {
                (10 as u8).serialize(buf);
                class_index.serialize(buf);
                name_and_type_index.serialize(buf);
            },
        }
    }
}

impl Serialize for Interface {
    fn serialize(self, buf: &mut Vec<u8>) {
        panic!("TODO implement Interface::serialize")
    }
}

impl Serialize for Field {
    fn serialize(self, buf: &mut Vec<u8>) {
        panic!("TODO implement Field::serialize")
    }
}

impl Serialize for Method {
    fn serialize(self, buf: &mut Vec<u8>) {
        self.access_flags.serialize(buf);
        self.name_index.serialize(buf);
        self.descriptor_index.serialize(buf);
        self.attributes.serialize(buf);
    }
}

impl Serialize for Attribute {
    fn serialize(self, buf: &mut Vec<u8>) {
        match self {
            Attribute::Code(attribute_name_index, max_stack, max_locals, code, exception_table, attributes) => {
                // generate a temporary buffer holding the attribute "body"
                let mut buf2 = vec![];
                max_stack.serialize(&mut buf2);
                max_locals.serialize(&mut buf2);
                code.serialize(&mut buf2);
                exception_table.serialize(&mut buf2);
                attributes.serialize(&mut buf2);

                // append the attribute body to the real buffer
                attribute_name_index.serialize(buf);
                buf2.serialize(buf);
            },
        }
    }
}

impl Serialize for ExceptionTableEntry {
    fn serialize(self, buf: &mut Vec<u8>) {
        panic!("TODO implement ExceptionTableEntry::serialize")
    }
}

impl Serialize for Instruction {
    fn serialize(self, buf: &mut Vec<u8>) {
        match self {
            Instruction::GetStatic(index) => {
                (0xB2 as u8).serialize(buf);
                index.serialize(buf);
            },
            Instruction::LoadConstant(index) => {
                (0x12 as u8).serialize(buf);
                index.serialize(buf);
            },
            Instruction::InvokeVirtual(index) => {
                (0xB6 as u8).serialize(buf);
                index.serialize(buf);
            },
            Instruction::Bipush(val) => {
                (0x10 as u8).serialize(buf);
                val.serialize(buf);
            },
            Instruction::Iadd => {
                (0x60 as u8).serialize(buf);
            },
            Instruction::Return => {
                (0xB1 as u8).serialize(buf);
            },
        }
    }
}
