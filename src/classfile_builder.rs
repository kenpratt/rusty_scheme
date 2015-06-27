use classfile::*;

pub const ACC_PUBLIC: u16 = 0x1;
pub const ACC_STATIC: u16 = 0x8;

pub struct ClassfileBuilder {
    access_flags: u16,
    this_class_index: u16,
    super_class_index: u16,
    constants: Vec<Constant>,
    methods: Vec<Method>,
}

impl ClassfileBuilder {
    pub fn new(access_flags: u16, this_class: &str, super_class: &str) -> ClassfileBuilder {
        let mut builder = ClassfileBuilder {
            access_flags: access_flags,
            this_class_index: 0,
            super_class_index: 0,
            constants: vec![],
            methods: vec![],
        };
        builder.this_class_index = builder.define_class(this_class);
        builder.super_class_index = builder.define_class(super_class);
        builder
    }

    pub fn define_method(&mut self, access_flags: u16, name: &str, descriptor: &str) -> MethodBuilder {
        MethodBuilder::new(self, access_flags, name, descriptor)
    }

    fn push_constant(&mut self, constant: Constant) -> u16 {
        // TODO check if this constant is exactly equal to anything already defined in constants. If so, return the existing index instead of re-defining it.
        self.constants.push(constant);
        self.constants.len() as u16
    }

    fn define_utf8(&mut self, string: &str) -> u16 {
        self.push_constant(Constant::Utf8(string.to_owned()))
    }

    fn define_class(&mut self, class: &str) -> u16 {
        let name_index = self.define_utf8(class);
        self.push_constant(Constant::Class(name_index))
    }

    fn define_name_and_type(&mut self, name: &str, descriptor: &str) -> u16 {
        let name_index = self.define_utf8(name);
        let descriptor_index = self.define_utf8(descriptor);
        self.push_constant(Constant::NameAndType(name_index, descriptor_index))
    }

    fn define_fieldref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.define_class(class);
        let name_and_type_index = self.define_name_and_type(name, descriptor);
        self.push_constant(Constant::Fieldref(class_index, name_and_type_index))
    }

    fn define_methodref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.define_class(class);
        let name_and_type_index = self.define_name_and_type(name, descriptor);
        self.push_constant(Constant::Methodref(class_index, name_and_type_index))
    }

    pub fn done(self) -> Classfile {
        Classfile::new(self.constants, self.access_flags, self.this_class_index, self.super_class_index, self.methods)
    }
}

pub struct MethodBuilder<'a> {
    classfile: &'a mut ClassfileBuilder,
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    instructions: Vec<Instruction>,
    curr_stack_depth: u16,
    max_stack_depth: u16,
}

impl<'a> MethodBuilder<'a> {
    fn new(classfile: &'a mut ClassfileBuilder, access_flags: u16, name: &str, descriptor: &str) -> MethodBuilder<'a> {
        let name_index = classfile.define_utf8(name);
        let descriptor_index = classfile.define_utf8(descriptor);
        MethodBuilder {
            classfile: classfile,
            access_flags: access_flags,
            name_index: name_index,
            descriptor_index: descriptor_index,
            instructions: vec![],
            curr_stack_depth: 0,
            max_stack_depth: 0,
        }
    }

    pub fn get_static(&mut self, class: &str, name: &str, descriptor: &str) {
        let fieldref_index = self.classfile.define_fieldref(class, name, descriptor);
        self.instructions.push(Instruction::GetStatic(fieldref_index));
        self.increase_stack_depth();
    }

    pub fn invoke_virtual(&mut self, class: &str, name: &str, descriptor: &str) {
        let methodref_index = self.classfile.define_methodref(class, name, descriptor);
        self.instructions.push(Instruction::InvokeVirtual(methodref_index));
        self.decrease_stack_depth();
    }

    pub fn bipush(&mut self, value: i8) {
        self.instructions.push(Instruction::Bipush(value as u8));
        self.increase_stack_depth();
    }

    pub fn iadd(&mut self) {
        self.instructions.push(Instruction::Iadd);
        self.decrease_stack_depth();
    }

    pub fn do_return(&mut self) {
        self.instructions.push(Instruction::Return);
    }

    fn increase_stack_depth(&mut self) {
        self.curr_stack_depth += 1;
        if self.curr_stack_depth > self.max_stack_depth {
            self.max_stack_depth = self.curr_stack_depth;
        }
    }

    fn decrease_stack_depth(&mut self) {
        self.curr_stack_depth -= 1;
    }

    pub fn done(self) {
        let classfile = self.classfile;
        let code_index = classfile.define_utf8("Code");
        // TODO track locals counts instead of hard-coding
        let method = Method::new(self.access_flags, self.name_index, self.descriptor_index, vec![Attribute::Code(code_index, self.max_stack_depth, 1, self.instructions, vec![], vec![])]);
        classfile.methods.push(method);
    }
}
