use parser;
use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use std;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    TempRegister(u8),
    SaveRegister(u8),
    ArgRegister(u8),
    V0,
    Ra,
    StackPointer

}

impl Register {
    fn to_string(&self) -> String {
        match self {
            &Register::TempRegister(n) => format!("$t{}", n),
            &Register::SaveRegister(n) => format!("$s{}", n),
            &Register::ArgRegister(n) => format!("$a{}", n),
            &Register::V0 => "$v0".to_owned(),
            &Register::Ra => "$ra".to_owned(),
            &Register::StackPointer => "$sp".to_owned(),

        }
    }
}

#[derive(Debug)]
struct LiData {
    src: Register,
    data: i32,
}


#[derive(Debug)]
struct SrcDest {
    src: Register,
    dest: Register,
}


#[derive(Debug)]
struct SrcDestOffset {
    src: Register,
    dest: Register,
    offset: usize
}


#[derive(Debug)]
enum Instruction<'a> {
    Li(LiData),
    Move(SrcDest),
    Jal(String),
    FnDecl(&'a str),
    FnDeclOwned(String),
    Jr(Register),
    BeqLit(Register, i32, &'a str),
    J(String),
    AddLit(Register, Register, i32),
    SaveWord(SrcDestOffset),
    LoadWord(SrcDestOffset),
    Beq(Register, Register, String)

}

impl<'a> Instruction<'a> {
    fn to_string(&self) -> String {
        match self {
            &Instruction::Li(LiData{src, data}) => format!("li {}, {}", src.to_string(), data),
            &Instruction::Move(SrcDest {src, dest}) => format!("move {}, {}", dest.to_string(), src.to_string()),

            &Instruction::FnDecl(name) => format!("{}:", name),
            &Instruction::FnDeclOwned(ref name) => format!("{}:", name),

            &Instruction::BeqLit(reg, lit, dest) => format!("beq {}, {}, {}", reg.to_string(), lit, dest),
            &Instruction::Beq(reg1, reg2, ref dest) => format!("beq {}, {}, {}", reg1.to_string(), reg2.to_string(), dest),

            &Instruction::J(ref dest) => format!("j {}", dest),
            &Instruction::Jal(ref name) => format!("jal {}", name),
            &Instruction::Jr(reg) => format!("jr {}", reg.to_string()),

            &Instruction::AddLit(dest, operand1, i32lit) => format!("add {}, {}, {}" , dest.to_string(), operand1.to_string(), i32lit),

            &Instruction::SaveWord(SrcDestOffset {src, dest, offset}) => format!("sw {}, {}({})", src.to_string(), offset, dest.to_string()),
            &Instruction::LoadWord(SrcDestOffset {src, dest, offset}) => format!("lw {}, {}({})", dest.to_string(), offset, src.to_string()),
        }
    }
}



#[derive(Debug)]
struct RegisterController<'a> {
    used_registers: HashSet<Register>,
    var_lookups: HashMap<&'a str, Register>,
}

impl<'a> RegisterController<'a> {
    fn new() -> RegisterController<'a> {
        RegisterController {used_registers: HashSet::new(), var_lookups: HashMap::new()}
    }

    // YOU SHOULD NEVER DIRECTLY CALL THIS
    fn get_free_t_reg(&mut self) -> Option<Register> {
        for i in 0..10 {
            let reg = Register::TempRegister(i);
            if !self.used_registers.contains(&reg) {
                return Some(reg)
            }

        }
        return None
    }

    // YOU SHOULD NEVER DIRECTLY CALL THIS
    fn get_free_s_reg(&mut self) -> Option<Register> {
        for i in 0..9 {
            let reg = Register::SaveRegister(i);
            if !self.used_registers.contains(&reg) {
                return Some(reg);
            }
        }
        None
    }


    fn request_free_register(&mut self) -> Register {
        let mut maybe_reg = self.get_free_t_reg();

        if maybe_reg.is_none() {
            maybe_reg = self.get_free_s_reg();
        }


        match maybe_reg {
            Some(reg) => {
                self.used_registers.insert(reg);
                reg
            },
            None => {
                panic!("All the registers are already used!!! {:?}, {:?}", self.used_registers, self.var_lookups)
            }
        }
    }

    fn request_var_register<'b>(&'b mut self, var_name: &'a str) -> Register {
        let reg = self.request_free_register();
        let prev = self.var_lookups.insert(var_name, reg);

        assert!(prev.is_none(), "the variable name {} is already being used", var_name);
        reg

    }

    fn request_anonymous_register(&mut self) -> Register {
        self.request_free_register()
    }

    fn get_register(&self, name: &'a str) -> Register {
        self.var_lookups.get(name).expect(&format!("Expected to lookup register for {} but found nothing!", name)).clone()
    }
}



#[derive(Debug)]
struct MipsFunction<'a> {
    name: String,
    instr: Vec<Instruction<'a>>
}



#[derive(Debug)]
struct MipsConv<'b, 'a : 'b> {
    reg_ctr: RegisterController<'a>,
    program: &'b parser::Program<'a>,
    functions: Vec<MipsFunction<'a>>,
    counter: u32,
}



impl<'b, 'a : 'b> MipsConv<'b, 'a> {

    fn for_loop(&mut self, var_name: &'a str, start_e: &parser::Expr<'a>, end_e: &parser::Expr<'a>, block: &parser::Block<'a>) -> Vec<Instruction<'a>> {
        let iter_reg = self.reg_ctr.request_var_register(var_name);
        let end_reg = self.reg_ctr.request_anonymous_register();

        let evaluate_end_code = self.evaluate_into_register(end_reg, end_e);
        let evaluate_start_code = self.evaluate_into_register(iter_reg, start_e);



        let loop_name = format!("auto_generated_loop_{}", self.counter);
        let loop_ret_name = format!("auto_generated_loop_ret_{}", self.counter);
        self.counter += 1;


        let mut inner_loop_fn = vec![
            Instruction::FnDeclOwned(loop_name.clone()),
            Instruction::Beq(iter_reg, end_reg, loop_ret_name.clone()),
        ];

        inner_loop_fn.extend(self.block_to_mips(block));
        inner_loop_fn.push(Instruction::AddLit(iter_reg, iter_reg, 1));
        inner_loop_fn.push(Instruction::J(loop_name.clone()));


        let loop_return_fn = vec![
            Instruction::FnDeclOwned(loop_ret_name.clone()),
            Instruction::Jr(Register::Ra),
        ];


        self.functions.push(MipsFunction {instr: inner_loop_fn, name: loop_name.clone()} );
        self.functions.push(MipsFunction {instr: loop_return_fn, name: loop_ret_name });



        let mut full_calling_code = Vec::new();
        full_calling_code.extend(evaluate_start_code);
        full_calling_code.extend(evaluate_end_code);



        let ra_temp_reg = self.reg_ctr.request_anonymous_register();

        full_calling_code.push(Instruction::Move(SrcDest {src: Register::Ra, dest: ra_temp_reg}));
        full_calling_code.push(Instruction::Jal(loop_name));
        full_calling_code.push(Instruction::Move(SrcDest{src: ra_temp_reg, dest: Register::Ra}));

        // todo: free(ra_temp_reg)



        full_calling_code

    }






    fn conditional_branch<'c>(&mut self, cond:&parser::Condition<'a>, dest: &'c str) -> Vec<Instruction<'a>> {

        match cond {
            &parser::Condition::Equals(ref expr1, ref expr2) => {
                let lhs_reg = self.reg_ctr.request_anonymous_register();
                let lhs_eval_inst = self.evaluate_into_register(lhs_reg, expr1);

                let rhs_reg = self.reg_ctr.request_anonymous_register();
                let rhs_eval_inst = self.evaluate_into_register(rhs_reg, expr2);

                // Todo: do this on the stack instead?
                let ra_copy_reg = self.reg_ctr.request_anonymous_register();



                let call_and_update_ra = vec![
                    Instruction::Move(SrcDest {src: Register::Ra, dest: ra_copy_reg}),
                    Instruction::Jal("refresh_ra".to_owned()), // part of std_lib
                    Instruction::AddLit(Register::Ra, Register::Ra, 8), // Move 2 lines down (1st goes to the beq, 2nd goes to the next line)
                    Instruction::Beq(lhs_reg, rhs_reg, String::from(dest)),
                    Instruction::Move(SrcDest {src: ra_copy_reg, dest: Register::Ra}),
                ];

                let mut full_instructions = Vec::new();

                full_instructions.extend(lhs_eval_inst);
                full_instructions.extend(rhs_eval_inst);
                full_instructions.extend(call_and_update_ra);

                full_instructions


            }
        }


    }


    fn if_statement(&mut self, condition: &parser::Condition<'a>, block: &parser::Block<'a>) -> Vec<Instruction<'a>> {

        let owned_name = format!("auto_generated_if_{}", self.counter);


        self.counter += 1;


        let condition_instructions = self.conditional_branch(condition, &owned_name);




        let mut block_instrs = Vec::new();
        block_instrs.push(Instruction::FnDeclOwned(owned_name.clone()));
        block_instrs.extend(self.block_to_mips(block));
        block_instrs.push(Instruction::Jr(Register::Ra));


        let func = MipsFunction {instr: block_instrs, name: owned_name};
        self.functions.push(func);


        condition_instructions



    }

    fn declaration_to_mips(&mut self, dec: &parser::DeclData<'a>) -> Vec<Instruction<'a>> {
        let register = self.reg_ctr.request_var_register(dec.name);
        let value_expr = &dec.value;

        self.evaluate_into_register(register, value_expr)
    }

    fn call_function(&mut self, name: &'a str, args: &Vec<parser::Expr<'a>>) -> Vec<Instruction<'a>> { // Calls the function and leaves the result in v0
        assert!(args.len() <= 4);
        let mut eval_arg_instructions = Vec::new();

        for (i, arg_expr) in args.iter().enumerate() {
            let reg = Register::ArgRegister(i as u8);
            eval_arg_instructions.extend(self.evaluate_into_register(reg, arg_expr));
        }

        let stack_size = 4;



        let data_save_instructions = vec![
            Instruction::AddLit(Register::StackPointer, Register::StackPointer, -stack_size),
            Instruction::SaveWord(SrcDestOffset { src: Register::Ra, dest: Register::StackPointer, offset: 0}),
        ];



        let data_load_instructions = vec![
            Instruction::LoadWord(SrcDestOffset {dest: Register::Ra, src: Register::StackPointer, offset: 0}),
            Instruction::AddLit(Register::StackPointer, Register::StackPointer, stack_size),
        ];



        let mut full_instructions = Vec::new();

        full_instructions.extend(eval_arg_instructions);
        full_instructions.extend(data_save_instructions);
        full_instructions.push(Instruction::Jal(name.to_owned()));
        full_instructions.extend(data_load_instructions);





        full_instructions
    }



    fn evaluate_into_register(&mut self, reg: Register, expr: &parser::Expr<'a>) -> Vec<Instruction<'a>> {
        match expr {
            &parser::Expr::I32_lit(the_i32) => {
                vec![Instruction::Li(LiData {src: reg, data: the_i32})]
            }
            &parser::Expr::VarLookup(var_name) => {
                vec![Instruction::Move(SrcDest {src: self.reg_ctr.get_register(var_name), dest: reg})]
            }
            &parser::Expr::Funcall(name, ref args) => {
                let mut insts = self.call_function(name, args);
                insts.push(Instruction::Move(SrcDest { src: Register::V0, dest: reg}));
                insts
            }

        }

    }

    fn assignment(&mut self, ident: &'a str, expr: &parser::Expr<'a>) -> Vec<Instruction<'a>> {
        let reg = self.reg_ctr.get_register(ident);


        self.evaluate_into_register(reg, expr)

    }





    // Returns the code representing that block, but is also allowed to create functions
    fn block_to_mips(&mut self, block: &parser::Block<'a>) -> Vec<Instruction<'a>> {
        let mut instructions = Vec::new();
        for statement in block.stmts.iter() {
            let new_instrs = match statement {
                &parser::Statement::Declaration(ref DeclData) => {
                    self.declaration_to_mips(DeclData)
                }
                &parser::Statement::Evaluation(parser::Expr::Funcall(name, ref args)) => {
                    self.call_function(name, args)
                },
                &parser::Statement::ForLoop(parser::LoopData {var_name, ref start_expr, ref end_expr, ref block}) => {
                    self.for_loop(var_name, start_expr, end_expr, block)
                },
                &parser::Statement::If(parser::IfData{ ref condition, ref block}) => {
                    self.if_statement(condition, block)
                },
                &parser::Statement::Assignment(idet, ref expr) => {
                    self.assignment(idet, expr)
                }
                _ => {
                    panic!("Not yet impelemented mips for {:?}", statement)
                }
            };
            instructions.extend(new_instrs);
        }
        instructions
    }


    fn convert_function_to_mips(&mut self, f:&parser::Function<'a>) -> Vec<Instruction<'a>>{
        let fn_name = f.name;
        let mut instructions = Vec::new();

        instructions.push(Instruction::FnDecl(fn_name));
        instructions.extend(self.block_to_mips(&f.block));


        // TODO ADD THIS TO FUNCTIONS
        instructions

    }

    fn convert_all(&mut self) {
        for (name, function) in self.program.functions.iter() {
            let mut instructions = self.convert_function_to_mips(function);

            if name == &"main" {
                instructions.push(Instruction::J("mips_exit".to_owned()));

            }
            self.functions.push(MipsFunction {name: (String::from(*name)) , instr: instructions});
        }
    }

    fn dump_to_string(&self) -> String {
        let instructions = self.functions.iter().flat_map(|f| f.instr.iter().map(Instruction::to_string).chain(std::iter::once("\n".to_owned()))).collect::<Vec<_>>();



        let mut std_lib =
            r#"
### BEGIN STD LIB ###
mips_mul:
mult $a0, $a1
mflo $v0
jr $ra


refresh_ra:
jr $ra

mips_add:
add $v0, $a0, $a1
jr $ra

mips_print_i32:
li $v0, 1
syscall
li $a0, 10
li $v0, 11
syscall
jr $ra


mips_exit:
li $v0, 10
syscall
jr $ra


mips_rem:
div $a0, $a1
mfhi $v0
jr $ra

####  END STD LIB ###
"#.to_owned();

        std_lib.push_str(&instructions.join("\n"));
        std_lib
    }



}

pub fn to_mips<'a>(program:&parser::Program<'a>) -> String {
    let mut mips_conv = MipsConv {reg_ctr : RegisterController::new(), program, functions: Vec::new(), counter: 0};
    mips_conv.convert_all();



    println!("{:#?}", mips_conv.reg_ctr);
    println!("Used_registers = {:?}", mips_conv.reg_ctr.used_registers.len());


    mips_conv.dump_to_string()


}











