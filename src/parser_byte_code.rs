use super::TokenStream;

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub enum ParserByteCodeInstruction<T> {
    Label(usize),
    Nop,
    Call(usize),
    RetN(usize),
    Push(Value<T>),
    GetLocal(usize),
    SetLocal(usize),
    JumpTrue(usize),
    JumpFalse(usize),
    Jump(usize),
    EqTokenOp(Option<T>),
    ExecPred(Rc<RefCell<dyn FnMut(&Option<T>) -> bool>>),
    ReadToken,
    SavePos,
    RestorePos,
}

pub struct ParserByteCodeInstructions<T> {
    instructions: Vec<ParserByteCodeInstruction<T>>,
}

impl<T> ParserByteCodeInstructions<T> {
    pub fn new() -> ParserByteCodeInstructions<T> {
        ParserByteCodeInstructions {
            instructions: Vec::new(),
        }
    }

    pub fn next_line(&mut self) -> usize {
        self.instructions.len()
    }

    pub fn label(&mut self, ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::Label(ident));
    }

    pub fn nop(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::Nop);
    }

    pub fn call(&mut self, function: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::Call(function));
    }

    pub fn retn(&mut self, num_results: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::RetN(num_results));
    }

    pub fn push(&mut self, value: Value<T>) {
        self.instructions
            .push(ParserByteCodeInstruction::Push(value));
    }

    pub fn get_local(&mut self, ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::GetLocal(ident));
    }

    pub fn set_local(&mut self, ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::SetLocal(ident));
    }

    pub fn jump_true(&mut self, label_ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::JumpTrue(label_ident));
    }

    pub fn jump_false(&mut self, label_ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::JumpFalse(label_ident));
    }

    pub fn jump(&mut self, label_ident: usize) {
        self.instructions
            .push(ParserByteCodeInstruction::Jump(label_ident));
    }

    pub fn eq_token_op(&mut self, token_op: Option<T>) {
        self.instructions
            .push(ParserByteCodeInstruction::EqTokenOp(token_op));
    }

    pub fn exec_pred(&mut self, pred: Rc<RefCell<dyn FnMut(&Option<T>) -> bool>>) {
        self.instructions
            .push(ParserByteCodeInstruction::ExecPred(pred));
    }

    pub fn read_token(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::ReadToken);
    }

    pub fn save_pos(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::SavePos);
    }

    pub fn restore_pos(&mut self) {
        self.instructions
            .push(ParserByteCodeInstruction::RestorePos);
    }
}

#[derive(Clone)]
pub enum Value<T> {
    CodeLoc { function: usize, line: usize },
    TokenOp(Option<T>),
    TokenStreamPos(usize),
    UserVal(Rc<dyn Any>),
    Bool(bool),
    Function(usize),
}

pub struct ParserByteCode<T> {
    free_var: usize,
    functions: HashMap<usize, ParserByteCodeInstructions<T>>,
    entry_op: Option<usize>,
}

impl<T> ParserByteCode<T> {
    fn new() -> ParserByteCode<T> {
        ParserByteCode {
            free_var: 0,
            functions: HashMap::new(),
            entry_op: None,
        }
    }

    pub fn sweep_labels(&mut self) {
        let mut label_to_line_map: HashMap<usize, usize> = HashMap::new();
        for instructions in self.functions.values_mut() {
            let mut line: usize = 0;
            for i in 0..instructions.instructions.len() {
                let instruction = &instructions.instructions[i];
                match instruction {
                    &ParserByteCodeInstruction::Label(ident) => {
                        label_to_line_map.insert(ident, line);
                    }
                    _ => line += 1,
                }
            }
            for i in (0..instructions.instructions.len()).rev() {
                let instruction = &mut instructions.instructions[i];
                match instruction {
                    &mut ParserByteCodeInstruction::Label(_) => {
                        instructions.instructions.remove(i);
                    }
                    &mut ParserByteCodeInstruction::JumpTrue(ref mut ident) => {
                        *ident = *label_to_line_map.get(ident).unwrap();
                    }
                    &mut ParserByteCodeInstruction::JumpFalse(ref mut ident) => {
                        *ident = *label_to_line_map.get(ident).unwrap();
                    }
                    &mut ParserByteCodeInstruction::Jump(ref mut ident) => {
                        *ident = *label_to_line_map.get(ident).unwrap();
                    }
                    _ => {}
                }
            }
            label_to_line_map.clear();
        }
    }

    fn set_entry(&mut self, function: usize) {
        self.entry_op = Some(function);
    }

    fn alloc_var(&mut self) -> usize {
        let var = self.free_var;
        self.free_var += 1;
        return var;
    }

    fn define_func<MkInstrs: FnOnce(&mut ParserByteCode<T>, &mut ParserByteCodeInstructions<T>)>(
        &mut self,
        mk_instrs: MkInstrs,
    ) -> usize {
        let function = self.alloc_var();
        let mut instructions = ParserByteCodeInstructions::new();
        mk_instrs(self, &mut instructions);
        self.functions.insert(function, instructions);
        return function;
    }

    pub fn match_string(&mut self, str: &str) -> usize
    where
        T: From<char>,
    {
        self.define_func(|parser_byte_code, ctx| {
            let jump1 = 0;
            let jump2 = 1;
            let mut idx = 0;
            let num_chars = str.chars().count();
            for char in str.chars() {
                let token_op: Option<T> = Some(char.into());
                ctx.read_token();
                ctx.eq_token_op(token_op);
                if idx < num_chars - 1 {
                    ctx.jump_false(jump1);
                } else {
                    ctx.jump(jump2);
                }
                idx += 1;
            }
            ctx.label(jump1);
            ctx.push(Value::Bool(false));
            ctx.label(jump2);
        })
    }

    fn ordered_choice(&mut self, functions: Vec<usize>) -> usize {
        todo!();
    }

    fn unordered_choice(&mut self, functions: Vec<usize>) -> usize {
        // After function call:
        //     stack: (true, result, true, next_fn, ...): successful parse and resume exists
        //     stack: (true, result, false, ...): successful parse and no resume exists
        //     stack: (false, ...): parse was unsuccessful
        //
        // TODO: Tricky
        self.define_func(|parser_byte_code, ctx| {
            let mut first = false;
            let var_success = 0;
            let var_result = 1;
            for function in functions {
                if !first {
                    ctx.set_local(var_success);
                    ctx.get_local(var_success);
                    let jump_line = ctx.next_line();
                    ctx.nop();
                    ctx.set_local(var_result);
                }
                if first {
                    first = !first;
                }
            }
        })
    }
}

pub struct ParserByteCodeInterpretter<T> {
    parser_byte_code: ParserByteCode<T>,
    stack: Vec<Value<T>>,
    locals: HashMap<usize, Value<T>>,
}

impl<T> ParserByteCodeInterpretter<T> {
    pub fn new(parser_byte_code: ParserByteCode<T>) -> ParserByteCodeInterpretter<T> {
        ParserByteCodeInterpretter {
            parser_byte_code,
            stack: Vec::new(),
            locals: HashMap::new(),
        }
    }

    pub fn execute(&mut self, token_stream: &mut TokenStream<T>)
    where
        T: Clone + PartialEq,
    {
        self.parser_byte_code.sweep_labels();
        if self.parser_byte_code.entry_op.is_none() {
            return;
        }
        let entry = self.parser_byte_code.entry_op.unwrap();
        let mut at_function = entry;
        let mut at_line = 0;
        loop {
            let instructions = &self.parser_byte_code.functions[&at_function];
            if at_line >= instructions.instructions.len() {
                self.locals.clear();
                break;
            }
            for line in at_line..instructions.instructions.len() {
                let instruction = &instructions.instructions[line];
                match instruction {
                    &ParserByteCodeInstruction::Label(_) => {
                        panic!("Labels should already be stripped and converted to line numbers.")
                    }
                    &ParserByteCodeInstruction::Nop => {}
                    &ParserByteCodeInstruction::Call(function) => {
                        self.stack.push(Value::CodeLoc {
                            function: at_function,
                            line: line + 1,
                        });
                        at_function = function;
                        at_line = 0;
                        break;
                    }
                    &ParserByteCodeInstruction::RetN(num_results) => {
                        let value = self.stack.remove(self.stack.len() - 1 - num_results);
                        let ret_function;
                        let ret_line;
                        match value {
                            Value::CodeLoc { function, line } => {
                                ret_function = function;
                                ret_line = line;
                            }
                            _ => panic!("Expected CodeLoc in stack on call to return."),
                        }
                        at_function = ret_function;
                        at_line = ret_line;
                        self.locals.clear();
                        break;
                    }
                    &ParserByteCodeInstruction::Push(ref value) => {
                        self.stack.push(value.clone());
                    }
                    &ParserByteCodeInstruction::GetLocal(ident) => {
                        self.stack.push(
                            self.locals
                                .get(&ident)
                                .expect("Local variable read before set.")
                                .clone(),
                        );
                    }
                    &ParserByteCodeInstruction::SetLocal(ident) => {
                        let value = self.stack.pop().expect("Empty stack on set_local.");
                        self.locals.insert(ident, value);
                    }
                    &ParserByteCodeInstruction::JumpTrue(line) => {
                        let value = self.stack.pop().expect("Empty stack on jump_true.");
                        match value {
                            Value::Bool(cond) => {
                                if cond {
                                    at_line = line;
                                    break;
                                }
                            }
                            _ => panic!("Expected Bool in stack on call to jump_true."),
                        }
                    }
                    &ParserByteCodeInstruction::JumpFalse(line) => {
                        let value = self.stack.pop().expect("Empty stack on jump_false.");
                        match value {
                            Value::Bool(cond) => {
                                if !cond {
                                    at_line = line;
                                    break;
                                }
                            }
                            _ => panic!("Expected Bool in stack on call to jump_false."),
                        }
                    }
                    &ParserByteCodeInstruction::Jump(line) => {
                        at_line = line;
                        break;
                    }
                    &ParserByteCodeInstruction::EqTokenOp(ref token_op) => {
                        let value = self.stack.pop().expect("Empty stack on eq_token.");
                        match value {
                            Value::TokenOp(token_op2) => {
                                self.stack.push(Value::Bool(*token_op == token_op2));
                            }
                            _ => panic!("Expected TokenOp in stack on call to eq_token."),
                        }
                    }
                    &ParserByteCodeInstruction::ExecPred(ref pred) => {
                        let value = self.stack.pop().expect("Empty stack on exec_pred.");
                        match value {
                            Value::TokenOp(token) => {
                                self.stack.push(Value::Bool(pred.borrow_mut()(&token)));
                            }
                            _ => panic!("Expected TokenOp in stack on call to exec_pred."),
                        }
                    }
                    &ParserByteCodeInstruction::ReadToken => {
                        self.stack.push(Value::TokenOp(token_stream.read()));
                    }
                    &ParserByteCodeInstruction::SavePos => {
                        self.stack.push(Value::TokenStreamPos(token_stream.save()));
                    }
                    &ParserByteCodeInstruction::RestorePos => {
                        let value = self.stack.pop().expect("Empty stack on restore_pos.");
                        match value {
                            Value::TokenStreamPos(pos) => {
                                token_stream.restore(pos);
                            }
                            _ => panic!("Expected TokenStreamPos in stack on call to restore_pos."),
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn test_parser_byte_code() {
    let mut parser_byte_code: ParserByteCode<char> = ParserByteCode::new();
    let function = parser_byte_code.match_string("Test");
    parser_byte_code.set_entry(function);
    let mut interpretter = ParserByteCodeInterpretter::new(parser_byte_code);
    let mut token_stream = TokenStream::from_str("Test");
    interpretter.execute(&mut token_stream);
    let result = match interpretter.stack.pop().unwrap() {
        Value::Bool(x) => x,
        _ => panic!(),
    };
    println!("result = {}", result);
}
