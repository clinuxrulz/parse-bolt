use super::TokenStream;

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub enum ParserByteCodeInstruction<T> {
    Nop,
    Call(usize),
    RetN(usize),
    Push(Value<T>),
    JumpTrue(usize),
    JumpFalse(usize),
    Jump(usize),
    EqTokenOp(Option<T>),
    ExecPred(Rc<RefCell<dyn FnMut(&Option<T>)->bool>>),
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

    pub fn nop(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::Nop);
    }

    pub fn call(&mut self, function: usize) {
        self.instructions.push(ParserByteCodeInstruction::Call(function));
    }

    pub fn retn(&mut self, num_results: usize) {
        self.instructions.push(ParserByteCodeInstruction::RetN(num_results));
    }

    pub fn push(&mut self, value: Value<T>) {
        self.instructions.push(ParserByteCodeInstruction::Push(value));
    }

    pub fn jump_true(&mut self, line: usize) {
        self.instructions.push(ParserByteCodeInstruction::JumpTrue(line));
    }

    pub fn jump_false(&mut self, line: usize) {
        self.instructions.push(ParserByteCodeInstruction::JumpFalse(line));
    }

    pub fn eq_token_op(&mut self, token_op: Option<T>) {
        self.instructions.push(ParserByteCodeInstruction::EqTokenOp(token_op));
    }

    pub fn exec_pred(&mut self, pred: Rc<RefCell<dyn FnMut(&Option<T>)->bool>>) {
        self.instructions.push(ParserByteCodeInstruction::ExecPred(pred));
    }

    pub fn read_token(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::ReadToken);
    }

    pub fn save_pos(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::SavePos);
    }

    pub fn restore_pos(&mut self) {
        self.instructions.push(ParserByteCodeInstruction::RestorePos);
    }

    pub fn match_string(&mut self, str: &str) where T: From<char> {
        let mut jump_lines = Vec::new();
        for char in str.chars() {
            let token_op: Option<T> = Some(char.into());
            self.read_token();
            self.eq_token_op(token_op);
            jump_lines.push(self.next_line());
            self.nop();
        }
        let end_line = self.next_line();
        self.push(Value::Bool(false));
        let end_line2 = self.next_line();
        for i in 0..jump_lines.len()-1 {
            let jump_line = jump_lines[i];
            self.instructions[jump_line] = ParserByteCodeInstruction::JumpFalse(end_line);
        }
        self.instructions[jump_lines[jump_lines.len()-1]] = ParserByteCodeInstruction::Jump(end_line2);
    }
}

#[derive(Clone)]
pub enum Value<T> {
    CodeLoc { function: usize, line: usize },
    TokenOp(Option<T>),
    TokenStreamPos(usize),
    UserVal(Rc<dyn Any>),
    Bool(bool),
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

    fn set_entry(&mut self, function: usize) {
        self.entry_op = Some(function);
    }

    fn alloc_var(&mut self) -> usize {
        let var = self.free_var;
        self.free_var += 1;
        return var;
    }

    fn define_func<MkInstrs: FnOnce(&mut ParserByteCodeInstructions<T>)>(&mut self, mk_instrs: MkInstrs) -> usize {
        let function = self.alloc_var();
        let mut instructions = ParserByteCodeInstructions::new();
        mk_instrs(&mut instructions);
        self.functions.insert(function, instructions);
        return function;
    }
}

pub struct ParserByteCodeInterpretter<T> {
    parser_byte_code: ParserByteCode<T>,
    stack: Vec<Value<T>>,
}

impl<T> ParserByteCodeInterpretter<T> {
    pub fn new(parser_byte_code: ParserByteCode<T>) -> ParserByteCodeInterpretter<T> {
        ParserByteCodeInterpretter {
            parser_byte_code,
            stack: Vec::new(),
        }
    }

    pub fn execute(&mut self, token_stream: &mut TokenStream<T>) where T: Clone + PartialEq {
        if self.parser_byte_code.entry_op.is_none() {
            return;
        }
        let entry = self.parser_byte_code.entry_op.unwrap();
        let mut at_function = entry;
        let mut at_line = 0;
        loop {
            let instructions = &self.parser_byte_code.functions[&at_function];
            for line in at_line..instructions.instructions.len() {
                let instruction = &instructions.instructions[line];
                match instruction {
                    &ParserByteCodeInstruction::Nop => {},
                    &ParserByteCodeInstruction::Call(function) => {
                        self.stack.push(Value::CodeLoc { function: at_function, line: line + 1 });
                        at_function = function;
                        at_line = 0;
                        break;
                    },
                    &ParserByteCodeInstruction::RetN(num_results) => {
                        let value = self.stack.remove(self.stack.len() - 1 - num_results);
                        let ret_function;
                        let ret_line;
                        match value {
                            Value::CodeLoc { function, line } => {
                                ret_function = function;
                                ret_line = line;
                            },
                            _ => panic!("Expected CodeLoc in stack on call to return."),
                        }
                        at_function = ret_function;
                        at_line = ret_line;
                        break;
                    },
                    &ParserByteCodeInstruction::Push(ref value) => {
                        self.stack.push(value.clone());
                    },
                    &ParserByteCodeInstruction::JumpTrue(line) => {
                        let value = self.stack.pop().expect("Empty stack on jump_true");
                        match value {
                            Value::Bool(cond) => {
                                if cond {
                                    at_line = line;
                                    break;
                                }
                            },
                            _ => panic!("Expected Bool in stack on call to jump_true."),
                        }
                    },
                    &ParserByteCodeInstruction::JumpFalse(line) => {
                        let value = self.stack.pop().expect("Empty stack on jump_true");
                        match value {
                            Value::Bool(cond) => {
                                if !cond {
                                    at_line = line;
                                    break;
                                }
                            },
                            _ => panic!("Expected Bool in stack on call to jump_true."),
                        }
                    },
                    &ParserByteCodeInstruction::Jump(line) => {
                        at_line = line;
                        break;
                    },
                    &ParserByteCodeInstruction::EqTokenOp(ref token_op) => {
                        let value = self.stack.pop().expect("Empty stack on eq_token.");
                        match value {
                            Value::TokenOp(token_op2) => {
                                self.stack.push(Value::Bool(*token_op == token_op2));
                            },
                            _ => panic!("Expected TokenOp in stack on call to eq_token."),
                        }
                    }
                    &ParserByteCodeInstruction::ExecPred(ref pred) => {
                        let value = self.stack.pop().expect("Empty stack on exec_pred.");
                        match value {
                            Value::TokenOp(token) => {
                                self.stack.push(Value::Bool(pred.borrow_mut()(&token)));
                            },
                            _ => panic!("Expected TokenOp in stack on call to exec_pred.")
                        }
                    }
                    &ParserByteCodeInstruction::ReadToken => {
                        self.stack.push(Value::TokenOp(token_stream.read()));
                    },
                    &ParserByteCodeInstruction::SavePos => {
                        self.stack.push(Value::TokenStreamPos(token_stream.save()));
                    },
                    &ParserByteCodeInstruction::RestorePos => {
                        let value = self.stack.pop().expect("Empty stack on restore_pos.");
                        match value {
                            Value::TokenStreamPos(pos) => {
                                token_stream.restore(pos);
                            },
                            _ => panic!("Expected TokenStreamPos in stack on call to restore_pos."),
                        }
                    },
                }
            }
        }
    }
}

#[test]
fn test_parser_byte_code() {
    let mut parser_byte_code: ParserByteCode<char> = ParserByteCode::new();
    let function = parser_byte_code.define_func(|ctx| {
        ctx.match_string("Test");
    });
    parser_byte_code.set_entry(function);
    let instructions = &parser_byte_code.functions[&function];
    let mut interpretter = ParserByteCodeInterpretter::new(parser_byte_code);
    let mut token_stream = TokenStream::from_str("Test");
    interpretter.execute(&mut token_stream);
    let result = match interpretter.stack.pop().unwrap() {
        Value::Bool(x) => x,
        _ => panic!(),
    };
    println!("result = {}", result);
}
