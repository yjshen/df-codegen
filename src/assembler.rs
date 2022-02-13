use crate::error::{DataFusionError, Result};
use cranelift::codegen::ir;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::sync::Arc;

pub enum StmtCode {
    IfElse(Box<ExprCode>, Vec<StmtCode>, Vec<StmtCode>),
    WhileLoop(Box<ExprCode>, Vec<StmtCode>),
    Declare(NameType),
    Assign(String, Box<ExprCode>),
    Initialization(NameType, Box<ExprCode>),
    SideEffect(Box<ExprCode>),
}

pub struct NameType {
    pub name: String,
    pub typ: Type,
}

impl NameType {
    pub fn new(name: String, typ: Type) -> Self {
        Self { name, typ }
    }
}

pub enum ExprCode {
    Literal(String),
    Identifier(String),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

pub struct Expr {
    code: ExprCode,
    type_: Type,
}

impl Expr {
    pub fn new(code: ExprCode, type_: Type) -> Self {
        Self { code, type_ }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Type(ir::Type, u8);
pub const NIL: Type = Type(ir::types::INVALID, 0);
pub const BOOL: Type = Type(ir::types::B1, 1);
pub const I8: Type = Type(ir::types::I8, 2);
pub const I16: Type = Type(ir::types::I16, 3);
pub const I32: Type = Type(ir::types::I32, 4);
pub const I64: Type = Type(ir::types::I64, 5);
pub const F32: Type = Type(ir::types::F32, 6);
pub const F64: Type = Type(ir::types::F64, 7);
pub const R32: Type = Type(ir::types::R32, 8);
pub const R64: Type = Type(ir::types::R64, 9);

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            0 => write!(f, "nil"),
            1 => write!(f, "bool"),
            2 => write!(f, "i8"),
            3 => write!(f, "i16"),
            4 => write!(f, "i32"),
            5 => write!(f, "i64"),
            6 => write!(f, "f32"),
            7 => write!(f, "f64"),
            8 => write!(f, "small_ptr"),
            9 => write!(f, "ptr"),
            _ => write!(f, "unknown"),
        }
    }
}

impl From<&str> for Type {
    fn from(x: &str) -> Self {
        match x {
            "bool" => BOOL,
            "i8" => I8,
            "i16" => I16,
            "i32" => I32,
            "i64" => I64,
            "f32" => F32,
            "f64" => F64,
            "small_ptr" => R32,
            "ptr" => R64,
            _ => panic!("unknown type: {}", x),
        }
    }
}

struct ExternFuncSignature {
    name: String,
    code: *const u8,
    params: Vec<Type>,
    returns: Option<Type>,
}

pub struct AssemblerState {
    name_next_id: HashMap<String, u8>,
    extern_funcs: HashMap<String, ExternFuncSignature>,
    fields: HashMap<String, Type>,
}

impl AssemblerState {
    pub fn fresh_name(&mut self, name: &str) -> String {
        if !self.name_next_id.contains_key(name) {
            self.name_next_id.insert(name.to_owned(), 0);
        }

        let id = self.name_next_id.get_mut(name).unwrap();
        let name = format!("{}_{}", name, id);
        *id += 1;
        name
    }
}

pub struct Assembler {
    pub state: Arc<Mutex<AssemblerState>>,
}

macro_rules! err {
    ($($arg:tt)*) => {
        Err(DataFusionError::Internal(format!($($arg)*)))
    };
}

impl Assembler {
    pub fn lit(&self, val: impl Into<String>, ty: Type) -> Expr {
        Expr::new(ExprCode::Literal(val.into()), ty)
    }

    pub fn id(&self, name: impl Into<String>) -> Result<Expr> {
        let name = name.into();
        let fields = &self.state.lock().fields;
        let type_ = fields.get(&name);
        match type_ {
            Some(type_) => Ok(Expr::new(ExprCode::Identifier(name), *type_)),
            None => err!("unknown identifier: {}", name),
        }
    }

    pub fn register_extern_fn(
        &self,
        name: impl Into<String>,
        ptr: *const u8,
        params: Vec<Type>,
        returns: Option<Type>,
    ) -> Result<()> {
        let extern_funcs = &mut self.state.lock().extern_funcs;
        let fn_name = name.into();
        let old = extern_funcs.insert(
            fn_name.clone(),
            ExternFuncSignature {
                name: fn_name,
                code: ptr,
                params,
                returns,
            },
        );

        match old {
            None => Ok(()),
            Some(old) => err!("Function {} already exists", old.name),
        }
    }

    pub fn call(&self, name: impl Into<String>, params: Vec<Expr>) -> Result<Expr> {
        let fn_name = name.into();
        if let Some(func) = self.state.lock().extern_funcs.get(&fn_name) {
            for ((i, t1), t2) in params.iter().enumerate().zip(func.params.iter()) {
                if t1.type_ != *t2 {
                    return err!(
                        "Func {} need {} as arg{}, get {}",
                        &fn_name,
                        t2,
                        i,
                        t1.type_
                    );
                }
            }
            Ok(Expr::new(
                ExprCode::Call(fn_name, params),
                func.returns.unwrap_or(NIL),
            ))
        } else {
            err!("No func with the name {} exist", fn_name)
        }
    }
}
