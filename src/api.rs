use crate::error::{DataFusionError, Result};
use cranelift::codegen::ir;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum Stmt {
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    WhileLoop(Box<Expr>, Vec<Stmt>),
    Assign(String, Box<Expr>),
    SideEffect(Box<Expr>),
    Declare(String, Type),
}

#[derive(Clone, Debug)]
pub struct Expr {
    code: ExprCode,
    type_: Type,
}

impl Expr {
    pub fn new(code: ExprCode, type_: Type) -> Self {
        Self { code, type_ }
    }
}

#[derive(Clone, Debug)]
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
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Debug for Type {
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

pub struct AssemblerState {
    name_next_id: HashMap<String, u8>,
    extern_funcs: HashMap<String, ExternFuncSignature>,
}

impl Default for AssemblerState {
    fn default() -> Self {
        Self {
            name_next_id: Default::default(),
            extern_funcs: Default::default(),
        }
    }
}

impl AssemblerState {
    pub fn fresh_name(&mut self, name: impl Into<String>) -> String {
        let name = name.into();
        if !self.name_next_id.contains_key(&name) {
            self.name_next_id.insert(name.clone(), 0);
        }

        let id = self.name_next_id.get_mut(&name).unwrap();
        let name = format!("{}_{}", &name, id);
        *id += 1;
        name
    }
}

macro_rules! err {
    ($($arg:tt)*) => {
        Err(DataFusionError::Internal(format!($($arg)*)))
    };
}

pub struct GeneratedFunction {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Vec<Stmt>,
    pub ret: Option<(String, Type)>,
}

pub struct Assembler {
    pub state: Arc<Mutex<AssemblerState>>,
}

impl Default for Assembler {
    fn default() -> Self {
        Self {
            state: Arc::new(Default::default()),
        }
    }
}

impl Assembler {
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
            Some(old) => err!("Extern function {} already exists", old.name),
        }
    }

    pub fn new_func_builder(&self, name: impl Into<String>) -> FunctionBuilder {
        let name = self.state.lock().fresh_name(name);
        FunctionBuilder::new(name, self.state.clone())
    }
}

pub struct FunctionBuilder {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Vec<Stmt>,
    pub ret: Option<(String, Type)>,
    fields: VecDeque<HashMap<String, Type>>,
    assembler_state: Arc<Mutex<AssemblerState>>,
}

impl FunctionBuilder {
    pub fn new(name: impl Into<String>, assembler_state: Arc<Mutex<AssemblerState>>) -> Self {
        let mut fields = VecDeque::new();
        fields.push_back(HashMap::new());
        Self {
            name: name.into(),
            params: Vec::new(),
            body: Vec::new(),
            ret: None,
            fields,
            assembler_state,
        }
    }

    pub fn param(mut self, name: impl Into<String>, ty: Type) -> Self {
        let name = name.into();
        assert!(!self.fields.back().unwrap().contains_key(&name));
        self.params.push((name.clone(), ty));
        self.fields.back_mut().unwrap().insert(name, ty);
        self
    }

    pub fn ret(mut self, name: impl Into<String>, ty: Type) -> Self {
        let name = name.into();
        assert!(!self.fields.back().unwrap().contains_key(&name));
        self.ret = Some((name.clone(), ty));
        self.fields.back_mut().unwrap().insert(name, ty);
        self
    }

    pub fn enter_block(&mut self) -> CodeBlock {
        self.fields.push_back(HashMap::new());
        CodeBlock {
            fields: &mut self.fields,
            state: &self.assembler_state,
            stmts: vec![],
            while_state: None,
            if_state: None,
            fn_state: Some(GeneratedFunction {
                name: self.name.clone(),
                params: self.params.clone(),
                body: vec![],
                ret: self.ret.clone(),
            }),
        }
    }
}

pub struct WhileState {
    condition: Expr,
}

pub struct IfElseState {
    condition: Expr,
    then_stmts: Vec<Stmt>,
    in_then: bool,
}

impl IfElseState {
    fn to_else(&mut self, then_stmts: Vec<Stmt>) {
        self.then_stmts = then_stmts;
        self.in_then = false;
    }
}

pub struct CodeBlock<'a> {
    fields: &'a mut VecDeque<HashMap<String, Type>>,
    state: &'a Arc<Mutex<AssemblerState>>,
    stmts: Vec<Stmt>,
    while_state: Option<WhileState>,
    if_state: Option<IfElseState>,
    fn_state: Option<GeneratedFunction>,
}

impl<'a> CodeBlock<'a> {
    pub fn build(&mut self) -> GeneratedFunction {
        assert!(self.fn_state.is_some(), "Calling build on a non function block");
        let mut gen = self.fn_state.take().unwrap();
        gen.body = self.stmts.drain(..).collect::<Vec<_>>();
        gen
    }

    pub fn push(&mut self, stmt: Stmt) {
        self.stmts.push(stmt)
    }

    pub fn leave(&mut self) -> Result<Stmt> {
        self.fields.pop_back();
        if let Some(ref mut while_state) = self.while_state {
            let WhileState { condition } = while_state;
            let stmts = self.stmts.drain(..).collect::<Vec<_>>();
            return Ok(Stmt::WhileLoop(Box::new(condition.clone()), stmts));
        }

        if let Some(ref mut if_state) = self.if_state {
            let IfElseState {
                condition,
                then_stmts,
                in_then,
            } = if_state;
            return if *in_then {
                assert!(then_stmts.is_empty());
                let stmts = self.stmts.drain(..).collect::<Vec<_>>();
                Ok(Stmt::IfElse(Box::new(condition.clone()), stmts, Vec::new()))
            } else {
                assert!(!then_stmts.is_empty());
                let then_stmts = then_stmts.drain(..).collect::<Vec<_>>();
                let else_stmts = self.stmts.drain(..).collect::<Vec<_>>();
                Ok(Stmt::IfElse(
                    Box::new(condition.clone()),
                    then_stmts,
                    else_stmts,
                ))
            };
        }
        unreachable!()
    }

    pub fn enter_else(&mut self) {
        self.fields.pop_back();
        self.fields.push_back(HashMap::new());
        assert!(self.if_state.is_some() && self.if_state.as_ref().unwrap().in_then);
        let new_then = self.stmts.drain(..).collect::<Vec<_>>();
        match self.if_state.iter_mut().next() {
            Some(s) => s.to_else(new_then),
            None => {}
        }
    }

    pub fn declare(&mut self, name: impl Into<String>, ty: Type) -> Result<()> {
        let name = name.into();
        let type_ = self.fields.back().unwrap().get(&name);
        match type_ {
            Some(type_) => err!(
                "Variable {} of {} already exists in the current scope",
                name,
                type_
            ),
            None => {
                self.fields.back_mut().unwrap().insert(name.clone(), ty);
                self.stmts.push(Stmt::Declare(name, ty));
                Ok(())
            }
        }
    }

    fn find_type(&self, name: impl Into<String>) -> Option<Type> {
        let name = name.into();
        for scope in self.fields.iter().rev() {
            let type_ = scope.get(&name);
            match type_ {
                Some(type_) => return Some(*type_),
                None => {}
            }
        }
        None
    }

    pub fn assign(&mut self, name: impl Into<String>, expr: Expr) -> Result<()> {
        let name = name.into();
        let type_ = self.find_type(&name);
        match type_ {
            Some(type_) => {
                if type_ != expr.type_ {
                    err!(
                        "Variable {} of {} cannot be assigned to {}",
                        name,
                        type_,
                        expr.type_
                    )
                } else {
                    self.stmts.push(Stmt::Assign(name, Box::new(expr)));
                    Ok(())
                }
            }
            None => err!("unknown identifier: {}", name),
        }
    }

    pub fn declare_as(&mut self, name: impl Into<String>, expr: Expr) -> Result<()> {
        let name = name.into();
        let type_ = self.fields.back().unwrap().get(&name);
        match type_ {
            Some(type_) => {
                err!(
                    "Variable {} of {} already exists in the current scope",
                    name,
                    type_
                )
            }
            None => {
                self.fields
                    .back_mut()
                    .unwrap()
                    .insert(name.clone(), expr.type_);
                self.stmts.push(Stmt::Declare(name.clone(), expr.type_));
                self.stmts.push(Stmt::Assign(name, Box::new(expr)));
                Ok(())
            }
        }
    }

    pub fn run_expr(&mut self, expr: Expr) -> Result<()> {
        self.stmts.push(Stmt::SideEffect(Box::new(expr)));
        Ok(())
    }

    pub fn while_loop(&mut self, cond: Expr) -> Result<CodeBlock> {
        if cond.type_ != BOOL {
            err!("while condition must be bool")
        } else {
            self.fields.push_back(HashMap::new());
            Ok(CodeBlock {
                fields: &mut self.fields,
                state: &self.state,
                stmts: vec![],
                while_state: Some(WhileState { condition: cond }),
                if_state: None,
                fn_state: None,
            })
        }
    }

    pub fn if_else(&mut self, cond: Expr) -> Result<CodeBlock> {
        if cond.type_ != BOOL {
            err!("if condition must be bool")
        } else {
            self.fields.push_back(HashMap::new());
            Ok(CodeBlock {
                fields: &mut self.fields,
                state: &self.state,
                stmts: vec![],
                while_state: None,
                if_state: Some(IfElseState {
                    condition: cond,
                    then_stmts: vec![],
                    in_then: true,
                }),
                fn_state: None,
            })
        }
    }

    pub fn lit(&self, val: impl Into<String>, ty: Type) -> Expr {
        Expr::new(ExprCode::Literal(val.into()), ty)
    }

    pub fn id(&self, name: impl Into<String>) -> Result<Expr> {
        let name = name.into();
        match self.find_type(&name) {
            None => err!("unknown identifier: {}", name),
            Some(type_) => return Ok(Expr::new(ExprCode::Identifier(name), type_)),
        }
    }

    pub fn equal(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Eq(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn not_equal(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Ne(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn less(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Lt(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn less_equal(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Le(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn greater(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Gt(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn greater_equal(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot compare {} and {}", lhs.type_, rhs.type_)
        } else {
            Ok(Expr::new(ExprCode::Ge(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn add(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot add {} and {}", lhs.type_, rhs.type_)
        } else {
            let type_ = lhs.type_;
            Ok(Expr::new(
                ExprCode::Add(Box::new(lhs), Box::new(rhs)),
                type_,
            ))
        }
    }

    pub fn subtract(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot subtract {} and {}", lhs.type_, rhs.type_)
        } else {
            let type_ = lhs.type_;
            Ok(Expr::new(
                ExprCode::Sub(Box::new(lhs), Box::new(rhs)),
                type_,
            ))
        }
    }

    pub fn multiply(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot multiply {} and {}", lhs.type_, rhs.type_)
        } else {
            let type_ = lhs.type_;
            Ok(Expr::new(
                ExprCode::Mul(Box::new(lhs), Box::new(rhs)),
                type_,
            ))
        }
    }

    pub fn divide(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.type_ != rhs.type_ {
            err!("cannot divide {} and {}", lhs.type_, rhs.type_)
        } else {
            let type_ = lhs.type_;
            Ok(Expr::new(
                ExprCode::Div(Box::new(lhs), Box::new(rhs)),
                type_,
            ))
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

struct ExternFuncSignature {
    name: String,
    code: *const u8,
    params: Vec<Type>,
    returns: Option<Type>,
}
