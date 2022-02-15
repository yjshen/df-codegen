use crate::error::{DataFusionError, Result};
use crate::jit::JIT;
use cranelift::codegen::ir;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

struct ExternFuncSignature {
    name: String,
    code: *const u8,
    params: Vec<JITType>,
    returns: Option<JITType>,
}

#[derive(Clone, Debug)]
pub struct GeneratedFunction {
    pub name: String,
    pub params: Vec<(String, JITType)>,
    pub body: Vec<Stmt>,
    pub ret: Option<(String, JITType)>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    WhileLoop(Box<Expr>, Vec<Stmt>),
    Assign(String, Box<Expr>),
    SideEffect(Box<Expr>),
    Declare(String, JITType),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub(crate) code: ExprCode,
    pub(crate) typ: JITType,
}

impl Expr {
    pub fn new(code: ExprCode, typ: JITType) -> Self {
        Self { code, typ }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypedLit {
    Bool(bool),
    Int(i64),
    Float(f32),
    Double(f64),
}

#[derive(Clone, Debug)]
pub enum ExprCode {
    Literal(String),
    TypedLiteral(TypedLit),
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

#[derive(Clone, Debug)]
pub enum NewExpr {
    Literal(NewLiteral),
    Identifier(String, JITType),
    Binary(BinaryExpr),
    Call(String, Vec<NewExpr>, JITType),
}

#[derive(Clone, Debug)]
pub enum BinaryExpr {
    Eq(Box<NewExpr>, Box<NewExpr>, JITType),
    Ne(Box<NewExpr>, Box<NewExpr>, JITType),
    Lt(Box<NewExpr>, Box<NewExpr>, JITType),
    Le(Box<NewExpr>, Box<NewExpr>, JITType),
    Gt(Box<NewExpr>, Box<NewExpr>, JITType),
    Ge(Box<NewExpr>, Box<NewExpr>, JITType),
    Add(Box<NewExpr>, Box<NewExpr>),
    Sub(Box<NewExpr>, Box<NewExpr>),
    Mul(Box<NewExpr>, Box<NewExpr>),
    Div(Box<NewExpr>, Box<NewExpr>),
}

#[derive(Clone, Debug)]
pub enum NewLiteral {
    Parsing(String, JITType),
    Typed(TypedLit, JITType),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct JITType {
    pub(crate) native: ir::Type,
    /// re-expose inner field of `ir::Type` out for easier pattern matching
    pub(crate) code: u8,
}

pub const NIL: JITType = JITType {
    native: ir::types::INVALID,
    code: 0,
};
pub const BOOL: JITType = JITType {
    native: ir::types::B1,
    code: 0x70,
};
pub const I8: JITType = JITType {
    native: ir::types::I8,
    code: 0x76,
};
pub const I16: JITType = JITType {
    native: ir::types::I16,
    code: 0x77,
};
pub const I32: JITType = JITType {
    native: ir::types::I32,
    code: 0x78,
};
pub const I64: JITType = JITType {
    native: ir::types::I64,
    code: 0x79,
};
pub const F32: JITType = JITType {
    native: ir::types::F32,
    code: 0x7b,
};
pub const F64: JITType = JITType {
    native: ir::types::F64,
    code: 0x7c,
};
pub const R32: JITType = JITType {
    native: ir::types::R32,
    code: 0x7e,
};
pub const R64: JITType = JITType {
    native: ir::types::R64,
    code: 0x7f,
};

#[derive(Default)]
pub struct AssemblerState {
    name_next_id: HashMap<String, u8>,
    extern_funcs: HashMap<String, ExternFuncSignature>,
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
        params: Vec<JITType>,
        returns: Option<JITType>,
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

    pub fn create_jit(&self) -> JIT {
        let symbols = self
            .state
            .lock()
            .extern_funcs
            .values()
            .map(|s| (s.name.clone(), s.code))
            .collect::<Vec<_>>();
        JIT::new(symbols)
    }
}

pub struct FunctionBuilder {
    pub name: String,
    pub params: Vec<(String, JITType)>,
    pub body: Vec<Stmt>,
    pub ret: Option<(String, JITType)>,
    fields: VecDeque<HashMap<String, JITType>>,
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

    pub fn param(mut self, name: impl Into<String>, ty: JITType) -> Self {
        let name = name.into();
        assert!(!self.fields.back().unwrap().contains_key(&name));
        self.params.push((name.clone(), ty));
        self.fields.back_mut().unwrap().insert(name, ty);
        self
    }

    pub fn ret(mut self, name: impl Into<String>, ty: JITType) -> Self {
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
    fn enter_else(&mut self, then_stmts: Vec<Stmt>) {
        self.then_stmts = then_stmts;
        self.in_then = false;
    }
}

pub struct CodeBlock<'a> {
    fields: &'a mut VecDeque<HashMap<String, JITType>>,
    state: &'a Arc<Mutex<AssemblerState>>,
    stmts: Vec<Stmt>,
    while_state: Option<WhileState>,
    if_state: Option<IfElseState>,
    fn_state: Option<GeneratedFunction>,
}

impl<'a> CodeBlock<'a> {
    pub fn build(&mut self) -> GeneratedFunction {
        assert!(
            self.fn_state.is_some(),
            "Calling build on a non function block"
        );
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
        if let Some(s) = self.if_state.iter_mut().next() {
            s.enter_else(new_then)
        }
    }

    pub fn declare(&mut self, name: impl Into<String>, ty: JITType) -> Result<()> {
        let name = name.into();
        let typ = self.fields.back().unwrap().get(&name);
        match typ {
            Some(typ) => err!(
                "Variable {} of {} already exists in the current scope",
                name,
                typ
            ),
            None => {
                self.fields.back_mut().unwrap().insert(name.clone(), ty);
                self.stmts.push(Stmt::Declare(name, ty));
                Ok(())
            }
        }
    }

    fn find_type(&self, name: impl Into<String>) -> Option<JITType> {
        let name = name.into();
        for scope in self.fields.iter().rev() {
            let typ = scope.get(&name);
            if let Some(typ) = typ {
                return Some(*typ);
            }
        }
        None
    }

    pub fn assign(&mut self, name: impl Into<String>, expr: Expr) -> Result<()> {
        let name = name.into();
        let typ = self.find_type(&name);
        match typ {
            Some(typ) => {
                if typ != expr.typ {
                    err!(
                        "Variable {} of {} cannot be assigned to {}",
                        name,
                        typ,
                        expr.typ
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
        let typ = self.fields.back().unwrap().get(&name);
        match typ {
            Some(typ) => {
                err!(
                    "Variable {} of {} already exists in the current scope",
                    name,
                    typ
                )
            }
            None => {
                self.fields
                    .back_mut()
                    .unwrap()
                    .insert(name.clone(), expr.typ);
                self.stmts.push(Stmt::Declare(name.clone(), expr.typ));
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
        if cond.typ != BOOL {
            err!("while condition must be bool")
        } else {
            self.fields.push_back(HashMap::new());
            Ok(CodeBlock {
                fields: self.fields,
                state: self.state,
                stmts: vec![],
                while_state: Some(WhileState { condition: cond }),
                if_state: None,
                fn_state: None,
            })
        }
    }

    pub fn if_else(&mut self, cond: Expr) -> Result<CodeBlock> {
        if cond.typ != BOOL {
            err!("if condition must be bool")
        } else {
            self.fields.push_back(HashMap::new());
            Ok(CodeBlock {
                fields: self.fields,
                state: self.state,
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

    pub fn if_block<C, T, E>(&mut self, mut cond: C, mut then_blk: T, mut else_blk: E) -> Result<()>
    where
        C: FnMut(&mut CodeBlock) -> Result<Expr>,
        T: FnMut(&mut CodeBlock) -> Result<()>,
        E: FnMut(&mut CodeBlock) -> Result<()>,
    {
        let cond = cond(self)?;
        let mut body = self.if_else(cond)?;
        then_blk(&mut body)?;
        body.enter_else();
        else_blk(&mut body)?;
        let if_else = body.leave()?;
        self.stmts.push(if_else);
        Ok(())
    }

    pub fn while_block<C, B>(&mut self, mut cond: C, mut body_blk: B) -> Result<()>
    where
        C: FnMut(&mut CodeBlock) -> Result<Expr>,
        B: FnMut(&mut CodeBlock) -> Result<()>,
    {
        let cond = cond(self)?;
        let mut body = self.while_loop(cond)?;
        body_blk(&mut body)?;
        let while_stmt = body.leave()?;
        self.stmts.push(while_stmt);
        Ok(())
    }

    pub fn lit(&self, val: impl Into<String>, ty: JITType) -> Expr {
        Expr::new(ExprCode::Literal(val.into()), ty)
    }

    pub fn lit_i(&self, val: impl Into<i64>) -> Expr {
        Expr::new(ExprCode::TypedLiteral(TypedLit::Int(val.into())), I64)
    }

    pub fn lit_f(&self, val: f32) -> Expr {
        Expr::new(ExprCode::TypedLiteral(TypedLit::Float(val)), F32)
    }

    pub fn lit_d(&self, val: f64) -> Expr {
        Expr::new(ExprCode::TypedLiteral(TypedLit::Double(val)), F64)
    }

    pub fn lit_b(&self, val: bool) -> Expr {
        Expr::new(ExprCode::TypedLiteral(TypedLit::Bool(val)), BOOL)
    }

    pub fn id(&self, name: impl Into<String>) -> Result<Expr> {
        let name = name.into();
        match self.find_type(&name) {
            None => err!("unknown identifier: {}", name),
            Some(typ) => Ok(Expr::new(ExprCode::Identifier(name), typ)),
        }
    }

    pub fn eq(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Eq(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn ne(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Ne(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn lt(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Lt(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn le(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Le(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn gt(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Gt(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn ge(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot compare {} and {}", lhs.typ, rhs.typ)
        } else {
            Ok(Expr::new(ExprCode::Ge(Box::new(lhs), Box::new(rhs)), BOOL))
        }
    }

    pub fn add(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot add {} and {}", lhs.typ, rhs.typ)
        } else {
            let typ = lhs.typ;
            Ok(Expr::new(ExprCode::Add(Box::new(lhs), Box::new(rhs)), typ))
        }
    }

    pub fn sub(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot subtract {} and {}", lhs.typ, rhs.typ)
        } else {
            let typ = lhs.typ;
            Ok(Expr::new(ExprCode::Sub(Box::new(lhs), Box::new(rhs)), typ))
        }
    }

    pub fn mul(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot multiply {} and {}", lhs.typ, rhs.typ)
        } else {
            let typ = lhs.typ;
            Ok(Expr::new(ExprCode::Mul(Box::new(lhs), Box::new(rhs)), typ))
        }
    }

    pub fn div(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.typ != rhs.typ {
            err!("cannot divide {} and {}", lhs.typ, rhs.typ)
        } else {
            let typ = lhs.typ;
            Ok(Expr::new(ExprCode::Div(Box::new(lhs), Box::new(rhs)), typ))
        }
    }

    pub fn call(&self, name: impl Into<String>, params: Vec<Expr>) -> Result<Expr> {
        let fn_name = name.into();
        if let Some(func) = self.state.lock().extern_funcs.get(&fn_name) {
            for ((i, t1), t2) in params.iter().enumerate().zip(func.params.iter()) {
                if t1.typ != *t2 {
                    return err!("Func {} need {} as arg{}, get {}", &fn_name, t2, i, t1.typ);
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

impl Display for GeneratedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, (name, ty)) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, ty)?;
        }
        write!(f, ") -> ")?;
        if let Some((name, ty)) = &self.ret {
            write!(f, "{}: {}", name, ty)?;
        } else {
            write!(f, "()")?;
        }
        writeln!(f, " {{")?;
        for stmt in &self.body {
            stmt.fmt_ident(4, f)?;
        }
        write!(f, "}}")
    }
}

impl Stmt {
    /// print the statement with indentation
    pub fn fmt_ident(&self, ident: usize, f: &mut Formatter) -> std::fmt::Result {
        let mut ident_str = String::new();
        for _ in 0..ident {
            ident_str.push(' ');
        }
        match self {
            Stmt::IfElse(cond, then_stmts, else_stmts) => {
                writeln!(f, "{}if {} {{", ident_str, cond)?;
                for stmt in then_stmts {
                    stmt.fmt_ident(ident + 4, f)?;
                }
                writeln!(f, "{}}} else {{", ident_str)?;
                for stmt in else_stmts {
                    stmt.fmt_ident(ident + 4, f)?;
                }
                writeln!(f, "{}}}", ident_str)
            }
            Stmt::WhileLoop(cond, stmts) => {
                writeln!(f, "{}while {} {{", ident_str, cond)?;
                for stmt in stmts {
                    stmt.fmt_ident(ident + 4, f)?;
                }
                writeln!(f, "{}}}", ident_str)
            }
            Stmt::Assign(name, expr) => {
                writeln!(f, "{}{} = {};", ident_str, name, expr)
            }
            Stmt::SideEffect(expr) => {
                writeln!(f, "{}{};", ident_str, expr)
            }
            Stmt::Declare(name, ty) => {
                writeln!(f, "{}let {}: {};", ident_str, name, ty)
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_ident(0, f)?;
        Ok(())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.code {
            ExprCode::Literal(lit) => write!(f, "{}", lit),
            ExprCode::TypedLiteral(tl) => write!(f, "{}", tl),
            ExprCode::Identifier(name) => write!(f, "{}", name),
            ExprCode::Eq(lhs, rhs) => write!(f, "{} == {}", lhs, rhs),
            ExprCode::Ne(lhs, rhs) => write!(f, "{} != {}", lhs, rhs),
            ExprCode::Lt(lhs, rhs) => write!(f, "{} < {}", lhs, rhs),
            ExprCode::Le(lhs, rhs) => write!(f, "{} <= {}", lhs, rhs),
            ExprCode::Gt(lhs, rhs) => write!(f, "{} > {}", lhs, rhs),
            ExprCode::Ge(lhs, rhs) => write!(f, "{} >= {}", lhs, rhs),
            ExprCode::Add(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            ExprCode::Sub(lhs, rhs) => write!(f, "{} - {}", lhs, rhs),
            ExprCode::Mul(lhs, rhs) => write!(f, "{} * {}", lhs, rhs),
            ExprCode::Div(lhs, rhs) => write!(f, "{} / {}", lhs, rhs),
            ExprCode::Call(name, exprs) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Display for NewExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NewExpr::Literal(l) => write!(f, "{}", l),
            NewExpr::Identifier(name, _) => write!(f, "{}", name),
            NewExpr::Binary(be) => write!(f, "{}", be),
            NewExpr::Call(name, exprs, _) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Display for NewLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NewLiteral::Parsing(str, _) => write!(f, "{}", str),
            NewLiteral::Typed(tl, _) => write!(f, "{}", tl),
        }
    }
}

impl Display for TypedLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedLit::Bool(b) => write!(f, "{}", b),
            TypedLit::Int(i) => write!(f, "{}", i),
            TypedLit::Float(fl) => write!(f, "{}", fl),
            TypedLit::Double(d) => write!(f, "{}", d),
        }
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryExpr::Eq(lhs, rhs, _) => write!(f, "{} == {}", lhs, rhs),
            BinaryExpr::Ne(lhs, rhs, _) => write!(f, "{} != {}", lhs, rhs),
            BinaryExpr::Lt(lhs, rhs, _) => write!(f, "{} < {}", lhs, rhs),
            BinaryExpr::Le(lhs, rhs, _) => write!(f, "{} <= {}", lhs, rhs),
            BinaryExpr::Gt(lhs, rhs, _) => write!(f, "{} > {}", lhs, rhs),
            BinaryExpr::Ge(lhs, rhs, _) => write!(f, "{} >= {}", lhs, rhs),
            BinaryExpr::Add(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            BinaryExpr::Sub(lhs, rhs) => write!(f, "{} - {}", lhs, rhs),
            BinaryExpr::Mul(lhs, rhs) => write!(f, "{} * {}", lhs, rhs),
            BinaryExpr::Div(lhs, rhs) => write!(f, "{} / {}", lhs, rhs),
        }
    }
}

impl std::fmt::Display for JITType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Debug for JITType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.code {
            0 => write!(f, "nil"),
            0x70 => write!(f, "bool"),
            0x76 => write!(f, "i8"),
            0x77 => write!(f, "i16"),
            0x78 => write!(f, "i32"),
            0x79 => write!(f, "i64"),
            0x7b => write!(f, "f32"),
            0x7c => write!(f, "f64"),
            0x7e => write!(f, "small_ptr"),
            0x7f => write!(f, "ptr"),
            _ => write!(f, "unknown"),
        }
    }
}

impl From<&str> for JITType {
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
