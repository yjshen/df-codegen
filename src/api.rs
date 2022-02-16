use crate::ast::*;
use crate::error::{DataFusionError, Result};
use crate::jit::JIT;
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
                if typ != expr.get_type() {
                    err!(
                        "Variable {} of {} cannot be assigned to {}",
                        name,
                        typ,
                        expr.get_type()
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
                    .insert(name.clone(), expr.get_type());
                self.stmts
                    .push(Stmt::Declare(name.clone(), expr.get_type()));
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
        if cond.get_type() != BOOL {
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
        if cond.get_type() != BOOL {
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
        Expr::Literal(Literal::Parsing(val.into(), ty))
    }

    pub fn lit_i(&self, val: impl Into<i64>) -> Expr {
        Expr::Literal(Literal::Typed(TypedLit::Int(val.into())))
    }

    pub fn lit_f(&self, val: f32) -> Expr {
        Expr::Literal(Literal::Typed(TypedLit::Float(val)))
    }

    pub fn lit_d(&self, val: f64) -> Expr {
        Expr::Literal(Literal::Typed(TypedLit::Double(val)))
    }

    pub fn lit_b(&self, val: bool) -> Expr {
        Expr::Literal(Literal::Typed(TypedLit::Bool(val)))
    }

    pub fn id(&self, name: impl Into<String>) -> Result<Expr> {
        let name = name.into();
        match self.find_type(&name) {
            None => err!("unknown identifier: {}", name),
            Some(typ) => Ok(Expr::Identifier(name, typ)),
        }
    }

    pub fn eq(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Eq(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn ne(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Ne(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn lt(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Lt(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn le(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Le(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn gt(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Gt(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn ge(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot compare {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Ge(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn add(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot add {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Add(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn sub(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot subtract {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Sub(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn mul(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot multiply {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Mul(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn div(&self, lhs: Expr, rhs: Expr) -> Result<Expr> {
        if lhs.get_type() != rhs.get_type() {
            err!("cannot divide {} and {}", lhs.get_type(), rhs.get_type())
        } else {
            Ok(Expr::Binary(BinaryExpr::Div(Box::new(lhs), Box::new(rhs))))
        }
    }

    pub fn call(&self, name: impl Into<String>, params: Vec<Expr>) -> Result<Expr> {
        let fn_name = name.into();
        if let Some(func) = self.state.lock().extern_funcs.get(&fn_name) {
            for ((i, t1), t2) in params.iter().enumerate().zip(func.params.iter()) {
                if t1.get_type() != *t2 {
                    return err!(
                        "Func {} need {} as arg{}, get {}",
                        &fn_name,
                        t2,
                        i,
                        t1.get_type()
                    );
                }
            }
            Ok(Expr::Call(fn_name, params, func.returns.unwrap_or(NIL)))
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
