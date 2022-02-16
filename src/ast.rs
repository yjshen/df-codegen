use cranelift::codegen::ir;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Stmt {
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    WhileLoop(Box<Expr>, Vec<Stmt>),
    Assign(String, Box<Expr>),
    SideEffect(Box<Expr>),
    Declare(String, JITType),
}

#[derive(Copy, Clone, Debug)]
pub enum TypedLit {
    Bool(bool),
    Int(i64),
    Float(f32),
    Double(f64),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(String, JITType),
    Binary(BinaryExpr),
    Call(String, Vec<Expr>, JITType),
}

impl Expr {
    pub fn get_type(&self) -> JITType {
        match self {
            Expr::Literal(lit) => lit.get_type(),
            Expr::Identifier(_, ty) => *ty,
            Expr::Binary(bin) => bin.get_type(),
            Expr::Call(_, _, ty) => *ty,
        }
    }
}

impl Literal {
    fn get_type(&self) -> JITType {
        match self {
            Literal::Parsing(_, ty) => *ty,
            Literal::Typed(tl) => tl.get_type(),
        }
    }
}

impl TypedLit {
    fn get_type(&self) -> JITType {
        match self {
            TypedLit::Bool(_) => BOOL,
            TypedLit::Int(_) => I64,
            TypedLit::Float(_) => F32,
            TypedLit::Double(_) => F64,
        }
    }
}

impl BinaryExpr {
    fn get_type(&self) -> JITType {
        match self {
            BinaryExpr::Eq(_, _) => BOOL,
            BinaryExpr::Ne(_, _) => BOOL,
            BinaryExpr::Lt(_, _) => BOOL,
            BinaryExpr::Le(_, _) => BOOL,
            BinaryExpr::Gt(_, _) => BOOL,
            BinaryExpr::Ge(_, _) => BOOL,
            BinaryExpr::Add(lhs, _) => lhs.get_type(),
            BinaryExpr::Sub(lhs, _) => lhs.get_type(),
            BinaryExpr::Mul(lhs, _) => lhs.get_type(),
            BinaryExpr::Div(lhs, _) => lhs.get_type(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinaryExpr {
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
}

#[derive(Clone, Debug)]
pub enum Literal {
    Parsing(String, JITType),
    Typed(TypedLit),
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
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Identifier(name, _) => write!(f, "{}", name),
            Expr::Binary(be) => write!(f, "{}", be),
            Expr::Call(name, exprs, _) => {
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

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Parsing(str, _) => write!(f, "{}", str),
            Literal::Typed(tl) => write!(f, "{}", tl),
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
            BinaryExpr::Eq(lhs, rhs) => write!(f, "{} == {}", lhs, rhs),
            BinaryExpr::Ne(lhs, rhs) => write!(f, "{} != {}", lhs, rhs),
            BinaryExpr::Lt(lhs, rhs) => write!(f, "{} < {}", lhs, rhs),
            BinaryExpr::Le(lhs, rhs) => write!(f, "{} <= {}", lhs, rhs),
            BinaryExpr::Gt(lhs, rhs) => write!(f, "{} > {}", lhs, rhs),
            BinaryExpr::Ge(lhs, rhs) => write!(f, "{} >= {}", lhs, rhs),
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
