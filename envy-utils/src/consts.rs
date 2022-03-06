#![allow(clippy::float_cmp)]

use crate::{clip, is_direct_expn_of, sext, unsext};
use if_chain::if_chain;
use rustc_ast::ast::{self, LitFloatType, LitKind};
use rustc_data_structures::sync::Lrc;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::{BinOp, BinOpKind, Block, Expr, ExprKind, HirId, QPath, UnOp};
use rustc_lint::LateContext;
use rustc_middle::mir::interpret::Scalar;
use rustc_middle::ty::subst::{Subst, SubstsRef};
use rustc_middle::ty::{self, FloatTy, ScalarInt, Ty, TyCtxt};
use rustc_middle::{bug, span_bug};
use rustc_span::symbol::Symbol;
use std::cmp::Ordering::{self, Equal};
use std::convert::TryInto;
use std::hash::{Hash, Hasher};
use std::iter;

/// A `LitKind`-like enum to fold constant `Expr`s into.
#[derive(Debug, Clone)]
pub enum Constant {
    /// A `String` (e.g., "abc").
    Str(String),
    /// A binary string (e.g., `b"abc"`).
    Binary(Lrc<[u8]>),
    /// A single `char` (e.g., `'a'`).
    Char(char),
    /// An integer's bit representation.
    Int(u128),
    /// An `f32`.
    F32(f32),
    /// An `f64`.
    F64(f64),
    /// `true` or `false`.
    Bool(bool),
    /// An array of constants.
    Vec(Vec<Constant>),
    /// Also an array, but with only one constant, repeated N times.
    Repeat(Box<Constant>, u64),
    /// A tuple of constants.
    Tuple(Vec<Constant>),
    /// A raw pointer.
    RawPtr(u128),
    /// A reference
    Ref(Box<Constant>),
    /// A literal with syntax error.
    Err(Symbol),
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Self::Str(ref ls), &Self::Str(ref rs)) => ls == rs,
            (&Self::Binary(ref l), &Self::Binary(ref r)) => l == r,
            (&Self::Char(l), &Self::Char(r)) => l == r,
            (&Self::Int(l), &Self::Int(r)) => l == r,
            (&Self::F64(l), &Self::F64(r)) => {
                // We want `Fw32 == FwAny` and `FwAny == Fw64`, and by transitivity we must have
                // `Fw32 == Fw64`, so don’t compare them.
                // `to_bits` is required to catch non-matching 0.0, -0.0, and NaNs.
                l.to_bits() == r.to_bits()
            },
            (&Self::F32(l), &Self::F32(r)) => {
                // We want `Fw32 == FwAny` and `FwAny == Fw64`, and by transitivity we must have
                // `Fw32 == Fw64`, so don’t compare them.
                // `to_bits` is required to catch non-matching 0.0, -0.0, and NaNs.
                f64::from(l).to_bits() == f64::from(r).to_bits()
            },
            (&Self::Bool(l), &Self::Bool(r)) => l == r,
            (&Self::Vec(ref l), &Self::Vec(ref r)) | (&Self::Tuple(ref l), &Self::Tuple(ref r)) => l == r,
            (&Self::Repeat(ref lv, ref ls), &Self::Repeat(ref rv, ref rs)) => ls == rs && lv == rv,
            (&Self::Ref(ref lb), &Self::Ref(ref rb)) => *lb == *rb,
            // TODO: are there inter-type equalities?
            _ => false,
        }
    }
}

impl Hash for Constant {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        std::mem::discriminant(self).hash(state);
        match *self {
            Self::Str(ref s) => {
                s.hash(state);
            },
            Self::Binary(ref b) => {
                b.hash(state);
            },
            Self::Char(c) => {
                c.hash(state);
            },
            Self::Int(i) => {
                i.hash(state);
            },
            Self::F32(f) => {
                f64::from(f).to_bits().hash(state);
            },
            Self::F64(f) => {
                f.to_bits().hash(state);
            },
            Self::Bool(b) => {
                b.hash(state);
            },
            Self::Vec(ref v) | Self::Tuple(ref v) => {
                v.hash(state);
            },
            Self::Repeat(ref c, l) => {
                c.hash(state);
                l.hash(state);
            },
            Self::RawPtr(u) => {
                u.hash(state);
            },
            Self::Ref(ref r) => {
                r.hash(state);
            },
            Self::Err(ref s) => {
                s.hash(state);
            },
        }
    }
}
