// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

//! DataFusion error types

use arrow::error::ArrowError;
use cranelift_module::ModuleError;
use std::error;
use std::fmt::{Display, Formatter};
use std::io;
use std::result;

/// Result type for operations that could result in an [DataFusionError]
pub type Result<T> = result::Result<T, DataFusionError>;

/// Error type for generic operations that could result in DataFusionError::External
pub type GenericError = Box<dyn error::Error + Send + Sync>;

/// DataFusion error
#[derive(Debug)]
#[allow(missing_docs)]
pub enum DataFusionError {
    /// Error returned by arrow.
    ArrowError(ArrowError),
    /// Error associated to I/O operations and associated traits.
    IoError(io::Error),
    /// Error returned on a branch that we know it is possible
    /// but to which we still have no implementation for.
    /// Often, these errors are tracked in our issue tracker.
    NotImplemented(String),
    /// Error returned as a consequence of an error in DataFusion.
    /// This error should not happen in normal usage of DataFusion.
    // DataFusions has internal invariants that we are unable to ask the compiler to check for us.
    // This error is raised when one of those invariants is not verified during execution.
    Internal(String),
    /// This error happens whenever a plan is not valid. Examples include
    /// impossible casts, schema inference not possible and non-unique column names.
    Plan(String),
    /// Error returned during execution of the query.
    /// Examples include files not found, errors in parsing certain types.
    Execution(String),
    /// This error is thrown when a consumer cannot acquire memory from the Memory Manager
    /// we can just cancel the execution of the partition.
    ResourcesExhausted(String),
    /// Errors originating from outside DataFusion's core codebase.
    /// For example, a custom S3Error from the crate datafusion-objectstore-s3
    External(GenericError),
    /// Error occurs during code generation
    JITError(ModuleError),
}

impl From<ArrowError> for DataFusionError {
    fn from(e: ArrowError) -> Self {
        DataFusionError::ArrowError(e)
    }
}

impl From<DataFusionError> for ArrowError {
    fn from(e: DataFusionError) -> Self {
        match e {
            DataFusionError::ArrowError(e) => e,
            DataFusionError::External(e) => ArrowError::ExternalError(e),
            other => ArrowError::ExternalError(Box::new(other)),
        }
    }
}

impl From<io::Error> for DataFusionError {
    fn from(e: io::Error) -> Self {
        DataFusionError::IoError(e)
    }
}

impl From<ModuleError> for DataFusionError {
    fn from(e: ModuleError) -> Self {
        DataFusionError::JITError(e)
    }
}

impl From<GenericError> for DataFusionError {
    fn from(err: GenericError) -> Self {
        DataFusionError::External(err)
    }
}

impl Display for DataFusionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match *self {
            DataFusionError::ArrowError(ref desc) => write!(f, "Arrow error: {}", desc),
            DataFusionError::IoError(ref desc) => write!(f, "IO error: {}", desc),
            DataFusionError::NotImplemented(ref desc) => {
                write!(f, "This feature is not implemented: {}", desc)
            }
            DataFusionError::Internal(ref desc) => {
                write!(
                    f,
                    "Internal error: {}. This was likely caused by a bug in DataFusion's \
                    code and we would welcome that you file an bug report in our issue tracker",
                    desc
                )
            }
            DataFusionError::Plan(ref desc) => {
                write!(f, "Error during planning: {}", desc)
            }
            DataFusionError::Execution(ref desc) => {
                write!(f, "Execution error: {}", desc)
            }
            DataFusionError::ResourcesExhausted(ref desc) => {
                write!(f, "Resources exhausted: {}", desc)
            }
            DataFusionError::External(ref desc) => {
                write!(f, "External error: {}", desc)
            }
            DataFusionError::JITError(ref desc) => {
                write!(f, "JIT error: {:?}", desc)
            }
        }
    }
}

impl error::Error for DataFusionError {}

#[macro_export]
macro_rules! internal_err {
    ($($arg:tt)*) => {
        Err(DataFusionError::Internal(format!($($arg)*)))
    };
}
