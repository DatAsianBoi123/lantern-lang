use std::{cell::RefCell, collections::{hash_map::Entry, HashMap}, rc::Rc};

use crate::{error::{MismatchedTypes, RuntimeError, UnknownItem}, record::LanternRecordFrame, LanternFunction, LanternValue, LanternVariable, ScopeMut};

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeContext {
    functions: HashMap<String, LanternFunction>,
    variables: HashMap<String, LanternVariable>,
    // Rc to avoid cloning large record frames
    records: HashMap<String, Rc<LanternRecordFrame>>,
}

impl Default for RuntimeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl RuntimeContext {
    pub fn new() -> Self {
        Self { functions: Default::default(), variables: Default::default(), records: Default::default() }
    }

    pub fn add_function(&mut self, function: LanternFunction) -> &mut Self {
        self.functions.insert(function.name.clone(), function);
        self
    }

    pub fn function(&self, name: &str) -> Option<&LanternFunction> {
        self.functions.get(name)
    }

    pub fn add_variable(&mut self, variable: LanternVariable) -> &mut Self {
        self.variables.insert(variable.name.clone(), variable);
        self
    }

    pub fn variable(&self, name: &str) -> Option<&LanternVariable> {
        self.variables.get(name)
    }

    pub fn add_record(&mut self, record: Rc<LanternRecordFrame>) -> &mut Self {
        self.records.insert(record.ident.name.clone(), record);
        self
    }

    pub fn record(&self, name: &str) -> Option<Rc<LanternRecordFrame>> {
        self.records.get(name).cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Context {
        parent: ScopeMut,
        context: RuntimeContext,
    },
    Head,
}

impl Scope {
    pub fn new(context: RuntimeContext) -> Self {
        Scope::Context {
            parent: Rc::new(RefCell::new(Scope::Head)),
            context,
        }
    }

    pub fn nested(parent: ScopeMut, context: RuntimeContext) -> Self {
        Self::Context {
            parent,
            context,
        }
    }

    pub fn function(&self, name: &str) -> Option<LanternFunction> {
        if let Self::Context { parent, context } = self {
            context.function(name)
                .cloned()
                .or_else(|| parent.borrow().function(name))
        } else { None }
    }

    pub fn add_function(&mut self, function: LanternFunction) {
        if let Self::Context { context, .. } = self {
            context.add_function(function);
        } else { panic!("can't add function to Head scope") }
    }

    pub fn variable(&self, name: &str) -> Option<LanternVariable> {
        if let Self::Context { parent, context } = self {
            context.variable(name)
                .cloned()
                .or_else(|| parent.borrow().variable(name))
        } else { None }
    }

    pub fn add_variable(&mut self, value: LanternVariable) {
        if let Self::Context { context, .. } = self {
            context.add_variable(value);
        } else { panic!("can't add variable to Head scope") }
    }

    pub fn record(&self, name: &str) -> Option<Rc<LanternRecordFrame>> {
        if let Self::Context { parent, context } = self {
            context.record(name)
                .or_else(|| parent.borrow().record(name))
        } else { None }
    }

    pub fn add_record(&mut self, record: Rc<LanternRecordFrame>) {
        if let Self::Context { context, .. } = self {
            context.add_record(record);
        } else { panic!("can't add function to Head scope") }
    }

    pub fn reassign_variable(&mut self, name: String, value: LanternValue) -> Result<(), RuntimeError> {
        if let Self::Context { parent, context } = self {
            match context.variables.entry(name.clone()) {
                Entry::Occupied(mut entry) => {
                    let expected_type = &entry.get().r#type;
                    if !expected_type.applies_to(&value.r#type()) {
                        return Err(RuntimeError::new(MismatchedTypes(expected_type.clone(), value.r#type())));
                    };

                    entry.get_mut().value = value;
                    Ok(())
                },
                Entry::Vacant(_) => parent.borrow_mut().reassign_variable(name, value),
            }
        } else {
            Err(RuntimeError::new(UnknownItem::Variable))
        }
    }
}

