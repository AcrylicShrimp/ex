use crate::{TypeTable, BUILT_IN_BINARY_OPERATOR, BUILT_IN_UNARY_OPERATOR};
use ex_parser::{ASTBinaryOperatorKind, ASTUnaryOperatorKind, NodeId};
use ex_resolve_ref::TypeKind;
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU64,
};

#[derive(Default, Debug, Clone)]
pub struct TypeTableBuilder {
    next_variable_id: u64,
    pub variables: HashMap<NodeId, TypeVariable>,
    pub variable_constraints: Vec<TypeVariableConstraint>,
    reverse_variables: HashMap<TypeVariable, NodeId>,
}

impl TypeTableBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_variable(&mut self, node: NodeId) -> TypeVariable {
        self.next_variable_id += 1;
        let variable = TypeVariable::new(self.next_variable_id);
        self.variables.insert(node, variable);
        self.reverse_variables.insert(variable, node);
        variable
    }

    pub fn resolve(mut self) -> TypeTable {
        // println!("{:#?}", self.variable_constraints);

        // 1. Collect all equalities.

        let mut equalities = HashMap::<TypeVariable, TypeVariable>::new();

        for constraint in &self.variable_constraints {
            if !constraint.is_equal() {
                continue;
            }

            let variable = &constraint.from;
            let concrete_variable = match &constraint.to {
                TypeVariableConstraintOperand::Variable { variable } => variable,
                _ => {
                    continue;
                }
            };

            if equalities.contains_key(variable) {
                panic!("variable {:?} already has an equality", variable);
            }

            equalities.insert(variable.clone(), concrete_variable.clone());
        }

        // 2. Collect all subtypes.

        let mut subtypes = HashSet::<TypeVariableConstraint>::new();
        let mut as_is_subtypes =
            HashMap::<TypeVariable, HashSet<TypeVariableConstraintOperand>>::new();
        let mut reverse_subtypes = HashMap::<TypeVariable, HashSet<TypeVariable>>::new();

        for constraint in &self.variable_constraints {
            if !constraint.is_subtype() {
                continue;
            }

            subtypes.insert(constraint.clone());
            as_is_subtypes
                .entry(constraint.from.clone())
                .or_default()
                .insert(constraint.to.clone());

            match &constraint.to {
                TypeVariableConstraintOperand::Variable { variable: to } => {
                    reverse_subtypes
                        .entry(to.clone())
                        .or_default()
                        .insert(constraint.from.clone());
                }
                _ => {}
            }
        }

        // 2. Propagate subtypes.

        loop {
            let mut changed = false;

            for (from, to) in &equalities {
                let mut added_subtypes = Vec::new();

                if let Some(subtype_tos) = as_is_subtypes.get(from) {
                    added_subtypes.extend(subtype_tos.iter().map(|subtype_to| {
                        TypeVariableConstraint::subtype(to.clone(), subtype_to.clone())
                    }));
                }

                if let Some(subtype_froms) = reverse_subtypes.get(from) {
                    added_subtypes.extend(subtype_froms.iter().map(|subtype_from| {
                        TypeVariableConstraint::subtype(
                            subtype_from.clone(),
                            TypeVariableConstraintOperand::variable(to.clone()),
                        )
                    }));
                }

                let count = subtypes.len();
                subtypes.extend(added_subtypes.iter().cloned());
                changed |= count != subtypes.len();

                for constraint in added_subtypes {
                    as_is_subtypes
                        .entry(constraint.from.clone())
                        .or_default()
                        .insert(constraint.to.clone());
                    match &constraint.to {
                        TypeVariableConstraintOperand::Variable { variable: to } => {
                            reverse_subtypes
                                .entry(to.clone())
                                .or_default()
                                .insert(constraint.from.clone());
                        }
                        _ => {}
                    }
                }
            }

            if !changed {
                break;
            }
        }

        // 3. Copy subtypes into constraint table.

        self.variable_constraints
            .retain(|constraint| !constraint.is_subtype());
        self.variable_constraints.extend(subtypes);

        // println!("{:#?}", self.variable_constraints);

        let mut type_table = TypeTable::new();

        while !self.variable_constraints.is_empty() {
            // println!("{}", self.variable_constraints.len());
            let mut changed = false;

            self.variable_constraints.retain(|constraint| {
                if !constraint.is_equal() {
                    return true;
                }

                let variable = &constraint.from;
                let type_kind = if let Some(type_kind) =
                    extract_type_kind(&self.reverse_variables, &type_table, &constraint.to)
                {
                    type_kind
                } else {
                    return true;
                };
                changed |= type_table.assign(self.reverse_variables[variable], type_kind);
                false
            });

            // println!("{}", self.variable_constraints.len());

            self.variable_constraints.retain(|constraint| {
                if !constraint.is_subtype() {
                    return true;
                }

                let variable = match &constraint.to {
                    TypeVariableConstraintOperand::Variable { variable } => variable,
                    _ => &constraint.from,
                };

                !type_table
                    .types
                    .contains_key(&self.reverse_variables[variable])
            });

            // println!("{}", self.variable_constraints.len());

            let mut indices = HashSet::new();
            let mut unions = HashMap::<TypeVariable, Vec<TypeKind>>::new();

            for (index, constraint) in self.variable_constraints.iter().enumerate() {
                if !constraint.is_subtype() {
                    continue;
                }

                let from = &constraint.from;

                if self
                    .variable_constraints
                    .iter()
                    .filter(|constraint| constraint.is_equal() && constraint.from.eq(from))
                    .count()
                    == 0
                {
                    if let Some(type_kind) =
                        extract_type_kind(&self.reverse_variables, &type_table, &constraint.to)
                    {
                        unions.entry(from.clone()).or_default().push(type_kind);
                        indices.insert(index);
                    }
                }

                let to = match &constraint.to {
                    TypeVariableConstraintOperand::Variable { variable } => variable,
                    _ => continue,
                };

                if self
                    .variable_constraints
                    .iter()
                    .filter(|constraint| constraint.is_equal() && constraint.from.eq(to))
                    .count()
                    == 0
                {
                    if let Some(type_kind) = type_table
                        .types
                        .get(&self.reverse_variables[&constraint.from])
                    {
                        unions
                            .entry(to.clone())
                            .or_default()
                            .push(type_kind.clone());
                        indices.insert(index);
                    }
                }
            }

            for (variable, mut type_kinds) in unions {
                type_kinds.sort_unstable();
                type_kinds.dedup();

                let type_kind = if type_kinds.len() == 1 {
                    type_kinds[0].clone()
                } else {
                    TypeKind::unknown()
                };
                changed |= type_table.assign(self.reverse_variables[&variable], type_kind);
            }

            let mut index = 0;

            self.variable_constraints.retain(|_| {
                let condition = indices.contains(&index);
                index += 1;
                !condition
            });

            // println!("{}", self.variable_constraints.len());

            // println!("{:#?}", self.variable_constraints);
            // println!("{:#?}", type_table);

            if !changed {
                break;
            }
        }

        type_table.purge_unknown();
        type_table
    }
}

fn extract_type_kind(
    reverse_variables: &HashMap<TypeVariable, NodeId>,
    type_table: &TypeTable,
    operand: &TypeVariableConstraintOperand,
) -> Option<TypeKind> {
    match operand {
        TypeVariableConstraintOperand::Concrete { kind } => Some(kind.clone()),
        TypeVariableConstraintOperand::Variable { variable } => {
            type_table.types.get(&reverse_variables[variable]).cloned()
        }
        TypeVariableConstraintOperand::BinaryOperation {
            operator,
            left,
            right,
        } => {
            match (
                type_table.types.get(&reverse_variables[left]),
                type_table.types.get(&reverse_variables[right]),
            ) {
                (Some(left_type_kind), Some(right_type_kind)) => Some(
                    match BUILT_IN_BINARY_OPERATOR.result_type(
                        operator.clone(),
                        left_type_kind.clone(),
                        right_type_kind.clone(),
                    ) {
                        Some(type_kind) => type_kind.clone(),
                        None => TypeKind::unknown(),
                    },
                ),
                _ => None,
            }
        }
        TypeVariableConstraintOperand::UnaryOperation { operator, right } => {
            match type_table.types.get(&reverse_variables[right]) {
                Some(right_type_kind) => Some(
                    match BUILT_IN_UNARY_OPERATOR
                        .result_type(operator.clone(), right_type_kind.clone())
                    {
                        Some(type_kind) => type_kind.clone(),
                        None => TypeKind::unknown(),
                    },
                ),
                _ => None,
            }
        }
        TypeVariableConstraintOperand::CallableReturnType { variable } => {
            match type_table.types.get(&reverse_variables[variable]) {
                Some(callable_type_kind) => Some(match callable_type_kind {
                    TypeKind::Callable { return_type, .. } => *return_type.clone(),
                    _ => TypeKind::unknown(),
                }),
                _ => None,
            }
        }
        TypeVariableConstraintOperand::CallableParameterType { variable, index } => {
            match type_table.types.get(&reverse_variables[variable]) {
                Some(callable_type_kind) => Some(match callable_type_kind {
                    TypeKind::Callable { parameters, .. } => parameters
                        .get(*index)
                        .cloned()
                        .unwrap_or_else(|| TypeKind::unknown()),
                    _ => TypeKind::unknown(),
                }),
                _ => None,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(NonZeroU64);

impl TypeVariable {
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
    }

    pub fn id(&self) -> u64 {
        self.0.get()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVariableConstraint {
    kind: TypeVariableConstraintKind,
    from: TypeVariable,
    to: TypeVariableConstraintOperand,
}

impl TypeVariableConstraint {
    pub fn equal(from: TypeVariable, to: TypeVariableConstraintOperand) -> Self {
        Self {
            kind: TypeVariableConstraintKind::Equal,
            from,
            to,
        }
    }

    pub fn subtype(from: TypeVariable, to: TypeVariableConstraintOperand) -> Self {
        Self {
            kind: TypeVariableConstraintKind::Subtype,
            from,
            to,
        }
    }

    pub fn is_equal(&self) -> bool {
        self.kind == TypeVariableConstraintKind::Equal
    }

    pub fn is_subtype(&self) -> bool {
        self.kind == TypeVariableConstraintKind::Subtype
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVariableConstraintKind {
    Equal,
    Subtype,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVariableConstraintOperand {
    Concrete {
        kind: TypeKind,
    },
    Variable {
        variable: TypeVariable,
    },
    BinaryOperation {
        operator: ASTBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    },
    UnaryOperation {
        operator: ASTUnaryOperatorKind,
        right: TypeVariable,
    },
    CallableReturnType {
        variable: TypeVariable,
    },
    CallableParameterType {
        variable: TypeVariable,
        index: usize,
    },
}

impl TypeVariableConstraintOperand {
    pub fn concrete(kind: TypeKind) -> Self {
        Self::Concrete { kind }
    }

    pub fn variable(variable: TypeVariable) -> Self {
        Self::Variable { variable }
    }

    pub fn binary_operation(
        operator: ASTBinaryOperatorKind,
        left: TypeVariable,
        right: TypeVariable,
    ) -> Self {
        Self::BinaryOperation {
            operator,
            left,
            right,
        }
    }

    pub fn unary_operation(operator: ASTUnaryOperatorKind, right: TypeVariable) -> Self {
        Self::UnaryOperation { operator, right }
    }

    pub fn callable_return_type(variable: TypeVariable) -> Self {
        Self::CallableReturnType { variable }
    }

    pub fn callable_parameter_type(variable: TypeVariable, index: usize) -> Self {
        Self::CallableParameterType { variable, index }
    }

    pub fn is_concrete(&self) -> bool {
        matches!(self, Self::Concrete { .. })
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }

    pub fn is_binary_operation(&self) -> bool {
        matches!(self, Self::BinaryOperation { .. })
    }

    pub fn is_unary_operation(&self) -> bool {
        matches!(self, Self::UnaryOperation { .. })
    }

    pub fn is_callable_return_type(&self) -> bool {
        matches!(self, Self::CallableReturnType { .. })
    }

    pub fn is_callable_parameter_type(&self) -> bool {
        matches!(self, Self::CallableParameterType { .. })
    }
}
