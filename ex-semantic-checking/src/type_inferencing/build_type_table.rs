use crate::{
    hir::HIRUnaryOperatorKind,
    resolve::{TopLevelTable, TypeKind},
    type_inferencing::{
        ReverseTypeVariableTable, TypeConstraint, TypeConstraintTable, TypeConstraintTarget,
        TypeVariable, BUILT_IN_BINARY_OPERATOR, BUILT_IN_UNARY_OPERATOR,
    },
};
use ex_parser::NodeId;
use std::collections::{HashMap, HashSet};

#[derive(Default, Debug, Clone)]
pub struct TypeTable {
    pub types: HashMap<NodeId, TypeKind>,
}

impl TypeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn assign(&mut self, node: NodeId, mut type_kind: TypeKind) -> bool {
        if let Some(old_type_kind) = self.types.get(&node) {
            if old_type_kind.eq(&type_kind) {
                return false;
            }
            type_kind = TypeKind::unknown();
        }
        self.types.insert(node, type_kind);
        true
    }

    pub fn purge_unknown(&mut self) {
        self.types.retain(|_, type_kind| !type_kind.is_unknown());
    }
}

pub fn build_type_table(
    top_level_table: &TopLevelTable,
    constraint_table: TypeConstraintTable,
) -> TypeTable {
    let reverse_table = constraint_table.reverse_type_variables;
    let mut constraints = constraint_table.type_constraints;

    // 1. Collect all equalities.

    let mut equalities = HashMap::<TypeVariable, TypeVariable>::new();

    for constraint in &constraints {
        if !constraint.is_equal() {
            continue;
        }

        let from = &constraint.from;
        let to = match &constraint.to {
            TypeConstraintTarget::Variable { variable } => variable,
            _ => {
                continue;
            }
        };

        if equalities.contains_key(from) {
            panic!("variable {:?} already has an equality", from);
        }

        equalities.insert(*from, *to);
    }

    // 2. Collect all subtypes.

    let mut subtypes = HashSet::<TypeConstraint>::new();
    let mut as_is_subtypes = HashMap::<TypeVariable, HashSet<TypeConstraintTarget>>::new();
    let mut reverse_subtypes = HashMap::<TypeVariable, HashSet<TypeVariable>>::new();

    for constraint in &constraints {
        if !constraint.is_subtype() {
            continue;
        }

        subtypes.insert(constraint.clone());
        as_is_subtypes
            .entry(constraint.from.clone())
            .or_default()
            .insert(constraint.to.clone());

        match &constraint.to {
            TypeConstraintTarget::Variable { variable: to } => {
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
                added_subtypes.extend(
                    subtype_tos
                        .iter()
                        .map(|subtype_to| TypeConstraint::subtype(to.clone(), subtype_to.clone())),
                );
            }

            if let Some(subtype_froms) = reverse_subtypes.get(from) {
                added_subtypes.extend(subtype_froms.iter().map(|subtype_from| {
                    TypeConstraint::subtype(
                        subtype_from.clone(),
                        TypeConstraintTarget::variable(to.clone()),
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
                    TypeConstraintTarget::Variable { variable: to } => {
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

    constraints.retain(|constraint| !constraint.is_subtype());
    constraints.extend(subtypes);

    // 4. Solve graph.

    let mut type_table = TypeTable::new();

    while !constraints.is_empty() {
        let mut changed = false;

        // Solve equalities.
        constraints.retain(|constraint| {
            if !constraint.is_equal() {
                return true;
            }

            let variable = &constraint.from;
            let type_kind = if let Some(type_kind) =
                extract_type_kind(top_level_table, &type_table, &reverse_table, &constraint.to)
            {
                type_kind
            } else {
                return true;
            };
            changed |= type_table.assign(reverse_table[variable], type_kind);
            false
        });

        // Purge subtypes of already solved variables.
        constraints.retain(|constraint| {
            if !constraint.is_subtype() {
                return true;
            }

            let variable = match &constraint.to {
                TypeConstraintTarget::Variable { variable } => variable,
                _ => &constraint.from,
            };

            !type_table
                .types
                .contains_key(&reverse_table[&constraint.from])
                || !type_table.types.contains_key(&reverse_table[variable])
        });

        let mut indices = HashSet::new();
        let mut unions = HashMap::<TypeVariable, Vec<TypeKind>>::new();

        // Unifies subtypes of variables that have no equalities.
        for (index, constraint) in constraints.iter().enumerate() {
            if !constraint.is_subtype() {
                continue;
            }

            let from = &constraint.from;
            let from_equality_count = constraints
                .iter()
                .filter(|constraint| constraint.is_equal() && constraint.from.eq(from))
                .count();

            if from_equality_count == 0 {
                if let Some(type_kind) =
                    extract_type_kind(top_level_table, &type_table, &reverse_table, &constraint.to)
                {
                    unions.entry(*from).or_default().push(type_kind);
                    indices.insert(index);
                }
            }

            let to = match &constraint.to {
                TypeConstraintTarget::Variable { variable } => variable,
                _ => continue,
            };
            let to_equality_count = constraints
                .iter()
                .filter(|constraint| constraint.is_equal() && constraint.from.eq(to))
                .count();

            if to_equality_count == 0 {
                if let Some(type_kind) = type_table.types.get(&reverse_table[&constraint.from]) {
                    unions.entry(*to).or_default().push(type_kind.clone());
                    indices.insert(index);
                }
            }
        }

        // Solve unions.
        for (variable, mut type_kinds) in unions {
            type_kinds.sort_unstable();
            type_kinds.dedup();

            let type_kind = if type_kinds.len() == 1 {
                type_kinds[0].clone()
            } else {
                // TODO: Unifies type kinds as general as possible.
                TypeKind::unknown()
            };
            changed |= type_table.assign(reverse_table[&variable], type_kind);
        }

        let mut index = 0;

        constraints.retain(|_| {
            let condition = indices.contains(&index);
            index += 1;
            !condition
        });

        if !changed {
            break;
        }
    }

    type_table.purge_unknown();
    type_table
}

fn extract_type_kind(
    top_level_table: &TopLevelTable,
    type_table: &TypeTable,
    reverse_table: &ReverseTypeVariableTable,
    target: &TypeConstraintTarget,
) -> Option<TypeKind> {
    match target {
        TypeConstraintTarget::Concrete { kind } => Some(kind.clone()),
        TypeConstraintTarget::Variable { variable } => {
            type_table.types.get(&reverse_table[variable]).cloned()
        }
        TypeConstraintTarget::BinaryOperation {
            operator,
            left,
            right,
        } => {
            let left = type_table.types.get(&reverse_table[left]);
            let right = type_table.types.get(&reverse_table[right]);
            match (left, right) {
                (Some(left), Some(right)) => BUILT_IN_BINARY_OPERATOR
                    .result_type(
                        *operator,
                        left.unwrap_reference().clone(),
                        right.unwrap_reference().clone(),
                    )
                    .cloned()
                    .or_else(|| Some(TypeKind::unknown())),
                _ => None,
            }
        }
        TypeConstraintTarget::UnaryOperation { operator, right } => {
            let right = type_table.types.get(&reverse_table[right]);
            match right {
                Some(right) => match operator {
                    HIRUnaryOperatorKind::AddressOf => {
                        Some(TypeKind::pointer(right.unwrap_reference().clone()))
                    }
                    HIRUnaryOperatorKind::Dereference => Some(match right.unwrap_reference() {
                        TypeKind::Pointer { inner } => TypeKind::reference(*inner.clone()),
                        _ => TypeKind::unknown(),
                    }),
                    _ => BUILT_IN_UNARY_OPERATOR
                        .result_type(*operator, right.unwrap_reference().clone())
                        .cloned()
                        .or_else(|| Some(TypeKind::unknown())),
                },
                _ => None,
            }
        }
        TypeConstraintTarget::CallableParamType { variable, index } => {
            match type_table.types.get(&reverse_table[variable]) {
                Some(type_kind) => Some(match type_kind.unwrap_reference() {
                    TypeKind::Callable { params, .. } => params
                        .get(*index)
                        .cloned()
                        .unwrap_or_else(|| TypeKind::unknown()),
                    _ => TypeKind::unknown(),
                }),
                None => None,
            }
        }
        TypeConstraintTarget::CallableReturnType { variable } => {
            match type_table.types.get(&reverse_table[variable]) {
                Some(type_kind) => Some(match type_kind.unwrap_reference() {
                    TypeKind::Callable { return_type, .. } => *return_type.clone(),
                    _ => TypeKind::unknown(),
                }),
                None => None,
            }
        }
        TypeConstraintTarget::MemberType { variable, member } => {
            match type_table.types.get(&reverse_table[variable]) {
                Some(type_kind) => Some(match type_kind.unwrap_reference() {
                    TypeKind::UserStruct { id } => {
                        let user_struct = top_level_table.user_types[id].as_user_struct().unwrap();
                        match user_struct.field_names.get(member) {
                            Some(index) => user_struct.fields[*index].clone(),
                            None => TypeKind::unknown(),
                        }
                    }
                    _ => TypeKind::unknown(),
                }),
                None => None,
            }
        }
    }
}
