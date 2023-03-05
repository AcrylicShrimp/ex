use crate::{BasicBlock, BasicBlockExit, BlockId, BlockIdAllocator};
use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin};
use ex_parser::NodeId;
use ex_resolve_ref::Function;
use ex_span::{SourceFile, Span};
use ex_symbol::Symbol;
use std::{
    collections::{HashMap, HashSet},
    sync::{mpsc::Sender, Arc},
};

#[derive(Debug, Clone)]
pub struct FunctionControlFlowGraph {
    pub node: NodeId,
    pub span: Span,
    pub initialized_variables: HashSet<NodeId>,
    pub partially_initialized_variables: HashMap<NodeId, HashSet<Vec<Symbol>>>,
    pub entry_block: BlockId,
    pub exit_block: Option<BlockId>,
    pub blocks: HashMap<BlockId, BasicBlock>,
}

impl FunctionControlFlowGraph {
    pub fn new(block_id_alloc: &mut BlockIdAllocator, node: NodeId, span: Span) -> Self {
        let entry_block_id = block_id_alloc.allocate();
        let entry_block = BasicBlock::new(entry_block_id);

        let blocks = HashMap::from_iter(vec![(entry_block_id, entry_block)]);

        Self {
            node,
            span,
            initialized_variables: Default::default(),
            partially_initialized_variables: Default::default(),
            entry_block: entry_block_id,
            exit_block: None,
            blocks,
        }
    }

    pub fn insert_block(&mut self, block: BasicBlock) {
        self.blocks.insert(block.id, block);
    }

    pub fn validate(
        mut self,
        function: &Function,
        file: &Arc<SourceFile>,
        diagnostics: &Sender<Diagnostics>,
    ) {
        // use std::io::Write;
        // let mut vis_file = std::fs::File::create(format!(
        //     "visualizations/{}-dag.html",
        //     function.name.symbol.to_str().replace("`", "")
        // ))
        // .unwrap();
        // writeln!(vis_file, "{}", self.visualize_in_html()).unwrap();

        let unreachable_blocks = self.purge_unreachable_blocks();

        for unreachable_block in unreachable_blocks {
            if unreachable_block.statements.is_empty() {
                continue;
            }

            let mut span = unreachable_block.statements[0].1;
            for (_, statement_span) in &unreachable_block.statements[1..] {
                span = span.to(*statement_span);
            }

            diagnostics
                .send(Diagnostics {
                    level: DiagnosticsLevel::Warning,
                    message: format!("unreachable code in function {}", function.name.symbol),
                    origin: Some(DiagnosticsOrigin {
                        file: file.clone(),
                        span,
                    }),
                    sub_diagnostics: vec![],
                })
                .unwrap();
        }

        if let Some(exit_block_id) = self.exit_block {
            if self.blocks.contains_key(&exit_block_id) {
                if function.return_typename.is_some() {
                    diagnostics
                        .send(Diagnostics {
                            level: DiagnosticsLevel::Error,
                            message: format!(
                                "function {} has a return type but does not return a value",
                                function.name.symbol
                            ),
                            origin: Some(DiagnosticsOrigin {
                                file: file.clone(),
                                span: function.span,
                            }),
                            sub_diagnostics: vec![],
                        })
                        .unwrap();
                }
            }
        }
    }

    fn compute_unreachable_blocks(&self) -> HashSet<BlockId> {
        let mut reachable_blocks = HashSet::new();
        let mut worklist = vec![self.entry_block];

        while let Some(block_id) = worklist.pop() {
            if reachable_blocks.contains(&block_id) {
                continue;
            }

            reachable_blocks.insert(block_id);

            let block = self.blocks.get(&block_id).unwrap();
            match block.exit.as_ref().unwrap() {
                BasicBlockExit::Terminate { .. } => {}
                BasicBlockExit::Jump { block } => {
                    worklist.push(*block);
                }
                BasicBlockExit::Branch {
                    left_block,
                    right_block,
                } => {
                    worklist.push(*left_block);
                    worklist.push(*right_block);
                }
            }
        }

        let mut unreachable_blocks = HashSet::new();

        for (block_id, _) in &self.blocks {
            if !reachable_blocks.contains(block_id) {
                unreachable_blocks.insert(*block_id);
            }
        }

        unreachable_blocks
    }

    fn purge_unreachable_blocks(&mut self) -> Vec<BasicBlock> {
        let unreachable_blocks = self.compute_unreachable_blocks();
        unreachable_blocks
            .into_iter()
            .map(|block_id| self.blocks.remove(&block_id).unwrap())
            .collect()
    }

    pub fn visualize_in_html(&self) -> String {
        let mut html_builder = Vec::new();
        html_builder.push(
            r#"<!DOCTYPE html>
<html>
<head>
<title>DAG Visualization</title>
<script type="text/javascript" src="https://cdn.jsdelivr.net/npm/gojs/release/go.js"></script>
</head>
<body>
<div id="myDiagramDiv" style="width: 1024px; height: 1024px;"></div>
<script>
var $ = go.GraphObject.make;

var myDiagram =
  $(go.Diagram, "myDiagramDiv",
    {
      layout: $(go.LayeredDigraphLayout, { direction: 90, layerSpacing: 20 }),
      "undoManager.isEnabled": true
    });

myDiagram.nodeTemplate =
  $(go.Node, "Auto",
    $(go.Shape, "RoundedRectangle", { fill: "white", portId: "" }),
    $(go.TextBlock, { margin: 8 },
      new go.Binding("text", "key"))
  );

myDiagram.linkTemplate =
  $(go.Link,
    { routing: go.Link.Orthogonal, corner: 5 },
    $(go.Shape, { strokeWidth: 1.5, stroke: "\#555" })
  );
        
        "#
            .to_owned(),
        );

        let node_data_array = self.blocks.values().map(|block| {
            let mut node_data = String::new();
            node_data.push_str("{ key: \"");
            node_data.push_str(&block.id.get().to_string());
            node_data.push_str("\" }");
            node_data
        });
        let link_data_array = self.blocks.values().map(|block| {
            if let Some(exit) = &block.exit {
                match exit {
                    BasicBlockExit::Terminate { .. } => return "".to_string(),
                    BasicBlockExit::Jump { block: target } => {
                        return format!(
                            "{{ from: \"{}\", to: \"{}\" }}",
                            block.id.get(),
                            target.get()
                        );
                    }
                    BasicBlockExit::Branch {
                        left_block,
                        right_block,
                    } => {
                        return format!(
                            "{{ from: \"{}\", to: \"{}\" }}, {{ from: \"{}\", to: \"{}\" }}",
                            block.id.get(),
                            left_block.get(),
                            block.id.get(),
                            right_block.get()
                        );
                    }
                }
            } else {
                return "".to_string();
            }
        });

        html_builder.push(format!(
            "var nodeDataArray = [{}];",
            node_data_array.collect::<Vec<_>>().join(", ")
        ));
        html_builder.push(format!(
            "var linkDataArray = [{}];",
            link_data_array
                .filter(|link_data| !link_data.is_empty())
                .collect::<Vec<_>>()
                .join(", ")
        ));

        html_builder.push(
            r#"myDiagram.model = new go.GraphLinksModel(nodeDataArray, linkDataArray);
</script>
</body>
</html>
"#
            .to_owned(),
        );

        html_builder.join("\n")
    }
}
