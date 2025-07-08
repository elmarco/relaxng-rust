use relaxng_model::model;
use std::collections::{BTreeSet, HashSet};
use std::fmt::Write;
use tracing::trace;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Node {
    Element(Element),
    Attribute(String),
    Text,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Element {
    name: String,
    attributes: BTreeSet<String>,
    children: BTreeSet<Node>,
}

pub struct TreeBuilder;

impl TreeBuilder {
    pub fn new() -> Self {
        TreeBuilder
    }

    pub fn build(&mut self, pattern: &model::Pattern) -> BTreeSet<Node> {
        let mut nodes = BTreeSet::new();
        let mut visited = HashSet::new();
        self.build_nodes(pattern, &mut nodes, &mut visited);
        nodes
    }

    fn build_nodes(
        &mut self,
        pattern: &model::Pattern,
        nodes: &mut BTreeSet<Node>,
        visited: &mut HashSet<*const model::DefineRule>,
    ) {
        match pattern {
            model::Pattern::Attribute(name_class, _) => {
                let mut name_dumper = Dumper::new();
                name_dumper.dump_name_class(name_class);
                let name = name_dumper.output;
                trace!(?name);
                nodes.insert(Node::Attribute(name));
            }
            model::Pattern::Element(name_class, content_pattern) => {
                let mut name_dumper = Dumper::new();
                name_dumper.dump_name_class(name_class);
                let name = name_dumper.output;

                let mut attributes = BTreeSet::new();
                let mut children = BTreeSet::new();
                let mut visited_clone = visited.clone();
                self.build_nodes(content_pattern, &mut children, &mut visited_clone);

                let mut real_children = BTreeSet::new();
                for child in children {
                    match child {
                        Node::Attribute(attr) => {
                            attributes.insert(attr);
                        }
                        Node::Element(element) => {
                            real_children.insert(Node::Element(element));
                        }
                        Node::Text => {
                            real_children.insert(Node::Text);
                        }
                    }
                }

                nodes.insert(Node::Element(Element {
                    name,
                    attributes,
                    children: real_children,
                }));
            }
            model::Pattern::Group(patterns)
            | model::Pattern::Interleave(patterns)
            | model::Pattern::Choice(patterns) => {
                for p in patterns {
                    self.build_nodes(p, nodes, visited);
                }
            }
            model::Pattern::Ref(_, _, pat_ref) => {
                let rule_ptr = pat_ref.0.as_ptr() as *const model::DefineRule;
                if !visited.contains(&rule_ptr) {
                    visited.insert(rule_ptr);
                    if let Some(rule) = pat_ref.0.borrow().as_ref() {
                        self.build_nodes(rule.pattern(), nodes, visited);
                    }
                }
            }
            model::Pattern::Optional(p)
            | model::Pattern::OneOrMore(p)
            | model::Pattern::ZeroOrMore(p)
            | model::Pattern::Mixed(p) => self.build_nodes(p, nodes, visited),
            model::Pattern::Text => {
                nodes.insert(Node::Text);
            }
            model::Pattern::Empty => dbg!(),
            model::Pattern::NotAllowed => dbg!(),
            model::Pattern::DatatypeValue { datatype: _ } => dbg!(),
            model::Pattern::DatatypeName {
                datatype: _,
                except: _,
            } => dbg!(),
            model::Pattern::List(_pattern) => dbg!(),
        }
    }
}

pub struct Dumper {
    indent: usize,
    output: String,
}

impl Dumper {
    pub fn new() -> Self {
        Dumper {
            indent: 0,
            output: String::new(),
        }
    }

    pub fn dump_to_string(&mut self, model: &model::DefineRule) -> String {
        let mut builder = TreeBuilder::new();
        let tree = builder.build(model.pattern());
        self.dump_nodes(&tree);
        self.output.clone()
    }

    fn dump_nodes(&mut self, nodes: &BTreeSet<Node>) {
        for node in nodes {
            match node {
                Node::Element(element) => {
                    self.print_indent();
                    write!(self.output, "<{}", element.name).unwrap();
                    for attr in &element.attributes {
                        write!(self.output, " {}", attr).unwrap();
                    }
                    if element.children.is_empty() {
                        writeln!(self.output, "/>").unwrap();
                    } else {
                        writeln!(self.output, ">").unwrap();
                        self.indent += 1;
                        self.dump_nodes(&element.children);
                        self.indent -= 1;
                        self.print_indent();
                        writeln!(self.output, "</{}>", element.name).unwrap();
                    }
                }
                Node::Text => {
                    self.print_indent();
                    writeln!(self.output, "text").unwrap();
                }
                Node::Attribute(_) => {}
            }
        }
    }
    fn dump_name_class(&mut self, name_class: &model::NameClass) {
        match name_class {
            model::NameClass::Named { name, .. } => {
                write!(self.output, "{}", name).unwrap();
            }
            model::NameClass::NsName {
                namespace_uri,
                except,
            } => {
                write!(self.output, "{}:*", namespace_uri).unwrap();
                if let Some(e) = except {
                    write!(self.output, " - ").unwrap();
                    self.dump_name_class(e);
                }
            }
            model::NameClass::AnyName { except } => {
                write!(self.output, "*").unwrap();
                if let Some(e) = except {
                    write!(self.output, " - ").unwrap();
                    self.dump_name_class(e);
                }
            }
            model::NameClass::Alt { a, b } => {
                self.dump_name_class(a);
                write!(self.output, " | ").unwrap();
                self.dump_name_class(b);
            }
        }
    }

    fn print_indent(&mut self) {
        for _ in 0..self.indent {
            write!(self.output, "  ").unwrap();
        }
    }
}
