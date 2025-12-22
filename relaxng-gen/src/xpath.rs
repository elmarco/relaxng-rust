use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, space0};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::delimited;
use nom::{IResult, Parser};
use serde::Deserialize;
use tracing::error;

// sort-of-xpath...
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub(crate) struct XPath(Vec<XPathExpr>);

impl FromStr for XPath {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_xpath(s) {
            Ok(("", expr)) => Ok(XPath(expr)),
            Ok((remaining, _)) => Err(format!("Unparsed input: {remaining}")),
            Err(e) => Err(format!("Parse error: {e}")),
        }
    }
}

impl XPath {
    pub(crate) fn matches(&self, xpath: &XPath) -> bool {
        matches_recurse(&self.0, &xpath.0)
    }

    pub(crate) fn format(&self, with_index: bool) -> String {
        let mut res = String::new();
        for e in self.0.iter() {
            match e {
                XPathExpr::Sep => res.push('/'),
                XPathExpr::Descendant => res.push_str("//"),
                XPathExpr::Node(n) => res.push_str(n),
                XPathExpr::Predicate(xpath_predicate) => match xpath_predicate {
                    XPathPredicate::Index(n) => {
                        if with_index {
                            res = format!("{res}[{n}]")
                        }
                    }
                    XPathPredicate::Equal(name, value) => res = format!("{res}[{name}='{value}']"),
                },
            }
        }

        res
    }

    #[cfg(test)]
    pub(crate) fn to_string(&self, with_index: bool) -> String {
        self.format(with_index)
    }

    #[cfg(test)]
    pub(crate) fn matches_str(&self, path: &str) -> bool {
        let Ok(path) = Self::from_str(path) else {
            return false;
        };

        self.matches(&path)
    }
}

impl std::fmt::Display for XPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(true))
    }
}

fn matches_recurse(pattern: &[XPathExpr], path: &[XPathExpr]) -> bool {
    match (pattern, path) {
        ([], [XPathExpr::Predicate(XPathPredicate::Index(_))]) => true,
        ([XPathExpr::Sep, tail @ ..], [XPathExpr::Sep, ptail @ ..]) => matches_recurse(tail, ptail),
        ([XPathExpr::Sep, tail @ ..], [XPathExpr::Predicate(_), ptail @ ..]) => {
            matches_recurse(tail, ptail)
        }
        ([XPathExpr::Descendant, tail @ ..], [XPathExpr::Sep, XPathExpr::Node(_), ptail @ ..]) => {
            matches_recurse(pattern, ptail) || matches_recurse(tail, path)
        }
        ([XPathExpr::Descendant, tail @ ..], [XPathExpr::Predicate(_), ptail @ ..]) => {
            matches_recurse(pattern, ptail) || matches_recurse(tail, path)
        }

        ([XPathExpr::Node(name), tail @ ..], [XPathExpr::Node(pname), ptail @ ..]) => {
            if name == pname {
                matches_recurse(tail, ptail)
            } else {
                false
            }
        }
        (
            [XPathExpr::Node(name), tail @ ..],
            [XPathExpr::Sep, XPathExpr::Node(pname), ptail @ ..],
        ) => {
            if name == pname {
                matches_recurse(tail, ptail)
            } else {
                matches_recurse(pattern, ptail)
            }
        }
        ([XPathExpr::Predicate(pred), tail @ ..], [XPathExpr::Predicate(ppred), ptail @ ..]) => {
            if pred == ppred {
                matches_recurse(tail, ptail)
            } else {
                false
            }
        }
        ([XPathExpr::Node(_), ..], [XPathExpr::Predicate(_), ..]) => false,
        ([], []) => true,
        (_, []) => false,
        ([], _) => false,
        (_, _) => {
            error!(?pattern, ?path);
            unimplemented!()
        }
    }
}

impl<'de> Deserialize<'de> for XPath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        XPath::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum XPathExpr {
    Sep,
    Descendant,
    Node(String),
    Predicate(XPathPredicate),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum XPathPredicate {
    Index(usize),
    Equal(String, String),
}

fn is_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

// Parse a name (element or attribute name)
fn parse_name(input: &str) -> IResult<&str, &str> {
    take_while1(is_name_char)(input)
}

// Parse ()
fn parse_arg(input: &str) -> IResult<&str, &str> {
    tag("()").parse(input)
}

// Parse string literal in single or double quotes
fn parse_string_literal(input: &str) -> IResult<&str, &str> {
    alt((
        delimited(char('"'), take_while1(|c| c != '"'), char('"')),
        delimited(char('\''), take_while1(|c| c != '\''), char('\'')),
    ))
    .parse(input)
}

fn parse_sep(input: &str) -> IResult<&str, XPathExpr> {
    map(char('/'), |_| XPathExpr::Sep).parse(input)
}

fn parse_descendant(input: &str) -> IResult<&str, XPathExpr> {
    map(tag("//"), |_| XPathExpr::Descendant).parse(input)
}

fn parse_node(input: &str) -> IResult<&str, XPathExpr> {
    map(parse_name, |name: &str| XPathExpr::Node(name.to_string())).parse(input)
}

fn parse_sep_or_descendant(input: &str) -> IResult<&str, XPathExpr> {
    alt((parse_descendant, parse_sep)).parse(input)
}

fn parse_index_predicate(input: &str) -> IResult<&str, XPathPredicate> {
    map(delimited(char('['), nom::character::complete::u64, char(']')), |index| {
        XPathPredicate::Index(index as usize)
    })
    .parse(input)
}

// Parse equality predicate like "[@attr='value']" or "[name='value']"
fn parse_eq_predicate(input: &str) -> IResult<&str, XPathPredicate> {
    map(
        delimited(
            char('['),
            (
                opt(char('@')),
                parse_name,
                opt(parse_arg),
                space0,
                char('='),
                space0,
                parse_string_literal,
            ),
            char(']'),
        ),
        |(attr_prefix, name, arg, _, _, _, value)| {
            let name_str = if attr_prefix.is_some() {
                format!("@{name}")
            } else if arg.is_some() {
                format!("{name}()")
            } else {
                name.to_string()
            };
            XPathPredicate::Equal(name_str, value.to_string())
        },
    )
    .parse(input)
}

fn parse_predicate(input: &str) -> IResult<&str, XPathPredicate> {
    alt((parse_index_predicate, parse_eq_predicate)).parse(input)
}

fn parse_expr_with_predicate(input: &str) -> IResult<&str, (XPathExpr, Vec<XPathExpr>)> {
    map((parse_node, many0(parse_predicate)), |(node, predicate)| {
        (node, predicate.into_iter().map(XPathExpr::Predicate).collect())
    })
    .parse(input)
}

fn parse_xpath(input: &str) -> IResult<&str, Vec<XPathExpr>> {
    let (input, (opt_sep, (first, opt_pred))) =
        (opt(parse_sep_or_descendant), parse_expr_with_predicate).parse(input)?;
    let mut xpath = Vec::new();
    xpath.extend(opt_sep);
    xpath.push(first);
    xpath.extend(opt_pred);
    let (input, rest) = many0((parse_sep_or_descendant, parse_expr_with_predicate)).parse(input)?;
    for (sep, (el, opt_pred)) in rest {
        xpath.extend([sep, el]);
        xpath.extend(opt_pred);
    }
    Ok((input, xpath))
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use super::*;

    #[test]
    fn test_from_str_path_with_predicate() {
        let _p = XPath::from_str("book").unwrap();
        let _p = XPath::from_str("/book").unwrap();
        let _p = XPath::from_str("/book//title").unwrap();
        let p = XPath::from_str("//library/book[@lang='en']//title").unwrap();
        assert_eq!(p.to_string(true), "//library/book[@lang='en']//title");
    }

    #[test]
    fn test_matches() {
        let p = XPath::from_str("book").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(p.matches_str("/some/book"));
        assert!(!p.matches_str("/some/book/author"));
        let p = XPath::from_str("/book").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(p.matches_str("/book"));
        assert!(!p.matches_str("/some/book"));
        let p = XPath::from_str("//book").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(p.matches_str("/some/book"));
        assert!(!p.matches_str("/some/book/author"));
        let p = XPath::from_str("/some/book").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(!p.matches_str("/book"));
        assert!(p.matches_str("/some/book"));
        assert!(!p.matches_str("/some/book/author"));
        let p = XPath::from_str("/some//book").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(!p.matches_str("/book"));
        assert!(p.matches_str("/some/book"));
        assert!(!p.matches_str("/some/book/author"));
        assert!(p.matches_str("/some/other/book"));
        assert!(!p.matches_str("/some/other/book/author"));
        let p = XPath::from_str("/book[@author='John']").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(!p.matches_str("/book"));
        assert!(!p.matches_str("/book[@author='Fred']"));
        assert!(p.matches_str("/book[@author='John']"));
        assert!(!p.matches_str("/book[@author='John']/foo"));
        let p = XPath::from_str("//ref[@name='cpuMode']").unwrap();
        assert!(p.matches_str(
            "/choice[1]/ref[@name='guestcpu']/element[@name='cpu']/group/optional/ref[@name='cpuMode']"
        ));
        let p = XPath::from_str("/some[1]").unwrap();
        assert!(!p.matches_str(""));
        assert!(!p.matches_str("/some"));
        assert!(p.matches_str("/some[1]"));
        let p = XPath::from_str("/some").unwrap();
        assert!(!p.matches_str(""));
        assert!(p.matches_str("/some"));
        assert!(p.matches_str("/some[1]"));
        assert!(p.matches_str("/some[2]"));
    }

    #[test]
    fn test_text() {
        let p = XPath::from_str("/e[text()='foo']").unwrap();
        assert!(p.matches_str("/e[text()='foo']"));
    }

    #[test]
    fn test_one() {
        let p = XPath::from_str("/element[@name='foo']").unwrap();
        assert!(p.matches_str("/element[@name='foo'][2]"));
    }
}
