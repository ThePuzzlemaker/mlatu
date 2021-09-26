use anyhow::bail;
use combine::parser::char::char;
use combine::parser::choice::or;
use combine::{eof, many1, parser, satisfy, sep_end_by, skip_many, EasyParser, Parser, Stream};
use unic_ucd::category::GeneralCategory;

use crate::ast::{Rule, Term};

const RESERVED:&[char] = &['(', ')', ';', '='];

fn is_word_component(c:char) -> bool {
  if RESERVED.contains(&c) {
    false
  } else {
    let cat = GeneralCategory::of(c);
    cat.is_letter() || cat.is_number() || cat.is_punctuation() | cat.is_symbol()
  }
}

fn is_separator(c:char) -> bool {
  let cat = GeneralCategory::of(c);
  cat.is_separator()
}

fn word_parser<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=char>, {
  many1::<String, _, _>(satisfy(is_word_component))
}

fn separator_parser<Input>() -> impl Parser<Input, Output=()>
  where Input: Stream<Token=char>, {
  skip_many(satisfy(is_separator))
}

fn left_paren<Input>() -> impl Parser<Input, Output=char>
  where Input: Stream<Token=char>, {
  char('(')
}

fn right_paren<Input>() -> impl Parser<Input, Output=char>
  where Input: Stream<Token=char>, {
  char(')')
}

fn semicolon<Input>() -> impl Parser<Input, Output=char>
  where Input: Stream<Token=char>, {
  char(';')
}

fn equal<Input>() -> impl Parser<Input, Output=char>
  where Input: Stream<Token=char>, {
  char('=')
}

fn term_parser<Input>() -> impl Parser<Input, Output=Term>
  where Input: Stream<Token=char>, {
  let word = word_parser().map(Term::new_word);
  let quote = left_paren().with(terms_parser().map(Term::new_quote).skip(right_paren()));
  or(word, quote)
}

parser! {
fn terms_parser[Input]()(Input) -> Vec<Term>
where [Input: Stream<Token = char>] {
    separator_parser().with(sep_end_by::<Vec<_>, _, _, _>(term_parser(), separator_parser()))
}
}

fn rule_parser<Input>() -> impl Parser<Input, Output=Rule>
  where Input: Stream<Token=char>, {
  let pattern = terms_parser().skip(equal());
  let replacement = terms_parser().skip(semicolon());
  pattern.and(replacement).map(|(p, r)| Rule::new(p, r))
}

parser! {
fn rules_parser[Input]()(Input) -> Vec<Rule>
where [Input: Stream<Token = char>] {
    separator_parser().with(sep_end_by::<Vec<_>, _, _, _>(rule_parser(), separator_parser()))
}
}

/// # Errors
///
/// Will return `Err` if there was an error parsing the term.
pub fn parse_term(input:&str) -> anyhow::Result<Term> {
  let mut parser = term_parser().skip(eof());
  match parser.easy_parse(input) {
    | Ok((result, _)) => Ok(result),
    | Err(e) => bail!("{}", e.map_position(|p| p.translate_position(input))),
  }
}

/// # Errors
///
/// Will return `Err` if there was an error parsing the terms.
pub fn parse_terms(input:&str) -> anyhow::Result<Vec<Term>> {
  let mut parser = terms_parser().skip(eof());
  match parser.easy_parse(input) {
    | Ok((result, _)) => Ok(result),
    | Err(e) => bail!("{}", e.map_position(|p| p.translate_position(input))),
  }
}

/// # Errors
///
/// Will return `Err` if there was an error parsing the terms.
pub fn parse_rule(input:&str) -> anyhow::Result<Rule> {
  let mut parser = rule_parser().skip(eof());
  match parser.easy_parse(input) {
    | Ok((result, _)) => Ok(result),
    | Err(e) => bail!("{}", e.map_position(|p| p.translate_position(input))),
  }
}

/// # Errors
///
/// Will return `Err` if there was an error parsing the terms.
pub fn parse_rules(input:&str) -> anyhow::Result<Vec<Rule>> {
  let mut parser = rules_parser().skip(eof());
  match parser.easy_parse(input) {
    | Ok((result, _)) => Ok(result),
    | Err(e) => bail!("{}", e.map_position(|p| p.translate_position(input))),
  }
}