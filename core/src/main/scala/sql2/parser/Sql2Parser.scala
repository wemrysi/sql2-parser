/*
 * Copyright 2014–2018 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sql2.parser

import parseback._

import java.lang.SuppressWarnings

import scala.{Any, Array, StringContext}
// FIXME: What is required for "foo".r support?
import scala.Predef._
import scala.util.matching.Regex

/** Strict superset of SQL-92.
  *
  * SQL-92 parsing ported from the following BNF grammar
  * https://ronsavage.github.io/SQL/sql-92.bnf.html
  */
@SuppressWarnings(Array("org.wartremover.warts.JavaSerializable"))
object Sql2Parser {

  implicit final class CaseInsensitivePattern(val s: String) extends scala.AnyVal {
    def ci: Regex = s"(?i)${s}".r
  }

  // FIXME: Should be unecessary eventually.
  def l(s: String): Parser[String] =
    parseback.literal(s)


  // Basic Definitions of Characters Used, Tokens, Symbols, Etc.

  //   space
  // | doubleQuote
  // | percent
  // | ampersand
  // | quote
  // | leftParen
  // | rightParen
  // | asterisk
  // | plusSign
  // | comma
  // | minusSign
  // | period
  // | solidus
  // | colon
  // | semicolon
  // | lessThanOperator
  // | greaterThanOperator
  // | equalsOperator
  // | questionMark
  // | underscore
  // | verticalBar
  lazy val sqlSpecialCharacter: Parser[Any] =
    """[ "%&'\(\)\*\+,-\.\\:;<>=\?_|]""".r

  lazy val space: Parser[Any] =
    l(" ")

  lazy val doubleQuote: Parser[Any] =
    l("\"")

  lazy val quote: Parser[Any] =
    l("'")

  lazy val period: Parser[Any] =
    l(".")

  lazy val underscore: Parser[Any] =
    l("_")

  lazy val leftBracket: Parser[Any] =
    l("[")

  lazy val rightBracket: Parser[Any] =
    l("]")

  lazy val token: Parser[Any] =
    nondelimiterToken | delimiterToken

  lazy val nondelimiterToken: Parser[Any] = (
      regularIdentifier
    | keyWord
    | unsignedNumericLiteral
    | nationalCharacterStringLiteral
    | bitStringLiteral
    | hexStringLiteral
  )

  lazy val regularIdentifier: Parser[Any] =
    identifierBody

  // FIXME: What is an appropriate identifier start?
  lazy val identifierBody: Parser[Any] =
    s"""\p{Alpha}\w*""".r

  lazy val keyWord: Parser[Any] =
    reservedWord | nonReservedWord

  // TODO
  lazy val reservedWord: Parser[Any] =
    ()

  // TODO
  lazy val nonReservedWord: Parser[Any] =
    ()


  // Literal Numbers, Strings, Dates and Times

  lazy val unsignedNumericLiteral: Parser[Any] = (
      exactNumericLiteral
    | approximateNumericLiteral
  )

  lazy val exactNumericLiteral: Parser[Any] = (
      unsignedInteger ~ (period ~ unsignedInteger.?).?
    | period ~ unsignedInteger
  )

  lazy val unsignedInteger: Parser[Any] =
    """\d+""".r

  lazy val approximateNumericLiteral: Parser[Any] =
    mantissa ~ "E".ci ~ exponent

  lazy val mantissa: Parser[Any] =
    exactNumericLiteral

  lazy val exponent: Parser[Any] =
    signedInteger

  lazy val signedInteger: Parser[Any] =
    sign.? ~ unsignedInteger

  lazy val sign: Parser[Any] =
    "[+-]".r

  lazy val nationalCharacterStringLiteral: Parser[Any] =
    ("N".ci ~ quote ~!~ characters.? ~!~ quote) ~ (separator.+ ~ quote ~!~ characters.? ~!~ quote).*

  // NB: characterRepresentation+
  lazy val characters: Parser[Any] =
    "(?:[^']|'')+".r

  lazy val separator: Parser[Any] =
    (comment | space | newline).+

  lazy val comment: Parser[Any] =
    commentIntroducer ~ commentCharacters.? ~ newline

  lazy val commentIntroducer: Parser[Any] =
    "---*".r

  // NB: commentCharacter+
  lazy val commentCharacters: Parser[Any] =
    ".+".r

  lazy val newline: Parser[Any] =
    "\\n|\\r\\n".r

  lazy val bitStringLiteral: Parser[Any] =
    ("B".ci ~ quote ~!~ bits.? ~!~ quote) ~ (separator.+ ~ quote ~!~ bits.? ~!~ quote).*

  // NB: bit+
  lazy val bits: Parser[Any] =
    "[01]+".r

  lazy val hexStringLiteral: Parser[Any] =
    ("X".ci ~ quote ~!~ hexits.? ~!~ quote) ~ (separator.+ ~ quote ~!~ hexits.? ~!~ quote).*

  // NB: hexit+
  lazy val hexits: Parser[Any] =
    """\p{XDigit}+""".r

  lazy val delimiterToken: Parser[Any] = (
      characterStringLiteral
    | dateString
    | timeString
    | timestampString
    | delimitedIdentifier
    | sqlSpecialCharacter
    | notEqualsOperator
    | greaterThanOrEqualsOperator
    | concatenationOperator
    | doublePeriod
    | leftBracket
    | rightBracket
  )

  lazy val characterStringLiteral: Parser[Any] =
    (introducer ~ characterSetSpecification).? ~ (quote ~!~ characters.? ~!~ quote) ~ (separator.+ ~ quote ~!~ characters.? ~!~ quote).*

  lazy val introducer: Parser[Any] =
    underscore

  lazy val characterSetSpecification: Parser[Any] = (
      standardCharacterRepertoireName
    | implementationDefinedCharacterRepertoireName
    | userDefinedCharacterRepertoireName
    | standardUniversalCharacterFormOfUseName
    | implementationDefinedUniversalCharacterFormOfUseName
  )

  lazy val standardCharacterRepertoireName: Parser[Any] =
    characterSetName

  lazy val characterSetName: Parser[Any] =
    (schemaName ~ period).? ~ sqlLanguageIdentifier

  lazy val schemaName: Parser[Any] =
    (catalogName ~ period).? ~ unqualifiedSchemaName

  lazy val catalogName: Parser[Any] =
    identifier

  lazy val identifier: Parser[Any] =
    (introducer ~ characterSetSpecification).? ~ actualIdentifier

  lazy val actualIdentifier: Parser[Any] =
    regularIdentifier | delimitedIdentifier

  lazy val delimitedIdentifier: Parser[Any] =
    doubleQuote ~!~ delimitedIdentifierBody ~!~ doubleQuote

  lazy val delimitedIdentifierBody: Parser[Any] =
    """(?:[^"]|"")+""".r

  lazy val unqualifiedSchemaName: Parser[Any] =
    identifier

  lazy val sqlLanguageIdentifier: Parser[Any] =
    """\p{Alpha}\w*""".r

  lazy val implementationDefinedCharacterRepertoireName: Parser[Any] =
    characterSetName

  lazy val userDefinedCharacterRepertoireName: Parser[Any] =
    characterSetName

  lazy val standardUniversalCharacterFormOfUseName: Parser[Any] =
    characterSetName

  lazy val implementationDefinedUniversalCharacterFormOfUseName: Parser[Any] =
    characterSetName

  lazy val dateString: Parser[Any] =
    quote ~!~ dateValue ~!~ quote

  lazy val dateValue: Parser[Any] =
    dateRegex.r

  lazy val dateRegex: String =
    """\d+-\d+-\d+"""

  lazy val timeString: Parser[Any] =
    quote ~!~ timeAndZoneValue ~!~ quote

  // NB: timeValue ~ timeZoneInterval.?
  lazy val timeAndZoneValue: Parser[Any] =
    timeAndZoneRegex.r

  lazy val timeAndZoneRegex: String =
    s"""\d+:\d+:${secondsRegex}(?:${timeZoneIntervalRegex})?"""

  lazy val secondsRegex: String =
    """\d+(?:\.\d*)?"""

  lazy val timeZoneIntervalRegex: String =
    """[+-]\d+:\d+"""

  lazy val timestampString: Parser[Any] =
    quote ~!~ timestampValue ~!~ quote

  lazy val timestampValue: Parser[Any] =
    (dateRegex + " " + timeAndZoneRegex).r

  lazy val intervalString: Parser[Any] =
    quote ~!~ (yearMonthLiteral | dayTimeLiteral) ~!~ quote

  lazy val yearMonthLiteral: Parser[Any] =
    """(?:\d+-)?\d+""".r

  lazy val dayTimeLiteral: Parser[Any] =
    dayTimeInterval | timeInterval

  lazy val dayTimeInterval: Parser[Any] =
    s"""\d+(?: \d+(?::\d+(?::${secondsRegex}(?:${timeZoneIntervalRegex})?)?)?)?""".r

  lazy val timeInterval: Parser[Any] = (
      s"""\d+(?::\d+(?::${secondsRegex})?)?""".r
    | s"""\d+(?::${secondsRegex})?""".r
    | secondsRegex.r
  )

  lazy val notEqualsOperator: Parser[Any] =
    l("<>")

  lazy val greaterThanOrEqualsOperator: Parser[Any] =
    l(">=")

  lazy val lessThanOrEqualsOperator: Parser[Any] =
    l("<=")

  lazy val concatenationOperator: Parser[Any] =
    l("||")

  lazy val doublePeriod: Parser[Any] =
    l("..")


  // SQL Module

  lazy val qualifiedLocalTableName: Parser[Any] =
    "MODULE".ci ~ "." ~ localTableName

  lazy val localTableName: Parser[Any] =
    qualifiedIdentifier

  lazy val qualifiedIdentifier: Parser[Any] =
    identifier

  lazy val columnName: Parser[Any] =
    identifier


  // Data Types

  lazy val dataType: Parser[Any] =
    ()

  lazy val domainName: Parser[Any] =
    qualifiedName

  lazy val qualifiedName: Parser[Any] =
    ()


  // Literals

  lazy val literal: Parser[Any] =
    ()


  // Constraints

  lazy val tableName: Parser[Any] =
    qualifiedName | qualifiedLocalTableName

  lazy val columnNameList: Parser[Any] =
    columnName ~ (l(",") ~ columnName).*


  // Search Conditions

  lazy val searchCondition: Parser[Any] = (
      booleanTerm
    | searchCondition ~ "OR".ci ~ booleanTerm
  )

  lazy val booleanTerm: Parser[Any] = (
      booleanFactor
    | booleanTerm ~ "AND".ci ~ booleanFactor
  )

  lazy val booleanFactor: Parser[Any] =
    "NOT".ci.? ~ booleanTest

  lazy val booleanTest: Parser[Any] =
    booleanPrimary ~ ("IS".ci ~ "NOT".ci.? ~ truthValue).?

  lazy val booleanPrimary: Parser[Any] =
    predicate | l("(") ~ searchCondition ~ l(")")

  lazy val predicate: Parser[Any] = (
      comparisonPredicate
    | betweenPredicate
    | inPredicate
    | likePredicate
    | nullPredicate
    | quantifiedComparisonPredicate
    | existsPredicate
    | matchPredicate
    | overlapsPredicate
  )

  lazy val comparisonPredicate: Parser[Any] =
    rowValueConstructor ~ compOp ~ rowValueConstructor

  lazy val rowValueConstructor: Parser[Any] = (
      rowValueConstructorElement
    | l("(") ~ rowValueConstructorList ~ l(")")
    | rowSubquery
  )

  lazy val rowValueConstructorElement: Parser[Any] = (
      valueExpression
    | nullSpecification
    | defaultSpecification
  )

  lazy val valueExpression: Parser[Any] = (
      numericValueExpression
    | stringValueExpression
    | datetimeValueExpression
    | intervalValueExpression
  )

  lazy val numericValueExpression: Parser[Any] = (
      term
    | numericValueExpression ~ "+" ~ term
    | numericValueExpression ~ "-" ~ term
  )

  lazy val term: Parser[Any] = (
      factor
    | term ~ "*" ~ factor
    | term ~ "/" ~ factor
  )

  lazy val factor: Parser[Any] =
    sign.? ~ numericPrimary

  lazy val numericPrimary: Parser[Any] =
    valueExpressionPrimary | numericValueFunction

  lazy val valueExpressionPrimary: Parser[Any] = (
      unsignedValueSpecification
    | columnReference
    | setFunctionSpecification
    | scalarSubquery
    | caseExpression
    | l("(") ~ valueExpression ~ l(")")
    | castSpecification
  )

  lazy val unsignedValueSpecification: Parser[Any] =
    unsignedLiteral | generalValueSpecification

  lazy val unsignedLiteral: Parser[Any] =
    unsignedNumericLiteral | generalLiteral

  // NB: USER expressions and variables omitted.
  lazy val generalValueSpecification: Parser[Any] = (
      parameterSpecification
    | dynamicParameterSpecification
  )

  lazy val parameterSpecification: Parser[Any] =
    parameterName ~ indicatorParameter.?

  lazy val parameterName: Parser[Any] =
    l(":") ~ identifier

  lazy val indicatorParameter: Parser[Any] =
    "INDICATOR".ci.? ~ parameterName

  lazy val dynamicParameterSpecification: Parser[Any] =
    "?".ci

  lazy val columnReference: Parser[Any] =
    (qualifier ~ ".").? ~ columnName

  lazy val qualifier: Parser[Any] =
    tableName | correlationName

  lazy val correlationName: Parser[Any] =
    identifier

  lazy val setFunctionSpecification: Parser[Any] = (
      "COUNT".ci ~ "(" ~ "*" ~ ")"
    | generalSetFunction
  )

  lazy val generalSetFunction: Parser[Any] =
    setFunctionType ~ "(" ~ setQuantifier.? ~ valueExpression ~ ")"

  lazy val setFunctionType: Parser[Any] =
    "AVG".ci | "MAX".ci | "MIN".ci | "SUM".ci | "COUNT".ci

  lazy val setQuantifier: Parser[Any] =
    "DISTINCT".ci | "ALL".ci


  // Queries

  lazy val scalarSubquery: Parser[Any] =
    subquery

  lazy val subquery: Parser[Any] =
    l("(") ~ queryExpression ~ l(")")

  lazy val queryExpression: Parser[Any] = (
      nonJoinQueryExpression
    | joinedTable
  )

  lazy val nonJoinQueryExpression: Parser[Any] = (
      nonJoinQueryTerm
    | queryExpression ~ "UNION".ci ~ "ALL".ci.? ~ correspondingSpec.? ~ queryTerm
    | queryExpression ~ "EXCEPT".ci ~ "ALL".ci.? ~ correspondingSpec.? ~ queryTerm
  )

  lazy val nonJoinQueryTerm: Parser[Any] = (
      nonJoinQueryPrimary
    | queryTerm ~ "INTERSECT".ci ~ "ALL".ci.? ~ correspondingSpec.? ~ queryPrimary
  )

  lazy val nonJoinQueryPrimary: Parser[Any] = (
      simpleTable
    | l("(") ~ nonJoinQueryExpression ~ l(")")
  )

  lazy val simpleTable: Parser[Any] = (
      querySpecification
    | tableValueConstructor
    | explicitTable
  )

  lazy val querySpecification: Parser[Any] =
    "SELECT".ci ~ setQuantifier.? ~ selectList ~ tableExpression

  lazy val selectList: Parser[Any] = (
      l("*")
    | selectSublist ~ (l(",") ~ selectSublist).*
  )

  lazy val selectSublist: Parser[Any] =
    derivedColumn | qualifier ~ "." ~ "*"

  lazy val derivedColumn: Parser[Any] =
    valueExpression ~ asClause.?

  lazy val asClause: Parser[Any] =
    "AS".ci ~ columnName

  lazy val tableExpression: Parser[Any] = (
      fromClause
    | whereClause
    | groupByClause
    | havingClause
  )

  lazy val fromClause: Parser[Any] =
    "FROM".ci ~ tableReference ~ (l(",") ~ tableReference).*

  lazy val tableReference: Parser[Any] = (
      tableName ~ correlationSpecification.?
    | derivedTable ~ correlationSpecification
    | joinedTable
  )

  lazy val correlationSpecification: Parser[Any] =
    "AS".ci.? ~ correlationName ~ (l("(") ~ derivedColumnList ~ l(")")).?

  lazy val derivedColumnList: Parser[Any] =
    columnNameList

  lazy val derivedTable: Parser[Any] =
    tableSubquery

  lazy val tableSubquery: Parser[Any] =
    subquery

  lazy val joinedTable: Parser[Any] = (
      crossJoin
    | qualifiedJoin
    | l("(") ~ joinedTable ~ l(")")
  )

  lazy val crossJoin: Parser[Any] =
    tableReference ~ "CROSS".ci ~ "JOIN".ci ~ tableReference

  lazy val qualifiedJoin: Parser[Any] =
    tableReference ~ "NATURAL".ci.? ~ joinType.? ~ "JOIN".ci ~ tableReference ~ joinSpecification.?

  lazy val joinType: Parser[Any] = (
      "INNER".ci
    | outerJoinType ~ "OUTER".ci.?
    | "UNION".ci
  )

  lazy val outerJoinType: Parser[Any] =
    "LEFT".ci | "RIGHT".ci | "FULL".ci

  lazy val joinSpecification: Parser[Any] =
    joinCondition | namedColumnsJoin

  lazy val joinCondition: Parser[Any] =
    "ON".ci ~ searchCondition

  lazy val namedColumnsJoin: Parser[Any] =
    "USING".ci ~ (l("(") ~ joinColumnList ~ l(")"))

  lazy val joinColumnList: Parser[Any] =
    columnNameList

  lazy val whereClause: Parser[Any] =
    "WHERE".ci ~ searchCondition

  lazy val groupByClause: Parser[Any] =
    "GROUP".ci ~ "BY".ci ~ groupingColumnReferenceList

  lazy val groupingColumnReferenceList: Parser[Any] =
    groupingColumnReference ~ (l(",") ~ groupingColumnReference).*

  lazy val groupingColumnReference: Parser[Any] =
    columnReference ~ collateClause.?

  lazy val collateClause: Parser[Any] =
    "COLLATE".ci ~ collationName

  lazy val collationName: Parser[Any] =
    qualifiedName

  lazy val havingClause: Parser[Any] =
    "HAVING".ci ~ searchCondition

  lazy val tableValueConstructor: Parser[Any] =
    "VALUES".ci ~ tableValueConstructorList

  lazy val tableValueConstructorList: Parser[Any] =
    rowValueConstructor ~ (l(",") ~ rowValueConstructor).*

  lazy val explicitTable: Parser[Any] =
    "TABLE".ci ~ tableName

  lazy val queryTerm: Parser[Any] =
    nonJoinQueryTerm | joinedTable

  lazy val correspondingSpec: Parser[Any] =
    "CORRESPONDING".ci ~ ("BY".ci ~ (l("(") ~ correspondingColumnList ~ l(")")))

  lazy val correspondingColumnList: Parser[Any] =
    columnNameList

  lazy val queryPrimary: Parser[Any] =
    nonJoinQueryPrimary | joinedTable


  // Query expression components

  lazy val caseExpression: Parser[Any] =
    caseAbbreviation | caseSpecification

  lazy val caseAbbreviation: Parser[Any] = (
      "NULLIF".ci ~ "(" ~ valueExpression ~ "," ~ valueExpression ~ ")"
    | "COALESCE".ci ~ "(" ~ valueExpression ~ (l(",") ~ valueExpression).+ ~ ")"
  )

  lazy val caseSpecification: Parser[Any] =
    simpleCase | searchedCase

  lazy val simpleCase: Parser[Any] =
    "CASE".ci ~ caseOperand ~ simpleWhenClause.+ ~ elseClause.? ~ "END".ci

  lazy val caseOperand: Parser[Any] =
    valueExpression

  lazy val simpleWhenClause: Parser[Any] =
    "WHEN".ci ~ whenOperand ~ "THEN".ci ~ result

  lazy val whenOperand: Parser[Any] =
    valueExpression

  lazy val result: Parser[Any] =
    resultExpression | "NULL".ci

  lazy val resultExpression: Parser[Any] =
    valueExpression

  lazy val elseClause: Parser[Any] =
    "ELSE".ci ~ result

  lazy val searchedCase: Parser[Any] =
    "CASE".ci ~ searchedWhenClause.+ ~ elseClause.? ~ "END".ci

  lazy val searchedWhenClause: Parser[Any] =
    "WHEN".ci ~ searchCondition ~ "THEN".ci ~ result

  lazy val castSpecification: Parser[Any] =
    "CAST".ci ~ "(" ~ castOperand ~ "AS".ci ~ castTarget ~ ")"

  lazy val castOperand: Parser[Any] =
    valueExpression | "NULL".ci

  lazy val castTarget: Parser[Any] =
    domainName | dataType

  lazy val numericValueFunction: Parser[Any] =
    positionExpression | extractExpression | lengthExpression

  lazy val positionExpression: Parser[Any] =
    "POSITION".ci ~ "(" ~ characterValueExpression ~ "IN".ci ~ characterValueExpression ~ ")"

  lazy val characterValueExpression: Parser[Any] =
    concatenation | characterFactor

  lazy val concatenation: Parser[Any] =
    characterValueExpression ~ concatenationOperator ~ characterFactor

  lazy val characterFactor: Parser[Any] =
    characterPrimary | collateClause.?

  lazy val characterPrimary: Parser[Any] =
    valueExpressionPrimary | stringValueFunction

  lazy val stringValueFunction: Parser[Any] =
    characterValueFunction | bitValueFunction

  lazy val characterValueFunction: Parser[Any] = (
      characterSubstringFunction
    | fold
    | formOfUseConversion
    | characterTranslation
    | trimFunction
  )

  lazy val characterSubstringFunction: Parser[Any] =
    "SUBSTRING".ci ~ "(" ~ characterValueExpression ~ "FROM".ci ~ startPosition ~ ("FOR".ci ~ stringLength).? ~ ")"

  lazy val startPosition: Parser[Any] =
    numericValueExpression

  lazy val stringLength: Parser[Any] =
    numericValueExpression

  lazy val fold: Parser[Any] =
    ("UPPER".ci | "LOWER".ci) ~ "(" ~ characterValueExpression ~ ")"

  lazy val formOfUseConversion: Parser[Any] =
    "CONVERT".ci ~ "(" ~ characterValueExpression ~ "USING".ci ~ formOfUseConversionName ~ ")"

  lazy val formOfUseConversionName: Parser[Any] =
    qualifiedName

  lazy val characterTranslation: Parser[Any] =
    "TRANSLATE".ci ~ "(" ~ characterValueExpression ~ "USING" ~ translationName ~ ")"

  lazy val translationName: Parser[Any] =
    qualifiedName

  lazy val trimFuntion: Parser[Any] =
    "TRIM".ci ~ "(" ~ trimOperands ~ ")"

  lazy val trimOperands: Parser[Any] =
    (trimSpecification.? ~ trimCharacter.? ~ "FROM".ci).? ~ trimSource

  lazy val trimSpecification: Parser[Any] =
    "LEADING".ci | "TRAILING".ci | "BOTH".ci

  lazy val trimCharacter: Parser[Any] =
    characterValueExpression

  lazy val trimSource: Parser[Any] =
    characterValueExpression

  lazy val bitValueFunction: Parser[Any] =
    bitSubstringFunction

  lazy val bitSubstringFunction: Parser[Any] =
    "SUBSTRING".ci ~ "(" ~ bitValueExpression ~ "FROM".ci ~ startPosition ~ ("FOR".ci ~ stringLength).? ~ ")"

  lazy val bitValueExpression: Parser[Any] =
    bitConcatenation | bitFactor

  lazy val bitConcatenation: Parser[Any] =
    bitValueExpression ~ concatenationOperator ~ bitFactor

  lazy val bitFactor: Parser[Any] =
    bitPrimary

  lazy val bitPrimary: Parser[Any] =
    valueExpressionPrimary | stringValueFunction

  lazy val extractExpression: Parser[Any] =
    "EXTRACT".ci ~ "(" ~ extractField ~ "FROM".ci ~ extractSource ~ ")"

  lazy val extractField: Parser[Any] =
    datetimeField | timeZoneField

  lazy val datetimeField: Parser[Any] =
    nonSecondDatetimeField | "SECOND".ci

  lazy val truthValue: Parser[Any] =
    "TRUE".ci | "FALSE".ci | "UNKNOWN".ci
}

