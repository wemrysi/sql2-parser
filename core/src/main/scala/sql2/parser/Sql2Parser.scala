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

import scala.Any
// FIXME: What is required for "foo".r support?
import scala.Predef._
import scala.util.matching.Regex

/** Strict superset of SQL-92.
  *
  * SQL-92 parsing ported from the following BNF grammar
  * https://ronsavage.github.io/SQL/sql-92.bnf.html
  */
object Sql2Parser {

  implicit final class CaseInsensitivePattern(val s: String) extends scala.AnyVal {
    def ci: Regex = ("(?i)" + s).r
  }

  // Literal Numbers, Strings, Dates and Times

  lazy val identifier: Parser[Any] =
    ()


  // SQL Module

  lazy val columnName: Parser[Any] =
    identifier


  // Literals

  lazy val literal: Parser[Any] =
    ()


  // Search Conditions

  lazy val searchCondition: Parser[Any] =
    ()

  lazy val setQuantifier: Parser[Any] =
    "DISTINCT".ci | "ALL".ci


  // Queries

  lazy val scalarSubquery: Parser[Any] =
    subquery

  lazy val subquery: Parser[Any] = (
      "(" ~> queryExpression <~ ")"
  )

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
    | "(" ~> nonJoinQueryExpression <~ ")"
  )

  lazy val simpleTable: Parser[Any] = (
      querySpecification
    | tableValueConstructor
    | explicitTable
  )

  lazy val querySpecification: Parser[Any] =
    "SELECT".ci ~ setQuantifier.? ~ selectList ~ tableExpression

  lazy val selectList: Parser[Any] =
    "*" | selectSublist ~ ("," ~ selectSublist).*

  lazy val selectSublist: Parser[Any] =
    derivedColumn | qualifier ~ ".*"

  lazy val derivedColumn: Parser[Any] =
    valueExpression ~ asClause.?

  lazy val asClause: Parser[Any] =
    "as".ci ~ columnName
}

