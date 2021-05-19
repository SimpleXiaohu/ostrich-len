/*
 * This file is part of Sloth, an SMT solver for strings.
 * Copyright (C) 2017  Philipp Ruemmer, Petr Janku
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package strsolver
import java.util.Date
import ap._
import ap.parameters.PreprocessingSettings
import ap.parser._
import ap.theories.TheoryRegistry
import ap.util.CmdlParser
import strsolver.preprop.RRFunsToTransducer

import scala.collection.mutable.ArrayBuffer


object SMTLIBMain {

  class MainException(msg : String) extends Exception(msg)
  object TimeoutException extends MainException("timeout")
  object StoppedException extends MainException("stopped")

  def doMain(args: Array[String], flags:Flags) : String = try {
    val filenames = new ArrayBuffer[String]
    var timeout : Option[Long] = None
    var model = false
    var assertions = false

    for (str <- args) str match {
      case CmdlParser.ValueOpt("strategy", value) =>{
        flags.strategy = value
      }
      case CmdlParser.ValueOpt("tmpfile", value) =>{
        flags.tmpFileName = value
      }
      case CmdlParser.ValueOpt("window", value) => {
        flags.windowSize = value
      }
      case CmdlParser.ValueOpt("timeout", value) =>
        timeout = Some(value.toLong * 1000)
      case CmdlParser.ValueOpt("nuxmvtime", value) =>
        flags.nuxmvTime = value
      case CmdlParser.Opt("model", value) =>
        model = value
      case CmdlParser.Opt("assert", value) =>
        assertions = value
      case CmdlParser.Opt("useparikh", _) =>
        flags.useParikh = true
      case str =>
        filenames += str
    }

    if (filenames.size != 1)
      throw new Exception("expected a single filename as argument")

    val startTime = System.currentTimeMillis

    val timeoutChecker = timeout match {
      case Some(to) => () => {
        if (System.currentTimeMillis - startTime > to)
          throw TimeoutException
      }
      case None => () =>
    }

    ap.util.Debug.enableAllAssertions(assertions)

    ////////////////////////////////////////////////////////////////////////////

    val fileName = filenames.head
//debug----------------
    Console.err.println("Reading file " + fileName + " ...")

    val reader = new SMTReader(fileName)
    if (reader.includesGetModel)
      model = true

    // just handle the problem using a normal solver
    val functionEnc =
      new FunctionEncoder (true, false)
    for (t <- reader.signature.theories)
      functionEnc addTheory t

    val (List(INamedPart(_, formula)), _, signature) =
    Preprocessing(reader.rawFormula,
      List(),
      reader.signature,
      PreprocessingSettings.DEFAULT,
      functionEnc)

    // tell the AFA store about introduced relations
   for ((p, f) <- functionEnc.predTranslation)
     if ((TheoryRegistry lookupSymbol f).isEmpty) {
       RRFunsToTransducer.addRel2Fun(p, f)
     }
    val stringTheory = new StringTheory(flags)

    val formulaWithoutQuans = SMTReader.eliminateUniQuantifiers(formula)

    val intFormula = StringTheoryTranslator(formulaWithoutQuans,
      reader.wordConstants,stringTheory)
    ap.util.Timer.reset
    SimpleAPI.withProver(enableAssert = assertions) { p =>
      import SimpleAPI._
      // import strsolver.preprop.StoreLC

      import p._
      addTheory(stringTheory)
      addConstantsRaw(SymbolCollector constantsSorted intFormula)
      addRelations(for (p <- signature.order.orderedPredicates.toSeq sortBy (_.name);
                        if ((TheoryRegistry lookupSymbol p).isEmpty))
                   yield p)

      ??(intFormula)

//debug----------------
      Console.err.println
      val res = {
       checkSat(false)
       while (getStatus(100) == ProverStatus.Running)
         timeoutChecker()
        ???
      }
      res match {
        case ProverStatus.Valid => /*println("unsat")*/ "unsat"
        case ProverStatus.Inconclusive => /*println("unknown")*/ "unknown"
        case ProverStatus.OutOfMemory => /*println("OOM")*/ "OOM"
        case ProverStatus.Invalid => {
          /*println("sat")*/ "sat"
        }
      }
    }
  } catch {
    case t@(TimeoutException) => {
      println("timeout")
      sys.exit(0)
    }
    case t : Throwable => {
      println("(error \"" + t.getMessage + "\")")
      t.printStackTrace
      sys.exit(0)
    }
  }

  def main(args: Array[String]) : Unit = {
    val flag = new Flags
    val startTime : Double = new Date().getTime
    println(doMain(args, flag))
    val endTime : Double = new Date().getTime
    println("use "+(endTime-startTime)/1000.0)
  }

}
