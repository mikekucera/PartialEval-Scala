package pe.flowchart

import pe.flowchart.FlowChartSyntax._
import pe.flowchart.FlowChartInterpreter._

import scala.collection.immutable.SortedMap


/**
 * First crack at the flow chart specializer.
 * 
 * TODO: compute a division properly
 */
object FlowChartSpecializer {
  
  type StaticEnv = Map[String, Value]
  type ProgramPoint = (String, StaticEnv)
  type Division = Set[String]
  type Blocks = Map[String, List[Line]]
  
  
  implicit def embedValue(value: Value): Expression = value match {
    case StringValue(s) => Str(s)
    case ListValue(l) => Lst(l)
  }
   
  
  def isLiteral(expr: Expression) = expr match {
    case Str(_) | Lst (_) => true
    case _ => false
  }
  
  
  def blocks(program: Program): Blocks = 
    for((label, l :: ls) <- programTails(program)) 
      yield (label, l :: (ls takeWhile (_.label.isEmpty)))
  
  
  def isExprStatic(expr: Expression, div: Division): Boolean = expr match {
    case Var(v) => div contains v
    case Unary(_, e) => isExprStatic(e, div)
    case Binary(_, e1, e2) => isExprStatic(e1, div) && isExprStatic(e2, div)
    case _ => true
  }
  
  
  def reduce(expr: Expression, div: Division, env: StaticEnv): Expression = expr match {
    case s:Str => s
    case l:Lst => l
    case Var(v) => if(div contains v) env(v) else Var(v)
    case Unary(op,e) =>
      val r = reduce(e, div, env)
      if(isLiteral(r)) eval(Unary(op, r))(env) else Unary(op, r)
    case Binary(op, e1, e2) =>
      val r1 = reduce(e1, div, env)
      val r2 = reduce(e2, div, env)
      if(isLiteral(r1) && isLiteral(r2)) eval(Binary(op, r1, r2))(env) else Binary(op, r1, r2)
  }
  
  
  def generateResidualBlock(label: String, blocks: Blocks, div: Division, env: StaticEnv) = {
    def generate(lines: List[Line], residualLines: List[Line], env: StaticEnv): (List[Line], List[ProgramPoint]) = lines match {
      case Nil => (residualLines, Nil)
      case Line(_,command) :: rest => command match {
        case Assign(name, expr) if div contains name => 
          val newEnv = env + (name -> eval(expr)(env))
          generate(rest, residualLines, newEnv)
        case Assign(name, expr) =>
          val reduced = Line(command = Assign(name, reduce(expr, div, env)))
          generate(rest, reduced :: residualLines, env)
        case Return(expr) =>
          val reduced = Line(command = Return(reduce(expr, div, env)))
          (reduced :: residualLines, Nil)
        case Goto(label) =>
          generate(blocks(label), residualLines, env)
        case IfGotoElse(e1, e2, l1, l2) if isExprStatic(e1, div) && isExprStatic(e2, div) =>
          val label = if(eval(e1)(env) == eval(e2)(env)) l1 else l2
          generate(blocks(label), residualLines, env)
        case IfGotoElse(e1, e2, l1, l2) =>
          val r1 = reduce(e1, div, env)
          val r2 = reduce(e2, div, env)
          val lab1 = genLabel(l1, env)
          val lab2 = genLabel(l2, env)
          val residual = Line(command = IfGotoElse(r1,r2,lab1,lab2))
          (residual :: residualLines, (lab1,env) :: (lab2,env) :: Nil)
      }
    }
    val startBlock = blocks(label)
    val (residualLines, successors) = generate(startBlock, Nil, env)
    (residualLines.reverse, successors)
  }
  
  
  def genLabel(pp:ProgramPoint):String = pp match {
    case (label, env) => label + "$" + java.lang.Long.toString(env.hashCode & 0xFFFFFFFFL, 16)
  }
  
  
  def relabelBlock(block: List[Line], pp: ProgramPoint) = block match {
    case Nil => Nil
    case Line(_, c) :: t => Line(genLabel(pp), c) :: t
  }
  
  
  def mix(blocks: Blocks, division: Division, pending: List[ProgramPoint], marked: List[ProgramPoint]): List[Line] = pending match {
    case Nil => Nil
    case (pp @ (label, env)) :: pendingTail => 
      if(marked contains pp)
        mix(blocks, division, pendingTail, marked)
      else {
        val (residualBlock, successors) = generateResidualBlock(label, blocks, division, env)
        relabelBlock(residualBlock, pp) ++ mix(blocks, division, pendingTail ++ successors, pp::marked)
      }
  }
  
  
  def relabel(lines: List[Line]): List[Line] = {
    def relabelLines(lines: List[Line], counters: Map[String,Map[String,Int]]): List[Line] = lines match {
      case Nil => Nil
      case Line("",   command) :: rest => Line("", command) :: relabelLines(rest, counters)
      case Line(label,command) :: rest => {
        val Array(name,hash) = label.split('$')
        if(counters contains name) {
          val hashes = counters(name)
          if(hashes contains hash) {
            Line(name + "_" + hashes(hash), command) :: relabelLines(rest, counters)
          }
          else {
            val n = hashes.size + 1
            Line(name + "_" + n, command) :: relabelLines(rest, counters + (name -> (hashes + (hash -> n))))
          }
        }
        else
          Line(name + "_" + 1, command) :: relabelLines(rest, counters + (name -> Map(hash -> 1)))
      }
    } 
    relabelLines(lines, Map())
  }
  
  
  def residualReads(read: Read, div: Division) = Read(read.names.filter(!div.contains(_)))
  
  def computeDivision(program: Program, staticInput: StaticEnv) = (for((k,v) <- staticInput) yield k).toSet  // TODO compute the division properly
  
  
  def specialize(program: Program, staticInput: StaticEnv): Program = {
    val division = computeDivision(program, staticInput)
    val firstPP = (program.lines.head.label, staticInput)
    val bls = blocks(program)
    val residual = mix(bls, division, List(firstPP), Nil)
    val read = residualReads(program.read, division)
    Program(read, relabel(residual))
  }
}
