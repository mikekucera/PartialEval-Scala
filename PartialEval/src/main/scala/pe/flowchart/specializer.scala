package pe.flowchart

import pe.flowchart.FlowChartSyntax._
import pe.flowchart.FlowChartInterpreter._

import scala.collection.immutable.SortedMap


/**
 * First crack at the flow chart specializer.
 * 
 * TODO: generate proper labels
 * TODO: relabel the output program
 * TODO: compute a division properly
 * TODO: compute new 'read' list for program
 */
object FlowChartSpecializer {
  
  type StaticEnv = SortedMap[String, Value] // sorted makes it easier to generate new labels (for now)
  type ProgramPoint = (String, StaticEnv)
  type Division = Set[String]
  type Blocks = Map[String, List[Line]]
  
  
  implicit def embedValue(value:Value):Expression = value match {
    case StringValue(s) => Str(s)
    case ListValue(l) => Lst(l)
  }
   
  implicit class DivCheck(name:String) {
    def isStatic(implicit div:Division) = div contains name 
  }
    
  def isLiteral(expr:Expression) = expr match {
    case Str(_) | Lst (_) => true
    case _ => false
  }
  
  
  def blocks(program:Program): Blocks = 
    for((label, l :: ls) <- programTails(program)) yield (label, l :: (ls takeWhile (_.label.isEmpty)))
  
  
  def isExprStatic(expr:Expression)(implicit division:Division): Boolean = expr match {
    case Var(v) => v.isStatic
    case Unary(_, e) => isExprStatic(e)
    case Binary(_, e1, e2) => isExprStatic(e1) && isExprStatic(e2)
    case _ => true
  }
  
  
  def reduce(expr:Expression)(implicit division:Division, env:StaticEnv): Expression = expr match {
    case s:Str => s
    case l:Lst => l
    case Var(v) => if(v.isStatic) env(v) else Var(v)
    case Unary(op,e) =>
      val r = reduce(e)
      if(isLiteral(r)) eval(Unary(op, r)) else Unary(op, r)
    case Binary(op, e1, e2) =>
      val r1 = reduce(e1)
      val r2 = reduce(e2)
      if(isLiteral(r1) && isLiteral(r2)) eval(Binary(op, r1, r2)) else Binary(op, r1, r2)
  }
  
  
  def generateResidualBlock(label: String, blocks: Blocks, division:Division, env:StaticEnv): (List[Line], List[ProgramPoint]) = {
    implicit val div = division
    def generate(lines:List[Line], residualLines:List[Line])(implicit env:StaticEnv): (List[Line], List[ProgramPoint]) = lines match {
      case Nil => (residualLines, Nil)
      case Line(_,command) :: rest => command match {
        case Assign(name, expr) if name.isStatic => 
          val newEnv = env + (name -> eval(expr))
          generate(rest, residualLines)(newEnv)
        case Assign(name, expr) =>
          val reduced = Line(command = Assign(name, reduce(expr)))
          generate(rest, reduced :: residualLines)
        case Return(expr) =>
          val reduced = Line(command = Return(reduce(expr)))
          (reduced :: residualLines, Nil)
        case Goto(label) =>
          generate(blocks(label), residualLines)
        case IfGotoElse(e1, e2, l1, l2) if isExprStatic(e1) && isExprStatic(e2) =>
          val label = if(eval(e1) == eval(e2)) l1 else l2
          generate(blocks(label), residualLines)
        case IfGotoElse(e1, e2, l1, l2) =>
          val r1 = reduce(e1)
          val r2 = reduce(e2)
          val lab1 = genLabel(l1, env)
          val lab2 = genLabel(l2, env)
          val residual = Line(command = IfGotoElse(r1,r2,lab1,lab2))
          (residual :: residualLines, (lab1,env) :: (lab2,env) :: Nil)
      }
    }
    val startBlock = blocks(label)
    val (residualLines, successors) = generate(startBlock, Nil)(env)
    (residualLines.reverse, successors)
  }
  
  
  def genLabel(pp:ProgramPoint):String = pp match {
    case (label, env) => label + "$" + java.lang.Long.toString(env.hashCode & 0xFFFFFFFFL, 16)
  }
  
  def relabelBlock(block: List[Line], pp:ProgramPoint) = block match {
    case Nil => Nil
    case Line(_, c) :: t => Line(genLabel(pp), c) :: t
  }
  
  
  def mix(blocks:Blocks, division:Division, pending:List[ProgramPoint], marked:List[ProgramPoint]): List[Line] = pending match {
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
    def relabelLines(lines: List[Line], counters:Map[String,Map[String,Int]]): List[Line] = lines match {
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
  
  
  def computeDivision(program: Program, staticInput: StaticEnv) = (for((k,v) <- staticInput) yield k).toSet  // TODO compute the division propery
  
  def specialize(program: Program, staticInput: StaticEnv): Program = {
    val division = computeDivision(program, staticInput)
    val firstPP = (program.lines.head.label, staticInput)
    val bls = blocks(program)
    val residual = mix(bls, division, List(firstPP), List())
    Program(program.read, relabel(residual)) // TODO need to compute new 'read'
  }
}
