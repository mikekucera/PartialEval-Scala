package pe.flowchart

import pe.flowchart.FlowChartSyntax._
import pe.flowchart.FlowChartInterpreter._

import scala.collection.immutable.SortedMap


/**
 * First crack at the flow chart specializer.
 * 
 * TODO: correct access modifiers
 * TODO: Syntax.pretty needs to be updated with new list syntax
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
  
  implicit class DivOps(val division: Division) extends AnyVal {
    def isStatic(v: String) = division contains v
    def isDynamic(v: String) = !isStatic(v)
    def isStatic(expr: Expression): Boolean = expr match {
	  case Var(v) => isStatic(v)
	  case Unary(_, e) => isStatic(e)
	  case Binary(_, e1, e2) => isStatic(e1) && isStatic(e2)
	  case _ => true
	}
  }
   
  
  def isLiteral(expr: Expression) = expr match {
    case Str(_) | Lst (_) => true
    case _ => false
  }
  
  
  def blocks(program: Program): Blocks = 
    for((label, l :: ls) <- programTails(program)) 
      yield (label, l :: (ls takeWhile (_.label.isEmpty)))
  
  
  
  
  
  def reduce(expr: Expression, div: Division, env: StaticEnv): Expression = expr match {
    case s:Str => s
    case l:Lst => l
    case Var(v) => if(div.isStatic(v)) env(v) else Var(v)
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
        case Assign(name, expr) if div.isStatic(name) => 
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
          
        case IfGotoElse(e1, e2, l1, l2) if div.isStatic(e1) && div.isStatic(e2) =>
          val label = if(eval(e1)(env) == eval(e2)(env)) l1 else l2
          generate(blocks(label), residualLines, env)
          
        case IfGotoElse(e1, e2, l1, l2) =>
          val r1 = reduce(e1, div, env)
          val r2 = reduce(e2, div, env)
          val lab1 = tempLabel(l1, env)
          val lab2 = tempLabel(l2, env)
          val residual = Line(command = IfGotoElse(r1,r2,lab1,lab2))
          (residual :: residualLines, (l1,env) :: (l2,env) :: Nil)
      }
    }
    
    val startBlock = blocks(label)
    val (residualLines, successors) = generate(startBlock, Nil, env)
    (residualLines.reverse, successors)
  }
  
  
  def tempLabel(pp:ProgramPoint):String = pp match {
    case (label, env) => label + "$" + java.lang.Long.toString(env.hashCode & 0xFFFFFFFFL, 16)
  }
  
  
  def tempLabelForBlock(block: List[Line], pp: ProgramPoint) = block match {
    case Nil => Nil
    case Line(_, c) :: t => Line(tempLabel(pp), c) :: t
  }
  
  
  def mix(blocks: Blocks, division: Division, pending: List[ProgramPoint], marked: List[ProgramPoint]): List[Line] = pending match {
    case Nil => Nil
    case (pp @ (label, env)) :: pendingTail =>
      if(marked contains pp)
        mix(blocks, division, pendingTail, marked)
      else {
        val (residualBlock, successors) = generateResidualBlock(label, blocks, division, env)
        tempLabelForBlock(residualBlock, pp) ++ mix(blocks, division, pendingTail ++ successors, pp::marked)
      }
  }
  
  
  def relabel(lines: List[Line]): List[Line] = {
    type LabelMap = Map[String,Map[String,Int]]
    
    def relabelLines(lines: List[Line], labelMap: LabelMap): List[Line] = lines match {
      case Nil => Nil
      case Line(label, command) :: rest =>
        val (newLabel, labelMap2) = updateCounters(label, labelMap)
        val (newCommand, labelMap3) = relabelCommand(command, labelMap2)
        Line(newLabel, newCommand) :: relabelLines(rest, labelMap3)
    } 
    
    def relabelCommand(command: Command, labels: LabelMap): (Command, LabelMap) = command match {
      case Goto(label) =>
        val (newLabel, labels2) = updateCounters(label, labels)
        (Goto(newLabel), labels2)
      case IfGotoElse(e1, e2, goto, els) =>
        val (newGoto, labels2) = updateCounters(goto, labels)
        val (newElse, labels3) = updateCounters(els,  labels2)
        (IfGotoElse(e1, e2, newGoto, newElse), labels3)
      case command => (command, labels)
    }
    
    def updateCounters(label: String, labels: LabelMap): (String, LabelMap) = label.split('$') match {
      case Array(name) => (name, labels)
      case Array(name,hash) => 
        if(labels contains name) {
          val hashes = labels(name)
          if(hashes contains hash)
            (name + "_" + hashes(hash), labels)
          else
            (name + "_" + hashes.size + 1, labels + (name -> (hashes + (hash -> (hashes.size + 1)))))
        }
        else (name + "_" + 1, labels + (name -> Map(hash -> 1)))
    }
    
    relabelLines(lines, Map())
  }
  
  
  def computeDivision(program: Program, staticInput: StaticEnv) = {
    val assigns = allAssigns(program)
    val programVars = assigns.map(_.name)
    val initialDivision = staticInput.keySet ++ programVars.filter(!program.read.names.contains(_)) // all program variables are initially marked as static
    
    def iterate(div: Division): Division = {
      val newDiv = nextDivision(div)
      if(div == newDiv) newDiv else iterate(newDiv)
    }
    
    def nextDivision(div: Division): Division = {
      assigns.foldLeft(div) {
        case (div, Assign(n, expr)) if varsIn(expr).exists(div.isDynamic) => div - n
        case (div, _) => div  
      }
    }
    
    iterate(initialDivision) // find the fixed point
  }
  
  
  def varsIn(expr: Expression): List[String] = expr match {
     case Str(_) | Lst(_) => Nil
     case Var(name) => List(name)
     case Unary(_, expr) => varsIn(expr)
     case Binary(_, left, right) => varsIn(left) ++ varsIn(right) // performance doesn't really matter here
  }
  
  
  /** @return All the assignment statements in the program as a List[Assign]. */
  def allAssigns(program: Program): List[Assign] =
    program.lines.collect {
      case Line(_, Assign(n,e)) => Assign(n,e)
    }
  
  
  def residualReads(read: Read, div: Division) = Read(read.names.filter(div.isDynamic))
  
  
  def specialize(program: Program, staticInput: StaticEnv): Program = {
    val division = computeDivision(program, staticInput)
    val firstPP = (program.lines.head.label, staticInput)
    val bls = blocks(program)
    val residual = mix(bls, division, List(firstPP), Nil)
    val read = residualReads(program.read, division)
    Program(read, relabel(residual))
  }
}
