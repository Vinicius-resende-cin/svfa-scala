package br.unb.cic.soot

import br.unb.cic.soot.graph.{NodeType, SimpleNode, SinkNode, SourceNode}

class FieldTest extends JSVFATest {
  override def getClassName(): String = "samples.FieldSample"

  override def getMainMethod(): String = "main"

  override def analyze(unit: soot.Unit): NodeType = {
    if (unit.getJavaSourceStartLineNumber == 7) {
      return SourceNode
    }
    if (unit.getJavaSourceStartLineNumber == 8) {
      return SinkNode
    }
    return SimpleNode
  }
}
