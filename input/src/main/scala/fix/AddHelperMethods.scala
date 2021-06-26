/*
rule = AddHelperMethods
 */
package org.example

import simulacrum.addHelperMethods

@addHelperMethods sealed trait Command1[+T]
final case class Write1[T](value: T) extends Command1[T]
case object Read1 extends Command1[Nothing]

@addHelperMethods sealed trait Command2
final case class Write2[T](value: T) extends Command2
case object Read2 extends Command2
