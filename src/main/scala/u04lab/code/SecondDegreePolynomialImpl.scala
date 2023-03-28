package u04lab.code

class SecondDegreePolynomialImpl(override val constant: Double,
                                 override val firstDegree: Double,
                                 override val secondDegree: Double) extends SecondDegreePolynomial:
  override def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    val c = constant + polynomial.constant
    val f = firstDegree + polynomial.firstDegree
    val s = secondDegree + polynomial.secondDegree
    SecondDegreePolynomial(c,f,s)

  override def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    val c = constant - polynomial.constant
    val f = firstDegree - polynomial.firstDegree
    val s = secondDegree - polynomial.secondDegree
    SecondDegreePolynomial(c,f,s)


