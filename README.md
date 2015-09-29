# BinaryNat
Natural numbers in binary representation for type-level calculations in scala

Use it:

```scala
import com.awt.binnat._
```

Define numbers in binary form:

```scala
val x = _1 // x: _1 = bn1
val y = _1 _0 // y: _1 ## _0 = bn10
val z = _1 _0 _1 // z: _1 ## _0 ## _1 = bn101

x.asBigInt // res0: BigInt = 1
y.asBigInt // res1: BigInt = 2
z.asBigInt // res2: BigInt = 5
```

Check type constraints

```scala
implicitly[#+#.Aux[_1 ## _0, _1 ## _1, _1 ## _0 ## _1] ] //Ok
implicitly[#+#.Aux[_1 ## _0, _1 ## _1, _1 ## _1 ## _0] ] //Error
 

implicitly[( _1 ## _0 ## _1 ) #<# ( _1 ## _1 ## _0) ] //Ok
implicitly[( _1 ## _1 ## _1 ) #<# ( _1 ## _1 ## _0) ] //Error
```

Even more constraints

```scala
def isTriangle[A <: BNat, B <: BNat, C <: BNat, `A+B` <: BNat, `B+C` <: BNat, `A+C` <: BNat]
(a: A, b: B, c: C)
(implicit
 `a+b`: #+#.Aux[A, B, `A+B`],
 `b+c`: #+#.Aux[B, C, `B+C`],
 `a+c`: #+#.Aux[A, C, `A+C`],
  `c<a+b`: C #<# `A+B`,
  `b<a+c`: B #<# `A+C`,
  `a<b+c`: A #<# `B+C`) = true
  
val two = _1 _0
val three = _1 _1
val four = two + two
val five = two + three
val ten = _1 _0 _1 _0

isTriangle(three, four, five) //true
isTriangle(three, four, ten) //error: 
// could not find implicit value for parameter c$lessa$plusb:#<#[this.Out,A+B]
```

last one is pretty informative: could not prove that `c<a+b`




