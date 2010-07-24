package parsing.test

import org.scalatest.FunSuite
import parsing.ast.{Location,SimpleLocation,ArrayLocation}

/**
 * Parsing tests for location expressions
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class LocationTests extends ParserTest[Location] with FunSuite{
  val parser = location

  test("Location expressions"){
    input = "ident"
    result must be (Some(SimpleLocation("ident",None)))

    input = "ident[index]"
    result must be (Some(ArrayLocation("ident",SimpleLocation("index"))))

    input = "ident.member"
    val memberLocation = SimpleLocation("member")
    result must be (Some(SimpleLocation(
	"ident",Some(memberLocation)
    )))

    input = "ident[index].member"
    result must be (Some(
      ArrayLocation("ident",SimpleLocation("index"),Some(memberLocation))
    ))

    input = "ident[ident2[index2]]"
    result must be (
      Some(
	ArrayLocation(
	  "ident", ArrayLocation(
	    "ident2",SimpleLocation("index2")
	  )
	)
      )
    )
    
    input = "ident.member.member2"
    result must be (
      Some(
	SimpleLocation(
	  "ident",Some(
	    SimpleLocation("member",Some(SimpleLocation("member2")))
	  )
	)
      )
    )
    
    input = "ident[index].member.member2"
    result must be (
      Some(
	ArrayLocation(
	  "ident",SimpleLocation("index"),Some(
	    SimpleLocation("member",Some(SimpleLocation("member2")))
	  )
	)
      )
    )    
  }
}
