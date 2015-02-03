import com.wordnik.swagger.config.SwaggerConfig
import com.wordnik.swagger.jaxrs.reader.DefaultJaxrsApiReader
import com.wordnik.swagger.model.{AllowableListValues, AnyAllowableValues}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import testresources.BeanParamTargetResource

/**
 * @author elwood
 * @since
 */
@RunWith(classOf[JUnitRunner])
class BeanParamTargetTest extends FlatSpec with Matchers {
  it should "lalala" in {
    val reader = new DefaultJaxrsApiReader
    val config = new SwaggerConfig()
    val apiResource = reader.read("/api-docs", classOf[BeanParamTargetResource], config).getOrElse(fail("should not be None"))

    val apis = apiResource.apis
    apis.size should be (1)

    val ops = apis.head.operations
    ops.size should be (1)
    val op = ops.head
    op.parameters.size should be (2)

    val qpt = op.parameters(0)
    qpt.name should be ("query")
    qpt.allowableValues should be (AllowableListValues(List("a", "b", "c")))
    qpt.dataType should be ("string")

    val id = op.parameters(1)
    id.name should be ("conds")
    id.allowableValues should be (AnyAllowableValues)
    id.dataType should be ("List[string]")
  }
}
