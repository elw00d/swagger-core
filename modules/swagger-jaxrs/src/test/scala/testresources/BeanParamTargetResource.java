package testresources;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;

import javax.ws.rs.BeanParam;
import javax.ws.rs.GET;
import javax.ws.rs.Path;

/**
 * @author elwood
 */
@Path("/beanParamTest")
@Api("/beanParamTest")
public class BeanParamTargetResource {
    @GET
    @ApiOperation("")
    public String search(@BeanParam BeanParamBean param) {
        return "";
    }
}
