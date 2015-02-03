package testresources;

import com.wordnik.swagger.annotations.ApiParam;

import javax.ws.rs.QueryParam;
import java.util.List;

/**
 * @author elwood
 */
public class BeanParamBean {
    private String q;
    private List<String> conditions;

    public String getQ() {
        return q;
    }

    @QueryParam("query")
    @ApiParam(value = "sample param data", required = true, allowableValues = "a,b,c")
    public void setQ(String q) {
        this.q = q;
    }

    public List<String> getConditions() {
        return conditions;
    }

    @QueryParam("conds")
    public void setConditions(List<String> conditions) {
        this.conditions = conditions;
    }
}
