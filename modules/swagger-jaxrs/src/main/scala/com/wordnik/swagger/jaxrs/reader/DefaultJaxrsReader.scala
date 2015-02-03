package com.wordnik.swagger.jaxrs.reader

import java.lang.annotation.Annotation
import java.lang.reflect
import java.lang.reflect.Method
import javax.ws.rs._
import javax.ws.rs.core.Context

import com.wordnik.swagger.annotations._
import com.wordnik.swagger.config._
import com.wordnik.swagger.core.ApiValues._
import com.wordnik.swagger.core.util.ModelUtil
import com.wordnik.swagger.jaxrs._
import com.wordnik.swagger.model._

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

class DefaultJaxrsApiReader extends JaxrsApiReader {
  def readRecursive(
    docRoot: String, 
    parentPath: String, cls: Class[_], 
    config: SwaggerConfig,
    operations: ListBuffer[Tuple3[String, String, ListBuffer[Operation]]],
    parentMethods: ListBuffer[Method]): Option[ApiListing] = {
    val api = cls.getAnnotation(classOf[Api])

    // must have @Api annotation to process!
    if(api != null) {
      val consumes = Option(api.consumes) match {
        case Some(e) if(e != "") => e.split(",").map(_.trim).toList
        case _ => cls.getAnnotation(classOf[Consumes]) match {
          case e: Consumes => e.value.toList
          case _ => List()
        }
      }
      val produces = Option(api.produces) match {
        case Some(e) if(e != "") => e.split(",").map(_.trim).toList
        case _ => cls.getAnnotation(classOf[Produces]) match {
          case e: Produces => e.value.toList
          case _ => List()
        }
      }
      val protocols = Option(api.protocols) match {
        case Some(e) if(e != "") => e.split(",").map(_.trim).toList
        case _ => List()
      }
      val description = api.description match {
        case e: String if(e != "") => Some(e)
        case _ => None
      }
      // look for method-level annotated properties
      val parentParams: List[Parameter] = getAllParamsFromFields(cls)

      for(method <- cls.getMethods) {
        val returnType = findSubresourceType(method)
        val path = method.getAnnotation(classOf[Path]) match {
          case e: Path => e.value()
          case _ => ""
        }
        val endpoint = (parentPath + pathFromMethod(method)).replace("//", "/")
        Option(returnType.getAnnotation(classOf[Api])) match {
          case Some(e) => {
            val root = docRoot + api.value + pathFromMethod(method)
            parentMethods += method
            readRecursive(root, endpoint, returnType, config, operations, parentMethods)
            parentMethods -= method
          }
          case _ => {
            if(method.getAnnotation(classOf[ApiOperation]) != null) {
              readMethod(method, parentParams, parentMethods) match {
                case Some(op) => appendOperation(endpoint, path, op, operations)
                case None =>
              }
            }
          }
        }
      }
      // sort them by min position in the operations
      val s = (for(op <- operations) yield {
        (op, op._3.map(_.position).toList.min)
      }).sortWith(_._2 < _._2).toList
      val orderedOperations = new ListBuffer[Tuple3[String, String, ListBuffer[Operation]]]
      s.foreach(op => {
        val ops = op._1._3.sortWith(_.position < _.position)
        orderedOperations += Tuple3(op._1._1, op._1._2, ops)
      })
      val apis = (for ((endpoint, resourcePath, operationList) <- orderedOperations) yield {
        val orderedOperations = new ListBuffer[Operation]
        operationList.sortWith(_.position < _.position).foreach(e => orderedOperations += e)
        ApiDescription(
          addLeadingSlash(endpoint),
          None,
          orderedOperations.toList,
          hidden = api.hidden)
      }).toList

      val basePath = {
        if(api.basePath == "")
          config.basePath
        else
          api.basePath
      }
      val models = ModelUtil.modelsFromApis(apis)
      Some(ApiListing (
        apiVersion = config.apiVersion,
        swaggerVersion = config.swaggerVersion,
        basePath = basePath,
        resourcePath = addLeadingSlash(api.value),
        apis = ModelUtil.stripPackages(apis),
        models = models,
        description = description,
        produces = produces,
        consumes = consumes,
        protocols = protocols,
        position = api.position)
      )
    }
    else None
  }

  // decorates a Parameter based on annotations, returns None if param should be ignored
  def processParamAnnotations(mutable: MutableParameter, paramAnnotations: Array[Annotation]): List[Parameter] = {
    var shouldIgnore = false
    for (pa <- paramAnnotations) {
      pa match {
        // Для BeanParam-параметра просто берём все сеттеры, помеченные одной из обычных params-аннотаций,
        // и рекурсивно получаем для них списки параметров. После этого возвращаем соединённые списки наверх
        case e: BeanParam => {
          val beanClass = Class.forName(mutable.dataType)
          val methods: Array[Method] = beanClass.getMethods

          // Это замыкание конвертирует метод внутри BeanParam-класса в кортеж аргументов
          // для processParamAnnotations или в null, если метод нам не интересен
          // (нет аннотации, не сеттер). Сеттер должен иметь один параметр и не иметь возвращаемого значения.
          val clojure : (Method) => (MutableParameter, Array[Annotation]) = (m) => {
            val annotations: Array[Annotation] = m.getAnnotations
            if (annotations.nonEmpty) {
              val supportedAnnotations: immutable.HashSet[Class[_]] = immutable.HashSet[Class[_]] (
                classOf[ApiParam],
                classOf[QueryParam],
                classOf[PathParam],
                classOf[MatrixParam],
                classOf[HeaderParam],
                classOf[FormParam],
                classOf[CookieParam],
                classOf[DefaultValue],
                classOf[Context]
              )

              val filteredAnnotations: Array[Annotation] = annotations.filter(a => supportedAnnotations.contains(a.annotationType()))
              if (filteredAnnotations.nonEmpty) {
                // todo : check return type is void
                // todo : check parameters count is 1
                val param: reflect.Parameter = m.getParameters.lift(0).orNull
                if (null != param) {
                  val mutableParam = new MutableParameter
                  mutableParam.dataType = processDataType(param.getType, param.getParameterizedType)
                  mutableParam.allowableValues = processAllowableValues(param.getType, param.getParameterizedType)

                  (mutableParam, filteredAnnotations)
                } else {
                  null
                }
              } else {
                null
              }
            } else {
              null
            }
          }

          val map: Array[(MutableParameter, Array[Annotation])] = methods.map(clojure)
          val list: List[(MutableParameter, Array[Annotation])] = map.filter(i => i != null).toList
          if (list.isEmpty) return List.empty[Parameter]
          val allParams: List[Parameter] = list.map(tuple => {
            processParamAnnotations(tuple._1, tuple._2)
          }).reduce((a: List[Parameter], b: List[Parameter]) => List.concat(a, b))
          return allParams
        }
        case e: ApiParam => parseApiParamAnnotation(mutable, e)
        case e: QueryParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.paramType = readString(TYPE_QUERY, mutable.paramType)
        }
        case e: PathParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.required = true
          mutable.paramType = readString(TYPE_PATH, mutable.paramType)
        }
        case e: MatrixParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.paramType = readString(TYPE_MATRIX, mutable.paramType)
        }
        case e: HeaderParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.paramType = readString(TYPE_HEADER, mutable.paramType)
        }
        case e: FormParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.paramType = readString(TYPE_FORM, mutable.paramType)
        }
        case e: CookieParam => {
          mutable.name = readString(e.value, mutable.name)
          mutable.paramType = readString(TYPE_COOKIE, mutable.paramType)
        }
        case e: DefaultValue => {
          mutable.defaultValue = Option(readString(e.value))
        }
        case e: Context => shouldIgnore = true
        case _ =>
      }
    }
    if(!shouldIgnore) {
      if(mutable.paramType == null) {
        mutable.paramType = TYPE_BODY
        mutable.name = TYPE_BODY
      }
      List(mutable.asParameter)
    }
    else List.empty
  }

  def findSubresourceType(method: Method): Class[_] = {
    method.getReturnType
  }
}
