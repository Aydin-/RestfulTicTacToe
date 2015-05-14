package com.aydin

import io.dropwizard.Application
import io.dropwizard.setup.Bootstrap
import io.dropwizard.setup.Environment
import com.fasterxml.jackson.databind.ObjectMapper
import io.dropwizard.jersey.jackson.JacksonMessageBodyProvider
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object TicTacApplication {
  def main(args: Array[String]) {
    new TicTacApplication().run(args:_*)
  }
}

class TicTacApplication extends Application[TicTacConfiguration] {
  override def getName() = "noughts"

  override def initialize(bootstrap: Bootstrap[TicTacConfiguration]) {
    
  }

  override def run(configuration: TicTacConfiguration, environment: Environment) {
    val resource = new NoughtsResource()
    val objectMapper = new ObjectMapper()
    objectMapper.registerModule(DefaultScalaModule)
    environment.jersey().register(new JacksonMessageBodyProvider(objectMapper, environment.getValidator()));
    environment.jersey().register(resource);
  }

}
