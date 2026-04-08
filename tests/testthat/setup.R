library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()

DummyClient <- R6::R6Class("Chat",
  public = list(
    set_system_prompt = function(value) {
      invisible()
    },
    set_turns = function(value) {
      invisible()
    },
    chat = function(prompt, echo) {
      response <- "
{
\"excludedConditions\": \"\",
\"proposedInExcluded\": \"NO\",
\"finalAnswer\": \"YES\",
\"rationaleForAnswer\": \"Just because\",
\"confidenceLevel\": \"90%\"
}
      "
      return(response)
    },
    chat_structured = function(prompt, echo, type) {
      response <- as.list(jsonlite::fromJSON("
{
\"excludedConditions\": \"\",
\"proposedInExcluded\": \"NO\",
\"finalAnswer\": \"YES\",
\"rationaleForAnswer\": \"Just because\",
\"confidenceLevel\": \"90%\"
}
      "))
      return(response)
    },
    get_cost = function() {
      1e6
    },
    get_model = function() {
      "1e6"
    }
  )
)

client <- DummyClient$new()

withr::defer(
  {
    unlink(connectionDetails$server())
  },
  testthat::teardown_env()
)
