case class Output(
    result: List[String]
)

case class InputLambda(
    code: String
)

case class InputGCode(
    code: String,
    onlyResult: Boolean
)

case class Error(
    err: String
)
