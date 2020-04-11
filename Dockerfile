FROM hseeberger/scala-sbt:8u242_1.3.8_2.12.10 AS build

WORKDIR /src

COPY . /src

RUN sbt  assembly

FROM openjdk:8u242-jre

COPY --from=build \
    /src/target/scala-*/lambda-assembly-*.jar \
    /src/lambda.jar

WORKDIR /src

CMD java -jar lambda.jar
