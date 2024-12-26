#*** Take from personal saved boilerplate
# First Stage - Build
FROM maven:3.9.9-eclipse-temurin-23 AS builder
ARG COMPILE_DIR=/compiledir
WORKDIR ${COMPILE_DIR}

# Copy necessary files for Maven
COPY mvnw . 
COPY mvnw.cmd . 
COPY pom.xml . 
COPY .mvn .mvn 
COPY src src  

# Set executable permission for mvnw
RUN chmod +x mvnw

# Run Maven to build the project
RUN ./mvnw clean package -DskipTests

# Second Stage - Runtime
FROM maven:3.9.9-eclipse-temurin-23
ARG WORK_DIR=/app
WORKDIR ${WORK_DIR}

# Copy the application jar from the build stage
COPY --from=builder /compiledir/target/vttp5a_miniproject-0.0.1-SNAPSHOT.jar vttp5a_miniproject.jar

# Install curl -- like Chuk did in class
# RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV SERVER_PORT=8080

# Expose port
EXPOSE ${SERVER_PORT}

#Healthcehck
# HEALTHCHECK --interval=60s --timeout=10s --start-period=120s --retries=3 CMD curl -s -f http://localhost:${SERVER_PORT}/status || exit 1

# Command to run the application
ENTRYPOINT ["java", "-jar", "vttp5a_miniproject.jar"]

# Build and run the container
# docker build -t fahmyeby/noticeboardapp:v<number> . <- *** REMEMBER TO ADD <space>. 
# docker run -p 8080:8080 fahmyeby/noticeboardapp:v<number> <- *** WITHOUT additional .