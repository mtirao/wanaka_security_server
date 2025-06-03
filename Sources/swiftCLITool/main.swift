// The Swift Programming Language
// https://docs.swift.org/swift-book

// This is a simple Swift program that uses a C function to add two numbers. 
// It imports a C header file named CFiles.h, which contains the declaration of the add function.
// The main function calls the add function with two integers, 10 and 20, and prints the result to the console.
// The expected output is "The result of adding 10 and 20 is 30".
// This demonstrates how Swift can interoperate with C code, allowing developers to leverage existing C libraries and functions within Swift applications.  
import cxxLibrary
import MQTTNIO
import Vapor


let client = MQTTClient(
    host: "192.168.0.56",
    port: 1883,
    identifier: "My Client",
    eventLoopGroupProvider: .shared(.singletonMultiThreadedEventLoopGroup)
)

let connectFuture = client.connect()
connectFuture.whenSuccess { _ in
    print("Connected to MQTT broker")
}
connectFuture.whenFailure { error in
    print("Failed to connect to MQTT broker: \(error)")
}


var env = try Environment.detect()
    try LoggingSystem.bootstrap(from: &env)
        
let app = try await Application.make(env)


app.get("hello", ":name") { req async throws -> String in
    // 2
    let name = try req.parameters.require("name")
    
    let greeting = "Hello, \(name.capitalized)!"

    client.publish(to: "/topic/qos1", payload: ByteBuffer(string:greeting), qos: MQTTQoS.atLeastOnce ).whenSuccess { _ in
        print("Published greeting to MQTT broker")
    }
    
    return "Hello, \(name.capitalized)!"
}

do {   
    try await app.execute()
} catch {
    app.logger.report(error: error)
    try? await app.asyncShutdown()
    throw error
}
try await app.asyncShutdown()
