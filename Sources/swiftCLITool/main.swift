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


var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = try await Application.make(env)

// This attempts to install NIO as the Swift Concurrency global executor.
// You can enable it if you'd like to reduce the amount of context switching between NIO and Swift Concurrency.
// Note: this has caused issues with some libraries that use `.wait()` and cleanly shutting down.
// If enabled, you should be careful about calling async functions before this point as it can cause assertion failures.
// let executorTakeoverSuccess = NIOSingletons.unsafeTryInstallSingletonPosixEventLoopGroupAsConcurrencyGlobalExecutor()
// app.logger.debug("Tried to install SwiftNIO's EventLoopGroup as Swift's global concurrency executor", metadata: ["success": .stringConvertible(executorTakeoverSuccess)])

do {
    try await configure(app)
    
    app.routes.all
        .forEach { app.logger.info("\($0)") }
    try await app.execute()
} catch {
    app.logger.report(error: error)
    try? await app.asyncShutdown()
    throw error
}
try await app.asyncShutdown()
