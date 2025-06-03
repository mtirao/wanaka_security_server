// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "CommandLineSwiftToolUsesCxx",
    platforms: [
        .macOS(.v13)
    ],
    products: [
        .library(
            name: "cxxLibrary",
            targets: ["cxxLibrary"]),
        .executable(
            name: "swiftCLITool",
            targets: ["swiftCLITool"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from:"4.115.0"),
        .package(url: "https://github.com/apple/swift-nio.git", from:"2.65.0"),
        .package(url: "https://github.com/swift-server-community/mqtt-nio.git", from:"2.12.1")
        
    ],
    targets: [
        .target(
            name: "cxxLibrary"),
        .executableTarget(
            name: "swiftCLITool",
            dependencies: ["cxxLibrary",
                            .product(name: "Vapor", package: "vapor"),
                            .product(name: "NIO", package: "swift-nio"),
                            .product(name: "NIOPosix", package: "swift-nio"),
                            .product(name: "MQTTNIO", package: "mqtt-nio")],
            swiftSettings: [.interoperabilityMode(.Cxx)])
    ]
)
