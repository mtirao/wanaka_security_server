// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "CommandLineSwiftToolUsesCxx",
    products: [
        .library(
            name: "cxxLibrary",
            targets: ["cxxLibrary"]),
        .executable(
            name: "swiftCLITool",
            targets: ["swiftCLITool"])
    ],
    targets: [
        .target(
            name: "cxxLibrary"),
        .executableTarget(
            name: "swiftCLITool",
            dependencies: ["cxxLibrary"],
            swiftSettings: [.interoperabilityMode(.Cxx)])
    ]
)
