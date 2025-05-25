// The Swift Programming Language
// https://docs.swift.org/swift-book

// This is a simple Swift program that uses a C function to add two numbers. 
// It imports a C header file named CFiles.h, which contains the declaration of the add function.
// The main function calls the add function with two integers, 10 and 20, and prints the result to the console.
// The expected output is "The result of adding 10 and 20 is 30".
// This demonstrates how Swift can interoperate with C code, allowing developers to leverage existing C libraries and functions within Swift applications.  
import cxxLibrary

let result = add(10, 20)
print("The result of adding 10 and 20 is \(result)")
