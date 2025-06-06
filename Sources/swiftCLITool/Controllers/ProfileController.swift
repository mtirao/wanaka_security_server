//
//  File.swift
//  CommandLineSwiftToolUsesCxx
//
//  Created by Marcos Tirao on 05/06/2025.
//

import Vapor


struct ProfileController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let auth = routes.grouped("api", "profile")
        auth.get(use: index)
    }
    
    func index(_ req: Request) async throws -> String {
        print("User ID:\(req.headers["X-User-Id"])")
        return "Hello, world!"
    }
    
}

